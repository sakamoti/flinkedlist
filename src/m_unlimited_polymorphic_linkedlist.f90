module m_unlimited_polymorphic_linkedlist
  !$ use omp_lib
  use iso_fortran_env
  implicit none
  private
  !--subroutinies
  public obj_show        ! the routine showing `class(*)` data
  !-- function interfaces
  public list_sort_func  ! function interface for sort inside a list
  !public list_apply_proc ! interface for apply function to each elements
  public obj_show_proc   ! function interface for print `class(*)` data object

  type,private :: node
    !* Objects of list elements
    ! 
    !  this is internal data structure in the list
    ! @note fortran2003 and 2008 features are needed
    class(*)  ,pointer,private :: obj=>null() !! data pointer
    type(node),pointer,private :: nxt=>null() !! link to the next node
    type(node),pointer,private :: bef=>null() !! link to the before(previous) node
    contains
      final :: node_final
      procedure,private :: node_equal !! user defined assignment procedure
      generic :: assignment(=) => node_equal !! user defined assignemet (generic)
  end type

  type,public :: node_operator_type
    !* リスト要素オブジェクト(外部からの操作機能付き)
    ! 
    ! この型を利用する前にリスト要素を示すように初期化されている必要がある。
    ! initメソッドで初期化可能。
    type(node)  ,pointer,private :: pos => null()  !!リスト要素へのポインタ
    class(list_type),pointer,private :: parent=>null() !!親オブジェクトへのポインタ
    contains
      final :: node_operator_type_final !! destractor of node_operator_type
      procedure,non_overridable,public :: init    => node_operator_type_init !! initialization
      procedure,non_overridable,public :: head    => node_operator_type_head !! 指示先をリスト先頭に戻す
      procedure,non_overridable,public :: tail    => node_operator_type_tail !! 指示先をリスト最後尾に
      procedure,non_overridable,public :: next    => node_operator_type_next !! 指示先を一つ次に移す
      procedure,non_overridable,public :: prev    => node_operator_type_previous !!指示先を一つ前に移す
      procedure,non_overridable,public :: show => node_show !! show node value
      procedure,non_overridable,public :: get_ptr  => node_operator_type_getobj_ptr   !!指示先の要素へのポインタを得る
      procedure,non_overridable,public :: get_alloc  => node_operator_type_getobj_alloc   !!指示先の要素への実体を得る
  end type

  type,public :: list_type
    !! doubly linked list object
    type(node),pointer,private :: head=>null() !! list head
    integer,private :: num=0         !! number of elements in the list
    contains
      final :: list_final !! destructor of list_type
      procedure,non_overridable,public :: append    => list_append
        !!append a element (subroutine,impure elemental)
      procedure,non_overridable,public :: delete    => node_delete
        !!delete a element (subroutine,impure elemental)
      procedure,non_overridable,public :: delall    => list_delall
        !!delete all elements (subroutine,pure elemental)
      procedure,non_overridable,public :: count     => list_count_node
        !!count how many elements are there (subroutine,pure elemental)
      procedure,non_overridable,public :: showall   => list_showall
        !!show all elements (subroutine)
      !procedure,non_overridable,public :: apply     => list_apply
        !!apply user defined routine in each elements (subroutine)
      procedure,non_overridable,public :: sort      => list_sort
        !!sort elements by using user defined routine (subroutine)
      procedure,non_overridable,public :: listarray => list_elem_pointer_array
        !!リスト要素を指す配列を作る(function)
      procedure,non_overridable,public :: listarray_ptr => list_elem_pointer_array_ptr
        !!リスト要素を指す配列を作る(function)
      procedure,non_overridable,nopass,public :: show => node_show
        !!指定された要素1つを表示(subroutine)
      procedure,non_overridable,private :: copy      => list_copy
        !!ユーザー定義代入操作の実体
      generic :: assignment(=) => copy
        !!ユーザー定義代入操作(リストのコピー)
  end type

  interface
    function list_sort_func(one,two,passdata)result(is_swap)
      !! リストをソートするための関数インターフェイス
      class(*),intent(in) :: one !!ソートで比較する要素1
      class(*),intent(in) :: two !!ソートで比較する要素2
      class(*),intent(in),optional :: passdata !!比較の為に使う追加データ
      logical :: is_swap !!oneとtwoを入れ替えるときTRUE
    end function
   !subroutine list_apply_proc(obj,passdata)
   !  !! apply関数で与える関数の型
   !  class(*),intent(inout),pointer :: obj !! 操作対象のオブジェクト(リスト要素)
   !  class(*),intent(in),optional :: passdata !!追加データが必要な時に使う
   !end subroutine
    subroutine obj_show_proc(obj,passdata,fid)
      !! obj_showルーチンでユーザー定義型を表示する関数
      class(*),intent(in) :: obj !! 操作対象のオブジェクト(リスト要素)
      class(*),intent(in),optional :: passdata !!追加データが必要な時に使う
      integer ,intent(in),optional :: fid !! ファイルid
    end subroutine
  end interface

  !------------------------------------------------------------
  contains
    impure elemental subroutine node_operator_type_init(self,list)
      class(node_operator_type),intent(inout) :: self
      class(list_type),intent(in),target :: list
      self%parent=>list
      call self%head()
      if(associated(list%head))then
        self%pos%nxt=>list%head%nxt
        self%pos%bef=>list%head%bef
      else
        self%pos%nxt=>null()
        self%pos%bef=>null()
      endif
    end subroutine
    !--------------------------------
    impure elemental subroutine node_operator_type_head(self)
      !! node_operator_typeが指し示すリスト要素を初期化する
      class(node_operator_type),intent(inout) :: self !! node_operator_type
      if(associated(self%parent))then
        self%pos=>self%parent%head
      else
        self%pos=>self%parent%head
      end if
    end subroutine
    !--------------------------------
    impure elemental subroutine node_operator_type_tail(self)
      !! node_operator_typeが指し示すリスト要素を最後尾にする
      class(node_operator_type),intent(inout) :: self
      call node_operator_type_check_parent(self)
      if(.not.associated(self%pos)) return
      do while(associated(self%pos%nxt))
        call self%next()
      enddo
    end subroutine
    !--------------------------------
    impure elemental subroutine node_operator_type_next(self)
      !! node_operator_typeが指し示す要素を一つ次に進める
      class(node_operator_type),intent(inout),target :: self
      call node_operator_type_check_parent(self)
      if(.not.associated(self%pos)) return
      if(associated(self%pos%nxt))then
        self%pos=>self%pos%nxt
      endif
    end subroutine
    !--------------------------------
    impure elemental subroutine node_operator_type_previous(self)
      !! node_operator_typeが指し示す要素を一つ前に進める
      class(node_operator_type),intent(inout),target :: self
      call node_operator_type_check_parent(self)
      if(.not.associated(self%pos)) return
      if(associated(self%pos%bef))then
        self%pos=>self%pos%bef
      endif
    end subroutine
    !--------------------------------
    subroutine node_operator_type_getobj_ptr(self,ptr)
      !! node_operator_typeが指し示す要素を直に指すポインタを返す
      !!@note
      !! このルーチンは、リスト内部の要素そのものを指し示すポインタを返すため、
      !! ポインタ指示先を書き換えた場合はがダイレクトにリスト要素へ反映される。
      class(node_operator_type),intent(in) :: self !! 操作対象のnode_operator_type型
      class(*),pointer,intent(inout) :: ptr !! 無限多相性のオブジェクトを示すポインタ
      ptr=>null()
      if(.not.associated(self%pos))return
      if(.not.associated(self%pos%obj))return
      ptr=>self%pos%obj
    end subroutine
    !--------------------------------
    subroutine node_operator_type_getobj_alloc(self,res)
      !!node_operator_typeが指し示す要素のコピーを返す
      !!@note
      !! このルーチンで返る`res`は、新たにメモリallocateされた結果である。
      !! したがって、この値を変更してもリスト要素には反映されない。
      class(node_operator_type),intent(in) :: self !!リスト内部の特定部位を指すオブジェクト
      class(*),allocatable :: res !! 無限多相性のオブジェクト(ポインタ指示先のコピー)
      if(.not.associated(self%pos))return
      if(.not.associated(self%pos%obj))return
      allocate(res,source=self%pos%obj)
    end subroutine
    !--------------------------------
    impure elemental subroutine node_operator_type_check_parent(elpt)
      !* node_operator_typeが親リストを持つかどうかチェックする
      ! 
      !  - 要素の関連付けなし。親リストの関連付けなし。:処理を中断。
      !  - 要素の関連付けなし。親リストの関連付けあり。:要素をリスト先頭に関連付け。
      !  - 要素の関連付けあり。親リストの関連付けなし。:警告を表示して続行。
      !  .
      class(node_operator_type),intent(inout) :: elpt !! 操作対象のnode_operator_type型
      if(.not.associated(elpt%pos))then
        if(associated(elpt%parent))then
          elpt%pos=>elpt%parent%head
        else
          stop "@node_operator_type_check_parent"
        endif
      endif
    end subroutine
    !--------------------------------
    pure elemental subroutine node_operator_type_final(self)
      !! node_operator_type型のデストラクタ
      type(node_operator_type),intent(inout) :: self
      self%pos=>null()
      self%parent=>null()
    end subroutine
    !--------------------------------
    impure elemental subroutine node_equal(left,right)
      !* イコールの演算子でリストの要素をコピーする
      ! 
      !  ただし、コピー元リストの前後ポインタは関連付けない
      class(node),intent(out) :: left  !! イコールの左側
      class(node),intent(in)  :: right !! イコールの右側
      if(associated(right%obj))allocate(left%obj,source=right%obj)
      left%nxt=> null()
      left%bef=> null()
    end subroutine
    !--------------------------------
    impure elemental subroutine node_final(self)
      !! destructor for a node type
      type(node),intent(inout) :: self !< self object
      if(associated(self%obj))deallocate(self%obj)
      self%nxt=>null()
      self%bef=>null()
    end subroutine
    !--------------------------------
    subroutine node_show(mydata,showproc,passdata,fid)
      !! nodeptr型が示すオブジェクトを表示
      class(node_operator_type),intent(inout) :: mydata !! mydata node_operator_type型
      procedure(obj_show_proc),optional :: showproc !! obj_show_procインターフェースで示される引数を持つ関数
      class(*),intent(in),optional :: passdata !! 表示で使うオプションデータ
      integer ,intent(in),optional :: fid !! 装置id
      if(.not.associated(mydata%pos))return
      call node_operator_type_check_parent(mydata)
      call obj_show(mydata%pos%obj,printobj=showproc,passdata=passdata,fid=fid)
    end subroutine
    !--------------------------------
    impure elemental subroutine list_delall(self)
      !! list_type型の全要素を削除する
      class(list_type),intent(inout) :: self
      type(node),pointer :: tmp,inode
      integer :: istat
      inode=>self%head
      do
        if(associated(inode))then
          !print*,"deallocate list contents of ",i
          tmp=>inode%nxt
          deallocate(inode,stat=istat)
          inode=>tmp
        else
          exit
        endif
      enddo
      self%num=0
      self%head=>null()
    end subroutine
    !--------------------------------
    impure elemental subroutine list_final(self)
      !! list_type型のデストラクタ
      type(list_type),intent(inout) :: self
      call self%delall()
    end subroutine
    !--------------------------------
    subroutine list_append(self,obj,addloc)
      !<@brief append data to the list
      !<
      !< data is newly allocated and insert into the list. (default is head position)
      class(list_type),intent(inout),target :: self  !! list
      class(*),intent(in) :: obj  !! unlimited polimorphic type to append the list
      type(node_operator_type),intent(inout),optional :: addloc !! obj is added next to this pointer
      type(node),pointer :: add,tmp
      allocate(node::add)
      allocate(add%obj,source=obj)
      if(present(addloc))then
        ! insert data next to the position `addloc`
        if(.not.associated(addloc%parent))then
          write(error_unit,*)"ERROR: Though addloc optional parameter is added, not associated to `list_type`"
          write(error_unit,*)"ERROR: Fail to append the data to the list"
          return
        endif
        if(.not.associated(addloc%pos))then
          write(error_unit,*)"ERROR: NULL POINTER (addloc%pos)"
          write(error_unit,*)"ERROR: Fail to append the data to the list"
          return
        else
          !append
          add%nxt=>addloc%pos%nxt
          addloc%pos%nxt=>add      !addloc pointer is changed
          add%bef=>addloc%pos
        endif
      else
        !add data to the head position
        if(.not.associated(self%head))then
          self%head=>add
        else
          tmp=>self%head
          self%head=>add
          self%head%nxt=>tmp
          tmp%bef=>self%head
        endif
      endif
      self%num=self%num+1
    end subroutine
    !--------------------------------
   !subroutine list_apply(self,applyproc,passdata,parallel)
   !  !! リストの全要素に関数`applyproc`を適用する
   !  !! @note 
   !  !! Rのapply関数と同様の動作を意図して作成した。
   !  class(list_type),intent(inout) :: self !! list_type型
   !  procedure(list_apply_proc) :: applyproc !!`list_apply_proc`インターフェースを持つユーザー定義ルーチン
   !  class(*),intent(in),optional :: passdata !! applyprocルーチンで追加データを利用する場合に使用
   !  logical,intent(in),optional :: parallel !! OpenMPで並列実行したいときに.TRUE.を指定
   !  type(node_operator_type) :: ipt
   !  integer :: i
   !  logical :: do_para
   !  type(node_operator_type),dimension(:),allocatable :: temp

   !  do_para=.false.
   !  if(present(parallel))then
   !    if(parallel) do_para=.true.
   !  endif

   !  if(do_para)then
   !    !with OpenMP
   !    temp=self%listarray_ptr()
   !    !$omp parallel
   !    !$omp do
   !    do i=1,size(temp)
   !      !apply function to list elements
   !      call applyproc(temp(i)%pos%obj,passdata=passdata)
   !    enddo
   !    !$omp end do
   !    !$omp end parallel
   !    !print *, "Hello! N =", omp_get_num_threads(), " and I am ", omp_get_thread_num()
   !  else
   !    !sequential
   !    call ipt%init(self)
   !    do i=1,self%num
   !      call applyproc(ipt%pos%obj,passdata=passdata)
   !      call ipt%next()
   !    enddo
   !  endif
   !end subroutine
    !--------------------------------
    subroutine list_showall(self,showproc,passdata,fid)
      !! リスト要素を全て表示する
      class(list_type),intent(in),target :: self !! list_type型
      procedure(obj_show_proc),optional :: showproc !! ユーザー定義型を表示するルーチン
      class(*),intent(in),optional :: passdata !! 表示に使う追加データ
      integer,intent(in),optional :: fid !! 装置id
      type(node_operator_type) :: ipt
      integer :: i,fileid
      if(present(fid))then
        fileid=fid
      else
        fileid=output_unit
      endif
      call ipt%init(self)
      do i=1,self%num
        write(fileid,'(1X,"Item ",I0,"/",I0,";",2X)',advance="no") &
          self%num-i+1,self%num
        call self%show(ipt,showproc=showproc,passdata=passdata,fid=fileid)
        call ipt%next()
      enddo
    end subroutine
    !--------------------------------
    subroutine node_delete(self,delnode)
      !* リストから要素を削除する
      !
      ! 引数に与えたポインタが示す先の要素をリストから削除する。
      class(list_type),intent(inout),target :: self !! list_type型
      class(node_operator_type),intent(inout) :: delnode !! リスト要素のポインタ
      type(node),pointer :: bef,nxt
      class(list_type),pointer :: plist
      !ポインタ指示先の確認
      if(.not.associated(delnode%pos))return
      if(.not.associated(delnode%parent,target=self))then
        write(error_unit,'("ERROR: delnode is not point own elements")')
        write(error_unit,'("ERROR: fail to delete@node_delete")')
        return
      else
        !delete
        plist=>delnode%parent
        plist%num=plist%num-1
        bef=>delnode%pos%bef
        nxt=>delnode%pos%nxt
        if(associated(bef))then
          bef%nxt=>nxt
        else
          nxt%bef => null()
          ! head pointer needs to be fix
          plist%head=>nxt
        endif
        if(associated(nxt))then
          nxt%bef=>bef
        else
          bef%nxt => null()
        endif
        deallocate(delnode%pos)
        delnode%pos=>nxt
      endif
    end subroutine
    !--------------------------------
    pure elemental function list_count_node(self)result(n)
      !! リスト要素数を返す関数
      class(list_type),intent(in) :: self !! list_type型
      integer :: n
      n=self%num
    end function
    !--------------------------------
    impure elemental subroutine list_copy(left,right)
      !! リストのコピー(メモリを新しくアロケート)
      class(list_type),intent(in) :: right !! イコールの右
      class(list_type),intent(out) :: left !! イコールの左
      type(node_operator_type) :: elpr
      integer :: i,n
      !コピー元の要素を指すelprを初期化
      call elpr%init(right)
      !リスト最後尾を指す状態にする
      call elpr%tail()
      !リストを一つずつ追加
      n=right%count()
      do i=1,n
        call left%append(elpr%pos%obj)
        call elpr%prev()
      enddo
    end subroutine
    !--------------------------------
    subroutine list_sort(self,func,passdata)
      !! do bubble sort
      class(list_type),intent(inout),target :: self
      procedure(list_sort_func) :: func !! subroutine which has list_sort_func interface
      class(*),intent(in),optional :: passdata !! additional parameter
      class(node),pointer :: ipt
      class(node),pointer :: jpt
      class(*),pointer :: tmpptr
      ! if list node is less than 1, sort is not needed
      if(self%num<=1)return
      ipt=>null()
      jpt=>null()
      !func(one,two)result(is_swap)
      !one,twoの２つの順番を入れ替えるときにis_swapがTRUEとなる
      ipt=>self%head
      do
        if(.not.associated(ipt))exit
        jpt=>ipt%nxt
        do
          if(.not.associated(jpt))exit
          ! check swapping
          if(func(ipt%obj,jpt%obj,passdata))then
            tmpptr=>ipt%obj
            ipt%obj=>jpt%obj
            jpt%obj=>tmpptr
          endif
          ! next pointer
          jpt=>jpt%nxt
        enddo
        ipt=>ipt%nxt
      enddo
    end subroutine
    !--------------------------------
    function list_elem_pointer_array_ptr(self)result(res)
      !! リスト要素を配列として扱うためのnode_operator_type型配列の生成ルーチン
      class(list_type),intent(in),target :: self
      type(node_operator_type),dimension(:),allocatable :: res !! node_operator_type配列(オリジナルのリスト要素のポインタ)
      type(node),pointer :: i_ptr
      integer :: i
      allocate(res(self%num))
      if(self%num<=0)return
      call res(:)%init(self) !initialize elementary
      i_ptr => self%head
      do i=1,self%count()
        allocate(res(i)%pos)
        !allocate(res(i)%pos%obj,source=i_ptr%obj)
        res(i)%pos%obj => i_ptr%obj
        i_ptr => i_ptr%nxt
      end do
    end function
    !--------------------------------
    function list_elem_pointer_array(self)result(res)
      !! リスト要素を配列として扱うためのnode_operator_type型配列の生成ルーチン
      class(list_type),intent(in),target :: self
      type(node_operator_type),dimension(:),allocatable :: res 
        !! node_operator_type配列(オリジナルのリスト要素のコピー)
      type(node),pointer :: i_ptr
      integer :: i
      allocate(res(self%num))
      if(self%num<=0)return
      call res(:)%init(self) !initialize elementary
      i_ptr => self%head
      do i=1,self%count()
        allocate(res(i)%pos)
        allocate(res(i)%pos%obj,source=i_ptr%obj)
        i_ptr => i_ptr%nxt
      end do
    end function
    !--------------------------------
    subroutine obj_show(obj,printobj,passdata,fid)
      !! 無限多相性オブジェクトを表示するルーチン
      class(*),intent(in)::obj !! 要素
      procedure(obj_show_proc),optional::printobj
        !! obj_show_procインターフェースを持つユーザー定義型を表示する場合の手続き
      class(*),intent(in),optional :: passdata !! printobjで使う追加データ
      integer ,intent(in),optional :: fid !! 装置id
      integer :: fileid
      !ファイルidの設定
      if(present(fid))then
        fileid=fid
      else
        fileid=output_unit
      endif
      !showing procedure
      select type(x=>obj)
      type is(integer(kind=int8))
        write(fileid,'(1X,G0)')x
      type is(integer(kind=int16))
        write(fileid,'(1X,G0)')x
      type is(integer(kind=int32))
        write(fileid,'(1X,G0)')x
      type is(integer(kind=int64))
        write(fileid,'(1X,G0)')x
      type is(real(kind=real32))
        write(fileid,'(1X,G0)')x
      type is(real(kind=real64))
        write(fileid,'(1X,G0)')x
      type is(real(kind=real128))
        write(fileid,'(1X,G0)')x
      type is(logical)
        write(fileid,'(1X,G0)')x
      type is(complex)
        write(fileid,'(1X,"(",G0,",",G0,")")')real(x),aimag(x)
      type is(complex(kind(0d0)))
        write(fileid,'(1X,"(",G0,",",G0,")")')real(x),aimag(x)
      type is(character(len=*))
        write(fileid,'(1X,G0)')x
      class default
        if(present(printobj))then
          call printobj(obj,passdata=passdata,fid=fid)
        else
          write(fileid,*)"unrecognised item"
        endif
      end select
    end subroutine
end module
