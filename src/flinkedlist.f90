!>@file flinkedlistf90
!> Doubly linked list module using unlimited polymorphic class.
!>
!-------------------------------------------------------------------
!>@file flinkedlist.f90
!>@brief Doubly linked list module using unlimited polymorphic class
!>
!> About *finkedlist* module
!> ==========================
!> Doubly linked list using unlimited polymorphic object pointer.
!> Doubly linked list which can hold any predefined data type or user defined type.@n
!>
!> * Public type
!>   name of type | commentary
!>   -------------|-------------------------
!>   list_type    | Object holding doubly linked list structure and data.
!>   node_operator_type    | Objects for external manipulation of list elements.
!>
!> * Public interface (these function may be defined and passed by user)
!>   interfadce      | commentary
!>   ----------------|-------------------------
!>   list_sort_func  | sort linked list element
!>   list_apply_proc | this subroutine is apply all elements
!>   obj_show_proc   | print routine. this is useful to show user defined type.
!> .
!>
!> Coding Policy
!> ==========================
!> Uses the Object Oriented Programing model incorporated in fortran 2003.
!> Encapsulation is taken into consideration, so that the list structure cannot be
!> directly modified from the outside. Therefore, list operations should be
!> performed throuth public methods.
!>
!> Example Usage
!> ==========================
!>@code
!>program test
!>  use flinkedlist
!>  implicit none
!>  type(list_type) :: samplelist
!>  type(node_operator_type) :: element_ptr
!>
!>  !... process begin ...
!>  call samplelist%append(2d0)    !append element(real64) in the *samplelist*
!>  call samplelist%append(.TRUE.) !append element(logical) in the *samplelist*
!>  call samplelist%showall()      !show all elements in the *samplelist*
!>  !... process end ...
!>end program
!>@endcode
!>
module flinkedlist
  !$ use omp_lib
  use iso_fortran_env
  implicit none
  private
  !public list_type
  !public node_operator_type      !$B%j%9%HMWAG$r;X$9%]%$%s%?$r4^$`%*%V%8%'%/%H(B
  !--subroutinies
  public obj_show        !$BAH$_9~$_7?$NI=<((B+$B%*%W%7%g%s$GL58BB?Aj7?$NI=<((B
  !-- function interfaces
  public list_sort_func  !$B%=!<%H4X?t$N%$%s%?!<%U%'!<%9(B
  public list_apply_proc !apply $B4X?t$N%$%s%?!<%U%'!<%9(B
  public obj_show_proc   !$BL58BB?Aj@-%*%V%8%'%/%HI=<($N%$%s%?!<%U%'!<%9(B

  !---------
  !>@brief Objects of list elements
  !>
  !>$B%b%8%e!<%kFb$G;HMQ$5$l$k%j%9%HMWAG(B
  !>@note $B%*%V%8%'%/%H;X8~$J$N$G(Bfortran2003,2008$B$N5!G=I,?\!#(B
  type,private :: node
    class(*)   ,pointer,private :: obj=>null() !<$B%j%9%HMWAG$N<BBN$X$N%]%$%s%?(B
    type(node)  ,pointer,private :: nxt=>null() !<$B<!MWAG$X$N%j%s%/(B
    type(node)  ,pointer,private :: bef=>null() !<$BA0MWAG$X$N%j%s%/(B
    class(list_type),pointer,private :: parent=>null() !<$B%j%9%H$rJ];}$9$k?F%*%V%8%'%/%H$X$N%]%$%s%?(B
    contains
      final :: node_final
      procedure,private :: node_equal !< user defined assignment procedure
      generic :: assignment(=) => node_equal !<$B%f!<%6!<Dj5ABeF~A`:n(B(generic,assignment)
  end type
  !---------
  !>@brief $B%j%9%HMWAG%*%V%8%'%/%H(B($B30It$+$i$NA`:n5!G=IU$-(B)
  !>
  !>$B$3$N7?$rMxMQ$9$kA0$K%j%9%HMWAG$r<($9$h$&$K=i4|2=$5$l$F$$$kI,MW$,$"$k!#(B
  !>init$B%a%=%C%I$G=i4|2=2DG=!#(B
  type,public :: node_operator_type
    type(node)  ,pointer,private :: pos => null()  !<$B%j%9%HMWAG$X$N%]%$%s%?(B
    class(list_type),pointer,private :: parent=>null() !<$B?F%*%V%8%'%/%H$X$N%]%$%s%?(B
    contains
      final :: node_operator_type_final !<node_operator_type$B7?$N%G%9%H%i%/%?(B
      procedure,non_overridable,public :: init    => node_operator_type_head !<$B;X<(@h$r%j%9%H@hF,$KLa$9(B
      procedure,non_overridable,public :: tail    => node_operator_type_tail !<$B;X<(@h$r%j%9%H:G8eHx$K(B
      procedure,non_overridable,public :: next    => node_operator_type_next !<$B;X<(@h$r0l$D<!$K0\$9(B
      procedure,non_overridable,public :: prev    => node_operator_type_previous !<$B;X<(@h$r0l$DA0$K0\$9(B
      procedure,non_overridable,public :: getobj  => node_operator_type_getobj   !<$B;X<(@h$NMWAG$X$N%]%$%s%?$rF@$k(B
  end type
  !---------
  !>@brief $B%j%s%/%j%9%H$rJ];}$9$k%*%V%8%'%/%H!#(B
  type,public :: list_type
    type(node),pointer,private :: head=>null() !<$B%j%9%H@hF,$X$N%]%$%s%?(B
    integer,private :: num=0         !<$B%j%9%HMWAG?t(B
    contains
      final :: list_final !<list_type$B7?$N%G%9%H%i%/%?(B
      procedure,non_overridable,public :: append    => list_append  !<
        !<append a element (subroutine,impure elemental)
      procedure,non_overridable,public :: delete    => node_delete !<
        !<delete a element (subroutine,impure elemental)
      procedure,non_overridable,public :: delall    => list_delall !<
        !<delete all elements (subroutine,pure elemental)
      procedure,non_overridable,public :: count     => list_count_node !<
        !<count how many elements are there (subroutine,pure elemental)
      procedure,non_overridable,public :: showall   => list_showall !<
        !<show all elements (subroutine)
      procedure,non_overridable,public :: apply     => list_apply !<
        !<apply user defined routine in each elements (subroutine)
      procedure,non_overridable,public :: sort      => list_sort !<
        !<sort elements by using user defined routine (subroutine)
      procedure,non_overridable,public :: listarray => list_elem_pointer_array !<
        !<$B%j%9%HMWAG$r;X$9G[Ns$r:n$k(B(function)
      procedure,non_overridable,nopass,public :: show => node_show !<
        !<$B;XDj$5$l$?MWAG(B1$B$D$rI=<((B(subroutine)
      procedure,non_overridable,private :: copy      => list_copy !<
        !<$B%f!<%6!<Dj5ABeF~A`:n$N<BBN(B
      generic :: assignment(=) => copy !<
        !<$B%f!<%6!<Dj5ABeF~A`:n(B($B%j%9%H$N%3%T!<(B)
  end type

  !----
  interface
    !>@brief $B%j%9%H$r%=!<%H$9$k$?$a$N4X?t(B
    !>@param[in] one $B%=!<%H$GHf3S$9$kMWAG(B1
    !>@param[in] two $B%=!<%H$GHf3S$9$kMWAG(B2
    !>@param[in] passdata (optional)$BHf3S$N0Y$K;H$&DI2C%G!<%?(B
    !>@retval is_swap one$B$H(Btwo$B$rF~$lBX$($k$H$-(BTRUE
    function list_sort_func(one,two,passdata)result(is_swap)
      class(*),intent(in) :: one,two
      class(*),intent(in),optional :: passdata
      logical :: is_swap
    end function
    !>@brief apply$B4X?t$GM?$($k4X?t$N7?(B
    !>@param[inout] obj $BA`:nBP>]$N%*%V%8%'%/%H(B($B%j%9%HMWAG(B)
    !>@param[in] passdata (optional)$BDI2C%G!<%?$,I,MW$J;~$K;H$&(B
    subroutine list_apply_proc(obj,passdata)
      class(*),intent(inout) :: obj
      class(*),intent(in),optional :: passdata
    end subroutine
    !>@brief obj_show$B%k!<%A%s$G%f!<%6!<Dj5A7?$rI=<($9$k4X?t(B
    !>@param[in] obj $BA`:nBP>]$N%*%V%8%'%/%H(B($B%j%9%HMWAG(B)
    !>@param[in] passdata (optional)$BDI2C%G!<%?$,I,MW$J;~$K;H$&(B
    !>@param[in] fid $B%U%!%$%k(Bid
    subroutine obj_show_proc(obj,passdata,fid)
      class(*),intent(in) :: obj
      class(*),intent(in),optional :: passdata
      integer ,intent(in),optional :: fid
    end subroutine
  end interface
  contains
    !--------------------------------
    !>@brief node_operator_type$B$,;X$7<($9%j%9%HMWAG$r=i4|2=$9$k(B
    !>
    !>@param[inout] self node_operator_type$B7?(B
    !>@param[in] list $B?F$H$J$k(Blist_type$B7?(B
    impure elemental subroutine node_operator_type_head(self,list)
      class(node_operator_type),intent(inout) :: self
      class(list_type),intent(in),target :: list
      self%parent=>list
      if(associated(list%head))then
        self%pos=>list%head
      else
        self%pos=>null()
        !write(error_unit,*) "wornning!! This list doesn't have the head element."
      endif
    end subroutine
    !--------------------------------
    !>@brief node_operator_type$B$,;X$7<($9%j%9%HMWAG$r:G8eHx$K$9$k(B
    !>
    !>@param[inout] self node_operator_type$B7?(B
    impure elemental subroutine node_operator_type_tail(self)
      class(node_operator_type),intent(inout) :: self
      !integer :: i,n
      call node_operator_type_check_parent(self)
      if(.not.associated(self%pos)) return
      !n=self%parent%count()
      !do i=1,n
      do while(associated(self%pos%nxt))
        call self%next()
      enddo
    end subroutine
    !--------------------------------
    !>@brief node_operator_type$B$,;X$7<($9MWAG$r0l$D<!$K?J$a$k(B
    !>
    !>@param[inout] self $BA`:nBP>]$N(Bnode_operator_type$B7?(B
    impure elemental subroutine node_operator_type_next(self)
      class(node_operator_type),intent(inout),target :: self
      call node_operator_type_check_parent(self)
      if(.not.associated(self%pos)) return
      if(associated(self%pos%nxt))then
        self%pos=>self%pos%nxt
      else
        !self%ipos_ptr=>self%head
      endif
    end subroutine
    !--------------------------------
    !>@brief node_operator_type$B$,;X$7<($9MWAG$r0l$DA0$K?J$a$k(B
    !>
    !>@param[inout] self $BA`:nBP>]$N(Bnode_operator_type$B7?(B
    impure elemental subroutine node_operator_type_previous(self)
      class(node_operator_type),intent(inout),target :: self
      call node_operator_type_check_parent(self)
      if(.not.associated(self%pos)) return
      if(associated(self%pos%bef))then
        self%pos=>self%pos%bef
      else
        !self%ipos_ptr=>self%head
      endif
    end subroutine
    !--------------------------------
    !>@brief node_operator_type$B$,;X$7<($9MWAG$rD>$K;X$9%]%$%s%?$rJV$9(B
    !>
    !>@param[in] self $BA`:nBP>]$N(Bnode_operator_type$B7?(B
    !>@retval res $BL58BB?Aj@-$N%*%V%8%'%/%H$r<($9%]%$%s%?(B
    function node_operator_type_getobj(self)result(res)
      class(node_operator_type),intent(in) :: self
      class(*),pointer :: res
      res=>null()
      if(.not.associated(self%pos))return
      if(.not.associated(self%pos%obj))return
      !allocate(res,source=self%pos%obj)
      res=>self%pos%obj
    end function
    !--------------------------------
    !>@brief node_operator_type$B$,?F%j%9%H$r;}$D$+$I$&$+%A%'%C%/$9$k(B
    !>
    !> - $BMWAG$N4XO"IU$1$J$7!#?F%j%9%H$N4XO"IU$1$J$7!#(B:$B=hM}$rCfCG!#(B
    !> - $BMWAG$N4XO"IU$1$J$7!#?F%j%9%H$N4XO"IU$1$"$j!#(B:$BMWAG$r%j%9%H@hF,$K4XO"IU$1!#(B
    !> - $BMWAG$N4XO"IU$1$"$j!#?F%j%9%H$N4XO"IU$1$J$7!#(B:$B7Y9p$rI=<($7$FB39T!#(B
    !> .
    !>@param[in] elpt $BA`:nBP>]$N(Bnode_operator_type$B7?(B
    impure elemental subroutine node_operator_type_check_parent(elpt)
      class(node_operator_type),intent(inout) :: elpt
      if(.not.associated(elpt%pos))then
        if(associated(elpt%parent))then
          elpt%pos=>elpt%parent%head
        else
          stop "@node_operator_type_check_parent"
        endif
     !else
     !  if(.not.associated(elpt%pos%parent))then
     !    write(error_unit,*) &
     !      "warnning!! This node_operator_type doesn't have the parent type 'list'."
     !  endif
      endif
    end subroutine
    !--------------------------------
    !>@brief node_operator_type$B7?$N%G%9%H%i%/%?(B
    !>
    !>@param[in] self node_operator_type$B7?(B
    pure elemental subroutine node_operator_type_final(self)
      type(node_operator_type),intent(inout) :: self
      self%pos=>null()
      self%parent=>null()
    end subroutine
    !--------------------------------
    !>@brief $B%$%3!<%k$N1i;;;R$G%j%9%H$NMWAG$r%3%T!<$9$k(B
    !>
    !>@param[out] left $B%$%3!<%k$N:8B&(B
    !>@param[in]  right $B%$%3!<%k$N1&B&(B
    impure elemental subroutine node_equal(left,right)
      class(node),intent(out) :: left
      class(node),intent(in)  :: right
      if(associated(right%obj))allocate(left%obj,source=right%obj)
      left%nxt=>right%nxt
      left%bef=>right%bef
      left%parent=>right%parent !$BMWAG$N%3%T!<$@$1H/@8$9$k>l9g(B,$B?F$OJQ$($J$$(B
    end subroutine
    !--------------------------------
    !>@brief node$B7?$N%G%9%H%i%/%?(B
    !>
    !>@param[in] self node$B7?(B
    impure elemental subroutine node_final(self)
      type(node),intent(inout) :: self
      !print*,"dealloc node"
      if(associated(self%obj))deallocate(self%obj)
      self%nxt=>null()
      self%bef=>null()
      self%parent=>null()
    end subroutine
    !--------------------------------
    !>@brief nodeptr$B7?$,<($9%*%V%8%'%/%H$rI=<((B
    !>
    !>@param[inout] mydata node_operator_type$B7?(B
    !>@param[in] showproc obj_show_proc$B%$%s%?!<%U%'!<%9$G<($5$l$k0z?t$r;}$D4X?t(B
    !>@param[in] passdata (optional)$BI=<($G;H$&%*%W%7%g%s%G!<%?(B
    !>@param[in] fid $B%U%!%$%k(Bid
    subroutine node_show(mydata,showproc,passdata,fid)
      class(node_operator_type),intent(inout) :: mydata
      procedure(obj_show_proc),optional :: showproc
      class(*),intent(in),optional :: passdata
      integer ,intent(in),optional :: fid
      if(.not.associated(mydata%pos))return
      call node_operator_type_check_parent(mydata)
      call obj_show(mydata%pos%obj,printobj=showproc,passdata=passdata,fid=fid)
    end subroutine
    !--------------------------------
    !>@brief list_type$B7?$NA4MWAG$r:o=|$9$k(B
    !>
    !>@param[inout] self list_type$B7?(B
    impure elemental subroutine list_delall(self)
      class(list_type),intent(inout) :: self
      type(node),pointer :: tmp,inode
      integer :: i
      i=0
      inode=>self%head
      do
        if(associated(inode))then
          !print*,"deallocate list contents of ",i
          i=i+1
          tmp=>inode%nxt
          deallocate(inode)
          inode=>tmp
        else
          exit
        endif
      enddo
      !if(mod(i,100000)==1) print*,"list_delall",i
      self%num=0
      self%head=>null()
    end subroutine
    !--------------------------------
    !>@brief list_type$B7?$N%G%9%H%i%/%?(B
    !>
    !>@param[inout] self list_type$B7?(B
    impure elemental subroutine list_final(self)
      type(list_type),intent(inout) :: self
      !type(node),pointer :: tmp,inode
      !integer,save :: icnt=0
      call self%delall()
     !icnt=icnt+1
     !if(mod(icnt,100000)==1)then
     !  print*,"list_final call",icnt
     !endif
    end subroutine
    !--------------------------------
    !>@brief list_type$B7?$X$NMWAGDI2C4X?t(B
    !>
    !>@param[inout] self list_type$B7?(B
    !>@param[in] obj $B%j%9%H@hF,$KMWAG(B($B2?$G$bNI$$(B)$B$rDI2C$9$k(B
    !>@param[inout] addloc (optinal)node_operator_type$B$N;X<(@h$N<!$KMWAG(Bobj$B$rDI2C$9$k(B
    subroutine list_append(self,obj,addloc)
      class(list_type),intent(inout),target :: self
      class(*),intent(in) :: obj
      type(node_operator_type),intent(inout),optional :: addloc
      type(node),pointer :: add,tmp
      allocate(node::add)
      allocate(add%obj,source=obj)
      add%parent=>self !$BMWAG$N?F%j%9%H$r<($9(B
      if(present(addloc))then
        !list$BCf$N(Baddloc$B$N<!$N0LCV$K(Bnode$B$rA^F~(B
        if(.not.associated(addloc%parent))then
          write(error_unit,*)"$BMWAG$rDI2C$9$k%j%9%H$,;XDj$5$l$F$$$^$;$s(B"
          write(error_unit,*)"$B%G!<%?$O%j%9%H$KDI2C$5$l$^$;$s$G$7$?!#(B"
          return
        endif
        if(.not.associated(addloc%pos))then
          write(error_unit,*)"$BDI2C$9$k>l=j$N%]%$%s%?$,6u$G$9(B"
          write(error_unit,*)"$B%G!<%?$O%j%9%H$KDI2C$5$l$^$;$s$G$7$?!#(B"
          return
        else
          !$BDI2C(B
          add%nxt=>addloc%pos%nxt
          addloc%pos%nxt=>add      !$B$3$3$G(Baddloc$B$N%]%$%s%?;X<(@h$,=q$-49$($i$l$k(B
          add%bef=>addloc%pos
        endif
      else
        !head$B$KA^F~(B
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
    !>@brief $B%j%9%H$NA4MWAG$K4X?t$rE,MQ$9$k(B
    !>
    !>@param[in] self list_type$B7?(B
    !>@param[in] applyproc (list_apply_proc)$B%$%s%?!<%U%'!<%9$r;}$D%f!<%6!<Dj5A%k!<%A%s(B
    !>@param[in] passdata (optinal)applyproc$B%k!<%A%s$GDI2C%G!<%?$rMxMQ$9$k>l9g$K;HMQ(B
    !>@param[in] parallel (optional)$BJBNs<B9T$7$?$$$H$-$K(B.TRUE.$B$r;XDj(B
    !>@note R$B$N(Bapply$B4X?t$HF1MM$NF0:n$r0U?^$7$F:n@.$7$?!#(B
    subroutine list_apply(self,applyproc,passdata,parallel)
      class(list_type),intent(inout) :: self
      procedure(list_apply_proc) :: applyproc
      class(*),intent(in),optional :: passdata
      logical,intent(in),optional :: parallel
      type(node_operator_type) :: ipt
      integer :: i
      logical :: do_para
      type(node_operator_type),dimension(:),allocatable :: temp

      do_para=.false.
      if(present(parallel))then
        if(parallel) do_para=.true.
      endif

      if(do_para)then
        !$BJBNs<B9T(B(OpenMP)
          temp=self%listarray()
          !$omp parallel
          !$omp do
          do i=1,size(temp)
            !$B4X?t$r%j%9%HMWAG$KE,MQ(B
            call applyproc(temp(i)%pos%obj,passdata=passdata)
          enddo
          !$omp end do
          !print *, "Hello! N =", omp_get_num_threads(), " and I am ", omp_get_thread_num()
          !$omp end parallel
      else
        !$BC`<!<B9T(B
        call ipt%init(self)
        do i=1,self%num
          !$B4X?t$r%j%9%HMWAG$KE,MQ(B
          call applyproc(ipt%pos%obj,passdata=passdata)
          call ipt%next()
        enddo
      endif
    end subroutine
    !--------------------------------
    !>@brief $B%j%9%HMWAG$rA4$FI=<($9$k(B
    !>
    !>@param[in] self list_type$B7?(B
    !>@param[in] showproc (optinal)$B%f!<%6!<Dj5A7?$rI=<($9$k%k!<%A%s(B
    !>@param[in] passdata (optional)$BI=<($K;H$&DI2C%G!<%?(B
    !>@param[in] fid $B%U%!%$%k(Bid
    subroutine list_showall(self,showproc,passdata,fid)
      class(list_type),intent(in),target :: self
      procedure(obj_show_proc),optional :: showproc
      class(*),intent(in),optional :: passdata
      integer,intent(in),optional :: fid
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
          i,self%num
        call self%show(ipt,showproc=showproc,passdata=passdata,fid=fileid)
        call ipt%next()
      enddo
    end subroutine
    !--------------------------------
    !>@brief $B%j%9%H$+$iMWAG$r:o=|$9$k(B
    !>
    !>$B0z?t$KM?$($?%]%$%s%?$,<($9@h$NMWAG$r%j%9%H$+$i:o=|$9$k!#(B
    !>@param[in] self list_type$B7?(B
    !>@param[inout] delnode $B%j%9%HMWAG$N%]%$%s%?(B
    subroutine node_delete(self,delnode)
      class(list_type),intent(in),target :: self
      class(node_operator_type),intent(inout) :: delnode
      type(node),pointer :: bef,nxt
      class(list_type),pointer :: plist
      !$B%]%$%s%?;X<(@h$N3NG'(B
      if(.not.associated(delnode%pos))return
      if(.not.associated(delnode%parent,target=self))then
        !print*,"$BDI2C$9$k>l=j$O%j%9%H$NMWAG$G$O$"$j$^$;$s(B"
        print*,"$B:o=|$G$-$^$;$s$G$7$?!#(B"
        return
      else
        !$B:o=|(B
        plist=>delnode%parent
        plist%num=plist%num-1
        bef=>delnode%pos%bef
        nxt=>delnode%pos%nxt
        if(associated(bef))then
          bef%nxt=>nxt
        else
          !$B%j%9%H@hF,$r:o=|$9$k$N$G(Bhead$B$N;Y;}@h$r=$@5(B
          plist%head=>nxt
        endif
        if(associated(nxt))then
          nxt%bef=>bef
        endif
        deallocate(delnode%pos)
        delnode%pos=>nxt
      endif
    end subroutine
    !--------------------------------
    !>@brief $B%j%9%HMWAG?t$rJV$94X?t(B
    !>
    !>@param[in] self list_type$B7?(B
    pure elemental function list_count_node(self)result(n)
      class(list_type),intent(in) :: self
      integer :: n
      n=self%num
    end function
    !--------------------------------
    !>@brief $B%j%9%H$N%3%T!<(B($B%a%b%j$r?7$7$/%"%m%1!<%H(B)
    !>
    !>@param[in] right $B%$%3!<%k$N1&(B
    !>@param[in] left  $B%$%3!<%k$N:8(B
    impure elemental subroutine list_copy(left,right)
      class(list_type),intent(in) :: right
      class(list_type),intent(out) :: left
      type(node_operator_type) :: elpr
      integer :: i,n
      !$B%3%T!<85$NMWAG$r;X$9(Belpr$B$r=i4|2=(B
      call elpr%init(right)
      !$B%j%9%H:G8eHx$r;X$9>uBV$K$9$k(B
      call elpr%tail()
      !$B%j%9%H$r0l$D$:$DDI2C(B
      n=right%count()
      do i=1,n
        call left%append(elpr%pos%obj)
        call elpr%prev()
      enddo
    end subroutine
    !--------------------------------
    !>@brief $B%P%V%k%=!<%H$N<B9T(B
    !>
    !>@param[inout] self $B%=!<%H$r<B9T$9$k(B
    !>@param[in] func list_sort_func$B%$%s%?!<%U%'!<%9$r;}$D%5%V%k!<%A%s(B
    !>@param[in] passdata (optional)$B%=!<%H$G;H$&DI2C%G!<%?(B
    subroutine list_sort(self,func,passdata)
      class(list_type),intent(inout),target :: self
      procedure(list_sort_func) :: func
      class(*),intent(in),optional :: passdata
      class(node),pointer :: ipt
      class(node),pointer :: jpt
      class(*),pointer :: tmpptr
      !class(*),allocatable :: tmp
      !$BMWAG?t$,(B1$B0J2<$J$i(Bsort$B$7$J$/$FNI$$(B
      if(self%num<=1)return
      ipt=>null()
      jpt=>null()
      !func(one,two)result(is_swap)
      !one,two$B$N#2$D$N=gHV$rF~$lBX$($k$H$-$K(Bis_swap$B$,(BTRUE$B$H$J$k(B
      ipt=>self%head
      do
        if(.not.associated(ipt))exit
        jpt=>ipt%nxt
        do
          if(.not.associated(jpt))exit
          !$BF~$lBX$($N%A%'%C%/(B
          if(func(ipt%obj,jpt%obj,passdata))then
            !$BF~$lBX$((Bswap
            !allocate(tmp,source=ipt%obj)
            !call move_alloc(from=jpt%obj,to=ipt%obj)
            !call move_alloc(from=tmp,to=jpt%obj)
            !$B%j%s%/$N=$@5$NJ}$,Aa$$(B
            tmpptr=>ipt%obj
            ipt%obj=>jpt%obj
            jpt%obj=>tmpptr
          endif
          !$B%]%$%s%?$r$9$9$a$k(B
          jpt=>jpt%nxt
        enddo
        ipt=>ipt%nxt
      enddo
    end subroutine
    !--------------------------------
    !>@brief $B%j%9%HMWAG$rG[Ns$H$7$F07$&$?$a$N(Bnode_operator_type$B7?G[Ns$N@8@.%k!<%A%s(B
    !>
    !>@param[in] self $B%j%9%H(B
    !>@retval res node_operator_type$BG[Ns(B
    function list_elem_pointer_array(self)result(res)
      class(list_type),intent(in),target :: self
      type(node_operator_type),dimension(:),allocatable :: res
      type(node),pointer :: tmp
      integer :: i
      allocate(res(self%num))
      if(self%num<=0)return
      call res(:)%init(self) !initialize elementary
      tmp=>self%head
      do i=1,self%num
        res(i)%pos=>tmp
        tmp=>tmp%nxt
      enddo
    end function
    !--------------------------------
    !>@brief $BL58BB?Aj@-%*%V%8%'%/%H$rI=<($9$k%k!<%A%s(B
    !>
    !>@param[in] obj $BMWAG(B
    !>@param[in] printobj (optional)obj_show_proc$B%$%s%?!<%U%'!<%9$r;}$D(B
    !>                  $B%f!<%6!<Dj5A7?$rI=<($9$k>l9g$N<jB3$-(B
    !>@param[in] passdata (optional)printobj$B$G;H$&DI2C%G!<%?(B
    !>@param[in] fid (optional)$B%U%!%$%k(Bid
    subroutine obj_show(obj,printobj,passdata,fid)
      class(*),intent(in)::obj
      procedure(obj_show_proc),optional::printobj
      class(*),intent(in),optional :: passdata
      integer ,intent(in),optional :: fid
      integer :: fileid
      !$B%U%!%$%k(Bid$B$N@_Dj(B
      if(present(fid))then
        fileid=fid
      else
        fileid=output_unit
      endif
      !$BI=<(%k!<%A%s(B
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
end module flinkedlist
