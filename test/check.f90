program check
  use flinkedlist
  use iso_fortran_env
  implicit none
  type user_type
    integer :: i = 1
    real(kind=real32) :: x =1.6
  end type
  type(user_type) :: ud
  type(list_type) :: list
  !--- test routines
  call append_and_show(list)
  call copy_list(list)
  call node_operation(list)
  call transform_list2array(list)
  call list%delall()
  call convert_sametype_list2array()
  call example_sort()
  call example_apply()
  !---
  contains
    subroutine user_show_proc(obj,passdata,fid)
      class(*),intent(in) :: obj
      class(*),intent(in),optional :: passdata
      integer,intent(in),optional :: fid
      select type(obj)
      type is(user_type)
        print *,"user_type:", obj
      end select
    end subroutine

    subroutine transform_list2array(list)
      class(list_type) :: list
      type(node_operator_type),allocatable :: nodearray(:)
      class(*),pointer :: val_ptr
      integer :: i
      print*,"!===   list to node_operator_array (Head:1 ~ tail:10)"
      nodearray = list%listarray()
      do i=1,size(nodearray)
        write(output_unit,'(3x,i3)',advance='no') i
        call nodearray(i)%show(showproc=user_show_proc)
        call nodearray(i)%get_ptr(val_ptr)
      end do
    end subroutine

    subroutine append_and_show(list)
      class(list_type) :: list
      !... append data into the list ...
      call list%append(1_int32) !append element(int32) in the *samplelist*
      call list%append(32.0_real32)    !append element(real64) in the *samplelist*
      call list%append(64.0_real64)    !append element(real64) in the *samplelist*
      call list%append(128.0_real128)    !append element(real128) in the *samplelist*
      call list%append((3.0_real32,2.0_real32))    !append element(complex32) in the *samplelist*
      call list%append((6.0_real64,4.0_real64))    !append element(complex64) in the *samplelist*
      call list%append(.TRUE.) !append element(logical) in the *samplelist*
      call list%append("a")    !append element(character) in the *samplelist*
      call list%append("Hello Fortran Linked list!")    !append element(strings) in the *samplelist*
      call list%append(ud) !append element(user defined type) in the *samplelist*
      !---
      print *, "!-- show all (intrinsinc data type only)"
      call list%showall()  !show all elements in the *samplelist*
      print *, "!-- show all (add user defined type showing routine)"
      call list%showall(showproc=user_show_proc) !show all elements in the *samplelist*
    end subroutine

    subroutine copy_list(list)
      class(list_type) :: list
      type(list_type) :: list_copy
      type(node_operator_type) :: node
      !... copy the list to list_copy
      select type(list)
      class is (list_type)
        list_copy = list
      end select
      print *,"!--- copy_list --"
      print *,"Original list element number=", list%count()
      print *,"Copied list element number=", list_copy%count()
      !call list_copy%showall()
      !... initialize node_operator_type
      call node%init(list_copy)  ! associate list and node_operator_type
      !delete last 3 elements
      !-
      call node%tail() !move pointer to tail
      write(*,'(3x,A)',advance="no") "delete :"
      call list_copy%show(node)
      call list_copy%delete(node)
      !-
      call node%tail() !move pointer to tail
      write(*,'(3x,A)',advance="no") "delete :"
      call list_copy%show(node,showproc=user_show_proc)
      call list_copy%delete(node)
      !-
      call node%tail() !move pointer to tail
      write(*,'(3x,A)',advance="no") "delete :"
      call list_copy%show(node,showproc=user_show_proc)
      call list_copy%delete(node)
      print *,"  Copied list element number(after delete 3elements)=", list_copy%count()
      !-
      call list_copy%showall(showproc=user_show_proc) !move pointer to tail
      print *,"!--- end copy_list --"//new_line("")
    end subroutine

    subroutine node_operation(list)
      class(list_type) :: list
      type(node_operator_type) :: node
      class(*),allocatable :: val
      class(*),pointer :: val_ptr
      integer :: i
      !=== get value from list
      print *,"!--- node_operation --"
      call node%init(list) !initialize a operator
      call node%head()
      ! node pointer point 5th element from header
      do i=1,4
        call node%next()
      end do
      write(*,'("  4th element=")',advance='no') 
      call list%show(node) ! show inside the list object
      call node%get_alloc(val) !newly allocated value
      call node%get_ptr(val_ptr ) ! pointer points inside the list
      call obj_show(val)
      print *,"!--- end node_operation --"
    end subroutine

    subroutine convert_sametype_list2array()
      class(list_type),allocatable :: li
      type(node_operator_type),allocatable :: nodearray(:)
      type(user_type),allocatable :: ud(:)
      integer :: i,n
      allocate(list_type::li)
      n=8
      print*,"!===   convert_sametype_list2array (Head:1~Tail:n)"
      do i=1,n
        call li%append(user_type(i,real(i,kind=real32)))
      end do
      nodearray = li%listarray()
      print *,"! show values from node_operator_type",size(nodearray),li%count()
      do i=1,size(nodearray)
        write(output_unit,'(3x,i3)',advance='no') i
        call nodearray(i)%show(showproc=user_show_proc)
      end do
      ud=polimophicval2userdefinedtype(nodearray)
      print *,"! show values from user_type array"
      do i=1,size(ud)
        print*, ud(i)
      end do
      print*,"!===   end convert_sametype_list2array"
    end subroutine

    impure elemental function polimophicval2userdefinedtype(self) result(ud)
      type(node_operator_type),intent(in) :: self
      type(user_type) :: ud
      class(*),allocatable :: var
      call self%get_alloc(var)
      select type(var)
      type is(user_type)
        ud = var
      end select
    end function

    impure elemental function polimophicval2real(self) result(ud)
      type(node_operator_type),intent(in) :: self
      real(kind=real64) :: ud
      class(*),allocatable :: var
      call self%get_alloc(var)
      select type(var)
      type is(real(kind=real64))
        ud = var
      end select
    end function

    subroutine example_sort()
      type(list_type) :: sortedlist,reversedlist,originlist
      real(kind=real64) :: x(10)
      type(node_operator_type) :: n1,n2,n3
      class(*),pointer :: x1,x2,x3
      integer :: i
      call random_number(x)
      do i=1,size(x)
        call sortedlist%append(x(i))
      end do
      originlist=sortedlist !save original
      reversedlist=sortedlist !save original
      call sortedlist%sort(sortfun,.false.) !sort
      call reversedlist%sort(sortfun,.true.) !sort(reverse)
      call n1%init(originlist)
      call n2%init(sortedlist)
      call n3%init(reversedlist)
      print *,"!=== sort procedure usage example"
      print *, "  i x(original)    x(sorted)  x(reversed)"
      do i=1,size(x)
        call n1%get_ptr(x1)
        call n2%get_ptr(x2)
        call n3%get_ptr(x3)
        select type(x1)
        type is (real(kind=real64))
          select type(x2)
            type is (real(kind=real64))
            select type(x3)
              type is (real(kind=real64))
              print'(i3,3f13.9)', i,x1,x2,x3
            end select
          end select
        end select
        call n1%next()
        call n2%next()
        call n3%next()
      end do
      print *,"!=== END: sort procedure usage example"
    end subroutine
    
    logical function sortfun(one,two,passdata)
      class(*),intent(in) :: one,two
      class(*),intent(in),optional :: passdata
      select type(one)
      type is (real(kind=real64))
        select type(two)
        type is (real(kind=real64))
          if(one < two)then
            sortfun=.true.
          else
            sortfun=.false.
          end if
        end select
      end select
      !---
      select type(passdata)
      type is (logical)
        if(passdata) then
          sortfun= .not. sortfun
        end if
      end select
    end function

    subroutine example_apply()
      type(list_type),allocatable :: nlist(:)
      type(node_operator_type),allocatable :: nodeop(:,:)
      real(kind=real64),allocatable :: x(:,:)
      integer :: i,j
      allocate(nlist(5))
      do i=1,10
        do j=1,size(nlist)
          call nlist(j)%append(real(i,kind=real64))
        end do
      end do
      allocate(nodeop(10,5))
      allocate(x(10,5))
      do i=1,size(nlist)
        call nlist(i)%apply(applyproc,passdata=real(i,kind=real64),&
          & parallel=.true.)
        nodeop(:,i)=nlist(i)%listarray()
      end do
      x=polimophicval2real(nodeop)
      print *, "!=== apply function example"
      print *, "i-th    x1     x2     x3     x4     x5"
      do i=1,size(x(:,1))
        print'(i4,5f7.1)',i,x(i,:)
      end do
      print *, "!=== END: apply function example"
    end subroutine

    subroutine applyproc(obj,passdata)
      class(*),intent(inout) :: obj
      class(*),intent(in),optional :: passdata
      select type(obj)
      type is(real(kind=real64))
        select type(passdata)
        type is(real(kind=real64))
          obj=obj*passdata
        end select
      end select
    end subroutine
end program check
