program check
  use flinkedlist
  use iso_fortran_env
  implicit none
  type user_type
    integer :: i = 1
    real(kind=real32) :: x =1.6
  end type
  type(list_type) :: list
  type(list_type) :: list_copy
  type(node_operator_type) :: node
  type(user_type) :: ud
  class(*),pointer :: value
  integer :: i
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
  !... copy the list to list_copy
  list_copy = list
  print *,"!---"
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

  !=== get value from list
  call node%init(list)
  call node%head()
  ! node pointer point 5th element from header
  do i=1,4
    call node%next()
  end do
  write(*,'("4th element=")',advance='no') 
  call list%show(node)
  !value = list%getobj(node)
  !---
  call transform_list2array(list)
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
      class(*),allocatable :: val
      print*,"!===   list to node_operator_array (Head:1 ~ tail:10)"
      nodearray = list%listarray()
      do i=1,size(nodearray)
        write(output_unit,'(3x,i3)',advance='no') i
        call nodearray(i)%show(showproc=user_show_proc)
        val = nodearray(i)%get_alloc()
      end do
    end subroutine
end program check
