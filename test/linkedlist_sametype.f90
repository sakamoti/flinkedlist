module m_mylist
    use m_linkedlist_sametype
    implicit none

    type,extends(list_t) :: mylist
        contains
        procedure,public :: get_ptr => list_get_mynode
    end type

    type,extends(node_t) :: node_int_t
        integer(int8) :: i8 =1
        integer(int16) :: i16 =1
        integer(int32) :: i32 =1
        integer(int64) :: i64 =1
    end type
    contains

    subroutine list_get_mynode(self,loc,ptr)
        class(mylist),intent(inout) :: self
        integer,intent(in) :: loc
        class(node_t),pointer :: node_t_ptr
        type(node_int_t),pointer :: ptr
        ptr => null()
        call self%get_nodeclass(loc,node_t_ptr)
        select type(node_t_ptr)
        class is(node_int_t)
          ptr => node_t_ptr
        end select
        if(.not.associated(ptr))then
          error stop
        endif
    end subroutine
end module


program main
    use m_mylist
    implicit none
    integer :: i
    type(mylist) :: list
    type(node_int_t) :: node_int
    type(node_int_t),pointer :: node_int_ptr => null()

    print *,"!=== LINKEDLIST TEST =================="
    do i=1,10
      node_int%i8=i*1
      node_int%i16=i*10
      node_int%i32=i*100
      node_int%i64=i*1000
      call list%append(node_int)
    end do

    call list%remove(3)          ! delete 3rd node 
    call list%insert(3,node_int) ! insert node to 3rd position
    call list%insert(1,node_int) ! insert node to head
    !
    print *,"Entries number in the list:",list%entries()
    do i=1,list%entries()
      call list%get_ptr(i,node_int_ptr)
      print*,i,":",&
          node_int_ptr%i8,node_int_ptr%i16,&
          node_int_ptr%i32,node_int_ptr%i64
    end do
    !
    call list%clear()
    print*,"After clearing list, Entries number in the list is:",list%entries()
end program main
