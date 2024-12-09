program main
    use m_linkedlist
    use mynode
    use iso_fortran_env
    implicit none
    integer :: i
    type(list_t) :: list
    double precision :: ts, te
    class(node_t), allocatable, target ::  node_copied
    class(node_t), pointer ::  node_ptr

    print *,"!=== LINKEDLIST TEST =================="
    do i=1,3
        call list%append(r64node_factory(real(i, kind=real64)))
        call list%append(i32node_factory(i))
        call list%append(circle_node_factory(real(i, kind=real64)))
        call list%append(rectangle_node_factory(real(i, kind=real64),real(i*3, kind=real64)))
    end do

    call list%remove(3)          ! delete 3rd node 
    call list%insert(3,circle_node_factory(10d0)) ! insert node to 3rd position
    call list%insert(1,rectangle_node_factory(5d0,10d0)) ! insert node to head
    !
    print *,"Entries number in the list:",list%entries()
    call cpu_time(ts)
    do i=1,list%entries()
      call list%get_nodeclass(i,node_copied)
      !print*,i,":",&
    end do
    call cpu_time(te)
    print*, "cputime=",te-ts,"sec"

    print *, "!-----------------------------------"
    call cpu_time(te)
    print*, "cputime=",te-ts,"sec"
    !
    call list%clear()
    print*,"After clearing list, Entries number in the list is:",list%entries()
end program main
