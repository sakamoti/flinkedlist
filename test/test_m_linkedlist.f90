program main
    use m_linkedlist
    use mynode
    use iso_fortran_env
    implicit none
    integer :: i
    type(list_t) :: list, extend_list
    double precision :: ts, te
    class(node_t), allocatable, target ::  node_copied
    class(node_t), pointer ::  node_ptr
    character(len=33),parameter :: cputime_format_str = '("    cputime=",en11.3e1,"[sec]")'
    write(output_unit,*) "!=== LINKEDLIST TEST =================="
    do i=1,2
        call list%append(i32node_factory(i))
        call list%append(r64node_factory(real(i, kind=real64)))
        call list%append(circle_node_factory(real(i, kind=real64)))
        call list%append(rectangle_node_factory(real(i, kind=real64),real(i*3, kind=real64)))
    end do

    call list%insert(3,circle_node_factory(10d0)) ! insert node to 3rd position
    call list%remove(3)          ! delete 3rd node 
    call list%insert(1,rectangle_node_factory(5d0,10d0)) ! insert node to head
    call list%remove(1)          ! delete 1st node 
    !
    write(output_unit,*) "Show numbers of nodes in the list:",list%entries()
    write(output_unit,*) "!---loop head to tail with index ----"
    call cpu_time(ts)
    do i=1,list%entries()
      call list%get_nodeclass(i,node_copied)
      call write_line(i, node_copied)
    end do
    call cpu_time(te)
    write(output_unit,fmt=cputime_format_str) te-ts

    call cpu_time(ts)
    call show_with_ptr_head_to_tail(list)
    call cpu_time(te)
    write(output_unit,fmt=cputime_format_str) te-ts

    call cpu_time(ts)
    call show_with_ptr_tail_to_head(list) 
    call cpu_time(te)
    write(output_unit,fmt=cputime_format_str) te-ts

    call list%pop_tail(node_ptr)
    call list%pop_head(node_ptr)
    write(output_unit,*) "!-----------------------------------"
    write(output_unit,*) "After popright and popleft:",list%entries()
    call show_with_ptr_head_to_tail(list)

    write(output_unit,*) "!-----------------------------------"
    call extend_list%append(i32node_factory(i))
    call extend_list%append(r64node_factory(real(i, kind=real64)))
    call extend_list%append(circle_node_factory(real(i, kind=real64)))
    call extend_list%append(rectangle_node_factory(real(i, kind=real64),real(i*3, kind=real64)))
    write(output_unit,*) "Before extend list:",list%entries(),extend_list%entries()
    call list%extend(extend_list)
    write(output_unit,*) "After extend list:",list%entries(),extend_list%entries()
    call show_with_ptr_head_to_tail(list)

    call list%clear()
    write(output_unit,*) "!-----------------------------------"
    write(output_unit,*) "After clearing list, Entries number in the list is:",list%entries()
  contains
    subroutine show_with_ptr_head_to_tail(self)
      class(list_t),intent(inout) :: self
      write(output_unit,*) "!--- show list elements (head to tail)---"
      call self%set_current_node_to_head()
      i=1
      do while(.not.self%is_current_node_null())
        call self%get_current_nodeclass(node_ptr)
        call write_line(i, node_ptr)
        call self%next()
        i = i + 1
      end do
    end subroutine
    subroutine show_with_ptr_tail_to_head(self)
      class(list_t),intent(inout) :: self
      write(output_unit,*) "!--- show list elements (tail to head)---"
      call self%set_current_node_to_tail()
      i=self%entries()
      do while(.not.self%is_current_node_null())
        call self%get_current_nodeclass(node_ptr)
        call write_line(i, node_ptr)
        call self%before()
        i = i - 1
      end do
    end subroutine
    subroutine write_line(loc, node)
      integer, intent(in) :: loc
      class(node_t), intent(in) :: node
      select type(node)
      class is(i_node_with_io_t)
        write(output_unit,fmt='(i5,") ",dt)')loc,node
      end select
    end subroutine
end program main
