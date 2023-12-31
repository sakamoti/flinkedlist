module m_linkedlist_sametype
    use iso_fortran_env
    implicit none

    type,abstract :: node_t
        class(node_t),pointer,private :: nxt => null()
        class(node_t),pointer,private :: bef => null()
        contains
    end type

    type,abstract :: list_t
        !! you will extend this type because get_nodeclass only get class(node_t).
        !! class(node_t) need `select type` to get extended type.
        !! so, this type is `abstract type` and you should add your get method.
        integer(int64),private :: n_elements = 0
        class(node_t),pointer,private :: head => null()
        class(node_t),pointer,private :: tail => null()
        contains
        procedure,non_overridable,public :: entries => get_the_numver_of_entries
        procedure,non_overridable,public :: append => list_t_append
        procedure,non_overridable,public :: insert => list_t_insert
        procedure,non_overridable,public :: remove => list_t_remove_idx
        procedure,non_overridable,public :: clear => list_t_clear
        procedure,non_overridable,public :: get_nodeclass => list_t_get_node
    end type

    contains

    integer function get_the_numver_of_entries(self)
        class(list_t),intent(in) :: self
        get_the_numver_of_entries = self%n_elements
    end function

    subroutine list_t_append(self, node)
        class(list_t),intent(inout) :: self
        class(node_t),intent(in) :: node
        class(node_t),pointer :: node_copy
        allocate(node_copy,source=node)
        ! add data to the tail position
        if(.not.associated(self%tail))then
            self%head => node_copy
            self%tail => node_copy
        else
            node_copy%nxt => null()
            node_copy%bef => self%tail
            self%tail%nxt => node_copy
            self%tail => node_copy
        endif
        self%n_elements = self%n_elements + 1
    end subroutine

    subroutine list_t_insert(self, loc, node)
        class(list_t),intent(inout) :: self
        integer,intent(in) :: loc
        class(node_t),intent(in) :: node
        class(node_t),pointer :: node_ptr
        class(node_t),pointer :: node_copy
        integer :: i
        allocate(node_copy,source=node)
        ! add data to the tail position
        if(loc<=0 .or. self%n_elements < loc)then
            write(*,'("INSERT WARNING :: index[",i0,"] is out of range[",i0,"]")') ,loc,self%n_elements
            return
        end if
        if(loc==1)then
            node_copy%bef => null()
            node_copy%nxt => self%head
            self%head => node_copy
            node_copy%nxt%bef => node_copy
            self%n_elements = self%n_elements + 1
            return
        end if
        node_ptr => self%head
        do i=2,loc-1
            node_ptr => node_ptr%nxt
        end do
        node_copy%bef => node_ptr
        node_copy%nxt => node_ptr%nxt
        node_ptr%nxt%bef => node_copy
        node_ptr%nxt => node_copy
        self%n_elements = self%n_elements + 1
    end subroutine

    subroutine list_t_clear(self)
        class(list_t),intent(inout) :: self
        class(node_t),pointer :: node_ptr, node_ptr_nxt
        node_ptr => self%head
        do while(associated(node_ptr)) 
            node_ptr_nxt => node_ptr%nxt
            deallocate(node_ptr)
            self%n_elements = self%n_elements - 1
            node_ptr => node_ptr_nxt
        end do
    end subroutine

    subroutine list_t_remove_idx(self, loc)
        class(list_t),intent(inout) :: self
        integer,intent(in) :: loc
        class(node_t),pointer :: node_ptr
        integer :: i
        if(loc<=0 .or. self%n_elements < loc)then
            write(*,'("DELETE WARNING :: index[",i0,"] is out of range[",i0,"]")') ,loc,self%n_elements
            return
        else if(loc==1.and.self%n_elements/=1)then
            node_ptr => self%head
            self%head => node_ptr%nxt
            self%tail => null()
            deallocate(node_ptr)
            self%n_elements = self%n_elements - 1
            return
        else if(self%n_elements==1)then
            node_ptr => self%head
            self%head => null()
            self%tail => null()
            deallocate(node_ptr)
            self%n_elements = self%n_elements - 1
            return
        else if(self%n_elements==loc)then
            node_ptr => self%tail
            self%tail => node_ptr%bef
            self%tail%nxt => null()
            deallocate(node_ptr)
            self%n_elements = self%n_elements - 1
            return
        end if
        !
        node_ptr => self%head
        do i=2,loc
            node_ptr => node_ptr%nxt
        end do
        node_ptr%bef%nxt => node_ptr%nxt
        node_ptr%nxt%bef => node_ptr%bef
        deallocate(node_ptr)
        self%n_elements = self%n_elements - 1
    end subroutine

    subroutine list_t_get_node(self, loc, node_ptr)
        class(list_t),intent(inout) :: self
        integer,intent(in) :: loc
        class(node_t),pointer,intent(inout) :: node_ptr
        integer :: i
        node_ptr => self%head
        do i=2,loc
            node_ptr => node_ptr%nxt
        end do
    end subroutine
end module