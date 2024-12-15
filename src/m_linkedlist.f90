module m_linkedlist
    use iso_fortran_env
    implicit none
    private

    public node_t, list_t

    type,abstract :: node_t
        class(node_t),pointer,private :: nxt => null()
        class(node_t),pointer,private :: bef => null()
    end type

    type :: list_t
        integer(int64),private :: n_elements = 0
        class(node_t),pointer,private :: head => null()
        class(node_t),pointer,private :: tail => null()
        class(node_t),pointer,private :: current_node => null() ! for iteration
        integer,private :: warning_output_unit = error_unit
    contains
        ! private procedures
        procedure,private :: get_current_node_ptr => list_t_get_current_node_ptr
        procedure,private :: get_current_node_allocatable => list_t_get_current_node_allocatable
        procedure,private :: get_node_ptr => list_t_get_node_ptr
        procedure,private :: get_node_allocatable => list_t_get_node_allocatable
        procedure,non_overridable,private :: nullify => list_t_nullify
        ! public procedures
        procedure,non_overridable,public :: entries => get_the_numver_of_entries
        procedure,non_overridable,public :: pop_tail => list_t_pop_tail
        procedure,non_overridable,public :: pop_head => list_t_pop_head
        procedure,non_overridable,public :: append => list_t_append
        procedure,non_overridable,public :: extend => list_t_extend
        procedure,non_overridable,public :: insert => list_t_insert
        procedure,non_overridable,public :: remove => list_t_remove_idx
        procedure,non_overridable,public :: clear => list_t_clear
        procedure,non_overridable,public :: set_current_node_to_head => list_t_set_current_node_to_head
        procedure,non_overridable,public :: set_current_node_to_tail => list_t_set_current_node_to_tail
        procedure,non_overridable,public :: next => list_t_next_node
        procedure,non_overridable,public :: before => list_t_before_node
        procedure,non_overridable,public :: is_current_node_null => list_t_is_current_node_null
        generic,public :: get_nodeclass => get_node_ptr, get_node_allocatable
        generic,public :: get_current_nodeclass => get_current_node_ptr, get_current_node_allocatable 
    end type

    contains

    integer(int64) function get_the_numver_of_entries(self)
        class(list_t),intent(in) :: self
        get_the_numver_of_entries = self%n_elements
    end function

    subroutine list_t_pop_tail(self, node_ptr)
        class(list_t), intent(inout) :: self
        class(node_t), pointer :: node_ptr
        if(self%entries()>1)then
            call self%set_current_node_to_tail()
            call self%get_current_nodeclass(node_ptr)
            self%tail%bef%nxt => null()
            self%tail => node_ptr%bef
            self%n_elements = self%n_elements -1
        else if(self%entries()==1)then
            call self%set_current_node_to_tail()
            call self%get_current_nodeclass(node_ptr)
            self%n_elements = self%n_elements - 1
        end if
        if(self%entries()==0)then
            call self%nullify()
        end if
    end subroutine

    subroutine list_t_pop_head(self, node_ptr)
        class(list_t), intent(inout) :: self
        class(node_t), pointer :: node_ptr
        if(self%entries()>1)then
            call self%set_current_node_to_head()
            call self%get_current_nodeclass(node_ptr)
            self%head%nxt%bef => null()
            self%head => node_ptr%nxt
            self%n_elements = self%n_elements -1
        else if(self%entries()==1)then
            call self%set_current_node_to_head()
            call self%get_current_nodeclass(node_ptr)
            self%n_elements = self%n_elements -1
        end if
        if(self%entries()==0)then
            call self%nullify()
        end if
    end subroutine

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

    subroutine list_t_extend(self, extend_list)
        class(list_t),intent(inout) :: self
        class(list_t),intent(inout) :: extend_list
        if(extend_list%entries()==0)then
            return
        endif
        if(self%entries()==0)then
            self%head => extend_list%head
            self%tail => extend_list%tail
        else
            self%tail%nxt => extend_list%head
            self%tail => extend_list%tail
        end if
        call extend_list%nullify()
    end subroutine

    subroutine list_t_nullify(self)
        class(list_t),intent(inout) :: self
        ! nullify extend_list
        self%head => null()
        self%tail => null()
        self%current_node => null()
        self%n_elements = 0
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
            write(self%warning_output_unit,'("INSERT WARNING :: index[",i0,"] is out of range[",i0,"]")') loc,self%n_elements
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
        self%head => null()
        self%tail => null()
        self%current_node => null()
    end subroutine

    subroutine list_t_remove_idx(self, loc)
        class(list_t),intent(inout) :: self
        integer,intent(in) :: loc
        class(node_t),pointer :: node_ptr
        integer :: i
        if(loc<=0 .or. self%n_elements < loc)then
            write(self%warning_output_unit,'("DELETE WARNING :: index[",i0,"] is out of range[",i0,"]")') loc,self%n_elements
            return
        else if(loc==1.and.self%n_elements>1)then
            node_ptr => self%head
            self%head => node_ptr%nxt
            self%head%bef => null()
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

    subroutine list_t_get_node_ptr(self, loc, node_ptr)
        class(list_t),intent(inout) :: self
        integer,intent(in) :: loc
        class(node_t),pointer,intent(inout) :: node_ptr
        integer :: i
        node_ptr => self%head
        do i=2,loc
            node_ptr => node_ptr%nxt
        end do
    end subroutine

    subroutine list_t_get_node_allocatable(self, loc, node_allocatable)
        class(list_t),intent(inout) :: self
        integer,intent(in) :: loc
        class(node_t),allocatable,intent(inout) :: node_allocatable
        class(node_t),pointer :: node_ptr
        integer :: i
        node_ptr => self%head
        do i=2,loc
            node_ptr => node_ptr%nxt
        end do
        if(allocated(node_allocatable)) deallocate(node_allocatable)
        allocate(node_allocatable, source = node_ptr)
    end subroutine

    subroutine list_t_set_current_node_to_head(self)
        class(list_t),intent(inout) :: self
        self%current_node => self%head
    end subroutine

    subroutine list_t_set_current_node_to_tail(self)
        class(list_t),intent(inout) :: self
        self%current_node => self%tail
    end subroutine

    subroutine list_t_next_node(self, status)
        class(list_t),intent(inout) :: self
        integer,intent(out),optional :: status
        integer :: i_status
        if(.not.associated(self%current_node))then
          return
        end if
        if(associated(self%current_node%nxt))then
          self%current_node => self%current_node%nxt
          i_status = 0
        else
          self%current_node => null()
          i_status = 1
        end if
        if(present(status))then
            status = i_status
        end if
    end subroutine

    subroutine list_t_before_node(self, status)
        class(list_t),intent(inout) :: self
        integer,intent(out),optional :: status
        integer :: i_status
        if(.not.associated(self%current_node))then
          return
        end if
        if(associated(self%current_node%bef))then
          self%current_node => self%current_node%bef
          i_status = 0
        else
          self%current_node => null()
          i_status = 1
        end if
        if(present(status))then
            status = i_status
        end if
    end subroutine

    subroutine list_t_get_current_node_ptr(self, node_ptr)
        class(list_t),intent(inout) :: self
        class(node_t),pointer,intent(inout) :: node_ptr
        node_ptr => self%current_node
    end subroutine

    subroutine list_t_get_current_node_allocatable(self, node_allocatable)
        class(list_t),intent(inout) :: self
        class(node_t),allocatable,intent(inout) :: node_allocatable
        class(node_t),pointer :: node_ptr
        node_ptr => self%current_node
        if(allocated(node_allocatable)) deallocate(node_allocatable)
        if(associated(node_ptr))then
            allocate(node_allocatable, source = node_ptr)
        end if
    end subroutine

    logical function list_t_is_current_node_null(self)
        class(list_t), intent(in) :: self
        if(associated(self%current_node))then
            list_t_is_current_node_null = .false.
        else
            list_t_is_current_node_null = .true.
        end if
    end function
end module