module mynode
    use iso_fortran_env
    use m_linkedlist
    implicit none
    public
    type,abstract,extends(node_t) :: shape_t
        contains
        procedure(area_interface),pass,deferred,public :: area
    end type
    abstract interface
        function area_interface(self) result(area)
          import shape_t, real64
          class(shape_t),intent(in) :: self
          real(real64) :: area
        end function
    end interface

    type,extends(shape_t) :: circle_node_t
        real(real64) :: r
        contains
        procedure,public :: area => area_circle
    end type
    type,extends(shape_t) :: rectangle_node_t
        real(real64) :: width
        real(real64) :: height
        contains
        procedure,public :: area => area_rect
    end type

    type,extends(node_t) :: r64node_t
        real(real64) :: x
        contains
        procedure,private :: r64node_write
        generic :: write(formatted) => r64node_write 
    end type
    type,extends(node_t) :: i32node_t
        integer(int32) :: idx
    end type

    contains
      function area_circle(self) result(area)
        class(circle_node_t),intent(in) :: self
        real(real64) :: area
        area = self%r * self%r * 3.14d0
      end function
      function area_rect(self) result(area)
        class(rectangle_node_t),intent(in) :: self
        real(real64) :: area
        area = self%width * self%height
      end function

      ! factories
      function r64node_factory(x) result(node)
        real(real64),intent(in) :: x
        class(r64node_t), allocatable :: node 
        allocate(r64node_t::node)
        node%x = x
      end function
      subroutine r64node_write(self,unit,iotype,vlist,iostat,iomsg)
        class(r64node_t),intent(in) :: self
        integer,intent(in) :: unit
        character(*),intent(in) :: iotype
        integer,intent(in) :: vlist(:)
        integer,intent(out) :: iostat
        character(*),intent(inout) :: iomsg
        print*, self%x
      end subroutine
    
      function i32node_factory(idx) result(node)
        integer(int32),intent(in) :: idx
        class(i32node_t), allocatable :: node 
        allocate(i32node_t::node)
        node%idx = idx
      end function
      function circle_node_factory(r) result(node)
        real(real64),intent(in) :: r
        class(circle_node_t), allocatable :: node 
        allocate(circle_node_t::node)
        node%r = r
      end function
      function rectangle_node_factory(w, h) result(node)
        real(real64),intent(in) :: w, h
        class(rectangle_node_t), allocatable :: node 
        allocate(rectangle_node_t::node)
        node%width = w
        node%height = h
      end function
end module