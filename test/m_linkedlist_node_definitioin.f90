module mynode
    use iso_fortran_env
    use m_linkedlist
    implicit none
    public

    ! define an abstract type to enable the use of print structures easily
    type,abstract,extends(node_t) :: i_node_with_io_t 
      contains
      procedure(interface_write_formatted),deferred,private :: write_formatted
      generic :: write(formatted) => write_formatted
    end type

    ! define custom structure (this is abstract type)
    type,abstract,extends(i_node_with_io_t) :: i_shape_t
        contains
        procedure(area_interface),pass,deferred,public :: area
    end type

    ! define interfaces for abstract types
    abstract interface
        subroutine interface_write_formatted(self, unit, iotype, v_list, iostat, iomsg)
          import i_node_with_io_t
          class(i_node_with_io_t), intent(in) :: self
          integer, intent(in) :: unit
          character(*), intent(in) :: iotype
          integer, intent(in) :: v_list(:)
          integer, intent(out) :: iostat
          character(*), intent(inout) :: iomsg
        end subroutine
        function area_interface(self) result(area)
          import i_shape_t, real64
          class(i_shape_t),intent(in) :: self
          real(real64) :: area
        end function
    end interface

    type,extends(i_shape_t) :: circle_node_t
        real(real64) :: r
        contains
        procedure,public :: area => area_circle
        procedure,private :: write_formatted => circle_node_t_write_formatted
    end type
    type,extends(i_shape_t) :: rectangle_node_t
        real(real64) :: width
        real(real64) :: height
        contains
        procedure,public :: area => area_rect
        procedure,private :: write_formatted => rectangle_node_t_write_formatted
    end type

    type,extends(i_node_with_io_t) :: r64node_t
        real(real64) :: x
        contains
        procedure,private :: write_formatted => r64node_write
    end type
    type,extends(i_node_with_io_t) :: i32node_t
        integer(int32) :: idx
        contains
        procedure,private :: write_formatted =>i32node_write
    end type

    contains
      ! circle_node_t
      function area_circle(self) result(area)
        class(circle_node_t),intent(in) :: self
        real(real64) :: area
        area = self%r * self%r * 3.14d0
      end function
      subroutine circle_node_t_write_formatted(self, unit, iotype, v_list, iostat, iomsg)
        class(circle_node_t), intent(in) :: self
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        write(unit, fmt='(a,f10.3,a,f10.3)',iostat=iostat, iomsg=iomsg) &
          "circle_node_t   : r    =",self%r, ", area  =", self%area()
      end subroutine

      ! rectangle_node_t
      function area_rect(self) result(area)
        class(rectangle_node_t),intent(in) :: self
        real(real64) :: area
        area = self%width * self%height
      end function
      subroutine rectangle_node_t_write_formatted(self, unit, iotype, v_list, iostat, iomsg)
        class(rectangle_node_t), intent(in) :: self
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        write(unit, fmt='(a,f10.3,a,f10.3,a,f10.3)',iostat=iostat, iomsg=iomsg) &
          "rectangle_node_t: width=",self%width, ", height=", self%height, ", area=", self%area()
      end subroutine

      ! r64node_t
      subroutine r64node_write(self,unit,iotype,v_list,iostat,iomsg)
        class(r64node_t),intent(in) :: self
        integer,intent(in) :: unit
        character(*),intent(in) :: iotype
        integer,intent(in) :: v_list(:)
        integer,intent(out) :: iostat
        character(*),intent(inout) :: iomsg
        write(unit, fmt='(a,f10.3)',iostat=iostat, iomsg=iomsg) &
          "r64node_t       : x    =",self%x
      end subroutine

      ! i32node_t
      subroutine i32node_write(self,unit,iotype,v_list,iostat,iomsg)
        class(i32node_t),intent(in) :: self
        integer,intent(in) :: unit
        character(*),intent(in) :: iotype
        integer,intent(in) :: v_list(:)
        integer,intent(out) :: iostat
        character(*),intent(inout) :: iomsg
        write(unit, fmt='(a,i10)',iostat=iostat, iomsg=iomsg) &
          "r32node_t       : idx  =",self%idx
      end subroutine

      ! factories
      function r64node_factory(x) result(node)
        real(real64),intent(in) :: x
        class(r64node_t), allocatable :: node 
        allocate(r64node_t::node)
        node%x = x
      end function
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