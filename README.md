# flinkedlist

## flinkedlist, Fortran Linked List 

- flinkedlist is a pure Fortran library providing an  simple linked list.
- flinkedlist is OOP designed.
- flinkedlist has some useful features like below.
  - sort elements(which is called `node` in this library) by user defined function
  - apply user defined function in each `node`.
  - Includes a convenience method to aid printf debugging by automatically displaying variables of built-in types and providing a dedicated display function for user-defined types.
  - By using the `node_operator_type` type to manipulate list elements, users can use the list data type without worrying about the internal details of the type.
  - List elements can be converted to arrays of type `node_operator_array`, so you can choose an array-like access method for list elements.

## Build
This repository is built using [fpm](https://fpm.fortran-lang.org/en/index.html).
You can also use this library by compiling `flinkedlist.f90' with your main program source code, since it is no dependency other than fortran code.

If you use fpm, do the following.
```bash
pip install fpm
git clone https://github.com/sakamoti/flinkedlist.git
cd flinkedlist

# build and run test program
fpm test
```

To use `flinkedlist` within your fpm project, add the following to your `fpm.toml` file:
```bash
[dependencies]
flinkedlist = {git = "https://github.com/sakamoti/flinkedlist.git"}
```

## DOCUMENT (API)

If you don't have documentation tool, you should first install
[`ford`](https://forddocs.readthedocs.io/en/latest/index.html).
```bash
pip install ford
```
Then, auto generated documentation is available.
```bash
ford flinkedlist-doc-ford-settings.md
```

## USAGE
Please check `test/check.f90` or document generated by `ford`.

About `list_type`.
- `list_type` is a double linked list that can hold any data type.
- By declaring `type(list_type) :: list`, you can register data into the list one after another, such as `list%append("AnyType")`.
- `list%append` inserts data in the first element (it is called `head` in the source) of the list if the position of data insertion is not specified.
- The `node_operator_type` type is used for user access to the elements of the list.

About `node_operator_type`.
- The `node_operator_type` type is responsible for moving through the list, retrieving and displaying data.
- When acquiring data, there is a method `get_alloc` to allocate memory separate from the list elements, and a method ` get_ptr` to get a pointer that points directly inside the list.

## Examples

### Append and retrieve data
```fortran
program append_and_retrieve
    use flinkedlist
    use iso_fortran_env
    implicit none
    
    type user_type
      integer :: i = 1
      real(kind=real32) :: x =1.6
    end type
    
    type(user_type) :: ud
    type(list_type) :: list
    type(node_operator_type) node
    class(*), allocatable :: val
    class(*), pointer :: val_ptr
    integer :: i
    
    !... append data into the list ...
    call list%append(1_int32) !append node(int32)
    call list%append(32.0_real32)    !append node(real64)
    call list%append(64.0_real64)    !append node(real64)
    call list%append(128.0_real128)    !append node(real128)
    call list%append((3.0_real32,2.0_real32))    !append node(complex32)
    call list%append((6.0_real64,4.0_real64))    !append node(complex64)
    call list%append(.TRUE.) !append node(logical)
    call list%append("a")    !append node(character)
    call list%append("Hello Fortran Linked list!")    !append node(strings)
    call list%append(ud) !append node(user defined type)
    !... show list data
    print *, "!-- show all (intrinsinc data type only)"
    call list%showall()  !show all nodes
    print *, "!-- show all (add user defined type showing routine)"
    call list%showall(showproc=user_show_proc) !show all nodes

    !... retrieve data from the list
    call node%init(list)
    call node%head()
    do i=1,4
      call node%next()
    end do
    call node%get_alloc(val) !copy list node to new memory
    call node%get_ptr(val_ptr) !pointer which points inside the list

    !... showing data from a list
    call obj_show(val)
    call obj_show(val_ptr)
    contains
      subroutine user_show_proc(obj, passdata, fid)
        class(*), intent(in) :: obj
        class(*), intent(in),optional :: passdata
        integer, intent(in), optional :: fid
        select type(obj)
        type is (user_type)
          print *, "user_typee:", obj
        end select
      end subroutine
end program
```

### Apply procedure to each data
```fortran
program applyprocedure
  use iso_fortran_env
  use flinkedlist
  implicit none
  integer,parameter :: n = 1000
  type(list_type) :: list
  type(node_operator_type) :: node
  integer :: i

  type my_data
    real(real64) :: x(n,2)
  end type
  type(my_data) :: xarray


  do i=1,5
    call random_number(xarray%x)
    call list%append(xarray)
  end do

  print *, "!--- original list ---"
  call list%apply(array_double,parallel=.FALSE.)

  contains
    subroutine array_double(obj, passdata)
      class(*), intent(inout) :: obj
      class(*), intent(in), optional :: passdata
      real(real64),allocatable,target :: dot
      select type(obj)
      type is (my_data)
        obj%x = obj%x * 2d0
      end select
    end subroutine
end program
```

### Deep copy of list_type

```fortran
 program deepcopy
  use flinkedlist
  implicit none
  type(list_type) :: list_origin, list_copied
  type(node_operator_type) :: node
  integer :: i
  do i=1,5
    call list_origin%append(i)
  end do

  print *, "!--- original list ---"
  call list_origin%showall()

  list_copied = list_origin ! deep copy

  print *, "!--- copied list ---"
  call list_copied%showall()
end program
```

### Node operation

### Sort data

### Transform (list -> array)
