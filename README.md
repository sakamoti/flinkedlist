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

## Build and Install
```bash
pip install fpm
git clone https://github.com/sakamoti/flinkedlist.git
cd flinkedlist

# build and run test program
fpm test
```
## USAGE
No details yet.

Please read `test/check.f90`.