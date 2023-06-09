src_dir: ./src
output_dir: ./doc-flinkedlist
project: flinkedlist
summary: Doubly linked list
author: Yuichiro Sakamoto
graph: true
github: https://github.com/sakamoti/flinkedlist


# About *finkedlist* module

 Doubly linked list using unlimited polymorphic object pointer.
 Doubly linked list which can hold any predefined data type or user defined type.

## Public type

   name of type | commentary
   -------------|-------------------------
   list_type    | Object holding doubly linked list structure and data.
   node_operator_type    | Objects for external manipulation of list elements.

## Public interface (these function may be defined and passed by user)

   interface      | commentary
   ----------------|-------------------------
   list_sort_func  | sort linked list element
   list_apply_proc | this subroutine is apply all elements
   obj_show_proc   | print routine. this is useful to show user defined type.
 .

# Coding Policy

 Uses the Object Oriented Programing model incorporated in fortran 2003.
 Encapsulation is taken into consideration, so that the list structure cannot be
 directly modified from the outside. Therefore, list operations should be
 performed throuth public methods.

# Example Usage

```fortran
program test
  use flinkedlist
  implicit none
  type(list_type) :: samplelist
  type(node_operator_type) :: element_ptr

  !... process begin ...
  call samplelist%append(2d0)    !append element(real64) in the *samplelist*
  call samplelist%append(.TRUE.) !append element(logical) in the *samplelist*
  call samplelist%showall()      !show all elements in the *samplelist*
  !... process end ...
end program
```
