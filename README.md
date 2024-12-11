# flinkedlist

## flinkedlist, Fortran Linked List modules

- flinkedlist is a pure Fortran library providing an  simple linked list.
- flinkedlist has two defferent type linked list. You can choose one of them.
  - `m_unlimited_polymorphic_linked_list.f90`
    - this module define the linked list with node `class(*)`.
  - `m_linked_list.f90`
    - this module define the linked list with node `type,abstract :: node_t`.
    - user must define your own extends type of `node_t`.

## Build
This repository is built using [fpm](https://fpm.fortran-lang.org/en/index.html).
You can also use this library by compiling `f90' code with your main program source code, since it is no dependency other than fortran code.

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
Please check these codes.
- Usage of `m_unlimited_polymorphic_linkedlist.f90`
  - `test/test_m_unlimited_polymorphic_linkedlist.f90`
- Usage of `m_linkedlist.f90`
  - `test/test_m_linkedlist.f90`
  - `test/m_linkedlist_node_definition.f90`
