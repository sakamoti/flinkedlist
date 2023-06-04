program check
  use flinkedlist
  use iso_fortran_env
  implicit none
  type user_type
    integer :: i = 1
    real(kind=real32) :: x =1.6
  end type
  type(list_type) :: list
  type(node_operator_type) :: node
  type(user_type) :: ud
  !... process begin ...
  call list%append(1_int32) !append element(int32) in the *samplelist*
  call list%append(32.0_real32)    !append element(real64) in the *samplelist*
  call list%append(64.0_real64)    !append element(real64) in the *samplelist*
  call list%append(128.0_real128)    !append element(real128) in the *samplelist*
  call list%append((3.0_real32,2.0_real32))    !append element(complex32) in the *samplelist*
  call list%append((6.0_real64,4.0_real64))    !append element(complex64) in the *samplelist*
  call list%append(.TRUE.) !append element(logical) in the *samplelist*
  call list%append("a")    !append element(character) in the *samplelist*
  call list%append("Hello Fortran Linked list!")    !append element(strings) in the *samplelist*
  call list%append(ud) !append element(user defined type) in the *samplelist*
  print *, "!-- show all (intrinsinc data type only)"
  call list%showall()  !show all elements in the *samplelist*
  print *, "!-- show all (add user defined type showing routine)"
  call list%showall(showproc=user_show_proc)      !show all elements in the *samplelist*
  !... process end ...
  contains
    subroutine user_show_proc(obj,passdata,fid)
      class(*),intent(in) :: obj
      class(*),intent(in),optional :: passdata
      integer,intent(in),optional :: fid
      select type(obj)
      type is(user_type)
        print *,"user_type:", obj
      end select
    end subroutine
end program check
