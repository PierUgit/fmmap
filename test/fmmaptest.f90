program fmmaptest
use iso_c_binding
use fmmap_m
implicit none

type(c_ptr) :: cptr
double precision, pointer :: pr(:)
integer, pointer :: pi2(:,:), pi3(:,:,:)
integer :: i, stat
integer(fmmap_size_t) :: n, nbytes

type sometype
   integer :: i
   double precision :: a
   character(len=7) :: str
end type
type(sometype), pointer :: pt(:)

n = 1000_fmmap_size_t

print*, "Testing FMMAP_SCRATCH:"

call fmmap_create(pr,[n],FMMAP_SCRATCH)
pr = [(real(i), i=1,n)]
if (size(pr) /= n .or. pr(n) /= n) then
   print*, "FAILED"
   error stop
end if
call fmmap_destroy(pr)
if (associated(pr)) then
   print*, "FAILED"
   error stop
end if

print*, "PASSED"

print*, "Testing FMMAP_NEW"

call fmmap_create(pi2,[n,n],FMMAP_NEW,"./fun1.bin")
pi2(:,:) = 1
pi2(n,n) = -1
call fmmap_destroy(pi2)
if (associated(pi2)) then
   print*, "FAILED"
   error stop
end if

print*, "PASSED"

print*, "Testing FMMAP_OLD"

call fmmap_create(pi3,[n,n/2],FMMAP_OLD,"./fun1.bin")
if (any(shape(pi3) /= [n,n/2,2_fmmap_size_t]) .or. pi3(1,1,1) /= 1 .or. pi3(n,n/2,2) /= -1) then
   print*, "FAILED"
   error stop
end if
call fmmap_destroy(pi3)
if (associated(pi3)) then
   print*, "FAILED"
   error stop
end if

print*, "PASSED"

print*, "Testing FMMAP_SCRATCH"

n = 10_fmmap_size_t ** 8
nbytes = fmmap_nbytes(n,storage_size(pt))
print*, "     "//"creating scratch mapping of", nbytes," bytes"
call fmmap_create(cptr,nbytes,FMMAP_SCRATCH,"./")
call c_f_pointer(cptr, pt, [n])
print*, "     "//"filling the array"
call random_number( pt(:)%a ); pt(n)%a = 0.5
pt(:)%i = [(i, i=1,n)]
pt(:)%str = "Hello"
if (pt(n)%i /= n .or. trim(pt(n)%str) /= "Hello" .or. pt(n)%a /= 0.5) then
   print*, "FAILED"
   error stop
end if
call fmmap_destroy(cptr)
if (c_associated(cptr)) then
   print*, "FAILED"
   error stop
end if

print*, "PASSED"

end program
