program fmmaptest
use iso_c_binding
use fmmap_m
implicit none

type(c_ptr) :: cptr
double precision, pointer :: pr(:)
integer, pointer :: pi1(:), pi2(:,:), pi3(:,:,:)
integer :: i, stat, lu
integer(fmmap_size_t) :: n, n1, n2, n3, nbytes

type sometype
   integer :: i
   double precision :: a
   character(len=7) :: str
end type
type(sometype), pointer :: pt(:)

n1 = 1000_fmmap_size_t
n2 = 10_fmmap_size_t ** 7
n3 = 2 * 10_fmmap_size_t ** 9


print*, "Testing dp/rank1/FMMAP_SCRATCH:"

call fmmap_create(pr,[n1],FMMAP_SCRATCH)
pr = [(real(i), i=1,n1)]
if (size(pr) /= n1 .or. pr(n1) /= n1) then
   print*, "FAILED"
   error stop
end if
call fmmap_destroy(pr)
if (associated(pr)) then
   print*, "FAILED"
   error stop
end if

print*, "PASSED"


print*, "Testing dp/rank1/FMMAP_SCRATCH large:"

call fmmap_create(pr,[n3],FMMAP_SCRATCH)
pr(n3) = 42d0
if (size(pr) /= n3 .or. pr(n3) /= 42d0) then
   print*, "FAILED"
   error stop
end if
call fmmap_destroy(pr)

print*, "PASSED"


print*, "Testing int/rank2/FMMAP_NEW"

call fmmap_create(pi2,[n1,n1],FMMAP_NEW,"./fun1.bin")
pi2(:,:) = 1
pi2(n1,n1) = -1
call fmmap_destroy(pi2)
if (associated(pi2)) then
   print*, "FAILED"
   error stop
end if

print*, "PASSED"


print*, "Testing int/rank3/FMMAP_OLD"

call fmmap_create(pi3,[n1,n1/2],FMMAP_OLD,"./fun1.bin")
if (any(shape(pi3) /= [n1,n1/2,2_fmmap_size_t]) .or. pi3(1,1,1) /= 1 .or. pi3(n1,n1/2,2) /= -1) then
   print*, "FAILED"
   error stop
end if
call fmmap_destroy(pi3)
if (associated(pi3)) then
   print*, "FAILED"
   error stop
end if

print*, "PASSED"


print*, "Testing cptr/rank1/FMMAP_SCRATCH"

nbytes = fmmap_nbytes(n2,storage_size(pt))
print*, "     "//"creating scratch mapping of", nbytes," bytes"
call fmmap_create(cptr,nbytes,FMMAP_SCRATCH,"./")
call c_f_pointer(cptr, pt, [n2])
print*, "     "//"filling the array"
call random_number( pt(:)%a ); pt(n2)%a = 0.5
pt(:)%i = [(i, i=1,n2)]
pt(:)%str = "Hello"
if (pt(n2)%i /= n2 .or. trim(pt(n2)%str) /= "Hello" .or. pt(n2)%a /= 0.5) then
   print*, "FAILED"
   error stop
end if
call fmmap_destroy(cptr)
if (c_associated(cptr)) then
   print*, "FAILED"
   error stop
end if

print*, "PASSED"


print*, "Testing cptr/rank1/FMMAP_OLD/private/writeback"

call fmmap_create(cptr,nbytes,FMMAP_OLD,"./fun1.bin",private=.true.)
n = fmmap_nelems(nbytes,storage_size(pi1))
call c_f_pointer(cptr,pi1,[n])
if (pi1(n) /= -1) then
   print*, "FAILED"
   error stop
end if
pi1(n/2) = 42
call fmmap_destroy(cptr)
call fmmap_create(cptr,nbytes,FMMAP_OLD,"./fun1.bin",private=.true.)
call c_f_pointer(cptr,pi1,[n])
if (pi1(n/2) /= 1) then
   print*, "FAILED"
   error stop
end if
pi1(n/2) = 42
call fmmap_destroy(cptr,writeback=.true.)
call fmmap_create(cptr,nbytes,FMMAP_OLD,"./fun1.bin")
call c_f_pointer(cptr,pi1,[n])
if (pi1(n/2) /= 42) then
   print*, "FAILED"
   error stop
end if
call fmmap_destroy(cptr)

print*, "PASSED"


open(newunit=lu, file="./fun1.bin", status="OLD")
close(lu,status="DELETE")

end program
