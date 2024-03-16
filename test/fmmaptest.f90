program fmmaptest
use iso_c_binding
use fmmap_m
implicit none

type(fmmap_t) :: x
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
print*, "     "//"1000   1000.0 should be printed:"

call fmmap_create(pr,[n],FMMAP_SCRATCH)
pr = [(real(i), i=1,n)]
print*, "     ", size(pr), pr(n)
call fmmap_destroy(pr)

print*, "Testing FMMAP_NEW"

call fmmap_create(pi2,[n,n],FMMAP_NEW,"./fun1.bin")
pi2(:,:) = 1
pi2(n,n) = -1
call fmmap_destroy(pi2)

print*, "Testing FMMAP_OLD"
print*, "     "//"1000   500   2   1   -1   should be printed:"

call fmmap_create(pi3,[n,n/2],FMMAP_OLD,"./fun1.bin")
print*, "     ", shape(pi3), pi3(1,1,1), pi3(n,n/2,2)
call fmmap_destroy(pi3)

print*, "Testing FMMAP_SCRATCH"

n = 10_fmmap_size_t ** 8
nbytes = fmmap_nbytes(n,storage_size(pt))
print*, "     "//"creating scratch mapping of", nbytes," bytes"
call fmmap_create(x,nbytes,FMMAP_SCRATCH,"./")
call c_f_pointer(x%cptr, pt, [n])
print*, "     "//"filling the array"
call random_number( pt(:)%a )
pt(:)%i = [(i, i=1,n)]
pt(:)%str = "Hello"
print*, "     "//"100000000 <random number> Hello   should be printed:"
print*, "     ", pt(n)
call fmmap_destroy(x)

end program
