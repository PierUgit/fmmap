program fmmaptest
use iso_c_binding
use fmmap_m, fst => fmmap_size_t
implicit none

type(fmmap_t) :: x, y
double precision, pointer :: pr(:)
integer, pointer :: pi1(:), pi2(:,:), pi3(:,:,:), pi1b(:)
integer :: i, stat, lu
integer(fst) :: n, n1, n2, n3, length
character(len=:), allocatable :: filename

n1 = 1000_fst
n2 = 10_fst ** 7
n3 = 2 * 10_fst ** 9

#ifdef _WIN32
filename = "C:\Temp\fun1.bin"
#else
filename = "./fun1.bin"
#endif


print*
print*, "Testing FMMAP_SCRATCH large:"

length = fmmap_elem2byte( n3, storage_size(pr) )
print*,  length/10**9, " GBytes"
call fmmap_create( x, FMMAP_SCRATCH, "", length ) 
call c_f_pointer( x%cptr(), pr, [n3] )
pr(n3) = 42d0
if (pr(n3) /= 42d0) then
   print*, "FAILED"
   error stop
end if
call fmmap_destroy(x)

print*, "PASSED"



end program
