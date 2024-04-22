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
print*, "Testing FMMAP_NOFILE"

length = fmmap_elem2byte( n2, storage_size(pr) )
call fmmap_create( x, FMMAP_NOFILE, "", length )
call c_f_pointer( x%cptr(), pr, [n2] )
pr(:) = 42d0
allocate( pi1(n2), source = 0 )
if (any(pr /= 42d0)) then
   print*, "FAILED"
end if
deallocate( pi1 )
call fmmap_destroy( x )

print*, "PASSED"


end program
