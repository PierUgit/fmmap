program fmmaptest
use iso_c_binding, cst => c_size_t
use fmmap
implicit none

type(fmmap_t) :: x
double precision, pointer :: pr(:)
integer :: i, stat, lu
integer(cst) :: n, n3, length
character(len=:), allocatable :: filename
character(len=128) :: str, dir

print*, "Testing now very large allocations..."

length = 16
print*, "How much? (in GiB) (default=16)"
read(*,"(A)") str
if (str /= "") read(str,*) length
length = length * 2**30

print*, "Where? (default is . (posix) or C:\Temp (Windows))"
read(*,"(A)") dir

if (dir == "") then
#ifdef _WIN32
   dir = "C:\Temp"
#else
   dir = "."
#endif
end if

#ifdef _WIN32
   filename = trim(dir)//"\fun1.bin"
#else
   filename = trim(dir)//"/fun1.bin"
#endif

print*
print*, "Testing FMMAP_SCRATCH large:"

n3 = fmmap_byte2elem( length,  storage_size(pr) )
print*,  length/2**30, " GiBytes"
call fmmap_create( x, FMMAP_SCRATCH, dir, length ) 
call c_f_pointer( x%cptr(), pr, [n3] )
pr(:) = 42d0
if (pr(n3/2) /= 42d0) then
   print*, "FAILED"
   error stop
end if
call fmmap_destroy(x)

print*, "PASSED"



end program
