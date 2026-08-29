program test2
use iso_c_binding, cst => c_size_t
use fmmap
implicit none

type(fmmap_t) :: x
double precision, pointer :: pr(:)
integer :: i, stat, lu
integer(cst) :: n, n3, length
character(len=:), allocatable :: filename
character(len=128) :: str, dir

print*
print*, "TEST 2 - very large allocations..."

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

print*, "   Testing FMMAP_SCRATCH large"

! testing the deprecated approach, as it still has to work
n3 = length / fmmap_sizeof( pr )
print*,  length/2**30, " GiBytes"
call x% create( FMMAP_SCRATCH, dir, length )
call c_f_pointer( x%cptr(), pr, [n3] )
pr(:) = 42d0
if (pr(n3/2) /= 42d0) then
   error stop "FAILED"
end if
call x% destroy()

print*, "TEST 2 PASSED"



end program
