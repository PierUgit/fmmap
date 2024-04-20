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

type sometype
   integer :: i
   double precision :: a
   character(len=7) :: str
end type
type(sometype), pointer :: pt(:)

n1 = 1000_fst
n2 = 10_fst ** 7
n3 = 2 * 10_fst ** 9

#ifdef _WIN32
filename = "C:\Temp\fun1.bin"
#else
filename = "./fun1.bin"
#endif

print*
print*, "Testing FMMAP_SCRATCH:"

length = fmmap_elem2byte( n1, storage_size(pr) )
call fmmap_create( x, FMMAP_SCRATCH, FMMAP_TMPDIR, length ) 
call c_f_pointer( x%cptr(), pr, [n1] )
pr = [(real(i), i=1,n1)]
if (size(pr) /= n1 .or. pr(n1) /= n1) then
   print*, "FAILED"
   error stop
end if
call fmmap_destroy(x)
if (c_associated(x%cptr())) then
   print*, "FAILED"
   error stop
end if

print*, "PASSED"


print*
print*, "Testing FMMAP_SCRATCH large:"

length = fmmap_elem2byte( n3, storage_size(pr) )
call fmmap_create( x, FMMAP_SCRATCH, FMMAP_TMPDIR, length ) 
call c_f_pointer( x%cptr(), pr, [n3] )
pr(n3) = 42d0
if (pr(n3) /= 42d0) then
   print*, "FAILED"
   error stop
end if
call fmmap_destroy(x)

print*, "PASSED"


print*
print*, "Testing FMMAP_NEW"

length = fmmap_elem2byte( n1*n1, storage_size(pi2) )
call fmmap_create( x, FMMAP_NEW, filename, length )
call c_f_pointer( x%cptr(), pi2, [n1,n1] )
pi2(:,:) = 1
pi2(n1,n1) = -1
call fmmap_destroy(x)

print*, "PASSED"


print*
print*, "Testing FMMAP_OLD"

call fmmap_create( x, FMMAP_OLD, filename )
call c_f_pointer( x%cptr(), pi3, [n1,n1/2,2_fst] )
if (pi3(1,1,1) /= 1 .or. pi3(n1,n1/2,2) /= -1) then
   print*, "FAILED"
   error stop
end if
call fmmap_destroy(x)

print*, "PASSED"


print*
print*, "Testing FMMAP_OLD/multiple_maps"

call fmmap_create( x, FMMAP_OLD, filename )
n = fmmap_byte2elem( x%length(), storage_size(pi1b) )
call c_f_pointer( x%cptr(), pi1, [n] )
   print*, "     1st mapping ok"
call fmmap_create( y, FMMAP_OLD, filename )
call c_f_pointer( y%cptr(), pi1b, [n] )
   print*, "     2nd mapping ok"
pi1(10) = -999
if (pi1b(10) /= pi1(10)) then
   print*, "FAILED"
   error stop
end if
call fmmap_destroy(x)
call fmmap_destroy(y)

print*, "PASSED"


print*
print*, "Testing FMMAP_SCRATCH"

length = fmmap_elem2byte( n2, storage_size(pt) )
print*, "     "//"creating scratch mapping of", length," bytes"
call fmmap_create( x, FMMAP_SCRATCH, FMMAP_TMPDIR, length)
call c_f_pointer( x%cptr(), pt, [n2] )
print*, "     "//"filling the array"
call random_number( pt(:)%a ); pt(n2)%a = 0.5
pt(:)%i = [(i, i=1,n2)]
pt(:)%str = "Hello"
if (pt(n2)%i /= n2 .or. trim(pt(n2)%str) /= "Hello" .or. pt(n2)%a /= 0.5) then
   print*, "FAILED"
   error stop
end if
call fmmap_destroy(x)

print*, "PASSED"


print*
print*, "Testing FMMAP_OLD/private/writeback"

call fmmap_create( x, FMMAP_OLD, filename, private=.true. )
n = fmmap_byte2elem( x%length(), storage_size(pi1))
call c_f_pointer( x%cptr(), pi1, [n] )
if (pi1(n) /= -1) then
   print*, "FAILED 1"
   error stop
end if
pi1(n/2) = 42
call fmmap_destroy(x)
call fmmap_create( x, FMMAP_OLD, filename, private=.true.)
call c_f_pointer( x%cptr(), pi1, [n] )
if (pi1(n/2) /= 1) then
   print*, "FAILED 2", pi1(n/2)
   error stop
end if
pi1(n/2) = 42
call fmmap_destroy( x, writeback=.true. )
call fmmap_create( x, FMMAP_OLD, filename)
call c_f_pointer( x%cptr(), pi1, [n] )
if (pi1(n/2) /= 42) then
   print*, "FAILED 3"
   error stop
end if
call fmmap_destroy( x )

print*, "PASSED"


print*
print*, "Testing FMMAP_NOFILE"

length = fmmap_elem2byte( n2, storage_size(pr) )
call fmmap_create( x, FMMAP_NOFILE, "", length )
!call c_f_pointer( x%cptr(), pr, [n2] )
!pr(:) = 42d0
!print*, "CCC"
call fmmap_destroy( x )

print*, "PASSED"


print*
open(newunit=lu, file=filename, status="OLD")
close(lu,status="DELETE")

end program
