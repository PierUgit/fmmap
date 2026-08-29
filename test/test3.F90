program test3
use iso_c_binding
use fmmap
implicit none

   interface
   
      integer function fork() bind(C)
      end function 

   end interface
   
   integer :: pid = -1, lu
   integer, parameter :: fibo(*) = [1, 2, 3, 5, 8, 13, 21]
   integer(c_size_t) :: n
   type(fmmap_t) :: x, y
   integer, pointer, contiguous :: a(:), b(:)
   
print*
print*, "TEST 3 - Inter Process Communication"
#ifdef _WIN32

   print*, "TEST 3 cannot be run on Windows, as it uses posix fork()"
   
#else

   n = size(fibo)
   
   call y%create( FMMAP_NOFILE, "", n, mold=0, private=.false. )
      
   pid = fork()
   if (pid == 0) then
      n = size(fibo)
      call x%create( FMMAP_NEW, "mappedfile", n, mold=0 )
      call c_f_pointer( x%cptr(), a, [n] )      
      a(:) = fibo(:)
      call c_f_pointer( y%cptr(), b, [n] )
      b(:) = fibo(:)
      call sleep(2*10**6) ; 
   else
      call sleep(10**6) ; 
      call x%create( FMMAP_OLD, "mappedfile", n, mold=0 )
      call c_f_pointer( x%cptr(), a, [n] )
      if (any(a(:) /=  fibo(:))) then
         error stop "FAILED 1"
      end if
      call c_f_pointer( y%cptr(), b, [n] )
      if (any(b(:) /=  fibo(:))) then
         error stop "FAILED 2"
      end if
   end if
   
   call x%destroy()
   call y%destroy()
   
   if (pid == 0) then
      print*, "TEST 3 PASSED (parent)"
      open(newunit=lu, file="mappedfile", status="OLD")
      close(lu,status="DELETE")   
   else
      print*, "TEST 3 PASSED (child)"
   end if
   
#endif
   
contains

   subroutine sleep(n)
      integer, intent(in) :: n
      integer :: i
      real :: r
      do i = 1, n  ! "sleep loop"
         call random_number(r)
      end do
   end subroutine
   
end
