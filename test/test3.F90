program test3
use iso_c_binding
use fmmap
implicit none

   interface
   
      integer function fork() bind(C)
      end function 

   end interface
   
   integer :: pid = -1
   integer, parameter :: fibo(*) = [1, 2, 3, 5, 8, 13, 21]
   integer(c_size_t) :: n
   type(fmmap_t) :: x, y
   integer, pointer, contiguous :: a(:), b(:)
   
#ifdef _WIN32

   print*, "Test3 cannot be run on Windows, as it uses posix fork()"
   
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
         print*, "FAILED 1"
         error stop
      end if
      print*, "PASSED"
      call c_f_pointer( y%cptr(), b, [n] )
      if (any(b(:) /=  fibo(:))) then
         print*, "FAILED 2"
         error stop
      end if
      print*, "PASSED"
   end if
   
   call x%destroy()
   call y%destroy()
   
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
