# Memory Mapped Files in Fortran - Examples

Back to [README](../README.md)

These are not working examples that can be compiled, rather pieces of code for illustration.

### Example 1 - mapping a scratch file to an array of a user derived-type

```fortran
use iso_C_binding, cst => c_size_t
use fmmap

type sometype
   integer :: i
   double precision :: a
   character(len=8) :: str
end type
type(sometype), pointer :: pt(:)
type(sometype) :: t
type(fmmap_t) :: x

integer(cst) :: n = 10_cst**10   !< may make pt(:) larger than RAM+swap space

!> creates a mapping to a temporary file 
call x%create(FMMAP_SCRATCH, "/SCRe", n, mold=t)

!> conversion to a Fortran pointer
call c_f_pointer(x%cptr(), pt, [n])       
     
!> work on pt(:) as if it was a classical array
!! ..

!> closes the mapping and deletes the file
call x%destroy()                 

end
```
Execution of `./a.out`:

### basic example 2 - mapping a newly created file to a 2D array

```fortran
use iso_C_binding, cst => c_size_t
use fmmap

integer, pointer, contiguous :: pi(:,:), tmpi(:,:)
type(fmmap_t) :: x
integer(cst) :: n
...
n = 1000_cst   !! can be larger than RAM+swap space

!> Mapping to a new named file for n*n integer elements
call x%create( FMMAP_NEW, "./foo1.bin", n*n, mold=0 ) 

!> conversion to a 2D Fortran pointer, in 2 stages because we want a lower bound /= 1
call c_f_pointer(x%cptr(), tmpi, [n,n])      
pi(0:n-1,1:n) => tmpi
                    
!> work on pi(:,:) as if it was a classical array
! ...

!> closes the mapping (the file is NOT deleted)
call x%destroy()
```

### Example 3 - Private mapping

```fortran
use iso_C_binding, cst => c_size_t
use fmmap

integer, pointer :: pi(:)
type(fmmap_t) :: x
integer(cst) :: n
...

!> Mapping an existing named file
call x%create( FMMAP_OLD, "./foo1.bin", n, mold=0, private=.true.) 
!> Conversion to a Fortran pointer
call c_f_pointer( x%cptr(), pi, [n] )      
                    
!> work on pi(:) as if it was a classical array
!> All the changes reside only in memory, the file is unmodified 
! ...

if (...) then
    !> Closes the mapping; all the changes are lost and the original file is kept
    call x%destroy()
else
    !> Alternatively, the changes are written back to the file before unmapping
    call x%destroy( writeback=.true. )
end if
```

### Example 4 - Basic Inter-Process Communication

The two process map the same physical file. Because a physical file is involved there
is possibly some performance hit. But as long as the size of the mapping is small or 
moderate, the file can entirely live in the RAM cache, thus without significant 
apparent performance penalty.

Both process A and B are run here from the same code, but it could be 2 different codes:
```fortran
use iso_C_binding, cst => c_size_t
use fmmap

integer, pointer :: pi(:)
type(fmmap_t) :: x
integer(cst) :: n
...

!> Mapping an existing named file
call x%create( FMMAP_OLD, "./foo1.bin", n, mold=0 ) 
!> Conversion to a Fortran pointer
call c_f_pointer( x%cptr(), pi, [n] )      
                    
!> work on pi(:) as if it was a classical array
!> All the changes made here by process A are "instantly" visible by process B 
!> and vice-versa
! ...
```

### Example 5 - Basic Inter-Process communication

This one is possible only if a child process is forked from a parent process (hence not
on Windows, as `fork()` doesn't exist). No backing file is involved, everything happens
in memory only. The size of the mapping cannot exceed the available RAM+swap size.

```fortran
use iso_C_binding, cst => c_size_t
use fmmap

interface
    integer function fork() bind(C)
    end function
end interface

integer, pointer :: p(:), q(:)
type(fmmap_t) :: x
integer(cst) :: n
integer :: pid
...

!> Anonymous Mapping, NOT private (important)
call x%create( FMMAP_NOFILE, "", n, mold=0, private=.false. )

pid = fork()
if (pid == 0) then
    ! parent process 
    !> Conversion to a Fortran pointer
    call c_f_pointer( x%cptr(), p, [n] )      
                    
    !> work on p(:) as if it was a classical array
    !> All the changes made here are "instantly" visible by the child process in q(:)
    ! ...
else
    ! child process
    !> Conversion to a Fortran pointer
    call c_f_pointer( x%cptr(), q, [n] )      
                    
    !> work on q(:) as if it was a classical array
    !> All the changes made here are "instantly" visible by the parent process in p(:)
    ! ...
end if

call x%destroy()
