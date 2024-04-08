# fmmap 0.6.0 : memory mapped files in Fortran

## Introduction

These routines provide *some* of the features of the posix or Windows memory mapped files under a simple and unique Fortran interface.

2 main usages:
- allocating arrays that are potentially bigger the RAM+swap size, and which are backed by a temporary file
- opening existing files or creating new files, and mapping them to an array

Copy-on-Write mapping is possible, with optional write-back of the modifications to the file. Such mapping is useful when one doesn't want to modify the file on disk, or when one wants to work only in memory and decide when to update the file (or not).

## Usage

There are 2 different approaches:
- one which is "type agnostic", as it just manipulates bytes and returns a C pointer. The programmer has to manage the conversion between elements and bytes and between the C and a Fortran pointer... Which is not a big deal anyway (the module provides some functions for the conversions).
- another one that hides all the C stuff and returns directly a Fortran pointer, and which is overall simpler. It could be implemented for all the intrinsic types/kinds, but it is just implemented right now for `real(kind=real32)`, `real(kind=real64)`, `complex(kind=real32)`, `complex(kind=real64)`, `integer(kind=int32)`, `integer(kind=int64)`, and `character(kind=c_char)` (the kind values being defined in the `iso_fortran_env` and `iso_c_binding` intrinsic modules).

The approaches are complementary and both are available in the module.

### "type-agnostic" usage

Example:
```fortran
use iso_C_binding
use ffmap_m

type sometype
   integer :: i
   double precision :: a
   character(len=7) :: str
end type
type(sometype), pointer :: pt(:)
type(c_ptr) :: cptr 


integer(fmmap_size_t) :: n, nbytes
...
...
n = 10_fmmap_size_t ** 9   !! can be larger than RAM+swap space

!> converts n elements to a number of bytes
nbytes = fmmap_nbytes(n, storage_size(pt)) 

!> creates a mapping to a temporary file and returns a C pointer
call fmmap_create(cptr, nbytes, FMMAP_SCRATCH)

!> conversion to a Fortran pointer
call c_f_pointer(cptr, pt, [n])       
     
!> work on pt(:) as if it was a classical array
! ...
! ...

!> closes the mapping and deletes the temporary file
call fmmap_destroy(cptr)                  
```

### intrinsic types usage

Example:
```fortran
use ffmap_m   !! note that iso_c_binding is not needed

integer, pointer :: pi(:)
integer(fmmap_size_t) :: n
...
n = 10_fmmap_size_t ** 9   !! can be larger than RAM+swap space

!> Mapping to a new named file; returns a 1D pointer of size n
call fmmap_create(pi, [n], FMMAP_NEW, "./foo1.bin") 
                    
!> work on pi(:) as if it was a classical array
! ...
! ...

!> closes the mapping and the file (the file is NOT deleted)
call fmmap_destroy(pi)
```

Other example:
```fortran
use ffmap_m

double precision, pointer :: pr(:,:,:)
integer(fmmap_size_t) :: n, m

n = 1000; m = 100

!> Mapping to an existing named file
!> Returns a 3D pointer of size n*m*p, where p is determined by the file size
call fmmap_create(pr, [n,m,-1_fmmap_size_t], FMMAP_OLD, "./foo2.bin") 

!> work on pr(:) as if it was a classical array
! ...
! ...

!> closes the mapping and the file (the file is NOT deleted)
call fmmap_destroy(pr)
```

### Copy-on-Write mapping

```fortran
use ffmap_m

integer, pointer :: pi(:)
integer(fmmap_size_t) :: n
...
n = 10_fmmap_size_t ** 9

!> Mapping to a existing named file; returns a 1D pointer of size n
call fmmap_create(pi, [n], FMMAP_OLD, "./foo1.bin", copyonwrite=.true.) 
                    
!> work on pi(:) as if it was a classical array
!> All the changes are resident only in memory, the file is unmodified 
! ...
! ...

if (...) then
    !> closes the mapping and the file. 
    !> All the changes are lost and the original file is kept
    call fmmap_destroy(pi)
else
    !> alternatively...
    !> All the changes are written back to the file before unmapping and closing
    call fmmap_destroy(pi,writeback=.true.)
end if
```


## Compilation

The repository has an fpm (Fortran Package Manager) structure:
```
fpm test
```
On Windows, the presence of the `_WIN32` macro is assumed

### Tested on
macOS 10.13      / gcc-gfortran 13  
Windows 10 MSYS2 / gcc-gfortran 13  
Linux Debian 11  / Intel icc-ifort 2021 (without fpm)  
Lubuntu 22.04    / gcc-gfortran 11 (without fpm)

### Known issues

Under Windows MSYS2 the `_WIN32` macro is not defined in gfortran (while it is in gcc). I don't know how it is under environments others than MSYS2. The fix is to pass it explicitly: `fpm test --flag "-D_WIN32"`

## Current limitations

- The whole file is mapped, it's not possible to map only a portion of it
- Only one mapping can be active at the same time on a given file
- Mapping to an array of a derived type containing allocatable or pointer components is not allowed (well, it's technically possible, but the memory allocated by these components won't be part of the mapping).
- The `ffmap_create()` and `ffmap_destroy()` routines are not thread-safe. 
