# fmmap 0.4.0 : memory mapped files in Fortran

## Introduction

These routines provide *some* of the features of the C posix or Windows memory mapped files under a simple and unique Fortran interface.

2 usages:
- allocating arrays that are potentially bigger the RAM+swap size, and which are backed in a temporary file (anonymous mapping)
- opening existing files or creating new files, and mapping them to an array

## Usage

There are 2 different approaches:
- one which is "type agnostic", as it just manipulates bytes and returns a C pointer. The programmer has to manage the conversion between elements and bytes and between the C and a Fortran pointer... Which is not a big deal anyway (the module provides some functions for the conversions).
- another one that hides all the C stuff and returns directly a Fortran pointer, and which is overall simpler. It could be implemented for all the intrinsic types/kinds, but it is just implemented right now for `real`, `double precision`, `complex` `double complex`, `integer`, and `integer(fmmap_other_int)` (the latter being an integer different from the default one; it will virtually always be `integer(int64)`).

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
type(fmmap_t) :: x 


integer(fmmap_size) :: n, nbytes
...
...
n = 1000
nbytes = fmmap_nbytes(n, storage_size(pt)) ! converts 1000 elements to a number of bytes
call fmmap_create(x, nbytes, FMMAP_SCRATCH)    ! anonymous mapping
call c_f_pointer(x%cptr, pt, [n])          ! conversion to a Fortran pointer
...
call fmmap_destroy(x)                      ! close the temporary file, etc...
```

### intrinsic types usage

Example:
```fortran
use ffmap_m   ! note that iso_c_binding is not needed

integer, pointer :: pi(:)
integer(fmmap_size) :: n
...
n = 1000
call fmmap_create(pi, [n], FMMAP_NEW, "./foo1.bin") ! named mapping
...                                            ! returns a 1D pointer of size n
call fmmap_destroy(pi)
```

Other example:
```fortran
use ffmap_m

double precision, pointer :: pr(:,:,:)
integer(fmmap_size) :: n, m
...
n = 1000; m = 100
call fmmap_create(pr, [n,m], FMMAP_OLD, "./foo2.bin") ! named mapping
...                                              ! returns a 3D pointer of size n*m*p
                                                 ! where p is determined by the file size
call fmmap_destroy(pr)
```

## Compilation

The repository has an fpm (Fortran Package Manager) structure. A macro `POSIX` or `WIN32` must be passed depending on the OS:
```
fpm test
```
On Windows, the presence of the `_WIN32` macro is assumed

### Tested on
macOS 10.13      / gcc-gfortran 13  
Windows 10 MSYS2 / gcc-gfortran 13  
Linux Debian 11  / Intel icc-ifort 2021 (without fpm)  
Lubuntu 22.04    / gcc-gfortran 11 (without fpm)

### Issues


## Limitations

- The intrinsic type versions of `ffmap_create()` and `ffmap_destroy()` are not thread-safe. 
- Mapping to an array of a derived type containing allocatable or pointer components is not allowed (well, it's technically possible, but you have to be aware that the memory allocated by these components won't be part of the mapping).
