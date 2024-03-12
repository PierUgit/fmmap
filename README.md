# fmmap 0.2.0 : memory mapped files in Fortran

## Introduction

These routines provide *some* of the features of the C posix or Windows memory mapped files under a simple and unique Fortran interface.

2 usages:
- allocating arrays that are potentially bigger the RAM+swap size, and which are backed in a temporary file (anonymous mapping)
- opening existing files or creating new files, and mapping them to an array

## Usage

There are 2 different approaches:
- one which is "type agnostic", as it just manipulates bytes and returns a C pointer. The programmer has to manage the conversion between elements and bytes and between the C and a Fortran pointer... Which is not a big deal anyway
- another one that hides all the C stuff and returns directly a Fortran pointer. Overall simpler. It could be implemented for all the intrinsic types/kinds, but for demonstration it is just implemented right now for `real`, `double precision`, `complex` `complex(kind(0d0)`, `integer`, and `integer(int64)`.

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
fpm test --flag '-DPOSIX`
# or
fpm test --flag '-DWIN32'
```

### Tested on
macOS 10.13 / gcc-gfortran 13
Windows 10 MSYS2 / gcc-gfortan 13

### Issues
On macOS 10.13, for some reason fpm (0.10) does not pass the `-DPOSIX` macro. The compilation has to be performed manually:
`gcc -c -DPOSIX src/fmmap_c.c && gfortran -DPOSIX -Isrc src/fmmap_m.F90 test/fmmaptest.f90 fmmap_c.o && ./a.out`


## Limitations

- The intrinsic type versions of `ffmap_create()` and `ffmap_destroy()` are not thread-safe. 
- Mapping to an array of a derived type containing allocatable or pointer components is not allowed. 
