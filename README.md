# fmmap 0.9.0 : memory mapped files in Fortran

See also the ["detailed" documentation](doc/index.md)

## Introduction

These routines provide *some* of the features of the posix or Windows memory mapped files under a simple and unique Fortran interface.

2 main usages:
- allocating arrays that are potentially bigger than the RAM+swap size, and which are backed by a temporary file
- opening existing files or creating new files (still potentially bigger than the RAM+swap size), and mapping them to an array

Private mapping is possible, with optional write-back of the modifications to the file. Such mapping can be useful when one doesn't want to modify the file on disk, or when one wants to work only in memory and decide when to update (or not) the file. 

## Usage

The interface manipulates bytes and returns a C pointer. The programmer has to manage the conversion between elements and bytes (the module provides some functions for the conversions), and between the C and a Fortran pointer. 


### Example 1

```fortran
use iso_C_binding
use ffmap_m, fst => fmmap_size_t

type sometype
   integer :: i
   double precision :: a
   character(len=7) :: str
end type
type(sometype), pointer :: pt(:)
type(fmmap_t) :: x

integer(fst) :: n, nbytes
...
...
n = 10_fst ** 9   !! can be larger than RAM+swap space

!> converts n elements to a number of bytes
nbytes = fmmap_elem2byte(n, storage_size(pt)) 

!> creates a mapping to a temporary file
call fmmap_create(x, FMMAP_SCRATCH, "", nbytes)

!> conversion to a Fortran pointer
call c_f_pointer(x%cptr(), pt, [n])       
     
!> work on pt(:) as if it was a classical array
! ...
! ...

!> closes the mapping and delete the file
call fmmap_destroy(x)                  
```

### basic example 2

```fortran
use iso_C_binding
use ffmap_m, fst => fmmap_size_t

integer, pointer, contiguous :: pi(:,:), tmpi(:,:)
type(fmmap_t) :: x
integer(fst) :: n
...
n = 1000_fst   !! can be larger than RAM+swap space

!> converts n*n elements to a number of bytes
nbytes = fmmap_elem2byte(n*n, storage_size(pi)) 

!> Mapping to a new named file
call fmmap_create(x, FMMAP_NEW, "./foo1.bin", nbytes) 

!> conversion to a Fortran pointer, in 2 stages because we need a lower bound /= 1
call c_f_pointer(x%cptr(), tmpi, [n,n])      
pi(0:n-1,1:n) => tmpi
                    
!> work on pi(:,:) as if it was a classical array
! ...
! ...

!> closes the mapping (the file is NOT deleted)
call fmmap_destroy(x)
```

### Private mapping

```fortran
use iso_C_binding
use ffmap_m, fst => fmmap_size_t 

integer, pointer :: pi(:)
type(fmmap_t) :: x
integer(fst) :: n
...

!> Mapping to a existing named file
call fmmap_create(x, nbytes, FMMAP_OLD, "./foo1.bin", private=.true.) 
n = fmmap_byte2elem(x%length(), storage_size(pi))
!> Conversion to a Fortran pointer
call c_f_pointer(x%cptr(), pi, [n])      
                    
!> work on pi(:) as if it was a classical array
!> All the changes are resident only in memory, the file is unmodified 
! ...
! ...

if (...) then
    !> Closes the mapping 
    !> All the changes are lost and the original file is kept
    call fmmap_destroy(x)
else
    !> Alternatively...
    !> All the changes are written back to the file before unmapping
    call fmmap_destroy(x,writeback=.true.)
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

## Limitations

- The whole file is mapped, it's not possible to map only a portion of it
- The access is always read+write, there's no option to restrict it
- The file is always opened with non-blocking access (i.e. it can be opened again by the
  same process or another process), which corresponds to the usual behavior on posix
  systems but not Windows. 
- Mapping to an array of a derived type containing allocatable or pointer components is 
  not allowed (well, it's technically possible, but the memory allocated by these  
  components won't be part of the mapping).
