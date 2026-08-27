# fmmap 1.2.0 : memory mapped files in Fortran

See also the ["detailed" documentation](doc/index.md)

## Introduction

These routines provide *some* of the features of the **posix** or **Windows** memory mapped files under 
a simple and unique Fortran interface.

2 main usages:
- allocating arrays that are potentially bigger than the RAM+swap size, and which are backed 
  by a temporary file
- opening existing files or creating new files (still potentially bigger than the RAM+swap size), 
  and mapping them to an array

Private mapping is possible, with optional write-back of the modifications to the file. 
Such mapping can be useful when one doesn't want to modify the file on disk, or when one wants 
to work only in memory and decide when to update (or not) the file. 

Anonmymous mapping is also possible, i.e. allocating virtual memory without a physical backing file. 
This is actually what the C malloc() generally does (and therefore also what the Fortran allocate() does) 
when the allocated size is above some threshold. This option is provided to easily switch 
between on-disk and in-memory only modes by just changing an argument in the calls. 

## Usage

The user creates a mapping by basically telling that he wants `n` elements of an given type/kind. 
The type/kind is defined through a `mold=` parameter. Then they can associate a Fortran pointer to
the mapping. C interoperabilty is required, as C is used beyond the scene). See: 
```fortran
use iso_fortran_env
use iso_C_binding
type(fmmap_t) :: x
real(kind=real64), pointer :: a(:)
integer(c_size_t) :: n = 10**9

call x%create( FMMAP_SCRATCH, "", n, mold=a )
call c_f_pointer( x%cptr(), a, [n] )
```

### Example 1

```fortran
use iso_C_binding, cst => c_size_t
use fmmap

type sometype
   integer :: i
   double precision :: a
   character(len=7) :: str
end type
type(sometype), pointer :: pt(:)
type(fmmap_t) :: x

integer(cst) :: n,
...
...
n = 10_cst ** 9   !! can be larger than RAM+swap space

!> creates a mapping to a temporary file
call fmmap_create(x, FMMAP_SCRATCH, "", n, mold=pt)

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
use iso_C_binding, cst => c_size_t
use fmmap

integer, pointer, contiguous :: pi(:,:), tmpi(:,:)
type(fmmap_t) :: x
integer(cst) :: n
...
n = 1000_cst   !! can be larger than RAM+swap space

!> Mapping to a new named file for n*n integer elements
call x%create( FMMAP_NEW, "./foo1.bin", n*n, mold=pi ) 

!> conversion to a Fortran pointer, in 2 stages because we need a lower bound /= 1
call c_f_pointer(x%cptr(), tmpi, [n,n])      
pi(0:n-1,1:n) => tmpi
                    
!> work on pi(:,:) as if it was a classical array
! ...
! ...

!> closes the mapping (the file is NOT deleted)
call x%destroy()
```

### Private mapping

```fortran
use iso_C_binding, cst => c_size_t
use fmmap

integer, pointer :: pi(:)
type(fmmap_t) :: x
integer(cst) :: n
...

!> Mapping to a existing named file
call x%create( FMMAP_OLD, "./foo1.bin", n, mold=0, private=.true.) 
!> Conversion to a Fortran pointer
call c_f_pointer( x%cptr(), pi, [n] )      
                    
!> work on pi(:) as if it was a classical array
!> All the changes are resident only in memory, the file is unmodified 
! ...
! ...

if (...) then
    !> Closes the mapping 
    !> All the changes are lost and the original file is kept
    call x%destroy()
else
    !> Alternatively...
    !> All the changes are written back to the file before unmapping
    call x%destroy( writeback=.true. )
end if
```


## Compilation

The repository has an fpm (Fortran Package Manager) structure:
```
fpm test
```
On Windows, the presence of the `_WIN32` macro is assumed

### Tested on
macOS 26   /   gcc-gfortran 16  
Windows 11 MSYS2   /   gcc-gfortran 13  
Linux Debian 11   /   Intel icc-ifort 2021  
(up to v0.11.3: Lubuntu 22.04   /   gcc-gfortran 11)

### Known compilation issues

Under Windows MSYS2 the `_WIN32` macro is not defined in gfortran (while it is in gcc). I don't know how it is under environments others than MSYS2. The fix is to pass it explicitly: `fpm test --flag "-D_WIN32"`

## Limitations

- It is assumed that the Fortran file storage unit is a byte. This is checked in the routines.
- The whole file is mapped, it's not possible to map only a portion of it
- The access is always read+write, there's no option to restrict it
- The file is always opened with non-blocking access (i.e. it can be opened again by the same process or another process), which corresponds to the usual behavior on posix systems but not Windows. 
- Mapping to an array of a derived type containing allocatable or pointer components is not allowed (well, it's technically possible, but the memory allocated by these components won't be part of the mapping).

## Known issues

None...
