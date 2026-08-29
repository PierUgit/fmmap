# fmmap 1.3.0 : memory mapped files in Fortran

See also:
- the ["detailed" documentation](doc/index.md)
- the ["examples"](doc/examples.md)

## Introduction

These routines provide *some* of the features of the **posix** or **Windows** memory mapped files under 
a simple and unique Fortran interface.

2 main usages:
- allocating arrays that are potentially bigger than the RAM+swap size, and which are backed 
  by a temporary file
- opening existing files or creating new files (still potentially bigger than the RAM+swap size), 
  and mapping them to an array

It can also provide a form of inter-process communication.

Private mapping is possible, with optional write-back of the modifications to the file. 
Such mapping can be useful when one doesn't want to modify the file on disk, or when one wants 
to work only in memory and decide when to update (or not) the file. 

Anonmymous mapping is also possible, i.e. allocating virtual memory without a physical backing file. 
This is actually what the C malloc() generally does (and therefore also what the Fortran allocate() does) 
when the allocated size is above some threshold. This option is provided to easily switch 
between on-disk and in-memory only modes by just changing an argument in the calls. 

## Usage

The user creates a mapping by basically telling that he wants `n` elements of an given type/kind. 
The type+kind is defined through a `mold=` parameter. Then they can associate a Fortran pointer to
the mapping. This is as simple as:
```fortran
use iso_C_binding
type(fmmap_t) :: x
real, pointer :: a(:)
integer(c_size_t) :: n = 10**9

call x%create( FMMAP_SCRATCH, "", n, mold=0.0 ) ! creates the mapping to a temporary file
call c_f_pointer( x%cptr(), a, [n] )            ! associates a pointer to the mapping

!> work on a(:) as if it was a classical array in memory
! ...
```

- C interoperability (iso_C_binding) is required, as C is used behind the scene. 
- Only the type+kind of the `mold=` argument matters; it can be a literal constant or a
  variable, a scalar or an array of any rank and size, it is just used to determine the 
  size in memory of an elements of that type+kind

More ["detailed" documentation](doc/index.md)  
More [examples](doc/examples.md)

## Compilation

The repository has an fpm (Fortran Package Manager) structure:
```
fpm test
```
On Windows, the presence of the `_WIN32` macro is required

### Tested on
macOS 26         /  gcc-gfortran 15 
Windows 11 MSYS2 /  gcc-gfortran 13  
Linux Debian 11  /  Intel icc-ifort 2021  
Lubuntu 22.04    /  gcc-gfortran 11   (up to v0.11.3)

It has been used in production on Linux.

### Known compilation issues

Under Windows MSYS2 the `_WIN32` macro is not defined in gfortran (while it is in gcc). 
I don't know how it is under environments others than MSYS2. The fix is to pass it 
explicitly: `fpm test --flag "-D_WIN32"`

## Limitations

- It is assumed that the Fortran file storage unit is a byte. This is checked in the routines.
- The whole file is mapped, it's not possible to map only a portion of it
- The access is always read+write, there's no option to restrict it
- The file is always opened with non-blocking access (i.e. it can be opened again by the 
  same process or another process), which corresponds to the usual behavior on posix systems 
  but not Windows. 
- Mapping to an array of a derived type containing allocatable or pointer components is 
  not allowed (well, it's technically possible, but the memory allocated by these components 
  won't be part of the mapping).

## Known issues

None...
