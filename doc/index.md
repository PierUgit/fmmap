# Memory Mapped Files in Fortran - Crude documentation

Back to [README](../README.md)

## Generalities

### Private feature:

- The mapped file can still be bigger than the RAM+swap size, however the amount of writes 
  is limited by the RAM+swap size (to overcome this, one can close the mapping with 
  write-back and remap the file
- Currently, write-back means the entire file is rewritten, whatever the amount of 
  modifications. This can be inefficient. If the underlying filesystem natively supports 
  copy-on-write, a better strategy consists in creating a copy of the file with a system 
  call, and mapping the copy without the private feature.

### Non blocking access

The files are opened with non-blocking read and write accesses, which means that nothing 
prevents multiples threads or other processes to concurrently open and write into the same 
files. This is the responsability of the user to take car of that.

### Error status

In case of incorrect or inconsistent input arguments, the routines always print an 
explicit message and the program aborts.

In case something goes unexpectedly wrong internally (file can't be opened, or mapped, or closed...):
- if the output `stat` argument is present the routines return an error code and the 
  execution continues. The specific error can be inquired with the `fmmap_errmsg()` function.
- if the output `stat` argument is not present, the routines print an explicit message 
  and the program aborts.

## Module

`use fmmap`

## public kinds

/

## public types

`fmmap_t` : derived type holding the properties of the mapping, with no public component.

## public type-bound procedures

`type(fmmap_t) :: x` 

| type-bound procedure       | => module procedures   |                                            |
| -------------------------- | ---------------------- | ------------------------------------------ |
| (S) `x%create()`           | `fmmap_t_create()`     | creates a mapping                          |
| (F) `x%cptr()`             | `fmmap_t_get_cptr()`   | returns the C pointer of the mapping       |
| (f) `x%length()`           | `fmmap_t_get_length()` | returns the size of the mapping            |
| (S) `x%destroy()`          | `fmmap_t_destroy()`    | destroys a mapping                         |

## public constants

`FMMAP_SCRATCH` : mapping of a temporary file which is deleted once the mapping is closed  
`FMMAP_NEW`     : mapping of newly created file (aborts if the file exists beforehand)  
`FMMAP_OLD`     : mapping of an existing file (aborts if the file does not exist beforehand)  
`FMMAP_NOFILE`  : mapping without a backing file (anonymous mapping)

## procedures pointed by the type-bound procedures

### `fmmap_t_create`

```Fortran
   !********************************************************************************************
   !> @brief
   !! Opens a file and creates a "generic" mapping to a C pointer.
   !! The whole file is mapped.
   !! 
   !! This is a type-bound procedure, which means that the invocation is actually:
   !! ```
   !! call x%create( <all arguments but x> )
   !! ```
   !********************************************************************************************
   subroutine fmmap_t_create(x,filestatus,filename,length,mold,private,stat)
   !********************************************************************************************
   class(fmmap_t),        intent(out)           :: x
      !< descriptor of the mapped file
   integer,               intent(in)            :: filestatus
      !< FMMAP_SCRATCH: mapping a temporary file  
      !! FMMAP_OLD    : mapping an already existing file  
      !! FMMAP_NEW    : mapping a newly created created file  
      !! FMMAP_NOFILE : no physical file
   character(*),          intent(in)            :: filename
      !< FMMAP_OLD or FMMAP_NEW: name of the file (with or without path)  
      !! FMMAP_SCRATCH: name of the path where the temporary file is created
      !! - (a processor dependent unique filename is generated and appended to the path);
      !! - if blank:
      !!   - POSIX: the current directory ("./") is used
      !!   - WIN32: the Windows temporary path is inquired and used
      !! FMMAP_NOFILE:  must be empty ("")
   integer(c_size_t)                            :: length
      !< Size of the file (or virtual file)  
      !! FMMAP_SCRATCH, FMMAP_NEW, and FMMAP_NOFILE: input length of the mapping  
      !! FMMAP_OLD: output length of the mapping
   class(*),              intent(in)            :: mold(..)
      !< length` is expressed in number of elements of the type/kind `mold`
   logical,               intent(in),  optional :: private
      !< if .true., all the changes made to the mapped file are visible only by the current
      !! mapping. All concurrent accesses to the file see the original data and not the
      !! changes. Technically the changes are permanently cached in memory pages dedicated
      !! to current mapping.
      !! - is .false. by default with FMMAP_NEW, FMMAP_OLD, and FMMAP_SCRATCH
      !! - is .true. by default with FMMAP_NOFILE
   integer,               intent(out), optional :: stat
      !< return status; is 0 if no error occurred
```

### `fmmap_t_get_cptr`

```Fortran
   !********************************************************************************************
   !> @brief
   !! Returns the C pointer of a mapped file
   !!
   !! This is a type-bound procedure, which means that the invocation is actually:
   !! ```
   !! mycptr = x%cptr()
   !! ```
   !********************************************************************************************
   function fmmap_t_get_cptr(x) result(cptr)
   !********************************************************************************************
   class(fmmap_t), intent(in) :: x
      !< descriptor of the mapped file
   type(c_ptr)                :: cptr
      !< the output C pointer
```

### `fmmap_t_get_length`

```fortran
   !********************************************************************************************
   !> @brief
   !! Returns the length of a mapped file
   !!
   !! This is a type-bound procedure, which means that the invocation is actually:
   !! ```
   !! mylength = x%length( <all arguments but x> )
   !! ```
   !********************************************************************************************
   function fmmap_t_get_length(x,mold) result(length)
   !********************************************************************************************
   class(fmmap_t), intent(in)            :: x
      !< descriptor of the mapped file
   class(*),       intent(in)            :: mold(..)
      !< the returned length is expressed in number of elements of the type/kind `mold`
   integer(c_size_t)                     :: length
      !< length in elements of the type/kind of ` mold`
```

### `fmmap_t_destroy`

```Fortran
   !********************************************************************************************
   !> @brief
   !! Destroys a generic mapping
   !!
   !! This is a type-bound procedure, which means that the invocation is actually:
   !! ```
   !! call x%destroy()
   !! ```
   !********************************************************************************************
   subroutine fmmap_t_destroy(x,writeback,stat)
   !********************************************************************************************
   class(fmmap_t), intent(inout)          :: x
      !< descriptor of the mapped file
   logical,        intent(in),   optional :: writeback
      !< If .true., the changes in memory in the private mode are written back to the file
      !! before unmapping.
      !! - is .false. by default with FFMAP_SCRATCH, FMMAP_OLD, and FFMAP_NOFILE
      !! - is .true. by default with FMMAP_NEW
   integer,       intent(out),   optional :: stat
      !< return status, is 0 if no error occurred
```

### `fmmap_t_create_bytes` - DEPRECATED

```Fortran
   !********************************************************************************************
   !> @brief
   !! DEPRECATED : always use fmmap_t_create()
   !!
   !! Same as fmmap_t_create(), with `mold` occupying 1 byte  
   !********************************************************************************************
   subroutine fmmap_t_create_bytes(x,filestatus,filename,length,private,stat)
```

### `fmmap_t_get_length_bytes` - DEPRECATED

```Fortran
   !********************************************************************************************
   !> @brief
   !! DEPRECATED : always use fmmap_t_get_length()
   !!
   !! same as fmmap_t_get_length(), with `mold` occupying 1 byte  
   !********************************************************************************************
   function fmmap_t_get_length_bytes(x) result(length)
```

## Public utility procedures

### `fmmap_errmsg`

```Fortran
   !********************************************************************************************
   !> @brief
   !! Returns the error messages corresponding to an error code
   !********************************************************************************************
   function fmmap_errmsg(stat) result(msg)
   !********************************************************************************************
   integer, intent(in) :: stat            !< error code
   character(len=:), allocatable :: msg   !< corresponding error message
```

### `fmmap_sizeof` 

```Fortran
   !********************************************************************************************
   !> @brief
   !! Returns the number of bytes occupied in memory by a scalar object of any type
   !!
   !! Can be useful for advanced usages of the library, not for a standard usage 
   !********************************************************************************************
   function fmmap_sizeof(object) result(nbytes)
   !********************************************************************************************
   class(*), intent(in) :: object(..)   !< object of any type (unlimited polymorphic)
   integer(c_size_t) :: nbytes          !< number of bytes of a scalar of object type
```

### `fmmap_e2b` - DEPRECATED

```Fortran
   !********************************************************************************************
   !> @brief
   !! DEPRECATED : USE fmmap_sizeof() INSTEAD
   !!
   !! converts a number of elements to a number of bytes
   !! `ss` is typically obtained with the intrinsic function `ss = storage_size(var)`,
   !!  where `var` is any variable of the manipulated type+kind
   !********************************************************************************************
   function fmmap_e2b(nelems,ss) result(nbytes)
   !********************************************************************************************
   integer(c_size_t), intent(in) :: nelems   !< number of elements
   integer,           intent(in) :: ss       !< storage size (in bits) of 1 element
   integer(c_size_t)             :: nbytes   !< number of bytes
```

### `fmmap_b2e` - DEPRECATED

```
   !********************************************************************************************
   !> @brief
   !! DEPRECATED : USE fmmap_sizeof() INSTEAD
   !!
   !! converts a number of bytes to a number of elements
   !! `ss` is typically obtained with the intrinsic function `ss = storage_size(var)`,
   !!  where `var` is any variable of the manipulated type+kind
   !********************************************************************************************
   function fmmap_b2e(nbytes,ss) result(nelems)
   !********************************************************************************************
   integer(c_size_t), intent(in) :: nbytes   !< number of nbytes
   integer,           intent(in) :: ss       !< storage size (in bits) of 1 element
   integer(c_size_t)             :: nelems   !< number of elements
```

