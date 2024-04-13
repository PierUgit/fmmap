# Memory Mapped Files in Fortran - Crude documentation

See also the [README](../README.md)

## Generalities

### Copy-on-Write feature:

- The mapped file can still be bigger than the RAM+swap size, however the amount of writes is limited by the RAM+swap size (to overcome this, one can close the mapping with write-back and remap the file
- Currently, write-back means the entire file is rewritten, whatever the amount of modifications. This can be inefficient. If the underlying filesystem natively supports copy-on-write, a better strategy consists in creating a copy of the file with a system call, and mapping the copy without Copy-on-Write.

### Non blocking access

The files are opened with non-blocking read and write accesses, which means that nothing prevents multiples threads or other processes to concurrently open and write into the same files. This is the responsability of the user to take car of that.

## Module

`use fmmap`

## public kinds

`fmmap_size_t` : integer kind used for the sizes in the interfaces. 
- the range of `integer(kind=fmmap_size_t)` is large enough to handle any possible file
- generally corresponds to `int64`, but without guarantee

## public types

`fmmap_t` : derived type holding the properties of the mapping. Public components:
   - `% cptr` : the C pointer the memory area where the file is mapped

## public constants

`FMMAP_SCRATCH` : mapping of a temporary file which is deleted once the mapping is closed  
`FMMAP_NEW`     : mapping of newly created file  
`FMMAP_OLD`     : mapping of an existing file

## public procedures 

### `fmmap_create`

```    
   !********************************************************************************************
   subroutine fmmap_create(x,nbytes,filemode,filename,copyonwrite,stat)
   !********************************************************************************************
   !! Opens a file and creates a "generic" mapping to a C pointer.  
   !! The whole file is mapped.  
   !********************************************************************************************
   type(fmmap_t),         intent(out)           :: x
      !! C pointer to the mapped file
   integer(fmmap_size_t), intent(inout)         :: nbytes 
      !! input requested size (FMMAP_SCRATCH or FMMAP_NEW), 
      !! or output size of existing file (FMMAP_OLD)
   integer,               intent(in)            :: filemode 
      !! FMMAP_SCRATCH, FMMAP_OLD, or FMMAP_NEW
   character(*),          intent(in),  optional :: filename 
      !! FMMAP_OLD or FMMAP_new: required name of the file (with or without path)
      !! FMMAP_SCRATCH: name of the path; not required;
      !! - if not present:
      !!   - POSIX: is "." (current directory) by default
      !!   - WIN32: the Windows temporary path is inquired     
      !! - a processor dependent unique filename is be generated
   logical,               intent(in),  optional :: copyonwrite
      !! if .true., all the changes made to the mapped file stay only in memory
      !! and are not written back to the file.
   integer,               intent(out),  optional :: stat
      !! return status, is 0 if no error occurred
```

### `fmmap_destroy` (C pointer version)

```
   !********************************************************************************************
   subroutine fmmap_destroy(x,writeback,stat)
   !********************************************************************************************
   !! Destroys a generic mapping
   !********************************************************************************************
   type(fmmap_t), intent(inout)           :: x 
      !! C pointer to the mapped file; is nullified on output
   logical,     intent(in)   , optional :: writeback  
      !! if .true., the changes in memory in the copyonwrite mode are written back to the file
   integer,               intent(out),  optional :: stat
      !! return status, is 0 if no error occurred
```

## Public utility procedures

### `fmmap_nbytes`

```
   !********************************************************************************************
   function fmmap_nbytes(n,ss)
   !********************************************************************************************
   !! converts a number of elements to a number of bytes
   !********************************************************************************************
   integer(fmmap_size_t), intent(in) :: n   !! number of elements
   integer,               intent(in) :: ss  !! storage size (in bits) of 1 element
   integer(fmmap_size_t) :: fmmap_nbytes    !! number of bytes
```

### `fmmap_nelems`

```
   !********************************************************************************************
   function fmmap_nelems(nbytes,ss)
   !********************************************************************************************
   !! converts a number of bytes into a number of elements
   !********************************************************************************************
   integer(fmmap_size_t), intent(in) :: nbytes !! number of nbytes
   integer,               intent(in) :: ss     !! storage size (in bits) of 1 element
   integer(fmmap_size_t) :: fmmap_nelems   !! number of elements
```

### `fmmap_errmsg`

```
   !********************************************************************************************
   function fmmap_errmsg(stat) result(msg)
   !********************************************************************************************
   !! Returns the error messages corresponding to an error code
   !********************************************************************************************
   integer, intent(in) :: stat
   character(len=:), allocatable :: msg
```
