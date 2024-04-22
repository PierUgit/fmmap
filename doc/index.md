# Memory Mapped Files in Fortran - Crude documentation

See also the [README](../README.md)

## Generalities

### Private feature:

- The mapped file can still be bigger than the RAM+swap size, however the amount of writes is limited by the RAM+swap size (to overcome this, one can close the mapping with write-back and remap the file
- Currently, write-back means the entire file is rewritten, whatever the amount of modifications. This can be inefficient. If the underlying filesystem natively supports copy-on-write, a better strategy consists in creating a copy of the file with a system call, and mapping the copy without the private feature.

### Non blocking access

The files are opened with non-blocking read and write accesses, which means that nothing prevents multiples threads or other processes to concurrently open and write into the same files. This is the responsability of the user to take car of that.

## Module

`use fmmap_m`

## public kinds

`fmmap_size_t` : integer kind used for the sizes in the interfaces. 
- the range of `integer(kind=fmmap_size_t)` is large enough to handle any possible file
- generally corresponds to `int64`, but without any guarantee

## public types

`fmmap_t` : derived type holding the properties of the mapping, with no public component.

public type bound procedures, `type(fmmap_t) :: x`

`type(c_ptr) x%cptr()` : returns the C pointer of the mapping

## public constants

`FMMAP_SCRATCH` : mapping of a temporary file which is deleted once the mapping is closed  
`FMMAP_NEW`     : mapping of newly created file  
`FMMAP_OLD`     : mapping of an existing file
`FMMAP_NOFILE`  : mapping without a backing file (anonymous mapping)

## public procedures 

### `fmmap_create`

```    
   !********************************************************************************************
   subroutine fmmap_create(x,filestatus,filename,length,private,stat)
   !********************************************************************************************
   !! Opens a file and creates a "generic" mapping to a C pointer.  
   !! The whole file is mapped.  
   !********************************************************************************************
   type(fmmap_t),         intent(out)           :: x
      !! descriptor of the mapped file
   integer,               intent(in)            :: filestatus 
      !! FMMAP_SCRATCH: mapping a temporary file
      !! FMMAP_OLD    : mapping an already existing file
      !! FMMAP_NEW    : mapping a newly created created file
      !! FMMAP_NOFILE : no physical file
   character(*),          intent(in)            :: filename 
      !! FMMAP_OLD or FMMAP_NEW: 
      !! - name of the file (with or without path)
      !! FMMAP_SCRATCH: 
      !! - name of the path where the temporary file is created; if blank:
      !!   - POSIX: the current directory ("./") is used
      !!   - WIN32: the Windows temporary path is inquired and used 
      !! - a processor dependent unique filename is then generated and appended to the path
      !! FMMAP_NOFILE:
      !! - must be empty ("")
   integer(fmmap_size_t), intent(inout)         :: length 
      !! FMMAP_SCRATCH, FMMAP_NEW, and FMMAP_NOFILE:
      !!    input length of the mapping (in number of bytes)
      !! FMMAP_OLD:
      !!    output length of the mapping (in number of bytes)
      !! This is actually the size of the file (or virtual file)
   logical,               intent(in),  optional :: private
      !! if .true., all the changes made to the mapped file are visible only by the current
      !  mapping. All concurrent accesses to the file see the original data and not the 
      !! changes. Technically the changes are permanently cached in memory pages dedicated
      !! to current mapping.
      !! - .false. by default with FMMAP_NEW, FMMAP_OLD, and FMMAP_SCRATCH
      !! - .true. by default with FMMAP_NOFILE 
   integer,               intent(out), optional :: stat
      !! return status; is 0 if no error occurred
```


### `fmmap_destroy`

```
   !********************************************************************************************
   subroutine fmmap_destroy_cptr(x,writeback,stat)
   !********************************************************************************************
   !! Destroys a generic mapping
   !********************************************************************************************
   type(fmmap_t), intent(inout)           :: x 
      !! descriptor of the mapped file
   logical,     intent(in)   , optional :: writeback  
      !! If .true., the changes in memory in the private mode are written back to the file 
      !! before unmapping.
      !! .false. by default with FFMAP_SCRATCH, FMMAP_OLD, and FFMAP_NOFILE
      !! .true. by default with FMMAP_NEW 
   integer,               intent(out),  optional :: stat
      !! return status, is 0 if no error occurred
```

## Public utility procedures

### `fmmap_elem2byte`

```
   !********************************************************************************************
   function fmmap_elem2byte(nelems,ss) result(nbytes)
   !********************************************************************************************
   !! converts a number of elements to a number of bytes  
   !! `ss` is typically obtained with the intrinsic function `ss = storage_size(var)`,
   !!  where `var` is any variable of the manipulated type+kind
   !********************************************************************************************
   integer(fmmap_size_t), intent(in) :: nelems   !! number of elements
   integer,               intent(in) :: ss       !! storage size (in bits) of 1 element
   integer(fmmap_size_t)             :: nbytes   !! number of bytes
```

### `fmmap_byte2elem`

```
   !********************************************************************************************
   function fmmap_byte2elem(nbytes,ss) result(nelems)
   !********************************************************************************************
   !! converts a number of bytes into a number of elements
   !! `ss` is typically obtained with the intrinsic function `ss = storage_size(var)`,
   !!  where `var` is any variable of the manipulated type+kind
   !********************************************************************************************
   integer(fmmap_size_t), intent(in) :: nbytes   !! number of nbytes
   integer,               intent(in) :: ss       !! storage size (in bits) of 1 element
   integer(fmmap_size_t)             :: nelems   !! number of elements
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
