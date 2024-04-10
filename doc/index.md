# Memory Mapped Files in Fortran - Crude documentation

See also the [README](../README.md)

## Generalities

About the types:
- The  intrinsic types versions of the routines handle a selection of types:
  - `real(real32)` (<-> default `real` on virtually all existing processors)
  - `real(real64)` (<-> `double precision` on virtually all existing processors)
  - `complex(real32)`
  - `complex(real64)`
  - `integer(int32)` (<-> default `integer` on virtually all existing processors)
  - `integer(int64)`
  - `character(kind=c_char)` (useful for byte-based addressing)


About the Copy-on-Write feature:
- The mapped file can still be bigger than the RAM+swap size, however the amount of writes is limited by the RAM+swap size (to overcome this, one can close the mapping with write-back and remap the file
- Currently, write-back means the entire file is rewritten, whatever the amount of modifications. This can be inefficient. If the underlying filesystem natively supports copy-on-write, a better strategy consists in creating a copy of the file with a system call, and mapping the copy without Copy-on-Write.

## Module

`use fmmap`

## public kinds

`fmmap_size_t` : integer kind used for the sizes in the interfaces. 
- the range of `integer(kind=fmmap_size_t)` is large enough to handle any possible file
- generally corresponds to `int64`, but without guarantee

## public types

None

## public constants

`FMMAP_SCRATCH` : mapping of a temporary file which is deleted once the mapping is closed  
`FMMAP_NEW`     : mapping of newly created file  
`FMMAP_OLD`     : mapping of an existing file

## public procedures 

### `fmmap_create` (C pointer version)

```
   !********************************************************************************************
   subroutine fmmap_create_cptr(cptr,nbytes,filemode,filename,copyonwrite)
   !********************************************************************************************
   !! Opens a file and creates a "generic" mapping to a C pointer.  
   !! The whole file is mapped.  
   !! This routine is not thread safe.
   !********************************************************************************************
   type(c_ptr),           intent(out)           :: cptr   
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
```

### `fmmap_create` (intrinsic types version)

`<sometype>` can be:
- `real(real32)`
- `real(real64)`
- `complex(real32)`
- `complex(real64)`
- `integer(int32)`
- `integer(int64)`
- `character(kind=c_char)`

```  !********************************************************************************************
   subroutine fmmap_create(p,shape,filemode,filename,lower,copyonwrite)
   !********************************************************************************************
   !! Opens a file and creates a mapping to a <sometype> pointer `p`  
   !! The whole file is mapped.  
   !! This routine is not thread safe.
   !********************************************************************************************
   <sometype>, pointer :: p(..)   !! on output, `p` points to the mapped file
   integer(fmmap_size_t), intent(in)         :: shape(:) 
      !! shape of the pointer
      !! - `size(shape)` must be equal to the rank of `p`
      !! - With `FMMAP_OLD` the last element of `shape` can be negative. In this case,
      !!   the size of the last dimension of `p` is determined from the file size. This is
      !!   the recommended method.
   integer,             intent(in)           :: filemode 
      !! FMMAP_SCRATCH, FMMAP_OLD, or FMMAP_NEW
   character(*),        intent(in), optional :: filename
     !! FMMAP_OLD or FMMAP_new: required name of the file (with or without path)
     !! FMMAP_SCRATCH: name of the path; not required;
     !! - if not present:
     !!   - POSIX: is "." (current directory) by default
     !!   - WIN32: the Windows temporary path is inquired     
     !! - a processor dependent unique filename is be generated
   integer(fmmap_size_t), intent(in), optional :: lower(:)
      !! lower bounds of the output pointer;
      !! the size of `lower` must be equal to the rank of `p`
   logical,               intent(in), optional :: copyonwrite
      !! if .true., all the changes made to the mapped file stay only in memory
      !! and are not written back to the file.
```

### `fmmap_destroy` (C pointer version)

```
   !********************************************************************************************
   subroutine fmmap_destroy_cptr(cptr,writeback)
   !********************************************************************************************
   !! Destroys a generic mapping and closes the file.  
   !! This routine is not thread safe.
   !********************************************************************************************
   type(c_ptr), intent(inout)           :: cptr
      !! C pointer to the mapped file; is nullified on output
   logical,     intent(in)   , optional :: writeback  
      !! if .true., the changes in memory in the copyonwrite mode are written back to the file
```

### `fmmap_destroy` (intrinsic types version)

`<sometype>` can be:
- `real(real32)`
- `real(real64)`
- `complex(real32)`
- `complex(real64)`
- `integer(int32)`
- `integer(int64)`
- `character(kind=c_char)`

```
   !********************************************************************************************
   subroutine fmmap_destroy(p,writeback)
   !********************************************************************************************
   !! Destroys a mapping to a <sometype> pointer and closes the file.  
   !! This routine is not thread safe.
   !********************************************************************************************
   <sometype>, pointer :: p(..)   
      !! pointer to the mapped file; is nullified on output
   logical,     intent(in)   , optional :: writeback
      !! if .true., the changes in memory in the copyonwrite mode are written back to the file
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

