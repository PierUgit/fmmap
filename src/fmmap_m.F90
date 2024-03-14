!***********************************************************************************************
module fmmap_m
!***********************************************************************************************
use, intrinsic :: iso_c_binding
use, intrinsic :: iso_fortran_env
implicit none

   private
   public :: fmmap_size, fmmap_bigint, fmmap_t, fmmap_nbytes, fmmap_nelems
   public :: fmmap_create, fmmap_destroy
   public :: FMMAP_SCRATCH, FMMAP_OLD, FMMAP_NEW

   !> integer kind used for the sizes and and number of bytes or elements
   integer, parameter :: fmmap_size = c_long_long
   
   !> integer kind of at least 18 digits range for pointers in fmmap_create and fmmap_destroy
   !> mapped to int64 if defined
   integer, parameter :: fmmap_bigint = merge(int64,selected_int_kind(r=18),int64 > 0)
   
   character(c_char) :: c
   integer, parameter :: bitsperbyte = storage_size(c)
   
   type, bind(C) :: fmmap_t   ! descriptor
      private
      type(c_ptr), public :: cptr = c_null_ptr
      integer(fmmap_size) :: cn
      integer(c_int)      :: cfilemode
#ifdef _WIN32
      type(c_ptr)         :: cfiledes = c_null_ptr;
      type(c_ptr)         :: cmapdes  = c_null_ptr
#else
! posix assumed
      integer(c_int)      :: cfiledes
#endif
      logical(c_bool)     :: used = .true.
   end type
   
   type(fmmap_t), allocatable :: table(:)
   
   !> predefined values for the `filemode` argument
   integer, parameter :: FMMAP_SCRATCH = 1
   integer, parameter :: FMMAP_OLD     = 2
   integer, parameter :: FMMAP_NEW     = 3
   
   character(*), parameter :: msg0="*** fmmap_create(): "
   character(*), parameter :: msg1="the rank of must be 1 to 7"
   character(*), parameter :: msg2="wrong size of sh"
   character(*), parameter :: msg3="requested pointer shape incompatible with file size"
   
   interface
   
      integer(c_int) function c_mmap_create( x, cfilename ) BIND(C)
         import :: c_int, c_char, fmmap_t
         type(fmmap_t),                intent(inout) :: x 
         character(kind=c_char,len=1), intent(in)    :: cfilename(*)
      end function c_mmap_create

      integer(c_int) function c_mmap_destroy( x ) BIND(C)
         import :: c_int, c_char, fmmap_t
         type(fmmap_t),                intent(inout) :: x 
      end function c_mmap_destroy
      
   end interface
   
   !> Generic routine name that creates either a generic mapping to a C pointer or a mapping
   !> to array pointers of some selected intrinsic types/kinds.
   !> This routine is not thread safe !
   interface fmmap_create
      module procedure fmmap_create_cptr
      module procedure fmmap_create_real,    fmmap_create_dp
      module procedure fmmap_create_complex, fmmap_create_dc
      module procedure fmmap_create_integer, fmmap_create_di
   end interface
   
   !> Generic routine name that destroys an existing mapping
   !> This routine is not thread safe !
   interface fmmap_destroy
      module procedure fmmap_destroy_cptr
      module procedure fmmap_destroy_real,    fmmap_destroy_dp
      module procedure fmmap_destroy_complex, fmmap_destroy_dc
      module procedure fmmap_destroy_integer, fmmap_destroy_di
   end interface
   
contains
   
   !********************************************************************************************
   function fmmap_nbytes(n,ss)
   !********************************************************************************************
   !! converts a number of elements to a number of bytes
   !********************************************************************************************
   integer(fmmap_size), intent(in) :: n   !! number of elements
   integer,             intent(in) :: ss  !! storage size (in bits) of 1 element
   integer(fmmap_size) :: fmmap_nbytes    !! number of bytes
   !********************************************************************************************
   fmmap_nbytes = n * (ss / bitsperbyte)
   end function fmmap_nbytes
   
   !********************************************************************************************
   function fmmap_nelems(nbytes,ss)
   !********************************************************************************************
   !! converts a number of bytes into a number of elements
   !********************************************************************************************
   integer(fmmap_size), intent(in) :: nbytes !! number of nbytes
   integer,             intent(in) :: ss     !! storage size (in bits) of 1 element
   integer(fmmap_size) :: fmmap_nelems   !! number of elements
   
   integer(fmmap_size) :: bytesperelem
   !********************************************************************************************
   bytesperelem = ss / bitsperbyte
   fmmap_nelems = nbytes / bytesperelem
   if (fmmap_nelems * bytesperelem /= nbytes) then
      error stop "*** fmmap_nelems(): the number of bytes does not form an integer number of elements"
   end if
   end function fmmap_nelems
   

   !********************************************************************************************
   subroutine fmmap_create_cptr(x,nbytes,filemode,filename)
   !********************************************************************************************
   !! Creates a "generic" mapping to a C pointer, stored in `x%cptr
   !********************************************************************************************
   type(fmmap_t),    intent(out)           :: x   !! descriptor
   integer(fmmap_size)                     :: nbytes 
      !! input requested size (for filemode 1 or 3), 
      !! or output size of existing file (for filemode 2)
   integer,          intent(in)            :: filemode !! FILE_SCRATCH, FILE_OLD, or FILE_NEW
   character(*),     intent(in),  optional :: filename 
     !! FILE_OLD or FILE_new: required name of the file (with or without path)
     !! FILE_SCRATCH: name of the path; not required;
     !! - if not present:
     !!   - POSIX: is "." (current directory) by default
     !!   - WIN32: the Windows temporary path is inquired     
     !! - a processor dependent unique filename is be generated
   
   integer :: i, lu, stat
   character(:), allocatable :: filename___
   character(kind=c_char,len=:), allocatable :: c_filename
   character(128) :: msg
   !********************************************************************************************

   if (file_storage_size /= bitsperbyte) then
      error stop "*** fmmap_init: the file storage unit is not a byte"
   end if
   
   x%cfilemode = filemode
   x%used      = .true.
   
   if (filemode == FMMAP_SCRATCH) then
      x%cn = nbytes
      filename___ = "" ; if (present(filename)) filename___ = trim(filename)
   else if (filemode == FMMAP_OLD) then
      filename___ = filename
      inquire(file=trim(filename___), size=x%cn)
      if (x%cn < 0) then
         error stop "*** fmmap_create_cptr: unable to get the file size"
      end if
      nbytes = x%cn
   else if (filemode == FMMAP_NEW) then
      x%cn = nbytes
      filename___ = filename
      open(newunit=lu,file=filename___,status='new',form='unformatted',access='stream')
      write(lu,pos=x%cn) c_null_char
      close(lu)
   else
      error stop "*** fmmap_create_cptr: wrong filemode"
   end if
   
   c_filename = filename___ // c_null_char
   stat = c_mmap_create( x, c_filename )
   if (stat /= 0) then
      write(msg,*) "*** fmmap_create_cptr: error code ", stat
      error stop trim(msg)
   end if
                  
   end subroutine fmmap_create_cptr


   !********************************************************************************************
   subroutine fmmap_destroy_cptr(x)
   !********************************************************************************************
   !! Destroys a generic mapping 
   !! (the file is unmapped and closed, and `x%cptr` is set to `c_null_ptr`)
   !********************************************************************************************
   type(fmmap_t), intent(inout) :: x   !! descriptor
   
   integer :: i, stat
   character(128) :: msg
   !********************************************************************************************
   
   if (.not.c_associated(x%cptr)) then
      error stop "*** fmmap_destroy_cptr: attempt to free a non associated pointer"
   end if
      
   stat = c_mmap_destroy( x )
   if (stat /= 0) then
      write(msg,*) "*** fmmap_destroy_cptr: error code ", stat
      error stop trim(msg)
   end if
   
   end subroutine fmmap_destroy_cptr
   
   
   !********************************************************************************************
   subroutine fmmap_create_real(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   !! Creates a mapping to a `real` pointer `p`
   !! This routine is not thread safe !
   !********************************************************************************************
   real, pointer :: p(..)   ! on output, `p` points to the mapped file
   real, pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_real


   !********************************************************************************************
   subroutine fmmap_create_dp(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   !! Creates a mapping to a `double precision` pointer `p`
   !! This routine is not thread safe !
   !********************************************************************************************
   double precision, pointer :: p(..)   ! on output, `p` points to the mapped file
   double precision, pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_dp


   !********************************************************************************************
   subroutine fmmap_create_complex(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   !! Creates a mapping to a `complex` pointer `p`
   !! This routine is not thread safe !
   !********************************************************************************************
   complex, pointer :: p(..)   ! on output, `p` points to the mapped file
   complex, pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_complex


   !********************************************************************************************
   subroutine fmmap_create_dc(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   !! Creates a mapping to a `double complex` pointer `p`
   !! This routine is not thread safe !
   !********************************************************************************************
   complex(kind=kind(0d0)), pointer :: p(..)   ! on output, `p` points to the mapped file
   complex(kind=kind(0d0)), pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_dc


   !********************************************************************************************
   subroutine fmmap_create_integer(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   !! Creates a mapping to a `integer` pointer `p`
   !! This routine is not thread safe !
   !********************************************************************************************
   integer, pointer :: p(..)   ! on output, `p` points to the mapped file
   integer, pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_integer


   !********************************************************************************************
   subroutine fmmap_create_di(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   !! Creates a mapping to a `integer(kind=fmmap_bigint)` pointer `p`
   !! This routine is not thread safe !
   !********************************************************************************************
   integer(kind=fmmap_bigint), pointer :: p(..)   ! on output, `p` points to the mapped file
   integer(kind=fmmap_bigint), pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_di


   !********************************************************************************************
   subroutine fmmap_destroy_real(p)
   !********************************************************************************************
   !! Destroys a mapping to a `real` pointer
   !! (the file is unmapped and closed, and the pointer is nullified)
   !! This routine is not thread safe !
   !********************************************************************************************
   real, pointer :: p(..)   ! the pointer associated to the mapping to destroy

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_real
   

   !********************************************************************************************
   subroutine fmmap_destroy_dp(p)
   !********************************************************************************************
   !! Destroys a mapping to a `double precision` pointer
   !! (the file is unmapped and closed, and the pointer is nullified)
   !! This routine is not thread safe !
   !********************************************************************************************
   double precision, pointer :: p(..)   ! the pointer associated to the mapping to destroy

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_dp


   !********************************************************************************************
   subroutine fmmap_destroy_complex(p)
   !********************************************************************************************
   !! Destroys a mapping to a `complex` pointer
   !! (the file is unmapped and closed, and the pointer is nullified)
   !! This routine is not thread safe !
   !********************************************************************************************
   complex, pointer :: p(..)   ! the pointer associated to the mapping to destroy

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_complex
   

   !********************************************************************************************
   subroutine fmmap_destroy_dc(p)
   !********************************************************************************************
   !! Destroys a mapping to a `double complex` pointer
   !! (the file is unmapped and closed, and the pointer is nullified)
   !! This routine is not thread safe !
   !********************************************************************************************
   complex(kind=kind(0d0)), pointer :: p(..)   ! the pointer associated to the mapping to destroy

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_dc


   !********************************************************************************************
   subroutine fmmap_destroy_integer(p)
   !********************************************************************************************
   !! Destroys a mapping to a `integer` pointer
   !! (the file is unmapped and closed, and the pointer is nullified)
   !! This routine is not thread safe !
   !********************************************************************************************
   integer, pointer :: p(..)   ! the pointer associated to the mapping to destroy

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_integer
   

   !********************************************************************************************
   subroutine fmmap_destroy_di(p)
   !********************************************************************************************
   !! Destroys a mapping to a `integer(kind=fmmap_bigint)` pointer
   !! (the file is unmapped and closed, and the pointer is nullified)
   !! This routine is not thread safe !
   !********************************************************************************************
   integer(kind=fmmap_bigint), pointer :: p(..)   ! the pointer associated to the mapping to destroy

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_di
   
   
   !********************************************************************************************
   subroutine fmmap_table_push(x)
   !********************************************************************************************
   ! Stores a descriptor into the internal table
   !********************************************************************************************
   type(fmmap_t), intent(in) :: x
   
   integer :: i
   logical(c_bool), allocatable :: used(:)
   !********************************************************************************************
   if (.not.allocated(table)) then
      table = [x]
   else
      used = table(:)%used
      i = findloc( used, .false., dim=1 )
      if (i > 0) then ; table(i) = x
                 else ; table = [ table, x ]
      end if
   end if
   end subroutine fmmap_table_push
   
   !********************************************************************************************
   subroutine fmmap_table_pull(x,cptr)
   !********************************************************************************************
   ! Removes from the internal table the descriptor that matches the given pointer
   !********************************************************************************************
   type(fmmap_t), intent(out) :: x   ! descriptor
   type(c_ptr),   intent(in)  :: cptr   ! pointer to search in the table
   
   integer :: i
   logical :: found
   logical(c_bool), allocatable :: used(:)
   !********************************************************************************************
   if (.not.allocated(table)) then
      error stop "*** fmmap_destroy(): the internal table is not allocated"
   end if
   i = 1
   do while (i <= size(table))
      if (c_associated(table(i)%cptr,cptr)) exit
      i = i+1
   end do
   if (i > size(table)) then
      error stop "*** fmmap_destroy(): pointer not found in the internal table"
   end if
   x = table(i)
   x%cptr = table(i)%cptr   ! not required(?)... 
                            ! But potentially ifort 21 crash without that
   table(i)%used = .false.

   used = table(:)%used
   if (count(used) == 0) then
      deallocate(table)
   else if (size(table) >= 100 .and. count(used) < 0.75*size(table)) then
      table = pack( table, mask=used )
   end if

   end subroutine fmmap_table_pull
   

end module fmmap_m
