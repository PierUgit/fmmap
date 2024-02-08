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
   
   type fmmap_t   ! descriptor
      private
      type(c_ptr), public :: cptr = c_null_ptr
      integer(c_int)      :: cfd  = -1
      integer(fmmap_size) :: cn   = 0
      logical             :: used = .false.
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
   
      integer(c_int) function c_mmap_create( cp, n, cfm, cfilename, cfd ) BIND(C)
         import :: c_ptr, c_int, c_long_long, c_char
         type(c_ptr),                  intent(out) :: cp
         integer(c_long_long),         value       :: n
         character(kind=c_char,len=1), intent(in)  :: cfilename(*)
         integer(c_int),               value       :: cfm
         integer(c_int),               intent(out) :: cfd
      end function c_mmap_create

      integer(c_int) function c_mmap_destroy( cp, n, cfd ) BIND(C)
         import :: c_ptr, c_int, c_long_long
         type(c_ptr),          value :: cp
         integer(c_long_long), value :: n
         integer(c_int),       value :: cfd
      end function c_mmap_destroy
      
   end interface
   
   !> Generic routine name that creates either a generic mapping to a C pointer or a mapping
   !> to array pointers of some selected intrinsic types/kinds.
   interface fmmap_create
      module procedure fmmap_create_cptr
      module procedure fmmap_create_real,    fmmap_create_dp
      module procedure fmmap_create_complex, fmmap_create_dc
      module procedure fmmap_create_integer, fmmap_create_di
   end interface
   
   !> Generic routine name that destroys an existing mapping
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
     !! FILE_OLD or FILE_new: required name of the file
     !! FILE_SCRATCH: not required; if present:
     !!  - a processor dependent unique suffix will be appended
     !!  - can be a directory; in this case the trailing directory separator must be present,
     !!    e.g. `"/tmp/"` rather than "`/tmp`"
   
   integer(c_int) :: c_fm
   integer :: i, lu, stat
   character(:), allocatable :: filename___
   character(kind=c_char,len=:), allocatable :: c_filename
   character(128) :: msg
   !********************************************************************************************
   
   if (file_storage_size /= bitsperbyte) then
      error stop "*** fmmap_init: the file storage unit is not a byte"
   end if
   
   x%cptr = c_null_ptr
   
   if (filemode == FMMAP_SCRATCH) then
      x%cn = nbytes
      if (present(filename)) then
         filename___ = trim(filename)//"fmmaptmp"
      else 
         filename___ = "./fmmaptmp"
      end if
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
   c_fm = filemode
   stat = c_mmap_create( x%cptr      &
                       , x%cn        &
                       , c_fm         &
                       , c_filename  &
                       , x%cfd       )
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
      
   stat = c_mmap_destroy( x%cptr             &
                        , x%cn               &
                        , x%cfd              )
   if (stat /= 0) then
      write(msg,*) "*** fmmap_destroy_cptr: error code ", stat
      error stop trim(msg)
   end if
   
   end subroutine fmmap_destroy_cptr
   
   
   !********************************************************************************************
   subroutine fmmap_create_real(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   !! Creates a mapping to a `real` pointer `p`
   !********************************************************************************************
   real, pointer :: p(..)   ! on output, `p` points to the mapped file
   real, pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_real


   !********************************************************************************************
   subroutine fmmap_create_dp(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   !! Creates a mapping to a `double precision` pointer `p`
   !********************************************************************************************
   double precision, pointer :: p(..)   ! on output, `p` points to the mapped file
   double precision, pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_dp


   !********************************************************************************************
   subroutine fmmap_create_complex(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   !! Creates a mapping to a `complex` pointer `p`
   !********************************************************************************************
   complex, pointer :: p(..)   ! on output, `p` points to the mapped file
   complex, pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_complex


   !********************************************************************************************
   subroutine fmmap_create_dc(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   !! Creates a mapping to a `double complex` pointer `p`
   !********************************************************************************************
   complex(kind=kind(0d0)), pointer :: p(..)   ! on output, `p` points to the mapped file
   complex(kind=kind(0d0)), pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_dc


   !********************************************************************************************
   subroutine fmmap_create_integer(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   !! Creates a mapping to a `integer` pointer `p`
   !********************************************************************************************
   integer, pointer :: p(..)   ! on output, `p` points to the mapped file
   integer, pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_integer


   !********************************************************************************************
   subroutine fmmap_create_di(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   !! Creates a mapping to a `integer(kind=fmmap_bigint)` pointer `p`
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
   !********************************************************************************************
   real, pointer :: p(..)   ! the pointer associated to the mapping to destroy

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_real
   

   !********************************************************************************************
   subroutine fmmap_destroy_dp(p)
   !********************************************************************************************
   !! Destroys a mapping to a `double precision` pointer
   !! (the file is unmapped and closed, and the pointer is nullified)
   !********************************************************************************************
   double precision, pointer :: p(..)   ! the pointer associated to the mapping to destroy

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_dp


   !********************************************************************************************
   subroutine fmmap_destroy_complex(p)
   !********************************************************************************************
   !! Destroys a mapping to a `complex` pointer
   !! (the file is unmapped and closed, and the pointer is nullified)
   !********************************************************************************************
   complex, pointer :: p(..)   ! the pointer associated to the mapping to destroy

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_complex
   

   !********************************************************************************************
   subroutine fmmap_destroy_dc(p)
   !********************************************************************************************
   !! Destroys a mapping to a `double complex` pointer
   !! (the file is unmapped and closed, and the pointer is nullified)
   !********************************************************************************************
   complex(kind=kind(0d0)), pointer :: p(..)   ! the pointer associated to the mapping to destroy

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_dc


   !********************************************************************************************
   subroutine fmmap_destroy_integer(p)
   !********************************************************************************************
   !! Destroys a mapping to a `integer` pointer
   !! (the file is unmapped and closed, and the pointer is nullified)
   !********************************************************************************************
   integer, pointer :: p(..)   ! the pointer associated to the mapping to destroy

   type(fmmap_t) :: x
   !********************************************************************************************
   call fmmap_table_pull(x,c_loc(p))
   call fmmap_destroy_cptr(x)
   !p => null()
   
   end subroutine fmmap_destroy_integer
   

   !********************************************************************************************
   subroutine fmmap_destroy_di(p)
   !********************************************************************************************
   !! Destroys a mapping to a `integer(kind=fmmap_bigint)` pointer
   !! (the file is unmapped and closed, and the pointer is nullified)
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
   !********************************************************************************************
   if (.not.allocated(table)) then
      table = [x]
   else
      i = 1
      do while (i <= size(table))
         if (.not.c_associated(table(i)%cptr)) exit
         i = i+1
      end do
      if (i <= size(table)) then ; table(i) = x
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
   !********************************************************************************************
   i = 1
   do while (i <= size(table))
      if (c_associated(table(i)%cptr,cptr)) exit
      i = i+1
   end do
   if (i >size(table)) then
      error stop "*** fmmap_destroy(): pointer not found in the internal table"
   end if
   x = table(i)
   table(i)%cptr = c_null_ptr
   
   if (i == size(table)) then
      i = size(table) - 1
      do while (i > 0)
         if (c_associated(table(i)%cptr)) exit
         i = i-1
      end do
      if (i == 0) then ; deallocate( table )
                  else ; table = table(1:i)
      end if
   end if
   end subroutine fmmap_table_pull
   

end module fmmap_m