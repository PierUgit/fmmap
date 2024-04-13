!***********************************************************************************************
module fmmap_m
!***********************************************************************************************
use, intrinsic :: iso_c_binding
use, intrinsic :: iso_fortran_env
implicit none

   private
   public :: fmmap_size_t
   public :: FMMAP_SCRATCH, FMMAP_OLD, FMMAP_NEW
   public :: fmmap_nbytes, fmmap_nelems
   public :: fmmap_create, fmmap_destroy

   !> integer kind used for the sizes and and number of bytes or elements
   integer, parameter :: fmmap_size_t = c_long_long
   
   !> kinds for the intrinsic kind versions
   integer, parameter :: rk1 = real32, rk2 = real64, ik1 = int32, ik2 = int64
      
   character(c_char) :: c
   integer, parameter :: bitsperbyte = storage_size(c)
   
   type, bind(C) :: fmmap_t   ! descriptor
      private
      type(c_ptr), public    :: cptr = c_null_ptr
      integer(fmmap_size_t)  :: cn
      type(c_ptr)            :: cfilename
      integer(c_int)         :: cfilemode
#ifdef _WIN32
      type(c_ptr)            :: cfiledes = c_null_ptr
      type(c_ptr)            :: cmapdes  = c_null_ptr
#else
! posix assumed
      integer(c_int)         :: cfiledes
#endif
      logical(c_bool)        :: ccow
      logical(c_bool)        :: used = .true.
   end type
   
   type(fmmap_t), allocatable :: table(:)
   
   !> predefined values for the `filemode` argument
   integer, parameter :: FMMAP_SCRATCH = 1
   integer, parameter :: FMMAP_OLD     = 2
   integer, parameter :: FMMAP_NEW     = 3
   
   character(*), parameter :: msg0="*** fmmap_create(): "
   character(*), parameter :: msg1="the rank of must be 1 to 7"
   character(*), parameter :: msg2="wrong size of shape"
   character(*), parameter :: msg3="requested pointer shape incompatible with file size"
   character(*), parameter :: msg4="only the last element of shape can be <0"
   character(*), parameter :: msg5="the last element of shape can be <0 only with FMMAP_OLD"
   
   interface
   
      integer(c_int) function c_mmap_create( x, cfilename ) BIND(C)
         import :: c_int, c_char, fmmap_t
         type(fmmap_t),                intent(inout) :: x 
         character(kind=c_char,len=1), intent(in)    :: cfilename(*)
      end function c_mmap_create

      integer(c_int) function c_mmap_destroy( x, wb ) BIND(C)
         import :: c_int, c_bool, fmmap_t
         type(fmmap_t),                intent(inout) :: x 
         logical(c_bool),              value         :: wb
      end function c_mmap_destroy
      
      integer(c_int) function c_mmap_writeback( x ) BIND(C)
         import :: c_int, c_char, fmmap_t
         type(fmmap_t),                intent(inout) :: x 
      end function c_mmap_writeback

   end interface
   
   !> Generic routine name that creates either a generic mapping to a C pointer or a mapping
   !> to array pointers of some selected intrinsic types/kinds.
   !> This routine is not thread safe !
   interface fmmap_create
      module procedure fmmap_create_cptr
      module procedure fmmap_create_rk1, fmmap_create_rk2
      module procedure fmmap_create_ck1, fmmap_create_ck2
      module procedure fmmap_create_ik1, fmmap_create_ik2
      module procedure fmmap_create_cchar
   end interface
   
   !> Generic routine name that destroys an existing mapping
   !> This routine is not thread safe !
   interface fmmap_destroy
      module procedure fmmap_destroy_cptr
      module procedure fmmap_destroy_rk1, fmmap_destroy_rk2
      module procedure fmmap_destroy_ck1, fmmap_destroy_ck2
      module procedure fmmap_destroy_ik1, fmmap_destroy_ik2
      module procedure fmmap_destroy_cchar
   end interface
   
contains
   
   !********************************************************************************************
   function fmmap_nbytes(n,ss)
   !********************************************************************************************
   !! converts a number of elements to a number of bytes
   !********************************************************************************************
   integer(fmmap_size_t), intent(in) :: n   !! number of elements
   integer,               intent(in) :: ss  !! storage size (in bits) of 1 element
   integer(fmmap_size_t) :: fmmap_nbytes    !! number of bytes
   !********************************************************************************************
   fmmap_nbytes = n * (ss / bitsperbyte)
   end function fmmap_nbytes
   
   !********************************************************************************************
   function fmmap_nelems(nbytes,ss)
   !********************************************************************************************
   !! converts a number of bytes into a number of elements
   !********************************************************************************************
   integer(fmmap_size_t), intent(in) :: nbytes !! number of nbytes
   integer,               intent(in) :: ss     !! storage size (in bits) of 1 element
   integer(fmmap_size_t) :: fmmap_nelems   !! number of elements
   
   integer(fmmap_size_t) :: bytesperelem
   !********************************************************************************************
   bytesperelem = ss / bitsperbyte
   fmmap_nelems = nbytes / bytesperelem
   if (fmmap_nelems * bytesperelem /= nbytes) then
      error stop "*** fmmap_nelems(): the number of bytes does not form an integer number of elements"
   end if
   end function fmmap_nelems
   

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
   
   type(fmmap_t) :: x
   integer :: i, lu, stat
   character(:), allocatable :: filename___
   character(kind=c_char,len=:), allocatable :: c_filename
   character(128) :: msg
   !********************************************************************************************

   if (file_storage_size /= bitsperbyte) then
      error stop "*** fmmap_init: the file storage unit is not a byte"
   end if
   
   x%cfilemode = filemode
   x%ccow      = .false. ; if (present(copyonwrite)) x%ccow = copyonwrite
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
   
   cptr = x%cptr
   call fmmap_table_push(x)
                  
   end subroutine fmmap_create_cptr


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
   
   type(fmmap_t) :: x 
   integer :: i, stat
   logical(c_bool) :: wb
   character(128) :: msg
   !********************************************************************************************
   
   if (.not.c_associated(cptr)) then
      error stop "*** fmmap_destroy_cptr: attempt to free a non associated pointer"
   end if
   
   call fmmap_table_pull( x, cptr )
   
   wb = (x% cfilemode == FMMAP_NEW .and. x% ccow); if (present(writeback)) wb = writeback
   if (wb .and. .not. x% ccow) then
      error stop "*** fmmap_destroy_cptr: writeback must be .false. if Copy-on-Write is not used"
   end if
   if (wb .and. x% cfilemode == FMMAP_SCRATCH) then
      error stop "*** fmmap_destroy_cptr: writeback must be .false. with FMMAP_SCRATCH"
   end if
   if (.not.wb .and. x% cfilemode == FMMAP_NEW .and. x% ccow) then
      error stop "*** fmmap_destroy_cptr: writeback must be .true. with FMMAP_NEW and Copy-on-Write"
   end if
      
   stat = c_mmap_destroy( x, wb )
   if (stat /= 0) then
      write(msg,*) "*** fmmap_destroy_cptr: error code ", stat
      error stop trim(msg)
   end if
   cptr = c_null_ptr
   
   end subroutine fmmap_destroy_cptr


   !********************************************************************************************
   subroutine fmmap_create_rk1(p,shape,filemode,filename,lower,copyonwrite)
   !********************************************************************************************
   !! Opens a file and creates a mapping to a `real(real32)` pointer `p`  
   !! The whole file is mapped.  
   !! This routine is not thread safe.
   !********************************************************************************************
   real(rk1), pointer :: p(..)   !! on output, `p` points to the mapped file
   real(rk1), pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_rk1


   !********************************************************************************************
   subroutine fmmap_create_rk2(p,shape,filemode,filename,lower,copyonwrite)
   !********************************************************************************************
   !! Opens a file and creates a mapping to a `real(real64)` pointer `p`  
   !! The whole file is mapped.  
   !! This routine is not thread safe.
   !********************************************************************************************
   real(rk2), pointer :: p(..)   !! on output, `p` points to the mapped file
   real(rk2), pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_rk2


   !********************************************************************************************
   subroutine fmmap_create_ck1(p,shape,filemode,filename,lower,copyonwrite)
   !********************************************************************************************
   !! Opens a file and creates a mapping to a `complex(real32)` pointer `p`  
   !! The whole file is mapped.  
   !! This routine is not thread safe.
   !********************************************************************************************
   complex(rk1), pointer :: p(..)   !! on output, `p` points to the mapped file
   complex(rk1), pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_ck1


   !********************************************************************************************
   subroutine fmmap_create_ck2(p,shape,filemode,filename,lower,copyonwrite)
   !********************************************************************************************
   !! Opens a file and creates a mapping to a `complex(real64)` pointer `p`  
   !! The whole file is mapped.  
   !! This routine is not thread safe.
   !********************************************************************************************
   complex(rk2), pointer :: p(..)   !! on output, `p` points to the mapped file
   complex(rk2), pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_ck2


   !********************************************************************************************
   subroutine fmmap_create_ik1(p,shape,filemode,filename,lower,copyonwrite)
   !********************************************************************************************
   !! Opens a file and creates a mapping to an `integer(int32)` pointer `p`  
   !! The whole file is mapped.  
   !! This routine is not thread safe.
   !********************************************************************************************
   integer(ik1), pointer :: p(..)   !! on output, `p` points to the mapped file
   integer(ik1), pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_ik1


   !********************************************************************************************
   subroutine fmmap_create_ik2(p,shape,filemode,filename,lower,copyonwrite)
   !********************************************************************************************
   !! Opens a file and creates a mapping to an `integer(int64)` pointer `p`  
   !! The whole file is mapped.  
   !! This routine is not thread safe.
   !********************************************************************************************
   integer(ik2), pointer :: p(..)   !! on output, `p` points to the mapped file
   integer(ik2), pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_ik2


   !********************************************************************************************
   subroutine fmmap_create_cchar(p,shape,filemode,filename,lower,copyonwrite)
   !********************************************************************************************
   !! Opens a file and creates a mapping to a `character(kind=c_char)` pointer `p`  
   !! The whole file is mapped.  
   !! This routine is not thread safe.
   !********************************************************************************************
   character(kind=c_char), pointer :: p(..)   !! on output, `p` points to the mapped file
   character(kind=c_char), pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_cchar


   !********************************************************************************************
   subroutine fmmap_destroy_rk1(p,writeback)
   !********************************************************************************************
   !! Destroys a mapping to a `real(real32)` pointer and closes the file.  
   !! This routine is not thread safe.
   !********************************************************************************************
   real(rk1), pointer :: p(..)   
      !! pointer to the mapped file; is nullified on output

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_rk1
   

   !********************************************************************************************
   subroutine fmmap_destroy_rk2(p,writeback)
   !********************************************************************************************
   !! Destroys a mapping to a `real(real64)` pointer and closes the file.  
   !! This routine is not thread safe.
   !********************************************************************************************
   real(rk2), pointer :: p(..)
      !! pointer to the mapped file; is nullified on output

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_rk2


   !********************************************************************************************
   subroutine fmmap_destroy_ck1(p,writeback)
   !********************************************************************************************
   !! Destroys a mapping to a `complex(real32)` pointer and closes the file.  
   !! This routine is not thread safe.
   !********************************************************************************************
   complex(rk1), pointer :: p(..)   
      !! pointer to the mapped file; is nullified on output

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_ck1
   

   !********************************************************************************************
   subroutine fmmap_destroy_ck2(p,writeback)
   !********************************************************************************************
   !! Destroys a mapping to a `complex(real64)` pointer and closes the file.  
   !! This routine is not thread safe.
   !********************************************************************************************
   complex(rk2), pointer :: p(..)    
      !! pointer to the mapped file; is nullified on output

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_ck2


   !********************************************************************************************
   subroutine fmmap_destroy_ik1(p,writeback)
   !********************************************************************************************
   !! Destroys a mapping to an `integer(int32)` pointer and closes the file.  
   !! This routine is not thread safe.
   !********************************************************************************************
   integer(ik1), pointer :: p(..)   
      !! pointer to the mapped file; is nullified on output

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_ik1
   

   !********************************************************************************************
   subroutine fmmap_destroy_ik2(p,writeback)
   !********************************************************************************************
   !! Destroys a mapping to an `integer(int64)` pointer and closes the file.  
   !! This routine is not thread safe.
   !********************************************************************************************
   integer(ik2), pointer :: p(..)   
      !! pointer to the mapped file; is nullified on output

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_ik2
   
   
   !********************************************************************************************
   subroutine fmmap_destroy_cchar(p,writeback)
   !********************************************************************************************
   !! Destroys a mapping to a `character(kind=c_char)` pointer and closes the file.  
   !! This routine is not thread safe.
   !********************************************************************************************
   character(kind=c_char), pointer :: p(..)   
      !! pointer to the mapped file; is nullified on output

   include "fmmap_destroy.fi"
   
   end subroutine fmmap_destroy_cchar


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
   type(fmmap_t), intent(out) :: x   !! descriptor
   type(c_ptr),   intent(in)  :: cptr   !! pointer to search in the table
   
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
