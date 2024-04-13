!***********************************************************************************************
module fmmap_m
!***********************************************************************************************
use, intrinsic :: iso_c_binding
use, intrinsic :: iso_fortran_env
use fmmap_errors_m
implicit none

   private
   public :: fmmap_size_t, fmmap_t
   public :: FMMAP_SCRATCH, FMMAP_OLD, FMMAP_NEW
   public :: fmmap_nbytes, fmmap_nelems
   public :: fmmap_create, fmmap_destroy
   public :: fmmap_errmsg

   !> integer kind used for the sizes and and number of bytes or elements
   integer, parameter :: fmmap_size_t = c_long_long
         
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
   end type
      
   !> predefined values for the `filemode` argument
   integer, parameter :: FMMAP_SCRATCH = 1
   integer, parameter :: FMMAP_OLD     = 2
   integer, parameter :: FMMAP_NEW     = 3
   
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
      
   end interface
   
   !> Generic routine name that creates a generic mapping to a C pointer
   interface fmmap_create
      module procedure fmmap_create_cptr
   end interface
   
   !> Generic routine name that destroys an existing mapping
   interface fmmap_destroy
      module procedure fmmap_destroy_cptr
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
   subroutine fmmap_create_cptr(x,nbytes,filemode,filename,copyonwrite,stat)
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
   
   integer :: i, lu, stat___
   character(:), allocatable :: filename___
   character(kind=c_char,len=:), allocatable :: c_filename
   character(128) :: msg
   !********************************************************************************************
   
   stat___ = 0
   
   BODY: BLOCK
   
   if (file_storage_size /= bitsperbyte) then
      error stop "*** fmmap_init: the file storage unit is not a byte"
   end if

   x%cfilemode = filemode
   x%ccow      = .false. ; if (present(copyonwrite)) x%ccow = copyonwrite
   
   if (filemode == FMMAP_SCRATCH) then
      x%cn = nbytes
      filename___ = "" ; if (present(filename)) filename___ = trim(filename)
   else if (filemode == FMMAP_OLD) then
      filename___ = filename
      inquire(file=trim(filename___), size=x%cn)
      if (x%cn < 0) then
         stat___ = 2
         exit BODY
      end if
      nbytes = x%cn
   else if (filemode == FMMAP_NEW) then
      x%cn = nbytes
      filename___ = filename
      open(newunit=lu,file=filename___,status='new' &
          ,form='unformatted',access='stream',iostat=stat___)
      if (stat___ /= 0) then
         stat___ = 3
         exit BODY
      end if
      write(lu,pos=x%cn,iostat=stat___) c_null_char
      if (stat___ /= 0) then
         stat___ = 3
         exit BODY
      end if
      close(lu,iostat=stat___)
      if (stat___ /= 0) then
         stat___ = 3
         exit BODY
      end if
   else
      error stop "*** fmmap_create_cptr: wrong filemode"
   end if
   
   c_filename = filename___ // c_null_char
   stat___ = c_mmap_create( x, c_filename )
   if (stat___ /= 0) exit BODY
   
   END BLOCK BODY
   
   
   if (present(stat)) then
      stat = stat___
   else
      if (stat___ > 0) error stop "*** fmmap_create_cptr: "//fmmap_errmsg(stat___)
   end if
                  
   end subroutine fmmap_create_cptr


   !********************************************************************************************
   subroutine fmmap_destroy_cptr(x,writeback,stat)
   !********************************************************************************************
   !! Destroys a generic mapping
   !********************************************************************************************
   type(fmmap_t), intent(inout)           :: x 
      !! C pointer to the mapped file; is nullified on output
   logical,     intent(in)   , optional :: writeback  
      !! if .true., the changes in memory in the copyonwrite mode are written back to the file
   integer,               intent(out),  optional :: stat
      !! return status, is 0 if no error occurred
   
   integer :: i, stat___
   logical(c_bool) :: wb
   character(128) :: msg
   !********************************************************************************************
   
   stat___ = 0
   
   BODY: BLOCK
   
   if (.not.c_associated(x%cptr)) then
      stat___ = 10
      exit BODY
   end if
      
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
      
   stat___ = c_mmap_destroy( x, wb )
   if (stat___ /= 0) exit BODY
   x% cptr = c_null_ptr
   
   END BLOCK BODY
   
   
   if (present(stat)) then
      stat = stat___
   else
      if (stat___ > 0) error stop "*** fmmap_destroy_cptr: "//fmmap_errmsg(stat___)
   end if   
   
   end subroutine fmmap_destroy_cptr

end module fmmap_m
