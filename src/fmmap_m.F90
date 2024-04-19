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
   public :: fmmap_byte2elem, fmmap_elem2byte
   public :: fmmap_create, fmmap_destroy
   public :: fmmap_errmsg

   !> integer kind used for the sizes and and number of bytes or elements
   integer, parameter :: fmmap_size_t = c_size_t
         
   character(c_char) :: c
   integer, parameter :: bitsperbyte = storage_size(c)
   
   type, bind(C) :: fmmap_s   ! structure for the C interface
      type(c_ptr)            :: ptr = c_null_ptr
      integer(fmmap_size_t)  :: n
      integer(fmmap_size_t)  :: offset
      type(c_ptr)            :: filename
      integer(c_int)         :: filemode
#ifdef _WIN32
      type(c_ptr)            :: filedes = c_null_ptr
      type(c_ptr)            :: mapdes  = c_null_ptr
#else
! posix assumed
      integer(c_int)         :: filedes
#endif
      logical(c_bool)        :: cow
   end type
   
   type :: fmmap_t   ! public descriptor
      private
      ! nested type, so that type-bound procedures are possible
      ! (which is not possible with bind(C) types)
      type(fmmap_s), allocatable :: cx
   contains
      procedure, public :: cptr => fmmap_get_cptr
      procedure, public :: length => fmmap_get_length
   end type
      
   !> predefined values for the `filemode` argument
   integer, parameter :: FMMAP_SCRATCH = 1
   integer, parameter :: FMMAP_OLD     = 2
   integer, parameter :: FMMAP_NEW     = 3
   
   interface
   
      integer(c_int) function c_mmap_create( cx, cfilename ) BIND(C)
         import :: c_int, c_char, fmmap_s
         type(fmmap_s),                intent(inout) :: cx 
         character(kind=c_char,len=1), intent(in)    :: cfilename(*)
      end function c_mmap_create

      integer(c_int) function c_mmap_destroy( cx, wb ) BIND(C)
         import :: c_int, c_bool, fmmap_s
         type(fmmap_s),                intent(inout) :: cx 
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
   function fmmap_elem2byte(nelems,ss) result(nbytes)
   !********************************************************************************************
   !! converts a number of elements to a number of bytes  
   !! `ss` is typically obtained with the intrinsic function `ss = storage_size(var)`,
   !!  where `var` is any variable of the manipulated type+kind
   !********************************************************************************************
   integer(fmmap_size_t), intent(in) :: nelems   !! number of elements
   integer,               intent(in) :: ss       !! storage size (in bits) of 1 element
   integer(fmmap_size_t)             :: nbytes   !! number of bytes
   !********************************************************************************************
   if (modulo(ss,bitsperbyte) /= 0) then
      error stop "*** fmmap_elem2byte(): the storage size is not a multiple of the number of bits per byte"
   end if
   nbytes = nelems * (ss / bitsperbyte)
   end function fmmap_elem2byte
   
   
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
   
   integer(fmmap_size_t) :: bytesperelem
   !********************************************************************************************
   if (modulo(ss,bitsperbyte) /= 0) then
      error stop "*** fmmap_byte2elem(): the storage size is not a multiple of the number of bits per byte"
   end if
   bytesperelem = ss / bitsperbyte
   nelems = nbytes / bytesperelem
   if (nelems * bytesperelem /= nbytes) then
      error stop "*** fmmap_byte2elem(): the number of bytes does not form an integer number of elements"
   end if
   end function fmmap_byte2elem
   

   !********************************************************************************************
   subroutine fmmap_create_cptr(x,filemode,filename,length,pos,copyonwrite,stat)
   !********************************************************************************************
   !! Opens a file and creates a "generic" mapping to a C pointer.  
   !! The whole file is mapped.  
   !********************************************************************************************
   type(fmmap_t),         intent(out)           :: x
      !! descriptor of the mapped file
   integer,               intent(in)            :: filemode 
      !! FMMAP_SCRATCH, FMMAP_OLD, or FMMAP_NEW
   character(*),          intent(in)            :: filename 
      !! FMMAP_OLD or FMMAP_new: name of the file (with or without path)
      !! FMMAP_SCRATCH: name of the path; can be empty (="")
      !!   if empty:
      !!   - POSIX: is set to "." (current directory)
      !!   - WIN32: the Windows temporary path is inquired     
      !! - a processor dependent unique filename is generated and appended to the path
   integer(fmmap_size_t), intent(in),  optional :: length 
      !! size of the mapping in number of bytes
      !! required with FMMAP_SCRATCH or FMMAP_NEW
      !! - the size of the file is then pos+length-1  
      !! optional with FMMAP_OLD
      !! - if not present, the file is mapped from pos to the end of file
   integer(fmmap_size_t), intent(in),  optional :: pos
      !! Position in the file (in bytes) where the mapping starts
      !! default = 1
      !! Must be = 1 with FFMAP_SCRATCH and FMMAP_NEW
      !! Follows the Fortran 1-based indexing (pos=1 is the first byte of the file)
   logical,               intent(in),  optional :: copyonwrite
      !! if .true., all the changes made to the mapped file stay only in memory
      !! and are not written back to the file.
      !! .false. by default
   integer,               intent(out), optional :: stat
      !! return status; is 0 if no error occurred
   
   integer :: i, lu, stat___
   character(kind=c_char,len=:), allocatable :: c_filename
   character(*), parameter :: msgpre = "*** fmmap_create_cptr: "
   !********************************************************************************************
   
   if (file_storage_size /= bitsperbyte) then
      error stop msgpre//"the file storage unit is not a byte"
   end if
   
   if (present(length)) then
      if (length <= 0) then
         error stop msgpre//"length must be >0"
      end if
   else
      if (filemode /= FMMAP_OLD) then
         error stop msgpre//"length must be present with FMMAP_SCRATCH and FMMAP_NEW"
      end if
   end if
   
   if (present(pos)) then
      if (filemode /= FMMAP_OLD .and. pos /= 1) then
         error stop msgpre//"pos must be 1 with FMMAP_SCRATCH and FMMAP_NEW"
      end if
      if (pos <= 0) then
         error stop msgpre//"pos must be >0"
      end if      
   end if
   
   if (filemode /= FMMAP_SCRATCH .and. trim(filename) == "") then
      error stop msgpre//"filename must not be empty with FMMAP_NEW and FMMAP_OLD"
   end if
   
   allocate( x% cx )
   stat___ = 0
   
   BODY: BLOCK
   ASSOCIATE( cx => x% cx )
   
   cx%filemode = filemode
   cx%cow      = .false. ; if (present(copyonwrite)) cx%cow = copyonwrite  
   cx% offset  = 0       ; if (present(pos)) cx%offset = pos - 1
   
   if (filemode == FMMAP_SCRATCH) then
      cx%n = length
   else if (filemode == FMMAP_NEW) then
      cx%n = length
      open(newunit=lu,file=trim(filename),status='new' &
          ,form='unformatted',access='stream',iostat=stat___)
      if (stat___ /= 0) then
         stat___ = 3
         exit BODY
      end if
      write(lu,pos=cx%n,iostat=stat___) c_null_char
      if (stat___ /= 0) then
         stat___ = 3
         exit BODY
      end if
      close(lu,iostat=stat___)
      if (stat___ /= 0) then
         stat___ = 3
         exit BODY
      end if
   else if (filemode == FMMAP_OLD) then
      if (present(length)) then
         cx%n = length
      else
         inquire(file=trim(filename), size=cx%n)
         if (cx%n < 0) then
            stat___ = 2
            exit BODY
         end if
      end if
   else
      error stop msgpre//"wrong filemode"
   end if
   
   c_filename = trim(filename) // c_null_char
   stat___ = c_mmap_create( cx, c_filename )
   if (stat___ /= 0) exit BODY
   
   END ASSOCIATE
   END BLOCK BODY
   
   
   if (present(stat)) then
      stat = stat___
      if (stat > 0) deallocate( x% cx )
   else
      if (stat___ > 0) error stop "*** fmmap_create_cptr: "//fmmap_errmsg(stat___)
   end if
                  
   end subroutine fmmap_create_cptr


   !********************************************************************************************
   function fmmap_get_cptr(x)
   !********************************************************************************************
   !! Returns the C pointer of a mapped file  
   !********************************************************************************************
   class(fmmap_t), intent(in) :: x
      !! descriptor of the mapped file
   type(c_ptr)                :: fmmap_get_cptr
   !********************************************************************************************
   fmmap_get_cptr = c_null_ptr
   if (allocated(x% cx)) fmmap_get_cptr = x% cx % ptr
   end function fmmap_get_cptr


   !********************************************************************************************
   function fmmap_get_length(x)
   !********************************************************************************************
   !! Returns the number of bytes that are mapped  
   !********************************************************************************************
   class(fmmap_t), intent(in) :: x
      !! descriptor of the mapped file
   integer(fmmap_size_t)      :: fmmap_get_length
   !********************************************************************************************
   fmmap_get_length = -1
   if (allocated(x% cx)) fmmap_get_length = x% cx % n
   end function fmmap_get_length


   !********************************************************************************************
   subroutine fmmap_destroy_cptr(x,writeback,stat)
   !********************************************************************************************
   !! Destroys a generic mapping
   !********************************************************************************************
   type(fmmap_t), intent(inout)           :: x 
      !! descriptor of the mapped file
   logical,     intent(in)   , optional :: writeback  
      !! if .true., the changes in memory in the copyonwrite mode are written back to the file
   integer,               intent(out),  optional :: stat
      !! return status, is 0 if no error occurred
   
   integer :: i, stat___
   logical(c_bool) :: wb
   character(*), parameter :: msgpre = "*** fmmap_destroy_cptr: "
   !********************************************************************************************
   
   stat___ = 0
   
   BODY: BLOCK
   ASSOCIATE( cx => x% cx )

   if (.not.allocated(x% cx)) then
      stat___ = 10
      exit BODY
   end if
         
   wb = (cx% filemode == FMMAP_NEW .and. cx% cow); if (present(writeback)) wb = writeback
   if (wb .and. .not. cx% cow) then
      error stop msgpre//"writeback must be .false. if Copy-on-Write is not used"
   end if
   if (wb .and. cx% filemode == FMMAP_SCRATCH) then
      error stop msgpre//"writeback must be .false. with FMMAP_SCRATCH"
   end if
   if (.not.wb .and. cx% filemode == FMMAP_NEW .and. cx% cow) then
      error stop msgpre//"writeback must be .true. with FMMAP_NEW and Copy-on-Write"
   end if
      
   stat___ = c_mmap_destroy( cx, wb )
   if (stat___ /= 0) exit BODY
   cx% ptr = c_null_ptr
   
   END ASSOCIATE
   END BLOCK BODY
   
   
   if (present(stat)) then
      stat = stat___
      if (stat > 0 .and. allocated(x% cx)) deallocate( x% cx )
   else
      if (stat___ > 0) error stop msgpre//fmmap_errmsg(stat___)
   end if   
   
   end subroutine fmmap_destroy_cptr

end module fmmap_m
