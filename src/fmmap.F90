!***********************************************************************************************
! Memory Mapped file in Fortran
! Works both on Posix systems and Windows
! Author: Pierre Hugonnet
! Licence: GPL v3
! https://github.com/PierUgit/fmmap
!***********************************************************************************************
module fmmap
!***********************************************************************************************
use, intrinsic :: iso_c_binding
use, intrinsic :: iso_fortran_env
implicit none

   private
   public :: fmmap_t
   public :: FMMAP_SCRATCH, FMMAP_OLD, FMMAP_NEW, FMMAP_NOFILE
   public :: fmmap_sizeof, fmmap_b2e, fmmap_e2b
   public :: fmmap_errmsg

   character(c_char) :: c
   integer, parameter :: bitsperbyte = storage_size(c)

   type, bind(C) :: fmmap_s   ! structure for the C interface
      type(c_ptr)            :: ptr = c_null_ptr
      integer(c_size_t)      :: n
      type(c_ptr)            :: filename
      integer(c_int)         :: filestatus
#ifdef _WIN32
      type(c_ptr)            :: filedes = c_null_ptr
      type(c_ptr)            :: mapdes  = c_null_ptr
#else
! posix assumed
      integer(c_int)         :: filedes
#endif
      logical(c_bool)        :: private = .false.
   end type

   type :: fmmap_t   ! public descriptor
      private
      ! nested type, so that type-bound procedures are possible
      ! (which is not possible with bind(C) types)
      type(fmmap_s), allocatable :: cx
   contains
      private
      procedure         :: fmmap_t_create,     fmmap_t_create_bytes
      procedure         :: fmmap_t_get_length, fmmap_t_get_length_bytes
      generic,   public :: create  => fmmap_t_create, fmmap_t_create_bytes
      procedure, public :: cptr    => fmmap_t_get_cptr
      generic,   public :: length  => fmmap_t_get_length, fmmap_t_get_length_bytes
      procedure, public :: destroy => fmmap_t_destroy
      final             ::            fmmap_t_final
   end type

   !> predefined values for the `filestatus` argument
#define FMMAP_LANG_F
#include "constants.h"


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

   interface operator(.streq.)
      module procedure strcomp
   end interface

contains

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

   integer(c_size_t) :: length___
   character(*), parameter :: msgpre = "*** fmmap_create: "
   !********************************************************************************************

   if (file_storage_size /= bitsperbyte) then
      error stop msgpre//"the file storage unit is not a byte"
   end if
   if (modulo(storage_size(mold),bitsperbyte) /= 0) then
      error stop msgpre//"the storage size is not a multiple of the number of bits per byte"
   end if

   if (filestatus /= FMMAP_OLD) length___ = length * storage_size(mold) / bitsperbyte
   call fmmap_t_create_bytes(x,filestatus,filename,length___,private,stat)
   if (filestatus == FMMAP_OLD) length = length___ * bitsperbyte / storage_size(mold)

   end subroutine fmmap_t_create


   !********************************************************************************************
   !> @brief
   !! Returns the C pointer of a mapped file
   !!
   !! This is a type-bound procedure, which means that the invocation is actually:
   !! ```
   !! mycptr = x%get_cptr()
   !! ```
   !********************************************************************************************
   function fmmap_t_get_cptr(x) result(cptr)
   !********************************************************************************************
   class(fmmap_t), intent(in) :: x
      !< descriptor of the mapped file
   type(c_ptr)                :: cptr
      !< the output C pointer
   !********************************************************************************************
   cptr = c_null_ptr
   if (allocated(x% cx)) cptr = x% cx % ptr
   end function fmmap_t_get_cptr


   !********************************************************************************************
   !> @brief
   !! Returns the length of a mapped file
   !!
   !! This is a type-bound procedure, which means that the invocation is actually:
   !! ```
   !! mylength = x%get_length( <all arguments but x> )
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
   
   character(*), parameter :: msgpre = "*** fmmap_t_get_length_elts: "
   !********************************************************************************************
   if (modulo(storage_size(mold),bitsperbyte) /= 0) then
      error stop msgpre//"the storage size is not a multiple of the number of bits per byte"
   end if
   
   length = 0
   if (allocated(x% cx)) length = x% cx % n * storage_size(mold) / bitsperbyte
   end function fmmap_t_get_length

   
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

   integer :: stat___
   logical(c_bool) :: wb
   character(*), parameter :: msgpre = "*** fmmap_destroy: "
   character(:), allocatable :: msg
   !********************************************************************************************

   stat___ = 0

   BODY: BLOCK
   ASSOCIATE( cx => x% cx )

   if (.not.allocated(x% cx)) then
      stat___ = 10
      exit BODY
   end if

   wb = (cx% filestatus == FMMAP_NEW .and. cx% private)
   if (present(writeback)) wb = writeback
   if (wb) then
      if (.not. cx% private) then
         error stop msgpre//"writeback must be .false. if private was .false."
      end if
      if (cx% filestatus == FMMAP_SCRATCH .or. cx% filestatus == FMMAP_NOFILE) then
         error stop msgpre//"writeback must be .false. with FMMAP_SCRATCH or FMMAP_NOFILE"
      end if
   else
      if (cx% filestatus == FMMAP_NEW .and. cx% private) then
         error stop msgpre//"writeback must be .true. with FMMAP_NEW if private was true"
      end if
   end if

   stat___ = c_mmap_destroy( cx, wb )
   if (stat___ /= 0) exit BODY
   cx% ptr = c_null_ptr

   END ASSOCIATE
   END BLOCK BODY


   if (allocated(x% cx)) deallocate( x% cx )
   if (present(stat)) then
      stat = stat___
   else if (stat___ > 0) then
      msg = msgpre//fmmap_errmsg(stat___)
      error stop msg
   end if

   end subroutine fmmap_t_destroy


   !********************************************************************************************
   !> @brief
   !! Returns the error messages corresponding to an error code
   !********************************************************************************************
   function fmmap_errmsg(stat) result(msg)
   !********************************************************************************************
   integer, intent(in) :: stat            !< error code
   character(len=:), allocatable :: msg   !< corresponding error message
   !********************************************************************************************

   select case (stat)
   case (  0); msg = "No error!!!"
   case (  1); msg = "The Fortran file storage unit is not a byte; fmmap module not usable"
   case (  2); msg = "Unable to inquire the file size (F)"
   case (  3); msg = "Unable to create the NEW file (F)"
   case ( 10); msg = "Attempt to free a non associated C pointer (F)"
   case (101); msg = "Unable to create the SCRATCH file (C)"
   case (105); msg = "Unable to reopen the SCRATCH file (C)"
   case (111); msg = "Unable to open the file (C)"
   case (121); msg = "Unable to map the file (C)"
   case (122); msg = "Unable to mapview the file (C)"
   case (131); msg = "Unable to close the file (C)"
   case (201); msg = "Unable to create a new mapping for writeback (C)"
   case (202); msg = "Unable to destroy the new mapping for writeback (C)"
   case (211); msg = "Unable to flush/sync the mapping (C)"
   case (221); msg = "Unable to unmap (C)"
   case (222); msg = "Unable to unmapview (C)"

   case default; msg = "Invalid error code!!!"
   end select

   end function fmmap_errmsg

   
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
   !********************************************************************************************
   nbytes = storage_size( object )
   if (modulo(nbytes,int(bitsperbyte,kind=c_size_t)) /= 0) then
      error stop "*** fmmap_sizeof(): the storage size of the object is not a multiple of the number of bits per byte"
   end if
   nbytes = nbytes / bitsperbyte
   end function fmmap_sizeof
   
   
   !********************************************************************************************
   impure elemental subroutine fmmap_t_final(x)
   !********************************************************************************************
   !! Destroys a generic mapping
   !********************************************************************************************
   type(fmmap_t), intent(inout)          :: x
      !< descriptor of the mapped file

   integer :: stat
   character(*), parameter :: msgpre = "*** fmmap_final: "
   character(:), allocatable :: msg
   !********************************************************************************************

   if (allocated(x% cx)) then
      call fmmap_t_destroy(x,stat=stat)
      msg = msgpre//fmmap_errmsg(stat)
      if (stat > 0) error stop msg
   end if

   end subroutine fmmap_t_final


   !********************************************************************************************
   function upcase(str)
   !********************************************************************************************
   character(*), intent(in) :: str
   character(len(str)) :: upcase

   character(*), parameter :: LC = "abcdefghijklmnopqrstuvwxz"
   character(*), parameter :: UC = "ABCDEFGHIJKLMNOPQRSTUVWXZ"
   integer :: i, j
   !********************************************************************************************
   do i = 1, len(str)
      j = index( LC, str(i:i) )
      upcase(i:i) = merge( UC(j:j), str(i:i), j > 0)
   end do
   end function upcase


   !********************************************************************************************
   logical pure function strcomp(s,t)
   !********************************************************************************************
   character(*), intent(in) :: s, t
   !********************************************************************************************
   strcomp = (s == t) .and. (len(s) == len(t))
   end function strcomp


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
   !********************************************************************************************
   if (modulo(ss,bitsperbyte) /= 0) then
      error stop "*** fmmap_e2b(): the storage size is not a multiple of the number of bits per byte"
   end if
   nbytes = nelems * (ss / bitsperbyte)
   end function fmmap_e2b


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

   integer(c_size_t) :: bytesperelem
   !********************************************************************************************
   if (modulo(ss,bitsperbyte) /= 0) then
      error stop "*** fmmap_b2e(): the storage size is not a multiple of the number of bits per byte"
   end if
   bytesperelem = ss / bitsperbyte
   nelems = nbytes / bytesperelem
   if (nelems * bytesperelem /= nbytes) then
      error stop "*** fmmap_b2e(): the number of bytes does not form an integer number of elements"
   end if
   end function fmmap_b2e

   !********************************************************************************************
   !> @brief
   !! DEPRECATED : always use fmmap_t_create()
   !!
   !! Same as fmmap_t_create(), with `mold` occupying 1 byte  
   !********************************************************************************************
   subroutine fmmap_t_create_bytes(x,filestatus,filename,length,private,stat)
   !********************************************************************************************
   class(fmmap_t),        intent(out)           :: x
   integer,               intent(in)            :: filestatus
   character(*),          intent(in)            :: filename
   integer(c_size_t)                            :: length
   logical,               intent(in),  optional :: private
   integer,               intent(out), optional :: stat

   integer :: ss, lu, stat___
   character(kind=c_char,len=:), allocatable :: c_filename
   character(*), parameter :: msgpre = "*** fmmap_create_with_bytes: "
   character(:), allocatable :: msg
   !********************************************************************************************

   if (file_storage_size /= bitsperbyte) then
      error stop msgpre//"the file storage unit is not a byte"
   end if

   ss = bitsperbyte

   allocate( x% cx )
   stat___ = 0

   BODY: BLOCK
   ASSOCIATE( cx => x% cx )

   cx%filestatus = filestatus
   cx% private = (filestatus == FMMAP_NOFILE)
   if (present(private)) cx%private = private

   if (filestatus == FMMAP_SCRATCH) then
      if (length < 0) then
         error stop msgpre//"length must be >=0 with FMMAP_SCRATCH"
      end if
      cx%n = fmmap_e2b( length, ss )
   else if (filestatus == FMMAP_NOFILE) then
      if (len(filename) /= 0) then
         error stop msgpre//"filename must be empty with FMMAP_NOFILE"
      end if
      if (length < 0) then
         error stop msgpre//"length must be >=0 with FMMAP_NOFILE"
      end if
      if (.not.cx%private) then
         error stop msgpre//"private must be .true. with FMMAP_NOFILE"
      end if
      cx%n = fmmap_e2b( length, ss )
   else if (filestatus == FMMAP_NEW) then
      if (len_trim(filename) == 0) then
         error stop msgpre//"filename must not be blank with FMMAP_NEW"
      end if
      if (length < 0) then
         error stop msgpre//"length must be >=0 with FMMAP_NEW"
      end if
      cx%n = fmmap_e2b( length, ss )
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
   else if (filestatus == FMMAP_OLD) then
      if (len_trim(filename) == 0) then
         error stop msgpre//"filename must not be blank with FMMAP_OLD"
      end if
      inquire(file=trim(filename), size=cx%n)
      if (cx%n < 0) then
         stat___ = 2
         exit BODY
      end if
      length = fmmap_b2e( cx%n, ss )
   else
      error stop msgpre//"wrong filestatus"
   end if

   c_filename = trim(filename) // c_null_char
   stat___ = c_mmap_create( cx, c_filename )
   if (stat___ /= 0) exit BODY

   END ASSOCIATE
   END BLOCK BODY


   if (present(stat)) then
      stat = stat___
      if (stat > 0) deallocate( x% cx )
   else if (stat___ > 0) then
      msg = msgpre//fmmap_errmsg(stat___)
      error stop msg
   end if

   end subroutine fmmap_t_create_bytes


   !********************************************************************************************
   !> @brief
   !! DEPRECATED : always use fmmap_t_get_length()
   !!
   !! same as fmmap_t_get_length(), with `mold` occupying 1 byte  
   !********************************************************************************************
   function fmmap_t_get_length_bytes(x) result(length)
   !********************************************************************************************
   class(fmmap_t), intent(in)            :: x
   integer(c_size_t)                     :: length
   !********************************************************************************************
   length = 0
   if (allocated(x% cx)) length = x% cx % n
   end function fmmap_t_get_length_bytes


end module fmmap
