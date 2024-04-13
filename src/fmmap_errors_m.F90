!***********************************************************************************************
module fmmap_errors_m
!***********************************************************************************************
implicit none

   private
   public :: fmmap_errmsg

contains
   !********************************************************************************************
   function fmmap_errmsg(stat) result(msg)
   !********************************************************************************************
   !! Returns the error messages corresponding to an error code
   !********************************************************************************************
   integer, intent(in) :: stat
   character(len=:), allocatable :: msg
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
   
   end function
   
end module