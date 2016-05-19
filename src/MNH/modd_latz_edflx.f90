!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/11/23 17:28:26
!-----------------------------------------------------------------
!     ######################## 
      MODULE MODD_LATZ_EDFLX
!     ########################
!
!!****  *MODD_LATZ_EDFLX$n* - declaration of the control parameters for
!!                           eddy flux parametrization
!!
!!
!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
LOGICAL, SAVE :: LUV_FLX
REAL, SAVE    :: XUV_FLX1
REAL, SAVE    :: XUV_FLX2
LOGICAL, SAVE :: LTH_FLX
REAL, SAVE    :: XTH_FLX
!
END MODULE MODD_LATZ_EDFLX
