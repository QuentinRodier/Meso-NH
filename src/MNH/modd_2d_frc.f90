!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/11/23 17:28:26
!-----------------------------------------------------------------
!     ######################## 
      MODULE MODD_2D_FRC 
!     ########################
!
!!****  *MODD_2D_FRC* - declaration of the control parameters for
!!                           2d forcong : advection and relaxation
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
LOGICAL, SAVE :: L2D_ADV_FRC 
LOGICAL, SAVE :: L2D_REL_FRC
REAL, SAVE    :: XRELAX_HEIGHT_BOT
REAL, SAVE    :: XRELAX_HEIGHT_TOP
REAL, SAVE    :: XRELAX_TIME
!
END MODULE MODD_2D_FRC
