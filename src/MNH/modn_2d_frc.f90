!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modn 2006/11/23 17:22:54
!-----------------------------------------------------------------
!     ########################  
      MODULE MODN_2D_FRC
!     ########################
!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_2D_FRC
!
IMPLICIT NONE
!
!
NAMELIST/NAM_2D_FRC/L2D_ADV_FRC,L2D_REL_FRC,XRELAX_HEIGHT_BOT, XRELAX_HEIGHT_TOP,XRELAX_TIME
!

END MODULE MODN_2D_FRC
