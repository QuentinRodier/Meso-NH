!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_MEAN_FIELD
!     ##########################
!
!
INTERFACE

      SUBROUTINE MEAN_FIELD(PUT, PVT, PWT, PTHT, PTKET,PPABST)   

REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT, PVT, PWT   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT, PTKET   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST   ! variables

END SUBROUTINE MEAN_FIELD

END INTERFACE

END MODULE MODI_MEAN_FIELD
!
!     #######################################################
      SUBROUTINE MEAN_FIELD(PUT, PVT, PWT, PTHT, PTKET,PPABST)   
!     #######################################################
!
!!****  *MEAN_FIELD * -
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     P. Aumond 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2009
!!---------------------------------------------------------------
!
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_MEAN_FIELD_n
USE MODD_MEAN_FIELD
USE MODD_CST

!  
IMPLICIT NONE

!*       0.1   Declarations of dummy arguments :
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT, PVT, PWT   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT, PTKET   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST   ! variables

!
!*       0.2   Declarations of local variables :
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) ::  ZTEMPT
!-----------------------------------------------------------------------
!1. MEAN
!
   ZTEMPT = PTHT*(((PPABST)/XP00)**(XRD/XCPD))
!
   XUM_MEAN  = PUT + XUM_MEAN 
   XVM_MEAN  = PVT + XVM_MEAN
   XWM_MEAN  = PWT + XWM_MEAN
   XTHM_MEAN = PTHT + XTHM_MEAN
   XTEMPM_MEAN = ZTEMPT + XTEMPM_MEAN
   XTKEM_MEAN = PTKET + XTKEM_MEAN
   XPABSM_MEAN = PPABST + XPABSM_MEAN
!
   XU2_MEAN  = PUT**2 + XU2_MEAN 
   XV2_MEAN  = PVT**2 + XV2_MEAN
   XW2_MEAN  = PWT**2 + XW2_MEAN
   XTH2_MEAN = PTHT**2 + XTH2_MEAN
   XTEMP2_MEAN = ZTEMPT**2 + XTEMP2_MEAN
   XPABS2_MEAN = PPABST**2 + XPABS2_MEAN
!
   MEAN_COUNT = MEAN_COUNT + 1
!
END SUBROUTINE MEAN_FIELD
