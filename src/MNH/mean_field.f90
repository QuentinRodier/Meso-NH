!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_MEAN_FIELD
!     ##########################
!
!
INTERFACE

      SUBROUTINE MEAN_FIELD(PUM, PVM, PWM, PTHM, PTKEM,PPABSM,PUT, PVT, &
                            PWT, PTHT, PPABST,KTCOUNT,PTSTEP)

REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUM, PVM, PWM   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHM   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTKEM           !   at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABSM   ! variables

REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT, PVT, PWT   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST   ! variables

INTEGER, INTENT(IN)  :: KTCOUNT ! Temporal loop COUNTer
REAL,    INTENT(IN)  :: PTSTEP            !  Effective Time step

END SUBROUTINE MEAN_FIELD

END INTERFACE

END MODULE MODI_MEAN_FIELD
!
!     ##################################################################################
       SUBROUTINE MEAN_FIELD(PUM, PVM, PWM, PTHM, PTKEM, PPABSM,PUT, PVT, &
                             PWT, PTHT, PPABST,KTCOUNT,PTSTEP)
!     ###############################################################################
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
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUM, PVM, PWM   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHM   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTKEM           !   at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABSM   ! variables

REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT, PVT, PWT   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST   ! variables

INTEGER, INTENT(IN)  :: KTCOUNT ! Temporal loop COUNTer
REAL,    INTENT(IN) :: PTSTEP            !  Effective Time step
!
!*       0.2   Declarations of local variables :
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) ::  ZTEMPM, ZTEMPT
!-----------------------------------------------------------------------
!1. MEAN
!
   ZTEMPM = PTHM*(((PPABSM)/XP00)**(XRD/XCPD))
   ZTEMPT = PTHT*(((PPABST)/XP00)**(XRD/XCPD))
!
   XUM_MEAN  = PUM + XUM_MEAN 
   XVM_MEAN  = PVM + XVM_MEAN
   XWM_MEAN  = PWM + XWM_MEAN
   XTHM_MEAN = PTHM + XTHM_MEAN
   XTEMPM_MEAN = ZTEMPM + XTEMPM_MEAN
   XTKEM_MEAN = PTKEM + XTKEM_MEAN
   XPABSM_MEAN = PPABSM + XPABSM_MEAN
!
   XU2_MEAN  = PUM**2 + XU2_MEAN 
   XV2_MEAN  = PVM**2 + XV2_MEAN
   XW2_MEAN  = PWM**2 + XW2_MEAN
   XTH2_MEAN = PTHM**2 + XTH2_MEAN
   XTEMP2_MEAN = ZTEMPM**2 + XTEMP2_MEAN
   XPABS2_MEAN = PPABSM**2 + XPABS2_MEAN
!
   MEAN_COUNT = MEAN_COUNT + 1
!
END SUBROUTINE MEAN_FIELD
