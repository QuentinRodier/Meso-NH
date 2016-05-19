!     #################
      MODULE MODD_FRC_n
!     #################
!
!!****  *MODD_FRC_n* - declaration of forcing evolution variables
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the variables for
!       forcing evolution
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	V. Masson & C. Lac    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2012                  
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE FRC_t
!          
  REAL, DIMENSION(:,:,:), POINTER :: XUFRC_PAST=>NULL()! Forcing wind components
  REAL, DIMENSION(:,:,:), POINTER :: XVFRC_PAST=>NULL()! at previous time-step
! 
END TYPE FRC_t

TYPE(FRC_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: FRC_MODEL

REAL, DIMENSION(:,:,:), POINTER :: XUFRC_PAST=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XVFRC_PAST=>NULL()

CONTAINS

SUBROUTINE FRC_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
FRC_MODEL(KFROM)%XUFRC_PAST=>XUFRC_PAST
FRC_MODEL(KFROM)%XVFRC_PAST=>XVFRC_PAST
!
! Current model is set to model KTO
XUFRC_PAST=>FRC_MODEL(KTO)%XUFRC_PAST
XVFRC_PAST=>FRC_MODEL(KTO)%XVFRC_PAST

END SUBROUTINE FRC_GOTO_MODEL

END MODULE MODD_FRC_n
