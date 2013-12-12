!    ######################################
     MODULE MODI_THLRT_FROM_THRVRCRI
!    ######################################
!
INTERFACE
      
!     #################################################################
      SUBROUTINE THLRT_FROM_THRVRCRI( KRR,                             &
                                       PTH, PR, PLVOCPEXN,PLSOCPEXN,   &
                                       PTHL, PRT                       )
!     #################################################################

!*               1.1  Declaration of Arguments
!

INTEGER,                INTENT(IN)   :: KRR           ! number of moist var.

REAL, DIMENSION(:,:,:), INTENT(IN)   :: PTH      ! theta
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PR       ! water species
REAL, DIMENSION(:,:,:), INTENT(IN)    :: PLVOCPEXN,PLSOCPEXN     ! L/(cp*Pi)

REAL, DIMENSION(:,:,:), INTENT(OUT)  ::  PTHL, PRT
!
END SUBROUTINE THLRT_FROM_THRVRCRI

END INTERFACE
!
END MODULE MODI_THLRT_FROM_THRVRCRI

                                       
!     #################################################################
      SUBROUTINE THLRT_FROM_THRVRCRI( KRR,                             &
                                       PTH, PR, PLVOCPEXN,PLSOCPEXN,   &
                                       PTHL, PRT                       )
!     #################################################################
!
!!
!!****  *THLRT_FROM_THRVRCRI* - 
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!    
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!     
!!     V. Masson     *CNRM*
!!
!!    MODIFICATIONS
!!    -------------
!!     Original   06/2011
!!
!! --------------------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
INTEGER,                INTENT(IN)   :: KRR           ! number of moist var.

REAL, DIMENSION(:,:,:), INTENT(IN)   :: PTH      ! theta
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PR       ! water species
REAL, DIMENSION(:,:,:), INTENT(IN)    :: PLVOCPEXN,PLSOCPEXN     ! L/(cp*Pi)

REAL, DIMENSION(:,:,:), INTENT(OUT)  ::  PTHL, PRT
!
!-------------------------------------------------------------------------------
!
!
IF ( KRR == 0 ) THEN
  PTHL(:,:,:) = PTH(:,:,:)
  PRT (:,:,:) = 0.
ELSE IF (KRR==1) THEN
  PTHL(:,:,:) = PTH(:,:,:)
  PRT (:,:,:) = PR (:,:,:,1)
ELSE IF ( KRR>= 4 ) THEN
    ! Rnp at t
    PRT(:,:,:)  = PR(:,:,:,1)  + PR(:,:,:,2)  + PR(:,:,:,4)
    ! Theta_l at t
    PTHL(:,:,:)  = PTH(:,:,:)  - PLVOCPEXN(:,:,:) * PR(:,:,:,2) &
                               - PLSOCPEXN(:,:,:) * PR(:,:,:,4)
ELSE IF ( KRR>= 2 ) THEN
    ! Rnp at t
    PRT(:,:,:)  = PR(:,:,:,1)  + PR(:,:,:,2)
    ! Theta_l at t
    PTHL(:,:,:)  = PTH(:,:,:)  - PLVOCPEXN(:,:,:) * PR(:,:,:,2)
END IF
!-------------------------------------------------------------------------------
!
END SUBROUTINE THLRT_FROM_THRVRCRI
