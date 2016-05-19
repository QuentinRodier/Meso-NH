!
!
!     #####################
      MODULE MODI_PPM_SCALAR
!     #####################
!
INTERFACE
!
      SUBROUTINE PPM_SCALAR (HLBCX,HLBCY, KSV, KTCOUNT,   &
                     PCRU, PCRV, PCRW, PTSTEP, PRHODJ,    &
                     PSVT, PRSVS, HSV_ADV_SCHEME  ) 
!
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX ! X direction LBC type
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY ! Y direction LBC type
CHARACTER (LEN=6),               INTENT(IN) :: HSV_ADV_SCHEME
!
INTEGER,                  INTENT(IN)    :: KSV    ! Number of Scalar Variables
INTEGER,                  INTENT(IN)    :: KTCOUNT! iteration count
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCRU  ! Courant
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCRV  ! numbers
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCRW  ! 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ ! density
!
REAL,                     INTENT(IN)    :: PTSTEP ! Time step 
!
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PSVT         ! Vars at t
!
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRSVS ! Source terms
!
!
END SUBROUTINE PPM_SCALAR
!
END INTERFACE
!
END MODULE MODI_PPM_SCALAR
!
!     ######################################################################
      SUBROUTINE PPM_SCALAR (HLBCX,HLBCY, KSV, KTCOUNT,  &
                     PCRU, PCRV, PCRW, PTSTEP, PRHODJ,   &
                     PSVT, PRSVS, HSV_ADV_SCHEME  ) 
!     ######################################################################
!
!!****  *PPM_SCALAR * 
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    MODULE MODD_ARGSLIST
!!         HALO2LIST_ll : type for a list of "HALO2_lls"
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    MODIFICATIONS
!!    -------------
!!      Original 11.05.2006. T.Maric
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!USE MODE_ll
!
USE MODD_PARAMETERS
USE MODD_CONF
USE MODD_BUDGET
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
USE MODI_SHUMAN
USE MODI_BUDGET
USE MODI_PPM
USE MODI_ADVEC_PPM_ALGO
!
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX ! X direction LBC type
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY ! Y direction LBC type
CHARACTER (LEN=6),               INTENT(IN) :: HSV_ADV_SCHEME
!
INTEGER,                  INTENT(IN)    :: KSV    ! Number of Scalar Variables
INTEGER,                  INTENT(IN)    :: KTCOUNT! iteration count
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCRU  ! contravariant
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCRV  !  components
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCRW  ! of momentum
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ ! density
!
REAL,                     INTENT(IN)    :: PTSTEP ! Time step 
!
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PSVT             
!
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRSVS         ! Source terms
!
!
!*       0.2   Declarations of local variables :
!
INTEGER :: JSV           ! Loop index for Scalar Variables
!
INTEGER :: IGRID ! localisation on the model grid
!
!*        Variables specific to ppm scheme
!
! Advection source term calulated in the PPM algorithm
REAL, DIMENSION(SIZE(PCRU,1),SIZE(PCRU,2),SIZE(PCRU,3)) :: ZSRC
!
! Temporary advected rhodj
REAL, DIMENSION(SIZE(PCRU,1),SIZE(PCRU,2),SIZE(PCRU,3)) :: ZRHOX1,ZRHOX2
REAL, DIMENSION(SIZE(PCRU,1),SIZE(PCRU,2),SIZE(PCRU,3)) :: ZRHOY1,ZRHOY2
REAL, DIMENSION(SIZE(PCRU,1),SIZE(PCRU,2),SIZE(PCRU,3)) :: ZRHOZ1,ZRHOZ2
REAL, DIMENSION(SIZE(PCRU,1),SIZE(PCRU,2),SIZE(PCRU,3)) :: ZUNIT
!
!-------------------------------------------------------------------------------
!
!*       1.     CALL THE ADVEC_PPM_ALGO ROUTINE FOR EACH FIELD
!               -----------------------------------------------
!
IGRID = 1
!
! Calculate the advection of the density RHODJ to pass to the algorithm
!
ZUNIT = 1.0
ZRHOX1 = PPM_S0_X(HLBCX, IGRID, ZUNIT, PCRU, PRHODJ, PTSTEP)
ZRHOY1 = PPM_S0_Y(HLBCY, IGRID, ZUNIT, PCRV, ZRHOX1, PTSTEP)
ZRHOZ1 = PPM_S0_Z(IGRID, ZUNIT, PCRW, ZRHOY1, PTSTEP)
ZRHOZ2 = PPM_S0_Z(IGRID, ZUNIT, PCRW, PRHODJ, PTSTEP)
ZRHOY2 = PPM_S0_Y(HLBCY, IGRID, ZUNIT, PCRV, ZRHOZ2, PTSTEP)
ZRHOX2 = PPM_S0_X(HLBCX, IGRID, ZUNIT, PCRU, ZRHOY2, PTSTEP)
!
! Case with KSV tracers
!
DO JSV=1,KSV
!
   CALL ADVEC_PPM_ALGO(HSV_ADV_SCHEME, HLBCX, HLBCY, IGRID, PSVT(:,:,:,JSV), & 
                       PRHODJ, PTSTEP, & 
                       ZRHOX1, ZRHOX2, ZRHOY1, ZRHOY2, ZRHOZ1, ZRHOZ2, &
                       ZSRC, KTCOUNT, PCRU, PCRV, PCRW)
! add the advection to the sources
   PRSVS(:,:,:,JSV) =  PRSVS(:,:,:,JSV) + ZSRC(:,:,:)   
!
   IF (LBUDGET_SV) CALL BUDGET (PRSVS(:,:,:,JSV),JSV+12,'ADV_BU_RSV')
!
END DO
!
!
END SUBROUTINE PPM_SCALAR
