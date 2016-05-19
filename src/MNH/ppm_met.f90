!
!     #####################
      MODULE MODI_PPM_MET  
!     #####################
!
INTERFACE
!
      SUBROUTINE PPM_MET (HLBCX,HLBCY, KRR, KTCOUNT,              &
                          PCRU, PCRV, PCRW, PTSTEP, PRHODJ,       &
                          PTHT, PTKET, PRT,                       &
                          PRTHS, PRTKES, PRRS, HMET_ADV_SCHEME    )
!
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX ! X direction LBC type
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY ! Y direction LBC type
CHARACTER (LEN=6),               INTENT(IN) :: HMET_ADV_SCHEME
!
INTEGER,                  INTENT(IN)    :: KRR    ! Number of moist variables
INTEGER,                  INTENT(IN)    :: KTCOUNT! iteration count
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCRU  ! Courant
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCRV  ! numbers
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCRW  ! 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ ! density
!
REAL,                     INTENT(IN)    :: PTSTEP ! Single Time step 
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT, PTKET        ! Vars at t
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PRT 
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTHS, PRTKES! Source terms
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS 
!
END SUBROUTINE PPM_MET   
!
END INTERFACE
!
END MODULE MODI_PPM_MET
!
!     ######################################################################
      SUBROUTINE PPM_MET (HLBCX,HLBCY, KRR, KTCOUNT,              &
                          PCRU, PCRV, PCRW, PTSTEP, PRHODJ,       &
                          PTHT, PTKET, PRT,                       &
                          PRTHS, PRTKES, PRRS, HMET_ADV_SCHEME    )
!     ######################################################################
!
!!****  *PPM_MET * 
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
! incorporate ADVEC_4TH_ORDER_ALG, MZF4 and MZM4
!USE MODI_ADVEC_4TH_ORDER_AUX
!
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX ! X direction LBC type
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY ! Y direction LBC type
CHARACTER (LEN=6),               INTENT(IN) :: HMET_ADV_SCHEME
!
INTEGER,                  INTENT(IN)    :: KRR    ! Number of moist variables
INTEGER,                  INTENT(IN)    :: KTCOUNT! iteration count
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCRU  ! contravariant
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCRV  !  components
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCRW  ! of momentum
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ ! density
!
REAL,                     INTENT(IN)    :: PTSTEP ! Time step 
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT, PTKET ! Vars at t
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PRT 
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTHS, PRTKES! Source terms
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS 
!
!*       0.2   Declarations of local variables :
!
INTEGER :: JRR           ! Loop index for  moist variables
!
LOGICAL :: GTKEALLOC     ! true if TKE arrays are not zero-sized
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
!*       1.     COMPUTES THE DOMAIN DIMENSIONS
!               ------------------------------
!
GTKEALLOC = SIZE(PTKET,1) /= 0
!
!-------------------------------------------------------------------------------
!
!*       2.     CALL THE ADVEC_PPM_ALGO ROUTINE FOR EACH FIELD
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
!
! Potential temperature
!
CALL ADVEC_PPM_ALGO(HMET_ADV_SCHEME, HLBCX, HLBCY, IGRID, PTHT, PRHODJ, PTSTEP, &
                    ZRHOX1, ZRHOX2, ZRHOY1, ZRHOY2, ZRHOZ1, ZRHOZ2, &
                    ZSRC, KTCOUNT, PCRU, PCRV, PCRW)
! add the advection to the sources
PRTHS = PRTHS +  ZSRC 
!
IF (LBUDGET_TH) CALL BUDGET (PRTHS,4,'ADV_BU_RTH')
!
! Turbulence variables
!
IF (GTKEALLOC) THEN
   CALL ADVEC_PPM_ALGO(HMET_ADV_SCHEME, HLBCX, HLBCY, IGRID, PTKET,PRHODJ,PTSTEP, &
                       ZRHOX1, ZRHOX2, ZRHOY1, ZRHOY2, ZRHOZ1, ZRHOZ2, &
                       ZSRC, KTCOUNT, PCRU, PCRV, PCRW)
  PRTKES = PRTKES + ZSRC
!
  IF (LBUDGET_TKE) CALL BUDGET (PRTKES,5,'ADV_BU_RTKE')
!
END IF
!
!
!
! Case with KRR moist variables
!
DO JRR=1,KRR
!
   CALL ADVEC_PPM_ALGO(HMET_ADV_SCHEME, HLBCX, HLBCY, IGRID, PRT(:,:,:,JRR), &
                       PRHODJ, PTSTEP, &
                       ZRHOX1, ZRHOX2, ZRHOY1, ZRHOY2, ZRHOZ1, ZRHOZ2, &
                       ZSRC, KTCOUNT, PCRU, PCRV, PCRW)
   PRRS(:,:,:,JRR) = PRRS(:,:,:,JRR) + ZSRC(:,:,:)
!
   IF (JRR==1.AND.LBUDGET_RV) CALL BUDGET (PRRS(:,:,:,1),6,'ADV_BU_RRV') 
   IF (JRR==2.AND.LBUDGET_RC) CALL BUDGET (PRRS(:,:,:,2),7,'ADV_BU_RRC') 
   IF (JRR==3.AND.LBUDGET_RR) CALL BUDGET (PRRS(:,:,:,3),8,'ADV_BU_RRR') 
   IF (JRR==4.AND.LBUDGET_RI) CALL BUDGET (PRRS(:,:,:,4),9,'ADV_BU_RRI') 
   IF (JRR==5.AND.LBUDGET_RS) CALL BUDGET (PRRS(:,:,:,5),10,'ADV_BU_RRS') 
   IF (JRR==6.AND.LBUDGET_RG) CALL BUDGET (PRRS(:,:,:,6),11,'ADV_BU_RRG') 
   IF (JRR==7.AND.LBUDGET_RH) CALL BUDGET (PRRS(:,:,:,7),12,'ADV_BU_RRH') 
!
END DO
!
!
END SUBROUTINE PPM_MET
