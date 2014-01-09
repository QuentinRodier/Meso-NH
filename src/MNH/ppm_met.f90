!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!
!     #####################
      MODULE MODI_PPM_MET  
!     #####################
!
INTERFACE
!
      SUBROUTINE PPM_MET (HLBCX,HLBCY, KRR, KTCOUNT,              &
                          PCRU, PCRV, PCRW, PTSTEP, PRHODJ,       &
                          PRHOX1, PRHOX2, PRHOY1, PRHOY2,         &
                          PRHOZ1, PRHOZ2, PTHT, PTKET, PRT,       &
                          PRTHS, PRTKES, PRRS, HMET_ADV_SCHEME    )
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
! Temporary advected rhodj
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHOX1,PRHOX2
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHOY1,PRHOY2
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHOZ1,PRHOZ2
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
                          PRHOX1, PRHOX2, PRHOY1, PRHOY2,         &
                          PRHOZ1, PRHOZ2, PTHT, PTKET, PRT,       &
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
!!      Modification : 11.2011 C.Lac, V.Masson : Advection of (theta_l,r_t) 
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
!
USE MODI_SHUMAN
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
! Temporary advected rhodj
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHOX1,PRHOX2
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHOY1,PRHOY2
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHOZ1,PRHOZ2
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
!
! Potential temperature
!
CALL ADVEC_PPM_ALGO(HMET_ADV_SCHEME, HLBCX, HLBCY, IGRID, PTHT, PRHODJ, PTSTEP, &
                    PRHOX1, PRHOX2, PRHOY1, PRHOY2, PRHOZ1, PRHOZ2, &
                    ZSRC, KTCOUNT, PCRU, PCRV, PCRW)
! add the advection to the sources
PRTHS = PRTHS +  ZSRC 
!
!
! Turbulence variables
!
IF (GTKEALLOC) THEN
   CALL ADVEC_PPM_ALGO(HMET_ADV_SCHEME, HLBCX, HLBCY, IGRID, PTKET,PRHODJ,PTSTEP, &
                       PRHOX1, PRHOX2, PRHOY1, PRHOY2, PRHOZ1, PRHOZ2, &
                       ZSRC, KTCOUNT, PCRU, PCRV, PCRW)
  PRTKES = PRTKES + ZSRC
!
!
END IF
!
!
!
! Case with KRR moist variables
!
DO JRR=1,KRR
!
   CALL ADVEC_PPM_ALGO(HMET_ADV_SCHEME, HLBCX, HLBCY, IGRID,           &
                       PRT(:,:,:,JRR), PRHODJ, PTSTEP,                 &
                       PRHOX1, PRHOX2, PRHOY1, PRHOY2, PRHOZ1, PRHOZ2, &
                       ZSRC, KTCOUNT, PCRU, PCRV, PCRW                 )
   PRRS(:,:,:,JRR) = PRRS(:,:,:,JRR) + ZSRC(:,:,:)
!
END DO
!
!
END SUBROUTINE PPM_MET
