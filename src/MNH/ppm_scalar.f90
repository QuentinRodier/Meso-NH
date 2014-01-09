!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
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
                     PRHOX1, PRHOX2, PRHOY1, PRHOY2,      &
                     PRHOZ1, PRHOZ2,                      &
                     PSVT, PRSVS, HSV_ADV_SCHEME          ) 
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
! Temporary advected rhodj
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHOX1,PRHOX2
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHOY1,PRHOY2
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHOZ1,PRHOZ2
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
      SUBROUTINE PPM_SCALAR (HLBCX,HLBCY, KSV, KTCOUNT,   &
                     PCRU, PCRV, PCRW, PTSTEP, PRHODJ,    &
                     PRHOX1, PRHOX2, PRHOY1, PRHOY2,      &
                     PRHOZ1, PRHOZ2,                      &
                     PSVT, PRSVS, HSV_ADV_SCHEME          ) 
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
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
USE MODI_SHUMAN
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
! Temporary advected rhodj
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHOX1,PRHOX2
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHOY1,PRHOY2
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHOZ1,PRHOZ2
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
!
!-------------------------------------------------------------------------------
!
!*       1.     CALL THE ADVEC_PPM_ALGO ROUTINE FOR EACH FIELD
!               -----------------------------------------------
!
IGRID = 1
!
! Case with KSV tracers
!
DO JSV=1,KSV
!
   CALL ADVEC_PPM_ALGO(HSV_ADV_SCHEME, HLBCX, HLBCY, IGRID, PSVT(:,:,:,JSV), & 
                       PRHODJ, PTSTEP, & 
                       PRHOX1, PRHOX2, PRHOY1, PRHOY2, PRHOZ1, PRHOZ2, &
                       ZSRC, KTCOUNT, PCRU, PCRV, PCRW)
! add the advection to the sources
   PRSVS(:,:,:,JSV) =  PRSVS(:,:,:,JSV) + ZSRC(:,:,:)   
!
END DO
!
!
END SUBROUTINE PPM_SCALAR
