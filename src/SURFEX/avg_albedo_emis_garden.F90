!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE AVG_ALBEDO_EMIS_GARDEN(HALBEDO,          &
                                 PVEG,PZ0,PLAI,PTG1,        &
                                 PSW_BANDS,                 &
                                 PALBNIR_VEG,PALBVIS_VEG,   &
                                 PALBUV_VEG,                &
                                 PALBNIR_SOIL,PALBVIS_SOIL, &
                                 PALBUV_SOIL,               &
                                 PEMIS_ECO,                 &
                                 TPSNOW,                    &
                                 PALBNIR_ECO,PALBVIS_ECO,   &
                                 PALBUV_ECO,                &
                                 PDIR_ALB,PSCA_ALB,         &
                                 PEMIS,PTSRAD               )  
!     ###################################################
!
!!**** ** computes radiative fields used in GARDEN
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    01/2004
!!     A. Bogatchev 09/2005 EBA snow option
!!     B. Decharme  2008    The fraction of vegetation covered by snow must be
!                            <= to XPSNG
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,  ONLY : XUNDEF
!
USE MODD_TYPE_SNOW
!
USE MODD_SNOW_PAR,   ONLY : XEMISSN
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODD_TEB_GARDEN_n,    ONLY : TSNOW, XPSN, XPSNV_A, XPSNG, XPSNV
!
USE MODI_ALBEDO
USE MODI_ALBEDO_FROM_NIR_VIS
USE MODI_ISBA_SNOW_FRAC
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=4),       INTENT(IN)   :: HALBEDO     ! albedo type
! Albedo dependance with surface soil water content
!   "EVOL" = albedo evolves with soil wetness
!   "DRY " = constant albedo value for dry soil
!   "WET " = constant albedo value for wet soil
!   "MEAN" = constant albedo value for medium soil wetness
!
REAL, DIMENSION(:),   INTENT(IN)   :: PVEG        ! vegetation fraction
REAL, DIMENSION(:),   INTENT(IN)   :: PZ0         ! roughness length
REAL, DIMENSION(:),   INTENT(IN)   :: PLAI        ! leaf area index
REAL, DIMENSION(:),   INTENT(IN)   :: PTG1        ! soil surface temperature
REAL, DIMENSION(:),     INTENT(IN)   :: PSW_BANDS   ! middle wavelength of each band 

REAL, DIMENSION(:),   INTENT(IN)   :: PALBNIR_VEG ! near-infra-red albedo of vegetation
REAL, DIMENSION(:),   INTENT(IN)   :: PALBVIS_VEG ! visible albedo of vegetation
REAL, DIMENSION(:),   INTENT(IN)   :: PALBUV_VEG  ! UV albedo of vegetation
REAL, DIMENSION(:),   INTENT(IN)   :: PALBNIR_SOIL! near-infra-red albedo of soil
REAL, DIMENSION(:),   INTENT(IN)   :: PALBVIS_SOIL! visible albedo of soil
REAL, DIMENSION(:),   INTENT(IN)   :: PALBUV_SOIL ! UV albedo of soil
REAL, DIMENSION(:),   INTENT(IN)   :: PEMIS_ECO   ! emissivity (soil+vegetation)
TYPE(SURF_SNOW),        INTENT(IN)   :: TPSNOW      ! prognostic snow cover
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PALBNIR_ECO ! near-infra-red albedo (soil+vegetation)
REAL, DIMENSION(:),   INTENT(OUT)  :: PALBVIS_ECO ! visible albedo (soil+vegetation)
REAL, DIMENSION(:),   INTENT(OUT)  :: PALBUV_ECO  ! UV albedo (soil+vegetation)
!
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PDIR_ALB    ! averaged direct albedo  (per wavelength)
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PSCA_ALB    ! averaged diffuse albedo (per wavelength)
REAL, DIMENSION(:),     INTENT(OUT)  :: PEMIS       ! averaged emissivity
REAL, DIMENSION(:),     INTENT(OUT)  :: PTSRAD      ! averaged radiaitve temp.
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
REAL, DIMENSION(SIZE(PALBNIR_VEG)) :: ZALBNIR ! near-infra-red albedo with snow
REAL, DIMENSION(SIZE(PALBVIS_VEG)) :: ZALBVIS ! visible albedo with snow
REAL, DIMENSION(SIZE(PALBUV_VEG )) :: ZALBUV  ! UV albedo with snow
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!
!*    1.      averaged albedo on natural continental surfaces (except prognostic snow)
!             -----------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('AVG_ALBEDO_EMIS_GARDEN',0,ZHOOK_HANDLE)
 CALL ALBEDO(HALBEDO,                                    &
              PALBVIS_VEG,PALBNIR_VEG,PALBUV_VEG,PVEG,    &
              PALBVIS_SOIL,PALBNIR_SOIL,PALBUV_SOIL,      &
              PALBVIS_ECO,PALBNIR_ECO,PALBUV_ECO          )  

!
!*    2.      averaged albedo and emis. on natural continental surfaces (with prognostic snow)
!             ---------------------------------------------------------
!
ZALBNIR(:)=0.
ZALBVIS(:)=0.
ZALBUV (:)=0.
!
PDIR_ALB(:,:)=0.
PSCA_ALB(:,:)=0.
PEMIS   (:)  =0.
PTSRAD  (:)  =0.
!   
!
  CALL ISBA_SNOW_FRAC(TSNOW%SCHEME,           &
         TSNOW%WSNOW(:,:,1), TSNOW%RHO(:,:,1),&
         TSNOW%ALB  (:,1),                    &
         PVEG(:), PLAI(:), PZ0(:),            &
         XPSN(:), XPSNV_A(:),                 &
         XPSNG(:), XPSNV(:)                   )
!
 WHERE (PVEG(:)/=XUNDEF)
!
! albedo on this tile
!
    ZALBNIR(:) = (1.-XPSN(:))*PALBNIR_ECO(:) &
                +    XPSN(:) *TPSNOW%ALB (:,1)  
      
    ZALBVIS(:) = (1.-XPSN(:))*PALBVIS_ECO(:) &
                +    XPSN(:) *TPSNOW%ALB (:,1)  
      
    ZALBUV(:)  = (1.-XPSN(:))*PALBUV_ECO (:) &
                +    XPSN(:) *TPSNOW%ALB (:,1)  
  END WHERE
!
!* albedo for each wavelength
!
  CALL ALBEDO_FROM_NIR_VIS(PSW_BANDS,ZALBNIR, ZALBVIS, ZALBUV,  &
                           PDIR_ALB(:,:), PSCA_ALB(:,:)         )  
!
! emissivity
!
  WHERE (PEMIS_ECO(:)/=XUNDEF)
    PEMIS(:)   = (1.-XPSN(:))*PEMIS_ECO  (:) &
                +    XPSN(:) *XEMISSN  
  END WHERE
!
!* radiative surface temperature
!
  IF (TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA') THEN
    PTSRAD(:) = PTG1(:)
  ELSE IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
    WHERE (PEMIS_ECO(:)/=XUNDEF)
    PTSRAD(:) =( ( (1.-XPSN(:))*PEMIS      (:)       *PTG1     (:)**4         &
                  +    XPSN(:) *TPSNOW%EMIS(:,1)*TPSNOW%TS(:,1)**4 ) )**0.25  &
                             / PEMIS(:)**0.25  
    END WHERE
  END IF
!
IF (LHOOK) CALL DR_HOOK('AVG_ALBEDO_EMIS_GARDEN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVG_ALBEDO_EMIS_GARDEN
