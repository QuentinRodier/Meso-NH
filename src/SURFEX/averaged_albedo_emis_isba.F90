!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGED_ALBEDO_EMIS_ISBA (I, &
                                            OFLOOD, HALBEDO, &
                                 PZENITH,PVEG,PZ0,PLAI,     &
                                 OMEB_PATCH,PGNDLITTER,     &
                                 PZ0LITTER,PLAIGV,          &
                                 PH_VEG, PTV,               &
                                 PTG1,PPATCH,               &
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
                                 PEMIS,PTSRAD,PTSURF,       &
                                 PDIR_SW, PSCA_SW           )
!     ###################################################
!
!!**** ** computes radiative fields used in ISBA
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
!                            <= to ZSNG
!!     B. Decharme  2013    new coupling variable and optimization    
!!     P. Samuelsson 10/2014 MEB
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_SURF_PAR,  ONLY : XUNDEF
!
USE MODD_TYPE_SNOW
!
USE MODD_CSTS,      ONLY : XSTEFAN
USE MODE_MEB,       ONLY : MEBPALPHAN
!
USE MODI_ALBEDO
USE MODI_AVERAGE_RAD
USE MODI_UPDATE_RAD_ISBA_n
USE MODI_ISBA_LWNET_MEB
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
!
TYPE(ISBA_t), INTENT(INOUT) :: I
!
LOGICAL,                INTENT(IN)   :: OFLOOD
 CHARACTER(LEN=4),       INTENT(IN)   :: HALBEDO     ! albedo type
! Albedo dependance with surface soil water content
!   "EVOL" = albedo evolves with soil wetness
!   "DRY " = constant albedo value for dry soil
!   "WET " = constant albedo value for wet soil
!   "MEAN" = constant albedo value for medium soil wetness
!
REAL, DIMENSION(:,:),   INTENT(IN)   :: PVEG        ! vegetation fraction
REAL, DIMENSION(:,:),   INTENT(IN)   :: PZ0         ! roughness length
REAL, DIMENSION(:,:),   INTENT(IN)   :: PLAI        ! leaf area index
LOGICAL, DIMENSION(:),  INTENT(IN)   :: OMEB_PATCH  ! multi-energy balance logical vector
REAL, DIMENSION(:,:),   INTENT(IN)   :: PGNDLITTER  ! Ground litter fraction
REAL, DIMENSION(:,:),   INTENT(IN)   :: PLAIGV      ! Understory leaf area index
REAL, DIMENSION(:,:),   INTENT(IN)   :: PZ0LITTER   ! Ground litter roughness length
REAL, DIMENSION(:,:),   INTENT(IN)   :: PH_VEG      ! Height of vegetation
REAL, DIMENSION(:,:),   INTENT(IN)   :: PTV         ! canopy vegetation temperature
REAL, DIMENSION(:,:),   INTENT(IN)   :: PTG1        ! soil surface temperature
REAL, DIMENSION(:,:),   INTENT(IN)   :: PPATCH      ! tile fraction
REAL, DIMENSION(:),     INTENT(IN)   :: PSW_BANDS   ! middle wavelength of each band
REAL, DIMENSION(:),     INTENT(IN)   :: PZENITH     

REAL, DIMENSION(:,:),   INTENT(IN)   :: PALBNIR_VEG ! near-infra-red albedo of vegetation
REAL, DIMENSION(:,:),   INTENT(IN)   :: PALBVIS_VEG ! visible albedo of vegetation
REAL, DIMENSION(:,:),   INTENT(IN)   :: PALBUV_VEG  ! UV albedo of vegetation
REAL, DIMENSION(:,:),   INTENT(IN)   :: PALBNIR_SOIL! near-infra-red albedo of soil
REAL, DIMENSION(:,:),   INTENT(IN)   :: PALBVIS_SOIL! visible albedo of soil
REAL, DIMENSION(:,:),   INTENT(IN)   :: PALBUV_SOIL ! UV albedo of soil
REAL, DIMENSION(:,:),   INTENT(IN)   :: PEMIS_ECO   ! emissivity (soil+vegetation)
TYPE(SURF_SNOW),        INTENT(IN)   :: TPSNOW      ! prognostic snow cover
!
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PALBNIR_ECO ! near-infra-red albedo (soil+vegetation)
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PALBVIS_ECO ! visible albedo (soil+vegetation)
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PALBUV_ECO  ! UV albedo (soil+vegetation)
!
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PDIR_ALB    ! averaged direct albedo  (per wavelength)
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PSCA_ALB    ! averaged diffuse albedo (per wavelength)
REAL, DIMENSION(:),     INTENT(OUT)  :: PEMIS       ! averaged emissivity
REAL, DIMENSION(:),     INTENT(OUT)  :: PTSRAD      ! averaged radiaitve temp.
REAL, DIMENSION(:),     INTENT(OUT)  :: PTSURF      ! surface effective temperature         (K)
!
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL   :: PDIR_SW ! Downwelling direct SW radiation
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL   :: PSCA_SW ! Downwelling diffuse SW radiation
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
REAL, DIMENSION(SIZE(PALBNIR_VEG,1),SIZE(PSW_BANDS),SIZE(PALBVIS_VEG,2)) :: ZDIR_ALB_PATCH 
!                                                     ! direct albedo
REAL, DIMENSION(SIZE(PALBNIR_VEG,1),SIZE(PSW_BANDS),SIZE(PALBVIS_VEG,2)) :: ZSCA_ALB_PATCH 
!                                                     ! diffuse albedo
REAL, DIMENSION(SIZE(PEMIS_ECO,  1),SIZE(PALBVIS_VEG,2)) :: ZEMIS_PATCH   ! emissivity with snow-flood
REAL, DIMENSION(SIZE(PEMIS_ECO,  1),SIZE(PALBVIS_VEG,2)) :: ZTSRAD_PATCH  ! Tsrad
REAL, DIMENSION(SIZE(PEMIS_ECO,  1),SIZE(PALBVIS_VEG,2)) :: ZTSURF_PATCH  ! Tsurf
REAL, DIMENSION(SIZE(PEMIS_ECO,  1)) :: ZEMIS         ! emissivity with flood
!
REAL, DIMENSION(SIZE(PEMIS_ECO,  1)) :: ZSNOWDEPTH    ! Total snow depth
REAL, DIMENSION(SIZE(PEMIS_ECO,  1)) :: ZPALPHAN      ! Snow/canopy ratio factor 
REAL, DIMENSION(SIZE(PEMIS_ECO,  1)) :: ZLW_RAD       ! Fake downwelling LW rad
REAL, DIMENSION(SIZE(PEMIS_ECO,  1)) :: ZLW_UP        ! Upwelling LW rad
REAL, DIMENSION(SIZE(PEMIS_ECO,  1)) :: ZLWNET_N      ! LW net for snow surface
REAL, DIMENSION(SIZE(PEMIS_ECO,  1)) :: ZLWNET_V      ! LW net for canopy veg
REAL, DIMENSION(SIZE(PEMIS_ECO,  1)) :: ZLWNET_G      ! LW net for ground
REAL, DIMENSION(SIZE(PEMIS_ECO,  1)) :: ZDUMMY
REAL, DIMENSION(SIZE(PEMIS_ECO,  1)) :: ZEMISF
REAL, DIMENSION(SIZE(PEMIS_ECO,  1)) :: ZFF
!
LOGICAL :: LEXPLICIT_SNOW ! snow scheme key
!
INTEGER :: INP, INI
INTEGER :: JP, JI ! loop on patches
INTEGER :: JPATCH ! loop on patches
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    0.      Init
!             ----
!
IF (LHOOK) CALL DR_HOOK('AVERAGED_ALBEDO_EMIS_ISBA',0,ZHOOK_HANDLE)
!
INI=SIZE(PPATCH,1)
INP=SIZE(PPATCH,2)
!
PDIR_ALB(:,:)=0.
PSCA_ALB(:,:)=0.
PEMIS   (:)  =0.
PTSRAD  (:)  =0.
PTSURF  (:)  =0.
!
ZDIR_ALB_PATCH(:,:,:)=0.
ZSCA_ALB_PATCH(:,:,:)=0.
ZEMIS_PATCH   (:,:  )=0.
!
LEXPLICIT_SNOW = (TPSNOW%SCHEME=='3-L'.OR.TPSNOW%SCHEME=='CRO')
!
ZTSRAD_PATCH (:,:) = PTG1(:,:)
ZTSURF_PATCH (:,:) = PTG1(:,:)
!
!
!*    1.      averaged albedo on natural continental surfaces (except prognostic snow)
!             -----------------------------------------------
!
 CALL ALBEDO(HALBEDO,                                    &
              PALBVIS_VEG,PALBNIR_VEG,PALBUV_VEG,PVEG,    &
              PALBVIS_SOIL,PALBNIR_SOIL,PALBUV_SOIL,      &
              PALBVIS_ECO,PALBNIR_ECO,PALBUV_ECO          )  

!
!*    2.      averaged albedo and emis. on natural continental surfaces (with prognostic snow)
!             ---------------------------------------------------------
!
! A dummy downwelling LW radiation can be used for calculation of radiative surface temp 
!
ZLW_RAD(:) = 300.0
!    
!* Initialization of albedo for each wavelength, emissivity and snow/flood fractions
!
IF(PRESENT(PDIR_SW))THEN
!
! For the case when MEB patch albedo is requested downweeling SW is needed
!
  CALL UPDATE_RAD_ISBA_n(I, &
                         OFLOOD, TPSNOW%SCHEME,PZENITH,PSW_BANDS,PVEG,PLAI,PZ0, &
                         OMEB_PATCH,PLAIGV,PGNDLITTER,PZ0LITTER, PH_VEG,        &
                         PALBNIR_ECO,PALBVIS_ECO,PALBUV_ECO,PEMIS_ECO,          &
                         ZDIR_ALB_PATCH,ZSCA_ALB_PATCH,ZEMIS_PATCH,             &
                         PDIR_SW, PSCA_SW,                                      &
                         PALBNIR_VEG, PALBNIR_SOIL,                             &
                         PALBVIS_VEG, PALBVIS_SOIL                              )
ELSE
!
! For cases when MEB patch albedo is not requested no downweeling SW is needed
!
  CALL UPDATE_RAD_ISBA_n(I, &
                         OFLOOD, TPSNOW%SCHEME,PZENITH,PSW_BANDS,PVEG,PLAI,PZ0, &
                         OMEB_PATCH,PLAIGV,PGNDLITTER,PZ0LITTER, PH_VEG,        &
                         PALBNIR_ECO,PALBVIS_ECO,PALBUV_ECO,PEMIS_ECO,          &
                         ZDIR_ALB_PATCH,ZSCA_ALB_PATCH,ZEMIS_PATCH              )
ENDIF
!
!
!* radiative surface temperature
!
DO JPATCH=1,SIZE(PALBVIS_VEG,2)
!
  IF(OMEB_PATCH(JPATCH))THEN  ! MEB patches
!
!   ZPALPHAN is needed as input to ISBA_LWNET_MEB
!
    ZSNOWDEPTH(:) = SUM(TPSNOW%WSNOW(:,:,JPATCH)/TPSNOW%RHO(:,:,JPATCH),2)
    ZPALPHAN(:)   = MEBPALPHAN(ZSNOWDEPTH,PH_VEG(:,JPATCH))
!
!   ZLWNET_N,ZLWNET_V,ZLWNET_G are needed for ZLW_UP and ZTSRAD_PATCH
!
    IF(OFLOOD)THEN
      ZEMISF(:) = I%XEMISF(:,JPATCH)
      ZFF   (:) = I%XFF   (:,JPATCH)
    ELSE
      ZEMISF(:) = XUNDEF
      ZFF   (:) = 0.0
    ENDIF
!
    CALL ISBA_LWNET_MEB(PLAI(:,JPATCH),I%XPSN(:,JPATCH),ZPALPHAN,   &
        TPSNOW%EMIS(:,JPATCH),ZEMISF(:),ZFF(:),                   &
        PTV(:,JPATCH),PTG1(:,JPATCH),TPSNOW%TS(:,JPATCH),         &
        ZLW_RAD,ZLWNET_N,ZLWNET_V,ZLWNET_G,                       &
        ZDUMMY,ZDUMMY,ZDUMMY,                                     &
        ZDUMMY,ZDUMMY,ZDUMMY,                                     &
        ZDUMMY,ZDUMMY,ZDUMMY,                                     &
        ZDUMMY,ZDUMMY,ZDUMMY                                      )
!
    ZLW_UP(:)   = ZLW_RAD(:) - (ZLWNET_V(:) + ZLWNET_G(:) + ZLWNET_N(:))
!
!   MEB patch radiative temperature
!
    WHERE (ZEMIS_PATCH(:,JPATCH)/=0.)
      ZTSRAD_PATCH(:,JPATCH) = ((ZLW_UP(:) - ZLW_RAD(:)*(1.0-ZEMIS_PATCH(:,JPATCH)))/ &
                              (XSTEFAN*ZEMIS_PATCH(:,JPATCH)))**0.25
    END WHERE
!
  ELSE   ! Non-MEB patches

    ZEMIS(:) = PEMIS_ECO(:,JPATCH)
!
    IF(OFLOOD.AND.LEXPLICIT_SNOW)THEN
      WHERE(I%XPSN(:,JPATCH)<1.0.AND.PEMIS_ECO(:,JPATCH)/=XUNDEF)          
        ZEMIS(:) = ((1.-I%XFF(:,JPATCH)-I%XPSN(:,JPATCH))*PEMIS_ECO(:,JPATCH) + I%XFF(:,JPATCH)*I%XEMISF(:,JPATCH)) &
                   /(1.-I%XPSN(:,JPATCH))
      ENDWHERE   
    ENDIF
!
    IF (.NOT.LEXPLICIT_SNOW) THEN
      ZTSRAD_PATCH(:,JPATCH) = PTG1(:,JPATCH)
    ELSE IF (LEXPLICIT_SNOW) THEN
      WHERE (PEMIS_ECO(:,JPATCH)/=XUNDEF .AND. ZEMIS_PATCH(:,JPATCH)/=0.)
        ZTSRAD_PATCH(:,JPATCH) =( ( (1.-I%XPSN(:,JPATCH))*ZEMIS      (:)       *PTG1     (:,JPATCH)**4            &
                                    +    I%XPSN(:,JPATCH) *TPSNOW%EMIS(:,JPATCH)*TPSNOW%TS(:,JPATCH)**4 ) )**0.25  &
                                 / ZEMIS_PATCH(:,JPATCH)**0.25  
      END WHERE
    END IF
  ENDIF
END DO
!
!* averaged radiative fields
!
 CALL AVERAGE_RAD(PPATCH,                                                     &
                   ZDIR_ALB_PATCH, ZSCA_ALB_PATCH, ZEMIS_PATCH, ZTSRAD_PATCH, &
                   PDIR_ALB,       PSCA_ALB,       PEMIS,       PTSRAD        )  
!
!* averaged effective temperature
!
IF(LEXPLICIT_SNOW)THEN
  ZTSURF_PATCH(:,:) = PTG1(:,:)*(1.-I%XPSN(:,:)) + TPSNOW%TS(:,:)*I%XPSN(:,:)
ENDIF
!
DO JP=1,INP
  DO JI=1,INI
     PTSURF(JI) = PTSURF(JI) + PPATCH(JI,JP) * ZTSURF_PATCH(JI,JP)
  ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('AVERAGED_ALBEDO_EMIS_ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGED_ALBEDO_EMIS_ISBA
