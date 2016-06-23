!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GREENROOF_PROPERTIES (T, TVG, GRM, &
                                       PDIR_SW, PSCA_SW, PSW_BANDS, KSW,&
                                      PTS, PEMIS, PALB, PTA,           &  
                                      PALBNIR_TVEG, PALBVIS_TVEG,      &
                                      PALBNIR_TSOIL, PALBVIS_TSOIL     )  
!     ##########################################################################
!
!!****  *GREENROOF_PROPERTIES*  
!!
!!    PURPOSE
!!    -------
!
!     Based on garden_properties
!     Calculates grid-averaged albedo and emissivity (according to snow scheme)
!         
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      
!!    AUTHOR
!!    ------
!!
!!      S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original                      ?     
!!      C. de Munck and A. Lemonsu    09/2011  
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
!
USE MODD_SURF_PAR,            ONLY : XUNDEF
!
!
!
USE MODI_ISBA_PROPERTIES
USE MODI_FLAG_TEB_GREENROOF_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PDIR_SW            ! direct incoming solar radiation
REAL, DIMENSION(:,:), INTENT(IN)   :: PSCA_SW            ! diffus incoming solar radiation
REAL, DIMENSION(:)  , INTENT(IN)   :: PSW_BANDS          ! mean wavelength of each shortwave band (m)
INTEGER,              INTENT(IN)   :: KSW                ! number of short-wave spectral bands
!
REAL, DIMENSION(:)  , INTENT(OUT)  :: PTS                ! radiative surface temperature
REAL, DIMENSION(:)  , INTENT(OUT)  :: PEMIS              ! green areas emissivity
REAL, DIMENSION(:)  , INTENT(OUT)  :: PALB               ! green areas albedo
!
REAL, DIMENSION(:)  , INTENT(IN), OPTIONAL :: PTA        ! Air temperature (K)
!
REAL, DIMENSION(:)  , INTENT(OUT), OPTIONAL  :: PALBNIR_TVEG       ! nearIR  veg tot albedo
REAL, DIMENSION(:)  , INTENT(OUT), OPTIONAL  :: PALBVIS_TVEG       ! visible veg tot albedo
REAL, DIMENSION(:)  , INTENT(OUT), OPTIONAL  :: PALBNIR_TSOIL      ! nearIR  soil tot albedo
REAL, DIMENSION(:)  , INTENT(OUT), OPTIONAL  :: PALBVIS_TSOIL      ! visible soil tot albedo
!
!-------------------------------------------------------------------------------
!
!*      0.2    Local variables
!              ---------------
!
INTEGER                        :: JLAYER
INTEGER                        :: JSWB
!
REAL, DIMENSION(SIZE(PALB))    :: ZTSNOSNOW ! surf. temp. on snow free part
REAL, DIMENSION(SIZE(PALB))    :: ZTSSNOW   ! surf. temp. on snow covered part
REAL, DIMENSION(SIZE(PALB))    :: ZANOSNOW  ! snow-free surface albedo
REAL, DIMENSION(SIZE(PALB))    :: ZASNOW    ! snow albedo
REAL, DIMENSION(SIZE(PALB))    :: ZENOSNOW  ! snow-free surface emissivity
REAL, DIMENSION(SIZE(PALB))    :: ZESNOW    ! snow emissivity
!
REAL, DIMENSION(SIZE(PALB))    :: ZALBNIR_TVEG       ! nearIR  veg tot albedo
REAL, DIMENSION(SIZE(PALB))    :: ZALBVIS_TVEG       ! visible veg tot albedo
REAL, DIMENSION(SIZE(PALB))    :: ZALBNIR_TSOIL      ! nearIR  soil tot albedo
REAL, DIMENSION(SIZE(PALB))    :: ZALBVIS_TSOIL      ! visible soil tot albedo
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* only one patch for green roofs
IF (LHOOK) CALL DR_HOOK('GREENROOF_PROPERTIES',0,ZHOOK_HANDLE)
!
!*      1.     Set physical values for points where there is no green roof
!              -----------------------------------------------------------
!
! This way, ISBA can run without problem for these points
!
 CALL FLAG_TEB_GREENROOF_n(GRM%TGR, GRM%TGRO, GRM%TGRPE, T, TVG, &
                           1)
!
!
!*      2.     Computes several properties of green roofs
!              ------------------------------------------
!
!
 CALL ISBA_PROPERTIES(GRM%TGRO%CISBA_GR, GRM%TGRO%LTR_ML_GR, GRM%TGR%CUR%TSNOW, 1,       &
                      PDIR_SW, PSCA_SW, PSW_BANDS, KSW,          &
                      GRM%TGRPE%CUR%XALBNIR, GRM%TGRPE%CUR%XALBVIS, GRM%TGRPE%CUR%XALBUV,  &
                      GRM%TGRP%XALBNIR_VEG, GRM%TGRP%XALBVIS_VEG, GRM%TGRP%XALBUV_VEG,      &
                      GRM%TGRP%XALBNIR_SOIL, GRM%TGRP%XALBVIS_SOIL,                &
                      GRM%TGRP%XALBUV_SOIL, GRM%TGRPE%CUR%XVEG, GRM%TGRPE%CUR%XLAI, &
                      GRM%TGRPE%CUR%XZ0, GRM%TGRPE%CUR%XEMIS, GRM%TGR%CUR%XTG(:,1),          &
                      ZASNOW,ZANOSNOW,                           &
                      ZESNOW,ZENOSNOW,                           &
                      ZTSSNOW,ZTSNOSNOW,                         &
                      GRM%TGR%CUR%XSNOWFREE_ALB_VEG, GRM%TGR%CUR%XSNOWFREE_ALB_SOIL,     &
                      ZALBNIR_TVEG, ZALBVIS_TVEG, ZALBNIR_TSOIL, &
                      ZALBVIS_TSOIL,                             &
                      GRM%TGR%CUR%XPSN, GRM%TGR%CUR%XPSNV_A, GRM%TGR%CUR%XPSNG, GRM%TGR%CUR%XPSNV         )
!
GRM%TGR%CUR%XSNOWFREE_ALB = ZANOSNOW
!
!* averaged albedo
PALB =  GRM%TGR%CUR%XPSN(:)    * ZASNOW              + (1.-GRM%TGR%CUR%XPSN(:)) * ZANOSNOW
!* averaged emissivity
PEMIS=  GRM%TGR%CUR%XPSN(:)    * ZESNOW              + (1.-GRM%TGR%CUR%XPSN(:)) * ZENOSNOW
!* averaged surface radiative temperature
!  (recomputed from emitted long wave)
PTS  =((GRM%TGR%CUR%XPSN(:)    * ZESNOW * ZTSSNOW**4 + &
        (1.-GRM%TGR%CUR%XPSN(:)) * ZENOSNOW * ZTSNOSNOW**4) / PEMIS)**0.25
!
IF(PRESENT(PALBNIR_TVEG))PALBNIR_TVEG(:)=ZALBNIR_TVEG(:)
IF(PRESENT(PALBVIS_TVEG))PALBVIS_TVEG(:)=ZALBVIS_TVEG(:)
IF(PRESENT(PALBNIR_TSOIL))PALBNIR_TSOIL(:)=ZALBNIR_TSOIL(:)
IF(PRESENT(PALBVIS_TSOIL))PALBVIS_TSOIL(:)=ZALBVIS_TSOIL(:)
!
IF (LHOOK) CALL DR_HOOK('GREENROOF_PROPERTIES',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!

END SUBROUTINE GREENROOF_PROPERTIES
