!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE VEGETATION_UPDATE_GREENROOF (DTCO, DTI, IG, I, T, TOP, TVG, DTGD, GRM, &
                                            TPTIME,PTSTEP,KLU)  
!   ##########################################################################
!
!!****  *GREENROOF*  
!!
!!    PURPOSE
!!    -------
!
!     
!!**  METHOD
!     ------
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      A. Lemonsu          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!    Original    05/2009
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
USE MODD_DATA_TEB_GARDEN_n, ONLY : DATA_TEB_GARDEN_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
!
USE MODD_TYPE_DATE_SURF,    ONLY: DATE_TIME
!
USE MODI_VEGETATION_UPDATE
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    Declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
TYPE(DATA_TEB_GARDEN_t), INTENT(INOUT) :: DTGD
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
!
TYPE(DATE_TIME)     , INTENT(IN)    :: TPTIME             ! current date and time from teb
REAL                , INTENT(IN)    :: PTSTEP             ! time step
INTEGER,              INTENT(IN)    :: KLU                ! number of points
!
!
!*      0.2    Declarations of local variables
!
REAL, DIMENSION(KLU,1) :: ZZ0EFFIP
REAL, DIMENSION(KLU,1) :: ZZ0EFFIM
REAL, DIMENSION(KLU,1) :: ZZ0EFFJP
REAL, DIMENSION(KLU,1) :: ZZ0EFFJM
REAL, DIMENSION(KLU)   :: ZAOSIP
REAL, DIMENSION(KLU)   :: ZAOSIM
REAL, DIMENSION(KLU)   :: ZAOSJP
REAL, DIMENSION(KLU)   :: ZAOSJM
REAL, DIMENSION(KLU)   :: ZHO2IP
REAL, DIMENSION(KLU)   :: ZHO2IM
REAL, DIMENSION(KLU)   :: ZHO2JP
REAL, DIMENSION(KLU)   :: ZHO2JM
REAL, DIMENSION(KLU,1) :: ZLAI
REAL, DIMENSION(KLU,1) :: ZVEG
REAL, DIMENSION(KLU,1) :: ZZ0
REAL, DIMENSION(KLU,1) :: ZALBNIR
REAL, DIMENSION(KLU,1) :: ZALBVIS
REAL, DIMENSION(KLU,1) :: ZALBUV
REAL, DIMENSION(KLU,1) :: ZEMIS
REAL, DIMENSION(KLU,1) :: ZRSMIN
REAL, DIMENSION(KLU,1) :: ZGAMMA
REAL, DIMENSION(KLU,1) :: ZWRMAX_CF
REAL, DIMENSION(KLU,1) :: ZRGL
REAL, DIMENSION(KLU,1) :: ZCV
REAL, DIMENSION(KLU,1) :: ZGMES
REAL, DIMENSION(KLU,1) :: ZBSLAI
REAL, DIMENSION(KLU,1) :: ZLAIMIN
REAL, DIMENSION(KLU,1) :: ZSEFOLD
REAL, DIMENSION(KLU,1) :: ZGC
REAL, DIMENSION(KLU,1) :: ZDMAX
REAL, DIMENSION(KLU,1) :: ZF2I
LOGICAL, DIMENSION(KLU,1) :: GSTRESS
REAL, DIMENSION(KLU,1) :: ZALBNIR_VEG
REAL, DIMENSION(KLU,1) :: ZALBVIS_VEG
REAL, DIMENSION(KLU,1) :: ZALBUV_VEG
REAL, DIMENSION(KLU,1) :: ZALBNIR_SOIL
REAL, DIMENSION(KLU,1) :: ZALBVIS_SOIL
REAL, DIMENSION(KLU,1) :: ZALBUV_SOIL
REAL, DIMENSION(KLU,1) :: ZCE_NITRO
REAL, DIMENSION(KLU,1) :: ZCF_NITRO
REAL, DIMENSION(KLU,1) :: ZCNA_NITRO
! MEB stuff
REAL, DIMENSION(KLU,1) :: ZGNDLITTER
REAL, DIMENSION(KLU,1) :: ZRGLGV
REAL, DIMENSION(KLU,1) :: ZGAMMAGV
REAL, DIMENSION(KLU,1) :: ZRSMINGV
REAL, DIMENSION(KLU,1) :: ZWRMAX_CFGV
REAL, DIMENSION(KLU,1) :: ZH_VEG
REAL, DIMENSION(KLU,1) :: ZLAIGV
REAL, DIMENSION(KLU,1) :: ZZ0LITTER
!
TYPE (DATE_TIME), DIMENSION(KLU,1) :: TZSEED
TYPE (DATE_TIME), DIMENSION(KLU,1) :: TZREAP
REAL, DIMENSION(KLU,1) :: ZWATSUP
REAL, DIMENSION(KLU,1) :: ZIRRIG
LOGICAL :: GUPDATED              ! T if VEGETATION_UPDATE has reset fields
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*      1.     various initialisations
!              -----------------------
!
IF (LHOOK) CALL DR_HOOK('VEGETATION_UPDATE_GREENROF',0,ZHOOK_HANDLE)
!
!* orographic roughness not used
!
ZAOSIP = 0.
ZAOSIM = 0.
ZAOSJP = 0.
ZAOSJM = 0.
ZHO2IP = 0.
ZHO2IM = 0.
ZHO2JP = 0.
ZHO2JM = 0.
!
!* vegetation parameters to update
!
ZVEG(:,1)         = GRM%TGRPE%CUR%XVEG
ZZ0(:,1)          = GRM%TGRPE%CUR%XZ0
ZALBNIR(:,1)      = GRM%TGRPE%CUR%XALBNIR
ZALBVIS(:,1)      = GRM%TGRPE%CUR%XALBVIS
ZALBUV(:,1)       = GRM%TGRPE%CUR%XALBUV
ZEMIS(:,1)        = GRM%TGRPE%CUR%XEMIS
ZRSMIN(:,1)       = GRM%TGRP%XRSMIN
ZGAMMA(:,1)       = GRM%TGRP%XGAMMA
ZWRMAX_CF(:,1)    = GRM%TGRP%XWRMAX_CF
ZRGL(:,1)         = GRM%TGRP%XRGL
ZCV(:,1)          = GRM%TGRP%XCV
ZGMES(:,1)        = GRM%TGRP%XGMES
ZBSLAI(:,1)       = GRM%TGRP%XBSLAI
ZLAIMIN(:,1)      = GRM%TGRP%XLAIMIN
ZSEFOLD(:,1)      = GRM%TGRP%XSEFOLD
ZGC(:,1)          = GRM%TGRP%XGC
ZDMAX(:,1)        = GRM%TGRP%XDMAX
ZF2I(:,1)         = GRM%TGRP%XF2I
GSTRESS(:,1)      = GRM%TGRP%LSTRESS
ZALBNIR_VEG(:,1)  = GRM%TGRP%XALBNIR_VEG
ZALBVIS_VEG(:,1)  = GRM%TGRP%XALBVIS_VEG
ZALBUV_VEG(:,1)   = GRM%TGRP%XALBUV_VEG
ZALBNIR_SOIL(:,1) = GRM%TGRP%XALBNIR_SOIL
ZALBVIS_SOIL(:,1) = GRM%TGRP%XALBVIS_SOIL
ZALBUV_SOIL(:,1)  = GRM%TGRP%XALBUV_SOIL
ZCE_NITRO(:,1)    = GRM%TGRP%XCE_NITRO
ZCF_NITRO(:,1)    = GRM%TGRP%XCF_NITRO
ZCNA_NITRO(:,1)   = GRM%TGRP%XCNA_NITRO
! --------------------------------------------------------------------------------------
! Vegetation update (in case of non-interactive vegetation):
! --------------------------------------------------------------------------------------
!
GUPDATED=.FALSE.
IF (TVG%CPHOTO=='NON' .OR. TVG%CPHOTO=='AGS' .OR. TVG%CPHOTO=='AST') THEN
     CALL VEGETATION_UPDATE(DTCO, DTI, DTGD, GRM%DTGR, IG, I, GRM%TGRO, &
                            PTSTEP,TPTIME,TOP%XCOVER,TOP%LCOVER,                 &
                         TVG%CISBA,(.NOT. GRM%TGRO%LPAR_GREENROOF), TVG%CPHOTO, .FALSE.,  &
                         TVG%LTR_ML, 'GR ',                                  &
                         ZLAI,ZVEG,ZZ0,                                  &
                         ZALBNIR,ZALBVIS,ZALBUV,ZEMIS,                   &
                         ZRSMIN,ZGAMMA,ZWRMAX_CF,                        &
                         ZRGL,ZCV,                                       &
                         ZGMES,ZBSLAI,ZLAIMIN,ZSEFOLD,ZGC,ZDMAX,         &
                         ZF2I, GSTRESS,                                  &
                         ZAOSIP,ZAOSIM,ZAOSJP,ZAOSJM,                    &
                         ZHO2IP,ZHO2IM,ZHO2JP,ZHO2JM,                    &
                         ZZ0EFFIP,ZZ0EFFIM,ZZ0EFFJP,ZZ0EFFJM,            &
                         TVG%CALBEDO, ZALBNIR_VEG, ZALBVIS_VEG, ZALBUV_VEG,  &
                         ZALBNIR_SOIL, ZALBVIS_SOIL, ZALBUV_SOIL,        &
                         ZCE_NITRO, ZCF_NITRO, ZCNA_NITRO,               &
                         TZSEED, TZREAP, ZWATSUP, ZIRRIG,                &
                         ZGNDLITTER, ZRGLGV,ZGAMMAGV,                    &
                         ZRSMINGV, ZWRMAX_CFGV,                          &
                         ZH_VEG, ZLAIGV, ZZ0LITTER,                      &
                         GUPDATED, OABSENT=(T%CUR%XGREENROOF==0.)              )
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GRM%TGRPE%CUR%XVEG         = ZVEG(:,1)
GRM%TGRPE%CUR%XZ0          = ZZ0(:,1)
GRM%TGRPE%CUR%XALBNIR      = ZALBNIR(:,1)
GRM%TGRPE%CUR%XALBVIS      = ZALBVIS(:,1)
GRM%TGRPE%CUR%XALBUV       = ZALBUV(:,1)
GRM%TGRPE%CUR%XEMIS        = ZEMIS(:,1)
GRM%TGRP%XRSMIN       = ZRSMIN(:,1)
GRM%TGRP%XGAMMA       = ZGAMMA(:,1)
GRM%TGRP%XWRMAX_CF    = ZWRMAX_CF(:,1)
GRM%TGRP%XRGL         = ZRGL(:,1)
GRM%TGRP%XCV          = ZCV(:,1)
GRM%TGRP%XGMES        = ZGMES(:,1)
GRM%TGRP%XBSLAI       = ZBSLAI(:,1)
GRM%TGRP%XLAIMIN      = ZLAIMIN(:,1)
GRM%TGRP%XSEFOLD      = ZSEFOLD(:,1)
GRM%TGRP%XGC          = ZGC(:,1)
GRM%TGRP%XDMAX        = ZDMAX(:,1)
GRM%TGRP%XF2I         = ZF2I(:,1)
GRM%TGRP%LSTRESS      = GSTRESS(:,1)
GRM%TGRP%XALBNIR_VEG  = ZALBNIR_VEG(:,1)
GRM%TGRP%XALBVIS_VEG  = ZALBVIS_VEG(:,1)
GRM%TGRP%XALBUV_VEG   = ZALBUV_VEG(:,1)
GRM%TGRP%XALBNIR_SOIL = ZALBNIR_SOIL(:,1)
GRM%TGRP%XALBVIS_SOIL = ZALBVIS_SOIL(:,1)
GRM%TGRP%XALBUV_SOIL  = ZALBUV_SOIL(:,1)
GRM%TGRP%XCE_NITRO    = ZCE_NITRO(:,1)
GRM%TGRP%XCF_NITRO    = ZCF_NITRO(:,1)
GRM%TGRP%XCNA_NITRO   = ZCNA_NITRO(:,1)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (LHOOK) CALL DR_HOOK('VEGETATION_UPDATE_GREENROOF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE VEGETATION_UPDATE_GREENROOF
