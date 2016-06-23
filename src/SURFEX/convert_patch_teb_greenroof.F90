!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE CONVERT_PATCH_TEB_GREENROOF (DTCO, DTI, I, TGRO, TGRPE, TGRP, TOP, TVG, &
                                        KLU,KDECADE)
!#############################################################
!
!!****  *CONVERT_PATCH_TEB_GREENROOF* - routine to initialize TEB_GREENROOF parameters 
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
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      A. Lemonsu  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_TEB_GREENROOF_OPTION_n, ONLY : TEB_GREENROOF_OPTIONS_t
USE MODD_TEB_GREENROOF_PGD_EVOL_n, ONLY : TEB_GREENROOF_PGD_EVOL_t
USE MODD_TEB_GREENROOF_PGD_n, ONLY : TEB_GREENROOF_PGD_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
!
USE MODD_TYPE_DATE_SURF,    ONLY: DATE_TIME
!
USE MODI_CONVERT_PATCH_ISBA
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(TEB_GREENROOF_OPTIONS_t), INTENT(INOUT) :: TGRO
TYPE(TEB_GREENROOF_PGD_EVOL_t), INTENT(INOUT) :: TGRPE
TYPE(TEB_GREENROOF_PGD_t), INTENT(INOUT) :: TGRP
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
!
INTEGER, INTENT(IN) :: KLU     ! number of points
INTEGER, INTENT(IN) :: KDECADE ! number of decades
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL,             DIMENSION(KLU,1)           :: ZLAI
REAL,             DIMENSION(KLU,1)           :: ZVEG
REAL,             DIMENSION(KLU,1)           :: ZZ0
REAL,             DIMENSION(KLU,1)           :: ZALBNIR
REAL,             DIMENSION(KLU,1)           :: ZALBVIS
REAL,             DIMENSION(KLU,1)           :: ZALBUV
REAL,             DIMENSION(KLU,1)           :: ZEMIS
REAL,             DIMENSION(KLU,1)           :: ZRSMIN
REAL,             DIMENSION(KLU,1)           :: ZGAMMA
REAL,             DIMENSION(KLU,1)           :: ZWRMAX_CF
REAL,             DIMENSION(KLU,1)           :: ZRGL
REAL,             DIMENSION(KLU,1)           :: ZCV
REAL,             DIMENSION(KLU,1)           :: ZGMES
REAL,             DIMENSION(KLU,1)           :: ZBSLAI
REAL,             DIMENSION(KLU,1)           :: ZLAIMIN
REAL,             DIMENSION(KLU,1)           :: ZSEFOLD
REAL,             DIMENSION(KLU,1)           :: ZGC
REAL,             DIMENSION(KLU,1)           :: ZDMAX
REAL,             DIMENSION(KLU,1)           :: ZF2I
REAL,             DIMENSION(KLU,1)           :: ZALBNIR_VEG
REAL,             DIMENSION(KLU,1)           :: ZALBVIS_VEG
REAL,             DIMENSION(KLU,1)           :: ZALBUV_VEG
REAL,             DIMENSION(KLU,1)           :: ZALBNIR_SOIL
REAL,             DIMENSION(KLU,1)           :: ZALBVIS_SOIL
REAL,             DIMENSION(KLU,1)           :: ZALBUV_SOIL
REAL,             DIMENSION(KLU,1)           :: ZCE_NITRO
REAL,             DIMENSION(KLU,1)           :: ZCF_NITRO
REAL,             DIMENSION(KLU,1)           :: ZCNA_NITRO
REAL,             DIMENSION(KLU,1)           :: ZRE25
REAL,             DIMENSION(KLU,1)           :: ZH_TREE
REAL,             DIMENSION(KLU,TGRO%NLAYER_GR,1) :: ZROOTFRAC
REAL,             DIMENSION(KLU,TGRO%NLAYER_GR,1) :: ZDG
REAL,             DIMENSION(KLU,1)               :: ZDROOT
REAL,             DIMENSION(KLU,1)               :: ZDG2
INTEGER,          DIMENSION(KLU,1)               :: IWG_LAYER
REAL,             DIMENSION(KLU,1)           :: ZZ0_O_Z0H
REAL,             DIMENSION(KLU,1)           :: ZD_ICE
LOGICAL,          DIMENSION(KLU,1)           :: GSTRESS

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('CONVERT_PATCH_TEB_GREENROOF',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
  CALL CONVERT_PATCH_ISBA(DTCO, DTI, I, &
                          TVG%CISBA,KDECADE,KDECADE,TOP%XCOVER,TOP%LCOVER,TVG%CPHOTO,.FALSE.,  &
                        .FALSE.,TVG%LTR_ML,'GRD',PVEG=ZVEG,PLAI=ZLAI,              &
                        PRSMIN=ZRSMIN,PGAMMA=ZGAMMA,PWRMAX_CF=ZWRMAX_CF,       &
                        PRGL=ZRGL,PCV=ZCV,PSOILGRID=TGRO%XSOILGRID_GR,              &
                        PDG=ZDG,KWG_LAYER=IWG_LAYER,PDROOT=ZDROOT,PDG2=ZDG2,   &
                        PZ0=ZZ0,PZ0_O_Z0H=ZZ0_O_Z0H,                           &
                        PALBNIR_VEG=ZALBNIR_VEG,PALBVIS_VEG=ZALBVIS_VEG,       &
                        PALBUV_VEG=ZALBUV_VEG,PEMIS_ECO=ZEMIS,                 &
                        PVEGTYPE=TGRP%XVEGTYPE,PROOTFRAC=ZROOTFRAC,                 &
                        PGMES=ZGMES,PBSLAI=ZBSLAI,PLAIMIN=ZLAIMIN,             &
                        PSEFOLD=ZSEFOLD,PGC=ZGC,                               &
                        PDMAX=ZDMAX,PF2I=ZF2I,OSTRESS=GSTRESS,PH_TREE=ZH_TREE, &
                        PRE25=ZRE25,PCE_NITRO=ZCE_NITRO,PCF_NITRO=ZCF_NITRO,   &
                        PCNA_NITRO=ZCNA_NITRO,PD_ICE=ZD_ICE                    )
!
TGRPE%CUR%XVEG         = ZVEG(:,1)
TGRPE%CUR%XLAI         = ZLAI(:,1)
TGRPE%CUR%XZ0          = ZZ0(:,1)
TGRPE%CUR%XALBNIR      = ZALBNIR(:,1)
TGRPE%CUR%XALBVIS      = ZALBVIS(:,1)
TGRPE%CUR%XALBUV       = ZALBUV(:,1)
TGRPE%CUR%XEMIS        = ZEMIS(:,1)
TGRP%XRSMIN       = ZRSMIN(:,1)
TGRP%XGAMMA       = ZGAMMA(:,1)
TGRP%XWRMAX_CF    = ZWRMAX_CF(:,1)
TGRP%XRGL         = ZRGL(:,1)
TGRP%XCV          = ZCV(:,1)
TGRP%XGMES        = ZGMES(:,1)
TGRP%XBSLAI       = ZBSLAI(:,1)
TGRP%XLAIMIN      = ZLAIMIN(:,1)
TGRP%XSEFOLD      = ZSEFOLD(:,1)
TGRP%XGC          = ZGC(:,1)
TGRP%XDMAX        = ZDMAX(:,1)
TGRP%XF2I         = ZF2I(:,1)
TGRP%LSTRESS      = GSTRESS(:,1)
TGRP%XALBNIR_VEG  = ZALBNIR_VEG(:,1)
TGRP%XALBVIS_VEG  = ZALBVIS_VEG(:,1)
TGRP%XALBUV_VEG   = ZALBUV_VEG(:,1)
TGRP%XALBNIR_SOIL = ZALBNIR_SOIL(:,1)
TGRP%XALBVIS_SOIL = ZALBVIS_SOIL(:,1)
TGRP%XALBUV_SOIL  = ZALBUV_SOIL(:,1)
TGRP%XCE_NITRO    = ZCE_NITRO(:,1)
TGRP%XCF_NITRO    = ZCF_NITRO(:,1)
TGRP%XCNA_NITRO   = ZCNA_NITRO(:,1)
TGRP%XH_TREE      = ZH_TREE(:,1)
TGRP%XRE25        = ZRE25(:,1)
TGRP%XROOTFRAC    = ZROOTFRAC(:,:,1)
TGRP%XDG          = ZDG(:,:,1)
TGRP%XZ0_O_Z0H    = ZZ0_O_Z0H(:,1)
TGRP%XD_ICE       = ZD_ICE(:,1)
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CONVERT_PATCH_TEB_GREENROOF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE CONVERT_PATCH_TEB_GREENROOF
