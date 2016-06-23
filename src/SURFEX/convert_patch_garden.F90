!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE CONVERT_PATCH_GARDEN (DTCO, DTI, I, TGDO, TGDPE, TGDP, TOP, TVG, &
                                 KLU,KDECADE)
!#############################################################
!
!!****  *CONVERT_PATCH_GARDEN* - routine to initialize GARDEN parameters 
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
USE MODD_TEB_GARDEN_OPTION_n, ONLY : TEB_GARDEN_OPTIONS_t
USE MODD_TEB_GARDEN_PGD_EVOL_n, ONLY : TEB_GARDEN_PGD_EVOL_t
USE MODD_TEB_GARDEN_PGD_n, ONLY : TEB_GARDEN_PGD_t
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
TYPE(TEB_GARDEN_OPTIONS_t), INTENT(INOUT) :: TGDO
TYPE(TEB_GARDEN_PGD_EVOL_t), INTENT(INOUT) :: TGDPE
TYPE(TEB_GARDEN_PGD_t), INTENT(INOUT) :: TGDP
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
REAL,             DIMENSION(KLU,1)               :: ZLAI
REAL,             DIMENSION(KLU,1)               :: ZVEG
REAL,             DIMENSION(KLU,1)               :: ZZ0
REAL,             DIMENSION(KLU,1)               :: ZEMIS
REAL,             DIMENSION(KLU,1)               :: ZRSMIN
REAL,             DIMENSION(KLU,1)               :: ZGAMMA
REAL,             DIMENSION(KLU,1)               :: ZWRMAX_CF
REAL,             DIMENSION(KLU,1)               :: ZRGL
REAL,             DIMENSION(KLU,1)               :: ZCV
REAL,             DIMENSION(KLU,1)               :: ZGMES
REAL,             DIMENSION(KLU,1)               :: ZBSLAI
REAL,             DIMENSION(KLU,1)               :: ZLAIMIN
REAL,             DIMENSION(KLU,1)               :: ZSEFOLD
REAL,             DIMENSION(KLU,1)               :: ZGC
REAL,             DIMENSION(KLU,1)               :: ZDMAX
REAL,             DIMENSION(KLU,1)               :: ZF2I
REAL,             DIMENSION(KLU,1)               :: ZALBNIR_VEG
REAL,             DIMENSION(KLU,1)               :: ZALBVIS_VEG
REAL,             DIMENSION(KLU,1)               :: ZALBUV_VEG
REAL,             DIMENSION(KLU,1)               :: ZCE_NITRO
REAL,             DIMENSION(KLU,1)               :: ZCF_NITRO
REAL,             DIMENSION(KLU,1)               :: ZCNA_NITRO
REAL,             DIMENSION(KLU,1)               :: ZRE25
REAL,             DIMENSION(KLU,1)               :: ZH_TREE
REAL,             DIMENSION(KLU,1)               :: ZZ0_O_Z0H
REAL,             DIMENSION(KLU,1)               :: ZD_ICE
REAL,             DIMENSION(KLU,TGDO%NGROUND_LAYER,1) :: ZROOTFRAC
REAL,             DIMENSION(KLU,TGDO%NGROUND_LAYER,1) :: ZDG
REAL,             DIMENSION(KLU,1)               :: ZDROOT
REAL,             DIMENSION(KLU,1)               :: ZDG2
INTEGER,          DIMENSION(KLU,1)               :: IWG_LAYER
LOGICAL,          DIMENSION(KLU,1)               :: GSTRESS
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('CONVERT_PATCH_GARDEN',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
  CALL CONVERT_PATCH_ISBA(DTCO, DTI, I, &
                          TVG%CISBA,KDECADE,KDECADE,TOP%XCOVER,TOP%LCOVER,TVG%CPHOTO,.FALSE.,  &
                        .FALSE.,TVG%LTR_ML,'GRD',PVEG=ZVEG,PLAI=ZLAI,              &
                        PRSMIN=ZRSMIN,PGAMMA=ZGAMMA,PWRMAX_CF=ZWRMAX_CF,       &
                        PRGL=ZRGL,PCV=ZCV,PSOILGRID=TGDO%XSOILGRID,                 &
                        PDG=ZDG,KWG_LAYER=IWG_LAYER,PDROOT=ZDROOT,PDG2=ZDG2,   &
                        PZ0=ZZ0,PZ0_O_Z0H=ZZ0_O_Z0H,                           &
                        PALBNIR_VEG=ZALBNIR_VEG,PALBVIS_VEG=ZALBVIS_VEG,       &
                        PALBUV_VEG=ZALBUV_VEG,PEMIS_ECO=ZEMIS,                 &
                        PVEGTYPE=TGDP%XVEGTYPE,PROOTFRAC=ZROOTFRAC,                 &
                        PGMES=ZGMES,PBSLAI=ZBSLAI,PLAIMIN=ZLAIMIN,             &
                        PSEFOLD=ZSEFOLD,PGC=ZGC,                               &
                        PDMAX=ZDMAX,PF2I=ZF2I,OSTRESS=GSTRESS,PH_TREE=ZH_TREE, &
                        PRE25=ZRE25,PCE_NITRO=ZCE_NITRO,PCF_NITRO=ZCF_NITRO,   &
                        PCNA_NITRO=ZCNA_NITRO,PD_ICE=ZD_ICE                    )
!
TGDPE%CUR%XVEG         = ZVEG(:,1)
TGDPE%CUR%XLAI         = ZLAI(:,1)
TGDPE%CUR%XZ0          = ZZ0(:,1)
TGDPE%CUR%XEMIS        = ZEMIS(:,1)
TGDP%XRSMIN       = ZRSMIN(:,1)
TGDP%XGAMMA       = ZGAMMA(:,1)
TGDP%XWRMAX_CF    = ZWRMAX_CF(:,1)
TGDP%XRGL         = ZRGL(:,1)
TGDP%XCV          = ZCV(:,1)
TGDP%XGMES        = ZGMES(:,1)
TGDP%XBSLAI       = ZBSLAI(:,1)
TGDP%XLAIMIN      = ZLAIMIN(:,1)
TGDP%XSEFOLD      = ZSEFOLD(:,1)
TGDP%XGC          = ZGC(:,1)
TGDP%XDMAX        = ZDMAX(:,1)
TGDP%XF2I         = ZF2I(:,1)
TGDP%LSTRESS      = GSTRESS(:,1)
TGDP%XALBNIR_VEG  = ZALBNIR_VEG(:,1)
TGDP%XALBVIS_VEG  = ZALBVIS_VEG(:,1)
TGDP%XALBUV_VEG   = ZALBUV_VEG(:,1)
TGDP%XCE_NITRO    = ZCE_NITRO(:,1)
TGDP%XCF_NITRO    = ZCF_NITRO(:,1)
TGDP%XCNA_NITRO   = ZCNA_NITRO(:,1)
TGDP%XH_TREE      = ZH_TREE(:,1)
TGDP%XRE25        = ZRE25(:,1)
TGDP%XROOTFRAC    = ZROOTFRAC(:,:,1)
TGDP%XDG          = ZDG(:,:,1)
TGDP%XDROOT       = ZDROOT(:,1)
TGDP%XDG2         = ZDG2(:,1)
TGDP%NWG_LAYER    = IWG_LAYER(:,1)
TGDP%XZ0_O_Z0H    = ZZ0_O_Z0H(:,1)
TGDP%XD_ICE       = ZD_ICE(:,1)
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CONVERT_PATCH_GARDEN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE CONVERT_PATCH_GARDEN
