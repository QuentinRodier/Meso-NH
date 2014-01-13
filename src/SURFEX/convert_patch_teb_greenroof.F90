!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!#############################################################
SUBROUTINE CONVERT_PATCH_TEB_GREENROOF(KLU,KDECADE)
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
!!	A. Lemonsu  *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_DATE_SURF,    ONLY: DATE_TIME
USE MODD_TEB_n,             ONLY: XCOVER
USE MODD_TEB_VEG_n,         ONLY: CISBA, CPHOTO
USE MODD_TEB_GREENROOF_n,   ONLY: LPAR_GREENROOF, LSTRESS, NLAYER_GR,      &
                                  XEMIS, XVEG, XLAI, XWRMAX_CF, XRSMIN,    &
                                  XGAMMA, XCV, XRGL,                       &
                                  XZ0, XDG2, XDROOT, NWG_LAYER,            &
                                  XGMES, XSOILGRID_GR,                     &
                                  XBSLAI, XLAIMIN, XSEFOLD,                &
                                  XF2I, XGC,                               &
                                  XCE_NITRO, XCF_NITRO, XCNA_NITRO,        &
                                  XRE25,                                   &
                                  XALBNIR_VEG, XALBVIS_VEG, XALBUV_VEG,    &
                                  XALBNIR_SOIL, XALBVIS_SOIL, XALBUV_SOIL, &
                                  XDMAX, XALBNIR, XALBVIS, XALBUV,         &
                                  XVEGTYPE,                                &
                                  XD_ICE, XDG, XH_TREE, XRE25, XROOTFRAC,  &
                                  XZ0_O_Z0H
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
REAL,             DIMENSION(KLU,NLAYER_GR,1) :: ZROOTFRAC
REAL,             DIMENSION(KLU,NLAYER_GR,1) :: ZDG
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
  CALL CONVERT_PATCH_ISBA(CISBA,KDECADE,KDECADE,XCOVER,CPHOTO,.FALSE.,.FALSE., &
                        'GRD',PVEG=ZVEG,PLAI=ZLAI,                             &
                        PRSMIN=ZRSMIN,PGAMMA=ZGAMMA,PWRMAX_CF=ZWRMAX_CF,       &
                        PRGL=ZRGL,PCV=ZCV,PSOILGRID=XSOILGRID_GR,              &
                        PDG=ZDG,KWG_LAYER=IWG_LAYER,PDROOT=ZDROOT,PDG2=ZDG2,   &
                        PZ0=ZZ0,PZ0_O_Z0H=ZZ0_O_Z0H,                           &
                        PALBNIR_VEG=ZALBNIR_VEG,PALBVIS_VEG=ZALBVIS_VEG,       &
                        PALBUV_VEG=ZALBUV_VEG,PEMIS_ECO=ZEMIS,                 &
                        PVEGTYPE=XVEGTYPE,PROOTFRAC=ZROOTFRAC,                 &
                        PGMES=ZGMES,PBSLAI=ZBSLAI,PLAIMIN=ZLAIMIN,             &
                        PSEFOLD=ZSEFOLD,PGC=ZGC,                               &
                        PDMAX=ZDMAX,PF2I=ZF2I,OSTRESS=GSTRESS,PH_TREE=ZH_TREE, &
                        PRE25=ZRE25,PCE_NITRO=ZCE_NITRO,PCF_NITRO=ZCF_NITRO,   &
                        PCNA_NITRO=ZCNA_NITRO,PD_ICE=ZD_ICE                    )
!
XVEG         = ZVEG(:,1)
XLAI         = ZLAI(:,1)
XZ0          = ZZ0(:,1)
XALBNIR      = ZALBNIR(:,1)
XALBVIS      = ZALBVIS(:,1)
XALBUV       = ZALBUV(:,1)
XEMIS        = ZEMIS(:,1)
XRSMIN       = ZRSMIN(:,1)
XGAMMA       = ZGAMMA(:,1)
XWRMAX_CF    = ZWRMAX_CF(:,1)
XRGL         = ZRGL(:,1)
XCV          = ZCV(:,1)
XGMES        = ZGMES(:,1)
XBSLAI       = ZBSLAI(:,1)
XLAIMIN      = ZLAIMIN(:,1)
XSEFOLD      = ZSEFOLD(:,1)
XGC          = ZGC(:,1)
XDMAX        = ZDMAX(:,1)
XF2I         = ZF2I(:,1)
LSTRESS      = GSTRESS(:,1)
XALBNIR_VEG  = ZALBNIR_VEG(:,1)
XALBVIS_VEG  = ZALBVIS_VEG(:,1)
XALBUV_VEG   = ZALBUV_VEG(:,1)
XALBNIR_SOIL = ZALBNIR_SOIL(:,1)
XALBVIS_SOIL = ZALBVIS_SOIL(:,1)
XALBUV_SOIL  = ZALBUV_SOIL(:,1)
XCE_NITRO    = ZCE_NITRO(:,1)
XCF_NITRO    = ZCF_NITRO(:,1)
XCNA_NITRO   = ZCNA_NITRO(:,1)
XH_TREE      = ZH_TREE(:,1)
XRE25        = ZRE25(:,1)
XROOTFRAC    = ZROOTFRAC(:,:,1)
XDG          = ZDG(:,:,1)
XZ0_O_Z0H    = ZZ0_O_Z0H(:,1)
XD_ICE       = ZD_ICE(:,1)
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CONVERT_PATCH_TEB_GREENROOF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE CONVERT_PATCH_TEB_GREENROOF
