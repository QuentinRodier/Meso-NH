!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
    SUBROUTINE VEGETATION_UPDATE_GREENROOF(TPTIME,PTSTEP,KLU)  
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
!!	A. Lemonsu          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!    Original    05/2009
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TYPE_DATE_SURF,    ONLY: DATE_TIME
USE MODD_TEB_n,             ONLY: XCOVER
USE MODD_TEB_VEG_n,         ONLY: CISBA, CPHOTO, CALBEDO
USE MODD_TEB_GREENROOF_n,   ONLY: LPAR_GREENROOF, LSTRESS,                 &
                                  XEMIS, XVEG, XLAI, XWRMAX_CF, XRSMIN,    &
                                  XGAMMA, XCV, XRGL,                       &
                                  XZ0,                                     &
                                  XGMES,                                   &
                                  XBSLAI, XLAIMIN, XSEFOLD,                &
                                  XF2I, XGC,                               &
                                  XCE_NITRO, XCF_NITRO, XCNA_NITRO,        &
                                  XRE25,                                   &
                                  XALBNIR_VEG, XALBVIS_VEG, XALBUV_VEG,    &
                                  XALBNIR_SOIL, XALBVIS_SOIL, XALBUV_SOIL, &
                                  XALBNIR, XALBVIS, XALBUV, XDMAX


  
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
TYPE (DATE_TIME), DIMENSION(KLU,1) :: TZSEED
TYPE (DATE_TIME), DIMENSION(KLU,1) :: TZREAP
REAL, DIMENSION(KLU,1) :: ZWATSUP
REAL, DIMENSION(KLU,1) :: ZIRRIG
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
ZVEG(:,1)         = XVEG
ZZ0(:,1)          = XZ0
ZALBNIR(:,1)      = XALBNIR
ZALBVIS(:,1)      = XALBVIS
ZALBUV(:,1)       = XALBUV
ZEMIS(:,1)        = XEMIS
ZRSMIN(:,1)       = XRSMIN
ZGAMMA(:,1)       = XGAMMA
ZWRMAX_CF(:,1)    = XWRMAX_CF
ZRGL(:,1)         = XRGL
ZCV(:,1)          = XCV
ZGMES(:,1)        = XGMES
ZBSLAI(:,1)       = XBSLAI
ZLAIMIN(:,1)      = XLAIMIN
ZSEFOLD(:,1)      = XSEFOLD
ZGC(:,1)          = XGC
ZDMAX(:,1)        = XDMAX
ZF2I(:,1)         = XF2I
GSTRESS(:,1)      = LSTRESS
ZALBNIR_VEG(:,1)  = XALBNIR_VEG
ZALBVIS_VEG(:,1)  = XALBVIS_VEG
ZALBUV_VEG(:,1)   = XALBUV_VEG
ZALBNIR_SOIL(:,1) = XALBNIR_SOIL
ZALBVIS_SOIL(:,1) = XALBVIS_SOIL
ZALBUV_SOIL(:,1)  = XALBUV_SOIL
ZCE_NITRO(:,1)    = XCE_NITRO
ZCF_NITRO(:,1)    = XCF_NITRO
ZCNA_NITRO(:,1)   = XCNA_NITRO
! --------------------------------------------------------------------------------------
! Vegetation update (in case of non-interactive vegetation):
! --------------------------------------------------------------------------------------
!
IF (CPHOTO=='NON' .OR. CPHOTO=='AGS' .OR. CPHOTO=='AST') THEN
     CALL VEGETATION_UPDATE(PTSTEP,TPTIME,XCOVER,                        &
                         CISBA,(.NOT. LPAR_GREENROOF), CPHOTO, .FALSE., 'GR ',  &
                         ZLAI,ZVEG,ZZ0,                                  &
                         ZALBNIR,ZALBVIS,ZALBUV,ZEMIS,                   &
                         ZRSMIN,ZGAMMA,ZWRMAX_CF,                        &
                         ZRGL,ZCV,                                       &
                         ZGMES,ZBSLAI,ZLAIMIN,ZSEFOLD,ZGC,ZDMAX,         &
                         ZF2I, GSTRESS,                                  &
                         ZAOSIP,ZAOSIM,ZAOSJP,ZAOSJM,                    &
                         ZHO2IP,ZHO2IM,ZHO2JP,ZHO2JM,                    &
                         ZZ0EFFIP,ZZ0EFFIM,ZZ0EFFJP,ZZ0EFFJM,            &
                         CALBEDO, ZALBNIR_VEG, ZALBVIS_VEG, ZALBUV_VEG,  &
                         ZALBNIR_SOIL, ZALBVIS_SOIL, ZALBUV_SOIL,        &
                         ZCE_NITRO, ZCF_NITRO, ZCNA_NITRO,               &
                         TZSEED, TZREAP, ZWATSUP, ZIRRIG                 )  
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XVEG         = ZVEG(:,1)
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
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (LHOOK) CALL DR_HOOK('VEGETATION_UPDATE_GREENROOF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE VEGETATION_UPDATE_GREENROOF
