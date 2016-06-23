!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE VEGETATION_UPDATE_GARDEN (DTCO, DTI, IG, I, T, TOP, DTGR, TGRO, GDM, &
                                         TPTIME,PTSTEP,KLU)  
!   ##########################################################################
!
!!****  *GARDEN*  
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
!!    P. Samuelsson  10/2014  Introduced MEB dummy variables in call to VEGETATION_UPDATE
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_DATA_TEB_GREENROOF_n, ONLY : DATA_TEB_GREENROOF_t
USE MODD_TEB_GREENROOF_OPTION_n, ONLY : TEB_GREENROOF_OPTIONS_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
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
TYPE(DATA_TEB_GREENROOF_t), INTENT(INOUT) :: DTGR
TYPE(TEB_GREENROOF_OPTIONS_t), INTENT(INOUT) :: TGRO
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
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
REAL, DIMENSION(KLU) :: ZAOSIP
REAL, DIMENSION(KLU) :: ZAOSIM
REAL, DIMENSION(KLU) :: ZAOSJP
REAL, DIMENSION(KLU) :: ZAOSJM
REAL, DIMENSION(KLU) :: ZHO2IP
REAL, DIMENSION(KLU) :: ZHO2IM
REAL, DIMENSION(KLU) :: ZHO2JP
REAL, DIMENSION(KLU) :: ZHO2JM
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
!
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
TYPE (DATE_TIME),  DIMENSION(KLU,1) :: TZSEED
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
IF (LHOOK) CALL DR_HOOK('VEGETATION_UPDATE_GARDEN',0,ZHOOK_HANDLE)
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
ZVEG(:,1) = GDM%TGDPE%CUR%XVEG
ZZ0(:,1) = GDM%TGDPE%CUR%XZ0
ZALBNIR(:,1) = GDM%TGDPE%CUR%XALBNIR
ZALBVIS(:,1) = GDM%TGDPE%CUR%XALBVIS
ZALBUV(:,1) = GDM%TGDPE%CUR%XALBUV
ZEMIS(:,1) = GDM%TGDPE%CUR%XEMIS
ZRSMIN(:,1) = GDM%TGDP%XRSMIN
ZGAMMA(:,1) = GDM%TGDP%XGAMMA
ZWRMAX_CF(:,1) = GDM%TGDP%XWRMAX_CF
ZRGL(:,1) = GDM%TGDP%XRGL
ZCV(:,1) = GDM%TGDP%XCV
ZGMES(:,1) = GDM%TGDP%XGMES
ZBSLAI(:,1) = GDM%TGDP%XBSLAI
ZLAIMIN(:,1) = GDM%TGDP%XLAIMIN
ZSEFOLD(:,1) = GDM%TGDP%XSEFOLD
ZGC(:,1) = GDM%TGDP%XGC
ZDMAX(:,1) = GDM%TGDP%XDMAX
ZF2I(:,1) = GDM%TGDP%XF2I
GSTRESS(:,1) = GDM%TGDP%LSTRESS
ZALBNIR_VEG(:,1) = GDM%TGDP%XALBNIR_VEG
ZALBVIS_VEG(:,1) = GDM%TGDP%XALBVIS_VEG
ZALBUV_VEG(:,1) = GDM%TGDP%XALBUV_VEG
ZALBNIR_SOIL(:,1) = GDM%TGDP%XALBNIR_SOIL
ZALBVIS_SOIL(:,1) = GDM%TGDP%XALBVIS_SOIL
ZALBUV_SOIL(:,1) = GDM%TGDP%XALBUV_SOIL
ZCE_NITRO(:,1) = GDM%TGDP%XCE_NITRO
ZCF_NITRO(:,1) = GDM%TGDP%XCF_NITRO
ZCNA_NITRO(:,1) = GDM%TGDP%XCNA_NITRO
! --------------------------------------------------------------------------------------
! Vegetation update (in case of non-interactive vegetation):
! --------------------------------------------------------------------------------------
!
GUPDATED=.FALSE.
IF (GDM%TVG%CPHOTO=='NON' .OR. GDM%TVG%CPHOTO=='AGS' .OR. GDM%TVG%CPHOTO=='AST') THEN
     CALL VEGETATION_UPDATE(DTCO, DTI, GDM%DTGD, DTGR, IG, I, TGRO, &
                            PTSTEP,TPTIME,TOP%XCOVER,TOP%LCOVER,                 &
                         GDM%TVG%CISBA,(.NOT. GDM%TGDO%LPAR_GARDEN), &
                         GDM%TVG%CPHOTO, .FALSE.,     &
                         GDM%TVG%LTR_ML, 'GRD',                                  &
                         ZLAI,ZVEG,ZZ0,                                  &
                         ZALBNIR,ZALBVIS,ZALBUV,ZEMIS,                   &
                         ZRSMIN,ZGAMMA,ZWRMAX_CF,                        &
                         ZRGL,ZCV,                                       &
                         ZGMES,ZBSLAI,ZLAIMIN,ZSEFOLD,ZGC,ZDMAX,         &
                         ZF2I, GSTRESS,                                  &
                         ZAOSIP,ZAOSIM,ZAOSJP,ZAOSJM,                    &
                         ZHO2IP,ZHO2IM,ZHO2JP,ZHO2JM,                    &
                         ZZ0EFFIP,ZZ0EFFIM,ZZ0EFFJP,ZZ0EFFJM,            &
                         GDM%TVG%CALBEDO, ZALBNIR_VEG, ZALBVIS_VEG, ZALBUV_VEG,  &
                         ZALBNIR_SOIL, ZALBVIS_SOIL, ZALBUV_SOIL,        &
                         ZCE_NITRO, ZCF_NITRO, ZCNA_NITRO,               &
                         TZSEED, TZREAP, ZWATSUP, ZIRRIG,                &
                         ZGNDLITTER,ZRGLGV,ZGAMMAGV,                     &
                         ZRSMINGV, ZWRMAX_CFGV,                          &
                         ZH_VEG, ZLAIGV, ZZ0LITTER,                      &
                         GUPDATED, OABSENT=(T%CUR%XGARDEN==0.)                 )
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GDM%TGDPE%CUR%XVEG = ZVEG(:,1)
GDM%TGDPE%CUR%XZ0 = ZZ0(:,1)
GDM%TGDPE%CUR%XALBNIR = ZALBNIR(:,1)
GDM%TGDPE%CUR%XALBVIS = ZALBVIS(:,1)
GDM%TGDPE%CUR%XALBUV = ZALBUV(:,1)
GDM%TGDPE%CUR%XEMIS = ZEMIS(:,1)
GDM%TGDP%XRSMIN = ZRSMIN(:,1)
GDM%TGDP%XGAMMA = ZGAMMA(:,1)
GDM%TGDP%XWRMAX_CF = ZWRMAX_CF(:,1)
GDM%TGDP%XRGL = ZRGL(:,1)
GDM%TGDP%XCV = ZCV(:,1)
GDM%TGDP%XGMES = ZGMES(:,1)
GDM%TGDP%XBSLAI = ZBSLAI(:,1)
GDM%TGDP%XLAIMIN = ZLAIMIN(:,1)
GDM%TGDP%XSEFOLD = ZSEFOLD(:,1)
GDM%TGDP%XGC = ZGC(:,1)
GDM%TGDP%XDMAX = ZDMAX(:,1)
GDM%TGDP%XF2I = ZF2I(:,1)
GDM%TGDP%LSTRESS = GSTRESS(:,1)
GDM%TGDP%XALBNIR_VEG = ZALBNIR_VEG(:,1)
GDM%TGDP%XALBVIS_VEG = ZALBVIS_VEG(:,1)
GDM%TGDP%XALBUV_VEG = ZALBUV_VEG(:,1)
GDM%TGDP%XALBNIR_SOIL = ZALBNIR_SOIL(:,1)
GDM%TGDP%XALBVIS_SOIL = ZALBVIS_SOIL(:,1)
GDM%TGDP%XALBUV_SOIL = ZALBUV_SOIL(:,1)
GDM%TGDP%XCE_NITRO = ZCE_NITRO(:,1)
GDM%TGDP%XCF_NITRO = ZCF_NITRO(:,1)
GDM%TGDP%XCNA_NITRO = ZCNA_NITRO(:,1)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (LHOOK) CALL DR_HOOK('VEGETATION_UPDATE_GARDEN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE VEGETATION_UPDATE_GARDEN
