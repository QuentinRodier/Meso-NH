!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB (DTCO, UG, U, USS, GCP, TOP, BOP, NB, TG, SB, NT, GDM, GRM, HM, &
                     NPAR_VEG_IRR_USE, HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
!     #################################################################################
!
!!****  *PREP_TEB* - prepares TEB fields
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      S. Riette   06/2009 PREP_TEB_CANOPY has no more argument
!!      P. Marguinaud10/2014, Support for a 2-part PREP
!!      A. Druel     02/2019, Transmit NPAR_VEG_IRR_USE for irrigation
!!
!!------------------------------------------------------------------
!
USE MODD_DATA_COVER_n,     ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n,  ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n,       ONLY : SURF_ATM_t
USE MODD_SSO_n,            ONLY : SSO_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_TEB_OPTION_n,     ONLY : TEB_OPTIONS_t
USE MODD_BEM_OPTION_n,     ONLY : BEM_OPTIONS_t
USE MODD_BEM_n,            ONLY : BEM_NP_t
USE MODD_SFX_GRID_n,       ONLY : GRID_t
USE MODD_CANOPY_n,         ONLY : CANOPY_t
USE MODD_TEB_n,            ONLY : TEB_NP_t
USE MODD_SURFEX_n,         ONLY : TEB_GARDEN_MODEL_t, TEB_GREENROOF_MODEL_t
USE MODD_SURFEX_n,        ONLY : TEB_HYDRO_MODEL_t
!
USE MODE_PREP_CTL,         ONLY : PREP_CTL
!
USE MODI_PREP_HOR_TEB_FIELD
USE MODI_PREP_VER_TEB
USE MODI_PREP_OUTPUT_GRID
USE MODI_GET_LUOUT
USE MODI_PREP_SBL
USE MODI_PREP_TEB_GARDEN
USE MODI_PREP_TEB_GREENROOF
USE MODI_PREP_TEB_HYDRO
!
USE MODN_PREP_TEB
!
USE MODD_PREP,             ONLY : XZS_LS
USE MODD_PREP_TEB_GARDEN,  ONLY : XWSNOW_GD, XRSNOW_GD, XTSNOW_GD, XLWCSNOW_GD, &
                                  XAGESNOW_GD
USE MODD_PREP_TEB_GREENROOF, ONLY : XWSNOW_GR, XRSNOW_GR, XTSNOW_GR, XLWCSNOW_GR, &
                                    XAGESNOW_GR
!
USE MODD_SURF_ATM,         ONLY : LVERTSHIFT
USE MODD_SURF_PAR,         ONLY: NFILENAMELGTMAX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_CLEAN_PREP_OUTPUT_GRID
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t),          INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t),       INTENT(INOUT) :: UG
TYPE(SURF_ATM_t),            INTENT(INOUT) :: U
TYPE(SSO_t),                 INTENT(INOUT) :: USS
TYPE(GRID_CONF_PROJ_t),      INTENT(INOUT) :: GCP
!
TYPE(TEB_OPTIONS_t),         INTENT(INOUT) :: TOP
TYPE(BEM_OPTIONS_t),         INTENT(INOUT) :: BOP
TYPE(BEM_NP_t),              INTENT(INOUT) :: NB
TYPE(GRID_t),                INTENT(INOUT) :: TG
TYPE(CANOPY_t),              INTENT(INOUT) :: SB
TYPE(TEB_NP_t),              INTENT(INOUT) :: NT
TYPE(TEB_GARDEN_MODEL_t),    INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
TYPE(TEB_HYDRO_MODEL_t),     INTENT(INOUT) :: HM
!
TYPE (PREP_CTL),    INTENT(INOUT) :: YDCTL
!
INTEGER, DIMENSION(:),            INTENT(IN)  :: NPAR_VEG_IRR_USE ! vegtype with irrigation
 CHARACTER(LEN=6),                INTENT(IN)  :: HPROGRAM    ! program calling surf. schemes
 CHARACTER(LEN=NFILENAMELGTMAX),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),                INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=NFILENAMELGTMAX),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),                INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
!*      0.2    declarations of local variables
!
INTEGER :: ILUOUT
INTEGER :: JP         ! TEB patch number
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!*      1.     Default of configuration
!
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL PREP_OUTPUT_GRID(UG%G, TG, U%NSIZE_FULL, ILUOUT)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!
!* option for roads
!
TOP%CROAD_DIR = CROAD_DIR
TOP%CWALL_OPT = CWALL_OPT
!
DO JP=1,TOP%NTEB_PATCH
  !
  !*      2.0    Large scale orography
  !
  IF (JP==1) &
    CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                            HPROGRAM,'ZS     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,1,NPAR_VEG_IRR_USE,YDCTL)


  !*      2.1    Water reservoirs
  !
  CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'WS_ROOF',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
  CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'WS_ROAD',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
  !
  !*      2.2    Building temperature
  !
  CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'TI_BLD ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
  !
  !*      2.3    TEB deep temperature
  !
  CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'TDEEP_T',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
  !
  !*      2.4    Temperature profiles
  !
  CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'T_ROAD ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
  CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'T_WALLA',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
  CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'T_WALLB',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
  CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'T_ROOF ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
    CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'T_BLD  ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
!
  CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'T_WIN1 ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
  IF (TOP%CBEM == 'BEM') THEN
    CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'QI_BLD ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
    CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'T_WIN2 ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
    CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'T_FLOOR',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
    CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'T_MASS ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
    CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'PSOLD  ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
    CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'VENTNIG',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
    CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'SHADVAC',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)  
ENDIF  
  !*      2.5    Snow variables
  !
  NT%AL(JP)%TSNOW_ROOF%SCHEME='1-L'
  CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'SN_ROOF',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
  NT%AL(JP)%TSNOW_ROAD%SCHEME='1-L'
  CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'SN_ROAD',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
  !
  !*      2.6    Canyon air variables
  !
  CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'T_CAN  ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
  CALL PREP_HOR_TEB_FIELD(NB%AL(JP), BOP, DTCO, U, GCP, TG, NT%AL(JP), TOP, &
                         HPROGRAM,'Q_CAN  ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,NPAR_VEG_IRR_USE,YDCTL)
  !
  !-------------------------------------------------------------------------------------
  !
  !*      3.     Vertical interpolations of all variables
  !
  IF(LVERTSHIFT)THEN
    CALL PREP_VER_TEB(NB%AL(JP), NT%AL(JP), TOP%XZS, TOP%CBEM)
  ENDIF
  !
  !-------------------------------------------------------------------------------------
  !
  !*      4.     Urban green areas
  !
  IF (TOP%LGARDEN) THEN
    CALL PREP_TEB_GARDEN(DTCO, UG, U, USS,  GCP, TG, TOP, &
                                           GDM%O, GDM%S, GDM%K, GDM%P, GDM%NPE%AL(JP), GDM%PHV, GDM%NPEHV%AL(JP), &
                                              NPAR_VEG_IRR_USE, HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,YDCTL)
    GDM%K%XTDEEP(:) = NT%AL(JP)%XTDEEP_TEB(:)
  ENDIF
  IF (TOP%LGREENROOF) THEN
     CALL PREP_TEB_GREENROOF(DTCO, UG, U, USS,  GCP, TG, TOP, GRM%O, GRM%S, GRM%K, GRM%P, GRM%NPE%AL(JP), &
                             NPAR_VEG_IRR_USE,                                                            &
                             HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JP,YDCTL)
    !
    ! Initializing deep GR temp. with that of the outer layer of the structural roof 
    !
    GRM%K%XTDEEP(:) = NT%AL(JP)%XT_ROOF(:,1)  
    !
  ENDIF
  !
  !-------------------------------------------------------------------------------------
  !
  !*      4.     Urban Hydrology
  !
  !
  IF (TOP%LGARDEN.AND.TOP%LURBHYDRO)  CALL PREP_TEB_HYDRO( GDM%NPE%AL(JP), HM%NTH%AL(JP) )
  !
ENDDO
!
DEALLOCATE(XWSNOW_GD,XRSNOW_GD,XTSNOW_GD,XLWCSNOW_GD,XAGESNOW_GD)
DEALLOCATE(XWSNOW_GR,XRSNOW_GR,XTSNOW_GR,XLWCSNOW_GR,XAGESNOW_GR)
!
!-------------------------------------------------------------------------------------
!
!*      5.     Preparation of canopy air variables
!
TOP%LCANOPY = LTEB_CANOPY
IF (TOP%LCANOPY) CALL PREP_SBL(TG%NDIM, SB)
!
IF (LATM_CANOPY.AND..NOT.TOP%LCANOPY) THEN
   CALL ABOR1_SFX("PREP_TEB: TEB-Canopy must be activated for multi-level coupling")
ENDIF
!
TOP%LATM_CANOPY = LATM_CANOPY
!
IF (YDCTL%LPART5) THEN
  DEALLOCATE(XZS_LS)
ENDIF
!
!-------------------------------------------------------------------------------------
 CALL CLEAN_PREP_OUTPUT_GRID
IF (LHOOK) CALL DR_HOOK('PREP_TEB',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_TEB
