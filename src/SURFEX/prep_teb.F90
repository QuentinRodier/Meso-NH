!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB (DTCO, UG, U, USS, IG, I, TM, GDM, GRM,GCP, &
                     HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
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
!!------------------------------------------------------------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURFEX_n, ONLY : TEB_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
!
USE MODI_PREP_HOR_TEB_FIELD
USE MODI_PREP_VER_TEB
USE MODI_PREP_OUTPUT_GRID
USE MODI_GET_LUOUT
USE MODI_PREP_TEB_CANOPY
USE MODI_PREP_TEB_GARDEN
USE MODI_PREP_TEB_GREENROOF
USE MODI_GOTO_WRAPPER_TEB_PATCH
!
USE MODN_PREP_TEB
!
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
!
USE MODD_PREP,       ONLY : XZS_LS
!
USE MODD_PREP_TEB_GARDEN, ONLY : XWSNOW_GD, XRSNOW_GD, XTSNOW_GD, XLWCSNOW_GD, &
                                 XAGESNOW_GD
!
USE MODD_PREP_TEB_GREENROOF, ONLY : XWSNOW_GR, XRSNOW_GR, XTSNOW_GR, XLWCSNOW_GR, &
                                    XAGESNOW_GR
!
USE MODD_SURF_ATM,   ONLY : LVERTSHIFT
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_CLEAN_PREP_OUTPUT_GRID
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
!*      0.2    declarations of local variables
!
INTEGER :: ILUOUT
INTEGER :: JPATCH         ! TEB patch number
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!*      1.     Default of configuration
!
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL PREP_OUTPUT_GRID(UG, U, &
                       ILUOUT,TM%TG%CGRID,TM%TG%XGRID_PAR,TM%TG%XLAT,TM%TG%XLON)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!*      2.0    Large scale orography
!
 CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'ZS     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,1)
!
!* option for roads
!
TM%TOP%CROAD_DIR = CROAD_DIR
TM%TOP%CWALL_OPT = CWALL_OPT
!
DO JPATCH=1,TM%TOP%NTEB_PATCH
  !
  CALL GOTO_WRAPPER_TEB_PATCH(TM%B, TM%DGCT, TM%DGMT, TM%T, &
                              GDM%TGD, GDM%TGDPE, GRM%TGR, GRM%TGRPE, JPATCH)
  !*      2.1    Water reservoirs
  !
  CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'WS_ROOF',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
  CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'WS_ROAD',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
  !
  !*      2.2    Building temperature
  !
  CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'TI_BLD ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
  !
  !*      2.3    Road deep temperature
  !
  CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'TI_ROAD',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
  !
  !*      2.4    Temperature profiles
  !
  CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'T_ROAD ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
  CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'T_WALLA',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
  CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'T_WALLB',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
  CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'T_ROOF ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
  !
  CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'T_WIN1 ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
  IF (TM%TOP%CBEM == 'BEM') THEN
    CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'QI_BLD ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
    CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'T_WIN2 ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
    CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'T_FLOOR',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
    CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'T_MASS ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
  ENDIF  
  !*      2.5    Snow variables
  !
  TM%T%CUR%TSNOW_ROOF%SCHEME='1-L'
  CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'SN_ROOF',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
  TM%T%CUR%TSNOW_ROAD%SCHEME='1-L'
  CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'SN_ROAD',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
  !
  !*      2.6    Canyon air variables
  !
  CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'T_CAN  ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
  CALL PREP_HOR_TEB_FIELD(TM%B, TM%BOP, DTCO, IG, U, TM%TG, TM%T, TM%TOP,GCP, &
                         HPROGRAM,'Q_CAN  ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
  !
  !-------------------------------------------------------------------------------------
  !
  !*      3.     Vertical interpolations of all variables
  !
  IF(LVERTSHIFT)THEN
    CALL PREP_VER_TEB(TM%B, TM%T, TM%TOP)
  ENDIF
  !
  !-------------------------------------------------------------------------------------
  !
  !*      4.     Urban green areas
  !
  
  IF (TM%TOP%LGARDEN)    CALL PREP_TEB_GARDEN(DTCO, UG, U, USS, IG, I, &
                                              TM%TG, TM%TOP, GDM,GCP,  &
                                              HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
  IF (TM%TOP%LGREENROOF) CALL PREP_TEB_GREENROOF(DTCO, UG, U, USS, IG, I, &
                                                 TM%TG, TM%T, TM%TOP, GDM%TVG, GRM,GCP, &
                                              HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE, JPATCH)
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
TM%TOP%LCANOPY = LTEB_CANOPY
IF (TM%TOP%LCANOPY) CALL PREP_TEB_CANOPY(TM%TCP, TM%TG)
!
DEALLOCATE(XZS_LS)
!
!-------------------------------------------------------------------------------------
 CALL CLEAN_PREP_OUTPUT_GRID
IF (LHOOK) CALL DR_HOOK('PREP_TEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_TEB
