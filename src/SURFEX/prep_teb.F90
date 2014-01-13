!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB(HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
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
USE MODI_PREP_HOR_TEB_FIELD
USE MODI_PREP_VER_TEB
USE MODI_PREP_OUTPUT_GRID
USE MODI_GET_LUOUT
USE MODI_PREP_TEB_CANOPY
USE MODI_PREP_TEB_GARDEN
USE MODI_PREP_TEB_GREENROOF
USE MODI_GOTO_TEB
!
USE MODN_PREP_TEB
!
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
USE MODD_PREP,       ONLY : XZS_LS
USE MODD_SURF_ATM,   ONLY : LVERTSHIFT
USE MODD_TEB_n,      ONLY : TSNOW_ROOF,TSNOW_ROAD, NTEB_PATCH,              &
                            LCANOPY, LGARDEN, LGREENROOF,                   &
                            CBEM,                                           &
                            CROAD_DIR_n=>CROAD_DIR, CWALL_OPT_n=>CWALL_OPT
USE MODD_TEB_GRID_n, ONLY : CGRID, XGRID_PAR, XLAT, XLON
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
 CALL PREP_OUTPUT_GRID(ILUOUT,CGRID,XGRID_PAR,XLAT,XLON)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!*      2.0    Large scale orography
!
 CALL PREP_HOR_TEB_FIELD(HPROGRAM,'ZS     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!* option for roads
!
CROAD_DIR_n = CROAD_DIR
CWALL_OPT_n = CWALL_OPT
!
DO JPATCH=1,NTEB_PATCH
  !
  CALL GOTO_TEB(JPATCH)
  !*      2.1    Water reservoirs
  !
  CALL PREP_HOR_TEB_FIELD(HPROGRAM,'WS_ROOF',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  CALL PREP_HOR_TEB_FIELD(HPROGRAM,'WS_ROAD',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  !
  !*      2.2    Building temperature
  !
  CALL PREP_HOR_TEB_FIELD(HPROGRAM,'TI_BLD ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  !
  !*      2.3    Road deep temperature
  !
  CALL PREP_HOR_TEB_FIELD(HPROGRAM,'TI_ROAD',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  !
  !*      2.4    Temperature profiles
  !
  CALL PREP_HOR_TEB_FIELD(HPROGRAM,'T_ROAD ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  CALL PREP_HOR_TEB_FIELD(HPROGRAM,'T_WALLA',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  CALL PREP_HOR_TEB_FIELD(HPROGRAM,'T_WALLB',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  CALL PREP_HOR_TEB_FIELD(HPROGRAM,'T_ROOF ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  !
  CALL PREP_HOR_TEB_FIELD(HPROGRAM,'T_WIN1 ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  IF (CBEM == 'BEM') THEN
    CALL PREP_HOR_TEB_FIELD(HPROGRAM,'QI_BLD ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
    CALL PREP_HOR_TEB_FIELD(HPROGRAM,'T_WIN2 ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
    CALL PREP_HOR_TEB_FIELD(HPROGRAM,'T_FLOOR',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
    CALL PREP_HOR_TEB_FIELD(HPROGRAM,'T_MASS ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  ENDIF  
  !*      2.5    Snow variables
  !
  TSNOW_ROOF%SCHEME='1-L'
  CALL PREP_HOR_TEB_FIELD(HPROGRAM,'SN_ROOF',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  TSNOW_ROAD%SCHEME='1-L'
  CALL PREP_HOR_TEB_FIELD(HPROGRAM,'SN_ROAD',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  !
  !*      2.6    Canyon air variables
  !
  CALL PREP_HOR_TEB_FIELD(HPROGRAM,'T_CAN  ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  CALL PREP_HOR_TEB_FIELD(HPROGRAM,'Q_CAN  ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  !
  !-------------------------------------------------------------------------------------
  !
  !*      3.     Vertical interpolations of all variables
  !
  IF(LVERTSHIFT)THEN
    CALL PREP_VER_TEB
  ENDIF
  !
  !-------------------------------------------------------------------------------------
  !
  !*      4.     Urban green areas
  !
  IF (LGARDEN)    CALL PREP_TEB_GARDEN   (HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  IF (LGREENROOF) CALL PREP_TEB_GREENROOF(HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  !  
ENDDO
!-------------------------------------------------------------------------------------
!
!*      5.     Preparation of canopy air variables
!
LCANOPY = LTEB_CANOPY
IF (LCANOPY) CALL PREP_TEB_CANOPY()
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
