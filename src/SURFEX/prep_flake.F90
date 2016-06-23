!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_FLAKE (DTCO, USS, FM, UG, U,GCP, &
                       HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!     #################################################################################
!
!!****  *PREP_FLAKE* - prepares FLAKE fields
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
!!     S. Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      S. Riette   06/2009 PREP_FLAKE_SBL has no more argument
!!      E. Kourzeneva 09/2010 (i)  Change the default initialisation,
!!                            (ii) Include the possibility to use 
!!                                 lake climate data
!!------------------------------------------------------------------
!
!
!
USE MODD_SURFEX_n, ONLY : FLAKE_MODEL_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
!
USE MODI_PREP_HOR_FLAKE_FIELD
USE MODI_PREP_VER_FLAKE
USE MODI_PREP_FLAKE_SBL
USE MODI_PREP_OUTPUT_GRID
USE MODI_GET_LUOUT
USE MODI_CLI_LAKE
!
USE MODN_PREP_FLAKE
!
USE MODD_READ_NAMELIST,ONLY : LNAM_READ
USE MODD_SURF_ATM,     ONLY : LVERTSHIFT
USE MODD_PREP,         ONLY : XZS_LS
USE MODD_PREP_FLAKE,   ONLY : LCLIM_LAKE
USE MODD_SURF_PAR,     ONLY : XUNDEF
!
!
USE MODD_CSTS,       ONLY : XTT

!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_CLEAN_PREP_OUTPUT_GRID
!
USE MODI_ABOR1_SFX
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
!
TYPE(FLAKE_MODEL_t), INTENT(INOUT) :: FM
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
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
LOGICAL :: GNOVALUE       ! if the variable is not defined
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!*      1.     Default of configuration
!
!
IF (LHOOK) CALL DR_HOOK('PREP_FLAKE',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL PREP_OUTPUT_GRID(UG, U, &
                       ILUOUT,FM%FG%CGRID,FM%FG%XGRID_PAR,FM%FG%XLAT,FM%FG%XLON)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!*      2.0    Large scale orography
!
 CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, &
                           FM%FG, FM%F,GCP, &
                           HPROGRAM,'ZS     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.1    FLake variables
!
GNOVALUE = .FALSE.
!
IF (.NOT.LCLIM_LAKE) THEN
  !
  CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, &
                           FM%FG, FM%F,GCP, &
                           HPROGRAM,'TS     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GNOVALUE)
  IF (GNOVALUE) CALL ABOR1_SFX('PREP_FLAKE: AT LEAST TS SHOULD BE GIVEN!')
  !
  CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, &
                           FM%FG, FM%F,GCP, &
                           HPROGRAM,'T_SNOW ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, &
                           FM%FG, FM%F,GCP, &
                           HPROGRAM,'T_ICE  ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, &
                           FM%FG, FM%F,GCP, &
                           HPROGRAM,'T_WML  ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  ALLOCATE(FM%F%XT_MNW(SIZE(FM%FG%XLAT)))
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, &
                           FM%FG, FM%F,GCP, &
                           HPROGRAM,'T_BOT  ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, &
                           FM%FG, FM%F,GCP, &
                           HPROGRAM,'T_B1   ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, &
                           FM%FG, FM%F,GCP, &
                           HPROGRAM,'CT     ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, &
                           FM%FG, FM%F,GCP, &
                           HPROGRAM,'H_SNOW ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, &
                           FM%FG, FM%F,GCP, &
                           HPROGRAM,'H_ICE  ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, &
                           FM%FG, FM%F,GCP, &
                           HPROGRAM,'H_ML   ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, &
                           FM%FG, FM%F,GCP, &
                           HPROGRAM,'H_B1   ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
ENDIF
!
IF (LCLIM_LAKE .OR. GNOVALUE) THEN
  IF (LCLIM_LAKE) THEN
    ALLOCATE(FM%F%XTS(SIZE(FM%FG%XLAT)))
    FM%F%XTS(:)=XUNDEF
  ENDIF
  ALLOCATE(FM%F%XT_SNOW(SIZE(FM%FG%XLAT))) 
  ALLOCATE(FM%F%XT_ICE(SIZE(FM%FG%XLAT)))  
  ALLOCATE(FM%F%XT_WML(SIZE(FM%FG%XLAT)))
  ALLOCATE(FM%F%XT_MNW(SIZE(FM%FG%XLAT))) 
  ALLOCATE(FM%F%XT_BOT(SIZE(FM%FG%XLAT)))  
  ALLOCATE(FM%F%XT_B1(SIZE(FM%FG%XLAT)))
  ALLOCATE(FM%F%XCT(SIZE(FM%FG%XLAT)))  
  ALLOCATE(FM%F%XH_SNOW(SIZE(FM%FG%XLAT)))  
  ALLOCATE(FM%F%XH_ICE(SIZE(FM%FG%XLAT)))
  ALLOCATE(FM%F%XH_ML(SIZE(FM%FG%XLAT)))
  ALLOCATE(FM%F%XH_B1(SIZE(FM%FG%XLAT)))  
  FM%F%XT_SNOW(:)=XUNDEF
  FM%F%XT_ICE(:)=XUNDEF
  FM%F%XT_WML(:)=XUNDEF
  FM%F%XT_MNW(:)=XUNDEF
  FM%F%XT_BOT(:)=XUNDEF
  FM%F%XT_B1(:)=XUNDEF
  FM%F%XCT(:)=XUNDEF
  FM%F%XH_SNOW(:)=XUNDEF
  FM%F%XH_ICE(:)=XUNDEF
  FM%F%XH_ML(:)=XUNDEF
  FM%F%XH_B1(:)=XUNDEF
ENDIF
!
!-------------------------------------------------------------------------------------
!
 CALL CLEAN_PREP_OUTPUT_GRID
!
!*      2.2    Roughness
!
ALLOCATE(FM%F%XZ0(SIZE(FM%F%XTS)))
FM%F%XZ0 = 0.001
!
!*      2.2    Friction velocity
!
ALLOCATE(FM%F%XUSTAR(SIZE(FM%F%XTS)))
FM%F%XUSTAR = 0.
!
!-------------------------------------------------------------------------------------

!
!*      3.     Vertical interpolations of all variables
!
IF(.NOT.LCLIM_LAKE) THEN
  IF (LVERTSHIFT)THEN    
    CALL PREP_VER_FLAKE(FM%F)
    WRITE(ILUOUT,*) "WARNING: You want the vertical shift for lakes?"
    WRITE(ILUOUT,*) "WARNING: Vertical shift for the lake temperature profile is impossible!"
    WRITE(ILUOUT,*) "WARNING: So, set the default vertical profiles from the shifted surface temperature."    !
    GNOVALUE=.TRUE.
  ENDIF
END IF
!
DEALLOCATE(XZS_LS)
!-------------------------------------------------------------------------------------
!
!*      4.    Compute T_MNW and give the default profile if needed 
!              or read data from climate files 
!
IF (LCLIM_LAKE) THEN
 CALL CLI_LAKE(FM%FG, FM%F)
ELSEIF (.NOT.GNOVALUE) THEN
  FM%F%XT_MNW(:)=FM%F%XT_WML(:)-(FM%F%XT_WML(:)-FM%F%XT_BOT(:))*(1.-FM%F%XH_ML(:)/FM%F%XWATER_DEPTH(:))*FM%F%XCT(:)
ELSE
  WRITE(ILUOUT,*) "WARNING! One of the lake profile variales was not indicated, so set the default profile!"
  FM%F%XT_WML=MAX(FM%F%XTS(:),XTT)  
  FM%F%XT_SNOW=MIN(FM%F%XTS(:),XTT)
  FM%F%XT_ICE=MIN(FM%F%XTS(:),XTT)
  FM%F%XH_B1=0.0 
  FM%F%XCT=0.5   
  FM%F%XH_SNOW=0.0   
  WHERE (FM%F%XTS <= XTT)
   FM%F%XT_BOT=XTT+4.
   FM%F%XT_B1=XTT+3.9
   FM%F%XH_ICE=0.01
   FM%F%XH_ML=FM%F%XWATER_DEPTH/2.
   FM%F%XT_MNW=FM%F%XT_WML-(FM%F%XT_WML-FM%F%XT_BOT)*(1.-FM%F%XH_ML/FM%F%XWATER_DEPTH)*FM%F%XCT
  ELSEWHERE
   FM%F%XT_BOT=FM%F%XTS
   FM%F%XT_B1=FM%F%XTS-0.1
   FM%F%XH_ICE=0.0
   FM%F%XH_ML=FM%F%XWATER_DEPTH
   FM%F%XT_MNW=FM%F%XTS 
  END WHERE
END IF
!
!-------------------------------------------------------------------------------------
!
!*      6.     Preparation of SBL air variables
!
FM%F%LSBL = LWAT_SBL
IF (FM%F%LSBL) CALL PREP_FLAKE_SBL(FM%FG, FM%FSB)
IF (LHOOK) CALL DR_HOOK('PREP_FLAKE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_FLAKE
