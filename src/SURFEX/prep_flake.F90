!     #########
SUBROUTINE PREP_FLAKE(HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
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
USE MODD_FLAKE_n,      ONLY : XTS, XT_SNOW, XT_ICE, XT_MNW, XT_WML, XT_BOT, XT_B1,   &
                                XCT, XH_SNOW, XH_ICE, XH_ML, XH_B1, &
                                XWATER_DEPTH, LSBL, XZ0, XUSTAR  
!
USE MODD_FLAKE_GRID_n, ONLY : CGRID, XGRID_PAR, XLAT, XLON
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
 CALL PREP_OUTPUT_GRID(ILUOUT,CGRID,XGRID_PAR,XLAT,XLON)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!*      2.0    Large scale orography
!
 CALL PREP_HOR_FLAKE_FIELD(HPROGRAM,'ZS     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.1    FLake variables
!
GNOVALUE = .FALSE.
!
IF (.NOT.LCLIM_LAKE) THEN
  !
  CALL PREP_HOR_FLAKE_FIELD(HPROGRAM,'TS     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GNOVALUE)
  IF (GNOVALUE) CALL ABOR1_SFX('PREP_FLAKE: AT LEAST TS SHOULD BE GIVEN!')
  !
  CALL PREP_HOR_FLAKE_FIELD(HPROGRAM,'T_SNOW ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(HPROGRAM,'T_ICE  ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(HPROGRAM,'T_WML  ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  ALLOCATE(XT_MNW(SIZE(XLAT)))
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(HPROGRAM,'T_BOT  ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(HPROGRAM,'T_B1   ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(HPROGRAM,'CT     ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(HPROGRAM,'H_SNOW ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(HPROGRAM,'H_ICE  ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(HPROGRAM,'H_ML   ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(HPROGRAM,'H_B1   ',HATMFILE,HATMFILETYPE,&
        HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
ENDIF
!
IF (LCLIM_LAKE .OR. GNOVALUE) THEN
  IF (LCLIM_LAKE) THEN
    ALLOCATE(XTS(SIZE(XLAT)))
    XTS(:)=XUNDEF
  ENDIF
  ALLOCATE(XT_SNOW(SIZE(XLAT))) 
  ALLOCATE(XT_ICE(SIZE(XLAT)))  
  ALLOCATE(XT_WML(SIZE(XLAT)))
  ALLOCATE(XT_MNW(SIZE(XLAT))) 
  ALLOCATE(XT_BOT(SIZE(XLAT)))  
  ALLOCATE(XT_B1(SIZE(XLAT)))
  ALLOCATE(XCT(SIZE(XLAT)))  
  ALLOCATE(XH_SNOW(SIZE(XLAT)))  
  ALLOCATE(XH_ICE(SIZE(XLAT)))
  ALLOCATE(XH_ML(SIZE(XLAT)))
  ALLOCATE(XH_B1(SIZE(XLAT)))  
  XT_SNOW(:)=XUNDEF
  XT_ICE(:)=XUNDEF
  XT_WML(:)=XUNDEF
  XT_MNW(:)=XUNDEF
  XT_BOT(:)=XUNDEF
  XT_B1(:)=XUNDEF
  XCT(:)=XUNDEF
  XH_SNOW(:)=XUNDEF
  XH_ICE(:)=XUNDEF
  XH_ML(:)=XUNDEF
  XH_B1(:)=XUNDEF
ENDIF
!
!-------------------------------------------------------------------------------------
!
 CALL CLEAN_PREP_OUTPUT_GRID
!
!*      2.2    Roughness
!
ALLOCATE(XZ0(SIZE(XTS)))
XZ0 = 0.001
!
!*      2.2    Friction velocity
!
ALLOCATE(XUSTAR(SIZE(XTS)))
XUSTAR = 0.
!
!-------------------------------------------------------------------------------------

!
!*      3.     Vertical interpolations of all variables
!
IF(.NOT.LCLIM_LAKE) THEN
  IF (LVERTSHIFT)THEN    
    CALL PREP_VER_FLAKE
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
 CALL CLI_LAKE
ELSEIF (.NOT.GNOVALUE) THEN
  XT_MNW(:)=XT_WML(:)-(XT_WML(:)-XT_BOT(:))*(1.-XH_ML(:)/XWATER_DEPTH(:))*XCT(:)
ELSE
  WRITE(ILUOUT,*) "WARNING! One of the lake profile variales was not indicated, so set the default profile!"
  XT_WML=MAX(XTS(:),XTT)  
  XT_SNOW=MIN(XTS(:),XTT)
  XT_ICE=MIN(XTS(:),XTT)
  XH_B1=0.0 
  XCT=0.5   
  XH_SNOW=0.0   
  WHERE (XTS <= XTT)
   XT_BOT=XTT+4.
   XT_B1=XTT+3.9
   XH_ICE=0.01
   XH_ML=XWATER_DEPTH/2.
   XT_MNW=XT_WML-(XT_WML-XT_BOT)*(1.-XH_ML/XWATER_DEPTH)*XCT
  ELSEWHERE
   XT_BOT=XTS
   XT_B1=XTS-0.1
   XH_ICE=0.0
   XH_ML=XWATER_DEPTH
   XT_MNW=XTS 
  END WHERE
END IF
!
!-------------------------------------------------------------------------------------
!
!*      6.     Preparation of SBL air variables
!
LSBL = LWAT_SBL
IF (LSBL) CALL PREP_FLAKE_SBL()
IF (LHOOK) CALL DR_HOOK('PREP_FLAKE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_FLAKE
