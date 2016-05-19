!     #############################################################
SUBROUTINE INIT_FLAKE_n(HPROGRAM,HINIT,                            &
                          KI,KSV,KSW,                                &
                          HSV,PCO2,PRHOA,                            &
                          PZENITH,PAZIM,PSW_BANDS,PDIR_ALB,PSCA_ALB, &
                          PEMIS,PTSRAD,                              &
                          KYEAR, KMONTH,KDAY, PTIME,                 &
                          HATMFILE,HATMFILETYPE,                     &
                          HTEST                                     )   
!     #############################################################
!
!!****  *INIT_FLAKE_n* - routine to initialize FLAKE model
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003
!!      B. Decharme    07/11 : read pgd+prep
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS,           ONLY : XTT, XPI, XOMEGA 
USE MODD_WATER_PAR,      ONLY : XALBWAT, XEMISWAT
!?? USE MODD_SNOW_PAR,       ONLY : XANSMAX, XEMISSN
USE MODD_FLAKE_GRID_n,  ONLY : XLAT
USE MODD_FLAKE_n,  ONLY : XCOVER          , TTIME         , XTSTEP        , &
                            XOUT_TSTEP    , XEMIS         , XWATER_DEPTH  , &
                            XWATER_FETCH  , XT_BS         , XDEPTH_BS     , &
                            XCORIO        , XDIR_ALB      , XSCA_ALB      , &
                            XICE_ALB      , XSNOW_ALB     , XEXTCOEF_WATER, &
                            XEXTCOEF_ICE  , XEXTCOEF_SNOW , XT_SNOW       , &
                            XT_ICE        , XT_MNW        , XT_WML        , &
                            XT_BOT        , XT_B1         , XCT           , &
                            XH_SNOW       , XH_ICE        , XH_ML         , &
                            XH_B1         , XTS           , LSEDIMENTS    , &
                            CSNOW_FLK     , CFLK_FLUX     , CFLK_ALB      , &
                            LSBL          , XICHCE        , LPRECIP       , &
                            LPWEBB



USE MODD_DIAG_FLAKE_n, ONLY : N2M, LSURF_BUDGET, LRAD_BUDGET, XDIAG_TSTEP, &
                                L2M_MIN_ZS, LCOEF, LSURF_VARS, LSURF_BUDGETC,&
                                LRESET_BUDGETC  
USE MODD_DIAG_MISC_FLAKE_n,    ONLY : LWATER_PROFILE , XZWAT_PROFILE,     &
                                      XZW_PROFILE, XTW_PROFILE
USE MODD_CH_WATFLUX_n,   ONLY : XDEP, CCH_DRY_DEP, CSV, CCH_NAMES, &
                                  NBEQ, NSV_CHSBEG, NSV_CHSEND,  &
                                  NAEREQ, NSV_AERBEG, NSV_AEREND, CAER_NAMES,&
                                  NSV_DSTBEG, NSV_DSTEND, NDSTEQ, CDSTNAMES, &
                                  NSV_SLTBEG, NSV_SLTEND, NSLTEQ, CSLTNAMES  
USE MODD_CHS_AEROSOL,    ONLY: LVARSIGI, LVARSIGJ
USE MODD_DST_SURF,       ONLY: LVARSIG_DST, NDSTMDE, NDST_MDEBEG, LRGFIX_DST
USE MODD_SLT_SURF,       ONLY: LVARSIG_SLT, NSLTMDE, NSLT_MDEBEG, LRGFIX_SLT
!
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
USE MODI_INIT_IO_SURF_n
USE MODI_DEFAULT_CH_DEP
USE MODI_DEFAULT_FLAKE
USE MODI_DEFAULT_DIAG_FLAKE 
USE MODI_READ_DEFAULT_FLAKE_n
USE MODI_READ_FLAKE_CONF_n
USE MODI_READ_FLAKE_n
USE MODI_READ_PGD_FLAKE_n
USE MODI_DIAG_FLAKE_INIT_n
USE MODI_END_IO_SURF_n
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_READ_FLAKE_DATE
USE MODI_READ_NAM_PREP_FLAKE_n
USE MODI_INIT_CHEMICAL_n
USE MODI_PREP_CTRL_FLAKE
USE MODI_UPDATE_RAD_SEAWAT
USE MODI_READ_FLAKE_SBL_n
!
USE MODI_SET_SURFEX_FILEIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_GET_TYPE_DIM_n
!
USE MODI_WRITE_COVER_TEX_WATER
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),                 INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                 INTENT(IN)  :: HINIT     ! choice of fields to initialize
INTEGER,                          INTENT(IN)  :: KI        ! number of points
INTEGER,                          INTENT(IN)  :: KSV       ! number of scalars
INTEGER,                          INTENT(IN)  :: KSW       ! number of short-wave spectral bands
 CHARACTER(LEN=6), DIMENSION(KI), INTENT(IN)  :: HSV       ! name of all scalar variables
REAL,             DIMENSION(KI),  INTENT(IN)  :: PCO2      ! CO2 concentration (kg/m3)
REAL,             DIMENSION(KI),  INTENT(IN)  :: PRHOA     ! air density
REAL,             DIMENSION(KI),  INTENT(IN)  :: PZENITH   ! solar zenithal angle
REAL,             DIMENSION(KI),  INTENT(IN)  :: PAZIM     ! solar azimuthal angle (rad from N, clock)
REAL,             DIMENSION(KSW), INTENT(IN)  :: PSW_BANDS ! middle wavelength of each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB  ! direct albedo for each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB  ! diffuse albedo for each band
REAL,             DIMENSION(KI),  INTENT(OUT) :: PEMIS     ! emissivity
REAL,             DIMENSION(KI),  INTENT(OUT) :: PTSRAD    ! radiative temperature
INTEGER,                          INTENT(IN)  :: KYEAR     ! current year (UTC)
INTEGER,                          INTENT(IN)  :: KMONTH    ! current month (UTC)
INTEGER,                          INTENT(IN)  :: KDAY      ! current day (UTC)
REAL,                             INTENT(IN)  :: PTIME     ! current time since
                                                          !  midnight (UTC, s)
!
 CHARACTER(LEN=28),                INTENT(IN)  :: HATMFILE    ! atmospheric file name
 CHARACTER(LEN=6),                 INTENT(IN)  :: HATMFILETYPE! atmospheric file type
 CHARACTER(LEN=2),                 INTENT(IN)  :: HTEST       ! must be equal to 'OK'
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
INTEGER           :: ILU    ! sizes of FLAKE arrays
INTEGER           :: ILUOUT ! unit of output listing file
INTEGER           :: IRESP  ! return code
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!

!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_FLAKE_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('INIT_FLAKEN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
ALLOCATE(XZWAT_PROFILE(100))
!
!         Others litlle things
!
PDIR_ALB = XUNDEF
PSCA_ALB = XUNDEF
PEMIS    = XUNDEF
PTSRAD   = XUNDEF
!
IF (LNAM_READ) THEN
 !
 !*       0.     Defaults
 !               --------
 !
 !        0.1. Hard defaults
 !      
 CALL DEFAULT_FLAKE(XTSTEP,XOUT_TSTEP,LSEDIMENTS,CSNOW_FLK,CFLK_FLUX,CFLK_ALB,&
                    XICHCE,LPRECIP,LPWEBB)
 CALL DEFAULT_CH_DEP(CCH_DRY_DEP)
 CALL DEFAULT_DIAG_FLAKE(N2M,LSURF_BUDGET,L2M_MIN_ZS,LRAD_BUDGET,LCOEF,LSURF_VARS, &
                         LWATER_PROFILE,LSURF_BUDGETC,LRESET_BUDGETC,XDIAG_TSTEP,  &
                         XZWAT_PROFILE             )  
 !
ENDIF
!
!        0.2. Defaults from file header
!    
 CALL READ_DEFAULT_FLAKE_n(HPROGRAM)

!
!*       1.1    Reading of configuration:
!               -------------------------
!
 CALL READ_FLAKE_CONF_n(HPROGRAM)
!
IF (LWATER_PROFILE) THEN
   CALL GET_TYPE_DIM_n('WATER ',ILU)
   ALLOCATE (XZW_PROFILE(count (XZWAT_PROFILE /= XUNDEF))) 
   ALLOCATE (XTW_PROFILE(count (XZWAT_PROFILE /= XUNDEF),ILU)) 
   XZW_PROFILE=XZWAT_PROFILE(:count (XZWAT_PROFILE /= XUNDEF))
 ELSE
   ALLOCATE (XZW_PROFILE(1)) 
   ALLOCATE (XTW_PROFILE(1,1)) 
 END IF

!-------------------------------------------------------------------------------
!
!*       1.     Cover fields and grid:
!               ---------------------
!* date
!
SELECT CASE (HINIT)
  CASE ('PGD')
    TTIME%TDATE%YEAR = NUNDEF
    TTIME%TDATE%MONTH= NUNDEF
    TTIME%TDATE%DAY  = NUNDEF
    TTIME%TIME       = XUNDEF

  CASE ('PRE')
    CALL PREP_CTRL_FLAKE(N2M,LSURF_BUDGET,L2M_MIN_ZS,LRAD_BUDGET,LCOEF,LSURF_VARS,&
                             ILUOUT,LWATER_PROFILE,LSURF_BUDGETC) 
    IF (LNAM_READ) CALL READ_NAM_PREP_FLAKE_n(HPROGRAM)                            
    CALL READ_FLAKE_DATE(HPROGRAM,HINIT,ILUOUT,HATMFILE,HATMFILETYPE,KYEAR,KMONTH,KDAY,PTIME,TTIME)

  CASE DEFAULT
    CALL INIT_IO_SURF_n(HPROGRAM,'WATER ','FLAKE ','READ ')
    CALL READ_SURF(HPROGRAM,'DTCUR',TTIME,IRESP)
    CALL END_IO_SURF_n(HPROGRAM)
END SELECT
!
!-----------------------------------------------------------------------------------------------------
! READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
!         Initialisation for IO
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
 CALL INIT_IO_SURF_n(HPROGRAM,'WATER ','FLAKE ','READ ')
!
!         Reading of the fields
!
 CALL READ_PGD_FLAKE_n(HPROGRAM)
!
 CALL WRITE_COVER_TEX_WATER
!
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
!
!-----------------------------------------------------------------------------------------------------
! END READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
!* if only physiographic fields are to be initialized, stop here.
!
IF (HINIT/='ALL') THEN
  IF (LHOOK) CALL DR_HOOK('INIT_FLAKE_N',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------
!
!*       2.     Prognostic and cover fields:
!               ---------------------------
!
 CALL INIT_IO_SURF_n(HPROGRAM,'WATER ','FLAKE ','READ ')
!
 CALL READ_FLAKE_n(HPROGRAM)
!
ILU = SIZE(XCOVER,1)
!
!-------------------------------------------------------------------------------
!
!*       3.     Specific fields 
!               ---------------
!
ALLOCATE(XCORIO         (ILU))
ALLOCATE(XICE_ALB       (ILU))
ALLOCATE(XSNOW_ALB      (ILU))
ALLOCATE(XEXTCOEF_ICE   (ILU))
ALLOCATE(XEXTCOEF_SNOW  (ILU))
!
XCORIO(:) = 2*XOMEGA*SIN(XLAT(:)*XPI/180.)
!
XICE_ALB      = XUNDEF
XSNOW_ALB     = XUNDEF
XEXTCOEF_ICE  = XUNDEF
XEXTCOEF_SNOW = XUNDEF
!-------------------------------------------------------------------------------
!
!*       4.     Albedo, emissivity and radiative fields on lake
!               -----------------------------------------------
!
ALLOCATE(XDIR_ALB (ILU))
ALLOCATE(XSCA_ALB (ILU))
ALLOCATE(XEMIS    (ILU))
XDIR_ALB = 0.0
XSCA_ALB = 0.0
XEMIS    = 0.0
!
 CALL UPDATE_RAD_SEAWAT(CFLK_ALB,XTS,PZENITH,XTT,XEMIS,XDIR_ALB   ,&
                      XSCA_ALB,PDIR_ALB,PSCA_ALB,PEMIS,PTSRAD   )
!
!-------------------------------------------------------------------------------
!
!*       6.     SBL air fields:
!               --------------
!
 CALL READ_FLAKE_SBL_n(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*       6.     Chemistry / dust
!               ----------------
!
!
 CALL INIT_CHEMICAL_n(ILUOUT, KSV, HSV, NBEQ, CSV, NAEREQ,            &
                     NSV_CHSBEG, NSV_CHSEND, NSV_AERBEG, NSV_AEREND, &
                     CCH_NAMES, CAER_NAMES, NDSTEQ, NSV_DSTBEG,      &
                     NSV_DSTEND, NSLTEQ, NSV_SLTBEG, NSV_SLTEND,     &
                     HDSTNAMES=CDSTNAMES, HSLTNAMES=CSLTNAMES        )
!
!* depositiion scheme
!
IF (NBEQ>0 .AND. CCH_DRY_DEP=='WES89') THEN
  ALLOCATE(XDEP(ILU,NBEQ))
ELSE
  ALLOCATE(XDEP(0,0))
END IF
!
!-------------------------------------------------------------------------------
!
!*       7.     diagnostics initialization
!               --------------------------
!
 CALL DIAG_FLAKE_INIT_n(ILU,KSW)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('INIT_FLAKE_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_FLAKE_n
