!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #############################################################
      SUBROUTINE INIT_TEB_n     (HPROGRAM,HINIT,                            &
                                 KI,KSV,KSW,                                &
                                 HSV,PCO2,PRHOA,                            &
                                 PZENITH,PAZIM,PSW_BANDS,PDIR_ALB,PSCA_ALB, &
                                 PEMIS,PTSRAD,                              &
                                 KYEAR, KMONTH,KDAY, PTIME,                 &
                                 HATMFILE,HATMFILETYPE,                     &
                                 HTEST                                      )  
!     #############################################################
!
!!****  *INIT_TEB_n* - routine to initialize TEB
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
!!      G. Pigeon   09/2012: add ROUGH_WALL/ROUGH_ROOF/CH_BEM for conv. coef.
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_IO_SURF_ASC,ONLY: CMASK
USE MODD_SNOW_PAR, ONLY : XEMISSN
!
USE MODD_READ_NAMELIST, ONLY : LNAM_READ

USE MODD_TEB_n,           ONLY: LGARDEN, LGREENROOF,                                     &
                                XTSTEP, XOUT_TSTEP, TTIME, XCOVER,                       &
                                XH_TRAFFIC, XLE_TRAFFIC, XH_INDUSTRY, XLE_INDUSTRY,      &
                                XZ0_TOWN, XBLD, XGARDEN, XROAD_DIR, XGREENROOF,          &
                                XROAD, XBLD_HEIGHT, XWALL_O_HOR, XCAN_HW_RATIO,          &
                                XROAD_O_GRND, XGARDEN_O_GRND, XWALL_O_GRND, XWALL_O_BLD, &
                                XALB_ROOF, XEMIS_ROOF, XHC_ROOF,XTC_ROOF, XD_ROOF,       &
                                XALB_ROAD, XEMIS_ROAD, XHC_ROAD,XTC_ROAD, XD_ROAD,       &
                                XALB_WALL, XEMIS_WALL, XHC_WALL,XTC_WALL, XD_WALL,       &
                                XSVF_ROAD, XSVF_GARDEN, XSVF_WALL,                       &
                                TSNOW_ROOF, TSNOW_ROAD,                                  &
                                NROOF_LAYER, NROAD_LAYER, NWALL_LAYER,                   &
                                XT_ROOF, XT_ROAD, XT_WALL_A, XT_WALL_B, CZ0H,            &
                                CROAD_DIR, CWALL_OPT,                                    &
                                XT_CANYON, XQ_CANYON,                                    &
                                XAC_ROOF, XAC_ROAD, XAC_WALL, XAC_TOP,                   &
                                XAC_ROOF_WAT, XAC_ROAD_WAT,                              &
                                XQSAT_ROOF, XQSAT_ROAD, XDELT_ROOF, XDELT_ROAD,          &
                                NTEB_PATCH, XTEB_PATCH, CBEM, CCH_BEM,                   &
                                XROUGH_ROOF, XROUGH_WALL

USE MODD_BEM_n,           ONLY: NFLOOR_LAYER, XHC_FLOOR, XTC_FLOOR, XD_FLOOR,            &
                                XTCOOL_TARGET, XTHEAT_TARGET, XF_WASTE_CAN, XEFF_HEAT,   &
                                XQIN, XQIN_FRAD, XSHGC, XSHGC_SH, XU_WIN, XGR,           &
                                XFLOOR_HEIGHT, XINF, XQIN_FLAT, XHR_TARGET, XV_VENT,     &
                                XCAP_SYS_HEAT, XAUX_MAX, XCAP_SYS_RAT, XT_ADP,           &
                                XM_SYS_RAT, XCOP_RAT, XT_SIZE_MAX, XT_SIZE_MIN,          &
                                CCOOL_COIL, CHEAT_COIL, XF_WATER_COND, LSHAD_DAY,        &
                                LNATVENT_NIGHT, LSHADE, XSHADE, CNATVENT, XNATVENT,      &
                                LAUTOSIZE, XT_WIN1, XALB_WIN, XABS_WIN, XUGG_WIN,        &
                                XN_FLOOR, XGLAZ_O_BLD, XMASS_O_BLD, XFLOOR_HW_RATIO,     &
                                XF_FLOOR_MASS, XF_FLOOR_WALL, XF_FLOOR_WIN,              &
                                XF_FLOOR_ROOF, XF_WALL_FLOOR, XF_WALL_MASS,              &
                                XF_WALL_WIN, XF_WIN_FLOOR, XF_WIN_MASS, XF_WIN_WALL,     &
                                XF_MASS_FLOOR, XF_MASS_WALL, XF_MASS_WIN, XTRAN_WIN

USE MODD_TEB_VEG_n,       ONLY: CC1DRY, CSOILFRZ, CDIFSFCOND, CSNOWRES,                  &
                                CCPSURF, XCGMAX, CKSAT, CTOPREG,                         &
                                CRAIN, CHORT,                                            &
                                LCANOPY_DRAG, LVEGUPD

USE MODD_CH_TEB_n,        ONLY: XDEP, CCH_DRY_DEP, CSV, CCH_NAMES,                       &
                                NBEQ, NSV_CHSBEG, NSV_CHSEND,                            &
                                NAEREQ, NSV_AERBEG, NSV_AEREND, CAER_NAMES,              &
                                NSV_DSTBEG, NSV_DSTEND, NDSTEQ, CDSTNAMES,               &
                                NSV_SLTBEG, NSV_SLTEND, NSLTEQ, CSLTNAMES  


USE MODD_CHS_AEROSOL,     ONLY: LVARSIGI, LVARSIGJ
USE MODD_DST_SURF,        ONLY: LVARSIG_DST, NDSTMDE, NDST_MDEBEG, LRGFIX_DST 
USE MODD_SLT_SURF,        ONLY: LVARSIG_SLT, NSLTMDE, NSLT_MDEBEG, LRGFIX_SLT
USE MODD_DIAG_TEB_n,      ONLY: N2M, LSURF_BUDGET, LRAD_BUDGET, XDIAG_TSTEP,             &
                                  LPGD, LPGD_FIX, L2M_MIN_ZS, LCOEF, LSURF_VARS  
USE MODD_DIAG_MISC_TEB_n, ONLY: LSURF_MISC_BUDGET,                                       &
                                  LSURF_DIAG_ALBEDO, LSURF_EVAP_BUDGET  
USE MODD_DIAG_UTCI_TEB_n, ONLY: LUTCI
USE MODD_SURF_PAR,        ONLY: XUNDEF, NUNDEF
!
USE MODD_TEB_GARDEN_n,    ONLY : XLAI_GARDEN => XLAI
USE MODD_TEB_GREENROOF_n, ONLY : XLAI_GREENROOF => XLAI, NLAYER_GR
!
USE MODI_INIT_IO_SURF_n
USE MODI_DEFAULT_CH_DEP
USE MODI_DEFAULT_TEB
USE MODI_DEFAULT_DIAG_TEB
USE MODI_READ_DEFAULT_TEB_n
USE MODI_READ_TEB_CONF_n
USE MODI_PREP_CTRL_TEB
USE MODI_READ_TEB_n
USE MODI_READ_PGD_TEB_n
USE MODI_CONVERT_TEB
USE MODI_CONVERT_PATCH_TEB
USE MODI_INIT_SNOW_LW
USE MODI_AVERAGED_TSRAD_TEB
USE MODI_AVERAGED_ALBEDO_TEB
USE MODI_DIAG_TEB_INIT_n
USE MODI_DIAG_MISC_TEB_INIT_n
USE MODI_END_IO_SURF_n
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_READ_PREP_TEB_SNOW
USE MODI_READ_TEB_DATE
USE MODI_READ_NAM_PREP_TEB_n
USE MODI_INIT_CHEMICAL_n
USE MODI_GARDEN_PROPERTIES
USE MODI_HVAC_AUTOSIZE
USE MODI_GOTO_TEB
!
USE MODI_INIT_TEB_GARDEN_n
USE MODI_INIT_TEB_GARDEN_PGD_n
USE MODI_INIT_TEB_VEG_OPTIONS_n
USE MODI_TEB_MORPHO
USE MODI_INIT_BEM_n
USE MODI_INIT_TEB_GREENROOF_n
USE MODI_INIT_TEB_GREENROOF_PGD_n
USE MODI_GREENROOF_PROPERTIES
!
USE MODI_READ_COVER_GARDEN
USE MODI_WRITE_COVER_TEX_TEB
USE MODI_ABOR1_SFX
USE MODI_READ_TEB_CANOPY_n
USE MODI_SET_SURFEX_FILEIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),                   INTENT(IN)  :: HPROGRAM    ! program calling surf. schemes
 CHARACTER(LEN=3),                   INTENT(IN)  :: HINIT       ! choice of fields to initialize
INTEGER,                            INTENT(IN)  :: KI          ! number of points
INTEGER,                            INTENT(IN)  :: KSV         ! number of scalars
INTEGER,                            INTENT(IN)  :: KSW         ! number of short-wave spectral bands
 CHARACTER(LEN=6), DIMENSION(KSV),   INTENT(IN)  :: HSV         ! name of all scalar variables
REAL,             DIMENSION(KI),    INTENT(IN)  :: PCO2        ! CO2 concentration (kg/m3)
REAL,             DIMENSION(KI),    INTENT(IN)  :: PRHOA       ! air density
REAL,             DIMENSION(KI),    INTENT(IN)  :: PZENITH     ! solar zenithal angle
REAL,             DIMENSION(KI),    INTENT(IN)  :: PAZIM       ! solar azimuthal angle (rad from N, clock)
REAL,             DIMENSION(KSW),   INTENT(IN)  :: PSW_BANDS   ! middle wavelength of each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB    ! direct albedo for each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB    ! diffuse albedo for each band
REAL,             DIMENSION(KI),    INTENT(OUT) :: PEMIS       ! emissivity
REAL,             DIMENSION(KI),    INTENT(OUT) :: PTSRAD      ! radiative temperature
INTEGER,                            INTENT(IN)  :: KYEAR       ! current year (UTC)
INTEGER,                            INTENT(IN)  :: KMONTH      ! current month (UTC)
INTEGER,                            INTENT(IN)  :: KDAY        ! current day (UTC)
REAL,                               INTENT(IN)  :: PTIME       ! current time since
                                                               !  midnight (UTC, s)
!
 CHARACTER(LEN=28),                  INTENT(IN)  :: HATMFILE    ! atmospheric file name
 CHARACTER(LEN=6),                   INTENT(IN)  :: HATMFILETYPE! atmospheric file type
 CHARACTER(LEN=2),                   INTENT(IN)  :: HTEST       ! must be equal to 'OK'
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                         :: ILU              ! sizes of TEB arrays
INTEGER                         :: ILUOUT           ! unit of output listing file
INTEGER                         :: IRESP            ! return code
!
INTEGER                         :: ISWB             ! number of shortwave spectral bands
INTEGER                         :: JSWB             ! loop on shortwave spectral bands
!
REAL                            :: ZDEF_ROAD_DIR    ! default raod direction
REAL, DIMENSION(:), ALLOCATABLE :: ZDIR_ALB         ! direct town albedo
REAL, DIMENSION(:), ALLOCATABLE :: ZSCA_ALB         ! diffuse town albedo
!
!              local variables for urban green areas
REAL, DIMENSION(KI,KSW)         :: ZDIR_SW          ! direct  SW for each band
REAL, DIMENSION(KI,KSW)         :: ZSCA_SW          ! diffuse SW for each band
REAL, DIMENSION(KI)             :: ZEMIS_GARDEN     ! emissivity
REAL, DIMENSION(KI)             :: ZALB_GARDEN      ! albedo
REAL, DIMENSION(KI)             :: ZTS_GARDEN       ! radiative temperature
!
!              local variables for urban greenroofs
REAL, DIMENSION(KI)             :: ZEMIS_GREENROOF     ! emissivity
REAL, DIMENSION(KI)             :: ZALB_GREENROOF      ! albedo
REAL, DIMENSION(KI)             :: ZTS_GREENROOF       ! radiative temperature
!
INTEGER                         :: JPATCH
INTEGER                         :: IVERSION, IBUGFIX

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('INIT_TEBN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
!         Other little things
!
PDIR_ALB = XUNDEF
PSCA_ALB = XUNDEF
PEMIS    = XUNDEF
PTSRAD   = XUNDEF
!
LSURF_DIAG_ALBEDO = .FALSE.
LSURF_EVAP_BUDGET = .FALSE.
!
IF (LNAM_READ) THEN
 !
 !*       0.     Defaults
 !               --------
 !
 !        0.1. Hard defaults
 !      
 CALL DEFAULT_TEB(CZ0H,XTSTEP,XOUT_TSTEP, CCH_BEM)
 CALL DEFAULT_CH_DEP(CCH_DRY_DEP)
 CALL DEFAULT_DIAG_TEB(N2M,LSURF_BUDGET,L2M_MIN_ZS,LRAD_BUDGET,LCOEF,LSURF_VARS, &
                       LSURF_MISC_BUDGET,LUTCI,LPGD,LPGD_FIX,XDIAG_TSTEP)  
!
ENDIF
!
!        0.2. Defaults from file header
!    
 CALL READ_DEFAULT_TEB_n(HPROGRAM)
!
!*       1.     Reading of configuration:
!               -------------------------
!
 CALL READ_TEB_CONF_n(HPROGRAM)
!
!* initialization of snow scheme
!
IF (HINIT=='PRE') THEN
  DO JPATCH=1,NTEB_PATCH
    CALL GOTO_TEB(JPATCH)
    CALL READ_PREP_TEB_SNOW(HPROGRAM,TSNOW_ROOF%SCHEME,TSNOW_ROOF%NLAYER, &
                                     TSNOW_ROAD%SCHEME,TSNOW_ROAD%NLAYER)
  END DO
ENDIF
!
!*       2.     Cover fields and grid:
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
    CALL PREP_CTRL_TEB(N2M,LSURF_BUDGET,L2M_MIN_ZS,LRAD_BUDGET,LCOEF,LSURF_VARS,&
                         LSURF_EVAP_BUDGET,LSURF_MISC_BUDGET,LUTCI,ILUOUT )           
    IF (LNAM_READ) CALL READ_NAM_PREP_TEB_n(HPROGRAM)   
    CALL READ_TEB_DATE(HPROGRAM,HINIT,ILUOUT,HATMFILE,HATMFILETYPE,KYEAR,KMONTH,KDAY,PTIME,TTIME)

  CASE DEFAULT
    CALL INIT_IO_SURF_n(HPROGRAM,'TOWN  ','TEB   ','READ ')
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
 CALL INIT_IO_SURF_n(HPROGRAM,'TOWN  ','TEB   ','READ ')
!
 CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
!
!         Reading of the fields
!
 CALL READ_COVER_GARDEN(HPROGRAM,LGARDEN)
!
 CALL READ_PGD_TEB_n(HPROGRAM)
!
 CALL END_IO_SURF_n(HPROGRAM)
! 
!*        Fraction of each patch in the grid mesh
!
ILU = SIZE(XCOVER,1)
!
ALLOCATE(XTEB_PATCH(ILU,NTEB_PATCH))
 CALL CONVERT_TEB(XCOVER,XTEB_PATCH)
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
 CALL INIT_IO_SURF_n(HPROGRAM,'TOWN  ','TEB   ','READ ')
!
 CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
!
!* reads what is the option defined for road orientations & walls
!
IF (HINIT=='ALL') THEN
  CROAD_DIR='UNIF'
  CWALL_OPT='UNIF'
  IF (IVERSION>7 .OR. (IVERSION==7 .AND. IBUGFIX>=3)) THEN
    CALL READ_SURF(HPROGRAM,'ROAD_DIR',CROAD_DIR,IRESP)
    CALL READ_SURF(HPROGRAM,'WALL_OPT',CWALL_OPT,IRESP)
  END IF
END IF
 CALL END_IO_SURF_n(HPROGRAM)
!-----------------------------------------------------------------------------------
!
!*              LOOP ON TEB PATCHES
!               -------------------
!
DO JPATCH=1,NTEB_PATCH

  CALL GOTO_TEB(JPATCH)
  !-----------------------------------------------------------------------------------
  !
  !*       3.     Physiographic data fields from land cover:
  !               -----------------------------------------
  !
  ALLOCATE(XZ0_TOWN     (ILU))
  ALLOCATE(XALB_ROOF    (ILU))
  ALLOCATE(XEMIS_ROOF   (ILU))
  ALLOCATE(XALB_ROAD    (ILU))
  ALLOCATE(XEMIS_ROAD   (ILU))
  ALLOCATE(XALB_WALL    (ILU))
  ALLOCATE(XEMIS_WALL   (ILU))
  ALLOCATE(XBLD         (ILU))
  ALLOCATE(XROAD_DIR    (ILU))
  ALLOCATE(XROAD        (ILU))
  ALLOCATE(XBLD_HEIGHT  (ILU))
  ALLOCATE(XWALL_O_HOR  (ILU))
  ALLOCATE(XCAN_HW_RATIO(ILU))
  ALLOCATE(XROAD_O_GRND(ILU))
  ALLOCATE(XGARDEN_O_GRND(ILU))
  ALLOCATE(XWALL_O_GRND(ILU))
  ALLOCATE(XWALL_O_BLD(ILU))
  ALLOCATE(XH_TRAFFIC   (ILU))
  ALLOCATE(XLE_TRAFFIC  (ILU))
  ALLOCATE(XH_INDUSTRY  (ILU))
  ALLOCATE(XLE_INDUSTRY (ILU))
  ALLOCATE(XHC_ROOF     (ILU,NROOF_LAYER))
  ALLOCATE(XTC_ROOF     (ILU,NROOF_LAYER))
  ALLOCATE(XD_ROOF      (ILU,NROOF_LAYER))
  ALLOCATE(XHC_ROAD     (ILU,NROAD_LAYER))
  ALLOCATE(XTC_ROAD     (ILU,NROAD_LAYER))
  ALLOCATE(XD_ROAD      (ILU,NROAD_LAYER))
  ALLOCATE(XHC_WALL     (ILU,NWALL_LAYER))
  ALLOCATE(XTC_WALL     (ILU,NWALL_LAYER))
  ALLOCATE(XD_WALL      (ILU,NWALL_LAYER))
  ALLOCATE(XROUGH_ROOF      (ILU))
  ALLOCATE(XROUGH_WALL      (ILU))
  ALLOCATE(XGREENROOF       (ILU))
  ALLOCATE(XGARDEN          (ILU))
  !
  XROAD_DIR(:) = 0.
  XROAD    (:) = 0.
  !
  ZDEF_ROAD_DIR = 0.
  IF (CROAD_DIR/='UNIF') THEN
    !* road direction if not specified by the user depends on patch number
    !  First patch has a Notrh-South road. Other patches have roads spaced by
    !  regular angles
    ZDEF_ROAD_DIR = 180. * FLOAT(JPATCH-1) / FLOAT(NTEB_PATCH)
  END IF
  !
  CALL CONVERT_PATCH_TEB(XCOVER, ZDEF_ROAD_DIR,                                  &
                      PZ0_TOWN=XZ0_TOWN,                                         &
                      PALB_ROOF=XALB_ROOF,                                       &
                      PEMIS_ROOF=XEMIS_ROOF,PHC_ROOF=XHC_ROOF,PTC_ROOF=XTC_ROOF, &
                      PD_ROOF=XD_ROOF,                                           &
                      PALB_ROAD=XALB_ROAD,                                       &
                      PEMIS_ROAD=XEMIS_ROAD,PHC_ROAD=XHC_ROAD,PTC_ROAD=XTC_ROAD, &
                      PD_ROAD=XD_ROAD,                                           &
                      PALB_WALL=XALB_WALL,                                       &
                      PEMIS_WALL=XEMIS_WALL,PHC_WALL=XHC_WALL,PTC_WALL=XTC_WALL, &
                      PD_WALL=XD_WALL,                                           &
                      PBLD_HEIGHT=XBLD_HEIGHT,                                   &
                      PWALL_O_HOR=XWALL_O_HOR,PBLD=XBLD, PROAD_DIR=XROAD_DIR,    &
                      PGARDEN=XGARDEN,                                           &
                      PH_TRAFFIC=XH_TRAFFIC, PLE_TRAFFIC=XLE_TRAFFIC,            &
                      PH_INDUSTRY=XH_INDUSTRY, PLE_INDUSTRY=XLE_INDUSTRY,        &
                      PROUGH_ROOF = XROUGH_ROOF, PROUGH_WALL = XROUGH_WALL,      &
                      PGREENROOF = XGREENROOF                                    )
  !
  IF (.NOT. LGREENROOF .AND. MAXVAL(XGREENROOF)>0. ) THEN !<== A paralleliser pour un stop propre
    WRITE(ILUOUT,*) 'You choose NOT to have greenroofs, BUT your greenroof fraction is not zero'
    WRITE(ILUOUT,*) 'Please activate the greenroof option (and rerun the SURFEX suite from the PGD step)'
    WRITE(ILUOUT,*) 'Or be sure NOT to have any greenroofs in your area'
    CALL ABOR1_SFX('INIT_TEBN: GREENROOF OPTION NOT ACTIVATED WHILE GREENROOFS ARE PRESENT')
  ENDIF
  !-------------------------------------------------------------------------------
  !
  !*       5.     Sky-view-factors:
  !               ----------------
  !
  ALLOCATE(XSVF_ROAD  (ILU))
  ALLOCATE(XSVF_GARDEN(ILU))
  ALLOCATE(XSVF_WALL  (ILU))
  !
  ALLOCATE(XGR          (ILU))
  ALLOCATE(XALB_WIN     (ILU))
  ALLOCATE(XF_WASTE_CAN (ILU))
  !
  !
  CALL TEB_MORPHO(HPROGRAM, XBLD, XWALL_O_HOR, XGARDEN, XBLD_HEIGHT, XROAD, XROAD_O_GRND, &
                XGARDEN_O_GRND, XWALL_O_GRND, XCAN_HW_RATIO, XSVF_ROAD, XSVF_GARDEN,    &
                XSVF_WALL, XZ0_TOWN, XWALL_O_BLD, XH_TRAFFIC, XLE_TRAFFIC               )
                !
  !-------------------------------------------------------------------------------
  !
  !*       6.     Building Energy Model
  !               ---------------------
  !
  CALL INIT_BEM_n(ILUOUT)
  !
  !-------------------------------------------------------------------------------
  !
  !*      7.      Case of urban green areas
  !               -------------------------
  !
  IF (LGARDEN) THEN
  !
    CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
    CALL INIT_IO_SURF_n(HPROGRAM,'TOWN  ','TEB   ','READ ')     
    IF (JPATCH==1) CALL INIT_TEB_VEG_OPTIONS_n(HPROGRAM)
    CALL INIT_TEB_GARDEN_PGD_n(HPROGRAM,HINIT,(JPATCH==1),KI,KSV,HSV,IVERSION,IBUGFIX,PCO2,PRHOA)
    ! Case of urban green roofs
    IF (LGREENROOF) CALL INIT_TEB_GREENROOF_PGD_n(HPROGRAM,HINIT,(JPATCH==1), &
                                                  KI,KSV,HSV,IVERSION,PCO2,PRHOA)
    CALL END_IO_SURF_n(HPROGRAM)
    !
  ENDIF
!-------------------------------------------------------------------------------
END DO ! end of loop on TEB patches
!-------------------------------------------------------------------------------
!
!* if only physiographic fields are to be initialized, stop here.
!
 CALL WRITE_COVER_TEX_TEB
!
IF (HINIT/='ALL') THEN
  IF (LHOOK) CALL DR_HOOK('INIT_TEB_N',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
 CALL INIT_IO_SURF_n(HPROGRAM,'TOWN  ','TEB   ','READ ')
!
!*       9.     Prognostic fields:
!               -----------------
!
!               -------------------------
!

!
!*              LOOP ON TEB PATCHES
!               -------------------
!
DO JPATCH=1,NTEB_PATCH
  CALL GOTO_TEB(JPATCH)
!
!* TEB fields
  CALL READ_TEB_n(HPROGRAM,JPATCH)
!
  ALLOCATE(XAC_ROOF    (ILU))
  ALLOCATE(XAC_ROAD    (ILU))
  ALLOCATE(XAC_WALL    (ILU))
  ALLOCATE(XAC_TOP     (ILU))
  ALLOCATE(XAC_ROOF_WAT(ILU))
  ALLOCATE(XAC_ROAD_WAT(ILU))
  ALLOCATE(XQSAT_ROOF  (ILU))
  ALLOCATE(XQSAT_ROAD  (ILU))
  ALLOCATE(XDELT_ROOF  (ILU))
  ALLOCATE(XDELT_ROAD  (ILU))
!
!* Case of urban green areas
  IF (LGARDEN) THEN
!    CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! change input file name to pgd name
!    CALL INIT_IO_SURF_n(HPROGRAM,'TOWN  ','TEB   ','READ ')       
    CALL INIT_TEB_GARDEN_n(HPROGRAM,HINIT,KI,KSW,PSW_BANDS)
  ! Case of urban green roofs
    IF (LGREENROOF) CALL INIT_TEB_GREENROOF_n(HPROGRAM,HINIT,KI,KSV,PSW_BANDS)
!    CALL END_IO_SURF_n(HPROGRAM)
  ENDIF
!-------------------------------------------------------------------------------
!
!*      10.     Infra-red Radiative fields:
!               --------------------------
!
!* snow long-wave properties (not initialized in read_gr_snow)
!
  CALL INIT_SNOW_LW(XEMISSN,TSNOW_ROOF)
  CALL INIT_SNOW_LW(XEMISSN,TSNOW_ROAD)
!
  IF (LGARDEN) THEN
    ZDIR_SW=0. ! night as first guess for albedo computation
    ZSCA_SW=0. !
    CALL GARDEN_PROPERTIES(ZDIR_SW, ZSCA_SW, PSW_BANDS, KSW,     &
                           ZTS_GARDEN, ZEMIS_GARDEN, ZALB_GARDEN )      
  ELSE
    ZALB_GARDEN = XUNDEF
    ZEMIS_GARDEN= XUNDEF
    ZTS_GARDEN  = XUNDEF
  END IF
  !
  IF (LGREENROOF) THEN
    ZDIR_SW=0. ! night as first guess for albedo computation
    ZSCA_SW=0. !
    CALL GREENROOF_PROPERTIES(ZDIR_SW, ZSCA_SW, PSW_BANDS, KSW,              &
                              ZTS_GREENROOF, ZEMIS_GREENROOF, ZALB_GREENROOF )  
  ELSE
    ZALB_GREENROOF  = XUNDEF
    ZEMIS_GREENROOF = XUNDEF
    ZTS_GREENROOF   = XUNDEF
  END IF
!
!* averaged albedo, emissivity and radiative temperature
!
  CALL AVERAGED_TSRAD_TEB(XEMIS_ROOF,XT_ROOF(:,1),       &
                        XEMIS_ROAD,XT_ROAD(:,1),       &
                        XEMIS_WALL,                    &
                        XT_WALL_A(:,1),                &
                        XT_WALL_B(:,1),                &
                        ZEMIS_GARDEN, ZTS_GARDEN,      &
                        ZEMIS_GREENROOF, ZTS_GREENROOF,&
                        TSNOW_ROOF,TSNOW_ROAD,         &
                        XROAD, XGREENROOF, XGARDEN,    &
                        XBLD,XWALL_O_HOR,              &
                        XSVF_ROAD,XSVF_WALL,           &
                        XSVF_GARDEN,                   &
                        PEMIS,PTSRAD, XT_WIN1,         &
                        XGR                            )
!
!
!*       9.     Visible and near-infra-red Radiative fields:
!               -------------------------------------------
!
  ALLOCATE(ZDIR_ALB(ILU))
  ALLOCATE(ZSCA_ALB(ILU))
!
  CALL AVERAGED_ALBEDO_TEB(CBEM,CROAD_DIR,CWALL_OPT,PZENITH,PAZIM, &
                       XBLD, XGARDEN, XROAD_DIR, XROAD, XGREENROOF,&
                       XWALL_O_HOR, XCAN_HW_RATIO,                 &
                       XALB_ROOF,                                  &
                       XALB_ROAD, XSVF_ROAD,                       &
                       XALB_WALL, XSVF_WALL,                       &
                       ZALB_GARDEN, XSVF_GARDEN,                   &
                       ZALB_GREENROOF,                             &
                       TSNOW_ROOF, TSNOW_ROAD,                     &
                       XGR, XSHGC, XSHGC_SH, XABS_WIN, XALB_WIN,   &
                       LSHAD_DAY,                                  &
                       ZDIR_ALB, ZSCA_ALB, XTRAN_WIN               )  

  ISWB=SIZE(PSW_BANDS)
  DO JSWB=1,ISWB
    PDIR_ALB(:,JSWB) = ZDIR_ALB(:)
    PSCA_ALB(:,JSWB) = ZSCA_ALB(:)
  END DO
  !
  DEALLOCATE(ZDIR_ALB)
  DEALLOCATE(ZSCA_ALB)
!-------------------------------------------------------------------------------
!
!*      10.     Chemistry /dust
!               ---------------
!
  CALL INIT_CHEMICAL_n(ILUOUT, KSV, HSV, NBEQ, CSV, NAEREQ,            &
                     NSV_CHSBEG, NSV_CHSEND, NSV_AERBEG, NSV_AEREND, &
                     CCH_NAMES, CAER_NAMES, NDSTEQ, NSV_DSTBEG,      &
                     NSV_DSTEND, NSLTEQ, NSV_SLTBEG, NSV_SLTEND,     &
                     HDSTNAMES=CDSTNAMES, HSLTNAMES=CSLTNAMES        )
!
!* Initialization of dry deposition scheme (chemistry)
!
  IF (NBEQ>0 .AND. CCH_DRY_DEP=='WES89') THEN
    ALLOCATE(XDEP(ILU,NBEQ))
  ELSE
    ALLOCATE(XDEP(0,0))
  END IF
!
!-------------------------------------------------------------------------------
END DO ! end of loop on patches
!-------------------------------------------------------------------------------
!
!*       7.     Canopy air fields:
!               ------------------
!
 CALL READ_TEB_CANOPY_n(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*      11.     Diagnostics:
!               -----------
!
 CALL DIAG_TEB_INIT_n(HPROGRAM,ILU,ISWB)
DO JPATCH=1,NTEB_PATCH
  CALL GOTO_TEB(JPATCH)
  CALL DIAG_MISC_TEB_INIT_n(HPROGRAM,ILU,ISWB)
END DO ! end of loop on patches
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('INIT_TEB_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE INIT_TEB_n
