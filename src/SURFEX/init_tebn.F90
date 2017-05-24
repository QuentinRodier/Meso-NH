!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################################################
      SUBROUTINE INIT_TEB_n (DTCO, DGU, UG, U, CHI, DTI, I, &
                             TM, GDM, GRM, DST, SLT,GCP, &
                                  HPROGRAM,HINIT,                            &
                                 KI,KSV,KSW,                                &
                                 HSV,PCO2,PRHOA,                            &
                                 PZENITH,PAZIM,PSW_BANDS,PDIR_ALB,PSCA_ALB, &
                                 PEMIS,PTSRAD,PTSURF,                       &
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003
!!      G. Pigeon   09/2012: add ROUGH_WALL/ROUGH_ROOF/CH_BEM for conv. coef.
!!      B. Decharme  04/2013 new coupling variables
!!                           delete CTOPREG option (never used)
!!      M.Moge      02/2015 MPPDB_CHECK
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURFEX_n, ONLY : TEB_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
USE MODD_DST_n, ONLY : DST_t
USE MODD_SLT_n, ONLY : SLT_t
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
!
USE MODD_IO_SURF_ASC,ONLY: CMASK
USE MODD_SNOW_PAR, ONLY : XEMISSN
!
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
!
USE MODD_CHS_AEROSOL,     ONLY: LVARSIGI, LVARSIGJ
USE MODD_DST_SURF,        ONLY: LVARSIG_DST, NDSTMDE, NDST_MDEBEG, LRGFIX_DST 
USE MODD_SLT_SURF,        ONLY: LVARSIG_SLT, NSLTMDE, NSLT_MDEBEG, LRGFIX_SLT
USE MODD_SURF_PAR,        ONLY: XUNDEF, NUNDEF
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
USE MODI_GOTO_WRAPPER_TEB_PATCH
!
USE MODI_INIT_TEB_GARDEN_n
USE MODI_INIT_TEB_GARDEN_PGD_n
USE MODI_INIT_TEB_VEG_OPTIONS_n
USE MODI_TEB_MORPHO
USE MODI_INIT_BEM_n
USE MODI_INIT_TEB_GREENROOF_n
USE MODI_INIT_TEB_GREENROOF_PGD_n
USE MODI_GREENROOF_PROPERTIES
USE MODI_READ_PGD_TEB_IRRIG_n
!
USE MODI_READ_COVER_GARDEN
USE MODI_ABOR1_SFX
USE MODI_READ_TEB_CANOPY_n
USE MODI_SET_SURFEX_FILEIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef MNH_PARALLEL
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
USE MODE_MPPDB
!
#endif
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
TYPE(DST_t), INTENT(INOUT) :: DST
TYPE(SLT_t), INTENT(INOUT) :: SLT
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
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
REAL,             DIMENSION(KI),    INTENT(OUT) :: PTSURF      ! surface effective temperature         (K)
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
PTSURF   = XUNDEF
!
TM%DGMTO%LSURF_EVAP_BUDGET = .FALSE.
!
IF (LNAM_READ) THEN
 !
 !*       0.     Defaults
 !               --------
 !
 !        0.1. Hard defaults
 !      
 CALL DEFAULT_TEB(TM%TOP%CZ0H,TM%TOP%XTSTEP,TM%TOP%XOUT_TSTEP, TM%TOP%CCH_BEM, &
                  TM%T%CUR%XDT_RES, TM%T%CUR%XDT_OFF)
 CALL DEFAULT_CH_DEP(TM%CHT%CCH_DRY_DEP)
 CALL DEFAULT_DIAG_TEB(TM%DGT%N2M,TM%DGT%LSURF_BUDGET,TM%DGT%L2M_MIN_ZS,TM%DGT%LRAD_BUDGET,&
                       TM%DGT%LCOEF,TM%DGT%LSURF_VARS,TM%DGMTO%LSURF_MISC_BUDGET,&
                       TM%DGMTO%LSURF_DIAG_ALBEDO,TM%DGUT%LUTCI,TM%DGT%LPGD,&
                       TM%DGT%LPGD_FIX,TM%DGT%XDIAG_TSTEP)  
!
ENDIF
!
!        0.2. Defaults from file header
!    
 CALL READ_DEFAULT_TEB_n(TM%CHT, TM%DGMTO, TM%DGT, TM%DGUT, GRM%TGRO, TM%T, TM%TOP, &
                         HPROGRAM)
!
!*       1.     Reading of configuration:
!               -------------------------
!
 CALL READ_TEB_CONF_n(TM%CHT, TM%DGMTO, TM%DGT, TM%DGUT, TM%T, TM%TOP, &
                      HPROGRAM)
!
!* initialization of snow scheme
!
IF (HINIT=='PRE') THEN
  DO JPATCH=1,TM%TOP%NTEB_PATCH
    CALL GOTO_WRAPPER_TEB_PATCH(TM%B, TM%DGCT, TM%DGMT, TM%T, GDM%TGD, GDM%TGDPE, &
                                GRM%TGR, GRM%TGRPE, JPATCH)
    CALL READ_PREP_TEB_SNOW(HPROGRAM,TM%T%CUR%TSNOW_ROOF%SCHEME,TM%T%CUR%TSNOW_ROOF%NLAYER, &
                                     TM%T%CUR%TSNOW_ROAD%SCHEME,TM%T%CUR%TSNOW_ROAD%NLAYER)
  END DO
ENDIF
!
!*       2.     Cover fields and grid:
!               ---------------------
!* date
!
SELECT CASE (HINIT)
  CASE ('PGD')
    TM%TOP%TTIME%TDATE%YEAR = NUNDEF
    TM%TOP%TTIME%TDATE%MONTH= NUNDEF
    TM%TOP%TTIME%TDATE%DAY  = NUNDEF
    TM%TOP%TTIME%TIME       = XUNDEF

  CASE ('PRE')
    CALL PREP_CTRL_TEB(TM%DGT%N2M,TM%DGT%LSURF_BUDGET,TM%DGT%L2M_MIN_ZS,TM%DGT%LRAD_BUDGET,&
                       TM%DGT%LCOEF,TM%DGT%LSURF_VARS,TM%DGMTO%LSURF_EVAP_BUDGET,&
                       TM%DGMTO%LSURF_MISC_BUDGET,TM%DGUT%LUTCI,ILUOUT )           
    IF (LNAM_READ) CALL READ_NAM_PREP_TEB_n(HPROGRAM)   
    CALL READ_TEB_DATE(&
                       HPROGRAM,HINIT,ILUOUT,HATMFILE,HATMFILETYPE,KYEAR,KMONTH,KDAY,PTIME,TM%TOP%TTIME)

  CASE DEFAULT
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                        HPROGRAM,'TOWN  ','TEB   ','READ ')
    CALL READ_SURF(&
                   HPROGRAM,'DTCUR',TM%TOP%TTIME,IRESP)
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
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                        HPROGRAM,'TOWN  ','TEB   ','READ ')
!
 CALL READ_SURF(&
                   HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(&
                   HPROGRAM,'BUG',IBUGFIX,IRESP)
!
!         Reading of the fields
!
 CALL READ_COVER_GARDEN(&
                        HPROGRAM,TM%TOP%LGARDEN)
!
 CALL READ_PGD_TEB_n(DTCO, U, TM,GCP, &
                     HPROGRAM)
#ifdef MNH_PARALLEL
 CALL MPPDB_CHECK_SURFEX3D(TM%TOP%XCOVER,"INIT_TEB_n after READ_PGD_TEB_n:XCOVER",PRECISION,ILUOUT, 'TOWN  ',JPCOVER)
#endif
!
 CALL END_IO_SURF_n(HPROGRAM)
! 
!*        Fraction of each patch in the grid mesh
!
ILU = SIZE(TM%TOP%XCOVER,1)
!
ALLOCATE(TM%TOP%XTEB_PATCH(ILU,TM%TOP%NTEB_PATCH))
 CALL CONVERT_TEB(TM%TOP, &
                  TM%TOP%XCOVER,TM%TOP%XTEB_PATCH)
#ifdef MNH_PARALLEL
 CALL MPPDB_CHECK_SURFEX3D(TM%TOP%XCOVER,"INIT_TEB_n after CONVERT_TEB:XCOVER",PRECISION,ILUOUT, 'TOWN  ',JPCOVER)
#endif
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                        HPROGRAM,'TOWN  ','TEB   ','READ ')
!
 CALL READ_SURF(&
                   HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(&
                   HPROGRAM,'BUG',IBUGFIX,IRESP)
!
!* reads what is the option defined for road orientations & walls
!
IF (HINIT=='ALL') THEN
  TM%TOP%CROAD_DIR='UNIF'
  TM%TOP%CWALL_OPT='UNIF'
  IF (IVERSION>7 .OR. (IVERSION==7 .AND. IBUGFIX>=3)) THEN
    CALL READ_SURF(&
                   HPROGRAM,'ROAD_DIR',TM%TOP%CROAD_DIR,IRESP)
    CALL READ_SURF(&
                   HPROGRAM,'WALL_OPT',TM%TOP%CWALL_OPT,IRESP)
  END IF
END IF
 CALL END_IO_SURF_n(HPROGRAM)
!-----------------------------------------------------------------------------------
!
!*              LOOP ON TEB PATCHES
!               -------------------
!
DO JPATCH=1,TM%TOP%NTEB_PATCH

  CALL GOTO_WRAPPER_TEB_PATCH(TM%B, TM%DGCT, TM%DGMT, TM%T, &
                              GDM%TGD, GDM%TGDPE, GRM%TGR, GRM%TGRPE, JPATCH)
  !-----------------------------------------------------------------------------------
  !
  !*       3.     Physiographic data fields from land cover:
  !               -----------------------------------------
  !
  ALLOCATE(TM%T%CUR%XZ0_TOWN     (ILU))
  ALLOCATE(TM%T%CUR%XALB_ROOF    (ILU))
  ALLOCATE(TM%T%CUR%XEMIS_ROOF   (ILU))
  ALLOCATE(TM%T%CUR%XALB_ROAD    (ILU))
  ALLOCATE(TM%T%CUR%XEMIS_ROAD   (ILU))
  ALLOCATE(TM%T%CUR%XALB_WALL    (ILU))
  ALLOCATE(TM%T%CUR%XEMIS_WALL   (ILU))
  ALLOCATE(TM%T%CUR%XBLD         (ILU))
  ALLOCATE(TM%T%CUR%XROAD_DIR    (ILU))
  ALLOCATE(TM%T%CUR%XROAD        (ILU))
  ALLOCATE(TM%T%CUR%XBLD_HEIGHT  (ILU))
  ALLOCATE(TM%T%CUR%XWALL_O_HOR  (ILU))
  ALLOCATE(TM%T%CUR%XCAN_HW_RATIO(ILU))
  ALLOCATE(TM%T%CUR%XROAD_O_GRND(ILU))
  ALLOCATE(TM%T%CUR%XGARDEN_O_GRND(ILU))
  ALLOCATE(TM%T%CUR%XWALL_O_GRND(ILU))
  ALLOCATE(TM%T%CUR%XWALL_O_BLD(ILU))
  ALLOCATE(TM%T%CUR%XH_TRAFFIC   (ILU))
  ALLOCATE(TM%T%CUR%XLE_TRAFFIC  (ILU))
  ALLOCATE(TM%T%CUR%XH_INDUSTRY  (ILU))
  ALLOCATE(TM%T%CUR%XLE_INDUSTRY (ILU))
  ALLOCATE(TM%T%CUR%XHC_ROOF     (ILU,TM%TOP%NROOF_LAYER))
  ALLOCATE(TM%T%CUR%XTC_ROOF     (ILU,TM%TOP%NROOF_LAYER))
  ALLOCATE(TM%T%CUR%XD_ROOF      (ILU,TM%TOP%NROOF_LAYER))
  ALLOCATE(TM%T%CUR%XHC_ROAD     (ILU,TM%TOP%NROAD_LAYER))
  ALLOCATE(TM%T%CUR%XTC_ROAD     (ILU,TM%TOP%NROAD_LAYER))
  ALLOCATE(TM%T%CUR%XD_ROAD      (ILU,TM%TOP%NROAD_LAYER))
  ALLOCATE(TM%T%CUR%XHC_WALL     (ILU,TM%TOP%NWALL_LAYER))
  ALLOCATE(TM%T%CUR%XTC_WALL     (ILU,TM%TOP%NWALL_LAYER))
  ALLOCATE(TM%T%CUR%XD_WALL      (ILU,TM%TOP%NWALL_LAYER))
  ALLOCATE(TM%T%CUR%XROUGH_ROOF      (ILU))
  ALLOCATE(TM%T%CUR%XROUGH_WALL      (ILU))
  ALLOCATE(TM%T%CUR%XRESIDENTIAL     (ILU))
  ALLOCATE(TM%T%CUR%XGREENROOF       (ILU))
  ALLOCATE(TM%T%CUR%XGARDEN          (ILU))
  ALLOCATE(TM%TPN%XEMIS_PANEL      (ILU))
  ALLOCATE(TM%TPN%XALB_PANEL       (ILU))
  ALLOCATE(TM%TPN%XEFF_PANEL       (ILU))
  ALLOCATE(TM%TPN%XFRAC_PANEL      (ILU))
  !
  TM%T%CUR%XROAD_DIR(:) = 0.
  TM%T%CUR%XROAD    (:) = 0.
  !
  ZDEF_ROAD_DIR = 0.
  IF (TM%TOP%CROAD_DIR/='UNIF') THEN
    !* road direction if not specified by the user depends on patch number
    !  First patch has a Notrh-South road. Other patches have roads spaced by
    !  regular angles
    ZDEF_ROAD_DIR = 180. * FLOAT(JPATCH-1) / FLOAT(TM%TOP%NTEB_PATCH)
  END IF
  !
  CALL CONVERT_PATCH_TEB(TM%BDD, TM%DTB, DTCO, TM%DTT, TM%TOP, &
                         TM%TOP%XCOVER, TM%TOP%LCOVER, ZDEF_ROAD_DIR,                   &
                      PZ0_TOWN=TM%T%CUR%XZ0_TOWN, PALB_ROOF=TM%T%CUR%XALB_ROOF,         &
                      PEMIS_ROOF=TM%T%CUR%XEMIS_ROOF,PHC_ROOF=TM%T%CUR%XHC_ROOF,PTC_ROOF=TM%T%CUR%XTC_ROOF, &
                      PD_ROOF=TM%T%CUR%XD_ROOF, PALB_ROAD=TM%T%CUR%XALB_ROAD,                                &
                      PEMIS_ROAD=TM%T%CUR%XEMIS_ROAD,PHC_ROAD=TM%T%CUR%XHC_ROAD,PTC_ROAD=TM%T%CUR%XTC_ROAD, &
                      PD_ROAD=TM%T%CUR%XD_ROAD, PALB_WALL=TM%T%CUR%XALB_WALL,                                &
                      PEMIS_WALL=TM%T%CUR%XEMIS_WALL,PHC_WALL=TM%T%CUR%XHC_WALL,PTC_WALL=TM%T%CUR%XTC_WALL, &
                      PD_WALL=TM%T%CUR%XD_WALL, PBLD_HEIGHT=TM%T%CUR%XBLD_HEIGHT,                        &
                      PWALL_O_HOR=TM%T%CUR%XWALL_O_HOR,PBLD=TM%T%CUR%XBLD, PROAD_DIR=TM%T%CUR%XROAD_DIR,    &
                      PGARDEN=TM%T%CUR%XGARDEN,                                           &
                      PH_TRAFFIC=TM%T%CUR%XH_TRAFFIC, PLE_TRAFFIC=TM%T%CUR%XLE_TRAFFIC,            &
                      PH_INDUSTRY=TM%T%CUR%XH_INDUSTRY, PLE_INDUSTRY=TM%T%CUR%XLE_INDUSTRY,        &
                      PROUGH_ROOF = TM%T%CUR%XROUGH_ROOF, PROUGH_WALL = TM%T%CUR%XROUGH_WALL,      &
                      PRESIDENTIAL = TM%T%CUR%XRESIDENTIAL,                               &
                      PGREENROOF = TM%T%CUR%XGREENROOF,                                   &
                      PEMIS_PANEL=TM%TPN%XEMIS_PANEL, PALB_PANEL=TM%TPN%XALB_PANEL,            &
                      PEFF_PANEL=TM%TPN%XEFF_PANEL, PFRAC_PANEL=TM%TPN%XFRAC_PANEL             )
  !
  IF (.NOT. TM%TOP%LGREENROOF .AND. MAXVAL(TM%T%CUR%XGREENROOF)>0. ) THEN !<== A paralleliser pour un stop propre
    WRITE(ILUOUT,*) 'You choose NOT to have greenroofs, BUT your greenroof fraction is not zero'
    WRITE(ILUOUT,*) 'Please activate the greenroof option (and rerun the SURFEX suite from the PGD step)'
    WRITE(ILUOUT,*) 'Or be sure NOT to have any greenroofs in your area'
    CALL ABOR1_SFX('INIT_TEBN: GREENROOF OPTION NOT ACTIVATED WHILE GREENROOFS ARE PRESENT')
  ENDIF
  !
  IF (.NOT. TM%TOP%LSOLAR_PANEL .AND. MAXVAL(TM%TPN%XFRAC_PANEL)>0. ) THEN !<== A paralleliser pour un stop propre
    WRITE(ILUOUT,*) 'You choose NOT to have solar panels, BUT your solar panel fraction is not zero'
    WRITE(ILUOUT,*) 'Please activate the solar panel option (and rerun the SURFEX suite from the PGD step)'
    WRITE(ILUOUT,*) 'Or be sure NOT to have any solar panel in your area'
    CALL ABOR1_SFX('INIT_TEBN: SOLAR_PANEL OPTION NOT ACTIVATED WHILE SOLAR PANELS ARE PRESENT')
  ENDIF
  !
  !-------------------------------------------------------------------------------
  !
  !*       5.     Sky-view-factors:
  !               ----------------
  !
  ALLOCATE(TM%T%CUR%XSVF_ROAD  (ILU))
  ALLOCATE(TM%T%CUR%XSVF_GARDEN(ILU))
  ALLOCATE(TM%T%CUR%XSVF_WALL  (ILU))
  !
  ALLOCATE(TM%B%CUR%XGR          (ILU))
  ALLOCATE(TM%B%CUR%XALB_WIN     (ILU))
  ALLOCATE(TM%B%CUR%XF_WASTE_CAN (ILU))
  !
  !
  CALL TEB_MORPHO(HPROGRAM, TM%T%CUR%XBLD, TM%T%CUR%XWALL_O_HOR, TM%T%CUR%XGARDEN, TM%T%CUR%XBLD_HEIGHT, &
                  TM%T%CUR%XROAD, TM%T%CUR%XROAD_O_GRND, TM%T%CUR%XGARDEN_O_GRND, TM%T%CUR%XWALL_O_GRND, &
                  TM%T%CUR%XCAN_HW_RATIO, TM%T%CUR%XSVF_ROAD, TM%T%CUR%XSVF_GARDEN, TM%T%CUR%XSVF_WALL, &
                  TM%T%CUR%XZ0_TOWN, TM%T%CUR%XWALL_O_BLD, TM%T%CUR%XH_TRAFFIC, TM%T%CUR%XLE_TRAFFIC      )
                !
  !-------------------------------------------------------------------------------
  !
  !*       6.     Building Energy Model
  !               ---------------------
  !
  CALL INIT_BEM_n(DGU, DTCO, UG, U, TM, &
                  ILUOUT)
  !
  !-------------------------------------------------------------------------------
  !
  !*      7.      Case of urban green areas
  !               -------------------------
  !
  IF (TM%TOP%LGARDEN) THEN
  !
    CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                        HPROGRAM,'TOWN  ','TEB   ','READ ')     
    IF (JPATCH==1) CALL INIT_TEB_VEG_OPTIONS_n(&
                                               TM%CHT, TM%DGMTO, GDM%TGDO, GDM%TVG, &
                                               HPROGRAM)
    CALL INIT_TEB_GARDEN_PGD_n(DTCO, U, CHI, DTI, I, DST, SLT, TM%CHT, TM%TG, TM%T, TM%TOP, GDM, &
                               HPROGRAM,HINIT,(JPATCH==1),KI,KSV,HSV,IVERSION,IBUGFIX,PCO2,PRHOA)
    ! Case of urban green roofs
    IF (TM%TOP%LGREENROOF) CALL INIT_TEB_GREENROOF_PGD_n(DTCO, U, CHI, DTI, I, DST, SLT, &
                                       TM%CHT, TM%TG, TM%T, TM%TOP, GDM%TVG, GRM, &
                                        HPROGRAM,HINIT,(JPATCH==1),KI,KSV,HSV,IVERSION,PCO2,PRHOA)
    CALL END_IO_SURF_n(HPROGRAM)
    !
  ENDIF
!-------------------------------------------------------------------------------
END DO ! end of loop on TEB patches
!-------------------------------------------------------------------------------
!
!* Read irrigation parameters for TEB
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                        HPROGRAM,'TOWN  ','TEB   ','READ ')     
 CALL READ_PGD_TEB_IRRIG_n(&
                           TM%TG, GDM%TIR, &
                           HPROGRAM)
 CALL END_IO_SURF_n(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!* if only physiographic fields are to be initialized, stop here.
!
IF (HINIT/='ALL' .AND. HINIT/='SOD') THEN
  IF (LHOOK) CALL DR_HOOK('INIT_TEB_N',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                        HPROGRAM,'TOWN  ','TEB   ','READ ')
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
DO JPATCH=1,TM%TOP%NTEB_PATCH
  CALL GOTO_WRAPPER_TEB_PATCH(TM%B, TM%DGCT, TM%DGMT, TM%T, &
                        GDM%TGD, GDM%TGDPE, GRM%TGR, GRM%TGRPE, JPATCH)
!
!* TEB fields
  CALL READ_TEB_n(TM%B, TM%BOP, DTCO, DGU, U, TM%T, TM%TOP, TM%TPN, &
                  HPROGRAM,JPATCH)
!
  ALLOCATE(TM%T%CUR%XAC_ROOF    (ILU))
  ALLOCATE(TM%T%CUR%XAC_ROAD    (ILU))
  ALLOCATE(TM%T%CUR%XAC_WALL    (ILU))
  ALLOCATE(TM%T%CUR%XAC_TOP     (ILU))
  ALLOCATE(TM%T%CUR%XAC_ROOF_WAT(ILU))
  ALLOCATE(TM%T%CUR%XAC_ROAD_WAT(ILU))
  ALLOCATE(TM%T%CUR%XQSAT_ROOF  (ILU))
  ALLOCATE(TM%T%CUR%XQSAT_ROAD  (ILU))
  ALLOCATE(TM%T%CUR%XDELT_ROOF  (ILU))
  ALLOCATE(TM%T%CUR%XDELT_ROAD  (ILU))
!
!* Case of urban green areas
  IF (TM%TOP%LGARDEN) THEN
!    CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! change input file name to pgd name
!    CALL INIT_IO_SURF_n(HPROGRAM,'TOWN  ','TEB   ','READ ')       
    CALL INIT_TEB_GARDEN_n(DTCO, DGU, UG, U, TM%DGMTO, TM%TOP, GDM, &
                           HPROGRAM,HINIT,KI,KSW,PSW_BANDS,JPATCH)
  ! Case of urban green roofs
    IF (TM%TOP%LGREENROOF) CALL INIT_TEB_GREENROOF_n(DTCO, U, TM%DGMTO, TM%TOP, GDM%TVG, GRM, &
                                                  HPROGRAM,HINIT,KI,KSV,PSW_BANDS,JPATCH)
!    CALL END_IO_SURF_n(HPROGRAM)
  ENDIF
!-------------------------------------------------------------------------------
!
!*      10.     Infra-red Radiative fields:
!               --------------------------
!
!* snow long-wave properties (not initialized in read_gr_snow)
!
  CALL INIT_SNOW_LW(XEMISSN,TM%T%CUR%TSNOW_ROOF)
  CALL INIT_SNOW_LW(XEMISSN,TM%T%CUR%TSNOW_ROAD)
!
  IF (TM%TOP%LGARDEN) THEN
    ZDIR_SW=0. ! night as first guess for albedo computation
    ZSCA_SW=0. !
    CALL GARDEN_PROPERTIES(TM%T, GDM, &
                           ZDIR_SW, ZSCA_SW, PSW_BANDS, KSW,     &
                           ZTS_GARDEN, ZEMIS_GARDEN, ZALB_GARDEN )      
  ELSE
    ZALB_GARDEN = XUNDEF
    ZEMIS_GARDEN= XUNDEF
    ZTS_GARDEN  = XUNDEF
  END IF
  !
  IF (TM%TOP%LGREENROOF) THEN
    ZDIR_SW=0. ! night as first guess for albedo computation
    ZSCA_SW=0. !
    CALL GREENROOF_PROPERTIES(TM%T, GDM%TVG, GRM, & 
                              ZDIR_SW, ZSCA_SW, PSW_BANDS, KSW,              &
                              ZTS_GREENROOF, ZEMIS_GREENROOF, ZALB_GREENROOF )  
  ELSE
    ZALB_GREENROOF  = XUNDEF
    ZEMIS_GREENROOF = XUNDEF
    ZTS_GREENROOF   = XUNDEF
  END IF
!
!* averaged albedo, emissivity and radiative temperature
!
  CALL AVERAGED_TSRAD_TEB(TM%T%CUR%XEMIS_ROOF,TM%T%CUR%XT_ROOF(:,1),       &
                        TM%T%CUR%XEMIS_ROAD,TM%T%CUR%XT_ROAD(:,1),       &
                        TM%T%CUR%XEMIS_WALL,                    &
                        TM%T%CUR%XT_WALL_A(:,1),                &
                        TM%T%CUR%XT_WALL_B(:,1),                &
                        ZEMIS_GARDEN, ZTS_GARDEN,      &
                        ZEMIS_GREENROOF, ZTS_GREENROOF,&
                        TM%T%CUR%TSNOW_ROOF,TM%T%CUR%TSNOW_ROAD,         &
                        TM%T%CUR%XROAD, TM%T%CUR%XGREENROOF, TM%T%CUR%XGARDEN,    &
                        TM%T%CUR%XBLD,TM%T%CUR%XWALL_O_HOR,              &
                        TM%T%CUR%XSVF_ROAD,TM%T%CUR%XSVF_WALL,           &
                        TM%T%CUR%XSVF_GARDEN,                   &
                        PEMIS,PTSRAD, TM%B%CUR%XT_WIN1,         &
                        TM%B%CUR%XGR                            )
!
!
!*       9.     Visible and near-infra-red Radiative fields:
!               -------------------------------------------
!
  ALLOCATE(ZDIR_ALB(ILU))
  ALLOCATE(ZSCA_ALB(ILU))
!
  CALL AVERAGED_ALBEDO_TEB(TM%TOP%CBEM,TM%TOP%CROAD_DIR,TM%TOP%CWALL_OPT,PZENITH,PAZIM, &
                       TM%T%CUR%XBLD, TM%T%CUR%XGARDEN, TM%T%CUR%XROAD_DIR, TM%T%CUR%XROAD, &
                       TM%T%CUR%XGREENROOF, TM%TPN%XFRAC_PANEL, TM%TPN%XALB_PANEL,       &
                       TM%T%CUR%XWALL_O_HOR, TM%T%CUR%XCAN_HW_RATIO,                 &
                       TM%T%CUR%XALB_ROOF, TM%T%CUR%XALB_ROAD, TM%T%CUR%XSVF_ROAD,   &
                       TM%T%CUR%XALB_WALL, TM%T%CUR%XSVF_WALL,                       &
                       ZALB_GARDEN, TM%T%CUR%XSVF_GARDEN, ZALB_GREENROOF,            &
                       TM%T%CUR%TSNOW_ROOF, TM%T%CUR%TSNOW_ROAD,                     &
                       TM%B%CUR%XGR, TM%B%CUR%XSHGC, TM%B%CUR%XSHGC_SH, &
                       TM%B%CUR%XABS_WIN, TM%B%CUR%XALB_WIN,   &
                       TM%B%CUR%LSHAD_DAY,                                  &
                       ZDIR_ALB, ZSCA_ALB, TM%B%CUR%XTRAN_WIN               )  

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
  CALL INIT_CHEMICAL_n(ILUOUT, KSV, HSV, TM%CHT%SVT,          &
                     TM%CHT%CCH_NAMES, TM%CHT%CAER_NAMES,     &
                     HDSTNAMES=TM%CHT%CDSTNAMES, HSLTNAMES=TM%CHT%CSLTNAMES        )
!
!* Initialization of dry deposition scheme (chemistry)
!
  IF (TM%CHT%SVT%NBEQ>0 .AND. TM%CHT%CCH_DRY_DEP=='WES89') THEN
    ALLOCATE(TM%CHT%XDEP(ILU,TM%CHT%SVT%NBEQ))
  ELSE
    ALLOCATE(TM%CHT%XDEP(0,0))
  END IF
!
!-------------------------------------------------------------------------------
END DO ! end of loop on patches
!
IF (HINIT/='ALL') THEN
  CALL END_IO_SURF_n(HPROGRAM)
  IF (LHOOK) CALL DR_HOOK('INIT_TEB_N',1,ZHOOK_HANDLE)
  RETURN
END IF
!-------------------------------------------------------------------------------
!
!*       7.     Canopy air fields:
!               ------------------
!
 CALL READ_TEB_CANOPY_n(DTCO, U, TM%TCP, TM%TOP, &
                        HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*      11.     Diagnostics:
!               -----------
!
 CALL DIAG_TEB_INIT_n(TM%DGT, TM%DGUT, &
                      HPROGRAM,ILU,ISWB)
DO JPATCH=1,TM%TOP%NTEB_PATCH
  CALL GOTO_WRAPPER_TEB_PATCH(TM%B, TM%DGCT, TM%DGMT, TM%T, &
                        GDM%TGD, GDM%TGDPE, GRM%TGR, GRM%TGRPE, JPATCH)
  CALL DIAG_MISC_TEB_INIT_n(TM%DGCT, TM%DGMT, TM%DGMTO, TM%TOP, &
                            HPROGRAM,ILU,ISWB)
END DO ! end of loop on patches
#ifdef MNH_PARALLEL
 CALL MPPDB_CHECK_SURFEX3D(TM%TOP%XCOVER,"INIT_TEB_n end:XCOVER",PRECISION,ILUOUT, 'TOWN  ',JPCOVER)
#endif
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
