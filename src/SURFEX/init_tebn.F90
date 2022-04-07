!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################################################
      SUBROUTINE INIT_TEB_n (DTCO, UG, U, GCP, CHT, DTT, SB, TG, SPAOP, TOP, TPN, &
                             TIR, NT, TD, BDD, BOP, DTB, NB, TM_AT, GDM, GRM, &
                             HM, HPROGRAM, HINIT, KI, KSV, KSW, HSV, PCO2,        &
                             PRHOA, PZENITH, PAZIM, PSW_BANDS, PDIR_ALB,      &
                             PSCA_ALB, PEMIS, PTSRAD, PTSURF, KYEAR, KMONTH,  &
                             KDAY, PTIME, AT, HATMFILE, HATMFILETYPE, HTEST )  
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
!!      E.Redon/A.Lemonsu        12/2015  street trees
!!                               12/2015  air temperature TA as arguments to defined tree temperature
!!      K.Chancibault/A.Lemonsu  01/2016  urban hydrology
!!      M. Goret                 04/2017  add check on the date of run start
!!      M. Goret                 04/2017  add further allocation for CO2 fluxes
!!      M. Goret                 05/2017  add HPROGRAM as INIT_TEB_SOIL_PGD_n arg. for writing
!!      M. Goret                 08/2017  add TEB_GARDEN diagnostics
!!      C. de Munck              10/2019  added TEB-GARDEN cumulative diagnostics  
!!                               10/2019  added TEB-GREENROOF cumulative diagnostics
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_CH_TEB_n, ONLY : CH_TEB_t
USE MODD_CSTS, ONLY : XSURF_EPSILON
USE MODD_DATA_TEB_n, ONLY : DATA_TEB_t
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t, DIAG_MISC_TEB_INIT
USE MODD_CANOPY_n, ONLY: CANOPY_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_SPARTACUS_OPTION_n, ONLY : SPARTACUS_OPTIONS_t
USE MODD_TEB_PANEL_n, ONLY : TEB_PANEL_t
USE MODD_TEB_IRRIG_n, ONLY : TEB_IRRIG_t
USE MODD_TEB_n, ONLY : TEB_NP_t
USE MODD_SURFEX_n, ONLY : TEB_DIAG_t, TEB_VEG_DIAG_t, TEB_GARDEN_MODEL_t, TEB_GREENROOF_MODEL_t, TEB_HYDRO_MODEL_t
USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_DATA_BEM_n, ONLY : DATA_BEM_t
USE MODD_BEM_n, ONLY : BEM_NP_t
!
USE MODD_SNOW_PAR, ONLY : XEMISSN
!
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
!
USE MODD_SURF_PAR,        ONLY: XUNDEF, NUNDEF
USE MODD_TYPE_DATE_SURF
USE MODD_ISBA_PAR,        ONLY: XEMISVEG
USE MODD_TEB_PAR,         ONLY: XD_FLOOR_DEF, XHC_FLOOR_DEF, XTC_FLOOR_DEF, XTS_FLOOR
USE MODD_TEB_VEG
USE MODD_TEB_STRUCT_ROAD
!
USE MODD_SURF_ATM_TURB_n, ONLY : SURF_ATM_TURB_t
!
USE MODN_PREP_TEB, ONLY : CROAD_DIR, CWALL_OPT
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
USE MODI_READ_PGD_TEB_HYDRO_n
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
USE MODI_TEB_VEG_PROPERTIES
USE MODI_IS_A_REAL_DATE
!
USE MODI_INIT_TEB_GARDEN_n
USE MODI_INIT_TEB_GARDEN_PGD_n
USE MODI_INIT_TEB_VEG_OPTIONS_n
USE MODI_TEB_MORPHO
USE MODI_INIT_BEM_n
USE MODI_INIT_TEB_GREENROOF_n
USE MODI_INIT_TEB_GREENROOF_PGD_n
USE MODI_READ_PGD_TEB_IRRIG_n
USE MODI_INIT_TEB_ROAD_GRID_n
USE MODI_INIT_TEB_SOIL_PGD_n
USE MODI_INIT_TEB_HYDRO_n
USE MODI_BLD_OCC_CALENDAR
USE MODI_SUNPOS
USE MODI_URBTREE_PROPERTIES
USE MODI_THRMCONDZ
USE MODI_HEATCAPZ
!
USE MODE_SOIL
!
USE MODI_READ_COVER_GARDEN
USE MODI_ABOR1_SFX
USE MODI_READ_SBL_n
USE MODI_SET_SURFEX_FILEIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef MNH_PARALLEL
USE MODE_MPPDB
!
#endif
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE(CH_TEB_t), INTENT(INOUT) :: CHT 
TYPE(DATA_TEB_t), INTENT(INOUT) :: DTT
TYPE(CANOPY_t), INTENT(INOUT) :: SB
TYPE(GRID_t), INTENT(INOUT) :: TG
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(SPARTACUS_OPTIONS_t), INTENT(INOUT) :: SPAOP
TYPE(TEB_PANEL_t), INTENT(INOUT) :: TPN
TYPE(TEB_IRRIG_t), INTENT(INOUT) :: TIR
TYPE(TEB_NP_t), INTENT(INOUT) :: NT
!
TYPE(TEB_DIAG_t), INTENT(INOUT) :: TD
!
TYPE(BLD_DESC_t), INTENT(INOUT) :: BDD
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP 
TYPE(DATA_BEM_t), INTENT(INOUT) :: DTB
TYPE(BEM_NP_t), INTENT(INOUT) :: NB
!
TYPE(SURF_ATM_TURB_t), INTENT(INOUT) :: TM_AT         ! atmospheric turbulence parameters
!
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
TYPE(TEB_HYDRO_MODEL_t),     INTENT(INOUT) :: HM
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
TYPE(SURF_ATM_TURB_t), INTENT(IN) :: AT         ! atmospheric turbulence parameters
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
!              local variables for urban trees
INTEGER,PARAMETER               :: NCAN=2          ! Number of layers in the canyon
REAL, DIMENSION(KI)             :: ZEMIS_HVEG      ! emissivity
REAL, DIMENSION(KI,NCAN)        :: ZTRANS_HVEG     ! transmissivity profile by canyon zones
REAL, DIMENSION(KI  )           :: ZTRANS_HVCR     ! transmissivity profile for all height of tree crown
REAL, DIMENSION(KI)             :: ZTS_HVEG        ! radiative temperature
REAL, DIMENSION(KI)             :: ZALB_HVEG       ! albedo
!
  CHARACTER(LEN=12)               :: YRECFM           ! Name of the article to be read
REAL   , DIMENSION(KI)          :: ZPGD_CLAY        ! Clay fraction read in PGD file
REAL   , DIMENSION(KI)          :: ZPGD_SAND        ! Sand fraction read in PGD file
!
REAL, DIMENSION(KI)             :: ZTSUN            ! solar time (s from midnight)
REAL, DIMENSION(KI)             :: ZZENITH          ! solar zenith angle
REAL, DIMENSION(KI)             :: ZAZIM            ! solar azimuth angle
!
REAL, DIMENSION(KI)             :: ZSUM_FRAC        ! sum of horizontal surface fractions
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZSHAD_BEHAV_ADAPTI
REAL, DIMENSION(:,:), ALLOCATABLE :: ZSHAD_BEHAV_ANYWAY
REAL, DIMENSION(:,:), ALLOCATABLE :: ZISNIGHT
REAL, DIMENSION(:,:), ALLOCATABLE :: ZBLDOCC  
!
INTEGER                         :: JI
INTEGER                         :: JJ  
INTEGER                         :: JLAYER
INTEGER                         :: JCOMP
INTEGER                         :: JP
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
TD%MTO%LSURF_EVAP_BUDGET = .FALSE.
!
IF (LNAM_READ) THEN
 !
 !*       0.     Defaults
 !               --------
 !
 !        0.1. Hard defaults
 !  
  CALL DEFAULT_TEB(TOP%CZ0H, TOP%CZ0EFF_GD, TOP%XTSTEP, TOP%XOUT_TSTEP, TOP%CCH_BEM, TOP%CURB_LM, TPN%CSOLAR_PANEL)
  CALL DEFAULT_CH_DEP(CHT%CCH_DRY_DEP)
  CALL DEFAULT_DIAG_TEB(TD%O%N2M, TD%O%LSURF_BUDGET, TD%O%L2M_MIN_ZS, TD%O%LRAD_BUDGET,&
                        TD%O%LCOEF, TD%O%LSURF_VARS, TD%MTO%LSURF_MISC_BUDGET, &
                        TD%MTO%LSURF_DIAG_ALBEDO, TD%DU%LUTCI, TD%O%LPGD,     &
                        TD%O%XDIAG_TSTEP)  
!
ENDIF
!
!        0.2. Defaults from file header
!    
 CALL READ_DEFAULT_TEB_n(CHT, TD%MTO, TD%O, TD%DU, GRM%O, NT%AL(1), TOP, TPN, HPROGRAM)
!
!*       1.     Reading of configuration:
!               -------------------------
!
 CALL READ_TEB_CONF_n(CHT, TD%MTO, TD%O, TD%DU, NT%AL(1), TOP, TPN, HPROGRAM)
!
!* initialization of snow scheme
!
IF (HINIT=='PRE') THEN
  CALL READ_PREP_TEB_SNOW(HPROGRAM, NT%AL(1)%TSNOW_ROOF%SCHEME, NT%AL(1)%TSNOW_ROOF%NLAYER, &
                                    NT%AL(1)%TSNOW_ROAD%SCHEME, NT%AL(1)%TSNOW_ROAD%NLAYER)
ENDIF
!
!*       2.     Cover fields and grid:
!               ---------------------
!* date
!
SELECT CASE (HINIT)
  CASE ('PGD')
    TOP%TTIME%TDATE%YEAR = NUNDEF
    TOP%TTIME%TDATE%MONTH= NUNDEF
    TOP%TTIME%TDATE%DAY  = NUNDEF
    TOP%TTIME%TIME       = XUNDEF

  CASE ('PRE')
    CALL PREP_CTRL_TEB(TD%O, TD%MTO%LSURF_EVAP_BUDGET, TD%MTO%LSURF_MISC_BUDGET, TD%DU%LUTCI,ILUOUT )           
    IF (LNAM_READ) CALL READ_NAM_PREP_TEB_n(HPROGRAM)   
    CALL READ_TEB_DATE(HPROGRAM,HINIT,ILUOUT,HATMFILE,HATMFILETYPE,KYEAR,KMONTH,KDAY,PTIME,TOP%TTIME)
    IF (.NOT. IS_A_REAL_DATE(TOP%TTIME)) THEN
       WRITE(ILUOUT,*) "The following date: ", TOP%TTIME%TDATE%DAY, TOP%TTIME%TDATE%MONTH, &
            TOP%TTIME%TDATE%YEAR, TOP%TTIME%TIME
       WRITE(ILUOUT,*) "doesn't exist"
       WRITE(ILUOUT,*) "Please provide a true date as starting date of the simulation"
       IF (LHOOK) CALL DR_HOOK('INIT_TEB_N',1,ZHOOK_HANDLE)
       CALL ABOR1_SFX("Wrong starting date")
    ENDIF

  CASE DEFAULT
    CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'FULL  ','SURF  ','READ ')
    CALL READ_SURF(HPROGRAM,'DTCUR',TOP%TTIME,IRESP)
    CALL END_IO_SURF_n(HPROGRAM)
    IF (.NOT. IS_A_REAL_DATE(TOP%TTIME)) THEN
       WRITE(ILUOUT,*) "The following date: ", TOP%TTIME%TDATE%DAY, TOP%TTIME%TDATE%MONTH, &
            TOP%TTIME%TDATE%YEAR, TOP%TTIME%TIME
       WRITE(ILUOUT,*) "doesn't exist"
       WRITE(ILUOUT,*) "Please provide a true date as starting date of the simulation"
       IF (LHOOK) CALL DR_HOOK('INIT_TEB_N',1,ZHOOK_HANDLE)
       CALL ABOR1_SFX("Wrong starting date")
    ENDIF
  END SELECT
!
!-----------------------------------------------------------------------------------------------------
! READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
!         Initialisation for IO
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','READ ')
!
 CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
!
!         Reading of the fields
!
 CALL READ_COVER_GARDEN(HPROGRAM,TOP%LGARDEN)
!
 CALL READ_PGD_TEB_n(DTCO, U, UG, GCP, TOP, SPAOP, TG, BOP, BDD, DTB, DTT, HPROGRAM)
!
#ifdef MNH_PARALLEL
 CALL MPPDB_CHECK_SURFEX3D(TOP%XCOVER,"INIT_TEB_n after READ_PGD_TEB_n:XCOVER",PRECISION,ILUOUT, 'TOWN  ',SIZE(TOP%XCOVER,2))
#endif
!
CALL END_IO_SURF_n(HPROGRAM)
! 
!*        Fraction of each patch in the grid mesh
!
ILU = SIZE(TOP%XCOVER,1)
!
ALLOCATE(TOP%XTEB_PATCH(ILU,TOP%NTEB_PATCH))
 CALL CONVERT_TEB(TOP%NTEB_PATCH, TOP%XCOVER,TOP%XTEB_PATCH)
!
#ifdef MNH_PARALLEL
 CALL MPPDB_CHECK_SURFEX3D(TOP%XCOVER,"INIT_TEB_n after CONVERT_TEB:XCOVER",PRECISION,ILUOUT, 'TOWN  ',SIZE(TOP%XCOVER,2))
#endif
!
!* reads what is the option defined for road orientations & walls
!
IF (HINIT=='ALL') THEN
  !
  CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
  CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','READ ')
!
  CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
  CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
!
  TOP%CROAD_DIR='UNIF'
  TOP%CWALL_OPT='UNIF'
  IF (IVERSION>7 .OR. (IVERSION==7 .AND. IBUGFIX>=3)) THEN
    CALL READ_SURF(HPROGRAM,'ROAD_DIR',TOP%CROAD_DIR,IRESP)
    CALL READ_SURF(HPROGRAM,'WALL_OPT',TOP%CWALL_OPT,IRESP)
  END IF
  CALL END_IO_SURF_n(HPROGRAM)
  !
ELSE
  !
  TOP%CROAD_DIR = CROAD_DIR
  TOP%CWALL_OPT = CWALL_OPT
  !  
ENDIF
!   
!-----------------------------------------------------------------------------------
!
!                  Determines the characteristics of the road grid
!                  -----------------------------------------------
!
!
  CALL INIT_TEB_ROAD_GRID_n(HPROGRAM, TOP, DTCO, DTT, KI)
!
!-----------------------------------------------------------------------------------
!
!*              LOOP ON TEB PATCHES
!               -------------------
!
DO JP=1,TOP%NTEB_PATCH
  !
  !-----------------------------------------------------------------------------------
  !
  !*       3.     Definition of soil columns for roads and buildings
  !               --------------------------------------------------
  !
 IF (JP==1) THEN
  ! Default values for clay and sand fractions (if not GARDEN for versions below Surfex8)
  ZPGD_CLAY(:) = 0.33
  ZPGD_SAND(:) = 0.33
  !
  !IF ( (IVERSION>8) .OR. (IVERSION==8 .AND. IBUGFIX>=2) .OR. TOP%LGARDEN) THEN
  IF (IVERSION>=9 .OR. TOP%LGARDEN) THEN
     CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
     CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','READ ')     
     YRECFM='TWN_CLAY'
     IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_CLAY'
     CALL READ_SURF(HPROGRAM,YRECFM,ZPGD_CLAY(:),IRESP)
     YRECFM='TWN_SAND'
     IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_SAND'
     CALL READ_SURF(HPROGRAM,YRECFM,ZPGD_SAND(:),IRESP)
     CALL END_IO_SURF_n(HPROGRAM)
  ENDIF
 ENDIF
  !
  CALL INIT_TEB_SOIL_PGD_n(HPROGRAM, TOP, NT%AL(JP), GDM, KI, ZPGD_CLAY, ZPGD_SAND)
  !
  !-----------------------------------------------------------------------------------
  ! Read vegetation options if LGARDEN
  GDM%O%NGROUND_LAYER=TOP%NTEB_SOIL
  IF (TOP%LGARDEN) THEN
     !
     IF (JP==1) THEN    
       GDM%O%NGROUND_LAYER=TOP%NTEB_SOIL
       CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
        CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','READ ')     
        CALL INIT_TEB_VEG_OPTIONS_n(CHT, TD%MTO%LSURF_DIAG_ALBEDO, TOP%LGREENROOF, GDM%O, GRM%O, TOP, HPROGRAM, HINIT)
        CALL END_IO_SURF_n(HPROGRAM)
     ENDIF
     !
     ! Garden grid set equal to TEB soil grid
     IF (GDM%O%CISBA=='DIF') THEN
        ALLOCATE(GDM%O%XSOILGRID(GDM%O%NGROUND_LAYER))
        GDM%O%XSOILGRID = TOP%XTEB_SOILGRID 
     END IF
     !
  ENDIF
  !
  ! Clay and sand fraction in garden also initialized here
  ALLOCATE(GDM%K%XCLAY(KI,GDM%O%NGROUND_LAYER))
  ALLOCATE(GDM%K%XSAND(KI,GDM%O%NGROUND_LAYER))
  DO JLAYER=1,GDM%O%NGROUND_LAYER
     GDM%K%XCLAY(:,JLAYER) = ZPGD_CLAY(:)
     GDM%K%XSAND(:,JLAYER) = ZPGD_SAND(:)
  ENDDO
  !
  !
  !-----------------------------------------------------------------------------------
  !
  !*       3.     Physiographic data fields from land cover:
  !               -----------------------------------------
  !
  ALLOCATE(NT%AL(JP)%XZ0_TOWN     (ILU))
  ALLOCATE(NT%AL(JP)%XALB_ROOF    (ILU))
  ALLOCATE(NT%AL(JP)%XEMIS_ROOF   (ILU))
  ALLOCATE(NT%AL(JP)%XALB_ROAD    (ILU))
  ALLOCATE(NT%AL(JP)%XEMIS_ROAD   (ILU))
  ALLOCATE(NT%AL(JP)%XALB_WALL    (ILU))
  ALLOCATE(NT%AL(JP)%XEMIS_WALL   (ILU))
  ALLOCATE(NT%AL(JP)%XBLD         (ILU))
  ALLOCATE(NT%AL(JP)%XGARDEN      (ILU))
  ALLOCATE(NT%AL(JP)%XROAD        (ILU))
  ALLOCATE(NT%AL(JP)%XTOTS_O_HORS (ILU))
  ALLOCATE(NT%AL(JP)%XGREENROOF   (ILU))
  ALLOCATE(NT%AL(JP)%XURBTREE     (ILU))
  ALLOCATE(NT%AL(JP)%XFRAC_HVEG   (ILU))
  ALLOCATE(NT%AL(JP)%XFRAC_LVEG   (ILU))
  ALLOCATE(NT%AL(JP)%XFRAC_NVEG   (ILU))
  ALLOCATE(NT%AL(JP)%XROAD_DIR    (ILU))
  ALLOCATE(NT%AL(JP)%XBLD_HEIGHT  (ILU))
  ALLOCATE(NT%AL(JP)%XWALL_O_HOR  (ILU))
  ALLOCATE(NT%AL(JP)%XCAN_HW_RATIO(ILU))
  ALLOCATE(NT%AL(JP)%XROAD_O_GRND (ILU))
  ALLOCATE(NT%AL(JP)%XGARDEN_O_GRND(ILU))
  ALLOCATE(NT%AL(JP)%XWALL_O_GRND (ILU))
  ALLOCATE(NT%AL(JP)%XWALL_O_BLD(  ILU))
  ALLOCATE(NT%AL(JP)%XH_TRAFFIC   (ILU))
  ALLOCATE(NT%AL(JP)%XLE_TRAFFIC  (ILU))
  ALLOCATE(NT%AL(JP)%XH_INDUSTRY  (ILU))
  ALLOCATE(NT%AL(JP)%XLE_INDUSTRY (ILU))
  ALLOCATE(NT%AL(JP)%XHC_ROOF     (ILU,TOP%NROOF_LAYER))
  ALLOCATE(NT%AL(JP)%XTC_ROOF     (ILU,TOP%NROOF_LAYER))
  ALLOCATE(NT%AL(JP)%XD_ROOF      (ILU,TOP%NROOF_LAYER))
  ALLOCATE(NT%AL(JP)%XHC_WALL     (ILU,TOP%NWALL_LAYER))
  ALLOCATE(NT%AL(JP)%XTC_WALL     (ILU,TOP%NWALL_LAYER))
  ALLOCATE(NT%AL(JP)%XD_WALL      (ILU,TOP%NWALL_LAYER))
  ALLOCATE(NT%AL(JP)%XROUGH_ROOF  (ILU))
  ALLOCATE(NT%AL(JP)%XROUGH_WALL  (ILU))
  ALLOCATE(NT%AL(JP)%XNB_POP      (ILU))
  ALLOCATE(NT%AL(JP)%XSFCO2_RD    (ILU))
  ALLOCATE(NT%AL(JP)%XDELTA_LEGAL_TIME (ILU,TOP%NTIME_CHANGE+1))
  ALLOCATE(NT%AL(JP)%XTIME_OF_CHANGE   (TOP%NTIME_CHANGE))
  ALLOCATE(TPN%XEMIS_PANEL    (ILU))
  ALLOCATE(TPN%XALB_PANEL     (ILU))
  ALLOCATE(TPN%XEFF_PANEL     (ILU))
  ALLOCATE(TPN%XFRAC_PANEL    (ILU))
  !
  ! New discretization for road and building soil column
  ALLOCATE(NT%AL(JP)%XHC_ROAD (ILU,TOP%NTEB_SOIL))
  ALLOCATE(NT%AL(JP)%XTC_ROAD (ILU,TOP%NTEB_SOIL))
  ALLOCATE(NT%AL(JP)%XD_ROAD  (ILU,TOP%NTEB_SOIL))
  ALLOCATE(NT%AL(JP)%XHC_BLD  (ILU,TOP%NTEB_SOIL))
  ALLOCATE(NT%AL(JP)%XTC_BLD  (ILU,TOP%NTEB_SOIL))
  ALLOCATE(NT%AL(JP)%XD_BLD   (ILU,TOP%NTEB_SOIL))
  !
  ! Variables related with energy and moisture storage
  !
  ALLOCATE(NT%AL(JP)%XTHEWALL   (ILU))
  ALLOCATE(NT%AL(JP)%XTHEROOF   (ILU))
  ALLOCATE(NT%AL(JP)%XTHEFLOOR  (ILU))
  ALLOCATE(NT%AL(JP)%XTHESOILBLD(ILU))
  ALLOCATE(NT%AL(JP)%XTHEMASS   (ILU))
  ALLOCATE(NT%AL(JP)%XTHEROAD   (ILU))
  ALLOCATE(NT%AL(JP)%XTHEAIRIN  (ILU))
  ALLOCATE(NT%AL(JP)%XTHETOTAL  (ILU))
  ALLOCATE(NT%AL(JP)%XLATWATROOF(ILU))
  ALLOCATE(NT%AL(JP)%XLATWATROAD(ILU))
  ALLOCATE(NT%AL(JP)%XLATICEROOF(ILU))
  ALLOCATE(NT%AL(JP)%XLATICEROAD(ILU))
  ALLOCATE(NT%AL(JP)%XLATAIRIN  (ILU))
  ALLOCATE(NT%AL(JP)%XLATSOILROAD(ILU))
  ALLOCATE(NT%AL(JP)%XLATSOILBLD(ILU))
  ALLOCATE(NT%AL(JP)%XLATTOTAL  (ILU))
  ALLOCATE(NT%AL(JP)%XENETOTAL  (ILU))
  !
  NT%AL(JP)%XTHEWALL   (:) = XUNDEF
  NT%AL(JP)%XTHEROOF   (:) = XUNDEF
  NT%AL(JP)%XTHEFLOOR  (:) = XUNDEF
  NT%AL(JP)%XTHESOILBLD(:) = XUNDEF
  NT%AL(JP)%XTHEMASS   (:) = XUNDEF
  NT%AL(JP)%XTHEROAD   (:) = XUNDEF
  NT%AL(JP)%XTHEAIRIN  (:) = XUNDEF
  NT%AL(JP)%XTHETOTAL  (:) = XUNDEF
  NT%AL(JP)%XLATWATROOF(:) = XUNDEF
  NT%AL(JP)%XLATWATROAD(:) = XUNDEF
  NT%AL(JP)%XLATICEROOF(:) = XUNDEF
  NT%AL(JP)%XLATICEROAD(:) = XUNDEF
  NT%AL(JP)%XLATAIRIN  (:) = XUNDEF
  NT%AL(JP)%XLATSOILROAD(:)= XUNDEF
  NT%AL(JP)%XLATSOILBLD(:) = XUNDEF
  NT%AL(JP)%XLATTOTAL  (:) = XUNDEF
  NT%AL(JP)%XENETOTAL  (:) = XUNDEF
  !
  NT%AL(JP)%XROAD_DIR(:) = 0.
  NT%AL(JP)%XROAD    (:) = 0.
  !
  !-----------------------------------------------------------------------------------
  !-----------------------------------------------------------------------------------
  !
  !     4. TEB patches definition
  !        ----------------------
  !
  !-----------------------------------------------------------------------------------
  !-----------------------------------------------------------------------------------
  !
  ! If several patches are used, those are (currently) differentiated by road directions.
  ! User could change this to have patches representing a diversity of another sort
  !
  !
  ZDEF_ROAD_DIR = 0.
  IF (TOP%NTEB_PATCH>1) THEN
    IF (TOP%CROAD_DIR/='UNIF') THEN
       !* road direction if not specified by the user depends on patch number
       !  First patch has a Notrh-South road. Other patches have roads spaced by
       !  regular angles
       ZDEF_ROAD_DIR = 180. * FLOAT(JP-1) / FLOAT(TOP%NTEB_PATCH)
    END IF
  END IF
  !
  CALL CONVERT_PATCH_TEB (HPROGRAM, BDD, DTB, DTCO, DTT, TOP, ZDEF_ROAD_DIR,    &
                             T=NT%AL(JP), TPN=TPN  )
  !
  !       GARDEN fraction
  !       ---------------
  
  NT%AL(JP)%XGARDEN=NT%AL(JP)%XFRAC_LVEG + NT%AL(JP)%XFRAC_NVEG
  !
  !       URBTREE fraction in canyon
  !       --------------------------
  !
  IF (TOP%CURBTREE/='NONE') THEN
     NT%AL(JP)%XURBTREE=NT%AL(JP)%XFRAC_HVEG/(1.-NT%AL(JP)%XBLD)
  ELSE
     NT%AL(JP)%XURBTREE=0.
  END IF
  !
  IF (.NOT. TOP%LGREENROOF .AND. MAXVAL(NT%AL(JP)%XGREENROOF)>0. ) THEN
    WRITE(ILUOUT,*) 'You choose NOT to have greenroofs, BUT your greenroof fraction is not zero'
    WRITE(ILUOUT,*) 'Please activate the greenroof option (and rerun the SURFEX suite from the PGD step)'
    WRITE(ILUOUT,*) 'Or be sure NOT to have any greenroofs in your area'
    CALL ABOR1_SFX('INIT_TEBN: GREENROOF OPTION NOT ACTIVATED WHILE GREENROOFS ARE PRESENT')
  ENDIF
  !
  IF (.NOT. TOP%LSOLAR_PANEL .AND. MAXVAL(TPN%XFRAC_PANEL)>0. ) THEN
    WRITE(ILUOUT,*) 'You choose NOT to have solar panels, BUT your solar panel fraction is not zero'
    WRITE(ILUOUT,*) 'Please activate the solar panel option (and rerun the SURFEX suite from the PGD step)'
    WRITE(ILUOUT,*) 'Or be sure NOT to have any solar panel in your area'
    CALL ABOR1_SFX('INIT_TEBN: SOLAR_PANEL OPTION NOT ACTIVATED WHILE SOLAR PANELS ARE PRESENT')
  ENDIF
  !
  IF (TOP%CBEM/='BEM' .AND. TPN%CSOLAR_PANEL/='PV ') THEN
    WRITE(ILUOUT,*) "Option for solar panels CSOLAR_PANEL = ", TPN%CSOLAR_PANEL
    WRITE(ILUOUT,*) "can only be used if BEM is activated."
    WRITE(ILUOUT,*) "Here CBEM = ",TOP%CBEM," therefore CSOLAR_PANEL should be set to 'PV '"
    IF (LHOOK) CALL DR_HOOK('READ_TEB_CONF_n',1,ZHOOK_HANDLE)
    CALL ABOR1_SFX("CSOLAR_PANEL chosen option needs BEM activated")
  ENDIF
  !-------------------------------------------------------------------------------
  !
  ! coherence check on coverage fractions
  !
  ZSUM_FRAC = NT%AL(JP)%XBLD + NT%AL(JP)%XROAD + NT%AL(JP)%XFRAC_LVEG + NT%AL(JP)%XFRAC_NVEG
  IF (ANY(ABS(ZSUM_FRAC-1.)>1.E-6)) THEN
     WRITE(ILUOUT,*) 'The sum of fraction building (BLD), road (ROAD), low vegetation (FRAC_LVEG) and no vegetation (FRAC_NVEG)'
     WRITE(ILUOUT,*) 'are not equal to 1. Please check your input data and options in PGD namelists.'
     CALL ABOR1_SFX('INIT_TEBN: INCOHERENCE BETWEEN BLD, ROAD, LOW and NO URBAN VEGETATION FRACTIONS')
  ELSE
     NT%AL(JP)%XBLD       = NT%AL(JP)%XBLD       / ZSUM_FRAC
     NT%AL(JP)%XROAD      = NT%AL(JP)%XROAD      / ZSUM_FRAC
     NT%AL(JP)%XFRAC_LVEG = NT%AL(JP)%XFRAC_LVEG / ZSUM_FRAC
     NT%AL(JP)%XFRAC_NVEG = NT%AL(JP)%XFRAC_NVEG / ZSUM_FRAC
     NT%AL(JP)%XFRAC_HVEG = NT%AL(JP)%XFRAC_HVEG / ZSUM_FRAC
  END IF
  !----------------------------------------------------------------------------------!
  !
  ! cohernece check on HVEG fraction

  IF (ANY(NT%AL(JP)%XFRAC_HVEG > 1. - NT%AL(JP)%XBLD)) THEN
    WRITE(ILUOUT,*) 'The high vegetation must be contained entirely in the canyon'
    WRITE(ILUOUT,*) 'Therefore, FRAC_HVEG must be smaller than 1-BLD (1-building fraction).'
    CALL ABOR1_SFX('INIT_TEBN: FRACTION OF HIGH VEGETATION IS LARGER THAN CANYON FRACTION') 
  END IF

  !----------------------------------------------------------------------------------!

  ! computation of XTOTS_O_HORS
  !
  NT%AL(JP)%XTOTS_O_HORS = 1. + NT%AL(JP)%XWALL_O_HOR + NT%AL(JP)%XURBTREE *(1.-NT%AL(JP)%XBLD)
  !
  !
  !----------------------------------------------------------------------------------!
  !
  ! Update of thermal and hydrological properties :
  ! -----------------------------------------------
  !
  ! Soil layers under buildings
  ! Soil layer thicknesses :
  NT%AL(JP)%XD_BLD(:,1) = TOP%XTEB_SOILGRID(1)
  DO JLAYER=2,TOP%NTEB_SOIL
      NT%AL(JP)%XD_BLD(:,JLAYER) = TOP%XTEB_SOILGRID(JLAYER)-TOP%XTEB_SOILGRID(JLAYER-1)
  ENDDO
  ! Thermal properties :
  NT%AL(JP)%XHC_BLD(:,:) = NT%AL(JP)%XHCAPSOIL_BLD(:,:)
  NT%AL(JP)%XTC_BLD(:,:) = NT%AL(JP)%XCONDDRY_BLD (:,:)
  !
  ! For structural road, hydrological properties are prescribed
  ! (according to Bouilloud 2006)
  !
  DO JI=1,KI
    ! coating
    NT%AL(JP)%XBCOEF_ROAD  (JI,1:TOP%NCOAT_ROAD(JI)) = XBCOEF_COAT_ROAD
    NT%AL(JP)%XMPOTSAT_ROAD(JI,1:TOP%NCOAT_ROAD(JI)) = XMPOTSAT_COAT_ROAD
    NT%AL(JP)%XCONDSAT_ROAD(JI,1:TOP%NCOAT_ROAD(JI)) = XCONDSAT_COAT_ROAD
    NT%AL(JP)%XWSAT_ROAD   (JI,1:TOP%NCOAT_ROAD(JI)) = XWSAT_COAT_ROAD
    NT%AL(JP)%XWFC_ROAD    (JI,1:TOP%NCOAT_ROAD(JI)) = XWFC_COAT_ROAD
    NT%AL(JP)%XWWILT_ROAD  (JI,1:TOP%NCOAT_ROAD(JI)) = XWWILT_COAT_ROAD
    ! basement 
    ! keeping of GARDEN properties until realistic properties are estimated !cecile17112020
    !NT%AL(JP)%XBCOEF_ROAD  (JI,TOP%NCOAT_ROAD(JI)+1:TOP%NTEB_ROAD) = XBCOEF_BASE_ROAD
    !NT%AL(JP)%XMPOTSAT_ROAD(JI,TOP%NCOAT_ROAD(JI)+1:TOP%NTEB_ROAD) = XMPOTSAT_BASE_ROAD
    !NT%AL(JP)%XCONDSAT_ROAD(JI,TOP%NCOAT_ROAD(JI)+1:TOP%NTEB_ROAD) = XCONDSAT_BASE_ROAD
    !NT%AL(JP)%XWSAT_ROAD   (JI,TOP%NCOAT_ROAD(JI)+1:TOP%NTEB_ROAD) = XWSAT_BASE_ROAD
    !NT%AL(JP)%XWFC_ROAD    (JI,TOP%NCOAT_ROAD(JI)+1:TOP%NTEB_ROAD) = XWFC_BASE_ROAD
    !NT%AL(JP)%XWWILT_ROAD  (JI,TOP%NCOAT_ROAD(JI)+1:TOP%NTEB_ROAD) = XWWILT_BASE_ROAD
    !
  END DO
  !
  !-----------------------------------------------------------------------------------
  !
  !*       5.     Sky-view-factors:
  !               ----------------
  !
  ALLOCATE(NT%AL(JP)%XSVF_RS    (ILU))
  ALLOCATE(NT%AL(JP)%XSVF_WS    (ILU))
  !
  ALLOCATE(NB%AL(JP)%XGR          (ILU))
  ALLOCATE(NB%AL(JP)%XALB_WIN     (ILU))
  ALLOCATE(NB%AL(JP)%XF_WASTE_CAN (ILU))
  !
  CALL TEB_MORPHO(HPROGRAM, NT%AL(JP), TG)
  !
  ALLOCATE(NT%AL(JP)%XSVF_RW(ILU))
  ALLOCATE(NT%AL(JP)%XSVF_WR(ILU))
  ALLOCATE(NT%AL(JP)%XSVF_SW(ILU))
  ALLOCATE(NT%AL(JP)%XSVF_WW(ILU))
  NT%AL(JP)%XSVF_RW(:) = 1. - NT%AL(JP)%XSVF_RS(:)
  NT%AL(JP)%XSVF_WR(:) = NT%AL(JP)%XSVF_WS(:)
  NT%AL(JP)%XSVF_WW(:) = 1. - 2. * NT%AL(JP)%XSVF_WS(:)
  NT%AL(JP)%XSVF_SW(:) = 1. - NT%AL(JP)%XSVF_RS(:)
  !
  !-------------------------------------------------------------------------------
  !
  !*      7.      Case of urban green areas
  !               -------------------------
  !
  !
  ALLOCATE(NT%AL(JP)%XSVF_TS(ILU))
  ALLOCATE(NT%AL(JP)%XSVF_TR(ILU))
  ALLOCATE(NT%AL(JP)%XSVF_RT(ILU))
  ALLOCATE(NT%AL(JP)%XSVF_WT(ILU))
  ALLOCATE(NT%AL(JP)%XSVF_TW(ILU))
  ALLOCATE(NT%AL(JP)%XSVF_ST(ILU))
  !
  ALLOCATE(NT%AL(JP)%XTAU_WW(ILU))
  ALLOCATE(NT%AL(JP)%XTAU_SW(ILU))
  ALLOCATE(NT%AL(JP)%XTAU_SR(ILU))
  ALLOCATE(NT%AL(JP)%XTAU_WR(ILU))
  !
  NT%AL(JP)%XSVF_TS(:) = 0.
  NT%AL(JP)%XSVF_TR(:) = 0.
  NT%AL(JP)%XSVF_RT(:) = 0.
  NT%AL(JP)%XSVF_WT(:) = 0.
  NT%AL(JP)%XSVF_TW(:) = 0.
  NT%AL(JP)%XSVF_ST(:) = 0.
  !
  NT%AL(JP)%XTAU_SW(:) = 0.
  NT%AL(JP)%XTAU_SR(:) = 0.
  NT%AL(JP)%XTAU_WW(:) = 0.
  NT%AL(JP)%XTAU_WR(:) = 0.
  !
  IF (TOP%LGARDEN) THEN
  !
    CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
    CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','READ ')     
    !IF (JP==1) CALL INIT_TEB_VEG_OPTIONS_n(CHT, TD%MTO%LSURF_DIAG_ALBEDO, TOP%LGREENROOF, GDM%O, GRM%O, TOP, HPROGRAM, HINIT)
    CALL INIT_TEB_GARDEN_PGD_n(DTCO, U, CHT%LCH_BIO_FLUX, CHT%CPARAMBVOC, TG, NT%AL(JP)%XGARDEN, TOP, &
                               GDM%O, GDM%S, GDM%K, GDM%P, GDM%NPE%AL(JP), GDM%DTV, GDM%PHV, GDM%NPEHV%AL(JP),&
                               GDM%DTHV, NT%AL(JP), GDM%GB, HPROGRAM,HINIT,(JP==1),KI,IVERSION,IBUGFIX,&
                               PCO2,PRHOA)
  !
  ! Case of urban street trees
  ! --------------------------
   IF (TOP%CURBTREE=='TREE'.OR.TOP%CURBTREE=='GRWL') THEN
       CALL FLUSH(ILUOUT)
       !
       ! Height of maximum LAI for urban trees     
       GDM%PHV%XH_LAI_MAX(:) = GDM%PHV%XHTRUNK_HVEG(:) + (GDM%PHV%XH_TREE(:)-GDM%PHV%XHTRUNK_HVEG(:))/2.
       !
       ! Sky-view factors for urban trees
       NT%AL(JP)%XSVF_TS (:) =  SQRT( ( NT%AL(JP)%XCAN_HW_RATIO(:) * ( NT%AL(JP)%XBLD_HEIGHT(:)-GDM%PHV%XH_LAI_MAX(:) ) &
                /NT%AL(JP)%XBLD_HEIGHT(:) ) **2+1. )                                              &
                -    ( NT%AL(JP)%XCAN_HW_RATIO(:) * ( NT%AL(JP)%XBLD_HEIGHT(:)-GDM%PHV%XH_LAI_MAX(:) )   &
                /NT%AL(JP)%XBLD_HEIGHT(:) )
       !
       NT%AL(JP)%XSVF_TR (:) =  SQRT( ( NT%AL(JP)%XCAN_HW_RATIO(:)*GDM%PHV%XH_LAI_MAX(:) &
                /NT%AL(JP)%XBLD_HEIGHT(:) ) **2+1. )                &
                -    ( NT%AL(JP)%XCAN_HW_RATIO(:) * GDM%PHV%XH_LAI_MAX(:) &
                /NT%AL(JP)%XBLD_HEIGHT(:) )
       !
       ! New deduced view factors
       NT%AL(JP)%XSVF_ST(:) = NT%AL(JP)%XSVF_TS(:)
       NT%AL(JP)%XSVF_RT(:) = NT%AL(JP)%XSVF_TR(:)
       NT%AL(JP)%XSVF_WT(:) = 1. - 0.5 * (NT%AL(JP)%XSVF_ST(:) + NT%AL(JP)%XSVF_RT(:))
       NT%AL(JP)%XSVF_TW(:) = NT%AL(JP)%XSVF_WT(:)
       !
       ! Extinction coefficients
       CALL URBTREE_PROPERTIES (GDM%NPEHV%AL(JP),GDM%PHV, NT%AL(JP), NCAN, ZTRANS_HVEG, ZTRANS_HVCR, HPROGRAM)
       !
       NT%AL(JP)%XTAU_SW(:) = 1. - NT%AL(JP)%XURBTREE(:)*(1.-ZTRANS_HVEG(:,2))
       NT%AL(JP)%XTAU_SR(:) = 1. - NT%AL(JP)%XURBTREE(:)*(1.-ZTRANS_HVCR(:)  )
       NT%AL(JP)%XTAU_WW(:) = 1. - NT%AL(JP)%XURBTREE(:)*(1.-ZTRANS_HVCR(:)  )
       NT%AL(JP)%XTAU_WR(:) = 1. - NT%AL(JP)%XURBTREE(:)*(1.-ZTRANS_HVEG(:,1))
       ZEMIS_HVEG(:) = XEMISVEG
       ZALB_HVEG(:)  = 0.5 * (GDM%NPEHV%AL(JP)%XALBNIR_VEG + GDM%NPEHV%AL(JP)%XALBVIS_VEG)
    ELSE
       NT%AL(JP)%XSVF_TS(:) = 1. 
       NT%AL(JP)%XSVF_TR(:) = 1.  
       NT%AL(JP)%XSVF_ST(:) = 1. 
       NT%AL(JP)%XSVF_RT(:) = 1. 
       NT%AL(JP)%XSVF_WT(:) = 1. 
       NT%AL(JP)%XSVF_TW(:) = 1. 
       !
       NT%AL(JP)%XTAU_SW(:) = 1.
       NT%AL(JP)%XTAU_SR(:) = 1. 
       NT%AL(JP)%XTAU_WW(:) = 1.
       NT%AL(JP)%XTAU_WR(:) = 1. 
       ZEMIS_HVEG(:)        = 1.
       ZALB_HVEG(:)         = 1.
       !
       ZTRANS_HVCR(:)       = 1.
       ZTRANS_HVEG(:,:)     = 0.5

    ENDIF
    !
    ! Case of urban green roofs
    IF (TOP%LGREENROOF) THEN
      CALL INIT_TEB_GREENROOF_PGD_n(DTCO, U, CHT%LCH_BIO_FLUX, CHT%CPARAMBVOC,TG, NT%AL(JP)%XGREENROOF, TOP, &
                                    GRM%O, GRM%S, GRM%K, GRM%P, GRM%NPE%AL(JP), GRM%DTV, GRM%GB, &
                                    HPROGRAM,HINIT,(JP==1),KI,IVERSION,PCO2,PRHOA)
    ENDIF
    CALL END_IO_SURF_n(HPROGRAM)
    !
  ELSE
    NT%AL(JP)%XSVF_TS(:) = XUNDEF 
    NT%AL(JP)%XSVF_TR(:) = XUNDEF 
    NT%AL(JP)%XSVF_ST(:) = XUNDEF 
    NT%AL(JP)%XSVF_RT(:) = XUNDEF 
    NT%AL(JP)%XSVF_WT(:) = XUNDEF 
    NT%AL(JP)%XSVF_TW(:) = XUNDEF 
    !
    NT%AL(JP)%XTAU_SW(:) = 1.
    NT%AL(JP)%XTAU_SR(:) = 1. 
    NT%AL(JP)%XTAU_WW(:) = 1.
    NT%AL(JP)%XTAU_WR(:) = 1. 
    ZEMIS_HVEG(:) = XUNDEF
    ZALB_HVEG (:) = XUNDEF
    ZTRANS_HVCR(:) = 1.
    !
  ENDIF
  !
  !*      Case of urban hydrology 
  !     -------------------------
  !
  IF (JP==1 .AND. TOP%LURBHYDRO) & 
    CALL READ_PGD_TEB_HYDRO_n(DTCO, U, GCP, HM%THP, HM%DTH, KI, NT%AL(1)%XD_ROAD, & !
                               TOP%NTEB_SOIL, HPROGRAM)
  !
  !-------------------------------------------------------------------------------
  !
  !*       6.     Building Energy Model
  !               ---------------------
  !
  CALL INIT_BEM_n(DTCO, TOP, BOP, DTT, DTB, BDD, TG, NT%AL(JP), NB%AL(JP), GDM, KSW, ILUOUT, HPROGRAM, HINIT)
  !
!-------------------------------------------------------------------------------
END DO ! end of loop on TEB patches
!-------------------------------------------------------------------------------
!
!* Read irrigation parameters for TEB
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
 CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','READ ')     
 CALL READ_PGD_TEB_IRRIG_n(TG, TIR, HPROGRAM)
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
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','READ ')
!
!*       9.     Prognostic fields:
!               -----------------
!
!*              LOOP ON TEB PATCHES
!               -------------------
!
DO JP=1,TOP%NTEB_PATCH
!
!* TEB fields
  CALL READ_TEB_n(NB%AL(JP), BOP, DTCO, U, NT%AL(JP), TOP, TPN, HPROGRAM,JP)
!
  ALLOCATE(NT%AL(JP)%XAC_ROOF    (ILU))
  ALLOCATE(NT%AL(JP)%XAC_ROAD    (ILU))
  ALLOCATE(NT%AL(JP)%XAC_WALL    (ILU))
  ALLOCATE(NT%AL(JP)%XAC_TOP     (ILU))
  ALLOCATE(NT%AL(JP)%XAC_ROOF_WAT(ILU))
  ALLOCATE(NT%AL(JP)%XAC_ROAD_WAT(ILU))
  ALLOCATE(NT%AL(JP)%XQSAT_ROOF  (ILU))
  ALLOCATE(NT%AL(JP)%XQSAT_ROAD  (ILU))
  ALLOCATE(NT%AL(JP)%XDELT_ROOF  (ILU))
  ALLOCATE(NT%AL(JP)%XDELT_ROAD  (ILU))
!
!* Case of urban green areas
  IF (TOP%LGARDEN) THEN
    !     
    CALL INIT_TEB_GARDEN_n(DTCO, UG, U, TD%MTO, TOP, GDM%O, GDM%DTV, GDM%K, GDM%P, &
                           GDM%NPE%AL(JP), GDM%PHV, GDM%NPEHV%AL(JP), &
                           GDM%VD%ND%AL(JP), GDM%VD%NDC%AL(JP), GDM%VD%NDE%AL(JP),&
                           GDM%VD%NDEC%AL(JP), GDM%VD%NDM%AL(JP), & 
                           HPROGRAM, HINIT, KI, KSW, PSW_BANDS, JP)
    ! Case of urban green roofs
    IF (TOP%LGREENROOF) CALL INIT_TEB_GREENROOF_n(DTCO, U, TD%MTO, TOP, GRM%O, GRM%DTV, GRM%K, GRM%P, &
                           GRM%NPE%AL(JP), GRM%VD%ND%AL(JP), GRM%VD%NDC%AL(JP),GRM%VD%NDE%AL(JP), &
                           GRM%VD%NDEC%AL(JP), GRM%VD%NDM%AL(JP), &
                           HPROGRAM, HINIT, KI, KSV, PSW_BANDS, JP)
    !
    ! Case of urban hydrology
    IF (TOP%LURBHYDRO) CALL INIT_TEB_HYDRO_n(DTCO,TOP,U,HM%NTH%AL(JP),JP,TOP%NTEB_SOIL,HPROGRAM,HINIT,KI)
  ENDIF
!
!* Urban tree temperature
!High vegetation temperature is prescribed to air temperature inside the canyon
!
    IF (TOP%LGARDEN .AND. TOP%CURBTREE /= 'NONE') THEN
       ZTS_HVEG(:) = NT%AL(JP)%XT_CANYON(:)
    ELSE
       ZTS_HVEG(:) = XUNDEF
    ENDIF
!
!-------------------------------------------------------------------------------
!
!*      10.     Infra-red Radiative fields:
!               --------------------------
!
!* snow long-wave properties (not initialized in read_gr_snow)
!
  CALL INIT_SNOW_LW(XEMISSN,NT%AL(JP)%TSNOW_ROOF)
  CALL INIT_SNOW_LW(XEMISSN,NT%AL(JP)%TSNOW_ROAD)
!
  IF (TOP%LGARDEN) THEN
    ZDIR_SW=0. ! night as first guess for albedo computation
    ZSCA_SW=0. !
    CALL TEB_VEG_PROPERTIES(NT%AL(JP)%XGARDEN, GDM%O, GDM%NPE%AL(JP), &
                           ZDIR_SW, ZSCA_SW, PSW_BANDS, KSW,      &
                           ZTS_GARDEN, ZEMIS_GARDEN, ZALB_GARDEN )      
  ELSE
    ZALB_GARDEN = XUNDEF
    ZEMIS_GARDEN= XUNDEF
    ZTS_GARDEN  = XUNDEF
  END IF
  !
  IF (TOP%LGREENROOF) THEN
    ZDIR_SW=0. ! night as first guess for albedo computation
    ZSCA_SW=0. !
    CALL TEB_VEG_PROPERTIES(NT%AL(JP)%XGREENROOF, GRM%O, GRM%NPE%AL(JP),         & 
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
  CALL AVERAGED_TSRAD_TEB(NT%AL(JP), NB%AL(JP), ZEMIS_GARDEN, ZTS_GARDEN,  &
                          ZEMIS_GREENROOF, ZTS_GREENROOF,ZEMIS_HVEG, ZTS_HVEG, PEMIS, PTSRAD  )
!
!
!*       9.     Visible and near-infra-red Radiative fields:
!               -------------------------------------------
!
  ALLOCATE(ZDIR_ALB(ILU))
  ALLOCATE(ZSCA_ALB(ILU))
!
  ALLOCATE(ZSHAD_BEHAV_ADAPTI(ILU,BOP%NBEMCOMP))
  ALLOCATE(ZSHAD_BEHAV_ANYWAY(ILU,BOP%NBEMCOMP))
  ALLOCATE(ZISNIGHT(ILU,BOP%NBEMCOMP))
  ALLOCATE(ZBLDOCC(ILU,BOP%NBEMCOMP))
!
! Calculate shading behaviours
!
  CALL SUNPOS(TOP%TTIME%TDATE%YEAR, TOP%TTIME%TDATE%MONTH, TOP%TTIME%TDATE%DAY, PTIME, TG%XLON, TG%XLAT, ZTSUN, ZZENITH, ZAZIM)
!
  IF (TOP%CBEM=='BEM') THEN
    DO JCOMP=1,BOP%NBEMCOMP
       CALL BLD_OCC_CALENDAR(HPROGRAM,TOP%TTIME%TDATE%YEAR,TOP%TTIME%TDATE%MONTH,TOP%TTIME%TDATE%DAY,        &
                             ZTSUN,NB%AL(JP)%XDAYWBEG_SCHED(:,:,JCOMP),NB%AL(JP)%XHOURBEG_SCHED(:,:,JCOMP),                     &
                             NB%AL(JP)%XPROBOCC(:,:,JCOMP),NB%AL(JP)%XBEG_HOLIDAY(:,:,JCOMP),NB%AL(JP)%XEND_HOLIDAY(:,:,JCOMP), &
                             NB%AL(JP)%XMOD_HOLIDAY(:,JCOMP), ZBLDOCC(:,JCOMP), ZISNIGHT(:,JCOMP)                          )
    ENDDO
!
    IF ((MINVAL(ZBLDOCC).LT.0.0).OR.(MAXVAL(ZBLDOCC).GT.1.0)) THEN
       CALL ABOR1_SFX("TEB_GARDEN: Wrong probability of building occupation")
    ENDIF
    !
    IF ((MINVAL(ZISNIGHT).LT.0.0).OR.(MAXVAL(ZISNIGHT).GT.1.0)) THEN
       CALL ABOR1_SFX("TEB_GARDEN: Wrong day/night switch")
    ENDIF
    !   
    ZSHAD_BEHAV_ANYWAY(:,:) = XUNDEF
    ZSHAD_BEHAV_ADAPTI(:,:) = XUNDEF
    !
    DO JJ=1,SIZE(ZISNIGHT,1)
       DO JCOMP=1,SIZE(ZISNIGHT,2)
          !
          ZSHAD_BEHAV_ANYWAY(JJ,JCOMP) = ZISNIGHT(JJ,JCOMP) * NB%AL(JP)%XFSNIG(JJ,JCOMP) + &
                 (1.0-ZISNIGHT(JJ,JCOMP))*(1.0-ZBLDOCC(JJ,JCOMP))* NB%AL(JP)%XSHADVACSW(JJ,JCOMP) * NB%AL(JP)%XFSVAC(JJ,JCOMP)
          !
          ZSHAD_BEHAV_ADAPTI(JJ,JCOMP) = (1.0-ZISNIGHT(JJ,JCOMP))*ZBLDOCC(JJ,JCOMP)*NB%AL(JP)%XFSSUM(JJ,JCOMP)
          !
       ENDDO
    ENDDO
!
! Plausibility checks
!
    IF ((MINVAL(ZSHAD_BEHAV_ANYWAY).LT.-XSURF_EPSILON).OR.(MAXVAL(ZSHAD_BEHAV_ANYWAY).GT.(1.0+XSURF_EPSILON))) THEN
       CALL ABOR1_SFX("Unrealistic shading behaviour")
    ENDIF
    !
    IF ((MINVAL(ZSHAD_BEHAV_ADAPTI).LT.-XSURF_EPSILON).OR.(MAXVAL(ZSHAD_BEHAV_ADAPTI).GT.(1.0+XSURF_EPSILON))) THEN
        CALL ABOR1_SFX("Unrealistic shading behaviour")
    ENDIF
    !
    IF ((MINVAL(ZSHAD_BEHAV_ANYWAY+ZSHAD_BEHAV_ADAPTI).LT.-XSURF_EPSILON).OR. &
         (MAXVAL(ZSHAD_BEHAV_ANYWAY+ZSHAD_BEHAV_ADAPTI).GT.(1.0+XSURF_EPSILON))) THEN
        CALL ABOR1_SFX("Unrealistic shading behaviour")
    ENDIF
    !
  ELSE
    ZSHAD_BEHAV_ANYWAY(:,:) = 0.
    ZSHAD_BEHAV_ADAPTI(:,:) = 0.
  END IF
!
  CALL AVERAGED_ALBEDO_TEB(TOP, BOP, ILU, KSW, NT%AL(JP),TPN,NB%AL(JP),GDM%P,PZENITH,PAZIM, &
                           ZTRANS_HVCR, ZSHAD_BEHAV_ANYWAY, ZSHAD_BEHAV_ADAPTI,   &
                           ZALB_GARDEN, ZALB_GREENROOF, ZALB_HVEG, ZDIR_ALB, ZSCA_ALB)  

  ISWB=SIZE(PSW_BANDS)
  DO JSWB=1,ISWB
    PDIR_ALB(:,JSWB) = ZDIR_ALB(:)
    PSCA_ALB(:,JSWB) = ZSCA_ALB(:)
  END DO
  !
  DEALLOCATE(ZDIR_ALB)
  DEALLOCATE(ZSCA_ALB)
  DEALLOCATE(ZSHAD_BEHAV_ADAPTI)
  DEALLOCATE(ZSHAD_BEHAV_ANYWAY)
  DEALLOCATE(ZISNIGHT)
  DEALLOCATE(ZBLDOCC)
!
!-------------------------------------------------------------------------------
!
!*      10.     Chemistry /dust
!               ---------------
!
  CALL INIT_CHEMICAL_n(ILUOUT, KSV, HSV, CHT%SVT,  CHT%SLTT,  CHT%DSTT,     &
                       CHT%CCH_NAMES, CHT%CAER_NAMES,     &
                       HDSTNAMES=CHT%CDSTNAMES, HSLTNAMES=CHT%CSLTNAMES        )
!
!* Initialization of dry deposition scheme (chemistry)
!
  IF (CHT%SVT%NBEQ>0 .AND. CHT%CCH_DRY_DEP=='WES89') THEN
    ALLOCATE(CHT%XDEP(ILU,CHT%SVT%NBEQ))
  ELSE
    ALLOCATE(CHT%XDEP(0,0))
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
 CALL READ_SBL_n(DTCO, U, SB, TOP%LCANOPY, HPROGRAM, "TOWN  ")
!
!-------------------------------------------------------------------------------
!
!*       8.     atmospheric turbulence parameters
!               ---------------------------------
!
TM_AT=AT
!
!
!-------------------------------------------------------------------------------
!*      11.     Diagnostics:
!               -----------
!
 CALL DIAG_TEB_INIT_n(TD%O, TD%D, TD%DU, BOP, HPROGRAM,ILU,ISWB)
!
DO JP=1,TOP%NTEB_PATCH
  CALL DIAG_MISC_TEB_INIT_n(TD%NDMTC%AL(JP), TD%NDMT%AL(JP), TD%MTO, TOP, BOP, ILU, ISWB, &
                               HM%NTH%AL(JP), GDM%NPE%AL(JP), NT%AL(JP))
END DO
!
!-------------------------------------------------------------------------------
!
#ifdef MNH_PARALLEL
 CALL MPPDB_CHECK_SURFEX3D(TOP%XCOVER,"INIT_TEB_n end:XCOVER",PRECISION,ILUOUT, 'TOWN  ',SIZE(TOP%XCOVER,2))
#endif
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('INIT_TEB_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE INIT_TEB_n
