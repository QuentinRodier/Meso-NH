!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_SURF_ATM_n(HPROGRAM,HINIT, OLAND_USE,                   &
                             KI,KSV,KSW,                                &
                             HSV,PCO2,PRHOA,                            &
                             PZENITH,PAZIM,PSW_BANDS,PDIR_ALB,PSCA_ALB, &
                             PEMIS,PTSRAD,                              &
                             KYEAR, KMONTH,KDAY, PTIME,                 &
                             HATMFILE,HATMFILETYPE,                     &
                             HTEST                                      )  
!#############################################################
!
!!****  *INIT_SURF_ATM_n* - routine to initialize GROUND
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
!      (P.Tulet )             01/11/03  initialisation of the surface chemistry!
!!     (D.Gazen)    01/12/03  change emissions handling for surf. externalization
!!     (P.LeMoigne) 18/07/05  get 1d mask only if associated tile exists    
!!     (B.Decharme)  03/2009  New keys read for arrange cover by user
!!     (B.Decharme)  04/2009  Read precipitation forcing from the restart file for ARPEGE/ALADIN run
!!     (A. Lemonsu)    2009   New key read for urban green areas
!!     (B.Decharme)  07/2011  Read pgd+prep
!!     (S. Queguiner)  2011   Modif chemistry (2.4)
!!     (B. Decharme)   2013   Read grid only once in AROME case
!!     (G. Tanguy)     2013   Add IF(ALLOCATED(NMASK_FULL))  before deallocate
!!     (J.Durand)      2014   add activation of chemical deposition if LCH_EMIS=F
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
USE MODD_SURF_CONF,      ONLY : CPROGNAME
USE MODD_SURF_ATM_n,     ONLY : CSEA,      CWATER,      CTOWN,      CNATURE,      &
                                XSEA,      XWATER,      XTOWN,      XNATURE,      &
                                NSIZE_SEA, NSIZE_WATER, NSIZE_TOWN, NSIZE_NATURE, &
                                NR_SEA,    NR_WATER,    NR_TOWN,    NR_NATURE,    &
                                XCOVER, XOUT_TSTEP, TTIME,                        &
                                NDIM_FULL, NSIZE_FULL,                            &
                                NDIM_NATURE, NDIM_SEA, NDIM_WATER, NDIM_TOWN,     &
                                LECOCLIMAP, LWATER_TO_NATURE, LTOWN_TO_ROCK,      &
                                LGARDEN 
USE MODD_SURF_ATM_SSO_n, ONLY : CROUGH, XAOSIP, XAOSIM, XAOSJP, XAOSJM, &
                                XHO2IP, XHO2IM, XHO2JP, XHO2JM,         &
                                XZ0EFFIP, XZ0EFFIM, XZ0EFFJP, XZ0EFFJM, &
                                XZ0REL, XZ0EFFJPDIR, XFRACZ0, XCOEFBE
USE MODD_CH_SURF_n,      ONLY : CCH_NAMES, LCH_EMIS, LRW_CH_EMIS, &
                                LCH_SURF_EMIS, CCHEM_SURF_FILE, CAER_NAMES,&
                                CCH_EMIS
USE MODD_SV_n,           ONLY : NBEQ, CSV, NSV_CHSBEG, NSV_CHSEND, &
                                NSV_DSTBEG, NSV_DSTEND, NDSTEQ,    &
                                NSV_SLTBEG, NSV_SLTEND, NSLTEQ,    &
                                NAEREQ, NSV_AERBEG, NSV_AEREND 
USE MODD_DST_SURF,       ONLY : NDSTMDE, NDST_MDEBEG, LVARSIG_DST, LRGFIX_DST 
USE MODD_SLT_SURF,       ONLY : NSLTMDE, NSLT_MDEBEG, LVARSIG_SLT, LRGFIX_SLT                                
USE MODD_SURF_ATM_GRID_n,ONLY : XLAT, XLON, XMESH_SIZE, CGRID, XGRID_PAR, &
                                NGRID_PAR, XGRID_FULL_PAR

USE MODD_DIAG_SURF_ATM_n,ONLY : N2M, L2M_MIN_ZS, LSURF_BUDGET,     &
                                LRAD_BUDGET, LCOEF, XDIAG_TSTEP,   &
                                LFRAC, LSURF_VARS, LDIAG_GRID,     &
                                LSURF_BUDGETC, LRESET_BUDGETC,     &
                                LPROVAR_TO_DIAG, LSELECT, CSELECT  
USE MODD_DATA_COVER_PAR, ONLY : NTILESFC
USE MODD_DATA_COVER_n,   ONLY : NYEAR
USE MODD_DATA_COVER,     ONLY : LCLIM_LAI, XDATA_LAI_ALL_YEARS, XDATA_LAI, &
                                NECO2_START_YEAR, NECO2_END_YEAR  
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_CHS_AEROSOL,    ONLY : LVARSIGI, LVARSIGJ
USE MODD_WRITE_SURF_ATM, ONLY : LNOWRITE_CANOPY, LNOWRITE_TEXFILE  
!
USE MODD_SURFEX_MPI, ONLY : XTIME_INIT_SEA, XTIME_INIT_WATER, XTIME_INIT_NATURE, XTIME_INIT_TOWN, &
                            NRANK, NPIO, NWG_LAYER_TOT, NSIZE
USE MODD_SURFEX_OMP, ONLY : NINDX2, NWORK, XWORK, XWORK2, XWORK3, NWORK_FULL, XWORK_FULL, XWORK2_FULL
!
USE MODD_MASK, ONLY: NMASK_FULL
!
USE MODI_INIT_IO_SURF_n
USE MODI_DEFAULT_SSO
USE MODI_DEFAULT_CH_SURF_ATM
USE MODI_DEFAULT_DIAG_SURF_ATM
USE MODI_READ_DEFAULT_SURF_ATM_n
USE MODI_READ_SURF_ATM_CONF_n
USE MODI_READ_SURF_ATM_DATE
USE MODI_READ_NAM_PREP_SURF_n
USE MODI_READ_SURF
USE MODI_GET_SIZE_FULL_n
USE MODI_READ_COVER_n
USE MODI_READ_SSO_n
USE MODI_SUBSCALE_Z0EFF
USE MODI_READ_SSO_CANOPY_n
USE MODI_READ_DUMMY_n
USE MODI_READ_GRID
USE MODI_READ_GRIDTYPE
USE MODI_END_IO_SURF_n
USE MODI_PREP_CTRL_SURF_ATM
USE MODI_AVERAGE_RAD
USE MODI_WRITE_COVER_TEX_START
USE MODI_WRITE_COVER_TEX_END
USE MODI_INIT_CHEMICAL_n
USE MODI_CH_INIT_DEPCONST
USE MODI_CH_INIT_EMISSION_n
USE MODI_CH_INIT_SNAP_n
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_READ_PRECIP_n
USE MODI_ABOR1_SFX
USE MODI_ALLOC_DIAG_SURF_ATM_n
USE MODI_GET_1D_MASK
USE MODI_INI_DATA_COVER
USE MODI_INIT_INLAND_WATER_n
USE MODI_INIT_NATURE_n
USE MODI_INIT_SEA_n
USE MODI_INIT_TOWN_n
USE MODI_READ_ARRANGE_COVER
USE MODI_READ_COVER_GARDEN
USE MODI_READ_ECO2_IRRIG
USE MODI_READ_LCLIM_LAI
USE MODI_READ_LECOCLIMAP
USE MODI_SURF_VERSION
USE MODI_WRITE_COVER_TEX_COVER
USE MODI_GET_LUOUT
USE MODI_SET_SURFEX_FILEIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifndef NOMPI
INCLUDE 'mpif.h'
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),                 INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                 INTENT(IN)  :: HINIT     ! choice of fields to initialize
LOGICAL,                          INTENT(IN)  :: OLAND_USE ! 
INTEGER,                          INTENT(IN)  :: KI        ! number of points
INTEGER,                          INTENT(IN)  :: KSV       ! number of scalars
INTEGER,                          INTENT(IN)  :: KSW       ! number of short-wave spectral bands
 CHARACTER(LEN=6), DIMENSION(KSV), INTENT(IN)  :: HSV       ! name of all scalar variables
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
!*       0.2   Declarations of local variables
!              -------------------------------
!
CHARACTER(LEN=3)  :: YREAD
!
INTEGER           :: ISWB     ! number of shortwave bands
INTEGER           :: JTILE    ! loop counter on tiles
INTEGER           :: IRESP    ! error return code
INTEGER           :: ILUOUT   ! unit of output listing file
INTEGER           :: ICH      ! unit of input chemical file
INTEGER           :: IVERSION, IBUGFIX       ! surface version
!
REAL, DIMENSION(:,:), ALLOCATABLE                       :: ZFRAC_TILE     ! fraction of each surface type
REAL, DIMENSION(KI,KSW,NTILESFC) :: ZDIR_ALB_TILE  ! direct albedo
REAL, DIMENSION(KI,KSW,NTILESFC) :: ZSCA_ALB_TILE  ! diffuse albedo
REAL, DIMENSION(KI,NTILESFC)                 :: ZEMIS_TILE     ! emissivity
REAL, DIMENSION(KI,NTILESFC)                 :: ZTSRAD_TILE    ! radiative temperature
!
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_ZENITH   ! zenithal angle
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_AZIM     ! azimuthal angle
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_CO2      ! air CO2 concentration
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_RHOA     ! air density
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZP_DIR_ALB  ! direct albedo
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZP_SCA_ALB  ! diffuse albedo
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_EMIS     ! emissivity
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_TSRAD    ! radiative temperature
!
REAL :: XTIME0
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_SURF_ATM_N',0,ZHOOK_HANDLE)
!
!
CPROGNAME=HPROGRAM
!
IF (HTEST/='OK') THEN
   CALL ABOR1_SFX('INIT_SURF_ATMN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
!-------------------------------------------------------------------------------
!
 CALL SURF_VERSION
!
!-------------------------------------------------------------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (LNAM_READ) THEN
 !
 !*       0.     Defaults
 !               --------
 !
 !        0.1. Hard defaults
 !      
 CALL DEFAULT_SSO(CROUGH,XFRACZ0,XCOEFBE)
 CALL DEFAULT_CH_SURF_ATM(CCHEM_SURF_FILE,LCH_SURF_EMIS)
 CALL DEFAULT_DIAG_SURF_ATM(N2M,LSURF_BUDGET,L2M_MIN_ZS,LRAD_BUDGET, &
                            LCOEF,LSURF_VARS,LSURF_BUDGETC,          &
                            LRESET_BUDGETC,LSELECT, LPROVAR_TO_DIAG, &
                            LDIAG_GRID, LFRAC, XDIAG_TSTEP )                       
 !
ENDIF
!
!        0.2. Defaults from file header
!    
 CALL READ_DEFAULT_SURF_ATM_n(HPROGRAM)
!
!*       1.     Reading of configuration
!               ------------------------
!
!        1.1. general options (diagnostics, etc...)
!
 CALL READ_SURF_ATM_CONF_n(HPROGRAM)
!
 CALL WRITE_COVER_TEX_START(HPROGRAM)
!
!        1.2. Date
!
SELECT CASE (HINIT)
  CASE ('PGD')
    TTIME%TDATE%YEAR = NUNDEF
    TTIME%TDATE%MONTH= NUNDEF
    TTIME%TDATE%DAY  = NUNDEF
    TTIME%TIME       = XUNDEF
        
  CASE ('PRE')
    ! check that diagnostics are off if hinit=='pre'
    CALL PREP_CTRL_SURF_ATM(N2M,LSURF_BUDGET,L2M_MIN_ZS,LRAD_BUDGET,LCOEF,LSURF_VARS,    &
                              LSURF_BUDGETC,LRESET_BUDGETC,LNOWRITE_TEXFILE,LSELECT,ILUOUT,&
                              LPROVAR_TO_DIAG)  
    ! preparation of fields  (date not present in PGD file)
    IF (LNAM_READ) CALL READ_NAM_PREP_SURF_n(HPROGRAM)
    CALL READ_SURF_ATM_DATE(HPROGRAM,HINIT,ILUOUT,HATMFILE,HATMFILETYPE,KYEAR,KMONTH,KDAY,PTIME,TTIME)

  CASE DEFAULT
    CALL INIT_IO_SURF_n(HPROGRAM,'FULL  ','SURF  ','READ ')
    CALL READ_SURF(HPROGRAM,'DTCUR',TTIME,IRESP)
    CALL END_IO_SURF_n(HPROGRAM)

END SELECT
!
!-----------------------------------------------------------------------------------------------------
! READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
!        1.3. Schemes used
!
!         Initialisation for IO
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
 CALL INIT_IO_SURF_n(HPROGRAM,'FULL  ','SURF  ','READ ')
!
 CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
!
IF (IVERSION>7 .OR. IVERSION==7 .AND.IBUGFIX>=2) THEN
  CALL READ_SURF(HPROGRAM,'STORAGETYPE',YREAD,IRESP)
ENDIF
!         reading
!
 CALL READ_SURF(HPROGRAM,'SEA   ',CSEA   ,IRESP)
 CALL READ_SURF(HPROGRAM,'WATER ',CWATER ,IRESP)
 CALL READ_SURF(HPROGRAM,'NATURE',CNATURE,IRESP)
 CALL READ_SURF(HPROGRAM,'TOWN  ',CTOWN  ,IRESP)
!
 CALL READ_SURF(HPROGRAM,'DIM_FULL  ',NDIM_FULL,  IRESP)
IF (HINIT=='PRE') THEN
  !Initialize full dimension
  NINDX2 = NDIM_FULL
  NSIZE = NDIM_FULL
  CALL END_IO_SURF_n(HPROGRAM)
  !Initialize full mask with good dimension
  IF ( ALLOCATED(NMASK_FULL) ) DEALLOCATE(NMASK_FULL)
  CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ')
  CALL INIT_IO_SURF_n(HPROGRAM,'FULL  ','SURF  ','READ ')
  ALLOCATE(NWORK(NDIM_FULL))
  ALLOCATE(XWORK(NDIM_FULL))
  ALLOCATE(XWORK2(NDIM_FULL,2))
  ALLOCATE(XWORK3(NDIM_FULL,10,10))
  IF (NRANK==NPIO) THEN
    ALLOCATE(NWORK_FULL(NDIM_FULL))
    ALLOCATE(XWORK_FULL(NDIM_FULL))
    ALLOCATE(XWORK2_FULL(NDIM_FULL,10))
  ELSE
    ALLOCATE(NWORK_FULL(0))
    ALLOCATE(XWORK_FULL(0))
    ALLOCATE(XWORK2_FULL(0,0))
  ENDIF
ENDIF  
 CALL READ_SURF(HPROGRAM,'DIM_SEA   ',NDIM_SEA,   IRESP)
 CALL READ_SURF(HPROGRAM,'DIM_NATURE',NDIM_NATURE,IRESP)
 CALL READ_SURF(HPROGRAM,'DIM_WATER ',NDIM_WATER, IRESP)
 CALL READ_SURF(HPROGRAM,'DIM_TOWN  ',NDIM_TOWN,  IRESP)
 CALL READ_LECOCLIMAP(HPROGRAM,LECOCLIMAP)
 CALL READ_ARRANGE_COVER(HPROGRAM,LWATER_TO_NATURE,LTOWN_TO_ROCK)
 CALL READ_COVER_GARDEN(HPROGRAM,LGARDEN)
!
!* reads if climatological LAI is used or not for ecoclimap2. If not, looks for year to be used.
 CALL READ_LCLIM_LAI(HPROGRAM,LCLIM_LAI)
IF (.NOT. LCLIM_LAI .AND. TTIME%TDATE%YEAR >= NECO2_START_YEAR &
                     .AND. TTIME%TDATE%YEAR <= NECO2_END_YEAR   ) NYEAR=TTIME%TDATE%YEAR
 CALL INI_DATA_COVER
 CALL READ_ECO2_IRRIG(HPROGRAM)
!
 CALL WRITE_COVER_TEX_COVER
!
!*       2.     Cover fields and grid:
!               ---------------------
!
!        2.0. Get number of points on this proc
!
 CALL GET_SIZE_FULL_n(HPROGRAM,NDIM_FULL,NSIZE_FULL)
!
!        2.1. Read cover
!
 CALL READ_COVER_n(HPROGRAM)
!
!        2.2. Read grid
!
ALLOCATE(XLAT       (NSIZE_FULL))
ALLOCATE(XLON       (NSIZE_FULL))
ALLOCATE(XMESH_SIZE (NSIZE_FULL))
ALLOCATE(XZ0EFFJPDIR(NSIZE_FULL))
 CALL READ_GRID(HPROGRAM,CGRID,XGRID_PAR,XLAT,XLON,XMESH_SIZE,IRESP,XZ0EFFJPDIR)
NGRID_PAR=SIZE(XGRID_PAR)
!
IF (HPROGRAM/='AROME '.AND.NRANK==NPIO) THEN
  !
  IF (.NOT.ASSOCIATED(XGRID_FULL_PAR)) THEN
    CALL READ_GRIDTYPE(HPROGRAM,CGRID,NGRID_PAR,NSIZE_FULL,.FALSE.,HDIR='H')
    ALLOCATE(XGRID_FULL_PAR(NGRID_PAR))
    CALL READ_GRIDTYPE(HPROGRAM,CGRID,NGRID_PAR,NSIZE_FULL,.TRUE.,&
                       XGRID_FULL_PAR,IRESP,HDIR='H')
  ENDIF
  !
ENDIF
!
!*       2.4     Allocation of chemical species name, chemical index of HSV array 
!
 CALL INIT_CHEMICAL_n(ILUOUT, KSV, HSV, NBEQ, CSV, NAEREQ,            &
                     NSV_CHSBEG, NSV_CHSEND, NSV_AERBEG, NSV_AEREND, &
                     CCH_NAMES, CAER_NAMES, NDSTEQ, NSV_DSTBEG,      &
                     NSV_DSTEND, NSLTEQ, NSV_SLTBEG, NSV_SLTEND      )
!
!        2.4 Initialize Chemical Emissions
!
 CALL READ_SURF(HPROGRAM,'CH_EMIS',LCH_EMIS,IRESP)
!
IF (LCH_EMIS) THEN
  !
  IF ( IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<3 ) THEN
    CCH_EMIS='AGGR'
  ELSE
    CALL READ_SURF(HPROGRAM,'CH_EMIS_OPT',CCH_EMIS,IRESP)
  END IF
  !
  IF (CCH_EMIS=='AGGR') LRW_CH_EMIS = .TRUE.
  !
  IF (NBEQ > 0) THEN
    !
    CALL OPEN_NAMELIST(HPROGRAM,ICH,HFILE=CCHEM_SURF_FILE)
    !
    IF (LCH_SURF_EMIS) THEN
      IF (CCH_EMIS=='AGGR') THEN
        CALL CH_INIT_EMISSION_n(HPROGRAM,NSIZE_FULL,ICH,PRHOA) 
      ELSE
        CALL CH_INIT_SNAP_n(HPROGRAM,NSIZE_FULL,HINIT,ICH,PRHOA)
      END IF
    ENDIF
    CALL CLOSE_NAMELIST(HPROGRAM,ICH)
  ENDIF
  !
END IF
    !
    !*       2.5 Initialization of dry deposition scheme (chemistry)
    !    
!
IF (NBEQ .GT. 0) THEN
 CALL OPEN_NAMELIST(HPROGRAM,ICH,HFILE=CCHEM_SURF_FILE)

 IF (HINIT=='ALL') CALL CH_INIT_DEPCONST(ICH,ILUOUT,CSV(NSV_CHSBEG:NSV_CHSEND))
!
 CALL CLOSE_NAMELIST(HPROGRAM,ICH)
!
END IF
!
!*       2.5 Subgrid orography
!
 CALL READ_SSO_n(HPROGRAM)
!
!*       2.6 Orographic roughness length
!
ALLOCATE(XZ0EFFIP(NSIZE_FULL))
ALLOCATE(XZ0EFFIM(NSIZE_FULL))
ALLOCATE(XZ0EFFJP(NSIZE_FULL))
ALLOCATE(XZ0EFFJM(NSIZE_FULL))
ALLOCATE(XZ0REL  (NSIZE_FULL))
!
 CALL SUBSCALE_Z0EFF(XAOSIP,XAOSIM,XAOSJP,XAOSJM,         &
                    XHO2IP,XHO2IM,XHO2JP,XHO2JM,0.,      &
                    XZ0EFFIP,XZ0EFFIM,XZ0EFFJP,XZ0EFFJM, &
                    XZ0REL                               )
!
!*       2.7 Dummy fields
!
 CALL READ_DUMMY_n(HPROGRAM)
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
!
!-----------------------------------------------------------------------------------------------------
! END READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
!
!         Initialisation for IO
!
 CALL INIT_IO_SURF_n(HPROGRAM,'FULL  ','SURF  ','READ ')
!
!*       2.8 Allocations and Initialization of diagnostics
!
IF (HINIT=='ALL') CALL ALLOC_DIAG_SURF_ATM_n(HPROGRAM,KSW)
!
!
!*       Canopy fields if Beljaars et al 2004 parameterization is used
!
IF (CROUGH=='BE04') CALL READ_SSO_CANOPY_n(HPROGRAM,HINIT)
!
!*       Precip fields (for ARPEGE/ALADIN run)
!
 CALL READ_PRECIP_n(HPROGRAM,HINIT)
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
!
!-----------------------------------------------------------------------------------------------------
!
!*       4.     Initialization of masks for each surface
!               ----------------------------------------
!
!* number of geographical points
NSIZE_NATURE    = COUNT(XNATURE(:) > 0.0)
NSIZE_TOWN      = COUNT(XTOWN(:)   > 0.0)
NSIZE_WATER     = COUNT(XWATER(:)  > 0.0)
NSIZE_SEA       = COUNT(XSEA(:)    > 0.0)
!
ALLOCATE(NR_NATURE (NSIZE_NATURE))
ALLOCATE(NR_TOWN   (NSIZE_TOWN  ))
ALLOCATE(NR_WATER  (NSIZE_WATER ))
ALLOCATE(NR_SEA    (NSIZE_SEA   ))
!
IF (NSIZE_SEA   >0)CALL GET_1D_MASK( NSIZE_SEA,    NSIZE_FULL, XSEA   , NR_SEA   )
IF (NSIZE_WATER >0)CALL GET_1D_MASK( NSIZE_WATER,  NSIZE_FULL, XWATER , NR_WATER )
IF (NSIZE_TOWN  >0)CALL GET_1D_MASK( NSIZE_TOWN,   NSIZE_FULL, XTOWN  , NR_TOWN  )
IF (NSIZE_NATURE>0)CALL GET_1D_MASK( NSIZE_NATURE, NSIZE_FULL, XNATURE, NR_NATURE)
!
!* number of shortwave spectral bands
ISWB=SIZE(PSW_BANDS)
!
!* tile number
ALLOCATE(ZFRAC_TILE(NSIZE_FULL,NTILESFC))
JTILE = 0
!
!
!*       5.     Default values
!               --------------
!
ZDIR_ALB_TILE = XUNDEF
ZSCA_ALB_TILE = XUNDEF
ZEMIS_TILE    = XUNDEF
ZTSRAD_TILE   = XUNDEF
!
#ifndef NOMPI
XTIME0 = MPI_WTIME()
#endif
!
!*       6.     Initialization of sea
!               ---------------------
!
JTILE               = JTILE + 1
ZFRAC_TILE(:,JTILE) = XSEA(:)
!
! pack variables which are arguments to this routine
 CALL PACK_SURF_INIT_ARG(NSIZE_SEA,NR_SEA)
!
! initialization
IF (NDIM_SEA>0) &
  CALL INIT_SEA_n(HPROGRAM,HINIT,NSIZE_SEA,KSV,KSW,                  &
                  HSV,ZP_CO2,ZP_RHOA,                                &
                  ZP_ZENITH,ZP_AZIM,PSW_BANDS,ZP_DIR_ALB,ZP_SCA_ALB, &
                  ZP_EMIS,ZP_TSRAD,                                  &
                  KYEAR,KMONTH,KDAY,PTIME, HATMFILE,HATMFILETYPE,    &
                  'OK'                                               )  
!
!
 CALL UNPACK_SURF_INIT_ARG(JTILE,NSIZE_SEA,NR_SEA)  
!
#ifndef NOMPI
XTIME_INIT_SEA = XTIME_INIT_SEA + (MPI_WTIME() - XTIME0)*100./MAX(1,NSIZE_SEA)
XTIME0 = MPI_WTIME()
#endif
!
!*       7.     Initialization of lakes
!               -----------------------
!
!
JTILE               = JTILE + 1
ZFRAC_TILE(:,JTILE) = XWATER(:)
!
! pack variables which are arguments to this routine
 CALL PACK_SURF_INIT_ARG(NSIZE_WATER,NR_WATER)
!
! initialization
IF (NDIM_WATER>0) &
  CALL INIT_INLAND_WATER_n(HPROGRAM,HINIT,NSIZE_WATER,KSV,KSW,                &
                           HSV,ZP_CO2,ZP_RHOA,                                &
                           ZP_ZENITH,ZP_AZIM,PSW_BANDS,ZP_DIR_ALB,ZP_SCA_ALB, &
                           ZP_EMIS,ZP_TSRAD,                                  &
                           KYEAR,KMONTH,KDAY,PTIME, HATMFILE,HATMFILETYPE,    &
                           'OK'                                               )
!
 CALL UNPACK_SURF_INIT_ARG(JTILE,NSIZE_WATER,NR_WATER)
!
#ifndef NOMPI
XTIME_INIT_WATER = XTIME_INIT_WATER + (MPI_WTIME() - XTIME0)*100./MAX(1,NSIZE_WATER)
XTIME0 = MPI_WTIME()
#endif
!
!*       8.     Initialization of vegetation scheme
!               -----------------------------------
!
!
JTILE               = JTILE + 1
ZFRAC_TILE(:,JTILE) = XNATURE(:)
!
! pack variables which are arguments to this routine
 CALL PACK_SURF_INIT_ARG(NSIZE_NATURE,NR_NATURE)
!
!$OMP SINGLE
IF ( ALLOCATED(NWG_LAYER_TOT) ) DEALLOCATE(NWG_LAYER_TOT)
ALLOCATE(NWG_LAYER_TOT(NDIM_FULL,1))
!$OMP END SINGLE
!
! initialization
IF (NDIM_NATURE>0) &
  CALL INIT_NATURE_n(HPROGRAM,HINIT,OLAND_USE,NSIZE_NATURE,KSV,KSW,     &
                     HSV,ZP_CO2,ZP_RHOA,                                &
                     ZP_ZENITH,ZP_AZIM,PSW_BANDS,ZP_DIR_ALB,ZP_SCA_ALB, &
                     ZP_EMIS,ZP_TSRAD,                                  &
                     KYEAR,KMONTH,KDAY,PTIME, HATMFILE,HATMFILETYPE,    &
                     'OK'                                               )
!
!
 CALL UNPACK_SURF_INIT_ARG(JTILE,NSIZE_NATURE,NR_NATURE)  
!
#ifndef NOMPI
XTIME_INIT_NATURE = XTIME_INIT_NATURE + (MPI_WTIME() - XTIME0)*100./MAX(1,NSIZE_NATURE)
XTIME0 = MPI_WTIME()
#endif
!
!*       9.     Initialization of urban scheme
!               ------------------------------
!
!
JTILE               = JTILE + 1
ZFRAC_TILE(:,JTILE) = XTOWN(:)
!
! pack variables which are arguments to this routine
 CALL PACK_SURF_INIT_ARG(NSIZE_TOWN,NR_TOWN)
!
! initialization
IF (NDIM_TOWN>0) &
  CALL INIT_TOWN_n(HPROGRAM,HINIT,NSIZE_TOWN,KSV,KSW,                 &
                   HSV,ZP_CO2,ZP_RHOA,                                &
                   ZP_ZENITH,ZP_AZIM,PSW_BANDS,ZP_DIR_ALB,ZP_SCA_ALB, &
                   ZP_EMIS,ZP_TSRAD,                                  &
                   KYEAR,KMONTH,KDAY,PTIME, HATMFILE,HATMFILETYPE,    &
                   'OK'                                               )  
!
!
 CALL UNPACK_SURF_INIT_ARG(JTILE,NSIZE_TOWN,NR_TOWN)  
!
#ifndef NOMPI
XTIME_INIT_TOWN = XTIME_INIT_TOWN + (MPI_WTIME() - XTIME0)*100./MAX(1,NSIZE_TOWN)
#endif
!
!
!*      10.     Output radiative fields
!               -----------------------
!
IF (SIZE(PDIR_ALB)>0)                                                   &
  CALL AVERAGE_RAD(ZFRAC_TILE,                                            &
                   ZDIR_ALB_TILE, ZSCA_ALB_TILE, ZEMIS_TILE, ZTSRAD_TILE, &
                   PDIR_ALB,      PSCA_ALB,      PEMIS,      PTSRAD       )  

DEALLOCATE(ZFRAC_TILE)
!
!
!
!*      11.     check diagnostics flag
!               -----------------------
!
IF(LPROVAR_TO_DIAG)THEN
   IF (NDIM_WATER>0.AND.CWATER=='FLAKE ') THEN
       CALL ABOR1_SFX('For the moment LPROVAR_TO_DIAG can not be activated with CWATER=FLAKE')
   ENDIF
   IF (NDIM_TOWN>0.AND.CTOWN=='TEB') THEN
       CALL ABOR1_SFX('For the moment LPROVAR_TO_DIAG can not be activated with CTOWN=TEB')
   ENDIF
ENDIF 
!
!-------------------------------------------------------------------------------
 CALL WRITE_COVER_TEX_END(HPROGRAM)
!-------------------------------------------------------------------------------
!==============================================================================
IF (LHOOK) CALL DR_HOOK('INIT_SURF_ATM_N',1,ZHOOK_HANDLE)
CONTAINS
!==============================================================================
SUBROUTINE PACK_SURF_INIT_ARG(KSIZE,KMASK)
!
INTEGER, INTENT(IN)               :: KSIZE
INTEGER, INTENT(IN), DIMENSION(:) :: KMASK
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! input arguments:
!
IF (LHOOK) CALL DR_HOOK('PACK_SURF_INIT_ARG',0,ZHOOK_HANDLE)
ALLOCATE(ZP_CO2          (KSIZE))
ALLOCATE(ZP_RHOA         (KSIZE))
ALLOCATE(ZP_ZENITH       (KSIZE))
ALLOCATE(ZP_AZIM         (KSIZE))
!
!
! output arguments:
!
ALLOCATE(ZP_DIR_ALB(KSIZE,ISWB))
ALLOCATE(ZP_SCA_ALB(KSIZE,ISWB))
ALLOCATE(ZP_EMIS   (KSIZE))
ALLOCATE(ZP_TSRAD  (KSIZE))
!
IF (KSIZE>0) THEN
  ZP_CO2    = 6.E-4
  ZP_RHOA   = 1.2
  ZP_ZENITH = 0.
  ZP_AZIM   = 0.
  ZP_DIR_ALB = XUNDEF
  ZP_SCA_ALB = XUNDEF
  ZP_EMIS    = XUNDEF
  ZP_TSRAD   = XUNDEF
END IF
!
DO JJ=1,KSIZE
IF (SIZE(PCO2)>0) &
     ZP_CO2   (JJ)     = PCO2        (KMASK(JJ))  
IF (SIZE(PRHOA)>0) &
     ZP_RHOA  (JJ)     = PRHOA       (KMASK(JJ))  
IF (SIZE(PZENITH)>0) &
     ZP_ZENITH(JJ)     = PZENITH     (KMASK(JJ))  
IF (SIZE(PAZIM  )>0) &
     ZP_AZIM  (JJ)     = PAZIM       (KMASK(JJ))  
ENDDO
IF (LHOOK) CALL DR_HOOK('PACK_SURF_INIT_ARG',1,ZHOOK_HANDLE)
!
END SUBROUTINE PACK_SURF_INIT_ARG
!==============================================================================
SUBROUTINE UNPACK_SURF_INIT_ARG(KTILE,KSIZE,KMASK)
!
INTEGER, INTENT(IN) :: KTILE, KSIZE
!
INTEGER, INTENT(IN), DIMENSION(:) :: KMASK
!
INTEGER :: JJ   ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
IF (LHOOK) CALL DR_HOOK('UNPACK_SURF_INIT_ARG',0,ZHOOK_HANDLE)
DO JJ=1,KSIZE
IF (SIZE(ZTSRAD_TILE)>0) &
     ZTSRAD_TILE  (KMASK(JJ),KTILE)  = ZP_TSRAD     (JJ)  
IF (SIZE(ZDIR_ALB_TILE)>0) &
     ZDIR_ALB_TILE(KMASK(JJ),:,KTILE)= ZP_DIR_ALB   (JJ,:)  
IF (SIZE(ZSCA_ALB_TILE)>0) &
     ZSCA_ALB_TILE(KMASK(JJ),:,KTILE)= ZP_SCA_ALB   (JJ,:)  
IF (SIZE(ZEMIS_TILE)>0) &
     ZEMIS_TILE   (KMASK(JJ),KTILE)  = ZP_EMIS      (JJ)  
ENDDO
!
DEALLOCATE(ZP_CO2    )
DEALLOCATE(ZP_RHOA   )
DEALLOCATE(ZP_ZENITH )
DEALLOCATE(ZP_AZIM   )
DEALLOCATE(ZP_DIR_ALB)
DEALLOCATE(ZP_SCA_ALB)
DEALLOCATE(ZP_EMIS   )
DEALLOCATE(ZP_TSRAD  )
IF (LHOOK) CALL DR_HOOK('UNPACK_SURF_INIT_ARG',1,ZHOOK_HANDLE)
!
END SUBROUTINE UNPACK_SURF_INIT_ARG
!==============================================================================
!
END SUBROUTINE INIT_SURF_ATM_n


