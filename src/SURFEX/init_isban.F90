!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_ISBA_n (DTCO, DGU, UG, U, IM, DTZ, DST, SLT, SV, GCP,&
                            HPROGRAM,HINIT,OLAND_USE,                    &
                             KI,KSV,KSW,                                &
                             HSV,PCO2,PRHOA,                            &
                             PZENITH,PAZIM,PSW_BANDS,PDIR_ALB,PSCA_ALB, &
                             PEMIS,PTSRAD, PTSURF,                      &
                             KYEAR, KMONTH,KDAY, PTIME,                 &
                             HATMFILE,HATMFILETYPE,                     &
                             HTEST                                      )  
!#############################################################
!
!!****  *INIT_ISBA_n* - routine to initialize ISBA
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
!!      Original    01/2004
!!      Modified by P. Le Moigne (11/2004): miscellaneous diagnostics
!!      Modified by P. Le Moigne (06/2006): seeding and irrigation    
!!      Modified by B. Decharme    (2008) : SGH and Flooding scheme
!!      Modified by B. Decharme  (01/2009): optional deep soil temperature as in Arpege
!!      Modified by R. Hamdi     (01/2009): Cp and L
!!      Modified by B. Decharme  (06/2009): read topographic index statistics
!!      Modified by P. Le Moigne (01/2009): Beljaars sso
!!      Modified by B. Decharme  (08/2009): Active Trip coupling variable if Earth System Model
!!      A.L. Gibelin   04/09 : change BSLAI_NITRO initialisation
!!      A.L. Gibelin   04/09 : modifications for CENTURY model 
!!      A.L. Gibelin   06/09 : soil carbon initialisation
!!      B. Decharme    07/11 : read pgd+prep
!!      R. Alkama      05/12 : new carbon spinup
!!      J.Escobar      11/13 : add USE MODI_DEFAULT_CROCUS
!!      B. Decharme  04/2013 new coupling variables
!!      P. Samuelsson  10/14 : MEB
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_DATA_TSZ0_n, ONLY : DATA_TSZ0_t
USE MODD_DST_n, ONLY : DST_t
USE MODD_SLT_n, ONLY : SLT_t
USE MODD_SV_n, ONLY : SV_t
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
!
!
USE MODD_DATA_COVER,     ONLY : XDATA_LAI, XDATA_H_TREE,                          &
                                XDATA_ALBNIR_VEG, XDATA_ALBVIS_VEG,               &
                                XDATA_ALBUV_VEG, XDATA_RSMIN,                     &
                                XDATA_ROOT_EXTINCTION,XDATA_ROOT_LIN,             &
                                XDATA_RGL, XDATA_CV, XDATA_GAMMA, XDATA_GMES,     &
                                XDATA_GC, XDATA_BSLAI, XDATA_SEFOLD, XDATA_LAIMIN,&
                                XDATA_DMAX, XDATA_STRESS, XDATA_F2I,              &
                                XDATA_VEG, XDATA_GREEN, XDATA_Z0, XDATA_Z0_O_Z0H, &
                                XDATA_EMIS_ECO, XDATA_WRMAX_CF,                   &
                                XDATA_CE_NITRO,XDATA_CF_NITRO,XDATA_CNA_NITRO,    &
                                XDATA_SOILRC_SO2, XDATA_SOILRC_O3, XDATA_RE25,    &
                                XDATA_GMES_ST, XDATA_BSLAI_ST, XDATA_SEFOLD_ST,   &
                                XDATA_GC_ST, XDATA_DMAX_ST
!
USE MODD_SURF_ATM,       ONLY : LCPL_GCM
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_AGRI,           ONLY : LAGRIP
!
USE MODE_TARTES, ONLY : INIT_TARTES
USE MODE_SNOWCRO_FLANNER, ONLY : READ_FZ06
!
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
!
USE MODD_CO2V_PAR,  ONLY : XMCO2, XSPIN_CO2
USE MODD_CSTS,      ONLY : XMD
!
USE MODI_INIT_IO_SURF_n
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
USE MODI_DEFAULT_ISBA
USE MODI_DEFAULT_CH_DEP
USE MODI_DEFAULT_CH_BIO_FLUX
USE MODI_DEFAULT_DIAG_ISBA
USE MODI_DEFAULT_CROCUS
USE MODI_READ_DEFAULT_ISBA_n
USE MODI_READ_ISBA_CONF_n
USE MODI_READ_PREP_ISBA_SNOW
USE MODI_READ_PREP_ISBA_CARBON
USE MODI_READ_SURF
USE MODI_PREP_CTRL_ISBA
USE MODI_READ_ISBA_DATE
USE MODI_READ_PGD_ISBA_n
USE MODI_COMPUTE_ISBA_PARAMETERS
USE MODI_READ_NAM_PREP_ISBA_n
USE MODI_INI_DATA_PARAM
USE MODI_CONVERT_COVER_ISBA
!
USE MODI_SET_SURFEX_FILEIN
!
USE MODI_END_IO_SURF_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(DATA_TSZ0_t), INTENT(INOUT) :: DTZ
TYPE(DST_t), INTENT(INOUT) :: DST
TYPE(SLT_t), INTENT(INOUT) :: SLT
TYPE(SV_t), INTENT(INOUT) :: SV
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
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
REAL,             DIMENSION(KI),  INTENT(OUT) :: PTSURF    ! surface effective temperature         (K)
!
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
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(KI) :: ZCO2     ! CO2 concentration  (kg/m3)
REAL                :: ZSPINCO2
INTEGER             :: ISPINEND
!
INTEGER             :: ILUOUT   ! unit of output listing file
INTEGER             :: IVERSION       ! surface version
INTEGER             :: IRESP   ! return code
INTEGER             :: ISIZE_LMEB_PATCH   ! Number of patches where multi-energy balance should be applied
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
!
IF (LHOOK) CALL DR_HOOK('INIT_ISBA_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('INIT_ISBAN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
!               Other little things
!
IF (LNAM_READ) THEN
 !
 !*       0.     Defaults
!               --------

 !        0.1. Hard defaults
 !      
 CALL DEFAULT_ISBA(IM%I%XTSTEP, IM%I%XOUT_TSTEP,                           &
                     IM%I%CROUGH,IM%I%CRUNOFF,IM%I%CALBEDO,IM%I%CSCOND,              &
                     IM%I%CC1DRY, IM%I%CSOILFRZ, IM%I%CDIFSFCOND, IM%I%CSNOWRES,     &
                     IM%I%CCPSURF, IM%I%XCGMAX, IM%I%XCDRAG, IM%I%CKSAT, IM%I%LSOC,       &
                     IM%I%CRAIN, IM%I%CHORT, IM%I%LGLACIER, IM%I%LCANOPY_DRAG,       &
                     IM%I%LVEGUPD, IM%I%LSPINUPCARBS, IM%I%LSPINUPCARBW,        &
                     IM%I%XSPINMAXS, IM%I%XSPINMAXW, IM%I%XCO2_START, IM%I%XCO2_END, &
                     IM%I%NNBYEARSPINS, IM%I%NNBYEARSPINW, IM%I%LNITRO_DILU     )
 !                  
 CALL DEFAULT_CH_DEP(IM%CHI%CCH_DRY_DEP)
 CALL DEFAULT_CH_BIO_FLUX(IM%CHI%LCH_BIO_FLUX)                  
 CALL DEFAULT_DIAG_ISBA(IM%DGI%N2M,IM%DGI%LSURF_BUDGET,IM%DGI%L2M_MIN_ZS,IM%DGI%LRAD_BUDGET,   &
                        IM%DGI%LCOEF,IM%DGI%LSURF_VARS,IM%DGEI%LSURF_EVAP_BUDGET,        &
                        IM%DGMI%LSURF_MISC_BUDGET,IM%DGMI%LSURF_DIAG_ALBEDO,       &
                        IM%DGEI%LSURF_BUDGETC,IM%DGMI%LSURF_MISC_DIF,IM%DGI%LPATCH_BUDGET,&
                        IM%DGI%LPGD,IM%DGEI%LRESET_BUDGETC,IM%DGEI%LWATER_BUDGET,         &
                        IM%DGI%XDIAG_TSTEP                                )  
 !
 CALL DEFAULT_CROCUS(IM%I%LSNOWDRIFT,IM%I%LSNOWDRIFT_SUBLIM,IM%I%LSNOW_ABS_ZENITH,&
                     IM%I%CSNOWMETAMO,IM%I%CSNOWRAD)
 ! 
ENDIF
!
!        0.2. Defaults from file header
!    
 CALL READ_DEFAULT_ISBA_n(IM%CHI, IM%DGEI, IM%DGI, IM%DGMI, IM%I, &
                          HPROGRAM)
!
 CALL READ_ISBA_CONF_n(IM%CHI, IM%DGEI, IM%DGI, IM%DGMI, IM%I, &
                       HPROGRAM)
!
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'FULL  ','ISBA  ','READ ')
 CALL READ_SURF(&
                HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL END_IO_SURF_n(HPROGRAM)
!
!*       1.     Reading of configuration:
!               -------------------------
!
!* initialization of snow and carbon schemes
!
IM%I%NNBYEARSOLD = 1
IM%I%NSPINS      = 1
IM%I%NSPINW      = 1
!
IF (HINIT=='PRE') THEN 
  CALL READ_PREP_ISBA_SNOW(HPROGRAM,IM%I%TSNOW%SCHEME,IM%I%TSNOW%NLAYER) 
!
!* initialization of soil carbon scheme
!
  CALL READ_PREP_ISBA_CARBON(HPROGRAM,IM%I%CRESPSL)
!
  IF (IM%I%CRESPSL=='CNT') THEN
    IM%I%NNLITTER = 2
    IM%I%NNLITTLEVS = 2
    IM%I%NNSOILCARB = 3
  ELSE
    IM%I%NNLITTER = 0
    IM%I%NNLITTLEVS = 0
    IM%I%NNSOILCARB = 0
  ENDIF

ELSEIF (HINIT=='ALL') THEN
!
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'NATURE','ISBA  ','READ ')
!
  IF (IVERSION<6) THEN
    IM%I%CRESPSL='DEF'
  ELSE  
    CALL READ_SURF(&
                HPROGRAM,'RESPSL',IM%I%CRESPSL,IRESP)
    CALL READ_SURF(&
                HPROGRAM,'NLITTER',IM%I%NNLITTER,IRESP)
    CALL READ_SURF(&
                HPROGRAM,'NLITTLEVS',IM%I%NNLITTLEVS,IRESP)
    CALL READ_SURF(&
                HPROGRAM,'NSOILCARB',IM%I%NNSOILCARB,IRESP)
    IF(IVERSION>=7.AND.(IM%I%LSPINUPCARBS.OR.IM%I%LSPINUPCARBW))THEN
      CALL READ_SURF(&
                HPROGRAM,'NBYEARSOLD',IM%I%NNBYEARSOLD,IRESP)
    ELSE
      IM%I%NNBYEARSOLD=NUNDEF
    ENDIF
  ENDIF
!
  CALL END_IO_SURF_n(HPROGRAM)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       2.     Physiographic fields
!               --------------------
!
!
!* date
!
SELECT CASE (HINIT)
  CASE ('PGD')
    IM%I%TTIME%TDATE%YEAR = NUNDEF
    IM%I%TTIME%TDATE%MONTH= NUNDEF
    IM%I%TTIME%TDATE%DAY  = NUNDEF
    IM%I%TTIME%TIME       = XUNDEF

  CASE ('PRE')
    CALL PREP_CTRL_ISBA(IM%DGI%N2M,IM%DGI%LSURF_BUDGET,IM%DGI%L2M_MIN_ZS,IM%DGI%LRAD_BUDGET,&
                        IM%DGI%LCOEF,IM%DGI%LSURF_VARS,&
                          IM%DGEI%LSURF_EVAP_BUDGET,IM%DGMI%LSURF_MISC_BUDGET,IM%DGEI%LSURF_BUDGETC,     &
                          IM%DGI%LPATCH_BUDGET,IM%DGMI%LSURF_MISC_DIF,ILUOUT                    )    
    IF (LNAM_READ) CALL READ_NAM_PREP_ISBA_n(HPROGRAM)                        
    CALL READ_ISBA_DATE(&
                        HPROGRAM,HINIT,ILUOUT,HATMFILE,HATMFILETYPE,KYEAR,KMONTH,KDAY,PTIME,IM%I%TTIME)

  CASE DEFAULT
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'NATURE','ISBA  ','READ ')
    CALL READ_SURF(&
                HPROGRAM,'DTCUR',IM%I%TTIME,IRESP)
    CALL END_IO_SURF_n(HPROGRAM)
END SELECT
!
!-----------------------------------------------------------------------------------------------------
! READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
! initialization for I/O
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'NATURE','ISBA  ','READ ')
!
!
!*       2.1    Cover, soil and orographic fields:
!               ---------------------------------
!
 CALL READ_PGD_ISBA_n(IM%CHI, DTCO, IM%DTI, DTZ, DGU, IM%GB, IM%IG, IM%I, &
                      UG, U, SV,GCP, &
                      HPROGRAM,OLAND_USE)
!
ISIZE_LMEB_PATCH=COUNT(IM%I%LMEB_PATCH(:))
!
!
!*       2.2    Check:
!               ------
!
IF ( IM%I%CPHOTO/='NON' .AND. IM%I%NPATCH/=12 .AND. IM%I%NPATCH/=19 )THEN
  CALL ABOR1_SFX('INIT_ISBAN: INCONSISTENCY BETWEEN CPHOTO AND NPATCH')
ENDIF
!
IF (HINIT=='PRE' .AND. IM%I%TSNOW%SCHEME.NE.'3-L' .AND. IM%I%TSNOW%SCHEME.NE.'CRO' .AND. IM%I%CISBA=='DIF') THEN
    CALL ABOR1_SFX("INIT_ISBAN: WITH CISBA = DIF, CSNOW MUST BE 3-L OR CRO")
ENDIF
IF ( IM%I%CPHOTO/='LAI' .AND. IM%I%CPHOTO/='LST' .AND. IM%I%CPHOTO/='NIT' .AND. IM%I%CPHOTO/='NCB' .AND. LAGRIP) THEN
  CALL ABOR1_SFX('INIT_ISBAN: INCONSISTENCY BETWEEN CPHOTO AND LAGRIP')
ENDIF
IF ( IM%I%CPHOTO/='NCB' .AND. IM%I%CRESPSL=='CNT') THEN
  CALL ABOR1_SFX('INIT_ISBAN: INCONSISTENCY BETWEEN CPHOTO AND CRESPSL')
ENDIF
IF (HINIT=='PRE' .AND. ISIZE_LMEB_PATCH>0 .AND. IM%I%TSNOW%SCHEME.NE.'3-L' .AND. IM%I%TSNOW%SCHEME.NE.'CRO') THEN
    CALL ABOR1_SFX("INIT_ISBAN: WITH LMEB_PATCH = TRUE, CSNOW MUST BE 3-L OR CRO")
ENDIF
IF(IM%I%CPHOTO/='NCB'.AND.IM%I%LSPINUPCARBW)THEN
  CALL ABOR1_SFX('INIT_ISBAN: INCONSISTENCY BETWEEN CPHOTO AND LSPINUPCARBW (if not NCB must be false)')
ENDIF
IF(IM%I%CRESPSL/='CNT'.AND.IM%I%LSPINUPCARBS)THEN
  CALL ABOR1_SFX('INIT_ISBAN: INCONSISTENCY BETWEEN CRESPSL AND LSPINUPCARBS (if not CNT must be false)')
ENDIF
IF(IM%I%LSPINUPCARBW.AND.REAL(IM%I%NNBYEARSPINW)>REAL(IM%I%NNBYEARSPINS)*0.5)THEN
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(ILUOUT,*)'INIT_ISBAN: INCONSISTENCY BETWEEN NNBYEARSPINW AND NNBYEARSPINS'
  WRITE(ILUOUT,*)'NNBYEARSPINW MUST BE < TO 0.5 * NNBYEARSPINS'
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  CALL ABOR1_SFX('INIT_ISBAN: INCONSISTENCY BETWEEN NNBYEARSPINW AND NNBYEARSPINS')
ENDIF
IF(IM%I%LSPINUPCARBS.AND.(IM%I%XCO2_START==XUNDEF.OR.IM%I%XCO2_END==XUNDEF))THEN
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(ILUOUT,*)'INIT_ISBAN: INCONSISTENCY BETWEEN LSPINUPCARBS AND XCO2_START OR XCO2_END'
  WRITE(ILUOUT,*)'FOR ISBA-CC SPINUP XCO2_START AND XCO2_END MUST BE DEFINED'
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  CALL ABOR1_SFX('INIT_ISBAN: INCONSISTENCY BETWEEN LSPINUPCARBS AND XCO2_START OR XCO2_END')
ENDIF
!
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
!
!-------------------------------------------------------------------------------
!
! During soil carbon spinup with ISBA-CC: 
!        (1) grass parameters are attributed to all agricultural PFT with atmospheric CO2 concentration 
!            fixed to Pre-industrial CO2 consentration XCO2_START
!        (2) Atmospheric CO2 concentration rampin up from XCO2_START to XCO2_END
!
ISPINEND=IM%I%NNBYEARSPINS-NINT(IM%I%NNBYEARSPINS*XSPIN_CO2)
!
IM%I%LAGRI_TO_GRASS = .FALSE.
!
IF ( IM%I%LSPINUPCARBS .AND. (IM%I%NNBYEARSOLD <= ISPINEND) ) THEN
!
   IM%I%LAGRI_TO_GRASS = .TRUE.
!
   CALL INI_DATA_PARAM(DTCO%XDATA_VEGTYPE, PSURF=DTCO%XDATA_NATURE, PH_TREE=XDATA_H_TREE,PLAI=XDATA_LAI,                   &
                                  PALBNIR_VEG=XDATA_ALBNIR_VEG, PALBVIS_VEG=XDATA_ALBVIS_VEG,                    &
                                  PALBUV_VEG=XDATA_ALBUV_VEG, PRSMIN=XDATA_RSMIN,                                &
                                  PRGL=XDATA_RGL, PCV=XDATA_CV, PGAMMA=XDATA_GAMMA,                              &
                                  PGMES=XDATA_GMES, PGC=XDATA_GC, PBSLAI=XDATA_BSLAI,                            &
                                  PSEFOLD=XDATA_SEFOLD, PLAIMIN_OUT=XDATA_LAIMIN, PDMAX=XDATA_DMAX,              &
                                  PSTRESS=XDATA_STRESS, PF2I=XDATA_F2I, PVEG_OUT=XDATA_VEG,                      &
                                  PGREEN=XDATA_GREEN, PZ0=XDATA_Z0, PZ0_O_Z0H=XDATA_Z0_O_Z0H,                    &
                                  PEMIS_ECO=XDATA_EMIS_ECO, PWRMAX_CF=XDATA_WRMAX_CF,                            &
                                  PROOT_LIN=XDATA_ROOT_LIN, PROOT_EXTINCTION=XDATA_ROOT_EXTINCTION,              &
                                  PSOILRC_SO2=XDATA_SOILRC_SO2, PSOILRC_O3=XDATA_SOILRC_O3, PRE25=XDATA_RE25,    &
                                  PCE_NITRO=XDATA_CE_NITRO,PCF_NITRO=XDATA_CF_NITRO,PCNA_NITRO=XDATA_CNA_NITRO,  &
                                  PGMES_ST=XDATA_GMES_ST, PGC_ST=XDATA_GC_ST, PBSLAI_ST=XDATA_BSLAI_ST,          &
                                  PSEFOLD_ST=XDATA_SEFOLD_ST, PDMAX_ST=XDATA_DMAX_ST, OAGRI_TO_GRASS=IM%I%LAGRI_TO_GRASS)
!
   ZCO2(:) = PRHOA(:) * IM%I%XCO2_START * 1.E-6 * XMCO2 / XMD
!
ELSEIF(IM%I%LSPINUPCARBS .AND. (IM%I%NNBYEARSOLD > ISPINEND) .AND. (IM%I%NNBYEARSOLD <= IM%I%NNBYEARSPINS) )THEN
!
   ZSPINCO2 = IM%I%XCO2_START + (IM%I%XCO2_END-IM%I%XCO2_START) * REAL(IM%I%NNBYEARSOLD - ISPINEND) / &
                REAL(IM%I%NNBYEARSPINS - ISPINEND)
!
   ZCO2(:) = PRHOA(:) * ZSPINCO2 * 1.E-6 * XMCO2 / XMD
!
ELSE
!
   ZCO2(:) = PCO2(:)
!
ENDIF
!
!-----------------------------------------------------------------------------------------------------
! END READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------------------------
! Make sure some diags are conputed when coupled with atmosphere
!-----------------------------------------------------------------------------------------------------
!
IF(LCPL_GCM.AND.IM%DGI%LSURF_BUDGET)THEN
  IM%DGEI%LSURF_EVAP_BUDGET=.TRUE.
ENDIF
!
!-----------------------------------------------------------------------------------------------------
!
IF (OLAND_USE .OR. HINIT=='PGD') THEN
  ! ISBA diagnostic PGD fields to improve efficiency in further PREP steps
  IF (IM%I%LECOCLIMAP) THEN
    ALLOCATE(IM%I%XDG(KI,IM%I%NGROUND_LAYER,IM%I%NPATCH))
    IF (IM%I%CISBA=='DIF') THEN
      ALLOCATE(IM%I%NWG_LAYER(KI,IM%I%NPATCH))
    ELSE
      ALLOCATE(IM%I%NWG_LAYER(0,0))
    END IF
    CALL CONVERT_COVER_ISBA(DTCO,IM%I,IM%I%CISBA,1,IM%I%XCOVER,&
                            IM%I%LCOVER,'   ','NAT',&
                            PSOILGRID=IM%I%XSOILGRID,PDG=IM%I%XDG,&
                            KWG_LAYER=IM%I%NWG_LAYER)
  END IF
! end of initialization
IF (LHOOK) CALL DR_HOOK('INIT_ISBA_N',1,ZHOOK_HANDLE)
  RETURN
END IF
!
 CALL COMPUTE_ISBA_PARAMETERS(DTCO, DGU, UG, U, IM, DST, SLT, SV, &
                             HPROGRAM,HINIT,OLAND_USE,                  &
                             KI,KSV,KSW,                                &
                             HSV,ZCO2,PRHOA,                            &
                             PZENITH,PSW_BANDS,PDIR_ALB,PSCA_ALB,       &
                             PEMIS,PTSRAD,PTSURF,                       &
                             HTEST                                )
!
IF ( IM%I%CSNOWMETAMO/="B92" ) THEN
  CALL READ_FZ06('drdt_bst_fit_60.nc')
ENDIF
!
IF ( IM%I%CSNOWRAD=="TAR" .OR. IM%I%CSNOWRAD=="TA1" .OR.  IM%I%CSNOWRAD=="TA2" ) THEN
  CALL INIT_TARTES()
END IF
!
IF (LHOOK) CALL DR_HOOK('INIT_ISBA_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_ISBA_n
