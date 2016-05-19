!     ###############################################################################
SUBROUTINE COUPLING_ISBA_n(HPROGRAM, HCOUPLING,                                              &
                 PTSTEP, KYEAR, KMONTH, KDAY, PTIME, KI, KSV, KSW, PTSUN, PZENITH, PZENITH2, &
                 PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV,                 &
                 PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA,                   &
                 PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,                                    &
                 PTRAD, PDIR_ALB, PSCA_ALB, PEMIS,                                           &
                 PPEW_A_COEF, PPEW_B_COEF,                                                   &
                 PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,                         &
                 HTEST                                                                       )  
!     ###############################################################################
!
!!****  *COUPLING_ISBA_n * - Driver for ISBA time step   
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!! First, all actions dependant on each patch is donbe independantly
!!     (loop on patches)
!! Second, actions common to all patches (e.g. prescription of new vegetation)
!! Third, energy fluxes are averaged
!!
!! Nota that chemical fluxes are also treated.
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
!!      P Le Moigne 11/2004 add new diagnostics for isba
!!      A.Bogatchev 09/2005 EBA snow option
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      P Le Moigne 02/2006 z0h with snow
!!      P.Le Moigne 06/2006 seeding and irrigation
!!      B. Decharme   2008  reset the subgrid topographic effect on the forcing
!!                          PSNV allways <= PSNG
!!                          News diag
!!                          Flooding scheme and allows TRIP variables coupling
!!      A.L. Gibelin 04/2009 : Add respiration diagnostics
!!      A.L. Gibelin 04/2009 : BIOMASS and RESP_BIOMASS arrays 
!!      A.L. Gibelin 04/2009 : TAU_WOOD for NCB option 
!!      A.L. Gibelin 05/2009 : Add carbon spinup
!!      A.L. Gibelin 06/2009 : Soil carbon variables for CNT option
!!      A.L. Gibelin 07/2009 : Suppress RDK and transform GPP as a diagnostic
!!      A.L. Gibelin 07/2009 : Suppress PPST and PPSTF as outputs
!!        S.Lafont   01/2011 : add PTSTEP as arg of diag_misc
!!       B.Decharme  09/2012 : Bug in hydro_glacier calculation with ES or Crocus
!!                             New wind implicitation
!!                             New soil carbon spinup and diag
!!                             Isba budget
!!-------------------------------------------------------------------
!
USE MODD_CSTS,         ONLY : XRD, XRV, XP00, XCPD, XPI,XAVOGADRO
USE MODD_SURF_PAR,     ONLY : XUNDEF
USE MODD_SNOW_PAR,     ONLY : XZ0SN
USE MODD_TYPE_DATE_SURF
USE MODD_ISBA_n,       ONLY : NSIZE_NATURE_P, NR_NATURE_P, CROUGH, NPATCH, LGLACIER,     &
                                NNBIOMASS, XABC, XPOI, CSNOWRES, CDIFSFCOND, CSOILFRZ,   &
                                CSCOND, CC1DRY, CRUNOFF, CPHOTO, LTR_ML, CISBA, XPATCH,  &
                                TTIME, CALBEDO, XCOVER, XLAI, XVEG, XZ0, XEMIS,          &
                                XALBNIR, XALBVIS, XALBUV, XEMIS_NAT,  XTSRAD_NAT,        &
                                XALBNIR_VEG, XALBVIS_VEG, XALBUV_VEG, NGROUND_LAYER,     &
                                XALBNIR_DRY, XALBVIS_DRY, XALBUV_DRY,                    &
                                XALBNIR_SOIL, XALBVIS_SOIL, XALBUV_SOIL,                 &
                                XALBNIR_WET, XALBVIS_WET, XALBUV_WET, XWG, XWSAT,        &
                                XRSMIN, XGAMMA, XWRMAX_CF, XRGL, XCV, XZ0_O_Z0H,         &
                                XVEGTYPE, XROOTFRAC, XGMES, XBSLAI, XLAIMIN, XSEFOLD,    &
                                XGC, XF2I, LSTRESS, XH_TREE, XDMAX, XRE25,               &
                                XZ0EFFIP, XZ0EFFIM, XZ0EFFJP, XZ0EFFJM, XZ0EFFJPDIR,     &
                                XAOSIP, XAOSIM, XAOSJP, XAOSJM,                          &
                                XHO2IP, XHO2IM, XHO2JP, XHO2JM, TSNOW, CRESPSL,          &
                                XCE_NITRO, XCF_NITRO, XCNA_NITRO, LECOCLIMAP, CCPSURF,   &
                                TSEED, TREAP, XWATSUP, XIRRIG, XCGMAX,                   &
                                CKSAT, CSOC, CHORT, CRAIN, XMUF, XFSAT, LTRIP,           &
                                LFLOOD, XFFLOOD, XPIFLOOD, LTEMP_ARP, XSODELX,           &
                                LVEGUPD, NLAYER_HORT, NLAYER_DUN,                        &
                                LSPINUPCARBS, LSPINUPCARBW, XSPINMAXS, XSPINMAXW,        &
                                NNBYEARSPINS, NNBYEARSPINW, NNBYEARSOLD, NSPINS, NSPINW  
!
USE MODD_SURF_ATM,    ONLY : LNOSOF, CIMPLICIT_WIND
USE MODD_SURF_ATM_n,  ONLY : NDIM_FULL
!
USE MODD_DST_n,       ONLY : XSFDST, XSFDSTM, XEMISRADIUS_DST, XEMISSIG_DST
USE MODD_SLT_n,       ONLY : XSFSLT, XEMISRADIUS_SLT, XEMISSIG_SLT
USE MODD_DST_SURF
USE MODD_SLT_SURF
USE MODE_DSLT_SURF
!
USE MODD_PACK_ISBA,   ONLY : XP_SSO_SLOPE, XP_Z0, XP_Z0REL, XP_Z0EFFIP,        &
                             XP_Z0EFFIM, XP_Z0EFFJP, XP_Z0EFFJM, XP_Z0FLOOD,   &
                             XP_AOSIP, XP_AOSIM ,XP_AOSJP, XP_AOSJM, XP_HO2IP, &
                             XP_HO2IM, XP_HO2JP, XP_HO2JM, XP_Z0_O_Z0H,        &
                             XP_ALBNIR, XP_ALBVIS, XP_ALBUV, XP_ALBNIR_VEG,    &
                             XP_ALBVIS_VEG, XP_ALBUV_VEG, XP_ALBNIR_SOIL,      &
                             XP_ALBVIS_SOIL, XP_ALBUV_SOIL, NK_WG_LAYER,       &
                             XP_RSMIN, XP_RGL, XP_GAMMA, XP_CV, XP_RUNOFFD,    &
                             XP_WRMAX_CF, XP_VEG, XP_LAI, XP_DZG, XP_DZDIF,    &
                             XP_EMIS, XP_VEGTYPE_PATCH, XP_RUNOFFB, XP_CGSAT,  &
                             XP_C1SAT, XP_C2REF, XP_C3, XP_C4B, XP_C4REF,      &
                             XP_ACOEF, XP_PCOEF, XP_TAUICE, XP_WDRAIN,         &
                             XP_TDEEP, XP_GAMMAT, XP_PSN, XP_PSNG, XP_PSNV,    &
                             XP_PSNV_A, XP_IRRIG, XP_WATSUP, XP_THRESHOLD,     &
                             XP_LIRRIGATE, XP_LIRRIDAY, LP_STRESS, XP_GC,      &
                             XP_F2I, XP_DMAX, XP_AH, XP_BH, XP_GMES, XP_FZERO, &
                             XP_EPSO, XP_GAMM, XP_QDGAMM, XP_QDGMES, XP_T1GMES,&
                             XP_T2GMES, XP_AMAX, XP_QDAMAX, XP_T1AMAX,         &
                             XP_T2AMAX, XP_DG, XP_ROOTFRAC, XP_WFC, XP_WWILT,  &
                             XP_WSAT, XP_BCOEF, XP_CONDSAT, XP_MPOTSAT,        &
                             XP_HCAPSOIL, XP_CONDDRY, XP_CONDSLD,              &
                             XP_D_ICE, XP_KSAT_ICE, XP_SOILWGHT,               &
                             XP_MUF, XP_FSAT, XP_FF, XP_FFG, XP_FFV,           &
                             XP_FFROZEN, XP_ALBF, XP_EMISF, XP_FFLOOD,         &
                             XP_PIFLOOD, XP_LAT, XP_LON, XP_TG, XP_WG, XP_WGI, &
                             XP_CPS, XP_LVTT, XP_LSTT, XP_WR, XP_RESA, XP_ANFM,&
                             XP_SNOWALB, XP_SNOWSWE, XP_SNOWHEAT, XP_SNOWRHO,  &
                             XP_SNOWGRAN1, XP_SNOWGRAN2, XP_SNOWHIST,          &
                             XP_SNOWAGE, XP_SNOWEMIS, XP_LE, XP_FAPARC,        &
                             XP_FAPIRC, XP_LAI_EFFC, XP_MUS, XP_AN, XP_ANDAY,  &
                             XP_ANF, XP_ICE_STO, XP_ALBVIS_DRY, XP_ALBNIR_DRY, &
                             XP_ALBUV_DRY, XP_ALBVIS_WET, XP_ALBNIR_WET,       &
                             XP_ALBUV_WET, XP_H_TREE, XP_BSLAI, XP_LAIMIN,     &
                             XP_SEFOLD, XP_ANMAX, XP_CE_NITRO, XP_CF_NITRO,    &
                             XP_CNA_NITRO, XP_BSLAI_NITRO, XP_BIOMASS,         &
                             XP_RESP_BIOMASS, XP_INCREASE, XP_TURNOVER,        &
                             XP_TAU_WOOD, TP_SEED, TP_REAP, XP_RE25, XP_LITTER,&
                             XP_LIGNIN_STRUC, XP_SOILCARB,  XP_CLAY, XP_SAND,  &
                             XP_DIR_ALB_WITH_SNOW, XP_SCA_ALB_WITH_SNOW
!
USE MODD_PACK_DIAG_ISBA, ONLY : XP_Z0EFF, XP_Z0_WITH_SNOW, XP_Z0H_WITH_SNOW,   &
                                XP_SNOWFREE_ALB, XP_SNOWFREE_ALB_VEG,          &
                                XP_SNOWFREE_ALB_SOIL, XP_IFLOOD, XP_PFLOOD,    &
                                XP_LE_FLOOD, XP_LEI_FLOOD, XP_GRNDFLUX,        &
                                XP_HPSNOW, XP_SNOWHMASS, XP_SMELTFLUX,         &
                                XP_RNSNOW, XP_HSNOW, XP_GFLUXSNOW,             &
                                XP_USTARSNOW, XP_SRSFC, XP_RRSFC, XP_LESL,     &
                                XP_CDSNOW, XP_CHSNOW, XP_TSRAD, XP_TS, XP_HV,  &
                                XP_QS, XP_SNOWTEMP, XP_SNOWLIQ, XP_SNOWDZ,     &
                                XP_CG, XP_C1, XP_C2, XP_WGEQ, XP_CT, XP_CH,    &
                                XP_CD, XP_CDN, XP_RI, XP_HU, XP_HUG, XP_ALBT,  &
                                XP_RS, XP_RN, XP_H, XP_LEI, XP_LEGI, XP_LEG,   &
                                XP_LEV, XP_LES, XP_LER, XP_LETR, XP_EVAP,      &
                                XP_GFLUX, XP_RESTORE, XP_DRAIN, XP_RUNOFF,     &
                                XP_MELT, XP_MELTADV, XP_RN_ISBA, XP_H_ISBA,    &
                                XP_LEG_ISBA, XP_LEGI_ISBA, XP_LEV_ISBA,        &
                                XP_LETR_ISBA, XP_USTAR_ISBA, XP_LER_ISBA,      &
                                XP_LE_ISBA, XP_LEI_ISBA, XP_GFLUX_ISBA,        &
                                XP_HORT, XP_DRIP, XP_RRVEG, XP_IACAN,          &
                                XP_GPP, XP_FAPAR, XP_FAPIR, XP_FAPAR_BS,       &
                                XP_FAPIR_BS, XP_ICEFLUX, XP_IRRIG_FLUX,        &
                                XP_RESP_AUTO, XP_RESP_ECO, XP_DWG, XP_DWGI,    &
                                XP_DWR, XP_DSWE, XP_WATBUD  
!                         
USE MODD_PACK_CH_ISBA,   ONLY : XP_SOILRC_SO2, XP_SOILRC_O3, XP_DEP

USE MODD_CH_ISBA_n,      ONLY : CSV, CCH_DRY_DEP, LCH_BIO_FLUX, XDEP, LCH_NO_FLUX,&
                                  NBEQ, NSV_CHSBEG, NSV_CHSEND,                   &
                                  NSV_DSTBEG, NSV_DSTEND, NAEREQ, NDSTEQ, NSLTEQ, &
                                  NSV_AERBEG, NSV_AEREND, NSV_SLTBEG, NSV_SLTEND  
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK
!
USE MODD_AGRI,           ONLY : LAGRIP
USE MODD_DEEPSOIL,       ONLY : LDEEPSOIL
!
USE MODI_IRRIGATION_UPDATE
USE MODI_ADD_FORECAST_TO_DATE_SURF
USE MODI_Z0EFF
USE MODI_ISBA
USE MODI_AVERAGE_FLUX
USE MODI_AVERAGE_RAD
USE MODI_AVERAGE_DIAG_ISBA_n
USE MODI_VEGETATION_EVOL
USE MODI_VEGETATION_UPDATE
USE MODI_CARBON_EVOL
USE MODI_SUBSCALE_Z0EFF
USE MODI_SOIL_ALBEDO
USE MODI_ALBEDO
USE MODI_DIAG_INLINE_ISBA_n
USE MODI_DIAG_EVAP_ISBA_n
USE MODI_DIAG_MISC_ISBA_n
!
USE MODI_UPDATE_RAD_ISBA_n
USE MODI_DEEPSOIL_UPDATE
USE MODI_ISBA_SGH_UPDATE
USE MODI_ISBA_FLOOD_PROPERTIES
USE MODI_DIAG_CPL_ESM_ISBA
USE MODI_HYDRO_GLACIER
USE MODI_ISBA_ALBEDO
USE MODI_CARBON_SPINUP
USE MODI_PACK_ISBA_PATCH_n    
USE MODI_PACK_ISBA_PATCH_GET_SIZE_n
USE MODI_PACK_CH_ISBA_PATCH_n     
USE MODI_PACK_DIAG_PATCH_n
USE MODI_PACK_DIAG_PATCH_GET_SIZE_n
USE MODI_UNPACK_ISBA_PATCH_n     
USE MODI_UNPACK_CH_ISBA_PATCH_n     
USE MODI_UNPACK_DIAG_PATCH_n     
USE MODI_CH_AER_DEP
USE MODI_ABOR1_SFX
USE MODI_AVERAGE_DIAG_EVAP_ISBA_n
USE MODI_AVERAGE_DIAG_MISC_ISBA_n
USE MODI_CH_BVOCEM_n
USE MODI_SOILEMISNO_n
USE MODI_CH_DEP_ISBA
USE MODI_DSLT_DEP
USE MODI_COUPLING_DST_n
USE MODI_COUPLING_SURF_TOPD
USE MODI_ISBA_BUDGET_INIT
USE MODI_ISBA_BUDGET
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=1),    INTENT(IN)  :: HCOUPLING ! type of coupling
                                              ! 'E' : explicit
                                              ! 'I' : implicit
INTEGER,             INTENT(IN)  :: KYEAR     ! current year (UTC)
INTEGER,             INTENT(IN)  :: KMONTH    ! current month (UTC)
INTEGER,             INTENT(IN)  :: KDAY      ! current day (UTC)
REAL,                INTENT(IN)  :: PTIME     ! current time since midnight (UTC, s)
INTEGER,             INTENT(IN)  :: KI        ! number of points
INTEGER,             INTENT(IN)  :: KSV       ! number of scalars
INTEGER,             INTENT(IN)  :: KSW       ! number of short-wave spectral bands
REAL, DIMENSION(KI), INTENT(IN)  :: PTSUN     ! solar time                    (s from midnight)
REAL,                INTENT(IN)  :: PTSTEP    ! atmospheric time-step                 (s)
REAL, DIMENSION(KI), INTENT(IN)  :: PZREF     ! height of T,q forcing                 (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PUREF     ! height of wind forcing                (m)
!
REAL, DIMENSION(KI), INTENT(IN)  :: PTA       ! air temperature forcing               (K)
REAL, DIMENSION(KI), INTENT(IN)  :: PQA       ! air humidity forcing                  (kg/m3)
REAL, DIMENSION(KI), INTENT(IN)  :: PRHOA     ! air density                           (kg/m3)
REAL, DIMENSION(KI,KSV),INTENT(IN) :: PSV     ! scalar variables
!                                             ! chemistry:   first char. in HSV: '#'  (molecule/m3)
!   
CHARACTER(LEN=6), DIMENSION(KSV),INTENT(IN):: HSV  ! name of all scalar variables!
REAL, DIMENSION(KI), INTENT(IN)  :: PU        ! zonal wind                            (m/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PV        ! meridian wind                         (m/s)
REAL, DIMENSION(KI,KSW),INTENT(IN) :: PDIR_SW ! direct  solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI,KSW),INTENT(IN) :: PSCA_SW ! diffuse solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KSW),INTENT(IN)  :: PSW_BANDS ! mean wavelength of each shortwave band (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH   ! zenithal angle at t  (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH2  ! zenithal angle at t+1(radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PLW       ! longwave radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI), INTENT(IN)  :: PPS       ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PPA       ! pressure at forcing level             (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PZS       ! atmospheric model orography           (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PCO2      ! CO2 concentration in the air          (kg/kg)
REAL, DIMENSION(KI), INTENT(IN)  :: PSNOW     ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PRAIN     ! liquid precipitation                  (kg/m2/s)
!
!
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTH     ! flux of heat                          (W/m2)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTQ     ! flux of water vapor                   (kg/m2/s)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFU      ! zonal momentum flux                   (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFV      ! meridian momentum flux                (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFCO2    ! flux of CO2 positive toward the atmosphere (m/s*kg_CO2/kg_air)
REAL, DIMENSION(KI,KSV),INTENT(OUT):: PSFTS   ! flux of scalar var.                   (kg/m2/s)
!
REAL, DIMENSION(KI), INTENT(OUT) :: PTRAD     ! radiative temperature                 (K)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PDIR_ALB! direct albedo for each spectral band  (-)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PSCA_ALB! diffuse albedo for each spectral band (-)
REAL, DIMENSION(KI), INTENT(OUT) :: PEMIS     ! emissivity                            (-)
!
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_A_COEF! implicit coefficients
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_B_COEF! needed if HCOUPLING='I'
REAL, DIMENSION(KI), INTENT(IN) :: PPET_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPET_B_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_B_COEF
 CHARACTER(LEN=2),    INTENT(IN) :: HTEST ! must be equal to 'OK'
!
!
!*      0.2    declarations of local variables
!
!* forcing variables
!
REAL, DIMENSION(KI)     :: ZWIND    ! lowest atmospheric level wind speed           (m/s)
REAL, DIMENSION(KI)     :: ZDIR     ! wind direction                        (rad from N clockwise)
REAL, DIMENSION(KI)     :: ZEXNA    ! Exner function at lowest atmospheric level    (-)
REAL, DIMENSION(KI)     :: ZEXNS    ! Exner function at surface                     (-)
REAL, DIMENSION(KI)     :: ZALFA    ! Wind direction                                (-)
REAL, DIMENSION(KI)     :: ZQA      ! specific humidity                             (kg/kg)
REAL, DIMENSION(KI)     :: ZCO2     ! CO2 concentration                             (kg/kg)
REAL, DIMENSION(KI)     :: ZPEQ_A_COEF ! specific humidity implicit
REAL, DIMENSION(KI)     :: ZPEQ_B_COEF ! coefficients (hum. in kg/kg)
! Patch outputs:
!
REAL, DIMENSION(KI,NPATCH) :: ZSFTH_TILE     ! surface heat flux (W/m2)
REAL, DIMENSION(KI,NPATCH) :: ZSFTQ_TILE     ! surface vapor flux (kg/m2/s)
REAL, DIMENSION(KI,NPATCH) :: ZSFCO2_TILE    ! surface CO2 flux positive toward the atmosphere (m/s*kg_CO2/kg_air)
REAL, DIMENSION(KI,NPATCH) :: ZSFU_TILE      ! zonal momentum flux
REAL, DIMENSION(KI,NPATCH) :: ZSFV_TILE      ! meridian momentum flux
REAL, DIMENSION(KI,NPATCH) :: ZTRAD_TILE     ! radiative surface temperature
REAL, DIMENSION(KI,NPATCH) :: ZEMIS_TILE     ! emissivity
REAL, DIMENSION(KI,KSW,NPATCH) :: ZDIR_ALB_TILE  ! direct albedo
REAL, DIMENSION(KI,KSW,NPATCH) :: ZSCA_ALB_TILE  ! diffuse albedo
REAL, DIMENSION(KI,KSV,NPATCH) :: ZSFTS_TILE     ! scalar surface flux
!
REAL, DIMENSION(KI, NPATCH) :: ZCPL_DRAIN     ! For the coupling with TRIP
REAL, DIMENSION(KI, NPATCH) :: ZCPL_RUNOFF    ! For the coupling with TRIP
REAL, DIMENSION(KI, NPATCH) :: ZCPL_EFLOOD    ! For the coupling with TRIP
REAL, DIMENSION(KI, NPATCH) :: ZCPL_PFLOOD    ! For the coupling with TRIP
REAL, DIMENSION(KI, NPATCH) :: ZCPL_IFLOOD    ! For the coupling with TRIP
REAL, DIMENSION(KI, NPATCH) :: ZCPL_ICEFLUX
!
! for chemical computations
!
REAL, DIMENSION(KI, NPATCH) :: ZSW_FORBIO
!
REAL                       :: ZCONVERTFACM0_SLT, ZCONVERTFACM0_DST
REAL                       :: ZCONVERTFACM3_SLT, ZCONVERTFACM3_DST
REAL                       :: ZCONVERTFACM6_SLT, ZCONVERTFACM6_DST
!
! dimensions and loop counters
!
INTEGER :: INI_FLOOD
INTEGER :: ISWB   ! number of spectral shortwave bands
INTEGER :: JSWB   ! loop on number of spectral shortwave bands
INTEGER :: JPATCH ! loop on patches
INTEGER :: JSV, IDST, IMOMENT, II
INTEGER :: JLAYER, JMODE, JSV_IDX
!
! logical units
!
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! --------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('COUPLING_ISBA_N',0,ZHOOK_HANDLE)
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('COUPLING_ISBAN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
! --------------------------------------------------------------------------------------
!
!*      1.     Initializations
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Allocations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ZSFTH_TILE   (:,:)   = XUNDEF
ZSFTQ_TILE   (:,:)   = XUNDEF
ZSFCO2_TILE  (:,:)   = XUNDEF
ZSFU_TILE    (:,:)   = XUNDEF
ZSFV_TILE    (:,:)   = XUNDEF
ZTRAD_TILE   (:,:)   = XUNDEF
ZEMIS_TILE   (:,:)   = XUNDEF
ZDIR_ALB_TILE(:,:,:) = XUNDEF
ZSCA_ALB_TILE(:,:,:) = XUNDEF
!
ZSFTS_TILE(:,:,:) = 0.
!
ZCPL_DRAIN(:,:)   = 0.0
ZCPL_RUNOFF(:,:)  = 0.0
ZCPL_EFLOOD(:,:)  = 0.0
ZCPL_PFLOOD(:,:)  = 0.0
ZCPL_IFLOOD(:,:)  = 0.0
ZCPL_ICEFLUX(:,:) = 0.0
!
ZSW_FORBIO(:,:)   =  XUNDEF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Forcing Modifications:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ZDIR=0.
!
DO JJ=1,SIZE(PQA) 
! specific humidity (conversion from kg/m3 to kg/kg)
!
  ZQA(JJ) = PQA(JJ) / PRHOA(JJ)
  ZPEQ_A_COEF(JJ) = PPEQ_A_COEF(JJ) / PRHOA(JJ)
  ZPEQ_B_COEF(JJ) = PPEQ_B_COEF(JJ) / PRHOA(JJ)
!
  ZCO2(JJ) = PCO2(JJ) / PRHOA(JJ)
!
!
! Other forcing variables depending on incoming forcing (argument list)JJ
!
  ZEXNS(JJ)   = (PPS(JJ)/XP00)**(XRD/XCPD)
  ZEXNA(JJ)   = (PPA(JJ)/XP00)**(XRD/XCPD)
!
!* wind strength
!
  ZWIND(JJ) = SQRT(PU(JJ)**2+PV(JJ)**2)
!
!* wind direction
!
  IF (ZWIND(JJ)>0.)  ZDIR(JJ)=ATAN2(PU(JJ),PV(JJ))
!
!* angle between z0eff J axis and wind direction (rad., clockwise)
!
  ZALFA(JJ) = ZDIR(JJ) - XZ0EFFJPDIR(JJ) * XPI/180.

  IF (ZALFA(JJ)<-XPI) ZALFA(JJ) = ZALFA(JJ) + 2.*XPI
  IF (ZALFA(JJ)>=XPI) ZALFA(JJ) = ZALFA(JJ) - 2.*XPI
!
ENDDO
!
!* number of shortwave spectral bands
!
ISWB = KSW
!
!* irrigation
!
IF (LAGRIP .AND. (CPHOTO=='LAI' .OR. CPHOTO=='LST' .OR. CPHOTO=='NIT'.OR. CPHOTO=='NCB') ) THEN
   CALL IRRIGATION_UPDATE(XIRRIG,PTSTEP,KMONTH,KDAY,PTIME,               &
                            TSEED(:,:)%TDATE%MONTH,TSEED(:,:)%TDATE%DAY,   &
                            TREAP(:,:)%TDATE%MONTH,TREAP(:,:)%TDATE%DAY    )  
ENDIF
!
!* Actualization of the SGH variable (Fmu, Fsat)
!
 CALL ISBA_SGH_UPDATE(CISBA,CRUNOFF,CRAIN,PRAIN,XMUF,XFSAT)
!
!
!* Actualization of deep soil characteristics
!
IF (LDEEPSOIL) THEN
   CALL DEEPSOIL_UPDATE(TTIME%TDATE%MONTH)
ENDIF
!
!* Actualization of soil and wood carbon spinup
!
IF(LSPINUPCARBS.OR.LSPINUPCARBW)THEN
  CALL CARBON_SPINUP(TTIME%TDATE%MONTH,TTIME%TDATE%DAY,TTIME%TIME,       &
                     LSPINUPCARBS, LSPINUPCARBW, XSPINMAXS, XSPINMAXW,   &
                     NNBYEARSPINS, NNBYEARSPINW, NNBYEARSOLD, CPHOTO,    &
                     CRESPSL, NSPINS, NSPINW                             )
ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Time evolution
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
TTIME%TIME = TTIME%TIME + PTSTEP
 CALL ADD_FORECAST_TO_DATE_SURF(TTIME%TDATE%YEAR,TTIME%TDATE%MONTH,TTIME%TDATE%DAY,TTIME%TIME)
!
! --------------------------------------------------------------------------------------
!
!*      2.     Physical evolution
!
 CALL PACK_ISBA_PATCH_GET_SIZE_n
!
 CALL PACK_DIAG_PATCH_GET_SIZE_n
!
! --------------------------------------------------------------------------------------
! Patch Dependent Calculations
! --------------------------------------------------------------------------------------
!
PATCH_LOOP: DO JPATCH=1,NPATCH
!
  IF (NSIZE_NATURE_P(JPATCH) == 0 ) CYCLE
!
! Pack dummy arguments for each patch:
!
  CALL TREAT_PATCH(NSIZE_NATURE_P(JPATCH),NR_NATURE_P(:,JPATCH))
!
ENDDO PATCH_LOOP
!
! --------------------------------------------------------------------------------------
! TRIP coupling update if used :
! --------------------------------------------------------------------------------------
!
IF(LTRIP)THEN
  CALL DIAG_CPL_ESM_ISBA(PTSTEP,ZCPL_DRAIN,ZCPL_RUNOFF,ZCPL_EFLOOD, &
                           ZCPL_PFLOOD,ZCPL_IFLOOD,ZCPL_ICEFLUX         )  
ENDIF
!
! --------------------------------------------------------------------------------------
! Vegetation update (in case of non-interactive vegetation):
! --------------------------------------------------------------------------------------
!
IF ((CPHOTO=='NON' .OR. CPHOTO=='AGS' .OR. CPHOTO=='AST') .AND. LVEGUPD) THEN
     CALL VEGETATION_UPDATE(PTSTEP,TTIME,XCOVER,                       &
                         CISBA,LECOCLIMAP, CPHOTO, LAGRIP, 'NAT',        &
                         XLAI,XVEG,XZ0,                                  &
                         XALBNIR,XALBVIS,XALBUV,XEMIS,                   &
                         XRSMIN,XGAMMA,XWRMAX_CF,                        &
                         XRGL,XCV,                                       &
                         XGMES,XBSLAI,XLAIMIN,XSEFOLD,XGC,XDMAX,         &
                         XF2I, LSTRESS,                                  &
                         XAOSIP,XAOSIM,XAOSJP,XAOSJM,                    &
                         XHO2IP,XHO2IM,XHO2JP,XHO2JM,                    &
                         XZ0EFFIP,XZ0EFFIM,XZ0EFFJP,XZ0EFFJM,            &
                         CALBEDO, XALBNIR_VEG, XALBVIS_VEG, XALBUV_VEG,  &
                         XALBNIR_SOIL, XALBVIS_SOIL, XALBUV_SOIL,        &
                         XCE_NITRO, XCF_NITRO, XCNA_NITRO,               &
                         TSEED, TREAP, XWATSUP, XIRRIG                   )  
END IF
!
! --------------------------------------------------------------------------------------
! Outputs for the atmospheric model or update the snow/flood fraction 
! --------------------------------------------------------------------------------------
! Grid box average fluxes/properties: Arguments and standard diagnostics
!
 CALL AVERAGE_FLUX(XPATCH,                                             &
                  ZSFTH_TILE, ZSFTQ_TILE, ZSFTS_TILE, ZSFCO2_TILE,    &
                  ZSFU_TILE, ZSFV_TILE,                               &
                  PSFTH, PSFTQ, PSFTS, PSFCO2,                        &
                  PSFU, PSFV                                          )  
!
! Albedo, Emissivity and fraction at time t+1
!
 CALL UPDATE_RAD_ISBA_n(LFLOOD, TSNOW%SCHEME, PZENITH2, PSW_BANDS,       &
                       XVEG, XLAI, XZ0, XALBNIR, XALBVIS, XALBUV, XEMIS,&
                       ZDIR_ALB_TILE,ZSCA_ALB_TILE,ZEMIS_TILE           )  
!
 CALL AVERAGE_RAD(XPATCH,                                               &
                 ZDIR_ALB_TILE, ZSCA_ALB_TILE, ZEMIS_TILE, ZTRAD_TILE, &
                 PDIR_ALB,      PSCA_ALB,      XEMIS_NAT,  XTSRAD_NAT  )  
!
PEMIS = XEMIS_NAT
PTRAD = XTSRAD_NAT
!
! Any additional diagnostics (stored in MODD_DIAG_ISBA_n)
!
 CALL AVERAGE_DIAG_ISBA_n(PUREF,PZREF,PSFCO2)
!
! Cumulated diagnostics (stored in MODD_DIAG_EVAP_ISBA_n)
!
 CALL AVERAGE_DIAG_EVAP_ISBA_n(PRAIN,PSNOW)
!
! Miscellaneous diagnostics (stored in MODD_DIAG_MISC_ISBA_n)
!
 CALL AVERAGE_DIAG_MISC_ISBA_n
!
!--------------------------------------------------------------------------------------
!
 CALL COUPLING_SURF_TOPD(HPROGRAM,NDIM_FULL)
!
! --------------------------------------------------------------------------------------
! Snow/Flood fractions, albedo and emissivity update :
! --------------------------------------------------------------------------------------
!
! --------------------------------------------------------------------------------------
! Chemical fluxes :
! --------------------------------------------------------------------------------------
!
IF (NBEQ>0 .AND. LCH_BIO_FLUX) THEN
 CALL CH_BVOCEM_n(ZSW_FORBIO,PRHOA,PSFTS)
ENDIF
!
!SOILNOX
IF (LCH_NO_FLUX) THEN
  CALL SOILEMISNO_n(PU,PV)
ENDIF
!
!==========================================================================================
!
IF (LHOOK) CALL DR_HOOK('COUPLING_ISBA_N',1,ZHOOK_HANDLE)
CONTAINS
!
!=======================================================================================
SUBROUTINE TREAT_PATCH(KSIZE,KMASK)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)               :: KSIZE
INTEGER, INTENT(IN), DIMENSION(KI) :: KMASK
!
REAL, DIMENSION(KSIZE) :: ZP_ZREF    ! height of T,q forcing                 (m)
REAL, DIMENSION(KSIZE) :: ZP_UREF    ! height of wind forcing                (m)
REAL, DIMENSION(KSIZE) :: ZP_U       ! zonal wind                            (m/s)
REAL, DIMENSION(KSIZE) :: ZP_V       ! meridian wind                         (m/s)
REAL, DIMENSION(KSIZE) :: ZP_WIND    ! wind                                  (m/s)
REAL, DIMENSION(KSIZE) :: ZP_DIR     ! wind direction                        (rad from N clockwise)
REAL, DIMENSION(KSIZE) :: ZP_QA      ! air specific humidity forcing         (kg/kg)
REAL, DIMENSION(KSIZE) :: ZP_TA      ! air temperature forcing               (K)
REAL, DIMENSION(KSIZE) :: ZP_CO2     ! CO2 concentration in the air          (kg/kg)
REAL, DIMENSION(KSIZE,KSV) :: ZP_SV      ! scalar concentration in the air       (kg/kg)
REAL, DIMENSION(KSIZE) :: ZP_ZENITH  ! zenithal angle        radian from the vertical)
REAL, DIMENSION(KSIZE) :: ZP_PEW_A_COEF ! implicit coefficients
REAL, DIMENSION(KSIZE) :: ZP_PEW_B_COEF ! needed if HCOUPLING='I'
REAL, DIMENSION(KSIZE) :: ZP_PET_A_COEF
REAL, DIMENSION(KSIZE) :: ZP_PET_B_COEF
REAL, DIMENSION(KSIZE) :: ZP_PEQ_A_COEF
REAL, DIMENSION(KSIZE) :: ZP_PEQ_B_COEF
REAL, DIMENSION(KSIZE) :: ZP_RAIN    ! liquid precipitation                  (kg/m2/s)
REAL, DIMENSION(KSIZE) :: ZP_SNOW    ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KSIZE) :: ZP_LW      ! longwave radiation (W/m2)
REAL, DIMENSION(KSIZE,ISWB) :: ZP_DIR_SW  ! direct  solar radiation (W/m2)
REAL, DIMENSION(KSIZE,ISWB) :: ZP_SCA_SW  ! diffuse solar radiation (W/m2)
REAL, DIMENSION(KSIZE) :: ZP_PS      ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(KSIZE) :: ZP_PA      ! pressure at forcing level             (Pa)
REAL, DIMENSION(KSIZE) :: ZP_ZS      ! atmospheric model orography           (m)
REAL, DIMENSION(KSIZE) :: ZP_SFTQ    ! flux of water vapor <w'q'>            (kg.m-2.s-1)
REAL, DIMENSION(KSIZE) :: ZP_SFTH    ! flux of temperature <w'T'>            (W/m2)
REAL, DIMENSION(KSIZE,KSV) :: ZP_SFTS    ! flux of scalar      <w'sv'>           (mkg/kg/s)
REAL, DIMENSION(KSIZE) :: ZP_SFCO2   ! flux of CO2 positive toward the atmosphere (m/s*kg_CO2/kg_air)
REAL, DIMENSION(KSIZE) :: ZP_USTAR   ! friction velocity                     (m/s)
REAL, DIMENSION(KSIZE) :: ZP_SFU     ! zonal momentum flux                   (pa)
REAL, DIMENSION(KSIZE) :: ZP_SFV     ! meridian momentum flux                (pa)
REAL, DIMENSION(KSIZE) :: ZP_TRAD    ! radiative temperature                 (K)
!
!*  other forcing variables (packed for each patch)
!
REAL, DIMENSION(KSIZE) :: ZP_RHOA    ! lowest atmospheric level air density          (kg/m3)
REAL, DIMENSION(KSIZE) :: ZP_EXNA    ! Exner function at lowest atmospheric level    (-)
REAL, DIMENSION(KSIZE) :: ZP_EXNS    ! Exner function at surface                     (-)
REAL, DIMENSION(KSIZE) :: ZP_ALFA    ! Wind direction   (-)
!
!*  working variables (packed for each patch)
!
REAL, DIMENSION(KSIZE)      :: ZP_ALBNIR_TVEG         ! total vegetation albedo in ir
REAL, DIMENSION(KSIZE)      :: ZP_ALBNIR_TSOIL        ! total soil albedo in ir
REAL, DIMENSION(KSIZE)      :: ZP_ALBVIS_TVEG         ! total vegetation albedo in vis
REAL, DIMENSION(KSIZE)      :: ZP_ALBVIS_TSOIL        ! total soil albedo in vis
REAL, DIMENSION(KSIZE) :: ZP_EMIS                      ! emissiviity
REAL, DIMENSION(KSIZE) :: ZP_GLOBAL_SW                 ! global incoming SW rad.
REAL, DIMENSION(KSIZE) :: ZP_SLOPE_COS                 ! typical slope in the grid cosine
!
REAL, DIMENSION(KSIZE) :: ZP_FFGNOS   !Floodplain fraction over the ground without snow
REAL, DIMENSION(KSIZE) :: ZP_FFVNOS   !Floodplain fraction over vegetation without snow
!
REAL, DIMENSION(KSIZE,NNBIOMASS) :: ZP_RESP_BIOMASS_INST         ! instantaneous biomass respiration (kgCO2/kgair m/s)
!
!*  Aggregated coeffs for evaporative flux calculations
!
REAL, DIMENSION(KSIZE) :: ZP_AC_AGG      ! aggregated aerodynamic resistance
REAL, DIMENSION(KSIZE) :: ZP_HU_AGG      ! aggregated relative humidity
!
!*  ISBA water and energy budget
!
REAL, DIMENSION(KSIZE) :: ZP_WG_INI
REAL, DIMENSION(KSIZE) :: ZP_WGI_INI
REAL, DIMENSION(KSIZE) :: ZP_WR_INI
REAL, DIMENSION(KSIZE) :: ZP_SWE_INI
!
! miscellaneous
!
REAL, DIMENSION(KSIZE)               :: ZP_DEEP_FLUX ! Flux at the bottom of the soil
REAL, DIMENSION(KSIZE)               :: ZP_TDEEP_A   ! coefficient for implicitation of Tdeep
INTEGER :: JJ, JI, JK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('COUPLING_ISBA_n:TREAT_PATCH',0,ZHOOK_HANDLE)
!
!--------------------------------------------------------------------------------------
!
! Pack isba forcing outputs
!
IF (NPATCH==1) THEN
   ZP_ZENITH(:)     = PZENITH     (:)
   ZP_ZREF(:)       = PZREF       (:)
   ZP_UREF(:)       = PUREF       (:)
   ZP_WIND(:)       = ZWIND       (:)
   ZP_U(:)          = PU          (:)
   ZP_V(:)          = PV          (:)
   ZP_DIR(:)        = ZDIR        (:)
   ZP_QA(:)         = ZQA         (:)
   ZP_TA(:)         = PTA         (:)
   ZP_CO2(:)        = ZCO2        (:)
   ZP_SV(:,:)       = PSV         (:,:)
   ZP_PEW_A_COEF(:) = PPEW_A_COEF (:)
   ZP_PEW_B_COEF(:) = PPEW_B_COEF (:)
   ZP_PET_A_COEF(:) = PPET_A_COEF (:)
   ZP_PET_B_COEF(:) = PPET_B_COEF (:)
   ZP_PEQ_A_COEF(:) = ZPEQ_A_COEF (:)
   ZP_PEQ_B_COEF(:) = ZPEQ_B_COEF (:)
   ZP_RAIN(:)       = PRAIN       (:)
   ZP_SNOW(:)       = PSNOW       (:)
   ZP_LW(:)         = PLW         (:)
   ZP_DIR_SW(:,:)   = PDIR_SW     (:,:)
   ZP_SCA_SW(:,:)   = PSCA_SW     (:,:)
   ZP_PS(:)         = PPS         (:)
   ZP_PA(:)         = PPA         (:)
   ZP_ZS(:)         = PZS         (:)
!
   ZP_RHOA(:)       = PRHOA       (:)
   ZP_EXNA(:)       = ZEXNA       (:)
   ZP_EXNS(:)       = ZEXNS       (:)
   ZP_ALFA(:)       = ZALFA       (:)
ELSE
!cdir nodep
!cdir unroll=8
  DO JJ=1,KSIZE
   JI = KMASK(JJ)
   ZP_ZENITH(JJ)     = PZENITH     (JI)
   ZP_ZREF(JJ)       = PZREF       (JI)
   ZP_UREF(JJ)       = PUREF       (JI)
   ZP_WIND(JJ)       = ZWIND       (JI)
   ZP_U(JJ)          = PU          (JI)
   ZP_V(JJ)          = PV          (JI)
   ZP_DIR(JJ)        = ZDIR        (JI)
   ZP_QA(JJ)         = ZQA         (JI)
   ZP_TA(JJ)         = PTA         (JI)
   ZP_CO2(JJ)        = ZCO2        (JI)
   ZP_PEW_A_COEF(JJ) = PPEW_A_COEF (JI)
   ZP_PEW_B_COEF(JJ) = PPEW_B_COEF (JI)
   ZP_PET_A_COEF(JJ) = PPET_A_COEF (JI)
   ZP_PET_B_COEF(JJ) = PPET_B_COEF (JI)
   ZP_PEQ_A_COEF(JJ) = ZPEQ_A_COEF (JI)
   ZP_PEQ_B_COEF(JJ) = ZPEQ_B_COEF (JI)
   ZP_RAIN(JJ)       = PRAIN       (JI)
   ZP_SNOW(JJ)       = PSNOW       (JI)
   ZP_LW(JJ)         = PLW         (JI)
   ZP_PS(JJ)         = PPS         (JI)
   ZP_PA(JJ)         = PPA         (JI)
   ZP_ZS(JJ)         = PZS         (JI)
!
   ZP_RHOA(JJ)       = PRHOA       (JI)
   ZP_EXNA(JJ)       = ZEXNA       (JI)
   ZP_EXNS(JJ)       = ZEXNS       (JI)
   ZP_ALFA(JJ)       = ZALFA       (JI)
  ENDDO
!
  DO JK=1,KSV
!cdir nodep
!cdir unroll=8
    DO JJ=1,KSIZE
      JI=KMASK(JJ)
      ZP_SV(JJ,JK) = PSV(JI,JK)
    ENDDO
  ENDDO
!
  DO JK=1,SIZE(PDIR_SW,2)
!cdir nodep
!cdir unroll=8
    DO JJ=1,KSIZE
      JI=KMASK(JJ)
      ZP_DIR_SW(JJ,JK) = PDIR_SW (JI,JK)
      ZP_SCA_SW(JJ,JK) = PSCA_SW (JI,JK)
    ENDDO
  ENDDO
!
ENDIF
!
!--------------------------------------------------------------------------------------
!
! Pack ISBA input and prognostic variables (modd_isban) for each patch:
!
 CALL PACK_ISBA_PATCH_n(KMASK,KSIZE,JPATCH)     
!

! Pack chemistry input and prognostic variables (modd_ch_isban) for each patch:
!
IF (NBEQ>0) THEN
  IF( CCH_DRY_DEP == "WES89") THEN
    CALL PACK_CH_ISBA_PATCH_n(KMASK,KSIZE,NPATCH,JPATCH)     
  END IF
END IF
!
! Allocate ISBA diagnostics for each patch:
!
 CALL PACK_DIAG_PATCH_n(KSIZE,ISWB)     
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Cosine of the slope typically encoutered in the grid mesh (including subgrid orography)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ZP_SLOPE_COS(:) = 1./SQRT(1.+XP_SSO_SLOPE(:)**2)
IF(LNOSOF)ZP_SLOPE_COS(:) = 1.0
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Snow fractions
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! now caculated at the initialization and at the end of the time step 
! (see update_frac_alb_emis_isban.f90) in order to close the energy budget
! between surfex and the atmosphere. This fact do not change the offline runs.
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! No implicitation of Tdeep
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ZP_TDEEP_A = 0.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Flood properties 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF(LFLOOD)THEN
  WHERE(XP_FFLOOD(:)==0.0) 
    XP_Z0FLOOD(:) = XZ0SN
    ZP_FFGNOS (:) = 0.0
    ZP_FFVNOS (:) = 0.0
  ENDWHERE
  INI_FLOOD =COUNT(XP_FFLOOD(:)>0.0)
  IF (INI_FLOOD>0) &
    CALL ISBA_FLOOD_PROPERTIES(INI_FLOOD,ZP_TA,ZP_EXNA,ZP_RHOA,XP_TG(:,1), &
                               ZP_EXNS,ZP_QA,ZP_WIND,ZP_ZREF,ZP_UREF,ZP_PS,&
                               ZP_SLOPE_COS,XP_VEG,XP_LAI,XP_FFLOOD,       &
                               XP_FFROZEN,XP_Z0FLOOD,ZP_FFGNOS,ZP_FFVNOS   )  
ELSE
  XP_Z0FLOOD = XUNDEF
  ZP_FFGNOS  = 0.0
  ZP_FFVNOS  = 0.0
ENDIF
!

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Surface Roughness lengths (m):
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!* effective roughness
!
 CALL Z0EFF(CROUGH, ZP_ALFA, ZP_ZREF, ZP_UREF, XP_Z0, XP_Z0REL, XP_PSN,   &
     XP_Z0EFFIP,XP_Z0EFFIM,XP_Z0EFFJP,XP_Z0EFFJM, XP_FF, XP_Z0FLOOD,     &
     XP_AOSIP,XP_AOSIM,XP_AOSJP,XP_AOSJM,                                &
     XP_HO2IP,XP_HO2IM,XP_HO2JP,XP_HO2JM,                                &
     XP_Z0_O_Z0H, XP_Z0_WITH_SNOW, XP_Z0H_WITH_SNOW, XP_Z0EFF            )  
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Shortwave computations for outputs (albedo for radiative scheme)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! now caculated at the initialization and at the end of the time step 
! (see update_frac_alb_emis_isban.f90) in order to close the energy budget
! between surfex and the atmosphere. This fact do not change the offline runs.
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Shortwave computations for ISBA inputs (global snow-free albedo)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
! ISBA needs global incoming solar radiation: it currently does
! not distinguish between the scattered and direct components,
! or between different wavelengths.
!
!
!* Snow-free surface albedo for each wavelength
!
 CALL ISBA_ALBEDO(TSNOW%SCHEME, LTR_ML,                                   &
                   ZP_DIR_SW, ZP_SCA_SW, PSW_BANDS,ISWB,                 &
                   XP_ALBNIR, XP_ALBVIS, XP_ALBUV,                       &
                   XP_ALBNIR_VEG, XP_ALBVIS_VEG, XP_ALBUV_VEG,           &
                   XP_ALBNIR_SOIL, XP_ALBVIS_SOIL, XP_ALBUV_SOIL,        &
                   XP_SNOWALB, XP_PSNV, XP_PSNG, XP_ALBF, XP_FFV, XP_FFG,& 
                   ZP_GLOBAL_SW, XP_SNOWFREE_ALB, XP_SNOWFREE_ALB_VEG,   &
                   XP_SNOWFREE_ALB_SOIL, ZP_ALBNIR_TVEG, ZP_ALBVIS_TVEG, &
                   ZP_ALBNIR_TSOIL, ZP_ALBVIS_TSOIL                      )  
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Intialize computation of ISBA water and energy budget
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL ISBA_BUDGET_INIT(CISBA,TSNOW%SCHEME,            &
                      XP_WG,XP_WGI,XP_WR,XP_SNOWSWE, &
                      XP_DG, XP_DZG, ZP_WG_INI,      &
                      ZP_WGI_INI, ZP_WR_INI,         &
                      ZP_SWE_INI                     )
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Over Natural Land Surfaces:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL ISBA(CISBA, CPHOTO, LTR_ML, CRUNOFF, CKSAT, CSOC, CRAIN, CHORT, CC1DRY, CSCOND,      &
          TSNOW%SCHEME, CSNOWRES, CCPSURF, CSOILFRZ, CDIFSFCOND, TTIME, LFLOOD, LTEMP_ARP,&
          LGLACIER, PTSTEP, CIMPLICIT_WIND,                                               &
          XCGMAX, ZP_ZREF, ZP_UREF, ZP_SLOPE_COS, ZP_TA, ZP_QA, ZP_EXNA,                  &
          ZP_RHOA, ZP_PS, ZP_EXNS, ZP_RAIN, ZP_SNOW, ZP_ZENITH, ZP_GLOBAL_SW, ZP_LW,      &
          ZP_WIND, ZP_PEW_A_COEF, ZP_PEW_B_COEF, ZP_PET_A_COEF, ZP_PEQ_A_COEF,            &
          ZP_PET_B_COEF, ZP_PEQ_B_COEF,  XP_RSMIN, XP_RGL, XP_GAMMA, XP_CV, XP_RUNOFFD,   &
          XP_SOILWGHT, NLAYER_HORT, NLAYER_DUN, ZP_ALBNIR_TVEG, ZP_ALBVIS_TVEG,           &
          ZP_ALBNIR_TSOIL, ZP_ALBVIS_TSOIL, XP_SNOWFREE_ALB, XP_WRMAX_CF, XP_VEG, XP_LAI, &
          XP_EMIS, XP_Z0_WITH_SNOW, XP_Z0H_WITH_SNOW, XP_VEGTYPE_PATCH, XP_Z0EFF,         &
          XP_RUNOFFB, XP_CGSAT, XP_C1SAT, XP_C2REF, XP_C3, XP_C4B, XP_C4REF, XP_ACOEF,    &
          XP_PCOEF, XP_TAUICE, XP_WDRAIN, ZP_TDEEP_A, XP_TDEEP, XP_GAMMAT,                &
          XP_PSN, XP_PSNG, XP_PSNV,                                                       &
          XP_PSNV_A, XP_SNOWFREE_ALB_VEG, XP_SNOWFREE_ALB_SOIL, XP_IRRIG, XP_WATSUP,      &
          XP_THRESHOLD, XP_LIRRIGATE, XP_LIRRIDAY, LP_STRESS, XP_GC, XP_F2I, XP_DMAX,     &
          XP_AH, XP_BH, ZP_CO2, XP_GMES, XPOI, XP_FZERO, XP_EPSO, XP_GAMM, XP_QDGAMM,     &
          XP_QDGMES, XP_T1GMES, XP_T2GMES, XP_AMAX, XP_QDAMAX,  XP_T1AMAX, XP_T2AMAX,     &
          XABC, XP_DG, XP_DZG, XP_DZDIF, NK_WG_LAYER, XP_ROOTFRAC, XP_WFC,                &
          XP_WWILT, XP_WSAT, XP_BCOEF, XP_CONDSAT, XP_MPOTSAT, XP_HCAPSOIL, XP_CONDDRY,   &
          XP_CONDSLD, XP_D_ICE, XP_KSAT_ICE, XP_MUF, XP_FF, XP_FFG, XP_FFV, ZP_FFGNOS,    &
          ZP_FFVNOS, XP_FFROZEN, XP_ALBF, XP_EMISF, XP_FFLOOD, XP_PIFLOOD, XP_IFLOOD,     &
          XP_PFLOOD, XP_LE_FLOOD, XP_LEI_FLOOD, XSODELX, XP_LAT, XP_LON, XP_TG, XP_WG,    &
          XP_WGI, XP_CPS, XP_LVTT, XP_LSTT, XP_WR, XP_RESA, XP_ANFM, XP_FSAT,             &
          XP_SNOWALB, XP_SNOWSWE, XP_SNOWHEAT, XP_SNOWRHO, XP_SNOWGRAN1, XP_SNOWGRAN2,    &
          XP_SNOWHIST, XP_SNOWAGE, XP_GRNDFLUX, XP_HPSNOW, XP_SNOWHMASS,  XP_SMELTFLUX,   &
          XP_RNSNOW, XP_HSNOW, XP_GFLUXSNOW, XP_USTARSNOW, XP_SRSFC, XP_RRSFC, XP_LESL,   &
          XP_SNOWEMIS, XP_CDSNOW, XP_CHSNOW, XP_TSRAD, XP_TS, XP_HV, XP_QS, XP_SNOWTEMP,  &
          XP_SNOWLIQ, XP_SNOWDZ, XP_CG, XP_C1, XP_C2, XP_WGEQ, XP_CT, XP_CH, XP_CD,       &
          XP_CDN, XP_RI, XP_HU, XP_HUG, ZP_EMIS, XP_ALBT, XP_RS, XP_LE, XP_RN, XP_H,      &
          XP_LEI, XP_LEGI, XP_LEG, XP_LEV, XP_LES, XP_LER, XP_LETR, XP_EVAP, XP_GFLUX,    &
          XP_RESTORE, ZP_USTAR, XP_DRAIN, XP_RUNOFF, XP_MELT, XP_MELTADV, XP_RN_ISBA,     &
          XP_H_ISBA, XP_LEG_ISBA, XP_LEGI_ISBA, XP_LEV_ISBA, XP_LETR_ISBA, XP_USTAR_ISBA, &
          XP_LER_ISBA, XP_LE_ISBA, XP_LEI_ISBA, XP_GFLUX_ISBA, XP_HORT, XP_DRIP, XP_RRVEG,&
          ZP_AC_AGG, ZP_HU_AGG, XP_FAPARC, XP_FAPIRC, XP_MUS, XP_LAI_EFFC, XP_AN,         &
          XP_ANDAY, ZP_RESP_BIOMASS_INST, XP_IACAN, XP_ANF, XP_GPP, XP_FAPAR, XP_FAPIR,   &
          XP_FAPAR_BS, XP_FAPIR_BS, XP_IRRIG_FLUX, ZP_DEEP_FLUX                           )  
!
ZP_TRAD=XP_TSRAD
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Glacier : ice runoff flux (especally for Earth System Model)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF(LGLACIER)THEN
!           
  CALL HYDRO_GLACIER(PTSTEP,ZP_SNOW,XP_SNOWRHO,XP_SNOWSWE,XP_ICE_STO,XP_ICEFLUX)
!     
ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Calculation of ISBA water and energy budget (and time tendencies of each reservoir)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL ISBA_BUDGET(CISBA,TSNOW%SCHEME,LGLACIER,PTSTEP,          &
                 XP_WG,XP_WGI,XP_WR,XP_SNOWSWE,XP_DG,XP_DZG,  & 
                 ZP_WG_INI,ZP_WGI_INI,ZP_WR_INI,ZP_SWE_INI,   &
                 ZP_RAIN,ZP_SNOW,XP_EVAP,XP_DRAIN,XP_RUNOFF,  &
                 XP_IFLOOD,XP_PFLOOD,XP_ICEFLUX,XP_IRRIG_FLUX,&
                 XP_DWG,XP_DWGI,XP_DWR,XP_DSWE,XP_WATBUD      )
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Evolution of soil albedo, when depending on surface soil wetness:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (CALBEDO=='EVOL' .AND. LECOCLIMAP) THEN
  CALL SOIL_ALBEDO(CALBEDO,                                    &
                   XP_WSAT(:,1),XP_WG(:,1),                    &
                   XP_ALBVIS_DRY,XP_ALBNIR_DRY,XP_ALBUV_DRY,   &
                   XP_ALBVIS_WET,XP_ALBNIR_WET,XP_ALBUV_WET,   &
                   XP_ALBVIS_SOIL,XP_ALBNIR_SOIL,XP_ALBUV_SOIL )  
  !
  CALL ALBEDO(CALBEDO,                                          &
              XP_ALBVIS_VEG,XP_ALBNIR_VEG,XP_ALBUV_VEG,XP_VEG,  &
              XP_ALBVIS_SOIL,XP_ALBNIR_SOIL,XP_ALBUV_SOIL,      &
              XP_ALBVIS,XP_ALBNIR,XP_ALBUV                      )  
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Vegetation evolution for interactive LAI
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (CPHOTO=='LAI' .OR. CPHOTO=='LST' .OR. CPHOTO=='NIT' .OR. CPHOTO=='NCB') THEN
  CALL VEGETATION_EVOL(CISBA, CPHOTO, CRESPSL, CALBEDO, LAGRIP, LTR_ML,           &
                       PTSTEP, KMONTH, KDAY, NSPINW, PTIME, XP_LAT, ZP_RHOA,      &
                       XP_DG, XP_DZG, NK_WG_LAYER,                                &                       
                       XP_TG, XP_ALBNIR_VEG, XP_ALBVIS_VEG, XP_ALBUV_VEG,         &
                       XP_ALBNIR_SOIL, XP_ALBVIS_SOIL, XP_ALBUV_SOIL,             &
                       XP_VEGTYPE_PATCH, XP_SEFOLD, XP_ANMAX, XP_H_TREE, XP_BSLAI,&
                       XP_LAIMIN, ZP_CO2, XP_CE_NITRO, XP_CF_NITRO, XP_CNA_NITRO, &
                       XP_BSLAI_NITRO, XP_GMES, XP_TAU_WOOD, TP_SEED,             &
                       TP_REAP, XP_AOSIP, XP_AOSIM, XP_AOSJP, XP_AOSJM,           &
                       XP_HO2IP, XP_HO2IM, XP_HO2JP, XP_HO2JM, XP_Z0EFFIP,        &
                       XP_Z0EFFIM, XP_Z0EFFJP, XP_Z0EFFJM, XP_LAI, XP_VEG,        &
                       XP_Z0, XP_ALBNIR, XP_ALBVIS, XP_ALBUV, XP_EMIS,            &
                       XP_ANFM, XP_ANDAY, XP_BIOMASS, XP_RESP_BIOMASS,            &
                       ZP_RESP_BIOMASS_INST, XP_INCREASE, XP_TURNOVER             )  
END IF
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Diagnostic of respiration carbon fluxes and soil carbon evolution
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ZP_SFCO2    (:)=0.
XP_RESP_ECO (:)=0.
XP_RESP_AUTO(:)=0.
!
IF ( CPHOTO/='NON' .AND. CRESPSL/='NON' .AND. ANY(XP_LAI(:)/=XUNDEF) ) THEN
  CALL CARBON_EVOL(CISBA, CRESPSL, CPHOTO, PTSTEP, NSPINS,                   &
                   ZP_RHOA, XP_TG, XP_WG, XP_WFC, XP_WWILT, XP_WSAT, XP_SAND,&
                   XP_DG, XP_DZG, NK_WG_LAYER,                               &                   
                   XP_RE25, XP_LAI, ZP_RESP_BIOMASS_INST, XP_TURNOVER,       &
                   XP_LITTER, XP_LIGNIN_STRUC , XP_SOILCARB,                 &
                   XP_RESP_AUTO, XP_RESP_ECO                                 )  
  ! calculation of vegetation CO2 flux
  ! Positive toward the atmosphere
  ZP_SFCO2(:) = XP_RESP_ECO(:) - XP_GPP(:)  
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Reset effecitve roughness lentgh to its nominal value when snow has just disappeared
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL SUBSCALE_Z0EFF(XP_AOSIP,XP_AOSIM,XP_AOSJP,XP_AOSJM,            &
                    XP_HO2IP,XP_HO2IM,XP_HO2JP,XP_HO2JM,XP_Z0,      &
                    XP_Z0EFFIP,XP_Z0EFFIM,XP_Z0EFFJP,XP_Z0EFFJM,    &
                    OMASK=(XP_SNOWSWE(:,1)==0. .AND. XP_PSN(:)>0.)  )   
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Turbulent fluxes
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ZP_SFTH(:) = XP_H(:)
ZP_SFTQ(:) = XP_EVAP(:)

ZP_SFU (:) = 0.
ZP_SFV (:) = 0.
WHERE (ZP_WIND>0.)
  ZP_SFU (:) = - ZP_U(:)/ZP_WIND(:) * ZP_USTAR(:)**2 * ZP_RHOA(:)
  ZP_SFV (:) = - ZP_V(:)/ZP_WIND(:) * ZP_USTAR(:)**2 * ZP_RHOA(:)
END WHERE
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Scalar fluxes
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ZP_SFTS(:,:) = 0.
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
! --------------------------------------------------------------------------------------
! Chemical dry deposition :
! --------------------------------------------------------------------------------------
IF (NBEQ>0) THEN
  IF( CCH_DRY_DEP == "WES89") THEN

    CALL CH_DEP_ISBA         (ZP_USTAR, XP_HU, XP_PSN,             &
                        XP_VEG, XP_LAI, XP_SAND, XP_CLAY, XP_RESA, &
                        XP_RS(:),  XP_Z0(:),                       &
                        ZP_TA, ZP_PA, ZP_TRAD(:),                  &
                        XP_VEGTYPE_PATCH(:,NVT_NO),                &
                        XP_VEGTYPE_PATCH(:,NVT_ROCK),              &
                        CSV(NSV_CHSBEG:NSV_CHSEND),                &
                        XP_SOILRC_SO2,  XP_SOILRC_O3 ,             &
                        XP_DEP(:,1:NBEQ)                           )  
 
    ZP_SFTS(:,NSV_CHSBEG:NSV_CHSEND) = - ZP_SV(:,NSV_CHSBEG:NSV_CHSEND)  &
                                                    * XP_DEP(:,1:NBEQ)  
    IF (NAEREQ > 0 ) THEN
      CALL CH_AER_DEP(ZP_SV(:,NSV_AERBEG:NSV_AEREND),&
                           ZP_SFTS(:,NSV_AERBEG:NSV_AEREND),&
                           ZP_USTAR, XP_RESA,ZP_TA,ZP_RHOA)     
    END IF
  ELSE
    ZP_SFTS(:,NSV_CHSBEG:NSV_CHSEND) = 0.
    ZP_SFTS(:,NSV_AERBEG:NSV_AEREND) = 0.
  ENDIF
ENDIF
!
! --------------------------------------------------------------------------------------
! Dust deposition and emission:
! --------------------------------------------------------------------------------------
!
IF(NDSTEQ>0)THEN
  IDST = NSV_DSTEND - NSV_DSTBEG + 1

  CALL COUPLING_DST_n(           &
            HPROGRAM,                    &!I [char] Name of program
            KSIZE,      &!I [nbr] number of points in patch
            IDST,                        &!I [nbr] number of dust emissions variables
            JPATCH,                      &!I [idx] patch in question
            XP_CLAY(:,1),                &!I [frc] mass fraction clay in first soil layer
            ZP_PS,                       &!I [Pa] surface pressure
            ZP_QA,                       &!I [kg/kg] specific humidity
            XP_RESA,                     &!I [s/m] atmospheric resistance
            ZP_RHOA,                     &!I [kg/m3] atmospheric density
            XP_SAND(:,1),                &!I [frc] mass fraction of sand in first soil layer
            ZP_SFTH,                     &!I [W/m2] surface heat flux
            ZP_SFTQ,                     &!I [kg/m2/s] surface vapor flux
            ZP_TA,                       &!I [K] Atmospheric temperature
            XP_TG(:,1),                  &!I [K] Ground temperature
            ZP_U,                        &!I [m/s] zonal wind at atmospheric height 
            ZP_UREF,                     &!I [m] reference height of wind
            ZP_V,                        &!I [m/s] meridional wind at atmospheric height
            XP_WG(:,1),                  &!I [m3/m3] ground volumetric water content
            XP_WSAT(:,1),                &!I [m3/m3] saturation volumetric water content
            ZP_ZREF,                     &!I [m] reference height of wind
            XP_CD,                       &
            XP_RI,                       &
            XP_Z0H_WITH_SNOW,            &!I [frc] Z0 (heat) with snow
            ZP_SFTS(:,NSV_DSTBEG:NSV_DSTEND)  &!O [kg/m2/sec] flux of dust            
            )  
!
   IF (NSV_AEREND > 0)  THEN ! case of dust/ anthropogenic aerosols coupling
     DO JMODE=1,NDSTMDE

      !Make index which is 0 for first mode, 3 for second, 6 for third etc
       IF (LVARSIG_DST) THEN
         JSV_IDX = (JMODE-1)*3
       ELSE IF (LRGFIX_DST) THEN
         JSV_IDX = JMODE-2
       ELSE
         JSV_IDX = (JMODE-1)*2
       END IF

       DO JSV=1, size(HSV)
         IF ((TRIM(HSV(JSV)) == "@DSTI").AND.(JMODE==3)) THEN 
           ! add dust flux and conversion kg/m2/s into molec.m2/s
           ZP_SFTS(:,JSV) = ZP_SFTS(:,JSV) + ZP_SFTS(:,NSV_DSTBEG-1+JSV_IDX+2)*XAVOGADRO/XMOLARWEIGHT_DST
         END IF
         IF ( (TRIM(HSV(JSV)) == "@DSTJ").AND.(JMODE==2)) THEN 
           ! add dust flux and conversion kg/m2/sec into molec.m2/s
           ZP_SFTS(:,JSV) = ZP_SFTS(:,JSV) + ZP_SFTS(:,NSV_DSTBEG-1+JSV_IDX+2)*XAVOGADRO/XMOLARWEIGHT_DST
         END IF
       END DO
     END DO

    END IF
!Modify fluxes due to dry deposition, we introduce a negative flux where dust is lost
  CALL DSLT_DEP(ZP_SV(:,NSV_DSTBEG:NSV_DSTEND), ZP_SFTS(:,NSV_DSTBEG:NSV_DSTEND), &
                ZP_USTAR, XP_RESA, ZP_TA, ZP_RHOA, XEMISSIG_DST, XEMISRADIUS_DST, &
                JPMODE_DST, XDENSITY_DST, XMOLARWEIGHT_DST, ZCONVERTFACM0_DST,    &
                ZCONVERTFACM6_DST, ZCONVERTFACM3_DST, LVARSIG_DST, LRGFIX_DST,    &
                CVERMOD  )

 !Transfer these fluxes to fluxes understandable by all moments
  CALL MASSFLUX2MOMENTFLUX(           &
    ZP_SFTS(:,NSV_DSTBEG:NSV_DSTEND), & !I/O ![kg/m2/sec] In: flux of only mass, out: flux of moments
    ZP_RHOA,                          & !I [kg/m3] air density
    XEMISRADIUS_DST,                  &!I [um] emitted radius for the modes (max 3)
    XEMISSIG_DST,                     &!I [-] emitted sigma for the different modes (max 3)
    NDSTMDE,                          &
    ZCONVERTFACM0_DST,                &
    ZCONVERTFACM6_DST,                &
    ZCONVERTFACM3_DST,                &
    LVARSIG_DST, LRGFIX_DST           )   

ENDIF !Check on CDSTYN
!
! --------------------------------------------------------------------------------------
! Sea Salt deposition
! --------------------------------------------------------------------------------------
!
IF (NSLTEQ>0) THEN
  CALL DSLT_DEP(ZP_SV(:,NSV_SLTBEG:NSV_SLTEND), ZP_SFTS(:,NSV_SLTBEG:NSV_SLTEND), &
                ZP_USTAR, XP_RESA, ZP_TA, ZP_RHOA, XEMISSIG_SLT, XEMISRADIUS_SLT, &
                JPMODE_SLT, XDENSITY_SLT, XMOLARWEIGHT_SLT, ZCONVERTFACM0_SLT,    &
                ZCONVERTFACM6_SLT, ZCONVERTFACM3_SLT, LVARSIG_SLT, LRGFIX_SLT,    &
                CVERMOD  )  

  CALL MASSFLUX2MOMENTFLUX(           &
    ZP_SFTS(:,NSV_SLTBEG:NSV_SLTEND), & !I/O ![kg/m2/sec] In: flux of only mass, out: flux of moments
    ZP_RHOA,                          & !I [kg/m3] air density
    XEMISRADIUS_SLT,                  &!I [um] emitted radius for the modes (max 3)
    XEMISSIG_SLT,                     &!I [-] emitted sigma for the different modes (max 3)
    NSLTMDE,                          &
    ZCONVERTFACM0_SLT,                &
    ZCONVERTFACM6_SLT,                &
    ZCONVERTFACM3_SLT,                &
    LVARSIG_SLT, LRGFIX_SLT         ) 
ENDIF !Check on CSLTYN
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Inline diagnostics
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL DIAG_INLINE_ISBA_n(ZP_TA, ZP_TRAD, ZP_QA, ZP_PA, ZP_PS, ZP_RHOA, ZP_U, ZP_V,      &
                          ZP_ZREF, ZP_UREF,                                            &
                          XP_CD, XP_CDN, XP_CH, XP_RI, XP_HU, XP_Z0_WITH_SNOW,         &
                          XP_Z0H_WITH_SNOW, XP_Z0EFF,                                  &
                          ZP_SFTH, ZP_SFTQ, ZP_SFU, ZP_SFV, XP_QS,                     &
                          XP_DIR_ALB_WITH_SNOW, XP_SCA_ALB_WITH_SNOW,                  &
                          ZP_DIR_SW, ZP_SCA_SW, ZP_LW, XP_RN                           )  
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Isba offline diagnostics for each patch
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL DIAG_EVAP_ISBA_n(CPHOTO,PTSTEP,KMASK,KSIZE,JPATCH,ZP_RHOA)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Isba offline diagnostics for miscellaneous terms over each patch
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL DIAG_MISC_ISBA_n(PTSTEP, CISBA, CPHOTO, TSNOW%SCHEME, LAGRIP, LTR_ML,    &
                      PTIME, KSIZE, JPATCH, KMASK, XP_THRESHOLD,              &
                      XP_PSN, XP_PSNG, XP_PSNV, XP_FF, XP_FFG, XP_FFV,        &
                      XP_WG, XP_WGI, XP_WFC, XP_WWILT, XP_SNOWSWE, XP_SNOWRHO,&
                      XP_FAPARC, XP_FAPIRC, XP_LAI_EFFC, XP_MUS, XP_FSAT,     &
                      XP_DG, XP_TG       )                  
!
! Unpack ISBA diagnostics (modd_diag_isban) for each patch:ISIZE_MAX = MAXVAL(NSIZE_NATURE_P)

!  (MUST be done BEFORE UNPACK_ISBA_PATCH, because of XP_LE)
!
 CALL UNPACK_DIAG_PATCH_n(KMASK,KSIZE,NPATCH,JPATCH, &
                           ZCPL_DRAIN,ZCPL_RUNOFF,ZCPL_EFLOOD,ZCPL_PFLOOD,           &
                           ZCPL_IFLOOD, ZCPL_ICEFLUX)  
!
! for chemical deposition
!
IF (NBEQ>0) THEN
  IF( CCH_DRY_DEP == "WES89") THEN
    CALL UNPACK_CH_ISBA_PATCH_n(KMASK,KSIZE,NPATCH,JPATCH)     
  END IF
END IF
!
! Unpack ISBA variables (modd_isban) for each patch:
!
 CALL UNPACK_ISBA_PATCH_n(KMASK,KSIZE,JPATCH)
!
!----------------------------------------------------------------------
!
! for further chemical biogenic emissions
!
IF (NBEQ>0 .AND. LCH_BIO_FLUX) THEN
  !
  DO JJ=1,KSIZE
    ZSW_FORBIO(KMASK(JJ),JPATCH) = 0.
  ENDDO
  !
  DO JSWB=1,ISWB
!cdir nodep
!cdir unroll=8
    DO JJ=1,KSIZE
      ZSW_FORBIO(KMASK(JJ),JPATCH) = ZSW_FORBIO(KMASK(JJ),JPATCH)              &
                                     + ZP_DIR_SW(JJ,JSWB) + ZP_SCA_SW(JJ,JSWB)  
    ENDDO
  ENDDO
  !
ENDIF
!----------------------------------------------------------------------
!
! Unpack output dummy arguments for each patch:
!
IF (NPATCH==1) THEN
   ZSFTQ_TILE      (:,JPATCH)  = ZP_SFTQ      (:)
   ZSFTH_TILE      (:,JPATCH)  = ZP_SFTH      (:)
   ZSFTS_TILE      (:,:,JPATCH)= ZP_SFTS      (:,:)
   ZSFCO2_TILE     (:,JPATCH)  = ZP_SFCO2     (:)
   ZSFU_TILE       (:,JPATCH)  = ZP_SFU       (:)
   ZSFV_TILE       (:,JPATCH)  = ZP_SFV       (:)
   ZTRAD_TILE      (:,JPATCH)  = ZP_TRAD      (:)
ELSE
!cdir nodep
!cdir unroll=8
 DO JJ=1,KSIZE
   JI = KMASK(JJ)
   ZSFTQ_TILE      (JI,JPATCH)  = ZP_SFTQ      (JJ)
   ZSFTH_TILE      (JI,JPATCH)  = ZP_SFTH      (JJ)
   ZSFCO2_TILE     (JI,JPATCH)  = ZP_SFCO2     (JJ)
   ZSFU_TILE       (JI,JPATCH)  = ZP_SFU       (JJ)
   ZSFV_TILE       (JI,JPATCH)  = ZP_SFV       (JJ)
   ZTRAD_TILE      (JI,JPATCH)  = ZP_TRAD      (JJ)
 ENDDO
!
!cdir nodep
!cdir unroll=8
  DO JK=1,SIZE(ZP_SFTS,2)
    DO JJ=1,KSIZE
      JI=KMASK(JJ)    
      ZSFTS_TILE      (JI,JK,JPATCH)= ZP_SFTS      (JJ,JK)
    ENDDO
  ENDDO
ENDIF
!
!----------------------------------------------------------------------
!
! Get output dust flux if we are calculating dust
IF (NDSTMDE .GE. 1) IMOMENT = INT(IDST / NDSTMDE)
IF (NDSTEQ>0) THEN
  DO JSV = 1,NDSTMDE
    IF (IMOMENT == 1) THEN
      XSFDST(:,JSV,JPATCH)=ZSFTS_TILE(:,NDST_MDEBEG+JSV-1,JPATCH)
    ELSE
      XSFDST(:,JSV,JPATCH)=ZSFTS_TILE(:,NDST_MDEBEG+(JSV-1)*IMOMENT+1,JPATCH)
    END IF

    XSFDSTM(:,JSV,JPATCH)=XSFDSTM(:,JSV,JPATCH) + XSFDST(:,JSV,JPATCH) * PTSTEP
  ENDDO
ENDIF
!
IF (LHOOK) CALL DR_HOOK('COUPLING_ISBA_n:TREAT_PATCH',1,ZHOOK_HANDLE)
!
END SUBROUTINE TREAT_PATCH
!==========================================================================================
END SUBROUTINE COUPLING_ISBA_n
