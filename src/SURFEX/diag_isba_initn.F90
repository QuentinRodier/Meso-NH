!     #########
      SUBROUTINE DIAG_ISBA_INIT_n(HPROGRAM,KLU,KSW)
!     #####################
!
!!****  *DIAG_ISBA_INIT_n* - routine to initialize ISBA-AGS diagnostic variables
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
!!      Original    02/2003 
!!      modified    11/2003 by P. LeMoigne: surface cumulated energy budget
!!      modified    10/2004 by P. LeMoigne: surface miscellaneous fields
!!      B. Decharme    2008    New diag for water budget and allow to reset
!                               cumulatives variables at the beginning of a run
!!      B. Decharme 06/2009    add patch budget switch 
!!      B. Decharme 08/2009    add cummulative diag
!!      A.L. Gibelin 04/2009 : Add respiration diagnostics
!!      A.L. Gibelin 05/2009 : Add carbon spinup
!!      A.L. Gibelin 07/2009 : Suppress RDK and transform GPP as a diagnostic
!!      B. Decharme  05/12   : Carbon fluxes in diag_evap
!!      B. Decharme  10/12     Isba water budget diag
!!      B. Decharme  10/12     New diag for DIF:
!!                             F2 stress
!!                             Root zone swi, wg and wgi
!!                             swi, wg and wgi comparable to ISBA-FR-DG2 and DG3 layers
!!                             active layer thickness over permafrost
!!                             frozen layer thickness over non-permafrost
!-------------------------------------------------------------------------------
!
!*       0.0    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_ISBA_n,         ONLY : NPATCH, NGROUND_LAYER, LFLOOD, CHORT, LGLACIER,  &
                                LTR_ML, TSNOW, CISBA, XABC, CPHOTO
USE MODD_CH_ISBA_n,      ONLY : LCH_BIO_FLUX, NBEQ 
USE MODD_TYPE_DATE_SURF
USE MODD_AGRI,           ONLY : LAGRIP
USE MODD_DIAG_SURF_ATM_n,ONLY : LREAD_BUDGETC
USE MODD_DIAG_ISBA_n,    ONLY : N2M, LSURF_BUDGET, LCOEF, LSURF_VARS,            &
                                LPATCH_BUDGET,                                   &
                                XRN, XH, XGFLUX, XLEI, XRI, XCD, XCH, XCE,       &
                                XTS, XTSRAD, XT2M, XQ2M, XHU2M,                  &
                                XZON10M, XMER10M,                                &
                                XZ0_WITH_SNOW, XZ0H_WITH_SNOW, XZ0EFF, XQS,      &
                                XSWD, XSWU, XSWBD, XSWBU, XLWD, XLWU, XFMU, XFMV,&
                                XAVG_RN, XAVG_H, XAVG_LE, XAVG_LEI,              &
                                XAVG_GFLUX, XAVG_RI, XAVG_CD, XAVG_CH, XAVG_CE,  &
                                XAVG_T2M, XAVG_Q2M, XAVG_HU2M, XAVG_T2M_MIN,     &
                                XAVG_ZON10M, XAVG_MER10M, XAVG_T2M_MAX,          &
                                XAVG_Z0, XAVG_Z0H, XAVG_Z0EFF, XAVG_QS,          &
                                XAVG_SWD, XAVG_SWU, XAVG_SWBD, XAVG_SWBU,        &
                                XAVG_LWD, XAVG_LWU, XAVG_FMU, XAVG_FMV,          &
                                XSWDC, XSWUC, XLWDC, XLWUC, XFMUC, XFMVC,        &
                                XAVG_SWDC, XAVG_SWUC, XAVG_LWDC, XAVG_LWUC,      &
                                XAVG_FMUC, XAVG_FMVC, XAVG_TS, XAVG_TSRAD,       &
                                XAVG_HU2M_MIN, XAVG_HU2M_MAX, XWIND10M,          &
                                XAVG_WIND10M, XAVG_WIND10M_MAX, XAVG_SFCO2  
!
USE MODD_DIAG_EVAP_ISBA_n, ONLY : LSURF_EVAP_BUDGET, LSURF_BUDGETC, LRESET_BUDGETC,&
                                  LWATER_BUDGET,                                   &
                                  XRNC, XAVG_RNC, XHC, XAVG_HC,                    &
                                  XLEC, XAVG_LEC, XGFLUXC, XAVG_GFLUXC,            &
                                  XLEIC, XAVG_LEIC,                                &
                                  XLEG, XLEGC, XAVG_LEG, XAVG_LEGC,                &
                                  XLEGI, XLEGIC, XAVG_LEGI, XAVG_LEGIC,            &
                                  XLEV, XLEVC, XAVG_LEV, XAVG_LEVC,                &
                                  XLES, XLESC, XAVG_LES, XAVG_LESC,                &
                                  XLESL, XLESLC, XAVG_LESL, XAVG_LESLC,            &
                                  XLER, XLERC, XAVG_LER, XAVG_LERC,                &
                                  XLETR, XLETRC, XAVG_LETR, XAVG_LETRC,            &
                                  XEVAP, XEVAPC, XAVG_EVAP, XAVG_EVAPC,            &
                                  XDRAIN, XDRAINC, XAVG_DRAIN, XAVG_DRAINC,        &
                                  XRUNOFF, XRUNOFFC, XAVG_RUNOFF, XAVG_RUNOFFC,    &
                                  XHORT, XHORTC, XAVG_HORT, XAVG_HORTC,            &
                                  XDRIP, XDRIPC, XAVG_DRIP, XAVG_DRIPC,            &
                                  XMELT, XMELTC, XAVG_MELT, XAVG_MELTC,            &
                                  XIFLOOD, XIFLOODC, XAVG_IFLOOD, XAVG_IFLOODC,    &
                                  XPFLOOD, XPFLOODC, XAVG_PFLOOD, XAVG_PFLOODC,    &
                                  XLE_FLOOD, XLE_FLOODC, XAVG_LE_FLOOD,            &
                                  XAVG_LE_FLOODC, XLEI_FLOOD, XLEI_FLOODC,         &
                                  XAVG_LEI_FLOOD, XAVG_LEI_FLOODC,                 &
                                  XICEFLUXC, XAVG_ICEFLUXC,                        &
                                  XRRVEG, XRRVEGC, XAVG_RRVEG, XAVG_RRVEGC,        &
                                  XIRRIG_FLUX, XIRRIG_FLUXC, XAVG_IRRIG_FLUX,      &
                                  XAVG_IRRIG_FLUXC,                                &
                                  XGPP,XGPPC,XAVG_GPP,XAVG_GPPC, XRESP_AUTO,       &
                                  XRESPC_AUTO,XAVG_RESP_AUTO,XAVG_RESPC_AUTO,      &
                                  XRESP_ECO,XRESPC_ECO,XAVG_RESP_ECO,              &
                                  XAVG_RESPC_ECO,XDWG,XDWGC,XAVG_DWG,XAVG_DWGC,    &     
                                  XDWGI,XDWGIC,XAVG_DWGI,XAVG_DWGIC,               &
                                  XDWR,XDWRC,XAVG_DWR,XAVG_DWRC,                   &
                                  XDSWE,XDSWEC,XAVG_DSWE,XAVG_DSWEC,               &
                                  XRAINFALL,XRAINFALLC,XSNOWFALL,XSNOWFALLC,       &
                                  XWATBUD,XWATBUDC,XAVG_WATBUD,XAVG_WATBUDC
! 
USE MODD_DIAG_MISC_ISBA_n, ONLY : LSURF_MISC_BUDGET, LSURF_MISC_DIF,              &
                                  XHV,  XSWI, XTSWI, XTWSNOW, XTDSNOW, XTTSNOW,   &
                                  XDPSNG, XDPSNV, XDPSN,                          &
                                  XAVG_HV, XAVG_SWI, XAVG_TSWI,                   &
                                  XALT, XFLT, XAVG_ALT, XAVG_FLT,                 &
                                  XAVG_TWSNOW, XAVG_TDSNOW, XAVG_TTSNOW,          &
                                  XAVG_PSNG, XAVG_PSNV, XAVG_PSN, XSEUIL,         &
                                  XAVG_ALBT, XALBT,                               &
                                  XSOIL_TSWI, XSOIL_TWG, XSOIL_TWGI,              &
                                  XDFFG, XDFFV, XDFF, XAVG_FFG, XAVG_FFV, XAVG_FF,&
                                  XSNOWLIQ, XSNOWTEMP, XDFSAT, XAVG_FSAT,         &
                                  XSURF_TSWI, XSURF_TWG, XSURF_TWGI,  XROOT_TSWI, &
                                  XROOT_TWG,  XROOT_TWGI, XFRD2_TSWI, XFRD2_TWG,  &
                                  XFRD2_TWGI, XFRD3_TSWI, XFRD3_TWG, XFRD3_TWGI,  &
                                  XFAPAR, XFAPIR, XDFAPARC, XDFAPIRC,             &
                                  XFAPAR_BS, XFAPIR_BS, XDLAI_EFFC, XAVG_LAI                                  
!
USE MODD_GR_BIOG_n,        ONLY : XFISO, XFMONO, XIACAN
!
USE MODI_READ_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.01   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(IN)         :: KLU       ! size of arrays
INTEGER, INTENT(IN)         :: KSW       ! spectral bands
 CHARACTER(LEN=6), INTENT(IN):: HPROGRAM  ! program calling
!
!*       0.02   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IVERSION, IBUG
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YREC           ! Name of the article to be read
 CHARACTER(LEN=4) :: YREC2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* surface energy budget
!
IF (LHOOK) CALL DR_HOOK('DIAG_ISBA_INIT_N',0,ZHOOK_HANDLE)
!
IF (LSURF_BUDGET) THEN
  ALLOCATE(XAVG_RN           (KLU))
  ALLOCATE(XAVG_H            (KLU))
  ALLOCATE(XAVG_LE           (KLU))
  ALLOCATE(XAVG_LEI          (KLU))
  ALLOCATE(XAVG_GFLUX        (KLU))
  ALLOCATE(XAVG_SWD          (KLU))
  ALLOCATE(XAVG_SWU          (KLU))
  ALLOCATE(XAVG_SWBD         (KLU,KSW))
  ALLOCATE(XAVG_SWBU         (KLU,KSW))
  ALLOCATE(XAVG_LWD          (KLU))
  ALLOCATE(XAVG_LWU          (KLU))
  ALLOCATE(XAVG_FMU          (KLU))
  ALLOCATE(XAVG_FMV          (KLU))
  !
  XAVG_RN      = XUNDEF
  XAVG_H       = XUNDEF
  XAVG_LE      = XUNDEF
  XAVG_LEI     = XUNDEF
  XAVG_GFLUX   = XUNDEF
  XAVG_SWD     = XUNDEF
  XAVG_SWU     = XUNDEF
  XAVG_SWBD    = XUNDEF
  XAVG_SWBU    = XUNDEF
  XAVG_LWD     = XUNDEF
  XAVG_LWU     = XUNDEF
  XAVG_FMU     = XUNDEF
  XAVG_FMV     = XUNDEF
  !
  ALLOCATE(XRN       (KLU,NPATCH))
  ALLOCATE(XH        (KLU,NPATCH))
  ALLOCATE(XGFLUX    (KLU,NPATCH))
  ALLOCATE(XLEI      (KLU,NPATCH))
  ALLOCATE(XSWD      (KLU,NPATCH))
  ALLOCATE(XSWU      (KLU,NPATCH))
  ALLOCATE(XSWBD     (KLU,KSW,NPATCH))
  ALLOCATE(XSWBU     (KLU,KSW,NPATCH))
  ALLOCATE(XLWD      (KLU,NPATCH))
  ALLOCATE(XLWU      (KLU,NPATCH))
  ALLOCATE(XFMU      (KLU,NPATCH))
  ALLOCATE(XFMV      (KLU,NPATCH))
  !
  XRN      = XUNDEF
  XH       = XUNDEF
  XGFLUX   = XUNDEF
  XLEI     = XUNDEF
  XSWD     = XUNDEF
  XSWU     = XUNDEF
  XSWBD    = XUNDEF
  XSWBU    = XUNDEF
  XLWD     = XUNDEF
  XLWU     = XUNDEF
  XFMU     = XUNDEF
  XFMV     = XUNDEF
  !
ELSE
  ALLOCATE(XAVG_RN           (0))
  ALLOCATE(XAVG_H            (0))
  ALLOCATE(XAVG_LE           (0))
  ALLOCATE(XAVG_LEI          (0))
  ALLOCATE(XAVG_GFLUX        (0))
  ALLOCATE(XAVG_SWD          (0))
  ALLOCATE(XAVG_SWU          (0))
  ALLOCATE(XAVG_SWBD         (0,0))
  ALLOCATE(XAVG_SWBU         (0,0))
  ALLOCATE(XAVG_LWD          (0))
  ALLOCATE(XAVG_LWU          (0))
  ALLOCATE(XAVG_FMU          (0))
  ALLOCATE(XAVG_FMV          (0))
!
  ALLOCATE(XRN       (0,0))
  ALLOCATE(XH        (0,0))
  ALLOCATE(XGFLUX    (0,0))
  ALLOCATE(XLEI      (0,0))
  ALLOCATE(XSWD      (0,0))
  ALLOCATE(XSWU      (0,0))
  ALLOCATE(XSWBD     (0,0,0))
  ALLOCATE(XSWBU     (0,0,0))
  ALLOCATE(XLWD      (0,0))
  ALLOCATE(XLWU      (0,0))
  ALLOCATE(XFMU      (0,0))
  ALLOCATE(XFMV      (0,0))
END IF
!
!* detailed surface energy budget
!
IF (LSURF_EVAP_BUDGET) THEN
  ALLOCATE(XAVG_LEG       (KLU))
  ALLOCATE(XAVG_LEGI      (KLU))
  ALLOCATE(XAVG_LEV       (KLU))
  ALLOCATE(XAVG_LES       (KLU))
  ALLOCATE(XAVG_LESL      (KLU))
  ALLOCATE(XAVG_LER       (KLU))
  ALLOCATE(XAVG_LETR      (KLU))
  ALLOCATE(XAVG_EVAP      (KLU))
  ALLOCATE(XAVG_DRAIN     (KLU))
  ALLOCATE(XAVG_RUNOFF    (KLU))
  ALLOCATE(XAVG_HORT      (KLU))
  ALLOCATE(XAVG_DRIP      (KLU))
  ALLOCATE(XAVG_RRVEG     (KLU))
  ALLOCATE(XAVG_MELT      (KLU))
  ALLOCATE(XAVG_IRRIG_FLUX(KLU))
  ALLOCATE(XAVG_GPP       (KLU))
  ALLOCATE(XAVG_RESP_AUTO (KLU))
  ALLOCATE(XAVG_RESP_ECO  (KLU))   
  !
  XAVG_LEG        = XUNDEF
  XAVG_LEGI       = XUNDEF
  XAVG_LEV        = XUNDEF
  XAVG_LES        = XUNDEF
  XAVG_LESL       = XUNDEF
  XAVG_LER        = XUNDEF
  XAVG_LETR       = XUNDEF
  XAVG_EVAP       = XUNDEF
  XAVG_DRAIN      = XUNDEF
  XAVG_RUNOFF     = XUNDEF
  XAVG_HORT       = XUNDEF
  XAVG_DRIP       = XUNDEF
  XAVG_RRVEG      = XUNDEF
  XAVG_MELT       = XUNDEF
  XAVG_IRRIG_FLUX = XUNDEF
  XAVG_GPP        = XUNDEF
  XAVG_RESP_AUTO  = XUNDEF
  XAVG_RESP_ECO   = XUNDEF  
  !
  ALLOCATE(XAVG_IFLOOD   (KLU))
  ALLOCATE(XAVG_PFLOOD   (KLU))
  ALLOCATE(XAVG_LE_FLOOD (KLU))
  ALLOCATE(XAVG_LEI_FLOOD(KLU))
  XAVG_IFLOOD    = XUNDEF
  XAVG_PFLOOD    = XUNDEF
  XAVG_LE_FLOOD  = XUNDEF
  XAVG_LEI_FLOOD = XUNDEF
  !
  ALLOCATE(XLEG       (KLU,NPATCH))
  ALLOCATE(XLEGI      (KLU,NPATCH))
  ALLOCATE(XLEV       (KLU,NPATCH))
  ALLOCATE(XLES       (KLU,NPATCH))
  ALLOCATE(XLESL      (KLU,NPATCH))
  ALLOCATE(XLER       (KLU,NPATCH))
  ALLOCATE(XLETR      (KLU,NPATCH))
  ALLOCATE(XEVAP      (KLU,NPATCH))
  ALLOCATE(XDRAIN     (KLU,NPATCH))
  ALLOCATE(XRUNOFF    (KLU,NPATCH))
  ALLOCATE(XHORT      (KLU,NPATCH))
  ALLOCATE(XDRIP      (KLU,NPATCH))
  ALLOCATE(XRRVEG     (KLU,NPATCH))
  ALLOCATE(XMELT      (KLU,NPATCH))
  ALLOCATE(XIRRIG_FLUX(KLU,NPATCH))
  ALLOCATE(XGPP       (KLU,NPATCH))
  ALLOCATE(XRESP_AUTO (KLU,NPATCH))
  ALLOCATE(XRESP_ECO  (KLU,NPATCH))
  !
  XLEG           = XUNDEF 
  XLEGI          = XUNDEF
  XLEV           = XUNDEF
  XLES           = XUNDEF
  XLESL          = XUNDEF
  XLER           = XUNDEF
  XLETR          = XUNDEF
  XEVAP          = XUNDEF
  XDRAIN         = XUNDEF
  XRUNOFF        = XUNDEF
  XHORT          = XUNDEF
  XDRIP          = XUNDEF
  XRRVEG         = XUNDEF
  XMELT          = XUNDEF
  XIRRIG_FLUX    = XUNDEF
  XGPP           = XUNDEF
  XRESP_AUTO     = XUNDEF
  XRESP_ECO      = XUNDEF  
  !
  ALLOCATE(XIFLOOD (KLU,NPATCH))
  ALLOCATE(XPFLOOD (KLU,NPATCH))
  ALLOCATE(XLE_FLOOD(KLU,NPATCH))
  ALLOCATE(XLEI_FLOOD(KLU,NPATCH))
  XIFLOOD        = XUNDEF
  XPFLOOD        = XUNDEF
  XLE_FLOOD      = XUNDEF
  XLEI_FLOOD     = XUNDEF
  !
  IF(LWATER_BUDGET)THEN
    !      
    ALLOCATE(XRAINFALL  (KLU))
    ALLOCATE(XSNOWFALL  (KLU))
    ALLOCATE(XAVG_DWG   (KLU))
    ALLOCATE(XAVG_DWGI  (KLU))
    ALLOCATE(XAVG_DWR   (KLU))
    ALLOCATE(XAVG_DSWE  (KLU))
    ALLOCATE(XAVG_WATBUD(KLU))
    XRAINFALL   = XUNDEF
    XSNOWFALL   = XUNDEF
    XAVG_DWG    = XUNDEF
    XAVG_DWGI   = XUNDEF
    XAVG_DWR    = XUNDEF
    XAVG_DSWE   = XUNDEF
    XAVG_WATBUD = XUNDEF
    !
    ALLOCATE(XDWG   (KLU,NPATCH))
    ALLOCATE(XDWGI  (KLU,NPATCH))
    ALLOCATE(XDWR   (KLU,NPATCH))
    ALLOCATE(XDSWE  (KLU,NPATCH))
    ALLOCATE(XWATBUD(KLU,NPATCH))
    XDWG    = XUNDEF
    XDWGI   = XUNDEF
    XDWR    = XUNDEF
    XDSWE   = XUNDEF
    XWATBUD = XUNDEF
    ! 
  ELSE
    !
    ALLOCATE(XRAINFALL  (0))
    ALLOCATE(XSNOWFALL  (0))
    ALLOCATE(XAVG_DWG   (0))
    ALLOCATE(XAVG_DWGI  (0))
    ALLOCATE(XAVG_DWR   (0))
    ALLOCATE(XAVG_DSWE  (0))
    ALLOCATE(XAVG_WATBUD(0))
    !
    ALLOCATE(XDWG   (0,0))
    ALLOCATE(XDWGI  (0,0))
    ALLOCATE(XDWR   (0,0))
    ALLOCATE(XDSWE  (0,0))
    ALLOCATE(XWATBUD(0,0))
    !
  ENDIF
  !
ELSE
  ALLOCATE(XAVG_LEG       (0))
  ALLOCATE(XAVG_LEGI      (0))
  ALLOCATE(XAVG_LEV       (0))
  ALLOCATE(XAVG_LES       (0))
  ALLOCATE(XAVG_LESL      (0))
  ALLOCATE(XAVG_LER       (0))
  ALLOCATE(XAVG_LETR      (0))
  ALLOCATE(XAVG_EVAP      (0))
  ALLOCATE(XAVG_DRAIN     (0))
  ALLOCATE(XAVG_RUNOFF    (0))
  ALLOCATE(XAVG_HORT      (0))
  ALLOCATE(XAVG_DRIP      (0))
  ALLOCATE(XAVG_RRVEG     (0))
  ALLOCATE(XAVG_MELT      (0))
  ALLOCATE(XAVG_IRRIG_FLUX(0))
  ALLOCATE(XAVG_GPP       (0))
  ALLOCATE(XAVG_RESP_AUTO (0))
  ALLOCATE(XAVG_RESP_ECO  (0))
  ALLOCATE(XAVG_IFLOOD    (0))
  ALLOCATE(XAVG_PFLOOD    (0))
  ALLOCATE(XAVG_LE_FLOOD  (0))
  ALLOCATE(XAVG_LEI_FLOOD (0))
!
  ALLOCATE(XLEG       (0,0))
  ALLOCATE(XLEGI      (0,0))
  ALLOCATE(XLEV       (0,0))
  ALLOCATE(XLES       (0,0))
  ALLOCATE(XLESL      (0,0))
  ALLOCATE(XLER       (0,0))
  ALLOCATE(XLETR      (0,0))
  ALLOCATE(XEVAP      (0,0))
  ALLOCATE(XDRAIN     (0,0))
  ALLOCATE(XRUNOFF    (0,0))
  ALLOCATE(XHORT      (0,0))
  ALLOCATE(XDRIP      (0,0))
  ALLOCATE(XRRVEG     (0,0))
  ALLOCATE(XMELT      (0,0))
  ALLOCATE(XIRRIG_FLUX(0,0))
  ALLOCATE(XGPP       (0,0))
  ALLOCATE(XRESP_AUTO (0,0))
  ALLOCATE(XRESP_ECO  (0,0))  
  ALLOCATE(XIFLOOD    (0,0))
  ALLOCATE(XPFLOOD    (0,0))
  ALLOCATE(XLE_FLOOD  (0,0))
  ALLOCATE(XLEI_FLOOD (0,0))
  !
  ALLOCATE(XRAINFALL  (0))
  ALLOCATE(XSNOWFALL  (0))
  ALLOCATE(XAVG_DWG   (0))
  ALLOCATE(XAVG_DWGI  (0))
  ALLOCATE(XAVG_DWR   (0))
  ALLOCATE(XAVG_DSWE  (0))
  ALLOCATE(XAVG_WATBUD(0))
  !
  ALLOCATE(XDWG   (0,0))
  ALLOCATE(XDWGI  (0,0))
  ALLOCATE(XDWR   (0,0))
  ALLOCATE(XDSWE  (0,0))
  ALLOCATE(XWATBUD(0,0))
  ! 
END IF
!
!* surface cumulated energy budget
!
IF (LSURF_BUDGETC) THEN
  ALLOCATE(XAVG_RNC        (KLU))
  ALLOCATE(XAVG_HC         (KLU))
  ALLOCATE(XAVG_LEC        (KLU))
  ALLOCATE(XAVG_LEIC       (KLU))
  ALLOCATE(XAVG_GFLUXC     (KLU))
  ALLOCATE(XAVG_LEGC       (KLU))
  ALLOCATE(XAVG_LEGIC      (KLU))
  ALLOCATE(XAVG_LEVC       (KLU))
  ALLOCATE(XAVG_LESC       (KLU))
  ALLOCATE(XAVG_LESLC      (KLU))
  ALLOCATE(XAVG_LERC       (KLU))
  ALLOCATE(XAVG_LETRC      (KLU))
  ALLOCATE(XAVG_EVAPC      (KLU))
  ALLOCATE(XAVG_DRAINC     (KLU))
  ALLOCATE(XAVG_RUNOFFC    (KLU))
  ALLOCATE(XAVG_HORTC      (KLU))
  ALLOCATE(XAVG_DRIPC      (KLU))
  ALLOCATE(XAVG_RRVEGC     (KLU))
  ALLOCATE(XAVG_MELTC      (KLU))
  ALLOCATE(XAVG_IRRIG_FLUXC(KLU))
  ALLOCATE(XAVG_GPPC       (KLU))
  ALLOCATE(XAVG_RESPC_AUTO (KLU))
  ALLOCATE(XAVG_RESPC_ECO  (KLU))
  ALLOCATE(XAVG_IFLOODC    (KLU))
  ALLOCATE(XAVG_PFLOODC    (KLU))
  ALLOCATE(XAVG_LE_FLOODC  (KLU))
  ALLOCATE(XAVG_LEI_FLOODC (KLU))  
  !
  ALLOCATE(XRNC        (KLU,NPATCH))
  ALLOCATE(XHC         (KLU,NPATCH))
  ALLOCATE(XLEC        (KLU,NPATCH))
  ALLOCATE(XLEIC       (KLU,NPATCH))
  ALLOCATE(XGFLUXC     (KLU,NPATCH))
  ALLOCATE(XLEGC       (KLU,NPATCH))
  ALLOCATE(XLEGIC      (KLU,NPATCH))
  ALLOCATE(XLEVC       (KLU,NPATCH))
  ALLOCATE(XLESC       (KLU,NPATCH))
  ALLOCATE(XLESLC      (KLU,NPATCH))
  ALLOCATE(XLERC       (KLU,NPATCH))
  ALLOCATE(XLETRC      (KLU,NPATCH))
  ALLOCATE(XEVAPC      (KLU,NPATCH))
  ALLOCATE(XDRAINC     (KLU,NPATCH))
  ALLOCATE(XRUNOFFC    (KLU,NPATCH))
  ALLOCATE(XHORTC      (KLU,NPATCH))
  ALLOCATE(XDRIPC      (KLU,NPATCH))
  ALLOCATE(XRRVEGC     (KLU,NPATCH))
  ALLOCATE(XMELTC      (KLU,NPATCH))
  ALLOCATE(XIRRIG_FLUXC(KLU,NPATCH))
  ALLOCATE(XGPPC       (KLU,NPATCH))
  ALLOCATE(XRESPC_AUTO (KLU,NPATCH))
  ALLOCATE(XRESPC_ECO  (KLU,NPATCH))
  ALLOCATE(XIFLOODC    (KLU,NPATCH))
  ALLOCATE(XPFLOODC    (KLU,NPATCH))
  ALLOCATE(XLE_FLOODC  (KLU,NPATCH))
  ALLOCATE(XLEI_FLOODC (KLU,NPATCH))
  !
  ALLOCATE(XAVG_SWDC        (KLU))
  ALLOCATE(XAVG_SWUC        (KLU))
  ALLOCATE(XAVG_LWDC        (KLU))
  ALLOCATE(XAVG_LWUC        (KLU))
  ALLOCATE(XAVG_FMUC        (KLU))
  ALLOCATE(XAVG_FMVC        (KLU))
  ALLOCATE(XSWDC     (KLU,NPATCH))
  ALLOCATE(XSWUC     (KLU,NPATCH))
  ALLOCATE(XLWDC     (KLU,NPATCH))
  ALLOCATE(XLWUC     (KLU,NPATCH))
  ALLOCATE(XFMUC     (KLU,NPATCH))
  ALLOCATE(XFMVC     (KLU,NPATCH))
  !
  IF(LGLACIER)THEN
    ALLOCATE(XAVG_ICEFLUXC(KLU))
    ALLOCATE(XICEFLUXC(KLU,NPATCH))
  ENDIF
  !
  IF(LWATER_BUDGET)THEN
    !      
    ALLOCATE(XRAINFALLC  (KLU))
    ALLOCATE(XSNOWFALLC  (KLU))
    ALLOCATE(XAVG_DWGC   (KLU))
    ALLOCATE(XAVG_DWGIC  (KLU))
    ALLOCATE(XAVG_DWRC   (KLU))
    ALLOCATE(XAVG_DSWEC  (KLU))
    ALLOCATE(XAVG_WATBUDC(KLU))
    !
    ALLOCATE(XDWGC   (KLU,NPATCH))
    ALLOCATE(XDWGIC  (KLU,NPATCH))
    ALLOCATE(XDWRC   (KLU,NPATCH))
    ALLOCATE(XDSWEC  (KLU,NPATCH))
    ALLOCATE(XWATBUDC(KLU,NPATCH))
    !
  ELSE
    !      
    ALLOCATE(XRAINFALLC  (0))
    ALLOCATE(XSNOWFALLC  (0))
    ALLOCATE(XAVG_DWGC   (0))
    ALLOCATE(XAVG_DWGIC  (0))
    ALLOCATE(XAVG_DWRC   (0))
    ALLOCATE(XAVG_DSWEC  (0))
    ALLOCATE(XAVG_WATBUDC(0))
    !
    ALLOCATE(XDWGC   (0,0))
    ALLOCATE(XDWGIC  (0,0))
    ALLOCATE(XDWRC   (0,0))
    ALLOCATE(XDSWEC  (0,0))
    ALLOCATE(XWATBUDC(0,0))
    !          
  ENDIF
  !
  IF (.NOT.LREAD_BUDGETC) THEN
      XAVG_RNC         = 0.0
      XAVG_HC          = 0.0
      XAVG_LEC         = 0.0
      XAVG_LEIC        = 0.0
      XAVG_GFLUXC      = 0.0
      XAVG_LEGC        = 0.0
      XAVG_LEGIC       = 0.0
      XAVG_LEVC        = 0.0
      XAVG_LESC        = 0.0
      XAVG_LESLC       = 0.0
      XAVG_LERC        = 0.0
      XAVG_LETRC       = 0.0
      XAVG_EVAPC       = 0.0
      XAVG_DRAINC      = 0.0
      XAVG_RUNOFFC     = 0.0
      XAVG_HORTC       = 0.0
      XAVG_DRIPC       = 0.0
      XAVG_RRVEGC      = 0.0
      XAVG_MELTC       = 0.0
      XAVG_IRRIG_FLUXC = 0.0
      XAVG_GPPC        = 0.0
      XAVG_RESPC_AUTO  = 0.0
      XAVG_RESPC_ECO   = 0.0  
      XAVG_IFLOODC     = 0.0
      XAVG_PFLOODC     = 0.0
      XAVG_LE_FLOODC   = 0.0
      XAVG_LEI_FLOODC  = 0.0      
      !
      XRNC         = 0.0
      XHC          = 0.0
      XLEC         = 0.0
      XLEIC        = 0.0
      XGFLUXC      = 0.0
      XLEGC        = 0.0 
      XLEGIC       = 0.0
      XLEVC        = 0.0
      XLESC        = 0.0
      XLESLC       = 0.0
      XLERC        = 0.0
      XLETRC       = 0.0
      XEVAPC       = 0.0
      XDRAINC      = 0.0
      XRUNOFFC     = 0.0
      XHORTC       = 0.0
      XDRIPC       = 0.0
      XRRVEGC      = 0.0
      XMELTC       = 0.0
      XIRRIG_FLUXC = 0.0
      XGPPC        = 0.0
      XRESPC_AUTO  = 0.0
      XRESPC_ECO   = 0.0    
      XIFLOODC     = 0.0
      XPFLOODC     = 0.0
      XLE_FLOODC   = 0.0
      XLEI_FLOODC  = 0.0
      !
      XAVG_SWDC = 0.0
      XAVG_SWUC = 0.0
      XAVG_LWDC = 0.0
      XAVG_LWUC = 0.0
      XAVG_FMUC = 0.0
      XAVG_FMVC = 0.0
      XSWDC     = 0.0
      XSWUC     = 0.0
      XLWDC     = 0.0
      XLWUC     = 0.0
      XFMUC     = 0.0
      XFMVC     = 0.0
      !
      IF(LGLACIER)THEN
         XAVG_ICEFLUXC = 0.0
         XICEFLUXC     = 0.0
      ENDIF
      !
      IF(LWATER_BUDGET)THEN
        !      
        XRAINFALLC   = 0.0
        XSNOWFALLC   = 0.0
        XAVG_DWGC    = 0.0
        XAVG_DWGIC   = 0.0
        XAVG_DWRC    = 0.0
        XAVG_DSWEC   = 0.0
        XAVG_WATBUDC = 0.0
        !
        XDWGC    = 0.0
        XDWGIC   = 0.0
        XDWRC    = 0.0
        XDSWEC   = 0.0
        XWATBUDC = 0.0
        !
      ENDIF
      !     
  ELSEIF (LREAD_BUDGETC.AND.LRESET_BUDGETC) THEN
      !
      XAVG_RNC         = 0.0
      XAVG_HC          = 0.0
      XAVG_LEC         = 0.0
      XAVG_LEIC        = 0.0
      XAVG_GFLUXC      = 0.0
      XAVG_LEGC        = 0.0
      XAVG_LEGIC       = 0.0
      XAVG_LEVC        = 0.0
      XAVG_LESC        = 0.0
      XAVG_LESLC       = 0.0
      XAVG_LERC        = 0.0
      XAVG_LETRC       = 0.0
      XAVG_EVAPC       = 0.0
      XAVG_DRAINC      = 0.0
      XAVG_RUNOFFC     = 0.0
      XAVG_HORTC       = 0.0
      XAVG_DRIPC       = 0.0
      XAVG_RRVEGC      = 0.0
      XAVG_MELTC       = 0.0
      XAVG_IRRIG_FLUXC = 0.0
      XAVG_GPPC        = 0.0
      XAVG_RESPC_AUTO  = 0.0
      XAVG_RESPC_ECO   = 0.0    
      XAVG_IFLOODC     = 0.0
      XAVG_PFLOODC     = 0.0
      XAVG_LE_FLOODC   = 0.0
      XAVG_LEI_FLOODC  = 0.0      
      !
      XRNC         = 0.0
      XHC          = 0.0
      XLEC         = 0.0
      XLEIC        = 0.0
      XGFLUXC      = 0.0
      XLEGC        = 0.0 
      XLEGIC       = 0.0
      XLEVC        = 0.0
      XLESC        = 0.0
      XLESLC       = 0.0
      XLERC        = 0.0
      XLETRC       = 0.0
      XEVAPC       = 0.0
      XDRAINC      = 0.0
      XRUNOFFC     = 0.0
      XHORTC       = 0.0
      XDRIPC       = 0.0
      XRRVEGC      = 0.0
      XMELTC       = 0.0
      XIRRIG_FLUXC = 0.0
      XGPPC        = 0.0
      XRESPC_AUTO  = 0.0
      XRESPC_ECO   = 0.0   
      XIFLOODC     = 0.0
      XPFLOODC     = 0.0
      XLE_FLOODC   = 0.0
      XLEI_FLOODC  = 0.0
      !
      XAVG_SWDC = 0.0
      XAVG_SWUC = 0.0
      XAVG_LWDC = 0.0
      XAVG_LWUC = 0.0
      XAVG_FMUC = 0.0
      XAVG_FMVC = 0.0
      XSWDC     = 0.0
      XSWUC     = 0.0
      XLWDC     = 0.0
      XLWUC     = 0.0
      XFMUC     = 0.0
      XFMVC     = 0.0      
      !
      IF(LGLACIER)THEN
         XAVG_ICEFLUXC = 0.0
         XICEFLUXC     = 0.0
      ENDIF
      !
      IF(LWATER_BUDGET)THEN
        !      
        XRAINFALLC   = 0.0
        XSNOWFALLC   = 0.0
        XAVG_DWGC    = 0.0
        XAVG_DWGIC   = 0.0
        XAVG_DWRC    = 0.0
        XAVG_DSWEC   = 0.0
        XAVG_WATBUDC = 0.0
        !
        XDWGC    = 0.0
        XDWGIC   = 0.0
        XDWRC    = 0.0
        XDSWEC   = 0.0
        XWATBUDC = 0.0
        !
      ENDIF
      !
  ELSE
    !      
    CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
    CALL READ_SURF(HPROGRAM,'BUG ',IBUG,IRESP)
    !
    YREC='RNC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_RNC    ,IRESP)
    YREC='HC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_HC     ,IRESP)
    YREC='LEC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_LEC    ,IRESP)
    YREC='LEIC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_LEIC   ,IRESP)      
    YREC='GFLUXC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_GFLUXC ,IRESP)
    YREC='LEGC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_LEGC   ,IRESP)
    YREC='LEGIC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_LEGIC  ,IRESP)
    YREC='LEVC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_LEVC   ,IRESP)
    YREC='LESC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_LESC   ,IRESP)
    IF( (TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') .AND. &
        (IVERSION>7 .OR. IVERSION==7 .AND. IBUG>=3) ) THEN  
      YREC='LESLC_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XAVG_LESLC  ,IRESP)      
    ELSE
      XAVG_LESLC = 0.0
    ENDIF
    YREC='LERC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_LERC   ,IRESP)
    YREC='LETRC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_LETRC  ,IRESP)
    YREC='EVAPC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_EVAPC  ,IRESP)
    YREC='DRAINC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_DRAINC ,IRESP)
    YREC='RUNOFFC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_RUNOFFC,IRESP)
    YREC='DRIVEGC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_DRIPC  ,IRESP)
    YREC='RRVEGC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_RRVEGC ,IRESP)
    YREC='SNOMLTC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_MELTC,IRESP)
    IF (LAGRIP) THEN
      YREC='IRRIGC_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XAVG_IRRIG_FLUXC,IRESP)    
    ELSE
      XAVG_IRRIG_FLUXC = 0.0
    ENDIF
    !
    IF(CPHOTO/='NON' .AND. (IVERSION>7 .OR. IVERSION==7 .AND. IBUG>=3))THEN
      YREC='GPPC_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XAVG_GPPC,IRESP)
      YREC='RC_AUTO_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XAVG_RESPC_AUTO,IRESP)
      YREC='RC_ECO_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XAVG_RESPC_ECO,IRESP)        
    ELSE
      XAVG_GPPC = 0.0
      XAVG_RESPC_AUTO = 0.0
      XAVG_RESPC_ECO = 0.0
    ENDIF
    !
    IF(CHORT=='SGH'.OR.CISBA=='DIF')THEN
      YREC='HORTONC_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XAVG_HORTC,IRESP)
    ELSE
      XAVG_HORTC = 0.0
    ENDIF
    !
    IF(LFLOOD)THEN
      YREC='IFLOODC_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XAVG_IFLOODC,IRESP)
      YREC='PFLOODC_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XAVG_PFLOODC,IRESP)
      YREC='LEFC_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XAVG_LE_FLOODC,IRESP)
      YREC='LEIFC_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XAVG_LEI_FLOODC,IRESP)
    ELSE
      XAVG_IFLOODC   = 0.0
      XAVG_PFLOODC   = 0.0
      XAVG_LE_FLOODC = 0.0
      XAVG_LEI_FLOODC = 0.0
    ENDIF
!      
    YREC='SWDC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_SWDC,IRESP)
    YREC='SWUC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_SWUC,IRESP)
    YREC='LWDC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_LWDC,IRESP)
    YREC='LWUC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_LWUC,IRESP)
    YREC='FMUC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_FMUC,IRESP)
    YREC='FMVC_ISBA'
    CALL READ_SURF(HPROGRAM,YREC,XAVG_FMVC,IRESP)      
    !
    IF(LGLACIER)THEN
      YREC='ICE_FC_ISBA'         
      CALL READ_SURF(HPROGRAM,YREC,XAVG_ICEFLUXC,IRESP)      
    ENDIF
    !  
    IF(LWATER_BUDGET .AND. (IVERSION>7 .OR. IVERSION==7 .AND. IBUG>=3))THEN 
      YREC='RAINFC_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XRAINFALLC,IRESP)
      YREC='SNOWFC_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XSNOWFALLC,IRESP)
      YREC='DWGC_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XAVG_DWGC,IRESP)
      YREC='DWGIC_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XAVG_DWGIC,IRESP)
      YREC='DWRC_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XAVG_DWRC,IRESP)
      YREC='DSWEC_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XAVG_DSWEC,IRESP)
      YREC='WATBUDC_ISBA'
      CALL READ_SURF(HPROGRAM,YREC,XAVG_WATBUDC,IRESP)
    ELSE
      XRAINFALLC = 0.0
      XSNOWFALLC = 0.0
      XAVG_DWGC = 0.0
      XAVG_DWGIC = 0.0
      XAVG_DWRC = 0.0
      XAVG_DSWEC = 0.0
      XAVG_WATBUDC = 0.0
    ENDIF
    !
    IF(LPATCH_BUDGET .AND. NPATCH>1)THEN
      !
      CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
      CALL READ_SURF(HPROGRAM,'BUG ',IBUG,IRESP)
      YREC2=''
      IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUG<3) YREC2='ATCH'
      YREC='RNC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XRNC    ,IRESP)
      YREC='HC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XHC     ,IRESP)
      YREC='LEC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XLEC    ,IRESP)
      YREC='LEIC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XLEIC   ,IRESP)        
      YREC='GFLUXC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XGFLUXC ,IRESP)
      YREC='LEGC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XLEGC   ,IRESP)
      YREC='LEGIC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XLEGIC  ,IRESP)
      YREC='LEVC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XLEVC   ,IRESP)
      YREC='LESC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XLESC   ,IRESP)
      IF((TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') .AND. &
          (IVERSION>7 .OR. IVERSION==7 .AND. IBUG>=3))THEN  
        YREC='LESLC_P'
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XLESLC,IRESP)        
      ELSE
        XLESLC = 0.0
      ENDIF
      YREC='LERC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XLERC   ,IRESP)
      YREC='LETRC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XLETRC  ,IRESP)
      YREC='EVAPC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XEVAPC  ,IRESP)
      YREC='DRAINC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XDRAINC ,IRESP)
      YREC='RUNOFFC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XRUNOFFC,IRESP)
      YREC='DRIVEGC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XDRIPC,IRESP)
      YREC='RRVEGC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XRRVEGC,IRESP)
      YREC='SNOMLTC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XMELTC,IRESP)
      IF (LAGRIP) THEN
        YREC='IRRIGC_P'
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XIRRIG_FLUXC,IRESP)
      ELSE
        XIRRIG_FLUXC = 0.0
      ENDIF
      !
      IF(CPHOTO/='NON' .AND. (IVERSION>7 .OR. IVERSION==7 .AND. IBUG>=3))THEN
        YREC='GPPC_P'
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XGPPC,IRESP)
        YREC='RC_AUTO_P'
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XRESPC_AUTO,IRESP)
        YREC='RC_ECO_P'
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XRESPC_ECO,IRESP)  
      ELSE
        XGPPC      =0.0
        XRESPC_AUTO=0.0
        XRESPC_ECO =0.0
      ENDIF
      !
      YREC='SWDC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XSWDC,IRESP)
      YREC='SWUC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XSWUC,IRESP)
      YREC='LWDC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XLWDC,IRESP)
      YREC='LWUC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XLWUC,IRESP)
      YREC='FMUC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XFMUC,IRESP)
      YREC='FMVC_P'
      CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XFMVC,IRESP)
      !
      IF(LGLACIER)THEN
        YREC='ICE_FC_P'         
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XICEFLUXC,IRESP)      
      ENDIF
      !
      IF(CHORT=='SGH'.OR.CISBA=='DIF')THEN
        YREC='HORTONC_P'
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XHORTC    ,IRESP)
      ELSE
        XHORTC     = 0.0
      ENDIF
      !
      IF(LFLOOD)THEN
        YREC='IFLOODC_P'
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XIFLOODC,IRESP)
        YREC='PFLOODC_P'
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XPFLOODC,IRESP)
        YREC='LEFC_P'
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XLE_FLOODC,IRESP)
        YREC='LEIFC_P'
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XLEI_FLOODC,IRESP)
      ELSE
        XIFLOODC       = 0.0
        XPFLOODC       = 0.0
        XLE_FLOODC     = 0.0
        XLEI_FLOODC    = 0.0
      ENDIF
      !  
      IF(LWATER_BUDGET .AND. (IVERSION>7 .OR. IVERSION==7 .AND. IBUG>=3))THEN 
        YREC='DWGC_P'
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XDWGC,IRESP)
        YREC='DWGIC_P'
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XDWGIC,IRESP)
        YREC='DWRC_P'
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XDWRC,IRESP)
        YREC='DSWEC_P'
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XDSWEC,IRESP)
        YREC='WATBUDC_P'
        CALL READ_SURF(HPROGRAM,TRIM(YREC)//YREC2,XWATBUDC,IRESP)
      ELSE
        XDWGC = 0.0
        XDWGIC = 0.0
        XDWRC = 0.0
        XDSWEC = 0.0
        XWATBUDC = 0.0
      ENDIF
      !
    ELSE
      !
      XRNC         = 0.0
      XHC          = 0.0
      XLEC         = 0.0
      XLEIC        = 0.0
      XGFLUXC      = 0.0
      XLEGC        = 0.0 
      XLEGIC       = 0.0
      XLEVC        = 0.0
      XLESC        = 0.0
      XLESLC       = 0.0
      XLERC        = 0.0
      XLETRC       = 0.0
      XEVAPC       = 0.0
      XDRAINC      = 0.0
      XRUNOFFC     = 0.0
      XHORTC       = 0.0
      XDRIPC       = 0.0
      XRRVEGC      = 0.0
      XMELTC       = 0.0
      XIRRIG_FLUXC = 0.0
      XGPPC        = 0.0
      XRESPC_AUTO  = 0.0
      XRESPC_ECO   = 0.0   
      XIFLOODC     = 0.0
      XPFLOODC     = 0.0
      XLE_FLOODC   = 0.0
      XLEI_FLOODC  = 0.0
      !
      XSWDC     = 0.0
      XSWUC     = 0.0
      XLWDC     = 0.0
      XLWUC     = 0.0
      XFMUC     = 0.0
      XFMVC     = 0.0
      !
      IF(LGLACIER)THEN
        XICEFLUXC=0.0      
      ENDIF
      !   
      IF(LWATER_BUDGET)THEN
        XDWGC    = 0.0
        XDWGIC   = 0.0
        XDWRC    = 0.0
        XDSWEC   = 0.0
        XWATBUDC = 0.0
      ENDIF
      !   
    ENDIF
    !
  ENDIF
ELSE
  ALLOCATE(XAVG_RNC        (0))
  ALLOCATE(XAVG_HC         (0))
  ALLOCATE(XAVG_LEC        (0))
  ALLOCATE(XAVG_LEIC       (0))
  ALLOCATE(XAVG_GFLUXC     (0))
  ALLOCATE(XAVG_LEGC       (0))
  ALLOCATE(XAVG_LEGIC      (0))
  ALLOCATE(XAVG_LEVC       (0))
  ALLOCATE(XAVG_LESC       (0))
  ALLOCATE(XAVG_LESLC      (0))
  ALLOCATE(XAVG_LERC       (0))
  ALLOCATE(XAVG_LETRC      (0))
  ALLOCATE(XAVG_EVAPC      (0))
  ALLOCATE(XAVG_DRAINC     (0))
  ALLOCATE(XAVG_RUNOFFC    (0))
  ALLOCATE(XAVG_HORTC      (0))
  ALLOCATE(XAVG_DRIPC      (0))
  ALLOCATE(XAVG_RRVEGC     (0))
  ALLOCATE(XAVG_MELTC      (0))
  ALLOCATE(XAVG_IRRIG_FLUXC(0))
  ALLOCATE(XAVG_GPPC       (0))
  ALLOCATE(XAVG_RESPC_AUTO (0))
  ALLOCATE(XAVG_RESPC_ECO  (0)) 
  ALLOCATE(XAVG_IFLOODC    (0))
  ALLOCATE(XAVG_PFLOODC    (0))
  ALLOCATE(XAVG_LE_FLOODC  (0))
  ALLOCATE(XAVG_LEI_FLOODC (0))
  
!
  ALLOCATE(XRNC        (0,0))
  ALLOCATE(XHC         (0,0))
  ALLOCATE(XLEC        (0,0))
  ALLOCATE(XLEIC       (0,0))
  ALLOCATE(XGFLUXC     (0,0))
  ALLOCATE(XLEGC       (0,0))
  ALLOCATE(XLEGIC      (0,0))
  ALLOCATE(XLEVC       (0,0))
  ALLOCATE(XLESC       (0,0))
  ALLOCATE(XLESLC      (0,0))
  ALLOCATE(XLERC       (0,0))
  ALLOCATE(XLETRC      (0,0))
  ALLOCATE(XEVAPC      (0,0))
  ALLOCATE(XDRAINC     (0,0))
  ALLOCATE(XRUNOFFC    (0,0))
  ALLOCATE(XHORTC      (0,0))
  ALLOCATE(XDRIPC      (0,0))
  ALLOCATE(XRRVEGC     (0,0))
  ALLOCATE(XMELTC      (0,0))
  ALLOCATE(XIRRIG_FLUXC(0,0))
  ALLOCATE(XGPPC       (0,0))
  ALLOCATE(XRESPC_AUTO (0,0))
  ALLOCATE(XRESPC_ECO  (0,0))  
  ALLOCATE(XIFLOODC    (0,0))
  ALLOCATE(XPFLOODC    (0,0))
  ALLOCATE(XLE_FLOODC  (0,0))
  ALLOCATE(XLEI_FLOODC (0,0))
!
  ALLOCATE(XAVG_SWDC   (0))
  ALLOCATE(XAVG_SWUC   (0))
  ALLOCATE(XAVG_LWDC   (0))
  ALLOCATE(XAVG_LWUC   (0))
  ALLOCATE(XAVG_FMUC   (0))
  ALLOCATE(XAVG_FMVC   (0))
  ALLOCATE(XSWDC     (0,0))
  ALLOCATE(XSWUC     (0,0))
  ALLOCATE(XLWDC     (0,0))
  ALLOCATE(XLWUC     (0,0))
  ALLOCATE(XFMUC     (0,0))
  ALLOCATE(XFMVC     (0,0))
  !      
  ALLOCATE(XRAINFALLC  (0))
  ALLOCATE(XSNOWFALLC  (0))
  ALLOCATE(XAVG_DWGC   (0))
  ALLOCATE(XAVG_DWGIC  (0))
  ALLOCATE(XAVG_DWRC   (0))
  ALLOCATE(XAVG_DSWEC  (0))
  ALLOCATE(XAVG_WATBUDC(0))
  !
  ALLOCATE(XDWGC   (0,0))
  ALLOCATE(XDWGIC  (0,0))
  ALLOCATE(XDWRC   (0,0))
  ALLOCATE(XDSWEC  (0,0))
  ALLOCATE(XWATBUDC(0,0))
  !
ENDIF
!
IF(.NOT.LGLACIER)THEN
  ALLOCATE(XAVG_ICEFLUXC(0))
  ALLOCATE(XICEFLUXC(0,0))
ENDIF
!
!
!* surface temperature and parameters at 2m
!
ALLOCATE(XTS    (KLU,NPATCH))
ALLOCATE(XAVG_TS(KLU))
XTS     = XUNDEF
XAVG_TS = XUNDEF
ALLOCATE(XTSRAD    (KLU,NPATCH))
ALLOCATE(XAVG_TSRAD(KLU))
XTSRAD     = XUNDEF
XAVG_TSRAD = XUNDEF

!
IF (N2M>=1) THEN
  ALLOCATE(XAVG_RI           (KLU))
  ALLOCATE(XAVG_T2M          (KLU))
  ALLOCATE(XAVG_T2M_MIN      (KLU))
  ALLOCATE(XAVG_T2M_MAX      (KLU))
  ALLOCATE(XAVG_Q2M          (KLU))
  ALLOCATE(XAVG_HU2M         (KLU))
  ALLOCATE(XAVG_HU2M_MIN     (KLU))
  ALLOCATE(XAVG_HU2M_MAX     (KLU))
  ALLOCATE(XAVG_ZON10M       (KLU))
  ALLOCATE(XAVG_MER10M       (KLU))
  ALLOCATE(XAVG_WIND10M      (KLU))
  ALLOCATE(XAVG_WIND10M_MAX  (KLU))
  ALLOCATE(XAVG_SFCO2        (KLU))
  XAVG_RI      = XUNDEF
  XAVG_T2M     = XUNDEF
  XAVG_T2M_MIN = XUNDEF
  XAVG_T2M_MAX = 0.0
  XAVG_Q2M     = XUNDEF
  XAVG_HU2M    = XUNDEF
  XAVG_HU2M_MIN= XUNDEF
  XAVG_HU2M_MAX= -XUNDEF
  XAVG_ZON10M  = XUNDEF
  XAVG_MER10M  = XUNDEF
  XAVG_WIND10M = XUNDEF
  XAVG_WIND10M_MAX = 0.0
  XAVG_SFCO2       = XUNDEF
  !
  ALLOCATE(XRI     (KLU,NPATCH))
  ALLOCATE(XT2M    (KLU,NPATCH))
  ALLOCATE(XQ2M    (KLU,NPATCH))
  ALLOCATE(XHU2M   (KLU,NPATCH))
  ALLOCATE(XZON10M (KLU,NPATCH))
  ALLOCATE(XMER10M (KLU,NPATCH))
  ALLOCATE(XWIND10M(KLU,NPATCH))
  !
  XRI      = XUNDEF
  XT2M     = XUNDEF
  XQ2M     = XUNDEF
  XHU2M    = XUNDEF
  XZON10M  = XUNDEF
  XMER10M  = XUNDEF
  XWIND10M = XUNDEF
ELSE
  ALLOCATE(XAVG_RI           (0))
  ALLOCATE(XAVG_T2M          (0))
  ALLOCATE(XAVG_T2M_MIN      (0))
  ALLOCATE(XAVG_T2M_MAX      (0))
  ALLOCATE(XAVG_Q2M          (0))
  ALLOCATE(XAVG_HU2M         (0))
  ALLOCATE(XAVG_HU2M_MIN     (0))
  ALLOCATE(XAVG_HU2M_MAX     (0))
  ALLOCATE(XAVG_ZON10M       (0))
  ALLOCATE(XAVG_MER10M       (0))
  ALLOCATE(XAVG_WIND10M      (0))
  ALLOCATE(XAVG_WIND10M_MAX  (0))
!
  ALLOCATE(XRI     (0,0))
  ALLOCATE(XT2M    (0,0))
  ALLOCATE(XQ2M    (0,0))
  ALLOCATE(XHU2M   (0,0))
  ALLOCATE(XZON10M (0,0))
  ALLOCATE(XMER10M (0,0))
  ALLOCATE(XWIND10M(0,0))
END IF
!
!* miscellaneous surface fields
!
IF (LSURF_MISC_BUDGET) THEN
  ALLOCATE(XAVG_HV           (KLU))
  ALLOCATE(XAVG_PSNG         (KLU))
  ALLOCATE(XAVG_PSNV         (KLU))
  ALLOCATE(XAVG_PSN          (KLU))
  ALLOCATE(XAVG_ALBT         (KLU))
  ALLOCATE(XAVG_LAI          (KLU))
  !
  ALLOCATE(XAVG_FSAT        (KLU))  
  ALLOCATE(XAVG_FFG         (KLU))
  ALLOCATE(XAVG_FFV         (KLU))
  ALLOCATE(XAVG_FF          (KLU))
  !
  ALLOCATE(XSOIL_TSWI        (KLU))
  ALLOCATE(XSOIL_TWG         (KLU))
  ALLOCATE(XSOIL_TWGI        (KLU))
  ALLOCATE(XAVG_SWI          (KLU,NGROUND_LAYER))
  ALLOCATE(XAVG_TSWI         (KLU,NGROUND_LAYER))
  !
  ALLOCATE(XAVG_TWSNOW       (KLU))
  ALLOCATE(XAVG_TDSNOW       (KLU))
  ALLOCATE(XAVG_TTSNOW       (KLU))
  !
  XAVG_HV      = XUNDEF
  XAVG_SWI     = XUNDEF
  XAVG_TSWI    = XUNDEF
  XSOIL_TSWI   = XUNDEF
  XSOIL_TWG    = XUNDEF
  XSOIL_TWGI   = XUNDEF
  XAVG_PSNG    = XUNDEF
  XAVG_PSNV    = XUNDEF
  XAVG_PSN     = XUNDEF
  XAVG_ALBT    = XUNDEF
  XAVG_LAI     = XUNDEF
  XAVG_FSAT    = XUNDEF  
  XAVG_FFG     = XUNDEF
  XAVG_FFV     = XUNDEF
  XAVG_FF      = XUNDEF
  XAVG_TWSNOW  = XUNDEF
  XAVG_TDSNOW  = XUNDEF
  XAVG_TTSNOW  = XUNDEF
  !
  ALLOCATE(XHV     (KLU,NPATCH))
  ALLOCATE(XSWI    (KLU,NGROUND_LAYER,NPATCH))
  ALLOCATE(XTSWI   (KLU,NGROUND_LAYER,NPATCH))
  ALLOCATE(XTWSNOW (KLU,NPATCH))
  ALLOCATE(XTDSNOW (KLU,NPATCH))
  ALLOCATE(XTTSNOW (KLU,NPATCH))
  ALLOCATE(XDPSNG  (KLU,NPATCH))
  ALLOCATE(XDPSNV  (KLU,NPATCH))
  ALLOCATE(XDPSN   (KLU,NPATCH))
  ALLOCATE(XALBT   (KLU,NPATCH))
  !
  ALLOCATE(XDFSAT  (KLU,NPATCH))
  ALLOCATE(XDFFG   (KLU,NPATCH))
  ALLOCATE(XDFFV   (KLU,NPATCH))
  ALLOCATE(XDFF    (KLU,NPATCH))
  !
  ALLOCATE(XSNOWLIQ  (KLU,TSNOW%NLAYER,NPATCH))
  ALLOCATE(XSNOWTEMP (KLU,TSNOW%NLAYER,NPATCH))
  !
  XHV      = XUNDEF
  XSWI     = XUNDEF
  XTSWI    = XUNDEF
  XTWSNOW  = XUNDEF
  XTDSNOW  = XUNDEF
  XTTSNOW  = XUNDEF
  XDPSNG   = XUNDEF
  XDPSNV   = XUNDEF
  XDPSN    = XUNDEF
  XALBT    = XUNDEF
  XDFSAT   = XUNDEF  
  XDFFG    = XUNDEF
  XDFFV    = XUNDEF
  XDFF     = XUNDEF
  XSNOWLIQ = XUNDEF
  XSNOWTEMP= XUNDEF
  !
  IF(CISBA=='DIF'.AND.LSURF_MISC_DIF)THEN
    ALLOCATE(XSURF_TSWI(KLU))
    ALLOCATE(XSURF_TWG (KLU))
    ALLOCATE(XSURF_TWGI(KLU))
    ALLOCATE(XROOT_TSWI(KLU))
    ALLOCATE(XROOT_TWG (KLU))
    ALLOCATE(XROOT_TWGI(KLU))
    ALLOCATE(XFRD2_TSWI(KLU))
    ALLOCATE(XFRD2_TWG (KLU))
    ALLOCATE(XFRD2_TWGI(KLU))
    ALLOCATE(XFRD3_TSWI(KLU))
    ALLOCATE(XFRD3_TWG (KLU))
    ALLOCATE(XFRD3_TWGI(KLU))    
    XSURF_TSWI = XUNDEF
    XSURF_TWG  = XUNDEF
    XSURF_TWGI = XUNDEF
    XROOT_TSWI = XUNDEF
    XROOT_TWG  = XUNDEF
    XROOT_TWGI = XUNDEF
    XFRD2_TSWI = XUNDEF
    XFRD2_TWG  = XUNDEF
    XFRD2_TWGI = XUNDEF
    XFRD3_TSWI = XUNDEF
    XFRD3_TWG  = XUNDEF
    XFRD3_TWGI = XUNDEF  
  ENDIF
  !
  IF(CISBA=='DIF')THEN
    ALLOCATE(XALT(KLU,NPATCH))
    ALLOCATE(XFLT(KLU,NPATCH))
    ALLOCATE(XAVG_ALT(KLU))
    ALLOCATE(XAVG_FLT(KLU))
    XALT     = XUNDEF
    XFLT     = XUNDEF
    XAVG_ALT = XUNDEF
    XAVG_FLT = XUNDEF          
  ENDIF  
  !
  IF (LTR_ML) THEN
    ALLOCATE (XFAPAR      (KLU, NPATCH))
    ALLOCATE (XFAPIR      (KLU, NPATCH))
    ALLOCATE (XFAPAR_BS   (KLU, NPATCH))
    ALLOCATE (XFAPIR_BS   (KLU, NPATCH))
    ALLOCATE (XDFAPARC    (KLU, NPATCH))
    ALLOCATE (XDFAPIRC    (KLU, NPATCH))
    ALLOCATE (XDLAI_EFFC  (KLU, NPATCH))
    !
    XFAPAR      = XUNDEF
    XFAPIR      = XUNDEF
    XFAPAR_BS   = XUNDEF
    XFAPIR_BS   = XUNDEF
    XDFAPARC    = 0.
    XDFAPIRC    = 0.
    XDLAI_EFFC  = 0.
  ENDIF
  !
ELSE
  ALLOCATE(XAVG_HV           (0))
  ALLOCATE(XAVG_PSNG         (0))
  ALLOCATE(XAVG_PSNV         (0))
  ALLOCATE(XAVG_PSN          (0))
  ALLOCATE(XAVG_ALBT         (0))
  ALLOCATE(XAVG_LAI          (0))
!
  ALLOCATE(XAVG_FSAT        (0))  
  ALLOCATE(XAVG_FFG         (0))
  ALLOCATE(XAVG_FFV         (0))
  ALLOCATE(XAVG_FF          (0))
!
  ALLOCATE(XSOIL_TSWI        (0))
  ALLOCATE(XSOIL_TWG         (0))
  ALLOCATE(XSOIL_TWGI        (0))
  ALLOCATE(XAVG_SWI          (0,0))
  ALLOCATE(XAVG_TSWI         (0,0))
!
  ALLOCATE(XAVG_TWSNOW       (0))
  ALLOCATE(XAVG_TDSNOW       (0))
  ALLOCATE(XAVG_TTSNOW       (0))
!
  ALLOCATE(XHV     (0,0))
  ALLOCATE(XSWI    (0,0,0))
  ALLOCATE(XTSWI   (0,0,0))
  ALLOCATE(XTWSNOW (0,0))
  ALLOCATE(XTDSNOW (0,0))
  ALLOCATE(XTTSNOW (0,0))
  ALLOCATE(XDPSNG  (0,0))
  ALLOCATE(XDPSNV  (0,0))
  ALLOCATE(XDPSN   (0,0))
  ALLOCATE(XALBT   (0,0))
!
  ALLOCATE(XDFSAT  (0,0))
  ALLOCATE(XDFFG   (0,0))
  ALLOCATE(XDFFV   (0,0))
  ALLOCATE(XDFF    (0,0))
!
  ALLOCATE(XSNOWLIQ  (0,0,0))
  ALLOCATE(XSNOWTEMP (0,0,0))
END IF
!
IF (CISBA/='DIF') THEN
  ALLOCATE(XSURF_TSWI(0))
  ALLOCATE(XSURF_TWG (0))
  ALLOCATE(XSURF_TWGI(0))
  ALLOCATE(XROOT_TSWI(0))
  ALLOCATE(XROOT_TWG (0))
  ALLOCATE(XROOT_TWGI(0))
  ALLOCATE(XFRD2_TSWI(0))
  ALLOCATE(XFRD2_TWG (0))
  ALLOCATE(XFRD2_TWGI(0))
  ALLOCATE(XFRD3_TSWI(0))
  ALLOCATE(XFRD3_TWG (0))
  ALLOCATE(XFRD3_TWGI(0))    
  ALLOCATE(XALT(0,0))
  ALLOCATE(XFLT(0,0))
  ALLOCATE(XAVG_ALT(0))
  ALLOCATE(XAVG_FLT(0))      
ENDIF
!
IF (.NOT.LTR_ML) THEN
  ALLOCATE (XFAPAR      (0, 0))
  ALLOCATE (XFAPIR      (0, 0))
  ALLOCATE (XFAPAR_BS   (0, 0))
  ALLOCATE (XFAPIR_BS   (0, 0))
  ALLOCATE (XDFAPARC    (0, 0))
  ALLOCATE (XDFAPIRC    (0, 0))
  ALLOCATE (XDLAI_EFFC  (0, 0))
ENDIF
!
!* transfer coefficients
!
IF (LCOEF) THEN
  ALLOCATE(XAVG_CD   (KLU))
  ALLOCATE(XAVG_CH   (KLU))
  ALLOCATE(XAVG_CE   (KLU))
  ALLOCATE(XAVG_Z0   (KLU))
  ALLOCATE(XAVG_Z0H  (KLU))
  ALLOCATE(XAVG_Z0EFF(KLU))
  !
  XAVG_CD      = XUNDEF
  XAVG_CH      = XUNDEF
  XAVG_CE      = XUNDEF
  XAVG_Z0      = XUNDEF
  XAVG_Z0H     = XUNDEF
  XAVG_Z0EFF   = XUNDEF
  !
  ALLOCATE(XCD            (KLU,NPATCH))
  ALLOCATE(XCH            (KLU,NPATCH))
  ALLOCATE(XCE            (KLU,NPATCH))
  ALLOCATE(XZ0_WITH_SNOW  (KLU,NPATCH))
  ALLOCATE(XZ0H_WITH_SNOW (KLU,NPATCH))
  ALLOCATE(XZ0EFF         (KLU,NPATCH))
  !
  XCD            = XUNDEF
  XCH            = XUNDEF
  XCE            = XUNDEF
  XZ0_WITH_SNOW  = XUNDEF
  XZ0H_WITH_SNOW = XUNDEF
  XZ0EFF         = XUNDEF
ELSE
  ALLOCATE(XAVG_CD   (0))
  ALLOCATE(XAVG_CH   (0))
  ALLOCATE(XAVG_CE   (0))
  ALLOCATE(XAVG_Z0   (0))
  ALLOCATE(XAVG_Z0H  (0))
  ALLOCATE(XAVG_Z0EFF(0))
!
  ALLOCATE(XCD            (0,0))
  ALLOCATE(XCH            (0,0))
  ALLOCATE(XCE            (0,0))
  ALLOCATE(XZ0_WITH_SNOW  (0,0))
  ALLOCATE(XZ0H_WITH_SNOW (0,0))
  ALLOCATE(XZ0EFF         (0,0))
END IF
!
!
!* surface humidity
!
IF (LSURF_VARS) THEN
  ALLOCATE(XAVG_QS   (KLU))
  !
  XAVG_QS      = XUNDEF
  !
  ALLOCATE(XQS            (KLU,NPATCH))
  !
  XQS            = XUNDEF
ELSE
  ALLOCATE(XQS            (0,0))
END IF
!
!* Irrigation threshold
!
IF (LAGRIP) THEN
  ALLOCATE(XSEUIL         (KLU,NPATCH))
  !
  XSEUIL         = XUNDEF
ELSE
  ALLOCATE(XSEUIL         (0,0))
END IF
!
!* Chemical fluxes
IF (NBEQ>0 .AND. LCH_BIO_FLUX) THEN
  ALLOCATE(XFISO(KLU))
  ALLOCATE(XFMONO(KLU))
  !
  XFISO         = XUNDEF
  XFMONO        = XUNDEF
ELSE
  ALLOCATE(XFISO(0))
  ALLOCATE(XFMONO(0))
ENDIF
!
IF (CPHOTO/='NON') THEN
  ALLOCATE(XIACAN(KLU,SIZE(XABC),NPATCH))
  !
  XIACAN        = XUNDEF
  !
ELSE
  ALLOCATE(XIACAN(0,0,0))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_ISBA_INIT_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_ISBA_INIT_n
