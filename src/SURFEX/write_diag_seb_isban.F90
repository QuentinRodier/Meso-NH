!     #########
      SUBROUTINE WRITE_DIAG_SEB_ISBA_n(HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_SEB_ISBA* - writes the ISBA diagnostic fields
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
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
!!      Original    01/2004
!!      B. Decharme 06/2009  key to write (or not) patch result
!!      B. Decharme 08/2009  cumulative radiative budget
!!      B. Decharme  09/2012 : Bug in local variables declaration in PROVAR_TO_DIAG
!!      B. Decharme 09/2012  New diag :
!!                           carbon fluxes and reservoirs
!!                           soil liquid and ice water content in kg/m2 and m3/m3
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_MPI, ONLY : NWG_SIZE
!
USE MODD_SURF_PAR,   ONLY : XUNDEF, NUNDEF
!
USE MODD_CSTS,       ONLY : XRHOLW, XTT, XLMTT
!
USE MODD_DIAG_SURF_ATM_n,ONLY : LPROVAR_TO_DIAG,  LRESET_BUDGETC
!
USE MODD_ISBA_n,     ONLY :   NPATCH, XPATCH, LFLOOD, CISBA, CHORT,   &
                              LGLACIER, NGROUND_LAYER, LTEMP_ARP,     &
                              NTEMPLAYER_ARP, TSNOW, XLE, XDG, XTG,   &
                              XWG, XWGI, XWR, XICE_STO, XWSAT, XDZG,  &
                              NWG_LAYER, CPHOTO, CRESPSL, XBIOMASS,   &
                              XLITTER, XSOILCARB, XLIGNIN_STRUC,      &
                              NNBIOMASS, NNLITTER, NNSOILCARB,        &
                              NNLITTLEVS
!         
USE MODD_AGRI  ,     ONLY : LAGRIP
!
USE MODD_DIAG_ISBA_n,ONLY :   N2M, LSURF_BUDGET, LRAD_BUDGET, LCOEF,            &
                              LSURF_VARS,LPATCH_BUDGET,                         &
                              XAVG_RN, XAVG_H, XAVG_LE, XAVG_LEI, XAVG_GFLUX,   &
                              XAVG_RI, XAVG_CD, XAVG_CH, XAVG_CE,               &
                              XAVG_T2M, XAVG_Q2M, XAVG_HU2M,                    &
                              XAVG_ZON10M, XAVG_MER10M, XAVG_Z0, XAVG_Z0H,      &
                              XAVG_QS, XAVG_T2M_MIN, XAVG_T2M_MAX,              &
                              XAVG_SWD, XAVG_SWU, XAVG_SWBD, XAVG_SWBU,         &
                              XAVG_LWD, XAVG_LWU, XAVG_FMU, XAVG_FMV,           &
                              XRN, XH, XGFLUX, XLEI,                            &
                              XRI,XT2M, XQ2M, XHU2M, XZON10M, XMER10M,          &
                              XZ0_WITH_SNOW, XZ0H_WITH_SNOW, XQS, XWIND10M,     &
                              XSWD, XSWU, XSWBD, XSWBU, XLWD, XLWU, XFMU, XFMV, &
                              XSWDC, XSWUC, XLWDC, XLWUC, XFMUC, XFMVC,         &
                              XAVG_SWDC, XAVG_SWUC, XAVG_LWDC, XAVG_LWUC,       &
                              XAVG_FMUC, XAVG_FMVC, XAVG_HU2M_MIN,              &
                              XAVG_HU2M_MAX, XAVG_WIND10M, XAVG_WIND10M_MAX,    &  
                              XAVG_SFCO2
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
USE MODD_DIAG_EVAP_ISBA_n,ONLY :   LSURF_EVAP_BUDGET, LSURF_BUDGETC,              &
                                   LWATER_BUDGET,                                 &
                                   XRNC, XAVG_RNC, XHC, XAVG_HC,                  &
                                   XLEC, XAVG_LEC, XGFLUXC, XAVG_GFLUXC,          &
                                   XLEIC, XAVG_LEIC,                              &
                                   XLEG, XLEGC, XAVG_LEG, XAVG_LEGC,              &
                                   XLEGI, XLEGIC, XAVG_LEGI, XAVG_LEGIC,          &
                                   XLEV, XLEVC, XAVG_LEV, XAVG_LEVC,              &
                                   XLES, XLESC, XAVG_LES, XAVG_LESC,              &
                                   XLESL, XLESLC, XAVG_LESL, XAVG_LESLC,          &
                                   XLER, XLERC, XAVG_LER, XAVG_LERC,              &
                                   XLETR, XLETRC, XAVG_LETR, XAVG_LETRC,          &
                                   XEVAP, XEVAPC, XAVG_EVAP, XAVG_EVAPC,          &
                                   XDRAIN, XDRAINC, XAVG_DRAIN, XAVG_DRAINC,      &
                                   XRUNOFF, XRUNOFFC, XAVG_RUNOFF, XAVG_RUNOFFC,  &
                                   XHORT, XHORTC, XAVG_HORT, XAVG_HORTC,          &
                                   XDRIP, XDRIPC, XAVG_DRIP, XAVG_DRIPC,          &
                                   XMELT, XMELTC, XAVG_MELT, XAVG_MELTC,          &
                                   XIFLOOD, XIFLOODC, XAVG_IFLOOD, XAVG_IFLOODC,  &
                                   XPFLOOD, XPFLOODC, XAVG_PFLOOD, XAVG_PFLOODC,  &
                                   XLE_FLOOD, XLE_FLOODC, XAVG_LE_FLOOD,          &
                                   XAVG_LE_FLOODC, XLEI_FLOOD, XLEI_FLOODC,       &
                                   XAVG_LEI_FLOOD, XAVG_LEI_FLOODC,               &
                                   XICEFLUXC, XAVG_ICEFLUXC,                      &
                                   XRRVEG, XRRVEGC, XAVG_RRVEG, XAVG_RRVEGC,      &
                                   XIRRIG_FLUX, XIRRIG_FLUXC, XAVG_IRRIG_FLUX,    &
                                   XAVG_IRRIG_FLUXC,                              &
                                   XGPP,XGPPC,XAVG_GPP,XAVG_GPPC, XRESP_AUTO,     &
                                   XRESPC_AUTO,XAVG_RESP_AUTO,XAVG_RESPC_AUTO,    &
                                   XRESP_ECO,XRESPC_ECO,XAVG_RESP_ECO,            &
                                   XAVG_RESPC_ECO,                                & 
                                   XDWG, XDWGC, XAVG_DWG, XAVG_DWGC,              &
                                   XDWGI, XDWGIC, XAVG_DWGI, XAVG_DWGIC,          &
                                   XDWR, XDWRC, XAVG_DWR, XAVG_DWRC,              &
                                   XDSWE, XDSWEC, XAVG_DSWE, XAVG_DSWEC,          &
                                   XRAINFALL, XRAINFALLC, XSNOWFALL, XSNOWFALLC,  &
                                   XWATBUD, XWATBUDC, XAVG_WATBUD, XAVG_WATBUDC                               
!
USE MODD_CH_ISBA_n,    ONLY : XDEP, CCH_DRY_DEP, LCH_BIO_FLUX, CCH_NAMES, NBEQ, &
                              NDSTEQ, LCH_NO_FLUX
USE MODD_GR_BIOG_n,    ONLY : XFISO, XFMONO, XNOFLUX
USE MODD_DST_n
USE MODD_DST_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be write
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=2)  :: YNUM
!
INTEGER           :: JSV, JSW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_ISBA_N',0,ZHOOK_HANDLE)
 CALL INIT_IO_SURF_n(HPROGRAM,'NATURE','ISBA  ','WRITE')
!
!-------------------------------------------------------------------------------
!
!*       2.     Richardson number :
!               -----------------
!
IF (N2M>=1) THEN
  !
  YRECFM='RI_ISBA'
  YCOMMENT='Richardson number over tile nature'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_RI(:),IRESP,HCOMMENT=YCOMMENT)
  !
END IF
!
!*       3.     Energy fluxes :
!               -------------
!
IF (LSURF_BUDGET) THEN
  !
  YRECFM='RN_ISBA'
  YCOMMENT='Net radiation over tile nature'//' (W/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_RN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='H_ISBA'
  YCOMMENT='Sensible heat flux over tile nature'//' (W/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_H(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LE_ISBA'
  YCOMMENT='total latent heat flux over tile nature'//' (W/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LE(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEI_ISBA'
  YCOMMENT='sublimation latent heat flux over tile nature'//' (W/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LEI(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='GFLUX_ISBA'
  YCOMMENT='Ground flux over tile nature'//' (W/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_GFLUX(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF (LRAD_BUDGET  .OR. (LSURF_BUDGETC .AND. .NOT.LRESET_BUDGETC)) THEN
    !
    YRECFM='SWD_ISBA'
    YCOMMENT='short wave downward radiation over tile nature'//' (W/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_SWD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWU_ISBA'
    YCOMMENT='short wave upward radiation over tile nature'//' (W/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_SWU(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWD_ISBA'
    YCOMMENT='long wave downward radiation over tile nature'//' (W/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LWD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWU_ISBA'
    YCOMMENT='long wave upward radiation over tile nature'//' (W/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LWU(:),IRESP,HCOMMENT=YCOMMENT)
    !
    DO JSW=1, SIZE(XSWBD,2)
      YNUM=ACHAR(48+JSW)
      !
      YRECFM='SWD_ISBA_'//YNUM
      YCOMMENT='short wave downward radiation over tile nature for spectral band'//YNUM//' (W/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_SWBD(:,JSW),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='SWU_ISBA_'//YNUM
      YCOMMENT='short wave upward radiation over tile nature for spectral band'//YNUM//' (W/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_SWBU(:,JSW),IRESP,HCOMMENT=YCOMMENT)
      !
    ENDDO
    !
  ENDIF
  !
  YRECFM='FMU_ISBA'
  YCOMMENT='u component of wind stress'//' (Pa)'  
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_FMU(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='FMV_ISBA'
  YCOMMENT='v component of wind stress'//' (Pa)'  
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_FMV(:),IRESP,HCOMMENT=YCOMMENT)
  !
END IF
!
!*       4.    Specific Energy fluxes :(for each patch)
!              ----------------------------------------
!
IF (LSURF_EVAP_BUDGET) THEN
  !
  YRECFM='LEG_ISBA'
  YCOMMENT='bare ground evaporation for tile nature'//' (W/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LEG(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEGI_ISBA'
  YCOMMENT='bare ground sublimation for tile nature'//' (W/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LEGI(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEV_ISBA'
  YCOMMENT='total vegetation evaporation for tile nature'//' (W/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LEV(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LES_ISBA'
  YCOMMENT='snow sublimation for tile nature'//' (W/m2)'  
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LES(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO')THEN  
    YRECFM='LESL_ISBA'
    YCOMMENT='liquid water evaporation over snow for tile nature'//' (W/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LESL(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !  
  YRECFM='LER_ISBA'
  YCOMMENT='canopy direct evaporation for tile nature'//' (W/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LER(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LETR_ISBA'
  YCOMMENT='vegetation transpiration for tile nature'//' (W/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LETR(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='EVAP_ISBA'
  YCOMMENT='total evaporative flux for tile nature'//' (Kg/m2/s)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_EVAP(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='DRAIN_ISBA'
  YCOMMENT='drainage for tile nature'//' (Kg/m2/s)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_DRAIN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='RUNOFF_ISBA'
  YCOMMENT='runoff for tile nature'//' (Kg/m2/s)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_RUNOFF(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(CHORT=='SGH'.OR.CISBA=='DIF')THEN
    YRECFM='HORTON_ISBA'
    YCOMMENT='horton runoff for tile nature'//' (Kg/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_HORT(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  YRECFM='DRIVEG_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_DRIP(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='RRVEG_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_RRVEG(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='SNOMLT_ISBA'
  YCOMMENT='snow melting rate'//' (Kg/m2/s)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_MELT(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(LAGRIP)THEN
    YRECFM='IRRIG_ISBA'
    YCOMMENT='irrigation rate'//' (Kg/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_IRRIG_FLUX(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF  
  !
  IF(LFLOOD)THEN
    !        
    YRECFM='IFLOOD_ISBA'
    YCOMMENT='flood soil infiltration (Kg/m2/s)'    
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_IFLOOD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='PFLOOD_ISBA'
    YCOMMENT='intercepted precipitation by floodplains (Kg/m2/s)'    
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_PFLOOD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LEF_ISBA'
    YCOMMENT='total floodplains evaporation (W/m2)'   
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LE_FLOOD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LEIF_ISBA'
    YCOMMENT='solid floodplains evaporation (W/m2)'    
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LEI_FLOOD(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
  IF(CPHOTO/='NON')THEN
    !
    YRECFM='GPP_ISBA'
    YCOMMENT='gross primary production over tile nature (kgCO2/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_GPP(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='R_AUTO_ISBA'
    YCOMMENT='autotrophic respiration over tile nature (kgCO2/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_RESP_AUTO(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='R_ECO_ISBA'
    YCOMMENT='ecosystem respiration over tile nature (kgCO2/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_RESP_ECO(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !  
  IF(LWATER_BUDGET)THEN 
    !
    YRECFM='RAINF_ISBA'
    YCOMMENT='input rainfall rate (Kg/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XRAINFALL(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SNOWF_ISBA'
    YCOMMENT='input snowfall rate (Kg/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XSNOWFALL(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DWG_ISBA'
    YCOMMENT='change in liquid soil moisture (Kg/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_DWG(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DWGI_ISBA'
    YCOMMENT='change in solid soil moisture (Kg/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_DWGI(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DWR_ISBA'
    YCOMMENT='change in water on canopy (Kg/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_DWR(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DSWE_ISBA'
    YCOMMENT='change in snow water equivalent (Kg/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_DSWE(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='WATBUD_ISBA'
    YCOMMENT='isba water budget as residue (Kg/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_WATBUD(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !  
ENDIF
!
!*       5.    Cumulated Energy fluxes
!              -----------------------
!
IF (LSURF_BUDGETC) THEN
  !
  YRECFM='LEGC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LEGC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEGIC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LEGIC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEVC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LEVC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LESC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LESC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO')THEN  
    YRECFM='LESLC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LESLC(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !  
  YRECFM='LERC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LERC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LETRC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LETRC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='EVAPC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_EVAPC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='DRAINC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_DRAINC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='RUNOFFC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_RUNOFFC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(CHORT=='SGH'.OR.CISBA=='DIF')THEN
    YRECFM='HORTONC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_HORTC(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  YRECFM='DRIVEGC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_DRIPC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='RRVEGC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_RRVEGC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='SNOMLTC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_MELTC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(LAGRIP)THEN
    YRECFM='IRRIGC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_IRRIG_FLUXC(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !  
  IF(LGLACIER)THEN
    YRECFM='ICE_FC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_ICEFLUXC(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  IF(LFLOOD)THEN
    !
    YRECFM='IFLOODC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_IFLOODC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='PFLOODC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_PFLOODC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LEFC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LE_FLOODC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LEIFC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LEI_FLOODC(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
  YRECFM='RNC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_RNC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='HC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_HC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LEC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEIC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LEIC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='GFLUXC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_GFLUXC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF (LRAD_BUDGET .OR. (LSURF_BUDGETC .AND. .NOT.LRESET_BUDGETC)) THEN
    !
    YRECFM='SWDC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_SWDC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWUC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_SWUC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWDC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LWDC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWUC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LWUC(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
  YRECFM='FMUC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Pa.s)'  
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_FMUC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='FMVC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Pa.s)'  
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_FMVC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(CPHOTO/='NON')THEN
    !
    YRECFM='GPPC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kgCO2/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_GPPC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='RC_AUTO_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kgCO2/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_RESPC_AUTO(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='RC_ECO_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kgCO2/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_RESPC_ECO(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !  
  IF(LWATER_BUDGET .OR. (LSURF_BUDGETC .AND. .NOT.LRESET_BUDGETC))THEN 
    !
    YRECFM='RAINFC_ISBA'
    YCOMMENT='cumulated input rainfall rate (Kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XRAINFALLC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SNOWFC_ISBA'
    YCOMMENT='cumulated input snowfall rate (Kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XSNOWFALLC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DWGC_ISBA'
    YCOMMENT='cumulated change in liquid soil moisture (Kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_DWGC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DWGIC_ISBA'
    YCOMMENT='cumulated change in solid soil moisture (Kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_DWGIC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DWRC_ISBA'
    YCOMMENT='cumulated change in water on canopy (Kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_DWRC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DSWEC_ISBA'
    YCOMMENT='cumulated change in snow water equivalent (Kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_DSWEC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='WATBUDC_ISBA'
    YCOMMENT='cumulated isba water budget as residue (Kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_WATBUDC(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF 
  !  
ENDIF
!
!*       6.     parameters at 2 and 10 meters :
!               -------------------------------
!
IF (N2M>=1) THEN
  !
  YRECFM='T2M_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (K)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_T2M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='T2MMIN_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (K)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_T2M_MIN(:),IRESP,HCOMMENT=YCOMMENT)
  XAVG_T2M_MIN(:)=XUNDEF
  !
  YRECFM='T2MMAX_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (K)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_T2M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
  XAVG_T2M_MAX(:)=0.0
  !
  YRECFM='Q2M_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_Q2M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='HU2M_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (-)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_HU2M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='HU2MMIN_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (-)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_HU2M_MIN(:),IRESP,HCOMMENT=YCOMMENT)
  XAVG_HU2M_MIN(:)=XUNDEF
  !
  YRECFM='HU2MMAX_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (-)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_HU2M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
  XAVG_HU2M_MAX(:)=-XUNDEF
  !
  YRECFM='ZON10M_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (M/S)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_ZON10M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='MER10M_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (M/S)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_MER10M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='W10M_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (M/S)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_WIND10M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='W10MMAX_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (M/S)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_WIND10M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
  XAVG_WIND10M_MAX(:)=0.0
  !
  YRECFM='SFCO2_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (KG/M2/S)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_SFCO2(:),IRESP,HCOMMENT=YCOMMENT)
  !  
END IF
!----------------------------------------------------------------------------
!
!*       7.     Transfer coefficients
!               ---------------------
!
IF (LCOEF) THEN
  !
  YRECFM='CD_ISBA'
  YCOMMENT='X_Y_'//YRECFM
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_CD(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='CH_ISBA'
  YCOMMENT='X_Y_'//YRECFM
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_CH(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='CE_ISBA'
  YCOMMENT='X_Y_'//YRECFM
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_CE(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='Z0_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (M)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_Z0(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='Z0H_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (M)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_Z0H(:),IRESP,HCOMMENT=YCOMMENT)
  !
ENDIF
!
!----------------------------------------------------------------------------
!
!*       8.     Surface humidity
!               ----------------
IF (LSURF_VARS) THEN
  !
  YRECFM='QS_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_QS(:),IRESP,HCOMMENT=YCOMMENT)
  !
ENDIF
!
!----------------------------------------------------------------------------
!
!*       9.     Diag of prognostic fields
!               -------------------------
!
IF (LPROVAR_TO_DIAG) CALL PROVAR_TO_DIAG
!
!----------------------------------------------------------------------------
!
!User want (or not) patch output
IF(LPATCH_BUDGET.AND.(NPATCH >1))THEN
    !----------------------------------------------------------------------------
    !
    !*      10.     Richardson number (for each patch)
    !               -----------------
    !
    IF (N2M>=1) THEN
      !
      YRECFM='RI_P'
      YCOMMENT='X_Y_'//YRECFM
      CALL WRITE_SURF(HPROGRAM,YRECFM,XRI(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
    END IF
    !
    !*       11.     Energy fluxes :(for each patch)
    !                -------------
    !
    IF (LSURF_BUDGET) THEN
      !
      YRECFM='RN_P'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XRN(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='H_P'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XH(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LE_P'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLE(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LEI_P'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLEI(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='GFLUX_P'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XGFLUX(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      IF (LRAD_BUDGET .OR. (LSURF_BUDGETC .AND. .NOT.LRESET_BUDGETC)) THEN
        !
        YRECFM='SWD_P'
        YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XSWD(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='SWU_P'
        YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XSWU(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='LWD_P'
        YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XLWD(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='LWU_P'
        YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XLWU(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        DO JSW=1, SIZE(XSWBD,2)
          YNUM=ACHAR(48+JSW)
          !
          YRECFM='SWD_P'//YNUM
          YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
          CALL WRITE_SURF(HPROGRAM,YRECFM,XSWBD(:,JSW,:),IRESP,HCOMMENT=YCOMMENT)
          !
          YRECFM='SWU_P'//YNUM
          YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
          CALL WRITE_SURF(HPROGRAM,YRECFM,XSWBU(:,JSW,:),IRESP,HCOMMENT=YCOMMENT)
          !
        ENDDO
        !
      ENDIF
      !
      YRECFM='FMU_P'
      YCOMMENT='X_Y_'//YRECFM//' (Pa)'      
      CALL WRITE_SURF(HPROGRAM,YRECFM,XFMU(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='FMV_P'
      YCOMMENT='X_Y_'//YRECFM//' (Pa)'      
      CALL WRITE_SURF(HPROGRAM,YRECFM,XFMV(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
    END IF
    !
    !*       12.    Specific Energy fluxes :(for each patch)
    !               ----------------------------------------
    !
    IF (LSURF_EVAP_BUDGET) THEN
      !
      YRECFM='LEG_P'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLEG(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LEGI_P'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLEGI(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LEV_P'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLEV(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LES_P'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLES(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      IF(TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO')THEN  
        YRECFM='LESL_P'
        YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XLESL(:,:),IRESP,HCOMMENT=YCOMMENT)
      ENDIF
      !      
      YRECFM='LER_P'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLER(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LETR_P'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLETR(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='EVAP_P'
      YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XEVAP(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='DRAIN_P'
      YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XDRAIN(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='RUNOFF_P'
      YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XRUNOFF(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      IF(CHORT=='SGH'.OR.CISBA=='DIF')THEN
        YRECFM='HORTON_P'
        YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XHORT(:,:),IRESP,HCOMMENT=YCOMMENT)
      ENDIF
      !
      YRECFM='DRIVEG_P'
      YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XDRIP(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='RRVEG_P'
      YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XRRVEG(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='SNOMLT_P'
      YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XMELT(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      IF(LAGRIP)THEN
        YRECFM='IRRIG_P'
        YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XIRRIG_FLUX(:,:),IRESP,HCOMMENT=YCOMMENT)
      ENDIF
      !      
      IF(LFLOOD)THEN
        !
        YRECFM='IFLOOD_P'
        YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XIFLOOD(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='PFLOOD_P'
        YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XPFLOOD(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='LEF_P'
        YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XLE_FLOOD(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='LEIF_P'
        YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XLEI_FLOOD(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
      ENDIF
      !
      IF(CPHOTO/='NON')THEN
        !
        YRECFM='GPP_P'
        YCOMMENT='gross primary production per patch (kgCO2/m2/s)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XGPP(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='R_AUTO_P'
        YCOMMENT='autotrophic respiration per patch (kgCO2/m2/s)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XRESP_AUTO(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='R_ECO_P'
        YCOMMENT='ecosystem respiration per patch (kgCO2/m2/s)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XRESP_ECO(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
      ENDIF
      !
      IF(LWATER_BUDGET)THEN 
        !
        YRECFM='DWG_P'
        YCOMMENT='change in liquid soil moisture per patch (Kg/m2/s)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XDWG(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='DWGI_P'
        YCOMMENT='change in solid soil moisture per patch (Kg/m2/s)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XDWGI(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='DWR_P'
        YCOMMENT='change in water on canopy per patch (Kg/m2/s)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XDWR(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='DSWE_P'
        YCOMMENT='change in snow water equivalent per patch (Kg/m2/s)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XDSWE(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='WATBUD_P'
        YCOMMENT='isba water budget as residue per patch (Kg/m2/s)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XWATBUD(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
      ENDIF
      !      
    ENDIF
    !
    !*       13.    surface temperature parameters at 2 and 10 meters (for each patch):
    !               -------------------------------------------------------------------
    !
    IF (N2M>=1) THEN
      !
      YRECFM='T2M_P'
      YCOMMENT='X_Y_'//YRECFM//' (K)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XT2M(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='Q2M_P'
      YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XQ2M(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='HU2M_P'
      YCOMMENT='X_Y_'//YRECFM//' (PERCENT)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XHU2M(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='ZON10M_P'
      YCOMMENT='X_Y_'//YRECFM//' (M/S)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XZON10M(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='MER10M_P'
      YCOMMENT='X_Y_'//YRECFM//' (M/S)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XMER10M(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='W10M_P'
      YCOMMENT='X_Y_'//YRECFM//' (M/S)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XWIND10M(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
    END IF
    !
    !*       14.    Cumulated Energy fluxes :(for each patch)
    !               -----------------------------------------
    !
    IF (LSURF_BUDGETC) THEN
      !
      YRECFM='LEGC_P'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLEGC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LEGIC_P'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLEGIC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LEVC_P'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLEVC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LESC_P'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLESC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      IF(TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO')THEN  
        YRECFM='LESLC_P'
        YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XLESLC(:,:),IRESP,HCOMMENT=YCOMMENT)
      ENDIF      
      !
      YRECFM='LERC_P'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLERC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LETRC_P'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLETRC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='EVAPC_P'
      YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XEVAPC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='DRAINC_P'
      YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XDRAINC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='RUNOFFC_P'
      YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XRUNOFFC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      IF(CHORT=='SGH'.OR.CISBA=='DIF')THEN
        YRECFM='HORTONC_P'
        YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XHORTC(:,:),IRESP,HCOMMENT=YCOMMENT)
      ENDIF
      !
      YRECFM='DRIVEGC_P'
      YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XDRIPC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='RRVEGC_P'
      YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XRRVEGC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='SNOMLTC_P'
      YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XMELTC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      IF(LAGRIP)THEN
        YRECFM='IRRIGC_P'
        YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XIRRIG_FLUXC(:,:),IRESP,HCOMMENT=YCOMMENT)
      ENDIF      
      !
      IF(LGLACIER)THEN
        YRECFM='ICE_FC_P'
        YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XICEFLUXC(:,:),IRESP,HCOMMENT=YCOMMENT)
      ENDIF
      !
      IF(LFLOOD)THEN
        !        
        YRECFM='IFLOODC_P'
        YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XIFLOODC(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='PFLOODC_P'
        YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XPFLOODC(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='LEFC_P'
        YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XLE_FLOODC(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='LEIFC_P'
        YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XLEI_FLOODC(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
      ENDIF
      !
      YRECFM='RNC_P'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XRNC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='HC_P'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XHC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LEC_P'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLEC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LEIC_P'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLEIC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='GFLUXC_P'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XGFLUXC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      IF (LRAD_BUDGET) THEN
        !
        YRECFM='SWDC_P'
        YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XSWDC(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='SWUC_P'
        YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XSWUC(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='LWDC_P'
        YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XLWDC(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='LWUC_P'
        YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XLWUC(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
      ENDIF
      !
      YRECFM='FMUC_P'
      YCOMMENT='X_Y_'//YRECFM//' (Pa.s)'      
      CALL WRITE_SURF(HPROGRAM,YRECFM,XFMUC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='FMVC_P'
      YCOMMENT='X_Y_'//YRECFM//' (Pa.s)'      
      CALL WRITE_SURF(HPROGRAM,YRECFM,XFMVC(:,:),IRESP,HCOMMENT=YCOMMENT)
      !
      IF(CPHOTO/='NON')THEN
        !
        YRECFM='GPPC_P'
        YCOMMENT='cumulated gross primary production per patch (kgCO2/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XGPPC(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='RC_AUTO_P'
        YCOMMENT='cumulated autotrophic respiration per patch (kgCO2/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XRESPC_AUTO(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='RC_ECO_P'
        YCOMMENT='cumulated ecosystem respiration per patch (kgCO2/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XRESPC_ECO(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
      ENDIF
      !  
      IF(LWATER_BUDGET .OR. (LSURF_BUDGETC .AND. .NOT.LRESET_BUDGETC))THEN 
        !
        YRECFM='DWGC_P'
        YCOMMENT='cumulated change in liquid soil moisture per patch (Kg/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XDWGC(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='DWGIC_P'
        YCOMMENT='cumulated change in solid soil moisture per patch (Kg/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XDWGIC(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='DWRC_P'
        YCOMMENT='cumulated change in water on canopy per patch (Kg/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XDWRC(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='DSWEC_P'
        YCOMMENT='cumulated change in snow water equivalent per patch (Kg/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XDSWEC(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
        YRECFM='WATBUDC_P'
        YCOMMENT='cumulated isba water budget as residue per patch (Kg/m2)'
        CALL WRITE_SURF(HPROGRAM,YRECFM,XWATBUDC(:,:),IRESP,HCOMMENT=YCOMMENT)
        !
      ENDIF
      !      
    ENDIF
    !-------------------------------------------------------------------------------
ENDIF
!User want (or not) patch output
!-------------------------------------------------------------------------------
!
!*       15.     chemical diagnostics:
!               --------------------
!
IF (NBEQ>0 .AND. CCH_DRY_DEP=="WES89 ") THEN
  !
  DO JSV = 1,SIZE(CCH_NAMES,1)
    YRECFM='DV_NAT_'//TRIM(CCH_NAMES(JSV))
    WRITE(YCOMMENT,'(A13,I3.3)')'(m/s) DV_NAT_',JSV
    CALL WRITE_SURF(HPROGRAM,YRECFM,XDEP(:,JSV,:),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
ENDIF
!
IF (NBEQ>0 .AND. LCH_BIO_FLUX) THEN
  !
  IF (ASSOCIATED(XFISO)) THEN
    YRECFM='FISO'
    WRITE(YCOMMENT,'(A21)')'FISO (molecules/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XFISO(:),IRESP,HCOMMENT=YCOMMENT)
  END IF
  !
  IF (ASSOCIATED(XFISO)) THEN
    YRECFM='FMONO'
    WRITE(YCOMMENT,'(A22)')'FMONO (molecules/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XFMONO(:),IRESP,HCOMMENT=YCOMMENT)
  END IF
  !
ENDIF
!
IF (LCH_NO_FLUX) THEN
  IF (ASSOCIATED(XNOFLUX)) THEN
    YRECFM='NOFLUX'
    WRITE(YCOMMENT,'(A21)')'NOFLUX (molecules/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XNOFLUX(:),IRESP,HCOMMENT=YCOMMENT)
  END IF
END IF
!
IF (NDSTEQ > 0)THEN
  !
  DO JSV = 1,NDSTMDE ! for all dust modes
    WRITE(YRECFM,'(A7,I3.3)')'FLX_DST',JSV
    YCOMMENT='X_Y_'//YRECFM//' (kg/m2/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XSFDST(:,JSV,:),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_ISBA_N',1,ZHOOK_HANDLE)
!
CONTAINS
!
!-------------------------------------------------------------------------------
!
SUBROUTINE PROVAR_TO_DIAG
!
REAL, DIMENSION(SIZE(XTG,1))             :: ZPATCH, ZWORK
REAL, DIMENSION(SIZE(XWG,1),SIZE(XWG,2)) :: ZWG
REAL, DIMENSION(SIZE(XWG,1),SIZE(XWG,2)) :: ZWGI
REAL, DIMENSION(SIZE(XWG,1),SIZE(XWG,2)) :: ZMOIST
REAL, DIMENSION(SIZE(XWG,1),SIZE(XWG,2)) :: ZICE
REAL, DIMENSION(SIZE(XTG,1),SIZE(XTG,2)) :: ZTG
REAL, DIMENSION(SIZE(XDG,1),SIZE(XDG,2)) :: ZDG_TOT
REAL, DIMENSION(SIZE(XDG,1),SIZE(XDG,2),SIZE(XDG,3)) :: ZDG
!
REAL, DIMENSION(SIZE(XDG,1),NNBIOMASS)   :: ZBIOMASS
REAL, DIMENSION(SIZE(XDG,1),NNSOILCARB)  :: ZSOILCARB
REAL, DIMENSION(SIZE(XDG,1),NNLITTLEVS)  :: ZLIGNIN_STRUC
REAL, DIMENSION(SIZE(XDG,1),NNLITTER,NNLITTLEVS)  :: ZLITTER
!
 CHARACTER(LEN=8 ) :: YUNIT
 CHARACTER(LEN=4 ) :: YLVL
INTEGER :: JLAYER, JPATCH, JJ, INI, IWORK, IDEPTH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_ISBA_N:PROVAR_TO_DIAG',0,ZHOOK_HANDLE)
!
INI=SIZE(XDG,1)
!
! * soil temperatures (K)
!
IF(LTEMP_ARP)THEN
  IWORK=NTEMPLAYER_ARP
ELSEIF(CISBA/='DIF')THEN
  IWORK=NGROUND_LAYER-1
ELSE
  IWORK=NGROUND_LAYER
ENDIF
!
ZTG(:,:)=0.0
DO JPATCH=1,NPATCH
   DO JLAYER=1,IWORK
      DO JJ=1,INI 
         ZTG(JJ,JLAYER) = ZTG(JJ,JLAYER) + XPATCH(JJ,JPATCH) * XTG(JJ,JLAYER,JPATCH)
      ENDDO
   ENDDO
ENDDO
!
DO JLAYER=1,IWORK
  WRITE(YLVL,'(I4)') JLAYER
  YRECFM='TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (K)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,ZTG(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
! * Compute soil liquid and ice water content (kg/m2 and m3/m3) 
!
ZWG (:,:)=0.0
ZWGI(:,:)=0.0
ZDG_TOT(:,:)=0.0
ZMOIST (:,:)=XUNDEF
ZICE   (:,:)=XUNDEF
!  
IF(CISBA=='DIF')THEN
  !
  IWORK = NWG_SIZE
  !
  DO JPATCH=1,NPATCH
     DO JLAYER=1,NGROUND_LAYER
        DO JJ=1,INI 
!
!          liquid and ice water content
           IDEPTH=NWG_LAYER(JJ,JPATCH)
           IF(JLAYER<=IDEPTH)THEN    
             ZWG    (JJ,JLAYER)=ZWG    (JJ,JLAYER)+XPATCH(JJ,JPATCH)*XWG (JJ,JLAYER,JPATCH)*XDZG(JJ,JLAYER,JPATCH)
             ZWGI   (JJ,JLAYER)=ZWGI   (JJ,JLAYER)+XPATCH(JJ,JPATCH)*XWGI(JJ,JLAYER,JPATCH)*XDZG(JJ,JLAYER,JPATCH)
             ZDG_TOT(JJ,JLAYER)=ZDG_TOT(JJ,JLAYER)+XPATCH(JJ,JPATCH)*XDZG(JJ,JLAYER,JPATCH)
           ENDIF
!                      
        ENDDO
     ENDDO
  ENDDO
!  
ELSE
  !  
  IWORK = NGROUND_LAYER
  !
  ZDG(:,1,:) = XDG(:,1,:)
  ZDG(:,2,:) = XDG(:,2,:)
  IF(CISBA=='3-L')THEN
    ZDG(:,3,:) = XDG(:,3,:)-XDG(:,2,:)
  ENDIF
!
  DO JPATCH=1,NPATCH
     DO JLAYER=1,NGROUND_LAYER
        DO JJ=1,INI 
           ZWG    (JJ,JLAYER)=ZWG    (JJ,JLAYER)+XPATCH(JJ,JPATCH)*XWG (JJ,JLAYER,JPATCH)*ZDG(JJ,JLAYER,JPATCH)
           ZWGI   (JJ,JLAYER)=ZWGI   (JJ,JLAYER)+XPATCH(JJ,JPATCH)*XWGI(JJ,JLAYER,JPATCH)*ZDG(JJ,JLAYER,JPATCH)
           ZDG_TOT(JJ,JLAYER)=ZDG_TOT(JJ,JLAYER)+XPATCH(JJ,JPATCH)*ZDG(JJ,JLAYER,JPATCH)
        ENDDO
     ENDDO
  ENDDO
!  
ENDIF
!
WHERE(ZDG_TOT(:,:)>0.0)
      ZMOIST(:,:)=ZWG (:,:)*XRHOLW
      ZICE  (:,:)=ZWGI(:,:)*XRHOLW        
      ZWG   (:,:)=ZWG (:,:)/ZDG_TOT(:,:)
      ZWGI  (:,:)=ZWGI(:,:)/ZDG_TOT(:,:)
ELSEWHERE
      ZMOIST(:,:)=XUNDEF
      ZICE  (:,:)=XUNDEF       
      ZWG   (:,:)=XUNDEF
      ZWGI  (:,:)=XUNDEF      
ENDWHERE
!
! * soil liquid water content (m3/m3) and soil moisture (kg/m2)
!
YUNIT=' (m3/m3)'
DO JLAYER=1,IWORK
  WRITE(YLVL,'(I4)') JLAYER
  YRECFM='WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
  YCOMMENT='X_Y_'//YRECFM//YUNIT  
  CALL WRITE_SURF(HPROGRAM,YRECFM,ZWG(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
YUNIT=' (kg/m2)'
DO JLAYER=1,IWORK
  WRITE(YLVL,'(I4)') JLAYER
  YRECFM='SOILM'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
  YCOMMENT='X_Y_'//YRECFM//YUNIT
  CALL WRITE_SURF(HPROGRAM,YRECFM,ZMOIST(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
! * soil ice water content (m3/m3) and soil ice mass (kg/m2)
!
IWORK=NGROUND_LAYER
IF(CISBA/='DIF')THEN
  IWORK=NGROUND_LAYER-1 ! No ice in the FR 3-layers
ENDIF
!
YUNIT=' (m3/m3)'
DO JLAYER=1,IWORK
  WRITE(YLVL,'(I4)') JLAYER
  YRECFM='WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,ZWGI(:,JLAYER),IRESP,HCOMMENT=YCOMMENT) 
END DO   
!
YUNIT=' (kg/m2)'
DO JLAYER=1,IWORK
  WRITE(YLVL,'(I4)') JLAYER
  YRECFM='SOILI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
  YCOMMENT='X_Y_'//YRECFM//YUNIT
  CALL WRITE_SURF(HPROGRAM,YRECFM,ZICE(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
! * water intercepted on leaves (kg/m2)
!
ZWORK(:)=0.0
DO JPATCH=1,NPATCH
  ZWORK(:) = ZWORK(:) + XPATCH(:,JPATCH) * XWR(:,JPATCH)
ENDDO
!
YRECFM='WR_ISBA'
YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Glacier ice storage (semi-prognostic) (kg/m2)
!
IF(LGLACIER)THEN
  !
  ZWORK(:)=0.0
  DO JPATCH=1,NPATCH
    ZWORK(:) = ZWORK(:) + XPATCH(:,JPATCH) * XICE_STO(:,JPATCH)
  ENDDO    
  !
  YRECFM='ICE_STO_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
  !
ENDIF
!
! * Snow albedo (-) 
!
ZPATCH(:) = 0.0
ZWORK (:) = 0.0
DO JPATCH=1,NPATCH
  WHERE(TSNOW%ALB(:,JPATCH)/=XUNDEF)
    ZWORK (:) = ZWORK(:)  + XPATCH(:,JPATCH) * TSNOW%ALB(:,JPATCH)
    ZPATCH(:) = ZPATCH(:) + XPATCH(:,JPATCH)
  ENDWHERE
ENDDO
!
WHERE(ZPATCH(:)>0.0)
  ZWORK(:) = ZWORK(:) / ZPATCH(:)
ELSEWHERE
  ZWORK(:) = XUNDEF
ENDWHERE
!
YRECFM='ASNOW_ISBA'
YCOMMENT='X_Y_'//YRECFM//' (-)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!  
IF(TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO')THEN
  !
  ! * Snow reservoir (kg/m2) by layer
  !
  DO JLAYER = 1,TSNOW%NLAYER
    !
    ZWORK(:)=0.0
    DO JPATCH=1,NPATCH
      ZWORK(:) = ZWORK(:) + XPATCH(:,JPATCH) * TSNOW%WSNOW(:,JLAYER,JPATCH)
    ENDDO
    !
    WRITE(YLVL,'(I4)') JLAYER
    YRECFM='WSNOW_'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDDO
  !
  ! * Snow depth (m)
  !
  DO JLAYER = 1,TSNOW%NLAYER
    !
    ZWORK(:)=0.0
    DO JPATCH=1,NPATCH
      ZWORK(:) = ZWORK(:) + XPATCH(:,JPATCH) * TSNOW%WSNOW(:,JLAYER,JPATCH)/TSNOW%RHO(:,JLAYER,JPATCH)
    ENDDO
    !
    WRITE(YLVL,'(I4)') JLAYER
    YRECFM='DSNOW_'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDDO
  !
  ! * Snow temperature (k)
  !    
  DO JLAYER = 1,TSNOW%NLAYER
    !
    ZWORK (:) = 0.0
    ZPATCH(:) = 0.0
    DO JPATCH=1,NPATCH
      WHERE(TSNOW%WSNOW(:,JLAYER,JPATCH)>0.)
        ZWORK (:) = ZWORK (:) + XPATCH(:,JPATCH) * TSNOW%TEMP(:,JLAYER,JPATCH) 
        ZPATCH(:) = ZPATCH(:) + XPATCH(:,JPATCH)
      ENDWHERE
    ENDDO
    !
    WHERE(ZPATCH(:)>0.0)
      ZWORK(:) = ZWORK(:) / ZPATCH(:)
    ELSEWHERE
      ZWORK(:) = XUNDEF
    ENDWHERE
    !
    WRITE(YLVL,'(I4)') JLAYER
    YRECFM='TSNOW_'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (K)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDDO
  !
ENDIF
!
! * Isba-Ags biomass reservoir
!
! * Isba-Ags biomass reservoir
!
IF(CPHOTO=='NIT'.OR.CPHOTO=='NCB')THEN
!
  ZBIOMASS(:,:)=0.0
  DO JPATCH=1,NPATCH
     DO JLAYER=1,NNBIOMASS
        DO JJ=1,INI 
         ZBIOMASS(JJ,JLAYER) = ZBIOMASS(JJ,JLAYER) + XPATCH(JJ,JPATCH) * XBIOMASS(JJ,JLAYER,JPATCH)
        ENDDO
     ENDDO
  ENDDO
!
  DO JLAYER = 1,NNBIOMASS
    WRITE(YLVL,'(I4)') JLAYER
    YRECFM='BIOM'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kgDM/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,ZBIOMASS(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)  
  ENDDO
!
ENDIF
!
! * Isba-CC carbon reservoir
!
IF(CRESPSL=='CNT')THEN
!
  ZLITTER(:,:,:)=0.0
  ZLIGNIN_STRUC(:,:)=0.0
  DO JPATCH=1,NPATCH
     DO JLAYER=1,NNLITTLEVS
       DO JJ=1,INI 
          ZLITTER(JJ,1,JLAYER) = ZLITTER(JJ,1,JLAYER) + XPATCH(JJ,JPATCH) * XLITTER(JJ,1,JLAYER,JPATCH)
          ZLITTER(JJ,2,JLAYER) = ZLITTER(JJ,2,JLAYER) + XPATCH(JJ,JPATCH) * XLITTER(JJ,2,JLAYER,JPATCH)
          ZLIGNIN_STRUC(JJ,JLAYER) = ZLIGNIN_STRUC(JJ,JLAYER) + XPATCH(JJ,JPATCH) * XLIGNIN_STRUC(JJ,JLAYER,JPATCH)
       ENDDO
    ENDDO
  ENDDO
!       
  DO JLAYER=1,NNLITTLEVS
     WRITE(YLVL,'(I4)') JLAYER
     YRECFM='LIT1_'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
     YCOMMENT='X_Y_'//YRECFM//' (gC/m2)'
     CALL WRITE_SURF(HPROGRAM,YRECFM,ZLITTER(:,1,JLAYER),IRESP,HCOMMENT=YCOMMENT)  
     WRITE(YLVL,'(I4)') JLAYER
     YRECFM='LIT2_'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
     YCOMMENT='X_Y_'//YRECFM//' (gC/m2)'
     CALL WRITE_SURF(HPROGRAM,YRECFM,ZLITTER(:,2,JLAYER),IRESP,HCOMMENT=YCOMMENT)
     WRITE(YLVL,'(I4)') JLAYER
     YRECFM='LIGSTR'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
     YCOMMENT='X_Y_'//YRECFM//' (-)'
     CALL WRITE_SURF(HPROGRAM,YRECFM,ZLIGNIN_STRUC(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)      
  END DO
!
  ZSOILCARB(:,:)=0.0
  DO JPATCH=1,NPATCH
     DO JLAYER=1,NNSOILCARB
       DO JJ=1,INI 
          ZSOILCARB(JJ,JLAYER) = ZSOILCARB(JJ,JLAYER) + XPATCH(JJ,JPATCH) * XSOILCARB(JJ,JLAYER,JPATCH)
       ENDDO
    ENDDO
  ENDDO
!
  DO JLAYER = 1,NNSOILCARB
    WRITE(YLVL,'(I4)') JLAYER
    YRECFM='SCARB'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (gC/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,ZSOILCARB(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)  
  ENDDO
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_ISBA_N:PROVAR_TO_DIAG',1,ZHOOK_HANDLE)
!
END SUBROUTINE PROVAR_TO_DIAG
!
END SUBROUTINE WRITE_DIAG_SEB_ISBA_n
