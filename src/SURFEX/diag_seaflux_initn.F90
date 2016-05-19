!     #########
      SUBROUTINE DIAG_SEAFLUX_INIT_n(HPROGRAM,KLU,KSW)
!     #####################
!
!!****  *DIAG_SEAFLUX_INIT_n* - routine to initialize SEAFLUX diagnostic variables
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
!!      Original    01/2004 
!!      Modified    01/2006 : sea flux parameterization.
!!      Modified    08/2009 : cumulative sea flux 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SURF_ATM,       ONLY : LCPL_ESM
USE MODD_DIAG_SURF_ATM_n,ONLY : LREAD_BUDGETC
USE MODD_DIAG_SEAFLUX_n, ONLY : N2M, LSURF_BUDGET, LCOEF, LSURF_VARS,     &
                                  LSURF_BUDGETC, LRESET_BUDGETC,            &
                                  XRN, XH, XLE, XLEI, XGFLUX,               &
                                  XRI, XCD, XCH, XCE, XZ0, XZ0H, XT2M,      &
                                  XQ2M, XHU2M, XZON10M, XMER10M, XQS, XSWD, &
                                  XSWU, XLWD, XLWU, XT2M_MIN, XT2M_MAX,     &
                                  XSWBD, XSWBU, XFMU, XFMV, XDIAG_SST,      &
                                  XRNC, XHC, XLEC, XLEIC, XGFLUXC,          &
                                  XSWDC, XSWUC, XLWDC, XLWUC, XFMUC, XFMVC, &
                                  XHU2M_MIN, XHU2M_MAX, XWIND10M, XWIND10M_MAX  
!                                
USE MODD_DIAG_OCEAN_n,   ONLY : LDIAG_OCEAN, XTOCMOY, XSOCMOY, XUOCMOY,   &
                                  XVOCMOY, XDOCMOY  
!
USE MODD_SEAFLUX_n,      ONLY : XCPL_SEA_WIND,                 &
                                  XCPL_SEA_EVAP,XCPL_SEA_HEAT,   &
                                  XCPL_SEA_SNET,XCPL_SEA_FWSU,   &
                                  XCPL_SEA_FWSV,XCPL_SEA_RAIN,   &
                                  XCPL_SEA_SNOW,XCPL_SEA_FWSM,   &
                                  XCPL_SEAICE_EVAP,              &
                                  XCPL_SEAICE_HEAT,              &
                                  XCPL_SEAICE_SNET  
!
USE MODI_READ_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(IN) :: KLU   ! size of arrays
INTEGER, INTENT(IN) :: KSW   ! number of SW spectral bands
 CHARACTER(LEN=6), INTENT(IN):: HPROGRAM  ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YREC           ! Name of the article to be read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* surface energy budget
!
IF (LHOOK) CALL DR_HOOK('DIAG_SEAFLUX_INIT_N',0,ZHOOK_HANDLE)
ALLOCATE(XDIAG_SST(KLU))
XDIAG_SST = XUNDEF
!
IF (LSURF_BUDGET.OR.LSURF_BUDGETC) THEN
  ALLOCATE(XRN     (KLU))
  ALLOCATE(XH      (KLU))
  ALLOCATE(XLE     (KLU))
  ALLOCATE(XLEI    (KLU))
  ALLOCATE(XGFLUX  (KLU))
  ALLOCATE(XSWD    (KLU))
  ALLOCATE(XSWU    (KLU))
  ALLOCATE(XLWD    (KLU))
  ALLOCATE(XLWU    (KLU))
  ALLOCATE(XSWBD   (KLU,KSW))
  ALLOCATE(XSWBU   (KLU,KSW))
  ALLOCATE(XFMU    (KLU))
  ALLOCATE(XFMV    (KLU))
  !
  XRN      = XUNDEF
  XH       = XUNDEF
  XLE      = XUNDEF
  XLEI     = XUNDEF
  XGFLUX   = XUNDEF
  XSWD     = XUNDEF
  XSWU     = XUNDEF
  XLWD     = XUNDEF
  XLWU     = XUNDEF
  XSWBD    = XUNDEF
  XSWBU    = XUNDEF
  XFMU     = XUNDEF
  XFMV     = XUNDEF
  !
ELSE
  ALLOCATE(XRN     (0))
  ALLOCATE(XH      (0))
  ALLOCATE(XLE     (0))
  ALLOCATE(XLEI    (0))
  ALLOCATE(XGFLUX  (0))
  ALLOCATE(XSWD    (0))
  ALLOCATE(XSWU    (0))
  ALLOCATE(XLWD    (0))
  ALLOCATE(XLWU    (0))
  ALLOCATE(XSWBD   (0,0))
  ALLOCATE(XSWBU   (0,0))
  ALLOCATE(XFMU    (0))
  ALLOCATE(XFMV    (0))
ENDIF
!
!* cumulative surface energy budget
!
IF (LSURF_BUDGETC) THEN
!        
  ALLOCATE(XRNC    (KLU))
  ALLOCATE(XHC     (KLU))
  ALLOCATE(XLEC    (KLU))
  ALLOCATE(XLEIC   (KLU))
  ALLOCATE(XGFLUXC (KLU))
  ALLOCATE(XSWDC   (KLU))
  ALLOCATE(XSWUC   (KLU))
  ALLOCATE(XLWDC   (KLU))
  ALLOCATE(XLWUC   (KLU))
  ALLOCATE(XFMUC   (KLU))
  ALLOCATE(XFMVC   (KLU))
!
  IF (.NOT. LREAD_BUDGETC) THEN        
     XRNC    = 0.0
     XHC     = 0.0
     XLEC    = 0.0
     XLEIC   = 0.0
     XGFLUXC = 0.0
     XSWDC   = 0.0
     XSWUC   = 0.0
     XLWDC   = 0.0
     XLWUC   = 0.0
     XFMUC   = 0.0
     XFMVC   = 0.0
  ELSEIF (LREAD_BUDGETC.AND.LRESET_BUDGETC) THEN
     XRNC    = 0.0
     XHC     = 0.0
     XLEC    = 0.0
     XLEIC   = 0.0
     XGFLUXC = 0.0
     XSWDC   = 0.0
     XSWUC   = 0.0
     XLWDC   = 0.0
     XLWUC   = 0.0
     XFMUC   = 0.0
     XFMVC   = 0.0
  ELSE
     YREC='RNC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,XRNC,IRESP)
     YREC='HC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,XHC ,IRESP)
     YREC='LEC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,XLEC,IRESP)
     YREC='LEIC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,XLEIC,IRESP)     
     YREC='GFLUXC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,XGFLUXC ,IRESP)
     YREC='SWDC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,XSWDC,IRESP)
     YREC='SWUC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,XSWUC,IRESP)
     YREC='LWDC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,XLWDC,IRESP)
     YREC='LWUC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,XLWUC,IRESP)
     YREC='FMUC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,XFMUC,IRESP)
     YREC='FMVC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,XFMVC,IRESP)
  ENDIF   
ELSE
  ALLOCATE(XRNC    (0))
  ALLOCATE(XHC     (0))
  ALLOCATE(XLEC    (0))
  ALLOCATE(XLEIC   (0))
  ALLOCATE(XGFLUXC (0))
  ALLOCATE(XSWDC   (0))
  ALLOCATE(XSWUC   (0))
  ALLOCATE(XLWDC   (0))
  ALLOCATE(XLWUC   (0))
  ALLOCATE(XFMUC   (0))
  ALLOCATE(XFMVC   (0))
ENDIF
!
!* parameters at 2m
!
IF (N2M>=1) THEN
  ALLOCATE(XRI      (KLU))
  ALLOCATE(XT2M     (KLU))
  ALLOCATE(XT2M_MIN (KLU))
  ALLOCATE(XT2M_MAX (KLU))
  ALLOCATE(XQ2M     (KLU))
  ALLOCATE(XHU2M    (KLU))
  ALLOCATE(XHU2M_MIN(KLU))
  ALLOCATE(XHU2M_MAX(KLU))
  ALLOCATE(XZON10M  (KLU))
  ALLOCATE(XMER10M  (KLU))
  ALLOCATE(XWIND10M (KLU))
  ALLOCATE(XWIND10M_MAX(KLU))
  !
  XRI      = XUNDEF
  XT2M     = XUNDEF
  XT2M_MIN = XUNDEF
  XT2M_MAX = 0.0
  XQ2M     = XUNDEF
  XHU2M    = XUNDEF
  XHU2M_MIN= XUNDEF
  XHU2M_MAX=-XUNDEF
  XZON10M  = XUNDEF
  XMER10M  = XUNDEF
  XWIND10M = XUNDEF
  XWIND10M_MAX = 0.0
ELSE
  ALLOCATE(XRI      (0))
  ALLOCATE(XT2M     (0))
  ALLOCATE(XT2M_MIN (0))
  ALLOCATE(XT2M_MAX (0))
  ALLOCATE(XQ2M     (0))
  ALLOCATE(XHU2M    (0))
  ALLOCATE(XHU2M_MIN(0))
  ALLOCATE(XHU2M_MAX(0))
  ALLOCATE(XZON10M  (0))
  ALLOCATE(XMER10M  (0))
  ALLOCATE(XWIND10M (0))
  ALLOCATE(XWIND10M_MAX(0))
END IF
!
!* transfer coefficients
!
IF (LCOEF) THEN
  ALLOCATE(XCD     (KLU))
  ALLOCATE(XCH     (KLU))
  ALLOCATE(XCE     (KLU))
  ALLOCATE(XZ0     (KLU))
  ALLOCATE(XZ0H    (KLU))
  !
  XCD      = XUNDEF
  XCH      = XUNDEF
  XCE      = XUNDEF
  XZ0      = XUNDEF
  XZ0H     = XUNDEF
ELSE
  ALLOCATE(XCD     (0))
  ALLOCATE(XCH     (0))
  ALLOCATE(XCE     (0))
  ALLOCATE(XZ0     (0))
  ALLOCATE(XZ0H    (0))
END IF
!
!
!* surface humidity
!
IF (LSURF_VARS) THEN
  ALLOCATE(XQS     (KLU))
  !
  XQS      = XUNDEF
ELSE
  ALLOCATE(XQS     (0))
END IF
!
!* ocean diag
!
IF (LDIAG_OCEAN) THEN
  ALLOCATE(XTOCMOY  (KLU))
  ALLOCATE(XSOCMOY  (KLU))
  ALLOCATE(XUOCMOY  (KLU))
  ALLOCATE(XVOCMOY  (KLU))
  ALLOCATE(XDOCMOY  (KLU))
  !
  XTOCMOY(:)=XUNDEF
  XSOCMOY(:)=XUNDEF
  XUOCMOY(:)=XUNDEF
  XVOCMOY(:)=XUNDEF
  XDOCMOY(:)=XUNDEF
ELSE
  ALLOCATE(XTOCMOY  (0))
  ALLOCATE(XSOCMOY  (0))
  ALLOCATE(XUOCMOY  (0))
  ALLOCATE(XVOCMOY  (0))
  ALLOCATE(XDOCMOY  (0))
ENDIF
!
!* Earth system model coupling variables
!
IF(LCPL_ESM)THEN
!        
  ALLOCATE(XCPL_SEA_WIND(KLU))
  ALLOCATE(XCPL_SEA_FWSU(KLU))
  ALLOCATE(XCPL_SEA_FWSV(KLU))
  ALLOCATE(XCPL_SEA_SNET(KLU))
  ALLOCATE(XCPL_SEA_HEAT(KLU))
  ALLOCATE(XCPL_SEA_EVAP(KLU))
  ALLOCATE(XCPL_SEA_RAIN(KLU))
  ALLOCATE(XCPL_SEA_SNOW(KLU))
  ALLOCATE(XCPL_SEA_FWSM(KLU))
  XCPL_SEA_WIND(:) = 0.0
  XCPL_SEA_FWSU(:) = 0.0
  XCPL_SEA_FWSV(:) = 0.0
  XCPL_SEA_SNET(:) = 0.0
  XCPL_SEA_HEAT(:) = 0.0
  XCPL_SEA_EVAP(:) = 0.0
  XCPL_SEA_RAIN(:) = 0.0
  XCPL_SEA_SNOW(:) = 0.0
  XCPL_SEA_FWSM(:) = 0.0
!  
  ALLOCATE(XCPL_SEAICE_SNET(KLU))
  ALLOCATE(XCPL_SEAICE_HEAT(KLU))
  ALLOCATE(XCPL_SEAICE_EVAP(KLU))
  XCPL_SEAICE_SNET(:) = 0.0
  XCPL_SEAICE_HEAT(:) = 0.0
  XCPL_SEAICE_EVAP(:) = 0.0
!
ELSE
  ALLOCATE(XCPL_SEA_WIND(0))
  ALLOCATE(XCPL_SEA_FWSU(0))
  ALLOCATE(XCPL_SEA_FWSV(0))
  ALLOCATE(XCPL_SEA_SNET(0))
  ALLOCATE(XCPL_SEA_HEAT(0))
  ALLOCATE(XCPL_SEA_EVAP(0))
  ALLOCATE(XCPL_SEA_RAIN(0))
  ALLOCATE(XCPL_SEA_SNOW(0))
  ALLOCATE(XCPL_SEA_FWSM(0))
!
  ALLOCATE(XCPL_SEAICE_SNET(0))
  ALLOCATE(XCPL_SEAICE_HEAT(0))
  ALLOCATE(XCPL_SEAICE_EVAP(0))
ENDIF
IF (LHOOK) CALL DR_HOOK('DIAG_SEAFLUX_INIT_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SEAFLUX_INIT_n
