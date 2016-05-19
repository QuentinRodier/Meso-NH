!     #########
      SUBROUTINE DIAG_TEB_GARDEN_INIT_n(HPROGRAM,KLU,KSW)
!     #####################
!
!!****  *DIAG_TEB_GARDEN_INIT_n* - routine to initialize TEB-ISBA diagnostic variables
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
!               cumulatives variables at the beginning of a run
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,          ONLY : XUNDEF
USE MODD_TEB_VEG_n,         ONLY : CHORT, CPHOTO
USE MODD_TEB_GARDEN_n,      ONLY : NGROUND_LAYER, TSNOW, XABC
USE MODD_TYPE_DATE_SURF
USE MODD_DIAG_SURF_ATM_n,   ONLY : LREAD_BUDGETC
USE MODD_DIAG_TEB_n,        ONLY : N2M, LSURF_BUDGET, LCOEF, LSURF_VARS
USE MODD_DIAG_MISC_TEB_n,   ONLY : LSURF_MISC_BUDGET  
USE MODD_DIAG_TEB_GARDEN_n, ONLY : XRN, XH, XGFLUX, XLEI, XRI, XCD, XCDN, XCH, XCE, &
                                   XTS, XTSRAD, XFAPAR, XFAPIR, XFAPAR_BS, XFAPIR_BS, &
                                   XDFAPARC, XDFAPIRC, XZ0_WITH_SNOW, XZ0H_WITH_SNOW, &
                                   XZ0EFF, XQS, XSWD, XSWU, XSWBD, XSWBU, XLWD, XLWU, &
                                   XFMU, XFMV, XLEG, XLEGI, XLEV, XLES, XLER, XLETR, &
                                   XEVAP, XDRAIN, XRUNOFF, XHORT, XDRIP, XMELT, XRRVEG, &
                                   XHV,  XSWI, XTSWI, XTWSNOW, XTDSNOW, XSEUIL, XGPP, &
                                   XRESP_AUTO, XRESP_ECO, XALBT, XEMIST, XCG, XC1, XC2, &
                                   XWGEQ, XCT, XRS, XHU, XHUG, XRESTORE, XUSTAR, XDLAI_EFFC, & 
                                   XIACAN, XSNOWTEMP, XSNOWLIQ, XSNOWDZ, XSNOWHMASS,        &
                                   XMELTADV, XIACAN, XIRRIG_FLUX  
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(IN)         :: KLU       ! size of arrays
INTEGER, INTENT(IN)         :: KSW       ! spectral bands
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
IF (LHOOK) CALL DR_HOOK('DIAG_TEB_GARDEN_INIT_N',0,ZHOOK_HANDLE)
!
ALLOCATE(XRI     (KLU)) 
!
XRI         = XUNDEF
!
ALLOCATE(XCD     (KLU)) 
ALLOCATE(XCH     (KLU)) 
ALLOCATE(XRN     (KLU)) 
ALLOCATE(XH      (KLU)) 
ALLOCATE(XGFLUX  (KLU)) 
ALLOCATE(XQS     (KLU)) 
!
XCD         = XUNDEF
XCH         = XUNDEF
XRN         = XUNDEF
XH          = XUNDEF
XGFLUX      = XUNDEF
XQS         = XUNDEF
!
ALLOCATE(XLEI    (KLU)) 
ALLOCATE(XLEG    (KLU)) 
ALLOCATE(XLEGI   (KLU)) 
ALLOCATE(XLEV    (KLU)) 
ALLOCATE(XLES    (KLU)) 
ALLOCATE(XLER    (KLU)) 
ALLOCATE(XLETR   (KLU)) 
ALLOCATE(XEVAP   (KLU)) 
ALLOCATE(XDRAIN  (KLU)) 
ALLOCATE(XRUNOFF (KLU)) 
ALLOCATE(XHORT   (KLU)) 
ALLOCATE(XDRIP   (KLU)) 
ALLOCATE(XRRVEG  (KLU)) 
ALLOCATE(XMELT   (KLU)) 
ALLOCATE(XIRRIG_FLUX(KLU))
!
XLEI        = XUNDEF
XLEG        = XUNDEF
XLEGI       = XUNDEF
XLEV        = XUNDEF
XLES        = XUNDEF
XLER        = XUNDEF
XLETR       = XUNDEF
XEVAP       = XUNDEF
XDRAIN      = XUNDEF
XRUNOFF     = XUNDEF
XHORT       = XUNDEF
XDRIP       = XUNDEF
XRRVEG      = XUNDEF
XMELT       = XUNDEF
XIRRIG_FLUX = XUNDEF
!
ALLOCATE(XCG     (KLU)) 
ALLOCATE(XC1     (KLU)) 
ALLOCATE(XC2     (KLU)) 
ALLOCATE(XWGEQ   (KLU)) 
ALLOCATE(XCT     (KLU)) 
ALLOCATE(XRS     (KLU)) 
ALLOCATE(XCDN    (KLU)) 
ALLOCATE(XHU     (KLU)) 
ALLOCATE(XHUG    (KLU)) 
ALLOCATE(XRESTORE(KLU)) 
ALLOCATE(XUSTAR  (KLU)) 
IF (CPHOTO/='NON') THEN
  ALLOCATE(XIACAN  (KLU,SIZE(XABC)          ))
ELSE
  ALLOCATE(XIACAN  (0,0))
END IF
!
XCG         = XUNDEF
XC1         = XUNDEF
XC2         = XUNDEF
XWGEQ       = XUNDEF
XCT         = XUNDEF
XRS         = XUNDEF
XCDN        = XUNDEF
XHU         = XUNDEF
XHUG        = XUNDEF
XRESTORE    = XUNDEF
XUSTAR      = XUNDEF
IF (CPHOTO/='NON') THEN
  XIACAN    = XUNDEF
END IF
!
ALLOCATE(XSNOWTEMP(KLU,TSNOW%NLAYER        )) 
ALLOCATE(XSNOWLIQ (KLU,TSNOW%NLAYER        )) 
ALLOCATE(XSNOWDZ  (KLU,TSNOW%NLAYER        )) 
ALLOCATE(XSNOWHMASS(KLU)) 
ALLOCATE(XMELTADV  (KLU)) 
!
XSNOWTEMP   = XUNDEF
XSNOWLIQ    = XUNDEF
XSNOWDZ     = XUNDEF
XSNOWHMASS  = XUNDEF
XMELTADV    = XUNDEF
!
ALLOCATE(XHV     (KLU))
ALLOCATE(XALBT   (KLU)) 
ALLOCATE(XEMIST  (KLU)) 
!
XHV               = XUNDEF
XALBT             = XUNDEF
XEMIST            = XUNDEF
!
ALLOCATE(XFAPAR    (KLU))
ALLOCATE(XFAPIR    (KLU))
ALLOCATE(XFAPAR_BS (KLU))
ALLOCATE(XFAPIR_BS (KLU))
ALLOCATE(XDFAPARC  (KLU))
ALLOCATE(XDFAPIRC  (KLU))
ALLOCATE(XDLAI_EFFC(KLU))
!
XFAPAR     = XUNDEF
XFAPIR     = XUNDEF
XFAPAR_BS  = XUNDEF
XFAPIR_BS  = XUNDEF
XDFAPARC   = XUNDEF
XDFAPIRC   = XUNDEF
XDLAI_EFFC = XUNDEF
!
!* surface energy budget
!
!IF (LSURF_BUDGET) THEN
  !
  ALLOCATE(XSWD      (KLU))
  ALLOCATE(XSWU      (KLU))
  ALLOCATE(XSWBD     (KLU,KSW))
  ALLOCATE(XSWBU     (KLU,KSW))
  ALLOCATE(XLWD      (KLU))
  ALLOCATE(XLWU      (KLU))
  ALLOCATE(XFMU      (KLU))
  ALLOCATE(XFMV      (KLU))
  !
  XSWD     = XUNDEF
  XSWU     = XUNDEF
  XSWBD    = XUNDEF
  XSWBU    = XUNDEF
  XLWD     = XUNDEF
  XLWU     = XUNDEF
  XFMU     = XUNDEF
  XFMV     = XUNDEF
  !
!END IF
!
!* surface temperature and parameters at 2m
!
ALLOCATE(XTS    (KLU))
XTS     = XUNDEF
ALLOCATE(XTSRAD (KLU))
XTSRAD  = XUNDEF
!
!* miscellaneous surface fields
!
IF (LSURF_MISC_BUDGET) THEN
  ALLOCATE(XSWI    (KLU,NGROUND_LAYER))
  ALLOCATE(XTSWI   (KLU,NGROUND_LAYER))
  ALLOCATE(XTWSNOW (KLU))
  ALLOCATE(XTDSNOW (KLU))
  XSWI     = XUNDEF
  XTSWI    = XUNDEF
  XTWSNOW  = XUNDEF
  XTDSNOW  = XUNDEF
ELSE
  ALLOCATE(XSWI    (0,0))
  ALLOCATE(XTSWI   (0,0))
  ALLOCATE(XTWSNOW (0))
  ALLOCATE(XTDSNOW (0))
ENDIF
!
ALLOCATE(XALBT   (KLU))
ALLOCATE(XGPP    (KLU))
ALLOCATE(XRESP_AUTO  (KLU))
ALLOCATE(XRESP_ECO   (KLU))
!
XALBT    = XUNDEF
XGPP     = XUNDEF
XRESP_AUTO   = XUNDEF
XRESP_ECO    = XUNDEF  
!
!END IF
!
!* transfer coefficients
!
!IF (LCOEF) THEN
  !
  ALLOCATE(XCE            (KLU))
  ALLOCATE(XZ0_WITH_SNOW  (KLU))
  ALLOCATE(XZ0H_WITH_SNOW (KLU))
  ALLOCATE(XZ0EFF         (KLU))
  !
  XCE            = XUNDEF
  XZ0_WITH_SNOW  = XUNDEF
  XZ0H_WITH_SNOW = XUNDEF
  XZ0EFF         = XUNDEF
!END IF
!
!
!* surface humidity
!
!IF (LSURF_VARS) THEN
  ALLOCATE(XQS            (KLU))
  !
  XQS            = XUNDEF
!END IF
!
!* Irrigation threshold
!
!IF (LAGRIP) THEN
  ALLOCATE(XSEUIL(KLU))
  !
  XSEUIL         = XUNDEF
!END IF
!
IF (LHOOK) CALL DR_HOOK('DIAG_TEB_GARDEN_INIT_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_TEB_GARDEN_INIT_n
