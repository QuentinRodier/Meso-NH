!     #########
      SUBROUTINE DIAG_TEB_GREENROOF_INIT_n(HPROGRAM,KLU,KSW)
!     #####################
!
!!****  *DIAG_TEB_GREENROOF_INIT_n* - routine to initialize TEB-ISBA diagnostic variables
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!    Based on diag_teb_garden_initn
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
!!      Original                  02/2003 
!!      modified                  11/2003  by P. LeMoigne: surface cumulated energy budget
!!      modified                  10/2004  by P. LeMoigne: surface miscellaneous fields
!!      B. Decharme               2008     New diag for water budget and allow to reset
!!                                         cumulatives variables at the beginning of a run
!       C. de Munck & A. Lemonsu  09/2011  Greenroofs
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,             ONLY : XUNDEF
USE MODD_TEB_VEG_n,            ONLY : CPHOTO, CHORT
USE MODD_TEB_GREENROOF_n,      ONLY : NLAYER_GR
USE MODD_TYPE_DATE_SURF
USE MODD_DIAG_SURF_ATM_n,      ONLY : LREAD_BUDGETC
USE MODD_DIAG_TEB_n,           ONLY : N2M, LSURF_BUDGET, LCOEF, LSURF_VARS
USE MODD_DIAG_MISC_TEB_n,      ONLY : LSURF_EVAP_BUDGET, LSURF_MISC_BUDGET  
USE MODD_DIAG_TEB_GREENROOF_n, ONLY : XRN, XH, XGFLUX, XLEI, XRI, XCD, XCDN, XCH, XCE, &
                                      XTS, XTSRAD,                                     &
                                      XZ0_WITH_SNOW, XZ0H_WITH_SNOW, XZ0EFF, XQS,      &
                                      XSWD, XSWU, XSWBD, XSWBU, XLWD, XLWU, XFMU, XFMV,&
                                      XLEG, XLEGI, XLEV, XLES, XLER, XLETR, XEVAP,     &
                                      XDRAIN, XRUNOFF, XHORT, XDRIP, XMELT,            &
                                      XRRVEG, XHV,  XSWI, XTSWI, XTWSNOW,              &
                                      XTDSNOW, XSEUIL, XGPP, XRESP_AUTO, XRESP_ECO,    &
                                      XALBT, XEMIST, XALBT,                            &
                                      XCG, XC1, XC2, XWGEQ, XCT, XRS, XHU, XHUG,       &
                                      XRESTORE, XUSTAR,                                &
                                      XSNOWTEMP, XSNOWLIQ, XSNOWDZ, XSNOWHMASS,        &
                                      XMELTADV, XIACAN  
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
IF (LHOOK) CALL DR_HOOK('DIAG_TEB_GREENROOF_INIT_N',0,ZHOOK_HANDLE)
XCG         = XUNDEF
XC1         = XUNDEF
XC2         = XUNDEF
XWGEQ       = XUNDEF
XCT         = XUNDEF
XRS         = XUNDEF
XHU         = XUNDEF
XHUG        = XUNDEF
XHV         = XUNDEF
XRESTORE    = XUNDEF
XRI         = XUNDEF
XUSTAR      = XUNDEF
XRN         = XUNDEF
XH          = XUNDEF
XGFLUX      = XUNDEF
XSNOWTEMP   = XUNDEF
XSNOWLIQ    = XUNDEF
XSNOWDZ     = XUNDEF
XSNOWHMASS  = XUNDEF
XMELTADV    = XUNDEF
IF (CPHOTO/='NON') THEN
  XIACAN    = XUNDEF
END IF
XCD         = XUNDEF
XCDN        = XUNDEF
XCH         = XUNDEF
XQS         = XUNDEF
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
XALBT       = XUNDEF
XEMIST      = XUNDEF
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
  !
  ALLOCATE(XSWI    (KLU,NLAYER_GR))
  ALLOCATE(XTSWI   (KLU,NLAYER_GR))
  ALLOCATE(XTWSNOW (KLU))
  ALLOCATE(XTDSNOW (KLU))
  XSWI     = XUNDEF
  XTSWI    = XUNDEF
  XTWSNOW  = XUNDEF
  XTDSNOW  = XUNDEF
ENDIF


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
IF (LHOOK) CALL DR_HOOK('DIAG_TEB_GREENROOF_INIT_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_TEB_GREENROOF_INIT_n
