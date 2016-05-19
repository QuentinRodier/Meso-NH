!     #########
      SUBROUTINE DIAG_TEB_INIT_n(HPROGRAM,KLU,KSW)
!     #####################
!
!!****  *DIAG_TEB_INIT_n* - routine to initialize TEB diagnostic variables
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_TYPE_DATE_SURF
USE MODD_TEB_n,      ONLY : CBEM
USE MODD_DIAG_TEB_n, ONLY : N2M, LSURF_BUDGET, LCOEF, LSURF_VARS, &
                              XRN, XH, XLE, XGFLUX, XRI,            &
                              XCD, XCH, XCE, XZ0, XZ0H,             &
                              XT2M, XQ2M, XHU2M,                    &
                              XZON10M, XMER10M, XSFCO2, XQS,        &
                              XSWD, XSWU, XSWBD, XSWBU, XLWD, XLWU, &
                              XFMU, XFMV  
!
USE MODD_DIAG_UTCI_TEB_n,   ONLY : XUTCI_OUTSUN, XUTCI_OUTSHADE, XTRAD_SUN, &
                                   XTRAD_SHADE, XUTCI_IN, LUTCI

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
INTEGER, INTENT(IN) :: KSW   ! spectral bands
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
IF (LHOOK) CALL DR_HOOK('DIAG_TEB_INIT_N',0,ZHOOK_HANDLE)
IF (LSURF_BUDGET) THEN
  ALLOCATE(XRN     (KLU))
  ALLOCATE(XH      (KLU))
  ALLOCATE(XLE     (KLU))
  ALLOCATE(XGFLUX  (KLU))
  ALLOCATE(XSWD    (KLU))
  ALLOCATE(XSWU    (KLU))
  ALLOCATE(XSWBD   (KLU,KSW))
  ALLOCATE(XSWBU   (KLU,KSW))
  ALLOCATE(XLWD    (KLU))
  ALLOCATE(XLWU    (KLU))
  ALLOCATE(XFMU    (KLU))
  ALLOCATE(XFMV    (KLU))
  ALLOCATE(XSFCO2  (KLU))
  !
  XRN      = XUNDEF
  XH       = XUNDEF
  XLE      = XUNDEF
  XGFLUX   = XUNDEF
  XSWD     = XUNDEF
  XSWU     = XUNDEF
  XSWBD    = XUNDEF
  XSWBU    = XUNDEF
  XLWD     = XUNDEF
  XLWU     = XUNDEF
  XFMU     = XUNDEF
  XFMV     = XUNDEF
  XSFCO2   = XUNDEF
ELSE
  ALLOCATE(XRN     (0))
  ALLOCATE(XH      (0))
  ALLOCATE(XLE     (0))
  ALLOCATE(XGFLUX  (0))
  ALLOCATE(XSWD    (0))
  ALLOCATE(XSWU    (0))
  ALLOCATE(XSWBD   (0,0))
  ALLOCATE(XSWBU   (0,0))  
  ALLOCATE(XLWD    (0))
  ALLOCATE(XLWU    (0))
  ALLOCATE(XFMU    (0))
  ALLOCATE(XFMV    (0))
  ALLOCATE(XSFCO2  (0))
END IF
!
!* parameters at 2m
!
IF (N2M>=1) THEN
  ALLOCATE(XRI     (KLU))
  ALLOCATE(XT2M    (KLU))
  ALLOCATE(XQ2M    (KLU))
  ALLOCATE(XHU2M   (KLU))
  ALLOCATE(XZON10M (KLU))
  ALLOCATE(XMER10M (KLU))
  !
  XRI      = XUNDEF
  XT2M     = XUNDEF
  XQ2M     = XUNDEF
  XHU2M    = XUNDEF
  XZON10M  = XUNDEF
  XMER10M  = XUNDEF
ELSE
  ALLOCATE(XRI      (0))
  ALLOCATE(XT2M     (0))
  ALLOCATE(XQ2M     (0))
  ALLOCATE(XHU2M    (0))
  ALLOCATE(XZON10M  (0))
  ALLOCATE(XMER10M  (0))  
END IF
!!
!* miscellaneous fields
!
IF (N2M>0 .AND. LUTCI) THEN
  !
  ALLOCATE(XUTCI_IN       (KLU))
  ALLOCATE(XUTCI_OUTSUN   (KLU))
  ALLOCATE(XUTCI_OUTSHADE (KLU))
  ALLOCATE(XTRAD_SUN      (KLU))
  ALLOCATE(XTRAD_SHADE    (KLU))
  !
  XUTCI_IN        = XUNDEF
  XUTCI_OUTSUN    = XUNDEF
  XUTCI_OUTSHADE  = XUNDEF
  XTRAD_SUN       = XUNDEF
  XTRAD_SHADE     = XUNDEF
  !  
ELSE
  ALLOCATE(XUTCI_IN       (0))
  ALLOCATE(XUTCI_OUTSUN   (0))
  ALLOCATE(XUTCI_OUTSHADE (0))
  ALLOCATE(XTRAD_SUN      (0))
  ALLOCATE(XTRAD_SHADE    (0))        
ENDIF
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
IF (LHOOK) CALL DR_HOOK('DIAG_TEB_INIT_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_TEB_INIT_n
