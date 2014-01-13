!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE DIAG_IDEAL_INIT_n(KLU,KSW)
!     #####################
!
!!****  *DIAG_IDEAL_INIT_n* - routine to initialize IDEAL diagnostic variables
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
!!	P. Le Moigne   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2009 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_DIAG_IDEAL_n, ONLY : N2M, LSURF_BUDGET, LCOEF, LSURF_VARS, &
                              XRN, XH, XLE, XGFLUX, XRI,            &
                              XCD, XCH, XCE, XZ0, XZ0H,             &
                              XT2M, XQ2M, XHU2M,                    &
                              XZON10M, XMER10M, XQS,                &
                              XSWD, XSWU, XSWBD, XSWBU, XLWD, XLWU, &
                              XFMU, XFMV  
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
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
!* surface energy budget
!
IF (LHOOK) CALL DR_HOOK('DIAG_IDEAL_INIT_N',0,ZHOOK_HANDLE)
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
ELSE
  ALLOCATE(XRN     (0))
  ALLOCATE(XH      (0))
  ALLOCATE(XLE     (0))
  ALLOCATE(XGFLUX  (0))
  ALLOCATE(XSWD    (0))
  ALLOCATE(XSWU    (0))
  ALLOCATE(XLWD    (0))
  ALLOCATE(XLWU    (0))
  ALLOCATE(XSWBD   (0,0))
  ALLOCATE(XSWBU   (0,0))
  ALLOCATE(XFMU    (0))
  ALLOCATE(XFMV    (0))
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
  ALLOCATE(XRI     (0))
  ALLOCATE(XT2M    (0))
  ALLOCATE(XQ2M    (0))
  ALLOCATE(XHU2M   (0))
  ALLOCATE(XZON10M (0))
  ALLOCATE(XMER10M (0))
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
IF (LHOOK) CALL DR_HOOK('DIAG_IDEAL_INIT_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_IDEAL_INIT_n
