!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DIAG_TEB_INIT_n (DGT, DGUT, &
                                  HPROGRAM,KLU,KSW)
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!       V. Masson  10/2013 Adds integrated UTCI diagnostics
!       B. decharme 04/2013 : Add DIAG_TS
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DIAG_TEB_n, ONLY : DIAG_TEB_t
USE MODD_DIAG_UTCI_TEB_n, ONLY : DIAG_UTCI_TEB_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_TYPE_DATE_SURF
!
USE MODD_UTCI,              ONLY : NUTCI_STRESS

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
!
TYPE(DIAG_TEB_t), INTENT(INOUT) :: DGT
TYPE(DIAG_UTCI_TEB_t), INTENT(INOUT) :: DGUT
!
INTEGER, INTENT(IN) :: KLU   ! size of arrays
INTEGER, INTENT(IN) :: KSW   ! spectral bands
 CHARACTER(LEN=6), INTENT(IN):: HPROGRAM  ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=LEN_HREC) :: YREC           ! Name of the article to be read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* surface energy budget
!
IF (LHOOK) CALL DR_HOOK('DIAG_TEB_INIT_N',0,ZHOOK_HANDLE)
!
ALLOCATE(DGT%XDIAG_TS(KLU))
DGT%XDIAG_TS = XUNDEF
!
IF (DGT%LSURF_BUDGET) THEN
  ALLOCATE(DGT%XRN     (KLU))
  ALLOCATE(DGT%XH      (KLU))
  ALLOCATE(DGT%XLE     (KLU))
  ALLOCATE(DGT%XGFLUX  (KLU))
  ALLOCATE(DGT%XSWD    (KLU))
  ALLOCATE(DGT%XSWU    (KLU))
  ALLOCATE(DGT%XSWBD   (KLU,KSW))
  ALLOCATE(DGT%XSWBU   (KLU,KSW))
  ALLOCATE(DGT%XLWD    (KLU))
  ALLOCATE(DGT%XLWU    (KLU))
  ALLOCATE(DGT%XFMU    (KLU))
  ALLOCATE(DGT%XFMV    (KLU))
  ALLOCATE(DGT%XSFCO2  (KLU))
  !
  DGT%XRN      = XUNDEF
  DGT%XH       = XUNDEF
  DGT%XLE      = XUNDEF
  DGT%XGFLUX   = XUNDEF
  DGT%XSWD     = XUNDEF
  DGT%XSWU     = XUNDEF
  DGT%XSWBD    = XUNDEF
  DGT%XSWBU    = XUNDEF
  DGT%XLWD     = XUNDEF
  DGT%XLWU     = XUNDEF
  DGT%XFMU     = XUNDEF
  DGT%XFMV     = XUNDEF
  DGT%XSFCO2   = XUNDEF
ELSE
  ALLOCATE(DGT%XRN     (0))
  ALLOCATE(DGT%XH      (0))
  ALLOCATE(DGT%XLE     (0))
  ALLOCATE(DGT%XGFLUX  (0))
  ALLOCATE(DGT%XSWD    (0))
  ALLOCATE(DGT%XSWU    (0))
  ALLOCATE(DGT%XSWBD   (0,0))
  ALLOCATE(DGT%XSWBU   (0,0))  
  ALLOCATE(DGT%XLWD    (0))
  ALLOCATE(DGT%XLWU    (0))
  ALLOCATE(DGT%XFMU    (0))
  ALLOCATE(DGT%XFMV    (0))
  ALLOCATE(DGT%XSFCO2  (0))
END IF
!
!* parameters at 2m
!
IF (DGT%N2M>=1) THEN
  ALLOCATE(DGT%XRI     (KLU))
  ALLOCATE(DGT%XT2M    (KLU))
  ALLOCATE(DGT%XT2M_MIN (KLU))
  ALLOCATE(DGT%XT2M_MAX (KLU))
  ALLOCATE(DGT%XQ2M    (KLU))
  ALLOCATE(DGT%XHU2M   (KLU))
  ALLOCATE(DGT%XHU2M_MIN(KLU))
  ALLOCATE(DGT%XHU2M_MAX(KLU))
  ALLOCATE(DGT%XZON10M (KLU))
  ALLOCATE(DGT%XMER10M (KLU))
  ALLOCATE(DGT%XWIND10M (KLU))
  ALLOCATE(DGT%XWIND10M_MAX(KLU))
  !
  DGT%XRI      = XUNDEF
  DGT%XT2M     = XUNDEF
  DGT%XT2M_MIN = XUNDEF
  DGT%XT2M_MAX = -XUNDEF
  DGT%XQ2M     = XUNDEF
  DGT%XHU2M    = XUNDEF
  DGT%XHU2M_MIN= XUNDEF
  DGT%XHU2M_MAX=-XUNDEF
  DGT%XZON10M  = XUNDEF
  DGT%XMER10M  = XUNDEF
  DGT%XWIND10M = XUNDEF
  DGT%XWIND10M_MAX = -XUNDEF
ELSE
  ALLOCATE(DGT%XRI      (0))
  ALLOCATE(DGT%XT2M     (0))
  ALLOCATE(DGT%XT2M_MIN (0))
  ALLOCATE(DGT%XT2M_MAX (0))
  ALLOCATE(DGT%XQ2M     (0))
  ALLOCATE(DGT%XHU2M    (0))
  ALLOCATE(DGT%XHU2M_MIN(0))
  ALLOCATE(DGT%XHU2M_MAX(0))
  ALLOCATE(DGT%XZON10M  (0))
  ALLOCATE(DGT%XMER10M  (0))  
  ALLOCATE(DGT%XWIND10M (0))
  ALLOCATE(DGT%XWIND10M_MAX(0))
END IF
!!
!* miscellaneous fields
!
IF (DGT%N2M>0 .AND. DGUT%LUTCI) THEN
  !
  ALLOCATE(DGUT%XUTCI_IN       (KLU))
  ALLOCATE(DGUT%XUTCI_OUTSUN   (KLU))
  ALLOCATE(DGUT%XUTCI_OUTSHADE (KLU))
  ALLOCATE(DGUT%XTRAD_SUN      (KLU))
  ALLOCATE(DGUT%XTRAD_SHADE    (KLU))
  ALLOCATE(DGUT%XUTCIC_IN      (KLU,NUTCI_STRESS))
  ALLOCATE(DGUT%XUTCIC_OUTSUN  (KLU,NUTCI_STRESS))
  ALLOCATE(DGUT%XUTCIC_OUTSHADE(KLU,NUTCI_STRESS))
  !
  DGUT%XUTCI_IN        = XUNDEF
  DGUT%XUTCI_OUTSUN    = XUNDEF
  DGUT%XUTCI_OUTSHADE  = XUNDEF
  DGUT%XTRAD_SUN       = XUNDEF
  DGUT%XTRAD_SHADE     = XUNDEF
  DGUT%XUTCIC_IN       = 0.
  DGUT%XUTCIC_OUTSUN   = 0.
  DGUT%XUTCIC_OUTSHADE = 0.
  !  
ELSE
  ALLOCATE(DGUT%XUTCI_IN       (0))
  ALLOCATE(DGUT%XUTCI_OUTSUN   (0))
  ALLOCATE(DGUT%XUTCI_OUTSHADE (0))
  ALLOCATE(DGUT%XTRAD_SUN      (0))
  ALLOCATE(DGUT%XTRAD_SHADE    (0))        
  ALLOCATE(DGUT%XUTCIC_IN      (0,0))
  ALLOCATE(DGUT%XUTCIC_OUTSUN  (0,0))
  ALLOCATE(DGUT%XUTCIC_OUTSHADE(0,0))
ENDIF
!
!* transfer coefficients
!
IF (DGT%LCOEF) THEN
  ALLOCATE(DGT%XCD     (KLU))
  ALLOCATE(DGT%XCH     (KLU))
  ALLOCATE(DGT%XCE     (KLU))
  ALLOCATE(DGT%XZ0     (KLU))
  ALLOCATE(DGT%XZ0H    (KLU))
  !
  DGT%XCD      = XUNDEF
  DGT%XCH      = XUNDEF
  DGT%XCE      = XUNDEF
  DGT%XZ0      = XUNDEF
  DGT%XZ0H     = XUNDEF
ELSE
  ALLOCATE(DGT%XCD     (0))
  ALLOCATE(DGT%XCH     (0))
  ALLOCATE(DGT%XCE     (0))
  ALLOCATE(DGT%XZ0     (0))
  ALLOCATE(DGT%XZ0H    (0))
END IF
!
!
!* surface humidity
!
IF (DGT%LSURF_VARS) THEN
  ALLOCATE(DGT%XQS     (KLU))
  !
  DGT%XQS      = XUNDEF
ELSE
  ALLOCATE(DGT%XQS     (0))  
END IF
IF (LHOOK) CALL DR_HOOK('DIAG_TEB_INIT_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_TEB_INIT_n
