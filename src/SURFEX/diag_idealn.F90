!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_IDEAL_n(HPROGRAM, PQS, PZ0, PZ0H, PH, PLE, PRN, PGFLUX)
!     ###############################################################################
!
!!****  *DIAG_IDEAL_n * - Stores IDEAL_n diagnostics
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     P. Le Moigne 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2009
!!------------------------------------------------------------------
!

!
USE MODD_SURF_PAR,    ONLY : XUNDEF
USE MODD_DIAG_IDEAL_n, ONLY : LSURF_BUDGET, LCOEF, LSURF_VARS, &
                              XQS, XZ0, XZ0H, XH, XLE, XRN, XGFLUX
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
!
REAL, DIMENSION(:), INTENT(OUT) :: PQS
REAL, DIMENSION(:), INTENT(OUT) :: PZ0      ! rough. length wind  (m)
REAL, DIMENSION(:), INTENT(OUT) :: PZ0H     ! rough. length heat  (m)
REAL, DIMENSION(:), INTENT(OUT) :: PH       ! Sensible heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLE      ! Latent heat flux    (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PRN      ! net flux    (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PGFLUX   ! net flux    (W/m2)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_IDEAL_N',0,ZHOOK_HANDLE)
!
IF (LSURF_BUDGET) THEN
  PH       = XH
  PLE      = XLE
  PRN      = XRN
  PGFLUX   = XGFLUX
END IF
!
IF (LCOEF) THEN
  PZ0  = XZ0
  PZ0H = XZ0H
ENDIF
!
IF (LSURF_VARS) THEN
  PQS = XQS
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_IDEAL_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_IDEAL_n
