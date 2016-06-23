!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_TEB_n (DGT, &
                       HPROGRAM,                                               &
                        PRN, PH, PLE, PGFLUX, PRI, PCD, PCH, PCE, PQS,          &
                        PZ0, PZ0H, PT2M, PTS, PQ2M, PHU2M, PZON10M, PMER10M,    &
                        PSWD, PSWU, PLWD, PLWU, PSWBD, PSWBU, PFMU, PFMV,       &
                        PT2M_MIN, PT2M_MAX, PHU2M_MIN, PHU2M_MAX,               &
                        PWIND10M, PWIND10M_MAX                                  )
!     ###############################################################################
!
!!****  *DIAG_TEB_n * - diagnostics for TEB
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
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!       B. decharme 04/2013 : Add Ts diag
!!------------------------------------------------------------------
!
!
!
USE MODD_DIAG_TEB_n, ONLY : DIAG_TEB_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
! 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_TEB_t), INTENT(INOUT) :: DGT
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
!
REAL, DIMENSION(:), INTENT(OUT) :: PRN      ! Net radiation       (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PH       ! Sensible heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLE      ! Latent heat flux    (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PGFLUX   ! Storage flux        (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PRI      ! Richardson number   (-)
REAL, DIMENSION(:), INTENT(OUT) :: PCD      ! drag coefficient    (W/s2)
REAL, DIMENSION(:), INTENT(OUT) :: PCH      ! transf. coef heat   (W/s)
REAL, DIMENSION(:), INTENT(OUT) :: PCE      ! transf. coef vapor  (W/s/K)
REAL, DIMENSION(:), INTENT(OUT) :: PZ0      ! rough. length wind  (m)
REAL, DIMENSION(:), INTENT(OUT) :: PQS
REAL, DIMENSION(:), INTENT(OUT) :: PZ0H     ! rough. length heat  (m)
REAL, DIMENSION(:), INTENT(OUT) :: PTS      ! surface temperature (K)
REAL, DIMENSION(:), INTENT(OUT) :: PT2M     ! temperature at 2m   (K)
REAL, DIMENSION(:), INTENT(OUT) :: PQ2M     ! humidity at 2m      (kg/kg)
REAL, DIMENSION(:), INTENT(OUT) :: PHU2M    ! relative humidity at 2m (-)
REAL, DIMENSION(:), INTENT(OUT) :: PZON10M  ! zonal wind at 10m   (m/s)
REAL, DIMENSION(:), INTENT(OUT) :: PMER10M  ! meridian wind at 10m(m/s)
REAL, DIMENSION(:), INTENT(OUT) :: PSWD     ! incoming short-wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSWU     ! upward short-wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLWD     ! incoming long-wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLWU     ! upward long-wave radiation (W/m2)
REAL, DIMENSION(:,:), INTENT(OUT) :: PSWBD  ! incoming short-wave radiation by spectral band (W/m2)
REAL, DIMENSION(:,:), INTENT(OUT) :: PSWBU  ! upward short-wave radiation by spectral band (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PFMU     ! zonal momentum flux (m2/s2)
REAL, DIMENSION(:), INTENT(OUT) :: PFMV     ! meridian momentum flux (m2/s2)
REAL, DIMENSION(:), INTENT(OUT) :: PT2M_MIN ! Minimum temperature at 2m   (K)
REAL, DIMENSION(:), INTENT(OUT) :: PT2M_MAX ! Maximum temperature at 2m   (K)
REAL, DIMENSION(:), INTENT(OUT) :: PHU2M_MIN! Minimum relative humidity at 2m (-)
REAL, DIMENSION(:), INTENT(OUT) :: PHU2M_MAX! Maximum relative humidity at 2m (-)
REAL, DIMENSION(:), INTENT(OUT) :: PWIND10M ! wind at 10m (m/s)
REAL, DIMENSION(:), INTENT(OUT) :: PWIND10M_MAX! Maximum wind at 10m (m/s)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_TEB_N',0,ZHOOK_HANDLE)
IF (DGT%LSURF_BUDGET) THEN
  PRN      = DGT%XRN
  PH       = DGT%XH
  PLE      = DGT%XLE
  PGFLUX   = DGT%XGFLUX
  PSWD     = DGT%XSWD
  PSWU     = DGT%XSWU
  PLWD     = DGT%XLWD
  PLWU     = DGT%XLWU
  PSWBD    = DGT%XSWBD
  PSWBU    = DGT%XSWBU
  PFMU     = DGT%XFMU
  PFMV     = DGT%XFMV
END IF
!
IF (DGT%N2M>=1 .OR. DGT%LSURF_BUDGET) PTS = DGT%XDIAG_TS
!
IF (DGT%N2M>=1) THEN
  PRI      = DGT%XRI
  PT2M     = DGT%XT2M
  PT2M_MIN = DGT%XT2M_MIN
  PT2M_MAX = DGT%XT2M_MAX
  PQ2M     = DGT%XQ2M
  PHU2M    = DGT%XHU2M
  PHU2M_MIN= DGT%XHU2M_MIN
  PHU2M_MAX= DGT%XHU2M_MAX
  PZON10M  = DGT%XZON10M
  PMER10M  = DGT%XMER10M
  PWIND10M = DGT%XWIND10M
  PWIND10M_MAX = DGT%XWIND10M_MAX
END IF
!
IF (DGT%LCOEF) THEN
  PCD      = DGT%XCD
  PCH      = DGT%XCH
  PCE      = DGT%XCE
  PZ0      = DGT%XZ0
  PZ0H     = DGT%XZ0H
END IF
!
IF (DGT%LSURF_VARS) THEN
  PQS = DGT%XQS
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_TEB_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_TEB_n
