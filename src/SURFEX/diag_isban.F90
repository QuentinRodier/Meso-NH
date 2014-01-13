!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_ISBA_n(HPROGRAM,                                               &
                         PRN, PH, PLE, PLEI, PGFLUX, PRI, PCD, PCH, PCE, PQS,    &
                         PZ0, PZ0H, PT2M, PTS, PQ2M, PHU2M, PZON10M, PMER10M,    &
                         PSWD, PSWU, PLWD, PLWU, PSWBD, PSWBU, PFMU, PFMV,       &
                         PRNC, PHC, PLEC, PGFLUXC, PSWDC, PSWUC, PLWDC,          &
                         PLWUC, PFMUC, PFMVC, PT2M_MIN, PT2M_MAX, PLEIC,         &
                         PHU2M_MIN, PHU2M_MAX, PWIND10M, PWIND10M_MAX            )  
!     ###############################################################################
!
!!****  *DIAG_ISBA_n * - Stores ISBA diagnostics
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
!!      Modified    01/2006 : sea flux parameterization.
!!      Modified    08/2009 : new diag
!!------------------------------------------------------------------
!
!
USE MODD_SURF_PAR,    ONLY : XUNDEF
USE MODD_ISBA_n,      ONLY : TTIME
USE MODD_DIAG_EVAP_ISBA_n, ONLY : LSURF_BUDGETC, XAVG_RNC,       &
                                    XAVG_HC, XAVG_LEC, XAVG_LEIC,  &
                                    XAVG_GFLUXC  
!                                  
USE MODD_DIAG_ISBA_n, ONLY : N2M, LSURF_BUDGET, LCOEF, LSURF_VARS,       &
                               XAVG_RN, XAVG_H, XAVG_LE, XAVG_GFLUX,     &
                               XAVG_RI, XAVG_CD, XAVG_CH, XAVG_CE,       &
                               XAVG_T2M, XAVG_Q2M, XAVG_HU2M,            &
                               XAVG_ZON10M, XAVG_MER10M, XAVG_T2M_MAX,   &
                               XAVG_QS, XAVG_Z0, XAVG_Z0H, XAVG_T2M_MIN, &
                               XAVG_SWD, XAVG_SWU, XAVG_SWBD, XAVG_SWBU, &
                               XAVG_LWD, XAVG_LWU, XAVG_FMU, XAVG_FMV  , &
                               XAVG_SWDC, XAVG_SWUC, XAVG_LWDC,          &
                               XAVG_LWUC, XAVG_FMUC, XAVG_FMVC,          &
                               XAVG_TS, XAVG_LEI, XAVG_HU2M_MIN,         &
                               XAVG_HU2M_MAX, XAVG_WIND10M,              &
                               XAVG_WIND10M_MAX  
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
REAL, DIMENSION(:), INTENT(OUT) :: PRN      ! Net radiation       (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PH       ! Sensible heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLE      ! Total latent heat flux    (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLEI     ! Sublimation latent heat flux    (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PGFLUX   ! Storage flux        (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PRI      ! Richardson number   (-)
REAL, DIMENSION(:), INTENT(OUT) :: PCD      ! drag coefficient    (W/s2)
REAL, DIMENSION(:), INTENT(OUT) :: PCH      ! transf. coef heat   (W/s)
REAL, DIMENSION(:), INTENT(OUT) :: PCE      ! transf. coef vapor  (W/s/K)
REAL, DIMENSION(:), INTENT(OUT) :: PQS
REAL, DIMENSION(:), INTENT(OUT) :: PZ0      ! rough. length wind  (m)
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
REAL, DIMENSION(:), INTENT(OUT) :: PFMU     ! zonal momentum flux (Pa)
REAL, DIMENSION(:), INTENT(OUT) :: PFMV     ! meridian momentum flux (Pa)
REAL, DIMENSION(:), INTENT(OUT) :: PRNC     ! Net radiation       (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PHC      ! Sensible heat flux  (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLEC     ! Total latent heat flux    (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLEIC    ! Sublimation latent heat flux    (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PGFLUXC  ! Storage flux        (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSWDC    ! incoming short wave radiation (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSWUC    ! outgoing short wave radiation (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLWDC    ! incoming long wave radiation (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLWUC    ! outgoing long wave radiation (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PFMUC    ! zonal friction
REAL, DIMENSION(:), INTENT(OUT) :: PFMVC    ! meridian friction
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
IF (LHOOK) CALL DR_HOOK('DIAG_ISBA_N',0,ZHOOK_HANDLE)
IF (LSURF_BUDGET) THEN
  PRN      = XAVG_RN
  PH       = XAVG_H
  PLE      = XAVG_LE
  PLEI     = XAVG_LEI
  PGFLUX   = XAVG_GFLUX
  PSWD     = XAVG_SWD
  PSWU     = XAVG_SWU
  PLWD     = XAVG_LWD
  PLWU     = XAVG_LWU
  PSWBD    = XAVG_SWBD
  PSWBU    = XAVG_SWBU
  PFMU     = XAVG_FMU
  PFMV     = XAVG_FMV
END IF
!
IF (LSURF_BUDGETC) THEN
  PRNC      = XAVG_RNC
  PHC       = XAVG_HC
  PLEC      = XAVG_LEC
  PLEIC     = XAVG_LEIC
  PGFLUXC   = XAVG_GFLUXC
  PSWDC     = XAVG_SWDC
  PSWUC     = XAVG_SWUC
  PLWDC     = XAVG_LWDC
  PLWUC     = XAVG_LWUC
  PFMUC     = XAVG_FMUC
  PFMVC     = XAVG_FMVC
END IF
!
IF (N2M>=1 .OR. LSURF_BUDGET .OR. LSURF_BUDGETC) PTS = XAVG_TS
!
IF (N2M>=1) THEN
  PRI      = XAVG_RI
  PT2M     = XAVG_T2M
  PT2M_MIN = XAVG_T2M_MIN
  PT2M_MAX = XAVG_T2M_MAX
  PQ2M     = XAVG_Q2M
  PHU2M    = XAVG_HU2M
  PHU2M_MIN= XAVG_HU2M_MIN
  PHU2M_MAX= XAVG_HU2M_MAX
  PZON10M  = XAVG_ZON10M
  PMER10M  = XAVG_MER10M
  PWIND10M = XAVG_WIND10M
  PWIND10M_MAX = XAVG_WIND10M_MAX
END IF
!
IF (LCOEF) THEN
  PCD      = XAVG_CD
  PCH      = XAVG_CH
  PCE      = XAVG_CE
  PZ0      = XAVG_Z0
  PZ0H     = XAVG_Z0H
END IF
!
IF (LSURF_VARS) THEN
  PQS = XAVG_QS
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_ISBA_n
