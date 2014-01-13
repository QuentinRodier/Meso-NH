!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_SEAFLUX_n(HPROGRAM,                                           &
                            PRN, PH, PLE, PLEI, PGFLUX, PRI, PCD, PCH, PCE, PQS,&
                            PZ0, PZ0H, PT2M, PTS, PQ2M, PHU2M, PZON10M, PMER10M,&
                            PSWD, PSWU, PLWD, PLWU, PSWBD, PSWBU, PFMU, PFMV,   &
                            PRNC, PHC, PLEC, PGFLUXC, PSWDC, PSWUC, PLWDC,      &
                            PLWUC, PFMUC, PFMVC, PT2M_MIN, PT2M_MAX, PLEIC,     &
                            PHU2M_MIN, PHU2M_MAX, PWIND10M, PWIND10M_MAX        )  
!     ###############################################################################
!
!!****  *DIAG_SEAFLUX_n * - diagnostics for SEAFLUX
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
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SEAFLUX_n,      ONLY : TTIME
USE MODD_DIAG_SEAFLUX_n, ONLY : N2M, LSURF_BUDGET, LCOEF, LSURF_BUDGETC, LSURF_VARS,  &
                                  XRN, XH, XLE, XLEI, XGFLUX, XRI, XT2M, XQ2M, XHU2M, &
                                  XZON10M, XMER10M, XCD, XCH, XCE, XQS, XZ0, XZ0H,    &
                                  XSWD, XSWU, XSWBD, XSWBU, XLWD, XLWU, XFMU, XFMV,   &
                                  XRNC, XHC, XLEC, XGFLUXC, XSWDC, XSWUC, XLWDC,      &
                                  XLWUC, XFMUC, XFMVC, XT2M_MIN, XT2M_MAX, XLEIC,     &
                                  XDIAG_SST, XHU2M_MIN, XHU2M_MAX, XWIND10M, XWIND10M_MAX  
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
REAL, DIMENSION(:), INTENT(OUT) :: PLEI     ! Sublimation latent heat flux (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PGFLUX   ! Storage flux        (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PRI      ! Richardson number   (-)
REAL, DIMENSION(:), INTENT(OUT) :: PCD      ! drag coef           (W/s2)
REAL, DIMENSION(:), INTENT(OUT) :: PCH      ! transfer coef heat  (W/s)
REAL, DIMENSION(:), INTENT(OUT) :: PCE      ! transfer coef vapor (W/s/K)
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
IF (LHOOK) CALL DR_HOOK('DIAG_SEAFLUX_N',0,ZHOOK_HANDLE)
IF (LSURF_BUDGET) THEN
  PRN      = XRN
  PH       = XH
  PLE      = XLE
  PLEI     = XLEI
  PGFLUX   = XGFLUX
  PSWD     = XSWD
  PSWU     = XSWU
  PLWD     = XLWD
  PLWU     = XLWU
  PSWBD    = XSWBD
  PSWBU    = XSWBU
  PFMU     = XFMU
  PFMV     = XFMV
END IF
!
IF (LSURF_BUDGETC) THEN
  PRNC      = XRNC
  PHC       = XHC
  PLEC      = XLEC
  PLEIC     = XLEIC
  PGFLUXC   = XGFLUXC
  PSWDC     = XSWDC
  PSWUC     = XSWUC
  PLWDC     = XLWDC
  PLWUC     = XLWUC
  PFMUC     = XFMUC
  PFMVC     = XFMVC
END IF
!
IF (N2M>=1 .OR. LSURF_BUDGET .OR. LSURF_BUDGETC) PTS = XDIAG_SST
!
IF (N2M>=1) THEN
  PRI      = XRI
  PT2M     = XT2M
  PT2M_MIN = XT2M_MIN
  PT2M_MAX = XT2M_MAX  
  PQ2M     = XQ2M
  PHU2M    = XHU2M
  PHU2M_MIN= XHU2M_MIN
  PHU2M_MAX= XHU2M_MAX
  PZON10M  = XZON10M
  PMER10M  = XMER10M
  PWIND10M = XWIND10M
  PWIND10M_MAX = XWIND10M_MAX
END IF 
!
IF (LCOEF) THEN
  PCD      = XCD
  PCH      = XCH
  PCE      = XCE
  PZ0      = XZ0
  PZ0H     = XZ0H
END IF
!
IF (LSURF_VARS) THEN
  PQS = XQS
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_SEAFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SEAFLUX_n
