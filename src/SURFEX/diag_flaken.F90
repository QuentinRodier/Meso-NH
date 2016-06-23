!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_FLAKE_n (DGF, &
                         HPROGRAM,                                                 &
                            PRN, PH, PLE, PLEI, PGFLUX, PRI, PCD, PCH, PCE, PQS,    &
                            PZ0, PZ0H, PT2M, PTS, PQ2M, PHU2M, PZON10M, PMER10M,    &
                            PSWD, PSWU, PLWD, PLWU, PSWBD, PSWBU, PFMU, PFMV,       &
                            PRNC, PHC, PLEC, PGFLUXC, PSWDC, PSWUC, PLWDC,          &
                            PLWUC, PFMUC, PFMVC, PT2M_MIN, PT2M_MAX, PLEIC,         &
                            PHU2M_MIN, PHU2M_MAX, PWIND10M, PWIND10M_MAX,           &
                            PEVAP, PEVAPC, PSUBL, PSUBLC                            )
!     ###############################################################################
!
!!****  *DIAG_FLAKE_n * - diagnostics for lakes
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
!!       V.Masson   10/2013 Adds min and max 2m parameters
!!      Modified    04/2013 P.LeMoigne : cumulated diag & t2m min/max
!!------------------------------------------------------------------
!

!
!
USE MODD_DIAG_FLAKE_n, ONLY : DIAG_FLAKE_t
!
USE MODD_SURF_PAR,     ONLY : XUNDEF
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
TYPE(DIAG_FLAKE_t), INTENT(INOUT) :: DGF
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
!
REAL, DIMENSION(:), INTENT(OUT) :: PRN      ! Net radiation       (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PH       ! Sensible heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLE      ! Total latent heat flux    (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLEI     ! Sublimation latent heat flux    (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PGFLUX   ! Storage flux        (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PEVAP    ! Total evapotranspiration  (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT) :: PSUBL    ! Sublimation (kg/m2/s)
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
REAL, DIMENSION(:), INTENT(OUT) :: PEVAPC   ! Total evapotranspiration  (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT) :: PSUBLC   ! Sublimation (kg/m2/s)
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
IF (LHOOK) CALL DR_HOOK('DIAG_FLAKE_N',0,ZHOOK_HANDLE)
IF (DGF%LSURF_BUDGET) THEN
  PRN      = DGF%XRN
  PH       = DGF%XH
  PLE      = DGF%XLE
  PLEI     = DGF%XLEI
  PGFLUX   = DGF%XGFLUX
  PEVAP    = DGF%XEVAP
  PSUBL    = DGF%XSUBL
  PSWD     = DGF%XSWD
  PSWU     = DGF%XSWU
  PLWD     = DGF%XLWD
  PLWU     = DGF%XLWU
  PSWBD    = DGF%XSWBD
  PSWBU    = DGF%XSWBU
  PFMU     = DGF%XFMU
  PFMV     = DGF%XFMV
END IF
!
IF (DGF%LSURF_BUDGETC) THEN
  PRNC      = DGF%XRNC
  PHC       = DGF%XHC
  PLEC      = DGF%XLEC
  PLEIC     = DGF%XLEIC
  PGFLUXC   = DGF%XGFLUXC
  PEVAPC    = DGF%XEVAPC
  PSUBLC    = DGF%XSUBLC  
  PSWDC     = DGF%XSWDC
  PSWUC     = DGF%XSWUC
  PLWDC     = DGF%XLWDC
  PLWUC     = DGF%XLWUC
  PFMUC     = DGF%XFMUC
  PFMVC     = DGF%XFMVC
END IF
!
IF (DGF%N2M>=1 .OR. DGF%LSURF_BUDGET .OR. DGF%LSURF_BUDGETC) PTS = DGF%XDIAG_TS
!
IF (DGF%N2M>=1) THEN
  PRI      = DGF%XRI
  PT2M     = DGF%XT2M
  PT2M_MIN = DGF%XT2M_MIN
  PT2M_MAX = DGF%XT2M_MAX  
  PQ2M     = DGF%XQ2M
  PHU2M    = DGF%XHU2M
  PHU2M_MIN= DGF%XHU2M_MIN
  PHU2M_MAX= DGF%XHU2M_MAX
  PZON10M  = DGF%XZON10M
  PMER10M  = DGF%XMER10M
  PWIND10M = DGF%XWIND10M
  PWIND10M_MAX = DGF%XWIND10M_MAX
END IF
!
IF (DGF%LCOEF) THEN
  PCD      = DGF%XCD
  PCH      = DGF%XCH
  PCE      = DGF%XCE
  PZ0      = DGF%XZ0
  PZ0H     = DGF%XZ0H
END IF
!
IF (DGF%LSURF_VARS) THEN
  PQS = DGF%XQS
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_FLAKE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_FLAKE_n
