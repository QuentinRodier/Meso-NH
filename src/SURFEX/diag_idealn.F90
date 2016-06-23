!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_IDEAL_n (DGL, &
                         HPROGRAM, &
                           PRN, PH, PLE, PLEI, PGFLUX, PRI, PCD, PCH, PCE, PQS,    &
                            PZ0, PZ0H, PT2M, PTS, PQ2M, PHU2M, PZON10M, PMER10M,    &
                            PSWD, PSWU, PLWD, PLWU, PSWBD, PSWBU, PFMU, PFMV,       &
                            PRNC, PHC, PLEC, PGFLUXC, PSWDC, PSWUC, PLWDC,          &
                            PLWUC, PFMUC, PFMVC, PT2M_MIN, PT2M_MAX, PLEIC,         &
                            PHU2M_MIN, PHU2M_MAX, PWIND10M, PWIND10M_MAX,           &
                            PEVAP, PEVAPC, PSUBL, PSUBLC                            )
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
!!      P.Le Moigne 03/2015 add diagnostics IDEAL case
!!------------------------------------------------------------------
!

!
!
USE MODD_DIAG_IDEAL_n, ONLY : DIAG_IDEAL_t
!
USE MODD_SURF_PAR,    ONLY : XUNDEF
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
TYPE(DIAG_IDEAL_t), INTENT(INOUT) :: DGL
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
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_IDEAL_N',0,ZHOOK_HANDLE)
!
IF (DGL%LSURF_BUDGET) THEN
  PRN      = DGL%XRN
  PH       = DGL%XH
  PLE      = DGL%XLE
  PLEI     = DGL%XLEI
  PGFLUX   = DGL%XGFLUX
  PEVAP    = DGL%XEVAP
  PSUBL    = DGL%XSUBL
  PSWD     = DGL%XSWD
  PSWU     = DGL%XSWU
  PLWD     = DGL%XLWD
  PLWU     = DGL%XLWU
  PSWBD    = DGL%XSWBD
  PSWBU    = DGL%XSWBU
  PFMU     = DGL%XFMU
  PFMV     = DGL%XFMV
END IF
!
IF (DGL%LSURF_BUDGETC) THEN
  PRNC      = DGL%XRNC
  PHC       = DGL%XHC
  PLEC      = DGL%XLEC
  PLEIC     = DGL%XLEIC
  PGFLUXC   = DGL%XGFLUXC
  PEVAPC    = DGL%XEVAPC
  PSUBLC    = DGL%XSUBLC  
  PSWDC     = DGL%XSWDC
  PSWUC     = DGL%XSWUC
  PLWDC     = DGL%XLWDC
  PLWUC     = DGL%XLWUC
  PFMUC     = DGL%XFMUC
  PFMVC     = DGL%XFMVC
END IF
!
IF (DGL%N2M>=1 .OR. DGL%LSURF_BUDGET .OR. DGL%LSURF_BUDGETC) PTS = DGL%XDIAG_TS
!
IF (DGL%N2M>=1) THEN
  PRI      = DGL%XRI
  PT2M     = DGL%XT2M
  PT2M_MIN = DGL%XT2M_MIN
  PT2M_MAX = DGL%XT2M_MAX  
  PQ2M     = DGL%XQ2M
  PHU2M    = DGL%XHU2M
  PHU2M_MIN= DGL%XHU2M_MIN
  PHU2M_MAX= DGL%XHU2M_MAX
  PZON10M  = DGL%XZON10M
  PMER10M  = DGL%XMER10M
  PWIND10M = DGL%XWIND10M
  PWIND10M_MAX = DGL%XWIND10M_MAX
END IF
!
IF (DGL%LCOEF) THEN
  PCD  = DGL%XCD
  PCH  = DGL%XCH
  PCE  = DGL%XCE        
  PZ0  = DGL%XZ0
  PZ0H = DGL%XZ0H
ENDIF
!
IF (DGL%LSURF_VARS) THEN
  PQS = DGL%XQS
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_IDEAL_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_IDEAL_n
