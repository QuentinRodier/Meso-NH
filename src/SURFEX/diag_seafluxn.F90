!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_SEAFLUX_n (DGS, &
                           HPROGRAM,                                           &
                            PRN, PH, PLE, PLEI, PGFLUX, PRI, PCD, PCH, PCE, PQS,&
                            PZ0, PZ0H, PT2M, PTS, PQ2M, PHU2M, PZON10M, PMER10M,&
                            PSWD, PSWU, PLWD, PLWU, PSWBD, PSWBU, PFMU, PFMV,   &
                            PRNC, PHC, PLEC, PGFLUXC, PSWDC, PSWUC, PLWDC,      &
                            PLWUC, PFMUC, PFMVC, PT2M_MIN, PT2M_MAX, PLEIC,     &
                            PHU2M_MIN, PHU2M_MAX, PWIND10M, PWIND10M_MAX,       &
                            PEVAP, PEVAPC, PSUBL, PSUBLC                        )  
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
!       B. decharme 04/2013 : Add EVAP and SUBL diag
!!------------------------------------------------------------------
!

!
!
USE MODD_DIAG_SEAFLUX_n, ONLY : DIAG_SEAFLUX_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
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
TYPE(DIAG_SEAFLUX_t), INTENT(INOUT) :: DGS
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
!
REAL, DIMENSION(:), INTENT(OUT) :: PRN      ! Net radiation       (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PH       ! Sensible heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLE      ! Total latent heat flux    (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLEI     ! Sublimation latent heat flux (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PGFLUX   ! Storage flux        (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PEVAP    ! Total evapotranspiration  (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT) :: PSUBL    ! Sublimation (kg/m2/s)
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
IF (LHOOK) CALL DR_HOOK('DIAG_SEAFLUX_N',0,ZHOOK_HANDLE)
IF (DGS%LSURF_BUDGET) THEN
  PRN      = DGS%XRN
  PH       = DGS%XH
  PLE      = DGS%XLE
  PLEI     = DGS%XLE_ICE
  PGFLUX   = DGS%XGFLUX
  PEVAP    = DGS%XEVAP
  PSUBL    = DGS%XSUBL
  PSWD     = DGS%XSWD
  PSWU     = DGS%XSWU
  PLWD     = DGS%XLWD
  PLWU     = DGS%XLWU
  PSWBD    = DGS%XSWBD
  PSWBU    = DGS%XSWBU
  PFMU     = DGS%XFMU
  PFMV     = DGS%XFMV
END IF
!
IF (DGS%LSURF_BUDGETC) THEN
  PRNC      = DGS%XRNC
  PHC       = DGS%XHC
  PLEC      = DGS%XLEC
  PLEIC     = DGS%XLEC_ICE
  PGFLUXC   = DGS%XGFLUXC
  PEVAPC    = DGS%XEVAPC
  PSUBLC    = DGS%XSUBLC
  PSWDC     = DGS%XSWDC
  PSWUC     = DGS%XSWUC
  PLWDC     = DGS%XLWDC
  PLWUC     = DGS%XLWUC
  PFMUC     = DGS%XFMUC
  PFMVC     = DGS%XFMVC
END IF
!
IF (DGS%N2M>=1 .OR. DGS%LSURF_BUDGET .OR. DGS%LSURF_BUDGETC) PTS = DGS%XTS
!
IF (DGS%N2M>=1) THEN
  PRI      = DGS%XRI
  PT2M     = DGS%XT2M
  PT2M_MIN = DGS%XT2M_MIN
  PT2M_MAX = DGS%XT2M_MAX  
  PQ2M     = DGS%XQ2M
  PHU2M    = DGS%XHU2M
  PHU2M_MIN= DGS%XHU2M_MIN
  PHU2M_MAX= DGS%XHU2M_MAX
  PZON10M  = DGS%XZON10M
  PMER10M  = DGS%XMER10M
  PWIND10M = DGS%XWIND10M
  PWIND10M_MAX = DGS%XWIND10M_MAX
END IF 
!
IF (DGS%LCOEF) THEN
  PCD      = DGS%XCD
  PCH      = DGS%XCH
  PCE      = DGS%XCE
  PZ0      = DGS%XZ0
  PZ0H     = DGS%XZ0H
END IF
!
IF (DGS%LSURF_VARS) THEN
  PQS = DGS%XQS
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_SEAFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SEAFLUX_n
