!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_ISBA_n (DGEI, DGI, &
                        HPROGRAM,                                               &
                         PRN, PH, PLE, PLEI, PGFLUX, PRI, PCD, PCH, PCE, PQS,    &
                         PZ0, PZ0H, PT2M, PTS, PQ2M, PHU2M, PZON10M, PMER10M,    &
                         PSWD, PSWU, PLWD, PLWU, PSWBD, PSWBU, PFMU, PFMV,       &
                         PRNC, PHC, PLEC, PGFLUXC, PSWDC, PSWUC, PLWDC,          &
                         PLWUC, PFMUC, PFMVC, PT2M_MIN, PT2M_MAX, PLEIC,         &
                         PHU2M_MIN, PHU2M_MAX, PWIND10M, PWIND10M_MAX,           &
                            PEVAP, PEVAPC, PSUBL, PSUBLC                         )
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
!       B. decharme 04/2013 : Add EVAP and SUBL diag
!!------------------------------------------------------------------
!
!
!
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_ISBA_n, ONLY : DIAG_ISBA_t
!
USE MODD_SURF_PAR,    ONLY : XUNDEF
!                                  
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
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DGEI
TYPE(DIAG_ISBA_t), INTENT(INOUT) :: DGI
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
IF (LHOOK) CALL DR_HOOK('DIAG_ISBA_N',0,ZHOOK_HANDLE)
IF (DGI%LSURF_BUDGET) THEN
  PRN      = DGI%XAVG_RN
  PH       = DGI%XAVG_H
  PLE      = DGI%XAVG_LE
  PLEI     = DGI%XAVG_LEI
  PGFLUX   = DGI%XAVG_GFLUX
  PSWD     = DGI%XAVG_SWD
  PSWU     = DGI%XAVG_SWU
  PLWD     = DGI%XAVG_LWD
  PLWU     = DGI%XAVG_LWU
  PSWBD    = DGI%XAVG_SWBD
  PSWBU    = DGI%XAVG_SWBU
  PFMU     = DGI%XAVG_FMU
  PFMV     = DGI%XAVG_FMV
END IF
!
IF (DGEI%LSURF_EVAP_BUDGET) THEN
  PEVAP    = DGEI%XAVG_EVAP
  PSUBL    = DGEI%XAVG_SUBL
ENDIF
!
IF (DGEI%LSURF_BUDGETC) THEN
  PRNC      = DGEI%XAVG_RNC
  PHC       = DGEI%XAVG_HC
  PLEC      = DGEI%XAVG_LEC
  PLEIC     = DGEI%XAVG_LEIC
  PGFLUXC   = DGEI%XAVG_GFLUXC
  PEVAPC    = DGEI%XAVG_EVAPC
  PSUBLC    = DGEI%XAVG_SUBLC
  PSWDC     = DGI%XAVG_SWDC
  PSWUC     = DGI%XAVG_SWUC
  PLWDC     = DGI%XAVG_LWDC
  PLWUC     = DGI%XAVG_LWUC
  PFMUC     = DGI%XAVG_FMUC
  PFMVC     = DGI%XAVG_FMVC
END IF
!
IF (DGI%N2M>=1 .OR. DGI%LSURF_BUDGET .OR. DGEI%LSURF_BUDGETC) PTS = DGI%XAVG_TS
!
IF (DGI%N2M>=1) THEN
  PRI      = DGI%XAVG_RI
  PT2M     = DGI%XAVG_T2M
  PT2M_MIN = DGI%XAVG_T2M_MIN
  PT2M_MAX = DGI%XAVG_T2M_MAX
  PQ2M     = DGI%XAVG_Q2M
  PHU2M    = DGI%XAVG_HU2M
  PHU2M_MIN= DGI%XAVG_HU2M_MIN
  PHU2M_MAX= DGI%XAVG_HU2M_MAX
  PZON10M  = DGI%XAVG_ZON10M
  PMER10M  = DGI%XAVG_MER10M
  PWIND10M = DGI%XAVG_WIND10M
  PWIND10M_MAX = DGI%XAVG_WIND10M_MAX
END IF
!
IF (DGI%LCOEF) THEN
  PCD      = DGI%XAVG_CD
  PCH      = DGI%XAVG_CH
  PCE      = DGI%XAVG_CE
  PZ0      = DGI%XAVG_Z0
  PZ0H     = DGI%XAVG_Z0H
END IF
!
IF (DGI%LSURF_VARS) THEN
  PQS = DGI%XAVG_QS
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_ISBA_n
