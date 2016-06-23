!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################
      MODULE MODD_DIAG_WATFLUX_n
!     ######################
!
!!****  *MODD_DIAG_WATFLUX - declaration of diagnostics for WATFLUX scheme
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2004
!!      Modified    01/2006 : sea flux parameterization.
!
!*       0.   DECLARATIONS
!             ------------
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE DIAG_WATFLUX_t
!------------------------------------------------------------------------------
!
  REAL    :: XDIAG_TSTEP  ! time step for diagnostics writing
!
  INTEGER :: N2M          ! flag for 2 meters (and 10 meters) quantities
  LOGICAL :: L2M_MIN_ZS   ! flag for 2 meters quantities evaluated on
!                         ! the minimum orographyy of the grid      
  LOGICAL :: LSURF_BUDGET ! flag for surface energy budget
  LOGICAL :: LRAD_BUDGET  ! flag for radiative energy budget      
  LOGICAL :: LCOEF        ! flag for transfer coefficients
  LOGICAL :: LSURF_VARS   ! flag for surface variables
  LOGICAL :: LSURF_BUDGETC       ! flag for surface cumulated energy budget
  LOGICAL :: LRESET_BUDGETC      ! flag for surface cumulated energy budget  
!
!* averaged variables
!
  REAL, POINTER, DIMENSION(:)   :: XRI      ! Bulk-Richardson number           (-)
  REAL, POINTER, DIMENSION(:)   :: XCD      ! drag coefficient for wind        (W/s2)
  REAL, POINTER, DIMENSION(:)   :: XCH      ! drag coefficient for heat        (W/s)
  REAL, POINTER, DIMENSION(:)   :: XCE      ! drag coefficient for vapor       (W/s/K)
  REAL, POINTER, DIMENSION(:)   :: XZ0      ! roughness length for momentum    (m)
  REAL, POINTER, DIMENSION(:)   :: XZ0H     ! roughness length for heat        (m)      
  REAL, POINTER, DIMENSION(:)   :: XRN      ! net radiation at surface         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XH       ! sensible heat flux               (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLE      ! total latent heat flux           (W/m2) 
  REAL, POINTER, DIMENSION(:)   :: XLEI     ! sublimation latent heat flux     (W/m2) 
  REAL, POINTER, DIMENSION(:)   :: XGFLUX   ! net soil-vegetation flux         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XEVAP    ! total evaporation                (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XSUBL    ! sublimation                      (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XT2M     ! air temperature at 2 meters      (K)
  REAL, POINTER, DIMENSION(:)   :: XT2M_MIN ! Minimum air temperature at 2 meters      (K)
  REAL, POINTER, DIMENSION(:)   :: XT2M_MAX ! Maximum air temperature at 2 meters      (K)
  REAL, POINTER, DIMENSION(:)   :: XQ2M     ! air humidity at 2 meters         (kg/kg)
  REAL, POINTER, DIMENSION(:)   :: XHU2M    ! air relative humidity at 2 meters(-)
  REAL, POINTER, DIMENSION(:)   :: XHU2M_MIN! Minimum relative humidity at 2 meters    (-)
  REAL, POINTER, DIMENSION(:)   :: XHU2M_MAX! Maximum relative humidity at 2 meters    (-)
  REAL, POINTER, DIMENSION(:)   :: XQS      ! air humidity at surface          (kg/kg)
  REAL, POINTER, DIMENSION(:)   :: XZON10M  ! zonal wind at 10 meters          (m/s)
  REAL, POINTER, DIMENSION(:)   :: XMER10M  ! meridian wind at 10 meters       (m/s)
  REAL, POINTER, DIMENSION(:)   :: XWIND10M ! wind at 10 meters                (m/s)
  REAL, POINTER, DIMENSION(:)   :: XWIND10M_MAX! Maximum wind at 10 meters     (m/s)
  REAL, POINTER, DIMENSION(:)   :: XLWD     ! downward long wave radiation     (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLWU     ! upward long wave radiation       (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWD     ! downward short wave radiation    (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWU     ! upward short wave radiation      (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWBD    ! downward short wave radiation by spectral band   (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWBU    ! upward short wave radiation by spectral band (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XFMU     ! horizontal momentum flux zonal   (kg/ms2)
  REAL, POINTER, DIMENSION(:)   :: XFMV     ! horizontal momentum flux meridian (kg/ms2)
  REAL, POINTER, DIMENSION(:)   :: XDIAG_TS ! water surface temperature (K)
!
!* cumulated averaged variables
!
  REAL, POINTER, DIMENSION(:)   :: XRNC     ! net radiation at surface         (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XHC      ! sensible heat flux               (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XLEC     ! total latent heat flux           (J/m2) 
  REAL, POINTER, DIMENSION(:)   :: XLEIC    ! sublimation latent heat flux     (J/m2) 
  REAL, POINTER, DIMENSION(:)   :: XGFLUXC  ! net soil-vegetation flux         (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XEVAPC   ! total evaporation                (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XSUBLC   ! sublimation                      (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XLWDC    ! downward long wave radiation     (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XLWUC    ! upward long wave radiation       (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWDC    ! downward short wave radiation    (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWUC    ! upward short wave radiation      (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XFMUC    ! horizontal momentum flux zonal    (kg/ms)
  REAL, POINTER, DIMENSION(:)   :: XFMVC    ! horizontal momentum flux meridian (kg/ms)
!  
!------------------------------------------------------------------------------
!

END TYPE DIAG_WATFLUX_t



 CONTAINS

!




SUBROUTINE DIAG_WATFLUX_INIT(YDIAG_WATFLUX)
TYPE(DIAG_WATFLUX_t), INTENT(INOUT) :: YDIAG_WATFLUX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_WATFLUX_N:DIAG_WATFLUX_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YDIAG_WATFLUX%XRI)
  NULLIFY(YDIAG_WATFLUX%XCD)
  NULLIFY(YDIAG_WATFLUX%XCH)
  NULLIFY(YDIAG_WATFLUX%XCE)
  NULLIFY(YDIAG_WATFLUX%XZ0)
  NULLIFY(YDIAG_WATFLUX%XZ0H)
  NULLIFY(YDIAG_WATFLUX%XRN)
  NULLIFY(YDIAG_WATFLUX%XH)
  NULLIFY(YDIAG_WATFLUX%XLE)
  NULLIFY(YDIAG_WATFLUX%XLEI)
  NULLIFY(YDIAG_WATFLUX%XGFLUX)
  NULLIFY(YDIAG_WATFLUX%XEVAP)
  NULLIFY(YDIAG_WATFLUX%XSUBL)  
  NULLIFY(YDIAG_WATFLUX%XT2M)
  NULLIFY(YDIAG_WATFLUX%XT2M_MIN)
  NULLIFY(YDIAG_WATFLUX%XT2M_MAX)
  NULLIFY(YDIAG_WATFLUX%XQ2M)
  NULLIFY(YDIAG_WATFLUX%XHU2M)
  NULLIFY(YDIAG_WATFLUX%XHU2M_MIN)
  NULLIFY(YDIAG_WATFLUX%XHU2M_MAX)
  NULLIFY(YDIAG_WATFLUX%XQS)
  NULLIFY(YDIAG_WATFLUX%XZON10M)
  NULLIFY(YDIAG_WATFLUX%XMER10M)
  NULLIFY(YDIAG_WATFLUX%XWIND10M)
  NULLIFY(YDIAG_WATFLUX%XWIND10M_MAX)
  NULLIFY(YDIAG_WATFLUX%XLWD)
  NULLIFY(YDIAG_WATFLUX%XLWU)
  NULLIFY(YDIAG_WATFLUX%XSWD)
  NULLIFY(YDIAG_WATFLUX%XSWU)
  NULLIFY(YDIAG_WATFLUX%XSWBD)
  NULLIFY(YDIAG_WATFLUX%XSWBU)
  NULLIFY(YDIAG_WATFLUX%XFMU)
  NULLIFY(YDIAG_WATFLUX%XFMV)
  NULLIFY(YDIAG_WATFLUX%XDIAG_TS)
  NULLIFY(YDIAG_WATFLUX%XRNC)
  NULLIFY(YDIAG_WATFLUX%XHC)
  NULLIFY(YDIAG_WATFLUX%XLEC)
  NULLIFY(YDIAG_WATFLUX%XLEIC)
  NULLIFY(YDIAG_WATFLUX%XGFLUXC)
  NULLIFY(YDIAG_WATFLUX%XEVAPC)
  NULLIFY(YDIAG_WATFLUX%XSUBLC)
  NULLIFY(YDIAG_WATFLUX%XLWDC)
  NULLIFY(YDIAG_WATFLUX%XLWUC)
  NULLIFY(YDIAG_WATFLUX%XSWDC)
  NULLIFY(YDIAG_WATFLUX%XSWUC)
  NULLIFY(YDIAG_WATFLUX%XFMUC)
  NULLIFY(YDIAG_WATFLUX%XFMVC)
YDIAG_WATFLUX%XDIAG_TSTEP=0.
YDIAG_WATFLUX%N2M=0
YDIAG_WATFLUX%L2M_MIN_ZS=.FALSE.
YDIAG_WATFLUX%LSURF_BUDGET=.FALSE.
YDIAG_WATFLUX%LRAD_BUDGET=.FALSE.
YDIAG_WATFLUX%LCOEF=.FALSE.
YDIAG_WATFLUX%LSURF_VARS=.FALSE.
YDIAG_WATFLUX%LSURF_BUDGETC=.FALSE.
YDIAG_WATFLUX%LRESET_BUDGETC=.FALSE.
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_WATFLUX_N:DIAG_WATFLUX_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DIAG_WATFLUX_INIT


END MODULE MODD_DIAG_WATFLUX_n
