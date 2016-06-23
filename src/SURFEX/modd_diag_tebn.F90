!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################
      MODULE MODD_DIAG_TEB_n
!     ######################
!
!!****  *MODD_DIAG_TEB - declaration of diagnostics for TEB scheme
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

TYPE DIAG_TEB_t
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
!
  LOGICAL :: LPGD         ! flag for writing of PGD files
  LOGICAL :: LPGD_FIX     ! flag for writing of PGD files for time
!                           invariant field
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
  REAL, POINTER, DIMENSION(:)   :: XLE      ! latent heat flux                 (W/m2) 
  REAL, POINTER, DIMENSION(:)   :: XGFLUX   ! net soil-vegetation flux         (W/m2)
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
  REAL, POINTER, DIMENSION(:)   :: XSFCO2   ! CO2 flux                         (m/s*kg_CO2/kg_air)
  REAL, POINTER, DIMENSION(:)   :: XLWD     ! downward long wave radiation     (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLWU     ! upward long wave radiation       (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWD     ! downward short wave radiation    (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWU     ! upward short wave radiation      (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWBD    ! downward short wave radiation by spectral band   (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWBU    ! upward short wave radiation by spectral band (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XFMU     ! horizontal momentum flux zonal   (m2/s2)
  REAL, POINTER, DIMENSION(:)   :: XFMV     ! horizontal momentum flux meridian (m2/s2)             
  REAL, POINTER, DIMENSION(:)   :: XDIAG_TS ! arithmetic mean of surface temperature (K)
!------------------------------------------------------------------------------
!

END TYPE DIAG_TEB_t



 CONTAINS

!




SUBROUTINE DIAG_TEB_INIT(YDIAG_TEB)
TYPE(DIAG_TEB_t), INTENT(INOUT) :: YDIAG_TEB
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_TEB_N:DIAG_TEB_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YDIAG_TEB%XRI)
  NULLIFY(YDIAG_TEB%XCD)
  NULLIFY(YDIAG_TEB%XCH)
  NULLIFY(YDIAG_TEB%XCE)
  NULLIFY(YDIAG_TEB%XZ0)
  NULLIFY(YDIAG_TEB%XZ0H)
  NULLIFY(YDIAG_TEB%XRN)
  NULLIFY(YDIAG_TEB%XH)
  NULLIFY(YDIAG_TEB%XLE)
  NULLIFY(YDIAG_TEB%XGFLUX)
  NULLIFY(YDIAG_TEB%XT2M)
  NULLIFY(YDIAG_TEB%XT2M_MIN)
  NULLIFY(YDIAG_TEB%XT2M_MAX)
  NULLIFY(YDIAG_TEB%XQ2M)
  NULLIFY(YDIAG_TEB%XHU2M)
  NULLIFY(YDIAG_TEB%XHU2M_MIN)
  NULLIFY(YDIAG_TEB%XHU2M_MAX)
  NULLIFY(YDIAG_TEB%XQS)
  NULLIFY(YDIAG_TEB%XZON10M)
  NULLIFY(YDIAG_TEB%XMER10M)
  NULLIFY(YDIAG_TEB%XWIND10M)
  NULLIFY(YDIAG_TEB%XWIND10M_MAX)
  NULLIFY(YDIAG_TEB%XSFCO2)
  NULLIFY(YDIAG_TEB%XLWD)
  NULLIFY(YDIAG_TEB%XLWU)
  NULLIFY(YDIAG_TEB%XSWD)
  NULLIFY(YDIAG_TEB%XSWU)
  NULLIFY(YDIAG_TEB%XSWBD)
  NULLIFY(YDIAG_TEB%XSWBU)
  NULLIFY(YDIAG_TEB%XFMU)
  NULLIFY(YDIAG_TEB%XFMV)
  NULLIFY(YDIAG_TEB%XDIAG_TS)
YDIAG_TEB%XDIAG_TSTEP=0.
YDIAG_TEB%N2M=0
YDIAG_TEB%L2M_MIN_ZS=.FALSE.
YDIAG_TEB%LSURF_BUDGET=.FALSE.
YDIAG_TEB%LRAD_BUDGET=.FALSE.
YDIAG_TEB%LCOEF=.FALSE.
YDIAG_TEB%LSURF_VARS=.FALSE.
YDIAG_TEB%LPGD=.FALSE.
YDIAG_TEB%LPGD_FIX=.FALSE.
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_TEB_N:DIAG_TEB_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DIAG_TEB_INIT


END MODULE MODD_DIAG_TEB_n
