!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################
      MODULE MODD_DIAG_SEAFLUX_n
!     ######################
!
!!****  *MODD_DIAG_SEAFLUX - declaration of diagnostics for SEAFLUX scheme
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
!!      S.Senesi    01/2014 : add diags on seaice 
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

TYPE DIAG_SEAFLUX_t
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
  REAL, POINTER, DIMENSION(:)   :: XRI_ICE  ! Seaice Bulk-Richardson number    (-)
  REAL, POINTER, DIMENSION(:)   :: XCD      ! drag coefficient for wind        (W/s2)
  REAL, POINTER, DIMENSION(:)   :: XCD_ICE  ! Seaice drag coefficient for wind (W/s2)
  REAL, POINTER, DIMENSION(:)   :: XCH      ! drag coefficient for heat        (W/s)
  REAL, POINTER, DIMENSION(:)   :: XCH_ICE  ! Seaice drag coefficient for heat (W/s)
  REAL, POINTER, DIMENSION(:)   :: XCE      ! drag coefficient for vapor       (W/s/K)
  REAL, POINTER, DIMENSION(:)   :: XZ0      ! roughness length for momentum    (m)
  REAL, POINTER, DIMENSION(:)   :: XZ0_ICE  ! Seaice roughness length for momentum (m)
  REAL, POINTER, DIMENSION(:)   :: XZ0H     ! roughness length for heat        (m)
  REAL, POINTER, DIMENSION(:)   :: XZ0H_ICE ! Seaice roughness length for heat (m)
  REAL, POINTER, DIMENSION(:)   :: XRN      ! net radiation at surface         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XRN_ICE  ! Seaice net radiation at surface  (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XH       ! sensible heat flux               (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XH_ICE   ! Seaice  sensible heat flux       (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLE      ! total latent heat flux           (W/m2) 
  REAL, POINTER, DIMENSION(:)   :: XLE_ICE     ! sublimation latent heat flux     (W/m2) 
  REAL, POINTER, DIMENSION(:)   :: XGFLUX   ! net soil-vegetation flux         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XGFLUX_ICE ! net soil-vegetation flux (seaice) (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XEVAP    ! total evaporation                (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XSUBL    ! sublimation                      (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XT2M     ! air temperature at 2 meters      (K)
  REAL, POINTER, DIMENSION(:)   :: XT2M_ICE ! Seaice air temperature at 2 meters (K)
  REAL, POINTER, DIMENSION(:)   :: XT2M_MIN ! Minimum air temperature at 2 meters (K)
  REAL, POINTER, DIMENSION(:)   :: XT2M_MAX ! Maximum air temperature at 2 meters (K)
  REAL, POINTER, DIMENSION(:)   :: XQ2M     ! air humidity at 2 meters         (kg/kg)
  REAL, POINTER, DIMENSION(:)   :: XQ2M_ICE ! Seaice air humidity at 2 meters  (kg/kg)
  REAL, POINTER, DIMENSION(:)   :: XHU2M    ! air relative humidity at 2 meters(-)
  REAL, POINTER, DIMENSION(:)   :: XHU2M_ICE! Seaice air relative humidity at 2 meters(-)
  REAL, POINTER, DIMENSION(:)   :: XHU2M_MIN! Minimum relative humidity at 2 meters (-)
  REAL, POINTER, DIMENSION(:)   :: XHU2M_MAX! Maximum relative humidity at 2 meters (-)
  REAL, POINTER, DIMENSION(:)   :: XQS      ! air humidity at surface          (kg/kg)
  REAL, POINTER, DIMENSION(:)   :: XQS_ICE  ! Seaice air humidity at surface   (kg/kg)
  REAL, POINTER, DIMENSION(:)   :: XZON10M  ! zonal wind at 10 meters          (m/s)
  REAL, POINTER, DIMENSION(:)   :: XZON10M_ICE ! Seaice zonal wind at 10 meters(m/s)
  REAL, POINTER, DIMENSION(:)   :: XMER10M  ! meridian wind at 10 meters       (m/s)
  REAL, POINTER, DIMENSION(:)   :: XMER10M_ICE ! Seaice meridian wind at 10 meters (m/s)
  REAL, POINTER, DIMENSION(:)   :: XWIND10M ! wind at 10 meters                (m/s)
  REAL, POINTER, DIMENSION(:)   :: XWIND10M_ICE ! Seaice wind at 10 meters     (m/s)
  REAL, POINTER, DIMENSION(:)   :: XWIND10M_MAX! Maximum wind at 10 meters     (m/s)
  REAL, POINTER, DIMENSION(:)   :: XLWD     ! downward long wave radiation     (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLWU     ! upward long wave radiation       (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLWU_ICE ! Seaice upward long wave radiation (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWD     ! downward short wave radiation    (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWU     ! upward short wave radiation      (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWU_ICE ! Seaice upward short wave radiation (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWBD    ! downward short wave radiation by spectral band (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWBU    ! upward short wave radiation by spectral band (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWBU_ICE! Seaice upward short wave radiation by spectral band (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XFMU     ! horizontal momentum flux zonal    (kg/ms2)
  REAL, POINTER, DIMENSION(:)   :: XFMU_ICE ! Seaice horizontal momentum flux zonal (kg/ms2)
  REAL, POINTER, DIMENSION(:)   :: XFMV     ! horizontal momentum flux meridian (kg/ms2)
  REAL, POINTER, DIMENSION(:)   :: XFMV_ICE ! Seaice horizontal momentum flux meridian (kg/ms2)
!
  REAL, POINTER, DIMENSION(:)   :: XTS     ! surface temperature              (K)
  REAL, POINTER, DIMENSION(:)   :: XTSRAD  ! surface radiative temperature    (K)
  REAL, POINTER, DIMENSION(:)   :: XALBT   ! Total Albedo  
!
!* cumulated averaged variables
!
  REAL, POINTER, DIMENSION(:)   :: XRNC     ! net radiation at surface         (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XRNC_ICE ! Seaice net radiation at surface  (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XHC      ! sensible heat flux               (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XHC_ICE  ! Seaice sensible heat flux        (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XLEC     ! total latent heat flux           (J/m2) 
  REAL, POINTER, DIMENSION(:)   :: XLEC_ICE    ! sublimation latent heat flux     (J/m2) 
  REAL, POINTER, DIMENSION(:)   :: XGFLUXC  ! net soil-vegetation flux         (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XGFLUXC_ICE !Seaice net soil-vegetation flux(J/m2)
  REAL, POINTER, DIMENSION(:)   :: XEVAPC   ! total evaporation                (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XSUBLC   ! sublimation                      (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XLWDC    ! downward long wave radiation     (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XLWUC    ! upward long wave radiation       (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XLWUC_ICE! Seaice upward long wave radiation(J/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWDC    ! downward short wave radiation    (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWUC    ! upward short wave radiation      (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWUC_ICE! Seaice upward short wave radiation(J/m2)
  REAL, POINTER, DIMENSION(:)   :: XFMUC    ! horizontal momentum flux zonal    (kg/ms)
  REAL, POINTER, DIMENSION(:)   :: XFMUC_ICE! Seaice horizontal momentum flux zonal (kg/ms)
  REAL, POINTER, DIMENSION(:)   :: XFMVC    ! horizontal momentum flux meridian (kg/ms)
  REAL, POINTER, DIMENSION(:)   :: XFMVC_ICE! Seaice horizontal momentum flux meridian (kg/ms)
!
!------------------------------------------------------------------------------
!
END TYPE DIAG_SEAFLUX_t



 CONTAINS

!





SUBROUTINE DIAG_SEAFLUX_INIT(YDIAG_SEAFLUX)
TYPE(DIAG_SEAFLUX_t), INTENT(INOUT) :: YDIAG_SEAFLUX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_SEAFLUX_N:DIAG_SEAFLUX_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YDIAG_SEAFLUX%XRI)
  NULLIFY(YDIAG_SEAFLUX%XRI_ICE)
  NULLIFY(YDIAG_SEAFLUX%XCD)
  NULLIFY(YDIAG_SEAFLUX%XCD_ICE)
  NULLIFY(YDIAG_SEAFLUX%XCH)
  NULLIFY(YDIAG_SEAFLUX%XCH_ICE)
  NULLIFY(YDIAG_SEAFLUX%XCE)
  NULLIFY(YDIAG_SEAFLUX%XZ0)
  NULLIFY(YDIAG_SEAFLUX%XZ0_ICE)
  NULLIFY(YDIAG_SEAFLUX%XZ0H)
  NULLIFY(YDIAG_SEAFLUX%XZ0H_ICE)
  NULLIFY(YDIAG_SEAFLUX%XRN)
  NULLIFY(YDIAG_SEAFLUX%XRN_ICE)
  NULLIFY(YDIAG_SEAFLUX%XH)
  NULLIFY(YDIAG_SEAFLUX%XH_ICE)
  NULLIFY(YDIAG_SEAFLUX%XLE)
  NULLIFY(YDIAG_SEAFLUX%XLE_ICE)
  NULLIFY(YDIAG_SEAFLUX%XGFLUX)
  NULLIFY(YDIAG_SEAFLUX%XGFLUX_ICE)
  NULLIFY(YDIAG_SEAFLUX%XEVAP)
  NULLIFY(YDIAG_SEAFLUX%XSUBL)
  NULLIFY(YDIAG_SEAFLUX%XT2M)
  NULLIFY(YDIAG_SEAFLUX%XT2M_ICE)
  NULLIFY(YDIAG_SEAFLUX%XT2M_MIN)
  NULLIFY(YDIAG_SEAFLUX%XT2M_MAX)
  NULLIFY(YDIAG_SEAFLUX%XQ2M)
  NULLIFY(YDIAG_SEAFLUX%XQ2M_ICE)
  NULLIFY(YDIAG_SEAFLUX%XHU2M)
  NULLIFY(YDIAG_SEAFLUX%XHU2M_ICE)
  NULLIFY(YDIAG_SEAFLUX%XHU2M_MIN)
  NULLIFY(YDIAG_SEAFLUX%XHU2M_MAX)
  NULLIFY(YDIAG_SEAFLUX%XQS)
  NULLIFY(YDIAG_SEAFLUX%XQS_ICE)
  NULLIFY(YDIAG_SEAFLUX%XZON10M)
  NULLIFY(YDIAG_SEAFLUX%XZON10M_ICE)
  NULLIFY(YDIAG_SEAFLUX%XMER10M)
  NULLIFY(YDIAG_SEAFLUX%XMER10M_ICE)
  NULLIFY(YDIAG_SEAFLUX%XWIND10M)
  NULLIFY(YDIAG_SEAFLUX%XWIND10M_ICE)
  NULLIFY(YDIAG_SEAFLUX%XWIND10M_MAX)
  NULLIFY(YDIAG_SEAFLUX%XLWD)
  NULLIFY(YDIAG_SEAFLUX%XLWU)
  NULLIFY(YDIAG_SEAFLUX%XLWU_ICE)
  NULLIFY(YDIAG_SEAFLUX%XSWD)
  NULLIFY(YDIAG_SEAFLUX%XSWU)
  NULLIFY(YDIAG_SEAFLUX%XSWU_ICE)
  NULLIFY(YDIAG_SEAFLUX%XSWBD)
  NULLIFY(YDIAG_SEAFLUX%XSWBU)
  NULLIFY(YDIAG_SEAFLUX%XSWBU_ICE)
  NULLIFY(YDIAG_SEAFLUX%XFMU)
  NULLIFY(YDIAG_SEAFLUX%XFMU_ICE)
  NULLIFY(YDIAG_SEAFLUX%XFMV)
  NULLIFY(YDIAG_SEAFLUX%XFMV_ICE)
  NULLIFY(YDIAG_SEAFLUX%XTS)
  NULLIFY(YDIAG_SEAFLUX%XTSRAD)
  NULLIFY(YDIAG_SEAFLUX%XALBT)
  NULLIFY(YDIAG_SEAFLUX%XRNC)
  NULLIFY(YDIAG_SEAFLUX%XRNC_ICE)
  NULLIFY(YDIAG_SEAFLUX%XHC)
  NULLIFY(YDIAG_SEAFLUX%XHC_ICE)
  NULLIFY(YDIAG_SEAFLUX%XLEC)
  NULLIFY(YDIAG_SEAFLUX%XLEC_ICE)
  NULLIFY(YDIAG_SEAFLUX%XGFLUXC)
  NULLIFY(YDIAG_SEAFLUX%XGFLUXC_ICE)
  NULLIFY(YDIAG_SEAFLUX%XEVAPC)
  NULLIFY(YDIAG_SEAFLUX%XSUBLC)
  NULLIFY(YDIAG_SEAFLUX%XLWDC)
  NULLIFY(YDIAG_SEAFLUX%XLWUC)
  NULLIFY(YDIAG_SEAFLUX%XLWUC_ICE)
  NULLIFY(YDIAG_SEAFLUX%XSWDC)
  NULLIFY(YDIAG_SEAFLUX%XSWUC)
  NULLIFY(YDIAG_SEAFLUX%XSWUC_ICE)
  NULLIFY(YDIAG_SEAFLUX%XFMUC)
  NULLIFY(YDIAG_SEAFLUX%XFMUC_ICE)
  NULLIFY(YDIAG_SEAFLUX%XFMVC)
  NULLIFY(YDIAG_SEAFLUX%XFMVC_ICE)
YDIAG_SEAFLUX%XDIAG_TSTEP=0.
YDIAG_SEAFLUX%N2M=0
YDIAG_SEAFLUX%L2M_MIN_ZS=.FALSE.
YDIAG_SEAFLUX%LSURF_BUDGET=.FALSE.
YDIAG_SEAFLUX%LRAD_BUDGET=.FALSE.
YDIAG_SEAFLUX%LCOEF=.FALSE.
YDIAG_SEAFLUX%LSURF_VARS=.FALSE.
YDIAG_SEAFLUX%LSURF_BUDGETC=.FALSE.
YDIAG_SEAFLUX%LRESET_BUDGETC=.FALSE.
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_SEAFLUX_N:DIAG_SEAFLUX_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DIAG_SEAFLUX_INIT


END MODULE MODD_DIAG_SEAFLUX_n
