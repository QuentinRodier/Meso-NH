!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!######################
MODULE MODD_DIAG_ISBA_n
!######################
!
!!****  *MODD_DIAG_ISBA - declaration of diagnostics for ISBA scheme
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
!!      P. Samuelsson 10/2014 : added min max for XT2M
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

TYPE DIAG_ISBA_t
!------------------------------------------------------------------------------
!
  REAL    :: XDIAG_TSTEP  ! time step for diagnostics writing
!
  INTEGER :: N2M          ! flag for 2 meters (and 10 meters) quantities
  LOGICAL :: L2M_MIN_ZS   ! flag for 2 meters quantities evaluated on
!                         ! the minimum orographyy of the grid      
  LOGICAL :: LSURF_BUDGET   ! flag for surface energy budget
  LOGICAL :: LRAD_BUDGET    ! flag for radiative energy budget
!
  LOGICAL :: LCOEF        ! flag for transfer coefficients
  LOGICAL :: LSURF_VARS   ! flag for surface variables
!
  LOGICAL :: LPGD          ! flag for writing of PGD files
  LOGICAL :: LPATCH_BUDGET ! flag for patch output
!
!* variables for each patch
!
  REAL, POINTER, DIMENSION(:,:) :: XRI     ! Bulk-Richardson number           (-)
  REAL, POINTER, DIMENSION(:,:) :: XCD     ! drag coefficient for wind        (W/s2)
  REAL, POINTER, DIMENSION(:,:) :: XCH     ! drag coefficient for heat        (W/s)
  REAL, POINTER, DIMENSION(:,:) :: XCE     ! drag coefficient for vapor       (W/s/K)
  REAL, POINTER, DIMENSION(:,:) :: XRN     ! net radiation at surface         (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XH      ! sensible heat flux               (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLE     ! total latent heat flux           (W/m2) 
  REAL, POINTER, DIMENSION(:,:) :: XLEI    ! sublimation latent heat flux     (W/m2) 
  REAL, POINTER, DIMENSION(:,:) :: XGFLUX  ! net soil-vegetation flux         (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XTS     ! surface temperature              (K)
  REAL, POINTER, DIMENSION(:,:) :: XTSRAD  ! surface radiative temperature    (K)  
  REAL, POINTER, DIMENSION(:,:) :: XT2M    ! temperature at 2 meters          (K)
  REAL, POINTER, DIMENSION(:,:) :: XT2M_MIN! min temperature at 2 meters      (K)
  REAL, POINTER, DIMENSION(:,:) :: XT2M_MAX! max temperature at 2 meters      (K)
  REAL, POINTER, DIMENSION(:,:) :: XQ2M    ! humidity    at 2 meters          (kg/kg)
  REAL, POINTER, DIMENSION(:,:) :: XHU2M   ! relative humidity at 2 meters    (-)
  REAL, POINTER, DIMENSION(:,:) :: XQS     ! humidity at surface              (kg/kg)
  REAL, POINTER, DIMENSION(:,:) :: XZON10M ! zonal wind at 10 meters          (m/s)
  REAL, POINTER, DIMENSION(:,:) :: XMER10M ! meridian wind at 10 meters       (m/s)
  REAL, POINTER, DIMENSION(:,:) :: XWIND10M! wind at 10 meters                (m/s)
  REAL, POINTER, DIMENSION(:,:) :: XLWD    ! downward long wave radiation     (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLWU    ! upward long wave radiation       (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWD    ! downward short wave radiation    (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWU    ! upward short wave radiation      (W/m2)
  REAL, POINTER, DIMENSION(:,:,:) :: XSWBD ! downward short wave radiation by spectral band   (W/m2)
  REAL, POINTER, DIMENSION(:,:,:) :: XSWBU ! upward short wave radiation by spectral band (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XFMU    ! horizontal momentum flux zonal    (Pa)
  REAL, POINTER, DIMENSION(:,:) :: XFMV    ! horizontal momentum flux meridian (Pa)             
  ! 
  REAL, POINTER, DIMENSION(:,:) :: XSWDC   ! downward short wave radiation     (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWUC   ! upward short wave radiation       (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLWDC   ! downward long wave radiation      (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLWUC   ! upward long wave radiation        (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XFMUC   ! horizontal momentum flux zonal    (Pa.s)
  REAL, POINTER, DIMENSION(:,:) :: XFMVC   ! horizontal momentum flux meridian (Pa.s)
  !
  REAL, POINTER, DIMENSION(:,:) :: XZ0_WITH_SNOW  ! roughness length for momentum
                                                  ! for vegetation and snow    (m)
  REAL, POINTER, DIMENSION(:,:) :: XZ0H_WITH_SNOW ! roughness length for heat
                                                  ! for vegetation and snow    (m)
  REAL, POINTER, DIMENSION(:,:) :: XZ0EFF         ! effective roughness length for heat
                                                  ! for vegetation and snow    (m)
!
!* averaged variables
!
  REAL, POINTER, DIMENSION(:)   :: XAVG_RI       ! Bulk-Richardson number           (-)
  REAL, POINTER, DIMENSION(:)   :: XAVG_CD       ! drag coefficient for wind        (W/s2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_CH       ! drag coefficient for heat        (W/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_CE       ! drag coefficient for vapor       (W/s/K)
  REAL, POINTER, DIMENSION(:)   :: XAVG_RN       ! net radiation at surface         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_H        ! sensible heat flux               (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LE       ! total latent heat flux           (W/m2) 
  REAL, POINTER, DIMENSION(:)   :: XAVG_LEI      ! sublimation latent heat flux     (W/m2) 
  REAL, POINTER, DIMENSION(:)   :: XAVG_GFLUX    ! net soil-vegetation flux         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_TS       ! surface temperature              (K)
  REAL, POINTER, DIMENSION(:)   :: XAVG_T2M      ! temperature at 2 meters          (K)
  REAL, POINTER, DIMENSION(:)   :: XAVG_T2M_MIN  ! Minimum temperature at 2 meters          (K)
  REAL, POINTER, DIMENSION(:)   :: XAVG_T2M_MAX  ! Maximum temperature at 2 meters          (K)
  REAL, POINTER, DIMENSION(:)   :: XAVG_Q2M      ! humidity    at 2 meters          (kg/kg)
  REAL, POINTER, DIMENSION(:)   :: XAVG_HU2M     ! relative humidity at 2 meters    (-)
  REAL, POINTER, DIMENSION(:)   :: XAVG_HU2M_MIN ! Minimum relative humidity at 2 meters    (-)
  REAL, POINTER, DIMENSION(:)   :: XAVG_HU2M_MAX ! Maximum relative humidity at 2 meters    (-)
  REAL, POINTER, DIMENSION(:)   :: XAVG_QS       ! humidity at surface              (kg/kg)
  REAL, POINTER, DIMENSION(:)   :: XAVG_ZON10M   ! zonal wind at 10 meters          (m/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_MER10M   ! meridian wind at 10 meters       (m/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_WIND10M  ! wind at 10 meters                (m/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_WIND10M_MAX  ! Maximum wind at 10 meters    (m/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_SFCO2    ! CO2 flux                         (m/s*kg_CO2/kg_air)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LWD      ! downward long wave radiation     (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LWU      ! upward long wave radiation       (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_SWD      ! downward short wave radiation    (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_SWU      ! upward short wave radiation      (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XAVG_SWBD     ! downward short wave radiation by spectral band   (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XAVG_SWBU     ! upward short wave radiation by spectral band (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_FMU      ! horizontal momentum flux zonal   (m2/s2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_FMV      ! horizontal momentum flux meridian (m2/s2) 
  REAL, POINTER, DIMENSION(:)   :: XAVG_LWDC     ! downward long wave radiation     (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LWUC     ! upward long wave radiation       (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_SWDC     ! downward short wave radiation    (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_SWUC     ! upward short wave radiation      (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_FMUC     ! horizontal momentum flux zonal   (Pa.s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_FMVC     ! horizontal momentum flux meridian (Pa.s)             
  
  !                                                
  REAL, POINTER, DIMENSION(:)   :: XAVG_Z0       ! roughness length for momentum
                                                 ! for vegetation and snow    (m)
  REAL, POINTER, DIMENSION(:)   :: XAVG_Z0H      ! roughness length for heat
                                                 ! for vegetation and snow    (m)
  REAL, POINTER, DIMENSION(:)   :: XAVG_Z0EFF    ! effective roughness length for heat
                                                 ! for vegetation and snow    (m)
!------------------------------------------------------------------------------
!

END TYPE DIAG_ISBA_t



 CONTAINS

!





SUBROUTINE DIAG_ISBA_INIT(YDIAG_ISBA)
TYPE(DIAG_ISBA_t), INTENT(INOUT) :: YDIAG_ISBA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_ISBA_N:DIAG_ISBA_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YDIAG_ISBA%XRI)
  NULLIFY(YDIAG_ISBA%XCD)
  NULLIFY(YDIAG_ISBA%XCH)
  NULLIFY(YDIAG_ISBA%XCE)
  NULLIFY(YDIAG_ISBA%XRN)
  NULLIFY(YDIAG_ISBA%XH)
  NULLIFY(YDIAG_ISBA%XLE)
  NULLIFY(YDIAG_ISBA%XLEI)
  NULLIFY(YDIAG_ISBA%XGFLUX)
  NULLIFY(YDIAG_ISBA%XTS)
  NULLIFY(YDIAG_ISBA%XTSRAD)
  NULLIFY(YDIAG_ISBA%XT2M)
  NULLIFY(YDIAG_ISBA%XT2M_MIN)
  NULLIFY(YDIAG_ISBA%XT2M_MAX)
  NULLIFY(YDIAG_ISBA%XQ2M)
  NULLIFY(YDIAG_ISBA%XHU2M)
  NULLIFY(YDIAG_ISBA%XQS)
  NULLIFY(YDIAG_ISBA%XZON10M)
  NULLIFY(YDIAG_ISBA%XMER10M)
  NULLIFY(YDIAG_ISBA%XWIND10M)
  NULLIFY(YDIAG_ISBA%XLWD)
  NULLIFY(YDIAG_ISBA%XLWU)
  NULLIFY(YDIAG_ISBA%XSWD)
  NULLIFY(YDIAG_ISBA%XSWU)
  NULLIFY(YDIAG_ISBA%XSWBD)
  NULLIFY(YDIAG_ISBA%XSWBU)
  NULLIFY(YDIAG_ISBA%XFMU)
  NULLIFY(YDIAG_ISBA%XFMV)
  NULLIFY(YDIAG_ISBA%XSWDC)
  NULLIFY(YDIAG_ISBA%XSWUC)
  NULLIFY(YDIAG_ISBA%XLWDC)
  NULLIFY(YDIAG_ISBA%XLWUC)
  NULLIFY(YDIAG_ISBA%XFMUC)
  NULLIFY(YDIAG_ISBA%XFMVC)
  NULLIFY(YDIAG_ISBA%XZ0_WITH_SNOW)
  NULLIFY(YDIAG_ISBA%XZ0H_WITH_SNOW)
  NULLIFY(YDIAG_ISBA%XZ0EFF)
  NULLIFY(YDIAG_ISBA%XAVG_RI)
  NULLIFY(YDIAG_ISBA%XAVG_CD)
  NULLIFY(YDIAG_ISBA%XAVG_CH)
  NULLIFY(YDIAG_ISBA%XAVG_CE)
  NULLIFY(YDIAG_ISBA%XAVG_RN)
  NULLIFY(YDIAG_ISBA%XAVG_H)
  NULLIFY(YDIAG_ISBA%XAVG_LE)
  NULLIFY(YDIAG_ISBA%XAVG_LEI)
  NULLIFY(YDIAG_ISBA%XAVG_GFLUX)
  NULLIFY(YDIAG_ISBA%XAVG_TS)
  NULLIFY(YDIAG_ISBA%XAVG_T2M)
  NULLIFY(YDIAG_ISBA%XAVG_T2M_MIN)
  NULLIFY(YDIAG_ISBA%XAVG_T2M_MAX)
  NULLIFY(YDIAG_ISBA%XAVG_Q2M)
  NULLIFY(YDIAG_ISBA%XAVG_HU2M)
  NULLIFY(YDIAG_ISBA%XAVG_HU2M_MIN)
  NULLIFY(YDIAG_ISBA%XAVG_HU2M_MAX)
  NULLIFY(YDIAG_ISBA%XAVG_QS)
  NULLIFY(YDIAG_ISBA%XAVG_ZON10M)
  NULLIFY(YDIAG_ISBA%XAVG_MER10M)
  NULLIFY(YDIAG_ISBA%XAVG_WIND10M)
  NULLIFY(YDIAG_ISBA%XAVG_WIND10M_MAX)
  NULLIFY(YDIAG_ISBA%XAVG_SFCO2)
  NULLIFY(YDIAG_ISBA%XAVG_LWD)
  NULLIFY(YDIAG_ISBA%XAVG_LWU)
  NULLIFY(YDIAG_ISBA%XAVG_SWD)
  NULLIFY(YDIAG_ISBA%XAVG_SWU)
  NULLIFY(YDIAG_ISBA%XAVG_SWBD)
  NULLIFY(YDIAG_ISBA%XAVG_SWBU)
  NULLIFY(YDIAG_ISBA%XAVG_FMU)
  NULLIFY(YDIAG_ISBA%XAVG_FMV)
  NULLIFY(YDIAG_ISBA%XAVG_LWDC)
  NULLIFY(YDIAG_ISBA%XAVG_LWUC)
  NULLIFY(YDIAG_ISBA%XAVG_SWDC)
  NULLIFY(YDIAG_ISBA%XAVG_SWUC)
  NULLIFY(YDIAG_ISBA%XAVG_FMUC)
  NULLIFY(YDIAG_ISBA%XAVG_FMVC)
  NULLIFY(YDIAG_ISBA%XAVG_Z0)
  NULLIFY(YDIAG_ISBA%XAVG_Z0H)
  NULLIFY(YDIAG_ISBA%XAVG_Z0EFF)
YDIAG_ISBA%XDIAG_TSTEP=0.
YDIAG_ISBA%N2M=0
YDIAG_ISBA%L2M_MIN_ZS=.FALSE.
YDIAG_ISBA%LSURF_BUDGET=.FALSE.
YDIAG_ISBA%LRAD_BUDGET=.FALSE.
YDIAG_ISBA%LCOEF=.FALSE.
YDIAG_ISBA%LSURF_VARS=.FALSE.
YDIAG_ISBA%LPGD=.FALSE.
YDIAG_ISBA%LPATCH_BUDGET=.FALSE.
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_ISBA_N:DIAG_ISBA_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DIAG_ISBA_INIT


END MODULE MODD_DIAG_ISBA_n
