!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!######################
MODULE MODD_DIAG_TEB_GREENROOF_n
!######################
!
!!****  *MODD_DIAG_TEB_GREENROOF - declaration of diagnostics for ISBA scheme
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
!!    Based on modd_diag_teb_gardenn
!!
!!    AUTHOR
!!    ------
!!      C. de Munck & A. Lemonsu   *Meteo France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       07/2011
!
!*       0.   DECLARATIONS
!             ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE DIAG_TEB_GREENROOF_t
!------------------------------------------------------------------------------
!
!* variables for one patch
!
  REAL, POINTER, DIMENSION(:)   :: XRI     ! Bulk-Richardson number           (-)
  REAL, POINTER, DIMENSION(:)   :: XCD     ! drag coefficient for wind        (W/s2)
  REAL, POINTER, DIMENSION(:)   :: XCH     ! drag coefficient for heat        (W/s)
  REAL, POINTER, DIMENSION(:)   :: XCE     ! drag coefficient for vapor       (W/s/K)
  REAL, POINTER, DIMENSION(:)   :: XRN     ! net radiation at surface         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XH      ! sensible heat flux               (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XGFLUX  ! net soil-vegetation flux         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XTS     ! surface temperature              (K)
  REAL, POINTER, DIMENSION(:)   :: XTSRAD  ! radiative surface temperature    (K)
  REAL, POINTER, DIMENSION(:)   :: XQS     ! humidity at surface              (kg/kg)
  REAL, POINTER, DIMENSION(:)   :: XLWD    ! downward long wave radiation     (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLWU    ! upward long wave radiation       (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWD    ! downward short wave radiation    (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWU    ! upward short wave radiation      (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWBD ! downward short wave radiation by spectral band   (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWBU ! upward short wave radiation by spectral band (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XFMU    ! horizontal momentum flux zonal   (m2/s2)
  REAL, POINTER, DIMENSION(:)   :: XFMV    ! horizontal momentum flux meridian (m2/s2)  
  !    
  REAL, POINTER, DIMENSION(:)   :: XZ0_WITH_SNOW  ! roughness length for momentum
                                                  ! for vegetation and snow    (m)
  REAL, POINTER, DIMENSION(:)   :: XZ0H_WITH_SNOW ! roughness length for heat
                                                  ! for vegetation and snow    (m)
  REAL, POINTER, DIMENSION(:)   :: XZ0EFF         ! effective roughness length for heat
                                                  ! for vegetation and snow    (m)
!
  REAL, POINTER, DIMENSION(:)   :: XLEI          ! sublimation latent heat flux     (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLEG          ! latent heat of evaporation over the ground   (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLEGI         ! surface soil ice sublimation                 (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLEV          ! latent heat of evaporation over vegetation   (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLES          ! latent heat of evaporation over the snow     (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLER          ! evaporation from canopy water interception   (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLETR         ! evapotranspiration of the vegetation         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XEVAP         ! evapotranspiration                           (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XDRAIN        ! soil drainage flux                           (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XRUNOFF       ! sub-grid and supersaturation runoff          (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XHORT         ! sub-grid Horton runoff from the SGH scheme   (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XRRVEG        !  precipitation intercepted by the vegetation (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XMELT         ! snow melt                                    (kg/m2/s)       
  REAL, POINTER, DIMENSION(:)   :: XDRIP         ! dripping from the vegetation reservoir       (kg/m2/s)  
!
!* pack diag
!
  REAL, POINTER, DIMENSION(:)   :: XCG           ! heat capacity of the ground
  REAL, POINTER, DIMENSION(:)   :: XC1           ! coefficients for the moisure
  REAL, POINTER, DIMENSION(:)   :: XC2           ! equation.
  REAL, POINTER, DIMENSION(:)   :: XWGEQ         ! equilibrium volumetric water content
  REAL, POINTER, DIMENSION(:)   :: XCT           ! area-averaged heat capacity
  REAL, POINTER, DIMENSION(:)   :: XRS           ! stomatal resistance                            (s/m)
  REAL, POINTER, DIMENSION(:)   :: XCDN          ! neutral drag coefficient                      (-)
  REAL, POINTER, DIMENSION(:)   :: XHU           ! area averaged surface humidity coefficient    (-)
  REAL, POINTER, DIMENSION(:)   :: XHUG          ! baresoil surface humidity coefficient         (-)
  REAL, POINTER, DIMENSION(:)   :: XRESTORE      ! surface energy budget restore term            (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XUSTAR        ! friction velocity                             (m/s)
  REAL, POINTER, DIMENSION(:,:) :: XIACAN        ! PAR in the canopy at different gauss level    (micmolphot/m2/s)
!
! for ISBA-ES:3-L
  REAL, POINTER, DIMENSION(:,:) :: XSNOWTEMP     ! snow temperature profile (ISBA-ES:3-L)        (K)
  REAL, POINTER, DIMENSION(:,:) :: XSNOWLIQ      ! snow liquid water profile (ISBA-ES:3-L)       (m)
  REAL, POINTER, DIMENSION(:,:) :: XSNOWDZ       ! snow layer thicknesses                        (m)
  REAL, POINTER, DIMENSION(:)   :: XSNOWHMASS    ! heat content change due to mass changes in snowpack (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XMELTADV      ! advective energy from snow melt water!
!
!* budget summation variables for one patch
!
!
  REAL, POINTER, DIMENSION(:)   :: XHV           ! Halstead coefficient
!      
  REAL, POINTER, DIMENSION(:,:) :: XSWI        ! Soil wetness index
  REAL, POINTER, DIMENSION(:,:) :: XTSWI       ! Total soil wetness index
!     
  REAL, POINTER, DIMENSION(:)   :: XTWSNOW       ! Total snow reservoir
  REAL, POINTER, DIMENSION(:)   :: XTDSNOW       ! Total snow height
!
  REAL, POINTER, DIMENSION(:)   :: XALBT             ! Total Albedo
  REAL, POINTER, DIMENSION(:)   :: XEMIST            ! averaged emissivity                     (-)
!
  REAL, POINTER, DIMENSION(:)   :: XSEUIL        ! Irrigation threshold
!
  REAL, POINTER, DIMENSION(:)   :: XGPP          ! Gross Primary Production
  REAL, POINTER, DIMENSION(:)   :: XRESP_AUTO    ! Autotrophic respiration
  REAL, POINTER, DIMENSION(:)   :: XRESP_ECO     ! Ecosystem respiration
!
!------------------------------------------------------------------------------
!

END TYPE DIAG_TEB_GREENROOF_t



 CONTAINS

!




SUBROUTINE DIAG_TEB_GREENROOF_INIT(YDIAG_TEB_GREENROOF)
TYPE(DIAG_TEB_GREENROOF_t), INTENT(INOUT) :: YDIAG_TEB_GREENROOF
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_TEB_GREENROOF_N:DIAG_TEB_GREENROOF_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YDIAG_TEB_GREENROOF%XRI)
  NULLIFY(YDIAG_TEB_GREENROOF%XCD)
  NULLIFY(YDIAG_TEB_GREENROOF%XCH)
  NULLIFY(YDIAG_TEB_GREENROOF%XCE)
  NULLIFY(YDIAG_TEB_GREENROOF%XRN)
  NULLIFY(YDIAG_TEB_GREENROOF%XH)
  NULLIFY(YDIAG_TEB_GREENROOF%XGFLUX)
  NULLIFY(YDIAG_TEB_GREENROOF%XTS)
  NULLIFY(YDIAG_TEB_GREENROOF%XTSRAD)
  NULLIFY(YDIAG_TEB_GREENROOF%XQS)
  NULLIFY(YDIAG_TEB_GREENROOF%XLWD)
  NULLIFY(YDIAG_TEB_GREENROOF%XLWU)
  NULLIFY(YDIAG_TEB_GREENROOF%XSWD)
  NULLIFY(YDIAG_TEB_GREENROOF%XSWU)
  NULLIFY(YDIAG_TEB_GREENROOF%XSWBD)
  NULLIFY(YDIAG_TEB_GREENROOF%XSWBU)
  NULLIFY(YDIAG_TEB_GREENROOF%XFMU)
  NULLIFY(YDIAG_TEB_GREENROOF%XFMV)
  NULLIFY(YDIAG_TEB_GREENROOF%XZ0_WITH_SNOW)
  NULLIFY(YDIAG_TEB_GREENROOF%XZ0H_WITH_SNOW)
  NULLIFY(YDIAG_TEB_GREENROOF%XZ0EFF)
  NULLIFY(YDIAG_TEB_GREENROOF%XLEI)
  NULLIFY(YDIAG_TEB_GREENROOF%XLEG)
  NULLIFY(YDIAG_TEB_GREENROOF%XLEGI)
  NULLIFY(YDIAG_TEB_GREENROOF%XLEV)
  NULLIFY(YDIAG_TEB_GREENROOF%XLES)
  NULLIFY(YDIAG_TEB_GREENROOF%XLER)
  NULLIFY(YDIAG_TEB_GREENROOF%XLETR)
  NULLIFY(YDIAG_TEB_GREENROOF%XEVAP)
  NULLIFY(YDIAG_TEB_GREENROOF%XDRAIN)
  NULLIFY(YDIAG_TEB_GREENROOF%XRUNOFF)
  NULLIFY(YDIAG_TEB_GREENROOF%XHORT)
  NULLIFY(YDIAG_TEB_GREENROOF%XRRVEG)
  NULLIFY(YDIAG_TEB_GREENROOF%XMELT)
  NULLIFY(YDIAG_TEB_GREENROOF%XDRIP)
  NULLIFY(YDIAG_TEB_GREENROOF%XCG)
  NULLIFY(YDIAG_TEB_GREENROOF%XC1)
  NULLIFY(YDIAG_TEB_GREENROOF%XC2)
  NULLIFY(YDIAG_TEB_GREENROOF%XWGEQ)
  NULLIFY(YDIAG_TEB_GREENROOF%XCT)
  NULLIFY(YDIAG_TEB_GREENROOF%XRS)
  NULLIFY(YDIAG_TEB_GREENROOF%XCDN)
  NULLIFY(YDIAG_TEB_GREENROOF%XHU)
  NULLIFY(YDIAG_TEB_GREENROOF%XHUG)
  NULLIFY(YDIAG_TEB_GREENROOF%XRESTORE)
  NULLIFY(YDIAG_TEB_GREENROOF%XUSTAR)
  NULLIFY(YDIAG_TEB_GREENROOF%XIACAN)
  NULLIFY(YDIAG_TEB_GREENROOF%XSNOWTEMP)
  NULLIFY(YDIAG_TEB_GREENROOF%XSNOWLIQ)
  NULLIFY(YDIAG_TEB_GREENROOF%XSNOWDZ)
  NULLIFY(YDIAG_TEB_GREENROOF%XSNOWHMASS)
  NULLIFY(YDIAG_TEB_GREENROOF%XMELTADV)
  NULLIFY(YDIAG_TEB_GREENROOF%XHV)
  NULLIFY(YDIAG_TEB_GREENROOF%XSWI)
  NULLIFY(YDIAG_TEB_GREENROOF%XTSWI)
  NULLIFY(YDIAG_TEB_GREENROOF%XTWSNOW)
  NULLIFY(YDIAG_TEB_GREENROOF%XTDSNOW)
  NULLIFY(YDIAG_TEB_GREENROOF%XALBT)
  NULLIFY(YDIAG_TEB_GREENROOF%XEMIST)
  NULLIFY(YDIAG_TEB_GREENROOF%XSEUIL)
  NULLIFY(YDIAG_TEB_GREENROOF%XGPP)
  NULLIFY(YDIAG_TEB_GREENROOF%XRESP_AUTO)
  NULLIFY(YDIAG_TEB_GREENROOF%XRESP_ECO)
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_TEB_GREENROOF_N:DIAG_TEB_GREENROOF_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DIAG_TEB_GREENROOF_INIT


END MODULE MODD_DIAG_TEB_GREENROOF_n
