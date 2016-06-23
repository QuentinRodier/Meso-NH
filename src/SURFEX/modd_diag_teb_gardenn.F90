!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!######################
MODULE MODD_DIAG_TEB_GARDEN_n
!######################
!
!!****  *MODD_DIAG_TEB_GARDEN - declaration of diagnostics for ISBA scheme
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
!!      V. Masson   *Meteo France
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

TYPE DIAG_TEB_GARDEN_t
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
  REAL, POINTER, DIMENSION(:)   :: XIRRIG_FLUX   ! irrigation rate (as soil input)              (kg/m2/s)

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
  REAL, POINTER, DIMENSION(:)   :: XFAPAR        ! Fapar of vegetation
  REAL, POINTER, DIMENSION(:)   :: XFAPIR        ! Fapir of vegetation
  REAL, POINTER, DIMENSION(:)   :: XDFAPARC      ! Fapar of vegetation (cumul)
  REAL, POINTER, DIMENSION(:)   :: XDFAPIRC      ! Fapir of vegetation (cumul)
  REAL, POINTER, DIMENSION(:)   :: XFAPAR_BS     ! Fapar of bare soil
  REAL, POINTER, DIMENSION(:)   :: XFAPIR_BS     ! Fapir of bare soil
  REAL, POINTER, DIMENSION(:)   :: XDLAI_EFFC    ! Effective LAI (cumul)
!
!------------------------------------------------------------------------------
!

END TYPE DIAG_TEB_GARDEN_t



 CONTAINS

!




SUBROUTINE DIAG_TEB_GARDEN_INIT(YDIAG_TEB_GARDEN)
TYPE(DIAG_TEB_GARDEN_t), INTENT(INOUT) :: YDIAG_TEB_GARDEN
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_TEB_GARDEN_N:DIAG_TEB_GARDEN_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YDIAG_TEB_GARDEN%XRI)
  NULLIFY(YDIAG_TEB_GARDEN%XCD)
  NULLIFY(YDIAG_TEB_GARDEN%XCH)
  NULLIFY(YDIAG_TEB_GARDEN%XCE)
  NULLIFY(YDIAG_TEB_GARDEN%XRN)
  NULLIFY(YDIAG_TEB_GARDEN%XH)
  NULLIFY(YDIAG_TEB_GARDEN%XGFLUX)
  NULLIFY(YDIAG_TEB_GARDEN%XTS)
  NULLIFY(YDIAG_TEB_GARDEN%XTSRAD)
  NULLIFY(YDIAG_TEB_GARDEN%XQS)
  NULLIFY(YDIAG_TEB_GARDEN%XLWD)
  NULLIFY(YDIAG_TEB_GARDEN%XLWU)
  NULLIFY(YDIAG_TEB_GARDEN%XSWD)
  NULLIFY(YDIAG_TEB_GARDEN%XSWU)
  NULLIFY(YDIAG_TEB_GARDEN%XSWBD)
  NULLIFY(YDIAG_TEB_GARDEN%XSWBU)
  NULLIFY(YDIAG_TEB_GARDEN%XFMU)
  NULLIFY(YDIAG_TEB_GARDEN%XFMV)
  NULLIFY(YDIAG_TEB_GARDEN%XZ0_WITH_SNOW)
  NULLIFY(YDIAG_TEB_GARDEN%XZ0H_WITH_SNOW)
  NULLIFY(YDIAG_TEB_GARDEN%XZ0EFF)
  NULLIFY(YDIAG_TEB_GARDEN%XLEI)
  NULLIFY(YDIAG_TEB_GARDEN%XLEG)
  NULLIFY(YDIAG_TEB_GARDEN%XLEGI)
  NULLIFY(YDIAG_TEB_GARDEN%XLEV)
  NULLIFY(YDIAG_TEB_GARDEN%XLES)
  NULLIFY(YDIAG_TEB_GARDEN%XLER)
  NULLIFY(YDIAG_TEB_GARDEN%XLETR)
  NULLIFY(YDIAG_TEB_GARDEN%XEVAP)
  NULLIFY(YDIAG_TEB_GARDEN%XDRAIN)
  NULLIFY(YDIAG_TEB_GARDEN%XRUNOFF)
  NULLIFY(YDIAG_TEB_GARDEN%XHORT)
  NULLIFY(YDIAG_TEB_GARDEN%XRRVEG)
  NULLIFY(YDIAG_TEB_GARDEN%XMELT)
  NULLIFY(YDIAG_TEB_GARDEN%XDRIP)
  NULLIFY(YDIAG_TEB_GARDEN%XIRRIG_FLUX)
  NULLIFY(YDIAG_TEB_GARDEN%XCG)
  NULLIFY(YDIAG_TEB_GARDEN%XC1)
  NULLIFY(YDIAG_TEB_GARDEN%XC2)
  NULLIFY(YDIAG_TEB_GARDEN%XWGEQ)
  NULLIFY(YDIAG_TEB_GARDEN%XCT)
  NULLIFY(YDIAG_TEB_GARDEN%XRS)
  NULLIFY(YDIAG_TEB_GARDEN%XCDN)
  NULLIFY(YDIAG_TEB_GARDEN%XHU)
  NULLIFY(YDIAG_TEB_GARDEN%XHUG)
  NULLIFY(YDIAG_TEB_GARDEN%XRESTORE)
  NULLIFY(YDIAG_TEB_GARDEN%XUSTAR)
  NULLIFY(YDIAG_TEB_GARDEN%XIACAN)
  NULLIFY(YDIAG_TEB_GARDEN%XSNOWTEMP)
  NULLIFY(YDIAG_TEB_GARDEN%XSNOWLIQ)
  NULLIFY(YDIAG_TEB_GARDEN%XSNOWDZ)
  NULLIFY(YDIAG_TEB_GARDEN%XSNOWHMASS)
  NULLIFY(YDIAG_TEB_GARDEN%XMELTADV)
  NULLIFY(YDIAG_TEB_GARDEN%XHV)
  NULLIFY(YDIAG_TEB_GARDEN%XSWI)
  NULLIFY(YDIAG_TEB_GARDEN%XTSWI)
  NULLIFY(YDIAG_TEB_GARDEN%XTWSNOW)
  NULLIFY(YDIAG_TEB_GARDEN%XTDSNOW)
  NULLIFY(YDIAG_TEB_GARDEN%XALBT)
  NULLIFY(YDIAG_TEB_GARDEN%XEMIST)
  NULLIFY(YDIAG_TEB_GARDEN%XSEUIL)
  NULLIFY(YDIAG_TEB_GARDEN%XGPP)
  NULLIFY(YDIAG_TEB_GARDEN%XRESP_AUTO)
  NULLIFY(YDIAG_TEB_GARDEN%XRESP_ECO)
  NULLIFY(YDIAG_TEB_GARDEN%XFAPAR)
  NULLIFY(YDIAG_TEB_GARDEN%XFAPIR)
  NULLIFY(YDIAG_TEB_GARDEN%XDFAPARC)
  NULLIFY(YDIAG_TEB_GARDEN%XDFAPIRC)
  NULLIFY(YDIAG_TEB_GARDEN%XFAPAR_BS)
  NULLIFY(YDIAG_TEB_GARDEN%XFAPIR_BS)
  NULLIFY(YDIAG_TEB_GARDEN%XDLAI_EFFC)
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_TEB_GARDEN_N:DIAG_TEB_GARDEN_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DIAG_TEB_GARDEN_INIT


END MODULE MODD_DIAG_TEB_GARDEN_n
