!MNH_LIC Copyright 2019-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ################
      MODULE MODD_FIRE
!     ################
!
!!****  *MODD_FIRE* - declaration of Fire model parameters
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare Fire model parameters for all models.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!  A. Costes   *Meteo France/Cerfacs*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      24/10/2019
!---------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
USE MODD_PRECISION,  ONLY: MNHTIME

IMPLICIT NONE

SAVE

INTEGER, PARAMETER :: NFIREENTRIES = 22

TYPE TFIRE_t
  REAL, DIMENSION(:,:,:), POINTER :: XGRADLSPHIX    =>NULL()   ! Grad of phi on x direction
  REAL, DIMENSION(:,:,:), POINTER :: XGRADLSPHIY    =>NULL()   ! Grad of phi on y direction
  REAL, DIMENSION(:,:,:), POINTER :: XFIREWIND      =>NULL()   ! Surface wind speed in spread direction
  REAL, DIMENSION(:,:)  , POINTER :: XLSPHI2D       =>NULL()   ! Phi on 2d grid for computation
  REAL, DIMENSION(:,:)  , POINTER :: XGRADLSPHIX2D  =>NULL()   ! Grad of phi on x direction on 2d grid
  REAL, DIMENSION(:,:)  , POINTER :: XGRADLSPHIY2D  =>NULL()   ! Grad of phi on y direction on 2d grid
  REAL, DIMENSION(:,:)  , POINTER :: XGRADMASKX     =>NULL()   ! Grad mask x
  REAL, DIMENSION(:,:)  , POINTER :: XGRADMASKY     =>NULL()   ! Grad mask y
  REAL, DIMENSION(:,:)  , POINTER :: XSURFRATIO2D   =>NULL()   ! Burnt surface ratio
  REAL, DIMENSION(:,:)  , POINTER :: XLSDIFFUX2D    =>NULL()   ! LS diffusion x
  REAL, DIMENSION(:,:)  , POINTER :: XLSDIFFUY2D    =>NULL()   ! LS diffusion y
  REAL, DIMENSION(:,:)  , POINTER :: XFIRERW2D      =>NULL()   ! ROS woth wind and slope 2d
  !
END TYPE TFIRE_t

TYPE(TFIRE_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: TFIRE_MODEL

! Blaze fire model declarations
REAL, DIMENSION(:,:,:),   POINTER :: XLSPHI         =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XBMAP          =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMRFA         =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMR0          =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMR00         =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMWF0         =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMIGNITION    =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMFUELTYPE    =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFIRETAU       =>NULL()
REAL, DIMENSION(:,:,:,:), POINTER :: XFLUXPARAMH    =>NULL()
REAL, DIMENSION(:,:,:,:), POINTER :: XFLUXPARAMW    =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFIRERW        =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMASE         =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMAWC         =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMWALKIG      =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMFLUXHDH     =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMFLUXHDW     =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMHWS         =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMWINDU       =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMWINDV       =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMWINDW       =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XGRADLSPHIX    =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XGRADLSPHIY    =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFIREWIND      =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMGRADOROX    =>NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMGRADOROY    =>NULL()
!! fire grid Blaze declarations
REAL, DIMENSION(:,:),     POINTER :: XLSPHI2D       =>NULL()
REAL, DIMENSION(:,:),     POINTER :: XGRADLSPHIX2D  =>NULL()
REAL, DIMENSION(:,:),     POINTER :: XGRADLSPHIY2D  =>NULL()
REAL, DIMENSION(:,:),     POINTER :: XGRADMASKX     =>NULL()
REAL, DIMENSION(:,:),     POINTER :: XGRADMASKY     =>NULL()
REAL, DIMENSION(:,:),     POINTER :: XSURFRATIO2D   =>NULL()
REAL, DIMENSION(:,:),     POINTER :: XLSDIFFUX2D    =>NULL()
REAL, DIMENSION(:,:),     POINTER :: XLSDIFFUY2D    =>NULL()
REAL, DIMENSION(:,:),     POINTER :: XFIRERW2D      =>NULL()


LOGICAL           :: LBLAZE               ! Flag for Fire model use, default FALSE

CHARACTER(LEN=11) :: CPROPAG_MODEL        ! Fire propagation model (default SANTONI2011)
CHARACTER(LEN=3)  :: CHEAT_FLUX_MODEL     ! Sensible heat flux injection model (default CST)
CHARACTER(LEN=3)  :: CLATENT_FLUX_MODEL   ! latent heat flux injection model (default CST)

CHARACTER(LEN=7)  :: CFIRE_CPL_MODE       ! Coupling mode (default 2WAYCPL)

CHARACTER(LEN=28) :: CBMAPFILE            ! BMap file for FIR2ATM mode (default INIFILE)
LOGICAL           :: LINTERPWIND          ! Flag for wind interpolation
LOGICAL           :: LSGBAWEIGHT          ! Flag for use of weighted average method for SubGrid Burning Area computation

INTEGER           :: NFIRE_WENO_ORDER     ! Weno order (1,3,5)
INTEGER           :: NFIRE_RK_ORDER       ! Runge Kutta order (1,2,3,4)

INTEGER           :: NREFINX              ! Refinement ratio X
INTEGER           :: NREFINY              ! Refinement ratio Y

REAL              :: XCFLMAXFIRE          ! Maximum CFL on fire mesh
REAL              :: XLSDIFFUSION         ! Numerical diffusion of LevelSet
REAL              :: XROSDIFFUSION        ! Numerical diffusion of ROS

REAL              :: XFERR                ! Flamming Energy Release ratio (between 0.5 <= FERR < 1)

REAL              :: XFLUXZEXT            ! Flux distribution on vertical caracteristic length
REAL              :: XFLUXZMAX            ! Flux distribution on vertical max injetion height

REAL              :: XFLXCOEFTMP          ! Flux multiplicator. For testing

LOGICAL           :: LWINDFILTER          ! Fire wind filtering flag
CHARACTER(LEN=4)  :: CWINDFILTER          ! Wind filter method (EWAM or WLIM)
REAL              :: XEWAMTAU             ! Time averaging constant for EWAM method (s)
REAL              :: XWLIMUTH             ! Thresehold wind value for WLIM method (m/s)
REAL              :: XWLIMUTMAX           ! Maximum wind value for WLIM method (m/s) (needs to be >= XWLIMUTH )

INTEGER           :: NWINDSLOPECPLMODE    ! Flag for use of wind/slope in ROS (0=wind + slope, 1=wind only, 2=slope only (U0=0))

INTEGER           :: NNBSMOKETRACER
!
! Parameters not in the namelist
!
REAL,               DIMENSION(2) :: XFIREMESHSIZE     ! Fire Mesh size [dxf,dyf]
REAL(KIND=MNHTIME), DIMENSION(2) :: XFIREPERF         ! Blaze fire model performance
REAL(KIND=MNHTIME), DIMENSION(2) :: XGRADPERF         ! Grad computation performance
REAL(KIND=MNHTIME), DIMENSION(2) :: XROSWINDPERF      ! ROS and wind interpolation computation performance
REAL(KIND=MNHTIME), DIMENSION(2) :: XPROPAGPERF       ! Propagation computation performance
REAL(KIND=MNHTIME), DIMENSION(2) :: XFLUXPERF         ! Heat fluxes computation performance
LOGICAL                          :: LRESTA_ASE        ! Flag for using ASE in RESTA file
LOGICAL                          :: LRESTA_AWC        ! Flag for using AWC in RESTA file
LOGICAL                          :: LRESTA_EWAM       ! Flag for using EWAM in RESTA file
LOGICAL                          :: LRESTA_WLIM       ! Flag for using WLIM in RESTA file


CONTAINS

SUBROUTINE FIRE_GOTO_MODEL(KFROM, KTO)
  INTEGER, INTENT(IN) :: KFROM, KTO

  ! Save current state for allocated arrays
  TFIRE_MODEL(KFROM)%XGRADLSPHIY    => XGRADLSPHIX
  TFIRE_MODEL(KFROM)%XGRADLSPHIY    => XGRADLSPHIY
  TFIRE_MODEL(KFROM)%XFIREWIND      => XFIREWIND
  !! 2d Blaze
  TFIRE_MODEL(KFROM)%XLSPHI2D       => XLSPHI2D
  TFIRE_MODEL(KFROM)%XGRADLSPHIX2D  => XGRADLSPHIX2D
  TFIRE_MODEL(KFROM)%XGRADLSPHIY2D  => XGRADLSPHIY2D
  TFIRE_MODEL(KFROM)%XGRADMASKX     => XGRADMASKX
  TFIRE_MODEL(KFROM)%XGRADMASKY     => XGRADMASKY
  TFIRE_MODEL(KFROM)%XSURFRATIO2D   => XSURFRATIO2D
  TFIRE_MODEL(KFROM)%XLSDIFFUX2D    => XLSDIFFUX2D
  TFIRE_MODEL(KFROM)%XLSDIFFUY2D    => XLSDIFFUY2D
  TFIRE_MODEL(KFROM)%XFIRERW2D      => XFIRERW2D
  !
  ! Current model is set to model KTO
  XGRADLSPHIX   => TFIRE_MODEL(KTO)%XGRADLSPHIX
  XGRADLSPHIY   => TFIRE_MODEL(KTO)%XGRADLSPHIY
  XFIREWIND     => TFIRE_MODEL(KTO)%XFIREWIND
  !! 2d Blaze
  XLSPHI2D      => TFIRE_MODEL(KTO)%XLSPHI2D
  XGRADLSPHIX2D => TFIRE_MODEL(KTO)%XGRADLSPHIX2D
  XGRADLSPHIY2D => TFIRE_MODEL(KTO)%XGRADLSPHIY2D
  XGRADMASKX    => TFIRE_MODEL(KTO)%XGRADMASKX
  XGRADMASKY    => TFIRE_MODEL(KTO)%XGRADMASKY
  XSURFRATIO2D  => TFIRE_MODEL(KTO)%XSURFRATIO2D
  XLSDIFFUX2D   => TFIRE_MODEL(KTO)%XLSDIFFUX2D
  XLSDIFFUY2D   => TFIRE_MODEL(KTO)%XLSDIFFUY2D
  XFIRERW2D     => TFIRE_MODEL(KTO)%XFIRERW2D
END SUBROUTINE FIRE_GOTO_MODEL

END MODULE MODD_FIRE
