!MNH_LIC Copyright 2019-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ################
      MODULE MODD_FIRE_n
!     ################
!
!!****  *MODD_FIRE_n* - declaration of Fire model parameters
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
!  P. Wautelet 15/02/2023: restructure for grid-nesting
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
  ! Blaze fire model declarations
!Managed by FIELDLIST_GOTO_MODEL   REAL, DIMENSION(:,:,:),   POINTER :: XLSPHI         => NULL()
!Managed by FIELDLIST_GOTO_MODEL   REAL, DIMENSION(:,:,:),   POINTER :: XBMAP          => NULL()
  REAL, DIMENSION(:,:,:),   POINTER :: XFMRFA         => NULL()
!Managed by FIELDLIST_GOTO_MODEL   REAL, DIMENSION(:,:,:),   POINTER :: XFMR0          => NULL()
  REAL, DIMENSION(:,:,:),   POINTER :: XFMR00         => NULL()
  REAL, DIMENSION(:,:,:),   POINTER :: XFMWF0         => NULL()
  REAL, DIMENSION(:,:,:),   POINTER :: XFMIGNITION    => NULL()
  REAL, DIMENSION(:,:,:),   POINTER :: XFMFUELTYPE    => NULL()
  REAL, DIMENSION(:,:,:),   POINTER :: XFIRETAU       => NULL()
  REAL, DIMENSION(:,:,:,:), POINTER :: XFLUXPARAMH    => NULL()
  REAL, DIMENSION(:,:,:,:), POINTER :: XFLUXPARAMW    => NULL()
!Managed by FIELDLIST_GOTO_MODEL   REAL, DIMENSION(:,:,:),   POINTER :: XFIRERW        => NULL()
!Managed by FIELDLIST_GOTO_MODEL   REAL, DIMENSION(:,:,:),   POINTER :: XFMASE         => NULL()
!Managed by FIELDLIST_GOTO_MODEL   REAL, DIMENSION(:,:,:),   POINTER :: XFMAWC         => NULL()
  REAL, DIMENSION(:,:,:),   POINTER :: XFMWALKIG      => NULL()
!Managed by FIELDLIST_GOTO_MODEL   REAL, DIMENSION(:,:,:),   POINTER :: XFMFLUXHDH     => NULL()
!Managed by FIELDLIST_GOTO_MODEL   REAL, DIMENSION(:,:,:),   POINTER :: XFMFLUXHDW     => NULL()
!Managed by FIELDLIST_GOTO_MODEL   REAL, DIMENSION(:,:,:),   POINTER :: XFMHWS         => NULL()
!Managed by FIELDLIST_GOTO_MODEL   REAL, DIMENSION(:,:,:),   POINTER :: XFMWINDU       => NULL()
!Managed by FIELDLIST_GOTO_MODEL   REAL, DIMENSION(:,:,:),   POINTER :: XFMWINDV       => NULL()
!Managed by FIELDLIST_GOTO_MODEL   REAL, DIMENSION(:,:,:),   POINTER :: XFMWINDW       => NULL()
  REAL, DIMENSION(:,:,:),   POINTER :: XGRADLSPHIX    => NULL()   ! Grad of phi on x direction
  REAL, DIMENSION(:,:,:),   POINTER :: XGRADLSPHIY    => NULL()   ! Grad of phi on y direction
  REAL, DIMENSION(:,:,:),   POINTER :: XFIREWIND      => NULL()   ! Surface wind speed in spread direction
!Managed by FIELDLIST_GOTO_MODEL   REAL, DIMENSION(:,:,:),   POINTER :: XFMGRADOROX    => NULL()
!Managed by FIELDLIST_GOTO_MODEL   REAL, DIMENSION(:,:,:),   POINTER :: XFMGRADOROY    => NULL()
  !! fire grid Blaze declarations
  REAL, DIMENSION(:,:)  , POINTER :: XLSPHI2D       => NULL()   ! Phi on 2d grid for computation
  REAL, DIMENSION(:,:)  , POINTER :: XGRADLSPHIX2D  => NULL()   ! Grad of phi on x direction on 2d grid
  REAL, DIMENSION(:,:)  , POINTER :: XGRADLSPHIY2D  => NULL()   ! Grad of phi on y direction on 2d grid
  REAL, DIMENSION(:,:)  , POINTER :: XGRADMASKX     => NULL()   ! Grad mask x
  REAL, DIMENSION(:,:)  , POINTER :: XGRADMASKY     => NULL()   ! Grad mask y
  REAL, DIMENSION(:,:)  , POINTER :: XSURFRATIO2D   => NULL()   ! Burnt surface ratio
  REAL, DIMENSION(:,:)  , POINTER :: XLSDIFFUX2D    => NULL()   ! LS diffusion x
  REAL, DIMENSION(:,:)  , POINTER :: XLSDIFFUY2D    => NULL()   ! LS diffusion y
  REAL, DIMENSION(:,:)  , POINTER :: XFIRERW2D      => NULL()   ! ROS woth wind and slope 2d

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
END TYPE TFIRE_t

TYPE(TFIRE_t), DIMENSION(JPMODELMAX), TARGET :: TFIRE_MODEL

! Blaze fire model declarations
REAL, DIMENSION(:,:,:),   POINTER :: XLSPHI         => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XBMAP          => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMRFA         => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMR0          => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMR00         => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMWF0         => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMIGNITION    => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMFUELTYPE    => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFIRETAU       => NULL()
REAL, DIMENSION(:,:,:,:), POINTER :: XFLUXPARAMH    => NULL()
REAL, DIMENSION(:,:,:,:), POINTER :: XFLUXPARAMW    => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFIRERW        => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMASE         => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMAWC         => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMWALKIG      => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMFLUXHDH     => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMFLUXHDW     => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMHWS         => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMWINDU       => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMWINDV       => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMWINDW       => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XGRADLSPHIX    => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XGRADLSPHIY    => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFIREWIND      => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMGRADOROX    => NULL()
REAL, DIMENSION(:,:,:),   POINTER :: XFMGRADOROY    => NULL()
!! fire grid Blaze declarations
REAL, DIMENSION(:,:),     POINTER :: XLSPHI2D       => NULL()
REAL, DIMENSION(:,:),     POINTER :: XGRADLSPHIX2D  => NULL()
REAL, DIMENSION(:,:),     POINTER :: XGRADLSPHIY2D  => NULL()
REAL, DIMENSION(:,:),     POINTER :: XGRADMASKX     => NULL()
REAL, DIMENSION(:,:),     POINTER :: XGRADMASKY     => NULL()
REAL, DIMENSION(:,:),     POINTER :: XSURFRATIO2D   => NULL()
REAL, DIMENSION(:,:),     POINTER :: XLSDIFFUX2D    => NULL()
REAL, DIMENSION(:,:),     POINTER :: XLSDIFFUY2D    => NULL()
REAL, DIMENSION(:,:),     POINTER :: XFIRERW2D      => NULL()

LOGICAL,                          POINTER :: LBLAZE             => NULL()
CHARACTER(LEN=11),                POINTER :: CPROPAG_MODEL      => NULL()
CHARACTER(LEN=3),                 POINTER :: CHEAT_FLUX_MODEL   => NULL()
CHARACTER(LEN=3),                 POINTER :: CLATENT_FLUX_MODEL => NULL()
CHARACTER(LEN=7),                 POINTER :: CFIRE_CPL_MODE     => NULL()
CHARACTER(LEN=28),                POINTER :: CBMAPFILE          => NULL()
LOGICAL,                          POINTER :: LINTERPWIND        => NULL()
LOGICAL,                          POINTER :: LSGBAWEIGHT        => NULL()
INTEGER,                          POINTER :: NFIRE_WENO_ORDER   => NULL()
INTEGER,                          POINTER :: NFIRE_RK_ORDER     => NULL()
INTEGER,                          POINTER :: NREFINX            => NULL()
INTEGER,                          POINTER :: NREFINY            => NULL()
REAL,                             POINTER :: XCFLMAXFIRE        => NULL()
REAL,                             POINTER :: XLSDIFFUSION       => NULL()
REAL,                             POINTER :: XROSDIFFUSION      => NULL()
REAL,                             POINTER :: XFERR              => NULL()
REAL,                             POINTER :: XFLUXZEXT          => NULL()
REAL,                             POINTER :: XFLUXZMAX          => NULL()
REAL,                             POINTER :: XFLXCOEFTMP        => NULL()
LOGICAL,                          POINTER :: LWINDFILTER        => NULL()
CHARACTER(LEN=4),                 POINTER :: CWINDFILTER        => NULL()
REAL,                             POINTER :: XEWAMTAU           => NULL()
REAL,                             POINTER :: XWLIMUTH           => NULL()
REAL,                             POINTER :: XWLIMUTMAX         => NULL()
INTEGER,                          POINTER :: NWINDSLOPECPLMODE  => NULL()
INTEGER,                          POINTER :: NNBSMOKETRACER     => NULL()
REAL,               DIMENSION(:), POINTER :: XFIREMESHSIZE      => NULL()
REAL(KIND=MNHTIME), DIMENSION(:), POINTER :: XFIREPERF          => NULL()
REAL(KIND=MNHTIME), DIMENSION(:), POINTER :: XGRADPERF          => NULL()
REAL(KIND=MNHTIME), DIMENSION(:), POINTER :: XROSWINDPERF       => NULL()
REAL(KIND=MNHTIME), DIMENSION(:), POINTER :: XPROPAGPERF        => NULL()
REAL(KIND=MNHTIME), DIMENSION(:), POINTER :: XFLUXPERF          => NULL()
LOGICAL,                          POINTER :: LRESTA_ASE         => NULL()
LOGICAL,                          POINTER :: LRESTA_AWC         => NULL()
LOGICAL,                          POINTER :: LRESTA_EWAM        => NULL()
LOGICAL,                          POINTER :: LRESTA_WLIM        => NULL()


CONTAINS

SUBROUTINE FIRE_GOTO_MODEL( KFROM, KTO )
  INTEGER, INTENT(IN) :: KFROM, KTO

  ! Save current state for allocated arrays

  ! Blaze fire model declarations
!Managed by FIELDLIST_GOTO_MODEL   TFIRE_MODEL(KFROM)%XLSPHI             => XLSPHI
!Managed by FIELDLIST_GOTO_MODEL   TFIRE_MODEL(KFROM)%XBMAP              => XBMAP
  TFIRE_MODEL(KFROM)%XFMRFA             => XFMRFA
!Managed by FIELDLIST_GOTO_MODEL   TFIRE_MODEL(KFROM)%XFMR0              => XFMR0
  TFIRE_MODEL(KFROM)%XFMR00             => XFMR00
  TFIRE_MODEL(KFROM)%XFMWF0             => XFMWF0
  TFIRE_MODEL(KFROM)%XFMIGNITION        => XFMIGNITION
  TFIRE_MODEL(KFROM)%XFMFUELTYPE        => XFMFUELTYPE
  TFIRE_MODEL(KFROM)%XFIRETAU           => XFIRETAU
  TFIRE_MODEL(KFROM)%XFLUXPARAMH        => XFLUXPARAMH
  TFIRE_MODEL(KFROM)%XFLUXPARAMW        => XFLUXPARAMW
!Managed by FIELDLIST_GOTO_MODEL   TFIRE_MODEL(KFROM)%XFIRERW            => XFIRERW
!Managed by FIELDLIST_GOTO_MODEL   TFIRE_MODEL(KFROM)%XFMASE             => XFMASE
!Managed by FIELDLIST_GOTO_MODEL   TFIRE_MODEL(KFROM)%XFMAWC             => XFMAWC
  TFIRE_MODEL(KFROM)%XFMWALKIG          => XFMWALKIG
!Managed by FIELDLIST_GOTO_MODEL   TFIRE_MODEL(KFROM)%XFMFLUXHDH         => XFMFLUXHDH
!Managed by FIELDLIST_GOTO_MODEL   TFIRE_MODEL(KFROM)%XFMFLUXHDW         => XFMFLUXHDW
!Managed by FIELDLIST_GOTO_MODEL   TFIRE_MODEL(KFROM)%XFMHWS             => XFMHWS
!Managed by FIELDLIST_GOTO_MODEL   TFIRE_MODEL(KFROM)%XFMWINDU           => XFMWINDU
!Managed by FIELDLIST_GOTO_MODEL   TFIRE_MODEL(KFROM)%XFMWINDV           => XFMWINDV
!Managed by FIELDLIST_GOTO_MODEL   TFIRE_MODEL(KFROM)%XFMWINDW           => XFMWINDW
  TFIRE_MODEL(KFROM)%XGRADLSPHIX        => XGRADLSPHIX
  TFIRE_MODEL(KFROM)%XGRADLSPHIY        => XGRADLSPHIY
  TFIRE_MODEL(KFROM)%XFIREWIND          => XFIREWIND
!Managed by FIELDLIST_GOTO_MODEL   TFIRE_MODEL(KFROM)%XFMGRADOROX        => XFMGRADOROX
!Managed by FIELDLIST_GOTO_MODEL   TFIRE_MODEL(KFROM)%XFMGRADOROY        => XFMGRADOROY
  !! fire grid Blaze declarations
  TFIRE_MODEL(KFROM)%XLSPHI2D           => XLSPHI2D
  TFIRE_MODEL(KFROM)%XGRADLSPHIX2D      => XGRADLSPHIX2D
  TFIRE_MODEL(KFROM)%XGRADLSPHIY2D      => XGRADLSPHIY2D
  TFIRE_MODEL(KFROM)%XGRADMASKX         => XGRADMASKX
  TFIRE_MODEL(KFROM)%XGRADMASKY         => XGRADMASKY
  TFIRE_MODEL(KFROM)%XSURFRATIO2D       => XSURFRATIO2D
  TFIRE_MODEL(KFROM)%XLSDIFFUX2D        => XLSDIFFUX2D
  TFIRE_MODEL(KFROM)%XLSDIFFUY2D        => XLSDIFFUY2D
  TFIRE_MODEL(KFROM)%XFIRERW2D          => XFIRERW2D

  ! Current model is set to model KTO

  ! Blaze fire model declarations
!Managed by FIELDLIST_GOTO_MODEL   XLSPHI             => TFIRE_MODEL(KTO)%XLSPHI
!Managed by FIELDLIST_GOTO_MODEL   XBMAP              => TFIRE_MODEL(KTO)%XBMAP
  XFMRFA             => TFIRE_MODEL(KTO)%XFMRFA
!Managed by FIELDLIST_GOTO_MODEL   XFMR0              => TFIRE_MODEL(KTO)%XFMR0
  XFMR00             => TFIRE_MODEL(KTO)%XFMR00
  XFMWF0             => TFIRE_MODEL(KTO)%XFMWF0
  XFMIGNITION        => TFIRE_MODEL(KTO)%XFMIGNITION
  XFMFUELTYPE        => TFIRE_MODEL(KTO)%XFMFUELTYPE
  XFIRETAU           => TFIRE_MODEL(KTO)%XFIRETAU
  XFLUXPARAMH        => TFIRE_MODEL(KTO)%XFLUXPARAMH
  XFLUXPARAMW        => TFIRE_MODEL(KTO)%XFLUXPARAMW
!Managed by FIELDLIST_GOTO_MODEL   XFIRERW            => TFIRE_MODEL(KTO)%XFIRERW
!Managed by FIELDLIST_GOTO_MODEL   XFMASE             => TFIRE_MODEL(KTO)%XFMASE
!Managed by FIELDLIST_GOTO_MODEL   XFMAWC             => TFIRE_MODEL(KTO)%XFMAWC
  XFMWALKIG          => TFIRE_MODEL(KTO)%XFMWALKIG
!Managed by FIELDLIST_GOTO_MODEL   XFMFLUXHDH         => TFIRE_MODEL(KTO)%XFMFLUXHDH
!Managed by FIELDLIST_GOTO_MODEL   XFMFLUXHDW         => TFIRE_MODEL(KTO)%XFMFLUXHDW
!Managed by FIELDLIST_GOTO_MODEL   XFMHWS             => TFIRE_MODEL(KTO)%XFMHWS
!Managed by FIELDLIST_GOTO_MODEL   XFMWINDU           => TFIRE_MODEL(KTO)%XFMWINDU
!Managed by FIELDLIST_GOTO_MODEL   XFMWINDV           => TFIRE_MODEL(KTO)%XFMWINDV
!Managed by FIELDLIST_GOTO_MODEL   XFMWINDW           => TFIRE_MODEL(KTO)%XFMWINDW
  XGRADLSPHIX        => TFIRE_MODEL(KTO)%XGRADLSPHIX
  XGRADLSPHIY        => TFIRE_MODEL(KTO)%XGRADLSPHIY
  XFIREWIND          => TFIRE_MODEL(KTO)%XFIREWIND
!Managed by FIELDLIST_GOTO_MODEL   XFMGRADOROX        => TFIRE_MODEL(KTO)%XFMGRADOROX
!Managed by FIELDLIST_GOTO_MODEL   XFMGRADOROY        => TFIRE_MODEL(KTO)%XFMGRADOROY
  !! fire grid Blaze declarations
  XLSPHI2D           => TFIRE_MODEL(KTO)%XLSPHI2D
  XGRADLSPHIX2D      => TFIRE_MODEL(KTO)%XGRADLSPHIX2D
  XGRADLSPHIY2D      => TFIRE_MODEL(KTO)%XGRADLSPHIY2D
  XGRADMASKX         => TFIRE_MODEL(KTO)%XGRADMASKX
  XGRADMASKY         => TFIRE_MODEL(KTO)%XGRADMASKY
  XSURFRATIO2D       => TFIRE_MODEL(KTO)%XSURFRATIO2D
  XLSDIFFUX2D        => TFIRE_MODEL(KTO)%XLSDIFFUX2D
  XLSDIFFUY2D        => TFIRE_MODEL(KTO)%XLSDIFFUY2D
  XFIRERW2D          => TFIRE_MODEL(KTO)%XFIRERW2D

  LBLAZE             => TFIRE_MODEL(KTO)%LBLAZE
  CPROPAG_MODEL      => TFIRE_MODEL(KTO)%CPROPAG_MODEL
  CHEAT_FLUX_MODEL   => TFIRE_MODEL(KTO)%CHEAT_FLUX_MODEL
  CLATENT_FLUX_MODEL => TFIRE_MODEL(KTO)%CLATENT_FLUX_MODEL
  CFIRE_CPL_MODE     => TFIRE_MODEL(KTO)%CFIRE_CPL_MODE
  CBMAPFILE          => TFIRE_MODEL(KTO)%CBMAPFILE
  LINTERPWIND        => TFIRE_MODEL(KTO)%LINTERPWIND
  LSGBAWEIGHT        => TFIRE_MODEL(KTO)%LSGBAWEIGHT
  NFIRE_WENO_ORDER   => TFIRE_MODEL(KTO)%NFIRE_WENO_ORDER
  NFIRE_RK_ORDER     => TFIRE_MODEL(KTO)%NFIRE_RK_ORDER
  NREFINX            => TFIRE_MODEL(KTO)%NREFINX
  NREFINY            => TFIRE_MODEL(KTO)%NREFINY
  XCFLMAXFIRE        => TFIRE_MODEL(KTO)%XCFLMAXFIRE
  XLSDIFFUSION       => TFIRE_MODEL(KTO)%XLSDIFFUSION
  XROSDIFFUSION      => TFIRE_MODEL(KTO)%XROSDIFFUSION
  XFERR              => TFIRE_MODEL(KTO)%XFERR
  XFLUXZEXT          => TFIRE_MODEL(KTO)%XFLUXZEXT
  XFLUXZMAX          => TFIRE_MODEL(KTO)%XFLUXZMAX
  XFLXCOEFTMP        => TFIRE_MODEL(KTO)%XFLXCOEFTMP
  LWINDFILTER        => TFIRE_MODEL(KTO)%LWINDFILTER
  CWINDFILTER        => TFIRE_MODEL(KTO)%CWINDFILTER
  XEWAMTAU           => TFIRE_MODEL(KTO)%XEWAMTAU
  XWLIMUTH           => TFIRE_MODEL(KTO)%XWLIMUTH
  XWLIMUTMAX         => TFIRE_MODEL(KTO)%XWLIMUTMAX
  NWINDSLOPECPLMODE  => TFIRE_MODEL(KTO)%NWINDSLOPECPLMODE
  NNBSMOKETRACER     => TFIRE_MODEL(KTO)%NNBSMOKETRACER
  XFIREMESHSIZE      => TFIRE_MODEL(KTO)%XFIREMESHSIZE
  XFIREPERF          => TFIRE_MODEL(KTO)%XFIREPERF
  XGRADPERF          => TFIRE_MODEL(KTO)%XGRADPERF
  XROSWINDPERF       => TFIRE_MODEL(KTO)%XROSWINDPERF
  XPROPAGPERF        => TFIRE_MODEL(KTO)%XPROPAGPERF
  XFLUXPERF          => TFIRE_MODEL(KTO)%XFLUXPERF
  LRESTA_ASE         => TFIRE_MODEL(KTO)%LRESTA_ASE
  LRESTA_AWC         => TFIRE_MODEL(KTO)%LRESTA_AWC
  LRESTA_EWAM        => TFIRE_MODEL(KTO)%LRESTA_EWAM
  LRESTA_WLIM        => TFIRE_MODEL(KTO)%LRESTA_WLIM

END SUBROUTINE FIRE_GOTO_MODEL

END MODULE MODD_FIRE_n
