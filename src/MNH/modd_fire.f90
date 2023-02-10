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
USE MODD_PRECISION, ONLY: MNHTIME
!
IMPLICIT NONE
!
SAVE

INTEGER, PARAMETER :: NFIREENTRIES = 22

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


END MODULE MODD_FIRE
