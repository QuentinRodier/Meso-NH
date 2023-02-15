!MNH_LIC Copyright 2018-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##################
      MODULE MODN_FIRE_n
!     ##################
!
!!****  *MODN_FIRE_n* - declaration of namelist NAM_FIREn
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to specify the namelist  NAM_FIREn
!     which concerns the instants for the outputs realized by all models.
!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_FIRE_n : contains declaration of the variables describing
!!                          the instants for the outputs
!!
!!
!!    REFERENCE
!!    ---------
!!      Book2 of Meso-NH documentation (module MODD_FIRE_n)
!!
!!    AUTHOR
!!    ------
!!  A. Costes      *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/07/2018
!  P. Wautelet 15/02/2023: restructure for grid-nesting
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_FIRE_n, ONLY:                        &
  LBLAZE_n             => LBLAZE,             &
  CPROPAG_MODEL_n      => CPROPAG_MODEL,      &
  CHEAT_FLUX_MODEL_n   => CHEAT_FLUX_MODEL,   &
  CLATENT_FLUX_MODEL_n => CLATENT_FLUX_MODEL, &
  CFIRE_CPL_MODE_n     => CFIRE_CPL_MODE,     &
  CBMAPFILE_n          => CBMAPFILE,          &
  LINTERPWIND_n        => LINTERPWIND,        &
  LSGBAWEIGHT_n        => LSGBAWEIGHT,        &
  NFIRE_WENO_ORDER_n   => NFIRE_WENO_ORDER,   &
  NFIRE_RK_ORDER_n     => NFIRE_RK_ORDER,     &
  NREFINX_n            => NREFINX,            &
  NREFINY_n            => NREFINY,            &
  XCFLMAXFIRE_n        => XCFLMAXFIRE,        &
  XLSDIFFUSION_n       => XLSDIFFUSION,       &
  XROSDIFFUSION_n      => XROSDIFFUSION,      &
  XFERR_n              => XFERR,              &
  XFLUXZEXT_n          => XFLUXZEXT,          &
  XFLUXZMAX_n          => XFLUXZMAX,          &
  XFLXCOEFTMP_n        => XFLXCOEFTMP,        &
  LWINDFILTER_n        => LWINDFILTER,        &
  CWINDFILTER_n        => CWINDFILTER,        &
  XEWAMTAU_n           => XEWAMTAU,           &
  XWLIMUTH_n           => XWLIMUTH,           &
  XWLIMUTMAX_n         => XWLIMUTMAX,         &
  NWINDSLOPECPLMODE_n  => NWINDSLOPECPLMODE,  &
  NNBSMOKETRACER_n     => NNBSMOKETRACER

IMPLICIT NONE

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

NAMELIST /NAM_FIREn/ &
  LBLAZE, &
  CPROPAG_MODEL, CHEAT_FLUX_MODEL, CLATENT_FLUX_MODEL, CFIRE_CPL_MODE, CBMAPFILE,            &
  LINTERPWIND, LSGBAWEIGHT, NFIRE_WENO_ORDER, NFIRE_RK_ORDER, NREFINX, NREFINY, XCFLMAXFIRE, &
  XLSDIFFUSION, XROSDIFFUSION, XFERR, XFLUXZEXT, XFLUXZMAX, XFLXCOEFTMP, LWINDFILTER,        &
  CWINDFILTER, XEWAMTAU, XWLIMUTH, XWLIMUTMAX, NWINDSLOPECPLMODE, NNBSMOKETRACER

CONTAINS

SUBROUTINE INIT_NAM_FIREn
  LBLAZE             = LBLAZE_n
  CPROPAG_MODEL      = CPROPAG_MODEL_n
  CHEAT_FLUX_MODEL   = CHEAT_FLUX_MODEL_n
  CLATENT_FLUX_MODEL = CLATENT_FLUX_MODEL_n
  CFIRE_CPL_MODE     = CFIRE_CPL_MODE_n
  CBMAPFILE          = CBMAPFILE_n
  LINTERPWIND        = LINTERPWIND_n
  LSGBAWEIGHT        = LSGBAWEIGHT_n
  NFIRE_WENO_ORDER   = NFIRE_WENO_ORDER_n
  NFIRE_RK_ORDER     = NFIRE_RK_ORDER_n
  NREFINX            = NREFINX_n
  NREFINY            = NREFINY_n
  XCFLMAXFIRE        = XCFLMAXFIRE_n
  XLSDIFFUSION       = XLSDIFFUSION_n
  XROSDIFFUSION      = XROSDIFFUSION_n
  XFERR              = XFERR_n
  XFLUXZEXT          = XFLUXZEXT_n
  XFLUXZMAX          = XFLUXZMAX_n
  XFLXCOEFTMP        = XFLXCOEFTMP_n
  LWINDFILTER        = LWINDFILTER_n
  CWINDFILTER        = CWINDFILTER_n
  XEWAMTAU           = XEWAMTAU_n
  XWLIMUTH           = XWLIMUTH_n
  XWLIMUTMAX         = XWLIMUTMAX_n
  NWINDSLOPECPLMODE  = NWINDSLOPECPLMODE_n
  NNBSMOKETRACER     = NNBSMOKETRACER_n
END SUBROUTINE INIT_NAM_FIREn

SUBROUTINE UPDATE_NAM_FIREn
  LBLAZE_n             = LBLAZE
  CPROPAG_MODEL_n      = CPROPAG_MODEL
  CHEAT_FLUX_MODEL_n   = CHEAT_FLUX_MODEL
  CLATENT_FLUX_MODEL_n = CLATENT_FLUX_MODEL
  CFIRE_CPL_MODE_n     = CFIRE_CPL_MODE
  CBMAPFILE_n          = CBMAPFILE
  LINTERPWIND_n        = LINTERPWIND
  LSGBAWEIGHT_n        = LSGBAWEIGHT
  NFIRE_WENO_ORDER_n   = NFIRE_WENO_ORDER
  NFIRE_RK_ORDER_n     = NFIRE_RK_ORDER
  NREFINX_n            = NREFINX
  NREFINY_n            = NREFINY
  XCFLMAXFIRE_n        = XCFLMAXFIRE
  XLSDIFFUSION_n       = XLSDIFFUSION
  XROSDIFFUSION_n      = XROSDIFFUSION
  XFERR_n              = XFERR
  XFLUXZEXT_n          = XFLUXZEXT
  XFLUXZMAX_n          = XFLUXZMAX
  XFLXCOEFTMP_n        = XFLXCOEFTMP
  LWINDFILTER_n        = LWINDFILTER
  CWINDFILTER_n        = CWINDFILTER
  XEWAMTAU_n           = XEWAMTAU
  XWLIMUTH_n           = XWLIMUTH
  XWLIMUTMAX_n         = XWLIMUTMAX
  NWINDSLOPECPLMODE_n  = NWINDSLOPECPLMODE
  NNBSMOKETRACER_n     = NNBSMOKETRACER
END SUBROUTINE UPDATE_NAM_FIREn

END MODULE MODN_FIRE_n
