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
!!	A. Costes   *Meteo France/Cerfacs*
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
LOGICAL		,	SAVE	::	LBLAZE		      ! Flag for Fire model use, default FALSE

CHARACTER(LEN=11)	,     SAVE  ::	CPROPAG_MODEL	      ! Fire propagation model (default SANTONI2011)
CHARACTER(LEN=3)	,	SAVE	::	CHEAT_FLUX_MODEL	      ! Sensible heat flux injection model (default CST)
CHARACTER(LEN=3)	,	SAVE	::	CLATENT_FLUX_MODEL      ! latent heat flux injection model (default CST)

CHARACTER(LEN=7)	,	SAVE	::	CFIRE_CPL_MODE 	      ! Coupling mode (default 2WAYCPL)

CHARACTER(LEN=28)	,	SAVE	::	CBMAPFILE 	            ! BMap file for FIR2ATM mode (default INIFILE)
LOGICAL	      ,	SAVE	::	LINTERPWIND             ! Flag for wind interpolation
LOGICAL		,	SAVE	::	LSGBAWEIGHT             ! Flag for use of weighted average method for SubGrid Burning Area computation

INTEGER		,	SAVE	::	NFIRE_WENO_ORDER	      ! Weno order (1,3,5)
INTEGER		,	SAVE	::	NFIRE_RK_ORDER	      ! Runge Kutta order (1,2,3,4)

INTEGER		,	SAVE	::	NREFINX	            ! Refinement ratio X
INTEGER		,	SAVE	::	NREFINY	            ! Refinement ratio Y

REAL              ,     SAVE  ::    XCFLMAXFIRE             ! Maximum CFL on fire mesh
REAL              ,     SAVE  ::    XLSDIFFUSION            ! Numerical diffusion of LevelSet
REAL              ,     SAVE  ::    XROSDIFFUSION           ! Numerical diffusion of ROS

REAL              ,     SAVE  ::    XFERR                   ! Flamming Energy Release ratio (between 0.5 <= FERR < 1)

REAL              ,     SAVE  ::    XFLUXZEXT               ! Flux distribution on vertical caracteristic length
REAL              ,     SAVE  ::    XFLUXZMAX               ! Flux distribution on vertical max injetion height

REAL              ,     SAVE  ::    XFLXCOEFTMP             ! Flux multiplicator. For testing

LOGICAL           ,     SAVE  ::    LWINDFILTER             ! Fire wind filtering flag
CHARACTER(LEN=4)	,     SAVE  ::    CWINDFILTER 	      ! Wind filter method (EWAM or WLIM)
REAL              ,     SAVE  ::    XEWAMTAU                ! Time averaging constant for EWAM method (s)
REAL              ,     SAVE  ::    XWLIMUTH                ! Thresehold wind value for WLIM method (m/s)
REAL              ,     SAVE  ::    XWLIMUTMAX              ! Maximum wind value for WLIM method (m/s) (needs to be >= XWLIMUTH )

INTEGER		,	SAVE	::	NWINDSLOPECPLMODE       ! Flag for use of wind/slope in ROS (0 = wind + slope, 1 = wind only, 2 = slope only (U0=0))

INTEGER		,	SAVE	::    NNBSMOKETRACER
!
! Parameters not in the namelist
!
REAL,               DIMENSION(2), SAVE ::  XFIREMESHSIZE    ! Fire Mesh size [dxf,dyf]
REAL(KIND=MNHTIME), DIMENSION(2), SAVE :: XFIREPERF         ! Blaze fire model performance
REAL(KIND=MNHTIME), DIMENSION(2), SAVE :: XGRADPERF         ! Grad computation performance
REAL(KIND=MNHTIME), DIMENSION(2), SAVE :: XROSWINDPERF      ! ROS and wind interpolation computation performance
REAL(KIND=MNHTIME), DIMENSION(2), SAVE :: XPROPAGPERF       ! Propagation computation performance
REAL(KIND=MNHTIME), DIMENSION(2), SAVE :: XFLUXPERF         ! Heat fluxes computation performance
LOGICAL           ,     SAVE  ::    LRESTA_ASE              ! Flag for using ASE in RESTA file
LOGICAL           ,     SAVE  ::    LRESTA_AWC              ! Flag for using AWC in RESTA file
LOGICAL           ,     SAVE  ::    LRESTA_EWAM             ! Flag for using EWAM in RESTA file
LOGICAL           ,     SAVE  ::    LRESTA_WLIM             ! Flag for using WLIM in RESTA file


END MODULE MODD_FIRE
