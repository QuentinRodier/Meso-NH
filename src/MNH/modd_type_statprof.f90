!MNH_LIC Copyright 2002-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ############################
      MODULE MODD_TYPE_STATPROF
!     ############################
!
!!****  *MODD_STATION* - declaration of stations
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to define
!      the different stations types.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      NONE 
!!
!!    REFERENCE
!!    --------- 
!!       
!!    AUTHOR
!!    ------
!!	P. Tulet   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    15/01/02
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet    04/2022: restructure stations for better performance, reduce memory usage and correct some problems/bugs
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
use modd_type_date,  only: date_time
use modd_parameters, only: NNEGUNDEF, NSTATPROFNAMELGTMAX, XUNDEF

implicit none

private

public :: TSTATPROFTIME, TSTATIONDATA

TYPE :: TSTATPROFTIME
  REAL                                       :: XTIME_CUR = XUNDEF  ! current time since last storage
  INTEGER                                    :: N_CUR     = 0           ! current step of storage
  REAL                                       :: XTSTEP    = 60.     ! storage time step (default reset later by INI_STATION_n)
  type(date_time), dimension(:), ALLOCATABLE :: tpdates             ! dates(n) (n: recording instants)
END TYPE TSTATPROFTIME

TYPE TSTATIONDATA
! Type to store all the data of 1 station
CHARACTER(LEN=NSTATPROFNAMELGTMAX) :: CNAME = ''  ! station name


INTEGER :: NID = 0 ! Global identification number of the station (from 1 to total number of stations of the model)

REAL :: XX   = XUNDEF  ! X(n)
REAL :: XY   = XUNDEF  ! Y(n)
REAL :: XZ   = XUNDEF  ! Z(n)
REAL :: XLON = XUNDEF  ! longitude(n)
REAL :: XLAT = XUNDEF  ! latitude (n)
REAL :: XZS  = XUNDEF  ! zs(n)

! Position in the mesh
INTEGER :: NI_M = NNEGUNDEF ! X position for mass-point axis (between this one and the next one)
INTEGER :: NJ_M = NNEGUNDEF ! Y position for mass-point axis (between this one and the next one)
INTEGER :: NI_U = NNEGUNDEF ! X position for u-point axis (between this one and the next one)
INTEGER :: NJ_V = NNEGUNDEF ! Y position for v-point axis (between this one and the next one)

! Coefficient to interpolate values (stations are usually not exactly on mesh points)
REAL :: XXMCOEF = XUNDEF ! Interpolation coefficient for X (mass-point)
REAL :: XYMCOEF = XUNDEF ! Interpolation coefficient for Y (mass-point)
REAL :: XXUCOEF = XUNDEF ! Interpolation coefficient for X (U-point)
REAL :: XYVCOEF = XUNDEF ! Interpolation coefficient for Y (V-point)

INTEGER :: NK = NNEGUNDEF ! Model level for altitude comparisons

REAL, DIMENSION(:),   ALLOCATABLE :: XZON    ! zonal wind(n)
REAL, DIMENSION(:),   ALLOCATABLE :: XMER    ! meridian wind(n)
REAL, DIMENSION(:),   ALLOCATABLE :: XW      ! w(n)  (air vertical speed)
REAL, DIMENSION(:),   ALLOCATABLE :: XP      ! p(n)
REAL, DIMENSION(:),   ALLOCATABLE :: XTKE    ! tke(n)
REAL, DIMENSION(:),   ALLOCATABLE :: XTH     ! th(n)
REAL, DIMENSION(:,:), ALLOCATABLE :: XR      ! r*(n)
REAL, DIMENSION(:,:), ALLOCATABLE :: XSV     ! Sv*(n)
REAL, DIMENSION(:),   ALLOCATABLE :: XTSRAD  ! Ts(n)

REAL, DIMENSION(:),   ALLOCATABLE :: XT2M
REAL, DIMENSION(:),   ALLOCATABLE :: XQ2M
REAL, DIMENSION(:),   ALLOCATABLE :: XHU2M
REAL, DIMENSION(:),   ALLOCATABLE :: XZON10M
REAL, DIMENSION(:),   ALLOCATABLE :: XMER10M
REAL, DIMENSION(:),   ALLOCATABLE :: XRN
REAL, DIMENSION(:),   ALLOCATABLE :: XH
REAL, DIMENSION(:),   ALLOCATABLE :: XLE
REAL, DIMENSION(:),   ALLOCATABLE :: XLEI
REAL, DIMENSION(:),   ALLOCATABLE :: XGFLUX
REAL, DIMENSION(:),   ALLOCATABLE :: XSWD
REAL, DIMENSION(:),   ALLOCATABLE :: XSWU
REAL, DIMENSION(:),   ALLOCATABLE :: XLWD
REAL, DIMENSION(:),   ALLOCATABLE :: XLWU
REAL, DIMENSION(:),   ALLOCATABLE :: XSWDIR
REAL, DIMENSION(:),   ALLOCATABLE :: XSWDIFF
REAL, DIMENSION(:),   ALLOCATABLE :: XDSTAOD ! Dust Aerosol Optical Depth
REAL, DIMENSION(:),   ALLOCATABLE :: XSFCO2  ! CO2 surface flux

END TYPE TSTATIONDATA

END MODULE MODD_TYPE_STATPROF
