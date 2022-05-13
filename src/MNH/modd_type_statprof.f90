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
!  P. Wautelet    04/2022: restructure stations/profilers for better performance, reduce memory usage and correct some problems/bugs
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
use modd_type_date,  only: date_time
use modd_parameters, only: NNEGUNDEF, NSTATPROFNAMELGTMAX, XUNDEF

implicit none

private

public :: TSTATPROFTIME
public :: TPROFILERDATA, TSTATIONDATA, TSTATPROFDATA

TYPE :: TSTATPROFTIME
  REAL                                       :: XTIME_CUR = XUNDEF  ! current time since last storage
  INTEGER                                    :: N_CUR     = 0       ! current step of storage
  REAL                                       :: XTSTEP    = 60.     ! storage time step (default reset later by INI_STATION_n)
  type(date_time), dimension(:), ALLOCATABLE :: tpdates             ! dates(n) (n: recording instants)
END TYPE TSTATPROFTIME

TYPE :: TSTATPROFDATA
  ! Type to store data common to stations and profilers
  ! It is used as a basis for the TSTATIONDATA and TPROFILERDATA
  ! and for common procedures for these 2 types
  CHARACTER(LEN=NSTATPROFNAMELGTMAX) :: CNAME = ''  ! Station/profiler name

  INTEGER :: NID = 0 ! Global identification number of the station/profiler (from 1 to total number)

  REAL :: XX   = XUNDEF  ! X(n)
  REAL :: XY   = XUNDEF  ! Y(n)
  REAL :: XZ   = XUNDEF  ! Z(n)
  REAL :: XLON = XUNDEF  ! longitude(n)
  REAL :: XLAT = XUNDEF  ! latitude (n)

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

  ! Dimension corresponds to recording instants
  REAL, DIMENSION(:),   ALLOCATABLE :: XT2M    ! 2 m air temperature (C)
  REAL, DIMENSION(:),   ALLOCATABLE :: XQ2M    ! 2 m humidity (kg/kg)
  REAL, DIMENSION(:),   ALLOCATABLE :: XHU2M   ! 2 m relative humidity (%)
  REAL, DIMENSION(:),   ALLOCATABLE :: XZON10M ! 10 m zonal wind (m/s)
  REAL, DIMENSION(:),   ALLOCATABLE :: XMER10M ! 10 m merid. wind (m/s)
  REAL, DIMENSION(:),   ALLOCATABLE :: XRN     ! net radiation (W m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XH      ! sensible heat flux (W m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XLE     ! Total latent heat flux (W m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XLEI    ! Solid latent heat flux (W m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XGFLUX  ! storage heat flux (W m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XSWD    ! IR downward radiation (W m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XSWU    ! IR upward radiation (W m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XLWD    ! solar downward radiation (W m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XLWU    ! solar upward radiation (W m2)
END TYPE

TYPE, EXTENDS( TSTATPROFDATA ) ::  TSTATIONDATA
  ! Type to store all the data of 1 station
  INTEGER :: NK = NNEGUNDEF ! Model level for altitude comparisons

  REAL :: XZS  = XUNDEF  ! zs(n)

  ! (n: recording instants)
  REAL, DIMENSION(:),   ALLOCATABLE :: XZON    ! zonal wind(n)
  REAL, DIMENSION(:),   ALLOCATABLE :: XMER    ! meridian wind(n)
  REAL, DIMENSION(:),   ALLOCATABLE :: XW      ! w(n)  (air vertical speed)
  REAL, DIMENSION(:),   ALLOCATABLE :: XP      ! p(n)
  REAL, DIMENSION(:),   ALLOCATABLE :: XTKE    ! tke(n)
  REAL, DIMENSION(:),   ALLOCATABLE :: XTH     ! th(n)
  REAL, DIMENSION(:,:), ALLOCATABLE :: XR      ! r*(n)
  REAL, DIMENSION(:,:), ALLOCATABLE :: XSV     ! Sv*(n)
  REAL, DIMENSION(:),   ALLOCATABLE :: XTSRAD  ! Ts(n)

  REAL, DIMENSION(:),   ALLOCATABLE :: XSWDIR
  REAL, DIMENSION(:),   ALLOCATABLE :: XSWDIFF
  REAL, DIMENSION(:),   ALLOCATABLE :: XDSTAOD ! Dust Aerosol Optical Depth
  REAL, DIMENSION(:),   ALLOCATABLE :: XSFCO2  ! CO2 surface flux
END TYPE TSTATIONDATA

TYPE, EXTENDS( TSTATPROFDATA ) ::  TPROFILERDATA
  ! Type to store all the data of 1 profiler
  CHARACTER(LEN=NSTATPROFNAMELGTMAX) :: CTYPE = ''  ! Profiler type

  ! (n: recording instants)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XZON       ! zonal wind(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XMER       ! meridian wind(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XFF        ! wind intensity
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XDD        ! wind direction
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XW         ! w(n)  (air vertical speed)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XP         ! p(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XZZ        ! altitude(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XTKE       ! tke(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XTH        ! th(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XTHV       ! thv(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XVISI      ! VISI(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XVISIKUN   ! VISI KUNKEL(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XCRARE     ! radar reflectivity (n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XCRARE_ATT ! radar attenuated reflectivity (n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XCIZ       ! Ice number concentration ICE3 (n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XLWCZ      ! liquid water content (n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XIWCZ      ! ice water content (n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XRHOD      ! density of dry air/moist air
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: XR         ! r*(n)
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: XSV        ! Sv*(n)
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: XAER       ! AER*(n) aerosol extinction

  REAL, DIMENSION(:), ALLOCATABLE :: XIWV ! integrated water vpour(n)
  REAL, DIMENSION(:), ALLOCATABLE :: XZTD ! GPS zenith tropo delay(n)
  REAL, DIMENSION(:), ALLOCATABLE :: XZWD ! GPS zenith wet delay(n)
  REAL, DIMENSION(:), ALLOCATABLE :: XZHD ! GPS zenith hydro delay(n)

  REAL, DIMENSION(:,:), ALLOCATABLE :: XTKE_DISS ! TKE dissipation rate
END TYPE

END MODULE MODD_TYPE_STATPROF
