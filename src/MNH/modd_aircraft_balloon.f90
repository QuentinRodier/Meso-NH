!MNH_LIC Copyright 2000-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ############################
      MODULE MODD_AIRCRAFT_BALLOON
!     ############################
!
!!****  *MODD_AIRCRAFT_BALLOON* - declaration of balloons
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to define
!      the different balloons types.
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
!! P. Jabouille   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    15/05/00
!!              Apr,19, 2001 (G.Jaubert) add CVBALL type
!!              March, 2013 : O.Caumont, C.Lac : add vertical profiles
!!              Oct,2016 : G.DELAUTIER LIMA
!  P. Wautelet 08/02/2019: add missing NULL association for pointers
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet    06/2022: reorganize flyers
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
use modd_parameters,    only: NNEGUNDEF, XNEGUNDEF, XUNDEF
USE MODD_TYPE_STATPROF, ONLY: TSTATPROFTIME
use modd_type_date,     only: date_time

USE MODE_DATETIME,      ONLY: TPREFERENCE_DATE

implicit none

save

INTEGER, PARAMETER :: NCRASH_NO        = 0 ! Not crashed
INTEGER, PARAMETER :: NCRASH_OUT_HORIZ = 1 ! Flyer is outside of horizontal domain
INTEGER, PARAMETER :: NCRASH_OUT_LOW   = 2 ! Flyer crashed on ground (or sea!)
INTEGER, PARAMETER :: NCRASH_OUT_HIGH  = 3 ! Flyer is too high (outside of domain)

LOGICAL :: LFLYER = .FALSE. ! flag to use aircraft/balloons

TYPE :: TFLYERDATA
  !
  !* general information
  !
  CHARACTER(LEN=3) :: CMODEL = 'FIX' ! type of model used for each balloon/aircraft
                                     ! 'FIX' : NMODEL used during the run
                                     ! 'MOB' : change od model depends of the
                                     !         balloon/aircraft location
  INTEGER          :: NMODEL = 0 ! model number for each balloon/aircraft
  INTEGER          :: NID    = 0 ! Identification number
  CHARACTER(LEN=6) :: CTYPE = ''  ! flyer type:
                                  ! 'RADIOS' : radiosounding balloon
                                  ! 'ISODEN' : iso-density balloon
                                  ! 'AIRCRA' : aircraft
                                  ! 'CVBALL' : Constant Volume balloon
  CHARACTER(LEN=10) :: CTITLE = ''  ! title or name for the balloon/aircraft
  TYPE(DATE_TIME)   :: TLAUNCH = TPREFERENCE_DATE ! launch/takeoff date and time
  LOGICAL           :: LCRASH = .FALSE. ! occurence of crash
  INTEGER           :: NCRASH = NCRASH_NO
  LOGICAL           :: LFLY   = .FALSE. ! occurence of flying
  !
  !* storage monitoring
  !
  LOGICAL             :: LSTORE = .FALSE. ! Do we have to store data now
  TYPE(TSTATPROFTIME) :: TFLYER_TIME ! Time management for flyer
  !
  !* current position of the balloon/aircraft
  !
  REAL :: XX_CUR = XNEGUNDEF ! current x
  REAL :: XY_CUR = XNEGUNDEF ! current y
  REAL :: XZ_CUR = XNEGUNDEF ! current z (if 'RADIOS' or 'AIRCRA' and 'ALTDEF' = T)
  REAL :: XP_CUR = XNEGUNDEF ! current p (if 'AIRCRA' and 'ALTDEF' = F)
  INTEGER :: NRANK_CUR = NNEGUNDEF ! Rank of the process where the flyer is
  !
  !* data records
  !
  INTEGER, DIMENSION(:),  ALLOCATABLE :: NMODELHIST ! List of models where data has been computed
  REAL, DIMENSION(:),     ALLOCATABLE :: XX         ! X(n)
  REAL, DIMENSION(:),     ALLOCATABLE :: XY         ! Y(n)
  REAL, DIMENSION(:),     ALLOCATABLE :: XZ         ! Z(n)
  REAL, DIMENSION(:),     ALLOCATABLE :: XLAT       ! latitude (n)
  REAL, DIMENSION(:),     ALLOCATABLE :: XLON       ! longitude(n)
  REAL, DIMENSION(:),     ALLOCATABLE :: XZON       ! zonal wind(n)
  REAL, DIMENSION(:),     ALLOCATABLE :: XMER       ! meridian wind(n)
  REAL, DIMENSION(:),     ALLOCATABLE :: XW         ! w(n)  (air vertical speed)
  REAL, DIMENSION(:),     ALLOCATABLE :: XP         ! p(n)
  REAL, DIMENSION(:),     ALLOCATABLE :: XTKE       ! tke(n)
  REAL, DIMENSION(:),     ALLOCATABLE :: XTKE_DISS  ! tke dissipation rate
  REAL, DIMENSION(:),     ALLOCATABLE :: XTH        ! th(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XR         ! r*(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XSV        ! Sv*(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XRTZ       ! tot hydrometeor mixing ratio
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: XRZ        ! water vapour mixing ratio
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XFFZ       ! horizontal wind
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XIWCZ      ! ice water content
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XLWCZ      ! liquid water content
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XCIZ       ! Ice concentration
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XCCZ       ! Cloud concentration (LIMA)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XCRZ       ! Rain concentration (LIMA)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XCRARE     ! cloud radar reflectivity
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XCRARE_ATT ! attenuated (= more realistic) cloud radar reflectivity
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XWZ        ! vertical profile of vertical velocity
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XZZ        ! vertical profile of mass point altitude (above sea)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XAER       ! Extinction at 550 nm
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XDST_WL    ! Extinction by wavelength
  REAL, DIMENSION(:),     ALLOCATABLE :: XZS        ! zs(n)
  REAL, DIMENSION(:),     ALLOCATABLE :: XTSRAD     ! Ts(n)
  !
  REAL, DIMENSION(:),     ALLOCATABLE :: XTHW_FLUX  ! thw_flux(n)
  REAL, DIMENSION(:),     ALLOCATABLE :: XRCW_FLUX  ! rcw_flux(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XSVW_FLUX  ! psw_flux(n)
END TYPE TFLYERDATA

TYPE, EXTENDS( TFLYERDATA ) :: TAIRCRAFTDATA
  !
  !* aircraft flight definition
  !
  INTEGER :: NPOS     = 0  ! number of aircraft positions (segment extremities)
  INTEGER :: NPOSCUR  = 1  ! current flight segment number
  REAL, DIMENSION(:), ALLOCATABLE :: XPOSLAT  ! latitude of flight segment extremities  (LEG+1)
  REAL, DIMENSION(:), ALLOCATABLE :: XPOSLON  ! longitude of flight segment extremities (LEG+1)
  REAL, DIMENSION(:), ALLOCATABLE :: XPOSX    ! X of flight segment extremities         (LEG+1)
  REAL, DIMENSION(:), ALLOCATABLE :: XPOSY    ! Y of flight segment extremities         (LEG+1)
  REAL, DIMENSION(:), ALLOCATABLE :: XPOSP    ! pressure of flight segment extremities  (LEG+1)
  REAL, DIMENSION(:), ALLOCATABLE :: XPOSZ    ! altitude of flight segment extremities  (LEG+1)
  REAL, DIMENSION(:), ALLOCATABLE :: XPOSTIME ! time since launch (corresponding to flight segments extremities (LEG+1)
  TYPE(DATE_TIME) :: TLAND =  TPREFERENCE_DATE    ! landing / end of flight date and time
  !
  !* aircraft altitude type definition
  !
  LOGICAL :: LALTDEF = .FALSE.  ! TRUE == altitude given in pressure
END TYPE TAIRCRAFTDATA

TYPE, EXTENDS( TFLYERDATA ) :: TBALLOONDATA
  !
  !* balloon dynamical characteristics
  !
  REAL :: XLATLAUNCH = XUNDEF ! latitude of launch
  REAL :: XLONLAUNCH = XUNDEF ! lontitude of launch
  REAL :: XXLAUNCH   = XUNDEF ! X coordinate of launch
  REAL :: XYLAUNCH   = XUNDEF ! Y coordinate of launch
  REAL :: XALTLAUNCH = XNEGUNDEF ! altitude of launch (if 'RADIOS' or 'ISODEN' or 'CVBALL')
  REAL :: XWASCENT   = XNEGUNDEF ! ascent vertical speed, m/s (constant if 'RADIOS' or variable if 'CVBALL')
  REAL :: XRHO       = XNEGUNDEF ! density of launch (if 'ISODEN')
  REAL :: XPRES      = XNEGUNDEF ! pressure of launch (if 'ISODEN' or 'CVBALL')
  REAL :: XDIAMETER  = XNEGUNDEF ! apparent diameter of the balloon (m) (if 'CVBALL')
  REAL :: XAERODRAG  = XNEGUNDEF ! aerodynamic drag coefficient of the balloon (if 'CVBALL')
  REAL :: XINDDRAG   = XNEGUNDEF ! induced drag coefficient (i.e. air shifted by the balloon) (if 'CVBALL')
  REAL :: XVOLUME    = XNEGUNDEF ! volume of the balloon (m3) (if 'CVBALL')
  REAL :: XMASS      = XNEGUNDEF ! mass of the balloon (kg) (if 'CVBALL')

  TYPE(DATE_TIME) :: TPOS_CUR = TPREFERENCE_DATE ! Time corresponding to the current position (XX_CUR, XY_CUR...)

END TYPE TBALLOONDATA

INTEGER :: NAIRCRAFTS = 0 ! Total number of aircrafts
INTEGER :: NBALLOONS  = 0 ! Total number of balloons

TYPE(TAIRCRAFTDATA), DIMENSION(:), ALLOCATABLE :: TAIRCRAFTS ! characteristics and records of the aircrafts

TYPE(TBALLOONDATA),  DIMENSION(:), ALLOCATABLE :: TBALLOONS  ! characteristics and records of the balloons

END MODULE MODD_AIRCRAFT_BALLOON
