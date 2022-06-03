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
use modd_parameters,    only: XUNDEF
USE MODD_TYPE_STATPROF, ONLY: TSTATPROFTIME
use modd_type_date,     only: date_time

implicit none

save

!-------------------------------------------------------------------------------------------
!
LOGICAL :: LFLYER    ! flag to use aircraft/balloons
!
TYPE :: TFLYERDATA
  !
  !* general information
  !
  CHARACTER(LEN=3) :: CMODEL = 'FIX' ! type of model used for each balloon/aircraft
                                     ! 'FIX' : NMODEL used during the run
                                     ! 'MOB' : change od model depends of the
                                     !         balloon/aircraft location
  INTEGER          :: NMODEL = 0 ! model number for each balloon/aircraft
  CHARACTER(LEN=6) :: CTYPE = ''  ! flyer type:
                                  ! 'RADIOS' : radiosounding balloon
                                  ! 'ISODEN' : iso-density balloon
                                  ! 'AIRCRA' : aircraft
                                  ! 'CVBALL' : Constant Volume balloon
  CHARACTER(LEN=10) :: CTITLE = ''  ! title or name for the balloon/aircraft
  TYPE(DATE_TIME)   :: TLAUNCH      ! launch/takeoff date and time
  LOGICAL           :: LCRASH = .FALSE. ! occurence of crash
  LOGICAL           :: LFLY   = .FALSE. ! occurence of flying
  !
  !* storage monitoring
  !
  TYPE(TSTATPROFTIME) :: TFLYER_TIME ! Time management for flyer
  !
  !* current position of the balloon/aircraft
  !
  REAL :: XX_CUR = XUNDEF ! current x
  REAL :: XY_CUR = XUNDEF ! current y
  REAL :: XZ_CUR = XUNDEF ! current z (if 'RADIOS' or 'AIRCRA' and 'ALTDEF' = T)
  REAL :: XP_CUR = XUNDEF ! current p (if 'AIRCRA' and 'ALTDEF' = F)
  !
  !* data records
  !
  REAL, DIMENSION(:),     POINTER :: XX         => NULL() ! X(n)
  REAL, DIMENSION(:),     POINTER :: XY         => NULL() ! Y(n)
  REAL, DIMENSION(:),     POINTER :: XZ         => NULL() ! Z(n)
  REAL, DIMENSION(:),     POINTER :: XLAT       => NULL() ! latitude (n)
  REAL, DIMENSION(:),     POINTER :: XLON       => NULL() ! longitude(n)
  REAL, DIMENSION(:),     POINTER :: XZON       => NULL() ! zonal wind(n)
  REAL, DIMENSION(:),     POINTER :: XMER       => NULL() ! meridian wind(n)
  REAL, DIMENSION(:),     POINTER :: XW         => NULL() ! w(n)  (air vertical speed)
  REAL, DIMENSION(:),     POINTER :: XP         => NULL() ! p(n)
  REAL, DIMENSION(:),     POINTER :: XTKE       => NULL() ! tke(n)
  REAL, DIMENSION(:),     POINTER :: XTKE_DISS  => NULL() ! tke dissipation rate
  REAL, DIMENSION(:),     POINTER :: XTH        => NULL() ! th(n)
  REAL, DIMENSION(:,:),   POINTER :: XR         => NULL() ! r*(n)
  REAL, DIMENSION(:,:),   POINTER :: XSV        => NULL() ! Sv*(n)
  REAL, DIMENSION(:,:),   POINTER :: XRTZ       => NULL() ! tot hydrometeor mixing ratio
  REAL, DIMENSION(:,:,:), POINTER :: XRZ        => NULL() ! water vapour mixing ratio
  REAL, DIMENSION(:,:),   POINTER :: XFFZ       => NULL() ! horizontal wind
  REAL, DIMENSION(:,:),   POINTER :: XIWCZ      => NULL() ! ice water content
  REAL, DIMENSION(:,:),   POINTER :: XLWCZ      => NULL() ! liquid water content
  REAL, DIMENSION(:,:),   POINTER :: XCIZ       => NULL() ! Ice concentration
  REAL, DIMENSION(:,:),   POINTER :: XCCZ       => NULL() ! Cloud concentration (LIMA)
  REAL, DIMENSION(:,:),   POINTER :: XCRZ       => NULL() ! Rain concentration (LIMA)
  REAL, DIMENSION(:,:),   POINTER :: XCRARE     => NULL() ! cloud radar reflectivity
  REAL, DIMENSION(:,:),   POINTER :: XCRARE_ATT => NULL() ! attenuated (= more realistic) cloud radar reflectivity
  REAL, DIMENSION(:,:),   POINTER :: XWZ        => NULL() ! vertical profile of vertical velocity
  REAL, DIMENSION(:,:),   POINTER :: XZZ        => NULL() ! vertical profile of mass point altitude (above sea)
  REAL, DIMENSION(:,:),   POINTER :: XAER       => NULL() ! Extinction at 550 nm
  REAL, DIMENSION(:,:),   POINTER :: XDST_WL    => NULL() ! Extinction by wavelength
  REAL, DIMENSION(:),     POINTER :: XZS        => NULL() ! zs(n)
  REAL, DIMENSION(:),     POINTER :: XTSRAD     => NULL() ! Ts(n)
  !
  REAL, DIMENSION(:),     POINTER :: XTHW_FLUX => NULL() ! thw_flux(n)
  REAL, DIMENSION(:),     POINTER :: XRCW_FLUX => NULL() ! rcw_flux(n)
  REAL, DIMENSION(:,:),   POINTER :: XSVW_FLUX => NULL() ! psw_flux(n)
END TYPE TFLYERDATA

TYPE, EXTENDS( TFLYERDATA ) :: TAIRCRAFTDATA
  !
  !* aircraft flight definition
  !
  INTEGER :: NSEG     = 0  ! number of aircraft flight segments
  INTEGER :: NSEGCURN = 1  ! current flight segment number
  REAL    :: XSEGCURT = 0. ! current flight segment time spent
  REAL, DIMENSION(:), POINTER :: XSEGLAT  => NULL() ! latitude of flight segment extremities  (LEG+1)
  REAL, DIMENSION(:), POINTER :: XSEGLON  => NULL() ! longitude of flight segment extremities (LEG+1)
  REAL, DIMENSION(:), POINTER :: XSEGX    => NULL() ! X of flight segment extremities         (LEG+1)
  REAL, DIMENSION(:), POINTER :: XSEGY    => NULL() ! Y of flight segment extremities         (LEG+1)
  REAL, DIMENSION(:), POINTER :: XSEGP    => NULL() ! pressure of flight segment extremities  (LEG+1)
  REAL, DIMENSION(:), POINTER :: XSEGZ    => NULL() ! altitude of flight segment extremities  (LEG+1)
  REAL, DIMENSION(:), POINTER :: XSEGTIME => NULL() ! duration of flight segments             (LEG  )
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
  REAL :: XALTLAUNCH = XUNDEF ! altitude of launch (if 'RADIOS' or 'ISODEN' or 'CVBALL')
  REAL :: XWASCENT   = 5.     ! ascent vertical speed, m/s (if 'RADIOS')
  REAL :: XRHO       = XUNDEF ! density of launch (if 'ISODEN')
  REAL :: XPRES      = XUNDEF ! pressure of launch (if 'ISODEN')
  REAL :: XDIAMETER  = XUNDEF ! apparent diameter of the balloon (m) (if 'CVBALL')
  REAL :: XAERODRAG  = XUNDEF ! aerodynamic drag coefficient of the balloon (if 'CVBALL')
  REAL :: XINDDRAG   = XUNDEF ! induced drag coefficient (i.e. air shifted by the balloon) (if 'CVBALL')
  REAL :: XVOLUME    = XUNDEF ! volume of the balloon (m3) (if 'CVBALL')
  REAL :: XMASS      = XUNDEF ! mass of the balloon (kg) (if 'CVBALL')
END TYPE TBALLOONDATA

INTEGER :: NAIRCRAFTS = 0 ! Total number of aircrafts
INTEGER :: NBALLOONS  = 0 ! Total number of balloons

TYPE(TAIRCRAFTDATA), DIMENSION(:), ALLOCATABLE :: TAIRCRAFTS ! characteristics and records of the aircrafts

TYPE(TBALLOONDATA),  DIMENSION(:), ALLOCATABLE :: TBALLOONS  ! characteristics and records of the balloons

END MODULE MODD_AIRCRAFT_BALLOON
