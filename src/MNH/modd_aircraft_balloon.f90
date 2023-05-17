!MNH_LIC Copyright 2000-2023 CNRS, Meteo-France and Universite Paul Sabatier
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
use modd_sensor,        only: tsensor
USE MODD_TYPE_STATPROF, ONLY: TSTATPROFTIME
use modd_type_date,     only: date_time

USE MODE_DATETIME,      ONLY: TPREFERENCE_DATE

implicit none

save

INTEGER, PARAMETER :: NCRASH_NO        = 0 ! Not crashed
INTEGER, PARAMETER :: NCRASH_OUT_HORIZ = 1 ! Flyer is outside of horizontal domain
INTEGER, PARAMETER :: NCRASH_OUT_LOW   = 2 ! Flyer crashed on ground (or sea!)
INTEGER, PARAMETER :: NCRASH_OUT_HIGH  = 3 ! Flyer is too high (outside of domain)

INTEGER, PARAMETER :: NFLYER_DEFAULT_RANK = 1

LOGICAL :: LFLYER = .FALSE. ! flag to use aircraft/balloons

TYPE, EXTENDS(TSENSOR), ABSTRACT :: TFLYERDATA
  !
  !* general information
  !
  CHARACTER(LEN=3) :: CMODEL = 'FIX' ! type of model used for each balloon/aircraft
                                     ! 'FIX' : NMODEL used during the run
                                     ! 'MOB' : change od model depends of the
                                     !         balloon/aircraft location
  INTEGER          :: NMODEL = 0 ! model number for each balloon/aircraft (may change if CMODEL='MOB')
  INTEGER          :: NID    = 0 ! Identification number
  CHARACTER(LEN=6) :: CTYPE = ''  ! flyer type:
                                  ! 'RADIOS' : radiosounding balloon
                                  ! 'ISODEN' : iso-density balloon
                                  ! 'AIRCRA' : aircraft
                                  ! 'CVBALL' : Constant Volume balloon
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
  INTEGER :: NRANK_CUR = NFLYER_DEFAULT_RANK ! Rank of the process where the flyer is
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
  REAL, DIMENSION(:),     ALLOCATABLE :: XZS        ! zs(n)
  REAL, DIMENSION(:),     ALLOCATABLE :: XTSRAD     ! Ts(n)
  !
  REAL, DIMENSION(:),     ALLOCATABLE :: XTHW_FLUX  ! thw_flux(n)
  REAL, DIMENSION(:),     ALLOCATABLE :: XRCW_FLUX  ! rcw_flux(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XSVW_FLUX  ! psw_flux(n)

  CONTAINS
    PROCEDURE :: ALLOCATE => ALLOCATE_FLYER
END TYPE TFLYERDATA

TYPE, EXTENDS( TFLYERDATA ) :: TAIRCRAFTDATA
  LOGICAL :: LTOOKOFF = .FALSE. ! Set to true once the aircraft takes off
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
  REAL    :: XP_CUR = XNEGUNDEF ! current p (only if LALTDEF = F)
END TYPE TAIRCRAFTDATA

TYPE, EXTENDS( TFLYERDATA ) :: TBALLOONDATA
  LOGICAL :: LPOSITION_INIT = .FALSE. ! True if initial position has been computed
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

TYPE TAIRCRAFT_PTR
  TYPE(TAIRCRAFTDATA), POINTER :: TAIRCRAFT => NULL()
END TYPE TAIRCRAFT_PTR

TYPE TBALLOON_PTR
  TYPE(TBALLOONDATA), POINTER :: TBALLOON => NULL()
END TYPE TBALLOON_PTR

TYPE(TAIRCRAFT_PTR), DIMENSION(:), ALLOCATABLE :: TAIRCRAFTS ! characteristics and records of the aircrafts

TYPE(TBALLOON_PTR),  DIMENSION(:), ALLOCATABLE :: TBALLOONS  ! characteristics and records of the balloons

INTEGER, DIMENSION(:), ALLOCATABLE :: NRANKCUR_AIRCRAFT ! Array to store the rank of the process where a given aircraft is present
INTEGER, DIMENSION(:), ALLOCATABLE :: NRANKNXT_AIRCRAFT ! Array to store the rank of the process where a given aircraft is going

INTEGER, DIMENSION(:), ALLOCATABLE :: NRANKCUR_BALLOON  ! Array to store the rank of the process where a given ballon is present
INTEGER, DIMENSION(:), ALLOCATABLE :: NRANKNXT_BALLOON  ! Array to store the rank of the process where a given ballon is going


CONTAINS
SUBROUTINE ALLOCATE_FLYER( TPSENSOR, KSTORE )

  USE MODD_CONF,             ONLY: CPROGRAM
  USE MODD_CONF_n,           ONLY: NRR
  USE MODD_DIAG_FLAG,        ONLY: NTIME_AIRCRAFT_BALLOON
  USE MODD_DIM_n,            ONLY: NKMAX
  USE MODD_DYN,              ONLY: XSEGLEN
  USE MODD_DYN_n,            ONLY: DYN_MODEL
  USE MODD_NSV,              ONLY: NSV
  USE MODD_PARAMETERS,       ONLY: JPVEXT, NNEGUNDEF, XUNDEF
  USE MODD_PARAM_n,          ONLY: CCLOUD, CTURB
  USE MODD_SURF_PAR,         ONLY: XUNDEF_SFX => XUNDEF

  USE MODE_MSG

  IMPLICIT NONE

  CLASS(TFLYERDATA), INTENT(INOUT) :: TPSENSOR
  INTEGER, OPTIONAL, INTENT(IN)    :: KSTORE

  INTEGER :: IKU    ! number of vertical levels
  INTEGER :: ISTORE ! number of storage instants

  CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'ALLOCATE_FLYER', 'flyer: ' // TRIM(TPSENSOR%CNAME), OLOCAL = .TRUE. )

  IKU = NKMAX + 2 * JPVEXT

  IF ( PRESENT( KSTORE ) ) THEN
    ISTORE = KSTORE
  ELSE
    IF ( CPROGRAM == 'DIAG  ' ) THEN
      ISTORE = INT ( NTIME_AIRCRAFT_BALLOON / TPSENSOR%TFLYER_TIME%XTSTEP ) + 1
    ELSE
      ISTORE = NINT ( ( XSEGLEN - DYN_MODEL(1)%XTSTEP ) / TPSENSOR%TFLYER_TIME%XTSTEP ) + 1
    ENDIF
  END IF

  ALLOCATE( TPSENSOR%TFLYER_TIME%TPDATES(ISTORE) )
  ALLOCATE( TPSENSOR%NMODELHIST(ISTORE) )
  ALLOCATE( TPSENSOR%XX   (ISTORE) )
  ALLOCATE( TPSENSOR%XY   (ISTORE) )
  ALLOCATE( TPSENSOR%XZ   (ISTORE) )
  ALLOCATE( TPSENSOR%XLON (ISTORE) )
  ALLOCATE( TPSENSOR%XLAT (ISTORE) )
  ALLOCATE( TPSENSOR%XZON (ISTORE) )
  ALLOCATE( TPSENSOR%XMER (ISTORE) )
  ALLOCATE( TPSENSOR%XW   (ISTORE) )
  ALLOCATE( TPSENSOR%XP   (ISTORE) )
  ALLOCATE( TPSENSOR%XTH  (ISTORE) )
  ALLOCATE( TPSENSOR%XR   (ISTORE, NRR) )
  ALLOCATE( TPSENSOR%XSV  (ISTORE, NSV) )
  ALLOCATE( TPSENSOR%XRTZ (ISTORE, IKU) )
  ALLOCATE( TPSENSOR%XRZ  (ISTORE, IKU, NRR) )
  ALLOCATE( TPSENSOR%XFFZ (ISTORE, IKU) )
  ALLOCATE( TPSENSOR%XIWCZ(ISTORE, IKU) )
  ALLOCATE( TPSENSOR%XLWCZ(ISTORE, IKU) )
  ALLOCATE( TPSENSOR%XCIZ (ISTORE, IKU) )
  IF ( CCLOUD == 'LIMA' ) THEN
    ALLOCATE( TPSENSOR%XCCZ(ISTORE, IKU) )
    ALLOCATE( TPSENSOR%XCRZ(ISTORE, IKU) )
  ELSE
    ALLOCATE( TPSENSOR%XCCZ(0, 0) )
    ALLOCATE( TPSENSOR%XCRZ(0, 0) )
  ENDIF
  ALLOCATE( TPSENSOR%XCRARE    (ISTORE, IKU) )
  ALLOCATE( TPSENSOR%XCRARE_ATT(ISTORE, IKU) )
  ALLOCATE( TPSENSOR%XWZ       (ISTORE, IKU) )
  ALLOCATE( TPSENSOR%XZZ       (ISTORE, IKU) )
  IF ( CTURB == 'TKEL' ) THEN
    ALLOCATE( TPSENSOR%XTKE(ISTORE) )
  ELSE
    ALLOCATE( TPSENSOR%XTKE(0) )
  END IF
  ALLOCATE( TPSENSOR%XTKE_DISS(ISTORE) )
  ALLOCATE( TPSENSOR%XTSRAD   (ISTORE) )
  ALLOCATE( TPSENSOR%XZS      (ISTORE) )

  ALLOCATE( TPSENSOR%XTHW_FLUX(ISTORE) )
  ALLOCATE( TPSENSOR%XRCW_FLUX(ISTORE) )
  ALLOCATE( TPSENSOR%XSVW_FLUX(ISTORE, NSV) )

  TPSENSOR%NMODELHIST = NNEGUNDEF
  TPSENSOR%XX    = XUNDEF
  TPSENSOR%XY    = XUNDEF
  TPSENSOR%XZ    = XUNDEF
  TPSENSOR%XLON  = XUNDEF
  TPSENSOR%XLAT  = XUNDEF
  TPSENSOR%XZON  = XUNDEF
  TPSENSOR%XMER  = XUNDEF
  TPSENSOR%XW    = XUNDEF
  TPSENSOR%XP    = XUNDEF
  TPSENSOR%XTH   = XUNDEF
  TPSENSOR%XR    = XUNDEF
  TPSENSOR%XSV   = XUNDEF
  TPSENSOR%XRTZ  = XUNDEF
  TPSENSOR%XRZ   = XUNDEF
  TPSENSOR%XFFZ  = XUNDEF
  TPSENSOR%XIWCZ = XUNDEF
  TPSENSOR%XLWCZ = XUNDEF
  TPSENSOR%XCIZ  = XUNDEF
  TPSENSOR%XCCZ  = XUNDEF
  TPSENSOR%XCRZ  = XUNDEF
  TPSENSOR%XCRARE     = XUNDEF
  TPSENSOR%XCRARE_ATT = XUNDEF
  TPSENSOR%XWZ        = XUNDEF
  TPSENSOR%XZZ        = XUNDEF
  TPSENSOR%XTKE       = XUNDEF
  TPSENSOR%XTKE_DISS  = XUNDEF
  TPSENSOR%XTSRAD     = XUNDEF_SFX
  TPSENSOR%XZS        = XUNDEF

  TPSENSOR%XTHW_FLUX = XUNDEF
  TPSENSOR%XRCW_FLUX = XUNDEF
  TPSENSOR%XSVW_FLUX = XUNDEF

END SUBROUTINE ALLOCATE_FLYER

END MODULE MODD_AIRCRAFT_BALLOON
