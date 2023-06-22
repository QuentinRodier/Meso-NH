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
use modd_sensor,        only: tsensor, tsensortime
use modd_type_date,     only: date_time

USE MODE_DATETIME,      ONLY: TPREFERENCE_DATE

implicit none

save

private :: DATA_ARRAYS_ALLOCATE_FLYER, DATA_ARRAYS_DEALLOCATE_FLYER


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
  LOGICAL           :: LSTORE = .FALSE. ! Do we have to store data now
  TYPE(TSENSORTIME) :: TFLYER_TIME      ! Time management for flyer
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
  REAL, DIMENSION(:),     ALLOCATABLE :: XTKE_DISS  ! tke dissipation rate
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XRTZ       ! tot hydrometeor mixing ratio
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: XRZ        ! water vapour mixing ratio
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XFFZ       ! horizontal wind
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XCIZ       ! Ice concentration
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XCCZ       ! Cloud concentration (LIMA)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XCRZ       ! Rain concentration (LIMA)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XWZ        ! vertical profile of vertical velocity
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XZZ        ! vertical profile of mass point altitude (above sea)
  REAL, DIMENSION(:),     ALLOCATABLE :: XZS        ! zs(n)
  !
  REAL, DIMENSION(:),     ALLOCATABLE :: XTHW_FLUX  ! thw_flux(n)
  REAL, DIMENSION(:),     ALLOCATABLE :: XRCW_FLUX  ! rcw_flux(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XSVW_FLUX  ! psw_flux(n)

  CONTAINS
    ! Remark: DATA_ARRAYS_(DE)ALLOCATE_FLYER do not point to DATA_ARRAYS_(DE)ALLOCATE to allow other dummy arguments if needed
    PROCEDURE :: DATA_ARRAYS_ALLOCATE_FLYER
    PROCEDURE :: DATA_ARRAYS_DEALLOCATE_FLYER
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

  CONTAINS
    PROCEDURE :: DATA_ARRAYS_ALLOCATE   => DATA_ARRAYS_ALLOCATE_AIRCRAFT
    PROCEDURE :: DATA_ARRAYS_DEALLOCATE => DATA_ARRAYS_DEALLOCATE_AIRCRAFT
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

  CONTAINS
    PROCEDURE :: DATA_ARRAYS_ALLOCATE   => DATA_ARRAYS_ALLOCATE_BALLOON
    PROCEDURE :: DATA_ARRAYS_DEALLOCATE => DATA_ARRAYS_DEALLOCATE_BALLOON
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
! #######################################################
SUBROUTINE DATA_ARRAYS_ALLOCATE_FLYER( TPSENSOR, KSTORE )
! #######################################################

  USE MODD_CONF,             ONLY: CPROGRAM
  USE MODD_CONF_n,           ONLY: NRR
  USE MODD_DIAG_FLAG,        ONLY: NTIME_AIRCRAFT_BALLOON
  USE MODD_DIM_n,            ONLY: NKMAX
  USE MODD_DYN,              ONLY: XSEGLEN
  USE MODD_DYN_n,            ONLY: DYN_MODEL
  USE MODD_NSV,              ONLY: NSV
  USE MODD_PARAMETERS,       ONLY: JPVEXT, NNEGUNDEF, XUNDEF
  USE MODD_PARAM_n,          ONLY: CCLOUD, CRAD, CTURB
  USE MODD_SURF_PAR,         ONLY: XUNDEF_SFX => XUNDEF

  USE MODE_MSG


  CLASS(TFLYERDATA), INTENT(INOUT) :: TPSENSOR
  INTEGER, OPTIONAL, INTENT(IN)    :: KSTORE

  INTEGER :: IKU    ! number of vertical levels
  INTEGER :: ISTORE ! number of storage instants

  CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Data_arrays_allocate_flyer', 'flyer: ' // TRIM(TPSENSOR%CNAME), OLOCAL = .TRUE. )

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

  CALL TPSENSOR%Data_arrays_allocate_sensor( .TRUE., KLEVELS = 1, KSTORE = ISTORE )

  ALLOCATE( TPSENSOR%TFLYER_TIME%TPDATES(ISTORE) )
  ALLOCATE( TPSENSOR%NMODELHIST(ISTORE) )
  ALLOCATE( TPSENSOR%XX   (ISTORE) )
  ALLOCATE( TPSENSOR%XY   (ISTORE) )
  ALLOCATE( TPSENSOR%XZ   (ISTORE) )
  ALLOCATE( TPSENSOR%XLON (ISTORE) )
  ALLOCATE( TPSENSOR%XLAT (ISTORE) )
  ALLOCATE( TPSENSOR%XRTZ (IKU, ISTORE) )
  ALLOCATE( TPSENSOR%XRZ  (IKU, ISTORE, NRR) )
  ALLOCATE( TPSENSOR%XFFZ (IKU, ISTORE) )
  ALLOCATE( TPSENSOR%XCIZ (IKU, ISTORE) )
  IF ( CCLOUD == 'LIMA' ) THEN
    ALLOCATE( TPSENSOR%XCCZ(IKU, ISTORE) )
    ALLOCATE( TPSENSOR%XCRZ(IKU, ISTORE) )
  ELSE
    ALLOCATE( TPSENSOR%XCCZ(0, 0) )
    ALLOCATE( TPSENSOR%XCRZ(0, 0) )
  ENDIF
  ALLOCATE( TPSENSOR%XWZ       (IKU, ISTORE) )
  ALLOCATE( TPSENSOR%XZZ       (IKU, ISTORE) )
  ALLOCATE( TPSENSOR%XTKE_DISS(ISTORE) )
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
  TPSENSOR%XRTZ  = XUNDEF
  TPSENSOR%XRZ   = XUNDEF
  TPSENSOR%XFFZ  = XUNDEF
  TPSENSOR%XCIZ  = XUNDEF
  TPSENSOR%XCCZ  = XUNDEF
  TPSENSOR%XCRZ  = XUNDEF
  TPSENSOR%XWZ        = XUNDEF
  TPSENSOR%XZZ        = XUNDEF
  TPSENSOR%XTKE_DISS  = XUNDEF
  TPSENSOR%XZS        = XUNDEF

  TPSENSOR%XTHW_FLUX = XUNDEF
  TPSENSOR%XRCW_FLUX = XUNDEF
  TPSENSOR%XSVW_FLUX = XUNDEF

END SUBROUTINE DATA_ARRAYS_ALLOCATE_FLYER

! #################################################
SUBROUTINE DATA_ARRAYS_DEALLOCATE_FLYER( TPSENSOR )
! #################################################

  USE MODE_MSG

  CLASS(TFLYERDATA), INTENT(INOUT) :: TPSENSOR

  CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Data_arrays_deallocate_flyer', 'flyer: ' // TRIM(TPSENSOR%CNAME), OLOCAL = .TRUE. )

  CALL TPSENSOR%Data_arrays_deallocate_sensor()

  DEALLOCATE( TPSENSOR%TFLYER_TIME%TPDATES )
  DEALLOCATE( TPSENSOR%NMODELHIST )
  DEALLOCATE( TPSENSOR%XX         )
  DEALLOCATE( TPSENSOR%XY         )
  DEALLOCATE( TPSENSOR%XZ         )
  DEALLOCATE( TPSENSOR%XLON       )
  DEALLOCATE( TPSENSOR%XLAT       )
  DEALLOCATE( TPSENSOR%XRTZ       )
  DEALLOCATE( TPSENSOR%XRZ        )
  DEALLOCATE( TPSENSOR%XFFZ       )
  DEALLOCATE( TPSENSOR%XCIZ       )
  DEALLOCATE( TPSENSOR%XCCZ       )
  DEALLOCATE( TPSENSOR%XCRZ       )
  DEALLOCATE( TPSENSOR%XWZ        )
  DEALLOCATE( TPSENSOR%XZZ        )
  DEALLOCATE( TPSENSOR%XTKE_DISS  )
  DEALLOCATE( TPSENSOR%XZS        )

  DEALLOCATE( TPSENSOR%XTHW_FLUX )
  DEALLOCATE( TPSENSOR%XRCW_FLUX )
  DEALLOCATE( TPSENSOR%XSVW_FLUX )

  SELECT TYPE( TPSENSOR )
    CLASS IS ( TAIRCRAFTDATA )
      DEALLOCATE( TPSENSOR%XPOSLAT  )
      DEALLOCATE( TPSENSOR%XPOSLON  )
      DEALLOCATE( TPSENSOR%XPOSX    )
      DEALLOCATE( TPSENSOR%XPOSY    )
      IF ( TPSENSOR%LALTDEF ) THEN
        DEALLOCATE( TPSENSOR%XPOSP  )
      ELSE
        DEALLOCATE( TPSENSOR%XPOSZ  )
      END IF
      DEALLOCATE( TPSENSOR%XPOSTIME )
  END SELECT

END SUBROUTINE DATA_ARRAYS_DEALLOCATE_FLYER


! ##########################################################
SUBROUTINE DATA_ARRAYS_ALLOCATE_AIRCRAFT( TPSENSOR, KSTORE )
! ##########################################################

  USE MODE_MSG

  CLASS(TAIRCRAFTDATA), INTENT(INOUT) :: TPSENSOR
  INTEGER, OPTIONAL, INTENT(IN)    :: KSTORE

  CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Data_arrays_allocate_aircraft', 'aircraft: ' // TRIM(TPSENSOR%CNAME), OLOCAL = .TRUE. )

  CALL DATA_ARRAYS_ALLOCATE_FLYER( TPSENSOR, KSTORE )

END SUBROUTINE DATA_ARRAYS_ALLOCATE_AIRCRAFT


! ####################################################
SUBROUTINE DATA_ARRAYS_DEALLOCATE_AIRCRAFT( TPSENSOR )
! ####################################################

  USE MODE_MSG

  CLASS(TAIRCRAFTDATA), INTENT(INOUT) :: TPSENSOR

  CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Data_arrays_deallocate_aircraft', 'aircraft: ' // TRIM(TPSENSOR%CNAME), OLOCAL = .TRUE. )

  CALL DATA_ARRAYS_DEALLOCATE_FLYER( TPSENSOR )

END SUBROUTINE DATA_ARRAYS_DEALLOCATE_AIRCRAFT


! #########################################################
SUBROUTINE DATA_ARRAYS_ALLOCATE_BALLOON( TPSENSOR, KSTORE )
! #########################################################

  USE MODE_MSG

  CLASS(TBALLOONDATA), INTENT(INOUT) :: TPSENSOR
  INTEGER, OPTIONAL, INTENT(IN)    :: KSTORE

  CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Data_arrays_allocate_balloon', 'balloon: ' // TRIM(TPSENSOR%CNAME), OLOCAL = .TRUE. )

  CALL DATA_ARRAYS_ALLOCATE_FLYER( TPSENSOR, KSTORE )

END SUBROUTINE DATA_ARRAYS_ALLOCATE_BALLOON


! ###################################################
SUBROUTINE DATA_ARRAYS_DEALLOCATE_BALLOON( TPSENSOR )
! ###################################################

  USE MODE_MSG

  CLASS(TBALLOONDATA), INTENT(INOUT) :: TPSENSOR

  CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Data_arrays_deallocate_balloon', 'balloon: ' // TRIM(TPSENSOR%CNAME), OLOCAL = .TRUE. )

  CALL DATA_ARRAYS_DEALLOCATE_FLYER( TPSENSOR )

END SUBROUTINE DATA_ARRAYS_DEALLOCATE_BALLOON

END MODULE MODD_AIRCRAFT_BALLOON
