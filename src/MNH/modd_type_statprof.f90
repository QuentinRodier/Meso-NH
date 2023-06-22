!MNH_LIC Copyright 2002-2023 CNRS, Meteo-France and Universite Paul Sabatier
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
use modd_parameters,  only: NNEGUNDEF, NSENSORNAMELGTMAX, XUNDEF
use modd_sensor,      only: tsensor
use modd_type_date,   only: date_time

implicit none

private

public :: TPROFILERDATA, TSTATIONDATA, TSTATPROFDATA

TYPE, EXTENDS(TSENSOR), ABSTRACT :: TSTATPROFDATA
  ! Type to store data common to stations and profilers
  ! It is used as a basis for the TSTATIONDATA and TPROFILERDATA
  ! and for common procedures for these 2 types

  ! Dimension corresponds to recording instants
  REAL, DIMENSION(:),   ALLOCATABLE :: XT2M    ! 2 m air temperature (C)
  REAL, DIMENSION(:),   ALLOCATABLE :: XQ2M    ! 2 m humidity (kg/kg)
  REAL, DIMENSION(:),   ALLOCATABLE :: XHU2M   ! 2 m relative humidity (%)
  REAL, DIMENSION(:),   ALLOCATABLE :: XZON10M ! 10 m zonal wind (m/s)
  REAL, DIMENSION(:),   ALLOCATABLE :: XMER10M ! 10 m merid. wind (m/s)
  REAL, DIMENSION(:),   ALLOCATABLE :: XRN     ! net radiation (W/m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XH      ! sensible heat flux (W/m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XLE     ! Total latent heat flux (W/m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XLEI    ! Solid latent heat flux (W/m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XGFLUX  ! storage heat flux (W/m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XSWD    ! IR downward radiation (W/m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XSWU    ! IR upward radiation (W/m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XLWD    ! solar downward radiation (W/m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XLWU    ! solar upward radiation (W/m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XSWDIR  ! IR downward direct radiation (W/m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XSWDIFF ! IR downward diffuse radiation (W/m2)
  REAL, DIMENSION(:),   ALLOCATABLE :: XDSTAOD ! Dust Aerosol Optical Depth (m)
  REAL, DIMENSION(:),   ALLOCATABLE :: XSLTAOD ! Salt Aerosol Optical Depth (m)
  REAL, DIMENSION(:),   ALLOCATABLE :: XSFCO2  ! CO2 surface flux (mg/m2/s)

  CONTAINS
    ! Remark: DATA_ARRAYS_(DE)ALLOCATE_STATPROF do not point to DATA_ARRAYS_(DE)ALLOCATE to allow other dummy arguments
    PROCEDURE :: DATA_ARRAYS_ALLOCATE_STATPROF
    PROCEDURE :: DATA_ARRAYS_DEALLOCATE_STATPROF
END TYPE

TYPE, EXTENDS( TSTATPROFDATA ) ::  TSTATIONDATA
  ! Type to store all the data of 1 station
  INTEGER :: NK = NNEGUNDEF ! Model level for altitude comparisons

  REAL :: XZS  = XUNDEF  ! zs(n)
  REAL :: XZMEAS = XUNDEF ! interpolated altitude used for measurements

  CONTAINS
    PROCEDURE :: DATA_ARRAYS_ALLOCATE   => DATA_ARRAYS_ALLOCATE_STATION
    PROCEDURE :: DATA_ARRAYS_DEALLOCATE => DATA_ARRAYS_DEALLOCATE_STATION
  END TYPE TSTATIONDATA

TYPE, EXTENDS( TSTATPROFDATA ) ::  TPROFILERDATA
  ! Type to store all the data of 1 profiler
  CHARACTER(LEN=NSENSORNAMELGTMAX) :: CTYPE = ''  ! Profiler type

  ! (n: recording instants)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XFF        ! wind intensity
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XDD        ! wind direction
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XZZ        ! altitude(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XTHV       ! thv(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XVISIGUL   ! VISI GULTEPE(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XVISIKUN   ! VISI KUNKEL(n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XCIZ       ! Ice number concentration ICE3 (n)
  REAL, DIMENSION(:,:),   ALLOCATABLE :: XRHOD      ! density of dry air/moist air
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: XAER       ! AER*(n) aerosol extinction

  REAL, DIMENSION(:), ALLOCATABLE :: XIWV ! integrated water vpour(n)
  REAL, DIMENSION(:), ALLOCATABLE :: XZTD ! GPS zenith tropo delay(n)
  REAL, DIMENSION(:), ALLOCATABLE :: XZWD ! GPS zenith wet delay(n)
  REAL, DIMENSION(:), ALLOCATABLE :: XZHD ! GPS zenith hydro delay(n)

  REAL, DIMENSION(:,:), ALLOCATABLE :: XTKE_DISS ! TKE dissipation rate

  CONTAINS
    PROCEDURE :: DATA_ARRAYS_ALLOCATE   => DATA_ARRAYS_ALLOCATE_PROFILER
    PROCEDURE :: DATA_ARRAYS_DEALLOCATE => DATA_ARRAYS_DEALLOCATE_PROFILER
END TYPE


CONTAINS

! ##############################################################################
SUBROUTINE DATA_ARRAYS_ALLOCATE_STATPROF( TPSENSOR, overtprof, klevels, KSTORE )
! ##############################################################################

  USE MODE_MSG

  CLASS(TSTATPROFDATA), INTENT(INOUT) :: TPSENSOR
  logical, optional,       intent(in)    :: overtprof ! vertical profile or not
  integer, OPTIONAL,       intent(in)    :: klevels   ! number of vertical levels
  INTEGER, OPTIONAL, INTENT(IN)    :: KSTORE

  CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Data_arrays_allocate_statprof', 'statprof: ' // TRIM(TPSENSOR%CNAME), OLOCAL = .TRUE. )

  CALL TPSENSOR%DATA_ARRAYS_ALLOCATE_SENSOR( overtprof, klevels, kstore )

END SUBROUTINE DATA_ARRAYS_ALLOCATE_STATPROF


! ####################################################
SUBROUTINE DATA_ARRAYS_DEALLOCATE_STATPROF( TPSENSOR )
! ####################################################

  USE MODE_MSG

  CLASS(TSTATPROFDATA), INTENT(INOUT) :: TPSENSOR

  CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Data_arrays_deallocate_statprof', 'statprof: ' // TRIM(TPSENSOR%CNAME), OLOCAL = .TRUE. )

  CALL TPSENSOR%DATA_ARRAYS_DEALLOCATE_SENSOR()

END SUBROUTINE DATA_ARRAYS_DEALLOCATE_STATPROF


! ##########################################################
SUBROUTINE DATA_ARRAYS_ALLOCATE_PROFILER( TPSENSOR, KSTORE )
! ##########################################################

    USE MODD_ALLPROFILER_n, ONLY: LDIAG_SURFRAD_PROF
    USE MODD_CONF_n,        ONLY: NRR
    USE MODD_DIM_n,         ONLY: NKMAX
    USE MODD_NSV,           ONLY: NSV
    USE MODD_PARAMETERS,    ONLY: JPVEXT, XUNDEF
    USE MODD_PARAM_n,       ONLY: CCLOUD, CRAD, CTURB
    USE MODD_RADIATIONS_n,  ONLY: NAER
    USE MODD_SURF_PAR,      ONLY: XUNDEF_SFX => XUNDEF

    USE MODE_MSG

    CLASS(TPROFILERDATA), INTENT(INOUT) :: TPSENSOR
    INTEGER, OPTIONAL,    INTENT(IN)    :: KSTORE  ! number of moments to store

    INTEGER :: IKU

    CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Data_arrays_allocate_profiler', 'profiler: ' // TRIM(TPSENSOR%CNAME), OLOCAL = .TRUE. )

    IF ( .NOT.PRESENT( KSTORE ) ) &
      CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'Data_arrays_allocate_profiler', 'KSTORE not provided', OLOCAL = .TRUE. )

    IKU = NKMAX + 2 * JPVEXT

    CALL DATA_ARRAYS_ALLOCATE_STATPROF( TPSENSOR, OVERTPROF = .TRUE., KLEVELS = IKU, KSTORE = KSTORE )

    ALLOCATE( TPSENSOR%XFF       (IKU, KSTORE) )
    ALLOCATE( TPSENSOR%XDD       (IKU, KSTORE) )
    ALLOCATE( TPSENSOR%XZZ       (IKU, KSTORE) )
    ALLOCATE( TPSENSOR%XTHV      (IKU, KSTORE) )
    IF ( CCLOUD == 'C2R2' .OR. CCLOUD == 'KHKO' ) THEN
      ALLOCATE( TPSENSOR%XVISIGUL  (IKU, KSTORE) )
    ELSE
      ALLOCATE( TPSENSOR%XVISIGUL  (0, 0) )
    END IF
    IF ( CCLOUD /= 'NONE' .AND. CCLOUD /= 'REVE' ) THEN
      ALLOCATE( TPSENSOR%XVISIKUN  (IKU, KSTORE) )
    ELSE
      ALLOCATE( TPSENSOR%XVISIKUN  (0, 0) )
    END IF
    IF ( CCLOUD == 'ICE3' .OR. CCLOUD == 'ICE4' ) THEN
      ALLOCATE( TPSENSOR%XCIZ    (IKU, KSTORE) )
    ELSE
      ALLOCATE( TPSENSOR%XCIZ    (0, 0) )
    END IF
    ALLOCATE( TPSENSOR%XRHOD     (IKU, KSTORE) )
    ALLOCATE( TPSENSOR%XAER      (IKU, KSTORE, NAER ) )

    ALLOCATE( TPSENSOR%XIWV(KSTORE) )
    ALLOCATE( TPSENSOR%XZTD(KSTORE) )
    ALLOCATE( TPSENSOR%XZWD(KSTORE) )
    ALLOCATE( TPSENSOR%XZHD(KSTORE) )

    IF ( LDIAG_SURFRAD_PROF ) THEN
      ALLOCATE( TPSENSOR%XT2M   (KSTORE) )
      ALLOCATE( TPSENSOR%XQ2M   (KSTORE) )
      ALLOCATE( TPSENSOR%XHU2M  (KSTORE) )
      ALLOCATE( TPSENSOR%XZON10M(KSTORE) )
      ALLOCATE( TPSENSOR%XMER10M(KSTORE) )
      ALLOCATE( TPSENSOR%XRN    (KSTORE) )
      ALLOCATE( TPSENSOR%XH     (KSTORE) )
      ALLOCATE( TPSENSOR%XLE    (KSTORE) )
      ALLOCATE( TPSENSOR%XLEI   (KSTORE) )
      ALLOCATE( TPSENSOR%XGFLUX (KSTORE) )
      IF ( CRAD /= 'NONE' ) THEN
        ALLOCATE( TPSENSOR%XSWD   (KSTORE) )
        ALLOCATE( TPSENSOR%XSWU   (KSTORE) )
        ALLOCATE( TPSENSOR%XLWD   (KSTORE) )
        ALLOCATE( TPSENSOR%XLWU   (KSTORE) )
        ALLOCATE( TPSENSOR%XSWDIR (KSTORE) )
        ALLOCATE( TPSENSOR%XSWDIFF(KSTORE) )
        ALLOCATE( TPSENSOR%XDSTAOD(KSTORE) )
        ALLOCATE( TPSENSOR%XSLTAOD(KSTORE) )
      ELSE
        ALLOCATE( TPSENSOR%XSWD   (0) )
        ALLOCATE( TPSENSOR%XSWU   (0) )
        ALLOCATE( TPSENSOR%XLWD   (0) )
        ALLOCATE( TPSENSOR%XLWU   (0) )
        ALLOCATE( TPSENSOR%XSWDIR (0) )
        ALLOCATE( TPSENSOR%XSWDIFF(0) )
        ALLOCATE( TPSENSOR%XDSTAOD(0) )
        ALLOCATE( TPSENSOR%XSLTAOD(0) )
      END IF
      ALLOCATE( TPSENSOR%XSFCO2   (KSTORE) )
    ELSE
      ALLOCATE( TPSENSOR%XT2M   (0) )
      ALLOCATE( TPSENSOR%XQ2M   (0) )
      ALLOCATE( TPSENSOR%XHU2M  (0) )
      ALLOCATE( TPSENSOR%XZON10M(0) )
      ALLOCATE( TPSENSOR%XMER10M(0) )
      ALLOCATE( TPSENSOR%XRN    (0) )
      ALLOCATE( TPSENSOR%XH     (0) )
      ALLOCATE( TPSENSOR%XLE    (0) )
      ALLOCATE( TPSENSOR%XLEI   (0) )
      ALLOCATE( TPSENSOR%XGFLUX (0) )
      ALLOCATE( TPSENSOR%XSWD   (0) )
      ALLOCATE( TPSENSOR%XSWU   (0) )
      ALLOCATE( TPSENSOR%XLWD   (0) )
      ALLOCATE( TPSENSOR%XLWU   (0) )
      ALLOCATE( TPSENSOR%XSWDIR (0) )
      ALLOCATE( TPSENSOR%XSWDIFF(0) )
      ALLOCATE( TPSENSOR%XDSTAOD(0) )
      ALLOCATE( TPSENSOR%XSLTAOD(0) )
      ALLOCATE( TPSENSOR%XSFCO2 (0) )
    END IF

    ALLOCATE( TPSENSOR%XTKE_DISS(IKU, KSTORE) )

    TPSENSOR%XFF       (:,:) = XUNDEF
    TPSENSOR%XDD       (:,:) = XUNDEF
    TPSENSOR%XZZ       (:,:) = XUNDEF
    TPSENSOR%XTHV      (:,:) = XUNDEF
    IF ( CCLOUD == 'C2R2' .OR. CCLOUD == 'KHKO' )  TPSENSOR%XVISIGUL(:,:) = XUNDEF
    IF ( CCLOUD /= 'NONE' .AND. CCLOUD /= 'REVE' ) TPSENSOR%XVISIKUN(:,:) = XUNDEF
    IF ( CCLOUD == 'ICE3' .OR. CCLOUD == 'ICE4' ) TPSENSOR%XCIZ      (:,:) = XUNDEF
    TPSENSOR%XRHOD     (:,:) = XUNDEF
    TPSENSOR%XAER      (:,:,:) = XUNDEF

    TPSENSOR%XIWV(:) = XUNDEF
    TPSENSOR%XZTD(:) = XUNDEF
    TPSENSOR%XZWD(:) = XUNDEF
    TPSENSOR%XZHD(:) = XUNDEF

    IF ( LDIAG_SURFRAD_PROF ) THEN
      TPSENSOR%XT2M   (:) = XUNDEF_SFX
      TPSENSOR%XQ2M   (:) = XUNDEF_SFX
      TPSENSOR%XHU2M  (:) = XUNDEF_SFX
      TPSENSOR%XZON10M(:) = XUNDEF_SFX
      TPSENSOR%XMER10M(:) = XUNDEF_SFX
      TPSENSOR%XRN    (:) = XUNDEF_SFX
      TPSENSOR%XH     (:) = XUNDEF_SFX
      TPSENSOR%XLE    (:) = XUNDEF_SFX
      TPSENSOR%XLEI   (:) = XUNDEF_SFX
      TPSENSOR%XGFLUX (:) = XUNDEF_SFX
      IF ( CRAD /= 'NONE' ) THEN
        TPSENSOR%XSWD   (:) = XUNDEF
        TPSENSOR%XSWU   (:) = XUNDEF
        TPSENSOR%XLWD   (:) = XUNDEF
        TPSENSOR%XLWU   (:) = XUNDEF
        TPSENSOR%XSWDIR (:) = XUNDEF
        TPSENSOR%XSWDIFF(:) = XUNDEF
        TPSENSOR%XDSTAOD(:) = XUNDEF
        TPSENSOR%XSLTAOD(:) = XUNDEF
      END IF
      TPSENSOR%XSFCO2(:)      = XUNDEF
    END IF

    TPSENSOR%XTKE_DISS(:,:) = XUNDEF

  END SUBROUTINE DATA_ARRAYS_ALLOCATE_PROFILER

  ! ####################################################
  SUBROUTINE DATA_ARRAYS_DEALLOCATE_PROFILER( TPSENSOR )
  ! ####################################################
    USE MODE_MSG

    CLASS(TPROFILERDATA), INTENT(INOUT) :: TPSENSOR

    CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Data_arrays_deallocate_profiler', 'profiler: ' // TRIM(TPSENSOR%CNAME), OLOCAL = .TRUE. )

    CALL DATA_ARRAYS_DEALLOCATE_STATPROF( TPSENSOR )

    DEALLOCATE( TPSENSOR%XFF        )
    DEALLOCATE( TPSENSOR%XDD        )
    DEALLOCATE( TPSENSOR%XZZ        )
    DEALLOCATE( TPSENSOR%XTHV       )
    DEALLOCATE( TPSENSOR%XVISIGUL   )
    DEALLOCATE( TPSENSOR%XVISIKUN   )
    DEALLOCATE( TPSENSOR%XCIZ       )
    DEALLOCATE( TPSENSOR%XRHOD      )
    DEALLOCATE( TPSENSOR%XAER       )

    DEALLOCATE( TPSENSOR%XIWV )
    DEALLOCATE( TPSENSOR%XZTD )
    DEALLOCATE( TPSENSOR%XZWD )
    DEALLOCATE( TPSENSOR%XZHD )

    DEALLOCATE( TPSENSOR%XT2M    )
    DEALLOCATE( TPSENSOR%XQ2M    )
    DEALLOCATE( TPSENSOR%XHU2M   )
    DEALLOCATE( TPSENSOR%XZON10M )
    DEALLOCATE( TPSENSOR%XMER10M )
    DEALLOCATE( TPSENSOR%XRN     )
    DEALLOCATE( TPSENSOR%XH      )
    DEALLOCATE( TPSENSOR%XLE     )
    DEALLOCATE( TPSENSOR%XLEI    )
    DEALLOCATE( TPSENSOR%XGFLUX  )
    DEALLOCATE( TPSENSOR%XSWD    )
    DEALLOCATE( TPSENSOR%XSWU    )
    DEALLOCATE( TPSENSOR%XLWD    )
    DEALLOCATE( TPSENSOR%XLWU    )
    DEALLOCATE( TPSENSOR%XSWDIR  )
    DEALLOCATE( TPSENSOR%XSWDIFF )
    DEALLOCATE( TPSENSOR%XDSTAOD )
    DEALLOCATE( TPSENSOR%XSLTAOD )
    DEALLOCATE( TPSENSOR%XSFCO2  )

    DEALLOCATE( TPSENSOR%XTKE_DISS )

  END SUBROUTINE DATA_ARRAYS_DEALLOCATE_PROFILER

  ! #########################################################
  SUBROUTINE DATA_ARRAYS_ALLOCATE_STATION( TPSENSOR, KSTORE )
  ! #########################################################

    USE MODD_ALLSTATION_n, ONLY: LDIAG_SURFRAD_STAT
    USE MODD_CONF_n,       ONLY: NRR
    USE MODD_NSV,          ONLY: NSV
    USE MODD_PARAMETERS,   ONLY: XUNDEF
    USE MODD_PARAM_n,      ONLY: CRAD, CTURB
    USE MODD_SURF_PAR,     ONLY: XUNDEF_SFX => XUNDEF

    USE MODE_MSG

    CLASS(TSTATIONDATA), INTENT(INOUT) :: TPSENSOR
    INTEGER, OPTIONAL,   INTENT(IN)    :: KSTORE  ! number of moments to store

    CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Data_arrays_allocate_station', 'station: ' // TRIM(TPSENSOR%CNAME), OLOCAL = .TRUE. )

    IF ( .NOT.PRESENT( KSTORE ) ) &
      CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'Data_arrays_allocate_station', 'KSTORE not provided', OLOCAL = .TRUE. )

    CALL DATA_ARRAYS_ALLOCATE_STATPROF( TPSENSOR, OVERTPROF = .FALSE., KLEVELS = 1, KSTORE = KSTORE )

    IF ( LDIAG_SURFRAD_STAT ) THEN
      ALLOCATE( TPSENSOR%XT2M   (KSTORE) )
      ALLOCATE( TPSENSOR%XQ2M   (KSTORE) )
      ALLOCATE( TPSENSOR%XHU2M  (KSTORE) )
      ALLOCATE( TPSENSOR%XZON10M(KSTORE) )
      ALLOCATE( TPSENSOR%XMER10M(KSTORE) )
      ALLOCATE( TPSENSOR%XRN    (KSTORE) )
      ALLOCATE( TPSENSOR%XH     (KSTORE) )
      ALLOCATE( TPSENSOR%XLE    (KSTORE) )
      ALLOCATE( TPSENSOR%XLEI   (KSTORE) )
      ALLOCATE( TPSENSOR%XGFLUX (KSTORE) )
      IF ( CRAD /= 'NONE' ) THEN
        ALLOCATE( TPSENSOR%XSWD   (KSTORE) )
        ALLOCATE( TPSENSOR%XSWU   (KSTORE) )
        ALLOCATE( TPSENSOR%XLWD   (KSTORE) )
        ALLOCATE( TPSENSOR%XLWU   (KSTORE) )
        ALLOCATE( TPSENSOR%XSWDIR (KSTORE) )
        ALLOCATE( TPSENSOR%XSWDIFF(KSTORE) )
        ALLOCATE( TPSENSOR%XDSTAOD(KSTORE) )
        ALLOCATE( TPSENSOR%XSLTAOD(KSTORE) )
      END IF
      ALLOCATE( TPSENSOR%XSFCO2(KSTORE) )
    END IF

    IF ( LDIAG_SURFRAD_STAT ) THEN
      TPSENSOR%XT2M(:)    = XUNDEF_SFX
      TPSENSOR%XQ2M(:)    = XUNDEF_SFX
      TPSENSOR%XHU2M(:)   = XUNDEF_SFX
      TPSENSOR%XZON10M(:) = XUNDEF_SFX
      TPSENSOR%XMER10M(:) = XUNDEF_SFX
      TPSENSOR%XRN(:)     = XUNDEF_SFX
      TPSENSOR%XH(:)      = XUNDEF_SFX
      TPSENSOR%XLE(:)     = XUNDEF_SFX
      TPSENSOR%XLEI(:)    = XUNDEF_SFX
      TPSENSOR%XGFLUX(:)  = XUNDEF_SFX
      IF ( CRAD /= 'NONE' ) THEN
        TPSENSOR%XSWD(:)    = XUNDEF
        TPSENSOR%XSWU(:)    = XUNDEF
        TPSENSOR%XLWD(:)    = XUNDEF
        TPSENSOR%XLWU(:)    = XUNDEF
        TPSENSOR%XSWDIR(:)  = XUNDEF
        TPSENSOR%XSWDIFF(:) = XUNDEF
        TPSENSOR%XDSTAOD(:) = XUNDEF
        TPSENSOR%XSLTAOD(:) = XUNDEF
      END IF
      TPSENSOR%XSFCO2(:)  = XUNDEF_SFX
    END IF

  END SUBROUTINE DATA_ARRAYS_ALLOCATE_STATION

  ! ###################################################
  SUBROUTINE DATA_ARRAYS_DEALLOCATE_STATION( TPSENSOR )
  ! ###################################################

    USE MODE_MSG

    CLASS(TSTATIONDATA), INTENT(INOUT) :: TPSENSOR

    CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Data_arrays_deallocate_station', 'station: ' // TRIM(TPSENSOR%CNAME), OLOCAL = .TRUE. )

    CALL DATA_ARRAYS_DEALLOCATE_STATPROF( TPSENSOR )

    DEALLOCATE( TPSENSOR%XT2M    )
    DEALLOCATE( TPSENSOR%XQ2M    )
    DEALLOCATE( TPSENSOR%XHU2M   )
    DEALLOCATE( TPSENSOR%XZON10M )
    DEALLOCATE( TPSENSOR%XMER10M )
    DEALLOCATE( TPSENSOR%XRN     )
    DEALLOCATE( TPSENSOR%XH      )
    DEALLOCATE( TPSENSOR%XLE     )
    DEALLOCATE( TPSENSOR%XLEI    )
    DEALLOCATE( TPSENSOR%XGFLUX  )
    DEALLOCATE( TPSENSOR%XSWD    )
    DEALLOCATE( TPSENSOR%XSWU    )
    DEALLOCATE( TPSENSOR%XLWD    )
    DEALLOCATE( TPSENSOR%XLWU    )
    DEALLOCATE( TPSENSOR%XSWDIR  )
    DEALLOCATE( TPSENSOR%XSWDIFF )
    DEALLOCATE( TPSENSOR%XDSTAOD )
    DEALLOCATE( TPSENSOR%XSLTAOD )
    DEALLOCATE( TPSENSOR%XSFCO2  )

  END SUBROUTINE DATA_ARRAYS_DEALLOCATE_STATION

END MODULE MODD_TYPE_STATPROF
