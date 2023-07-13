!MNH_LIC Copyright 2002-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Authors:
!  Misc: some of the code was taken from older subroutines/functions for stations
!  P. Wautelet 08/04/2022
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 30/09/2022: bugfix: use XUNDEF from SURFEX for surface variables computed by SURFEX
!  P. Wautelet 25/11/2022: rewrite STATPROF_INSTANT algorithm (does not depends on model timestep anymore => independent of model)
!  P. Wautelet 24/05/2023: move TSTATPROF_TIME and STATPROF_INSTANT to modd_sensor (+ rename them)
!-----------------------------------------------------------------
!      ###################
MODULE MODE_STATPROF_TOOLS
!      ###################

USE MODD_TYPE_STATPROF, ONLY: TPROFILERDATA, TSTATIONDATA, TSTATPROFDATA

IMPLICIT NONE

PRIVATE

PUBLIC :: STATPROF_INI_INTERP
PUBLIC :: STATPROF_POSITION
PUBLIC :: PROFILER_ADD, STATION_ADD

CONTAINS

! ##########################################
SUBROUTINE STATPROF_INI_INTERP( TPSTATPROF )
! ##########################################

  USE MODD_GRID,         ONLY: XLATORI, XLONORI
  USE MODD_PARAMETERS,   ONLY: XUNDEF

  USE MODE_GRIDPROJ,     ONLY: SM_XYHAT
  USE MODE_MSG

  IMPLICIT NONE

  CLASS(TSTATPROFDATA), INTENT(INOUT) :: TPSTATPROF

  IF ( TPSTATPROF%XLAT_CUR == XUNDEF .OR. TPSTATPROF%XLON_CUR == XUNDEF ) THEN
    CMNHMSG(1) = 'Error in station or profiler position'
    CMNHMSG(2) = 'either LATitude or LONgitude segment'
    CMNHMSG(3) = 'or I and J segment'
    CMNHMSG(4) = 'definition is not complete.'
    CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'STATPROF_INI_INTERP' )
  END IF

  CALL SM_XYHAT( XLATORI,             XLONORI,             &
                 TPSTATPROF%XLAT_CUR, TPSTATPROF%XLON_CUR, &
                 TPSTATPROF%XX_CUR,   TPSTATPROF%XY_CUR    )

END SUBROUTINE STATPROF_INI_INTERP

! ###########################################################
SUBROUTINE STATPROF_POSITION( TPSTATPROF, OINSIDE, OPRESENT )
! ###########################################################
! Subroutine to determine the position of a station/profiler on the model grid
! and set the useful coefficients for data interpolation

  USE MODD_CONF,           ONLY: L1D
  USE MODD_GRID_n,         ONLY: NPHYS_XMIN, NPHYS_XMAX, NPHYS_YMIN, NPHYS_YMAX, XHAT_BOUND, XHATM_BOUND, &
                                 XXHAT, XYHAT, XXHATM, XYHATM, XZZ
  USE MODD_PARAMETERS,     ONLY: JPHEXT, JPVEXT

  USE MODE_MSG
  USE MODE_NEST_LL,        ONLY: GET_MODEL_NUMBER_ll
  USE MODE_TOOLS_ll,       ONLY: GET_INDICE_ll

  IMPLICIT NONE

  CLASS(TSTATPROFDATA), INTENT(INOUT) :: TPSTATPROF
  LOGICAL,              INTENT(OUT)   :: OINSIDE  ! True if station/profiler is inside physical domain of model
  LOGICAL,              INTENT(OUT)   :: OPRESENT ! True if station/profiler is present on the current process

  INTEGER :: IIB ! domain sizes of current process
  INTEGER :: IJB !
  INTEGER :: IIE !
  INTEGER :: IJE !
  INTEGER :: IMI
  INTEGER :: JK
  REAL    :: ZLOW, ZHIGH

  OPRESENT = .FALSE.
  OINSIDE  = .FALSE.

  CALL GET_INDICE_ll( IIB, IJB, IIE, IJE )

  IF (       TPSTATPROF%XX_CUR >= XHAT_BOUND(NPHYS_XMIN) .AND. TPSTATPROF%XX_CUR <= XHAT_BOUND(NPHYS_XMAX) &
       .AND. TPSTATPROF%XY_CUR >= XHAT_BOUND(NPHYS_YMIN) .AND. TPSTATPROF%XY_CUR <= XHAT_BOUND(NPHYS_YMAX) ) THEN
    OINSIDE = .TRUE.
  ELSE
    CALL GET_MODEL_NUMBER_ll(IMI)
    WRITE( CMNHMSG(1), "( 'station or profiler ', A, ' is outside of physical domain of model', I3 )" ) TRIM(TPSTATPROF%CNAME), IMI
    CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'STATPROF_POSITION' )
 END IF

  ! X position
  TPSTATPROF%NI_U = COUNT( XXHAT (:) <= TPSTATPROF%XX_CUR )
  TPSTATPROF%NI_M = COUNT( XXHATM(:) <= TPSTATPROF%XX_CUR )

  ! Y position
  TPSTATPROF%NJ_V = COUNT( XYHAT (:) <= TPSTATPROF%XY_CUR )
  TPSTATPROF%NJ_M = COUNT( XYHATM(:) <= TPSTATPROF%XY_CUR )

  ! Position of station/profiler according to process
  IF (       TPSTATPROF%NI_U >= IIB .AND. TPSTATPROF%NI_U <= IIE &
       .AND. TPSTATPROF%NJ_V >= IJB .AND. TPSTATPROF%NJ_V <= IJE ) OPRESENT = .TRUE.
  IF ( L1D ) OPRESENT = .TRUE.

  ! Check if station/profiler is too near of physical domain border (outside of physical domain for mass points)
  IF ( OINSIDE .AND. .NOT. L1D ) THEN
    IF (      TPSTATPROF%XX_CUR < XHATM_BOUND(NPHYS_XMIN) .OR. TPSTATPROF%XX_CUR > XHATM_BOUND(NPHYS_XMAX) &
         .OR. TPSTATPROF%XY_CUR < XHATM_BOUND(NPHYS_YMIN) .OR. TPSTATPROF%XY_CUR > XHATM_BOUND(NPHYS_YMAX) ) THEN
      CALL GET_MODEL_NUMBER_ll(IMI)
      WRITE( CMNHMSG(1), "( 'station or profiler ', A, ' is outside of mass-points physical domain of model', I3 )" ) &
             TRIM(TPSTATPROF%CNAME), IMI
      CMNHMSG(2) = 'but is inside of flux-points physical domain.'
      CMNHMSG(3) = 'Meaning: station or profiler is too close to the boundaries of physical domain.'
      CMNHMSG(4) = '=> station or profiler disabled (not computed)'
      CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'STATPROF_POSITION' )
      OPRESENT = .FALSE.
      OINSIDE = .FALSE.
    END IF
  END IF

  ! Computations only on correct process
  IF ( OPRESENT .AND. .NOT. L1D ) THEN
    ! Interpolation coefficient for X (mass-point)
    TPSTATPROF%XXMCOEF = ( TPSTATPROF%XX_CUR - XXHATM(TPSTATPROF%NI_M) ) / ( XXHATM(TPSTATPROF%NI_M+1) - XXHATM(TPSTATPROF%NI_M) )
    ! Interpolation coefficient for Y (mass-point)
    TPSTATPROF%XYMCOEF = ( TPSTATPROF%XY_CUR - XYHATM(TPSTATPROF%NJ_M) ) / ( XYHATM(TPSTATPROF%NJ_M+1) - XYHATM(TPSTATPROF%NJ_M) )
    ! Interpolation coefficient for X (U-point)
    TPSTATPROF%XXUCOEF = ( TPSTATPROF%XX_CUR - XXHAT(TPSTATPROF%NI_U) )  / ( XXHAT(TPSTATPROF%NI_U+1)  - XXHAT(TPSTATPROF%NI_U) )
    ! Interpolation coefficient for Y (V-point)
    TPSTATPROF%XYVCOEF = ( TPSTATPROF%XY_CUR - XYHAT(TPSTATPROF%NJ_V) )  / ( XYHAT(TPSTATPROF%NJ_V+1)  - XYHAT(TPSTATPROF%NJ_V) )
  END IF

  IF ( OPRESENT ) THEN
    SELECT TYPE( TPSTATPROF )
      TYPE IS( TPROFILERDATA )
        ! Nothing to do

      TYPE IS( TSTATIONDATA )
        ! The closest K-level to the station altitude is chosen
        JK = JPVEXT + 1
        DO WHILE ( (   TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XZZ(:,:,JK) )         &
                     - TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XZZ(:,:,JPVEXT+1) ) ) &
                   < TPSTATPROF%XZ_CUR )
          JK = JK + 1
        END DO
        ZLOW  = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XZZ(:,:,JK-1) ) - TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XZZ(:,:,JPVEXT+1) )
        ZHIGH = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XZZ(:,:,JK  ) ) - TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XZZ(:,:,JPVEXT+1) )
        !If the station/profiler is nearer from the lower level, select it
        IF ( ( ZHIGH - TPSTATPROF%XZ_CUR ) > ( TPSTATPROF%XZ_CUR - ZLOW ) ) JK = JK - 1
        TPSTATPROF%NK = JK
        TPSTATPROF%XZMEAS = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XZZ(:,:,JK) )

      CLASS DEFAULT
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'STATPROF_POSITION', 'unknown type for TPSTATPROF', OLOCAL = .TRUE. )
    END SELECT
  END IF

END SUBROUTINE STATPROF_POSITION

! ###################################
SUBROUTINE PROFILER_ADD( TPPROFILER )
! ###################################
! Subroutine to add a profiler to the local list of profilers
  USE MODD_PROFILER_n, ONLY: NUMBPROFILER_LOC, TPROFILERS

  IMPLICIT NONE

  CLASS(TPROFILERDATA), INTENT(IN) :: TPPROFILER

  INTEGER :: JS
  TYPE(TPROFILERDATA), DIMENSION(:), POINTER :: TZPROFILERS

  NUMBPROFILER_LOC = NUMBPROFILER_LOC + 1

  ALLOCATE( TZPROFILERS( NUMBPROFILER_LOC ) )
  DO JS = 1, NUMBPROFILER_LOC - 1
    TZPROFILERS(JS) = TPROFILERS(JS)
  END DO

  TZPROFILERS(NUMBPROFILER_LOC)  = TPPROFILER

  IF ( ASSOCIATED( TPROFILERS ) ) DEALLOCATE( TPROFILERS ) !Can be done without memory leak because allocatable arrays were
                                                           !not yet allocated (will be done in PROFILER_ALLOCATE)
  TPROFILERS => TZPROFILERS

END SUBROUTINE PROFILER_ADD

! #################################
SUBROUTINE STATION_ADD( TPSTATION )
! #################################
! Subroutine to add a station to the local list of stations
  USE MODD_STATION_n, ONLY: NUMBSTAT_LOC, TSTATIONS

  IMPLICIT NONE

  CLASS(TSTATIONDATA), INTENT(IN) :: TPSTATION

  INTEGER :: JS
  TYPE(TSTATIONDATA), DIMENSION(:), POINTER :: TZSTATIONS

  NUMBSTAT_LOC = NUMBSTAT_LOC + 1

  ALLOCATE( TZSTATIONS( NUMBSTAT_LOC ) )
  DO JS = 1, NUMBSTAT_LOC - 1
    TZSTATIONS(JS) = TSTATIONS(JS)
  END DO

  TZSTATIONS(NUMBSTAT_LOC) = TPSTATION

  IF ( ASSOCIATED( TSTATIONS ) ) DEALLOCATE( TSTATIONS ) !Can be done without memory leak because allocatable arrays were
                                                         !not yet allocated (will be done in STATION_ALLOCATE)
  TSTATIONS => TZSTATIONS

END SUBROUTINE STATION_ADD

END MODULE MODE_STATPROF_TOOLS
