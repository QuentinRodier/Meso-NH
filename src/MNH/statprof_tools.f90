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
PUBLIC :: STATPROF_DIAG_SURFRAD
PUBLIC :: ADD_DIAG_SURFRAD_DATA

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

! #################################
SUBROUTINE STATPROF_DIAG_SURFRAD( TPSTATPROF, KSTORE )
! #################################
  ! Store surface and radiative values
  USE MODD_DIAG_IN_RUN
  USE MODD_PARAM_n,     ONLY: CRAD

  CLASS(TSTATPROFDATA), INTENT(INOUT) :: TPSTATPROF
  INTEGER,              INTENT(IN)    :: KSTORE

  TPSTATPROF%XZON10M(KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_ZON10M )
  TPSTATPROF%XMER10M(KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_MER10M )
  TPSTATPROF%XT2M   (KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_T2M    )
  TPSTATPROF%XQ2M   (KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_Q2M    )
  TPSTATPROF%XHU2M  (KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_HU2M   )
  TPSTATPROF%XRN    (KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_RN     )
  TPSTATPROF%XH     (KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_H      )
  TPSTATPROF%XLE    (KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_LE     )
  TPSTATPROF%XLEI   (KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_LEI    )
  TPSTATPROF%XGFLUX (KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_GFLUX  )
  IF ( CRAD /= 'NONE' ) THEN
    TPSTATPROF%XSWD   (KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_SWD    )
    TPSTATPROF%XSWU   (KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_SWU    )
    TPSTATPROF%XLWD   (KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_LWD    )
    TPSTATPROF%XLWU   (KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_LWU    )
    TPSTATPROF%XSWDIR (KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_SWDIR  )
    TPSTATPROF%XSWDIFF(KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_SWDIFF )
    TPSTATPROF%XDSTAOD(KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_DSTAOD )
    TPSTATPROF%XSLTAOD(KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_SLTAOD )
  END IF
  TPSTATPROF%XSFCO2(KSTORE) = TPSTATPROF%INTERP_HOR_FROM_MASSPOINT( XCURRENT_SFCO2 )

END SUBROUTINE STATPROF_DIAG_SURFRAD

! ############################################
SUBROUTINE Add_diag_surfrad_data( TPSTATPROF )
! ############################################
  use modd_param_n, only: crad, csurf

  use mode_sensor,         only: Add_point

  class(tstatprofdata), intent(in) :: tpstatprof

  if ( csurf == "EXTE" ) then
    call Add_point( 'T2m',    '2-m temperature',        'K',       tpstatprof%xt2m(:)    )
    call Add_point( 'Q2m',    '2-m humidity',           'kg kg-1', tpstatprof%xq2m(:)    )
    call Add_point( 'HU2m',   '2-m relative humidity',  'percent', tpstatprof%xhu2m(:)   )
    call Add_point( 'zon10m', '10-m zonal wind',        'm s-1',   tpstatprof%xzon10m(:) )
    call Add_point( 'mer10m', '10-m meridian wind',     'm s-1',   tpstatprof%xmer10m(:) )
    call Add_point( 'RN',     'Net radiation',          'W m-2',   tpstatprof%xrn(:)     )
    call Add_point( 'H',      'Sensible heat flux',     'W m-2',   tpstatprof%xh(:)      )
    call Add_point( 'LE',     'Total Latent heat flux', 'W m-2',   tpstatprof%xle(:)     )
    call Add_point( 'G',      'Storage heat flux',      'W m-2',   tpstatprof%xgflux(:)  )
    call Add_point( 'LEI',    'Solid Latent heat flux', 'W m-2',   tpstatprof%xlei(:)    )
  end if

  if ( crad /= 'NONE' ) then
    call Add_point( 'SWD',    'Downward short-wave radiation',         'W m-2', tpstatprof%xswd(:)    )
    call Add_point( 'SWU',    'Upward short-wave radiation',           'W m-2', tpstatprof%xswu(:)    )
    call Add_point( 'LWD',    'Downward long-wave radiation',          'W m-2', tpstatprof%xlwd(:)    )
    call Add_point( 'LWU',    'Upward long-wave radiation',            'W m-2', tpstatprof%xlwu(:)    )
    call Add_point( 'SWDIR',  'Downward direct short-wave radiation',  'W m-2', tpstatprof%xswdir(:)  )
    call Add_point( 'SWDIFF', 'Downward diffuse short-wave radiation', 'W m-2', tpstatprof%xswdiff(:) )
    call Add_point( 'DSTAOD', 'Dust aerosol optical depth',            'm',     tpstatprof%xdstaod(:) )
    call Add_point( 'SLTAOD', 'Salt aerosol optical depth',            'm',     tpstatprof%xsltaod(:) )
  end if

  call Add_point( 'SFCO2', 'CO2 Surface Flux', 'mg m-2 s-1', tpstatprof%xsfco2(:) )

END SUBROUTINE Add_diag_surfrad_data

END MODULE MODE_STATPROF_TOOLS
