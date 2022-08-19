!MNH_LIC Copyright 2000-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
MODULE MODE_INI_AIRCRAFT

IMPLICIT NONE

PRIVATE

PUBLIC :: INI_AIRCRAFT

INTEGER, PARAMETER :: NMAXLINELGT = 256

CONTAINS

!     #######################
      SUBROUTINE INI_AIRCRAFT
!     #######################
!
!
!!****  *INI_AIRCRAFT* - user initializes the aircraft flight path
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!    
!!   Must be defined (for each aircraft):
!!   ---------------
!!
!!  No default exist for these variables.
!!  ************************************
!!
!!  1) the model in which the aircraft will evolve
!!     if NOT initialized, the aircraft is NOT used.
!!
!!  2) the possibility to switch from a model to its dad or kid
!!       'FIX' : NMODEL used during the run
!!       'MOB' : best resolution model used. NMODEL=1 is used at the beginning
!!
!!
!!  3) the type of aircraft
!!
!!     'AIRCRA' for aircraft
!!
!!  4) the takeoff date and time
!!
!!  5) the number of flight path segments (SEG)
!!
!!  6) the (SEG  ) duration of flight in the segments, in the flight order (sec.)
!!
!!  6bis) TAIRCRAFT%LALTDEF : flag to define the mode of initialisation of
!!        aircraft altitude TRUE for pressure (corresponding to %XSEGP)
!!        or FALSE for Z (corresponding to %XSEGZ)
!!
!!  7) the (SEG+1) latitudes of the segments ends, in the flight order
!!     first point is take-off
!!     last  point is landing
!!
!!  8) the (SEG+1) longitudes of the segments ends, in the flight order
!!
!!  9) the (SEG+1) pressure (%XSEGP) or Z (%XSEGZ) of the segments ends, in the flight order
!!
!!
!!
!!   Can be defined  (for each aircraft):
!!   --------------
!!
!!
!!  9) the time step for data storage.
!!    default is 60s
!!
!! 10) the name or title describing the aircraft (8 characters)
!!     default is the aircraft type (6 characters) + the aircraft numbers (2 characters)
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      Valery Masson             * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!     Original 15/05/2000
!!             Sept2009, A. Boilley add initialisation of aircraft altitude by Z
!  P. Wautelet    06/2022: reorganize flyers
!  P. Wautelet 19/08/2022: provide aircraft characteristics in namelist and CSV file instead of hardcoded
! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_AIRCRAFT_BALLOON
USE MODD_CONF,             ONLY: NMODEL_NEST => NMODEL
USE MODD_PARAMETERS,       ONLY: XNEGUNDEF

USE MODE_MSG

USE MODN_AIRCRAFTS

IMPLICIT NONE

INTEGER :: JI

ALLOCATE( TAIRCRAFTS(NAIRCRAFTS) )

!Treat aircraft data read in namelist
DO JI = 1, NAIRCRAFTS
  IF ( CTITLE(JI) == '' ) THEN
    WRITE( CTITLE(JI), FMT = '( A, I3.3) ') TRIM( CTYPE(JI) ), JI

    WRITE( CMNHMSG(1), FMT = '( A, I4 )' ) 'no title given to aircraft number ', JI
    CMNHMSG(2) = 'title set to ' // TRIM( CTITLE(JI) )
    CALL PRINT_MSG( NVERB_INFO, 'GEN', 'INI_AIRCRAFT' )
  END IF
  TAIRCRAFTS(JI)%CTITLE = CTITLE(JI)

  IF ( CMODEL(JI) == 'FIX' ) THEN
    IF ( NMODEL(JI) < 1 .OR. NMODEL(JI) > NMODEL_NEST ) THEN
      CMNHMSG(1) = 'invalid NMODEL aircraft ' // TRIM( CTITLE(JI) )
      CMNHMSG(2) = 'NMODEL must be between 1 and the last nested model number'
      CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_AIRCRAFT' )
      NMODEL(JI) = 1
    END IF
  ELSE IF ( CMODEL(JI) == 'MOB' ) THEN
    IF ( NMODEL(JI) /= 0 .AND. NMODEL(JI) /= 1 ) THEN
      CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_AIRCRAFT', &
                      'NMODEL is set to 1 at start for a CMODEL="MOB" aircraft (aircraft ' // TRIM( CTITLE(JI) ) // ')' )
    END IF
    NMODEL(JI) = 1
  ELSE
    CMNHMSG(1) = 'invalid CMODEL (' // TRIM( CMODEL(JI) ) // ') for aircraft ' // TRIM( CTITLE(JI) )
    CMNHMSG(2) = 'CMODEL must be FIX or MOB (default="FIX")'
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_AIRCRAFT' )
    CMODEL(JI) = 'FIX'
    NMODEL(JI) = 1
  END IF
  TAIRCRAFTS(JI)%CMODEL = CMODEL(JI)
  TAIRCRAFTS(JI)%NMODEL = NMODEL(JI)

  TAIRCRAFTS(JI)%CTYPE = CTYPE(JI)

  IF ( .NOT. TLAUNCH(JI)%CHECK( TRIM( CTITLE(JI) ) ) ) &
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_AIRCRAFT', &
                        'problem with TLAUNCH (not set or incorrect values) for aircraft ' // TRIM( CTITLE(JI) ) )
  TAIRCRAFTS(JI)%TLAUNCH  = TLAUNCH(JI)

  IF ( XTSTEP(JI) == XNEGUNDEF ) THEN
    CALL PRINT_MSG( NVERB_INFO, 'GEN', 'INI_AIRCRAFT', &
                    'data storage frequency not provided for aircraft ' // TRIM( CTITLE(JI) ) // ' => set to 60s' )
    XTSTEP(JI) = 60.
  ELSE IF ( XTSTEP(JI) <=0. ) THEN
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_AIRCRAFT', 'invalid data storage frequency for aircraft ' // TRIM( CTITLE(JI) ) )
    XTSTEP(JI) = 60.
  END IF
  TAIRCRAFTS(JI)%TFLYER_TIME%XTSTEP = XTSTEP(JI)

  IF ( NPOS(JI) < 1 ) THEN
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_AIRCRAFT', 'NPOS should be at least 1 for aircraft ' // TRIM( CTITLE(JI) ) )
  END IF
  TAIRCRAFTS(JI)%NSEG = NPOS(JI)-1

  TAIRCRAFTS(JI)%LALTDEF = LALTDEF(JI)

  IF ( CFILE(JI) == '' ) &
    CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_AIRCRAFT', 'name of CSV file with trajectory not provided for aircraft ' &
                    // TRIM( CTITLE(JI) ) )

  ! Allocate trajectory data
  ALLOCATE( TAIRCRAFTS(JI)%XSEGTIME(TAIRCRAFTS(JI)%NSEG  ) ); TAIRCRAFTS(JI)%XSEGTIME(:) = XNEGUNDEF
  ALLOCATE( TAIRCRAFTS(JI)%XSEGLAT (TAIRCRAFTS(JI)%NSEG+1) ); TAIRCRAFTS(JI)%XSEGLAT(:)  = XNEGUNDEF
  ALLOCATE( TAIRCRAFTS(JI)%XSEGLON (TAIRCRAFTS(JI)%NSEG+1) ); TAIRCRAFTS(JI)%XSEGLON(:)  = XNEGUNDEF
  IF ( TAIRCRAFTS(JI)%LALTDEF ) THEN
    ALLOCATE( TAIRCRAFTS(JI)%XSEGP (TAIRCRAFTS(JI)%NSEG+1) ); TAIRCRAFTS(JI)%XSEGP(:)    = XNEGUNDEF
  ELSE
    ALLOCATE( TAIRCRAFTS(JI)%XSEGZ (TAIRCRAFTS(JI)%NSEG+1) ); TAIRCRAFTS(JI)%XSEGZ(:)    = XNEGUNDEF
  END IF

  ! Read CSV data (trajectory)
  CALL AIRCRAFT_CSV_READ( TAIRCRAFTS(JI), CFILE(JI) )

END DO

IF ( NAIRCRAFTS > 0 ) CALL AIRCRAFTS_NML_DEALLOCATE()
!
!----------------------------------------------------------------------------
!
!
END SUBROUTINE INI_AIRCRAFT


SUBROUTINE AIRCRAFT_CSV_READ( TPAIRCRAFT, HFILE )

USE MODD_AIRCRAFT_BALLOON, ONLY: TAIRCRAFTDATA

USE MODE_MSG

IMPLICIT NONE

TYPE(TAIRCRAFTDATA), INTENT(INOUT) :: TPAIRCRAFT
CHARACTER(LEN=*),    INTENT(IN)    :: HFILE !Name of the CSV file with the aircraft trajectory

CHARACTER(LEN=NMAXLINELGT) :: YSTRING
INTEGER                    :: ILU      ! logical unit of the file
INTEGER                    :: JI
REAL                       :: ZTIME, ZLAT, ZLON, ZALT
REAL                       :: ZTIME_OLD

ZTIME_OLD = 0.

! Open file
OPEN( NEWUNIT = ILU, FILE = HFILE, FORM = 'formatted' )

READ( ILU, END = 101, FMT = '(A)' ) YSTRING ! Reading of header (skip it)

DO JI = 1, TPAIRCRAFT%NSEG + 1
  ! Read aircraft position
  READ( ILU, END = 101, FMT = '(A)' ) YSTRING

  READ( YSTRING, * ) ZTIME, ZLAT, ZLON, ZALT

  IF ( JI > 1 ) TPAIRCRAFT%XSEGTIME(JI-1) = ZTIME - ZTIME_OLD
  TPAIRCRAFT%XSEGLAT(JI) = ZLAT
  TPAIRCRAFT%XSEGLON(JI) = ZLON
  IF ( TPAIRCRAFT%LALTDEF ) THEN
    TPAIRCRAFT%XSEGP(JI) = ZALT * 100. ! *100 to convert from hPa to Pa
  ELSE
    TPAIRCRAFT%XSEGZ(JI) = ZALT
  END IF

  ZTIME_OLD = ZTIME
END DO

101 CONTINUE

CLOSE( ILU )

IF ( JI < TPAIRCRAFT%NSEG + 1 ) &
  CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'AIRCRAFT_CSV_READ', 'Data not found in file ' // TRIM( HFILE ) )

END SUBROUTINE AIRCRAFT_CSV_READ

END MODULE MODE_INI_AIRCRAFT
