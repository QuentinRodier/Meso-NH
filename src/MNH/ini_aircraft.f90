!MNH_LIC Copyright 2000-2023 CNRS, Meteo-France and Universite Paul Sabatier
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
TYPE(TAIRCRAFTDATA), POINTER :: TZAIRCRAFT

!Treat aircraft data read in namelist
DO JI = 1, NAIRCRAFTS
    ALLOCATE( TAIRCRAFTS(JI)%TAIRCRAFT )
    TZAIRCRAFT => TAIRCRAFTS(JI)%TAIRCRAFT

    TZAIRCRAFT%NID = JI

    TZAIRCRAFT%LFIX = .FALSE.

  IF ( CTITLE(JI) == '' ) THEN
    WRITE( CTITLE(JI), FMT = '( A, I3.3) ') TRIM( CTYPE(JI) ), JI

    WRITE( CMNHMSG(1), FMT = '( A, I4 )' ) 'no title given to aircraft number ', JI
    CMNHMSG(2) = 'title set to ' // TRIM( CTITLE(JI) )
    CALL PRINT_MSG( NVERB_INFO, 'GEN', 'INI_AIRCRAFT', OLOCAL = .TRUE. )
  END IF
  TZAIRCRAFT%CNAME = CTITLE(JI)

  IF ( CMODEL(JI) == 'FIX' ) THEN
    IF ( NMODEL(JI) < 1 .OR. NMODEL(JI) > NMODEL_NEST ) THEN
      CMNHMSG(1) = 'invalid NMODEL aircraft ' // TRIM( CTITLE(JI) )
      CMNHMSG(2) = 'NMODEL must be between 1 and the last nested model number'
      CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_AIRCRAFT', OLOCAL = .TRUE. )
      NMODEL(JI) = 1
    END IF
  ELSE IF ( CMODEL(JI) == 'MOB' ) THEN
    IF ( NMODEL(JI) /= 0 .AND. NMODEL(JI) /= 1 ) THEN
      CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_AIRCRAFT', &
                      'NMODEL is set to 1 at start for a CMODEL="MOB" aircraft (aircraft ' // TRIM( CTITLE(JI) ) // ')', &
                      OLOCAL = .TRUE. )
    END IF
    IF ( NMODEL_NEST == 1 ) CMODEL(JI) = 'FIX' ! If only one model, FIX and MOB are the same
    NMODEL(JI) = 1
  ELSE
    CMNHMSG(1) = 'invalid CMODEL (' // TRIM( CMODEL(JI) ) // ') for aircraft ' // TRIM( CTITLE(JI) )
    CMNHMSG(2) = 'CMODEL must be FIX or MOB (default="FIX")'
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_AIRCRAFT', OLOCAL = .TRUE. )
    CMODEL(JI) = 'FIX'
    NMODEL(JI) = 1
  END IF
  TZAIRCRAFT%CMODEL = CMODEL(JI)
  TZAIRCRAFT%NMODEL = NMODEL(JI)

  TZAIRCRAFT%CTYPE = CTYPE(JI)

  IF ( .NOT. TLAUNCH(JI)%CHECK( TRIM( CTITLE(JI) ) ) ) &
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_AIRCRAFT', &
                        'problem with TLAUNCH (not set or incorrect values) for aircraft ' // TRIM( CTITLE(JI) ), OLOCAL = .TRUE. )
  TZAIRCRAFT%TLAUNCH  = TLAUNCH(JI)

  IF ( XTSTEP(JI) == XNEGUNDEF ) THEN
    CALL PRINT_MSG( NVERB_INFO, 'GEN', 'INI_AIRCRAFT', &
                    'data storage frequency not provided for aircraft ' // TRIM( CTITLE(JI) ) // ' => set to 60s', OLOCAL = .TRUE. )
    XTSTEP(JI) = 60.
  ELSE IF ( XTSTEP(JI) <=0. ) THEN
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_AIRCRAFT', 'invalid data storage frequency for aircraft ' // TRIM( CTITLE(JI) ), &
                    OLOCAL = .TRUE. )
    XTSTEP(JI) = 60.
  END IF
  TZAIRCRAFT%TFLYER_TIME%XTSTEP = XTSTEP(JI)

  IF ( NPOS(JI) < 2 ) THEN
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_AIRCRAFT', 'NPOS should be at least 2 for aircraft ' // TRIM( CTITLE(JI) ), &
                    OLOCAL = .TRUE. )
  END IF
  TZAIRCRAFT%NPOS = NPOS(JI)

  TZAIRCRAFT%LALTDEF = LALTDEF(JI)

  IF ( CFILE(JI) == '' ) &
    CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_AIRCRAFT', 'name of CSV file with trajectory not provided for aircraft ' &
                    // TRIM( CTITLE(JI) ), OLOCAL = .TRUE. )

  ! Allocate trajectory data
  ALLOCATE( TZAIRCRAFT%XPOSTIME(TZAIRCRAFT%NPOS) ); TZAIRCRAFT%XPOSTIME(:) = XNEGUNDEF
  ALLOCATE( TZAIRCRAFT%XPOSLAT (TZAIRCRAFT%NPOS) ); TZAIRCRAFT%XPOSLAT(:)  = XNEGUNDEF
  ALLOCATE( TZAIRCRAFT%XPOSLON (TZAIRCRAFT%NPOS) ); TZAIRCRAFT%XPOSLON(:)  = XNEGUNDEF
  IF ( TZAIRCRAFT%LALTDEF ) THEN
    ALLOCATE( TZAIRCRAFT%XPOSP (TZAIRCRAFT%NPOS) ); TZAIRCRAFT%XPOSP(:)    = XNEGUNDEF
  ELSE
    ALLOCATE( TZAIRCRAFT%XPOSZ (TZAIRCRAFT%NPOS) ); TZAIRCRAFT%XPOSZ(:)    = XNEGUNDEF
  END IF

  ! Read CSV data (trajectory)
  CALL AIRCRAFT_CSV_READ( TZAIRCRAFT, CFILE(JI) )

  IF ( TZAIRCRAFT%LALTDEF ) THEN
    ! Print a warning if pressures seem too high (> 2000 hPa)
    IF ( ANY( TZAIRCRAFT%XPOSP > 2.E5 ) ) THEN
      CMNHMSG(1) = TRIM( TZAIRCRAFT%CNAME ) // ': pressure values seem too high'
      CMNHMSG(2) = 'check that they are given in hPa and not Pa'
      CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_AIRCRAFT', OLOCAL = .TRUE. )
    END IF
  END IF

END DO

IF ( NAIRCRAFTS > 0 ) CALL AIRCRAFTS_NML_DEALLOCATE()
!
!----------------------------------------------------------------------------
!
!
END SUBROUTINE INI_AIRCRAFT


SUBROUTINE AIRCRAFT_CSV_READ( TPAIRCRAFT, HFILE )

USE MODD_AIRCRAFT_BALLOON, ONLY: TAIRCRAFTDATA

USE MODE_DATETIME
USE MODE_MSG

IMPLICIT NONE

TYPE(TAIRCRAFTDATA), INTENT(INOUT) :: TPAIRCRAFT
CHARACTER(LEN=*),    INTENT(IN)    :: HFILE !Name of the CSV file with the aircraft trajectory

CHARACTER(LEN=NMAXLINELGT) :: YSTRING
INTEGER                    :: ILU      ! logical unit of the file
INTEGER                    :: ILINESREAD ! Number of lines read and treated (the 1st one is skipped)
INTEGER                    :: JI
REAL                       :: ZLAT, ZLON, ZALT
REAL                       :: ZTIME

ILINESREAD = 0

! Open file
OPEN( NEWUNIT = ILU, FILE = HFILE, FORM = 'formatted' )

READ( ILU, END = 101, FMT = '(A)' ) YSTRING ! Reading of header (skip it)

DO JI = 1, TPAIRCRAFT%NPOS
  ! Read aircraft position
  READ( ILU, END = 101, FMT = '(A)' ) YSTRING
  ILINESREAD = ILINESREAD + 1

  READ( YSTRING, * ) ZTIME, ZLAT, ZLON, ZALT

  TPAIRCRAFT%XPOSTIME(JI) = ZTIME
  TPAIRCRAFT%XPOSLAT(JI)  = ZLAT
  TPAIRCRAFT%XPOSLON(JI)  = ZLON
  IF ( TPAIRCRAFT%LALTDEF ) THEN
    TPAIRCRAFT%XPOSP(JI)  = ZALT * 100. ! *100 to convert from hPa to Pa
  ELSE
    TPAIRCRAFT%XPOSZ(JI)  = ZALT
  END IF
END DO

101 CONTINUE

CLOSE( ILU )

IF ( ILINESREAD < TPAIRCRAFT%NPOS ) &
  CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'AIRCRAFT_CSV_READ', 'Data not found in file ' // TRIM( HFILE ), OLOCAL = .TRUE. )

TPAIRCRAFT%TLAND = TPAIRCRAFT%TLAUNCH + TPAIRCRAFT%XPOSTIME(TPAIRCRAFT%NPOS)

END SUBROUTINE AIRCRAFT_CSV_READ

END MODULE MODE_INI_AIRCRAFT
