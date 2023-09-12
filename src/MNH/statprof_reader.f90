!MNH_LIC Copyright 2020-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ############################
       MODULE MODE_STATPROF_READER
!     ############################

IMPLICIT NONE

PRIVATE

PUBLIC :: STATPROF_CSV_READ

INTEGER, PARAMETER :: NMAXLINELGT = 400

CONTAINS
!-------------------------------------------------------------------
!
!!****  *STATPROF_CSV_READ* -
!!
!!    PURPOSE
!!    -------
!!    Prescribe probes through a CSV file
!!
!!    AUTHOR
!!    ------
!!     E. Jezequel *CNRM & IFPEN*
!!
!!    MODIFICATIONS
!!    -------------
!!     03/2020      Original
!  P. Wautelet    04/2022: restructure stations/profilers for better performance, reduce memory usage and correct some problems/bugs
!---------------------------------------------------------------
!
!###############################################################
SUBROUTINE STATPROF_CSV_READ( TPSTATPROF, HFILE, KNUMBSTATPROF )
!###############################################################

USE MODD_CONF,          ONLY: LCARTESIAN
USE MODD_TYPE_STATPROF, ONLY: TPROFILERDATA, TSTATIONDATA, TSTATPROFDATA

USE MODE_MSG
USE MODE_STATPROF_TOOLS, ONLY: PROFILER_ADD, STATION_ADD, STATPROF_INI_INTERP, STATPROF_POSITION

CLASS(TSTATPROFDATA), INTENT(IN)  :: TPSTATPROF ! Used only to identify datatype
CHARACTER(LEN=*),     INTENT(IN)  :: HFILE ! file to read
INTEGER,              INTENT(OUT) :: KNUMBSTATPROF ! Total number of stations/profilers (inside physical domain of model)
!
CHARACTER(LEN=NMAXLINELGT) :: YSTRING
INTEGER             :: ILU      ! logical unit of the file
INTEGER             :: INBLINE  ! Nb of lines in csv file
LOGICAL             :: GINSIDE  ! True if station/profiler is inside physical domain of model
LOGICAL             :: GPRESENT ! True if station/profiler is present on the current process
TYPE(TSTATIONDATA),  TARGET :: TZSTATION
TYPE(TPROFILERDATA), TARGET :: TZPROFILER

CLASS(TSTATPROFDATA), POINTER :: TZSTATPROF

SELECT TYPE( TPSTATPROF )
  TYPE IS( TPROFILERDATA )
    TZSTATPROF => TZPROFILER

  TYPE IS( TSTATIONDATA )
    TZSTATPROF => TZSTATION

  CLASS DEFAULT
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'STATPROF_CSV_READ', 'unknown type for TPSTATPROF' )
END SELECT

INBLINE       = 0 !Number of stations/profilers found in the file
KNUMBSTATPROF = 0 !Number of stations/profilers found in the file AND inside the model domain

! Open file
OPEN( NEWUNIT = ILU, FILE = HFILE, FORM = 'formatted' )

READ( ILU, END = 101, FMT = '(A)' ) YSTRING ! Reading of header (skip it)

DO
  ! Read station/profiler coordinates
  READ( ILU, END = 101, FMT = '(A)' ) YSTRING

  ! Skip empty lines
  IF ( LEN_TRIM( YSTRING ) == 0 ) CYCLE

  ! Check if record is written in French convention
  CALL FRENCH_TO_ENGLISH( YSTRING )

  IF ( LCARTESIAN ) THEN
    READ( YSTRING, * ) TZSTATPROF%CNAME, TZSTATPROF%XX_CUR,   TZSTATPROF%XY_CUR,   TZSTATPROF%XZ_CUR
  ELSE
    READ( YSTRING, * ) TZSTATPROF%CNAME, TZSTATPROF%XLAT_CUR, TZSTATPROF%XLON_CUR, TZSTATPROF%XZ_CUR
  END IF

  IF ( .NOT. LCARTESIAN ) CALL STATPROF_INI_INTERP( TZSTATPROF )
  CALL STATPROF_POSITION( TZSTATPROF, GINSIDE, GPRESENT )

  IF ( GINSIDE ) THEN
    KNUMBSTATPROF = KNUMBSTATPROF + 1
    TZSTATPROF%NID = KNUMBSTATPROF
  END IF

  IF ( GPRESENT ) THEN
    SELECT TYPE( TZSTATPROF )
      TYPE IS( TPROFILERDATA )
        CALL PROFILER_ADD( TZSTATPROF )

      TYPE IS( TSTATIONDATA )
        CALL STATION_ADD( TZSTATPROF )

      CLASS DEFAULT
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'STATPROF_CSV_READ', 'unknown type for TPSTATPROF', OLOCAL = .TRUE. )
    END SELECT
  END IF

  INBLINE = INBLINE + 1
END DO

101 CONTINUE

CLOSE( ILU )

IF ( INBLINE == 0 ) CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'STATPROF_CSV_READ', 'Data not found in file ' // TRIM( HFILE ) )

END SUBROUTINE STATPROF_CSV_READ

!#########################################################
SUBROUTINE FRENCH_TO_ENGLISH(HSTRING)
CHARACTER(LEN=NMAXLINELGT), INTENT(INOUT) :: HSTRING ! csv record

INTEGER :: JL
LOGICAL :: GFRENCH
!
GFRENCH = .FALSE.
!* analyses if the record has been written in French convention 
!     French  convention (separator is ;  decimal symbol is ,) 
!  or English convention (separator is ,  decimal symbol is .)
DO JL = 1, NMAXLINELGT
 IF (HSTRING(JL:JL)==';') GFRENCH=.TRUE.
END DO
!
! If French convention is used in the file, transforms it in English convention
IF (GFRENCH) THEN
 DO JL = 1, NMAXLINELGT
   IF (HSTRING(JL:JL)==',') HSTRING(JL:JL)='.'
   IF (HSTRING(JL:JL)==';') HSTRING(JL:JL)=','
 END DO
END IF
!
END SUBROUTINE FRENCH_TO_ENGLISH

END MODULE MODE_STATPROF_READER
