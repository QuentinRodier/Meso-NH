!MNH_LIC Copyright 2020-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
       MODULE MODE_STATION_READER
!     #######################

IMPLICIT NONE

PRIVATE

PUBLIC :: READ_CSV_STATION

INTEGER, PARAMETER :: NMAXLINELGT = 400

CONTAINS
!-------------------------------------------------------------------
!
!!****  *READ_CSV_STATION* -
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
!  P. Wautelet    04/2022: restructure stations for better performance, reduce memory usage and correct some problems/bugs
!---------------------------------------------------------------
!
!###############################################################################################
SUBROUTINE READ_CSV_STATION( HFILE, PXHAT_GLOB, PYHAT_GLOB, PXHATM, PYHATM,                    &
                             PXHATM_PHYS_MIN, PXHATM_PHYS_MAX,PYHATM_PHYS_MIN, PYHATM_PHYS_MAX )
!###############################################################################################

USE MODD_CONF,          ONLY: LCARTESIAN
USE MODD_STATION_n,     ONLY: NUMBSTAT
USE MODD_TYPE_STATION,  ONLY: TSTATIONDATA

USE MODE_MSG
USE MODE_STATION_TOOLS, ONLY: STATION_ADD, STATION_INI_INTERP, STATION_POSITION

CHARACTER(LEN=*),   INTENT(IN) :: HFILE ! file to read
REAL, DIMENSION(:), INTENT(IN) :: PXHAT_GLOB
REAL, DIMENSION(:), INTENT(IN) :: PYHAT_GLOB
REAL, DIMENSION(:), INTENT(IN) :: PXHATM ! mass point coordinates
REAL, DIMENSION(:), INTENT(IN) :: PYHATM ! mass point coordinates
REAL,               INTENT(IN) :: PXHATM_PHYS_MIN, PYHATM_PHYS_MIN  ! Minimum X coordinate of mass points in the physical domain
REAL,               INTENT(IN) :: PXHATM_PHYS_MAX, PYHATM_PHYS_MAX  ! Minimum X coordinate of mass points in the physical domain
!
CHARACTER(LEN=NMAXLINELGT) :: YSTRING
INTEGER            :: ILU      ! logical unit of the file
INTEGER            :: INBLINE  ! Nb of lines in csv file
INTEGER            :: JI
LOGICAL            :: GINSIDE  ! True if station is inside physical domain of model
LOGICAL            :: GPRESENT ! True if station is present on the current process
TYPE(TSTATIONDATA) :: TZSTATION

INBLINE  = 0 !Number of stations found in the file
NUMBSTAT = 0 !Number of stations found in the file AND inside the model domain

! Open file
OPEN( NEWUNIT = ILU, FILE = HFILE, FORM = 'formatted' )

READ( ILU, END = 101, FMT = '(A)' ) YSTRING ! Reading of header (skip it)

DO
  ! Read station coordinates
  READ( ILU, END = 101, FMT = '(A)' ) YSTRING

  ! Check if record is written in French convention
  CALL FRENCH_TO_ENGLISH( YSTRING )

  IF ( LCARTESIAN ) THEN
    READ( YSTRING, * ) TZSTATION%CNAME, TZSTATION%XX,   TZSTATION%XY,   TZSTATION%XZ
  ELSE
    READ( YSTRING, * ) TZSTATION%CNAME, TZSTATION%XLAT, TZSTATION%XLON, TZSTATION%XZ
  END IF

  IF ( .NOT. LCARTESIAN ) CALL STATION_INI_INTERP( TZSTATION )
  CALL STATION_POSITION( TZSTATION, PXHAT_GLOB, PYHAT_GLOB, PXHATM, PYHATM,                  &
                         PXHATM_PHYS_MIN, PXHATM_PHYS_MAX, PYHATM_PHYS_MIN, PYHATM_PHYS_MAX, &
                         GINSIDE, GPRESENT                                                   )

  IF ( GINSIDE ) THEN
    NUMBSTAT = NUMBSTAT + 1
    TZSTATION%NID = NUMBSTAT
  END IF

  IF ( GPRESENT ) CALL STATION_ADD( TZSTATION )

  INBLINE = INBLINE + 1
END DO

101 CONTINUE

CLOSE( ILU )

IF ( INBLINE == 0 ) CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'READ_CSV_STATION', 'Data not found in file ' // TRIM( HFILE ) )

END SUBROUTINE READ_CSV_STATION

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

END MODULE MODE_STATION_READER
