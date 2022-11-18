!MNH_LIC Copyright 2018-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 22/02/2019: use MOD intrinsics with same kind for all arguments (to respect Fortran standard)
!  P. Wautelet 19/04/2019: use modd_precision kinds
!  P. Wautelet 20/07/2021: modify DATETIME_TIME2REFERENCE and DATETIME_DISTANCE to allow correct computation with 32-bit floats
!  P. Wautelet 27/10/2022: add +, -, <= and > operators and improve older comparison subroutines (more robust but slower)
!-----------------------------------------------------------------
MODULE MODE_DATETIME
!
USE MODD_TYPE_DATE
!
USE MODE_MSG
!
IMPLICIT NONE
!
PRIVATE
!
PUBLIC :: DATETIME_DISTANCE, DATETIME_CORRECTDATE
PUBLIC :: TPREFERENCE_DATE
PUBLIC :: OPERATOR(<)
PUBLIC :: OPERATOR(<=)
PUBLIC :: OPERATOR(>)
PUBLIC :: OPERATOR(>=)
PUBLIC :: OPERATOR(+)
PUBLIC :: OPERATOR(-)
!
!Reference date (do not change it)
!To work with DATETIME_TIME2REFERENCE, we assume the year is a multiple of 400 + 1 and the date is January 1st (and time=0.)
TYPE(DATE_TIME),PARAMETER :: TPREFERENCE_DATE = DATE_TIME( nyear = 1601, nmonth = 1, nday =1 , xtime = 0. )
!
INTERFACE OPERATOR(<)
  MODULE PROCEDURE DATETIME_LT
END INTERFACE
!
INTERFACE OPERATOR(<=)
  MODULE PROCEDURE DATETIME_LE
END INTERFACE
!
INTERFACE OPERATOR(>)
  MODULE PROCEDURE DATETIME_GT
END INTERFACE
!
INTERFACE OPERATOR(>=)
  MODULE PROCEDURE DATETIME_GE
END INTERFACE
!
INTERFACE OPERATOR(+)
  MODULE PROCEDURE DATETIME_TIME_ADD
END INTERFACE
!
INTERFACE OPERATOR(-)
  MODULE PROCEDURE DATETIME_TIME_SUBSTRACT
END INTERFACE
!
CONTAINS
!
SUBROUTINE DATETIME_TIME2REFERENCE( TPDATE, KDAYS, PSEC )
!
!Compute number of days and seconds since reference date (and time)
!Days and seconds are separated to allow correct computation of differences even
!with reduced precision (mantissa is too small for 32-bit floats)
!
use modd_precision, only: MNHINT64

TYPE(DATE_TIME), INTENT(IN)  :: TPDATE
INTEGER,         INTENT(OUT) :: KDAYS
REAL,            INTENT(OUT) :: PSEC
!
INTEGER(KIND=MNHINT64) :: ILEAPS                          !Number of leap days
INTEGER(KIND=MNHINT64) :: IDAYS                           !Number of days since reference date
INTEGER(KIND=MNHINT64) :: IYEARS                          !Number of years since reference date
INTEGER(KIND=MNHINT64) :: IDAY_CUR, IMONTH_CUR, IYEAR_CUR !Currrent day, month and year
REAL                   :: ZSEC                            !Current time of the day (in seconds)
TYPE(DATE_TIME)        :: TZDATE
!
ILEAPS = 0_MNHINT64
IDAYS  = 0_MNHINT64
!
TZDATE = TPDATE
CALL DATETIME_CORRECTDATE(TZDATE)
!
IYEAR_CUR  = int( TZDATE%nyear,  kind=MNHINT64 )
IMONTH_CUR = int( TZDATE%nmonth, kind=MNHINT64 )
IDAY_CUR   = int( TZDATE%nday,   kind=MNHINT64 )
ZSEC       = TZDATE%xtime
!
!Compute number of days since beginning of the year
IF ( ((MOD(IYEAR_CUR,4_MNHINT64)==0).AND.(MOD(IYEAR_CUR,100_MNHINT64)/=0)) .OR. (MOD(IYEAR_CUR,400_MNHINT64)==0)) ILEAPS=1
SELECT CASE(IMONTH_CUR)
  CASE(1)
    IDAYS = IDAY_CUR-1
  CASE(2)
    IDAYS = IDAY_CUR-1+31
  CASE(3)
    IDAYS = IDAY_CUR-1+31+28+ILEAPS
  CASE(4)
    IDAYS = IDAY_CUR-1+31+28+ILEAPS+31
  CASE(5)
    IDAYS = IDAY_CUR-1+31+28+ILEAPS+31+30
  CASE(6)
    IDAYS = IDAY_CUR-1+31+28+ILEAPS+31+30+31
  CASE(7)
    IDAYS = IDAY_CUR-1+31+28+ILEAPS+31+30+31+30
  CASE(8)
    IDAYS = IDAY_CUR-1+31+28+ILEAPS+31+30+31+30+31
  CASE(9)
    IDAYS = IDAY_CUR-1+31+28+ILEAPS+31+30+31+30+31+31
  CASE(10)
    IDAYS = IDAY_CUR-1+31+28+ILEAPS+31+30+31+30+31+31+30
  CASE(11)
    IDAYS = IDAY_CUR-1+31+28+ILEAPS+31+30+31+30+31+31+30+31
  CASE(12)
    IDAYS = IDAY_CUR-1+31+28+ILEAPS+31+30+31+30+31+31+30+31+30
END SELECT
!
IYEARS = IYEAR_CUR - int( TPREFERENCE_DATE%nyear, kind=MNHINT64 )
IF ( IYEARS < 0_MNHINT64 ) THEN
  CALL PRINT_MSG(NVERB_WARNING,'GEN','DATETIME_TIME2REFERENCE', &
                 'input year is smaller than reference year => result could be invalid')
END IF
!
!Compute number of years + number of leap days from reference date
ILEAPS = IYEARS/4            ! 1 leap year every 4 years
ILEAPS = ILEAPS-(IYEARS/100) ! multiple of 100 are not leap years
ILEAPS = ILEAPS+(IYEARS/400) ! multiple of 400 are leap years
!
!Compute number of days since reference date
IDAYS = IDAYS + 365*IYEARS + ILEAPS
!
KDAYS = IDAYS
PSEC  = ZSEC
!
END SUBROUTINE DATETIME_TIME2REFERENCE
!
!
SUBROUTINE DATETIME_DISTANCE(TPDATEBEG,TPDATEEND,PDIST)
!
!Compute distance (in seconds) between 2 dates
!
TYPE(DATE_TIME), INTENT(IN)  :: TPDATEBEG
TYPE(DATE_TIME), INTENT(IN)  :: TPDATEEND
REAL,            INTENT(OUT) :: PDIST
!
INTEGER :: IDAYSBEG, IDAYSEND
REAL    :: ZSECBEG, ZSECEND
!
CALL DATETIME_TIME2REFERENCE( TPDATEBEG, IDAYSBEG, ZSECBEG )
CALL DATETIME_TIME2REFERENCE( TPDATEEND, IDAYSEND, ZSECEND )
!
IF ( ZSECEND < ZSECBEG ) THEN
  !Add 1 day to ZSECEND and remove it from IDAYSEND
  ZSECEND = ZSECEND + REAL( 24 * 60 * 60 )
  IDAYSEND = IDAYSEND - 1
  IF ( ZSECEND < ZSECBEG ) CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'DATETIME_DISTANCE', 'unexpected: ZSECEND is too small' )
END IF
!
PDIST = REAL( ( IDAYSEND - IDAYSBEG ) * (24*60*60) ) + ZSECEND - ZSECBEG
!
END SUBROUTINE DATETIME_DISTANCE
!
PURE SUBROUTINE DATETIME_CORRECTDATE(TPDATE)
!
! Correct the date if not in the correct interval
! Change the date if time is <0 or >=86400 s
! or if day is not a valid day for the current month
! or if month<1 or >12
!
TYPE(DATE_TIME), INTENT(INOUT)  :: TPDATE
!
INTEGER :: IDAYS                           !Number of days to add
INTEGER :: IDAY_CUR, IMONTH_CUR, IYEAR_CUR !Currrent day, month and year
INTEGER :: IMONTH_LGT                      !Number of days in a month
LOGICAL :: GKO
REAL    :: ZSEC                            !Current time of the day (in seconds)
!
IYEAR_CUR  = TPDATE%nyear
IMONTH_CUR = TPDATE%nmonth
IDAY_CUR   = TPDATE%nday
ZSEC       = TPDATE%xtime
! print *,'DATETIME_CORRECTDATE in: ',IYEAR_CUR,IMONTH_CUR,IDAY_CUR,ZSEC
!
CALL DATETIME_GETMONTHLGT(IYEAR_CUR,IMONTH_CUR,IMONTH_LGT)
IF (TPDATE%xtime<0. .OR. TPDATE%xtime>=86400. .OR. &
    IDAY_CUR<1     .OR. IDAY_CUR>IMONTH_LGT .OR. &
    IMONTH_CUR<1   .OR. IMONTH_CUR>12            ) THEN
  GKO = .TRUE.
ELSE
  GKO = .FALSE.
END IF
!
IF (TPDATE%xtime<0.) THEN
  !Number of days to remove
  IDAYS = INT(TPDATE%xtime/86400.)-1
ELSE IF (TPDATE%xtime>=86400.) THEN
  !Number of days to add
  IDAYS = INT(TPDATE%xtime/86400.)
ELSE
  IDAYS = 0
END IF
!
!Correct time
ZSEC = ZSEC - IDAYS * 86400.
!
!Correct date
DO WHILE (GKO)
  IDAY_CUR = IDAY_CUR + IDAYS
  !
  !Check if year changed
  IF (IMONTH_CUR>12) THEN
    IYEAR_CUR = IYEAR_CUR + (IMONTH_CUR-1)/12
    IMONTH_CUR = MOD(IMONTH_CUR-1,12)+1
  ELSE IF (IMONTH_CUR<1) THEN
    IYEAR_CUR = IYEAR_CUR + IMONTH_CUR/12 - 1
    IMONTH_CUR = 12+MOD(IMONTH_CUR,12)
  END IF
  !
  CALL DATETIME_GETMONTHLGT(IYEAR_CUR,IMONTH_CUR,IMONTH_LGT)
  !
  !Check if month changed
  IF (IDAY_CUR<=IMONTH_LGT .AND. IDAY_CUR>=1) THEN
    IDAYS = 0
  ELSE IF (IDAY_CUR>IMONTH_LGT) THEN
    IMONTH_CUR = IMONTH_CUR + 1
    IDAYS = IDAY_CUR-IMONTH_LGT
    IDAY_CUR = 0
  ELSE !IDAY_CUR<1
    IMONTH_CUR = IMONTH_CUR - 1
    !Check if year changed
    IF (IMONTH_CUR<1) THEN
      IMONTH_CUR = 12
      IYEAR_CUR = IYEAR_CUR - 1
    END IF
    CALL DATETIME_GETMONTHLGT(IYEAR_CUR,IMONTH_CUR,IMONTH_LGT)
    IDAYS = IDAY_CUR
    IDAY_CUR = IMONTH_LGT
  END IF
  !
! print *,'DATETIME_CORRECTDATE du2:',IYEAR_CUR,IMONTH_CUR,IDAY_CUR,ZSEC,IDAYS
  IF (IDAYS==0      .AND.                            &
      IDAY_CUR>=1   .AND. IDAY_CUR<=IMONTH_LGT .AND. &
      IMONTH_CUR>=1 .AND. IMONTH_CUR<=12             ) GKO=.FALSE.
END DO
! print *,'DATETIME_CORRECTDATE out:',IYEAR_CUR,IMONTH_CUR,IDAY_CUR,ZSEC
!
TPDATE%nyear  = IYEAR_CUR
TPDATE%nmonth = IMONTH_CUR
TPDATE%nday   = IDAY_CUR
TPDATE%xtime  = ZSEC
!
END SUBROUTINE DATETIME_CORRECTDATE
!
!
PURE SUBROUTINE DATETIME_GETMONTHLGT(KYEAR,KMONTH,KLGT)
!
INTEGER, INTENT(IN)  :: KYEAR
INTEGER, INTENT(IN)  :: KMONTH
INTEGER, INTENT(OUT) :: KLGT
!
SELECT CASE(KMONTH)
  CASE(1,3,5,7,8,10,12)
    KLGT = 31
  CASE(2)
    IF ( ((MOD(KYEAR,4)==0).AND.(MOD(KYEAR,100)/=0)) .OR. (MOD(KYEAR,400)==0)) THEN
      KLGT = 29
    ELSE
      KLGT = 28
    END IF
  CASE(4,6,9,11)
    KLGT = 30
  CASE DEFAULT !Not an error (useful for DATETIME_CORRECTDATE)
    KLGT = 0
END SELECT
!
END SUBROUTINE DATETIME_GETMONTHLGT


FUNCTION DATETIME_LT(TPT1, TPT2) RESULT (OLT)
!
! TRUE if TPT1 .LT. TPT2
!
IMPLICIT NONE

TYPE(DATE_TIME), INTENT(IN) :: TPT1, TPT2

LOGICAL :: OLT

INTEGER :: IDAYS1, IDAYS2
REAL    :: ZSEC1, ZSEC2

#if 0
!Simpler but works only for correct dates (see DATETIME_CORRECTDATE)
IF ( TPT1%nyear .EQ. TPT2%nyear ) THEN
  IF ( TPT1%nmonth .EQ. TPT2%nmonth ) THEN
    IF ( TPT1%nday .EQ. TPT2%nday ) THEN
      OLT = TPT1%xtime .LT. TPT2%xtime
    ELSE
      OLT = TPT1%nday .LT. TPT2%nday
    END IF
  ELSE
   OLT = TPT1%nmonth .LT. TPT2%nmonth
  END IF
ELSE
  OLT = TPT1%nyear .LT. TPT2%nyear
ENDIF
#else
CALL DATETIME_TIME2REFERENCE( TPT1, IDAYS1, ZSEC1 )
CALL DATETIME_TIME2REFERENCE( TPT2, IDAYS2, ZSEC2 )

OLT = .FALSE.

IF ( IDAYS1 < IDAYS2 ) THEN
  OLT = .TRUE.
ELSE IF ( IDAYS1 == IDAYS2 ) THEN
  IF ( ZSEC1 < ZSEC2 ) OLT = .TRUE.
END IF
#endif

END FUNCTION DATETIME_LT


FUNCTION DATETIME_LE(TPT1, TPT2) RESULT (OLE)
!
! TRUE if TPT1 <= TPT2
!
IMPLICIT NONE

TYPE(DATE_TIME), INTENT(IN) :: TPT1, TPT2

LOGICAL :: OLE

INTEGER :: IDAYS1, IDAYS2
REAL    :: ZSEC1, ZSEC2

#if 0
!Simpler but works only for correct dates (see DATETIME_CORRECTDATE)
IF ( TPT1%nyear == TPT2%nyear ) THEN
  IF ( TPT1%nmonth == TPT2%nmonth ) THEN
    IF ( TPT1%nday == TPT2%nday ) THEN
      OLE = TPT1%xtime <= TPT2%xtime
    ELSE
      OLE = TPT1%nday <= TPT2%nday
    END IF
  ELSE
   OLE = TPT1%nmonth <= TPT2%nmonth
  END IF
ELSE
  OLE = TPT1%nyear <= TPT2%nyear
ENDIF
#else
CALL DATETIME_TIME2REFERENCE( TPT1, IDAYS1, ZSEC1 )
CALL DATETIME_TIME2REFERENCE( TPT2, IDAYS2, ZSEC2 )

OLE = .FALSE.

IF ( IDAYS1 < IDAYS2 ) THEN
  OLE = .TRUE.
ELSE IF ( IDAYS1 == IDAYS2 ) THEN
  IF ( ZSEC1 <= ZSEC2 ) OLE = .TRUE.
END IF
#endif
!
END FUNCTION DATETIME_LE


FUNCTION DATETIME_GE(TPT1, TPT2) RESULT (OGE)
!
! TRUE if TPT1 >=. TPT2
!
IMPLICIT NONE

TYPE(DATE_TIME), INTENT(IN) :: TPT1, TPT2

LOGICAL :: OGE

OGE = .NOT. DATETIME_LT( TPT1, TPT2 )

END FUNCTION DATETIME_GE


FUNCTION DATETIME_GT(TPT1, TPT2) RESULT (OGT)
!
! TRUE if TPT1 > TPT2
!
IMPLICIT NONE

TYPE(DATE_TIME), INTENT(IN) :: TPT1, TPT2

LOGICAL :: OGT

OGT = .NOT. DATETIME_LE( TPT1, TPT2 )

END FUNCTION DATETIME_GT


FUNCTION DATETIME_TIME_ADD( TPIN, PTIME ) RESULT ( TPOUT )

IMPLICIT NONE

TYPE(DATE_TIME), INTENT(IN) :: TPIN  ! Start date
REAL,            INTENT(IN) :: PTIME ! Added time
TYPE(DATE_TIME) :: TPOUT             ! End date = start date + added time

TPOUT = TPIN
TPOUT%XTIME = TPOUT%XTIME + PTIME

CALL DATETIME_CORRECTDATE( TPOUT )

END FUNCTION DATETIME_TIME_ADD


FUNCTION DATETIME_TIME_SUBSTRACT( TPT1, TPT2 ) RESULT( PDIST )
!
!Compute distance (in seconds) between 2 dates
!

IMPLICIT NONE

TYPE(DATE_TIME), INTENT(IN)  :: TPT1
TYPE(DATE_TIME), INTENT(IN)  :: TPT2
REAL                         :: PDIST

CALL DATETIME_DISTANCE( TPT2, TPT1, PDIST )

END FUNCTION  DATETIME_TIME_SUBSTRACT

END MODULE MODE_DATETIME
