FUNCTION JULIAN (KYEAR, KMNTH, KMDAY) RESULT(KJULIAN)

!***********************************************************************
! Version "$Id: julian.F 45 2014-09-12 20:05:29Z coats $"
! EDSS/Models-3 I/O API.
! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
! (C) 2003-2010 by Baron Advanced Meteorological Systems.
! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
! See file "LGPL.txt" for conditions of use.
!.........................................................................
!  function body starts at line  68
!
!  FUNCTION:  returns the Julian day (1...365,366) corresponding to
!      the date MNTH-MDAY-YEAR.
!      NOTE:  This is NOT the Julian DATE -- only the
!      day-number.  To get the Julian date:
!
!      JDATE = 1000 * YEAR  +  JULIAN ( YEAR , MNTH , MDAY )
!
!  ARGUMENT LIST DESCRIPTION:
!
!    Input arguments:
!
!      YEAR     Calendar year
!      MNTH     Month of year  1, 12
!      MDAY     Day of month   1, 31
!
!     Output arguments:  none
!
!  RETURN VALUE:
!
!      JULIAN   The Julian DAY of the input arguments combined
!
!  REVISION HISTORY:
!
!    5/1988   Modified for ROMNET
!
!    8/1990   Modified for ROM 2.2 by Carlie J. Coats, Jr., CSC
!       improved comments; improved Zeller's Congruence algorithm
!       and using IF-THEN ... ELSE IF ... construction.
!
!    8/1999   Version for global-climate IO_360, which uses 360-day "year"
!       
!       2/2002 Unification by CJC with global-climate JULIAN
!
!       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
!***********************************************************************

IMPLICIT NONE

!...........   ARGUMENTS and their descriptions:

INTEGER, INTENT(IN) :: KYEAR            ! year YYYY
INTEGER, INTENT(IN) :: KMNTH            ! month 1...12
INTEGER, INTENT(IN) :: KMDAY            ! day-of-month 1...28,29,30,31

INTEGER :: KJULIAN 

!...........   SCRATCH LOCAL VARIABLES:

INTEGER :: JM, JN, JL

!***********************************************************************
!   begin body of function  JULIAN

#ifdef IO_360

KJULIAN = KMDAY + 30 * ( KMNTH - 1 )

#else

JM = MOD ((KMNTH + 9), 12)
JN = (JM * 153 + 2) / 5 + KMDAY + 58

IF  ( MOD(KYEAR,4).NE.0 ) THEN
  JL = 365
ELSE IF ( MOD(KYEAR,100).NE.0 ) THEN
  JL = 366
  JN = 1 + JN
ELSE IF ( MOD(KYEAR,400).NE.0 )  THEN
  JL = 365
ELSE 
  JL = 366
  JN = 1 + JN
END IF

KJULIAN = 1 + MOD(JN,JL)

#endif

END FUNCTION JULIAN

