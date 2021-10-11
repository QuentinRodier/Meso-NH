!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
FUNCTION LT_DATE(DATE1, DATE2)
!!
!!***  *LT_DATE* - 
!!
!!    PURPOSE
!!    -------
!!      Check if a date is lower than an other
!!
!!**  METHOD
!!    ------
!!
!!    AUTHOR
!!    ------
!!    M. Goret
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 03/2017
!!
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!
USE MODD_TYPE_DATE_SURF
!
  TYPE(DATE_TIME), INTENT(IN) :: DATE1, DATE2
  LOGICAL   :: LT_DATE !Boolean flush to true if date1 is lower than date2

  LT_DATE=.TRUE.

  !Check the year
  IF (DATE2%TDATE%YEAR == DATE1%TDATE%YEAR) THEN
      IF (DATE2%TDATE%MONTH == DATE1%TDATE%MONTH) THEN
         IF (DATE2%TDATE%DAY == DATE1%TDATE%DAY) THEN
            IF (DATE2%TIME <= DATE1%TIME) LT_DATE=.FALSE.
         ELSEIF (DATE2%TDATE%DAY < DATE1%TDATE%DAY) THEN
               LT_DATE=.FALSE.
         ENDIF
      ELSEIF (DATE2%TDATE%MONTH < DATE1%TDATE%MONTH) THEN
         LT_DATE=.FALSE.
      ENDIF
  ELSEIF (DATE2%TDATE%YEAR < DATE1%TDATE%YEAR) THEN
      LT_DATE=.FALSE.
  ENDIF

END FUNCTION LT_DATE
