!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 operators 2006/05/18 13:07:25
!-----------------------------------------------------------------
MODULE MODI_TEMPORAL_LT
INTERFACE
FUNCTION TEMPORAL_LT(TPT1, TPT2) RESULT (OLT)
USE MODD_TIME, ONLY: DATE_TIME
IMPLICIT NONE
LOGICAL :: OLT
TYPE(DATE_TIME), INTENT(IN) :: TPT1, TPT2
END FUNCTION TEMPORAL_LT
END INTERFACE
END MODULE MODI_TEMPORAL_LT
!
FUNCTION TEMPORAL_LT(TPT1, TPT2) RESULT (OLT)
USE MODD_TIME, ONLY: DATE_TIME
IMPLICIT NONE
LOGICAL :: OLT
TYPE(DATE_TIME), INTENT(IN) :: TPT1, TPT2
!
! TRUE if TPT1 .LT. TPT2
!
!
IF ( TPT1%TDATE%YEAR .EQ. TPT2%TDATE%YEAR ) THEN
  IF ( TPT1%TDATE%MONTH .EQ. TPT2%TDATE%MONTH ) THEN
    IF ( TPT1%TDATE%DAY .EQ. TPT2%TDATE%DAY ) THEN
      OLT = TPT1%TIME .LT. TPT2%TIME
    ELSE
      OLT = TPT1%TDATE%DAY .LT. TPT2%TDATE%DAY
    END IF
  ELSE
   OLT = TPT1%TDATE%MONTH .LT. TPT2%TDATE%MONTH
  END IF
ELSE
  OLT = TPT1%TDATE%YEAR .LT. TPT2%TDATE%YEAR
ENDIF
!
END FUNCTION TEMPORAL_LT
