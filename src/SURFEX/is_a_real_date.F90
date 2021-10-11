!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODI_IS_A_REAL_DATE
!!
!!  PURPOSE
!!  -------
!! IS_A_REAL_DATE check if a date truly exists. If it does, True is return.
!!
!! AUTHOR
!! ------
!! M. Goret *Météo-France*
!! 
!! MODIFICATIONS
!! -------------
!!   Original   04/2017
!
!
INTERFACE IS_A_REAL_DATE
!
  MODULE PROCEDURE IS_A_REAL_DATE_SC        ! Scalar version for date without time
  MODULE PROCEDURE IS_A_REAL_DATE_VECT      ! Vectorial version for date without time
  MODULE PROCEDURE IS_A_REAL_DATE_TIME_SC   ! Scalar version for date with time
  MODULE PROCEDURE IS_A_REAL_DATE_TIME_VECT ! Vectorial version for date with time

!
!
!
END INTERFACE
!
CONTAINS
!---------------------------------------------------------------------------
!
!
!---------------------------------------------
  FUNCTION IS_A_REAL_DATE_SC(TDATE) 
!---------------------------------------------
!
USE MODD_TYPE_DATE_SURF
USE MODD_CSTS, ONLY : NB_MONTH, NMONTHDAY_NON, NMONTHDAY_BIS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!  0.1 Declaration of arguments
!  ----------------------------
TYPE(DATE), INTENT(IN)  :: TDATE             !The date that have to be tested
LOGICAL                 :: IS_A_REAL_DATE_SC !True if the date exists
!
!  0.2 Declaration of local variales
!  ----------------------------
LOGICAL         :: LL_BIS        ! True if the year is bissextile
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('IS_A_REAL_DATE_SC',0,ZHOOK_HANDLE)
!
!  1. Check the month
!  ------------------
!
IF (TDATE%MONTH>0 .AND. TDATE%MONTH<=NB_MONTH) THEN
   !
   !  2. Check the day
   !  -----------------
   LL_BIS =( MOD(TDATE%YEAR,4).EQ.0 .AND. (MOD(TDATE%YEAR,100).NE.0 .OR. MOD(TDATE%YEAR,400).EQ.0))

   IF (LL_BIS) THEN
     IS_A_REAL_DATE_SC= TDATE%DAY>0 .AND. TDATE%DAY<=NMONTHDAY_BIS(TDATE%MONTH)
   ELSE
     IS_A_REAL_DATE_SC= TDATE%DAY>0 .AND. TDATE%DAY<=NMONTHDAY_NON(TDATE%MONTH)
   ENDIF

ELSE
   IS_A_REAL_DATE_SC=.FALSE.
ENDIF
!             
!
IF (LHOOK) CALL DR_HOOK('IS_A_REAL_DATE_SC',1,ZHOOK_HANDLE)
!
END FUNCTION IS_A_REAL_DATE_SC
!
!
!---------------------------------------------
  FUNCTION IS_A_REAL_DATE_VECT(TDATE) 
!---------------------------------------------
!
USE MODD_TYPE_DATE_SURF
USE MODD_CSTS, ONLY : NB_MONTH, NMONTHDAY_NON, NMONTHDAY_BIS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!  0.1 Declaration of arguments
!  ----------------------------
TYPE(DATE), INTENT(IN), DIMENSION(:)            :: TDATE               !Dates that have to be tested
LOGICAL,                DIMENSION(SIZE(TDATE))  :: IS_A_REAL_DATE_VECT !True if the date exists
!
!  0.2 Declaration of local variales
!  ----------------------------
LOGICAL, DIMENSION(SIZE(TDATE)) :: LL_BIS        ! True if the year is bissextile
LOGICAL, DIMENSION(SIZE(TDATE)) :: LL_MONTH_OK   ! True is the number coding for the month is ok
REAL(KIND=JPRB)                 :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('IS_A_REAL_DATE_VECT',0,ZHOOK_HANDLE)
!
!  1. Check the month
!  ------------------
!
LL_MONTH_OK = TDATE%MONTH>0 .AND. TDATE%MONTH<=NB_MONTH
!LL_BIS=.FALSE.
!WRITE(*,*) LL_BIS
!call flush()
!
!  2. Check the day
!  -----------------
WHERE (LL_MONTH_OK)
   LL_BIS =( MOD(TDATE%YEAR,4).EQ.0 .AND. (MOD(TDATE%YEAR,100).NE.0 .OR. MOD(TDATE%YEAR,400).EQ.0))

   WHERE (LL_BIS)
     IS_A_REAL_DATE_VECT= TDATE%DAY>0 .AND. TDATE%DAY<=NMONTHDAY_BIS(TDATE%MONTH)
   ELSEWHERE
     IS_A_REAL_DATE_VECT= TDATE%DAY>0 .AND. TDATE%DAY<=NMONTHDAY_NON(TDATE%MONTH)
   END WHERE

ELSEWHERE
  IS_A_REAL_DATE_VECT=.FALSE.
END WHERE
!
!WRITE(*,*) LL_BIS
!call flush()
!             
!
IF (LHOOK) CALL DR_HOOK('IS_A_REAL_DATE_VECT',1,ZHOOK_HANDLE)
!
END FUNCTION IS_A_REAL_DATE_VECT
!
!
!---------------------------------------------
  FUNCTION IS_A_REAL_DATE_TIME_SC(TDATE_TIME) 
!---------------------------------------------
!
USE MODD_TYPE_DATE_SURF
USE MODD_CSTS, ONLY : NDAYSEC
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!  0.1 Declaration of arguments
!  ----------------------------
TYPE(DATE_TIME), INTENT(IN)  :: TDATE_TIME             !The date that have to be tested
LOGICAL                      :: IS_A_REAL_DATE_TIME_SC !True if the date exists
!
!  0.2 Declaration of local variales
!  ----------------------------
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!
IF (LHOOK) CALL DR_HOOK('IS_A_REAL_DATE_TIME_SC',0,ZHOOK_HANDLE)
!
!
IS_A_REAL_DATE_TIME_SC=(IS_A_REAL_DATE_SC(TDATE_TIME%TDATE) .AND. TDATE_TIME%TIME>=0 &
                                                            .AND. TDATE_TIME%TIME<NDAYSEC)                
!
IF (LHOOK) CALL DR_HOOK('IS_A_REAL_DATE_TIME_SC',1,ZHOOK_HANDLE)
!
END FUNCTION IS_A_REAL_DATE_TIME_SC
!
!
!
!---------------------------------------------
  FUNCTION IS_A_REAL_DATE_TIME_VECT(TDATE_TIME) 
!---------------------------------------------
!
USE MODD_TYPE_DATE_SURF
USE MODD_CSTS, ONLY : NDAYSEC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!  0.1 Declaration of arguments
!  ----------------------------
TYPE(DATE_TIME), INTENT(IN), DIMENSION(:)                :: TDATE_TIME               !Dates that have to be tested
LOGICAL,                     DIMENSION(SIZE(TDATE_TIME)) :: IS_A_REAL_DATE_TIME_VECT !True if the date exists
!
!  0.2 Declaration of local variales
!  ----------------------------
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!
IF (LHOOK) CALL DR_HOOK('IS_A_REAL_DATE_TIME_VECT',0,ZHOOK_HANDLE)
!
!
IS_A_REAL_DATE_TIME_VECT=(IS_A_REAL_DATE_VECT(TDATE_TIME%TDATE) .AND. TDATE_TIME%TIME>=0 &
                                                            .AND. TDATE_TIME%TIME<NDAYSEC)                
!
IF (LHOOK) CALL DR_HOOK('IS_A_REAL_DATE_TIME_VECT',1,ZHOOK_HANDLE)
!
END FUNCTION IS_A_REAL_DATE_TIME_VECT

END MODULE MODI_IS_A_REAL_DATE

