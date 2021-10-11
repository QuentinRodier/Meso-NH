!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODI_DAY_OF_WEEK
INTERFACE DAY_OF_WEEK

  SUBROUTINE DAY_OF_WEEK_SC(PYEAR1, PMONTH1, PDAY1, PDOW1)
INTEGER,INTENT(IN) :: PYEAR1 !current year (UTC)
INTEGER,INTENT(IN) :: PMONTH1!current month (UTC)
INTEGER,INTENT(IN) :: PDAY1  !current day (UTC)
INTEGER,INTENT(OUT):: PDOW1  !current day of the week
  END SUBROUTINE DAY_OF_WEEK_SC

  SUBROUTINE DAY_OF_WEEK_VECT(PYEAR2, PMONTH2, PDAY2, PDOW2)
INTEGER, DIMENSION(:),INTENT(IN) :: PYEAR2 !current year (UTC)
INTEGER, DIMENSION(:),INTENT(IN) :: PMONTH2!current month (UTC)
INTEGER, DIMENSION(:),INTENT(IN) :: PDAY2  !current day (UTC)
INTEGER, DIMENSION(:),INTENT(OUT):: PDOW2  !current day of the week
  END SUBROUTINE DAY_OF_WEEK_VECT
END INTERFACE

END MODULE MODI_DAY_OF_WEEK

!################################################
SUBROUTINE DAY_OF_WEEK_VECT(PYEAR, PMONTH, PDAY, PDOW)
!################################################
!!    AUTHOR
!!    ------
!!    J.Arteta 
!!    Original   August 2010
!!
!!
!!    MODifICATIONS
!!    -------------
!!    S. Queguiner 10/2011  DAY:Monday->Sunday => DOW:1->7
!!    A. Alias     07/2016  gmkpack problem : name of the internal subroutine modified 
!!                          because exist already (view SURFEX/day_of_week.F90 )
!!    M. Goret     03/2017  move this code from a local subroutine of ch_emission_snapn to this module
!!                          change dummy argument order to stick to day_of_week_sc order.
!!
!!          
!!          

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!! DECLARATION
IMPLICIT NONE
!! 1. declaraction of arguments
INTEGER, DIMENSION(:),INTENT(IN) :: PYEAR !current year (UTC)
INTEGER, DIMENSION(:),INTENT(IN) :: PMONTH!current month (UTC)
INTEGER, DIMENSION(:),INTENT(IN) :: PDAY  !current day (UTC)
INTEGER, DIMENSION(:),INTENT(OUT):: PDOW  !current day of the week
!!
!! 2. declaration of local variables
INTEGER, DIMENSION(SIZE(PDOW))  :: DAY, YR, MN, N1, N2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('DAY_OF_WEEK',0,ZHOOK_HANDLE)
!
YR = PYEAR
MN = PMONTH
   
!   IF JANUARY OR FEBRUARY, ADJUST MONTH AND YEAR
   
WHERE(MN .LE. 2)
   MN = MN + 12
   YR = YR - 1
END WHERE
N1 = (26 * (MN + 1)) / 10
N2 = (125 * YR) / 100
DAY = PDAY + N1 + N2 - (YR / 100) + (YR / 400) - 1
PDOW = MOD(DAY, 7) + 7
WHERE (PDOW .GT. 7) PDOW= PDOW -7
!
IF (LHOOK) CALL DR_HOOK('DAY_OF_WEEK',1,ZHOOK_HANDLE)

!
END SUBROUTINE DAY_OF_WEEK_VECT


SUBROUTINE DAY_OF_WEEK_SC(PYEAR, PMONTH, PDAY, PDOW)
!!
!!  PURPOSE
!!  -------
!! DAY_OF_WEEK COMPUTES THE DAY OF THE WEEK BASED UPON THE GIVEN DATE,
!! MONTH AND YEAR.  IT USES THE ZELLER CONGRUENCE ALGORITHIM.
!! PDAY IS THE DAY OF THE MONTH, 1 - 31
!! PMONTH IS THE MONTH OF THE YEAR, 1 - 12
!! PYEAR IS THE YEAR, E.G., 1977
!! IT RETURNS 1 FOR SUNDAY, 2 FOR MONDAY, ETC. => this as been change since
!! the introduction of the interface day_of_week 1=monday, 2=tuesday ...
!!
!! AUTHOR
!! ------
!! G. Pigeon *Météo-France*
!! 
!! MODIFICATIONS
!! -------------
!! Original  02/2010
!! M. Goret  03/2017 move the original subroutine to this interface, change in day definition
!!                   to stick to day_of_week_sc convention : 1->7= monday-> sunday

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!! DECLARATION
IMPLICIT NONE
!! 1. declaraction of arguments
INTEGER,INTENT(IN) :: PYEAR !current year (UTC)
INTEGER,INTENT(IN) :: PMONTH!current month (UTC)
INTEGER,INTENT(IN) :: PDAY  !current day (UTC)
INTEGER,INTENT(OUT):: PDOW  !current day of the week
!!
!! 2. declaration of local variables
INTEGER :: DAY, YR, MN, N1, N2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('DAY_OF_WEEK',0,ZHOOK_HANDLE)
!
YR = PYEAR
MN = PMONTH
   
!   IF JANUARY OR FEBRUARY, ADJUST MONTH AND YEAR
   
IF (MN .LE. 2) THEN
   MN = MN + 12
   YR = YR - 1
END IF
N1 = (26 * (MN + 1)) / 10
N2 = (125 * YR) / 100
DAY = PDAY + N1 + N2 - (YR / 100) + (YR / 400) - 1
PDOW = MOD(DAY, 7) + 7
IF (PDOW .GT. 7) PDOW= PDOW -7
!
IF (LHOOK) CALL DR_HOOK('DAY_OF_WEEK',1,ZHOOK_HANDLE)


  END SUBROUTINE DAY_OF_WEEK_SC


