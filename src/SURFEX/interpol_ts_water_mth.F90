!     #########
      SUBROUTINE INTERPOL_TS_WATER_MTH(KYEAR,KMONTH,KDAY,PTS)
!     #######################################################
!
!!****  *INTERPOL_TS_WATER_MTH* - Interpolation of monthly TS water
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
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
!!	
!     B.Decharme  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    28/01/10
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_WATFLUX_n,  ONLY : XTS_MTH, CINTERPOL_TS
!
USE MODI_INTERPOL_QUADRA
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!------------------------
! 
INTEGER, INTENT(IN ) :: KYEAR  ! year of date
INTEGER, INTENT(IN ) :: KMONTH ! month of date
INTEGER, INTENT(IN ) :: KDAY   ! day of date
!
REAL, DIMENSION(:), INTENT(OUT) :: PTS   ! Water surface temperature at time t 
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
REAL    :: ZDAT,ZNDAT
INTEGER :: IMTH1,IMTH2,IMTH3
INTEGER :: INDAYS ! number of days in KMONTH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       1.    Number of days in a month
!              -------------------------
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_TS_WATER_MTH',0,ZHOOK_HANDLE)
SELECT CASE (KMONTH)
    CASE(4,6,9,11)
      INDAYS=30
    CASE(1,3,5,7:8,10,12)
      INDAYS=31
    CASE(2)
      IF( ((MOD(KYEAR,4)==0).AND.(MOD(KYEAR,100)/=0)) .OR. (MOD(KYEAR,400)==0))THEN
        INDAYS=29
      ELSE
        INDAYS=28
      ENDIF
END SELECT
!
!
!-------------------------------------------------------------------------------
!
!*       2.    TS water Interpolation using previous, current and next month
!              -------------------------------------------------------------
!
ZDAT = REAL(KDAY)
ZNDAT= REAL(INDAYS)
!
! The current month correspond to the indice 2 (or KMONTH+1 if ANNUAL)
!
IF(CINTERPOL_TS=='MONTH ')THEN
  CALL INTERPOL_QUADRA(ZDAT,ZNDAT,XTS_MTH(:,1),XTS_MTH(:,2),XTS_MTH(:,3),PTS)
ELSE
  IMTH1=KMONTH
  IMTH2=KMONTH+1
  IMTH3=KMONTH+2
  CALL INTERPOL_QUADRA(ZDAT,ZNDAT,XTS_MTH(:,IMTH1),XTS_MTH(:,IMTH2),XTS_MTH(:,IMTH3),PTS)
ENDIF
IF (LHOOK) CALL DR_HOOK('INTERPOL_TS_WATER_MTH',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INTERPOL_TS_WATER_MTH
