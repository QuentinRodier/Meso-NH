!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE TRAFFIC_FLUX_MODULATION ( TOP, PSUNTIME, PDELTA_LEGAL_TIME, KDELTA_LEGAL_TIME, &
     PTIME_OF_CHANGE, LD_TIME_OF_CHANGE, PLON, HPROGRAM, PMODULATED_VALUE_TRAFFIC,        &
     PMODULATED_VALUE_POPULATION )
!     ######################################################################
!!
!!***  *TRAFFIC_FLUX_MODULATION* - 
!!
!!    PURPOSE
!!    -------
!!      modulate the mean value of an emission according to the month, day of week and hour
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
!!    Robert   11/2020 : Add similar modulation of population density
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!
USE MODD_SURF_PAR,     ONLY : XUNDEF
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TYPE_DATE_SURF
!
USE MODI_ADD_FORECAST_TO_DATE_SURF
USE MODI_DAY_OF_WEEK
USE MODI_LT_DATE
USE MODI_SUBSTRACT_TO_DATE_SURF
USE MODI_SWITCH_TIME
!------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*       0.1  declaration of arguments
!
TYPE(TEB_OPTIONS_t),             INTENT(IN)   :: TOP
REAL,            DIMENSION(:),   INTENT(IN)   :: PSUNTIME          ! Solar time (s since midnight)
REAL,            DIMENSION(:,:), INTENT(IN)   :: PDELTA_LEGAL_TIME ! Difference (in hours)) between
!                                                                  ! Legal time and UTC time
INTEGER,                         INTENT(INOUT):: KDELTA_LEGAL_TIME ! Current indice of PDELTA_LEGAL_TIME
TYPE(DATE_TIME), DIMENSION(:),   INTENT(IN)   :: PTIME_OF_CHANGE   !Time UTC at which a change of legal time occurs
LOGICAL,                         INTENT(IN)   :: LD_TIME_OF_CHANGE !Logical for presence of TIME_OF_CHANGE in the namelist
REAL,            DIMENSION(:),   INTENT(IN)   :: PLON              ! Longitude (deg, from Greenwich)
                                                                   ! (must be between -180deg and 180deg)
CHARACTER(LEN=6),                INTENT(IN)   :: HPROGRAM          ! Type of program
REAL, DIMENSION(:), INTENT(OUT) :: PMODULATED_VALUE_TRAFFIC    ! modulation of traffic as a function of month, day and hour
REAL, DIMENSION(:), INTENT(OUT) :: PMODULATED_VALUE_POPULATION ! modulation of population as a function of month, day and hour
!
!*       0.2  declaration of local variables
!
INTEGER :: ISIZE !number of points
INTEGER :: JI !loop compter
!
INTEGER, DIMENSION(SIZE(PLON)) :: IYEAR, IMONTH, IDAY, IHOUR, IDOW ! year, month, day, day of week in
                                                                   ! UTC time before call to switch_time
                                                                   ! ref time = solar or legal after call to switch_time
REAL, DIMENSION(SIZE(PLON)) :: ZTIME                  ! time (s) in ref time=solar or legal
REAL, DIMENSION(SIZE(PLON)) :: ZFST_VALUE_TRAFFIC, ZSCD_VALUE_TRAFFIC ! Values to be used for the interpolation
REAL, DIMENSION(SIZE(PLON)) :: ZFST_VALUE_POP, ZSCD_VALUE_POP ! Values to be used for the interpolation  
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*    EXECUTABLE STATEMENTS
!     ---------------------
!
IF (LHOOK) CALL DR_HOOK('TRAFFIC_FLUX_MODULATION',0,ZHOOK_HANDLE)
!
!*         1. Initialisation
!
ISIZE           = SIZE(PLON)
IMONTH (:)      = TOP%TTIME%TDATE%MONTH
IYEAR  (:)      = TOP%TTIME%TDATE%YEAR
IDAY   (:)      = TOP%TTIME%TDATE%DAY
!
!*        2.1 switch time reference
!       ---------------------------
!
IF (ANY(PDELTA_LEGAL_TIME(:,:)/= XUNDEF)) THEN
   IF (LD_TIME_OF_CHANGE) THEN ! There is a change of the legal hour during the simulation
      DO WHILE (TIME_HAVE_TO_BE_CHANGE(KDELTA_LEGAL_TIME, PTIME_OF_CHANGE, TOP%TTIME)) !check if legal time has to be change
         !
         KDELTA_LEGAL_TIME=KDELTA_LEGAL_TIME+1 !change of the legal time
         !
      END DO
   ENDIF
   CALL SWITCH_TIME(PLON, PSUNTIME, PDELTA_LEGAL_TIME(:,KDELTA_LEGAL_TIME),&
          IYEAR(:), IMONTH(:), IDAY(:),                          & 
          'LEGAL', HPROGRAM, TOP%TTIME%TIME, ISIZE, ZTIME          ) 
ELSE 
   CALL SWITCH_TIME(PLON, PSUNTIME, PDELTA_LEGAL_TIME(:,KDELTA_LEGAL_TIME),&
          IYEAR(:), IMONTH(:), IDAY(:),                          & 
          'SOLAR', HPROGRAM, TOP%TTIME%TIME, ISIZE, ZTIME          ) 
ENDIF
!
!*        2.2 find the value for the interpolation
!       ------------------------------------------
!
CALL DAY_OF_WEEK(IYEAR(:), IMONTH(:), IDAY(:), IDOW(:))
IHOUR(:)=INT(ZTIME(:)/3600.)
!
ZFST_VALUE_TRAFFIC(:) = &
    TOP%XTRAF_MONTHLY(IMONTH(:))*TOP%XTRAF_DAILY(IDOW(:))*TOP%XTRAF_HOURLY(IHOUR(:)+1) !+1 is added as array indexation start at one
  ZFST_VALUE_POP(:) = &
    TOP%XPOP_MONTHLY(IMONTH(:))*TOP%XPOP_DAILY(IDOW(:))*TOP%XPOP_HOURLY(IHOUR(:)+1) !+1 is added as array indexation start at one
!
ZTIME(:)=ZTIME(:) + 3600.

DO JI=1, ISIZE
  CALL ADD_FORECAST_TO_DATE_SURF(IYEAR(JI),IMONTH(JI),IDAY(JI),ZTIME(JI))
ENDDO
!
CALL DAY_OF_WEEK(IYEAR(:), IMONTH(:), IDAY(:), IDOW(:))
IHOUR(:)=INT(ZTIME(:)/3600.)
!
ZSCD_VALUE_TRAFFIC(:) = &
    TOP%XTRAF_MONTHLY(IMONTH(:))*TOP%XTRAF_DAILY(IDOW(:))*TOP%XTRAF_HOURLY(IHOUR(:)+1)!+1 is added as array indexation start at one
ZSCD_VALUE_POP(:) = &
    TOP%XPOP_MONTHLY(IMONTH(:))*TOP%XPOP_DAILY(IDOW(:))*TOP%XPOP_HOURLY(IHOUR(:)+1)!+1 is added as array indexation start at one
!
!*        2.3  Linear temporal interpolation between the two values
!       ------------------------------------------------------------
!
PMODULATED_VALUE_TRAFFIC(:) = ZFST_VALUE_TRAFFIC(:) + &
    (ZSCD_VALUE_TRAFFIC(:)-ZFST_VALUE_TRAFFIC(:))/3600.*(ZTIME(:)-IHOUR(:)*3600.)
PMODULATED_VALUE_POPULATION(:) = ZFST_VALUE_POP(:) + &
    (ZSCD_VALUE_POP(:)-ZFST_VALUE_POP(:))/3600.*(ZTIME(:)-IHOUR(:)*3600.)  
!
IF (LHOOK) CALL DR_HOOK('TRAFFIC_FLUX_MODULATION',1,ZHOOK_HANDLE)
!
CONTAINS
FUNCTION TIME_HAVE_TO_BE_CHANGE(KDELTA_LEGAL_TIME, PTIME_OF_CHANGE, TTIME)
   USE MODI_LT_DATE
   IMPLICIT NONE
   INTEGER,                         INTENT(IN):: KDELTA_LEGAL_TIME ! Current indice of PDELTA_LEGAL_TIME
   TYPE(DATE_TIME), DIMENSION(:),   INTENT(IN):: PTIME_OF_CHANGE   !Time UTC at which a change of legal time occurs
   TYPE(DATE_TIME),                 INTENT(IN):: TTIME             !current time UTC of the simulation
   LOGICAL                                    :: TIME_HAVE_TO_BE_CHANGE !T if a change in legal time have to be done, F otherwise
   INTEGER                                    :: KSIZE_TIME_OF_CHANGE !Size of PTIME_OF_CHANGE
   REAL(KIND=JPRB)                            :: ZHOOK_HANDLE
   !
   IF (LHOOK) CALL DR_HOOK('TRAFFIC_FLUX_MODULATION:TIME_HAVE_TO_BE_CHANGE',0,ZHOOK_HANDLE)
    !
    KSIZE_TIME_OF_CHANGE=SIZE(PTIME_OF_CHANGE)
    !
    IF (KDELTA_LEGAL_TIME <= KSIZE_TIME_OF_CHANGE) THEN
       !the end of the list of date of change of legal time has not been reach
       !change of legal time have to be done if next time of change is lower than current time of the simulation
       TIME_HAVE_TO_BE_CHANGE=LT_DATE(PTIME_OF_CHANGE(KDELTA_LEGAL_TIME),TTIME)
    ELSE
       !the end of the list of date of change of legal time has been reach
       !no more change of legal time have to be done
       TIME_HAVE_TO_BE_CHANGE=.FALSE.
    ENDIF
    !
   IF (LHOOK) CALL DR_HOOK('TRAFFIC_FLUX_MODULATION:TIME_HAVE_TO_BE_CHANGE',1,ZHOOK_HANDLE)
   !
END FUNCTION TIME_HAVE_TO_BE_CHANGE


END SUBROUTINE TRAFFIC_FLUX_MODULATION
