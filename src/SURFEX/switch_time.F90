!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE SWITCH_TIME (PLON, PSUNTIME, PDELTA_LEGAL_TIME,          &
                              KYEAR, KMONTH, KDAY,                        & 
                              CDTIME_REF, HPROGRAM, PSIMTIME, KSIZE, PTIME) 

!     ######################################################################
!!
!!***  *SWITCH_TIME extracted from CH_EMISSION_SNAP_n* - 
!!
!!    PURPOSE
!!    -------
!!      Change the reference time
!!
!!**  METHOD
!!    ------
!!
!!    AUTHOR
!!    ------
!!    S. Queguiner
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 10/2011
!!
!!    A. Alias     07/2016  gmkpack problem : name of the internal subroutine modified 
!!                          because exist already (view SURFEX/day_of_week.F90 )
!!    M. Goret     03/2017  extract this part of the routine from CH_EMISSION_SNAP_n
!!                          in order to reuse it in TEB.
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!
!
USE MODD_CSTS,        ONLY: XDAY
!
USE MODI_ADD_FORECAST_TO_DATE_SURF
USE MODI_SUBSTRACT_TO_DATE_SURF
USE MODI_ABOR1_SFX
USE MODI_GET_LUOUT
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
!

REAL, DIMENSION(KSIZE), INTENT(IN)         :: PLON              ! Longitude (deg, from Greenwich)
                                                                ! (must be between -180deg and 180deg)
REAL, DIMENSION(KSIZE), INTENT(IN)         :: PSUNTIME          ! Solar time (s since midnight)
REAL, DIMENSION(KSIZE), INTENT(IN)         :: PDELTA_LEGAL_TIME ! Difference (in hours)) between
!                                                               ! Legal time and UTC time
INTEGER, DIMENSION(KSIZE), INTENT(INOUT)   :: KYEAR,KMONTH,KDAY ! UTC year, month, day (in)
                                                                ! ref time year, month, day (out)
CHARACTER(LEN=5),       INTENT(IN)         :: CDTIME_REF        ! Reference time
!                                                               !  'UTC  ' : UTC   time
!                                                               !  'SOLAR' : SOLAR time
!                                                               !  'LEGAL' : LEGAL time
CHARACTER(LEN=6),       INTENT(IN)         :: HPROGRAM          ! Type of program
REAL,                   INTENT(IN)         :: PSIMTIME          ! time of simulation in sec UTC
                                                                ! (counting from midnight of
                                                                ! the current day)
INTEGER,                INTENT(IN)         :: KSIZE             ! number of points
REAL, DIMENSION(KSIZE), INTENT(OUT)        :: PTIME             ! ref time since midnight (s)
!
!
!*       0.2  declaration of local variables
!
REAL,   DIMENSION(KSIZE)  :: ZLON      ! Longitude centered in Greenwich meridian
INTEGER                   :: JI        ! Loop counter on points
INTEGER                   :: ILUOUT    ! logical unit of output file
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*    EXECUTABLE STATEMENTS
!     ---------------------
!
IF (LHOOK) CALL DR_HOOK('SWITCH_TIME',0,ZHOOK_HANDLE)
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!
SELECT CASE (CDTIME_REF)
  CASE ('UTC  ')
    PTIME(:)=PSIMTIME
  CASE ('SOLAR')
    ZLON(:)=PLON(:)
    WHERE(PLON(:)>  180.) ZLON(:)=PLON(:)-360.
    WHERE(PLON(:)<=-180.) ZLON(:)=PLON(:)+360.
    !*  retrieves solar date and time
    PTIME(:)=PSIMTIME + ZLON(:)*240. ! first guess is approximated solar time.
                                     ! The suntime should be close to this.
    DO JI=1,KSIZE
      IF (PTIME(JI)>PSUNTIME(JI)+XDAY/2.) THEN
        PTIME(JI) = PSUNTIME(JI) + XDAY
      ELSEIF (PTIME(JI)<PSUNTIME(JI)-XDAY/2.) THEN
        PTIME(JI) = PSUNTIME(JI) - XDAY
      ELSE
        PTIME(JI) = PSUNTIME(JI)
      END IF
      CALL ADD_FORECAST_TO_DATE_SURF(KYEAR(JI),KMONTH(JI),KDAY(JI),PTIME(JI))
      CALL SUBSTRACT_TO_DATE_SURF   (KYEAR(JI),KMONTH(JI),KDAY(JI),PTIME(JI))
    ENDDO
    
  CASE ('LEGAL')
    PTIME(:)=PSIMTIME + PDELTA_LEGAL_TIME(:) * 3600.
    DO JI=1,KSIZE
      CALL ADD_FORECAST_TO_DATE_SURF(KYEAR(JI),KMONTH(JI),KDAY(JI),PTIME(JI))
      CALL SUBSTRACT_TO_DATE_SURF   (KYEAR(JI),KMONTH(JI),KDAY(JI),PTIME(JI))
    ENDDO

  CASE DEFAULT
    WRITE(ILUOUT,*) CDTIME_REF, 'is an unexpected value for reference time'
    WRITE(ILUOUT,*) "Please choose one among UTC, LEGAL, SOLAR"
    CALL FLUSH(ILUOUT)
    CALL ABOR1_SFX ("SWITCH_TIME: Unknown ref time")

END SELECT

!
IF (LHOOK) CALL DR_HOOK('SWITCH_TIME',1,ZHOOK_HANDLE)
!
END SUBROUTINE SWITCH_TIME
