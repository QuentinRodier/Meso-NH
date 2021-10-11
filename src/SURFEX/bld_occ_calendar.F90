!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#####################################################################################
SUBROUTINE BLD_OCC_CALENDAR(HPROGRAM,KYEAR,KMONTH,KDAY,KTSUN,KDAYWBEG_SCHED,KHOURBEG_SCHED, &
     KPROBOCC, KBEG_HOLIDAY, KEND_HOLIDAY, KMOD_HOLIDAY, PBLDOCC, PISNIGHT)
!###################################################################################
!! **** BLD_OCC_CALENDAR *
!!
!!  PURPOSE
!!  -------
!!
!! AUTHOR
!! ------
!! C. de Munck     *Météo-France*
!! 
!! MODIFICATIONS
!! -------------
!! Original  02/2013
!
! Robert: 
! The probability of building occupation is now assigned according to schedules.
! At the moment no switch between winter and summer time is made.
! M. Goret 04/2017  change comment for number of the day of the week to be consistent with day_of_week changes.
!
USE MODI_DAY_OF_WEEK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!! 1. declaration of arguments
!
CHARACTER(LEN=6), INTENT(IN)    :: HPROGRAM     ! program calling surf. schemes  
INTEGER, INTENT(IN)             :: KYEAR        ! current year  (UTC)
INTEGER, INTENT(IN)             :: KMONTH       ! current month (UTC)
INTEGER, INTENT(IN)             :: KDAY         ! current day   (UTC)
REAL, DIMENSION(:), INTENT(IN)  :: KTSUN        ! current solar time  (s, UTC)
!
REAL, DIMENSION(:,:), INTENT(IN) :: KDAYWBEG_SCHED ! Start day of schedule
REAL, DIMENSION(:,:), INTENT(IN) :: KHOURBEG_SCHED ! Start hour of schedule
REAL, DIMENSION(:,:), INTENT(IN) :: KPROBOCC       ! Probability of building occupation
REAL, DIMENSION(:,:), INTENT(IN) :: KBEG_HOLIDAY   ! Julian day of beginning of holiday
REAL, DIMENSION(:,:), INTENT(IN) :: KEND_HOLIDAY   ! Julian day of end of holiday
REAL, DIMENSION(:)  , INTENT(IN) :: KMOD_HOLIDAY   ! Factor of modulation of building occuppation during holiday
!
REAL, DIMENSION(:), INTENT(OUT) :: PBLDOCC      ! Building occupation status
REAL, DIMENSION(:), INTENT(OUT) :: PISNIGHT     ! Flag for day and night
!
! Declaration of local variables
!
REAL, DIMENSION(13) :: ZBLDOCC_VEC   ! Building occupation status, shifted times
REAL, DIMENSION(13) :: ZISNIGHT_VEC  ! Flag for day and night, shifted times
REAL, DIMENSION(SIZE(KTSUN)) :: ZSHIFT_TSUN
!
INTEGER :: ILUOUT
INTEGER, DIMENSION(0:11) :: IBIS, INOBIS ! Cumulative number of days per month
                                         ! for bissextile and regular years
!
REAL :: ZJULIAN
INTEGER                      :: JDOW     ! day of week
INTEGER                      :: NDAY     ! Number of days in schedules
INTEGER                      :: NCRE     ! Number of time periods in schedules
INTEGER                      :: NPOS     ! Position in schedule vector
INTEGER                      :: JJ,LL,MM ! Loop counter
INTEGER                      :: TSHIFT   ! Loop counter
INTEGER                      :: NCOUNT   ! Counter
!
REAL(KIND=JPRB)              :: ZDRDOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('BLD_OCC_CALENDAR',0,ZDRDOOK_HANDLE)
!
!--------------------------------------------------------------------------------------
!  Determine the day of the week and the local time scheme in France
!--------------------------------------------------------------------------------------
!
! 1 = Monday
! 2 = Tuesday
! ....
! 7 = Sunday
!
CALL DAY_OF_WEEK(KYEAR,KMONTH,KDAY,JDOW)
!
! Determine the Julian day
!
INOBIS(:) = (/0,31,59,90,120,151,181,212,243,273,304,334/)
IBIS(0:1) = INOBIS(0:1)
!
DO JJ=2,11
  IBIS(JJ) = INOBIS(JJ)+1
END DO
!
IF( MOD(KYEAR,4).EQ.0 .AND. (MOD(KYEAR,100).NE.0 .OR. MOD(KYEAR,400).EQ.0)) THEN
   ZJULIAN = FLOAT(KDAY + IBIS(KMONTH-1))   - 1
ELSE
   ZJULIAN = FLOAT(KDAY + INOBIS(KMONTH-1)) - 1
END IF
!
!--------------------------------------------------------------------------------------
! Find the matching schedule (day of week and hour of day)
! At the moment the schedules for the vacation periods are neglected.
!--------------------------------------------------------------------------------------
!
NDAY=SIZE(KDAYWBEG_SCHED(1,:))
NCRE=SIZE(KHOURBEG_SCHED(1,:))/NDAY
!
ZBLDOCC_VEC (:) = -9999.0
ZISNIGHT_VEC(:) = -9999.0
!
DO JJ=1,SIZE(KTSUN)
   !
   ! Find the day of week
   !
   IF ((JDOW.GE.KDAYWBEG_SCHED(JJ,1)).AND.(JDOW.LT.KDAYWBEG_SCHED(JJ,2))) THEN
      LL=1
   ELSEIF (JDOW.EQ.KDAYWBEG_SCHED(JJ,2)) THEN
      LL=2
   ELSEIF (JDOW.EQ.KDAYWBEG_SCHED(JJ,3)) THEN
      LL=3
   ELSE
      CALL ABOR1_SFX("Day of week not found")
   ENDIF
   !
   ! Find the values for the ISNIGHT and PROBOCC for a set of 
   ! times shifted by +- 1 hour around the actual time
   ! the increment is 10 minutes.
   ! The day is not changed
   !
   NCOUNT = 1
   !
   DO TSHIFT=-6,+6,+1
      !
      ZSHIFT_TSUN(JJ) = KTSUN(JJ)+600.0*REAL(TSHIFT)
      !
      NPOS=-9999
      !
      DO MM=1,(NCRE-1)
         !
         IF ( (ZSHIFT_TSUN(JJ).GE.3600.0*KHOURBEG_SCHED(JJ,MM+NCRE*(LL-1))) .AND. &
              (ZSHIFT_TSUN(JJ).LT.3600.0*KHOURBEG_SCHED(JJ,MM+1+NCRE*(LL-1))) ) THEN
            !
            NPOS=MM+NCRE*(LL-1)
            ZISNIGHT_VEC(NCOUNT)=0.0
            EXIT
            !
         ENDIF
         !
         IF ( (ZSHIFT_TSUN(JJ).LT.3600.0*KHOURBEG_SCHED(JJ,1+NCRE*(LL-1))) .OR. &
              (ZSHIFT_TSUN(JJ).GE.3600.0*KHOURBEG_SCHED(JJ,NCRE+NCRE*(LL-1))) ) THEN
            !
            NPOS=NCRE+NCRE*(LL-1)
            ZISNIGHT_VEC(NCOUNT)=1.0
            EXIT
            !
         ENDIF
         !
      ENDDO
      !
      IF (NPOS.LT.1) THEN
         CALL GET_LUOUT(HPROGRAM,ILUOUT)
         WRITE(ILUOUT,*) " "
         WRITE(ILUOUT,*) "In bld_occ_calendar: position not found "
         WRITE(ILUOUT,*) "NPOS ",NPOS 
         CALL FLUSH(ILUOUT)
         CALL ABOR1_SFX ("BLD_OCC_CALENDAR:Position not found")
      ENDIF
      !
      ZBLDOCC_VEC(NCOUNT)=KPROBOCC(JJ,NPOS)
      NCOUNT = NCOUNT+1
      !
   ENDDO
   !
   ! The variables are averaged over the time window
   !
   PISNIGHT(JJ)=SUM(ZISNIGHT_VEC)/SIZE(ZISNIGHT_VEC)
   PBLDOCC (JJ)=SUM(ZBLDOCC_VEC )/SIZE(ZBLDOCC_VEC )
   !
   ! Modulation of building occupation during holidays
   !
   DO MM=1,SIZE(KBEG_HOLIDAY,2)
     !
     IF ((ZJULIAN.GE.KBEG_HOLIDAY(JJ,MM)).AND.(ZJULIAN.LE.KEND_HOLIDAY(JJ,MM))) THEN
        PBLDOCC (JJ) = PBLDOCC (JJ) * KMOD_HOLIDAY(JJ)
        EXIT
     ENDIF
     !
   ENDDO
!
ENDDO
!
!--------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('BLD_OCC_CALENDAR',1,ZDRDOOK_HANDLE)
!
END SUBROUTINE BLD_OCC_CALENDAR
