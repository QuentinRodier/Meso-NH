!     #########
      SUBROUTINE IRRIGATION_UPDATE(PIRRIG, PTSTEP, KMONTH, KDAY,   &
       PTIME,TSEEDMONTH,TSEEDDAY,TREAPMONTH,TREAPDAY) 
!     ####################################################################
!
!!****  *IRRIGATION_UPDATE* - routine to update irrigation fields
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	P. Le Moigne  *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2006
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_AGRI,   ONLY   : JPSTAGE, XTHRESHOLD
USE MODD_AGRI_n, ONLY   : NIRRINUM, LIRRIDAY, XTHRESHOLDSPT, LIRRIGATE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
INTEGER, DIMENSION(:,:), INTENT(IN) :: TSEEDMONTH
INTEGER, DIMENSION(:,:), INTENT(IN) :: TSEEDDAY
INTEGER, DIMENSION(:,:), INTENT(IN) :: TREAPMONTH
INTEGER, DIMENSION(:,:), INTENT(IN) :: TREAPDAY
REAL   , DIMENSION(:,:), INTENT(IN) :: PIRRIG
REAL,    INTENT(IN)  :: PTSTEP, PTIME
INTEGER, INTENT(IN)  :: KMONTH, KDAY
INTEGER              :: IL, JL                        
LOGICAL              :: GMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.1   Declarations of arguments
!-------------------------------------------------------------------------------
!
! Mask to realize update only once a day
!
IF (LHOOK) CALL DR_HOOK('MODI_IRRIGATION_UPDATE:IRRIGATION_UPDATE',0,ZHOOK_HANDLE)
GMASK = ( PTIME - PTSTEP < 0. ) .AND. ( PTIME >= 0. )
!
IF (GMASK) THEN
!
   WHERE( (PIRRIG(:,:).GT.0.).AND.(LIRRIDAY(:,:)) .AND.(NIRRINUM(:,:).LT.JPSTAGE))
      NIRRINUM (:,:) = NIRRINUM(:,:) + 1
      LIRRIDAY (:,:) = .FALSE.
   ENDWHERE
!   
   DO IL=1,SIZE(PIRRIG,1)
       DO JL=1,SIZE(PIRRIG,2)
           XTHRESHOLDSPT(IL,JL)=XTHRESHOLD(NIRRINUM(IL,JL))
       ENDDO
   ENDDO
!
END IF
!
! Reinitialization of irrigation stage (necessary for runs from August to August)
!
IF((KMONTH==1).AND.(KDAY==1)) THEN
   NIRRINUM(:,:) = 1
ENDIF
!
LIRRIGATE(:,:) = .FALSE.
DO IL=1,SIZE(PIRRIG,1)
   DO JL=1,SIZE(PIRRIG,2)
      !
      ! Activate irrigation after seeding date
      !
      IF (KMONTH == TSEEDMONTH(IL,JL) .AND. KDAY .GE. TSEEDDAY(IL,JL)) THEN
         LIRRIGATE(IL,JL) = .TRUE.
      END IF
      IF (KMONTH > TSEEDMONTH(IL,JL)) THEN
         LIRRIGATE(IL,JL) = .TRUE.
      END IF
      !
      ! Stop irrigation after reaping date
      !
      IF (KMONTH == TREAPMONTH(IL,JL) .AND. KDAY .GT. TREAPDAY(IL,JL)) THEN
         LIRRIGATE(IL,JL) = .FALSE.
      END IF
      IF (KMONTH > TREAPMONTH(IL,JL)) THEN
         LIRRIGATE(IL,JL) = .FALSE.
      END IF
   ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_IRRIGATION_UPDATE:IRRIGATION_UPDATE',1,ZHOOK_HANDLE)
!
END SUBROUTINE IRRIGATION_UPDATE
