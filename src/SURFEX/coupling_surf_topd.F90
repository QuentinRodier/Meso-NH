!###################################################################
SUBROUTINE COUPLING_SURF_TOPD (HPROGRAM,KI)
!###################################################################
!
!!****  *COUPLING_SURF_TOPD*  
!!
!!    PURPOSE
!!    -------
!!   
!!    Driver for the coupling between SURFEX and TOPODYN
!!      
!!    REFERENCE
!!    ---------
!!    *COUPLING_SURF_TRIP from B. Decharme
!!      
!!    AUTHOR
!!    ------
!!	B. Vincendon    
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/06/11 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODI_GET_LUOUT
USE MODI_COUPL_TOPD
USE MODI_ROUT_DATA_ISBA
USE MODI_BUDGET_COUPL_ROUT
USE MODI_WRITE_DISCHARGE_FILE
USE MODI_WRITE_BUDGET_COUPL_ROUT
USE MODI_PREP_RESTART_COUPL_TOPD
!
USE MODD_TOPODYN,       ONLY : XQTOT, NNB_TOPD_STEP, XQB_RUN, XQB_DR
USE MODD_COUPLING_TOPD, ONLY : LCOUPL_TOPD, LBUDGET_TOPD, NNB_TOPD, LTOPD_STEP, NTOPD_STEP, &
                                 NYEAR,NMONTH,NDAY,NH,NM
!
USE MODD_ISBA_n,          ONLY : CRUNOFF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN)         :: HPROGRAM ! program calling surf. schemes
INTEGER,          INTENT(IN)         :: KI       ! Surfex grid dimension
                                                 ! in a forcing iteration
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=3)              :: YSTEP    ! time stepsurf_tmp/off
INTEGER                       :: ILUOUT   ! unit of output listing file
INTEGER                       :: JJ       ! loop control
!
REAL, DIMENSION(KI)           :: ZDG_FULL
REAL, DIMENSION(KI)           :: ZWG2_FULL,ZWG3_FULL,ZDG2_FULL,ZDG3_FULL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('COUPLING_SURF_TOPD',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF ( .NOT.LCOUPL_TOPD ) THEN
  IF (LHOOK) CALL DR_HOOK('COUPLING_SURF_TOPD',1,ZHOOK_HANDLE)
  RETURN
ENDIF
  !
IF ( LTOPD_STEP ) THEN
  !
  ! * 1. Calling coupling or routing
  !
  IF (NTOPD_STEP<10) THEN
    WRITE(YSTEP,'(I1)') NTOPD_STEP
  ELSEIF (NTOPD_STEP < 100) THEN
    WRITE(YSTEP,'(I2)') NTOPD_STEP
  ELSE
    WRITE(YSTEP,'(I3)') NTOPD_STEP
  ENDIF
  !
  write(*,*) 'pas de temps coupl ',YSTEP
  !
  IF (CRUNOFF=='TOPD') THEN
    CALL COUPL_TOPD(HPROGRAM,YSTEP,KI,NTOPD_STEP)
  ELSE
    CALL ROUT_DATA_ISBA(HPROGRAM,KI,NTOPD_STEP)
  ENDIF
  !
  IF (LBUDGET_TOPD) CALL BUDGET_COUPL_ROUT(KI,NTOPD_STEP)
  !
ENDIF! (LCOUPL_TOPD.AND......
!
IF (LHOOK) CALL DR_HOOK('COUPLING_SURF_TOPD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE COUPLING_SURF_TOPD
