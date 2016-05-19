!
!     ##########################
      SUBROUTINE WRITE_BUDGET_COUPL_ROUT
!     ##########################
!
!!
!!    PURPOSE
!!    -------
!        
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
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
!!      L. Bouilloud & B. Vincendon	* Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original  03/2008 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_TOPODYN,       ONLY : CCAT, NNCAT, NNB_TOPD_STEP
USE MODD_BUDGET_COUPL_ROUT
!
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!      none
!*      0.2    declarations of local variables
INTEGER           :: JCAT,JSTP ! loop control
INTEGER           :: IUNIT     ! file unit numbers
 CHARACTER(LEN=28) :: YFILE     ! file name
 CHARACTER(LEN=40) :: YFORM     ! Writing format
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITE_BUDGET_COUPL_ROUT',0,ZHOOK_HANDLE)
!
!*       1.     WRITING BUDGET FILES
!               ------------
!
DO JCAT=1,NNCAT
  !
  YFORM='(i6,10f15.1)'
  YFILE=TRIM('bilan_bv_')//TRIM(CCAT(JCAT))//TRIM('.txt')
  !
  CALL OPEN_FILE('ASCII ',IUNIT,HFILE=YFILE,HFORM='FORMATTED',HACTION='WRITE')
  !
  WRITE(IUNIT,*) '     T','         ',YB_VAR(1),'         ',YB_VAR(2),'         ',&
                                      YB_VAR(3),'         ',YB_VAR(4),'         ',&
                                      YB_VAR(5),'         ',YB_VAR(6),'         ',&
                                      YB_VAR(7),'         ',YB_VAR(8),'         ',&
                                      YB_VAR(9),'         ',YB_VAR(10)
  !
  DO JSTP=1,NNB_TOPD_STEP
    WRITE(IUNIT,YFORM) JSTP,XB_VAR_BV(JSTP,JCAT,1:10)
  ENDDO
  !
  CALL CLOSE_FILE('ASCII ',IUNIT)
  ! 
  YFILE=TRIM('bilan_nobv_')//TRIM(CCAT(JCAT))//TRIM('.txt')
  !
  CALL OPEN_FILE('ASCII ',IUNIT,HFILE=YFILE,HFORM='FORMATTED',HACTION='WRITE')
  ! 
  WRITE(IUNIT,*) '     T','         ',YB_VAR(1),'         ',YB_VAR(2),'         ',&
                                      YB_VAR(3),'         ',YB_VAR(4),'         ',&
                                      YB_VAR(5),'         ',YB_VAR(6),'         ',&
                                      YB_VAR(7),'         ',YB_VAR(8),'         ',&
                                      YB_VAR(9),'         ',YB_VAR(10)
  !
  DO JSTP=1,NNB_TOPD_STEP
    WRITE(IUNIT,YFORM) JSTP,XB_VAR_NOBV(JSTP,JCAT,1:10)
  ENDDO
  !
  CALL CLOSE_FILE('ASCII ',IUNIT)
  !
  YFORM='(i6,5f15.1)'
  YFILE=TRIM('bilan_q.txt')
  !
  CALL OPEN_FILE('ASCII ',IUNIT,HFILE=YFILE,HFORM='FORMATTED',HACTION='WRITE')
  !
  WRITE(IUNIT,*) '     T','         ',YB_VARQ(1),'         ',YB_VARQ(2),'         ',&
                                      YB_VARQ(3),'         ',YB_VARQ(4),'         ',&
                                      YB_VARQ(5),'         '
  ! 
  DO JSTP=1,NNB_TOPD_STEP
    WRITE(IUNIT,YFORM) JSTP,XB_VAR_Q(JSTP,JCAT,1:5)
  ENDDO
  !
  CALL CLOSE_FILE('ASCII ',IUNIT)
!  ENDIF 
  !
ENDDO !JCAT

YFORM='(i6,10f15.1)'
YFILE=TRIM('bilan_tot.txt')
!
 CALL OPEN_FILE('ASCII ',IUNIT,HFILE=YFILE,HFORM='FORMATTED',HACTION='WRITE')
!   
WRITE(IUNIT,*) '     T','         ',YB_VAR(1),'         ',YB_VAR(2),'         ',&
                                    YB_VAR(3),'         ',YB_VAR(4),'         ',&
                                    YB_VAR(5),'         ',YB_VAR(6),'         ',&
                                    YB_VAR(7),'         ',YB_VAR(8),'         ',&
                                    YB_VAR(9),'         ',YB_VAR(10)
!
DO JSTP=1,NNB_TOPD_STEP
  WRITE(IUNIT,YFORM) JSTP,XB_VAR_TOT(JSTP,1:10)
ENDDO
!
 CALL CLOSE_FILE('ASCII ',IUNIT)
!
IF (LHOOK) CALL DR_HOOK('WRITE_BUDGET_COUPL_ROUT',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_BUDGET_COUPL_ROUT
