!-----------------------------------------------------------------
!     #######################     
      SUBROUTINE READ_CONNEX_FILE(HPROGRAM,HFILE,HFORM,KNMC,PCONN,KLINE)
!     #######################
!
!!****  *READ_CONNEX_FILE*  
!!
!!    PURPOSE
!!    -------
!     This routine aims at reading connexion file
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
!!    
!!    
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Vincendon    * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   11/2006
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TOPODYN, ONLY : NPMAX
USE MODD_SURF_PAR,  ONLY : XUNDEF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*),  INTENT(IN)  :: HPROGRAM    !
 CHARACTER(LEN=*),  INTENT(IN)  :: HFILE       ! File to be read
 CHARACTER(LEN=*),  INTENT(IN)  :: HFORM       ! Format of the file to be read
INTEGER,           INTENT(IN)  :: KNMC       ! Number of pixels in the catchment
REAL, DIMENSION(:,:),INTENT(OUT)   :: PCONN    ! pixels topographic slope/length flow
INTEGER, DIMENSION(:),INTENT(OUT)  :: KLINE    ! second index of the pixel in the array PCONN
!
!*      0.2    declarations of local variables
!
!
INTEGER                   :: JJ          ! loop control 
INTEGER                   :: IUNIT       ! Unit of the files
INTEGER                   :: ILUOUT      ! Unit of the files
INTEGER                   :: IINDEX      ! index of the pixel in the topo domain
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_CONNEX_FILE',0,ZHOOK_HANDLE)
!
!*       0.2    preparing file openning
!               ----------------------
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL OPEN_FILE(HPROGRAM,IUNIT,HFILE,HFORM,HACTION='READ')
!
WRITE(ILUOUT,*) 'Open ',HFILE,'debut'
!
DO JJ=1,7
  READ(IUNIT,*) 
ENDDO
!
DO JJ=1,KNMC
  !
  READ(IUNIT,'(f9.0,1x,f8.3,1x,2(f1.0,1x),8(f8.0,1x,f8.6,1x))',END=120) PCONN(JJ,:)
  IINDEX = INT(PCONN(JJ,1))
  KLINE(IINDEX) = JJ
  !
ENDDO
!   
120   CALL CLOSE_FILE(HPROGRAM,IUNIT)
!
IF (LHOOK) CALL DR_HOOK('READ_CONNEX_FILE',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_CONNEX_FILE







