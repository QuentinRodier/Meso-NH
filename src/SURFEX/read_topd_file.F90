!-----------------------------------------------------------------
!     #######################
      SUBROUTINE READ_TOPD_FILE(HPROGRAM,HFILE,HFORM,KNPT,PTOPD_READ)
!     #######################
!
!!****  *READ_TOPD_FILE*  
!!
!!    PURPOSE
!!    -------
!     This routine aims at reading topographic files
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
USE MODI_GET_LUOUT
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
USE MODD_TOPODYN, ONLY : NPMAX
USE MODD_SURF_PAR,  ONLY : XUNDEF
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
INTEGER,           INTENT(IN)  :: KNPT        ! Number of points in the catchment
REAL, DIMENSION(:), INTENT(OUT) :: PTOPD_READ ! Topographic parameter read on file
!
!*      0.2    declarations of local variables
!
INTEGER                   :: JJ,JI  ! loop control
INTEGER                   :: IUNIT  ! Unit of the files
INTEGER                   :: ILUOUT ! Unit of the files
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_TOPD_FILE',0,ZHOOK_HANDLE)
!
!*       0.2    preparing file openning
!               ----------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL OPEN_FILE(HPROGRAM,IUNIT,HFILE,HFORM,HACTION='READ')
!PTOPD_READ(:)=XUNDEF
!
DO JJ=1,13
  READ(IUNIT,*) 
ENDDO
!
DO JI=1,KNPT
  !
  READ(IUNIT,*,END=110) PTOPD_READ(JI)
  !
  IF (PTOPD_READ(JI).LT.0.0) PTOPD_READ(JI) = XUNDEF
  !
ENDDO
!
110 CALL CLOSE_FILE(HPROGRAM,IUNIT)
!
IF (LHOOK) CALL DR_HOOK('READ_TOPD_FILE',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_TOPD_FILE







