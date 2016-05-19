!-----------------------------------------------------------------
!     #######################     
      SUBROUTINE READ_SLOPE_FILE(HPROGRAM,HFILE,HFORM,KNMC,PTANB,PSLOP,PDAREA,PLAMBDA)
!     #######################
!
!!****  *READ_SLOPE_FILE*  
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
INTEGER,           INTENT(IN)  :: KNMC        ! Number of pixels in the catchment
REAL, DIMENSION(:),  INTENT(OUT)   :: PTANB    ! pixels topographic slope(tan(beta)
REAL, DIMENSION(:),  INTENT(OUT)   :: PSLOP   ! pixels topographic slope/length flow
REAL, DIMENSION(:),  INTENT(OUT)   :: PDAREA  ! drainage area (aire drainee)
REAL, DIMENSION(:),  INTENT(OUT)   :: PLAMBDA ! pure topographic index
!
!*      0.2    declarations of local variables
!
!
INTEGER                   :: JJ ! loop control 
INTEGER                   :: IWRK        ! work variable
INTEGER                   :: IUNIT       ! Unit of the files
INTEGER                   :: ILUOUT      ! Unit of the files
!
REAL                      :: ZWRK        ! work variable
REAL, DIMENSION(KNMC)     :: ZDAREA      ! drainage area (aire drainee)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_SLOPE_FILE',0,ZHOOK_HANDLE)
!
!*       0.2    preparing file openning
!               ----------------------
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL OPEN_FILE(HPROGRAM,IUNIT,HFILE,HFORM,HACTION='READ')
!
READ(IUNIT,*) 
!
DO JJ=1,KNMC
  !
   READ(IUNIT,*,END=110) IWRK, PTANB(JJ), PSLOP(JJ), ZWRK, PDAREA(JJ)
  PLAMBDA(JJ) = LOG(PDAREA(JJ)/PSLOP(JJ))
  !
ENDDO
!
110 CALL CLOSE_FILE(HPROGRAM,IUNIT)
!
IF (LHOOK) CALL DR_HOOK('READ_SLOPE_FILE',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SLOPE_FILE







