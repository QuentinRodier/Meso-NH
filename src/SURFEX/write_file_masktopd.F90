!-----------------------------------------------------------------
!     ##########################
      SUBROUTINE WRITE_FILE_MASKTOPD(KI)
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
!!      B. Vincendon	* Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   11/2011
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TOPODYN, ONLY : CCAT, NNCAT
USE MODD_COUPLING_TOPD, ONLY : NMASKI, NNPIX
USE MODD_SURF_PAR,        ONLY : XUNDEF
!
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
USE MODE_GRIDTYPE_CONF_PROJ
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN)             :: KI    ! Grid dimensions
!
!*      0.2    declarations of local variables
INTEGER           :: JCAT,JMESH,JPIX
INTEGER           :: IUNIT
 CHARACTER(LEN=50) :: YNAME
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITE_FILE_MASKTOPD',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
!
DO JCAT=1,NNCAT
  !
  YNAME = TRIM(CCAT(JCAT))//TRIM('.mask_surf')
  !
  CALL OPEN_FILE('ASCII ',IUNIT,YNAME,'FORMATTED',HACTION='WRITE')
  !
  DO JMESH=1,KI
    DO JPIX=1,NNPIX(JMESH)
      WRITE(IUNIT,*) NMASKI(JMESH,JCAT,JPIX)
    ENDDO
  ENDDO
  !
  CALL CLOSE_FILE('ASCII ',IUNIT)
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('WRITE_FILE_MASKTOPD',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_FILE_MASKTOPD
