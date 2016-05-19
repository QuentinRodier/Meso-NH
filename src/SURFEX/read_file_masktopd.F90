!-----------------------------------------------------------------
!     ##########################
      SUBROUTINE READ_FILE_MASKTOPD(KI)
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
!
USE MODD_TOPODYN
USE MODD_COUPLING_TOPD, ONLY : NMASKI, NNPIX, NMASKT
USE MODD_SURF_PAR,        ONLY : XUNDEF, NUNDEF
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
USE MODI_READ_TOPD_FILE
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
!
!*      0.2    declarations of local variables
INTEGER                    :: JCAT,JMESH,JPIX
INTEGER                    :: INUMPIX
INTEGER :: IIMAX,IJMAX,IUNIT
 CHARACTER(LEN=50) :: YNAME
REAL, DIMENSION(:),ALLOCATABLE    :: ZTOPD_READ !Topgraphic variable read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_FILE_MASKTOPD',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
!
ALLOCATE(ZTOPD_READ(NPMAX))
!
DO JCAT=1,NNCAT
  !
  YNAME=TRIM(CCAT(JCAT))//TRIM('.mask_topd')
  !
  CALL READ_TOPD_FILE('OFFLIN',YNAME,'FORMATTED',NNPT(JCAT),ZTOPD_READ)
  !
  DO JPIX=1,NPMAX
    !
    IF ( NLINE(JCAT,JPIX)/=0 ) THEN
      IF  (ZTOPD_READ(JPIX)==XUNDEF ) THEN
        NMASKT(JCAT,NLINE(JCAT,JPIX)) = NUNDEF
      ELSE
        NMASKT(JCAT,NLINE(JCAT,JPIX)) = FLOOR(ZTOPD_READ(JPIX))
      ENDIF
    ENDIF
    !
  ENDDO
  !
ENDDO
!
ALLOCATE(NNPIX(KI))
NNPIX(:) = NUNDEF
DO JMESH=1,KI
  NNPIX(JMESH) = COUNT(NMASKT(:,:)==JMESH)
ENDDO
INUMPIX=MAXVAL(NNPIX)
!
ALLOCATE(NMASKI(KI,NNCAT,INUMPIX))
NMASKI(:,:,:) = NUNDEF

DO JCAT=1,NNCAT
  !
  YNAME=TRIM(CCAT(JCAT))//TRIM('.mask_surf')
  CALL OPEN_FILE('ASCII ',IUNIT,YNAME,'FORMATTED','READ')
  ! 
  DO JMESH=1,KI
    DO JPIX=1,NNPIX(JMESH)
      READ(IUNIT,*) NMASKI(JMESH,JCAT,JPIX)
    ENDDO
  ENDDO
  !
  CALL CLOSE_FILE('ASCII ',IUNIT)
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('READ_FILE_MASKTOPD',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_FILE_MASKTOPD
