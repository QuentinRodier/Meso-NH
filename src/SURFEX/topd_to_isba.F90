!-----------------------------------------------------------------
!     ####################
      SUBROUTINE TOPD_TO_ISBA(KI,KSTEP,GTOPD)
!     ####################
!
!!****  *TOPD_TO_ISBA*  
!!
!!    PURPOSE
!!    -------
!    
!     
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
!!      K. Chancibault	* LTHE / Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   09/10/2003
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!*** SurfEx ***
USE MODI_UNPACK_SAME_RANK
!*** Coupling ***
USE MODI_WRITE_FILE_ISBAMAP
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
USE MODD_TOPODYN,       ONLY : NNCAT, NNMC, NNB_TOPD_STEP
USE MODD_COUPLING_TOPD, ONLY : XWG_FULL, XDTOPT, XWTOPT, XWSUPSAT,&
                                 NMASKT, XTOTBV_IN_MESH, NNPIX, NFREQ_MAPS_WG
!
USE MODD_ISBA_n,          ONLY : XWSAT
USE MODD_SURF_ATM_GRID_n, ONLY : XMESH_SIZE
USE MODD_SURF_PAR,        ONLY : XUNDEF
USE MODD_SURF_ATM_n,      ONLY : NR_NATURE
USE MODD_ISBA_PAR,        ONLY : XWGMIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
INTEGER, INTENT(IN)                 :: KI      ! Grid dimensions
INTEGER, INTENT(IN)                 :: KSTEP   ! Topodyn current time step
LOGICAL, DIMENSION(:), INTENT(IN) :: GTOPD     ! 
!
!*      0.2    declarations of local variables
!
!
INTEGER                :: JJ, JI          ! loop control 
INTEGER                :: IUNIT               
REAL, DIMENSION(KI)    :: ZW              ! TOPODYN water content on ISBA grid (mm)
REAL, DIMENSION(KI)    :: ZCOUNT          ! TOPO pixel number in an ISBA pixel
REAL, DIMENSION(KI)    :: ZWSAT_FULL      ! Water content at saturation on the layer 2 
                                          ! on the full grid
REAL, DIMENSION(KI)    :: ZWG_OLD
REAL, DIMENSION(KI)    :: ZDG_FULL
 CHARACTER(LEN=3)       :: YSTEP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('TOPD_TO_ISBA',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
!
ZW(: )= 0.0
!
ZWG_OLD(:) = XWG_FULL(:)
!
!
!*       1.     TOPODYN-LAT => ISBA
!               -------------------
!*       1.1    mobilizable water
!               -----------------
!
DO JJ=1,NNCAT
  IF (GTOPD(JJ)) THEN
    DO JI=1,NNMC(JJ)
      IF (XDTOPT(JJ,JI) /= XUNDEF) THEN
        ZW(NMASKT(JJ,JI)) = ZW(NMASKT(JJ,JI)) + XWTOPT(JJ,JI)
      ENDIF
    ENDDO
  ELSE
    GOTO 10 
  ENDIF
ENDDO
!
ZCOUNT(:)=REAL(NNPIX(:))
!
WHERE (ZCOUNT(:) /= 0.0.AND.XWG_FULL(:)/=XUNDEF)
  ZW(:) = ZW(:) / ZCOUNT
ENDWHERE
!
!
! The soil water content is the balanced mean between the soil water content calculated by TOPODYN 
! and the initial soil water content in each mesh, in function of the area of the mesh occupied by TOPODYN
WHERE ( XMESH_SIZE(:) - XTOTBV_IN_MESH(:) <= 0.0) ! la maille isba est totalement couverte par des bassins versants
  XWG_FULL(:) = ZW(:)
ELSEWHERE (XTOTBV_IN_MESH(:) /= 0.0)
  XWG_FULL = (XTOTBV_IN_MESH(:)/XMESH_SIZE(:))*ZW(:) + ((XMESH_SIZE(:)-XTOTBV_IN_MESH(:))/XMESH_SIZE(:))*XWG_FULL(:)

ENDWHERE
!
XWG_FULL(:) = MAX(XWG_FULL(:),XWGMIN)
!
10 CONTINUE 
!
 CALL UNPACK_SAME_RANK(NR_NATURE,XWSAT(:,2),ZWSAT_FULL)
!
IF (.NOT.ALLOCATED(XWSUPSAT)) ALLOCATE(XWSUPSAT(KI))
XWSUPSAT=0.
!ludo glace Wsat varie
WHERE ( XWG_FULL(:) > ZWSAT_FULL(:) .AND. XWG_FULL(:)/=XUNDEF )
  !ludo calcul sat avant wg
  XWSUPSAT(:) = XWG_FULL(:) - ZWSAT_FULL(:)
  XWG_FULL(:) = ZWSAT_FULL(:)
ENDWHERE
!
WHERE (XWG_FULL(:) < XWGMIN .AND. XWG_FULL(:)/=XUNDEF)
  XWG_FULL(:) = XWGMIN
ENDWHERE
!
IF ( NFREQ_MAPS_WG/=0 .AND. (MOD(KSTEP,NFREQ_MAPS_WG)==0 .OR. KSTEP==NNB_TOPD_STEP) ) THEN
  ! writing of YSTEP to be able to write maps
  IF (KSTEP<10) THEN
    WRITE(YSTEP,'(I1)') KSTEP
  ELSEIF (KSTEP < 100) THEN
    WRITE(YSTEP,'(I2)') KSTEP
  ELSE
    WRITE(YSTEP,'(I3)') KSTEP
  ENDIF
  !
  CALL OPEN_FILE('ASCII ',IUNIT,HFILE='carte_w'//YSTEP,HFORM='FORMATTED',HACTION='WRITE')
  CALL WRITE_FILE_ISBAMAP(IUNIT,XWG_FULL,KI)
  CALL CLOSE_FILE('ASCII ',IUNIT)
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TOPD_TO_ISBA',1,ZHOOK_HANDLE)
!
END SUBROUTINE TOPD_TO_ISBA
