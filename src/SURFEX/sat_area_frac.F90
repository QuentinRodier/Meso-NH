!-----------------------------------------------------------------
!     ########################
      SUBROUTINE SAT_AREA_FRAC(PDEF,PAS)
!     ########################
!
!!*****    * SAT_AREA_FRAC *
!
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
!!      Original   27/11/2006
!
!----------------------------------------------------------------------
!*       0.      DECLARATIONS
!                ------------
!
USE MODD_TOPODYN,       ONLY : NNCAT, NNMC, XDXT
USE MODD_COUPLING_TOPD, ONLY : NMASKT
USE MODD_SURF_ATM_GRID_n, ONLY : XMESH_SIZE
USE MODD_SURF_PAR,        ONLY : XUNDEF, NUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:),INTENT(IN)     :: PDEF    ! deficit
REAL, DIMENSION(:), INTENT(OUT)     :: PAS     !contributive area fraction in Isba meshes
!
!*      0.2    declarations of local variables
INTEGER               :: JJ, JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SAT_AREA_FRAC',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
! 
PAS(:)=0.0
!
DO JJ=1,NNCAT
  DO JI=1,NNMC(JJ)
    IF (PDEF(JJ,JI)==0.0 .AND. NMASKT(JJ,JI)/=NUNDEF) THEN 
      PAS(NMASKT(JJ,JI)) = PAS(NMASKT(JJ,JI)) + XDXT(JJ)**2
    ENDIF
  ENDDO
ENDDO
!
! Calculation of the saturated area ratio in each Isba mesh
WHERE ((XMESH_SIZE/=0.).AND.(PAS/=XUNDEF))
  PAS(:) = PAS(:) / XMESH_SIZE(:) 
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('SAT_AREA_FRAC',1,ZHOOK_HANDLE)
!
END SUBROUTINE SAT_AREA_FRAC
