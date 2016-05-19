!-----------------------------------------------------------------
!     ####################
      SUBROUTINE TOPD_TO_ISBA_SLOPE(KI)
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
!!      B. Vincendon	* Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   12/11/2012
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TOPODYN,       ONLY : NNCAT, NNMC, XTANB
USE MODD_COUPLING_TOPD, ONLY : NMASKT,NNPIX
USE MODD_SURF_ATM_SSO_n, ONLY : XSSO_SLOPE
USE MODD_SURF_PAR,        ONLY : NUNDEF
USE MODD_SURF_ATM_n,      ONLY : XNATURE, NSIZE_NATURE, NR_NATURE, &
                                 NSIZE_FULL, NDIM_NATURE, NDIM_FULL  
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
!
!*      0.2    declarations of local variables
!
!
INTEGER                :: JCAT,JPIX,JJ          ! loop control 
REAL, DIMENSION(KI)    :: ZCOUNT                ! TOPO pixel number in an ISBA pixel
                                                ! on the full grid
REAL, DIMENSION(KI)    :: ZSSO_SLOPE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TOPD_TO_ISBA_SLOPE',0,ZHOOK_HANDLE)
!
!*        1.0  Compute Mean slope over each ISBA_MESH
!            ----------------------------------------------------------------------
!
!write(*,*) 'pente avt topmodel',MINVAL(XSSO_SLOPE),MAXVAL(XSSO_SLOPE),SUM(XSSO_SLOPE,MASK=XSSO_SLOPE/=XUNDEF)
!
ZSSO_SLOPE = XSSO_SLOPE
!
ZCOUNT(:) = REAL(NNPIX(:))

WHERE (ZCOUNT /= 0.0)
   ZSSO_SLOPE = 0.
ENDWHERE
!
DO JCAT=1,NNCAT
  DO JPIX=1,NNMC(JCAT)
    IF (NMASKT(JCAT,JPIX) /= NUNDEF) THEN
      ZSSO_SLOPE(NMASKT(JCAT,JPIX)) = ZSSO_SLOPE(NMASKT(JCAT,JPIX)) + XTANB(JCAT,JPIX)
    ENDIF
  ENDDO
ENDDO
!
WHERE (ZCOUNT /= 0.0)
   ZSSO_SLOPE = ZSSO_SLOPE / ZCOUNT
ENDWHERE
!
XSSO_SLOPE = ZSSO_SLOPE
!
!write(*,*) 'pente apres modification',MINVAL(XSSO_SLOPE),MAXVAL(XSSO_SLOPE),COUNT(ZCOUNT/=0.0),SUM(XSSO_SLOPE,MASK=XSSO_SLOPE/=XUNDEF)
!
IF (LHOOK) CALL DR_HOOK('TOPD_TO_ISBA_SLOPE',1,ZHOOK_HANDLE)
!
END SUBROUTINE TOPD_TO_ISBA_SLOPE
