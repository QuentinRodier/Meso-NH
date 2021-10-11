!     #########
      SUBROUTINE URBTREE_PROPERTIES(PE, P, T, KCAN, PTRANS_HVEG, PTRANS_HVCR, HPROGRAM)
!     ##########################################################################
!
!!****  *URBTREE_PROPERTIES*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates leaf area density profile for urban trees thanks to LAI
!         
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      
!!    AUTHOR
!!    ------
!!
!!    E. Redon & A. Lemonsu           * Meteo-France *
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_n, ONLY : ISBA_PE_t, ISBA_K_t, ISBA_P_t
USE MODD_TEB_n,  ONLY : TEB_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODD_URBTREE, ONLY : XTAU_SWHV
!
USE MODI_GET_LUOUT
!
USE YOMHOOK       ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1      ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_P_t),  INTENT(INOUT) :: P
TYPE(ISBA_PE_t), INTENT(INOUT) :: PE
TYPE(TEB_t),     INTENT(INOUT) :: T
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM    ! program calling surf. schemes
!
INTEGER,              INTENT(IN)   :: KCAN            ! Number of layers in the canyon
                                                      ! (only even number)
REAL, DIMENSION(:,:), INTENT(OUT)  :: PTRANS_HVEG     ! transmissivity profile by layer trough urban trees crown
                                                      ! within 2 urban layers
                                                      ! (0,h/2) : first level
                                                      ! (h/2,h) : second level
REAL, DIMENSION(:),   INTENT(OUT)  :: PTRANS_HVCR     ! transmissivity profile by layer trough all urban trees crown
!-------------------------------------------------------------------------------
!
!*      0.2    Local variables
!              ---------------
!
INTEGER                                     :: JL,JI           ! Loop variable
!
REAL, DIMENSION(2)                          :: ZINF,ZSUP       ! Vertical grid inside canyon (intermediate variables for calculation)
REAL, DIMENSION(SIZE(PE%XLAI),2)       :: ZH              ! Foliage thickness in vertical layers of canyon
!
REAL, DIMENSION(SIZE(PE%XLAI),KCAN)    :: ZLAD_HVEG       ! leaf area density
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*     1.     INITIALIZATION
!             --------------
!
PTRANS_HVEG (:,:) = XUNDEF
PTRANS_HVCR (:)   = XUNDEF
ZLAD_HVEG   (:,:) = XUNDEF
!
!--------------------------------------------------------------------------------
!
!*     2.     TRANSMISSIVITY PROFILE
!             -------------------------
!
! Nouvelle hypothèse
! Discretization of height of crown between 2 canyon layers
! LAYER1=[0;h/2] , LAYER2=[h/2;h] 
!
ZH(:,:) = 0.
!
DO JI = 1,SIZE(PE%XLAI)
 IF (T%XURBTREE(JI)==0.) CYCLE
!
  ZINF(1) = 0.
  ZINF(2) = T%XBLD_HEIGHT(JI)*1./2.
!
  ZSUP(1) = T%XBLD_HEIGHT(JI)*1./2.
  ZSUP(2) = T%XBLD_HEIGHT(JI)
!
  DO JL = 1,2
!
    IF (P%XHTRUNK_HVEG(JI) .GT. ZSUP(JL)) THEN
      ZH(JI,JL) = 0.
    ELSE IF (P%XH_TREE(JI) .LE. ZINF(JL)) THEN
      ZH(JI,JL) = 0.
    ELSE
      IF (P%XHTRUNK_HVEG(JI) .GT. ZINF(JL)) THEN
        IF (P%XH_TREE(JI) .GT. ZSUP(JL)) THEN
          ZH(JI,JL) = ZSUP(JL)-P%XHTRUNK_HVEG(JI)
        ELSE
          ZH(JI,JL) = P%XH_TREE(JI)-P%XHTRUNK_HVEG(JI)
        ENDIF
      ELSE
        IF (P%XH_TREE(JI) .GT. ZSUP(JL)) THEN
          ZH(JI,JL) = ZSUP(JL)-ZINF(JL)
        ELSE
          ZH(JI,JL) = P%XH_TREE(JI)-ZINF(JL)
        ENDIF
      ENDIF
    ENDIF
!
  ENDDO
!
  ZLAD_HVEG(JI,1)   = PE%XLAI(JI) * ( ZH(JI,1) / (P%XH_TREE(JI)-P%XHTRUNK_HVEG(JI)) )
  ZLAD_HVEG(JI,2)   = PE%XLAI(JI) * ( ZH(JI,2) / (P%XH_TREE(JI)-P%XHTRUNK_HVEG(JI)) )
!
! Partial transmissivity calculated function of a thickness of crown (LAD)
  PTRANS_HVEG(JI,:) = EXP(-XTAU_SWHV * ZLAD_HVEG(JI,:) )

!
!Calculation of transmissivity through all crown (vertically)
  PTRANS_HVCR(JI) = EXP(-XTAU_SWHV * PE%XLAI(JI) )
!
ENDDO
!--------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('URBTREE_PROPERTIES',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE URBTREE_PROPERTIES
