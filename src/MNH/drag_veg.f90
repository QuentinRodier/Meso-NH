!     #######################
       MODULE MODI_DRAG_VEG
!     #######################
!
INTERFACE

SUBROUTINE DRAG_VEG(PUT,PVT,PTKET,PRHODJ,PZZ,PRUS, PRVS, PRTKES)
!

REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT, PVT   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTKET           !   at t
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ    ! dry Density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ       ! Height (z)
!

!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS, PRVS       ! Sources of Momentum
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTKES           ! Sources of Tke
!
!

END SUBROUTINE DRAG_VEG

END INTERFACE

END MODULE MODI_DRAG_VEG
!
!     ###################################################################
        SUBROUTINE DRAG_VEG(PUT,PVT,PTKET,PRHODJ,PZZ,PRUS, PRVS, PRTKES)
!     ###################################################################
!
!!****  *DRAG_VEG_n * -
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     P. Aumond 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2009
!!       C.Lac      07/2011 : Add budgets
!!---------------------------------------------------------------
!
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CONF
USE MODD_CST
USE MODD_DYN
USE MODD_DYN_n
USE MODD_VEG_n
USE MODD_BUDGET

!
USE MODI_SHUMAN
USE MODD_PGDFIELDS
USE MODD_GROUND_PAR
USE MODI_MNHGET_SURF_PARAM_n
USE MODI_BUDGET

!  
IMPLICIT NONE
!  
!*       0.1   Declarations of dummy arguments :
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT, PVT   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTKET           !   at t
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ    ! dry Density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ       ! Height (z)
!

!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS, PRVS       ! Sources of Momentum
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTKES           ! Sources of Tke
!
!
!*       0.2   Declarations of local variables :
!
INTEGER    ::  IIU,IJU,IKU,IKV         ! array size along the k direction 
INTEGER  :: JI, JJ, JK             ! loop index
!
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZH_TREE_PGD ! surface cover types
REAL, DIMENSION(:,:), ALLOCATABLE :: ZLAI_PGD ! surface cover types
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) ::           &
                              ZWORK1, ZWORK2, ZWORK3, ZUT, ZVT,   &
                              ZUS, ZVS, ZTKES, ZTKET
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) ::           &
                              ZCDRAG, ZDENSITY
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2)) ::           &
                              ZVH,ZLAI           !  LAI, Hauteur de la vegetation

!
!
IIU = SIZE(PUT,1)
IJU = SIZE(PUT,2)
IKU = SIZE(PUT,3)
!
!*       0.3     Initialisation de kelkes variables
!	       
ZVH(:,:)=0.
ZLAI(:,:)=0.
ZCDRAG(:,:,:)=0.
ZDENSITY(:,:,:)=0.
!
ALLOCATE(ZH_TREE_PGD(IIU,IJU))
ALLOCATE(ZLAI_PGD(IIU,IJU))
!
CALL MNHGET_SURF_PARAM_n(PVH=ZH_TREE_PGD,PLAI=ZLAI_PGD)
!
ZVH(:,:)=ZH_TREE_PGD(:,:)
ZLAI(:,:)=ZLAI_PGD(:,:)
!
DEALLOCATE(ZH_TREE_PGD)
DEALLOCATE(ZLAI_PGD)
!
!-------------------------------------------------------------------------------
!
!
!*       1.     COMPUTES THE TRUE VELOCITY COMPONENTS
!	        -------------------------------------
!
ZUT(:,:,:) = PUT(:,:,:) 
ZVT(:,:,:) = PVT(:,:,:) 
ZTKET(:,:,:) = PTKET(:,:,:) 
!-------------------------------------------------------------------------------
!
!*      1.     Computations of wind tendency due to canopy drag
!              ------------------------------------------------
!
!
!
! Ext = - Cdrag  * u- * u- * Sv       tree canopy drag
!       - u'w'(ground)     * Sh       horizontal surfaces (ground)
!
!*      1.1    Drag coefficient by vegetation (Patton et al 2001)
!              ------------------------------
!
DO JJ=2,(IJU-1)
 DO JI=2,(IIU-1)
   IF (ZVH(JI,JJ) /= 0) THEN
     DO JK=2,(IKU-1) 
         IF ((ZVH(JI,JJ)+PZZ(JI,JJ,2))<PZZ(JI,JJ,JK)) EXIT
           ZCDRAG(JI,JJ,JK)  = 0.3 !0.075
           ZDENSITY(JI,JJ,JK) = MAX((4 * (ZLAI(JI,JJ) *&
                               (PZZ(JI,JJ,JK)-PZZ(JI,JJ,2)) *&
                               (PZZ(JI,JJ,JK)-PZZ(JI,JJ,2)) *&
                               (ZVH(JI,JJ)-(PZZ(JI,JJ,JK)-PZZ(JI,JJ,2)))/&
                                ZVH(JI,JJ)**3)-&
                               (0.30*((ZLAI(JI,JJ) *&
                               (PZZ(JI,JJ,JK)-PZZ(JI,JJ,2)) *&
                               (PZZ(JI,JJ,JK)-PZZ(JI,JJ,2)) *&
                               (PZZ(JI,JJ,JK)-PZZ(JI,JJ,2)) /&
                               (ZVH(JI,JJ)**3))-ZLAI(JI,JJ))))/&
                                ZVH(JI,JJ), 0.)

                                            
     END DO
   END IF
 END DO
END DO
!
!
!*      1.2    Drag force by wall surfaces
!              ---------------------------
!
!* drag force by vertical surfaces
!
ZUS(:,:,:)=  ZUT(:,:,:)/(1 + ZCDRAG(:,:,:)* ZDENSITY(:,:,:) &
            *SQRT(ZUT(:,:,:)**2+ZVT(:,:,:)**2))
!
ZVS(:,:,:)=  ZVT(:,:,:)/(1 + ZCDRAG(:,:,:)* ZDENSITY(:,:,:) &
            *SQRT(ZUT(:,:,:)**2+ZVT(:,:,:)**2))
!
PRUS(:,:,:)=PRUS(:,:,:)+((ZUS(:,:,:)-ZUT(:,:,:))*PRHODJ(:,:,:))
!
PRVS(:,:,:)=PRVS(:,:,:)+((ZVS(:,:,:)-ZVT(:,:,:))*PRHODJ(:,:,:))
!
IF (LBUDGET_U) CALL BUDGET (PRUS,1,'DRAG_BU_RU')
IF (LBUDGET_V) CALL BUDGET (PRVS,2,'DRAG_BU_RV')
!
!
!*      3.     Computations of TKE  tendency due to canopy drag
!              ------------------------------------------------

!*      3.1    Creation of TKE by wake
!              -----------------------
!
! from Kanda and Hino (1994)
!
! Ext = + Cd * u+^3  * Sv/Vair        vertical surfaces or trees             
! Ext = - Cd * e * u  * Sv        trees Destruction of TKE due to 
!   small-scale motions forced by leaves from Kanda and Hino (1994)
!
! with Vair = Vair/Vtot * Vtot = (Vair/Vtot) * Stot * Dz
! and  Sv/Vair = (Sv/Stot) * Stot/Vair = (Sv/Stot) / (Vair/Vtot) / Dz
!
ZTKES(:,:,:)=  (ZTKET(:,:,:) + (ZCDRAG(:,:,:)* ZDENSITY(:,:,:) &
            *(SQRT(ZUT(:,:,:)**2+ZVT(:,:,:)**2))**3))   /&
            (1.+(2.*ZCDRAG(:,:,:)* ZDENSITY(:,:,:)*SQRT(ZUT(:,:,:)**2+ZVT(:,:,:)**2)))
!
PRTKES(:,:,:)=PRTKES(:,:,:)+((ZTKES(:,:,:)-ZTKET(:,:,:))*PRHODJ(:,:,:))
!
IF (LBUDGET_TKE) CALL BUDGET (PRTKES(:,:,:),5,'DRAG_BU_RTKE')
!
END SUBROUTINE DRAG_VEG
