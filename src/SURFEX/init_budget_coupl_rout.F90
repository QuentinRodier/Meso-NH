!     ##########################
      SUBROUTINE INIT_BUDGET_COUPL_ROUT(KNI)
!     ##########################
!
!!
!!    PURPOSE
!!    -------
!    Initialise varriables usefull for budget computation    
!     
!!**  METHOD
!!    ------
!!    Terms of the budget on all the domain
!!    XB_VAR_TOT(forcing time step,variable)
!!    Water going in the system
!!      variable =1 : Rain
!!      variable =2 : Snow
!!    Water going out of the system
!!      variable =3 : Incerception 
!!      variable =4 : Evaporation
!!      variable =5 : Runoff
!!      variable =6 : Drainage
!!      variable =7 : Variation of liquid water stocked in the ground
!!      variable =8 : Variation of solid water stocked in the ground
!!      variable =9 : Variation of melting snow
!!    Budget 
!!      variable =10:  Water going in the system- Water going out of the system
!!    
!!    Terms of the budget on a given catchment
!!    XB_VAR_BV(forcing time step,catchment,variable)
!!    XB_VAR_NOBV(forcing time step,catchment,variable)
!!    
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
!!    L. Bouilloud &  B. Vincendon	* Meteo-France *

!!    
!!    MODIFICATIONS
!!    -------------
!!
!!      Original  03/2008 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
      ! declarative modules
USE MODD_BUDGET_COUPL_ROUT ! contains all useful variables XB_*
!
USE MODD_TOPODYN,       ONLY : NNCAT, NNMC, XQTOT,&
                                 NNB_TOPD_STEP, XDXT, XQB_DR, XQB_RUN
USE MODD_COUPLING_TOPD, ONLY : XAS_NATURE,  NNB_TOPD,&
                                 XRUNOFF_TOP, XATOP, XBV_IN_MESH
USE MODD_ISBA_n,          ONLY : XWG, XDG,  XWR, CRUNOFF, XWGI, TSNOW
USE MODD_DIAG_EVAP_ISBA_n,ONLY : XAVG_EVAPC, XAVG_LEGC, XAVG_RUNOFFC ,XAVG_DRAINC,&
                                 XAVG_DRAIN, XAVG_RUNOFF, XAVG_EVAP !ludo
USE MODD_ISBA_GRID_n,     ONLY : XMESH_SIZE !ludo
USE MODD_SURF_ATM_n,      ONLY : NR_NATURE
!
USE MODD_SURF_PAR,         ONLY:XUNDEF
!
USE MODI_UNPACK_SAME_RANK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN)           :: KNI      ! expected physical size of full surface array
!
!*      0.2    declarations of local variables
!
INTEGER    :: JJ,JWRK2
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_BUDGET_COUPL_ROUT',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
!
!write(*,*) 'In init_budget_coupl_rout KNI=',KNI
ALLOCATE(YB_VAR(10)) 
YB_VAR(1)='RAIN  '
YB_VAR(2)='SNOW  '
YB_VAR(3)='INTERC'
YB_VAR(4)='EVATRA'
YB_VAR(5)='RUNOFF'
YB_VAR(6)='DRAINA'
YB_VAR(7)='DSTOWG'
YB_VAR(8)='DSTOWI'
YB_VAR(9)='DSTOSW'
YB_VAR(10)='BUDGET'
!   
!
ALLOCATE(XB_RAIN(KNI))
ALLOCATE(XB_SNOW(KNI))
!
ALLOCATE(XB_WR(KNI))
ALLOCATE(XB_EVAP(KNI))
ALLOCATE(XB_RUNOFF_ISBA(KNI))
ALLOCATE(XB_DRAIN(KNI))
ALLOCATE(XB_WG2(KNI))
ALLOCATE(XB_WG3(KNI))
ALLOCATE(XB_WGTOT(KNI))
ALLOCATE(XB_WGI2(KNI))
ALLOCATE(XB_WGI3(KNI))
ALLOCATE(XB_WGITOT(KNI))
ALLOCATE(XB_SWE1(KNI))
ALLOCATE(XB_SWE2(KNI))
ALLOCATE(XB_SWE3(KNI))
ALLOCATE(XB_SWETOT(KNI))
!
ALLOCATE(XB_WRM(KNI))
ALLOCATE(XB_EVAPM(KNI))
ALLOCATE(XB_DRAINM(KNI))
ALLOCATE(XB_RUNOFF_ISBAM(KNI))
ALLOCATE(XB_WG2M(KNI))
ALLOCATE(XB_WG3M(KNI))
ALLOCATE(XB_WGTOTM(KNI))
ALLOCATE(XB_WGI2M(KNI))
ALLOCATE(XB_WGI3M(KNI))
ALLOCATE(XB_WGITOTM(KNI))
ALLOCATE(XB_SWE1M(KNI))
ALLOCATE(XB_SWE2M(KNI))
ALLOCATE(XB_SWE3M(KNI))   
ALLOCATE(XB_SWETOTM(KNI))
!
ALLOCATE(XB_MESH_SIZE(KNI))
ALLOCATE(XB_DG2(KNI))
ALLOCATE(XB_DG3(KNI))
!
 CALL UNPACK_SAME_RANK(NR_NATURE,XWR(:,1),XB_WRM)
 CALL UNPACK_SAME_RANK(NR_NATURE,XAVG_EVAPC,XB_EVAPM)
 CALL UNPACK_SAME_RANK(NR_NATURE,XAVG_RUNOFFC,XB_RUNOFF_ISBAM)
 CALL UNPACK_SAME_RANK(NR_NATURE,XAVG_DRAINC,XB_DRAINM)
 CALL UNPACK_SAME_RANK(NR_NATURE,XWG(:,2,1),XB_WG2M)
 CALL UNPACK_SAME_RANK(NR_NATURE,XWG(:,3,1),XB_WG3M)
 CALL UNPACK_SAME_RANK(NR_NATURE,XDG(:,2,1),XB_DG2)
 CALL UNPACK_SAME_RANK(NR_NATURE,XDG(:,3,1),XB_DG3)
!
WHERE ( XB_WG2M/=XUNDEF .AND. XB_DG2/=XUNDEF .AND. XB_WG3M/=XUNDEF .AND. XB_DG3/=XUNDEF )
  XB_WGTOTM(:) = XB_WG2M(:)*XB_DG2(:) + XB_WG3M(:)*(XB_DG3(:)-XB_DG2(:)) !m3/m2
ELSEWHERE
  XB_WGTOTM(:) = XUNDEF
ENDWHERE
!
 CALL UNPACK_SAME_RANK(NR_NATURE,XWGI(:,2,1),XB_WGI2M)
 CALL UNPACK_SAME_RANK(NR_NATURE,XWGI(:,3,1),XB_WGI3M)
WHERE ((XB_WGI2M/=XUNDEF).AND.(XB_DG2/=XUNDEF).AND.(XB_WGI3M/=XUNDEF).AND.(XB_DG3/=XUNDEF))
  XB_WGITOTM(:) = XB_WGI2M(:)*XB_DG2(:) + XB_WGI3M(:)*(XB_DG3(:)-XB_DG2(:)) !m3/m2
ELSEWHERE
  XB_WGITOTM(:) = XUNDEF
ENDWHERE
!
 CALL UNPACK_SAME_RANK(NR_NATURE,TSNOW%WSNOW(:,1,1),XB_SWE1M)
 CALL UNPACK_SAME_RANK(NR_NATURE,TSNOW%WSNOW(:,2,1),XB_SWE2M)
 CALL UNPACK_SAME_RANK(NR_NATURE,TSNOW%WSNOW(:,3,1),XB_SWE3M)
XB_SWETOTM(:) = XB_SWE1M(:)+XB_SWE2M(:)+XB_SWE3M(:)
!
 CALL UNPACK_SAME_RANK(NR_NATURE,XMESH_SIZE,XB_MESH_SIZE)
!
ALLOCATE(XB_ABV_BYMESH(KNI,NNCAT))
DO JJ=1,KNI
  XB_ABV_BYMESH(JJ,:) = XBV_IN_MESH(JJ,:)/XB_MESH_SIZE(JJ) !*NNMC(:)*XDXT(:)**2 ! 
  XB_ABV_BYMESH(JJ,:) = MIN(1.,XB_ABV_BYMESH(JJ,:))      
ENDDO
! 
ALLOCATE(XB_VAR_BV(NNB_TOPD_STEP,NNCAT,10))
XB_VAR_BV(:,:,:) = 0.
ALLOCATE(XB_VAR_NOBV(NNB_TOPD_STEP,NNCAT,10))
XB_VAR_NOBV(:,:,:) = 0.
! 
ALLOCATE(XB_VAR_TOT(NNB_TOPD_STEP,10))
XB_VAR_TOT(:,:) = 0.
!
ALLOCATE(XB_RUNOFF_TOPD(KNI))
ALLOCATE(XB_RUNOFF_TOPDM(KNI))
ALLOCATE(XB_ATOP_BYMESH(KNI))
!  
 CALL UNPACK_SAME_RANK(NR_NATURE,XATOP,XB_ATOP_BYMESH)
IF(CRUNOFF=='TOPD')THEN
  CALL UNPACK_SAME_RANK(NR_NATURE,XRUNOFF_TOP,XB_RUNOFF_TOPDM)
ELSE
  XB_RUNOFF_TOPDM = XB_RUNOFF_ISBAM
ENDIF
!
ALLOCATE(YB_VARQ(5)) 
YB_VARQ(1)='Q_TOT '
YB_VARQ(2)='Q_RUN '
YB_VARQ(3)='Q_DR  '
YB_VARQ(4)='ST_RUN'
YB_VARQ(5)='ST_DR '
!
!
ALLOCATE(XB_QTOT(NNCAT))
ALLOCATE(XB_QDR(NNCAT))
ALLOCATE(XB_QRUN(NNCAT))
ALLOCATE(XB_VAR_Q(NNB_TOPD_STEP,NNCAT,5))
!
ALLOCATE(XB_QTOTM(NNCAT))
ALLOCATE(XB_QDRM(NNCAT))
ALLOCATE(XB_QRUNM(NNCAT))
! 
!init var bilan q
!
XB_QTOT(:)=0.
XB_QDR(:) =0.
XB_QRUN(:)=0.
!
XB_VAR_Q(:,:,:)=0
!
DO JJ=1,NNCAT
  XB_QTOTM(JJ) = SUM(XQTOT(JJ,:))
  XB_QRUNM(JJ) = SUM(XQB_RUN(JJ,:))
  XB_QDRM(JJ)  = SUM(XQB_DR(JJ,:))
ENDDO
!
IF (LHOOK) CALL DR_HOOK('INIT_BUDGET_COUPL_ROUT',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_BUDGET_COUPL_ROUT
