!-----------------------------------------------------------------
!     #######################
      SUBROUTINE INIT_COUPL_TOPD(HPROGRAM,KI)
!     #######################
!
!!****  *INIT_COUPL_TOPD*  
!!
!!    PURPOSE
!!    -------
!!     This routine aims at initialising the variables 
!     needed for coupling with Topmodel.
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
!!      Original   16/10/2003
!!      Modif BV : supression of variables specific to Topmodel
!!      20/12/2007 - mll : Adaptation between a lonlat grid system for ISBA
!!                         and lambert II projection for topmodel
!!      11/2011: Modif BV : Creation of masks between ISBA and TOPODYN
!                transfered in PGD step (routine init_pgd_topd)
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
! Modules
USE MODD_COUPLING_TOPD, ONLY : XWSTOPI, XWFCTOPI, XDTOPI, XAS_NATURE, XATOP,&
                                 XCSTOPI, XWTOPT, XAVG_RUNOFFCM, XAVG_DRAINCM,&
                                 XDTOPT, XKA_PRE, XKAC_PRE, NMASKI, XDMAXFC, &
                                 XWG_FULL, XWSTOPT, XWFCTOPT, NMASKT, & 
                                 NNBV_IN_MESH, XBV_IN_MESH, XTOTBV_IN_MESH,&
                                 XRUNOFF_TOP, NNPIX,&
                                 XFRAC_D2, XFRAC_D3, XWGI_FULL,&
                                 XRUN_TOROUT, XDR_TOROUT,&
                                 LSTOCK_TOPD,NNB_STP_RESTART 
USE MODD_DUMMY_EXP_PROFILE,ONLY :XF_PARAM, XC_DEPTH_RATIO
USE MODD_TOPODYN,       ONLY : NNCAT, XMPARA, XCSTOPT, NMESHT, XDXT,&
                                 NNMC, XRTOP_D2, NNB_TOPD_STEP
!
USE MODD_SURF_PAR,         ONLY : XUNDEF, NUNDEF
USE MODD_ISBA_n,           ONLY : XSAND, XDG, XCLAY, XWG,&
                                  CKSAT, XCONDSAT,XWGI, XF_PARAM_i=>XF_PARAM, &
                                  XC_DEPTH_RATIO_i=>XC_DEPTH_RATIO
USE MODD_DIAG_EVAP_ISBA_n, ONLY : XAVG_RUNOFFC, XAVG_DRAINC

USE MODD_SURF_ATM_GRID_N,  ONLY : XMESH_SIZE
USE MODD_SURF_ATM_n,       ONLY : NSIZE_NATURE, NR_NATURE
!
! Interfaces
USE MODI_GET_LUOUT
USE MODI_READ_FILE_MASKTOPD
USE MODI_PACK_SAME_RANK
USE MODI_UNPACK_SAME_RANK
USE MODI_ISBA_TO_TOPD
USE MODI_RESTART_COUPL_TOPD
!
USE MODE_SOIL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*), INTENT(IN) :: HPROGRAM    ! 
INTEGER, INTENT(IN)          :: KI          ! Grid dimensions
!
!*      0.2    declarations of local variables
!
!REAL, DIMENSION(:), ALLOCATABLE   :: ZDTAV                            ! Averaged depth soil on TOP-LAT grid
REAL, DIMENSION(:), ALLOCATABLE   :: ZSAND_FULL, ZCLAY_FULL, ZDG_FULL ! Isba variables on the full domain
REAL, DIMENSION(:), ALLOCATABLE   :: ZFRAC    ! fraction of SurfEx mesh that covers one or several catchments
REAL, DIMENSION(:), ALLOCATABLE   :: ZDMAXAV  ! dificit maximal moyen par bassin
REAL, DIMENSION(:),ALLOCATABLE    :: ZSANDTOPI, ZCLAYTOPI!, ZWWILTTOPI !sand and clay fractions on TOPMODEL layers
!
!ludo
REAL, DIMENSION(:), ALLOCATABLE   :: ZKSAT       !ksat surf 
REAL, DIMENSION(:), ALLOCATABLE   :: ZF_PARAM_FULL
REAL, DIMENSION(:,:), ALLOCATABLE :: ZF_PARAMT!, ZWWILTTOPT
REAL, DIMENSION(:), ALLOCATABLE   :: ZDG2_FULL, ZDG3_FULL, ZWG2_FULL, ZWG3_FULL, ZRTOP_D2
REAL,DIMENSION(:), ALLOCATABLE    :: ZWGI_FULL, Z_WFCTOPI, Z_WSTOPI
!
REAL                              :: ZCOEF_ANIS  !coefficient anisotropie Ksat:
                                                 ! Ksat horiz=ZCOEF*Ksat vert
INTEGER                   :: JJ,JI           ! loop control 
INTEGER                   :: JCAT,JMESH      ! loop control 
INTEGER                   :: ILUOUT          ! Logical unit for output filr
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_COUPL_TOPD',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
WRITE(ILUOUT,*) 'INITIALISATION INIT_COUPL_TOPD'
!
ALLOCATE(NMASKT(NNCAT,NMESHT))
NMASKT(:,:) = NUNDEF
!
!*       1    Initialization:
!               ---------------
!
! la surface saturee, à l'initialisation est nulle, donc on initialise les lambdas de telle sorte qu'aucun pixel ne soit sature
ALLOCATE(XKA_PRE (NNCAT,NMESHT))
ALLOCATE(XKAC_PRE(NNCAT))
XKA_PRE(:,:) = 0.0
XKAC_PRE(:)  = MAXVAL(XKA_PRE) + 1.
!
!Cumulated runoff initialisation
ALLOCATE(XRUNOFF_TOP(NSIZE_NATURE))
XRUNOFF_TOP  (:) = XAVG_RUNOFFC(:)
!
IF(.NOT.ALLOCATED(XAVG_RUNOFFCM)) ALLOCATE(XAVG_RUNOFFCM(NSIZE_NATURE))
XAVG_RUNOFFCM(:) = XAVG_RUNOFFC(:)
!
IF(.NOT.ALLOCATED(XAVG_DRAINCM )) ALLOCATE(XAVG_DRAINCM (NSIZE_NATURE))
XAVG_DRAINCM (:) = XAVG_DRAINC(:)
!
!
! Reading masks
 CALL READ_FILE_MASKTOPD(KI)
!
!*      2.1     Fraction of SurfEx mesh with TOPMODEL
!               -------------------------------------
!
ALLOCATE(NNBV_IN_MESH  (KI,NNCAT))
ALLOCATE(XBV_IN_MESH   (KI,NNCAT))
ALLOCATE(XTOTBV_IN_MESH(KI))
!
XTOTBV_IN_MESH(:) = 0.0
!
DO JJ=1,KI
  !
  XBV_IN_MESH(JJ,:) = 0.0
  !
  DO JI=1,NNCAT
    NNBV_IN_MESH(JJ,JI) = COUNT( NMASKI(JJ,JI,:)/=NUNDEF )
    XBV_IN_MESH (JJ,JI) = REAL(NNBV_IN_MESH(JJ,JI)) * XDXT(JI)**2
    XTOTBV_IN_MESH (JJ) = XTOTBV_IN_MESH(JJ) + XBV_IN_MESH(JJ,JI)
  ENDDO
  !
ENDDO
!
!*      2.2     Fraction of SurfEx mesh with each catchment
!               -------------------------------------------
!
ALLOCATE(ZFRAC(KI))  ! fraction not covered by catchments
ZFRAC(:) = ( XMESH_SIZE(:)-XTOTBV_IN_MESH(:) ) / XMESH_SIZE(:)
ZFRAC(:) = MIN(MAX(ZFRAC(:),0.),1.)
!
ALLOCATE(XATOP(NSIZE_NATURE)) ! fraction covered by catchments part nature
 CALL PACK_SAME_RANK(NR_NATURE,(1.-ZFRAC),XATOP)
!
!
IF (HPROGRAM=='POST  ') GOTO 10
!
!*      3.0     Wsat, Wfc and depth for TOPODYN on ISBA grid
!               --------------------------------------------
!*      3.1     clay, sand fraction, depth hydraulic conductivity at saturation of the layer for TOPODYN
!               ---------------------------------------------------------
!
ALLOCATE(ZSAND_FULL(KI))
ALLOCATE(ZCLAY_FULL(KI))
 CALL UNPACK_SAME_RANK(NR_NATURE,XSAND(:,2),ZSAND_FULL)
 CALL UNPACK_SAME_RANK(NR_NATURE,XCLAY(:,2),ZCLAY_FULL)
!
!ludo prof variable pour tr lat (OK car sol homogene verticalement, faux sinon)
ALLOCATE(ZDG2_FULL(KI))
ALLOCATE(ZDG3_FULL(KI))
 CALL UNPACK_SAME_RANK(NR_NATURE,XDG(:,2,1),ZDG2_FULL)
 CALL UNPACK_SAME_RANK(NR_NATURE,XDG(:,3,1),ZDG3_FULL)
!
ALLOCATE(ZRTOP_D2(KI))
ZRTOP_D2(:) = 0.
!
DO JMESH=1,KI
  IF ( ZDG2_FULL(JMESH)/=XUNDEF .AND. ZFRAC(JMESH)<1. ) THEN
    ZRTOP_D2(JMESH) = 0.
    DO JCAT=1,NNCAT
     !moyenne ponderee pour cas ou plusieurs BV sur maille
      ZRTOP_D2(JMESH) = ZRTOP_D2(JMESH) + XRTOP_D2(JCAT)*XBV_IN_MESH(JMESH,JCAT)/XMESH_SIZE(JMESH)    
    END DO
  ENDIF   
ENDDO
!ZTOP_D2 * D2 < D3 : the depth concernet by lateral transfers is lower than D2
WHERE( ZDG2_FULL/=XUNDEF .AND. ZRTOP_D2*ZDG2_FULL>ZDG3_FULL ) ZRTOP_D2(:) = ZDG3_FULL(:)/ZDG2_FULL(:)
!
DEALLOCATE(ZFRAC)
!
ALLOCATE(XFRAC_D2 (KI))
ALLOCATE(XFRAC_D3 (KI))
XFRAC_D2(:)=1.
XFRAC_D3(:)=0.
WHERE( ZDG2_FULL/=XUNDEF .AND. ZRTOP_D2*ZDG2_FULL>ZDG2_FULL  ) ! if the depth is > D2
  XFRAC_D2(:) = MIN(1.,ZRTOP_D2(:))
  XFRAC_D3(:) = (ZRTOP_D2(:)*ZDG2_FULL(:)-ZDG2_FULL(:)) / (ZDG3_FULL(:)-ZDG2_FULL(:))
  XFRAC_D3(:) = MAX(0.,XFRAC_D3(:))
END WHERE
!
ALLOCATE(ZDG_FULL(KI))
WHERE (ZDG2_FULL/=XUNDEF)
  ZDG_FULL = XFRAC_D2*ZDG2_FULL + XFRAC_D3*(ZDG3_FULL-ZDG2_FULL)
ELSEWHERE
  ZDG_FULL = XUNDEF
END WHERE
!
ALLOCATE(ZSANDTOPI(KI))
ALLOCATE(ZCLAYTOPI(KI))
ZSANDTOPI(:)=0.0
ZCLAYTOPI(:)=0.0
ALLOCATE(XDTOPI(KI))
XDTOPI(:)=0.0
WHERE ( ZDG_FULL/=XUNDEF .AND. ZDG_FULL/=0. )
  XDTOPI = ZDG_FULL
  ZSANDTOPI = ZSANDTOPI + ZSAND_FULL * ZDG_FULL
  ZCLAYTOPI = ZCLAYTOPI + ZCLAY_FULL * ZDG_FULL
  ZSANDTOPI = ZSANDTOPI / XDTOPI
  ZCLAYTOPI = ZCLAYTOPI / XDTOPI
ELSEWHERE
  ZSANDTOPI = XUNDEF
  ZCLAYTOPI = XUNDEF
  XDTOPI = XUNDEF
END WHERE
DEALLOCATE(ZSAND_FULL)
DEALLOCATE(ZCLAY_FULL)
!
!*      4.1     depth of the Isba layer on TOP-LAT grid
!               ---------------------------------------
!
ALLOCATE(XDTOPT(NNCAT,NMESHT))
XDTOPT(:,:) = 0.0
 CALL ISBA_TO_TOPD(XDTOPI,XDTOPT)
!
!*      3.2     Wsat and Wfc on TOPODYN layer
!               -----------------------------
!
ALLOCATE(XWSTOPI   (KI))
ALLOCATE(XWFCTOPI  (KI))
XWSTOPI (:) = 0.0
XWFCTOPI(:) = 0.0
!ALLOCATE(ZWWILTTOPI(KI))
XWSTOPI    = WSAT_FUNC_1D (ZCLAYTOPI,ZSANDTOPI,'CH78')
XWFCTOPI   = WFC_FUNC_1D  (ZCLAYTOPI,ZSANDTOPI,'CH78')
!ZWWILTTOPI = WWILT_FUNC_1D(ZCLAYTOPI,ZSANDTOPI,'CH78')
!
!modif ludo test ksat exp
WRITE(ILUOUT,*) 'CKSAT==',CKSAT

ALLOCATE(ZKSAT(KI))
ZKSAT   (:) = 0.0
ALLOCATE(XCSTOPI(KI))
XCSTOPI(:) = 0.0
IF( CKSAT=='SGH' .OR. CKSAT=='EXP' ) THEN
  !
  !ludo calcul des profondeur efficaces
  !ZRTOP_D2(:) = 1.
  !ALLOCATE(XC_DEPTH_RATIO(SIZE(XC_DEPTH_RATIO_i)))
  !XC_DEPTH_RATIO(:) = XC_DEPTH_RATIO_i(:)
  !ZRTOP_D2(:) = XC_DEPTH_RATIO(:)
  !valeur patch 1 (idem wsat wfc) a voir cas ou il existe plusieurs patchs 
  CALL UNPACK_SAME_RANK(NR_NATURE,XCONDSAT(:,1,1),ZKSAT)
  !passage de definition Ksat(profondeur) en Ksat(deficit)
  WHERE ( ZDG_FULL/=XUNDEF .AND. (XWSTOPI-XWFCTOPI/=0.) )
    XCSTOPI(:) = ZKSAT(:) / (XWSTOPI(:)-XWFCTOPI(:))
  END WHERE
  !
ELSE
  !
  XCSTOPI(:) = HYDCONDSAT_FUNC(ZCLAYTOPI,ZSANDTOPI,'CH78')
  !passage de definition Ksat(profondeur) en Ksat(deficit)
  WHERE ( ZDG_FULL/=XUNDEF .AND. (XWSTOPI-XWFCTOPI/=0.) )
    XCSTOPI(:) = XCSTOPI(:) / (XWSTOPI(:)-XWFCTOPI(:))
  END WHERE
  !
ENDIF
!
DEALLOCATE(ZSANDTOPI)
DEALLOCATE(ZCLAYTOPI)
DEALLOCATE(ZRTOP_D2)
!
!*      4.3     Ko on TOP-LAT grid
!               ------------------
!
ALLOCATE(XCSTOPT(NNCAT,NMESHT))
 CALL ISBA_TO_TOPD(XCSTOPI,XCSTOPT)
WHERE (XCSTOPT == XUNDEF) XCSTOPT = 0.0
!
!*      3.3     Initialization of the previous time step water storage on ISBA grid to calculate the refill on Isba grid
!               -------------------------------------------------------------------------
!
ALLOCATE(ZWG2_FULL(KI))
ALLOCATE(ZWG3_FULL(KI))
 CALL UNPACK_SAME_RANK(NR_NATURE,XWG(:,2,1),ZWG2_FULL)
 CALL UNPACK_SAME_RANK(NR_NATURE,XWG(:,3,1),ZWG3_FULL)
!
ALLOCATE(XWG_FULL(KI))
WHERE ( ZWG2_FULL/=XUNDEF .AND. ZDG_FULL/=0. )
  XWG_FULL = ( XFRAC_D2*ZDG2_FULL*ZWG2_FULL + XFRAC_D3*(ZDG3_FULL-ZDG2_FULL)*ZWG3_FULL ) / ZDG_FULL
ELSEWHERE
  XWG_FULL = XUNDEF
END WHERE
!
DEALLOCATE(ZWG2_FULL)
DEALLOCATE(ZWG3_FULL)
DEALLOCATE(ZDG3_FULL)
!
!
ALLOCATE(XWTOPT(NNCAT,NMESHT))
XWTOPT(:,:) = 0.0
 CALL ISBA_TO_TOPD(XWG_FULL,XWTOPT)
WHERE (XWTOPT == XUNDEF) XWTOPT = 0.0
!
!ludo prise en compte glace (pas de glace dans 3e couche)
ALLOCATE(ZWGI_FULL(KI))
ALLOCATE(XWGI_FULL(KI))
 CALL UNPACK_SAME_RANK(NR_NATURE,XWGI(:,2,1),ZWGI_FULL)
!
WHERE ( ZWGI_FULL/=XUNDEF .AND. XFRAC_D2>0. .AND. ZDG_FULL/=0. )
  XWGI_FULL = (XFRAC_D2*ZDG2_FULL*ZWGI_FULL) / ZDG_FULL
ELSEWHERE
  XWGI_FULL = XUNDEF
END WHERE
!
DEALLOCATE(ZWGI_FULL)
DEALLOCATE(ZDG2_FULL)
DEALLOCATE(ZDG_FULL)
!
ALLOCATE(Z_WFCTOPI(KI))
ALLOCATE(Z_WSTOPI (KI))
!test reservoir top=eau+glace -> pas de modif Wsat et Wfc
WHERE ( XWGI_FULL/=XUNDEF .AND. XWSTOPI/=0. )
  Z_WSTOPI  = XWSTOPI - XWGI_FULL
  Z_WFCTOPI = XWFCTOPI * Z_WSTOPI / XWSTOPI
END WHERE
!
!ludo calcul en fct teneur glace
!
ALLOCATE(XWSTOPT (NNCAT,NMESHT))
ALLOCATE(XWFCTOPT(NNCAT,NMESHT))
 CALL ISBA_TO_TOPD(Z_WSTOPI,XWSTOPT)
 CALL ISBA_TO_TOPD(Z_WFCTOPI,XWFCTOPT)
DEALLOCATE(Z_WSTOPI)
DEALLOCATE(Z_WFCTOPI)
!
!*      4.0     calcul of time constant variables on TOPODYN grid 
!               -------------------------------------------------
!
!*      4.2     Wsat and Wfc on TOP-LAT grid
!               ----------------------------
!
ALLOCATE(XDMAXFC(NNCAT,NMESHT))
XDMAXFC(:,:) = XUNDEF
WHERE (XWFCTOPT /= XUNDEF) XDMAXFC = (XWSTOPT - XWFCTOPT) * XDTOPT ! (m)
!
!
!*      4.4     Initialisation of the previous time step water storage on topodyn-lat grid
!               --------------------------------------------------------------------------
!*      4.5     M parameter on TOPODYN grid
!               ------------------------
!*      4.5.1   Mean depth soil on catchment
!
ALLOCATE(XMPARA (NNCAT))
XMPARA  (:) = 0.0
!
IF( CKSAT=='EXP' .OR. CKSAT=='SGH' ) THEN
  !ludo test
  ALLOCATE(ZF_PARAM_FULL(KI))
  ALLOCATE(ZF_PARAMT(NNCAT,NMESHT))
  ALLOCATE(XF_PARAM(SIZE(XF_PARAM_i)))
  XF_PARAM(:) = XF_PARAM_i(:)
  CALL UNPACK_SAME_RANK(NR_NATURE,XF_PARAM(:),ZF_PARAM_FULL)
  CALL ISBA_TO_TOPD(ZF_PARAM_FULL,ZF_PARAMT)
  DEALLOCATE(ZF_PARAM_FULL)
  !
  !passage de f a M (M=Wsat-Wfc/f)
  !ludo test ksat exp
  !ALLOCATE(ZWWILTTOPT(NNCAT,NMESHT))
  !CALL ISBA_TO_TOPD(ZWWILTTOPI,ZWWILTTOPT)
  WHERE( ZF_PARAMT/=XUNDEF .AND. ZF_PARAMT/=0. ) ZF_PARAMT = (XWSTOPT-XWFCTOPT)/ZF_PARAMT
  !DEALLOCATE(ZWWILTTOPT)
  !
  DO JJ=1,NNCAT
    XMPARA(JJ) = SUM(ZF_PARAMT(JJ,:),MASK=ZF_PARAMT(JJ,:)/=XUNDEF) / NNMC(JJ)
  ENDDO
  !
  ZCOEF_ANIS = 1.
  XCSTOPT = XCSTOPT*ZCOEF_ANIS
  !
  DEALLOCATE(ZF_PARAMT)
  !
ELSE
  !
  ALLOCATE(ZDMAXAV(NNCAT))
  ZDMAXAV(:) = 0.0
  DO JJ=1,NNCAT
    ZDMAXAV(JJ) = SUM( XDMAXFC(JJ,:),MASK=XDMAXFC(JJ,:)/=XUNDEF ) / NNMC(JJ)
  ENDDO
  !
  !ALLOCATE(ZDTAV  (NNCAT))
  !ZDTAV   (:) = 0.0
  DO JJ=1,NNCAT 
    !ZDTAV(JJ) = SUM(XDTOPT(JJ,:),MASK=XDTOPT(JJ,:)/=XUNDEF) / NNMC(JJ)
    XMPARA(JJ) = ZDMAXAV(JJ) / 4.
  ENDDO
  !DEALLOCATE(ZDTAV)
  DEALLOCATE(ZDMAXAV)
  !
ENDIF
!
!DEALLOCATE(ZWWILTTOPI)
! 
!*      5.0      Initial saturated area computation
!               -----------------------------------------------------------
!
ALLOCATE(XAS_NATURE(NSIZE_NATURE))
XAS_NATURE(:) = 0.0
!
!*      6.0     Stock management in case of restart
!               -----------------------------------------------------------
!
10 CONTINUE
!
!stock
ALLOCATE(XRUN_TOROUT(NNCAT,NNB_TOPD_STEP+NNB_STP_RESTART))
ALLOCATE(XDR_TOROUT (NNCAT,NNB_TOPD_STEP+NNB_STP_RESTART))
XRUN_TOROUT(:,:) = 0.
XDR_TOROUT (:,:) = 0.
!
IF (HPROGRAM=='POST  ') GOTO 20
!
IF (LSTOCK_TOPD) CALL RESTART_COUPL_TOPD(HPROGRAM,KI)
!
!*      7.0     deallocate
!               ----------
!
20 CONTINUE
!
IF (LHOOK) CALL DR_HOOK('INIT_COUPL_TOPD',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_COUPL_TOPD







