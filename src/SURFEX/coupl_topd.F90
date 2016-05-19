!-----------------------------------------------------------------
!     #####################
      SUBROUTINE COUPL_TOPD(HPROGRAM,HSTEP,KI,KSTEP)
!     #####################
!
!!****  *COUPL_TOPD*  
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
!!      Original   15/10/2003
!!      09/2007 : New organisation of exfiltration, computation of saturated
!!                area, routing.
!!                Soil ice content taken into account
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TOPODYN,        ONLY : NNCAT, NMESHT, NNMC, XMPARA
USE MODD_COUPLING_TOPD,  ONLY : XWG_FULL, XDTOPI, XKAC_PRE, XDTOPT, XWTOPT, XWSTOPT, XAS_NATURE,&
                                  XKA_PRE, NMASKT, XWSUPSAT,&
                                  XRUNOFF_TOP, XATOP, XWFCTOPI, NNPIX,&
                                  XFRAC_D2, XFRAC_D3, XWSTOPI, XDMAXFC, XWFCTOPT, XWGI_FULL,&
                                  NFREQ_MAPS_ASAT, XAVG_RUNOFFCM, XAVG_DRAINCM
!
USE MODD_ISBA_n,           ONLY : XWG, XDG, XWGI
USE MODD_CSTS,             ONLY : XRHOLW, XRHOLI
USE MODD_DIAG_EVAP_ISBA_n, ONLY : XAVG_RUNOFFC, XAVG_DRAINC
USE MODD_SURF_ATM_n,       ONLY : NR_NATURE
USE MODD_SURF_ATM_GRID_n,  ONLY : XMESH_SIZE
USE MODD_SURF_PAR,         ONLY : XUNDEF, NUNDEF
USE MODD_ISBA_PAR,         ONLY : XWGMIN
!
USE MODI_GET_LUOUT
USE MODI_UNPACK_SAME_RANK
USE MODI_PACK_SAME_RANK
USE MODI_ISBA_TO_TOPD
USE MODI_RECHARGE_SURF_TOPD
USE MODI_TOPODYN_LAT
USE MODI_SAT_AREA_FRAC
USE MODI_TOPD_TO_ISBA
USE MODI_DIAG_ISBA_TO_ROUT
USE MODI_ISBA_TO_TOPDSAT
USE MODI_ROUTING
USE MODI_OPEN_FILE
USE MODI_WRITE_FILE_ISBAMAP
USE MODI_CLOSE_FILE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! program calling surf. schemes
 CHARACTER(LEN=*), INTENT(IN) :: HSTEP  ! atmospheric loop index
INTEGER, INTENT(IN)          :: KI    ! Grid dimensions
INTEGER, INTENT(IN)          :: KSTEP ! current time step 
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(XRUNOFF_TOP,1)) :: ZWG2_TMP,ZWG3_TMP
REAL, DIMENSION(SIZE(XRUNOFF_TOP,1)) :: ZWSAT_NAT
REAL, DIMENSION(NNCAT,NMESHT) :: ZRT             ! recharge on TOP-LAT grid (m)
REAL, DIMENSION(NNCAT,NMESHT) :: ZDEFT           ! local deficits on TOPODYN grid (m)
REAL, DIMENSION(NNCAT,NMESHT) :: ZRI_WGIT        ! water changing of phase on TOPMODEL grid
REAL, DIMENSION(NNCAT,NMESHT) :: ZRUNOFF_TOPD    ! Runoff on the Topodyn grid (m3/s)
REAL, DIMENSION(NNCAT,NMESHT) :: ZDRAIN_TOPD     ! Drainage from Isba on Topodyn grid (m3/s)
REAL, DIMENSION(NNCAT,NMESHT) :: ZKAPPA          ! topographic index
REAL, DIMENSION(NNCAT)        :: ZKAPPAC         ! critical topographic index
REAL, DIMENSION(KI)           :: ZRI             ! recharge on ISBA grid (m)
REAL, DIMENSION(KI)           :: ZRI_WGI         ! water changing of phase on ISBA grid
REAL, DIMENSION(KI)           :: Z_WSTOPI, Z_WFCTOPI
REAL, DIMENSION(KI)           :: ZWM             ! Water content on SurfEx grid after the previous topodyn time step
REAL, DIMENSION(KI)           :: ZWGIM           ! soil ice content at previous time step
REAL, DIMENSION(KI)           :: ZRUNOFFC_FULL   ! Cumulated runoff from isba on the full domain (kg/m2)
REAL, DIMENSION(KI)           :: ZRUNOFFC_FULLM  ! Cumulated runoff from isba on the full domain (kg/m2) at t-dt
REAL, DIMENSION(KI)           :: ZRUNOFF_ISBA    ! Runoff from Isba (kg/m2)
REAL, DIMENSION(KI)           :: ZDRAINC_FULL    ! Cumulated drainage from Isba on the full domain (kg/m2)
REAL, DIMENSION(KI)           :: ZDRAINC_FULLM   ! Cumulated drainage from Isba on the full domain (kg/m2) at t-dt
REAL, DIMENSION(KI)           :: ZDRAIN_ISBA     ! Drainage from Isba (m3/s)
REAL, DIMENSION(KI)           :: ZDG_FULL
REAL, DIMENSION(KI)           :: ZWG2_FULL, ZWG3_FULL, ZDG2_FULL, ZDG3_FULL
REAL, DIMENSION(KI)           :: ZWGI_FULL
REAL, DIMENSION(KI)           :: ZAS             ! Saturated area fraction for each Isba meshes
REAL, DIMENSION(NNCAT)        :: Z_DW1,Z_DW2     ! Wsat-Wfc to actualise M in fonction of WI
REAL                          :: ZAVG_MESH_SIZE
LOGICAL, DIMENSION(NNCAT)     :: GTOPD           ! logical variable = true if topodyn_lat runs
INTEGER                       :: JJ,JI           ! loop control 
INTEGER                       :: IUNIT           ! unit number of results files
INTEGER                       :: ILUOUT          ! unit number of listing file
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('COUPL_TOPD',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*       0.     Initialization:
!               ---------------
!
ZWM(:) = XWG_FULL(:)
ZWGIM(:) = XWGI_FULL(:)
ZWG2_TMP(:) = XWG(:,2,1)
ZWG3_TMP(:) = XWG(:,3,1)
!
!*       1.     ISBA => TOPODYN
!               ---------------
!*       1.1    Computation of the useful depth and water for lateral transfers
!               -----------------------------------
!
 CALL UNPACK_SAME_RANK(NR_NATURE,XDG(:,2,1),ZDG2_FULL)
 CALL UNPACK_SAME_RANK(NR_NATURE,XDG(:,3,1),ZDG3_FULL)
WHERE ( ZDG2_FULL/=XUNDEF )
  ZDG_FULL = XFRAC_D2*ZDG2_FULL + XFRAC_D3*(ZDG3_FULL-ZDG2_FULL)
ELSEWHERE
  ZDG_FULL = XUNDEF
END WHERE
!
 CALL UNPACK_SAME_RANK(NR_NATURE,XWG(:,2,1),ZWG2_FULL)
 CALL UNPACK_SAME_RANK(NR_NATURE,XWG(:,3,1),ZWG3_FULL)
WHERE ( ZDG_FULL/=XUNDEF .AND. ZDG_FULL/=0. )
  XWG_FULL = XFRAC_D2*(ZDG2_FULL/ZDG_FULL)*ZWG2_FULL + XFRAC_D3*((ZDG3_FULL-ZDG2_FULL)/ZDG_FULL)*ZWG3_FULL
ELSEWHERE
  XWG_FULL = XUNDEF
END WHERE
!
!ludo prise en compte glace (pas de glace dans 3e couche)
 CALL UNPACK_SAME_RANK(NR_NATURE,XWGI(:,2,1),ZWGI_FULL)
WHERE ( ZWGI_FULL/=XUNDEF .AND. XFRAC_D2>0 .AND. ZDG_FULL/=0. )
  XWGI_FULL = XFRAC_D2*(ZDG2_FULL/ZDG_FULL)*ZWGI_FULL
ELSEWHERE
  XWGI_FULL = XUNDEF
END WHERE
!
!
WHERE ( XDTOPI/=XUNDEF ) 
  ZRI_WGI = ( XWGI_FULL - ZWGIM ) !m3/m3
ELSEWHERE
  ZRI_WGI = 0.0
END WHERE
!
 CALL ISBA_TO_TOPD(ZRI_WGI,ZRI_WGIT)
!
!*       1.2    Water recharge 
!               ---------------
! Topodyn uses :
! - a water recharge = water added since last time step to compute hydrological similarity indexes
! - the total water content to compute a deficit
!
! This recharge is computed without regarding the changing of phase of water
! and the lateral transfers are performed regarding wsat et Wfc of last time step
!
WHERE ( XDTOPI/=XUNDEF )
  ZRI = ( (XWG_FULL - ZWM) + ZRI_WGI ) * XDTOPI
ELSEWHERE
  ZRI = 0.0
ENDWHERE
!
! The water recharge on ISBA grid is computed on TOPMODEL grid
 CALL RECHARGE_SURF_TOPD(ZRI,ZRT,KI)
!
!*       2.     Lateral distribution
!               --------------------
!*       2.1    Computation of local deficits on TOPODYN grid
!               ----------------------------------------
!
 CALL TOPODYN_LAT(ZRT(:,:),ZDEFT(:,:),ZKAPPA(:,:),ZKAPPAC(:),GTOPD)
!
!*       2.2    Computation of contributive area on ISBA grid
!               ----------------------------------------
!
 CALL SAT_AREA_FRAC(ZDEFT,ZAS)
!
 CALL PACK_SAME_RANK(NR_NATURE,ZAS,XAS_NATURE)
!
!*       3.    Deficit (m) -> water storage (m3/m3) and changing of phase
!               ------------------------------------
!ancien Wsat-Wfc pour actualisation param M
Z_DW1(:)=0
DO JJ=1,NNCAT
  Z_DW1(JJ) = SUM( XWSTOPT(JJ,:)-XWFCTOPT(JJ,:), MASK=XWSTOPT(JJ,:)/=XUNDEF ) / NNMC(JJ)
ENDDO
!
DO JJ=1,NNCAT
  WHERE ( XDTOPT(JJ,:)/=XUNDEF .AND. XDTOPT(JJ,:)/=0. )
    XWTOPT(JJ,:) = XWSTOPT(JJ,:) - ( ZDEFT(JJ,:) / XDTOPT(JJ,:) )      
    !changing phase
    XWTOPT(JJ,:) = XWTOPT(JJ,:) - ZRI_WGIT(JJ,:)
  END WHERE
ENDDO
WHERE (XWTOPT > XWSTOPT ) XWTOPT = XWSTOPT
!
!actualisation Wsat, Wfc et Dmax pour prochain pas de temps
WHERE ( ZWG2_FULL/=XUNDEF .AND. XWSTOPI/=0. )
  Z_WSTOPI  = XWSTOPI - XWGI_FULL
  Z_WFCTOPI = XWFCTOPI * Z_WSTOPI / XWSTOPI
END WHERE
 CALL ISBA_TO_TOPD(Z_WSTOPI,XWSTOPT)
 CALL ISBA_TO_TOPD(Z_WFCTOPI,XWFCTOPT)
!
!ludo test empeche erreur num chgt phase
WHERE ( ABS(XWSTOPT-XWTOPT) < 0.0000000001 ) XWSTOPT = XWTOPT
!
WHERE ( XWTOPT>XWSTOPT ) XWTOPT = XWSTOPT
!
WHERE ( XWFCTOPT/= XUNDEF ) XDMAXFC = (XWSTOPT - XWFCTOPT) * XDTOPT ! (m)
!
!actualisation M-> M=M*rapport Wsat-Wfc
Z_DW2(:)=0
DO JJ=1,NNCAT
  Z_DW2(JJ) = SUM( XWSTOPT(JJ,:)-XWFCTOPT(JJ,:), MASK=XWSTOPT(JJ,:)/=XUNDEF) / NNMC(JJ)
ENDDO
!
XMPARA(:) = XMPARA(:) * Z_DW2(:)/Z_DW1(:)
!
!*       4.     TOPODYN => ISBA
!               ---------------
!*       4.1    Calculation of water storage on ISBA grid
!               -----------------------------------------
!
 CALL TOPD_TO_ISBA(KI,KSTEP,GTOPD)
 CALL PACK_SAME_RANK(NR_NATURE, (1-XFRAC_D2)*ZWG2_FULL + XFRAC_D2*XWG_FULL, XWG(:,2,1))
 CALL PACK_SAME_RANK(NR_NATURE, (1-XFRAC_D3)*ZWG3_FULL + XFRAC_D3*XWG_FULL, XWG(:,3,1))
!
!*       4.2    Budget correction
!  -----------------------------------------
!
ZAVG_MESH_SIZE = SUM(XMESH_SIZE(:)) / SIZE(XMESH_SIZE,1)
!
 CALL BUDG_WG(ZWG2_TMP(:),XWG(:,2,1),XDG(:,2,1),XMESH_SIZE,ZAVG_MESH_SIZE)
!   
 CALL BUDG_WG(ZWG3_TMP(:),XWG(:,3,1),XDG(:,3,1)-XDG(:,2,1),XMESH_SIZE,ZAVG_MESH_SIZE)
!
!*      5.0    Total discharge
!              ---------------
!
!*      5.1    Total water for runoff on TOPODYN grid
!              ---------------------------------------
!
 CALL PACK_SAME_RANK(NR_NATURE,XWSUPSAT,ZWSAT_NAT)
WHERE ( ZWSAT_NAT(:)<10E-10 ) 
  ZWSAT_NAT(:) = 0.
ELSEWHERE( ZWSAT_NAT(:)/=XUNDEF ) 
  XRUNOFF_TOP(:) = XRUNOFF_TOP(:) + ZWSAT_NAT(:)*XRHOLW*XDG(:,2,1)
ENDWHERE
!
!write(*,*) ' ds coupl_topd ZWSAT_NAT',SUM(ZWSAT_NAT,MASK=ZWSAT_NAT(:)/=XUNDEF)!
 CALL UNPACK_SAME_RANK(NR_NATURE,XRUNOFF_TOP,ZRUNOFFC_FULL)
 CALL UNPACK_SAME_RANK(NR_NATURE,XAVG_RUNOFFCM,ZRUNOFFC_FULLM)
!
 CALL DIAG_ISBA_TO_ROUT(ZRUNOFFC_FULL,ZRUNOFFC_FULLM,ZRUNOFF_ISBA)
!
XAVG_RUNOFFCM(:) = XRUNOFF_TOP(:)
!
WHERE (ZRUNOFF_ISBA==XUNDEF) ZRUNOFF_ISBA = 0.
!
ZRUNOFF_TOPD(:,:) = 0
!
 CALL ISBA_TO_TOPDSAT(XKA_PRE,XKAC_PRE,KI,ZRUNOFF_ISBA,ZRUNOFF_TOPD)
!
!
!*      5.2    Total water for drainage on TOPODYN grid
!              ----------------------------------------
!
 CALL UNPACK_SAME_RANK(NR_NATURE,XAVG_DRAINC*XATOP,ZDRAINC_FULL)
 CALL UNPACK_SAME_RANK(NR_NATURE,XAVG_DRAINCM*XATOP,ZDRAINC_FULLM)
!
 CALL DIAG_ISBA_TO_ROUT(ZDRAINC_FULL,ZDRAINC_FULLM,ZDRAIN_ISBA)
!
WHERE (ZDRAIN_ISBA==XUNDEF) ZDRAIN_ISBA=0.
!
XAVG_DRAINCM(:)  = XAVG_DRAINC(:)
!
ZDRAIN_TOPD(:,:) = 0.0
!
 CALL ISBA_TO_TOPD(ZDRAIN_ISBA,ZDRAIN_TOPD)
!
DO JJ=1,NNCAT
  DO JI=1,NNMC(JJ)
    IF (NMASKT(JJ,JI)/=NUNDEF) &
      ZDRAIN_TOPD(JJ,JI) = ZDRAIN_TOPD(JJ,JI) / NNPIX(NMASKT(JJ,JI))
  ENDDO
ENDDO   
!
!*      6    Routing (runoff + drainage + exfiltration)
!
 CALL ROUTING(ZRUNOFF_TOPD,ZDRAIN_TOPD,KSTEP)
!
XKA_PRE(:,:) = ZKAPPA(:,:)
XKAC_PRE(:) = ZKAPPAC(:)
!
!*      7.0    Writing results in map files
!              ----------------------------
!
IF (NFREQ_MAPS_ASAT/=0.AND.MOD(KSTEP,NFREQ_MAPS_ASAT)==0) THEN
  CALL OPEN_FILE('ASCII ',IUNIT,HFILE='carte_surfcont'//HSTEP,HFORM='FORMATTED',HACTION='WRITE')
  CALL WRITE_FILE_ISBAMAP(IUNIT,ZAS,KI)
  CALL CLOSE_FILE('ASCII ',IUNIT)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('COUPL_TOPD',1,ZHOOK_HANDLE)
!
CONTAINS
!
SUBROUTINE BUDG_WG(PWGM,PWG,PDG,PMESH_SIZE,PAVG_MESH_SIZE)
!
REAL, DIMENSION(:), INTENT(IN) :: PWGM
REAL, DIMENSION(:), INTENT(INOUT) :: PWG
REAL, DIMENSION(:), INTENT(IN) :: PDG
REAL, DIMENSION(:), INTENT(IN) :: PMESH_SIZE
REAL, INTENT(IN) :: PAVG_MESH_SIZE
!
REAL :: ZSTOCK_WGM, ZSTOCK_WG
REAL :: ZAVG_DG, ZBUDG_WG
REAL :: ZTMP, ZTMP2
INTEGER :: JMESH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('COUPL_TOPD:BUDG_WG',0,ZHOOK_HANDLE)
!
ZSTOCK_WGM = SUM( PWGM(:)*PDG(:)*PMESH_SIZE(:) )    ! water stocked in the ground (m3)
ZSTOCK_WG  = SUM( PWG (:)*PDG(:)*PMESH_SIZE(:) )    ! water stocked in the layer2 (m3)
ZAVG_DG = SUM(PDG(:)) / SIZE(PDG)
ZBUDG_WG = ( ZSTOCK_WG - ZSTOCK_WGM )/ ZAVG_DG / PAVG_MESH_SIZE
!ZTMP2=0.
!DO JMESH=1,SIZE(PWG)             
! IF ((PWG(JMESH)/=XUNDEF).AND.(PWGM(JMESH)/=XUNDEF)) THEN
!   IF(PWG(JMESH)/=PWGM(JMESH)) ZTMP2=ZTMP2+1.
! ENDIF
!ENDDO
!
ZTMP  = COUNT( PWG(:)/=PWGM(:) )
ZTMP2 = COUNT( PWG(:)/=PWGM(:) .AND. PWG(:)>XWGMIN+(ZBUDG_WG/ZTMP) )
!   
DO JMESH=1,SIZE(PWG)
  IF( PWG(JMESH)/=PWGM(JMESH) .AND. ZTMP2/=0. .AND. PWG(JMESH)>XWGMIN+(ZBUDG_WG/ZTMP) ) THEN
    !IF( PWG(JMESH)/=PWGM(JMESH) .AND. (ZTMP2/=0. ) THEN
    PWG(JMESH) = PWG(JMESH) - (ZBUDG_WG/ZTMP2)
    !PWG(JMESH) = MAX(PWG(JMESH) - (ZBUDG_WG/ZTMP2),XWGMIN)
  ENDIF
ENDDO
!write(*,*) KSTEP,COUNT(PWG(:)<XWGMIN),COUNT(PWG(:)<0.)
!
IF (LHOOK) CALL DR_HOOK('COUPL_TOPD:BUDG_WG',1,ZHOOK_HANDLE)
!
END SUBROUTINE BUDG_WG
!
END SUBROUTINE COUPL_TOPD
