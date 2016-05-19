!     #########
SUBROUTINE DIAG_SURF_ATM_n(HPROGRAM)
!     #################################################################################
!
!!****  *DIAG_SURF_ATM_n * - Chooses the surface schemes for diagnostics
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
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified    01/2006 : sea flux parameterization.
!!      Modified    08/2008 : cumulated fluxes
!!------------------------------------------------------------------
!

!
USE MODD_SURF_CONF,      ONLY : CPROGNAME
USE MODD_SURF_ATM_n,     ONLY : XSEA, XTOWN, XNATURE, XWATER, TTIME,              &
                                  NSIZE_SEA, NSIZE_TOWN, NSIZE_NATURE, NSIZE_WATER, &
                                  NDIM_SEA,  NDIM_TOWN,  NDIM_NATURE,  NDIM_WATER,  &
                                  NR_SEA, NR_TOWN, NR_NATURE, NR_WATER  
USE MODD_SURF_ATM_n,     ONLY : XZS
USE MODD_SURF_ATM_SSO_n, ONLY : XMIN_ZS
USE MODD_DATA_COVER_PAR, ONLY : NTILESFC
USE MODD_DIAG_SURF_ATM_n,ONLY : N2M, L2M_MIN_ZS, LSURF_BUDGET, LCOEF, LSURF_VARS,&
                                  XRN_TILE, XH_TILE, XLE_TILE, XGFLUX_TILE,      &
                                  XRI_TILE, XCD_TILE, XCH_TILE, XCE_TILE,        &
                                  XT2M_TILE, XTS_TILE, XQ2M_TILE, XHU2M_TILE,    &
                                  XZON10M_TILE, XMER10M_TILE, XLEI_TILE,         &
                                  XQS_TILE, XZ0_TILE, XZ0H_TILE, XT2M_MIN_TILE,  &
                                  XT2M_MAX_TILE,                                 &
                                  XSWD_TILE, XSWU_TILE, XLWD_TILE, XLWU_TILE,    &
                                  XSWBD_TILE, XSWBU_TILE, XFMU_TILE, XFMV_TILE,  &
                                  XAVG_RN, XAVG_H, XAVG_LE, XAVG_GFLUX,          &
                                  XAVG_RI, XAVG_CD, XAVG_CH, XAVG_CE,            &
                                  XAVG_T2M, XAVG_TS, XAVG_Q2M, XAVG_HU2M,        &
                                  XAVG_T2M_MIN_ZS,XAVG_Q2M_MIN_ZS,               &
                                  XAVG_HU2M_MIN_ZS, XAVG_ZON10M, XAVG_MER10M,    &
                                  XAVG_QS, XAVG_Z0, XAVG_Z0H, XAVG_LEI,          &
                                  XDIAG_UREF, XDIAG_ZREF,                        &
                                  XAVG_SWD, XAVG_SWU, XAVG_LWD, XAVG_LWU,        &
                                  XAVG_SWBD, XAVG_SWBU, XAVG_FMU, XAVG_FMV,      &
                                  XPS, XRHOA, LSURF_BUDGETC,                     &
                                  XRNC_TILE, XHC_TILE, XLEC_TILE, XGFLUXC_TILE,  &
                                  XSWDC_TILE, XSWUC_TILE, XLWDC_TILE, XLWUC_TILE,&
                                  XFMUC_TILE, XFMVC_TILE, XLEIC_TILE,            &
                                  XAVG_RNC, XAVG_HC, XAVG_LEC, XAVG_GFLUXC,      &
                                  XAVG_SWDC, XAVG_SWUC, XAVG_LWDC, XAVG_LWUC,    &
                                  XAVG_FMUC, XAVG_FMVC, XAVG_T2M_MIN,            &
                                  XAVG_T2M_MAX, XAVG_LEIC, XHU2M_MIN_TILE,       &
                                  XHU2M_MAX_TILE, XAVG_HU2M_MIN, XAVG_HU2M_MAX,  &
                                  XWIND10M_TILE, XWIND10M_MAX_TILE,              &
                                  XAVG_WIND10M, XAVG_WIND10M_MAX  
!
USE MODI_DIAG_NATURE_n 
USE MODI_DIAG_SEA_n 
USE MODI_DIAG_INLAND_WATER_n 
USE MODI_DIAG_TOWN_n 
USE MODI_AVERAGE_DIAG
!
USE MODI_FORCING_VERT_SHIFT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
!
!
!*      0.2    declarations of local variables
!
INTEGER :: JTILE                        ! loop on type of surface
LOGICAL :: GNATURE, GTOWN, GWATER, GSEA ! .T. if the corresponding surface is represented
INTEGER :: JSW                          ! number of spectral whort wave bands
!
REAL, DIMENSION(SIZE(XSEA),NTILESFC) :: ZFRAC_TILE! fraction of each tile
INTEGER, DIMENSION(5) :: IFACT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
! Preliminaries: Tile related operations
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_ATM_N',0,ZHOOK_HANDLE)
CPROGNAME = HPROGRAM
!
! FLAGS for the various surfaces:
!
GSEA      = NDIM_SEA    >0
GWATER    = NDIM_WATER  >0
GTOWN     = NDIM_TOWN   >0
GNATURE   = NDIM_NATURE >0
!
! Tile counter:
!
JTILE     = 0 
!
! Fractions for each tile:
!
ZFRAC_TILE(:,:)    = 0.0
!
! Number of spectral short wave bands for detailed radiation budget
JSW = SIZE(XSWBD_TILE,3)
!
!
 CALL GET_DIMS(IFACT)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! SEA Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
! first, pack vector...then call ALMA routine
!
JTILE               = JTILE + 1
!
IF(GSEA)THEN
! 
  ZFRAC_TILE(:,JTILE) = XSEA(:)
!
  CALL TREAT_SURF(JTILE,NSIZE_SEA,NR_SEA,IFACT)
!
ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! INLAND WATER Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE               = JTILE + 1
!
IF(GWATER)THEN
!
  ZFRAC_TILE(:,JTILE) = XWATER(:)
!
  CALL TREAT_SURF(JTILE,NSIZE_WATER,NR_WATER,IFACT)
!
ENDIF 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! NATURAL SURFACE Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE               = JTILE + 1
!
IF(GNATURE)THEN
!
    ZFRAC_TILE(:,JTILE) = XNATURE(:)
!
  CALL TREAT_SURF(JTILE,NSIZE_NATURE,NR_NATURE,IFACT)  
!
ENDIF 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! URBAN Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE               = JTILE + 1
!
IF(GTOWN)THEN
!
    ZFRAC_TILE(:,JTILE) = XTOWN(:)
!
  CALL TREAT_SURF(JTILE,NSIZE_TOWN,NR_TOWN,IFACT)  
!
ENDIF 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Grid box average fluxes/properties:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
 CALL AVERAGE_DIAG(N2M, LSURF_BUDGET, LSURF_BUDGETC, LCOEF, LSURF_VARS,   &
                    ZFRAC_TILE, XRN_TILE, XH_TILE, XLE_TILE, XLEI_TILE , &
                    XGFLUX_TILE,XRI_TILE, XCD_TILE, XCH_TILE, XCE_TILE,  &
                    XT2M_TILE, XTS_TILE, XQ2M_TILE, XHU2M_TILE,          &
                    XZON10M_TILE, XMER10M_TILE,                          &
                    XQS_TILE, XZ0_TILE, XZ0H_TILE,                       &
                    XSWD_TILE, XSWU_TILE, XSWBD_TILE, XSWBU_TILE,        &
                    XLWD_TILE, XLWU_TILE, XFMU_TILE, XFMV_TILE,          &
                    XRNC_TILE, XHC_TILE, XLEC_TILE, XGFLUXC_TILE,        &
                    XSWDC_TILE, XSWUC_TILE, XLWDC_TILE, XLWUC_TILE,      &
                    XFMUC_TILE, XFMVC_TILE, XT2M_MIN_TILE,               &
                    XT2M_MAX_TILE, XLEIC_TILE,                           &
                    XAVG_RN, XAVG_H, XAVG_LE, XAVG_LEI, XAVG_GFLUX,      &
                    XAVG_RI, XAVG_CD, XAVG_CH, XAVG_CE,                  &
                    XAVG_T2M, XAVG_TS, XAVG_Q2M, XAVG_HU2M,              &
                    XAVG_ZON10M, XAVG_MER10M,                            &
                    XAVG_QS, XAVG_Z0, XAVG_Z0H,                          &
                    XDIAG_UREF, XDIAG_ZREF,                              &
                    XAVG_SWD, XAVG_SWU, XAVG_SWBD, XAVG_SWBU,            &
                    XAVG_LWD, XAVG_LWU, XAVG_FMU, XAVG_FMV,              &
                    XAVG_RNC, XAVG_HC, XAVG_LEC, XAVG_GFLUXC,            &
                    XAVG_SWDC, XAVG_SWUC, XAVG_LWDC, XAVG_LWUC,          &
                    XAVG_FMUC, XAVG_FMVC, XAVG_T2M_MIN,                  &
                    XAVG_T2M_MAX, XAVG_LEIC,                             &
                    XHU2M_MIN_TILE, XHU2M_MAX_TILE, XAVG_HU2M_MIN,       &
                    XAVG_HU2M_MAX, XWIND10M_TILE, XWIND10M_MAX_TILE,     &
                    XAVG_WIND10M, XAVG_WIND10M_MAX                       )  
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Quantities at 2 meters above the minimum orography of the grid mesh
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
IF (L2M_MIN_ZS) CALL GET_2M
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_ATM_N',1,ZHOOK_HANDLE)
CONTAINS
!=======================================================================================
SUBROUTINE GET_2M
!
REAL, DIMENSION(SIZE(XSEA)) :: ZPS         ! surface air pressure
REAL, DIMENSION(SIZE(XSEA)) :: ZRHOA       ! surface air density
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_ATM_n:GET_2M',0,ZHOOK_HANDLE)
!
 CALL FORCING_VERT_SHIFT(XZS,XMIN_ZS,XAVG_T2M,XAVG_Q2M,XPS,XRHOA, &
                            XAVG_T2M_MIN_ZS,XAVG_Q2M_MIN_ZS,ZPS,ZRHOA)  
XAVG_HU2M_MIN_ZS = XAVG_HU2M
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_ATM_n:GET_2M',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_2M
!
!=======================================================================================
!
SUBROUTINE GET_DIMS(KFACT)
!
IMPLICIT NONE
!
INTEGER, DIMENSION(5), INTENT(OUT) :: KFACT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_ATM_n:GET_DIMS',0,ZHOOK_HANDLE)
!
KFACT(:)=0
!
IF (LSURF_BUDGET) KFACT(1)=1
!
IF (LSURF_BUDGETC) KFACT(2)=1
!
IF (N2M>=1) KFACT(3)=1
!
IF (LCOEF) KFACT(4)=1
!
IF (LSURF_VARS) KFACT(5)=1
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_ATM_n:GET_DIMS',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_DIMS
!
!=======================================================================================
!
SUBROUTINE TREAT_SURF(KTILE,KSIZE,KMASK,KFACT)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)               :: KTILE
INTEGER, INTENT(IN)               :: KSIZE
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
INTEGER, DIMENSION(5), INTENT(IN) :: KFACT
!
REAL, DIMENSION(KSIZE) :: ZP_TS       ! surface temperature (K)
!
REAL, DIMENSION(KSIZE*KFACT(1)) :: ZP_RN       ! Net radiation (W/m2)
REAL, DIMENSION(KSIZE*KFACT(1)) :: ZP_H        ! sensible heat flux (W/m2)
REAL, DIMENSION(KSIZE*KFACT(1)) :: ZP_LE       ! total latent heat flux (W/m2)
REAL, DIMENSION(KSIZE*KFACT(1)) :: ZP_LEI      ! sublimation latent heat flux (W/m2)
REAL, DIMENSION(KSIZE*KFACT(1)) :: ZP_GFLUX    ! storage flux (W/m2)
!
REAL, DIMENSION(KSIZE*KFACT(1)) :: ZP_SWD      ! short wave incoming radiation (W/m2)
REAL, DIMENSION(KSIZE*KFACT(1)) :: ZP_SWU      ! short wave outgoing radiation (W/m2)
REAL, DIMENSION(KSIZE*KFACT(1),JSW*KFACT(1)) :: ZP_SWBD   ! short wave incoming radiation by spectral band (W/m2)
REAL, DIMENSION(KSIZE*KFACT(1),JSW*KFACT(1)) :: ZP_SWBU   ! short wave outgoing radiation by spectral band (W/m2)
REAL, DIMENSION(KSIZE*KFACT(1)) :: ZP_LWD      ! long wave incoming radiation (W/m2)
REAL, DIMENSION(KSIZE*KFACT(1)) :: ZP_LWU      ! long wave outgoing radiation (W/m2)
REAL, DIMENSION(KSIZE*KFACT(1)) :: ZP_FMU      ! zonal friction
REAL, DIMENSION(KSIZE*KFACT(1)) :: ZP_FMV      ! meridian friction 
!
REAL, DIMENSION(KSIZE*KFACT(2)) :: ZP_RNC      ! Cumulated Net radiation (W/m2)
REAL, DIMENSION(KSIZE*KFACT(2)) :: ZP_HC       ! Cumulated sensible heat flux (W/m2)
REAL, DIMENSION(KSIZE*KFACT(2)) :: ZP_LEC      ! Cumulated total latent heat flux (W/m2)
REAL, DIMENSION(KSIZE*KFACT(2)) :: ZP_LEIC     ! Cumulated sublimation latent heat flux (W/m2)
REAL, DIMENSION(KSIZE*KFACT(2)) :: ZP_GFLUXC   ! Cumulated storage flux (W/m2)
REAL, DIMENSION(KSIZE*KFACT(2)) :: ZP_SWDC     ! Cumulated short wave incoming radiation (W/m2)
REAL, DIMENSION(KSIZE*KFACT(2)) :: ZP_SWUC     ! Cumulated short wave outgoing radiation (W/m2)
REAL, DIMENSION(KSIZE*KFACT(2)) :: ZP_LWDC     ! Cumulated long wave incoming radiation (W/m2)
REAL, DIMENSION(KSIZE*KFACT(2)) :: ZP_LWUC     ! Cumulated long wave outgoing radiation (W/m2)
REAL, DIMENSION(KSIZE*KFACT(2)) :: ZP_FMUC     ! Cumulated zonal friction
REAL, DIMENSION(KSIZE*KFACT(2)) :: ZP_FMVC     ! Cumulated meridian friction 
!
REAL, DIMENSION(KSIZE*KFACT(3)) :: ZP_RI       ! Richardson number
REAL, DIMENSION(KSIZE*KFACT(3)) :: ZP_T2M      ! air temperature at 2 meters (K)
REAL, DIMENSION(KSIZE*KFACT(3)) :: ZP_T2M_MIN  ! Minimum air temperature at 2 meters (K)
REAL, DIMENSION(KSIZE*KFACT(3)) :: ZP_T2M_MAX  ! Maximum air temperature at 2 meters (K)
REAL, DIMENSION(KSIZE*KFACT(3)) :: ZP_Q2M      ! air humidity at 2 meters (kg/kg)
REAL, DIMENSION(KSIZE*KFACT(3)) :: ZP_HU2M     ! air relative humidity at 2 meters (-)
REAL, DIMENSION(KSIZE*KFACT(3)) :: ZP_HU2M_MIN ! Minimum air relative humidity at 2 meters (-)
REAL, DIMENSION(KSIZE*KFACT(3)) :: ZP_HU2M_MAX ! Maximum air relative humidity at 2 meters (-)
REAL, DIMENSION(KSIZE*KFACT(3)) :: ZP_ZON10M   ! zonal wind at 10 meters (m/s)
REAL, DIMENSION(KSIZE*KFACT(3)) :: ZP_MER10M   ! meridian wind at 10 meters (m/s)
REAL, DIMENSION(KSIZE*KFACT(3)) :: ZP_WIND10M  ! wind at 10 meters (m/s)
REAL, DIMENSION(KSIZE*KFACT(3)) :: ZP_WIND10M_MAX ! Maximum wind at 10 meters (m/s)
!
REAL, DIMENSION(KSIZE*KFACT(4)) :: ZP_CD       ! drag coefficient for wind
REAL, DIMENSION(KSIZE*KFACT(4)) :: ZP_CH       ! drag coefficient for heat
REAL, DIMENSION(KSIZE*KFACT(4)) :: ZP_CE       ! drag coefficient for evaporation
REAL, DIMENSION(KSIZE*KFACT(4)) :: ZP_Z0       ! roughness length for momentum
REAL, DIMENSION(KSIZE*KFACT(4)) :: ZP_Z0H      ! roughness length for heat
!
REAL, DIMENSION(KSIZE*KFACT(5)) :: ZP_QS       ! specific humidity
!
INTEGER :: JJ, JJSW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_ATM_n:TREAT_SURF',0,ZHOOK_HANDLE)
!
IF (KTILE==1) THEN
  !
  CALL DIAG_SEA_n(HPROGRAM,                             &
                  ZP_RN, ZP_H, ZP_LE, ZP_LEI, ZP_GFLUX, &
                  ZP_RI, ZP_CD, ZP_CH, ZP_CE,           &
                  ZP_QS, ZP_Z0, ZP_Z0H,                 &
                  ZP_T2M, ZP_TS, ZP_Q2M, ZP_HU2M,       &
                  ZP_ZON10M, ZP_MER10M,                 &
                  ZP_SWD, ZP_SWU, ZP_SWBD, ZP_SWBU,     &
                  ZP_LWD, ZP_LWU, ZP_FMU, ZP_FMV,       &
                  ZP_RNC, ZP_HC, ZP_LEC, ZP_GFLUXC,     &
                  ZP_SWDC, ZP_SWUC, ZP_LWDC, ZP_LWUC,   &
                  ZP_FMUC, ZP_FMVC, ZP_T2M_MIN,         &
                  ZP_T2M_MAX, ZP_LEIC, ZP_HU2M_MIN,     &
                  ZP_HU2M_MAX, ZP_WIND10M,              &
                  ZP_WIND10M_MAX                        )   
  !
ELSEIF (KTILE==2) THEN
  !
  CALL DIAG_INLAND_WATER_n(HPROGRAM,                            &
                           ZP_RN, ZP_H, ZP_LE, ZP_LEI, ZP_GFLUX,&
                           ZP_RI, ZP_CD, ZP_CH, ZP_CE,          &
                           ZP_QS, ZP_Z0, ZP_Z0H,                &
                           ZP_T2M, ZP_TS, ZP_Q2M, ZP_HU2M,      &
                           ZP_ZON10M, ZP_MER10M,                &
                           ZP_SWD, ZP_SWU, ZP_SWBD, ZP_SWBU,    &
                           ZP_LWD, ZP_LWU, ZP_FMU, ZP_FMV,      &
                           ZP_RNC, ZP_HC, ZP_LEC, ZP_GFLUXC,    &
                           ZP_SWDC, ZP_SWUC, ZP_LWDC, ZP_LWUC,  &
                           ZP_FMUC, ZP_FMVC, ZP_T2M_MIN,        &
                           ZP_T2M_MAX, ZP_LEIC, ZP_HU2M_MIN,    &
                           ZP_HU2M_MAX, ZP_WIND10M,             &
                           ZP_WIND10M_MAX                       )   
  !
ELSEIF (KTILE==3) THEN
  !
  CALL DIAG_NATURE_n(HPROGRAM,                            &
                     ZP_RN, ZP_H, ZP_LE, ZP_LEI, ZP_GFLUX,&
                     ZP_RI, ZP_CD, ZP_CH, ZP_CE,          &
                     ZP_QS, ZP_Z0, ZP_Z0H,                &
                     ZP_T2M, ZP_TS, ZP_Q2M, ZP_HU2M,      &
                     ZP_ZON10M, ZP_MER10M,                &
                     ZP_SWD, ZP_SWU, ZP_SWBD, ZP_SWBU,    &
                     ZP_LWD, ZP_LWU, ZP_FMU, ZP_FMV,      &
                     ZP_RNC, ZP_HC, ZP_LEC, ZP_GFLUXC,    &
                     ZP_SWDC, ZP_SWUC, ZP_LWDC, ZP_LWUC,  &
                     ZP_FMUC, ZP_FMVC, ZP_T2M_MIN,        &
                     ZP_T2M_MAX, ZP_LEIC, ZP_HU2M_MIN,    &
                     ZP_HU2M_MAX, ZP_WIND10M,             &
                     ZP_WIND10M_MAX                       )   
  !
ELSEIF (KTILE==4) THEN
  !
  CALL DIAG_TOWN_n(HPROGRAM,                            &
                   ZP_RN, ZP_H, ZP_LE, ZP_LEI, ZP_GFLUX,&
                   ZP_RI, ZP_CD, ZP_CH, ZP_CE,          &
                   ZP_QS, ZP_Z0, ZP_Z0H,                &
                   ZP_T2M, ZP_TS, ZP_Q2M, ZP_HU2M,      &
                   ZP_ZON10M, ZP_MER10M,                &
                   ZP_SWD, ZP_SWU, ZP_SWBD, ZP_SWBU,    &
                   ZP_LWD, ZP_LWU, ZP_FMU, ZP_FMV,      &
                   ZP_RNC, ZP_HC, ZP_LEC, ZP_GFLUXC,    &
                   ZP_SWDC, ZP_SWUC, ZP_LWDC, ZP_LWUC,  &
                   ZP_FMUC, ZP_FMVC, ZP_T2M_MIN,        &
                   ZP_T2M_MAX, ZP_LEIC, ZP_HU2M_MIN,    &
                   ZP_HU2M_MAX, ZP_WIND10M,             &
                   ZP_WIND10M_MAX                       )  
  !
ENDIF
!
!----------------------------------------------------------------------
IF (LSURF_BUDGET) THEN
  DO JJ=1,KSIZE
   XRN_TILE      (KMASK(JJ),KTILE)  = ZP_RN       (JJ)
   XH_TILE       (KMASK(JJ),KTILE)  = ZP_H        (JJ)
   XLE_TILE      (KMASK(JJ),KTILE)  = ZP_LE       (JJ)
   XLEI_TILE     (KMASK(JJ),KTILE)  = ZP_LEI      (JJ)
   XGFLUX_TILE   (KMASK(JJ),KTILE)  = ZP_GFLUX    (JJ)
   XSWD_TILE     (KMASK(JJ),KTILE)  = ZP_SWD      (JJ)
   XSWU_TILE     (KMASK(JJ),KTILE)  = ZP_SWU      (JJ)
   XLWD_TILE     (KMASK(JJ),KTILE)  = ZP_LWD      (JJ)
   XLWU_TILE     (KMASK(JJ),KTILE)  = ZP_LWU      (JJ)
   XFMU_TILE     (KMASK(JJ),KTILE)  = ZP_FMU      (JJ)
   XFMV_TILE     (KMASK(JJ),KTILE)  = ZP_FMV      (JJ)
   DO JJSW=1, SIZE(XSWBD_TILE,3)
      XSWBD_TILE    (KMASK(JJ),KTILE,JJSW) = ZP_SWBD     (JJ,JJSW)
      XSWBU_TILE    (KMASK(JJ),KTILE,JJSW) = ZP_SWBU     (JJ,JJSW)
   ENDDO
  ENDDO
END IF
!
IF (LSURF_BUDGETC) THEN
  DO JJ=1,KSIZE
   XRNC_TILE      (KMASK(JJ),KTILE)  = ZP_RNC       (JJ)
   XHC_TILE       (KMASK(JJ),KTILE)  = ZP_HC        (JJ)
   XLEC_TILE      (KMASK(JJ),KTILE)  = ZP_LEC       (JJ)
   XLEIC_TILE     (KMASK(JJ),KTILE)  = ZP_LEIC      (JJ)
   XGFLUXC_TILE   (KMASK(JJ),KTILE)  = ZP_GFLUXC    (JJ)
   XSWDC_TILE     (KMASK(JJ),KTILE)  = ZP_SWDC      (JJ)
   XSWUC_TILE     (KMASK(JJ),KTILE)  = ZP_SWUC      (JJ)
   XLWDC_TILE     (KMASK(JJ),KTILE)  = ZP_LWDC      (JJ)
   XLWUC_TILE     (KMASK(JJ),KTILE)  = ZP_LWUC      (JJ)
   XFMUC_TILE     (KMASK(JJ),KTILE)  = ZP_FMUC      (JJ)
   XFMVC_TILE     (KMASK(JJ),KTILE)  = ZP_FMVC      (JJ)
  ENDDO
END IF
!
DO JJ=1,KSIZE
   XTS_TILE       (KMASK(JJ),KTILE)  = ZP_TS      (JJ)
ENDDO
!
IF (N2M>=1) THEN
  DO JJ=1,KSIZE
   XRI_TILE      (KMASK(JJ),KTILE)  = ZP_RI       (JJ)
   XT2M_TILE     (KMASK(JJ),KTILE)  = ZP_T2M      (JJ)
   XT2M_MIN_TILE (KMASK(JJ),KTILE)  = ZP_T2M_MIN  (JJ)
   XT2M_MAX_TILE (KMASK(JJ),KTILE)  = ZP_T2M_MAX  (JJ)
   XQ2M_TILE     (KMASK(JJ),KTILE)  = ZP_Q2M      (JJ)
   XHU2M_TILE    (KMASK(JJ),KTILE)  = ZP_HU2M     (JJ)
   XHU2M_MIN_TILE(KMASK(JJ),KTILE)  = ZP_HU2M_MIN (JJ)
   XHU2M_MAX_TILE(KMASK(JJ),KTILE)  = ZP_HU2M_MAX (JJ)
   XZON10M_TILE  (KMASK(JJ),KTILE)  = ZP_ZON10M   (JJ)
   XMER10M_TILE  (KMASK(JJ),KTILE)  = ZP_MER10M   (JJ)
   XWIND10M_TILE (KMASK(JJ),KTILE)  = ZP_WIND10M   (JJ)
   XWIND10M_MAX_TILE (KMASK(JJ),KTILE)  = ZP_WIND10M_MAX   (JJ)
  ENDDO
END IF
!
IF (LCOEF) THEN
  DO JJ=1,KSIZE
   XCD_TILE      (KMASK(JJ),KTILE)  = ZP_CD       (JJ)
   XCH_TILE      (KMASK(JJ),KTILE)  = ZP_CH       (JJ)
   XCE_TILE      (KMASK(JJ),KTILE)  = ZP_CE       (JJ)
   XQS_TILE      (KMASK(JJ),KTILE)  = ZP_QS       (JJ)
   XZ0_TILE      (KMASK(JJ),KTILE)  = ZP_Z0       (JJ)
   XZ0H_TILE     (KMASK(JJ),KTILE)  = ZP_Z0H      (JJ)
  ENDDO
END IF
!----------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_ATM_n:TREAT_SURF',1,ZHOOK_HANDLE)
!
END SUBROUTINE TREAT_SURF
!=======================================================================================
END SUBROUTINE DIAG_SURF_ATM_n
