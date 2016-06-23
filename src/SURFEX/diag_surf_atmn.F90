!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_SURF_ATM_n (DGEI, DGF, DGL, DGI, DGS, DGU, DGT, DGW, U, USS, &
                            HPROGRAM)
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
!       B. decharme 04/2013 : Add EVAP and SUBL diag
!!------------------------------------------------------------------
!

!
!
!
!
!
!
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_FLAKE_n, ONLY : DIAG_FLAKE_t
USE MODD_DIAG_IDEAL_n, ONLY : DIAG_IDEAL_t
USE MODD_DIAG_ISBA_n, ONLY : DIAG_ISBA_t
USE MODD_DIAG_SEAFLUX_n, ONLY : DIAG_SEAFLUX_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_DIAG_TEB_n, ONLY : DIAG_TEB_t
USE MODD_DIAG_WATFLUX_n, ONLY : DIAG_WATFLUX_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
!
USE MODD_SURF_CONF,      ONLY : CPROGNAME
USE MODD_DATA_COVER_PAR, ONLY : NTILESFC
!
USE MODI_DIAG_NATURE_n 
USE MODI_DIAG_SEA_n 
USE MODI_DIAG_INLAND_WATER_n 
USE MODI_DIAG_TOWN_n 
USE MODI_AVERAGE_DIAG
!
USE MODI_MINZS_VERT_SHIFT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DGEI
TYPE(DIAG_FLAKE_t), INTENT(INOUT) :: DGF
TYPE(DIAG_IDEAL_t), INTENT(INOUT) :: DGL
TYPE(DIAG_ISBA_t), INTENT(INOUT) :: DGI
TYPE(DIAG_SEAFLUX_t), INTENT(INOUT) :: DGS
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(DIAG_TEB_t), INTENT(INOUT) :: DGT
TYPE(DIAG_WATFLUX_t), INTENT(INOUT) :: DGW
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
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
REAL, DIMENSION(SIZE(U%XSEA),NTILESFC) :: ZFRAC_TILE! fraction of each tile
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
GSEA      = U%NDIM_SEA    >0
GWATER    = U%NDIM_WATER  >0
GTOWN     = U%NDIM_TOWN   >0
GNATURE   = U%NDIM_NATURE >0
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
JSW = SIZE(DGU%XSWBD_TILE,3)
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
  ZFRAC_TILE(:,JTILE) = U%XSEA(:)
!
  CALL TREAT_SURF(JTILE,U%NSIZE_SEA,U%NR_SEA,IFACT)
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
  ZFRAC_TILE(:,JTILE) = U%XWATER(:)
!
  CALL TREAT_SURF(JTILE,U%NSIZE_WATER,U%NR_WATER,IFACT)
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
    ZFRAC_TILE(:,JTILE) = U%XNATURE(:)
!
  CALL TREAT_SURF(JTILE,U%NSIZE_NATURE,U%NR_NATURE,IFACT)  
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
    ZFRAC_TILE(:,JTILE) = U%XTOWN(:)
!
  CALL TREAT_SURF(JTILE,U%NSIZE_TOWN,U%NR_TOWN,IFACT)  
!
ENDIF 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Grid box average fluxes/properties:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
 CALL AVERAGE_DIAG(DGU%N2M, DGU%LT2MMW, DGU%LSURF_BUDGET, DGU%LSURF_BUDGETC, DGU%LCOEF, DGU%LSURF_VARS,   &
                    ZFRAC_TILE, DGU%XRN_TILE, DGU%XH_TILE, DGU%XLE_TILE, DGU%XLEI_TILE , &
                    DGU%XGFLUX_TILE,DGU%XRI_TILE, DGU%XCD_TILE, DGU%XCH_TILE, DGU%XCE_TILE,  &
                    DGU%XT2M_TILE, DGU%XTS_TILE, DGU%XQ2M_TILE, DGU%XHU2M_TILE,          &
                    DGU%XZON10M_TILE, DGU%XMER10M_TILE,                          &
                    DGU%XQS_TILE, DGU%XZ0_TILE, DGU%XZ0H_TILE,                       &
                    DGU%XSWD_TILE, DGU%XSWU_TILE, DGU%XSWBD_TILE, DGU%XSWBU_TILE,        &
                    DGU%XLWD_TILE, DGU%XLWU_TILE, DGU%XFMU_TILE, DGU%XFMV_TILE,          &
                    DGU%XRNC_TILE, DGU%XHC_TILE, DGU%XLEC_TILE, DGU%XGFLUXC_TILE,        &
                    DGU%XSWDC_TILE, DGU%XSWUC_TILE, DGU%XLWDC_TILE, DGU%XLWUC_TILE,      &
                    DGU%XFMUC_TILE, DGU%XFMVC_TILE, DGU%XT2M_MIN_TILE,               &
                    DGU%XT2M_MAX_TILE, DGU%XLEIC_TILE,                           &
                    DGU%XAVG_RN, DGU%XAVG_H, DGU%XAVG_LE, DGU%XAVG_LEI, DGU%XAVG_GFLUX,      &
                    DGU%XAVG_RI, DGU%XAVG_CD, DGU%XAVG_CH, DGU%XAVG_CE,                  &
                    DGU%XAVG_T2M, DGU%XAVG_TS, DGU%XAVG_Q2M, DGU%XAVG_HU2M,              &
                    DGU%XAVG_ZON10M, DGU%XAVG_MER10M,                            &
                    DGU%XAVG_QS, DGU%XAVG_Z0, DGU%XAVG_Z0H,                          &
                    DGU%XDIAG_UREF, DGU%XDIAG_ZREF,                              &
                    DGU%XAVG_SWD, DGU%XAVG_SWU, DGU%XAVG_SWBD, DGU%XAVG_SWBU,            &
                    DGU%XAVG_LWD, DGU%XAVG_LWU, DGU%XAVG_FMU, DGU%XAVG_FMV,              &
                    DGU%XAVG_RNC, DGU%XAVG_HC, DGU%XAVG_LEC, DGU%XAVG_GFLUXC,            &
                    DGU%XAVG_SWDC, DGU%XAVG_SWUC, DGU%XAVG_LWDC, DGU%XAVG_LWUC,          &
                    DGU%XAVG_FMUC, DGU%XAVG_FMVC, DGU%XAVG_T2M_MIN,                  &
                    DGU%XAVG_T2M_MAX, DGU%XAVG_LEIC,                             &
                    DGU%XHU2M_MIN_TILE, DGU%XHU2M_MAX_TILE, DGU%XAVG_HU2M_MIN,       &
                    DGU%XAVG_HU2M_MAX, DGU%XWIND10M_TILE, DGU%XWIND10M_MAX_TILE,     &
                    DGU%XAVG_WIND10M, DGU%XAVG_WIND10M_MAX,                      &
                    DGU%XEVAP_TILE, DGU%XEVAPC_TILE, DGU%XAVG_EVAP, DGU%XAVG_EVAPC,      &
                    DGU%XSUBL_TILE, DGU%XSUBLC_TILE, DGU%XAVG_SUBL, DGU%XAVG_SUBLC       )                    
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Quantities at 2 meters above the minimum orography of the grid mesh
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
IF (DGU%L2M_MIN_ZS) CALL GET_2M
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_ATM_N',1,ZHOOK_HANDLE)
 CONTAINS
!=======================================================================================
SUBROUTINE GET_2M
!
REAL, DIMENSION(SIZE(U%XSEA)) :: ZPS         ! surface air pressure
REAL, DIMENSION(SIZE(U%XSEA)) :: ZRHOA       ! surface air density
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_ATM_n:GET_2M',0,ZHOOK_HANDLE)
!
 CALL MINZS_VERT_SHIFT(U%XZS,USS%XMIN_ZS,DGU%XAVG_T2M,DGU%XAVG_Q2M,DGU%XPS,DGU%XRHOA, &
                      DGU%XAVG_T2M_MIN_ZS,DGU%XAVG_Q2M_MIN_ZS,ZPS,ZRHOA)  
DGU%XAVG_HU2M_MIN_ZS = DGU%XAVG_HU2M
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
IF (DGU%LSURF_BUDGET) KFACT(1)=1
!
IF (DGU%LSURF_BUDGETC) KFACT(2)=1
!
IF (DGU%N2M>=1) KFACT(3)=1
!
IF (DGU%LCOEF) KFACT(4)=1
!
IF (DGU%LSURF_VARS) KFACT(5)=1
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
REAL, DIMENSION(KSIZE*KFACT(1)) :: ZP_EVAP     ! total evapotranspiration (kg/m2/s)
REAL, DIMENSION(KSIZE*KFACT(1)) :: ZP_SUBL     ! sublimation (kg/m2/s)
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
REAL, DIMENSION(KSIZE*KFACT(2)) :: ZP_EVAPC    ! Cumulated total evapotranspiration (kg/m2)
REAL, DIMENSION(KSIZE*KFACT(2)) :: ZP_SUBLC    ! Cumulated sublimation (kg/m2)
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
  CALL DIAG_SEA_n(DGL, DGS, U, &
                  HPROGRAM,                             &
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
                  ZP_WIND10M_MAX,                       &
                  ZP_EVAP, ZP_EVAPC, ZP_SUBL, ZP_SUBLC  )   
  !
ELSEIF (KTILE==2) THEN
  !
  CALL DIAG_INLAND_WATER_n(DGF, DGL, DGW, U, &
                           HPROGRAM,                            &
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
                           ZP_WIND10M_MAX,                      &
                           ZP_EVAP, ZP_EVAPC, ZP_SUBL, ZP_SUBLC  )   
  !
ELSEIF (KTILE==3) THEN
  !
  CALL DIAG_NATURE_n(DGEI, DGL, DGI, U, &
                     HPROGRAM,                            &
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
                     ZP_WIND10M_MAX,                      &
                     ZP_EVAP, ZP_EVAPC, ZP_SUBL, ZP_SUBLC )   
  !
ELSEIF (KTILE==4) THEN
  !
  CALL DIAG_TOWN_n(DGL, DGT, U, &
                   HPROGRAM,                            &
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
                   ZP_WIND10M_MAX,                      &
                   ZP_EVAP, ZP_EVAPC, ZP_SUBL, ZP_SUBLC )  
  !
ENDIF
!
!----------------------------------------------------------------------
IF (DGU%LSURF_BUDGET) THEN
  DO JJ=1,KSIZE
   DGU%XRN_TILE      (KMASK(JJ),KTILE)  = ZP_RN       (JJ)
   DGU%XH_TILE       (KMASK(JJ),KTILE)  = ZP_H        (JJ)
   DGU%XLE_TILE      (KMASK(JJ),KTILE)  = ZP_LE       (JJ)
   DGU%XLEI_TILE     (KMASK(JJ),KTILE)  = ZP_LEI      (JJ)
   DGU%XGFLUX_TILE   (KMASK(JJ),KTILE)  = ZP_GFLUX    (JJ)
   DGU%XEVAP_TILE    (KMASK(JJ),KTILE)  = ZP_EVAP     (JJ)
   DGU%XSUBL_TILE    (KMASK(JJ),KTILE)  = ZP_SUBL     (JJ)
   DGU%XSWD_TILE     (KMASK(JJ),KTILE)  = ZP_SWD      (JJ)
   DGU%XSWU_TILE     (KMASK(JJ),KTILE)  = ZP_SWU      (JJ)
   DGU%XLWD_TILE     (KMASK(JJ),KTILE)  = ZP_LWD      (JJ)
   DGU%XLWU_TILE     (KMASK(JJ),KTILE)  = ZP_LWU      (JJ)
   DGU%XFMU_TILE     (KMASK(JJ),KTILE)  = ZP_FMU      (JJ)
   DGU%XFMV_TILE     (KMASK(JJ),KTILE)  = ZP_FMV      (JJ)
   DO JJSW=1, SIZE(DGU%XSWBD_TILE,3)
      DGU%XSWBD_TILE    (KMASK(JJ),KTILE,JJSW) = ZP_SWBD     (JJ,JJSW)
      DGU%XSWBU_TILE    (KMASK(JJ),KTILE,JJSW) = ZP_SWBU     (JJ,JJSW)
   ENDDO
  ENDDO
END IF
!
IF (DGU%LSURF_BUDGETC) THEN
  DO JJ=1,KSIZE
   DGU%XRNC_TILE      (KMASK(JJ),KTILE)  = ZP_RNC       (JJ)
   DGU%XHC_TILE       (KMASK(JJ),KTILE)  = ZP_HC        (JJ)
   DGU%XLEC_TILE      (KMASK(JJ),KTILE)  = ZP_LEC       (JJ)
   DGU%XLEIC_TILE     (KMASK(JJ),KTILE)  = ZP_LEIC      (JJ)
   DGU%XGFLUXC_TILE   (KMASK(JJ),KTILE)  = ZP_GFLUXC    (JJ)
   DGU%XEVAPC_TILE    (KMASK(JJ),KTILE)  = ZP_EVAPC     (JJ)
   DGU%XSUBLC_TILE    (KMASK(JJ),KTILE)  = ZP_SUBLC     (JJ)
   DGU%XSWDC_TILE     (KMASK(JJ),KTILE)  = ZP_SWDC      (JJ)
   DGU%XSWUC_TILE     (KMASK(JJ),KTILE)  = ZP_SWUC      (JJ)
   DGU%XLWDC_TILE     (KMASK(JJ),KTILE)  = ZP_LWDC      (JJ)
   DGU%XLWUC_TILE     (KMASK(JJ),KTILE)  = ZP_LWUC      (JJ)
   DGU%XFMUC_TILE     (KMASK(JJ),KTILE)  = ZP_FMUC      (JJ)
   DGU%XFMVC_TILE     (KMASK(JJ),KTILE)  = ZP_FMVC      (JJ)
  ENDDO
END IF
!
DO JJ=1,KSIZE
   DGU%XTS_TILE       (KMASK(JJ),KTILE)  = ZP_TS      (JJ)
ENDDO
!
IF (DGU%N2M>=1) THEN
  DO JJ=1,KSIZE
   DGU%XRI_TILE      (KMASK(JJ),KTILE)  = ZP_RI       (JJ)
   DGU%XT2M_TILE     (KMASK(JJ),KTILE)  = ZP_T2M      (JJ)
   DGU%XT2M_MIN_TILE (KMASK(JJ),KTILE)  = ZP_T2M_MIN  (JJ)
   DGU%XT2M_MAX_TILE (KMASK(JJ),KTILE)  = ZP_T2M_MAX  (JJ)
   DGU%XQ2M_TILE     (KMASK(JJ),KTILE)  = ZP_Q2M      (JJ)
   DGU%XHU2M_TILE    (KMASK(JJ),KTILE)  = ZP_HU2M     (JJ)
   DGU%XHU2M_MIN_TILE(KMASK(JJ),KTILE)  = ZP_HU2M_MIN (JJ)
   DGU%XHU2M_MAX_TILE(KMASK(JJ),KTILE)  = ZP_HU2M_MAX (JJ)
   DGU%XZON10M_TILE  (KMASK(JJ),KTILE)  = ZP_ZON10M   (JJ)
   DGU%XMER10M_TILE  (KMASK(JJ),KTILE)  = ZP_MER10M   (JJ)
   DGU%XWIND10M_TILE (KMASK(JJ),KTILE)  = ZP_WIND10M   (JJ)
   DGU%XWIND10M_MAX_TILE (KMASK(JJ),KTILE)  = ZP_WIND10M_MAX   (JJ)
  ENDDO
END IF
!
IF (DGU%LCOEF) THEN
  DO JJ=1,KSIZE
   DGU%XCD_TILE      (KMASK(JJ),KTILE)  = ZP_CD       (JJ)
   DGU%XCH_TILE      (KMASK(JJ),KTILE)  = ZP_CH       (JJ)
   DGU%XCE_TILE      (KMASK(JJ),KTILE)  = ZP_CE       (JJ)
   DGU%XZ0_TILE      (KMASK(JJ),KTILE)  = ZP_Z0       (JJ)
   DGU%XZ0H_TILE     (KMASK(JJ),KTILE)  = ZP_Z0H      (JJ)
  ENDDO
END IF
IF (DGU%LSURF_VARS) THEN      
  DO JJ=1,KSIZE
   DGU%XQS_TILE      (KMASK(JJ),KTILE)  = ZP_QS       (JJ)
  ENDDO
END IF
!----------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_ATM_n:TREAT_SURF',1,ZHOOK_HANDLE)
!
END SUBROUTINE TREAT_SURF
!=======================================================================================
END SUBROUTINE DIAG_SURF_ATM_n
