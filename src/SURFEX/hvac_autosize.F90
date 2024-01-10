!SFX_LIC Copyright 2011-2018 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################################################
SUBROUTINE HVAC_AUTOSIZE (B, BOP, G, T, TOP, GDM, KI, KSW, KLUOUT)
!     #############################################################
!!
!!    PURPOSE
!!    -------
!!
!!    Calibrates HVAC systems for TEB-BEM
!!      
!!    AUTHOR
!!    ------
!!
!!      G. Pigeon           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!    Original    05/2011
!!    modified    08/2013 add solar panels (V. Masson)
!!                01/2016 add high vegetation (E.Redon/A.Lemonsu)
!!                01/2016 add urban hydrology (K.Chancibault/A.Lemonsu)
!!                02/2017 add CO2 fluxes (M. Goret)
!!                04/2017 suppress eff_heat as arg. of TEB  (M. Goret)
!!                10/2017 add hot water
!!                13/02/2018: comment OpenMP include/use if compiled without OpenMP (P. Wautelet)
!!                04.2020 completes energy check for high vegetation IR exchanges (V. Masson)
!!                06/2022 SIZE(YDMT%XWSNOW_GARDEN_INI,2) not defined (B. Decharme)
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t, TEB_HYDRO_MODEL_t
USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SURF_PAR, ONLY : XUNDEF, XSURF_EPSILON
USE MODD_TEB_IRRIG_n, ONLY : TEB_IRRIG_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_IRRIG_n, ONLY : TEB_IRRIG_t, TEB_IRRIG_INIT
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t, DIAG_MISC_TEB_INIT
USE MODD_CHECK_TEB, ONLY : CHECK_TEB_t
USE MODD_LW_COEF, ONLY : LW_COEF_t
USE MODD_DIAG_MISC_TEB_OPTIONS_n, ONLY : DIAG_MISC_TEB_OPTIONS_t, DIAG_MISC_TEB_OPTIONS_INIT
USE MODD_TEB_HYDRO_n,       ONLY : TEB_HYDRO_t
USE MODD_ISBA_n,            ONLY : ISBA_PE_t
USE MODD_SURF_ATM_TURB_n, ONLY : SURF_ATM_TURB_t
!
USE MODD_CSTS,  ONLY : XCPD, XPI, XP00, XRD
!
USE MODI_TEB_BLD_ROAD
USE MODI_SUNPOS
USE MODI_SW_DAYCYCLE
USE MODI_URBAN_LW_COEF
USE MODI_URBAN_SOLAR_ABS
USE MODI_GET_SIZES_PARALLEL
USE MODI_DIAG_MISC_TEB_INIT_n
USE MODI_ALLOC_LW_COEF
USE MODI_DEALLOC_LW_COEF
USE MODI_EXPLICIT_LONGWAVE
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
USE MODI_ALLOC_CHECK_TEB
USE MODI_DEALLOC_CHECK_TEB
#ifdef AIX64 
!$ USE OMP_LIB
#endif
!
IMPLICIT NONE
!
#ifndef AIX64
!$  INCLUDE 'omp_lib.h'
#endif
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
!
INTEGER, INTENT(IN) :: KSW    ! number of short-wave spectral bands
INTEGER,       INTENT(IN)    :: KI     ! number of points
INTEGER,       INTENT(IN)    :: KLUOUT ! output listing logical unit
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!-------------------------------------------------------------------------
!
TYPE(DIAG_MISC_TEB_t) :: YDMTC
TYPE(DIAG_MISC_TEB_t) :: YDMT
TYPE(DIAG_MISC_TEB_OPTIONS_t) :: YDMTO
TYPE(TEB_IRRIG_t)     :: YIR
TYPE(CHECK_TEB_t)     :: CT
TYPE(LW_COEF_t)       :: LW
TYPE(TEB_HYDRO_t)     :: YTH
TYPE(ISBA_PE_t)       :: YPE
TYPE(TEB_t)           :: YT
!
LOGICAL :: GCANOPY
LOGICAL :: GPAR_RD_IRRIG
LOGICAL :: GSURF_MISC_BUDGET
 CHARACTER(LEN=3) :: YBEM
 CHARACTER(LEN=6) :: YZ0H 
 CHARACTER(LEN=5) :: YCH_BEM  
!-------------------------------------------------------------------------
!
REAL, PARAMETER     :: ZTSTEP = 300.
INTEGER, PARAMETER  :: JPYEAR = 2004   ! Current year (UTC)
!
!local variable
!
INTEGER             :: IMONTH = 7  ! Current month (UTC)
INTEGER             :: IDAY   = 12 ! Current day (UTC)
REAL                :: ZTIME  = 0. ! Time at start of the run (s)
!
! Teb Fields T
!
REAL, DIMENSION(KI) :: ZGD
!
! Bem Options BOP
!
 CHARACTER(LEN=6)    :: YCOOL_COIL
 CHARACTER(LEN=6)    :: YHEAT_COIL
!
REAL, DIMENSION(KI) :: ZF_WATER_COND
REAL, DIMENSION(KI) :: ZAUX_MAX
!
CHARACTER(LEN=3)    :: YIMPLICIT_WIND = 'NEW'
!
! Loop control indexes
!
INTEGER             :: JJ
INTEGER             :: JFORC_STEP
INTEGER             :: INB_STEP_ATM
!
! Intermediate variables
!
REAL, DIMENSION(KI) :: ZU_RF
REAL, DIMENSION(KI) :: ZU_WL
!
REAL,DIMENSION(KI) :: ZT1    ! intermediate variable
REAL,DIMENSION(KI) :: ZTN    ! intermediate variable
!
REAL, DIMENSION(KI) :: ZT_SKY
!
REAL, DIMENSION(KI) :: ZTOT_SW
REAL, DIMENSION(KI) :: ZTOUT_EQ
!
! Arguments to TEB
!
REAL, DIMENSION(KI) :: ZT_CAN
REAL, DIMENSION(KI) :: ZQ_CAN
REAL, DIMENSION(KI) :: ZU_CAN
REAL, DIMENSION(KI) :: ZZ_LOWCAN
!
REAL, DIMENSION(KI) :: ZPS
REAL, DIMENSION(KI) :: ZPA
REAL, DIMENSION(KI) :: ZEXNS
REAL, DIMENSION(KI) :: ZEXNA
REAL, DIMENSION(KI) :: ZTA
REAL, DIMENSION(KI) :: ZQA
REAL, DIMENSION(KI) :: ZTA_RF
REAL, DIMENSION(KI) :: ZQA_RF
REAL, DIMENSION(KI) :: ZRHOA
REAL, DIMENSION(KI) :: ZLW_RAD
REAL, DIMENSION(KI) :: ZRR
REAL, DIMENSION(KI) :: ZSR
REAL, DIMENSION(KI) :: ZZREF
!
REAL, DIMENSION(KI) :: ZPEW_A_COEF
REAL, DIMENSION(KI) :: ZPEW_B_COEF
!
REAL, DIMENSION(KI) :: ZTS_GD
!
REAL, DIMENSION(KI) :: ZDF_RF
REAL, DIMENSION(KI) :: ZDN_RF
REAL, DIMENSION(KI) :: ZDN_RD
REAL, DIMENSION(KI) :: ZDF_RD
!
REAL, DIMENSION(KI) :: ZQSAT_RF
REAL, DIMENSION(KI) :: ZQSAT_RD
REAL, DIMENSION(KI) :: ZDELT_RF
REAL, DIMENSION(KI) :: ZDELT_RD
!
 CHARACTER(LEN=4)    :: YSNOW_RF
 CHARACTER(LEN=4)    :: YSNOW_RD
!
REAL, DIMENSION(KI) :: ZLEW_RF     ! latent heat flux over roof (snow)
!
REAL, DIMENSION(KI) :: ZLEW_RD     ! latent heat flux over road (snow)
!
REAL, DIMENSION(KI) :: ZPSOLD
REAL, DIMENSION(KI,BOP%NBEMCOMP) :: ZAUXQIN
REAL, DIMENSION(KI,BOP%NBEMCOMP) :: ZAUXFRACOMP
REAL, DIMENSION(KI,BOP%NBEMCOMP) :: ZAUXVENT_BEHAV_ANYWAY
REAL, DIMENSION(KI,BOP%NBEMCOMP) :: ZAUXVENT_BEHAV_ADAPTI
REAL, DIMENSION(KI) :: ZAUXINF
REAL, DIMENSION(KI) :: AUXU10M
!
REAL, DIMENSION(KI) :: ZAC_AGG_GARDEN
REAL, DIMENSION(KI) :: ZHU_AGG_GARDEN
!
REAL, DIMENSION(KI) :: ZFRAC_PANEL
REAL, DIMENSION(KI) :: ZALB_PANEL
REAL, DIMENSION(KI) :: ZASNOW_ROOF
REAL, DIMENSION(KI) :: ZASNOW_ROAD
REAL, DIMENSION(KI) :: ZEMIS_GARDEN
REAL, DIMENSION(KI) :: ZESNOW_ROAD
REAL, DIMENSION(KI) :: ZTSSNOW_ROAD
REAL, DIMENSION(KI) :: ZWS_ROOF
REAL, DIMENSION(KI) :: ZWS_ROAD
 CHARACTER(LEN=4)    :: HSNOW_ROOF
REAL, DIMENSION(KI,1):: ZWSNOW_ROOF
REAL, DIMENSION(KI,1) :: ZTSNOW_ROOF
REAL, DIMENSION(KI,1) :: ZRSNOW_ROOF
REAL, DIMENSION(KI) :: ZTSSNOW_ROOF
REAL, DIMENSION(KI) :: ZESNOW_ROOF
 CHARACTER(LEN=4)    :: HSNOW_ROAD
REAL, DIMENSION(KI,1) :: ZWSNOW_ROAD
REAL, DIMENSION(KI,1) :: ZTSNOW_ROAD
REAL, DIMENSION(KI,1) :: ZRSNOW_ROAD
REAL, DIMENSION(KI) :: ZDIR_ALB_TWN
REAL, DIMENSION(KI) :: ZSCA_ALB_TWN
REAL, DIMENSION(KI) :: ZREC_SW_WIN
REAL, DIMENSION(KI) :: ZREF_SW_GRND
REAL, DIMENSION(KI) :: ZREF_SW_FAC
REAL, DIMENSION(KI) :: ZREF_SW_HVEG
!
REAL, DIMENSION(KI) :: ZEMIS_WIN
REAL, DIMENSION(KI) :: ZRNSNOW_RF  ! net radiation over snow
REAL, DIMENSION(KI) :: ZHSNOW_RF   ! sensible heat flux over snow
REAL, DIMENSION(KI) :: ZLESNOW_RF  ! latent heat flux over snow
REAL, DIMENSION(KI) :: ZGSNOW_RF   ! flux under the snow
REAL, DIMENSION(KI) :: ZMELT_RF    ! snow melt
!
REAL, DIMENSION(KI) :: ZRNSNOW_RD  ! net radiation over snow
REAL, DIMENSION(KI) :: ZHSNOW_RD   ! sensible heat flux over snow
REAL, DIMENSION(KI) :: ZLESNOW_RD  ! latent heat flux over snow
REAL, DIMENSION(KI) :: ZGSNOW_RD   ! flux under the snow
REAL, DIMENSION(KI) :: ZMELT_RD    ! snow melt
!
REAL, DIMENSION(KI) :: ZUW_RD      ! Momentum flux for roads
REAL, DIMENSION(KI) :: ZUW_RF      ! Momentum flux for roofs
REAL, DIMENSION(KI) :: ZDUWDU_RD   !
REAL, DIMENSION(KI) :: ZDUWDU_RF   !
REAL, DIMENSION(KI) :: ZUSTAR_TWN  ! friction velocity over town
!
REAL, DIMENSION(KI) :: ZCD         ! town averaged drag coefficient
REAL, DIMENSION(KI) :: ZCDN        ! town averaged neutral drag coefficient
REAL, DIMENSION(KI) :: ZCH_TWN     ! town averaged heat transfer coefficient
REAL, DIMENSION(KI) :: ZRI_TWN     ! town averaged Richardson number
REAL, DIMENSION(KI) :: ZRESA_TWN   ! town aerodynamical resistance
REAL, DIMENSION(KI) :: ZAC_RF      ! roof conductance
REAL, DIMENSION(KI) :: ZAC_RD      ! road conductance
REAL, DIMENSION(KI) :: ZAC_WL      ! wall conductance
REAL, DIMENSION(KI) :: ZAC_TOP     ! top conductance
REAL, DIMENSION(KI) :: ZAC_GD      ! garden conductance
REAL, DIMENSION(KI) :: ZAC_RF_WAT  ! roof water conductance
REAL, DIMENSION(KI) :: ZAC_RD_WAT  ! roof water conductance 
REAL, DIMENSION(KI) :: ZEMIT_LW_FAC
REAL, DIMENSION(KI) :: ZEMIT_LW_RD
REAL, DIMENSION(KI) :: ZTSUN
!
! Arguments to urban_solar_abs
!
REAL, DIMENSION(KI) :: ZZENITH
REAL, DIMENSION(KI) :: ZAZIM
!
REAL, DIMENSION(KI) :: ZDIR_SW
REAL, DIMENSION(KI) :: ZSCA_SW
!
REAL, DIMENSION(KI) :: ZSVF_GD
!
REAL, DIMENSION(KI) :: ZALB_GR
REAL, DIMENSION(KI) :: ZALB_GD
REAL, DIMENSION(KI) :: ZALB_HVEG
REAL, DIMENSION(KI,BOP%NBEMCOMP) :: ZAUXSHAD_BEHAV_ANYWAY
REAL, DIMENSION(KI,BOP%NBEMCOMP) :: ZAUXSHAD_BEHAV_ADAPTI
!
!new for shading
REAL, DIMENSION(KI) :: ZE_SHADING
REAL, DIMENSION(KI) :: PSHADE
!
! Case greenroof
REAL, DIMENSION(KI) :: ZUW_GR
REAL, DIMENSION(KI) :: ZRN_GR
REAL, DIMENSION(KI) :: ZH_GR
REAL, DIMENSION(KI) :: ZLE_GR
REAL, DIMENSION(KI) :: ZGFLUX_GR
REAL, DIMENSION(KI) :: ZRUNOFF_GR 
REAL, DIMENSION(KI) :: ZDRAIN_GR 
!
! Case of high vegetation
REAL, DIMENSION(KI,2):: ZTRANS_HVEG    ! transmissivity profile
REAL, DIMENSION(KI) :: ZEMIS_HVEG      ! emissivity for high vegetation
REAL, DIMENSION(KI) :: ZTS_HVEG        ! surface temperature of urban trees at t
REAL, DIMENSION(KI) :: ZTRANS_HVCR     ! transmissivity profile through tree crown         
!
REAL, DIMENSION(KI,BOP%NBEMCOMP) :: ZAUXTCOOL_TARGET
REAL, DIMENSION(KI,BOP%NBEMCOMP) :: ZAUXTHEAT_TARGET
!
REAL, DIMENSION(KI) :: ZREC_SW_GD
REAL, DIMENSION(KI) :: ZREC_SW_RF
REAL, DIMENSION(KI) :: ZEMIS_GD
!
REAL, DIMENSION(KI) :: ZCST_H_WASTE_CANY
REAL, DIMENSION(KI) :: ZCST_LE_WASTE_CANY                          
REAL, DIMENSION(KI) :: ZCOE_H_WASTE_CANY
REAL, DIMENSION(KI) :: ZCOE_LE_WASTE_CANY                          
REAL, DIMENSION(KI) :: ZMUL_H_WASTE_CANY
REAL, DIMENSION(KI) :: ZMUL_LE_WASTE_CANY
!
! Case of urban irrigation
REAL                :: ZCONNEX         ! impervious surfaces connexion rate to the sewer (-)
REAL, DIMENSION(KI) :: ZWS_RF_MAX      
REAL, DIMENSION(KI) :: ZWS_RD_MAX
!
TYPE(SURF_ATM_TURB_t) :: AT   ! atmospheric turbulence parameters
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.     Initialization
!              --------------
!
IF (LHOOK) CALL DR_HOOK('HVAC_AUTOSIZE',0,ZHOOK_HANDLE)
!
CT%LCHECK_TEB = TOP%LCHECK_TEB
CT%XCHECK_PROCESS = TOP%XEPS_BDGT_FAC
IF (CT%LCHECK_TEB) CALL ALLOC_CHECK_TEB(CT, KI, BOP%NBEMCOMP)
!
CALL ALLOC_LW_COEF(LW,KI)
!
!    Design parameters
!
ZPS   = 101325.
ZPA = ZPS
ZEXNS = (ZPS/XP00)**(XRD/XCPD)
ZEXNA = (ZPA/XP00)**(XRD/XCPD)
ZQA   = 0.011
ZLW_RAD= 300.
ZRR = 0.0 
ZSR = 0.0 
ZZREF =  50.
!
ZPEW_A_COEF  = 0.5  
ZPEW_B_COEF  = 0.5 
!
! initial value for air temperature and outdoor wall/roof/window/road temperature
ZT_CAN(:) = 10.7/2 * SIN(2*XPI/(24*3600) * (ZTIME+16*3600)) + (B%XT_SIZE_MAX(:)-10.7/2)
!
ZQ_CAN = 0.011
ZU_CAN = 2.5 
ZZ_LOWCAN = ZZREF
ZAUX_MAX = B%XAUX_MAX
B%XAUX_MAX = 0.
ZT_SKY = B%XT_SIZE_MIN-20.0
!
ZTS_GD = 300.
!
ZDN_RF = 0.0
ZDF_RF = 1.0
ZDN_RD = 0.0
ZDF_RD = 1.0
!
ZQSAT_RF = 0.015
ZQSAT_RD = 0.015
ZDELT_RF = 0.0 
ZDELT_RD = 0.0 
!
B%XM_SYS_RAT    = 0.
B%XCAP_SYS_RAT  = 0.
B%XCAP_SYS_HEAT = 0.
!
ZAC_AGG_GARDEN(:) = 0.0
ZHU_AGG_GARDEN(:) = 0.0 
INB_STEP_ATM      = 3600*24*4/ZTSTEP
HSNOW_ROOF        = 'NONE'
HSNOW_ROAD        = 'NONE'
ZASNOW_ROOF       = 0.8
ZASNOW_ROAD       = 0.8
ZEMIS_WIN         = 1.0
ZEMIS_GARDEN      = 1.0
ZESNOW_ROAD       = 1.0
ZTSSNOW_ROAD      = 273.0
ZWS_ROOF          = 0.0
ZWS_ROAD          = 0.0
ZWSNOW_ROOF       = 0.0
ZTSNOW_ROOF       = 273.0
ZRSNOW_ROOF       = 0.0
ZTSSNOW_ROOF      = 273.0
ZESNOW_ROOF       = 0.0
ZWSNOW_ROAD       = 0.0
ZTSNOW_ROAD       = 273.0
ZRSNOW_ROAD       = 0.0
ZE_SHADING(:)     = 0.
PSHADE    (:)     = 0.
! solar panels are not taken into account in the building's HVAC equipment sizing process
ZFRAC_PANEL       = 0.
ZALB_PANEL        = 0.1
!
!
!
! Robert: New TEB input variables are hardcoded
!
ZAUXVENT_BEHAV_ANYWAY(:,:) = 0.0
ZAUXVENT_BEHAV_ADAPTI(:,:) = 0.0
ZAUXSHAD_BEHAV_ANYWAY(:,:) = 0.0
ZAUXSHAD_BEHAV_ADAPTI(:,:) = 0.0
ZAUXFRACOMP(:,:)      = 1.0
ZAUXINF(:)            = 0.5
ZPSOLD(:)             = 101325
AUXU10M(:)            = 3.0
ZAUXQIN(:,:)          = 5.0
ZAUXTCOOL_TARGET(:,:) = 298.15
ZAUXTHEAT_TARGET(:,:) = 293.15
!
!*      A.     Autosize of the heating system
!              ---------------------------------
!
YCOOL_COIL = 'IDEAL '
YHEAT_COIL = 'IDEAL '
ZF_WATER_COND(:) = 0.
ZRHOA = 1.30
ZTOUT_EQ(:) = (B%XT_SIZE_MIN(:) + ZT_SKY(:))/2.
!
ZU_RF(:) = 0.0
DO JJ=1,TOP%NROOF_LAYER
  ZU_RF(:) = ZU_RF(:) + T%XD_ROOF(:,JJ)/T%XTC_ROOF(:,JJ) 
END DO
ZU_RF(:) = ZU_RF(:) + 1./10. + 1./25.         
ZU_RF(:) = 1. / ZU_RF(:)
!
ZU_WL(:) = 0.0
DO JJ=1,TOP%NWALL_LAYER
  ZU_WL(:) = ZU_WL(:) + T%XD_WALL(:,JJ)/T%XTC_WALL(:,JJ)
END DO
ZU_WL(:) = ZU_WL(:) + 1./10. + 1./25.         
ZU_WL(:) = 1. / ZU_WL(:)
!
!   Heating Coil Capacity [W m-2(bld)]
B%XCAP_SYS_HEAT(:) = ZU_WL(:) * T%XWALL_O_BLD(:) * (ZAUXTHEAT_TARGET(:,1) - ZTOUT_EQ(:)) &
               + B%XU_WIN(:)  * B%XGLAZ_O_BLD(:) * (ZAUXTHEAT_TARGET(:,1) - ZTOUT_EQ(:)) &
               + ZU_RF(:)                        * (ZAUXTHEAT_TARGET(:,1) - ZTOUT_EQ(:)) &
               - ZAUXQIN(:,1) * T%XBLD_HEIGHT(:) / B%XFLOOR_HEIGHT(:)*          &
                 (1 - B%XQIN_FLAT(:))                                           &
               + ZAUXINF(:)  * T%XBLD_HEIGHT(:) / 3600* ZRHOA(:) * XCPD *       &
                 (ZAUXTHEAT_TARGET(:,1) - B%XT_SIZE_MIN(:))                     &
               + 0.0 * T%XBLD_HEIGHT(:) / 3600* ZRHOA(:) * XCPD *               &
                 (ZAUXTHEAT_TARGET(:,1) - B%XT_SIZE_MIN(:))
!
! Impose a minimum value for the heating system capacity
!
WHERE(B%XCAP_SYS_HEAT.LT.10.0) B%XCAP_SYS_HEAT(:)=10.0
!
!   Rated air flow rate [kg s-1 m-2(bld)]
B%XM_SYS_RAT(:) = B%XCAP_SYS_HEAT(:)/XCPD/(323.15 - ZAUXTHEAT_TARGET(:,1))
!
!
!*      B.     Autosize of the cooling system
!              -----------------------------------
!
ZRHOA = 1.15
!
!-------------------------------------------------------
!
!    Initial values
!
! Options TOP
!
YBEM = TOP%CBEM
TOP%CBEM = "BEM" 
!
YZ0H = TOP%CZ0H
TOP%CZ0H = 'KAND07'
!
GCANOPY = TOP%LCANOPY
TOP%LCANOPY = .FALSE.
!
YCH_BEM = TOP%CCH_BEM
TOP%CCH_BEM = 'DOE-2'
!
!
! Teb Fields T
!
ALLOCATE(T%XWS_ROOF(KI))
ALLOCATE(T%XWS_ROAD(KI))
T%XWS_ROOF = 0.0
T%XWS_ROAD = 0.0
!
ALLOCATE(T%TSNOW_ROOF%WSNOW(KI,1))
ALLOCATE(T%TSNOW_ROOF%T    (KI,1))
ALLOCATE(T%TSNOW_ROOF%ALB  (KI))
ALLOCATE(T%TSNOW_ROOF%RHO  (KI,1))
ALLOCATE(T%TSNOW_ROOF%TS   (KI))
ALLOCATE(T%TSNOW_ROOF%EMIS (KI))
!
YSNOW_RF = T%TSNOW_ROOF%SCHEME 
T%TSNOW_ROOF%SCHEME = 'NONE'
T%TSNOW_ROOF%WSNOW  = 0.0
T%TSNOW_ROOF%T      = 273.0
T%TSNOW_ROOF%RHO    = 0.0
T%TSNOW_ROOF%ALB    = 0.8
T%TSNOW_ROOF%TS     = 273.0
T%TSNOW_ROOF%EMIS   = 0.0
!
ALLOCATE(T%TSNOW_ROAD%WSNOW(KI,1))
ALLOCATE(T%TSNOW_ROAD%T    (KI,1))
ALLOCATE(T%TSNOW_ROAD%ALB  (KI))
ALLOCATE(T%TSNOW_ROAD%RHO  (KI,1))
ALLOCATE(T%TSNOW_ROAD%TS   (KI))
ALLOCATE(T%TSNOW_ROAD%EMIS (KI))
!
YSNOW_RD = T%TSNOW_ROAD%SCHEME 
T%TSNOW_ROAD%SCHEME = 'NONE'
T%TSNOW_ROAD%WSNOW  = 0.0
T%TSNOW_ROAD%T      = 273.0
T%TSNOW_ROAD%RHO    = 0.0
T%TSNOW_ROAD%ALB    = 0.8
T%TSNOW_ROAD%TS     = 273.0
T%TSNOW_ROAD%EMIS   = 1.0
!
ALLOCATE(YT%TSNOW_ROOF%WSNOW(KI,1))
ALLOCATE(YT%TSNOW_ROOF%T    (KI,1))
ALLOCATE(YT%TSNOW_ROOF%ALB  (KI))
ALLOCATE(YT%TSNOW_ROOF%RHO  (KI,1))
ALLOCATE(YT%TSNOW_ROOF%TS   (KI))
ALLOCATE(YT%TSNOW_ROOF%EMIS (KI))
!
YT%TSNOW_ROOF%SCHEME = 'NONE'
YT%TSNOW_ROOF%WSNOW  = 0.0
YT%TSNOW_ROOF%T      = 273.0
YT%TSNOW_ROOF%RHO    = 0.0
YT%TSNOW_ROOF%ALB    = 0.8
YT%TSNOW_ROOF%TS     = 273.0
YT%TSNOW_ROOF%EMIS   = 0.0
!
ALLOCATE(YT%TSNOW_ROAD%WSNOW(KI,1))
ALLOCATE(YT%TSNOW_ROAD%T    (KI,1))
ALLOCATE(YT%TSNOW_ROAD%ALB  (KI))
ALLOCATE(YT%TSNOW_ROAD%RHO  (KI,1))
ALLOCATE(YT%TSNOW_ROAD%TS   (KI))
ALLOCATE(YT%TSNOW_ROAD%EMIS (KI))
!
YT%TSNOW_ROAD%SCHEME = 'NONE'
YT%TSNOW_ROAD%WSNOW  = 0.0
YT%TSNOW_ROAD%T      = 273.0
YT%TSNOW_ROAD%RHO    = 0.0
YT%TSNOW_ROAD%ALB    = 0.8
YT%TSNOW_ROAD%TS     = 273.0
YT%TSNOW_ROAD%EMIS   = 1.0
!
ZGD = T%XGARDEN
T%XGARDEN = 0.
!
!------------------------------------------------
!BEM 
!
YCOOL_COIL = BOP%CCOOL_COIL
BOP%CCOOL_COIL = 'IDEAL '
YHEAT_COIL = BOP%CHEAT_COIL
BOP%CHEAT_COIL ='IDEAL '
!
ALLOCATE(B%XTI_BLD(KI,BOP%NBEMCOMP))
DO JJ=1,KI
   B%XTI_BLD(JJ,:) = MAX(ZAUXTHEAT_TARGET(JJ,1),ZT_CAN(JJ)) ! indoor air temperature
ENDDO
!
ALLOCATE(T%XT_ROOF  (KI,TOP%NROOF_LAYER))
ALLOCATE(T%XT_ROAD  (KI,TOP%NTEB_SOIL))
ALLOCATE(T%XT_WALL_A(KI,TOP%NWALL_LAYER))
ALLOCATE(T%XT_WALL_B(KI,TOP%NWALL_LAYER))
!
!RF
T%XT_ROOF  (:,TOP%NROOF_LAYER)   = B%XTI_BLD(:,1)   ! roof layers temperatures
T%XT_ROOF(:,1) = ZT_CAN(:)
ZT1(:) = T%XT_ROOF(:,1)
ZTN(:) = T%XT_ROOF(:,TOP%NROOF_LAYER)
IF (TOP%NROOF_LAYER .GT. 2) CALL INTERP_PROFTWL(ZT1, ZTN, T%XD_ROOF, T%XT_ROOF)
!
!RD
DO JJ=1,TOP%NTEB_SOIL
   T%XT_ROAD(:,JJ) = ZT_CAN(:)
ENDDO
!
!T_FLOOR, T_MASS
ALLOCATE(B%XT_FLOOR  (KI,BOP%NFLOOR_LAYER,BOP%NBEMCOMP))
ALLOCATE(B%XT_MASS   (KI,BOP%NMASS_LAYER,BOP%NBEMCOMP))
ALLOCATE(T%XT_BLD    (KI,TOP%NTEB_SOIL))
!
DO JJ=1,BOP%NFLOOR_LAYER
   B%XT_FLOOR(:,JJ,:)  = B%XTI_BLD(:,:) ! building floor temperature
ENDDO
DO JJ=1,BOP%NMASS_LAYER                                                   
   B%XT_MASS(:,JJ,:)   = B%XTI_BLD(:,:) ! building mass temperature
ENDDO
!
!BLD
!
DO JJ=1,TOP%NTEB_SOIL
   T%XT_BLD(:,JJ) = T%XT_ROAD(:,JJ)
ENDDO
!
!WL_A
T%XT_WALL_A(:,TOP%NWALL_LAYER)   = B%XTI_BLD(:,1)   ! wall layers temperatures
T%XT_WALL_A(:,1) = ZT_CAN(:)
ZT1(:)=T%XT_WALL_A(:,1)
ZTN(:)=T%XT_WALL_A(:,TOP%NWALL_LAYER)
IF (TOP%NWALL_LAYER .GT. 2) CALL INTERP_PROFTWL(ZT1, ZTN, T%XD_WALL, T%XT_WALL_A)
!
!WL_B
T%XT_WALL_B = T%XT_WALL_A
!
!OUTDOOR WINDOW TEMPERATURE
ALLOCATE(B%XT_WIN1(KI))
B%XT_WIN1(:) = ZT_CAN(:)
!! 
ALLOCATE(B%XT_WIN2(KI))
B%XT_WIN2(:) = B%XTI_BLD(:,1)
!
ALLOCATE(B%XQI_BLD(KI,BOP%NBEMCOMP))
B%XQI_BLD = 0.011
!
ALLOCATE(YDMT%XT_SYS   (KI,BOP%NBEMCOMP))
ALLOCATE(YDMT%XQ_SYS   (KI,BOP%NBEMCOMP))
!
YDMT%XT_SYS(:,:) = B%XTI_BLD(:,:)
YDMT%XQ_SYS(:,:) = B%XQI_BLD(:,:)
!
ZF_WATER_COND(:) = B%XF_WATER_COND(:)
B%XF_WATER_COND(:) = 0.
!
ZEMIS_HVEG (:) = 0.97
!
ZTS_HVEG     (:)   = ZT_CAN(:)
ZTRANS_HVEG  (:,:) = 1/2.

DO JJ=1,SIZE(T%XROAD)
  IF (T%XURBTREE(JJ).GT.0.) THEN
      ZTRANS_HVCR (JJ) = 0.5
  ELSE
      ZTRANS_HVCR (JJ) = 1.
  ENDIF
ENDDO

! 
!! Case of urban hydrology
ZCONNEX            = 1.
ZWS_RF_MAX(:) = 1.0
ZWS_RD_MAX(:) = 1.0
ALLOCATE(YTH%XWG_ROAD  (KI,TOP%NTEB_SOIL))
ALLOCATE(YTH%XWG_BLD   (KI,TOP%NTEB_SOIL))
ALLOCATE(YTH%XWGI_ROAD (KI,TOP%NTEB_SOIL))
ALLOCATE(YTH%XWGI_BLD  (KI,TOP%NTEB_SOIL))
ALLOCATE(YPE%XWGI      (KI,TOP%NTEB_SOIL))
ALLOCATE(YPE%XWG       (KI,TOP%NTEB_SOIL))
ALLOCATE(YPE%TSNOW%WSNOW(KI,1))
ALLOCATE(YPE%XWR       (KI))
ALLOCATE(YT%XWS_ROAD   (KI))
ALLOCATE(YT%XWS_ROOF   (KI))
YTH%XWG_ROAD(:,:)  = 0
YTH%XWG_BLD(:,:)   = 0
YTH%XWGI_ROAD(:,:)  = 0
YTH%XWGI_BLD(:,:)   = 0
YPE%XWGI(:,:)      = 0
YPE%XWG(:,:)       = 0
YPE%TSNOW%WSNOW(:,:)= 0
YPE%XWR(:)         = 0
YT%XWS_ROAD(:)     = 0
YT%XWS_ROOF(:)     = 0
!
!* road watering (not used)
!
GPAR_RD_IRRIG = YIR%LPAR_RD_IRRIG
YIR%LPAR_RD_IRRIG   = .FALSE.
!
ZRN_GR    (:) = 0.
ZH_GR     (:) = 0.
ZLE_GR    (:) = 0.
ZGFLUX_GR (:) = 0.
ZRUNOFF_GR(:) = 0.
ZDRAIN_GR (:) = 0.
!
 CALL DIAG_MISC_TEB_INIT(YDMT)
!
ZUW_GR    (:) = 0.
!
ZAC_GD    (:) = 0.
!
ZFRAC_PANEL  = 0.
ZALB_PANEL   = 0.1
ZSVF_GD   (:) = 0.
ZALB_GD   (:) = 0.
ZALB_GR(:) = 0.
ZALB_HVEG(:) = 0.
ZE_SHADING(:) = 0.
!
 CALL DIAG_MISC_TEB_OPTIONS_INIT(YDMTO)
 CALL DIAG_MISC_TEB_INIT(YDMTC)
!
 GSURF_MISC_BUDGET = YDMTO%LSURF_MISC_BUDGET
 YDMTO%LSURF_MISC_BUDGET = .TRUE.
!
 CALL DIAG_MISC_TEB_INIT_n(YDMTC, YDMT, YDMTO, TOP, BOP, KI, KSW, YTH, YPE, YT)
!
!* one supposes zero conduction heat flux between the greenroof and the nnroof.
YDMT%XG_GREENROOF_ROOF(:) = 0.
!
! On suppose aucune irrigation sur les routes
YDMT%XIRRIG_ROAD(:) = 0.
!
!* one supposes zero runoff from roofs and roads not connected to sewer
YDMT%XNOC_ROAD(:) = 0.
YDMT%XNOC_ROOF(:) = 0.
!
INB_STEP_ATM = 3600*24*4/ZTSTEP
DO JFORC_STEP= 1,INB_STEP_ATM
!
! Daily outdoor air temperature cycle
  ZT_CAN(:) = 10.7/2 * SIN(2*XPI/(24*3600) * (ZTIME+16*3600))  &
                 + (B%XT_SIZE_MAX(:)-10.7/2)
  ZTA(:) = ZT_CAN(:)
!
! No need of fine extrapolation for roof level temperature in hvacautosize
  ZTA_RF(:) = ZTA(:)
  ZQA_RF(:) = ZQA(:)
!
!*      B.1     Solar radiation
!               ---------------
  !
  ! Take radiation from January instead of July for air conditioning capacity
  ! calculation on the south hemisphere.
  !
  IF (SIZE(G%XLAT).GT.0) THEN
     IF ( (SUM(G%XLAT)/SIZE(G%XLAT)).LT.0.0 ) THEN
        IMONTH=1
     ENDIF
  ENDIF
  !
  CALL SUNPOS(JPYEAR, IMONTH, IDAY, ZTIME, G%XLON, G%XLAT, ZTSUN, ZZENITH, ZAZIM)
!
  CALL SW_DAYCYCLE(KI, ZZENITH, ZTOT_SW)
!
  ZDIR_SW(:) = 0.88 * ZTOT_SW(:) * 0.85 ! manual adjustment
  ZSCA_SW(:) = 0.12 * ZTOT_SW(:) * 0.85 ! manual adjustment
  WHERE (ZDIR_SW < 0.0) 
    ZDIR_SW = 0.0
  END WHERE
  WHERE (ZSCA_SW < 0.0) 
    ZSCA_SW = 0.0
  END WHERE
!

! solar panels are not taken into account in the building's HVAC equipment sizing process
  CALL URBAN_SOLAR_ABS(TOP, T, B, YDMT, GDM%P, ZDIR_SW, ZSCA_SW, ZZENITH, ZAZIM,          &
                       ZFRAC_PANEL, ZALB_PANEL, ZALB_GD, ZALB_GR, ZALB_HVEG, ZDN_RF, ZDF_RF, ZDN_RD, &
                       ZDF_RD, ZTRANS_HVCR,                  &
                       ZREC_SW_GD, ZREC_SW_RF, ZDIR_ALB_TWN,  &
                       ZSCA_ALB_TWN, ZREC_SW_WIN, ZREF_SW_GRND,               &
                       ZREF_SW_FAC, ZREF_SW_HVEG, ZE_SHADING, ZAUXSHAD_BEHAV_ANYWAY,      &
                       ZAUXSHAD_BEHAV_ADAPTI)
!
!*      B.2     LW properties
!               -------------
!
  ZEMIS_GD = 1.0
  CALL URBAN_LW_COEF(B, T, LW, ZLW_RAD, ZEMIS_GD,                       &
                     T%TSNOW_ROAD%TS, ZTS_GD,                           &
                     ZEMIS_HVEG, ZTS_HVEG                               )
  !
  ! Explicit calculation of longwave exchanges
  !
  CALL EXPLICIT_LONGWAVE(TOP, T, B, LW, YDMT, CT, ZLW_RAD, ZTS_GD,   &
       ZTS_HVEG, ZDN_RD, ZDF_RD, ZDN_RF, ZEMIS_GD, ZEMIS_HVEG, "OK")
!
!*      B.3     TEB simulation
!               -------------
!
  CALL TEB_BLD_ROAD  (TOP, T, BOP, B, YIR, YDMT, CT, LW, "OFFLINE", YIMPLICIT_WIND, ZTSUN,        &
             ZTA_RF, ZQA_RF, ZT_CAN, ZQ_CAN, ZU_CAN, ZT_CAN, ZQ_CAN, ZU_CAN, ZZ_LOWCAN,  &
             ZPEW_A_COEF, ZPEW_B_COEF, ZPEW_A_COEF, ZPEW_B_COEF, AT, ZPS, ZPSOLD, ZPA,       &
             ZEXNS, ZEXNA, ZTA, ZQA, ZRHOA, ZLW_RAD, ZRR, ZSR, ZZREF, ZZREF, &
             ZU_CAN, T%XH_TRAFFIC, T%XLE_TRAFFIC, ZTSTEP, ZDF_RF, ZDN_RF,    &
             ZDF_RD, ZDN_RD, ZQSAT_RF, ZQSAT_RD,                             &
             ZWS_RF_MAX, ZWS_RD_MAX,                                         &
             ZDELT_RF, ZDELT_RD, ZTS_GD, ZTS_HVEG,                           &
             ZLEW_RF, ZUW_GR, ZLEW_RD, ZRNSNOW_RF,                           &
             ZHSNOW_RF, ZLESNOW_RF, ZGSNOW_RF, ZMELT_RF, ZRN_GR, ZH_GR,      &
             ZLE_GR, ZGFLUX_GR, ZDRAIN_GR, ZRUNOFF_GR, ZRNSNOW_RD,           &
             ZHSNOW_RD, ZLESNOW_RD, ZGSNOW_RD, ZMELT_RD, ZUW_RD, ZUW_RF,     &
             ZDUWDU_RD, ZDUWDU_RF, ZUSTAR_TWN, ZCD, ZCDN, ZCH_TWN, ZRI_TWN,  &
             ZRESA_TWN,                                                      &
             ZCST_H_WASTE_CANY, ZCST_LE_WASTE_CANY,                          &
             ZCOE_H_WASTE_CANY, ZCOE_LE_WASTE_CANY,                          &
             ZMUL_H_WASTE_CANY, ZMUL_LE_WASTE_CANY,                          &
             ZAC_RF, ZAC_RD, ZAC_WL, ZAC_TOP, ZAC_GD, ZAC_RF_WAT,            &
             ZAC_RD_WAT,                                                     &
             IDAY,                                                           &
             ZEMIT_LW_FAC, ZEMIT_LW_RD, ZTIME, ZAUXQIN,                      &
             ZAUXTHEAT_TARGET, ZAUXTCOOL_TARGET,                             &
             ZAUXVENT_BEHAV_ANYWAY, ZAUXVENT_BEHAV_ADAPTI, ZE_SHADING, "OK"  )
! 
!   Time update
  ZTIME = ZTIME + ZTSTEP
  IF (ZTIME >= 86400) THEN
    ZTIME = 0.0
    IDAY = IDAY + 1
  END IF
!
ENDDO
!
! Check results of autosize calculations
!
IF (MINVAL(B%XM_SYS_RAT)   .LT.-1.0E-6) CALL ABOR1_SFX("HVAC_AUTOSIZE: Wrong M_SYS_RAT")
IF (MINVAL(B%XCAP_SYS_HEAT).LT.-1.0E-6) CALL ABOR1_SFX("HVAC_AUTOSIZE: Wrong CAP_SYS_HEAT")
IF (MINVAL(B%XCAP_SYS_RAT ).LT.-1.0E-6) CALL ABOR1_SFX("HVAC_AUTOSIZE: Wrong CAP_SYS_RAT")
!
IF(CT%LCHECK_TEB) CALL DEALLOC_CHECK_TEB(CT)
CALL DEALLOC_LW_COEF(LW)
CALL TEB_IRRIG_INIT(YIR)
CALL DIAG_MISC_TEB_INIT(YDMT)
CALL DIAG_MISC_TEB_INIT(YDMTC)
!----------------------------------------------------
!
! Options
!
TOP%CZ0H = YZ0H
TOP%CBEM = YBEM
TOP%LCANOPY = GCANOPY
TOP%CCH_BEM = YCH_BEM
!
! Teb Fields T
!
DEALLOCATE(T%XT_ROOF,T%XT_ROAD,T%XT_WALL_A,T%XT_WALL_B)
DEALLOCATE(T%XWS_ROOF,T%XWS_ROAD)
!
T%TSNOW_ROOF%SCHEME = YSNOW_RF
DEALLOCATE(T%TSNOW_ROOF%WSNOW)
DEALLOCATE(T%TSNOW_ROOF%T)
DEALLOCATE(T%TSNOW_ROOF%ALB)
DEALLOCATE(T%TSNOW_ROOF%RHO)
DEALLOCATE(T%TSNOW_ROOF%TS)
DEALLOCATE(T%TSNOW_ROOF%EMIS)
!
T%TSNOW_ROAD%SCHEME = YSNOW_RD
DEALLOCATE(T%TSNOW_ROAD%WSNOW)
DEALLOCATE(T%TSNOW_ROAD%T)
DEALLOCATE(T%TSNOW_ROAD%ALB)
DEALLOCATE(T%TSNOW_ROAD%RHO)
DEALLOCATE(T%TSNOW_ROAD%TS)
DEALLOCATE(T%TSNOW_ROAD%EMIS)
!
DEALLOCATE(YT%TSNOW_ROOF%WSNOW)
DEALLOCATE(YT%TSNOW_ROOF%T    )
DEALLOCATE(YT%TSNOW_ROOF%ALB  )
DEALLOCATE(YT%TSNOW_ROOF%RHO  )
DEALLOCATE(YT%TSNOW_ROOF%TS   )
DEALLOCATE(YT%TSNOW_ROOF%EMIS )
!
DEALLOCATE(YT%TSNOW_ROAD%WSNOW)
DEALLOCATE(YT%TSNOW_ROAD%T    )
DEALLOCATE(YT%TSNOW_ROAD%ALB  )
DEALLOCATE(YT%TSNOW_ROAD%RHO  )
DEALLOCATE(YT%TSNOW_ROAD%TS   )
DEALLOCATE(YT%TSNOW_ROAD%EMIS )
!
T%XGARDEN = ZGD 
!
YIR%LPAR_RD_IRRIG = GPAR_RD_IRRIG
YDMTO%LSURF_MISC_BUDGET = GSURF_MISC_BUDGET
!
!------------------------------------------------
!BEM 
!
BOP%CCOOL_COIL = YCOOL_COIL
BOP%CHEAT_COIL = YHEAT_COIL
!
DEALLOCATE(B%XTI_BLD)
!
DEALLOCATE(B%XQI_BLD)
!T_FLOOR, T_MASS
DEALLOCATE(B%XT_FLOOR,B%XT_MASS)
!
!OUTDOOR WINDOW TEMPERATURE
DEALLOCATE(B%XT_WIN1)
DEALLOCATE(B%XT_WIN2)
!
B%XF_WATER_COND(:) = ZF_WATER_COND(:)
B%XAUX_MAX = ZAUX_MAX
!
DEALLOCATE(YTH%XWG_ROAD)
DEALLOCATE(YTH%XWG_BLD)
DEALLOCATE(YTH%XWGI_ROAD)
DEALLOCATE(YTH%XWGI_BLD)
DEALLOCATE(YPE%XWGI)
DEALLOCATE(YPE%XWG)
DEALLOCATE(YPE%TSNOW%WSNOW)
DEALLOCATE(YPE%XWR)
DEALLOCATE(YT%XWS_ROAD)
DEALLOCATE(YT%XWS_ROOF)
!
!--------------------------------------------------------
!
! -----------------------------------------------------------
! Write autosize results
! -----------------------------------------------------------
!
WRITE(KLUOUT,*) ' '
WRITE(KLUOUT,*) '    --------------------------------'
WRITE(KLUOUT,*) '      HVAC AUTOSIZE CALCULATIONS '
WRITE(KLUOUT,*) ' '
WRITE(KLUOUT,*) '    Rated mass flow rate:'
WRITE(KLUOUT,*) '    ',MAXVAL(B%XM_SYS_RAT), 'kg s-1 m-2(bld)'
WRITE(KLUOUT,*) '    ',MINVAL(B%XM_SYS_RAT), 'kg s-1 m-2(bld)'
WRITE(KLUOUT,*) '    Rated cooling system capacity:'
WRITE(KLUOUT,*) '    ',MAXVAL(B%XCAP_SYS_RAT), 'W m-2(bld)'
WRITE(KLUOUT,*) '    ',MINVAL(B%XCAP_SYS_RAT), 'W m-2(bld)'
WRITE(KLUOUT,*) '    Rated heating sysem capacity:'
WRITE(KLUOUT,*) '    ',MAXVAL(B%XCAP_SYS_HEAT), 'W m-2(bld)'
WRITE(KLUOUT,*) '    ',MINVAL(B%XCAP_SYS_HEAT), 'W m-2(bld)'
WRITE(KLUOUT,*) '    --------------------------------'
WRITE(KLUOUT,*) ' '
IF (LHOOK) CALL DR_HOOK('HVAC_AUTOSIZE',1,ZHOOK_HANDLE)
!
CONTAINS 
!
SUBROUTINE INTERP_PROFTWL(PT1, PTN, PD, PT)
!interpolation of vertical profile for 'wall' : roof/wall
!arguments
REAL, DIMENSION(:), INTENT(IN)    :: PT1 !temperature layer 1
REAL, DIMENSION(:), INTENT(IN)    :: PTN !temperature layer N
REAL, DIMENSION(:,:), INTENT(IN)  :: PD  !depth of all layers
REAL, DIMENSION(:,:), INTENT(OUT) :: PT  !temperature of all layers
!local variables
INTEGER :: ILAYER ! number of layers
REAL, DIMENSION(SIZE(PT1)) :: ZDN ! total depth from mid layer 1 to mid layer n
REAL, DIMENSION(SIZE(PT1)) :: ZD  ! sequential depth in the calculation
INTEGER :: JJ, JI

ILAYER=SIZE(PD,2)
DO JI=1,KI
   ZDN(JI) = 0.5 * PD(JI,1)
   DO JJ=2,ILAYER-1
      ZDN(JI) = ZDN(JI) + PD(JI,JJ)
   ENDDO
   ZDN(JI) = ZDN(JI) + 0.5 * PD(JI,ILAYER)
ENDDO
DO JI=1,KI
   ZD(JI) = 0.5*PD(JI,1)
   DO JJ=2,ILAYER-1
      ZD(JI) = ZD(JI) + 0.5*PD(JI,JJ)
      PT(JI,JJ) = PT1(JI) + (PTN(JI)-PT1(JI)) / ZDN(JI) * ZD(JI) 
      ZD(JI) = ZD(JI) + 0.5 * PD(JI,JJ)
   ENDDO
   PT(JI,1) = PT1(JI)
   PT(JI,ILAYER) = PTN(JI)
ENDDO
END SUBROUTINE INTERP_PROFTWL

END SUBROUTINE HVAC_AUTOSIZE
