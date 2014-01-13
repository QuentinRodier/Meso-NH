!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #############################################################
SUBROUTINE HVAC_AUTOSIZE(KI,KLUOUT)
!     #############################################################
!
USE MODD_BEM_n, ONLY : NFLOOR_LAYER, XHC_FLOOR, XTC_FLOOR, XD_FLOOR,       &
                       XTCOOL_TARGET, XTHEAT_TARGET, XF_WASTE_CAN,         &
                       XEFF_HEAT, XQIN, XQIN_FRAD, XQIN_FLAT, XSHGC,       &
                       XSHGC_SH,XU_WIN, XGR, XFLOOR_HEIGHT, XINF,          &
                       LAUTOSIZE, XHR_TARGET, XV_VENT, XCAP_SYS_HEAT,      &
                       XCAP_SYS_RAT, XT_ADP, XM_SYS_RAT, XCOP_RAT,         &
                       XALB_WIN, XABS_WIN, XT_SIZE_MAX, XT_SIZE_MIN,       &
                       XUGG_WIN, XN_FLOOR, XGLAZ_O_BLD, XMASS_O_BLD,       &
                       XFLOOR_HW_RATIO, XF_FLOOR_MASS, XF_FLOOR_WALL,      &
                       XF_FLOOR_WIN, XF_FLOOR_ROOF, XF_WALL_FLOOR,         &
                       XF_WALL_MASS, XF_WALL_WIN, XF_WIN_FLOOR,            &
                       XF_WIN_MASS, XF_WIN_WALL, XF_MASS_FLOOR,            &
                       XF_MASS_WALL, XF_MASS_WIN, XTRAN_WIN, XF_WIN_WIN

USE MODD_TEB_n, ONLY : NROOF_LAYER, NWALL_LAYER, NROAD_LAYER, XGARDEN,     &
                       XBLD, XROAD, XCAN_HW_RATIO, XBLD_HEIGHT,            &
                       XWALL_O_HOR, XWALL_O_GRND, XWALL_O_BLD, XZ0_TOWN,   &
                       XSVF_ROAD, XSVF_GARDEN, XSVF_WALL,                  &
                       CROAD_DIR, XROAD_DIR, CWALL_OPT,                    &
                       XALB_ROOF, XEMIS_ROOF, XHC_ROOF, XTC_ROOF, XD_ROOF, &
                       XALB_ROAD, XEMIS_ROAD, XHC_ROAD, XTC_ROAD, XD_ROAD, &
                       XALB_WALL, XEMIS_WALL, XHC_WALL, XTC_WALL, XD_WALL, &
                       XH_TRAFFIC, XLE_TRAFFIC, XH_INDUSTRY, XLE_INDUSTRY, &
                       XROUGH_ROOF, XROUGH_WALL, XGREENROOF


USE MODD_CSTS,            ONLY : XCPD, XPI, XP00, XRD, XSTEFAN
USE MODD_TEB_GRID_n,      ONLY : XLAT, XLON
!
USE MODD_SURFEX_OMP, ONLY : NBLOCKTOT
!
USE MODI_TEB
USE MODI_SUNPOS
USE MODI_SW_DAYCYCLE
USE MODI_URBAN_LW_COEF
USE MODI_URBAN_SOLAR_ABS
USE MODI_GET_SIZES_PARALLEL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!arguments
INTEGER,       INTENT(IN)    :: KI     ! number of points
INTEGER,       INTENT(IN)    :: KLUOUT ! output listing logical unit

!local parameters
LOGICAL, PARAMETER :: GCANOPY=.FALSE.
 CHARACTER(LEN=3), PARAMETER :: YBEM='BEM'

REAL, PARAMETER     :: PPTSTEP      = 300.
INTEGER, PARAMETER  :: JPYEAR       = 2004   ! Current year (UTC)

!local variable
INTEGER             :: IMONTH ! Current month (UTC)
INTEGER             :: IDAY   ! Current day (UTC)
REAL                :: ZTIME  ! Time at start of the run (s)
!
REAL, DIMENSION(KI) :: ZU_ROOF
REAL, DIMENSION(KI) :: ZU_WALL
REAL, DIMENSION(KI) :: ZT_CANYON
REAL, DIMENSION(KI) :: ZQ_CANYON
REAL, DIMENSION(KI) :: ZU_CANYON
REAL, DIMENSION(KI) :: ZZ_LOWCAN
REAL, DIMENSION(KI) :: ZDIR_SW
REAL, DIMENSION(KI) :: ZSCA_SW
!
 CHARACTER(LEN=3)    :: HIMPLICIT_WIND
 CHARACTER(LEN=6)    :: HZ0H 
 CHARACTER(LEN=5)    :: YCH_BEM 
INTEGER             :: JJ
INTEGER             :: JFORC_STEP
INTEGER             :: INB_STEP_ATM
!
!! GREGOIRE 13/03
REAL, DIMENSION(KI) :: ZROAD
REAL, DIMENSION(KI) :: ZGARDEN
REAL, DIMENSION(KI) :: ZSVF_GARDEN
!! GREGOIRE 13/03
REAL, DIMENSION(KI) :: ZZREF
REAL, DIMENSION(KI) :: ZPS
REAL, DIMENSION(KI) :: ZPA
REAL, DIMENSION(KI) :: ZEXNS
REAL, DIMENSION(KI) :: ZEXNA
REAL, DIMENSION(KI) :: ZTA
REAL, DIMENSION(KI) :: ZQA
REAL, DIMENSION(KI) :: ZRHOA
REAL, DIMENSION(KI) :: ZLW_RAD
REAL, DIMENSION(KI) :: ZASNOW_ROOF
REAL, DIMENSION(KI) :: ZASNOW_ROAD
REAL, DIMENSION(KI) :: ZDN_ROOF
REAL, DIMENSION(KI) :: ZDF_ROOF
REAL, DIMENSION(KI) :: ZDN_ROAD
REAL, DIMENSION(KI) :: ZDF_ROAD
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
REAL, DIMENSION(KI) :: ZRR
REAL, DIMENSION(KI) :: ZSR
REAL, DIMENSION(KI) :: ZQSAT_ROOF
REAL, DIMENSION(KI) :: ZQSAT_ROAD
REAL, DIMENSION(KI) :: ZDELT_ROOF
REAL, DIMENSION(KI) :: ZDELT_ROAD
REAL, DIMENSION(KI) :: ZABS_SW_ROOF
REAL, DIMENSION(KI) :: ZABS_SW_ROAD
REAL, DIMENSION(KI) :: ZABS_SW_WALL_A
REAL, DIMENSION(KI) :: ZABS_SW_WALL_B
REAL, DIMENSION(KI) :: ZABS_SW_GARDEN
REAL, DIMENSION(KI) :: ZABS_SW_GREENROOF
REAL, DIMENSION(KI) :: ZABS_SW_SNOW_ROOF
REAL, DIMENSION(KI) :: ZABS_SW_SNOW_ROAD
REAL, DIMENSION(KI) :: ZREC_SW_ROAD
REAL, DIMENSION(KI) :: ZREC_SW_SNOW_ROAD
REAL, DIMENSION(KI) :: ZREC_SW_WALL_A
REAL, DIMENSION(KI) :: ZREC_SW_WALL_B
REAL, DIMENSION(KI) :: ZREC_SW_GARDEN
REAL, DIMENSION(KI) :: ZABS_LW_ROOF      ! absorbed IR rad by roof
REAL, DIMENSION(KI) :: ZABS_LW_SNOW_ROOF ! absorbed IR rad by snow on roof
REAL, DIMENSION(KI) :: ZABS_LW_ROAD      ! absorbed IR rad by road
REAL, DIMENSION(KI) :: ZABS_LW_SNOW_ROAD ! absorbed IR rad by snow on road
REAL, DIMENSION(KI) :: ZABS_LW_WALL_A    ! absorbed IR rad by wall
REAL, DIMENSION(KI) :: ZABS_LW_WALL_B    ! absorbed IR rad by wall
REAL, DIMENSION(KI) :: ZDIR_ALB_TOWN
REAL, DIMENSION(KI) :: ZSCA_ALB_TOWN
REAL, DIMENSION(KI) :: ZSW_RAD_GARDEN
REAL, DIMENSION(KI) :: ZABS_SW_WIN
REAL, DIMENSION(KI) :: ZREC_SW_WIN
REAL, DIMENSION(KI) :: ZREF_SW_GRND
REAL, DIMENSION(KI) :: ZREF_SW_FAC
REAL,DIMENSION(KI) :: ZT1    ! intermediate variable
REAL,DIMENSION(KI) :: ZTN    ! intermediate variable
REAL,DIMENSION(KI,NROAD_LAYER ) :: ZT_ROAD    ! road layers temperatures
REAL,DIMENSION(KI,NROOF_LAYER) :: ZT_ROOF    ! roof layers temperatures
REAL,DIMENSION(KI,NWALL_LAYER) :: ZT_WALL_A  ! wall layers temperatures
REAL,DIMENSION(KI,NWALL_LAYER) :: ZT_WALL_B  ! wall layers temperatures
REAL,DIMENSION(KI,NFLOOR_LAYER ) :: ZT_FLOOR   ! building floor temperature
REAL,DIMENSION(KI,NFLOOR_LAYER ) :: ZT_MASS    ! building mass temperature
REAL, DIMENSION(KI) :: ZTS_GARDEN
REAL, DIMENSION(KI) :: ZT_WIN1
REAL, DIMENSION(KI) :: ZLW_WA_TO_WB   ! longwave exchange coefficients
REAL, DIMENSION(KI) :: ZLW_WA_TO_R
REAL, DIMENSION(KI) :: ZLW_WB_TO_R
REAL, DIMENSION(KI) :: ZLW_WA_TO_NR
REAL, DIMENSION(KI) :: ZLW_WB_TO_NR
REAL, DIMENSION(KI) :: ZLW_WA_TO_G
REAL, DIMENSION(KI) :: ZLW_WB_TO_G
REAL, DIMENSION(KI) :: ZLW_WA_TO_WIN
REAL, DIMENSION(KI) :: ZLW_WB_TO_WIN
REAL, DIMENSION(KI) :: ZLW_R_TO_WA
REAL, DIMENSION(KI) :: ZLW_R_TO_WB
REAL, DIMENSION(KI) :: ZLW_R_TO_WIN
REAL, DIMENSION(KI) :: ZLW_G_TO_WA
REAL, DIMENSION(KI) :: ZLW_G_TO_WB
REAL, DIMENSION(KI) :: ZLW_G_TO_WIN
REAL, DIMENSION(KI) :: ZLW_S_TO_WA
REAL, DIMENSION(KI) :: ZLW_S_TO_WB
REAL, DIMENSION(KI) :: ZLW_S_TO_R
REAL, DIMENSION(KI) :: ZLW_S_TO_NR
REAL, DIMENSION(KI) :: ZLW_S_TO_G
REAL, DIMENSION(KI) :: ZLW_S_TO_WIN
REAL, DIMENSION(KI) :: ZLW_WIN_TO_WA
REAL, DIMENSION(KI) :: ZLW_WIN_TO_WB
REAL, DIMENSION(KI) :: ZLW_WIN_TO_R
REAL, DIMENSION(KI) :: ZLW_WIN_TO_NR
REAL, DIMENSION(KI) :: ZLW_WIN_TO_G
REAL, DIMENSION(KI) :: ZLW_NR_TO_WA
REAL, DIMENSION(KI) :: ZLW_NR_TO_WB
REAL, DIMENSION(KI) :: ZLW_NR_TO_WIN
REAL, DIMENSION(KI) :: ZTI_BLD
REAL, DIMENSION(KI) :: ZRN_ROOF      ! net radiation over roof
REAL, DIMENSION(KI) :: ZH_ROOF       ! sensible heat flux over roof
REAL, DIMENSION(KI) :: ZLE_ROOF      ! latent heat flux over roof
REAL, DIMENSION(KI) :: ZLEW_ROOF     ! latent heat flux over roof (snow)
REAL, DIMENSION(KI) :: ZGFLUX_ROOF   ! flux through the roof
REAL, DIMENSION(KI) :: ZRUNOFF_ROOF  ! runoff over the ground
REAL, DIMENSION(KI) :: ZRN_ROAD      ! net radiation over road
REAL, DIMENSION(KI) :: ZH_ROAD       ! sensible heat flux over road
REAL, DIMENSION(KI) :: ZLE_ROAD      ! latent heat flux over road
REAL, DIMENSION(KI) :: ZLEW_ROAD     ! latent heat flux over road (snow)
REAL, DIMENSION(KI) :: ZGFLUX_ROAD   ! flux through the road
REAL, DIMENSION(KI) :: ZRUNOFF_ROAD  ! runoff over the ground
REAL, DIMENSION(KI) :: ZRN_WALL_A    ! net radiation over wall
REAL, DIMENSION(KI) :: ZH_WALL_A     ! sensible heat flux over wall
REAL, DIMENSION(KI) :: ZLE_WALL_A    ! latent heat flux over wall
REAL, DIMENSION(KI) :: ZGFLUX_WALL_A ! flux through the wall
REAL, DIMENSION(KI) :: ZRN_WALL_B    ! net radiation over wall
REAL, DIMENSION(KI) :: ZH_WALL_B     ! sensible heat flux over wall
REAL, DIMENSION(KI) :: ZLE_WALL_B    ! latent heat flux over wall
REAL, DIMENSION(KI) :: ZGFLUX_WALL_B ! flux through the wall
REAL, DIMENSION(KI) :: ZRN_BLT       ! net radiation over built surf 
REAL, DIMENSION(KI) :: ZH_BLT        ! sensible heat flux over built surf 
REAL, DIMENSION(KI) :: ZLE_BLT       ! latent heat flux over built surf 
REAL, DIMENSION(KI) :: ZGFLUX_BLT    ! flux through the built surf 
REAL, DIMENSION(KI) :: ZRNSNOW_ROOF  ! net radiation over snow
REAL, DIMENSION(KI) :: ZHSNOW_ROOF   ! sensible heat flux over snow
REAL, DIMENSION(KI) :: ZLESNOW_ROOF  ! latent heat flux over snow
REAL, DIMENSION(KI) :: ZGSNOW_ROOF   ! flux under the snow
REAL, DIMENSION(KI) :: ZMELT_ROOF    ! snow melt
REAL, DIMENSION(KI) :: ZRNSNOW_ROAD  ! net radiation over snow
REAL, DIMENSION(KI) :: ZHSNOW_ROAD   ! sensible heat flux over snow
REAL, DIMENSION(KI) :: ZLESNOW_ROAD  ! latent heat flux over snow
REAL, DIMENSION(KI) :: ZGSNOW_ROAD   ! flux under the snow
REAL, DIMENSION(KI) :: ZMELT_ROAD    ! snow melt
REAL, DIMENSION(KI) :: ZRUNOFF_TOWN  ! runoff over the ground
REAL, DIMENSION(KI) :: ZUW_ROAD      ! Momentum flux for roads
REAL, DIMENSION(KI) :: ZUW_ROOF      ! Momentum flux for roofs
REAL, DIMENSION(KI) :: ZDUWDU_ROAD   !
REAL, DIMENSION(KI) :: ZDUWDU_ROOF   !
REAL, DIMENSION(KI) :: ZUSTAR_TOWN   ! friction velocity over town
REAL, DIMENSION(KI) :: ZCD           ! town averaged drag coefficient
REAL, DIMENSION(KI) :: ZCDN          ! town averaged neutral drag coefficient
REAL, DIMENSION(KI) :: ZCH_TOWN      ! town averaged heat transfer coefficient
REAL, DIMENSION(KI) :: ZRI_TOWN      ! town averaged Richardson number
REAL, DIMENSION(KI) :: ZRESA_TOWN    ! town aerodynamical resistance
REAL, DIMENSION(KI) :: ZDQS_TOWN     ! heat storage inside town
REAL, DIMENSION(KI) :: ZQF_TOWN      ! total anthropogenic heat
REAL, DIMENSION(KI) :: ZQF_BLD       ! anthropogenic heat flux of domestic heating  
REAL, DIMENSION(KI) :: ZFLX_BLD      ! heat flux between inside of the bld
REAL, DIMENSION(KI) :: ZAC_ROOF      ! roof conductance
REAL, DIMENSION(KI) :: ZAC_ROAD       ! road conductance
REAL, DIMENSION(KI) :: ZAC_WALL      ! wall conductance
REAL, DIMENSION(KI) :: ZAC_TOP       ! top conductance
REAL, DIMENSION(KI) :: ZAC_GARDEN     ! garden conductance
REAL, DIMENSION(KI) :: ZAC_ROOF_WAT  ! roof water conductance
REAL, DIMENSION(KI) :: ZAC_ROAD_WAT  ! roof water conductance 
REAL, DIMENSION(KI) :: ZH_BLD_COOL
REAL, DIMENSION(KI) :: ZT_BLD_COOL
REAL, DIMENSION(KI) :: ZH_BLD_HEAT
REAL, DIMENSION(KI) :: ZLE_BLD_COOL
REAL, DIMENSION(KI) :: ZLE_BLD_HEAT
REAL, DIMENSION(KI) :: ZH_WASTE
REAL, DIMENSION(KI) :: ZLE_WASTE
REAL, DIMENSION(KI) :: ZHVAC_COOL
REAL, DIMENSION(KI) :: ZHVAC_HEAT
REAL, DIMENSION(KI) :: ZT_WIN2
REAL, DIMENSION(KI) :: ZQI_BLD
REAL, DIMENSION(KI) :: ZM_SYS
REAL, DIMENSION(KI) :: ZQ_SYS
REAL, DIMENSION(KI) :: ZT_SYS
REAL, DIMENSION(KI) :: ZTR_SW_WIN
REAL, DIMENSION(KI) :: ZFAN_POWER
REAL, DIMENSION(KI) :: ZABS_LW_WIN
REAL, DIMENSION(KI) :: ZEMIT_LW_FAC
REAL, DIMENSION(KI) :: ZEMIT_LW_ROAD
REAL, DIMENSION(KI) :: ZT_RAD_IND
REAL, DIMENSION(KI) :: ZHU_BLD
REAL, DIMENSION(KI) :: ZTSUN
REAL, DIMENSION(KI) :: ZZENITH
REAL, DIMENSION(KI) :: ZAZIM
REAL, DIMENSION(KI) :: ZALB_GARDEN
REAL, DIMENSION(KI) :: ZALB_GREENROOF
REAL, DIMENSION(KI) :: ZAUX_MAX
REAL, DIMENSION(KI) :: ZCAP_SYS
REAL, DIMENSION(KI) :: ZCOP
REAL, DIMENSION(KI) :: ZPEW_A_COEF
REAL, DIMENSION(KI) :: ZPEW_B_COEF
REAL, DIMENSION(KI) :: ZTOT_SW
REAL, DIMENSION(KI) :: ZTOUT_EQ
REAL, DIMENSION(KI) :: ZT_SKY
REAL, DIMENSION(KI) :: ZF_WATER_COND
!
!new for shading
REAL, DIMENSION(KI) :: ZE_SHADING
LOGICAL, DIMENSION(KI) :: GSHAD_DAY
LOGICAL, DIMENSION(KI) :: GSHADE
LOGICAL, DIMENSION(KI) :: GNATVENT_NIGHT
!
 CHARACTER(LEN=6)    :: YCOOL_COIL
 CHARACTER(LEN=6)    :: YHEAT_COIL
 CHARACTER(LEN=4),DIMENSION(KI) :: YNATVENT
!
! Case greenroof
REAL, DIMENSION(KI) :: ZRN_GREENROOF
REAL, DIMENSION(KI) :: ZH_GREENROOF
REAL, DIMENSION(KI) :: ZLE_GREENROOF
REAL, DIMENSION(KI) :: ZGFLUX_GREENROOF
REAL, DIMENSION(KI) :: ZUW_GREENROOF
REAL, DIMENSION(KI) :: ZG_GREENROOF_ROOF
REAL, DIMENSION(KI) :: ZRN_STRLROOF
REAL, DIMENSION(KI) :: ZH_STRLROOF
REAL, DIMENSION(KI) :: ZLE_STRLROOF
REAL, DIMENSION(KI) :: ZGFLUX_STRLROOF
!
INTEGER, DIMENSION(:), ALLOCATABLE :: ISIZE_OMP
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.     Initialization
!              --------------
!
IF (LHOOK) CALL DR_HOOK('HVAC_AUTOSIZE',0,ZHOOK_HANDLE)
!
HIMPLICIT_WIND = 'NEW'
!
!    Date
IMONTH = 7
IDAY   = 12
ZTIME  = 0.
!
!    Design parameters
ZZREF =  50.
ZPS   = 101325.
ZQA   = 0.011
ZU_CANYON = 2.5 
ZAUX_MAX = 0.
ZT_SKY = 253.15
!
!
!    Initialization
XM_SYS_RAT    = 0.
XCAP_SYS_RAT  = 0.
XCAP_SYS_HEAT = 0.
ZLW_RAD= 300.
!
!   Non-used parameters
ZZ_LOWCAN = ZZREF
HZ0H = 'KAND07'
YCH_BEM = 'DOE-2'
 

INB_STEP_ATM = 3600*24*4/PPTSTEP
ZPA = ZPS
ZEXNS = (ZPS/XP00)**(XRD/XCPD)
ZEXNA = (ZPA/XP00)**(XRD/XCPD)
HSNOW_ROOF = 'NONE'
HSNOW_ROAD = 'NONE'
ZASNOW_ROOF = 0.8
ZASNOW_ROAD = 0.8
ZDN_ROOF = 0.0
ZDF_ROOF = 1.0
ZDN_ROAD = 0.0
ZDF_ROAD = 1.0
ZEMIS_GARDEN = 1.0
ZESNOW_ROAD = 1.0
ZTSSNOW_ROAD = 273.0
ZWS_ROOF = 0.0
ZWS_ROAD = 0.0
ZWSNOW_ROOF = 0.0
ZTSNOW_ROOF = 273.0
ZRSNOW_ROOF = 0.0
ZTSSNOW_ROOF = 273.0
ZESNOW_ROOF = 0.0
ZWSNOW_ROAD = 0.0
ZTSNOW_ROAD = 273.0
ZRSNOW_ROAD = 0.0
ZRR = 0.0 
ZSR = 0.0 
ZQSAT_ROOF = 0.015
ZQSAT_ROAD = 0.015
ZDELT_ROOF = 0.0 
ZDELT_ROAD = 0.0 
ZTS_GARDEN = 300.
ZPEW_A_COEF  = 0.5  
ZPEW_B_COEF  = 0.5 
ZE_SHADING(:) = 0.
GNATVENT_NIGHT(:) = .FALSE.
GSHADE        (:) = .FALSE.
GSHAD_DAY     (:) = .FALSE.
!
! Case greenroofs
ZRN_GREENROOF    (:) = 0.
ZH_GREENROOF     (:) = 0.
ZLE_GREENROOF    (:) = 0.
ZGFLUX_GREENROOF (:) = 0.
ZUW_GREENROOF    (:) = 0.
!* one supposes zero conduction heat flux between the greenroof and the roof.
ZG_GREENROOF_ROOF(:) = 0.
!
!*      A.     Autosize of the heating system
!              ---------------------------------
!
YCOOL_COIL = 'IDEAL '
YHEAT_COIL = 'IDEAL '
YNATVENT(:) = 'NONE'
ZF_WATER_COND(:) = 0.
ZRHOA = 1.30
ZTOUT_EQ(:) = (XT_SIZE_MIN(:) + ZT_SKY(:))/2.
!
ZU_ROOF(:) = 0.0
DO JJ=1,NROOF_LAYER
  ZU_ROOF(:) = ZU_ROOF(:) + XD_ROOF(:,JJ)/XTC_ROOF(:,JJ)
END DO
ZU_ROOF(:) = ZU_ROOF(:) + 1./10. + 1./25.         
ZU_ROOF(:) = 1. / ZU_ROOF(:)
!
ZU_WALL(:) = 0.0
DO JJ=1,NWALL_LAYER
  ZU_WALL(:) = ZU_WALL(:) + XD_WALL(:,JJ)/XTC_WALL(:,JJ)
END DO
ZU_WALL(:) = ZU_WALL(:) + 1./10. + 1./25.         
ZU_WALL(:) = 1. / ZU_WALL(:)
!
!   Heating Coil Capacity [W m-2(bld)]
XCAP_SYS_HEAT(:) = ZU_WALL(:) * XWALL_O_BLD(:) * (XTHEAT_TARGET(:) - ZTOUT_EQ(:)) &
                 + XU_WIN(:)  * XGLAZ_O_BLD(:)  * (XTHEAT_TARGET(:) - ZTOUT_EQ(:)) &
                 + ZU_ROOF(:)              * (XTHEAT_TARGET(:) - ZTOUT_EQ(:)) &
                 - XQIN(:) * XBLD_HEIGHT(:) / XFLOOR_HEIGHT(:)*             &
                   (1 - XQIN_FLAT(:))                                        &
                 + XINF(:) * XBLD_HEIGHT(:) / 3600* ZRHOA(:) * XCPD *       &
                   (XTHEAT_TARGET(:) - XT_SIZE_MIN(:)) &
                 + XV_VENT(:) * XBLD_HEIGHT(:) / 3600* ZRHOA(:) * XCPD *    &
                   (XTHEAT_TARGET(:) - XT_SIZE_MIN(:))
!
!   Rated air flow rate [kg s-1 m-2(bld)]
XM_SYS_RAT(:)   = XCAP_SYS_HEAT(:)/XCPD/(323.15 - XTHEAT_TARGET(:))
!
!
!*      B.     Autosize of the cooling system
!              -----------------------------------
!
ZRHOA = 1.15
!    Initial values
! initial value for air temperature and outdoor wall/roof/window/road temperature
ZT_CANYON(:) = 10.7/2 * SIN(2*XPI/(24*3600) * (ZTIME+16*3600)) + (XT_SIZE_MAX(:)-10.7/2)
!! !ZTI_BLD   = 297.16 ! indoor air temperature
DO JJ=1,KI
    ZTI_BLD(JJ) = MAX(XTHEAT_TARGET(JJ),ZT_CANYON(JJ)) ! indoor air temperature
ENDDO
ZT_ROOF  (:,NROOF_LAYER)   = ZTI_BLD(:)   ! roof layers temperatures
ZT_WALL_A(:,NWALL_LAYER)   = ZTI_BLD(:)   ! wall layers temperatures
DO JJ=1,NFLOOR_LAYER
   ZT_FLOOR(:,JJ)  = ZTI_BLD(:) ! building floor temperature
   ZT_MASS(:,JJ)   = ZTI_BLD(:) ! building mass temperature
ENDDO

!ROAD
DO JJ=1,NROAD_LAYER
   ZT_ROAD(:,JJ) = ZT_CANYON(:)
ENDDO
!ROOF
ZT_ROOF(:,1) = ZT_CANYON(:)
ZT1(:)=ZT_ROOF(:,1)
ZTN(:)=ZT_ROOF(:,NROOF_LAYER)
IF (NROOF_LAYER .GT. 2) CALL INTERP_PROFTWALL(ZT1, ZTN, XD_ROOF, ZT_ROOF)
!WALL
ZT_WALL_A(:,1) = ZT_CANYON(:)
ZT1(:)=ZT_WALL_A(:,1)
ZTN(:)=ZT_WALL_A(:,NWALL_LAYER)
IF (NWALL_LAYER .GT. 2) CALL INTERP_PROFTWALL(ZT1, ZTN, XD_WALL, ZT_WALL_A)
ZT_WALL_B = ZT_WALL_A
!OUTDOOR WINDOW TEMPERATURE
ZT_WIN1(:) = ZT_CANYON(:)
!! 
ZT_WIN2(:)   = ZTI_BLD(:)
!! 
ZQ_CANYON = 0.011
ZQI_BLD   = 0.011
ZT_SYS = ZTI_BLD
ZQ_SYS = ZQI_BLD
!
!! GREGOIRE 13/03
ZROAD         (:) = XROAD(:)+XGARDEN(:)
ZGARDEN       (:) = 0.
ZALB_GARDEN   (:) = 0.
ZALB_GREENROOF(:) = 0.
ZAC_GARDEN    (:) = 0.
ZSVF_GARDEN   (:) = 0.
!! GREGOIRE 13/03

ALLOCATE(ISIZE_OMP(0:NBLOCKTOT-1))
 CALL GET_SIZES_PARALLEL(NBLOCKTOT,KI,0,ISIZE_OMP)

DO JFORC_STEP= 1,INB_STEP_ATM
!
!   Daily outdoor air temperature cycle
    ZT_CANYON(:) = 10.7/2 * SIN(2*XPI/(24*3600) * (ZTIME+16*3600))  &
              + (XT_SIZE_MAX(:)-10.7/2)
    ZTA(:) = ZT_CANYON(:)
!
!
!*      B.1     Solar radiation
!               ---------------
!
    CALL SUNPOS(ISIZE_OMP, JPYEAR, IMONTH, IDAY, ZTIME, XLON, XLAT, ZTSUN, ZZENITH, ZAZIM)
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
    CALL URBAN_SOLAR_ABS(YBEM, CROAD_DIR, CWALL_OPT,               &
                     ZDIR_SW, ZSCA_SW, ZZENITH, ZAZIM,             &
                     XBLD, ZGARDEN, XROAD_DIR, XROAD, XGREENROOF,  &
                     XWALL_O_HOR, XCAN_HW_RATIO,                   &
                     XALB_ROOF,                                    &
                     XALB_ROAD, XSVF_ROAD, XALB_WALL, XSVF_WALL,   &
                     ZALB_GARDEN, ZSVF_GARDEN,                     &
                     ZALB_GREENROOF,                               &
                     ZASNOW_ROOF, ZASNOW_ROAD,                     &
                     ZDN_ROOF, ZDF_ROOF, ZDN_ROAD, ZDF_ROAD,       &
                     XGR, XABS_WIN, XSHGC, XSHGC_SH, XALB_WIN,     &                     
                     ZABS_SW_ROOF, ZABS_SW_ROAD,                   &
                     ZABS_SW_WALL_A, ZABS_SW_WALL_B,               &
                     ZABS_SW_GARDEN, ZABS_SW_GREENROOF,            &
                     ZABS_SW_SNOW_ROOF, ZABS_SW_SNOW_ROAD,         &
                     ZREC_SW_ROAD,  ZREC_SW_SNOW_ROAD,             &
                     ZREC_SW_WALL_A, ZREC_SW_WALL_B,               &
                     ZREC_SW_GARDEN,                               &
                     ZDIR_ALB_TOWN,ZSCA_ALB_TOWN,                  &
                     ZSW_RAD_GARDEN, ZABS_SW_WIN, ZREC_SW_WIN,     &
                     XTRAN_WIN,                                    &
                     ZREF_SW_GRND, ZREF_SW_FAC, ZTR_SW_WIN,        &
                     ZE_SHADING, GSHAD_DAY, GSHADE                 )
!
!
!*      B.2     LW properties
!               -------------
!
   CALL URBAN_LW_COEF(XGR, XBLD, ZLW_RAD,                                &
                      XEMIS_ROAD, XSVF_ROAD, XEMIS_WALL, XSVF_WALL,      &
                      ZEMIS_GARDEN, XROAD, ZGARDEN,                      &
                      ZESNOW_ROAD,                                       &
                      ZTSSNOW_ROAD, ZT_WALL_A(:,1), ZT_WALL_B(:,1),      &
                      ZT_ROAD(:,1), ZTS_GARDEN, ZT_WIN1,                 &  
                      ZLW_WA_TO_WB, ZLW_WA_TO_R, ZLW_WB_TO_R,            &
                      ZLW_WA_TO_NR,ZLW_WB_TO_NR,                         &
                      ZLW_WA_TO_G, ZLW_WB_TO_G,                          &
                      ZLW_WA_TO_WIN, ZLW_WB_TO_WIN,                      &
                      ZLW_R_TO_WA, ZLW_R_TO_WB, ZLW_R_TO_WIN,            &
                      ZLW_G_TO_WA, ZLW_G_TO_WB, ZLW_G_TO_WIN,            &
                      ZLW_S_TO_WA, ZLW_S_TO_WB, ZLW_S_TO_R,              &
                      ZLW_S_TO_NR, ZLW_S_TO_G, ZLW_S_TO_WIN,             &
                      ZLW_WIN_TO_WA, ZLW_WIN_TO_WB, ZLW_WIN_TO_R,        &
                      ZLW_WIN_TO_NR, ZLW_WIN_TO_G,                       &
                      ZLW_NR_TO_WA, ZLW_NR_TO_WB, ZLW_NR_TO_WIN          )
!
!*      B.3     TEB simulation
!               -------------
!
    CALL TEB  (HZ0H, HIMPLICIT_WIND, CWALL_OPT, YBEM,                 &
             ZT_CANYON, ZQ_CANYON, ZU_CANYON,                         &
             ZT_CANYON, ZQ_CANYON, ZU_CANYON, ZZ_LOWCAN, ZTI_BLD,     &
             ZT_ROOF, ZT_ROAD, ZT_WALL_A, ZT_WALL_B,                  &
             ZWS_ROOF, ZWS_ROAD,                                      &
             HSNOW_ROOF, ZWSNOW_ROOF, ZTSNOW_ROOF, ZRSNOW_ROOF,       &
             ZASNOW_ROOF, ZTSSNOW_ROOF, ZESNOW_ROOF,                  &
             HSNOW_ROAD, ZWSNOW_ROAD, ZTSNOW_ROAD, ZRSNOW_ROAD,       &
             ZASNOW_ROAD, ZTSSNOW_ROAD, ZESNOW_ROAD,                  &
             ZPEW_A_COEF, ZPEW_B_COEF,                                &
             ZPEW_A_COEF, ZPEW_B_COEF,                                &
             ZPS, ZPA, ZEXNS, ZEXNA, ZTA, ZQA, ZRHOA, ZLW_RAD,        &
             ZRR, ZSR, ZZREF, ZZREF, ZU_CANYON,                       &
             XH_TRAFFIC, XLE_TRAFFIC, XH_INDUSTRY, XLE_INDUSTRY,      &
             PPTSTEP, XZ0_TOWN, XBLD, ZGARDEN, XROAD, XGREENROOF,     &
             XBLD_HEIGHT, XWALL_O_HOR, XCAN_HW_RATIO, XWALL_O_GRND,   &
             ZDF_ROOF, ZDN_ROOF, ZDF_ROAD,                            &
             ZDN_ROAD, ZQSAT_ROOF, ZQSAT_ROAD, ZDELT_ROOF, ZDELT_ROAD,&
             XEMIS_ROOF, XHC_ROOF, XTC_ROOF, XD_ROOF,                 &
             XEMIS_ROAD, XHC_ROAD, XTC_ROAD,                          &
             XD_ROAD, XEMIS_WALL, ZTS_GARDEN,                         &
             XHC_WALL, XTC_WALL, XD_WALL, ZRN_ROOF, ZH_ROOF, ZLE_ROOF,&
             ZLEW_ROOF, ZGFLUX_ROOF, ZRUNOFF_ROOF,                    &
             ZRN_GREENROOF, ZH_GREENROOF, ZLE_GREENROOF,              &
             ZGFLUX_GREENROOF, ZUW_GREENROOF,                         &
             ZRN_STRLROOF, ZH_STRLROOF, ZLE_STRLROOF, ZGFLUX_STRLROOF,&
             ZRN_ROAD, ZH_ROAD,                                       &
             ZLE_ROAD, ZLEW_ROAD, ZGFLUX_ROAD, ZRUNOFF_ROAD,          &
             ZRN_WALL_A, ZH_WALL_A, ZLE_WALL_A, ZGFLUX_WALL_A,        &
             ZRN_WALL_B, ZH_WALL_B, ZLE_WALL_B, ZGFLUX_WALL_B,        &
             ZRN_BLT, ZH_BLT, ZLE_BLT, ZGFLUX_BLT,                    &
             ZRNSNOW_ROOF, ZHSNOW_ROOF, ZLESNOW_ROOF, ZGSNOW_ROOF,    &
             ZMELT_ROOF,                                              &
             ZRNSNOW_ROAD, ZHSNOW_ROAD, ZLESNOW_ROAD, ZGSNOW_ROAD,    &
             ZMELT_ROAD,                                              &
             ZG_GREENROOF_ROOF,                                       &
             ZRUNOFF_TOWN,                                            &
             ZUW_ROAD, ZUW_ROOF, ZDUWDU_ROAD, ZDUWDU_ROOF,            &
             ZUSTAR_TOWN, ZCD, ZCDN, ZCH_TOWN, ZRI_TOWN,              &
             ZRESA_TOWN, ZDQS_TOWN, ZQF_TOWN, ZQF_BLD, ZFLX_BLD,      &
             ZAC_ROOF, ZAC_ROAD, ZAC_WALL, ZAC_TOP, ZAC_GARDEN,       &
             ZAC_ROOF_WAT, ZAC_ROAD_WAT, ZABS_SW_ROOF, ZABS_LW_ROOF,  &
             ZABS_SW_SNOW_ROOF, ZABS_LW_SNOW_ROOF, ZABS_SW_ROAD,      &
             ZABS_LW_ROAD, ZABS_SW_SNOW_ROAD, ZABS_LW_SNOW_ROAD,      &
             ZABS_SW_WALL_A, ZABS_LW_WALL_A,                          &
             ZABS_SW_WALL_B, ZABS_LW_WALL_B,                          &
             ZLW_WA_TO_WB,                                            &
             ZLW_WA_TO_R, ZLW_WB_TO_R,                                &
             ZLW_WA_TO_NR, ZLW_WB_TO_NR,                              &
             ZLW_R_TO_WA, ZLW_R_TO_WB,                                &
             ZLW_G_TO_WA, ZLW_G_TO_WB,                                &
             ZLW_S_TO_WA, ZLW_S_TO_WB, ZLW_S_TO_R,                    &
             ZLW_S_TO_NR, ZLW_NR_TO_WA, ZLW_NR_TO_WB,                 &
             ZLW_NR_TO_WIN, ZLW_WA_TO_WIN, ZLW_WB_TO_WIN,             &
             ZLW_G_TO_WIN,                                            &
             ZLW_R_TO_WIN, ZLW_S_TO_WIN, ZLW_WIN_TO_WA, ZLW_WIN_TO_WB,&
             ZLW_WIN_TO_R, ZLW_WIN_TO_NR,                             &
             YNATVENT,                                                &
             YCOOL_COIL, ZF_WATER_COND, YHEAT_COIL, LAUTOSIZE,        &
             IDAY, ZAUX_MAX, ZT_FLOOR, ZT_MASS, ZH_BLD_COOL,          &
             ZT_BLD_COOL, ZH_BLD_HEAT, ZLE_BLD_COOL, ZLE_BLD_HEAT,    &
             ZH_WASTE, ZLE_WASTE, XF_WASTE_CAN, ZHVAC_COOL, ZHVAC_HEAT,&
             XQIN, XQIN_FRAD, XQIN_FLAT, XGR, XEFF_HEAT,              &
             XINF, XTCOOL_TARGET, XTHEAT_TARGET, XHR_TARGET, ZT_WIN2, &
             ZQI_BLD, XV_VENT, XCAP_SYS_HEAT, XCAP_SYS_RAT, XT_ADP,   &
             XM_SYS_RAT, XCOP_RAT, ZCAP_SYS, ZM_SYS, ZCOP, ZQ_SYS,    &
             ZT_SYS, ZTR_SW_WIN, ZFAN_POWER, XHC_FLOOR, XTC_FLOOR,    &
             XD_FLOOR, ZT_WIN1, ZABS_SW_WIN, ZABS_LW_WIN,             &
             XUGG_WIN, ZEMIT_LW_FAC,                                  &
             ZEMIT_LW_ROAD, ZT_RAD_IND, ZHU_BLD, ZTIME, ZE_SHADING,    &
             GNATVENT_NIGHT(:), XN_FLOOR, XWALL_O_BLD, XGLAZ_O_BLD,    &
             XMASS_O_BLD, XFLOOR_HW_RATIO, XF_FLOOR_MASS, XF_FLOOR_WALL, &
             XF_FLOOR_WIN, XF_FLOOR_ROOF, XF_WALL_FLOOR, XF_WALL_MASS, &
             XF_WALL_WIN, XF_WIN_FLOOR, XF_WIN_MASS, XF_WIN_WALL,      &
             XF_MASS_FLOOR, XF_MASS_WALL, XF_MASS_WIN, GCANOPY, YCH_BEM, &
             XROUGH_ROOF, XROUGH_WALL, XF_WIN_WIN)
!! GREGOIRE 15/03 : commente appel à TEB
! 
!
!   Time update
    ZTIME = ZTIME + PPTSTEP
    IF (ZTIME >= 86400) THEN
      ZTIME = 0.0
      IDAY = IDAY + 1
    END IF
!
ENDDO

!
!
!
! -----------------------------------------------------------
! Print autosize results
! -----------------------------------------------------------
!
WRITE(KLUOUT,*) ' '
WRITE(KLUOUT,*) '    --------------------------------'
WRITE(KLUOUT,*) '      HVAC AUTOSIZE CALCULATIONS '
WRITE(KLUOUT,*) ' '
WRITE(KLUOUT,*) '    Rated mass flow rate:'
WRITE(KLUOUT,*) '    ',MAXVAL(XM_SYS_RAT), 'kg s-1 m-2(bld)'
WRITE(KLUOUT,*) '    ',MINVAL(XM_SYS_RAT), 'kg s-1 m-2(bld)'
WRITE(KLUOUT,*) '    Rated cooling system capacity:'
WRITE(KLUOUT,*) '    ',MAXVAL(XCAP_SYS_RAT), 'W m-2(bld)'
WRITE(KLUOUT,*) '    ',MINVAL(XCAP_SYS_RAT), 'W m-2(bld)'
WRITE(KLUOUT,*) '    Rated heating sysem capacity:'
WRITE(KLUOUT,*) '    ',MAXVAL(XCAP_SYS_HEAT), 'W m-2(bld)'
WRITE(KLUOUT,*) '    ',MINVAL(XCAP_SYS_HEAT), 'W m-2(bld)'
WRITE(KLUOUT,*) '    --------------------------------'
WRITE(KLUOUT,*) ' '
IF (LHOOK) CALL DR_HOOK('HVAC_AUTOSIZE',1,ZHOOK_HANDLE)
!
CONTAINS 
!
SUBROUTINE INTERP_PROFTWALL(PT1, PTN, PD, PT)
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
END SUBROUTINE INTERP_PROFTWALL

END SUBROUTINE HVAC_AUTOSIZE
