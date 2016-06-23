!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################################################
SUBROUTINE HVAC_AUTOSIZE (BDD, DTCO, DGU, B, BOP, UG, U, TG, T, TOP, &
                          KI,KLUOUT)
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
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
!
USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_TEB_GRID_n, ONLY : TEB_GRID_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
!
USE MODD_CSTS,            ONLY : XCPD, XPI, XP00, XRD, XSTEFAN
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
#ifdef AIX64 
USE OMP_LIB
#endif
!
IMPLICIT NONE
!
#ifndef AIX64
  INCLUDE 'omp_lib.h'
#endif
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE(BLD_DESC_t), INTENT(INOUT) :: BDD
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(TEB_GRID_t), INTENT(INOUT) :: TG
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
!
INTEGER,       INTENT(IN)    :: KI     ! number of points
INTEGER,       INTENT(IN)    :: KLUOUT ! output listing logical unit
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
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
REAL, DIMENSION(KI) :: ZFRAC_PANEL
REAL, DIMENSION(KI) :: ZALB_PANEL
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
REAL, DIMENSION(KI) :: ZABS_SW_PANEL
REAL, DIMENSION(KI) :: ZABS_SW_SNOW_ROOF
REAL, DIMENSION(KI) :: ZABS_SW_SNOW_ROAD
REAL, DIMENSION(KI) :: ZREC_SW_ROAD
REAL, DIMENSION(KI) :: ZREC_SW_SNOW_ROAD
REAL, DIMENSION(KI) :: ZREC_SW_WALL_A
REAL, DIMENSION(KI) :: ZREC_SW_WALL_B
REAL, DIMENSION(KI) :: ZREC_SW_GARDEN
REAL, DIMENSION(KI) :: ZREC_SW_ROOF
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
REAL,DIMENSION(KI,TOP%NROAD_LAYER ) :: ZT_ROAD    ! road layers temperatures
REAL,DIMENSION(KI,TOP%NROOF_LAYER) :: ZT_ROOF    ! roof layers temperatures
REAL,DIMENSION(KI,TOP%NWALL_LAYER) :: ZT_WALL_A  ! wall layers temperatures
REAL,DIMENSION(KI,TOP%NWALL_LAYER) :: ZT_WALL_B  ! wall layers temperatures
REAL,DIMENSION(KI,BOP%NFLOOR_LAYER ) :: ZT_FLOOR   ! building floor temperature
REAL,DIMENSION(KI,BOP%NFLOOR_LAYER ) :: ZT_MASS    ! building mass temperature
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
REAL, DIMENSION(KI) :: ZRUNOFF_GREENROOF 
REAL, DIMENSION(KI) :: ZDRAIN_GREENROOF 
REAL, DIMENSION(KI) :: ZUW_GREENROOF
REAL, DIMENSION(KI) :: ZG_GREENROOF_ROOF
REAL, DIMENSION(KI) :: ZRN_STRLROOF
REAL, DIMENSION(KI) :: ZH_STRLROOF
REAL, DIMENSION(KI) :: ZLE_STRLROOF
REAL, DIMENSION(KI) :: ZGFLUX_STRLROOF
REAL, DIMENSION(KI) :: ZRUNOFF_STRLROOF 
!
! Road irrigation (not used here)
LOGICAL             :: GPAR_RD_IRRIG = .FALSE.
REAL, DIMENSION(KI) :: ZRD_START_MONTH
REAL, DIMENSION(KI) :: ZRD_END_MONTH
REAL, DIMENSION(KI) :: ZRD_START_HOUR
REAL, DIMENSION(KI) :: ZRD_END_HOUR
REAL, DIMENSION(KI) :: ZRD_24H_IRRIG
REAL, DIMENSION(KI) :: ZIRRIG_ROAD
!
INTEGER, DIMENSION(:), ALLOCATABLE :: ISIZE_OMP
!
!RJ: temp variable for OMP region handling
#ifdef RJ_OFIX
INTEGER :: INBLOCKTOT
#endif
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
B%CUR%XM_SYS_RAT    = 0.
B%CUR%XCAP_SYS_RAT  = 0.
B%CUR%XCAP_SYS_HEAT = 0.
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
! solar panels are not taken into account in the building's HVAC equipment sizing process
ZFRAC_PANEL  = 0.
ZALB_PANEL   = 0.1
!
! greenroofs   are not taken into account in the building's HVAC equipment sizing process
ZRN_GREENROOF    (:) = 0.
ZH_GREENROOF     (:) = 0.
ZLE_GREENROOF    (:) = 0.
ZGFLUX_GREENROOF (:) = 0.
ZUW_GREENROOF    (:) = 0.
ZRUNOFF_GREENROOF (:) = 0.
ZDRAIN_GREENROOF (:) = 0.
!* one supposes zero conduction heat flux between the greenroof and the roof.
ZG_GREENROOF_ROOF(:) = 0.
!
!* road watering (not used)
ZRD_START_MONTH= 1.
ZRD_END_MONTH  = 1.
ZRD_START_HOUR = 0.
ZRD_END_HOUR   = 24.
ZRD_24H_IRRIG  = 0.
!
!*      A.     Autosize of the heating system
!              ---------------------------------
!
YCOOL_COIL = 'IDEAL '
YHEAT_COIL = 'IDEAL '
YNATVENT(:) = 'NONE'
ZF_WATER_COND(:) = 0.
ZRHOA = 1.30
ZTOUT_EQ(:) = (B%CUR%XT_SIZE_MIN(:) + ZT_SKY(:))/2.
!
ZU_ROOF(:) = 0.0
DO JJ=1,TOP%NROOF_LAYER
  ZU_ROOF(:) = ZU_ROOF(:) + T%CUR%XD_ROOF(:,JJ)/T%CUR%XTC_ROOF(:,JJ)
END DO
ZU_ROOF(:) = ZU_ROOF(:) + 1./10. + 1./25.         
ZU_ROOF(:) = 1. / ZU_ROOF(:)
!
ZU_WALL(:) = 0.0
DO JJ=1,TOP%NWALL_LAYER
  ZU_WALL(:) = ZU_WALL(:) + T%CUR%XD_WALL(:,JJ)/T%CUR%XTC_WALL(:,JJ)
END DO
ZU_WALL(:) = ZU_WALL(:) + 1./10. + 1./25.         
ZU_WALL(:) = 1. / ZU_WALL(:)
!
!   Heating Coil Capacity [W m-2(bld)]
B%CUR%XCAP_SYS_HEAT(:) = ZU_WALL(:) * T%CUR%XWALL_O_BLD(:) * (B%CUR%XTHEAT_TARGET(:) - ZTOUT_EQ(:)) &
                 + B%CUR%XU_WIN(:)  * B%CUR%XGLAZ_O_BLD(:)  * (B%CUR%XTHEAT_TARGET(:) - ZTOUT_EQ(:)) &
                 + ZU_ROOF(:)              * (B%CUR%XTHEAT_TARGET(:) - ZTOUT_EQ(:)) &
                 - B%CUR%XQIN(:) * T%CUR%XBLD_HEIGHT(:) / B%CUR%XFLOOR_HEIGHT(:)*             &
                   (1 - B%CUR%XQIN_FLAT(:))                                        &
                 + B%CUR%XINF(:) * T%CUR%XBLD_HEIGHT(:) / 3600* ZRHOA(:) * XCPD *       &
                   (B%CUR%XTHEAT_TARGET(:) - B%CUR%XT_SIZE_MIN(:)) &
                 + B%CUR%XV_VENT(:) * T%CUR%XBLD_HEIGHT(:) / 3600* ZRHOA(:) * XCPD *    &
                   (B%CUR%XTHEAT_TARGET(:) - B%CUR%XT_SIZE_MIN(:))
!
!   Rated air flow rate [kg s-1 m-2(bld)]
B%CUR%XM_SYS_RAT(:)   = B%CUR%XCAP_SYS_HEAT(:)/XCPD/(323.15 - B%CUR%XTHEAT_TARGET(:))
!
!
!*      B.     Autosize of the cooling system
!              -----------------------------------
!
ZRHOA = 1.15
!    Initial values
! initial value for air temperature and outdoor wall/roof/window/road temperature
ZT_CANYON(:) = 10.7/2 * SIN(2*XPI/(24*3600) * (ZTIME+16*3600)) + (B%CUR%XT_SIZE_MAX(:)-10.7/2)
!! !ZTI_BLD   = 297.16 ! indoor air temperature
DO JJ=1,KI
    ZTI_BLD(JJ) = MAX(B%CUR%XTHEAT_TARGET(JJ),ZT_CANYON(JJ)) ! indoor air temperature
ENDDO
ZT_ROOF  (:,TOP%NROOF_LAYER)   = ZTI_BLD(:)   ! roof layers temperatures
ZT_WALL_A(:,TOP%NWALL_LAYER)   = ZTI_BLD(:)   ! wall layers temperatures
DO JJ=1,BOP%NFLOOR_LAYER
   ZT_FLOOR(:,JJ)  = ZTI_BLD(:) ! building floor temperature
   ZT_MASS(:,JJ)   = ZTI_BLD(:) ! building mass temperature
ENDDO

!ROAD
DO JJ=1,TOP%NROAD_LAYER
   ZT_ROAD(:,JJ) = ZT_CANYON(:)
ENDDO
!ROOF
ZT_ROOF(:,1) = ZT_CANYON(:)
ZT1(:)=ZT_ROOF(:,1)
ZTN(:)=ZT_ROOF(:,TOP%NROOF_LAYER)
IF (TOP%NROOF_LAYER .GT. 2) CALL INTERP_PROFTWALL(ZT1, ZTN, T%CUR%XD_ROOF, ZT_ROOF)
!WALL
ZT_WALL_A(:,1) = ZT_CANYON(:)
ZT1(:)=ZT_WALL_A(:,1)
ZTN(:)=ZT_WALL_A(:,TOP%NWALL_LAYER)
IF (TOP%NWALL_LAYER .GT. 2) CALL INTERP_PROFTWALL(ZT1, ZTN, T%CUR%XD_WALL, ZT_WALL_A)
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
ZROAD         (:) = T%CUR%XROAD(:)+T%CUR%XGARDEN(:)
ZGARDEN       (:) = 0.
ZALB_GARDEN   (:) = 0.
ZALB_GREENROOF(:) = 0.
ZAC_GARDEN    (:) = 0.
ZSVF_GARDEN   (:) = 0.
!
!
!RJ: fix for OMP races
!RJ: run parallely for prep, but pseudo serially for offline (already in OMP here)
#ifdef RJ_OFIX
INBLOCKTOT=NBLOCKTOT
!RJ: next one prevents double split in OMP parallel region
!$ IF(OMP_IN_PARALLEL()) INBLOCKTOT=1
ALLOCATE(ISIZE_OMP(0:INBLOCKTOT-1))
 CALL GET_SIZES_PARALLEL(DTCO, DGU, UG, U, &
                         INBLOCKTOT,KI,0,ISIZE_OMP)
#else
ALLOCATE(ISIZE_OMP(0:0))
 CALL GET_SIZES_PARALLEL(DTCO, DGU, UG, U, &
                         1,KI,0,ISIZE_OMP)
#endif

DO JFORC_STEP= 1,INB_STEP_ATM
!
!   Daily outdoor air temperature cycle
    ZT_CANYON(:) = 10.7/2 * SIN(2*XPI/(24*3600) * (ZTIME+16*3600))  &
              + (B%CUR%XT_SIZE_MAX(:)-10.7/2)
    ZTA(:) = ZT_CANYON(:)
!
!
!*      B.1     Solar radiation
!               ---------------
!
    CALL SUNPOS(ISIZE_OMP, JPYEAR, IMONTH, IDAY, ZTIME, TG%XLON, TG%XLAT, ZTSUN, ZZENITH, ZAZIM)
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
    CALL URBAN_SOLAR_ABS(YBEM, TOP%CROAD_DIR, TOP%CWALL_OPT,               &
                     ZDIR_SW, ZSCA_SW, ZZENITH, ZAZIM,             &
                     T%CUR%XBLD, ZGARDEN, T%CUR%XROAD_DIR, T%CUR%XROAD, T%CUR%XGREENROOF,  &
                     T%CUR%XWALL_O_HOR, T%CUR%XCAN_HW_RATIO,                   &
                     T%CUR%XALB_ROOF,                                    &
                     T%CUR%XALB_ROAD, T%CUR%XSVF_ROAD, T%CUR%XALB_WALL, T%CUR%XSVF_WALL,   &
                     ZFRAC_PANEL, ZALB_PANEL,                      &
                     ZALB_GARDEN, ZSVF_GARDEN,                     &
                     ZALB_GREENROOF,                               &
                     ZASNOW_ROOF, ZASNOW_ROAD,                     &
                     ZDN_ROOF, ZDF_ROOF, ZDN_ROAD, ZDF_ROAD,       &
                     B%CUR%XGR, B%CUR%XABS_WIN, B%CUR%XSHGC, B%CUR%XSHGC_SH, B%CUR%XALB_WIN,     &                     
                     ZABS_SW_ROOF, ZABS_SW_ROAD,                   &
                     ZABS_SW_WALL_A, ZABS_SW_WALL_B,               &
                     ZABS_SW_GARDEN, ZABS_SW_GREENROOF,            &
                     ZABS_SW_SNOW_ROOF, ZABS_SW_SNOW_ROAD,         &
                     ZABS_SW_PANEL,                                &
                     ZREC_SW_ROAD,  ZREC_SW_SNOW_ROAD,             &
                     ZREC_SW_WALL_A, ZREC_SW_WALL_B,               &
                     ZREC_SW_GARDEN, ZREC_SW_ROOF,                 &
                     ZDIR_ALB_TOWN,ZSCA_ALB_TOWN,                  &
                     ZSW_RAD_GARDEN, ZABS_SW_WIN, ZREC_SW_WIN,     &
                     B%CUR%XTRAN_WIN,                                    &
                     ZREF_SW_GRND, ZREF_SW_FAC, ZTR_SW_WIN,        &
                     ZE_SHADING, GSHAD_DAY, GSHADE                 )
!
!
!*      B.2     LW properties
!               -------------
!
   CALL URBAN_LW_COEF(B%CUR%XGR, T%CUR%XBLD, ZLW_RAD,                                &
                      T%CUR%XEMIS_ROAD, T%CUR%XSVF_ROAD, T%CUR%XEMIS_WALL, T%CUR%XSVF_WALL,      &
                      ZEMIS_GARDEN, T%CUR%XROAD, ZGARDEN,                      &
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
    CALL TEB  (HZ0H, HIMPLICIT_WIND, TOP%CWALL_OPT, YBEM,                 &
             TOP%TTIME, ZTSUN,                                            &
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
             T%CUR%XH_TRAFFIC, T%CUR%XLE_TRAFFIC, T%CUR%XH_INDUSTRY, T%CUR%XLE_INDUSTRY,      &
             PPTSTEP, T%CUR%XZ0_TOWN, T%CUR%XBLD, ZGARDEN, T%CUR%XROAD, T%CUR%XGREENROOF,     &
             T%CUR%XBLD_HEIGHT, T%CUR%XWALL_O_HOR, T%CUR%XCAN_HW_RATIO, T%CUR%XWALL_O_GRND,   &
             ZDF_ROOF, ZDN_ROOF, ZDF_ROAD,                            &
             ZDN_ROAD, ZQSAT_ROOF, ZQSAT_ROAD, ZDELT_ROOF, ZDELT_ROAD,&
             T%CUR%XEMIS_ROOF, T%CUR%XHC_ROOF, T%CUR%XTC_ROOF, T%CUR%XD_ROOF,                 &
             T%CUR%XEMIS_ROAD, T%CUR%XHC_ROAD, T%CUR%XTC_ROAD,                          &
             T%CUR%XD_ROAD, T%CUR%XEMIS_WALL, ZTS_GARDEN,                         &
             T%CUR%XHC_WALL, T%CUR%XTC_WALL, T%CUR%XD_WALL, ZRN_ROOF, ZH_ROOF, ZLE_ROOF,&
             ZLEW_ROOF, ZGFLUX_ROOF, ZRUNOFF_ROOF,                    &
             ZRN_GREENROOF, ZH_GREENROOF, ZLE_GREENROOF,              &
             ZGFLUX_GREENROOF, ZUW_GREENROOF,                         &
             ZRUNOFF_GREENROOF,ZDRAIN_GREENROOF,                      &
             ZRN_STRLROOF, ZH_STRLROOF, ZLE_STRLROOF, ZGFLUX_STRLROOF,&
             ZRUNOFF_STRLROOF,                                        &
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
             YCOOL_COIL, ZF_WATER_COND, YHEAT_COIL, BOP%LAUTOSIZE,        &
             IDAY, ZAUX_MAX, ZT_FLOOR, ZT_MASS, ZH_BLD_COOL,          &
             ZT_BLD_COOL, ZH_BLD_HEAT, ZLE_BLD_COOL, ZLE_BLD_HEAT,    &
             ZH_WASTE, ZLE_WASTE, B%CUR%XF_WASTE_CAN, ZHVAC_COOL, ZHVAC_HEAT,&
             B%CUR%XQIN, B%CUR%XQIN_FRAD, B%CUR%XQIN_FLAT, B%CUR%XGR, B%CUR%XEFF_HEAT,              &
             B%CUR%XINF, B%CUR%XTCOOL_TARGET, B%CUR%XTHEAT_TARGET, B%CUR%XHR_TARGET, ZT_WIN2, &
             ZQI_BLD, B%CUR%XV_VENT, B%CUR%XCAP_SYS_HEAT, B%CUR%XCAP_SYS_RAT, B%CUR%XT_ADP,   &
             B%CUR%XM_SYS_RAT, B%CUR%XCOP_RAT, ZCAP_SYS, ZM_SYS, ZCOP, ZQ_SYS,    &
             ZT_SYS, ZTR_SW_WIN, ZFAN_POWER, B%CUR%XHC_FLOOR, B%CUR%XTC_FLOOR,    &
             B%CUR%XD_FLOOR, ZT_WIN1, ZABS_SW_WIN, ZABS_LW_WIN,             &
             B%CUR%XUGG_WIN, ZEMIT_LW_FAC,                                  &
             ZEMIT_LW_ROAD, ZT_RAD_IND, ZHU_BLD, ZTIME, ZE_SHADING,    &
             GNATVENT_NIGHT(:), B%CUR%XN_FLOOR, T%CUR%XWALL_O_BLD, B%CUR%XGLAZ_O_BLD,    &
             B%CUR%XMASS_O_BLD, B%CUR%XFLOOR_HW_RATIO, B%CUR%XF_FLOOR_MASS, B%CUR%XF_FLOOR_WALL, &
             B%CUR%XF_FLOOR_WIN, B%CUR%XF_FLOOR_ROOF, B%CUR%XF_WALL_FLOOR, B%CUR%XF_WALL_MASS, &
             B%CUR%XF_WALL_WIN, B%CUR%XF_WIN_FLOOR, B%CUR%XF_WIN_MASS, B%CUR%XF_WIN_WALL,      &
             B%CUR%XF_MASS_FLOOR, B%CUR%XF_MASS_WALL, B%CUR%XF_MASS_WIN, GCANOPY, YCH_BEM, &
             T%CUR%XROUGH_ROOF, T%CUR%XROUGH_WALL, B%CUR%XF_WIN_WIN,                     &
             GPAR_RD_IRRIG, ZRD_START_MONTH, ZRD_END_MONTH,            &
             ZRD_START_HOUR, ZRD_END_HOUR, ZRD_24H_IRRIG, ZIRRIG_ROAD  )
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
WRITE(KLUOUT,*) '    ',MAXVAL(B%CUR%XM_SYS_RAT), 'kg s-1 m-2(bld)'
WRITE(KLUOUT,*) '    ',MINVAL(B%CUR%XM_SYS_RAT), 'kg s-1 m-2(bld)'
WRITE(KLUOUT,*) '    Rated cooling system capacity:'
WRITE(KLUOUT,*) '    ',MAXVAL(B%CUR%XCAP_SYS_RAT), 'W m-2(bld)'
WRITE(KLUOUT,*) '    ',MINVAL(B%CUR%XCAP_SYS_RAT), 'W m-2(bld)'
WRITE(KLUOUT,*) '    Rated heating sysem capacity:'
WRITE(KLUOUT,*) '    ',MAXVAL(B%CUR%XCAP_SYS_HEAT), 'W m-2(bld)'
WRITE(KLUOUT,*) '    ',MINVAL(B%CUR%XCAP_SYS_HEAT), 'W m-2(bld)'
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
