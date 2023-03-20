!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE TOWN_ENERGY_BALANCE (DTCO, G, TOP, SPAOP, T, BOP, B, TPN, TIR, DMT, GDM, GRM, HM,     &
         SB, CT, KTEB_P, HPROGRAM, HIMPLICIT_WIND, PTSUN, PT_CAN, PQ_CAN, PU_CAN, PT_LOWCAN,     &
         PQ_LOWCAN, PU_LOWCAN, PZ_LOWCAN, PTA_HVEG, PQA_HVEG,                                    &
         PPEW_A_COEF, PPEW_B_COEF, PPEW_A_COEF_LOWCAN, PPEW_B_COEF_LOWCAN, AT,                   &
         PPS, PPSOLD, PPA, PEXNS, PEXNA, PTA, PQA, PRHOA, PCO2, PLW_RAD, PDIR_SW, PSCA_SW,       &
         PSW_BANDS, KSW, PZENITH, PAZIM, PRR, PSR, PZREF, PUREF, PVMOD, PH_TRAFFIC, PLE_TRAFFIC, &
         PTSTEP, PLEW_RF, PLEW_RD, PRNSN_RF, PHSN_RF, PLESN_RF, PGSN_RF, PMELT_RF, PRNSN_RD,     &
         PHSN_RD, PLESN_RD, PGSN_RD, PMELT_RD, PRN_GRND, PH_GRND, PLE_GRND, PGFLX_GRND, PRN_TWN, &
         PH_TWN, PH_TWN_SURF, PH_TWN_WALL, PH_TWN_ROOF, PLE_TWN, PGFLX_TWN, PQF_TWN, PEVAP_TWN,  &
         PEVAP_TWN_SURF, PEVAP_TWN_WALL, PEVAP_TWN_ROOF, PUW_GRND, PUW_RF, PDUWDU_GRND,          &
         PDUWDU_RF, PUSTAR_TWN, PCD, PCDN, PCH_TWN, PRI_TWN, PTS_TWN, PEMIS_TWN, PDIR_ALB_TWN,   &
         PSCA_ALB_TWN, PRESA_TWN, PAC_RD, PAC_GD, PAC_GR, PAC_RD_WAT, PAC_GD_WAT, PAC_GR_WAT,    &
         KDAY, PEMIT_LW_HVEG, PREF_SW_GRND, PREF_SW_FAC, PREF_SW_HVEG, PTIME, PDN_RF,PDN_RD,     &
         PTS_HVEG, PTSRAD_GD, PTSRAD_GR, PLAD_CAN, PTRAF_MODULATION,                             &
         PPOP_MODULATION, PDH_HVEG, PDLE_HVEG, PSCA_SW_SKY, PLW_RAD_SKY, PSCA_SW_GROUND_DOWN,    &
         PSCA_SW_GROUND_UP, PSCA_SW_GROUND_HOR, PLW_GROUND_DOWN, PLW_GROUND_HOR, HTEST           )
!
!   ##########################################################################
!
!!****  *TOWN_ENERGY_BALANCE*  
!!
!!    PURPOSE
!!    -------
!
!     
!!**  METHOD
!     ------
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      A. Lemonsu          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!    Original    05/2009
!!                04/2012 add B%XTRAN_WIN
!!    modified    08/2012 TOP%CCH_BEM/ ROUGH_WALL and ROUGH_ROOF for buildind conv coef.
!!    modified    10/2012 add B%XF_WIN_WIN as arg
!!    modified    03/2014 add TOP%CURBTREE (key for urban trees and green walls)
!!  			      SHAPE_HVEG, HTRUNK_HVEG, WCROWN_HVEG
!!    modified    01/2016 add arguments for urban hydrology (K.Chancibault/A.Lemonsu)
!!    modified    03/2017 (M. Goret) bug fix in ZQA calculus according de E. Redon
!!    modified    04/2017 (M. Goret) suppress PEFF_HEAT as a dummy argument 
!!    modified    07/2017 (M. Goret) add HPROGRAM as GARDEN argument
!!    modified    07/2017 (M. Goret) add anthropogenic flux diagnostics
!!    modified    09/2017 (M. Goret) add diagnostic of heat storage link to snow
!!    modified    10/2017 (M. Goret) add hot water 
!!      V. Masson   04.2020 completes energy check for high vegetation IR exchanges
!----------------------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_SPARTACUS_OPTION_n, ONLY : SPARTACUS_OPTIONS_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_TEB_PANEL_n, ONLY : TEB_PANEL_t
USE MODD_TEB_IRRIG_n, ONLY : TEB_IRRIG_t
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
USE MODD_CANOPY_n, ONLY : CANOPY_t
!
USE MODD_DIAG_n, ONLY : DIAG_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_HYDRO_MODEL_t
!
USE MODD_CHECK_TEB, ONLY : CHECK_TEB_t
USE MODD_LW_COEF, ONLY : LW_COEF_t
!
USE MODD_TYPE_DATE_SURF,    ONLY: DATE_TIME
USE MODD_CSTS,              ONLY: XTT, XSTEFAN, XLVTT, XLSTT, XSURF_EPSILON
USE MODD_TEB_PAR,           ONLY: XHUM_CO2
USE MODD_SURF_PAR,          ONLY: XUNDEF
USE MODD_SNOW_PAR,          ONLY: XEMISSN, XANSMAX
USE MODD_ISBA_PAR,          ONLY: XEMISVEG,XWGMIN
USE MODD_SURF_ATM_TURB_n,   ONLY : SURF_ATM_TURB_t
!
USE MODE_THERMOS
USE MODE_SURF_SNOW_FRAC
!
USE MODI_SOLAR_PANEL
USE MODI_TEB_VEG_PROPERTIES
USE MODI_URBTREE_PROPERTIES
USE MODI_WINDOW_SHADING_AVAILABILITY
USE MODI_URBAN_SOLAR_ABS
USE MODI_URBAN_LW_COEF
USE MODI_TEB_IRRIG
USE MODI_URBAN_RUNOFF
USE MODI_GARDEN
USE MODI_GARDEN_HVEG
USE MODI_GREENROOF
USE MODI_TEB_HYDRO
USE MODI_URBAN_HYDRO_COND
USE MODI_URBAN_HYDRO_HTRANSFERT
USE MODI_UPDATE_THERMALPROP
USE MODI_TEB_BLD_ROAD
USE MODI_AVG_URBAN_FLUXES
USE MODI_BLD_OCC_CALENDAR
USE MODI_ALLOC_LW_COEF
USE MODI_DEALLOC_LW_COEF
USE MODI_TEB_SPARTACUS
USE MODI_EXPLICIT_LONGWAVE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    Declarations of arguments
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
TYPE(GRID_t), INTENT(INOUT) :: G
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(SPARTACUS_OPTIONS_t), INTENT(INOUT) :: SPAOP
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(TEB_PANEL_t), INTENT(INOUT) :: TPN
TYPE(TEB_IRRIG_t), INTENT(INOUT) :: TIR
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DMT
TYPE(SURF_ATM_TURB_t), INTENT(IN) :: AT            ! atmospheric turbulence parameters
!
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
TYPE(TEB_HYDRO_MODEL_t),        INTENT(INOUT) :: HM
TYPE(CANOPY_t),           INTENT(INOUT) :: SB
TYPE(CHECK_TEB_t),        INTENT(INOUT) :: CT
!
INTEGER, INTENT(IN) :: KTEB_P                             ! TEB current patch number 
 CHARACTER(LEN=2),     INTENT(IN)    :: HTEST             ! must be equal to 'OK'  
 CHARACTER(LEN=6),     INTENT(IN)    :: HPROGRAM          ! program calling surf. schemes
 CHARACTER(LEN=*),     INTENT(IN)  :: HIMPLICIT_WIND      ! wind implicitation option
!                                                         ! 'OLD' = direct
!                                                         ! 'NEW' = Taylor serie, order 1
REAL, DIMENSION(:),   INTENT(IN)    :: PTSUN              ! solar time   (s from midnight)
!                                                         
REAL, DIMENSION(:)  , INTENT(INOUT) :: PT_CAN             ! canyon air temperature
REAL, DIMENSION(:)  , INTENT(INOUT) :: PQ_CAN             ! canyon air specific humidity
REAL, DIMENSION(:)  , INTENT(IN)    :: PU_CAN             ! canyon hor. wind
REAL, DIMENSION(:)  , INTENT(IN)    :: PU_LOWCAN          ! wind near the road
REAL, DIMENSION(:)  , INTENT(IN)    :: PT_LOWCAN          ! temp. near the road
REAL, DIMENSION(:)  , INTENT(IN)    :: PQ_LOWCAN          ! hum. near the road
REAL, DIMENSION(:)  , INTENT(IN)    :: PZ_LOWCAN          ! height of atm. var. near the road
REAL, DIMENSION(:)  , INTENT(IN)    :: PTA_HVEG           ! canyon air temperature       at tree level
REAL, DIMENSION(:)  , INTENT(IN)    :: PQA_HVEG           ! canyon air specific humidity at tree level
REAL, DIMENSION(:)  , INTENT(IN)    :: PPEW_A_COEF        ! implicit coefficients
REAL, DIMENSION(:)  , INTENT(IN)    :: PPEW_B_COEF        ! for wind coupling
REAL, DIMENSION(:)  , INTENT(IN)    :: PPEW_A_COEF_LOWCAN ! implicit coefficients for wind coupling
REAL, DIMENSION(:)  , INTENT(IN)    :: PPEW_B_COEF_LOWCAN ! between low canyon wind and road
REAL, DIMENSION(:)  , INTENT(IN)    :: PPS                ! pressure at the surface
REAL, DIMENSION(:)  , INTENT(IN)    :: PPSOLD             ! pressure at the surface at previous time step
REAL, DIMENSION(:)  , INTENT(IN)    :: PPA                ! pressure at the first atmospheric level
REAL, DIMENSION(:)  , INTENT(IN)    :: PEXNS              ! surface exner function
REAL, DIMENSION(:)  , INTENT(IN)    :: PEXNA              ! exner function at the lowest level
REAL, DIMENSION(:)  , INTENT(IN)    :: PTA                ! temperature at the lowest level
REAL, DIMENSION(:)  , INTENT(IN)    :: PQA                ! specific humidity at the lowest level
REAL, DIMENSION(:)  , INTENT(IN)    :: PRHOA              ! air density at the lowest level
REAL, DIMENSION(:)  , INTENT(IN)    :: PCO2               ! CO2 concentration in the air    (kg/m3)
REAL, DIMENSION(:)  , INTENT(IN)    :: PLW_RAD            ! atmospheric infrared radiation
REAL, DIMENSION(:,:), INTENT(IN)    :: PDIR_SW            ! incoming direct solar rad on an horizontal surface
REAL, DIMENSION(:,:), INTENT(IN)    :: PSCA_SW            ! scattered incoming solar rad.
REAL, DIMENSION(:)  , INTENT(IN)    :: PSW_BANDS          ! mean wavelength of each shortwave band (m)
INTEGER,              INTENT(IN)    :: KSW                ! number of short-wave spectral bands
REAL, DIMENSION(:)  , INTENT(IN)    :: PZENITH            ! solar zenithal angle
REAL, DIMENSION(:)  , INTENT(IN)    :: PAZIM              ! solar azimuthal angle
                                                          ! (radian form N, clockwise)
REAL, DIMENSION(:)  , INTENT(IN)    :: PRR                ! rain rate
REAL, DIMENSION(:)  , INTENT(IN)    :: PSR                ! snow rate
REAL, DIMENSION(:)  , INTENT(IN)    :: PH_TRAFFIC         ! anthropogenic sensible heat fluxes due to traffic
REAL, DIMENSION(:)  , INTENT(IN)    :: PLE_TRAFFIC        ! anthropogenic latent heat fluxes due to traffic
REAL, DIMENSION(:)  , INTENT(IN)    :: PZREF              ! reference height of the first atm level (temperature)
REAL, DIMENSION(:)  , INTENT(IN)    :: PUREF              ! reference height of the first atm level (wind)
REAL, DIMENSION(:)  , INTENT(IN)    :: PVMOD              ! module of the horizontal wind
REAL                , INTENT(IN)    :: PTSTEP             ! time step
!
REAL, DIMENSION(:)  , INTENT(OUT)   :: PLEW_RF          ! latent heat flux over roof (snow)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PLEW_RD          ! latent heat flux over road (snow)
!
REAL, DIMENSION(:)  , INTENT(OUT)   :: PRNSN_RF       ! net radiation over snow
REAL, DIMENSION(:)  , INTENT(OUT)   :: PHSN_RF        ! sensible heat flux over snow
REAL, DIMENSION(:)  , INTENT(OUT)   :: PLESN_RF       ! latent heat flux over snow
REAL, DIMENSION(:)  , INTENT(OUT)   :: PGSN_RF        ! flux under the snow
REAL, DIMENSION(:)  , INTENT(OUT)   :: PMELT_RF         ! snow melt
REAL, DIMENSION(:)  , INTENT(OUT)   :: PRNSN_RD       ! net radiation over snow
REAL, DIMENSION(:)  , INTENT(OUT)   :: PHSN_RD        ! sensible heat flux over snow
REAL, DIMENSION(:)  , INTENT(OUT)   :: PLESN_RD       ! latent heat flux over snow
REAL, DIMENSION(:)  , INTENT(OUT)   :: PGSN_RD        ! flux under the snow
REAL, DIMENSION(:)  , INTENT(OUT)   :: PMELT_RD       ! snow melt
!
REAL, DIMENSION(:)  , INTENT(OUT)   :: PRN_GRND          ! net radiation over ground
REAL, DIMENSION(:)  , INTENT(OUT)   :: PH_GRND           ! sensible heat flux over ground
REAL, DIMENSION(:)  , INTENT(OUT)   :: PLE_GRND          ! latent heat flux over ground
REAL, DIMENSION(:)  , INTENT(OUT)   :: PGFLX_GRND        ! flux through the ground
REAL, DIMENSION(:)  , INTENT(OUT)   :: PRN_TWN           ! net radiation over town
REAL, DIMENSION(:)  , INTENT(OUT)   :: PH_TWN            ! sensible heat flux over town
REAL, DIMENSION(:)  , INTENT(OUT)   :: PH_TWN_SURF       ! sensible heat flux over town, surface level
REAL, DIMENSION(:)  , INTENT(OUT)   :: PH_TWN_WALL       ! sensible heat flux over town, wall level
REAL, DIMENSION(:)  , INTENT(OUT)   :: PH_TWN_ROOF       ! sensible heat flux over town, roof level
REAL, DIMENSION(:)  , INTENT(OUT)   :: PLE_TWN           ! latent heat flux over town
REAL, DIMENSION(:)  , INTENT(OUT)   :: PGFLX_TWN         ! flux through the ground
REAL, DIMENSION(:)  , INTENT(OUT)   :: PQF_TWN           ! anthropogenic flux
REAL, DIMENSION(:)  , INTENT(OUT)   :: PEVAP_TWN         ! evaporation flux (kg/m2/s)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PEVAP_TWN_SURF    ! evaporation flux, surface level (kg/m2/s)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PEVAP_TWN_WALL    ! evaporation flux, wall level (kg/m2/s)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PEVAP_TWN_ROOF    ! evaporation flux, roof level (kg/m2/s)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PUW_GRND          ! momentum flux for ground built surf
REAL, DIMENSION(:)  , INTENT(OUT)   :: PUW_RF            ! momentum flux for roofs
REAL, DIMENSION(:)  , INTENT(OUT)   :: PDUWDU_GRND       !
REAL, DIMENSION(:)  , INTENT(OUT)   :: PDUWDU_RF         !
REAL, DIMENSION(:)  , INTENT(OUT)   :: PUSTAR_TWN        ! friciton velocity over town
REAL, DIMENSION(:)  , INTENT(OUT)   :: PCD               ! town averaged drag coefficient
REAL, DIMENSION(:)  , INTENT(OUT)   :: PCDN              ! town averaged neutral drag coefficient
REAL, DIMENSION(:)  , INTENT(OUT)   :: PCH_TWN           ! town averaged heat transfer coefficient
REAL, DIMENSION(:)  , INTENT(OUT)   :: PRI_TWN           ! town averaged Richardson number
REAL, DIMENSION(:)  , INTENT(OUT)   :: PTS_TWN           ! town surface temperature
REAL, DIMENSION(:)  , INTENT(OUT)   :: PEMIS_TWN         ! town equivalent emissivity
REAL, DIMENSION(:)  , INTENT(OUT)   :: PDIR_ALB_TWN      ! town equivalent direct albedo
REAL, DIMENSION(:)  , INTENT(OUT)   :: PSCA_ALB_TWN      ! town equivalent diffuse albedo
REAL, DIMENSION(:)  , INTENT(OUT)   :: PRESA_TWN         ! town aerodynamical resistance
REAL, DIMENSION(:)  , INTENT(OUT)   :: PAC_RD            ! road conductance
REAL, DIMENSION(:)  , INTENT(OUT)   :: PAC_GD            ! green area conductance
REAL, DIMENSION(:)  , INTENT(OUT)   :: PAC_GR            ! green roof conductance
REAL, DIMENSION(:)  , INTENT(OUT)   :: PAC_RD_WAT        ! road conductance for latent heat
REAL, DIMENSION(:)  , INTENT(OUT)   :: PAC_GD_WAT        ! green area conductance for latent heat
REAL, DIMENSION(:)  , INTENT(OUT)   :: PAC_GR_WAT        ! green roof conductance for latent heat
!
! new arguments created after BEM
!
INTEGER             , INTENT(IN)    :: KDAY               ! Simulation day
! new argument for the UTCI calculation
REAL, DIMENSION(:)  , INTENT(OUT)    :: PEMIT_LW_HVEG     ! LW flux emitted by high veg   (W/m2 ground)
REAL, DIMENSION(:)  , INTENT(OUT)    :: PREF_SW_GRND      ! total solar rad reflected from ground
REAL, DIMENSION(:)  , INTENT(OUT)    :: PREF_SW_FAC       ! total solar rad reflected from facade
REAL, DIMENSION(:)  , INTENT(OUT)    :: PREF_SW_HVEG      ! total solar rad reflected from high veg
!
! new arguments for shading, schedule or natural ventilation
REAL                , INTENT(IN)     :: PTIME             ! current time since midnight (UTC, s)
!
REAL, DIMENSION(:)  , INTENT(OUT)    :: PDN_RF          ! snow fraction on roofs
REAL, DIMENSION(:)  , INTENT(OUT)    :: PDN_RD          ! snow fraction on roads
!
! new arguments for high vegetation
!
REAL, DIMENSION(:)  , INTENT(IN)    :: PTS_HVEG           ! foliar surface temperature of urban high vegetation (K)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PTSRAD_GD          ! skin surface temperature of urban low vegetation (K)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PTSRAD_GR          ! skin surface temperature of urban green roofs (K)
REAL, DIMENSION(:,:), INTENT(OUT)   :: PLAD_CAN           ! vertical profile of Leaf Area Density on canopy grid
REAL, DIMENSION(:,:), INTENT(OUT)   :: PDH_HVEG           ! sensible heat flux from trees discretized on canopy grid
REAL, DIMENSION(:,:), INTENT(OUT)   :: PDLE_HVEG          ! sensible heat flux from trees discretized on canopy grid
REAL, DIMENSION(:)  , INTENT(OUT)   :: PSCA_SW_SKY        ! Diff sol rad from sky received by people (inc attenuation by trees)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PLW_RAD_SKY        ! IR rad from sky received by people (inc attenuation by trees)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PSCA_SW_GROUND_DOWN ! Diffusive downwelling solar radiation at ground level (W/m2)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PSCA_SW_GROUND_UP   ! Diffusive upwelling solar radiation at ground level (W/m2)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PSCA_SW_GROUND_HOR  ! Diffusive horizontal solar radiation at ground level (W/m2)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PLW_GROUND_DOWN     ! Downwelling longwave radiation at ground level (W/m2)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PLW_GROUND_HOR      ! Longwave radiation in horizontal direction at ground level (W/m2)
REAL, DIMENSION(:)  , INTENT(IN)    :: PTRAF_MODULATION    ! modulation of traffic CO2 flux as a function of month, day and hour
REAL, DIMENSION(:)  , INTENT(IN)    :: PPOP_MODULATION     ! modulation of CO2 flux due to metabolism as a function of month, day and hour
!
!*      0.2    Declarations of local variables
!
!
TYPE(DIAG_t), POINTER :: GDD
TYPE(DIAG_EVAP_ISBA_t), POINTER :: GDDE
TYPE(DIAG_MISC_ISBA_t), POINTER :: GDDM
!
TYPE(DIAG_t), POINTER :: GRD
TYPE(DIAG_EVAP_ISBA_t), POINTER :: GRDE
TYPE(DIAG_MISC_ISBA_t), POINTER :: GRDM
!
TYPE(LW_COEF_t) :: LW
!
REAL, DIMENSION(SIZE(PTA)) :: ZTA_RF     ! air temperature extrapolated at roof level
REAL, DIMENSION(SIZE(PTA)) :: ZQA_RF     ! air humidity extrapolated at roof level
!
REAL, DIMENSION(SIZE(PTA)) :: ZDF_RF       ! free-snow fraction on roofs
REAL, DIMENSION(SIZE(PTA)) :: ZDF_RD       ! free-snow fraction on roads
REAL, DIMENSION(SIZE(PTA)) :: ZAC_RF       ! roof conductance
REAL, DIMENSION(SIZE(PTA)) :: ZAC_RF_WAT   ! roof water conductance
REAL, DIMENSION(SIZE(PTA)) :: ZAC_WL       ! wall conductance
REAL, DIMENSION(SIZE(PTA)) :: ZAC_TOP      ! top conductance
REAL, DIMENSION(SIZE(PTA)) :: ZQSAT_RD     ! hum of saturation for roads
REAL, DIMENSION(SIZE(PTA)) :: ZQSAT_GD     ! hum of saturation for green areas
REAL, DIMENSION(SIZE(PTA)) :: ZQSAT_RF     ! hum of saturation for roofs
REAL, DIMENSION(SIZE(PTA)) :: ZQSAT_GR     ! hum of saturation for green roofs
REAL, DIMENSION(SIZE(PTA)) :: ZQSAT_HVEG   ! hum of saturation for trees
!
! coefficients for LW computations over snow (from previous time-step)
!
REAL, DIMENSION(SIZE(PTA)) :: ZTSSN_RF   ! roof snow temp at previous time-step
REAL, DIMENSION(SIZE(PTA)) :: ZTSSN_RD   ! road snow temp at previous time-step
REAL, DIMENSION(SIZE(PTA)) :: ZESN_RF    ! snow emissivity at previous time-step
REAL, DIMENSION(SIZE(PTA)) :: ZESN_RD    ! snow emissivity at previous time-step
!
! incoming shortwave radiation
!
REAL, DIMENSION(SIZE(PTA)) :: ZDIR_SW             ! direct  solar rad
REAL, DIMENSION(SIZE(PTA)) :: ZSCA_SW             ! diffuse solar rad
INTEGER                    :: JSWB
!
! albedo & emissivity
!
REAL, DIMENSION(SIZE(PTA)) :: ZEMIS_GD   ! emissivity for green areas
REAL, DIMENSION(SIZE(PTA)) :: ZALB_GR    ! albedo     for green roofs
REAL, DIMENSION(SIZE(PTA)) :: ZEMIS_GR   ! emissivity for green roofs
!
REAL, DIMENSION(SIZE(PTA)) :: ZALBNIR_TVEG_GD  ! nearIR  veg tot albedo
REAL, DIMENSION(SIZE(PTA)) :: ZALBVIS_TVEG_GD  ! visible veg tot albedo
REAL, DIMENSION(SIZE(PTA)) :: ZALBNIR_TSOIL_GD ! nearIR  soil tot albedo
REAL, DIMENSION(SIZE(PTA)) :: ZALBVIS_TSOIL_GD ! visible soil tot albedo
!
REAL, DIMENSION(SIZE(PTA)) :: ZALBNIR_TVEG_GR  ! nearIR  veg tot albedo
REAL, DIMENSION(SIZE(PTA)) :: ZALBVIS_TVEG_GR  ! visible veg tot albedo
REAL, DIMENSION(SIZE(PTA)) :: ZALBNIR_TSOIL_GR ! nearIR  soil tot albedo
REAL, DIMENSION(SIZE(PTA)) :: ZALBVIS_TSOIL_GR ! visible soil tot albedo
!
! radiation received by surfaces
!
REAL, DIMENSION(SIZE(PTA)) :: ZREC_SW_RF ! solar rad received by roofs in presence of solar panels
!
! coefficients for LW contributions
!
REAL, DIMENSION(SIZE(PTA)) :: ZREC_SW_WIN ! solar received by windows [W m-2(win)]
REAL, DIMENSION(SIZE(PTA)) :: ZREC_LW_RF  ! Incoming LW on roofs in presence of solar panels
REAL, DIMENSION(SIZE(PTA)) :: ZNOC_RF_RD
!
! local variable at previous time-step
!
REAL, DIMENSION(SIZE(PTA)) :: ZPET_A_COEF          
REAL, DIMENSION(SIZE(PTA)) :: ZPET_B_COEF          
REAL, DIMENSION(SIZE(PTA)) :: ZPEQ_A_COEF          
REAL, DIMENSION(SIZE(PTA)) :: ZPEQ_B_COEF          
!
REAL, DIMENSION(SIZE(PTA)) :: ZUW_RD       ! momentum flux for roads
REAL, DIMENSION(SIZE(PTA)) :: ZUW_GD       ! momentum flux for green areas
REAL, DIMENSION(SIZE(PTA)) :: ZUW_GR       ! momentum flux for green roofs
REAL, DIMENSION(SIZE(PTA)) :: ZDUWDU_RD    !
!
REAL, DIMENSION(SIZE(PTA)) :: ZHU_AGG_GD   ! aggreg. relative humidity for green areas
REAL, DIMENSION(SIZE(PTA)) :: ZHU_AGG_GR   ! aggreg. relative humidity for green roofs
REAL, DIMENSION(SIZE(PTA)) :: ZAC_HVEG     ! aeodynamic conductance for trees (H)
REAL, DIMENSION(SIZE(PTA)) :: ZHU_HVEG     ! relative humidity for trees
!
!  surfaces relative fractions
!
REAL, DIMENSION(SIZE(PTA)) :: ZRF_FRAC        ! roof, wall 
REAL, DIMENSION(SIZE(PTA)) :: ZWL_FRAC        ! road and
REAL, DIMENSION(SIZE(PTA)) :: ZRD_FRAC        ! high vegetation
REAL, DIMENSION(SIZE(PTA)) :: ZGD_FRAC        ! fractions  
REAL, DIMENSION(SIZE(PTA)) :: ZHVEG_FRAC      ! of exchange surf.               
REAL, DIMENSION(SIZE(PTA)) :: ZWL_O_RD        ! wall surface over road surface
REAL, DIMENSION(SIZE(PTA)) :: ZWL_O_GRND      ! wall surface over (road+green area) surface
!
! surface temperatures
!
REAL, DIMENSION(SIZE(PTA)) :: ZMTC_O_GR_R1        ! mean thermal conductivity over distance 
!                                                 ! between two layers (bottom GR & roof)
!
! fluxes from green surfaces
!
REAL, DIMENSION(SIZE(PTA)) :: ZSFCO2_HV      ! CO2 fluxes (m/s*kg_CO2/kg_air) for high vegetation
REAL, DIMENSION(SIZE(PTA)) :: ZSFCO2_GD      ! CO2 fluxes (m/s*kg_CO2/kg_air)
REAL, DIMENSION(SIZE(PTA)) :: ZEMIT_LW_GD    ! LW flux emitted by the garden (W/m2 garden)
REAL, DIMENSION(SIZE(PTA)) :: ZSFCO2_GR      ! CO2 fluxes over greenroofs (m/s*kg_CO2/kg_air)
!
! fluxes for ground-based vegetation 
!
REAL, DIMENSION(SIZE(PTA)) :: ZRN_GD    
REAL, DIMENSION(SIZE(PTA)) :: ZH_GD          
REAL, DIMENSION(SIZE(PTA)) :: ZLE_GD    
REAL, DIMENSION(SIZE(PTA)) :: ZGFLUX_GD 
REAL, DIMENSION(SIZE(PTA)) :: ZEVAP_GD  
REAL, DIMENSION(SIZE(PTA)) :: ZRUNOFF_GD
REAL, DIMENSION(SIZE(PTA)) :: ZDRAIN_GD
!
REAL, DIMENSION(SIZE(PTA)) :: ZRN_GR   
REAL, DIMENSION(SIZE(PTA)) :: ZH_GR     
REAL, DIMENSION(SIZE(PTA)) :: ZLE_GR    
REAL, DIMENSION(SIZE(PTA)) :: ZGFLUX_GR 
REAL, DIMENSION(SIZE(PTA)) :: ZEVAP_GR  
REAL, DIMENSION(SIZE(PTA)) :: ZRUNOFF_GR
REAL, DIMENSION(SIZE(PTA)) :: ZDRAIN_GR  
!
! fluxes from built surfaces
REAL, DIMENSION(SIZE(PTA)) :: ZEMIT_LW_RD    ! LW flux emitted by the road (W/m2 road)
!
! fluxes from/to solar panel
REAL, DIMENSION(SIZE(PTA)) :: ZEMIT_LWDN_PANEL  ! LW flux emitted DOWNWARDS by the solar panel (W/m2 panel)
REAL, DIMENSION(SIZE(PTA)) :: ZEMIT_LW_RF       ! LW flux emitted UPWARDS   by the roof        (W/m2 roof )
!
REAL, DIMENSION(SIZE(PTA),BOP%NBEMCOMP) :: ZMOD        ! Modulation factor according to building occupation
REAL, DIMENSION(SIZE(PTA),BOP%NBEMCOMP) :: ZQINMOD     ! Internal heat load modulated according to building occupation
 !
! Heating and cooling design temperature after modulation
! 
REAL, DIMENSION(SIZE(PTA),BOP%NBEMCOMP) :: ZTHEAT_TARGET
REAL, DIMENSION(SIZE(PTA),BOP%NBEMCOMP) :: ZTCOOL_TARGET
!
!new local variables for shading
REAL, DIMENSION(SIZE(PTA),BOP%NBEMCOMP) :: ZVENT_BEHAV_ADAPTI   ! Fraction of windows available for occupants adaptive actions
REAL, DIMENSION(SIZE(PTA),BOP%NBEMCOMP) :: ZVENT_BEHAV_ANYWAY   ! Fraction of windows closed in any case
!
REAL, DIMENSION(SIZE(PTA))     :: ZE_SHADING        ! energy not ref., nor absorbed, nor trans. by glazing [Wm-2(win)]
REAL, DIMENSION(SIZE(PTA))     :: ZDEEP_FLUX        ! deep flux calculated by isba
REAL, DIMENSION(SIZE(PTA),BOP%NBEMCOMP) :: ZISNIGHT          ! Switch for day and night
!
REAL, DIMENSION(SIZE(PTA)) :: ZIMB_SURF_GREENROOF ! Energy budget imbalance at surface of green roof
REAL, DIMENSION(SIZE(PTA)) :: ZCST_H_WASTE_CANY   ! Sensible waste heat released to canyon [W m-2(tot)]
REAL, DIMENSION(SIZE(PTA)) :: ZCST_LE_WASTE_CANY  ! Latent waste heat released to canyon [W m-2(tot)]
REAL, DIMENSION(SIZE(PTA)) :: ZCOE_H_WASTE_CANY
REAL, DIMENSION(SIZE(PTA)) :: ZCOE_LE_WASTE_CANY
REAL, DIMENSION(SIZE(PTA)) :: ZMUL_H_WASTE_CANY
REAL, DIMENSION(SIZE(PTA)) :: ZMUL_LE_WASTE_CANY
!
! local variables for urban trees
INTEGER,PARAMETER                 :: NCAN=2          ! Number of layers in the canyon
REAL, DIMENSION(SIZE(PTA))        :: ZALB_HVEG       ! albedo for high vegetation
REAL, DIMENSION(SIZE(PTA))        :: ZEMIS_HVEG      ! emissivity for high vegetation
REAL, DIMENSION(SIZE(PTA),NCAN)   :: ZTRANS_HVEG     ! transmissivity profile within canyon zones
REAL, DIMENSION(SIZE(PTA))        :: ZTRANS_HVCR     ! transmissivity profile through tree crown 
REAL, DIMENSION(SIZE(PTA),TOP%NTEB_SOIL) :: ZLE_HVEG ! Profile of impact of Tree transpiration on the soil
REAL, DIMENSION(SIZE(PTA),TOP%NTEB_SOIL) :: ZLE_HVEG_FOR_GARDEN ! Profile of mpact of Tree transpiration on the soil
! 
! local variables for urban hydrology
!                                              
INTEGER, DIMENSION(SIZE(PTA))            :: JWG_LAYER           ! number of moisture layers
REAL, DIMENSION(SIZE(PTA),TOP%NTEB_SOIL) :: ZSOILHCAPZ_ROAD     ! soil heat capacity for roads accounting for hydro
REAL, DIMENSION(SIZE(PTA),TOP%NTEB_SOIL) :: ZSOILCONDZ_ROAD     ! soil thermal conductivity for roads accounting for hydro
REAL, DIMENSION(SIZE(PTA),TOP%NTEB_SOIL) :: ZSOILHCAPZ_BLD      ! soil heat capacity under buildings accounting for hydro
REAL, DIMENSION(SIZE(PTA),TOP%NTEB_SOIL) :: ZSOILCONDZ_BLD      ! soil thermal conductivity under buildings accounting for hydro
REAL, DIMENSION(SIZE(PRR))               :: ZRR                 ! rain rate
REAL, DIMENSION(SIZE(PTA))               :: ZIRRIG_ROOF       ! water irrigation for roofs
REAL, DIMENSION(SIZE(PTA))               :: ZWS_ROOF_MAX      ! maximum water capacity of ROOF reservoir (mm) 
REAL, DIMENSION(SIZE(PTA))               :: ZWS_ROAD_MAX      ! maximum water capacity of ROAD reservoir (mm) 
REAL, DIMENSION(SIZE(PTA))               :: ZCONNEX           ! impervious surface connection fraction to sewer (-)
!
INTEGER :: JI, JJ, JCOMP, JLAYER

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*      1.     Initializations
!              ---------------
!
!*      1.0    broadband radiative fluxes
!              --------------------------
!
IF (LHOOK) CALL DR_HOOK('TOWN_ENERGY_BALANCE',0,ZHOOK_HANDLE)
!
GDD   => GDM%VD%ND%AL(KTEB_P)
GDDE  => GDM%VD%NDE%AL(KTEB_P)
GDDM  => GDM%VD%NDM%AL(KTEB_P)
!
GRD   => GRM%VD%ND%AL(KTEB_P)
GRDE  => GRM%VD%NDE%AL(KTEB_P)
GRDM  => GRM%VD%NDM%AL(KTEB_P)
!
IF (HTEST/='OK') THEN
   CALL ABOR1_SFX('TEB_GARDEN: FATAL ERROR DURING ARGUMENT TRANSFER')
ENDIF
!
ZDIR_SW(:) = 0.
ZSCA_SW(:) = 0.
!
DO JSWB=1,KSW
  DO JJ=1,SIZE(PDIR_SW,1)
    ZDIR_SW(JJ) = ZDIR_SW(JJ) + PDIR_SW(JJ,JSWB)
    ZSCA_SW(JJ) = ZSCA_SW(JJ) + PSCA_SW(JJ,JSWB)
  ENDDO
END DO
!
!
!*      1.1    surfaces relative fractions
!              ---------------------------
!
IF (TOP%CURBTREE.EQ.'NONE') THEN                       
   IF (MAXVAL(ABS(T%XURBTREE)).GT.XSURF_EPSILON) THEN
       CALL ABOR1_SFX ("TOWN_ENERGY_BALANCE: High vegetation fraction for HURBTREE=NONE specified")
   ENDIF
ENDIF
!
!
DO JJ=1,SIZE(T%XROAD)
   !
   T%XTOTS_O_HORS(JJ) = 1. + T%XWALL_O_HOR(JJ) + T%XURBTREE(JJ) *(1.-T%XBLD(JJ))
   !
   ZRF_FRAC  (JJ) = T%XBLD       (JJ) 
   ZWL_FRAC  (JJ) = T%XWALL_O_HOR(JJ) 
   ZRD_FRAC  (JJ) = T%XROAD      (JJ) 
   ZGD_FRAC  (JJ) = T%XGARDEN    (JJ) 
   ZHVEG_FRAC(JJ) = T%XURBTREE   (JJ) *(1.-T%XBLD(JJ))
   ZWL_O_RD  (JJ) = ZWL_FRAC     (JJ) / ZRD_FRAC(JJ)
   ZWL_O_GRND(JJ) = ZWL_FRAC     (JJ) / (ZRD_FRAC(JJ)+ZGD_FRAC(JJ))
   !
   IF ( ABS(T%XTOTS_O_HORS(JJ)-ZRF_FRAC(JJ)-ZWL_FRAC(JJ)-ZRD_FRAC(JJ)-ZGD_FRAC(JJ)-ZHVEG_FRAC(JJ)).GT.1.0E-6) THEN
      STOP ("Wrong TOTS_O_HOR fractions")
   ENDIF
   !
ENDDO
!
!-------------------------------------------------------------------------------
!
!*      2.     Snow-covered surfaces relative effects
!              --------------------------------------
!
!*      2.1    Snow-covered surfaces relative fractions (at previous time-step)
!              ----------------------------------------
 CALL SNOW_FRAC_ROAD(T%TSNOW_ROAD%WSNOW(:,1),PSR(:)>0.,PDN_RD,ZDF_RD)
 CALL SNOW_FRAC_ROOF(T%TSNOW_ROOF%WSNOW(:,1),PSR(:)>0.,PDN_RF,ZDF_RF)
!
!* new snow albedo
!
WHERE (T%TSNOW_ROAD%WSNOW(:,1)==0. .AND. PSR(:)>0.) T%TSNOW_ROAD%ALB(:) = XANSMAX
WHERE (T%TSNOW_ROOF%WSNOW(:,1)==0. .AND. PSR(:)>0.) T%TSNOW_ROOF%ALB(:) = XANSMAX
!
!*      2.2    If snow was not present at previous time-step but is falling
!              ------------------------------------------------------------
!
WHERE (T%TSNOW_ROAD%WSNOW(:,1)==0. .AND. PSR(:)>0.)
  T%TSNOW_ROAD%ALB (:) = XANSMAX
  T%TSNOW_ROAD%EMIS(:) = XEMISSN
  T%TSNOW_ROAD%TS  (:) = MIN(T%XT_ROAD(:,1), XTT)
END WHERE
WHERE (T%TSNOW_ROOF%WSNOW(:,1)==0. .AND. PSR(:)>0.)
  T%TSNOW_ROOF%ALB (:) = XANSMAX
  T%TSNOW_ROOF%EMIS(:) = XEMISSN
  T%TSNOW_ROOF%TS  (:) = MIN(T%XT_ROOF(:,1), XTT)
END WHERE
!
!*      2.3    Radiative snow variables at previous time-step
!              ----------------------------------------------
!
ZESN_RF  (:) = T%TSNOW_ROOF%EMIS(:)
ZESN_RD  (:) = T%TSNOW_ROAD%EMIS(:)
ZTSSN_RF (:) = T%TSNOW_ROOF%TS  (:)
ZTSSN_RD (:) = T%TSNOW_ROAD%TS  (:)
!
!-------------------------------------------------------------------------------
!
!*      3.     Extrapolation of atmospheric T and q at roof level (for fluxes computation)
!              --------------------------------------------------
!
ZTA_RF(:) = PTA(:) * PEXNS(:) / PEXNA(:)
ZQA_RF(:) = PQA(:) * QSAT(ZTA_RF(:),PPS(:)) / QSAT(PTA(:),PPA(:))
!
!-------------------------------------------------------------------------------
!
!*      4.     Grid-averaged albedo and emissivity of green areas
!              --------------------------------------------------
!
DMT%XALB_GD = XUNDEF
ZEMIS_GD    = XUNDEF
PTSRAD_GD   = XUNDEF
!
IF (TOP%LGARDEN) THEN
 CALL TEB_VEG_PROPERTIES(T%XGARDEN, GDM%O, GDM%NPE%AL(KTEB_P), &
                        PDIR_SW, PSCA_SW, PSW_BANDS, KSW,      &
                        PTSRAD_GD, ZEMIS_GD, DMT%XALB_GD,      &
                        PTA=PT_LOWCAN,                      &
                        PALBNIR_TVEG=ZALBNIR_TVEG_GD,       &
                        PALBVIS_TVEG=ZALBVIS_TVEG_GD,       &
                        PALBNIR_TSOIL=ZALBNIR_TSOIL_GD,     &
                        PALBVIS_TSOIL=ZALBVIS_TSOIL_GD     )
ENDIF
!
! for greenroofs :
!
ZALB_GR   = XUNDEF
ZEMIS_GR  = XUNDEF
PTSRAD_GR = XUNDEF
!
IF (TOP%LGREENROOF) THEN
 CALL TEB_VEG_PROPERTIES(T%XGREENROOF, GRM%O, GRM%NPE%AL(KTEB_P), &
                           PDIR_SW, PSCA_SW, PSW_BANDS, KSW,    &
                           PTSRAD_GR, ZEMIS_GR, ZALB_GR,        &
                           PTA=PTA,                             &
                           PALBNIR_TVEG=ZALBNIR_TVEG_GR,        &
                           PALBVIS_TVEG=ZALBVIS_TVEG_GR,        &
                           PALBNIR_TSOIL=ZALBNIR_TSOIL_GR,      &
                           PALBVIS_TSOIL=ZALBVIS_TSOIL_GR  ) 
ENDIF
!
! for urban trees :
!              
ZEMIS_HVEG(:) = XEMISVEG
!
IF (TOP%CURBTREE == 'TREE' .OR. TOP%CURBTREE == 'GRWL') THEN
 CALL URBTREE_PROPERTIES (GDM%NPEHV%AL(KTEB_P), GDM%PHV, T, NCAN, ZTRANS_HVEG, ZTRANS_HVCR, HPROGRAM)
    T%XTAU_SW(:) = 1. - T%XURBTREE(:)*(1.-ZTRANS_HVEG(:,2))
    T%XTAU_SR(:) = 1. - T%XURBTREE(:)*(1.-ZTRANS_HVCR(:))
    T%XTAU_WW(:) = 1. - T%XURBTREE(:)*(1.-ZTRANS_HVCR(:))
    T%XTAU_WR(:) = 1. - T%XURBTREE(:)*(1.-ZTRANS_HVEG(:,1))
    ZALB_HVEG(:) = 0.5 * (GDM%NPEHV%AL(KTEB_P)%XALBNIR_VEG + GDM%NPEHV%AL(KTEB_P)%XALBVIS_VEG)
ELSE
    ZTRANS_HVCR(:)   = 1.      
    ZTRANS_HVEG(:,:) = 0.5      
    T%XTAU_SW  (:)   = 1.
    T%XTAU_SR  (:)   = 1. 
    T%XTAU_WW  (:)   = 1. 
    T%XTAU_WR  (:)   = 1. 
    ZALB_HVEG  (:)   = XUNDEF
ENDIF
!
!-------------------------------------------------------------------------------
!
!*      5.     Occupation of buildings
!              -----------------------
!
!* when building in unoccupied, target temperature is modified
!
IF (TOP%CBEM=="BEM") THEN
  !
  ! Determination of building occupation status
  !
  DO JCOMP=1,BOP%NBEMCOMP
    CALL BLD_OCC_CALENDAR(HPROGRAM,TOP%TTIME%TDATE%YEAR,TOP%TTIME%TDATE%MONTH,TOP%TTIME%TDATE%DAY, &
                          PTSUN,B%XDAYWBEG_SCHED(:,:,JCOMP),B%XHOURBEG_SCHED(:,:,JCOMP),                              &
                          B%XPROBOCC(:,:,JCOMP), B%XBEG_HOLIDAY(:,:,JCOMP), B%XEND_HOLIDAY(:,:,JCOMP),                &
                          B%XMOD_HOLIDAY(:,JCOMP), DMT%XBLDOCC(:,JCOMP), ZISNIGHT(:,JCOMP)                              )
  ENDDO
  !
  IF ((MINVAL(DMT%XBLDOCC).LT.0.0).OR.(MAXVAL(DMT%XBLDOCC).GT.1.0)) THEN
     CALL ABOR1_SFX("TOWN_ENERGY_BALANCE: Wrong probability of building occupation")
  ENDIF
  !
  IF ((MINVAL(ZISNIGHT).LT.0.0).OR.(MAXVAL(ZISNIGHT).GT.1.0)) THEN
     CALL ABOR1_SFX("TOWN_ENERGY_BALANCE: Wrong day/night switch")
  ENDIF
  !
  ! Modulation of the internal heat release
  ! At the moment the long-term vacancies are excluded.
  !
  ZMOD = ZISNIGHT  * (DMT%XBLDOCC*B%XMODQIN_NIG + (1.0-DMT%XBLDOCC) * B%XMODQIN_NIG) + &
        (1.0-ZISNIGHT) * (DMT%XBLDOCC               + (1.0-DMT%XBLDOCC) * B%XMODQIN_VCD)
  !
  IF ((MINVAL(ZMOD).LT.0.0).OR.(MAXVAL(ZMOD).GT.1.0)) THEN
     CALL ABOR1_SFX("TOWN_ENERGY_BALANCE: Wrong modulation factor")
  ENDIF
  !
  ZQINMOD (:,:) = ZMOD(:,:) * B%XQIN(:,:)
  DO JCOMP=1,SIZE(B%XHOTWAT,2)
      DMT%XCOMP_HOTWAT(:,JCOMP) = ZMOD(:,JCOMP)*B%XHOTWAT(:,JCOMP)* B%XN_FLOOR(:)
  ENDDO
  !
  IF ((MINVAL(ZQINMOD).LT.0.0).OR.(MAXVAL(ZQINMOD).GT.50.1)) THEN
     CALL ABOR1_SFX("TOWN_ENERGY_BALANCE: Unplausible value for internal heat release")
  ENDIF
  !
  ! Modulation of heating and cooling design temperature
  ! according to day/night and building occupation status.
  ! At the moment no long-term vacancies.
  !
  ZTHEAT_TARGET(:,:) =                                                                           &
          ZISNIGHT(:,:)      *(DMT%XBLDOCC(:,:)*B%XTHEAT_OCCN(:,:)+(1.0-DMT%XBLDOCC(:,:))*B%XTHEAT_VCDN(:,:)) + &
          (1.0-ZISNIGHT(:,:))*(DMT%XBLDOCC(:,:)*B%XTHEAT_OCCD(:,:)+(1.0-DMT%XBLDOCC(:,:))*B%XTHEAT_VCDD(:,:))
  !
  ZTCOOL_TARGET(:,:) =                                                                           &
          ZISNIGHT(:,:)      *(DMT%XBLDOCC(:,:)*B%XTCOOL_OCCN(:,:)+(1.0-DMT%XBLDOCC(:,:))*B%XTCOOL_VCDN(:,:)) + &
          (1.0-ZISNIGHT(:,:))*(DMT%XBLDOCC(:,:)*B%XTCOOL_OCCD(:,:)+(1.0-DMT%XBLDOCC(:,:))*B%XTCOOL_VCDD(:,:))
  !
  ! Plausibility checks for input variables
  !
  IF ((MINVAL(ZTHEAT_TARGET).LT.170.0).OR.(MAXVAL(ZTHEAT_TARGET).GT.300.0)) THEN
      CALL ABOR1_SFX("Unrealistic heating design temperature")
  ENDIF
  !
  IF ((MINVAL(ZTCOOL_TARGET).LT.280.0).OR.(MAXVAL(ZTCOOL_TARGET).GT.400.1)) THEN
     CALL ABOR1_SFX("Unrealistic cooling design temperature")
  ENDIF
  !
  ! Determine shading and ventilation availability according to human behaviour settings
  !
  B%XSHAD_BEHAV_ANYWAY(:,:) = XUNDEF
  B%XSHAD_BEHAV_ADAPTI(:,:) = XUNDEF
  ZVENT_BEHAV_ANYWAY(:,:) = XUNDEF
  ZVENT_BEHAV_ADAPTI(:,:) = XUNDEF
  !
  DO JJ=1,SIZE(ZISNIGHT,1)
     DO JCOMP=1,SIZE(ZISNIGHT,2)
        !
        B%XSHAD_BEHAV_ANYWAY(JJ,JCOMP) = ZISNIGHT(JJ,JCOMP) * B%XFSNIG(JJ,JCOMP) + &
              (1.0-ZISNIGHT(JJ,JCOMP))*(1.0-DMT%XBLDOCC(JJ,JCOMP))* B%XSHADVACSW(JJ,JCOMP) * B%XFSVAC(JJ,JCOMP)
        !
        ZVENT_BEHAV_ANYWAY(JJ,JCOMP) = ZISNIGHT(JJ,JCOMP) * B%XVENTNIGSW(JJ,JCOMP) * B%XFVNIG(JJ,JCOMP) + &
              (1.0-ZISNIGHT(JJ,JCOMP))*(1.0-DMT%XBLDOCC(JJ,JCOMP))* B%XFVVAC(JJ,JCOMP)
        !
        B%XSHAD_BEHAV_ADAPTI(JJ,JCOMP) = (1.0-ZISNIGHT(JJ,JCOMP))*DMT%XBLDOCC(JJ,JCOMP)*B%XFSSUM(JJ,JCOMP)
        ZVENT_BEHAV_ADAPTI(JJ,JCOMP) = (1.0-ZISNIGHT(JJ,JCOMP))*DMT%XBLDOCC(JJ,JCOMP)*B%XFVSUM(JJ,JCOMP)        
        !
     ENDDO
  ENDDO
  !
  ! Plausibility checks
  !
  IF ((MINVAL(B%XSHAD_BEHAV_ANYWAY).LT.-XSURF_EPSILON).OR.(MAXVAL(B%XSHAD_BEHAV_ANYWAY).GT.(1.0+XSURF_EPSILON))) THEN
      CALL ABOR1_SFX("Unrealistic shading behaviour")
  ENDIF
  !
  IF ((MINVAL(B%XSHAD_BEHAV_ADAPTI).LT.-XSURF_EPSILON).OR.(MAXVAL(B%XSHAD_BEHAV_ADAPTI).GT.(1.0+XSURF_EPSILON))) THEN
     CALL ABOR1_SFX("Unrealistic shading behaviour")
  ENDIF
  !
  IF ((MINVAL(B%XSHAD_BEHAV_ANYWAY+B%XSHAD_BEHAV_ADAPTI).LT.-XSURF_EPSILON).OR. &
       (MAXVAL(B%XSHAD_BEHAV_ANYWAY+B%XSHAD_BEHAV_ADAPTI).GT.(1.0+XSURF_EPSILON))) THEN
     CALL ABOR1_SFX("Unrealistic shading behaviour")
  ENDIF
  !
  IF ((MINVAL(ZVENT_BEHAV_ANYWAY).LT.-XSURF_EPSILON).OR.(MAXVAL(ZVENT_BEHAV_ANYWAY).GT.(1.0+XSURF_EPSILON))) THEN
     CALL ABOR1_SFX("Unrealistic ventilation behaviour")
  ENDIF
  !
  IF ((MINVAL(ZVENT_BEHAV_ADAPTI).LT.-XSURF_EPSILON).OR.(MAXVAL(ZVENT_BEHAV_ADAPTI).GT.(1.0+XSURF_EPSILON))) THEN
     CALL ABOR1_SFX("Unrealistic ventilation behaviour")
  ENDIF
  !
  IF ((MINVAL(ZVENT_BEHAV_ANYWAY+ZVENT_BEHAV_ADAPTI).LT.-XSURF_EPSILON).OR. &
       (MAXVAL(ZVENT_BEHAV_ANYWAY+ZVENT_BEHAV_ADAPTI).GT.(1.0+XSURF_EPSILON))) THEN
      CALL ABOR1_SFX("Unrealistic ventilation behaviour")
  ENDIF
  !
  ! Calculation of the heat released inside BEM for output
  !
  DMT%XQINOUT(:)        = 0.0
  DMT%XQINOUTSEN(:)     = 0.0
  DMT%XQINOUTLAT(:)     = 0.0
  DMT%XCOMP_QINOUT(:,:) = XUNDEF
  !
  DO JCOMP=1,BOP%NBEMCOMP
     !
     DMT%XCOMP_QINOUT(:,JCOMP) = ZQINMOD(:,JCOMP) * B%XN_FLOOR(:)
     !
     DMT%XQINOUT(:) = DMT%XQINOUT(:) + B%XFRACOMP(:,JCOMP) * ZQINMOD(:,JCOMP) * B%XN_FLOOR(:)
     DMT%XQINOUTSEN(:) = DMT%XQINOUTSEN(:) + B%XFRACOMP(:,JCOMP) * ZQINMOD(:,JCOMP) * (1.0 - B%XQIN_FLAT(:)) * B%XN_FLOOR(:)
     DMT%XQINOUTLAT(:) = DMT%XQINOUTLAT(:) + B%XFRACOMP(:,JCOMP) * ZQINMOD(:,JCOMP) * B%XQIN_FLAT(:) * B%XN_FLOOR(:)     
     !
  ENDDO
  !
  IF ( MAXVAL(ABS(DMT%XQINOUT(:)-DMT%XQINOUTSEN(:)-DMT%XQINOUTLAT(:))).GT.XSURF_EPSILON) THEN
     CALL ABOR1_SFX("TOWN_ENERGY_BALANCE: Wrong partitioning between sensible and latent heat")
  ENDIF
  !
ELSE
   ZQINMOD (:,:) = XUNDEF
ENDIF
!
!-------------------------------------------------------------------------------
!
!*     6.     Solar radiation
!              ---------------
!
!
!*       6.1   computes solar radiation exchanges
!              ----------------------------------
!
IF (TOP%LSPARTACUS) THEN
   !
   ! Only in combination with TEB-canopy
   !
   IF (.NOT.TOP%LCANOPY) THEN
      CALL ABOR1_SFX("TOWN_ENERGY_BALANCE: canopy option must be used in combination with SPARTACUS")
   ENDIF
   !
   ! Longwave exchanges must be explicit
   !
   IF (.NOT.TOP%LEXPLW) THEN
      CALL ABOR1_SFX("TOWN_ENERGY_BALANCE: explicit longwave exchanges must be used in combination with SPARTACUS")
   ENDIF
   !
   ! Calculate urban radiation using the SPARTACUS-Surface radiation scheme
   !
   CALL TEB_SPARTACUS(TOP, SPAOP, T, B, DMT, GDM%P, SB, TPN, ZDIR_SW, ZSCA_SW, PZENITH, PLW_RAD, &
        TPN%XFRAC_PANEL, TPN%XALB_PANEL, DMT%XALB_GD, ZALB_GR, ZALB_HVEG, ZEMIS_GD, ZEMIS_GR,    &
        ZEMIS_HVEG, PTSRAD_GD, PTSRAD_GR, PTS_HVEG, GDM%NPEHV%AL(KTEB_P)%XLAI, PDN_RF, ZDF_RF,   &
        PDN_RD, ZDF_RD, ZTRANS_HVCR, DMT%XREC_SW_GARDEN, ZREC_SW_RF, PDIR_ALB_TWN, PSCA_ALB_TWN, &
        PREF_SW_GRND, PREF_SW_FAC, PREF_SW_HVEG, ZE_SHADING, B%XSHAD_BEHAV_ANYWAY,                 &
        B%XSHAD_BEHAV_ADAPTI, PSCA_SW_GROUND_DOWN, PSCA_SW_GROUND_UP, PSCA_SW_GROUND_HOR,          &
        PLW_GROUND_DOWN, PLW_GROUND_HOR, "OK" )
   !
ELSE
   !
   ! Classical TEB solar radiation calculation
   !
   CALL URBAN_SOLAR_ABS(TOP, T, B, DMT, GDM%PHV, ZDIR_SW, ZSCA_SW, PZENITH, PAZIM,    &
        TPN%XFRAC_PANEL, TPN%XALB_PANEL, DMT%XALB_GD, ZALB_GR, ZALB_HVEG, PDN_RF, ZDF_RF, &
        PDN_RD, ZDF_RD, ZTRANS_HVCR, DMT%XREC_SW_GARDEN, ZREC_SW_RF, PDIR_ALB_TWN,    &
        PSCA_ALB_TWN, ZREC_SW_WIN, PREF_SW_GRND, PREF_SW_FAC, PREF_SW_HVEG,           &
        ZE_SHADING, B%XSHAD_BEHAV_ANYWAY, B%XSHAD_BEHAV_ADAPTI)
   !
   ! Diagnostics for UTCI calculation with SPARTACUS-Surface, which are not used
   ! in the case the classical solar radiation calculation is active.
   !
   PSCA_SW_GROUND_DOWN(:) = -XUNDEF
   PSCA_SW_GROUND_UP(:)   = -XUNDEF
   PSCA_SW_GROUND_HOR(:)  = -XUNDEF   
   PLW_GROUND_DOWN(:)     = -XUNDEF
   PLW_GROUND_HOR(:)      = -XUNDEF  
   !
ENDIF
!
!-------------------------------------------------------------------------------
!
!*      6.     LW properties
!              -------------
!
 CALL ALLOC_LW_COEF(LW,SIZE(PTA))
!
 CALL URBAN_LW_COEF(B, T, LW, PLW_RAD, ZEMIS_GD, T%TSNOW_ROAD%TS, PTSRAD_GD, &
                    ZEMIS_HVEG(:), PTS_HVEG(:)                               )
 !
 IF (.NOT.TOP%LSPARTACUS) THEN
    CALL EXPLICIT_LONGWAVE(TOP, T, B, LW, DMT, CT, PLW_RAD, PTSRAD_GD, &
         PTS_HVEG, PDN_RD, ZDF_RD, PDN_RF, ZEMIS_GD, ZEMIS_HVEG, "OK")
 ENDIF
!
!-------------------------------------------------------------------------------
!
! The subroutine is splitted in 2 because of compilation optimization issues
 CALL TOWN_ENERGY_BALANCE2
 CALL TOWN_ENERGY_BALANCE3
!
IF (LHOOK) CALL DR_HOOK('TOWN_ENERGY_BALANCE',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
SUBROUTINE TOWN_ENERGY_BALANCE2
!
!*     7.2     Effect of solar panels on incoming LW on roofs
!              ----------------------------------------------
!
IF (TOP%LSOLAR_PANEL) THEN
  !
  ! solar panels downwards surface is supposed to be at air temperature
  ! and to be have an emissivity of 1.
  !
  ZEMIT_LWDN_PANEL = XSTEFAN * PTA**4
  !
  ! note that, for the time being, one considers that the solar panel 
  ! intercept radiation both above roof and greenroofs (if any)
  ZREC_LW_RF = (1.-TPN%XFRAC_PANEL(:)) * PLW_RAD + TPN%XFRAC_PANEL(:) * ZEMIT_LWDN_PANEL
ELSE
  ZEMIT_LWDN_PANEL = XUNDEF
  ZREC_LW_RF       = PLW_RAD
END IF
!
!-------------------------------------------------------------------------------
!
!*      8.     Surface runoff from impervious surfaces
!              ---------------------------------------
!
!
ZIRRIG_ROOF    (:) = 0.
ZNOC_RF_RD     (:) = 0.
!
IF (.NOT. TOP%LURBHYDRO) THEN 
    ZWS_ROOF_MAX(:) = 1.
    ZWS_ROAD_MAX(:) = 1.
    ZCONNEX         = 1.
ELSE
    ZWS_ROOF_MAX(:) = HM%THP%XWS_ROOF_MAX
    ZWS_ROAD_MAX(:) = HM%THP%XWS_ROAD_MAX
    ZCONNEX         = HM%THP%XCONNEX
ENDIF
!
! Update of maximum water capicity reservoir depending on snow fractions
ZWS_ROOF_MAX(:) = ZWS_ROOF_MAX(:) * ZDF_RF(:) 
ZWS_ROAD_MAX(:) = ZWS_ROAD_MAX(:) * ZDF_RD(:)
!
!* Road watering
 CALL TEB_IRRIG(TIR%LPAR_RD_IRRIG, PTSTEP, TOP%TTIME%TDATE%MONTH, PTSUN,     &
                   TIR%XRD_START_MONTH, TIR%XRD_END_MONTH, TIR%XRD_START_HOUR,  &
                   TIR%XRD_END_HOUR, TIR%XRD_24H_IRRIG, DMT%XIRRIG_ROAD         )
!
! Roofs
ZIRRIG_ROOF(:) = 0.
 CALL URBAN_RUNOFF(ZWS_ROOF_MAX, T%XWS_ROOF, PRR, ZIRRIG_ROOF, PTSTEP, &
                      ZCONNEX, DMT%XRUNOFF_STRLROOF, DMT%XNOC_ROOF        )
!
! Roads
 CALL URBAN_RUNOFF(ZWS_ROAD_MAX, T%XWS_ROAD, PRR, DMT%XIRRIG_ROAD, PTSTEP, &
                      ZCONNEX, DMT%XRUNOFF_ROAD, DMT%XNOC_ROAD            )
!
! Surface runoff not connected to network returns to garden
ZRR      (:) = PRR(:)
IF (TOP%LURBHYDRO) THEN
   DO JJ=1,SIZE(PDIR_SW,1)
     IF (T%XGARDEN(JJ).GT.0.) THEN 
        ZNOC_RF_RD(JJ) = (DMT%XNOC_ROOF(JJ)*T%XBLD(JJ)+DMT%XNOC_ROAD(JJ)*T%XROAD(JJ))/T%XGARDEN(JJ)
!     ELSE FIXME
     ELSE
        DMT%XRUNOFF_ROAD(JJ)     = DMT%XRUNOFF_ROAD(JJ)     + DMT%XNOC_ROAD(JJ)
        DMT%XRUNOFF_STRLROOF(JJ) = DMT%XRUNOFF_STRLROOF(JJ) + DMT%XNOC_ROOF(JJ)
        DMT%XNOC_ROAD(JJ) = 0.
        DMT%XNOC_ROOF(JJ) = 0.
     ENDIF
   ENDDO
ENDIF
!
!-------------------------------------------------------------------------------
!
!*      8.     Treatment of green areas
!              ------------------------
!
!*             Implicit coeefs for T and Q
!
!* explicit coupling for the time being.
!  canopy may need implicitation if there is a lot a garden in the grid mesh
!
ZPET_A_COEF(:) = 0.
ZPET_B_COEF(:) = PT_LOWCAN(:) / PEXNS(:)
ZPEQ_A_COEF(:) = 0.
ZPEQ_B_COEF(:) = PQ_LOWCAN(:)
!
!----------------------------------------------------------------------------------------
IF (TOP%LGARDEN) THEN
!----------------------------------------------------------------------------------------
!
!
!*      8.1    Call ISBA for urban trees
!              -------------------------
!
!
!
! In this case, high vegetation is present and treated separately and in a simpler
! way than low vegetation. However, its impact on water content is taken into account.
!
! The specificities here are that:
!  - absorption of radiation is already computed before and provided as an argument
!  - The energy balance is computed following isba MEB approach (for vegetation foliage only) 
! evolution of the leaves. It does not impact the temperature of the soil or low vegetation.
!
  IF (TOP%CURBTREE/='NONE') THEN
   
    CALL GARDEN_HVEG (DTCO, G, SB, T, TOP, GDM, DMT, KTEB_P, TOP%TTIME, PTSUN, &
                      PTSTEP, PUREF, PTA_HVEG, PQA_HVEG, PEXNS, PRHOA, PCO2, PPS,  &
                      PZENITH, PVMOD,                                              & 
                      ZSFCO2_HV, PDH_HVEG, PDLE_HVEG, PLAD_CAN,                    &
                      ZQSAT_HVEG, ZAC_HVEG, ZHU_HVEG, ZLE_HVEG                     )
    !
    DMT%XTS_HVEG = GDM%NPEHV%AL(KTEB_P)%XTV
    !
  ELSE
    !
    DMT%XRN_HVEG   (:) = 0.
    DMT%XH_HVEG    (:) = 0.
    DMT%XLE_HVEG   (:) = 0.
    DMT%XGFLUX_HVEG(:) = 0.
    PDH_HVEG       (:,:)=0.
    PDLE_HVEG      (:,:)=0.
    PLAD_CAN       (:,:)=0.
    DMT%XTS_HVEG   (:)  =XUNDEF
    ZQSAT_HVEG     (:)  =XUNDEF
    ZAC_HVEG       (:)  =0.
    ZHU_HVEG       (:)  =0.
    ZSFCO2_HV      (:)  =0.
    ZLE_HVEG       (:,:)=0.
    !
  END IF
  !
  ! coefficient to indicate to garden ISBA how many water will be extracted
  ZLE_HVEG_FOR_GARDEN(:,:) = 0.
  DO JLAYER=1,TOP%NTEB_SOIL
    WHERE (T%XGARDEN(:)>0.) &
    ZLE_HVEG_FOR_GARDEN(:,JLAYER) = ZLE_HVEG(:,JLAYER) * T%XFRAC_HVEG(:) / T%XGARDEN(:)
  END DO
!
!----------------------------------------------------------------------------------------
!
!*     8.2     Call of ISBA for low vegetation
!              -------------------------------

  !
  CALL GARDEN(DTCO, G, T, TOP, TIR, AT, DMT, GDM%DTV, GDM%GB,  GDD, GDDE, GDDM,            &
            GDM%O, GDM%S, GDM%K, GDM%P, GDM%NPE%AL(KTEB_P),                            &
            HIMPLICIT_WIND, TOP%TTIME, PTSUN, PPEW_A_COEF_LOWCAN, PPEW_B_COEF_LOWCAN,  &
            ZPET_A_COEF, ZPEQ_A_COEF, ZPET_B_COEF, ZPEQ_B_COEF,                        &
            PTSTEP, PZ_LOWCAN, PT_LOWCAN, PQ_LOWCAN, PEXNS, PRHOA, PCO2, PPS, ZRR,     &
            PSR, PZENITH, PAZIM, DMT%XREC_SW_GARDEN, DMT%XREC_LW_GARDEN, ZLE_HVEG_FOR_GARDEN, PU_LOWCAN,   &
            ZALBNIR_TVEG_GD, ZALBVIS_TVEG_GD,                                          &
            ZALBNIR_TSOIL_GD, ZALBVIS_TSOIL_GD,                                        &
            ZSFCO2_GD, ZUW_GD,  PAC_GD, ZQSAT_GD, PTSRAD_GD,                           &
            DMT%XAC_AGG_GD, ZHU_AGG_GD, DMT%XIRRIG_GARDEN, ZNOC_RF_RD, ZDEEP_FLUX)  
  !
  !
  PAC_GD_WAT(:)          = PAC_GD(:)
  DMT%XABS_SW_GARDEN(:)  = (1.-DMT%XALB_GD(:)) * DMT%XREC_SW_GARDEN
  DMT%XABS_LW_GARDEN(:)  = ZEMIS_GD(:) * DMT%XREC_LW_GARDEN(:) - &
                                XSTEFAN * ZEMIS_GD(:) * PTSRAD_GD(:)**4 
  ZEMIT_LW_GD(:)         = XSTEFAN * PTSRAD_GD(:)**4 + &
                                (1 - ZEMIS_GD(:)) / ZEMIS_GD(:) * DMT%XABS_LW_GARDEN(:)   
  !
  !Fluxes for aggregated urban vegetation in canyon
  !
  ZRN_GD     = GDD%XRN
  ZGFLUX_GD  = GDD%XGFLUX
  ZEVAP_GD   = GDD%XEVAP
  ZRUNOFF_GD = GDDE%XRUNOFF
  ZDRAIN_GD  = GDDE%XDRAIN
  !
  ! Case NO GARDEN
ELSE
  !
  DMT%XH_HVEG        (:) = 0.
  DMT%XLE_HVEG       (:) = 0.
  DMT%XABS_LW_HVEG   (:) = XUNDEF
  !
  ZRN_GD     = 0.0
  ZH_GD      = 0.0
  ZLE_GD     = 0.0
  ZGFLUX_GD  = 0.0
  ZEVAP_GD   = 0.0
  ZRUNOFF_GD = 0.0
  ZDRAIN_GD  = 0.0
  !
  ZUW_GD         (:) = 0.
  PAC_GD         (:) = 0.
  ZSFCO2_GD      (:) = 0.
  ZSFCO2_HV      (:) = 0.
  ZQSAT_GD       (:) = XUNDEF
  PTSRAD_GD      (:) = XUNDEF
  DMT%XAC_AGG_GD (:) = XUNDEF
  ZHU_AGG_GD     (:) = XUNDEF
  PAC_GD_WAT     (:) = XUNDEF
  ZQSAT_HVEG     (:) = XUNDEF
  DMT%XTS_HVEG   (:) = XUNDEF
  ZAC_HVEG       (:) = 0.
  ZHU_HVEG       (:) = 0.
  DMT%XABS_SW_GARDEN (:) = XUNDEF
  DMT%XABS_LW_GARDEN (:) = XUNDEF
  ZEMIT_LW_GD    (:) = 0.
  DMT%XIRRIG_GARDEN  (:) = 0.
  !
ENDIF
!
!*      8.3    Call ISBA for greenroofs
!              -------------------------
!
IF (TOP%LGREENROOF) THEN
  !
  CALL GREENROOF(DTCO, G, T, TOP, TIR, AT, GRM%DTV, GRM%GB, GRD, GRDE, GRDM,   &
                 GRM%O, GRM%S, GRM%K, GRM%P, GRM%NPE%AL(KTEB_P),                     &
                 HIMPLICIT_WIND, TOP%TTIME, PTSUN, PPEW_A_COEF, PPEW_B_COEF,         &
                 ZPET_A_COEF, ZPEQ_A_COEF, ZPET_B_COEF, ZPEQ_B_COEF, PTSTEP, PZREF,  &
                 PUREF, PTA, PQA, PEXNS, PEXNA,PRHOA, PCO2, PPS, PRR, PSR, PZENITH,  &
                 PAZIM, ZREC_SW_RF, ZREC_LW_RF, PVMOD,ZALBNIR_TVEG_GR, ZALBVIS_TVEG_GR,     &
                 ZALBNIR_TSOIL_GR, ZALBVIS_TSOIL_GR, ZSFCO2_GR, ZUW_GR,              &
                 PAC_GR,ZQSAT_GR, PTSRAD_GR, DMT%XAC_AGG_GR, ZHU_AGG_GR,                 &
                 DMT%XG_GREENROOF_ROOF, DMT%XIRRIG_GREENROOF )
  !
  PAC_GR_WAT(:) = PAC_GR(:)
  DMT%XABS_SW_GREENROOF(:) = (1.-ZALB_GR(:)) * ZREC_SW_RF
  DMT%XABS_LW_GREENROOF(:) = ZEMIS_GR * ZREC_LW_RF - XSTEFAN * ZEMIS_GR * PTSRAD_GR**4
  !
  !
  ZIMB_SURF_GREENROOF(:)=T%XBLD(:)*T%XGREENROOF(:)*(GRD%XRN(:)-GRD%XLE(:)-GRD%XH(:)-GRD%XGFLUX(:))
  !
  IF (CT%LCHECK_TEB) THEN
     CT%XSEN_GREENROOF(:)=T%XBLD(:)*T%XGREENROOF(:)*(GRD%XGFLUX(:)-DMT%XG_GREENROOF_ROOF(:))
     !
     CT%XLAT_GREENROOF(:)=T%XBLD(:)*T%XGREENROOF(:)* (                             &
              XLVTT*(PRR(:)+DMT%XIRRIG_GREENROOF(:)-GRDE%XRUNOFF(:)-GRDE%XDRAIN(:) ) + &
              XLSTT*(PSR(:)) )
  END IF
  !
  ZRN_GR     = GRD%XRN
  ZH_GR      = GRD%XH 
  ZLE_GR     = GRD%XLE  
  ZGFLUX_GR  = GRD%XGFLUX 
  ZEVAP_GR   = GRD%XEVAP
  ZRUNOFF_GR = GRDE%XRUNOFF
  ZDRAIN_GR  = GRDE%XDRAIN
  !
ELSE
  !
  ZRN_GR     = 0.
  ZH_GR      = 0.
  ZLE_GR     = 0.
  ZGFLUX_GR  = 0.
  ZEVAP_GR   = 0.
  ZRUNOFF_GR = 0.
  ZDRAIN_GR  = 0.
  !
  ZUW_GR    (:) = 0.
  PAC_GR    (:) = 0.
  ZSFCO2_GR (:) = 0.
  ZQSAT_GR  (:) = XUNDEF
  PTSRAD_GR (:) = XUNDEF
  DMT%XAC_AGG_GR(:) = XUNDEF
  ZHU_AGG_GR(:) = XUNDEF 
  ZMTC_O_GR_R1(:) = XUNDEF 
  !
  DMT%XIRRIG_GREENROOF (:) = 0.
  DMT%XABS_SW_GREENROOF(:) = XUNDEF
  DMT%XABS_LW_GREENROOF(:) = XUNDEF
  DMT%XG_GREENROOF_ROOF(:) = XUNDEF
  !
  IF (CT%LCHECK_TEB) THEN
      CT%XSEN_GREENROOF(:)=0.
      CT%XLAT_GREENROOF(:)=0.
  END IF
  !
ENDIF

END SUBROUTINE TOWN_ENERGY_BALANCE2
!-------------------------------------------------------------------------------
SUBROUTINE TOWN_ENERGY_BALANCE3
!
!
!
!*     9.      Treatment of urban subsoil and hydrology
!              ----------------------------------------
!
IF (TOP%LURBHYDRO) THEN
   !
   CALL TEB_HYDRO(GDM%O, T, HM, KTEB_P,                             &
                      PTSTEP, PPS, PRR,                                 &
                      DMT%XRUNOFF_SW, DMT%XRUNOFF_WW,                   &
                      DMT%XRUNOFFSOIL_ROAD, DMT%XRUNOFFSOIL_BLD,        &
                      DMT%XDRAIN_ROAD, DMT%XDRAIN_BLD                   )
   !
   ! Hydraulic conductivity for soil column under gardens
   JWG_LAYER(:) = SIZE(GDM%NPE%AL(KTEB_P)%XWG,2)
   !
   CALL URBAN_HYDRO_COND(GDM%K%XBCOEF, GDM%K%XWSAT,                       &
                             GDM%P%XCONDSAT, GDM%K%XMPOTSAT,                  &
                             GDM%NPE%AL(KTEB_P)%XWG, GDM%NPE%AL(KTEB_P)%XWGI, & 
                             JWG_LAYER, HM%NTH%AL(KTEB_P)%XCOND_GD             )

   ! Horizontal transfer of water in the compartments
   CALL URBAN_HYDRO_HTRANSFERT(T, TOP, CT, GDM%NPE%AL(KTEB_P), HM%NTH%AL(KTEB_P), PTSTEP)
   !
   ! Thermal properties are modified according to hydrological state  
   !
   CALL UPDATE_THERMALPROP(HM%NTH%AL(KTEB_P)%XWG_ROAD,HM%NTH%AL(KTEB_P)%XWGI_ROAD,           &
                               T%XHCAPSOIL_ROAD, T%XCONDDRY_ROAD,  &
                               T%XCONDSLD_ROAD, T%XWSAT_ROAD,      &
                               ZSOILCONDZ_ROAD, ZSOILHCAPZ_ROAD    )
   !
   CALL UPDATE_THERMALPROP(HM%NTH%AL(KTEB_P)%XWG_BLD,HM%NTH%AL(KTEB_P)%XWGI_BLD,             &
                               T%XHCAPSOIL_BLD, T%XCONDDRY_BLD,    &
                               T%XCONDSLD_BLD, T%XWSAT_BLD,        &
                               ZSOILCONDZ_BLD, ZSOILHCAPZ_BLD      )
!
   T%XHC_ROAD  (:,TOP%NTEB_ROAD+1:TOP%NTEB_SOIL) = ZSOILHCAPZ_ROAD(:,TOP%NTEB_ROAD+1:TOP%NTEB_SOIL)
   T%XTC_ROAD  (:,TOP%NTEB_ROAD+1:TOP%NTEB_SOIL) = ZSOILCONDZ_ROAD(:,TOP%NTEB_ROAD+1:TOP%NTEB_SOIL)
   T%XHC_BLD   (:,TOP%NTEB_ROAD+1:TOP%NTEB_SOIL) = ZSOILHCAPZ_BLD (:,TOP%NTEB_ROAD+1:TOP%NTEB_SOIL)
   T%XTC_BLD   (:,TOP%NTEB_ROAD+1:TOP%NTEB_SOIL) = ZSOILCONDZ_BLD (:,TOP%NTEB_ROAD+1:TOP%NTEB_SOIL)
   !  
   ! Sensible heat stored in the road [J/m²(urb)]
   ! Recomputation is necessary because of change of heat capacity due to water: internal energy of water is not integrated in
   ! energy budget in TEB and ISBA.
   IF (CT%LCHECK_TEB) THEN
     WHERE (T%XENETOTAL/=XUNDEF) T%XENETOTAL(:) = T%XENETOTAL(:) - T%XTHEROAD(:) - T%XTHESOILBLD(:)
     WHERE (T%XTHETOTAL/=XUNDEF) T%XTHETOTAL(:) = T%XTHETOTAL(:) - T%XTHEROAD(:) - T%XTHESOILBLD(:)
     T%XTHEROAD(:)=0.0
     DO JLAYER=1,SIZE(T%XT_ROAD,2)
       T%XTHEROAD(:) = T%XTHEROAD(:) +  T%XROAD(:)   * &
              T%XD_ROAD(:,JLAYER)*T%XHC_ROAD(:,JLAYER) * &
              T%XT_ROAD(:,JLAYER)
     ENDDO
     T%XTHESOILBLD(:)=0.0
     DO JLAYER=1,SIZE(T%XT_BLD,2)
       T%XTHESOILBLD(:) = T%XTHESOILBLD(:) + &
               T%XBLD(:) * T%XD_BLD(:,JLAYER) *         &
               T%XHC_BLD(:,JLAYER)*T%XT_BLD(:,JLAYER)
     ENDDO
    WHERE (T%XENETOTAL/=XUNDEF) T%XENETOTAL(:) = T%XENETOTAL(:) + T%XTHEROAD(:) + T%XTHESOILBLD(:)
    WHERE (T%XTHETOTAL/=XUNDEF) T%XTHETOTAL(:) = T%XTHETOTAL(:) + T%XTHEROAD(:) + T%XTHESOILBLD(:)
  END IF

ELSE
   !
   DMT%XRUNOFFSOIL_ROAD (:) = 0.
   DMT%XRUNOFFSOIL_BLD  (:) = 0.
   DMT%XDRAIN_ROAD      (:) = 0.
   DMT%XDRAIN_BLD       (:) = 0.
   IF (CT%LCHECK_TEB)  CT%XLAT_SOIL_TO_GARDEN(:)= 0.
!
ENDIF
!
!*     10.     Treatment of built covers
!              -------------------------
!
  CALL TEB_BLD_ROAD  (TOP, T, BOP, B, TIR, DMT, CT, LW, HPROGRAM, HIMPLICIT_WIND,     &
         PTSUN, ZTA_RF, ZQA_RF, PT_CAN, PQ_CAN, PU_CAN, PT_LOWCAN, PQ_LOWCAN, PU_LOWCAN, &
         PZ_LOWCAN, PPEW_A_COEF, PPEW_B_COEF, PPEW_A_COEF_LOWCAN,              &
         PPEW_B_COEF_LOWCAN, AT, PPS, PPSOLD, PPA, PEXNS, PEXNA, PTA, PQA, PRHOA, PLW_RAD, &
         PRR, PSR, PZREF, PUREF, PVMOD, PH_TRAFFIC, PLE_TRAFFIC, PTSTEP,       &
         ZDF_RF, PDN_RF, ZDF_RD, PDN_RD, ZQSAT_RF, ZQSAT_RD,                   &
         ZWS_ROOF_MAX, ZWS_ROAD_MAX, DMT%XDW_ROOF,                             &
         DMT%XDW_ROAD, PTSRAD_GD, PTS_HVEG, PLEW_RF, ZUW_GR, PLEW_RD,          &
         PRNSN_RF, PHSN_RF, PLESN_RF, PGSN_RF, PMELT_RF,                       &
         ZRN_GR, ZH_GR, ZLE_GR, ZGFLUX_GR, ZDRAIN_GR, ZRUNOFF_GR,              &
         PRNSN_RD, PHSN_RD, PLESN_RD, PGSN_RD, PMELT_RD, ZUW_RD, PUW_RF,       &
         ZDUWDU_RD, PDUWDU_RF, PUSTAR_TWN, PCD, PCDN, PCH_TWN, PRI_TWN, PRESA_TWN, &
         ZCST_H_WASTE_CANY, ZCST_LE_WASTE_CANY,                                &
         ZCOE_H_WASTE_CANY,ZCOE_LE_WASTE_CANY,                                 &
         ZMUL_H_WASTE_CANY, ZMUL_LE_WASTE_CANY,                                &
         ZAC_RF, PAC_RD, ZAC_WL, ZAC_TOP, PAC_GD, ZAC_RF_WAT, PAC_RD_WAT,      &
         KDAY, DMT%XEMIT_LW_FAC, ZEMIT_LW_RD,                                  &
         PTIME, ZQINMOD, ZTHEAT_TARGET, ZTCOOL_TARGET, ZVENT_BEHAV_ANYWAY,     &
         ZVENT_BEHAV_ADAPTI, ZE_SHADING, "OK"                                  )
!
!-------------------------------------------------------------------------------
!
!*    10.      Treatment of solar panels
!              -------------------------
!
IF (TOP%LSOLAR_PANEL) THEN
  ! 
  !* LW radiation coming upwards from roofs
  !
  ZEMIT_LW_RF =  ZREC_LW_RF   &
            - (   T%XGREENROOF(:)  *             DMT%XABS_LW_GREENROOF(:)  &
            + (1.-T%XGREENROOF(:)) * ZDF_RF(:) * DMT%XABS_LW_ROOF(:)       &
            + (1.-T%XGREENROOF(:)) * PDN_RF(:) * DMT%XABS_LW_SNOW_ROOF(:) )
  !
  ! note that, for the time being, one considers that the solar panel 
  ! intercept radiation both above roof and greenroofs (if any)
  !
  CALL SOLAR_PANEL(TPN, DMT, HPROGRAM, PTSTEP, PTSUN, B%XRESIDENTIAL, ZEMIT_LW_RF, ZEMIT_LWDN_PANEL, &
            PLW_RAD, PTA, B%XN_FLOOR   )
ELSE
  DMT%XABS_LW_PANEL    = XUNDEF
  DMT%XTHER_PROD_PANEL = XUNDEF
  DMT%XPHOT_PROD_PANEL = XUNDEF
  DMT%XPROD_PANEL      = XUNDEF
  DMT%XTHER_PROD_BLD   = XUNDEF
  DMT%XPHOT_PROD_BLD   = XUNDEF
  DMT%XH_PANEL         = XUNDEF
  DMT%XRN_PANEL        = XUNDEF
END IF
!-------------------------------------------------------------------------------
 CALL DEALLOC_LW_COEF(LW)
!-------------------------------------------------------------------------------
!
!*     11.     Aggregation
!              -----------
!
 CALL AVG_URBAN_FLUXES(TOP, T, B, TPN, DMT,GDD, HPROGRAM, PTS_TWN, PEMIS_TWN, PT_CAN, PQ_CAN, &
      PT_LOWCAN, PQ_LOWCAN, ZTA_RF, ZQA_RF, PRHOA, PPS, PH_TRAFFIC, PLE_TRAFFIC, ZWL_O_GRND,  &
      ZESN_RF, ZEMIS_GR, PLW_RAD,  ZAC_RF, ZAC_RF_WAT, ZAC_WL, PAC_RD, PAC_RD_WAT, ZAC_TOP,   &
      PAC_GD, ZQSAT_GD, DMT%XAC_AGG_GD, ZHU_AGG_GD, DMT%XTS_HVEG, ZQSAT_HVEG, ZAC_HVEG, ZHU_HVEG, & 
      ZQSAT_RF, ZQSAT_RD, DMT%XDW_ROOF,                                                       &
      DMT%XDW_ROAD, ZRF_FRAC, ZWL_FRAC, ZRD_FRAC, ZGD_FRAC, ZHVEG_FRAC,                       &
      ZDF_RF, PDN_RF, ZDF_RD, PDN_RD, PLEW_RF, PLESN_RF, PLEW_RD,                             &
      PLESN_RD, PHSN_RD, PTSRAD_GD, ZEVAP_GD, ZRUNOFF_GD,                                     &
      ZEVAP_GR, ZRUNOFF_GR, ZDRAIN_GR, PRN_GRND, PH_GRND, PLE_GRND, PGFLX_GRND, PRN_TWN,      &
      PH_TWN, PH_TWN_SURF, PH_TWN_WALL, PH_TWN_ROOF, PLE_TWN, PGFLX_TWN, PQF_TWN, PEVAP_TWN,  &
      PEVAP_TWN_SURF, PEVAP_TWN_WALL, PEVAP_TWN_ROOF, ZEMIT_LW_RD,ZEMIT_LW_GD,                &
      DMT%XEMIT_LW_GRND, ZEMIS_GD, ZCST_H_WASTE_CANY, ZCST_LE_WASTE_CANY, ZCOE_H_WASTE_CANY,  &
      ZCOE_LE_WASTE_CANY, ZMUL_H_WASTE_CANY, ZMUL_LE_WASTE_CANY                               )
!
! CO2 fluxes not directly related to the buildings
!
DMT%XSFCO2_VEG(:) = T%XGARDEN(:) * ZSFCO2_GD(:) + T%XBLD(:) * T%XGREENROOF(:) * ZSFCO2_GR(:) + T%XFRAC_HVEG(:) * ZSFCO2_HV(:)
DMT%XSFCO2_RD(:)  = T%XSFCO2_RD(:) * PTRAF_MODULATION(:) / PRHOA(:) !conversion from (kg/m2 of town/s) to  (m/s*kg_CO2/kg_air)
DMT%XSFCO2_POP(:) = T%XNB_POP(:) * PPOP_MODULATION(:) * 1.E-6 * XHUM_CO2 / PRHOA(:)      !(1/km2 of town)*1.E-6*(Kg/s)/(Kg/m3) =>  (m/s*kg_CO2/kg_air)
!
! CO2 flux due to the buildings
!
IF (TOP%CBEM=='BEM') THEN
   !
   DO JCOMP=1,SIZE(DMT%XCOMP_HOTWAT,2)
       DMT%XCOMP_HOTWAT(:,JCOMP) = T%XBLD(:) * DMT%XCOMP_HOTWAT(:,JCOMP)
   ENDDO
   !
   DMT%XHOTWAT_GAS(:)  = T%XBLD(:) * DMT%XHOTWAT_GAS(:)
   DMT%XHOTWAT_ELEC(:) = T%XBLD(:) * DMT%XHOTWAT_ELEC(:)
   !
   DMT%XSFCO2_BLD(:)   = T%XBLD(:) * DMT%XSFCO2_BLD(:)
   !
   DMT%XHOTWATOUT(:) = 0.0
   DO JCOMP=1,SIZE(DMT%XCOMP_HOTWAT,2)
      DMT%XHOTWATOUT(:) = DMT%XHOTWATOUT(:) + B%XFRACOMP(:,JCOMP) * DMT%XCOMP_HOTWAT(:,JCOMP)
   ENDDO
   !
   ! Total CO2 flux
   !
   DMT%XSFCO2(:) = DMT%XSFCO2_VEG(:) + DMT%XSFCO2_BLD(:) + DMT%XSFCO2_RD(:) + DMT%XSFCO2_POP(:)
   !
ELSE
   !
   ! Total CO2 flux does not contain building related flux
   !
   DMT%XSFCO2(:) = DMT%XSFCO2_VEG(:) + DMT%XSFCO2_RD(:) + DMT%XSFCO2_POP(:)
   !
ENDIF
!
!-------------------------------------------------------------------------------
!
!*     12.     Momentum flux for ground built surfaces
!              ---------------------------------------
!
PUW_GRND (:)     = (T%XROAD(:)*ZUW_RD(:) + T%XGARDEN(:)*ZUW_GD(:)) / (T%XROAD(:)+T%XGARDEN(:))
!
PDUWDU_GRND (:)  = 0.
!
!-------------------------------------------------------------------------------
!
!*     13.     For UTCI
!              -----------
!
!
PSCA_SW_SKY  (:) = ZSCA_SW (:)
PLW_RAD_SKY  (:) = PLW_RAD (:)
PEMIT_LW_HVEG(:) = 0.
!
IF (TOP%CURBTREE == 'TREE' .OR. TOP%CURBTREE == 'GRWL') THEN
   !
   DO JI=1,SIZE(PREF_SW_FAC)      !
      !
      PEMIT_LW_HVEG   (JI) = (ZEMIS_HVEG(JI)*XSTEFAN*GDM%NPEHV%AL(KTEB_P)%XTV(JI)**4 + &
                           (1.-ZEMIS_HVEG(JI))*                                        &
                           (   T%XSVF_TR(JI) *DMT%XEMIT_LW_GRND(JI) +                  &
                           (1.-T%XSVF_TR(JI))*DMT%XEMIT_LW_FAC(JI)))
   ENDDO
   !
ENDIF
!
END SUBROUTINE TOWN_ENERGY_BALANCE3
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE TOWN_ENERGY_BALANCE
