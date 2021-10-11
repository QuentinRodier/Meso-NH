!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   ##########################################################################
    SUBROUTINE TEB_BLD_ROAD (TOP, T, BOP, B, TIR, DMT, CT, LW, HPROGRAM, HIMPLICIT_WIND, PTSUN,    &
                     PTA_RF, PQA_RF, PT_CANYON, PQ_CANYON, PU_CANYON, PT_LOWCAN, PQ_LOWCAN,&
                     PU_LOWCAN, PZ_LOWCAN, PPEW_A_COEF, PPEW_B_COEF, PPEW_A_COEF_LOWCAN,   &
                     PPEW_B_COEF_LOWCAN, AT, PPS, PPSOLD, PPA, PEXNS, PEXNA, PTA, PQA, PRHOA,  &
                     PLW_RAD, PRR, PSR, PZREF, PUREF, PVMOD, PH_TRAFFIC, PLE_TRAFFIC,      &
                     PTSTEP, PDF_RF, PDN_RF, PDF_RD, PDN_RD, PQSAT_RF, PQSAT_RD,           &
                     PWS_RF_MAX, PWS_RD_MAX,                                               &
                     PDELT_RF, PDELT_RD, PTS_GARDEN, PTS_HVEG, PLEW_RF, PUW_GR, PLEW_RD,   &
                     PRNSN_RF, PHSN_RF, PLESN_RF, PGSN_RF, PMELT_RF, PRN_GR,               &
                     PH_GR, PLE_GR, PGFLUX_GR, PDRAIN_GR, PRUNOFF_GR, PRNSN_RD,            &
                     PHSN_RD, PLESN_RD, PGSN_RD, PMELT_RD, PUW_RD, PUW_RF, PDUWDU_RD,      &
                     PDUWDU_RF, PUSTAR_TWN, PCD, PCDN, PCH_TWN, PRI_TWN, PRESA_TWN,        &
                     PCST_H_WASTE_CANY, PCST_LE_WASTE_CANY,                                &
                     PCOE_H_WASTE_CANY,PCOE_LE_WASTE_CANY,                                 &
                     PMUL_H_WASTE_CANY, PMUL_LE_WASTE_CANY,                                &
                     PAC_RF, PAC_RD, PAC_WL, PAC_TOP, PAC_GARDEN, PAC_RF_WAT, PAC_RD_WAT,  &
                     KDAY, PEMIT_LW_FAC,                                                   &
                     PEMIT_LW_RD, PTIME, PQINMOD, PTHEAT_TARGET, PTCOOL_TARGET,            &
                     PVENT_BEHAV_ANYWAY, PVENT_BEHAV_ADAPTI, PE_SHADING, HTEST             )
!   ##########################################################################
!
!!****  *TEB_BLD_ROAD*  historical TEB routine  
!!
!!    PURPOSE
!!    -------
!
!     Computes the evoultion of prognostic variables and the fluxes
!     over artificial surfaces as towns, taking into account the canyon like
!     geometry of urbanized areas.
!         
!     
!!**  METHOD
!     ------
!
!     The prognostic variables are:
!       - the surface temperature for roofs, roads, and walls
!       - the water reservoir, whose maximum value is 10mm
!
!
!    1 : Warning about snow
!        ******************
!
!     Except for snow mantel evolution, all other computation with snow
!   variables must be performed with these variables at previous time-step,
!   and NOT new time-step. This insure coherence between snow fractions
!   (computed at the begining) and other snow characteristics (albedo, Ts).
!
!
!    2 : computation of input solar radiation on each surface
!        ****************************************************
!
!      Those are now done in subroutine urban_solar_abs.F90
!
!    3 : drag coefficient for momentum 
!        *****************************
!
!
!    4 : aerodynamical resistance for heat transfers
!        *******************************************
!
!
!    5 : equation for evolution of Ts_roof
!        *********************************
!
!
!       Rn = (dir_Rg + sca_Rg) (1-a) + emis * ( Rat - sigma Ts**4 (t+dt) )
!
!       H  = rho Cp CH V ( Ts (t+dt) - Tas )
!
!       LE = rho Lv CH V ( qs (t+dt) - qas )
!
!      where the as subscript denotes atmospheric values at ground level
!      (and not at first half level)
!
!
!    6 : equations for evolution of Ts_road and Ts_wall simultaneously
!        *************************************************************
!
!
!
!   Rn_w = abs_Rg_w 
!  - sigma * emis_w                                                   * Ts_w**4 (t+dt)
!  +         emis_w                       *      SVF_w                * Rat
!  + sigma * emis_w * emis_r              *      SVF_w                * Ts_r**4 (t+dt)
!  + sigma * emis_w * emis_w              * (1-2*SVF_w)               * Ts_w**4 (t+dt)
!  + sigma * emis_w * emis_w * (1-emis_r) *      SVF_w  * (1-  SVF_r) * Ts_w**4 (t+dt)
!  + sigma * emis_w * emis_w * (1-emis_w) * (1-2*SVF_w) * (1-2*SVF_w) * Ts_w**4 (t+dt)
!  + sigma * emis_w * emis_r * (1-emis_w) *      SVF_w  * (1-2*SVF_w) * Ts_r**4 (t+dt)
!
!   Rn_r = abs_Rg_r
!  - sigma * emis_r                                                   * Ts_r**4 (t+dt)
!  +         emis_r                       *    SVF_r                  * Rat
!  + sigma * emis_r * emis_w              * (1-SVF_r)                 * Ts_w**4 (t+dt)
!  + sigma * emis_r * emis_w * (1-emis_w) * (1-SVF_r)   * (1-2*SVF_w) * Ts_w**4 (t+dt)
!  + sigma * emis_r * emis_r * (1-emis_w) * (1-SVF_r)   *      SVF_w  * Ts_r**4 (t+dt)
!
!  H_w  = rho Cp CH V ( Ts_w (t+dt) - Ta_canyon )
!
!  LE_w = rho Lv CH V ( qs_w (t+dt) - qa_canyon )
!
!  H_r  = rho Cp CH V ( Ts_r (t+dt) - Ta_canyon )
!
!  LE_r = rho Lv CH V ( qs_r (t+dt) - qa_canyon )
!
! with again
!                AC_can * Swall/Sroad * Twall + AC_can * Troad + AC_top * Ta + H_traffic/Cp/rho/Sroad
!   Ta_canyon = -------------------------------------------------------------------------------------
!                AC_can * Swall/Sroad         + AC_can         + AC_top
!
!
!                 AC_can * delt_road * Hu_road * qsat(Troad) + AC_top * qa + LE_traffic/Lv/rho/Sroad
!   qa_canyon = ------------------------------------------------------------------------------------
!                 AC_can * delt_road                        + AC_top
!
!
!
!
!    7 : computation of fluxes for each surface type
!        *******************************************
!
!
!    8 : averaging of the fluxes
!        ***********************
!
!   This is done on the total exchange surface (roof + wall + road),
!  which is bigger than the horizontal surface (roof+road), leading
!  to bigger fluxes.
!
!   The fluxes due to industrial activity are directly added into the 
!  atmosphere
!
!
!    9 : road reservoir evolution
!        ************************
!
!   The roof reservoir runoff goes directly into the road reservoir.
!
!   Runoff occurs for road reservoir (too much water), as well as drainage
!   (evacuation system, typical time scale: 1 day)
!
!    20 : Compute CO2 fluxes link to buildings
!         ************************************
!         Fluxes from heating, cooling, QIN, and hot water
!
!
!------------------------
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    MODD_CST
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/01/98 
!!     21 / 10 / 2003   P. Tulet    output aerodynamical resistance
!!     01 / 07 / 2005   P.Le Moigne Exner functions as arguments to urban_fluxes
!!     17 / 10 / 2005   (G. Pigeon) computation of anthropogenic heat from domestic heating
!!          01 / 2012   V. Masson   Separates the 2 walls 
!!     25 / 09 / 2012   B. Decharme new wind implicitation
!!          07 / 2013   V. Masson   Adds road watering
!!          01 / 2016   E.Redon/A.Lemonsu   Add high vegetation
!!          01 / 2016   K.Chancibault/A.Lemonsu   Add urban hydrology
!!          12 / 2016   M. Goret    Add CO2 fluxes
!!          03 / 2017   M. Goret Add Demuzere et al 2017 Surface Interception Distribution approach
!!          04 / 2017   M. Goret Change EFF_HEAT, from dummya arg. to local variable
!!          08 / 2017   M. Goret add anthropogenic flux diagnostics
!!          09 / 2017   M. Goret add diagnostic of heat storage link to snow
!!          09 / 2017   M. Goret add LE waste due to heating
!!          09 / 2017   M. Goret add hot water 
!!      V. Masson   04.2020 completes energy check for high vegetation IR exchanges
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!     ------------
!
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_TEB_IRRIG_n, ONLY : TEB_IRRIG_t
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
USE MODD_CHECK_TEB, ONLY : CHECK_TEB_t
USE MODD_LW_COEF, ONLY : LW_COEF_t
USE MODD_SURF_ATM_TURB_n,   ONLY : SURF_ATM_TURB_t
!
USE MODD_TYPE_DATE_SURF,ONLY: DATE_TIME
USE MODD_CSTS,         ONLY : XTT, XSTEFAN, XCPD, XLVTT, XLMTT
USE MODD_SURF_PAR,     ONLY : XUNDEF, XSURF_EPSILON
USE MODD_SNOW_PAR,     ONLY : XANSMAX_ROOF, XANSMAX_ROAD, XWCRN_ROOF, XWCRN_ROAD
!
USE MODE_THERMOS
USE MODE_SURF_SNOW_FRAC
!
USE MODI_SNOW_COVER_1LAYER
USE MODI_URBAN_DRAG
USE MODI_URBAN_SNOW_EVOL
USE MODI_ROOF_LAYER_E_BUDGET
USE MODI_ROAD_LAYER_E_BUDGET
USE MODI_FACADE_E_BUDGET
USE MODI_URBAN_FLUXES
USE MODI_BLD_E_BUDGET
USE MODI_WIND_THRESHOLD
USE MODI_BEM
USE MODI_BLDSOIL_LAYER_E_BUDGET
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    Declarations of arguments
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(TEB_IRRIG_t), INTENT(INOUT) :: TIR
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DMT
TYPE(CHECK_TEB_t), INTENT(INOUT) :: CT
TYPE(LW_COEF_t), INTENT(INOUT) :: LW
TYPE(SURF_ATM_TURB_t), INTENT(IN) :: AT            ! atmospheric turbulence parameters
!
CHARACTER(LEN=2), INTENT(IN) :: HTEST         ! must be equal to 'OK'  
CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM      ! program calling surf. schemes
CHARACTER(LEN=*), INTENT(IN) :: HIMPLICIT_WIND ! wind implicitation option
!                                              ! 'OLD' = direct
!                                              ! 'NEW' = Taylor serie, order 1
REAL, DIMENSION(:),   INTENT(IN)    :: PTSUN              ! solar time   (s from midnight)
REAL, DIMENSION(:), INTENT(IN)    :: PTA_RF        ! near roof air temperature
REAL, DIMENSION(:), INTENT(IN)    :: PQA_RF        ! near air air specific humidity
REAL, DIMENSION(:), INTENT(INOUT) :: PT_CANYON     ! canyon air temperature
REAL, DIMENSION(:), INTENT(INOUT) :: PQ_CANYON     ! canyon air specific humidity
REAL, DIMENSION(:), INTENT(IN)    :: PU_CANYON     ! canyon hor. wind
REAL, DIMENSION(:), INTENT(IN)    :: PU_LOWCAN     ! wind near the road
REAL, DIMENSION(:), INTENT(IN)    :: PT_LOWCAN     ! temp. near the road
REAL, DIMENSION(:), INTENT(IN)    :: PQ_LOWCAN     ! hum. near the road
REAL, DIMENSION(:), INTENT(IN)    :: PZ_LOWCAN     ! height of atm. var. near the road
REAL, DIMENSION(:), INTENT(IN)    :: PPEW_A_COEF   ! implicit coefficients
REAL, DIMENSION(:), INTENT(IN)    :: PPEW_B_COEF   ! for wind coupling
REAL, DIMENSION(:), INTENT(IN)    :: PPEW_A_COEF_LOWCAN ! implicit coefficients for wind coupling
REAL, DIMENSION(:), INTENT(IN)    :: PPEW_B_COEF_LOWCAN ! between low canyon wind and road
REAL, DIMENSION(:), INTENT(IN)    :: PPS           ! pressure at the surface
REAL, DIMENSION(:), INTENT(IN)    :: PPSOLD        ! pressure at the surface at previous time step
REAL, DIMENSION(:), INTENT(IN)    :: PPA           ! pressure at the first atmospheric level
REAL, DIMENSION(:), INTENT(IN)    :: PEXNS         ! surface exner function
REAL, DIMENSION(:), INTENT(IN)    :: PTA           ! temperature at the lowest level
REAL, DIMENSION(:), INTENT(IN)    :: PQA           ! specific humidity
                                                   ! at the lowest level
REAL, DIMENSION(:), INTENT(IN)    :: PVMOD         ! module of the horizontal wind
REAL, DIMENSION(:), INTENT(IN)    :: PEXNA         ! exner function
                                                   ! at the lowest level
REAL, DIMENSION(:), INTENT(IN)    :: PRHOA         ! air density
                                                   ! at the lowest level
REAL, DIMENSION(:), INTENT(IN)    :: PLW_RAD       ! atmospheric infrared radiation
REAL, DIMENSION(:), INTENT(IN)    :: PRR           ! rain rate
REAL, DIMENSION(:), INTENT(IN)    :: PSR           ! snow rate
REAL, DIMENSION(:), INTENT(IN)    :: PH_TRAFFIC    ! anthropogenic sensible
!                                                  ! heat fluxes due to traffic
REAL, DIMENSION(:), INTENT(IN)    :: PLE_TRAFFIC   ! anthropogenic latent
!                                                  ! heat fluxes due to traffic
REAL, DIMENSION(:), INTENT(IN)    :: PZREF         ! reference height of the first
                                                   ! atmospheric level (temperature)
REAL, DIMENSION(:), INTENT(IN)    :: PUREF         ! reference height of the first
                                                   ! atmospheric level (wind)
REAL,               INTENT(IN)    :: PTSTEP        ! time step
!
REAL, DIMENSION(:), INTENT(IN)    :: PWS_RF_MAX, PWS_RD_MAX !
!
REAL, DIMENSION(:,:), INTENT(IN) :: PQINMOD
REAL, DIMENSION(:,:), INTENT(IN) :: PTHEAT_TARGET
REAL, DIMENSION(:,:), INTENT(IN) :: PTCOOL_TARGET
REAL, DIMENSION(:,:), INTENT(IN) :: PVENT_BEHAV_ANYWAY
REAL, DIMENSION(:,:), INTENT(IN) :: PVENT_BEHAV_ADAPTI
!
REAL, DIMENSION(:), INTENT(INOUT) :: PDF_RF      ! snow-free    fraction on roofs
REAL, DIMENSION(:), INTENT(INOUT) :: PDN_RF      ! snow-covered fraction on roofs
REAL, DIMENSION(:), INTENT(INOUT) :: PDF_RD      ! snow-free    fraction on roads
REAL, DIMENSION(:), INTENT(INOUT) :: PDN_RD      ! snow-covered fraction on roads
REAL, DIMENSION(:), INTENT(OUT)   :: PQSAT_RF    ! hum at saturation over roof
REAL, DIMENSION(:), INTENT(OUT)   :: PQSAT_RD    ! hum at saturation over road
REAL, DIMENSION(:), INTENT(OUT)   :: PDELT_RF    ! water fraction on roof
REAL, DIMENSION(:), INTENT(OUT)   :: PDELT_RD    ! water fraction on road
!
REAL, DIMENSION(:), INTENT(IN)    :: PTS_GARDEN    ! GARDEN area surf temp.
REAL, DIMENSION(:), INTENT(IN)    :: PTS_HVEG      ! high veg surf temp.
!
REAL, DIMENSION(:), INTENT(OUT)   :: PLEW_RF    ! latent heat flux over roof (snow)
REAL, DIMENSION(:), INTENT(OUT)   :: PLEW_RD    ! latent heat flux over road (snow)

!
REAL, DIMENSION(:), INTENT(OUT)   :: PCST_H_WASTE_CANY     ! sensible waste heat released to canyon
REAL, DIMENSION(:), INTENT(OUT)   :: PCST_LE_WASTE_CANY    ! latent waste heat released to canyon
REAL, DIMENSION(:), INTENT(OUT)   :: PCOE_H_WASTE_CANY
REAL, DIMENSION(:), INTENT(OUT)   :: PCOE_LE_WASTE_CANY
REAL, DIMENSION(:), INTENT(OUT)   :: PMUL_H_WASTE_CANY
REAL, DIMENSION(:), INTENT(OUT)   :: PMUL_LE_WASTE_CANY
!
REAL, DIMENSION(:), INTENT(IN)    :: PRN_GR     ! net radiation over greenroof
REAL, DIMENSION(:), INTENT(IN)    :: PH_GR      ! sensible heat flux over greenroof
REAL, DIMENSION(:), INTENT(IN)    :: PLE_GR     ! latent heat flux over greenroof
REAL, DIMENSION(:), INTENT(IN)    :: PGFLUX_GR  ! flux through the greenroof
REAL, DIMENSION(:), INTENT(IN)    :: PUW_GR     ! Momentum flux for greenroofs
REAL, DIMENSION(:), INTENT(IN)    :: PRUNOFF_GR ! runoff over green roofs
REAL, DIMENSION(:), INTENT(IN)    :: PDRAIN_GR  ! outlet drainage at base of green roofs
!
REAL, DIMENSION(:), INTENT(OUT)   :: PRNSN_RF ! net radiation over snow
REAL, DIMENSION(:), INTENT(OUT)   :: PHSN_RF  ! sensible heat flux over snow
REAL, DIMENSION(:), INTENT(OUT)   :: PLESN_RF ! latent heat flux over snow
REAL, DIMENSION(:), INTENT(OUT)   :: PGSN_RF  ! flux under the snow
REAL, DIMENSION(:), INTENT(OUT)   :: PMELT_RF   ! snow melt
REAL, DIMENSION(:), INTENT(OUT)   :: PRNSN_RD ! net radiation over snow
REAL, DIMENSION(:), INTENT(OUT)   :: PHSN_RD  ! sensible heat flux over snow
REAL, DIMENSION(:), INTENT(OUT)   :: PLESN_RD ! latent heat flux over snow
REAL, DIMENSION(:), INTENT(OUT)   :: PGSN_RD  ! flux under the snow
REAL, DIMENSION(:), INTENT(OUT)   :: PMELT_RD   ! snow melt
!
REAL, DIMENSION(:), INTENT(OUT)   :: PUW_RD     ! Momentum flux for roads
REAL, DIMENSION(:), INTENT(OUT)   :: PUW_RF     ! Momentum flux for roofs
REAL, DIMENSION(:), INTENT(OUT)   :: PDUWDU_RD  !
REAL, DIMENSION(:), INTENT(OUT)   :: PDUWDU_RF  !
REAL, DIMENSION(:), INTENT(OUT)   :: PUSTAR_TWN ! friciton velocity over town
REAL, DIMENSION(:), INTENT(OUT)   :: PCD          ! town averaged drag coefficient
REAL, DIMENSION(:), INTENT(OUT)   :: PCDN         ! town averaged neutral drag coefficient
REAL, DIMENSION(:), INTENT(OUT)   :: PCH_TWN     ! town averaged heat transfer
!                                                 ! coefficient
REAL, DIMENSION(:), INTENT(OUT)   :: PRI_TWN      ! town averaged Richardson number
REAL, DIMENSION(:), INTENT(OUT)   :: PRESA_TWN    ! town aerodynamical resistance
REAL, DIMENSION(:), INTENT(OUT)   :: PAC_RF      ! roof conductance
REAL, DIMENSION(:), INTENT(INOUT) :: PAC_RD      ! road conductance
REAL, DIMENSION(:), INTENT(OUT)   :: PAC_WL      ! wall conductance
REAL, DIMENSION(:), INTENT(OUT)   :: PAC_TOP       ! top conductance
REAL, DIMENSION(:), INTENT(IN)    :: PAC_GARDEN    ! garden conductance
REAL, DIMENSION(:), INTENT(OUT)   :: PAC_RF_WAT  ! roof water conductance
REAL, DIMENSION(:), INTENT(OUT)   :: PAC_RD_WAT  ! roof water conductance
!
! new arguments after BEM
!
INTEGER,            INTENT(IN)     :: KDAY         ! Simulation day
 !new argument for PET calculation
REAL, DIMENSION(:), INTENT(OUT) :: PEMIT_LW_RD ! LW fluxes emitted by road (W/m2 surf road)
REAL, DIMENSION(:), INTENT(OUT) :: PEMIT_LW_FAC  ! LW fluxes emitted by wall (W/m2 surf wall)
REAL,                INTENT(IN)  :: PTIME        ! current time since midnight (UTC, s)
REAL, DIMENSION(:), INTENT(IN)  :: PE_SHADING    !energy not ref., nor absorbed, nor
                                                 !trans. by glazing [Wm-2(win)]
!
! *      0.2    Declarations of local variables
!
REAL, DIMENSION(SIZE(PTA)) :: ZVMOD          ! wind
!
REAL, DIMENSION(SIZE(PTA)) :: ZAC_BLD        ! surface conductance inside the building itself in DEF building model
!
REAL, DIMENSION(SIZE(PTA)) :: ZFLX_BLD_RF    ! Heat exchange between indoor air and roof (W/m²(roof))
REAL, DIMENSION(SIZE(PTA)) :: ZFLX_BLD_WL_A  ! Heat exchange between indoor air and wall A (W/m²(wall)) 
REAL, DIMENSION(SIZE(PTA)) :: ZFLX_BLD_WL_B  ! Heat exchange between indoor air and wall B (W/m²(wall)) 
REAL, DIMENSION(SIZE(PTA)) :: ZDQS_RD      ! heat storage inside road
REAL, DIMENSION(SIZE(PTA)) :: ZDQS_RF      ! heat storage inside roof
REAL, DIMENSION(SIZE(PTA)) :: ZDQS_WL_A    ! heat storage inside wall
REAL, DIMENSION(SIZE(PTA)) :: ZDQS_WL_B    ! heat storage inside wall
REAL, DIMENSION(SIZE(PTA)) :: ZFLX_BLD_FL !heat flux from inside through floor
REAL, DIMENSION(SIZE(PTA)) :: ZFLX_BLD_MA  !heat flux from inside through mass
REAL, DIMENSION(SIZE(PTA)) :: ZAGG_TR_SW_WIN
!
!REAL, DIMENSION(SIZE(PTA)) :: ZMELT_BLT      ! Snow melt for built & impervious part
REAL, DIMENSION(SIZE(PTA)) :: ZEFF_HEAT      ! mean heating system efficiency
REAL, DIMENSION(SIZE(PTA)) :: ZFRAC_HEAT_LE  ! LE waste due to heating per heating energy demand 
!
REAL, DIMENSION(SIZE(PTA)) :: ZG_FLOOR       ! heat flux between building and soil below
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_G_FLOOR  ! heat flux between building compartiment and soil below
!
! coefficients for LW computations over snow (from previous time-step)
!
REAL, DIMENSION(SIZE(PTA)) :: ZTSSN_RD ! road snow temperature
!                                          ! at previous time-step
! new local variables after BEM
!
REAL, DIMENSION(SIZE(PTA)) :: ZTS_RD       ! road surface temperature 
!                                            ! at previous time-step
REAL, DIMENSION(SIZE(PTA)) :: ZTS_WL_A     ! wall A surface temperature 
!                                            ! at previous time-step
REAL, DIMENSION(SIZE(PTA)) :: ZTS_WL_B     ! wall B surface temperature 
!                                            ! at previous time-step
REAL, DIMENSION(SIZE(PTA)) :: ZTS_WL       ! averaged wall surface temperature 
!                                            ! at previous time-step
REAL, DIMENSION(SIZE(PTA)) :: ZTS_RF       ! roof surface temperature 
!                                            ! at previous time-step
REAL, DIMENSION(SIZE(PTA),SIZE(T%XT_WALL_A,2)) :: ZT_WL ! averaged wall surface temperature 
!
INTEGER :: IWL, IRF                      ! number of wall, roof layer
REAL, DIMENSION(SIZE(PTA),SIZE(B%XTI_BLD,2)) :: ZRADHT_IN     ! Indoor radiant heat transfer coefficient
                                                    ! [W K-1 m-2]
REAL, DIMENSION(SIZE(PTA),SIZE(B%XTI_BLD,2)) :: ZTS_FL       ! floor surface temperature [K]
REAL, DIMENSION(SIZE(PTA),SIZE(B%XTI_BLD,2)) :: ZTS_MA       ! mass surface temperature [K]
REAL, DIMENSION(SIZE(PTA)) :: ZRAD_RF_WL  ! rad. flux from roof to averaged wall [W m-2(roof)]
REAL, DIMENSION(SIZE(PTA)) :: ZRAD_RF_WIN   ! rad. flux from roof to window [W m-2(roof)]
REAL, DIMENSION(SIZE(PTA),SIZE(B%XTI_BLD,2)) :: ZRAD_RF_FL ! rad. flux from roof to floor [W m-2(roof)]
REAL, DIMENSION(SIZE(PTA),SIZE(B%XTI_BLD,2)) :: ZRAD_RF_MA  ! rad. flux from roof to mass [W m-2(roof)]
REAL, DIMENSION(SIZE(PTA),SIZE(B%XTI_BLD,2)) :: ZCONV_RF_BLD  ! rad. flux from roof to bld [W m-2(roof)]
REAL, DIMENSION(SIZE(PTA),SIZE(B%XTI_BLD,2)) :: ZRAD_WL_FL ! rad. flux from averaged wall to floor [W m-2(wall)]
REAL, DIMENSION(SIZE(PTA),SIZE(B%XTI_BLD,2)) :: ZRAD_WL_MA  ! rad. flux from averaged wall to mass [W m-2(wall)]
REAL, DIMENSION(SIZE(PTA),SIZE(B%XTI_BLD,2)) :: ZRAD_WIN_FL  ! rad. flux from averaged wall to floor [W m-2(win)]
REAL, DIMENSION(SIZE(PTA),SIZE(B%XTI_BLD,2)) :: ZRAD_WIN_MA   ! rad. flux from averaged wall to mass [W m-2(win)]
REAL, DIMENSION(SIZE(PTA),SIZE(B%XTI_BLD,2)) :: ZCONV_WL_BLD  ! rad. flux from roof to bld [W m-2(wall)]
REAL, DIMENSION(SIZE(PTA),SIZE(B%XTI_BLD,2)) :: ZCONV_WIN_BLD   ! rad. flux from roof to bld [W m-2(win)]
REAL, DIMENSION(SIZE(PTA)) :: ZAC_WIN         ! window aerodynamic conductance
REAL, DIMENSION(SIZE(PTA),SIZE(B%XTI_BLD,2)) :: ZCOMP_T_RAD_IND  ! Indoor mean radiant temperature [K]
!
REAL, DIMENSION(SIZE(PTA)) :: ZLOAD_IN_RF   ! indoor load on roof W/m2[roof]
REAL, DIMENSION(SIZE(PTA),SIZE(B%XTI_BLD,2)) :: ZLOAD_IN_FL   ! indoor load on floor W/m2[floor]
REAL, DIMENSION(SIZE(PTA)) :: ZLOAD_IN_WL   ! indoor load on wall W/m2[wall]
REAL, DIMENSION(SIZE(PTA)) :: ZLOAD_IN_WIN   ! indoor load on win W/m2[win]
REAL, DIMENSION(SIZE(PTA),SIZE(B%XTI_BLD,2)) :: ZLOAD_IN_MA   ! indoor load on mass W/m2[mass]
!
REAL, DIMENSION(SIZE(PTA)) :: ZSUMDIFIMP      ! Energy imbalance due to implicitation W/m²[urb]
!
REAL, DIMENSION(SIZE(PTA)) :: ZLEFLIM_ROOF
REAL, DIMENSION(SIZE(PTA)) :: ZLEFLIM_ROAD
!
REAL, DIMENSION(SIZE(PTA)) :: ZEMIT_LW_SNOW_ROAD
REAL, DIMENSION(SIZE(PTA)) :: ZEMIT_LW_SNOW_ROOF
REAL, DIMENSION(SIZE(PTA)) :: ZDIAG_TI_ROOF
REAL, DIMENSION(SIZE(PTA)) :: ZAGG_QIN
REAL, DIMENSION(SIZE(PTA)) :: ZAGG_HOTWAT ! energy needed for hot water over all compartiments [W/m2(bld)]
REAL, DIMENSION(SIZE(PTA)) :: ZRUNOFF_ROAD,ZRUNOFF_ROOF ! water runoff on roofs and roads (kg/m2/s)
!
! New local variables for BEM compartments
!
REAL, DIMENSION(SIZE(PTA),BOP%NBEMCOMP) :: ZRHOI         ! indoor air density
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_H_BLD_COOL
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_LE_BLD_COOL
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_HVAC_COOL
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_HVAC_HEAT
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_H_WASTE_CANY
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_LE_WASTE_CANY
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_H_WASTE_ROOF
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_LE_WASTE_ROOF
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_LE_EVAPORATIVE_COOLING
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_FLX_BLD_FLOOR
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_FLX_BLD_MASS
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_INFCALC
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_CST_H_WASTE_CANY
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_CST_LE_WASTE_CANY
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_COEFF_H_WASTE_CANY
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_COEFF_LE_WASTE_CANY
REAL, DIMENSION(SIZE(PTA)) :: ZCOMP_DIAG_DCS_AREA
!
INTEGER :: JJ, JCOMP   ! Loop index
INTEGER              :: ILUOUT     ! Unit number
!
!***!
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE                                             
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TEB_BLD_ROAD',0,ZHOOK_HANDLE)
!
!*      1.     Initializations
!              ---------------
!
!*      1.1    Water reservoirs
!              ----------------
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('TEB: FATAL ERROR DURING ARGUMENT TRANSFER')
ENDIF
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
ZEMIT_LW_SNOW_ROAD(:)=0.0
ZEMIT_LW_SNOW_ROOF(:)=0.0
!
!*      1.2    radiative snow variables at previous time-step
!              ----------------------------------------------
!
ZTSSN_RD(:) = T%TSNOW_ROAD%TS(:)
!
!
!*      1.3    indoor aerodynamique conductance for DEF case
!              ----------------------------------------------
!
ZAC_BLD(:) = XUNDEF
IF (TOP%CBEM=='DEF') ZAC_BLD=1. / 0.123 / (XCPD * PRHOA(:)) !* (normalized by rho Cp for convenience)
!-------------------------------------------------------------------------------
!
!*      1.3    number of roof/wall layer
!              -------------------------
!
IWL = SIZE(T%XT_WALL_A,2)
IRF = SIZE(T%XT_ROOF,2)
!
ZTS_WL_A  (:)=T%XT_WALL_A   (:,1)
ZTS_WL_B  (:)=T%XT_WALL_B   (:,1)
ZTS_WL    (:)=0.5 * (ZTS_WL_A(:)+ZTS_WL_B(:))
ZTS_RD    (:)=T%XT_ROAD     (:,1)
ZTS_RF    (:)=T%XT_ROOF     (:,1)
!
!*      1.4    load on indoor walls
!              -------------------------
!
IF (TOP%CBEM=='BEM') THEN
  !
  IF (MINVAL(PQINMOD).LT.0.0) CALL ABOR1_SFX ("TEB_BLD_ROAD: Negative B%XQIN")
  !
  ! The loads on roof, wall and window are aggregated
  ! over the compartments
  !
  ZLOAD_IN_RF (:) = 0.0
  ZLOAD_IN_WL (:) = 0.0
  ZLOAD_IN_WIN(:) = 0.0
  !
  DO JCOMP=1,BOP%NBEMCOMP
     !
     ZLOAD_IN_RF(:) = ZLOAD_IN_RF(:) + B%XFRACOMP(:,JCOMP) * ( &
        B%XF_FLOOR_WIN(:)*DMT%XTR_SW_WIN(:,JCOMP)                +   & 
        PQINMOD(:,JCOMP)*B%XN_FLOOR(:)*(1.0-B%XQIN_FLAT(:)) &
        *B%XQIN_FRAD(:)/(2.0+T%XWALL_O_BLD(:)+B%XGLAZ_O_BLD(:)+B%XMASS_O_BLD(:)))
     !
     ZLOAD_IN_FL(:,JCOMP) = B%XF_FLOOR_WIN * DMT%XTR_SW_WIN(:,JCOMP) + &
        PQINMOD(:,JCOMP)*B%XN_FLOOR(:)*(1.0-B%XQIN_FLAT(:)) &
        *B%XQIN_FRAD(:)/(2.0+T%XWALL_O_BLD(:)+B%XGLAZ_O_BLD(:)+B%XMASS_O_BLD(:))
     !
     ZLOAD_IN_MA(:,JCOMP)  = B%XF_MASS_WIN(:) *DMT%XTR_SW_WIN(:,JCOMP) + &
        PQINMOD(:,JCOMP)*B%XN_FLOOR(:)*(1.0-B%XQIN_FLAT(:)) &
        *B%XQIN_FRAD(:)/(2.0+T%XWALL_O_BLD(:)+B%XGLAZ_O_BLD(:)+B%XMASS_O_BLD(:))
     !
     ZLOAD_IN_WL(:) = ZLOAD_IN_WL(:) + B%XFRACOMP(:,JCOMP) * ( &
         B%XF_WALL_WIN(:) *DMT%XTR_SW_WIN(:,JCOMP)                +   &
         PQINMOD(:,JCOMP)*B%XN_FLOOR(:)*(1.0-B%XQIN_FLAT(:)) &
         *B%XQIN_FRAD(:)/(2.0+T%XWALL_O_BLD(:)+B%XGLAZ_O_BLD(:)+B%XMASS_O_BLD(:)))
     !
     ZLOAD_IN_WIN (:) = ZLOAD_IN_WIN (:) + B%XFRACOMP(:,JCOMP) * ( & 
         B%XF_WIN_WIN(:)  *DMT%XTR_SW_WIN(:,JCOMP)                +   &
         PQINMOD(:,JCOMP)*B%XN_FLOOR(:)*(1.0-B%XQIN_FLAT(:)) &
         *B%XQIN_FRAD(:)/(2.0+T%XWALL_O_BLD(:)+B%XGLAZ_O_BLD(:)+B%XMASS_O_BLD(:)))
     !
  ENDDO
  !
ELSE
  ZLOAD_IN_RF = 0.
  ZLOAD_IN_FL = 0.
  ZLOAD_IN_MA = 0.
  ZLOAD_IN_WL = 0.
  ZLOAD_IN_WIN = 0.
ENDIF
!
!-------------------------------------------------------------------------------
!
!*      4.     Surface drag
!              ------------
!
 CALL URBAN_DRAG(TOP, T, B, HIMPLICIT_WIND, PTSTEP, PT_CANYON, PQ_CANYON, &
                 PU_CANYON, PT_LOWCAN, PQ_LOWCAN, PU_LOWCAN, PZ_LOWCAN, &
                 PQA_RF,                                                &
                 ZTS_RF, ZTS_RD, ZTS_WL, PTS_GARDEN, PDN_RF, PDN_RD,    &
                 PEXNS, PEXNA, PTA, PQA, PPS, PRHOA, PZREF, PUREF,      &
                 PVMOD, PWS_RF_MAX, PWS_RD_MAX,                         &
                 PPEW_A_COEF,                                           &
                 PPEW_B_COEF, PPEW_A_COEF_LOWCAN, PPEW_B_COEF_LOWCAN, AT, &
                 PQSAT_RF, PQSAT_RD, PDELT_RF, PDELT_RD, PCD, PCDN,     &
                 PAC_RF, PAC_RF_WAT, PAC_WL, PAC_RD, PAC_RD_WAT,        &
                 PAC_TOP, PAC_GARDEN, PRI_TWN, PUW_RD, PUW_RF,          &
                 PDUWDU_RD, PDUWDU_RF, PUSTAR_TWN, ZAC_WIN    )
!
! The adjustment of the water reservoirs has been 
! shifted behind the call of urban_drag in order 
! to avoid wrong values of PDELT
!
!* area-averaged heat transfer coefficient
!
ZVMOD(:) = WIND_THRESHOLD(PVMOD(:),PUREF(:))
!
PCH_TWN(:) = (T%XBLD(:) * PAC_RF(:) + (1.-T%XBLD(:)) * PAC_TOP (:)) / ZVMOD(:)
!
!* aggregation of momentum fluxes for roofs (=> derivate of flux also recalculated)
!
PUW_RF (:) = (1-T%XGREENROOF(:)) * PUW_RF(:) + T%XGREENROOF(:) * PUW_GR(:)
WHERE (PVMOD(:)/=0.) PDUWDU_RF(:) = 2. * PUW_RF(:) / PVMOD(:)
!

!-------------------------------------------------------------------------------
!
!*      5.     Snow mantel model
!              -----------------
!
 CALL URBAN_SNOW_EVOL(TOP, T, B, DMT, CT, LW, HPROGRAM, PT_LOWCAN, PQ_LOWCAN, PU_LOWCAN, ZTS_RF, ZTS_RD, ZTS_WL_A,  &
                      ZTS_WL_B, PPS, PTA_RF, PQA_RF, PRHOA, PLW_RAD, PSR, PZREF, PUREF, PVMOD,           &
                      PTSTEP, PZ_LOWCAN, PDN_RF, DMT%XABS_SW_SNOW_ROOF,                                 &
                      DMT%XABS_LW_SNOW_ROOF, PDN_RD, DMT%XABS_SW_SNOW_ROAD,                            &
                      DMT%XABS_LW_SNOW_ROAD, PRNSN_RF, PHSN_RF, PLESN_RF, PGSN_RF,                      &
                      PMELT_RF, PRNSN_RD, PHSN_RD, PLESN_RD, PGSN_RD, PMELT_RD,                          &
                      PTS_HVEG,                                                                           &
                      ZEMIT_LW_SNOW_ROAD, ZEMIT_LW_SNOW_ROOF,                                             &
                      DMT%XSNOW_HEAT_ROAD, DMT%XSNOW_HEAT_ROOF)
!
! Recalculation of snow-free fractions
!
PDF_RD(:)=1.-PDN_RD(:)
PDF_RF(:)=1.-PDN_RF(:)
!
IF (CT%LCHECK_TEB) THEN
  CT%XSEN_MELT_ROAD(:)=T%XROAD(:)*XLMTT*PDN_RD(:)*PMELT_RD(:)
  CT%XSEN_MELT_ROOF(:)=(1.0-T%XGREENROOF(:))*T%XBLD(:)*XLMTT*PDN_RF(:)*PMELT_RF(:)
END IF
!
!
!-------------------------------------------------------------------------------
!
!*      7.    Indoor radiative temperature
!              ---------------------------
!
! uses the averaged temperature of both walls for the building energy balance
ZT_WL   (:,:)=0.5 * (T%XT_WALL_A(:,:)+T%XT_WALL_B(:,:))
!
SELECT CASE(TOP%CBEM)
   CASE("DEF")
      ZTS_FL(:,1) = 19. + XTT
      ZTS_MA(:,1) = XUNDEF    
      !
      DMT%XT_RAD_IND(:,1) = ( T%XWALL_O_HOR(:) / T%XBLD(:) * ZT_WL(:,IWL) + &
          T%XT_ROOF(:,IRF) + ZTS_FL(:,1) ) / (T%XWALL_O_HOR(:) / T%XBLD(:) + 1. + 1.) 
      !
      ZRADHT_IN(:,1) = XUNDEF
      !
   CASE("BEM")
      DO JCOMP=1,BOP%NBEMCOMP
         ZTS_FL(:,JCOMP) = B%XT_FLOOR(:,1,JCOMP)
         ZTS_MA(:,JCOMP) = B%XT_MASS (:,1,JCOMP)
      ENDDO
      !
      DMT%XT_RAD_IND(:,:) = 0.0
      !
      DO JCOMP=1,BOP%NBEMCOMP
         !
         ZCOMP_T_RAD_IND(:,JCOMP)  = (ZTS_MA(:,JCOMP)*B%XMASS_O_BLD(:) + ZT_WL(:,IWL)*T%XWALL_O_BLD(:)     &
            + ZTS_FL(:,JCOMP) + T%XT_ROOF(:,IRF) + B%XT_WIN2(:) * B%XGLAZ_O_BLD(:)) &
            / (B%XMASS_O_BLD(:) + T%XWALL_O_BLD(:) + 1. + 1. + B%XGLAZ_O_BLD(:))
         !
         DMT%XT_RAD_IND(:,JCOMP) = ZCOMP_T_RAD_IND(:,JCOMP)
         !
      ENDDO
      !
      ! Assuming indoor surface emissivities of 0.9
      !
      DO JCOMP=1,BOP%NBEMCOMP
         ZRADHT_IN(:,JCOMP) = 0.9 * 0.9 * 4 * XSTEFAN * ZCOMP_T_RAD_IND(:,JCOMP)**3
      ENDDO
      ! 
END SELECT
!
!
!*      7.    Roof Ts computation
!              -------------------
!
!* ts_roof and qsat_roof are updated
!
 CALL ROOF_LAYER_E_BUDGET(TOP, BOP, T, B, CT, DMT, HPROGRAM, PQSAT_RF, ZAC_BLD, PTSTEP, PDN_RF, PRHOA,    &
                          PAC_RF, PAC_RF_WAT, PLW_RAD, PPS, PDELT_RF, PTA_RF, PQA_RF,   &
                          PEXNA, PEXNS, DMT%XABS_SW_ROOF, PGSN_RF, ZFLX_BLD_RF, &
                          ZDQS_RF, DMT%XABS_LW_ROOF, DMT%XH_ROOF, PLEW_RF, &
                          DMT%XG_GREENROOF_ROOF, ZRADHT_IN, ZTS_FL, ZT_WL(:,IWL),&
                          ZRAD_RF_WL, ZRAD_RF_WIN, ZRAD_RF_FL, ZRAD_RF_MA, ZCONV_RF_BLD, PRR, &
                          ZLOAD_IN_RF, ZLEFLIM_ROOF, ZDIAG_TI_ROOF  )
!
!-------------------------------------------------------------------------------
!
!*      8.    Road Ts computations
!              -----------------------------
!
!* ts_road, ts_wall, qsat_road, t_canyon and q_canyon are updated
!
! By adding PIRRIG_ROAD to PRR, it is assumed that the temperature
! of the water used for road irrigation is equal to the temperature of
! the canyon air (similar to rain water). This is not necessarily realistic.
!
 CALL ROAD_LAYER_E_BUDGET(T, B, CT, LW, TOP, DMT, HPROGRAM, PTSTEP, PDN_RD, PRHOA, PAC_RD, PAC_RD_WAT, &
                          PLW_RAD, PPS, PQSAT_RD, PDELT_RD, PEXNS,         &
                          DMT%XABS_SW_ROAD, PGSN_RD, PQ_LOWCAN, PT_LOWCAN,&
                          ZTS_WL_A, ZTS_WL_B, ZTSSN_RD,  PTS_GARDEN, PTS_HVEG, &
                          PEMIT_LW_RD, ZDQS_RD, DMT%XABS_LW_ROAD, &
                          DMT%XH_ROAD, PLEW_RD, PRR+DMT%XIRRIG_ROAD, &
                          ZLEFLIM_ROAD                                )
!
DMT%XLEW_RF = PLEW_RF
DMT%XLEW_RD = PLEW_RD
DMT%XLESN_RD = PLESN_RD
DMT%XLESN_RF = PLESN_RF
! Calculation of the weighted average of the longwave emission
! on snow-free and snow-covered road
!
PEMIT_LW_RD(:)=PDF_RD(:)*PEMIT_LW_RD(:)+PDN_RD(:)*ZEMIT_LW_SNOW_ROAD(:)
!
! Conversion between latent and sensible heat on roads and roofs
!
IF (CT%LCHECK_TEB) THEN
  CT%XCONV_LAT_SEN_ROAD(:)=-PDN_RD(:)*PLESN_RD(:)-PDF_RD(:)*PLEW_RD(:)
  CT%XCONV_LAT_SEN_ROOF(:)=(1.0-T%XGREENROOF(:))*(-PDN_RF(:)*PLESN_RF(:)-PDF_RF(:)*PLEW_RF(:))
END IF
!
!-------------------------------------------------------------------------------
!
!*      8.     Wall Ts computations
!              -----------------------------
!
 CALL FACADE_E_BUDGET(TOP, BOP, T, B, DMT, CT, LW, HPROGRAM, PTSTEP, PDN_RD, PRHOA, PAC_WL, ZAC_BLD,   &
                     ZDIAG_TI_ROOF, PLW_RAD, PPS, PEXNS, PT_CANYON, &
                     ZTS_RD, ZTSSN_RD, PTS_GARDEN, PTS_HVEG, &
                     ZFLX_BLD_WL_A, ZDQS_WL_A, ZFLX_BLD_WL_B, ZDQS_WL_B, PEMIT_LW_FAC,      &
                     ZRADHT_IN, ZRAD_RF_WL, ZRAD_RF_WIN, ZRAD_WL_FL, ZRAD_WL_MA,     &
                     ZRAD_WIN_FL, ZRAD_WIN_MA, ZCONV_WL_BLD,  &
                     ZCONV_WIN_BLD, ZAC_WIN, ZLOAD_IN_WL, ZLOAD_IN_WIN,    &
                     ZSUMDIFIMP                                            )
!
!-------------------------------------------------------------------------------
!
!*      9.     Evolution of interior building air temperature
!              and temperature of soil column below
!              ----------------------------------------------
!
! uses the averaged temperature of both walls for the building energy balance
ZT_WL   (:,:)=0.5 * (T%XT_WALL_A(:,:)+T%XT_WALL_B(:,:))
!
DMT%XH_WASTE_ROOF(:)  = 0.0 
DMT%XLE_WASTE_ROOF(:) = 0.0
DMT%XLE_EVAPORATIVE_COOLING(:) = 0.0
!
SELECT CASE(TOP%CBEM)
CASE("DEF")
!
   CALL BLD_E_BUDGET(HPROGRAM, .TRUE., PTSTEP, T%XBLD, T%XWALL_O_HOR,        &
                     PRHOA, T%XT_ROOF, ZT_WL, B%XTI_BLD(:,1), ZTS_FL(:,1),   &
                     T%XT_BLD(:,1), ZG_FLOOR)
!

CASE("BEM")
   !
   ! Loop over all compartments
   ! At the moment the maximum number of compartments is considered
   ! for all grid points. However, bem could be changed to an 1d
   ! version in order to reduce the number of total calls. 
   !
   ! #######################################################################
   ! Initialisation of the variables to be aggregated
   ! #######################################################################
   !
   DMT%XH_BLD_COOL(:)    = 0.0
   DMT%XLE_BLD_COOL(:)   = 0.0 
   DMT%XHVAC_COOL(:)     = 0.0 
   DMT%XHVAC_HEAT(:)     = 0.0 
   DMT%XH_WASTE_CANY(:)  = 0.0 
   DMT%XLE_WASTE_CANY(:) = 0.0 
   PCST_H_WASTE_CANY(:) = 0.0 
   PCST_LE_WASTE_CANY(:)= 0.0 
   PCOE_H_WASTE_CANY  (:) = 0.
   PCOE_LE_WASTE_CANY (:) = 0.
   PMUL_H_WASTE_CANY  (:) = 0.
   PMUL_LE_WASTE_CANY (:) = 0.
   DMT%XCOP(:)           = 0.0 
   DMT%XCAP_SYS(:)       = 0.0 
   ZFLX_BLD_FL(:) = 0.0 
   ZFLX_BLD_MA(:)  = 0.0
   DMT%XINFCALC(:)       = 0.0
   !
   IF (CT%LCHECK_TEB) CT%XDIAG_DCS_AREA(:) = 0.0
   !
   ZEFF_HEAT=1./(B%XFRAC_HEAT_ELEC/BOP%XEFF_HEAT_ELEC + B%XFRAC_HEAT_GAS/BOP%XEFF_HEAT_GAS + &
                 B%XFRAC_HEAT_FUEL/BOP%XEFF_HEAT_FUEL + B%XFRAC_HEAT_OTHER/BOP%XEFF_HEAT_OTHER )

   ZFRAC_HEAT_LE=  B%XFRAC_HEAT_ELEC * (1.-BOP%XLHV_HHV_ELEC )/BOP%XEFF_HEAT_ELEC  &
                 + B%XFRAC_HEAT_GAS  * (1.-BOP%XLHV_HHV_GAS  )/BOP%XEFF_HEAT_GAS   &
                 + B%XFRAC_HEAT_FUEL * (1.-BOP%XLHV_HHV_FUEL )/BOP%XEFF_HEAT_FUEL  &
                 + B%XFRAC_HEAT_OTHER* (1.-BOP%XLHV_HHV_OTHER)/BOP%XEFF_HEAT_OTHER
   !
   IF ((MINVAL(ZEFF_HEAT).LT.0.0).OR.(MAXVAL(ZEFF_HEAT).GT.1.0)) THEN
      CALL ABOR1_SFX ("TEB_BLD_ROAD: Wrong value for ZEFF_HEAT")
   ENDIF
   !
   IF ((MINVAL(ZFRAC_HEAT_LE).LT.0.0).OR.(MAXVAL(ZFRAC_HEAT_LE).GT.1.0)) THEN
      CALL ABOR1_SFX ("TEB_BLD_ROAD: Wrong value for ZFRAC_HEAT_LE")
   ENDIF     
   !
   ! ######################################################
   ! Loop over the compartments
   ! ######################################################
   !
   ZG_FLOOR(:) = 0.
   !
   DO JCOMP=1,SIZE(B%XFRACOMP,2)
      CALL BEM(BOP, T, B, DMT, CT, JCOMP, HPROGRAM, PTSTEP, KDAY, PPS, PPSOLD, PRHOA, PT_CANYON, &
               PQ_CANYON, PU_CANYON, ZEFF_HEAT, ZRADHT_IN(:,JCOMP),ZLOAD_IN_FL(:,JCOMP),     &
               ZLOAD_IN_MA(:,JCOMP), ZCONV_RF_BLD(:,JCOMP),                             &
               ZCONV_WL_BLD(:,JCOMP),ZCONV_WIN_BLD(:,JCOMP),                            &
               ZRAD_RF_MA(:,JCOMP), ZRAD_RF_FL(:,JCOMP),                                &
               ZRAD_WL_MA(:,JCOMP), ZRAD_WL_FL(:,JCOMP),                                &
               ZRAD_WIN_MA(:,JCOMP), ZRAD_WIN_FL(:,JCOMP),                              &
               B%CNATVENT(:,JCOMP), PVENT_BEHAV_ANYWAY(:,JCOMP),                        &
               PVENT_BEHAV_ADAPTI(:,JCOMP), PTCOOL_TARGET(:,JCOMP),                     &
               PTHEAT_TARGET(:,JCOMP), PQINMOD(:,JCOMP),                                &
               ZCOMP_H_BLD_COOL, ZCOMP_LE_BLD_COOL, ZCOMP_HVAC_COOL,                    &
               ZCOMP_HVAC_HEAT, ZCOMP_H_WASTE_CANY,                                     &
               ZCOMP_LE_WASTE_CANY,ZCOMP_H_WASTE_ROOF, ZCOMP_LE_WASTE_ROOF, ZCOMP_LE_EVAPORATIVE_COOLING, &
               ZCOMP_FLX_BLD_FLOOR,ZCOMP_FLX_BLD_MASS,                                  &
               ZCOMP_INFCALC, ZRHOI(:,JCOMP),DMT%XDIAGVENT(:,JCOMP),DMT%XDIAGVEFL(:,JCOMP), &
               ZCOMP_CST_H_WASTE_CANY,ZCOMP_CST_LE_WASTE_CANY,ZCOMP_COEFF_H_WASTE_CANY, &
               ZCOMP_COEFF_LE_WASTE_CANY,ZFRAC_HEAT_LE,ZCOMP_G_FLOOR, ZCOMP_DIAG_DCS_AREA,  "OK")
      !
      ! Save heating and cooling energy demand for each compartment
      !
      DMT%XCOMP_HVAC_COOL(:,JCOMP) = ZCOMP_HVAC_COOL(:)
      DMT%XCOMP_HVAC_HEAT(:,JCOMP) = ZCOMP_HVAC_HEAT(:)
      !
      ! Aggregated flux with the soil below the building
      !
      ZG_FLOOR(:) = ZG_FLOOR(:) +  B%XFRACOMP(:,JCOMP) * ZCOMP_G_FLOOR(:)
      !
      ! ####################################################################
      ! Aggregation of output variables
      ! ####################################################################
      !
      DMT%XH_BLD_COOL(:)    = DMT%XH_BLD_COOL(:)    + B%XFRACOMP(:,JCOMP) * ZCOMP_H_BLD_COOL(:)
      DMT%XLE_BLD_COOL(:)   = DMT%XLE_BLD_COOL(:)   + B%XFRACOMP(:,JCOMP) * ZCOMP_LE_BLD_COOL(:)
      DMT%XHVAC_COOL(:)     = DMT%XHVAC_COOL(:)     + B%XFRACOMP(:,JCOMP) * ZCOMP_HVAC_COOL(:)
      DMT%XHVAC_HEAT(:)     = DMT%XHVAC_HEAT(:)     + B%XFRACOMP(:,JCOMP) * ZCOMP_HVAC_HEAT(:)
      PCST_H_WASTE_CANY(:)  = PCST_H_WASTE_CANY(:)  + B%XFRACOMP(:,JCOMP) * ZCOMP_CST_H_WASTE_CANY(:)
      PCST_LE_WASTE_CANY(:) = PCST_LE_WASTE_CANY(:) + B%XFRACOMP(:,JCOMP) * ZCOMP_CST_LE_WASTE_CANY(:)
      PCOE_H_WASTE_CANY (:) = PCOE_H_WASTE_CANY (:) + B%XFRACOMP(:,JCOMP) * ZCOMP_COEFF_H_WASTE_CANY (:)
      PCOE_LE_WASTE_CANY(:) = PCOE_LE_WASTE_CANY(:) + B%XFRACOMP(:,JCOMP) * ZCOMP_COEFF_LE_WASTE_CANY(:)
      PMUL_H_WASTE_CANY (:) = PMUL_H_WASTE_CANY (:) + B%XFRACOMP(:,JCOMP) * ZCOMP_COEFF_H_WASTE_CANY (:) * B%XTI_BLD(:,JCOMP)
      PMUL_LE_WASTE_CANY(:) = PMUL_LE_WASTE_CANY(:) + B%XFRACOMP(:,JCOMP) * ZCOMP_COEFF_LE_WASTE_CANY(:) * B%XQI_BLD(:,JCOMP)
      DMT%XINFCALC(:)       = DMT%XINFCALC(:)       + B%XFRACOMP(:,JCOMP) * ZCOMP_INFCALC(:)
      DMT%XH_WASTE_CANY(:)  = DMT%XH_WASTE_CANY(:)  + B%XFRACOMP(:,JCOMP) * ZCOMP_H_WASTE_CANY(:)
      DMT%XLE_WASTE_CANY(:) = DMT%XLE_WASTE_CANY(:) + B%XFRACOMP(:,JCOMP) * ZCOMP_LE_WASTE_CANY(:)
      DMT%XH_WASTE_ROOF(:)  = DMT%XH_WASTE_ROOF(:)  + B%XFRACOMP(:,JCOMP) * ZCOMP_H_WASTE_ROOF(:)
      DMT%XLE_WASTE_ROOF(:) = DMT%XLE_WASTE_ROOF(:) + B%XFRACOMP(:,JCOMP) * ZCOMP_LE_WASTE_ROOF(:)
      DMT%XLE_EVAPORATIVE_COOLING(:) = DMT%XLE_EVAPORATIVE_COOLING(:) + B%XFRACOMP(:,JCOMP) * ZCOMP_LE_EVAPORATIVE_COOLING(:)
      ZFLX_BLD_FL(:)        = ZFLX_BLD_FL(:)        + B%XFRACOMP(:,JCOMP) * ZCOMP_FLX_BLD_FLOOR(:)
      ZFLX_BLD_MA(:)        = ZFLX_BLD_MA(:)        + B%XFRACOMP(:,JCOMP) * ZCOMP_FLX_BLD_MASS(:)
      !
      IF (CT%LCHECK_TEB) THEN
         CT%XDIAG_DCS_AREA(:) = CT%XDIAG_DCS_AREA(:) + B%XFRACOMP(:,JCOMP) * ZCOMP_DIAG_DCS_AREA(:)
      ENDIF
      !
   ENDDO
   !
   IF (CT%LCHECK_TEB) CT%XRHOI = ZRHOI
   !
END SELECT
!
!         The energy imbalance due to the implicitation
!         is added to the waste heat flux at roof level
!
  DMT%XH_WASTE_ROOF(:)=DMT%XH_WASTE_ROOF(:)+ZSUMDIFIMP(:)
!
!         The error in the latent heat flux made due to the
!         limitation of the available water reservoir is
!         added to the latent heat waste flux at roof level
!
  DMT%XLE_WASTE_ROOF(:)=DMT%XLE_WASTE_ROOF(:)                    + &
       (1.0-T%XGREENROOF(:))*T%XBLD(:)*PDF_RF(:)*ZLEFLIM_ROOF(:) + &
       T%XROAD(:)*PDF_RD(:)*ZLEFLIM_ROAD(:)
!
!
!-------------------------------------------------------------------------------
!
!*      9.     Evolution of temperature of soil column below buildings
!              -------------------------------------------------------
!
!
CALL BLDSOIL_LAYER_E_BUDGET(HPROGRAM, CT%LCHECK_TEB ,CT%XCHECK_PROCESS, &
                            T%XT_BLD, PTSTEP, ZG_FLOOR,  &
                            T%XHC_BLD, T%XTC_BLD, T%XD_BLD, T%XBLD )
!
!-------------------------------------------------------------------------------
!
!*      10.    Fluxes over built surfaces
!              --------------------------
!
ZAGG_TR_SW_WIN(:) = 0.0
ZAGG_QIN      (:) = 0.0
ZAGG_HOTWAT   (:) = 0.0
!
IF (TOP%CBEM.EQ."BEM") THEN
   DO JCOMP=1,SIZE(B%XFRACOMP,2)
      ZAGG_TR_SW_WIN(:) = ZAGG_TR_SW_WIN(:) + B%XFRACOMP(:,JCOMP) * DMT%XTR_SW_WIN(:,JCOMP)
      ZAGG_QIN      (:) = ZAGG_QIN(:)       + B%XFRACOMP(:,JCOMP) * PQINMOD(:,JCOMP)
      ZAGG_HOTWAT   (:) = ZAGG_HOTWAT   (:) + B%XFRACOMP(:,JCOMP) * DMT%XCOMP_HOTWAT(:,JCOMP)
   ENDDO
ELSE
   ZAGG_QIN    (:) = XUNDEF
   ZAGG_HOTWAT (:) = XUNDEF
ENDIF
!
IF (MINVAL(ZAGG_QIN)   .LT.-XSURF_EPSILON) CALL ABOR1_SFX("TEB_BLD_ROAD: Negative ZAGG_QIN")
IF (MINVAL(ZAGG_HOTWAT).LT.-XSURF_EPSILON) CALL ABOR1_SFX("TEB_BLD_ROAD: Negative ZAGG_HOTWAT")
!
 CALL URBAN_FLUXES   (TOP, T, B, DMT, HIMPLICIT_WIND, PT_CANYON, PPEW_A_COEF, PPEW_B_COEF,                &
                      PEXNS, PRHOA, PVMOD, PH_TRAFFIC, PLE_TRAFFIC, ZAGG_TR_SW_WIN,                       &
                      ZAC_WIN, PCD, PDF_RF, PDN_RF, PDF_RD, PDN_RD, PRNSN_RF, PHSN_RF, PLESN_RF, PGSN_RF, &
                      PRNSN_RD, PHSN_RD, PLESN_RD, PGSN_RD, PMELT_RF, ZDQS_RF, PMELT_RD,                  &
                      ZDQS_RD, ZDQS_WL_A, ZDQS_WL_B, ZFLX_BLD_RF, ZFLX_BLD_WL_A,                          &
                      ZFLX_BLD_WL_B, ZFLX_BLD_FL, ZFLX_BLD_MA, PE_SHADING, ZAGG_QIN, PLEW_RF,             &
                      PRN_GR, PH_GR, PLE_GR, PGFLUX_GR,                                                   &
                      PLEW_RD, DMT%XMELT_BLT, PUSTAR_TWN                                                      )
!
DMT%XMELT_BLD=PMELT_RF
DMT%XMELT_RD=PMELT_RD
IF (CT%LCHECK_TEB) THEN
  CT%XFLUXFLOOR(:) = T%XBLD(:)*ZG_FLOOR(:)
  CT%XFLX_BLD_ROOF(:) = ZFLX_BLD_RF(:)
  CT%XFLX_BLD_WALL_A(:) = ZFLX_BLD_WL_A(:)
  CT%XFLX_BLD_WALL_B(:) = ZFLX_BLD_WL_B(:)
END IF
!
! Water transfer from snow reservoir to water reservoir in case of snow melt
! Calculation of runoff due to snow melting
!
IF (CT%LCHECK_TEB) THEN
  CT%XDIFF_SNOW_WAT_ROOF(:)=0.0
  CT%XDIFF_SNOW_WAT_ROAD(:)=0.0
END IF

DO JJ=1,SIZE(ZRUNOFF_ROOF)
   !
   ZRUNOFF_ROOF(JJ)=MAX(0.0,(T%XWS_ROOF(JJ)+PDN_RF(JJ)*PMELT_RF(JJ)*PTSTEP)-PWS_RF_MAX(JJ))/PTSTEP
   ZRUNOFF_ROAD(JJ)=MAX(0.0,(T%XWS_ROAD(JJ)+PDN_RD(JJ)*PMELT_RD(JJ)*PTSTEP)-PWS_RD_MAX(JJ))/PTSTEP
   !
   IF (CT%LCHECK_TEB) THEN
     IF (PMELT_RF(JJ).GT.0.0) CT%XDIFF_SNOW_WAT_ROOF(JJ)=(1.0-T%XGREENROOF(JJ))*T%XBLD(JJ) *XLMTT*PDN_RF(JJ)*PMELT_RF(JJ)
     IF (PMELT_RD(JJ).GT.0.0) CT%XDIFF_SNOW_WAT_ROAD(JJ)=T%XROAD(JJ)*XLMTT*PDN_RD(JJ)*PMELT_RD(JJ)
   END IF
   !
   T%XWS_ROOF(JJ)=MIN(PWS_RF_MAX(JJ),T%XWS_ROOF(JJ)+PDN_RF(JJ)*PMELT_RF(JJ)*PTSTEP)
   T%XWS_ROAD(JJ)=MIN(PWS_RD_MAX(JJ),T%XWS_ROAD(JJ)+PDN_RD(JJ)*PMELT_RD(JJ)*PTSTEP)
   !
ENDDO
!
!-------------------------------------------------------------------------------
!
!*      11.    Roof and road reservoirs evolution
!              ----------------------------------
!
!              Update of water reservoirs with evaporation
!
!              Hydro for roofs
!              ---------------
!
T%XWS_ROOF(:) = T%XWS_ROOF(:) - PTSTEP*PDF_RF(:)*PLEW_RF(:)/XLVTT
T%XWS_ROOF(:) = MAX(0., T%XWS_ROOF(:))
!
!              Hydro for roads
!              ---------------
!
T%XWS_ROAD(:) = T%XWS_ROAD(:) - PTSTEP*PDF_RD(:)*PLEW_RD(:)/XLVTT
T%XWS_ROAD(:) = MAX(0., T%XWS_ROAD(:))
!
!              Surface runoff for roads and roofs
!              ----------------------------------
!
DMT%XRUNOFF_STRLROOF(:) = DMT%XRUNOFF_STRLROOF(:) + ZRUNOFF_ROOF(:)
DMT%XRUNOFF_ROOF(:)     = (1. - T%XGREENROOF(:)) * DMT%XRUNOFF_STRLROOF(:)                      &
                          +     T%XGREENROOF(:)  * (PRUNOFF_GR(:)+PDRAIN_GR(:))
DMT%XRUNOFF_ROAD(:)     = DMT%XRUNOFF_ROAD(:) + ZRUNOFF_ROAD(:)
!
!
!-------------------------------------------------------------------------------
!
!*      19.    Compute aerodynamical resistance 
!              --------------------------------
!
PRESA_TWN(:) = 1. / ( T%XBLD(:) * PAC_RF(:)  + ( 1. - T%XBLD(:)) * PAC_TOP (:))
!
!-------------------------------------------------------------------------------
!       20.     Compute CO2 fluxes link to buildings
!-------------------------------------------------------------------------------
!
SELECT CASE(TOP%CBEM)
CASE("DEF")
   DMT%XSFCO2_BLD(:)=0.
CASE("BEM")
   !
   DMT%XHOTWAT_GAS(:) = ZAGG_HOTWAT * (B%XF_HW_GAS   )/BOP%XEFF_HEAT_GAS
   DMT%XHOTWAT_ELEC(:)= ZAGG_HOTWAT * (1.-B%XF_HW_GAS)/BOP%XEFF_HEAT_ELEC
   !
   IF (MINVAL(ZAGG_QIN)        .LT.-XSURF_EPSILON) CALL ABOR1_SFX("TEB_BLD_ROAD: Negative value for internal heat release")
   IF (MINVAL(DMT%XHOTWAT_GAS) .LT.-XSURF_EPSILON) CALL ABOR1_SFX("TEB_BLD_ROAD: Negative value for warm water heating by gas")
   IF (MINVAL(DMT%XHOTWAT_ELEC).LT.-XSURF_EPSILON) &
             CALL ABOR1_SFX("TEB_BLD_ROAD: Negative value for warm water heating by electricity")
   IF (MINVAL(DMT%XHVAC_COOL)  .LT.-XSURF_EPSILON) CALL ABOR1_SFX("TEB_BLD_ROAD: Negative value for cooling energy release")
   IF (MINVAL(DMT%XHVAC_HEAT)  .LT.-XSURF_EPSILON) CALL ABOR1_SFX("TEB_BLD_ROAD: Negative value for heating energy release")
   !
   DMT%XSFCO2_BLD(:) = (                                                          &
      ZAGG_QIN(:) * B%XN_FLOOR(:) * BOP%XCF_CO2_ELEC/BOP%XEFF_HEAT_ELEC           + &
      DMT%XHOTWAT_GAS * BOP%XCF_CO2_GAS                                         + &
      DMT%XHOTWAT_ELEC * BOP%XCF_CO2_ELEC                                       + &
      DMT%XHVAC_COOL(:) * BOP%XCF_CO2_ELEC/BOP%XEFF_HEAT_ELEC                       + &
      DMT%XHVAC_HEAT(:) * ZEFF_HEAT(:)                           * ( &
        B%XFRAC_HEAT_ELEC(:)  * BOP%XCF_CO2_ELEC  /BOP%XEFF_HEAT_ELEC  + &
        B%XFRAC_HEAT_GAS(:)   * BOP%XCF_CO2_GAS   /BOP%XEFF_HEAT_GAS   + &
        B%XFRAC_HEAT_FUEL(:)  * BOP%XCF_CO2_FUEL  /BOP%XEFF_HEAT_FUEL  + &
        B%XFRAC_HEAT_OTHER(:) * BOP%XCF_CO2_OTHER /BOP%XEFF_HEAT_OTHER   ) &
                )/PRHOA(:)
   !
   DMT%XHVAC_HEAT_ELEC   = DMT%XHVAC_HEAT * (ZEFF_HEAT/BOP%XEFF_HEAT_ELEC ) * B%XFRAC_HEAT_ELEC
   DMT%XHVAC_HEAT_GAS    = DMT%XHVAC_HEAT * (ZEFF_HEAT/BOP%XEFF_HEAT_GAS  ) * B%XFRAC_HEAT_GAS
   DMT%XHVAC_HEAT_FUEL   = DMT%XHVAC_HEAT * (ZEFF_HEAT/BOP%XEFF_HEAT_FUEL ) * B%XFRAC_HEAT_FUEL 
   DMT%XHVAC_HEAT_OTHER  = DMT%XHVAC_HEAT * (ZEFF_HEAT/BOP%XEFF_HEAT_OTHER) * B%XFRAC_HEAT_OTHER
   !
CASE DEFAULT
   CALL  ABOR1_SFX('TEB_BLD_ROAD : unknown value for CBEM : '//TOP%CBEM)
ENDSELECT
!
IF (LHOOK) CALL DR_HOOK('TEB_BLD_ROAD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE TEB_BLD_ROAD
