!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ############################
      MODULE MODD_DIAG_MISC_TEB_n
!     ############################
!
!!****  *MODD_DIAG_MISC_TEB - declaration of packed surface parameters for TEB scheme
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       07/10/04
!!      C de Munck     02/13  adding runoff contributions for teb garden  
!!      V. Masson      06/2013  splits module in two
!!      E. Redon, A. Lemonsu      12/2015 Short and longwave rad absorbed by urbtrees
!!      K. Chancibault/A. Lemonsu 01/2016 Urban hydrology
!!      M. Goret       07/2017 add new diagnostics for CO2 fluxes
!!      M. Goret       08/2017 add anthropogenic flux diagnostics
!!      M. Goret       09/2017 add diagnostic of heat storage link to snow
!!      M. Goret       10/2017 add diagnostic for hot water
!
!
!*       0.   DECLARATIONS
!             ------------
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE DIAG_MISC_TEB_t
!
!* miscellaneous variables
!
  REAL, POINTER, DIMENSION(:)   :: XZ0_TOWN  ! town roughness length
  REAL, POINTER, DIMENSION(:)   :: XQF_BLD   ! domestic heating
  REAL, POINTER, DIMENSION(:)   :: XFLX_BLD ! heat flux from bld
  REAL, POINTER, DIMENSION(:)   :: XQF_TOWN  ! total anthropogenic heat
  REAL, POINTER, DIMENSION(:)   :: XDQS_TOWN ! storage inside building
!
  REAL, POINTER, DIMENSION(:)   :: XMELT_BLT   ! snow melt for built & impervious part (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XMELT_BLD   ! snow melt for building part (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XMELT_RD   ! snow melt road part (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XH_WALL_A   ! wall sensible heat flux          (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XH_WALL_B   ! wall sensible heat flux          (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XH_ROOF     ! roof sensible heat flux          (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XH_ROAD     ! road sensible heat flux          (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XH_HVEG     ! high vegetation sensible heat flux (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XH_STRLROOF ! structural roof sens. heat flux  (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XH_BLT      ! built surf sensible heat flux    (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XRN_HVEG    ! net radiation for high vegetation(W/m2)
  REAL, POINTER, DIMENSION(:)   :: XRN_WALL_A  ! net radiation at wall            (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XRN_WALL_B  ! net radiation at wall            (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XRN_ROOF    ! net radiation at roof            (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XRN_ROAD    ! net radiation at road            (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XRN_STRLROOF !net radiation at structural roofs(W/m2)
  REAL, POINTER, DIMENSION(:)   :: XRN_BLT     ! net radiation at built surf      (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XGFLUX_HVEG ! storage for high vegetation      (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XGFLUX_WALL_A !net wall conduction flux        (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XGFLUX_WALL_B !net wall conduction flux        (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XGFLUX_ROOF ! net roof conduction flux         (W/m2)                                         
  REAL, POINTER, DIMENSION(:)   :: XGFLUX_ROAD ! net road conduction flux         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XGFLUX_STRLROOF !net structural roof cond flux (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XGFLUX_BLT  ! net built surf conduction flux   (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLE_WALL_A  ! wall latent heat flux            (W/m2 façade)
  REAL, POINTER, DIMENSION(:)   :: XLE_WALL_B  ! wall latent heat flux            (W/m2 façade)
  REAL, POINTER, DIMENSION(:)   :: XLE_ROOF    ! roof latent heat flux            (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLE_ROAD    ! road latent heat flux            (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLE_HVEG    ! high vegetation latent heat flux (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLE_STRLROOF !structural roof latent heat flux (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLE_BLT     ! built surf latent heat flux      (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XQF_WALL_A  ! wall anthropogenic flux      (W/m2 façade)
  REAL, POINTER, DIMENSION(:)   :: XQF_WALL_B  ! wall anthropogenic flux      (W/m2 façade)
  REAL, POINTER, DIMENSION(:)   :: XQF_ROAD    ! road anthropogenic flux      (W/m2 road)
  REAL, POINTER, DIMENSION(:)   :: XQF_ROOF    ! roof anthropogenic flux      (W/m2 roof)
  REAL, POINTER, DIMENSION(:)   :: XQF_BLT     ! mean anthropogenic flux due to build surfaces (W/m2 urb.)
  REAL, POINTER, DIMENSION(:)   :: XSNOW_HEAT_ROAD   !heat storage link to snow on road (W/m2 (road))
  REAL, POINTER, DIMENSION(:)   :: XSNOW_HEAT_ROOF   !heat storage link to snow on roof (W/m2 (roof))
!
  REAL, POINTER, DIMENSION(:)   :: XRUNOFF_TOWN      ! aggregated water runoff for town      (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XIRRIG_GARDEN     ! summer ground irrigation rate         (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XRUNOFF_ROAD      ! water runoff for roads                (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XIRRIG_ROAD       ! road man-made watering rate           (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XRUNOFF_ROOF      ! aggregated water runoff for roofs     (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XRUNOFF_STRLROOF  ! water runoff for structural roofs     (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XIRRIG_GREENROOF  ! summer ground irrigation rate         (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XALB_GD           ! albedo for green areas                   (-)
  REAL, POINTER, DIMENSION(:)   :: FSNOW_GD          ! snow fraction over vegetation            (-)
  REAL, POINTER, DIMENSION(:)   :: XAC_AGG_GD        ! aggreg. aeodynamic resistance for green areas 
  REAL, POINTER, DIMENSION(:)   :: XAC_AGG_GR        ! aggreg. aeodynamic resistance for green roofs
  REAL, POINTER, DIMENSION(:,:) :: XSWI
  !
  ! Fraction of road and roof covered by water and snow
  !
  REAL, POINTER, DIMENSION(:) :: XDN_ROOF ! Fraction of roof covered with snow 
  REAL, POINTER, DIMENSION(:) :: XDN_ROAD ! Fraction of road covered with snow 
  REAL, POINTER, DIMENSION(:) :: XDW_ROOF ! Fraction of roof covered with water 
  REAL, POINTER, DIMENSION(:) :: XDW_ROAD ! Fraction of road covered with water
  !
  REAL, POINTER, DIMENSION(:) :: XLESN_RF ! Latent heat flux over snow 
  REAL, POINTER, DIMENSION(:) :: XLESN_RD ! Latent heat flux over snow 
  REAL, POINTER, DIMENSION(:) :: XLEW_RD ! Latent heat flux over road (snow)
  REAL, POINTER, DIMENSION(:) :: XLEW_RF ! Latent heat flux over roof (snow)
  ! Urban hydrology
  !
  REAL, POINTER, DIMENSION(:)   :: XRUNOFF_WW        ! groundwater runoff into wastewater sewers (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XRUNOFF_SW        ! groundwater runoff into stormwater sewers (kg/m2/s) 
  REAL, POINTER, DIMENSION(:)   :: XDELTA_WGS_ROAD   ! Water storage on the whole road soil column surface and soil out-in (kg/m²) 
  REAL, POINTER, DIMENSION(:)   :: XDELTA_WGS_BLD    ! Water storage on the whole bld soil column surface and soil out-in  (kg/m²) 
  REAL, POINTER, DIMENSION(:)   :: XDELTA_WGS_GARDEN ! Water storage on the whole garden soil column surface and soil out-in (kg/m²) 
  REAL, POINTER, DIMENSION(:)   :: XDELTA_WSNOW_RD   ! Snow storage on road  
  REAL, POINTER, DIMENSION(:)   :: XDELTA_WSNOW_RF   ! Snow storage on roof  
  REAL, POINTER, DIMENSION(:)   :: XDELTA_WSNOW_GD   ! Snow storage on garden 
  REAL, POINTER, DIMENSION(:)   :: XHYDRO_BUD        ! Hydrological budget of town from the begining of simulation (OUT-IN)/IN*100 (%) 
  REAL, POINTER, DIMENSION(:)   :: XWAT_OUT          ! Cumulative water out from town at the end of simulation (kg/m²) 
  REAL, POINTER, DIMENSION(:)   :: XNOC_ROOF         ! runoff from roof portion not connected to sewer (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XNOC_ROAD         ! runoff from road portion not connected to sewer (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XRUNOFFSOIL_ROAD  ! lateral runoff in road soil column    (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XRUNOFFSOIL_BLD   ! lateral runoff in bld  soil column    (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XDRAIN_ROAD       ! drainage from ground under road (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XDRAIN_BLD        ! drainage from ground under bld (kg/m2/s)
 ! budget
  REAL, POINTER, DIMENSION(:)   :: XRAIN             !
  REAL, POINTER, DIMENSION(:)   :: XSNOW             !
  ! Reservoirs initialisation
  REAL, POINTER, DIMENSION(:,:) :: XWG_ROAD_INI 
  REAL, POINTER, DIMENSION(:,:) :: XWGI_ROAD_INI
  REAL, POINTER, DIMENSION(:,:) :: XWG_BLD_INI
  REAL, POINTER, DIMENSION(:,:) :: XWGI_BLD_INI
  REAL, POINTER, DIMENSION(:,:) :: XWG_GARDEN_INI
  REAL, POINTER, DIMENSION(:,:) :: XWGI_GARDEN_INI
  REAL, POINTER, DIMENSION(:)   :: XWS_ROAD_INI
  REAL, POINTER, DIMENSION(:)   :: XWS_ROOF_INI
  REAL, POINTER, DIMENSION(:)   :: XWS_GARDEN_INI
  REAL, POINTER, DIMENSION(:,:) :: XWSNOW_ROAD_INI
  REAL, POINTER, DIMENSION(:,:) :: XWSNOW_ROOF_INI
  REAL, POINTER, DIMENSION(:,:) :: XWSNOW_GARDEN_INI
!
  REAL, POINTER, DIMENSION(:)   :: XABS_SW_SKY       ! absorbed shortwave radiation by sky = reflected by the canyon
  REAL, POINTER, DIMENSION(:)   :: XABS_LW_SKY       ! absorbed shortwave radiation by sky = reflected by the canyon
  REAL, POINTER, DIMENSION(:)   :: XABS_SW_ROOF      ! absorbed shortwave radiation over roofs
  REAL, POINTER, DIMENSION(:)   :: XABS_SW_SNOW_ROOF ! absorbed shortgwave radiation over roofs
  REAL, POINTER, DIMENSION(:)   :: XABS_LW_ROOF      ! absorbed longwave radiation over roofs
  REAL, POINTER, DIMENSION(:)   :: XABS_LW_SNOW_ROOF ! absorbed longwave radiation over roofs
  REAL, POINTER, DIMENSION(:)   :: XDIR_SW_ROAD      ! received direct shortwave radiation over roads
  REAL, POINTER, DIMENSION(:)   :: XSCA_SW_ROAD      ! received scattered shortwave radiation over roads
  REAL, POINTER, DIMENSION(:)   :: XABS_SW_ROAD      ! absorbed shortwave radiation over roads
  REAL, POINTER, DIMENSION(:)   :: XABS_SW_SNOW_ROAD ! absorbed longwave radiation over roads
  REAL, POINTER, DIMENSION(:)   :: XABS_LW_ROAD      ! absorbed longwave radiation over roads
  REAL, POINTER, DIMENSION(:)   :: XABS_LW_SNOW_ROAD ! absorbed longwave radiation over roads
  REAL, POINTER, DIMENSION(:)   :: XDIR_SW_WALL      ! received direct shortwave radiation over walls
  REAL, POINTER, DIMENSION(:)   :: XDIR_SW_WALL_A    ! received direct shortwave radiation over wall A
  REAL, POINTER, DIMENSION(:)   :: XDIR_SW_WALL_B    ! received direct shortwave radiation over wall B
  REAL, POINTER, DIMENSION(:)   :: XSCA_SW_WALL      ! received scattered shortwave radiation over walls
  REAL, POINTER, DIMENSION(:)   :: XABS_SW_WALL_A    ! absorbed shortwave radiation over walls
  REAL, POINTER, DIMENSION(:)   :: XABS_SW_WALL_B    ! absorbed shortwave radiation over walls
  REAL, POINTER, DIMENSION(:)   :: XREC_SW_WALL      ! shortwave radiation received by walls  
  REAL, POINTER, DIMENSION(:)   :: XABS_LW_WALL_A    ! absorbed longwave radiation over walls
  REAL, POINTER, DIMENSION(:)   :: XABS_LW_WALL_B    ! absorbed longwave radiation over walls
  REAL, POINTER, DIMENSION(:)   :: XDIR_SW_GARDEN    ! received direct shortwave radiation over roads
  REAL, POINTER, DIMENSION(:)   :: XSCA_SW_GARDEN    ! received scattered shortwave radiation over roads
  REAL, POINTER, DIMENSION(:)   :: XREC_SW_GARDEN    ! received shortwave radiation over green areas
  REAL, POINTER, DIMENSION(:)   :: XREC_LW_GARDEN    ! received longwave radiation over green areas
  REAL, POINTER, DIMENSION(:)   :: XABS_SW_GARDEN    ! absorbed shortwave radiation over green areas
  REAL, POINTER, DIMENSION(:)   :: XABS_LW_GARDEN    ! absorbed longwave radiation over green areas
  REAL, POINTER, DIMENSION(:)   :: XABS_SW_GREENROOF ! absorbed shortwave radiation over green roofs
  REAL, POINTER, DIMENSION(:)   :: XABS_LW_GREENROOF ! absorbed shortwave radiation over green roofs
  REAL, POINTER, DIMENSION(:)   :: XG_GREENROOF_ROOF ! Heat flux between green roof and structural roof
  REAL, POINTER, DIMENSION(:)   :: XTS_PANEL         ! Surface temperature of solar panels 
  REAL, POINTER, DIMENSION(:)   :: XABS_SW_PANEL     ! absorbed shortwave radiation over solar panels
  REAL, POINTER, DIMENSION(:)   :: XABS_LW_PANEL     ! absorbed longwave  radiation over solar panels
  REAL, POINTER, DIMENSION(:)   :: XDIR_SW_HVEG      ! received direct shortwave radiation over high vegetation
  REAL, POINTER, DIMENSION(:)   :: XNTR_DIR_SW_HVEG  ! received direct shortwave radiation over hveg corrected from transmission
  REAL, POINTER, DIMENSION(:)   :: XSCA_SW_HVEG      ! received scattered Downward shortwave radiation over high vegetation
  REAL, POINTER, DIMENSION(:)   :: XREC_SW_HVEG      ! received shortwave radiation by high vegetation
  REAL, POINTER, DIMENSION(:)   :: XREC_LW_HVEG      ! received longwave radiation by high vegetation
  REAL, POINTER, DIMENSION(:)   :: XABS_SW_HVEG      ! absorbed shortwave radiation by high vegetation
  REAL, POINTER, DIMENSION(:)   :: XABS_LW_HVEG      ! absorbed longwave radiation by high vegetation
  REAL, POINTER, DIMENSION(:)   :: XNET_LW_HVEG      ! IR rad absorbed by high vegetation
  REAL, POINTER, DIMENSION(:)   :: XREC_SW_VEG       ! received shortwave radiation by urban vegetation (garden + high veg)
  REAL, POINTER, DIMENSION(:)   :: XREC_LW_VEG       ! received longwave radiation by urban vegetation (garden + high veg)
  REAL, POINTER, DIMENSION(:)   :: XSW_UP_ROOF       ! reflected shortwave radiation by roof
  REAL, POINTER, DIMENSION(:)   :: XSW_UP_CAN        ! reflected longwave radiation by the canyon
  REAL, POINTER, DIMENSION(:)   :: XLW_UP_ROOF       ! emitted shortwave radiation by roof
  REAL, POINTER, DIMENSION(:)   :: XLW_UP_CAN        ! emitted longwave radiation by the canyon
!
  REAL, POINTER, DIMENSION(:)   :: XTS_HVEG          ! Foliar surface temperature of urban high vegetation (K)
  REAL, POINTER, DIMENSION(:)   :: XTS_GD            ! Skin surface temperature of urban low vegetation (K)
  REAL, POINTER, DIMENSION(:)   :: XTS_GR            ! Skin surface temperature of green roofs (K)  
!
  REAL, POINTER, DIMENSION(:)   :: XRN_PANEL         ! net radiation           over solar panels (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XH_PANEL          ! sensible heat flux      over solar panels (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XTHER_PROD_PANEL  ! thermal      production of   solar panels (W/m2 thermal panel)
  REAL, POINTER, DIMENSION(:)   :: XPHOT_PROD_PANEL  ! photovoltaic production of   solar panels (W/m2 photovoltaic panel)
  REAL, POINTER, DIMENSION(:)   :: XPROD_PANEL       !              production of   solar panels (W/m2 panel)
  REAL, POINTER, DIMENSION(:)   :: XTHER_PROD_BLD    ! thermal      production of   solar panels (W/m2 bld)
  REAL, POINTER, DIMENSION(:)   :: XPHOT_PROD_BLD    ! photovoltaic production of   solar panels (W/m2 bld)

  REAL, POINTER, DIMENSION(:)   :: XH_BLD_COOL       ! Sensible cooling energy demand  
                                                     ! of the building [W m-2(bld)]
  REAL, POINTER, DIMENSION(:)   :: XLE_BLD_COOL      ! Latent cooling energy demand 
                                                     ! of the building [W m-2(bld)]
  REAL, POINTER, DIMENSION(:) :: XH_WASTE_CANY     ! Sensible waste heat released to canyon [W m-2(tot)]
  REAL, POINTER, DIMENSION(:) :: XLE_WASTE_CANY    ! Latent waste heat  released to canyon [W m-2(tot)]
  REAL, POINTER, DIMENSION(:) :: XH_WASTE_ROOF     ! Sensible waste heat released at roof level [W m-2(tot)]
  REAL, POINTER, DIMENSION(:) :: XLE_WASTE_ROOF    ! Latent waste heat  released at roof level [W m-2(tot)]
  REAL, POINTER, DIMENSION(:) :: XLE_EVAPORATIVE_COOLING ! Latent heat released due to evaporative cooling [W m-2(tot)] 
  REAL, POINTER, DIMENSION(:) :: XHVAC_COOL        ! Energy consumption of the cooling system [W m-2(urb)]
  REAL, POINTER, DIMENSION(:) :: XHVAC_HEAT        ! Energy consumption of the heating system [W m-2(urb)]
  REAL, POINTER, DIMENSION(:) :: XQINOUT           ! Internal energy release [W m-2(urb)]
  REAL, POINTER, DIMENSION(:) :: XQINOUTSEN        ! Internal energy release, sensible [W m-2(urb)]
  REAL, POINTER, DIMENSION(:) :: XQINOUTLAT        ! Internal energy release, latent [W m-2(urb)]  
  REAL, POINTER, DIMENSION(:) :: XHVAC_HEAT_ELEC   ! ELEC  Energy consumption of the heating system [W m-2(urb)]
  REAL, POINTER, DIMENSION(:) :: XHVAC_HEAT_GAS    ! GAS   Energy consumption of the heating system [W m-2(urb)]
  REAL, POINTER, DIMENSION(:) :: XHVAC_HEAT_FUEL   ! FUEL  Energy consumption of the heating system [W m-2(urb)]
  REAL, POINTER, DIMENSION(:) :: XHVAC_HEAT_OTHER  ! OTHER Energy consumption of the heating system [W m-2(urb)]
  REAL, POINTER, DIMENSION(:,:) :: XCOMP_HVAC_COOL   ! Energy consumption of the cooling system per compartment [W m-2(urb)]
  REAL, POINTER, DIMENSION(:,:) :: XCOMP_HVAC_HEAT   ! Energy consumption of the heating system per compartment [W m-2(urb)]
  REAL, POINTER, DIMENSION(:,:) :: XCOMP_QINOUT      ! Internal energy release per compartment  [W m-2(urb)]
  REAL, POINTER, DIMENSION(:) :: XH_TRAFFIC_OUT    ! Actual traffic sensible heat release [W m-2(urb)]
  REAL, POINTER, DIMENSION(:) :: XLE_TRAFFIC_OUT   ! Actual traffic latent   heat release [W m-2(urb)]
  REAL, POINTER, DIMENSION(:) :: XH_INDUSTRY_OUT   ! Actual industry sensible heat release [W m-2(urb)]
  REAL, POINTER, DIMENSION(:) :: XLE_INDUSTRY_OUT  ! Actual industry latent   heat release [W m-2(urb)]
  REAL, POINTER, DIMENSION(:) :: XSENFABSTOR       ! Sensible heat stored in urban fabric [W/m²(urb)]
  REAL, POINTER, DIMENSION(:) :: XLATFABSTOR       ! Latent heat stored on urban fabric [W/m²(urb)]
  REAL, POINTER, DIMENSION(:) :: XROOFTK           ! Total roof thickness [m]
  REAL, POINTER, DIMENSION(:) :: XWALLTK           ! Total wall thickness [m]
  REAL, POINTER, DIMENSION(:) :: XMASSTK           ! Total mass thickness [m]
  REAL, POINTER, DIMENSION(:) :: XU_LOWCAN         ! Wind speed at lowest level of canyon [m/s]
  REAL, POINTER, DIMENSION(:) :: XROAD_SHADE       ! Fraction of road shaded [1]
  REAL, POINTER, DIMENSION(:) :: XQIN_KWH          ! Internal heat release [kWh/m²(floor)]
  REAL, POINTER, DIMENSION(:) :: XHVAC_HT_KWH      ! Heating energy demand [kWh/m²(floor)]
  REAL, POINTER, DIMENSION(:) :: XHVAC_CL_KWH      ! Cooling energy demand [kWh/m²(floor)]
  REAL, POINTER, DIMENSION(:) :: XINFCALC          ! Calculated infiltration rate
  REAL, POINTER, DIMENSION(:,:) :: XDIAGSHAD         ! Diagnostic of shading usage
  REAL, POINTER, DIMENSION(:,:) :: XDIAGVENT         ! Diagnostic of ventilation status
  REAL, POINTER, DIMENSION(:,:) :: XDIAGVEFL         ! Diagnostic of ventilation exchange rate [1/h]
  REAL, POINTER, DIMENSION(:,:) :: XBLDOCC           ! Building occupation fraction [W m-2(bld)]
  REAL, POINTER, DIMENSION(:) :: XCAP_SYS            ! Actual capacity of the cooling system [W m-2(bld)] 
  REAL, POINTER, DIMENSION(:,:) :: XM_SYS            ! Actual HVAC mass flow rate [kg s-1 m-2(bld)]
  REAL, POINTER, DIMENSION(:) :: XCOP                ! COP of the cooling system
  REAL, POINTER, DIMENSION(:,:) :: XQ_SYS            ! Supply air specific humidity [kg kg-1]
  REAL, POINTER, DIMENSION(:,:) :: XT_SYS            ! Supply air temperature [K]
  REAL, POINTER, DIMENSION(:,:) :: XTR_SW_WIN        ! Solar radiation transmitted throught windows [W m-2(bld)]
  REAL, POINTER, DIMENSION(:) :: XABS_SW_WIN       ! window absorbed shortwave radiation [W m-2] 
  REAL, POINTER, DIMENSION(:) :: XABS_LW_WIN       ! absorbed infrared rad. [W m-2]
  REAL, POINTER, DIMENSION(:) :: XEMIT_LW_FAC      ! LW flux emitted by the facade (W/m2 facade)
  REAL, POINTER, DIMENSION(:) :: XEMIT_LW_GRND     ! LW flux emitted by the ground (W/m2 ground = road + garden)
  REAL, POINTER, DIMENSION(:,:) :: XT_RAD_IND        ! Indoor mean radiant temperature [K]
  REAL, POINTER, DIMENSION(:) :: XREF_SW_GRND      ! total solar rad reflected by ground
  REAL, POINTER, DIMENSION(:) :: XREF_SW_FAC       ! total solar rad reflected by facade
!
  REAL, POINTER, DIMENSION(:) :: XSFCO2          ! total CO2 flux over town         (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XSFCO2_VEG      ! CO2 flux over town link to vegetation (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XSFCO2_BLD      ! CO2 flux over town link to buildings  (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XSFCO2_RD       ! CO2 flux over town link to roads      (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XSFCO2_POP      ! CO2 flux over town link to people     (kg/m2/s)
  !
  REAL, POINTER, DIMENSION(:) :: XHOTWATOUT        ! Energy consumed for domestic warm water [W m-2(urb)]  
  REAL, POINTER, DIMENSION(:) :: XHOTWAT_GAS       ! gas consumption for domestic warm water [W m-2(urb)]
  REAL, POINTER, DIMENSION(:) :: XHOTWAT_ELEC      ! elec consumption for domestic warm water [W m-2(urb)]
  REAL, POINTER, DIMENSION(:,:) :: XCOMP_HOTWAT    ! Energy consumption for domestic warm water [W m-2(urb)]
!------------------------------------------------------------------------------
!

END TYPE DIAG_MISC_TEB_t

TYPE DIAG_MISC_TEB_NP_t
  !
  TYPE(DIAG_MISC_TEB_t), POINTER :: AL(:) => NULL()
  !
END TYPE DIAG_MISC_TEB_NP_t
!
CONTAINS
!
SUBROUTINE DIAG_MISC_TEB_INIT(YDIAG_MISC_TEB)
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: YDIAG_MISC_TEB
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_MISC_TEB_N:DIAG_MISC_TEB_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YDIAG_MISC_TEB%XZ0_TOWN)
  NULLIFY(YDIAG_MISC_TEB%XQF_BLD)
  NULLIFY(YDIAG_MISC_TEB%XFLX_BLD)
  NULLIFY(YDIAG_MISC_TEB%XQF_TOWN)
  NULLIFY(YDIAG_MISC_TEB%XDQS_TOWN)
  NULLIFY(YDIAG_MISC_TEB%XMELT_BLT)
  NULLIFY(YDIAG_MISC_TEB%XMELT_BLD)
  NULLIFY(YDIAG_MISC_TEB%XMELT_RD)
  NULLIFY(YDIAG_MISC_TEB%XH_WALL_A)
  NULLIFY(YDIAG_MISC_TEB%XH_WALL_B)
  NULLIFY(YDIAG_MISC_TEB%XLE_WALL_A)
  NULLIFY(YDIAG_MISC_TEB%XLE_WALL_B)
  NULLIFY(YDIAG_MISC_TEB%XH_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XH_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XH_HVEG)
  NULLIFY(YDIAG_MISC_TEB%XH_STRLROOF)
  NULLIFY(YDIAG_MISC_TEB%XH_BLT)
  NULLIFY(YDIAG_MISC_TEB%XRN_HVEG)
  NULLIFY(YDIAG_MISC_TEB%XRN_WALL_A)
  NULLIFY(YDIAG_MISC_TEB%XRN_WALL_B)
  NULLIFY(YDIAG_MISC_TEB%XRN_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XRN_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XRN_STRLROOF)
  NULLIFY(YDIAG_MISC_TEB%XRN_BLT)
  NULLIFY(YDIAG_MISC_TEB%XGFLUX_HVEG)
  NULLIFY(YDIAG_MISC_TEB%XGFLUX_WALL_A)
  NULLIFY(YDIAG_MISC_TEB%XGFLUX_WALL_B)
  NULLIFY(YDIAG_MISC_TEB%XGFLUX_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XGFLUX_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XGFLUX_STRLROOF)
  NULLIFY(YDIAG_MISC_TEB%XGFLUX_BLT)
  NULLIFY(YDIAG_MISC_TEB%XLE_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XLE_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XLE_HVEG)
  NULLIFY(YDIAG_MISC_TEB%XLE_STRLROOF)
  NULLIFY(YDIAG_MISC_TEB%XLE_BLT)
  NULLIFY(YDIAG_MISC_TEB%XQF_WALL_A)
  NULLIFY(YDIAG_MISC_TEB%XQF_WALL_B)
  NULLIFY(YDIAG_MISC_TEB%XQF_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XQF_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XQF_BLT)
  NULLIFY(YDIAG_MISC_TEB%XSNOW_HEAT_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XSNOW_HEAT_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XRUNOFF_TOWN)
  NULLIFY(YDIAG_MISC_TEB%XIRRIG_GARDEN)
  NULLIFY(YDIAG_MISC_TEB%XRUNOFF_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XIRRIG_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XRUNOFF_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XRUNOFF_STRLROOF)
  NULLIFY(YDIAG_MISC_TEB%XIRRIG_GREENROOF)
  NULLIFY(YDIAG_MISC_TEB%XSWI)
  NULLIFY(YDIAG_MISC_TEB%XRUNOFF_WW)
  NULLIFY(YDIAG_MISC_TEB%XRUNOFF_SW)
  NULLIFY(YDIAG_MISC_TEB%XDELTA_WGS_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XDELTA_WGS_BLD)
  NULLIFY(YDIAG_MISC_TEB%XDELTA_WGS_GARDEN)
  NULLIFY(YDIAG_MISC_TEB%XHYDRO_BUD)
  NULLIFY(YDIAG_MISC_TEB%XWAT_OUT)  
  NULLIFY(YDIAG_MISC_TEB%XNOC_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XNOC_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XDRAIN_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XDRAIN_BLD)
  NULLIFY(YDIAG_MISC_TEB%XRUNOFFSOIL_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XRUNOFFSOIL_BLD)
  NULLIFY(YDIAG_MISC_TEB%XRAIN)
  NULLIFY(YDIAG_MISC_TEB%XSNOW)
  NULLIFY(YDIAG_MISC_TEB%XWG_ROAD_INI)
  NULLIFY(YDIAG_MISC_TEB%XWG_BLD_INI)
  NULLIFY(YDIAG_MISC_TEB%XWG_GARDEN_INI)
  NULLIFY(YDIAG_MISC_TEB%XWGI_ROAD_INI)
  NULLIFY(YDIAG_MISC_TEB%XWGI_BLD_INI)
  NULLIFY(YDIAG_MISC_TEB%XWGI_GARDEN_INI)
  NULLIFY(YDIAG_MISC_TEB%XWS_ROAD_INI)
  NULLIFY(YDIAG_MISC_TEB%XWS_ROOF_INI)
  NULLIFY(YDIAG_MISC_TEB%XWS_GARDEN_INI)
  NULLIFY(YDIAG_MISC_TEB%XWSNOW_ROAD_INI)
  NULLIFY(YDIAG_MISC_TEB%XWSNOW_ROOF_INI)
  NULLIFY(YDIAG_MISC_TEB%XWSNOW_GARDEN_INI)
  NULLIFY(YDIAG_MISC_TEB%XABS_SW_SKY)
  NULLIFY(YDIAG_MISC_TEB%XABS_LW_SKY)
  NULLIFY(YDIAG_MISC_TEB%XABS_SW_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XABS_SW_SNOW_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XABS_LW_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XABS_LW_SNOW_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XDIR_SW_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XSCA_SW_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XABS_SW_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XABS_SW_SNOW_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XABS_LW_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XABS_LW_SNOW_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XDIR_SW_WALL)
  NULLIFY(YDIAG_MISC_TEB%XDIR_SW_WALL_A)
  NULLIFY(YDIAG_MISC_TEB%XDIR_SW_WALL_B)
  NULLIFY(YDIAG_MISC_TEB%XSCA_SW_WALL)
  NULLIFY(YDIAG_MISC_TEB%XABS_SW_WALL_A)
  NULLIFY(YDIAG_MISC_TEB%XABS_SW_WALL_B)
  NULLIFY(YDIAG_MISC_TEB%XREC_SW_WALL)  
  NULLIFY(YDIAG_MISC_TEB%XABS_LW_WALL_A)
  NULLIFY(YDIAG_MISC_TEB%XABS_LW_WALL_B)
  NULLIFY(YDIAG_MISC_TEB%XREC_SW_GARDEN)
  NULLIFY(YDIAG_MISC_TEB%XREC_LW_GARDEN)
  NULLIFY(YDIAG_MISC_TEB%XABS_SW_GARDEN)
  NULLIFY(YDIAG_MISC_TEB%XABS_LW_GARDEN)
  NULLIFY(YDIAG_MISC_TEB%XALB_GD)
  NULLIFY(YDIAG_MISC_TEB%FSNOW_GD)
  NULLIFY(YDIAG_MISC_TEB%XAC_AGG_GD)
  NULLIFY(YDIAG_MISC_TEB%XAC_AGG_GR)
  NULLIFY(YDIAG_MISC_TEB%XABS_SW_GREENROOF)
  NULLIFY(YDIAG_MISC_TEB%XABS_LW_GREENROOF)
  NULLIFY(YDIAG_MISC_TEB%XG_GREENROOF_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XTS_PANEL)
  NULLIFY(YDIAG_MISC_TEB%XABS_SW_PANEL)
  NULLIFY(YDIAG_MISC_TEB%XABS_LW_PANEL)
  NULLIFY(YDIAG_MISC_TEB%XDIR_SW_HVEG)
  NULLIFY(YDIAG_MISC_TEB%XNTR_DIR_SW_HVEG)
  NULLIFY(YDIAG_MISC_TEB%XSCA_SW_HVEG)
  NULLIFY(YDIAG_MISC_TEB%XREC_SW_HVEG)
  NULLIFY(YDIAG_MISC_TEB%XREC_LW_HVEG)
  NULLIFY(YDIAG_MISC_TEB%XABS_SW_HVEG)
  NULLIFY(YDIAG_MISC_TEB%XABS_LW_HVEG)
  NULLIFY(YDIAG_MISC_TEB%XNET_LW_HVEG)
  NULLIFY(YDIAG_MISC_TEB%XTS_HVEG)
  NULLIFY(YDIAG_MISC_TEB%XTS_GD)
  NULLIFY(YDIAG_MISC_TEB%XTS_GR)  
  NULLIFY(YDIAG_MISC_TEB%XREC_SW_VEG)
  NULLIFY(YDIAG_MISC_TEB%XREC_LW_VEG)
  NULLIFY(YDIAG_MISC_TEB%XSW_UP_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XSW_UP_CAN)
  NULLIFY(YDIAG_MISC_TEB%XLW_UP_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XLW_UP_CAN)
  NULLIFY(YDIAG_MISC_TEB%XRN_PANEL)
  NULLIFY(YDIAG_MISC_TEB%XH_PANEL)
  NULLIFY(YDIAG_MISC_TEB%XTHER_PROD_PANEL)
  NULLIFY(YDIAG_MISC_TEB%XPHOT_PROD_PANEL)
  NULLIFY(YDIAG_MISC_TEB%XPROD_PANEL)
  NULLIFY(YDIAG_MISC_TEB%XTHER_PROD_BLD)
  NULLIFY(YDIAG_MISC_TEB%XPHOT_PROD_BLD)
  NULLIFY(YDIAG_MISC_TEB%XH_BLD_COOL)
  NULLIFY(YDIAG_MISC_TEB%XLE_BLD_COOL)
  NULLIFY(YDIAG_MISC_TEB%XHVAC_COOL)
  NULLIFY(YDIAG_MISC_TEB%XHVAC_HEAT)
  NULLIFY(YDIAG_MISC_TEB%XHVAC_HEAT_ELEC)
  NULLIFY(YDIAG_MISC_TEB%XHVAC_HEAT_GAS)
  NULLIFY(YDIAG_MISC_TEB%XHVAC_HEAT_FUEL)
  NULLIFY(YDIAG_MISC_TEB%XHVAC_HEAT_OTHER)
  NULLIFY(YDIAG_MISC_TEB%XCOMP_HVAC_COOL)
  NULLIFY(YDIAG_MISC_TEB%XCOMP_HVAC_HEAT)
  NULLIFY(YDIAG_MISC_TEB%XH_WASTE_CANY)
  NULLIFY(YDIAG_MISC_TEB%XLE_WASTE_CANY)
  NULLIFY(YDIAG_MISC_TEB%XH_WASTE_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XLE_WASTE_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XLE_EVAPORATIVE_COOLING)  
  NULLIFY(YDIAG_MISC_TEB%XQINOUT)
  NULLIFY(YDIAG_MISC_TEB%XQINOUTSEN)
  NULLIFY(YDIAG_MISC_TEB%XQINOUTLAT)  
  NULLIFY(YDIAG_MISC_TEB%XCOMP_QINOUT)
  NULLIFY(YDIAG_MISC_TEB%XH_TRAFFIC_OUT)
  NULLIFY(YDIAG_MISC_TEB%XLE_TRAFFIC_OUT)
  NULLIFY(YDIAG_MISC_TEB%XH_INDUSTRY_OUT)
  NULLIFY(YDIAG_MISC_TEB%XLE_INDUSTRY_OUT)
  NULLIFY(YDIAG_MISC_TEB%XSENFABSTOR)
  NULLIFY(YDIAG_MISC_TEB%XLATFABSTOR)
  NULLIFY(YDIAG_MISC_TEB%XROOFTK)
  NULLIFY(YDIAG_MISC_TEB%XWALLTK)
  NULLIFY(YDIAG_MISC_TEB%XMASSTK)
  NULLIFY(YDIAG_MISC_TEB%XU_LOWCAN)
  NULLIFY(YDIAG_MISC_TEB%XROAD_SHADE)
  NULLIFY(YDIAG_MISC_TEB%XQIN_KWH)
  NULLIFY(YDIAG_MISC_TEB%XHVAC_HT_KWH)
  NULLIFY(YDIAG_MISC_TEB%XHVAC_CL_KWH)
  NULLIFY(YDIAG_MISC_TEB%XINFCALC)
  NULLIFY(YDIAG_MISC_TEB%XDIAGSHAD)
  NULLIFY(YDIAG_MISC_TEB%XDIAGVENT)
  NULLIFY(YDIAG_MISC_TEB%XDIAGVEFL)
  NULLIFY(YDIAG_MISC_TEB%XBLDOCC)
  NULLIFY(YDIAG_MISC_TEB%XCAP_SYS)
  NULLIFY(YDIAG_MISC_TEB%XM_SYS)
  NULLIFY(YDIAG_MISC_TEB%XCOP)
  NULLIFY(YDIAG_MISC_TEB%XQ_SYS)
  NULLIFY(YDIAG_MISC_TEB%XT_SYS)
  NULLIFY(YDIAG_MISC_TEB%XTR_SW_WIN)
  NULLIFY(YDIAG_MISC_TEB%XABS_SW_WIN)
  NULLIFY(YDIAG_MISC_TEB%XABS_LW_WIN)
  NULLIFY(YDIAG_MISC_TEB%XEMIT_LW_GRND)
  NULLIFY(YDIAG_MISC_TEB%XEMIT_LW_FAC)
  NULLIFY(YDIAG_MISC_TEB%XT_RAD_IND)
  NULLIFY(YDIAG_MISC_TEB%XREF_SW_GRND)
  NULLIFY(YDIAG_MISC_TEB%XREF_SW_FAC)
  NULLIFY(YDIAG_MISC_TEB%XSFCO2)
  NULLIFY(YDIAG_MISC_TEB%XSFCO2_VEG)
  NULLIFY(YDIAG_MISC_TEB%XSFCO2_BLD)
  NULLIFY(YDIAG_MISC_TEB%XSFCO2_RD)
  NULLIFY(YDIAG_MISC_TEB%XSFCO2_POP)
  NULLIFY(YDIAG_MISC_TEB%XHOTWATOUT)
  NULLIFY(YDIAG_MISC_TEB%XHOTWAT_GAS)
  NULLIFY(YDIAG_MISC_TEB%XHOTWAT_ELEC)
  NULLIFY(YDIAG_MISC_TEB%XCOMP_HOTWAT)
  NULLIFY(YDIAG_MISC_TEB%XDN_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XDN_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XDW_ROOF)
  NULLIFY(YDIAG_MISC_TEB%XDW_ROAD)
  NULLIFY(YDIAG_MISC_TEB%XLESN_RF)
  NULLIFY(YDIAG_MISC_TEB%XLESN_RD)
  NULLIFY(YDIAG_MISC_TEB%XLEW_RD)
  NULLIFY(YDIAG_MISC_TEB%XLEW_RF)
  !  
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_MISC_TEB_N:DIAG_MISC_TEB_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DIAG_MISC_TEB_INIT
!
SUBROUTINE DIAG_MISC_TEB_NP_INIT(YNDIAG_MISC_TEB,KPATCH)
TYPE(DIAG_MISC_TEB_NP_t), INTENT(INOUT) :: YNDIAG_MISC_TEB
INTEGER, INTENT(IN) :: KPATCH
INTEGER :: JP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_MISC_TEB_N:DIAG_MISC_TEB_NP_INIT",0,ZHOOK_HANDLE)
IF (.NOT.ASSOCIATED(YNDIAG_MISC_TEB%AL)) THEN
  ALLOCATE(YNDIAG_MISC_TEB%AL(KPATCH))
  DO JP=1,KPATCH
    CALL DIAG_MISC_TEB_INIT(YNDIAG_MISC_TEB%AL(JP))
  ENDDO
ELSE
  DO JP=1,KPATCH
    CALL DIAG_MISC_TEB_INIT(YNDIAG_MISC_TEB%AL(JP))
  ENDDO
  DEALLOCATE(YNDIAG_MISC_TEB%AL)
ENDIF  
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_MISC_TEB_N:DIAG_MISC_TEB_NP_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DIAG_MISC_TEB_NP_INIT



END MODULE MODD_DIAG_MISC_TEB_n
