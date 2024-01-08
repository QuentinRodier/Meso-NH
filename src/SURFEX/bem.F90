!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   ##########################################################################
  SUBROUTINE BEM (BOP, T, B, DMT, CT, JCOMP, HPROGRAM, PTSTEP, KDAY, PPS, PPSOLD, PRHOA, PT_CAN,  &
       PQ_CAN, PU_CAN, PEFF_HEAT, PRADHT_IN, PLOAD_IN_FL, PLOAD_IN_MA,   &
       PCONV_RF_BLD, PCONV_WL_BLD, PCONV_WIN_BLD, PRAD_RF_MA, PRAD_RF_FL, &
       PRAD_WL_MA, PRAD_WL_FL, PRAD_WIN_MA,                                   &
       PRAD_WIN_FL, HNATVENT, PVENT_BEHAV_ANYWAY, PVENT_BEHAV_ADAPTI,      &
       PTCOOL_TARGET,PTHEAT_TARGET, PQIN,                                     &
       PH_BLD_COOL, PLE_BLD_COOL, PHVAC_COOL, PHVAC_HEAT, PH_WASTE_CANY,      &
       PLE_WASTE_CANY, PH_WASTE_ROOF, PLE_WASTE_ROOF, PLE_EVAPORATIVE_COOLING, &
       PFLX_BLD_FL,PFLX_BLD_MA,PINFCALC,PRHOI_NEW,PDIAGVENT,PDIAGVEFL,   &
       PCST_H_WASTE_CANY,PCST_LE_WASTE_CANY,PCOEFF_H_WASTE_CANY,              &
       PCOEFF_LE_WASTE_CANY,PFRAC_HEAT_LE,PG_FLOOR, PDIAG_DCS_AREA, HTEST)
!   ##########################################################################
!
!!****  *BEM*
!!
!!    PURPOSE
!!    -------
!
!     Computes the temperature and humidity evolution of indoor air, 
!     building energy demand, HVAC energy consumption, 
!     waste heat from HVAC systems, and heat fluxes from indoor to building surfaces.
!
!
!!**  METHOD
!     ------
!
!              NOMENCLATURE: bld  - refers to building plant area; 
!                            floor- refers to building plant area multiplied 
!                                   by the number of floors;
!                            wall - refers to wall area (excluding windows).
!                            win  - refers to window area. 
!                            mass - refers to internal mass area. 
!
!
!        solar radiation transmitted through windows
!        *******************************************
!
!     Qsol_tr_win = Qsol_facade * tr_win * GR 
!
!
!        indoor wall conv/rad heat transfer coefficients
!        ***********************************************
!
!     The calculation of CHTC accounts for favorable or unfavorable convection 
!     depending on the relative position between the hot layer and cold layer
!
! 
!        building energy demand
!        **********************
!
!     Calculation of the cooling and heating, sensible and latent building energy demand.
!     The sensible demand includes the convective heat transfer from indoor surfaces, the 
!     convective fraction of internal heat gains, and sensible infiltration/ventilation heat
!     gains. The latent demand includes the latent fraction of internal heat gains and latent
!     infiltration/ventilation heat gains.  
!
!        surface areas and volummes (referred to m2_bld)
!        ***********************************************
!
!     Awall   =  WALL_O_HOR * (1 - GR) / BLD [m2_wall/m2_bld]
!     Awin    =  WALL_O_HOR * GR / BLD       [m2_win/m2_bld]   
!     Amass   =  2 * N_FLOOR                  [m2_mass/m2_bld]  
!     N_FLOOR  =  BLD_HEIGHT / FLOOR_HEIGHT   [#]
!     Aroof   =  1                           [m2_roof/m2_bld]  
!     Afloor  =  1                           [m2_floor/m2_bld]   
!     Vol_air =  BLD_HEIGHT                  [m3_bld/m2_bld]
!
!
!        evolution of the internal temperature
!        *************************************
!

!                                  dTin  
!     Vol_air * ro_air * cp_air * ---- = h_wall * Awall * (Twall - Tin)
!                                   dt    + h_roof * Aroof * (Troof -Tin)
!                                         + h_floor * Afloor *(Tfloor - Tin)
!                                         + h_mass * Amass * (Tmass - Tin)  
!                                         + h_win * Awin * (Twin - Tin)
!                                         + Qig * (1 - fig_rad) * (1-fig_lat)
!                                         + Vinf * ro_air * cp_air * (Tout - Tin) 
!                                         + Vsys * ro_air * cp_air * (Tsys - Tin) 
!
!
!        evolution of the internal specific humidity
!        *******************************************
!
!                                  dQin  
!      Vol_air * ro_air * lv_air * ---- = Qig * fig_lat
!                                   dt    + Vinf * ro_air * lv_air * (Qout - Qin) 
!                                         + Vsys * ro_air * lv_air * (Qsys - Qin) 
!
!
!        heat fluxes from indoor to surfaces
!        ***********************************
!
!      Qin_wall  = h_wall  * (Tin - Twall)  [W/m2_wall]
!      Qin_roof  = h_roof  * (Tin - Troof)  [W/m2_roof] 
!      Qin_floor = h_floor * (Tin - Tfloor) [W/m2_floor] 
!      Qin_mass  = h_wall  * (Tin - Tmass)  
!                + Qig * fig_rad * (1-fig_lat)/ 2  
!                + Qsol_tr_win              [W/m2_mass]
!
!
!        energy consumption and waste heat from cooling system
!        *****************************************************
!
!      Qhvac  = Qbld / COP
!      Qwaste = Qbld + Qhvac
!
!
!        energy consumption and waste heat from heating system
!        *****************************************************
!
!      Qhvac  = Qbld / Eff
!      Qwaste = Qhvac - Qbld
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
!!      B. Bueno           * Meteo-France *
!!
!!!    MODIFICATIONS
!!    -------------
!!     Original 2010
!!     G. Pigeon nov. 2011: inclusion floor/mass budget inside
!!                          add automatic/manual ventilation
!!                          conserve exchanges with the different surfaces inside 1 time step
!!    G. Pigeon sept. 2012: use of TARP/DOE coef for indoor convection
!!                          use of both T%XT_WALL_A and T%XT_WALL_B for calculations
!!                          the internal mass depth is 1/2 of the floor depth
!!                          add the option of no atmospheric heat releases by HVAC system (B%XF_WATER_COND < 0)
!!    G. Pigeon oct. 2012:  use indoor air density + new solar heat gain distribution
!!    V. Masson May  2013   implicitation of internal building temperature evolution
!!    V. Masson Mars 2016   soil column under buildings
!!    M. Goret  Avr. 2017   replace if statement for HEAT_COIL by a SELECT CASE statement, and use a vectorize syntax
!!    M. Goret  Avr. 2017   idem for COOL_COIL, add an IDEAL2 case to test the vectorize syntax
!!    M. Goret  mar. 2017   add a latent heat waste for heating
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
USE MODD_CHECK_TEB, ONLY : CHECK_TEB_t
!
USE MODD_CSTS, ONLY : XCPD, XRD, XRV, XLVTT, XSURF_EPSILON
USE MODE_THERMOS
USE MODE_PSYCHRO
USE MODI_DX_AIR_COOLING_COIL_CV
USE MODI_FLOOR_LAYER_E_BUDGET
USE MODI_MASS_LAYER_E_BUDGET
USE MODE_CONV_DOE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
USE MODI_ABOR1_SFX
USE MODI_GET_LUOUT
!
IMPLICIT NONE
!
!*      0.1    Declarations of arguments
!
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DMT
TYPE(CHECK_TEB_t), INTENT(INOUT) :: CT
!
CHARACTER(LEN=2),    INTENT(IN)   :: HTEST         ! must be equal to 'OK'
REAL,                INTENT(IN)   :: PTSTEP        ! Time step
CHARACTER(LEN=6),    INTENT(IN)   :: HPROGRAM      ! program calling surf. schemes
INTEGER,             INTENT(IN)   :: KDAY          ! Simulation day
INTEGER,          INTENT(IN) :: JCOMP      ! Loop counter for compartments
!
REAL, DIMENSION(:),   INTENT(IN)  :: PPS          ! Canyon air pressure [Pa]
REAL, DIMENSION(:),   INTENT(IN)  :: PPSOLD       ! Canyon air pressure at previous time step [Pa]
REAL, DIMENSION(:),   INTENT(IN)  :: PRHOA        ! Air density at the lowest level [kg m-3]
REAL, DIMENSION(:),   INTENT(IN)  :: PT_CAN    ! Canyon air temperature [K]
REAL, DIMENSION(:),   INTENT(IN)  :: PQ_CAN    ! Canyon air specific humidity [kg kg-1]
REAL, DIMENSION(:),   INTENT(IN)  :: PU_CAN    ! Canyon wind speed (m s-1)
REAL, DIMENSION(:),   INTENT(IN)  :: PEFF_HEAT
!
REAL, DIMENSION(:),   INTENT(IN)  :: PRADHT_IN ! Indoor radiant heat transfer coefficient [W K-1 m-2]
CHARACTER(LEN=4), DIMENSION(:), INTENT(IN) :: HNATVENT
REAL, DIMENSION(:),   INTENT(IN)  :: PVENT_BEHAV_ANYWAY
REAL, DIMENSION(:),   INTENT(IN)  :: PVENT_BEHAV_ADAPTI
REAL, DIMENSION(:),   INTENT(IN)  :: PTCOOL_TARGET
REAL, DIMENSION(:),   INTENT(IN)  :: PTHEAT_TARGET
REAL, DIMENSION(:),   INTENT(IN)  :: PQIN
REAL, DIMENSION(:),   INTENT(IN)  :: PFRAC_HEAT_LE
!
REAL, DIMENSION(:)  , INTENT(IN)  :: PRAD_RF_MA  ! Rad. fluxes between roof and mass
REAL, DIMENSION(:)  , INTENT(IN)  :: PRAD_RF_FL ! Rad. fluxes between roof and floor
REAL, DIMENSION(:)  , INTENT(IN)  :: PRAD_WL_MA  ! Rad. fluxes between wall and mass
REAL, DIMENSION(:)  , INTENT(IN)  :: PRAD_WL_FL ! Rad. fluxes between wall and floor
REAL, DIMENSION(:)  , INTENT(IN)  :: PRAD_WIN_MA   ! Rad. fluxes between wind. and mass
REAL, DIMENSION(:)  , INTENT(IN)  :: PRAD_WIN_FL  ! Rad. fluxes between wind. and floor
REAL, DIMENSION(:)  , INTENT(IN)  :: PCONV_RF_BLD  ! Conv. fluxes between roof and indoor air
REAL, DIMENSION(:)  , INTENT(IN)  :: PCONV_WL_BLD  ! Conv. fluxes between wall and indoor air
REAL, DIMENSION(:)  , INTENT(IN)  :: PCONV_WIN_BLD   ! Conv. fluxes between wind. and indoor air
REAL, DIMENSION(:)  , INTENT(IN)  :: PLOAD_IN_FL  ! solar + int heat gain on floor W/m² [floor]
REAL, DIMENSION(:)  , INTENT(IN)  :: PLOAD_IN_MA   ! solar + int heat gain on floor W/m² [mass]
!
REAL, DIMENSION(:)  , INTENT(OUT) :: PFLX_BLD_FL! Heat flux from indoor air to floor [W m-2(bld)]
REAL, DIMENSION(:)  , INTENT(OUT) :: PFLX_BLD_MA ! Heat flux from indoor air to mass [W m-2(bld)]
!
REAL, DIMENSION(:)  , INTENT(OUT) :: PHVAC_COOL
REAL, DIMENSION(:)  , INTENT(OUT) :: PHVAC_HEAT
!
REAL, DIMENSION(:)  , INTENT(OUT) :: PH_BLD_COOL
REAL, DIMENSION(:)  , INTENT(OUT) :: PLE_BLD_COOL
!
REAL, DIMENSION(:)  , INTENT(OUT) :: PH_WASTE_CANY
REAL, DIMENSION(:)  , INTENT(OUT) :: PLE_WASTE_CANY
REAL, DIMENSION(:)  , INTENT(OUT) :: PH_WASTE_ROOF
REAL, DIMENSION(:)  , INTENT(OUT) :: PLE_WASTE_ROOF
REAL, DIMENSION(:)  , INTENT(OUT) :: PLE_EVAPORATIVE_COOLING
!
REAL, DIMENSION(:)  , INTENT(OUT) :: PCST_H_WASTE_CANY
REAL, DIMENSION(:)  , INTENT(OUT) :: PCST_LE_WASTE_CANY
REAL, DIMENSION(:)  , INTENT(OUT) :: PCOEFF_H_WASTE_CANY
REAL, DIMENSION(:)  , INTENT(OUT) :: PCOEFF_LE_WASTE_CANY 
!
REAL, DIMENSION(:)  , INTENT(OUT) :: PRHOI_NEW
REAL, DIMENSION(:)  , INTENT(OUT) :: PINFCALC
REAL, DIMENSION(:)  , INTENT(OUT) :: PDIAGVEFL      ! Diagnostic of ventilation exchange rate [1/h]
REAL, DIMENSION(:)  , INTENT(OUT) :: PDIAGVENT      ! Is ventilation active?
REAL, DIMENSION(:)  , INTENT(OUT) :: PG_FLOOR       ! Flux between compartiment floor and soil below (W/m-2(comp))
REAL, DIMENSION(:)  , INTENT(OUT) :: PDIAG_DCS_AREA
!
!*      0.2    Declarations of local variables 
!
INTEGER :: IRF     ! Number of roof layers
INTEGER :: IWL     ! Number of wall layers
INTEGER :: JJ      ! Loop counter
INTEGER :: ILUOUT  ! Unit number
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZAC_IN_MA_COOL
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZAC_IN_FL_COOL
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZAC_IN_RF_COOL
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZAC_IN_WL_A_COOL
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZAC_IN_WL_B_COOL
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZAC_IN_WIN_COOL   
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZAC_IN_MA_HEAT
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZAC_IN_FL_HEAT
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZAC_IN_RF_HEAT
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZAC_IN_WL_A_HEAT
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZAC_IN_WL_B_HEAT
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZAC_IN_WIN_HEAT
!
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZCHTC_IN_FLOOR
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZTS_FLOOR_CONV
!
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZSRCCONVWALL
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZSRCCONVGLAZ
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZSRCCONVMASS
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZSRCCONVROOF
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZSRCCONVFLOO
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZSRCQINSENHE
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZSRCQINLATHE
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZSRCSENINFVE
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZSRCLATINFVE
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZSRCSENHHVAC
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZSRCLATHHVAC
!
REAL, DIMENSION(SIZE(B%XTI_BLD,1)):: ZQIN          ! Internal heat gains [W m-2(bld)]
REAL, DIMENSION(SIZE(B%XTI_BLD,1)):: ZRHOI
REAL, DIMENSION(SIZE(B%XTI_BLD,1)):: ZINF
REAL, DIMENSION(SIZE(B%XTI_BLD,1)):: ZTV_BLD_NEW  
REAL, DIMENSION(SIZE(B%XTI_BLD,1)):: ZTV_BLD_OLD 
REAL, DIMENSION(SIZE(B%XTI_BLD,1)):: ZTI_BLD_OLD
REAL, DIMENSION(SIZE(B%XTI_BLD,1)):: ZQI_BLD_OLD 
REAL, DIMENSION(SIZE(B%XTI_BLD,1)):: ZTI_BLD_OPEN
REAL, DIMENSION(SIZE(B%XTI_BLD,1)):: ZTI_BLD_CLOSED
REAL, DIMENSION(SIZE(B%XTI_BLD,1)):: ZNAT_VENT
REAL, DIMENSION(SIZE(B%XTI_BLD,1)):: ZT_MIX
REAL, DIMENSION(SIZE(B%XTI_BLD,1)):: ZQ_MIX
REAL, DIMENSION(SIZE(B%XTI_BLD,1)):: ZQCOOL_TARGET
REAL, DIMENSION(SIZE(B%XTI_BLD,1)):: ZQHEAT_TARGET  
REAL, DIMENSION(SIZE(B%XTI_BLD,1)):: ZWASTE
!
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZDIFFAIRSEN
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZDIFFAIRLAT
!
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZTHEAIRIN_NEW
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZTHEAIRIN_OLD
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZLATAIRIN_NEW
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZLATAIRIN_OLD
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZDIFFSENAIRIN
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZDIFFLATAIRIN
!
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZUP_HVAC_COOL
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZFRAC_VENT
!
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZSHR ! Rated sensible heat rate
!
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZPSATCOOL
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZPSATHEAT
!
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZAUX_H_WASTE_ROOF
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZIMB_LATH_BLD
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZIMB_SENH_BLD
!
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZDQS_FL
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZDQS_MA
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZDIFFAIRMASS
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZRHOI_OLD
!
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZRAD_FL_MA
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZCONV_MA_BLD
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZCONV_FL_BLD
!
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZH_BLD_HEAT
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZLE_BLD_HEAT 
!
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZM_SYS_RAT
REAL, DIMENSION(SIZE(B%XTI_BLD,1)) :: ZXMIX
!
REAL :: ZINFI
REAL :: ZMECH
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('BEM',0,ZHOOK_HANDLE)
!
IF (HTEST/='OK') THEN
 CALL ABOR1_SFX('BEM: FATAL ERROR DURING ARGUMENT TRANSFER')
ENDIF
!
! ###########################################################
! 1.   Initializations
! ###########################################################
!
! Array dimensions
!
IRF=SIZE(T%XT_ROOF,2)
IWL=SIZE(T%XT_WALL_A,2)
!
ZNAT_VENT (:) = 0.0
!
PH_WASTE_CANY  (:) = 0.0
PLE_WASTE_CANY (:) = 0.0
PH_WASTE_ROOF  (:) = 0.0
PLE_WASTE_ROOF (:) = 0.0
PDIAG_DCS_AREA (:) = 0.0
PLE_EVAPORATIVE_COOLING(:) = 0.0
!
! Robert: Calculation of infiltration rate as a function of
!         building characteristics and meteorological conditions
!
CALL GET_INFILTRATION(PRHOA,B%XN50,B%XTI_BLD(:,JCOMP),PT_CAN,PU_CAN,T%XBLD_HEIGHT,PINFCALC)
!
! Conversion AC/H -> [m3 s-1 m-2(bld)]
!
ZINF(:)=PINFCALC(:)*T%XBLD_HEIGHT(:)/3600
!
! For the case where a permanent mechanical ventilation system is present,
! the ventilation due to this mechanical ventilation is added to the infiltration
! rate using the sum of squares. 
! This assumes that the two processes dont influence each other,
! which might not always be justified.
!
DO JJ=1,SIZE(B%XISMECH,1)
   IF (B%XISMECH(JJ).GT.0.5) THEN
      !
      ZMECH=B%XMECHRATE(JJ)*T%XBLD_HEIGHT(JJ)/3600.
      ZINFI=ZINF(JJ)
      !
      ZINF(JJ)=SQRT(ZMECH**2+ZINFI**2)
      PINFCALC(JJ)=3600.0*ZINF(JJ)/T%XBLD_HEIGHT(JJ)
      !
   ENDIF
ENDDO
!
! Conversion W/m²/floor -> W/m²(bld)
!
ZQIN(:)=PQIN(:)*B%XN_FLOOR(:)
!
! Indoor air density
!
! Robert: Use pressure from old time step in order to be consistant
!         with temperature and humidity before update
!
ZRHOI(:)=PPSOLD(:)/(XRD*B%XTI_BLD(:,JCOMP)*(1.+((XRV/XRD)-1.)*B%XQI_BLD(:,JCOMP)))
!
!###########################################################
! 2.   heat balance for building floor and mass
!###########################################################
!
! 2.1 FLOOR HEAT BALANCE
!
CALL FLOOR_LAYER_E_BUDGET(B, T, CT, HPROGRAM, PTSTEP, JCOMP, PFLX_BLD_FL, ZDQS_FL, PRADHT_IN, &
       PRAD_WL_FL, PRAD_RF_FL, PRAD_WIN_FL, PLOAD_IN_FL,    &
       ZRAD_FL_MA, ZCONV_FL_BLD, ZCHTC_IN_FLOOR, ZTS_FLOOR_CONV, PG_FLOOR)
!
!
! 2.2 MASS HEAT BALANCE
!
CALL MASS_LAYER_E_BUDGET(B, CT, HPROGRAM, PTSTEP, JCOMP, PFLX_BLD_MA, ZDQS_MA, PRADHT_IN, &
     PRAD_WL_MA, PRAD_RF_MA, PRAD_WIN_MA, PLOAD_IN_MA,    &
     ZRAD_FL_MA, ZCONV_MA_BLD  )
!
ZAC_IN_WL_A_COOL = CHTC_VERT_DOE(T%XT_WALL_A(:,IWL), PTCOOL_TARGET)
ZAC_IN_WL_B_COOL = CHTC_VERT_DOE(T%XT_WALL_B(:,IWL), PTCOOL_TARGET)
ZAC_IN_WIN_COOL  = CHTC_VERT_DOE(B%XT_WIN2         , PTCOOL_TARGET)
ZAC_IN_MA_COOL   = CHTC_VERT_DOE(B%XT_MASS(:,1,JCOMP), PTCOOL_TARGET)
ZAC_IN_RF_COOL   = CHTC_DOWN_DOE(T%XT_ROOF(:,IRF)  , PTCOOL_TARGET)
ZAC_IN_FL_COOL   = CHTC_UP_DOE  (B%XT_FLOOR(:,1,JCOMP), PTCOOL_TARGET)
!
ZAC_IN_WL_A_HEAT = CHTC_VERT_DOE(T%XT_WALL_A(:,IWL), PTHEAT_TARGET)
ZAC_IN_WL_B_HEAT = CHTC_VERT_DOE(T%XT_WALL_B(:,IWL), PTHEAT_TARGET)
ZAC_IN_WIN_HEAT  = CHTC_VERT_DOE(B%XT_WIN2         , PTHEAT_TARGET)
ZAC_IN_MA_HEAT   = CHTC_VERT_DOE(B%XT_MASS(:,1,JCOMP), PTHEAT_TARGET)
ZAC_IN_RF_HEAT   = CHTC_DOWN_DOE(T%XT_ROOF(:,IRF)  , PTHEAT_TARGET)
ZAC_IN_FL_HEAT   = CHTC_UP_DOE  (B%XT_FLOOR(:,1,JCOMP), PTHEAT_TARGET)
!
! Calculation of ventilation exchange rate before loop
!
CALL GET_NAT_VENT(B%XTI_BLD(:,JCOMP), PT_CAN, PU_CAN, B%XGLAZ_O_BLD, ZNAT_VENT, B%XFOPEN)
!
DO JJ=1,SIZE(PT_CAN)
   !
   ! Impose lower limit for coefficients
   !
   ZAC_IN_WL_A_COOL(JJ) = MAX(1.,ZAC_IN_WL_A_COOL(JJ))
   ZAC_IN_WL_B_COOL(JJ) = MAX(1.,ZAC_IN_WL_B_COOL(JJ))
   ZAC_IN_WIN_COOL(JJ)  = MAX(1.,ZAC_IN_WIN_COOL(JJ))
   ZAC_IN_MA_COOL(JJ)   = MAX(1.,ZAC_IN_MA_COOL(JJ))
   ZAC_IN_RF_COOL(JJ)   = MAX(1.,ZAC_IN_RF_COOL(JJ))
   ZAC_IN_FL_COOL(JJ)   = MAX(1.,ZAC_IN_FL_COOL(JJ))
   !   
   ZAC_IN_WL_A_HEAT(JJ) = MAX(1.,ZAC_IN_WL_A_HEAT(JJ))
   ZAC_IN_WL_B_HEAT(JJ) = MAX(1.,ZAC_IN_WL_B_HEAT(JJ))
   ZAC_IN_WIN_HEAT(JJ)  = MAX(1.,ZAC_IN_WIN_HEAT(JJ))
   ZAC_IN_MA_HEAT(JJ)   = MAX(1.,ZAC_IN_MA_HEAT(JJ))
   ZAC_IN_RF_HEAT(JJ)   = MAX(1.,ZAC_IN_RF_HEAT(JJ))
   ZAC_IN_FL_HEAT(JJ)   = MAX(1.,ZAC_IN_FL_HEAT(JJ))
   !
   ! Calculation of ventilation depending on choice of HNATVENT
   !
   IF (HNATVENT(JJ)=='NONE') THEN
      !
      ! No ventilation
      !
      PDIAGVENT(JJ)=0.0
      !
   ELSEIF (HNATVENT(JJ)=='AUTO') THEN
      !
      ! Automatic opening of windows
      !
      ! Calculation of hypothetical air temperature 
      ! inside opened or closed buildings.
      !
      ZTI_BLD_CLOSED(JJ) = ( B%XTI_BLD(JJ,JCOMP)             + &
           PTSTEP/(ZRHOI(JJ)*XCPD*T%XBLD_HEIGHT(JJ))         * &
           ( T%XWALL_O_BLD(JJ) * PCONV_WL_BLD(JJ)          + &
           B%XGLAZ_O_BLD (JJ)* PCONV_WIN_BLD(JJ)             + &
           B%XMASS_O_BLD(JJ) * ZCONV_MA_BLD(JJ)            + &
           PCONV_RF_BLD(JJ)                                + &
           ZCONV_FL_BLD(JJ)                               + &
           ZQIN(JJ)*(1-B%XQIN_FRAD(JJ))*(1-B%XQIN_FLAT(JJ)) )    + &
           ZINF(JJ)*PTSTEP/T%XBLD_HEIGHT(JJ)*PT_CAN(JJ) )   / &
           (1.0+ZINF(JJ)*PTSTEP/T%XBLD_HEIGHT(JJ))
      !
      ZTI_BLD_OPEN(JJ)   = ( B%XTI_BLD(JJ,JCOMP)         + &
           PTSTEP/(ZRHOI(JJ)*XCPD*T%XBLD_HEIGHT(JJ))     * &
           ( T%XWALL_O_BLD(JJ) * PCONV_WL_BLD(JJ)                           + &
           B%XGLAZ_O_BLD (JJ)* PCONV_WIN_BLD(JJ)                            + &
           B%XMASS_O_BLD(JJ) * ZCONV_MA_BLD(JJ)                           + &
           PCONV_RF_BLD(JJ)                           + &
           ZCONV_FL_BLD(JJ)                          + &
           ZQIN(JJ)*(1-B%XQIN_FRAD(JJ))*(1-B%XQIN_FLAT(JJ)) )                 + &
           (ZNAT_VENT(JJ)+ZINF(JJ))*PTSTEP/T%XBLD_HEIGHT(JJ)*PT_CAN(JJ) )/ &
           (1.0+(ZNAT_VENT(JJ)+ZINF(JJ))*PTSTEP/T%XBLD_HEIGHT(JJ))
      !
      ! Robert: Ventilation is made if it permits to reduce the difference 
      !         to the ventilation design temperature and if neither heating 
      !         nor cooling would be required for the ventilated building
      !         The automatic ventilation is independend of the building 
      !         occupation and therefore effectuated in exactly the same
      !         for all building in a urban district (binary switch)
      !
      IF ( ( ABS(ZTI_BLD_OPEN(JJ) - B%XTDESV(JJ))     .LT.  &
           ABS(ZTI_BLD_CLOSED(JJ) - B%XTDESV(JJ)) )   .AND. &
           ( ZTI_BLD_OPEN(JJ) .LT. PTCOOL_TARGET(JJ) ) .AND. &
           ( ZTI_BLD_OPEN(JJ) .GT. PTHEAT_TARGET(JJ) ) ) THEN
         !
         PDIAGVENT(JJ)=1.0
         !
      ELSE
         PDIAGVENT(JJ)=0.0
      END IF
      !
      ! Manual ventilation
      !
   ELSEIF (HNATVENT(JJ)=='MANU') THEN
      !
      ZFRAC_VENT(JJ) = (1.0 / (1.0 + EXP(-1.0*( B%XTI_BLD (JJ,JCOMP) - B%XTDESV  (JJ)-2.0)))) * &
                       (1.0 / (1.0 + EXP(-1.0*( B%XTI_BLD (JJ,JCOMP) - PT_CAN(JJ)   ))))
      !
      PDIAGVENT(JJ) = PVENT_BEHAV_ANYWAY(JJ) + ZFRAC_VENT(JJ) * PVENT_BEHAV_ADAPTI(JJ)
      !
      ! Ventilation fractions below 0.2 are set to 0.0 since ventilation 
      ! is very non-linear and interpheres with heating
      !
      IF (PDIAGVENT(JJ).LT.0.2) PDIAGVENT(JJ)=0.0
      !
      IF ((PDIAGVENT(JJ).LT.-XSURF_EPSILON).OR.(PDIAGVENT(JJ).GT.(1.0+XSURF_EPSILON))) THEN
         CALL GET_LUOUT(HPROGRAM,ILUOUT)
         WRITE(ILUOUT,*) "                                  "
         WRITE(ILUOUT,*) "In bem: wrong ventilation fraction"
         WRITE(ILUOUT,*) "PDIAGVENT(JJ) ",PDIAGVENT(JJ)
         CALL FLUSH(ILUOUT)
         CALL ABOR1_SFX ("BEM: Wrong ventilation fraction, check report")
      ENDIF
      !
   ELSE
      CALL ABOR1_SFX("This ventilation case is not implemented")
   ENDIF
   !
ENDDO
!
!################################################################################
! Calculation of heating and cooling energy demand
!################################################################################
!
! Re-calculation of ventilation rate
!
CALL GET_NAT_VENT(B%XTI_BLD(:,JCOMP),PT_CAN,PU_CAN,B%XGLAZ_O_BLD,ZNAT_VENT,B%XFOPEN)
!
! In case of manual ventilation, the ventilation flow rate is multiplied
! by the fraction of windows that have been opened
!
DO JJ=1,SIZE(PDIAGVENT)
    IF (HNATVENT(JJ)=='MANU') ZNAT_VENT(JJ) = ZNAT_VENT(JJ) * PDIAGVENT(JJ)
ENDDO
!
! If natural or mechanical surventilation ACTIVE
!
DO JJ=1,SIZE(PDIAGVENT)
   IF (PDIAGVENT(JJ).GT.0.0) THEN
      !
      PH_BLD_COOL (JJ) = 0.0
      ZH_BLD_HEAT (JJ) = 0.0
      PLE_BLD_COOL(JJ) = 0.0
      ZLE_BLD_HEAT(JJ) = 0.0
      !    
      PHVAC_COOL(JJ)       = 0.0
      DMT%XT_SYS(JJ,JCOMP) = B%XTI_BLD(JJ,JCOMP)
      DMT%XQ_SYS(JJ,JCOMP) = B%XQI_BLD(JJ,JCOMP)
      !
      PH_WASTE_CANY  (JJ) = 0.0
      PLE_WASTE_CANY (JJ) = 0.0
      PH_WASTE_ROOF  (JJ) = 0.0
      PLE_WASTE_ROOF (JJ) = 0.0
      PLE_EVAPORATIVE_COOLING(JJ) = 0.0
      !
      PHVAC_HEAT (JJ) = 0.0
      !
      DMT%XM_SYS  (JJ,JCOMP) = 0.0
      DMT%XCOP    (JJ) = 0.0
      DMT%XCAP_SYS(JJ) = 0.0
      !
   ENDIF
ENDDO
!
! *If natural surventilation INACTIVE
!
WHERE (PDIAGVENT(:).LE.0.0)
   !
   ZNAT_VENT(:) = 0.
   !
   ! ------------------------------------------------
   ! Building energy demand for heating and cooling
   ! ------------------------------------------------
   !
   PH_BLD_COOL(:)                                                                              &
        = 0.5*T%XWALL_O_BLD(:)*(ZAC_IN_WL_A_COOL(:) * (T%XT_WALL_A(:,IWL)- PTCOOL_TARGET(:)) &
        +                       ZAC_IN_WL_B_COOL(:) * (T%XT_WALL_B(:,IWL)- PTCOOL_TARGET(:)))&
        +     B%XGLAZ_O_BLD(:)* ZAC_IN_WIN_COOL(:)    * (B%XT_WIN2(:)      - PTCOOL_TARGET(:)) &
        +     B%XMASS_O_BLD(:)* ZAC_IN_MA_COOL(:)   * (B%XT_MASS(:,1,JCOMP)- PTCOOL_TARGET(:)) &
        +                       ZAC_IN_RF_COOL(:)   * (T%XT_ROOF(:,IRF)  - PTCOOL_TARGET(:)) &
        +                       ZAC_IN_FL_COOL(:)  * (B%XT_FLOOR(:,1,JCOMP)   - PTCOOL_TARGET(:)) &
        + ZQIN(:)*(1.0-B%XQIN_FRAD(:))*(1.0-B%XQIN_FLAT(:))                                        &
        + ZINF(:)*XCPD*ZRHOI(:)*(PT_CAN(:)-PTCOOL_TARGET(:))
   !
   ZH_BLD_HEAT(:)                                                                                    &
        = - ( 0.5*T%XWALL_O_BLD(:) * (ZAC_IN_WL_A_HEAT(:)*(T%XT_WALL_A(:,IWL)-PTHEAT_TARGET(:))   &
        +                             ZAC_IN_WL_B_HEAT(:)*(T%XT_WALL_B(:,IWL)-PTHEAT_TARGET(:)) ) &
        +         B%XGLAZ_O_BLD(:) *  ZAC_IN_WIN_HEAT(:)   *(B%XT_WIN2(:)      -PTHEAT_TARGET(:))   &    
        +         B%XMASS_O_BLD(:) *  ZAC_IN_MA_HEAT(:)  *(B%XT_MASS(:,1,JCOMP)    -PTHEAT_TARGET(:))   &
        +                             ZAC_IN_RF_HEAT(:)  *(T%XT_ROOF(:,IRF)  -PTHEAT_TARGET(:))   &
        +                             ZAC_IN_FL_HEAT(:) *(B%XT_FLOOR(:,1,JCOMP)   -PTHEAT_TARGET(:))   &
        + ZQIN(:)*(1.0-B%XQIN_FRAD(:))*(1.0-B%XQIN_FLAT(:))                                            &
        + ZINF(:)*XCPD*ZRHOI(:)*(PT_CAN(:)-PTHEAT_TARGET(:)) )
   !
   ! The design specific humidity is calculated based on 
   ! the desing temperature and the design relative humidity
   !
   ZPSATCOOL(:) = PSAT(PTCOOL_TARGET(:))
   ZPSATHEAT(:) = PSAT(PTHEAT_TARGET(:))
   !
   ZQCOOL_TARGET(:) = 0.62198*B%XHR_TARGET(:)*ZPSATCOOL(:) / &
        (PPS(:)-B%XHR_TARGET(:)*ZPSATCOOL(:))
   !
   ZQHEAT_TARGET(:) = 0.62198*B%XHR_TARGET(:)*ZPSATHEAT(:) / &
        (PPS(:)-B%XHR_TARGET(:)*ZPSATHEAT(:))
   !
   ! The latent heat required to reach the target specific humidity is calculated
   !
   PLE_BLD_COOL(:) = ZQIN(:)*B%XQIN_FLAT(:)+ZINF(:)*XLVTT*(PQ_CAN(:)-ZQCOOL_TARGET(:))
   !
   ! No target humidity for heating
   !
   ZLE_BLD_HEAT(:) = 0.0
   !
   ! ZLE_BLD_HEAT(:)=ZQIN(:)*PQIN_FLAT(:)+ZINF(:)*ZRHOI(:)*XLVTT*(PQ_CAN(:)-ZQHEAT_TARGET(:))
   !
ENDWHERE
!
! Autosize calculations
!
IF (BOP%LAUTOSIZE.AND.(KDAY.EQ.15)) THEN
   !
   DO JJ=1,SIZE(PH_BLD_COOL)
      !
      IF (PH_BLD_COOL(JJ) .GT. B%XAUX_MAX(JJ))  THEN
         !
         B%XAUX_MAX    (JJ) = PH_BLD_COOL(JJ)
         !
         ! Cooling coil sensible heat rate 
         !
         ZSHR(JJ) = MIN(XCPD*(PTCOOL_TARGET(JJ)-B%XT_ADP(JJ))/     &
              (ENTH_FN_T_Q(PTCOOL_TARGET(JJ),ZQCOOL_TARGET(JJ)) -  &
               ENTH_FN_T_Q(B%XT_ADP(JJ),QSAT(B%XT_ADP(JJ),PPS(JJ)))), 1.)
         !
         ! Cooling Coil Capacity [W m-2(bld)]
         !
         B%XCAP_SYS_RAT(JJ)=PH_BLD_COOL(JJ)/ZSHR(JJ) 
         !
         ! Cooling rated air flow rate [kg s-1 m-2(bld)]
         !
         ZM_SYS_RAT(JJ)=PH_BLD_COOL(JJ)/XCPD/(PTCOOL_TARGET(JJ)-(14.0+273.16))
         IF (ZM_SYS_RAT(JJ).GT.B%XM_SYS_RAT(JJ)) B%XM_SYS_RAT(JJ)=ZM_SYS_RAT(JJ)
         !
      END IF
      !
      ! Impose a minimum and maximum value of the cooling system capacity
      !
      B%XCAP_SYS_RAT(JJ) = MAX(B%XCAP_SYS_RAT(JJ),(B%XM_SYS_RAT(JJ)/ZRHOI(JJ)/0.00004027))
      B%XCAP_SYS_RAT(JJ) = MIN(B%XCAP_SYS_RAT(JJ),(B%XM_SYS_RAT(JJ)/ZRHOI(JJ)/0.00006041))
      !
   ENDDO
   !
END IF
!
! End of autosize calculations
!
! Calculation of system efficiency
!
DMT%XM_SYS  (:,JCOMP) = B%XM_SYS_RAT  (:)
DMT%XCOP    (:) = B%XCOP_RAT    (:)
DMT%XCAP_SYS(:) = B%XCAP_SYS_RAT(:)
!
! Calculation of mixing conditions
! Robert: At the moment the mixing conditions are equal
!         to the indoor conditions since there is no 
!         cooling/heating when there is ventilation.
!
ZXMIX (:) = 0.0*ZRHOI(:)/DMT%XM_SYS(:,JCOMP)
ZT_MIX(:) = ZXMIX(:)*PT_CAN(:)+(1.-ZXMIX(:))*B%XTI_BLD(:,JCOMP)
ZQ_MIX(:) = ZXMIX(:)*PQ_CAN(:)+(1.-ZXMIX(:))*B%XQI_BLD(:,JCOMP)
!
! ########################################### 
! Calculations related to cooling
! Only for grid points with cooling required
! ###########################################
!
! Calculation of cooling system performance and waste heat
!
SELECT CASE (BOP%CCOOL_COIL)
  CASE('IDEAL')
     DO JJ=1,SIZE(PH_BLD_COOL)
        IF (PH_BLD_COOL(JJ).GT.0.0) THEN
          !
          ! Ideal system
          !
          PHVAC_COOL(JJ)=PH_BLD_COOL(JJ)/B%XCOP_RAT(JJ)
          !
          DMT%XT_SYS(JJ,JCOMP) = ZT_MIX(JJ) - PH_BLD_COOL(JJ)/DMT%XM_SYS(JJ,JCOMP)/XCPD
          DMT%XQ_SYS(JJ,JCOMP) = ZQ_MIX(JJ)
          !
          ! On part of the waste heat from climatisation is ejected into
          ! the canyon, the other part at the roof.
          !
          PH_WASTE_CANY (JJ) = B%XF_WASTE_CAN(JJ)*(PHVAC_COOL(JJ)*(1.+B%XCOP_RAT(JJ))*(1.-B%XF_WATER_COND(JJ)))
          PLE_WASTE_CANY(JJ) = B%XF_WASTE_CAN(JJ)*(PHVAC_COOL(JJ)*(1.+B%XCOP_RAT(JJ))*B%XF_WATER_COND(JJ))
          !
          PLE_WASTE_ROOF(JJ) = (1.0-B%XF_WASTE_CAN(JJ))*(PHVAC_COOL(JJ)*(1.+B%XCOP_RAT(JJ))*B%XF_WATER_COND(JJ))
          !
       ENDIF
     ENDDO
   !
  CASE('DXCOIL')
   DO JJ=1,SIZE(PH_BLD_COOL)
      IF (PH_BLD_COOL(JJ).GT.0.0) THEN
         !
         IF (B%X_DCS_AREA(JJ).LE.0.5) THEN
            !
            CALL DX_AIR_COOLING_COIL_CV(PT_CAN(JJ), PQ_CAN(JJ), PPS(JJ),                    &
               ZRHOI(JJ), ZT_MIX(JJ), ZQ_MIX(JJ), B%XCOP_RAT(JJ), B%XF_WASTE_CAN(JJ),       &
               B%XCAP_SYS_RAT(JJ), B%XT_ADP(JJ), B%XF_WATER_COND(JJ),                       &
               DMT%XM_SYS(JJ,JCOMP), PH_BLD_COOL(JJ), PH_WASTE_CANY(JJ),PLE_WASTE_CANY(JJ), &
               ZAUX_H_WASTE_ROOF(JJ),PLE_WASTE_ROOF(JJ), DMT%XCOP(JJ), DMT%XCAP_SYS(JJ),    &
               DMT%XT_SYS(JJ,JCOMP),DMT%XQ_SYS(JJ,JCOMP),PHVAC_COOL(JJ)                     )
            !
         ELSE
            !
            ! Idealised calculation of air conditioning, but with different COP
            ! than for traditional air conditioning systems
            !
            DMT%XCOP(JJ) = B%XCOP_DCS(JJ)
            !
            PHVAC_COOL(JJ)=PH_BLD_COOL(JJ)/DMT%XCOP(JJ)
            !
            DMT%XT_SYS(JJ,JCOMP) = ZT_MIX(JJ) - PH_BLD_COOL(JJ)/DMT%XM_SYS(JJ,JCOMP)/XCPD
            DMT%XQ_SYS(JJ,JCOMP) = ZQ_MIX(JJ)
            !
         ENDIF
         !
      ENDIF
   ENDDO
  CASE DEFAULT
    WRITE(ILUOUT,*) "In bem: The following cooling system is not implemented"   
    WRITE(ILUOUT,*) "BOP%CCOOL_COIL",BOP%CCOOL_COIL
    CALL FLUSH(ILUOUT)
    CALL ABOR1_SFX("BEM:This type of cooling system is not implemented")
END SELECT
!
! Cooling system without atmospheric releases.
! NOTE: In this case the energy leaving the system would need to
!       be considered.
!
DO JJ=1,SIZE(PH_BLD_COOL)
   IF (PH_BLD_COOL(JJ).GT.0.0) THEN
      !
      IF(B%X_DCS_AREA(JJ).GT.0.5) THEN
         PH_WASTE_CANY(JJ)  = 0.
         PLE_WASTE_CANY(JJ) = 0.
         PLE_WASTE_ROOF(JJ) = 0.
         PLE_EVAPORATIVE_COOLING(JJ) = 0.0
      ENDIF
      !
      ! From EP Engineering Reference (p. 647)
      !
      ZH_BLD_HEAT (JJ) = 0.0
      ZLE_BLD_HEAT(JJ) = 0.0
      PHVAC_HEAT  (JJ) = 0.0
      !
   ENDIF
ENDDO
!
! ---------------------------------------------
! * HEATING system : Performance and Waste heat
! ---------------------------------------------
!
IF (MAXVAL(ZH_BLD_HEAT).GT.0.0) THEN
   !
   ! If the heating system is not idal, the delivered 
   ! heating is limited to the capacity of the system
   !
   SELECT CASE (BOP%CHEAT_COIL)
      CASE('IDEAL')
      CASE('FINCAP')
          ZH_BLD_HEAT=MIN(ZH_BLD_HEAT, B%XCAP_SYS_HEAT)
      CASE DEFAULT
         CALL ABOR1_SFX("Uncorrect value for HHEAT_COIL: "//BOP%CHEAT_COIL)
   END SELECT
   !
   WHERE (ZH_BLD_HEAT(:).GT.0.0)
      !
      DMT%XT_SYS(:,JCOMP) = ZT_MIX(:) + ZH_BLD_HEAT(:)/DMT%XM_SYS(:,JCOMP)/XCPD
      DMT%XQ_SYS(:,JCOMP) = ZQ_MIX(:)
      !
      PHVAC_HEAT(:)=ZH_BLD_HEAT(:)/PEFF_HEAT(:)
      !
      ! No cooling
      ! 
      PH_BLD_COOL (:)=0.0
      PLE_BLD_COOL(:)=0.0
      PHVAC_COOL  (:)=0.0
      !
   END WHERE
   !
ENDIF
!
! ------------------------------
! * NEITHER COOLING NOR HEATING
! ------------------------------
!
WHERE ( (ZH_BLD_HEAT(:).LE.0.0).AND.(PH_BLD_COOL(:).LE.0.0) )
   !
   PH_BLD_COOL (:) = 0.0
   ZH_BLD_HEAT (:) = 0.0
   PLE_BLD_COOL(:) = 0.0
   ZLE_BLD_HEAT(:) = 0.0
   PHVAC_COOL  (:) = 0.0
   PHVAC_HEAT  (:) = 0.0
   !
   DMT%XT_SYS (:,JCOMP) = ZT_MIX(:)
   DMT%XQ_SYS (:,JCOMP) = ZQ_MIX(:)
   DMT%XM_SYS (:,JCOMP) = 0.0
   !
   PH_WASTE_CANY  (:) = 0.0
   PLE_WASTE_CANY (:) = 0.0
   PH_WASTE_ROOF  (:) = 0.0
   PLE_WASTE_ROOF (:) = 0.0
   PLE_EVAPORATIVE_COOLING(:) = 0.0
   !
ENDWHERE
!
!
!---------------------------------------------------------------------------------
! ENERGY DEMAND COMPUTED
!################################################################################
!
! EVOLUTION OF THE INTERNAL TEMPERATURE AND HUMIDITY
!
! Waste fluxes to canyon with respect to the old canyon temperature
!
ZWASTE(:) = ZINF(:) + ZNAT_VENT(:)
!
! Save indoor temperature and virtual temperature before update and humidity
!
ZTV_BLD_OLD(:)=B%XTI_BLD(:,JCOMP)*(1.+((XRV/XRD)-1.)*B%XQI_BLD(:,JCOMP))
ZTI_BLD_OLD(:)=B%XTI_BLD(:,JCOMP)
ZQI_BLD_OLD(:)=B%XQI_BLD(:,JCOMP)
!
ZRHOI_OLD(:)=ZRHOI(:)
!
! Calculate the total sensible and latent heat stored in
! the indoor air before update [J/m²(urb)]
!
ZTHEAIRIN_OLD(:) = XCPD *T%XBLD(:)*T%XBLD_HEIGHT(:)*ZRHOI_OLD(:)*B%XTI_BLD(:,JCOMP)
ZLATAIRIN_OLD(:) = XLVTT*T%XBLD(:)*T%XBLD_HEIGHT(:)*ZRHOI_OLD(:)*B%XQI_BLD(:,JCOMP)
!
! Update of indoor temperature
!
B%XTI_BLD(:,JCOMP) = ( B%XTI_BLD(:,JCOMP)              + &
     PTSTEP/(ZRHOI(:)*XCPD*T%XBLD_HEIGHT(:))           * &
     ( T%XWALL_O_BLD(:) * PCONV_WL_BLD(:)            + &
     B%XGLAZ_O_BLD(:) * PCONV_WIN_BLD(:)               + &
     B%XMASS_O_BLD(:) * ZCONV_MA_BLD(:)              + &
     PCONV_RF_BLD(:)                                + &
     ZCONV_FL_BLD(:)                               + &
     ZQIN(:)*(1.0-B%XQIN_FRAD(:))*(1.0-B%XQIN_FLAT(:)) )+ &
     PTSTEP/T%XBLD_HEIGHT(:)    *                         &
     ((ZINF(:)+ZNAT_VENT(:)) * PT_CAN(:) +   &
     DMT%XM_SYS(:,JCOMP)/ZRHOI(:)      * (DMT%XT_SYS   (:,JCOMP)) ) )  /  &
     (1. + PTSTEP/T%XBLD_HEIGHT(:)                      * &
     (ZINF(:) + ZNAT_VENT(:) + DMT%XM_SYS(:,JCOMP)/ZRHOI(:)) )
!
! Update of indoor humidity
!
B%XQI_BLD(:,JCOMP) = ( B%XQI_BLD(:,JCOMP) +  PTSTEP/T%XBLD_HEIGHT(:) * &
     (  ZQIN(:)*B%XQIN_FLAT(:)/(ZRHOI(:)*XLVTT)        &
     + (ZINF(:)+ZNAT_VENT(:))*PQ_CAN(:) &
     +  DMT%XM_SYS(:,JCOMP)/ZRHOI(:)   *(DMT%XQ_SYS   (:,JCOMP)) )    )  &
     / ( 1. + PTSTEP/T%XBLD_HEIGHT(:)*                   &
     (ZINF(:) + ZNAT_VENT(:) + DMT%XM_SYS(:,JCOMP)/ZRHOI(:)) )
!
! Robert: Include diagnostic of ventilation exchange rate [1/h]
!
PDIAGVEFL(:)=3600.*ZNAT_VENT(:)/T%XBLD_HEIGHT(:)
!
! Indoor air density at new time step
!
PRHOI_NEW(:)=PPS(:)/(XRD*B%XTI_BLD(:,JCOMP)*(1.+((XRV/XRD)-1.)*B%XQI_BLD(:,JCOMP)))
!
! Calculate the total sensible and latent heat stored in
! the indoor air after update [J/m²(urb)]
!
ZTHEAIRIN_NEW(:) = XCPD *T%XBLD(:)*T%XBLD_HEIGHT(:)*PRHOI_NEW(:)*B%XTI_BLD(:,JCOMP)
ZLATAIRIN_NEW(:) = XLVTT*T%XBLD(:)*T%XBLD_HEIGHT(:)*PRHOI_NEW(:)*B%XQI_BLD(:,JCOMP)
!
! Robert: Recalculation of heating and cooling demand after update
!
WHERE (ZH_BLD_HEAT(:).GT.0.0)
  !
  PH_WASTE_ROOF(:)  = PHVAC_HEAT(:) - ZH_BLD_HEAT(:) -  XCPD*DMT%XM_SYS(:,JCOMP) * ( ZTI_BLD_OLD(:) - B%XTI_BLD(:,JCOMP) )
  PLE_WASTE_ROOF(:) =                                - XLVTT*DMT%XM_SYS(:,JCOMP) * ( ZQI_BLD_OLD(:) - B%XQI_BLD(:,JCOMP) )
  !
  PLE_WASTE_ROOF(:) = PLE_WASTE_ROOF(:) + ZH_BLD_HEAT(:) * PFRAC_HEAT_LE(:)
  PH_WASTE_ROOF(:)  = PH_WASTE_ROOF(:)  - ZH_BLD_HEAT(:) * PFRAC_HEAT_LE(:)
  !
ENDWHERE
!
DO JJ=1,SIZE(PH_BLD_COOL)
   !
   IF (PH_BLD_COOL(JJ).GT.0.0) THEN
      !
      ZUP_HVAC_COOL(JJ) = XCPD*DMT%XM_SYS(JJ,JCOMP)*(B%XTI_BLD(JJ,JCOMP)-DMT%XT_SYS(JJ,JCOMP))/DMT%XCOP(JJ)
      !
      ! Distinction between air conditioning release into the air
      ! in the form of sensible or latent heat (0<=PF_WATER_COND<=1)
      ! or district cooling systems (PF_DCS_AREA>0.5) for which the energy
      ! is released into a river or the ocean. The loss of energy due to district cooling
      ! systems is not taken into account at the moment.
      !
      IF (B%X_DCS_AREA(JJ).LT.0.5) THEN
         !
         ! Sensible heat
         !
         PH_WASTE_CANY(JJ) = B%XF_WASTE_CAN(JJ)*(ZUP_HVAC_COOL(JJ)*(1.+DMT%XCOP(JJ))*(1.-B%XF_WATER_COND(JJ)))
         PH_WASTE_CANY(JJ) = PH_WASTE_CANY(JJ) - B%XF_WASTE_CAN(JJ)*(ZUP_HVAC_COOL(JJ)-PHVAC_COOL(JJ))
         !
         PH_WASTE_ROOF(JJ) = (1.0-B%XF_WASTE_CAN(JJ))*(ZUP_HVAC_COOL(JJ)*(1.+DMT%XCOP(JJ))*(1.-B%XF_WATER_COND(JJ)))
         PH_WASTE_ROOF(JJ) = PH_WASTE_ROOF(JJ) - (1.0-B%XF_WASTE_CAN(JJ))*(ZUP_HVAC_COOL(JJ)-PHVAC_COOL(JJ))
         !
         ! Latent heat
         !
         PLE_WASTE_CANY(JJ)=B%XF_WASTE_CAN(JJ)*(ZUP_HVAC_COOL(JJ)*(1.+DMT%XCOP(JJ))*B%XF_WATER_COND(JJ))
         PLE_WASTE_CANY(JJ)=PLE_WASTE_CANY(JJ) - B%XF_WASTE_CAN(JJ)* &
              (XLVTT*DMT%XM_SYS(JJ,JCOMP)*(DMT%XQ_SYS(JJ,JCOMP)-B%XQI_BLD(JJ,JCOMP)))
         !
         PLE_WASTE_ROOF(JJ)=(1.0-B%XF_WASTE_CAN(JJ))*(ZUP_HVAC_COOL(JJ)*(1.+DMT%XCOP(JJ))*B%XF_WATER_COND(JJ))
         PLE_WASTE_ROOF(JJ)=PLE_WASTE_ROOF(JJ) - (1.0-B%XF_WASTE_CAN(JJ))* &
              (XLVTT*DMT%XM_SYS(JJ,JCOMP)*(DMT%XQ_SYS(JJ,JCOMP)-B%XQI_BLD(JJ,JCOMP)))
         !
         ! Diagnostic of latent heat flux due to evaporative cooling
         !
         PLE_EVAPORATIVE_COOLING(JJ) = ZUP_HVAC_COOL(JJ) * (1.+DMT%XCOP(JJ)) * B%XF_WATER_COND(JJ)
         !
      ELSE
         !
         ! Sensible heat
         !
         PDIAG_DCS_AREA(JJ) = PDIAG_DCS_AREA(JJ) + B%XF_WASTE_CAN(JJ)*ZUP_HVAC_COOL(JJ)*(1.+DMT%XCOP(JJ))
         PH_WASTE_CANY(JJ) = - B%XF_WASTE_CAN(JJ)*(ZUP_HVAC_COOL(JJ)-PHVAC_COOL(JJ))
         !
         PDIAG_DCS_AREA(JJ) = PDIAG_DCS_AREA(JJ) + (1.0-B%XF_WASTE_CAN(JJ))*ZUP_HVAC_COOL(JJ)*(1.+DMT%XCOP(JJ))
         PH_WASTE_ROOF(JJ) = - (1.0-B%XF_WASTE_CAN(JJ))*(ZUP_HVAC_COOL(JJ)-PHVAC_COOL(JJ))
         !
         ! Latent heat
         !
         PLE_WASTE_CANY(JJ) = - B%XF_WASTE_CAN(JJ)*(XLVTT*DMT%XM_SYS(JJ,JCOMP)*(DMT%XQ_SYS(JJ,JCOMP)-B%XQI_BLD(JJ,JCOMP)))
         !
         PLE_WASTE_ROOF(JJ) = - (1.0-B%XF_WASTE_CAN(JJ))*(XLVTT*DMT%XM_SYS(JJ,JCOMP)*(DMT%XQ_SYS(JJ,JCOMP)-B%XQI_BLD(JJ,JCOMP)))
         !
         PLE_EVAPORATIVE_COOLING(JJ) = 0.0
         !
      ENDIF
      !
   ENDIF           
   !
ENDDO
  !
  ! Virtual temperature after update
  !
  ZTV_BLD_NEW(:) = B%XTI_BLD(:,JCOMP) * (1.+((XRV/XRD)-1.)*B%XQI_BLD(:,JCOMP))
  !
  ! --------------------------------------------------------------------
  ! Robert: Diagnostics of sensible and latent heat budget of indoor air
  ! --------------------------------------------------------------------
  !
  ! Sensible heat convective source terms (W/m²(urb))
  !
  ZSRCCONVWALL(:) = T%XBLD(:) * T%XWALL_O_BLD    (:) * PCONV_WL_BLD(:)
  ZSRCCONVGLAZ(:) = T%XBLD(:) * B%XGLAZ_O_BLD    (:) * PCONV_WIN_BLD (:)
  ZSRCCONVMASS(:) = T%XBLD(:) * B%XMASS_O_BLD    (:) * ZCONV_MA_BLD(:)
  ZSRCCONVROOF(:) = T%XBLD(:) * PCONV_RF_BLD (:)
  ZSRCCONVFLOO(:) = T%XBLD(:) * ZCONV_FL_BLD(:)
  !
  ! Internal load of sensible and latent heat (W/m²(urb))
  !
  ZSRCQINSENHE(:)=T%XBLD(:)*ZQIN(:)*(1.0-B%XQIN_FRAD(:))*(1.0-B%XQIN_FLAT(:))
  ZSRCQINLATHE(:)=T%XBLD(:)*ZQIN(:)*B%XQIN_FLAT(:)
  !
  ! Sensible and latent heat due to infiltration/ventilation (W/m²(urb))
  !
  ZSRCSENINFVE(:)=XCPD *T%XBLD(:)*(ZINF(:)+ZNAT_VENT(:))*ZRHOI_OLD(:)*(PT_CAN(:)-B%XTI_BLD(:,JCOMP))
  ZSRCLATINFVE(:)=XLVTT*T%XBLD(:)*(ZINF(:)+ZNAT_VENT(:))*ZRHOI_OLD(:)*(PQ_CAN(:)-B%XQI_BLD(:,JCOMP))
  !
  ! Sensible and latent heat due to heating and climatisation (W/m²(urb))
  !
  ZSRCSENHHVAC(:)=XCPD *T%XBLD(:)*DMT%XM_SYS(:,JCOMP)*(DMT%XT_SYS(:,JCOMP)-B%XTI_BLD(:,JCOMP))
  ZSRCLATHHVAC(:)=XLVTT*T%XBLD(:)*DMT%XM_SYS(:,JCOMP)*(DMT%XQ_SYS(:,JCOMP)-B%XQI_BLD(:,JCOMP))
  !
  ! Difference of sensible and latent heat reservoir (W/m²(urb))
  !
  ZDIFFSENAIRIN(:) = (ZTHEAIRIN_NEW(:)-ZTHEAIRIN_OLD(:))/PTSTEP
  ZDIFFLATAIRIN(:) = (ZLATAIRIN_NEW(:)-ZLATAIRIN_OLD(:))/PTSTEP
  !
  ! ---------------------------------------------------------
  ! ---------------------------------------------------------
  !
  ! Robert: When the indoor temperature increases (decreases), the volume
  ! of the air expands (contracts). Since the volume of the 
  ! building is fixed, there is thus air leaving/entering the building. 
  ! This needs to be taken into account in order to preserve
  ! the sensible heat as well as humidity/latent heat
  !
  ! Calculation of the indoor air mass difference 
  ! between actual and previous time step (kg/m²(bld))
  !
  ZDIFFAIRMASS(:)=T%XBLD_HEIGHT(:)*(PRHOI_NEW(:)-ZRHOI_OLD(:))
  !
  ! Calculation of the sensible and latent heat per time unit 
  ! contained in the air mass difference (W/m²(urb))
  !
  ZDIFFAIRSEN(:)=XCPD  * T%XBLD(:)*ZDIFFAIRMASS(:)*B%XTI_BLD(:,JCOMP)/PTSTEP
  ZDIFFAIRLAT(:)=XLVTT * T%XBLD(:)*ZDIFFAIRMASS(:)*B%XQI_BLD(:,JCOMP)/PTSTEP
  !
  ! Sensible and latent heat budget of indoor air. 
  !
  ZIMB_SENH_BLD(:) = ZSRCCONVWALL(:) + ZSRCCONVGLAZ(:)     + &
       ZSRCCONVMASS(:) + ZSRCCONVROOF(:) + ZSRCCONVFLOO(:) + &
       ZSRCQINSENHE(:) + ZSRCSENINFVE(:) + ZSRCSENHHVAC(:) + &
       ZDIFFAIRSEN (:) - ZDIFFSENAIRIN(:)
  !
  ZIMB_LATH_BLD(:) = ZSRCQINLATHE(:) + ZSRCLATINFVE(:) + &
       ZSRCLATHHVAC(:) + ZDIFFAIRLAT(:)- ZDIFFLATAIRIN(:)
  !
  ! Check sensible heat budget of indoor air
  !
  IF (MAXVAL(ZIMB_SENH_BLD).GT.1.0E-6) THEN
     !
     CALL GET_LUOUT(HPROGRAM,ILUOUT)
     !
     DO JJ=1,SIZE(ZIMB_SENH_BLD)
        !
        IF (ISNAN(ZIMB_SENH_BLD(JJ))) CALL ABOR1_SFX("BEM:NAN detected in energy budget diagnostics")
        !
        IF (ZIMB_SENH_BLD(JJ).GT.0.0) THEN
           !
           WRITE(ILUOUT,*) "                                        "
           WRITE(ILUOUT,*) "The sensible heat balance of indoor air "
           WRITE(ILUOUT,*) "                                        "
           WRITE(ILUOUT,*) "JJ                : ",JJ
           WRITE(ILUOUT,*) "ZSRCCONVWALL(JJ)  : ",ZSRCCONVWALL(JJ)
           WRITE(ILUOUT,*) "ZSRCCONVGLAZ(JJ)  : ",ZSRCCONVGLAZ(JJ)
           WRITE(ILUOUT,*) "ZSRCCONVMASS(JJ)  : ",ZSRCCONVMASS(JJ)
           WRITE(ILUOUT,*) "ZSRCCONVROOF(JJ)  : ",ZSRCCONVROOF(JJ)
           WRITE(ILUOUT,*) "ZSRCCONVFLOO(JJ)  : ",ZSRCCONVFLOO(JJ)
           WRITE(ILUOUT,*) "ZSRCQINSENHE(JJ)  : ",ZSRCQINSENHE(JJ)
           WRITE(ILUOUT,*) "ZSRCSENINFVE(JJ)  : ",ZSRCSENINFVE(JJ)
           WRITE(ILUOUT,*) "ZSRCSENHHVAC(JJ)  : ",ZSRCSENHHVAC(JJ)
           WRITE(ILUOUT,*) "ZDIFFAIRSEN (JJ)  : ",ZDIFFAIRSEN (JJ)
           WRITE(ILUOUT,*) "------------------  "
           WRITE(ILUOUT,*) "ZIMB_SENH_BLD(JJ) : ",ZIMB_SENH_BLD(JJ)
           WRITE(ILUOUT,*) "ZDIFFSENAIRIN(JJ) : ",ZDIFFSENAIRIN(JJ)
           CALL FLUSH(ILUOUT)
           !
        ENDIF
        !
     ENDDO
     !
     CALL ABOR1_SFX("Too large imbalance of indoor sensible heat budget")
     !
  END IF
  !
  ! Check latent heat budget of indoor air
  !
  IF (MAXVAL(ZIMB_LATH_BLD).GT.1.0E-6 ) THEN
     CALL GET_LUOUT(HPROGRAM,ILUOUT)
     DO JJ=1,SIZE(ZIMB_LATH_BLD)
        !
        IF (ISNAN(ZIMB_LATH_BLD(JJ))) CALL ABOR1_SFX("BEM:NAN detected in energy budget diagnostics")
        !
        IF (ZIMB_LATH_BLD(JJ).GT.0.0) THEN
           !
           WRITE(ILUOUT,*) "                                      "
           WRITE(ILUOUT,*) "The latent heat balance of indoor air "
           WRITE(ILUOUT,*) "                                      "
           WRITE(ILUOUT,*) "ZSRCQINLATHE(JJ)  : ",ZSRCQINLATHE(JJ)
           WRITE(ILUOUT,*) "ZSRCLATINFVE(JJ)  : ",ZSRCLATINFVE(JJ)
           WRITE(ILUOUT,*) "ZSRCLATHHVAC(JJ)  : ",ZSRCLATHHVAC(JJ)
           WRITE(ILUOUT,*) "ZDIFFAIRLAT (JJ)  : ",ZDIFFAIRLAT (JJ)
           WRITE(ILUOUT,*) "--------------  "
           WRITE(ILUOUT,*) "ZIMB_LATH_BLD(JJ) : ",ZIMB_LATH_BLD(JJ)
           WRITE(ILUOUT,*) "ZDIFFLATAIRIN(JJ) : ",ZDIFFLATAIRIN(JJ)
           CALL FLUSH(ILUOUT)
           !
        ENDIF
        !
     ENDDO
     CALL ABOR1_SFX("Too large imbalance of indoor latent heat budget")
  ENDIF
  !
  ! Robert:
  ! In order to avoid numerical instabilities when diagnosing
  ! T_CANYON and Q_CANYON, the waste fluxes to the canyon are splitted
  ! into a part not depending directly on PT_CAN (PQ_CAN)
  ! and the part directly depending on PT_CAN (PQ_CAN).
  ! In avg_urban_fluxes, the equations will be written in an implicit
  ! manner for the part depending on B%XTI_BLD - PT_CAN
  !
  ! Constant part
  !
  PCST_H_WASTE_CANY (:) = T%XBLD(:) * PH_WASTE_CANY (:)
  PCST_LE_WASTE_CANY(:) = T%XBLD(:) * PLE_WASTE_CANY(:)
  !
  ! Coefficients for implicitation
  !
  PCOEFF_H_WASTE_CANY (:) = T%XBLD(:) * XCPD  * ZWASTE(:) * ZRHOI_OLD(:)
  PCOEFF_LE_WASTE_CANY(:) = T%XBLD(:) * XLVTT * ZWASTE(:) * ZRHOI_OLD(:)
  !
  ! Original formulation of waste fluxes to be used for all diagnostics,
  ! except for T_CANYON and Q_CANYON
  !
  PH_WASTE_CANY (:) = T%XBLD(:) * PH_WASTE_CANY (:) + &
       T%XBLD(:) * XCPD  * ZWASTE(:) * ZRHOI_OLD(:) * (B%XTI_BLD(:,JCOMP) - PT_CAN(:))
  !
  PLE_WASTE_CANY(:) = T%XBLD(:) * PLE_WASTE_CANY(:) + &
       T%XBLD(:) * XLVTT * ZWASTE(:) * ZRHOI_OLD(:) * (B%XQI_BLD(:,JCOMP) - PQ_CAN(:))
  !
  ! Robert: The sensible and latent heat contained in the air mass
  !         entering or leaving the building due to expansion/contraction
  !         is added to the sensible and latent waste heat flux
  !
  PH_WASTE_ROOF (:) = T%XBLD(:) * PH_WASTE_ROOF (:) - ZDIFFAIRSEN(:)
  PLE_WASTE_ROOF(:) = T%XBLD(:) * PLE_WASTE_ROOF(:) - ZDIFFAIRLAT(:)
  !
  ! Conversion [W/m2(bld)] to [W/m2(urb)]
  !
  PDIAG_DCS_AREA(:) = T%XBLD(:) * PDIAG_DCS_AREA(:)
  PLE_EVAPORATIVE_COOLING(:) = T%XBLD(:) * PLE_EVAPORATIVE_COOLING(:)
  !
  IF (LHOOK) CALL DR_HOOK('BEM',1,ZHOOK_HANDLE)
  !
CONTAINS
  !
  SUBROUTINE GET_NAT_VENT(PPTI_BLD,PPT_CAN,PPU_CAN,PPGLAZ_O_BLD,PNAT_VENT,PFOPEN)
    !
    IMPLICIT NONE
    !
    REAL, DIMENSION(:) , INTENT(IN)  :: PPTI_BLD
    REAL, DIMENSION(:) , INTENT(IN)  :: PPT_CAN
    REAL, DIMENSION(:) , INTENT(IN)  :: PPU_CAN
    REAL, DIMENSION(:) , INTENT(IN)  :: PPGLAZ_O_BLD
    REAL, DIMENSION(:) , INTENT(IN)  :: PFOPEN
    REAL, DIMENSION(:) , INTENT(OUT) :: PNAT_VENT
    REAL(KIND=JPRB)   :: ZHOOK_HANDLE
    !
    ! Local variables
    !
    REAL :: ZHWIN,ZCT,ZCW,ZCST,ZALPHA,ZFAC
    !
    IF (LHOOK) CALL DR_HOOK('BEM:GET_NAT_VENT',0,ZHOOK_HANDLE)
    !
    ! Robert: Ventilation flow rate through single sided windows
    ! taken from the European directive prEN 15242.
    ! "Ventilation for buildings — Calculation methods for the
    !  determination of air flow rates in buildings including infiltration"
    ! The exchanges due to turbulence, wind force and buouyancy are considered
    ! The influence of the shading status on the ventilation air exchange rate
    ! is not considered, since usually only a part of the windows is opened
    ! and occupants might chose to open those windows where blinds are not closed.
    !
    ZCT    = 0.01   ! Coefficient for exchange due to turbulence
    ZCW    = 0.001  ! Coefficient for exchange due to wind force
    ZCST   = 0.0035 ! Coefficient for exchange due to buouyancy
    ZALPHA = 180.0  ! Window opening angle (0° for closed, 180° for fully open)
    ZHWIN  = 1.5    ! Height of the window (m)
    !
    ! Empirical factor for dependency of air-flow on window opening angle
    !
    ZFAC = 2.60E-7*ZALPHA**3-1.19E-4*ZALPHA**2+1.86E-2*ZALPHA
    !
    ! Natural ventilation flow rate [m^3/m^2(bld)/s] (single sided ventilation)
    !
    PNAT_VENT(:)=ZFAC*0.5*PPGLAZ_O_BLD(:)*SQRT(ZCT+ZCW*PPU_CAN(:)**2+ZCST*ZHWIN*ABS(PPTI_BLD(:)-PPT_CAN(:)))
    !
    ! Multiplication of the ventilation flow rate
    ! by the fraction of windows opened
    !
    PNAT_VENT(:)=PNAT_VENT(:)*PFOPEN(:)
    !
    IF (LHOOK) CALL DR_HOOK('BEM:GET_NAT_VENT',1,ZHOOK_HANDLE)
    !
  END SUBROUTINE GET_NAT_VENT
  !
  SUBROUTINE GET_INFILTRATION(PRHOA,PPN50,PPTI_BLD,PPT_CAN,PPU_CAN,PPBLD_HEIGHT,PINFCALC)
    !
    USE MODD_CSTS, ONLY : XG
    !
    ! Robert: Calculation of the air exchange rate due to infiltration [1/h]
    ! as a funtion of the simulated canyon wind speed and the simulated 
    ! outdoor and indoor temperatures.
    ! The equations are similar to the Alberta Air Infiltration Model 
    ! (e.g. Wang et al. 2009, Building and Environment).
    !
    IMPLICIT NONE
    !
    REAL, INTENT(IN), DIMENSION(:) :: PRHOA
    REAL, INTENT(IN), DIMENSION(:) :: PPN50
    REAL, INTENT(IN), DIMENSION(:) :: PPTI_BLD
    REAL, INTENT(IN), DIMENSION(:) :: PPT_CAN
    REAL, INTENT(IN), DIMENSION(:) :: PPU_CAN
    REAL, INTENT(IN), DIMENSION(:) :: PPBLD_HEIGHT
    !
    REAL, INTENT(OUT), DIMENSION(:) :: PINFCALC
    !
    REAL(KIND=JPRB) :: ZHOOK_HANDLE
    !
    ! Local variables
    !
    REAL :: ZFLOW
    REAL :: ZBETA
    REAL :: ZR
    REAL :: ZX
    REAL :: ZY
    REAL :: ZZF
    REAL :: ZM
    REAL :: ZXC
    REAL :: ZF
    REAL :: ZJ
    !
    REAL, DIMENSION(SIZE(PPTI_BLD)) :: ZCAIRTIGHT
    REAL, DIMENSION(SIZE(PPTI_BLD)) :: ZFSTACK
    REAL, DIMENSION(SIZE(PPTI_BLD)) :: ZFWIND
    REAL, DIMENSION(SIZE(PPTI_BLD)) :: ZQSTACK
    REAL, DIMENSION(SIZE(PPTI_BLD)) :: ZQWIND
    !
    IF (LHOOK) CALL DR_HOOK('BEM:GET_INFILTRATION',0,ZHOOK_HANDLE)
    !
    ZFLOW = 2./3. ! Flow exponent (1.0 = lam.; 0.5 = turb.; 2/3 for typical infil. flow)
    ZBETA = -0.33 ! Empirical constant
    ZR    = 0.0   ! All leakage concentrated in the walls
    ZX    = 0.0   ! No sealing-floor difference since all leakage in walls
    ZY    = 0.2   ! Typical value for flue fraction (Walker and Wilson, 1998)
    ZZF   = 1.5   ! Typical value for normalised flue height (Walker and Wilson, 1998)
    !
    ! Stack factor based on Walker and Wilson (1998)
    !
    ZM=MIN(1.0,(ZX+(2*ZFLOW+1)*ZY)**2/(2.0-ZR))
    !
    ZXC=ZR+2.0*(1.0-ZR-ZY)/(ZFLOW+1.0)-2.0*ZY*(ZZF-1.0)**ZFLOW
    !
    ZF=ZFLOW*ZY*(ZZF-1)**(ZFLOW-1./3.)*(1.0-(3*(ZXC-ZX)**2*(ZR)**(1.0-ZFLOW))/(2*(ZZF+1)))
    !
    ZFSTACK=((1+ZFLOW*ZR)/(ZFLOW+1))*(0.5-0.5*(ZM)**(5./4.))**(ZFLOW+1)+ZF
    !
    ! Infiltration factor based on Walker and Wilson (1998), no crawl space
    !
    ZJ=0.5*(ZX+ZR+2*ZY)
    !
    ZFWIND=0.19*(2.0-ZFLOW)*(1.0-(0.5*(ZX+ZR))**(1.5-ZY))-0.25*ZY*(ZJ-2.0*ZY*(ZJ)**4)
    !
    ! Conversion of PPN50 [1/h] to CAIRTIGHT [1/h]
    !
    ZCAIRTIGHT(:)=PPN50(:)/(50)**ZFLOW
    !
    ! The infiltration rate due to the stack effect
    !
    ZQSTACK(:)=ZCAIRTIGHT(:)*ZFSTACK*(PRHOA(:)*XG*PPBLD_HEIGHT(:)*(ABS(PPTI_BLD(:)-PPT_CAN(:))/PPTI_BLD(:)))**ZFLOW
    !
    ! The infiltration rate due to the wind effect
    ! A shelter factor of 1.0 is assumed since the canyon wind speed is used.
    !
    ZQWIND(:)=ZCAIRTIGHT(:)*ZFWIND*(PRHOA(:)*0.5*(PPU_CAN(:))**2)**ZFLOW
    !
    ! Total infiltration rate in 1/h
    !
    PINFCALC(:)=((ZQSTACK(:))**(1.0/ZFLOW)+(ZQWIND(:))**(1.0/ZFLOW)+ZBETA*(ZQSTACK(:)*ZQWIND(:))**(1.0/(2.0*ZFLOW)))**ZFLOW
    !
    IF (LHOOK) CALL DR_HOOK('BEM:GET_INFILTRATION',1,ZHOOK_HANDLE)
    !
  END SUBROUTINE GET_INFILTRATION
  !
END SUBROUTINE BEM
