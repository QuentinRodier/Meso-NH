!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE flake_interface (KI, &
! Atmospheric forcing
                               dMsnowdt_in, I_atm_in, Q_atm_lw_in, height_u_in, height_tq_in,     &
                               U_a_in, T_a_in, q_a_in, P_a_in,                                    &
! Constant parameters                           
                               depth_w, fetch, depth_bs, T_bs, par_Coriolis, del_time,            &
! Parameters that may change                             
                               emis_water, albedo_water,   albedo_ice,   albedo_snow,             &
                               extincoef_water, extincoef_ice, extincoef_snow,                    &
! Flake variables
                               T_snow, T_ice, T_mnw, T_wML, T_bot, T_B1, C_T, h_snow, h_ice,      &
                               h_ML, H_B1, T_sfc,                                                 &
! Surface heat and momentum fluxes                       
                               Q_sensible, Q_latent ,Q_momentum, z0, z0t, Ri, ustar, Cd_a,        &
! Switches to configure FLake runs
                               lflk_botsed, hflk_flux, PPEW_A_COEF, PPEW_B_COEF, rho_a,           &
                               HIMPLICIT_WIND                                                     )  
!------------------------------------------------------------------------------
!
! Description:
!
!  The FLake interface is
!  a communication routine between "flake_driver"
!  and a prediction system that uses FLake.
!  It assigns the FLake variables at the previous time step 
!  to their input values given by the driving model,
!  calls a number of routines to compute the heat and radiation fluxes,
!  calls "flake_driver",
!  and returns the updated FLake variables to the driving model.
!  The "flake_interface" does not contain any Flake physics. 
!  It only serves as a convenient means to organize calls of "flake_driver"
!  and of external routines that compute heat and radiation fluxes.
!  The interface may (should) be changed so that to provide 
!  the most convenient use of FLake.
!  Within a 3D atmospheric prediction system,
!  "flake_driver" may be called in a DO loop within "flake_interface" 
!  for each grid-point where a lake is present.
!  In this way, the driving atmospheric model should call "flake_interface"
!  only once, passing the FLake variables to "flake_interface" as 2D fields. 
!
!  Lines embraced with "!_tmp" contain temporary parts of the code.
!  These should be removed prior to using FLake in applications.
!  Lines embraced/marked with "!_dev" may be replaced
!  as improved parameterizations are developed and tested.
!  Lines embraced/marked with "!_dm" are DM's comments
!  that may be helpful to a user.
!
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  E-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov 
!  Initial release
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

!USE modd_data_parameters , ONLY : &
!      ireals,                   &! KIND-type parameter for real variables
!      iintegers                  ! KIND-type parameter for "normal" integer variables  

USE modd_flake_derivedtypes         ! Definitions of several derived TYPEs

USE modd_flake_parameters , ONLY :   &
    tpl_T_f                     ,  &! Fresh water freezing point [K]
    tpl_rho_w_r                 ,  &! Maximum density of fresh water [kg m^{-3}]
    h_Snow_min_flk              ,  &! Minimum snow thickness [m]
    h_Ice_min_flk                   ! Minimum ice thickness [m]  

USE modd_flake_paramoptic_ref       ! Reference values of the optical characteristics
                               ! of the lake water, lake ice and snow 

USE modd_flake_albedo_ref           ! Reference values the albedo for the lake water, lake ice and snow

USE mode_flake           , ONLY :    &
    flake_driver                ,  &! Subroutine, FLake driver
    flake_radflux               ,  &! Subroutine, computes radiation fluxes at various depths
                                  !
    T_snow_p_flk, T_snow_n_flk  ,  &! Temperature at the air-snow interface [K]
    T_ice_p_flk, T_ice_n_flk    ,  &! Temperature at the snow-ice or air-ice interface [K]
    T_mnw_p_flk, T_mnw_n_flk    ,  &! Mean temperature of the water column [K]
    T_wML_p_flk, T_wML_n_flk    ,  &! Mixed-layer temperature [K]
    T_bot_p_flk, T_bot_n_flk    ,  &! Temperature at the water-bottom sediment interface [K]
    T_B1_p_flk, T_B1_n_flk      ,  &! Temperature at the bottom of the upper layer of the sediments [K]
    C_T_p_flk, C_T_n_flk        ,  &! Shape factor (thermocline)
    h_snow_p_flk, h_snow_n_flk  ,  &! Snow thickness [m]
    h_ice_p_flk, h_ice_n_flk    ,  &! Ice thickness [m]
    h_ML_p_flk, h_ML_n_flk      ,  &! Thickness of the mixed-layer [m]
    H_B1_p_flk, H_B1_n_flk      ,  &! Thickness of the upper layer of bottom sediments [m]
                                  !
    Q_snow_flk                  ,  &! Heat flux through the air-snow interface [W m^{-2}]
    Q_ice_flk                   ,  &! Heat flux through the snow-ice or air-ice interface [W m^{-2}]
    Q_w_flk                     ,  &! Heat flux through the ice-water or air-water interface [W m^{-2}]
    Q_bot_flk                   ,  &! Heat flux through the water-bottom sediment interface [W m^{-2}]
    I_atm_flk                   ,  &! Radiation flux at the lower boundary of the atmosphere [W m^{-2}],
                                  ! i.e. the incident radiation flux with no regard for the surface albedo
    I_snow_flk                  ,  &! Radiation flux through the air-snow interface [W m^{-2}]
    I_ice_flk                   ,  &! Radiation flux through the snow-ice or air-ice interface [W m^{-2}]
    I_w_flk                     ,  &! Radiation flux through the ice-water or air-water interface [W m^{-2}]
    I_h_flk                     ,  &! Radiation flux through the mixed-layer-thermocline interface [W m^{-2}]
    I_bot_flk                   ,  &! Radiation flux through the water-bottom sediment interface [W m^{-2}]
    I_intm_0_h_flk              ,  &! Mean radiation flux over the mixed layer [W m^{-1}]
    I_intm_h_D_flk              ,  &! Mean radiation flux over the thermocline [W m^{-1}]
    Q_star_flk                  ,  &! A generalized heat flux scale [W m^{-2}]
    u_star_w_flk                ,  &! Friction velocity in the surface layer of lake water [m s^{-1}]
    w_star_sfc_flk              ,  &! Convective velocity scale, using a generalized heat flux scale [m s^{-1}]
    dMsnowdt_flk                    ! The rate of snow accumulation [kg m^{-2} s^{-1}]  


USE mode_sfcflx          , ONLY :    &
    sfcflx_lwradwsfc            ,  &! Function, returns the surface long-wave radiation flux
    sfcflx_momsenlat            ,  &! Subroutine, computes fluxes of momentum and of sensible and latent heat
    z0u_sf                      ,  &! Roughness length with respect to wind velocity [m]
    z0t_sf                          ! Roughness length with respect to potential temperature [m]  


USE modd_flake_configure, ONLY : lflk_botsed_use 
!==============================================================================
!
USE MODI_WIND_THRESHOLD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE

!==============================================================================
!
! Declarations

!
!*      0.1    declarations of arguments
!
!  Input (procedure arguments)
!
INTEGER, INTENT(IN)  :: KI              ! number of points
!
INTEGER :: i ! DO loop index
!
REAL, DIMENSION(KI), INTENT(IN) ::   &
    dMsnowdt_in                       ,  &! The rate of snow accumulation [kg m^{-2} s^{-1}]
    I_atm_in                          ,  &! Solar radiation flux at the surface [W m^{-2}]
    Q_atm_lw_in                       ,  &! Long-wave radiation flux from the atmosphere [W m^{-2}]
    height_u_in                       ,  &! Height above the lake surface where the wind speed is measured [m]
    height_tq_in                      ,  &! Height where temperature and humidity are measured [m]
    U_a_in                            ,  &! Wind speed at z=height_u_in [m s^{-1}]
    T_a_in                            ,  &! Air temperature at z=height_tq_in [K]
    q_a_in                            ,  &! Air specific humidity at z=height_tq_in
    P_a_in                                ! Surface air pressure [N m^{-2} = kg m^{-1} s^{-2}]  

REAL, DIMENSION(KI), INTENT(IN) ::   &
    depth_w                           ,  &! The lake depth [m]
    fetch                             ,  &! Typical wind fetch [m]
    depth_bs                          ,  &! Depth of the thermally active layer of the bottom sediments [m]
    T_bs                              ,  &! Temperature at the outer edge of 
                                        ! the thermally active layer of the bottom sediments [K]
    par_Coriolis                      ,  &! The Coriolis parameter [s^{-1}]
    del_time                          ,  &! The model time step [s]
    PPEW_A_COEF                       ,  &! coefficient A (m^2 s kg^{-1}) and B (m s^{-1})
    PPEW_B_COEF                       ,  &! for wind implicitation :  V+ = - A * rho_a u'w' + B
    rho_a                             ,  &! Air density (kg m ^{-3}) (from forcing atm. data)  
    emis_water                            ! Water surface emissivity
!
 CHARACTER(LEN=*),     INTENT(IN)  :: HIMPLICIT_WIND   ! wind implicitation option
!                                                     ! 'OLD' = direct
!                                                     ! 'NEW' = Taylor serie, order 1
!
LOGICAL ::  lflk_botsed ! Switch, .TRUE. -> use the bottom-sediment scheme 
 CHARACTER(LEN=5) ::  hflk_flux     ! 'DEF  '/'FLAKE'/'ECUME' compute the surface fluxes if = 'FLAKE'

!
!  Input/Output (procedure arguments)

REAL, DIMENSION(KI), INTENT(INOUT)  :: &
    albedo_water                        ,  &! Water surface albedo with respect to the solar radiation
    albedo_ice                          ,  &! Ice surface albedo with respect to the solar radiation
    albedo_snow                             ! Snow surface albedo with respect to the solar radiation  

REAL, DIMENSION(KI), INTENT(INOUT) ::  &
    extincoef_water                       ,  &! extintion coefficient of water
    extincoef_ice                         ,  &! extintion coefficient of ice
    extincoef_snow                            ! extintion coefficient of snow   

REAL, DIMENSION(KI), INTENT(INOUT)  :: &
    T_snow                        ,  &! Temperature at the air-snow interface [K] 
    T_ice                         ,  &! Temperature at the snow-ice or air-ice interface [K]
    T_mnw                         ,  &! Mean temperature of the water column [K]
    T_wML                         ,  &! Mixed-layer temperature [K]
    T_bot                         ,  &! Temperature at the water-bottom sediment interface [K]
    T_B1                          ,  &! Temperature at the bottom of the upper layer of the sediments [K]
    C_T                           ,  &! Shape factor (thermocline)
    h_snow                        ,  &! Snow thickness [m]
    h_ice                         ,  &! Ice thickness [m]
    h_ML                          ,  &! Thickness of the mixed-layer [m]
    H_B1                          ,  &! Thickness of the upper layer of bottom sediments [m]
    T_sfc                             ! Surface temperature at the previous time step [K]    

!  Output (procedure arguments)

REAL, DIMENSION(KI), INTENT(INOUT)  ::    &
    Q_sensible             ,  &! Sensible heat flux [W m^{-2}]
    Q_latent               ,  &! Latent heat flux [W m^{-2}]
    Q_momentum             ,  &! Momentum flux [N m^{-2}]
    z0                     ,  &! Roughness length with respect to wind velocity [m]
    z0t                    ,  &! Roughness length with respect to potential temperature [m]
    Ri                     ,  &! Gradient Richardson number 
    ustar                  ,  &! air friction velocity  
    Cd_a                       ! wind drag coefficient [no unit]




!
!*      0.2    declarations of local variables
!
INTEGER  :: JL                  ! loop counter on horizontal points

REAL :: T_sfc_n ! Surface temperature at the new time step [K]
REAL :: ustar2  ! square of air friction velocity (m2/s2)  
REAL :: zvmod   ! wind at t+1   
REAL, DIMENSION(KI) ::    &
    Q_watvap, &                      ! Flux of water vapour [kg m^{-2} s^{-1}] 
    zwind             ! thresholded wind

TYPE (opticpar_medium), DIMENSION(KI) ::  &
    opticpar_water                       ,  &! Optical characteristics of water
    opticpar_ice                         ,  &! Optical characteristics of ice
    opticpar_snow                            ! Optical characteristics of snow   
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!  Set albedos of the lake water, lake ice and snow
!------------------------------------------------------------------------------
!
! Use default value 
! albedo_water(:) = albedo_water_ref
! Use empirical formulation proposed by Mironov and Ritter (2004) for GME 
IF (LHOOK) CALL DR_HOOK('FLAKE_INTERFACE',0,ZHOOK_HANDLE)
albedo_ice   = albedo_whiteice_ref 
!albedo_ice(:)   = EXP(-c_albice_MR*(tpl_T_f-T_sfc(:))/tpl_T_f)
!albedo_ice(:)   = albedo_whiteice_ref*(1.-albedo_ice) + albedo_blueice_ref*albedo_ice
! Snow is not considered
albedo_snow(:)  = albedo_ice(:)  
!------------------------------------------------------------------------------
!  Set optical characteristics of the lake water, lake ice and snow
!------------------------------------------------------------------------------
!
! Use default values
!opticpar_water = opticpar_water_ref  ! don't use default values

opticpar_ice   = opticpar_ice_opaque   ! Opaque ice
opticpar_snow  = opticpar_snow_opaque  ! Opaque snow
!
lflk_botsed_use = lflk_botsed
!
zwind = WIND_THRESHOLD(U_a_in,height_u_in)
!
H_POINT_LOOP: DO JL = 1,KI ! begin of loop on horizontal points
!------------------------------------------------------------------------------
!  Set initial values
!------------------------------------------------------------------------------
   
opticpar_water(JL) = opticpar_medium(1,                       &
      (/1., (0.,i=2,nband_optic_max)/),            &
      (/extincoef_water(JL), (1.E+10,i=2,nband_optic_max)/))  
   T_snow_p_flk = T_snow(JL)
   T_ice_p_flk  = T_ice(JL)
   T_mnw_p_flk  = T_mnw(JL)
   T_wML_p_flk  = T_wML(JL)
   T_bot_p_flk  = T_bot(JL)
   T_B1_p_flk   = T_B1(JL)
   C_T_p_flk    = C_T(JL)
   h_snow_p_flk = h_snow(JL)
   h_ice_p_flk  = h_ice(JL)
   h_ML_p_flk   = h_ML(JL)
   H_B1_p_flk   = H_B1(JL)
   
!------------------------------------------------------------------------------
!  Set the rate of snow accumulation
!------------------------------------------------------------------------------
   
   dMsnowdt_flk = dMsnowdt_in(JL)  
   
!------------------------------------------------------------------------------
!  Compute solar radiation fluxes (positive downward)
!------------------------------------------------------------------------------
   
   I_atm_flk = I_atm_in(JL)
   CALL flake_radflux ( depth_w(JL), albedo_water(JL), albedo_ice(JL), &
                          albedo_snow(JL), opticpar_water(JL),           &
                          opticpar_ice(JL), opticpar_snow(JL) )  
   
!------------------------------------------------------------------------------
!  Compute long-wave radiation fluxes (positive downward)
!------------------------------------------------------------------------------
   
   Q_w_flk = Q_atm_lw_in(JL)                       ! Radiation of the atmosphere 
   Q_w_flk = Q_w_flk - SfcFlx_lwradwsfc(emis_water(JL),T_sfc(JL))  ! Radiation of the surface (notice the sign)
   
!------------------------------------------------------------------------------
!  Compute the surface friction velocity and fluxes of sensible and latent heat 
!------------------------------------------------------------------------------
   
   IF (hflk_flux=='FLAKE') THEN
           z0t(JL)=1.E-7  ! bug correction V. Masson: default value if
                          ! computations cannot be done in SfxFlx_momsenlat
      Q_momentum(JL) = - rho_a(JL) * ustar(JL)**2
      CALL SfcFlx_momsenlat ( height_u_in(JL), height_tq_in(JL), fetch(JL),  &
                             U_a_in(JL), T_a_in(JL), q_a_in(JL), T_sfc(JL), &
                             P_a_in(JL), h_ice_p_flk, Q_momentum(JL),       &
                             Q_sensible(JL), Q_latent(JL), Q_watvap(JL),    &
                             Ri(JL), z0(JL), z0t(JL)                        )  
      z0(JL)= z0u_sf
      z0t(JL)=z0t_sf
      ! recomputes the future wind speed and associated momentum flux
      ! in the case wind speed is implicited  (V. Masson, Meteo-France)
      ! 1st step : drag coefficient
      ! It is retrieved assumed a relationship between momentum flux
      !  and previous time-step wind : Q_mom = - rho_a * Cd_a * U_a_in**2
      !
      Cd_a(JL) = - Q_momentum(JL) / rho_a(JL) / zwind(JL)**2 
      ! 2nd step : friction velocity (for air) computed with future wind speed
      !            (the latter computed using implicit coefficients)
      ustar2 = 0.0
      zvmod  = U_a_in(JL)
      IF(HIMPLICIT_WIND=='OLD')THEN
      !  old implicitation
         ustar2 = (Cd_a(JL)*U_a_in(JL)*PPEW_B_COEF(JL))/            &
                  (1.0-rho_a(JL)*Cd_a(JL)*U_a_in(JL)*PPEW_A_COEF(JL))
      ELSE
      !  new implicitation
         ustar2 = (Cd_a(JL)*U_a_in(JL)*(2.*PPEW_B_COEF(JL)-U_a_in(JL)))/ &
                  (1.0-2.0*rho_a(JL)*Cd_a(JL)*U_a_in(JL)*PPEW_A_COEF(JL))
         zvmod  = rho_a(JL)*PPEW_A_COEF(JL)*ustar2 + PPEW_B_COEF(JL)
         zvmod  = max(0.0,zvmod)
         IF(PPEW_A_COEF(JL)/= 0.)THEN
           ustar2 = max((zvmod-PPEW_B_COEF(JL))/(rho_a(JL)*PPEW_A_COEF(JL)),0.0)
         ENDIF
      ENDIF
      ustar(JL) =sqrt(ustar2)
      ! 3rd step : momentum flux computed with the future wind speed
      Q_momentum(JL) = - rho_a(JL) * ustar2

   END IF
   u_star_w_flk = SQRT(-Q_momentum(JL)/tpl_rho_w_r)
   
!------------------------------------------------------------------------------
!  Compute heat fluxes Q_snow_flk, Q_ice_flk, Q_w_flk
!------------------------------------------------------------------------------
   
   Q_w_flk = Q_w_flk - Q_sensible(JL) - Q_latent(JL) ! Add sensible and latent heat fluxes 
                                                     ! (notice the signs)
   IF(h_ice_p_flk.GE.h_Ice_min_flk) THEN         ! Ice exists
     IF(h_snow_p_flk.GE.h_Snow_min_flk) THEN     ! There is snow above the ice
       Q_snow_flk = Q_w_flk
       Q_ice_flk  = 0.
       Q_w_flk    = 0.
     ELSE                                        ! No snow above the ice
       Q_snow_flk = 0.
       Q_ice_flk  = Q_w_flk
       Q_w_flk    = 0.
     END IF
   ELSE                                          ! No ice-snow cover
       Q_snow_flk = 0.
       Q_ice_flk  = 0.
   END IF
   
!------------------------------------------------------------------------------
!  Advance FLake variables
!------------------------------------------------------------------------------
   CALL flake_driver ( depth_w(JL), depth_bs(JL), T_bs(JL), par_Coriolis(JL),         &
                         opticpar_water(JL)%extincoef_optic(1),             &
                         del_time(JL), T_sfc(JL), T_sfc_n )  
   
!------------------------------------------------------------------------------
!  Set output values
!------------------------------------------------------------------------------
   
   T_snow(JL) = T_snow_n_flk  
   T_ice(JL)  = T_ice_n_flk      
   T_mnw(JL)  = T_mnw_n_flk     
   T_wML(JL)  = T_wML_n_flk    
   T_bot(JL)  = T_bot_n_flk   
   T_B1(JL)   = T_B1_n_flk    
   C_T(JL)    = C_T_n_flk     
   h_snow(JL) = h_snow_n_flk   
   h_ice(JL)  = h_ice_n_flk    
   h_ML(JL)   = h_ML_n_flk     
   H_B1(JL)   = H_B1_n_flk    
   T_sfc(JL)  = T_sfc_n
   
ENDDO H_POINT_LOOP
IF (LHOOK) CALL DR_HOOK('FLAKE_INTERFACE',1,ZHOOK_HANDLE)

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END SUBROUTINE flake_interface

