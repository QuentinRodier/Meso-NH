!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE ISBA(IO, KK, PK, PEK, G, AG, DK, DEK, DMK, TPTIME, PPOI, PABC, &
                      PIACAN,OMEB, PTSTEP, HIMPLICIT_WIND, PZREF, PUREF,        &
                      PDIRCOSZW,PCVHEATF, PSLOPE_DIR, PIMPWET, PIMPDRY,         &
                      PTA, PQA, PEXNA, PRHOA, PPS, PEXNS, PRR, PSR, PZENITH,    &
                      PAZIM, PSCA_SW, PSW_RAD, PLW_RAD, PLETR_HVEG, PVMOD,      &
                      PVDIR, PPEW_A_COEF, PPEW_B_COEF,                          &
                      PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF, AT,   &
                      PALBNIR_TVEG, PALBVIS_TVEG, PALBNIR_TSOIL, PALBVIS_TSOIL, &
                      PPALPHAN, PZ0G_WITHOUT_SNOW, PZ0_MEBV, PZ0H_MEBV,         &
                      PZ0EFF_MEBV, PZ0_MEBN, PZ0H_MEBN, PZ0EFF_MEBN, PTDEEP_A,  &
                      PCSP, PFFG_NOSNOW, PFFV_NOSNOW, PEMIST, PUSTAR, PAC_AGG,  &
                      PHU_AGG, PRESP_BIOMASS_INST, PDEEP_FLUX, PIRRIG_GR,       &
                      NPAR_VEG_IRR_USE, KTAB_SYT, P_DIR_SW, P_SCA_SW,           &
                      PRN_SHADE, PRN_SUNLIT, PBLOWSNW_FLUX, PBLOWSNW_CONC       )
!     ##########################################################################
!
!
!!****  *ISBA*  
!!
!!    PURPOSE
!!    -------
!       Monitor for the calculation of the surface fluxes and of the
!     prognostic variables of the surface over natural areas
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!      
!!    AUTHOR
!!    ------
!!      S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/03/95 
!!      (J.Stein)   25/10/95  add the rain flux computation at the ground
!!                            and the lbc
!!      (J.Stein)   15/11/95  include the strong slopes cases
!!      (J.Stein)   06/02/96  bug correction for the precipitation flux writing 
!!      (J.Stein)   20/05/96  set the right IGRID value for the rain rate
!!      (J.Viviand) 04/02/97  add cold and convective precipitation rate
!!      (J.Stein)   22/06/97  use the absolute pressure    
!!      (V.Masson)  09/07/97  add directional z0 computations and RESA correction     
!!      (V.Masson)  13/02/98  simplify the routine: only vegetation computation
!!                            are now made here.
!!      (A.Boone)   05/10/98  add: Boone et al. (1999) 3 soil-water Layers version
!!      (V.Masson)                 Dumenil and Todini (1992) runoff
!!                                 Calvet (1998) biomass and CO2 assimilation
!!                                 Calvet (1998) LAI evolution
!!      (A.Boone)  03/15/99   Soil ice scheme: modify CG, C1, C2, WSAT, WFC, WILT,
!!                            LEG (add soil ice sublimation); Can modify TS and T2.
!!                            New variables WGI1, WGI2
!!      (A.Boone)  18/01/00   ISBA-ES (3-layer explicit snow scheme option)
!!                            (Boone and Etchevers 2000)
!!                            New variable IPEK%TSNOW%HEAT(:,:,1)
!!      (V. Masson) 01/2004   wet leaves fraction computed in separate routine
!!                            all vegetation stress (ISBA, AGS, AST) routines
!!                            called at the same point
!!      (P. LeMoigne) 03/2004 computation of QSAT 
!!      (P. LeMoigne) 10/2004 halstead coefficient as diagnostic for isba
!!      (A. Bogatchev)09/2005 EBA snow option
!!      (P. LeMoigne) 02/2006 z0h and snow
!!      (B. Decharme) 05/2008 Add floodplains scheme
!!      (R. Hamdi)    01/09   Cp and L are not constants (As in ALADIN)
!!      (A.L. Gibelin)  03/2009 : Add respiration diagnostics
!!      A.L. Gibelin   06/09 : move calculations of CO2 fluxes
!!      A.L. Gibelin 07/2009 : Suppress PPST and PPSTF as outputs
!!      (A. Boone)    11/2009 Add local variable: total soil temperature change (before
!!                            phase change) for use by LWT scheme in ISBA-DIF. 
!!      (A. Boone)    03/2010 Add local variable: delta functions for LEG and LEGI
!!                            to numerically correct for when they should be
!!                            zero when hug(i) Qsat < Qa and Qsat > Qa
!!     (A. Carrer)    04/2011 : new radiative transfert (AGS)
!!      (B. Decharme) 09/2012 Bug : Save snow albedo values at beginning
!!                                  of time step for total albedo calculation
!!                            Bug : flood fraction in COTWORES
!!                            new wind implicitation
!!                            Irrigation rate diag
!!     (C. de Munck) 03/2013  Specified irrigation for ground
!!      (B. Decharme) 04/2013 Bug : Wrong radiative temperature
!!                            DIF lateral subsurface drainage
!!                            Sublimation diag flux
!!                            Qs for 3l or crocus (needed for coupling with atm)
!!                            water table / surface coupling
!!                            Routines drag, e_budget and isba_fluxes now in isba_ceb
!!      (A. Boone & P. Samuelsson) (10/2014) Added MEB v1
!!      P. LeMoigne   12/2014 EBA scheme update
!!      A. Boone      02/2015 Consider spectral band dependence of snow for IO%LTR_ML radiation option
!!      B. Decharme   01/2016 Bug with flood budget
!!      M. Dumont     11/2015 Atmotartes and spectral output
!!      M. Lafaysse      2016 Crocus multiphysics (Cluzet et al 2016)
!!      J.Etchanchu   01/2018 Add irrigation decision rules
!!      A. Druel      02/2019 Adapt the code to be compatible with irrigation (and new patches)
!!       V.Vionnet 2017 blow snow
!!      (P. Tulet)    06/2016 add RN leaves for MEGAN coupling
!!      B. Decharme    02/17 : exact computation of saturation deficit near the leaf surface
!!      B. Decharme    07/19 : addd many diag for water and energy balance computation 
!!                             Suppress PEK%XLE and repalce by DK%XLE
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n,   ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,           ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_SFX_GRID_n,       ONLY : GRID_t
USE MODD_AGRI_n,           ONLY : AGRI_t
USE MODD_DIAG_n,           ONLY : DIAG_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
USE MODD_AGRI,             ONLY : LIRRIGMODE
!
USE MODD_CO2V_PAR,         ONLY : XMC, XMCO2, XPCCO2
USE MODD_SURF_PAR,         ONLY : XUNDEF
!
USE MODD_CSTS,             ONLY : XTT,XPI
USE MODD_CO2V_PAR,         ONLY : XMC, XMCO2, XPCCO2
USE MODD_SURF_PAR,         ONLY : XUNDEF
USE MODD_MEB_PAR,          ONLY : XSW_WGHT_VIS, XSW_WGHT_NIR
!
USE MODD_TYPE_DATE_SURF,   ONLY : DATE_TIME
!
USE MODD_SURF_ATM_TURB_n,  ONLY : SURF_ATM_TURB_t
!
USE MODI_SOIL
USE MODI_SOILDIF
USE MODI_SOILSTRESS
USE MODI_WET_LEAVES_FRAC
USE MODI_VEG
USE MODI_SNOW3L_ISBA
USE MODI_HYDRO
USE MODI_ISBA_SNOW_AGR
!
USE MODI_RADIATIVE_TRANSFERT
USE MODI_COTWORES
USE MODI_IRRIGATION_TRIGGER
!
USE MODI_ISBA_CEB
USE MODI_ISBA_MEB
!
USE MODE_THERMOS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
!*      0.1    declarations of arguments
!              -------------------------
!
!
!* general variables
!  -----------------
!
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: IO
TYPE(ISBA_K_t),         INTENT(INOUT) :: KK
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
TYPE(GRID_t),           INTENT(INOUT) :: G
TYPE(AGRI_t),           INTENT(INOUT) :: AG
TYPE(DIAG_t),           INTENT(INOUT) :: DK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
TYPE(DATE_TIME), INTENT(IN)       :: TPTIME     ! current date and time
!
REAL, DIMENSION(:),    INTENT(IN) :: PPOI       ! Gaussian weights (as above)
REAL, DIMENSION(:), INTENT(INOUT) :: PABC       ! abscissa needed for integration
!                                               ! of net assimilation and stomatal
!                                               ! conductance over canopy depth
REAL, DIMENSION(:,:),   INTENT(OUT) :: PIACAN   ! PAR in the canopy at different gauss level
LOGICAL, INTENT(IN)                 :: OMEB     ! True = patch with multi-energy balance 
!                                               ! False = patch with classical ISBA 
REAL,                 INTENT(IN)  :: PTSTEP     ! timestep of the integration
CHARACTER(LEN=*),     INTENT(IN)  :: HIMPLICIT_WIND   ! wind implicitation option
!                                                     ! 'OLD' = direct
!                                                     ! 'NEW' = Taylor serie, order 1
!
REAL,                 INTENT(IN) :: PCVHEATF 
!
REAL, DIMENSION(:),   INTENT(IN) :: PZREF       ! normal distance of the first
!                                               ! atmospheric level to the
!                                               ! orography
REAL, DIMENSION(:),   INTENT(IN) :: PUREF       ! reference height of the wind
!                                               ! NOTE this is different from ZZREF
!                                               ! ONLY in stand-alone/forced mode,
!                                               ! NOT when coupled to a model (MesoNH)
REAL, DIMENSION(:),   INTENT(IN) ::  PDIRCOSZW  ! Director Cosinus along z
!                                               ! directions at surface w-point
REAL, DIMENSION(:),   INTENT(IN) ::  PSLOPE_DIR ! Slope direction
!
!* atmospheric variables
!  ---------------------
!
!            suffix 'A' stands for atmospheric variable at first model level
!            suffix 'S' stands for atmospheric variable at ground level
!
REAL, DIMENSION(:), INTENT(IN)  :: PTA        ! Temperature
REAL, DIMENSION(:), INTENT(IN)  :: PQA        ! specific humidity
REAL, DIMENSION(:), INTENT(IN)  :: PEXNA      ! Exner function
REAL, DIMENSION(:), INTENT(IN)  :: PRHOA      ! air density
!
REAL, DIMENSION(:), INTENT(IN)  :: PPS        ! Pressure
REAL, DIMENSION(:), INTENT(IN)  :: PEXNS      ! Exner function
!
REAL, DIMENSION(:), INTENT(IN)  :: PRR        ! Rain rate (in kg/m2/s)
REAL, DIMENSION(:), INTENT(IN)  :: PSR        ! Snow rate (in kg/m2/s)
!
REAL, DIMENSION(:), INTENT(IN)  :: PZENITH    ! solar zenith angle
REAL, DIMENSION(:), INTENT(IN)  :: PAZIM      ! azimuthal angle      (radian from North, clockwise)
REAL, DIMENSION(:), INTENT(IN)  :: PSW_RAD    ! solar   incoming radiation on slope
REAL, DIMENSION(:), INTENT(IN)  :: PSCA_SW    ! solar diffuse incoming radiation on slope
REAL, DIMENSION(:), INTENT(IN)  :: PLW_RAD    ! thermal incoming radiation

REAL, DIMENSION(:,:), INTENT(IN):: P_DIR_SW  ! solar direct spectral incoming radiation on slope
REAL, DIMENSION(:,:), INTENT(IN):: P_SCA_SW ! solar diffuse spectral incoming radiation on slope
!
REAL, DIMENSION(:), INTENT(IN)  :: PVMOD      ! modulus of the wind
!                                             ! parallel to the orography
REAL, DIMENSION(:), INTENT(IN)  :: PVDIR      ! wind direction
!
! implicit coupling coefficients:
!
REAL, DIMENSION(:), INTENT(IN)  :: PPEW_A_COEF, PPEW_B_COEF, &
                                   PPET_A_COEF, PPEQ_A_COEF, &
                                   PPET_B_COEF, PPEQ_B_COEF  
!                                  PPEW_A_COEF ! A-wind coefficient
!                                  PPEW_B_COEF ! B-wind coefficient
!                                  PPET_A_COEF ! A-air temperature coefficient
!                                  PPET_B_COEF ! B-air temperature coefficient
!                                  PPEQ_A_COEF ! A-air specific humidity coefficient
!                                  PPEQ_B_COEF ! B-air specific humidity coefficient
!
TYPE(SURF_ATM_TURB_t), INTENT(IN) :: AT         ! atmospheric turbulence parameters
!
!* vegetation parameters
!  ---------------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PALBNIR_TVEG  ! tot albedo of vegetation in NIR (needed for LM_TR)
REAL, DIMENSION(:), INTENT(IN)  :: PALBVIS_TVEG  ! tot albedo of vegetation in VIS
REAL, DIMENSION(:), INTENT(IN)  :: PALBNIR_TSOIL ! tot albedo of bare soil in NIR
REAL, DIMENSION(:), INTENT(IN)  :: PALBVIS_TSOIL ! tot albedo of bare soil in VIS
!
! For multi-energy balance
!
REAL, DIMENSION(:), INTENT(IN)    :: PPALPHAN           ! snow/canopy transition coefficient
REAL, DIMENSION(:), INTENT(IN)    :: PZ0G_WITHOUT_SNOW  ! roughness length for momentum at snow-free canopy floor
REAL, DIMENSION(:), INTENT(IN)    :: PZ0_MEBV           ! roughness length for momentum over MEB vegetation part of patch
REAL, DIMENSION(:), INTENT(IN)    :: PZ0H_MEBV          ! roughness length for heat over MEB vegetation part of path
REAL, DIMENSION(:), INTENT(IN)    :: PZ0EFF_MEBV        ! roughness length for momentum over MEB vegetation part of patch
REAL, DIMENSION(:), INTENT(IN)    :: PZ0_MEBN           ! roughness length for momentum over MEB snow part of patch
REAL, DIMENSION(:), INTENT(IN)    :: PZ0H_MEBN          ! roughness length for heat over MEB snow part of path
REAL, DIMENSION(:), INTENT(IN)    :: PZ0EFF_MEBN        ! roughness length for momentum over MEB snow part of patch
!
!* soil parameters
!  ---------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PTDEEP_A       ! Deep soil temperature (prescribed)
!                                                 PTDEEP_A = Deep soil temperature
!                                                 coefficient depending on flux
!* ISBA-Ags parameters
!  -------------------
!
REAL, DIMENSION(:),    INTENT(IN) :: PCSP       ! atmospheric CO2 concentration
!                                                 [ppmm]=[kg CO2 / kg air]
REAL, DIMENSION(:,:),    INTENT(IN) :: PIMPWET  ! flux of wet deposit for each impurity type 
REAL, DIMENSION(:,:),    INTENT(IN) :: PIMPDRY  ! flux of dry deposit for each impurity type 
!
!
!* ISBA-DF variables/parameters:                  
!  ------------------------------
!
REAL, DIMENSION(:), INTENT(IN)   :: PFFG_NOSNOW ! Without snow (ES)
REAL, DIMENSION(:), INTENT(IN)   :: PFFV_NOSNOW ! Without snow (ES)
!
!* diagnostic variables
!  --------------------
!
REAL, DIMENSION(:), INTENT(OUT) :: PEMIST     ! grid-area surface emissivity
!
!* surface fluxes
!  --------------
!
REAL, DIMENSION(:), INTENT(OUT) :: PUSTAR     ! friction velocity
!
! The following surface fluxes are from snow-free portion of grid
! box when the ISBA-ES option is ON. Otherwise, they are equal
! to the same variables without the _ISBA extension.
!
REAL, DIMENSION(:),  INTENT(OUT) :: PAC_AGG  ! aggregated aerodynamic conductance for evaporative flux calculations
REAL, DIMENSION(:),  INTENT(OUT) :: PHU_AGG  ! aggregated relative humidity for evaporative flux calculations
!
!* diagnostic variables for Carbon assimilation
!  --------------------------------------------
!
REAL, DIMENSION(:,:),   INTENT(OUT) :: PRESP_BIOMASS_INST  ! instantaneous biomass respiration (kgCO2/m2/s)
!
!* diagnostic variables for TEB
!  ---------------------------------------------------
!
REAL,    DIMENSION(:), INTENT(OUT)  :: PDEEP_FLUX  ! Heat flux at bottom of ISBA (W/m2)
!
REAL, DIMENSION(:,:),   INTENT(IN)  :: PLETR_HVEG  ! latent heat from overstory vegetation extracted from each soil layer
!
!* Irrigation
!  ----------
!
REAL,    DIMENSION(:), INTENT(IN)   :: PIRRIG_GR        ! ground irrigation rate (kg/m2/s)
!
INTEGER, DIMENSION(:), INTENT(IN)   :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!
!* Snow and Blowing snow variables
!  -------------------------------
!
INTEGER, DIMENSION(:), INTENT(IN)   ::  KTAB_SYT       ! Array of index defining opposite points for Sytron
!
REAL, DIMENSION(:), INTENT(INOUT) :: PRN_SHADE, PRN_SUNLIT ! RN leaves 
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PBLOWSNW_FLUX! Blowing snow particles flux:
!                                       1: Number (#/m2/s) 2: Mass (kg/m2/s)
!                                       IN : contains sedimentation flux
!                                        OUT : contains emitted turbulent flux towards the atmosphere
REAL, DIMENSION(:,:), INTENT(IN)    :: PBLOWSNW_CONC ! Blowing snow particles concentration:
!                                           1: Number (#/m3) 2: Mass (kg/m3)
!
!-------------------------------------------------------------------------------
!
!*      0.2    declarations of local variables
!              -------------------------------
!
!
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZCS       ! heat capacity of the snow
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZFROZEN1  ! ice fraction in superficial soil
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZDELTA    ! fraction of the foliage covered with intercepted water
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZQSAT     ! expression for the saturation specific humidity 
!
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZWRMAX    ! maximum canopy water interception
!
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZF5       ! water stress coefficient (based on F2) to enforce Etv=>0 as F2=>0
!
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZHUGI    ! humidity over frozen bare ground
!
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZEVAPCOR ! evaporation correction as last traces of snow cover ablate
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZLES3L   ! sublimation from ISBA-ES(3L)
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZLEL3L   ! evaporation heat flux of water in the snow (W/m2)
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZEVAP3L  ! evaporation flux over snow from ISBA-ES (kg/m2/s)
!
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZSNOW_THRUFAL ! rate that liquid water leaves snow pack: ISBA-ES [kg/(m2 s)]
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZSNOW_THRUFAL_SOIL !liquid water leaving the snowpack directly to the 
!                                                    !soil, ISBA-ES: [kg/(m2 s)] (equal to ZSNOW_THRUFAL
!                                                    !if OMEB_LITTER=False and zero if OMEB_LITTER=True)
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZALB3L   !Snow albedo at t-dt for total albedo calculation (ES/CROCUS)
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZRI3L    !Snow Ridcharson number (ES/CROCUS)
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZQS3L    ! surface humidity (kg/kg) (ES/CROCUS)
!
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZVEG
!
REAL, DIMENSION(SIZE(PEK%XWR),SIZE(PABC)) :: ZIACAN_SHADE, ZIACAN_SUNLIT
!                                          ! absorbed PAR of each level within the
!                                          ! canopy - Split into shaded and SUNLIT
REAL, DIMENSION(SIZE(PEK%XWR),SIZE(PABC)) :: ZFRAC_SUN  ! fraction of sunlit leaves
!
! ISBA-DF:
!                                                              
REAL, DIMENSION(SIZE(PEK%XWG,1),SIZE(PEK%XWG,2)) :: ZSOILHCAPZ ! ISBA-DF Soil heat capacity profile [J/(m3 K)]
REAL, DIMENSION(SIZE(PEK%XWG,1),SIZE(PEK%XWG,2)) :: ZSOILCONDZ ! ISBA-DF Soil conductivity profile  [W/(m K)]
!
REAL, DIMENSION(SIZE(PEK%XWR))               :: ZGRNDFLUX  ! snow/soil-biomass interface flux (W/m2)
REAL, DIMENSION(SIZE(PEK%XWR))               :: ZFLSN_COR  ! snow/soil-biomass correction flux (W/m2)
!
! MEB:
!
REAL, DIMENSION(SIZE(PEK%XWR))           :: ZSUBVCOR  ! A possible snow (intercepted by the canopy) mass correction 
!                                                       (to be potentially removed from soil) when MEB activated (kg/m2/s)
REAL, DIMENSION(SIZE(PEK%XWR))           :: ZLITCOR   ! A possible ice (in litter layer) mass correction 
!                                                       (to be potentially removed from soil) when litter activated (kg/m2/s)
!
! Crocus :
!
REAL, DIMENSION(SIZE(PZENITH,1))        :: ZZENITH
REAL, DIMENSION(SIZE(PEK%XWR))           :: ZANGL_ILLUM ! BC : moved here from snow3L_isba.F90
!                                           ZANGL_ILLUM  = Effective illumination angle, angle between the normal to the ground and the sun (=zenith for flat simulation)
!                                                          used only in TARTES for now
!
!                                                       (to be potentially removed from soil) when litter activated (kg/m2/s)
!
! Others :
!
LOGICAL, DIMENSION(SIZE(PEK%XWR))        :: GSHADE         ! mask where evolution occurs  
!
REAL, DIMENSION(SIZE(PEK%XWR))           :: ZDELHEATV_SFC  ! Change in heat storage of the explicit vegetation (MEB) layer over the current time step (W m-2)
REAL, DIMENSION(SIZE(PEK%XWR))           :: ZSNOWSFCH      ! snow surface layer pseudo-heating term owing to changes in grid thickness            (W m-2)
!
REAL, DIMENSION(SIZE(PEK%XWR))           :: ZEMIST, ZALBT  ! Necessary to close the energy budget between surfex and the atmosphere
REAL, DIMENSION(SIZE(PEK%XWR))           :: ZZHV, ZLEG_DELTA, ZLEGI_DELTA
!
INTEGER                                  :: JJ ! BC Loop control B
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA',0,ZHOOK_HANDLE)
!
!*      1.0    Preliminaries
!              -------------
!
DMK%XC1(:)      = XUNDEF
DMK%XC2(:)      = XUNDEF
DMK%XWGEQ(:)    = XUNDEF
ZCS(:)          = XUNDEF
!
ZEMIST      (:) = XUNDEF
ZALBT       (:) = XUNDEF
ZRI3L       (:) = XUNDEF
!
ZSOILHCAPZ(:,:) = XUNDEF
ZSOILCONDZ(:,:) = XUNDEF
ZEVAP3L   (:)   = XUNDEF
!
DMK%XRS       (:)   = 0.0
PAC_AGG       (:)   = 0.0
PHU_AGG       (:)   = 0.0
DMK%XSNOWTEMP (:,:) = XTT
DEK%XMELT     (:)   = 0.0
DK%XARES      (:)   = 0.0
!
DEK%XDELHEATG     (:) = 0.0 
DEK%XDELHEATG_SFC (:) = 0.0
DEK%XDELPHASEG    (:) = 0.0 
DEK%XDELPHASEG_SFC(:) = 0.0 
DEK%XDELHEATN     (:) = 0.0
DEK%XDELHEATN_SFC (:) = 0.0
DEK%XDELPHASEN    (:) = 0.0
DEK%XDELPHASEN_SFC(:) = 0.0
DEK%XRESTOREN     (:) = 0.0
DEK%XMELTSTOT     (:) = 0.0
DEK%XSNREFREEZ    (:) = 0.0
!
! MEB:
!
ZDELHEATV_SFC (:) = 0.0
ZSNOWSFCH     (:) = 0.0
ZSNOW_THRUFAL (:) = 0.0
!
ZSUBVCOR(:)     = 0.0
ZLITCOR(:)     = 0.0
ZLES3L          = 0.0
ZLEL3L          = 0.0
!
IF(OMEB)THEN
  ZVEG(:)           = 0.0
  DEK%XLEG(:)       = 0.0
  DEK%XLEGI(:)      = 0.0
  DEK%XLELITTER(:)  = 0.0
  DEK%XLELITTERI(:) = 0.0
ELSE
  ZVEG(:) = PEK%XVEG(:)
ENDIF
!
! Save snow albedo values at beginning of time step for total albedo calculation
!
ZALB3L(:)=PEK%TSNOW%ALB(:)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! MUST BE put in tarte in Crocus : 
!
ZANGL_ILLUM(:) = 0.0
ZZENITH(:) = PZENITH(:)
!
IF (PEK%TSNOW%SCHEME=='CRO'.AND.IO%CSNOWRAD=='T17') THEN 
!
   ZANGL_ILLUM(:) = PZENITH(:) ! BC
!
DO JJ=1,SIZE(ZZENITH,1) ! BC computation of illuminaiton angle from Tuzet calc.
   !
   ZANGL_ILLUM(JJ) = ACOS( MIN(1.0_JPRB,MAX(-1.0_JPRB, COS(ZZENITH(JJ))*COS(ACOS(MIN(1.0_JPRB,MAX(-1.0_JPRB,PDIRCOSZW(JJ))))) + &
      SIN(ZZENITH(JJ))*SIN(ACOS(MIN(1.0_JPRB,MAX(-1.0_JPRB,PDIRCOSZW(JJ))))*COS(PAZIM(JJ)-(PSLOPE_DIR(JJ)*XPI/180.0_JPRB))) ))) !Compute the effective illumination angle    
   !
ENDDO
!
ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!-------------------------------------------------------------------------------
!
!*      2.0    Soil parameters
!              ---------------
!
IF(IO%CISBA =='2-L' .OR. IO%CISBA == '3-L')THEN
  !
  CALL SOIL (IO, KK, PK, PEK, DMK, ZVEG, ZCS, ZFROZEN1, PFFG_NOSNOW, PFFV_NOSNOW )  
  !
ELSE
  !
  CALL SOILDIF (IO, KK, PK, PEK, DMK, ZVEG, ZFROZEN1, PFFG_NOSNOW, PFFV_NOSNOW, &
                ZSOILCONDZ, ZSOILHCAPZ, PCVHEATF                                )
  !
ENDIF
!
!-------------------------------------------------------------------------------

!
!*      3.0    Plant stress due to soil water deficit
!              --------------------------------------
!
CALL SOILSTRESS(KK, PK, PEK, IO%CISBA, DMK%XF2, DMK%XF2WGHT, ZF5)  
!
!------------------------------------------------------------------------------- 
!
IF(OMEB)THEN
  !
  !*      4.0    ISBA Explicit Canopy Vegetation scheme (MEB)
  !              --------------------------------------------
  !
  CALL ISBA_MEB(IO, KK, PK, PEK, DK, DEK, DMK, G, AG,                         &
                TPTIME, OMEB, GSHADE, HIMPLICIT_WIND, PTSTEP,                 &
                ZSOILHCAPZ, ZSOILCONDZ, ZFROZEN1, PPS, ZZENITH,ZANGL_ILLUM,   &
                PSCA_SW, PSW_RAD, PVMOD, PVDIR, PRR, PSR, PRHOA, PTA, PQA,    &
                PDIRCOSZW, PSLOPE_DIR, PEXNS, PEXNA, PPET_A_COEF, PPET_B_COEF,&
                PPEQ_A_COEF, PPEQ_B_COEF, PPEW_A_COEF, PPEW_B_COEF, AT,       &
                PZREF, PUREF, PZ0G_WITHOUT_SNOW, PZ0_MEBV, PZ0H_MEBV,         &
                PZ0EFF_MEBV, PZ0_MEBN, PZ0H_MEBN, PZ0EFF_MEBN,                & 
                PALBNIR_TVEG, PALBVIS_TVEG,PALBNIR_TSOIL, PALBVIS_TSOIL,      &
                PABC, PIACAN, PPOI, NPAR_VEG_IRR_USE, PCSP,                   &
                PRESP_BIOMASS_INST, PPALPHAN, DMK%XF2, PLW_RAD, ZGRNDFLUX,    &
                ZFLSN_COR, PUSTAR, ZEMIST, PHU_AGG, PAC_AGG, ZDELHEATV_SFC,   &
                PTDEEP_A, PDEEP_FLUX, ZRI3L, ZSNOW_THRUFAL,                   &
                ZSNOW_THRUFAL_SOIL, ZEVAPCOR, ZSUBVCOR, ZLITCOR, ZSNOWSFCH,   &
                ZQS3L, KTAB_SYT, P_DIR_SW, P_SCA_SW, PIMPWET, PIMPDRY,        &
                PRN_SHADE, PRN_SUNLIT, PBLOWSNW_FLUX, PBLOWSNW_CONC)
  !
ELSE
  !
  !*      5.0    ISBA Composit scheme (Original)
  !              -------------------------------
  !
  !* 5.1 Radiative transfert
  !      -------------------
  !
  IF (IO%LTR_ML) THEN
    CALL RADIATIVE_TRANSFERT(PK%XVEGTYPE_PATCH, PALBVIS_TVEG,                       &
                             PALBVIS_TSOIL, PALBNIR_TVEG, PALBNIR_TSOIL, PSW_RAD,   &
                             PEK%XLAI, ZZENITH, PABC, PEK%XFAPARC, PEK%XFAPIRC,     &
                             PEK%XMUS, PEK%XLAI_EFFC, GSHADE, PIACAN, ZIACAN_SUNLIT,&
                             ZIACAN_SHADE, ZFRAC_SUN, DMK%XFAPAR, DMK%XFAPIR,       &
                             DMK%XFAPAR_BS, DMK%XFAPIR_BS, NPAR_VEG_IRR_USE,        &
                             PRN_SHADE, PRN_SUNLIT)
  ENDIF
  !
  !
  !* 5.2 Fraction of leaves occupied by intercepted water
  !      ------------------------------------------------
  !
  CALL WET_LEAVES_FRAC(PEK%XWR, PEK%XVEG, PEK%XWRMAX_CF, DK%XZ0, PEK%XLAI, ZWRMAX, ZDELTA)
  !
  !
  !* 5.3   Explicit snow scheme
  !        --------------------
  !
  CALL SNOW3L_ISBA(IO, G, PK, PEK, DK, DEK, DMK, OMEB, HIMPLICIT_WIND,        &
                   TPTIME, PTSTEP, PK%XVEGTYPE_PATCH, PEK%XTG, DMK%XCT,       &
                   ZSOILHCAPZ,ZSOILCONDZ(:,1), PPS, PTA, PSW_RAD, PQA,        &
                   PVMOD, PVDIR, PLW_RAD, PRR,PSR, PRHOA, PUREF, PEXNS,       &
                   PEXNA, PDIRCOSZW, PSLOPE_DIR, PZREF, PEK%XSNOWFREE_ALB,    &
                   PK%XDG, PK%XDZG, PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF,    &
                   PPEQ_A_COEF,PPET_B_COEF, PPEQ_B_COEF, ZSNOW_THRUFAL_SOIL,  &
                   ZGRNDFLUX, ZFLSN_COR, ZEVAPCOR, ZLES3L, ZLEL3L,            &
                   ZEVAP3L, ZSNOWSFCH, ZRI3L, ZZENITH, ZANGL_ILLUM, ZQS3L,    &
                   NPAR_VEG_IRR_USE, KTAB_SYT, P_DIR_SW, P_SCA_SW, PIMPWET,   &
                   PIMPDRY, PBLOWSNW_FLUX, PBLOWSNW_CONC                      )
  !
  !
  !* 5.4   Plant stress, stomatal resistance and, possibly, CO2 assimilation
  !          -----------------------------------------------------------------
  !
  IF (IO%CPHOTO=='NON') THEN
     !
     CALL VEG(PSW_RAD, PTA, PQA, PPS, PEK%XRGL, PEK%XLAI, PEK%XRSMIN, PEK%XGAMMA, DMK%XF2, DMK%XRS)
     !
  ELSE IF (MAXVAL(PEK%XGMES(:)).NE.XUNDEF .OR. MINVAL(PEK%XGMES(:)).NE.XUNDEF) THEN
     !
     ZQSAT(:)=QSAT(PEK%XTG(:,1),PPS(:))  
     !
     CALL COTWORES(PTSTEP, IO, GSHADE, PK, PEK, PK%XDMAX, PPOI, PCSP, PEK%XTG(:,1),           &
                   DMK%XF2, PSW_RAD, PQA, ZQSAT, PEK%XPSN, PEK%XPSNV, ZDELTA, PRHOA, PZENITH, &
                   KK%XFFV, NPAR_VEG_IRR_USE, ZIACAN_SUNLIT, ZIACAN_SHADE, ZFRAC_SUN,         &
                   PIACAN, PABC, DMK%XRS, DEK%XGPP, PRESP_BIOMASS_INST(:,1)                   )
     !
  ELSE
     !
     DEK%XGPP          (:  ) = 0.0
     PRESP_BIOMASS_INST(:,1) = 0.0
     !
  ENDIF
  !
  !
  !* 5.5 ISBA Composit Energy Budget
  !        ---------------------------
  !
  CALL ISBA_CEB(IO, KK, PK, PEK, DK, DEK, DMK,                           &
                HIMPLICIT_WIND, PTSTEP, PPEW_A_COEF,                     &
                PPEW_B_COEF, PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF,      &
                PPEQ_B_COEF, AT, PSW_RAD, PLW_RAD, PEXNS, PEXNA, PTA,    &
                PVMOD, PQA, PRR, PSR, PPS, PZREF, PUREF, PDIRCOSZW,      &
                ZF5, PFFG_NOSNOW, PFFV_NOSNOW, PRHOA, ZCS,               &
                ZSOILCONDZ, ZSOILHCAPZ, ZFROZEN1, PTDEEP_A,              &
                ZGRNDFLUX, ZFLSN_COR, ZSNOW_THRUFAL_SOIL, ZDELTA, ZHUGI, &
                ZALBT, ZEMIST, PDEEP_FLUX, PUSTAR, PAC_AGG, PHU_AGG )
  !
  !*******************************************************************************
  ! WARNING: at this stage, all fluxes have two different meanings according
  !          to the ISBA snow-scheme option:
  !  'D95' : they represent aggregated (snow + flood + snow-flood-free) fluxes
  !  '3-L' : they represent                    flood + snow-flood-free  fluxes
  !
  ! The variables concerned by this are: PRN, PH, PLE, PLEI, DEK%XLEG, DEK%XLEGI, DEK%XLEV, DEK%XLES, 
  !                                      DEK%XLER, DEK%XLETR, PEVAP, PUSTAR, PGFLUX
  !*******************************************************************************
  !
ENDIF
!
!*      6.0    Irrigation decision rules 
!              -------------------------
!
IF ( LIRRIGMODE ) THEN
  IF ( ANY(PEK%XIRRIGTYPE /= 0 ) ) THEN
    CALL IRRIGATION_TRIGGER(AG, PEK, DMK%XF2, PTSTEP, TPTIME)
  ELSE
    AG%LIRRIGATE = .FALSE. 
  ENDIF
ENDIF
!
!-------------------------------------------------------------------------------
!
!*      7.0    Water transfers and phase change in the soil
!              --------------------------------------------
!
CALL HYDRO(IO, KK, PK, PEK, AG, DEK, DMK,                         &
           OMEB, PTSTEP, ZVEG, ZWRMAX, ZSNOW_THRUFAL_SOIL,        &
           ZEVAPCOR, ZSUBVCOR, PLETR_HVEG, ZSOILHCAPZ,            &
           DMK%XF2WGHT, DMK%XF2, PPS, PIRRIG_GR, NPAR_VEG_IRR_USE )
!
!-------------------------------------------------------------------------------
!
!*      8.0    Aggregated output fluxes and diagnostics
!              -----------------------------------------
!
!* add snow component to output radiative parameters and fluxes in case 
!  of ES or CROCUS snow schemes
!
CALL ISBA_SNOW_AGR(KK, PK, PEK, DMK, DK, DEK, AT,                &
                   OMEB, IO%LMEB_LITTER, PEXNS, PEXNA, PTA, PQA, &
                   PZREF, PUREF, PDIRCOSZW, PVMOD, PRR, PSR,     &
                   ZEMIST, ZALBT, PUSTAR, ZLES3L, ZLEL3L,        &
                   ZEVAP3L, ZQS3L, ZALB3L, ZGRNDFLUX, ZFLSN_COR, &
                   PEMIST, PPALPHAN, ZSNOWSFCH                   )  
!
!
! NOTE: To check surface energy conservation for isba, the error in W m-2 is defined HERE as
!
! surface isba error = DEK%XDELHEATG_SFC(:)+DEK%XDELHEATN_SFC(:)+DEK%XDELPHASEG_SFC(:)+DEK%XDELPHASEN_SFC(:) &
!                    - DK%XRN(:)+DK%XH(:)+DK%XLE(:)+DEK%XRESTORE(:)+DEK%XRESTOREN(:)
!
! NOTE: To check soil energy conservation for isba dif, the error in W m-2 is defined HERE as
!
! total soil error   = DEK%XDELHEATG(:)+DEK%XDELPHASEG(:)-(1.0-PEK%XPSN(:))*(DEK%XRN_SN_FR(:)-DEK%XH_SN_FR(:)) &
!                    + (DEK%XLEG(:)+DEK%XLEGI(:)+DEK%XLEV(:)+DEK%XLE_FLOOD(:))-DEK%XGRNDFLUX(:)
!
! surface soil error = DEK%XDELHEATG_SFC(:)+DEK%XDELPHASEG_SFC(:)-(1.0-PEK%XPSN(:))*(DEK%XRN_SN_FR(:)-DEK%XH_SN_FR(:)) &
!                    + (DEK%XLEG(:)+DEK%XLEGI(:)+DEK%XLEV(:)+DEK%XLE_FLOOD(:))+DEK%XRESTORE(:)
!
!
!***************************************************************************
! All output fluxes and radiative variables have recovered the same physical
! meaning, that is they are aggregated quantities (snow + snow-free)
!***************************************************************************
!
IF (LHOOK) CALL DR_HOOK('ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ISBA
