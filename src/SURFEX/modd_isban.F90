!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!##################
MODULE MODD_ISBA_n
!##################
!
!!****  *MODD_ISBA - declaration of packed surface parameters for ISBA scheme
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
!!      A. Boone   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       20/09/02
!!      A.L. Gibelin    04/2009 : BIOMASS and RESP_BIOMASS arrays 
!!      A.L. Gibelin    04/2009 : TAU_WOOD for NCB option 
!!      A.L. Gibelin    05/2009 : Add carbon spinup
!!      A.L. Gibelin    06/2009 : Soil carbon variables for CNT option
!!      A.L. Gibelin    07/2009 : Suppress RDK and transform GPP as a diagnostic
!!      A.L. Gibelin    07/2009 : Suppress PPST and PPSTF as outputs
!!      P. Samuelsson   02/2012 : MEB
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TYPE_DATE_SURF
USE MODD_TYPE_SNOW
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE ISBA_t
!-------------------------------------------------------------------------------
!
! ISBA Scheme Options:
!
  CHARACTER(LEN=4)               :: CROUGH   ! type of roughness length
                                           ! 'Z01D'
                                           ! 'Z04D'
  CHARACTER(LEN=3)               :: CISBA    ! type of ISBA version:
!                                          ! '2-L' (default)
!                                          ! '3-L'
!                                          ! 'DIF'
!
  CHARACTER(LEN=4)               :: CPEDOTF! NOTE: Only used when HISBA = DIF
!                                          ! 'CH78' = Clapp and Hornberger 1978 for BC (Default)
!                                          ! 'CO84' = Cosby et al. 1988 for BC
!
  CHARACTER(LEN=3)               :: CPHOTO   ! type of photosynthesis
!                                          ! 'NON'
!                                          ! 'AGS'
!                                          ! 'LAI'
!                                          ! 'LST'
!                                          ! 'AST'
!                                          ! 'NIT'
!                                          ! 'NCB'
  LOGICAL                        :: LTR_ML ! new radiative transfert
  REAL                           :: XRM_PATCH ! threshold to remove little fractions of patches 
  CHARACTER(LEN=4)               :: CALBEDO  ! albedo type
!                                          ! 'DRY ' 
!                                          ! 'EVOL' 
!                                          ! 'WET ' 
!                                          ! 'USER' 
  CHARACTER(LEN=4)               :: CSCOND   ! Thermal conductivity
!                                          ! 'DEF ' = DEFault: NP89 implicit method
!                                          ! 'PL98' = Peters-Lidard et al. 1998 used
!                                          ! for explicit computation of CG
  CHARACTER(LEN=4)               :: CC1DRY   ! C1 formulation for dry soils
!                                          ! 'DEF ' = DEFault: Giard-Bazile formulation
!                                          ! 'GB93' = Giordani 1993, Braud 1993 
!                                          !discontinuous at WILT
  CHARACTER(LEN=3)               :: CSOILFRZ ! soil freezing-physics option
!                                          ! 'DEF' = Default (Boone et al. 2000; 
!                                          !        Giard and Bazile 2000)
!                                          ! 'LWT' = Phase changes as above,
!                                          !         but relation between unfrozen 
!                                          !         water and temperature considered
!                            NOTE that when using the YISBA='DIF' multi-layer soil option,
!                            the 'LWT' method is used. It is only an option
!                            when using the force-restore soil method ('2-L' or '3-L')
!
  CHARACTER(LEN=4)               :: CDIFSFCOND ! Mulch effects
!                                          ! 'MLCH' = include the insulating effect of
!                                          ! leaf litter/mulch on the surf. thermal cond.
!                                          ! 'DEF ' = no mulch effect
!                           NOTE: Only used when YISBA = DIF
!
  CHARACTER(LEN=3)               :: CSNOWRES ! Turbulent exchanges over snow
!                                          ! 'DEF' = Default: Louis (ISBA)
!                                          ! 'RIL' = Maximum Richardson number limit
!                                          !         for stable conditions ISBA-SNOW3L
!                                          !         turbulent exchange option
!                                           
  CHARACTER(LEN=3)               :: CRESPSL  ! Soil respiration
!                                          ! 'DEF' = Default: Norman (1992)
!                                          ! 'PRM' = New Parameterization
!                                          ! 'CNT' = CENTURY model (Gibelin 2007)
!                                           
  CHARACTER(LEN=3)               :: CCPSURF! specific heat at surface
!                                          ! 'DRY' = default value (dry Cp)
!                                          ! 'HUM' = Cp as a fct of specific humidity
!
  LOGICAL                        :: LTEMP_ARP ! True  = time-varying force-restore soil temperature (as in ARPEGE)
                                              ! False = No time-varying force-restore soil temperature (Default)
!
  LOGICAL                        :: LGLACIER ! True = Over permanent snow and ice, 
!                                                     initialise WGI=WSAT,
!                                                     Hsnow>=10m and allow 0.8<SNOALB<0.85
                                             ! False = No specific treatment
  LOGICAL, POINTER, DIMENSION(:) :: LMEB_PATCH ! Vector with T/F values
                                               ! True = treat patch with multi-energy balance 
!                                              ! False = treat patch with classical ISBA 
  LOGICAL                        :: LFORC_MEASURE ! True = Forcing data from observations
!                                                         ! False = Forcing data from atmospheric model (default)
!
  LOGICAL                        :: LMEB_LITTER ! Activate Litter
  LOGICAL                        :: LMEB_GNDRES ! Activate Ground Resistance
!
  LOGICAL                        :: LVEGUPD  ! True = update vegetation parameters every decade
                                             ! False = keep vegetation parameters constant in time
!  
  LOGICAL                        :: LNITRO_DILU ! nitrogen dilution fct of CO2 (Calvet et al. 2008)
!
  LOGICAL                        :: LPERTSURF ! True  = apply random perturbations for ensemble prediction
                                              ! False = no random perturbation (default)
!-------------------------------------------------------------------------------
! Snow options
  LOGICAL                        :: LSNOWDRIFT, LSNOWDRIFT_SUBLIM ! Logicals for snowdrift and sublimation
!
  LOGICAL                        :: LSNOW_ABS_ZENITH ! if True modify solar absorption as a function of solar zenithal angle
                                                     ! (physically wrong but better results in polar regions when CSNOWRAD=B92)
! Scheme of snow metamorphism (Crocus)
  CHARACTER(3)                   :: CSNOWMETAMO ! B92 (historical version, Brun et al 92), C13, T07, F06 (see Carmagnola et al 2014)
!
! radiative transfer scheme in snow (Crocus)
  CHARACTER(3)                   :: CSNOWRAD ! B92 (historical version, Brun et al 92), TAR, TA1, TA2 (see Libois et al 2013)
!-------------------------------------------------------------------------------
!
  LOGICAL                        :: LCANOPY ! T: SBL scheme within the canopy
!                                           ! F: no atmospheric layers below forcing level
  LOGICAL                        :: LCANOPY_DRAG ! T: drag activated in SBL scheme within the canopy
!                                                ! F: no drag activated in SBL atmospheric layers
!-------------------------------------------------------------------------------
!
! type of initialization of vegetation: from cover types (ecoclimap) or parameters prescribed
!
  LOGICAL                        :: LECOCLIMAP ! T: parameters computed from ecoclimap
!                                              ! F: they are read in the file
!
  LOGICAL                        :: LCTI       ! Topographic index data
  LOGICAL                        :: LSOCP      ! Soil organic carbon profile data
  LOGICAL                        :: LPERM      ! Permafrost distribution data
  LOGICAL                        :: LGW        ! Groudwater distribution data
  LOGICAL                        :: LNOF  
!-------------------------------------------------------------------------------
!
! Soil and wood carbon spin up 
!
  LOGICAL                        :: LSPINUPCARBS  ! T: do the soil carb spinup, F: no
  LOGICAL                        :: LSPINUPCARBW  ! T: do the wood carb spinup, F: no  
  LOGICAL                        :: LAGRI_TO_GRASS! During soil carbon spinup with ISBA-CC, 
                                                  ! grass parameters are attributed to all agricultural PFT
!  
  REAL                           :: XSPINMAXS     ! max number of times CARBON_SOIL subroutine is
                                                  ! called for each timestep in simulation during
                                                  ! acceleration procedure number                             
  REAL                           :: XSPINMAXW     ! max number of times the wood is accelerated  
  REAL                           :: XCO2_START    ! Pre-industrial CO2 concentration
  REAL                           :: XCO2_END      ! Begin-transient CO2 concentration
  INTEGER                        :: NNBYEARSPINS  ! nbr years needed to reaches soil equilibrium 
  INTEGER                        :: NNBYEARSPINW  ! nbr years needed to reaches wood equilibrium
  INTEGER                        :: NNBYEARSOLD   ! nbr years executed at curent time step
  INTEGER                        :: NSPINS        ! number of times the soil is accelerated
  INTEGER                        :: NSPINW        ! number of times the wood is accelerated
!
!-------------------------------------------------------------------------------
!
! Mask and number of grid elements containing patches/tiles:
!
  INTEGER, POINTER, DIMENSION(:)   :: NSIZE_NATURE_P ! number of sub-patchs/tiles              (-)
  INTEGER, POINTER, DIMENSION(:,:) :: NR_NATURE_P    ! patch/tile mask                         (-)
  REAL, POINTER, DIMENSION(:,:)    :: XPATCH         ! fraction of each tile/patch             (-)
  REAL, POINTER, DIMENSION(:,:)    :: XPATCH_OLD     ! fraction of each tile/patch for land use
  REAL, POINTER, DIMENSION(:,:)    :: XVEGTYPE       ! fraction of each vegetation type for
!                                                      ! each grid mesh                          (-)
  REAL, POINTER, DIMENSION(:,:,:)  :: XVEGTYPE_PATCH ! fraction of each vegetation type for
!                                                      ! each vegetation unit/patch              (-)
  INTEGER                          :: NPATCH           ! maximum number of sub-tiles (patches)
!                                                      ! used at any grid point within a 
!                                                      ! natural surface fraction
  INTEGER                          :: NGROUND_LAYER    ! number of ground layers
!
  REAL, POINTER, DIMENSION(:)      :: XSOILGRID      ! Soil layer grid as reference for DIF
!
  INTEGER                          :: NTEMPLAYER_ARP ! Number of force-restore soil temperature layer, including Ts (Default = 4)
                                                     ! Only used if LTEMP_ARP=True
!
  REAL, POINTER, DIMENSION(:)      ::  XSODELX       ! Pulsation for each layer (Only used if LTEMP_ARP=True)
!
  INTEGER                              :: NNBIOMASS    ! number of biomass pools
  INTEGER                              :: NNLITTER     ! number of litter pools
  INTEGER                              :: NNLITTLEVS   ! number of litter levels
  INTEGER                              :: NNSOILCARB   ! number of soil carbon pools  
!
!-------------------------------------------------------------------------------
!
! General surface parameters:
!
  REAL, POINTER, DIMENSION(:)   :: XZS               ! relief                                  (m)
  REAL, POINTER, DIMENSION(:,:) :: XCOVER            ! fraction of each ecosystem              (-)
  LOGICAL, POINTER, DIMENSION(:):: LCOVER            ! GCOVER(i)=T --> ith cover field is not 0.
!
! Averaged Surface radiative parameters:
!
  REAL, POINTER, DIMENSION(:)   :: XALBNIR_DRY       ! dry soil near-infra-red albedo          (-)
  REAL, POINTER, DIMENSION(:)   :: XALBVIS_DRY       ! dry soil visible albedo                 (-)
  REAL, POINTER, DIMENSION(:)   :: XALBUV_DRY        ! dry soil UV albedo                      (-)
  REAL, POINTER, DIMENSION(:)   :: XALBNIR_WET       ! wet soil near-infra-red albedo          (-)
  REAL, POINTER, DIMENSION(:)   :: XALBVIS_WET       ! wet soil visible albedo                 (-)
  REAL, POINTER, DIMENSION(:)   :: XALBUV_WET        ! wet soil UV albedo                      (-)
  REAL, POINTER, DIMENSION(:,:) :: XALBNIR_SOIL      ! soil near-infra-red albedo              (-)
  REAL, POINTER, DIMENSION(:,:) :: XALBVIS_SOIL      ! soil visible albedo                     (-)
  REAL, POINTER, DIMENSION(:,:) :: XALBUV_SOIL       ! soil UV albedo                          (-)
  REAL, POINTER, DIMENSION(:)   :: XEMIS_NAT         ! patch averaged emissivity               (-)
  REAL, POINTER, DIMENSION(:)   :: XTSRAD_NAT        ! patch averaged radiative temperature    (K)
!
! Subgrid orography parameters
!
  REAL, DIMENSION(:), POINTER :: XAOSIP,XAOSIM,XAOSJP,XAOSJM
! directional A/S quantities in 4 coordinate directions
! (IP: i index up;  IM: i index down;  JP: j index up;  JM: j index down)
! They are used in soil routines to compute effective roughness length
!
  REAL, DIMENSION(:), POINTER :: XHO2IP,XHO2IM,XHO2JP,XHO2JM
! directional h/2 quantities in 4 coordinate directions
! (IP: i index up;  IM: i index down;  JP: j index up;  JM: j index down)
! They are used in soil routines to compute effective roughness length
!
  REAL, DIMENSION(:,:), POINTER :: XZ0EFFIP,XZ0EFFIM,XZ0EFFJP,XZ0EFFJM
! directional total roughness lenghts in 4 coordinate directions
! (IP: i index up;  IM: i index down;  JP: j index up;  JM: j index down)
!
  REAL, DIMENSION(:), POINTER   :: XZ0EFFJPDIR    ! heading of J direction (deg from N clockwise)
!
  REAL, DIMENSION(:), POINTER   :: XZ0REL         ! relief roughness length                 (m)
!
  REAL, DIMENSION(:), POINTER   :: XSSO_SLOPE     ! slope of S.S.O.                         (-)
  REAL, DIMENSION(:), POINTER   :: XSSO_STDEV     ! relief  standard deviation              (m)
!-------------------------------------------------------------------------------
!
! Input Parameters, per patch:
!
! - vegetation + bare soil:
!
  REAL, POINTER, DIMENSION(:,:) :: XZ0_O_Z0H         ! ratio of surface roughness lengths
!                                                      ! (momentum to heat)                      (-)
  REAL, POINTER, DIMENSION(:,:) :: XALBNIR           ! near-infra-red albedo                   (-)
  REAL, POINTER, DIMENSION(:,:) :: XALBVIS           ! visible albedo                          (-)
  REAL, POINTER, DIMENSION(:,:) :: XALBUV            ! UV albedo                               (-)
  REAL, POINTER, DIMENSION(:,:) :: XEMIS             ! surface emissivity                      (-)
  REAL, POINTER, DIMENSION(:,:) :: XZ0               ! surface roughness length                (m)
!
! - vegetation:
!
  REAL, POINTER, DIMENSION(:,:) :: XALBNIR_VEG       ! vegetation near-infra-red albedo        (-)
  REAL, POINTER, DIMENSION(:,:) :: XALBVIS_VEG       ! vegetation visible albedo               (-)
  REAL, POINTER, DIMENSION(:,:) :: XALBUV_VEG        ! vegetation UV albedo                    (-)
!
! - vegetation: default option (Jarvis) and general parameters:
!
  REAL, POINTER, DIMENSION(:,:) :: XVEG              ! vegetation cover fraction               (-)
  REAL, POINTER, DIMENSION(:,:) :: XWRMAX_CF         ! coefficient for maximum water 
!                                                      ! interception 
!                                                      ! storage capacity on the vegetation      (-)
  REAL, POINTER, DIMENSION(:,:) :: XRSMIN            ! minimum stomatal resistance             (s/m)
  REAL, POINTER, DIMENSION(:,:) :: XGAMMA            ! coefficient for the calculation
!                                                      ! of the surface stomatal
!                                                      ! resistance
  REAL, POINTER, DIMENSION(:,:) :: XCV               ! vegetation thermal inertia coefficient  (K m2/J)
  REAL, POINTER, DIMENSION(:,:) :: XRGL              ! maximum solar radiation
!                                                      ! usable in photosynthesis                (W/m2)
  REAL, POINTER, DIMENSION(:,:,:) :: XROOTFRAC       ! root fraction profile ('DIF' option)
!
! - Multi-energy balance (MEB) parameters.
! - Postfix GV denotes understory ground vegetation
!
  REAL, POINTER, DIMENSION(:,:) :: XGNDLITTER        ! ground litter fraction                  (-)
  REAL, POINTER, DIMENSION(:,:) :: XRGLGV            ! understory veg maximum solar radiation
!                                                    ! usable in photosynthesis                       (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XGAMMAGV          ! understory veg coefficient for the calculation
!                                                    ! of the surface stomatal resistance
  REAL, POINTER, DIMENSION(:,:) :: XRSMINGV          ! understory veg minimum stomatal resistance     (s/m)
  REAL, POINTER, DIMENSION(:,:,:) :: XROOTFRACGV     ! understory veg root fraction profile
  REAL, POINTER, DIMENSION(:,:) :: XWRMAX_CFGV       ! understory veg coefficient for maximum water 
!                                                    ! interception
  REAL, POINTER, DIMENSION(:,:) :: XLAIGV            ! understory veg Leaf Area Index                 (m2/m2)
  REAL, POINTER, DIMENSION(:,:) :: XZ0LITTER         ! ground litter roughness length                 (m)
!
  REAL, POINTER, DIMENSION(:,:) :: XH_VEG            ! height of vegetation                           (m)
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags parameters ('AGS', 'LAI', 'AST', 'LST', 'NIT', 'NCB' options)
!
  REAL, POINTER, DIMENSION(:)      :: XABC          ! abscissa needed for integration
!                                                   ! of net assimilation and stomatal
!                                                   ! conductance over canopy depth           (-)
  REAL, POINTER, DIMENSION(:)      :: XPOI          ! Gaussian weights for integration
!                                                   ! of net assimilation and stomatal
!                                                   ! conductance over canopy depth           (-)
  REAL, POINTER, DIMENSION(:,:)    :: XBSLAI        ! ratio d(biomass)/d(lai)                 (kg/m2)
  REAL, POINTER, DIMENSION(:,:)    :: XLAIMIN       ! minimum LAI (Leaf Area Index)           (m2/m2)
  REAL, POINTER, DIMENSION(:,:)    :: XSEFOLD       ! e-folding time for senescence           (s)
  REAL, POINTER, DIMENSION(:,:)    :: XTAU_WOOD     ! residence time in woody biomass         (s)  
  REAL, POINTER, DIMENSION(:,:)    :: XH_TREE       ! height of trees                         (m)
  REAL, POINTER, DIMENSION(:,:)    :: XANF          ! total assimilation over canopy          (
  REAL, POINTER, DIMENSION(:,:)    :: XANMAX        ! maximum photosynthesis rate             (
  REAL, POINTER, DIMENSION(:,:)    :: XFZERO        ! ideal value of F, no photo- 
!                                                     ! respiration or saturation deficit       (
  REAL, POINTER, DIMENSION(:,:)    :: XEPSO         ! maximum initial quantum use             
!                                                     ! efficiency                              (mg J-1 PAR)
  REAL, POINTER, DIMENSION(:,:)    :: XGAMM         ! CO2 conpensation concentration          (ppm)
  REAL, POINTER, DIMENSION(:,:)    :: XQDGAMM       ! Log of Q10 function for CO2 conpensation 
!                                                     ! concentration                           (-)
  REAL, POINTER, DIMENSION(:,:)    :: XGMES         ! mesophyll conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:,:)    :: XRE25         ! Ecosystem respiration parameter         (kg/kg.m.s-1)
  REAL, POINTER, DIMENSION(:,:)    :: XQDGMES       ! Log of Q10 function for mesophyll conductance  (-)
  REAL, POINTER, DIMENSION(:,:)    :: XT1GMES       ! reference temperature for computing 
!                                                     ! compensation concentration function for 
!                                                     ! mesophyll conductance: minimum
!                                                     ! temperature                             (K)
  REAL, POINTER, DIMENSION(:,:)    :: XT2GMES       ! reference temperature for computing 
!                                                     ! compensation concentration function for 
!                                                     ! mesophyll conductance: maximum
!                                                     ! temperature                             (K)
  REAL, POINTER, DIMENSION(:,:)    :: XAMAX         ! leaf photosynthetic capacity            (mg m-2 s-1)
  REAL, POINTER, DIMENSION(:,:)    :: XQDAMAX       ! Log of Q10 function for leaf photosynthetic 
!                                                     ! capacity                                (-)
  REAL, POINTER, DIMENSION(:,:)    :: XT1AMAX       ! reference temperature for computing 
!                                                     ! compensation concentration function for 
!                                                     ! leaf photosynthetic capacity: minimum
!                                                     ! temperature                             (K)
  REAL, POINTER, DIMENSION(:,:)    :: XT2AMAX       ! reference temperature for computing 
!                                                     ! compensation concentration function for 
!                                                     ! leaf photosynthetic capacity: maximum
!                                                     ! temperature                             (K)
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Stress parameters ('AST', 'LST', 'NIT', 'NCB' options)
!
  LOGICAL, POINTER, DIMENSION(:,:) :: LSTRESS       ! vegetation response type to water
!                                                     ! stress (true:defensive false:offensive) (-)
  REAL, POINTER, DIMENSION(:,:)    :: XF2I          ! critical normilized soil water 
!                                                     ! content for stress parameterisation
  REAL, POINTER, DIMENSION(:,:)    :: XGC           ! cuticular conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:,:)    :: XAH           ! coefficients for herbaceous water stress 
!                                                     ! response (offensive or defensive)       (log(mm/s))
  REAL, POINTER, DIMENSION(:,:)    :: XBH           ! coefficients for herbaceous water stress 
!                                                     ! response (offensive or defensive)       (-)
  REAL, POINTER, DIMENSION(:,:)    :: XDMAX         ! maximum air saturation deficit
!                                                     ! tolerate by vegetation                  (kg/kg)
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Nitrogen-model parameters ('NIT', 'NCB' option)
!
  REAL, POINTER, DIMENSION(:,:)    :: XCE_NITRO       ! leaf aera ratio sensitivity to 
!                                                       ! nitrogen concentration                (m2/kg)
  REAL, POINTER, DIMENSION(:,:)    :: XCF_NITRO       ! lethal minimum value of leaf area
!                                                       ! ratio                                 (m2/kg)
  REAL, POINTER, DIMENSION(:,:)    :: XCNA_NITRO      ! nitrogen concentration of active 
!                                                       ! biomass                               (kg/kg)
  REAL, POINTER, DIMENSION(:,:)    :: XBSLAI_NITRO    ! biomass/LAI ratio from nitrogen 
!                                                       ! decline theory                        (kg/m2)
!
!-------------------------------------------------------------------------------
!
! - soil: primary parameters
!
  REAL, POINTER, DIMENSION(:,:)    :: XSAND          ! sand fraction                           (-)
  REAL, POINTER, DIMENSION(:,:)    :: XCLAY          ! clay fraction                           (-)
  REAL, POINTER, DIMENSION(:,:)    :: XSOC           ! soil organic carbon content             (kg/m2)
  REAL, POINTER, DIMENSION(:)      :: XPERM          ! permafrost distribution                 (-)
  REAL, POINTER, DIMENSION(:)      :: XGW            ! groundwater distribution                 (-)

  REAL, POINTER, DIMENSION(:)      :: XWDRAIN        ! continuous drainage parameter           (-)
  REAL, POINTER, DIMENSION(:)      :: XTAUICE        ! soil freezing characteristic timescale  (s)
  REAL, POINTER, DIMENSION(:)      :: XGAMMAT        ! 'Force-Restore' timescale when using a
!                                                    ! prescribed lower boundary temperature   (1/days)
  REAL, POINTER, DIMENSION(:,:,:)  :: XDG            ! soil layer depth                  (m)
!                                                    ! NOTE: in Force-Restore mode, the 
!                                                    ! uppermost layer depth is superficial
!                                                    ! and is only explicitly used for soil 
!                                                    ! water phase changes                     (m)
  REAL, POINTER, DIMENSION(:,:,:)  :: XDG_OLD        ! For land use
!
  REAL, POINTER, DIMENSION(:,:,:)  :: XDZG           ! soil layers thicknesses (DIF option)
  REAL, POINTER, DIMENSION(:,:,:)  :: XDZDIF         ! distance between consecuative layer mid-points (DIF option)
!
  INTEGER, POINTER, DIMENSION(:,:) :: NWG_LAYER      ! Number of soil moisture layers for DIF
  REAL, POINTER, DIMENSION(:,:)    :: XDROOT         ! effective root depth for DIF (m)
  REAL, POINTER, DIMENSION(:,:)    :: XDG2           ! root depth for DIF as 3-L (m)
!
  REAL, POINTER, DIMENSION(:)    :: XPH              ! soil pH
  REAL, POINTER, DIMENSION(:)    :: XFERT            ! soil fertilisation rate (kgN/ha/h)
!
!-------------------------------------------------------------------------------
!
! - soil: Secondary parameters: hydrology
!
  REAL, POINTER, DIMENSION(:,:)    :: XC1SAT         ! 'Force-Restore' C1 coefficient at 
!                                                    ! saturation                              (-)
  REAL, POINTER, DIMENSION(:,:)    :: XC2REF         ! 'Force-Restore' reference value of C2   (-)
  REAL, POINTER, DIMENSION(:,:,:)  :: XC3            ! 'Force-Restore' C3 drainage coefficient (m)
  REAL, POINTER, DIMENSION(:)      :: XC4B           ! 'Force-Restore' sub-surface vertical 
!                                                    ! diffusion coefficient (slope parameter) (-)
  REAL, POINTER, DIMENSION(:,:)    :: XC4REF         ! 'Force-Restore' sub-surface vertical 
!                                                    ! diffusion coefficient                   (-)
  REAL, POINTER, DIMENSION(:)      :: XACOEF         ! 'Force-Restore' surface vertical 
!                                                    ! diffusion coefficient                   (-)
  REAL, POINTER, DIMENSION(:)      :: XPCOEF         ! 'Force-Restore' surface vertical 
!                                                    ! diffusion coefficient                   (-)
  REAL, POINTER, DIMENSION(:,:)    :: XWFC           ! field capacity volumetric water content
!                                                    ! profile                                 (m3/m3)
  REAL, POINTER, DIMENSION(:,:)    :: XWWILT         ! wilting point volumetric water content 
!                                                    ! profile                                 (m3/m3)
  REAL, POINTER, DIMENSION(:,:)    :: XWSAT          ! porosity profile                        (m3/m3) 
  REAL, POINTER, DIMENSION(:,:)    :: XBCOEF         ! soil water CH78 b-parameter             (-)
  REAL, POINTER, DIMENSION(:,:,:)  :: XCONDSAT       ! hydraulic conductivity at saturation    (m/s)
  REAL, POINTER, DIMENSION(:,:)    :: XMPOTSAT       ! matric potential at saturation          (m)
!
REAL, POINTER, DIMENSION(:) :: XF_PARAM
REAL, POINTER, DIMENSION(:) :: XC_DEPTH_RATIO
!-------------------------------------------------------------------------------
!
! - soil: Secondary parameters: thermal 
!
  REAL, POINTER, DIMENSION(:)      :: XCGSAT         ! soil thermal inertia coefficient at 
!                                                      ! saturation                              (K m2/J)
  REAL, POINTER, DIMENSION(:,:)    :: XHCAPSOIL      ! soil heat capacity                      (J/K/m3)
  REAL, POINTER, DIMENSION(:,:)    :: XCONDDRY       ! soil dry thermal conductivity           (W/m/K)
  REAL, POINTER, DIMENSION(:,:)    :: XCONDSLD       ! soil solids thermal conductivity        (W/m/K)
  REAL, POINTER, DIMENSION(:)      :: XTDEEP         ! prescribed deep soil temperature 
!                                                      ! (optional)                              (K)
!-------------------------------------------------------------------------------
!
! Prognostic variables:
!
! - Snow Cover:
!
  TYPE(SURF_SNOW)                       :: TSNOW         ! snow state: 
!                                                      ! scheme type/option                      (-)
!                                                      ! number of layers                        (-)
!                                                      ! snow (& liq. water) content             (kg/m2)
!                                                      ! heat content                            (J/m2)
!                                                      ! temperature                             (K)
!                                                      ! density                                 (kg m-3)
!
!-------------------------------------------------------------------------------
!
! - Soil and vegetation heat and water:
!
  REAL, POINTER, DIMENSION(:,:)     :: XWR           ! liquid water retained on the
!                                                      ! foliage of the vegetation
!                                                      ! canopy                                  (kg/m2)
  REAL, POINTER, DIMENSION(:,:,:)   :: XTG           ! surface and sub-surface soil 
!                                                      ! temperature profile                     (K)
  REAL, POINTER, DIMENSION(:,:,:)   :: XWG           ! soil volumetric water content profile   (m3/m3)
  REAL, POINTER, DIMENSION(:,:,:)   :: XWGI          ! soil liquid water equivalent volumetric 
!                                                      ! ice content profile                     (m3/m3)
  REAL, POINTER, DIMENSION(:,:)     :: XRESA         ! aerodynamic resistance                  (s/m)

  REAL, POINTER, DIMENSION(:,:)     :: XPCPS
  REAL, POINTER, DIMENSION(:,:)     :: XPLVTT
  REAL, POINTER, DIMENSION(:,:)     :: XPLSTT 
!
! - For multi-energy balance:
!
  REAL, POINTER, DIMENSION(:,:)     :: XWRL          ! liquid water retained on litter          (kg/m2)
  REAL, POINTER, DIMENSION(:,:)     :: XWRLI          ! ice retained on litter          (kg/m2)
  REAL, POINTER, DIMENSION(:,:)     :: XWRVN         ! snow retained on the foliage
!                                                    ! of the canopy vegetation                  (kg/m2)
  REAL, POINTER, DIMENSION(:,:)     :: XTV           ! canopy vegetation temperature             (K)
  REAL, POINTER, DIMENSION(:,:)     :: XTL           ! litter temperature             (K)
  REAL, POINTER, DIMENSION(:,:)     :: XTC           ! canopy air temperature                    (K)
  REAL, POINTER, DIMENSION(:,:)     :: XQC           ! canopy air specific humidity              (kg/kg)
!
!-------------------------------------------------------------------------------
!
! - Vegetation: Ags Prognostic (YPHOTO = ('LAI', 'LST', or 'NIT') or prescribed (YPHOTO='NON', 'AGS' or 'LST')
!
  REAL, POINTER, DIMENSION(:,:)     :: XLAI          ! Leaf Area Index                         (m2/m2)
!
!-------------------------------------------------------------------------------
!
! - Vegetation: Ags Prognostic (YPHOTO = 'AGS', 'LAI', 'AST', 'LST', 'NIT', 'NCB')
!
  REAL, POINTER, DIMENSION(:,:)     :: XAN           ! net CO2 assimilation                    (mg/m2/s)
  REAL, POINTER, DIMENSION(:,:)     :: XANDAY        ! daily net CO2 assimilation              (mg/m2)
  REAL, POINTER, DIMENSION(:,:)     :: XANFM         ! maximum leaf assimilation               (mg/m2/s)
  REAL, POINTER, DIMENSION(:,:)     :: XLE           ! evapotranspiration                      (W/m2)
  REAL, POINTER, DIMENSION(:,:)     :: XFAPARC       ! Fapar of vegetation (cumul)
  REAL, POINTER, DIMENSION(:,:)     :: XFAPIRC       ! Fapir of vegetation (cumul)
  REAL, POINTER, DIMENSION(:,:)     :: XLAI_EFFC     ! Effective LAI (cumul)
  REAL, POINTER, DIMENSION(:,:)     :: XMUS          ! cos zenithal angle (cumul)    
!
!-------------------------------------------------------------------------------
!
! - Vegetation: Ags Prognostic (YPHOTO = 'NIT', 'NCB')
!
  REAL, POINTER, DIMENSION(:,:,:)   :: XRESP_BIOMASS    ! daily cumulated respiration of 
!                                                       ! biomass                              (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:,:)   :: XBIOMASS         ! biomass of previous day              (kg/m2) 
  REAL, POINTER, DIMENSION(:,:,:)   :: XINCREASE        ! biomass increase                     (kg/m2/day)
!
!
!-------------------------------------------------------------------------------
!
! - Soil carbon (ISBA-CC, YRESPSL = 'CNT')
!
  REAL, POINTER, DIMENSION(:,:,:,:) :: XLITTER          ! litter pools                         (gC/m2)
  REAL, POINTER, DIMENSION(:,:,:)   :: XSOILCARB        ! soil carbon pools                    (gC/m2) 
  REAL, POINTER, DIMENSION(:,:,:)   :: XLIGNIN_STRUC    ! ratio Lignin/Carbon in structural
!                                                         litter                               (gC/m2)
!
  REAL, POINTER, DIMENSION(:,:,:)   :: XTURNOVER        ! turnover rates from biomass to litter (gC/m2/s)
!
!-------------------------------------------------------------------------------
!
  TYPE (DATE_TIME)                      :: TTIME            ! current date and time
!
  REAL                                  :: XTSTEP           ! ISBA time step
!
  REAL                                  :: XOUT_TSTEP       ! ISBA output writing time step
!-------------------------------------------------------------------------------
!
! - Irrigation, seeding and reaping
!
  TYPE (DATE_TIME), POINTER, DIMENSION(:,:)  :: TSEED          ! date of seeding
  TYPE (DATE_TIME), POINTER, DIMENSION(:,:)  :: TREAP          ! date of reaping
  REAL, POINTER, DIMENSION(:,:)         :: XWATSUP        ! water supply during irrigation process (mm)
  REAL, POINTER, DIMENSION(:,:)         :: XIRRIG         ! flag for irrigation (irrigation if >0.)
!-------------------------------------------------------------------------------
!
! - Adjustable physical parameters
!
  REAL                                  :: XCGMAX           ! maximum soil heat capacity
!
  REAL                                  :: XCDRAG           ! drag coefficient in canopy
!-------------------------------------------------------------------------------
!
! - Sub-grid hydrology and vertical hydrology
!                                                     
  CHARACTER(LEN=4)               :: CRUNOFF! surface runoff formulation
!                                          ! 'WSAT'
!                                          ! 'DT92'
!                                          ! 'SGH ' Topmodel
!                                                     
  CHARACTER(LEN=3)               :: CKSAT  ! ksat
!                                          ! 'DEF' = default value 
!                                          ! 'SGH' = profil exponentiel
!                                           
  LOGICAL                        :: LSOC   ! soil organic carbon effect
!                                          ! False = default value 
!                                          ! True  = soil SOC profil
!
  CHARACTER(LEN=3)               :: CRAIN  ! Rainfall spatial distribution
                                           ! 'DEF' = No rainfall spatial distribution
                                           ! 'SGH' = Rainfall exponential spatial distribution
                                           ! 
!
  CHARACTER(LEN=3)               :: CHORT  ! Horton runoff
                                           ! 'DEF' = no Horton runoff
                                           ! 'SGH' = Horton runoff
!
  INTEGER                          :: NLAYER_HORT
  INTEGER                          :: NLAYER_DUN
!
  REAL, POINTER, DIMENSION(:)      :: XRUNOFFB       ! sub-grid dt92 surface runoff slope parameter (-)
  REAL, POINTER, DIMENSION(:,:)    :: XRUNOFFD       ! depth over which sub-grid runoff is
!                                                    ! computed: in Force-Restore this is the
!                                                    ! total soil column ('2-L'), or root zone
!                                                    ! ('3-L'). For the 'DIF' option, it can
!                                                    ! be any depth within soil column         (m)
!
  REAL, POINTER, DIMENSION(:,:,:)  :: XSOILWGHT      ! ISBA-DIF: weights for vertical
!                                                    ! integration of soil water and properties
!
  REAL, POINTER, DIMENSION(:,:)  :: XTAB_FSAT !Satured fraction array
  REAL, POINTER, DIMENSION(:,:)  :: XTAB_WTOP !Active TOPMODEL-layer array
  REAL, POINTER, DIMENSION(:,:)  :: XTAB_QTOP !Subsurface flow TOPMODEL array
!                                        
  REAL, POINTER, DIMENSION(:,:)  :: XD_ICE    !depth of the soil column for the calculation
!                                              of the frozen soil fraction (m)
  REAL, POINTER, DIMENSION(:,:)  :: XKSAT_ICE !hydraulic conductivity at saturation
!                                              over frozen area (m s-1)
!
  REAL, POINTER, DIMENSION(:)    :: XTI_MIN,XTI_MAX,XTI_MEAN,XTI_STD,XTI_SKEW
!                                   Topmodel statistics                                    
!                                            
  REAL, POINTER, DIMENSION(:)    :: XMUF  ! fraction of the grid cell reached by the rainfall
  REAL, POINTER, DIMENSION(:)    :: XFSAT ! Topmodel or dt92 saturated fraction
  REAL, POINTER, DIMENSION(:,:,:):: XTOPQS! Topmodel subsurface flow by layer (m/s)
!
  REAL, POINTER, DIMENSION(:,:)  :: XFRACSOC ! Fraction of organic carbon in each soil layer
!
  REAL, POINTER, DIMENSION(:,:)  :: XKANISO  ! Anisotropy coeficient for hydraulic conductivity 
!                                            ! for lateral drainage ('DIF' option)
  REAL, POINTER, DIMENSION(:,:)  :: XWD0     ! water content equivalent to TOPMODEL maximum deficit
!
!-------------------------------------------------------------------------------
!
! - Snow and flood fractions and total albedo at time t:
!
  REAL, POINTER, DIMENSION(:,:) :: XPSNG         ! Snow fraction over ground
  REAL, POINTER, DIMENSION(:,:) :: XPSNV         ! Snow fraction over vegetation
  REAL, POINTER, DIMENSION(:,:) :: XPSNV_A       ! Snow fraction over vegetation
  REAL, POINTER, DIMENSION(:,:) :: XPSN          ! Total Snow fraction
! 
  REAL, POINTER, DIMENSION(:,:,:) :: XDIR_ALB_WITH_SNOW ! total direct albedo by bands
  REAL, POINTER, DIMENSION(:,:,:) :: XSCA_ALB_WITH_SNOW ! total diffuse albedo by bands
!
  REAL, POINTER, DIMENSION(:,:) :: XFFG          ! Flood fraction over ground
  REAL, POINTER, DIMENSION(:,:) :: XFFV          ! Flood fraction over vegetation
  REAL, POINTER, DIMENSION(:,:) :: XFFROZEN      ! Fraction of frozen floodplains
  REAL, POINTER, DIMENSION(:,:) :: XFF           ! Total Flood fraction  
  REAL, POINTER, DIMENSION(:,:) :: XALBF         ! Flood albedo
  REAL, POINTER, DIMENSION(:,:) :: XEMISF        ! Flood emissivity
!
  REAL, POINTER, DIMENSION(:,:) :: XICE_STO      ! Glacier ice storage reservoir
!
!-------------------------------------------------------------------------------
!
! - Flood scheme
!
  LOGICAL                      :: LFLOOD       ! Activation of the flooding scheme
  REAL, POINTER, DIMENSION(:)  :: XFFLOOD      ! Grid-cell flood fraction
  REAL, POINTER, DIMENSION(:)  :: XPIFLOOD     ! flood potential infiltration (kg/m2/s)
!
!-------------------------------------------------------------------------------
!
! - Water table depth coupling
!  
  LOGICAL                      :: LWTD          ! Activation of Water table depth coupling
  REAL, POINTER, DIMENSION(:)  :: XFWTD         ! grid-cell fraction of water table rise
  REAL, POINTER, DIMENSION(:)  :: XWTD          ! water table depth (negative below soil surface) (m)
!
!-------------------------------------------------------------------------------
!
! - Coupling with river routing model
!  
  LOGICAL                      :: LCPL_RRM     ! Activation of the coupling
  REAL, POINTER, DIMENSION(:)  :: XCPL_DRAIN   ! Surface runoff
  REAL, POINTER, DIMENSION(:)  :: XCPL_RUNOFF  ! Deep drainage or gourdwater recharge
  REAL, POINTER, DIMENSION(:)  :: XCPL_ICEFLUX ! Calving flux
  REAL, POINTER, DIMENSION(:)  :: XCPL_RECHARGE! Groundwater recharge
  REAL, POINTER, DIMENSION(:)  :: XCPL_EFLOOD  ! floodplains evaporation
  REAL, POINTER, DIMENSION(:)  :: XCPL_PFLOOD  ! floodplains precipitation interception
  REAL, POINTER, DIMENSION(:)  :: XCPL_IFLOOD  ! floodplains infiltration
!
!-------------------------------------------------------------------------------
!
!  - Random perturbations
!
  REAL, POINTER, DIMENSION(:)     :: XPERTVEG
  REAL, POINTER, DIMENSION(:)     :: XPERTLAI
  REAL, POINTER, DIMENSION(:)     :: XPERTCV
  REAL, POINTER, DIMENSION(:)     :: XPERTALB
  REAL, POINTER, DIMENSION(:)     :: XPERTZ0
!
!-------------------------------------------------------------------------------
!
!  - Assimilation: ENKF
!
  REAL, POINTER, DIMENSION(:,:,:)     :: XRED_NOISE
  REAL, POINTER, DIMENSION(:,:)     :: XINCR
!
!-------------------------------------------------------------------------------
!
END TYPE ISBA_t

 CONTAINS

SUBROUTINE ISBA_INIT(YISBA)
TYPE(ISBA_t), INTENT(INOUT) :: YISBA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_ISBA_N:ISBA_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YISBA%NSIZE_NATURE_P)
  NULLIFY(YISBA%NR_NATURE_P)
  NULLIFY(YISBA%XPATCH)
  NULLIFY(YISBA%XPATCH_OLD)  
  NULLIFY(YISBA%XVEGTYPE)
  NULLIFY(YISBA%XVEGTYPE_PATCH)
  NULLIFY(YISBA%XSODELX)
  NULLIFY(YISBA%XSOILGRID)
  NULLIFY(YISBA%XZS)
  NULLIFY(YISBA%XCOVER)
  NULLIFY(YISBA%LCOVER)
  NULLIFY(YISBA%LMEB_PATCH)
  NULLIFY(YISBA%XALBNIR_DRY)
  NULLIFY(YISBA%XALBVIS_DRY)
  NULLIFY(YISBA%XALBUV_DRY)
  NULLIFY(YISBA%XALBNIR_WET)
  NULLIFY(YISBA%XALBVIS_WET)
  NULLIFY(YISBA%XALBUV_WET)
  NULLIFY(YISBA%XALBNIR_SOIL)
  NULLIFY(YISBA%XALBVIS_SOIL)
  NULLIFY(YISBA%XALBUV_SOIL)
  NULLIFY(YISBA%XEMIS_NAT)
  NULLIFY(YISBA%XTSRAD_NAT)
  NULLIFY(YISBA%XAOSIP)
  NULLIFY(YISBA%XAOSIM)
  NULLIFY(YISBA%XAOSJP)
  NULLIFY(YISBA%XAOSJM)
  NULLIFY(YISBA%XHO2IP)
  NULLIFY(YISBA%XHO2IM)
  NULLIFY(YISBA%XHO2JP)
  NULLIFY(YISBA%XHO2JM)
  NULLIFY(YISBA%XZ0EFFIP)
  NULLIFY(YISBA%XZ0EFFIM)
  NULLIFY(YISBA%XZ0EFFJP)
  NULLIFY(YISBA%XZ0EFFJM)
  NULLIFY(YISBA%XZ0EFFJPDIR)
  NULLIFY(YISBA%XZ0REL)
  NULLIFY(YISBA%XSSO_SLOPE)
  NULLIFY(YISBA%XSSO_STDEV)
  NULLIFY(YISBA%XZ0_O_Z0H)
  NULLIFY(YISBA%XALBNIR)
  NULLIFY(YISBA%XALBVIS)
  NULLIFY(YISBA%XALBUV)
  NULLIFY(YISBA%XEMIS)
  NULLIFY(YISBA%XZ0)
  NULLIFY(YISBA%XALBNIR_VEG)
  NULLIFY(YISBA%XALBVIS_VEG)
  NULLIFY(YISBA%XALBUV_VEG)
  NULLIFY(YISBA%XVEG)
  NULLIFY(YISBA%XWRMAX_CF)
  NULLIFY(YISBA%XRSMIN)
  NULLIFY(YISBA%XGAMMA)
  NULLIFY(YISBA%XCV)
  NULLIFY(YISBA%XRGL)
  NULLIFY(YISBA%XROOTFRAC)
  NULLIFY(YISBA%XABC)
  NULLIFY(YISBA%XPOI)  
  NULLIFY(YISBA%XBSLAI)
  NULLIFY(YISBA%XLAIMIN)
  NULLIFY(YISBA%XSEFOLD)
  NULLIFY(YISBA%XTAU_WOOD)
  NULLIFY(YISBA%XH_TREE)
  NULLIFY(YISBA%XANF)
  NULLIFY(YISBA%XANMAX)
  NULLIFY(YISBA%XFZERO)
  NULLIFY(YISBA%XEPSO)
  NULLIFY(YISBA%XGAMM)
  NULLIFY(YISBA%XQDGAMM)
  NULLIFY(YISBA%XGMES)
  NULLIFY(YISBA%XRE25)
  NULLIFY(YISBA%XQDGMES)
  NULLIFY(YISBA%XT1GMES)
  NULLIFY(YISBA%XT2GMES)
  NULLIFY(YISBA%XAMAX)
  NULLIFY(YISBA%XQDAMAX)
  NULLIFY(YISBA%XT1AMAX)
  NULLIFY(YISBA%XT2AMAX)
  NULLIFY(YISBA%LSTRESS)
  NULLIFY(YISBA%XF2I)
  NULLIFY(YISBA%XGC)
  NULLIFY(YISBA%XAH)
  NULLIFY(YISBA%XBH)
  NULLIFY(YISBA%XDMAX)
  NULLIFY(YISBA%XCE_NITRO)
  NULLIFY(YISBA%XCF_NITRO)
  NULLIFY(YISBA%XCNA_NITRO)
  NULLIFY(YISBA%XBSLAI_NITRO)
  NULLIFY(YISBA%XSAND)
  NULLIFY(YISBA%XCLAY)
  NULLIFY(YISBA%XRUNOFFB)
  NULLIFY(YISBA%XWDRAIN)
  NULLIFY(YISBA%XTAUICE)
  NULLIFY(YISBA%XGAMMAT)
  NULLIFY(YISBA%XDG_OLD)
  NULLIFY(YISBA%XDG)
  NULLIFY(YISBA%XDZG)
  NULLIFY(YISBA%XDZDIF)
  NULLIFY(YISBA%NWG_LAYER)
  NULLIFY(YISBA%XDROOT)
  NULLIFY(YISBA%XDG2)
  NULLIFY(YISBA%XPH)
  NULLIFY(YISBA%XFERT)
  NULLIFY(YISBA%XRUNOFFD)
  NULLIFY(YISBA%XSOILWGHT)
  NULLIFY(YISBA%XC1SAT)
  NULLIFY(YISBA%XC2REF)
  NULLIFY(YISBA%XC3)
  NULLIFY(YISBA%XC4B)
  NULLIFY(YISBA%XC4REF)
  NULLIFY(YISBA%XACOEF)
  NULLIFY(YISBA%XPCOEF)
  NULLIFY(YISBA%XWFC)
  NULLIFY(YISBA%XWD0)
  NULLIFY(YISBA%XWWILT)
  NULLIFY(YISBA%XWSAT)
  NULLIFY(YISBA%XBCOEF)
  NULLIFY(YISBA%XCONDSAT)
  NULLIFY(YISBA%XMPOTSAT)
  NULLIFY(YISBA%XF_PARAM)
  NULLIFY(YISBA%XC_DEPTH_RATIO)
  NULLIFY(YISBA%XCGSAT)
  NULLIFY(YISBA%XHCAPSOIL)
  NULLIFY(YISBA%XCONDDRY)
  NULLIFY(YISBA%XCONDSLD)
  NULLIFY(YISBA%XTDEEP)
  NULLIFY(YISBA%XWR)
  NULLIFY(YISBA%XTG)
  NULLIFY(YISBA%XWG)
  NULLIFY(YISBA%XWGI)
  NULLIFY(YISBA%XRESA)
  NULLIFY(YISBA%XPCPS)
  NULLIFY(YISBA%XPLVTT)
  NULLIFY(YISBA%XPLSTT)
  NULLIFY(YISBA%XLAI)
  NULLIFY(YISBA%XAN)
  NULLIFY(YISBA%XANDAY)
  NULLIFY(YISBA%XANFM)
  NULLIFY(YISBA%XLE)
  NULLIFY(YISBA%XFAPARC)
  NULLIFY(YISBA%XFAPIRC)
  NULLIFY(YISBA%XLAI_EFFC)  
  NULLIFY(YISBA%XMUS)   
  NULLIFY(YISBA%XRESP_BIOMASS)
  NULLIFY(YISBA%XBIOMASS)
  NULLIFY(YISBA%XINCREASE)
  NULLIFY(YISBA%XLITTER)
  NULLIFY(YISBA%XSOILCARB)
  NULLIFY(YISBA%XLIGNIN_STRUC)
  NULLIFY(YISBA%XTURNOVER)
  NULLIFY(YISBA%XWATSUP)
  NULLIFY(YISBA%XIRRIG)
  NULLIFY(YISBA%XTAB_FSAT)
  NULLIFY(YISBA%XTAB_WTOP)
  NULLIFY(YISBA%XTAB_QTOP)
  NULLIFY(YISBA%XD_ICE)
  NULLIFY(YISBA%XKSAT_ICE)
  NULLIFY(YISBA%XFRACSOC)
  NULLIFY(YISBA%XKANISO)
  NULLIFY(YISBA%XTI_MIN)
  NULLIFY(YISBA%XTI_MAX)
  NULLIFY(YISBA%XTI_MEAN)
  NULLIFY(YISBA%XTI_STD)
  NULLIFY(YISBA%XTI_SKEW)
  NULLIFY(YISBA%XMUF)
  NULLIFY(YISBA%XFSAT)
  NULLIFY(YISBA%XTOPQS)
  NULLIFY(YISBA%XPSNG)
  NULLIFY(YISBA%XPSNV)
  NULLIFY(YISBA%XPSNV_A)
  NULLIFY(YISBA%XPSN)
  NULLIFY(YISBA%XDIR_ALB_WITH_SNOW)
  NULLIFY(YISBA%XSCA_ALB_WITH_SNOW)
  NULLIFY(YISBA%XFFG)
  NULLIFY(YISBA%XFFV)
  NULLIFY(YISBA%XFFROZEN)
  NULLIFY(YISBA%XFF)
  NULLIFY(YISBA%XALBF)
  NULLIFY(YISBA%XEMISF)
  NULLIFY(YISBA%XICE_STO)
  NULLIFY(YISBA%XFFLOOD)
  NULLIFY(YISBA%XPIFLOOD)
  NULLIFY(YISBA%XFWTD)
  NULLIFY(YISBA%XWTD)  
  NULLIFY(YISBA%XCPL_DRAIN)
  NULLIFY(YISBA%XCPL_RUNOFF)
  NULLIFY(YISBA%XCPL_ICEFLUX)
  NULLIFY(YISBA%XCPL_RECHARGE)
  NULLIFY(YISBA%XCPL_EFLOOD)
  NULLIFY(YISBA%XCPL_PFLOOD)
  NULLIFY(YISBA%XCPL_IFLOOD)
  NULLIFY(YISBA%XPERTVEG)
  NULLIFY(YISBA%XPERTLAI)
  NULLIFY(YISBA%XPERTCV)
  NULLIFY(YISBA%XPERTALB)
  NULLIFY(YISBA%XPERTZ0)
  NULLIFY(YISBA%XRED_NOISE)
  NULLIFY(YISBA%XINCR)
  !
  NULLIFY(YISBA%XGNDLITTER)
  NULLIFY(YISBA%XRGLGV)
  NULLIFY(YISBA%XGAMMAGV)
  NULLIFY(YISBA%XRSMINGV)
  NULLIFY(YISBA%XROOTFRACGV)
  NULLIFY(YISBA%XWRMAX_CFGV)
  NULLIFY(YISBA%XLAIGV)
  NULLIFY(YISBA%XZ0LITTER)
  NULLIFY(YISBA%XH_VEG)
  NULLIFY(YISBA%XWRL)
  NULLIFY(YISBA%XWRLI)
  NULLIFY(YISBA%XWRVN)
  NULLIFY(YISBA%XTV)
  NULLIFY(YISBA%XTL)
  NULLIFY(YISBA%XTC)
  NULLIFY(YISBA%XQC)
  !
YISBA%CROUGH=' '
YISBA%CISBA=' '
YISBA%CPEDOTF=' '
YISBA%CPHOTO=' '
YISBA%LTR_ML=.FALSE.
YISBA%XRM_PATCH=0.0
YISBA%CALBEDO=' '
YISBA%CSCOND=' '
YISBA%CC1DRY=' '
YISBA%CSOILFRZ=' '
YISBA%CDIFSFCOND=' '
YISBA%CSNOWRES=' '
YISBA%CRESPSL=' '
YISBA%CCPSURF=' '
YISBA%LTEMP_ARP=.FALSE.
YISBA%LGLACIER=.FALSE.
YISBA%LFORC_MEASURE=.FALSE.
YISBA%LMEB_LITTER=.FALSE.
YISBA%LMEB_GNDRES=.FALSE.
YISBA%LVEGUPD=.FALSE.
YISBA%LNITRO_DILU=.FALSE.
YISBA%LCANOPY=.FALSE.
YISBA%LCANOPY_DRAG=.FALSE.
YISBA%LPERTSURF=.FALSE.
YISBA%LSNOWDRIFT=.TRUE.
YISBA%LSNOWDRIFT_SUBLIM=.FALSE.
YISBA%LSNOW_ABS_ZENITH=.FALSE.
YISBA%CSNOWMETAMO='B92'
YISBA%CSNOWRAD='B92'
YISBA%LECOCLIMAP=.FALSE.
YISBA%LCTI=.FALSE.
YISBA%LSOCP=.FALSE.
YISBA%LPERM=.FALSE.
YISBA%LGW=.FALSE.
YISBA%LSPINUPCARBS=.FALSE.
YISBA%LSPINUPCARBW=.FALSE.
YISBA%LAGRI_TO_GRASS=.FALSE.
YISBA%XSPINMAXS=0.
YISBA%XSPINMAXW=0.
YISBA%XCO2_START=0.
YISBA%XCO2_END=0.
YISBA%NNBYEARSPINS=0
YISBA%NNBYEARSPINW=0
YISBA%NNBYEARSOLD=0
YISBA%NSPINS=1
YISBA%NSPINW=1
YISBA%LNOF=.FALSE.
YISBA%NPATCH=0
YISBA%NGROUND_LAYER=0
YISBA%NTEMPLAYER_ARP=0
YISBA%NNBIOMASS=0
YISBA%NNLITTER=0
YISBA%NNLITTLEVS=0
YISBA%NNSOILCARB=0
YISBA%XTSTEP=0.
YISBA%XOUT_TSTEP=0.
YISBA%XCGMAX=0.
YISBA%XCDRAG=0.
YISBA%CRUNOFF=' '
YISBA%CKSAT=' '
YISBA%LSOC=.FALSE.
YISBA%CRAIN=' '
YISBA%CHORT=' '
YISBA%NLAYER_HORT=0
YISBA%NLAYER_DUN=0
YISBA%LFLOOD=.FALSE.
YISBA%LWTD=.FALSE.
YISBA%LCPL_RRM=.FALSE.
IF (LHOOK) CALL DR_HOOK("MODD_ISBA_N:ISBA_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE ISBA_INIT

END MODULE MODD_ISBA_n
