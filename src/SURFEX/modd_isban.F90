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
!!	A. Boone   *Meteo France*
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
!	                                   ! 'DEF' = Default: Louis (ISBA)
!       	                           ! 'RIL' = Maximum Richardson number limit
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
  LOGICAL                        :: LVEGUPD  ! True = update vegetation parameters every decade
                                             ! False = keep vegetation parameters constant in time                                             
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
  LOGICAL                        :: LNOF  
!-------------------------------------------------------------------------------
!
! Soil and wood carbon spin up 
!
  LOGICAL                        :: LSPINUPCARBS  ! T: do the soil carb spinup, F: no
  LOGICAL                        :: LSPINUPCARBW  ! T: do the wood carb spinup, F: no  
  REAL                           :: XSPINMAXS     ! max number of times CARBON_SOIL subroutine is
                                                  ! called for each timestep in simulation during
                                                  ! acceleration procedure number
  REAL                           :: XSPINMAXW     ! max number of times the wood is accelerated                                                  
  INTEGER                        :: NNBYEARSPINS   ! nbr years needed to reaches soil equilibrium 
  INTEGER                        :: NNBYEARSPINW   ! nbr years needed to reaches wood equilibrium
  INTEGER                        :: NNBYEARSOLD    ! nbr years executed at curent time step
  INTEGER                        :: NSPINS    ! number of times the soil is accelerated
  INTEGER                        :: NSPINW    ! number of times the wood is accelerated

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
! - Sub-grid hydrology
!                                                     
  CHARACTER(LEN=4)               :: CRUNOFF! surface runoff formulation
!                                          ! 'WSAT'
!                                          ! 'DT92'
!                                          ! 'SGH ' Topmodel
!                                                     
  CHARACTER(LEN=3)               :: CTOPREG! Wolock and McCabe (2000) linear regression for Topmodel
                                           ! 'DEF' = Reg
                                           ! 'NON' = no Reg  
!                                           
  CHARACTER(LEN=3)               :: CKSAT  ! ksat
!                                          ! 'DEF' = default value 
!                                          ! 'SGH' = profil exponentiel
!                                           
  CHARACTER(LEN=3)               :: CSOC   ! soil organic carbon effect
!                                          ! 'DEF' = default value 
!                                          ! 'SGH' = soil SOC profil
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
!
  REAL, POINTER, DIMENSION(:,:)  :: XFRACSOC ! Fraction of organic carbon in each soil layer
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
! - ESM, TRIP and Flood scheme coupling
!
  LOGICAL                      :: LTRIP        ! Activation of the TRIP RRM
  LOGICAL                      :: LFLOOD       ! Activation of the flooding scheme
! 
  REAL, POINTER, DIMENSION(:)  :: XFFLOOD      ! Grid-cell flood fraction
  REAL, POINTER, DIMENSION(:)  :: XPIFLOOD     ! flood potential infiltration
  REAL, POINTER, DIMENSION(:)  :: XCPL_EFLOOD
  REAL, POINTER, DIMENSION(:)  :: XCPL_PFLOOD
  REAL, POINTER, DIMENSION(:)  :: XCPL_IFLOOD
  REAL, POINTER, DIMENSION(:)  :: XCPL_DRAIN
  REAL, POINTER, DIMENSION(:)  :: XCPL_RUNOFF  
  REAL, POINTER, DIMENSION(:)  :: XCPL_ICEFLUX  
  REAL, POINTER, DIMENSION(:,:):: XZ0_FLOOD        ! roughness length of Flood water
  REAL                         :: XTSTEP_COUPLING  ! ISBA-TRIP coulpling time step
!
!-------------------------------------------------------------------------------
!

END TYPE ISBA_t

TYPE(ISBA_t), ALLOCATABLE, TARGET, SAVE :: ISBA_MODEL(:)

 CHARACTER(LEN=4), POINTER :: CROUGH=>NULL()
!$OMP THREADPRIVATE(CROUGH)
 CHARACTER(LEN=3), POINTER :: CISBA=>NULL()
!$OMP THREADPRIVATE(CISBA)
 CHARACTER(LEN=4), POINTER :: CPEDOTF=>NULL()
!$OMP THREADPRIVATE(CPEDOTF)
 CHARACTER(LEN=3), POINTER :: CPHOTO=>NULL()
!$OMP THREADPRIVATE(CPHOTO)
LOGICAL,          POINTER :: LTR_ML=>NULL()
!$OMP THREADPRIVATE(LTR_ML)
REAL,             POINTER :: XRM_PATCH=>NULL()
!$OMP THREADPRIVATE(XRM_PATCH)
 CHARACTER(LEN=4), POINTER :: CALBEDO=>NULL()
!$OMP THREADPRIVATE(CALBEDO)
 CHARACTER(LEN=4), POINTER :: CRUNOFF=>NULL()
!$OMP THREADPRIVATE(CRUNOFF)
 CHARACTER(LEN=4), POINTER :: CSCOND=>NULL()
!$OMP THREADPRIVATE(CSCOND)
 CHARACTER(LEN=4), POINTER :: CC1DRY=>NULL()
!$OMP THREADPRIVATE(CC1DRY)
 CHARACTER(LEN=3), POINTER :: CSOILFRZ=>NULL()
!$OMP THREADPRIVATE(CSOILFRZ)
 CHARACTER(LEN=4), POINTER :: CDIFSFCOND=>NULL()
!$OMP THREADPRIVATE(CDIFSFCOND)
 CHARACTER(LEN=3), POINTER :: CSNOWRES=>NULL()
!$OMP THREADPRIVATE(CSNOWRES)
 CHARACTER(LEN=3), POINTER :: CRESPSL=>NULL()
!$OMP THREADPRIVATE(CRESPSL)
 CHARACTER(LEN=3), POINTER :: CCPSURF=>NULL()
!$OMP THREADPRIVATE(CCPSURF)
LOGICAL, POINTER :: LTEMP_ARP=>NULL()
!$OMP THREADPRIVATE(LTEMP_ARP)
LOGICAL, POINTER :: LGLACIER=>NULL()
!$OMP THREADPRIVATE(LGLACIER)
LOGICAL, POINTER :: LVEGUPD=>NULL()
!$OMP THREADPRIVATE(LVEGUPD)
LOGICAL, POINTER :: LCANOPY=>NULL()
!$OMP THREADPRIVATE(LCANOPY)
LOGICAL, POINTER :: LCANOPY_DRAG=>NULL()
!$OMP THREADPRIVATE(LCANOPY_DRAG)
LOGICAL, POINTER :: LECOCLIMAP=>NULL()
!$OMP THREADPRIVATE(LECOCLIMAP)
LOGICAL, POINTER :: LCTI=>NULL()
!$OMP THREADPRIVATE(LCTI)
LOGICAL, POINTER :: LSOCP=>NULL()
!$OMP THREADPRIVATE(LSOCP)
LOGICAL, POINTER :: LPERM=>NULL()
!$OMP THREADPRIVATE(LPERM)
LOGICAL, POINTER :: LSPINUPCARBS=>NULL()
!$OMP THREADPRIVATE(LSPINUPCARBS)
LOGICAL, POINTER :: LSPINUPCARBW=>NULL()
!$OMP THREADPRIVATE(LSPINUPCARBW)
REAL, POINTER :: XSPINMAXS=>NULL()
!$OMP THREADPRIVATE(XSPINMAXS)
REAL, POINTER :: XSPINMAXW=>NULL()
!$OMP THREADPRIVATE(XSPINMAXW)
INTEGER, POINTER :: NNBYEARSPINS=>NULL()
!$OMP THREADPRIVATE(NNBYEARSPINS)
INTEGER, POINTER :: NNBYEARSPINW=>NULL()
!$OMP THREADPRIVATE(NNBYEARSPINW)
INTEGER, POINTER :: NNBYEARSOLD=>NULL()
!$OMP THREADPRIVATE(NNBYEARSOLD)
INTEGER, POINTER :: NSPINS=>NULL()
!$OMP THREADPRIVATE(NSPINS)
INTEGER, POINTER :: NSPINW=>NULL()
!$OMP THREADPRIVATE(NSPINW)
LOGICAL, POINTER :: LNOF=>NULL()
!$OMP THREADPRIVATE(LNOF)
INTEGER, POINTER, DIMENSION(:)   :: NSIZE_NATURE_P=>NULL()
!$OMP THREADPRIVATE(NSIZE_NATURE_P)
INTEGER, POINTER, DIMENSION(:,:) :: NR_NATURE_P=>NULL()
!$OMP THREADPRIVATE(NR_NATURE_P)
REAL, POINTER, DIMENSION(:,:)    :: XPATCH=>NULL()
!$OMP THREADPRIVATE(XPATCH)
REAL, POINTER, DIMENSION(:,:)    :: XPATCH_OLD=>NULL()
!$OMP THREADPRIVATE(XPATCH_OLD)
REAL, POINTER, DIMENSION(:,:)    :: XVEGTYPE=>NULL()
!$OMP THREADPRIVATE(XVEGTYPE)
REAL, POINTER, DIMENSION(:,:,:)  :: XVEGTYPE_PATCH=>NULL()
!$OMP THREADPRIVATE(XVEGTYPE_PATCH)
INTEGER, POINTER :: NPATCH=>NULL()
!$OMP THREADPRIVATE(NPATCH)
INTEGER, POINTER :: NGROUND_LAYER=>NULL()
!$OMP THREADPRIVATE(NGROUND_LAYER)
INTEGER, POINTER :: NTEMPLAYER_ARP=>NULL()
!$OMP THREADPRIVATE(NTEMPLAYER_ARP)
REAL, POINTER, DIMENSION(:)   :: XSODELX=>NULL()
!$OMP THREADPRIVATE(XSODELX)
REAL, POINTER, DIMENSION(:)   :: XSOILGRID=>NULL()
!$OMP THREADPRIVATE(XSOILGRID)
INTEGER, POINTER :: NNBIOMASS=>NULL()
!$OMP THREADPRIVATE(NNBIOMASS)
INTEGER, POINTER :: NNLITTER=>NULL()
!$OMP THREADPRIVATE(NNLITTER)
INTEGER, POINTER :: NNLITTLEVS=>NULL()
!$OMP THREADPRIVATE(NNLITTLEVS)
INTEGER, POINTER :: NNSOILCARB=>NULL()
!$OMP THREADPRIVATE(NNSOILCARB)
REAL, POINTER, DIMENSION(:)   :: XZS=>NULL()
!$OMP THREADPRIVATE(XZS)
REAL, POINTER, DIMENSION(:,:) :: XCOVER=>NULL()
!$OMP THREADPRIVATE(XCOVER)
LOGICAL, POINTER, DIMENSION(:):: LCOVER=>NULL()
!$OMP THREADPRIVATE(LCOVER)
REAL, POINTER, DIMENSION(:)   :: XALBNIR_DRY=>NULL()
!$OMP THREADPRIVATE(XALBNIR_DRY)
REAL, POINTER, DIMENSION(:)   :: XALBVIS_DRY=>NULL()
!$OMP THREADPRIVATE(XALBVIS_DRY)
REAL, POINTER, DIMENSION(:)   :: XALBUV_DRY=>NULL()
!$OMP THREADPRIVATE(XALBUV_DRY)
REAL, POINTER, DIMENSION(:)   :: XALBNIR_WET=>NULL()
!$OMP THREADPRIVATE(XALBNIR_WET)
REAL, POINTER, DIMENSION(:)   :: XALBVIS_WET=>NULL()
!$OMP THREADPRIVATE(XALBVIS_WET)
REAL, POINTER, DIMENSION(:)   :: XALBUV_WET=>NULL()
!$OMP THREADPRIVATE(XALBUV_WET)
REAL, POINTER, DIMENSION(:,:) :: XALBNIR_SOIL=>NULL()
!$OMP THREADPRIVATE(XALBNIR_SOIL)
REAL, POINTER, DIMENSION(:,:) :: XALBVIS_SOIL=>NULL()
!$OMP THREADPRIVATE(XALBVIS_SOIL)
REAL, POINTER, DIMENSION(:,:) :: XALBUV_SOIL=>NULL()
!$OMP THREADPRIVATE(XALBUV_SOIL)
REAL, POINTER, DIMENSION(:)   :: XEMIS_NAT=>NULL()
!$OMP THREADPRIVATE(XEMIS_NAT)
REAL, POINTER, DIMENSION(:)   :: XTSRAD_NAT=>NULL()
!$OMP THREADPRIVATE(XTSRAD_NAT)
REAL, DIMENSION(:), POINTER :: XAOSIP=>NULL(),XAOSIM=>NULL(),XAOSJP=>NULL(),XAOSJM=>NULL()
!$OMP THREADPRIVATE(XAOSIP,XAOSIM,XAOSJP,XAOSJM)
REAL, DIMENSION(:), POINTER :: XHO2IP=>NULL(),XHO2IM=>NULL(),XHO2JP=>NULL(),XHO2JM=>NULL()
!$OMP THREADPRIVATE(XHO2IP,XHO2IM,XHO2JP,XHO2JM)
REAL, DIMENSION(:,:), POINTER :: XZ0EFFIP=>NULL(),XZ0EFFIM=>NULL(),XZ0EFFJP=>NULL(),XZ0EFFJM=>NULL()
!$OMP THREADPRIVATE(XZ0EFFIP,XZ0EFFIM,XZ0EFFJP,XZ0EFFJM)
REAL, DIMENSION(:), POINTER   :: XZ0EFFJPDIR=>NULL()
!$OMP THREADPRIVATE(XZ0EFFJPDIR)
REAL, DIMENSION(:), POINTER   :: XZ0REL=>NULL()
!$OMP THREADPRIVATE(XZ0REL)
REAL, DIMENSION(:), POINTER   :: XSSO_SLOPE=>NULL()
!$OMP THREADPRIVATE(XSSO_SLOPE)
REAL, DIMENSION(:), POINTER   :: XSSO_STDEV=>NULL()
!$OMP THREADPRIVATE(XSSO_STDEV)
REAL, POINTER, DIMENSION(:,:) :: XZ0_O_Z0H=>NULL()
!$OMP THREADPRIVATE(XZ0_O_Z0H)
REAL, POINTER, DIMENSION(:,:) :: XALBNIR=>NULL()
!$OMP THREADPRIVATE(XALBNIR)
REAL, POINTER, DIMENSION(:,:) :: XALBVIS=>NULL()
!$OMP THREADPRIVATE(XALBVIS)
REAL, POINTER, DIMENSION(:,:) :: XALBUV=>NULL()
!$OMP THREADPRIVATE(XALBUV)
REAL, POINTER, DIMENSION(:,:) :: XEMIS=>NULL()
!$OMP THREADPRIVATE(XEMIS)
REAL, POINTER, DIMENSION(:,:) :: XZ0=>NULL()
!$OMP THREADPRIVATE(XZ0)
REAL, POINTER, DIMENSION(:,:) :: XALBNIR_VEG=>NULL()
!$OMP THREADPRIVATE(XALBNIR_VEG)
REAL, POINTER, DIMENSION(:,:) :: XALBVIS_VEG=>NULL()
!$OMP THREADPRIVATE(XALBVIS_VEG)
REAL, POINTER, DIMENSION(:,:) :: XALBUV_VEG=>NULL()
!$OMP THREADPRIVATE(XALBUV_VEG)
REAL, POINTER, DIMENSION(:,:) :: XVEG=>NULL()
!$OMP THREADPRIVATE(XVEG)
REAL, POINTER, DIMENSION(:,:) :: XWRMAX_CF=>NULL()
!$OMP THREADPRIVATE(XWRMAX_CF)
REAL, POINTER, DIMENSION(:,:) :: XRSMIN=>NULL()
!$OMP THREADPRIVATE(XRSMIN)
REAL, POINTER, DIMENSION(:,:) :: XGAMMA=>NULL()
!$OMP THREADPRIVATE(XGAMMA)
REAL, POINTER, DIMENSION(:,:) :: XCV=>NULL()
!$OMP THREADPRIVATE(XCV)
REAL, POINTER, DIMENSION(:,:) :: XRGL=>NULL()
!$OMP THREADPRIVATE(XRGL)
REAL, POINTER, DIMENSION(:,:,:) :: XROOTFRAC=>NULL()
!$OMP THREADPRIVATE(XROOTFRAC)
REAL, POINTER, DIMENSION(:) :: XABC=>NULL()
!$OMP THREADPRIVATE(XABC)
REAL, POINTER, DIMENSION(:) :: XPOI=>NULL()
!$OMP THREADPRIVATE(XPOI)
REAL, POINTER, DIMENSION(:,:)    :: XBSLAI=>NULL()
!$OMP THREADPRIVATE(XBSLAI)
REAL, POINTER, DIMENSION(:,:)    :: XLAIMIN=>NULL()
!$OMP THREADPRIVATE(XLAIMIN)
REAL, POINTER, DIMENSION(:,:)    :: XSEFOLD=>NULL()
!$OMP THREADPRIVATE(XSEFOLD)
REAL, POINTER, DIMENSION(:,:)    :: XTAU_WOOD=>NULL()
!$OMP THREADPRIVATE(XTAU_WOOD)
REAL, POINTER, DIMENSION(:,:)    :: XH_TREE=>NULL()
!$OMP THREADPRIVATE(XH_TREE)
REAL, POINTER, DIMENSION(:,:)    :: XANF=>NULL()
!$OMP THREADPRIVATE(XANF)
REAL, POINTER, DIMENSION(:,:)    :: XANMAX=>NULL()
!$OMP THREADPRIVATE(XANMAX)
REAL, POINTER, DIMENSION(:,:)    :: XFZERO=>NULL()
!$OMP THREADPRIVATE(XFZERO)
REAL, POINTER, DIMENSION(:,:)    :: XEPSO=>NULL()
!$OMP THREADPRIVATE(XEPSO)
REAL, POINTER, DIMENSION(:,:)    :: XGAMM=>NULL()
!$OMP THREADPRIVATE(XGAMM)
REAL, POINTER, DIMENSION(:,:)    :: XQDGAMM=>NULL()
!$OMP THREADPRIVATE(XQDGAMM)
REAL, POINTER, DIMENSION(:,:)    :: XGMES=>NULL()
!$OMP THREADPRIVATE(XGMES)
REAL, POINTER, DIMENSION(:,:)    :: XRE25=>NULL()
!$OMP THREADPRIVATE(XRE25)
REAL, POINTER, DIMENSION(:,:)    :: XQDGMES=>NULL()
!$OMP THREADPRIVATE(XQDGMES)
REAL, POINTER, DIMENSION(:,:)    :: XT1GMES=>NULL()
!$OMP THREADPRIVATE(XT1GMES)
REAL, POINTER, DIMENSION(:,:)    :: XT2GMES=>NULL()
!$OMP THREADPRIVATE(XT2GMES)
REAL, POINTER, DIMENSION(:,:)    :: XAMAX=>NULL()
!$OMP THREADPRIVATE(XAMAX)
REAL, POINTER, DIMENSION(:,:)    :: XQDAMAX=>NULL()
!$OMP THREADPRIVATE(XQDAMAX)
REAL, POINTER, DIMENSION(:,:)    :: XT1AMAX=>NULL()
!$OMP THREADPRIVATE(XT1AMAX)
REAL, POINTER, DIMENSION(:,:)    :: XT2AMAX=>NULL()
!$OMP THREADPRIVATE(XT2AMAX)
LOGICAL, POINTER, DIMENSION(:,:) :: LSTRESS=>NULL()
!$OMP THREADPRIVATE(LSTRESS)
REAL, POINTER, DIMENSION(:,:)    :: XF2I=>NULL()
!$OMP THREADPRIVATE(XF2I)
REAL, POINTER, DIMENSION(:,:)    :: XGC=>NULL()
!$OMP THREADPRIVATE(XGC)
REAL, POINTER, DIMENSION(:,:)    :: XAH=>NULL()
!$OMP THREADPRIVATE(XAH)
REAL, POINTER, DIMENSION(:,:)    :: XBH=>NULL()
!$OMP THREADPRIVATE(XBH)
REAL, POINTER, DIMENSION(:,:)    :: XDMAX=>NULL()
!$OMP THREADPRIVATE(XDMAX)
REAL, POINTER, DIMENSION(:,:)    :: XCE_NITRO=>NULL()
!$OMP THREADPRIVATE(XCE_NITRO)
REAL, POINTER, DIMENSION(:,:)    :: XCF_NITRO=>NULL()
!$OMP THREADPRIVATE(XCF_NITRO)
REAL, POINTER, DIMENSION(:,:)    :: XCNA_NITRO=>NULL()
!$OMP THREADPRIVATE(XCNA_NITRO)
REAL, POINTER, DIMENSION(:,:)    :: XBSLAI_NITRO=>NULL()
!$OMP THREADPRIVATE(XBSLAI_NITRO)
REAL, POINTER, DIMENSION(:,:)    :: XSAND=>NULL()
!$OMP THREADPRIVATE(XSAND)
REAL, POINTER, DIMENSION(:,:)    :: XCLAY=>NULL()
!$OMP THREADPRIVATE(XCLAY)
REAL, POINTER, DIMENSION(:,:)    :: XSOC=>NULL()
!$OMP THREADPRIVATE(XSOC)
REAL, POINTER, DIMENSION(:)    :: XPERM=>NULL()
!$OMP THREADPRIVATE(XPERM)
REAL, POINTER, DIMENSION(:)      :: XRUNOFFB=>NULL()
!$OMP THREADPRIVATE(XRUNOFFB)
REAL, POINTER, DIMENSION(:)      :: XWDRAIN=>NULL()
!$OMP THREADPRIVATE(XWDRAIN)
REAL, POINTER, DIMENSION(:)      :: XTAUICE=>NULL()
!$OMP THREADPRIVATE(XTAUICE)
REAL, POINTER, DIMENSION(:)      :: XGAMMAT=>NULL()
!$OMP THREADPRIVATE(XGAMMAT)
REAL, POINTER, DIMENSION(:,:,:)  :: XDG_OLD=>NULL()
!$OMP THREADPRIVATE(XDG_OLD)
REAL, POINTER, DIMENSION(:,:,:)  :: XDG=>NULL()
!$OMP THREADPRIVATE(XDG)
REAL, POINTER, DIMENSION(:,:,:)  :: XDZG=>NULL()
!$OMP THREADPRIVATE(XDZG)
REAL, POINTER, DIMENSION(:,:,:)  :: XDZDIF=>NULL()
!$OMP THREADPRIVATE(XDZDIF)
INTEGER, POINTER, DIMENSION(:,:) :: NWG_LAYER=>NULL()
!$OMP THREADPRIVATE(NWG_LAYER)
REAL, POINTER, DIMENSION(:,:)    :: XDROOT=>NULL()
!$OMP THREADPRIVATE(XDROOT)
REAL, POINTER, DIMENSION(:,:)    :: XDG2=>NULL()
!$OMP THREADPRIVATE(XDG2)
REAL, POINTER, DIMENSION(:)      :: XPH=>NULL()
!$OMP THREADPRIVATE(XPH)
REAL, POINTER, DIMENSION(:)      :: XFERT=>NULL()
!$OMP THREADPRIVATE(XFERT)
REAL, POINTER, DIMENSION(:,:)    :: XRUNOFFD=>NULL()
!$OMP THREADPRIVATE(XRUNOFFD)
REAL, POINTER, DIMENSION(:,:,:)  :: XSOILWGHT=>NULL()
!$OMP THREADPRIVATE(XSOILWGHT)
REAL, POINTER, DIMENSION(:,:)    :: XC1SAT=>NULL()
!$OMP THREADPRIVATE(XC1SAT)
REAL, POINTER, DIMENSION(:,:)    :: XC2REF=>NULL()
!$OMP THREADPRIVATE(XC2REF)
REAL, POINTER, DIMENSION(:,:,:)  :: XC3=>NULL()
!$OMP THREADPRIVATE(XC3)
REAL, POINTER, DIMENSION(:)      :: XC4B=>NULL()
!$OMP THREADPRIVATE(XC4B)
REAL, POINTER, DIMENSION(:,:)    :: XC4REF=>NULL()
!$OMP THREADPRIVATE(XC4REF)
REAL, POINTER, DIMENSION(:)      :: XACOEF=>NULL()
!$OMP THREADPRIVATE(XACOEF)
REAL, POINTER, DIMENSION(:)      :: XPCOEF=>NULL()
!$OMP THREADPRIVATE(XPCOEF)
REAL, POINTER, DIMENSION(:,:)    :: XWFC=>NULL()
!$OMP THREADPRIVATE(XWFC)
REAL, POINTER, DIMENSION(:,:)    :: XWWILT=>NULL()
!$OMP THREADPRIVATE(XWWILT)
REAL, POINTER, DIMENSION(:,:)    :: XWSAT=>NULL()
!$OMP THREADPRIVATE(XWSAT)
REAL, POINTER, DIMENSION(:,:)    :: XBCOEF=>NULL()
!$OMP THREADPRIVATE(XBCOEF)
REAL, POINTER, DIMENSION(:,:,:)  :: XCONDSAT=>NULL()
!$OMP THREADPRIVATE(XCONDSAT)
REAL, POINTER, DIMENSION(:,:)    :: XMPOTSAT=>NULL()
!$OMP THREADPRIVATE(XMPOTSAT)
REAL, POINTER, DIMENSION(:) :: XF_PARAM=>NULL()
!$OMP THREADPRIVATE(XF_PARAM)
REAL, POINTER, DIMENSION(:) :: XC_DEPTH_RATIO=>NULL()
!$OMP THREADPRIVATE(XC_DEPTH_RATIO)
REAL, POINTER, DIMENSION(:)      :: XCGSAT=>NULL()
!$OMP THREADPRIVATE(XCGSAT)
REAL, POINTER, DIMENSION(:,:)    :: XHCAPSOIL=>NULL()
!$OMP THREADPRIVATE(XHCAPSOIL)
REAL, POINTER, DIMENSION(:,:)    :: XCONDDRY=>NULL()
!$OMP THREADPRIVATE(XCONDDRY)
REAL, POINTER, DIMENSION(:,:)    :: XCONDSLD=>NULL()
!$OMP THREADPRIVATE(XCONDSLD)
REAL, POINTER, DIMENSION(:)      :: XTDEEP=>NULL()
!$OMP THREADPRIVATE(XTDEEP)
TYPE(SURF_SNOW), POINTER :: TSNOW=>NULL()
!$OMP THREADPRIVATE(TSNOW)
REAL, POINTER, DIMENSION(:,:)     :: XWR=>NULL()
!$OMP THREADPRIVATE(XWR)
REAL, POINTER, DIMENSION(:,:,:)   :: XTG=>NULL()
!$OMP THREADPRIVATE(XTG)
REAL, POINTER, DIMENSION(:,:,:)   :: XWG=>NULL()
!$OMP THREADPRIVATE(XWG)
REAL, POINTER, DIMENSION(:,:,:)   :: XWGI=>NULL()
!$OMP THREADPRIVATE(XWGI)
REAL, POINTER, DIMENSION(:,:)     :: XRESA=>NULL()
!$OMP THREADPRIVATE(XRESA)
REAL, POINTER, DIMENSION(:,:)     :: XPCPS=>NULL()
!$OMP THREADPRIVATE(XPCPS)
REAL, POINTER, DIMENSION(:,:)     :: XPLVTT=>NULL()
!$OMP THREADPRIVATE(XPLVTT)
REAL, POINTER, DIMENSION(:,:)     :: XPLSTT=>NULL()
!$OMP THREADPRIVATE(XPLSTT)
REAL, POINTER, DIMENSION(:,:)     :: XLAI=>NULL()
!$OMP THREADPRIVATE(XLAI)
REAL, POINTER, DIMENSION(:,:)     :: XAN=>NULL()
!$OMP THREADPRIVATE(XAN)
REAL, POINTER, DIMENSION(:,:)     :: XANDAY=>NULL()
!$OMP THREADPRIVATE(XANDAY)
REAL, POINTER, DIMENSION(:,:)     :: XANFM=>NULL()
!$OMP THREADPRIVATE(XANFM)
REAL, POINTER, DIMENSION(:,:)     :: XLE=>NULL()
!$OMP THREADPRIVATE(XLE)
REAL, POINTER, DIMENSION(:,:)     :: XFAPARC=>NULL()
!$OMP THREADPRIVATE(XFAPARC)
REAL, POINTER, DIMENSION(:,:)     :: XFAPIRC=>NULL()
!$OMP THREADPRIVATE(XFAPIRC)
REAL, POINTER, DIMENSION(:,:)     :: XLAI_EFFC=>NULL()
!$OMP THREADPRIVATE(XLAI_EFFC)
REAL, POINTER, DIMENSION(:,:)     :: XMUS=>NULL()
!$OMP THREADPRIVATE(XMUS)
REAL, POINTER, DIMENSION(:,:,:)   :: XRESP_BIOMASS=>NULL()
!$OMP THREADPRIVATE(XRESP_BIOMASS)
REAL, POINTER, DIMENSION(:,:,:)   :: XBIOMASS=>NULL()
!$OMP THREADPRIVATE(XBIOMASS)
REAL, POINTER, DIMENSION(:,:,:)   :: XINCREASE=>NULL()
!$OMP THREADPRIVATE(XINCREASE)
REAL, POINTER, DIMENSION(:,:,:,:) :: XLITTER=>NULL()
!$OMP THREADPRIVATE(XLITTER)
REAL, POINTER, DIMENSION(:,:,:)   :: XSOILCARB=>NULL()
!$OMP THREADPRIVATE(XSOILCARB)
REAL, POINTER, DIMENSION(:,:,:)   :: XLIGNIN_STRUC=>NULL()
!$OMP THREADPRIVATE(XLIGNIN_STRUC)
REAL, POINTER, DIMENSION(:,:,:)   :: XTURNOVER=>NULL()
!$OMP THREADPRIVATE(XTURNOVER)
TYPE (DATE_TIME), POINTER :: TTIME=>NULL()
!$OMP THREADPRIVATE(TTIME)
REAL, POINTER :: XTSTEP=>NULL()
!$OMP THREADPRIVATE(XTSTEP)
REAL, POINTER :: XOUT_TSTEP=>NULL()
!$OMP THREADPRIVATE(XOUT_TSTEP)
TYPE (DATE_TIME), POINTER, DIMENSION(:,:) :: TSEED=>NULL()
!$OMP THREADPRIVATE(TSEED)
TYPE (DATE_TIME), POINTER, DIMENSION(:,:) :: TREAP=>NULL()
!$OMP THREADPRIVATE(TREAP)
REAL, POINTER, DIMENSION(:,:)        :: XWATSUP=>NULL()
!$OMP THREADPRIVATE(XWATSUP)
REAL, POINTER, DIMENSION(:,:)        :: XIRRIG=>NULL()
!$OMP THREADPRIVATE(XIRRIG)
REAL, POINTER :: XCGMAX=>NULL()
!$OMP THREADPRIVATE(XCGMAX)
REAL, POINTER :: XCDRAG=>NULL()
!$OMP THREADPRIVATE(XCDRAG)
!
REAL, POINTER, DIMENSION(:,:) :: XPSNG=>NULL()
!$OMP THREADPRIVATE(XPSNG)
REAL, POINTER, DIMENSION(:,:) :: XPSNV=>NULL()
!$OMP THREADPRIVATE(XPSNV)
REAL, POINTER, DIMENSION(:,:) :: XPSNV_A=>NULL()
!$OMP THREADPRIVATE(XPSNV_A)
REAL, POINTER, DIMENSION(:,:) :: XPSN=>NULL()
!$OMP THREADPRIVATE(XPSN)
REAL, POINTER, DIMENSION(:,:) :: XFFG=>NULL()
!$OMP THREADPRIVATE(XFFG)
REAL, POINTER, DIMENSION(:,:) :: XFFV=>NULL()
!$OMP THREADPRIVATE(XFFV)
REAL, POINTER, DIMENSION(:,:) :: XFFROZEN=>NULL()
!$OMP THREADPRIVATE(XFFROZEN)
REAL, POINTER, DIMENSION(:,:) :: XFF=>NULL()
!$OMP THREADPRIVATE(XFF)
REAL, POINTER, DIMENSION(:,:) :: XALBF=>NULL()
!$OMP THREADPRIVATE(XALBF)
REAL, POINTER, DIMENSION(:,:) :: XEMISF=>NULL()
!$OMP THREADPRIVATE(XEMISF)
REAL, POINTER, DIMENSION(:,:,:) :: XDIR_ALB_WITH_SNOW=>NULL()
!$OMP THREADPRIVATE(XDIR_ALB_WITH_SNOW)
REAL, POINTER, DIMENSION(:,:,:) :: XSCA_ALB_WITH_SNOW=>NULL()
!$OMP THREADPRIVATE(XSCA_ALB_WITH_SNOW)
REAL, POINTER, DIMENSION(:,:) :: XICE_STO=>NULL()
!$OMP THREADPRIVATE(XICE_STO)
!
!SGH scheme
!
 CHARACTER(LEN=3), POINTER      :: CTOPREG=>NULL()
!$OMP THREADPRIVATE(CTOPREG)
 CHARACTER(LEN=3), POINTER      :: CKSAT=>NULL()
!$OMP THREADPRIVATE(CKSAT)
 CHARACTER(LEN=3), POINTER      :: CSOC=>NULL()
!$OMP THREADPRIVATE(CSOC)
 CHARACTER(LEN=3), POINTER      :: CRAIN=>NULL()
!$OMP THREADPRIVATE(CRAIN)
 CHARACTER(LEN=3), POINTER      :: CHORT=>NULL()
!$OMP THREADPRIVATE(CHORT)
!
REAL, POINTER, DIMENSION(:)    :: XTI_MIN=>NULL()
!$OMP THREADPRIVATE(XTI_MIN)
REAL, POINTER, DIMENSION(:)    :: XTI_MAX=>NULL()
!$OMP THREADPRIVATE(XTI_MAX)
REAL, POINTER, DIMENSION(:)    :: XTI_MEAN=>NULL()
!$OMP THREADPRIVATE(XTI_MEAN)
REAL, POINTER, DIMENSION(:)    :: XTI_STD=>NULL()
!$OMP THREADPRIVATE(XTI_STD)
REAL, POINTER, DIMENSION(:)    :: XTI_SKEW=>NULL()
!$OMP THREADPRIVATE(XTI_SKEW)
REAL, POINTER, DIMENSION(:)    :: XMUF=>NULL()
!$OMP THREADPRIVATE(XMUF)
REAL, POINTER, DIMENSION(:)    :: XFSAT=>NULL()
!$OMP THREADPRIVATE(XFSAT)
!
REAL, POINTER, DIMENSION(:,:)  :: XTAB_FSAT=>NULL()
!$OMP THREADPRIVATE(XTAB_FSAT)
REAL, POINTER, DIMENSION(:,:)  :: XTAB_WTOP=>NULL()
!$OMP THREADPRIVATE(XTAB_WTOP)
!
REAL, POINTER, DIMENSION(:,:)  :: XD_ICE=>NULL()
!$OMP THREADPRIVATE(XD_ICE)
REAL, POINTER, DIMENSION(:,:)  :: XKSAT_ICE=>NULL()
!$OMP THREADPRIVATE(XKSAT_ICE)
REAL, POINTER, DIMENSION(:,:)  :: XFRACSOC=>NULL()
!$OMP THREADPRIVATE(XFRACSOC)
!
INTEGER, POINTER :: NLAYER_HORT=>NULL()
!$OMP THREADPRIVATE(NLAYER_HORT)
INTEGER, POINTER :: NLAYER_DUN=>NULL()
!$OMP THREADPRIVATE(NLAYER_DUN)
!
!TRIP and Flood scheme
!
LOGICAL, POINTER               :: LTRIP=>NULL()
!$OMP THREADPRIVATE(LTRIP)
LOGICAL, POINTER               :: LFLOOD=>NULL()
!$OMP THREADPRIVATE(LFLOOD)
REAL, POINTER, DIMENSION(:)    :: XFFLOOD=>NULL()      
!$OMP THREADPRIVATE(XFFLOOD)
REAL, POINTER, DIMENSION(:)    :: XPIFLOOD=>NULL()      
!$OMP THREADPRIVATE(XPIFLOOD)
REAL, POINTER, DIMENSION(:)    :: XCPL_EFLOOD=>NULL()
!$OMP THREADPRIVATE(XCPL_EFLOOD)
REAL, POINTER, DIMENSION(:)    :: XCPL_PFLOOD=>NULL()
!$OMP THREADPRIVATE(XCPL_PFLOOD)
REAL, POINTER, DIMENSION(:)    :: XCPL_IFLOOD=>NULL()
!$OMP THREADPRIVATE(XCPL_IFLOOD)
REAL, POINTER, DIMENSION(:)    :: XCPL_DRAIN=>NULL()
!$OMP THREADPRIVATE(XCPL_DRAIN)
REAL, POINTER, DIMENSION(:)    :: XCPL_RUNOFF=>NULL()
!$OMP THREADPRIVATE(XCPL_RUNOFF)
REAL, POINTER, DIMENSION(:)    :: XCPL_ICEFLUX=>NULL()
!$OMP THREADPRIVATE(XCPL_ICEFLUX)
REAL, POINTER, DIMENSION(:,:)  :: XZ0_FLOOD=>NULL()
!$OMP THREADPRIVATE(XZ0_FLOOD)
REAL, POINTER :: XTSTEP_COUPLING=>NULL()
!$OMP THREADPRIVATE(XTSTEP_COUPLING)
!
CONTAINS

SUBROUTINE ISBA_GOTO_MODEL(KFROM, KTO, LKFROM)
LOGICAL, INTENT(IN) :: LKFROM
INTEGER, INTENT(IN) :: KFROM, KTO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Save current state for allocated arrays
IF (LKFROM) THEN
ISBA_MODEL(KFROM)%NSIZE_NATURE_P=>NSIZE_NATURE_P
ISBA_MODEL(KFROM)%NR_NATURE_P=>NR_NATURE_P
ISBA_MODEL(KFROM)%XPATCH=>XPATCH
ISBA_MODEL(KFROM)%XPATCH_OLD=>XPATCH_OLD
ISBA_MODEL(KFROM)%XVEGTYPE=>XVEGTYPE
ISBA_MODEL(KFROM)%XVEGTYPE_PATCH=>XVEGTYPE_PATCH
ISBA_MODEL(KFROM)%XSODELX=>XSODELX
ISBA_MODEL(KFROM)%XSOILGRID=>XSOILGRID
ISBA_MODEL(KFROM)%XZS=>XZS
ISBA_MODEL(KFROM)%XCOVER=>XCOVER
ISBA_MODEL(KFROM)%LCOVER=>LCOVER
ISBA_MODEL(KFROM)%XALBNIR_DRY=>XALBNIR_DRY
ISBA_MODEL(KFROM)%XALBVIS_DRY=>XALBVIS_DRY
ISBA_MODEL(KFROM)%XALBUV_DRY=>XALBUV_DRY
ISBA_MODEL(KFROM)%XALBNIR_WET=>XALBNIR_WET
ISBA_MODEL(KFROM)%XALBVIS_WET=>XALBVIS_WET
ISBA_MODEL(KFROM)%XALBUV_WET=>XALBUV_WET
ISBA_MODEL(KFROM)%XALBNIR_SOIL=>XALBNIR_SOIL
ISBA_MODEL(KFROM)%XALBVIS_SOIL=>XALBVIS_SOIL
ISBA_MODEL(KFROM)%XALBUV_SOIL=>XALBUV_SOIL
ISBA_MODEL(KFROM)%XEMIS_NAT=>XEMIS_NAT
ISBA_MODEL(KFROM)%XTSRAD_NAT=>XTSRAD_NAT
ISBA_MODEL(KFROM)%XAOSIP=>XAOSIP
ISBA_MODEL(KFROM)%XAOSIM=>XAOSIM
ISBA_MODEL(KFROM)%XAOSJP=>XAOSJP
ISBA_MODEL(KFROM)%XAOSJM=>XAOSJM
ISBA_MODEL(KFROM)%XHO2IP=>XHO2IP
ISBA_MODEL(KFROM)%XHO2IM=>XHO2IM
ISBA_MODEL(KFROM)%XHO2JP=>XHO2JP
ISBA_MODEL(KFROM)%XHO2JM=>XHO2JM
ISBA_MODEL(KFROM)%XZ0EFFIP=>XZ0EFFIP
ISBA_MODEL(KFROM)%XZ0EFFIM=>XZ0EFFIM
ISBA_MODEL(KFROM)%XZ0EFFJP=>XZ0EFFJP
ISBA_MODEL(KFROM)%XZ0EFFJM=>XZ0EFFJM
ISBA_MODEL(KFROM)%XZ0EFFJPDIR=>XZ0EFFJPDIR
ISBA_MODEL(KFROM)%XZ0REL=>XZ0REL
ISBA_MODEL(KFROM)%XSSO_SLOPE=>XSSO_SLOPE
ISBA_MODEL(KFROM)%XSSO_STDEV=>XSSO_STDEV
ISBA_MODEL(KFROM)%XZ0_O_Z0H=>XZ0_O_Z0H
ISBA_MODEL(KFROM)%XALBNIR=>XALBNIR
ISBA_MODEL(KFROM)%XALBVIS=>XALBVIS
ISBA_MODEL(KFROM)%XALBUV=>XALBUV
ISBA_MODEL(KFROM)%XEMIS=>XEMIS
ISBA_MODEL(KFROM)%XZ0=>XZ0
ISBA_MODEL(KFROM)%XALBNIR_VEG=>XALBNIR_VEG
ISBA_MODEL(KFROM)%XALBVIS_VEG=>XALBVIS_VEG
ISBA_MODEL(KFROM)%XALBUV_VEG=>XALBUV_VEG
ISBA_MODEL(KFROM)%XVEG=>XVEG
ISBA_MODEL(KFROM)%XWRMAX_CF=>XWRMAX_CF
ISBA_MODEL(KFROM)%XRSMIN=>XRSMIN
ISBA_MODEL(KFROM)%XGAMMA=>XGAMMA
ISBA_MODEL(KFROM)%XCV=>XCV
ISBA_MODEL(KFROM)%XRGL=>XRGL
ISBA_MODEL(KFROM)%XROOTFRAC=>XROOTFRAC
ISBA_MODEL(KFROM)%XABC=>XABC
ISBA_MODEL(KFROM)%XPOI=>XPOI
ISBA_MODEL(KFROM)%XBSLAI=>XBSLAI
ISBA_MODEL(KFROM)%XLAIMIN=>XLAIMIN
ISBA_MODEL(KFROM)%XSEFOLD=>XSEFOLD
ISBA_MODEL(KFROM)%XTAU_WOOD=>XTAU_WOOD
ISBA_MODEL(KFROM)%XH_TREE=>XH_TREE
ISBA_MODEL(KFROM)%XANF=>XANF
ISBA_MODEL(KFROM)%XANMAX=>XANMAX
ISBA_MODEL(KFROM)%XFZERO=>XFZERO
ISBA_MODEL(KFROM)%XEPSO=>XEPSO
ISBA_MODEL(KFROM)%XGAMM=>XGAMM
ISBA_MODEL(KFROM)%XQDGAMM=>XQDGAMM
ISBA_MODEL(KFROM)%XGMES=>XGMES
ISBA_MODEL(KFROM)%XRE25=>XRE25
ISBA_MODEL(KFROM)%XQDGMES=>XQDGMES
ISBA_MODEL(KFROM)%XT1GMES=>XT1GMES
ISBA_MODEL(KFROM)%XT2GMES=>XT2GMES
ISBA_MODEL(KFROM)%XAMAX=>XAMAX
ISBA_MODEL(KFROM)%XQDAMAX=>XQDAMAX
ISBA_MODEL(KFROM)%XT1AMAX=>XT1AMAX
ISBA_MODEL(KFROM)%XT2AMAX=>XT2AMAX
ISBA_MODEL(KFROM)%LSTRESS=>LSTRESS
ISBA_MODEL(KFROM)%XF2I=>XF2I
ISBA_MODEL(KFROM)%XGC=>XGC
ISBA_MODEL(KFROM)%XAH=>XAH
ISBA_MODEL(KFROM)%XBH=>XBH
ISBA_MODEL(KFROM)%XDMAX=>XDMAX
ISBA_MODEL(KFROM)%XCE_NITRO=>XCE_NITRO
ISBA_MODEL(KFROM)%XCF_NITRO=>XCF_NITRO
ISBA_MODEL(KFROM)%XCNA_NITRO=>XCNA_NITRO
ISBA_MODEL(KFROM)%XBSLAI_NITRO=>XBSLAI_NITRO
ISBA_MODEL(KFROM)%XSAND=>XSAND
ISBA_MODEL(KFROM)%XCLAY=>XCLAY
ISBA_MODEL(KFROM)%XSOC=>XSOC
ISBA_MODEL(KFROM)%XPERM=>XPERM
ISBA_MODEL(KFROM)%XRUNOFFB=>XRUNOFFB
ISBA_MODEL(KFROM)%XWDRAIN=>XWDRAIN
ISBA_MODEL(KFROM)%XTAUICE=>XTAUICE
ISBA_MODEL(KFROM)%XGAMMAT=>XGAMMAT
ISBA_MODEL(KFROM)%XDG_OLD=>XDG_OLD
ISBA_MODEL(KFROM)%XDG=>XDG
ISBA_MODEL(KFROM)%XDZG=>XDZG
ISBA_MODEL(KFROM)%XDZDIF=>XDZDIF
ISBA_MODEL(KFROM)%NWG_LAYER=>NWG_LAYER
ISBA_MODEL(KFROM)%XDROOT=>XDROOT
ISBA_MODEL(KFROM)%XDG2=>XDG2
ISBA_MODEL(KFROM)%XPH=>XPH
ISBA_MODEL(KFROM)%XFERT=>XFERT
ISBA_MODEL(KFROM)%XRUNOFFD=>XRUNOFFD
ISBA_MODEL(KFROM)%XSOILWGHT=>XSOILWGHT
ISBA_MODEL(KFROM)%XC1SAT=>XC1SAT
ISBA_MODEL(KFROM)%XC2REF=>XC2REF
ISBA_MODEL(KFROM)%XC3=>XC3
ISBA_MODEL(KFROM)%XC4B=>XC4B
ISBA_MODEL(KFROM)%XC4REF=>XC4REF
ISBA_MODEL(KFROM)%XACOEF=>XACOEF
ISBA_MODEL(KFROM)%XPCOEF=>XPCOEF
ISBA_MODEL(KFROM)%XWFC=>XWFC
ISBA_MODEL(KFROM)%XWWILT=>XWWILT
ISBA_MODEL(KFROM)%XWSAT=>XWSAT
ISBA_MODEL(KFROM)%XBCOEF=>XBCOEF
ISBA_MODEL(KFROM)%XCONDSAT=>XCONDSAT
ISBA_MODEL(KFROM)%XMPOTSAT=>XMPOTSAT
ISBA_MODEL(KFROM)%XF_PARAM=>XF_PARAM
ISBA_MODEL(KFROM)%XC_DEPTH_RATIO=>XC_DEPTH_RATIO
ISBA_MODEL(KFROM)%XCGSAT=>XCGSAT
ISBA_MODEL(KFROM)%XHCAPSOIL=>XHCAPSOIL
ISBA_MODEL(KFROM)%XCONDDRY=>XCONDDRY
ISBA_MODEL(KFROM)%XCONDSLD=>XCONDSLD
ISBA_MODEL(KFROM)%XTDEEP=>XTDEEP
ISBA_MODEL(KFROM)%XWR=>XWR
ISBA_MODEL(KFROM)%XTG=>XTG
ISBA_MODEL(KFROM)%XWG=>XWG
ISBA_MODEL(KFROM)%XWGI=>XWGI
ISBA_MODEL(KFROM)%XRESA=>XRESA
ISBA_MODEL(KFROM)%XPCPS=>XPCPS
ISBA_MODEL(KFROM)%XPLVTT=>XPLVTT
ISBA_MODEL(KFROM)%XPLSTT=>XPLSTT
ISBA_MODEL(KFROM)%XLAI=>XLAI
ISBA_MODEL(KFROM)%XAN=>XAN
ISBA_MODEL(KFROM)%XANDAY=>XANDAY
ISBA_MODEL(KFROM)%XANFM=>XANFM
ISBA_MODEL(KFROM)%XLE=>XLE
ISBA_MODEL(KFROM)%XFAPARC=>XFAPARC
ISBA_MODEL(KFROM)%XFAPIRC=>XFAPIRC
ISBA_MODEL(KFROM)%XLAI_EFFC=>XLAI_EFFC
ISBA_MODEL(KFROM)%XMUS=>XMUS
ISBA_MODEL(KFROM)%XRESP_BIOMASS=>XRESP_BIOMASS
ISBA_MODEL(KFROM)%XBIOMASS=>XBIOMASS
ISBA_MODEL(KFROM)%XINCREASE=>XINCREASE
ISBA_MODEL(KFROM)%XLITTER=>XLITTER
ISBA_MODEL(KFROM)%XSOILCARB=>XSOILCARB
ISBA_MODEL(KFROM)%XLIGNIN_STRUC=>XLIGNIN_STRUC
ISBA_MODEL(KFROM)%XTURNOVER=>XTURNOVER
ISBA_MODEL(KFROM)%TSEED=>TSEED
ISBA_MODEL(KFROM)%TREAP=>TREAP
ISBA_MODEL(KFROM)%XWATSUP=>XWATSUP
ISBA_MODEL(KFROM)%XIRRIG=>XIRRIG
!
ISBA_MODEL(KFROM)%XDIR_ALB_WITH_SNOW=>XDIR_ALB_WITH_SNOW
ISBA_MODEL(KFROM)%XSCA_ALB_WITH_SNOW=>XSCA_ALB_WITH_SNOW
!
ISBA_MODEL(KFROM)%XPSNG=>XPSNG
ISBA_MODEL(KFROM)%XPSNV_A=>XPSNV_A
ISBA_MODEL(KFROM)%XPSNV=>XPSNV
ISBA_MODEL(KFROM)%XPSN=>XPSN
ISBA_MODEL(KFROM)%XFFG=>XFFG
ISBA_MODEL(KFROM)%XFFV=>XFFV
ISBA_MODEL(KFROM)%XFFROZEN=>XFFROZEN
ISBA_MODEL(KFROM)%XFF=>XFF
ISBA_MODEL(KFROM)%XALBF=>XALBF
ISBA_MODEL(KFROM)%XEMISF=>XEMISF
ISBA_MODEL(KFROM)%XICE_STO=>XICE_STO
!
!SGH scheme
!
ISBA_MODEL(KFROM)%XTI_MIN=>XTI_MIN
ISBA_MODEL(KFROM)%XTI_MAX=>XTI_MAX
ISBA_MODEL(KFROM)%XTI_MEAN=>XTI_MEAN
ISBA_MODEL(KFROM)%XTI_STD=>XTI_STD
ISBA_MODEL(KFROM)%XTI_SKEW=>XTI_SKEW
ISBA_MODEL(KFROM)%XMUF=>XMUF
ISBA_MODEL(KFROM)%XFSAT=>XFSAT
!
ISBA_MODEL(KFROM)%XTAB_FSAT=>XTAB_FSAT
ISBA_MODEL(KFROM)%XTAB_WTOP=>XTAB_WTOP
!                          
ISBA_MODEL(KFROM)%XD_ICE=>XD_ICE
ISBA_MODEL(KFROM)%XKSAT_ICE=>XKSAT_ICE
ISBA_MODEL(KFROM)%XFRACSOC=>XFRACSOC
!
!Flood scheme
!
ISBA_MODEL(KFROM)%XZ0_FLOOD=>XZ0_FLOOD
ISBA_MODEL(KFROM)%XFFLOOD=>XFFLOOD     
ISBA_MODEL(KFROM)%XPIFLOOD=>XPIFLOOD     
ISBA_MODEL(KFROM)%XCPL_EFLOOD=>XCPL_EFLOOD
ISBA_MODEL(KFROM)%XCPL_PFLOOD=>XCPL_PFLOOD
ISBA_MODEL(KFROM)%XCPL_IFLOOD=>XCPL_IFLOOD
ISBA_MODEL(KFROM)%XCPL_DRAIN=>XCPL_DRAIN
ISBA_MODEL(KFROM)%XCPL_RUNOFF=>XCPL_RUNOFF
ISBA_MODEL(KFROM)%XCPL_ICEFLUX=>XCPL_ICEFLUX
ENDIF
!
! Current model is set to model KTO
IF (LHOOK) CALL DR_HOOK('MODD_ISBA_N:ISBA_GOTO_MODEL',0,ZHOOK_HANDLE)
CROUGH=>ISBA_MODEL(KTO)%CROUGH
CISBA=>ISBA_MODEL(KTO)%CISBA
CPEDOTF=>ISBA_MODEL(KTO)%CPEDOTF
CPHOTO=>ISBA_MODEL(KTO)%CPHOTO
LTR_ML=>ISBA_MODEL(KTO)%LTR_ML
XRM_PATCH=>ISBA_MODEL(KTO)%XRM_PATCH
CALBEDO=>ISBA_MODEL(KTO)%CALBEDO
CRUNOFF=>ISBA_MODEL(KTO)%CRUNOFF
CSCOND=>ISBA_MODEL(KTO)%CSCOND
CC1DRY=>ISBA_MODEL(KTO)%CC1DRY
CSOILFRZ=>ISBA_MODEL(KTO)%CSOILFRZ
CDIFSFCOND=>ISBA_MODEL(KTO)%CDIFSFCOND
CSNOWRES=>ISBA_MODEL(KTO)%CSNOWRES
CRESPSL=>ISBA_MODEL(KTO)%CRESPSL
CCPSURF=>ISBA_MODEL(KTO)%CCPSURF
LTEMP_ARP=>ISBA_MODEL(KTO)%LTEMP_ARP
LGLACIER=>ISBA_MODEL(KTO)%LGLACIER
LVEGUPD=>ISBA_MODEL(KTO)%LVEGUPD
LCANOPY=>ISBA_MODEL(KTO)%LCANOPY
LCANOPY_DRAG=>ISBA_MODEL(KTO)%LCANOPY_DRAG
LECOCLIMAP=>ISBA_MODEL(KTO)%LECOCLIMAP
LCTI=>ISBA_MODEL(KTO)%LCTI
LSOCP=>ISBA_MODEL(KTO)%LSOCP
LPERM=>ISBA_MODEL(KTO)%LPERM
LSPINUPCARBS=>ISBA_MODEL(KTO)%LSPINUPCARBS
LSPINUPCARBW=>ISBA_MODEL(KTO)%LSPINUPCARBW
XSPINMAXS=>ISBA_MODEL(KTO)%XSPINMAXS
XSPINMAXW=>ISBA_MODEL(KTO)%XSPINMAXW
NNBYEARSPINS=>ISBA_MODEL(KTO)%NNBYEARSPINS
NNBYEARSPINW=>ISBA_MODEL(KTO)%NNBYEARSPINW
NNBYEARSOLD=>ISBA_MODEL(KTO)%NNBYEARSOLD
NSPINS=>ISBA_MODEL(KTO)%NSPINS
NSPINW=>ISBA_MODEL(KTO)%NSPINW
LNOF=>ISBA_MODEL(KTO)%LNOF
NSIZE_NATURE_P=>ISBA_MODEL(KTO)%NSIZE_NATURE_P
NR_NATURE_P=>ISBA_MODEL(KTO)%NR_NATURE_P
XPATCH=>ISBA_MODEL(KTO)%XPATCH
XPATCH_OLD=>ISBA_MODEL(KTO)%XPATCH_OLD
XVEGTYPE=>ISBA_MODEL(KTO)%XVEGTYPE
XVEGTYPE_PATCH=>ISBA_MODEL(KTO)%XVEGTYPE_PATCH
NPATCH=>ISBA_MODEL(KTO)%NPATCH
NGROUND_LAYER=>ISBA_MODEL(KTO)%NGROUND_LAYER
NTEMPLAYER_ARP=>ISBA_MODEL(KTO)%NTEMPLAYER_ARP
XSODELX=>ISBA_MODEL(KTO)%XSODELX
XSOILGRID=>ISBA_MODEL(KTO)%XSOILGRID
NNBIOMASS=>ISBA_MODEL(KTO)%NNBIOMASS
NNLITTER=>ISBA_MODEL(KTO)%NNLITTER
NNLITTLEVS=>ISBA_MODEL(KTO)%NNLITTLEVS
NNSOILCARB=>ISBA_MODEL(KTO)%NNSOILCARB
XZS=>ISBA_MODEL(KTO)%XZS
XCOVER=>ISBA_MODEL(KTO)%XCOVER
LCOVER=>ISBA_MODEL(KTO)%LCOVER
XALBNIR_DRY=>ISBA_MODEL(KTO)%XALBNIR_DRY
XALBVIS_DRY=>ISBA_MODEL(KTO)%XALBVIS_DRY
XALBUV_DRY=>ISBA_MODEL(KTO)%XALBUV_DRY
XALBNIR_WET=>ISBA_MODEL(KTO)%XALBNIR_WET
XALBVIS_WET=>ISBA_MODEL(KTO)%XALBVIS_WET
XALBUV_WET=>ISBA_MODEL(KTO)%XALBUV_WET
XALBNIR_SOIL=>ISBA_MODEL(KTO)%XALBNIR_SOIL
XALBVIS_SOIL=>ISBA_MODEL(KTO)%XALBVIS_SOIL
XALBUV_SOIL=>ISBA_MODEL(KTO)%XALBUV_SOIL
XEMIS_NAT=>ISBA_MODEL(KTO)%XEMIS_NAT
XTSRAD_NAT=>ISBA_MODEL(KTO)%XTSRAD_NAT
XAOSIP=>ISBA_MODEL(KTO)%XAOSIP
XAOSIM=>ISBA_MODEL(KTO)%XAOSIM
XAOSJP=>ISBA_MODEL(KTO)%XAOSJP
XAOSJM=>ISBA_MODEL(KTO)%XAOSJM
XHO2IP=>ISBA_MODEL(KTO)%XHO2IP
XHO2IM=>ISBA_MODEL(KTO)%XHO2IM
XHO2JP=>ISBA_MODEL(KTO)%XHO2JP
XHO2JM=>ISBA_MODEL(KTO)%XHO2JM
XZ0EFFIP=>ISBA_MODEL(KTO)%XZ0EFFIP
XZ0EFFIM=>ISBA_MODEL(KTO)%XZ0EFFIM
XZ0EFFJP=>ISBA_MODEL(KTO)%XZ0EFFJP
XZ0EFFJM=>ISBA_MODEL(KTO)%XZ0EFFJM
XZ0EFFJPDIR=>ISBA_MODEL(KTO)%XZ0EFFJPDIR
XZ0REL=>ISBA_MODEL(KTO)%XZ0REL
XSSO_SLOPE=>ISBA_MODEL(KTO)%XSSO_SLOPE
XSSO_STDEV=>ISBA_MODEL(KTO)%XSSO_STDEV
XZ0_O_Z0H=>ISBA_MODEL(KTO)%XZ0_O_Z0H
XALBNIR=>ISBA_MODEL(KTO)%XALBNIR
XALBVIS=>ISBA_MODEL(KTO)%XALBVIS
XALBUV=>ISBA_MODEL(KTO)%XALBUV
XEMIS=>ISBA_MODEL(KTO)%XEMIS
XZ0=>ISBA_MODEL(KTO)%XZ0
XALBNIR_VEG=>ISBA_MODEL(KTO)%XALBNIR_VEG
XALBVIS_VEG=>ISBA_MODEL(KTO)%XALBVIS_VEG
XALBUV_VEG=>ISBA_MODEL(KTO)%XALBUV_VEG
XVEG=>ISBA_MODEL(KTO)%XVEG
XWRMAX_CF=>ISBA_MODEL(KTO)%XWRMAX_CF
XRSMIN=>ISBA_MODEL(KTO)%XRSMIN
XGAMMA=>ISBA_MODEL(KTO)%XGAMMA
XCV=>ISBA_MODEL(KTO)%XCV
XRGL=>ISBA_MODEL(KTO)%XRGL
XROOTFRAC=>ISBA_MODEL(KTO)%XROOTFRAC
XABC=>ISBA_MODEL(KTO)%XABC
XPOI=>ISBA_MODEL(KTO)%XPOI
XBSLAI=>ISBA_MODEL(KTO)%XBSLAI
XLAIMIN=>ISBA_MODEL(KTO)%XLAIMIN
XSEFOLD=>ISBA_MODEL(KTO)%XSEFOLD
XTAU_WOOD=>ISBA_MODEL(KTO)%XTAU_WOOD
XH_TREE=>ISBA_MODEL(KTO)%XH_TREE
XANF=>ISBA_MODEL(KTO)%XANF
XANMAX=>ISBA_MODEL(KTO)%XANMAX
XFZERO=>ISBA_MODEL(KTO)%XFZERO
XEPSO=>ISBA_MODEL(KTO)%XEPSO
XGAMM=>ISBA_MODEL(KTO)%XGAMM
XQDGAMM=>ISBA_MODEL(KTO)%XQDGAMM
XGMES=>ISBA_MODEL(KTO)%XGMES
XRE25=>ISBA_MODEL(KTO)%XRE25
XQDGMES=>ISBA_MODEL(KTO)%XQDGMES
XT1GMES=>ISBA_MODEL(KTO)%XT1GMES
XT2GMES=>ISBA_MODEL(KTO)%XT2GMES
XAMAX=>ISBA_MODEL(KTO)%XAMAX
XQDAMAX=>ISBA_MODEL(KTO)%XQDAMAX
XT1AMAX=>ISBA_MODEL(KTO)%XT1AMAX
XT2AMAX=>ISBA_MODEL(KTO)%XT2AMAX
LSTRESS=>ISBA_MODEL(KTO)%LSTRESS
XF2I=>ISBA_MODEL(KTO)%XF2I
XGC=>ISBA_MODEL(KTO)%XGC
XAH=>ISBA_MODEL(KTO)%XAH
XBH=>ISBA_MODEL(KTO)%XBH
XDMAX=>ISBA_MODEL(KTO)%XDMAX
XCE_NITRO=>ISBA_MODEL(KTO)%XCE_NITRO
XCF_NITRO=>ISBA_MODEL(KTO)%XCF_NITRO
XCNA_NITRO=>ISBA_MODEL(KTO)%XCNA_NITRO
XBSLAI_NITRO=>ISBA_MODEL(KTO)%XBSLAI_NITRO
XSAND=>ISBA_MODEL(KTO)%XSAND
XCLAY=>ISBA_MODEL(KTO)%XCLAY
XSOC=>ISBA_MODEL(KTO)%XSOC
XPERM=>ISBA_MODEL(KTO)%XPERM
XRUNOFFB=>ISBA_MODEL(KTO)%XRUNOFFB
XWDRAIN=>ISBA_MODEL(KTO)%XWDRAIN
XTAUICE=>ISBA_MODEL(KTO)%XTAUICE
XGAMMAT=>ISBA_MODEL(KTO)%XGAMMAT
XDG_OLD=>ISBA_MODEL(KTO)%XDG_OLD
XDG=>ISBA_MODEL(KTO)%XDG
XDZG=>ISBA_MODEL(KTO)%XDZG
XDZDIF=>ISBA_MODEL(KTO)%XDZDIF
NWG_LAYER=>ISBA_MODEL(KTO)%NWG_LAYER
XDROOT=>ISBA_MODEL(KTO)%XDROOT
XDG2=>ISBA_MODEL(KTO)%XDG2
XPH=>ISBA_MODEL(KTO)%XPH
XFERT=>ISBA_MODEL(KTO)%XFERT
XRUNOFFD=>ISBA_MODEL(KTO)%XRUNOFFD
XSOILWGHT=>ISBA_MODEL(KTO)%XSOILWGHT
XC1SAT=>ISBA_MODEL(KTO)%XC1SAT
XC2REF=>ISBA_MODEL(KTO)%XC2REF
XC3=>ISBA_MODEL(KTO)%XC3
XC4B=>ISBA_MODEL(KTO)%XC4B
XC4REF=>ISBA_MODEL(KTO)%XC4REF
XACOEF=>ISBA_MODEL(KTO)%XACOEF
XPCOEF=>ISBA_MODEL(KTO)%XPCOEF
XWFC=>ISBA_MODEL(KTO)%XWFC
XWWILT=>ISBA_MODEL(KTO)%XWWILT
XWSAT=>ISBA_MODEL(KTO)%XWSAT
XBCOEF=>ISBA_MODEL(KTO)%XBCOEF
XCONDSAT=>ISBA_MODEL(KTO)%XCONDSAT
XMPOTSAT=>ISBA_MODEL(KTO)%XMPOTSAT
XF_PARAM=>ISBA_MODEL(KTO)%XF_PARAM
XC_DEPTH_RATIO=>ISBA_MODEL(KTO)%XC_DEPTH_RATIO
XCGSAT=>ISBA_MODEL(KTO)%XCGSAT
XHCAPSOIL=>ISBA_MODEL(KTO)%XHCAPSOIL
XCONDDRY=>ISBA_MODEL(KTO)%XCONDDRY
XCONDSLD=>ISBA_MODEL(KTO)%XCONDSLD
XTDEEP=>ISBA_MODEL(KTO)%XTDEEP
TSNOW=>ISBA_MODEL(KTO)%TSNOW
XWR=>ISBA_MODEL(KTO)%XWR
XTG=>ISBA_MODEL(KTO)%XTG
XWG=>ISBA_MODEL(KTO)%XWG
XWGI=>ISBA_MODEL(KTO)%XWGI
XRESA=>ISBA_MODEL(KTO)%XRESA
XPCPS=>ISBA_MODEL(KTO)%XPCPS
XPLVTT=>ISBA_MODEL(KTO)%XPLVTT
XPLSTT=>ISBA_MODEL(KTO)%XPLSTT
XLAI=>ISBA_MODEL(KTO)%XLAI
XAN=>ISBA_MODEL(KTO)%XAN
XANDAY=>ISBA_MODEL(KTO)%XANDAY
XANFM=>ISBA_MODEL(KTO)%XANFM
XLE=>ISBA_MODEL(KTO)%XLE
XFAPARC=>ISBA_MODEL(KTO)%XFAPARC
XFAPIRC=>ISBA_MODEL(KTO)%XFAPIRC
XLAI_EFFC=>ISBA_MODEL(KTO)%XLAI_EFFC
XMUS=>ISBA_MODEL(KTO)%XMUS
XRESP_BIOMASS=>ISBA_MODEL(KTO)%XRESP_BIOMASS
XBIOMASS=>ISBA_MODEL(KTO)%XBIOMASS
XINCREASE=>ISBA_MODEL(KTO)%XINCREASE
XLITTER=>ISBA_MODEL(KTO)%XLITTER
XSOILCARB=>ISBA_MODEL(KTO)%XSOILCARB
XLIGNIN_STRUC=>ISBA_MODEL(KTO)%XLIGNIN_STRUC
XTURNOVER=>ISBA_MODEL(KTO)%XTURNOVER
TTIME=>ISBA_MODEL(KTO)%TTIME
XTSTEP=>ISBA_MODEL(KTO)%XTSTEP
XOUT_TSTEP=>ISBA_MODEL(KTO)%XOUT_TSTEP
TSEED=>ISBA_MODEL(KTO)%TSEED
TREAP=>ISBA_MODEL(KTO)%TREAP
XWATSUP=>ISBA_MODEL(KTO)%XWATSUP
XIRRIG=>ISBA_MODEL(KTO)%XIRRIG
XCGMAX=>ISBA_MODEL(KTO)%XCGMAX
XCDRAG=>ISBA_MODEL(KTO)%XCDRAG
!
XDIR_ALB_WITH_SNOW=>ISBA_MODEL(KTO)%XDIR_ALB_WITH_SNOW
XSCA_ALB_WITH_SNOW=>ISBA_MODEL(KTO)%XSCA_ALB_WITH_SNOW
!
XPSNG=>ISBA_MODEL(KTO)%XPSNG
XPSNV_A=>ISBA_MODEL(KTO)%XPSNV_A
XPSNV=>ISBA_MODEL(KTO)%XPSNV
XPSN=>ISBA_MODEL(KTO)%XPSN
XFFG=>ISBA_MODEL(KTO)%XFFG
XFFV=>ISBA_MODEL(KTO)%XFFV
XFFROZEN=>ISBA_MODEL(KTO)%XFFROZEN
XFF=>ISBA_MODEL(KTO)%XFF
XALBF=>ISBA_MODEL(KTO)%XALBF
XEMISF=>ISBA_MODEL(KTO)%XEMISF
XICE_STO=>ISBA_MODEL(KTO)%XICE_STO
!
!SGH scheme
!
CTOPREG=>ISBA_MODEL(KTO)%CTOPREG
CKSAT=>ISBA_MODEL(KTO)%CKSAT
CSOC=>ISBA_MODEL(KTO)%CSOC
CRAIN=>ISBA_MODEL(KTO)%CRAIN
CHORT=>ISBA_MODEL(KTO)%CHORT
!
XTI_MIN=>ISBA_MODEL(KTO)%XTI_MIN
XTI_MAX=>ISBA_MODEL(KTO)%XTI_MAX
XTI_MEAN=>ISBA_MODEL(KTO)%XTI_MEAN
XTI_STD=>ISBA_MODEL(KTO)%XTI_STD
XTI_SKEW=>ISBA_MODEL(KTO)%XTI_SKEW
XMUF=>ISBA_MODEL(KTO)%XMUF
XFSAT=>ISBA_MODEL(KTO)%XFSAT
!
XTAB_FSAT=>ISBA_MODEL(KTO)%XTAB_FSAT
XTAB_WTOP=>ISBA_MODEL(KTO)%XTAB_WTOP
!
XD_ICE=>ISBA_MODEL(KTO)%XD_ICE
XKSAT_ICE=>ISBA_MODEL(KTO)%XKSAT_ICE
XFRACSOC=>ISBA_MODEL(KTO)%XFRACSOC
!
NLAYER_HORT=>ISBA_MODEL(KTO)%NLAYER_HORT
NLAYER_DUN=>ISBA_MODEL(KTO)%NLAYER_DUN
!
!TRIP and Flood scheme
!
LTRIP=>ISBA_MODEL(KTO)%LTRIP
LFLOOD=>ISBA_MODEL(KTO)%LFLOOD
XZ0_FLOOD=>ISBA_MODEL(KTO)%XZ0_FLOOD
XFFLOOD=>ISBA_MODEL(KTO)%XFFLOOD     
XPIFLOOD=>ISBA_MODEL(KTO)%XPIFLOOD     
XCPL_EFLOOD=>ISBA_MODEL(KTO)%XCPL_EFLOOD
XCPL_PFLOOD=>ISBA_MODEL(KTO)%XCPL_PFLOOD
XCPL_IFLOOD=>ISBA_MODEL(KTO)%XCPL_IFLOOD
XCPL_DRAIN=>ISBA_MODEL(KTO)%XCPL_DRAIN
XCPL_RUNOFF=>ISBA_MODEL(KTO)%XCPL_RUNOFF
XCPL_ICEFLUX=>ISBA_MODEL(KTO)%XCPL_ICEFLUX
XTSTEP_COUPLING=>ISBA_MODEL(KTO)%XTSTEP_COUPLING
IF (LHOOK) CALL DR_HOOK('MODD_ISBA_N:ISBA_GOTO_MODEL',1,ZHOOK_HANDLE)
!
END SUBROUTINE ISBA_GOTO_MODEL

SUBROUTINE ISBA_ALLOC(KMODEL)
INTEGER, INTENT(IN) :: KMODEL
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_ISBA_N:ISBA_ALLOC",0,ZHOOK_HANDLE)
ALLOCATE(ISBA_MODEL(KMODEL))
DO J=1,KMODEL
  NULLIFY(ISBA_MODEL(J)%NSIZE_NATURE_P)
  NULLIFY(ISBA_MODEL(J)%NR_NATURE_P)
  NULLIFY(ISBA_MODEL(J)%XPATCH)
  NULLIFY(ISBA_MODEL(J)%XPATCH_OLD)  
  NULLIFY(ISBA_MODEL(J)%XVEGTYPE)
  NULLIFY(ISBA_MODEL(J)%XVEGTYPE_PATCH)
  NULLIFY(ISBA_MODEL(J)%XSODELX)
  NULLIFY(ISBA_MODEL(J)%XSOILGRID)
  NULLIFY(ISBA_MODEL(J)%XZS)
  NULLIFY(ISBA_MODEL(J)%XCOVER)
  NULLIFY(ISBA_MODEL(J)%LCOVER)
  NULLIFY(ISBA_MODEL(J)%XALBNIR_DRY)
  NULLIFY(ISBA_MODEL(J)%XALBVIS_DRY)
  NULLIFY(ISBA_MODEL(J)%XALBUV_DRY)
  NULLIFY(ISBA_MODEL(J)%XALBNIR_WET)
  NULLIFY(ISBA_MODEL(J)%XALBVIS_WET)
  NULLIFY(ISBA_MODEL(J)%XALBUV_WET)
  NULLIFY(ISBA_MODEL(J)%XALBNIR_SOIL)
  NULLIFY(ISBA_MODEL(J)%XALBVIS_SOIL)
  NULLIFY(ISBA_MODEL(J)%XALBUV_SOIL)
  NULLIFY(ISBA_MODEL(J)%XEMIS_NAT)
  NULLIFY(ISBA_MODEL(J)%XTSRAD_NAT)
  NULLIFY(ISBA_MODEL(J)%XAOSIP)
  NULLIFY(ISBA_MODEL(J)%XAOSIM)
  NULLIFY(ISBA_MODEL(J)%XAOSJP)
  NULLIFY(ISBA_MODEL(J)%XAOSJM)
  NULLIFY(ISBA_MODEL(J)%XHO2IP)
  NULLIFY(ISBA_MODEL(J)%XHO2IM)
  NULLIFY(ISBA_MODEL(J)%XHO2JP)
  NULLIFY(ISBA_MODEL(J)%XHO2JM)
  NULLIFY(ISBA_MODEL(J)%XZ0EFFIP)
  NULLIFY(ISBA_MODEL(J)%XZ0EFFIM)
  NULLIFY(ISBA_MODEL(J)%XZ0EFFJP)
  NULLIFY(ISBA_MODEL(J)%XZ0EFFJM)
  NULLIFY(ISBA_MODEL(J)%XZ0EFFJPDIR)
  NULLIFY(ISBA_MODEL(J)%XZ0REL)
  NULLIFY(ISBA_MODEL(J)%XSSO_SLOPE)
  NULLIFY(ISBA_MODEL(J)%XSSO_STDEV)
  NULLIFY(ISBA_MODEL(J)%XZ0_O_Z0H)
  NULLIFY(ISBA_MODEL(J)%XALBNIR)
  NULLIFY(ISBA_MODEL(J)%XALBVIS)
  NULLIFY(ISBA_MODEL(J)%XALBUV)
  NULLIFY(ISBA_MODEL(J)%XEMIS)
  NULLIFY(ISBA_MODEL(J)%XZ0)
  NULLIFY(ISBA_MODEL(J)%XALBNIR_VEG)
  NULLIFY(ISBA_MODEL(J)%XALBVIS_VEG)
  NULLIFY(ISBA_MODEL(J)%XALBUV_VEG)
  NULLIFY(ISBA_MODEL(J)%XVEG)
  NULLIFY(ISBA_MODEL(J)%XWRMAX_CF)
  NULLIFY(ISBA_MODEL(J)%XRSMIN)
  NULLIFY(ISBA_MODEL(J)%XGAMMA)
  NULLIFY(ISBA_MODEL(J)%XCV)
  NULLIFY(ISBA_MODEL(J)%XRGL)
  NULLIFY(ISBA_MODEL(J)%XROOTFRAC)
  NULLIFY(ISBA_MODEL(J)%XABC)
  NULLIFY(ISBA_MODEL(J)%XPOI)  
  NULLIFY(ISBA_MODEL(J)%XBSLAI)
  NULLIFY(ISBA_MODEL(J)%XLAIMIN)
  NULLIFY(ISBA_MODEL(J)%XSEFOLD)
  NULLIFY(ISBA_MODEL(J)%XTAU_WOOD)
  NULLIFY(ISBA_MODEL(J)%XH_TREE)
  NULLIFY(ISBA_MODEL(J)%XANF)
  NULLIFY(ISBA_MODEL(J)%XANMAX)
  NULLIFY(ISBA_MODEL(J)%XFZERO)
  NULLIFY(ISBA_MODEL(J)%XEPSO)
  NULLIFY(ISBA_MODEL(J)%XGAMM)
  NULLIFY(ISBA_MODEL(J)%XQDGAMM)
  NULLIFY(ISBA_MODEL(J)%XGMES)
  NULLIFY(ISBA_MODEL(J)%XRE25)
  NULLIFY(ISBA_MODEL(J)%XQDGMES)
  NULLIFY(ISBA_MODEL(J)%XT1GMES)
  NULLIFY(ISBA_MODEL(J)%XT2GMES)
  NULLIFY(ISBA_MODEL(J)%XAMAX)
  NULLIFY(ISBA_MODEL(J)%XQDAMAX)
  NULLIFY(ISBA_MODEL(J)%XT1AMAX)
  NULLIFY(ISBA_MODEL(J)%XT2AMAX)
  NULLIFY(ISBA_MODEL(J)%LSTRESS)
  NULLIFY(ISBA_MODEL(J)%XF2I)
  NULLIFY(ISBA_MODEL(J)%XGC)
  NULLIFY(ISBA_MODEL(J)%XAH)
  NULLIFY(ISBA_MODEL(J)%XBH)
  NULLIFY(ISBA_MODEL(J)%XDMAX)
  NULLIFY(ISBA_MODEL(J)%XCE_NITRO)
  NULLIFY(ISBA_MODEL(J)%XCF_NITRO)
  NULLIFY(ISBA_MODEL(J)%XCNA_NITRO)
  NULLIFY(ISBA_MODEL(J)%XBSLAI_NITRO)
  NULLIFY(ISBA_MODEL(J)%XSAND)
  NULLIFY(ISBA_MODEL(J)%XCLAY)
  NULLIFY(ISBA_MODEL(J)%XRUNOFFB)
  NULLIFY(ISBA_MODEL(J)%XWDRAIN)
  NULLIFY(ISBA_MODEL(J)%XTAUICE)
  NULLIFY(ISBA_MODEL(J)%XGAMMAT)
  NULLIFY(ISBA_MODEL(J)%XDG_OLD)
  NULLIFY(ISBA_MODEL(J)%XDG)
  NULLIFY(ISBA_MODEL(J)%XDZG)
  NULLIFY(ISBA_MODEL(J)%XDZDIF)
  NULLIFY(ISBA_MODEL(J)%NWG_LAYER)
  NULLIFY(ISBA_MODEL(J)%XDROOT)
  NULLIFY(ISBA_MODEL(J)%XDG2)
  NULLIFY(ISBA_MODEL(J)%XPH)
  NULLIFY(ISBA_MODEL(J)%XFERT)
  NULLIFY(ISBA_MODEL(J)%XRUNOFFD)
  NULLIFY(ISBA_MODEL(J)%XSOILWGHT)
  NULLIFY(ISBA_MODEL(J)%XC1SAT)
  NULLIFY(ISBA_MODEL(J)%XC2REF)
  NULLIFY(ISBA_MODEL(J)%XC3)
  NULLIFY(ISBA_MODEL(J)%XC4B)
  NULLIFY(ISBA_MODEL(J)%XC4REF)
  NULLIFY(ISBA_MODEL(J)%XACOEF)
  NULLIFY(ISBA_MODEL(J)%XPCOEF)
  NULLIFY(ISBA_MODEL(J)%XWFC)
  NULLIFY(ISBA_MODEL(J)%XWWILT)
  NULLIFY(ISBA_MODEL(J)%XWSAT)
  NULLIFY(ISBA_MODEL(J)%XBCOEF)
  NULLIFY(ISBA_MODEL(J)%XCONDSAT)
  NULLIFY(ISBA_MODEL(J)%XMPOTSAT)
  NULLIFY(ISBA_MODEL(J)%XF_PARAM)
  NULLIFY(ISBA_MODEL(J)%XC_DEPTH_RATIO)
  NULLIFY(ISBA_MODEL(J)%XCGSAT)
  NULLIFY(ISBA_MODEL(J)%XHCAPSOIL)
  NULLIFY(ISBA_MODEL(J)%XCONDDRY)
  NULLIFY(ISBA_MODEL(J)%XCONDSLD)
  NULLIFY(ISBA_MODEL(J)%XTDEEP)
  NULLIFY(ISBA_MODEL(J)%XWR)
  NULLIFY(ISBA_MODEL(J)%XTG)
  NULLIFY(ISBA_MODEL(J)%XWG)
  NULLIFY(ISBA_MODEL(J)%XWGI)
  NULLIFY(ISBA_MODEL(J)%XRESA)
  NULLIFY(ISBA_MODEL(J)%XPCPS)
  NULLIFY(ISBA_MODEL(J)%XPLVTT)
  NULLIFY(ISBA_MODEL(J)%XPLSTT)
  NULLIFY(ISBA_MODEL(J)%XLAI)
  NULLIFY(ISBA_MODEL(J)%XAN)
  NULLIFY(ISBA_MODEL(J)%XANDAY)
  NULLIFY(ISBA_MODEL(J)%XANFM)
  NULLIFY(ISBA_MODEL(J)%XLE)
  NULLIFY(ISBA_MODEL(J)%XFAPARC)
  NULLIFY(ISBA_MODEL(J)%XFAPIRC)
  NULLIFY(ISBA_MODEL(J)%XLAI_EFFC)  
  NULLIFY(ISBA_MODEL(J)%XMUS)   
  NULLIFY(ISBA_MODEL(J)%XRESP_BIOMASS)
  NULLIFY(ISBA_MODEL(J)%XBIOMASS)
  NULLIFY(ISBA_MODEL(J)%XINCREASE)
  NULLIFY(ISBA_MODEL(J)%XLITTER)
  NULLIFY(ISBA_MODEL(J)%XSOILCARB)
  NULLIFY(ISBA_MODEL(J)%XLIGNIN_STRUC)
  NULLIFY(ISBA_MODEL(J)%XTURNOVER)
  NULLIFY(ISBA_MODEL(J)%XWATSUP)
  NULLIFY(ISBA_MODEL(J)%XIRRIG)
  NULLIFY(ISBA_MODEL(J)%XTAB_FSAT)
  NULLIFY(ISBA_MODEL(J)%XTAB_WTOP)
  NULLIFY(ISBA_MODEL(J)%XD_ICE)
  NULLIFY(ISBA_MODEL(J)%XKSAT_ICE)
  NULLIFY(ISBA_MODEL(J)%XTI_MIN)
  NULLIFY(ISBA_MODEL(J)%XTI_MAX)
  NULLIFY(ISBA_MODEL(J)%XTI_MEAN)
  NULLIFY(ISBA_MODEL(J)%XTI_STD)
  NULLIFY(ISBA_MODEL(J)%XTI_SKEW)
  NULLIFY(ISBA_MODEL(J)%XMUF)
  NULLIFY(ISBA_MODEL(J)%XFSAT)
  NULLIFY(ISBA_MODEL(J)%XPSNG)
  NULLIFY(ISBA_MODEL(J)%XPSNV)
  NULLIFY(ISBA_MODEL(J)%XPSNV_A)
  NULLIFY(ISBA_MODEL(J)%XPSN)
  NULLIFY(ISBA_MODEL(J)%XDIR_ALB_WITH_SNOW)
  NULLIFY(ISBA_MODEL(J)%XSCA_ALB_WITH_SNOW)
  NULLIFY(ISBA_MODEL(J)%XFFG)
  NULLIFY(ISBA_MODEL(J)%XFFV)
  NULLIFY(ISBA_MODEL(J)%XFFROZEN)
  NULLIFY(ISBA_MODEL(J)%XFF)
  NULLIFY(ISBA_MODEL(J)%XALBF)
  NULLIFY(ISBA_MODEL(J)%XEMISF)
  NULLIFY(ISBA_MODEL(J)%XICE_STO)
  NULLIFY(ISBA_MODEL(J)%XFFLOOD)
  NULLIFY(ISBA_MODEL(J)%XPIFLOOD)
  NULLIFY(ISBA_MODEL(J)%XCPL_EFLOOD)
  NULLIFY(ISBA_MODEL(J)%XCPL_PFLOOD)
  NULLIFY(ISBA_MODEL(J)%XCPL_IFLOOD)
  NULLIFY(ISBA_MODEL(J)%XCPL_DRAIN)
  NULLIFY(ISBA_MODEL(J)%XCPL_RUNOFF)
  NULLIFY(ISBA_MODEL(J)%XCPL_ICEFLUX)
  NULLIFY(ISBA_MODEL(J)%XZ0_FLOOD)
ENDDO
ISBA_MODEL(:)%CROUGH=' '
ISBA_MODEL(:)%CISBA=' '
ISBA_MODEL(:)%CPEDOTF=' '
ISBA_MODEL(:)%CPHOTO=' '
ISBA_MODEL(:)%LTR_ML=.FALSE.
ISBA_MODEL(:)%XRM_PATCH=0.0
ISBA_MODEL(:)%CALBEDO=' '
ISBA_MODEL(:)%CSCOND=' '
ISBA_MODEL(:)%CC1DRY=' '
ISBA_MODEL(:)%CSOILFRZ=' '
ISBA_MODEL(:)%CDIFSFCOND=' '
ISBA_MODEL(:)%CSNOWRES=' '
ISBA_MODEL(:)%CRESPSL=' '
ISBA_MODEL(:)%CCPSURF=' '
ISBA_MODEL(:)%LTEMP_ARP=.FALSE.
ISBA_MODEL(:)%LGLACIER=.FALSE.
ISBA_MODEL(:)%LVEGUPD=.FALSE.
ISBA_MODEL(:)%LCANOPY=.FALSE.
ISBA_MODEL(:)%LCANOPY_DRAG=.FALSE.
ISBA_MODEL(:)%LECOCLIMAP=.FALSE.
ISBA_MODEL(:)%LCTI=.FALSE.
ISBA_MODEL(:)%LSOCP=.FALSE.
ISBA_MODEL(:)%LPERM=.FALSE.
ISBA_MODEL(:)%LSPINUPCARBS=.FALSE.
ISBA_MODEL(:)%LSPINUPCARBW=.FALSE.
ISBA_MODEL(:)%XSPINMAXS=0.
ISBA_MODEL(:)%XSPINMAXW=0.
ISBA_MODEL(:)%NNBYEARSPINS=0
ISBA_MODEL(:)%NNBYEARSPINW=0
ISBA_MODEL(:)%NNBYEARSOLD=0
ISBA_MODEL(:)%NSPINS=1
ISBA_MODEL(:)%NSPINW=1
ISBA_MODEL(:)%LNOF=.FALSE.
ISBA_MODEL(:)%NPATCH=0
ISBA_MODEL(:)%NGROUND_LAYER=0
ISBA_MODEL(:)%NTEMPLAYER_ARP=0
ISBA_MODEL(:)%NNBIOMASS=0
ISBA_MODEL(:)%NNLITTER=0
ISBA_MODEL(:)%NNLITTLEVS=0
ISBA_MODEL(:)%NNSOILCARB=0
ISBA_MODEL(:)%XTSTEP=0.
ISBA_MODEL(:)%XOUT_TSTEP=0.
ISBA_MODEL(:)%XCGMAX=0.
ISBA_MODEL(:)%XCDRAG=0.
ISBA_MODEL(:)%CRUNOFF=' '
ISBA_MODEL(:)%CTOPREG=' '
ISBA_MODEL(:)%CKSAT=' '
ISBA_MODEL(:)%CSOC=' '
ISBA_MODEL(:)%CRAIN=' '
ISBA_MODEL(:)%CHORT=' '
ISBA_MODEL(:)%NLAYER_HORT=0
ISBA_MODEL(:)%NLAYER_DUN=0
ISBA_MODEL(:)%LTRIP=.FALSE.
ISBA_MODEL(:)%LFLOOD=.FALSE.
ISBA_MODEL(:)%XTSTEP_COUPLING=0.
IF (LHOOK) CALL DR_HOOK("MODD_ISBA_N:ISBA_ALLOC",1,ZHOOK_HANDLE)
END SUBROUTINE ISBA_ALLOC

SUBROUTINE ISBA_DEALLO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_ISBA_N:ISBA_DEALLO",0,ZHOOK_HANDLE)
IF (ALLOCATED(ISBA_MODEL)) DEALLOCATE(ISBA_MODEL)
IF (LHOOK) CALL DR_HOOK("MODD_ISBA_N:ISBA_DEALLO",1,ZHOOK_HANDLE)
END SUBROUTINE ISBA_DEALLO

END MODULE MODD_ISBA_n
