!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!######################
MODULE MODD_PACK_ISBA
!######################
!
!!****  *MODD_PACK_ISBA - declaration of packed surface parameters for ISBA scheme
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
!!      P. Samuelsson   10/2014 : MEB and additional snow albedos
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TYPE_DATE_SURF      
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!-------------------------------------------------------------------------------
!
TYPE PACK_ISBA_t

  INTEGER :: NSIZE_LSIMPLE
  INTEGER :: NSIZE_L0
  INTEGER :: NSIZE_NSIMPLE
  INTEGER :: NSIZE_N0
  INTEGER :: NSIZE_TSIMPLE
  INTEGER :: NSIZE_T0
  INTEGER :: NSIZE_SIMPLE
  INTEGER :: NSIZE_GROUND
  INTEGER :: NSIZE_VEGTYPE
  INTEGER :: NSIZE_TG
  INTEGER :: NSIZE_SNOW
  INTEGER :: NSIZE_ALB
  INTEGER :: NSIZE_2
  INTEGER :: NSIZE_BIOMASS
  INTEGER :: NSIZE_SOILCARB
  INTEGER :: NSIZE_LITTLEVS
  INTEGER :: NSIZE_LITTER
  INTEGER :: NSIZE_0
  INTEGER :: NSIZE_00
  INTEGER :: NSIZE_000
  INTEGER :: NSIZE_01
  LOGICAL, POINTER, DIMENSION(:,:) :: LBLOCK_SIMPLE 
  LOGICAL, POINTER, DIMENSION(:,:) :: LBLOCK_0
  INTEGER, POINTER, DIMENSION(:,:) :: NBLOCK_SIMPLE 
  INTEGER, POINTER, DIMENSION(:,:) :: NBLOCK_0
  TYPE(DATE_TIME), POINTER, DIMENSION(:,:) :: TBLOCK_SIMPLE
  TYPE(DATE_TIME), POINTER, DIMENSION(:,:) :: TBLOCK_0
  REAL, POINTER, DIMENSION(:,:) :: XBLOCK_SIMPLE
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_GROUND
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_VEGTYPE
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_TG
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_SNOW
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_ALB
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_2
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_BIOMASS
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_SOILCARB
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_LITTLEVS
  REAL, POINTER, DIMENSION(:,:,:,:) :: XBLOCK_LITTER
  REAL, POINTER, DIMENSION(:,:) :: XBLOCK_0
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_00
  REAL, POINTER, DIMENSION(:,:,:,:) :: XBLOCK_000
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_01
!
! Mask and number of grid elements containing patches/tiles:
!
  REAL, POINTER, DIMENSION(:,:)  :: XP_VEGTYPE_PATCH ! fraction of each vegetation type for
!                                                      ! each vegetation unit/patch              (-)
!
! General surface parameters:
!
  REAL, POINTER, DIMENSION(:)    :: XP_SSO_SLOPE     ! orography slope within the grid mesh    (-)
  REAL, POINTER, DIMENSION(:)    :: XP_LAT           ! latitude    (-)
  REAL, POINTER, DIMENSION(:)    :: XP_LON           ! latitude    (-)
!
! Subgrid orography parameters
!
  REAL, DIMENSION(:), POINTER :: XP_AOSIP,XP_AOSIM,XP_AOSJP,XP_AOSJM
! directional A/S quantities in 4 coordinate directions
! (IP: i index up;  IM: i index down;  JP: j index up;  JM: j index down)
! They are used in soil routines to compute effective roughness length
!
  REAL, DIMENSION(:), POINTER :: XP_HO2IP,XP_HO2IM,XP_HO2JP,XP_HO2JM
! directional h/2 quantities in 4 coordinate directions
! (IP: i index up;  IM: i index down;  JP: j index up;  JM: j index down)
! They are used in soil routines to compute effective roughness length
!
  REAL, DIMENSION(:), POINTER :: XP_Z0EFFIP,XP_Z0EFFIM,XP_Z0EFFJP,XP_Z0EFFJM
! directional total roughness lenghts in 4 coordinate directions
! (IP: i index up;  IM: i index down;  JP: j index up;  JM: j index down)
!
  REAL, POINTER, DIMENSION(:) :: XP_Z0REL         ! relief roughness length                 (m)

!
! Input Parameters:
!
! - bare soil:
!
  REAL, POINTER, DIMENSION(:,:) :: XP_CLAY         ! clay fraction profile                   (-)
  REAL, POINTER, DIMENSION(:,:) :: XP_SAND         ! sand fraction profile                   (-)

  REAL, POINTER, DIMENSION(:) :: XP_ALBNIR_DRY     ! near-infra-red albedo of dry soil       (-)
  REAL, POINTER, DIMENSION(:) :: XP_ALBVIS_DRY     ! visible albedo of dry soil              (-)
  REAL, POINTER, DIMENSION(:) :: XP_ALBUV_DRY      ! UV albedo of dry soil                   (-)
  REAL, POINTER, DIMENSION(:) :: XP_ALBNIR_WET     ! near-infra-red albedo of wet soil       (-)
  REAL, POINTER, DIMENSION(:) :: XP_ALBVIS_WET     ! visible albedo of wet soil              (-)
  REAL, POINTER, DIMENSION(:) :: XP_ALBUV_WET      ! UV albedo of wet soil                   (-)
  REAL, POINTER, DIMENSION(:) :: XP_ALBNIR_SOIL    ! near-infra-red albedo of wet soil       (-)
  REAL, POINTER, DIMENSION(:) :: XP_ALBVIS_SOIL    ! visible albedo of soil                  (-)
  REAL, POINTER, DIMENSION(:) :: XP_ALBUV_SOIL     ! UV albedo of soil                       (-)
!
! - vegetation + bare soil:
!
  REAL, POINTER, DIMENSION(:) :: XP_Z0_O_Z0H       ! ratio of surface roughness lengths
!                                                    ! (momentum to heat)                      (-)
  REAL, POINTER, DIMENSION(:) :: XP_ALBNIR         ! near-infra-red albedo                   (-)
  REAL, POINTER, DIMENSION(:) :: XP_ALBVIS         ! visible albedo                          (-)
  REAL, POINTER, DIMENSION(:) :: XP_ALBUV          ! UV albedo                               (-)
  REAL, POINTER, DIMENSION(:) :: XP_EMIS           ! snow-free surface emissivity                      (-)
  REAL, POINTER, DIMENSION(:) :: XP_Z0             ! snow-free surface roughness length                (m)
!
! - vegetation :
!
  REAL, POINTER, DIMENSION(:) :: XP_ALBNIR_VEG     ! near-infra-red albedo of vegetation     (-)
  REAL, POINTER, DIMENSION(:) :: XP_ALBVIS_VEG     ! visible albedo of vegetation            (-)
  REAL, POINTER, DIMENSION(:) :: XP_ALBUV_VEG      ! UV albedo of vegetation                 (-)
!
!
! - vegetation: default option (Jarvis) and general parameters:
!
  REAL, POINTER, DIMENSION(:) :: XP_VEG            ! vegetation cover fraction               (-)
  REAL, POINTER, DIMENSION(:) :: XP_WRMAX_CF       ! coefficient for maximum water 
!                                                    ! interception 
!                                                    ! storage capacity on the vegetation      (-)
  REAL, POINTER, DIMENSION(:) :: XP_RSMIN          ! minimum stomatal resistance             (s/m)
  REAL, POINTER, DIMENSION(:) :: XP_GAMMA          ! coefficient for the calculation
!                                                    ! of the surface stomatal
!                                                    ! resistance
  REAL, POINTER, DIMENSION(:) :: XP_CV             ! vegetation thermal inertia coefficient  (K m2/J)
  REAL, POINTER, DIMENSION(:) :: XP_RGL            ! maximum solar radiation
!                                                    ! usable in photosynthesis                (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XP_ROOTFRAC     ! root fraction profile ('DIF' option)
!
! - vegetation: Ags parameters ('AGS', 'LAI', 'AST', 'LST', 'NIT', 'NCB' options)
!
  REAL, POINTER, DIMENSION(:)    :: XP_BSLAI      ! ratio d(biomass)/d(lai)                 (kg/m2)
  REAL, POINTER, DIMENSION(:)    :: XP_LAIMIN     ! minimum LAI (Leaf Area Index)           (m2/m2)
  REAL, POINTER, DIMENSION(:)    :: XP_SEFOLD     ! e-folding time for senescence           (s)
  REAL, POINTER, DIMENSION(:)    :: XP_H_TREE     ! height of trees                         (m)
  REAL, POINTER, DIMENSION(:)    :: XP_ANF        ! total assimilation over canopy          (
  REAL, POINTER, DIMENSION(:)    :: XP_ANMAX      ! maximum photosynthesis rate             (
  REAL, POINTER, DIMENSION(:)    :: XP_FZERO      ! ideal value of F, no photo- 
!                                                   ! respiration or saturation deficit       (
  REAL, POINTER, DIMENSION(:)    :: XP_EPSO       ! maximum initial quantum use             
!                                                   ! efficiency                              (mg J-1 PAR)
  REAL, POINTER, DIMENSION(:)    :: XP_GAMM       ! CO2 conpensation concentration          (ppmv)
  REAL, POINTER, DIMENSION(:)    :: XP_QDGAMM     ! Log of Q10 function for CO2 conpensation 
!                                                   ! concentration                           (-)
  REAL, POINTER, DIMENSION(:)    :: XP_GMES       ! mesophyll conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:)    :: XP_RE25       ! Ecosystem respiration parameter         (kg m-2 s-1)
  REAL, POINTER, DIMENSION(:)    :: XP_QDGMES     ! Log of Q10 function for mesophyll conductance  (-)
  REAL, POINTER, DIMENSION(:)    :: XP_T1GMES     ! reference temperature for computing 
!                                                   ! compensation concentration function for 
!                                                   ! mesophyll conductance: minimum
!                                                   ! temperature                             (K)
  REAL, POINTER, DIMENSION(:)    :: XP_T2GMES     ! reference temperature for computing 
!                                                   ! compensation concentration function for 
!                                                   ! mesophyll conductance: maximum
!                                                   ! temperature                             (K)
  REAL, POINTER, DIMENSION(:)    :: XP_AMAX       ! leaf photosynthetic capacity            (kg m-2 s-1)
  REAL, POINTER, DIMENSION(:)    :: XP_QDAMAX     ! Log of Q10 function for leaf photosynthetic 
!                                                   ! capacity                                (-)
  REAL, POINTER, DIMENSION(:)    :: XP_T1AMAX     ! reference temperature for computing 
!                                                   ! compensation concentration function for 
!                                                   ! leaf photosynthetic capacity: minimum
!                                                   ! temperature                             (K)
  REAL, POINTER, DIMENSION(:)    :: XP_T2AMAX     ! reference temperature for computing 
!                                                   ! compensation concentration function for 
!                                                   ! leaf photosynthetic capacity: maximum
!                                                   ! temperature                             (K)
!
! - vegetation: Ags Stress parameters ('AST', 'LST', 'NIT', 'NCB' options)
!
  LOGICAL, POINTER, DIMENSION(:) :: LP_STRESS      ! vegetation response type to water
!                                                    ! stress (true:defensive false:offensive) (-)
  REAL, POINTER, DIMENSION(:)    :: XP_F2I         ! critical normilized soil water 
!                                                    ! content for stress parameterisation
  REAL, POINTER, DIMENSION(:)    :: XP_GC          ! cuticular conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:)    :: XP_AH          ! coefficients for herbaceous water stress 
!                                                    ! response (offensive or defensive)       (log(mm/s))
  REAL, POINTER, DIMENSION(:)    :: XP_BH          ! coefficients for herbaceous water stress 
!                                                    ! response (offensive or defensive)       (-)
  REAL, POINTER, DIMENSION(:)    :: XP_TAU_WOOD    ! residence time in woody biomass         (s)
  REAL, POINTER, DIMENSION(:)    :: XP_DMAX        ! maximum air saturation deficit
!                                                    ! tolerate by vegetation                  (kg/kg)
!
! - vegetation: Ags Nitrogen-model parameters ('NIT', 'NCB' option)
!
  REAL, POINTER, DIMENSION(:)    :: XP_CE_NITRO      ! leaf aera ratio sensitivity to 
!                                                      ! nitrogen concentration                (m2/kg)
  REAL, POINTER, DIMENSION(:)    :: XP_CF_NITRO      ! lethal minimum value of leaf area
!                                                      ! ratio                                 (m2/kg)
  REAL, POINTER, DIMENSION(:)    :: XP_CNA_NITRO     ! nitrogen concentration of active 
!                                                      ! biomass                               (kg/kg)
  REAL, POINTER, DIMENSION(:)    :: XP_BSLAI_NITRO! biomass/LAI ratio from nitrogen 
!                                                      ! decline theory                        (kg/m2)
!
! - soil: primary parameters
!
  REAL, POINTER, DIMENSION(:)      :: XP_RUNOFFB     ! sub-grid surface runoff slope parameter (-)
  REAL, POINTER, DIMENSION(:)      :: XP_WDRAIN      ! continuous drainage parameter           (-)
  REAL, POINTER, DIMENSION(:)      :: XP_TAUICE      ! soil freezing characteristic timescale  (s)
  REAL, POINTER, DIMENSION(:)      :: XP_GAMMAT      ! 'Force-Restore' timescale when using a
!                                                      ! prescribed lower boundary temperature   (1/days)
  REAL, POINTER, DIMENSION(:,:)    :: XP_DG              ! soil layer depth                  (m)
!                                                      ! NOTE: in Force-Restore mode, the 
!                                                      ! uppermost layer thickness is superficial
!                                                      ! and is only explicitly used for soil 
!                                                      ! water phase changes                     (m)
  REAL, POINTER, DIMENSION(:,:)    :: XP_DZG             ! soil layer thicknesses                  (m)
  REAL, POINTER, DIMENSION(:,:)    :: XP_DZDIF           ! distance between consecuative layer mid-points(m)
  INTEGER, POINTER, DIMENSION(:)   :: NK_WG_LAYER        ! Number of soil moisture layers for DIF

  REAL, POINTER, DIMENSION(:)      :: XP_RUNOFFD       ! depth over which sub-grid runoff is
!                                                      ! computed: in Force-Restore this is the
!                                                      ! total soil column ('2-L'), or root zone
!                                                      ! ('3-L'). For the 'DIF' option, it can
!                                                      ! be any depth within soil column         (m)
!
  REAL, POINTER, DIMENSION(:,:)  :: XP_SOILWGHT      ! ISBA-DIF: weights for vertical
!                                                  ! integration of soil water and properties
!
! - soil: Secondary parameters: hydrology
!
  REAL, POINTER, DIMENSION(:)    :: XP_C1SAT       ! 'Force-Restore' C1 coefficient at 
!                                                    ! saturation                              (-)
  REAL, POINTER, DIMENSION(:)    :: XP_C2REF       ! 'Force-Restore' reference value of C2   (-)
  REAL, POINTER, DIMENSION(:,:)  :: XP_C3          ! 'Force-Restore' C3 drainage coefficient (m)
  REAL, POINTER, DIMENSION(:)    :: XP_C4B         ! 'Force-Restore' sub-surface vertical 
!                                                    ! diffusion coefficient (slope parameter) (-)
  REAL, POINTER, DIMENSION(:)    :: XP_C4REF       ! 'Force-Restore' sub-surface vertical 
!                                                    ! diffusion coefficient                   (-)
  REAL, POINTER, DIMENSION(:)    :: XP_ACOEF       ! 'Force-Restore' surface vertical 
!                                                    ! diffusion coefficient                   (-)
  REAL, POINTER, DIMENSION(:)    :: XP_PCOEF       ! 'Force-Restore' surface vertical 
!                                                    ! diffusion coefficient                   (-)
  REAL, POINTER, DIMENSION(:,:)  :: XP_WFC         ! field capacity volumetric water content
!                                                    ! profile                                 (m3/m3)
  REAL, POINTER, DIMENSION(:,:)  :: XP_WWILT       ! wilting point volumetric water content 
!                                                    ! profile                                 (m3/m3)
  REAL, POINTER, DIMENSION(:,:)  :: XP_WSAT        ! porosity profile                        (m3/m3) 
  REAL, POINTER, DIMENSION(:,:)  :: XP_BCOEF       ! soil water CH78 b-parameter             (-)
  REAL, POINTER, DIMENSION(:,:)  :: XP_CONDSAT     ! hydraulic conductivity at saturation    (m/s)
  REAL, POINTER, DIMENSION(:,:)  :: XP_MPOTSAT     ! matric potential at saturation          (m)
!
! - soil: Secondary parameters: thermal 
!
  REAL, POINTER, DIMENSION(:)    :: XP_CGSAT       ! soil thermal inertia coefficient at 
!                                                    ! saturation                              (K m2/J)
  REAL, POINTER, DIMENSION(:,:)  :: XP_HCAPSOIL    ! soil heat capacity                      (J/K/m3)
  REAL, POINTER, DIMENSION(:,:)  :: XP_CONDDRY     ! soil dry thermal conductivity           (W/m/K)
  REAL, POINTER, DIMENSION(:,:)  :: XP_CONDSLD     ! soil solids thermal conductivity        (W/m/K)
  REAL, POINTER, DIMENSION(:)    :: XP_TDEEP       ! prescribed deep soil temperature 
!                                                    ! (optional)                              (K)
! Prognostic variables:
!
! - Snow Cover:
!
  REAL,  POINTER, DIMENSION(:,:) :: XP_SNOWSWE     ! snow (& liq. water) content             (kg/m2)
  REAL,  POINTER, DIMENSION(:,:) :: XP_SNOWHEAT    ! heat content                            (J/m2)
  REAL,  POINTER, DIMENSION(:,:) :: XP_SNOWRHO     ! density                                 (kg m-3)
  REAL,  POINTER, DIMENSION(:,:) :: XP_SNOWGRAN1   ! grain parameter 1                       (-)
  REAL,  POINTER, DIMENSION(:,:) :: XP_SNOWGRAN2   ! grain parameter 2                       (-)
  REAL,  POINTER, DIMENSION(:,:) :: XP_SNOWHIST    ! historical parameter                    (-)
  REAL, POINTER, DIMENSION(:,:)  :: XP_SNOWAGE     ! Snow grain age                          (days)
  REAL,  POINTER, DIMENSION(:)   :: XP_SNOWALB     ! snow tot albedo                         (-)
  REAL,  POINTER, DIMENSION(:)   :: XP_SNOWALBVIS     ! snow VIS albedo                                  (-)
  REAL,  POINTER, DIMENSION(:)   :: XP_SNOWALBNIR     ! snow NIR albedo                                  (-)
  REAL,  POINTER, DIMENSION(:)   :: XP_SNOWALBFIR     ! snow FIR albedo                                  (-)
  REAL,  POINTER, DIMENSION(:)   :: XP_SNOWEMIS    ! snow emissivity (ISBA-ES:3-L)           (-)
!
  REAL,  POINTER, DIMENSION(:)   :: XP_ICE_STO
!
! - Soil and vegetation heat and water:
!
  REAL, POINTER, DIMENSION(:)     :: XP_WR         ! liquid water retained on the
!                                                    ! foliage of the vegetation
!                                                    ! canopy                                  (kg/m2)
  REAL, POINTER, DIMENSION(:,:)   :: XP_TG         ! surface and sub-surface soil 
!                                                    ! temperature profile                     (K)
  REAL, POINTER, DIMENSION(:,:)   :: XP_WG         ! soil volumetric water content profile   (m3/m3)
  REAL, POINTER, DIMENSION(:,:)   :: XP_WGI        ! soil liquid water equivalent volumetric 
!                                                    ! ice content profile                     (m3/m3)
  REAL, POINTER, DIMENSION(:)     :: XP_RESA       ! aerodynamic resistance                  (s/m)
! - For multi-energy balance:
!
  REAL, POINTER, DIMENSION(:)     :: XP_WRL        ! liquid water retained on the litter
!                                                  ! of the canopy vegetation                (kg/m2)
  REAL, POINTER, DIMENSION(:)     :: XP_WRLI       ! solid water retained on the litter
  REAL, POINTER, DIMENSION(:)     :: XP_WRVN       ! snow retained on the foliage
!                                                    ! of the canopy vegetation                (kg/m2)
  REAL, POINTER, DIMENSION(:)     :: XP_TV         ! canopy vegetation temperature           (K)
  REAL, POINTER, DIMENSION(:)     :: XP_TL         ! litter temperature           (K)
  REAL, POINTER, DIMENSION(:)     :: XP_TC         ! canopy air temperature                  (K)
  REAL, POINTER, DIMENSION(:)     :: XP_QC         ! canopy air specific humidity            (kg/kg)
!
  REAL, POINTER, DIMENSION(:)     :: XP_RGLV         ! canopy veg maximum solar radiation
  REAL, POINTER, DIMENSION(:)     :: XP_GAMMAV       ! coefficient for the calculation
!                                                      ! of the canopy veg surface stomatal
  REAL, POINTER, DIMENSION(:)     :: XP_RSMINV       ! canopy veg minimum stomatal resistance  (s/m)
  REAL, POINTER, DIMENSION(:,:)   :: XP_ROOTFRACV    ! canopy veg root fraction profile ('DIF' option)
  REAL, POINTER, DIMENSION(:)     :: XP_WRMAX_CFV    ! canopy veg coefficient for maximum water 
!                                                      ! interception
  REAL, POINTER, DIMENSION(:)     :: XP_LAIV         ! canopy veg Leaf Area Index              (m2/m2)
  REAL, POINTER, DIMENSION(:)     :: XP_Z0V          ! canopy veg roughness length             (m)
  REAL, POINTER, DIMENSION(:)     :: XP_H_VEG        ! vegetation height                       (m)
  REAL, POINTER, DIMENSION(:)     :: XP_GNDLITTER    ! ground litter cover                     (-)
  REAL, POINTER, DIMENSION(:)     :: XP_Z0LITTER     ! ground litter roughness length          (m)
!
! - Vegetation: Ags Prognos
!
  REAL, POINTER, DIMENSION(:)     :: XP_FWTD       ! grid-cell fraction of water table to rise
  REAL, POINTER, DIMENSION(:)     :: XP_WTD        ! water table depth                  (m)
!
! - Vegetation: Ags Prognostic (YPHOTO = 'LAI', 'LST', 'NIT', 'NCB') or prescribed (YPHOTO = 'NON', 'AGS', 'AST')
!
  REAL, POINTER, DIMENSION(:)     :: XP_LAI        ! Leaf Area Index                         (m2/m2)
!
! - Vegetation: Ags Prognostic (YPHOTO = 'AGS', 'LAI', 'AST', 'LST', 'NIT', 'NCB')
!
  REAL, POINTER, DIMENSION(:)     :: XP_AN         ! net CO2 assimilation                    (mg/m2/s)
  REAL, POINTER, DIMENSION(:)     :: XP_ANDAY      ! daily net CO2 assimilation              (mg/m2)
  REAL, POINTER, DIMENSION(:)     :: XP_ANFM       ! maximum leaf assimilation               (mg/m2/s)
  REAL, POINTER, DIMENSION(:)     :: XP_LE         ! evapotranspiration                      (W/m2)
  REAL, POINTER, DIMENSION(:)     :: XP_LEI        ! sublimation                             (W/m2)
  REAL, POINTER, DIMENSION(:)     :: XP_FAPARC     ! FAPAR of vegetation (cumul)
  REAL, POINTER, DIMENSION(:)     :: XP_FAPIRC     ! FAPIR of vegetation (cumul)
  REAL, POINTER, DIMENSION(:)     :: XP_LAI_EFFC   ! effective LAI (cumul)
  REAL, POINTER, DIMENSION(:)     :: XP_MUS        ! 
!
! - Vegetation: Ags Prognostic (YPHOTO = 'NIT', 'NCB')
!
  REAL, POINTER, DIMENSION(:,:)   :: XP_RESP_BIOMASS    ! daily cumulated respiration of 
!                                                         ! biomass                            (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:)   :: XP_BIOMASS         ! biomass of previous day            (kg/m2) 
  REAL, POINTER, DIMENSION(:,:)   :: XP_INCREASE        ! biomass increase                   (kg/m2/day)
!
! - Soil carbon (ISBA-CC, YRESPSL = 'CNT')
!
  REAL, POINTER, DIMENSION(:,:,:)   :: XP_LITTER            ! litter pools                       (gC/m2)
  REAL, POINTER, DIMENSION(:,:)     :: XP_SOILCARB          ! soil carbon pools                  (gC/m2) 
  REAL, POINTER, DIMENSION(:,:)     :: XP_LIGNIN_STRUC      ! ratio Lignin/Carbon in structural
!                                                         litter                               (gC/m2)
!
  REAL, POINTER, DIMENSION(:,:)   :: XP_TURNOVER        ! turnover rates from biomass to litter (gC/m2/s)
!
! - Irrigation
!
  LOGICAL, POINTER, DIMENSION(:)     :: XP_LIRRIGATE    ! high level switch for irrigation
  LOGICAL, POINTER, DIMENSION(:)     :: XP_LIRRIDAY     ! flag used for daily irrigation stage
  REAL, POINTER, DIMENSION(:)        :: XP_THRESHOLD    ! threshold for stages
  REAL, POINTER, DIMENSION(:)        :: XP_WATSUP       ! water supply
  REAL, POINTER, DIMENSION(:)        :: XP_IRRIG        ! fraction of irrigated vegetation
  TYPE(DATE_TIME), DIMENSION(:), POINTER :: TP_SEED         ! seeding date
  TYPE(DATE_TIME), DIMENSION(:), POINTER :: TP_REAP         ! reaping date
!                                                         ! previous day                         (kg/m2)
! - SGH scheme
!
  REAL, POINTER, DIMENSION(:)        :: XP_D_ICE     !depth of the soil column for the calculation
!                                                       of the frozen soil fraction (m)
  REAL, POINTER, DIMENSION(:)        :: XP_KSAT_ICE  !hydraulic conductivity at saturation
!                                                       over frozen area (m s-1)
  REAL, POINTER, DIMENSION(:)        :: XP_FSAT      !Topmodel saturated fraction
  REAL, POINTER, DIMENSION(:)        :: XP_MUF       !Rainfall surface fraction 
  REAL, POINTER, DIMENSION(:,:)      :: XP_TOPQS     !Topmodel baseflow by layer (m s-1)
!
! - Courant time step properties
!
  REAL, POINTER, DIMENSION(:)        :: XP_PSN       ! fraction of the grid covered by snow          (-)
  REAL, POINTER, DIMENSION(:)        :: XP_PSNG      ! fraction of the the bare ground covered by snow (-)
  REAL, POINTER, DIMENSION(:)        :: XP_PSNV      ! fraction of the the vegetation covered by snow(-)
  REAL, POINTER, DIMENSION(:)        :: XP_PSNV_A    ! fraction of the the vegetation covered by snow for EBA scheme(-)
  REAL, POINTER, DIMENSION(:,:)      :: XP_DIR_ALB_WITH_SNOW ! Total direct albedo
  REAL, POINTER, DIMENSION(:,:)      :: XP_SCA_ALB_WITH_SNOW ! Total diffuse albedo
!
! - Flood scheme
!
  REAL, POINTER, DIMENSION(:)        :: XP_ALBF
  REAL, POINTER, DIMENSION(:)        :: XP_EMISF
!
  REAL, POINTER, DIMENSION(:)        :: XP_FF        ! flood fraction over the surface
  REAL, POINTER, DIMENSION(:)        :: XP_FFG       ! flood fraction over the ground
  REAL, POINTER, DIMENSION(:)        :: XP_FFV       ! flood fraction over the vegetation
  REAL, POINTER, DIMENSION(:)        :: XP_FFROZEN   ! fraction of frozen flood
  REAL, POINTER, DIMENSION(:)        :: XP_FFLOOD  ! Grdi-cell flood fraction           (-)
  REAL, POINTER, DIMENSION(:)        :: XP_PIFLOOD ! Floodplains potential infiltration (kg/m2/s)
!
  REAL, POINTER, DIMENSION(:)        :: XP_CPS, XP_LVTT, XP_LSTT

END TYPE PACK_ISBA_t
!
!-------------------------------------------------------------------------------
!


 CONTAINS

!
!




SUBROUTINE PACK_ISBA_INIT(YPACK_ISBA)
TYPE(PACK_ISBA_t), INTENT(INOUT) :: YPACK_ISBA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_PACK_ISBA_N:PACK_ISBA_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YPACK_ISBA%LBLOCK_SIMPLE)
  NULLIFY(YPACK_ISBA%LBLOCK_0)
  NULLIFY(YPACK_ISBA%NBLOCK_SIMPLE)
  NULLIFY(YPACK_ISBA%NBLOCK_0)
  NULLIFY(YPACK_ISBA%TBLOCK_SIMPLE)
  NULLIFY(YPACK_ISBA%TBLOCK_0)
  NULLIFY(YPACK_ISBA%XBLOCK_SIMPLE)
  NULLIFY(YPACK_ISBA%XBLOCK_GROUND)
  NULLIFY(YPACK_ISBA%XBLOCK_VEGTYPE)
  NULLIFY(YPACK_ISBA%XBLOCK_TG)
  NULLIFY(YPACK_ISBA%XBLOCK_SNOW)
  NULLIFY(YPACK_ISBA%XBLOCK_ALB)
  NULLIFY(YPACK_ISBA%XBLOCK_2)
  NULLIFY(YPACK_ISBA%XBLOCK_BIOMASS)
  NULLIFY(YPACK_ISBA%XBLOCK_SOILCARB)
  NULLIFY(YPACK_ISBA%XBLOCK_LITTLEVS)
  NULLIFY(YPACK_ISBA%XBLOCK_LITTER)
  NULLIFY(YPACK_ISBA%XBLOCK_0)
  NULLIFY(YPACK_ISBA%XBLOCK_00)
  NULLIFY(YPACK_ISBA%XBLOCK_000)
  NULLIFY(YPACK_ISBA%XBLOCK_01)
  NULLIFY(YPACK_ISBA%XP_VEGTYPE_PATCH)
  NULLIFY(YPACK_ISBA%XP_SSO_SLOPE)
  NULLIFY(YPACK_ISBA%XP_LAT)
  NULLIFY(YPACK_ISBA%XP_LON)
  NULLIFY(YPACK_ISBA%XP_AOSIP)
  NULLIFY(YPACK_ISBA%XP_AOSIM)
  NULLIFY(YPACK_ISBA%XP_AOSJP)
  NULLIFY(YPACK_ISBA%XP_AOSJM)
  NULLIFY(YPACK_ISBA%XP_HO2IP)
  NULLIFY(YPACK_ISBA%XP_HO2IM)
  NULLIFY(YPACK_ISBA%XP_HO2JP)
  NULLIFY(YPACK_ISBA%XP_HO2JM)
  NULLIFY(YPACK_ISBA%XP_Z0EFFIP)
  NULLIFY(YPACK_ISBA%XP_Z0EFFIM)
  NULLIFY(YPACK_ISBA%XP_Z0EFFJP)
  NULLIFY(YPACK_ISBA%XP_Z0EFFJM)
  NULLIFY(YPACK_ISBA%XP_Z0REL)
  NULLIFY(YPACK_ISBA%XP_CLAY)
  NULLIFY(YPACK_ISBA%XP_SAND)
  NULLIFY(YPACK_ISBA%XP_ALBNIR_DRY)
  NULLIFY(YPACK_ISBA%XP_ALBVIS_DRY)
  NULLIFY(YPACK_ISBA%XP_ALBUV_DRY)
  NULLIFY(YPACK_ISBA%XP_ALBNIR_WET)
  NULLIFY(YPACK_ISBA%XP_ALBVIS_WET)
  NULLIFY(YPACK_ISBA%XP_ALBUV_WET)
  NULLIFY(YPACK_ISBA%XP_ALBNIR_SOIL)
  NULLIFY(YPACK_ISBA%XP_ALBVIS_SOIL)
  NULLIFY(YPACK_ISBA%XP_ALBUV_SOIL)
  NULLIFY(YPACK_ISBA%XP_Z0_O_Z0H)
  NULLIFY(YPACK_ISBA%XP_ALBNIR)
  NULLIFY(YPACK_ISBA%XP_ALBVIS)
  NULLIFY(YPACK_ISBA%XP_ALBUV)
  NULLIFY(YPACK_ISBA%XP_EMIS)
  NULLIFY(YPACK_ISBA%XP_Z0)
  NULLIFY(YPACK_ISBA%XP_ALBNIR_VEG)
  NULLIFY(YPACK_ISBA%XP_ALBVIS_VEG)
  NULLIFY(YPACK_ISBA%XP_ALBUV_VEG)
  NULLIFY(YPACK_ISBA%XP_VEG)
  NULLIFY(YPACK_ISBA%XP_WRMAX_CF)
  NULLIFY(YPACK_ISBA%XP_RSMIN)
  NULLIFY(YPACK_ISBA%XP_GAMMA)
  NULLIFY(YPACK_ISBA%XP_CV)
  NULLIFY(YPACK_ISBA%XP_RGL)
  NULLIFY(YPACK_ISBA%XP_ROOTFRAC)
  NULLIFY(YPACK_ISBA%XP_BSLAI)
  NULLIFY(YPACK_ISBA%XP_LAIMIN)
  NULLIFY(YPACK_ISBA%XP_SEFOLD)
  NULLIFY(YPACK_ISBA%XP_H_TREE)
  NULLIFY(YPACK_ISBA%XP_ANF)
  NULLIFY(YPACK_ISBA%XP_ANMAX)
  NULLIFY(YPACK_ISBA%XP_FZERO)
  NULLIFY(YPACK_ISBA%XP_EPSO)
  NULLIFY(YPACK_ISBA%XP_GAMM)
  NULLIFY(YPACK_ISBA%XP_QDGAMM)
  NULLIFY(YPACK_ISBA%XP_GMES)
  NULLIFY(YPACK_ISBA%XP_RE25)
  NULLIFY(YPACK_ISBA%XP_QDGMES)
  NULLIFY(YPACK_ISBA%XP_T1GMES)
  NULLIFY(YPACK_ISBA%XP_T2GMES)
  NULLIFY(YPACK_ISBA%XP_AMAX)
  NULLIFY(YPACK_ISBA%XP_QDAMAX)
  NULLIFY(YPACK_ISBA%XP_T1AMAX)
  NULLIFY(YPACK_ISBA%XP_T2AMAX)
  NULLIFY(YPACK_ISBA%LP_STRESS)
  NULLIFY(YPACK_ISBA%XP_F2I)
  NULLIFY(YPACK_ISBA%XP_GC)
  NULLIFY(YPACK_ISBA%XP_AH)
  NULLIFY(YPACK_ISBA%XP_BH)
  NULLIFY(YPACK_ISBA%XP_TAU_WOOD)
  NULLIFY(YPACK_ISBA%XP_DMAX)
  NULLIFY(YPACK_ISBA%XP_CE_NITRO)
  NULLIFY(YPACK_ISBA%XP_CF_NITRO)
  NULLIFY(YPACK_ISBA%XP_CNA_NITRO)
  NULLIFY(YPACK_ISBA%XP_BSLAI_NITRO)
  NULLIFY(YPACK_ISBA%XP_RUNOFFB)
  NULLIFY(YPACK_ISBA%XP_WDRAIN)
  NULLIFY(YPACK_ISBA%XP_TAUICE)
  NULLIFY(YPACK_ISBA%XP_GAMMAT)
  NULLIFY(YPACK_ISBA%XP_DG)
  NULLIFY(YPACK_ISBA%XP_DZG)
  NULLIFY(YPACK_ISBA%XP_DZDIF)
  NULLIFY(YPACK_ISBA%NK_WG_LAYER)
  NULLIFY(YPACK_ISBA%XP_RUNOFFD)
  NULLIFY(YPACK_ISBA%XP_SOILWGHT)
  NULLIFY(YPACK_ISBA%XP_C1SAT)
  NULLIFY(YPACK_ISBA%XP_C2REF)
  NULLIFY(YPACK_ISBA%XP_C3)
  NULLIFY(YPACK_ISBA%XP_C4B)
  NULLIFY(YPACK_ISBA%XP_C4REF)
  NULLIFY(YPACK_ISBA%XP_ACOEF)
  NULLIFY(YPACK_ISBA%XP_PCOEF)
  NULLIFY(YPACK_ISBA%XP_WFC)
  NULLIFY(YPACK_ISBA%XP_WWILT)
  NULLIFY(YPACK_ISBA%XP_WSAT)
  NULLIFY(YPACK_ISBA%XP_BCOEF)
  NULLIFY(YPACK_ISBA%XP_CONDSAT)
  NULLIFY(YPACK_ISBA%XP_MPOTSAT)
  NULLIFY(YPACK_ISBA%XP_CGSAT)
  NULLIFY(YPACK_ISBA%XP_HCAPSOIL)
  NULLIFY(YPACK_ISBA%XP_CONDDRY)
  NULLIFY(YPACK_ISBA%XP_CONDSLD)
  NULLIFY(YPACK_ISBA%XP_TDEEP)
  NULLIFY(YPACK_ISBA%XP_SNOWSWE)
  NULLIFY(YPACK_ISBA%XP_SNOWHEAT)
  NULLIFY(YPACK_ISBA%XP_SNOWRHO)
  NULLIFY(YPACK_ISBA%XP_SNOWGRAN1)
  NULLIFY(YPACK_ISBA%XP_SNOWGRAN2)
  NULLIFY(YPACK_ISBA%XP_SNOWHIST)
  NULLIFY(YPACK_ISBA%XP_SNOWAGE)
  NULLIFY(YPACK_ISBA%XP_SNOWALB)
  NULLIFY(YPACK_ISBA%XP_SNOWALBVIS)
  NULLIFY(YPACK_ISBA%XP_SNOWALBNIR)
  NULLIFY(YPACK_ISBA%XP_SNOWALBFIR)
  NULLIFY(YPACK_ISBA%XP_SNOWEMIS)
  NULLIFY(YPACK_ISBA%XP_ICE_STO)
  NULLIFY(YPACK_ISBA%XP_WR)
  NULLIFY(YPACK_ISBA%XP_TG)
  NULLIFY(YPACK_ISBA%XP_WG)
  NULLIFY(YPACK_ISBA%XP_WGI)
  NULLIFY(YPACK_ISBA%XP_RESA)
  NULLIFY(YPACK_ISBA%XP_WRL)
  NULLIFY(YPACK_ISBA%XP_WRLI)
  NULLIFY(YPACK_ISBA%XP_WRVN)
  NULLIFY(YPACK_ISBA%XP_TV)
  NULLIFY(YPACK_ISBA%XP_TL)
  NULLIFY(YPACK_ISBA%XP_TC)
  NULLIFY(YPACK_ISBA%XP_QC)
  NULLIFY(YPACK_ISBA%XP_RGLV)
  NULLIFY(YPACK_ISBA%XP_GAMMAV)
  NULLIFY(YPACK_ISBA%XP_RSMINV)
  NULLIFY(YPACK_ISBA%XP_ROOTFRACV)
  NULLIFY(YPACK_ISBA%XP_WRMAX_CFV)
  NULLIFY(YPACK_ISBA%XP_LAIV)
  NULLIFY(YPACK_ISBA%XP_Z0V)
  NULLIFY(YPACK_ISBA%XP_H_VEG)
  NULLIFY(YPACK_ISBA%XP_GNDLITTER)
  NULLIFY(YPACK_ISBA%XP_Z0LITTER)
  NULLIFY(YPACK_ISBA%XP_FWTD)
  NULLIFY(YPACK_ISBA%XP_WTD)
  NULLIFY(YPACK_ISBA%XP_LAI)
  NULLIFY(YPACK_ISBA%XP_AN)
  NULLIFY(YPACK_ISBA%XP_ANDAY)
  NULLIFY(YPACK_ISBA%XP_ANFM)
  NULLIFY(YPACK_ISBA%XP_LE)
  NULLIFY(YPACK_ISBA%XP_LEI)
  NULLIFY(YPACK_ISBA%XP_FAPARC)
  NULLIFY(YPACK_ISBA%XP_FAPIRC)
  NULLIFY(YPACK_ISBA%XP_LAI_EFFC)
  NULLIFY(YPACK_ISBA%XP_MUS)
  NULLIFY(YPACK_ISBA%XP_RESP_BIOMASS)
  NULLIFY(YPACK_ISBA%XP_BIOMASS)
  NULLIFY(YPACK_ISBA%XP_INCREASE)
  NULLIFY(YPACK_ISBA%XP_LITTER)
  NULLIFY(YPACK_ISBA%XP_SOILCARB)
  NULLIFY(YPACK_ISBA%XP_LIGNIN_STRUC)
  NULLIFY(YPACK_ISBA%XP_TURNOVER)
  NULLIFY(YPACK_ISBA%XP_LIRRIGATE)
  NULLIFY(YPACK_ISBA%XP_LIRRIDAY)
  NULLIFY(YPACK_ISBA%XP_THRESHOLD)
  NULLIFY(YPACK_ISBA%XP_WATSUP)
  NULLIFY(YPACK_ISBA%XP_IRRIG)
  NULLIFY(YPACK_ISBA%TP_SEED)
  NULLIFY(YPACK_ISBA%TP_REAP)
  NULLIFY(YPACK_ISBA%XP_D_ICE)
  NULLIFY(YPACK_ISBA%XP_KSAT_ICE)
  NULLIFY(YPACK_ISBA%XP_FSAT)
  NULLIFY(YPACK_ISBA%XP_MUF)
  NULLIFY(YPACK_ISBA%XP_TOPQS)
  NULLIFY(YPACK_ISBA%XP_PSN)
  NULLIFY(YPACK_ISBA%XP_PSNG)
  NULLIFY(YPACK_ISBA%XP_PSNV)
  NULLIFY(YPACK_ISBA%XP_PSNV_A)
  NULLIFY(YPACK_ISBA%XP_DIR_ALB_WITH_SNOW)
  NULLIFY(YPACK_ISBA%XP_SCA_ALB_WITH_SNOW)
  NULLIFY(YPACK_ISBA%XP_ALBF)
  NULLIFY(YPACK_ISBA%XP_EMISF)
  NULLIFY(YPACK_ISBA%XP_FF)
  NULLIFY(YPACK_ISBA%XP_FFG)
  NULLIFY(YPACK_ISBA%XP_FFV)
  NULLIFY(YPACK_ISBA%XP_FFROZEN)
  NULLIFY(YPACK_ISBA%XP_FFLOOD)
  NULLIFY(YPACK_ISBA%XP_PIFLOOD)
  NULLIFY(YPACK_ISBA%XP_CPS)
  NULLIFY(YPACK_ISBA%XP_LVTT)
  NULLIFY(YPACK_ISBA%XP_LSTT)
IF (LHOOK) CALL DR_HOOK("MODD_PACK_ISBA_N:PACK_ISBA_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE PACK_ISBA_INIT


END MODULE MODD_PACK_ISBA
