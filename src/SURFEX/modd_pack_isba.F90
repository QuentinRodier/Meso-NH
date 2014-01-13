!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
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
!
IMPLICIT NONE
!-------------------------------------------------------------------------------
!
INTEGER :: NSIZE_LSIMPLE
!$OMP THREADPRIVATE(NSIZE_LSIMPLE)
INTEGER :: NSIZE_L0
!$OMP THREADPRIVATE(NSIZE_L0)
INTEGER :: NSIZE_NSIMPLE
!$OMP THREADPRIVATE(NSIZE_NSIMPLE)
INTEGER :: NSIZE_N0
!$OMP THREADPRIVATE(NSIZE_N0)
INTEGER :: NSIZE_TSIMPLE
!$OMP THREADPRIVATE(NSIZE_TSIMPLE)
INTEGER :: NSIZE_T0
!$OMP THREADPRIVATE(NSIZE_T0)
INTEGER :: NSIZE_SIMPLE
!$OMP THREADPRIVATE(NSIZE_SIMPLE)
INTEGER :: NSIZE_GROUND
!$OMP THREADPRIVATE(NSIZE_GROUND)
INTEGER :: NSIZE_VEGTYPE
!$OMP THREADPRIVATE(NSIZE_VEGTYPE)
INTEGER :: NSIZE_TG
!$OMP THREADPRIVATE(NSIZE_TG)
INTEGER :: NSIZE_SNOW
!$OMP THREADPRIVATE(NSIZE_SNOW)
INTEGER :: NSIZE_ALB
!$OMP THREADPRIVATE(NSIZE_ALB)
INTEGER :: NSIZE_2
!$OMP THREADPRIVATE(NSIZE_2)
INTEGER :: NSIZE_BIOMASS
!$OMP THREADPRIVATE(NSIZE_BIOMASS)
INTEGER :: NSIZE_SOILCARB
!$OMP THREADPRIVATE(NSIZE_SOILCARB)
INTEGER :: NSIZE_LITTLEVS
!$OMP THREADPRIVATE(NSIZE_LITTLEVS)
INTEGER :: NSIZE_LITTER
!$OMP THREADPRIVATE(NSIZE_LITTER)
INTEGER :: NSIZE_0
!$OMP THREADPRIVATE(NSIZE_0)
INTEGER :: NSIZE_00
!$OMP THREADPRIVATE(NSIZE_00)
INTEGER :: NSIZE_000
!$OMP THREADPRIVATE(NSIZE_000)
INTEGER :: NSIZE_01
!$OMP THREADPRIVATE(NSIZE_01)
LOGICAL, ALLOCATABLE, DIMENSION(:,:), TARGET :: LBLOCK_SIMPLE 
!$OMP THREADPRIVATE(LBLOCK_SIMPLE)
LOGICAL, ALLOCATABLE, DIMENSION(:,:), TARGET :: LBLOCK_0
!$OMP THREADPRIVATE(LBLOCK_0)
INTEGER, ALLOCATABLE, DIMENSION(:,:), TARGET :: NBLOCK_SIMPLE 
!$OMP THREADPRIVATE(NBLOCK_SIMPLE)
INTEGER, ALLOCATABLE, DIMENSION(:,:), TARGET :: NBLOCK_0
!$OMP THREADPRIVATE(NBLOCK_0)
TYPE(DATE_TIME), ALLOCATABLE, DIMENSION(:,:), TARGET :: TBLOCK_SIMPLE
!$OMP THREADPRIVATE(TBLOCK_SIMPLE)
TYPE(DATE_TIME), ALLOCATABLE, DIMENSION(:,:), TARGET :: TBLOCK_0
!$OMP THREADPRIVATE(TBLOCK_0)
REAL, ALLOCATABLE, DIMENSION(:,:), TARGET :: XBLOCK_SIMPLE
!$OMP THREADPRIVATE(XBLOCK_SIMPLE)
REAL, ALLOCATABLE, DIMENSION(:,:,:),TARGET :: XBLOCK_GROUND
!$OMP THREADPRIVATE(XBLOCK_GROUND)
REAL, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: XBLOCK_VEGTYPE
!$OMP THREADPRIVATE(XBLOCK_VEGTYPE)
REAL, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: XBLOCK_TG
!$OMP THREADPRIVATE(XBLOCK_TG)
REAL, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: XBLOCK_SNOW
!$OMP THREADPRIVATE(XBLOCK_SNOW)
REAL, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: XBLOCK_ALB
!$OMP THREADPRIVATE(XBLOCK_ALB)
REAL, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: XBLOCK_2
!$OMP THREADPRIVATE(XBLOCK_2)
REAL, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: XBLOCK_BIOMASS
!$OMP THREADPRIVATE(XBLOCK_BIOMASS)
REAL, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: XBLOCK_SOILCARB
!$OMP THREADPRIVATE(XBLOCK_SOILCARB)
REAL, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: XBLOCK_LITTLEVS
!$OMP THREADPRIVATE(XBLOCK_LITTLEVS)
REAL, ALLOCATABLE, DIMENSION(:,:,:,:), TARGET :: XBLOCK_LITTER
!$OMP THREADPRIVATE(XBLOCK_LITTER)
REAL, ALLOCATABLE, DIMENSION(:,:), TARGET :: XBLOCK_0
!$OMP THREADPRIVATE(XBLOCK_0)
REAL, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: XBLOCK_00
!$OMP THREADPRIVATE(XBLOCK_00)
REAL, ALLOCATABLE, DIMENSION(:,:,:,:), TARGET :: XBLOCK_000
!$OMP THREADPRIVATE(XBLOCK_000)
REAL, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: XBLOCK_01
!$OMP THREADPRIVATE(XBLOCK_01)
!
! Mask and number of grid elements containing patches/tiles:
!
REAL, POINTER, DIMENSION(:,:)  :: XP_VEGTYPE_PATCH ! fraction of each vegetation type for
!$OMP THREADPRIVATE(XP_VEGTYPE_PATCH)
!                                                      ! each vegetation unit/patch              (-)
!
! General surface parameters:
!
REAL, POINTER, DIMENSION(:)    :: XP_SSO_SLOPE     ! orography slope within the grid mesh    (-)
!$OMP THREADPRIVATE(XP_SSO_SLOPE)
REAL, POINTER, DIMENSION(:)    :: XP_LAT           ! latitude    (-)
!$OMP THREADPRIVATE(XP_LAT)
REAL, POINTER, DIMENSION(:)    :: XP_LON           ! latitude    (-)
!$OMP THREADPRIVATE(XP_LON)
!
! Subgrid orography parameters
!
REAL, DIMENSION(:), POINTER :: XP_AOSIP,XP_AOSIM,XP_AOSJP,XP_AOSJM
!$OMP THREADPRIVATE(XP_AOSIP, XP_AOSIM, XP_AOSJP, XP_AOSJM)
! directional A/S quantities in 4 coordinate directions
! (IP: i index up;  IM: i index down;  JP: j index up;  JM: j index down)
! They are used in soil routines to compute effective roughness length
!
REAL, DIMENSION(:), POINTER :: XP_HO2IP,XP_HO2IM,XP_HO2JP,XP_HO2JM
!$OMP THREADPRIVATE(XP_HO2IP, XP_HO2IM, XP_HO2JP, XP_HO2JM)
! directional h/2 quantities in 4 coordinate directions
! (IP: i index up;  IM: i index down;  JP: j index up;  JM: j index down)
! They are used in soil routines to compute effective roughness length
!
REAL, DIMENSION(:), POINTER :: XP_Z0EFFIP,XP_Z0EFFIM,XP_Z0EFFJP,XP_Z0EFFJM
!$OMP THREADPRIVATE(XP_Z0EFFIP, XP_Z0EFFIM, XP_Z0EFFJP, XP_Z0EFFJM)
! directional total roughness lenghts in 4 coordinate directions
! (IP: i index up;  IM: i index down;  JP: j index up;  JM: j index down)
!
REAL, POINTER, DIMENSION(:) :: XP_Z0REL         ! relief roughness length                 (m)
!$OMP THREADPRIVATE(XP_Z0REL)

!
! Input Parameters:
!
! - bare soil:
!
REAL, POINTER, DIMENSION(:,:) :: XP_CLAY         ! clay fraction profile                   (-)
!$OMP THREADPRIVATE(XP_CLAY)
REAL, POINTER, DIMENSION(:,:) :: XP_SAND         ! sand fraction profile                   (-)
!$OMP THREADPRIVATE(XP_SAND)

REAL, POINTER, DIMENSION(:) :: XP_ALBNIR_DRY     ! near-infra-red albedo of dry soil       (-)
!$OMP THREADPRIVATE(XP_ALBNIR_DRY)
REAL, POINTER, DIMENSION(:) :: XP_ALBVIS_DRY     ! visible albedo of dry soil              (-)
!$OMP THREADPRIVATE(XP_ALBVIS_DRY)
REAL, POINTER, DIMENSION(:) :: XP_ALBUV_DRY      ! UV albedo of dry soil                   (-)
!$OMP THREADPRIVATE(XP_ALBUV_DRY)
REAL, POINTER, DIMENSION(:) :: XP_ALBNIR_WET     ! near-infra-red albedo of wet soil       (-)
!$OMP THREADPRIVATE(XP_ALBNIR_WET)
REAL, POINTER, DIMENSION(:) :: XP_ALBVIS_WET     ! visible albedo of wet soil              (-)
!$OMP THREADPRIVATE(XP_ALBVIS_WET)
REAL, POINTER, DIMENSION(:) :: XP_ALBUV_WET      ! UV albedo of wet soil                   (-)
!$OMP THREADPRIVATE(XP_ALBUV_WET)
REAL, POINTER, DIMENSION(:) :: XP_ALBNIR_SOIL    ! near-infra-red albedo of wet soil       (-)
!$OMP THREADPRIVATE(XP_ALBNIR_SOIL)
REAL, POINTER, DIMENSION(:) :: XP_ALBVIS_SOIL    ! visible albedo of soil                  (-)
!$OMP THREADPRIVATE(XP_ALBVIS_SOIL)
REAL, POINTER, DIMENSION(:) :: XP_ALBUV_SOIL     ! UV albedo of soil                       (-)
!$OMP THREADPRIVATE(XP_ALBUV_SOIL)
!
! - vegetation + bare soil:
!
REAL, POINTER, DIMENSION(:) :: XP_Z0_O_Z0H       ! ratio of surface roughness lengths
!$OMP THREADPRIVATE(XP_Z0_O_Z0H)
!                                                    ! (momentum to heat)                      (-)
REAL, POINTER, DIMENSION(:) :: XP_ALBNIR         ! near-infra-red albedo                   (-)
!$OMP THREADPRIVATE(XP_ALBNIR)
REAL, POINTER, DIMENSION(:) :: XP_ALBVIS         ! visible albedo                          (-)
!$OMP THREADPRIVATE(XP_ALBVIS)
REAL, POINTER, DIMENSION(:) :: XP_ALBUV          ! UV albedo                               (-)
!$OMP THREADPRIVATE(XP_ALBUV)
REAL, POINTER, DIMENSION(:) :: XP_EMIS           ! snow-free surface emissivity                      (-)
!$OMP THREADPRIVATE(XP_EMIS)
REAL, POINTER, DIMENSION(:) :: XP_Z0             ! snow-free surface roughness length                (m)
!$OMP THREADPRIVATE(XP_Z0)
!
! - vegetation :
!
REAL, POINTER, DIMENSION(:) :: XP_ALBNIR_VEG     ! near-infra-red albedo of vegetation     (-)
!$OMP THREADPRIVATE(XP_ALBNIR_VEG)
REAL, POINTER, DIMENSION(:) :: XP_ALBVIS_VEG     ! visible albedo of vegetation            (-)
!$OMP THREADPRIVATE(XP_ALBVIS_VEG)
REAL, POINTER, DIMENSION(:) :: XP_ALBUV_VEG      ! UV albedo of vegetation                 (-)
!$OMP THREADPRIVATE(XP_ALBUV_VEG)
!
!
! - vegetation: default option (Jarvis) and general parameters:
!
REAL, POINTER, DIMENSION(:) :: XP_VEG            ! vegetation cover fraction               (-)
!$OMP THREADPRIVATE(XP_VEG)
REAL, POINTER, DIMENSION(:) :: XP_WRMAX_CF       ! coefficient for maximum water 
!$OMP THREADPRIVATE(XP_WRMAX_CF)
!                                                    ! interception 
!                                                    ! storage capacity on the vegetation      (-)
REAL, POINTER, DIMENSION(:) :: XP_RSMIN          ! minimum stomatal resistance             (s/m)
!$OMP THREADPRIVATE(XP_RSMIN)
REAL, POINTER, DIMENSION(:) :: XP_GAMMA          ! coefficient for the calculation
!$OMP THREADPRIVATE(XP_GAMMA)
!                                                    ! of the surface stomatal
!                                                    ! resistance
REAL, POINTER, DIMENSION(:) :: XP_CV             ! vegetation thermal inertia coefficient  (K m2/J)
!$OMP THREADPRIVATE(XP_CV)
REAL, POINTER, DIMENSION(:) :: XP_RGL            ! maximum solar radiation
!$OMP THREADPRIVATE(XP_RGL)
!                                                    ! usable in photosynthesis                (W/m2)
REAL, POINTER, DIMENSION(:,:) :: XP_ROOTFRAC     ! root fraction profile ('DIF' option)
!$OMP THREADPRIVATE(XP_ROOTFRAC)
!
! - vegetation: Ags parameters ('AGS', 'LAI', 'AST', 'LST', 'NIT', 'NCB' options)
!
REAL, POINTER, DIMENSION(:)    :: XP_BSLAI      ! ratio d(biomass)/d(lai)                 (kg/m2)
!$OMP THREADPRIVATE(XP_BSLAI)
REAL, POINTER, DIMENSION(:)    :: XP_LAIMIN     ! minimum LAI (Leaf Area Index)           (m2/m2)
!$OMP THREADPRIVATE(XP_LAIMIN)
REAL, POINTER, DIMENSION(:)    :: XP_SEFOLD     ! e-folding time for senescence           (s)
!$OMP THREADPRIVATE(XP_SEFOLD)
REAL, POINTER, DIMENSION(:)    :: XP_H_TREE     ! height of trees                         (m)
!$OMP THREADPRIVATE(XP_H_TREE)
REAL, POINTER, DIMENSION(:)    :: XP_ANF        ! total assimilation over canopy          (
!$OMP THREADPRIVATE(XP_ANF)
REAL, POINTER, DIMENSION(:)    :: XP_ANMAX      ! maximum photosynthesis rate             (
!$OMP THREADPRIVATE(XP_ANMAX)
REAL, POINTER, DIMENSION(:)    :: XP_FZERO      ! ideal value of F, no photo- 
!$OMP THREADPRIVATE(XP_FZERO)
!                                                   ! respiration or saturation deficit       (
REAL, POINTER, DIMENSION(:)    :: XP_EPSO       ! maximum initial quantum use             
!$OMP THREADPRIVATE(XP_EPSO)
!                                                   ! efficiency                              (mg J-1 PAR)
REAL, POINTER, DIMENSION(:)    :: XP_GAMM       ! CO2 conpensation concentration          (ppmv)
!$OMP THREADPRIVATE(XP_GAMM)
REAL, POINTER, DIMENSION(:)    :: XP_QDGAMM     ! Log of Q10 function for CO2 conpensation 
!$OMP THREADPRIVATE(XP_QDGAMM)
!                                                   ! concentration                           (-)
REAL, POINTER, DIMENSION(:)    :: XP_GMES       ! mesophyll conductance                   (m s-1)
!$OMP THREADPRIVATE(XP_GMES)
REAL, POINTER, DIMENSION(:)    :: XP_RE25       ! Ecosystem respiration parameter         (kg m-2 s-1)
!$OMP THREADPRIVATE(XP_RE25)
REAL, POINTER, DIMENSION(:)    :: XP_QDGMES     ! Log of Q10 function for mesophyll conductance  (-)
!$OMP THREADPRIVATE(XP_QDGMES)
REAL, POINTER, DIMENSION(:)    :: XP_T1GMES     ! reference temperature for computing 
!$OMP THREADPRIVATE(XP_T1GMES)
!                                                   ! compensation concentration function for 
!                                                   ! mesophyll conductance: minimum
!                                                   ! temperature                             (K)
REAL, POINTER, DIMENSION(:)    :: XP_T2GMES     ! reference temperature for computing 
!$OMP THREADPRIVATE(XP_T2GMES)
!                                                   ! compensation concentration function for 
!                                                   ! mesophyll conductance: maximum
!                                                   ! temperature                             (K)
REAL, POINTER, DIMENSION(:)    :: XP_AMAX       ! leaf photosynthetic capacity            (kg m-2 s-1)
!$OMP THREADPRIVATE(XP_AMAX)
REAL, POINTER, DIMENSION(:)    :: XP_QDAMAX     ! Log of Q10 function for leaf photosynthetic 
!$OMP THREADPRIVATE(XP_QDAMAX)
!                                                   ! capacity                                (-)
REAL, POINTER, DIMENSION(:)    :: XP_T1AMAX     ! reference temperature for computing 
!$OMP THREADPRIVATE(XP_T1AMAX)
!                                                   ! compensation concentration function for 
!                                                   ! leaf photosynthetic capacity: minimum
!                                                   ! temperature                             (K)
REAL, POINTER, DIMENSION(:)    :: XP_T2AMAX     ! reference temperature for computing 
!$OMP THREADPRIVATE(XP_T2AMAX)
!                                                   ! compensation concentration function for 
!                                                   ! leaf photosynthetic capacity: maximum
!                                                   ! temperature                             (K)
!
! - vegetation: Ags Stress parameters ('AST', 'LST', 'NIT', 'NCB' options)
!
LOGICAL, POINTER, DIMENSION(:) :: LP_STRESS      ! vegetation response type to water
!$OMP THREADPRIVATE(LP_STRESS)
!                                                    ! stress (true:defensive false:offensive) (-)
REAL, POINTER, DIMENSION(:)    :: XP_F2I         ! critical normilized soil water 
!$OMP THREADPRIVATE(XP_F2I)
!                                                    ! content for stress parameterisation
REAL, POINTER, DIMENSION(:)    :: XP_GC          ! cuticular conductance                   (m s-1)
!$OMP THREADPRIVATE(XP_GC)
REAL, POINTER, DIMENSION(:)    :: XP_AH          ! coefficients for herbaceous water stress 
!$OMP THREADPRIVATE(XP_AH)
!                                                    ! response (offensive or defensive)       (log(mm/s))
REAL, POINTER, DIMENSION(:)    :: XP_BH          ! coefficients for herbaceous water stress 
!$OMP THREADPRIVATE(XP_BH)
!                                                    ! response (offensive or defensive)       (-)
REAL, POINTER, DIMENSION(:)    :: XP_TAU_WOOD    ! residence time in woody biomass         (s)
!$OMP THREADPRIVATE(XP_TAU_WOOD)
REAL, POINTER, DIMENSION(:)    :: XP_DMAX        ! maximum air saturation deficit
!$OMP THREADPRIVATE(XP_DMAX)
!                                                    ! tolerate by vegetation                  (kg/kg)
!
! - vegetation: Ags Nitrogen-model parameters ('NIT', 'NCB' option)
!
REAL, POINTER, DIMENSION(:)    :: XP_CE_NITRO      ! leaf aera ratio sensitivity to 
!$OMP THREADPRIVATE(XP_CE_NITRO)
!                                                      ! nitrogen concentration                (m2/kg)
REAL, POINTER, DIMENSION(:)    :: XP_CF_NITRO      ! lethal minimum value of leaf area
!$OMP THREADPRIVATE(XP_CF_NITRO)
!                                                      ! ratio                                 (m2/kg)
REAL, POINTER, DIMENSION(:)    :: XP_CNA_NITRO     ! nitrogen concentration of active 
!$OMP THREADPRIVATE(XP_CNA_NITRO)
!                                                      ! biomass                               (kg/kg)
REAL, POINTER, DIMENSION(:)    :: XP_BSLAI_NITRO! biomass/LAI ratio from nitrogen 
!$OMP THREADPRIVATE(XP_BSLAI_NITRO)
!                                                      ! decline theory                        (kg/m2)
!
! - soil: primary parameters
!
REAL, POINTER, DIMENSION(:)      :: XP_RUNOFFB     ! sub-grid surface runoff slope parameter (-)
!$OMP THREADPRIVATE(XP_RUNOFFB)
REAL, POINTER, DIMENSION(:)      :: XP_WDRAIN      ! continuous drainage parameter           (-)
!$OMP THREADPRIVATE(XP_WDRAIN)
REAL, POINTER, DIMENSION(:)      :: XP_TAUICE      ! soil freezing characteristic timescale  (s)
!$OMP THREADPRIVATE(XP_TAUICE)
REAL, POINTER, DIMENSION(:)      :: XP_GAMMAT      ! 'Force-Restore' timescale when using a
!$OMP THREADPRIVATE(XP_GAMMAT)
!                                                      ! prescribed lower boundary temperature   (1/days)
REAL, POINTER, DIMENSION(:,:)    :: XP_DG              ! soil layer depth                  (m)
!$OMP THREADPRIVATE(XP_DG)
!                                                      ! NOTE: in Force-Restore mode, the 
!                                                      ! uppermost layer thickness is superficial
!                                                      ! and is only explicitly used for soil 
!                                                      ! water phase changes                     (m)
REAL, POINTER, DIMENSION(:,:)    :: XP_DZG             ! soil layer thicknesses                  (m)
!$OMP THREADPRIVATE(XP_DZG)
REAL, POINTER, DIMENSION(:,:)    :: XP_DZDIF           ! distance between consecuative layer mid-points(m)
!$OMP THREADPRIVATE(XP_DZDIF)
INTEGER, POINTER, DIMENSION(:)   :: NK_WG_LAYER        ! Number of soil moisture layers for DIF
!$OMP THREADPRIVATE(NK_WG_LAYER)

REAL, POINTER, DIMENSION(:)      :: XP_RUNOFFD       ! depth over which sub-grid runoff is
!$OMP THREADPRIVATE(XP_RUNOFFD)
!                                                      ! computed: in Force-Restore this is the
!                                                      ! total soil column ('2-L'), or root zone
!                                                      ! ('3-L'). For the 'DIF' option, it can
!                                                      ! be any depth within soil column         (m)
!
REAL, POINTER, DIMENSION(:,:)  :: XP_SOILWGHT      ! ISBA-DIF: weights for vertical
!$OMP THREADPRIVATE(XP_SOILWGHT)
!                                                  ! integration of soil water and properties
!
! - soil: Secondary parameters: hydrology
!
REAL, POINTER, DIMENSION(:)    :: XP_C1SAT       ! 'Force-Restore' C1 coefficient at 
!$OMP THREADPRIVATE(XP_C1SAT)
!                                                    ! saturation                              (-)
REAL, POINTER, DIMENSION(:)    :: XP_C2REF       ! 'Force-Restore' reference value of C2   (-)
!$OMP THREADPRIVATE(XP_C2REF)
REAL, POINTER, DIMENSION(:,:)  :: XP_C3          ! 'Force-Restore' C3 drainage coefficient (m)
!$OMP THREADPRIVATE(XP_C3)
REAL, POINTER, DIMENSION(:)    :: XP_C4B         ! 'Force-Restore' sub-surface vertical 
!$OMP THREADPRIVATE(XP_C4B)
!                                                    ! diffusion coefficient (slope parameter) (-)
REAL, POINTER, DIMENSION(:)    :: XP_C4REF       ! 'Force-Restore' sub-surface vertical 
!$OMP THREADPRIVATE(XP_C4REF)
!                                                    ! diffusion coefficient                   (-)
REAL, POINTER, DIMENSION(:)    :: XP_ACOEF       ! 'Force-Restore' surface vertical 
!$OMP THREADPRIVATE(XP_ACOEF)
!                                                    ! diffusion coefficient                   (-)
REAL, POINTER, DIMENSION(:)    :: XP_PCOEF       ! 'Force-Restore' surface vertical 
!$OMP THREADPRIVATE(XP_PCOEF)
!                                                    ! diffusion coefficient                   (-)
REAL, POINTER, DIMENSION(:,:)  :: XP_WFC         ! field capacity volumetric water content
!$OMP THREADPRIVATE(XP_WFC)
!                                                    ! profile                                 (m3/m3)

REAL, POINTER, DIMENSION(:,:)  :: XP_WWILT       ! wilting point volumetric water content 
!$OMP THREADPRIVATE(XP_WWILT)
!                                                    ! profile                                 (m3/m3)
REAL, POINTER, DIMENSION(:,:)  :: XP_WSAT        ! porosity profile                        (m3/m3) 
!$OMP THREADPRIVATE(XP_WSAT)
REAL, POINTER, DIMENSION(:,:)  :: XP_BCOEF       ! soil water CH78 b-parameter             (-)
!$OMP THREADPRIVATE(XP_BCOEF)
REAL, POINTER, DIMENSION(:,:)  :: XP_CONDSAT     ! hydraulic conductivity at saturation    (m/s)
!$OMP THREADPRIVATE(XP_CONDSAT)
REAL, POINTER, DIMENSION(:,:)  :: XP_MPOTSAT     ! matric potential at saturation          (m)
!$OMP THREADPRIVATE(XP_MPOTSAT)
!
! - soil: Secondary parameters: thermal 
!
REAL, POINTER, DIMENSION(:)    :: XP_CGSAT       ! soil thermal inertia coefficient at 
!$OMP THREADPRIVATE(XP_CGSAT)
!                                                    ! saturation                              (K m2/J)
REAL, POINTER, DIMENSION(:,:)  :: XP_HCAPSOIL    ! soil heat capacity                      (J/K/m3)
!$OMP THREADPRIVATE(XP_HCAPSOIL)
REAL, POINTER, DIMENSION(:,:)  :: XP_CONDDRY     ! soil dry thermal conductivity           (W/m/K)
!$OMP THREADPRIVATE(XP_CONDDRY)
REAL, POINTER, DIMENSION(:,:)  :: XP_CONDSLD     ! soil solids thermal conductivity        (W/m/K)
!$OMP THREADPRIVATE(XP_CONDSLD)
REAL, POINTER, DIMENSION(:)    :: XP_TDEEP       ! prescribed deep soil temperature 
!$OMP THREADPRIVATE(XP_TDEEP)
!                                                    ! (optional)                              (K)
! Prognostic variables:
!
! - Snow Cover:
!
REAL,  POINTER, DIMENSION(:,:) :: XP_SNOWSWE     ! snow (& liq. water) content             (kg/m2)
!$OMP THREADPRIVATE(XP_SNOWSWE)
REAL,  POINTER, DIMENSION(:,:) :: XP_SNOWHEAT    ! heat content                            (J/m2)
!$OMP THREADPRIVATE(XP_SNOWHEAT)
REAL,  POINTER, DIMENSION(:,:) :: XP_SNOWRHO     ! density                                 (kg m-3)
!$OMP THREADPRIVATE(XP_SNOWRHO)
REAL,  POINTER, DIMENSION(:,:) :: XP_SNOWGRAN1   ! grain parameter 1                       (-)
!$OMP THREADPRIVATE(XP_SNOWGRAN1)
REAL,  POINTER, DIMENSION(:,:) :: XP_SNOWGRAN2   ! grain parameter 2                       (-)
!$OMP THREADPRIVATE(XP_SNOWGRAN2)
REAL,  POINTER, DIMENSION(:,:) :: XP_SNOWHIST    ! historical parameter                    (-)
!$OMP THREADPRIVATE(XP_SNOWHIST)
REAL, POINTER, DIMENSION(:,:)  :: XP_SNOWAGE     ! Snow grain age                          (days)
!$OMP THREADPRIVATE(XP_SNOWAGE)
REAL,  POINTER, DIMENSION(:)   :: XP_SNOWALB     ! albedo                                  (-)
!$OMP THREADPRIVATE(XP_SNOWALB)
REAL,  POINTER, DIMENSION(:)   :: XP_SNOWEMIS    ! snow emissivity (ISBA-ES:3-L)           (-)
!$OMP THREADPRIVATE(XP_SNOWEMIS)
!
REAL,  POINTER, DIMENSION(:)   :: XP_ICE_STO
!$OMP THREADPRIVATE(XP_ICE_STO)
!
! - Soil and vegetation heat and water:
!
REAL, POINTER, DIMENSION(:)     :: XP_WR         ! liquid water retained on the
!$OMP THREADPRIVATE(XP_WR)
!                                                    ! foliage of the vegetation
!                                                    ! canopy                                  (kg/m2)
REAL, POINTER, DIMENSION(:,:)   :: XP_TG         ! surface and sub-surface soil 
!$OMP THREADPRIVATE(XP_TG)
!                                                    ! temperature profile                     (K)
REAL, POINTER, DIMENSION(:,:)   :: XP_WG         ! soil volumetric water content profile   (m3/m3)
!$OMP THREADPRIVATE(XP_WG)
REAL, POINTER, DIMENSION(:,:)   :: XP_WGI        ! soil liquid water equivalent volumetric 
!$OMP THREADPRIVATE(XP_WGI)
!                                                    ! ice content profile                     (m3/m3)
REAL, POINTER, DIMENSION(:)     :: XP_RESA       ! aerodynamic resistance                  (s/m)
!$OMP THREADPRIVATE(XP_RESA)
!
! - Vegetation: Ags Prognostic (YPHOTO = 'LAI', 'LST', 'NIT', 'NCB') or prescribed (YPHOTO = 'NON', 'AGS', 'AST')
!
REAL, POINTER, DIMENSION(:)     :: XP_LAI        ! Leaf Area Index                         (m2/m2)
!$OMP THREADPRIVATE(XP_LAI)
!
! - Vegetation: Ags Prognostic (YPHOTO = 'AGS', 'LAI', 'AST', 'LST', 'NIT', 'NCB')
!
REAL, POINTER, DIMENSION(:)     :: XP_AN         ! net CO2 assimilation                    (mg/m2/s)
!$OMP THREADPRIVATE(XP_AN)
REAL, POINTER, DIMENSION(:)     :: XP_ANDAY      ! daily net CO2 assimilation              (mg/m2)
!$OMP THREADPRIVATE(XP_ANDAY)
REAL, POINTER, DIMENSION(:)     :: XP_ANFM       ! maximum leaf assimilation               (mg/m2/s)
!$OMP THREADPRIVATE(XP_ANFM)
REAL, POINTER, DIMENSION(:)     :: XP_LE         ! evapotranspiration                      (W/m2)
!$OMP THREADPRIVATE(XP_LE)
REAL, POINTER, DIMENSION(:)     :: XP_LEI        ! sublimation                             (W/m2)
!$OMP THREADPRIVATE(XP_LEI)
REAL, POINTER, DIMENSION(:)     :: XP_FAPARC     ! FAPAR of vegetation (cumul)
!$OMP THREADPRIVATE(XP_FAPARC)
REAL, POINTER, DIMENSION(:)     :: XP_FAPIRC     ! FAPIR of vegetation (cumul)
!$OMP THREADPRIVATE(XP_FAPIRC)
REAL, POINTER, DIMENSION(:)     :: XP_LAI_EFFC   ! effective LAI (cumul)
!$OMP THREADPRIVATE(XP_LAI_EFFC)
REAL, POINTER, DIMENSION(:)     :: XP_MUS        ! 
!$OMP THREADPRIVATE(XP_MUS)
!
! - Vegetation: Ags Prognostic (YPHOTO = 'NIT', 'NCB')
!
REAL, POINTER, DIMENSION(:,:)   :: XP_RESP_BIOMASS    ! daily cumulated respiration of 
!$OMP THREADPRIVATE(XP_RESP_BIOMASS)
!                                                         ! biomass                            (kg/m2/s)
REAL, POINTER, DIMENSION(:,:)   :: XP_BIOMASS         ! biomass of previous day            (kg/m2) 
!$OMP THREADPRIVATE(XP_BIOMASS)
REAL, POINTER, DIMENSION(:,:)   :: XP_INCREASE        ! biomass increase                   (kg/m2/day)
!$OMP THREADPRIVATE(XP_INCREASE)
!
! - Soil carbon (ISBA-CC, YRESPSL = 'CNT')
!
REAL, POINTER, DIMENSION(:,:,:)   :: XP_LITTER            ! litter pools                       (gC/m2)
!$OMP THREADPRIVATE(XP_LITTER)
REAL, POINTER, DIMENSION(:,:)     :: XP_SOILCARB          ! soil carbon pools                  (gC/m2) 
!$OMP THREADPRIVATE(XP_SOILCARB)
REAL, POINTER, DIMENSION(:,:)     :: XP_LIGNIN_STRUC      ! ratio Lignin/Carbon in structural
!$OMP THREADPRIVATE(XP_LIGNIN_STRUC)
!                                                         litter                               (gC/m2)
!
REAL, POINTER, DIMENSION(:,:)   :: XP_TURNOVER        ! turnover rates from biomass to litter (gC/m2/s)
!$OMP THREADPRIVATE(XP_TURNOVER)
!
! - Irrigation
!
LOGICAL, POINTER, DIMENSION(:)     :: XP_LIRRIGATE    ! high level switch for irrigation
!$OMP THREADPRIVATE(XP_LIRRIGATE)
LOGICAL, POINTER, DIMENSION(:)     :: XP_LIRRIDAY     ! flag used for daily irrigation stage
!$OMP THREADPRIVATE(XP_LIRRIDAY)
REAL, POINTER, DIMENSION(:)        :: XP_THRESHOLD    ! threshold for stages
!$OMP THREADPRIVATE(XP_THRESHOLD)
REAL, POINTER, DIMENSION(:)        :: XP_WATSUP       ! water supply
!$OMP THREADPRIVATE(XP_WATSUP)
REAL, POINTER, DIMENSION(:)        :: XP_IRRIG        ! fraction of irrigated vegetation
!$OMP THREADPRIVATE(XP_IRRIG)
TYPE(DATE_TIME), DIMENSION(:), POINTER :: TP_SEED         ! seeding date
!$OMP THREADPRIVATE(TP_SEED)
TYPE(DATE_TIME), DIMENSION(:), POINTER :: TP_REAP         ! reaping date
!$OMP THREADPRIVATE(TP_REAP)
!                                                         ! previous day                         (kg/m2)
! - SGH scheme
!
REAL, POINTER, DIMENSION(:)        :: XP_D_ICE     !depth of the soil column for the calculation
!$OMP THREADPRIVATE(XP_D_ICE)
!                                                       of the frozen soil fraction (m)
REAL, POINTER, DIMENSION(:)        :: XP_KSAT_ICE  !hydraulic conductivity at saturation
!$OMP THREADPRIVATE(XP_KSAT_ICE)
!                                                       over frozen area (m s-1)
REAL, POINTER, DIMENSION(:)        :: XP_FSAT      !Topmodel saturated fraction
!$OMP THREADPRIVATE(XP_FSAT)
REAL, POINTER, DIMENSION(:)        :: XP_MUF       !Rainfall surface fraction 
!$OMP THREADPRIVATE(XP_MUF)
!                                                         ! previous day                         (kg/m2)
! - Courant time step properties
!
REAL, POINTER, DIMENSION(:)        :: XP_PSN       ! fraction of the grid covered by snow          (-)
!$OMP THREADPRIVATE(XP_PSN)
REAL, POINTER, DIMENSION(:)        :: XP_PSNG      ! fraction of the the bare ground covered by snow (-)
!$OMP THREADPRIVATE(XP_PSNG)
REAL, POINTER, DIMENSION(:)        :: XP_PSNV      ! fraction of the the vegetation covered by snow(-)
!$OMP THREADPRIVATE(XP_PSNV)
REAL, POINTER, DIMENSION(:)        :: XP_PSNV_A    ! fraction of the the vegetation covered by snow for EBA scheme(-)
!$OMP THREADPRIVATE(XP_PSNV_A)
REAL, POINTER, DIMENSION(:,:)      :: XP_DIR_ALB_WITH_SNOW ! Total direct albedo
!$OMP THREADPRIVATE(XP_DIR_ALB_WITH_SNOW)
REAL, POINTER, DIMENSION(:,:)      :: XP_SCA_ALB_WITH_SNOW ! Total diffuse albedo
!$OMP THREADPRIVATE(XP_SCA_ALB_WITH_SNOW)
!
! - Flood scheme
!
REAL, POINTER, DIMENSION(:)        :: XP_ALBF
!$OMP THREADPRIVATE(XP_ALBF)
REAL, POINTER, DIMENSION(:)        :: XP_EMISF
!$OMP THREADPRIVATE(XP_EMISF)
!
REAL, POINTER, DIMENSION(:)        :: XP_FF        ! flood fraction over the surface
!$OMP THREADPRIVATE(XP_FF)
REAL, POINTER, DIMENSION(:)        :: XP_FFG       ! flood fraction over the ground
!$OMP THREADPRIVATE(XP_FFG)
REAL, POINTER, DIMENSION(:)        :: XP_FFV       ! flood fraction over the vegetation
!$OMP THREADPRIVATE(XP_FFV)
REAL, POINTER, DIMENSION(:)        :: XP_FFROZEN   ! fraction of frozen flood
!$OMP THREADPRIVATE(XP_FFROZEN)
REAL, POINTER, DIMENSION(:)        :: XP_FFLOOD  ! Grdi-cell flood fraction           (-)
!$OMP THREADPRIVATE(XP_FFLOOD)
REAL, POINTER, DIMENSION(:)        :: XP_PIFLOOD ! Floodplains potential infiltration (kg/m2/s)
!$OMP THREADPRIVATE(XP_PIFLOOD)
REAL, POINTER, DIMENSION(:)        :: XP_Z0FLOOD ! Floodplains roughness length
!$OMP THREADPRIVATE(XP_Z0FLOOD)
!
REAL, POINTER, DIMENSION(:)        :: XP_CPS, XP_LVTT, XP_LSTT
!$OMP THREADPRIVATE(XP_CPS, XP_LVTT, XP_LSTT)
!
END MODULE MODD_PACK_ISBA
