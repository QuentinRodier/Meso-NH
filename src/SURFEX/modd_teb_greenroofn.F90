!##################
MODULE MODD_TEB_GREENROOF_n
!##################
!
!!****  *MODD_TEB_GREENROOF - declaration of ISBA scheme packed surface parameters for urban green roofs
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
!!	A. Lemonsu *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       09/2009
!!      C. de Munck     06/2011 
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TYPE_SNOW
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE




TYPE TEB_GREENROOF_OPTIONS_t
!-------------------------------------------------------------------------------
!
! type of initialization : from cover types (ecoclimap) or parameters prescribed
!
  LOGICAL                         :: LPAR_GREENROOF ! T: parameters computed from ecoclimap
!                                                   ! F: they are read in the file
!
! ISBA Scheme Options specific to urban green roofs:
!
  CHARACTER(LEN=3)                :: CISBA_GR       ! type of ISBA version ('2-L' = default, '3-L', 'DIF')
  CHARACTER(LEN=4)                :: CSCOND_GR      ! Thermal conductivity ('DEF '= NP89 implicit method , 
                                                    ! 'PL98' = Peters-Lidard et al. 1998 used for explicit computation of CG)
!
  LOGICAL                          :: LTR_ML_GR
!-------------------------------------------------------------------------------
!
! type of initialization of vegetation: from cover types (ecoclimap) or parameters prescribed
!
  INTEGER                         :: NLAYER_GR       ! number of ground layers
  INTEGER                         :: NTIME_GR        ! number of time data : for VEG, LAI, EMIS, Z0
!
  INTEGER                              :: NLAYER_HORT_GR
  INTEGER                              :: NLAYER_DUN_GR
!
  REAL, POINTER, DIMENSION(:)          :: XSOILGRID_GR        ! Soil layer grid as reference for DIF
!-------------------------------------------------------------------------------
!
! - SGH scheme
!                                                     
  CHARACTER(LEN=4)                :: CRUNOFF_GR      ! surface runoff formulation for green roofs
!                                                    ! 'WSAT'
!                                                    ! 'DT92'
!                                                    ! 'SGH ' Topmodel
  CHARACTER(LEN=3)                :: CTOPREG_GR      ! Wolock and McCabe (2000) linear regression for Topmodel
!                                                    ! 'DEF' = Reg
!                                                    ! 'NON' = no Reg  
  CHARACTER(LEN=3)                :: CKSAT_GR        ! ksat
!                                                    ! 'DEF' = default value 
!                                                    ! 'SGH' = profil exponentiel
  CHARACTER(LEN=3)                :: CHORT_GR        ! Horton runoff
!                                                    ! 'DEF' = no Horton runoff
!                                                    ! 'SGH' = Horton runoff
  CHARACTER(LEN=3)               :: CSOC_GR          ! soil organic carbon effect
!                                                   ! 'DEF' = default value 
!                                                   ! 'SGH' = SOC profil
!
!-------------------------------------------------------------------------------
!                                 
! Type of green roof (characterization of green roof structure based on GR vegetation)
!
  CHARACTER(LEN=5)                :: CTYP_GR         ! type of green roof
!
!-------------------------------------------------------------------------------
!
END TYPE TEB_GREENROOF_OPTIONS_t




TYPE TEB_GREENROOF_PGD_t
!-------------------------------------------------------------------------------
!
! Mask and number of grid elements containing patches/tiles:
!
  REAL, POINTER, DIMENSION(:,:) :: XVEGTYPE          ! fraction of each vegetation type for
!                                                    ! each grid mesh                          (-)
!
!-------------------------------------------------------------------------------
!
! Averaged Surface radiative parameters:
!
  REAL, POINTER, DIMENSION(:)   :: XALBNIR_DRY       ! dry soil near-infra-red albedo          (-)
  REAL, POINTER, DIMENSION(:)   :: XALBVIS_DRY       ! dry soil visible albedo                 (-)
  REAL, POINTER, DIMENSION(:)   :: XALBUV_DRY        ! dry soil UV albedo                      (-)
  REAL, POINTER, DIMENSION(:)   :: XALBNIR_WET       ! wet soil near-infra-red albedo          (-)
  REAL, POINTER, DIMENSION(:)   :: XALBVIS_WET       ! wet soil visible albedo                 (-)
  REAL, POINTER, DIMENSION(:)   :: XALBUV_WET        ! wet soil UV albedo                      (-)
  REAL, POINTER, DIMENSION(:)   :: XALBNIR_SOIL      ! soil near-infra-red albedo              (-)
  REAL, POINTER, DIMENSION(:)   :: XALBVIS_SOIL      ! soil visible albedo                     (-)
  REAL, POINTER, DIMENSION(:)   :: XALBUV_SOIL       ! soil UV albedo                          (-)
  REAL, POINTER, DIMENSION(:)   :: XALBNIR_TSOIL     ! total near-infra-red albedo of wet soil (-)
  REAL, POINTER, DIMENSION(:)   :: XALBVIS_TSOIL     ! total visible albedo of soil            (-)  
!
!-------------------------------------------------------------------------------
!
! Input Parameters, per patch:
!
! - vegetation + bare soil:
!
  REAL, POINTER, DIMENSION(:)   :: XZ0_O_Z0H         ! ratio of surface roughness lengths
!                                                    ! (momentum to heat)                      (-)

!
! - vegetation:
!
  REAL, POINTER, DIMENSION(:)   :: XALBNIR_VEG       ! vegetation near-infra-red albedo        (-)
  REAL, POINTER, DIMENSION(:)   :: XALBVIS_VEG       ! vegetation visible albedo               (-)
  REAL, POINTER, DIMENSION(:)   :: XALBUV_VEG        ! vegetation UV albedo                    (-)
  REAL, POINTER, DIMENSION(:)   :: XALBNIR_TVEG      ! total near-infra-red albedo of vegetation (-)
  REAL, POINTER, DIMENSION(:)   :: XALBVIS_TVEG      ! total visible albedo of vegetation        (-)    
!
! - vegetation: default option (Jarvis) and general parameters:
!
  REAL, POINTER, DIMENSION(:)   :: XWRMAX_CF         ! coefficient for maximum water 
                                                     ! interception 
                                                     ! storage capacity on the vegetation      (-)
  REAL, POINTER, DIMENSION(:)   :: XRSMIN            ! minimum stomatal resistance             (s/m)
  REAL, POINTER, DIMENSION(:)   :: XGAMMA            ! coefficient for the calculation
                                                     ! of the surface stomatal
                                                     ! resistance
  REAL, POINTER, DIMENSION(:)   :: XCV               ! vegetation thermal inertia coefficient  (K m2/J)
  REAL, POINTER, DIMENSION(:)   :: XRGL              ! maximum solar radiation
                                                     ! usable in photosynthesis                (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XROOTFRAC         ! root fraction profile ('DIF' option)
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags parameters ('AGS', 'LAI', 'AST', 'LST', 'NIT', 'NCB' options)
!
  REAL, POINTER, DIMENSION(:)   :: XABC              ! abscissa needed for integration
                                                     ! of net assimilation and stomatal
                                                     ! conductance over canopy depth           (-)
  REAL, POINTER, DIMENSION(:)   :: XPOI              ! Gaussian weights for integration
                                                     ! of net assimilation and stomatal
                                                     ! conductance over canopy depth           (-)
  REAL, POINTER, DIMENSION(:)   :: XBSLAI            ! ratio d(biomass)/d(lai)                 (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XLAIMIN           ! minimum LAI (Leaf Area Index)           (m2/m2)
  REAL, POINTER, DIMENSION(:)   :: XSEFOLD           ! e-folding time for senescence           (s)
  REAL, POINTER, DIMENSION(:)   :: XH_TREE           ! height of trees                         (m)
  REAL, POINTER, DIMENSION(:)   :: XANF              ! total assimilation over canopy          (
  REAL, POINTER, DIMENSION(:)   :: XANMAX            ! maximum photosynthesis rate             (
  REAL, POINTER, DIMENSION(:)   :: XFZERO            ! ideal value of F, no photo- 
                                                     ! respiration or saturation deficit       (
  REAL, POINTER, DIMENSION(:)   :: XEPSO             ! maximum initial quantum use             
                                                     ! efficiency                              (mg J-1 PAR)
  REAL, POINTER, DIMENSION(:)   :: XGAMM             ! CO2 conpensation concentration          (ppm)
  REAL, POINTER, DIMENSION(:)   :: XQDGAMM           ! Q10 function for CO2 conpensation 
                                                     ! concentration                           (-)
  REAL, POINTER, DIMENSION(:)   :: XGMES             ! mesophyll conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:)   :: XRE25             ! Ecosystem respiration parameter         (kg/kg.m.s-1)
  REAL, POINTER, DIMENSION(:)   :: XQDGMES           ! Q10 function for mesophyll conductance  (-)
  REAL, POINTER, DIMENSION(:)   :: XT1GMES           ! reference temperature for computing 
                                                     ! compensation concentration function for 
                                                     ! mesophyll conductance: minimum
                                                     ! temperature                             (K)
  REAL, POINTER, DIMENSION(:)   :: XT2GMES           ! reference temperature for computing 
                                                     ! compensation concentration function for 
                                                     ! mesophyll conductance: maximum
                                                     ! temperature                             (K)
  REAL, POINTER, DIMENSION(:)   :: XAMAX             ! leaf photosynthetic capacity            (mg m-2 s-1)
  REAL, POINTER, DIMENSION(:)   :: XQDAMAX           ! Q10 function for leaf photosynthetic 
                                                     ! capacity                                (-)
  REAL, POINTER, DIMENSION(:)   :: XT1AMAX           ! reference temperature for computing 
                                                     ! compensation concentration function for 
                                                     ! leaf photosynthetic capacity: minimum
                                                     ! temperature                             (K)
  REAL, POINTER, DIMENSION(:)   :: XT2AMAX           ! reference temperature for computing 
                                                     ! compensation concentration function for 
                                                     ! leaf photosynthetic capacity: maximum
                                                     ! temperature                             (K)
!                                      
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Stress parameters ('AST', 'LST', 'NIT', 'NCB' options)
!
  LOGICAL, POINTER, DIMENSION(:):: LSTRESS           ! vegetation response (offensive/defensive)
  REAL, POINTER, DIMENSION(:)   :: XF2I              ! critical normilized soil water 
                                                     ! content for stress parameterisation
  REAL, POINTER, DIMENSION(:)   :: XGC               ! cuticular conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:)   :: XAH               ! coefficients for herbaceous water stress 
                                                     ! response (offensive or defensive)       (log(mm/s))
  REAL, POINTER, DIMENSION(:)   :: XBH               ! coefficients for herbaceous water stress 
                                                     ! response (offensive or defensive)       (-)
  REAL, POINTER, DIMENSION(:)   :: XDMAX             ! maximum air saturation deficit
                                                     ! tolerate by vegetation                  (kg/kg)
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Nitrogen-model parameters ('NIT', 'NCB' option)
!
  REAL, POINTER, DIMENSION(:)    :: XCE_NITRO        ! leaf aera ratio sensitivity to 
                                                     ! nitrogen concentration                (m2/kg)
  REAL, POINTER, DIMENSION(:)    :: XCF_NITRO        ! lethal minimum value of leaf area
                                                     ! ratio                                 (m2/kg)
  REAL, POINTER, DIMENSION(:)    :: XCNA_NITRO       ! nitrogen concentration of active 
                                                     ! biomass                               (kg/kg)
  REAL, POINTER, DIMENSION(:)    :: XBSLAI_NITRO     ! biomass/LAI ratio from nitrogen 
                                                     ! decline theory                        (kg/m2)
!
!-------------------------------------------------------------------------------
!
! - soil: primary parameters
!
  REAL, POINTER, DIMENSION(:,:)  :: XOM_GR           ! green roof OM fraction (-)
  REAL, POINTER, DIMENSION(:,:)  :: XSAND_GR         ! green roof sand fraction of the non-OM part (-)
  REAL, POINTER, DIMENSION(:,:)  :: XCLAY_GR         ! green roof clay fraction of the non-OM part (-)
  REAL, POINTER, DIMENSION(:)    :: XRUNOFFB_GR      ! green roof sub-grid surface runoff slope parameter (-)
  REAL, POINTER, DIMENSION(:)    :: XWDRAIN_GR       ! green roof continuous drainage parameter           (-)
  REAL, POINTER, DIMENSION(:)    :: XTAUICE          ! soil freezing characteristic timescale  (s)
  REAL, POINTER, DIMENSION(:)    :: XGAMMAT          ! 'Force-Restore' timescale when using a
                                                     ! prescribed lower boundary temperature   (1/days)
  REAL, POINTER, DIMENSION(:,:)  :: XDG              ! soil layer thicknesses                  (m)
                                                     ! NOTE: in Force-Restore mode, the 
                                                     ! uppermost layer thickness is superficial
                                                     ! and is only explicitly used for soil 
                                                     ! water phase changes                     (m)
  REAL, POINTER, DIMENSION(:)    :: XRUNOFFD         ! depth over which sub-grid runoff is
                                                     ! computed: in Force-Restore this is the
                                                     ! total soil column ('2-L'), or root zone
                                                     ! ('3-L'). For the 'DIF' option, it can
                                                     ! be any depth within soil column         (m)
!
  REAL, POINTER, DIMENSION(:,:)  :: XSOILWGHT      ! ISBA-DIF: weights for vertical
  REAL, POINTER, DIMENSION(:,:)  :: XDZG           ! soil layers thicknesses (DIF option)
  REAL, POINTER, DIMENSION(:,:)  :: XDZDIF         ! distance between consecuative layer mid-points (DIF option)
!
  INTEGER, POINTER, DIMENSION(:) :: NWG_LAYER      ! Number of soil moisture layers for DIF
  REAL, POINTER, DIMENSION(:)    :: XDROOT         ! effective root depth for DIF (m)
  REAL, POINTER, DIMENSION(:)    :: XDG2           ! root depth for DIF as 3-L (m)
!-------------------------------------------------------------------------------
!
! - soil: Secondary parameters: hydrology
!
  REAL, POINTER, DIMENSION(:)    :: XC1SAT           ! 'Force-Restore' C1 coefficient at 
                                                     ! saturation                              (-)
  REAL, POINTER, DIMENSION(:)    :: XC2REF           ! 'Force-Restore' reference value of C2   (-)
  REAL, POINTER, DIMENSION(:,:)  :: XC3              ! 'Force-Restore' C3 drainage coefficient (m)
  REAL, POINTER, DIMENSION(:)    :: XC4B             ! 'Force-Restore' sub-surface vertical 
                                                     ! diffusion coefficient (slope parameter) (-)
  REAL, POINTER, DIMENSION(:)    :: XC4REF           ! 'Force-Restore' sub-surface vertical 
                                                     ! diffusion coefficient                   (-)
  REAL, POINTER, DIMENSION(:)    :: XACOEF           ! 'Force-Restore' surface vertical 
                                                     ! diffusion coefficient                   (-)
  REAL, POINTER, DIMENSION(:)    :: XPCOEF           ! 'Force-Restore' surface vertical 
                                                     ! diffusion coefficient                   (-)
  REAL, POINTER, DIMENSION(:,:)  :: XWFC             ! field capacity volumetric water content
                                                     ! profile                             (m3/m3)
  REAL, POINTER, DIMENSION(:,:)  :: XWWILT           ! wilting point volumetric water content 
                                                     ! profile         
  REAL, POINTER, DIMENSION(:,:)  :: XWSAT            ! porosity profile                      (m3/m3) 
  REAL, POINTER, DIMENSION(:,:)  :: XBCOEF           ! soil water CH78 b-parameter             (-)
  REAL, POINTER, DIMENSION(:,:)  :: XCONDSAT         ! hydraulic conductivity at saturation    (m/s)
  REAL, POINTER, DIMENSION(:,:)  :: XMPOTSAT         ! matric potential at saturation          (m)
!
  REAL, POINTER, DIMENSION(:)    :: XPCPS
  REAL, POINTER, DIMENSION(:)    :: XPLVTT
  REAL, POINTER, DIMENSION(:)    :: XPLSTT 
!
!-------------------------------------------------------------------------------
!
! - soil: Secondary parameters: thermal 
!
  REAL, POINTER, DIMENSION(:)    :: XCGSAT           ! soil thermal inertia coefficient at 
                                                     ! saturation                              (K m2/J)
  REAL, POINTER, DIMENSION(:,:)  :: XHCAPSOIL        ! soil heat capacity                      (J/K/m3)
  REAL, POINTER, DIMENSION(:,:)  :: XCONDDRY         ! soil dry thermal conductivity           (W/m/K)
  REAL, POINTER, DIMENSION(:,:)  :: XCONDSLD         ! soil solids thermal conductivity        (W/m/K)
  REAL, POINTER, DIMENSION(:)    :: XTDEEP           ! prescribed deep soil temperature 
                                                     ! (optional)                              (K)
!
! - SGH scheme
!
  REAL, POINTER, DIMENSION(:)    :: XD_ICE          ! depth of the soil column for the calculation
                                                    ! of the frozen soil fraction (m)
!-------------------------------------------------------------------------------
!
END TYPE TEB_GREENROOF_PGD_t


TYPE TEB_GREENROOF_PGD_EVOL_t
! - Vegetation: Ags Prognostic (YPHOTO = ('LAI', 'LST', or 'NIT') or prescribed (YPHOTO='NON', 'AGS' or 'LST')
!
  REAL, POINTER, DIMENSION(:)   :: XLAI              ! Leaf Area Index                         (m2/m2)
  REAL, POINTER, DIMENSION(:)   :: XVEG              ! vegetation cover fraction               (-)
  REAL, POINTER, DIMENSION(:)   :: XALBNIR           ! near-infra-red albedo                   (-)
  REAL, POINTER, DIMENSION(:)   :: XALBVIS           ! visible albedo                          (-)
  REAL, POINTER, DIMENSION(:)   :: XALBUV            ! UV albedo                               (-)
  REAL, POINTER, DIMENSION(:)   :: XEMIS             ! surface emissivity                      (-)
  REAL, POINTER, DIMENSION(:)   :: XZ0               ! surface roughness length                (m)
!
END TYPE TEB_GREENROOF_PGD_EVOL_t


TYPE TEB_GREENROOF_t
!-------------------------------------------------------------------------------
!
! Prognostic variables:
!
! - Snow Cover:
!
  TYPE(SURF_SNOW)                :: TSNOW            ! snow state: 
                                                     ! scheme type/option                      (-)
                                                     ! number of layers                        (-)
                                                     ! snow (& liq. water) content             (kg/m2)
                                                     ! heat content                            (J/m2)
                                                     ! temperature                             (K)
                                                     ! density                                 (kg m-3)
!
! - Soil and vegetation heat and water:
!
  REAL, POINTER, DIMENSION(:)    :: XWR              ! liquid water retained on the
                                                     ! foliage of the vegetation
                                                     ! canopy                                  (kg/m2)
  REAL, POINTER, DIMENSION(:,:)  :: XTG              ! surface and sub-surface soil 
                                                     ! temperature profile                     (K)
  REAL, POINTER, DIMENSION(:,:)  :: XWG              ! soil volumetric water content profile   (m3/m3)
  REAL, POINTER, DIMENSION(:,:)  :: XWGI             ! soil liquid water equivalent volumetric 
                                                     ! ice content profile                     (m3/m3)
  REAL, POINTER, DIMENSION(:)    :: XRESA            ! aerodynamic resistance                  (s/m)
!

! - Vegetation: Ags Prognostic (YPHOTO = 'AGS', 'LAI', 'AST', 'LST', 'NIT', 'NCB')
!
  REAL, POINTER, DIMENSION(:)    :: XAN              ! net CO2 assimilation                    (mg/m2/s)
  REAL, POINTER, DIMENSION(:)    :: XANDAY           ! daily net CO2 assimilation              (mg/m2)
  REAL, POINTER, DIMENSION(:)    :: XANFM            ! maximum leaf assimilation               (mg/m2/s)
  REAL, POINTER, DIMENSION(:)    :: XLE              ! evapotranspiration                      (W/m2)
  REAL, POINTER, DIMENSION(:)     :: XFAPARC       ! Fapar of vegetation (cumul)
  REAL, POINTER, DIMENSION(:)     :: XFAPIRC       ! Fapir of vegetation (cumul)
  REAL, POINTER, DIMENSION(:)     :: XLAI_EFFC     ! Effective LAI (cumul)
  REAL, POINTER, DIMENSION(:)     :: XMUS          ! cos zenithal angle (cumul)    
!
! - Vegetation: Ags Prognostic (YPHOTO = 'NIT', 'NCB')
!
  REAL, POINTER, DIMENSION(:,:)  :: XRESP_BIOMASS    ! daily cumulated respiration of 
                                                     ! biomass                              (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:)  :: XBIOMASS         ! biomass of previous day              (kg/m2) 
!
! - SGH scheme
!                                                     
  REAL, POINTER, DIMENSION(:)    :: XKSAT_ICE       ! hydraulic conductivity at saturation
                                                    ! over frozen area (m s-1)                                     
!-------------------------------------------------------------------------------
!
! - Snow and flood fractions and total albedo at time t:
!
  REAL, POINTER, DIMENSION(:)    :: XPSNG              ! Snow fraction over ground
  REAL, POINTER, DIMENSION(:)    :: XPSNV              ! Snow fraction over vegetation
  REAL, POINTER, DIMENSION(:)    :: XPSNV_A            ! Snow fraction over vegetation
  REAL, POINTER, DIMENSION(:)    :: XPSN               ! Total Snow fraction
! 
  REAL, POINTER, DIMENSION(:)    :: XSNOWFREE_ALB      ! snow free albedo                        (-)
  REAL, POINTER, DIMENSION(:)    :: XSNOWFREE_ALB_VEG  ! snow free albedo for vegetation         (-)
  REAL, POINTER, DIMENSION(:)    :: XSNOWFREE_ALB_SOIL ! snow free albedo for soil               (-)
!
!-------------------------------------------------------------------------------
!
END TYPE TEB_GREENROOF_t



TYPE(TEB_GREENROOF_OPTIONS_t), ALLOCATABLE, TARGET, SAVE :: TEB_GREENROOF_OPTIONS_MODEL(:)
TYPE(TEB_GREENROOF_PGD_t),     ALLOCATABLE, TARGET, SAVE :: TEB_GREENROOF_PGD_MODEL(:)
TYPE(TEB_GREENROOF_PGD_EVOL_t),ALLOCATABLE, TARGET, SAVE :: TEB_GREENROOF_PGD_EVOL_MODEL(:,:)
TYPE(TEB_GREENROOF_t),         ALLOCATABLE, TARGET, SAVE :: TEB_GREENROOF_MODEL(:,:)



LOGICAL, POINTER                 :: LPAR_GREENROOF=>NULL()
!$OMP THREADPRIVATE(LPAR_GREENROOF)
 CHARACTER(LEN=3), POINTER        :: CISBA_GR=>NULL()
!$OMP THREADPRIVATE(CISBA_GR)
 CHARACTER(LEN=4), POINTER        :: CRUNOFF_GR=>NULL()
!$OMP THREADPRIVATE(CRUNOFF_GR)
 CHARACTER(LEN=4), POINTER        :: CSCOND_GR=>NULL()
!$OMP THREADPRIVATE(CSCOND_GR)
LOGICAL,          POINTER        :: LTR_ML_GR=>NULL()
!$OMP THREADPRIVATE(LTR_ML_GR)
REAL, POINTER, DIMENSION(:,:)    :: XVEGTYPE=>NULL()
!$OMP THREADPRIVATE(XVEGTYPE)
INTEGER, POINTER                 :: NLAYER_GR=>NULL()
!$OMP THREADPRIVATE(NLAYER_GR)
INTEGER, POINTER                 :: NLAYER_HORT_GR=>NULL()
!$OMP THREADPRIVATE(NLAYER_HORT_GR)
INTEGER, POINTER                 :: NLAYER_DUN_GR=>NULL()
!$OMP THREADPRIVATE(NLAYER_DUN_GR)
REAL, POINTER, DIMENSION(:)      :: XSOILGRID_GR=>NULL()
!$OMP THREADPRIVATE(XSOILGRID_GR)
INTEGER, POINTER                 :: NTIME_GR=>NULL()
!$OMP THREADPRIVATE(NTIME_GR)
REAL, POINTER, DIMENSION(:)      :: XALBNIR_DRY=>NULL()
!$OMP THREADPRIVATE(XALBNIR_DRY)
REAL, POINTER, DIMENSION(:)      :: XALBVIS_DRY=>NULL()
!$OMP THREADPRIVATE(XALBVIS_DRY)
REAL, POINTER, DIMENSION(:)      :: XALBUV_DRY=>NULL()
!$OMP THREADPRIVATE(XALBUV_DRY)
REAL, POINTER, DIMENSION(:)      :: XALBNIR_WET=>NULL()
!$OMP THREADPRIVATE(XALBNIR_WET)
REAL, POINTER, DIMENSION(:)      :: XALBVIS_WET=>NULL()
!$OMP THREADPRIVATE(XALBVIS_WET)
REAL, POINTER, DIMENSION(:)      :: XALBUV_WET=>NULL()
!$OMP THREADPRIVATE(XALBUV_WET)
REAL, POINTER, DIMENSION(:)      :: XALBNIR_SOIL=>NULL()
!$OMP THREADPRIVATE(XALBNIR_SOIL)
REAL, POINTER, DIMENSION(:)      :: XALBVIS_SOIL=>NULL()
!$OMP THREADPRIVATE(XALBVIS_SOIL)
REAL, POINTER, DIMENSION(:)      :: XALBUV_SOIL=>NULL()
!$OMP THREADPRIVATE(XALBUV_SOIL)
REAL, POINTER, DIMENSION(:)   :: XALBNIR_TSOIL=>NULL()
!$OMP THREADPRIVATE(XALBNIR_TSOIL)
REAL, POINTER, DIMENSION(:)   :: XALBVIS_TSOIL=>NULL()
!$OMP THREADPRIVATE(XALBVIS_TSOIL)
REAL, POINTER, DIMENSION(:)      :: XZ0_O_Z0H=>NULL()
!$OMP THREADPRIVATE(XZ0_O_Z0H)
REAL, POINTER, DIMENSION(:)      :: XALBNIR=>NULL()
!$OMP THREADPRIVATE(XALBNIR)
REAL, POINTER, DIMENSION(:)      :: XALBVIS=>NULL()
!$OMP THREADPRIVATE(XALBVIS)
REAL, POINTER, DIMENSION(:)      :: XALBUV=>NULL()
!$OMP THREADPRIVATE(XALBUV)
REAL, POINTER, DIMENSION(:)      :: XEMIS=>NULL()
!$OMP THREADPRIVATE(XEMIS)
REAL, POINTER, DIMENSION(:)      :: XZ0=>NULL()
!$OMP THREADPRIVATE(XZ0)
REAL, POINTER, DIMENSION(:)      :: XALBNIR_VEG=>NULL()
!$OMP THREADPRIVATE(XALBNIR_VEG)
REAL, POINTER, DIMENSION(:)      :: XALBVIS_VEG=>NULL()
!$OMP THREADPRIVATE(XALBVIS_VEG)
REAL, POINTER, DIMENSION(:)      :: XALBUV_VEG=>NULL()
!$OMP THREADPRIVATE(XALBUV_VEG)
REAL, POINTER, DIMENSION(:)   :: XALBNIR_TVEG=>NULL()
!$OMP THREADPRIVATE(XALBNIR_TVEG)
REAL, POINTER, DIMENSION(:)   :: XALBVIS_TVEG=>NULL()
!$OMP THREADPRIVATE(XALBVIS_TVEG)
REAL, POINTER, DIMENSION(:)      :: XVEG=>NULL()
!$OMP THREADPRIVATE(XVEG)
REAL, POINTER, DIMENSION(:)      :: XWRMAX_CF=>NULL()
!$OMP THREADPRIVATE(XWRMAX_CF)
REAL, POINTER, DIMENSION(:)      :: XRSMIN=>NULL()
!$OMP THREADPRIVATE(XRSMIN)
REAL, POINTER, DIMENSION(:)      :: XGAMMA=>NULL()
!$OMP THREADPRIVATE(XGAMMA)
REAL, POINTER, DIMENSION(:)      :: XCV=>NULL()
!$OMP THREADPRIVATE(XCV)
REAL, POINTER, DIMENSION(:)      :: XRGL=>NULL()
!$OMP THREADPRIVATE(XRGL)
REAL, POINTER, DIMENSION(:,:)    :: XROOTFRAC=>NULL()
!$OMP THREADPRIVATE(XROOTFRAC)
REAL, DIMENSION(:), POINTER      :: XABC=>NULL()
!$OMP THREADPRIVATE(XABC)
REAL, DIMENSION(:), POINTER      :: XPOI=>NULL()
!$OMP THREADPRIVATE(XPOI)
REAL, POINTER, DIMENSION(:)      :: XBSLAI=>NULL()
!$OMP THREADPRIVATE(XBSLAI)
REAL, POINTER, DIMENSION(:)      :: XLAIMIN=>NULL()
!$OMP THREADPRIVATE(XLAIMIN)
REAL, POINTER, DIMENSION(:)      :: XSEFOLD=>NULL()
!$OMP THREADPRIVATE(XSEFOLD)
REAL, POINTER, DIMENSION(:)      :: XH_TREE=>NULL()
!$OMP THREADPRIVATE(XH_TREE)
REAL, POINTER, DIMENSION(:)      :: XANF=>NULL()
!$OMP THREADPRIVATE(XANF)
REAL, POINTER, DIMENSION(:)      :: XANMAX=>NULL()
!$OMP THREADPRIVATE(XANMAX)
REAL, POINTER, DIMENSION(:)      :: XFZERO=>NULL()
!$OMP THREADPRIVATE(XFZERO)
REAL, POINTER, DIMENSION(:)      :: XEPSO=>NULL()
!$OMP THREADPRIVATE(XEPSO)
REAL, POINTER, DIMENSION(:)      :: XGAMM=>NULL()
!$OMP THREADPRIVATE(XGAMM)
REAL, POINTER, DIMENSION(:)      :: XQDGAMM=>NULL()
!$OMP THREADPRIVATE(XQDGAMM)
REAL, POINTER, DIMENSION(:)      :: XGMES=>NULL()
!$OMP THREADPRIVATE(XGMES)
REAL, POINTER, DIMENSION(:)      :: XRE25=>NULL()
!$OMP THREADPRIVATE(XRE25)
REAL, POINTER, DIMENSION(:)      :: XQDGMES=>NULL()
!$OMP THREADPRIVATE(XQDGMES)
REAL, POINTER, DIMENSION(:)      :: XT1GMES=>NULL()
!$OMP THREADPRIVATE(XT1GMES)
REAL, POINTER, DIMENSION(:)      :: XT2GMES=>NULL()
!$OMP THREADPRIVATE(XT2GMES)
REAL, POINTER, DIMENSION(:)      :: XAMAX=>NULL()
!$OMP THREADPRIVATE(XAMAX)
REAL, POINTER, DIMENSION(:)      :: XQDAMAX=>NULL()
!$OMP THREADPRIVATE(XQDAMAX)
REAL, POINTER, DIMENSION(:)      :: XT1AMAX=>NULL()
!$OMP THREADPRIVATE(XT1AMAX)
REAL, POINTER, DIMENSION(:)      :: XT2AMAX=>NULL()
!$OMP THREADPRIVATE(XT2AMAX)
LOGICAL, POINTER, DIMENSION(:)   :: LSTRESS=>NULL()
!$OMP THREADPRIVATE(LSTRESS)
REAL, POINTER, DIMENSION(:)      :: XF2I=>NULL()
!$OMP THREADPRIVATE(XF2I)
REAL, POINTER, DIMENSION(:)      :: XGC=>NULL()
!$OMP THREADPRIVATE(XGC)
REAL, POINTER, DIMENSION(:)      :: XAH=>NULL()
!$OMP THREADPRIVATE(XAH)
REAL, POINTER, DIMENSION(:)      :: XBH=>NULL()
!$OMP THREADPRIVATE(XBH)
REAL, POINTER, DIMENSION(:)      :: XDMAX=>NULL()
!$OMP THREADPRIVATE(XDMAX)
REAL, POINTER, DIMENSION(:)      :: XCE_NITRO=>NULL()
!$OMP THREADPRIVATE(XCE_NITRO)
REAL, POINTER, DIMENSION(:)      :: XCF_NITRO=>NULL()
!$OMP THREADPRIVATE(XCF_NITRO)
REAL, POINTER, DIMENSION(:)      :: XCNA_NITRO=>NULL()
!$OMP THREADPRIVATE(XCNA_NITRO)
REAL, POINTER, DIMENSION(:)      :: XBSLAI_NITRO=>NULL()
!$OMP THREADPRIVATE(XBSLAI_NITRO)
REAL, POINTER, DIMENSION(:,:)    :: XOM_GR=>NULL()
!$OMP THREADPRIVATE(XOM_GR)
REAL, POINTER, DIMENSION(:,:)    :: XSAND_GR=>NULL()
!$OMP THREADPRIVATE(XSAND_GR)
REAL, POINTER, DIMENSION(:,:)    :: XCLAY_GR=>NULL()
!$OMP THREADPRIVATE(XCLAY_GR)
REAL, POINTER, DIMENSION(:)      :: XRUNOFFB_GR=>NULL()
!$OMP THREADPRIVATE(XRUNOFFB_GR)
REAL, POINTER, DIMENSION(:)      :: XWDRAIN_GR=>NULL()
!$OMP THREADPRIVATE(XWDRAIN_GR)
REAL, POINTER, DIMENSION(:)      :: XTAUICE=>NULL()
!$OMP THREADPRIVATE(XTAUICE)
REAL, POINTER, DIMENSION(:)      :: XGAMMAT=>NULL()
!$OMP THREADPRIVATE(XGAMMAT)
REAL, POINTER, DIMENSION(:,:)    :: XDG=>NULL()
!$OMP THREADPRIVATE(XDG)
REAL, POINTER, DIMENSION(:)      :: XRUNOFFD=>NULL()
!$OMP THREADPRIVATE(XRUNOFFD)
REAL, POINTER, DIMENSION(:,:)  :: XSOILWGHT=>NULL()
!$OMP THREADPRIVATE(XSOILWGHT)
REAL, POINTER, DIMENSION(:,:)  :: XDZG=>NULL()
!$OMP THREADPRIVATE(XDZG)
REAL, POINTER, DIMENSION(:,:)  :: XDZDIF=>NULL()
!$OMP THREADPRIVATE(XDZDIF)
INTEGER, POINTER, DIMENSION(:) :: NWG_LAYER=>NULL()
!$OMP THREADPRIVATE(NWG_LAYER)
REAL, POINTER, DIMENSION(:)    :: XDROOT=>NULL()
!$OMP THREADPRIVATE(XDROOT)
REAL, POINTER, DIMENSION(:)    :: XDG2=>NULL()
!$OMP THREADPRIVATE(XDG2)
REAL, POINTER, DIMENSION(:)      :: XC1SAT=>NULL()
!$OMP THREADPRIVATE(XC1SAT)
REAL, POINTER, DIMENSION(:)      :: XC2REF=>NULL()
!$OMP THREADPRIVATE(XC2REF)
REAL, POINTER, DIMENSION(:,:)    :: XC3=>NULL()
!$OMP THREADPRIVATE(XC3)
REAL, POINTER, DIMENSION(:)      :: XC4B=>NULL()
!$OMP THREADPRIVATE(XC4B)
REAL, POINTER, DIMENSION(:)      :: XC4REF=>NULL()
!$OMP THREADPRIVATE(XC4REF)
REAL, POINTER, DIMENSION(:)      :: XACOEF=>NULL()
!$OMP THREADPRIVATE(XACOEF)
REAL, POINTER, DIMENSION(:)       :: XPCOEF=>NULL()
!$OMP THREADPRIVATE(XPCOEF)
REAL, POINTER, DIMENSION(:,:)     :: XWFC=>NULL()
!$OMP THREADPRIVATE(XWFC)
REAL, POINTER, DIMENSION(:,:)     :: XWWILT=>NULL()
!$OMP THREADPRIVATE(XWWILT)
REAL, POINTER, DIMENSION(:,:)     :: XWSAT=>NULL()
!$OMP THREADPRIVATE(XWSAT)
REAL, POINTER, DIMENSION(:,:)     :: XBCOEF=>NULL()
!$OMP THREADPRIVATE(XBCOEF)
REAL, POINTER, DIMENSION(:,:)     :: XCONDSAT=>NULL()
!$OMP THREADPRIVATE(XCONDSAT)
REAL, POINTER, DIMENSION(:,:)     :: XMPOTSAT=>NULL()
!$OMP THREADPRIVATE(XMPOTSAT)
REAL, POINTER, DIMENSION(:)       :: XCGSAT=>NULL()
!$OMP THREADPRIVATE(XCGSAT)
REAL, POINTER, DIMENSION(:,:)     :: XHCAPSOIL=>NULL()
!$OMP THREADPRIVATE(XHCAPSOIL)
REAL, POINTER, DIMENSION(:,:)     :: XCONDDRY=>NULL()
!$OMP THREADPRIVATE(XCONDDRY)
REAL, POINTER, DIMENSION(:,:)     :: XCONDSLD=>NULL()
!$OMP THREADPRIVATE(XCONDSLD)
REAL, POINTER, DIMENSION(:)       :: XTDEEP=>NULL()
!$OMP THREADPRIVATE(XTDEEP)
TYPE(SURF_SNOW), POINTER          :: TSNOW=>NULL()
!$OMP THREADPRIVATE(TSNOW)
REAL, POINTER, DIMENSION(:)       :: XWR=>NULL()
!$OMP THREADPRIVATE(XWR)
REAL, POINTER, DIMENSION(:,:)     :: XTG=>NULL()
!$OMP THREADPRIVATE(XTG)
REAL, POINTER, DIMENSION(:,:)     :: XWG=>NULL()
!$OMP THREADPRIVATE(XWG)
REAL, POINTER, DIMENSION(:,:)     :: XWGI=>NULL()
!$OMP THREADPRIVATE(XWGI)
REAL, POINTER, DIMENSION(:)       :: XRESA=>NULL()
!$OMP THREADPRIVATE(XRESA)
REAL, POINTER, DIMENSION(:)       :: XPCPS=>NULL()
!$OMP THREADPRIVATE(XPCPS)
REAL, POINTER, DIMENSION(:)       :: XPLVTT=>NULL()
!$OMP THREADPRIVATE(XPLVTT)
REAL, POINTER, DIMENSION(:)       :: XPLSTT=>NULL()
!$OMP THREADPRIVATE(XPLSTT)
REAL, POINTER, DIMENSION(:)       :: XLAI=>NULL()
!$OMP THREADPRIVATE(XLAI)
REAL, POINTER, DIMENSION(:)       :: XAN=>NULL()
!$OMP THREADPRIVATE(XAN)
REAL, POINTER, DIMENSION(:)       :: XANDAY=>NULL()
!$OMP THREADPRIVATE(XANDAY)
REAL, POINTER, DIMENSION(:)       :: XANFM=>NULL()
!$OMP THREADPRIVATE(XANFM)
REAL, POINTER, DIMENSION(:)       :: XLE=>NULL()
!$OMP THREADPRIVATE(XLE)
REAL, POINTER, DIMENSION(:)     :: XFAPARC=>NULL()
!$OMP THREADPRIVATE(XFAPARC)
REAL, POINTER, DIMENSION(:)     :: XFAPIRC=>NULL()
!$OMP THREADPRIVATE(XFAPIRC)
REAL, POINTER, DIMENSION(:)     :: XLAI_EFFC=>NULL()
!$OMP THREADPRIVATE(XLAI_EFFC)
REAL, POINTER, DIMENSION(:)     :: XMUS=>NULL()
!$OMP THREADPRIVATE(XMUS)
REAL, POINTER, DIMENSION(:,:)     :: XRESP_BIOMASS=>NULL()
!$OMP THREADPRIVATE(XRESP_BIOMASS)
REAL, POINTER, DIMENSION(:,:)     :: XBIOMASS=>NULL()
!$OMP THREADPRIVATE(XBIOMASS)
REAL, POINTER, DIMENSION(:)       :: XPSNG=>NULL()
!$OMP THREADPRIVATE(XPSNG)
REAL, POINTER, DIMENSION(:)       :: XPSNV=>NULL()
!$OMP THREADPRIVATE(XPSNV)
REAL, POINTER, DIMENSION(:)       :: XPSNV_A=>NULL()
!$OMP THREADPRIVATE(XPSNV_A)
REAL, POINTER, DIMENSION(:)       :: XPSN=>NULL()
!$OMP THREADPRIVATE(XPSN)
REAL, POINTER, DIMENSION(:)       :: XSNOWFREE_ALB=>NULL()
!$OMP THREADPRIVATE(XSNOWFREE_ALB)
REAL, POINTER, DIMENSION(:)       :: XSNOWFREE_ALB_VEG=>NULL()
!$OMP THREADPRIVATE(XSNOWFREE_ALB_VEG)
REAL, POINTER, DIMENSION(:)       :: XSNOWFREE_ALB_SOIL=>NULL()
!$OMP THREADPRIVATE(XSNOWFREE_ALB_SOIL)
!
!SGH scheme
!
 CHARACTER(LEN=3), POINTER         :: CTOPREG_GR=>NULL()
!$OMP THREADPRIVATE(CTOPREG_GR)
 CHARACTER(LEN=3), POINTER         :: CKSAT_GR=>NULL()
!$OMP THREADPRIVATE(CKSAT_GR)
 CHARACTER(LEN=3), POINTER         :: CHORT_GR=>NULL()
!$OMP THREADPRIVATE(CHORT_GR)
 CHARACTER(LEN=3), POINTER         :: CSOC_GR=>NULL()
!$OMP THREADPRIVATE(CSOC_GR)
!
REAL, POINTER, DIMENSION(:)       :: XD_ICE=>NULL()
!$OMP THREADPRIVATE(XD_ICE)
REAL, POINTER, DIMENSION(:)       :: XKSAT_ICE=>NULL()
!$OMP THREADPRIVATE(XKSAT_ICE)
!
! Type of green roof (characterization of green roof structure based on GR vegetation)
 CHARACTER(LEN=5), POINTER         :: CTYP_GR=>NULL()
!$OMP THREADPRIVATE(CTYP_GR)
!
CONTAINS



SUBROUTINE TEB_GREENROOF_OPTIONS_GOTO_MODEL(KFROM, KTO, LKFROM)
LOGICAL, INTENT(IN) :: LKFROM
INTEGER, INTENT(IN) :: KFROM, KTO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Save current state for allocated arrays
IF (LKFROM) THEN
ENDIF
!
! Current model is set to model KTO
IF (LHOOK) CALL DR_HOOK('MODD_TEB_GREENROOF_N:TEB_GREENROOF_OPTIONS_GOTO_MODEL',0,ZHOOK_HANDLE)
LPAR_GREENROOF=>TEB_GREENROOF_OPTIONS_MODEL(KTO)%LPAR_GREENROOF
IF (LKFROM) THEN
TEB_GREENROOF_OPTIONS_MODEL(KTO)%XSOILGRID_GR=>XSOILGRID_GR
ENDIF
CISBA_GR=>TEB_GREENROOF_OPTIONS_MODEL(KTO)%CISBA_GR
LTR_ML_GR=>TEB_GREENROOF_OPTIONS_MODEL(KTO)%LTR_ML_GR
CRUNOFF_GR=>TEB_GREENROOF_OPTIONS_MODEL(KTO)%CRUNOFF_GR
CSCOND_GR=>TEB_GREENROOF_OPTIONS_MODEL(KTO)%CSCOND_GR
CTOPREG_GR=>TEB_GREENROOF_OPTIONS_MODEL(KTO)%CTOPREG_GR
CKSAT_GR=>TEB_GREENROOF_OPTIONS_MODEL(KTO)%CKSAT_GR
CHORT_GR=>TEB_GREENROOF_OPTIONS_MODEL(KTO)%CHORT_GR
CTYP_GR=>TEB_GREENROOF_OPTIONS_MODEL(KTO)%CTYP_GR
CSOC_GR=>TEB_GREENROOF_OPTIONS_MODEL(KTO)%CSOC_GR
NLAYER_GR=>TEB_GREENROOF_OPTIONS_MODEL(KTO)%NLAYER_GR
NLAYER_HORT_GR=>TEB_GREENROOF_OPTIONS_MODEL(KTO)%NLAYER_HORT_GR
NLAYER_DUN_GR=>TEB_GREENROOF_OPTIONS_MODEL(KTO)%NLAYER_DUN_GR
XSOILGRID_GR=>TEB_GREENROOF_OPTIONS_MODEL(KTO)%XSOILGRID_GR
NTIME_GR=>TEB_GREENROOF_OPTIONS_MODEL(KTO)%NTIME_GR
IF (LHOOK) CALL DR_HOOK('MODD_TEB_GREENROOF_N:TEB_GREENROOF_OPTIONS_GOTO_MODEL',1,ZHOOK_HANDLE)
!
END SUBROUTINE TEB_GREENROOF_OPTIONS_GOTO_MODEL

SUBROUTINE TEB_GREENROOF_OPTIONS_ALLOC(KMODEL)
INTEGER, INTENT(IN) :: KMODEL
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_ALLOC",0,ZHOOK_HANDLE)
ALLOCATE(TEB_GREENROOF_OPTIONS_MODEL(KMODEL))
DO J=1,KMODEL
  NULLIFY(TEB_GREENROOF_OPTIONS_MODEL(J)%XSOILGRID_GR)
ENDDO
TEB_GREENROOF_OPTIONS_MODEL(:)%LPAR_GREENROOF=.TRUE.
TEB_GREENROOF_OPTIONS_MODEL(:)%CISBA_GR=' '
TEB_GREENROOF_OPTIONS_MODEL(:)%LTR_ML_GR=.FALSE.
TEB_GREENROOF_OPTIONS_MODEL(:)%CSOC_GR=' '
TEB_GREENROOF_OPTIONS_MODEL(:)%CRUNOFF_GR=' '
TEB_GREENROOF_OPTIONS_MODEL(:)%CSCOND_GR=' '
TEB_GREENROOF_OPTIONS_MODEL(:)%CTOPREG_GR=' '
TEB_GREENROOF_OPTIONS_MODEL(:)%CKSAT_GR=' '
TEB_GREENROOF_OPTIONS_MODEL(:)%CHORT_GR=' '
TEB_GREENROOF_OPTIONS_MODEL(:)%CTYP_GR=' '
TEB_GREENROOF_OPTIONS_MODEL(:)%NLAYER_GR=0
TEB_GREENROOF_OPTIONS_MODEL(:)%NLAYER_HORT_GR=0
TEB_GREENROOF_OPTIONS_MODEL(:)%NLAYER_DUN_GR=0
TEB_GREENROOF_OPTIONS_MODEL(:)%NTIME_GR=0
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_OPTIONS_ALLOC",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GREENROOF_OPTIONS_ALLOC

SUBROUTINE TEB_GREENROOF_OPTIONS_DEALLO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_OPTIONS_DEALLO",0,ZHOOK_HANDLE)
IF (ALLOCATED(TEB_GREENROOF_OPTIONS_MODEL)) DEALLOCATE(TEB_GREENROOF_OPTIONS_MODEL)
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_OPTIONS_DEALLO",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GREENROOF_OPTIONS_DEALLO



SUBROUTINE TEB_GREENROOF_PGD_GOTO_MODEL(KFROM, KTO, LKFROM)
LOGICAL, INTENT(IN) :: LKFROM
INTEGER, INTENT(IN) :: KFROM, KTO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Save current state for allocated arrays
IF (LKFROM) THEN
TEB_GREENROOF_PGD_MODEL(KFROM)%XVEGTYPE=>XVEGTYPE
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBNIR_DRY=>XALBNIR_DRY
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBVIS_DRY=>XALBVIS_DRY
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBUV_DRY=>XALBUV_DRY
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBNIR_WET=>XALBNIR_WET
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBVIS_WET=>XALBVIS_WET
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBUV_WET=>XALBUV_WET
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBNIR_SOIL=>XALBNIR_SOIL
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBVIS_SOIL=>XALBVIS_SOIL
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBUV_SOIL=>XALBUV_SOIL
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBNIR_TSOIL=>XALBNIR_TSOIL
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBVIS_TSOIL=>XALBVIS_TSOIL
TEB_GREENROOF_PGD_MODEL(KFROM)%XZ0_O_Z0H=>XZ0_O_Z0H
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBNIR_VEG=>XALBNIR_VEG
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBVIS_VEG=>XALBVIS_VEG
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBUV_VEG=>XALBUV_VEG
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBNIR_TVEG=>XALBNIR_TVEG
TEB_GREENROOF_PGD_MODEL(KFROM)%XALBVIS_TVEG=>XALBVIS_TVEG
TEB_GREENROOF_PGD_MODEL(KFROM)%XWRMAX_CF=>XWRMAX_CF
TEB_GREENROOF_PGD_MODEL(KFROM)%XRSMIN=>XRSMIN
TEB_GREENROOF_PGD_MODEL(KFROM)%XGAMMA=>XGAMMA
TEB_GREENROOF_PGD_MODEL(KFROM)%XCV=>XCV
TEB_GREENROOF_PGD_MODEL(KFROM)%XRGL=>XRGL
TEB_GREENROOF_PGD_MODEL(KFROM)%XROOTFRAC=>XROOTFRAC
TEB_GREENROOF_PGD_MODEL(KFROM)%XABC=>XABC
TEB_GREENROOF_PGD_MODEL(KFROM)%XPOI=>XPOI
TEB_GREENROOF_PGD_MODEL(KFROM)%XBSLAI=>XBSLAI
TEB_GREENROOF_PGD_MODEL(KFROM)%XLAIMIN=>XLAIMIN
TEB_GREENROOF_PGD_MODEL(KFROM)%XSEFOLD=>XSEFOLD
TEB_GREENROOF_PGD_MODEL(KFROM)%XH_TREE=>XH_TREE
TEB_GREENROOF_PGD_MODEL(KFROM)%XANF=>XANF
TEB_GREENROOF_PGD_MODEL(KFROM)%XANMAX=>XANMAX
TEB_GREENROOF_PGD_MODEL(KFROM)%XFZERO=>XFZERO
TEB_GREENROOF_PGD_MODEL(KFROM)%XEPSO=>XEPSO
TEB_GREENROOF_PGD_MODEL(KFROM)%XGAMM=>XGAMM
TEB_GREENROOF_PGD_MODEL(KFROM)%XQDGAMM=>XQDGAMM
TEB_GREENROOF_PGD_MODEL(KFROM)%XGMES=>XGMES
TEB_GREENROOF_PGD_MODEL(KFROM)%XRE25=>XRE25
TEB_GREENROOF_PGD_MODEL(KFROM)%XQDGMES=>XQDGMES
TEB_GREENROOF_PGD_MODEL(KFROM)%XT1GMES=>XT1GMES
TEB_GREENROOF_PGD_MODEL(KFROM)%XT2GMES=>XT2GMES
TEB_GREENROOF_PGD_MODEL(KFROM)%XAMAX=>XAMAX
TEB_GREENROOF_PGD_MODEL(KFROM)%XQDAMAX=>XQDAMAX
TEB_GREENROOF_PGD_MODEL(KFROM)%XT1AMAX=>XT1AMAX
TEB_GREENROOF_PGD_MODEL(KFROM)%XT2AMAX=>XT2AMAX
TEB_GREENROOF_PGD_MODEL(KFROM)%LSTRESS=>LSTRESS
TEB_GREENROOF_PGD_MODEL(KFROM)%XF2I=>XF2I
TEB_GREENROOF_PGD_MODEL(KFROM)%XGC=>XGC
TEB_GREENROOF_PGD_MODEL(KFROM)%XAH=>XAH
TEB_GREENROOF_PGD_MODEL(KFROM)%XBH=>XBH
TEB_GREENROOF_PGD_MODEL(KFROM)%XDMAX=>XDMAX
TEB_GREENROOF_PGD_MODEL(KFROM)%XCE_NITRO=>XCE_NITRO
TEB_GREENROOF_PGD_MODEL(KFROM)%XCF_NITRO=>XCF_NITRO
TEB_GREENROOF_PGD_MODEL(KFROM)%XCNA_NITRO=>XCNA_NITRO
TEB_GREENROOF_PGD_MODEL(KFROM)%XBSLAI_NITRO=>XBSLAI_NITRO
TEB_GREENROOF_PGD_MODEL(KFROM)%XOM_GR=>XOM_GR
TEB_GREENROOF_PGD_MODEL(KFROM)%XSAND_GR=>XSAND_GR
TEB_GREENROOF_PGD_MODEL(KFROM)%XCLAY_GR=>XCLAY_GR
TEB_GREENROOF_PGD_MODEL(KFROM)%XRUNOFFB_GR=>XRUNOFFB_GR
TEB_GREENROOF_PGD_MODEL(KFROM)%XWDRAIN_GR=>XWDRAIN_GR
TEB_GREENROOF_PGD_MODEL(KFROM)%XTAUICE=>XTAUICE
TEB_GREENROOF_PGD_MODEL(KFROM)%XGAMMAT=>XGAMMAT
TEB_GREENROOF_PGD_MODEL(KFROM)%XDG=>XDG
TEB_GREENROOF_PGD_MODEL(KFROM)%XRUNOFFD=>XRUNOFFD
TEB_GREENROOF_PGD_MODEL(KFROM)%XSOILWGHT=>XSOILWGHT
TEB_GREENROOF_PGD_MODEL(KFROM)%XDZG=>XDZG
TEB_GREENROOF_PGD_MODEL(KFROM)%XDZDIF=>XDZDIF
TEB_GREENROOF_PGD_MODEL(KFROM)%NWG_LAYER=>NWG_LAYER
TEB_GREENROOF_PGD_MODEL(KFROM)%XDROOT=>XDROOT
TEB_GREENROOF_PGD_MODEL(KFROM)%XDG2=>XDG2
TEB_GREENROOF_PGD_MODEL(KFROM)%XPCPS=>XPCPS
TEB_GREENROOF_PGD_MODEL(KFROM)%XPLVTT=>XPLVTT
TEB_GREENROOF_PGD_MODEL(KFROM)%XPLSTT=>XPLSTT
TEB_GREENROOF_PGD_MODEL(KFROM)%XC1SAT=>XC1SAT
TEB_GREENROOF_PGD_MODEL(KFROM)%XC2REF=>XC2REF
TEB_GREENROOF_PGD_MODEL(KFROM)%XC3=>XC3
TEB_GREENROOF_PGD_MODEL(KFROM)%XC4B=>XC4B
TEB_GREENROOF_PGD_MODEL(KFROM)%XC4REF=>XC4REF
TEB_GREENROOF_PGD_MODEL(KFROM)%XACOEF=>XACOEF
TEB_GREENROOF_PGD_MODEL(KFROM)%XPCOEF=>XPCOEF
TEB_GREENROOF_PGD_MODEL(KFROM)%XWFC=>XWFC
TEB_GREENROOF_PGD_MODEL(KFROM)%XWWILT=>XWWILT
TEB_GREENROOF_PGD_MODEL(KFROM)%XWSAT=>XWSAT
TEB_GREENROOF_PGD_MODEL(KFROM)%XBCOEF=>XBCOEF
TEB_GREENROOF_PGD_MODEL(KFROM)%XCONDSAT=>XCONDSAT
TEB_GREENROOF_PGD_MODEL(KFROM)%XMPOTSAT=>XMPOTSAT
TEB_GREENROOF_PGD_MODEL(KFROM)%XCGSAT=>XCGSAT
TEB_GREENROOF_PGD_MODEL(KFROM)%XHCAPSOIL=>XHCAPSOIL
TEB_GREENROOF_PGD_MODEL(KFROM)%XCONDDRY=>XCONDDRY
TEB_GREENROOF_PGD_MODEL(KFROM)%XCONDSLD=>XCONDSLD
TEB_GREENROOF_PGD_MODEL(KFROM)%XTDEEP=>XTDEEP
TEB_GREENROOF_PGD_MODEL(KFROM)%XD_ICE=>XD_ICE
ENDIF
!
! Current model is set to model KTO
IF (LHOOK) CALL DR_HOOK('MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_GOTO_MODEL',0,ZHOOK_HANDLE)
XVEGTYPE=>TEB_GREENROOF_PGD_MODEL(KTO)%XVEGTYPE
XALBNIR_DRY=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBNIR_DRY
XALBVIS_DRY=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBVIS_DRY
XALBUV_DRY=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBUV_DRY
XALBNIR_WET=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBNIR_WET
XALBVIS_WET=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBVIS_WET
XALBUV_WET=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBUV_WET
XALBNIR_SOIL=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBNIR_SOIL
XALBVIS_SOIL=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBVIS_SOIL
XALBUV_SOIL=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBUV_SOIL
XALBNIR_TSOIL=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBNIR_TSOIL
XALBVIS_TSOIL=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBVIS_TSOIL
XZ0_O_Z0H=>TEB_GREENROOF_PGD_MODEL(KTO)%XZ0_O_Z0H
XALBNIR_VEG=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBNIR_VEG
XALBVIS_VEG=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBVIS_VEG
XALBUV_VEG=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBUV_VEG
XALBNIR_TVEG=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBNIR_TVEG
XALBVIS_TVEG=>TEB_GREENROOF_PGD_MODEL(KTO)%XALBVIS_TVEG
XWRMAX_CF=>TEB_GREENROOF_PGD_MODEL(KTO)%XWRMAX_CF
XRSMIN=>TEB_GREENROOF_PGD_MODEL(KTO)%XRSMIN
XGAMMA=>TEB_GREENROOF_PGD_MODEL(KTO)%XGAMMA
XCV=>TEB_GREENROOF_PGD_MODEL(KTO)%XCV
XRGL=>TEB_GREENROOF_PGD_MODEL(KTO)%XRGL
XROOTFRAC=>TEB_GREENROOF_PGD_MODEL(KTO)%XROOTFRAC
XABC=>TEB_GREENROOF_PGD_MODEL(KTO)%XABC
XPOI=>TEB_GREENROOF_PGD_MODEL(KTO)%XPOI
XBSLAI=>TEB_GREENROOF_PGD_MODEL(KTO)%XBSLAI
XLAIMIN=>TEB_GREENROOF_PGD_MODEL(KTO)%XLAIMIN
XSEFOLD=>TEB_GREENROOF_PGD_MODEL(KTO)%XSEFOLD
XH_TREE=>TEB_GREENROOF_PGD_MODEL(KTO)%XH_TREE
XANF=>TEB_GREENROOF_PGD_MODEL(KTO)%XANF
XANMAX=>TEB_GREENROOF_PGD_MODEL(KTO)%XANMAX
XFZERO=>TEB_GREENROOF_PGD_MODEL(KTO)%XFZERO
XEPSO=>TEB_GREENROOF_PGD_MODEL(KTO)%XEPSO
XGAMM=>TEB_GREENROOF_PGD_MODEL(KTO)%XGAMM
XQDGAMM=>TEB_GREENROOF_PGD_MODEL(KTO)%XQDGAMM
XGMES=>TEB_GREENROOF_PGD_MODEL(KTO)%XGMES
XRE25=>TEB_GREENROOF_PGD_MODEL(KTO)%XRE25
XQDGMES=>TEB_GREENROOF_PGD_MODEL(KTO)%XQDGMES
XT1GMES=>TEB_GREENROOF_PGD_MODEL(KTO)%XT1GMES
XT2GMES=>TEB_GREENROOF_PGD_MODEL(KTO)%XT2GMES
XAMAX=>TEB_GREENROOF_PGD_MODEL(KTO)%XAMAX
XQDAMAX=>TEB_GREENROOF_PGD_MODEL(KTO)%XQDAMAX
XT1AMAX=>TEB_GREENROOF_PGD_MODEL(KTO)%XT1AMAX
XT2AMAX=>TEB_GREENROOF_PGD_MODEL(KTO)%XT2AMAX
LSTRESS=>TEB_GREENROOF_PGD_MODEL(KTO)%LSTRESS
XF2I=>TEB_GREENROOF_PGD_MODEL(KTO)%XF2I
XGC=>TEB_GREENROOF_PGD_MODEL(KTO)%XGC
XAH=>TEB_GREENROOF_PGD_MODEL(KTO)%XAH
XBH=>TEB_GREENROOF_PGD_MODEL(KTO)%XBH
XDMAX=>TEB_GREENROOF_PGD_MODEL(KTO)%XDMAX
XCE_NITRO=>TEB_GREENROOF_PGD_MODEL(KTO)%XCE_NITRO
XCF_NITRO=>TEB_GREENROOF_PGD_MODEL(KTO)%XCF_NITRO
XCNA_NITRO=>TEB_GREENROOF_PGD_MODEL(KTO)%XCNA_NITRO
XBSLAI_NITRO=>TEB_GREENROOF_PGD_MODEL(KTO)%XBSLAI_NITRO
XOM_GR=>TEB_GREENROOF_PGD_MODEL(KTO)%XOM_GR
XSAND_GR=>TEB_GREENROOF_PGD_MODEL(KTO)%XSAND_GR
XCLAY_GR=>TEB_GREENROOF_PGD_MODEL(KTO)%XCLAY_GR
XRUNOFFB_GR=>TEB_GREENROOF_PGD_MODEL(KTO)%XRUNOFFB_GR
XWDRAIN_GR=>TEB_GREENROOF_PGD_MODEL(KTO)%XWDRAIN_GR
XTAUICE=>TEB_GREENROOF_PGD_MODEL(KTO)%XTAUICE
XGAMMAT=>TEB_GREENROOF_PGD_MODEL(KTO)%XGAMMAT
XDG=>TEB_GREENROOF_PGD_MODEL(KTO)%XDG
XRUNOFFD=>TEB_GREENROOF_PGD_MODEL(KTO)%XRUNOFFD
XSOILWGHT=>TEB_GREENROOF_PGD_MODEL(KTO)%XSOILWGHT
XDZG=>TEB_GREENROOF_PGD_MODEL(KTO)%XDZG
XDZDIF=>TEB_GREENROOF_PGD_MODEL(KTO)%XDZDIF
NWG_LAYER=>TEB_GREENROOF_PGD_MODEL(KTO)%NWG_LAYER
XDROOT=>TEB_GREENROOF_PGD_MODEL(KTO)%XDROOT
XDG2=>TEB_GREENROOF_PGD_MODEL(KTO)%XDG2
XPCPS=>TEB_GREENROOF_PGD_MODEL(KTO)%XPCPS
XPLVTT=>TEB_GREENROOF_PGD_MODEL(KTO)%XPLVTT
XPLSTT=>TEB_GREENROOF_PGD_MODEL(KTO)%XPLSTT
XC1SAT=>TEB_GREENROOF_PGD_MODEL(KTO)%XC1SAT
XC2REF=>TEB_GREENROOF_PGD_MODEL(KTO)%XC2REF
XC3=>TEB_GREENROOF_PGD_MODEL(KTO)%XC3
XC4B=>TEB_GREENROOF_PGD_MODEL(KTO)%XC4B
XC4REF=>TEB_GREENROOF_PGD_MODEL(KTO)%XC4REF
XACOEF=>TEB_GREENROOF_PGD_MODEL(KTO)%XACOEF
XPCOEF=>TEB_GREENROOF_PGD_MODEL(KTO)%XPCOEF
XWFC=>TEB_GREENROOF_PGD_MODEL(KTO)%XWFC
XWWILT=>TEB_GREENROOF_PGD_MODEL(KTO)%XWWILT
XWSAT=>TEB_GREENROOF_PGD_MODEL(KTO)%XWSAT
XBCOEF=>TEB_GREENROOF_PGD_MODEL(KTO)%XBCOEF
XCONDSAT=>TEB_GREENROOF_PGD_MODEL(KTO)%XCONDSAT
XMPOTSAT=>TEB_GREENROOF_PGD_MODEL(KTO)%XMPOTSAT
XCGSAT=>TEB_GREENROOF_PGD_MODEL(KTO)%XCGSAT
XHCAPSOIL=>TEB_GREENROOF_PGD_MODEL(KTO)%XHCAPSOIL
XCONDDRY=>TEB_GREENROOF_PGD_MODEL(KTO)%XCONDDRY
XCONDSLD=>TEB_GREENROOF_PGD_MODEL(KTO)%XCONDSLD
XTDEEP=>TEB_GREENROOF_PGD_MODEL(KTO)%XTDEEP
XD_ICE=>TEB_GREENROOF_PGD_MODEL(KTO)%XD_ICE
IF (LHOOK) CALL DR_HOOK('MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_GOTO_MODEL',1,ZHOOK_HANDLE)
!
END SUBROUTINE TEB_GREENROOF_PGD_GOTO_MODEL

SUBROUTINE TEB_GREENROOF_PGD_ALLOC(KMODEL)
INTEGER, INTENT(IN) :: KMODEL
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_ALLOC",0,ZHOOK_HANDLE)
ALLOCATE(TEB_GREENROOF_PGD_MODEL(KMODEL))
DO J=1,KMODEL
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XVEGTYPE)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBNIR_DRY)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBVIS_DRY)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBUV_DRY)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBNIR_WET)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBVIS_WET)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBUV_WET)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBNIR_SOIL)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBVIS_SOIL)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBUV_SOIL)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBNIR_TSOIL)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBVIS_TSOIL)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XZ0_O_Z0H)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBNIR_VEG)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBVIS_VEG)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBUV_VEG)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBNIR_TVEG)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XALBVIS_TVEG)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XWRMAX_CF)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XRSMIN)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XGAMMA)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XCV)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XRGL)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XROOTFRAC)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XABC)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XPOI)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XBSLAI)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XLAIMIN)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XSEFOLD)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XH_TREE)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XANF)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XANMAX)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XFZERO)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XEPSO)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XGAMM)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XQDGAMM)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XGMES)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XRE25)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XQDGMES)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XT1GMES)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XT2GMES)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XAMAX)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XQDAMAX)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XT1AMAX)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XT2AMAX)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%LSTRESS)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XF2I)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XGC)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XAH)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XBH)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XDMAX)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XCE_NITRO)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XCF_NITRO)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XCNA_NITRO)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XBSLAI_NITRO)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XOM_GR)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XSAND_GR)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XCLAY_GR)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XRUNOFFB_GR)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XWDRAIN_GR)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XTAUICE)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XGAMMAT)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XDG)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XRUNOFFD)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XSOILWGHT)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XDZG)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XDZDIF)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%NWG_LAYER)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XDROOT)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XDG2)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XPCPS)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XPLVTT)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XPLSTT)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XC1SAT)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XC2REF)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XC3)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XC4B)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XC4REF)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XACOEF)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XPCOEF)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XWFC)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XWWILT)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XWSAT)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XBCOEF)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XCONDSAT)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XMPOTSAT)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XCGSAT)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XHCAPSOIL)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XCONDDRY)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XCONDSLD)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XTDEEP)
NULLIFY(TEB_GREENROOF_PGD_MODEL(J)%XD_ICE)
ENDDO
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_ALLOC",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GREENROOF_PGD_ALLOC

SUBROUTINE TEB_GREENROOF_PGD_DEALLO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_DEALLO",0,ZHOOK_HANDLE)
IF (ALLOCATED(TEB_GREENROOF_PGD_MODEL)) DEALLOCATE(TEB_GREENROOF_PGD_MODEL)
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_DEALLO",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GREENROOF_PGD_DEALLO




SUBROUTINE TEB_GREENROOF_PGD_EVOL_GOTO_MODEL(KFROM, KTO, LKFROM, KFROM_PATCH, KTO_PATCH)
LOGICAL, INTENT(IN) :: LKFROM
INTEGER, INTENT(IN) :: KFROM, KTO
INTEGER, INTENT(IN) :: KFROM_PATCH, KTO_PATCH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Save current state for allocated arrays
IF (LKFROM) THEN
TEB_GREENROOF_PGD_EVOL_MODEL(KFROM,KFROM_PATCH)%XALBNIR=>XALBNIR
TEB_GREENROOF_PGD_EVOL_MODEL(KFROM,KFROM_PATCH)%XALBVIS=>XALBVIS
TEB_GREENROOF_PGD_EVOL_MODEL(KFROM,KFROM_PATCH)%XALBUV=>XALBUV
TEB_GREENROOF_PGD_EVOL_MODEL(KFROM,KFROM_PATCH)%XEMIS=>XEMIS
TEB_GREENROOF_PGD_EVOL_MODEL(KFROM,KFROM_PATCH)%XZ0=>XZ0
TEB_GREENROOF_PGD_EVOL_MODEL(KFROM,KFROM_PATCH)%XVEG=>XVEG
TEB_GREENROOF_PGD_EVOL_MODEL(KFROM,KFROM_PATCH)%XLAI=>XLAI
ENDIF
!
! Current model is set to model KTO
IF (LHOOK) CALL DR_HOOK('MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_EVOL_GOTO_MODEL',0,ZHOOK_HANDLE)
XALBNIR=>TEB_GREENROOF_PGD_EVOL_MODEL(KTO,KTO_PATCH)%XALBNIR
XALBVIS=>TEB_GREENROOF_PGD_EVOL_MODEL(KTO,KTO_PATCH)%XALBVIS
XALBUV=>TEB_GREENROOF_PGD_EVOL_MODEL(KTO,KTO_PATCH)%XALBUV
XEMIS=>TEB_GREENROOF_PGD_EVOL_MODEL(KTO,KTO_PATCH)%XEMIS
XZ0=>TEB_GREENROOF_PGD_EVOL_MODEL(KTO,KTO_PATCH)%XZ0
XVEG=>TEB_GREENROOF_PGD_EVOL_MODEL(KTO,KTO_PATCH)%XVEG
XLAI=>TEB_GREENROOF_PGD_EVOL_MODEL(KTO,KTO_PATCH)%XLAI
IF (LHOOK) CALL DR_HOOK('MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_EVOL_GOTO_MODEL',1,ZHOOK_HANDLE)
!
END SUBROUTINE TEB_GREENROOF_PGD_EVOL_GOTO_MODEL

SUBROUTINE TEB_GREENROOF_PGD_EVOL_ALLOC(KMODEL,KPATCH)
INTEGER, INTENT(IN) :: KMODEL, KPATCH
INTEGER :: J, JP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_EVOL_ALLOC",0,ZHOOK_HANDLE)
ALLOCATE(TEB_GREENROOF_PGD_EVOL_MODEL(KMODEL,KPATCH))
DO J=1,KMODEL
 DO JP=1,KPATCH
NULLIFY(TEB_GREENROOF_PGD_EVOL_MODEL(J,JP)%XALBNIR)
NULLIFY(TEB_GREENROOF_PGD_EVOL_MODEL(J,JP)%XALBVIS)
NULLIFY(TEB_GREENROOF_PGD_EVOL_MODEL(J,JP)%XALBUV)
NULLIFY(TEB_GREENROOF_PGD_EVOL_MODEL(J,JP)%XEMIS)
NULLIFY(TEB_GREENROOF_PGD_EVOL_MODEL(J,JP)%XZ0)
NULLIFY(TEB_GREENROOF_PGD_EVOL_MODEL(J,JP)%XVEG)
NULLIFY(TEB_GREENROOF_PGD_EVOL_MODEL(J,JP)%XLAI)
 ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_EVOL_ALLOC",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GREENROOF_PGD_EVOL_ALLOC

SUBROUTINE TEB_GREENROOF_PGD_EVOL_DEALLO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_EVOL_DEALLO",0,ZHOOK_HANDLE)
IF (ALLOCATED(TEB_GREENROOF_PGD_EVOL_MODEL)) DEALLOCATE(TEB_GREENROOF_PGD_EVOL_MODEL)
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_EVOL_DEALLO",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GREENROOF_PGD_EVOL_DEALLO




SUBROUTINE TEB_GREENROOF_GOTO_MODEL(KFROM, KTO, LKFROM, KFROM_PATCH, KTO_PATCH)
LOGICAL, INTENT(IN) :: LKFROM
INTEGER, INTENT(IN) :: KFROM, KTO
INTEGER, INTENT(IN) :: KFROM_PATCH, KTO_PATCH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Save current state for allocated arrays
IF (LKFROM) THEN
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XWR=>XWR
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XTG=>XTG
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XWG=>XWG
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XWGI=>XWGI
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XRESA=>XRESA
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XAN=>XAN
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XANDAY=>XANDAY
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XANFM=>XANFM
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XLE=>XLE
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XFAPARC=>XFAPARC
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XFAPIRC=>XFAPIRC
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XLAI_EFFC=>XLAI_EFFC
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XMUS=>XMUS
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XRESP_BIOMASS=>XRESP_BIOMASS
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XBIOMASS=>XBIOMASS
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XPSNG=>XPSNG
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XPSNV_A=>XPSNV_A
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XPSNV=>XPSNV
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XPSN=>XPSN
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XSNOWFREE_ALB=>XSNOWFREE_ALB
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XSNOWFREE_ALB_VEG=>XSNOWFREE_ALB_VEG
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XSNOWFREE_ALB_SOIL=>XSNOWFREE_ALB_SOIL
TEB_GREENROOF_MODEL(KFROM,KFROM_PATCH)%XKSAT_ICE=>XKSAT_ICE
ENDIF
!
! Current model is set to model KTO
IF (LHOOK) CALL DR_HOOK('MODD_TEB_GREENROOF_N:TEB_GREENROOF_GOTO_MODEL',0,ZHOOK_HANDLE)
TSNOW=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%TSNOW
XWR=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XWR
XTG=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XTG
XWG=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XWG
XWGI=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XWGI
XRESA=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XRESA
XAN=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XAN
XANDAY=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XANDAY
XANFM=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XANFM
XLE=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XLE
XFAPARC=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XFAPARC
XFAPIRC=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XFAPIRC
XLAI_EFFC=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XLAI_EFFC
XMUS=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XMUS
XRESP_BIOMASS=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XRESP_BIOMASS
XBIOMASS=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XBIOMASS
XPSNG=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XPSNG
XPSNV_A=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XPSNV_A
XPSNV=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XPSNV
XPSN=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XPSN
XSNOWFREE_ALB=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XSNOWFREE_ALB
XSNOWFREE_ALB_VEG=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XSNOWFREE_ALB_VEG
XSNOWFREE_ALB_SOIL=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XSNOWFREE_ALB_SOIL
XKSAT_ICE=>TEB_GREENROOF_MODEL(KTO,KTO_PATCH)%XKSAT_ICE
IF (LHOOK) CALL DR_HOOK('MODD_TEB_GREENROOF_N:TEB_GREENROOF_GOTO_MODEL',1,ZHOOK_HANDLE)
!
END SUBROUTINE TEB_GREENROOF_GOTO_MODEL

SUBROUTINE TEB_GREENROOF_ALLOC(KMODEL,KPATCH)
INTEGER, INTENT(IN) :: KMODEL, KPATCH
INTEGER :: J, JP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_ALLOC",0,ZHOOK_HANDLE)
ALLOCATE(TEB_GREENROOF_MODEL(KMODEL,KPATCH))
DO J=1,KMODEL
 DO JP=1,KPATCH
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XWR)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XTG)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XWG)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XWGI)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XRESA)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XAN)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XANDAY)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XANFM)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XLE)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XFAPARC)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XFAPIRC)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XLAI_EFFC)  
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XMUS)     
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XRESP_BIOMASS)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XBIOMASS)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XKSAT_ICE)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XPSNG)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XPSNV)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XPSNV_A)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XPSN)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XSNOWFREE_ALB)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XSNOWFREE_ALB_VEG)
  NULLIFY(TEB_GREENROOF_MODEL(J,JP)%XSNOWFREE_ALB_SOIL)
 ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_ALLOC",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GREENROOF_ALLOC

SUBROUTINE TEB_GREENROOF_DEALLO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_DEALLO",0,ZHOOK_HANDLE)
IF (ALLOCATED(TEB_GREENROOF_MODEL)) DEALLOCATE(TEB_GREENROOF_MODEL)
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_DEALLO",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GREENROOF_DEALLO

END MODULE MODD_TEB_GREENROOF_n
