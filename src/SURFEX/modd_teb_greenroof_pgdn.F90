!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!##################
MODULE MODD_TEB_GREENROOF_PGD_n
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
!!      A. Lemonsu *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       09/2009
!!      C. de Munck     06/2011 
!!      V. Masson       06/2013  splits module in 4
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE


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
  REAL, POINTER, DIMENSION(:)    :: XKSAT_ICE       ! hydraulic conductivity at saturation
                                                    ! over frozen area (m s-1)
!-------------------------------------------------------------------------------
!
END TYPE TEB_GREENROOF_PGD_t




 CONTAINS



!


!

SUBROUTINE TEB_GREENROOF_PGD_INIT(YTEB_GREENROOF_PGD)
TYPE(TEB_GREENROOF_PGD_t), INTENT(INOUT) :: YTEB_GREENROOF_PGD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_INIT",0,ZHOOK_HANDLE)
NULLIFY(YTEB_GREENROOF_PGD%XVEGTYPE)
NULLIFY(YTEB_GREENROOF_PGD%XALBNIR_DRY)
NULLIFY(YTEB_GREENROOF_PGD%XALBVIS_DRY)
NULLIFY(YTEB_GREENROOF_PGD%XALBUV_DRY)
NULLIFY(YTEB_GREENROOF_PGD%XALBNIR_WET)
NULLIFY(YTEB_GREENROOF_PGD%XALBVIS_WET)
NULLIFY(YTEB_GREENROOF_PGD%XALBUV_WET)
NULLIFY(YTEB_GREENROOF_PGD%XALBNIR_SOIL)
NULLIFY(YTEB_GREENROOF_PGD%XALBVIS_SOIL)
NULLIFY(YTEB_GREENROOF_PGD%XALBUV_SOIL)
NULLIFY(YTEB_GREENROOF_PGD%XZ0_O_Z0H)
NULLIFY(YTEB_GREENROOF_PGD%XALBNIR_VEG)
NULLIFY(YTEB_GREENROOF_PGD%XALBVIS_VEG)
NULLIFY(YTEB_GREENROOF_PGD%XALBUV_VEG)
NULLIFY(YTEB_GREENROOF_PGD%XWRMAX_CF)
NULLIFY(YTEB_GREENROOF_PGD%XRSMIN)
NULLIFY(YTEB_GREENROOF_PGD%XGAMMA)
NULLIFY(YTEB_GREENROOF_PGD%XCV)
NULLIFY(YTEB_GREENROOF_PGD%XRGL)
NULLIFY(YTEB_GREENROOF_PGD%XROOTFRAC)
NULLIFY(YTEB_GREENROOF_PGD%XABC)
NULLIFY(YTEB_GREENROOF_PGD%XPOI)
NULLIFY(YTEB_GREENROOF_PGD%XBSLAI)
NULLIFY(YTEB_GREENROOF_PGD%XLAIMIN)
NULLIFY(YTEB_GREENROOF_PGD%XSEFOLD)
NULLIFY(YTEB_GREENROOF_PGD%XH_TREE)
NULLIFY(YTEB_GREENROOF_PGD%XANF)
NULLIFY(YTEB_GREENROOF_PGD%XANMAX)
NULLIFY(YTEB_GREENROOF_PGD%XFZERO)
NULLIFY(YTEB_GREENROOF_PGD%XEPSO)
NULLIFY(YTEB_GREENROOF_PGD%XGAMM)
NULLIFY(YTEB_GREENROOF_PGD%XQDGAMM)
NULLIFY(YTEB_GREENROOF_PGD%XGMES)
NULLIFY(YTEB_GREENROOF_PGD%XRE25)
NULLIFY(YTEB_GREENROOF_PGD%XQDGMES)
NULLIFY(YTEB_GREENROOF_PGD%XT1GMES)
NULLIFY(YTEB_GREENROOF_PGD%XT2GMES)
NULLIFY(YTEB_GREENROOF_PGD%XAMAX)
NULLIFY(YTEB_GREENROOF_PGD%XQDAMAX)
NULLIFY(YTEB_GREENROOF_PGD%XT1AMAX)
NULLIFY(YTEB_GREENROOF_PGD%XT2AMAX)
NULLIFY(YTEB_GREENROOF_PGD%LSTRESS)
NULLIFY(YTEB_GREENROOF_PGD%XF2I)
NULLIFY(YTEB_GREENROOF_PGD%XGC)
NULLIFY(YTEB_GREENROOF_PGD%XAH)
NULLIFY(YTEB_GREENROOF_PGD%XBH)
NULLIFY(YTEB_GREENROOF_PGD%XDMAX)
NULLIFY(YTEB_GREENROOF_PGD%XCE_NITRO)
NULLIFY(YTEB_GREENROOF_PGD%XCF_NITRO)
NULLIFY(YTEB_GREENROOF_PGD%XCNA_NITRO)
NULLIFY(YTEB_GREENROOF_PGD%XBSLAI_NITRO)
NULLIFY(YTEB_GREENROOF_PGD%XOM_GR)
NULLIFY(YTEB_GREENROOF_PGD%XSAND_GR)
NULLIFY(YTEB_GREENROOF_PGD%XCLAY_GR)
NULLIFY(YTEB_GREENROOF_PGD%XRUNOFFB_GR)
NULLIFY(YTEB_GREENROOF_PGD%XWDRAIN_GR)
NULLIFY(YTEB_GREENROOF_PGD%XTAUICE)
NULLIFY(YTEB_GREENROOF_PGD%XGAMMAT)
NULLIFY(YTEB_GREENROOF_PGD%XDG)
NULLIFY(YTEB_GREENROOF_PGD%XRUNOFFD)
NULLIFY(YTEB_GREENROOF_PGD%XSOILWGHT)
NULLIFY(YTEB_GREENROOF_PGD%XDZG)
NULLIFY(YTEB_GREENROOF_PGD%XDZDIF)
NULLIFY(YTEB_GREENROOF_PGD%NWG_LAYER)
NULLIFY(YTEB_GREENROOF_PGD%XDROOT)
NULLIFY(YTEB_GREENROOF_PGD%XDG2)
NULLIFY(YTEB_GREENROOF_PGD%XPCPS)
NULLIFY(YTEB_GREENROOF_PGD%XPLVTT)
NULLIFY(YTEB_GREENROOF_PGD%XPLSTT)
NULLIFY(YTEB_GREENROOF_PGD%XC1SAT)
NULLIFY(YTEB_GREENROOF_PGD%XC2REF)
NULLIFY(YTEB_GREENROOF_PGD%XC3)
NULLIFY(YTEB_GREENROOF_PGD%XC4B)
NULLIFY(YTEB_GREENROOF_PGD%XC4REF)
NULLIFY(YTEB_GREENROOF_PGD%XACOEF)
NULLIFY(YTEB_GREENROOF_PGD%XPCOEF)
NULLIFY(YTEB_GREENROOF_PGD%XWFC)
NULLIFY(YTEB_GREENROOF_PGD%XWWILT)
NULLIFY(YTEB_GREENROOF_PGD%XWSAT)
NULLIFY(YTEB_GREENROOF_PGD%XBCOEF)
NULLIFY(YTEB_GREENROOF_PGD%XCONDSAT)
NULLIFY(YTEB_GREENROOF_PGD%XMPOTSAT)
NULLIFY(YTEB_GREENROOF_PGD%XCGSAT)
NULLIFY(YTEB_GREENROOF_PGD%XHCAPSOIL)
NULLIFY(YTEB_GREENROOF_PGD%XCONDDRY)
NULLIFY(YTEB_GREENROOF_PGD%XCONDSLD)
NULLIFY(YTEB_GREENROOF_PGD%XTDEEP)
NULLIFY(YTEB_GREENROOF_PGD%XD_ICE)
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GREENROOF_PGD_INIT


END MODULE MODD_TEB_GREENROOF_PGD_n
