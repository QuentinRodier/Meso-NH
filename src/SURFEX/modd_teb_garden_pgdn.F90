!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!##################
MODULE MODD_TEB_GARDEN_PGD_n
!##################
!
!!****  *MODD_TEB_GARDEN - declaration of packed surface parameters for ISBA scheme
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
!!      A. Lemonsu   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2011
!!      V. Masson      06/2013 splits module in 4
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

!-------------------------------------------------------------------------------
TYPE TEB_GARDEN_PGD_t
!-------------------------------------------------------------------------------
!
! Mask and number of grid elements containing patches/tiles:
!
  REAL, POINTER, DIMENSION(:,:) :: XVEGTYPE          ! fraction of each vegetation type for
!                                                    ! each grid mesh                          (-)
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
  REAL, POINTER, DIMENSION(:) :: XZ0_O_Z0H         ! ratio of surface roughness lengths
!                                                    ! (momentum to heat)                      (-)
!
! - vegetation:
!
  REAL, POINTER, DIMENSION(:) :: XALBNIR_VEG       ! vegetation near-infra-red albedo        (-)
  REAL, POINTER, DIMENSION(:) :: XALBVIS_VEG       ! vegetation visible albedo               (-)
  REAL, POINTER, DIMENSION(:) :: XALBUV_VEG        ! vegetation UV albedo                    (-)
!
! - vegetation: default option (Jarvis) and general parameters:
!
  REAL, POINTER, DIMENSION(:) :: XWRMAX_CF         ! coefficient for maximum water 
!                                                      ! interception 
!                                                      ! storage capacity on the vegetation      (-)
  REAL, POINTER, DIMENSION(:) :: XRSMIN            ! minimum stomatal resistance             (s/m)
  REAL, POINTER, DIMENSION(:) :: XGAMMA            ! coefficient for the calculation
!                                                      ! of the surface stomatal
!                                                      ! resistance
  REAL, POINTER, DIMENSION(:) :: XCV               ! vegetation thermal inertia coefficient  (K m2/J)
  REAL, POINTER, DIMENSION(:) :: XRGL              ! maximum solar radiation
!                                                      ! usable in photosynthesis                (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XROOTFRAC       ! root fraction profile ('DIF' option)
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags parameters ('AGS', 'LAI', 'AST', 'LST', 'NIT', 'NCB' options)
!
!  REAL,              DIMENSION(3)   :: XABC             ! abscissa needed for integration
  REAL, POINTER,     DIMENSION(:)   :: XABC       ! abscissa needed for integration
!                                                 ! of net assimilation and stomatal
!                                                 ! conductance over canopy depth           (-)
!  REAL,              DIMENSION(3)   :: XPOI             ! Gaussian weights for integration
  REAL, POINTER,     DIMENSION(:)   :: XPOI       ! Gaussian weights for integration
!                                                 ! of net assimilation and stomatal
!                                                 ! conductance over canopy depth           (-)
  REAL, POINTER, DIMENSION(:)    :: XBSLAI        ! ratio d(biomass)/d(lai)                 (kg/m2)
  REAL, POINTER, DIMENSION(:)    :: XLAIMIN       ! minimum LAI (Leaf Area Index)           (m2/m2)
  REAL, POINTER, DIMENSION(:)    :: XSEFOLD       ! e-folding time for senescence           (s)
  REAL, POINTER, DIMENSION(:)    :: XH_TREE       ! height of trees                         (m)
  REAL, POINTER, DIMENSION(:)    :: XANF          ! total assimilation over canopy          (
  REAL, POINTER, DIMENSION(:)    :: XANMAX        ! maximum photosynthesis rate             (
  REAL, POINTER, DIMENSION(:)    :: XFZERO        ! ideal value of F, no photo- 
!                                                     ! respiration or saturation deficit       (
  REAL, POINTER, DIMENSION(:)    :: XEPSO         ! maximum initial quantum use             
!                                                     ! efficiency                              (mg J-1 PAR)
  REAL, POINTER, DIMENSION(:)    :: XGAMM         ! CO2 conpensation concentration          (ppm)
  REAL, POINTER, DIMENSION(:)    :: XQDGAMM       ! Log of Q10 function for CO2 conpensation 
!                                                 ! concentration                           (-)
  REAL, POINTER, DIMENSION(:)    :: XGMES         ! mesophyll conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:)    :: XRE25         ! Ecosystem respiration parameter         (kg/kg.m.s-1)
  REAL, POINTER, DIMENSION(:)    :: XQDGMES       ! Log of Q10 function for mesophyll conductance  (-)
  REAL, POINTER, DIMENSION(:)    :: XT1GMES       ! reference temperature for computing 
!                                                     ! compensation concentration function for 
!                                                     ! mesophyll conductance: minimum
!                                                     ! temperature                             (K)
  REAL, POINTER, DIMENSION(:)    :: XT2GMES       ! reference temperature for computing 
!                                                     ! compensation concentration function for 
!                                                     ! mesophyll conductance: maximum
!                                                     ! temperature                             (K)
  REAL, POINTER, DIMENSION(:)    :: XAMAX         ! leaf photosynthetic capacity            (mg m-2 s-1)
  REAL, POINTER, DIMENSION(:)    :: XQDAMAX       ! Log of Q10 function for leaf photosynthetic 
!                                                     ! capacity                                (-)
  REAL, POINTER, DIMENSION(:)    :: XT1AMAX       ! reference temperature for computing 
!                                                     ! compensation concentration function for 
!                                                     ! leaf photosynthetic capacity: minimum
!                                                     ! temperature                             (K)
  REAL, POINTER, DIMENSION(:)    :: XT2AMAX       ! reference temperature for computing 
!                                                     ! compensation concentration function for 
!                                                     ! leaf photosynthetic capacity: maximum
!                                                     ! temperature                             (K)
!                                      

!-------------------------------------------------------------------------------
!
! - vegetation: Ags Stress parameters ('AST', 'LST', 'NIT', 'NCB' options)
!
  LOGICAL, POINTER, DIMENSION(:) :: LSTRESS       ! vegetation response type to water
!                                                     ! stress (true:defensive false:offensive) (-)
  REAL, POINTER, DIMENSION(:)    :: XF2I          ! critical normilized soil water 
!                                                     ! content for stress parameterisation
  REAL, POINTER, DIMENSION(:)    :: XGC           ! cuticular conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:)    :: XAH           ! coefficients for herbaceous water stress 
!                                                     ! response (offensive or defensive)       (log(mm/s))
  REAL, POINTER, DIMENSION(:)    :: XBH           ! coefficients for herbaceous water stress 
!                                                     ! response (offensive or defensive)       (-)
  REAL, POINTER, DIMENSION(:)    :: XDMAX         ! maximum air saturation deficit
!                                                     ! tolerate by vegetation                  (kg/kg)
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Nitrogen-model parameters ('NIT', 'NCB' option)
!
  REAL, POINTER, DIMENSION(:)    :: XCE_NITRO       ! leaf aera ratio sensitivity to 
!                                                       ! nitrogen concentration                (m2/kg)
  REAL, POINTER, DIMENSION(:)    :: XCF_NITRO       ! lethal minimum value of leaf area
!                                                       ! ratio                                 (m2/kg)
  REAL, POINTER, DIMENSION(:)    :: XCNA_NITRO      ! nitrogen concentration of active 
!                                                       ! biomass                               (kg/kg)
  REAL, POINTER, DIMENSION(:)    :: XBSLAI_NITRO   ! biomass/LAI ratio from nitrogen 
!                                                       ! decline theory                        (kg/m2)
!
!-------------------------------------------------------------------------------
!
! - soil: primary parameters
!
  REAL, POINTER, DIMENSION(:,:)    :: XSAND          ! sand fraction                           (-)
  REAL, POINTER, DIMENSION(:,:)    :: XCLAY          ! clay fraction                           (-)
  REAL, POINTER, DIMENSION(:)      :: XRUNOFFB       ! sub-grid surface runoff slope parameter (-)
  REAL, POINTER, DIMENSION(:)      :: XWDRAIN        ! continuous drainage parameter           (-)
  REAL, POINTER, DIMENSION(:)      :: XTAUICE        ! soil freezing characteristic timescale  (s)
  REAL, POINTER, DIMENSION(:)      :: XGAMMAT        ! 'Force-Restore' timescale when using a
!                                                    ! prescribed lower boundary temperature   (1/days)
  REAL, POINTER, DIMENSION(:,:)    :: XDG            ! soil layer thicknesses                  (m)
!                                                    ! NOTE: in Force-Restore mode, the 
!                                                    ! uppermost layer thickness is superficial
!                                                    ! and is only explicitly used for soil 
!                                                    ! water phase changes                     (m)
  REAL, POINTER, DIMENSION(:)      :: XRUNOFFD       ! depth over which sub-grid runoff is
!                                                    ! computed: in Force-Restore this is the
!                                                    ! total soil column ('2-L'), or root zone
!                                                    ! ('3-L'). For the 'DIF' option, it can
!                                                    ! be any depth within soil column         (m)
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
  REAL, POINTER, DIMENSION(:)    :: XC1SAT         ! 'Force-Restore' C1 coefficient at 
!                                                    ! saturation                              (-)
  REAL, POINTER, DIMENSION(:)    :: XC2REF         ! 'Force-Restore' reference value of C2   (-)
  REAL, POINTER, DIMENSION(:,:)  :: XC3            ! 'Force-Restore' C3 drainage coefficient (m)
  REAL, POINTER, DIMENSION(:)      :: XC4B           ! 'Force-Restore' sub-surface vertical 
!                                                    ! diffusion coefficient (slope parameter) (-)
  REAL, POINTER, DIMENSION(:)    :: XC4REF         ! 'Force-Restore' sub-surface vertical 
!                                                    ! diffusion coefficient                   (-)
  REAL, POINTER, DIMENSION(:)      :: XACOEF         ! 'Force-Restore' surface vertical 
!                                                    ! diffusion coefficient                   (-)
  REAL, POINTER, DIMENSION(:)      :: XPCOEF         ! 'Force-Restore' surface vertical 
!                                                    ! diffusion coefficient                   (-)
  REAL, POINTER, DIMENSION(:,:)    :: XWFC           ! field capacity volumetric water content
!                                                      ! profile                                 (m3/m3)
  REAL, POINTER, DIMENSION(:,:)    :: XWWILT         ! wilting point volumetric water content 
!                                                      ! profile                                 (m3/m3)
  REAL, POINTER, DIMENSION(:,:)    :: XWSAT          ! porosity profile                        (m3/m3) 
  REAL, POINTER, DIMENSION(:,:)    :: XBCOEF         ! soil water CH78 b-parameter             (-)
  REAL, POINTER, DIMENSION(:,:)  :: XCONDSAT       ! hydraulic conductivity at saturation    (m/s)
  REAL, POINTER, DIMENSION(:,:)  :: XMPOTSAT       ! matric potential at saturation          (m)
!
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
!                                                    ! (optional)                              (K) 
  REAL, POINTER, DIMENSION(:)     :: XPCPS
  REAL, POINTER, DIMENSION(:)     :: XPLVTT
  REAL, POINTER, DIMENSION(:)     :: XPLSTT 
!-------------------------------------------------------------------------------
!
! - SGH scheme
!                                                     
  REAL, POINTER, DIMENSION(:)  :: XD_ICE    !depth of the soil column for the calculation
!                                              of the frozen soil fraction (m)
  REAL, POINTER, DIMENSION(:)  :: XKSAT_ICE !hydraulic conductivity at saturation
!                                              over frozen area (m s-1)                                     
!-------------------------------------------------------------------------------
!
! Type of vegetation (simplification of vegetation charaterization)
 CHARACTER(LEN=4)             :: CTYPE_HVEG   ! type of high vegetation
 CHARACTER(LEN=4)             :: CTYPE_LVEG   ! type of low vegetation
 CHARACTER(LEN=4)             :: CTYPE_NVEG   ! type of bare soil (no vegetation)
!-------------------------------------------------------------------------------
!
END TYPE TEB_GARDEN_PGD_t
!



 CONTAINS

!


!

SUBROUTINE TEB_GARDEN_PGD_INIT(YTEB_GARDEN_PGD)
TYPE(TEB_GARDEN_PGD_t), INTENT(INOUT) :: YTEB_GARDEN_PGD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GARDEN_PGD_N:TEB_GARDEN_PGD_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YTEB_GARDEN_PGD%XVEGTYPE)
  NULLIFY(YTEB_GARDEN_PGD%XALBNIR_DRY)
  NULLIFY(YTEB_GARDEN_PGD%XALBVIS_DRY)
  NULLIFY(YTEB_GARDEN_PGD%XALBUV_DRY)
  NULLIFY(YTEB_GARDEN_PGD%XALBNIR_WET)
  NULLIFY(YTEB_GARDEN_PGD%XALBVIS_WET)
  NULLIFY(YTEB_GARDEN_PGD%XALBUV_WET)
  NULLIFY(YTEB_GARDEN_PGD%XALBNIR_SOIL)
  NULLIFY(YTEB_GARDEN_PGD%XALBVIS_SOIL)
  NULLIFY(YTEB_GARDEN_PGD%XALBUV_SOIL)
  NULLIFY(YTEB_GARDEN_PGD%XZ0_O_Z0H)
  NULLIFY(YTEB_GARDEN_PGD%XALBNIR_VEG)
  NULLIFY(YTEB_GARDEN_PGD%XALBVIS_VEG)
  NULLIFY(YTEB_GARDEN_PGD%XALBUV_VEG)
  NULLIFY(YTEB_GARDEN_PGD%XWRMAX_CF)
  NULLIFY(YTEB_GARDEN_PGD%XRSMIN)
  NULLIFY(YTEB_GARDEN_PGD%XGAMMA)
  NULLIFY(YTEB_GARDEN_PGD%XCV)
  NULLIFY(YTEB_GARDEN_PGD%XRGL)
  NULLIFY(YTEB_GARDEN_PGD%XROOTFRAC)
  NULLIFY(YTEB_GARDEN_PGD%XBSLAI)
  NULLIFY(YTEB_GARDEN_PGD%XLAIMIN)
  NULLIFY(YTEB_GARDEN_PGD%XSEFOLD)
  NULLIFY(YTEB_GARDEN_PGD%XH_TREE)
  NULLIFY(YTEB_GARDEN_PGD%XANF)
  NULLIFY(YTEB_GARDEN_PGD%XANMAX)
  NULLIFY(YTEB_GARDEN_PGD%XFZERO)
  NULLIFY(YTEB_GARDEN_PGD%XEPSO)
  NULLIFY(YTEB_GARDEN_PGD%XGAMM)
  NULLIFY(YTEB_GARDEN_PGD%XQDGAMM)
  NULLIFY(YTEB_GARDEN_PGD%XGMES)
  NULLIFY(YTEB_GARDEN_PGD%XRE25)
  NULLIFY(YTEB_GARDEN_PGD%XQDGMES)
  NULLIFY(YTEB_GARDEN_PGD%XT1GMES)
  NULLIFY(YTEB_GARDEN_PGD%XT2GMES)
  NULLIFY(YTEB_GARDEN_PGD%XAMAX)
  NULLIFY(YTEB_GARDEN_PGD%XQDAMAX)
  NULLIFY(YTEB_GARDEN_PGD%XT1AMAX)
  NULLIFY(YTEB_GARDEN_PGD%XT2AMAX)
  NULLIFY(YTEB_GARDEN_PGD%LSTRESS)
  NULLIFY(YTEB_GARDEN_PGD%XF2I)
  NULLIFY(YTEB_GARDEN_PGD%XGC)
  NULLIFY(YTEB_GARDEN_PGD%XAH)
  NULLIFY(YTEB_GARDEN_PGD%XBH)
  NULLIFY(YTEB_GARDEN_PGD%XDMAX)
  NULLIFY(YTEB_GARDEN_PGD%XCE_NITRO)
  NULLIFY(YTEB_GARDEN_PGD%XCF_NITRO)
  NULLIFY(YTEB_GARDEN_PGD%XCNA_NITRO)
  NULLIFY(YTEB_GARDEN_PGD%XBSLAI_NITRO)
  NULLIFY(YTEB_GARDEN_PGD%XSAND)
  NULLIFY(YTEB_GARDEN_PGD%XCLAY)
  NULLIFY(YTEB_GARDEN_PGD%XRUNOFFB)
  NULLIFY(YTEB_GARDEN_PGD%XWDRAIN)
  NULLIFY(YTEB_GARDEN_PGD%XTAUICE)
  NULLIFY(YTEB_GARDEN_PGD%XGAMMAT)
  NULLIFY(YTEB_GARDEN_PGD%XDG)
  NULLIFY(YTEB_GARDEN_PGD%XRUNOFFD)
  NULLIFY(YTEB_GARDEN_PGD%XSOILWGHT)
  NULLIFY(YTEB_GARDEN_PGD%XDZG)
  NULLIFY(YTEB_GARDEN_PGD%XDZDIF)
  NULLIFY(YTEB_GARDEN_PGD%NWG_LAYER)
  NULLIFY(YTEB_GARDEN_PGD%XDROOT)
  NULLIFY(YTEB_GARDEN_PGD%XDG2)  
  NULLIFY(YTEB_GARDEN_PGD%XPCPS)
  NULLIFY(YTEB_GARDEN_PGD%XPLVTT)
  NULLIFY(YTEB_GARDEN_PGD%XPLSTT)  
  NULLIFY(YTEB_GARDEN_PGD%XC1SAT)
  NULLIFY(YTEB_GARDEN_PGD%XC2REF)
  NULLIFY(YTEB_GARDEN_PGD%XC3)
  NULLIFY(YTEB_GARDEN_PGD%XC4B)
  NULLIFY(YTEB_GARDEN_PGD%XC4REF)
  NULLIFY(YTEB_GARDEN_PGD%XACOEF)
  NULLIFY(YTEB_GARDEN_PGD%XPCOEF)
  NULLIFY(YTEB_GARDEN_PGD%XWFC)
  NULLIFY(YTEB_GARDEN_PGD%XWWILT)
  NULLIFY(YTEB_GARDEN_PGD%XWSAT)
  NULLIFY(YTEB_GARDEN_PGD%XBCOEF)
  NULLIFY(YTEB_GARDEN_PGD%XCONDSAT)
  NULLIFY(YTEB_GARDEN_PGD%XMPOTSAT)
  NULLIFY(YTEB_GARDEN_PGD%XCGSAT)
  NULLIFY(YTEB_GARDEN_PGD%XHCAPSOIL)
  NULLIFY(YTEB_GARDEN_PGD%XCONDDRY)
  NULLIFY(YTEB_GARDEN_PGD%XCONDSLD)
  NULLIFY(YTEB_GARDEN_PGD%XTDEEP)
  NULLIFY(YTEB_GARDEN_PGD%XD_ICE)
  NULLIFY(YTEB_GARDEN_PGD%XKSAT_ICE)
  NULLIFY(YTEB_GARDEN_PGD%XABC)
  NULLIFY(YTEB_GARDEN_PGD%XPOI)
YTEB_GARDEN_PGD%CTYPE_HVEG=' '
YTEB_GARDEN_PGD%CTYPE_LVEG=' '
YTEB_GARDEN_PGD%CTYPE_NVEG=' '
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GARDEN_PGD_N:TEB_GARDEN_PGD_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GARDEN_PGD_INIT


END MODULE MODD_TEB_GARDEN_PGD_n
