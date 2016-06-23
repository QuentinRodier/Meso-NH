!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODD_DATA_TEB_GREENROOF_n
!     ##################
!
!!****  *MODD_DATA_ISBA - declaration of DATA surface parameters for ISBA scheme
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
!!      V. Masson  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original                    05/2005
!!      A. Lemonsu / C. de Munck     04/2011  : TEB GreenRoof
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE DATA_TEB_GREENROOF_t
!-------------------------------------------------------------------------------
!

  REAL, POINTER, DIMENSION(:,:) :: XPAR_OM_GR           ! fraction of organic matter (OM) in green roof layer
  REAL, POINTER, DIMENSION(:,:) :: XPAR_CLAY_GR         ! fraction of clay for the non-OM part of the green roof layer
  REAL, POINTER, DIMENSION(:,:) :: XPAR_SAND_GR         ! fraction of sand for the non-OM part of the green roof layer
  REAL, POINTER, DIMENSION(:,:) :: XPAR_LAI_GR          ! LAI of green roof vegetation
!
!
! Mask and number of grid elements containing patches/tiles:
!
  REAL, POINTER, DIMENSION(:,:)  :: XPAR_VEGTYPE        ! fraction of each vegetation type for
                                                        ! each grid mesh                          (-)
!
!-------------------------------------------------------------------------------
!
! Input Parameters, per patch:
!
! - vegetation + bare soil:
!
  REAL, POINTER, DIMENSION(:)   :: XPAR_Z0_O_Z0H        ! ratio of surface roughness lengths
                                                        ! (momentum to heat)                      (-)
  REAL, POINTER, DIMENSION(:,:)   :: XPAR_EMIS          ! surface emissivity                      (-)
  REAL, POINTER, DIMENSION(:,:)   :: XPAR_Z0            ! surface roughness length                (m)
!
! - vegetation:
!
  REAL, POINTER, DIMENSION(:)   :: XPAR_ALBNIR_VEG       ! vegetation near-infra-red albedo        (-)
  REAL, POINTER, DIMENSION(:)   :: XPAR_ALBVIS_VEG       ! vegetation visible albedo               (-)
  REAL, POINTER, DIMENSION(:)   :: XPAR_ALBUV_VEG        ! vegetation UV albedo                    (-)
!
! - vegetation: default option (Jarvis) and general parameters:
!
  REAL, POINTER, DIMENSION(:,:) :: XPAR_VEG              ! vegetation cover fraction               (-)
  REAL, POINTER, DIMENSION(:)   :: XPAR_WRMAX_CF         ! coefficient for maximum water 
                                                         ! interception 
                                                         ! storage capacity on the vegetation      (-)
  REAL, POINTER, DIMENSION(:)   :: XPAR_RSMIN            ! minimum stomatal resistance             (s/m)
  REAL, POINTER, DIMENSION(:)   :: XPAR_GAMMA            ! coefficient for the calculation
                                                         ! of the surface stomatal
                                                         ! resistance
  REAL, POINTER, DIMENSION(:)   :: XPAR_CV               ! vegetation thermal inertia coefficient  (K m2/J)
  REAL, POINTER, DIMENSION(:)   :: XPAR_RGL              ! maximum solar radiation
                                                         ! usable in photosynthesis                (W/m2)
  REAL, POINTER, DIMENSION(:,:)   :: XPAR_ROOTFRAC       ! root fraction profile ('DIF' option)
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags parameters ('AGS', 'LAI', 'AST', 'LST', 'NIT', 'NCB' options)
!
  REAL, POINTER, DIMENSION(:)      :: XPAR_BSLAI         ! ratio d(biomass)/d(lai)                 (kg/m2)
  REAL, POINTER, DIMENSION(:)      :: XPAR_LAIMIN        ! minimum LAI (Leaf Area Index)           (m2/m2)
  REAL, POINTER, DIMENSION(:)      :: XPAR_SEFOLD        ! e-folding time for senescence           (s)
  REAL, POINTER, DIMENSION(:)      :: XPAR_H_TREE        ! height of trees                         (m)
  REAL, POINTER, DIMENSION(:)      :: XPAR_GMES          ! mesophyll conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:)      :: XPAR_RE25          ! Ecosystem respiration parameter         (kg m2 s-1)
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Stress parameters ('AST', 'LST', 'NIT', 'NCB' options)
!
  LOGICAL, POINTER, DIMENSION(:)   :: LDATA_STRESS       ! vegetation response type to water
                                                         ! stress (true:defensive false:offensive) (-)
  REAL, POINTER, DIMENSION(:)      :: XPAR_F2I           ! critical normilized soil water 
                                                         ! content for stress parameterisation
  REAL, POINTER, DIMENSION(:)      :: XPAR_GC            ! cuticular conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:)      :: XPAR_DMAX          ! maximum air saturation deficit
                                                         ! tolerate by vegetation                  (kg/kg)
!
  REAL, POINTER, DIMENSION(:)      :: XPAR_BSLAI_ST      ! ratio d(biomass)/d(lai)                 (kg/m2)
  REAL, POINTER, DIMENSION(:)      :: XPAR_SEFOLD_ST     ! e-folding time for senescence           (s)
  REAL, POINTER, DIMENSION(:)      :: XPAR_GMES_ST       ! mesophyll conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:)      :: XPAR_GC_ST         ! cuticular conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:)      :: XPAR_DMAX_ST       ! maximum air saturation deficit
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Nitrogen-model parameters ('NIT', 'NCB' option)
!
  REAL, POINTER, DIMENSION(:)      :: XPAR_CE_NITRO      ! leaf aera ratio sensitivity to 
                                                         ! nitrogen concentration                (m2/kg)
  REAL, POINTER, DIMENSION(:)      :: XPAR_CF_NITRO      ! lethal minimum value of leaf area
                                                         ! ratio                                 (m2/kg)
  REAL, POINTER, DIMENSION(:)      :: XPAR_CNA_NITRO     ! nitrogen concentration of active 
                                                         ! biomass                               (kg/kg)
!
!-------------------------------------------------------------------------------
!
! - soil: primary parameters
!
  REAL, POINTER, DIMENSION(:,:)    :: XPAR_DG            ! soil layer thicknesses                  (m)
                                                         ! NOTE: in Force-Restore mode, the 
                                                         ! uppermost layer thickness is superficial
                                                         ! and is only explicitly used for soil 
                                                         ! water phase changes                     (m)
!
  REAL, POINTER,DIMENSION(:)       :: XPAR_DICE          ! depth of the soil column for the calculation
                                                         ! of the frozen soil fraction (m)
!
! - bare soil albedo
!
  REAL, POINTER, DIMENSION(:)   :: XPAR_ALBNIR_SOIL      ! soil near-infra-red albedo        (-)
  REAL, POINTER, DIMENSION(:)   :: XPAR_ALBVIS_SOIL      ! soil visible albedo               (-)
  REAL, POINTER, DIMENSION(:)   :: XPAR_ALBUV_SOIL       ! soil UV albedo                    (-)
  REAL, POINTER, DIMENSION(:)   :: XPAR_ALBNIR_DRY       ! dry soil near-infra-red albedo    (-)
  REAL, POINTER, DIMENSION(:)   :: XPAR_ALBVIS_DRY       ! dry soil visible albedo           (-)
  REAL, POINTER, DIMENSION(:)   :: XPAR_ALBUV_DRY        ! dry soil UV albedo                (-)
  REAL, POINTER, DIMENSION(:)   :: XPAR_ALBNIR_WET       ! wet soil near-infra-red albedo    (-)
  REAL, POINTER, DIMENSION(:)   :: XPAR_ALBVIS_WET       ! wet soil visible albedo           (-)
  REAL, POINTER, DIMENSION(:)   :: XPAR_ALBUV_WET        ! wet soil UV albedo                (-)
!
!-------------------------------------------------------------------------------
!
!- Vegetation: Ags Prognostic (YPHOTO = ('LAI', 'LST', 'NIT', or 'NCB') or prescribed (YPHOTO='NON', 'AGS' or 'AST')
!
  REAL, POINTER, DIMENSION(:,:)     :: XPAR_LAI          ! Leaf Area Index                         (m2/m2)
!
!-------------------------------------------------------------------------------
!

END TYPE DATA_TEB_GREENROOF_t



 CONTAINS

!




SUBROUTINE DATA_TEB_GREENROOF_INIT(YDATA_TEB_GREENROOF)
TYPE(DATA_TEB_GREENROOF_t), INTENT(INOUT) :: YDATA_TEB_GREENROOF
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DATA_TEB_GREENROOF_N:DATA_TEB_GREENROOF_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_OM_GR)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_CLAY_GR)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_SAND_GR)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_LAI_GR)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_VEGTYPE)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_Z0_O_Z0H)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_EMIS)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_Z0)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_ALBNIR_VEG)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_ALBVIS_VEG)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_ALBUV_VEG)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_VEG)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_WRMAX_CF)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_RSMIN)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_GAMMA)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_CV)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_RGL)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_ROOTFRAC)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_BSLAI)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_LAIMIN)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_SEFOLD)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_H_TREE)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_GMES)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_RE25)
  NULLIFY(YDATA_TEB_GREENROOF%LDATA_STRESS)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_F2I)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_GC)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_DMAX)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_BSLAI_ST)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_SEFOLD_ST)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_GMES_ST)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_GC_ST)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_DMAX_ST)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_CE_NITRO)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_CF_NITRO)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_CNA_NITRO)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_DG)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_DICE)  
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_ALBNIR_SOIL)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_ALBVIS_SOIL)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_ALBUV_SOIL)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_ALBNIR_DRY)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_ALBVIS_DRY)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_ALBUV_DRY)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_ALBNIR_WET)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_ALBVIS_WET)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_ALBUV_WET)
  NULLIFY(YDATA_TEB_GREENROOF%XPAR_LAI)
IF (LHOOK) CALL DR_HOOK("MODD_DATA_TEB_GREENROOF_N:DATA_TEB_GREENROOF_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DATA_TEB_GREENROOF_INIT


END MODULE MODD_DATA_TEB_GREENROOF_n
