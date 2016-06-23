!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODD_DATA_TEB_GARDEN_n
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
!!      Original       05/2005
!
!*       0.   DECLARATIONS
!             ------------
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE DATA_TEB_GARDEN_t
!-------------------------------------------------------------------------------
!
  REAL, POINTER, DIMENSION(:)   :: XDATA_FRAC_HVEG        ! fraction of high vegetation
  REAL, POINTER, DIMENSION(:)   :: XDATA_FRAC_LVEG        ! fraction of low  vegetation
  REAL, POINTER, DIMENSION(:)   :: XDATA_FRAC_NVEG        ! fraction of bare soil
  REAL, POINTER, DIMENSION(:,:) :: XDATA_LAI_HVEG         ! LAI      of high vegetation
  REAL, POINTER, DIMENSION(:,:) :: XDATA_LAI_LVEG         ! LAI      of low  vegetation
  REAL, POINTER, DIMENSION(:)   :: XDATA_H_HVEG           ! height of trees
!
!
! Mask and number of grid elements containing patches/tiles:
!
  REAL, POINTER, DIMENSION(:,:)  :: XDATA_VEGTYPE       ! fraction of each vegetation type for
!                                                  ! each grid mesh                          (-)
!
!-------------------------------------------------------------------------------
!
  INTEGER                       :: NTIME               ! number of time data
!                                                      ! for VEG, LAI, EMIS, Z0
!
! Input Parameters:
!
! - vegetation + bare soil:
!
  REAL, POINTER, DIMENSION(:) :: XDATA_Z0_O_Z0H         ! ratio of surface roughness lengths
!                                                      ! (momentum to heat)                      (-)
  REAL, POINTER, DIMENSION(:,:) :: XDATA_EMIS             ! surface emissivity                      (-)
  REAL, POINTER, DIMENSION(:,:) :: XDATA_Z0               ! surface roughness length                (m)
!
! - vegetation:
!
  REAL, POINTER, DIMENSION(:) :: XDATA_ALBNIR_VEG       ! vegetation near-infra-red albedo        (-)
  REAL, POINTER, DIMENSION(:) :: XDATA_ALBVIS_VEG       ! vegetation visible albedo               (-)
  REAL, POINTER, DIMENSION(:) :: XDATA_ALBUV_VEG        ! vegetation UV albedo                    (-)
!
! - vegetation: default option (Jarvis) and general parameters:
!
  REAL, POINTER, DIMENSION(:,:) :: XDATA_VEG            ! vegetation cover fraction               (-)
  REAL, POINTER, DIMENSION(:) :: XDATA_WRMAX_CF         ! coefficient for maximum water 
!                                                      ! interception 
!                                                      ! storage capacity on the vegetation      (-)
  REAL, POINTER, DIMENSION(:) :: XDATA_RSMIN            ! minimum stomatal resistance             (s/m)
  REAL, POINTER, DIMENSION(:) :: XDATA_GAMMA            ! coefficient for the calculation
!                                                      ! of the surface stomatal
!                                                      ! resistance
  REAL, POINTER, DIMENSION(:) :: XDATA_CV               ! vegetation thermal inertia coefficient  (K m2/J)
  REAL, POINTER, DIMENSION(:) :: XDATA_RGL              ! maximum solar radiation
!                                                      ! usable in photosynthesis                (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XDATA_ROOTFRAC       ! root fraction profile ('DIF' option)
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags parameters ('AGS', 'LAI', 'AST', 'LST', 'NIT', 'NCB' options)
!
  REAL, POINTER, DIMENSION(:)    :: XDATA_BSLAI        ! ratio d(biomass)/d(lai)                 (kg/m2)
  REAL, POINTER, DIMENSION(:)    :: XDATA_LAIMIN       ! minimum LAI (Leaf Area Index)           (m2/m2)
  REAL, POINTER, DIMENSION(:)    :: XDATA_SEFOLD       ! e-folding time for senescence           (s)
  REAL, POINTER, DIMENSION(:)    :: XDATA_H_TREE       ! height of trees                         (m)
  REAL, POINTER, DIMENSION(:)    :: XDATA_GMES         ! mesophyll conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:)    :: XDATA_RE25         ! Ecosystem respiration parameter         (kg m2 s-1)
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Stress parameters ('AST', 'LST', 'NIT', 'NCB' options)
!
  LOGICAL, POINTER, DIMENSION(:) :: LDATA_STRESS       ! vegetation response type to water
!                                                     ! stress (true:defensive false:offensive) (-)
  REAL, POINTER, DIMENSION(:)    :: XDATA_F2I          ! critical normilized soil water 
!                                                     ! content for stress parameterisation
  REAL, POINTER, DIMENSION(:)    :: XDATA_GC           ! cuticular conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:)    :: XDATA_DMAX         ! maximum air saturation deficit
!                                                     ! tolerate by vegetation                  (kg/kg)
!
  REAL, POINTER, DIMENSION(:)    :: XDATA_BSLAI_ST     ! ratio d(biomass)/d(lai)                 (kg/m2)
  REAL, POINTER, DIMENSION(:)    :: XDATA_SEFOLD_ST    ! e-folding time for senescence           (s)
  REAL, POINTER, DIMENSION(:)    :: XDATA_GMES_ST      ! mesophyll conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:)    :: XDATA_GC_ST        ! cuticular conductance                   (m s-1)
  REAL, POINTER, DIMENSION(:)    :: XDATA_DMAX_ST      ! maximum air saturation deficit
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Nitrogen-model parameters ('NIT', 'NCB' option)
!
  REAL, POINTER, DIMENSION(:)    :: XDATA_CE_NITRO       ! leaf aera ratio sensitivity to 
!                                                       ! nitrogen concentration                (m2/kg)
  REAL, POINTER, DIMENSION(:)    :: XDATA_CF_NITRO       ! lethal minimum value of leaf area
!                                                       ! ratio                                 (m2/kg)
  REAL, POINTER, DIMENSION(:)    :: XDATA_CNA_NITRO      ! nitrogen concentration of active 
!                                                       ! biomass                               (kg/kg)
!
!-------------------------------------------------------------------------------
!
! - soil: primary parameters
!
  REAL, POINTER, DIMENSION(:,:)  :: XDATA_DG            ! soil layer thicknesses                  (m)
!                                                      ! NOTE: in Force-Restore mode, the 
!                                                      ! uppermost layer thickness is superficial
!                                                      ! and is only explicitly used for soil 
!                                                      ! water phase changes                     (m)
!
  REAL, POINTER,DIMENSION(:)     :: XDATA_DICE       ! depth of the soil column for the calculation
!                                                        of the frozen soil fraction (m)
!
! - bare soil albedo
!
  REAL, POINTER, DIMENSION(:)   :: XDATA_ALBNIR_SOIL      ! soil near-infra-red albedo        (-)
  REAL, POINTER, DIMENSION(:)   :: XDATA_ALBVIS_SOIL      ! soil visible albedo               (-)
  REAL, POINTER, DIMENSION(:)   :: XDATA_ALBUV_SOIL       ! soil UV albedo                    (-)
  REAL, POINTER, DIMENSION(:)   :: XDATA_ALBNIR_DRY       ! dry soil near-infra-red albedo    (-)
  REAL, POINTER, DIMENSION(:)   :: XDATA_ALBVIS_DRY       ! dry soil visible albedo           (-)
  REAL, POINTER, DIMENSION(:)   :: XDATA_ALBUV_DRY        ! dry soil UV albedo                (-)
  REAL, POINTER, DIMENSION(:)   :: XDATA_ALBNIR_WET       ! wet soil near-infra-red albedo    (-)
  REAL, POINTER, DIMENSION(:)   :: XDATA_ALBVIS_WET       ! wet soil visible albedo           (-)
  REAL, POINTER, DIMENSION(:)   :: XDATA_ALBUV_WET        ! wet soil UV albedo                (-)
!
!-------------------------------------------------------------------------------
!
!- Vegetation: Ags Prognostic (YPHOTO = ('LAI', 'LST', 'NIT', or 'NCB') or prescribed (YPHOTO='NON', 'AGS' or 'AST')
!
  REAL, POINTER, DIMENSION(:,:)     :: XDATA_LAI          ! Leaf Area Index                         (m2/m2)
!
!-------------------------------------------------------------------------------
!

END TYPE DATA_TEB_GARDEN_t



 CONTAINS

!




SUBROUTINE DATA_TEB_GARDEN_INIT(YDATA_TEB_GARDEN)
TYPE(DATA_TEB_GARDEN_t), INTENT(INOUT) :: YDATA_TEB_GARDEN
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DATA_TEB_GARDEN_N:DATA_TEB_GARDEN_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_FRAC_HVEG)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_FRAC_LVEG)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_FRAC_NVEG)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_LAI_HVEG)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_LAI_LVEG)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_H_HVEG)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_VEGTYPE)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_Z0_O_Z0H)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_EMIS)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_Z0)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_ALBNIR_VEG)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_ALBVIS_VEG)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_ALBUV_VEG)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_VEG)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_WRMAX_CF)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_RSMIN)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_GAMMA)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_CV)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_RGL)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_ROOTFRAC)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_BSLAI)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_LAIMIN)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_SEFOLD)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_H_TREE)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_GMES)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_RE25)
  NULLIFY(YDATA_TEB_GARDEN%LDATA_STRESS)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_F2I)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_GC)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_DMAX)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_BSLAI_ST)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_SEFOLD_ST)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_GMES_ST)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_GC_ST)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_DMAX_ST)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_CE_NITRO)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_CF_NITRO)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_CNA_NITRO)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_DG)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_DICE)  
  NULLIFY(YDATA_TEB_GARDEN%XDATA_ALBNIR_SOIL)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_ALBVIS_SOIL)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_ALBUV_SOIL)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_ALBNIR_DRY)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_ALBVIS_DRY)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_ALBUV_DRY)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_ALBNIR_WET)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_ALBVIS_WET)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_ALBUV_WET)
  NULLIFY(YDATA_TEB_GARDEN%XDATA_LAI)
YDATA_TEB_GARDEN%NTIME=0
IF (LHOOK) CALL DR_HOOK("MODD_DATA_TEB_GARDEN_N:DATA_TEB_GARDEN_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DATA_TEB_GARDEN_INIT


END MODULE MODD_DATA_TEB_GARDEN_n
