!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!       ################
MODULE MODD_SPARTACUS_OPTION_n
  !     ################
  !
  !!****  *MODD_SPARTACUS_n - declaration of parameters for SPARTACUS urban radiation scheme
  !!
  !!    PURPOSE
  !!    -------
  !     Declaration of parameters for SPARTACUS urban radiation scheme
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
  !!      R. Schoetter   *Meteo France*
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original       10/2020
  !
  !*       0.   DECLARATIONS
  !             ------------
  !
  USE MODD_SURF_PAR,       ONLY : NUNDEF
  !
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
  USE PARKIND1  ,ONLY : JPRB
  !
  IMPLICIT NONE
  !
  TYPE SPARTACUS_OPTIONS_t
     !
     LOGICAL :: LDO_SW                 ! Compute shortwave fluxes?
     LOGICAL :: LDO_LW                 ! Compute longwave fluxes?
     LOGICAL :: LUSE_SW_DIRECT_ALBEDO  ! Specify ground and roof albedos separately for direct solar radiation?
     !
     LOGICAL :: LDO_VEGETATION  ! Will vegetation be represented?
     LOGICAL :: LDO_URBAN       ! Will urban areas be represented?
     !
     INTEGER :: N_VEGETATION_REGION_URBAN  ! Number of regions used to describe urban vegetation (2 needed for heterogeneity)
     INTEGER :: N_VEGETATION_REGION_FOREST ! Number of regions used to describe forests (2 needed for heterogeneity)
     !
     INTEGER :: NSW ! Number of shortwave bands
     INTEGER :: NLW ! Number of longwave bands
     !
     INTEGER :: N_STREAM_SW_URBAN  ! Number of streams per hemisphere to describe diffuse shortwave radiation, urban areas
     INTEGER :: N_STREAM_LW_URBAN  ! Number of streams per hemisphere to describe longwave radiation, urban areas
     INTEGER :: N_STREAM_SW_FOREST ! Number of streams per hemisphere to describe diffuse shortwave radiation, forests
     INTEGER :: N_STREAM_LW_FOREST ! Number of streams per hemisphere to describe longwave radiation, forests
     !
     LOGICAL :: LUSE_SYMMETRIC_VEGETATION_SCALE_URBAN  ! TRUE : Tree crowns touch each other; Eq. 20 of Hogan et al. (2018).
     ! FALSE: Tree crowns separate (shyness); Eq. 19 of Hogan et al. (2018).
     LOGICAL :: LUSE_SYMMETRIC_VEGETATION_SCALE_FOREST ! TRUE : Tree crowns touch each other; Eq. 20 of Hogan et al. (2018).
     ! FALSE: Tree crowns separate (shyness); Eq. 19 of Hogan et al. (2018).
     ! 
     REAL :: XVEGETATION_ISOLATION_FACTOR_URBAN  ! 0.0: Dense vegetation region is embedded within sparse region
     ! 1.0: Dense vegetation is in physically isolated regions
     REAL :: XVEGETATION_ISOLATION_FACTOR_FOREST ! 0.0: Dense vegetation region is embedded within sparse region
     ! 1.0: Dense vegetation is in physically isolated regions
     !
     REAL :: XMIN_VEGETATION_FRACTION ! Minimum area fraction below which a vegetation region is removed completely
     REAL :: XMIN_BUILDING_FRACTION   ! Minimum area fraction below which a building region is removed completely
     !
  END TYPE SPARTACUS_OPTIONS_t
  !
CONTAINS
  !----------------------------------------------------------------------------
  !
  SUBROUTINE SPARTACUS_OPTIONS_INIT(YSPARTACUS_OPTIONS)
    !
    TYPE(SPARTACUS_OPTIONS_t), INTENT(INOUT) :: YSPARTACUS_OPTIONS
    !
    REAL(KIND=JPRB) :: ZHOOK_HANDLE
    !
    IF (LHOOK) CALL DR_HOOK("MODD_TEB_N:SPARTACUS_OPTIONS_INIT",0,ZHOOK_HANDLE)
    !
    YSPARTACUS_OPTIONS%LDO_SW=.FALSE.
    YSPARTACUS_OPTIONS%LDO_LW=.FALSE.
    YSPARTACUS_OPTIONS%LUSE_SW_DIRECT_ALBEDO=.FALSE.
    !
    YSPARTACUS_OPTIONS%LDO_VEGETATION=.FALSE.
    YSPARTACUS_OPTIONS%LDO_URBAN=.FALSE.
    !
    YSPARTACUS_OPTIONS%N_VEGETATION_REGION_URBAN=0
    YSPARTACUS_OPTIONS%N_VEGETATION_REGION_FOREST=0
    !
    YSPARTACUS_OPTIONS%NSW=0
    YSPARTACUS_OPTIONS%NLW=0
    !
    YSPARTACUS_OPTIONS%N_STREAM_SW_URBAN=0
    YSPARTACUS_OPTIONS%N_STREAM_LW_URBAN=0
    YSPARTACUS_OPTIONS%N_STREAM_SW_FOREST=0
    YSPARTACUS_OPTIONS%N_STREAM_LW_FOREST=0
    !
    YSPARTACUS_OPTIONS%LUSE_SYMMETRIC_VEGETATION_SCALE_URBAN=.FALSE.
    YSPARTACUS_OPTIONS%LUSE_SYMMETRIC_VEGETATION_SCALE_FOREST=.FALSE.  
    !
    YSPARTACUS_OPTIONS%XVEGETATION_ISOLATION_FACTOR_URBAN=0.0
    YSPARTACUS_OPTIONS%XVEGETATION_ISOLATION_FACTOR_FOREST=0.0
    !
    YSPARTACUS_OPTIONS%XMIN_VEGETATION_FRACTION=0.0
    YSPARTACUS_OPTIONS%XMIN_BUILDING_FRACTION=0.0
    !
    IF (LHOOK) CALL DR_HOOK("MODD_TEB_N:SPARTACUS_OPTIONS_INIT",1,ZHOOK_HANDLE)
    !
  END SUBROUTINE SPARTACUS_OPTIONS_INIT
  !
  !----------------------------------------------------------------------------
  !
END MODULE MODD_SPARTACUS_OPTION_n
