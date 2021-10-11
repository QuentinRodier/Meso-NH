!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
!
SUBROUTINE READ_NAM_PGD_SPARTACUS(HPROGRAM, ODO_SW, ODO_LW, &
     OUSE_SW_DIRECT_ALBEDO, ODO_VEGETATION, ODO_URBAN,      &
     K_VEGETATION_REGION_URBAN, K_VEGETATION_REGION_FOREST, &
     KSW, KLW, K_STREAM_SW_URBAN, K_STREAM_LW_URBAN,        &
     K_STREAM_SW_FOREST, K_STREAM_LW_FOREST,                &
     OUSE_SYMMETRIC_VEGETATION_SCALE_URBAN,                 &
     OUSE_SYMMETRIC_VEGETATION_SCALE_FOREST,                &  
     ZVEGETATION_ISOLATION_FACTOR_URBAN,                    &
     ZVEGETATION_ISOLATION_FACTOR_FOREST,                   &
     ZMIN_VEGETATION_FRACTION, ZMIN_BUILDING_FRACTION, HTEST)
  !
  !     ##############################################################
  !
  !!**** *READ_NAM_PGD_SPARTACUS* reads namelist for SPARTACUS
  !!
  !!    PURPOSE
  !!    -------
  !!
  !!    METHOD
  !!    ------
  !!   
  !
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!
  !!    AUTHOR
  !!    ------
  !!
  !!    R. Schoetter        Meteo-France
  !!
  !!    MODIFICATION
  !!    ------------
  !!
  !!    Original 10/2020
  !
  !----------------------------------------------------------------------------
  !
  !*    0.     DECLARATION
  !            -----------
  !
  USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
  !
  USE MODI_GET_LUOUT
  USE MODI_OPEN_NAMELIST
  USE MODI_CLOSE_NAMELIST
  !
  USE MODE_POS_SURF
  !
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
  USE PARKIND1  ,ONLY : JPRB
  !
  IMPLICIT NONE
  !
  !*    0.1    Declaration of arguments
  !            ------------------------
  !
  CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! Type of program
  CHARACTER(LEN=2), INTENT(IN) :: HTEST    ! must be equal to 'OK' 
  !
  LOGICAL, INTENT(OUT) :: ODO_SW                 ! Compute shortwave fluxes?
  LOGICAL, INTENT(OUT) :: ODO_LW                 ! Compute longwave fluxes?
  LOGICAL, INTENT(OUT) :: OUSE_SW_DIRECT_ALBEDO  ! Specify ground and roof albedos separately for direct solar radiation?
  !
  LOGICAL, INTENT(OUT) :: ODO_VEGETATION  ! Will vegetation be represented?
  LOGICAL, INTENT(OUT) :: ODO_URBAN       ! Will urban areas be represented?
  !
  INTEGER, INTENT(OUT) :: K_VEGETATION_REGION_URBAN  ! Number of regions used to describe urban vegetation (2 needed for heterogeneity)
  INTEGER, INTENT(OUT) :: K_VEGETATION_REGION_FOREST ! Number of regions used to describe forests (2 needed for heterogeneity)
  !
  INTEGER, INTENT(OUT) :: KSW ! Number of shortwave bands
  INTEGER, INTENT(OUT) :: KLW ! Number of longwave bands
  !
  INTEGER, INTENT(OUT) :: K_STREAM_SW_URBAN  ! Number of streams per hemisphere to describe diffuse shortwave radiation, urban areas
  INTEGER, INTENT(OUT) :: K_STREAM_LW_URBAN  ! Number of streams per hemisphere to describe longwave radiation, urban areas
  INTEGER, INTENT(OUT) :: K_STREAM_SW_FOREST ! Number of streams per hemisphere to describe diffuse shortwave radiation, forests
  INTEGER, INTENT(OUT) :: K_STREAM_LW_FOREST ! Number of streams per hemisphere to describe longwave radiation, forests
  !
  LOGICAL, INTENT(OUT) :: OUSE_SYMMETRIC_VEGETATION_SCALE_URBAN  ! TRUE : Tree crowns touch each other; Eq. 20 of Hogan et al. (2018).
  ! FALSE: Tree crowns separate (shyness); Eq. 19 of Hogan et al. (2018).
  LOGICAL, INTENT(OUT) :: OUSE_SYMMETRIC_VEGETATION_SCALE_FOREST ! TRUE : Tree crowns touch each other; Eq. 20 of Hogan et al. (2018).
  ! FALSE: Tree crowns separate (shyness); Eq. 19 of Hogan et al. (2018).
  ! 
  REAL, INTENT(OUT) :: ZVEGETATION_ISOLATION_FACTOR_URBAN  ! 0.0: Dense vegetation region is embedded within sparse region
  ! 1.0: Dense vegetation is in physically isolated regions
  REAL, INTENT(OUT) :: ZVEGETATION_ISOLATION_FACTOR_FOREST ! 0.0: Dense vegetation region is embedded within sparse region
  ! 1.0: Dense vegetation is in physically isolated regions
  !
  REAL, INTENT(OUT) :: ZMIN_VEGETATION_FRACTION ! Minimum area fraction below which a vegetation region is removed completely
  REAL, INTENT(OUT) :: ZMIN_BUILDING_FRACTION   ! Minimum area fraction below which a building region is removed completely
  !
  !
  !*    0.2    Declaration of local variables
  !            ------------------------------
  !
  INTEGER :: ILUOUT  ! output listing logical unit
  INTEGER :: ILUNAM  ! namelist file logical unit
  LOGICAL :: GFOUND  ! flag when namelist is present
  !
  !*    0.3    Declaration of namelists
  !            ------------------------
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
  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  !
  NAMELIST/NAM_SPARTACUS/ LDO_SW, LDO_LW, LUSE_SW_DIRECT_ALBEDO, LDO_VEGETATION, &
       LDO_URBAN, N_VEGETATION_REGION_URBAN, N_VEGETATION_REGION_FOREST,         &
       NSW, NLW, N_STREAM_SW_URBAN, N_STREAM_LW_URBAN, N_STREAM_SW_FOREST,       &
       N_STREAM_LW_FOREST, LUSE_SYMMETRIC_VEGETATION_SCALE_URBAN,                &
       LUSE_SYMMETRIC_VEGETATION_SCALE_FOREST,                                   &
       XVEGETATION_ISOLATION_FACTOR_URBAN, XVEGETATION_ISOLATION_FACTOR_FOREST,  &
       XMIN_VEGETATION_FRACTION, XMIN_BUILDING_FRACTION 
  !
  !-------------------------------------------------------------------------------
  !
  !*    1.      Initializations of defaults
  !             ---------------------------
  !
  IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_SPARTACUS',0,ZHOOK_HANDLE)
  !
  IF (HTEST/='OK') THEN
     CALL ABOR1_SFX('READ_NAM_PGD_SPARTACUS: FATAL ERROR DURING ARGUMENT TRANSFER')
  ENDIF
  !
  LDO_SW = .TRUE.
  LDO_LW = .TRUE.
  LUSE_SW_DIRECT_ALBEDO = .FALSE.
  LDO_VEGETATION = .TRUE.
  LDO_URBAN = .TRUE.
  N_VEGETATION_REGION_URBAN = 1
  N_VEGETATION_REGION_FOREST = 1
  NSW = 1
  NLW = 1
  N_STREAM_SW_URBAN = 4
  N_STREAM_LW_URBAN = 4
  N_STREAM_SW_FOREST = 4
  N_STREAM_LW_FOREST = 4
  LUSE_SYMMETRIC_VEGETATION_SCALE_URBAN  = .FALSE.
  LUSE_SYMMETRIC_VEGETATION_SCALE_FOREST = .FALSE.
  XVEGETATION_ISOLATION_FACTOR_URBAN  = 0.0
  XVEGETATION_ISOLATION_FACTOR_FOREST = 0.0
  XMIN_VEGETATION_FRACTION = 1.0E-6
  XMIN_BUILDING_FRACTION   = 1.0E-6
  !
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  !
  !-------------------------------------------------------------------------------
  !
  !*    2.      Reading of namelist
  !             -------------------
  !
  CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
  !
  CALL POSNAM(ILUNAM,'NAM_SPARTACUS',GFOUND,ILUOUT)
  IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_SPARTACUS)
  !
  CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
  !
  !-------------------------------------------------------------------------------
  !
  ODO_SW=LDO_SW
  ODO_LW=LDO_LW
  OUSE_SW_DIRECT_ALBEDO=LUSE_SW_DIRECT_ALBEDO
  ODO_VEGETATION=LDO_VEGETATION
  ODO_URBAN=LDO_URBAN
  K_VEGETATION_REGION_URBAN=N_VEGETATION_REGION_URBAN
  K_VEGETATION_REGION_FOREST=N_VEGETATION_REGION_FOREST
  KSW=NSW
  KLW=NLW
  K_STREAM_SW_URBAN=N_STREAM_SW_URBAN
  K_STREAM_LW_URBAN=N_STREAM_LW_URBAN
  K_STREAM_SW_FOREST=N_STREAM_SW_FOREST
  K_STREAM_LW_FOREST=N_STREAM_LW_FOREST
  OUSE_SYMMETRIC_VEGETATION_SCALE_URBAN=LUSE_SYMMETRIC_VEGETATION_SCALE_URBAN
  OUSE_SYMMETRIC_VEGETATION_SCALE_FOREST=LUSE_SYMMETRIC_VEGETATION_SCALE_FOREST
  ZVEGETATION_ISOLATION_FACTOR_URBAN=XVEGETATION_ISOLATION_FACTOR_URBAN
  ZVEGETATION_ISOLATION_FACTOR_FOREST=XVEGETATION_ISOLATION_FACTOR_FOREST
  ZMIN_VEGETATION_FRACTION=XMIN_VEGETATION_FRACTION
  ZMIN_BUILDING_FRACTION=XMIN_BUILDING_FRACTION
  !
  IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_SPARTACUS',1,ZHOOK_HANDLE)
  !
  !-------------------------------------------------------------------------------
  !
END SUBROUTINE READ_NAM_PGD_SPARTACUS
