!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB_PAR(HPROGRAM,OGARDEN,OGREENROOF,HBLD_ATYPE)
!     ##############################################################
!
!!**** *PGD_TEB_PAR* monitor for averaging and interpolations of cover fractions
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/12/97
!!
!!       Modified 08/12/05, P. Le Moigne: user defined fields
!!    G. Pigeon      09/2012: add ROUGH_WALL/ROUGH_ROOF for outdoor convection
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF, NUNDEF
USE MODD_TEB_GRID_n, ONLY : NDIM
USE MODD_DATA_TEB_n, ONLY : NPAR_ROAD_LAYER_n => NPAR_ROAD_LAYER,          &
                            NPAR_ROOF_LAYER_n => NPAR_ROOF_LAYER,          &
                            NPAR_WALL_LAYER_n => NPAR_WALL_LAYER,          &
                            XPAR_Z0_TOWN, XPAR_BLD, XPAR_ALB_ROOF,         &
                            XPAR_EMIS_ROOF, XPAR_HC_ROOF, XPAR_TC_ROOF,    &
                            XPAR_D_ROOF, XPAR_ALB_ROAD, XPAR_EMIS_ROAD,    &
                            XPAR_HC_ROAD, XPAR_TC_ROAD, XPAR_D_ROAD,       &
                            XPAR_ALB_WALL, XPAR_EMIS_WALL, XPAR_HC_WALL,   &
                            XPAR_TC_WALL, XPAR_D_WALL, XPAR_BLD_HEIGHT,    &
                            XPAR_WALL_O_HOR,                               &
                            XPAR_H_TRAFFIC, XPAR_LE_TRAFFIC,               &
                            XPAR_H_INDUSTRY, XPAR_LE_INDUSTRY ,            &
                            XPAR_GARDEN, NPAR_BLDTYPE,                     &
                            XPAR_ROAD_DIR, NPAR_USETYPE, NPAR_BLD_AGE,     &
                            NPAR_BLDCODE,                                  &
                            LDATA_Z0_TOWN, LDATA_BLD, LDATA_ALB_ROOF,      &
                            LDATA_EMIS_ROOF, LDATA_HC_ROOF, LDATA_TC_ROOF, &
                            LDATA_D_ROOF, LDATA_ALB_ROAD, LDATA_EMIS_ROAD, &
                            LDATA_HC_ROAD, LDATA_TC_ROAD, LDATA_D_ROAD,    &
                            LDATA_ALB_WALL, LDATA_EMIS_WALL, LDATA_HC_WALL,&
                            LDATA_TC_WALL, LDATA_D_WALL, LDATA_BLD_HEIGHT, &
                            LDATA_WALL_O_HOR,                              &
                            LDATA_H_TRAFFIC, LDATA_LE_TRAFFIC,             &
                            LDATA_H_INDUSTRY, LDATA_LE_INDUSTRY ,          &
                            LDATA_GARDEN, LDATA_BLDTYPE,                   &
                            LDATA_ROAD_DIR, LDATA_USETYPE, LDATA_BLD_AGE,  &
                            LDATA_ROUGH_ROOF, LDATA_ROUGH_WALL,            &
                            XPAR_ROUGH_ROOF, XPAR_ROUGH_WALL,              &
                            LDATA_GREENROOF, XPAR_GREENROOF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_INI_VAR_FROM_DATA_0D
USE MODI_INI_VAR_FROM_DATA
USE MODI_TEST_NAM_VAR_SURF
USE MODI_READ_CSVDATA_TEB
USE MODI_BLDCODE
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
LOGICAL,             INTENT(IN)    :: OGARDEN      ! T if urban green areas
LOGICAL,             INTENT(IN)    :: OGREENROOF   ! T if greenroofs option is activated
 CHARACTER(LEN=3),    INTENT(OUT)   :: HBLD_ATYPE    ! Type of building averaging
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
!
REAL, DIMENSION(NDIM) :: ZWORK
REAL                  :: ZUNIF     ! temporary variable
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER, PARAMETER :: NROOF_MAX  = 9
INTEGER, PARAMETER :: NROAD_MAX  = 9
INTEGER, PARAMETER :: NWALL_MAX  = 9
INTEGER            :: NPAR_ROAD_LAYER ! number of road layers
INTEGER            :: NPAR_ROOF_LAYER ! number of roof layers
INTEGER            :: NPAR_WALL_LAYER ! number of wall layers
!
! Geometric Parameters:
!
INTEGER                                 :: NUNIF_BLDTYPE
 CHARACTER(LEN=28)                       :: CFNAM_BLDTYPE
 CHARACTER(LEN=6)                        :: CFTYP_BLDTYPE
INTEGER                                 :: NUNIF_BLD_AGE
 CHARACTER(LEN=28)                       :: CFNAM_BLD_AGE
 CHARACTER(LEN=6)                        :: CFTYP_BLD_AGE
 CHARACTER(LEN=28)                       :: CCSVDATAFILE
INTEGER                                 :: NUNIF_USETYPE
 CHARACTER(LEN=28)                       :: CFNAM_USETYPE
 CHARACTER(LEN=6)                        :: CFTYP_USETYPE
 CHARACTER(LEN=3)                        :: CBLD_ATYPE         ! type of averaging for buildings

!
REAL                                    :: XUNIF_BLD          ! fraction of buildings            (-)
REAL                                    :: XUNIF_BLD_HEIGHT   ! buildings height 'h'             (m)
REAL                                    :: XUNIF_WALL_O_HOR   ! wall surf. / hor. surf.          (-)
REAL                                    :: XUNIF_Z0_TOWN      ! roughness length for momentum    (m)
REAL                                    :: XUNIF_GARDEN       ! fraction of veg in the streets   (-)
REAL                                    :: XUNIF_GREENROOF    ! fraction of greenroofs on roofs  (-)
REAL                                    :: XUNIF_ROAD_DIR     ! road direction (° from North, clockwise)
 CHARACTER(LEN=28)                       :: CFNAM_BLD          ! file name for BLD 
 CHARACTER(LEN=28)                       :: CFNAM_BLD_HEIGHT   ! file name for BLD_HEIGHT
 CHARACTER(LEN=28)                       :: CFNAM_WALL_O_HOR   ! file name for WALL_O_HOR
 CHARACTER(LEN=28)                       :: CFNAM_Z0_TOWN      ! file name for Z0_TOWN
 CHARACTER(LEN=28)                       :: CFNAM_GARDEN       ! file name for GARDEN  
 CHARACTER(LEN=28)                       :: CFNAM_GREENROOF    ! file name for GREENROOF
 CHARACTER(LEN=28)                       :: CFNAM_ROAD_DIR     ! file name for ROAD_DIR  
 CHARACTER(LEN=6)                        :: CFTYP_BLD          ! file type for BLD 
 CHARACTER(LEN=6)                        :: CFTYP_BLD_HEIGHT   ! file type for BLD_HEIGHT
 CHARACTER(LEN=6)                        :: CFTYP_WALL_O_HOR   ! file type for WALL_O_HOR
 CHARACTER(LEN=6)                        :: CFTYP_Z0_TOWN      ! file type for Z0_TOWN
 CHARACTER(LEN=6)                        :: CFTYP_GARDEN       ! file type for GARDEN  
 CHARACTER(LEN=6)                        :: CFTYP_GREENROOF    ! file type for GREENROOF
 CHARACTER(LEN=6)                        :: CFTYP_ROAD_DIR     ! file type for ROAD_DIR
!
! Roof parameters
!
REAL                                    :: XUNIF_ALB_ROOF     ! roof albedo                      (-)
REAL                                    :: XUNIF_EMIS_ROOF    ! roof emissivity                  (-)
 CHARACTER(LEN=28)                       :: CFNAM_ALB_ROOF     ! file name for ALB_ROOF
 CHARACTER(LEN=28)                       :: CFNAM_EMIS_ROOF    ! file name for EMIS_ROOF
 CHARACTER(LEN=6)                        :: CFTYP_ALB_ROOF     ! file name for ALB_ROOF   
 CHARACTER(LEN=6)                        :: CFTYP_EMIS_ROOF    ! file name for EMIS_ROOF  
REAL, DIMENSION(NROOF_MAX)              :: XUNIF_HC_ROOF      ! roof layers heat capacity        (J/K/m3)
REAL, DIMENSION(NROOF_MAX)              :: XUNIF_TC_ROOF      ! roof layers thermal conductivity (W/K/m)
REAL, DIMENSION(NROOF_MAX)              :: XUNIF_D_ROOF       ! depth of roof layers             (m)
 CHARACTER(LEN=28), DIMENSION(NROOF_MAX) :: CFNAM_HC_ROOF      ! file name for HC_ROOF   
 CHARACTER(LEN=28), DIMENSION(NROOF_MAX) :: CFNAM_TC_ROOF      ! file name for TC_ROOF
 CHARACTER(LEN=28), DIMENSION(NROOF_MAX) :: CFNAM_D_ROOF       ! file name for D_ROOF
 CHARACTER(LEN=6),  DIMENSION(NROOF_MAX) :: CFTYP_HC_ROOF      ! file type for HC_ROOF   
 CHARACTER(LEN=6),  DIMENSION(NROOF_MAX) :: CFTYP_TC_ROOF      ! file type for TC_ROOF
 CHARACTER(LEN=6),  DIMENSION(NROOF_MAX) :: CFTYP_D_ROOF       ! file type for D_ROOF
REAL                                    :: XUNIF_ROUGH_ROOF  ! roof roughness coef
 CHARACTER(LEN=28)                       :: CFNAM_ROUGH_ROOF  ! file name for ROUGH_ROOF
 CHARACTER(LEN=6)                        :: CFTYP_ROUGH_ROOF  ! file type for ROUGH_ROOF
!
!
! Road parameters
!
REAL                                    :: XUNIF_ALB_ROAD     ! road albedo                      (-)
REAL                                    :: XUNIF_EMIS_ROAD    ! road emissivity                  (-)
 CHARACTER(LEN=28)                       :: CFNAM_ALB_ROAD     ! file name for ALB_ROAD
 CHARACTER(LEN=28)                       :: CFNAM_EMIS_ROAD    ! file name for EMIS_ROAD
 CHARACTER(LEN=6)                        :: CFTYP_ALB_ROAD     ! file type for ALB_ROAD
 CHARACTER(LEN=6)                        :: CFTYP_EMIS_ROAD    ! file type for EMIS_ROAD
REAL, DIMENSION(NROAD_MAX)              :: XUNIF_HC_ROAD      ! road layers heat capacity        (J/K/m3)
REAL, DIMENSION(NROAD_MAX)              :: XUNIF_TC_ROAD      ! road layers thermal conductivity (W/K/m)
REAL, DIMENSION(NROAD_MAX)              :: XUNIF_D_ROAD       ! depth of road layers             (m)
 CHARACTER(LEN=28), DIMENSION(NROAD_MAX) :: CFNAM_HC_ROAD      ! file name for HC_ROAD   
 CHARACTER(LEN=28), DIMENSION(NROAD_MAX) :: CFNAM_TC_ROAD      ! file name for TC_ROAD
 CHARACTER(LEN=28), DIMENSION(NROAD_MAX) :: CFNAM_D_ROAD       ! file name for D_ROAD
 CHARACTER(LEN=6),  DIMENSION(NROAD_MAX) :: CFTYP_HC_ROAD      ! file type for HC_ROAD   
 CHARACTER(LEN=6),  DIMENSION(NROAD_MAX) :: CFTYP_TC_ROAD      ! file type for TC_ROAD
 CHARACTER(LEN=6),  DIMENSION(NROAD_MAX) :: CFTYP_D_ROAD       ! file type for D_ROAD
!
! Wall parameters
!
REAL                                    :: XUNIF_ALB_WALL     ! wall albedo                      (-)
REAL                                    :: XUNIF_EMIS_WALL    ! wall emissivity                  (-)
 CHARACTER(LEN=28)                       :: CFNAM_ALB_WALL     ! file name for ALB_WALL
 CHARACTER(LEN=28)                       :: CFNAM_EMIS_WALL    ! file name for EMIS_WALL
 CHARACTER(LEN=6)                        :: CFTYP_ALB_WALL     ! file type for ALB_WALL
 CHARACTER(LEN=6)                        :: CFTYP_EMIS_WALL    ! file type for EMIS_WALL
REAL, DIMENSION(NWALL_MAX)              :: XUNIF_HC_WALL      ! wall layers heat capacity        (J/K/m3)
REAL, DIMENSION(NWALL_MAX)              :: XUNIF_TC_WALL      ! wall layers thermal conductivity (W/K/m)
REAL, DIMENSION(NWALL_MAX)              :: XUNIF_D_WALL       ! depth of wall layers             (m)
 CHARACTER(LEN=28), DIMENSION(NWALL_MAX) :: CFNAM_HC_WALL      ! file name for HC_WALL   
 CHARACTER(LEN=28), DIMENSION(NWALL_MAX) :: CFNAM_TC_WALL      ! file name for TC_WALL
 CHARACTER(LEN=28), DIMENSION(NWALL_MAX) :: CFNAM_D_WALL       ! file name for D_WALL
 CHARACTER(LEN=6),  DIMENSION(NWALL_MAX) :: CFTYP_HC_WALL      ! file type for HC_WALL   
 CHARACTER(LEN=6),  DIMENSION(NWALL_MAX) :: CFTYP_TC_WALL      ! file type for TC_WALL
 CHARACTER(LEN=6),  DIMENSION(NWALL_MAX) :: CFTYP_D_WALL       ! file type for D_WALL
REAL                                    :: XUNIF_ROUGH_WALL  ! wall roughness coef
 CHARACTER(LEN=28)                       :: CFNAM_ROUGH_WALL  ! file name for ROUGH_WALL
 CHARACTER(LEN=6)                        :: CFTYP_ROUGH_WALL  ! file type for ROUGH_WALL
!
! anthropogenic fluxes
!
REAL                                    :: XUNIF_H_TRAFFIC    ! anthropogenic sensible
!                                                             ! heat fluxes due to traffic       (W/m2)
REAL                                    :: XUNIF_LE_TRAFFIC   ! anthropogenic latent
!                                                             ! heat fluxes due to traffic       (W/m2)
REAL                                    :: XUNIF_H_INDUSTRY   ! anthropogenic sensible                   
!                                                             ! heat fluxes due to factories     (W/m2)
REAL                                    :: XUNIF_LE_INDUSTRY  ! anthropogenic latent
!                                                             ! heat fluxes due to factories     (W/m2)
 CHARACTER(LEN=28)                       :: CFNAM_H_TRAFFIC    ! file name for H_TRAFFIC
 CHARACTER(LEN=28)                       :: CFNAM_LE_TRAFFIC   ! file name for LE_TRAFFIC
 CHARACTER(LEN=28)                       :: CFNAM_H_INDUSTRY   ! file name for H_INDUSTRY
 CHARACTER(LEN=28)                       :: CFNAM_LE_INDUSTRY  ! file name for LE_INDUSTRY
 CHARACTER(LEN=6)                        :: CFTYP_H_TRAFFIC    ! file type for H_TRAFFIC
 CHARACTER(LEN=6)                        :: CFTYP_LE_TRAFFIC   ! file type for LE_TRAFFIC
 CHARACTER(LEN=6)                        :: CFTYP_H_INDUSTRY   ! file type for H_INDUSTRY
 CHARACTER(LEN=6)                        :: CFTYP_LE_INDUSTRY  ! file type for LE_INDUSTRY
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!

NAMELIST/NAM_DATA_TEB/      NPAR_ROOF_LAYER, NPAR_ROAD_LAYER, NPAR_WALL_LAYER,&
                              CBLD_ATYPE,                                     &
                              NUNIF_BLDTYPE, CFNAM_BLDTYPE, CFTYP_BLDTYPE,    &
                              NUNIF_BLD_AGE, CFNAM_BLD_AGE, CFTYP_BLD_AGE,    &
                              CCSVDATAFILE,                                   &
                              NUNIF_USETYPE, CFNAM_USETYPE, CFTYP_USETYPE,    &
                              XUNIF_ALB_ROOF,                                 &
                              XUNIF_EMIS_ROOF, XUNIF_HC_ROOF, XUNIF_TC_ROOF,  &
                              XUNIF_D_ROOF, XUNIF_ALB_ROAD, XUNIF_EMIS_ROAD,  &
                              XUNIF_HC_ROAD, XUNIF_TC_ROAD, XUNIF_D_ROAD,     &
                              XUNIF_ALB_WALL, XUNIF_EMIS_WALL, XUNIF_HC_WALL, &
                              XUNIF_TC_WALL, XUNIF_D_WALL,                    &
                              XUNIF_Z0_TOWN, XUNIF_BLD, XUNIF_BLD_HEIGHT,     &
                              XUNIF_WALL_O_HOR,                               &
                              XUNIF_H_TRAFFIC, XUNIF_LE_TRAFFIC,              &
                              XUNIF_H_INDUSTRY, XUNIF_LE_INDUSTRY,            &
                              XUNIF_GARDEN, XUNIF_GREENROOF,                  &
                              XUNIF_ROAD_DIR,                                 &
                              CFNAM_ALB_ROOF,                                 &
                              CFNAM_EMIS_ROOF, CFNAM_HC_ROOF, CFNAM_TC_ROOF,  &
                              CFNAM_D_ROOF, CFNAM_ALB_ROAD, CFNAM_EMIS_ROAD,  &
                              CFNAM_HC_ROAD, CFNAM_TC_ROAD, CFNAM_D_ROAD,     &
                              CFNAM_ALB_WALL, CFNAM_EMIS_WALL, CFNAM_HC_WALL, &
                              CFNAM_TC_WALL, CFNAM_D_WALL,                    &
                              CFNAM_Z0_TOWN, CFNAM_BLD, CFNAM_BLD_HEIGHT,     &
                              CFNAM_WALL_O_HOR,                               &
                              CFNAM_H_TRAFFIC, CFNAM_LE_TRAFFIC,              &
                              CFNAM_H_INDUSTRY, CFNAM_LE_INDUSTRY,            &
                              CFNAM_GARDEN, CFNAM_ROAD_DIR, CFNAM_GREENROOF,  &
                              CFTYP_ALB_ROOF,                                 &
                              CFTYP_EMIS_ROOF, CFTYP_HC_ROOF, CFTYP_TC_ROOF,  &
                              CFTYP_D_ROOF, CFTYP_ALB_ROAD, CFTYP_EMIS_ROAD,  &
                              CFTYP_HC_ROAD, CFTYP_TC_ROAD, CFTYP_D_ROAD,     &
                              CFTYP_ALB_WALL, CFTYP_EMIS_WALL, CFTYP_HC_WALL, &
                              CFTYP_TC_WALL, CFTYP_D_WALL,                    &
                              CFTYP_Z0_TOWN, CFTYP_BLD, CFTYP_BLD_HEIGHT,     &
                              CFTYP_WALL_O_HOR,                               &
                              CFTYP_H_TRAFFIC, CFTYP_LE_TRAFFIC,              &
                              CFTYP_H_INDUSTRY, CFTYP_LE_INDUSTRY,            &
                              CFTYP_GARDEN, CFTYP_ROAD_DIR, CFTYP_GREENROOF,  &
                              XUNIF_ROUGH_ROOF, CFNAM_ROUGH_ROOF, CFTYP_ROUGH_ROOF, &
                              XUNIF_ROUGH_WALL, CFNAM_ROUGH_WALL, CFTYP_ROUGH_WALL

!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK)   CALL DR_HOOK('PGD_TEB_PAR',0,ZHOOK_HANDLE)
NPAR_ROOF_LAYER=0
NPAR_ROAD_LAYER=0
NPAR_WALL_LAYER=0
CBLD_ATYPE ='MAJ'
NUNIF_BLDTYPE      = NUNDEF
NUNIF_BLD_AGE      = NUNDEF
NUNIF_USETYPE      = NUNDEF
XUNIF_BLD          = XUNDEF
XUNIF_BLD_HEIGHT   = XUNDEF
XUNIF_WALL_O_HOR   = XUNDEF
XUNIF_Z0_TOWN      = XUNDEF
XUNIF_ALB_ROOF     = XUNDEF
XUNIF_EMIS_ROOF    = XUNDEF
XUNIF_HC_ROOF      = XUNDEF
XUNIF_TC_ROOF      = XUNDEF
XUNIF_D_ROOF       = XUNDEF
XUNIF_ALB_ROAD     = XUNDEF
XUNIF_EMIS_ROAD    = XUNDEF
XUNIF_HC_ROAD      = XUNDEF
XUNIF_TC_ROAD      = XUNDEF
XUNIF_D_ROAD       = XUNDEF
XUNIF_ALB_WALL     = XUNDEF
XUNIF_EMIS_WALL    = XUNDEF
XUNIF_HC_WALL      = XUNDEF
XUNIF_TC_WALL      = XUNDEF
XUNIF_D_WALL       = XUNDEF
XUNIF_H_TRAFFIC    = XUNDEF
XUNIF_LE_TRAFFIC   = XUNDEF
XUNIF_H_INDUSTRY   = XUNDEF
XUNIF_LE_INDUSTRY  = XUNDEF
XUNIF_GARDEN       = XUNDEF
XUNIF_GREENROOF    = XUNDEF
XUNIF_ROAD_DIR     = XUNDEF
XUNIF_ROUGH_ROOF   = XUNDEF
XUNIF_ROUGH_WALL   = XUNDEF

CFNAM_BLDTYPE      = '                            '
CFNAM_BLD_AGE      = '                            '
CFNAM_USETYPE      = '                            '
CCSVDATAFILE       ='                            '
CFNAM_BLD          = '                            '
CFNAM_BLD_HEIGHT   = '                            '
CFNAM_WALL_O_HOR   = '                            '
CFNAM_Z0_TOWN      = '                            '

CFNAM_ALB_ROOF (:) = '                            '
CFNAM_EMIS_ROOF(:) = '                            '
CFNAM_HC_ROOF  (:) = '                            '
CFNAM_TC_ROOF  (:) = '                            '
CFNAM_D_ROOF   (:) = '                            '
CFNAM_ROUGH_ROOF(:) = '                            '
CFNAM_ROUGH_WALL(:) = '                            '
CFNAM_ALB_ROAD (:) = '                            '
CFNAM_EMIS_ROAD(:) = '                            '
CFNAM_HC_ROAD  (:) = '                            '
CFNAM_TC_ROAD  (:) = '                            '
CFNAM_D_ROAD   (:) = '                            '
CFNAM_ALB_WALL (:) = '                            '
CFNAM_EMIS_WALL(:) = '                            '
CFNAM_HC_WALL  (:) = '                            '
CFNAM_TC_WALL  (:) = '                            '
CFNAM_D_WALL   (:) = '                            '

CFNAM_H_TRAFFIC    = '                            '
CFNAM_LE_TRAFFIC   = '                            '
CFNAM_H_INDUSTRY   = '                            '
CFNAM_LE_INDUSTRY  = '                            '

CFNAM_GARDEN       = '                            '
CFNAM_GREENROOF    = '                            '
CFNAM_ROAD_DIR     = '                            '

CFTYP_BLDTYPE      = '      '
CFTYP_BLD_AGE      = '      '
CFTYP_USETYPE      = '      '
CFTYP_BLD          = '      '
CFTYP_BLD_HEIGHT   = '      '
CFTYP_WALL_O_HOR   = '      '
CFTYP_Z0_TOWN      = '      '
CFTYP_ALB_ROOF(:)  = '      '
CFTYP_EMIS_ROOF(:) = '      '
CFTYP_HC_ROOF(:)   = '      '
CFTYP_TC_ROOF(:)   = '      '
CFTYP_D_ROOF(:)    = '      '
CFTYP_ROUGH_ROOF(:)    = '      '
CFTYP_ROUGH_WALL(:)    = '      '
CFTYP_ALB_ROAD(:)  = '      '
CFTYP_EMIS_ROAD(:) = '      '
CFTYP_HC_ROAD(:)   = '      '
CFTYP_TC_ROAD(:)   = '      '
CFTYP_D_ROAD(:)    = '      '
CFTYP_ALB_WALL(:)  = '      '
CFTYP_EMIS_WALL(:) = '      '
CFTYP_HC_WALL(:)   = '      '
CFTYP_TC_WALL(:)   = '      '
CFTYP_D_WALL(:)    = '      '
CFTYP_H_TRAFFIC    = '      '
CFTYP_LE_TRAFFIC   = '      '
CFTYP_H_INDUSTRY   = '      '
CFTYP_LE_INDUSTRY  = '      '
CFTYP_GARDEN       = '      '
CFTYP_GREENROOF    = '      '
CFTYP_ROAD_DIR     = '      '
!

!
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DATA_TEB',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_TEB)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CBLD_ATYPE',CBLD_ATYPE,'ARI','MAJ')
!
NPAR_ROOF_LAYER_n = NPAR_ROOF_LAYER
NPAR_ROAD_LAYER_n = NPAR_ROAD_LAYER
NPAR_WALL_LAYER_n = NPAR_WALL_LAYER
!
HBLD_ATYPE = CBLD_ATYPE
!-------------------------------------------------------------------------------
!
!* coherence check
!
IF ((     ANY(XUNIF_HC_ROAD/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_HC_ROAD)>0) &
     .OR. ANY(XUNIF_TC_ROAD/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_TC_ROAD)>0) &
     .OR. ANY(XUNIF_D_ROAD /=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_D_ROAD )>0) &
    ) .AND. NPAR_ROAD_LAYER<1                                  ) THEN
  CALL ABOR1_SFX('In order to initialize road thermal quantities, please specify NPAR_ROAD_LAYER in namelist NAM_DATA_TEB')
END IF
!
IF ((     ANY(XUNIF_HC_ROOF/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_HC_ROOF)>0) &
     .OR. ANY(XUNIF_TC_ROOF/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_TC_ROOF)>0) &
     .OR. ANY(XUNIF_D_ROOF /=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_D_ROOF )>0) &
    ) .AND. NPAR_ROOF_LAYER<1                                  ) THEN
  CALL ABOR1_SFX('In order to initialize ROOF thermal quantities, please specify NPAR_ROOF_LAYER in namelist NAM_DATA_TEB')
END IF
!
IF ((     ANY(XUNIF_HC_WALL/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_HC_WALL)>0) &
     .OR. ANY(XUNIF_TC_WALL/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_TC_WALL)>0) &
     .OR. ANY(XUNIF_D_WALL /=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_D_WALL )>0) &
    ) .AND. NPAR_WALL_LAYER<1                                  ) THEN
  CALL ABOR1_SFX('In order to initialize WALL thermal quantities, please specify NPAR_WALL_LAYER in namelist NAM_DATA_TEB')
END IF
!-------------------------------------------------------------------------------
ALLOCATE(NPAR_BLDTYPE     (NDIM))
ALLOCATE(NPAR_BLD_AGE     (NDIM))
ALLOCATE(NPAR_BLDCODE     (NDIM))
ALLOCATE(NPAR_USETYPE     (NDIM))
ALLOCATE(XPAR_Z0_TOWN     (NDIM))
ALLOCATE(XPAR_ALB_ROOF    (NDIM))
ALLOCATE(XPAR_EMIS_ROOF   (NDIM))
ALLOCATE(XPAR_ALB_ROAD    (NDIM))
ALLOCATE(XPAR_EMIS_ROAD   (NDIM))
ALLOCATE(XPAR_ALB_WALL    (NDIM))
ALLOCATE(XPAR_EMIS_WALL   (NDIM))
ALLOCATE(XPAR_BLD         (NDIM))
ALLOCATE(XPAR_BLD_HEIGHT  (NDIM))
ALLOCATE(XPAR_WALL_O_HOR  (NDIM))
ALLOCATE(XPAR_H_TRAFFIC   (NDIM))
ALLOCATE(XPAR_LE_TRAFFIC  (NDIM))
ALLOCATE(XPAR_H_INDUSTRY  (NDIM))
ALLOCATE(XPAR_LE_INDUSTRY (NDIM))
ALLOCATE(XPAR_GARDEN      (NDIM))
ALLOCATE(XPAR_GREENROOF   (NDIM))
ALLOCATE(XPAR_ROAD_DIR    (NDIM))
!
ALLOCATE(XPAR_HC_ROOF     (NDIM,NPAR_ROOF_LAYER))
ALLOCATE(XPAR_TC_ROOF     (NDIM,NPAR_ROOF_LAYER))
ALLOCATE(XPAR_D_ROOF      (NDIM,NPAR_ROOF_LAYER))
ALLOCATE(XPAR_HC_ROAD     (NDIM,NPAR_ROAD_LAYER))
ALLOCATE(XPAR_TC_ROAD     (NDIM,NPAR_ROAD_LAYER))
ALLOCATE(XPAR_D_ROAD      (NDIM,NPAR_ROAD_LAYER))
ALLOCATE(XPAR_HC_WALL     (NDIM,NPAR_WALL_LAYER))
ALLOCATE(XPAR_TC_WALL     (NDIM,NPAR_WALL_LAYER))
ALLOCATE(XPAR_D_WALL      (NDIM,NPAR_WALL_LAYER))
ALLOCATE(XPAR_ROUGH_ROOF    (NDIM))
ALLOCATE(XPAR_ROUGH_WALL    (NDIM))
!
!-------------------------------------------------------------------------------
IF (NROOF_MAX < NPAR_ROOF_LAYER) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_teb_par.f90 routine :      '
  WRITE(ILUOUT,*) 'The maximum number of ROOF LAYER             '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NPAR_ROOF_LAYER
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_TEB_PAR: MAXIMUM NUMBER OF NROOF_LAYER MUST BE INCREASED')
ENDIF
!-------------------------------------------------------------------------------
IF (NROAD_MAX < NPAR_ROAD_LAYER) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_teb_par.f90 routine :      '
  WRITE(ILUOUT,*) 'The maximum number of ROAD LAYER             '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NPAR_ROAD_LAYER
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_TEB_PAR: MAXIMUM NUMBER OF NROAD_LAYER MUST BE INCREASED')
ENDIF
!-------------------------------------------------------------------------------
IF (NWALL_MAX < NPAR_WALL_LAYER) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_teb_par.f90 routine :      '
  WRITE(ILUOUT,*) 'The maximum number of WALL LAYER             '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NPAR_WALL_LAYER
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_TEB_PAR: MAXIMUM NUMBER OF NWALL_LAYER MUST BE INCREASED')
ENDIF

!-------------------------------------------------------------------------------
!
!*    3.      user defined fields are prescribed
!             ----------------------------------
!
!* building's type
ZUNIF = XUNDEF
IF (NUNIF_BLDTYPE/=NUNDEF) ZUNIF=FLOAT(NUNIF_BLDTYPE)
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'MAJ','BLDTYPE    ','TWN', CFNAM_BLDTYPE,CFTYP_BLDTYPE,ZUNIF,&
        ZWORK(:),LDATA_BLDTYPE )
IF (.NOT. LDATA_BLDTYPE) THEN
  DEALLOCATE(NPAR_BLDTYPE)
ELSE
  NPAR_BLDTYPE = NINT(ZWORK)
END IF
!
!* building's age
ZUNIF = XUNDEF
IF (NUNIF_BLD_AGE/=NUNDEF) ZUNIF=FLOAT(NUNIF_BLD_AGE)
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','BLD_AGE    ','TWN', CFNAM_BLD_AGE,CFTYP_BLD_AGE,ZUNIF,&
        ZWORK(:),LDATA_BLD_AGE )
IF (.NOT. LDATA_BLD_AGE) THEN
  DEALLOCATE(NPAR_BLD_AGE)
ELSE
  NPAR_BLD_AGE = NINT(ZWORK)
END IF
!
IF (LDATA_BLDTYPE .AND. .NOT. LDATA_BLD_AGE) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) ' You chose to define building types :        '
  IF (NUNIF_BLDTYPE/=NUNDEF) THEN
    WRITE(ILUOUT,*) ' NUNIF_BLDTYPE=', NUNIF_BLDTYPE
  ELSE
    WRITE(ILUOUT,*) ' CFNAM_BLDTYPE =',CFNAM_BLDTYPE 
    WRITE(ILUOUT,*) ' CFTYP_BLDTYPE =',CFTYP_BLDTYPE 
  END IF
  WRITE(ILUOUT,*) ' But            '
  WRITE(ILUOUT,*) " You did not chose to define building's age"
  WRITE(ILUOUT,*) '- - - - - - - - - - - - - - - - - - - - - - -'
  WRITE(ILUOUT,*) ' Please define the construction date of the buildings. '
  WRITE(ILUOUT,*) ' To do so, use either :'
  WRITE(ILUOUT,*) ' NUNIF_BLD_AGE   (to have a uniform construction date for all buildings'
  WRITE(ILUOUT,*) ' or CFNAM_BLD_AGE and CFTYP_BLD_AGE (to incorporate spatial data ) '
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX("PGD_TEB_PAR: Building's age data is missing")
END IF
!
!* building's use
ZUNIF = XUNDEF
IF (NUNIF_USETYPE/=NUNDEF) ZUNIF=FLOAT(NUNIF_USETYPE)
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'MAJ','USETYPE    ','TWN', CFNAM_USETYPE,CFTYP_USETYPE,ZUNIF,&
        ZWORK(:),LDATA_USETYPE )
IF (.NOT. LDATA_USETYPE) THEN
  DEALLOCATE(NPAR_USETYPE)
ELSE
  NPAR_USETYPE = NINT(ZWORK)
END IF
!
IF (LDATA_BLDTYPE .OR. LDATA_BLD_AGE .OR. LDATA_USETYPE)  CALL READ_CSVDATA_TEB(HPROGRAM,CCSVDATAFILE)
!
!* building's code
IF (ASSOCIATED(NPAR_BLDTYPE)) NPAR_BLDCODE(:) = BLDCODE(NPAR_BLDTYPE,NPAR_BLD_AGE)
!
!
!* other building parameters
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','BLD        ','TWN', CFNAM_BLD,CFTYP_BLD,XUNIF_BLD,XPAR_BLD,LDATA_BLD )
IF (.NOT.LDATA_BLD) DEALLOCATE(XPAR_BLD)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','BLD_HEIGHT ','TWN',CFNAM_BLD_HEIGHT,CFTYP_BLD_HEIGHT,XUNIF_BLD_HEIGHT,&
        XPAR_BLD_HEIGHT,LDATA_BLD_HEIGHT)
IF (.NOT.LDATA_BLD_HEIGHT) DEALLOCATE(XPAR_BLD_HEIGHT)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','WALL_O_HOR ','TWN',CFNAM_WALL_O_HOR,CFTYP_WALL_O_HOR,XUNIF_WALL_O_HOR,&
        XPAR_WALL_O_HOR,LDATA_WALL_O_HOR)
IF (.NOT.LDATA_WALL_O_HOR) DEALLOCATE(XPAR_WALL_O_HOR)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'CDN','Z0_TOWN    ','TWN',CFNAM_Z0_TOWN,CFTYP_Z0_TOWN,XUNIF_Z0_TOWN,&
        XPAR_Z0_TOWN,LDATA_Z0_TOWN)
IF (.NOT.LDATA_Z0_TOWN) DEALLOCATE(XPAR_Z0_TOWN)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,CBLD_ATYPE,'ALB_ROOF   ','TWN',CFNAM_ALB_ROOF,CFTYP_ALB_ROOF,XUNIF_ALB_ROOF  ,&
        XPAR_ALB_ROOF,LDATA_ALB_ROOF)
IF (.NOT.LDATA_ALB_ROOF) DEALLOCATE(XPAR_ALB_ROOF)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,CBLD_ATYPE,'EMIS_ROOF  ','TWN',CFNAM_EMIS_ROOF,CFTYP_EMIS_ROOF,XUNIF_EMIS_ROOF ,&
        XPAR_EMIS_ROOF,LDATA_EMIS_ROOF)
IF (.NOT.LDATA_EMIS_ROOF) DEALLOCATE(XPAR_EMIS_ROOF)
!
 CALL INI_VAR_FROM_DATA(HPROGRAM,CBLD_ATYPE,'HC_ROOF  ','TWN',CFNAM_HC_ROOF,CFTYP_HC_ROOF, &
        XUNIF_HC_ROOF,XPAR_HC_ROOF,LDATA_HC_ROOF ) 
IF (.NOT.LDATA_HC_ROOF) DEALLOCATE(XPAR_HC_ROOF)
! 
 CALL INI_VAR_FROM_DATA(HPROGRAM,CBLD_ATYPE,'TC_ROOF  ','TWN',CFNAM_TC_ROOF,CFTYP_TC_ROOF, &
                 XUNIF_TC_ROOF ,XPAR_TC_ROOF, LDATA_TC_ROOF ) 
IF (.NOT.LDATA_TC_ROOF) DEALLOCATE(XPAR_TC_ROOF)
! 
 CALL INI_VAR_FROM_DATA(HPROGRAM,CBLD_ATYPE,'D_ROOF   ','TWN',CFNAM_D_ROOF,CFTYP_D_ROOF, &
                 XUNIF_D_ROOF  ,XPAR_D_ROOF , LDATA_D_ROOF ) 
IF (.NOT.LDATA_D_ROOF) DEALLOCATE(XPAR_D_ROOF)
! 
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','ALB_ROAD   ','TWN',CFNAM_ALB_ROAD  ,CFTYP_ALB_ROAD  ,XUNIF_ALB_ROAD  ,&
        XPAR_ALB_ROAD, LDATA_ALB_ROAD  )
IF (.NOT.LDATA_ALB_ROAD) DEALLOCATE(XPAR_ALB_ROAD)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','EMIS_ROAD  ','TWN',CFNAM_EMIS_ROAD ,CFTYP_EMIS_ROAD ,XUNIF_EMIS_ROAD ,&
        XPAR_EMIS_ROAD, LDATA_EMIS_ROAD )
IF (.NOT.LDATA_EMIS_ROAD) DEALLOCATE(XPAR_EMIS_ROAD)
!
 CALL INI_VAR_FROM_DATA(HPROGRAM,CBLD_ATYPE,'HC_ROAD  ','TWN',CFNAM_HC_ROAD ,CFTYP_HC_ROAD , &
                   XUNIF_HC_ROAD ,XPAR_HC_ROAD, LDATA_HC_ROAD  )  
IF (.NOT.LDATA_HC_ROAD) DEALLOCATE(XPAR_HC_ROAD)
!
 CALL INI_VAR_FROM_DATA(HPROGRAM,CBLD_ATYPE,'TC_ROAD  ','TWN',CFNAM_TC_ROAD ,CFTYP_TC_ROAD , &
                   XUNIF_TC_ROAD ,XPAR_TC_ROAD, LDATA_TC_ROAD  )  
IF (.NOT.LDATA_TC_ROAD) DEALLOCATE(XPAR_TC_ROAD)
!
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','D_ROAD   ','TWN',CFNAM_D_ROAD  ,CFTYP_D_ROAD  , &
                   XUNIF_D_ROAD  ,XPAR_D_ROAD , LDATA_D_ROAD  )
IF (.NOT.LDATA_D_ROAD) DEALLOCATE(XPAR_D_ROAD)
!  
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,CBLD_ATYPE,'ALB_WALL   ','TWN',CFNAM_ALB_WALL  ,CFTYP_ALB_WALL  ,XUNIF_ALB_WALL  ,&
        XPAR_ALB_WALL, LDATA_ALB_WALL   )
IF (.NOT.LDATA_ALB_WALL) DEALLOCATE(XPAR_ALB_WALL)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,CBLD_ATYPE,'EMIS_WALL  ','TWN',CFNAM_EMIS_WALL ,CFTYP_EMIS_WALL ,XUNIF_EMIS_WALL ,&
        XPAR_EMIS_WALL, LDATA_EMIS_WALL  )
IF (.NOT.LDATA_EMIS_WALL) DEALLOCATE(XPAR_EMIS_WALL)
!
 CALL INI_VAR_FROM_DATA(HPROGRAM,CBLD_ATYPE,'HC_WALL  ','TWN',CFNAM_HC_WALL ,CFTYP_HC_WALL , &
                   XUNIF_HC_WALL ,XPAR_HC_WALL, LDATA_HC_WALL  ) 
IF (.NOT.LDATA_HC_WALL) DEALLOCATE(XPAR_HC_WALL)
! 
 CALL INI_VAR_FROM_DATA(HPROGRAM,CBLD_ATYPE,'TC_WALL  ','TWN',CFNAM_TC_WALL ,CFTYP_TC_WALL , &
                   XUNIF_TC_WALL ,XPAR_TC_WALL, LDATA_TC_WALL  ) 
IF (.NOT.LDATA_TC_WALL) DEALLOCATE(XPAR_TC_WALL)
! 
 CALL INI_VAR_FROM_DATA(HPROGRAM,CBLD_ATYPE,'D_WALL   ','TWN',CFNAM_D_WALL  ,CFTYP_D_WALL  , &
                   XUNIF_D_WALL  ,XPAR_D_WALL , LDATA_D_WALL  ) 
IF (.NOT.LDATA_D_WALL) DEALLOCATE(XPAR_D_WALL)
! 
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','H_TRAFFIC  ','TWN',CFNAM_H_TRAFFIC  ,CFTYP_H_TRAFFIC  ,XUNIF_H_TRAFFIC  ,&
        XPAR_H_TRAFFIC, LDATA_H_TRAFFIC   )
IF (.NOT.LDATA_H_TRAFFIC) DEALLOCATE(XPAR_H_TRAFFIC)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','LE_TRAFFIC ','TWN',CFNAM_LE_TRAFFIC ,CFTYP_LE_TRAFFIC ,XUNIF_LE_TRAFFIC ,&
        XPAR_LE_TRAFFIC, LDATA_LE_TRAFFIC  )
IF (.NOT.LDATA_LE_TRAFFIC) DEALLOCATE(XPAR_LE_TRAFFIC)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','H_INDUSTRY ','TWN',CFNAM_H_INDUSTRY ,CFTYP_H_INDUSTRY ,XUNIF_H_INDUSTRY ,&
        XPAR_H_INDUSTRY, LDATA_H_INDUSTRY  )
IF (.NOT.LDATA_H_INDUSTRY) DEALLOCATE(XPAR_H_INDUSTRY)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','LE_INDUSTRY','TWN',CFNAM_LE_INDUSTRY,CFTYP_LE_INDUSTRY,XUNIF_LE_INDUSTRY,&
        XPAR_LE_INDUSTRY, LDATA_LE_INDUSTRY )
IF (.NOT.LDATA_LE_INDUSTRY) DEALLOCATE(XPAR_LE_INDUSTRY)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,CBLD_ATYPE,'ROUGH_ROOF','TWN',CFNAM_ROUGH_ROOF,CFTYP_ROUGH_ROOF,XUNIF_ROUGH_ROOF ,&
        XPAR_ROUGH_ROOF,LDATA_ROUGH_ROOF)
IF (.NOT.LDATA_ROUGH_ROOF) DEALLOCATE(XPAR_ROUGH_ROOF)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,CBLD_ATYPE,'ROUGH_WALL','TWN',CFNAM_ROUGH_WALL,CFTYP_ROUGH_WALL,XUNIF_ROUGH_WALL ,&
        XPAR_ROUGH_WALL,LDATA_ROUGH_WALL)
IF (.NOT.LDATA_ROUGH_WALL) DEALLOCATE(XPAR_ROUGH_WALL)

!-------------------------------------------------------------------------------
!
!* coherence checks
!
 CALL COHERENCE_THERMAL_DATA('ROAD',LDATA_HC_ROAD,LDATA_TC_ROAD,LDATA_D_ROAD)
 CALL COHERENCE_THERMAL_DATA('ROOF',LDATA_HC_ROOF,LDATA_TC_ROOF,LDATA_D_ROOF)
 CALL COHERENCE_THERMAL_DATA('WALL',LDATA_HC_WALL,LDATA_TC_WALL,LDATA_D_WALL)

!-------------------------------------------------------------------------------
!
!* road directions
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','ROAD_DIR   ','TWN',CFNAM_ROAD_DIR  ,CFTYP_ROAD_DIR    ,XUNIF_ROAD_DIR   ,&
        XPAR_ROAD_DIR, LDATA_ROAD_DIR    )
IF (.NOT.LDATA_ROAD_DIR) DEALLOCATE(XPAR_ROAD_DIR)
!
!-------------------------------------------------------------------------------
!
!* greenroof fraction
!
IF (OGREENROOF) THEN
  CALL INI_VAR_FROM_DATA_0D(HPROGRAM,CBLD_ATYPE,'GREENROOF','BLD',CFNAM_GREENROOF,CFTYP_GREENROOF,XUNIF_GREENROOF ,&
        XPAR_GREENROOF,LDATA_GREENROOF)
  IF (.NOT.LDATA_GREENROOF) DEALLOCATE(XPAR_GREENROOF)
ELSE IF ( (XUNIF_GREENROOF/=0. .AND. XUNIF_GREENROOF/=XUNDEF) .OR. LEN_TRIM(CFNAM_GREENROOF)/=0) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) ' You chose not to include greenroofs in urban areas : LGREENROOF=.FALSE.     '
  WRITE(ILUOUT,*) ' But            '
  IF (XUNIF_GREENROOF/=0. .AND. XUNIF_GREENROOF/=XUNDEF) THEN
    WRITE(ILUOUT,*) ' You also chose a greenroof fraction that is not zero : XUNIF_GREENROOF=',XUNIF_GREENROOF
  ELSE
    WRITE(ILUOUT,*) ' You also chose a greenroof fraction that is not zero : CFNAM_GREENROOF=',CFNAM_GREENROOF
  END IF
  WRITE(ILUOUT,*) '- - - - - - - - - - - - - - - - - - - - - - -'
  WRITE(ILUOUT,*) ' Please choose either:'
  WRITE(ILUOUT,*) ' LGREENROOF=.TRUE. or set GREENROOF fraction to zero (XUNIF_GREENROOF=0.) in namelist PGD_TEB_PAR'
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_TEB_PAR: GREENROOF flag and GREENROOF fraction not coherent')
END IF
!
!-------------------------------------------------------------------------------
!
!* gardens
!
IF (OGARDEN) THEN
  CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','GARDEN     ','TWN',CFNAM_GARDEN    ,CFTYP_GARDEN    ,XUNIF_GARDEN    ,&
        XPAR_GARDEN, LDATA_GARDEN    )
  IF (.NOT.LDATA_GARDEN) DEALLOCATE(XPAR_GARDEN)
ELSE IF ( (XUNIF_GARDEN/=0. .AND. XUNIF_GARDEN/=XUNDEF) .OR. LEN_TRIM(CFNAM_GARDEN)/=0) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) ' You chose not to include gardens in urban areas : LGARDEN=.FALSE.     '
  WRITE(ILUOUT,*) ' But            '
  IF (XUNIF_GARDEN/=0. .AND. XUNIF_GARDEN/=XUNDEF) THEN
    WRITE(ILUOUT,*) ' You also chose a garden fraction that is not zero : XUNIF_GARDEN=',XUNIF_GARDEN
  ELSE
    WRITE(ILUOUT,*) ' You also chose a garden fraction that is not zero : CFNAM_GARDEN=',CFNAM_GARDEN
  END IF
  WRITE(ILUOUT,*) '- - - - - - - - - - - - - - - - - - - - - - -'
  WRITE(ILUOUT,*) ' Please choose either:'
  WRITE(ILUOUT,*) ' LGARDEN=.TRUE. or set GARDEN fraction to zero (XUNIF_GARDEN=0.) in namelist PGD_TEB_PAR'
  WRITE(ILUOUT,*) '- - - - - - - - - - - - - - - - - - - - - - -'
  WRITE(ILUOUT,*) ' Beware that in this case, it may be necessary to change the'
  WRITE(ILUOUT,*) ' road fraction if you want to keep the same canyon aspect ratio'
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_TEB_PAR: GARDEN flag and GARDEN fraction not coherent')
END IF
!
!
!-------------------------------------------------------------------------------
IF (LHOOK)   CALL DR_HOOK('PGD_TEB_PAR',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
CONTAINS
SUBROUTINE COHERENCE_THERMAL_DATA(HTYPE,ODATA_HC,ODATA_TC,ODATA_D)
 CHARACTER(LEN=4), INTENT(IN) :: HTYPE
LOGICAL,          INTENT(IN) :: ODATA_HC
LOGICAL,          INTENT(IN) :: ODATA_TC
LOGICAL,          INTENT(IN) :: ODATA_D
!
IF (ODATA_HC .OR. ODATA_TC .OR. ODATA_D) THEN
  IF (.NOT. (ODATA_HC .AND. ODATA_TC .AND. ODATA_D)) THEN
    WRITE(ILUOUT,*) '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'
    WRITE(ILUOUT,*) 'When specifying data for thermal ',TRIM(HTYPE),' characteristics,'
    WRITE(ILUOUT,*) 'All three parameters MUST be defined:'
    WRITE(ILUOUT,*) 'Heat capacity, Thermal conductivity and depths of layers'
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) 'In your case :'
    IF (ODATA_HC) THEN
      WRITE(ILUOUT,*) 'Heat capacity is defined'
    ELSE
      WRITE(ILUOUT,*) 'Heat capacity is NOT defined'
    END IF
    IF (ODATA_TC) THEN
      WRITE(ILUOUT,*) 'Thermal conductivity is defined'
    ELSE
      WRITE(ILUOUT,*) 'Thermal conductivity is NOT defined'
    END IF
    IF (ODATA_D) THEN
      WRITE(ILUOUT,*) 'Depths of layers are defined'
    ELSE
      WRITE(ILUOUT,*) 'Depths of layers are NOT defined'
    END IF
    WRITE(ILUOUT,*) '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'
    CALL ABOR1_SFX('Heat capacity, Thermal conductivity and depths of layers MUST all be defined for '//HTYPE)
  END IF
END IF
END SUBROUTINE COHERENCE_THERMAL_DATA
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_TEB_PAR
