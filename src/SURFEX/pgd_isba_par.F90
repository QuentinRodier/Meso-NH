!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_ISBA_PAR(HPROGRAM)
!     ##############################################################
!
!!**** *PGD_ISBA_PAR* monitor for averaging and interpolations of cover fractions
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
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_ISBA_GRID_n,    ONLY : NDIM
USE MODD_ISBA_n,         ONLY : LECOCLIMAP, CISBA, CPHOTO, NGROUND_LAYER, XSOILGRID
USE MODD_DATA_ISBA_n,    ONLY : XPAR_VEGTYPE,  XPAR_LAI, XPAR_H_TREE, XPAR_DG, &
                                XPAR_ROOTFRAC, XPAR_VEG, XPAR_Z0, XPAR_EMIS, XPAR_DICE, &
                                XPAR_RSMIN, XPAR_GAMMA, XPAR_WRMAX_CF, XPAR_RGL, &
                                XPAR_CV, XPAR_Z0_O_Z0H, &
                                XPAR_ALBNIR_VEG, XPAR_ALBVIS_VEG, XPAR_ALBUV_VEG,      &
                                XPAR_ALBNIR_SOIL, XPAR_ALBVIS_SOIL, XPAR_ALBUV_SOIL,   &
                                XPAR_GMES, XPAR_BSLAI, XPAR_SEFOLD, XPAR_GC, XPAR_DMAX, &
                                XPAR_RE25, XPAR_LAIMIN, XPAR_F2I,  &
                                XPAR_CE_NITRO,XPAR_CF_NITRO,XPAR_CNA_NITRO, &
                                XPAR_GROUND_DEPTH, XPAR_ROOT_DEPTH,               &
                                XPAR_ROOT_EXTINCTION, XPAR_ROOT_LIN,              &
                                LPAR_STRESS, XPAR_IRRIG, XPAR_WATSUP, &
                                LDATA_VEGTYPE, LDATA_LAI, LDATA_H_TREE, LDATA_DG, &
                                LDATA_ROOTFRAC, LDATA_VEG, LDATA_Z0, LDATA_EMIS,  &
                                LDATA_RSMIN, LDATA_GAMMA, LDATA_WRMAX_CF, LDATA_RGL, &
                                LDATA_CV, LDATA_Z0_O_Z0H, LDATA_DICE, &
                                LDATA_ALBNIR_VEG, LDATA_ALBVIS_VEG, LDATA_ALBUV_VEG, &
                                LDATA_ALBVIS_SOIL, LDATA_ALBNIR_SOIL, LDATA_ALBUV_SOIL, &
                                LDATA_GMES, LDATA_BSLAI, LDATA_SEFOLD, LDATA_GC, LDATA_DMAX, &
                                LDATA_RE25, LDATA_LAIMIN, LDATA_F2I, &
                                LDATA_CE_NITRO,LDATA_CF_NITRO, LDATA_CNA_NITRO,&
                                LDATA_STRESS, LDATA_IRRIG, LDATA_WATSUP,              &
                                LDATA_GROUND_DEPTH, LDATA_ROOT_DEPTH,             &
                                LDATA_ROOT_EXTINCTION, LDATA_ROOT_LIN, &
                                NTIME_n=>NTIME
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_INI_VAR_FROM_DATA
USE MODI_EXTRAPOL_FIELDS
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
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
!
INTEGER               :: JVEGTYPE  ! loop counter on patch
LOGICAL               :: GPAR_STRESS   ! type of stress
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER :: NTIME
INTEGER, PARAMETER :: NTIME_MAX    = 36
INTEGER, PARAMETER :: NGROUND_MAX  = 150
INTEGER, PARAMETER :: NVEGTYPE_MAX = 12
!
REAL, DIMENSION(NVEGTYPE_MAX)   :: XSTRESS   ! 1. if defensive /0. if offensive
!
REAL, DIMENSION(NVEGTYPE_MAX)             :: XUNIF_VEGTYPE    ! fractions of each vegtypes
!
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_VEG        ! vegetation fraction
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_LAI        ! LAI
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_Z0         ! roughness length of vegetation
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_EMIS       ! emissivity
!
REAL, DIMENSION(NVEGTYPE_MAX,NGROUND_MAX)   :: XUNIF_DG         ! soil depths
REAL, DIMENSION(NVEGTYPE_MAX,NGROUND_MAX)   :: XUNIF_ROOTFRAC   ! root fraction profiles
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_GROUND_DEPTH! ground depth
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ROOT_DEPTH ! root depth
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ROOT_EXTINCTION! root extinction parameter
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ROOT_LIN   ! root linear parameter
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_DICE       ! soil ice depth for runoff
!
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_RSMIN      ! minimal stomatal resistance
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_GAMMA      ! gamma parameter
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_WRMAX_CF   ! coefficient for interception
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_RGL        !Rgl
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_CV         ! Cv
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_Z0_O_Z0H   ! ratio of roughness lengths
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ALBNIR_VEG ! albedo of vegetation (near-infra-red)
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ALBVIS_VEG ! albedo of vegetation (visible)
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ALBUV_VEG  ! albedo of vegetation (UV)
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ALBNIR_SOIL! albedo of soil (near-infra-red)
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ALBVIS_SOIL! albedo of soil (visible)
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ALBUV_SOIL ! albedo of soil (UV)
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_GMES       ! Gmes
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_BSLAI      ! Biomass over LAI
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_LAIMIN     ! minimum LAI
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_SEFOLD     ! Sefold
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_GC         ! Gc
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_DMAX       ! Dmax
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_F2I        ! F2I
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_H_TREE     ! height of trees
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_RE25       ! soil respiration parameter
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_CE_NITRO   ! CE for nitrogen
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_CF_NITRO   ! CF for nitrogen
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_CNA_NITRO  ! CNA for nitrogen
!
LOGICAL, DIMENSION(NVEGTYPE_MAX)            :: LUNIF_STRESS     ! stress type
!
! name of files containing data
!
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)           :: CFNAM_VEGTYPE    ! fractions of each vegtypes
!
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_VEG        ! vegetation fraction
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_LAI        ! LAI
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_Z0         ! roughness length
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_EMIS       ! emissivity
!
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NGROUND_MAX) :: CFNAM_DG         ! soil depth
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NGROUND_MAX) :: CFNAM_ROOTFRAC   ! root fraction profiles
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_GROUND_DEPTH! ground depth
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ROOT_DEPTH ! root depth
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ROOT_EXTINCTION! root extinction parameter
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ROOT_LIN   ! root linear parameter
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_DICE       ! soil ice depth for runoff (m)
!
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_RSMIN      ! minimal stomatal resistance
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_GAMMA      ! gamma parameter
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_WRMAX_CF   ! coefficient for interception
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_RGL        ! Rgl
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_CV         ! Cv
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_Z0_O_Z0H   ! ratio of roughness lengths
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ALBNIR_VEG ! albedo of vegetation (near-infra-red)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ALBVIS_VEG ! albedo of vegetation (visible)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ALBUV_VEG  ! albedo of vegetation (UV)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ALBNIR_SOIL! albedo of soil (near-infra-red)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ALBVIS_SOIL! albedo of soil (visible)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ALBUV_SOIL ! albedo of soil (UV)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_GMES       ! Gmes
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_BSLAI      ! Biomass over LAI
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_LAIMIN     ! minimum LAI
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_SEFOLD     ! e-folding time for senesence
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_GC         ! cuticular conductance
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_DMAX       ! Dmax
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_F2I        ! F2I
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_H_TREE     ! height of trees
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_RE25       ! soil respiration parameter
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_CE_NITRO   ! CE for nitrogen
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_CF_NITRO   ! CF for nitrogen
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_CNA_NITRO  ! CNA for nitrogen
!
! types of file containing data
!
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)           :: CFTYP_VEGTYPE    ! fractions of each vegtypes
!
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_VEG        ! vegetation fraction
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_LAI        ! LAI
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_Z0         ! roughness length
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_EMIS       ! emissivity
!
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NGROUND_MAX) :: CFTYP_DG         ! soil depth
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NGROUND_MAX) :: CFTYP_ROOTFRAC   ! root fraction profiles
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_GROUND_DEPTH! ground depth
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ROOT_DEPTH ! root depth
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ROOT_EXTINCTION! root extinction parameter
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ROOT_LIN   ! root linear parameter
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_DICE       ! soil ice depth for runoff
!
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_RSMIN      ! minimal stomatal resistance
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_GAMMA      ! gamma parameter
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_WRMAX_CF   ! coefficient for interception
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_RGL        ! Rgl
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_CV         ! Cv
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_Z0_O_Z0H   ! ratio of roughness lengths
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ALBNIR_VEG ! albedo of vegetation (near-infra-red)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ALBVIS_VEG ! albedo of vegetation (visible)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ALBUV_VEG  ! albedo of vegetation (UV)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ALBNIR_SOIL! albedo of soil (near-infra-red)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ALBVIS_SOIL! albedo of soil (visible)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ALBUV_SOIL ! albedo of soil (UV)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_GMES       ! Gmes
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_BSLAI      ! Biomass over LAI
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_LAIMIN     ! minimum LAI
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_SEFOLD     ! e-folding time for senesence
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_GC         ! cuticular conductance
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_DMAX       ! Dmax
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_F2I        ! F2I
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_H_TREE     ! height of trees
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_RE25       ! soil respiration parameter
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_CE_NITRO   ! CE for nitrogen
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_CF_NITRO   ! CF for nitrogen
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_CNA_NITRO  ! CNA for nitrogen
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
INTEGER :: JI
NAMELIST/NAM_DATA_ISBA/NTIME, XUNIF_VEGTYPE, XUNIF_DG, XUNIF_ROOTFRAC, XUNIF_DICE,                  &
                         XUNIF_GROUND_DEPTH, XUNIF_ROOT_DEPTH, XUNIF_ROOT_EXTINCTION,               &
                         XUNIF_ROOT_LIN, XUNIF_LAI, XUNIF_VEG, XUNIF_Z0, XUNIF_EMIS,                &
                         XUNIF_RSMIN, XUNIF_GAMMA, XUNIF_WRMAX_CF, XUNIF_RGL,                       &
                         XUNIF_CV, XUNIF_Z0_O_Z0H,                                                  &
                         XUNIF_ALBNIR_VEG, XUNIF_ALBVIS_VEG, XUNIF_ALBUV_VEG,                       &
                         XUNIF_ALBNIR_SOIL, XUNIF_ALBVIS_SOIL, XUNIF_ALBUV_SOIL,                    &
                         XUNIF_GMES, XUNIF_BSLAI, XUNIF_LAIMIN, XUNIF_SEFOLD,                       &
                         XUNIF_GC, XUNIF_DMAX, XUNIF_F2I, LUNIF_STRESS, XUNIF_H_TREE, XUNIF_RE25,   &
                         XUNIF_CE_NITRO, XUNIF_CF_NITRO, XUNIF_CNA_NITRO,                           &
                         CFNAM_VEG,CFNAM_LAI,CFNAM_RSMIN,CFNAM_GAMMA,CFNAM_WRMAX_CF,                &
                         CFNAM_RGL,CFNAM_CV,CFNAM_DG,CFNAM_DICE,CFNAM_Z0,CFNAM_Z0_O_Z0H,            &
                         CFNAM_ALBNIR_VEG,CFNAM_ALBVIS_VEG,CFNAM_ALBUV_VEG,                         &
                         CFNAM_ALBNIR_SOIL,CFNAM_ALBVIS_SOIL,CFNAM_ALBUV_SOIL,                      &
                         CFNAM_EMIS,                                                                &
                         CFNAM_VEGTYPE,CFNAM_ROOTFRAC,                                              &
                         CFNAM_GROUND_DEPTH,CFNAM_ROOT_DEPTH,CFNAM_ROOT_EXTINCTION,CFNAM_ROOT_LIN,  &
                         CFNAM_GMES,CFNAM_BSLAI,CFNAM_LAIMIN,CFNAM_SEFOLD,CFNAM_GC,                 &
                         CFNAM_DMAX,CFNAM_F2I, CFNAM_H_TREE,CFNAM_RE25,                             &
                         CFNAM_CE_NITRO,CFNAM_CF_NITRO,CFNAM_CNA_NITRO,                             &
                         CFTYP_VEG,CFTYP_LAI,CFTYP_RSMIN,CFTYP_GAMMA,CFTYP_WRMAX_CF,                &
                         CFTYP_RGL,CFTYP_CV,CFTYP_DG,CFTYP_DICE,CFTYP_Z0,CFTYP_Z0_O_Z0H,            &
                         CFTYP_ALBNIR_VEG,CFTYP_ALBVIS_VEG,CFTYP_ALBUV_VEG,                         &
                         CFTYP_ALBNIR_SOIL,CFTYP_ALBVIS_SOIL,CFTYP_ALBUV_SOIL,                      &
                         CFTYP_EMIS,                                                                &
                         CFTYP_VEGTYPE,CFTYP_ROOTFRAC,                                              &
                         CFTYP_GROUND_DEPTH,CFTYP_ROOT_DEPTH,CFTYP_ROOT_EXTINCTION,CFTYP_ROOT_LIN,  &
                         CFTYP_GMES,CFTYP_BSLAI,CFTYP_LAIMIN,CFTYP_SEFOLD,CFTYP_GC,                 &
                         CFTYP_DMAX,CFTYP_F2I, CFTYP_H_TREE,CFTYP_RE25,                             &
                         CFTYP_CE_NITRO,CFTYP_CF_NITRO,CFTYP_CNA_NITRO  

DATA XSTRESS /1.,1.,1.,0.,1.,0.,1.,0.,1.,0.,0.,0./
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_ISBA_PAR',0,ZHOOK_HANDLE)
NTIME                 = 36
XUNIF_VEG             = XUNDEF ! vegetation fraction
XUNIF_LAI             = XUNDEF ! LAI
XUNIF_RSMIN           = XUNDEF ! minimal stomatal resistance
XUNIF_GAMMA           = XUNDEF ! gamma parameter
XUNIF_WRMAX_CF        = XUNDEF ! coefficient for interception
XUNIF_RGL             = XUNDEF ! Rgl
XUNIF_CV              = XUNDEF ! Cv
XUNIF_DG              = XUNDEF ! soil depths
XUNIF_DICE            = XUNDEF ! soil ice depths for runoff
XUNIF_Z0              = XUNDEF ! roughness length of vegetation
XUNIF_Z0_O_Z0H        = XUNDEF ! ratio of roughness lengths
XUNIF_ALBNIR_VEG      = XUNDEF ! albedo of vegetation (near-infra-red)
XUNIF_ALBVIS_VEG      = XUNDEF ! albedo of vegetation (visible)
XUNIF_ALBUV_VEG       = XUNDEF ! albedo of vegetation (UV)
XUNIF_ALBNIR_SOIL     = XUNDEF ! albedo of soil (near-infra-red)
XUNIF_ALBVIS_SOIL     = XUNDEF ! albedo of soil (visible)
XUNIF_ALBUV_SOIL      = XUNDEF ! albedo of soil (UV)
XUNIF_EMIS            = XUNDEF ! emissivity of vegetation
XUNIF_VEGTYPE         = XUNDEF ! fractions of each vegtypes
XUNIF_ROOTFRAC        = XUNDEF ! root fraction profiles
XUNIF_GROUND_DEPTH    = XUNDEF ! ground depth
XUNIF_ROOT_DEPTH      = XUNDEF ! root depth
XUNIF_ROOT_EXTINCTION = XUNDEF ! root extinction parameter
XUNIF_ROOT_LIN        = XUNDEF ! root linear parameter
XUNIF_GMES            = XUNDEF ! Gmes
XUNIF_BSLAI           = XUNDEF ! Biomass over LAI
XUNIF_LAIMIN          = XUNDEF ! minimum LAI
XUNIF_SEFOLD          = XUNDEF ! Sefold
XUNIF_GC              = XUNDEF ! Gc
XUNIF_DMAX            = XUNDEF ! Dmax
XUNIF_F2I             = XUNDEF ! F2I
LUNIF_STRESS          = .TRUE.! stress type
XUNIF_H_TREE          = XUNDEF ! height of trees
XUNIF_RE25            = XUNDEF ! soil respiration parameter
XUNIF_CE_NITRO        = XUNDEF ! CE for nitrogen
XUNIF_CF_NITRO        = XUNDEF ! CF for nitrogen
XUNIF_CNA_NITRO       = XUNDEF ! CNA for nitrogen
!
CFNAM_VEGTYPE (:)     = '                            '
!
CFNAM_VEG  (:,:)      = '                            '
CFNAM_LAI  (:,:)      = '                            '
CFNAM_Z0   (:,:)      = '                            '
CFNAM_EMIS (:,:)      = '                            '
!
CFNAM_DG       (:,:)  = '                            '
CFNAM_ROOTFRAC (:,:)  = '                            '
CFNAM_DICE     (:)    = '                            '
!
CFNAM_GROUND_DEPTH    (:) = '                            '
CFNAM_ROOT_DEPTH      (:) = '                            '
CFNAM_ROOT_EXTINCTION (:) = '                            '
CFNAM_ROOT_LIN        (:) = '                            '
!
CFNAM_RSMIN       (:) = '                            '
CFNAM_GAMMA       (:) = '                            '
CFNAM_WRMAX_CF    (:) = '                            '
CFNAM_RGL         (:) = '                            '
CFNAM_CV          (:) = '                            '
CFNAM_Z0_O_Z0H    (:) = '                            '
CFNAM_ALBNIR_VEG  (:) = '                            '
CFNAM_ALBVIS_VEG  (:) = '                            '
CFNAM_ALBUV_VEG   (:) = '                            '
CFNAM_ALBNIR_SOIL (:) = '                            '
CFNAM_ALBVIS_SOIL (:) = '                            '
CFNAM_ALBUV_SOIL  (:) = '                            '
CFNAM_GMES        (:) = '                            '
CFNAM_BSLAI       (:) = '                            '
CFNAM_LAIMIN      (:) = '                            '
CFNAM_SEFOLD      (:) = '                            '
CFNAM_GC          (:) = '                            '
CFNAM_DMAX        (:) = '                            '
CFNAM_F2I         (:) = '                            '
CFNAM_H_TREE      (:) = '                            '
CFNAM_RE25        (:) = '                            '
CFNAM_CE_NITRO    (:) = '                            '
CFNAM_CF_NITRO    (:) = '                            '
CFNAM_CNA_NITRO   (:) = '                            '
!
CFTYP_VEGTYPE (:)     = '      '
!
CFTYP_VEG  (:,:)      = '      '
CFTYP_LAI  (:,:)      = '      '
CFTYP_Z0   (:,:)      = '      '
CFTYP_EMIS (:,:)      = '      '
!
CFTYP_DG       (:,:)  = '      '
CFTYP_ROOTFRAC (:,:)  = '      '
CFTYP_DICE     (:)    = '      '
!
CFTYP_GROUND_DEPTH    (:) = '      '
CFTYP_ROOT_DEPTH      (:) = '      '
CFTYP_ROOT_EXTINCTION (:) = '      '
CFTYP_ROOT_LIN        (:) = '      '
!
CFTYP_RSMIN       (:) = '      '
CFTYP_GAMMA       (:) = '      '
CFTYP_WRMAX_CF    (:) = '      '
CFTYP_RGL         (:) = '      '
CFTYP_CV          (:) = '      '
CFTYP_Z0_O_Z0H    (:) = '      '
CFTYP_ALBNIR_VEG  (:) = '      '
CFTYP_ALBVIS_VEG  (:) = '      '
CFTYP_ALBUV_VEG   (:) = '      '
CFTYP_ALBNIR_SOIL (:) = '      '
CFTYP_ALBVIS_SOIL (:) = '      '
CFTYP_ALBUV_SOIL  (:) = '      '
CFTYP_GMES        (:) = '      '
CFTYP_BSLAI       (:) = '      '
CFTYP_LAIMIN      (:) = '      '
CFTYP_SEFOLD      (:) = '      '
CFTYP_GC          (:) = '      '
CFTYP_DMAX        (:) = '      '
CFTYP_F2I         (:) = '      '
CFTYP_H_TREE      (:) = '      '
CFTYP_RE25        (:) = '      '
CFTYP_CE_NITRO    (:) = '      '
CFTYP_CF_NITRO    (:) = '      '
CFTYP_CNA_NITRO   (:) = '      '
!
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DATA_ISBA',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_ISBA)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
NTIME_n = NTIME
!
!-------------------------------------------------------------------------------
IF (NVEGTYPE_MAX < NVEGTYPE) THEN
  WRITE(ILUOUT,*) '------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_isba_par.f90 routine : '
  WRITE(ILUOUT,*) 'The maximum number of VEGTYPE  '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NVEGTYPE
  WRITE(ILUOUT,*) '------------------------------------'
  CALL ABOR1_SFX('PGD_ISBA_PAR: MAXIMUM NUMBER OF VEGTYPE MUST BE INCREASED IN NAMELIST DECLARATION')
END IF
!-------------------------------------------------------------------------------
IF (NGROUND_MAX < NGROUND_LAYER) THEN
  WRITE(ILUOUT,*) '------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_isba_par.f90 routine : '
  WRITE(ILUOUT,*) 'The maximum number of soil layers  '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NGROUND_LAYER
  WRITE(ILUOUT,*) '------------------------------------'
  CALL ABOR1_SFX('PGD_ISBA_PAR: MAXIMUM NUMBER OF SOIL LAYERS MUST BE INCREASED IN NAMELIST DECLARATION')
END IF
!-------------------------------------------------------------------------------
!
IF (NTIME/=36 .AND. NTIME/=12 .AND. NTIME/=2 .AND. NTIME/=1) &
   CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG VALUE FOR NTIME (POSSIBLE VALUES ARE 1, 12 OR 36')
!
!-------------------------------------------------------------------------------
!
!*    3.      Uniform fields are prescribed
!             -----------------------------
!
!-------------------------------------vegtypes-----------------------------------------
!
ALLOCATE(XPAR_VEGTYPE     (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','VEGTYPE: vegetation type','NAT',CFNAM_VEGTYPE,   &
       CFTYP_VEGTYPE,XUNIF_VEGTYPE,XPAR_VEGTYPE,LDATA_VEGTYPE)  
!
IF (.NOT.LECOCLIMAP .AND. .NOT.LDATA_VEGTYPE) THEN
  !
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in PGD field preparation of field VEGTYPE         *'
  WRITE(ILUOUT,*) '* There is no prescribed value and no input file          *'
  WRITE(ILUOUT,*) '* Without ECOCLIMAP, this field must be prescribed        *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR VEGTYPE')
  !
ELSEIF (LDATA_VEGTYPE) THEN
  !
  WHERE (XPAR_VEGTYPE(:,:)==XUNDEF) XPAR_VEGTYPE(:,:)=0.
  WHERE (XPAR_VEGTYPE(:,:)/=0.) XPAR_VEGTYPE(:,:) = XPAR_VEGTYPE(:,:) / SPREAD(SUM(XPAR_VEGTYPE(:,:),2),2,NVEGTYPE)
  !  
ENDIF
!
!--------------------------------temporal fields-----------------------------------
!
ALLOCATE(XPAR_VEG      (NDIM,NTIME,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','VEG: vegetation fraction','NAT',CFNAM_VEG,CFTYP_VEG,XUNIF_VEG,XPAR_VEG,LDATA_VEG)
IF (.NOT. LDATA_VEG) DEALLOCATE(XPAR_VEG)
!
ALLOCATE(XPAR_LAI      (NDIM,NTIME,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','LAI: leaf area index','NAT',CFNAM_LAI,CFTYP_LAI,XUNIF_LAI,XPAR_LAI,LDATA_LAI) 
IF (.NOT. LDATA_VEGTYPE .AND. .NOT. LDATA_LAI) DEALLOCATE(XPAR_LAI)
!
ALLOCATE(XPAR_Z0       (NDIM,NTIME,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'CDN','Z0: roughness length','NAT',CFNAM_Z0,CFTYP_Z0,XUNIF_Z0,XPAR_Z0,LDATA_Z0)
DO JI=1,NDIM
XPAR_Z0(JI,:,:) = XPAR_Z0(JI,:,:) * (0.5 + MOD(NINT ( (FLOAT(JI)**1.5) ),100)/100. )
END DO
IF (.NOT. LDATA_Z0) DEALLOCATE(XPAR_Z0)
!
ALLOCATE(XPAR_EMIS     (NDIM,NTIME,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','EMIS: emissivity','NAT',CFNAM_EMIS,CFTYP_EMIS,XUNIF_EMIS,XPAR_EMIS,LDATA_EMIS)
IF (.NOT. LDATA_EMIS) DEALLOCATE(XPAR_EMIS)
!
IF (.NOT.LECOCLIMAP .AND. .NOT.(LDATA_VEG .AND. LDATA_LAI .AND. LDATA_Z0 .AND. LDATA_EMIS)) THEN
  !
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in PGD field preparation of temporal fields       *'
  WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
  IF (.NOT.LDATA_VEG ) WRITE(ILUOUT,*) '* for VEG                            *'
  IF (.NOT.LDATA_LAI ) WRITE(ILUOUT,*) '* for LAI                            *'
  IF (.NOT.LDATA_Z0  ) WRITE(ILUOUT,*) '* for Z0                             *'
  IF (.NOT.LDATA_EMIS) WRITE(ILUOUT,*) '* for EMIS                           *'
  WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR TEMPORAL PARAMETERS')
  !
ENDIF
!
!--------------------------------depths fields-----------------------------------
!
ALLOCATE(XPAR_DG          (NDIM,NGROUND_LAYER,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','DG: ground depth','NAT',CFNAM_DG,CFTYP_DG,XUNIF_DG,XPAR_DG,LDATA_DG)
IF (.NOT. LDATA_VEGTYPE .AND. .NOT. LDATA_DG) DEALLOCATE(XPAR_DG)
!  
ALLOCATE(XPAR_ROOT_DEPTH    (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','ROOT_DEPTH: root depth','NAT',CFNAM_ROOT_DEPTH,CFTYP_ROOT_DEPTH,&
      XUNIF_ROOT_DEPTH,XPAR_ROOT_DEPTH,LDATA_ROOT_DEPTH)
!
ALLOCATE(XPAR_GROUND_DEPTH    (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','GROUND_DEPTH: ground depth','NAT',CFNAM_GROUND_DEPTH,CFTYP_GROUND_DEPTH,&
      XUNIF_GROUND_DEPTH,XPAR_GROUND_DEPTH,LDATA_GROUND_DEPTH)
!
IF(CISBA=='DIF')THEN 
  ! 
  ALLOCATE(XPAR_ROOTFRAC    (NDIM,NGROUND_LAYER,NVEGTYPE))  
  CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','ROOTFRAC: root fraction','NAT',CFNAM_ROOTFRAC,CFTYP_ROOTFRAC,&
        XUNIF_ROOTFRAC,XPAR_ROOTFRAC,LDATA_ROOTFRAC)
  IF (.NOT. LDATA_ROOTFRAC) DEALLOCATE(XPAR_ROOTFRAC)
  !        
  ALLOCATE(XPAR_ROOT_EXTINCTION    (NDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','ROOT_EXTINCTION: root extinction','NAT',CFNAM_ROOT_EXTINCTION,CFTYP_ROOT_EXTINCTION,&
        XUNIF_ROOT_EXTINCTION,XPAR_ROOT_EXTINCTION,LDATA_ROOT_EXTINCTION)
  IF (.NOT. LDATA_ROOT_EXTINCTION) DEALLOCATE(XPAR_ROOT_EXTINCTION)
  !        
  ALLOCATE(XPAR_ROOT_LIN    (NDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','ROOT_LIN: root linear','NAT',CFNAM_ROOT_LIN,CFTYP_ROOT_LIN,&
        XUNIF_ROOT_LIN,XPAR_ROOT_LIN,LDATA_ROOT_LIN)
  IF (.NOT. LDATA_ROOT_LIN) DEALLOCATE(XPAR_ROOT_LIN)
  !
  IF (.NOT.LECOCLIMAP) THEN
    IF(LDATA_DG .AND. .NOT.LDATA_ROOTFRAC .AND. &
       (.NOT.LDATA_ROOT_DEPTH.OR..NOT.LDATA_ROOT_EXTINCTION.OR..NOT.LDATA_ROOT_LIN)) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP, Error in PGD field preparation for ISBA-DIF           *'
      WRITE(ILUOUT,*) '* There is no prescribed value and no input file                           *'
      WRITE(ILUOUT,*) '*  (1) XUNIF_ROOTFRAC must be given.                                       *'
      WRITE(ILUOUT,*) '*  (2) Other solution, give all these fields:                              *'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_DEPTH      (soil root depth)                            *'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_EXTINCTION (root extinction parameter [Jackson 1996])   *'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_LIN        (0.05 usually; 1=uniform root distribution!!)*'
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX("PGD_ISBA_PAR: PROBLEM IN SOIL GRID COMPUTATION") 
    ELSEIF( .NOT.ALL(XSOILGRID(:)==XUNDEF) .AND. &
           (.NOT.LDATA_GROUND_DEPTH   .OR..NOT.LDATA_ROOT_DEPTH.OR. &
            .NOT.LDATA_ROOT_EXTINCTION.OR..NOT.LDATA_ROOT_LIN  )) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP, Error in PGD field preparation for ISBA-DIF           *'
      WRITE(ILUOUT,*) '* There is no prescribed value and no input file.                          *'
      WRITE(ILUOUT,*) '* When XSOILGRID is given, other field are needed :                        *'
      WRITE(ILUOUT,*) '*     - XUNIF_GROUND_DEPTH    (soil ground depth for moisture)             *'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_DEPTH      (soil root depth)                            *'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_LIN        (0.05 usually; 1=uniform root distribution!!)*'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_EXTINCTION (root extinction parameter [Jackson 1996])   *'
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX("PGD_ISBA_PAR: PROBLEM IN SOIL GRID COMPUTATION") 
    ENDIF
    IF(.NOT.LDATA_DG .AND.ALL(XSOILGRID(:)==XUNDEF))THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP, Error in PGD field preparation for ISBA-DIF           *'
      WRITE(ILUOUT,*) '* There is no prescribed value to compute vertical soil grid.              *'
      WRITE(ILUOUT,*) '* 2 solutions:                                                             *'
      WRITE(ILUOUT,*) '* (1) Give XUNIF_DG in NAM_DATA_ISBA.                                      *'
      WRITE(ILUOUT,*) '*  OR                                                                      *'
      WRITE(ILUOUT,*) '* (2) Give XSOILGRID in NAM_ISBA                                           *'
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) ' '       
      CALL ABOR1_SFX("PGD_ISBA_PAR: PROBLEM IN SOIL GRID COMPUTATION") 
    ENDIF
  ENDIF
  !
ELSE   
  !
  IF ( .NOT.LECOCLIMAP .AND. .NOT.LDATA_DG .AND. &
      (.NOT.LDATA_GROUND_DEPTH.OR..NOT.LDATA_ROOT_DEPTH) ) THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '****************************************************************************'
    WRITE(ILUOUT,*) '* Without ECOCLIMAP, Error in PGD field preparation                        *'
    WRITE(ILUOUT,*) '* There is no prescribed value and no input file                           *'
    WRITE(ILUOUT,*) '* XUNIF_DG or both XUNIF_GROUND_DEPTH and XUNIF_ROOT_DEPTH must be given.  *'
    WRITE(ILUOUT,*) '****************************************************************************'
    WRITE(ILUOUT,*) ' '       
    CALL ABOR1_SFX("PGD_ISBA_PAR: PROBLEM IN SOIL GRID COMPUTATION") 
  ENDIF
  !
ENDIF
!
ALLOCATE(XPAR_DICE        (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','DICE: ice depth for runoff','NAT',CFNAM_DICE,CFTYP_DICE,&
        XUNIF_DICE,XPAR_DICE,LDATA_DICE)
!
IF (.NOT.LECOCLIMAP.AND..NOT. LDATA_DICE) THEN
  IF(CISBA/='DIF' .AND. (LDATA_DG.OR.LDATA_ROOT_DEPTH)) THEN   
    IF(LDATA_DG)THEN
      XPAR_DICE(:,:) = MAX(0.2,0.8*XPAR_DG(:,2,:))
    ELSEIF(LDATA_ROOT_DEPTH)THEN
      XPAR_DICE(:,:) = MAX(0.2,0.8*XPAR_ROOT_DEPTH(:,:))
    ENDIF
    LDATA_DICE=.TRUE.
  ELSEIF (CISBA=='DIF') THEN
    XPAR_DICE(:,:) = 0.0
    LDATA_DICE=.TRUE.
  ELSE
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in PGD field preparation of field DICE            *'
    WRITE(ILUOUT,*) '* There is no prescribed value and no input file          *'
    WRITE(ILUOUT,*) '* Without ECOCLIMAP, this field must be prescribed        *'
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR DICE')
  ENDIF
ENDIF  
!
!---------------------classical fields---------------------------------------------
!
ALLOCATE(XPAR_RSMIN       (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'INV','RSMIN: minimal stomatal resistance','NAT',CFNAM_RSMIN,   &
       CFTYP_RSMIN,XUNIF_RSMIN,XPAR_RSMIN,LDATA_RSMIN)
IF (.NOT. LDATA_RSMIN) DEALLOCATE(XPAR_RSMIN)
!
ALLOCATE(XPAR_GAMMA       (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','GAMMA: gamma coefficient','NAT',CFNAM_GAMMA,   &
       CFTYP_GAMMA,XUNIF_GAMMA,XPAR_GAMMA,LDATA_GAMMA)
IF (.NOT. LDATA_GAMMA) DEALLOCATE(XPAR_GAMMA)
!
ALLOCATE(XPAR_WRMAX_CF    (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','WRMAX_CF: coeff. for max WR','NAT',CFNAM_WRMAX_CF,   &
       CFTYP_WRMAX_CF,XUNIF_WRMAX_CF,XPAR_WRMAX_CF,LDATA_WRMAX_CF)
IF (.NOT. LDATA_WRMAX_CF) DEALLOCATE(XPAR_WRMAX_CF)
!
ALLOCATE(XPAR_RGL         (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','RGL: max SW rad. for photosynthesis','NAT',CFNAM_RGL,   &
       CFTYP_RGL,XUNIF_RGL,XPAR_RGL,LDATA_RGL)  
IF (.NOT. LDATA_RGL) DEALLOCATE(XPAR_RGL)
!
ALLOCATE(XPAR_CV          (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'INV','CV: thermal inertia for vegetation','NAT',CFNAM_CV,   &
       CFTYP_CV,XUNIF_CV,XPAR_CV,LDATA_CV)  
IF (.NOT. LDATA_CV) DEALLOCATE(XPAR_CV)
!
ALLOCATE(XPAR_Z0_O_Z0H    (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','Z0_O_Z0H: ratio of roughness lengths','NAT',CFNAM_Z0_O_Z0H,   &
       CFTYP_Z0_O_Z0H,XUNIF_Z0_O_Z0H,XPAR_Z0_O_Z0H,LDATA_Z0_O_Z0H)  
IF (.NOT. LDATA_Z0_O_Z0H) DEALLOCATE(XPAR_Z0_O_Z0H)
!
ALLOCATE(XPAR_ALBNIR_VEG  (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','ALBNIR_VEG: NIR albedo of vegetation','NAT',CFNAM_ALBNIR_VEG,   &
       CFTYP_ALBNIR_VEG,XUNIF_ALBNIR_VEG,XPAR_ALBNIR_VEG,LDATA_ALBNIR_VEG)
IF (.NOT. LDATA_ALBNIR_VEG) DEALLOCATE(XPAR_ALBNIR_VEG)
!
ALLOCATE(XPAR_ALBVIS_VEG  (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','ALBVIS_VEG: VIS albedo of vegetation','NAT',CFNAM_ALBVIS_VEG,   &
       CFTYP_ALBVIS_VEG,XUNIF_ALBVIS_VEG,XPAR_ALBVIS_VEG,LDATA_ALBVIS_VEG)  
IF (.NOT. LDATA_ALBVIS_VEG) DEALLOCATE(XPAR_ALBVIS_VEG)
!
ALLOCATE(XPAR_ALBUV_VEG   (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','ALBUV_VEG: UV albedo of vegetation','NAT',CFNAM_ALBUV_VEG,   &
       CFTYP_ALBUV_VEG,XUNIF_ALBUV_VEG,XPAR_ALBUV_VEG,LDATA_ALBUV_VEG)
IF (.NOT. LDATA_ALBUV_VEG) DEALLOCATE(XPAR_ALBUV_VEG)
!
ALLOCATE(XPAR_ALBNIR_SOIL (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','ALBNIR_SOIL: NIR albedo of SOIL','NAT',CFNAM_ALBNIR_SOIL,   &
       CFTYP_ALBNIR_SOIL,XUNIF_ALBNIR_SOIL,XPAR_ALBNIR_SOIL,LDATA_ALBNIR_SOIL)  
IF (.NOT. LDATA_ALBNIR_SOIL) DEALLOCATE(XPAR_ALBNIR_SOIL)
!
ALLOCATE(XPAR_ALBVIS_SOIL (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','ALBVIS_SOIL: VIS albedo of SOIL','NAT',CFNAM_ALBVIS_SOIL,   &
       CFTYP_ALBVIS_SOIL,XUNIF_ALBVIS_SOIL,XPAR_ALBVIS_SOIL,LDATA_ALBVIS_SOIL)  
IF (.NOT. LDATA_ALBVIS_SOIL) DEALLOCATE(XPAR_ALBVIS_SOIL)
!
ALLOCATE(XPAR_ALBUV_SOIL  (NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','ALBUV_SOIL: UV albedo of SOIL','NAT',CFNAM_ALBUV_SOIL,   &
       CFTYP_ALBUV_SOIL,XUNIF_ALBUV_SOIL,XPAR_ALBUV_SOIL,LDATA_ALBUV_SOIL)  
IF (.NOT. LDATA_ALBUV_SOIL) DEALLOCATE(XPAR_ALBUV_SOIL)
!
IF (.NOT.LECOCLIMAP .AND. .NOT.(LDATA_RSMIN.AND.LDATA_GAMMA.AND.LDATA_WRMAX_CF.AND.LDATA_RGL &
         .AND.LDATA_CV.AND.LDATA_Z0_O_Z0H.AND.LDATA_ALBNIR_VEG.AND.LDATA_ALBVIS_VEG.AND.&
         LDATA_ALBUV_VEG.AND.LDATA_ALBNIR_SOIL.AND.LDATA_ALBVIS_SOIL.AND.LDATA_ALBUV_SOIL)) THEN
  !
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in PGD field preparation of classical fields      *'
  WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
  IF (.NOT.LDATA_RSMIN       ) WRITE(ILUOUT,*) '* for RSMIN                  *'
  IF (.NOT.LDATA_GAMMA       ) WRITE(ILUOUT,*) '* for GAMMA                  *'
  IF (.NOT.LDATA_WRMAX_CF    ) WRITE(ILUOUT,*) '* for WRMAX_CF               *'
  IF (.NOT.LDATA_RGL         ) WRITE(ILUOUT,*) '* for RGL                    *'
  IF (.NOT.LDATA_CV          ) WRITE(ILUOUT,*) '* for CV                     *'
  IF (.NOT.LDATA_Z0_O_Z0H    ) WRITE(ILUOUT,*) '* for Z0_O_Z0H               *'
  IF (.NOT.LDATA_ALBNIR_VEG  ) WRITE(ILUOUT,*) '* for ALBNIR_VEG             *'
  IF (.NOT.LDATA_ALBVIS_VEG  ) WRITE(ILUOUT,*) '* for ALBVIS_VEG             *'
  IF (.NOT.LDATA_ALBUV_VEG   ) WRITE(ILUOUT,*) '* for ALBUV_VEG              *'
  IF (.NOT.LDATA_ALBNIR_SOIL ) WRITE(ILUOUT,*) '* for ALBNIR_SOIL            *'
  IF (.NOT.LDATA_ALBVIS_SOIL ) WRITE(ILUOUT,*) '* for ALBVIS_SOIL            *'
  IF (.NOT.LDATA_ALBUV_SOIL  ) WRITE(ILUOUT,*) '* for ALBUV_SOIL             *'
  WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR CLASSICAL PARAMETERS')
  !
ENDIF
!
!--------------------------------------AGS parameters----------------------------
!
IF (CPHOTO/='NON' .OR. (.NOT.LDATA_Z0.AND.(LDATA_LAI.OR.LDATA_VEGTYPE))) THEN
  !
  ALLOCATE(XPAR_H_TREE      (NDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','H_TREE: height of trees','NAT',CFNAM_H_TREE,   &
         CFTYP_H_TREE,XUNIF_H_TREE,XPAR_H_TREE,LDATA_H_TREE)  
  IF (.NOT. LDATA_VEGTYPE .AND. .NOT. LDATA_H_TREE) DEALLOCATE(XPAR_H_TREE)
  !
ENDIF
  
IF (CPHOTO/='NON') THEN
  !
  ALLOCATE(XPAR_RE25        (NDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','RE25: ecosystem respiration','NAT',CFNAM_RE25,   &
       CFTYP_RE25,XUNIF_RE25,XPAR_RE25,LDATA_RE25)  
  IF (.NOT. LDATA_RE25) DEALLOCATE(XPAR_RE25)  
  !
  ALLOCATE(XPAR_LAIMIN      (NDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','LAIMIN: minimum LAI','NAT',CFNAM_LAIMIN,   &
         CFTYP_LAIMIN,XUNIF_LAIMIN,XPAR_LAIMIN,LDATA_LAIMIN)  
  IF (.NOT. LDATA_LAIMIN) DEALLOCATE(XPAR_LAIMIN)          
  !
  ALLOCATE(XPAR_BSLAI       (NDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','BSLAI: biomass over LAI','NAT',CFNAM_BSLAI,   &
       CFTYP_BSLAI,XUNIF_BSLAI,XPAR_BSLAI,LDATA_BSLAI)  
  IF (.NOT. LDATA_BSLAI) DEALLOCATE(XPAR_BSLAI)
  !
  ALLOCATE(XPAR_SEFOLD      (NDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','SEFOLD: e-folding time for senescence','NAT',CFNAM_SEFOLD,   &
         CFTYP_SEFOLD,XUNIF_SEFOLD,XPAR_SEFOLD,LDATA_SEFOLD)  
  IF (.NOT. LDATA_SEFOLD) DEALLOCATE(XPAR_SEFOLD)
  !  
  ALLOCATE(XPAR_GMES        (NDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','GMES: mesophyl conductance','NAT',CFNAM_GMES,   &
         CFTYP_GMES,XUNIF_GMES,XPAR_GMES,LDATA_GMES)
  IF (.NOT. LDATA_GMES) DEALLOCATE(XPAR_GMES)
  !
  ALLOCATE(XPAR_GC          (NDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','GC: cuticular conductance','NAT',CFNAM_GC,   &
       CFTYP_GC,XUNIF_GC,XPAR_GC,LDATA_GC)  
  IF (.NOT. LDATA_GC) DEALLOCATE(XPAR_GC)
  !
  IF (.NOT.LECOCLIMAP .AND. .NOT.(LDATA_H_TREE.AND.LDATA_RE25.AND.LDATA_LAIMIN.AND.&
           LDATA_BSLAI.AND.LDATA_SEFOLD.AND.LDATA_GMES.AND.LDATA_GC)) THEN
    !
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in PGD field preparation of AGS fields            *'
    WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
    IF (.NOT.LDATA_H_TREE ) WRITE(ILUOUT,*) '* for H_TREE                      *'
    IF (.NOT.LDATA_RE25   ) WRITE(ILUOUT,*) '* for RE25                        *'    
    IF (.NOT.LDATA_LAIMIN ) WRITE(ILUOUT,*) '* for LAIMIN                      *'    
    IF (.NOT.LDATA_BSLAI  ) WRITE(ILUOUT,*) '* for BSLAI                       *'
    IF (.NOT.LDATA_SEFOLD ) WRITE(ILUOUT,*) '* for SEFOLD                      *'
    IF (.NOT.LDATA_GMES   ) WRITE(ILUOUT,*) '* for GMES                        *'
    IF (.NOT.LDATA_GC     ) WRITE(ILUOUT,*) '* for GC                          *'
    WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR AGS PARAMETERS')
    !
  ENDIF
  !  
  !--------------------------------------AGS Stress parameters----------------------------
  !  
  IF (CPHOTO/='AGS' .AND. CPHOTO/='LAI') THEN
    !
    ALLOCATE(XPAR_F2I         (NDIM,NVEGTYPE))
    CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','F2I: critical normalized soil water content (stress)','NAT',CFNAM_F2I,   &
         CFTYP_F2I,XUNIF_F2I,XPAR_F2I,LDATA_F2I)
    IF (.NOT. LDATA_F2I) DEALLOCATE(XPAR_F2I)
    !
    ALLOCATE(XPAR_DMAX        (NDIM,NVEGTYPE))
    CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','DMAX: maximum air saturation deficit','NAT',CFNAM_DMAX,   &
         CFTYP_DMAX,XUNIF_DMAX,XPAR_DMAX,LDATA_DMAX)  
    IF (.NOT. LDATA_DMAX) DEALLOCATE(XPAR_DMAX)
    !
    ALLOCATE(LPAR_STRESS      (NDIM,NVEGTYPE))
    DO JVEGTYPE=1,NVEGTYPE
      GPAR_STRESS = LUNIF_STRESS(JVEGTYPE)
      IF (XSTRESS(JVEGTYPE)<1.) GPAR_STRESS = .FALSE.
      IF (XSTRESS(JVEGTYPE)==1. .AND. .NOT.GPAR_STRESS) LDATA_STRESS=.TRUE.
      LPAR_STRESS(:,JVEGTYPE) = GPAR_STRESS
    ENDDO
    IF (.NOT. LDATA_STRESS) DEALLOCATE(LPAR_STRESS)
    !
    IF (.NOT.LECOCLIMAP .AND. .NOT.(LDATA_F2I.AND.LDATA_DMAX)) THEN
      !
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '***********************************************************'
      WRITE(ILUOUT,*) '* Error in PGD field preparation of AGS Stress fields     *'
      WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
      IF (.NOT.LDATA_F2I  ) WRITE(ILUOUT,*) '* for F2I                           *'
      IF (.NOT.LDATA_DMAX ) WRITE(ILUOUT,*) '* for DMAX                          *'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
      WRITE(ILUOUT,*) '***********************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR AGS STRESS PARAMETERS')
      !
    ENDIF
    !    
    !--------------------------------------AGS Nitrogen parameters----------------------------
    !  
    IF (CPHOTO=='NIT' .OR. CPHOTO=='NCB') THEN
      !
      ALLOCATE(XPAR_CE_NITRO    (NDIM,NVEGTYPE))
      CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','CE_NITRO: leaf area ratio sensitivity to nitrogen ccion','NAT',&
                CFNAM_CE_NITRO, CFTYP_CE_NITRO,XUNIF_CE_NITRO,XPAR_CE_NITRO,LDATA_CE_NITRO)  
      IF (.NOT. LDATA_CE_NITRO) DEALLOCATE(XPAR_CE_NITRO)
      !
      ALLOCATE(XPAR_CF_NITRO    (NDIM,NVEGTYPE))
      CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','CF_NITRO: lethal minimum value of leaf area ratio','NAT',&
                CFNAM_CF_NITRO,CFTYP_CF_NITRO,XUNIF_CF_NITRO,XPAR_CF_NITRO,LDATA_CF_NITRO)
      IF (.NOT. LDATA_CF_NITRO) DEALLOCATE(XPAR_CF_NITRO)
      !
      ALLOCATE(XPAR_CNA_NITRO   (NDIM,NVEGTYPE))
      CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','CNA_NITRO: nitrogen ccion of active biomass','NAT',&
                CFNAM_CNA_NITRO,CFTYP_CNA_NITRO,XUNIF_CNA_NITRO,XPAR_CNA_NITRO,LDATA_CNA_NITRO)
      IF (.NOT. LDATA_CNA_NITRO) DEALLOCATE(XPAR_CNA_NITRO)
      !
      IF (.NOT.LECOCLIMAP .AND. .NOT.(LDATA_CE_NITRO.AND.LDATA_CF_NITRO.AND.LDATA_CNA_NITRO)) THEN
        !
        WRITE(ILUOUT,*) ' '
        WRITE(ILUOUT,*) '***********************************************************'
        WRITE(ILUOUT,*) '* Error in PGD field preparation of AGS Nitrogen fields   *'
        WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
        IF (.NOT.LDATA_CE_NITRO  ) WRITE(ILUOUT,*) '* for CE_NITRO                 *'
        IF (.NOT.LDATA_CF_NITRO  ) WRITE(ILUOUT,*) '* for CF_NITRO                 *'
        IF (.NOT.LDATA_CNA_NITRO ) WRITE(ILUOUT,*) '* for CNA_NITRO                *'
        WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
        WRITE(ILUOUT,*) '***********************************************************'
        WRITE(ILUOUT,*) ' '
        CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR AGS NITROGEN PARAMETERS')
        !
      ENDIF
      !    
    ENDIF
    !
  ENDIF
  !
ENDIF
!
!--------------------------------------irrigation parameters----------------------------
!
LDATA_IRRIG=.FALSE.
LDATA_WATSUP=.FALSE.
!
!----------------------------------------------------------------------------------------
!
IF (LECOCLIMAP .AND. LDATA_VEGTYPE) THEN
  !
  ALLOCATE(XPAR_IRRIG       (NDIM,NTIME,NVEGTYPE))
  ALLOCATE(XPAR_WATSUP      (NDIM,NTIME,NVEGTYPE)) 
  !  
  CALL EXTRAPOL_FIELDS(HPROGRAM,ILUOUT)
  !
  IF (.NOT. LDATA_LAI) DEALLOCATE(XPAR_LAI)
  IF (.NOT. LDATA_H_TREE .AND. CPHOTO/='NON') DEALLOCATE(XPAR_H_TREE)
  IF (.NOT. LDATA_DG) DEALLOCATE(XPAR_DG)
  IF (.NOT. LDATA_ROOT_DEPTH) DEALLOCATE(XPAR_ROOT_DEPTH)
  IF (.NOT. LDATA_GROUND_DEPTH) DEALLOCATE(XPAR_GROUND_DEPTH)
  !
ENDIF
!
!----------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_ISBA_PAR',1,ZHOOK_HANDLE)
!
END SUBROUTINE PGD_ISBA_PAR
