!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_ISBA_PAR (DTCO, DGU, UG, U, USS, DTI, I, IG, &
                               HPROGRAM)
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
!!                 05/2012  R. Alkama   : 19 vegtypes rather than 12    
!!       Modified 02/2012,  P. Samuelsson: MEB
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
!
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(IG%NDIM,NVEGTYPE)     :: ZROOTFRACGV,TEST,TEST2,TEST3
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
INTEGER               :: IHGROUND_LAYER ! Half number of NGROUND_LAYER
INTEGER               :: IIH       ! Ground layer counter
LOGICAL               :: GFOUND    ! true if namelist is found
!
INTEGER               :: JVEGTYPE  ! loop counter on patch
LOGICAL               :: GPAR_STRESS   ! type of stress
!
INTEGER               :: ISIZE_LMEB_PATCH  ! Number of patches with MEB=true
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER :: NTIME
INTEGER, PARAMETER :: NTIME_MAX    = 36
INTEGER, PARAMETER :: NGROUND_MAX  = 150
INTEGER, PARAMETER :: NVEGTYPE_MAX = 19
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
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_LAIGV
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_Z0LITTER
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_GNDLITTER
REAL, DIMENSION(NVEGTYPE_MAX,NGROUND_MAX)   :: XUNIF_ROOTFRACGV
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_RSMINGV
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_GAMMAGV
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_WRMAX_CFGV
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_RGLGV
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_H_VEG
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ROOT_EXTINCTIONGV! root extinction parameter
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
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_LAIGV
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_Z0LITTER
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_GNDLITTER
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NGROUND_MAX) :: CFNAM_ROOTFRACGV
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_RSMINGV
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_GAMMAGV
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_WRMAX_CFGV
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_RGLGV
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_H_VEG
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ROOT_EXTINCTIONGV! root extinction parameter
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
!
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_LAIGV
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_Z0LITTER
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_GNDLITTER
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NGROUND_MAX) :: CFTYP_ROOTFRACGV
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_RSMINGV
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_GAMMAGV
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_WRMAX_CFGV
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_RGLGV
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_H_VEG
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ROOT_EXTINCTIONGV! root extinction parameter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
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
                         CFTYP_CE_NITRO,CFTYP_CF_NITRO,CFTYP_CNA_NITRO,                             &
                         XUNIF_LAIGV, XUNIF_Z0LITTER, XUNIF_ROOTFRACGV, XUNIF_GNDLITTER, XUNIF_RSMINGV, &
                         XUNIF_GAMMAGV, XUNIF_WRMAX_CFGV, XUNIF_RGLGV,                              &
                         XUNIF_H_VEG, XUNIF_ROOT_EXTINCTIONGV,                                      &
                         CFNAM_LAIGV, CFNAM_Z0LITTER, CFNAM_ROOTFRACGV, CFNAM_GNDLITTER, CFNAM_RSMINGV, &
                         CFNAM_GAMMAGV, CFNAM_WRMAX_CFGV, CFNAM_RGLGV,                              &
                         CFNAM_H_VEG, CFNAM_ROOT_EXTINCTIONGV,                                      &
                         CFTYP_LAIGV, CFTYP_Z0LITTER, CFTYP_ROOTFRACGV, CFTYP_GNDLITTER, CFTYP_RSMINGV, &
                         CFTYP_GAMMAGV, CFTYP_WRMAX_CFGV, CFTYP_RGLGV,                              &
                         CFTYP_H_VEG, CFTYP_ROOT_EXTINCTIONGV

DATA XSTRESS /1.,1.,1.,0.,1.,0.,1.,0.,1.,0.,0.,0.,0.,0.,1.,0.,1.,0.,0./
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
XUNIF_LAIGV           = XUNDEF
XUNIF_Z0LITTER        = XUNDEF
XUNIF_ROOTFRACGV      = XUNDEF
XUNIF_GNDLITTER       = XUNDEF
XUNIF_RSMINGV         = XUNDEF
XUNIF_GAMMAGV         = XUNDEF
XUNIF_WRMAX_CFGV      = XUNDEF
XUNIF_RGLGV           = XUNDEF
XUNIF_H_VEG           = XUNDEF
XUNIF_ROOT_EXTINCTIONGV = XUNDEF ! root extinction parameter
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
 CFNAM_LAIGV       (:,:) = '                            '
 CFNAM_Z0LITTER    (:,:) = '                            '
 CFNAM_GNDLITTER   (:,:) = '                            '
 CFNAM_ROOTFRACGV  (:,:) = '                            '
 CFNAM_RSMINGV     (:) = '                            '
 CFNAM_GAMMAGV     (:) = '                            '
 CFNAM_WRMAX_CFGV  (:) = '                            '
 CFNAM_RGLGV       (:) = '                            '
 CFNAM_H_VEG       (:,:) = '                            '
 CFNAM_ROOT_EXTINCTIONGV (:) = '                            '

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
 CFTYP_LAIGV       (:,:) = '      '
 CFTYP_Z0LITTER    (:,:) = '      '
 CFTYP_GNDLITTER   (:,:) = '      '
 CFTYP_ROOTFRACGV  (:,:) = '      '
 CFTYP_RSMINGV     (:) = '      '
 CFTYP_GAMMAGV     (:) = '      '
 CFTYP_WRMAX_CFGV  (:) = '      '
 CFTYP_RGLGV       (:) = '      '
 CFTYP_H_VEG       (:,:) = '      '
 CFTYP_ROOT_EXTINCTIONGV (:) = '      '
!
ISIZE_LMEB_PATCH=COUNT(I%LMEB_PATCH(:))
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
DTI%NTIME = NTIME
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
IF (NGROUND_MAX < I%NGROUND_LAYER) THEN
  WRITE(ILUOUT,*) '------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_isba_par.f90 routine : '
  WRITE(ILUOUT,*) 'The maximum number of soil layers  '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', I%NGROUND_LAYER
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
ALLOCATE(DTI%XPAR_VEGTYPE     (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','VEGTYPE: vegetation type','NAT',CFNAM_VEGTYPE,   &
       CFTYP_VEGTYPE,XUNIF_VEGTYPE,DTI%XPAR_VEGTYPE,DTI%LDATA_VEGTYPE)  
!
IF (.NOT.I%LECOCLIMAP .AND. .NOT.DTI%LDATA_VEGTYPE) THEN
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
ELSEIF (DTI%LDATA_VEGTYPE) THEN
  !
  WHERE (DTI%XPAR_VEGTYPE(:,:)==XUNDEF) DTI%XPAR_VEGTYPE(:,:)=0.
  WHERE (DTI%XPAR_VEGTYPE(:,:)/=0.) DTI%XPAR_VEGTYPE(:,:) = DTI%XPAR_VEGTYPE(:,:) / &
                                                    SPREAD(SUM(DTI%XPAR_VEGTYPE(:,:),2),2,NVEGTYPE)
  !  
ENDIF
!
!--------------------------------temporal fields-----------------------------------
!
ALLOCATE(DTI%XPAR_VEG      (IG%NDIM,NTIME,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','VEG: vegetation fraction','NAT',CFNAM_VEG,CFTYP_VEG,XUNIF_VEG, &
                        DTI%XPAR_VEG,DTI%LDATA_VEG)
IF (.NOT. DTI%LDATA_VEG) DEALLOCATE(DTI%XPAR_VEG)
!
ALLOCATE(DTI%XPAR_LAI      (IG%NDIM,NTIME,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','LAI: leaf area index','NAT',CFNAM_LAI,CFTYP_LAI,XUNIF_LAI, &
                        DTI%XPAR_LAI,DTI%LDATA_LAI) 
IF (.NOT. DTI%LDATA_VEGTYPE .AND. .NOT. DTI%LDATA_LAI) DEALLOCATE(DTI%XPAR_LAI)
!
ALLOCATE(DTI%XPAR_H_VEG       (IG%NDIM,NTIME,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','H_VEG: vegetation height','NAT',CFNAM_H_VEG,CFTYP_H_VEG,XUNIF_H_VEG, &
                        DTI%XPAR_H_VEG,DTI%LDATA_H_VEG) 
IF (.NOT. DTI%LDATA_H_VEG) DEALLOCATE(DTI%XPAR_H_VEG)
!
ALLOCATE(DTI%XPAR_Z0       (IG%NDIM,NTIME,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'CDN','Z0: roughness length','NAT',CFNAM_Z0,CFTYP_Z0,XUNIF_Z0, &
                        DTI%XPAR_Z0,DTI%LDATA_Z0)
IF (.NOT. DTI%LDATA_Z0) DEALLOCATE(DTI%XPAR_Z0)
!
ALLOCATE(DTI%XPAR_EMIS     (IG%NDIM,NTIME,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','EMIS: emissivity','NAT',CFNAM_EMIS,CFTYP_EMIS,XUNIF_EMIS, &
                        DTI%XPAR_EMIS,DTI%LDATA_EMIS)
IF (.NOT. DTI%LDATA_EMIS) DEALLOCATE(DTI%XPAR_EMIS)
!
IF (.NOT.I%LECOCLIMAP .AND. .NOT.(DTI%LDATA_VEG .AND. DTI%LDATA_LAI .AND. DTI%LDATA_Z0 .AND. DTI%LDATA_EMIS)) THEN
  !
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in PGD field preparation of temporal fields       *'
  WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
  IF (.NOT.DTI%LDATA_VEG ) WRITE(ILUOUT,*) '* for VEG                            *'
  IF (.NOT.DTI%LDATA_LAI ) WRITE(ILUOUT,*) '* for LAI                            *'
  IF (.NOT.DTI%LDATA_Z0  ) WRITE(ILUOUT,*) '* for Z0                             *'
  IF (.NOT.DTI%LDATA_EMIS) WRITE(ILUOUT,*) '* for EMIS                           *'
  WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR TEMPORAL PARAMETERS')
  !
ENDIF
!
! ------------ Begin MEB parameters ---------------------
IF(ISIZE_LMEB_PATCH>0) THEN
  !
  ALLOCATE(DTI%XPAR_LAIGV       (IG%NDIM,NTIME,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','LAIGV: GV leaf area index','NAT', &
                         CFNAM_LAIGV,CFTYP_LAIGV,XUNIF_LAIGV,DTI%XPAR_LAIGV,DTI%LDATA_LAIGV) 
  IF (.NOT. DTI%LDATA_LAIGV) DEALLOCATE(DTI%XPAR_LAIGV)
  !
  ALLOCATE(DTI%XPAR_GNDLITTER   (IG%NDIM,NTIME,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','GNDLITTER: ground litter fraction','NAT',&
                         CFNAM_GNDLITTER,CFTYP_GNDLITTER,XUNIF_GNDLITTER,DTI%XPAR_GNDLITTER,DTI%LDATA_GNDLITTER)
  IF (.NOT. DTI%LDATA_GNDLITTER) DEALLOCATE(DTI%XPAR_GNDLITTER)
  !
  ALLOCATE(DTI%XPAR_Z0LITTER        (IG%NDIM,NTIME,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'CDN','Z0LITTER: ground litter roughness length','NAT', &
                         CFNAM_Z0LITTER,CFTYP_Z0LITTER,XUNIF_Z0LITTER,DTI%XPAR_Z0LITTER,DTI%LDATA_Z0LITTER)
  IF (.NOT. DTI%LDATA_Z0LITTER) DEALLOCATE(DTI%XPAR_Z0LITTER)
  !
ENDIF
! ------------ End MEB parameters ---------------------
!
!--------------------------------depths fields-----------------------------------
!
ALLOCATE(DTI%XPAR_DG          (IG%NDIM,I%NGROUND_LAYER,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','DG: ground depth','NAT',CFNAM_DG,CFTYP_DG,XUNIF_DG,DTI%XPAR_DG,DTI%LDATA_DG)
IF (.NOT. DTI%LDATA_VEGTYPE .AND. .NOT. DTI%LDATA_DG) DEALLOCATE(DTI%XPAR_DG)
!  
ALLOCATE(DTI%XPAR_ROOT_DEPTH    (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','ROOT_DEPTH: root depth','NAT',CFNAM_ROOT_DEPTH,CFTYP_ROOT_DEPTH,&
      XUNIF_ROOT_DEPTH,DTI%XPAR_ROOT_DEPTH,DTI%LDATA_ROOT_DEPTH)
!
ALLOCATE(DTI%XPAR_GROUND_DEPTH    (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','GROUND_DEPTH: ground depth','NAT',CFNAM_GROUND_DEPTH,CFTYP_GROUND_DEPTH,&
      XUNIF_GROUND_DEPTH,DTI%XPAR_GROUND_DEPTH,DTI%LDATA_GROUND_DEPTH)
!
IF(I%CISBA=='DIF')THEN 
  ! 
  ALLOCATE(DTI%XPAR_ROOTFRAC    (IG%NDIM,I%NGROUND_LAYER,NVEGTYPE))  
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','ROOTFRAC: root fraction','NAT',CFNAM_ROOTFRAC,CFTYP_ROOTFRAC,&
        XUNIF_ROOTFRAC,DTI%XPAR_ROOTFRAC,DTI%LDATA_ROOTFRAC)
  IF (.NOT. DTI%LDATA_ROOTFRAC) DEALLOCATE(DTI%XPAR_ROOTFRAC)
  !
  ALLOCATE(DTI%XPAR_ROOT_EXTINCTION    (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','ROOT_EXTINCTION: root extinction','NAT',CFNAM_ROOT_EXTINCTION,CFTYP_ROOT_EXTINCTION,&
        XUNIF_ROOT_EXTINCTION,DTI%XPAR_ROOT_EXTINCTION,DTI%LDATA_ROOT_EXTINCTION)
  IF (.NOT. DTI%LDATA_ROOT_EXTINCTION) DEALLOCATE(DTI%XPAR_ROOT_EXTINCTION)
  !        
  ALLOCATE(DTI%XPAR_ROOT_LIN    (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','ROOT_LIN: root linear','NAT',CFNAM_ROOT_LIN,CFTYP_ROOT_LIN,&
        XUNIF_ROOT_LIN,DTI%XPAR_ROOT_LIN,DTI%LDATA_ROOT_LIN)
  IF (.NOT. DTI%LDATA_ROOT_LIN) DEALLOCATE(DTI%XPAR_ROOT_LIN)
  !
  ! ------------ Begin MEB parameters ---------------------
  IF(ISIZE_LMEB_PATCH>0) THEN
    !
    ALLOCATE(DTI%XPAR_ROOTFRACGV  (IG%NDIM,I%NGROUND_LAYER,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','ROOTFRACGV: GV root fraction','NAT',CFNAM_ROOTFRACGV,CFTYP_ROOTFRACGV,&
            XUNIF_ROOTFRACGV,DTI%XPAR_ROOTFRACGV,DTI%LDATA_ROOTFRACGV)
    IF (.NOT. DTI%LDATA_ROOTFRACGV) DEALLOCATE(DTI%XPAR_ROOTFRACGV)
    !
    !  Apply a function of XPAR_ROOTFRAC on XPAR_ROOTFRACGV if XUNIF_ROOTFRACGV is undefined
    IF (.NOT. DTI%LDATA_ROOTFRACGV .AND. DTI%LDATA_ROOTFRAC)THEN
      ALLOCATE(DTI%XPAR_ROOTFRACGV  (IG%NDIM,I%NGROUND_LAYER,NVEGTYPE))
      IHGROUND_LAYER=INT(CEILING(REAL(I%NGROUND_LAYER)/2.0))
      DTI%XPAR_ROOTFRACGV=0.
      DO IIH=1,IHGROUND_LAYER
        DTI%XPAR_ROOTFRACGV(:,IIH,:)=DTI%XPAR_ROOTFRAC(:,IIH,:)
      ENDDO
      ZROOTFRACGV(:,:)=SUM(DTI%XPAR_ROOTFRACGV,DIM=2)
      DO IIH=1,IHGROUND_LAYER
        TEST=RESHAPE(DTI%XPAR_ROOTFRACGV(:,IIH,:),(/IG%NDIM,NVEGTYPE/))
        TEST2=1.
        WHERE(ZROOTFRACGV>0.)TEST2 = 1./ZROOTFRACGV
        TEST3=TEST * TEST2
        DTI%XPAR_ROOTFRACGV(:,IIH,:)=TEST3
      ENDDO
      DTI%LDATA_ROOTFRACGV = .TRUE.
    ENDIF
    !                                     '                                '
    ALLOCATE(DTI%XPAR_ROOT_EXTINCTIONGV(IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','ROOT_EXTINCTIONGV: root ext gv  ','NAT',CFNAM_ROOT_EXTINCTIONGV,CFTYP_ROOT_EXTINCTIONGV,&
          XUNIF_ROOT_EXTINCTIONGV,DTI%XPAR_ROOT_EXTINCTIONGV,DTI%LDATA_ROOT_EXTINCTIONGV)
    IF (.NOT. DTI%LDATA_ROOT_EXTINCTIONGV) DEALLOCATE(DTI%XPAR_ROOT_EXTINCTIONGV)
    !
    IF (.NOT. DTI%LDATA_ROOT_EXTINCTIONGV .AND. DTI%LDATA_ROOT_EXTINCTION)THEN
      ALLOCATE(DTI%XPAR_ROOT_EXTINCTIONGV(IG%NDIM,NVEGTYPE))
      DTI%XPAR_ROOT_EXTINCTIONGV = DTI%XPAR_ROOT_EXTINCTION
      DTI%LDATA_ROOT_EXTINCTIONGV = .TRUE.
    ENDIF
    !
  ENDIF
  ! ------------ End MEB parameters ---------------------
  !
  IF (.NOT.I%LECOCLIMAP) THEN
    IF(DTI%LDATA_DG .AND. .NOT.DTI%LDATA_ROOTFRAC .AND. &
       (.NOT.DTI%LDATA_ROOT_DEPTH.OR..NOT.DTI%LDATA_ROOT_EXTINCTION.OR..NOT.DTI%LDATA_ROOT_LIN)) THEN
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
    ELSEIF( .NOT.ALL(I%XSOILGRID(:)==XUNDEF) .AND. &
           (.NOT.DTI%LDATA_GROUND_DEPTH   .OR..NOT.DTI%LDATA_ROOT_DEPTH.OR. &
            .NOT.DTI%LDATA_ROOT_EXTINCTION.OR..NOT.DTI%LDATA_ROOT_LIN  )) THEN
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
    IF(.NOT.DTI%LDATA_DG .AND.ALL(I%XSOILGRID(:)==XUNDEF))THEN
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
  IF ( .NOT.I%LECOCLIMAP .AND. .NOT.DTI%LDATA_DG .AND. &
      (.NOT.DTI%LDATA_GROUND_DEPTH.OR..NOT.DTI%LDATA_ROOT_DEPTH) ) THEN
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
ALLOCATE(DTI%XPAR_DICE        (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','DICE: ice depth for runoff','NAT',CFNAM_DICE,CFTYP_DICE,&
        XUNIF_DICE,DTI%XPAR_DICE,DTI%LDATA_DICE)
!
IF (.NOT.I%LECOCLIMAP.AND..NOT. DTI%LDATA_DICE) THEN
  IF(I%CISBA/='DIF' .AND. (DTI%LDATA_DG.OR.DTI%LDATA_ROOT_DEPTH)) THEN   
    IF(DTI%LDATA_DG)THEN
      DTI%XPAR_DICE(:,:) = MAX(0.2,0.8*DTI%XPAR_DG(:,2,:))
    ELSEIF(DTI%LDATA_ROOT_DEPTH)THEN
      DTI%XPAR_DICE(:,:) = MAX(0.2,0.8*DTI%XPAR_ROOT_DEPTH(:,:))
    ENDIF
    DTI%LDATA_DICE=.TRUE.
  ELSEIF (I%CISBA=='DIF') THEN
    DTI%XPAR_DICE(:,:) = 0.0
    DTI%LDATA_DICE=.TRUE.
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
ALLOCATE(DTI%XPAR_RSMIN       (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'INV','RSMIN: minimal stomatal resistance','NAT',CFNAM_RSMIN,   &
       CFTYP_RSMIN,XUNIF_RSMIN,DTI%XPAR_RSMIN,DTI%LDATA_RSMIN)
IF (.NOT. DTI%LDATA_RSMIN) DEALLOCATE(DTI%XPAR_RSMIN)
!
ALLOCATE(DTI%XPAR_GAMMA       (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','GAMMA: gamma coefficient','NAT',CFNAM_GAMMA,   &
       CFTYP_GAMMA,XUNIF_GAMMA,DTI%XPAR_GAMMA,DTI%LDATA_GAMMA)
IF (.NOT. DTI%LDATA_GAMMA) DEALLOCATE(DTI%XPAR_GAMMA)
!
ALLOCATE(DTI%XPAR_WRMAX_CF    (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','WRMAX_CF: coeff. for max WR','NAT',CFNAM_WRMAX_CF,   &
       CFTYP_WRMAX_CF,XUNIF_WRMAX_CF,DTI%XPAR_WRMAX_CF,DTI%LDATA_WRMAX_CF)
IF (.NOT. DTI%LDATA_WRMAX_CF) DEALLOCATE(DTI%XPAR_WRMAX_CF)
!
ALLOCATE(DTI%XPAR_RGL         (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','RGL: max SW rad. for photosynthesis','NAT',CFNAM_RGL,   &
       CFTYP_RGL,XUNIF_RGL,DTI%XPAR_RGL,DTI%LDATA_RGL)  
IF (.NOT. DTI%LDATA_RGL) DEALLOCATE(DTI%XPAR_RGL)
!
ALLOCATE(DTI%XPAR_CV          (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'INV','CV: thermal inertia for vegetation','NAT',CFNAM_CV,   &
       CFTYP_CV,XUNIF_CV,DTI%XPAR_CV,DTI%LDATA_CV)  
IF (.NOT. DTI%LDATA_CV) DEALLOCATE(DTI%XPAR_CV)
!
ALLOCATE(DTI%XPAR_Z0_O_Z0H    (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','Z0_O_Z0H: ratio of roughness lengths','NAT',CFNAM_Z0_O_Z0H,   &
       CFTYP_Z0_O_Z0H,XUNIF_Z0_O_Z0H,DTI%XPAR_Z0_O_Z0H,DTI%LDATA_Z0_O_Z0H)  
IF (.NOT. DTI%LDATA_Z0_O_Z0H) DEALLOCATE(DTI%XPAR_Z0_O_Z0H)
!
ALLOCATE(DTI%XPAR_ALBNIR_VEG  (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','ALBNIR_VEG: NIR albedo of vegetation','NAT',CFNAM_ALBNIR_VEG,   &
       CFTYP_ALBNIR_VEG,XUNIF_ALBNIR_VEG,DTI%XPAR_ALBNIR_VEG,DTI%LDATA_ALBNIR_VEG)
IF (.NOT. DTI%LDATA_ALBNIR_VEG) DEALLOCATE(DTI%XPAR_ALBNIR_VEG)
!
ALLOCATE(DTI%XPAR_ALBVIS_VEG  (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','ALBVIS_VEG: VIS albedo of vegetation','NAT',CFNAM_ALBVIS_VEG,   &
       CFTYP_ALBVIS_VEG,XUNIF_ALBVIS_VEG,DTI%XPAR_ALBVIS_VEG,DTI%LDATA_ALBVIS_VEG)  
IF (.NOT. DTI%LDATA_ALBVIS_VEG) DEALLOCATE(DTI%XPAR_ALBVIS_VEG)
!
ALLOCATE(DTI%XPAR_ALBUV_VEG   (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','ALBUV_VEG: UV albedo of vegetation','NAT',CFNAM_ALBUV_VEG,   &
       CFTYP_ALBUV_VEG,XUNIF_ALBUV_VEG,DTI%XPAR_ALBUV_VEG,DTI%LDATA_ALBUV_VEG)
IF (.NOT. DTI%LDATA_ALBUV_VEG) DEALLOCATE(DTI%XPAR_ALBUV_VEG)
!
ALLOCATE(DTI%XPAR_ALBNIR_SOIL (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','ALBNIR_SOIL: NIR albedo of SOIL','NAT',CFNAM_ALBNIR_SOIL,   &
       CFTYP_ALBNIR_SOIL,XUNIF_ALBNIR_SOIL,DTI%XPAR_ALBNIR_SOIL,DTI%LDATA_ALBNIR_SOIL)  
IF (.NOT. DTI%LDATA_ALBNIR_SOIL) DEALLOCATE(DTI%XPAR_ALBNIR_SOIL)
!
ALLOCATE(DTI%XPAR_ALBVIS_SOIL (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','ALBVIS_SOIL: VIS albedo of SOIL','NAT',CFNAM_ALBVIS_SOIL,   &
       CFTYP_ALBVIS_SOIL,XUNIF_ALBVIS_SOIL,DTI%XPAR_ALBVIS_SOIL,DTI%LDATA_ALBVIS_SOIL)  
IF (.NOT. DTI%LDATA_ALBVIS_SOIL) DEALLOCATE(DTI%XPAR_ALBVIS_SOIL)
!
ALLOCATE(DTI%XPAR_ALBUV_SOIL  (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','ALBUV_SOIL: UV albedo of SOIL','NAT',CFNAM_ALBUV_SOIL,   &
       CFTYP_ALBUV_SOIL,XUNIF_ALBUV_SOIL,DTI%XPAR_ALBUV_SOIL,DTI%LDATA_ALBUV_SOIL)  
IF (.NOT. DTI%LDATA_ALBUV_SOIL) DEALLOCATE(DTI%XPAR_ALBUV_SOIL)
!
IF (.NOT.I%LECOCLIMAP .AND. .NOT.(DTI%LDATA_RSMIN.AND.DTI%LDATA_GAMMA.AND.DTI%LDATA_WRMAX_CF.AND.DTI%LDATA_RGL &
         .AND.DTI%LDATA_CV.AND.DTI%LDATA_Z0_O_Z0H.AND.DTI%LDATA_ALBNIR_VEG.AND.DTI%LDATA_ALBVIS_VEG.AND.&
         DTI%LDATA_ALBUV_VEG.AND.DTI%LDATA_ALBNIR_SOIL.AND.DTI%LDATA_ALBVIS_SOIL.AND.DTI%LDATA_ALBUV_SOIL)) THEN
  !
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in PGD field preparation of classical fields      *'
  WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
  IF (.NOT.DTI%LDATA_RSMIN       ) WRITE(ILUOUT,*) '* for RSMIN                  *'
  IF (.NOT.DTI%LDATA_GAMMA       ) WRITE(ILUOUT,*) '* for GAMMA                  *'
  IF (.NOT.DTI%LDATA_WRMAX_CF    ) WRITE(ILUOUT,*) '* for WRMAX_CF               *'
  IF (.NOT.DTI%LDATA_RGL         ) WRITE(ILUOUT,*) '* for RGL                    *'
  IF (.NOT.DTI%LDATA_CV          ) WRITE(ILUOUT,*) '* for CV                     *'
  IF (.NOT.DTI%LDATA_Z0_O_Z0H    ) WRITE(ILUOUT,*) '* for Z0_O_Z0H               *'
  IF (.NOT.DTI%LDATA_ALBNIR_VEG  ) WRITE(ILUOUT,*) '* for ALBNIR_VEG             *'
  IF (.NOT.DTI%LDATA_ALBVIS_VEG  ) WRITE(ILUOUT,*) '* for ALBVIS_VEG             *'
  IF (.NOT.DTI%LDATA_ALBUV_VEG   ) WRITE(ILUOUT,*) '* for ALBUV_VEG              *'
  IF (.NOT.DTI%LDATA_ALBNIR_SOIL ) WRITE(ILUOUT,*) '* for ALBNIR_SOIL            *'
  IF (.NOT.DTI%LDATA_ALBVIS_SOIL ) WRITE(ILUOUT,*) '* for ALBVIS_SOIL            *'
  IF (.NOT.DTI%LDATA_ALBUV_SOIL  ) WRITE(ILUOUT,*) '* for ALBUV_SOIL             *'
  WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR CLASSICAL PARAMETERS')
  !
ENDIF
!
! ------------ Begin MEB parameters ---------------------
IF(ISIZE_LMEB_PATCH>0) THEN
  !
  ALLOCATE(DTI%XPAR_RSMINGV     (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'INV','RSMINGV: GV min stomatal res','NAT',CFNAM_RSMINGV,   &
         CFTYP_RSMINGV,XUNIF_RSMINGV,DTI%XPAR_RSMINGV,DTI%LDATA_RSMINGV)
  IF (.NOT. DTI%LDATA_RSMINGV) DEALLOCATE(DTI%XPAR_RSMINGV)
  !                                     '                            '
  ALLOCATE(DTI%XPAR_GAMMAGV     (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','GAMMAGV: GV gamma coeff','NAT',CFNAM_GAMMAGV,   &
         CFTYP_GAMMAGV,XUNIF_GAMMAGV,DTI%XPAR_GAMMAGV,DTI%LDATA_GAMMAGV)
  IF (.NOT. DTI%LDATA_GAMMAGV) DEALLOCATE(DTI%XPAR_GAMMAGV)
  !                                     '                            '
  ALLOCATE(DTI%XPAR_WRMAX_CFGV  (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','WRMAX_CFGV: cff max GV WR','NAT',CFNAM_WRMAX_CFGV,   &
         CFTYP_WRMAX_CFGV,XUNIF_WRMAX_CFGV,DTI%XPAR_WRMAX_CFGV,DTI%LDATA_WRMAX_CFGV)
  IF (.NOT. DTI%LDATA_WRMAX_CFGV) DEALLOCATE(DTI%XPAR_WRMAX_CFGV)
  !                                     '                            '
  ALLOCATE(DTI%XPAR_RGLGV       (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','RGLGV: max GV SW photosynth','NAT',CFNAM_RGLGV,   &
         CFTYP_RGLGV,XUNIF_RGLGV,DTI%XPAR_RGLGV,DTI%LDATA_RGLGV)  
  IF (.NOT. DTI%LDATA_RGLGV) DEALLOCATE(DTI%XPAR_RGLGV)
  !
ENDIF
! ------------ End MEB parameters ---------------------
!
!--------------------------------------AGS parameters----------------------------
!
IF (I%CPHOTO/='NON' .OR. (.NOT.DTI%LDATA_Z0.AND.(DTI%LDATA_LAI.OR.DTI%LDATA_VEGTYPE)) .OR. ISIZE_LMEB_PATCH>0) THEN
  !
  ALLOCATE(DTI%XPAR_H_TREE      (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','H_TREE: height of trees','NAT',CFNAM_H_TREE,   &
         CFTYP_H_TREE,XUNIF_H_TREE,DTI%XPAR_H_TREE,DTI%LDATA_H_TREE)  
  IF (.NOT. DTI%LDATA_VEGTYPE .AND. .NOT. DTI%LDATA_H_TREE) DEALLOCATE(DTI%XPAR_H_TREE)
  !
ENDIF

IF (I%CPHOTO/='NON' .OR. ISIZE_LMEB_PATCH>0) THEN
  ALLOCATE(DTI%XPAR_BSLAI       (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','BSLAI: biomass over LAI','NAT',CFNAM_BSLAI,   &
       CFTYP_BSLAI,XUNIF_BSLAI,DTI%XPAR_BSLAI,DTI%LDATA_BSLAI)  
  IF (.NOT. DTI%LDATA_BSLAI) DEALLOCATE(DTI%XPAR_BSLAI)
ENDIF
!
IF (.NOT.I%LECOCLIMAP .AND. ISIZE_LMEB_PATCH>0 .AND. .NOT.(DTI%LDATA_H_TREE      &
         .AND. DTI%LDATA_GNDLITTER .AND. DTI%LDATA_Z0LITTER  &
         .AND. DTI%LDATA_BSLAI))THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in PGD field preparation of MEB fields            *'
  WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
  IF (.NOT.DTI%LDATA_GNDLITTER   ) WRITE(ILUOUT,*) '* for GNDLITTER              *'
  IF (.NOT.DTI%LDATA_Z0LITTER    ) WRITE(ILUOUT,*) '* for Z0LITTER               *'
  IF (.NOT.DTI%LDATA_H_TREE      ) WRITE(ILUOUT,*) '* for H_TREE                 *'
  IF (.NOT.DTI%LDATA_BSLAI       ) WRITE(ILUOUT,*) '* for BSLAI                  *'
  WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR MEB PARAMETERS')
  !
ENDIF
!
IF (I%CPHOTO/='NON') THEN
  !
  ALLOCATE(DTI%XPAR_RE25        (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','RE25: ecosystem respiration','NAT',CFNAM_RE25,   &
       CFTYP_RE25,XUNIF_RE25,DTI%XPAR_RE25,DTI%LDATA_RE25)  
  IF (.NOT. DTI%LDATA_RE25) DEALLOCATE(DTI%XPAR_RE25)  
  !
  ALLOCATE(DTI%XPAR_LAIMIN      (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','LAIMIN: minimum LAI','NAT',CFNAM_LAIMIN,   &
         CFTYP_LAIMIN,XUNIF_LAIMIN,DTI%XPAR_LAIMIN,DTI%LDATA_LAIMIN)  
  IF (.NOT. DTI%LDATA_LAIMIN) DEALLOCATE(DTI%XPAR_LAIMIN)          
  !
  ALLOCATE(DTI%XPAR_SEFOLD      (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','SEFOLD: e-folding time for senescence','NAT',CFNAM_SEFOLD,   &
         CFTYP_SEFOLD,XUNIF_SEFOLD,DTI%XPAR_SEFOLD,DTI%LDATA_SEFOLD)  
  IF (.NOT. DTI%LDATA_SEFOLD) DEALLOCATE(DTI%XPAR_SEFOLD)
  !  
  ALLOCATE(DTI%XPAR_GMES        (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','GMES: mesophyl conductance','NAT',CFNAM_GMES,   &
         CFTYP_GMES,XUNIF_GMES,DTI%XPAR_GMES,DTI%LDATA_GMES)
  IF (.NOT. DTI%LDATA_GMES) DEALLOCATE(DTI%XPAR_GMES)
  !
  ALLOCATE(DTI%XPAR_GC          (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','GC: cuticular conductance','NAT',CFNAM_GC,   &
       CFTYP_GC,XUNIF_GC,DTI%XPAR_GC,DTI%LDATA_GC)  
  IF (.NOT. DTI%LDATA_GC) DEALLOCATE(DTI%XPAR_GC)
  !
  IF (.NOT.I%LECOCLIMAP .AND. .NOT.(DTI%LDATA_H_TREE.AND.DTI%LDATA_RE25.AND.DTI%LDATA_LAIMIN.AND.&
           DTI%LDATA_BSLAI.AND.DTI%LDATA_SEFOLD.AND.DTI%LDATA_GMES.AND.DTI%LDATA_GC)) THEN
    !
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in PGD field preparation of AGS fields            *'
    WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
    IF (.NOT.DTI%LDATA_H_TREE ) WRITE(ILUOUT,*) '* for H_TREE                      *'
    IF (.NOT.DTI%LDATA_RE25   ) WRITE(ILUOUT,*) '* for RE25                        *'    
    IF (.NOT.DTI%LDATA_LAIMIN ) WRITE(ILUOUT,*) '* for LAIMIN                      *'    
    IF (.NOT.DTI%LDATA_BSLAI  ) WRITE(ILUOUT,*) '* for BSLAI                       *'
    IF (.NOT.DTI%LDATA_SEFOLD ) WRITE(ILUOUT,*) '* for SEFOLD                      *'
    IF (.NOT.DTI%LDATA_GMES   ) WRITE(ILUOUT,*) '* for GMES                        *'
    IF (.NOT.DTI%LDATA_GC     ) WRITE(ILUOUT,*) '* for GC                          *'
    WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR AGS PARAMETERS')
    !
  ENDIF
  !  
  !--------------------------------------AGS Stress parameters----------------------------
  !  
  IF (I%CPHOTO/='AGS' .AND. I%CPHOTO/='LAI') THEN
    !
    ALLOCATE(DTI%XPAR_F2I         (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','F2I: critical normalized soil water content (stress)','NAT',CFNAM_F2I,   &
         CFTYP_F2I,XUNIF_F2I,DTI%XPAR_F2I,DTI%LDATA_F2I)
    IF (.NOT. DTI%LDATA_F2I) DEALLOCATE(DTI%XPAR_F2I)
    !
    ALLOCATE(DTI%XPAR_DMAX        (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','DMAX: maximum air saturation deficit','NAT',CFNAM_DMAX,   &
         CFTYP_DMAX,XUNIF_DMAX,DTI%XPAR_DMAX,DTI%LDATA_DMAX)  
    IF (.NOT. DTI%LDATA_DMAX) DEALLOCATE(DTI%XPAR_DMAX)
    !
    ALLOCATE(DTI%LPAR_STRESS      (IG%NDIM,NVEGTYPE))
    DO JVEGTYPE=1,NVEGTYPE
      GPAR_STRESS = LUNIF_STRESS(JVEGTYPE)
      IF (XSTRESS(JVEGTYPE)<1.) GPAR_STRESS = .FALSE.
      IF (XSTRESS(JVEGTYPE)==1. .AND. .NOT.GPAR_STRESS) DTI%LDATA_STRESS=.TRUE.
      DTI%LPAR_STRESS(:,JVEGTYPE) = GPAR_STRESS
    ENDDO
    IF (.NOT. DTI%LDATA_STRESS) DEALLOCATE(DTI%LPAR_STRESS)
    !
    IF (.NOT.I%LECOCLIMAP .AND. .NOT.(DTI%LDATA_F2I.AND.DTI%LDATA_DMAX)) THEN
      !
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '***********************************************************'
      WRITE(ILUOUT,*) '* Error in PGD field preparation of AGS Stress fields     *'
      WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
      IF (.NOT.DTI%LDATA_F2I  ) WRITE(ILUOUT,*) '* for F2I                           *'
      IF (.NOT.DTI%LDATA_DMAX ) WRITE(ILUOUT,*) '* for DMAX                          *'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
      WRITE(ILUOUT,*) '***********************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR AGS STRESS PARAMETERS')
      !
    ENDIF
    !    
    !--------------------------------------AGS Nitrogen parameters----------------------------
    !  
    IF (I%CPHOTO=='NIT' .OR. I%CPHOTO=='NCB') THEN
      !
      ALLOCATE(DTI%XPAR_CE_NITRO    (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','CE_NITRO: leaf area ratio sensitivity to nitrogen ccion','NAT',&
                CFNAM_CE_NITRO, CFTYP_CE_NITRO,XUNIF_CE_NITRO,DTI%XPAR_CE_NITRO,DTI%LDATA_CE_NITRO)  
      IF (.NOT. DTI%LDATA_CE_NITRO) DEALLOCATE(DTI%XPAR_CE_NITRO)
      !
      ALLOCATE(DTI%XPAR_CF_NITRO    (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','CF_NITRO: lethal minimum value of leaf area ratio','NAT',&
                CFNAM_CF_NITRO,CFTYP_CF_NITRO,XUNIF_CF_NITRO,DTI%XPAR_CF_NITRO,DTI%LDATA_CF_NITRO)
      IF (.NOT. DTI%LDATA_CF_NITRO) DEALLOCATE(DTI%XPAR_CF_NITRO)
      !
      ALLOCATE(DTI%XPAR_CNA_NITRO   (IG%NDIM,NVEGTYPE))
 CALL INI_VAR_FROM_DATA(DTCO, DGU, UG, U, USS, DTI, &
                        HPROGRAM,'ARI','CNA_NITRO: nitrogen ccion of active biomass','NAT',&
                CFNAM_CNA_NITRO,CFTYP_CNA_NITRO,XUNIF_CNA_NITRO,DTI%XPAR_CNA_NITRO,DTI%LDATA_CNA_NITRO)
      IF (.NOT. DTI%LDATA_CNA_NITRO) DEALLOCATE(DTI%XPAR_CNA_NITRO)
      !
      IF (.NOT.I%LECOCLIMAP .AND. .NOT.(DTI%LDATA_CE_NITRO.AND.DTI%LDATA_CF_NITRO.AND.DTI%LDATA_CNA_NITRO)) THEN
        !
        WRITE(ILUOUT,*) ' '
        WRITE(ILUOUT,*) '***********************************************************'
        WRITE(ILUOUT,*) '* Error in PGD field preparation of AGS Nitrogen fields   *'
        WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
        IF (.NOT.DTI%LDATA_CE_NITRO  ) WRITE(ILUOUT,*) '* for CE_NITRO                 *'
        IF (.NOT.DTI%LDATA_CF_NITRO  ) WRITE(ILUOUT,*) '* for CF_NITRO                 *'
        IF (.NOT.DTI%LDATA_CNA_NITRO ) WRITE(ILUOUT,*) '* for CNA_NITRO                *'
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
DTI%LDATA_IRRIG=.FALSE.
DTI%LDATA_WATSUP=.FALSE.
!
!----------------------------------------------------------------------------------------
!
IF (I%LECOCLIMAP .AND. DTI%LDATA_VEGTYPE) THEN
  !
  ALLOCATE(DTI%XPAR_IRRIG       (IG%NDIM,NTIME,NVEGTYPE))
  ALLOCATE(DTI%XPAR_WATSUP      (IG%NDIM,NTIME,NVEGTYPE)) 
  ALLOCATE(DTI%XPAR_ROOT_DEPTHGV(IG%NDIM,NVEGTYPE))
  !  
  CALL EXTRAPOL_FIELDS(DTCO, DTI, IG, I, UG, U, &
                       HPROGRAM,ILUOUT)
  !
  IF (.NOT. DTI%LDATA_LAI) DEALLOCATE(DTI%XPAR_LAI)
  IF (.NOT. DTI%LDATA_H_TREE .AND. I%CPHOTO/='NON') DEALLOCATE(DTI%XPAR_H_TREE)
  IF (.NOT. DTI%LDATA_DG) DEALLOCATE(DTI%XPAR_DG)
  IF (.NOT. DTI%LDATA_ROOT_DEPTH) DEALLOCATE(DTI%XPAR_ROOT_DEPTH)
  IF (.NOT. DTI%LDATA_ROOT_DEPTHGV) DEALLOCATE(DTI%XPAR_ROOT_DEPTHGV)
  IF (.NOT. DTI%LDATA_GROUND_DEPTH) DEALLOCATE(DTI%XPAR_GROUND_DEPTH)
  !
ENDIF
!
!----------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_ISBA_PAR',1,ZHOOK_HANDLE)
!
END SUBROUTINE PGD_ISBA_PAR
