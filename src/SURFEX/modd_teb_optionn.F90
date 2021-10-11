!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_TEB_OPTION_n
!     ################
!
!!****  *MODD_TEB_n - declaration of surface parameters for urban surface
!!
!!    PURPOSE
!!    -------
!     Declaration of surface parameters
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2004
!!      A. Lemonsu      07/2012         Key for urban hydrology
!!      V. Masson       06/2013         splits module
!!      E.Redon/A.Lemonsu 12/2015         Key for urban trees
!!      M. Goret          04/2017         add NTIME_CHANGE (for change in legal time)  
!!      M. Goret          05/2017         move traffic cycle from modd_tebn 
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_CSTS, ONLY : NB_MONTH, NB_DAY, NB_HOUR
USE MODD_TYPE_DATE_SURF
!
USE MODD_SURF_PAR,       ONLY : NUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE


TYPE TEB_OPTIONS_t
! TEB scheme option
!
  LOGICAL                        :: LCANOPY      ! T: SBL scheme within the canopy
                                                 ! F: no atmospheric layers below forcing level
  LOGICAL                        :: LATM_CANOPY  ! T: Atmospheric model's prognostic variables replace the canopy variables
  LOGICAL                        :: LGARDEN      ! T: Urban green areas (call ISBA from TEB)
                                                 ! F: No urban green areas
  CHARACTER(LEN=6)               :: CROAD_GRID   ! TEB option for vertical discretization of road soil column
                                                 ! 'LOW   ' : low resolution discretization (operational configuration)
                                                 ! 'MEDIUM' : mdeium resolution discretization (operational configuration)
                                                 ! 'HIGH  ' : high resolution discretization (operational configuration)
  CHARACTER(LEN=4)               :: CROAD_DIR    ! TEB option for road directions
                                                 ! 'UNIF' : no specific direction
                                                 ! 'ORIE' : many road ORIEntations
                                                 ! ( one per TEB patch)
  CHARACTER(LEN=4)               :: CWALL_OPT    ! TEB option for walls
                                                 ! 'UNIF' : uniform walls
                                                 ! 'TWO ' : two separated walls
  CHARACTER(LEN=3)               :: CBLD_ATYPE   ! Type of averaging for walls
                                                 ! 'ARI'  : Characteristics are
                                                 !          linearly averaged
                                                 ! 'MAJ ' : Majoritary building in
                                                 !          grid mesh is chosen
  CHARACTER(LEN=6)               :: CZ0H         ! TEB option for z0h roof & road
                                                 ! 'MASC95' : Mascart et al 1995
                                                 ! 'BRUT82' : Brustaert     1982
                                                 ! 'KAND07' : Kanda         2007
  CHARACTER(LEN=4)               :: CZ0EFF_GD    ! TEB option for effective roughness length for low urban vegetation
                                                 ! 'LR21' : Lemonsu, Redon et al 2021
                                                 ! 'NONE' : only vegetation roughness length is used, not taking into account the environment
  CHARACTER(LEN=5)               :: CCH_BEM      ! BEM option for roof/wall outside convective coefficient
                                                 ! 'DOE-2' : DOE-2 model from EnergyPlus Engineering reference, p65
  CHARACTER(LEN=4)               :: CURB_LM      ! Option to compute urban mixing length
  CHARACTER(LEN=3)               :: CBEM         ! TEB option for the building energy model
                                                 ! 'DEF':  DEFault version force-restore model from Masson et al. 2002
                                                 ! 'BEM':  Building Energy Model Bueno et al. 2011

  CHARACTER(LEN=4)               :: CURBTREE     ! TEB option for the high vegetation : street trees or green walls
                                                 ! 'NONE':  Default version without explicit high vegetation
                                                 ! 'TREE':  Street trees (middle of the street)
                                                 ! 'GRWL':  Green walls (trees near walls) 
  LOGICAL                        :: LGREENROOF   ! T: green roofs (call ISBA from TEB)
  LOGICAL                        :: LURBHYDRO    ! T: urban subsoil and hydrology processes
  LOGICAL                        :: LSOLAR_PANEL ! T: solar panels on roofs
  LOGICAL                        :: LSPARTACUS   ! T: Use of SPARTACUS-Surface radiation scheme
  LOGICAL                        :: LEXPLW       ! T: Calculation of longwave exchanges explicitly
  LOGICAL                        :: LCHECK_TEB   ! T: Energy budget verification for TEB
  REAL                           :: XEPS_BDGT_GLOB  ! Difference allowed in energy budget for TEB for global processes
  REAL                           :: XEPS_BDGT_FAC !  Difference allowed in energy budget for TEB for facade processes
! 
! type of initialization of vegetation: from cover types (ecoclimap) or parameters prescribed
!
  LOGICAL                        :: LECOCLIMAP   ! T: parameters computed from ecoclimap
!                                                ! F: they are read in the file
!
! General surface: 
!
  REAL, POINTER, DIMENSION(:)   :: XZS           ! orography                        (m)
  REAL, POINTER, DIMENSION(:,:) :: XCOVER        ! fraction of each ecosystem       (-)
  LOGICAL, POINTER, DIMENSION(:):: LCOVER        ! GCOVER(i)=T --> ith cover field is not 0.
  INTEGER                       :: NTEB_PATCH    ! number of TEB patches
  REAL, POINTER, DIMENSION(:,:) :: XTEB_PATCH    ! fraction of each TEB patch
!
! Number of layers
!
  INTEGER                       :: NROOF_LAYER   ! number of layers in roofs
  INTEGER                       :: NWALL_LAYER   ! number of layers in walls
  INTEGER                       :: NTIME_CHANGE  ! Number of changes in legal time
!
! Grid in the soil
!
  INTEGER                       :: NTEB_SOIL     ! total number of layers in roads (structural road + soil)
  INTEGER                       :: NTEB_ROAD     ! number of layers in structural roads only

  INTEGER, POINTER, DIMENSION(:):: NCOAT_ROAD    ! number of layers describing the structural road coating only
  REAL,    POINTER, DIMENSION(:):: XTEB_SOILGRID ! vertical grid for roads, below buildings, and gardens (if 'DIF' ISBA option)
!
! Date:
!
  TYPE (DATE_TIME)              :: TTIME         ! current date and time
!
! Time-step:
!
  REAL                          :: XTSTEP        ! time step for TEB
!
  REAL                          :: XOUT_TSTEP    ! TEB output writing time step
!
! Traffic cycle
!
  REAL, DIMENSION(NB_MONTH) :: XTRAF_MONTHLY ! monthly cycle of traffic
  REAL, DIMENSION(NB_DAY)   :: XTRAF_DAILY   ! daily cycle of traffic from  monday to sunday
  REAL, DIMENSION(NB_HOUR)  :: XTRAF_HOURLY  ! hourly cycle of traffic from 00h to 23h
!
! Inhabitants density cycle
!
  REAL, DIMENSION(NB_MONTH) :: XPOP_MONTHLY ! monthly cycle of inhabitants density 
  REAL, DIMENSION(NB_DAY)   :: XPOP_DAILY   ! daily cycle of inhabitants density  from  monday to sunday
  REAL, DIMENSION(NB_HOUR)  :: XPOP_HOURLY  ! hourly cycle of inhabitants density from 00h to 23h
!
END TYPE TEB_OPTIONS_t
!
CONTAINS
!----------------------------------------------------------------------------
SUBROUTINE TEB_OPTIONS_INIT(YTEB_OPTIONS)
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: YTEB_OPTIONS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_N:TEB_OPTIONS_INIT",0,ZHOOK_HANDLE)
!
NULLIFY(YTEB_OPTIONS%XZS)
NULLIFY(YTEB_OPTIONS%XCOVER)
NULLIFY(YTEB_OPTIONS%LCOVER)
NULLIFY(YTEB_OPTIONS%XTEB_PATCH)
NULLIFY(YTEB_OPTIONS%XTEB_SOILGRID)
NULLIFY(YTEB_OPTIONS%NCOAT_ROAD)
YTEB_OPTIONS%LCANOPY=.FALSE.
YTEB_OPTIONS%LATM_CANOPY=.FALSE.  
YTEB_OPTIONS%LGARDEN=.FALSE.
YTEB_OPTIONS%CROAD_GRID=' '
YTEB_OPTIONS%CROAD_DIR=' '
YTEB_OPTIONS%CWALL_OPT=' '
YTEB_OPTIONS%CBLD_ATYPE=' '
YTEB_OPTIONS%CZ0H=' '
YTEB_OPTIONS%CZ0EFF_GD=' '
YTEB_OPTIONS%CCH_BEM=' '
YTEB_OPTIONS%CURB_LM='    '
YTEB_OPTIONS%CBEM=' '
YTEB_OPTIONS%CURBTREE=' '
YTEB_OPTIONS%LGREENROOF=.FALSE.
YTEB_OPTIONS%LURBHYDRO=.FALSE.
YTEB_OPTIONS%LSOLAR_PANEL=.FALSE.
YTEB_OPTIONS%LSPARTACUS=.FALSE.
YTEB_OPTIONS%LEXPLW=.FALSE.
YTEB_OPTIONS%LCHECK_TEB=.TRUE.
YTEB_OPTIONS%XEPS_BDGT_GLOB=1.E-3
YTEB_OPTIONS%XEPS_BDGT_FAC=1.E-6
YTEB_OPTIONS%LECOCLIMAP=.FALSE.
YTEB_OPTIONS%NTEB_PATCH=0
YTEB_OPTIONS%NTEB_SOIL=0
YTEB_OPTIONS%NTEB_ROAD=0
YTEB_OPTIONS%NROOF_LAYER=0
YTEB_OPTIONS%NWALL_LAYER=0
YTEB_OPTIONS%NTIME_CHANGE=0
YTEB_OPTIONS%XTSTEP=0.
YTEB_OPTIONS%XOUT_TSTEP=0.
YTEB_OPTIONS%XTRAF_MONTHLY  = (/1,1,1,1,1,1,1,1,1,1,1,1/)
YTEB_OPTIONS%XTRAF_DAILY    = (/1.05,1.07,1.08,1.09,1.15,0.86,0.7/)
YTEB_OPTIONS%XTRAF_HOURLY   = (/(12./33),(12./33),(12./33),(12./33),(12./33),(12./33), &  
                                (12./33),(48./33),(48./33),(48./33),(48./33),(48./33), &
                                (48./33),(48./33),(48./33),(48./33),(48./33),(48./33), &
                                (48./33),(48./33),(48./33),(12./33),(12./33),(12./33)/)
YTEB_OPTIONS%XPOP_MONTHLY = (/1,1,1,1,1,1,1,1,1,1,1,1/)
YTEB_OPTIONS%XPOP_DAILY   = (/1,1,1,1,1,1,1/)
YTEB_OPTIONS%XPOP_HOURLY  = (/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/)
!
IF (LHOOK) CALL DR_HOOK("MODD_TEB_N:TEB_OPTIONS_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_OPTIONS_INIT
!----------------------------------------------------------------------------
END MODULE MODD_TEB_OPTION_n
