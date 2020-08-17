!MNH_LIC Copyright 1995-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##################
      MODULE MODD_BUDGET
!     ##################
!
!!****  *MODD_BUDGET* - declaration of budget variables
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the budget
!     variables
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_BUDGET)
!!          
!!    AUTHOR
!!    ------
!!	P. Hereil   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        23/02/95 
!!      J.-P. Lafore    10/02/98    adding of rhodj declaration for budget  
!!      V. Ducrocq      4/06/99     //
!!      J.-P. Pinty     25/09/00    additional budget terms for C2R2 scheme
!!      D. Gazen        22/01/01    add NCHEMSV
!!      V. Masson       06/11/02    new flags for budget calls and time counters
!!      V. Masson       27/11/02    add 2way nesting effect
!!      P. Jabouille    07/07/04    add budget terms for microphysics
!!      C. Barthe       19/11/09    add budget terms for electricity          
!!      C.Lac           04/2016  negative contribution to the budget split between advection, turbulence and microphysics for KHKO/C2R2
!!      C. Barthe            /16    add budget terms for LIMA
!!      C. LAc          10/2016 add droplets deposition
!!      S. Riette       11/2016  New budgets for ICE3/ICE4
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 19/07/2019: parameters to identify budget number
!  P. Wautelet 15/11/2019: remove unused CBURECORD variable
!  P. Wautelet 17/01/2020: add new budget data types
!  P. Wautelet 27/01/2020: use the tfield_metadata_base abstract datatype
!  P. Wautelet 28/01/2020: add missing budgets for viscosity
!  P. Wautelet 28/01/2020: add trhodj in tbudgetdata datatype
!  B. Vie      03/02/2020: LIMA negativity checks after turbulence, advection and microphysics budgets
!  P. Wautelet 09/03/2020: add tburhodj variable
!  P .Wautelet 09/03/2020: add missing budgets for electricity
!  P. Wautelet 17/04/2020: set default values for budgets switch values
!  P. Wautelet 23/04/2020: add nid in tbudgetdata datatype
!  P. Wautelet 30/06/2020: add NNETURSV, NNEADVSV and NNECONSV variables
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------

use modd_field,      only: tfield_metadata_base
use modd_parameters, only: NBUNAMELGTMAX, NCOMMENTLGTMAX

implicit none

public

integer, parameter :: NBUDGET_RHO = 0  ! Reference number for budget of RhoJ
integer, parameter :: NBUDGET_U   = 1  ! Reference number for budget of RhoJu  and/or LES budgets with u
integer, parameter :: NBUDGET_V   = 2  ! Reference number for budget of RhoJv  and/or LES budgets with u
integer, parameter :: NBUDGET_W   = 3  ! Reference number for budget of RhoJw  and/or LES budgets with u
integer, parameter :: NBUDGET_TH  = 4  ! Reference number for budget of RhoJTh and/or LES budgets with th
integer, parameter :: NBUDGET_TKE = 5  ! Reference number for budget of RhoJTke and/or LES budgets with Tke
integer, parameter :: NBUDGET_RV  = 6  ! Reference number for budget of RhoJrv and/or LES budgets with rv
integer, parameter :: NBUDGET_RC  = 7  ! Reference number for budget of RhoJrc and/or LES budgets with rc
integer, parameter :: NBUDGET_RR  = 8  ! Reference number for budget of RhoJrr and/or LES budgets with rr
integer, parameter :: NBUDGET_RI  = 9  ! Reference number for budget of RhoJri and/or LES budgets with ri
integer, parameter :: NBUDGET_RS  = 10 ! Reference number for budget of RhoJrs and/or LES budgets with rs
integer, parameter :: NBUDGET_RG  = 11 ! Reference number for budget of RhoJrg and/or LES budgets with rg
integer, parameter :: NBUDGET_RH  = 12 ! Reference number for budget of RhoJrh and/or LES budgets with rh
integer, parameter :: NBUDGET_SV1 = 13 ! Reference number for 1st budget of RhoJsv and/or LES budgets with sv

integer :: nbudgets ! Number of budget categories


type tbudgetdata
  character(len=NBUNAMELGTMAX)  :: cname    = ''
  character(len=NCOMMENTLGTMAX) :: ccomment = ''
  integer :: nid         = -1 !Identifier number (based on parameters NBUDGET_*)
  integer :: ngroups     = 0 !Number of groups of source terms to store
  integer :: nsources    = 0 !Number of source terms
  integer :: nsourcesmax = 0 !Maximum number of source terms
  integer :: ntmpstoresource = 0 !Reference of the source term using the xtmpstore array
  logical :: lenabled = .false. ! True if corresponding budget flag is set to true
  real, dimension(:,:,:), allocatable :: xtmpstore ! Array to store temporary data
                                                   !  (to allow to store the difference between 2 places)
  type(tbusourcedata), dimension(:), allocatable :: tsources ! Full list of source terms (used or not)
  type(tbugroupdata),  dimension(:), allocatable :: tgroups  ! Full list of groups of source terms (to be written)
  type(tburhodata),    pointer                   :: trhodj => null() ! Budget array for rhodj
end type tbudgetdata


type, extends( tfield_metadata_base ) :: tbusourcedata
  integer :: ngroup = 0 ! Number of the source term group in which storing the source term
                        !  (0: no store, 1: individual store, >1: number of the group)
  logical :: lenabled   = .false.
  logical :: ldonotinit = .false. ! if true, does not need a call to Budget_store_init
                                  ! It may be true only if the source term is in a group not containing other sources
  logical :: loverwrite = .false. ! if true, source term values will overwrite the previuos ones
                                  ! It may be true only if the source term is in a group not containing other sources
end type tbusourcedata

type, extends( tfield_metadata_base ) :: tbugroupdata
  integer :: nsources = 0 ! Number of source terms composing this group
  integer, dimension(:),     allocatable :: nsourcelist ! List of the source terms composing this group
  real,    dimension(:,:,:), allocatable :: xdata ! Array to store the budget data
end type tbugroupdata

type, extends( tfield_metadata_base ) :: tburhodata
  real, dimension(:,:,:), allocatable :: xdata ! Array to store the budget data
end type tburhodata

type(tbudgetdata), dimension(:), allocatable, save :: tbudgets
type(tburhodata),                pointer,     save :: tburhodj => null() ! Budget array for rhodj used inside some tbudgets


!                       General variables
LOGICAL, SAVE :: LBU_ENABLE
!
CHARACTER (LEN=4), SAVE :: CBUTYPE         ! type of desired budget 'CART'
                                           ! (cartesian box) or 'MASK' (budget
                                           ! zone defined by a mask) or 'NONE'
                                           ! (no budget)
INTEGER, SAVE :: NBUMOD                    ! model in which budget is 
                                           ! calculated
!
LOGICAL, SAVE :: LBU_BEG                   ! switch for budget beginning
!
REAL, SAVE    :: XBULEN                    ! length in seconds of the budget 
                                           ! temporal average
!
INTEGER, SAVE :: NBUSTEP                   ! number of model timesteps required 
                                           ! for the budget time average
REAL, SAVE    :: XBUWRI                       ! period in seconds of
                                           ! budget writing on FM-files
INTEGER, SAVE :: NBUWRNB                   ! number of budget periods when storage
                                           ! arrays are written on FM-files
INTEGER, SAVE :: NBUTSHIFT                 ! temporal shift for budgets writing
!
INTEGER, SAVE :: NBUKL, NBUKH              ! lowest and highest K indice values 
                                           ! of the budget box 
LOGICAL, SAVE :: LBU_KCP                   ! switch for compression in K
                                           ! direction
!
!                Variables used by the cartesian box case ('CART') only
!
INTEGER, SAVE :: NBUIL, NBUIH              ! lowest and highest I indice values 
                                           ! of the cartesian box 
INTEGER, SAVE :: NBUJL, NBUJH              ! lowest and highest J indice values 
                                           ! of the cartesian box 
LOGICAL, SAVE :: LBU_ICP                   ! switch for compression in I
                                           ! direction
LOGICAL, SAVE :: LBU_JCP                   ! switch for comppression in J
                                           ! direction
!
!                Variables used by the  mask case ('MASK') only
!
INTEGER, SAVE :: NBUMASK                   ! number of MASK zones for which 
                                           ! budgets are performed 
LOGICAL, SAVE, DIMENSION(:,:,:),         & ! define the zone where the MASK 
           ALLOCATABLE :: LBU_MASK         ! is True 
!                                          
REAL, SAVE, DIMENSION(:,:,:,:),          & ! surface for each mask at each   
           ALLOCATABLE :: XBUSURF          ! budget step   
!             
INTEGER, SAVE :: NBUTIME                   ! number of budget time periods
!
!                       Variables for budget storage 
!
!                       General variables
INTEGER, SAVE :: NBUSIL, NBUSIH      ! lowest and highest I indices of the intersection
                                     ! of the cartesian box with the sub-domain
INTEGER, SAVE :: NBUSJL, NBUSJH      ! lowest and highest J indices of the intersection
                                     ! of the global cartesian box
INTEGER, SAVE :: NBUIMAX_ll                ! second dimension of the budget
INTEGER, SAVE :: NBUJMAX_ll                ! second dimension of the budget
                                           ! array in the global domain (in CART case)
!
INTEGER, SAVE :: NBUIMAX                   ! first dimension of the budget
                                           ! tabular
INTEGER, SAVE :: NBUJMAX                   ! second dimension of the budget
                                           ! tabular
INTEGER, SAVE :: NBUKMAX                   ! dimension along K of the budget
                                           ! tabular
!
!      Allowed processes for the budget of the x scalar variables
!        (transport part only)
!
! For each budget, the switches values for budgets
! activation may be set by the user in a namelist. Their default value is 0.
! In the following declaration, the corresponding process names  are given 
! beside as comments.
!     
!      Allowed processes for the budget of RU (wind component along x)
!
! Courant namelist: NAM_BURU
!
LOGICAL, SAVE :: LBU_RU = .FALSE. ! True when the budget of RU is performed
!                         
INTEGER, SAVE :: NASSEU  = 0 ! time filter
INTEGER, SAVE :: NNESTU  = 0 ! Efffect of 2way nesting on U
INTEGER, SAVE :: NADVU   = 0 ! advection
INTEGER, SAVE :: NFRCU   = 0 ! forcing
INTEGER, SAVE :: NNUDU   = 0 ! nudging
INTEGER, SAVE :: NCURVU  = 0 ! curvature
INTEGER, SAVE :: NCORU   = 0 ! Coriolis terms
INTEGER, SAVE :: NDIFU   = 0 ! numerical diffusion
INTEGER, SAVE :: NRELU   = 0 ! relaxation
INTEGER, SAVE :: NHTURBU = 0 ! horizontal TURBulence
INTEGER, SAVE :: NVTURBU = 0 ! vertical turbulence
INTEGER, SAVE :: NDRAGU  = 0 ! vegetation drag
INTEGER, SAVE :: NMAFLU  = 0 ! mass flux
INTEGER, SAVE :: NPRESU  = 0 ! pressure term
INTEGER, SAVE :: NVISCU  = 0 ! viscosity
!
!      Allowed processes for the budget of RV (wind component along y)
!                                                  
! Courant namelist: NAM_BURV
!
LOGICAL, SAVE :: LBU_RV = .FALSE. ! True when the budget of RV is performed
!
INTEGER, SAVE :: NASSEV  = 0 ! time filter
INTEGER, SAVE :: NNESTV  = 0 ! Efffect of 2way nesting on V
INTEGER, SAVE :: NADVV   = 0 ! advection
INTEGER, SAVE :: NFRCV   = 0 ! forcing
INTEGER, SAVE :: NNUDV   = 0 ! nudging
INTEGER, SAVE :: NCURVV  = 0 ! curvature
INTEGER, SAVE :: NCORV   = 0 ! Coriolis terms
INTEGER, SAVE :: NDIFV   = 0 ! numerical diffusion
INTEGER, SAVE :: NRELV   = 0 ! relaxation
INTEGER, SAVE :: NHTURBV = 0 ! horizontal turbulence
INTEGER, SAVE :: NVTURBV = 0 ! vertical turbulence
INTEGER, SAVE :: NDRAGV  = 0 ! vegetation drag
INTEGER, SAVE :: NMAFLV  = 0 ! mass flux
INTEGER, SAVE :: NPRESV  = 0 ! pressure term
INTEGER, SAVE :: NVISCV  = 0 ! viscosity
!
!      Allowed processes for the budget of RW (wind vertical component)
!                                                  
! Courant namelist: NAM_BURW
!
LOGICAL, SAVE :: LBU_RW = .FALSE. ! True when the budget of RW is performed
!                                                  
INTEGER, SAVE :: NASSEW  = 0 ! time filter
INTEGER, SAVE :: NNESTW  = 0 ! Efffect of 2way nesting on W
INTEGER, SAVE :: NADVW   = 0 ! advection
INTEGER, SAVE :: NFRCW   = 0 ! forcing
INTEGER, SAVE :: NNUDW   = 0 ! nudging
INTEGER, SAVE :: NCURVW  = 0 ! curvature
INTEGER, SAVE :: NCORW   = 0 ! Coriolis terms
INTEGER, SAVE :: NGRAVW  = 0 ! gravity term
INTEGER, SAVE :: NDIFW   = 0 ! numerical diffusion
INTEGER, SAVE :: NRELW   = 0 ! relaxation
INTEGER, SAVE :: NHTURBW = 0 ! horizontal turbulence
INTEGER, SAVE :: NVTURBW = 0 ! vertical turbulence
INTEGER, SAVE :: NPRESW  = 0 ! pressure term
INTEGER, SAVE :: NVISCW  = 0 ! viscosity
!
!      Allowed processes for the budget of RTH (potential temperature)
!                                                  
! Courant namelist: NAM_BURTH
!
LOGICAL, SAVE :: LBU_RTH = .FALSE. ! True when the budget of RTH is performed
!
INTEGER, SAVE :: NASSETH  = 0 ! time filter
INTEGER, SAVE :: NNESTTH  = 0 ! Efffect of 2way nesting on Th
INTEGER, SAVE :: NADVTH   = 0 ! Total advection for PPM
INTEGER, SAVE :: NFRCTH   = 0 ! forcing
INTEGER, SAVE :: N2DADVTH = 0 ! 2d advecting forcing
INTEGER, SAVE :: N2DRELTH = 0 ! 2d relaxation forcing
INTEGER, SAVE :: NNUDTH   = 0 ! nudging
INTEGER, SAVE :: NPREFTH  = 0 ! theta source term due to the reference pressure
                              ! (Dyn. Sources) only present if KRR>0
INTEGER, SAVE :: NDIFTH   = 0  ! numerical diffusion
INTEGER, SAVE :: NRELTH   = 0  ! relaxation
INTEGER, SAVE :: NRADTH   = 0  ! RADiation
INTEGER, SAVE :: NDCONVTH = 0  ! KAFR CONVection
INTEGER, SAVE :: NMAFLTH  = 0  ! Mass flux
INTEGER, SAVE :: NHTURBTH = 0  ! horizontal turbulence
INTEGER, SAVE :: NVTURBTH = 0  ! vertical turbulence
INTEGER, SAVE :: NDISSHTH = 0  ! dissipative heating
INTEGER, SAVE :: NNEGATH  = 0  ! negative correction induced by hydrometeors
INTEGER, SAVE :: NNETURTH = 0   ! negative correction induced by hydrometeors
INTEGER, SAVE :: NNEADVTH = 0   ! negative correction induced by hydrometeors
INTEGER, SAVE :: NNECONTH = 0   ! negative correction induced by hydrometeors
INTEGER, SAVE :: NREVATH  = 0  ! rain evaporation
INTEGER, SAVE :: NCONDTH  = 0  ! evaporation/condensation
INTEGER, SAVE :: NHENUTH  = 0  ! HEterogenous NUcleation ICE3
INTEGER, SAVE :: NHONTH   = 0  ! HOmogeneous Nucleation  ICE3
INTEGER, SAVE :: NSFRTH   = 0  ! Spontaneous FReezing    ICE3
INTEGER, SAVE :: NDEPSTH  = 0  ! DEPosition on Snow      ICE3
INTEGER, SAVE :: NDEPGTH  = 0  ! DEPosition on Graupel   ICE3
INTEGER, SAVE :: NRIMTH   = 0  ! RIMing of cloudwater    ICE3
INTEGER, SAVE :: NACCTH   = 0  ! ACCretion of rainwater  ICE3
INTEGER, SAVE :: NCFRZTH  = 0  ! Conversion FReeZing     ICE3
INTEGER, SAVE :: NWETGTH  = 0  ! WET Growth of graupel   ICE3
INTEGER, SAVE :: NDRYGTH  = 0  ! DRY Growth of graupel   ICE3
INTEGER, SAVE :: NGMLTTH  = 0  ! Graupel MeLTing         ICE3
INTEGER, SAVE :: NIMLTTH  = 0  ! Ice MeLTing             ICE3
INTEGER, SAVE :: NBERFITH = 0  ! BERgeron-FIndeisen gth. ICE3
INTEGER, SAVE :: NCDEPITH = 0  ! Cond./DEPosition on ice ICE3
INTEGER, SAVE :: NWETHTH  = 0  ! wet growth of hail      ICE4
INTEGER, SAVE :: NDRYHTH  = 0  ! dry growth of hail      ICE4
INTEGER, SAVE :: NHMLTTH  = 0  ! melting of hail         ICE4
INTEGER, SAVE :: NADJUTH  = 0  ! adjustement before rain_ice ICE3
INTEGER, SAVE :: NCORRTH  = 0  ! tendencies correction after ICE3
INTEGER, SAVE :: NHINDTH  = 0  ! Heterogeneous Nucleation by Deposition LIMA
INTEGER, SAVE :: NHINCTH  = 0  ! Heterogeneous Nucleation by Contact    LIMA
INTEGER, SAVE :: NHONHTH  = 0  ! Haze Homogeneous Nucleation            LIMA
INTEGER, SAVE :: NHONCTH  = 0  ! droplet homogeneous nucleation         LIMA
INTEGER, SAVE :: NHONRTH  = 0  ! drop homogeneous nucleation            LIMA
INTEGER, SAVE :: NCEDSTH  = 0  ! adjustment
INTEGER, SAVE :: NSEDITH  = 0  ! Temperature transport by hydrometeors sedimentation
INTEGER, SAVE :: NVISCTH  = 0  ! viscosity
!
!      Allowed processes for the budget of RTKE (kinetic energy)
!                                                  
! Courant namelist: NAM_BURTKE
!
LOGICAL, SAVE :: LBU_RTKE = .FALSE. ! True when the budget of RTKE is performed
!
INTEGER, SAVE :: NASSETKE = 0 ! time filter
INTEGER, SAVE :: NADVTKE  = 0 ! Total advection for PPM
INTEGER, SAVE :: NFRCTKE  = 0 ! forcing
INTEGER, SAVE :: NDIFTKE  = 0 ! numerical diffusion
INTEGER, SAVE :: NRELTKE  = 0 ! relaxation
INTEGER, SAVE :: NDPTKE   = 0 ! dynamic production of TKE
INTEGER, SAVE :: NTPTKE   = 0 ! thermal production of TKE
INTEGER, SAVE :: NDRAGTKE = 0 ! vegetation drag
INTEGER, SAVE :: NDISSTKE = 0 ! dissipation of TKE
INTEGER, SAVE :: NTRTKE   = 0 ! turbulent transport of TKE
!
!
!      Allowed processes for the budget of moist variable RRV (water vapor)
!                                                  
! Courant namelist: NAM_BURRV
!
LOGICAL, SAVE :: LBU_RRV = .FALSE. ! true when the budget of RRV is performed
!
INTEGER, SAVE :: NASSERV  = 0 ! time filter
INTEGER, SAVE :: NNESTRV  = 0 ! Effect of 2way nesting on Rv
INTEGER, SAVE :: NADVRV   = 0 ! Total advection for PPM
INTEGER, SAVE :: NFRCRV   = 0 ! forcing
INTEGER, SAVE :: N2DADVRV = 0 ! 2d advecting forcing
INTEGER, SAVE :: N2DRELRV = 0 ! 2d relaxation forcing
INTEGER, SAVE :: NNUDRV   = 0 ! nudging
INTEGER, SAVE :: NDIFRV   = 0 ! numerical diffusion
INTEGER, SAVE :: NRELRV   = 0 ! relaxation
INTEGER, SAVE :: NDCONVRV = 0 ! KAFR CONVection
INTEGER, SAVE :: NMAFLRV  = 0 ! Mass flux
INTEGER, SAVE :: NHTURBRV = 0 ! horizontal turbulence
INTEGER, SAVE :: NVTURBRV = 0 ! vertical turbulence
INTEGER, SAVE :: NNEGARV  = 0 ! negative correction
INTEGER, SAVE :: NNETURRV = 0 ! negative correction
INTEGER, SAVE :: NNECONRV = 0 ! negative correction
INTEGER, SAVE :: NNEADVRV = 0 ! negative correction
INTEGER, SAVE :: NREVARV  = 0 ! rain evaporation
INTEGER, SAVE :: NCONDRV  = 0 ! evaporation/condensation
INTEGER, SAVE :: NHENURV  = 0 ! HEterogenous NUcleation ICE3
INTEGER, SAVE :: NDEPSRV  = 0 ! DEPosition on Snow      ICE3
INTEGER, SAVE :: NDEPGRV  = 0 ! DEPosition on Graupel   ICE3
INTEGER, SAVE :: NCDEPIRV = 0 ! Cond./DEPosition on ice ICE3
INTEGER, SAVE :: NADJURV  = 0 ! adjustement before rain_ice ICE3
INTEGER, SAVE :: NCORRRV  = 0 ! tendencies correction after ICE3
INTEGER, SAVE :: NHINDRV  = 0 ! Heterogeneous Nucleation by Deposition LIMA
INTEGER, SAVE :: NHONHRV  = 0 ! Haze Homogeneous Nucleation            LIMA
INTEGER, SAVE :: NCEDSRV  = 0 ! adjustement
INTEGER, SAVE :: NVISCRV  = 0 ! viscosity
!
!      Allowed processes for the budget of moist variable RRC (cloud water)
!                                                  
! Courant namelist: NAM_BURRC
!
LOGICAL, SAVE :: LBU_RRC = .FALSE. ! True when the budget of RRC is performed
!
INTEGER, SAVE :: NASSERC   = 0 ! time filter
INTEGER, SAVE :: NNESTRC   = 0 ! Efffect of 2way nesting on Rc
INTEGER, SAVE :: NADVRC    = 0 ! Total advection for PPM
INTEGER, SAVE :: NFRCRC    = 0 ! forcing
INTEGER, SAVE :: NDIFRC    = 0 ! numerical diffusion
INTEGER, SAVE :: NRELRC    = 0 ! relaxation
INTEGER, SAVE :: NDCONVRC  = 0 ! Deep CONVection
INTEGER, SAVE :: NHTURBRC  = 0 ! horizontal turbulence
INTEGER, SAVE :: NVTURBRC  = 0 ! vertical turbulence
INTEGER, SAVE :: NNEGARC   = 0 ! negative correction
INTEGER, SAVE :: NNETURRC  = 0 ! negative correction
INTEGER, SAVE :: NNECONRC  = 0 ! negative correction
INTEGER, SAVE :: NNEADVRC  = 0 ! negative correction
INTEGER, SAVE :: NACCRRC   = 0 ! accretion
INTEGER, SAVE :: NAUTORC   = 0 ! autoconversion
INTEGER, SAVE :: NCONDRC   = 0 ! evaporation/condensation
INTEGER, SAVE :: NHONRC    = 0 ! HOmogeneous Nucleation  ICE3
INTEGER, SAVE :: NRIMRC    = 0 ! RIMing of cloudwater    ICE3
INTEGER, SAVE :: NCMELRC   = 0 ! collection by snow and conversion into rain with T>XTT ICE3
INTEGER, SAVE :: NWETGRC   = 0 ! WET Growth of graupel   ICE3
INTEGER, SAVE :: NDRYGRC   = 0 ! DRY Growth of graupel   ICE3
INTEGER, SAVE :: NIMLTRC   = 0 ! Ice MeLTing             ICE3
INTEGER, SAVE :: NBERFIRC  = 0 ! BERgeron-FIndeisen gth. ICE3
INTEGER, SAVE :: NCDEPIRC  = 0 ! Cond./DEPosition on ice ICE3
INTEGER, SAVE :: NHENURC   = 0 ! CCN Activation C2R2
INTEGER, SAVE :: NSEDIRC   = 0 ! sedimentation  C2R2
INTEGER, SAVE :: NDEPORC   = 0 ! ground deposition
INTEGER, SAVE :: NDEPOTRRC = 0 ! deposition on tree
INTEGER, SAVE :: NWETHRC   = 0 ! wet growth of hail
INTEGER, SAVE :: NDRYHRC   = 0 ! dry growth of hail      ICE4
INTEGER, SAVE :: NADJURC   = 0 ! adjustement before rain_ice ICE3
INTEGER, SAVE :: NHINCRC   = 0 ! Heterogeneous Nucleation by Contact LIMA
INTEGER, SAVE :: NHONCRC   = 0 ! droplet homogeneous nucleation      LIMA
INTEGER, SAVE :: NCEDSRC   = 0 ! adjustment                          LIMA
INTEGER, SAVE :: NREVARC   = 0 ! evaporation of rain drops
INTEGER, SAVE :: NCORRRC   = 0 ! rain <-> cloud transfer at the beginning of LIMA
INTEGER, SAVE :: NR2C1RC   = 0 ! rain -> cloud change after sedimentation in LIMA
INTEGER, SAVE :: NCVRCRC   = 0 ! rain -> cloud change after other microphysical processes in LIMA
INTEGER, SAVE :: NVISCRC   = 0 ! viscosity
!
!      Allowed processes for the budget of moist variable RRR (rain water)
!
! Courant namelist: NAM_BURRR
!
LOGICAL, SAVE :: LBU_RRR = .FALSE. ! True when the budget of RRR is performed
!
INTEGER, SAVE :: NASSERR  = 0 ! time filter
INTEGER, SAVE :: NNESTRR  = 0 ! Efffect of 2way nesting on Rr
INTEGER, SAVE :: NADVRR   = 0 ! Total advection for PPM
INTEGER, SAVE :: NFRCRR   = 0 ! forcing
INTEGER, SAVE :: NDIFRR   = 0 ! numerical diffusion
INTEGER, SAVE :: NRELRR   = 0 ! relaxation
INTEGER, SAVE :: NNEGARR  = 0 ! negative correction
INTEGER, SAVE :: NNETURRR = 0 ! negative correction
INTEGER, SAVE :: NNEADVRR = 0 ! negative correction
INTEGER, SAVE :: NNECONRR = 0 ! negative correction
INTEGER, SAVE :: NACCRRR  = 0 ! accretion
INTEGER, SAVE :: NAUTORR  = 0 ! autoconversion
INTEGER, SAVE :: NREVARR  = 0 ! rain evaporation
INTEGER, SAVE :: NSEDIRR  = 0 ! sedimentation
INTEGER, SAVE :: NSFRRR   = 0 ! Spontaneous FReezing    ICE3
INTEGER, SAVE :: NACCRR   = 0 ! ACCretion of rainwater  ICE3
INTEGER, SAVE :: NCMELRR  = 0 ! collection of droplets by snow and conversion into rain with T>XTT ICE3
INTEGER, SAVE :: NCFRZRR  = 0 ! Conversion FReeZing     ICE3
INTEGER, SAVE :: NWETGRR  = 0 ! WET Growth of graupel   ICE3
INTEGER, SAVE :: NDRYGRR  = 0 ! DRY Growth of graupel   ICE3
INTEGER, SAVE :: NGMLTRR  = 0 ! Graupel MeLTing         ICE3
INTEGER, SAVE :: NWETHRR  = 0 ! wet growth of hail      ICE4
INTEGER, SAVE :: NDRYHRR  = 0 ! dry growth of hail      ICE4
INTEGER, SAVE :: NHMLTRR  = 0 ! melting of hail         ICE4
INTEGER, SAVE :: NCORRRR  = 0 ! tendencies correction after ICE3
INTEGER, SAVE :: NHONRRR  = 0 ! drop homogeneous nucleation LIMA
INTEGER, SAVE :: NR2C1RR  = 0 ! rain -> cloud change after sedimentation in LIMA
INTEGER, SAVE :: NCVRCRR  = 0 ! rain -> cloud change after other microphysical processes in LIMA
INTEGER, SAVE :: NVISCRR  = 0 ! viscosity
!
!      Allowed processes for the budget of moist variable RRI (ice)
!
! Courant namelist: NAM_BURRI
!
LOGICAL, SAVE :: LBU_RRI = .FALSE. ! True when the budget of RRI is performed
!
INTEGER, SAVE :: NASSERI  = 0 ! time filter
INTEGER, SAVE :: NNESTRI  = 0 ! Efffect of 2way nesting on Ri
INTEGER, SAVE :: NADVRI   = 0 ! Total advection for PPM
INTEGER, SAVE :: NFRCRI   = 0 ! forcing
INTEGER, SAVE :: NDIFRI   = 0 ! numerical diffusion
INTEGER, SAVE :: NRELRI   = 0 ! relaxation
INTEGER, SAVE :: NDCONVRI = 0 ! Deep CONVection
INTEGER, SAVE :: NHTURBRI = 0 ! horizontal turbulence
INTEGER, SAVE :: NVTURBRI = 0 ! vertical turbulence
INTEGER, SAVE :: NNEGARI  = 0 ! negative correction
INTEGER, SAVE :: NNETURRI = 0 ! negative correction
INTEGER, SAVE :: NNEADVRI = 0 ! negative correction
INTEGER, SAVE :: NNECONRI = 0 ! negative correction
INTEGER, SAVE :: NSEDIRI  = 0 ! SEDImentation           ICE3
INTEGER, SAVE :: NHENURI  = 0 ! HEterogenous NUcleation ICE3
INTEGER, SAVE :: NHONRI   = 0 ! HOmogeneous Nucleation  ICE3
INTEGER, SAVE :: NAGGSRI  = 0 ! AGGregation of snow     ICE3
INTEGER, SAVE :: NAUTSRI  = 0 ! AUToconversion of ice   ICE3
INTEGER, SAVE :: NCFRZRI  = 0 ! Conversion FReeZing     ICE3
INTEGER, SAVE :: NWETGRI  = 0 ! WET Growth of graupel   ICE3
INTEGER, SAVE :: NDRYGRI  = 0 ! DRY Growth of graupel   ICE3
INTEGER, SAVE :: NIMLTRI  = 0 ! Ice MeLTing             ICE3
INTEGER, SAVE :: NBERFIRI = 0 ! BERgeron-FIndeisen gth. ICE3
INTEGER, SAVE :: NCDEPIRI = 0 ! Cond./DEPosition on ice ICE3
INTEGER, SAVE :: NWETHRI  = 0 ! wet growth of hail      ICE4
INTEGER, SAVE :: NDRYHRI  = 0 ! dry growth of hail      ICE4
INTEGER, SAVE :: NADJURI  = 0 ! adjustement before rain_ice ICE3
INTEGER, SAVE :: NHINDRI  = 0 ! heterogeneous nucleation by deposition LIMA
INTEGER, SAVE :: NHINCRI  = 0 ! heterogeneous nucleation by contact    LIMA
INTEGER, SAVE :: NHONHRI  = 0 ! haze homogeneous nucleation source     LIMA
INTEGER, SAVE :: NHONCRI  = 0 ! droplet homogeneous nucleation         LIMA
INTEGER, SAVE :: NCNVIRI  = 0 ! Conversion of snow to r_i              LIMA
INTEGER, SAVE :: NCNVSRI  = 0 ! Conversion of pristine ice to r_s      LIMA
INTEGER, SAVE :: NHMSRI   = 0 ! Hallett-Mossop ice multiplication process due to snow riming LIMA
INTEGER, SAVE :: NHMGRI   = 0 ! Hallett-Mossop ice multiplication process due to graupel riming LIMA
INTEGER, SAVE :: NCEDSRI  = 0 ! adjustement LIMA
INTEGER, SAVE :: NCORRRI  = 0 ! ice <-> snow transfer at the beginning of LIMA
INTEGER, SAVE :: NVISCRI  = 0 ! viscosity
!
!      Allowed processes for the budget of moist variable RRS (snow)
!
! Courant namelist: NAM_BURRS
!
LOGICAL, SAVE :: LBU_RRS = .FALSE. ! True when the budget of RRS is performed
!
INTEGER, SAVE :: NASSERS  = 0 ! time filter
INTEGER, SAVE :: NNESTRS  = 0 ! Efffect of 2way nesting on Rs
INTEGER, SAVE :: NADVRS   = 0 ! Total advection for PPM
INTEGER, SAVE :: NFRCRS   = 0 ! forcing
INTEGER, SAVE :: NDIFRS   = 0 ! numerical diffusion
INTEGER, SAVE :: NRELRS   = 0 ! relaxation
INTEGER, SAVE :: NNEGARS  = 0 ! negative correction
INTEGER, SAVE :: NNETURRS = 0 ! negative correction
INTEGER, SAVE :: NNEADVRS = 0 ! negative correction
INTEGER, SAVE :: NNECONRS = 0 ! negative correction
INTEGER, SAVE :: NSEDIRS  = 0 ! SEDImentation           ICE3
INTEGER, SAVE :: NDEPSRS  = 0 ! DEPosition on Snow      ICE3
INTEGER, SAVE :: NAGGSRS  = 0 ! AGGregation of snow     ICE3
INTEGER, SAVE :: NAUTSRS  = 0 ! AUToconversion of ice   ICE3
INTEGER, SAVE :: NRIMRS   = 0 ! RIMing of cloudwater    ICE3
INTEGER, SAVE :: NACCRS   = 0 ! ACCretion of rainwater  ICE3
INTEGER, SAVE :: NCMELRS  = 0 ! Conversion MeLTing      ICE3
INTEGER, SAVE :: NWETGRS  = 0 ! WET Growth of graupel   ICE3
INTEGER, SAVE :: NDRYGRS  = 0 ! DRY Growth of graupel   ICE3
INTEGER, SAVE :: NWETHRS  = 0 ! wet growth of hail      ICE4
INTEGER, SAVE :: NDRYHRS  = 0 ! dry growth of hail      ICE4
INTEGER, SAVE :: NCNVIRS  = 0 ! Conversion of snow to r_i         LIMA
INTEGER, SAVE :: NCNVSRS  = 0 ! Conversion of pristine ice to r_s LIMA
INTEGER, SAVE :: NHMSRS   = 0 ! Hallett-Mossop ice multiplication process due to snow riming LIMA
INTEGER, SAVE :: NCORRRS  = 0 ! ice <-> snow transfer at the beginning of LIMA
INTEGER, SAVE :: NVISCRS  = 0 ! viscosity
!
!      Allowed processes for the budget of moist variable RRG (graupel)
!
! Courant namelist: NAM_BURRG
!
LOGICAL, SAVE :: LBU_RRG = .FALSE. ! True when the budget of RRG is performed
!
INTEGER, SAVE :: NASSERG  = 0 ! time filter
INTEGER, SAVE :: NNESTRG  = 0 ! Efffect of 2way nesting on Rg
INTEGER, SAVE :: NADVRG   = 0 ! Total advection for PPM
INTEGER, SAVE :: NFRCRG   = 0 ! forcing
INTEGER, SAVE :: NDIFRG   = 0 ! numerical diffusion
INTEGER, SAVE :: NRELRG   = 0 ! relaxation
INTEGER, SAVE :: NNEGARG  = 0 ! negative correction
INTEGER, SAVE :: NNETURRG = 0 ! negative correction
INTEGER, SAVE :: NNEADVRG = 0 ! negative correction
INTEGER, SAVE :: NNECONRG = 0 ! negative correction
INTEGER, SAVE :: NSEDIRG  = 0 ! SEDImentation           ICE3
INTEGER, SAVE :: NSFRRG   = 0 ! Spontaneous FReezing    ICE3
INTEGER, SAVE :: NDEPGRG  = 0 ! DEPosition on Snow      ICE3
INTEGER, SAVE :: NRIMRG   = 0 ! RIMing of cloudwater    ICE3
INTEGER, SAVE :: NACCRG   = 0 ! ACCretion of rainwater  ICE3
INTEGER, SAVE :: NCMELRG  = 0 ! Conversion MeLTing      ICE3
INTEGER, SAVE :: NCFRZRG  = 0 ! Conversion FReeZing     ICE3
INTEGER, SAVE :: NWETGRG  = 0 ! WET Growth of graupel   ICE3
INTEGER, SAVE :: NDRYGRG  = 0 ! DRY Growth of graupel   ICE3
INTEGER, SAVE :: NGMLTRG  = 0 ! Graupel MeLTing         ICE3
INTEGER, SAVE :: NWETHRG  = 0 ! wet growth of hail      ICE4
INTEGER, SAVE :: NDRYHRG  = 0 ! dry growth of hail      ICE4
INTEGER, SAVE :: NCORRRG  = 0 ! tendencies correction after ICE3
INTEGER, SAVE :: NHGCVRG  = 0 ! Hail to Graupel ConVersion ICE4
INTEGER, SAVE :: NGHCVRG  = 0 ! Graupel to Hail ConVersion ICE4
INTEGER, SAVE :: NHONRRG  = 0 ! drop homogeneous nucleation LIMA
INTEGER, SAVE :: NHMGRG   = 0 ! Hallett-Mossop ice multiplication process due to graupel riming
INTEGER, SAVE :: NCOHGRG  = 0 ! conversion of hail to graupel
INTEGER, SAVE :: NVISCRG  = 0 ! viscosity
!
!      Allowed processes for the budget of moist variable RRH (hail)
!
! Courant namelist: NAM_BURRH
!
LOGICAL, SAVE :: LBU_RRH = .FALSE. ! True when the budget of RRH is performed
!
INTEGER, SAVE :: NASSERH  = 0 ! time filter
INTEGER, SAVE :: NNESTRH  = 0 ! Efffect of 2way nesting on Rh
INTEGER, SAVE :: NADVRH   = 0 ! Total advection for PPM
INTEGER, SAVE :: NFRCRH   = 0 ! forcing
INTEGER, SAVE :: NDIFRH   = 0 ! numerical diffusion
INTEGER, SAVE :: NRELRH   = 0 ! relaxation
INTEGER, SAVE :: NNEGARH  = 0 ! negative correction
INTEGER, SAVE :: NNETURRH = 0 ! negative correction
INTEGER, SAVE :: NNEADVRH = 0 ! negative correction
INTEGER, SAVE :: NNECONRH = 0 ! negative correction
INTEGER, SAVE :: NSEDIRH  = 0 ! sedimentation
INTEGER, SAVE :: NWETGRH  = 0 ! wet growth of graupel
INTEGER, SAVE :: NWETHRH  = 0 ! wet growth of hail
INTEGER, SAVE :: NCOHGRH  = 0 ! reconversion from hail to graupel LIMA
INTEGER, SAVE :: NDRYHRH  = 0 ! dry growth of hail      ICE4
INTEGER, SAVE :: NHMLTRH  = 0 ! melting
INTEGER, SAVE :: NCORRRH  = 0 ! tendencies correction after ICE3
INTEGER, SAVE :: NHGCVRH  = 0 ! Hail to Graupel ConVersion ICE4
INTEGER, SAVE :: NGHCVRH  = 0 ! Graupel to Hail ConVersion ICE4
INTEGER, SAVE :: NVISCRH  = 0 ! viscosity
!
! Courant namelist: NAM_BURSV
!
LOGICAL, SAVE :: LBU_RSV = .FALSE. ! True when the budget of RSVx is performed
!
INTEGER, SAVE :: NASSESV   = 0 ! Asselin-Robert time filter
INTEGER, SAVE :: NNESTSV   = 0 ! Efffect of 2way nesting on Sv
INTEGER, SAVE :: NADVSV    = 0 ! Total advection for PPM
INTEGER, SAVE :: NFRCSV    = 0 ! forcing
INTEGER, SAVE :: NDIFSV    = 0 ! numerical diffusion
INTEGER, SAVE :: NRELSV    = 0 ! relaxation
INTEGER, SAVE :: NDCONVSV  = 0 !  Deep CONVection
INTEGER, SAVE :: NMAFLSV   = 0 ! mass flux
INTEGER, SAVE :: NDEPOTRSV = 0 ! deposition on tree
INTEGER, SAVE :: NHTURBSV  = 0 ! horizontal turbulence
INTEGER, SAVE :: NVTURBSV  = 0 ! vertical turbulence
INTEGER, SAVE :: NCHEMSV   = 0 ! chemistry activity
INTEGER, SAVE :: NVISCSV   = 0 ! viscosity
!
INTEGER, SAVE :: NNEGASV  = 0 ! negative correction
INTEGER, SAVE :: NNETURSV = 0 ! negative correction
INTEGER, SAVE :: NNEADVSV = 0 ! negative correction
INTEGER, SAVE :: NNECONSV = 0 ! negative correction
!
! Allowed processes for the budget of electric charge carried by water vapor
INTEGER, SAVE :: NDEPSQV   = 0
INTEGER, SAVE :: NDEPGQV   = 0
INTEGER, SAVE :: NREVAQV   = 0
INTEGER, SAVE :: NCDEPIQV  = 0
INTEGER, SAVE :: NNEUTQV   = 0
!
! Allowed processes for the budget of electric charge carried by cloud droplets
INTEGER, SAVE :: NHONQC    = 0
INTEGER, SAVE :: NAUTOQC   = 0
INTEGER, SAVE :: NACCRQC   = 0
INTEGER, SAVE :: NRIMQC    = 0
INTEGER, SAVE :: NWETGQC   = 0
INTEGER, SAVE :: NDRYGQC   = 0
INTEGER, SAVE :: NINCGQC   = 0
INTEGER, SAVE :: NWETHQC   = 0
INTEGER, SAVE :: NIMLTQC   = 0
INTEGER, SAVE :: NBERFIQC  = 0
INTEGER, SAVE :: NSEDIQC   = 0
INTEGER, SAVE :: NCDEPIQC  = 0
INTEGER, SAVE :: NNEUTQC   = 0
!
! Allowed processes for the budget of electric charge carried by rain drops
INTEGER, SAVE :: NSFRQR    = 0
INTEGER, SAVE :: NAUTOQR   = 0
INTEGER, SAVE :: NACCRQR   = 0
INTEGER, SAVE :: NREVAQR   = 0
INTEGER, SAVE :: NACCQR    = 0
INTEGER, SAVE :: NCFRZQR   = 0
INTEGER, SAVE :: NWETGQR   = 0
INTEGER, SAVE :: NDRYGQR   = 0
INTEGER, SAVE :: NGMLTQR   = 0
INTEGER, SAVE :: NWETHQR   = 0
INTEGER, SAVE :: NHMLTQR   = 0
INTEGER, SAVE :: NSEDIQR   = 0
INTEGER, SAVE :: NNEUTQR   = 0
!
! Allowed processes for the budget of electric charge carried by ice crystals
INTEGER, SAVE :: NHONQI    = 0
INTEGER, SAVE :: NAGGSQI   = 0
INTEGER, SAVE :: NAUTSQI   = 0
INTEGER, SAVE :: NCFRZQI   = 0
INTEGER, SAVE :: NWETGQI   = 0
INTEGER, SAVE :: NDRYGQI   = 0
INTEGER, SAVE :: NWETHQI   = 0
INTEGER, SAVE :: NIMLTQI   = 0
INTEGER, SAVE :: NBERFIQI  = 0
INTEGER, SAVE :: NNIISQI   = 0 ! non-inductive I-S
INTEGER, SAVE :: NSEDIQI   = 0
INTEGER, SAVE :: NCDEPIQI  = 0
INTEGER, SAVE :: NNEUTQI   = 0
!
! Allowed processes for the budget of electric charge carried by snow
INTEGER, SAVE :: NDEPSQS   = 0
INTEGER, SAVE :: NAGGSQS   = 0
INTEGER, SAVE :: NAUTSQS   = 0
INTEGER, SAVE :: NRIMQS    = 0
INTEGER, SAVE :: NACCQS    = 0
INTEGER, SAVE :: NCMELQS   = 0
INTEGER, SAVE :: NWETGQS   = 0
INTEGER, SAVE :: NDRYGQS   = 0
INTEGER, SAVE :: NNIISQS   = 0 ! non-inductive I-S
INTEGER, SAVE :: NWETHQS   = 0
INTEGER, SAVE :: NSEDIQS   = 0
INTEGER, SAVE :: NNEUTQS   = 0
!
! Allowed processes for the budget of electric charge carried by graupel
INTEGER, SAVE :: NSFRQG    = 0
INTEGER, SAVE :: NDEPGQG   = 0
INTEGER, SAVE :: NRIMQG    = 0
INTEGER, SAVE :: NACCQG    = 0
INTEGER, SAVE :: NCMELQG   = 0
INTEGER, SAVE :: NCFRZQG   = 0
INTEGER, SAVE :: NWETGQG   = 0
INTEGER, SAVE :: NDRYGQG   = 0
INTEGER, SAVE :: NINCGQG   = 0
INTEGER, SAVE :: NGMLTQG   = 0
INTEGER, SAVE :: NWETHQG   = 0
INTEGER, SAVE :: NSEDIQG   = 0
INTEGER, SAVE :: NNEUTQG   = 0
!
! Allowed processes for the budget of electric charge carried by hail
INTEGER, SAVE :: NWETGQH   = 0
INTEGER, SAVE :: NWETHQH   = 0
INTEGER, SAVE :: NHMLTQH   = 0
INTEGER, SAVE :: NSEDIQH   = 0
INTEGER, SAVE :: NNEUTQH   = 0
!
! Allowed processes for the budget of electric charge carried by negative ions
INTEGER, SAVE :: NDEPSNI   = 0
INTEGER, SAVE :: NDEPGNI   = 0
INTEGER, SAVE :: NREVANI   = 0
INTEGER, SAVE :: NCDEPINI  = 0
INTEGER, SAVE :: NNEUTNI   = 0
!
!
REAL :: XTIME_BU          ! budget time in this time-step
REAL :: XTIME_BU_PROCESS  ! budget time per process for this time-step
!
LOGICAL :: LBUDGET_U  ! flag to compute budget of RhoJu  and/or LES budgets with u
LOGICAL :: LBUDGET_V  ! flag to compute budget of RhoJv  and/or LES budgets with u
LOGICAL :: LBUDGET_W  ! flag to compute budget of RhoJw  and/or LES budgets with u
LOGICAL :: LBUDGET_TH ! flag to compute budget of RhoJTh and/or LES budgets with th
LOGICAL :: LBUDGET_TKE! flag to compute budget of RhoJTke and/or LES budgets with Tke
LOGICAL :: LBUDGET_RV ! flag to compute budget of RhoJrv and/or LES budgets with rv
LOGICAL :: LBUDGET_RC ! flag to compute budget of RhoJrc and/or LES budgets with rc
LOGICAL :: LBUDGET_RR ! flag to compute budget of RhoJrr and/or LES budgets with rr
LOGICAL :: LBUDGET_RI ! flag to compute budget of RhoJri and/or LES budgets with ri
LOGICAL :: LBUDGET_RS ! flag to compute budget of RhoJrs and/or LES budgets with rs
LOGICAL :: LBUDGET_RG ! flag to compute budget of RhoJrg and/or LES budgets with rg
LOGICAL :: LBUDGET_RH ! flag to compute budget of RhoJrh and/or LES budgets with rh
LOGICAL :: LBUDGET_SV ! flag to compute budget of RhoJsv and/or LES budgets with sv
!
END MODULE MODD_BUDGET
