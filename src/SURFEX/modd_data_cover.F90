!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #####################
      MODULE MODD_DATA_COVER
!     #####################
!
!!****  *MODD_DATA_COVER* - declaration of correspondances between surface
!!                          classes and variables
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify the 
!       arrays with correspondances between surface classes and
!       variables
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    15/12/97                      
!!      F.solmon    01/06/00 adaptation for patch approach + 
!!                           1D for surface fields of ISBA
!!      V. Masson   01/2004  surface externalization
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      P Le Moigne 06/2006 seeding and irrigation
!!      G Pigeon    08/2012 ROUGH_ROOF, ROUGH_WALL
!!      V. Masson   08/2013  Adds solar panel variables
!!      P Samuelsson 10/2014 Multi-energy balance (MEB)
!!      M. Goret     01/2017 Add CO2 fluxes
!!      M. Goret     01/2017 suppress EFF_HEAT
!!      M. Goret     10/2017 add hot water
!!      J.Etchanchu 01/2018 Add irrigation variables (A. Druel 02/2019 Add IRRIGFRAC + change dim)
!!----------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
USE MODD_TYPE_DATE_SURF      
!
IMPLICIT NONE
!
LOGICAL :: LREAD_DATA_COVER    ! true if ecoclimap(2) parameters are read in binary files
LOGICAL :: LDATA_IRRIG    ! true if ecoclimap2 irrigation parameters have been modified by user
LOGICAL :: LCLIM_LAI =.TRUE.  ! F: uses current year LAI (if between 2002 and 2006). 
!                              ! T: uses average of LAI  (average is done using the 5 years) 
INTEGER :: NECO2_START_YEAR   =2002     ! first year of data for ecoclimap2
INTEGER :: NECO2_END_YEAR     =2006     ! last  year of data for ecoclimap2
!-------------------------------------------------------------------------------
!
! Mask and number of grid elements containing patches/tiles:
!
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_VEGTYPE   ! fractions of veg. types
!
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_DICE       ! depth of the soil column for the calculation
!                                                       of the frozen soil fraction (m)
!
! Input Parameters, per patch:
!
! - vegetation + bare soil:
!
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_Z0_O_Z0H! ratio of z0 for momentum and heat
!
REAL, DIMENSION(:,:,:), ALLOCATABLE  :: XDATA_EMIS_ECO! (emissivity vg + bare ground)
!                                                     !  monthly
REAL, DIMENSION(:,:,:), ALLOCATABLE :: XDATA_Z0     ! dynamical veg. roughness length
!
! - vegetation:                                                   ! (monthly)
! 
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_ALBNIR_VEG ! near infra-red albedo
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_ALBVIS_VEG ! visible albedo
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_ALBUV_VEG  ! UV albedo
!
REAL, DIMENSION(:,:,:), ALLOCATABLE ::  XDATA_ALB_VEG_NIR  ! near infra-red albedo
REAL, DIMENSION(:,:,:), ALLOCATABLE ::  XDATA_ALB_VEG_VIS  ! visible albedo
REAL, DIMENSION(:,:,:), ALLOCATABLE ::  XDATA_ALB_SOIL_NIR ! near infra-red albedo
REAL, DIMENSION(:,:,:), ALLOCATABLE ::  XDATA_ALB_SOIL_VIS ! visible albedo
!
! - vegetation: default option (Jarvis) and general parameters:
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: XDATA_VEG    ! veg. fraction   (monthly)
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_WRMAX_CF ! coefficient for interception reservoir
!
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_RSMIN  ! minimum stomatal resistance
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_GAMMA  ! 
!
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_CV     ! inverse of veg. thermal capacity
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_RGL    !
!
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_ROOT_EXTINCTION! root extinction
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_ROOT_LIN       ! ponderation coefficient between
                                                            ! root frac.
                                                            ! formulations
!       
!-------------------------------------------------------------------------------
!
! - vegetation: Ags parameters ('AST', 'NIT', 'NCB' options)
!
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_BSLAI   ! ratio d(biomass)/d(lai)
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_SEFOLD  ! e-folding time for senescence (s)
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_GMES    ! mesophyll conductance (m s-1)
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_GC      ! cuticular conductance (m/s)
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_DMAX    ! maximum air saturation deficit
!                                                    ! tolerate by vegetation       (kg/kg)
!
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_BSLAI_ST  ! ratio d(biomass)/d(lai)
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_SEFOLD_ST ! e-folding time for senescence (s)
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_GMES_ST   ! mesophyll conductance (m s-1)
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_GC_ST     ! cuticular conductance (m/s)
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_DMAX_ST   ! maximum air saturation deficit
!                                                      ! tolerate by vegetation       (kg/kg)
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Stress parameters ('AST', 'NIT', 'NCB' options)
!
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_LAIMIN  ! minimum LAI
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_H_TREE  ! height of vegetation
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_HTRUNK_HVEG ! height of TRUNK of trees
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_WCROWN_HVEG ! width of crown of trees
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_RE25    ! Ecosystem Respiration parameter (kg.m-2.s-1)
!
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_STRESS  !  defensive/offensive strategy (1/0)
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_F2I     ! critical normilized soil water 
!                                                    ! content for stress parameterisation
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Nitrogen-model parameters ('NIT', 'NCB' option)
!
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_CE_NITRO!  leaf aera ratio sensitivity 
                                                     !  to nitrogen concentration
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_CF_NITRO!  lethal minimum value of leaf area ratio
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_CNA_NITRO! nitrogen concentration of active biomass
!
!-------------------------------------------------------------------------------
!
! - soil: primary parameters
!
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_GROUND_DEPTH   ! total soil depth
REAL, DIMENSION(:,:),   ALLOCATABLE :: XDATA_ROOT_DEPTH     ! root depth
!-------------------------------------------------------------------------------
!
! - Vegetation: Ags Prognostic (YPHOTO = ('NIT', or 'NCB') or prescribed (YPHOTO='NON' or 'AST')
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: XDATA_LAI    ! leaf area index (monthly)
REAL, DIMENSION(:,:,:), ALLOCATABLE :: XDATA_LAI_ALL_YEARS ! leaf area index fromm 2002 to 2007       
!
!
TYPE (DATE_TIME), POINTER, DIMENSION(:,:)   :: TDATA_SEED     ! seeding date      
TYPE (DATE_TIME), POINTER, DIMENSION(:,:)   :: TDATA_REAP     ! reaping date      
!      
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_WATSUP      ! water supply quantity
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_IRRIGTYPE   ! irrigation type
!
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_IRRIGFRAC   ! irrigation maximal frequency
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_IRRIGFREQ   ! irrigation maximal frequency
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_IRRIGTIME   ! irrigation amount application time
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: XDATA_F2THRESHOLD    ! f2 threshold for irrigation triggering
!
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_TOWN   ! artificial surfaces fraction
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_NATURE ! natural and cul. fraction
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_WATER  ! inland water fraction
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_SEA    ! sea fraction
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: XDATA_GREEN  ! greeness fraction   (monthly)
REAL, DIMENSION(:,:),  ALLOCATABLE :: XDATA_SOILRC_SO2 ! for SO2 deposition
REAL, DIMENSION(:,:),  ALLOCATABLE :: XDATA_SOILRC_O3  ! for O3  deposition
!
!
! Geometric Parameters:
!
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_GARDEN      ! garden fraction
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_BLD         ! building fraction in
                                                       ! artificial areas
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_ROAD        ! road fraction
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_FRAC_HVEG   ! fraction of high vegetation
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_FRAC_LVEG   ! fraction of low  vegetation
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_FRAC_NVEG   ! fraction of no   vegetation
!
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_BLD_HEIGHT  ! buildings height h
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_WALL_O_HOR  ! ratio of vert. surf.
!                                                      ! over hor. surf.
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_CAN_HW_RATIO! canyons h/W ratio
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_Z0_TOWN   ! town roughness length
                                                     ! for momentum
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_Z0H_TOWN  ! town roughness length
                                                     ! for heat                                                     
!
! Roof parameters
!
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_ALB_ROOF  ! albedo of roofs
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_EMIS_ROOF ! emissivity of roofs
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_HC_ROOF   ! heat capacity
                                                     ! for roof layers
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_TC_ROOF   ! thermal conductivity
                                                     ! for roof layers
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_D_ROOF    ! width of roof layers
!
! Road parameters
!
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_ALB_ROAD  ! albedo of roads
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_EMIS_ROAD ! emissivity of roads
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_HC_ROAD   ! heat capacity
                                                     ! for road layers
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_TC_ROAD   ! thermal conductivity
                                                     ! for road layers
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_D_ROAD    ! width of road layers
!
! Wall parameters
!
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_ALB_WALL  ! albedo of roads
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_EMIS_WALL ! emissivity of roads
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_HC_WALL   ! heat capacity
                                                     ! for wall layers
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_TC_WALL   ! thermal conductivity
                                                     ! for wall layers
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_D_WALL    ! width of wall layers
!
! anthropogenic fluxes
!
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_H_TRAFFIC   ! anthropogenic sensible
!                                                      ! heat fluxes due to traffic
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_LE_TRAFFIC  ! anthropogenic latent
!                                                      ! heat fluxes due to traffic
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_H_INDUSTRY  ! anthropogenic sensible
!                                                      ! heat fluxes due to factories
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_LE_INDUSTRY ! anthropogenic latent
!                                                      ! heat fluxes due to factories
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_HC_FLOOR     ! heat capacity of floor layers [J m-3 K-1]
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_TC_FLOOR     ! thermal conductivity of floor layers [W m-1 K-1]
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_D_FLOOR      ! thickness of floor layers [m]
!
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_HC_MASS     ! heat capacity of mass layers [J m-3 K-1]
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_TC_MASS     ! thermal conductivity of mass layers [W m-1 K-1]
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_D_MASS      ! thickness of mass layers [m]
!
! For multi-energy balance (MEB)
REAL, DIMENSION(:,:,:), ALLOCATABLE :: XDATA_GNDLITTER          ! Ground litter coverage
REAL, DIMENSION(:,:,:), ALLOCATABLE :: XDATA_Z0LITTER           ! Ground litter roughness length
REAL, DIMENSION(:,:,:), ALLOCATABLE :: XDATA_H_VEG              ! Height of canopy vegetation
!
! For Building Energy Module (BEM)
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_FRACOMP              ! Fraction of compartments in bem
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_RESIDENTIAL          ! Fraction of residential use
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_THEAT_OCCD
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_THEAT_OCCN
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_THEAT_VCDD
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_THEAT_VCDN
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_THEAT_VCLD
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_TCOOL_OCCD
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_TCOOL_OCCN
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_TCOOL_VCDD
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_TCOOL_VCDN
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_TCOOL_VCLD
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_FVSUM
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_FVVAC
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_FSSUM
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_FSVAC
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_PROBOCC
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_DAYWBEG_SCHED
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_HOURBEG_SCHED
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_BEG_HOLIDAY
REAL, DIMENSION(:,:), ALLOCATABLE :: XDATA_END_HOLIDAY
REAL, DIMENSION(:)  , ALLOCATABLE :: XDATA_MOD_HOLIDAY
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_F_WASTE_CAN  ! fraction of waste heat released into the canyon
!
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_QIN          ! internal heat gains [W m-2(floor)]
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_QIN_FRAD     ! radiant fraction of internal heat gains
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_SHGC         ! solar transmitance of windows
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_U_WIN        ! glazing thermal resistance [K m W-2]
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_GR           ! glazing ratio
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_SHGC_SH      ! solar transmitance of windows
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_FLOOR_HEIGHT ! building floor height [m]
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_ISMASS       ! Presence of internal mass ?
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_N50          ! infiltration/ventilation flow rate [AC/H]
!
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_F_WATER_COND
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_DCS_AREA
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_QIN_FLAT
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_HR_TARGET
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_CAP_SYS_HEAT
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_CAP_SYS_RAT
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_T_ADP
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_M_SYS_RAT
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_COP_RAT      ! COP of the cooling system
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_COP_DCS      ! COP of the district cooling system
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_T_SIZE_MAX
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_T_SIZE_MIN
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_ISMECH
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_MECHRATE
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_SHADEARCHI
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_NATVENT
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_ROUGH_ROOF
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_ROUGH_WALL
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_FSNIG
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_FVNIG
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_MODQIN_VCD
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_MODQIN_VLD     
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_MODQIN_NIG
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_TDESV
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_WIN_SW_MAX
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_FOPEN
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_EMIS_PANEL  ! emissivity of solar panels
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_ALB_PANEL   ! albedo     of solar panels
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_EFF_PANEL   ! efficiency of solar panels
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_FRAC_PANEL  ! fraction   of solar panels on roofs
!
! For CO2 fluxes
REAL, DIMENSION(:), ALLOCATABLE :: XDATA_FRAC_HEAT_ELEC  ! Fraction of electric heating
REAL, DIMENSION(:), ALLOCATABLE :: XDATA_FRAC_HEAT_GAS   ! Fraction of town gas heating
REAL, DIMENSION(:), ALLOCATABLE :: XDATA_FRAC_HEAT_FUEL  ! Fraction of fuel heating
REAL, DIMENSION(:), ALLOCATABLE :: XDATA_FRAC_HEAT_OTHER ! Fraction of other heating
REAL, DIMENSION(:), ALLOCATABLE :: XDATA_NB_POP          ! Number of people per square kilometer
REAL, DIMENSION(:), ALLOCATABLE :: XDATA_SFCO2_RD        ! CO2 flux link to traffic (roads) (kg/m2 of town/s)
REAL, DIMENSION(:), ALLOCATABLE :: XDATA_HOTWAT          ! Energy needed for hot water [W m-2(floor)]
REAL, DIMENSION(:), ALLOCATABLE :: XDATA_F_HW_GAS        ! Fraction of water heat by gas
!
!
! urban vegetation parameters
!
REAL, DIMENSION(:),   ALLOCATABLE :: XDATA_FRAC_GR     ! fraction of greenroof
!
!-------------------------------------------------------------------------------
!
END MODULE MODD_DATA_COVER
