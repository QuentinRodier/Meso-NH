!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_BLD_DESCRIPTION_n
!     ################
!
!!****  *MODD_BLD_DESCRIPTION_n - declaration of surface parameters for typical
!                               buildings
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
!!      G. Pigeon   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       08/2011
!!       V. Masson     08/2013 adds solar panels
!!       V. Masson     10/2013 adds residential fraction
!!----------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE BLD_DESC_t
!
 CHARACTER(LEN=50), POINTER, DIMENSION(:) :: YDESC_TYPNAME ! Names of building types
 CHARACTER(LEN=50), POINTER, DIMENSION(:) :: YDESC_USENAME ! Names of building uses
 CHARACTER(LEN=50), POINTER, DIMENSION(:) :: YDESC_AGENAME ! Names of building construction periods
 CHARACTER(LEN=50), POINTER, DIMENSION(:) :: YDESC_TERNAME ! Names of building construction material territories   
!
  INTEGER                       :: NDESC_BLD          ! number of types of buildings
  INTEGER                       :: NDESC_AGE          ! number of building's construction dates ranges
  INTEGER                       :: NDESC_CODE         ! number of codes for buildings (merges type & age)
  INTEGER                       :: NDESC_USE          ! number of types of building's uses
  INTEGER                       :: NDESC_TER          ! Number of material territories
  INTEGER                       :: NDESC_NDAY_SCHED   ! Number of days in schedules
  INTEGER                       :: NDESC_NCRE_SCHED   ! Number of time periods in schedules
  INTEGER                       :: NDESC_HOLIDAY      ! Number of holidays
  INTEGER                       :: NDESC_CONDP        ! Number of conditional probabilities  
  INTEGER                       :: NDESC_ROOF_LAYER   ! number of layers in roofs
  INTEGER                       :: NDESC_ROAD_LAYER   ! number of layers in roads
  INTEGER                       :: NDESC_WALL_LAYER   ! number of layers in walls
  INTEGER                       :: NDESC_FLOOR_LAYER  ! number of layers in floor
  INTEGER                       :: NDESC_MASS_LAYER   ! number of layers in mass
!
! The positions of building types, uses and construction periods
! corresponding to the MApUCE classification
!
  INTEGER                       :: NDESC_POS_TYP_PD   
  INTEGER                       :: NDESC_POS_TYP_PSC  
  INTEGER                       :: NDESC_POS_TYP_PCIO 
  INTEGER                       :: NDESC_POS_TYP_PCIF 
  INTEGER                       :: NDESC_POS_TYP_ID   
  INTEGER                       :: NDESC_POS_TYP_ICIO 
  INTEGER                       :: NDESC_POS_TYP_ICIF 
  INTEGER                       :: NDESC_POS_TYP_BGH  
  INTEGER                       :: NDESC_POS_TYP_BA   
  INTEGER                       :: NDESC_POS_TYP_LOCA       
  INTEGER                       :: NDESC_POS_USE_AGR 
  INTEGER                       :: NDESC_POS_USE_CHA 
  INTEGER                       :: NDESC_POS_USE_COM 
  INTEGER                       :: NDESC_POS_USE_HAC 
  INTEGER                       :: NDESC_POS_USE_HAI 
  INTEGER                       :: NDESC_POS_USE_IND 
  INTEGER                       :: NDESC_POS_USE_LNC 
  INTEGER                       :: NDESC_POS_USE_REL 
  INTEGER                       :: NDESC_POS_USE_SAN 
  INTEGER                       :: NDESC_POS_USE_ENS 
  INTEGER                       :: NDESC_POS_USE_SER 
  INTEGER                       :: NDESC_POS_USE_SPO 
  INTEGER                       :: NDESC_POS_USE_TER        
  INTEGER                       :: NDESC_POS_AGE_P1
  INTEGER                       :: NDESC_POS_AGE_P2
  INTEGER                       :: NDESC_POS_AGE_P3
  INTEGER                       :: NDESC_POS_AGE_P4
  INTEGER                       :: NDESC_POS_AGE_P5
  INTEGER                       :: NDESC_POS_AGE_P6
  INTEGER                       :: NDESC_POS_AGE_P7
  INTEGER                       :: NDESC_POS_PX_DEFAULT
  INTEGER                       :: NDESC_POS_HAI_FORTCRE
  INTEGER                       :: NDESC_POS_HAI_FAIBCRE
  INTEGER                       :: NDESC_POS_HAC_FORTCRE
  INTEGER                       :: NDESC_POS_HAC_FAIBCRE
  !
  INTEGER, POINTER, DIMENSION(:):: NDESC_BLD_LIST     ! list of the types for buildings
  INTEGER, POINTER, DIMENSION(:):: NDESC_AGE_DATE     ! list of the contruction dates for buildings
  INTEGER, POINTER, DIMENSION(:):: NDESC_CODE_LIST    ! list of the codes for buildings
  INTEGER, POINTER, DIMENSION(:):: NDESC_AGE_LIST     ! list of the contruction dates' codes
  INTEGER, POINTER, DIMENSION(:):: NDESC_USE_LIST     ! list of the codes for building's uses
  INTEGER, POINTER, DIMENSION(:):: NDESC_TER_LIST     ! List of the codes for building's territories
  !
  REAL, POINTER, DIMENSION(:)   :: XDESC_ALB_ROOF     ! Roof albedo
  REAL, POINTER, DIMENSION(:)   :: XDESC_ALB_ROAD     ! Road albedo
  REAL, POINTER, DIMENSION(:)   :: XDESC_ALB_WALL     ! Wall albedo
  REAL, POINTER, DIMENSION(:)   :: XDESC_EMIS_ROOF    ! Roof emissivity
  REAL, POINTER, DIMENSION(:)   :: XDESC_EMIS_ROAD    ! Road emissivity
  REAL, POINTER, DIMENSION(:)   :: XDESC_EMIS_WALL    ! Wall emissivity
  REAL, POINTER, DIMENSION(:,:) :: XDESC_HC_FLOOR     ! heat capacity of floor layers [J m-3 K-1]
  REAL, POINTER, DIMENSION(:,:) :: XDESC_TC_FLOOR     ! thermal conductivity of floor layers [W m-1 K-1]
  REAL, POINTER, DIMENSION(:,:) :: XDESC_D_FLOOR      ! thickness of floor layers [m]
  REAL, POINTER, DIMENSION(:,:) :: XDESC_HC_MASS      ! heat capacity of mass layers [J m-3 K-1]
  REAL, POINTER, DIMENSION(:,:) :: XDESC_TC_MASS      ! thermal conductivity of mass layers [W m-1 K-1]
  REAL, POINTER, DIMENSION(:,:) :: XDESC_D_MASS       ! thickness of mass layers [m]
  REAL, POINTER, DIMENSION(:)   :: XDESC_ISMASS       ! Flag for presence of internal mass [1]
  REAL, POINTER, DIMENSION(:,:) :: XDESC_HC_ROOF      ! heat capacity of roof layers [J m-3 K-1]
  REAL, POINTER, DIMENSION(:,:) :: XDESC_TC_ROOF      ! thermal conductivity of roof layers [W m-1 K-1]
  REAL, POINTER, DIMENSION(:,:) :: XDESC_D_ROOF       ! thickness of roof layers [m]
  REAL, POINTER, DIMENSION(:,:) :: XDESC_HC_ROAD      ! heat capacity of road layers [J m-3 K-1]
  REAL, POINTER, DIMENSION(:,:) :: XDESC_TC_ROAD      ! thermal conductivity of road layers [W m-1 K-1]
  REAL, POINTER, DIMENSION(:,:) :: XDESC_D_ROAD       ! thickness of road layers [m]
  REAL, POINTER, DIMENSION(:,:) :: XDESC_HC_WALL      ! heat capacity of wall layers [J m-3 K-1]
  REAL, POINTER, DIMENSION(:,:) :: XDESC_TC_WALL      ! thermal conductivity of wall layers [W m-1 K-1]
  REAL, POINTER, DIMENSION(:,:) :: XDESC_D_WALL       ! thickness of wall layers [m]
  INTEGER, POINTER, DIMENSION(:):: NDESC_ISOROOFPOS   ! Position of isolation material in roof
  INTEGER, POINTER, DIMENSION(:):: NDESC_ISOWALLPOS   ! Position of isolation material in wall
  REAL, POINTER, DIMENSION(:)   :: XDESC_QIN          ! internal heat gains [W m-2(floor)]
  REAL, POINTER, DIMENSION(:)   :: XDESC_QIN_ADDBEHAV ! Modulation due to behavioural indicators for internal heat gains [W m-2(floor)]
  REAL, POINTER, DIMENSION(:)   :: XDESC_QIN_FRAD     ! radiant fraction of internal heat gains [1]
  REAL, POINTER, DIMENSION(:)   :: XDESC_QIN_FLAT     ! Latent franction of internal heat gains [1]
  REAL, POINTER, DIMENSION(:)   :: XDESC_MODQIN_VCD   ! Modulation of internal heat gains, short term vacancy [1]
  REAL, POINTER, DIMENSION(:)   :: XDESC_MODQIN_VLD   ! Modulation of internal heat gains, long term vacancy [1]
  REAL, POINTER, DIMENSION(:)   :: XDESC_MODQIN_NIG   ! Modulation of internal heat gains, night
  REAL, POINTER, DIMENSION(:)   :: XDESC_HOTWAT       ! Energy consumption for residential warm water [W/m²(floor)]
  REAL, POINTER, DIMENSION(:)   :: XDESC_SHGC         ! solar transmitance of windows
  REAL, POINTER, DIMENSION(:)   :: XDESC_U_WIN        ! glazing thermal resistance [K m W-2]
  REAL, POINTER, DIMENSION(:)   :: XDESC_GR           ! glazing ratio
  REAL, POINTER, DIMENSION(:)   :: XDESC_SHGC_SH      ! solar transmitance of windows + shading
  REAL, POINTER, DIMENSION(:)   :: XDESC_N50          ! airtightness [AC/H at 50 Pa]
  REAL, POINTER, DIMENSION(:)   :: XDESC_ISMECH       ! Presence of mecanical ventilation (architectural characteristic)
  REAL, POINTER, DIMENSION(:)   :: XDESC_MECHRATE     ! Exchange rate due to mecanical ventilation [1/h] (architectural characteristic)
  REAL, POINTER, DIMENSION(:)   :: XDESC_SHADEARCHI   ! Are shading devices present? (architectural characteristic)
!
  INTEGER, POINTER, DIMENSION(:,:) :: XDESC_BEG_HOLIDAY   ! Julian day of holiday begin
  INTEGER, POINTER, DIMENSION(:,:) :: XDESC_END_HOLIDAY   ! Julian day of holiday end
  REAL, POINTER, DIMENSION(:)   :: XDESC_MOD_HOLIDAY   ! Modulation factor for building occppation during holidays
  REAL, POINTER, DIMENSION(:,:) :: XDESC_PROBOCC       ! Probability of building occupation
  REAL, POINTER, DIMENSION(:,:) :: XDESC_DAYWBEG_SCHED ! Day of week for start of schedule
  REAL, POINTER, DIMENSION(:,:) :: XDESC_HOURBEG_SCHED ! Hour of day for start of schedule
  REAL, POINTER, DIMENSION(:)   :: XDESC_FLDT          ! Fraction of households heating to lower design temperature
  REAL, POINTER, DIMENSION(:)   :: XDESC_FIDT          ! Fraction of households heating to intermediate design temperature
  REAL, POINTER, DIMENSION(:)   :: XDESC_FHDT          ! Fraction of households heating to higher design temperature
!
  REAL, POINTER, DIMENSION(:)   :: XDESC_THEAT_OCCD_AVG ! Average heating design temperature, occupied, day
  REAL, POINTER, DIMENSION(:)   :: XDESC_THEAT_OCCN_AVG ! Average heating design temperature, occupied, night
  REAL, POINTER, DIMENSION(:)   :: XDESC_THEAT_VCDD_AVG ! Average heating design temperature, vacant, day
  REAL, POINTER, DIMENSION(:)   :: XDESC_THEAT_VCDN_AVG ! Average heating design temperature, vacant, night
  REAL, POINTER, DIMENSION(:)   :: XDESC_THEAT_VCLD_AVG ! Average heating design temperature, long term vacancy
  REAL, POINTER, DIMENSION(:)   :: XDESC_FNOHEAT_AVG    ! Average non-heated fraction of the building
!
  REAL, POINTER, DIMENSION(:)   :: XDESC_THEAT_OCCD_MOD ! Modulation of heating design temperature, occupied, day
  REAL, POINTER, DIMENSION(:)   :: XDESC_THEAT_OCCN_MOD ! Modulation of heating design temperature, occupied, night
  REAL, POINTER, DIMENSION(:)   :: XDESC_THEAT_VCDD_MOD ! Modulation of heating design temperature, vacant, day
  REAL, POINTER, DIMENSION(:)   :: XDESC_THEAT_VCDN_MOD ! Modulation of heating design temperature, vacant, night
  REAL, POINTER, DIMENSION(:)   :: XDESC_THEAT_VCLD_MOD ! Modulation of heating design temperature, long term vacancy
  REAL, POINTER, DIMENSION(:)   :: XDESC_FNOHEAT_MOD    ! Modulation of non-heated fraction of the building
!
  REAL, POINTER, DIMENSION(:)   :: XDESC_TCOOL_OCCD    ! Cooling design temperature, occupied, day
  REAL, POINTER, DIMENSION(:)   :: XDESC_TCOOL_OCCN    ! Cooling design temperature, occupied, night
  REAL, POINTER, DIMENSION(:)   :: XDESC_TCOOL_VCDD    ! Cooling design temperature, vacant, day
  REAL, POINTER, DIMENSION(:)   :: XDESC_TCOOL_VCDN    ! Cooling design temperature, vacant, night
  REAL, POINTER, DIMENSION(:)   :: XDESC_TCOOL_VCLD    ! Cooling design temperature, long term vacancy
  REAL, POINTER, DIMENSION(:)   :: XDESC_F_WATER_COND  ! fraction of evaporation for condensers
  REAL, POINTER, DIMENSION(:)   :: XDESC_COP_RAT       ! Rated COP of the cooling system
  REAL, POINTER, DIMENSION(:)   :: XDESC_HR_TARGET     ! Relative humidity setpoint
  REAL, POINTER, DIMENSION(:)   :: XDESC_NATVENT       ! Switch for ventilation and shading
  REAL, POINTER, DIMENSION(:)   :: XDESC_FVSUM         ! Fraction of households opening windows in summer
  REAL, POINTER, DIMENSION(:)   :: XDESC_FVNIG         ! Fraction of households opening windows during the night
  REAL, POINTER, DIMENSION(:)   :: XDESC_TDESV         ! Design temperature for ventilation
  REAL, POINTER, DIMENSION(:)   :: XDESC_FVVAC         ! Fraction of households opening windows when vacant
  REAL, POINTER, DIMENSION(:)   :: XDESC_FOPEN         ! Fraction of windows opened when ventilation is done
  REAL, POINTER, DIMENSION(:)   :: XDESC_FSSUM         ! Fraction of households closing shades when when its hot
  REAL, POINTER, DIMENSION(:)   :: XDESC_FSNIG         ! Fraction of households closing shades during the night
  REAL, POINTER, DIMENSION(:)   :: XDESC_FSVAC         ! Fraction of households closing shades when vacant
  REAL, POINTER, DIMENSION(:)   :: XDESC_WIN_SW_MAX    ! Threshold for wall insolation for shading
  REAL, POINTER, DIMENSION(:)   :: XDESC_F_WASTE_CAN   ! Fraction of waste heat released into the canyon
  REAL, POINTER, DIMENSION(:)   :: XDESC_GREENROOF    ! Greenroof fraction
  REAL, POINTER, DIMENSION(:)   :: XDESC_EMIS_PANEL   ! Emissivity of Solar panels
  REAL, POINTER, DIMENSION(:)   :: XDESC_ALB_PANEL    ! Albedo     of Solar panels
  REAL, POINTER, DIMENSION(:)   :: XDESC_EFF_PANEL    ! Efficiency of Solar panels
  REAL, POINTER, DIMENSION(:)   :: XDESC_FRAC_PANEL   ! Fraction   of Solar panels on roofs
!
END TYPE BLD_DESC_t



CONTAINS

!
!


!
!
SUBROUTINE BLD_DESC_INIT(YBLD_DESC)
TYPE(BLD_DESC_t), INTENT(INOUT) :: YBLD_DESC
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_BLD_DESCRIPTION_n:BLD_DESC_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YBLD_DESC%NDESC_BLD_LIST)
  NULLIFY(YBLD_DESC%NDESC_CODE_LIST)
  NULLIFY(YBLD_DESC%NDESC_AGE_LIST)
  NULLIFY(YBLD_DESC%NDESC_AGE_DATE)
  NULLIFY(YBLD_DESC%NDESC_USE_LIST)
  NULLIFY(YBLD_DESC%NDESC_TER_LIST)
  NULLIFY(YBLD_DESC%YDESC_TYPNAME) 
  NULLIFY(YBLD_DESC%YDESC_USENAME)
  NULLIFY(YBLD_DESC%YDESC_AGENAME)
  NULLIFY(YBLD_DESC%YDESC_TERNAME)  
  NULLIFY(YBLD_DESC%XDESC_ALB_ROOF)
  NULLIFY(YBLD_DESC%XDESC_ALB_ROAD)
  NULLIFY(YBLD_DESC%XDESC_ALB_WALL)
  NULLIFY(YBLD_DESC%XDESC_EMIS_ROOF)
  NULLIFY(YBLD_DESC%XDESC_EMIS_ROAD)
  NULLIFY(YBLD_DESC%XDESC_EMIS_WALL)
  NULLIFY(YBLD_DESC%XDESC_HC_FLOOR)
  NULLIFY(YBLD_DESC%XDESC_TC_FLOOR)
  NULLIFY(YBLD_DESC%XDESC_D_FLOOR)
  NULLIFY(YBLD_DESC%XDESC_HC_MASS)
  NULLIFY(YBLD_DESC%XDESC_TC_MASS)
  NULLIFY(YBLD_DESC%XDESC_D_MASS)
  NULLIFY(YBLD_DESC%XDESC_ISMASS)
  NULLIFY(YBLD_DESC%XDESC_HC_ROOF)
  NULLIFY(YBLD_DESC%XDESC_TC_ROOF)
  NULLIFY(YBLD_DESC%XDESC_D_ROOF)
  NULLIFY(YBLD_DESC%XDESC_HC_ROAD)
  NULLIFY(YBLD_DESC%XDESC_TC_ROAD)
  NULLIFY(YBLD_DESC%XDESC_D_ROAD)
  NULLIFY(YBLD_DESC%XDESC_HC_WALL)
  NULLIFY(YBLD_DESC%XDESC_TC_WALL)
  NULLIFY(YBLD_DESC%XDESC_D_WALL)
  NULLIFY(YBLD_DESC%NDESC_ISOROOFPOS)
  NULLIFY(YBLD_DESC%NDESC_ISOWALLPOS)
  NULLIFY(YBLD_DESC%XDESC_QIN)
  NULLIFY(YBLD_DESC%XDESC_QIN_ADDBEHAV)
  NULLIFY(YBLD_DESC%XDESC_QIN_FRAD)
  NULLIFY(YBLD_DESC%XDESC_QIN_FLAT)
  NULLIFY(YBLD_DESC%XDESC_MODQIN_VCD)
  NULLIFY(YBLD_DESC%XDESC_MODQIN_VLD)
  NULLIFY(YBLD_DESC%XDESC_MODQIN_NIG)
  NULLIFY(YBLD_DESC%XDESC_HOTWAT)
  NULLIFY(YBLD_DESC%XDESC_BEG_HOLIDAY)
  NULLIFY(YBLD_DESC%XDESC_END_HOLIDAY)
  NULLIFY(YBLD_DESC%XDESC_MOD_HOLIDAY)
  NULLIFY(YBLD_DESC%XDESC_DAYWBEG_SCHED)
  NULLIFY(YBLD_DESC%XDESC_HOURBEG_SCHED)
  NULLIFY(YBLD_DESC%XDESC_PROBOCC)
  NULLIFY(YBLD_DESC%XDESC_FLDT)
  NULLIFY(YBLD_DESC%XDESC_FIDT)
  NULLIFY(YBLD_DESC%XDESC_FHDT)
  !
  NULLIFY(YBLD_DESC%XDESC_THEAT_OCCD_AVG)
  NULLIFY(YBLD_DESC%XDESC_THEAT_OCCN_AVG)
  NULLIFY(YBLD_DESC%XDESC_THEAT_VCDD_AVG)
  NULLIFY(YBLD_DESC%XDESC_THEAT_VCDN_AVG)
  NULLIFY(YBLD_DESC%XDESC_THEAT_VCLD_AVG)
  NULLIFY(YBLD_DESC%XDESC_FNOHEAT_AVG)
  !
  NULLIFY(YBLD_DESC%XDESC_THEAT_OCCD_MOD)
  NULLIFY(YBLD_DESC%XDESC_THEAT_OCCN_MOD)
  NULLIFY(YBLD_DESC%XDESC_THEAT_VCDD_MOD)
  NULLIFY(YBLD_DESC%XDESC_THEAT_VCDN_MOD)
  NULLIFY(YBLD_DESC%XDESC_THEAT_VCLD_MOD)
  NULLIFY(YBLD_DESC%XDESC_FNOHEAT_MOD)
  !
  NULLIFY(YBLD_DESC%XDESC_TCOOL_OCCD)
  NULLIFY(YBLD_DESC%XDESC_TCOOL_OCCN)
  NULLIFY(YBLD_DESC%XDESC_TCOOL_VCDD)
  NULLIFY(YBLD_DESC%XDESC_TCOOL_VCDN)
  NULLIFY(YBLD_DESC%XDESC_TCOOL_VCLD)
  NULLIFY(YBLD_DESC%XDESC_NATVENT)
  NULLIFY(YBLD_DESC%XDESC_FVSUM)
  NULLIFY(YBLD_DESC%XDESC_FVNIG)
  NULLIFY(YBLD_DESC%XDESC_TDESV)
  NULLIFY(YBLD_DESC%XDESC_FVVAC)
  NULLIFY(YBLD_DESC%XDESC_FOPEN)
  NULLIFY(YBLD_DESC%XDESC_FSSUM)
  NULLIFY(YBLD_DESC%XDESC_FSNIG)
  NULLIFY(YBLD_DESC%XDESC_FSVAC)
  NULLIFY(YBLD_DESC%XDESC_WIN_SW_MAX)
  NULLIFY(YBLD_DESC%XDESC_SHGC) 
  NULLIFY(YBLD_DESC%XDESC_U_WIN)
  NULLIFY(YBLD_DESC%XDESC_GR)
  NULLIFY(YBLD_DESC%XDESC_N50)
  NULLIFY(YBLD_DESC%XDESC_F_WATER_COND)
  NULLIFY(YBLD_DESC%XDESC_HR_TARGET)  
  NULLIFY(YBLD_DESC%XDESC_COP_RAT)
  NULLIFY(YBLD_DESC%XDESC_GREENROOF)
  NULLIFY(YBLD_DESC%XDESC_ISMECH)
  NULLIFY(YBLD_DESC%XDESC_MECHRATE)
  NULLIFY(YBLD_DESC%XDESC_SHADEARCHI)
  NULLIFY(YBLD_DESC%XDESC_EMIS_PANEL)
  NULLIFY(YBLD_DESC%XDESC_ALB_PANEL)
  NULLIFY(YBLD_DESC%XDESC_EFF_PANEL)
  NULLIFY(YBLD_DESC%XDESC_FRAC_PANEL)
YBLD_DESC%NDESC_BLD=0
YBLD_DESC%NDESC_AGE=0
YBLD_DESC%NDESC_CODE=0
YBLD_DESC%NDESC_USE=0
YBLD_DESC%NDESC_TER=0
YBLD_DESC%NDESC_NDAY_SCHED=0
YBLD_DESC%NDESC_NCRE_SCHED=0
YBLD_DESC%NDESC_HOLIDAY=0
YBLD_DESC%NDESC_CONDP=0
YBLD_DESC%NDESC_ROOF_LAYER=0
YBLD_DESC%NDESC_ROAD_LAYER=0
YBLD_DESC%NDESC_WALL_LAYER=0
YBLD_DESC%NDESC_FLOOR_LAYER=0
YBLD_DESC%NDESC_MASS_LAYER=0
!
YBLD_DESC%NDESC_POS_TYP_PD   = 0   
YBLD_DESC%NDESC_POS_TYP_PSC  = 0
YBLD_DESC%NDESC_POS_TYP_PCIO = 0
YBLD_DESC%NDESC_POS_TYP_PCIF = 0
YBLD_DESC%NDESC_POS_TYP_ID   = 0 
YBLD_DESC%NDESC_POS_TYP_ICIO = 0
YBLD_DESC%NDESC_POS_TYP_ICIF = 0
YBLD_DESC%NDESC_POS_TYP_BGH  = 0 
YBLD_DESC%NDESC_POS_TYP_BA   = 0
YBLD_DESC%NDESC_POS_TYP_LOCA = 0     
YBLD_DESC%NDESC_POS_USE_AGR  = 0
YBLD_DESC%NDESC_POS_USE_CHA  = 0
YBLD_DESC%NDESC_POS_USE_COM  = 0
YBLD_DESC%NDESC_POS_USE_HAC  = 0
YBLD_DESC%NDESC_POS_USE_HAI  = 0
YBLD_DESC%NDESC_POS_USE_IND  = 0
YBLD_DESC%NDESC_POS_USE_LNC  = 0
YBLD_DESC%NDESC_POS_USE_REL  = 0
YBLD_DESC%NDESC_POS_USE_SAN  = 0
YBLD_DESC%NDESC_POS_USE_ENS  = 0
YBLD_DESC%NDESC_POS_USE_SER  = 0
YBLD_DESC%NDESC_POS_USE_SPO  = 0
YBLD_DESC%NDESC_POS_USE_TER  = 0
YBLD_DESC%NDESC_POS_AGE_P1   = 0
YBLD_DESC%NDESC_POS_AGE_P2   = 0
YBLD_DESC%NDESC_POS_AGE_P3   = 0
YBLD_DESC%NDESC_POS_AGE_P4   = 0
YBLD_DESC%NDESC_POS_AGE_P5   = 0
YBLD_DESC%NDESC_POS_AGE_P6   = 0
YBLD_DESC%NDESC_POS_AGE_P7   = 0
YBLD_DESC%NDESC_POS_PX_DEFAULT  = 0
YBLD_DESC%NDESC_POS_HAI_FORTCRE = 0
YBLD_DESC%NDESC_POS_HAI_FAIBCRE = 0
YBLD_DESC%NDESC_POS_HAC_FORTCRE = 0
YBLD_DESC%NDESC_POS_HAC_FAIBCRE = 0
!
IF (LHOOK) CALL DR_HOOK("MODD_BLD_DESCRIPTION_n:BLD_DESC_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE BLD_DESC_INIT
!
!
END MODULE MODD_BLD_DESCRIPTION_n
