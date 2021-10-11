!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB_PAR (DTCO, UG, U, USS, BDD, DTT, TOP, KDIM, &
                              HPROGRAM)
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
!!    V. Masson      08/2013: adds solar panels
!!    V. Masson      10/2013: adds residential fraction
!!    M. Goret       03/2017: add NB_POP, SFCO2_RD and DELTA_LEGAL_TIME  
!!    M. Goret       03/2017: change the nameliste name in greenroof error message
!!    M. Goret       04/2017: add TIME_OF_CHANGE
!!    M. Goret       04/2017: suppress EFF_HEAT
!!    M. Goret       05/2017: add traffic cycle
!!    M. Goret       07/2017: do test on XPAR_GARDEN only when a value is provided
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_CSTS
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
USE MODD_DATA_TEB_n, ONLY : DATA_TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF, NUNDEF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_INI_VAR_FROM_DATA_0D
USE MODI_INI_VAR_FROM_DATA
USE MODI_TEST_NAM_VAR_SURF
USE MODI_READ_CSVDATA_ARCHI_TEB
USE MODI_READ_CSVDATA_COMPO_TEB

USE MODI_BLDCODE
USE MODI_IS_A_REAL_DATE
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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(BLD_DESC_t), INTENT(INOUT) :: BDD
TYPE(DATA_TEB_t), INTENT(INOUT) :: DTT
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
INTEGER, INTENT(IN) :: KDIM
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
REAL, DIMENSION(KDIM) :: ZWORK
LOGICAL               :: GWORK
LOGICAL               :: GTEST
REAL                  :: ZUNIF     ! temporary variable
INTEGER               :: JJ        ! Loop index
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER, PARAMETER :: NROOF_MAX  = 9
INTEGER, PARAMETER :: NWALL_MAX  = 9
INTEGER            :: NPAR_ROOF_LAYER ! number of roof layers
INTEGER            :: NPAR_WALL_LAYER ! number of wall layers
!
! Geometric Parameters:
!
INTEGER                                 :: NUNIF_BLDTYPE
 CHARACTER(LEN=28)                       :: CFNAM_BLDTYPE
 CHARACTER(LEN=6)                        :: CFTYP_BLDTYPE
INTEGER                                 :: NUNIF_IND_BLD_AGE
 CHARACTER(LEN=28)                       :: CFNAM_IND_BLD_AGE
 CHARACTER(LEN=6)                        :: CFTYP_IND_BLD_AGE
INTEGER                                 :: NUNIF_COL_BLD_AGE
 CHARACTER(LEN=28)                       :: CFNAM_COL_BLD_AGE
 CHARACTER(LEN=6)                        :: CFTYP_COL_BLD_AGE
 CHARACTER(LEN=28)                       :: CCSVFILEARCHI
 CHARACTER(LEN=28)                       :: CCSVFILECOMPO
INTEGER                                 :: NUNIF_USETYPE
 CHARACTER(LEN=28)                       :: CFNAM_USETYPE
 CHARACTER(LEN=6)                        :: CFTYP_USETYPE
INTEGER                                 :: NUNIF_P1TERRITORY
 CHARACTER(LEN=28)                       :: CFNAM_P1TERRITORY
 CHARACTER(LEN=6)                        :: CFTYP_P1TERRITORY
INTEGER                                 :: NUNIF_PXTERRITORY
 CHARACTER(LEN=28)                       :: CFNAM_PXTERRITORY
 CHARACTER(LEN=6)                        :: CFTYP_PXTERRITORY
REAL                                    :: XUNIF_FRACIHS
 CHARACTER(LEN=28)                       :: CFNAM_FRACIHS
 CHARACTER(LEN=6)                        :: CFTYP_FRACIHS
REAL                                    :: XUNIF_FRACCHS
 CHARACTER(LEN=28)                       :: CFNAM_FRACCHS
 CHARACTER(LEN=6)                        :: CFTYP_FRACCHS
REAL                                    :: XUNIF_FRACCOM
 CHARACTER(LEN=28)                       :: CFNAM_FRACCOM
 CHARACTER(LEN=6)                        :: CFTYP_FRACCOM
REAL                                    :: XUNIF_FRACTER
 CHARACTER(LEN=28)                       :: CFNAM_FRACTER
 CHARACTER(LEN=6)                        :: CFTYP_FRACTER
REAL                                    :: XUNIF_FRACIND
 CHARACTER(LEN=28)                       :: CFNAM_FRACIND
 CHARACTER(LEN=6)                        :: CFTYP_FRACIND
REAL                                    :: XUNIF_FRACNHE
 CHARACTER(LEN=28)                       :: CFNAM_FRACNHE
 CHARACTER(LEN=6)                        :: CFTYP_FRACNHE
REAL                                    :: XUNIF_FRACPAV
 CHARACTER(LEN=28)                       :: CFNAM_FRACPAV
 CHARACTER(LEN=6)                        :: CFTYP_FRACPAV
REAL                                    :: XUNIF_FRACMRI
 CHARACTER(LEN=28)                       :: CFNAM_FRACMRI
 CHARACTER(LEN=6)                        :: CFTYP_FRACMRI
REAL                                    :: XUNIF_FRACHRI
 CHARACTER(LEN=28)                       :: CFNAM_FRACHRI
 CHARACTER(LEN=6)                        :: CFTYP_FRACHRI
REAL                                    :: XUNIF_FRACATB
 CHARACTER(LEN=28)                       :: CFNAM_FRACATB
 CHARACTER(LEN=6)                        :: CFTYP_FRACATB
REAL                                    :: XUNIF_FOEQI_MAIS
 CHARACTER(LEN=28)                       :: CFNAM_FOEQI_MAIS
 CHARACTER(LEN=6)                        :: CFTYP_FOEQI_MAIS
REAL                                    :: XUNIF_FOEQI_APPT
 CHARACTER(LEN=28)                       :: CFNAM_FOEQI_APPT
 CHARACTER(LEN=6)                        :: CFTYP_FOEQI_APPT
REAL                                    :: XUNIF_FAEQI_MAIS
 CHARACTER(LEN=28)                       :: CFNAM_FAEQI_MAIS
 CHARACTER(LEN=6)                        :: CFTYP_FAEQI_MAIS
REAL                                    :: XUNIF_FAEQI_APPT
 CHARACTER(LEN=28)                       :: CFNAM_FAEQI_APPT
 CHARACTER(LEN=6)                        :: CFTYP_FAEQI_APPT
REAL                                    :: XUNIF_CRE_MAIS
 CHARACTER(LEN=28)                       :: CFNAM_CRE_MAIS
 CHARACTER(LEN=6)                        :: CFTYP_CRE_MAIS
REAL                                    :: XUNIF_CRE_APPT
 CHARACTER(LEN=28)                       :: CFNAM_CRE_APPT
 CHARACTER(LEN=6)                        :: CFTYP_CRE_APPT
!
 CHARACTER(LEN=3)                        :: CBLD_ATYPE         ! type of averaging for buildings

!
REAL                                    :: XUNIF_BLD          ! fraction of buildings            (-)
REAL                                    :: XUNIF_ROAD         ! fraction of roads                (-)
REAL                                    :: XUNIF_BLD_HEIGHT   ! buildings height 'h'             (m)
REAL                                    :: XUNIF_WALL_O_HOR   ! wall surf. / hor. surf.          (-)
REAL                                    :: XUNIF_Z0_TOWN      ! roughness length for momentum    (m)
REAL                                    :: XUNIF_GREENROOF    ! fraction of greenroofs on roofs  (-)
REAL                                    :: XUNIF_FRAC_HVEG    ! fraction of high vegetation      (-)
REAL                                    :: XUNIF_FRAC_LVEG    ! fraction of low  vegetation      (-)
REAL                                    :: XUNIF_FRAC_NVEG    ! fraction of no   vegetation      (-)
REAL                                    :: XUNIF_ROAD_DIR     ! road direction (deg from North, clockwise)
 CHARACTER(LEN=28)                       :: CFNAM_BLD          ! file name for BLD 
 CHARACTER(LEN=28)                       :: CFNAM_ROAD         ! file name for ROAD
 CHARACTER(LEN=28)                       :: CFNAM_BLD_HEIGHT   ! file name for BLD_HEIGHT
 CHARACTER(LEN=28)                       :: CFNAM_WALL_O_HOR   ! file name for WALL_O_HOR
 CHARACTER(LEN=28)                       :: CFNAM_Z0_TOWN      ! file name for Z0_TOWN
 CHARACTER(LEN=28)                       :: CFNAM_GREENROOF    ! file name for GREENROOF
 CHARACTER(LEN=28)                       :: CFNAM_FRAC_HVEG    ! file name for FRAC_HVEG
 CHARACTER(LEN=28)                       :: CFNAM_FRAC_LVEG    ! file name for FRAC_LVEG
 CHARACTER(LEN=28)                       :: CFNAM_FRAC_NVEG    ! file name for FRAC_NVEG
 CHARACTER(LEN=28)                       :: CFNAM_ROAD_DIR     ! file name for ROAD_DIR  
 CHARACTER(LEN=6)                        :: CFTYP_BLD          ! file type for BLD 
 CHARACTER(LEN=6)                        :: CFTYP_ROAD         ! file type for ROAD
 CHARACTER(LEN=6)                        :: CFTYP_BLD_HEIGHT   ! file type for BLD_HEIGHT
 CHARACTER(LEN=6)                        :: CFTYP_WALL_O_HOR   ! file type for WALL_O_HOR
 CHARACTER(LEN=6)                        :: CFTYP_Z0_TOWN      ! file type for Z0_TOWN
 CHARACTER(LEN=6)                        :: CFTYP_GREENROOF    ! file type for GREENROOF
 CHARACTER(LEN=6)                        :: CFTYP_FRAC_HVEG    ! file type for FRAC_HVEG
 CHARACTER(LEN=6)                        :: CFTYP_FRAC_LVEG    ! file type for FRAC_LVEG
 CHARACTER(LEN=6)                        :: CFTYP_FRAC_NVEG    ! file type for FRAC_NVEG
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
REAL                                    :: XUNIF_HC_COATING_ROAD  ! road coating heat capacity        (J/K/m3)
REAL                                    :: XUNIF_TC_COATING_ROAD  ! road coating thermal conductivity (W/K/m)
REAL                                    :: XUNIF_D_COATING_ROAD   ! depth of road coating             (m)
REAL                                    :: XUNIF_HC_BASEMENT_ROAD ! road coating heat capacity        (J/K/m3)
REAL                                    :: XUNIF_TC_BASEMENT_ROAD ! road coating thermal conductivity (W/K/m)
 CHARACTER(LEN=28)                       :: CFNAM_HC_COATING_ROAD      ! file name for HC_ROAD   
 CHARACTER(LEN=28)                       :: CFNAM_TC_COATING_ROAD      ! file name for TC_ROAD
 CHARACTER(LEN=28)                       :: CFNAM_D_COATING_ROAD       ! file name for D_ROAD
 CHARACTER(LEN=28)                       :: CFNAM_HC_BASEMENT_ROAD     ! file name for HC_ROAD   
 CHARACTER(LEN=28)                       :: CFNAM_TC_BASEMENT_ROAD     ! file name for TC_ROAD
 CHARACTER(LEN=6)                        :: CFTYP_HC_COATING_ROAD      ! file type for HC_ROAD   
 CHARACTER(LEN=6)                        :: CFTYP_TC_COATING_ROAD      ! file type for TC_ROAD
 CHARACTER(LEN=6)                        :: CFTYP_D_COATING_ROAD       ! file type for D_ROAD
 CHARACTER(LEN=6)                        :: CFTYP_HC_BASEMENT_ROAD     ! file type for HC_ROAD   
 CHARACTER(LEN=6)                        :: CFTYP_TC_BASEMENT_ROAD     ! file type for TC_ROAD
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
!
! Solar panels parameters
!
REAL                                    :: XUNIF_EMIS_PANEL    ! emissivity of solar panel       (-)
REAL                                    :: XUNIF_ALB_PANEL     ! albedo     of solar panel       (-)
REAL                                    :: XUNIF_EFF_PANEL     ! efficiency of solar panel       (-)
REAL                                    :: XUNIF_FRAC_PANEL    ! fraction   of solar panel       (-)
 CHARACTER(LEN=28)                       :: CFNAM_EMIS_PANEL   ! file name for EMIS_PANEL
 CHARACTER(LEN=28)                       :: CFNAM_ALB_PANEL    ! file name for ALB_PANEL
 CHARACTER(LEN=28)                       :: CFNAM_EFF_PANEL    ! file name for EFF_PANEL
 CHARACTER(LEN=28)                       :: CFNAM_FRAC_PANEL   ! file name for FRAC_PANEL
 CHARACTER(LEN=6)                        :: CFTYP_EMIS_PANEL   ! file type for EMIS_PANEL
 CHARACTER(LEN=6)                        :: CFTYP_ALB_PANEL    ! file type for ALB_PANEL
 CHARACTER(LEN=6)                        :: CFTYP_EFF_PANEL    ! file type for EFF_PANEL
 CHARACTER(LEN=6)                        :: CFTYP_FRAC_PANEL   ! file type for FRAC_PANEL

 !
! New variables for CO2 flux calculus
!
REAL                                    :: XUNIF_NB_POP                 ! uniform value for population per square kilometer
CHARACTER(LEN=28)                       :: CFNAM_NB_POP                 ! file name for population per square kilometer
CHARACTER(LEN=6)                        :: CFTYP_NB_POP                 ! file type for population per square kilometer
!
REAL                                    :: XUNIF_SFCO2_RD               ! uniform value for CO2 flux link to traffic (roads) : (kg/m2 of town/s)
CHARACTER(LEN=28)                       :: CFNAM_SFCO2_RD               ! file name for CO2 flux link to traffic (roads) : (kg/m2 of town/s)
CHARACTER(LEN=6)                        :: CFTYP_SFCO2_RD               ! file type for CO2 flux link to traffic (roads) : (kg/m2 of town/s)
!
REAL, DIMENSION(TOP%NTIME_CHANGE+1)              :: XUNIF_DELTA_LEGAL_TIME       ! uniform value for difference between UTC and legal time (in hour)
CHARACTER(LEN=28), DIMENSION(TOP%NTIME_CHANGE+1) :: CFNAM_DELTA_LEGAL_TIME       ! file name for difference between UTC and legal time (in hour)
CHARACTER(LEN=6), DIMENSION(TOP%NTIME_CHANGE+1)  :: CFTYP_DELTA_LEGAL_TIME       ! file type for difference between UTC and legal time (in hour)
!
INTEGER, PARAMETER                      :: NINFO_TIME=4                 ! number of information needed to define the time (year, month, day, second)
REAL, DIMENSION(NINFO_TIME,TOP%NTIME_CHANGE):: XUNIF_TIME_OF_CHANGE         ! date of change of legal time
! traffic cycle
REAL, DIMENSION(NB_MONTH)   :: XPAR_TRAF_MONTHLY  ! monthly cycle of traffic
REAL, DIMENSION(NB_DAY)     :: XPAR_TRAF_DAILY    ! daily cycle of traffic from  monday to sunday
REAL, DIMENSION(NB_HOUR)    :: XPAR_TRAF_HOURLY   ! hourly cycle of traffic from 00h to 23h
! inhabitants density cycle
REAL, DIMENSION(NB_MONTH)   :: XPAR_POP_MONTHLY  ! monthly cycle of inhabitants density
REAL, DIMENSION(NB_DAY)     :: XPAR_POP_DAILY    ! daily cycle of inhabitants density from  monday to sunday
REAL, DIMENSION(NB_HOUR)    :: XPAR_POP_HOURLY   ! hourly cycle of inhabitants density from 00h to 23h
!
INTEGER :: IPOS_USE_HAI
INTEGER :: IPOS_AGE_P1
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!

NAMELIST/NAM_DATA_TEB/      NPAR_ROOF_LAYER, NPAR_WALL_LAYER,&
                              CBLD_ATYPE,                                     &
                              NUNIF_BLDTYPE, CFNAM_BLDTYPE, CFTYP_BLDTYPE,    &
                              NUNIF_IND_BLD_AGE, CFNAM_IND_BLD_AGE, CFTYP_IND_BLD_AGE, &
                              NUNIF_COL_BLD_AGE, CFNAM_COL_BLD_AGE, CFTYP_COL_BLD_AGE, &
                              CCSVFILEARCHI,CCSVFILECOMPO,                    &
                              NUNIF_USETYPE, CFNAM_USETYPE, CFTYP_USETYPE,    &
                              NUNIF_P1TERRITORY, CFNAM_P1TERRITORY, CFTYP_P1TERRITORY, &
                              NUNIF_PXTERRITORY, CFNAM_PXTERRITORY, CFTYP_PXTERRITORY, &
                              XUNIF_FRACIHS, CFNAM_FRACIHS, CFTYP_FRACIHS, &
                              XUNIF_FRACCHS, CFNAM_FRACCHS, CFTYP_FRACCHS, &
                              XUNIF_FRACCOM, CFNAM_FRACCOM, CFTYP_FRACCOM, &
                              XUNIF_FRACTER, CFNAM_FRACTER, CFTYP_FRACTER, &
                              XUNIF_FRACIND, CFNAM_FRACIND, CFTYP_FRACIND, &
                              XUNIF_FRACNHE, CFNAM_FRACNHE, CFTYP_FRACNHE, &                              
                              XUNIF_FRACPAV, CFNAM_FRACPAV, CFTYP_FRACPAV, &
                              XUNIF_FRACMRI, CFNAM_FRACMRI, CFTYP_FRACMRI, &
                              XUNIF_FRACHRI, CFNAM_FRACHRI, CFTYP_FRACHRI, &
                              XUNIF_FRACATB, CFNAM_FRACATB, CFTYP_FRACATB, &
                              XUNIF_FOEQI_MAIS , CFNAM_FOEQI_MAIS , CFTYP_FOEQI_MAIS, &
                              XUNIF_FOEQI_APPT , CFNAM_FOEQI_APPT , CFTYP_FOEQI_APPT, &
                              XUNIF_FAEQI_MAIS , CFNAM_FAEQI_MAIS , CFTYP_FAEQI_MAIS, &
                              XUNIF_FAEQI_APPT , CFNAM_FAEQI_APPT , CFTYP_FAEQI_APPT, &
                              XUNIF_CRE_MAIS, CFNAM_CRE_MAIS, CFTYP_CRE_MAIS, &
                              XUNIF_CRE_APPT, CFNAM_CRE_APPT, CFTYP_CRE_APPT, &
                              XUNIF_ALB_ROOF,                                 &
                              XUNIF_EMIS_ROOF, XUNIF_HC_ROOF, XUNIF_TC_ROOF,  &
                              XUNIF_D_ROOF, XUNIF_ALB_ROAD, XUNIF_EMIS_ROAD,  &
                              XUNIF_HC_COATING_ROAD, XUNIF_TC_COATING_ROAD,   &
                              XUNIF_D_COATING_ROAD,                           &
                              XUNIF_HC_BASEMENT_ROAD, XUNIF_TC_BASEMENT_ROAD, &
                              XUNIF_ALB_WALL, XUNIF_EMIS_WALL, XUNIF_HC_WALL, &
                              XUNIF_TC_WALL, XUNIF_D_WALL,                    &
                              XUNIF_Z0_TOWN, XUNIF_BLD, XUNIF_ROAD,           &
                              XUNIF_BLD_HEIGHT,                               &
                              XUNIF_WALL_O_HOR,                               &
                              XUNIF_H_TRAFFIC, XUNIF_LE_TRAFFIC,              &
                              XUNIF_H_INDUSTRY, XUNIF_LE_INDUSTRY,            &
                              XUNIF_GREENROOF, XUNIF_FRAC_HVEG,               &
                              XUNIF_FRAC_LVEG, XUNIF_FRAC_NVEG,               &
                              XUNIF_ROAD_DIR,                                 &
                              CFNAM_ALB_ROOF,                                 &
                              CFNAM_EMIS_ROOF, CFNAM_HC_ROOF, CFNAM_TC_ROOF,  &
                              CFNAM_D_ROOF, CFNAM_ALB_ROAD, CFNAM_EMIS_ROAD,  &
                              CFNAM_HC_COATING_ROAD, CFNAM_TC_COATING_ROAD,   &
                              CFNAM_D_COATING_ROAD,                           &
                              CFNAM_HC_BASEMENT_ROAD, CFNAM_TC_BASEMENT_ROAD, &
                              CFNAM_ALB_WALL, CFNAM_EMIS_WALL, CFNAM_HC_WALL, &
                              CFNAM_TC_WALL, CFNAM_D_WALL,                    &
                              CFNAM_Z0_TOWN, CFNAM_BLD, CFNAM_ROAD,           &
                              CFNAM_BLD_HEIGHT,                               &
                              CFNAM_WALL_O_HOR,                               &
                              CFNAM_H_TRAFFIC, CFNAM_LE_TRAFFIC,              &
                              CFNAM_H_INDUSTRY, CFNAM_LE_INDUSTRY,            &
                              CFNAM_ROAD_DIR, CFNAM_GREENROOF,                &
                              CFNAM_FRAC_HVEG, CFNAM_FRAC_LVEG,               &
                              CFNAM_FRAC_NVEG,                                &
                              CFTYP_ALB_ROOF,                                 &
                              CFTYP_EMIS_ROOF, CFTYP_HC_ROOF, CFTYP_TC_ROOF,  &
                              CFTYP_D_ROOF, CFTYP_ALB_ROAD, CFTYP_EMIS_ROAD,  &
                              CFTYP_HC_COATING_ROAD, CFTYP_TC_COATING_ROAD,   &
                              CFTYP_D_COATING_ROAD,                           &
                              CFTYP_HC_BASEMENT_ROAD, CFTYP_TC_BASEMENT_ROAD, &
                              CFTYP_ALB_WALL, CFTYP_EMIS_WALL, CFTYP_HC_WALL, &
                              CFTYP_TC_WALL, CFTYP_D_WALL,                    &
                              CFTYP_Z0_TOWN, CFTYP_BLD, CFTYP_ROAD,           &
                              CFTYP_BLD_HEIGHT,                               &
                              CFTYP_WALL_O_HOR,                               &
                              CFTYP_H_TRAFFIC, CFTYP_LE_TRAFFIC,              &
                              CFTYP_H_INDUSTRY, CFTYP_LE_INDUSTRY,            &
                              CFTYP_ROAD_DIR, CFTYP_GREENROOF,                &
                              CFTYP_FRAC_HVEG, CFTYP_FRAC_LVEG,               &
                              CFTYP_FRAC_NVEG,                                &
                              XUNIF_ROUGH_ROOF, CFNAM_ROUGH_ROOF, CFTYP_ROUGH_ROOF, &
                              XUNIF_ROUGH_WALL, CFNAM_ROUGH_WALL, CFTYP_ROUGH_WALL, &
                              XUNIF_EMIS_PANEL, CFNAM_EMIS_PANEL, CFTYP_EMIS_PANEL, &
                              XUNIF_ALB_PANEL,  CFNAM_ALB_PANEL,  CFTYP_ALB_PANEL,  &
                              XUNIF_EFF_PANEL,  CFNAM_EFF_PANEL,  CFTYP_EFF_PANEL,  &
                              XUNIF_FRAC_PANEL, CFNAM_FRAC_PANEL, CFTYP_FRAC_PANEL,  &
                              XUNIF_NB_POP, CFNAM_NB_POP, CFTYP_NB_POP,              &
                              XUNIF_SFCO2_RD, CFNAM_SFCO2_RD, CFTYP_SFCO2_RD,        &
                              XUNIF_DELTA_LEGAL_TIME, CFNAM_DELTA_LEGAL_TIME,        &
                              CFTYP_DELTA_LEGAL_TIME, XUNIF_TIME_OF_CHANGE,          &
                              XPAR_TRAF_MONTHLY,XPAR_TRAF_DAILY,XPAR_TRAF_HOURLY,    &
                              XPAR_POP_MONTHLY,XPAR_POP_DAILY,XPAR_POP_HOURLY
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK)   CALL DR_HOOK('PGD_TEB_PAR',0,ZHOOK_HANDLE)
NPAR_ROOF_LAYER=0
NPAR_WALL_LAYER=0
CBLD_ATYPE ='MAJ'
NUNIF_BLDTYPE           = NUNDEF
NUNIF_IND_BLD_AGE       = NUNDEF
NUNIF_COL_BLD_AGE       = NUNDEF
NUNIF_USETYPE           = NUNDEF
NUNIF_P1TERRITORY       = NUNDEF
NUNIF_PXTERRITORY       = NUNDEF
XUNIF_FRACIHS           = XUNDEF
XUNIF_FRACCHS           = XUNDEF
XUNIF_FRACCOM           = XUNDEF
XUNIF_FRACTER           = XUNDEF
XUNIF_FRACIND           = XUNDEF
XUNIF_FRACNHE           = XUNDEF
XUNIF_FRACPAV           = XUNDEF
XUNIF_FRACMRI           = XUNDEF
XUNIF_FRACHRI           = XUNDEF
XUNIF_FRACATB           = XUNDEF
XUNIF_FOEQI_MAIS        = XUNDEF
XUNIF_FOEQI_APPT        = XUNDEF
XUNIF_FAEQI_MAIS        = XUNDEF
XUNIF_FAEQI_APPT        = XUNDEF
XUNIF_CRE_MAIS          = XUNDEF
XUNIF_CRE_APPT          = XUNDEF
XUNIF_BLD               = XUNDEF
XUNIF_ROAD              = XUNDEF
XUNIF_BLD_HEIGHT        = XUNDEF
XUNIF_WALL_O_HOR        = XUNDEF
XUNIF_Z0_TOWN           = XUNDEF
XUNIF_ALB_ROOF          = XUNDEF
XUNIF_EMIS_ROOF         = XUNDEF
XUNIF_HC_ROOF           = XUNDEF
XUNIF_TC_ROOF           = XUNDEF
XUNIF_D_ROOF            = XUNDEF
XUNIF_ALB_ROAD          = XUNDEF
XUNIF_EMIS_ROAD         = XUNDEF
XUNIF_HC_COATING_ROAD   = XUNDEF
XUNIF_TC_COATING_ROAD   = XUNDEF
XUNIF_D_COATING_ROAD    = XUNDEF
XUNIF_HC_BASEMENT_ROAD  = XUNDEF
XUNIF_TC_BASEMENT_ROAD  = XUNDEF
XUNIF_ALB_WALL          = XUNDEF
XUNIF_EMIS_WALL         = XUNDEF
XUNIF_HC_WALL           = XUNDEF
XUNIF_TC_WALL           = XUNDEF
XUNIF_D_WALL            = XUNDEF
XUNIF_H_TRAFFIC         = XUNDEF
XUNIF_LE_TRAFFIC        = XUNDEF
XUNIF_H_INDUSTRY        = XUNDEF
XUNIF_LE_INDUSTRY       = XUNDEF
XUNIF_GREENROOF         = XUNDEF
XUNIF_FRAC_HVEG         = XUNDEF
XUNIF_FRAC_LVEG         = XUNDEF
XUNIF_FRAC_NVEG         = XUNDEF
XUNIF_ROAD_DIR          = XUNDEF
XUNIF_ROUGH_ROOF        = XUNDEF
XUNIF_ROUGH_WALL        = XUNDEF
XUNIF_EMIS_PANEL        = XUNDEF
XUNIF_ALB_PANEL         = XUNDEF
XUNIF_EFF_PANEL         = XUNDEF
XUNIF_FRAC_PANEL        = XUNDEF
XUNIF_NB_POP            = XUNDEF
XUNIF_SFCO2_RD          = XUNDEF
XUNIF_DELTA_LEGAL_TIME  = XUNDEF
XUNIF_TIME_OF_CHANGE    = XUNDEF
XPAR_TRAF_MONTHLY       = XUNDEF
XPAR_TRAF_DAILY         = XUNDEF
XPAR_TRAF_HOURLY        = XUNDEF
XPAR_POP_MONTHLY        = XUNDEF
XPAR_POP_DAILY          = XUNDEF
XPAR_POP_HOURLY         = XUNDEF
!
CFNAM_BLDTYPE         = '                            '
CFNAM_IND_BLD_AGE     = '                            '
CFNAM_COL_BLD_AGE     = '                            '
CFNAM_USETYPE         = '                            '
CFNAM_P1TERRITORY     = '                            '
CFNAM_PXTERRITORY     = '                            '
CFNAM_FRACIHS         = '                            '
CFNAM_FRACCHS         = '                            '
CFNAM_FRACCOM         = '                            '
CFNAM_FRACTER         = '                            '
CFNAM_FRACIND         = '                            '
CFNAM_FRACNHE         = '                            '
CFNAM_FRACPAV         = '                            '
CFNAM_FRACMRI         = '                            '
CFNAM_FRACHRI         = '                            '
CFNAM_FRACATB         = '                            '
CFNAM_FOEQI_MAIS      = '                            '
CFNAM_FOEQI_APPT      = '                            '
CFNAM_FAEQI_MAIS      = '                            '
CFNAM_FAEQI_APPT      = '                            '
CFNAM_CRE_MAIS        = '                            '
CFNAM_CRE_APPT        = '                            '
CCSVFILEARCHI         = '                            '
CCSVFILECOMPO         = '                            '
CFNAM_BLD             = '                            '
CFNAM_ROAD            = '                            '
CFNAM_BLD_HEIGHT      = '                            '
CFNAM_WALL_O_HOR      = '                            '
CFNAM_Z0_TOWN         = '                            '
CFNAM_ALB_ROOF (:)    = '                            '
CFNAM_EMIS_ROOF(:)    = '                            '
CFNAM_HC_ROOF  (:)    = '                            '
CFNAM_TC_ROOF  (:)    = '                            '
CFNAM_D_ROOF   (:)    = '                            '
CFNAM_ROUGH_ROOF(:)   = '                            '
CFNAM_ROUGH_WALL(:)   = '                            '
CFNAM_ALB_ROAD (:)    = '                            '
CFNAM_EMIS_ROAD(:)    = '                            '
CFNAM_HC_COATING_ROAD  (:)    = '                            '
CFNAM_TC_COATING_ROAD  (:)    = '                            '
CFNAM_D_COATING_ROAD   (:)    = '                            '
CFNAM_HC_BASEMENT_ROAD (:)    = '                            '
CFNAM_TC_BASEMENT_ROAD (:)    = '                            '
CFNAM_ALB_WALL (:)    = '                            '
CFNAM_EMIS_WALL(:)    = '                            '
CFNAM_HC_WALL  (:)    = '                            '
CFNAM_TC_WALL  (:)    = '                            '
CFNAM_D_WALL   (:)    = '                            '

CFNAM_H_TRAFFIC       = '                            '
CFNAM_LE_TRAFFIC      = '                            '
CFNAM_H_INDUSTRY      = '                            '
CFNAM_LE_INDUSTRY     = '                            '

CFNAM_GREENROOF       = '                            '
CFNAM_FRAC_HVEG       = '                            '
CFNAM_FRAC_LVEG       = '                            '
CFNAM_FRAC_NVEG       = '                            '
CFNAM_ROAD_DIR        = '                            '

CFNAM_EMIS_PANEL      = '                            '
CFNAM_ALB_PANEL       = '                            '
CFNAM_EFF_PANEL       = '                            '
CFNAM_FRAC_PANEL      = '                            '
CFNAM_NB_POP          = '                            '
CFNAM_SFCO2_RD        = '                            '
CFNAM_DELTA_LEGAL_TIME= '                            '
!
CFTYP_BLDTYPE         = '      '
CFTYP_IND_BLD_AGE     = '      '
CFTYP_COL_BLD_AGE     = '      '
CFTYP_USETYPE         = '      '
CFTYP_P1TERRITORY     = '      '
CFTYP_PXTERRITORY     = '      '
CFTYP_FRACIHS         = '      '
CFTYP_FRACCHS         = '      '
CFTYP_FRACCOM         = '      '
CFTYP_FRACTER         = '      '
CFTYP_FRACIND         = '      '
CFTYP_FRACNHE         = '      '
CFTYP_FRACPAV         = '      '
CFTYP_FRACMRI         = '      '
CFTYP_FRACHRI         = '      '
CFTYP_FRACATB         = '      '
CFTYP_FOEQI_MAIS      = '      '
CFTYP_FOEQI_APPT      = '      '
CFTYP_FAEQI_MAIS      = '      '
CFTYP_FAEQI_APPT      = '      '
CFTYP_CRE_MAIS        = '      '
CFTYP_CRE_APPT        = '      '
CFTYP_BLD             = '      '
CFTYP_ROAD            = '      '
CFTYP_BLD_HEIGHT      = '      '
CFTYP_WALL_O_HOR      = '      '
CFTYP_Z0_TOWN         = '      '
CFTYP_ALB_ROOF(:)     = '      '
CFTYP_EMIS_ROOF(:)    = '      '
CFTYP_HC_ROOF(:)      = '      '
CFTYP_TC_ROOF(:)      = '      '
CFTYP_D_ROOF(:)       = '      '
CFTYP_ROUGH_ROOF(:)   = '      '
CFTYP_ROUGH_WALL(:)   = '      '
CFTYP_ALB_ROAD(:)     = '      '
CFTYP_EMIS_ROAD(:)    = '      '
CFTYP_HC_COATING_ROAD(:) = '      '
CFTYP_TC_COATING_ROAD(:) = '      '
CFTYP_D_COATING_ROAD (:) = '      '
CFTYP_HC_BASEMENT_ROAD(:)= '      '
CFTYP_TC_BASEMENT_ROAD(:)= '      '
CFTYP_ALB_WALL(:)     = '      '
CFTYP_EMIS_WALL(:)    = '      '
CFTYP_HC_WALL(:)      = '      '
CFTYP_TC_WALL(:)      = '      '
CFTYP_D_WALL(:)       = '      '
CFTYP_H_TRAFFIC       = '      '
CFTYP_LE_TRAFFIC      = '      '
CFTYP_H_INDUSTRY      = '      '
CFTYP_LE_INDUSTRY     = '      '
CFTYP_GREENROOF       = '      '
CFTYP_FRAC_HVEG       = '      '
CFTYP_FRAC_LVEG       = '      '
CFTYP_FRAC_NVEG       = '      '
CFTYP_ROAD_DIR        = '      '
!
CFTYP_EMIS_PANEL      = '      '
CFTYP_ALB_PANEL       = '      '
CFTYP_EFF_PANEL       = '      '
CFTYP_FRAC_PANEL      = '      '
CFTYP_NB_POP          = '      '
CFTYP_SFCO2_RD        = '      '
CFTYP_DELTA_LEGAL_TIME= '      '
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
DTT%NPAR_ROOF_LAYER = NPAR_ROOF_LAYER
DTT%NPAR_WALL_LAYER = NPAR_WALL_LAYER
!
TOP%CBLD_ATYPE = CBLD_ATYPE
!-------------------------------------------------------------------------------
!
!* coherence check
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
IF (NROOF_MAX < NPAR_ROOF_LAYER) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_teb_par.f90 routine :      '
  WRITE(ILUOUT,*) 'The maximum number of ROOF LAYER             '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NPAR_ROOF_LAYER
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_TEB_PAR: MAXIMUM NUMBER OF NROOF_MAX MUST BE INCREASED')
ENDIF
!-------------------------------------------------------------------------------
IF (NWALL_MAX < NPAR_WALL_LAYER) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_teb_par.f90 routine :      '
  WRITE(ILUOUT,*) 'The maximum number of WALL LAYER             '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NPAR_WALL_LAYER
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_TEB_PAR: MAXIMUM NUMBER OF NWALL_MAX MUST BE INCREASED')
ENDIF
!
!--------------------------------------------------------------------------------
! default values are replaced if needed
!
IF (XPAR_TRAF_MONTHLY(1)/=XUNDEF)  TOP%XTRAF_MONTHLY = XPAR_TRAF_MONTHLY
IF (XPAR_TRAF_DAILY(1)  /=XUNDEF)  TOP%XTRAF_DAILY   = XPAR_TRAF_DAILY
IF (XPAR_TRAF_HOURLY(1) /=XUNDEF)  TOP%XTRAF_HOURLY  = XPAR_TRAF_HOURLY
!
!check if traffic cycles are well normalized
!
IF (ABS(SUM(TOP%XTRAF_MONTHLY)- REAL(NB_MONTH))>1E-5) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'The monthly traffic cycle is not well normalised,     '
  WRITE(ILUOUT,*) 'or not enough values are provided.                     '
  WRITE(ILUOUT,*) 'it must contains', NB_MONTH, 'values and the sum'
  WRITE(ILUOUT,*) 'of all the coefficients must be', NB_MONTH
  WRITE(ILUOUT,*) 'the actual cycle is:', TOP%XTRAF_MONTHLY
  WRITE(ILUOUT,*) 'the sum is :', SUM(TOP%XTRAF_MONTHLY)
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL FLUSH(ILUOUT)
  CALL ABOR1_SFX('PGD_TEB_PAR: WRONG MONTHLY TRAFFIC CYCLE')
ENDIF
IF (ABS(SUM(TOP%XTRAF_DAILY) - REAL(NB_DAY))>1E-5) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'The daily traffic cycle is not well normalised,     '
  WRITE(ILUOUT,*) 'or not enough values are provided.                     '
  WRITE(ILUOUT,*) 'it must contains', NB_DAY, 'values and the sum'
  WRITE(ILUOUT,*) 'of all the coefficients must be', NB_DAY
  WRITE(ILUOUT,*) 'the actual cycle is:', TOP%XTRAF_DAILY
  WRITE(ILUOUT,*) 'the sum is :', SUM(TOP%XTRAF_DAILY)
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL FLUSH(ILUOUT)
  CALL ABOR1_SFX('PGD_TEB_PAR: WRONG DAILY TRAFFIC CYCLE')
ENDIF
IF (ABS(SUM(TOP%XTRAF_HOURLY)- REAL(NB_HOUR))>1E-5) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'The hourly traffic cycle is not well normalised,     '
  WRITE(ILUOUT,*) 'or not enough values are provided.                     '
  WRITE(ILUOUT,*) 'it must contains', NB_HOUR, 'values and the sum'
  WRITE(ILUOUT,*) 'of all the coefficients must be', NB_HOUR
  WRITE(ILUOUT,*) 'the actual cycle is:', TOP%XTRAF_HOURLY
  WRITE(ILUOUT,*) 'the sum is :', SUM(TOP%XTRAF_HOURLY)
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL FLUSH(ILUOUT)
  CALL ABOR1_SFX('PGD_TEB_PAR: WRONG HOURLY TRAFFIC CYCLE')
ENDIF
!
IF (XPAR_POP_MONTHLY(1)/=XUNDEF)  TOP%XPOP_MONTHLY = XPAR_POP_MONTHLY
IF (XPAR_POP_DAILY(1)  /=XUNDEF)  TOP%XPOP_DAILY   = XPAR_POP_DAILY
IF (XPAR_POP_HOURLY(1) /=XUNDEF)  TOP%XPOP_HOURLY  = XPAR_POP_HOURLY
!
!check if inhabitants density cycles are well normalized
!
IF (ABS(SUM(TOP%XPOP_MONTHLY)- REAL(NB_MONTH))>1E-5) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'The monthly traffic cycle is not well normalised,     '
  WRITE(ILUOUT,*) 'or not enough values are provided.                     '
  WRITE(ILUOUT,*) 'it must contains', NB_MONTH, 'values and the sum'
  WRITE(ILUOUT,*) 'of all the coefficients must be', NB_MONTH
  WRITE(ILUOUT,*) 'the actual cycle is:', TOP%XPOP_MONTHLY
  WRITE(ILUOUT,*) 'the sum is :', SUM(TOP%XPOP_MONTHLY)
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL FLUSH(ILUOUT)
  CALL ABOR1_SFX('PGD_TEB_PAR: WRONG MONTHLY INHABITANTS DENSITY CYCLE')
ENDIF
IF (ABS(SUM(TOP%XPOP_DAILY) - REAL(NB_DAY))>1E-5) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'The daily traffic cycle is not well normalised,     '
  WRITE(ILUOUT,*) 'or not enough values are provided.                     '
  WRITE(ILUOUT,*) 'it must contains', NB_DAY, 'values and the sum'
  WRITE(ILUOUT,*) 'of all the coefficients must be', NB_DAY
  WRITE(ILUOUT,*) 'the actual cycle is:', TOP%XPOP_DAILY
  WRITE(ILUOUT,*) 'the sum is :', SUM(TOP%XPOP_DAILY)
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL FLUSH(ILUOUT)
  CALL ABOR1_SFX('PGD_TEB_PAR: WRONG DAILY INHABITANTS DENSITY CYCLE')
ENDIF
IF (ABS(SUM(TOP%XPOP_HOURLY)- REAL(NB_HOUR))>1E-5) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'The hourly traffic cycle is not well normalised,     '
  WRITE(ILUOUT,*) 'or not enough values are provided.                     '
  WRITE(ILUOUT,*) 'it must contains', NB_HOUR, 'values and the sum'
  WRITE(ILUOUT,*) 'of all the coefficients must be', NB_HOUR
  WRITE(ILUOUT,*) 'the actual cycle is:', TOP%XPOP_HOURLY
  WRITE(ILUOUT,*) 'the sum is :', SUM(TOP%XPOP_HOURLY)
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL FLUSH(ILUOUT)
  CALL ABOR1_SFX('PGD_TEB_PAR: WRONG HOURLY INHABITANTS DENSITY CYCLE')
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    3.      user defined fields are prescribed
!             ----------------------------------
!
!-------------------------------------------------------------------------------
!
!* coverage fractions : buildings, roads, low, high and not vegetated fractions
!
! building fraction
!
ALLOCATE(DTT%XPAR_BLD         (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','BLD        ','TWN', CFNAM_BLD,CFTYP_BLD,XUNIF_BLD,DTT%XPAR_BLD,DTT%LDATA_BLD )
!
IF (.NOT.DTT%LDATA_BLD) DEALLOCATE(DTT%XPAR_BLD)
!
! road fraction
!
ALLOCATE(DTT%XPAR_ROAD      (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                          HPROGRAM,'ARI','ROAD       ','TWN',CFNAM_ROAD      ,CFTYP_ROAD      ,XUNIF_ROAD      ,&
                          DTT%XPAR_ROAD     , GWORK      )
IF (.NOT. GWORK) DEALLOCATE(DTT%XPAR_ROAD)
!
IF (.NOT.DTT%LDATA_BLD .AND. GWORK) THEN
  WRITE(ILUOUT,*) 'Building fraction is not specified in NAM_DATA_TEB, but road fraction is.'
  WRITE(ILUOUT,*) 'Either ALL or NONE of the 5 fractions (BLD, ROAD, HVEG, LVEG, NVEG) can be specified.'
  CALL ABOR1_SFX('PGD_TEB_PAR: INCOHERENCE BETWEEN BLD, ROAD, LOW and NO URBAN VEGETATION FRACTIONS')
END IF
IF (DTT%LDATA_BLD .AND. .NOT. GWORK) THEN
  WRITE(ILUOUT,*) 'Building fraction is specified in NAM_DATA_TEB, but not road fraction.'
  WRITE(ILUOUT,*) 'Either ALL or NONE of the 5 fractions (BLD, ROAD, HVEG, LVEG, NVEG) can be specified.'
  CALL ABOR1_SFX('PGD_TEB_PAR: INCOHERENCE BETWEEN BLD, ROAD, LOW and NO URBAN VEGETATION FRACTIONS')
END IF
!
IF (TOP%LGARDEN) THEN
!
  ALLOCATE(DTT%XPAR_FRAC_HVEG      (KDIM))
  CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                            HPROGRAM,'ARI','FRAC_HVEG  ','TWN',CFNAM_FRAC_HVEG ,CFTYP_FRAC_HVEG ,XUNIF_FRAC_HVEG ,&
                            DTT%XPAR_FRAC_HVEG, GWORK)
  IF (.NOT. GWORK) DEALLOCATE(DTT%XPAR_FRAC_HVEG)

IF (.NOT.DTT%LDATA_BLD .AND. GWORK) THEN
  WRITE(ILUOUT,*) 'Building fraction is not specified in NAM_DATA_TEB, but high vegetation fraction is.'
  WRITE(ILUOUT,*) 'Either ALL or NONE of the 5 fractions (BLD, ROAD, HVEG, LVEG, NVEG) can be specified.'
  CALL ABOR1_SFX('PGD_TEB_PAR: INCOHERENCE BETWEEN BLD, ROAD, LOW and NO URBAN VEGETATION FRACTIONS')
END IF
IF (DTT%LDATA_BLD .AND. .NOT. GWORK) THEN
  WRITE(ILUOUT,*) 'Building fraction is specified in NAM_DATA_TEB, but not high vegetation fraction.'
  WRITE(ILUOUT,*) 'Either ALL or NONE of the 5 fractions (BLD, ROAD, HVEG, LVEG, NVEG) can be specified.'
  CALL ABOR1_SFX('PGD_TEB_PAR: INCOHERENCE BETWEEN BLD, ROAD, LOW and NO URBAN VEGETATION FRACTIONS')
END IF
!
  ALLOCATE(DTT%XPAR_FRAC_LVEG      (KDIM))
  CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                            HPROGRAM,'ARI','FRAC_LVEG  ','TWN',CFNAM_FRAC_LVEG ,CFTYP_FRAC_LVEG ,XUNIF_FRAC_LVEG ,&
                            DTT%XPAR_FRAC_LVEG, GWORK )
  IF (.NOT. GWORK) DEALLOCATE(DTT%XPAR_FRAC_LVEG)
!
IF (.NOT.DTT%LDATA_BLD .AND. GWORK) THEN
  WRITE(ILUOUT,*) 'Building fraction is not specified in NAM_DATA_TEB, but low vegetation fraction is.'
  WRITE(ILUOUT,*) 'Either ALL or NONE of the 5 fractions (BLD, ROAD, HVEG, LVEG, NVEG) can be specified.'
  CALL ABOR1_SFX('PGD_TEB_PAR: INCOHERENCE BETWEEN BLD, ROAD, LOW and NO URBAN VEGETATION FRACTIONS')
END IF
IF (DTT%LDATA_BLD .AND. .NOT. GWORK) THEN
  WRITE(ILUOUT,*) 'Building fraction is specified in NAM_DATA_TEB, but not low vegetation fraction.'
  WRITE(ILUOUT,*) 'Either ALL or NONE of the 5 fractions (BLD, ROAD, HVEG, LVEG, NVEG) can be specified.'
  CALL ABOR1_SFX('PGD_TEB_PAR: INCOHERENCE BETWEEN BLD, ROAD, LOW and NO URBAN VEGETATION FRACTIONS')
END IF

  ALLOCATE(DTT%XPAR_FRAC_NVEG      (KDIM))
  CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                            HPROGRAM,'ARI','FRAC_NVEG  ','TWN',CFNAM_FRAC_NVEG ,CFTYP_FRAC_NVEG ,XUNIF_FRAC_NVEG ,&
                            DTT%XPAR_FRAC_NVEG, GWORK )
  IF (.NOT. GWORK) DEALLOCATE(DTT%XPAR_FRAC_NVEG)
  !
IF (.NOT.DTT%LDATA_BLD .AND. GWORK) THEN
  WRITE(ILUOUT,*) 'Building fraction is not specified in NAM_DATA_TEB, but no vegetation fraction is.'
  WRITE(ILUOUT,*) 'Either ALL or NONE of the 5 fractions (BLD, ROAD, HVEG, LVEG, NVEG) can be specified.'
  CALL ABOR1_SFX('PGD_TEB_PAR: INCOHERENCE BETWEEN BLD, ROAD, LOW and NO URBAN VEGETATION FRACTIONS')
END IF
IF (DTT%LDATA_BLD .AND. .NOT. GWORK) THEN
  WRITE(ILUOUT,*) 'Building fraction is specified in NAM_DATA_TEB, but not no vegetation fraction.'
  WRITE(ILUOUT,*) 'Either ALL or NONE of the 5 fractions (BLD, ROAD, HVEG, LVEG, NVEG) can be specified.'
  CALL ABOR1_SFX('PGD_TEB_PAR: INCOHERENCE BETWEEN BLD, ROAD, LOW and NO URBAN VEGETATION FRACTIONS')
END IF
!
ELSE IF (     (XUNIF_FRAC_HVEG/=0. .AND. XUNIF_FRAC_HVEG/=XUNDEF) .OR. LEN_TRIM(CFNAM_FRAC_HVEG)/=0 &
         .OR. (XUNIF_FRAC_LVEG/=0. .AND. XUNIF_FRAC_LVEG/=XUNDEF) .OR. LEN_TRIM(CFNAM_FRAC_LVEG)/=0 &
         .OR. (XUNIF_FRAC_NVEG/=0. .AND. XUNIF_FRAC_NVEG/=XUNDEF) .OR. LEN_TRIM(CFNAM_FRAC_NVEG)/=0 &
        ) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) ' You chose not to include gardens in urban areas : LGARDEN=.FALSE.     '
  WRITE(ILUOUT,*) ' But            '
  IF (XUNIF_FRAC_HVEG/=0. .AND. XUNIF_FRAC_HVEG/=XUNDEF) THEN
    WRITE(ILUOUT,*) ' You also chose a high vegetation fraction that is not zero : XUNIF_FRAC_HVEG=',XUNIF_FRAC_HVEG
  ELSE IF (LEN_TRIM(CFNAM_FRAC_HVEG)/=0) THEN
    WRITE(ILUOUT,*) ' You also chose a high vegetation fraction that is not zero : CFNAM_FRAC_HVEG=',CFNAM_FRAC_HVEG
  END IF
    IF (XUNIF_FRAC_LVEG/=0. .AND. XUNIF_FRAC_LVEG/=XUNDEF) THEN
    WRITE(ILUOUT,*) ' You also chose a low  vegetation fraction that is not zero : XUNIF_FRAC_LVEG=',XUNIF_FRAC_LVEG
  ELSE IF (LEN_TRIM(CFNAM_FRAC_LVEG)/=0) THEN
    WRITE(ILUOUT,*) ' You also chose a low  vegatation fraction that is not zero : CFNAM_FRAC_LVEG=',CFNAM_FRAC_LVEG
  END IF
    IF (XUNIF_FRAC_NVEG/=0. .AND. XUNIF_FRAC_NVEG/=XUNDEF) THEN
    WRITE(ILUOUT,*) ' You also chose a bare soil fraction that is not zero : XUNIF_FRAC_NVEG=',XUNIF_FRAC_NVEG
  ELSE IF (LEN_TRIM(CFNAM_FRAC_NVEG)/=0) THEN
    WRITE(ILUOUT,*) ' You also chose a bare soil fraction that is not zero : CFNAM_FRAC_NVEG=',CFNAM_FRAC_NVEG
  END IF
  WRITE(ILUOUT,*) '- - - - - - - - - - - - - - - - - - - - - - -'
  WRITE(ILUOUT,*) ' Please choose either:'
  WRITE(ILUOUT,*) ' LGARDEN=.TRUE. or set the fractions to zero in namelist PGD_TEB_PAR'
  WRITE(ILUOUT,*) '- - - - - - - - - - - - - - - - - - - - - - -'
  WRITE(ILUOUT,*) ' Beware that in this case, it may be necessary to change the'
  WRITE(ILUOUT,*) ' road fraction if you want to keep the same canyon aspect ratio'
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_TEB_PAR: VEGETATION FRACTIONS and LGARDEN flag not coherent')
ELSE
  ! if garden option is not activated, there is no vegetation fraction of any type
  ALLOCATE(DTT%XPAR_FRAC_HVEG      (KDIM))
  ALLOCATE(DTT%XPAR_FRAC_LVEG      (KDIM))
  ALLOCATE(DTT%XPAR_FRAC_NVEG      (KDIM))
  DTT%XPAR_FRAC_HVEG = 0.
  DTT%XPAR_FRAC_LVEG = 0.
  DTT%XPAR_FRAC_NVEG = 0.
END IF
!
IF (DTT%LDATA_BLD) THEN
   !
   ! Check building fraction
   !
   IF (MAXVAL(DTT%XPAR_BLD).GT.(1.0+XSURF_EPSILON)) CALL ABOR1_SFX("PGD_TEB_PAR: Too high building fraction")
   IF (MINVAL(DTT%XPAR_BLD).LT.    -XSURF_EPSILON ) CALL ABOR1_SFX("PGD_TEB_PAR: Too low  building fraction")
   !
   ! Check road fraction
   !
   IF (MAXVAL(DTT%XPAR_ROAD).GT.(1.0+XSURF_EPSILON)) CALL ABOR1_SFX("PGD_TEB_PAR: Too high road fraction")
   IF (MINVAL(DTT%XPAR_ROAD).LT.    -XSURF_EPSILON ) CALL ABOR1_SFX("PGD_TEB_PAR: Too low  road fraction")
   !
   ! Check vegetation fractions
   !
   IF (MAXVAL(DTT%XPAR_FRAC_HVEG).GT.(1.0+XSURF_EPSILON)) CALL ABOR1_SFX("PGD_TEB_PAR: Too high high vegetation fraction")
   IF (MINVAL(DTT%XPAR_FRAC_HVEG).LT.    -XSURF_EPSILON ) CALL ABOR1_SFX("PGD_TEB_PAR: Too low  high vegetation fraction")
   !
   IF (MAXVAL(DTT%XPAR_FRAC_NVEG).GT.(1.0+XSURF_EPSILON)) CALL ABOR1_SFX("PGD_TEB_PAR: Too high no   vegetation fraction")
   IF (MINVAL(DTT%XPAR_FRAC_NVEG).LT.    -XSURF_EPSILON ) CALL ABOR1_SFX("PGD_TEB_PAR: Too low  no   vegetation fraction")
   !
   IF (MAXVAL(DTT%XPAR_FRAC_LVEG).GT.(1.0+XSURF_EPSILON)) CALL ABOR1_SFX("PGD_TEB_PAR: Too high low  vegetation fraction")
   IF (MINVAL(DTT%XPAR_FRAC_LVEG).LT.    -XSURF_EPSILON ) CALL ABOR1_SFX("PGD_TEB_PAR: Too low  low  vegetation fraction")
   !
   ! Checks coherence between the fractions
   !
     ZWORK = DTT%XPAR_BLD + DTT%XPAR_ROAD + DTT%XPAR_FRAC_LVEG + DTT%XPAR_FRAC_NVEG
     IF (ANY(ABS(ZWORK-1.)>1.E-6)) THEN
        WRITE(ILUOUT,*) 'The sum of fraction building (BLD), road (ROAD), low vegetation (FRAC_LVEG) and no vegetation (FRAC_NVEG)'
        WRITE(ILUOUT,*) 'are not equal to 1. Please check your input data and options in PGD namelists.'
        CALL ABOR1_SFX('PGD_TEB_PAR: INCOHERENCE BETWEEN BLD, ROAD, LOW and NO URBAN VEGETATION FRACTIONS')
     ELSE
        DTT%XPAR_BLD       = DTT%XPAR_BLD       / ZWORK
        DTT%XPAR_ROAD      = DTT%XPAR_ROAD      / ZWORK
        DTT%XPAR_FRAC_LVEG = DTT%XPAR_FRAC_LVEG / ZWORK
        DTT%XPAR_FRAC_NVEG = DTT%XPAR_FRAC_NVEG / ZWORK
        DTT%XPAR_FRAC_HVEG = DTT%XPAR_FRAC_HVEG / ZWORK
     END IF
   !
   ! Checks coherence between high vegetation fraction and canyon fraction
   !
     IF (ANY(DTT%XPAR_FRAC_HVEG>1.-DTT%XPAR_BLD)) THEN
        WRITE(ILUOUT,*) 'The high vegetation fraction is larger than road+low vegetation +no vegetation fractions.'
        WRITE(ILUOUT,*) 'Please check your input data and options in PGD namelists.'
        CALL ABOR1_SFX('PGD_TEB_PAR: INCOHERENCE BETWEEN BLD, ROAD, LOW and NO URBAN VEGETATION FRACTIONS')
     END IF
   !
   ! Checks coherence between high vegetation fraction and other vegetation fractions
   !
   !
   ! Checks coherence between high vegetation fraction and other vegetation fractions
   !
   IF (TOP%CURBTREE/='NONE') THEN
     IF (ANY(DTT%XPAR_FRAC_HVEG>5.*(DTT%XPAR_FRAC_LVEG+DTT%XPAR_FRAC_NVEG))) THEN
        WRITE(ILUOUT,*) '----------------------------------------------------------------------------------------'
        WRITE(ILUOUT,*) 'The URBTREE option is activated '
        WRITE(ILUOUT,*) 'However, there is at least one grind point with high vegetation '
        WRITE(ILUOUT,*) 'but without enough low vegetation and no vegetation fractions.'
        WRITE(ILUOUT,*) 'There is no way for the program simulate trees without any garden soil' 
        WRITE(ILUOUT,*) 'or with too few garden soil available.' 
        WRITE(ILUOUT,*) '==> Please put a bare soil or low vegetation fraction '
        WRITE(ILUOUT,*)  'at least one fifth of high vegetation fraction.' 
        WRITE(ILUOUT,*) 'Please check your input data and options in PGD namelists.'
        CALL ABOR1_SFX('PGD_TEB_PAR: INCOHERENCE BETWEEN HIGH, LOW and NO URBAN VEGETATION FRACTIONS')
     END IF
   END IF
END IF
!-------------------------------------------------------------------------------

!* building's type
ZUNIF = XUNDEF
IF (NUNIF_BLDTYPE/=NUNDEF) ZUNIF=FLOAT(NUNIF_BLDTYPE)
ALLOCATE(DTT%NPAR_BLDTYPE     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,'MAJ','BLDTYPE    ','TWN', &
        CFNAM_BLDTYPE,CFTYP_BLDTYPE,ZUNIF,ZWORK(:),DTT%LDATA_BLDTYPE )
IF (.NOT. DTT%LDATA_BLDTYPE) THEN
  DEALLOCATE(DTT%NPAR_BLDTYPE)
ELSE
  DTT%NPAR_BLDTYPE = NINT(ZWORK)
END IF
!
!* building's age (individual housing)
ZUNIF = XUNDEF
IF (NUNIF_IND_BLD_AGE/=NUNDEF) ZUNIF=FLOAT(NUNIF_IND_BLD_AGE)
ALLOCATE(DTT%NPAR_IND_BLD_AGE (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','IND_BLD_AGE    ','TWN', CFNAM_IND_BLD_AGE,CFTYP_IND_BLD_AGE,ZUNIF,&
        ZWORK(:),DTT%LDATA_IND_BLD_AGE )
IF (.NOT. DTT%LDATA_IND_BLD_AGE) THEN
  DEALLOCATE(DTT%NPAR_IND_BLD_AGE)
ELSE
  DTT%NPAR_IND_BLD_AGE = NINT(ZWORK)
END IF
!
!* building's age (collective housing)
ZUNIF = XUNDEF
IF (NUNIF_COL_BLD_AGE/=NUNDEF) ZUNIF=FLOAT(NUNIF_COL_BLD_AGE)
ALLOCATE(DTT%NPAR_COL_BLD_AGE (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','COL_BLD_AGE    ','TWN', CFNAM_COL_BLD_AGE,CFTYP_COL_BLD_AGE,ZUNIF,&
        ZWORK(:),DTT%LDATA_COL_BLD_AGE )
IF (.NOT. DTT%LDATA_COL_BLD_AGE) THEN
  DEALLOCATE(DTT%NPAR_COL_BLD_AGE)
ELSE
  DTT%NPAR_COL_BLD_AGE = NINT(ZWORK)
END IF
!
!* historical building material territory
ZUNIF = XUNDEF
IF (NUNIF_P1TERRITORY/=NUNDEF) ZUNIF=FLOAT(NUNIF_P1TERRITORY)
ALLOCATE(DTT%NPAR_P1TERRITORY (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','P1TERRITORY  ','TWN', CFNAM_P1TERRITORY,CFTYP_P1TERRITORY,ZUNIF,&
    ZWORK(:),DTT%LDATA_P1TERRITORY )
IF (.NOT. DTT%LDATA_P1TERRITORY) THEN
   DEALLOCATE(DTT%NPAR_P1TERRITORY)
ELSE
   DTT%NPAR_P1TERRITORY = NINT(ZWORK)
END IF
!
!* recent building material territory
ZUNIF = XUNDEF
IF (NUNIF_PXTERRITORY/=NUNDEF) ZUNIF=FLOAT(NUNIF_PXTERRITORY)
ALLOCATE(DTT%NPAR_PXTERRITORY (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','PXTERRITORY  ','TWN', CFNAM_PXTERRITORY,CFTYP_PXTERRITORY,ZUNIF,&
    ZWORK(:),DTT%LDATA_PXTERRITORY )
IF (.NOT. DTT%LDATA_PXTERRITORY) THEN
   DEALLOCATE(DTT%NPAR_PXTERRITORY)
ELSE
   DTT%NPAR_PXTERRITORY = NINT(ZWORK)
END IF
!
!* building's use
ZUNIF = XUNDEF
IF (NUNIF_USETYPE/=NUNDEF) ZUNIF=FLOAT(NUNIF_USETYPE)
ALLOCATE(DTT%NPAR_USETYPE     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','USETYPE    ','TWN', CFNAM_USETYPE,CFTYP_USETYPE,ZUNIF,&
        ZWORK(:),DTT%LDATA_USETYPE )
IF (.NOT. DTT%LDATA_USETYPE) THEN
  DEALLOCATE(DTT%NPAR_USETYPE)
ELSE
  DTT%NPAR_USETYPE = NINT(ZWORK)
END IF
!
IF (DTT%LDATA_BLDTYPE.AND.((.NOT.DTT%LDATA_USETYPE).OR.(.NOT.DTT%LDATA_IND_BLD_AGE).OR.&
       (.NOT.DTT%LDATA_COL_BLD_AGE).OR.((.NOT.DTT%LDATA_P1TERRITORY).OR.(.NOT.DTT%LDATA_PXTERRITORY)))) THEN

  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) ' You chose to define building types :        '
  IF (NUNIF_BLDTYPE/=NUNDEF) THEN
    WRITE(ILUOUT,*) ' NUNIF_BLDTYPE=', NUNIF_BLDTYPE
  ELSE
    WRITE(ILUOUT,*) ' CFNAM_BLDTYPE =',CFNAM_BLDTYPE 
    WRITE(ILUOUT,*) ' CFTYP_BLDTYPE =',CFTYP_BLDTYPE 
  END IF
  WRITE(ILUOUT,*) ' But            '
  WRITE(ILUOUT,*) " You did not chose to define building's age, use, or territories"
  WRITE(ILUOUT,*) '- - - - - - - - - - - - - - - - - - - - - - -'
  WRITE(ILUOUT,*) ' Please define the construction date of the buildings. '
  WRITE(ILUOUT,*) ' To do so, use either :'
  WRITE(ILUOUT,*) ' NUNIF_BLD_AGE   (to have a uniform construction date for all buildings'
  WRITE(ILUOUT,*) ' or CFNAM_BLD_AGE and CFTYP_BLD_AGE (to incorporate spatial data ) '
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX("PGD_TEB_PAR: Building information is missing")
END IF
!
IF (DTT%LDATA_BLDTYPE.OR.DTT%LDATA_IND_BLD_AGE.OR.DTT%LDATA_COL_BLD_AGE.OR. &
    DTT%LDATA_USETYPE.OR.DTT%LDATA_P1TERRITORY.OR.DTT%LDATA_PXTERRITORY) THEN
  !
  ! Read architectural characteristics
  !
  CALL READ_CSVDATA_ARCHI_TEB(BDD,HPROGRAM,CCSVFILEARCHI)
  !
  ! Read behavioural characteristics
  !
  CALL READ_CSVDATA_COMPO_TEB(BDD,HPROGRAM,CCSVFILECOMPO)
  !
ENDIF
!
! Robert: depending on the construction period a switch between the historical
!         and recent construction material territory is made.
!
IF (ASSOCIATED(DTT%NPAR_BLDTYPE)) THEN
   !
   ! Find the position of MApUCE building use corresponding to individual housing
   !
   IPOS_USE_HAI = -9999
   DO JJ=1,BDD%NDESC_USE
     IF (TRIM(BDD%YDESC_USENAME(JJ)).EQ."HABITAT INDIVIDUEL") IPOS_USE_HAI = JJ
   ENDDO
   IF (IPOS_USE_HAI.LE.0) CALL ABOR1_SFX ("Building use not found")
   !
   ! Find the position of the first MApUCE construction period
   !
   IPOS_AGE_P1 = -9999
   DO JJ=1,BDD%NDESC_AGE
     IF (TRIM(BDD%YDESC_AGENAME(JJ)).EQ."P1") IPOS_AGE_P1 = JJ
   ENDDO
   IF (IPOS_AGE_P1.LE.0) CALL ABOR1_SFX ("Building construction period not found")   
   ! 
   ALLOCATE(DTT%NPAR_BLDCODE     (KDIM))
   !
   DO JJ=1,SIZE(DTT%NPAR_USETYPE)
      !
      IF (DTT%NPAR_USETYPE(JJ).NE.IPOS_USE_HAI) THEN
         IF (DTT%NPAR_COL_BLD_AGE(JJ).EQ.IPOS_AGE_P1) THEN
            CALL BLDCODE(DTT%NPAR_BLDTYPE(JJ),DTT%NPAR_COL_BLD_AGE(JJ),DTT%NPAR_USETYPE(JJ), &
                         DTT%NPAR_P1TERRITORY(JJ),DTT%NPAR_BLDCODE(JJ))
         ELSE
            CALL BLDCODE(DTT%NPAR_BLDTYPE(JJ),DTT%NPAR_COL_BLD_AGE(JJ),DTT%NPAR_USETYPE(JJ), &
                         DTT%NPAR_PXTERRITORY(JJ),DTT%NPAR_BLDCODE(JJ))
         ENDIF
      ELSE
         IF (DTT%NPAR_IND_BLD_AGE(JJ).EQ.IPOS_AGE_P1) THEN
            CALL BLDCODE(DTT%NPAR_BLDTYPE(JJ),DTT%NPAR_IND_BLD_AGE(JJ),DTT%NPAR_USETYPE(JJ), &
                         DTT%NPAR_P1TERRITORY(JJ),DTT%NPAR_BLDCODE(JJ))
         ELSE
            CALL BLDCODE(DTT%NPAR_BLDTYPE(JJ),DTT%NPAR_IND_BLD_AGE(JJ),DTT%NPAR_USETYPE(JJ), &
                         DTT%NPAR_PXTERRITORY(JJ),DTT%NPAR_BLDCODE(JJ))
         ENDIF
      ENDIF
      !
   ENDDO
ENDIF
!
!
!* other building parameters
!
ALLOCATE(DTT%XPAR_BLD_HEIGHT  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,'ARI','BLD_HEIGHT ','TWN',&
        CFNAM_BLD_HEIGHT,CFTYP_BLD_HEIGHT,XUNIF_BLD_HEIGHT,&
        DTT%XPAR_BLD_HEIGHT,DTT%LDATA_BLD_HEIGHT)
IF (.NOT.DTT%LDATA_BLD_HEIGHT) DEALLOCATE(DTT%XPAR_BLD_HEIGHT)
!
ALLOCATE(DTT%XPAR_WALL_O_HOR  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,'ARI','WALL_O_HOR ','TWN',&
        CFNAM_WALL_O_HOR,CFTYP_WALL_O_HOR,XUNIF_WALL_O_HOR,DTT%XPAR_WALL_O_HOR,DTT%LDATA_WALL_O_HOR)
IF (.NOT.DTT%LDATA_WALL_O_HOR) DEALLOCATE(DTT%XPAR_WALL_O_HOR)
!
ALLOCATE(DTT%XPAR_Z0_TOWN     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,'CDN','Z0_TOWN    ','TWN',&
        CFNAM_Z0_TOWN,CFTYP_Z0_TOWN,XUNIF_Z0_TOWN,DTT%XPAR_Z0_TOWN,DTT%LDATA_Z0_TOWN)
IF (.NOT.DTT%LDATA_Z0_TOWN) DEALLOCATE(DTT%XPAR_Z0_TOWN)
!
ALLOCATE(DTT%XPAR_FRACIHS     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','FRACIHS    ','TWN',CFNAM_FRACIHS,CFTYP_FRACIHS,XUNIF_FRACIHS,DTT%XPAR_FRACIHS,DTT%LDATA_FRACIHS)
IF (.NOT.DTT%LDATA_FRACIHS) DEALLOCATE(DTT%XPAR_FRACIHS)

ALLOCATE(DTT%XPAR_FRACCHS     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','FRACCHS    ','TWN',CFNAM_FRACCHS,CFTYP_FRACCHS,XUNIF_FRACCHS,DTT%XPAR_FRACCHS,DTT%LDATA_FRACCHS)
IF (.NOT.DTT%LDATA_FRACCHS) DEALLOCATE(DTT%XPAR_FRACCHS)

ALLOCATE(DTT%XPAR_FRACCOM     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','FRACCOM    ','TWN',CFNAM_FRACCOM,CFTYP_FRACCOM,XUNIF_FRACCOM,DTT%XPAR_FRACCOM,DTT%LDATA_FRACCOM)
IF (.NOT.DTT%LDATA_FRACCOM) DEALLOCATE(DTT%XPAR_FRACCOM)

ALLOCATE(DTT%XPAR_FRACTER     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','FRACTER    ','TWN',CFNAM_FRACTER,CFTYP_FRACTER,XUNIF_FRACTER,DTT%XPAR_FRACTER,DTT%LDATA_FRACTER)
IF (.NOT.DTT%LDATA_FRACTER) DEALLOCATE(DTT%XPAR_FRACTER)
!
ALLOCATE(DTT%XPAR_FRACIND     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','FRACIND    ','TWN',CFNAM_FRACIND,CFTYP_FRACIND,XUNIF_FRACIND,DTT%XPAR_FRACIND,DTT%LDATA_FRACIND)
IF (.NOT.DTT%LDATA_FRACIND) DEALLOCATE(DTT%XPAR_FRACIND)

ALLOCATE(DTT%XPAR_FRACNHE     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','FRACNHE    ','TWN',CFNAM_FRACNHE,CFTYP_FRACNHE,XUNIF_FRACNHE,DTT%XPAR_FRACNHE,DTT%LDATA_FRACNHE)
IF (.NOT.DTT%LDATA_FRACNHE) DEALLOCATE(DTT%XPAR_FRACNHE)
!
ALLOCATE(DTT%XPAR_FRACPAV     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','FRACPAV    ','TWN',CFNAM_FRACPAV,CFTYP_FRACPAV,XUNIF_FRACPAV,DTT%XPAR_FRACPAV,DTT%LDATA_FRACPAV)
IF (.NOT.DTT%LDATA_FRACPAV) DEALLOCATE(DTT%XPAR_FRACPAV)
!
ALLOCATE(DTT%XPAR_FRACMRI     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','FRACMRI    ','TWN',CFNAM_FRACMRI,CFTYP_FRACMRI,XUNIF_FRACMRI,DTT%XPAR_FRACMRI,DTT%LDATA_FRACMRI)
IF (.NOT.DTT%LDATA_FRACMRI) DEALLOCATE(DTT%XPAR_FRACMRI)

ALLOCATE(DTT%XPAR_FRACHRI     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','FRACHRI    ','TWN',CFNAM_FRACHRI,CFTYP_FRACHRI,XUNIF_FRACHRI,DTT%XPAR_FRACHRI,DTT%LDATA_FRACHRI)
IF (.NOT.DTT%LDATA_FRACHRI) DEALLOCATE(DTT%XPAR_FRACHRI)
!
ALLOCATE(DTT%XPAR_FRACATB     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','FRACATB    ','TWN',CFNAM_FRACATB,CFTYP_FRACATB,XUNIF_FRACATB,DTT%XPAR_FRACATB,DTT%LDATA_FRACATB)
IF (.NOT.DTT%LDATA_FRACATB) DEALLOCATE(DTT%XPAR_FRACATB)
!
ALLOCATE(DTT%XPAR_FOEQI_MAIS  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','FOEQI_MAIS    ','TWN',CFNAM_FOEQI_MAIS,CFTYP_FOEQI_MAIS,XUNIF_FOEQI_MAIS, &
   DTT%XPAR_FOEQI_MAIS,DTT%LDATA_FOEQI_MAIS)
IF (.NOT.DTT%LDATA_FOEQI_MAIS) DEALLOCATE(DTT%XPAR_FOEQI_MAIS)
!
ALLOCATE(DTT%XPAR_FOEQI_APPT  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','FOEQI_APPT    ','TWN',CFNAM_FOEQI_APPT,CFTYP_FOEQI_APPT,XUNIF_FOEQI_APPT, &
   DTT%XPAR_FOEQI_APPT,DTT%LDATA_FOEQI_APPT)
IF (.NOT.DTT%LDATA_FOEQI_APPT) DEALLOCATE(DTT%XPAR_FOEQI_APPT)
!
ALLOCATE(DTT%XPAR_FAEQI_MAIS  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','IU_MAIS    ','TWN',CFNAM_FAEQI_MAIS,CFTYP_FAEQI_MAIS,XUNIF_FAEQI_MAIS, &
   DTT%XPAR_FAEQI_MAIS,DTT%LDATA_FAEQI_MAIS)
IF (.NOT.DTT%LDATA_FAEQI_MAIS) DEALLOCATE(DTT%XPAR_FAEQI_MAIS)
!
ALLOCATE(DTT%XPAR_FAEQI_APPT  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','IU_APPT    ','TWN',CFNAM_FAEQI_APPT,CFTYP_FAEQI_APPT,XUNIF_FAEQI_APPT, &
   DTT%XPAR_FAEQI_APPT,DTT%LDATA_FAEQI_APPT)
IF (.NOT.DTT%LDATA_FAEQI_APPT) DEALLOCATE(DTT%XPAR_FAEQI_APPT)
! 
ALLOCATE(DTT%XPAR_CRE_MAIS    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','CRE_MAIS    ','TWN',CFNAM_CRE_MAIS,CFTYP_CRE_MAIS,XUNIF_CRE_MAIS, &
     DTT%XPAR_CRE_MAIS,DTT%LDATA_CRE_MAIS)
IF (.NOT.DTT%LDATA_CRE_MAIS) DEALLOCATE(DTT%XPAR_CRE_MAIS)
!
ALLOCATE(DTT%XPAR_CRE_APPT    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','CRE_APPT    ','TWN',CFNAM_CRE_APPT,CFTYP_CRE_APPT,XUNIF_CRE_APPT, &
     DTT%XPAR_CRE_APPT,DTT%LDATA_CRE_APPT)
IF (.NOT.DTT%LDATA_CRE_APPT) DEALLOCATE(DTT%XPAR_CRE_APPT)
!
ALLOCATE(DTT%XPAR_ALB_ROOF    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,CBLD_ATYPE,'ALB_ROOF   ','TWN',&
          CFNAM_ALB_ROOF,CFTYP_ALB_ROOF,XUNIF_ALB_ROOF  ,DTT%XPAR_ALB_ROOF,DTT%LDATA_ALB_ROOF)
IF (.NOT.DTT%LDATA_ALB_ROOF) DEALLOCATE(DTT%XPAR_ALB_ROOF)
!
ALLOCATE(DTT%XPAR_EMIS_ROOF   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,CBLD_ATYPE,'EMIS_ROOF  ','TWN',&
        CFNAM_EMIS_ROOF,CFTYP_EMIS_ROOF,XUNIF_EMIS_ROOF ,DTT%XPAR_EMIS_ROOF,DTT%LDATA_EMIS_ROOF)
IF (.NOT.DTT%LDATA_EMIS_ROOF) DEALLOCATE(DTT%XPAR_EMIS_ROOF)
!
ALLOCATE(DTT%XPAR_HC_ROOF     (KDIM,NPAR_ROOF_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, HPROGRAM,CBLD_ATYPE,'HC_ROOF  ','TWN',&
        CFNAM_HC_ROOF,CFTYP_HC_ROOF, XUNIF_HC_ROOF,DTT%XPAR_HC_ROOF,DTT%LDATA_HC_ROOF ) 
IF (.NOT.DTT%LDATA_HC_ROOF) DEALLOCATE(DTT%XPAR_HC_ROOF)
! 
ALLOCATE(DTT%XPAR_TC_ROOF     (KDIM,NPAR_ROOF_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, HPROGRAM,CBLD_ATYPE,'TC_ROOF  ','TWN',&
        CFNAM_TC_ROOF,CFTYP_TC_ROOF, XUNIF_TC_ROOF ,DTT%XPAR_TC_ROOF, DTT%LDATA_TC_ROOF ) 
IF (.NOT.DTT%LDATA_TC_ROOF) DEALLOCATE(DTT%XPAR_TC_ROOF)
! 
ALLOCATE(DTT%XPAR_D_ROOF      (KDIM,NPAR_ROOF_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, HPROGRAM,CBLD_ATYPE,'D_ROOF   ','TWN',&
        CFNAM_D_ROOF,CFTYP_D_ROOF, XUNIF_D_ROOF  ,DTT%XPAR_D_ROOF , DTT%LDATA_D_ROOF ) 
IF (.NOT.DTT%LDATA_D_ROOF) DEALLOCATE(DTT%XPAR_D_ROOF)
! 
ALLOCATE(DTT%XPAR_ALB_ROAD    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,'ARI','ALB_ROAD   ','TWN',&
        CFNAM_ALB_ROAD  ,CFTYP_ALB_ROAD  ,XUNIF_ALB_ROAD  ,DTT%XPAR_ALB_ROAD, DTT%LDATA_ALB_ROAD  )
IF (.NOT.DTT%LDATA_ALB_ROAD) DEALLOCATE(DTT%XPAR_ALB_ROAD)
!
ALLOCATE(DTT%XPAR_EMIS_ROAD   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,'ARI','EMIS_ROAD  ','TWN',&
        CFNAM_EMIS_ROAD ,CFTYP_EMIS_ROAD ,XUNIF_EMIS_ROAD ,DTT%XPAR_EMIS_ROAD, DTT%LDATA_EMIS_ROAD )
IF (.NOT.DTT%LDATA_EMIS_ROAD) DEALLOCATE(DTT%XPAR_EMIS_ROAD)
!
ALLOCATE(DTT%XPAR_HC_COATING_ROAD     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                        HPROGRAM,CBLD_ATYPE,'HC_COATING','TWN',CFNAM_HC_COATING_ROAD ,CFTYP_HC_COATING_ROAD , &
                   XUNIF_HC_COATING_ROAD ,DTT%XPAR_HC_COATING_ROAD, DTT%LDATA_HC_COATING_ROAD  )  
IF (.NOT.DTT%LDATA_HC_COATING_ROAD) DEALLOCATE(DTT%XPAR_HC_COATING_ROAD)
!
ALLOCATE(DTT%XPAR_HC_BASEMENT_ROAD     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                        HPROGRAM,CBLD_ATYPE,'HC_BASEMENT','TWN',CFNAM_HC_BASEMENT_ROAD ,CFTYP_HC_BASEMENT_ROAD , &
                   XUNIF_HC_BASEMENT_ROAD ,DTT%XPAR_HC_BASEMENT_ROAD, DTT%LDATA_HC_BASEMENT_ROAD  )  
IF (.NOT.DTT%LDATA_HC_BASEMENT_ROAD) DEALLOCATE(DTT%XPAR_HC_BASEMENT_ROAD)

ALLOCATE(DTT%XPAR_TC_COATING_ROAD     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                        HPROGRAM,CBLD_ATYPE,'TC_COATING','TWN',CFNAM_TC_COATING_ROAD ,CFTYP_TC_COATING_ROAD , &
                   XUNIF_TC_COATING_ROAD ,DTT%XPAR_TC_COATING_ROAD, DTT%LDATA_TC_COATING_ROAD  )  
IF (.NOT.DTT%LDATA_TC_COATING_ROAD) DEALLOCATE(DTT%XPAR_TC_COATING_ROAD)
!
ALLOCATE(DTT%XPAR_TC_BASEMENT_ROAD     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                        HPROGRAM,CBLD_ATYPE,'TC_BASEMENT','TWN',CFNAM_TC_BASEMENT_ROAD ,CFTYP_TC_BASEMENT_ROAD , &
                   XUNIF_TC_BASEMENT_ROAD ,DTT%XPAR_TC_BASEMENT_ROAD, DTT%LDATA_TC_BASEMENT_ROAD  )  
IF (.NOT.DTT%LDATA_TC_BASEMENT_ROAD) DEALLOCATE(DTT%XPAR_TC_BASEMENT_ROAD)

ALLOCATE(DTT%XPAR_D_COATING_ROAD      (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                        HPROGRAM,'ARI','D_COAT_ROAD','TWN',CFNAM_D_COATING_ROAD  ,CFTYP_D_COATING_ROAD  , &
                   XUNIF_D_COATING_ROAD  ,DTT%XPAR_D_COATING_ROAD , DTT%LDATA_D_COATING_ROAD  )
IF (.NOT.DTT%LDATA_D_COATING_ROAD) DEALLOCATE(DTT%XPAR_D_COATING_ROAD)
!  
ALLOCATE(DTT%XPAR_ALB_WALL    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,CBLD_ATYPE,'ALB_WALL   ','TWN',CFNAM_ALB_WALL  ,CFTYP_ALB_WALL  ,XUNIF_ALB_WALL  ,&
        DTT%XPAR_ALB_WALL, DTT%LDATA_ALB_WALL   )
IF (.NOT.DTT%LDATA_ALB_WALL) DEALLOCATE(DTT%XPAR_ALB_WALL)
!
ALLOCATE(DTT%XPAR_EMIS_WALL   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,CBLD_ATYPE,'EMIS_WALL  ','TWN',CFNAM_EMIS_WALL ,CFTYP_EMIS_WALL ,XUNIF_EMIS_WALL ,&
        DTT%XPAR_EMIS_WALL, DTT%LDATA_EMIS_WALL  )
IF (.NOT.DTT%LDATA_EMIS_WALL) DEALLOCATE(DTT%XPAR_EMIS_WALL)
!
ALLOCATE(DTT%XPAR_HC_WALL     (KDIM,NPAR_WALL_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, &
                        HPROGRAM,CBLD_ATYPE,'HC_WALL  ','TWN',CFNAM_HC_WALL ,CFTYP_HC_WALL , &
                   XUNIF_HC_WALL ,DTT%XPAR_HC_WALL, DTT%LDATA_HC_WALL  ) 
IF (.NOT.DTT%LDATA_HC_WALL) DEALLOCATE(DTT%XPAR_HC_WALL)
! 
ALLOCATE(DTT%XPAR_TC_WALL     (KDIM,NPAR_WALL_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, &
                        HPROGRAM,CBLD_ATYPE,'TC_WALL  ','TWN',CFNAM_TC_WALL ,CFTYP_TC_WALL , &
                   XUNIF_TC_WALL ,DTT%XPAR_TC_WALL, DTT%LDATA_TC_WALL  ) 
IF (.NOT.DTT%LDATA_TC_WALL) DEALLOCATE(DTT%XPAR_TC_WALL)
! 
ALLOCATE(DTT%XPAR_D_WALL      (KDIM,NPAR_WALL_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, &
                        HPROGRAM,CBLD_ATYPE,'D_WALL   ','TWN',CFNAM_D_WALL  ,CFTYP_D_WALL  , &
                   XUNIF_D_WALL  ,DTT%XPAR_D_WALL , DTT%LDATA_D_WALL  ) 
IF (.NOT.DTT%LDATA_D_WALL) DEALLOCATE(DTT%XPAR_D_WALL)
! 
ALLOCATE(DTT%XPAR_H_TRAFFIC   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','H_TRAFFIC  ','TWN',CFNAM_H_TRAFFIC  ,CFTYP_H_TRAFFIC  ,XUNIF_H_TRAFFIC  ,&
        DTT%XPAR_H_TRAFFIC, DTT%LDATA_H_TRAFFIC   )
IF (.NOT.DTT%LDATA_H_TRAFFIC) DEALLOCATE(DTT%XPAR_H_TRAFFIC)
!
ALLOCATE(DTT%XPAR_LE_TRAFFIC  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','LE_TRAFFIC ','TWN',CFNAM_LE_TRAFFIC ,CFTYP_LE_TRAFFIC ,XUNIF_LE_TRAFFIC ,&
        DTT%XPAR_LE_TRAFFIC, DTT%LDATA_LE_TRAFFIC  )
IF (.NOT.DTT%LDATA_LE_TRAFFIC) DEALLOCATE(DTT%XPAR_LE_TRAFFIC)
!
ALLOCATE(DTT%XPAR_H_INDUSTRY  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','H_INDUSTRY ','TWN',CFNAM_H_INDUSTRY ,CFTYP_H_INDUSTRY ,XUNIF_H_INDUSTRY ,&
        DTT%XPAR_H_INDUSTRY, DTT%LDATA_H_INDUSTRY  )
IF (.NOT.DTT%LDATA_H_INDUSTRY) DEALLOCATE(DTT%XPAR_H_INDUSTRY)
!
ALLOCATE(DTT%XPAR_LE_INDUSTRY (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','LE_INDUSTRY','TWN',CFNAM_LE_INDUSTRY,CFTYP_LE_INDUSTRY,XUNIF_LE_INDUSTRY,&
        DTT%XPAR_LE_INDUSTRY, DTT%LDATA_LE_INDUSTRY )
IF (.NOT.DTT%LDATA_LE_INDUSTRY) DEALLOCATE(DTT%XPAR_LE_INDUSTRY)
!
ALLOCATE(DTT%XPAR_ROUGH_ROOF    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,CBLD_ATYPE,'ROUGH_ROOF','TWN',CFNAM_ROUGH_ROOF,CFTYP_ROUGH_ROOF,XUNIF_ROUGH_ROOF ,&
        DTT%XPAR_ROUGH_ROOF,DTT%LDATA_ROUGH_ROOF)
IF (.NOT.DTT%LDATA_ROUGH_ROOF) DEALLOCATE(DTT%XPAR_ROUGH_ROOF)
!
ALLOCATE(DTT%XPAR_ROUGH_WALL    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,CBLD_ATYPE,'ROUGH_WALL','TWN',CFNAM_ROUGH_WALL,CFTYP_ROUGH_WALL,XUNIF_ROUGH_WALL ,&
        DTT%XPAR_ROUGH_WALL,DTT%LDATA_ROUGH_WALL)
IF (.NOT.DTT%LDATA_ROUGH_WALL) DEALLOCATE(DTT%XPAR_ROUGH_WALL)
!
!-------------------------------------------------------------------------------
!
!* coherence checks
!
 CALL COHERENCE_THERMAL_DATA('ROOF',DTT%LDATA_HC_ROOF,DTT%LDATA_TC_ROOF,DTT%LDATA_D_ROOF)
 CALL COHERENCE_THERMAL_DATA('WALL',DTT%LDATA_HC_WALL,DTT%LDATA_TC_WALL,DTT%LDATA_D_WALL)

!-------------------------------------------------------------------------------
!
!* road directions
!
ALLOCATE(DTT%XPAR_ROAD_DIR    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','ROAD_DIR   ','TWN',CFNAM_ROAD_DIR  ,CFTYP_ROAD_DIR    ,XUNIF_ROAD_DIR   ,&
        DTT%XPAR_ROAD_DIR, DTT%LDATA_ROAD_DIR    )
IF (.NOT.DTT%LDATA_ROAD_DIR) DEALLOCATE(DTT%XPAR_ROAD_DIR)
!
!-------------------------------------------------------------------------------
!
!* solar panels
!
ALLOCATE(DTT%XPAR_EMIS_PANEL  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','EMIS_PANEL ','BLD',CFNAM_EMIS_PANEL,CFTYP_EMIS_PANEL,XUNIF_EMIS_PANEL,&
       DTT%XPAR_EMIS_PANEL, DTT%LDATA_EMIS_PANEL    )
IF (.NOT.DTT%LDATA_EMIS_PANEL) DEALLOCATE(DTT%XPAR_EMIS_PANEL)
ALLOCATE(DTT%XPAR_ALB_PANEL   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','ALB_PANEL  ','BLD',CFNAM_ALB_PANEL ,CFTYP_ALB_PANEL ,XUNIF_ALB_PANEL ,&
       DTT%XPAR_ALB_PANEL , DTT%LDATA_ALB_PANEL     )
IF (.NOT.DTT%LDATA_ALB_PANEL ) DEALLOCATE(DTT%XPAR_ALB_PANEL )
ALLOCATE(DTT%XPAR_EFF_PANEL   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','EFF_PANEL  ','BLD',CFNAM_EFF_PANEL ,CFTYP_EFF_PANEL ,XUNIF_EFF_PANEL ,&
       DTT%XPAR_EFF_PANEL , DTT%LDATA_EFF_PANEL     )
IF (.NOT.DTT%LDATA_EFF_PANEL ) DEALLOCATE(DTT%XPAR_EFF_PANEL )
ALLOCATE(DTT%XPAR_FRAC_PANEL  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','FRAC_PANEL ','BLD',CFNAM_FRAC_PANEL,CFTYP_FRAC_PANEL,XUNIF_FRAC_PANEL,&
       DTT%XPAR_FRAC_PANEL, DTT%LDATA_FRAC_PANEL    )
IF (.NOT.DTT%LDATA_FRAC_PANEL) DEALLOCATE(DTT%XPAR_FRAC_PANEL)
!
!-------------------------------------------------------------------------------
!
!* greenroof fraction
!
IF (TOP%LGREENROOF) THEN
   ALLOCATE(DTT%XPAR_GREENROOF   (KDIM))
  CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,CBLD_ATYPE,'GREENROOF','BLD',CFNAM_GREENROOF,CFTYP_GREENROOF,XUNIF_GREENROOF ,&
        DTT%XPAR_GREENROOF,DTT%LDATA_GREENROOF)
  IF (.NOT.DTT%LDATA_GREENROOF) DEALLOCATE(DTT%XPAR_GREENROOF)
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
!  Population number
!
ALLOCATE(DTT%XPAR_NB_POP          (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','NB_POP','TWN',CFNAM_NB_POP, CFTYP_NB_POP, XUNIF_NB_POP, &
        DTT%XPAR_NB_POP, DTT%LDATA_NB_POP)
IF (.NOT.DTT%LDATA_NB_POP) DEALLOCATE(DTT%XPAR_NB_POP)
!
!  Co2 flux due to traffic
!
ALLOCATE(DTT%XPAR_SFCO2_RD        (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','SFCO2_RD','TWN',CFNAM_SFCO2_RD, CFTYP_SFCO2_RD, XUNIF_SFCO2_RD, &
        DTT%XPAR_SFCO2_RD, DTT%LDATA_SFCO2_RD)
IF (.NOT.DTT%LDATA_SFCO2_RD) THEN
    DEALLOCATE(DTT%XPAR_SFCO2_RD)
ELSE
   WRITE(ILUOUT,*) "------------------------------------------------------------"
   WRITE(ILUOUT,*) "you choose to simulate the CO2 emitted by traffic with TEB, "
   WRITE(ILUOUT,*) "please, do not provide any CO2 due to traffic in town in "
   WRITE(ILUOUT,*) "NAM_CH_SNAP_EMIS_PGD to avoid double counting. "
   WRITE(ILUOUT,*) "------------------------------------------------------------"
ENDIF
!
!  Legal time
!
ALLOCATE(DTT%XPAR_DELTA_LEGAL_TIME(KDIM, TOP%NTIME_CHANGE+1))
DO JJ=1,TOP%NTIME_CHANGE+1
  CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                            HPROGRAM,'ARI','DELTA_LEGAL_TIME','TWN',CFNAM_DELTA_LEGAL_TIME(JJ), CFTYP_DELTA_LEGAL_TIME(JJ), &
                            XUNIF_DELTA_LEGAL_TIME(JJ), DTT%XPAR_DELTA_LEGAL_TIME(:,JJ), DTT%LDATA_DELTA_LEGAL_TIME)
  IF (JJ>1 .AND. (.NOT. DTT%LDATA_DELTA_LEGAL_TIME)) THEN
     WRITE(ILUOUT,*) "NTIME_CHANGE was fixed to ", TOP%NTIME_CHANGE, "in NAM_TEB, "
     WRITE(ILUOUT,*) "but only", JJ-1, "different legal time(s) were provided in NAM_DATA_TEB"
     IF (LHOOK)   CALL DR_HOOK('PGD_TEB_PAR',1,ZHOOK_HANDLE)
     CALL ABOR1_SFX("PGD_TEB_PAR: Not enough legal times provided")
  END IF
ENDDO
!
IF (.NOT.DTT%LDATA_DELTA_LEGAL_TIME) THEN
   DEALLOCATE(DTT%XPAR_DELTA_LEGAL_TIME)
   WRITE(ILUOUT,*) "-------------------------------------------------------"
   WRITE(ILUOUT,*) "no DELTA_LEGAL_TIME value was provided for TEB,        "
   WRITE(ILUOUT,*) "solar time will be used to modulate traffic emmissions."
   WRITE(ILUOUT,*) "-------------------------------------------------------"
ELSEIF (MAXVAL(ABS(DTT%XPAR_DELTA_LEGAL_TIME)).GT.24. ) THEN
   WRITE(ILUOUT,*) "-------------------------------------------------------"
   WRITE (ILUOUT,*) "DTT%XPAR_DELTA_LEGAL_TIME : ",DTT%XPAR_DELTA_LEGAL_TIME
   WRITE (ILUOUT,*) "Beware, an absolute value above 24 was detected for DELTA_LEGAL_TIME"
   WRITE (ILUOUT,*) "this is a weird value for difference between UTC and legal time in hours."
   WRITE(ILUOUT,*) "-------------------------------------------------------"
   CALL ABOR1_SFX("PGD_TEB_PAR: Unplausible value for XPAR_DELTA_LEGAL_TIME")
ENDIF
!
!  Change of legal time
!
IF (TOP%NTIME_CHANGE>0) THEN
  ALLOCATE(DTT%XPAR_TIME_OF_CHANGE  (TOP%NTIME_CHANGE))
  DTT%XPAR_TIME_OF_CHANGE(:)%TDATE%YEAR  = NINT(XUNIF_TIME_OF_CHANGE(1,:))
  DTT%XPAR_TIME_OF_CHANGE(:)%TDATE%MONTH = NINT(XUNIF_TIME_OF_CHANGE(2,:))
  DTT%XPAR_TIME_OF_CHANGE(:)%TDATE%DAY   = NINT(XUNIF_TIME_OF_CHANGE(3,:))
  DTT%XPAR_TIME_OF_CHANGE(:)%TIME  = XUNIF_TIME_OF_CHANGE(4,:)
!
  CALL CHECK_TIME_OF_CHANGE(DTT%XPAR_TIME_OF_CHANGE)
 ! 
  DTT%LDATA_TIME_OF_CHANGE=.TRUE.
ELSE
  DTT%LDATA_TIME_OF_CHANGE=.FALSE.
ENDIF
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
!
SUBROUTINE CHECK_TIME_OF_CHANGE(TOC)
!
USE MODI_LT_DATE
!
USE MODD_TYPE_DATE_SURF
!
  TYPE(DATE_TIME), DIMENSION(:), INTENT(IN) :: TOC !TIME_OF_CHANGE array that have to be check
  INTEGER                              :: JJ  !Loop counter
!
!      Check is the date(s) really  exists
 IF (.NOT. ALL(IS_A_REAL_DATE(TOC))) THEN
    WRITE(ILUOUT,*) "Uncorret date(s) was/were detected"
    DO JJ=1,TOP%NTIME_CHANGE
        IF (.NOT. IS_A_REAL_DATE(DTT%XPAR_TIME_OF_CHANGE(JJ))) THEN 
           WRITE(ILUOUT,*) TOC(JJ)%TDATE%DAY, " ",&
                           TOC(JJ)%TDATE%MONTH, "  ",&
                           TOC(JJ)%TDATE%YEAR, "  ",&
                           TOC(JJ)%TIME, " "
        ENDIF
    END DO
    CALL FLUSH(ILUOUT)
    IF (LHOOK)   CALL DR_HOOK('PGD_TEB_PAR',1,ZHOOK_HANDLE)
    CALL ABOR1_SFX('Wrong date provided')
 ENDIF
!
!     Check date order
DO JJ=2, SIZE(TOC)
  GTEST=LT_DATE(TOC(JJ),TOC(JJ-1))
  IF (GTEST) THEN
    WRITE(ILUOUT,*) "Uncorrect date order was detected"
    WRITE(ILUOUT,*) TOC(JJ)%TDATE%DAY, " ", TOC(JJ)%TDATE%MONTH, "  ",&
                    TOC(JJ)%TDATE%YEAR, " ", TOC(JJ)%TIME
    WRITE(ILUOUT,*) "should be strictly before:"
    WRITE(ILUOUT,*) TOC(JJ-1)%TDATE%DAY, " ", TOC(JJ-1)%TDATE%MONTH, "  ",&
                    TOC(JJ-1)%TDATE%YEAR, " ", TOC(JJ-1)%TIME
    CALL FLUSH(ILUOUT)
    IF (LHOOK)   CALL DR_HOOK('PGD_TEB_PAR',1,ZHOOK_HANDLE)
    CALL ABOR1_SFX('Wrong date order was provided')
   ENDIF
END DO
END SUBROUTINE CHECK_TIME_OF_CHANGE
  

!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_TEB_PAR
