!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########################################
      SUBROUTINE PGD_BEM_PAR (DTCO, UG, U, USS, DTB, BOP, KDIM, HPROGRAM)

!     ##########################################
!
!!**** *PGD_BEM_PAR* monitor for averaging and interpolations of BEM input data
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
!!    G. Pigeon        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    08/2011
!!    G. Pigeon   09/2012, NPAR_FLOOR_LAYER default to 1
!!    M. Goret    02/2017  Add fractions of heating and CO2 conversion factors
!!    M. Goret    02/2017  Add NBEMCOMP_MAX and bug fix
!!    M. Goret    10/2017 add hot water
!!    M. Goret    02/2017  Add fractions of heating and CO2 conversion factors
!!    M. Goret    02/2017  Add NBEMCOMP_MAX and bug fix
!!    M. Goret    10/2017 add hot water
!!
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_DATA_BEM_n, ONLY : DATA_BEM_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODE_COHERENCE_FRAC
!
USE MODD_SURF_PAR,   ONLY : XUNDEF, NUNDEF
USE MODD_CSTS,   ONLY : XSURF_EPSILON
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_TEST_NAM_VAR_SURF
USE MODI_INI_VAR_FROM_DATA_0D
USE MODI_INI_VAR_FROM_DATA
USE MODI_ABOR1_SFX
!
USE MODE_POS_SURF
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
TYPE(DATA_BEM_t), INTENT(INOUT) :: DTB
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
!
INTEGER, INTENT(IN) :: KDIM
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: ILUOUT ! output listing logical unit
INTEGER :: ILUNAM ! namelist file  logical unit
LOGICAL :: GFOUND ! true if namelist is found
INTEGER :: JCOMP  ! Loop counter on compartments
INTEGER :: JSCHED ! Loop counter on schedules
INTEGER :: JJ     ! Loop counter on grid points
!
!*    0.3    Declaration of namelists
!            ------------------------
!
! bem options
!
INTEGER, PARAMETER             :: NFLOOR_MAX  = 9 ! Maximum number of floor layers
INTEGER, PARAMETER             :: NMASS_MAX   = 9 ! Maximum number of internal mass layers
INTEGER, PARAMETER             :: NBEMCOMP_MAX= 9 ! Maximum number of tiles for use/behaviour
!
INTEGER, PARAMETER             :: NFNAM  = 28      ! max file name length
INTEGER, PARAMETER             :: NFTYP  = 6       ! max file type length
!
INTEGER                        :: NPAR_FLOOR_LAYER ! number of floor layers
INTEGER                        :: NPAR_MASS_LAYER  ! number of internal mass layers
!
REAL , DIMENSION(NBEMCOMP_MAX) :: XUNIF_NATVENT ! Control variable for ventilation (0=NONE; 1=MANU; 2=AUTO)
CHARACTER(LEN=NFNAM)           :: CFNAM_NATVENT ! File name for NATVENT
CHARACTER(LEN=NFTYP)           :: CFTYP_NATVENT ! File type for NATVENT
!
REAL                           :: XUNIF_RESIDENTIAL    ! Residential fraction (for solar panels) [1]
CHARACTER(LEN=NFNAM)           :: CFNAM_RESIDENTIAL    ! File name for RESIDENTIAL
CHARACTER(LEN=NFTYP)           :: CFTYP_RESIDENTIAL    ! File type for RESIDENTIAL
!
REAL , DIMENSION(NBEMCOMP_MAX)          :: XUNIF_FRACOMP    ! Fractions of tiles for uses/behaviour [1]
CHARACTER(LEN=NFNAM)                    :: CFNAM_FRACOMP    ! File name for FRACOMP
CHARACTER(LEN=NFTYP)                    :: CFTYP_FRACOMP    ! File type for FRACOMP
!
REAL                                    :: XUNIF_ISMECH     ! Control variable for presence of mechanical ventilation [0=NO, 1=YES]
CHARACTER(LEN=NFNAM)                    :: CFNAM_ISMECH     ! File name for ISMECH
CHARACTER(LEN=NFTYP)                    :: CFTYP_ISMECH     ! File type for ISMECH
!
REAL                                    :: XUNIF_MECHRATE   ! Air exchange rate due to mechanical ventilation [vol h-1]
CHARACTER(LEN=NFNAM)                    :: CFNAM_MECHRATE   ! File name for MECHRATE
CHARACTER(LEN=NFTYP)                    :: CFTYP_MECHRATE   ! File type for MECHRATE
!
REAL                                    :: XUNIF_SHADEARCHI ! Control variable for presence of shading elements [0=None; 1=Adaptive; 2=Permanent]
CHARACTER(LEN=NFNAM)                    :: CFNAM_SHADEARCHI ! File name for SHADEARCHI
CHARACTER(LEN=NFTYP)                    :: CFTYP_SHADEARCHI ! File type for SHADEARCHI
!
! Floor parameters
!
REAL, DIMENSION(NFLOOR_MAX)                 :: XUNIF_HC_FLOOR     ! Floor layers heat capacity [J.K-1.m-3]
REAL, DIMENSION(NFLOOR_MAX)                 :: XUNIF_TC_FLOOR     ! Floor layers thermal conductivity [W.K-1.m-1]
REAL, DIMENSION(NFLOOR_MAX)                 :: XUNIF_D_FLOOR      ! Depth of floor layers [m]
REAL                                        :: XUNIF_FLOOR_HEIGHT ! Building floor height [m]
CHARACTER(LEN=NFNAM), DIMENSION(NFLOOR_MAX) :: CFNAM_HC_FLOOR     ! File name for HC_FLOOR   
CHARACTER(LEN=NFNAM), DIMENSION(NFLOOR_MAX) :: CFNAM_TC_FLOOR     ! File name for TC_FLOOR
CHARACTER(LEN=NFNAM), DIMENSION(NFLOOR_MAX) :: CFNAM_D_FLOOR      ! File name for D_FLOOR
CHARACTER(LEN=NFNAM)                        :: CFNAM_FLOOR_HEIGHT ! File name for FLOOR_HEIGHT
CHARACTER(LEN=NFTYP), DIMENSION(NFLOOR_MAX) :: CFTYP_HC_FLOOR     ! File type for HC_FLOOR   
CHARACTER(LEN=NFTYP), DIMENSION(NFLOOR_MAX) :: CFTYP_TC_FLOOR     ! File type for TC_FLOOR
CHARACTER(LEN=NFTYP), DIMENSION(NFLOOR_MAX) :: CFTYP_D_FLOOR      ! File type for D_FLOOR
CHARACTER(LEN=NFTYP)                        :: CFTYP_FLOOR_HEIGHT ! File type for FLOOR_HEIGHT
!
! Mass parameters
!
REAL, DIMENSION(NMASS_MAX)                 :: XUNIF_HC_MASS  ! Mass layers heat capacity [J.K-1.m-3]
REAL, DIMENSION(NMASS_MAX)                 :: XUNIF_TC_MASS  ! Mass layers thermal conductivity [W.K-1.m-1]
REAL, DIMENSION(NMASS_MAX)                 :: XUNIF_D_MASS   ! Depth of mass layers [m]
CHARACTER(LEN=NFNAM), DIMENSION(NMASS_MAX) :: CFNAM_HC_MASS  ! File name for HC_MASS   
CHARACTER(LEN=NFNAM), DIMENSION(NMASS_MAX) :: CFNAM_TC_MASS  ! File name for TC_MASS
CHARACTER(LEN=NFNAM), DIMENSION(NMASS_MAX) :: CFNAM_D_MASS   ! File name for D_MASS
CHARACTER(LEN=NFTYP), DIMENSION(NMASS_MAX) :: CFTYP_HC_MASS  ! File type for HC_MASS   
CHARACTER(LEN=NFTYP), DIMENSION(NMASS_MAX) :: CFTYP_TC_MASS  ! File type for TC_MASS
CHARACTER(LEN=NFTYP), DIMENSION(NMASS_MAX) :: CFTYP_D_MASS   ! File type for D_MASS
!
! AC systems parameters
!
REAL                 :: XUNIF_F_WASTE_CAN  ! Fraction of waste heat into the canyon [1]
REAL                 :: XUNIF_HR_TARGET    ! Relative humidity setpoint [1]
REAL                 :: XUNIF_CAP_SYS_HEAT ! Capacity of the heating system [W.m-2(bld)] 
REAL                 :: XUNIF_CAP_SYS_RAT  ! Rated capacity of the cooling system [1]
REAL                 :: XUNIF_T_ADP        ! Apparatus dewpoint temperature [K]
REAL                 :: XUNIF_M_SYS_RAT    ! Rated HVAC mass flow rate [kg s-1 m-2(bld)]
REAL                 :: XUNIF_COP_RAT      ! Rated COP of the cooling system [1]
REAL                 :: XUNIF_COP_DCS      ! Rated COP of the district cooling system [1]
REAL                 :: XUNIF_F_WATER_COND ! fraction of evaporation of condensers [1]
REAL                 :: XUNIF_DCS_AREA     ! presence of district cooling system [1]
CHARACTER(LEN=NFNAM) :: CFNAM_F_WASTE_CAN  ! File name for F_WASTE_CAN
CHARACTER(LEN=NFNAM) :: CFNAM_HR_TARGET    ! File name for HR_TARGET
CHARACTER(LEN=NFNAM) :: CFNAM_CAP_SYS_HEAT ! File name for CAP_SYS_HEAT
CHARACTER(LEN=NFNAM) :: CFNAM_CAP_SYS_RAT  ! File name for CAP_SYS_RAT
CHARACTER(LEN=NFNAM) :: CFNAM_T_ADP        ! File name for T_ADP
CHARACTER(LEN=NFNAM) :: CFNAM_M_SYS_RAT    ! File name for M_SYS_RAT
CHARACTER(LEN=NFNAM) :: CFNAM_COP_RAT      ! File name for COP_RAT
CHARACTER(LEN=NFNAM) :: CFNAM_COP_DCS      ! File name for COP_DCS
CHARACTER(LEN=NFNAM) :: CFNAM_F_WATER_COND ! File name for F_WATER_COND
CHARACTER(LEN=NFNAM) :: CFNAM_DCS_AREA     ! File name for DCS_AREA
CHARACTER(LEN=NFTYP) :: CFTYP_F_WASTE_CAN  ! File type for F_WASTE_CAN
CHARACTER(LEN=NFTYP) :: CFTYP_HR_TARGET    ! File type for HR_TARGET
CHARACTER(LEN=NFTYP) :: CFTYP_CAP_SYS_HEAT ! File type for CAP_SYS_HEAT
CHARACTER(LEN=NFTYP) :: CFTYP_CAP_SYS_RAT  ! File type for CAP_SYS_RAT
CHARACTER(LEN=NFTYP) :: CFTYP_T_ADP        ! File type for T_ADP
CHARACTER(LEN=NFTYP) :: CFTYP_M_SYS_RAT    ! File type for M_SYS_RAT
CHARACTER(LEN=NFTYP) :: CFTYP_COP_RAT      ! File type for COP_RAT
CHARACTER(LEN=NFTYP) :: CFTYP_COP_DCS      ! File type for COP_DCS
CHARACTER(LEN=NFTYP) :: CFTYP_F_WATER_COND ! File type for F_WATER_COND
CHARACTER(LEN=NFTYP) :: CFTYP_DCS_AREA     ! File type for DCS_AREA
!
REAL                 :: XUNIF_TDESV       ! Design temperature for ventilation [K]
REAL                 :: XUNIF_WIN_SW_MAX  ! Threshold for shortwave radiation received by walls for shading [W.m-2(wall)]
REAL                 :: XUNIF_FOPEN       ! Maximum fraction of windows opened [1]
CHARACTER(LEN=NFNAM) :: CFNAM_TDESV       ! File name for TDESV
CHARACTER(LEN=NFNAM) :: CFNAM_WIN_SW_MAX  ! File name for WIN_SW_MAX
CHARACTER(LEN=NFNAM) :: CFNAM_FOPEN       ! File name for FOPEN
CHARACTER(LEN=NFTYP) :: CFTYP_TDESV       ! File type for TDESV
CHARACTER(LEN=NFTYP) :: CFTYP_WIN_SW_MAX  ! File type for WIN_SW_MAX
CHARACTER(LEN=NFTYP) :: CFTYP_FOPEN       ! File type for FOPEN
!
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_FVSUM ! Fraction of households opening windows during warm conditions [1]
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_FVVAC ! Fraction of households opening windows when the building is vacant [1]
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_FVNIG ! Fraction of households opening windows during the night [1]
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_FSSUM ! Fraction of households closing shading elements during warm conditions [1]
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_FSVAC ! Fraction of households closing shading elements when the building is vacant [1]
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_FSNIG ! Fraction of households closing shading elements during the night [1]
CHARACTER(LEN=NFNAM)          :: CFNAM_FVSUM ! File name for FVSUM
CHARACTER(LEN=NFNAM)          :: CFNAM_FVVAC ! File name for FVVAC
CHARACTER(LEN=NFNAM)          :: CFNAM_FVNIG ! File name for FVNIG
CHARACTER(LEN=NFNAM)          :: CFNAM_FSSUM ! File name for FSSUM
CHARACTER(LEN=NFNAM)          :: CFNAM_FSVAC ! File name for FSVAC
CHARACTER(LEN=NFNAM)          :: CFNAM_FSNIG ! File name for FSNIG
CHARACTER(LEN=NFTYP)          :: CFTYP_FVSUM ! File type for FVSUM
CHARACTER(LEN=NFTYP)          :: CFTYP_FVVAC ! File type for FVVAC
CHARACTER(LEN=NFTYP)          :: CFTYP_FVNIG ! File type for FVNIG
CHARACTER(LEN=NFTYP)          :: CFTYP_FSSUM ! File type for FSSUM
CHARACTER(LEN=NFTYP)          :: CFTYP_FSVAC ! File type for FSVAC
CHARACTER(LEN=NFTYP)          :: CFTYP_FSNIG ! File type for FSNIG
!
REAL, DIMENSION(:,:), ALLOCATABLE :: XUNIF_DAYWBEG_SCHED ! Day of week for beginning of schedule (1=Monday)
REAL, DIMENSION(:,:), ALLOCATABLE :: XUNIF_HOURBEG_SCHED ! Hour of day for beginning of schedule 
REAL, DIMENSION(:,:), ALLOCATABLE :: XUNIF_PROBOCC       ! Probability of building occupation [1]
REAL, DIMENSION(:,:), ALLOCATABLE :: XUNIF_BEG_HOLIDAY   ! Julian day of year for beginning of holiday
REAL, DIMENSION(:,:), ALLOCATABLE :: XUNIF_END_HOLIDAY   ! Julian day of year for end of holiday
REAL, DIMENSION(NBEMCOMP_MAX)     :: XUNIF_MOD_HOLIDAY   ! Modulation factor for internal heat release during holidays [1]
CHARACTER(LEN=NFNAM)              :: CFNAM_DAYWBEG_SCHED ! File name for DAYWBEG_SCHED
CHARACTER(LEN=NFNAM)              :: CFNAM_HOURBEG_SCHED ! File name for HOURBEG_SCHED 
CHARACTER(LEN=NFNAM)              :: CFNAM_PROBOCC       ! File name for PROBOCC 
CHARACTER(LEN=NFNAM)              :: CFNAM_BEG_HOLIDAY   ! File name for BEG_HOLIDAY
CHARACTER(LEN=NFNAM)              :: CFNAM_END_HOLIDAY   ! File name for END_HOLIDAY
CHARACTER(LEN=NFNAM)              :: CFNAM_MOD_HOLIDAY   ! File name for MOD_HOLIDAY
CHARACTER(LEN=NFTYP)              :: CFTYP_DAYWBEG_SCHED ! File type for DAYWBEG_SCHED
CHARACTER(LEN=NFTYP)              :: CFTYP_HOURBEG_SCHED ! File type for HOURBEG_SCHED
CHARACTER(LEN=NFTYP)              :: CFTYP_PROBOCC       ! File type for PROBOCC
CHARACTER(LEN=NFTYP)              :: CFTYP_BEG_HOLIDAY   ! File type for BEG_HOLIDAY
CHARACTER(LEN=NFTYP)              :: CFTYP_END_HOLIDAY   ! File type for END_HOLIDAY
CHARACTER(LEN=NFTYP)              :: CFTYP_MOD_HOLIDAY   ! File type for MOD_HOLIDAY
!
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_TCOOL_OCCD ! Design temperature for air conditioning, occupied, day [K]
CHARACTER(LEN=NFNAM)          :: CFNAM_TCOOL_OCCD ! File name for TCOOL_OCCD
CHARACTER(LEN=NFTYP)          :: CFTYP_TCOOL_OCCD ! File type for TCOOL_OCCD
!
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_TCOOL_OCCN ! Design temperature for air conditioning, occupied, night [K]
CHARACTER(LEN=NFNAM)          :: CFNAM_TCOOL_OCCN ! File name for TCOOL_OCCN
CHARACTER(LEN=NFTYP)          :: CFTYP_TCOOL_OCCN ! File type for TCOOL_OCCN
!
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_TCOOL_VCDD ! Design temperature for air conditioning, vacant, day [K]
CHARACTER(LEN=NFNAM)          :: CFNAM_TCOOL_VCDD ! File name for TCOOL_VCDD
CHARACTER(LEN=NFTYP)          :: CFTYP_TCOOL_VCDD ! File type for TCOOL_VCDD
!
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_TCOOL_VCDN ! Design temperature for air conditioning, vacant, night [K]
CHARACTER(LEN=NFNAM)          :: CFNAM_TCOOL_VCDN ! File name for TCOOL_VCDN
CHARACTER(LEN=NFTYP)          :: CFTYP_TCOOL_VCDN ! File type for TCOOL_VCDN
!
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_TCOOL_VCLD ! Design temperature for air conditioning, vacant, long duration [K]   
CHARACTER(LEN=NFNAM)          :: CFNAM_TCOOL_VCLD ! File name for TCOOL_VCLD
CHARACTER(LEN=NFTYP)          :: CFTYP_TCOOL_VCLD ! File type for TCOOL_VCLD
!
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_THEAT_OCCD ! Design temperature for heating, occupied, day [K]
CHARACTER(LEN=NFNAM)          :: CFNAM_THEAT_OCCD ! File name for THEAT_OCCD
CHARACTER(LEN=NFTYP)          :: CFTYP_THEAT_OCCD ! File type for THEAT_OCCD
!
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_THEAT_OCCN ! Design temperature for heating, occupied, night [K]
CHARACTER(LEN=NFNAM)          :: CFNAM_THEAT_OCCN ! File name for THEAT_OCCN
CHARACTER(LEN=NFTYP)          :: CFTYP_THEAT_OCCN ! File type for THEAT_OCCN 
!
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_THEAT_VCDD ! Design temperature for heating, vacant, day [K]
CHARACTER(LEN=NFNAM)          :: CFNAM_THEAT_VCDD ! File name for THEAT_VCDD  
CHARACTER(LEN=NFTYP)          :: CFTYP_THEAT_VCDD ! File type for THEAT_VCDD
!
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_THEAT_VCDN ! Design temperature for heating, vacant, night [K]  
CHARACTER(LEN=NFNAM)          :: CFNAM_THEAT_VCDN ! File name for THEAT_VCDN
CHARACTER(LEN=NFTYP)          :: CFTYP_THEAT_VCDN ! File type for THEAT_VCDN
!
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_THEAT_VCLD ! Design temperature for heating, vacant, long duration [K]
CHARACTER(LEN=NFNAM)          :: CFNAM_THEAT_VCLD ! File name for THEAT_VCLD  
CHARACTER(LEN=NFTYP)          :: CFTYP_THEAT_VCLD ! File type for THEAT_VCLD
!
! Internal heat gains
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_QIN         ! Internal heat gains [W m-2(floor)]
REAL                          :: XUNIF_QIN_FRAD    ! Radiant fraction of int heat gains [1]
REAL                          :: XUNIF_QIN_FLAT    ! Latent franction of internal heat gains [1]
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_MODQIN_VCD  ! Modulation during short-term vacancy [1]
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_MODQIN_VLD  ! Modulation during long-term vacancy [1]
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_MODQIN_NIG  ! Modulation during night [1] 
CHARACTER(LEN=NFNAM)          :: CFNAM_QIN         ! File name for QIN
CHARACTER(LEN=NFNAM)          :: CFNAM_QIN_FRAD    ! File name for QIN_FRAD
CHARACTER(LEN=NFNAM)          :: CFNAM_QIN_FLAT    ! File name for QIN_FLAT
CHARACTER(LEN=NFNAM)          :: CFNAM_MODQIN_VCD  ! File name for MODQIN_VCD 
CHARACTER(LEN=NFNAM)          :: CFNAM_MODQIN_VLD  ! File name for MODQIN_VLD 
CHARACTER(LEN=NFNAM)          :: CFNAM_MODQIN_NIG  ! File name for MODQIN_NIG
CHARACTER(LEN=NFTYP)          :: CFTYP_QIN         ! File type for QIN
CHARACTER(LEN=NFTYP)          :: CFTYP_QIN_FRAD    ! File type for QIN_FRAD  
CHARACTER(LEN=NFTYP)          :: CFTYP_QIN_FLAT    ! File type for QIN_FLAT 
CHARACTER(LEN=NFTYP)          :: CFTYP_MODQIN_VCD  ! File type for MODQIN_VCD  
CHARACTER(LEN=NFTYP)          :: CFTYP_MODQIN_VLD  ! File type for MODQIN_VLD 
CHARACTER(LEN=NFTYP)          :: CFTYP_MODQIN_NIG  ! File type for MODQIN_NIG  
!
! window parameters
REAL                 :: XUNIF_GR      ! Glazing ratio [1]
REAL                 :: XUNIF_SHGC    ! Solar transmittance of windows [1] 
REAL                 :: XUNIF_SHGC_SH ! solar transmittance of windows + shading [1]
REAL                 :: XUNIF_U_WIN   ! U-Value of window [W.m-2.K-1]
CHARACTER(LEN=NFNAM) :: CFNAM_GR      ! File name for GR
CHARACTER(LEN=NFNAM) :: CFNAM_SHGC    ! File name for SHGC
CHARACTER(LEN=NFNAM) :: CFNAM_SHGC_SH ! File name for SHGC_SH
CHARACTER(LEN=NFNAM) :: CFNAM_U_WIN   ! File name for U_WIN
CHARACTER(LEN=NFTYP) :: CFTYP_GR      ! File type for GR
CHARACTER(LEN=NFTYP) :: CFTYP_SHGC    ! File type for SHGC
CHARACTER(LEN=NFTYP) :: CFTYP_SHGC_SH ! File type for SHGC
CHARACTER(LEN=NFTYP) :: CFTYP_U_WIN   ! File type for U_WIN
!
! air renewal
REAL                 :: XUNIF_N50 ! Airtightness of the building [vol.h-1 at 5O Pa]
CHARACTER(LEN=NFNAM) :: CFNAM_N50 ! File name for N50
CHARACTER(LEN=NFTYP) :: CFTYP_N50 ! File type for N50
!
! parameters for autosize calculation of the AC systems
REAL                  :: XUNIF_T_SIZE_MAX ! Temperature for capacity of the cooling system [K]
REAL                  :: XUNIF_T_SIZE_MIN ! Temperature for capacity of the heating system [K]
 CHARACTER(LEN=NFNAM) :: CFNAM_T_SIZE_MAX ! File name for T_SIZE_MAX
 CHARACTER(LEN=NFNAM) :: CFNAM_T_SIZE_MIN ! File name for T_SIZE_MIN
 CHARACTER(LEN=NFTYP) :: CFTYP_T_SIZE_MAX ! File type for T_SIZE_MAX
 CHARACTER(LEN=NFTYP) :: CFTYP_T_SIZE_MIN ! File type for T_SIZE_MIN
!
! heating fractions and emission factors(for CO2 flux from buildings calculation)
REAL                 :: XUNIF_FRAC_HEAT_ELEC ! uniform value for Fraction of electric heating [1]
CHARACTER(LEN=NFNAM) :: CFNAM_FRAC_HEAT_ELEC ! file name for Fraction of electric heating
CHARACTER(LEN=NFTYP) :: CFTYP_FRAC_HEAT_ELEC ! file type for Fraction of electric heating
!
REAL                 :: XUNIF_FRAC_HEAT_GAS  ! uniform value for Fraction of gas heating [1]
CHARACTER(LEN=NFNAM) :: CFNAM_FRAC_HEAT_GAS  ! file name for Fraction of gas heating
CHARACTER(LEN=NFTYP) :: CFTYP_FRAC_HEAT_GAS  ! file type for Fraction of gas heating
!
REAL                 :: XUNIF_FRAC_HEAT_OTHER ! uniform value for Fraction of other heating [1]
CHARACTER(LEN=NFNAM) :: CFNAM_FRAC_HEAT_OTHER ! file name for Fraction of other heating
CHARACTER(LEN=NFTYP) :: CFTYP_FRAC_HEAT_OTHER ! file type for Fraction of other heating
!
REAL                 :: XUNIF_FRAC_HEAT_FUEL ! uniform value for Fraction of fuel heating [1]
CHARACTER(LEN=NFNAM) :: CFNAM_FRAC_HEAT_FUEL ! file name for Fraction of fuel heating
CHARACTER(LEN=NFTYP) :: CFTYP_FRAC_HEAT_FUEL ! file type for Fraction of fuel heating
!
REAL :: XPAR_CF_CO2_ELEC  ! Emission factor CO2/electricity [KgCO2.J-1]
REAL :: XPAR_CF_CO2_GAS   ! Emission factor CO2/gas [KgCO2.J-1]
REAL :: XPAR_CF_CO2_FUEL  ! Emission factor CO2/fuel [KgCO2.J-1]
REAL :: XPAR_CF_CO2_OTHER ! Emission factor CO2/other source [KgCO2.J-1]
!
REAL, DIMENSION(NBEMCOMP_MAX) :: XUNIF_HOTWAT ! Energy needed for hot water [W m-2(floor)]
CHARACTER(LEN=NFNAM)          :: CFNAM_HOTWAT ! Energy needed for hot water [W m-2(floor)]
CHARACTER(LEN=NFTYP)          :: CFTYP_HOTWAT ! Energy needed for hot water [W m-2(floor)]
!
REAL                 :: XUNIF_F_HW_GAS ! Fraction of warm water heated by gas [1]
CHARACTER(LEN=NFNAM) :: CFNAM_F_HW_GAS ! File name for F_HW_GAS
CHARACTER(LEN=NFTYP) :: CFTYP_F_HW_GAS ! File type for F_HW_GAS
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_DATA_BEM/ NPAR_FLOOR_LAYER, NPAR_MASS_LAYER,                            &
                  XPAR_CF_CO2_ELEC, XPAR_CF_CO2_GAS,                                 &
                  XPAR_CF_CO2_FUEL, XPAR_CF_CO2_OTHER,                               &
                  XUNIF_HC_FLOOR, XUNIF_TC_FLOOR, XUNIF_D_FLOOR,                     &
                  XUNIF_HC_MASS , XUNIF_TC_MASS , XUNIF_D_MASS ,                     &
                  XUNIF_FLOOR_HEIGHT,                                                &
                  XUNIF_F_WASTE_CAN,                                                 &
                  XUNIF_F_WATER_COND, XUNIF_DCS_AREA, XUNIF_HR_TARGET,               &
                  XUNIF_QIN, XUNIF_QIN_FRAD, XUNIF_QIN_FLAT,                         &   
                  XUNIF_MODQIN_VCD, XUNIF_MODQIN_VLD, XUNIF_MODQIN_NIG,              &
                  XUNIF_SHGC, XUNIF_U_WIN, XUNIF_GR,XUNIF_SHGC_SH,                   &
                  XUNIF_N50,                                                         &
                  XUNIF_CAP_SYS_HEAT,                                                &
                  XUNIF_CAP_SYS_RAT, XUNIF_T_ADP, XUNIF_M_SYS_RAT,                   &
                  XUNIF_COP_RAT, XUNIF_COP_DCS, XUNIF_T_SIZE_MAX,                    &
                  XUNIF_T_SIZE_MIN,                                                  &
                  XUNIF_ISMECH, CFNAM_ISMECH, CFTYP_ISMECH,                          &
                  XUNIF_MECHRATE, CFNAM_MECHRATE, CFTYP_MECHRATE,                    &
                  XUNIF_SHADEARCHI, CFNAM_SHADEARCHI, CFTYP_SHADEARCHI,              &
                  XUNIF_NATVENT, CFNAM_NATVENT, CFTYP_NATVENT,                       &
                  XUNIF_FRACOMP, CFNAM_FRACOMP, CFTYP_FRACOMP,                       &
                  XUNIF_RESIDENTIAL, CFNAM_RESIDENTIAL, CFTYP_RESIDENTIAL,           &                  
                  CFNAM_HC_FLOOR, CFNAM_TC_FLOOR, CFNAM_D_FLOOR,                     &
                  CFNAM_HC_MASS, CFNAM_TC_MASS, CFNAM_D_MASS,                        &
                  XUNIF_TDESV, XUNIF_WIN_SW_MAX,                                     &
                  XUNIF_FOPEN, XUNIF_FVSUM, XUNIF_FVVAC,                             &
                  XUNIF_FVNIG, XUNIF_FSSUM, XUNIF_FSVAC, XUNIF_FSNIG,                &
                  XUNIF_DAYWBEG_SCHED, XUNIF_HOURBEG_SCHED, XUNIF_PROBOCC,           &
                  XUNIF_BEG_HOLIDAY, XUNIF_END_HOLIDAY, XUNIF_MOD_HOLIDAY,           &
                  CFNAM_FLOOR_HEIGHT,                                                &
                  CFNAM_F_WASTE_CAN,                                                 &
                  CFNAM_F_WATER_COND, CFNAM_DCS_AREA, CFNAM_HR_TARGET,               &
                  CFNAM_QIN, CFNAM_QIN_FRAD, CFNAM_QIN_FLAT,                         &    
                  CFNAM_MODQIN_VCD, CFNAM_MODQIN_VLD, CFNAM_MODQIN_NIG,              &
                  CFNAM_SHGC, CFNAM_U_WIN, CFNAM_GR,                                 &
                  CFNAM_SHGC_SH, CFNAM_N50,                                          &
                  CFNAM_CAP_SYS_HEAT,                                                &
                  CFNAM_CAP_SYS_RAT, CFNAM_T_ADP, CFNAM_M_SYS_RAT,                   &
                  CFNAM_COP_RAT, CFNAM_COP_DCS, CFNAM_T_SIZE_MAX,                    &
                  CFNAM_T_SIZE_MIN, CFNAM_TDESV,                                     &
                  CFNAM_WIN_SW_MAX, CFNAM_FOPEN, CFNAM_FVSUM,                        &
                  CFNAM_FVVAC, CFNAM_FVNIG, CFNAM_FSSUM,                             &
                  CFNAM_FSVAC, CFNAM_FSNIG, CFNAM_DAYWBEG_SCHED,                     &
                  CFNAM_HOURBEG_SCHED, CFNAM_PROBOCC,                                &
                  CFNAM_BEG_HOLIDAY, CFNAM_END_HOLIDAY, CFNAM_MOD_HOLIDAY,           &
                  CFTYP_HC_FLOOR, CFTYP_TC_FLOOR, CFTYP_D_FLOOR,                     &
                  CFTYP_HC_MASS, CFTYP_TC_MASS, CFTYP_D_MASS,                        &
                  CFTYP_FLOOR_HEIGHT,                                                &
                  CFTYP_F_WASTE_CAN,                                                 &
                  CFTYP_F_WATER_COND, CFTYP_DCS_AREA, CFTYP_HR_TARGET,               &
                  CFTYP_QIN, CFTYP_QIN_FRAD, CFTYP_QIN_FLAT,                         &   
                  CFTYP_MODQIN_VCD, CFTYP_MODQIN_VLD, CFTYP_MODQIN_NIG,              &
                  CFTYP_SHGC, CFTYP_U_WIN, CFTYP_GR,                                 & 
                  CFTYP_SHGC_SH, CFTYP_N50,                                          &
                  CFTYP_CAP_SYS_HEAT,                                                &
                  CFTYP_CAP_SYS_RAT, CFTYP_T_ADP, CFTYP_M_SYS_RAT,                   &
                  CFTYP_COP_RAT, CFTYP_COP_DCS, CFTYP_T_SIZE_MAX,                    &
                  CFTYP_T_SIZE_MIN, CFTYP_TDESV,                                     &
                  CFTYP_WIN_SW_MAX, CFTYP_FOPEN, CFTYP_FVSUM,                        &
                  CFTYP_FVVAC, CFTYP_FVNIG, CFTYP_FSSUM,                             &
                  CFTYP_FSVAC, CFTYP_FSNIG, CFTYP_DAYWBEG_SCHED,                     &
                  CFTYP_HOURBEG_SCHED, CFTYP_PROBOCC,                                &
                  CFTYP_BEG_HOLIDAY, CFTYP_END_HOLIDAY, CFTYP_MOD_HOLIDAY,           &
                  XUNIF_FVSUM, CFNAM_FVSUM,CFTYP_FVSUM,                              &
                  XUNIF_TCOOL_OCCD,CFNAM_TCOOL_OCCD,CFTYP_TCOOL_OCCD,                &
                  XUNIF_TCOOL_OCCN,CFNAM_TCOOL_OCCN,CFTYP_TCOOL_OCCN,                &
                  XUNIF_TCOOL_VCDD,CFNAM_TCOOL_VCDD,CFTYP_TCOOL_VCDD,                &
                  XUNIF_TCOOL_VCDN,CFNAM_TCOOL_VCDN,CFTYP_TCOOL_VCDN,                &
                  XUNIF_TCOOL_VCLD,CFNAM_TCOOL_VCLD,CFTYP_TCOOL_VCLD,                &
                  XUNIF_THEAT_OCCD,CFNAM_THEAT_OCCD,CFTYP_THEAT_OCCD,                &
                  XUNIF_THEAT_OCCN,CFNAM_THEAT_OCCN,CFTYP_THEAT_OCCN,                &
                  XUNIF_THEAT_VCDD,CFNAM_THEAT_VCDD,CFTYP_THEAT_VCDD,                &
                  XUNIF_THEAT_VCDN,CFNAM_THEAT_VCDN,CFTYP_THEAT_VCDN,                &
                  XUNIF_THEAT_VCLD,CFNAM_THEAT_VCLD,CFTYP_THEAT_VCLD,                &
                  XUNIF_FRAC_HEAT_ELEC, CFNAM_FRAC_HEAT_ELEC, CFTYP_FRAC_HEAT_ELEC,  &
                  XUNIF_FRAC_HEAT_GAS,  CFNAM_FRAC_HEAT_GAS,  CFTYP_FRAC_HEAT_GAS,   &
                  XUNIF_FRAC_HEAT_OTHER,CFNAM_FRAC_HEAT_OTHER,CFTYP_FRAC_HEAT_OTHER, &
                  XUNIF_FRAC_HEAT_FUEL, CFNAM_FRAC_HEAT_FUEL, CFTYP_FRAC_HEAT_FUEL,  &
                  XUNIF_HOTWAT,         CFNAM_HOTWAT,         CFTYP_HOTWAT ,         &
                  XUNIF_F_HW_GAS,       CFNAM_F_HW_GAS,       CFTYP_F_HW_GAS
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PGD_BEM_PAR',0,ZHOOK_HANDLE)
!
!*    1.      Initializations
!             ---------------
!
DTB%NPAR_DAY_SCHED = 3
DTB%NPAR_CRE_SCHED = 4
DTB%NPAR_HOLIDAY   = 1
!
ALLOCATE(XUNIF_DAYWBEG_SCHED(DTB%NPAR_DAY_SCHED,NBEMCOMP_MAX))
ALLOCATE(XUNIF_HOURBEG_SCHED(DTB%NPAR_DAY_SCHED*DTB%NPAR_CRE_SCHED,NBEMCOMP_MAX))
ALLOCATE(XUNIF_PROBOCC      (DTB%NPAR_DAY_SCHED*DTB%NPAR_CRE_SCHED,NBEMCOMP_MAX))
!
ALLOCATE(XUNIF_BEG_HOLIDAY(DTB%NPAR_HOLIDAY,NBEMCOMP_MAX))
ALLOCATE(XUNIF_END_HOLIDAY(DTB%NPAR_HOLIDAY,NBEMCOMP_MAX))
!
NPAR_FLOOR_LAYER = NUNDEF
NPAR_MASS_LAYER  = NUNDEF
XPAR_CF_CO2_ELEC = XUNDEF
XPAR_CF_CO2_GAS  = XUNDEF
XPAR_CF_CO2_FUEL = XUNDEF
XPAR_CF_CO2_OTHER= XUNDEF
XUNIF_FRACOMP    = XUNDEF
XUNIF_RESIDENTIAL= XUNDEF
XUNIF_ISMECH     = XUNDEF
XUNIF_MECHRATE   = XUNDEF
XUNIF_SHADEARCHI = XUNDEF
XUNIF_NATVENT    = XUNDEF
XUNIF_HC_FLOOR   = XUNDEF
XUNIF_TC_FLOOR   = XUNDEF
XUNIF_D_FLOOR    = XUNDEF
XUNIF_HC_MASS    = XUNDEF
XUNIF_TC_MASS    = XUNDEF
XUNIF_D_MASS     = XUNDEF
XUNIF_F_WASTE_CAN= XUNDEF
XUNIF_TCOOL_OCCD = XUNDEF
XUNIF_TCOOL_OCCN = XUNDEF
XUNIF_TCOOL_VCDD = XUNDEF
XUNIF_TCOOL_VCDN = XUNDEF
XUNIF_TCOOL_VCLD = XUNDEF
XUNIF_THEAT_OCCD = XUNDEF
XUNIF_THEAT_OCCN = XUNDEF
XUNIF_THEAT_VCDD = XUNDEF
XUNIF_THEAT_VCDN = XUNDEF
XUNIF_THEAT_VCLD = XUNDEF
XUNIF_QIN        = XUNDEF
XUNIF_QIN_FRAD   = XUNDEF
XUNIF_MODQIN_VCD = XUNDEF
XUNIF_MODQIN_VLD = XUNDEF
XUNIF_MODQIN_NIG = XUNDEF
XUNIF_SHGC         = XUNDEF
XUNIF_U_WIN        = XUNDEF
XUNIF_GR           = XUNDEF
XUNIF_SHGC_SH      = XUNDEF
XUNIF_FLOOR_HEIGHT = XUNDEF
XUNIF_N50          = XUNDEF
XUNIF_F_WATER_COND = XUNDEF
XUNIF_DCS_AREA     = XUNDEF
XUNIF_QIN_FLAT     = XUNDEF
XUNIF_HR_TARGET    = XUNDEF
XUNIF_CAP_SYS_HEAT = XUNDEF
XUNIF_CAP_SYS_RAT  = XUNDEF
XUNIF_T_ADP        = XUNDEF
XUNIF_M_SYS_RAT    = XUNDEF
XUNIF_COP_RAT      = XUNDEF
XUNIF_COP_DCS      = XUNDEF
XUNIF_T_SIZE_MAX   = XUNDEF
XUNIF_T_SIZE_MIN   = XUNDEF
XUNIF_TDESV             = XUNDEF
XUNIF_WIN_SW_MAX        = XUNDEF
XUNIF_FOPEN             = XUNDEF
XUNIF_FVSUM             = XUNDEF
XUNIF_FVVAC             = XUNDEF
XUNIF_FVNIG             = XUNDEF
XUNIF_FSSUM             = XUNDEF
XUNIF_FSVAC             = XUNDEF
XUNIF_FSNIG             = XUNDEF
XUNIF_DAYWBEG_SCHED(:,:)= XUNDEF
XUNIF_HOURBEG_SCHED(:,:)= XUNDEF
XUNIF_PROBOCC(:,:)      = XUNDEF
XUNIF_BEG_HOLIDAY(:,:)  = XUNDEF
XUNIF_END_HOLIDAY(:,:)  = XUNDEF
XUNIF_MOD_HOLIDAY(:)    = XUNDEF
XUNIF_FRAC_HEAT_ELEC    = XUNDEF
XUNIF_FRAC_HEAT_GAS     = XUNDEF
XUNIF_FRAC_HEAT_OTHER   = XUNDEF
XUNIF_FRAC_HEAT_FUEL    = XUNDEF
XUNIF_HOTWAT            = XUNDEF
XUNIF_F_HW_GAS          = XUNDEF
!
CFNAM_ISMECH       = '                            '
CFNAM_MECHRATE     = '                            '
CFNAM_SHADEARCHI   = '                            '
CFNAM_NATVENT      = '                            '
CFNAM_FRACOMP      = '                            '
CFNAM_RESIDENTIAL  = '                            '
CFNAM_HC_FLOOR (:) = '                            '
CFNAM_TC_FLOOR (:) = '                            '
CFNAM_D_FLOOR  (:) = '                            '
CFNAM_HC_MASS (:) = '                            '
CFNAM_TC_MASS (:) = '                            '
CFNAM_D_MASS  (:) = '                            '
CFNAM_F_WASTE_CAN  = '                            '
CFNAM_TCOOL_OCCD  = '                            '
CFNAM_TCOOL_OCCN  = '                            '
CFNAM_TCOOL_VCDD  = '                            '
CFNAM_TCOOL_VCDN  = '                            '
CFNAM_TCOOL_VCLD  = '                            '
CFNAM_THEAT_OCCD  = '                            '
CFNAM_THEAT_OCCN  = '                            '
CFNAM_THEAT_VCDD  = '                            '
CFNAM_THEAT_VCDN  = '                            '
CFNAM_THEAT_VCLD  = '                            '
CFNAM_QIN         = '                            '
CFNAM_QIN_FRAD    = '                            '
CFNAM_MODQIN_VCD  = '                            '
CFNAM_MODQIN_VLD  = '                            '
CFNAM_MODQIN_NIG  = '                            '
CFNAM_TCOOL_OCCD  = '                            '
CFNAM_TCOOL_OCCN  = '                            '
CFNAM_TCOOL_VCDD  = '                            '
CFNAM_TCOOL_VCDN  = '                            '
CFNAM_TCOOL_VCLD  = '                            '
CFNAM_THEAT_OCCD  = '                            '
CFNAM_THEAT_OCCN  = '                            '
CFNAM_THEAT_VCDD  = '                            '
CFNAM_THEAT_VCDN  = '                            '
CFNAM_THEAT_VCLD  = '                            '
CFNAM_SHGC         = '                            '
CFNAM_U_WIN        = '                            '
CFNAM_GR           = '                            '
CFNAM_SHGC_SH      = '                            '
CFNAM_FLOOR_HEIGHT = '                            '
CFNAM_N50          = '                            '
CFNAM_F_WATER_COND = '                            '
CFNAM_DCS_AREA     = '                            '
CFNAM_QIN_FLAT     = '                            '
CFNAM_HR_TARGET    = '                            '
CFNAM_CAP_SYS_HEAT = '                            '
CFNAM_CAP_SYS_RAT  = '                            '
CFNAM_T_ADP        = '                            '
CFNAM_M_SYS_RAT    = '                            '
CFNAM_COP_RAT      = '                            '
CFNAM_COP_DCS      = '                            '
CFNAM_T_SIZE_MAX   = '                            '
CFNAM_T_SIZE_MIN   = '                            '
CFNAM_TDESV   = '                            '
CFNAM_WIN_SW_MAX  = '                            '
CFNAM_FOPEN = '                            '
CFNAM_FVSUM  = '                            ' 
CFNAM_FVVAC  = '                            ' 
CFNAM_FVNIG  = '                            ' 
CFNAM_FSSUM  = '                            ' 
CFNAM_FSVAC  = '                            '
CFNAM_FSNIG  = '                            '
CFNAM_DAYWBEG_SCHED= '                            '
CFNAM_HOURBEG_SCHED= '                            '
CFNAM_PROBOCC= '                            '
CFNAM_BEG_HOLIDAY= '                            '
CFNAM_END_HOLIDAY= '                            '
CFNAM_MOD_HOLIDAY= '                            '
CFNAM_FRAC_HEAT_ELEC  = '                            '
CFNAM_FRAC_HEAT_GAS   = '                            '
CFNAM_FRAC_HEAT_OTHER = '                            '
CFNAM_FRAC_HEAT_FUEL  = '                            '
CFNAM_HOTWAT = '                            '
CFNAM_F_HW_GAS  = '                            '
!
CFTYP_ISMECH      = '      '
CFTYP_MECHRATE    = '      '
CFTYP_SHADEARCHI  = '      '
CFTYP_NATVENT      = '      '
CFTYP_FRACOMP      = '      '
CFTYP_RESIDENTIAL  = '      '
CFTYP_HC_FLOOR(:)  = '      '
CFTYP_TC_FLOOR(:)  = '      '
CFTYP_D_FLOOR(:)   = '      '
CFTYP_HC_MASS(:)  = '      '
CFTYP_TC_MASS(:)  = '      '
CFTYP_D_MASS(:)   = '      '
CFTYP_F_WASTE_CAN  = '      '
CFTYP_QIN          = '      '
CFTYP_QIN_FRAD     = '      '
CFTYP_MODQIN_VCD   = '      '
CFTYP_MODQIN_VLD   = '      '
CFTYP_MODQIN_NIG   = '      '
CFTYP_SHGC         = '      '
CFTYP_U_WIN        = '      '
CFTYP_GR           = '      '
CFTYP_SHGC_SH      = '      '
CFTYP_FLOOR_HEIGHT = '      '
CFTYP_N50          = '      '
CFTYP_F_WATER_COND = '      '
CFTYP_DCS_AREA     = '      '
CFTYP_QIN_FLAT     = '      '
CFTYP_HR_TARGET    = '      '
CFTYP_CAP_SYS_HEAT = '      '
CFTYP_CAP_SYS_RAT  = '      '
CFTYP_T_ADP        = '      '
CFTYP_M_SYS_RAT    = '      '
CFTYP_COP_RAT      = '      '
CFTYP_COP_DCS      = '      '
CFTYP_T_SIZE_MAX   = '      '
CFTYP_T_SIZE_MIN   = '      '
CFTYP_FVNIG   = '      '
CFTYP_TDESV   = '      '
CFTYP_WIN_SW_MAX  = '      '
CFTYP_FOPEN    = '      '
CFTYP_FVSUM  = '      '
CFTYP_FVVAC  = '      '
CFTYP_FSSUM  = '      '
CFTYP_FSVAC  = '      '
CFTYP_FSNIG   = '      '
CFTYP_DAYWBEG_SCHED='      '
CFTYP_HOURBEG_SCHED='      '
CFTYP_PROBOCC='      '
CFTYP_BEG_HOLIDAY= '      '
CFTYP_END_HOLIDAY= '      '
CFTYP_MOD_HOLIDAY= '      '
CFTYP_FRAC_HEAT_ELEC  = '      '
CFTYP_FRAC_HEAT_GAS   = '      '
CFTYP_FRAC_HEAT_OTHER = '      '
CFTYP_FRAC_HEAT_FUEL  = '      '
CFTYP_HOTWAT = '      '
CFTYP_F_HW_GAS  = '      '
!
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DATA_BEM',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_BEM)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
!* coherence check
!
IF ((     ANY(XUNIF_HC_FLOOR/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_HC_FLOOR)>0) &
     .OR. ANY(XUNIF_TC_FLOOR/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_TC_FLOOR)>0) &
     .OR. ANY(XUNIF_D_FLOOR /=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_D_FLOOR )>0) &
    ) .AND. NPAR_FLOOR_LAYER==NUNDEF                                  ) THEN
  NPAR_FLOOR_LAYER=1 !default value
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'NPAR_FLOOR_LAYER is not specify, default value of',NPAR_FLOOR_LAYER, 'is taken'
  WRITE(ILUOUT,*) '---------------------------------------------'
END IF
!
IF ((     ANY(XUNIF_HC_MASS/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_HC_MASS)>0) &
     .OR. ANY(XUNIF_TC_MASS/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_TC_MASS)>0) &
     .OR. ANY(XUNIF_D_MASS /=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_D_MASS )>0) &
     ) .AND. (NPAR_MASS_LAYER ==NUNDEF)                                  ) THEN
  NPAR_MASS_LAYER=1 !default value
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'NPAR_MASS_LAYER is not specify, default value of',NPAR_MASS_LAYER, 'is taken'
  WRITE(ILUOUT,*) '---------------------------------------------'
END IF
!
!-------------------------------------------------------------------------------
!If not used, this variables are switch to 0 in order to have empty arrays
IF (NPAR_FLOOR_LAYER==NUNDEF) NPAR_FLOOR_LAYER=0
IF (NPAR_MASS_LAYER==NUNDEF) NPAR_MASS_LAYER=0
!-------------------------------------------------------------------------------
IF (NFLOOR_MAX < NPAR_FLOOR_LAYER) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_bem_par.F90 routine :      '
  WRITE(ILUOUT,*) 'The maximum number of FLOOR LAYER             '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NPAR_FLOOR_LAYER
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_BEM_PAR: MAXIMUM NUMBER OF NPAR_FLOOR_LAYER MUST BE INCREASED')
END IF
!-------------------------------------------------------------------------------
IF (NMASS_MAX < NPAR_MASS_LAYER) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_bem_par.F90 routine :      '
  WRITE(ILUOUT,*) 'The maximum number of MASS_LAYER             '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NPAR_MASS_LAYER
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_BEM_PAR: MAXIMUM NUMBER OF NPAR_MASS_LAYER MUST BE INCREASED')
END IF
!-------------------------------------------------------------------------------
IF (NBEMCOMP_MAX < BOP%NBEMCOMP) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_bem_par.F90 routine :      '
  WRITE(ILUOUT,*) 'The maximum number of BEMCOMP             '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', BOP%NBEMCOMP
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_BEM_PAR: MAXIMUM NUMBER OF NBEMCOMP MUST BE INCREASED')
END IF
!
!-------------------------------------------------------------------------------
!
! scalar values are stored in the DTB struture
! 
DTB%NPAR_FLOOR_LAYER  = NPAR_FLOOR_LAYER
DTB%NPAR_MASS_LAYER   = NPAR_MASS_LAYER
!
! default values are replaced if needed
 IF (XPAR_CF_CO2_ELEC/=XUNDEF)  BOP%XCF_CO2_ELEC =XPAR_CF_CO2_ELEC
 IF (XPAR_CF_CO2_GAS/=XUNDEF)   BOP%XCF_CO2_GAS  =XPAR_CF_CO2_GAS
 IF (XPAR_CF_CO2_FUEL/=XUNDEF)  BOP%XCF_CO2_FUEL =XPAR_CF_CO2_FUEL
 IF (XPAR_CF_CO2_OTHER/=XUNDEF) BOP%XCF_CO2_OTHER=XPAR_CF_CO2_OTHER
!
!-------------------------------------------------------------------------------
!
!*    3.      user defined fields are prescribed
!             ----------------------------------
!
ALLOCATE(DTB%XPAR_SHADEARCHI (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','SHADEARCHI      ','TWN', CFNAM_SHADEARCHI, CFTYP_SHADEARCHI, XUNIF_SHADEARCHI, &
        DTB%XPAR_SHADEARCHI, DTB%LDATA_SHADEARCHI )
IF (.NOT.DTB%LDATA_SHADEARCHI) DEALLOCATE(DTB%XPAR_SHADEARCHI)
!
ALLOCATE(DTB%XPAR_ISMECH  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','ISMECH      ','TWN', CFNAM_ISMECH, CFTYP_ISMECH, XUNIF_ISMECH, &
        DTB%XPAR_ISMECH, DTB%LDATA_ISMECH )
IF (.NOT.DTB%LDATA_ISMECH) DEALLOCATE(DTB%XPAR_ISMECH)
!
ALLOCATE(DTB%XPAR_MECHRATE(KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','MECHRATE      ','TWN', CFNAM_MECHRATE, CFTYP_MECHRATE, XUNIF_MECHRATE, &
        DTB%XPAR_MECHRATE, DTB%LDATA_MECHRATE )
IF (.NOT.DTB%LDATA_MECHRATE) DEALLOCATE(DTB%XPAR_MECHRATE)
!
ALLOCATE(DTB%XPAR_FRACOMP      (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
   CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
       HPROGRAM,'MAJ','FRACOMP   ','TWN', CFNAM_FRACOMP, CFTYP_FRACOMP, XUNIF_FRACOMP(JCOMP), &
       DTB%XPAR_FRACOMP(:,JCOMP), DTB%LDATA_FRACOMP )
ENDDO
IF (.NOT.DTB%LDATA_FRACOMP) THEN
   DEALLOCATE(DTB%XPAR_FRACOMP)
ELSE
   DO JJ=1,SIZE(DTB%XPAR_FRACOMP,1)
      IF (ABS(SUM(DTB%XPAR_FRACOMP(JJ,:))-1.0).GT.XSURF_EPSILON) CALL ABOR1_SFX("Wrong sum of compartment fractions")
   ENDDO
ENDIF
!
ALLOCATE(DTB%XPAR_RESIDENTIAL  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
    HPROGRAM,'MAJ','RESIDENTIAL','TWN', CFNAM_RESIDENTIAL, CFTYP_RESIDENTIAL, XUNIF_RESIDENTIAL, &
    DTB%XPAR_RESIDENTIAL(:), DTB%LDATA_RESIDENTIAL)
IF (.NOT.DTB%LDATA_RESIDENTIAL) THEN
   DEALLOCATE(DTB%XPAR_RESIDENTIAL)
ENDIF
!
ALLOCATE(DTB%XPAR_NATVENT(KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
   CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                             HPROGRAM,'MAJ','NATVENT   ','TWN', CFNAM_NATVENT, CFTYP_NATVENT, XUNIF_NATVENT(JCOMP), &
                             DTB%XPAR_NATVENT(:,JCOMP), DTB%LDATA_NATVENT )
ENDDO
IF (.NOT.DTB%LDATA_NATVENT) DEALLOCATE(DTB%XPAR_NATVENT)
!
ALLOCATE(DTB%XPAR_HC_FLOOR    (KDIM,NPAR_FLOOR_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, &
                        HPROGRAM,'INV','HC_FLOOR  ','TWN',CFNAM_HC_FLOOR,CFTYP_HC_FLOOR, &
        XUNIF_HC_FLOOR,DTB%XPAR_HC_FLOOR,DTB%LDATA_HC_FLOOR )
IF (.NOT.DTB%LDATA_HC_FLOOR) DEALLOCATE(DTB%XPAR_HC_FLOOR)
!
ALLOCATE(DTB%XPAR_TC_FLOOR    (KDIM,NPAR_FLOOR_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, &
                        HPROGRAM,'ARI','TC_FLOOR  ','TWN',CFNAM_TC_FLOOR,CFTYP_TC_FLOOR, &
                 XUNIF_TC_FLOOR ,DTB%XPAR_TC_FLOOR, DTB%LDATA_TC_FLOOR )
IF (.NOT.DTB%LDATA_TC_FLOOR) DEALLOCATE(DTB%XPAR_TC_FLOOR)
!
ALLOCATE(DTB%XPAR_D_FLOOR     (KDIM,NPAR_FLOOR_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, &
                        HPROGRAM,'ARI','D_FLOOR   ','TWN',CFNAM_D_FLOOR,CFTYP_D_FLOOR, &
                 XUNIF_D_FLOOR  ,DTB%XPAR_D_FLOOR , DTB%LDATA_D_FLOOR )
IF (.NOT.DTB%LDATA_D_FLOOR) DEALLOCATE(DTB%XPAR_D_FLOOR)
!
ALLOCATE(DTB%XPAR_HC_MASS    (KDIM,NPAR_MASS_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, &
                        HPROGRAM,'INV','HC_MASS   ','TWN',CFNAM_HC_MASS,CFTYP_HC_MASS, &
        XUNIF_HC_MASS,DTB%XPAR_HC_MASS,DTB%LDATA_HC_MASS )
IF (.NOT.DTB%LDATA_HC_MASS) DEALLOCATE(DTB%XPAR_HC_MASS)
!
ALLOCATE(DTB%XPAR_TC_MASS    (KDIM,NPAR_MASS_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, &
                        HPROGRAM,'ARI','TC_MASS   ','TWN',CFNAM_TC_MASS,CFTYP_TC_MASS, &
                 XUNIF_TC_MASS ,DTB%XPAR_TC_MASS, DTB%LDATA_TC_MASS )
IF (.NOT.DTB%LDATA_TC_MASS) DEALLOCATE(DTB%XPAR_TC_MASS)
!
ALLOCATE(DTB%XPAR_D_MASS     (KDIM,NPAR_MASS_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, &
                        HPROGRAM,'ARI','D_MASS   ','TWN',CFNAM_D_MASS,CFTYP_D_MASS, &
                 XUNIF_D_MASS  ,DTB%XPAR_D_MASS , DTB%LDATA_D_MASS )
IF (.NOT.DTB%LDATA_D_MASS) DEALLOCATE(DTB%XPAR_D_MASS)
!
ALLOCATE(DTB%XPAR_DAYWBEG_SCHED(KDIM,DTB%NPAR_DAY_SCHED,BOP%NBEMCOMP))
DO JSCHED=1,DTB%NPAR_DAY_SCHED
   DO JCOMP=1,BOP%NBEMCOMP
      CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
         HPROGRAM,'MAJ','DAYWBEG_SCHED','TWN',CFNAM_DAYWBEG_SCHED,CFTYP_DAYWBEG_SCHED, &
         XUNIF_DAYWBEG_SCHED(JSCHED,JCOMP),DTB%XPAR_DAYWBEG_SCHED(:,JSCHED,JCOMP), DTB%LDATA_DAYWBEG_SCHED )
   ENDDO
ENDDO
IF (.NOT.DTB%LDATA_DAYWBEG_SCHED) DEALLOCATE(DTB%XPAR_DAYWBEG_SCHED)
!
ALLOCATE(DTB%XPAR_HOURBEG_SCHED(KDIM,DTB%NPAR_DAY_SCHED*DTB%NPAR_CRE_SCHED,BOP%NBEMCOMP))
DO JSCHED=1,DTB%NPAR_DAY_SCHED*DTB%NPAR_CRE_SCHED
   DO JCOMP=1,BOP%NBEMCOMP
      CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
         HPROGRAM,'MAJ','XHOURBEG_SCHED','TWN',CFNAM_HOURBEG_SCHED,CFTYP_HOURBEG_SCHED, &
         XUNIF_HOURBEG_SCHED(JSCHED,JCOMP),DTB%XPAR_HOURBEG_SCHED(:,JSCHED,JCOMP),DTB%LDATA_HOURBEG_SCHED )
   ENDDO
ENDDO
IF (.NOT.DTB%LDATA_HOURBEG_SCHED) DEALLOCATE(DTB%XPAR_HOURBEG_SCHED)
!
ALLOCATE(DTB%XPAR_PROBOCC      (KDIM,DTB%NPAR_DAY_SCHED*DTB%NPAR_CRE_SCHED,BOP%NBEMCOMP))
DO JSCHED=1,DTB%NPAR_DAY_SCHED*DTB%NPAR_CRE_SCHED
   DO JCOMP=1,BOP%NBEMCOMP
      CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
        HPROGRAM,'MAJ','PROBOCC','TWN',CFNAM_PROBOCC,CFTYP_PROBOCC, &
        XUNIF_PROBOCC(JSCHED,JCOMP),DTB%XPAR_PROBOCC(:,JSCHED,JCOMP),DTB%LDATA_PROBOCC)
   ENDDO
ENDDO
IF (.NOT.DTB%LDATA_PROBOCC) DEALLOCATE(DTB%XPAR_PROBOCC)
!
ALLOCATE(DTB%XPAR_BEG_HOLIDAY(KDIM,DTB%NPAR_HOLIDAY,BOP%NBEMCOMP))
DO JSCHED=1,DTB%NPAR_HOLIDAY
   DO JCOMP=1,BOP%NBEMCOMP
      CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
         HPROGRAM,'MAJ','BEG_HOLIDAY','TWN',CFNAM_BEG_HOLIDAY,CFTYP_BEG_HOLIDAY, &
         XUNIF_BEG_HOLIDAY(JSCHED,JCOMP),DTB%XPAR_BEG_HOLIDAY(:,JSCHED,JCOMP), DTB%LDATA_BEG_HOLIDAY )
   ENDDO
ENDDO
IF (.NOT.DTB%LDATA_BEG_HOLIDAY) DEALLOCATE(DTB%XPAR_BEG_HOLIDAY)
!
ALLOCATE(DTB%XPAR_END_HOLIDAY(KDIM,DTB%NPAR_HOLIDAY,BOP%NBEMCOMP))
DO JSCHED=1,DTB%NPAR_HOLIDAY
   DO JCOMP=1,BOP%NBEMCOMP
      CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
         HPROGRAM,'MAJ','END_HOLIDAY','TWN',CFNAM_END_HOLIDAY,CFTYP_END_HOLIDAY, &
         XUNIF_END_HOLIDAY(JSCHED,JCOMP),DTB%XPAR_END_HOLIDAY(:,JSCHED,JCOMP), DTB%LDATA_END_HOLIDAY )
   ENDDO
ENDDO
IF (.NOT.DTB%LDATA_END_HOLIDAY) DEALLOCATE(DTB%XPAR_END_HOLIDAY)
!
ALLOCATE(DTB%XPAR_MOD_HOLIDAY(KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
   CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
     HPROGRAM,'MAJ','MOD_HOLIDAY','TWN',CFNAM_MOD_HOLIDAY, CFTYP_MOD_HOLIDAY, XUNIF_MOD_HOLIDAY(JCOMP), &
     DTB%XPAR_MOD_HOLIDAY(:,JCOMP), DTB%LDATA_MOD_HOLIDAY)
ENDDO
IF (.NOT.DTB%LDATA_MOD_HOLIDAY) DEALLOCATE(DTB%XPAR_MOD_HOLIDAY)
!
ALLOCATE(DTB%XPAR_FSNIG(KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
   CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
     HPROGRAM,'MAJ','FSNIG','TWN',CFNAM_FSNIG, CFTYP_FSNIG, XUNIF_FSNIG(JCOMP), &
     DTB%XPAR_FSNIG(:,JCOMP), DTB%LDATA_FSNIG)
ENDDO
IF (.NOT.DTB%LDATA_FSNIG) DEALLOCATE(DTB%XPAR_FSNIG)
!
ALLOCATE(DTB%XPAR_FVNIG(KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
   CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
      HPROGRAM,'MAJ','FVNIG','TWN',CFNAM_FVNIG, CFTYP_FVNIG, XUNIF_FVNIG(JCOMP), &
      DTB%XPAR_FVNIG(:,JCOMP), DTB%LDATA_FVNIG)
ENDDO
IF (.NOT.DTB%LDATA_FVNIG) DEALLOCATE(DTB%XPAR_FVNIG)
!
ALLOCATE(DTB%XPAR_MODQIN_VCD    (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
   CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
      HPROGRAM,'MAJ','MODQIN_VCD','TWN',CFNAM_MODQIN_VCD, CFTYP_MODQIN_VCD,XUNIF_MODQIN_VCD(JCOMP), &
      DTB%XPAR_MODQIN_VCD(:,JCOMP),DTB%LDATA_MODQIN_VCD)
ENDDO
IF (.NOT.DTB%LDATA_MODQIN_VCD) DEALLOCATE(DTB%XPAR_MODQIN_VCD)
!
ALLOCATE(DTB%XPAR_MODQIN_VLD    (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
   CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
      HPROGRAM,'MAJ','MODQIN_VLD','TWN',CFNAM_MODQIN_VLD, CFTYP_MODQIN_VLD, XUNIF_MODQIN_VLD(JCOMP), &
      DTB%XPAR_MODQIN_VLD(:,JCOMP), DTB%LDATA_MODQIN_VLD)
ENDDO
IF (.NOT.DTB%LDATA_MODQIN_VLD) DEALLOCATE(DTB%XPAR_MODQIN_VLD)
!
ALLOCATE(DTB%XPAR_MODQIN_NIG    (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
   CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
      HPROGRAM,'MAJ','MODQIN_NIG','TWN',CFNAM_MODQIN_NIG, CFTYP_MODQIN_NIG, XUNIF_MODQIN_NIG(JCOMP), &
      DTB%XPAR_MODQIN_NIG(:,JCOMP), DTB%LDATA_MODQIN_NIG)
ENDDO
IF (.NOT.DTB%LDATA_MODQIN_NIG) DEALLOCATE(DTB%XPAR_MODQIN_NIG)
!
ALLOCATE(DTB%XPAR_TDESV(KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','TDESV','TWN',CFNAM_TDESV, CFTYP_TDESV, & 
   XUNIF_TDESV,DTB%XPAR_TDESV, DTB%LDATA_TDESV)
IF (.NOT.DTB%LDATA_TDESV) DEALLOCATE(DTB%XPAR_TDESV)
!
ALLOCATE(DTB%XPAR_WIN_SW_MAX(KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','WIN_SW_MAX','TWN',CFNAM_WIN_SW_MAX, CFTYP_WIN_SW_MAX, XUNIF_WIN_SW_MAX, &
   DTB%XPAR_WIN_SW_MAX, DTB%LDATA_WIN_SW_MAX)
IF (.NOT.DTB%LDATA_WIN_SW_MAX) DEALLOCATE(DTB%XPAR_WIN_SW_MAX)
!
ALLOCATE(DTB%XPAR_FOPEN(KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
    HPROGRAM,'MAJ','FOPEN','TWN',CFNAM_FOPEN,CFTYP_FOPEN,XUNIF_FOPEN, &
    DTB%XPAR_FOPEN(:),DTB%LDATA_FOPEN)
IF (.NOT.DTB%LDATA_FOPEN) DEALLOCATE(DTB%XPAR_FOPEN)
!
ALLOCATE(DTB%XPAR_FVSUM(KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','FVSUM','TWN',CFNAM_FVSUM,CFTYP_FVSUM,XUNIF_FVSUM(JCOMP), &
   DTB%XPAR_FVSUM(:,JCOMP),DTB%LDATA_FVSUM)
ENDDO
IF (.NOT.DTB%LDATA_FVSUM) DEALLOCATE(DTB%XPAR_FVSUM)
!
ALLOCATE(DTB%XPAR_FVVAC(KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','FVVAC','TWN',CFNAM_FVVAC,CFTYP_FVVAC,XUNIF_FVVAC(JCOMP), &
   DTB%XPAR_FVVAC(:,JCOMP),DTB%LDATA_FVVAC)
ENDDO
IF (.NOT.DTB%LDATA_FVVAC) DEALLOCATE(DTB%XPAR_FVVAC)
!
ALLOCATE(DTB%XPAR_FSSUM(KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','FSSUM','TWN',CFNAM_FSSUM,CFTYP_FSSUM,XUNIF_FSSUM(JCOMP), &
   DTB%XPAR_FSSUM(:,JCOMP),DTB%LDATA_FSSUM)
ENDDO
IF (.NOT.DTB%LDATA_FSSUM) DEALLOCATE(DTB%XPAR_FSSUM)
!
ALLOCATE(DTB%XPAR_FSVAC(KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','FSVAC','TWN',CFNAM_FSVAC,CFTYP_FSVAC,XUNIF_FSVAC(JCOMP), &
   DTB%XPAR_FSVAC(:,JCOMP),DTB%LDATA_FSVAC)
ENDDO
IF (.NOT.DTB%LDATA_FSVAC) DEALLOCATE(DTB%XPAR_FSVAC)
!
ALLOCATE(DTB%XPAR_QIN           (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','QIN','TWN',CFNAM_QIN, CFTYP_QIN, XUNIF_QIN(JCOMP), &
   DTB%XPAR_QIN(:,JCOMP), DTB%LDATA_QIN)
ENDDO
IF (.NOT.DTB%LDATA_QIN) DEALLOCATE(DTB%XPAR_QIN)
!
ALLOCATE(DTB%XPAR_F_WASTE_CAN   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','F_WASTE_CAN','TWN',CFNAM_F_WASTE_CAN, CFTYP_F_WASTE_CAN, XUNIF_F_WASTE_CAN, &
        DTB%XPAR_F_WASTE_CAN, DTB%LDATA_F_WASTE_CAN)
IF (.NOT.DTB%LDATA_F_WASTE_CAN) DEALLOCATE(DTB%XPAR_F_WASTE_CAN)
!
ALLOCATE(DTB%XPAR_QIN_FRAD      (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','QIN_FRAD','TWN',CFNAM_QIN_FRAD, CFTYP_QIN_FRAD, XUNIF_QIN_FRAD, &
        DTB%XPAR_QIN_FRAD, DTB%LDATA_QIN_FRAD)
IF (.NOT.DTB%LDATA_QIN_FRAD) DEALLOCATE(DTB%XPAR_QIN_FRAD)
!
ALLOCATE(DTB%XPAR_SHGC          (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','SHGC','TWN',CFNAM_SHGC, CFTYP_SHGC, XUNIF_SHGC, DTB%XPAR_SHGC, DTB%LDATA_SHGC)
IF (.NOT.DTB%LDATA_SHGC) DEALLOCATE(DTB%XPAR_SHGC)
!
ALLOCATE(DTB%XPAR_U_WIN         (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','U_WIN','TWN',CFNAM_U_WIN, CFTYP_U_WIN, XUNIF_U_WIN, DTB%XPAR_U_WIN, DTB%LDATA_U_WIN)
IF (.NOT.DTB%LDATA_U_WIN) DEALLOCATE(DTB%XPAR_U_WIN)
!
ALLOCATE(DTB%XPAR_GR            (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','GR','TWN',CFNAM_GR, CFTYP_GR, XUNIF_GR, DTB%XPAR_GR, DTB%LDATA_GR)
IF (.NOT.DTB%LDATA_GR) DEALLOCATE(DTB%XPAR_GR)
!
ALLOCATE(DTB%XPAR_SHGC_SH       (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','SHGC_SH','TWN',CFNAM_SHGC_SH, CFTYP_SHGC_SH, XUNIF_SHGC_SH, &
        DTB%XPAR_SHGC_SH, DTB%LDATA_SHGC_SH)
IF (.NOT.DTB%LDATA_SHGC_SH) DEALLOCATE(DTB%XPAR_SHGC_SH)
!
ALLOCATE(DTB%XPAR_FLOOR_HEIGHT  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','FLOOR_HEIGHT','TWN',CFNAM_FLOOR_HEIGHT, CFTYP_FLOOR_HEIGHT, XUNIF_FLOOR_HEIGHT, &
        DTB%XPAR_FLOOR_HEIGHT, DTB%LDATA_FLOOR_HEIGHT)
IF (.NOT.DTB%LDATA_FLOOR_HEIGHT) DEALLOCATE(DTB%XPAR_FLOOR_HEIGHT)
!
ALLOCATE(DTB%XPAR_N50           (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','N50','TWN',CFNAM_N50, CFTYP_N50, XUNIF_N50, DTB%XPAR_N50, DTB%LDATA_N50)
IF (.NOT.DTB%LDATA_N50) DEALLOCATE(DTB%XPAR_N50)
!
ALLOCATE(DTB%XPAR_F_WATER_COND (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','F_WATER_COND','TWN',CFNAM_F_WATER_COND, CFTYP_F_WATER_COND, XUNIF_F_WATER_COND, &
        DTB%XPAR_F_WATER_COND, DTB%LDATA_F_WATER_COND)
IF (.NOT.DTB%LDATA_F_WATER_COND) DEALLOCATE(DTB%XPAR_F_WATER_COND)
!
ALLOCATE(DTB%XPAR_DCS_AREA (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','DCS_AREA','TWN',CFNAM_DCS_AREA, CFTYP_DCS_AREA, XUNIF_DCS_AREA, &
        DTB%XPAR_DCS_AREA, DTB%LDATA_DCS_AREA)
IF (.NOT.DTB%LDATA_DCS_AREA) DEALLOCATE(DTB%XPAR_DCS_AREA)
!
ALLOCATE(DTB%XPAR_TCOOL_OCCD   (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','TCOOL_OCCD','TWN',CFNAM_TCOOL_OCCD, CFTYP_TCOOL_OCCD, XUNIF_TCOOL_OCCD(JCOMP), &
   DTB%XPAR_TCOOL_OCCD(:,JCOMP), DTB%LDATA_TCOOL_OCCD)
ENDDO
IF (.NOT.DTB%LDATA_TCOOL_OCCD) DEALLOCATE(DTB%XPAR_TCOOL_OCCD)
!
ALLOCATE(DTB%XPAR_TCOOL_OCCN   (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','TCOOL_OCCN','TWN',CFNAM_TCOOL_OCCN, CFTYP_TCOOL_OCCN, XUNIF_TCOOL_OCCN(JCOMP), &
   DTB%XPAR_TCOOL_OCCN(:,JCOMP), DTB%LDATA_TCOOL_OCCN)
ENDDO
IF (.NOT.DTB%LDATA_TCOOL_OCCN) DEALLOCATE(DTB%XPAR_TCOOL_OCCN)
!
ALLOCATE(DTB%XPAR_TCOOL_VCDD   (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','TCOOL_VCDD','TWN',CFNAM_TCOOL_VCDD, CFTYP_TCOOL_VCDD, XUNIF_TCOOL_VCDD(JCOMP), &
   DTB%XPAR_TCOOL_VCDD(:,JCOMP), DTB%LDATA_TCOOL_VCDD)
ENDDO
IF (.NOT.DTB%LDATA_TCOOL_VCDD) DEALLOCATE(DTB%XPAR_TCOOL_VCDD)
!
ALLOCATE(DTB%XPAR_TCOOL_VCDN   (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','TCOOL_VCDN','TWN',CFNAM_TCOOL_VCDN, CFTYP_TCOOL_VCDN, XUNIF_TCOOL_VCDN(JCOMP), &
   DTB%XPAR_TCOOL_VCDN(:,JCOMP), DTB%LDATA_TCOOL_VCDN)
ENDDO
IF (.NOT.DTB%LDATA_TCOOL_VCDN) DEALLOCATE(DTB%XPAR_TCOOL_VCDN)
!
ALLOCATE(DTB%XPAR_TCOOL_VCLD   (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','TCOOL_VCLD','TWN',CFNAM_TCOOL_VCLD, CFTYP_TCOOL_VCLD, XUNIF_TCOOL_VCLD(JCOMP), &
   DTB%XPAR_TCOOL_VCLD(:,JCOMP), DTB%LDATA_TCOOL_VCLD)
ENDDO
IF (.NOT.DTB%LDATA_TCOOL_VCLD) DEALLOCATE(DTB%XPAR_TCOOL_VCLD)
!
ALLOCATE(DTB%XPAR_THEAT_OCCD   (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','THEAT_OCCD','TWN',CFNAM_THEAT_OCCD, CFTYP_THEAT_OCCD, XUNIF_THEAT_OCCD(JCOMP), &
   DTB%XPAR_THEAT_OCCD(:,JCOMP), DTB%LDATA_THEAT_OCCD)
ENDDO
IF (.NOT.DTB%LDATA_THEAT_OCCD) DEALLOCATE(DTB%XPAR_THEAT_OCCD)
!
ALLOCATE(DTB%XPAR_THEAT_OCCN   (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','THEAT_OCCN','TWN',CFNAM_THEAT_OCCN, CFTYP_THEAT_OCCN, XUNIF_THEAT_OCCN(JCOMP), &
   DTB%XPAR_THEAT_OCCN(:,JCOMP), DTB%LDATA_THEAT_OCCN)
ENDDO
IF (.NOT.DTB%LDATA_THEAT_OCCN) DEALLOCATE(DTB%XPAR_THEAT_OCCN)
!
ALLOCATE(DTB%XPAR_THEAT_VCDD   (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','THEAT_VCDD','TWN',CFNAM_THEAT_VCDD, CFTYP_THEAT_VCDD, XUNIF_THEAT_VCDD(JCOMP), &
   DTB%XPAR_THEAT_VCDD(:,JCOMP), DTB%LDATA_THEAT_VCDD)
ENDDO
IF (.NOT.DTB%LDATA_THEAT_VCDD) DEALLOCATE(DTB%XPAR_THEAT_VCDD)
!
ALLOCATE(DTB%XPAR_THEAT_VCDN   (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','THEAT_VCDN','TWN',CFNAM_THEAT_VCDN, CFTYP_THEAT_VCDN, XUNIF_THEAT_VCDN(JCOMP), &
   DTB%XPAR_THEAT_VCDN(:,JCOMP), DTB%LDATA_THEAT_VCDN)
ENDDO
IF (.NOT.DTB%LDATA_THEAT_VCDN) DEALLOCATE(DTB%XPAR_THEAT_VCDN)
!
ALLOCATE(DTB%XPAR_THEAT_VCLD   (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'MAJ','THEAT_VCLD','TWN',CFNAM_THEAT_VCLD, CFTYP_THEAT_VCLD, XUNIF_THEAT_VCLD(JCOMP), &
   DTB%XPAR_THEAT_VCLD(:,JCOMP), DTB%LDATA_THEAT_VCLD)
ENDDO
IF (.NOT.DTB%LDATA_THEAT_VCLD) DEALLOCATE(DTB%XPAR_THEAT_VCLD)
!
ALLOCATE(DTB%XPAR_QIN_FLAT     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','QIN_FLAT','TWN',CFNAM_QIN_FLAT, CFTYP_QIN_FLAT, XUNIF_QIN_FLAT, &
        DTB%XPAR_QIN_FLAT, DTB%LDATA_QIN_FLAT)
IF (.NOT.DTB%LDATA_QIN_FLAT) DEALLOCATE(DTB%XPAR_QIN_FLAT)
!
ALLOCATE(DTB%XPAR_HR_TARGET    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','HR_TARGET','TWN',CFNAM_HR_TARGET, CFTYP_HR_TARGET, XUNIF_HR_TARGET, &
        DTB%XPAR_HR_TARGET, DTB%LDATA_HR_TARGET)
IF (.NOT.DTB%LDATA_HR_TARGET) DEALLOCATE(DTB%XPAR_HR_TARGET)
!
ALLOCATE(DTB%XPAR_T_SIZE_MAX   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','T_SIZE_MAX','TWN',CFNAM_T_SIZE_MAX, CFTYP_T_SIZE_MAX, XUNIF_T_SIZE_MAX, &
        DTB%XPAR_T_SIZE_MAX, DTB%LDATA_T_SIZE_MAX)
IF (.NOT.DTB%LDATA_T_SIZE_MAX) DEALLOCATE(DTB%XPAR_T_SIZE_MAX)
!
ALLOCATE(DTB%XPAR_T_SIZE_MIN   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','T_SIZE_MIN','TWN',CFNAM_T_SIZE_MIN, CFTYP_T_SIZE_MIN, XUNIF_T_SIZE_MIN, &
        DTB%XPAR_T_SIZE_MIN, DTB%LDATA_T_SIZE_MIN)
IF (.NOT.DTB%LDATA_T_SIZE_MIN) DEALLOCATE(DTB%XPAR_T_SIZE_MIN)
!
ALLOCATE(DTB%XPAR_CAP_SYS_HEAT (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','CAP_SYS_HEAT','TWN',CFNAM_CAP_SYS_HEAT, CFTYP_CAP_SYS_HEAT, XUNIF_CAP_SYS_HEAT, &
        DTB%XPAR_CAP_SYS_HEAT, DTB%LDATA_CAP_SYS_HEAT)
IF (.NOT.DTB%LDATA_CAP_SYS_HEAT) DEALLOCATE(DTB%XPAR_CAP_SYS_HEAT)
!
ALLOCATE(DTB%XPAR_CAP_SYS_RAT  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','CAP_SYS_RAT','TWN',CFNAM_CAP_SYS_RAT, CFTYP_CAP_SYS_RAT, XUNIF_CAP_SYS_RAT, &
        DTB%XPAR_CAP_SYS_RAT, DTB%LDATA_CAP_SYS_RAT)
IF (.NOT.DTB%LDATA_CAP_SYS_RAT) DEALLOCATE(DTB%XPAR_CAP_SYS_RAT)
!
ALLOCATE(DTB%XPAR_M_SYS_RAT    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','M_SYS_RAT','TWN',CFNAM_M_SYS_RAT, CFTYP_M_SYS_RAT, XUNIF_M_SYS_RAT, &
        DTB%XPAR_M_SYS_RAT, DTB%LDATA_M_SYS_RAT)
IF (.NOT.DTB%LDATA_M_SYS_RAT) DEALLOCATE(DTB%XPAR_M_SYS_RAT)
  !
IF (BOP%LAUTOSIZE) THEN
  IF (DTB%LDATA_CAP_SYS_HEAT .OR. DTB%LDATA_CAP_SYS_RAT .OR. DTB%LDATA_M_SYS_RAT) THEN
    WRITE(ILUOUT,*) '==> You choose LAUTOSIZE=T <=='
    WRITE(ILUOUT,*) 'Therefore HVAC systems characteristics will be computed automatically'
    IF (DTB%LDATA_CAP_SYS_HEAT) THEN
      WRITE(ILUOUT,*) 'Data you provided for CAP_SYS_HEAT are then discarded.'
      DEALLOCATE(DTB%XPAR_CAP_SYS_HEAT)
    END IF
    IF (DTB%LDATA_CAP_SYS_RAT ) THEN
      WRITE(ILUOUT,*) 'Data you provided for CAP_SYS_RAT  are then discarded.'
      DEALLOCATE(DTB%XPAR_CAP_SYS_RAT)
    END IF
    IF (DTB%LDATA_M_SYS_RAT   ) THEN
      WRITE(ILUOUT,*) 'Data you provided for M_SYS_RAT    are then discarded.'
      DEALLOCATE(DTB%XPAR_M_SYS_RAT)
    END IF
  END IF
  DTB%LDATA_CAP_SYS_HEAT = .FALSE.
  DTB%LDATA_CAP_SYS_RAT  = .FALSE.
  DTB%LDATA_M_SYS_RAT    = .FALSE.
ELSE
  IF (DTB%LDATA_T_SIZE_MAX .OR. DTB%LDATA_T_SIZE_MAX) THEN
    WRITE(ILUOUT,*) '==> You choose LAUTOSIZE=F <=='
    WRITE(ILUOUT,*) 'Therefore HVAC systems characteristics are specified'
    WRITE(ILUOUT,*) 'and you do not need the minimal and maximum temperatures'
    WRITE(ILUOUT,*) 'that would be used if you have chosen an automatic calibration.'
    IF (DTB%LDATA_T_SIZE_MAX) THEN
      WRITE(ILUOUT,*) 'Data you provided for T_SIZE_MAX are then discarded.'
      DEALLOCATE(DTB%XPAR_T_SIZE_MAX)
    END IF
    IF (DTB%LDATA_T_SIZE_MIN) THEN
      WRITE(ILUOUT,*) 'Data you provided for T_SIZE_MIN are then discarded.'
      DEALLOCATE(DTB%XPAR_T_SIZE_MIN)
    END IF
  END IF
  DTB%LDATA_T_SIZE_MAX = .FALSE.
  DTB%LDATA_T_SIZE_MIN = .FALSE.
END IF
!
ALLOCATE(DTB%XPAR_T_ADP        (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','T_ADP','TWN',CFNAM_T_ADP, CFTYP_T_ADP, XUNIF_T_ADP, DTB%XPAR_T_ADP, DTB%LDATA_T_ADP)
IF (.NOT.DTB%LDATA_T_ADP) DEALLOCATE(DTB%XPAR_T_ADP)
!
ALLOCATE(DTB%XPAR_COP_RAT      (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','COP_RAT','TWN',CFNAM_COP_RAT, CFTYP_COP_RAT, XUNIF_COP_RAT, &
        DTB%XPAR_COP_RAT, DTB%LDATA_COP_RAT)
IF (.NOT.DTB%LDATA_COP_RAT) DEALLOCATE(DTB%XPAR_COP_RAT)
!
ALLOCATE(DTB%XPAR_COP_DCS      (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','COP_DCS','TWN',CFNAM_COP_DCS, CFTYP_COP_DCS, XUNIF_COP_DCS, &
        DTB%XPAR_COP_DCS, DTB%LDATA_COP_DCS)
IF (.NOT.DTB%LDATA_COP_DCS) DEALLOCATE(DTB%XPAR_COP_DCS)
!
!
!Add cases for heating fractions (for CO2 flux calculus)

ALLOCATE(DTB%XPAR_FRAC_HEAT_ELEC  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','FRAC_HEAT_ELEC','TWN',CFNAM_FRAC_HEAT_ELEC, CFTYP_FRAC_HEAT_ELEC, XUNIF_FRAC_HEAT_ELEC, &
        DTB%XPAR_FRAC_HEAT_ELEC, DTB%LDATA_FRAC_HEAT_ELEC)
!
ALLOCATE(DTB%XPAR_FRAC_HEAT_GAS   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','FRAC_HEAT_GAS','TWN',CFNAM_FRAC_HEAT_GAS, CFTYP_FRAC_HEAT_GAS, XUNIF_FRAC_HEAT_GAS, &
        DTB%XPAR_FRAC_HEAT_GAS, DTB%LDATA_FRAC_HEAT_GAS)
!
ALLOCATE(DTB%XPAR_FRAC_HEAT_FUEL  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','FRAC_HEAT_FUEL','TWN',CFNAM_FRAC_HEAT_FUEL, CFTYP_FRAC_HEAT_FUEL, XUNIF_FRAC_HEAT_FUEL, &
        DTB%XPAR_FRAC_HEAT_FUEL, DTB%LDATA_FRAC_HEAT_FUEL)
!
ALLOCATE(DTB%XPAR_FRAC_HEAT_OTHER (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                      HPROGRAM,'ARI','FRAC_HEAT_OTHER','TWN',CFNAM_FRAC_HEAT_OTHER, CFTYP_FRAC_HEAT_OTHER, XUNIF_FRAC_HEAT_OTHER, &
        DTB%XPAR_FRAC_HEAT_OTHER, DTB%LDATA_FRAC_HEAT_OTHER)
!
! F_HW_GAS
!
ALLOCATE(DTB%XPAR_F_HW_GAS (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                      HPROGRAM,'ARI','F_HW_GAS','TWN',CFNAM_F_HW_GAS, CFTYP_F_HW_GAS, XUNIF_F_HW_GAS, &
        DTB%XPAR_F_HW_GAS, DTB%LDATA_F_HW_GAS)
IF (.NOT.DTB%LDATA_F_HW_GAS) DEALLOCATE(DTB%XPAR_F_HW_GAS)
!
!  HOTWAT
!
ALLOCATE(DTB%XPAR_HOTWAT  (KDIM,BOP%NBEMCOMP))
DO JCOMP=1,BOP%NBEMCOMP
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
   HPROGRAM,'ARI','HOTWAT','TWN',CFNAM_HOTWAT, CFTYP_HOTWAT, XUNIF_HOTWAT(JCOMP), &
   DTB%XPAR_HOTWAT(:,JCOMP), DTB%LDATA_HOTWAT)
ENDDO
IF (.NOT.DTB%LDATA_HOTWAT) DEALLOCATE(DTB%XPAR_HOTWAT)
!
!
!-------------------------------------------------------------------------------
!
!* coherence checks
!
 CALL COHERENCE_THERMAL_DATA_FL('FLOOR',DTB%LDATA_HC_FLOOR,DTB%LDATA_TC_FLOOR,DTB%LDATA_D_FLOOR)
 CALL COHERENCE_THERMAL_DATA_FL('MASS ',DTB%LDATA_HC_MASS ,DTB%LDATA_TC_MASS ,DTB%LDATA_D_MASS )
 CALL COHERENCE_FRAC_HEAT(HPROGRAM, DTB, KDIM)
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PGD_BEM_PAR',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
CONTAINS
SUBROUTINE COHERENCE_THERMAL_DATA_FL(HTYPE,ODATA_HC,ODATA_TC,ODATA_D)
 CHARACTER(LEN=5), INTENT(IN) :: HTYPE
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
END SUBROUTINE COHERENCE_THERMAL_DATA_FL
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_BEM_PAR
