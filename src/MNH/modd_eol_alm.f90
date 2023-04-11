!MNH_LIC Copyright 2017-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!!
!!    #####################
      MODULE MODD_EOL_ALM
!!    #####################
!!
!!*** *MODD_EOL_ALM*
!!
!!    PURPOSE
!!    -------
!!       It is possible to include wind turbines parameterization in Meso-NH,
!!       and several models are available. One of the models is the Actuator 
!!       Line Method (ALM). MODD_EOL_ALM contains all the declarations for
!!       the ALM model. 
!!
!!
!!**  AUTHOR
!!    ------
!!    PA.Joulin                   *CNRM & IFPEN*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 04/01/17
!!    Modification 14/10/20 (PA. Joulin) Updated for a main version
!!    Modification    04/23 (PA. Joulin) variables moved in modd_eol_shared_io
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
!
IMPLICIT NONE
!
! ------ TYPES ------
!
TYPE FARM
        INTEGER                                      :: NNB_TURBINES ! Number of wind turbines [-]
        REAL,              DIMENSION(:), ALLOCATABLE :: XPOS_X       ! Tower base position, X coord [m]
        REAL,              DIMENSION(:), ALLOCATABLE :: XPOS_Y       ! Tower base position, Y coord [m]
        REAL,              DIMENSION(:), ALLOCATABLE :: XOMEGA       ! Rotor rotation speed [rad/s]
        REAL,              DIMENSION(:), ALLOCATABLE :: XNAC_YAW     ! Nacelle yaw angle [rad]
        REAL,              DIMENSION(:), ALLOCATABLE :: XBLA_PITCH   ! Blade picth angle [rad]
END TYPE FARM
!
TYPE TURBINE 
        CHARACTER(LEN=10)                            :: CNAME        ! Nom de la turbine [-]
        INTEGER                                      :: NNB_BLADES   ! Number of blades [-]
        REAL                                         :: XH_HEIGHT    ! Hub height [m]
        REAL                                         :: XH_DEPORT    ! Hub deport [m]
        REAL                                         :: XNAC_TILT    ! Tilt of the nacelle [m]
        REAL                                         :: XR_MIN       ! Minimum blade radius [m]
        REAL                                         :: XR_MAX       ! Maximum blade radius [m]
END TYPE TURBINE
!
TYPE BLADE 
        INTEGER                                      :: NNB_BLAELT   ! Number of blade element
        INTEGER                                      :: NNB_BLADAT   ! Number of blade data
        REAL,              DIMENSION(:), ALLOCATABLE :: XRAD         ! Data node radius [m]
        REAL,              DIMENSION(:), ALLOCATABLE :: XCHORD       ! Element chord [m]
        REAL,              DIMENSION(:), ALLOCATABLE :: XTWIST       ! Element twist [rad]
        CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: CAIRFOIL     ! Arifoil name [-]
END TYPE BLADE
!
TYPE AIRFOIL 
        CHARACTER(LEN=15)                            :: CNAME        ! Airfoil name [-]
        REAL,              DIMENSION(:), ALLOCATABLE :: XAA          ! Attack Angle [rad]
        REAL,              DIMENSION(:), ALLOCATABLE :: XRE          ! Reynolds Number [-]
        REAL,              DIMENSION(:), ALLOCATABLE :: XCL          ! Lift coefficient [-]
        REAL,              DIMENSION(:), ALLOCATABLE :: XCD          ! Drag coefficient [-]
        REAL,              DIMENSION(:), ALLOCATABLE :: XCM          ! Moment coefficient [-]
END TYPE AIRFOIL
!
! ------ VARIABLES ------
! --- Farm data ---
TYPE(FARM)                                           :: TFARM
TYPE(TURBINE)                                        :: TTURBINE
TYPE(BLADE)                                          :: TBLADE
TYPE(AIRFOIL), DIMENSION(:), ALLOCATABLE             :: TAIRFOIL 
!
! --- Global variables (Code & CPU) --- 
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: XFAERO_RE_GLB ! Aerodyn. force (lift+drag) in RE [N]
!
! Mean values
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: XFAERO_RE_SUM  ! Sum of aerodyn. force (lift+drag) in RE [N]
!
! Implicit from MODD_EOL_SHARED_IO :
!REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XELT_RAD      ! Blade elements radius [m]
!REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XAOA_GLB      ! Angle of attack of an element [rad]
!REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XFLIFT_GLB    ! Lift force, parallel to Urel [N]
!REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XFDRAG_GLB    ! Drag force, perpendicular to Urel [N]
!REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: XFAERO_RG_GLB ! Aerodyn. force (lift+drag) in RG [N]
!
!REAL, DIMENSION(:),       ALLOCATABLE :: XTHRUT         ! Thrust [N]
!REAL, DIMENSION(:),       ALLOCATABLE :: XTORQT         ! Torque [Nm]
!REAL, DIMENSION(:),       ALLOCATABLE :: XPOWT          ! Power [W]
!
!REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XAOA_SUM       ! Sum of angle of attack [rad]
!REAL, DIMENSION(:),       ALLOCATABLE :: XTHRU_SUM      ! Sum of thrust (N)
!REAL, DIMENSION(:),       ALLOCATABLE :: XTORQ_SUM      ! Sum of torque (Nm)
!REAL, DIMENSION(:),       ALLOCATABLE :: XPOW_SUM       ! Sum of power (W)
!
!
! --- Namelist NAM_EOL_ALM ---
! Implicit from MODD_EOL_SHARED_IO :
!CHARACTER(LEN=100) :: CFARM_CSVDATA     ! Farm file to read 
!CHARACTER(LEN=100) :: CTURBINE_CSVDATA  ! Turbine file to read  
!CHARACTER(LEN=100) :: CBLADE_CSVDATA    ! Blade file to read  
!CHARACTER(LEN=100) :: CAIRFOIL_CSVDATA  ! Airfoil file to read  
!CHARACTER(LEN=3)   :: CINTERP           ! Interpolation method for wind speed
!LOGICAL            :: LTIPLOSSG         ! Flag to apply Glauert's tip loss correction
!LOGICAL            :: LTECOUTPTS        ! Flag to get Tecplot file output of element points
!LOGICAL            :: LCSVOUTFRM        ! Flag to get CSV files output of frames
!
INTEGER            :: NNB_BLAELT        ! Number of blade elements
!
LOGICAL            :: LTIMESPLIT        ! Flag to apply Time splitting method
!
END MODULE MODD_EOL_ALM
