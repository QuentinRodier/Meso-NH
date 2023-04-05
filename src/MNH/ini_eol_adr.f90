!MNH_LIC Copyright 2018-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_INI_EOL_ADR
!     ##########################
!
INTERFACE
!
SUBROUTINE INI_EOL_ADR(PDXX,PDYY)
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDXX,PDYY    ! mesh size
!
END SUBROUTINE INI_EOL_ADR                  
!
END INTERFACE
!
END MODULE MODI_INI_EOL_ADR
!
!     ############################################################
      SUBROUTINE INI_EOL_ADR(PDXX,PDYY)
!     ############################################################
!
!!****  *INI_EOL_ADR* - routine to initialize the Actuator Line Model 
!!                      to simulate wind turbines      
!!
!!    PURPOSE
!!    -------
!!     Routine to initialized the ADR (wind turbine model) variables
!!      
!!**  METHOD
!!    ------
!!     
!!    EXTERNAL
!!    --------   
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    *MODD_EOL_SHARED_IO:
!!    *Namelist NAM_EOL_ADNR (INPUT) :
!!    CHARACTER(LEN=100)              :: CFARM_CSVDATA     ! File to read, with farm data
!!    CHARACTER(LEN=100)              :: CTURBINE_CSVDATA  ! File to read, turbine data
!!    CHARACTER(LEN=100)              :: CBLADE_CSVDATA    ! Blade file to read  
!!    CHARACTER(LEN=100)              :: CAIRFOIL_CSVDATA  ! Airfoil file to read  
!!    *for ouputs :
!!    REAL, DIMENSION(:), ALLOCATABLE :: XTHRUT        ! Thrust [N]
!!    REAL, DIMENSION(:), ALLOCATABLE :: XTORQT        ! Torque [Nm]
!!    REAL, DIMENSION(:), ALLOCATABLE :: XPOWT         ! Power [W]
!!    REAL, DIMENSION(:), ALLOCATABLE :: XTHRU_SUM     ! Sum of thrust (N)
!!    REAL, DIMENSION(:), ALLOCATABLE :: XTORQ_SUM     ! Sum of torque (Nm)
!!    REAL, DIMENSION(:), ALLOCATABLE :: XPOW_SUM      ! Sum of power (W)
!!
!!    INTEGER            :: NNB_RADELT        ! Number of radial elements
!!    INTEGER            :: NNB_AZIELT        ! Number of azimutal elements
!!    
!!    *MODD_EOL_ADR (OUTPUT):
!!    TYPE(FARM)                               :: TFARM
!!    TYPE(TURBINE)                            :: TTURBINE
!!    TYPE(BLADE)                              :: TBLADE
!!    TYPE(AIRFOIL), DIMENSION(:), ALLOCATABLE :: TAIRFOIL
!!    REAL, DIMENSION(:,:,:),      ALLOCATABLE :: XELT_RAD      ! Blade elements radius [m]
!!    REAL, DIMENSION(:,:,:),      ALLOCATABLE :: XAOA_GLB      ! Angle of attack of an element [rad]
!!    REAL, DIMENSION(:,:,:),      ALLOCATABLE :: XFLIFT_GLB    ! Lift force, parallel to Urel [N]
!!    REAL, DIMENSION(:,:,:),      ALLOCATABLE :: XFDRAG_GLB    ! Drag force, perpendicular to Urel [N]
!!    REAL, DIMENSION(:,:,:,:),    ALLOCATABLE :: XFAERO_REA_GLB ! Aerodyn. force (lift+drag) in RE [N]
!!    REAL, DIMENSION(:,:,:,:),    ALLOCATABLE :: XFAERO_RG_GLB ! Aerodyn. force (lift+drag) in RG [N]
!!    REAL, DIMENSION(:,:,:),      ALLOCATABLE :: XAOA_SUM      ! Sum of angle of attack [rad]
!!    REAL, DIMENSION(:,:,:,:),    ALLOCATABLE :: XFAERO_REA_SUM ! Sum of aerodyn. force (lift+drag) in RE [N]
!
!!    *MODD_EOL_KINEMATICS
!!    Positions
!!    Orientations
!!    Velocities
!!
!!    REFERENCE
!!    ---------
!!       
!!
!!    AUTHOR
!!    ------
!!	PA. Joulin      * Meteo France & IFPEN *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        31/05/18
!!      Modification    10/11/20 (PA. Joulin) Updated for a main version
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------ 
!
!*       0.1    Modules
!
USE MODD_EOL_ADR
USE MODD_EOL_SHARED_IO, ONLY: CFARM_CSVDATA
USE MODD_EOL_SHARED_IO, ONLY: CTURBINE_CSVDATA
USE MODD_EOL_SHARED_IO, ONLY: CBLADE_CSVDATA
USE MODD_EOL_SHARED_IO, ONLY: CAIRFOIL_CSVDATA
USE MODD_EOL_SHARED_IO, ONLY: XTHRUT, XTORQT, XPOWT
USE MODD_EOL_SHARED_IO, ONLY: XTHRU_SUM, XTORQ_SUM, XPOW_SUM
USE MODD_EOL_SHARED_IO, ONLY: XELT_RAD
USE MODD_EOL_SHARED_IO, ONLY: XAOA_GLB, XFLIFT_GLB, XFDRAG_GLB, XFAERO_RG_GLB
USE MODD_EOL_SHARED_IO, ONLY: XAOA_SUM 
USE MODI_EOL_READER,    ONLY: READ_CSVDATA_FARM_ADR
USE MODI_EOL_READER,    ONLY: READ_CSVDATA_TURBINE_ADR
USE MODI_EOL_READER,    ONLY: READ_CSVDATA_BLADE_ADR
USE MODI_EOL_READER,    ONLY: READ_CSVDATA_AIRFOIL_ADR
USE MODI_EOL_PRINTER,   ONLY: PRINT_DATA_FARM_ADR
USE MODI_EOL_PRINTER,   ONLY: PRINT_DATA_TURBINE_ADR
USE MODI_EOL_PRINTER,   ONLY: PRINT_DATA_BLADE_ADR
USE MODI_EOL_PRINTER,   ONLY: PRINT_DATA_AIRFOIL_ADR
USE MODD_EOL_KINE_ADR
USE MODI_EOL_MATHS
! To print in output listing
USE MODD_LUNIT_n,       ONLY: TLUOUT
! Constant
USE MODD_CST,           ONLY: XPI
! To know the grid
USE MODD_GRID_n,        ONLY: XXHAT,XYHAT,XZS
USE MODE_ll,            ONLY: GET_INDICE_ll
USE MODD_PARAMETERS,    ONLY: JPVEXT
! MPI stuffs
USE MODD_VAR_ll,        ONLY: NMNH_COMM_WORLD
USE MODD_PRECISION,     ONLY: MNHREAL_MPI
USE MODD_MPIF,          ONLY: MPI_SUM
!
!*       0.2    Variables
!
IMPLICIT NONE
! Interface
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDXX,PDYY    ! mesh size
!
! Some loop controlers 
! .. for wind turbines
INTEGER  :: JROT                    ! Rotor index
INTEGER  :: JTELT                   ! Tower element index
INTEGER  :: JNELT                   ! Nacelle element index
INTEGER  :: JRAD                    ! Radius index
INTEGER  :: JAZI                    ! Azimut index
! .. for domain
INTEGER  :: IIB,IJB,IKB             ! Begin of a CPU domain
INTEGER  :: IIE,IJE                 ! End of a CPU domain
INTEGER  :: JI,JJ                   ! Domain index
! Some variables to be coder-friendly
INTEGER  :: INB_WT, INB_B           ! Total numbers of wind turbines, blade
INTEGER  :: INB_RELT, INB_AELT      ! Total numbers of radial elt and azimut elt
INTEGER  :: INB_TELT, INB_NELT      ! Total numbers of tower elt and nacelle elt
REAL     :: ZRAD                    ! Radius along the blade 
! Tower base folowing the terrain
REAL,DIMENSION(:,:),ALLOCATABLE :: ZPOSINI_TOWO_RG ! Initial tower origin position
INTEGER  :: IINFO                   ! code info return
!
!-------------------------------------------------------------------
!
!*       1.    READING AND ALLOCATING DATA
!              --------------------------- 
! Reading in csv files
! Allocation of TFARM, TTURBINE, TBLADE and TAIRFOILS inside the function
!
!*       1.1    Wind farm data
!
CALL READ_CSVDATA_FARM_ADR(TRIM(CFARM_CSVDATA),TFARM)
!
!*       1.2    Wind turbine data
!
CALL READ_CSVDATA_TURBINE_ADR(TRIM(CTURBINE_CSVDATA),TTURBINE)
!
!*       1.3    Blade data
!
CALL READ_CSVDATA_BLADE_ADR(TRIM(CBLADE_CSVDATA),TTURBINE,TBLADE)
!
!*       1.4    Airfoil data
!
CALL READ_CSVDATA_AIRFOIL_ADR(TRIM(CAIRFOIL_CSVDATA),TBLADE,TAIRFOIL)
!
!
!-------------------------------------------------------------------
!
!*       2.    PRINTING DATA 
!              ------------- 
!
!*       2.1    Wind farm data
!
CALL PRINT_DATA_FARM_ADR(TLUOUT%NLU,TFARM)
!
!*       2.2    Wind turbine data
!
CALL PRINT_DATA_TURBINE_ADR(TLUOUT%NLU,TTURBINE)
!
!*       2.3    Blade data
!
CALL PRINT_DATA_BLADE_ADR(TLUOUT%NLU,TBLADE)
!
!*       2.4    Airfoil data
!
CALL PRINT_DATA_AIRFOIL_ADR(TLUOUT%NLU,TAIRFOIL)
!
!
!-------------------------------------------------------------------
!
!*       3.    ALLOCATING VARIABLES 
!              -------------------- 
!
!*       3.0    Preliminaries
INB_WT    = TFARM%NNB_TURBINES
INB_B     = TTURBINE%NNB_BLADES
INB_AELT  = TBLADE%NNB_AZIELT
INB_RELT  = TBLADE%NNB_RADELT
! Hard coded variables, but they will be usefull in next updates
INB_TELT  = 2
INB_NELT  = 2
!
XDELTA_AZI     =  2d0*XPI/INB_AELT                                  ! Rotor disc azimutal devision
XDELTA_RAD     =  (TTURBINE%XR_MAX - TTURBINE%XR_MIN)/INB_RELT      ! Rotor disc radialal devision


!*       3.1    MODD_EOL_ADR variables
! at t
ALLOCATE(XELT_RAD      (INB_WT,INB_AELT,INB_RELT  ))
ALLOCATE(XELT_AZI      (INB_WT,INB_AELT,INB_RELT  ))
ALLOCATE(XAOA_ELT      (INB_WT,INB_AELT,INB_RELT  ))
ALLOCATE(XAOA_GLB      (INB_WT,INB_AELT,INB_RELT  ))
ALLOCATE(XAOA_BLA      (INB_WT,INB_RELT  ))
ALLOCATE(XFDRAG_ELT    (INB_WT,INB_AELT,INB_RELT  ))
ALLOCATE(XFDRAG_GLB    (INB_WT,INB_AELT,INB_RELT  ))
ALLOCATE(XFLIFT_ELT    (INB_WT,INB_AELT,INB_RELT  ))
ALLOCATE(XFLIFT_GLB    (INB_WT,INB_AELT,INB_RELT  ))
ALLOCATE(XFAERO_RA_ELT (INB_WT,INB_AELT,INB_RELT,3))
ALLOCATE(XFAERO_RA_GLB (INB_WT,INB_AELT,INB_RELT,3))
ALLOCATE(XFAERO_RA_BLA (INB_WT,INB_RELT,3))
ALLOCATE(XFAERO_RG_ELT (INB_WT,INB_AELT,INB_RELT,3))
ALLOCATE(XFAERO_RG_GLB (INB_WT,INB_AELT,INB_RELT,3))
ALLOCATE(XTHRUT        (INB_WT                 ))
ALLOCATE(XTORQT        (INB_WT                 ))
ALLOCATE(XPOWT         (INB_WT                 ))
! for mean values
ALLOCATE(XAOA_SUM_ADR      (INB_WT,INB_RELT  ))
ALLOCATE(XFAERO_RA_SUM (INB_WT,INB_RELT,3))
ALLOCATE(XTHRU_SUM     (INB_WT))
ALLOCATE(XTORQ_SUM     (INB_WT))
ALLOCATE(XPOW_SUM      (INB_WT))
!
!*       3.2    MODD_EOL_KINE_ADR variables
!
! - ORIENTATION MATRIX - 
ALLOCATE(XMAT_RG_RT(INB_WT,3,3))
ALLOCATE(XMAT_RG_RN(INB_WT,3,3))
ALLOCATE(XMAT_RT_RN(INB_WT,3,3))
ALLOCATE(XMAT_RG_RH(INB_WT,3,3))
ALLOCATE(XMAT_RH_RG(INB_WT,3,3))
ALLOCATE(XMAT_RN_RH(INB_WT,3,3))
ALLOCATE(XMAT_RG_RC(INB_WT,3,3))
ALLOCATE(XMAT_RH_RC(INB_WT,3,3))
ALLOCATE(XMAT_RG_RTA(INB_WT,INB_AELT,INB_RELT,3,3))
ALLOCATE(XMAT_RTA_RG(INB_WT,INB_AELT,INB_RELT,3,3))
ALLOCATE(XMAT_RC_RTA(INB_WT,INB_AELT,INB_RELT,3,3))
ALLOCATE(XMAT_RG_RA(INB_WT,INB_AELT,INB_RELT,3,3))
ALLOCATE(XMAT_RA_RG(INB_WT,INB_AELT,INB_RELT,3,3))
ALLOCATE(XMAT_RTA_RA(INB_WT,INB_AELT,INB_RELT,3,3))
!
! - POSITIONS & ORIENTATIONS -
! Tower
ALLOCATE(ZPOSINI_TOWO_RG (INB_WT,3)                 ) ! Initial tower origin pos. in RG
ALLOCATE(XPOSINI_TOWO_RG (INB_WT,3)                 ) ! Initial tower origin pos. in RG
ALLOCATE(XPOS_TOWO_RG    (INB_WT,3)                 ) ! Current tower origin pos. in RG
ALLOCATE(XPOS_TELT_RG    (INB_WT,INB_TELT,3)        ) ! Current tower element pos. in RG
ALLOCATE(XPOS_TELT_RT    (INB_WT,INB_TELT,3)        ) ! Current tower element pos. in RT
ALLOCATE(XANGINI_TOW_RG  (INB_WT,3)                 ) ! Initial tower ori. in RG 
! Nacelle	
ALLOCATE(XPOSINI_NACO_RT (INB_WT,3)                 ) ! Initial nacelle origin pos. in RT
ALLOCATE(XPOS_NACO_RG    (INB_WT,3)                 ) ! Current nacelle origin pos. in RG 
ALLOCATE(XPOS_NELT_RG    (INB_WT,INB_NELT,3)        ) ! Current nacelle element pos. in RG 
ALLOCATE(XPOS_NELT_RN    (INB_WT,INB_NELT,3)        ) ! Current nacelle element pos. in RN
ALLOCATE(XANGINI_NAC_RT  (INB_WT,3)                 ) ! Initial nacelle ori. in RT
! Hub
ALLOCATE(XPOSINI_HUB_RN  (INB_WT,3)                 ) ! Initial hub pos. in RN
ALLOCATE(XPOS_HUB_RG     (INB_WT,3)                 ) ! Current hub pos. in RG
ALLOCATE(XANGINI_HUB_RN  (INB_WT,3)                 ) ! Initial hub ori. in RN     
! Cylindrical 
ALLOCATE(XPOSINI_CYL_RH  (INB_WT,3)                 ) ! Initial cylindrical center pos. in RH
ALLOCATE(XPOS_CYL_RG     (INB_WT,3)                 ) ! Current cylindrical center pos. in RG
ALLOCATE(XANGINI_CYL_RH  (INB_WT,3)                 ) ! Initial cylindrical center ori. RH
! Annular Element
ALLOCATE(XPOS_ELT_RC     (INB_WT,INB_AELT,INB_RELT,3)  ) ! Element pos. in RC
ALLOCATE(XPOS_ELT_RG     (INB_WT,INB_AELT,INB_RELT,3)  ) ! Element pos. in RG
ALLOCATE(XPOS_SEC_RC     (INB_WT,INB_AELT,INB_RELT+1,3)) ! Section pos. in RC
ALLOCATE(XPOS_SEC_RG     (INB_WT,INB_AELT,INB_RELT+1,3)) ! Section pos. in RG
ALLOCATE(XANGINI_ELT_RC  (INB_WT,INB_AELT,INB_RELT,3))   ! Initial element ori. in RC
ALLOCATE(XANGINI_ELT_RA  (INB_WT,INB_AELT,INB_RELT,3))   ! Initial element ori. in transition ref. 
ALLOCATE(XTWIST_ELT      (INB_WT,INB_RELT)             ) ! Element twist in RC
ALLOCATE(XCHORD_ELT      (INB_WT,INB_RELT)             ) ! Element chord lenght 
ALLOCATE(XSURF_ELT       (INB_WT,INB_RELT)             ) ! Element lift surface 
ALLOCATE(XSURF_APP_ELT   (INB_WT,INB_RELT)             ) ! Element lift surface 
!
! - STRUCTURAL VELOCITIES -
! Tower
ALLOCATE(XTVEL_TOWO_RG   (INB_WT,3)                 ) ! Tower base trans. vel. in RG
ALLOCATE(XTVEL_TELT_RG   (INB_WT,INB_TELT,3)        ) ! Tower element trans. vel. in RG
ALLOCATE(XRVEL_RT_RG     (INB_WT,3)                 ) ! RT/RG rot. vel.	
! Nacelle 
ALLOCATE(XTVEL_NACO_RT   (INB_WT,3)                 ) ! Nacelle base trans. vel. in RT
ALLOCATE(XTVEL_NELT_RG   (INB_WT,INB_NELT,3)        ) ! Nacelle element trans. vel. in RG
ALLOCATE(XRVEL_RN_RT     (INB_WT,3)                 ) ! RN/RT rot. vel.
ALLOCATE(XRVEL_RN_RG     (INB_WT,3)                 ) ! RN/RG rot. vel.
! Hub 
ALLOCATE(XTVEL_HUB_RN    (INB_WT,3)                 ) ! Hub base trans. vel. in RN
ALLOCATE(XTVEL_HUB_RG    (INB_WT,3)                 ) ! Hub base trans. vel. in RG
ALLOCATE(XRVEL_RH_RN     (INB_WT,3)                 ) ! RH/RT rot. vel.
ALLOCATE(XRVEL_RH_RG     (INB_WT,3)                 ) ! RH/RG rot. vel.
! Cylindrical  
ALLOCATE(XTVEL_CYL_RH    (INB_WT,3)                 ) ! Cylindrical base trans. vel. in RH
ALLOCATE(XTVEL_CYL_RG    (INB_WT,3)                 ) ! Cylindrical base trans. vel. in RG
ALLOCATE(XRVEL_RC_RH     (INB_WT,3)                 ) ! RC/RH rot. vel.
ALLOCATE(XRVEL_RC_RG     (INB_WT,3)                 ) ! RC/RG rot. vel.
! Annular Elements 
ALLOCATE(XTVEL_ELT_RC    (INB_WT,INB_AELT,INB_RELT,3)  ) ! Element base trans. vel. in RC
ALLOCATE(XTVEL_ELT_RG    (INB_WT,INB_AELT,INB_RELT,3)  ) ! Element base trans. vel. in RG
ALLOCATE(XTVEL_ELT_RA    (INB_WT,INB_AELT,INB_RELT,3)  ) ! Element base trans. vel. in RA
ALLOCATE(XRVEL_RA_RC     (INB_WT,INB_AELT,INB_RELT,3)  ) ! RA/RB rot. vel.
ALLOCATE(XRVEL_RA_RG     (INB_WT,INB_AELT,INB_RELT,3)  ) ! RA/RG rot. vel.
!
!-------------------------------------------------------------------
!
!*       4.    FIRST BUILDING OF WIND TURBINES 
!              ------------------------------- 
!
!*       4.1    Preliminaries


CALL GET_INDICE_ll(IIB,IJB,IIE,IJE) ! Get begin and end domain index (CPU)
IKB=1+JPVEXT                        ! Vertical begin index
!
XPOS_REF(:)     = 0d0                ! Global Origin
!
!
!*       4.2    Tower
DO JROT=1, INB_WT
! Velocities   (Rotor, (XYZ))
 XTVEL_TOWO_RG(JROT,:)       = 0d0 
 XRVEL_RT_RG(JROT,:)         = 0d0
! Positions
 ! Init
 ZPOSINI_TOWO_RG(JROT,1)     = 0
 ZPOSINI_TOWO_RG(JROT,2)     = 0
 ZPOSINI_TOWO_RG(JROT,3)     = 0
 ! Finding the elevation at the WT position
 DO JJ=IJB,IJE
  DO JI=IIB,IIE
   IF (TFARM%XPOS_X(JROT) >= XXHAT(JI) .AND. &
       TFARM%XPOS_X(JROT) <  XXHAT(JI) + PDXX(JI,JJ,IKB)) THEN
    IF (TFARM%XPOS_Y(JROT) >= XYHAT(JJ) .AND. &
        TFARM%XPOS_Y(JROT) <  XYHAT(JJ) + PDYY(JI,JJ,IKB)) THEN
     ! Horizontal position
     ZPOSINI_TOWO_RG(JROT,1)     = TFARM%XPOS_X(JROT)    ! Tower base position
     ZPOSINI_TOWO_RG(JROT,2)     = TFARM%XPOS_Y(JROT)
     ! Finding the elevation at the WT position
     ZPOSINI_TOWO_RG(JROT,3)     = XZS(JI,JJ)
    END IF
   END IF
  END DO
 END DO
 ! Sharing information
 CALL MPI_ALLREDUCE(ZPOSINI_TOWO_RG, XPOSINI_TOWO_RG, SIZE(XPOSINI_TOWO_RG),&
        MNHREAL_MPI,MPI_SUM,NMNH_COMM_WORLD,IINFO)
 ! Tower elements
 DO JTELT=1, INB_TELT
  XPOS_TELT_RT(JROT,JTELT,1) = 0d0
  XPOS_TELT_RT(JROT,JTELT,2) = 0d0
  XPOS_TELT_RT(JROT,JTELT,3) = (JTELT-1)*(TTURBINE%XH_HEIGHT)/(INB_TELT-1)
 END DO
 ! Angles
 XANGINI_TOW_RG(JROT,1)      = 0d0
 XANGINI_TOW_RG(JROT,2)      = 0d0
 XANGINI_TOW_RG(JROT,3)      = 0d0
!
!
!*       4.3    Nacelle
! Velocities
 XTVEL_NACO_RT(JROT,:)       = 0d0
 XRVEL_RN_RT(JROT,:)         = 0d0
! Positions   (Rotor, (XYZ))	! From last point of tower 
 ! Origin
 XPOSINI_NACO_RT(JROT,:)     = 0d0  ! Distance between nacelle base and tower top 
 ! Elements
 DO JNELT=1, INB_NELT
  XPOS_NELT_RN(JROT,JNELT,1) = (JNELT-1)*TTURBINE%XH_DEPORT/(INB_NELT-1)
  XPOS_NELT_RN(JROT,JNELT,2) = 0d0
  XPOS_NELT_RN(JROT,JNELT,3) = 0d0
 END DO
 ! Angles
 XANGINI_NAC_RT(JROT,1)      = 0d0
 XANGINI_NAC_RT(JROT,2)      = 0d0
 XANGINI_NAC_RT(JROT,3)      = XPI + TFARM%XNAC_YAW(JROT)
!
!
!*       4.4    Hub
! Velocities
 XTVEL_HUB_RN(JROT,:)      = 0d0
 XRVEL_RH_RN(JROT,1)       = 0d0
 XRVEL_RH_RN(JROT,2)       = 0d0
 XRVEL_RH_RN(JROT,3)       = TFARM%XOMEGA(JROT)
! Position   (Rotor, (XYZ)) 	! From nacelle last point 
 XPOSINI_HUB_RN(JROT,1)    = 0d0
 XPOSINI_HUB_RN(JROT,2)    = 0d0
 XPOSINI_HUB_RN(JROT,3)    = 0d0
 XANGINI_HUB_RN(JROT,1)    = 0d0
 XANGINI_HUB_RN(JROT,2)    = XPI/2d0 - TTURBINE%XNAC_TILT 
 XANGINI_HUB_RN(JROT,3)    = 0d0 
!
!
!*       4.5 Cylindrical
! Velocities 
  XRVEL_RC_RH(JROT,:)        = 0d0
  XTVEL_CYL_RH(JROT,:)       = 0d0
! Position 
  XPOSINI_CYL_RH(JROT,:)     = 0d0  
  XANGINI_CYL_RH(JROT,1)     = 0d0  
  XANGINI_CYL_RH(JROT,2)     = 0d0  
  XANGINI_CYL_RH(JROT,3)     = XPI 
!
!
!*      4.6 Annular Elements

 DO JAZI=1, INB_AELT


  DO JRAD=1, INB_RELT+1

! - Positioning of sections (cuts)
  
   XPOS_SEC_RC(JROT,JAZI,JRAD,1)    = TTURBINE%XR_MIN + (JRAD-1) * XDELTA_RAD
   XPOS_SEC_RC(JROT,JAZI,JRAD,2)    = (JAZI-1) * XDELTA_AZI
   XPOS_SEC_RC(JROT,JAZI,JRAD,3)    = 0d0
  END DO
  
  DO JRAD=1, INB_RELT
! - Positioning of centers (points of application) 
   XPOS_ELT_RC(JROT,JAZI,JRAD,1)    = XPOS_SEC_RC(JROT,JAZI,JRAD,1) + XDELTA_RAD/2d0
   XPOS_ELT_RC(JROT,JAZI,JRAD,2)    = XPOS_SEC_RC(JROT,JAZI,JRAD,2) + XDELTA_AZI/2d0
   XPOS_ELT_RC(JROT,JAZI,JRAD,3)    = 0d0
   XELT_RAD(JROT,JAZI,JRAD)         = XPOS_ELT_RC(JROT,JAZI,JRAD,1)
   XELT_AZI(JROT,JAZI,JRAD)         = XPOS_ELT_RC(JROT,JAZI,JRAD,2)


! - Calculating chord and twist
   ZRAD                             = XELT_RAD(JROT,JAZI,JRAD)
   XCHORD_ELT(JROT,JRAD)            = INTERP_SPLCUB(ZRAD, TBLADE%XRAD, TBLADE%XCHORD)
   XTWIST_ELT(JROT,JRAD)            = INTERP_SPLCUB(ZRAD, TBLADE%XRAD, TBLADE%XTWIST)
! - Calculating lifting surface 
   XSURF_ELT(JROT,JRAD)             = XCHORD_ELT(JROT,JRAD)                  &
                                     * (XPOS_SEC_RC(JROT,JAZI,JRAD+1,1)      &
                                     - XPOS_SEC_RC(JROT,JAZI,JRAD,1))
   XSURF_APP_ELT(JROT,JRAD)         = INB_B * XSURF_ELT(JROT,JRAD)           &                   
                                     * XDELTA_AZI/(2d0*XPI)

! - Velocities
   XRVEL_RA_RC(JROT,JAZI,JRAD,:)    = 0d0
   XTVEL_ELT_RC(JROT,JAZI,JRAD,:)   = 0d0

! - Orientation 
!   - Orientation in the transition reference frame      
   XANGINI_ELT_RC(JROT,JAZI,JRAD,1) = 0d0 
   XANGINI_ELT_RC(JROT,JAZI,JRAD,2) = 0d0
   XANGINI_ELT_RC(JROT,JAZI,JRAD,3) = 0d0 + (JAZI-1) * XDELTA_AZI + XDELTA_AZI/2d0  
!   - Orientation in the annular elements reference frame      
   XANGINI_ELT_RA(JROT,JAZI,JRAD,1) = 0d0 + TFARM%XBLA_PITCH(JROT) + XTWIST_ELT(JROT,JRAD)
   XANGINI_ELT_RA(JROT,JAZI,JRAD,2) = 0d0
   XANGINI_ELT_RA(JROT,JAZI,JRAD,3) = 0d0   
  END DO ! Loop radial
 END DO ! Loop azimutal
END DO ! Loop turbine
!
END SUBROUTINE INI_EOL_ADR
