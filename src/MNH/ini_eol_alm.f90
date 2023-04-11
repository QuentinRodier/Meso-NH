!MNH_LIC Copyright 2018-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_INI_EOL_ALM
!     ##########################
!
INTERFACE
!
SUBROUTINE INI_EOL_ALM(PDXX,PDYY)
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDXX,PDYY    ! mesh size
!
END SUBROUTINE INI_EOL_ALM                  
!
END INTERFACE
!
END MODULE MODI_INI_EOL_ALM
!
!     ############################################################
      SUBROUTINE INI_EOL_ALM(PDXX,PDYY)
!     ############################################################
!
!!****  *INI_EOL_ALM* - routine to initialize the Actuator Line Model 
!!                      to simulate wind turbines      
!!
!!    PURPOSE
!!    -------
!!     Routine to initialized the ALM (wind turbine model) variables
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
!!    INTEGER                 :: NNB_BLAELT        ! Number of blade elements
!!    
!!    *MODD_EOL_ALM (OUTPUT):
!!    TYPE(FARM)                               :: TFARM
!!    TYPE(TURBINE)                            :: TTURBINE
!!    TYPE(BLADE)                              :: TBLADE
!!    TYPE(AIRFOIL), DIMENSION(:), ALLOCATABLE :: TAIRFOIL
!!    REAL, DIMENSION(:,:,:),      ALLOCATABLE :: XELT_RAD      ! Blade elements radius [m]
!!    REAL, DIMENSION(:,:,:),      ALLOCATABLE :: XAOA_GLB      ! Angle of attack of an element [rad]
!!    REAL, DIMENSION(:,:,:),      ALLOCATABLE :: XFLIFT_GLB    ! Lift force, parallel to Urel [N]
!!    REAL, DIMENSION(:,:,:),      ALLOCATABLE :: XFDRAG_GLB    ! Drag force, perpendicular to Urel [N]
!!    REAL, DIMENSION(:,:,:,:),    ALLOCATABLE :: XFAERO_RE_GLB ! Aerodyn. force (lift+drag) in RE [N]
!!    REAL, DIMENSION(:,:,:,:),    ALLOCATABLE :: XFAERO_RG_GLB ! Aerodyn. force (lift+drag) in RG [N]
!!    REAL, DIMENSION(:,:,:),      ALLOCATABLE :: XAOA_SUM      ! Sum of angle of attack [rad]
!!    REAL, DIMENSION(:,:,:,:),    ALLOCATABLE :: XFAERO_RE_SUM ! Sum of aerodyn. force (lift+drag) in RE [N]
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
!!      Modification       04/23 (H. Toumi) Modified shared_io var
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------ 
!
!*       0.1    Modules
!
USE MODD_EOL_ALM
USE MODD_EOL_SHARED_IO, ONLY: CFARM_CSVDATA
USE MODD_EOL_SHARED_IO, ONLY: CTURBINE_CSVDATA
USE MODD_EOL_SHARED_IO, ONLY: CBLADE_CSVDATA
USE MODD_EOL_SHARED_IO, ONLY: CAIRFOIL_CSVDATA
USE MODD_EOL_SHARED_IO, ONLY: XTHRUT, XTORQT, XPOWT
USE MODD_EOL_SHARED_IO, ONLY: XTHRU_SUM, XTORQ_SUM, XPOW_SUM
USE MODD_EOL_SHARED_IO, ONLY: XELT_RAD
USE MODD_EOL_SHARED_IO, ONLY: XAOA_GLB, XFLIFT_GLB, XFDRAG_GLB, XFAERO_RG_GLB
USE MODD_EOL_SHARED_IO, ONLY: XAOA_SUM 
USE MODI_EOL_READER,    ONLY: READ_CSVDATA_FARM_ALM
USE MODI_EOL_READER,    ONLY: READ_CSVDATA_TURBINE_ALM
USE MODI_EOL_READER,    ONLY: READ_CSVDATA_BLADE_ALM
USE MODI_EOL_READER,    ONLY: READ_CSVDATA_AIRFOIL_ALM
USE MODI_EOL_PRINTER,   ONLY: PRINT_DATA_FARM_ALM
USE MODI_EOL_PRINTER,   ONLY: PRINT_DATA_TURBINE_ALM
USE MODI_EOL_PRINTER,   ONLY: PRINT_DATA_BLADE_ALM
USE MODI_EOL_PRINTER,   ONLY: PRINT_DATA_AIRFOIL_ALM
USE MODD_EOL_KINE_ALM
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
INTEGER  :: JBLA                    ! Blade index
INTEGER  :: JBELT                   ! Balde element index
! .. for domain
INTEGER  :: IIB,IJB,IKB             ! Begin of a CPU domain
INTEGER  :: IIE,IJE                 ! End of a CPU domain
INTEGER  :: JI,JJ                   ! Domain index
! Some variables to be coder-friendly
INTEGER  :: INB_WT, INB_B, INB_BELT ! Total numbers of wind turbines, blades, and blade elt
INTEGER  :: INB_TELT, INB_NELT      ! Total numbers of tower elt, and nacelle elt
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
CALL READ_CSVDATA_FARM_ALM(TRIM(CFARM_CSVDATA),TFARM)
!
!*       1.2    Wind turbine data
!
CALL READ_CSVDATA_TURBINE_ALM(TRIM(CTURBINE_CSVDATA),TTURBINE)
!
!*       1.3    Blade data
!
CALL READ_CSVDATA_BLADE_ALM(TRIM(CBLADE_CSVDATA),TTURBINE,TBLADE)
!
!*       1.4    Airfoil data
!
CALL READ_CSVDATA_AIRFOIL_ALM(TRIM(CAIRFOIL_CSVDATA),TBLADE,TAIRFOIL)
!
!
!-------------------------------------------------------------------
!
!*       2.    PRINTING DATA 
!              ------------- 
!
!*       2.1    Wind farm data
!
CALL PRINT_DATA_FARM_ALM(TLUOUT%NLU,TFARM)
!
!*       2.2    Wind turbine data
!
CALL PRINT_DATA_TURBINE_ALM(TLUOUT%NLU,TTURBINE)
!
!*       2.3    Blade data
!
CALL PRINT_DATA_BLADE_ALM(TLUOUT%NLU,TBLADE)
!
!*       2.4    Airfoil data
!
CALL PRINT_DATA_AIRFOIL_ALM(TLUOUT%NLU,TAIRFOIL)
!
!
!-------------------------------------------------------------------
!
!*       3.    ALLOCATING VARIABLES 
!              -------------------- 
!
!*       3.0    Preliminaries
INB_WT   = TFARM%NNB_TURBINES
INB_B    = TTURBINE%NNB_BLADES
INB_BELT  = TBLADE%NNB_BLAELT
! Hard coded variables, but they will be usefull in next updates
INB_TELT = 2
INB_NELT = 2
!
!*       3.1    MODD_EOL_ALM variables
! at t
ALLOCATE(XELT_RAD     (INB_WT,INB_B,INB_BELT  ))
ALLOCATE(XAOA_GLB     (INB_WT,INB_B,INB_BELT  ))
ALLOCATE(XFDRAG_GLB   (INB_WT,INB_B,INB_BELT  ))
ALLOCATE(XFLIFT_GLB   (INB_WT,INB_B,INB_BELT  ))
ALLOCATE(XFAERO_RE_GLB(INB_WT,INB_B,INB_BELT,3))
ALLOCATE(XFAERO_RG_GLB(INB_WT,INB_B,INB_BELT,3))
ALLOCATE(XTHRUT       (INB_WT                 ))
ALLOCATE(XTORQT       (INB_WT                 ))
ALLOCATE(XPOWT        (INB_WT                 ))
! for mean values
ALLOCATE(XAOA_SUM     (INB_WT,INB_B,INB_BELT  ))
ALLOCATE(XFAERO_RE_SUM(INB_WT,INB_B,INB_BELT,3))
ALLOCATE(XTHRU_SUM    (INB_WT))
ALLOCATE(XTORQ_SUM    (INB_WT))
ALLOCATE(XPOW_SUM     (INB_WT))
!
!*       3.2    MODD_EOL_KINE_ALM variables
!
! - ORIENTATION MATRIX - 
ALLOCATE(XMAT_RG_RT(INB_WT,3,3))
ALLOCATE(XMAT_RG_RN(INB_WT,3,3))
ALLOCATE(XMAT_RT_RN(INB_WT,3,3))
ALLOCATE(XMAT_RG_RH(INB_WT,3,3))
ALLOCATE(XMAT_RH_RG(INB_WT,3,3))
ALLOCATE(XMAT_RN_RH(INB_WT,3,3))
ALLOCATE(XMAT_RG_RB(INB_WT,INB_B,3,3))
ALLOCATE(XMAT_RH_RB(INB_WT,INB_B,3,3))
ALLOCATE(XMAT_RG_RE(INB_WT,INB_B,INB_BELT,3,3))
ALLOCATE(XMAT_RE_RG(INB_WT,INB_B,INB_BELT,3,3))
ALLOCATE(XMAT_RB_RE(INB_WT,INB_B,INB_BELT,3,3))
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
ALLOCATE(XPOS_NELT_RG  (INB_WT,INB_NELT,3)          ) ! Current nacelle element pos. in RG 
ALLOCATE(XPOS_NELT_RN  (INB_WT,INB_NELT,3)          ) ! Current nacelle element pos. in RN
ALLOCATE(XANGINI_NAC_RT  (INB_WT,3)                 ) ! Initial nacelle ori. in RT
! Hub
ALLOCATE(XPOSINI_HUB_RN  (INB_WT,3)                 ) ! Initial hub pos. in RN
ALLOCATE(XPOS_HUB_RG     (INB_WT,3)                 ) ! Current hub pos. in RG
ALLOCATE(XANGINI_HUB_RN  (INB_WT,3)                 ) ! Initial hub ori. in RN     
! Blade
ALLOCATE(XPOSINI_BLA_RH  (INB_WT,INB_B,3)           ) ! Initial blade root pos. in RH
ALLOCATE(XPOS_BLA_RG     (INB_WT,INB_B,3)           ) ! Current blade root pos. in RG
ALLOCATE(XANGINI_BLA_RH  (INB_WT,INB_B,3)           ) ! Initial blade ori. RH
! Element
ALLOCATE(XPOS_ELT_RB     (INB_WT,INB_B,INB_BELT,3)  ) ! Element pos. in RB
ALLOCATE(XPOS_ELT_RG     (INB_WT,INB_B,INB_BELT,3)  ) ! Element pos. in RG
ALLOCATE(XPOS_SEC_RB     (INB_WT,INB_B,INB_BELT+1,3)) ! Section pos. in RB
ALLOCATE(XPOS_SEC_RG     (INB_WT,INB_B,INB_BELT+1,3)) ! Section pos. in RG
ALLOCATE(XANGINI_ELT_RB  (INB_WT,INB_B,INB_BELT,3)  ) ! Initial element ori. in RB
ALLOCATE(XTWIST_ELT      (INB_WT,INB_B,INB_BELT)    ) ! Element twist in RB
ALLOCATE(XCHORD_ELT      (INB_WT,INB_B,INB_BELT)    ) ! Element chord lenght 
ALLOCATE(XSURF_ELT       (INB_WT,INB_B,INB_BELT)    ) ! Element lift surface 
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
! Blade 
ALLOCATE(XTVEL_BLA_RH    (INB_WT,INB_B,3)           ) ! Blade base trans. vel. in RH
ALLOCATE(XTVEL_BLA_RG    (INB_WT,INB_B,3)           ) ! Blade base trans. vel. in RG
ALLOCATE(XRVEL_RB_RH     (INB_WT,INB_B,3)           ) ! RB/RH rot. vel.
ALLOCATE(XRVEL_RB_RG     (INB_WT,INB_B,3)           ) ! RB/RG rot. vel.
! Elements 
ALLOCATE(XTVEL_ELT_RB    (INB_WT,INB_B,INB_BELT,3)  ) ! Element base trans. vel. in RB
ALLOCATE(XTVEL_ELT_RG    (INB_WT,INB_B,INB_BELT,3)  ) ! Element base trans. vel. in RG
ALLOCATE(XTVEL_ELT_RE    (INB_WT,INB_B,INB_BELT,3)  ) ! Element base trans. vel. in RE
ALLOCATE(XRVEL_RE_RB     (INB_WT,INB_B,INB_BELT,3)  ) ! RE/RB rot. vel.
ALLOCATE(XRVEL_RE_RG     (INB_WT,INB_B,INB_BELT,3)  ) ! RE/RG rot. vel.
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
!*       4.5    Blades
 DO JBLA=1, INB_B
! Velocities			
  XRVEL_RB_RH(JROT,JBLA,:)     = 0d0
  XTVEL_BLA_RH(JROT,JBLA,:)    = 0d0
! Position  (Rotor, Blade, (XYZ)) ! From hub point
  XPOSINI_BLA_RH(JROT,JBLA,:)  = 0d0
  XANGINI_BLA_RH(JROT,JBLA,1)  = 0d0
  XANGINI_BLA_RH(JROT,JBLA,2)  = 0d0
  XANGINI_BLA_RH(JROT,JBLA,3)  = (JBLA-1)*2d0*XPI/INB_B
!
!
!*       4.5    Elements
! - Positioning of sections (cuts)
  DO JBELT=1, INB_BELT+1
  
   XPOS_SEC_RB(JROT,JBLA,JBELT,1)    = TTURBINE%XR_MIN + (JBELT-1) &
                                     * (TTURBINE%XR_MAX -  TTURBINE%XR_MIN)/INB_BELT
   XPOS_SEC_RB(JROT,JBLA,JBELT,2)    = 0d0
   XPOS_SEC_RB(JROT,JBLA,JBELT,3)    = 0d0
  ENDDO                     
  DO JBELT=1, INB_BELT
! - Positioning of centers (points of application) 
   XPOS_ELT_RB(JROT,JBLA,JBELT,1)    = XPOS_SEC_RB(JROT,JBLA,JBELT,1) &
                                     + (XPOS_SEC_RB(JROT,JBLA,JBELT+1,1) &
                                     -  XPOS_SEC_RB(JROT,JBLA,JBELT,1))/2d0
   XPOS_ELT_RB(JROT,JBLA,JBELT,2)    = 0d0
   XPOS_ELT_RB(JROT,JBLA,JBELT,3)    = 0d0
   XELT_RAD(JROT,JBLA,JBELT)         = XPOS_ELT_RB(JROT,JBLA,JBELT,1)
! - Calculating chord and twist
   ZRAD                              = XELT_RAD(JROT,JBLA,JBELT)
   XCHORD_ELT(JROT,JBLA,JBELT)       = INTERP_SPLCUB(ZRAD, TBLADE%XRAD, TBLADE%XCHORD)
   XTWIST_ELT(JROT,JBLA,JBELT)       = INTERP_SPLCUB(ZRAD, TBLADE%XRAD, TBLADE%XTWIST)
! - Calculating lifting surface 
   XSURF_ELT(JROT,JBLA,JBELT)        = XCHORD_ELT(JROT,JBLA,JBELT)       &
                                     * (XPOS_SEC_RB(JROT,JBLA,JBELT+1,1) &
                                     - XPOS_SEC_RB(JROT,JBLA,JBELT,1))
! Velocities
   XRVEL_RE_RB(JROT,JBLA,JBELT,:)    = 0d0
   XTVEL_ELT_RB(JROT,JBLA,JBELT,:)   = 0d0
! Orientation 
   XANGINI_ELT_RB(JROT,JBLA,JBELT,1) = -XPI/2d0 + TFARM%XBLA_PITCH(JROT) &
                                     + XTWIST_ELT(JROT,JBLA,JBELT)
   XANGINI_ELT_RB(JROT,JBLA,JBELT,2) = XPI/2d0
   XANGINI_ELT_RB(JROT,JBLA,JBELT,3) = XPI/2d0
  END DO ! Loop element
 END DO ! Loop blade
END DO ! Loop turbine
!
END SUBROUTINE INI_EOL_ALM
