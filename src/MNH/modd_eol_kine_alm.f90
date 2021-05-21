!MNH_LIC Copyright 1994-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!!
!!    #####################
      MODULE MODD_EOL_KINE_ALM
!!    #####################
!!
!!*** *MODD_EOL_KINE_ALM*
!!
!!    PURPOSE
!!    -------
!       Declaration to take into account wind turbine motion
!              
!!
!!**  AUTHOR
!!    ------
!!    PA.Joulin                   *CNRM & IFPEN*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 04/18
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
!       
!
! - Matrix to move from one fram to an other -
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XMAT_RG_RT                         ! RG = Repere GLOBAL, RT = Repere TOWER 
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XMAT_RG_RN, XMAT_RT_RN             ! RN = Repere NACELLE
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XMAT_RG_RH, XMAT_RH_RG, XMAT_RN_RH ! RH = Repere HUB
DOUBLE PRECISION, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XMAT_RG_RB, XMAT_RH_RB             ! RB = Repere BLADE
DOUBLE PRECISION, DIMENSION(:,:,:,:,:), ALLOCATABLE    :: XMAT_RG_RE, XMAT_RE_RG, XMAT_RB_RE ! RE = Repere ELEMENT

! - POSITIONS & ORIENTATIONS -
DOUBLE PRECISION, DIMENSION(3)                         :: XPOS_REF                           ! Reference position

! Tower
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XPOSINI_TOWO_RG                    ! Initial tower origin position, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XPOS_TOWO_RG                       ! Current tower origin real position, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XPOS_TELT_RG                       ! Current tower element position, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XPOS_TELT_RT                       ! Current tower element position, in tower frame
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XANGINI_TOW_RG                     ! Initial tower orientation in global ref frame 

! Nacelle	
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XPOSINI_NACO_RT                    ! Initial nacelle position, in tower reference frame 
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XPOS_NACO_RG                       ! Initial nacelle position, in global reference frame 
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XPOS_NELT_RG                       ! Initial nacelle position, in global reference frame 
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XPOS_NELT_RN                       ! Initial nacelle position, in nacelle reference frame 
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XANGINI_NAC_RT                     ! Initial nacelle orientation, in tower reference frame

! Hub
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XPOSINI_HUB_RN                     ! Initial hub position, in nacelle reference frame
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XPOS_HUB_RG                        ! Current hub position, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XANGINI_HUB_RN                     ! Initial hub orientation, in nacelle reference frame
       
! Blade
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XPOSINI_BLA_RH                     ! Initial blade root position, in hub reference frame
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XPOS_BLA_RG                        ! Current blade root position, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XANGINI_BLA_RH                     ! Initial blade orientation, in hub reference frame

! Element
DOUBLE PRECISION, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XPOS_ELT_RB                        ! Element position, in blade reference frame
DOUBLE PRECISION, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XPOS_ELT_RG                        ! Element position, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XPOS_SEC_RB                        ! Section position, in blade reference frame
DOUBLE PRECISION, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XPOS_SEC_RG                        ! Section position, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XANGINI_ELT_RB                     ! Initial element orientation in blade reference frame
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XTWIST_ELT                         ! Element twist, interpolated from data
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XCHORD_ELT                         ! Element chord lenght, interpolated from data
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XSURF_ELT                          ! Element lift surface 
!
!
! - STRUCTURAL VELOCITIES -
! Tower
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XTVEL_TOWO_RG                      ! Tower base translation velocity, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XTVEL_TELT_RG                      ! Tower element velocity, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XRVEL_RT_RG                        ! RT/RG rotational velocity	

! Nacelle 
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XTVEL_NACO_RT                      ! Nacelle base translation velocity, in tower reference frame
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XTVEL_NELT_RG                      ! Nacelle element translation velocity, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XRVEL_RN_RT                        ! RN/RT rotational velocity
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XRVEL_RN_RG                        ! RN/RG rotational velocity

! Hub 
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XTVEL_HUB_RN                       ! Hub base translation velocity, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XTVEL_HUB_RG                       ! Hub base translation velocity, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XRVEL_RH_RN                        ! RH/RN rotational velocity
DOUBLE PRECISION, DIMENSION(:,:),       ALLOCATABLE    :: XRVEL_RH_RG                        ! RH/RG rotational velocity

! Blade 
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XTVEL_BLA_RH                       ! Blade base translation velocity, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XTVEL_BLA_RG                       ! Blade base translation velocity, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XRVEL_RB_RH                        ! RB/RH rotational velocity
DOUBLE PRECISION, DIMENSION(:,:,:),     ALLOCATABLE    :: XRVEL_RB_RG                        ! RB/RG rotational velocity

! Elements
DOUBLE PRECISION, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XTVEL_ELT_RB                       ! Element base translation velocity, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XTVEL_ELT_RG                       ! Element base translation velocity, in global reference frame
DOUBLE PRECISION, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XTVEL_ELT_RE                       ! Element base translation velocity, in element reference frame
DOUBLE PRECISION, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XRVEL_RE_RB                        ! RE/RB rotational velocity
DOUBLE PRECISION, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XRVEL_RE_RG                        ! RE/RG rotational velocity
        
END MODULE MODD_EOL_KINE_ALM
