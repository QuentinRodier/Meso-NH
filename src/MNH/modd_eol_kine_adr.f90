!MNH_LIC Copyright 2018-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!!
!!    #####################
      MODULE MODD_EOL_KINE_ADR
!!    #####################
!!
!!*** *MODD_EOL_KINE_ADR*
!!
!!    PURPOSE
!!    -------
!!      Declaration to take into account wind turbine motion
!!              
!!
!!**  AUTHOR
!!    ------
!!    H. Toumi                    *IFPEN*
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 09/22
!!    Modification 05/04/23 (PA. Joulin) Updated for a main version
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
!       
!
! - Matrix to move from one fram to an other -
REAL, DIMENSION(:,:,:),     ALLOCATABLE    :: XMAT_RG_RT                            ! RG  = Repere GLOBAL, RT = Repere TOWER 
REAL, DIMENSION(:,:,:),     ALLOCATABLE    :: XMAT_RG_RN, XMAT_RT_RN                ! RN  = Repere NACELLE
REAL, DIMENSION(:,:,:),     ALLOCATABLE    :: XMAT_RG_RH, XMAT_RH_RG, XMAT_RN_RH    ! RH  = Repere HUB
REAL, DIMENSION(:,:,:),     ALLOCATABLE    :: XMAT_RG_RC, XMAT_RH_RC                ! RC  = Repere CYLINDRICAL
REAL, DIMENSION(:,:,:,:,:), ALLOCATABLE    :: XMAT_RG_RTA, XMAT_RTA_RG, XMAT_RC_RTA ! RTA = Repere TRANSITION ANNULAR
REAL, DIMENSION(:,:,:,:,:), ALLOCATABLE    :: XMAT_RG_RA, XMAT_RA_RG, XMAT_RTA_RA   ! RE  = Repere ANNULAR

! - POSITIONS & ORIENTATIONS -
REAL, DIMENSION(3)                         :: XPOS_REF                           ! Reference position

! Tower
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XPOSINI_TOWO_RG                    ! Initial tower origin position, in global reference frame
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XPOS_TOWO_RG                       ! Current tower origin real position, in global reference frame
REAL, DIMENSION(:,:,:),     ALLOCATABLE    :: XPOS_TELT_RG                       ! Current tower element position, in global reference frame
REAL, DIMENSION(:,:,:),     ALLOCATABLE    :: XPOS_TELT_RT                       ! Current tower element position, in tower frame
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XANGINI_TOW_RG                     ! Initial tower orientation in global ref frame 

! Nacelle	
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XPOSINI_NACO_RT                    ! Initial nacelle position, in tower reference frame 
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XPOS_NACO_RG                       ! Initial nacelle position, in global reference frame 
REAL, DIMENSION(:,:,:),     ALLOCATABLE    :: XPOS_NELT_RG                       ! Initial nacelle position, in global reference frame 
REAL, DIMENSION(:,:,:),     ALLOCATABLE    :: XPOS_NELT_RN                       ! Initial nacelle position, in nacelle reference frame 
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XANGINI_NAC_RT                     ! Initial nacelle orientation, in tower reference frame

! Hub
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XPOSINI_HUB_RN                     ! Initial hub position, in nacelle reference frame
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XPOS_HUB_RG                        ! Current hub position, in global reference frame
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XANGINI_HUB_RN                     ! Initial hub orientation, in nacelle reference frame
       
! Cylindrical
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XPOSINI_CYL_RH                     ! Initial blade root position, in hub reference frame
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XPOS_CYL_RG                        ! Current blade root position, in global reference frame
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XANGINI_CYL_RH                     ! Initial blade orientation, in hub reference frame

! Annular Element
REAL, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XPOS_ELT_RC                        ! Element position, in cylindrical reference frame
REAL, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XPOS_ELT_RG                        ! Element position, in global reference frame
REAL, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XPOS_SEC_RC                        ! Section position, in cylindrical reference frame
REAL, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XPOS_SEC_RG                        ! Section position, in global reference frame
REAL, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XANGINI_ELT_RC                     ! Initial element orientation in cyl reference frame
REAL, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XANGINI_ELT_RA                     ! Initial element orientation in annular elt transition ref frame
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XTWIST_ELT                         ! Element twist, interpolated from data
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XCHORD_ELT                         ! Element chord lenght, interpolated from data
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XSURF_ELT                          ! Element lift surface 
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XSURF_APP_ELT                      ! Element lift surface  
!
!
! - STRUCTURAL VELOCITIES -
! Tower
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XTVEL_TOWO_RG                      ! Tower base translation velocity, in global reference frame
REAL, DIMENSION(:,:,:),     ALLOCATABLE    :: XTVEL_TELT_RG                      ! Tower element velocity, in global reference frame
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XRVEL_RT_RG                        ! RT/RG rotational velocity	

! Nacelle 
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XTVEL_NACO_RT                      ! Nacelle base translation velocity, in tower reference frame
REAL, DIMENSION(:,:,:),     ALLOCATABLE    :: XTVEL_NELT_RG                      ! Nacelle element translation velocity, in global reference frame
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XRVEL_RN_RT                        ! RN/RT rotational velocity
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XRVEL_RN_RG                        ! RN/RG rotational velocity

! Hub 
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XTVEL_HUB_RN                       ! Hub base translation velocity, in global reference frame
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XTVEL_HUB_RG                       ! Hub base translation velocity, in global reference frame
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XRVEL_RH_RN                        ! RH/RN rotational velocity
REAL, DIMENSION(:,:),       ALLOCATABLE    :: XRVEL_RH_RG                        ! RH/RG rotational velocity

! Cylindrical
REAL, DIMENSION(:,:),     ALLOCATABLE    :: XTVEL_CYL_RH                       ! Cyllindrical base translation velocity, in hub reference frame
REAL, DIMENSION(:,:),     ALLOCATABLE    :: XTVEL_CYL_RG                       ! Cylindrical base translation velocity, in global reference frame
REAL, DIMENSION(:,:),     ALLOCATABLE    :: XRVEL_RC_RH                        ! RC/RH rotational velocity
REAL, DIMENSION(:,:),     ALLOCATABLE    :: XRVEL_RC_RG                        ! RC/RG rotational velocity

! Annular Elements
REAL, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XTVEL_ELT_RC                       ! Element base translation velocity, in cylindrical reference frame
REAL, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XTVEL_ELT_RG                       ! Element base translation velocity, in global reference frame
REAL, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XTVEL_ELT_RA                       ! Element base translation velocity, in annular element reference frame
REAL, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XRVEL_RA_RC                        ! RA/RC rotational velocity
REAL, DIMENSION(:,:,:,:),   ALLOCATABLE    :: XRVEL_RA_RG                        ! RA/RG rotational velocity
                                                                                                        
END MODULE MODD_EOL_KINE_ADR
