!MNH_LIC Copyright 1994-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!!
!!    #####################
      MODULE MODD_EOL_ADNR
!!    #####################
!!
!!*** *MODD_EOL_ADNR*
!!
!!    PURPOSE
!!    -------
!!       It is possible to include wind turbines parameterization in Meso-NH,
!!       and several models are available. One of the models is the Non-Rotating
!!       Actuator  Disk (ADNR). MODD_EOL_ADNR contains all the declarations for 
!!       the ADNR model. 
!!
!!**  AUTHOR
!!    ------
!!    PA. Joulin                   *CNRM & IFPEN*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 04/16
!!    Modification 14/10/20 (PA. Joulin) Updated for a main version
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
!
IMPLICIT NONE
!
! ------ TYPES ------
TYPE FARM
        INTEGER                         :: NNB_TURBINES     ! Number of wind turbines [-]
        REAL, DIMENSION(:), ALLOCATABLE :: XPOS_X           ! Tower base position, X coord [m]
        REAL, DIMENSION(:), ALLOCATABLE :: XPOS_Y           ! Tower base position, Y coord [m]
        REAL, DIMENSION(:), ALLOCATABLE :: XCT_INF          ! Thrust coefficient from U_infinite [-]
END TYPE FARM
!
TYPE TURBINE 
        CHARACTER(LEN=10)               :: CNAME            ! Wind turbine name [-]
        REAL                            :: XH_HEIGHT        ! Hub height [m]
        REAL                            :: XR_MAX           ! Radius max of the blade [m]
END TYPE TURBINE
!
! ------ VARIABLES ------
! Farm data
TYPE(FARM)                              :: TFARM
TYPE(TURBINE)                           :: TTURBINE
!
! Global (CPU) variables 
REAL, DIMENSION(:), ALLOCATABLE         :: XA_INDU          ! Induction factor
REAL, DIMENSION(:), ALLOCATABLE         :: XCT_D            ! Adapted thrust coef (for U_d) [-]
!
! Implicit from MODD_EOL_SHARED_IO:
!REAL, DIMENSION(:),       ALLOCATABLE :: XTHRUT         ! Thrust [N]
!REAL, DIMENSION(:),       ALLOCATABLE :: XTHRU_SUM      ! Sum of thrust (N)
!
! Namelist NAM_EOL_ADNR :
! Implicit from MODD_EOL_SHARED_IO:
!CHARACTER(LEN=100)                     :: CFARM_CSVDATA    ! File to read, with farm data
!CHARACTER(LEN=100)                     :: CTURBINE_CSVDATA ! File to read, turbine data
!CHARACTER(LEN=3)                       :: CINTERP          ! Interpolation method for wind speed
!
END MODULE MODD_EOL_ADNR
