!MNH_LIC Copyright 2017-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!!
!!    #####################
      MODULE MODD_EOL_MAIN
!!    #####################
!!
!!*** *MODD_EOL_MAIN*
!!
!!    PURPOSE
!!    -------
!!       It is possible to include wind turbines parameterization in Meso-NH,
!!       and several methods are available. MODD_EOL_MAIN contains all the 
!!       main declarations. 
!!
!!**  AUTHOR
!!    ------
!!    PA.Joulin                   *CNRM & IFPEN*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 24/01/17
!!    Modification 14/10/20 (PA. Joulin) Updated for a main version
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
USE MODD_PARAMETERS
!
!
! Necessary for each models
IMPLICIT NONE
!
! ------ VARIABLES ------
!
! Aerodynamic forces in cartesian mesh
REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XFX_RG     ! Along X in RG frame [F]
REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XFY_RG     ! Along Y in RG frame [F]
REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XFZ_RG     ! Along Z in RG frame [F]
! Smeared forces 
REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XFX_SMR_RG ! Along X in RG frame [F]
REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XFY_SMR_RG ! Along Y in RG frame [F]
REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XFZ_SMR_RG ! ALong Z in RG frame [F]
!
! Namelist NAM_EOL :
LOGICAL          :: LMAIN_EOL     ! Flag to take into account wind turbine
CHARACTER(LEN=4) :: CMETH_EOL     ! Aerodynamic method
CHARACTER(LEN=4) :: CSMEAR        ! Type of smearing
INTEGER          :: NMODEL_EOL    ! Son number, where the wind farm is
!
END MODULE MODD_EOL_MAIN
