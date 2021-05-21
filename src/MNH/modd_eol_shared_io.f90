!MNH_LIC Copyright 1994-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!!
!!    #####################
      MODULE MODD_EOL_SHARED_IO
!!    #####################
!!
!!*** *MODD_EOL_SHARED_IO*
!!
!!    PURPOSE
!!    -------
!!       It is possible to include wind turbines parameterization in Meso-NH,
!!       and several models are available. 
!!
!!       MODD_EOL_SHARED_IO contains the declarations for the inputs/output
!!       shared by the differents models.
!!
!!
!!**  AUTHOR
!!    ------
!!    PA.Joulin                   *CNRM & IFPEN*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 17/11/20
!!
!-----------------------------------------------------------------------------
!
IMPLICIT NONE
!
!*       1.   INPUTS VAR
!        ---------------
!
! --- Namelistis NAM_EOL_ADNR/NAM_EOL_ALM ---
! * .csv files
CHARACTER(LEN=100) :: CFARM_CSVDATA     ! Farm file to read 
CHARACTER(LEN=100) :: CTURBINE_CSVDATA  ! Turbine file to read  
CHARACTER(LEN=100) :: CBLADE_CSVDATA    ! Blade file to read  
CHARACTER(LEN=100) :: CAIRFOIL_CSVDATA  ! Airfoil file to read  
! * flags
CHARACTER(LEN=3)   :: CINTERP           ! Interpolation method for wind speed
!
!
!*       2.   OUTPUTS VAR
!        -----------------
!
! --- Thruts torque and power ---
REAL, DIMENSION(:),       ALLOCATABLE :: XTHRUT         ! Thrust [N]
REAL, DIMENSION(:),       ALLOCATABLE :: XTORQT         ! Torque [Nm]
REAL, DIMENSION(:),       ALLOCATABLE :: XPOWT          ! Power [W]
REAL, DIMENSION(:),       ALLOCATABLE :: XTHRU_SUM      ! Sum of thrust (N)
REAL, DIMENSION(:),       ALLOCATABLE :: XTORQ_SUM      ! Sum of torque (Nm)
REAL, DIMENSION(:),       ALLOCATABLE :: XPOW_SUM       ! Sum of power (W)
!
END MODULE MODD_EOL_SHARED_IO
