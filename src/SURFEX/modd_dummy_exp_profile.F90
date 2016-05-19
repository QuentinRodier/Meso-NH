!     ###########################
      MODULE MODD_DUMMY_EXP_PROFILE
!     ###########################
!
!!****  *MODD_DUMMY_EXP_PROFILE - declaration For special f, dc exponential profile
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!     B. Vincendon
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/08/11
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TOPD_PAR, ONLY : JPCAT
!
IMPLICIT NONE
!-------------------------------------------------------------------------------
! **** For special f, dc exponential profile
! values for each isba_mesh
REAL, DIMENSION(:), ALLOCATABLE :: XF_PARAM
REAL, DIMENSION(:), ALLOCATABLE :: XC_DEPTH_RATIO
!values for each catchment
REAL, DIMENSION(JPCAT) :: XF_PARAM_BV
REAL, DIMENSION(JPCAT) :: XC_DEPTH_RATIO_BV
!-------------------------------------------------------------------------------------
!
END MODULE MODD_DUMMY_EXP_PROFILE

