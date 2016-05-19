!     ##########################
      MODULE MODD_VER_INTERP_LIN_SURF
!     ##########################
!
!!****  *MODD_VER_INTERP_LIN* - declaration of linear vertical interpolation
!!                              coefficients
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_VER_INTERP_LIN)
!!      
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    18/07/97
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: NKLIN      ! level in grid1 just below
                                                     ! level in grid2
                                                     ! (level for which XCOEFLIN
                                                     !  is computed)
REAL,    DIMENSION(:,:,:), ALLOCATABLE :: XCOEFLIN   ! interpolating coefficient
                                                     ! 0< <1 : interpolation
                                                     !    <0 : extrapolation up
                                                     ! 1<    : extrapolation down
!-------------------------------------------------------------------------------
!
END MODULE MODD_VER_INTERP_LIN_SURF
