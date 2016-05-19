!     ################
      MODULE MODD_GRID_GAUSS
!     ################
!
!!****  *MODD_GRID_GAUSS - declaration of Gauss grid characteristics
!!
!!    PURPOSE
!!    -------
!     Used if CINGRID_TYPE = 'GAUSS '
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
!!	V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2004
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
REAL                               :: XILA1   ! Lat. (y) of first input point
REAL                               :: XILO1   ! Lon. (x) of first input point
REAL                               :: XILA2   ! Lat. (y) of last input point
REAL                               :: XILO2   ! Lon. (x) of last input point
INTEGER                            :: NINLA   ! Number of parallels
INTEGER, DIMENSION(:), ALLOCATABLE :: NINLO   ! Nb. of points on each parallel
INTEGER                            :: NILEN   ! size of input arrays
!
LOGICAL                             :: LROTPOLE! .TRUE. if pole is rotated
REAL                                :: XLAP    ! Latitude of stretching pole
REAL                                :: XLOP    ! Longitude of stretching pole
REAL                                :: XCOEF   ! Stretching coefficient
!
END MODULE MODD_GRID_GAUSS

