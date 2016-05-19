!     ################
      MODULE MODD_GRID_LATLONREGUL
!     ################
!
!!****  *MODD_GRID_LATLONREGUL - declaration of Gauss grid characteristics
!!
!!    PURPOSE
!!    -------
!     Used if CINGRID_TYPE = 'LATLON'
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
!!	C. Lebeaupin Brossier    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2008
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
REAL                               :: XILAT1   ! Lat. (y) of first input point
REAL                               :: XILON1   ! Lon. (x) of first input point
REAL                               :: XILAT2   ! Lat. (y) of last input point
REAL                               :: XILON2   ! Lon. (x) of last input point
INTEGER                            :: NINLAT   ! Number of parallels
INTEGER , DIMENSION(:), ALLOCATABLE:: NINLON   ! Nb. of points on each parallel
INTEGER                            :: NILENGTH ! size of input arrays
INTEGER                            :: NINDEPTH  ! nb of vertical levels
REAL, DIMENSION(:), ALLOCATABLE    :: XILATARRAY! latitudes values array
REAL, DIMENSION(:), ALLOCATABLE    :: XILONARRAY! longitudes values array
REAL, DIMENSION(:), ALLOCATABLE    :: XIDEPARRAY! depths values array
!
END MODULE MODD_GRID_LATLONREGUL
