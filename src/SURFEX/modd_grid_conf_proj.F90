!     ################
      MODULE MODD_GRID_CONF_PROJ
!     ################
!
!!****  *MODD_GRID_CONF_PROJ - declaration of Arome gris characteristics
!!
!!    PURPOSE
!!    -------
!     Used if CINGRID_TYPE = 'CONF PROJ '
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
!!      Original       20/09/02
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
REAL, DIMENSION(:), ALLOCATABLE    :: XX  ! X coordinate (meters)
REAL, DIMENSION(:), ALLOCATABLE    :: XY  ! Y coordinate (meters)
INTEGER :: NX  ! number of points in X direction
INTEGER :: NY  ! number of points in Y direction
!
REAL    :: XLAT0  ! reference latitude
REAL    :: XLON0  ! reference longitude
REAL    :: XLATORI! origin latitude
REAL    :: XLONORI! origin longitude
REAL    :: XRPK   ! projection parameter for the conformal projection
REAL    :: XBETA  ! rotation   parameter for the conformal projection
REAL    :: XLATC ! centre latitude
REAL    :: XLONC ! centre longitude
!
END MODULE MODD_GRID_CONF_PROJ
