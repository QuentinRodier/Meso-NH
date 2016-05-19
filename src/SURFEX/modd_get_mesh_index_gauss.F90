!     ##########################
MODULE MODD_GET_MESH_INDEX_GAUSS
!     ##########################
!
!!****  *MODD_GRID_GAUSS - declaration of Gauss grid characteristics for
!                          routine get_mesh_index_gauss
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
!!	V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/2006
!
IMPLICIT NONE
!
!*       0.   DECLARATIONS
!             ------------
!
INTEGER                           :: NNLATI   ! number of pseudo-latitudes
INTEGER, DIMENSION(:), ALLOCATABLE:: NNLOPA   ! number of pseudo-longitudes
!                                             ! on each pseudo-latitude circle
REAL                              :: XLAPO    ! latitude  of the rotated pole (deg)
REAL                              :: XLOPO    ! longitude of the rotated pole (deg)
REAL                              :: XCODIL   ! stretching factor
!
LOGICAL                           :: LROTSTRETCH ! If true, rotated pole and/or stretching
!
REAL, DIMENSION(:), ALLOCATABLE   :: XXCEN    ! pseudo-longitude of center of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: XYCEN    ! pseudo-latitude  of center of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: XXINF    ! pseudo-longitude western   limit of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: XXSUP    ! pseudo-longitude eastern   limit of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: XXMAX    ! pseudo-longitude eastern   limit of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: XYINF    ! pseudo-latitude  southern  limit of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: XYSUP    ! pseudo-latitude  northern  limit of grid mesh
!
INTEGER, DIMENSION(:), ALLOCATABLE::IINDEX_1KM
INTEGER, DIMENSION(:), ALLOCATABLE::IINDEX_10KM
INTEGER, DIMENSION(:), ALLOCATABLE::IINDEX_100KM
INTEGER, DIMENSION(:), ALLOCATABLE::IISSOX_1KM
INTEGER, DIMENSION(:), ALLOCATABLE::IISSOX_10KM
INTEGER, DIMENSION(:), ALLOCATABLE::IISSOX_100KM
INTEGER, DIMENSION(:), ALLOCATABLE::IISSOY_1KM
INTEGER, DIMENSION(:), ALLOCATABLE::IISSOY_10KM
INTEGER, DIMENSION(:), ALLOCATABLE::IISSOY_100KM
!
INTEGER, DIMENSION(:), ALLOCATABLE::IMASK_GAUSS
!
END MODULE MODD_GET_MESH_INDEX_GAUSS
