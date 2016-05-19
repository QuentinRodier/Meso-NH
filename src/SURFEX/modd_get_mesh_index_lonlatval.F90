!     ##############################
MODULE MODD_GET_MESH_INDEX_LONLATVAL
!     ##############################
!
!!****  *MODD_GRID_GAUSS - declaration of conformal grid characteristics for
!                          routine get_mesh_index_IGN
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
REAL, DIMENSION(:), ALLOCATABLE   :: XXLIM    ! X left   limit of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: XYLIM    ! Y bottom limit of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: XDX      ! X size of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: XDY      ! Y size of grid mesh
REAL                              :: XX_MIN   ! minimum X of the whole grid
REAL                              :: XX_MAX   ! maximum X of the whole grid
REAL                              :: XY_MIN   ! minimum Y of the whole grid
REAL                              :: XY_MAX   ! maximum Y of the whole grid

END MODULE MODD_GET_MESH_INDEX_LONLATVAL
