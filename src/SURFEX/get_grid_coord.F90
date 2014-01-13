!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_GRID_COORD(KLUOUT,PX,PY,KL,HGRID,PGRID_PAR)
!     #######################################
!!
!!    PURPOSE
!!    -------
!!     Gets the coordinates of all points in the natural system of each projection
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     01/2004
!!                 10/2007  E. Martin  IGN Grid
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_ATM_GRID_n, ONLY : CGRID, NGRID_PAR, XGRID_PAR
USE MODD_SURF_ATM_n,      ONLY : NSIZE_FULL
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_GET_GRID_COORD_CARTESIAN
!
USE MODI_GET_GRID_COORD_CONF_PROJ
!
USE MODI_GET_GRID_COORD_GAUSS
!
USE MODI_GET_GRID_COORD_IGN
!
USE MODI_GET_GRID_COORD_LONLAT_REG
!
USE MODI_GET_GRID_COORD_LONLATVAL
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
INTEGER,                      INTENT(IN)  :: KLUOUT ! output listing logical unit
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PX     ! X natural coordinate in the projection
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PY     ! X natural coordinate in the projection
INTEGER,            OPTIONAL, INTENT(IN)  :: KL         ! number of points
 CHARACTER(LEN=10),  OPTIONAL, INTENT(IN)  :: HGRID      ! grid type
REAL, DIMENSION(:), OPTIONAL, POINTER     :: PGRID_PAR  ! parameters defining this grid
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(:), ALLOCATABLE :: ZX
REAL, DIMENSION(:), ALLOCATABLE :: ZY
!
 CHARACTER(LEN=10)           :: YGRID
REAL, DIMENSION(:), POINTER :: ZGRID_PAR
INTEGER                     :: IGRID_PAR
INTEGER                     :: IL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_GRID_COORD',0,ZHOOK_HANDLE)
IF (PRESENT(HGRID)) THEN
  YGRID = HGRID
  IGRID_PAR = SIZE(PGRID_PAR)
  IL = KL
  ALLOCATE(ZGRID_PAR(IGRID_PAR))
  ZGRID_PAR = PGRID_PAR
ELSE
  YGRID = CGRID
  IGRID_PAR = NGRID_PAR
  IL = NSIZE_FULL
  ALLOCATE(ZGRID_PAR(IGRID_PAR))
  ZGRID_PAR = XGRID_PAR
END IF
!
ALLOCATE(ZX(IL))
ALLOCATE(ZY(IL))
!
!
SELECT CASE (YGRID)
!
!*    1.      Conformal projection grid
!             -------------------------
!
      CASE ('CONF PROJ ')
        CALL GET_GRID_COORD_CONF_PROJ(IGRID_PAR,IL,ZGRID_PAR,ZX,ZY)
        ! note that all points of the grid will be kept, whatever the surface
        ! type under consideration (e.g. sea points will be kept even for
        ! initialization of continents)
        !
!
!*    2.      Regular latlon grid
!             -------------------
!
      CASE ('LONLAT REG')
        CALL GET_GRID_COORD_LONLAT_REG(IGRID_PAR,IL,ZGRID_PAR,ZX,ZY)

!
!*    3.      Cartesian grid
!             --------------
!
      CASE ('CARTESIAN ')
        CALL GET_GRID_COORD_CARTESIAN(IGRID_PAR,IL,ZGRID_PAR,ZX,ZY)
        ! note that all points of the grid will be kept, whatever the surface
        ! type under consideration (e.g. sea points will be kept even for
        ! initialization of continents)
        !
!
!*    4.      Gaussian grid
!             -------------
!
      CASE ('GAUSS     ')
        CALL GET_GRID_COORD_GAUSS(IGRID_PAR,IL,ZGRID_PAR,ZX,ZY)
!
!*    5.      IGN grid
!             -------------
!
      CASE ('IGN       ')
        CALL GET_GRID_COORD_IGN(IGRID_PAR,IL,ZGRID_PAR,ZX,ZY)

!
!*    5.      lonlatval
!             -------------
!
      CASE ('LONLATVAL ')
        CALL GET_GRID_COORD_LONLATVAL(IGRID_PAR,IL,ZGRID_PAR,ZX,ZY)

!
!*    5.      Other cases
!             -----------
!
      CASE DEFAULT
        CALL ABOR1_SFX('GET_GRID_COORD: GRID TYPE '//YGRID//' NOT SUPPORTED')

END SELECT
!
IF(PRESENT(PX)) PX(:)=ZX(:)
IF(PRESENT(PY)) PY(:)=ZY(:)
!
DEALLOCATE(ZX)
DEALLOCATE(ZY)
DEALLOCATE(ZGRID_PAR)
IF (LHOOK) CALL DR_HOOK('GET_GRID_COORD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_GRID_COORD
