!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########################################################################
      SUBROUTINE LATLON_GRIDTYPE_CARTESIAN(KGRID_PAR,KL,PGRID_PAR,PLAT,PLON,PMESH_SIZE,PDIR)
!     #########################################################################
!
!!****  *LATLON_GRIDTYPE_CARTESIAN* - routine to compute the horizontal geographic fields
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS,     ONLY : XPI
!
USE MODE_GRIDTYPE_CARTESIAN
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                    INTENT(IN)  :: KGRID_PAR  ! size of PGRID_PAR
INTEGER,                    INTENT(IN)  :: KL         ! number of points
REAL, DIMENSION(KGRID_PAR), INTENT(IN)  :: PGRID_PAR  ! parameters defining this grid
REAL, DIMENSION(KL),        INTENT(OUT) :: PLAT       ! latitude  (degrees)
REAL, DIMENSION(KL),        INTENT(OUT) :: PLON       ! longitude (degrees)
REAL, DIMENSION(KL),        INTENT(OUT) :: PMESH_SIZE ! mesh size (m2)
REAL, DIMENSION(KL),        INTENT(OUT) :: PDIR ! direction of main grid Y axis (deg. from N, clockwise)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL                              :: ZLAT0    ! reference latitude
REAL                              :: ZLON0    ! reference longitude
REAL, DIMENSION(:),   ALLOCATABLE :: ZDX      ! size in X conformal coordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZDY      ! size in Y conformal coordinate
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------------------------------
!
!*       1.    Projection and 2D grid parameters
!              ---------------------------------
!
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_CARTESIAN',0,ZHOOK_HANDLE)
ALLOCATE(ZDX(SIZE(PLAT)))
ALLOCATE(ZDY(SIZE(PLAT)))
!
 CALL GET_GRIDTYPE_CARTESIAN(PGRID_PAR,ZLAT0,ZLON0, &
                              PDX=ZDX,PDY=ZDY        )  
!
!---------------------------------------------------------------------------
!
!*       2.    Computation of latitude and longitude
!              -------------------------------------
!
 CALL LATLON_CARTESIAN(ZLAT0,ZLON0,PLAT,PLON)
!
!-----------------------------------------------------------------------------
!
!*       3.    Compute grid size (2D array)
!              -----------------
!
PMESH_SIZE(:) = ZDX(:) * ZDY(:)
!
!-----------------------------------------------------------------------------
!
!*       4.    Direction of Y axis (from North) for each grid point
!              ----------------------------------------------------
!
PDIR(:) = 0.
!
!---------------------------------------------------------------------------
DEALLOCATE(ZDX)
DEALLOCATE(ZDY)
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_CARTESIAN',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE LATLON_GRIDTYPE_CARTESIAN

