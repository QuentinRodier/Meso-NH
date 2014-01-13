!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########################################################################
      SUBROUTINE LATLON_GRIDTYPE_IGN(KGRID_PAR,KL,PGRID_PAR,PLAT,PLON,PMESH_SIZE,PDIR)
!     #########################################################################
!
!!****  *LATLON_GRIDTYPE_IGN* - routine to compute the horizontal geographic fields
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
!!	E. Martin   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2007 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS,     ONLY : XPI
USE MODD_IGN, ONLY : XA, XDELTY
!
USE MODE_GRIDTYPE_IGN
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
REAL, DIMENSION(:),   ALLOCATABLE :: ZX       ! X Lambert coordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZY       ! Y  Lambertcoordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZMAP     ! map factor
REAL, DIMENSION(:),   ALLOCATABLE :: ZDX      ! size in X Lambert coordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZDY      ! size in Y Lambert coordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZYDELTY  ! Y + DELTY Lambert coordinate            
REAL, DIMENSION(:),   ALLOCATABLE :: ZLATDY   ! latitude 
REAL, DIMENSION(:),   ALLOCATABLE :: ZLONDY   ! longitude
!
INTEGER :: ILAMBERT ! Lambert type
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Projection and 2D grid parameters
!              ---------------------------------
!
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_IGN',0,ZHOOK_HANDLE)
ALLOCATE(ZX (SIZE(PLAT)))
ALLOCATE(ZY (SIZE(PLAT)))
ALLOCATE(ZDX(SIZE(PLAT)))
ALLOCATE(ZDY(SIZE(PLAT)))
!
 CALL GET_GRIDTYPE_IGN(PGRID_PAR,KLAMBERT=ILAMBERT,PX=ZX,PY=ZY,PDX=ZDX,PDY=ZDY      )
!
!---------------------------------------------------------------------------
!
!*       2.    Computation of latitude and longitude
!              -------------------------------------
!
 CALL LATLON_IGN(ILAMBERT,ZX,ZY,PLAT,PLON)
!
!-----------------------------------------------------------------------------
!
!*       3.    Compute grid size (2D array)
!              -----------------
!
!        3.1   Map factor
!              ----------
!
ALLOCATE(ZMAP(SIZE(PLAT)))
!
 CALL MAP_FACTOR_IGN(ILAMBERT,ZX,ZY,ZMAP)
!
!        3.2   Grid size
!              ---------
!
PMESH_SIZE(:) = ZDX(:) * ZDY(:) / ZMAP(:)**2
!
!-----------------------------------------------------------------------------
!
!*       4.    Direction of Y axis (from North) for each grid point
!              ----------------------------------------------------
!
!* the following formulae is given for clockwise angles.
ALLOCATE(ZYDELTY(SIZE(PLAT)))
ALLOCATE(ZLATDY (SIZE(PLAT)))
ALLOCATE(ZLONDY (SIZE(PLAT)))
ZYDELTY=ZY+XDELTY
 CALL LATLON_IGN(ILAMBERT,ZX,ZYDELTY,ZLATDY,ZLONDY)

PDIR(:)= ATAN( (XA*(ZLONDY(:)-PLON(:))*XPI/180.) / XDELTY) * XPI/180.
!
!---------------------------------------------------------------------------
DEALLOCATE(ZX)
DEALLOCATE(ZY)
DEALLOCATE(ZMAP)
DEALLOCATE(ZDX)
DEALLOCATE(ZDY)
DEALLOCATE(ZYDELTY)
DEALLOCATE(ZLATDY)
DEALLOCATE(ZLONDY)
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_IGN',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE LATLON_GRIDTYPE_IGN

