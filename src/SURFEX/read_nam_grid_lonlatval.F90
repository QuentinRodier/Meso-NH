!     ################################################################
      SUBROUTINE READ_NAM_GRID_LONLATVAL(HPROGRAM,KGRID_PAR,KL,PGRID_PAR)
!     ################################################################
!
!!****  *READ_NAM_GRID_LONLATVAL* - routine to read in namelist the horizontal grid
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
USE MODE_POS_SURF
!
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_GET_LUOUT
!
USE MODE_GRIDTYPE_LONLATVAL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),           INTENT(IN)    :: HPROGRAM   ! calling program
INTEGER,                    INTENT(INOUT) :: KGRID_PAR  ! size of PGRID_PAR
INTEGER,                    INTENT(OUT)   :: KL         ! number of points
REAL, DIMENSION(KGRID_PAR), INTENT(OUT)   :: PGRID_PAR  ! parameters defining this grid
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT ! output listing logical unit
INTEGER :: ILUNAM ! namelist file  logical unit

REAL, DIMENSION(:),   ALLOCATABLE :: ZX       ! X conformal coordinate of grid mesh
REAL, DIMENSION(:),   ALLOCATABLE :: ZY       ! Y conformal coordinate of grid mesh
REAL, DIMENSION(:),   ALLOCATABLE :: ZDX      ! X grid mesh size
REAL, DIMENSION(:),   ALLOCATABLE :: ZDY      ! Y grid mesh size
!
!*       0.3   Declarations of namelist
!              ------------------------
!
INTEGER :: NPOINTS  ! number of points
REAL, DIMENSION(100000) :: XX  ! X coordinate of grid mesh center (in meters)
REAL, DIMENSION(100000) :: XY  ! Y coordinate of grid mesh center (in meters)
REAL, DIMENSION(100000) :: XDX ! X mesh size (in meters)
REAL, DIMENSION(100000) :: XDY ! Y mesh size (in meters)
!
REAL, DIMENSION(:), POINTER :: ZGRID_PAR
!
LOGICAL :: GFOUND
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_LONLATVAL/NPOINTS,XX,XY,XDX,XDY
!
!------------------------------------------------------------------------------
!
!*       1.    opening of namelist
! 
IF (LHOOK) CALL DR_HOOK('READ_NAM_GRID_LONLATVAL',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
!---------------------------------------------------------------------------
!
!*       2.    Reading of projection parameters
!              --------------------------------
!
 CALL POSNAM(ILUNAM,'NAM_LONLATVAL',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_LONLATVAL)
!
!---------------------------------------------------------------------------
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!---------------------------------------------------------------------------
!
!*       3.    Number of points
!              ----------------
!
KL = NPOINTS
!
!---------------------------------------------------------------------------
!
!*       3.    Array of X and Y coordinates
!              ----------------------------
!
!
ALLOCATE(ZX(KL))
ALLOCATE(ZY(KL))
ZX(:) = XX(:KL)
ZY(:) = XY(:KL)
!
!---------------------------------------------------------------------------
!
!*       4.    Array of X and Y increments
!              ---------------------------
!
ALLOCATE(ZDX(KL))
ALLOCATE(ZDY(KL))
ZDX(:) = XDX(:KL)
ZDY(:) = XDY(:KL)
!
!---------------------------------------------------------------------------
!
!*       8.    All this information stored into pointer PGRID_PAR
!              --------------------------------------------------
!
 CALL PUT_GRIDTYPE_LONLATVAL(ZGRID_PAR,ZX,ZY,ZDX,ZDY)
!
!---------------------------------------------------------------------------
DEALLOCATE(ZX)
DEALLOCATE(ZY)
DEALLOCATE(ZDX)
DEALLOCATE(ZDY)
!---------------------------------------------------------------------------
!
!* 1st call : initializes dimension
!
IF (KGRID_PAR==0) THEN
  KGRID_PAR = SIZE(ZGRID_PAR)
!
ELSE
!
!* 2nd call : initializes grid array
!
  PGRID_PAR(:) = 0.
  PGRID_PAR(:) = ZGRID_PAR
END IF
!
DEALLOCATE(ZGRID_PAR)
IF (LHOOK) CALL DR_HOOK('READ_NAM_GRID_LONLATVAL',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_GRID_LONLATVAL
