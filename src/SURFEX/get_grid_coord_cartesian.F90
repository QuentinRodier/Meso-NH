!     ###############################################
      SUBROUTINE GET_GRID_COORD_CARTESIAN(KGRID_PAR,KL,PGRID_PAR,PX,PY)
!     ###############################################
!
!!****  *GET_GRID_COORD_CARTESIAN* - computes X and Y of ALL points
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
USE MODE_GRIDTYPE_CARTESIAN
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
REAL, DIMENSION(KL),        INTENT(OUT) :: PX         ! X (m)
REAL, DIMENSION(KL),        INTENT(OUT) :: PY         ! Y (m)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!---------------------------------------------------------------------------
!
!*       1.    2D grid parameters
!              ------------------
!
IF (LHOOK) CALL DR_HOOK('GET_GRID_COORD_CARTESIAN',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_CARTESIAN(PGRID_PAR,PX=PX,PY=PY)
IF (LHOOK) CALL DR_HOOK('GET_GRID_COORD_CARTESIAN',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE GET_GRID_COORD_CARTESIAN
