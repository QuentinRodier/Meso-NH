!     #########
      SUBROUTINE WRITE_GRID(HPROGRAM,HGRID,PGRID_PAR,PLAT,PLON,PMESH_SIZE,KRESP,PDIR,HDIR)
!     #########################################
!
!!****  *WRITE_GRID* - routine to write the horizontal grid of a scheme
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
USE MODI_WRITE_SURF
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_WRITE_GRIDTYPE_CARTESIAN
!
USE MODI_WRITE_GRIDTYPE_CONF_PROJ
!
USE MODI_WRITE_GRIDTYPE_GAUSS
!
USE MODI_WRITE_GRIDTYPE_IGN
!
USE MODI_WRITE_GRIDTYPE_LONLAT_REG
!
USE MODI_WRITE_GRIDTYPE_LONLATVAL
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM   ! calling program
 CHARACTER(LEN=10),  INTENT(IN)  :: HGRID      ! type of horizontal grid
REAL, DIMENSION(:), POINTER     :: PGRID_PAR  ! parameters defining this grid
REAL, DIMENSION(:), INTENT(IN)  :: PLAT       ! latitude  (degrees)
REAL, DIMENSION(:), INTENT(IN)  :: PLON       ! longitude (degrees)
REAL, DIMENSION(:), INTENT(IN)  :: PMESH_SIZE ! horizontal mesh size (m2)
INTEGER,            INTENT(OUT) :: KRESP      ! error return code
REAL, DIMENSION(:), INTENT(IN) , OPTIONAL :: PDIR ! heading of main axis of grid compared to North (degrees)
 CHARACTER(LEN=1),    INTENT(IN), OPTIONAL :: HDIR ! type of field :
                                                  ! 'H' : field with
                                                  !       horizontal spatial dim.
                                                  ! 'A' : (complete) field with
                                                  !       horizontal spatial dim.
                                                  ! '-' : no horizontal dim.
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=100) :: YCOMMENT
 CHARACTER(LEN=1) :: YDIR
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Write type of grid
!              ------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_GRID',0,ZHOOK_HANDLE)
YCOMMENT='GRID TYPE'
 CALL WRITE_SURF(HPROGRAM,'GRID_TYPE',HGRID,KRESP,YCOMMENT)
!
!---------------------------------------------------------------------------
!
!*       2.    Write parameters of the grid
!              ----------------------------
!
YDIR='H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
SELECT CASE (HGRID)
  CASE("CONF PROJ ")
    CALL WRITE_GRIDTYPE_CONF_PROJ(HPROGRAM,SIZE(PLAT),SIZE(PGRID_PAR),PGRID_PAR(:),KRESP,YDIR)
  CASE("CARTESIAN ")
    CALL WRITE_GRIDTYPE_CARTESIAN(HPROGRAM,SIZE(PLAT),SIZE(PGRID_PAR),PGRID_PAR(:),KRESP,YDIR)
  CASE("LONLAT REG")
    CALL WRITE_GRIDTYPE_LONLAT_REG(HPROGRAM,SIZE(PLAT),SIZE(PGRID_PAR),PGRID_PAR(:),KRESP)
  CASE("GAUSS     ")
    CALL WRITE_GRIDTYPE_GAUSS(HPROGRAM,SIZE(PLAT),SIZE(PGRID_PAR),PGRID_PAR(:),KRESP)
  CASE("IGN       ")
    CALL WRITE_GRIDTYPE_IGN(HPROGRAM,SIZE(PLAT),SIZE(PGRID_PAR),PGRID_PAR(:),KRESP)
  CASE("LONLATVAL ")
    CALL WRITE_GRIDTYPE_LONLATVAL(HPROGRAM,SIZE(PLAT),SIZE(PGRID_PAR),PGRID_PAR(:),KRESP)
  CASE("NONE      ")
    YCOMMENT='LON (DEGREES)'
    CALL WRITE_SURF(HPROGRAM,'LON',      PLON,KRESP,YCOMMENT)
    IF (KRESP/=0 .AND. LHOOK) CALL DR_HOOK('WRITE_GRID',1,ZHOOK_HANDLE)
    IF (KRESP/=0) RETURN
    YCOMMENT='LAT (DEGREES)'
    CALL WRITE_SURF(HPROGRAM,'LAT',      PLAT,KRESP,YCOMMENT)
    IF (KRESP/=0 .AND. LHOOK) CALL DR_HOOK('WRITE_GRID',1,ZHOOK_HANDLE)
    IF (KRESP/=0) RETURN
    YCOMMENT='MESH SIZE (M2)'
    CALL WRITE_SURF(HPROGRAM,'MESH_SIZE',PMESH_SIZE,KRESP,YCOMMENT)
    IF (KRESP/=0 .AND. LHOOK) CALL DR_HOOK('WRITE_GRID',1,ZHOOK_HANDLE)
    IF (KRESP/=0) RETURN
END SELECT
IF (LHOOK) CALL DR_HOOK('WRITE_GRID',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE WRITE_GRID
