!     #########
      SUBROUTINE READ_GRID(HPROGRAM,HGRID,PGRID_PAR,PLAT,PLON,PMESH_SIZE,KRESP,PDIR)
!     #########################################
!
!!****  *READ_GRID* - routine to initialise the horizontal grid of a scheme
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
!!      Original    01/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_LATLON_GRID
USE MODI_READ_GRIDTYPE
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
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM   ! calling program
 CHARACTER(LEN=10),  INTENT(OUT) :: HGRID      ! type of horizontal grid
REAL, DIMENSION(:), POINTER     :: PGRID_PAR  ! parameters defining this grid
REAL, DIMENSION(:), INTENT(OUT) :: PLAT       ! latitude  (degrees)
REAL, DIMENSION(:), INTENT(OUT) :: PLON       ! longitude (degrees)
REAL, DIMENSION(:), INTENT(OUT) :: PMESH_SIZE ! horizontal mesh size (m2)
INTEGER,            INTENT(OUT) :: KRESP      ! error return code
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PDIR ! heading of main axis of grid compared to North (degrees)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: IGRID_PAR
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Reading of type of grid
!              -----------------------
!
IF (LHOOK) CALL DR_HOOK('READ_GRID',0,ZHOOK_HANDLE)
 CALL READ_SURF(HPROGRAM,'GRID_TYPE',HGRID,KRESP)
!
!---------------------------------------------------------------------------
!
!*       2.    Reading parameters of the grid
!              ------------------------------
!
 CALL READ_GRIDTYPE(HPROGRAM,HGRID,IGRID_PAR,SIZE(PLAT),.FALSE.)
!
ALLOCATE(PGRID_PAR(IGRID_PAR))
 CALL READ_GRIDTYPE(HPROGRAM,HGRID,IGRID_PAR,SIZE(PLAT),.TRUE.,PGRID_PAR,KRESP)
!
!---------------------------------------------------------------------------
!
!*       3.    Latitude, longitude, mesh size
!              ------------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
SELECT CASE (HGRID)
  CASE("NONE      ")
    IF (PRESENT(PDIR)) PDIR(:) = 0.
    !
    CALL READ_SURF(HPROGRAM,'LON',      PLON,KRESP)
    IF (KRESP/=0 .AND. LHOOK) CALL DR_HOOK('READ_GRID',1,ZHOOK_HANDLE)
    IF (KRESP/=0) RETURN
    CALL READ_SURF(HPROGRAM,'LAT',      PLAT,KRESP)
    IF (KRESP/=0 .AND. LHOOK) CALL DR_HOOK('READ_GRID',1,ZHOOK_HANDLE)
    IF (KRESP/=0) RETURN
    CALL READ_SURF(HPROGRAM,'MESH_SIZE',PMESH_SIZE,KRESP)
    IF (KRESP/=0 .AND. LHOOK) CALL DR_HOOK('READ_GRID',1,ZHOOK_HANDLE)
    IF (KRESP/=0) RETURN

  CASE DEFAULT
    IF (PRESENT(PDIR)) THEN
      CALL LATLON_GRID(HGRID,SIZE(PGRID_PAR),SIZE(PLAT),ILUOUT,PGRID_PAR,PLAT,PLON,PMESH_SIZE,PDIR)
    ELSE
      CALL LATLON_GRID(HGRID,SIZE(PGRID_PAR),SIZE(PLAT),ILUOUT,PGRID_PAR,PLAT,PLON,PMESH_SIZE)
    END IF

END SELECT
IF (LHOOK) CALL DR_HOOK('READ_GRID',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE READ_GRID
