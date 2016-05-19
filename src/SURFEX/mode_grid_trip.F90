!##############################
MODULE MODE_GRID_TRIP
!##############################
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
  INTERFACE PUT_GRID_TRIP
      MODULE PROCEDURE PUT_GRID_TRIP
  END INTERFACE

  INTERFACE GET_GRID_TRIP
      MODULE PROCEDURE GET_GRID_TRIP
  END INTERFACE
!
!############################################################################
!############################################################################
!############################################################################
CONTAINS
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE PUT_GRID_TRIP(PGRID_TRIP,PLONMIN,PLONMAX,PLATMIN, &
                                 PLATMAX,PRES,KLON,KLAT,PLON,PLAT    )  
!     ####################################################################
!
!!****  *PUT_GRID_TRIP* - routine to store in PGRID_TRIP the horizontal grid
!!
!!    AUTHOR
!!    ------
!!	B. Decharme   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2005 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
REAL,               INTENT(IN)  :: PLONMIN  ! minimum longitude
REAL,               INTENT(IN)  :: PLONMAX  ! maximum longitude
REAL,               INTENT(IN)  :: PLATMIN  ! minimum latitude
REAL,               INTENT(IN)  :: PLATMAX  ! maximum latitude
REAL,               INTENT(IN)  :: PRES     ! 1° or 0.5° resolution
INTEGER,            INTENT(IN)  :: KLON     ! number of points in longitude
INTEGER,            INTENT(IN)  :: KLAT     ! number of points in latitude
REAL, DIMENSION(:), INTENT(IN)  :: PLON     ! longitude
REAL, DIMENSION(:), INTENT(IN)  :: PLAT     ! latitude
REAL, DIMENSION(:), INTENT(OUT) :: PGRID_TRIP! parameters defining this grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_GRID_TRIP:PUT_GRID_TRIP',0,ZHOOK_HANDLE)
PGRID_TRIP(1)             = PLONMIN
PGRID_TRIP(2)             = PLONMAX
PGRID_TRIP(3)             = PLATMIN
PGRID_TRIP(4)             = PLATMAX
PGRID_TRIP(5)             = PRES
PGRID_TRIP(6)             = FLOAT(KLON)
PGRID_TRIP(7)             = FLOAT(KLAT)
PGRID_TRIP(8:7+KLON)      = PLON
PGRID_TRIP(8+KLON:7+KLON+KLAT) = PLAT
IF (LHOOK) CALL DR_HOOK('MODE_GRID_TRIP:PUT_GRID_TRIP',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE PUT_GRID_TRIP
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE GET_GRID_TRIP(PGRID_TRIP,PLONMIN,PLONMAX,PLATMIN, &
                                 PLATMAX,PRES,KLON,KLAT,PLON,PLAT    )  
!     ####################################################################
!
!!****  *GET_GRID_TRIP* - routine to get from PGRID_TRIP the horizontal grid
!!
!!    AUTHOR
!!    ------
!!	B. Decharme   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2005 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
REAL, DIMENSION(:), INTENT(IN)            :: PGRID_TRIP! parameters defining this grid
REAL,               INTENT(OUT), OPTIONAL :: PLONMIN  ! minimum longitude
REAL,               INTENT(OUT), OPTIONAL :: PLONMAX  ! maximum longitude
REAL,               INTENT(OUT), OPTIONAL :: PLATMIN  ! minimum latitude
REAL,               INTENT(OUT), OPTIONAL :: PLATMAX  ! maximum latitude
REAL,               INTENT(OUT), OPTIONAL :: PRES     ! 1° or 0.5° resolution
INTEGER,            INTENT(OUT), OPTIONAL :: KLON     ! number of points in longitude
INTEGER,            INTENT(OUT), OPTIONAL :: KLAT     ! number of points in latitude
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PLON     ! longitude
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PLAT     ! latitude
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILON, ILAT
INTEGER :: IL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_GRID_TRIP:GET_GRID_TRIP',0,ZHOOK_HANDLE)
ILON = NINT(PGRID_TRIP(6))
ILAT = NINT(PGRID_TRIP(7))
!
IF (PRESENT(PLONMIN))  PLONMIN = PGRID_TRIP(1)
IF (PRESENT(PLONMAX))  PLONMAX = PGRID_TRIP(2)
IF (PRESENT(PLATMIN))  PLATMIN = PGRID_TRIP(3)
IF (PRESENT(PLATMAX))  PLATMAX = PGRID_TRIP(4)
IF (PRESENT(PRES   ))  PRES    = PGRID_TRIP(5)
IF (PRESENT(KLON   ))  KLON    = ILON
IF (PRESENT(KLAT   ))  KLAT    = ILAT
IF (PRESENT(PLON   ))  PLON(:) = PGRID_TRIP(8:7+ILON)
IF (PRESENT(PLAT   ))  PLAT(:) = PGRID_TRIP(8+ILON:7+ILON+ILAT)
IF (LHOOK) CALL DR_HOOK('MODE_GRID_TRIP:GET_GRID_TRIP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE GET_GRID_TRIP
!---------------------------------------------------------------------------------
!
!############################################################################
!############################################################################
!############################################################################
!############################################################################
!############################################################################
!############################################################################
!
END MODULE MODE_GRID_TRIP
