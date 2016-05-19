!     #########
      SUBROUTINE GET_GRID_CONF_TRIP_n(PLONMIN,PLONMAX,PLATMIN,PLATMAX,PRES,KLON,KLAT)
!     #########################################
!
!!****  *GET_GRID_CONF_TRIP_n* - routine to get the TRIP grid configuration
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
USE MODE_GRID_TRIP
USE MODD_TRIP_GRID_n,    ONLY : XGRID_TRIP
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
REAL,               INTENT(OUT) :: PLONMIN
REAL,               INTENT(OUT) :: PLONMAX
REAL,               INTENT(OUT) :: PLATMIN
REAL,               INTENT(OUT) :: PLATMAX
REAL,               INTENT(OUT) :: PRES
INTEGER,            INTENT(OUT) :: KLON
INTEGER,            INTENT(OUT) :: KLAT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_GRID_CONF_TRIP_N',0,ZHOOK_HANDLE)
 CALL GET_GRID_TRIP(XGRID_TRIP,PLONMIN,PLONMAX,PLATMIN,PLATMAX,PRES,KLON,KLAT)
IF (LHOOK) CALL DR_HOOK('GET_GRID_CONF_TRIP_N',1,ZHOOK_HANDLE)
!    
!---------------------------------------------------------------------------
!
END SUBROUTINE GET_GRID_CONF_TRIP_n
