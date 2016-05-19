!###############################################
 SUBROUTINE GET_LONLAT_TRIP(KLON,KLAT,PLON,PLAT)
!###############################################
!
!!****  *GET_LONLAT_TRIP* - routine to get the TRIP longitude and latitude
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
INTEGER,               INTENT(IN ) :: KLON
INTEGER,               INTENT(IN ) :: KLAT
REAL, DIMENSION(KLON), INTENT(OUT) :: PLON
REAL, DIMENSION(KLAT), INTENT(OUT) :: PLAT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_LONLAT_TRIP',0,ZHOOK_HANDLE)
 CALL GET_GRID_TRIP(XGRID_TRIP,PLON=PLON,PLAT=PLAT)
IF (LHOOK) CALL DR_HOOK('GET_LONLAT_TRIP',1,ZHOOK_HANDLE)
!    
!---------------------------------------------------------------------------
!
END SUBROUTINE GET_LONLAT_TRIP
