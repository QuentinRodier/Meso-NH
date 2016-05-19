!     #########
      SUBROUTINE GET_TRIP_SIZE_n(KX,KY)
!     #####################################################
!
!!****  *GET_TRIP_SIZE_n* - routine to get X/Y size
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    -------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	B. Decharme    *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2008
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TRIP_GRID_n, ONLY : XAREA
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
!
INTEGER,           INTENT(OUT)   :: KX       ! number of X points of this surface type
INTEGER,           INTENT(OUT)   :: KY       ! number of Y points of this surface type
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_TRIP_SIZE_N',0,ZHOOK_HANDLE)
KX = SIZE(XAREA,1)
KY = SIZE(XAREA,2)
IF (LHOOK) CALL DR_HOOK('GET_TRIP_SIZE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_TRIP_SIZE_n
