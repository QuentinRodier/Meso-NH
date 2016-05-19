!     ########################################
      SUBROUTINE GET_DIM_FULL_n(KDIM_FULL)
!     ########################################
!
!!****  *GET_DIM_FULL_n* - routine to get some ISBA fields
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
!!      Original    04/2008
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_ATM_n,     ONLY : NDIM_FULL
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
INTEGER, INTENT(OUT) :: KDIM_FULL ! total number of points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_DIM_FULL_N',0,ZHOOK_HANDLE)
KDIM_FULL = NDIM_FULL
IF (LHOOK) CALL DR_HOOK('GET_DIM_FULL_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_DIM_FULL_n
