!     #########
      SUBROUTINE GET_CURRENT_TEB_PATCH(KCURRENT_PATCH)
!     #######################################################
!
!!****  *GET_CURRENT_TEB_PATCH* - routine to get output TEB current patch number
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
!!      Original    12/2011 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODE_MODELN_TEB_HANDLER
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,           INTENT(OUT) :: KCURRENT_PATCH ! current TEB patch
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_CURRENT_TEB_PATCH',0,ZHOOK_HANDLE)
!
KCURRENT_PATCH = GET_CURRENT_PATCH_INDEX_TEB()
!
IF (LHOOK) CALL DR_HOOK('GET_CURRENT_TEB_PATCH',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_CURRENT_TEB_PATCH
