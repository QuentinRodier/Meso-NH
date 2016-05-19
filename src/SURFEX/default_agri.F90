!     #########
      SUBROUTINE DEFAULT_AGRI(OAGRIP)
!     ########################################################################
!
!!****  *DEFAULT_ISBA* - routine to set default values for 
!                        main logical switch for irrigation
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
!!	L. Jarlan  *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2005
!!      Modified by P. Le Moigne (06/2006): seeding and irrigation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
LOGICAL, INTENT(OUT) :: OAGRIP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.1   Declarations of arguments
!-------------------------------------------------------------------------------
!
! General switch
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_AGRI',0,ZHOOK_HANDLE)
OAGRIP = .FALSE.
IF (LHOOK) CALL DR_HOOK('DEFAULT_AGRI',1,ZHOOK_HANDLE)
!
END SUBROUTINE DEFAULT_AGRI
