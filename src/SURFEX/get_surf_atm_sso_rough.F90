!     ########################################################################
      SUBROUTINE GET_SURF_ATM_SSO_ROUGH(HROUGH)
!     ########################################################################
!
!!****  *DEFAULT_ISBA* - routine to get value of the
!                        main logical switch for orographic roughness
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
!!	V. Masson  *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2010
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODN_SSO_n, ONLY : CROUGH
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
 CHARACTER(LEN=4), INTENT(OUT) :: HROUGH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.1   Declarations of arguments
!-------------------------------------------------------------------------------
!
! General switch
!
IF (LHOOK) CALL DR_HOOK('GET_SURF_ATM_SSO_ROUGH',0,ZHOOK_HANDLE)
HROUGH = CROUGH
IF (LHOOK) CALL DR_HOOK('GET_SURF_ATM_SSO_ROUGH',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_SURF_ATM_SSO_ROUGH
