!     ##################
      SUBROUTINE SURF_VERSION
!     ##################
!
!!****  *SURF_VERSION * - subroutine to initialize the surface version
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to initialize NVERSION and NBUGFIX
!     corresponding to the version chosen by the user.
!       The user can also set the name of his own binary library
!      These values will be writen in the output files
!
!!
!!    AUTHOR
!!    ------
!!	V. Masson          * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2004
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
USE MODD_SURF_PAR, ONLY : NVERSION,NBUGFIX
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('SURF_VERSION',0,ZHOOK_HANDLE)
NVERSION = 7
NBUGFIX  = 3
IF (LHOOK) CALL DR_HOOK('SURF_VERSION',1,ZHOOK_HANDLE)
!
END SUBROUTINE SURF_VERSION
