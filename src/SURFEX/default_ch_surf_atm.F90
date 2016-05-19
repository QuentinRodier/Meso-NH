!     #########
      SUBROUTINE DEFAULT_CH_SURF_ATM(HCHEM_SURF_FILE,OSURF_EMIS)
!     ########################################################################
!
!!****  *DEFAULT_CH_SURF_ATM* - routine to set default values for the choice of surface schemes
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
 CHARACTER(LEN=28),        INTENT(OUT) :: HCHEM_SURF_FILE
LOGICAL,                  INTENT(OUT) :: OSURF_EMIS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DEFAULT_CH_SURF_ATM',0,ZHOOK_HANDLE)
HCHEM_SURF_FILE = '                            '
OSURF_EMIS = .FALSE.
IF (LHOOK) CALL DR_HOOK('DEFAULT_CH_SURF_ATM',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_CH_SURF_ATM
