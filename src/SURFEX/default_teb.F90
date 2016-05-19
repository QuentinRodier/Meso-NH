!     #########
      SUBROUTINE DEFAULT_TEB(HZ0H,PTSTEP,POUT_TSTEP, HCH_BEM)
!     ########################################################################
!
!!****  *DEFAULT_TEB* - routine to set default values for the configuration for TEB scheme
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
!!      modified    08/2012 G. Pigeon, add HCH_BEM for building conv. coef. 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
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
 CHARACTER(LEN=6),  INTENT(OUT) :: HZ0H       ! TEB option for z0h roof & road
 CHARACTER(LEN=5),  INTENT(OUT) :: HCH_BEM    ! TEB option building conv. coef.
REAL,              INTENT(OUT) :: PTSTEP     ! time step for run
REAL,              INTENT(OUT) :: POUT_TSTEP ! time step for writing
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_TEB',0,ZHOOK_HANDLE)
HZ0H       = 'MASC95'
PTSTEP     = XUNDEF
POUT_TSTEP = XUNDEF
HCH_BEM    = ''
IF (LHOOK) CALL DR_HOOK('DEFAULT_TEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_TEB
