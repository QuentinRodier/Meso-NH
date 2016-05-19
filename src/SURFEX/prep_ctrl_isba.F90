!     #########
      SUBROUTINE PREP_CTRL_ISBA(K2M,OSURF_BUDGET,O2M_MIN_ZS,ORAD_BUDGET,OCOEF,OSURF_VARS,&
                                  OSURF_EVAP_BUDGET,OSURF_MISC_BUDGET,OSURF_BUDGETC,     &
                                  OPATCH_BUDGET,OSURF_MISC_DIF,KLUOUT                    )  
!     #################################################################################################################
!
!!****  *PREP_CTRL_ISBA* - routine to check that diagnostics are switched off
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
!!	P. Le Moigne   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2007 
!!      Modified by A.L. Gibelin, 04/2009: add carbon spinup
!!
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
INTEGER,  INTENT(INOUT) :: K2M                ! flag for 2m parameters
LOGICAL,  INTENT(INOUT) :: OSURF_BUDGET       ! flag for surface budget
LOGICAL,  INTENT(INOUT) :: O2M_MIN_ZS         ! flag for 2m parameters at min zs
LOGICAL,  INTENT(INOUT) :: ORAD_BUDGET        ! flag for radiative budget
LOGICAL,  INTENT(INOUT) :: OCOEF              ! flag for turbulent coefficients
LOGICAL,  INTENT(INOUT) :: OSURF_VARS         ! flag for other surface variables
LOGICAL,  INTENT(INOUT) :: OSURF_EVAP_BUDGET  ! flag for surface evaporation budget
LOGICAL,  INTENT(INOUT) :: OSURF_MISC_BUDGET  ! flag for surface miscellaneous budget
LOGICAL,  INTENT(INOUT) :: OSURF_BUDGETC      ! flag for cumulated surface budget
LOGICAL,  INTENT(INOUT) :: OPATCH_BUDGET      ! flaf for surface budget by patch
LOGICAL,  INTENT(INOUT) :: OSURF_MISC_DIF     ! flag for surface miscellaneous dif variables
INTEGER,  INTENT(IN)    :: KLUOUT             ! unit number
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_CTRL_ISBA',0,ZHOOK_HANDLE)
K2M = 0
!
OSURF_BUDGET  = .FALSE.
O2M_MIN_ZS    = .FALSE.
ORAD_BUDGET   = .FALSE.
OCOEF         = .FALSE.
OSURF_VARS    = .FALSE.
!
OSURF_BUDGETC     = .FALSE.
OPATCH_BUDGET     = .FALSE.
OSURF_EVAP_BUDGET = .FALSE.
OSURF_MISC_BUDGET = .FALSE.
OSURF_MISC_DIF    = .FALSE.
!
WRITE(KLUOUT,*)'ISBA DIAGNOSTICS DESACTIVATED'
IF (LHOOK) CALL DR_HOOK('PREP_CTRL_ISBA',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PREP_CTRL_ISBA
