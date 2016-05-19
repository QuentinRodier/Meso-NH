!     #########
      SUBROUTINE WRITESURF_ATM_CONF_n(HPROGRAM)
!     #########################################
!
!!****  *MNHWRITE_SURF_ATM_CONF* - routine to write the configuration for the surface
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
!!      Original    01/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODN_SSO_n
USE MODN_SURF_ATM_n
!
USE MODN_CHS_ORILAM
USE MODN_SURF_ATM
USE MODN_WRITE_SURF_ATM
!
USE MODI_GET_DEFAULT_NAM_n
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
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM ! program calling ISBA
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUDES
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_ATM_CONF_N',0,ZHOOK_HANDLE)
 CALL GET_DEFAULT_NAM_n(HPROGRAM,'WRITE',ILUDES)
!
IF (ILUDES==0 .AND. LHOOK) CALL DR_HOOK('WRITESURF_ATM_CONF_N',1,ZHOOK_HANDLE)
IF (ILUDES==0) RETURN
!
!-------------------------------------------------------------------------------
!
 CALL INIT_NAM_SSOn
 CALL INIT_NAM_CH_CONTROLn
 CALL INIT_NAM_CH_SURFn
 CALL INIT_NAM_DIAG_SURF_ATMn
 CALL INIT_NAM_DIAG_SURFn
 CALL INIT_NAM_WRITE_DIAG_SURFn
!
WRITE(UNIT=ILUDES,NML=NAM_SSOn)
WRITE(UNIT=ILUDES,NML=NAM_CH_CONTROLn)
WRITE(UNIT=ILUDES,NML=NAM_CH_SURFn)
WRITE(UNIT=ILUDES,NML=NAM_DIAG_SURF_ATMn)
WRITE(UNIT=ILUDES,NML=NAM_DIAG_SURFn)
WRITE(UNIT=ILUDES,NML=NAM_WRITE_DIAG_SURFn)
WRITE(UNIT=ILUDES,NML=NAM_CHS_ORILAM)
WRITE(UNIT=ILUDES,NML=NAM_SURF_ATM)
WRITE(UNIT=ILUDES,NML=NAM_WRITE_SURF_ATM)
IF (LHOOK) CALL DR_HOOK('WRITESURF_ATM_CONF_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_ATM_CONF_n
