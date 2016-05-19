!     #########
SUBROUTINE WRITE_DIAG_ISBA_n(HPROGRAM,HWRITE)
!     ###############################################################################
!
!!****  *WRITE_DIAG_ISBA_n * - Stores ISBA diagnostics
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!
!
USE MODD_SURF_PAR,    ONLY : XUNDEF
USE MODD_ISBA_n,      ONLY : TTIME
USE MODD_DIAG_ISBA_n, ONLY : XDIAG_TSTEP, LPGD
! 
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_WRITE_DIAG_MISC_ISBA_n
USE MODI_WRITE_DIAG_PGD_ISBA_n
USE MODI_WRITE_DIAG_SEB_ISBA_n
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HWRITE    ! 'PGD' : only physiographic fields are written
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                                            ! 'ALL' : all fields are written
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_ISBA_N',0,ZHOOK_HANDLE)
IF (HWRITE/='PGD') THEN
  IF (XDIAG_TSTEP==XUNDEF .OR. ABS(NINT(TTIME%TIME/XDIAG_TSTEP)*XDIAG_TSTEP-TTIME%TIME)<1.E-3 ) THEN
    CALL WRITE_DIAG_SEB_ISBA_n(HPROGRAM)
    CALL WRITE_DIAG_MISC_ISBA_n(HPROGRAM)
  END IF
END IF
!
IF (LPGD) THEN
  IF (XDIAG_TSTEP==XUNDEF .OR. ABS(NINT(TTIME%TIME/XDIAG_TSTEP)*XDIAG_TSTEP-TTIME%TIME)<1.E-3 ) THEN
    CALL WRITE_DIAG_PGD_ISBA_n(HPROGRAM)
  END IF
END IF
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_ISBA_n
