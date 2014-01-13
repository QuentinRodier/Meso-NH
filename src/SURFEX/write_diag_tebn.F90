!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE WRITE_DIAG_TEB_n(HPROGRAM,HWRITE)
!     ###############################################################################
!
!!****  *WRITE_DIAG_TEB_n * - diagnostics for TEB
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
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_TEB_n,      ONLY : TTIME, LGARDEN, XBLD, NTEB_PATCH
USE MODD_DIAG_TEB_n, ONLY : XDIAG_TSTEP, LPGD
!
USE MODI_GOTO_TEB
USE MODI_WRITE_DIAG_SEB_TEB_n
USE MODI_WRITE_DIAG_MISC_TEB_n
USE MODI_WRITE_DIAG_PGD_TEB_n
USE MODI_WRITE_DIAG_PGD_GRDN_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HWRITE   ! 'PGD' : only physiographic fields are written
!                                           ! 'ALL' : all fields are written
!
!*      0.2    declarations of local variables
!
INTEGER         :: JTEB_PATCH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_TEB_N',0,ZHOOK_HANDLE)
IF (HWRITE/='PGD') THEN
!        
   IF (XDIAG_TSTEP==XUNDEF .OR. ABS(NINT(TTIME%TIME/XDIAG_TSTEP)*XDIAG_TSTEP-TTIME%TIME)<1.E-3 ) THEN
      CALL WRITE_DIAG_SEB_TEB_n(HPROGRAM)
      DO JTEB_PATCH=1,NTEB_PATCH
        CALL GOTO_TEB(JTEB_PATCH)
        CALL WRITE_DIAG_MISC_TEB_n(HPROGRAM,JTEB_PATCH)
      END DO      
   END IF
!
ENDIF
!
IF (LPGD) THEN
  IF (XDIAG_TSTEP==XUNDEF .OR. ABS(NINT(TTIME%TIME/XDIAG_TSTEP)*XDIAG_TSTEP-TTIME%TIME)<1.E-3 ) THEN
    IF (ASSOCIATED(XBLD)) THEN
      CALL WRITE_DIAG_PGD_TEB_n(HPROGRAM)
      IF (LGARDEN) CALL WRITE_DIAG_PGD_GRDN_n(HPROGRAM)
    ENDIF
  END IF
END IF
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_TEB_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_TEB_n
