!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE WRITE_DIAG_SEAFLUX_n(HPROGRAM,HWRITE)
!     ###############################################################################
!
!!****  *WRITE_DIAG_SEAFLUX_n * - diagnostics for SEAFLUX
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
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SEAFLUX_n,      ONLY : TTIME
USE MODD_DIAG_OCEAN_n,   ONLY : LDIAG_OCEAN
USE MODD_DIAG_SEAFLUX_n, ONLY : XDIAG_TSTEP
!
USE MODI_WRITE_DIAG_SEB_SEAFLUX_n
USE MODI_WRITE_DIAG_SEB_OCEAN_n
! 
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
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                                           ! 'ALL' : all fields are written
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEAFLUX_N',0,ZHOOK_HANDLE)
IF (HWRITE/='PGD') THEN
!        
   IF (XDIAG_TSTEP==XUNDEF .OR. ABS(NINT(TTIME%TIME/XDIAG_TSTEP)*XDIAG_TSTEP-TTIME%TIME)<1.E-3 ) THEN
      CALL WRITE_DIAG_SEB_SEAFLUX_n(HPROGRAM)
      IF (LDIAG_OCEAN) CALL WRITE_DIAG_SEB_OCEAN_n(HPROGRAM)
   END IF
!        
ENDIF
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEAFLUX_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_SEAFLUX_n
