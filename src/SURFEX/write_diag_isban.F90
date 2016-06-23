!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE WRITE_DIAG_ISBA_n (DTCO, DGU, U, IM, DST, &
                              HPROGRAM,HWRITE)
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
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
USE MODD_DST_n, ONLY : DST_t
!
USE MODD_SURF_PAR,    ONLY : XUNDEF
! 
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_WRITE_DIAG_MISC_ISBA_n
USE MODI_WRITE_DIAG_PGD_ISBA_n
USE MODI_WRITE_DIAG_SEB_ISBA_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(DST_t), INTENT(INOUT) :: DST
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
  IF (IM%DGI%XDIAG_TSTEP==XUNDEF .OR. &
          ABS(NINT(IM%I%TTIME%TIME/IM%DGI%XDIAG_TSTEP)*IM%DGI%XDIAG_TSTEP-IM%I%TTIME%TIME)<1.E-3 ) THEN
    CALL WRITE_DIAG_SEB_ISBA_n(DTCO, DGU, U, IM%CHI, IM%DGEI, IM%DGI, DST, IM%GB, IM%I, &
                               HPROGRAM)
    CALL WRITE_DIAG_MISC_ISBA_n(DTCO, DGU, U, IM%DGI, IM%DGMI, IM%I, &
                                HPROGRAM)
  END IF
END IF
!
IF (IM%DGI%LPGD) THEN
  IF (IM%DGI%XDIAG_TSTEP==XUNDEF .OR. &
                ABS(NINT(IM%I%TTIME%TIME/IM%DGI%XDIAG_TSTEP)*IM%DGI%XDIAG_TSTEP-IM%I%TTIME%TIME)<1.E-3 ) THEN
    CALL WRITE_DIAG_PGD_ISBA_n(DTCO, DGU, U, IM%CHI, IM%DGMI, IM%I, &
                               HPROGRAM)
  END IF
END IF
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_ISBA_n
