!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE WRITE_DIAG_TEB_n (DTCO, DGU, U, TM, GDM, GRM, &
                             HPROGRAM,HWRITE)
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
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURFEX_n, ONLY : TEB_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_GOTO_WRAPPER_TEB_PATCH
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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
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
   IF (TM%DGT%XDIAG_TSTEP==XUNDEF .OR. &
         ABS(NINT(TM%TOP%TTIME%TIME/TM%DGT%XDIAG_TSTEP)*TM%DGT%XDIAG_TSTEP-TM%TOP%TTIME%TIME)<1.E-3 ) THEN
      CALL WRITE_DIAG_SEB_TEB_n(DTCO, DGU, U, TM%CHT, TM%DGT, TM%DGUT, &
                                HPROGRAM)
      DO JTEB_PATCH=1,TM%TOP%NTEB_PATCH
        CALL GOTO_WRAPPER_TEB_PATCH(TM%B, TM%DGCT, TM%DGMT, TM%T, &
                                    GDM%TGD, GDM%TGDPE, GRM%TGR, GRM%TGRPE, JTEB_PATCH)
        CALL WRITE_DIAG_MISC_TEB_n(DTCO, DGU, U, TM%DGCT, TM%DGMT, TM%DGMTO, TM%T, TM%TOP, &
                                   HPROGRAM,JTEB_PATCH)
      END DO      
   END IF
!
ENDIF
!
IF (TM%DGT%LPGD) THEN
  IF (TM%DGT%XDIAG_TSTEP==XUNDEF .OR. &
          ABS(NINT(TM%TOP%TTIME%TIME/TM%DGT%XDIAG_TSTEP)*TM%DGT%XDIAG_TSTEP-TM%TOP%TTIME%TIME)<1.E-3 ) THEN
    IF (ASSOCIATED(TM%T%CUR%XBLD)) THEN
      CALL WRITE_DIAG_PGD_TEB_n(DTCO, DGU, U, TM%B, TM%BOP, TM%T, TM%TOP, TM%TPN, &
                                HPROGRAM)
      IF (TM%TOP%LGARDEN) CALL WRITE_DIAG_PGD_GRDN_n(DTCO, DGU, U, TM%DGMTO, GDM%TGDPE, GDM%TGDP, GDM%TVG, &
                                                  HPROGRAM)
    ENDIF
  END IF
END IF
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_TEB_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_TEB_n
