!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE WRITE_DIAG_TEB_n (DTCO, DUO, U, TM, GDM, GRM, HM, HPROGRAM,HWRITE)
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
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURFEX_n, ONLY : TEB_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_HYDRO_MODEL_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
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
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DUO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
TYPE(TEB_HYDRO_MODEL_t),     INTENT(INOUT) :: HM
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HWRITE   ! 'PGD' : only physiographic fields are written
!                                           ! 'ALL' : all fields are written
!
!*      0.2    declarations of local variables
!
INTEGER         :: JP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_TEB_N',0,ZHOOK_HANDLE)
IF (HWRITE/='PGD') THEN
!        
   IF (TM%TD%O%XDIAG_TSTEP==XUNDEF .OR. &
         ABS(NINT(TM%TOP%TTIME%TIME/TM%TD%O%XDIAG_TSTEP)*TM%TD%O%XDIAG_TSTEP-TM%TOP%TTIME%TIME)<1.E-3 ) THEN
      CALL WRITE_DIAG_SEB_TEB_n(DTCO, DUO, U, TM%CHT, TM%TD%O, TM%TD%D, TM%TD%DU, HPROGRAM)
      DO JP=1,TM%TOP%NTEB_PATCH
        CALL WRITE_DIAG_MISC_TEB_n(DTCO, DUO%CSELECT, U, TM%TD%NDMTC%AL(JP), TM%TD%NDMT%AL(JP), TM%TD%MTO, &
                                   GDM%VD%ND%AL(JP), GDM%VD%NDC%AL(JP), GDM%VD%NDE%AL(JP), GDM%VD%NDEC%AL(JP), &
                                   GRM%VD%ND%AL(JP), GRM%VD%NDC%AL(JP), GRM%VD%NDE%AL(JP), GRM%VD%NDEC%AL(JP), & 
                                   TM%NT%AL(JP), TM%TOP, GDM%O, HPROGRAM,JP)
      END DO      
   END IF
!
ENDIF
!
IF (TM%TD%O%LPGD) THEN
  IF (TM%TD%O%XDIAG_TSTEP==XUNDEF .OR. &
          ABS(NINT(TM%TOP%TTIME%TIME/TM%TD%O%XDIAG_TSTEP)*TM%TD%O%XDIAG_TSTEP-TM%TOP%TTIME%TIME)<1.E-3 ) THEN
    IF (ASSOCIATED(TM%NT%AL(1)%XBLD)) THEN
      CALL WRITE_DIAG_PGD_TEB_n(DTCO, DUO%CSELECT, U, TM%NB%AL(1), TM%BOP, TM%NT%AL(1), TM%TOP, TM%TPN, HPROGRAM)
      IF (TM%TOP%LGARDEN) &
        CALL WRITE_DIAG_PGD_GRDN_n(DTCO, DUO%CSELECT, U, TM%TD%MTO%LSURF_DIAG_ALBEDO, &
                        'GD',GDM%S, GDM%P, GDM%K, GDM%NPE%AL(1), GDM%O, HPROGRAM)
      IF (TM%TOP%LGARDEN .AND. TM%TOP%CURBTREE/='NONE') &
        CALL WRITE_DIAG_PGD_GRDN_n(DTCO, DUO%CSELECT, U, TM%TD%MTO%LSURF_DIAG_ALBEDO, &
                        'HV',GDM%S, GDM%PHV, GDM%K, GDM%NPEHV%AL(1), GDM%O, HPROGRAM)
    ENDIF
  END IF
END IF
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_TEB_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_TEB_n
