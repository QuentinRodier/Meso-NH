!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE WRITE_DIAG_ISBA_n (DTCO, DUO, U, IM, NDST,BLOWSNW, HPROGRAM, HWRITE)
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
!!      A. Druel    02/2019 Change CALL of WRITE_DIAG_MISC_ISBA_n
!!      B. Decharme 11/2016 Add procedure to write "cmip type" diag
!!      B. Decharme 11/2022 Split carbon diag from WRITE_DIAG_SEB_ISBA_n
!!------------------------------------------------------------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_n,       ONLY : DIAG_OPTIONS_t
USE MODD_SURF_ATM_n,   ONLY : SURF_ATM_t
USE MODD_SURFEX_n,     ONLY : ISBA_MODEL_t
USE MODD_DST_n,        ONLY : DST_NP_t
USE MODD_BLOWSNW_n,    ONLY : BLOWSNW_t
!
USE MODD_SURF_PAR,    ONLY : XUNDEF
! 
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_WRITE_DIAG_MISC_ISBA_n
USE MODI_WRITE_DIAG_PGD_ISBA_n
USE MODI_WRITE_DIAG_SEB_ISBA_n
USE MODI_WRITE_DIAG_SEB_CC_ISBA_n
!
USE MODI_WRITE_DIAG_MIP_ENERGY_ISBA_n
USE MODI_WRITE_DIAG_MIP_WATER_ISBA_n
USE MODI_WRITE_DIAG_MIP_MISC_ISBA_n
USE MODI_WRITE_DIAG_MIP_FX_ISBA_n
USE MODI_WRITE_DIAG_MIP_CARBON_ISBA_n
USE MODI_WRITE_DIAG_MIP_LUT_ISBA_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t),   INTENT(INOUT) :: DTCO
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DUO
TYPE(SURF_ATM_t),     INTENT(INOUT) :: U
TYPE(ISBA_MODEL_t),   INTENT(INOUT) :: IM
TYPE(DST_NP_t),       INTENT(INOUT) :: NDST
TYPE(BLOWSNW_t),      INTENT(INOUT) :: BLOWSNW
!
!
CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
CHARACTER(LEN=3),   INTENT(IN)  :: HWRITE   ! 'PGD' : only physiographic fields are written
!                                           ! 'ALL' : all fields are written
!
!*      0.2    declarations of local variables
!
INTEGER         :: ITIME_RATIO
LOGICAL         :: GWRITE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_ISBA_N',0,ZHOOK_HANDLE)
!
ITIME_RATIO=NINT(IM%S%TTIME%TIME/IM%ID%O%XDIAG_TSTEP)
GWRITE     =(IM%ID%O%XDIAG_TSTEP==XUNDEF.OR.ABS(ITIME_RATIO*IM%ID%O%XDIAG_TSTEP-IM%S%TTIME%TIME)<1.E-3)
!
! * Write Diag other than pgd
!
IF (GWRITE.AND.HWRITE/='PGD'.AND.IM%ID%O%LDIAG_MIP) THEN
   !
   ! * Write "cmip type" Diag
   !
   CALL WRITE_DIAG_MIP_ENERGY_ISBA_n(DTCO, DUO, U, IM%ID, IM%O, IM%S, IM%NP, IM%NPE, HPROGRAM)
   CALL WRITE_DIAG_MIP_WATER_ISBA_n (DTCO, DUO, U, IM%ID, IM%O, IM%S, IM%K, IM%NP, IM%NPE, HPROGRAM)
   CALL WRITE_DIAG_MIP_MISC_ISBA_n  (DTCO, DUO, U, IM%ID, IM%O, IM%S, IM%K, IM%NP, IM%NPE, HPROGRAM)
   CALL WRITE_DIAG_MIP_FX_ISBA_n    (DTCO, DUO, U, IM%ID, IM%O, IM%S, IM%K, IM%NP, IM%NPE, IM%NK, IM%ISS, HPROGRAM)
   !
   IF(IM%O%CPHOTO/='NON')THEN
     CALL WRITE_DIAG_MIP_CARBON_ISBA_n(DTCO, DUO, U, IM%ID, IM%O, IM%S, IM%NP, IM%NPE, HPROGRAM)
   ENDIF
   !
   IF(IM%ID%O%LLUTILES_BUDGET) THEN
     CALL WRITE_DIAG_MIP_LUT_ISBA_n(DTCO, DUO, U, IM%ID, IM%O, IM%S, IM%K, IM%NP, IM%NPE, HPROGRAM)
   ENDIF
   !
ELSEIF (GWRITE.AND.HWRITE/='PGD') THEN
   !
   ! * Write original surfex diag
   !
   CALL WRITE_DIAG_SEB_ISBA_n(DTCO, DUO, U, IM%NCHI, IM%CHI, IM%ID, IM%ID%DU, NDST, BLOWSNW, IM%GB, &
                              IM%O, IM%S, IM%NP, IM%NPE, HPROGRAM)
   !
   CALL WRITE_DIAG_SEB_CC_ISBA_n(DTCO, DUO, U, IM%ID, IM%O, IM%S, IM%NP, IM%NPE, HPROGRAM)
   !
   IF (IM%ID%DM%LSURF_MISC_BUDGET) THEN
      CALL WRITE_DIAG_MISC_ISBA_n(DTCO, DUO%CSELECT, DUO%LSNOWDIMNC, DUO%LRESETCUMUL, U,        &
                                  IM%ID%O%LPATCH_BUDGET,IM%ID%D, IM%ID%ND, IM%ID%DM,            &
                                  IM%ID%NDM, IM%O, IM%S, IM%K, IM%NP, IM%NPE, IM%NAG,           &
                                  IM%NPE%AL(1)%TSNOW, HPROGRAM)
   ENDIF
   !
ENDIF
!
! * Write Diag from pgd
!
IF (GWRITE.AND.IM%ID%O%LPGD.AND.(.NOT.IM%ID%O%LDIAG_MIP)) THEN
   !
   CALL WRITE_DIAG_PGD_ISBA_n(DTCO, DUO%CSELECT, U, IM%CHI, IM%NCHI, IM%ID%DM%LSURF_DIAG_ALBEDO, &
                              IM%O, IM%S, IM%K, IM%NP, IM%NPE, IM%ISS, HPROGRAM)
   !
END IF
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_ISBA_n
