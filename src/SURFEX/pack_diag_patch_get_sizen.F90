!     #########
SUBROUTINE PACK_DIAG_PATCH_GET_SIZE_n
!##############################################
!
!!****  *PACK_DIAG_PATCH_GET_SIZE_n* - packs ISBA diagnostics
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
!!      Modified    10/2004 by P. Le Moigne: Halstead coefficient
!!      Modified    10/2005 by P. Le Moigne: Allocation (not EBA case)
!!      Modified       2008 by B. Decharme : Allocation for the floodplains
!!      Modified      04-09 by A.L. Gibelin : Add carbon diagnostics
!!
!!------------------------------------------------------------------
!
USE MODD_ISBA_n,            ONLY : TSNOW, CPHOTO, LTR_ML, LGLACIER
USE MODD_DIAG_ISBA_n,       ONLY : N2M
USE MODD_DIAG_MISC_ISBA_n,  ONLY : LSURF_MISC_BUDGET
USE MODD_DIAG_EVAP_ISBA_n,  ONLY : LWATER_BUDGET
!
USE MODD_PACK_DIAG_ISBA,  ONLY : NSIZE_SIMPLE, NSIZE_GROUND, NSIZE_SNOW, NSIZE_KSW, &
                                 NSIZE_ABC, NSIZE_0, NSIZE_00
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PACK_DIAG_PATCH_GET_SIZE_N',0,ZHOOK_HANDLE)
!
NSIZE_SIMPLE=57
NSIZE_GROUND=0
NSIZE_SNOW=0
NSIZE_KSW=2
NSIZE_ABC=0
NSIZE_0=0
NSIZE_00=0
!
IF (TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') THEN
  NSIZE_SIMPLE=NSIZE_SIMPLE+22
  NSIZE_SNOW=NSIZE_SNOW+2
ELSE
  NSIZE_0=NSIZE_0+22 
  NSIZE_00=NSIZE_00+2 
ENDIF
!
IF(TSNOW%SCHEME/='EBA') THEN
  NSIZE_SNOW=NSIZE_SNOW+1 
ELSE
  NSIZE_00=NSIZE_00+1 
ENDIF
!
IF(TSNOW%SCHEME=='EBA') THEN
  NSIZE_SIMPLE=NSIZE_SIMPLE+2 
ELSE
  NSIZE_0=NSIZE_0+2 
ENDIF
!
IF (LTR_ML) THEN
  NSIZE_SIMPLE=NSIZE_SIMPLE+4
ELSE
  NSIZE_0=NSIZE_0+4
ENDIF
!
IF (CPHOTO/='NON') THEN
  NSIZE_ABC=NSIZE_ABC+1 
ELSE
  NSIZE_00=NSIZE_00+1
ENDIF
!
IF (N2M>=1) THEN
  NSIZE_SIMPLE=NSIZE_SIMPLE+5 
ELSE
  NSIZE_0=NSIZE_0+5 
ENDIF
!
IF (LSURF_MISC_BUDGET) THEN
  NSIZE_GROUND=NSIZE_GROUND+2 
  NSIZE_SIMPLE=NSIZE_SIMPLE+2 
ELSE
  NSIZE_00=NSIZE_00+2 
  NSIZE_0=NSIZE_0+2 
ENDIF
!
IF(LGLACIER)THEN
  NSIZE_SIMPLE=NSIZE_SIMPLE+1 
ELSE
  NSIZE_0=NSIZE_0+1 
ENDIF
!
IF(LWATER_BUDGET)THEN
  NSIZE_SIMPLE=NSIZE_SIMPLE+5 
ELSE
  NSIZE_0=NSIZE_0+5 
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PACK_DIAG_PATCH_GET_SIZE_N',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------
!
END SUBROUTINE PACK_DIAG_PATCH_GET_SIZE_n
