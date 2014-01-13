!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_DEFAULT_SEAFLUX_n(HPROGRAM)
!     #############################################################
!
!!****  *READ_SEAFLUX_CONF* - routine to read the configuration for SEAFLUX
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
!!      Modified    01/2006 : sea flux parameterization.
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_MODELN_SURFEX_HANDLER
!
USE MODE_POS_SURF
!
USE MODI_TEST_NAM_VAR_SURF
USE MODI_GET_DEFAULT_NAM_n
USE MODI_GET_LUOUT
!
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
USE MODN_SEAFLUX_n
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling ISBA
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
LOGICAL           :: GFOUND         ! Return code when searching namelist
INTEGER           :: ILUOUT         ! output listing logical unit
INTEGER           :: ILUDES         ! .des file logical unit
INTEGER           :: IMI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_DEFAULT_SEAFLUX_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
! 
 CALL GET_DEFAULT_NAM_n(HPROGRAM,'READ ',ILUDES)
!
IF (ILUDES==0 .AND. LHOOK) CALL DR_HOOK('READ_DEFAULT_SEAFLUX_N',1,ZHOOK_HANDLE)
IF (ILUDES==0) RETURN
!
IMI=GET_CURRENT_MODEL_INDEX_SURFEX()
!
IF (IMI.NE.-1 .AND. LNAM_READ) THEN
 CALL INIT_NAM_SEAFLUXn
 CALL INIT_NAM_DIAG_SURFn
 CALL INIT_NAM_CH_SEAFLUXn
 CALL INIT_NAM_DIAG_OCEANn
ENDIF
! 
IF (LNAM_READ) THEN
 !
 !* reading of namelist
 !  -------------------
 !
 CALL POSNAM(ILUDES,'NAM_SEAFLUXN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_SEAFLUXn)
 CALL POSNAM(ILUDES,'NAM_DIAG_SURFN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_DIAG_SURFn)
 CALL POSNAM(ILUDES,'NAM_CH_SEAFLUXN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_CH_SEAFLUXn)
 CALL POSNAM(ILUDES,'NAM_DIAG_OCEANN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_DIAG_OCEANn)
 !
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSEA_FLUX',CSEA_FLUX,'DIRECT','ITERAT','ECUME ','COARE3')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSEA_ALB', CSEA_ALB, 'UNIF','TA96','MK10')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CCH_DRY_DEP',CCH_DRY_DEP,'      ','WES89 ','NONE  ')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CINTERPOL_SST',CINTERPOL_SST,'ANNUAL','MONTH ','NONE  ')

ENDIF
!
IF (IMI.NE.-1) THEN
 CALL UPDATE_NAM_SEAFLUXn
 CALL UPDATE_NAM_DIAG_SURFn
 CALL UPDATE_NAM_CH_SEAFLUXn
 CALL UPDATE_NAM_DIAG_OCEANn
ENDIF
IF (LHOOK) CALL DR_HOOK('READ_DEFAULT_SEAFLUX_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_DEFAULT_SEAFLUX_n
