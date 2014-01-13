!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_DEFAULT_FLAKE_n(HPROGRAM)
!     #############################################################
!
!!****  *READ_FLAKE_CONF* - routine to read the configuration for FLAKE
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
USE MODE_MODELN_SURFEX_HANDLER
!
USE MODE_POS_SURF
USE MODI_GET_LUOUT
USE MODI_GET_DEFAULT_NAM_n
!
USE MODN_FLAKE_n
USE MODD_DIAG_MISC_FLAKE_n,    ONLY : XZW_PROFILE, XTW_PROFILE
USE MODD_SURF_PAR,           ONLY : XUNDEF
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
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
LOGICAL           :: GFOUND         ! Return code when searching namelist
INTEGER           :: ILUOUT         ! output listing logical unit
INTEGER           :: ILUDES         ! .des file logical unit
INTEGER           :: ILU         ! 
INTEGER           :: IMI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('READ_DEFAULT_FLAKE_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL GET_DEFAULT_NAM_n(HPROGRAM,'READ ',ILUDES)
!
IF (ILUDES==0 .AND. LHOOK) CALL DR_HOOK('READ_DEFAULT_FLAKE_N',1,ZHOOK_HANDLE)
IF (ILUDES==0) RETURN
!
IMI=GET_CURRENT_MODEL_INDEX_SURFEX()
!
IF (IMI.NE.-1 .AND. LNAM_READ) THEN
 CALL INIT_NAM_FLAKEn
 CALL INIT_NAM_DIAG_SURFn
 CALL INIT_NAM_DIAG_FLAKEn
 CALL INIT_NAM_CH_WATFLUXn
ENDIF
!
IF (LNAM_READ) THEN
 !
 !* reading of namelist
 !  -------------------
 !
 CALL POSNAM(ILUDES,'NAM_FLAKEN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_FLAKEn)
 CALL POSNAM(ILUDES,'NAM_DIAG_SURFN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_DIAG_SURFn)
 CALL POSNAM(ILUDES,'NAM_DIAG_FLAKEN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_DIAG_FLAKEn)
 IF (LWATER_PROFILE .AND. count (XZWAT_PROFILE /= XUNDEF) == 0) &
     CALL ABOR1_SFX("XZWAT_PROFILE MUST BE DEFINED IN NAMELIST NAM_DIAG_FLAKEN IF LWATER_PROFILE=T")     
 CALL POSNAM(ILUDES,'NAM_CH_WATFLUXN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_CH_WATFLUXn)
!
ENDIF
!
IF (IMI.NE.-1) THEN
 CALL UPDATE_NAM_FLAKEn
 CALL UPDATE_NAM_DIAG_SURFn
 CALL UPDATE_NAM_DIAG_FLAKEn
 CALL UPDATE_NAM_CH_WATFLUXn
ENDIF
IF (LHOOK) CALL DR_HOOK('READ_DEFAULT_FLAKE_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_DEFAULT_FLAKE_n
