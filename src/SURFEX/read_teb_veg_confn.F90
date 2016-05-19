!     #########
      SUBROUTINE READ_TEB_VEG_CONF_n(HPROGRAM)
!     #######################################################
!
!!****  *READ_TEB_VEG_CONF* - routine to read the configuration for VEG
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
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      P Le Moigne 09/2005 CSNOWRES option
!!      Modified by P. Le Moigne (06/2006): seeding and irrigation
!!      Modified by P. Le Moigne (05/2008): deep soil characteristics
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
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODN_TEB_VEG_n
!
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
USE MODD_SURF_PAR,   ONLY : XUNDEF
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
INTEGER           :: ILUOUT         ! logical unit of output file
INTEGER           :: INAM           ! logical unit of namelist file
INTEGER           :: IMI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* get output listing file logical unit
!
IF (LHOOK) CALL DR_HOOK('READ_TEB_VEG_CONF_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IMI=GET_CURRENT_MODEL_INDEX_SURFEX()
!
IF (IMI.NE.-1 .AND. LNAM_READ) THEN
 CALL INIT_NAM_TEB_VEGn
 CALL INIT_NAM_CH_CONTROLn
 CALL INIT_NAM_CH_TEB_VEGn
 CALL INIT_NAM_SGH_TEB_VEGn        
ENDIF

IF (LNAM_READ) THEN
 !
 !* open namelist file
 !
 CALL OPEN_NAMELIST(HPROGRAM,INAM)
 !
 !* reading of namelist
 !  -------------------
 !
 CALL POSNAM(INAM,'NAM_ISBAN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_ISBAn)
! for the time being, chemistry is not implemented on gardens
! CALL POSNAM(INAM,'NAM_CH_ISBAN',GFOUND,ILUOUT)
! IF (GFOUND) READ(UNIT=INAM,NML=NAM_CH_ISBAn)
 CALL POSNAM(INAM,'NAM_CH_CONTROLN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_CH_CONTROLn)
 CALL POSNAM(INAM,'NAM_SGH_ISBAN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_SGH_ISBAn)
 !
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CROUGH',CROUGH,'NONE','Z01D','Z04D','BE04','UNDE')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSCOND',CSCOND,'NP89','PL98')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CALBEDO',CALBEDO,'EVOL','DRY ','WET ','MEAN','USER','CM13')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CC1DRY',CC1DRY,'DEF ','GB93')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSOILFRZ',CSOILFRZ,'DEF','LWT')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CDIFSFCOND',CDIFSFCOND,'DEF ','MLCH')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNOWRES',CSNOWRES,'DEF','RIL')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CCPSURF',CCPSURF,'DRY','HUM')
 !
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CRUNOFF',CRUNOFF,'WSAT','DT92','SGH ','TOPD')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CTOPREG',CTOPREG,'DEF','NON')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CKSAT',CKSAT,'DEF','SGH','EXP')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CHORT',CHORT,'DEF','SGH')
 !
 !* close namelist file
 !
 CALL CLOSE_NAMELIST(HPROGRAM,INAM)
 !
ENDIF
!
IF (IMI.NE.-1) THEN
 CALL UPDATE_NAM_TEB_VEGn
 CALL UPDATE_NAM_CH_TEB_VEGn
 CALL UPDATE_NAM_CH_CONTROLn
 CALL UPDATE_NAM_SGH_TEB_VEGn        
ENDIF
IF (LHOOK) CALL DR_HOOK('READ_TEB_VEG_CONF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!* surface time-step forced by the atmosphere
!
!XTSTEP = XUNDEF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_TEB_VEG_CONF_n
