!     #########
      SUBROUTINE READ_DEFAULT_TEB_VEG_n(HPROGRAM)
!     #######################################################
!
!!****  *READ_DEFAULT_TEB_VEG* - routine to read the configuration for VEGs
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
!
USE MODI_GET_LUOUT
USE MODI_GET_DEFAULT_NAM_n
!
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
USE MODN_TEB_VEG_n
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
IF (LHOOK) CALL DR_HOOK('READ_DEFAULT_TEB_VEG_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL GET_DEFAULT_NAM_n(HPROGRAM,'READ ',ILUDES)
!
IF (ILUDES==0 .AND. LHOOK) CALL DR_HOOK('READ_DEFAULT_TEB_VEG_N',1,ZHOOK_HANDLE)
IF (ILUDES==0) RETURN
!
IMI=GET_CURRENT_MODEL_INDEX_SURFEX()
!
IF (IMI.NE.-1 .AND. LNAM_READ) THEN
 CALL INIT_NAM_TEB_VEGn
 CALL INIT_NAM_CH_CONTROLn
 CALL INIT_NAM_CH_TEB_VEGn
 CALL INIT_NAM_SGH_TEB_VEGn
ENDIF
!
IF (LNAM_READ) THEN
 !
 !* reading of new defaults in file
 !  -------------------------------
 !
 CALL POSNAM(ILUDES,'NAM_ISBAN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_ISBAn)
 CALL POSNAM(ILUDES,'NAM_CH_CONTROLN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_CH_CONTROLn)
! for the time being, chemistry is not implemented on gardens
! CALL POSNAM(ILUDES,'NAM_CH_ISBAN',GFOUND,ILUOUT)
! IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_CH_ISBAn)
 CALL POSNAM(ILUDES,'NAM_SGH_ISBAN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_SGH_ISBAn)
 !
ENDIF
!
IF (IMI.NE.-1) THEN
 CALL UPDATE_NAM_TEB_VEGn
 CALL UPDATE_NAM_CH_CONTROLn
 CALL UPDATE_NAM_CH_TEB_VEGn
 CALL UPDATE_NAM_SGH_TEB_VEGn
ENDIF
IF (LHOOK) CALL DR_HOOK('READ_DEFAULT_TEB_VEG_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_DEFAULT_TEB_VEG_n
