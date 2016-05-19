!     #########
      SUBROUTINE OPEN_AUX_IO_SURF(HFILE,HFILETYPE,HMASK)
!     #######################################################
!
!!****  *OPEN_AUX_IO_SURF* - chooses the routine to OPENialize IO
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
!!	S.Malardel   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2003 
!!      Modified    04/2004 by P. LeMoigne: add HACTION if ASCII mode selected
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef ASC
USE MODI_OPEN_AUX_IO_SURF_ASC
#endif
!
#ifdef FA
USE MODI_OPEN_AUX_IO_SURF_FA
#endif
!
#ifdef LFI
USE MODI_OPEN_AUX_IO_SURF_LFI
#endif
!
#ifdef OL
USE MODI_OPEN_AUX_IO_SURF_OL
#endif
!
#ifdef MNH
USE MODI_MNHOPEN_AUX_IO_SURF
#endif
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=28), INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),  INTENT(IN)  :: HFILETYPE ! main program
 CHARACTER(LEN=6),  INTENT(IN)  :: HMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('OPEN_AUX_IO_SURF',0,ZHOOK_HANDLE)
IF (HFILETYPE=='MESONH') THEN
#ifdef MNH
  CALL MNHOPEN_AUX_IO_SURF(HFILE,HFILETYPE,HMASK)
#endif
END IF
!
IF (HFILETYPE=='OFFLIN' ) THEN
#ifdef OL
  CALL OPEN_AUX_IO_SURF_OL
#endif
ENDIF
!
IF (HFILETYPE=='ASCII ' ) THEN
#ifdef ASC
  CALL OPEN_AUX_IO_SURF_ASC(HFILE,HFILETYPE,HMASK)
#endif
ENDIF
!
IF (HFILETYPE=='AROME ' ) THEN
#ifdef ARO
  CALL AROOPEN_AUX_IO_SURF(HFILE,HFILETYPE,HMASK)
#endif
ENDIF
!
IF (HFILETYPE=='FA    ' ) THEN
#ifdef FA
  CALL OPEN_AUX_IO_SURF_FA(HFILE,HFILETYPE,HMASK)
#endif
ENDIF
!
IF (HFILETYPE=='LFI   ' ) THEN
#ifdef LFI
  CALL OPEN_AUX_IO_SURF_LFI(HFILE,HFILETYPE,HMASK)
#endif
ENDIF
IF (LHOOK) CALL DR_HOOK('OPEN_AUX_IO_SURF',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_AUX_IO_SURF
