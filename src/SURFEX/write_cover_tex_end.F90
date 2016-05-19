!     #########
      SUBROUTINE WRITE_COVER_TEX_END(HPROGRAM)
!     ##########################
!
!!**** *WRITE_COVER_TEX* closes the tex file
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    08/01/98
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_WRITE_COVER_TEX,ONLY : NTEX
!
#ifdef LFI
USE MODI_CLOSE_WRITE_COVER_TEX_LFI
#endif
#ifdef MNH
USE MODI_MNHCLOSE_WRITE_COVER_TEX
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),                INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_END',0,ZHOOK_HANDLE)
IF (NTEX==0 .AND. LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_END',1,ZHOOK_HANDLE)
IF (NTEX==0) RETURN
!
!* writing in the file
!
WRITE(NTEX,*) '}}'
WRITE(NTEX,*) '\end{document}'
!
!* close the file
!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL MNHCLOSE_WRITE_COVER_TEX
#endif
ELSEIF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
  CALL CLOSE_WRITE_COVER_TEX_LFI(NTEX)
#endif
ELSEIF (HPROGRAM=='AROME') THEN
#ifdef ARO
  CALL AROCLOSE_WRITE_COVER_TEX(NTEX)
#endif
ELSE
  CLOSE(NTEX)
END IF      
!
NTEX=0
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_END',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_COVER_TEX_END
