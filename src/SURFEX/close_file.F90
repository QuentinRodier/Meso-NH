!     #########
      SUBROUTINE CLOSE_FILE(HPROGRAM,KUNIT)
!     #######################################################
!
!!****  *CLOSE_FILE* - generic routine to close a file
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
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
#if defined(ASC) || defined(ARO) || defined(MNH)
USE MODI_CLOSE_FILE_ASC
#endif
#ifdef FA
USE MODI_CLOSE_FILE_FA
#endif
#ifdef OL
USE MODI_CLOSE_FILE_OL
#endif
#ifdef LFI
USE MODI_CLOSE_FILE_LFI
#endif
!
#ifdef MNH
USE MODI_CLOSE_FILE_MNH
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! main program
INTEGER,           INTENT(IN)  :: KUNIT    ! logical unit of file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CLOSE_FILE',0,ZHOOK_HANDLE)
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL CLOSE_FILE_MNH(HPROGRAM,KUNIT)
#endif
ELSE IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
  CALL CLOSE_FILE_OL(HPROGRAM,KUNIT)
#endif
ELSE IF (HPROGRAM=='ASCII ') THEN
#if defined(ASC) || defined(ARO) || defined(MNH)
  CALL CLOSE_FILE_ASC(HPROGRAM,KUNIT)
#endif
ELSE IF (HPROGRAM=='FA    ') THEN
#ifdef FA
  CALL CLOSE_FILE_FA(HPROGRAM,KUNIT)
#endif
ELSE IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
  CALL CLOSE_FILE_LFI(HPROGRAM,KUNIT)
#endif
END IF
IF (LHOOK) CALL DR_HOOK('CLOSE_FILE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CLOSE_FILE
