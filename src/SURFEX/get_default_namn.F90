!     #########
      SUBROUTINE GET_DEFAULT_NAM_n(HPROGRAM,HACTION,KLUDES)
!     #######################################################
!
!!****  *GET_DEFAULT_NAM* - routine to open a namelist file with new defaults in it
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
#ifdef MNH
USE MODI_MNHGET_DESFM_n
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
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM
 CHARACTER(LEN=5), INTENT(IN)  :: HACTION ! 'READ ', 'WRITE'
INTEGER, INTENT(OUT) :: KLUDES ! logical unit of .des file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_DEFAULT_NAM_N',0,ZHOOK_HANDLE)
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL MNHGET_DESFM_n(HACTION,KLUDES)
  IF (HACTION=='READ ' .AND. KLUDES.NE.0) REWIND(KLUDES)
#endif
ELSEIF (HPROGRAM=='AROME ') THEN
#ifdef ARO
  CALL AROGET_DESFM_n(HACTION,KLUDES)
#endif
ELSE
  KLUDES = 0
END IF
IF (LHOOK) CALL DR_HOOK('GET_DEFAULT_NAM_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_DEFAULT_NAM_n
