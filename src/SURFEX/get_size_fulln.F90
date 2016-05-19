!     #########
      SUBROUTINE GET_SIZE_FULL_n(HPROGRAM,KDIM_FULL,KSIZE_FULL)
!     #######################################################
!
!!****  *GET_SIZE_FULL_n* - get number of points for this proc
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
!!      Original    05/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_MPI, ONLY : WLOG_MPI
!
USE MODD_SURF_PAR,   ONLY : NUNDEF
USE MODD_SURF_ATM_n, ONLY : NSIZE_FULL, NDIM_FULL
!
USE MODD_SURFEX_MPI, ONLY : NINDEX, NRANK, NPROC
USE MODD_SURFEX_OMP, ONLY : NINDX1, NINDX2
!
#ifdef MNH
USE MODI_MNHGET_SIZE_FULL_n
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
INTEGER         ,  INTENT(IN)  :: KDIM_FULL  ! total number of points
INTEGER         ,  INTENT(OUT) :: KSIZE_FULL ! total number of points on this proc
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_SIZE_FULL_N',0,ZHOOK_HANDLE)
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL MNHGET_SIZE_FULL_n(HPROGRAM,KDIM_FULL,KSIZE_FULL)
#endif
END IF
!
IF ( HPROGRAM=='OFFLIN' .OR. HPROGRAM=='ASCII ' .OR. HPROGRAM=='FA    ' .OR. HPROGRAM=='LFI   ' .OR. &
     HPROGRAM=='TEXTE ' .OR. HPROGRAM=='BINARY' ) THEN
#ifdef OL
  IF (NSIZE_FULL/=NUNDEF .AND. NSIZE_FULL/=0) THEN
    KSIZE_FULL = NSIZE_FULL
  ELSEIF (ALLOCATED(NINDEX)) THEN
    KSIZE_FULL = 0
    DO J=1,SIZE(NINDEX)
      IF ( NINDEX(J)==MOD(NRANK,NPROC) ) KSIZE_FULL = KSIZE_FULL + 1
    ENDDO
  ELSE
    KSIZE_FULL = KDIM_FULL
  END IF
  IF ( NINDX2/=KDIM_FULL .OR. NINDX1/=1 ) KSIZE_FULL = MIN(KSIZE_FULL,NINDX2-NINDX1+1)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef ARO
  CALL AROGET_SIZE_FULL_n(HPROGRAM,KDIM_FULL,KSIZE_FULL)
#endif
ENDIF
IF (LHOOK) CALL DR_HOOK('GET_SIZE_FULL_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_SIZE_FULL_n
