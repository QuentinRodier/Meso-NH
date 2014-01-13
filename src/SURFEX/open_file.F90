!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE OPEN_FILE(HPROGRAM,KUNIT,HFILE,HFORM,HACTION,HACCESS,KRECL)
!     #######################################################
!
!!****  *OPEN_FILE* - routine to open a namelist file
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
#if defined(ASC) || defined(ARO) || defined(MNH)
USE MODI_OPEN_FILE_ASC
#endif
#ifdef FA
USE MODI_OPEN_FILE_FA
#endif
#ifdef LFI
USE MODI_OPEN_FILE_LFI
#endif
#ifdef OL
USE MODI_OPEN_FILE_OL
#endif
#ifdef MNH
USE MODI_OPEN_FILE_MNH
#endif
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)           :: HPROGRAM ! main program
INTEGER,           INTENT(OUT)          :: KUNIT    ! logical unit
 CHARACTER(LEN=*),  INTENT(IN)           :: HFILE    ! file to open
 CHARACTER(LEN=*),  INTENT(IN)           :: HFORM    ! type of file
 CHARACTER(LEN=*),  INTENT(IN), OPTIONAL :: HACTION  ! action
 CHARACTER(LEN=*),  INTENT(IN), OPTIONAL :: HACCESS  ! access type
INTEGER,           INTENT(IN), OPTIONAL :: KRECL    ! record length

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=28) :: YFILE
 CHARACTER(LEN=11) :: YFORM
 CHARACTER(LEN=9)  :: YACTION
 CHARACTER(LEN=6)  :: YACCESS
INTEGER           :: IRECL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('OPEN_FILE',0,ZHOOK_HANDLE)
!
YFILE = HFILE
YFORM = HFORM
IF (PRESENT(HACTION)) THEN
  YACTION = HACTION
ELSE
  YACTION = 'READWRITE'
END IF
IF (PRESENT(HACCESS)) THEN
  YACCESS = HACCESS
ELSE
  YACCESS = '      '
END IF
IF (PRESENT(KRECL)) THEN
  IRECL = KRECL
ELSE
  IRECL = 0
END IF
!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL OPEN_FILE_MNH(KUNIT,YFILE,YFORM,YACTION,YACCESS,IRECL)
#endif
ELSE IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
  CALL OPEN_FILE_OL(KUNIT,YFILE,YFORM,YACTION,YACCESS,IRECL)
#endif
ELSE IF (HPROGRAM=='ASCII ' .OR. HPROGRAM=='AROME ') THEN
#if defined(ASC) || defined(ARO) || defined(MNH)
  CALL OPEN_FILE_ASC(KUNIT,YFILE,YFORM,YACTION,YACCESS,IRECL)
#endif
ELSE IF (HPROGRAM=='FA    ') THEN
#ifdef FA
  CALL OPEN_FILE_FA(KUNIT,YFILE,YFORM,YACTION,YACCESS,IRECL)
#endif
ELSE IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
  CALL OPEN_FILE_LFI(KUNIT,YFILE,YFORM,YACTION,YACCESS,'ASIS  ',IRECL)
#endif
END IF
!
IF (LHOOK) CALL DR_HOOK('OPEN_FILE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_FILE
