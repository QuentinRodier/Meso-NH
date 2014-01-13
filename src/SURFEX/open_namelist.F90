!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE OPEN_NAMELIST(HPROGRAM,KLUNAM,HFILE)
!     #######################################################
!
!!****  *OPEN_NAMELIST* - routine to open a namelist file
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
#ifdef OL
USE MODI_OPEN_NAMELIST_OL
#endif
#ifdef ASC
USE MODI_OPEN_NAMELIST_ASC
#endif
#ifdef FA
USE MODI_OPEN_NAMELIST_FA
#endif
#ifdef LFI
USE MODI_OPEN_NAMELIST_LFI
#endif
#ifdef MNH
USE MODI_MNHOPEN_NAMELIST
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
INTEGER,           INTENT(OUT) :: KLUNAM   ! logical unit of namelist
 CHARACTER(LEN=28), INTENT(IN), OPTIONAL :: HFILE ! ASCII file to open

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=28) :: YFILE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('OPEN_NAMELIST',0,ZHOOK_HANDLE)
IF (PRESENT(HFILE)) THEN
  YFILE = HFILE
ELSE
  YFILE = '                           '
END IF

IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL MNHOPEN_NAMELIST(HPROGRAM,KLUNAM,YFILE)
#endif
ELSE IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
  CALL OPEN_NAMELIST_OL(HPROGRAM,KLUNAM,YFILE)
#endif
ELSE IF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
  CALL OPEN_NAMELIST_ASC(HPROGRAM,KLUNAM,YFILE)
#endif
ELSE IF (HPROGRAM=='AROME ') THEN
#ifdef ARO
  CALL AROOPEN_NAMELIST(HPROGRAM,KLUNAM,YFILE)
#endif
ELSE IF (HPROGRAM=='FA    ') THEN
#ifdef FA
  CALL OPEN_NAMELIST_FA(HPROGRAM,KLUNAM,YFILE)
#endif
ELSE IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
  CALL OPEN_NAMELIST_LFI(HPROGRAM,KLUNAM,YFILE)
#endif
END IF
IF (LHOOK) CALL DR_HOOK('OPEN_NAMELIST',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_NAMELIST
