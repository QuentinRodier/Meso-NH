!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE CLOSE_NAMELIST(HPROGRAM,KLUNAM)
!     #######################################################
!
!!****  *CLOSE_NAMELIST* - generic routine to close a namelist file
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
#ifdef ASC
USE MODI_CLOSE_NAMELIST_ASC
#endif
#ifdef FA
USE MODI_CLOSE_NAMELIST_FA
#endif
#ifdef LFI
USE MODI_CLOSE_NAMELIST_LFI
#endif
#ifdef OL
USE MODI_CLOSE_NAMELIST_OL
#endif
#ifdef MNH
USE MODI_MNHCLOSE_NAMELIST
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
INTEGER,           INTENT(IN)  :: KLUNAM   ! logical unit of namelist
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CLOSE_NAMELIST',0,ZHOOK_HANDLE)
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL MNHCLOSE_NAMELIST(HPROGRAM,KLUNAM)
#endif
ELSE IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
  CALL CLOSE_NAMELIST_OL(HPROGRAM,KLUNAM)
#endif
ELSE IF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
  CALL CLOSE_NAMELIST_ASC(HPROGRAM,KLUNAM)
#endif
ELSE IF (HPROGRAM=='AROME ') THEN
#ifdef ARO
  CALL AROCLOSE_NAMELIST(HPROGRAM,KLUNAM)
#endif
ELSE IF (HPROGRAM=='FA    ') THEN
#ifdef FA
  CALL CLOSE_NAMELIST_FA(HPROGRAM,KLUNAM)
#endif
ELSE IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
  CALL CLOSE_NAMELIST_LFI(HPROGRAM,KLUNAM)
#endif
END IF
IF (LHOOK) CALL DR_HOOK('CLOSE_NAMELIST',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CLOSE_NAMELIST
