!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
!     #######################################################
!
!!****  *CLOSE_AUX_IO_SURF* - chooses the routine to OPENialize IO
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
USE MODI_CLOSE_AUX_IO_SURF_ASC
#endif
!
#ifdef LFI
USE MODI_CLOSE_AUX_IO_SURF_LFI
#endif
!
#ifdef OL
USE MODI_CLOSE_AUX_IO_SURF_OL
#endif
!
#ifdef MNH
USE MODI_MNHCLOSE_AUX_IO_SURF
#endif
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=28), INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),  INTENT(IN)  :: HFILETYPE ! main program
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CLOSE_AUX_IO_SURF',0,ZHOOK_HANDLE)
IF (HFILETYPE=='MESONH') THEN
#ifdef MNH
  CALL MNHCLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
#endif
END IF
!
IF (HFILETYPE=='OFFLIN' ) THEN
#ifdef OL
  CALL CLOSE_AUX_IO_SURF_OL
#endif
ENDIF
!
IF (HFILETYPE=='ASCII ' ) THEN
#ifdef ASC
  CALL CLOSE_AUX_IO_SURF_ASC(HFILE,HFILETYPE)
#endif
ENDIF
!
IF (HFILETYPE=='AROME ' ) THEN
#ifdef ARO
  CALL AROCLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
#endif
ENDIF
!
IF (HFILETYPE=='LFI   ' ) THEN
#ifdef LFI
  CALL CLOSE_AUX_IO_SURF_LFI(HFILE,HFILETYPE)
#endif
ENDIF
IF (LHOOK) CALL DR_HOOK('CLOSE_AUX_IO_SURF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CLOSE_AUX_IO_SURF
