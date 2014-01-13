!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE END_IO_SURF_n(HPROGRAM)
!     #######################################################
!
!!****  *END_IO_SURF_n* - routine to close all relevant files
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
#ifdef ASC
USE MODI_END_IO_SURF_ASC_n
#endif
#ifdef FA
USE MODI_END_IO_SURF_FA_n
#endif
#ifdef LFI
USE MODI_END_IO_SURF_LFI_n
#endif
#ifdef OL
USE MODI_END_IO_SURF_OL_n
#endif
#ifdef MNH
USE MODI_MNHEND_IO_SURF_n
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
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('END_IO_SURF_N',0,ZHOOK_HANDLE)
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL MNHEND_IO_SURF_n(HPROGRAM)
#endif
ELSEIF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
  CALL END_IO_SURF_ASC_n(HPROGRAM)
#endif
ELSEIF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
  CALL END_IO_SURF_OL_n(HPROGRAM)
#endif
ELSEIF (HPROGRAM=='AROME ') THEN
#ifdef ARO
  CALL AROEND_IO_SURF_n(HPROGRAM)
#endif
ELSEIF (HPROGRAM=='FA    ') THEN
#ifdef FA
  CALL END_IO_SURF_FA_n(HPROGRAM)
#endif
ELSEIF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
  CALL END_IO_SURF_LFI_n(HPROGRAM)
#endif
END IF
IF (LHOOK) CALL DR_HOOK('END_IO_SURF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE END_IO_SURF_n
