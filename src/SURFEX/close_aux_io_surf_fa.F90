!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #######################################################
      SUBROUTINE CLOSE_AUX_IO_SURF_FA(HFILE,HFILETYPE)
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_IO_SURF_ASC,ONLY:NUNIT,CMASK,NMASK
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
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
IF (LHOOK) CALL DR_HOOK('CLOSE_AUX_IO_SURF_FA',0,ZHOOK_HANDLE)
CLOSE(NUNIT)
!
NUNIT=0
CMASK='      '
DEALLOCATE(NMASK)
IF (LHOOK) CALL DR_HOOK('CLOSE_AUX_IO_SURF_FA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CLOSE_AUX_IO_SURF_FA
