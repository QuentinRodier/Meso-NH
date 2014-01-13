!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE END_IO_SURF_FA_n(HPROGRAM)
!     #######################################################
!
!!****  *END_IO_SURF_FA_n* - routine to close IO files
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
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NFULL, CMASK, LOPEN
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, WLOG_MPI
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
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
INTEGER :: IRET ! error code
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('END_IO_SURF_FA_N',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
NFULL = 0
!
CMASK = '      '
!
IF (NRANK==NPIO .AND. LOPEN) THEN
!$OMP SINGLE         
  CALL FAIRME(IRET,NUNIT_FA,'UNKNOWN')
!$OMP END SINGLE
END IF
!
IF (LHOOK) CALL DR_HOOK('END_IO_SURF_FA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE END_IO_SURF_FA_n
