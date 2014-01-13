!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE WRITE_DIAG_NATURE_n(HPROGRAM,HWRITE)
!     ###############################################################################
!
!!****  *WRITE_DIAG_NATURE_n * - Chooses the surface schemes for diagnostics over
!!    natural continental parts
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!

!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SURF_ATM_n, ONLY : CNATURE
!
USE MODI_WRITE_DIAG_ISBA_n
! 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HWRITE   ! 'PGD' : only physiographic fields are written
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                                           ! 'ALL' : all fields are written
!
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_NATURE_N',0,ZHOOK_HANDLE)
IF (CNATURE=='ISBA  ') THEN
  CALL WRITE_DIAG_ISBA_n(HPROGRAM,HWRITE)
END IF
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_NATURE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_NATURE_n
