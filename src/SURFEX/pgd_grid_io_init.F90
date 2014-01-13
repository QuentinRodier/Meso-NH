!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_GRID_IO_INIT(HPROGRAM)
!     ######################################
!!
!!    PURPOSE
!!    -------
!!
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     13/10/03
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef MNH
USE MODI_PGD_GRID_IO_INIT_MNH
#endif
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling READ_PGD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_GRID_IO_INIT',0,ZHOOK_HANDLE)
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL PGD_GRID_IO_INIT_MNH
#endif
END IF
IF (LHOOK) CALL DR_HOOK('PGD_GRID_IO_INIT',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_GRID_IO_INIT
