!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_SEAFLUX_n(HPROGRAM,HWRITE)
!     ####################################
!
!!****  *WRITE_SEAFLUX_n* - routine to write surface variables in their respective files
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
!!      B. Decharme 07/2011 : Suppress pgd output
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_WRITE_SURF_ATM, ONLY : LNOWRITE_CANOPY
USE MODD_DIAG_SURF_ATM_n, ONLY: LSELECT
USE MODI_INIT_IO_SURF_n
USE MODI_WRITESURF_SEAFLUX_CONF_n
USE MODI_WRITESURF_SEAFLUX_n
USE MODI_WRITESURF_SEAFLUX_SBL_n
USE MODI_END_IO_SURF_n
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
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),    INTENT(IN)  :: HWRITE    ! 'PREP' : does not write SBL XUNDEF fields
!                                             ! 'ALL' : all fields are written
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_SEAFLUX_N',0,ZHOOK_HANDLE)
 CALL INIT_IO_SURF_n(HPROGRAM,'SEA   ','SEAFLX','WRITE')
!
!*       1.     Selection of surface scheme
!               ---------------------------
!
 CALL WRITESURF_SEAFLUX_CONF_n(HPROGRAM)
 CALL WRITESURF_SEAFLUX_n(HPROGRAM)
!
IF ((.NOT.LNOWRITE_CANOPY).OR.LSELECT) CALL WRITESURF_SEAFLUX_SBL_n(HPROGRAM,HWRITE)
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_SEAFLUX_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SEAFLUX_n
