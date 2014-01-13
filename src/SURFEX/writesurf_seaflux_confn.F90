!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_SEAFLUX_CONF_n(HPROGRAM)
!     ######################################################
!
!!****  *WRITESURF_SEAFLUX_CONF* - routine to read the configuration for SEAFLUX
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
USE MODN_SEAFLUX_n
USE MODN_SLT
!
USE MODI_GET_DEFAULT_NAM_n
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
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM ! program calling SEAFLUX

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUDES
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_SEAFLUX_CONF_N',0,ZHOOK_HANDLE)
 CALL GET_DEFAULT_NAM_n(HPROGRAM,'WRITE',ILUDES)
!
IF (ILUDES==0 .AND. LHOOK) CALL DR_HOOK('WRITESURF_SEAFLUX_CONF_N',1,ZHOOK_HANDLE)
IF (ILUDES==0) RETURN
!
!-------------------------------------------------------------------------------
!
 CALL INIT_NAM_SEAFLUXn
 CALL INIT_NAM_CH_SEAFLUXn
 CALL INIT_NAM_DIAG_OCEANn
!
WRITE(UNIT=ILUDES,NML=NAM_SEAFLUXn)
WRITE(UNIT=ILUDES,NML=NAM_CH_SEAFLUXn)
WRITE(UNIT=ILUDES,NML=NAM_DIAG_OCEANn)
WRITE(UNIT=ILUDES,NML=NAM_SURF_SLT)
IF (LHOOK) CALL DR_HOOK('WRITESURF_SEAFLUX_CONF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_SEAFLUX_CONF_n
