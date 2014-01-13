!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_FLAKE_CONF_n(HPROGRAM)
!     ######################################################
!
!!****  *WRITESURF_FLAKE_CONF* - routine to read the configuration for FLAKE
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
USE MODN_FLAKE_n
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
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM ! program calling FLAKE

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUDES
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!NAMELIST/NAM_DIAG_FLAKEn/LWATER_PROFILE,XZW_PROFILE
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_FLAKE_CONF_N',0,ZHOOK_HANDLE)
 CALL GET_DEFAULT_NAM_n(HPROGRAM,'WRITE',ILUDES)
!
IF (ILUDES==0 .AND. LHOOK) CALL DR_HOOK('WRITESURF_FLAKE_CONF_N',1,ZHOOK_HANDLE)
IF (ILUDES==0) RETURN
!
!-------------------------------------------------------------------------------
!
 CALL INIT_NAM_FLAKEn
 CALL INIT_NAM_DIAG_FLAKEn
 CALL INIT_NAM_CH_WATFLUXn
!
WRITE(UNIT=ILUDES,NML=NAM_FLAKEn)
WRITE(UNIT=ILUDES,NML=NAM_DIAG_FLAKEn)
WRITE(UNIT=ILUDES,NML=NAM_CH_WATFLUXn)
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_FLAKE_CONF_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_FLAKE_CONF_n
