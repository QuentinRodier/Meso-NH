!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_VAR_WATER_n (DGF, DGW, &
                                  HPROGRAM,KI,HWATER,PQS,PZ0,PZ0H)
!     ###########################################################
!
!!****  *GET_VAR_WATER_n* - routine to get variables defined only over water
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
!!      P. Le Moigne *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2006
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DIAG_FLAKE_n, ONLY : DIAG_FLAKE_t
USE MODD_DIAG_WATFLUX_n, ONLY : DIAG_WATFLUX_t
!
USE MODI_GET_LUOUT
USE MODD_SURF_PAR,       ONLY   : XUNDEF
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
!
TYPE(DIAG_FLAKE_t), INTENT(INOUT) :: DGF
TYPE(DIAG_WATFLUX_t), INTENT(INOUT) :: DGW
!
 CHARACTER(LEN=6),     INTENT(IN)     :: HPROGRAM
 CHARACTER(LEN=6),     INTENT(IN)     :: HWATER
INTEGER,              INTENT(IN)     :: KI      ! Number of points
REAL, DIMENSION(KI),  INTENT(OUT)    :: PQS     ! surface humidity
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZ0     ! surface roughness length
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZ0H    ! surface roughness length for heat
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('GET_VAR_WATER_N',0,ZHOOK_HANDLE)
IF (HWATER=='FLAKE') THEN
   CALL GET_VAR_FLAKE_n
ELSE
   CALL GET_VAR_WATFLX_n
END IF
!
IF (LHOOK) CALL DR_HOOK('GET_VAR_WATER_N',1,ZHOOK_HANDLE)
 CONTAINS
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
SUBROUTINE GET_VAR_WATFLX_n
!
!
!-------------------------------------------------------------------------------

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('GET_VAR_WATFLX_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF (DGW%LSURF_VARS) THEN 
        PQS      = DGW%XQS      
ELSE 
        PQS      = XUNDEF      
ENDIF           
IF (DGW%LCOEF) THEN 
        PZ0      = DGW%XZ0
        PZ0H     = DGW%XZ0H
ELSE 
        PZ0      = XUNDEF
        PZ0H     = XUNDEF
ENDIF           
IF (LHOOK) CALL DR_HOOK('GET_VAR_WATFLX_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_VAR_WATFLX_n
!
!-------------------------------------------------------------------------------
!
SUBROUTINE GET_VAR_FLAKE_n
!
!
!-------------------------------------------------------------------------------

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('GET_VAR_FLAKE_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF (DGF%LSURF_VARS) THEN 
        PQS      = DGF%XQS      
ELSE 
        PQS      = XUNDEF      
ENDIF           
IF (DGF%LCOEF) THEN 
        PZ0      = DGF%XZ0
        PZ0H     = DGF%XZ0H
ELSE 
        PZ0      = XUNDEF
        PZ0H     = XUNDEF
ENDIF           
IF (LHOOK) CALL DR_HOOK('GET_VAR_FLAKE_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_VAR_FLAKE_n
!
!==============================================================================
!
END SUBROUTINE GET_VAR_WATER_n
