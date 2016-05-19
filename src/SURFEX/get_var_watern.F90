!     #########
      SUBROUTINE GET_VAR_WATER_n(HPROGRAM,KI,HWATER,PQS,PZ0,PZ0H)
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
!!	P. Le Moigne *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2006
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
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
USE MODD_DIAG_WATFLUX_n, ONLY   : XZ0, XZ0H, XQS, LSURF_VARS, LCOEF
!
!-------------------------------------------------------------------------------

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('GET_VAR_WATFLX_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF (LSURF_VARS) THEN 
        PQS      = XQS      
ELSE 
        PQS      = XUNDEF      
ENDIF           
IF (LCOEF) THEN 
        PZ0      = XZ0
        PZ0H     = XZ0H
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
USE MODD_DIAG_FLAKE_n, ONLY   : XZ0, XZ0H, XQS, LSURF_VARS, LCOEF
!
!-------------------------------------------------------------------------------

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('GET_VAR_FLAKE_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF (LSURF_VARS) THEN 
        PQS      = XQS      
ELSE 
        PQS      = XUNDEF      
ENDIF           
IF (LCOEF) THEN 
        PZ0      = XZ0
        PZ0H     = XZ0H
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
