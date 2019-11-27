!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_VAR_TOWN_n (TOP, DGO, D, NT, HPROGRAM,KI,PQS,PZ0,PZ0H, &
                   PWALL_O_HOR,PBUILD_HEIGHT   )
!     ###################################################
!
!!****  *GET_VAR_TOWN_n* - routine to get variables defined only over town
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
!       M. Jidane   08/2008 Z0 and Z0H recovery from town tiles
!!      C.Lac       11/2019 correction in the drag formula and application to building in addition to tree
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_TEB_n,  ONLY : TEB_NP_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODI_GET_LUOUT
USE MODD_SURF_PAR,   ONLY   : XUNDEF
!
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
TYPE(TEB_OPTIONS_t),  INTENT(IN) :: TOP
TYPE(DIAG_OPTIONS_t), INTENT(IN) :: DGO
TYPE(DIAG_t),         INTENT(IN) :: D
TYPE(TEB_NP_t),       INTENT(IN) :: NT
!
 CHARACTER(LEN=6),     INTENT(IN)     :: HPROGRAM
INTEGER,              INTENT(IN)     :: KI      ! Number of points
REAL, DIMENSION(KI),  INTENT(OUT)    :: PQS     ! surface humidity
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZ0     ! surface roughness length
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZ0H    ! surface roughness length for heat
REAL, DIMENSION(KI),  INTENT(OUT)    :: PWALL_O_HOR   ! Facade surface density [m^2(fac.)/m^2(town)]
REAL, DIMENSION(KI),  INTENT(OUT)    :: PBUILD_HEIGHT ! Building height [m]
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JP     ! loop counter on TEB patches
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_VAR_TOWN_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF (DGO%LSURF_VARS) THEN 
  PQS  = D%XQS      
ELSE 
  PQS  = XUNDEF      
ENDIF           
IF (DGO%LCOEF) THEN 
  PZ0  = D%XZ0      
  PZ0H = D%XZ0H
ELSE 
  PZ0  = XUNDEF
  PZ0H = XUNDEF
ENDIF          
!
!* building height and external wall coverage fraction
!
PWALL_O_HOR   = 0.
PBUILD_HEIGHT = 0.

DO JP=1,TOP%NTEB_PATCH
  PWALL_O_HOR  (:) = PWALL_O_HOR  (:) + TOP%XTEB_PATCH(:,JP) * NT%AL(JP)%XWALL_O_HOR(:)
  PBUILD_HEIGHT(:) = PBUILD_HEIGHT(:) + TOP%XTEB_PATCH(:,JP) * NT%AL(JP)%XBLD_HEIGHT(:)
END DO

IF (LHOOK) CALL DR_HOOK('GET_VAR_TOWN_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_VAR_TOWN_n
