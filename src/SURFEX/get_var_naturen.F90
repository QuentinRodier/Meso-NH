!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_VAR_NATURE_n(HPROGRAM,KI,PQS,PSNG,PSNV,PZ0EFF,PZ0,PZ0H,PTWSNOW,PBARE)
!     ######################################################################
!
!!****  *GET_VAR_NATURE_n* - routine to get variables defined only over nature
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
!       M. Jidane   08/2008 Z0 and Z0H recovery from nature tiles
!       S. Riette   06/2010 TWSNOW added
!       V. Masson   02/2015 adds LAI, height of trees, fraction of bare soil
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_GET_LUOUT
USE MODD_SURF_PAR,    ONLY   : XUNDEF
USE MODD_DATA_COVER_PAR,   ONLY   : NVT_NO
USE MODD_ISBA_n,           ONLY   : XVEGTYPE
!
USE MODD_DIAG_ISBA_n,      ONLY   : XAVG_QS, LSURF_VARS, XAVG_Z0EFF, LCOEF, XAVG_Z0, XAVG_Z0H
USE MODD_DIAG_MISC_ISBA_n, ONLY   : XAVG_PSNG, XAVG_PSNV, XAVG_TWSNOW,LSURF_MISC_BUDGET
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
INTEGER,              INTENT(IN)     :: KI      ! Number of points
REAL, DIMENSION(KI),  INTENT(OUT)    :: PQS     ! surface humidity
REAL, DIMENSION(KI),  INTENT(OUT)    :: PSNG    ! snow fraction over ground
REAL, DIMENSION(KI),  INTENT(OUT)    :: PSNV    ! snow fraction over vegetation
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZ0EFF  ! effective roughness length (z0v+z0rel)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZ0     ! surface roughness length
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZ0H    ! surface roughness length for heat
REAL, DIMENSION(KI),  INTENT(OUT)    :: PTWSNOW ! Snow total reservoir
REAL, DIMENSION(KI),  INTENT(OUT)    :: PBARE   ! bare soil fraction on grid mesh     (-)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_VAR_NATURE_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF (LSURF_VARS) THEN 
        PQS      = XAVG_QS      
   ELSE 
        PQS      = XUNDEF      
ENDIF           
!
IF (LSURF_MISC_BUDGET) THEN 
        PSNG     = XAVG_PSNG      
        PSNV     = XAVG_PSNV      
        PTWSNOW  = XAVG_TWSNOW
   ELSE 
        PSNG     = XUNDEF      
        PSNV     = XUNDEF      
        PTWSNOW  = XUNDEF
ENDIF           
!
IF (LCOEF) THEN
   PZ0EFF   = XAVG_Z0EFF
   PZ0      = XAVG_Z0      
   PZ0H     = XAVG_Z0H
ELSE
   PZ0EFF   = XUNDEF
   PZ0      = XUNDEF      
   PZ0H     = XUNDEF
ENDIF
IF (SIZE(PBARE) > 0) THEN
  PBARE = XVEGTYPE(:,NVT_NO)
ELSE
  PBARE = XUNDEF         
ENDIF
!
IF (LHOOK) CALL DR_HOOK('GET_VAR_NATURE_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_VAR_NATURE_n
