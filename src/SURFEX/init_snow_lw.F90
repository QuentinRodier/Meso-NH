!     #########
      SUBROUTINE INIT_SNOW_LW(PEMISSN,TPSNOW)
!     #######################################
!
!!****  *INIT_SNOW_LW* - routine to initialize snow surf. temp and emissivity
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
USE MODD_TYPE_SNOW
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XTT, XLMTT
!
USE MODE_SNOW3L
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
REAL,                 INTENT(IN)    :: PEMISSN ! snow emissivity
TYPE(SURF_SNOW),      INTENT(INOUT) :: TPSNOW  ! snow characteristics
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JPATCH ! loop counter on tiles
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_SNOW_LW',0,ZHOOK_HANDLE)
DO JPATCH=1,SIZE(TPSNOW%WSNOW,3)
!
!*       1.    Emissivity
!              ----------
!
  IF (TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
    WHERE(TPSNOW%WSNOW(:,1,JPATCH)==0. .OR. TPSNOW%WSNOW(:,1,JPATCH)==XUNDEF )
      TPSNOW%EMIS (:,JPATCH)= XUNDEF
    ELSEWHERE
      TPSNOW%EMIS (:,JPATCH)= PEMISSN
    END WHERE
  END IF
!
!*      2.     Surface temperature
!              -------------------
!
  IF (TPSNOW%SCHEME=='1-L') THEN
    WHERE(TPSNOW%WSNOW(:,1,JPATCH)==0. .OR. TPSNOW%WSNOW(:,1,JPATCH)==XUNDEF )
      TPSNOW%TS (:,JPATCH)= XUNDEF
    ELSEWHERE
      TPSNOW%TS(:,JPATCH) = TPSNOW%T(:, TPSNOW%NLAYER,JPATCH)
    END WHERE
  END IF
!
  IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
    WHERE(TPSNOW%WSNOW(:,1,JPATCH)==0. .OR. TPSNOW%WSNOW(:,1,JPATCH)==XUNDEF )
      TPSNOW%TS (:,JPATCH)= XUNDEF
    ELSEWHERE
      TPSNOW%TS(:,JPATCH) = XTT +  (TPSNOW%HEAT(:,1,JPATCH)              &
                                      +  XLMTT * TPSNOW%RHO(:,1,JPATCH) )  &
                                      /  SNOW3LSCAP(TPSNOW%RHO(:,1,JPATCH))  
      TPSNOW%TS(:,JPATCH) = MIN(XTT, TPSNOW%TS(:,JPATCH))
    END WHERE
  END IF
!
END DO
IF (LHOOK) CALL DR_HOOK('INIT_SNOW_LW',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INIT_SNOW_LW
