!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ##############################################################################################
      SUBROUTINE UPDATE_ESM_ISBA_n(HPROGRAM,KI,KSW,PZENITH,PSW_BANDS,PDIR_ALB,PSCA_ALB,PEMIS,PTSRAD)
!     ##############################################################################################
!
!!****  *UPDATE_ESM_ISBA_n* - update ISBA radiative properties in Earth System Model 
!!                            after the call to OASIS coupler in order 
!!                            to close the energy budget between radiative scheme and surfex
!!
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
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_SNOW
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_ISBA_n,   ONLY : NPATCH,XTG,TSNOW,XPSN,XVEG,XLAI,XZ0, &
                            XALBNIR,XALBVIS,XALBUV,XEMIS,XPATCH, &
                            LFLOOD,XFF,XEMISF,XEMIS_NAT,XTSRAD_NAT  
!
USE MODI_AVERAGE_RAD
USE MODI_UPDATE_RAD_ISBA_n
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
 CHARACTER(LEN=6),                   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
INTEGER,                            INTENT(IN)  :: KI        ! number of points
INTEGER,                            INTENT(IN)  :: KSW       ! number of short-wave spectral bands
!
REAL,             DIMENSION(KI),    INTENT(IN)  :: PZENITH   ! solar zenithal angle
REAL,             DIMENSION(KSW),   INTENT(IN)  :: PSW_BANDS ! short-wave spectral bands
!
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB  ! direct albedo for each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB  ! diffuse albedo for each band
REAL,             DIMENSION(KI),    INTENT(OUT) :: PEMIS     ! emissivity
REAL,             DIMENSION(KI),    INTENT(OUT) :: PTSRAD    ! radiative temperature
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(KI,KSW,NPATCH) :: ZDIR_ALB_PATCH
REAL, DIMENSION(KI,KSW,NPATCH) :: ZSCA_ALB_PATCH
REAL, DIMENSION(KI,NPATCH)     :: ZEMIS_PATCH
REAL, DIMENSION(KI,NPATCH)     :: ZTRAD_PATCH
REAL, DIMENSION(KI)            :: ZEMIS     ! emissivity
!
INTEGER           :: JPATCH ! loop on patches
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.     Defaults
!               --------
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_ISBA_N',0,ZHOOK_HANDLE)
!
ZDIR_ALB_PATCH = 0.0
ZSCA_ALB_PATCH = 0.0
ZEMIS_PATCH    = 0.0
ZTRAD_PATCH    = 0.0
!
!*       2.     Update nature albedo and emissivity
!               -----------------------------------
!
 CALL UPDATE_RAD_ISBA_n(LFLOOD,TSNOW%SCHEME,PZENITH,PSW_BANDS,XVEG,XLAI,&
                         XZ0,XALBNIR,XALBVIS,XALBUV,XEMIS,             &
                         ZDIR_ALB_PATCH,ZSCA_ALB_PATCH,ZEMIS_PATCH     )  
!
!*       3.     radiative surface temperature
!               -----------------------------
!
DO JPATCH=1,NPATCH
!
   ZEMIS(:) = XEMIS(:,JPATCH)
!   
   IF(LFLOOD.AND.(TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO'))THEN
     WHERE(XPSN(:,JPATCH)<1.0.AND.XEMIS(:,JPATCH)/=XUNDEF)          
          ZEMIS(:) = ((1.-XFF(:,JPATCH)-XPSN(:,JPATCH))*XEMIS(:,JPATCH) + XFF(:,JPATCH)*XEMISF(:,JPATCH))/(1.-XPSN(:,JPATCH))
     ENDWHERE   
  ENDIF
!
  IF (TSNOW%SCHEME=='D95' .OR. TSNOW%SCHEME=='EBA') THEN
    ZTRAD_PATCH(:,JPATCH) = XTG(:,1,JPATCH)
  ELSE IF (TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') THEN
    WHERE (XEMIS(:,JPATCH)/=XUNDEF)
      ZTRAD_PATCH(:,JPATCH) =( ( (1.-XPSN(:,JPATCH))*ZEMIS     (:)       *XTG     (:,1,JPATCH)**4          &
                                  +    XPSN(:,JPATCH) *TSNOW%EMIS(:,JPATCH)*TSNOW%TS(:,JPATCH)**4 ) )**0.25  &
                               / ZEMIS_PATCH(:,JPATCH)**0.25  
    END WHERE
  END IF
END DO
!
!
!*       4.     averaged fields
!               ---------------
!
 CALL AVERAGE_RAD(XPATCH,                                                   &
                   ZDIR_ALB_PATCH, ZSCA_ALB_PATCH, ZEMIS_PATCH, ZTRAD_PATCH, &
                   PDIR_ALB,       PSCA_ALB,       XEMIS_NAT,   XTSRAD_NAT   )  
!
PEMIS = XEMIS_NAT
PTSRAD = XTSRAD_NAT
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UPDATE_ESM_ISBA_n
