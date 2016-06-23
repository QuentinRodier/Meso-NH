!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE UPDATE_ESM_ISBA_n (I, &
                                    KI,KSW,PZENITH,PSW_BANDS,PDIR_ALB,& 
                                   PSCA_ALB,PEMIS,PTSRAD,PTSURF      )
!     ################################################################
!
!!****  *UPDATE_ESM_ISBA_n* - update ISBA radiative and physical properties in Earth System Model 
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
!!      B. Decharme 06/2013 new coupling variables
!!      P. Samuelsson 10/2014 MEB
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_TYPE_SNOW
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODI_AVERAGE_RAD
USE MODI_AVERAGE_TSURF
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
!
TYPE(ISBA_t), INTENT(INOUT) :: I
!
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
REAL,             DIMENSION(KI),    INTENT(OUT) :: PTSURF    ! surface effective temperature         (K)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(KI,KSW,I%NPATCH) :: ZDIR_ALB_PATCH
REAL, DIMENSION(KI,KSW,I%NPATCH) :: ZSCA_ALB_PATCH
REAL, DIMENSION(KI,I%NPATCH)     :: ZEMIS_PATCH
REAL, DIMENSION(KI,I%NPATCH)     :: ZTSRAD_PATCH
REAL, DIMENSION(KI,I%NPATCH)     :: ZTSURF_PATCH
REAL, DIMENSION(KI,I%NPATCH)     :: ZEMIS          ! emissivity with flood
!
LOGICAL :: LEXPLICIT_SNOW ! snow scheme key
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.     Defaults
!               --------
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_ISBA_N',0,ZHOOK_HANDLE)
!
ZDIR_ALB_PATCH(:,:,:) = 0.0
ZSCA_ALB_PATCH(:,:,:) = 0.0
ZEMIS_PATCH   (:,:  ) = 0.0
ZEMIS         (:,:  ) = I%XEMIS(:,:)
!
LEXPLICIT_SNOW = (I%TSNOW%SCHEME=='3-L'.OR.I%TSNOW%SCHEME=='CRO')
!
ZTSRAD_PATCH (:,:) = I%XTG(:,1,:)
ZTSURF_PATCH (:,:) = I%XTG(:,1,:)
!
!
!*       2.     Update nature albedo and emissivity
!               -----------------------------------
!
 CALL UPDATE_RAD_ISBA_n(I, &
                        I%LFLOOD,I%TSNOW%SCHEME,PZENITH,PSW_BANDS,I%XVEG,I%XLAI,I%XZ0, &
                         I%LMEB_PATCH,I%XLAIGV,I%XGNDLITTER,I%XZ0LITTER,I%XH_VEG,      &
                         I%XALBNIR,I%XALBVIS,I%XALBUV,I%XEMIS,                       &
                         ZDIR_ALB_PATCH,ZSCA_ALB_PATCH,ZEMIS_PATCH           )
!
!*       3.     radiative surface temperature
!               -----------------------------
!
IF(LEXPLICIT_SNOW.AND.I%LFLOOD)THEN
  WHERE(I%XPSN(:,:)<1.0.AND.I%XEMIS(:,:)/=XUNDEF)
       ZEMIS(:,:) = ((1.-I%XFF(:,:)-I%XPSN(:,:))*I%XEMIS(:,:) + I%XFF(:,:)*I%XEMISF(:,:)) / (1.-I%XPSN(:,:))
  ENDWHERE
ENDIF
!
IF(LEXPLICIT_SNOW)THEN
  WHERE(I%XEMIS(:,:)/=XUNDEF.AND.ZEMIS_PATCH(:,:)/=0.)
       ZTSRAD_PATCH(:,:) = ( ( (1.-I%XPSN(:,:))*ZEMIS     (:,:)*I%XTG   (:,1,:)**4     &
                             +     I%XPSN(:,:) *I%TSNOW%EMIS(:,:)*I%TSNOW%TS(:,:)**4 )   &
                           / ZEMIS_PATCH(:,:) )**0.25         
  ENDWHERE
ENDIF        
!
!
!*       4.     averaged fields
!               ---------------
!
 CALL AVERAGE_RAD(I%XPATCH,                                                     &
                   ZDIR_ALB_PATCH, ZSCA_ALB_PATCH, ZEMIS_PATCH, ZTSRAD_PATCH, &
                   PDIR_ALB,       PSCA_ALB,       I%XEMIS_NAT,   I%XTSRAD_NAT    )  
!
PEMIS = I%XEMIS_NAT
PTSRAD = I%XTSRAD_NAT
!
!* averaged effective temperature
!
IF(LEXPLICIT_SNOW)THEN
  ZTSURF_PATCH(:,:) = I%XTG(:,1,:)*(1.-I%XPSN(:,:)) + I%TSNOW%TS(:,:)*I%XPSN(:,:)
ENDIF
!
 CALL AVERAGE_TSURF(I%XPATCH, ZTSURF_PATCH, PTSURF)
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UPDATE_ESM_ISBA_n
