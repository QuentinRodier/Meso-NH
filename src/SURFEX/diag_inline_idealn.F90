!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_INLINE_IDEAL_n (DGL, PTSTEP, PTA, PTS, PQA, PPA, PPS, PRHOA, PZONA,  &
                                         PMERA, PHT, PHW, PRAIN, PSNOW,                &
                                         PCD, PCDN, PCH, PRI, PHU, PZ0,                &
                                         PZ0H, PQSAT, PSFTH, PSFTQ, PSFZON, PSFMER,    &
                                         PDIR_SW, PSCA_SW, PLW, PDIR_ALB, PSCA_ALB,    &
                                         PLE, PLEI, PSUBL, PLWUP)  
!     ###############################################################################
!
!!****  *DIAG_INLINE_IDEAL_n * - computes diagnostics during IDEAL time-step
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     P. Le Moigne 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2015
!!------------------------------------------------------------------
!
!
USE MODD_DIAG_IDEAL_n, ONLY : DIAG_IDEAL_t
!
USE MODD_CSTS,         ONLY : XTT, XLVTT
USE MODD_SURF_PAR,     ONLY : XUNDEF
!
USE MODI_PARAM_CLS
USE MODI_CLS_TQ
USE MODI_CLS_WIND
USE MODI_DIAG_SURF_BUDGET_IDEAL
USE MODI_DIAG_SURF_BUDGETC_IDEAL
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(DIAG_IDEAL_t), INTENT(INOUT) :: DGL
!
REAL              , INTENT(IN) :: PTSTEP ! atmospheric time-step (s)
REAL, DIMENSION(:), INTENT(IN) :: PTA    ! atmospheric temperature
REAL, DIMENSION(:), INTENT(IN) :: PTS    ! surface temperature
REAL, DIMENSION(:), INTENT(IN) :: PQA    ! atmospheric specific humidity
REAL, DIMENSION(:), INTENT(IN) :: PPA    ! atmospheric level pressure
REAL, DIMENSION(:), INTENT(IN) :: PPS    ! surface pressure
REAL, DIMENSION(:), INTENT(IN) :: PRHOA  ! air density
REAL, DIMENSION(:), INTENT(IN) :: PZONA  ! zonal wind
REAL, DIMENSION(:), INTENT(IN) :: PMERA  ! meridian wind
REAL, DIMENSION(:), INTENT(IN) :: PHT    ! atmospheric level height
REAL, DIMENSION(:), INTENT(IN) :: PHW    ! atmospheric level height for wind
REAL, DIMENSION(:), INTENT(IN) :: PRAIN  ! Rainfall (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PSNOW  ! Snowfall (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PCD    ! drag coefficient for momentum
REAL, DIMENSION(:), INTENT(IN) :: PCDN   ! neutral drag coefficient
REAL, DIMENSION(:), INTENT(IN) :: PCH    ! drag coefficient for heat
REAL, DIMENSION(:), INTENT(IN) :: PRI    ! Richardson number
REAL, DIMENSION(:), INTENT(IN) :: PHU    ! near-surface humidity
REAL, DIMENSION(:), INTENT(IN) :: PZ0    ! roughness length for momentum
REAL, DIMENSION(:), INTENT(IN) :: PZ0H   ! roughness length for heat
REAL, DIMENSION(:), INTENT(IN) :: PQSAT  ! humidity at saturation
REAL, DIMENSION(:), INTENT(IN) :: PSFZON ! zonal friction
REAL, DIMENSION(:), INTENT(IN) :: PSFMER ! meridian friction
REAL, DIMENSION(:), INTENT(IN) :: PSFTH  ! heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSFTQ  ! water flux (kg/m2/s)
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
!                                           !                                       (W/m2)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
!                                           !                                       (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW       ! longwave radiation (on horizontal surf.) (W/m2)
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(:), INTENT(IN) :: PLE       ! total latent heat flux (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLEI      ! sublimation heat flux (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSUBL     ! sublimation (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PLWUP     ! upward longwave radiation (W/m2)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PTA)) :: ZH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_IDEAL_N',0,ZHOOK_HANDLE)
!
DGL%XDIAG_TS(:) = PTS(:)
!
  IF (DGL%N2M==1) THEN
    CALL PARAM_CLS(PTA, PTS, PQA, PPA, PRHOA, PZONA, PMERA, PHT, PHW, &
                     PSFTH, PSFTQ, PSFZON, PSFMER,                    &
                     DGL%XT2M, DGL%XQ2M, DGL%XHU2M, DGL%XZON10M, DGL%XMER10M )  
  ELSE IF (DGL%N2M==2) THEN
    ZH(:)=2.          
    CALL CLS_TQ(PTA, PQA, PPA, PPS, PHT,         &
                  PCD, PCH, PRI,                 &
                  PTS, PHU, PZ0H, ZH,            &
                  DGL%XT2M, DGL%XQ2M, DGL%XHU2M  )  
    ZH(:)=10.                
    CALL CLS_WIND(PZONA, PMERA, PHW,        &
                  PCD, PCDN, PRI, ZH,       &
                  DGL%XZON10M, DGL%XMER10M  )  
  END IF
!
  IF (DGL%N2M>=1) THEN
    !
    DGL%XT2M_MIN(:) = MIN(DGL%XT2M_MIN(:),DGL%XT2M(:))
    DGL%XT2M_MAX(:) = MAX(DGL%XT2M_MAX(:),DGL%XT2M(:))
    !
    DGL%XHU2M_MIN(:) = MIN(DGL%XHU2M_MIN(:),DGL%XHU2M(:))
    DGL%XHU2M_MAX(:) = MAX(DGL%XHU2M_MAX(:),DGL%XHU2M(:))
    !
    DGL%XWIND10M(:) = SQRT(DGL%XZON10M(:)**2+DGL%XMER10M(:)**2)
    DGL%XWIND10M_MAX(:) = MAX(DGL%XWIND10M_MAX(:),DGL%XWIND10M(:))
    !
    !* Richardson number
    DGL%XRI = PRI
    !
  ENDIF
!
IF (DGL%LSURF_BUDGET) THEN
  !
  DGL%XLE  (:) = PLE  (:)
  DGL%XLEI (:) = PLEI (:)
  DGL%XEVAP(:) = PSFTQ(:)
  DGL%XSUBL(:) = PSUBL(:)
  !
  CALL  DIAG_SURF_BUDGET_IDEAL ( PRHOA, PSFTH,                     &
                                  PDIR_SW, PSCA_SW, PLW,                &
                                  PDIR_ALB, PSCA_ALB, PLWUP,            &
                                  PSFZON, PSFMER, DGL%XLE, DGL%XRN,     &
                                  DGL%XH, DGL%XGFLUX, DGL%XSWD,         &
                                  DGL%XSWU, DGL%XSWBD, DGL%XSWBU,       &
                                  DGL%XLWD, DGL%XLWU, DGL%XFMU, DGL%XFMV )  
  !
END IF
!
IF( DGL%LSURF_BUDGETC)THEN
  CALL DIAG_SURF_BUDGETC_IDEAL(DGL, PTSTEP,  DGL%XRN,  DGL%XH,  DGL%XLE,  &
                               DGL%XLEI,  DGL%XGFLUX, DGL%XSWD,  DGL%XSWU, &
                               DGL%XLWD,  DGL%XLWU,  DGL%XFMU,  DGL%XFMV,  &
                               DGL%XEVAP, DGL%XSUBL                    )  
ENDIF
!
IF (DGL%LCOEF) THEN
  !
  !* Transfer coefficients
  !
  DGL%XCD = PCD
  DGL%XCH = PCH
  DGL%XCE = PCH
  !
  !* Roughness lengths
  !
  DGL%XZ0  = PZ0
  DGL%XZ0H = PZ0H
  !
END IF
!
IF (DGL%LSURF_VARS) THEN
  !
  !* Humidity at saturation
  !
  DGL%XQS = PQSAT
  !
END IF
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_IDEAL_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_INLINE_IDEAL_n
