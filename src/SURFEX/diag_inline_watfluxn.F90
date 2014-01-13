!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_INLINE_WATFLUX_n (PTSTEP, PTA, PTS, PQA, PPA, PPS, PRHOA, PZONA,  &
                                           PMERA, PHT, PHW, PCD, PCDN, PCH, PRI, PHU, PZ0, &
                                           PZ0H, PQSAT, PSFTH, PSFTQ, PSFZON, PSFMER,      &
                                           PDIR_SW, PSCA_SW, PLW, PDIR_ALB, PSCA_ALB,      &
                                           PEMIS, PTRAD, PRAIN, PSNOW, PTICE, PSFTH_ICE,   &
                                           PSFTQ_ICE                                       )  
!     ###############################################################################
!
!!****  *DIAG_INLINE_WATFLUX_n * - computes diagnostics during WATFLUX time-step
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
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      B. Decharme 08/2009 : Diag for Earth System Model Coupling
!!      S. Riette   06/2009 CLS_2M becomes CLS_TQ, CLS_TQ and CLS_WIND have one
!!                          more argument (height of diagnostic)
!!------------------------------------------------------------------
!

!
!
USE MODD_CSTS,           ONLY : XTT
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SURF_ATM,       ONLY : LCPL_ESM
USE MODD_WATFLUX_n,      ONLY : LSBL
USE MODD_DIAG_WATFLUX_n, ONLY : N2M, LSURF_BUDGET, LCOEF, LSURF_VARS, &
                                  XT2M, XQ2M, XHU2M, XZON10M, XMER10M,  &
                                  XRN, XH, XLE, XLEI, XGFLUX, XRI, XCD, &
                                  XCH, XCE, XZ0, XZ0H, XQS, XSWD, XSWU, &
                                  XLWD, XLWU, XSWBD, XSWBU, XFMU, XFMV, &
                                  LSURF_BUDGETC, XT2M_MIN, XT2M_MAX,    &
                                  XDIAG_TS, XHU2M_MIN, XHU2M_MAX,       &
                                  XWIND10M, XWIND10M_MAX  
!
USE MODI_PARAM_CLS
USE MODI_CLS_TQ
USE MODI_CLS_WIND
USE MODI_DIAG_SURF_BUDGET_WATER
USE MODI_DIAG_SURF_BUDGETC_WATER
USE MODI_DIAG_CPL_ESM_WATER
! 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,               INTENT(IN) :: PTSTEP ! atmospheric time-step                 (s)
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
REAL, DIMENSION(:), INTENT(IN) :: PLW       ! longwave radiation (on horizontal surf.)
REAL, DIMENSION(:), INTENT(IN) :: PTRAD     ! radiative temperature                 (K)
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(:), INTENT(IN) :: PEMIS     ! emissivity                            (-)
!
REAL, DIMENSION(:), INTENT(IN) :: PRAIN     ! Rainfall (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PSNOW     ! Snowfall (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PSFTH_ICE ! heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSFTQ_ICE ! water flux (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PTICE     ! Ice Surface Temperature
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PTA)) :: ZH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_WATFLUX_N',0,ZHOOK_HANDLE)
XDIAG_TS(:) = PTS(:)
!
IF (.NOT. LSBL) THEN
!        
  IF (N2M==1) THEN
       
    CALL PARAM_CLS(PTA, PTS, PQA, PPA, PRHOA, PZONA, PMERA, PHT, PHW, &
                     PSFTH, PSFTQ, PSFZON, PSFMER,                       &
                     XT2M, XQ2M, XHU2M, XZON10M, XMER10M                       )  
  ELSE IF (N2M==2) THEN
    ZH(:)=2.          
    CALL CLS_TQ(PTA, PQA, PPA, PPS, PHT,         &
                  PCD, PCH, PRI,                   &
                  PTS, PHU, PZ0H, ZH,              &
                  XT2M, XQ2M, XHU2M                )  
    ZH(:)=10.                
    CALL CLS_WIND(PZONA, PMERA, PHW,             &
                    PCD, PCDN, PRI, ZH,            &
                    XZON10M, XMER10M               )  
  END IF
!
  IF (N2M>=1) THEN
    !
    XT2M_MIN(:) = MIN(XT2M_MIN(:),XT2M(:))
    XT2M_MAX(:) = MAX(XT2M_MAX(:),XT2M(:))
    !
    XHU2M_MIN(:) = MIN(XHU2M_MIN(:),XHU2M(:))
    XHU2M_MAX(:) = MAX(XHU2M_MAX(:),XHU2M(:))
    !
    XWIND10M    (:) = SQRT(XZON10M**2+XMER10M**2)
    XWIND10M_MAX(:) = MAX(XWIND10M_MAX(:),XWIND10M(:))
    !
    !* Richardson number
    XRI = PRI
    !
  ENDIF
!
ELSE
  IF (N2M>=1) THEN        
    XT2M    = XUNDEF
    XQ2M    = XUNDEF
    XHU2M   = XUNDEF
    XZON10M = XUNDEF
    XMER10M = XUNDEF
    XRI     = PRI           
  ENDIF
ENDIF        
!
IF (LSURF_BUDGET.OR.LSURF_BUDGETC) THEN
  !
  CALL  DIAG_SURF_BUDGET_WATER (XTT, PRHOA, PSFTH, PSFTQ,             &
                                  PDIR_SW, PSCA_SW, PLW,                &
                                  PDIR_ALB, PSCA_ALB, PEMIS, PTRAD,     &
                                  PSFZON, PSFMER,                       &
                                  XRN, XH, XLE, XLEI, XGFLUX,           &
                                  XSWD, XSWU, XSWBD, XSWBU, XLWD, XLWU, &
                                  XFMU, XFMV )  
  !
END IF
!
IF(LSURF_BUDGETC)THEN
  CALL DIAG_SURF_BUDGETC_WATER(PTSTEP, XRN, XH, XLE, XLEI, XGFLUX, &
                                 XSWD, XSWU, XLWD, XLWU, XFMU, XFMV  )  
ENDIF
!
IF (LCOEF) THEN
  !
  !* Transfer coefficients
  !
  XCD = PCD
  XCH = PCH
  XCE = PCH
  !
  !* Roughness lengths
  !
  XZ0  = PZ0
  XZ0H = PZ0H
  !
END IF
!
IF (LSURF_VARS) THEN
  !
  !* Humidity at saturation
  !
  XQS = PQSAT
  !
END IF
!
! Diag for Earth System Model coupling
!
IF (LCPL_ESM) THEN
!
  CALL DIAG_CPL_ESM_WATER(PTSTEP,XZON10M,XMER10M,XFMU,XFMV,  &
                            XSWD,XSWU,XGFLUX,PSFTQ,PRAIN,      &
                            PSNOW,PLW,PTICE,PSFTH_ICE,         &
                            PSFTQ_ICE,PDIR_SW,PSCA_SW          )  
! 
ENDIF
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_WATFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_INLINE_WATFLUX_n
