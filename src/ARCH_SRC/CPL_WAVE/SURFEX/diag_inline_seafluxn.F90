!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_INLINE_SEAFLUX_n (DGS, DGSI, S, &
                                  PTSTEP, PTA, PQA, &
     PPA, PPS, PRHOA, PZONA,                             &
     PMERA, PHT, PHW, PCD, PCDN, PCH, PCE, PRI, PHU,     &
     PZ0H, PQSAT, PSFTH, PSFTQ, PSFZON, PSFMER,     &
     PDIR_SW, PSCA_SW, PLW, PDIR_ALB, PSCA_ALB, &
     PEMIS, PTRAD, PRAIN, PSNOW,                         & 
     PCD_ICE, PCDN_ICE, PCH_ICE, PCE_ICE, PRI_ICE,       &
     PZ0_ICE, PZ0H_ICE, PQSAT_ICE, PSFTH_ICE, PSFTQ_ICE, &
     PSFZON_ICE, PSFMER_ICE )
                                          
!     #####################################################################################
!
!!****  *DIAG_INLINE_SEAFLUX_n * - computes diagnostics during SEAFLUX time-step
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
!!      Modified    01/2006 : sea flux parameterization.
!!      B. Decharme 08/2009 : Diag for Earth System Model Coupling
!!      S. Riette   06/2009 CLS_2M becomes CLS_TQ, CLS_TQ and CLS_WIND have one
!!                          more argument (height of diagnostic)
!!      B. Decharme 04/2013 : Add EVAP and SUBL diag
!!      S. Senesi   01/2014 ! introduce fractional seaice and sea-ice model 
!!      J. Pianezze 08/2016 : Add surface pressure coupling parameter
!!------------------------------------------------------------------
!

!
!
!
USE MODD_DIAG_SEAFLUX_n, ONLY : DIAG_SEAFLUX_t
USE MODD_DIAG_SEAICE_n, ONLY : DIAG_SEAICE_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODD_CSTS,           ONLY : XTTS
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SFX_OASIS,      ONLY : LCPL_SEA
!
USE MODD_TYPES_GLT,     ONLY : T_GLT
USE MODD_GLT_PARAM ,    ONLY : GELATO_DIM=>NX
USE MODE_GLT_STATS ,    ONLY : GLT_AVHICEM, GLT_AVHSNWM
USE MODI_PARAM_CLS
USE MODI_CLS_TQ
USE MODI_CLS_WIND
USE MODI_DIAG_SURF_BUDGET_SEA
USE MODI_DIAG_SURF_BUDGETC_SEA
USE MODI_DIAG_CPL_ESM_SEA
!
USE MODI_SEAFLUX_ALBEDO
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_SEAFLUX_t), INTENT(INOUT) :: DGS
TYPE(DIAG_SEAICE_t), INTENT(INOUT) :: DGSI
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
REAL,               INTENT(IN) :: PTSTEP ! atmospheric time-step                 (s)
REAL, DIMENSION(:), INTENT(IN) :: PTA    ! atmospheric temperature
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
REAL, DIMENSION(:), INTENT(IN) :: PCE    ! drag coefficient for vapor
REAL, DIMENSION(:), INTENT(IN) :: PRI    ! Richardson number
REAL, DIMENSION(:), INTENT(IN) :: PHU    ! near-surface humidity
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
!
REAL, DIMENSION(:), INTENT(IN)    :: PCD_ICE    ! drag coefficient for momentum
REAL, DIMENSION(:), INTENT(IN)    :: PCDN_ICE   ! neutral drag coefficient
REAL, DIMENSION(:), INTENT(IN)    :: PCH_ICE    ! drag coefficient for heat
REAL, DIMENSION(:), INTENT(IN)    :: PCE_ICE    ! drag coefficient for vapor
REAL, DIMENSION(:), INTENT(IN)    :: PRI_ICE    ! Richardson number
REAL, DIMENSION(:), INTENT(IN)    :: PZ0_ICE    ! roughness length for momentum
REAL, DIMENSION(:), INTENT(IN)    :: PZ0H_ICE   ! roughness length for heat
REAL, DIMENSION(:), INTENT(IN)    :: PQSAT_ICE  ! humidity at saturation
REAL, DIMENSION(:), INTENT(IN)    :: PSFTH_ICE  ! heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(IN)    :: PSFTQ_ICE  ! water flux (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN)    :: PSFZON_ICE ! zonal friction
REAL, DIMENSION(:), INTENT(IN)    :: PSFMER_ICE ! meridian friction
!
!*      0.2    declarations of local variables
!
LOGICAL                         :: GSIC
REAL, DIMENSION(SIZE(PTA))      :: ZZ0W
REAL, DIMENSION(SIZE(PTA))      :: ZH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_SEAFLUX_N',0,ZHOOK_HANDLE)
!
! * Mean surface temperature need to couple with AGCM
!
IF (S%LHANDLE_SIC) THEN
   DGS%XTS   (:) = (1 - S%XSIC(:)) * S%XSST(:) + S%XSIC(:) * S%XTICE(:)
   DGS%XTSRAD(:) = PTRAD(:)
ELSE
   DGS%XTS   (:) = S%XSST (:)
   DGS%XTSRAD(:) = PTRAD(:)
ENDIF
!
IF (.NOT. S%LSBL) THEN
!
  IF (DGS%N2M==1) THEN        
    CALL PARAM_CLS(PTA, S%XSST, PQA, PPA, PRHOA, PZONA, PMERA, PHT, PHW, &
         PSFTH, PSFTQ, PSFZON, PSFMER,                                 &
         DGS%XT2M, DGS%XQ2M, DGS%XHU2M, DGS%XZON10M, DGS%XMER10M )  
    IF (S%LHANDLE_SIC) THEN
       CALL PARAM_CLS(PTA, S%XTICE, PQA, PPA, PRHOA, PZONA, PMERA, PHT, PHW, &
            PSFTH_ICE, PSFTQ_ICE, PSFZON_ICE, PSFMER_ICE,                  &
            DGS%XT2M_ICE, DGS%XQ2M_ICE, DGS%XHU2M_ICE, DGS%XZON10M_ICE, DGS%XMER10M_ICE  )  
    ENDIF
  ELSE IF (DGS%N2M==2) THEN
    ZH(:)=2.          
    CALL CLS_TQ(PTA, PQA, PPA, PPS, PHT,          &
                  PCD, PCH, PRI,                  &
                  S%XSST, PHU, PZ0H, ZH,            &
                  DGS%XT2M, DGS%XQ2M, DGS%XHU2M)
    ZH(:)=10.                
    CALL CLS_WIND(PZONA, PMERA, PHW,              &
                    PCD, PCDN, PRI, ZH,           &
                    DGS%XZON10M, DGS%XMER10M)  
    IF (S%LHANDLE_SIC) THEN
       ZH(:)=2.          
       CALL CLS_TQ(PTA, PQA, PPA, PPS, PHT,  &
            PCD_ICE, PCH_ICE, PRI_ICE,       &
            S%XTICE, PHU, PZ0H_ICE, ZH,        &
            DGS%XT2M_ICE, DGS%XQ2M_ICE, DGS%XHU2M_ICE)  
       ZH(:)=10.                
       CALL CLS_WIND(PZONA, PMERA, PHW,      &
            PCD_ICE, PCDN_ICE, PRI_ICE, ZH,  &
            DGS%XZON10M_ICE, DGS%XMER10M_ICE  )  
    ENDIF 
  END IF
!
  IF (DGS%N2M>=1) THEN
     IF (S%LHANDLE_SIC) THEN
        !
        DGS%XT2M    = DGS%XT2M    * (1 - S%XSIC) + DGS%XT2M_ICE    * S%XSIC
        DGS%XQ2M    = DGS%XQ2M    * (1 - S%XSIC) + DGS%XQ2M_ICE    * S%XSIC
        DGS%XHU2M   = DGS%XHU2M   * (1 - S%XSIC) + DGS%XHU2M_ICE   * S%XSIC
        !
        DGS%XZON10M(:) = DGS%XZON10M(:) * (1 - S%XSIC(:)) + DGS%XZON10M_ICE(:) * S%XSIC(:)
        DGS%XMER10M(:) = DGS%XMER10M(:) * (1 - S%XSIC(:)) + DGS%XMER10M_ICE(:) * S%XSIC(:)
        DGS%XWIND10M_ICE(:) = SQRT(DGS%XZON10M_ICE(:)**2+DGS%XMER10M_ICE(:)**2)
        !
        DGS%XRI    = PRI     * (1 - S%XSIC) + PRI_ICE     * S%XSIC
        DGS%XRI_ICE=PRI_ICE
     ELSE
        DGS%XRI    =PRI
     ENDIF
    !
    DGS%XT2M_MIN(:) = MIN(DGS%XT2M_MIN(:),DGS%XT2M(:))
    DGS%XT2M_MAX(:) = MAX(DGS%XT2M_MAX(:),DGS%XT2M(:))
    !
    DGS%XHU2M_MIN(:) = MIN(DGS%XHU2M_MIN(:),DGS%XHU2M(:))
    DGS%XHU2M_MAX(:) = MAX(DGS%XHU2M_MAX(:),DGS%XHU2M(:))
    !
    DGS%XWIND10M(:) = SQRT(DGS%XZON10M(:)**2+DGS%XMER10M(:)**2)
    DGS%XWIND10M_MAX(:) = MAX(DGS%XWIND10M_MAX(:),DGS%XWIND10M(:))
    !
  ENDIF
!
ELSE
  IF (DGS%N2M>=1) THEN
    DGS%XT2M    = XUNDEF
    DGS%XQ2M    = XUNDEF
    DGS%XHU2M   = XUNDEF
    DGS%XZON10M = XUNDEF
    DGS%XMER10M = XUNDEF
    DGS%XRI     = PRI
  ENDIF
ENDIF
!
IF (DGS%LSURF_BUDGET.OR.DGS%LSURF_BUDGETC) THEN
!
  CALL SEAFLUX_ALBEDO(PDIR_SW,PSCA_SW,PDIR_ALB,PSCA_ALB,DGS%XALBT)
!
  CALL DIAG_SURF_BUDGET_SEA   (XTTS, S%XSST, PRHOA, PSFTH, PSFTH_ICE,    &
                                 PSFTQ, PSFTQ_ICE,                     &
                                 PDIR_SW, PSCA_SW, PLW, PDIR_ALB,      &
                                 PSCA_ALB,S%XICE_ALB, PEMIS, PTRAD,      &
                                 PSFZON, PSFZON_ICE, PSFMER,           &
                                 PSFMER_ICE, S%LHANDLE_SIC, S%XSIC, S%XTICE, &
                                 DGS%XRN, DGS%XH, DGS%XLE, DGS%XLE_ICE, DGS%XGFLUX,        &
                                 DGS%XSWD, DGS%XSWU, DGS%XSWBD, DGS%XSWBU, DGS%XLWD, DGS%XLWU, &
                                 DGS%XFMU, DGS%XFMV, DGS%XEVAP, DGS%XSUBL,             &
                                 DGS%XRN_ICE, DGS%XH_ICE, DGS%XGFLUX_ICE,          &
                                 DGS%XSWU_ICE, DGS%XSWBU_ICE, DGS%XLWU_ICE,        &
                                 DGS%XFMU_ICE, DGS%XFMV_ICE                    ) 
!
END IF
!
IF(DGS%LSURF_BUDGETC)THEN
  CALL DIAG_SURF_BUDGETC_SEA(DGS, &
                             PTSTEP, DGS%XRN, DGS%XH, DGS%XLE, DGS%XLE_ICE, DGS%XGFLUX,  &
                               DGS%XSWD, DGS%XSWU, DGS%XLWD, DGS%XLWU, DGS%XFMU, DGS%XFMV,   &
                               DGS%XEVAP, DGS%XSUBL, S%LHANDLE_SIC,            &
                               DGS%XRN_ICE, DGS%XH_ICE, DGS%XGFLUX_ICE,          &
                               DGS%XSWU_ICE, DGS%XLWU_ICE, DGS%XFMU_ICE, DGS%XFMV_ICE)
ENDIF
!
IF (DGS%LCOEF) THEN
   IF (S%LHANDLE_SIC) THEN 
      !
      !* Transfer coefficients
      !
      DGS%XCD = (1 - S%XSIC) * PCD + S%XSIC * PCD_ICE
      DGS%XCH = (1 - S%XSIC) * PCH + S%XSIC * PCH_ICE
      DGS%XCE = (1 - S%XSIC) * PCE + S%XSIC * PCE_ICE
      !
      !* Roughness lengths
      !
      ZZ0W = ( 1 - S%XSIC ) * 1.0/(LOG(PHW/S%XZ0)    **2)  +  &
                   S%XSIC   * 1.0/(LOG(PHW/PZ0_ICE)**2)  
      DGS%XZ0  = PHW  * EXP ( - SQRT ( 1./  ZZ0W ))
      ZZ0W = ( 1 - S%XSIC ) * 1.0/(LOG(PHW/PZ0H)    **2)  +  &
                   S%XSIC   * 1.0/(LOG(PHW/PZ0H_ICE)**2)  
      DGS%XZ0H = PHW  * EXP ( - SQRT ( 1./  ZZ0W ))

      DGS%XCD_ICE  = PCD_ICE
      DGS%XCH_ICE  = PCH_ICE
      DGS%XZ0_ICE  = PZ0_ICE
      DGS%XZ0H_ICE = PZ0H_ICE
      !
   ELSE
      !
      !* Transfer coefficients
      !
      DGS%XCD = PCD
      DGS%XCH = PCH
      DGS%XCE = PCE
      !
      !* Roughness lengths
      !
      DGS%XZ0  = S%XZ0
      DGS%XZ0H = PZ0H
   ENDIF
   !
ENDIF
!
IF (DGS%LSURF_VARS) THEN
  !
  !* Humidity at saturation
  !
   IF (S%LHANDLE_SIC) THEN 
      DGS%XQS     = (1 - S%XSIC) * PQSAT + S%XSIC * PQSAT_ICE
      DGS%XQS_ICE = PQSAT_ICE
   ELSE 
      DGS%XQS = PQSAT
   ENDIF
ENDIF
!
! Diags from embedded Seaice model
! CALL DIAG_INLINE_SEAICE() : simply  : 
!
IF (DGSI%LDIAG_SEAICE) THEN
   IF (TRIM(S%CSEAICE_SCHEME) == 'GELATO') THEN 
      GELATO_DIM=SIZE(PTA)
      DGSI%XSIT  = RESHAPE(glt_avhicem(S%TGLT%dom,S%TGLT%sit),(/GELATO_DIM/))
      DGSI%XSND  = RESHAPE(glt_avhsnwm(S%TGLT%dom,S%TGLT%sit),(/GELATO_DIM/))
      DGSI%XMLT  = S%TGLT%oce_all(:,1)%tml
   ELSE
      ! Placeholder for an alternate seaice scheme
   ENDIF
ENDIF
!
! Diags for Earth System Model coupling or for embedded Seaice model
! (we are actually using XCPL_.. variables for feeding the seaice model)
!
GSIC=(S%LHANDLE_SIC.AND.(S%CSEAICE_SCHEME /= 'NONE  '))
!
IF (LCPL_SEA.OR.GSIC) THEN
!
  CALL DIAG_CPL_ESM_SEA(S, &
                        PTSTEP,DGS%XZON10M,DGS%XMER10M,DGS%XFMU,DGS%XFMV,  &
                          DGS%XSWD,DGS%XSWU,DGS%XGFLUX,PSFTQ,PRAIN,    &
                          PSNOW,PLW,PPS,S%XTICE,PSFTH_ICE,       &
                          PSFTQ_ICE,PDIR_SW,PSCA_SW,       &
                          DGS%XSWU_ICE,DGS%XLWU_ICE,GSIC           )
! 
ENDIF
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_SEAFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_INLINE_SEAFLUX_n
