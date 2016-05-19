!     #########
SUBROUTINE UNPACK_DIAG_PATCH_n(KMASK,KSIZE,KNPATCH,KPATCH,    &
                                 PCPL_DRAIN,PCPL_RUNOFF,      &
                                 PCPL_EFLOOD,PCPL_PFLOOD,     &
                                 PCPL_IFLOOD,PCPL_ICEFLUX     )  
!##############################################
!
!!****  *UNPACK_DIAG_PATCH_n* - unpacks ISBA diagnostics
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
!!      Modified    10/2004 by P. Le Moigne: Halstead coefficient
!!      Modified    10/2005 by P. Le Moigne: Deallocation (EBA)
!!      Modified    05/2008 by B. Decharme : Flooding scheme
!!      Modified    01/2010 by B. Decharme : new diag
!!      Modified      04-09 by A.L. Gibelin : Add carbon diagnostics
!!      Modified      05-09 by A.L. Gibelin : Add carbon spinup
!!      Modified    08/2012 by B. Decharme : optimization
!!
!!------------------------------------------------------------------
!
USE MODD_PACK_ISBA,      ONLY : XP_LVTT, XP_LSTT
USE MODD_PACK_DIAG_ISBA, ONLY :   XP_RNSNOW, XP_HSNOW, XP_HPSNOW, XP_SMELTFLUX, &
                                  XP_GFLUXSNOW, XP_USTARSNOW,                   &
                                  XP_GRNDFLUX, XP_SRSFC, XP_RRSFC, XP_LESL,     &
                                  XP_CDSNOW, XP_CHSNOW, XP_SNOWTEMP,            &
                                  XP_SNOWLIQ, XP_SNOWDZ, XP_SNOWHMASS,          &
                                  XP_RN_ISBA, XP_H_ISBA, XP_LEG_ISBA,           &
                                  XP_LEGI_ISBA, XP_LEV_ISBA, XP_LETR_ISBA,      &
                                  XP_USTAR_ISBA, XP_LER_ISBA,                   &
                                  XP_LE_ISBA, XP_GFLUX_ISBA, XP_MELTADV,        &
                                  XP_LEI_ISBA, XP_IACAN, XP_LEI,                &
                                  XP_CH, XP_CD, XP_CDN, XP_RI, XP_HU, XP_HUG,   &
                                  XP_RN, XP_H, XP_LEG, XP_LEGI, XP_LEV,         &
                                  XP_LES, XP_LER, XP_LETR, XP_EVAP, XP_GFLUX,   &
                                  XP_RESTORE, XP_DRAIN, XP_RUNOFF, XP_MELT,     &
                                  XP_SNOWFREE_ALB, XP_Z0_WITH_SNOW,             &
                                  XP_Z0H_WITH_SNOW, XP_Z0EFF,                   &
                                  XP_CG, XP_C1, XP_C2, XP_WGEQ, XP_CT, XP_RS,   &
                                  XP_T2M, XP_Q2M, XP_HU2M, XP_TS, XP_TSRAD,     &
                                  XP_ZON10M, XP_MER10M, XP_HV,                  &
                                  XP_SNOWFREE_ALB_VEG, XP_SNOWFREE_ALB_SOIL,    &
                                  XP_SWI, XP_TSWI, XP_QS, XP_CE,                &
                                  XP_TWSNOW, XP_TDSNOW,                         &
                                  XP_SWD, XP_SWU, XP_SWBD, XP_SWBU,             &
                                  XP_LWD, XP_LWU, XP_FMU, XP_FMV, XP_HORT,      &
                                  XP_DRIP, XP_ALBT, XP_IFLOOD, XP_PFLOOD,       &
                                  XP_LE_FLOOD, XP_LEI_FLOOD, XP_ICEFLUX,        &
                                  XP_RRVEG, XP_GPP, XP_RESP_AUTO, XP_RESP_ECO,  &
                                  XP_FAPAR, XP_FAPIR, XP_FAPAR_BS, XP_FAPIR_BS, &
                                  XP_IRRIG_FLUX,XP_DWG,XP_DWGI,XP_DSWE,         &
                                  XP_WATBUD,                                    &                                  
                                  XBLOCK_SIMPLE, XBLOCK_GROUND, XBLOCK_SNOW,    &
                                  XBLOCK_KSW, XBLOCK_ABC, XBLOCK_0, XBLOCK_00
!
USE MODD_DIAG_ISBA_n,    ONLY :   N2M, LSURF_BUDGET, LCOEF, LSURF_VARS,         &
                                  XRN, XH, XLEI, XGFLUX, XRI, XCD, XCH, XCE,    &
                                  XT2M, XQ2M, XHU2M, XZON10M, XMER10M,          &
                                  XZ0_WITH_SNOW, XZ0H_WITH_SNOW, XZ0EFF, XQS,   &
                                  XSWD, XSWU, XSWBD, XSWBU, XLWD, XLWU,         &
                                  XFMU, XFMV, XTS, XTSRAD, XWIND10M  
!
USE MODD_GR_BIOG_n,      ONLY : XIACAN
!
USE MODD_ISBA_n,         ONLY : CPHOTO, LTRIP, LFLOOD, TSNOW, LGLACIER
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)                :: KSIZE, KPATCH, KNPATCH
INTEGER, DIMENSION(:), INTENT(IN)  :: KMASK
!
!Coupling variable
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_DRAIN
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_RUNOFF
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_EFLOOD
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_PFLOOD
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_IFLOOD
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_ICEFLUX
!
INTEGER :: JJ, JI, JSW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('UNPACK_DIAG_PATCH_N',0,ZHOOK_HANDLE)
!
IF (KNPATCH==1) THEN
  !
  XTS(:, KPATCH) = XP_TS(:)
  XTSRAD(:, KPATCH) = XP_TSRAD(:)
  IF (N2M>=1) THEN
    XT2M    (:, KPATCH)    = XP_T2M    (:)
    XQ2M    (:, KPATCH)    = XP_Q2M    (:)
    XHU2M   (:, KPATCH)    = XP_HU2M   (:)
    XZON10M (:, KPATCH)    = XP_ZON10M (:)
    XMER10M (:, KPATCH)    = XP_MER10M (:)
    XRI     (:, KPATCH)    = XP_RI     (:)
!    
    XWIND10M(:, KPATCH)    = SQRT(XP_ZON10M(:)**2+XP_MER10M(:)**2)
!    
  END IF
  !
  IF (LSURF_BUDGET) THEN
    XRN    (:, KPATCH)    = XP_RN         (:)
    XH     (:, KPATCH)    = XP_H          (:)
    XGFLUX (:, KPATCH)    = XP_GFLUX      (:)
    XLEI   (:, KPATCH)    = XP_LEI        (:)
    XSWD   (:, KPATCH)    = XP_SWD        (:)
    XSWU   (:, KPATCH)    = XP_SWU        (:)
    XLWD   (:, KPATCH)    = XP_LWD        (:)
    XLWU   (:, KPATCH)    = XP_LWU        (:)
    XFMU   (:, KPATCH)    = XP_FMU        (:)
    XFMV   (:, KPATCH)    = XP_FMV        (:)
    !
    XSWBD   (:, :, KPATCH) = XP_SWBD  (:,:)
    XSWBU   (:, :, KPATCH) = XP_SWBU  (:,:)
    !
  END IF
  !
  IF (LCOEF) THEN
    XCD            (:, KPATCH)    = XP_CD             (:)
    XCH            (:, KPATCH)    = XP_CH             (:)
    XCE            (:, KPATCH)    = XP_CE             (:)
    XZ0_WITH_SNOW  (:, KPATCH)    = XP_Z0_WITH_SNOW   (:)
    XZ0H_WITH_SNOW (:, KPATCH)    = XP_Z0H_WITH_SNOW  (:)
    XZ0EFF         (:, KPATCH)    = XP_Z0EFF          (:)
  END IF
  !
  IF (LSURF_VARS) THEN
    XQS            (:, KPATCH)    = XP_QS             (:)
  END IF
  !
  IF (LTRIP) THEN
    PCPL_DRAIN     (:, KPATCH)    = XP_DRAIN         (:)
    PCPL_RUNOFF    (:, KPATCH)    = XP_RUNOFF        (:)
  END IF
  !
  IF (LFLOOD) THEN
    PCPL_EFLOOD    (:, KPATCH)    = XP_LE_FLOOD(:) / XP_LVTT(:) + XP_LEI_FLOOD(:) / XP_LSTT(:)
    PCPL_PFLOOD    (:, KPATCH)    = XP_PFLOOD        (:)
    PCPL_IFLOOD    (:, KPATCH)    = XP_IFLOOD        (:)
  END IF    
  !
  IF(LGLACIER)THEN
    PCPL_ICEFLUX   (:, KPATCH)    = XP_ICEFLUX       (:)
  ENDIF
  !
  IF(TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO')THEN
    TSNOW%TEMP(:,:,KPATCH) = XP_SNOWTEMP(:,:)
    TSNOW%TS  (:,KPATCH)   = XP_SNOWTEMP(:,1)
  ENDIF
  !
  IF (CPHOTO/='NON') THEN
    XIACAN(:,:,KPATCH) = XP_IACAN(:,:)
  ENDIF
  !
ELSE
  !
  DO JJ=1,KSIZE
     JI                      = KMASK     (JJ)
     XTS    (JI, KPATCH)     = XP_TS    (JJ)  
     XTSRAD (JI, KPATCH)     = XP_TSRAD    (JJ)  
  END DO
  IF (N2M>=1) THEN
    DO JJ=1,KSIZE
      JI                      = KMASK     (JJ)
      XT2M    (JI, KPATCH)    = XP_T2M    (JJ)
      XQ2M    (JI, KPATCH)    = XP_Q2M    (JJ)
      XHU2M   (JI, KPATCH)    = XP_HU2M   (JJ)
      XZON10M (JI, KPATCH)    = XP_ZON10M (JJ)
      XMER10M (JI, KPATCH)    = XP_MER10M (JJ)
      XRI     (JI, KPATCH)    = XP_RI     (JJ)
      !     
      XWIND10M(JI, KPATCH)    = SQRT(XP_ZON10M(JJ)**2+XP_MER10M(JJ)**2)
      !      
    END DO
  END IF
  !
  IF (LSURF_BUDGET) THEN
    DO JJ=1,KSIZE
      JI                     = KMASK         (JJ)
      XRN    (JI, KPATCH)    = XP_RN         (JJ)
      XH     (JI, KPATCH)    = XP_H          (JJ)
      XGFLUX (JI, KPATCH)    = XP_GFLUX      (JJ)
      XLEI   (JI, KPATCH)    = XP_LEI        (JJ)
      XSWD   (JI, KPATCH)    = XP_SWD        (JJ)
      XSWU   (JI, KPATCH)    = XP_SWU        (JJ)
      XLWD   (JI, KPATCH)    = XP_LWD        (JJ)
      XLWU   (JI, KPATCH)    = XP_LWU        (JJ)
      XFMU   (JI, KPATCH)    = XP_FMU        (JJ)
      XFMV   (JI, KPATCH)    = XP_FMV        (JJ)
      !
      DO JSW=1,SIZE(XSWBD,2)
        XSWBD   (JI, JSW, KPATCH) = XP_SWBD  (JJ,JSW)
        XSWBU   (JI, JSW, KPATCH) = XP_SWBU  (JJ,JSW)
      END DO
      !
    END DO
  END IF
  !
  IF (LCOEF) THEN
    DO JJ=1,KSIZE
      JI                               = KMASK             (JJ)
      XCD              (JI, KPATCH)    = XP_CD             (JJ)
      XCH              (JI, KPATCH)    = XP_CH             (JJ)
      XCE              (JI, KPATCH)    = XP_CE             (JJ)
      XZ0_WITH_SNOW    (JI, KPATCH)    = XP_Z0_WITH_SNOW   (JJ)
      XZ0H_WITH_SNOW   (JI, KPATCH)    = XP_Z0H_WITH_SNOW  (JJ)
      XZ0EFF           (JI, KPATCH)    = XP_Z0EFF          (JJ)
    END DO
  END IF
  !
  IF (LSURF_VARS) THEN
    DO JJ=1,KSIZE
      JI                               = KMASK             (JJ)
      XQS              (JI, KPATCH)    = XP_QS             (JJ)
    END DO
  END IF
  !
  IF (LTRIP) THEN
    DO JJ=1,KSIZE
      JI                               = KMASK             (JJ)
      PCPL_DRAIN       (JI, KPATCH)    = XP_DRAIN          (JJ)
      PCPL_RUNOFF      (JI, KPATCH)    = XP_RUNOFF         (JJ)
    END DO
  END IF
  !
  IF (LFLOOD) THEN
    DO JJ=1,KSIZE
      JI                               = KMASK             (JJ)
      PCPL_EFLOOD      (JI, KPATCH)    = XP_LE_FLOOD(JJ) / XP_LVTT(JJ) + XP_LEI_FLOOD(JJ) / XP_LSTT(JJ)
      PCPL_PFLOOD      (JI, KPATCH)    = XP_PFLOOD         (JJ)
      PCPL_IFLOOD      (JI, KPATCH)    = XP_IFLOOD         (JJ)
    END DO
  END IF
  !
  IF(LGLACIER)THEN
    DO JJ=1,KSIZE
      JI                              = KMASK             (JJ)
      PCPL_ICEFLUX    (JI, KPATCH)    = XP_ICEFLUX        (JJ)
    END DO          
  ENDIF
  !
  IF(TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO')THEN
    DO JJ=1,KSIZE
      JI                       = KMASK             (JJ)
      TSNOW%TS    (JI,KPATCH)  = XP_SNOWTEMP(JJ,1)
      DO JSW=1,SIZE(TSNOW%TEMP,2)
        TSNOW%TEMP(JI,JSW,KPATCH)  = XP_SNOWTEMP(JJ,JSW)
      ENDDO
    ENDDO          
  ENDIF
  !  
  IF (CPHOTO/='NON') THEN
    DO JJ=1,KSIZE
      JI                  = KMASK   (JJ)
      DO JSW=1,SIZE(XIACAN,2)
         XIACAN(JI,JSW,KPATCH) = XP_IACAN(JJ,JSW)
      ENDDO
    ENDDO
  ENDIF
  !
ENDIF
!
!------------------------------------------------------------------------
!
XP_CH           => NULL()
XP_CE           => NULL()
XP_CD           => NULL()
XP_CDN          => NULL()
XP_RI           => NULL()
XP_HU           => NULL()
XP_HUG          => NULL()
XP_ALBT         => NULL()
XP_RN           => NULL()
XP_H            => NULL()
XP_LEI          => NULL()
XP_LEG          => NULL()
XP_LEGI         => NULL()
XP_LEV          => NULL()
XP_LES          => NULL()
XP_LER          => NULL()
XP_LETR         => NULL()
XP_GFLUX        => NULL()
XP_EVAP         => NULL()
XP_RESTORE      => NULL()
XP_DRAIN        => NULL()
XP_RUNOFF       => NULL()
XP_MELT         => NULL()
XP_MELTADV      => NULL()
XP_SRSFC        => NULL()
XP_RRSFC        => NULL()
XP_SNOWFREE_ALB => NULL()
!
XP_HORT         => NULL()
XP_DRIP         => NULL()
XP_RRVEG        => NULL()
XP_IRRIG_FLUX   => NULL()
!
XP_SWBD         => NULL()
XP_SWBU         => NULL()
!
XP_SWD          => NULL()
XP_SWU          => NULL()
XP_LWD          => NULL()
XP_LWU          => NULL()
XP_FMU          => NULL()
XP_FMV          => NULL()
!
XP_Z0_WITH_SNOW => NULL()
XP_Z0H_WITH_SNOW=> NULL()
XP_Z0EFF        => NULL()
!
XP_CG           => NULL()
XP_C1           => NULL()
XP_C2           => NULL()
XP_WGEQ         => NULL()
XP_CT           => NULL()
XP_RS           => NULL()
XP_HV           => NULL()
XP_QS           => NULL()
!
XP_TS           => NULL()
XP_TSRAD        => NULL()
!
XP_RESP_AUTO    => NULL()
XP_RESP_ECO     => NULL()
XP_GPP          => NULL()
XP_FAPAR        => NULL()
XP_FAPIR        => NULL()
XP_FAPAR_BS     => NULL()
XP_FAPIR_BS     => NULL()
!
XP_IFLOOD       => NULL()
XP_PFLOOD       => NULL()
XP_LE_FLOOD     => NULL()
XP_LEI_FLOOD    => NULL()
!
XP_RNSNOW       => NULL()
XP_HSNOW        => NULL()
XP_HPSNOW       => NULL()
XP_SMELTFLUX    => NULL()
XP_GFLUXSNOW    => NULL()
XP_USTARSNOW    => NULL()
XP_GRNDFLUX     => NULL()
XP_LESL         => NULL()
XP_CDSNOW       => NULL()
XP_CHSNOW       => NULL()
XP_SNOWHMASS    => NULL()
XP_RN_ISBA      => NULL()
XP_H_ISBA       => NULL()
XP_LEG_ISBA     => NULL()
XP_LEGI_ISBA    => NULL()
XP_LEV_ISBA     => NULL()
XP_LETR_ISBA    => NULL()
XP_USTAR_ISBA   => NULL()
XP_LER_ISBA     => NULL()
XP_LE_ISBA      => NULL()
XP_LEI_ISBA     => NULL()
XP_GFLUX_ISBA   => NULL()
XP_SNOWLIQ      => NULL()
XP_SNOWDZ       => NULL()
!
XP_SNOWTEMP     => NULL()
!
XP_SNOWFREE_ALB_VEG=> NULL()
XP_SNOWFREE_ALB_SOIL=> NULL()
!
XP_IACAN        => NULL()
!
XP_T2M          => NULL()
XP_Q2M          => NULL()
XP_HU2M         => NULL()
XP_ZON10M       => NULL()
XP_MER10M       => NULL()
!
XP_SWI          => NULL()
XP_TSWI         => NULL()
XP_TWSNOW       => NULL()
XP_TDSNOW       => NULL()
!
XP_ICEFLUX      => NULL()
!
XP_DWG          => NULL()
XP_DWGI         => NULL()
XP_DSWE         => NULL()
XP_WATBUD       => NULL()
!
DEALLOCATE(XBLOCK_SIMPLE)
DEALLOCATE(XBLOCK_GROUND)
DEALLOCATE(XBLOCK_SNOW)
DEALLOCATE(XBLOCK_KSW)
DEALLOCATE(XBLOCK_ABC)
DEALLOCATE(XBLOCK_0)
DEALLOCATE(XBLOCK_00)
!
IF (LHOOK) CALL DR_HOOK('UNPACK_DIAG_PATCH_N',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------
!
END SUBROUTINE UNPACK_DIAG_PATCH_n
