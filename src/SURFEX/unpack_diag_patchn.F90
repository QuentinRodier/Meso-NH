!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE UNPACK_DIAG_PATCH_n (DGI, GB, I, PKDI, PKI, &
                                KMASK,KSIZE,KNPATCH,KPATCH,    &
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
!!      Modified    06/2013 by B. Decharme : add lateral drainage flux diag for DIF
!!                                           add tiotale sublimation flux
!!      Modified    10/2014 by P. Samuelsson: MEB
!!
!!------------------------------------------------------------------
!
!
!
!
!
!
USE MODD_DIAG_ISBA_n, ONLY : DIAG_ISBA_t
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_PACK_DIAG_ISBA, ONLY : PACK_DIAG_ISBA_t
USE MODD_PACK_ISBA, ONLY : PACK_ISBA_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(DIAG_ISBA_t), INTENT(INOUT) :: DGI
TYPE(GR_BIOG_t), INTENT(INOUT) :: GB
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(PACK_DIAG_ISBA_t), INTENT(INOUT) :: PKDI
TYPE(PACK_ISBA_t), INTENT(INOUT) :: PKI
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
  DGI%XTS(:, KPATCH) = PKDI%XP_TS(:)
  DGI%XTSRAD(:, KPATCH) = PKDI%XP_TSRAD(:)
  IF (DGI%N2M>=1) THEN
    DGI%XT2M    (:, KPATCH)    = PKDI%XP_T2M    (:)
    DGI%XQ2M    (:, KPATCH)    = PKDI%XP_Q2M    (:)
    DGI%XHU2M   (:, KPATCH)    = PKDI%XP_HU2M   (:)
    DGI%XZON10M (:, KPATCH)    = PKDI%XP_ZON10M (:)
    DGI%XMER10M (:, KPATCH)    = PKDI%XP_MER10M (:)
    DGI%XRI     (:, KPATCH)    = PKDI%XP_RI     (:)
!    
    DGI%XWIND10M(:, KPATCH)  = SQRT(PKDI%XP_ZON10M(:)**2+PKDI%XP_MER10M(:)**2)
!    
  END IF
  !
  IF (DGI%LSURF_BUDGET) THEN
    DGI%XRN    (:, KPATCH)    = PKDI%XP_RN         (:)
    DGI%XH     (:, KPATCH)    = PKDI%XP_H          (:)
    DGI%XGFLUX (:, KPATCH)    = PKDI%XP_GFLUX      (:)
    DGI%XLEI   (:, KPATCH)    = PKDI%XP_LEI        (:)
    DGI%XSWD   (:, KPATCH)    = PKDI%XP_SWD        (:)
    DGI%XSWU   (:, KPATCH)    = PKDI%XP_SWU        (:)
    DGI%XLWD   (:, KPATCH)    = PKDI%XP_LWD        (:)
    DGI%XLWU   (:, KPATCH)    = PKDI%XP_LWU        (:)
    DGI%XFMU   (:, KPATCH)    = PKDI%XP_FMU        (:)
    DGI%XFMV   (:, KPATCH)    = PKDI%XP_FMV        (:)
    !
    DGI%XSWBD   (:, :, KPATCH) = PKDI%XP_SWBD  (:,:)
    DGI%XSWBU   (:, :, KPATCH) = PKDI%XP_SWBU  (:,:)
    !
  END IF
  !
  IF (DGI%LCOEF) THEN
    DGI%XCD            (:, KPATCH)    = PKDI%XP_CD             (:)
    DGI%XCH            (:, KPATCH)    = PKDI%XP_CH             (:)
    DGI%XCE            (:, KPATCH)    = PKDI%XP_CE             (:)
    DGI%XZ0_WITH_SNOW  (:, KPATCH)    = PKDI%XP_Z0_WITH_SNOW   (:)
    DGI%XZ0H_WITH_SNOW (:, KPATCH)    = PKDI%XP_Z0H_WITH_SNOW  (:)
    DGI%XZ0EFF         (:, KPATCH)    = PKDI%XP_Z0EFF          (:)
  END IF
  !
  IF (DGI%LSURF_VARS) THEN
    DGI%XQS            (:, KPATCH)    = PKDI%XP_QS             (:)
  END IF
  !
  IF (I%LCPL_RRM) THEN
    PCPL_DRAIN     (:, KPATCH)    = PKDI%XP_DRAIN         (:)
    PCPL_RUNOFF    (:, KPATCH)    = PKDI%XP_RUNOFF        (:)
  END IF
  !
  IF (I%LFLOOD) THEN
    PCPL_EFLOOD    (:, KPATCH)    = PKDI%XP_LE_FLOOD (:) / PKI%XP_LVTT(:) &
                                  + PKDI%XP_LEI_FLOOD(:) / PKI%XP_LSTT(:)
    PCPL_PFLOOD    (:, KPATCH)    = PKDI%XP_PFLOOD                (:)
    PCPL_IFLOOD    (:, KPATCH)    = PKDI%XP_IFLOOD                (:)
  END IF    
  !
  IF(I%LCPL_RRM.AND.I%LGLACIER)THEN
    PCPL_ICEFLUX   (:, KPATCH)    = PKDI%XP_ICEFLUX       (:)
  ENDIF
  !
  IF(I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO')THEN
    I%TSNOW%TEMP(:,:,KPATCH) = PKDI%XP_SNOWTEMP(:,:)
    I%TSNOW%TS  (:,KPATCH)   = PKDI%XP_SNOWTEMP(:,1)
  ENDIF
  !
  IF (I%CPHOTO/='NON') THEN
    GB%XIACAN(:,:,KPATCH) = PKDI%XP_IACAN(:,:)
  ENDIF
  !
ELSE
  !
  DO JJ=1,KSIZE
     JI                      = KMASK     (JJ)
     DGI%XTS    (JI, KPATCH)     = PKDI%XP_TS    (JJ)  
     DGI%XTSRAD (JI, KPATCH)     = PKDI%XP_TSRAD    (JJ)  
  END DO
  IF (DGI%N2M>=1) THEN
    DO JJ=1,KSIZE
      JI                      = KMASK     (JJ)
      DGI%XT2M    (JI, KPATCH)    = PKDI%XP_T2M    (JJ)
      DGI%XQ2M    (JI, KPATCH)    = PKDI%XP_Q2M    (JJ)
      DGI%XHU2M   (JI, KPATCH)    = PKDI%XP_HU2M   (JJ)
      DGI%XZON10M (JI, KPATCH)    = PKDI%XP_ZON10M (JJ)
      DGI%XMER10M (JI, KPATCH)    = PKDI%XP_MER10M (JJ)
      DGI%XRI     (JI, KPATCH)    = PKDI%XP_RI     (JJ)
      !     
      DGI%XWIND10M(JI, KPATCH)  = SQRT(PKDI%XP_ZON10M(JJ)**2+PKDI%XP_MER10M(JJ)**2)
      !      
    END DO
  END IF
  !
  IF (DGI%LSURF_BUDGET) THEN
    DO JJ=1,KSIZE
      JI                     = KMASK         (JJ)
      DGI%XRN    (JI, KPATCH)    = PKDI%XP_RN         (JJ)
      DGI%XH     (JI, KPATCH)    = PKDI%XP_H          (JJ)
      DGI%XGFLUX (JI, KPATCH)    = PKDI%XP_GFLUX      (JJ)
      DGI%XLEI   (JI, KPATCH)    = PKDI%XP_LEI        (JJ)
      DGI%XSWD   (JI, KPATCH)    = PKDI%XP_SWD        (JJ)
      DGI%XSWU   (JI, KPATCH)    = PKDI%XP_SWU        (JJ)
      DGI%XLWD   (JI, KPATCH)    = PKDI%XP_LWD        (JJ)
      DGI%XLWU   (JI, KPATCH)    = PKDI%XP_LWU        (JJ)
      DGI%XFMU   (JI, KPATCH)    = PKDI%XP_FMU        (JJ)
      DGI%XFMV   (JI, KPATCH)    = PKDI%XP_FMV        (JJ)
      !
      DO JSW=1,SIZE(DGI%XSWBD,2)
        DGI%XSWBD   (JI, JSW, KPATCH) = PKDI%XP_SWBD  (JJ,JSW)
        DGI%XSWBU   (JI, JSW, KPATCH) = PKDI%XP_SWBU  (JJ,JSW)
      END DO
      !
    END DO
  END IF
  !
  IF (DGI%LCOEF) THEN
    DO JJ=1,KSIZE
      JI                               = KMASK             (JJ)
      DGI%XCD              (JI, KPATCH)    = PKDI%XP_CD             (JJ)
      DGI%XCH              (JI, KPATCH)    = PKDI%XP_CH             (JJ)
      DGI%XCE              (JI, KPATCH)    = PKDI%XP_CE             (JJ)
      DGI%XZ0_WITH_SNOW    (JI, KPATCH)    = PKDI%XP_Z0_WITH_SNOW   (JJ)
      DGI%XZ0H_WITH_SNOW   (JI, KPATCH)    = PKDI%XP_Z0H_WITH_SNOW  (JJ)
      DGI%XZ0EFF           (JI, KPATCH)    = PKDI%XP_Z0EFF          (JJ)
    END DO
  END IF
  !
  IF (DGI%LSURF_VARS) THEN
    DO JJ=1,KSIZE
      JI                               = KMASK             (JJ)
      DGI%XQS              (JI, KPATCH)    = PKDI%XP_QS             (JJ)
    END DO
  END IF
  !
  IF (I%LCPL_RRM) THEN
    DO JJ=1,KSIZE
      JI                               = KMASK             (JJ)
      PCPL_DRAIN       (JI, KPATCH)    = PKDI%XP_DRAIN          (JJ)
      PCPL_RUNOFF      (JI, KPATCH)    = PKDI%XP_RUNOFF         (JJ)
    END DO
  END IF
  !
  IF (I%LFLOOD) THEN
    DO JJ=1,KSIZE
      JI                               = KMASK                     (JJ)
      PCPL_EFLOOD      (JI, KPATCH)    = PKDI%XP_LE_FLOOD (JJ) / PKI%XP_LVTT(JJ) &
                                       + PKDI%XP_LEI_FLOOD(JJ) / PKI%XP_LSTT(JJ)
      PCPL_PFLOOD      (JI, KPATCH)    = PKDI%XP_PFLOOD                 (JJ)
      PCPL_IFLOOD      (JI, KPATCH)    = PKDI%XP_IFLOOD                 (JJ)
    END DO
  END IF
  !
  IF(I%LGLACIER)THEN
    DO JJ=1,KSIZE
      JI                              = KMASK             (JJ)
      PCPL_ICEFLUX    (JI, KPATCH)    = PKDI%XP_ICEFLUX        (JJ)
    END DO          
  ENDIF
  !
  IF(I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO')THEN
    DO JJ=1,KSIZE
      JI                       = KMASK             (JJ)
      I%TSNOW%TS    (JI,KPATCH)  = PKDI%XP_SNOWTEMP(JJ,1)
      DO JSW=1,SIZE(I%TSNOW%TEMP,2)
        I%TSNOW%TEMP(JI,JSW,KPATCH)  = PKDI%XP_SNOWTEMP(JJ,JSW)
      ENDDO
    ENDDO          
  ENDIF
  !  
  IF (I%CPHOTO/='NON') THEN
    DO JJ=1,KSIZE
      JI                  = KMASK   (JJ)
      DO JSW=1,SIZE(GB%XIACAN,2)
         GB%XIACAN(JI,JSW,KPATCH) = PKDI%XP_IACAN(JJ,JSW)
      ENDDO
    ENDDO
  ENDIF
  !
ENDIF
!
!------------------------------------------------------------------------
!
PKDI%XP_CH           => NULL()
PKDI%XP_CE           => NULL()
PKDI%XP_CD           => NULL()
PKDI%XP_CDN          => NULL()
PKDI%XP_RI           => NULL()
PKDI%XP_HU           => NULL()
PKDI%XP_HUG          => NULL()
PKDI%XP_ALBT         => NULL()
PKDI%XP_RN           => NULL()
PKDI%XP_H            => NULL()
PKDI%XP_LEI          => NULL()
PKDI%XP_LEG          => NULL()
PKDI%XP_LEGI         => NULL()
PKDI%XP_LEV          => NULL()
PKDI%XP_LES          => NULL()
PKDI%XP_LER          => NULL()
PKDI%XP_LETR         => NULL()
PKDI%XP_GFLUX        => NULL()
PKDI%XP_EVAP         => NULL()
PKDI%XP_SUBL         => NULL()
PKDI%XP_RESTORE      => NULL()
PKDI%XP_DRAIN        => NULL()
PKDI%XP_QSB          => NULL()
PKDI%XP_RUNOFF       => NULL()
PKDI%XP_MELT         => NULL()
PKDI%XP_MELTADV      => NULL()
PKDI%XP_SRSFC        => NULL()
PKDI%XP_RRSFC        => NULL()
PKDI%XP_SNOWFREE_ALB => NULL()
!
PKDI%XP_HORT         => NULL()
PKDI%XP_DRIP         => NULL()
PKDI%XP_RRVEG        => NULL()
PKDI%XP_IRRIG_FLUX   => NULL()
!
PKDI%XP_SWBD         => NULL()
PKDI%XP_SWBU         => NULL()
!
PKDI%XP_SWD          => NULL()
PKDI%XP_SWU          => NULL()
PKDI%XP_LWD          => NULL()
PKDI%XP_LWU          => NULL()
PKDI%XP_FMU          => NULL()
PKDI%XP_FMV          => NULL()
!
PKDI%XP_Z0_WITH_SNOW => NULL()
PKDI%XP_Z0H_WITH_SNOW=> NULL()
PKDI%XP_Z0EFF        => NULL()
!
PKDI%XP_CG           => NULL()
PKDI%XP_C1           => NULL()
PKDI%XP_C2           => NULL()
PKDI%XP_WGEQ         => NULL()
PKDI%XP_CT           => NULL()
PKDI%XP_RS           => NULL()
PKDI%XP_HV           => NULL()
PKDI%XP_QS           => NULL()
!
PKDI%XP_TS           => NULL()
PKDI%XP_TSRAD        => NULL()
!
PKDI%XP_RESP_AUTO    => NULL()
PKDI%XP_RESP_ECO     => NULL()
PKDI%XP_GPP          => NULL()
PKDI%XP_FAPAR        => NULL()
PKDI%XP_FAPIR        => NULL()
PKDI%XP_FAPAR_BS     => NULL()
PKDI%XP_FAPIR_BS     => NULL()
!
PKDI%XP_IFLOOD       => NULL()
PKDI%XP_PFLOOD       => NULL()
PKDI%XP_LE_FLOOD     => NULL()
PKDI%XP_LEI_FLOOD    => NULL()
!
PKDI%XP_RNSNOW       => NULL()
PKDI%XP_HSNOW        => NULL()
PKDI%XP_HPSNOW       => NULL()
PKDI%XP_GFLUXSNOW    => NULL()
PKDI%XP_USTARSNOW    => NULL()
PKDI%XP_GRNDFLUX     => NULL()
PKDI%XP_LESL         => NULL()
PKDI%XP_SNDRIFT      => NULL()
PKDI%XP_CDSNOW       => NULL()
PKDI%XP_CHSNOW       => NULL()
PKDI%XP_SNOWHMASS    => NULL()
PKDI%XP_RN_ISBA      => NULL()
PKDI%XP_H_ISBA       => NULL()
PKDI%XP_LEG_ISBA     => NULL()
PKDI%XP_LEGI_ISBA    => NULL()
PKDI%XP_LEV_ISBA     => NULL()
PKDI%XP_LETR_ISBA    => NULL()
PKDI%XP_USTAR_ISBA   => NULL()
PKDI%XP_LER_ISBA     => NULL()
PKDI%XP_LE_ISBA      => NULL()
PKDI%XP_LEI_ISBA     => NULL()
PKDI%XP_GFLUX_ISBA   => NULL()
PKDI%XP_SNOWLIQ      => NULL()
PKDI%XP_SNOWDZ       => NULL()
!
PKDI%XP_SNOWTEMP     => NULL()
!
PKDI%XP_SNOWFREE_ALB_VEG=> NULL()
PKDI%XP_SNOWFREE_ALB_SOIL=> NULL()
!
PKDI%XP_IACAN        => NULL()
!
PKDI%XP_T2M          => NULL()
PKDI%XP_Q2M          => NULL()
PKDI%XP_HU2M         => NULL()
PKDI%XP_ZON10M       => NULL()
PKDI%XP_MER10M       => NULL()
!
PKDI%XP_SWI          => NULL()
PKDI%XP_TSWI         => NULL()
PKDI%XP_TWSNOW       => NULL()
PKDI%XP_TDSNOW       => NULL()
!
PKDI%XP_ICEFLUX      => NULL()
!
PKDI%XP_DWG          => NULL()
PKDI%XP_DWGI         => NULL()
PKDI%XP_DSWE         => NULL()
PKDI%XP_WATBUD       => NULL()
!
PKDI%XP_SWUP       => NULL()
! MEB stuff
PKDI%XP_SWNET_V       => NULL()
PKDI%XP_SWNET_G       => NULL()
PKDI%XP_SWNET_N       => NULL()
PKDI%XP_SWNET_NS       => NULL()
PKDI%XP_LWUP       => NULL()
PKDI%XP_LWNET_V       => NULL()
PKDI%XP_LWNET_G       => NULL()
PKDI%XP_LWNET_N       => NULL()
PKDI%XP_LEVCV       => NULL()
PKDI%XP_LESC       => NULL()
PKDI%XP_H_V_C       => NULL()
PKDI%XP_H_G_C       => NULL()
PKDI%XP_LETRGV       => NULL()
PKDI%XP_LETRCV       => NULL()
PKDI%XP_LERGV       => NULL()
PKDI%XP_LELITTER     => NULL()
PKDI%XP_LELITTERI    => NULL()
PKDI%XP_DRIPLIT      => NULL()
PKDI%XP_RRLIT       => NULL()
PKDI%XP_LERCV       => NULL()
PKDI%XP_H_C_A       => NULL()
PKDI%XP_H_N_C       => NULL()
PKDI%XP_LE_C_A       => NULL()
PKDI%XP_LE_V_C       => NULL()
PKDI%XP_LE_G_C       => NULL()
PKDI%XP_LE_N_C       => NULL()
PKDI%XP_EVAP_N_C       => NULL()
PKDI%XP_EVAP_G_C       => NULL()
PKDI%XP_SR_GN       => NULL()
PKDI%XP_MELTCV       => NULL()
PKDI%XP_FRZCV       => NULL()
PKDI%XP_SWDOWN_GN       => NULL()
PKDI%XP_LWDOWN_GN       => NULL()
!
DEALLOCATE(PKDI%XBLOCK_SIMPLE)
DEALLOCATE(PKDI%XBLOCK_GROUND)
DEALLOCATE(PKDI%XBLOCK_SNOW)
DEALLOCATE(PKDI%XBLOCK_KSW)
DEALLOCATE(PKDI%XBLOCK_ABC)
DEALLOCATE(PKDI%XBLOCK_0)
DEALLOCATE(PKDI%XBLOCK_00)
!
IF (LHOOK) CALL DR_HOOK('UNPACK_DIAG_PATCH_N',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------
!
END SUBROUTINE UNPACK_DIAG_PATCH_n
