!     #########
SUBROUTINE UNPACK_ISBA_PATCH_n(KMASK,KSIZE,KPATCH)
!##############################################
!
!!****  *UNPACK_ISBA_PATCH_n* - unpacks ISBA prognostic variables
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
!!     A. Boone
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      B. Decharme    2008 Floodplains
!!      A.L. Gibelin 04/2009 : BIOMASS and RESP_BIOMASS arrays 
!!      A.L. Gibelin 04/2009 : TAU_WOOD for NCB option 
!!      A.L. Gibelin 05/2009 : Add carbon spinup
!!      A.L. Gibelin 06/2009 : Soil carbon variables for CNT option
!!      A.L. Gibelin 07/2009 : Suppress RDK and transform GPP as a diagnostic
!!      A.L. Gibelin 07/2009 : Suppress PPST and PPSTF as outputs
!!
!!------------------------------------------------------------------
!
USE MODD_PACK_ISBA, ONLY :   LBLOCK_SIMPLE, LBLOCK_0, TBLOCK_SIMPLE, TBLOCK_0, XBLOCK_SIMPLE,  &
                             XBLOCK_GROUND, XBLOCK_VEGTYPE, XBLOCK_TG, XBLOCK_SNOW, XBLOCK_ALB,&
                             XBLOCK_2, XBLOCK_BIOMASS, XBLOCK_SOILCARB, XBLOCK_LITTLEVS,       &
                             XBLOCK_LITTER, XBLOCK_0, XBLOCK_00, XBLOCK_000, XBLOCK_01,        &
                             NBLOCK_SIMPLE, NBLOCK_0,                                          &
                             XP_Z0_O_Z0H, XP_EMIS, XP_Z0,                                      &
                             XP_WRMAX_CF, XP_GAMMA, XP_ALBNIR, XP_ALBVIS, XP_ALBUV,            &
                             XP_CV, XP_RGL, XP_VEGTYPE_PATCH, XP_DG, XP_RUNOFFD, XP_RUNOFFB,   &
                             XP_WDRAIN, XP_TAUICE, XP_Z0REL, XP_GAMMAT, NK_WG_LAYER,           &
                             XP_C1SAT, XP_C2REF, XP_C3, XP_C4B, XP_C4REF, XP_ACOEF, XP_PCOEF,  &
                             XP_WFC, XP_WWILT, XP_WSAT, XP_BCOEF, XP_WR, XP_TG, XP_WG,         &
                             XP_WGI, XP_LAI, XP_RESA, XP_VEG, XP_TDEEP, XP_ROOTFRAC, XP_DZG,   &
                             XP_DZDIF, XP_CONDSAT, XP_MPOTSAT, XP_CGSAT, XP_HCAPSOIL,          &
                             XP_CONDDRY, XP_CONDSLD, XP_RSMIN, XP_BSLAI, XP_LAIMIN,            &
                             XP_SEFOLD, XP_H_TREE, XP_ANF, XP_ANMAX, XP_FZERO, XP_EPSO,        &
                             XP_GAMM, XP_QDGAMM, XP_GMES, XP_RE25, XP_QDGMES, XP_T1GMES,       &
                             XP_T2GMES, XP_TAU_WOOD, XP_SOILWGHT,                              &
                             XP_FAPARC, XP_FAPIRC, XP_LAI_EFFC, XP_MUS,                        &
                             XP_AMAX, XP_QDAMAX, XP_T1AMAX, XP_T2AMAX, XP_AN, XP_ANFM,         &
                             XP_LE, LP_STRESS, XP_F2I, XP_GC, XP_AH, XP_BH, XP_DMAX,           &
                             XP_ANDAY, XP_Z0EFFIP,XP_Z0EFFIM,XP_Z0EFFJP,XP_Z0EFFJM,            &
                             XP_AOSIP,XP_AOSIM,XP_AOSJP,XP_AOSJM,                              &
                             XP_HO2IP,XP_HO2IM,XP_HO2JP,XP_HO2JM,XP_SSO_SLOPE,                 &
                             XP_SNOWSWE, XP_SNOWRHO, XP_SNOWHEAT, XP_SNOWEMIS, XP_SNOWALB,     &
                             XP_SNOWGRAN1, XP_SNOWGRAN2,  XP_SNOWHIST, XP_SNOWAGE,             &
                             XP_ALBNIR_VEG, XP_ALBVIS_VEG, XP_ALBUV_VEG,                       &
                             XP_ALBNIR_DRY, XP_ALBVIS_DRY, XP_ALBUV_DRY,                       &
                             XP_ALBNIR_WET, XP_ALBVIS_WET, XP_ALBUV_WET,                       &
                             XP_ALBNIR_SOIL, XP_ALBVIS_SOIL, XP_ALBUV_SOIL,                    &
                             XP_CLAY, XP_SAND, XP_LAT, XP_LON,                                 &
                             XP_BIOMASS, XP_RESP_BIOMASS,                                      &
                             XP_LITTER, XP_SOILCARB, XP_LIGNIN_STRUC,                          &
                             XP_CE_NITRO, XP_CF_NITRO, XP_CNA_NITRO, XP_BSLAI_NITRO,           &
                             TP_SEED,TP_REAP,XP_IRRIG,XP_WATSUP,XP_LIRRIDAY,XP_THRESHOLD,      &
                             XP_LIRRIGATE, XP_D_ICE,XP_KSAT_ICE,XP_MUF,XP_FSAT,                &
                             XP_FFLOOD, XP_Z0FLOOD, XP_PIFLOOD,  XP_INCREASE, XP_TURNOVER,     &
                             XP_PSN, XP_PSNG, XP_PSNV, XP_PSNV_A, XP_FF, XP_FFG, XP_FFV,       &
                             XP_CPS, XP_LVTT, XP_LSTT, XP_DIR_ALB_WITH_SNOW,                   &
                             XP_SCA_ALB_WITH_SNOW, XP_ALBF, XP_EMISF, XP_ICE_STO, XP_FFROZEN

USE MODD_AGRI,     ONLY :  LAGRIP
USE MODD_AGRI_n,   ONLY :  LIRRIDAY

USE MODD_ISBA_n,   ONLY : TSNOW, XWR, XTG, XWG, XWGI, XRESA, XLAI, XAN, XANFM,          &
                            XLE, XANDAY, CPHOTO, XALBNIR, XALBVIS, XALBUV,              &
                            XALBNIR_VEG, XALBVIS_VEG, XALBUV_VEG, LGLACIER, LTR_ML,     &
                            XZ0EFFIP, XZ0EFFIM, XZ0EFFJP, XZ0EFFJM, XLAI_EFFC, XMUS,    &
                            XVEG, XZ0, XEMIS, XALBNIR_SOIL, XALBVIS_SOIL, XALBUV_SOIL,  &
                            NPATCH, NNBIOMASS, NNLITTER, NNLITTLEVS, NNSOILCARB,        &
                            XBIOMASS, XRESP_BIOMASS, XINCREASE, XTURNOVER, XFAPARC,     &
                            CRESPSL, XLITTER, XSOILCARB, XLIGNIN_STRUC, XFAPIRC,        &
                            XCE_NITRO, XCF_NITRO, XCNA_NITRO, XBSLAI_NITRO,             &
                            LFLOOD, XZ0_FLOOD, XPCPS, XPLVTT, XPLSTT, XICE_STO
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)               :: KSIZE, KPATCH
!
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
!
INTEGER JJ, JI, JK, JL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('UNPACK_ISBA_PATCH_N',0,ZHOOK_HANDLE)
IF (NPATCH==1) THEN
  TSNOW%WSNOW     (:, :, 1) = XP_SNOWSWE    (:, :)
  TSNOW%RHO       (:, :, 1) = XP_SNOWRHO    (:, :)
  TSNOW%ALB       (:, 1)    = XP_SNOWALB    (:)
  XWR             (:, 1)    = XP_WR         (:)
  XTG             (:, :, 1) = XP_TG         (:, :)
  XWG             (:, :, 1) = XP_WG         (:, :)
  XWGI            (:, :, 1) = XP_WGI        (:, :)
  XRESA           (:, 1)    = XP_RESA       (:) 
  XPCPS           (:, 1)    = XP_CPS        (:) 
  XPLVTT          (:, 1)    = XP_LVTT       (:) 
  XPLSTT          (:, 1)    = XP_LSTT       (:) 
  XALBNIR         (:, 1)    = XP_ALBNIR     (:) 
  XALBVIS         (:, 1)    = XP_ALBVIS     (:) 
  XALBUV          (:, 1)    = XP_ALBUV      (:) 
  XALBNIR_VEG     (:, 1)    = XP_ALBNIR_VEG (:) 
  XALBVIS_VEG     (:, 1)    = XP_ALBVIS_VEG (:) 
  XALBUV_VEG      (:, 1)    = XP_ALBUV_VEG  (:) 
  XALBNIR_SOIL    (:, 1)    = XP_ALBNIR_SOIL(:) 
  XALBVIS_SOIL    (:, 1)    = XP_ALBVIS_SOIL(:) 
  XALBUV_SOIL     (:, 1)    = XP_ALBUV_SOIL (:) 
  XEMIS           (:, 1)    = XP_EMIS       (:) 
  XLAI            (:, 1)    = XP_LAI        (:) 
  XVEG            (:, 1)    = XP_VEG        (:) 
  XZ0             (:, 1)    = XP_Z0         (:) 
  XZ0EFFIP        (:, 1)    = XP_Z0EFFIP    (:) 
  XZ0EFFIM        (:, 1)    = XP_Z0EFFIM    (:) 
  XZ0EFFJP        (:, 1)    = XP_Z0EFFJP    (:) 
  XZ0EFFJM        (:, 1)    = XP_Z0EFFJM    (:) 
  XLE             (:, 1)    = XP_LE         (:)
  !
  IF (LTR_ML) THEN
    XFAPARC         (:, 1)    = XP_FAPARC     (:)
    XFAPIRC         (:, 1)    = XP_FAPIRC     (:)
    XLAI_EFFC       (:, 1)    = XP_LAI_EFFC   (:)
    XMUS            (:, 1)    = XP_MUS        (:)
  ENDIF   
  !
  IF (CPHOTO/='NON') THEN
     XAN             (:, 1)    = XP_AN         (:)
     XANDAY          (:, 1)    = XP_ANDAY      (:)
     XANFM           (:, 1)    = XP_ANFM       (:)
     XBIOMASS        (:,:,1)   = XP_BIOMASS        (:,:)
     XRESP_BIOMASS   (:,:,1)   = XP_RESP_BIOMASS   (:,:)
  END IF
  !
  IF(CPHOTO=='NIT' .OR. CPHOTO=='NCB') THEN
     XBSLAI_NITRO    (:,1)    =    XP_BSLAI_NITRO    (:)          
  END IF
  !
    IF(CPHOTO=='NCB') THEN
     XINCREASE       (:,:,1)   =    XP_INCREASE       (:,:)
  END IF
  !
  IF(CRESPSL=='CNT') THEN
     XLITTER         (:,:,:,1) =    XP_LITTER         (:,:,:)
     XSOILCARB       (:,:,1)   =    XP_SOILCARB       (:,:)
     XLIGNIN_STRUC   (:,:,1)   =    XP_LIGNIN_STRUC   (:,:)
     XTURNOVER       (:,:,1)   =    XP_TURNOVER       (:,:)
  END IF
  !
  IF(LAGRIP .AND. (CPHOTO=='NIT' .OR. CPHOTO=='LAI' .OR. CPHOTO=='LST' .OR. CPHOTO=='NCB') ) THEN
    LIRRIDAY (:,1)  =    XP_LIRRIDAY (:)
  END IF
  !
  IF (TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') THEN
     TSNOW%HEAT      (:, :, 1) = XP_SNOWHEAT   (:, :)
     TSNOW%EMIS      (:, 1)    = XP_SNOWEMIS   (:)
  END IF

  IF (TSNOW%SCHEME=='CRO') THEN
     TSNOW%GRAN1     (:, :, 1) = XP_SNOWGRAN1   (:, :)
     TSNOW%GRAN2     (:, :, 1) = XP_SNOWGRAN2   (:, :)
     TSNOW%HIST      (:, :, 1) = XP_SNOWHIST    (:, :)
     TSNOW%AGE       (:, :, 1) = XP_SNOWAGE     (:, :)
  END IF
!
  IF(LFLOOD)THEN
     XZ0_FLOOD       (:,1)     = XP_Z0FLOOD    (:)
  END IF
  !
  IF(LGLACIER)THEN
     XICE_STO        (:,1)     = XP_ICE_STO    (:)
  ENDIF
!
ELSE
!
! Only save values for patches which are in use:
!
  DO JJ=1,KSIZE
    JI                              = KMASK         (JJ)
    TSNOW%ALB       (JI, KPATCH)    = XP_SNOWALB    (JJ)
    XWR             (JI, KPATCH)    = XP_WR         (JJ)
    XRESA           (JI, KPATCH)    = XP_RESA       (JJ) 
    XPCPS           (JI, KPATCH)    = XP_CPS        (JJ) 
    XPLVTT          (JI, KPATCH)    = XP_LVTT       (JJ) 
    XPLSTT          (JI, KPATCH)    = XP_LSTT       (JJ) 
    XALBNIR         (JI, KPATCH)    = XP_ALBNIR     (JJ) 
    XALBVIS         (JI, KPATCH)    = XP_ALBVIS     (JJ) 
    XALBUV          (JI, KPATCH)    = XP_ALBUV      (JJ) 
    XALBNIR_VEG     (JI, KPATCH)    = XP_ALBNIR_VEG (JJ) 
    XALBVIS_VEG     (JI, KPATCH)    = XP_ALBVIS_VEG (JJ) 
    XALBUV_VEG      (JI, KPATCH)    = XP_ALBUV_VEG  (JJ) 
    XALBNIR_SOIL    (JI, KPATCH)    = XP_ALBNIR_SOIL(JJ) 
    XALBVIS_SOIL    (JI, KPATCH)    = XP_ALBVIS_SOIL(JJ) 
    XALBUV_SOIL     (JI, KPATCH)    = XP_ALBUV_SOIL (JJ) 
    XEMIS           (JI, KPATCH)    = XP_EMIS       (JJ) 
    XLAI            (JI, KPATCH)    = XP_LAI        (JJ) 
    XVEG            (JI, KPATCH)    = XP_VEG        (JJ) 
    XZ0             (JI, KPATCH)    = XP_Z0         (JJ) 
    XZ0EFFIP        (JI, KPATCH)    = XP_Z0EFFIP    (JJ) 
    XZ0EFFIM        (JI, KPATCH)    = XP_Z0EFFIM    (JJ) 
    XZ0EFFJP        (JI, KPATCH)    = XP_Z0EFFJP    (JJ) 
    XZ0EFFJM        (JI, KPATCH)    = XP_Z0EFFJM    (JJ) 
    XLE             (JI, KPATCH)    = XP_LE         (JJ)
  !
  END DO
  DO JK=1,SIZE(XTG,2)
    DO JJ=1,KSIZE
      JI                      =    KMASK(JJ)
      XTG             (JI, JK, KPATCH) = XP_TG         (JJ, JK)
    ENDDO
  ENDDO
!  
  DO JK=1,SIZE(XWG,2)
    DO JJ=1,KSIZE
      JI                      =    KMASK(JJ)
      XWG             (JI, JK, KPATCH) = XP_WG         (JJ, JK)
      XWGI            (JI, JK, KPATCH) = XP_WGI        (JJ, JK)
    ENDDO
  ENDDO
!  
  DO JK=1,SIZE(XP_SNOWSWE,2)
    DO JJ=1,KSIZE
      JI                      =    KMASK(JJ)
      TSNOW%WSNOW     (JI, JK, KPATCH) = XP_SNOWSWE    (JJ, JK)
      TSNOW%RHO       (JI, JK, KPATCH) = XP_SNOWRHO    (JJ, JK)
    ENDDO
  ENDDO
  !
  IF (LTR_ML) THEN
    DO JJ=1,KSIZE
      JI                      =    KMASK(JJ)          
      XFAPARC         (JI, KPATCH)    = XP_FAPARC     (JJ)
      XFAPIRC         (JI, KPATCH)    = XP_FAPIRC     (JJ)
      XLAI_EFFC       (JI, KPATCH)    = XP_LAI_EFFC   (JJ)
      XMUS            (JI, KPATCH)    = XP_MUS        (JJ)
    ENDDO
  ENDIF  
  !
  IF (CPHOTO/='NON') THEN
    DO JJ=1,KSIZE
      JI                              = KMASK         (JJ)
      XAN             (JI, KPATCH)    = XP_AN         (JJ)
      XANDAY          (JI, KPATCH)    = XP_ANDAY      (JJ)
      XANFM           (JI, KPATCH)    = XP_ANFM       (JJ)
    ENDDO
    DO JK=1,SIZE(XBIOMASS,2)
      DO JJ=1,KSIZE
        JI                              = KMASK         (JJ)       
        XBIOMASS        (JI, JK, KPATCH) = XP_BIOMASS        (JJ, JK)
        XRESP_BIOMASS   (JI, JK, KPATCH) = XP_RESP_BIOMASS   (JJ, JK)
      ENDDO
    END DO
  END IF
  !
  IF (CPHOTO=='NIT' .OR. CPHOTO=='NCB') THEN
    DO JJ=1,KSIZE
      JI                                 = KMASK             (JJ)
      XBSLAI_NITRO    (JI, KPATCH)       = XP_BSLAI_NITRO    (JJ)
    END DO
  END IF
  !
  IF (CPHOTO=='NCB') THEN
    DO JK=1,SIZE(XINCREASE,2)
      DO JJ=1,KSIZE
        JI                                 = KMASK             (JJ)
        XINCREASE       (JI, JK, KPATCH)   = XP_INCREASE       (JJ, JK)
      ENDDO
    END DO
  END IF
  !
  IF (CRESPSL=='CNT') THEN
    DO JL=1,SIZE(XP_LITTER,3)
      DO JK=1,SIZE(XP_LITTER,2)
        DO JJ=1,KSIZE
          JI                                 = KMASK             (JJ)
          XLITTER       (JI, JK, JL, KPATCH) = XP_LITTER         (JJ, JK, JL)
        ENDDO
      ENDDO
    ENDDO
    DO JK=1,SIZE(XP_SOILCARB,2)
      DO JJ=1,KSIZE
        JI                                 = KMASK             (JJ)
        XSOILCARB       (JI, JK, KPATCH)   = XP_SOILCARB       (JJ, JK)
      ENDDO
    ENDDO
    DO JK=1,SIZE(XP_LIGNIN_STRUC,2)
      DO JJ=1,KSIZE
        JI                                  = KMASK             (JJ)
        XLIGNIN_STRUC   (JI, JK, KPATCH)    = XP_LIGNIN_STRUC   (JJ, JK)
      ENDDO
    ENDDO
    DO JK=1,SIZE(XP_TURNOVER,2)
      DO JJ=1,KSIZE
        JI                      =    KMASK(JJ)
        XTURNOVER       (JI, JK, KPATCH)    = XP_TURNOVER       (JJ, JK)
      ENDDO
    END DO
  END IF
  !
  IF(LAGRIP .AND. (CPHOTO=='NIT' .OR. CPHOTO=='LAI' .OR. CPHOTO=='LST' .OR. CPHOTO=='NCB') ) THEN
     DO JJ=1,KSIZE
       JI                    =  KMASK             (JJ)
       LIRRIDAY (JI,KPATCH)  =  XP_LIRRIDAY       (JJ)
     END DO
  END IF
  !
  IF (TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') THEN
    DO JK=1,SIZE(XP_SNOWHEAT,2)
      DO JJ=1,KSIZE
        JI                              = KMASK         (JJ)
        TSNOW%HEAT      (JI, JK, KPATCH) = XP_SNOWHEAT   (JJ, JK)
      ENDDO
    ENDDO
    DO JJ=1,KSIZE
      JI                              = KMASK         (JJ)
      TSNOW%EMIS      (JI, KPATCH)    = XP_SNOWEMIS   (JJ)
    END DO
  END IF

  IF (TSNOW%SCHEME=='CRO') THEN
    DO JK=1,SIZE(XP_SNOWGRAN1,2)
      DO JJ=1,KSIZE
        JI                              = KMASK         (JJ)
        TSNOW%GRAN1     (JI, JK, KPATCH) = XP_SNOWGRAN1   (JJ, JK)
        TSNOW%GRAN2     (JI, JK, KPATCH) = XP_SNOWGRAN2   (JJ, JK)
        TSNOW%HIST      (JI, JK, KPATCH) = XP_SNOWHIST    (JJ, JK)
        TSNOW%AGE       (JI, JK, KPATCH) = XP_SNOWAGE     (JJ, JK)
      ENDDO
    END DO
  END IF
  !
  IF(LFLOOD)THEN
    DO JJ=1,KSIZE
      JI                    = KMASK     (JJ)
      XZ0_FLOOD(JI, KPATCH) = XP_Z0FLOOD(JJ)
    END DO
  END IF  
  !
  IF(LGLACIER)THEN
    DO JJ=1,KSIZE
       JI                   = KMASK     (JJ)
       XICE_STO(JI, KPATCH) = XP_ICE_STO(JJ)
    ENDDO
  ENDIF
!
END IF
!
!------------------------------------------------------------------
!
XP_Z0_O_Z0H     => NULL()
XP_EMIS         => NULL()
XP_ALBNIR       => NULL()
XP_ALBVIS       => NULL()
XP_ALBUV        => NULL()
XP_ALBNIR_VEG   => NULL()
XP_ALBVIS_VEG   => NULL()
XP_ALBUV_VEG    => NULL()
XP_ALBNIR_SOIL  => NULL()
XP_ALBVIS_SOIL  => NULL()
XP_ALBUV_SOIL   => NULL()
XP_Z0           => NULL()
XP_WRMAX_CF     => NULL()
XP_GAMMA        => NULL()
XP_CV           => NULL()
XP_RGL          => NULL()
XP_RUNOFFD      => NULL()
XP_Z0EFFIP      => NULL()
XP_Z0EFFIM      => NULL()
XP_Z0EFFJP      => NULL()
XP_Z0EFFJM      => NULL()
XP_WR           => NULL() 
XP_LAI          => NULL() 
XP_RESA         => NULL()
XP_CPS          => NULL()
XP_LVTT         => NULL()
XP_LSTT         => NULL()
XP_VEG          => NULL()
XP_SNOWALB      => NULL()
XP_LE           => NULL() 
XP_PSN          => NULL()
XP_PSNG         => NULL()
XP_PSNV         => NULL()
XP_ALBNIR_DRY   => NULL()
XP_ALBVIS_DRY   => NULL()
XP_ALBUV_DRY    => NULL()
XP_ALBNIR_WET   => NULL()
XP_ALBVIS_WET   => NULL()
XP_ALBUV_WET    => NULL()
XP_RUNOFFB      => NULL()
XP_WDRAIN       => NULL()
XP_TAUICE       => NULL()
XP_Z0REL        => NULL()
XP_AOSIP        => NULL()
XP_AOSIM        => NULL()
XP_AOSJP        => NULL()
XP_AOSJM        => NULL()
XP_HO2IP        => NULL()
XP_HO2IM        => NULL()
XP_HO2JP        => NULL()
XP_HO2JM        => NULL()
XP_SSO_SLOPE    => NULL()
XP_GAMMAT       => NULL()
XP_TDEEP        => NULL() 
!
XP_CLAY         => NULL() 
XP_SAND         => NULL() 
XP_WFC          => NULL()
XP_WWILT        => NULL()
XP_WSAT         => NULL()
XP_CONDSAT      => NULL()
XP_DG           => NULL()
XP_WG           => NULL()
XP_WGI          => NULL()
!
XP_KSAT_ICE     => NULL()
XP_D_ICE        => NULL()
!
XP_VEGTYPE_PATCH=> NULL()
!
XP_TG           => NULL()
!
XP_SNOWSWE      => NULL()
XP_SNOWRHO      => NULL()
!
XP_DIR_ALB_WITH_SNOW=> NULL()
XP_SCA_ALB_WITH_SNOW=> NULL()
!
XP_FFLOOD       => NULL()
XP_PIFLOOD      => NULL()
XP_Z0FLOOD      => NULL()
XP_FF           => NULL()
XP_FFG          => NULL()
XP_FFV          => NULL()
XP_FFROZEN      => NULL()
XP_ALBF         => NULL()
XP_EMISF        => NULL()
!
XP_PSNV_A       => NULL()
!
XP_SNOWHEAT     => NULL()
XP_SNOWEMIS     => NULL() 
!
XP_SNOWGRAN1    => NULL()
XP_SNOWGRAN2    => NULL()
XP_SNOWHIST     => NULL()
XP_SNOWAGE      => NULL()
!
XP_ICE_STO      => NULL()
!
XP_HCAPSOIL     => NULL()
!
XP_CONDDRY      => NULL()
XP_CONDSLD      => NULL()
!
XP_C4B          => NULL() 
XP_ACOEF        => NULL() 
XP_PCOEF        => NULL()
XP_CGSAT        => NULL() 
XP_C1SAT        => NULL() 
XP_C2REF        => NULL() 
XP_C4REF        => NULL()
XP_C3           => NULL() 
!
XP_MPOTSAT      => NULL()
XP_BCOEF        => NULL()
!
XP_ROOTFRAC     => NULL()
XP_DZG          => NULL()
XP_DZDIF        => NULL()
NK_WG_LAYER     => NULL()
XP_SOILWGHT     => NULL()
!
XP_RSMIN        => NULL()
!
XP_BSLAI        => NULL()
XP_LAIMIN       => NULL()
XP_SEFOLD       => NULL()
XP_H_TREE       => NULL()
XP_ANF          => NULL()
XP_ANMAX        => NULL()
XP_FZERO        => NULL()
XP_EPSO         => NULL()
XP_GAMM         => NULL()
XP_QDGAMM       => NULL()
XP_GMES         => NULL()
XP_RE25         => NULL()
XP_QDGMES       => NULL()
XP_T1GMES       => NULL()
XP_T2GMES       => NULL()
XP_AMAX         => NULL()
XP_QDAMAX       => NULL()
XP_T1AMAX       => NULL()
XP_T2AMAX       => NULL()
XP_FAPARC       => NULL()
XP_FAPIRC       => NULL()
XP_LAI_EFFC     => NULL()
XP_MUS          => NULL()
XP_AN           => NULL() 
XP_ANDAY        => NULL() 
XP_ANFM         => NULL() 
XP_GC           => NULL()
XP_LAT          => NULL()
XP_LON          => NULL()
XP_BIOMASS      => NULL()
XP_RESP_BIOMASS => NULL()
!
LP_STRESS       => NULL()
XP_F2I          => NULL()
XP_AH           => NULL()
XP_BH           => NULL()
XP_DMAX         => NULL()
!
TP_SEED         => NULL()
TP_REAP         => NULL()
XP_IRRIG        => NULL()
XP_WATSUP       => NULL()
!
XP_LIRRIDAY     => NULL()
XP_THRESHOLD    => NULL()
XP_LIRRIGATE    => NULL()
!
XP_CE_NITRO     => NULL()
XP_CF_NITRO     => NULL()
XP_CNA_NITRO    => NULL()
XP_BSLAI_NITRO  => NULL()
!
XP_INCREASE     => NULL()
XP_TAU_WOOD     => NULL()
!
XP_LITTER       => NULL()
XP_SOILCARB     => NULL()
XP_LIGNIN_STRUC => NULL()
XP_TURNOVER     => NULL()
!
XP_FSAT=> NULL()
!
XP_MUF=> NULL()
!
!
DEALLOCATE(LBLOCK_SIMPLE)
DEALLOCATE(LBLOCK_0)
DEALLOCATE(NBLOCK_SIMPLE)
DEALLOCATE(NBLOCK_0)
DEALLOCATE(TBLOCK_SIMPLE)
DEALLOCATE(TBLOCK_0)
DEALLOCATE(XBLOCK_SIMPLE)
DEALLOCATE(XBLOCK_GROUND)
DEALLOCATE(XBLOCK_VEGTYPE)
DEALLOCATE(XBLOCK_TG)
DEALLOCATE(XBLOCK_SNOW)
DEALLOCATE(XBLOCK_ALB)
DEALLOCATE(XBLOCK_2)
DEALLOCATE(XBLOCK_BIOMASS)
DEALLOCATE(XBLOCK_SOILCARB)
DEALLOCATE(XBLOCK_LITTLEVS)
DEALLOCATE(XBLOCK_LITTER)
DEALLOCATE(XBLOCK_0)
DEALLOCATE(XBLOCK_00)
DEALLOCATE(XBLOCK_000)
DEALLOCATE(XBLOCK_01)
!
IF (LHOOK) CALL DR_HOOK('UNPACK_ISBA_PATCH_N',1,ZHOOK_HANDLE)
!------------------------------------------------------------------
!
END SUBROUTINE UNPACK_ISBA_PATCH_n
