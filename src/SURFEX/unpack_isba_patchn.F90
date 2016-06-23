!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE UNPACK_ISBA_PATCH_n (AG, I, PKI, &
                                KMASK,KSIZE,KPATCH)
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
!!      B. Decharme  06/2013 : add lateral drainage flux diag for DIF
!!                             water table / surface coupling
!!      P. Samuelsson 02/2012 : MEB
!!
!!------------------------------------------------------------------
!

!
USE MODD_AGRI_n, ONLY : AGRI_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_PACK_ISBA, ONLY : PACK_ISBA_t
!
USE MODD_AGRI,     ONLY :  LAGRIP

!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(AGRI_t), INTENT(INOUT) :: AG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(PACK_ISBA_t), INTENT(INOUT) :: PKI
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
IF (I%NPATCH==1) THEN
  I%TSNOW%WSNOW     (:, :, 1) = PKI%XP_SNOWSWE    (:, :)
  I%TSNOW%RHO       (:, :, 1) = PKI%XP_SNOWRHO    (:, :)
  I%TSNOW%ALB       (:, 1)    = PKI%XP_SNOWALB    (:)
  I%XWR             (:, 1)    = PKI%XP_WR         (:)
  I%XTG             (:, :, 1) = PKI%XP_TG         (:, :)
  I%XWG             (:, :, 1) = PKI%XP_WG         (:, :)
  I%XWGI            (:, :, 1) = PKI%XP_WGI        (:, :)
  I%XRESA           (:, 1)    = PKI%XP_RESA       (:) 
  I%XPCPS           (:, 1)    = PKI%XP_CPS        (:) 
  I%XPLVTT          (:, 1)    = PKI%XP_LVTT       (:) 
  I%XPLSTT          (:, 1)    = PKI%XP_LSTT       (:) 
  I%XALBNIR         (:, 1)    = PKI%XP_ALBNIR     (:) 
  I%XALBVIS         (:, 1)    = PKI%XP_ALBVIS     (:) 
  I%XALBUV          (:, 1)    = PKI%XP_ALBUV      (:) 
  I%XALBNIR_VEG     (:, 1)    = PKI%XP_ALBNIR_VEG (:) 
  I%XALBVIS_VEG     (:, 1)    = PKI%XP_ALBVIS_VEG (:) 
  I%XALBUV_VEG      (:, 1)    = PKI%XP_ALBUV_VEG  (:) 
  I%XALBNIR_SOIL    (:, 1)    = PKI%XP_ALBNIR_SOIL(:) 
  I%XALBVIS_SOIL    (:, 1)    = PKI%XP_ALBVIS_SOIL(:) 
  I%XALBUV_SOIL     (:, 1)    = PKI%XP_ALBUV_SOIL (:) 
  I%XEMIS           (:, 1)    = PKI%XP_EMIS       (:) 
  I%XZ0EFFIP        (:, 1)    = PKI%XP_Z0EFFIP    (:) 
  I%XZ0EFFIM        (:, 1)    = PKI%XP_Z0EFFIM    (:) 
  I%XZ0EFFJP        (:, 1)    = PKI%XP_Z0EFFJP    (:) 
  I%XZ0EFFJM        (:, 1)    = PKI%XP_Z0EFFJM    (:) 
  I%XLE             (:, 1)    = PKI%XP_LE         (:)
  !
   IF(I%LMEB_PATCH(KPATCH))THEN
     I%XWRL            (:, 1)    = PKI%XP_WRL        (:)
     I%XWRLI           (:, 1)    = PKI%XP_WRLI       (:)
     I%XWRVN           (:, 1)    = PKI%XP_WRVN       (:)
     I%XTV             (:, 1)    = PKI%XP_TV         (:)
     I%XTL             (:, 1)    = PKI%XP_TL         (:)
     I%XTC             (:, 1)    = PKI%XP_TC         (:)
     I%XQC             (:, 1)    = PKI%XP_QC         (:)
     I%XLAI            (:, 1)    = PKI%XP_LAI        (:) 
     I%XZ0             (:, 1)    = PKI%XP_Z0         (:) 
   ELSE
! Please note that XLAI, XVEG, and XZ0 are not unpacked
! in the case of MEB.
     I%XLAI            (:, 1)    = PKI%XP_LAI        (:) 
     I%XVEG            (:, 1)    = PKI%XP_VEG        (:) 
     I%XZ0             (:, 1)    = PKI%XP_Z0         (:) 
   ENDIF
  !
  IF (I%LTR_ML) THEN
    I%XFAPARC         (:, 1)    = PKI%XP_FAPARC     (:)
    I%XFAPIRC         (:, 1)    = PKI%XP_FAPIRC     (:)
    I%XLAI_EFFC       (:, 1)    = PKI%XP_LAI_EFFC   (:)
    I%XMUS            (:, 1)    = PKI%XP_MUS        (:)
  ENDIF   
  !
  IF (I%CPHOTO/='NON') THEN
     I%XAN             (:, 1)    = PKI%XP_AN         (:)
     I%XANDAY          (:, 1)    = PKI%XP_ANDAY      (:)
     I%XANFM           (:, 1)    = PKI%XP_ANFM       (:)
     I%XBIOMASS        (:,:,1)   = PKI%XP_BIOMASS        (:,:)
     I%XRESP_BIOMASS   (:,:,1)   = PKI%XP_RESP_BIOMASS   (:,:)
  END IF
  !
  IF(I%CPHOTO=='NIT' .OR. I%CPHOTO=='NCB') THEN
     I%XBSLAI_NITRO    (:,1)    =    PKI%XP_BSLAI_NITRO    (:)          
  END IF
  !
    IF(I%CPHOTO=='NCB') THEN
     I%XINCREASE       (:,:,1)   =    PKI%XP_INCREASE       (:,:)
  END IF
  !
  IF(I%CRESPSL=='CNT') THEN
     I%XLITTER         (:,:,:,1) =    PKI%XP_LITTER         (:,:,:)
     I%XSOILCARB       (:,:,1)   =    PKI%XP_SOILCARB       (:,:)
     I%XLIGNIN_STRUC   (:,:,1)   =    PKI%XP_LIGNIN_STRUC   (:,:)
     I%XTURNOVER       (:,:,1)   =    PKI%XP_TURNOVER       (:,:)
  END IF
  !
  IF(LAGRIP .AND. (I%CPHOTO=='NIT' .OR. I%CPHOTO=='LAI' .OR. I%CPHOTO=='LST' .OR. I%CPHOTO=='NCB') ) THEN
    AG%LIRRIDAY (:,1)  =    PKI%XP_LIRRIDAY (:)
  END IF
  !
  IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN
     I%TSNOW%HEAT      (:, :, 1) = PKI%XP_SNOWHEAT   (:, :)
     I%TSNOW%EMIS      (:, 1)    = PKI%XP_SNOWEMIS   (:)
     I%TSNOW%AGE       (:, :, 1) = PKI%XP_SNOWAGE    (:, :)
     I%TSNOW%ALBVIS    (:, 1)    = PKI%XP_SNOWALBVIS (:)
     I%TSNOW%ALBNIR    (:, 1)    = PKI%XP_SNOWALBNIR (:)
     I%TSNOW%ALBFIR    (:, 1)    = PKI%XP_SNOWALBFIR (:)     
  END IF

  IF (I%TSNOW%SCHEME=='CRO') THEN
     I%TSNOW%GRAN1     (:, :, 1) = PKI%XP_SNOWGRAN1   (:, :)
     I%TSNOW%GRAN2     (:, :, 1) = PKI%XP_SNOWGRAN2   (:, :)
     I%TSNOW%HIST      (:, :, 1) = PKI%XP_SNOWHIST    (:, :)
  END IF
  !
  IF(I%LGLACIER)THEN
     I%XICE_STO        (:,1)     = PKI%XP_ICE_STO    (:)
  ENDIF
!
ELSE
!
! Only save values for patches which are in use:
!
  DO JJ=1,KSIZE
    JI                              = KMASK         (JJ)
    I%TSNOW%ALB       (JI, KPATCH)    = PKI%XP_SNOWALB    (JJ)
    I%XWR             (JI, KPATCH)    = PKI%XP_WR         (JJ)
    I%XRESA           (JI, KPATCH)    = PKI%XP_RESA       (JJ) 
    I%XPCPS           (JI, KPATCH)    = PKI%XP_CPS        (JJ) 
    I%XPLVTT          (JI, KPATCH)    = PKI%XP_LVTT       (JJ) 
    I%XPLSTT          (JI, KPATCH)    = PKI%XP_LSTT       (JJ) 
    I%XALBNIR         (JI, KPATCH)    = PKI%XP_ALBNIR     (JJ) 
    I%XALBVIS         (JI, KPATCH)    = PKI%XP_ALBVIS     (JJ) 
    I%XALBUV          (JI, KPATCH)    = PKI%XP_ALBUV      (JJ) 
    I%XALBNIR_VEG     (JI, KPATCH)    = PKI%XP_ALBNIR_VEG (JJ) 
    I%XALBVIS_VEG     (JI, KPATCH)    = PKI%XP_ALBVIS_VEG (JJ) 
    I%XALBUV_VEG      (JI, KPATCH)    = PKI%XP_ALBUV_VEG  (JJ) 
    I%XALBNIR_SOIL    (JI, KPATCH)    = PKI%XP_ALBNIR_SOIL(JJ) 
    I%XALBVIS_SOIL    (JI, KPATCH)    = PKI%XP_ALBVIS_SOIL(JJ) 
    I%XALBUV_SOIL     (JI, KPATCH)    = PKI%XP_ALBUV_SOIL (JJ) 
    I%XEMIS           (JI, KPATCH)    = PKI%XP_EMIS       (JJ) 
    I%XZ0EFFIP        (JI, KPATCH)    = PKI%XP_Z0EFFIP    (JJ) 
    I%XZ0EFFIM        (JI, KPATCH)    = PKI%XP_Z0EFFIM    (JJ) 
    I%XZ0EFFJP        (JI, KPATCH)    = PKI%XP_Z0EFFJP    (JJ) 
    I%XZ0EFFJM        (JI, KPATCH)    = PKI%XP_Z0EFFJM    (JJ) 
    I%XLE             (JI, KPATCH)    = PKI%XP_LE         (JJ)
  !
  END DO
  !
  IF(I%LMEB_PATCH(KPATCH))THEN
    DO JJ=1,KSIZE
      JI                              = KMASK         (JJ)
      I%XWRL            (JI, KPATCH)    = PKI%XP_WRL        (JJ)
      I%XWRLI           (JI, KPATCH)    = PKI%XP_WRLI       (JJ)
      I%XWRVN           (JI, KPATCH)    = PKI%XP_WRVN       (JJ)
      I%XTV             (JI, KPATCH)    = PKI%XP_TV         (JJ)
      I%XTL             (JI, KPATCH)    = PKI%XP_TL         (JJ)
      I%XTC             (JI, KPATCH)    = PKI%XP_TC         (JJ)
      I%XQC             (JI, KPATCH)    = PKI%XP_QC         (JJ)
      I%XLAI            (JI, KPATCH)    = PKI%XP_LAI        (JJ) 
      I%XZ0             (JI, KPATCH)    = PKI%XP_Z0         (JJ) 
    END DO
  ELSE
! Please note that XLAI, XVEG, and XZ0 are not unpacked
! in the case of MEB yet. This must be done when interactive/carbon
! vegetation is activated for MEB.
    DO JJ=1,KSIZE
      JI                              = KMASK         (JJ)
      I%XLAI            (JI, KPATCH)    = PKI%XP_LAI        (JJ) 
      I%XVEG            (JI, KPATCH)    = PKI%XP_VEG        (JJ) 
      I%XZ0             (JI, KPATCH)    = PKI%XP_Z0         (JJ) 
    END DO
  ENDIF
  !
  DO JK=1,SIZE(I%XTG,2)
    DO JJ=1,KSIZE
      JI                      =    KMASK(JJ)
      I%XTG             (JI, JK, KPATCH) = PKI%XP_TG         (JJ, JK)
    ENDDO
  ENDDO
!  
  DO JK=1,SIZE(I%XWG,2)
    DO JJ=1,KSIZE
      JI                      =    KMASK(JJ)
      I%XWG             (JI, JK, KPATCH) = PKI%XP_WG         (JJ, JK)
      I%XWGI            (JI, JK, KPATCH) = PKI%XP_WGI        (JJ, JK)
    ENDDO
  ENDDO
!  
  DO JK=1,SIZE(PKI%XP_SNOWSWE,2)
    DO JJ=1,KSIZE
      JI                      =    KMASK(JJ)
      I%TSNOW%WSNOW     (JI, JK, KPATCH) = PKI%XP_SNOWSWE    (JJ, JK)
      I%TSNOW%RHO       (JI, JK, KPATCH) = PKI%XP_SNOWRHO    (JJ, JK)
    ENDDO
  ENDDO
  !
  IF (I%LTR_ML) THEN
    DO JJ=1,KSIZE
      JI                      =    KMASK(JJ)          
      I%XFAPARC         (JI, KPATCH)    = PKI%XP_FAPARC     (JJ)
      I%XFAPIRC         (JI, KPATCH)    = PKI%XP_FAPIRC     (JJ)
      I%XLAI_EFFC       (JI, KPATCH)    = PKI%XP_LAI_EFFC   (JJ)
      I%XMUS            (JI, KPATCH)    = PKI%XP_MUS        (JJ)
    ENDDO
  ENDIF  
  !
  IF (I%CPHOTO/='NON') THEN
    DO JJ=1,KSIZE
      JI                              = KMASK         (JJ)
      I%XAN             (JI, KPATCH)    = PKI%XP_AN         (JJ)
      I%XANDAY          (JI, KPATCH)    = PKI%XP_ANDAY      (JJ)
      I%XANFM           (JI, KPATCH)    = PKI%XP_ANFM       (JJ)
    ENDDO
    DO JK=1,SIZE(I%XBIOMASS,2)
      DO JJ=1,KSIZE
        JI                              = KMASK         (JJ)       
        I%XBIOMASS        (JI, JK, KPATCH) = PKI%XP_BIOMASS        (JJ, JK)
        I%XRESP_BIOMASS   (JI, JK, KPATCH) = PKI%XP_RESP_BIOMASS   (JJ, JK)
      ENDDO
    END DO
  END IF
  !
  IF (I%CPHOTO=='NIT' .OR. I%CPHOTO=='NCB') THEN
    DO JJ=1,KSIZE
      JI                                 = KMASK             (JJ)
      I%XBSLAI_NITRO    (JI, KPATCH)       = PKI%XP_BSLAI_NITRO    (JJ)
    END DO
  END IF
  !
  IF (I%CPHOTO=='NCB') THEN
    DO JK=1,SIZE(I%XINCREASE,2)
      DO JJ=1,KSIZE
        JI                                 = KMASK             (JJ)
        I%XINCREASE       (JI, JK, KPATCH)   = PKI%XP_INCREASE       (JJ, JK)
      ENDDO
    END DO
  END IF
  !
  IF (I%CRESPSL=='CNT') THEN
    DO JL=1,SIZE(PKI%XP_LITTER,3)
      DO JK=1,SIZE(PKI%XP_LITTER,2)
        DO JJ=1,KSIZE
          JI                                 = KMASK             (JJ)
          I%XLITTER       (JI, JK, JL, KPATCH) = PKI%XP_LITTER         (JJ, JK, JL)
        ENDDO
      ENDDO
    ENDDO
    DO JK=1,SIZE(PKI%XP_SOILCARB,2)
      DO JJ=1,KSIZE
        JI                                 = KMASK             (JJ)
        I%XSOILCARB       (JI, JK, KPATCH)   = PKI%XP_SOILCARB       (JJ, JK)
      ENDDO
    ENDDO
    DO JK=1,SIZE(PKI%XP_LIGNIN_STRUC,2)
      DO JJ=1,KSIZE
        JI                                  = KMASK             (JJ)
        I%XLIGNIN_STRUC   (JI, JK, KPATCH)    = PKI%XP_LIGNIN_STRUC   (JJ, JK)
      ENDDO
    ENDDO
    DO JK=1,SIZE(PKI%XP_TURNOVER,2)
      DO JJ=1,KSIZE
        JI                      =    KMASK(JJ)
        I%XTURNOVER       (JI, JK, KPATCH)    = PKI%XP_TURNOVER       (JJ, JK)
      ENDDO
    END DO
  END IF
  !
  IF(LAGRIP .AND. (I%CPHOTO=='NIT' .OR. I%CPHOTO=='LAI' .OR. I%CPHOTO=='LST' .OR. I%CPHOTO=='NCB') ) THEN
     DO JJ=1,KSIZE
       JI                    =  KMASK             (JJ)
       AG%LIRRIDAY (JI,KPATCH)  =  PKI%XP_LIRRIDAY       (JJ)
     END DO
  END IF
  !
  IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN
    DO JK=1,SIZE(PKI%XP_SNOWHEAT,2)
      DO JJ=1,KSIZE
        JI                              = KMASK         (JJ)
        I%TSNOW%HEAT      (JI, JK, KPATCH) = PKI%XP_SNOWHEAT  (JJ, JK)
        I%TSNOW%AGE       (JI, JK, KPATCH) = PKI%XP_SNOWAGE   (JJ, JK)
      ENDDO
    ENDDO
    DO JJ=1,KSIZE
      JI                              = KMASK         (JJ)
      I%TSNOW%EMIS      (JI, KPATCH)    = PKI%XP_SNOWEMIS   (JJ)
      I%TSNOW%ALBVIS    (JI, KPATCH)    = PKI%XP_SNOWALBVIS (JJ)
      I%TSNOW%ALBNIR    (JI, KPATCH)    = PKI%XP_SNOWALBNIR (JJ)
      I%TSNOW%ALBFIR    (JI, KPATCH)    = PKI%XP_SNOWALBFIR (JJ)     
    END DO
  END IF

  IF (I%TSNOW%SCHEME=='CRO') THEN
    DO JK=1,SIZE(PKI%XP_SNOWGRAN1,2)
      DO JJ=1,KSIZE
        JI                              = KMASK         (JJ)
        I%TSNOW%GRAN1     (JI, JK, KPATCH) = PKI%XP_SNOWGRAN1   (JJ, JK)
        I%TSNOW%GRAN2     (JI, JK, KPATCH) = PKI%XP_SNOWGRAN2   (JJ, JK)
        I%TSNOW%HIST      (JI, JK, KPATCH) = PKI%XP_SNOWHIST    (JJ, JK)
      ENDDO
    END DO
  END IF
  !
  IF(I%LGLACIER)THEN
    DO JJ=1,KSIZE
       JI                   = KMASK     (JJ)
       I%XICE_STO(JI, KPATCH) = PKI%XP_ICE_STO(JJ)
    ENDDO
  ENDIF
!
END IF
!
!------------------------------------------------------------------
!
PKI%XP_Z0_O_Z0H     => NULL()
PKI%XP_EMIS         => NULL()
PKI%XP_ALBNIR       => NULL()
PKI%XP_ALBVIS       => NULL()
PKI%XP_ALBUV        => NULL()
PKI%XP_ALBNIR_VEG   => NULL()
PKI%XP_ALBVIS_VEG   => NULL()
PKI%XP_ALBUV_VEG    => NULL()
PKI%XP_ALBNIR_SOIL  => NULL()
PKI%XP_ALBVIS_SOIL  => NULL()
PKI%XP_ALBUV_SOIL   => NULL()
PKI%XP_Z0           => NULL()
PKI%XP_WRMAX_CF     => NULL()
PKI%XP_GAMMA        => NULL()
PKI%XP_CV           => NULL()
PKI%XP_RGL          => NULL()
PKI%XP_RUNOFFD      => NULL()
PKI%XP_Z0EFFIP      => NULL()
PKI%XP_Z0EFFIM      => NULL()
PKI%XP_Z0EFFJP      => NULL()
PKI%XP_Z0EFFJM      => NULL()
PKI%XP_WR           => NULL() 
PKI%XP_LAI          => NULL() 
PKI%XP_RESA         => NULL()
PKI%XP_CPS          => NULL()
PKI%XP_LVTT         => NULL()
PKI%XP_LSTT         => NULL()
PKI%XP_VEG          => NULL()
PKI%XP_SNOWALB      => NULL()
PKI%XP_SNOWALBVIS   => NULL()
PKI%XP_SNOWALBNIR   => NULL()
PKI%XP_SNOWALBFIR   => NULL()
PKI%XP_LE           => NULL() 
PKI%XP_PSN          => NULL()
PKI%XP_PSNG         => NULL()
PKI%XP_PSNV         => NULL()
PKI%XP_ALBNIR_DRY   => NULL()
PKI%XP_ALBVIS_DRY   => NULL()
PKI%XP_ALBUV_DRY    => NULL()
PKI%XP_ALBNIR_WET   => NULL()
PKI%XP_ALBVIS_WET   => NULL()
PKI%XP_ALBUV_WET    => NULL()
PKI%XP_RUNOFFB      => NULL()
PKI%XP_WDRAIN       => NULL()
PKI%XP_TAUICE       => NULL()
PKI%XP_Z0REL        => NULL()
PKI%XP_AOSIP        => NULL()
PKI%XP_AOSIM        => NULL()
PKI%XP_AOSJP        => NULL()
PKI%XP_AOSJM        => NULL()
PKI%XP_HO2IP        => NULL()
PKI%XP_HO2IM        => NULL()
PKI%XP_HO2JP        => NULL()
PKI%XP_HO2JM        => NULL()
PKI%XP_SSO_SLOPE    => NULL()
PKI%XP_GAMMAT       => NULL()
PKI%XP_TDEEP        => NULL() 
!
PKI%XP_CLAY         => NULL() 
PKI%XP_SAND         => NULL() 
PKI%XP_WFC          => NULL()
PKI%XP_WWILT        => NULL()
PKI%XP_WSAT         => NULL()
PKI%XP_CONDSAT      => NULL()
PKI%XP_DG           => NULL()
PKI%XP_WG           => NULL()
PKI%XP_WGI          => NULL()
!
PKI%XP_KSAT_ICE     => NULL()
PKI%XP_D_ICE        => NULL()
!
PKI%XP_VEGTYPE_PATCH=> NULL()
!
PKI%XP_TG           => NULL()
!
PKI%XP_SNOWSWE      => NULL()
PKI%XP_SNOWRHO      => NULL()
!
PKI%XP_DIR_ALB_WITH_SNOW=> NULL()
PKI%XP_SCA_ALB_WITH_SNOW=> NULL()
!
PKI%XP_FFLOOD       => NULL()
PKI%XP_PIFLOOD      => NULL()
PKI%XP_FF           => NULL()
PKI%XP_FFG          => NULL()
PKI%XP_FFV          => NULL()
PKI%XP_FFROZEN      => NULL()
PKI%XP_ALBF         => NULL()
PKI%XP_EMISF        => NULL()
!
PKI%XP_PSNV_A       => NULL()
!
PKI%XP_SNOWHEAT     => NULL()
PKI%XP_SNOWEMIS     => NULL() 
!
PKI%XP_SNOWGRAN1    => NULL()
PKI%XP_SNOWGRAN2    => NULL()
PKI%XP_SNOWHIST     => NULL()
PKI%XP_SNOWAGE      => NULL()
!
PKI%XP_ICE_STO      => NULL()
!
PKI%XP_FWTD         => NULL()
PKI%XP_WTD          => NULL()
!
PKI%XP_HCAPSOIL     => NULL()
!
PKI%XP_CONDDRY      => NULL()
PKI%XP_CONDSLD      => NULL()
!
PKI%XP_C4B          => NULL() 
PKI%XP_ACOEF        => NULL() 
PKI%XP_PCOEF        => NULL()
PKI%XP_CGSAT        => NULL() 
PKI%XP_C1SAT        => NULL() 
PKI%XP_C2REF        => NULL() 
PKI%XP_C4REF        => NULL()
PKI%XP_C3           => NULL() 
!
PKI%XP_MPOTSAT      => NULL()
PKI%XP_BCOEF        => NULL()
!
PKI%XP_ROOTFRAC     => NULL()
PKI%XP_DZG          => NULL()
PKI%XP_DZDIF        => NULL()
PKI%NK_WG_LAYER     => NULL()
PKI%XP_SOILWGHT     => NULL()
!
PKI%XP_RSMIN        => NULL()
!
PKI%XP_BSLAI        => NULL()
PKI%XP_LAIMIN       => NULL()
PKI%XP_SEFOLD       => NULL()
PKI%XP_H_TREE       => NULL()
PKI%XP_ANF          => NULL()
PKI%XP_ANMAX        => NULL()
PKI%XP_FZERO        => NULL()
PKI%XP_EPSO         => NULL()
PKI%XP_GAMM         => NULL()
PKI%XP_QDGAMM       => NULL()
PKI%XP_GMES         => NULL()
PKI%XP_RE25         => NULL()
PKI%XP_QDGMES       => NULL()
PKI%XP_T1GMES       => NULL()
PKI%XP_T2GMES       => NULL()
PKI%XP_AMAX         => NULL()
PKI%XP_QDAMAX       => NULL()
PKI%XP_T1AMAX       => NULL()
PKI%XP_T2AMAX       => NULL()
PKI%XP_FAPARC       => NULL()
PKI%XP_FAPIRC       => NULL()
PKI%XP_LAI_EFFC     => NULL()
PKI%XP_MUS          => NULL()
PKI%XP_AN           => NULL() 
PKI%XP_ANDAY        => NULL() 
PKI%XP_ANFM         => NULL() 
PKI%XP_GC           => NULL()
PKI%XP_LAT          => NULL()
PKI%XP_LON          => NULL()
PKI%XP_BIOMASS      => NULL()
PKI%XP_RESP_BIOMASS => NULL()
!
PKI%LP_STRESS       => NULL()
PKI%XP_F2I          => NULL()
PKI%XP_AH           => NULL()
PKI%XP_BH           => NULL()
PKI%XP_DMAX         => NULL()
!
PKI%TP_SEED         => NULL()
PKI%TP_REAP         => NULL()
PKI%XP_IRRIG        => NULL()
PKI%XP_WATSUP       => NULL()
!
PKI%XP_LIRRIDAY     => NULL()
PKI%XP_THRESHOLD    => NULL()
PKI%XP_LIRRIGATE    => NULL()
!
PKI%XP_CE_NITRO     => NULL()
PKI%XP_CF_NITRO     => NULL()
PKI%XP_CNA_NITRO    => NULL()
PKI%XP_BSLAI_NITRO  => NULL()
!
PKI%XP_INCREASE     => NULL()
PKI%XP_TAU_WOOD     => NULL()
!
PKI%XP_LITTER       => NULL()
PKI%XP_SOILCARB     => NULL()
PKI%XP_LIGNIN_STRUC => NULL()
PKI%XP_TURNOVER     => NULL()
!
PKI%XP_FSAT=> NULL()
PKI%XP_TOPQS=> NULL()
!
PKI%XP_MUF=> NULL()
!
PKI%XP_WRL          => NULL()
PKI%XP_WRLI         => NULL()
PKI%XP_WRVN         => NULL() 
PKI%XP_TV           => NULL() 
PKI%XP_TL           => NULL() 
PKI%XP_TC           => NULL() 
PKI%XP_QC           => NULL() 

PKI%XP_H_VEG        => NULL()
PKI%XP_RGLV         => NULL()
PKI%XP_GAMMAV       => NULL()
PKI%XP_WRMAX_CFV    => NULL()
PKI%XP_LAIV         => NULL()
PKI%XP_Z0V          => NULL()
PKI%XP_RSMINV       => NULL()
PKI%XP_ROOTFRACV    => NULL()
PKI%XP_GNDLITTER    => NULL()
PKI%XP_Z0LITTER     => NULL()
!
!
DEALLOCATE(PKI%LBLOCK_SIMPLE)
DEALLOCATE(PKI%LBLOCK_0)
DEALLOCATE(PKI%NBLOCK_SIMPLE)
DEALLOCATE(PKI%NBLOCK_0)
DEALLOCATE(PKI%TBLOCK_SIMPLE)
DEALLOCATE(PKI%TBLOCK_0)
DEALLOCATE(PKI%XBLOCK_SIMPLE)
DEALLOCATE(PKI%XBLOCK_GROUND)
DEALLOCATE(PKI%XBLOCK_VEGTYPE)
DEALLOCATE(PKI%XBLOCK_TG)
DEALLOCATE(PKI%XBLOCK_SNOW)
DEALLOCATE(PKI%XBLOCK_ALB)
DEALLOCATE(PKI%XBLOCK_2)
DEALLOCATE(PKI%XBLOCK_BIOMASS)
DEALLOCATE(PKI%XBLOCK_SOILCARB)
DEALLOCATE(PKI%XBLOCK_LITTLEVS)
DEALLOCATE(PKI%XBLOCK_LITTER)
DEALLOCATE(PKI%XBLOCK_0)
DEALLOCATE(PKI%XBLOCK_00)
DEALLOCATE(PKI%XBLOCK_000)
DEALLOCATE(PKI%XBLOCK_01)
!
IF (LHOOK) CALL DR_HOOK('UNPACK_ISBA_PATCH_N',1,ZHOOK_HANDLE)
!------------------------------------------------------------------
!
END SUBROUTINE UNPACK_ISBA_PATCH_n
