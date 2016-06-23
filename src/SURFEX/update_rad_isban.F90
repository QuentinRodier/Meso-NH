!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE UPDATE_RAD_ISBA_n (I, &
                              OFLOOD,HSNOW,PZENITH,PSW_BANDS,PVEG,PLAI,PZ0, &
                               OMEB_PATCH,PLAIGV,PGNDLITTER,PZ0LITTER, PH_VEG, &
                               PALBNIR,PALBVIS,PALBUV,PEMIS,               &
                               PDIR_ALB_WITH_SNOW,PSCA_ALB_WITH_SNOW,PEMIST, &
                               PDIR_SW,PSCA_SW,                            &
                               PALBNIR_VEG, PALBNIR_SOIL,                  &
                               PALBVIS_VEG, PALBVIS_SOIL                   )
!     ####################################################################
!
!!****  *UPDATE_RAD_ISBA_n * - Calculate snow/flood fraction, dir/dif albedo
!!                             and emissivity at t+1 in order to close the 
!!                             energy budget between the atmospheric model 
!!                             and surfex  
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
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!!      P. Samuelsson 02/2012 MEB
!!      A. Boone      03/2015 MEB-use TR_ML scheme for SW radiation
!!------------------------------------------------------------------
!
!
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_TYPE_SNOW
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
!
USE MODD_CSTS,      ONLY : XTT
USE MODD_SURF_PAR,  ONLY : XUNDEF
USE MODD_SNOW_PAR,  ONLY : XRHOSMIN_ES,XRHOSMAX_ES,XSNOWDMIN,XEMISSN
USE MODD_WATER_PAR, ONLY : XALBSCA_WAT, XEMISWAT, XALBWATICE, XEMISWATICE 
USE MODD_MEB_PAR,   ONLY : XSW_WGHT_VIS, XSW_WGHT_NIR
!
USE MODE_SURF_FLOOD_FRAC
USE MODE_SURF_SNOW_FRAC      
USE MODE_MEB,       ONLY : MEB_SHIELD_FACTOR, MEBPALPHAN
!
USE MODI_ALBEDO_TA96
USE MODI_ALBEDO_FROM_NIR_VIS
USE MODI_PACK_SAME_RANK
USE MODI_UNPACK_SAME_RANK
USE MODI_ISBA_SNOW_FRAC
USE MODI_ISBA_EMIS_MEB
USE MODI_RADIATIVE_TRANSFERT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(ISBA_t), INTENT(INOUT) :: I
!
LOGICAL,                INTENT(IN)   :: OFLOOD
 CHARACTER(LEN=*),       INTENT(IN)   :: HSNOW
!
REAL, DIMENSION(:),     INTENT(IN)   :: PZENITH   ! Zenithal angle at t+1
REAL, DIMENSION(:),     INTENT(IN)   :: PSW_BANDS ! mean wavelength of each shortwave band (m)
REAL, DIMENSION(:,:),   INTENT(IN)   :: PVEG      ! Vegetation fraction at t+1
REAL, DIMENSION(:,:),   INTENT(IN)   :: PLAI      ! leaf area index at t+1
REAL, DIMENSION(:,:),   INTENT(IN)   :: PZ0       ! roughness length at t+1
REAL, DIMENSION(:,:),   INTENT(IN)   :: PALBNIR   ! near-infra-red albedo (soil+vegetation) at t+1
REAL, DIMENSION(:,:),   INTENT(IN)   :: PALBVIS   ! visible albedo (soil+vegetation) at t+1
REAL, DIMENSION(:,:),   INTENT(IN)   :: PALBUV    ! UV albedo (soil+vegetation) at t+1
REAL, DIMENSION(:,:),   INTENT(IN)   :: PEMIS     ! emissivity (soil+vegetation) at t+1
LOGICAL, DIMENSION(:),  INTENT(IN)   :: OMEB_PATCH  ! multi-energy balance logical vector
REAL, DIMENSION(:,:),   INTENT(IN)   :: PGNDLITTER  ! Ground litter fraction at t+1
REAL, DIMENSION(:,:),   INTENT(IN)   :: PLAIGV    ! Understory leaf area index at t+1
REAL, DIMENSION(:,:),   INTENT(IN)   :: PZ0LITTER ! Ground litter roughness length at t+1
REAL, DIMENSION(:,:),   INTENT(IN)   :: PH_VEG
!
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PDIR_ALB_WITH_SNOW ! Total direct albedo at t+1
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PSCA_ALB_WITH_SNOW ! Total diffuse albedo at t+1
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PEMIST             ! Total emissivity at t+1
!
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL   :: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL   :: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL   :: PALBNIR_VEG   ! near-infra-red albedo (vegetation) at t+1
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL   :: PALBNIR_SOIL  ! near-infra-red albedo (soil) at t+1
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL   :: PALBVIS_VEG   ! visible albedo (vegetation) at t+1
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL   :: PALBVIS_SOIL  ! visible albedo (soil) at t+1
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(I%XVEGTYPE,1),SIZE(I%XVEGTYPE,2)) :: ZVEGTYPE
!
INTEGER :: JPATCH, ISWB, JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
! Initialization
!-------------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_ISBA_N',0,ZHOOK_HANDLE)
ISWB   = SIZE(PSW_BANDS)
!
! Re-order VEGTYPE array to correspond with mask NR_NATURE_P:
!
ZVEGTYPE(:,:) = 0.
DO JPATCH=1,I%NPATCH
   DO JJ=1,I%NSIZE_NATURE_P(JPATCH)
      ZVEGTYPE(JJ,JPATCH) = I%XVEGTYPE(I%NR_NATURE_P(JJ,JPATCH),JPATCH)
   ENDDO
ENDDO
!
!-------------------------------------------------------------------------------------
!Patch loop
!
DO JPATCH=1,I%NPATCH
  !
  IF(I%NSIZE_NATURE_P(JPATCH)>0) CALL TREAT_NATURE(I%NSIZE_NATURE_P(JPATCH),JPATCH)
  !
ENDDO
!-------------------------------------------------------------------------------
!
!Update albedo with snow for the next time step
!
PDIR_ALB_WITH_SNOW(:,:,:)=I%XDIR_ALB_WITH_SNOW (:,:,:)
PSCA_ALB_WITH_SNOW(:,:,:)=I%XSCA_ALB_WITH_SNOW (:,:,:)
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_ISBA_N',1,ZHOOK_HANDLE)
!
 CONTAINS
!
SUBROUTINE TREAT_NATURE(KSIZE,KPATCH)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KSIZE
INTEGER, INTENT(IN) :: KPATCH
!
INTEGER             :: JP
INTEGER, DIMENSION(KSIZE) :: IMASK
!
REAL, DIMENSION(KSIZE,SIZE(I%TSNOW%WSNOW,2)) :: ZLAYERSWE
REAL, DIMENSION(KSIZE,SIZE(I%TSNOW%WSNOW,2)) :: ZLAYERRHO
REAL, DIMENSION(KSIZE,SIZE(I%TSNOW%WSNOW,2)) :: ZLAYERAGE
!
REAL, DIMENSION(KSIZE,ISWB) :: ZDIR_ALB_WITH_SNOW
REAL, DIMENSION(KSIZE,ISWB) :: ZSCA_ALB_WITH_SNOW
!
REAL, DIMENSION(KSIZE) :: ZSNOWALB          
REAL, DIMENSION(KSIZE) :: ZSNOWALBVIS
REAL, DIMENSION(KSIZE) :: ZSNOWALBNIR
REAL, DIMENSION(KSIZE) :: ZLAI             
!REAL, DIMENSION(KSIZE) :: ZLAIV             
REAL, DIMENSION(KSIZE) :: ZZ0               
REAL, DIMENSION(KSIZE) :: ZVEG
REAL, DIMENSION(KSIZE) :: ZEMIS    
REAL, DIMENSION(KSIZE) :: ZALBNIR           
REAL, DIMENSION(KSIZE) :: ZALBVIS           
REAL, DIMENSION(KSIZE) :: ZALBUV  
REAL, DIMENSION(KSIZE) :: ZALBNIR_VEG
REAL, DIMENSION(KSIZE) :: ZALBNIR_SOIL
REAL, DIMENSION(KSIZE) :: ZALBVIS_VEG
REAL, DIMENSION(KSIZE) :: ZALBVIS_SOIL
!
REAL, DIMENSION(KSIZE) :: ZPSN
REAL, DIMENSION(KSIZE) :: ZPSNV_A
REAL, DIMENSION(KSIZE) :: ZPSNG             
REAL, DIMENSION(KSIZE) :: ZPSNV 
!
REAL, DIMENSION(KSIZE) :: ZALBF
REAL, DIMENSION(KSIZE) :: ZALBF_DIR
REAL, DIMENSION(KSIZE) :: ZALBF_SCA
REAL, DIMENSION(KSIZE) :: ZEMISF   
REAL, DIMENSION(KSIZE) :: ZFF   
!
REAL, DIMENSION(KSIZE) :: ZALBNIR_WITH_SNOW
REAL, DIMENSION(KSIZE) :: ZALBVIS_WITH_SNOW
REAL, DIMENSION(KSIZE) :: ZALBUV_WITH_SNOW
!
REAL, DIMENSION(KSIZE) :: ZEMIST    
REAL, DIMENSION(KSIZE) :: ZZENITH
REAL, DIMENSION(KSIZE) :: ZH_VEG
REAL, DIMENSION(KSIZE) :: ZSNOWDEPTH, ZPALPHAN
REAL, DIMENSION(KSIZE) :: ZSWUP
REAL, DIMENSION(KSIZE) :: ZGLOBAL_SW
REAL, DIMENSION(KSIZE) :: ZALBT
REAL, DIMENSION(KSIZE) :: ZPSNA, ZSIGMA_F, ZSIGMA_FN, ZEMISSN
REAL, DIMENSION(KSIZE,ISWB) :: ZDIR_SW, ZSCA_SW
REAL, DIMENSION(KSIZE) :: ZPERMSNOWFRAC, ZDSGRAIN
REAL, DIMENSION(KSIZE,3) :: ZSPECTRALALBEDO
!
REAL, DIMENSION(KSIZE)            :: ZLAIN, ZALBVIS_TSOIL, ZALBNIR_TSOIL    
REAL, DIMENSION(KSIZE)            :: ZFAPIR, ZFAPAR, ZFAPIR_BS, ZFAPAR_BS
REAL, DIMENSION(KSIZE,SIZE(I%XABC)) :: ZIACAN_SUNLIT, ZIACAN_SHADE, ZFRAC_SUN, ZIACAN
REAL, DIMENSION(KSIZE)            :: ZFAPARC, ZFAPIRC, ZMUS, ZLAI_EFFC
LOGICAL, DIMENSION(KSIZE)         :: GSHADE
!
REAL, PARAMETER :: ZPUT0 = 0.0
INTEGER  :: JSWB
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_ISBA_N:TREAT_NATURE',0,ZHOOK_HANDLE)
!
IMASK(:)=I%NR_NATURE_P(1:KSIZE,KPATCH)
!
 CALL PACK_SAME_RANK(IMASK(:),I%TSNOW%WSNOW(:,:,KPATCH),ZLAYERSWE(:,:))
 CALL PACK_SAME_RANK(IMASK(:),I%TSNOW%RHO  (:,:,KPATCH),ZLAYERRHO(:,:))
!  
 CALL PACK_SAME_RANK(IMASK(:),I%TSNOW%ALB  (:,KPATCH),ZSNOWALB(:))
!
IF(OMEB_PATCH(KPATCH))THEN

!  IF(NPATCH==1)THEN
!    ZDIR_SW(:,:) = PDIR_SW(:,:)
!  ELSE
!    DO JK=1,SIZE(PDIR_SW,2)
!      DO JJ=1,KSIZE
!        JI=IMASK(JJ)
!        ZDIR_SW(JJ,JK) = PDIR_SW (JI,JK)
!        ZSCA_SW(JJ,JK) = PSCA_SW (JI,JK)
!      ENDDO
!    ENDDO
!  ENDIF

  CALL PACK_SAME_RANK(IMASK(:),PZENITH    (:),       ZZENITH     (:))
!  CALL PACK_SAME_RANK(IMASK(:),PLAIGV     (:,KPATCH),ZLAI        (:))
!  CALL PACK_SAME_RANK(IMASK(:),PLAI       (:,KPATCH),ZLAIV       (:))
!  CALL PACK_SAME_RANK(IMASK(:),PZ0LITTER  (:,KPATCH),ZZ0         (:))
!  CALL PACK_SAME_RANK(IMASK(:),PGNDLITTER (:,KPATCH),ZVEG        (:))
  ZVEG(:)=0. ! Set veg=0 for MEB to get bare soil conditions for snow cover and
!            ! flood fraction
  CALL PACK_SAME_RANK(IMASK(:),PH_VEG     (:,KPATCH),ZH_VEG      (:))
!
  IF(PRESENT(PDIR_SW))THEN
!
    CALL PACK_SAME_RANK(IMASK(:),PDIR_SW    (:,:),     ZDIR_SW     (:,:))
    CALL PACK_SAME_RANK(IMASK(:),PSCA_SW    (:,:),     ZSCA_SW     (:,:))
    CALL PACK_SAME_RANK(IMASK(:),PALBNIR_VEG  (:,KPATCH),ZALBNIR_VEG  (:))
    CALL PACK_SAME_RANK(IMASK(:),PALBNIR_SOIL (:,KPATCH),ZALBNIR_SOIL (:))
    CALL PACK_SAME_RANK(IMASK(:),PALBVIS_VEG  (:,KPATCH),ZALBVIS_VEG  (:))
    CALL PACK_SAME_RANK(IMASK(:),PALBVIS_SOIL (:,KPATCH),ZALBVIS_SOIL (:))
!
    CALL PACK_SAME_RANK(IMASK(:),I%TSNOW%AGE  (:,:,KPATCH),ZLAYERAGE(:,:))
!
    CALL PACK_SAME_RANK(IMASK(:),I%XVEGTYPE_PATCH(:,NVT_SNOW,KPATCH),ZPERMSNOWFRAC(:))    
!
  ENDIF
!
ELSE
!  CALL PACK_SAME_RANK(IMASK(:),PLAI       (:,KPATCH),ZLAI    (:))
!  CALL PACK_SAME_RANK(IMASK(:),PZ0        (:,KPATCH),ZZ0     (:))
  CALL PACK_SAME_RANK(IMASK(:),PVEG       (:,KPATCH),ZVEG    (:))
!  ZALBNIR_VEG(:)  = XUNDEF
!  ZALBNIR_SOIL(:) = XUNDEF
!  ZALBVIS_VEG(:)  = XUNDEF
!  ZALBVIS_SOIL(:) = XUNDEF
ENDIF
!
 CALL PACK_SAME_RANK(IMASK(:),PLAI       (:,KPATCH),ZLAI    (:))
 CALL PACK_SAME_RANK(IMASK(:),PZ0        (:,KPATCH),ZZ0     (:))
 CALL PACK_SAME_RANK(IMASK(:),PEMIS      (:,KPATCH),ZEMIS   (:))
 CALL PACK_SAME_RANK(IMASK(:),PALBNIR    (:,KPATCH),ZALBNIR (:))
 CALL PACK_SAME_RANK(IMASK(:),PALBVIS    (:,KPATCH),ZALBVIS (:))
 CALL PACK_SAME_RANK(IMASK(:),PALBUV     (:,KPATCH),ZALBUV  (:))
!
IF (HSNOW=='3-L' .OR. HSNOW=='CRO') THEN
  CALL PACK_SAME_RANK(IMASK(:),I%TSNOW%ALBVIS  (:,KPATCH),ZSNOWALBVIS(:))
  CALL PACK_SAME_RANK(IMASK(:),I%TSNOW%ALBNIR  (:,KPATCH),ZSNOWALBNIR(:))
ENDIF
!   
!-------------------------------------------------------------------------------
!
 CALL ISBA_SNOW_FRAC(HSNOW, ZLAYERSWE, ZLAYERRHO, ZSNOWALB,    &
         ZVEG, ZLAI, ZZ0,ZPSN(:), ZPSNV_A(:), ZPSNG(:), ZPSNV(:) )  
!
IF ( HSNOW=='EBA' ) CALL UNPACK_SAME_RANK(IMASK(:),ZPSNV_A(:),I%XPSNV_A(:,KPATCH),ZPUT0) 
!
!-------------------------------------------------------------------------------
!
! Flood fractions and properties
!
IF(OFLOOD)THEN   
  CALL TREAT_FLOOD(KSIZE,KPATCH,IMASK,ZPSNG,ZPSNV,ZLAI,ZVEG,&
                ZALBF, ZALBF_DIR,ZALBF_SCA,ZEMISF,ZFF)
ELSE
  ZALBF     (:)=0.0
  ZALBF_DIR (:)=0.0
  ZALBF_SCA (:)=0.0
  ZEMISF    (:)=0.0
  ZFF       (:)=0.0
ENDIF        
!-------------------------------------------------------------------------------
!
ZSPECTRALALBEDO(:,:) = 0.
ZPERMSNOWFRAC(:)     = 0.
!
IF(OMEB_PATCH(KPATCH))THEN
!
   ZSNOWDEPTH(:) = SUM(ZLAYERSWE(:,:)/ZLAYERRHO(:,:),2)
   ZPALPHAN(:)   = MEBPALPHAN(ZSNOWDEPTH,ZH_VEG)
!
   ZDIR_ALB_WITH_SNOW(:,:)=XUNDEF
   ZSCA_ALB_WITH_SNOW(:,:)=XUNDEF
!
   IF(PRESENT(PDIR_SW))THEN
!
! Albedo
!
! - just extract some parameters for call, but no need to update 
!   the cummulative variables in this routine:
!
      CALL PACK_SAME_RANK(IMASK(:),I%XLAI_EFFC(:,KPATCH),ZLAI_EFFC(:))
      CALL PACK_SAME_RANK(IMASK(:),I%XFAPARC(:,KPATCH),  ZFAPARC(:)  )
      CALL PACK_SAME_RANK(IMASK(:),I%XFAPIRC(:,KPATCH),  ZFAPIRC(:)  )
      CALL PACK_SAME_RANK(IMASK(:),I%XMUS(:,KPATCH),     ZMUS(:)     )
!
      ZPERMSNOWFRAC(:)     = 0. ! assume no vegetation overlying permanent snow 

      ZSPECTRALALBEDO(:,1) = ZSNOWALBVIS(:)
      ZSPECTRALALBEDO(:,2) = ZSNOWALBNIR(:)
      ZSPECTRALALBEDO(:,3) = XUNDEF ! Currently, MEB only considers 2 spectral bands
!
      DO JSWB=1,ISWB
         ZGLOBAL_SW(:) = ZDIR_SW(:,JSWB) + ZSCA_SW(:,JSWB)

         WHERE(ZSNOWALB(:)/=XUNDEF .AND. ZSNOWALBVIS(:)/=XUNDEF .AND. ZSNOWALBNIR(:)/=XUNDEF)
            ZLAIN(:)         = ZLAI(:)*(1.0-ZPALPHAN(:))
            ZALBVIS_TSOIL(:) = ZALBVIS_SOIL(:)*(1.-ZPSN(:)) + ZPSN(:)*ZSNOWALBVIS(:)
            ZALBNIR_TSOIL(:) = ZALBNIR_SOIL(:)*(1.-ZPSN(:)) + ZPSN(:)*ZSNOWALBNIR(:)
         ELSEWHERE
            ZLAIN(:)         = ZLAI(:)
            ZALBVIS_TSOIL(:) = ZALBVIS_SOIL(:)
            ZALBNIR_TSOIL(:) = ZALBNIR_SOIL(:)
         END WHERE
         !
         CALL RADIATIVE_TRANSFERT(I%LAGRI_TO_GRASS, ZVEGTYPE(1:KSIZE,:),           &
              ZALBVIS_VEG, ZALBVIS_TSOIL, ZALBNIR_VEG, ZALBNIR_TSOIL,              &
              ZGLOBAL_SW, ZLAIN, ZZENITH, I%XABC,                                  &
              ZFAPARC, ZFAPIRC, ZMUS, ZLAI_EFFC, GSHADE, ZIACAN,                   &              
              ZIACAN_SUNLIT, ZIACAN_SHADE, ZFRAC_SUN,                              &
              ZFAPAR, ZFAPIR, ZFAPAR_BS, ZFAPIR_BS                                 )    

! Total effective surface (canopy, ground/flooded zone, snow) all-wavelength
! albedo: diagnosed from shortwave energy budget closure.
! Final note: purely diagnostic - apply limits for night time

         ZALBT(:)      = 1. - (XSW_WGHT_VIS*(ZFAPAR(:)+ZFAPAR_BS(:)) +             &
                               XSW_WGHT_NIR*(ZFAPIR(:)+ZFAPIR_BS(:)))
         ZSWUP(:)      = ZGLOBAL_SW(:)*ZALBT(:)
         ZALBT(:)      = ZSWUP(:)/MAX(1.E-5, ZGLOBAL_SW(:))
!
         ZDIR_ALB_WITH_SNOW(:,JSWB)=ZALBT(:)
         ZSCA_ALB_WITH_SNOW(:,JSWB)=ZALBT(:) 
!
      END DO
!
   ENDIF
!
! Emissivity
!
  ZEMISSN(:)   = XEMISSN
  ZPSNA(:)     = 0.
  ZSIGMA_F(:)  = 1.0 - MEB_SHIELD_FACTOR(ZLAI,ZPSNA)
  ZSIGMA_FN(:) = 1.0 - MEB_SHIELD_FACTOR(ZLAI,ZPALPHAN)
!
  CALL ISBA_EMIS_MEB(ZPSN, ZPALPHAN, ZSIGMA_F, ZSIGMA_FN,  &
                     ZEMISSN, ZEMIST                       )

!
ELSE
!        
!  * albedo for near-infra-red and visible over snow-covered and snow-flood-free surface
!
  ZALBNIR_WITH_SNOW(:) = ZALBNIR(:) * (1.-ZPSN(:)-ZFF(:)) + ZSNOWALB (:) * ZPSN(:)   
  ZALBVIS_WITH_SNOW(:) = ZALBVIS(:) * (1.-ZPSN(:)-ZFF(:)) + ZSNOWALB (:) * ZPSN(:)  
  ZALBUV_WITH_SNOW (:) = ZALBUV (:) * (1.-ZPSN(:)-ZFF(:)) + ZSNOWALB (:) * ZPSN(:)  
!
!  * snow-flood-covered surface albedo for each wavelength (needed for outputs)
!
  CALL ALBEDO_FROM_NIR_VIS(PSW_BANDS,                                            &
                         ZALBNIR_WITH_SNOW,  ZALBVIS_WITH_SNOW, ZALBUV_WITH_SNOW,&
                         ZDIR_ALB_WITH_SNOW, ZSCA_ALB_WITH_SNOW                  )  
!
  DO JSWB=1,ISWB
    ZDIR_ALB_WITH_SNOW(:,JSWB)=ZDIR_ALB_WITH_SNOW(:,JSWB) + ZFF(:)*ZALBF_DIR(:)
    ZSCA_ALB_WITH_SNOW(:,JSWB)=ZSCA_ALB_WITH_SNOW(:,JSWB) + ZFF(:)*ZALBF_SCA(:)
  ENDDO
!
!-------------------------------------------------------------------------------
!
! longwave computations for outputs (emissivity for radiative scheme)
!
  ZEMIST(:) = (1.-ZPSN(:)-ZFF(:))*ZEMIS(:) + ZPSN(:) * XEMISSN + ZFF(:)*ZEMISF(:)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
! Unpack variable
!
 CALL UNPACK_SAME_RANK(IMASK(:),ZPSNG (:),I%XPSNG  (:,KPATCH),ZPUT0)     
 CALL UNPACK_SAME_RANK(IMASK(:),ZPSNV (:),I%XPSNV  (:,KPATCH),ZPUT0)     
 CALL UNPACK_SAME_RANK(IMASK(:),ZPSN  (:),I%XPSN   (:,KPATCH),ZPUT0)  
 CALL UNPACK_SAME_RANK(IMASK(:),ZEMIST(:),PEMIST (:,KPATCH),ZPUT0)  
 CALL UNPACK_SAME_RANK(IMASK(:),ZDIR_ALB_WITH_SNOW (:,:),I%XDIR_ALB_WITH_SNOW (:,:,KPATCH),ZPUT0)  
 CALL UNPACK_SAME_RANK(IMASK(:),ZSCA_ALB_WITH_SNOW (:,:),I%XSCA_ALB_WITH_SNOW (:,:,KPATCH),ZPUT0)
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_ISBA_N:TREAT_NATURE',1,ZHOOK_HANDLE)
!
END SUBROUTINE TREAT_NATURE
!
SUBROUTINE TREAT_FLOOD(KSIZE,KPATCH,KMASK,PPSNG,PPSNV,PLAI,PVEG,&
               PALBF, PALBF_DIR,PALBF_SCA,PEMISF,PFF)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KSIZE
INTEGER, INTENT(IN) :: KPATCH
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
REAL, DIMENSION(:),  INTENT(IN) :: PPSNG             
REAL, DIMENSION(:),  INTENT(IN) :: PPSNV 
REAL, DIMENSION(:),  INTENT(IN) :: PLAI
REAL, DIMENSION(:),  INTENT(IN) :: PVEG
REAL, DIMENSION(:), INTENT(OUT) :: PALBF_DIR
REAL, DIMENSION(:), INTENT(OUT) :: PALBF_SCA
REAL, DIMENSION(:), INTENT(OUT) :: PALBF
REAL, DIMENSION(:), INTENT(OUT) :: PEMISF   
REAL, DIMENSION(:), INTENT(OUT) :: PFF  
!
REAL, DIMENSION(KSIZE) :: ZTG
REAL, DIMENSION(KSIZE) :: ZZENITH
REAL, DIMENSION(KSIZE) :: ZFFLOOD
REAL, DIMENSION(KSIZE) :: ZFFG   
REAL, DIMENSION(KSIZE) :: ZFFV
REAL, DIMENSION(KSIZE) :: ZFFROZEN
REAL, DIMENSION(KSIZE) :: ZALBEDO
!
REAL, PARAMETER :: ZPUT0 = 0.0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_ISBA_N:TREAT_FLOOD',0,ZHOOK_HANDLE)
!
 CALL PACK_SAME_RANK(KMASK(:),I%XTG(:,1,KPATCH),ZTG(:))
!
 CALL PACK_SAME_RANK(KMASK(:),PZENITH(:),ZZENITH (:))
 CALL PACK_SAME_RANK(KMASK(:),I%XFFLOOD(:),ZFFLOOD (:))
!      
ZFFG(:) = FLOOD_FRAC_GROUND(PPSNG,ZFFLOOD)
ZFFV(:) = FLOOD_FRAC_VEG(PLAI,PPSNV,ZFFLOOD)
PFF (:) = FLOOD_FRAC_NAT(PVEG,ZFFG,ZFFV,ZFFLOOD)
!
ZALBEDO(:) = ALBEDO_TA96(ZZENITH(:))
WHERE(ZFFLOOD==0.0)
  PALBF_DIR (:) = XUNDEF
  PALBF_SCA (:) = XUNDEF
  PALBF     (:) = XUNDEF
  PEMISF    (:) = XUNDEF
  ZFFROZEN  (:) = 0.0
ELSEWHERE
  WHERE(ZTG(:)>=XTT)
    PALBF_DIR (:) = ZALBEDO(:)
    PALBF_SCA (:) = XALBSCA_WAT
    PEMISF    (:) = XEMISWAT
    ZFFROZEN  (:) = 0.0
  ELSEWHERE
    PALBF_DIR (:) = XALBWATICE
    PALBF_SCA (:) = XALBWATICE
    PEMISF    (:) = XEMISWATICE
    ZFFROZEN  (:) = 1.0
  END WHERE
  PALBF(:)=0.5*(PALBF_DIR(:)+PALBF_SCA(:))
ENDWHERE
!
 CALL UNPACK_SAME_RANK(KMASK(:),ZFFG    (:),I%XFFG    (:,KPATCH),ZPUT0)     
 CALL UNPACK_SAME_RANK(KMASK(:),ZFFV    (:),I%XFFV    (:,KPATCH),ZPUT0)     
 CALL UNPACK_SAME_RANK(KMASK(:),ZFFROZEN(:),I%XFFROZEN(:,KPATCH),ZPUT0)     
 CALL UNPACK_SAME_RANK(KMASK(:),PFF     (:),I%XFF     (:,KPATCH),ZPUT0) 
 CALL UNPACK_SAME_RANK(KMASK(:),PEMISF  (:),I%XEMISF  (:,KPATCH),XUNDEF)
 CALL UNPACK_SAME_RANK(KMASK(:),PALBF   (:),I%XALBF   (:,KPATCH),XUNDEF)     
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_ISBA_N:TREAT_FLOOD',1,ZHOOK_HANDLE)
!
END SUBROUTINE TREAT_FLOOD
!
END SUBROUTINE UPDATE_RAD_ISBA_n
