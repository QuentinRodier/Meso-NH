!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE UPDATE_RAD_ISBA_n(OFLOOD,HSNOW,PZENITH,PSW_BANDS,PVEG,PLAI,PZ0, &
                               PALBNIR,PALBVIS,PALBUV,PEMIS,               &
                               PDIR_ALB_WITH_SNOW,PSCA_ALB_WITH_SNOW,PEMIST)  
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
!!------------------------------------------------------------------
!
USE MODD_TYPE_SNOW
!
USE MODD_ISBA_n,    ONLY : NSIZE_NATURE_P,NR_NATURE_P,      &
                             NPATCH,XTG,TSNOW,XPSNG,XPSNV_A,  &
                             XPSNV,XPSN,XFFLOOD,XFF,XFFG,XFFV,&
                             XALBF,XEMISF,XDIR_ALB_WITH_SNOW, &
                             XSCA_ALB_WITH_SNOW,XFFROZEN  
!
USE MODD_CSTS,      ONLY : XTT
USE MODD_SURF_PAR,  ONLY : XUNDEF
USE MODD_SNOW_PAR,  ONLY : XRHOSMIN_ES,XRHOSMAX_ES,XSNOWDMIN,XEMISSN
USE MODD_WATER_PAR, ONLY : XALBSCA_WAT, XEMISWAT, XALBWATICE, XEMISWATICE 
!
USE MODE_SURF_FLOOD_FRAC
USE MODE_SURF_SNOW_FRAC      
!
USE MODI_ALBEDO_TA96
USE MODI_ALBEDO_FROM_NIR_VIS
USE MODI_PACK_SAME_RANK
USE MODI_UNPACK_SAME_RANK
USE MODI_ISBA_SNOW_FRAC
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
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
!
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PDIR_ALB_WITH_SNOW ! Total direct albedo at t+1
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PSCA_ALB_WITH_SNOW ! Total diffuse albedo at t+1
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PEMIST             ! Total emissivity at t+1
!
!*      0.2    declarations of local variables
!
INTEGER :: JPATCH, ISWB
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
! Initialization
!-------------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_ISBA_N',0,ZHOOK_HANDLE)
ISWB   = SIZE(PSW_BANDS)
!
!-------------------------------------------------------------------------------------
!Patch loop
!
DO JPATCH=1,NPATCH
  !
  IF(NSIZE_NATURE_P(JPATCH)>0) CALL TREAT_NATURE(NSIZE_NATURE_P(JPATCH),JPATCH)
  !
ENDDO
!-------------------------------------------------------------------------------
!
!Update albedo with snow for the next time step
!
PDIR_ALB_WITH_SNOW(:,:,:)=XDIR_ALB_WITH_SNOW (:,:,:)
PSCA_ALB_WITH_SNOW(:,:,:)=XSCA_ALB_WITH_SNOW (:,:,:)
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
INTEGER, DIMENSION(KSIZE) :: IMASK
!
REAL, DIMENSION(KSIZE,SIZE(TSNOW%WSNOW,2)) :: ZLAYERSWE
REAL, DIMENSION(KSIZE,SIZE(TSNOW%WSNOW,2)) :: ZLAYERRHO
!

REAL, DIMENSION(KSIZE,ISWB) :: ZDIR_ALB_WITH_SNOW
REAL, DIMENSION(KSIZE,ISWB) :: ZSCA_ALB_WITH_SNOW
!
REAL, DIMENSION(KSIZE) :: ZSNOWALB          
REAL, DIMENSION(KSIZE) :: ZLAI             
REAL, DIMENSION(KSIZE) :: ZZ0               
REAL, DIMENSION(KSIZE) :: ZVEG
REAL, DIMENSION(KSIZE) :: ZEMIS    
REAL, DIMENSION(KSIZE) :: ZALBNIR           
REAL, DIMENSION(KSIZE) :: ZALBVIS           
REAL, DIMENSION(KSIZE) :: ZALBUV  
!
REAL, DIMENSION(KSIZE) :: ZPSN
REAL, DIMENSION(KSIZE) :: ZPSNV_A
REAL, DIMENSION(KSIZE) :: ZPSNG             
REAL, DIMENSION(KSIZE) :: ZPSNV 
!
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
!
REAL, PARAMETER :: ZPUT0 = 0.0
INTEGER  :: JSWB
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_ISBA_N:TREAT_NATURE',0,ZHOOK_HANDLE)
!
IMASK(:)=NR_NATURE_P(1:KSIZE,KPATCH)
!
 CALL PACK_SAME_RANK(IMASK(:),TSNOW%WSNOW(:,:,KPATCH),ZLAYERSWE(:,:))
 CALL PACK_SAME_RANK(IMASK(:),TSNOW%RHO  (:,:,KPATCH),ZLAYERRHO(:,:))
!  
 CALL PACK_SAME_RANK(IMASK(:),TSNOW%ALB  (:,KPATCH),ZSNOWALB(:))
 CALL PACK_SAME_RANK(IMASK(:),PLAI       (:,KPATCH),ZLAI    (:))
 CALL PACK_SAME_RANK(IMASK(:),PZ0        (:,KPATCH),ZZ0     (:))
 CALL PACK_SAME_RANK(IMASK(:),PVEG       (:,KPATCH),ZVEG    (:))
 CALL PACK_SAME_RANK(IMASK(:),PEMIS      (:,KPATCH),ZEMIS   (:))
 CALL PACK_SAME_RANK(IMASK(:),PALBNIR    (:,KPATCH),ZALBNIR (:))
 CALL PACK_SAME_RANK(IMASK(:),PALBVIS    (:,KPATCH),ZALBVIS (:))
 CALL PACK_SAME_RANK(IMASK(:),PALBUV     (:,KPATCH),ZALBUV  (:))
!   
!-------------------------------------------------------------------------------
!
 CALL ISBA_SNOW_FRAC(HSNOW, ZLAYERSWE, ZLAYERRHO, ZSNOWALB,    &
         ZVEG, ZLAI, ZZ0,ZPSN(:), ZPSNV_A(:), ZPSNG(:), ZPSNV(:) )  
IF ( HSNOW=='EBA' ) CALL UNPACK_SAME_RANK(IMASK(:),ZPSNV_A(:),XPSNV_A(:,KPATCH),ZPUT0) 
!
!-------------------------------------------------------------------------------
!
! Flood fractions and properties
!
IF(OFLOOD)THEN   
  CALL TREAT_FLOOD(KSIZE,KPATCH,IMASK,ZPSNG,ZPSNV,ZLAI,ZVEG,&
                ZALBF_DIR,ZALBF_SCA,ZEMISF,ZFF)
ELSE
  ZALBF_DIR (:)=0.0
  ZALBF_SCA (:)=0.0
  ZEMISF    (:)=0.0
  ZFF       (:)=0.0
ENDIF        
!-------------------------------------------------------------------------------
!
!* albedo for near-infra-red and visible over snow-covered and snow-flood-free surface
!
ZALBNIR_WITH_SNOW(:) = ZALBNIR(:) * (1.-ZPSN(:)-ZFF(:)) + ZSNOWALB (:) * ZPSN(:)   
ZALBVIS_WITH_SNOW(:) = ZALBVIS(:) * (1.-ZPSN(:)-ZFF(:)) + ZSNOWALB (:) * ZPSN(:)  
ZALBUV_WITH_SNOW (:) = ZALBUV (:) * (1.-ZPSN(:)-ZFF(:)) + ZSNOWALB (:) * ZPSN(:)  
!
!* snow-flood-covered surface albedo for each wavelength (needed for outputs)
!
 CALL ALBEDO_FROM_NIR_VIS(PSW_BANDS,                                              &
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
!-------------------------------------------------------------------------------
!
! Unpack variable
!
 CALL UNPACK_SAME_RANK(IMASK(:),ZPSNG (:),XPSNG  (:,KPATCH),ZPUT0)     
 CALL UNPACK_SAME_RANK(IMASK(:),ZPSNV (:),XPSNV  (:,KPATCH),ZPUT0)     
 CALL UNPACK_SAME_RANK(IMASK(:),ZPSN  (:),XPSN   (:,KPATCH),ZPUT0)  
 CALL UNPACK_SAME_RANK(IMASK(:),ZEMIST(:),PEMIST (:,KPATCH),ZPUT0)  
 CALL UNPACK_SAME_RANK(IMASK(:),ZDIR_ALB_WITH_SNOW (:,:),XDIR_ALB_WITH_SNOW (:,:,KPATCH),ZPUT0)  
 CALL UNPACK_SAME_RANK(IMASK(:),ZSCA_ALB_WITH_SNOW (:,:),XSCA_ALB_WITH_SNOW (:,:,KPATCH),ZPUT0)
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_ISBA_N:TREAT_NATURE',1,ZHOOK_HANDLE)
!
END SUBROUTINE TREAT_NATURE
!
SUBROUTINE TREAT_FLOOD(KSIZE,KPATCH,KMASK,PPSNG,PPSNV,PLAI,PVEG,&
               PALBF_DIR,PALBF_SCA,PEMISF,PFF)
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
REAL, DIMENSION(:), INTENT(OUT) :: PEMISF   
REAL, DIMENSION(:), INTENT(OUT) :: PFF  
!
REAL, DIMENSION(KSIZE) :: ZTG
REAL, DIMENSION(KSIZE) :: ZZENITH
REAL, DIMENSION(KSIZE) :: ZFFLOOD
REAL, DIMENSION(KSIZE) :: ZFFG   
REAL, DIMENSION(KSIZE) :: ZFFV
REAL, DIMENSION(KSIZE) :: ZALBF
REAL, DIMENSION(KSIZE) :: ZFFROZEN
REAL, DIMENSION(KSIZE) :: ZALBEDO
!
REAL, PARAMETER :: ZPUT0 = 0.0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_ISBA_N:TREAT_FLOOD',0,ZHOOK_HANDLE)
!
 CALL PACK_SAME_RANK(KMASK(:),XTG(:,1,KPATCH),ZTG(:))
!
 CALL PACK_SAME_RANK(KMASK(:),PZENITH(:),ZZENITH (:))
 CALL PACK_SAME_RANK(KMASK(:),XFFLOOD(:),ZFFLOOD (:))
!      
ZFFG(:) = FLOOD_FRAC_GROUND(PPSNG,ZFFLOOD)
ZFFV(:) = FLOOD_FRAC_VEG(PLAI,PPSNV,ZFFLOOD)
PFF (:) = FLOOD_FRAC_NAT(PVEG,ZFFG,ZFFV,ZFFLOOD)
!
ZALBEDO(:) = ALBEDO_TA96(ZZENITH(:))
WHERE(ZFFLOOD==0.0)
  PALBF_DIR (:) = XUNDEF
  PALBF_SCA (:) = XUNDEF
  ZALBF     (:) = XUNDEF
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
  ZALBF(:)=0.5*(PALBF_DIR(:)+PALBF_SCA(:))
ENDWHERE
!
 CALL UNPACK_SAME_RANK(KMASK(:),ZFFG    (:),XFFG    (:,KPATCH),ZPUT0)     
 CALL UNPACK_SAME_RANK(KMASK(:),ZFFV    (:),XFFV    (:,KPATCH),ZPUT0)     
 CALL UNPACK_SAME_RANK(KMASK(:),ZFFROZEN(:),XFFROZEN(:,KPATCH),ZPUT0)     
 CALL UNPACK_SAME_RANK(KMASK(:),PFF     (:),XFF     (:,KPATCH),ZPUT0) 
 CALL UNPACK_SAME_RANK(KMASK(:),PEMISF  (:),XEMISF  (:,KPATCH),XUNDEF)
 CALL UNPACK_SAME_RANK(KMASK(:),ZALBF   (:),XALBF   (:,KPATCH),XUNDEF)     
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_ISBA_N:TREAT_FLOOD',1,ZHOOK_HANDLE)
!
END SUBROUTINE TREAT_FLOOD
!
END SUBROUTINE UPDATE_RAD_ISBA_n
