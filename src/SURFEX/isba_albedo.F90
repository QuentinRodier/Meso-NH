!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE ISBA_ALBEDO(HSNOW, OTR_ML,                               &
                               PDIR_SW, PSCA_SW, PSW_BANDS, KSW,          &
                               PALBNIR, PALBVIS, PALBUV,                  &
                               PALBNIR_VEG, PALBVIS_VEG, PALBUV_VEG,      &
                               PALBNIR_SOIL, PALBVIS_SOIL, PALBUV_SOIL,   &
                               PSNOWALB, PPSNV, PPSNG, PFALB, PFFV, PFFG, &
                               PGLOBAL_SW, PSNOWFREE_ALB,                 &
                               PSNOWFREE_ALB_VEG, PSNOWFREE_ALB_SOIL,     &
                               PALBNIR_TVEG, PALBVIS_TVEG,                &
                               PALBNIR_TSOIL, PALBVIS_TSOIL             )  
!     ##########################################################################
!
!!****  *ISBA_ALBEDO*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates grid-averaged albedo and emissivity (according to snow scheme)
!         
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      
!!    AUTHOR
!!    ------
!!
!!	S. Belair           * Meteo-France *
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_SURF_PAR,     ONLY : XUNDEF
!
USE MODI_ALBEDO_FROM_NIR_VIS
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*)    , INTENT(IN)   :: HSNOW      ! ISBA snow scheme
LOGICAL,              INTENT(IN)   :: OTR_ML
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PDIR_SW            ! direct incoming solar radiation
REAL, DIMENSION(:,:), INTENT(IN)   :: PSCA_SW            ! diffus incoming solar radiation
REAL, DIMENSION(:)  , INTENT(IN)   :: PSW_BANDS          ! mean wavelength of each shortwave band (m)
INTEGER,              INTENT(IN)   :: KSW                ! number of short-wave spectral bands
REAL, DIMENSION(:)  , INTENT(IN)   :: PALBNIR            ! nearIR  total albedo
REAL, DIMENSION(:)  , INTENT(IN)   :: PALBVIS            ! visible total albedo
REAL, DIMENSION(:)  , INTENT(IN)   :: PALBUV             ! UV      total albedo
REAL, DIMENSION(:)  , INTENT(IN)   :: PALBNIR_VEG        ! nearIR  veg   albedo
REAL, DIMENSION(:)  , INTENT(IN)   :: PALBVIS_VEG        ! visible veg   albedo
REAL, DIMENSION(:)  , INTENT(IN)   :: PALBUV_VEG         ! UV      veg   albedo
REAL, DIMENSION(:)  , INTENT(IN)   :: PALBNIR_SOIL       ! nearIR  soil  albedo
REAL, DIMENSION(:)  , INTENT(IN)   :: PALBVIS_SOIL       ! visible soil  albedo
REAL, DIMENSION(:)  , INTENT(IN)   :: PALBUV_SOIL        ! UV      soil  albedo
REAL, DIMENSION(:)  , INTENT(IN)   :: PSNOWALB           ! Snow albedo
REAL, DIMENSION(:)  , INTENT(IN)   :: PPSNV              ! fraction of the the veg.
!                                                        ! covered by snow
REAL, DIMENSION(:)  , INTENT(IN)   :: PPSNG              ! fraction of the the bare
!                                                        ! ground covered by snow
REAL, DIMENSION(:)  , INTENT(IN)   :: PFALB              ! Floodplain albedo
REAL, DIMENSION(:)  , INTENT(IN)   :: PFFV               ! Floodplain fraction over vegetation
REAL, DIMENSION(:)  , INTENT(IN)   :: PFFG               ! Floodplain fraction over the ground
!
REAL, DIMENSION(:)  , INTENT(OUT)  :: PGLOBAL_SW         ! global incoming SW rad.
REAL, DIMENSION(:)  , INTENT(OUT)  :: PSNOWFREE_ALB      !snow free albedo 
REAL, DIMENSION(:)  , INTENT(OUT)  :: PSNOWFREE_ALB_VEG  !snow free albedo of vegetation for EBA
REAL, DIMENSION(:)  , INTENT(OUT)  :: PSNOWFREE_ALB_SOIL !snow free albedo of soil for EBA option
REAL, DIMENSION(:)  , INTENT(OUT)  :: PALBNIR_TVEG       ! nearIR  veg tot albedo
REAL, DIMENSION(:)  , INTENT(OUT)  :: PALBVIS_TVEG       ! visible veg tot albedo
REAL, DIMENSION(:)  , INTENT(OUT)  :: PALBNIR_TSOIL      ! nearIR  soil tot albedo
REAL, DIMENSION(:)  , INTENT(OUT)  :: PALBVIS_TSOIL      ! visible soil tot albedo
!
!-------------------------------------------------------------------------------
!
!*      0.     Local variables
!              ---------------
!
INTEGER                          :: JLAYER
INTEGER                          :: JSWB
REAL, DIMENSION(SIZE(PALBNIR))      :: ZSW_UP
REAL, DIMENSION(SIZE(PALBNIR),KSW)  :: ZDIR_ALB_WITHOUT_SNOW
REAL, DIMENSION(SIZE(PALBNIR),KSW)  :: ZSCA_ALB_WITHOUT_SNOW
REAL, DIMENSION(SIZE(PALBNIR),KSW)  :: ZDIR_ALB_VEG_WITHOUT_SNOW
REAL, DIMENSION(SIZE(PALBNIR),KSW)  :: ZSCA_ALB_VEG_WITHOUT_SNOW
REAL, DIMENSION(SIZE(PALBNIR),KSW)  :: ZDIR_ALB_SOIL_WITHOUT_SNOW
REAL, DIMENSION(SIZE(PALBNIR),KSW)  :: ZSCA_ALB_SOIL_WITHOUT_SNOW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*      2.     Compute snow-free albedo
!              ------------------------
!
!* Snow-free surface albedo for each wavelength
!
IF (LHOOK) CALL DR_HOOK('ISBA_ALBEDO',0,ZHOOK_HANDLE)
!
IF (OTR_ML) THEN
  PALBNIR_TVEG (:) = ( 1.-PPSNV(:)-PFFV(:))*PALBNIR_VEG(:)  + PPSNV(:)*PSNOWALB(:) + PFFV(:)*PFALB(:)
  PALBNIR_TSOIL(:) = ( 1.-PPSNG(:)-PFFG(:))*PALBNIR_SOIL(:) + PPSNG(:)*PSNOWALB(:) + PFFG(:)*PFALB(:)   
  PALBVIS_TVEG (:) = ( 1.-PPSNV(:)-PFFV(:))*PALBVIS_VEG(:)  + PPSNV(:)*PSNOWALB(:) + PFFV(:)*PFALB(:)
  PALBVIS_TSOIL(:) = ( 1.-PPSNG(:)-PFFG(:))*PALBVIS_SOIL(:) + PPSNG(:)*PSNOWALB(:) + PFFG(:)*PFALB(:)
ENDIF
!
 CALL ALBEDO_FROM_NIR_VIS(PSW_BANDS, PALBNIR, PALBVIS, PALBUV,         &
                           ZDIR_ALB_WITHOUT_SNOW, ZSCA_ALB_WITHOUT_SNOW )  
!
!* total shortwave incoming radiation
!
  PGLOBAL_SW(:) = 0.
  DO JSWB=1,KSW
    PGLOBAL_SW(:) = PGLOBAL_SW(:) + (PDIR_SW(:,JSWB) + PSCA_SW(:,JSWB))
  END DO
!
!* snow-free global albedo (needed by ISBA)
!
  ZSW_UP(:) = 0. 
  DO JSWB=1,KSW
    ZSW_UP(:) =  ZSW_UP(:)                                       &
                 + ZDIR_ALB_WITHOUT_SNOW(:,JSWB) * PDIR_SW(:,JSWB) &
                 + ZSCA_ALB_WITHOUT_SNOW(:,JSWB) * PSCA_SW(:,JSWB)  
  END DO
  PSNOWFREE_ALB(:) = XUNDEF
  WHERE(PGLOBAL_SW(:)>0.)  
       PSNOWFREE_ALB(:) = ZSW_UP(:) / PGLOBAL_SW(:)
  ELSEWHERE
       PSNOWFREE_ALB(:) = ZDIR_ALB_WITHOUT_SNOW(:,1)
  END WHERE
!
  IF(HSNOW == 'EBA') THEN
     CALL ALBEDO_FROM_NIR_VIS(PSW_BANDS,            &
               PALBNIR_VEG, PALBVIS_VEG, PALBUV_VEG, &
               ZDIR_ALB_VEG_WITHOUT_SNOW,            &
               ZSCA_ALB_VEG_WITHOUT_SNOW             )  
     ZSW_UP(:) = 0.
     DO JSWB=1,KSW
        ZSW_UP(:) =  ZSW_UP(:)                                           &
                     + ZDIR_ALB_VEG_WITHOUT_SNOW(:,JSWB) * PDIR_SW(:,JSWB) &
                     + ZSCA_ALB_VEG_WITHOUT_SNOW(:,JSWB) * PSCA_SW(:,JSWB)  
     END DO
     PSNOWFREE_ALB_VEG(:) = XUNDEF
     WHERE(PGLOBAL_SW(:)>0.)  PSNOWFREE_ALB_VEG(:) = ZSW_UP(:) / PGLOBAL_SW(:)
!
     CALL ALBEDO_FROM_NIR_VIS(PSW_BANDS,               &
               PALBNIR_SOIL, PALBVIS_SOIL, PALBUV_SOIL, &
               ZDIR_ALB_SOIL_WITHOUT_SNOW,              &
               ZSCA_ALB_SOIL_WITHOUT_SNOW               )  
     ZSW_UP(:) = 0.
     DO JSWB=1,KSW
        ZSW_UP(:) =  ZSW_UP(:)                                            &
                     + ZDIR_ALB_SOIL_WITHOUT_SNOW(:,JSWB) * PDIR_SW(:,JSWB) &
                     + ZSCA_ALB_SOIL_WITHOUT_SNOW(:,JSWB) * PSCA_SW(:,JSWB)  
     END DO
     PSNOWFREE_ALB_SOIL(:) = XUNDEF
     WHERE(PGLOBAL_SW(:)>0.)  PSNOWFREE_ALB_SOIL(:) = ZSW_UP(:) / PGLOBAL_SW(:)             
  ENDIF
IF (LHOOK) CALL DR_HOOK('ISBA_ALBEDO',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ISBA_ALBEDO
