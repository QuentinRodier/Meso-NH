!      ###########
MODULE MODI_ALBEDO
!      ###########
!
INTERFACE ALBEDO
!
!
      SUBROUTINE ALBEDO_1D(HALBEDO,                                      &
                             PALBVIS_VEG,PALBNIR_VEG,PALBUV_VEG,PVEG,      &
                             PALBVIS_SOIL,PALBNIR_SOIL,PALBUV_SOIL,        &
                             PALBVIS_ECO ,PALBNIR_ECO, PALBUV_ECO,         &
                             PSNOW, OMASK                                  )  
!
!
!*      0.1    declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*),       INTENT(IN)   :: HALBEDO
! Albedo dependance wxith surface soil water content
!   "EVOL" = albedo evolves with soil wetness
!   "DRY " = constant albedo value for dry soil
!   "WET " = constant albedo value for wet soil
!   "MEAN" = constant albedo value for medium soil wetness
!
REAL, DIMENSION(:), INTENT(IN)  :: PALBVIS_VEG ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(IN)  :: PALBNIR_VEG ! albedo of the vegetation
REAL, DIMENSION(:), INTENT(IN)  :: PALBUV_VEG  !
REAL, DIMENSION(:), INTENT(IN)  :: PVEG        ! fraction of vegetation
REAL, DIMENSION(:), INTENT(IN)  :: PALBVIS_SOIL! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(IN)  :: PALBNIR_SOIL! soil albedo
REAL, DIMENSION(:), INTENT(IN)  :: PALBUV_SOIL !
!
REAL, DIMENSION(:), INTENT(INOUT) :: PALBVIS_ECO ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(INOUT) :: PALBNIR_ECO ! averaged albedo
REAL, DIMENSION(:), INTENT(INOUT) :: PALBUV_ECO  !
REAL,    DIMENSION(:), INTENT(IN), OPTIONAL  :: PSNOW ! fraction of permanent snow and ice
LOGICAL, DIMENSION(:), INTENT(IN), OPTIONAL  :: OMASK ! mask where computations are done
!
END SUBROUTINE ALBEDO_1D
!
!
      SUBROUTINE ALBEDO_1D_PATCH(HALBEDO,                                &
                             PALBVIS_VEG,PALBNIR_VEG,PALBUV_VEG,PVEG,      &
                             PALBVIS_SOIL,PALBNIR_SOIL,PALBUV_SOIL,        &
                             PALBVIS_ECO ,PALBNIR_ECO, PALBUV_ECO,         &
                             PVEGTYPE, OMASK                               )  
!
!
!*      0.1    declarations of arguments
!              -------------------------
!
!
 CHARACTER(LEN=*),       INTENT(IN)   :: HALBEDO
! Albedo dependance wxith surface soil water content
!   "EVOL" = albedo evolves with soil wetness
!   "DRY " = constant albedo value for dry soil
!   "WET " = constant albedo value for wet soil
!   "MEAN" = constant albedo value for medium soil wetness
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PALBVIS_VEG ! visible, near infra-red and UV
REAL, DIMENSION(:,:), INTENT(IN)  :: PALBNIR_VEG ! albedo of the vegetation
REAL, DIMENSION(:,:), INTENT(IN)  :: PALBUV_VEG  !
REAL, DIMENSION(:,:), INTENT(IN)  :: PVEG        ! fraction of vegetation
REAL, DIMENSION(:,:), INTENT(IN)  :: PALBVIS_SOIL! visible, near infra-red and UV
REAL, DIMENSION(:,:), INTENT(IN)  :: PALBNIR_SOIL! soil albedo
REAL, DIMENSION(:,:), INTENT(IN)  :: PALBUV_SOIL !
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PALBVIS_ECO ! visible, near infra-red and UV
REAL, DIMENSION(:,:), INTENT(INOUT) :: PALBNIR_ECO ! averaged albedo
REAL, DIMENSION(:,:), INTENT(INOUT) :: PALBUV_ECO  !
REAL,    DIMENSION(:,:), INTENT(IN), OPTIONAL :: PVEGTYPE ! vegetation type
LOGICAL, DIMENSION(:),   INTENT(IN), OPTIONAL :: OMASK    ! mask where computations are done
!
END SUBROUTINE ALBEDO_1D_PATCH
!
END INTERFACE
!
END MODULE MODI_ALBEDO
!
!     ####################################################################
      SUBROUTINE ALBEDO_1D(HALBEDO,                                      &
                             PALBVIS_VEG,PALBNIR_VEG,PALBUV_VEG,PVEG,      &
                             PALBVIS_SOIL,PALBNIR_SOIL,PALBUV_SOIL,        &
                             PALBVIS_ECO ,PALBNIR_ECO, PALBUV_ECO,         &
                             PSNOW, OMASK                                  )  
!     ####################################################################
!
!!****  *ALBEDO*  
!!
!!    PURPOSE
!!    -------
!       computes the albedo of the natural continental parts, from
! vegetation albedo and soil albedo.
! Soil albedo is estimated from sand fraction.
! A correction due to the soil humidity is used.
!
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!	V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    17/12/99 
!!                  01/2004  Externalization (V. Masson)
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SNOW_PAR, ONLY : XANSMAX
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*),       INTENT(IN)   :: HALBEDO
! Albedo dependance wxith surface soil water content
!   "EVOL" = albedo evolves with soil wetness
!   "DRY " = constant albedo value for dry soil
!   "WET " = constant albedo value for wet soil
!   "MEAN" = constant albedo value for medium soil wetness
!
REAL, DIMENSION(:), INTENT(IN)  :: PALBVIS_VEG ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(IN)  :: PALBNIR_VEG ! albedo of the vegetation
REAL, DIMENSION(:), INTENT(IN)  :: PALBUV_VEG  !
REAL, DIMENSION(:), INTENT(IN)  :: PVEG        ! fraction of vegetation
REAL, DIMENSION(:), INTENT(IN)  :: PALBVIS_SOIL! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(IN)  :: PALBNIR_SOIL! soil albedo
REAL, DIMENSION(:), INTENT(IN)  :: PALBUV_SOIL !
!
REAL, DIMENSION(:), INTENT(INOUT) :: PALBVIS_ECO ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(INOUT) :: PALBNIR_ECO ! averaged albedo
REAL, DIMENSION(:), INTENT(INOUT) :: PALBUV_ECO  !
REAL,    DIMENSION(:), INTENT(IN), OPTIONAL  :: PSNOW ! fraction of permanent snow and ice
LOGICAL, DIMENSION(:), INTENT(IN), OPTIONAL  :: OMASK ! mask where computations are done
!
!*      0.2    declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(SIZE(PVEG)) :: ZSNOW
LOGICAL, DIMENSION(SIZE(PVEG)) :: GMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_ALBEDO:ALBEDO_1D',0,ZHOOK_HANDLE)
IF (HALBEDO=='USER' .AND. LHOOK) CALL DR_HOOK('MODI_ALBEDO:ALBEDO_1D',1,ZHOOK_HANDLE)
IF (HALBEDO=='USER') RETURN
!
GMASK=.TRUE.
IF (PRESENT(OMASK)) GMASK=OMASK
!
ZSNOW(:) = 0.
IF (PRESENT(PSNOW)) ZSNOW(:) = PSNOW(:)
!
WHERE (GMASK(:)) 
  PALBVIS_ECO (:) = ( (1.-PVEG(:)) * PALBVIS_SOIL(:) &
                       +    PVEG(:)  * PALBVIS_VEG (:))&
                     * (1-ZSNOW(:))                    &
                    + XANSMAX  * ZSNOW(:)    
!
  PALBNIR_ECO (:) = ( (1.-PVEG(:)) * PALBNIR_SOIL(:) &
                       +    PVEG(:)  * PALBNIR_VEG (:))&
                     * (1-ZSNOW(:))                    &
                    + XANSMAX  * ZSNOW(:)    
!
  PALBUV_ECO (:)  = ( (1.-PVEG(:)) * PALBUV_SOIL(:)  &
                       +    PVEG(:)  * PALBUV_VEG (:)) &
                     * (1-ZSNOW(:))                    &
                    + XANSMAX  * ZSNOW(:)    
END WHERE
IF (LHOOK) CALL DR_HOOK('MODI_ALBEDO:ALBEDO_1D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE ALBEDO_1D
!
!     ####################################################################
      SUBROUTINE ALBEDO_1D_PATCH(HALBEDO,                                &
                             PALBVIS_VEG,PALBNIR_VEG,PALBUV_VEG,PVEG,      &
                             PALBVIS_SOIL,PALBNIR_SOIL,PALBUV_SOIL,        &
                             PALBVIS_ECO ,PALBNIR_ECO, PALBUV_ECO,         &
                             PVEGTYPE, OMASK                               )  
!     ####################################################################
!
!!****  *ALBEDO*  
!!
!!    PURPOSE
!!    -------
!  computes the albedo of for different types (patches) 
! of natural continental parts, from
! vegetation albedo and soil albedo.
! Soil albedo is estimated from sand fraction.
! A correction due to the soil humidity is used.
!
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!	F.Solmon  /  V. Masson          
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     
!!                  01/2004  Externalization (V. Masson)
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
USE MODD_SNOW_PAR,       ONLY : XANSMAX
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_VEGTYPE_TO_PATCH
USE MODI_SURF_PATCH
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*),       INTENT(IN)   :: HALBEDO
! Albedo dependance wxith surface soil water content
!   "EVOL" = albedo evolves with soil wetness
!   "DRY " = constant albedo value for dry soil
!   "WET " = constant albedo value for wet soil
!   "MEAN" = constant albedo value for medium soil wetness
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PALBVIS_VEG ! visible, near infra-red and UV
REAL, DIMENSION(:,:), INTENT(IN)  :: PALBNIR_VEG ! albedo of the vegetation
REAL, DIMENSION(:,:), INTENT(IN)  :: PALBUV_VEG  !
REAL, DIMENSION(:,:), INTENT(IN)  :: PVEG        ! fraction of vegetation
REAL, DIMENSION(:,:), INTENT(IN)  :: PALBVIS_SOIL! visible, near infra-red and UV
REAL, DIMENSION(:,:), INTENT(IN)  :: PALBNIR_SOIL! soil albedo
REAL, DIMENSION(:,:), INTENT(IN)  :: PALBUV_SOIL !
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PALBVIS_ECO ! visible, near infra-red and UV
REAL, DIMENSION(:,:), INTENT(INOUT) :: PALBNIR_ECO ! averaged albedo
REAL, DIMENSION(:,:), INTENT(INOUT) :: PALBUV_ECO  !
REAL,    DIMENSION(:,:), INTENT(IN), OPTIONAL :: PVEGTYPE ! vegetation type
LOGICAL, DIMENSION(:),   INTENT(IN), OPTIONAL :: OMASK    ! mask where computations are done
!
!
!*      0.2    declarations of local variables
!              -------------------------------
!
LOGICAL, DIMENSION(SIZE(PVEG,1)) :: GMASK
!
REAL, DIMENSION(SIZE(PVEG,1),SIZE(PVEG,2))  ::ZPATCH, ZSNOWPATCH 
INTEGER :: ISNOWPATCH !patch index for snow 
INTEGER :: IPATCH     ! number of patches
INTEGER :: JPATCH     !loop index for patches
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_ALBEDO:ALBEDO_1D_PATCH',0,ZHOOK_HANDLE)
IF (HALBEDO=='USER' .AND. LHOOK) CALL DR_HOOK('MODI_ALBEDO:ALBEDO_1D_PATCH',1,ZHOOK_HANDLE)
IF (HALBEDO=='USER') RETURN
!
GMASK(:) = .TRUE.
IF (PRESENT(OMASK)) GMASK(:) = OMASK(:)
!
IPATCH = SIZE(PVEG,2)

DO JPATCH=1,IPATCH
  WHERE (GMASK(:))
    PALBVIS_ECO (:,JPATCH) = XUNDEF
    PALBNIR_ECO (:,JPATCH) = XUNDEF
    PALBUV_ECO  (:,JPATCH) = XUNDEF
  END WHERE
END DO
!
!
!
ZSNOWPATCH (:,:) =0.
!
IF (PRESENT(PVEGTYPE)) THEN
  ! calculation of patch surfaces  (weights for average)
  CALL SURF_PATCH(IPATCH,PVEGTYPE,ZPATCH)
  ! permanent snow fraction in the corresponding patch
  ISNOWPATCH= VEGTYPE_TO_PATCH (NVT_SNOW,IPATCH)
  WHERE(GMASK(:) .AND. ZPATCH(:,ISNOWPATCH)>0.)
    ZSNOWPATCH (:,ISNOWPATCH)=PVEGTYPE(:,NVT_SNOW)/ZPATCH(:,ISNOWPATCH)
  END WHERE
END IF
!

DO JPATCH=1,IPATCH
  WHERE (GMASK(:) .AND. PVEG(:,JPATCH)/=XUNDEF)

    PALBVIS_ECO (:,JPATCH) =(  (1.-PVEG(:,JPATCH)) * PALBVIS_SOIL(:,JPATCH)  &
           +    PVEG(:,JPATCH)  * PALBVIS_VEG (:,JPATCH))                      &
           * (1-ZSNOWPATCH(:,JPATCH))                                          &
           + XANSMAX  * ZSNOWPATCH(:,JPATCH)   
    !
    PALBNIR_ECO (:,JPATCH) =(  (1.-PVEG(:,JPATCH)) * PALBNIR_SOIL(:,JPATCH)  &
           +      PVEG(:,JPATCH)  * PALBNIR_VEG (:,JPATCH))                    &
           * (1-ZSNOWPATCH(:,JPATCH))                                          &
           + XANSMAX  * ZSNOWPATCH(:,JPATCH)   
    !
    PALBUV_ECO  (:,JPATCH) =(  (1.-PVEG(:,JPATCH)) * PALBUV_SOIL (:,JPATCH)  &
           +      PVEG(:,JPATCH)  * PALBUV_VEG  (:,JPATCH))                    &
           * (1-ZSNOWPATCH(:,JPATCH))                                          &
           + XANSMAX  * ZSNOWPATCH(:,JPATCH)   
  END WHERE
END DO
IF (LHOOK) CALL DR_HOOK('MODI_ALBEDO:ALBEDO_1D_PATCH',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE ALBEDO_1D_PATCH
