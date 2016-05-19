!      ###########
MODULE MODI_SOIL_ALBEDO
!      ###########
!
INTERFACE SOIL_ALBEDO
!
!
      SUBROUTINE SOIL_ALBEDO_1D(HALBEDO,                             &
                             PWSAT,PWG1,                               &
                             PALBVIS_DRY,PALBNIR_DRY,PALBUV_DRY,       &
                             PALBVIS_WET,PALBNIR_WET,PALBUV_WET,       &
                             PALBVIS_SOIL, PALBNIR_SOIL,PALBUV_SOIL    )  
!
!
!*      0.1    declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*),       INTENT(IN)   :: HALBEDO
! SOIL_ALBEDO dependance wxith surface soil water content
!   "EVOL" = SOIL_ALBEDO evolves with soil wetness
!   "DRY " = constant SOIL_ALBEDO value for dry soil
!   "WET " = constant SOIL_ALBEDO value for wet soil
!   "MEAN" = constant SOIL_ALBEDO value for medium soil wetness
!
REAL, DIMENSION(:), INTENT(IN)  :: PWSAT       ! saturation water content
REAL, DIMENSION(:), INTENT(IN)  :: PWG1        ! surface water content
REAL, DIMENSION(:), INTENT(IN)  :: PALBVIS_DRY ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(IN)  :: PALBNIR_DRY ! dry soil albedo
REAL, DIMENSION(:), INTENT(IN)  :: PALBUV_DRY  ! 
REAL, DIMENSION(:), INTENT(IN)  :: PALBVIS_WET ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(IN)  :: PALBNIR_WET ! wet soil albedo
REAL, DIMENSION(:), INTENT(IN)  :: PALBUV_WET  ! wet soil albedo
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PALBVIS_SOIL! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PALBNIR_SOIL! soil albedo
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PALBUV_SOIL !
!
END SUBROUTINE SOIL_ALBEDO_1D
!
      SUBROUTINE SOIL_ALBEDO_1D_PATCH(HALBEDO,                       &
                             PWSAT,PWG1,                               &
                             PALBVIS_DRY,PALBNIR_DRY,PALBUV_DRY,       &
                             PALBVIS_WET,PALBNIR_WET,PALBUV_WET,       &
                             PALBVIS_SOIL, PALBNIR_SOIL,PALBUV_SOIL    )  
!
!
!*      0.1    declarations of arguments
!              -------------------------
!
!
 CHARACTER(LEN=*),       INTENT(IN)   :: HALBEDO
! SOIL_ALBEDO dependance wxith surface soil water content
!   "EVOL" = SOIL_ALBEDO evolves with soil wetness
!   "DRY " = constant SOIL_ALBEDO value for dry soil
!   "WET " = constant SOIL_ALBEDO value for wet soil
!   "MEAN" = constant SOIL_ALBEDO value for medium soil wetness
!
REAL, DIMENSION(:), INTENT(IN)    :: PWSAT       ! saturation water content
REAL, DIMENSION(:,:), INTENT(IN)  :: PWG1        ! surface water content
REAL, DIMENSION(:), INTENT(IN)  :: PALBVIS_DRY ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(IN)  :: PALBNIR_DRY ! dry soil albedo
REAL, DIMENSION(:), INTENT(IN)  :: PALBUV_DRY  !
REAL, DIMENSION(:), INTENT(IN)  :: PALBVIS_WET ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(IN)  :: PALBNIR_WET ! wet soil albedo
REAL, DIMENSION(:), INTENT(IN)  :: PALBUV_WET  !
!
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PALBVIS_SOIL! visible, near infra-red and UV
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PALBNIR_SOIL! soil albedo
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PALBUV_SOIL !
!
END SUBROUTINE SOIL_ALBEDO_1D_PATCH
!
END INTERFACE
!
END MODULE MODI_SOIL_ALBEDO
!
!     ####################################################################
      SUBROUTINE SOIL_ALBEDO_1D(HALBEDO,                             &
                             PWSAT,PWG1,                               &
                             PALBVIS_DRY,PALBNIR_DRY,PALBUV_DRY,       &
                             PALBVIS_WET,PALBNIR_WET,PALBUV_WET,       &
                             PALBVIS_SOIL, PALBNIR_SOIL,PALBUV_SOIL    )  
!     ####################################################################
!
!!****  *SOIL_ALBEDO*  
!!
!!    PURPOSE
!!    -------
!       computes the SOIL ALBEDO of the natural continental parts.
!
! Soil SOIL_ALBEDO is estimated from sand fraction.
! A correction due to the soil humidity can be used.
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
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
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
! SOIL_ALBEDO dependance wxith surface soil water content
!   "EVOL" = SOIL_ALBEDO evolves with soil wetness
!   "DRY " = constant SOIL_ALBEDO value for dry soil
!   "WET " = constant SOIL_ALBEDO value for wet soil
!   "MEAN" = constant SOIL_ALBEDO value for medium soil wetness
!
REAL, DIMENSION(:), INTENT(IN)  :: PWSAT       ! saturation water content
REAL, DIMENSION(:), INTENT(IN)  :: PWG1        ! surface water content
REAL, DIMENSION(:), INTENT(IN)  :: PALBVIS_DRY ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(IN)  :: PALBNIR_DRY ! dry soil albedo
REAL, DIMENSION(:), INTENT(IN)  :: PALBUV_DRY  !
REAL, DIMENSION(:), INTENT(IN)  :: PALBVIS_WET ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(IN)  :: PALBNIR_WET ! wet soil albedo
REAL, DIMENSION(:), INTENT(IN)  :: PALBUV_WET  !
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PALBVIS_SOIL! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PALBNIR_SOIL! soil albedo
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PALBUV_SOIL !
!
!*      0.2    declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(SIZE(PWSAT)) :: ZX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_SOIL_ALBEDO:SOIL_ALBEDO_1D',0,ZHOOK_HANDLE)
IF (HALBEDO=='USER' .AND. LHOOK) CALL DR_HOOK('MODI_SOIL_ALBEDO:SOIL_ALBEDO_1D',1,ZHOOK_HANDLE)
IF (HALBEDO=='USER') RETURN
!
SELECT CASE ( HALBEDO )
  CASE ('EVOL')
!
       ZX = MIN( PWG1(:)/PWSAT(:) , 1. ) 
!
!* linear formula
!      ZALBVIS_SOIL(:) = PALBVIS_DRY(:) + ZX(:)*(PALBVIS_WET(:)-PALBVIS_DRY(:))
!      ZALBNIR_SOIL(:) = PALBNIR_DRY(:) + ZX(:)*(PALBNIR_WET(:)-PALBNIR_DRY(:))
!      ZALBUV_SOIL (:) = PALBUV_DRY (:) + ZX(:)*(PALBUV_WET (:)-PALBUV_DRY (:))
!* quadratic formula
      IF (PRESENT(PALBVIS_SOIL))        &
        PALBVIS_SOIL(:) = PALBVIS_WET(:)                                    &
                        + (0.25*PALBVIS_DRY(:)-PALBVIS_WET(:))              &
                         * (1. - ZX(:))                                     &
                         * ( ZX(:) + (     PALBVIS_DRY(:)-PALBVIS_WET(:))   &
                                    /(0.25*PALBVIS_DRY(:)-PALBVIS_WET(:)) )  
      IF (PRESENT(PALBNIR_SOIL))        &                                    
        PALBNIR_SOIL(:) = PALBNIR_WET(:)                                    &
                        + (0.25*PALBNIR_DRY(:)-PALBNIR_WET(:))              &
                         * (1. - ZX(:))                                     &
                         * ( ZX(:) + (     PALBNIR_DRY(:)-PALBNIR_WET(:))   &
                                    /(0.25*PALBNIR_DRY(:)-PALBNIR_WET(:)) )  
      IF (PRESENT(PALBUV_SOIL))        &                                    
        PALBUV_SOIL (:) = PALBUV_WET(:)                                     &
                        + (0.25*PALBUV_DRY (:)-PALBUV_WET (:))              &
                         * (1. - ZX(:))                                     &
                         * ( ZX(:) + (     PALBUV_DRY (:)-PALBUV_WET (:))   &
                                    /(0.25*PALBUV_DRY (:)-PALBUV_WET (:)) )  
!
  CASE ('DRY ')
      IF (PRESENT(PALBVIS_SOIL)) PALBVIS_SOIL(:) = PALBVIS_DRY(:)
      IF (PRESENT(PALBNIR_SOIL)) PALBNIR_SOIL(:) = PALBNIR_DRY(:)
      IF (PRESENT(PALBUV_SOIL)) PALBUV_SOIL (:) = PALBUV_DRY (:)
  CASE ('WET ')
      IF (PRESENT(PALBVIS_SOIL)) PALBVIS_SOIL(:) = PALBVIS_WET(:)
      IF (PRESENT(PALBNIR_SOIL)) PALBNIR_SOIL(:) = PALBNIR_WET(:)
      IF (PRESENT(PALBUV_SOIL)) PALBUV_SOIL (:) = PALBUV_WET (:)
  CASE ('MEAN')
      IF (PRESENT(PALBVIS_SOIL)) PALBVIS_SOIL(:) = 0.5 * ( PALBVIS_DRY(:) + PALBVIS_WET(:) )
      IF (PRESENT(PALBNIR_SOIL)) PALBNIR_SOIL(:) = 0.5 * ( PALBNIR_DRY(:) + PALBNIR_WET(:) )
      IF (PRESENT(PALBUV_SOIL)) PALBUV_SOIL (:) = 0.5 * ( PALBUV_DRY (:) + PALBUV_WET (:) )
END SELECT
IF (LHOOK) CALL DR_HOOK('MODI_SOIL_ALBEDO:SOIL_ALBEDO_1D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SOIL_ALBEDO_1D
!
!-------------------------------------------------------------------------------
!
!     ####################################################################
      SUBROUTINE SOIL_ALBEDO_1D_PATCH(HALBEDO,                       &
                             PWSAT,PWG1,                               &
                             PALBVIS_DRY,PALBNIR_DRY,PALBUV_DRY,       &
                             PALBVIS_WET,PALBNIR_WET,PALBUV_WET,       &
                             PALBVIS_SOIL, PALBNIR_SOIL,PALBUV_SOIL    )  
!     ####################################################################
!
!!****  *SOIL_ALBEDO*  
!!
!!    PURPOSE
!!    -------
!  computes the SOIL_ALBEDO of for different types (patches) 
! of natural continental parts.
!
! Soil SOIL_ALBEDO is estimated from sand fraction.
! A correction due to the soil humidity can be used.
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
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
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
! SOIL_ALBEDO dependance wxith surface soil water content
!   "EVOL" = SOIL_ALBEDO evolves with soil wetness
!   "DRY " = constant SOIL_ALBEDO value for dry soil
!   "WET " = constant SOIL_ALBEDO value for wet soil
!   "MEAN" = constant SOIL_ALBEDO value for medium soil wetness
!
REAL, DIMENSION(:), INTENT(IN)    :: PWSAT       ! saturation water content
REAL, DIMENSION(:,:), INTENT(IN)  :: PWG1        ! surface water content
REAL, DIMENSION(:), INTENT(IN)  :: PALBVIS_DRY ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(IN)  :: PALBNIR_DRY ! dry soil albedo
REAL, DIMENSION(:), INTENT(IN)  :: PALBUV_DRY  !
REAL, DIMENSION(:), INTENT(IN)  :: PALBVIS_WET ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(IN)  :: PALBNIR_WET ! wet soil SOIL_ALBEDO
REAL, DIMENSION(:), INTENT(IN)  :: PALBUV_WET  !
!
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PALBVIS_SOIL! visible, near infra-red and UV
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PALBNIR_SOIL! soil albedo
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PALBUV_SOIL !
!
!*      0.2    declarations of local variables
!              -------------------------------
!
REAL,    DIMENSION(SIZE(PWSAT)) :: ZX
!
INTEGER :: IPATCH     ! number of patches
INTEGER :: JPATCH     !loop index for patches
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_SOIL_ALBEDO:SOIL_ALBEDO_1D_PATCH',0,ZHOOK_HANDLE)
IF (HALBEDO=='USER' .AND. LHOOK) CALL DR_HOOK('MODI_SOIL_ALBEDO:SOIL_ALBEDO_1D_PATCH',1,ZHOOK_HANDLE)
IF (HALBEDO=='USER') RETURN
!
IPATCH = SIZE(PWG1,2)

IF (PRESENT(PALBVIS_SOIL)) PALBVIS_SOIL = XUNDEF
IF (PRESENT(PALBNIR_SOIL)) PALBNIR_SOIL = XUNDEF
IF (PRESENT(PALBUV_SOIL)) PALBUV_SOIL  = XUNDEF
!
SELECT CASE ( HALBEDO )
CASE ('EVOL')

  DO JPATCH=1,IPATCH
    ZX = MIN( PWG1(:,JPATCH)/PWSAT(:) , 1. )

    !WHERE (PWG1(:,JPATCH)/=XUNDEF)
      !* linear formula
      !      ZALBVIS_SOIL(:) = PALBVIS_DRY(:) + ZX(:)*(PALBVIS_WET(:)-PALBVIS_DRY(:))
      !      ZALBNIR_SOIL(:) = PALBNIR_DRY(:) + ZX(:)*(PALBNIR_WET(:)-PALBNIR_DRY(:))
      !      ZALBUV_SOIL (:) = PALBUV_DRY (:) + ZX(:)*(PALBUV_WET (:)-PALBUV_DRY (:))
      !* quadratic formula
      IF (PRESENT(PALBVIS_SOIL)) &
        WHERE (PWG1(:,JPATCH)/=XUNDEF) &
          PALBVIS_SOIL(:,JPATCH) = PALBVIS_WET(:)             &
             + (0.25*PALBVIS_DRY(:)-PALBVIS_WET(:))           &
             * (1. - ZX(:))                                   &
             * ( ZX(:) + (     PALBVIS_DRY(:)-PALBVIS_WET(:)) &
             /(0.25*PALBVIS_DRY(:)-PALBVIS_WET(:)) )  
      IF (PRESENT(PALBNIR_SOIL))  &     
        WHERE (PWG1(:,JPATCH)/=XUNDEF) &      
        PALBNIR_SOIL(:,JPATCH) = PALBNIR_WET(:)               &
             + (0.25*PALBNIR_DRY(:)-PALBNIR_WET(:))           &
             * (1. - ZX(:))                                   &
             * ( ZX(:) + (     PALBNIR_DRY(:)-PALBNIR_WET(:)) &
             /(0.25*PALBNIR_DRY(:)-PALBNIR_WET(:)) )  
      IF (PRESENT(PALBUV_SOIL))  &   
        WHERE (PWG1(:,JPATCH)/=XUNDEF) &      
        PALBUV_SOIL (:,JPATCH) = PALBUV_WET (:)               &
             + (0.25*PALBUV_DRY (:)-PALBUV_WET (:))           &
             * (1. - ZX(:))                                   &
             * ( ZX(:) + (     PALBUV_DRY (:)-PALBUV_WET (:)) &
             /(0.25*PALBUV_DRY (:)-PALBUV_WET (:)) )  

    !END WHERE
  END DO

CASE ('DRY ')
  IF (PRESENT(PALBVIS_SOIL)) PALBVIS_SOIL(:,:) = SPREAD(PALBVIS_DRY(:),2,IPATCH)
  IF (PRESENT(PALBNIR_SOIL)) PALBNIR_SOIL(:,:) = SPREAD(PALBNIR_DRY(:),2,IPATCH)
  IF (PRESENT(PALBUV_SOIL)) PALBUV_SOIL (:,:) = SPREAD(PALBUV_DRY (:),2,IPATCH)

CASE ('WET ')
  IF (PRESENT(PALBVIS_SOIL)) PALBVIS_SOIL(:,:) = SPREAD(PALBVIS_WET(:),2,IPATCH)
  IF (PRESENT(PALBNIR_SOIL)) PALBNIR_SOIL(:,:) = SPREAD(PALBNIR_WET(:),2,IPATCH)
  IF (PRESENT(PALBUV_SOIL)) PALBUV_SOIL (:,:) = SPREAD(PALBUV_WET (:),2,IPATCH)

CASE ('MEAN')
  IF (PRESENT(PALBVIS_SOIL))  PALBVIS_SOIL(:,:) = 0.5 * ( SPREAD(PALBVIS_DRY(:),2,IPATCH) + SPREAD(PALBVIS_WET(:),2,IPATCH) )
  IF (PRESENT(PALBNIR_SOIL)) PALBNIR_SOIL(:,:) = 0.5 * ( SPREAD(PALBNIR_DRY(:),2,IPATCH) + SPREAD(PALBNIR_WET(:),2,IPATCH) )
  IF (PRESENT(PALBUV_SOIL)) PALBUV_SOIL (:,:) = 0.5 * ( SPREAD(PALBUV_DRY (:),2,IPATCH) + SPREAD(PALBUV_WET (:),2,IPATCH) )

END SELECT
IF (LHOOK) CALL DR_HOOK('MODI_SOIL_ALBEDO:SOIL_ALBEDO_1D_PATCH',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SOIL_ALBEDO_1D_PATCH
