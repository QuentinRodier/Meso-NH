!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!      #########################
MODULE MODI_DRY_WET_SOIL_ALBEDOS
!      #########################
!
INTERFACE DRY_WET_SOIL_ALBEDOS
!
!
      SUBROUTINE DRY_WET_SOIL_ALBEDOS_1D(PSAND,PCLAY,                           &
                                           PVEGTYPE,                              &
                                           PALBNIR_DRY,PALBVIS_DRY,PALBUV_DRY,    &
                                           PALBNIR_WET,PALBVIS_WET,PALBUV_WET     )  
!
!
!*      0.1    declarations of arguments
!              -------------------------
!
!
REAL, DIMENSION(:), INTENT(IN)  :: PSAND       ! sand fraction
REAL, DIMENSION(:), INTENT(IN)  :: PCLAY       ! clay fraction
REAL, DIMENSION(:,:), INTENT(IN):: PVEGTYPE    ! vegetation type
!
REAL, DIMENSION(:), INTENT(OUT) :: PALBVIS_DRY ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(OUT) :: PALBNIR_DRY ! dry bare soil albedo
REAL, DIMENSION(:), INTENT(OUT) :: PALBUV_DRY  !
REAL, DIMENSION(:), INTENT(OUT) :: PALBVIS_WET ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(OUT) :: PALBNIR_WET ! wet bare soil albedo
REAL, DIMENSION(:), INTENT(OUT) :: PALBUV_WET  !
!
END SUBROUTINE DRY_WET_SOIL_ALBEDOS_1D
!
!
      SUBROUTINE DRY_WET_SOIL_ALBEDOS_2D(PSAND,PCLAY,                             &
                                           PVEGTYPE,                                &
                                           PALBNIR_DRY,PALBVIS_DRY,PALBUV_DRY,      &
                                           PALBNIR_WET,PALBVIS_WET,PALBUV_WET       )  
!
!
!*      0.1    declarations of arguments
!              -------------------------
!
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PSAND       ! sand fraction
REAL, DIMENSION(:,:), INTENT(IN)  :: PCLAY       ! clay fraction
REAL, DIMENSION(:,:,:), INTENT(IN):: PVEGTYPE    ! vegetation type
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PALBVIS_DRY ! visible, near infra-red and UV
REAL, DIMENSION(:,:), INTENT(OUT) :: PALBNIR_DRY ! dry bare soil albedo
REAL, DIMENSION(:,:), INTENT(OUT) :: PALBUV_DRY  ! 
REAL, DIMENSION(:,:), INTENT(OUT) :: PALBVIS_WET ! visible, near infra-red and UV
REAL, DIMENSION(:,:), INTENT(OUT) :: PALBNIR_WET ! wet bare soil albedo
REAL, DIMENSION(:,:), INTENT(OUT) :: PALBUV_WET  !
!
END SUBROUTINE DRY_WET_SOIL_ALBEDOS_2D
!
END INTERFACE
!
END MODULE MODI_DRY_WET_SOIL_ALBEDOS
!
!     ##################################################################
      SUBROUTINE DRY_WET_SOIL_ALBEDOS_1D(PSAND,PCLAY,                            &
                                           PVEGTYPE,                               &
                                           PALBNIR_DRY,PALBVIS_DRY,PALBUV_DRY,     &
                                           PALBNIR_WET,PALBVIS_WET,PALBUV_WET      )  
!     ##################################################################
!
!!****  *DRY_WET_SOIL_ALBEDOS*  
!!
!!    PURPOSE
!!    -------
!       computes the albedo of bare soil, for dry or wet conditions
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
!       
!      (V. Masson)  16/02/01 Better fit with ISLSCP2; 
!                                            Ba et al 2001; 
!                                            Pinty et al 2000
!      (V. Masson) 01/2004  Add UV albedo
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_C3, NVT_C4, NVT_IRR, NVT_GRAS, NVT_TROG, &
                                  NVT_PARK, NVT_EVER, NVT_CONI, NVT_TREE  
!
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
REAL, DIMENSION(:), INTENT(IN)  :: PSAND       ! sand fraction
REAL, DIMENSION(:), INTENT(IN)  :: PCLAY       ! clay fraction
REAL, DIMENSION(:,:), INTENT(IN):: PVEGTYPE    ! vegetation type
!
REAL, DIMENSION(:), INTENT(OUT) :: PALBVIS_DRY ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(OUT) :: PALBNIR_DRY ! dry bare soil albedo
REAL, DIMENSION(:), INTENT(OUT) :: PALBUV_DRY  !
REAL, DIMENSION(:), INTENT(OUT) :: PALBVIS_WET ! visible, near infra-red and UV
REAL, DIMENSION(:), INTENT(OUT) :: PALBNIR_WET ! wet bare soil albedo
REAL, DIMENSION(:), INTENT(OUT) :: PALBUV_WET  !
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_DRY_WET_SOIL_ALBEDOS:DRY_WET_SOIL_ALBEDOS_1D',0,ZHOOK_HANDLE)
PALBVIS_DRY(:) = 0.05 +  (   0.05 + MAX(0.30 * PSAND(:), 0.10) )  &
                         * ( 1. - 0.9 * ( PVEGTYPE(:,NVT_C3)        &
                                        + PVEGTYPE(:,NVT_C4)        &
                                        + PVEGTYPE(:,NVT_IRR)       &
                                        + PVEGTYPE(:,NVT_GRAS)      &
                                        + PVEGTYPE(:,NVT_TROG)      &
                                        + PVEGTYPE(:,NVT_PARK)      &
                                        + PVEGTYPE(:,NVT_EVER)      &
                                        + PVEGTYPE(:,NVT_CONI)      &
                                        + PVEGTYPE(:,NVT_TREE) )**2 )  
!
PALBNIR_DRY(:) = PALBVIS_DRY(:) + 0.10
!
PALBUV_DRY (:) = 0.06 + 0.14 * PSAND(:)
!
PALBVIS_WET(:) = PALBVIS_DRY(:) / 2.
PALBNIR_WET(:) = PALBNIR_DRY(:) / 2.
PALBUV_WET (:) = PALBUV_DRY (:) / 2.
IF (LHOOK) CALL DR_HOOK('MODI_DRY_WET_SOIL_ALBEDOS:DRY_WET_SOIL_ALBEDOS_1D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DRY_WET_SOIL_ALBEDOS_1D
!
!     ##################################################################
      SUBROUTINE DRY_WET_SOIL_ALBEDOS_2D(PSAND,PCLAY,                             &
                                           PVEGTYPE,                                &
                                           PALBNIR_DRY,PALBVIS_DRY,PALBUV_DRY,      &
                                           PALBNIR_WET,PALBVIS_WET,PALBUV_WET       )  
!     ##################################################################
!
!!****  *DRY_WET_SOIL_ALBEDOS*  
!!
!!    PURPOSE
!!    -------
!       computes the albedo of bare soil, for dry or wet conditions
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
!      (V. Masson)  16/02/01 Better fit with ISLSCP2; 
!                                            Ba et al 2001; 
!                                            Pinty et al 2000
!      (V. Masson)  Add UV albedo
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVT_C3, NVT_C4, NVT_IRR, NVT_GRAS, NVT_TROG, &
                                  NVT_PARK, NVT_EVER, NVT_CONI, NVT_TREE  
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
REAL, DIMENSION(:,:), INTENT(IN)  :: PSAND       ! sand fraction
REAL, DIMENSION(:,:), INTENT(IN)  :: PCLAY       ! clay fraction
REAL, DIMENSION(:,:,:), INTENT(IN):: PVEGTYPE    ! vegetation type
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PALBVIS_DRY ! visible, near infra-red and UV
REAL, DIMENSION(:,:), INTENT(OUT) :: PALBNIR_DRY ! dry bare soil albedo
REAL, DIMENSION(:,:), INTENT(OUT) :: PALBUV_DRY  ! 
REAL, DIMENSION(:,:), INTENT(OUT) :: PALBVIS_WET ! visible, near infra-red and UV
REAL, DIMENSION(:,:), INTENT(OUT) :: PALBNIR_WET ! wet bare soil albedo
REAL, DIMENSION(:,:), INTENT(OUT) :: PALBUV_WET  !
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_DRY_WET_SOIL_ALBEDOS:DRY_WET_SOIL_ALBEDOS_2D',0,ZHOOK_HANDLE)
PALBVIS_DRY(:,:) = XUNDEF
PALBNIR_DRY(:,:) = XUNDEF
PALBUV_DRY (:,:) = XUNDEF
PALBVIS_WET(:,:) = XUNDEF
PALBNIR_WET(:,:) = XUNDEF
PALBUV_WET (:,:) = XUNDEF
!
WHERE (PSAND(:,:)/=XUNDEF)
  PALBVIS_DRY(:,:) = 0.05 +  (   0.05 + MAX( 0.30 * PSAND(:,:), 0.10) ) &
                             * ( 1. - 0.9 * ( PVEGTYPE(:,:,NVT_C3)        &
                                            + PVEGTYPE(:,:,NVT_C4)        &
                                            + PVEGTYPE(:,:,NVT_IRR)       &
                                            + PVEGTYPE(:,:,NVT_GRAS)      &
                                            + PVEGTYPE(:,:,NVT_TROG)      &
                                            + PVEGTYPE(:,:,NVT_PARK)      &
                                            + PVEGTYPE(:,:,NVT_EVER)      &
                                            + PVEGTYPE(:,:,NVT_CONI)      &
                                            + PVEGTYPE(:,:,NVT_TREE) )**2 )  
  !
  PALBNIR_DRY(:,:) = PALBVIS_DRY(:,:) + 0.10
  !
  PALBUV_DRY (:,:) = 0.06 + 0.14 * PSAND(:,:)
  !
  PALBVIS_WET(:,:) = PALBVIS_DRY(:,:) / 2.
  PALBNIR_WET(:,:) = PALBNIR_DRY(:,:) / 2.
  PALBUV_WET (:,:) = PALBUV_DRY (:,:) / 2.
END WHERE
IF (LHOOK) CALL DR_HOOK('MODI_DRY_WET_SOIL_ALBEDOS:DRY_WET_SOIL_ALBEDOS_2D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DRY_WET_SOIL_ALBEDOS_2D

