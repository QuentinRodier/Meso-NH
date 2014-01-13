!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!#####################
MODULE MODI_SURF_PATCH
!#####################
!
INTERFACE SURF_PATCH
      SUBROUTINE SURF_PATCH_2D(KPATCH,PVEGTYPE,PPATCH)

INTEGER               , INTENT(IN)  :: KPATCH   ! number of patches
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PVEGTYPE ! vegtype fractions
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PPATCH   ! patch weight in nature fraction

END SUBROUTINE SURF_PATCH_2D
      SUBROUTINE SURF_PATCH_1D(KPATCH,PVEGTYPE,PPATCH,PVEGTYPE_PATCH)

INTEGER               , INTENT(IN)  :: KPATCH   ! number of patches
REAL, DIMENSION(:,:),   INTENT(IN)  :: PVEGTYPE ! vegtype fractions
REAL, DIMENSION(:,:),   INTENT(OUT) :: PPATCH   ! patch weight in nature fraction
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(OUT) :: PVEGTYPE_PATCH  ! vegtype fractions
!                                                                ! for each patch

END SUBROUTINE SURF_PATCH_1D
!
END INTERFACE SURF_PATCH
!
END MODULE MODI_SURF_PATCH
!
!     #############################################
      SUBROUTINE SURF_PATCH_2D(KPATCH,PVEGTYPE,PPATCH)
!     #############################################
!
!!****  *SURF_PATCH * - subroutine to compute the patch fractions in each grid
!!                      mesh with nature in it.
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!
!!
!!    EXTERNAL
!!    --------
!!
!!
!! 
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!
!!    AUTHOR
!!    ------
!!
!!       V. Masson    * METEO-FRANCE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original     15/03/99
!       F.solmon      06/00 adaptation for patch approach
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_VEGTYPE_TO_PATCH
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
INTEGER,                INTENT(IN)  :: KPATCH   ! number of patches
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PVEGTYPE ! vegtype fractions
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PPATCH   ! patch weight in nature fraction
!
!
!*       0.2    Declarations of local variables for print on FM file
!
!
INTEGER                              ::JVEG, JPATCH  ! loop on patches
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_SURF_PATCH:SURF_PATCH_2D',0,ZHOOK_HANDLE)
PPATCH (:,:,:)=0.
DO JVEG=1,NVEGTYPE
  JPATCH=VEGTYPE_TO_PATCH (JVEG, KPATCH)
  WHERE (PVEGTYPE (:,:,JVEG) /= XUNDEF)
    PPATCH (:,:,JPATCH)=   PPATCH (:,:,JPATCH) +   PVEGTYPE (:,:,JVEG)
  END WHERE
END DO
IF (LHOOK) CALL DR_HOOK('MODI_SURF_PATCH:SURF_PATCH_2D',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SURF_PATCH_2D
!-------------------------------------------------------------------------------
!
!     #############################################
      SUBROUTINE SURF_PATCH_1D(KPATCH,PVEGTYPE,PPATCH,PVEGTYPE_PATCH)
!     #############################################
!
!!****  *SURF_PATCH * - subroutine to compute the patch fractions in each grid
!!                      mesh with nature in it.
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!
!!
!!    EXTERNAL
!!    --------
!!
!!
!! 
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!
!!    AUTHOR
!!    ------
!!
!!       V. Masson    * METEO-FRANCE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original     15/03/99
!       F.solmon      06/00 adaptation for patch approach
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_VEGTYPE_TO_PATCH
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
INTEGER               , INTENT(IN)  :: KPATCH   ! number of patches
REAL, DIMENSION(:,:),   INTENT(IN)  :: PVEGTYPE ! vegtype fractions
REAL, DIMENSION(:,:),   INTENT(OUT) :: PPATCH   ! patch weight in nature fraction
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(OUT) :: PVEGTYPE_PATCH  ! vegtype fractions
!                                                                ! for each patch
!
!
!*       0.2    Declarations of local variables for print on FM file
!
!
INTEGER                              ::JVEG, JPATCH  ! loop on patches
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_SURF_PATCH:SURF_PATCH_1D',0,ZHOOK_HANDLE)
PPATCH         (:,:)  =0.
IF (PRESENT(PVEGTYPE_PATCH)) PVEGTYPE_PATCH (:,:,:)=0.
DO JVEG=1,NVEGTYPE
  JPATCH=VEGTYPE_TO_PATCH (JVEG, KPATCH)
  WHERE (PVEGTYPE (:,JVEG) /= XUNDEF)
    PPATCH         (:,JPATCH)     =   PPATCH (:,JPATCH) +   PVEGTYPE (:,JVEG)
  END WHERE
  IF (PRESENT(PVEGTYPE_PATCH)) THEN
    WHERE (PVEGTYPE (:,JVEG) /= XUNDEF)
      PVEGTYPE_PATCH (:,JVEG,JPATCH)=                         PVEGTYPE (:,JVEG)
    END WHERE
  END IF
END DO
IF (PRESENT(PVEGTYPE_PATCH)) THEN
  DO JPATCH=1,KPATCH
    DO JVEG=1,NVEGTYPE
      WHERE (PVEGTYPE (:,JVEG) /= XUNDEF .AND. PPATCH(:,JPATCH)/= 0.) 
        PVEGTYPE_PATCH(:,JVEG,JPATCH) = PVEGTYPE_PATCH(:,JVEG,JPATCH) / PPATCH(:,JPATCH)
      END WHERE
    END DO
  END DO
END IF
IF (LHOOK) CALL DR_HOOK('MODI_SURF_PATCH:SURF_PATCH_1D',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SURF_PATCH_1D
