!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_VEG_n(KPATCH, KI, OCANOPY, HROUGH, OAGRI_TO_GRASS, TPSNOW, &
                         HPHOTO, OIMP_VEG, OIMP_Z0, OIMP_EMIS,  &
                         PLAIMIN, PH_TREE, PVEGTYPE_PATCH, PLAI, PZ0, PVEG, PEMIS, &
                         OTR_ML, PFAPARC, PFAPIRC, PLAI_EFFC, PMUS, &
                         PALBNIR_SOIL, PALBVIS_SOIL, PALBUV_SOIL, PALBNIR, PALBVIS, PALBUV, &
                         OSURF_DIAG_ALBEDO, PPSN, PPSNG, PPSNV, PPSNV_A, &
                         PDIR_ALB, PSCA_ALB, PEMIS_OUT, PTSRAD )  
!#############################################################
!
!!****  *INIT_VEG_n* - routine to initialize ISBA
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!
!!      B. Decharme    01/16 : Bug when vegetation veg, z0 and emis are imposed whith interactive vegetation
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_SNOW
USE MODD_SNOW_PAR,       ONLY : XEMISSN
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
USE MODI_SET_ROUGH
USE MODI_INIT_SNOW_LW
USE MODI_Z0V_FROM_LAI
USE MODI_VEG_FROM_LAI
USE MODI_EMIS_FROM_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(IN) :: KPATCH
INTEGER, INTENT(IN) :: KI
LOGICAL, INTENT(IN) :: OCANOPY
 CHARACTER(LEN=4), INTENT(INOUT) :: HROUGH
LOGICAL, INTENT(IN) :: OAGRI_TO_GRASS
TYPE(SURF_SNOW),      INTENT(INOUT) :: TPSNOW  ! snow characteristics
!
 CHARACTER(LEN=3), INTENT(IN) :: HPHOTO
!
LOGICAL, INTENT(IN) :: OIMP_VEG
LOGICAL, INTENT(IN) :: OIMP_Z0
LOGICAL, INTENT(IN) :: OIMP_EMIS
!
REAL, DIMENSION(:,:), INTENT(IN) :: PLAIMIN
REAL, DIMENSION(:,:), INTENT(IN) :: PH_TREE
REAL, DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE_PATCH
REAL, DIMENSION(:,:), INTENT(INOUT) :: PLAI
REAL, DIMENSION(:,:), INTENT(INOUT) :: PZ0
REAL, DIMENSION(:,:), INTENT(INOUT) :: PVEG
REAL, DIMENSION(:,:), INTENT(INOUT) :: PEMIS
!
LOGICAL, INTENT(IN) :: OTR_ML
REAL, DIMENSION(:,:), POINTER :: PFAPARC
REAL, DIMENSION(:,:), POINTER :: PFAPIRC
REAL, DIMENSION(:,:), POINTER :: PLAI_EFFC
REAL, DIMENSION(:,:), POINTER :: PMUS
!
REAL, DIMENSION(:,:), POINTER :: PALBNIR_SOIL
REAL, DIMENSION(:,:), POINTER :: PALBVIS_SOIL
REAL, DIMENSION(:,:), POINTER :: PALBUV_SOIL
REAL, DIMENSION(:,:), POINTER :: PALBNIR
REAL, DIMENSION(:,:), POINTER :: PALBVIS
REAL, DIMENSION(:,:), POINTER :: PALBUV
!
LOGICAL, INTENT(OUT) :: OSURF_DIAG_ALBEDO
!
REAL, DIMENSION(:,:), POINTER :: PPSN
REAL, DIMENSION(:,:), POINTER :: PPSNG
REAL, DIMENSION(:,:), POINTER :: PPSNV
REAL, DIMENSION(:,:), POINTER :: PPSNV_A
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PDIR_ALB
REAL, DIMENSION(:,:), INTENT(OUT) :: PSCA_ALB
REAL, DIMENSION(:), INTENT(OUT) :: PEMIS_OUT
REAL, DIMENSION(:), INTENT(OUT) :: PTSRAD
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JPATCH  ! loop counter on tiles
INTEGER :: JILU     ! loop increment
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_VEG_n',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*      1.     Roughness length option
!              -----------------------
!
 CALL SET_ROUGH(OCANOPY,HROUGH)
!
!-------------------------------------------------------------------------------
!
!*      2.     Radiative fields and snow/flood fracion initialization:
!              -------------------------------------------------------
!
!* snow long-wave properties (not initialized in read_gr_snow)
!
 CALL INIT_SNOW_LW(XEMISSN,TPSNOW)
!
!-------------------------------------------------------------------------------
!
!* z0 and vegetation fraction estimated from LAI if not imposed
IF (HPHOTO=='LAI' .OR. HPHOTO=='LST' .OR. HPHOTO=='NIT' .OR. HPHOTO=='NCB') THEN
  DO JPATCH=1,KPATCH
    DO JILU=1,KI    
      IF(PLAI(JILU,JPATCH)/=XUNDEF) THEN
        PLAI (JILU,JPATCH) = MAX(PLAIMIN(JILU,JPATCH),PLAI(JILU,JPATCH))
      END IF  
      IF(.NOT.OIMP_Z0.AND.PLAI(JILU,JPATCH)/=XUNDEF) THEN
        PZ0  (JILU,JPATCH) = Z0V_FROM_LAI(PLAI(JILU,JPATCH),PH_TREE(JILU,JPATCH),PVEGTYPE_PATCH(JILU,:,JPATCH),OAGRI_TO_GRASS)
      END IF  
      IF(.NOT.OIMP_VEG.AND.PLAI(JILU,JPATCH)/=XUNDEF) THEN
        PVEG (JILU,JPATCH) = VEG_FROM_LAI(PLAI(JILU,JPATCH),PVEGTYPE_PATCH(JILU,:,JPATCH),OAGRI_TO_GRASS)
      END IF  
      IF(.NOT.OIMP_EMIS.AND.PLAI(JILU,JPATCH)/=XUNDEF) THEN
        PEMIS(JILU,JPATCH) = EMIS_FROM_VEG(PVEG(JILU,JPATCH),PVEGTYPE_PATCH(JILU,:,JPATCH))
      END IF  
    END DO
  END DO
END IF
!
!-------------------------------------------------------------------------------
!
IF (OTR_ML) THEN
  ALLOCATE(PFAPARC   (KI, KPATCH))
  ALLOCATE(PFAPIRC   (KI, KPATCH))
  ALLOCATE(PLAI_EFFC (KI, KPATCH))
  ALLOCATE(PMUS      (KI, KPATCH))
  PFAPARC   (:,:) = 0.
  PFAPIRC   (:,:) = 0.
  PLAI_EFFC (:,:) = 0.
  PMUS      (:,:) = 0.
ELSE
  ALLOCATE(PFAPARC   (0,0))
  ALLOCATE(PFAPIRC   (0,0))
  ALLOCATE(PLAI_EFFC (0,0))
  ALLOCATE(PMUS      (0,0))
ENDIF        
!
!-------------------------------------------------------------------------------
!
!* albedo per tile and averaged albedo, emissivity and radiative temperature
!
ALLOCATE(PALBNIR_SOIL(KI,KPATCH))
ALLOCATE(PALBVIS_SOIL(KI,KPATCH))
ALLOCATE(PALBUV_SOIL (KI,KPATCH))
ALLOCATE(PALBNIR     (KI,KPATCH))
ALLOCATE(PALBVIS     (KI,KPATCH))
ALLOCATE(PALBUV      (KI,KPATCH))
PALBNIR_SOIL(:,:) = XUNDEF
PALBVIS_SOIL(:,:) = XUNDEF
PALBUV_SOIL (:,:) = XUNDEF
PALBNIR     (:,:) = XUNDEF
PALBVIS     (:,:) = XUNDEF
PALBUV      (:,:) = XUNDEF
!
OSURF_DIAG_ALBEDO = .TRUE.
!
!* Initialization of total albedo, emissivity and snow/flood fractions
!
ALLOCATE(PPSN (KI,KPATCH))
ALLOCATE(PPSNG(KI,KPATCH))
ALLOCATE(PPSNV(KI,KPATCH))
PPSN  = 0.0
PPSNG = 0.0
PPSNV = 0.0
!
IF(TPSNOW%SCHEME=='EBA')THEN
   ALLOCATE(PPSNV_A(KI,KPATCH))
   PPSNV_A = 0.0
ELSE
   ALLOCATE(PPSNV_A(0,0))
ENDIF
!
PDIR_ALB = XUNDEF
PSCA_ALB = XUNDEF
PEMIS_OUT= XUNDEF
PTSRAD   = XUNDEF
!
IF (LHOOK) CALL DR_HOOK('INIT_VEG_n',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_VEG_n
