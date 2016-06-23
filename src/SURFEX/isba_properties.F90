!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE ISBA_PROPERTIES(HISBA, OTR_ML, TPSNOW, KPATCH,           &
                                 PDIR_SW, PSCA_SW, PSW_BANDS, KSW,        &
                                 PALBNIR, PALBVIS, PALBUV,                &
                                 PALBNIR_VEG, PALBVIS_VEG, PALBUV_VEG,    &
                                 PALBNIR_SOIL, PALBVIS_SOIL, PALBUV_SOIL, &
                                 PVEG, PLAI, PZ0, PEMIS, PTG,             &
                                 PASNOW, PANOSNOW, PESNOW, PENOSNOW,      &
                                 PTSSNOW, PTSNOSNOW,                      &
                                 PSNOWFREE_ALB_VEG, PSNOWFREE_ALB_SOIL,   &
                                 PALBNIR_TVEG, PALBVIS_TVEG,              &
                                 PALBNIR_TSOIL, PALBVIS_TSOIL,            &
                                 PPSN, PPSNV_A, PPSNG, PPSNV              )  
!     ##########################################################################
!
!!****  *ISBA_PROPERTIES*  
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
!!
!!    MODIFICATIONS
!!    ------------- 
!!      
!!      P. Samuelsson  02/2012  MEB
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TYPE_SNOW
USE MODD_SNOW_PAR   , ONLY : XEMISSN, XEMCRIN, XSNOWDMIN, &
                               XRHOSMAX_ES, XRHOSMIN_ES  
USE MODD_WATER_PAR  , ONLY : XEMISWAT
!
USE MODI_ISBA_SNOW_FRAC
USE MODI_ISBA_ALBEDO
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*)    , INTENT(IN)   :: HISBA      ! ISBA scheme
LOGICAL             , INTENT(IN)   :: OTR_ML     ! new radiative transfert
TYPE(SURF_SNOW),      INTENT(IN)   :: TPSNOW     ! ISBA snow scheme
INTEGER,              INTENT(IN)   :: KPATCH     ! patch being treated
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
!
REAL, DIMENSION(:)  , INTENT(IN)   :: PVEG  ! PVEG = fraction of vegetation
REAL, DIMENSION(:)  , INTENT(IN)   :: PLAI  ! PLAI = leaf area index
REAL, DIMENSION(:)  , INTENT(IN)   :: PZ0   ! PZ0  = roughness length for momentum
REAL, DIMENSION(:)  , INTENT(IN)   :: PEMIS ! PEMIS = emissivity
REAL, DIMENSION(:)  , INTENT(IN)   :: PTG   !             
!
REAL, DIMENSION(:)  , INTENT(OUT)  :: PASNOW    ! = snow albedo
REAL, DIMENSION(:)  , INTENT(OUT)  :: PANOSNOW  ! = snow free albedo 
REAL, DIMENSION(:)  , INTENT(OUT)  :: PESNOW    ! = snow emissivity
REAL, DIMENSION(:)  , INTENT(OUT)  :: PENOSNOW  ! = snow free emissivity
REAL, DIMENSION(:)  , INTENT(OUT)  :: PTSSNOW   ! = snow radiative temperature
REAL, DIMENSION(:)  , INTENT(OUT)  :: PTSNOSNOW ! = snow free radiative temperature
REAL, DIMENSION(:)  , INTENT(OUT)  :: PSNOWFREE_ALB_VEG  !snow free albedo of vegetation for EBA
REAL, DIMENSION(:)  , INTENT(OUT)  :: PSNOWFREE_ALB_SOIL !snow free albedo of soil for EBA option
REAL, DIMENSION(:)  , INTENT(OUT)  :: PALBNIR_TVEG       ! nearIR  veg tot albedo
REAL, DIMENSION(:)  , INTENT(OUT)  :: PALBVIS_TVEG       ! visible veg tot albedo
REAL, DIMENSION(:)  , INTENT(OUT)  :: PALBNIR_TSOIL      ! nearIR  soil tot albedo
REAL, DIMENSION(:)  , INTENT(OUT)  :: PALBVIS_TSOIL      ! visible soil tot albedo
!
REAL, DIMENSION(:)  , INTENT(OUT):: PPSN    ! PPSN = grid fraction covered by snow
REAL, DIMENSION(:)  , INTENT(OUT):: PPSNG   ! PPSNG = fraction of the ground covered by snow
REAL, DIMENSION(:)  , INTENT(OUT):: PPSNV   ! PPSNV = fraction of the veg covered by snow 
REAL, DIMENSION(:)  , INTENT(OUT):: PPSNV_A !fraction of the the vegetation covered by snow for EBA scheme
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PDIR_SW,1)) :: ZGLOBAL_SW                 ! global incoming SW rad.
REAL, DIMENSION(SIZE(PALBNIR))   :: ZALBF
REAL, DIMENSION(SIZE(PALBNIR))   :: ZFFV
REAL, DIMENSION(SIZE(PALBNIR))   :: ZFFG
!
LOGICAL, PARAMETER :: GMEB=.FALSE.
REAL, DIMENSION(SIZE(PDIR_SW,1))   :: ZP_MEB_SCA_SW, ZALBNIR_TSNOW, ZALBVIS_TSNOW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_PROPERTIES',0,ZHOOK_HANDLE)
 CALL ISBA_SNOW_FRAC(TPSNOW%SCHEME,                                        &
                    TPSNOW%WSNOW(:,:,KPATCH), TPSNOW%RHO(:,:,KPATCH),     &
                    TPSNOW%ALB  (:,KPATCH), PVEG, PLAI, PZ0,              &
                    PPSN, PPSNV_A, PPSNG, PPSNV                           )  
!
!-------------------------------------------------------------------------------
!*      2.     Compute snow-free albedo
!              ------------------------
!
!* Snow-free surface albedo for each wavelength
!
ZALBF         = 0.
ZFFV          = 0.
ZFFG          = 0.
!
 CALL ISBA_ALBEDO(TPSNOW%SCHEME, OTR_ML, GMEB,                             &
                   PDIR_SW, PSCA_SW, PSW_BANDS, KSW,                       &
                   PALBNIR, PALBVIS, PALBUV,                               &
                   PALBNIR_VEG, PALBVIS_VEG, PALBUV_VEG,                   &
                   PALBNIR_SOIL, PALBVIS_SOIL, PALBUV_SOIL,                &
                   ZALBF, ZFFV, ZFFG,                                      &
                   ZGLOBAL_SW, PANOSNOW,                                   &
                   PSNOWFREE_ALB_VEG, PSNOWFREE_ALB_SOIL,                  &
                   ZP_MEB_SCA_SW,                                          &
                   PALBNIR_TVEG, PALBVIS_TVEG, PALBNIR_TSOIL, PALBVIS_TSOIL)

!-------------------------------------------------------------------------------
!
!*      3.     Compute aggeragted albedo and emissivity
!              ----------------------------------------
!
IF(TPSNOW%SCHEME == '3-L' .OR. TPSNOW%SCHEME == 'CRO' .OR. HISBA == 'DIF')THEN
!
! NON-SNOW covered Grid averaged albedo and emissivity for explicit snow scheme:
!
   PASNOW(:) = TPSNOW%ALB(:,KPATCH)
   PESNOW(:) = TPSNOW%EMIS(:,KPATCH)
   PENOSNOW(:) = PEMIS(:)

   PTSSNOW(:)   = TPSNOW%TS(:,KPATCH)
   PTSNOSNOW(:) = PTG(:)

ELSE
!
! Grid averaged albedo and emissivity for composite snow scheme:
!
   IF(TPSNOW%SCHEME =='EBA') THEN
!
      PASNOW(:) = TPSNOW%ALB(:,KPATCH)
      PESNOW(:) = XEMCRIN
      PENOSNOW(:) = PEMIS(:)

      PTSSNOW(:)   = PTG(:)
      PTSNOSNOW(:) = PTG(:)


   ELSE

      PASNOW(:) = TPSNOW%ALB(:,KPATCH)
      PESNOW(:) = XEMISSN
      PENOSNOW(:) = PEMIS(:)

      PTSSNOW(:)   = PTG(:)
      PTSNOSNOW(:) = PTG(:)

   ENDIF
!
ENDIF
IF (LHOOK) CALL DR_HOOK('ISBA_PROPERTIES',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ISBA_PROPERTIES
