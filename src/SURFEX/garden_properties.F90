!     #########
      SUBROUTINE GARDEN_PROPERTIES(PDIR_SW, PSCA_SW, PSW_BANDS, KSW, &
                                   PTS, PEMIS, PALB, PTA             )  
!     ##########################################################################
!
!!****  *GARDEN_PROPERTIES*  
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
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODD_TEB_VEG_n,         ONLY : CISBA, LTR_ML
USE MODD_TEB_GARDEN_n,      ONLY : TSNOW, XALBNIR, XALBVIS, XALBUV,    &
                                   XALBNIR_VEG, XALBVIS_VEG, XALBUV_VEG,      &
                                   XALBNIR_SOIL, XALBVIS_SOIL, XALBUV_SOIL,   &
                                   XALBNIR_TVEG, XALBVIS_TVEG,       &
                                   XALBNIR_TSOIL, XALBVIS_TSOIL,   &                                   
                                   XVEG, XLAI, XZ0, XEMIS, XTG,       &
                                   XPSN, XPSNV, XPSNG, XPSNV_A,  &
                                   XSNOWFREE_ALB_VEG, XSNOWFREE_ALB_SOIL,     &
                                   XSNOWFREE_ALB  
!
USE MODI_ISBA_PROPERTIES
USE MODI_FLAG_TEB_GARDEN_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PDIR_SW            ! direct incoming solar radiation
REAL, DIMENSION(:,:), INTENT(IN)   :: PSCA_SW            ! diffus incoming solar radiation
REAL, DIMENSION(:)  , INTENT(IN)   :: PSW_BANDS          ! mean wavelength of each shortwave band (m)
INTEGER,              INTENT(IN)   :: KSW                ! number of short-wave spectral bands
!
REAL, DIMENSION(:)  , INTENT(OUT)  :: PTS                ! radiative surface temperature
REAL, DIMENSION(:)  , INTENT(OUT)  :: PEMIS              ! green areas emissivity
REAL, DIMENSION(:)  , INTENT(OUT)  :: PALB               ! green areas albedo
!
REAL, DIMENSION(:)  , INTENT(IN), OPTIONAL :: PTA        ! Air temperature (K)
!-------------------------------------------------------------------------------
!
!*      0.2    Local variables
!              ---------------
!
INTEGER                        :: JLAYER
INTEGER                        :: JSWB
!
REAL, DIMENSION(SIZE(PALB))    :: ZTSNOSNOW ! surf. temp. on snow free part
REAL, DIMENSION(SIZE(PALB))    :: ZTSSNOW   ! surf. temp. on snow covered part
REAL, DIMENSION(SIZE(PALB))    :: ZANOSNOW  ! snow-free surface albedo
REAL, DIMENSION(SIZE(PALB))    :: ZASNOW    ! snow albedo
REAL, DIMENSION(SIZE(PALB))    :: ZENOSNOW  ! snow-free surface emissivity
REAL, DIMENSION(SIZE(PALB))    :: ZESNOW    ! snow emissivity
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GARDEN_PROPERTIES',0,ZHOOK_HANDLE)
!
!*      1.     Set physical values for points where there is no garden
!              -------------------------------------------------------
!
! This way, ISBA can run without problem for these points
!
 CALL FLAG_TEB_GARDEN_n(1)
!
!
!*      2.     Computes several properties of gardens
!              --------------------------------------
!
 CALL ISBA_PROPERTIES(CISBA, LTR_ML, TSNOW, 1,                            &
                     PDIR_SW, PSCA_SW, PSW_BANDS, KSW,                   &
                     XALBNIR(:), XALBVIS(:), XALBUV(:),                  &
                     XALBNIR_VEG(:), XALBVIS_VEG(:), XALBUV_VEG(:),      &
                     XALBNIR_SOIL(:), XALBVIS_SOIL(:), XALBUV_SOIL(:),   &
                     XVEG(:), XLAI(:), XZ0(:), XEMIS(:),XTG(:,1),          &
                     ZASNOW, ZANOSNOW, ZESNOW, ZENOSNOW, ZTSSNOW, ZTSNOSNOW,      &
                     XSNOWFREE_ALB_VEG, XSNOWFREE_ALB_SOIL,                       &
                     XALBNIR_TVEG, XALBVIS_TVEG, XALBNIR_TSOIL, XALBVIS_TSOIL,    &
                     XPSN(:), XPSNV_A(:), XPSNG(:), XPSNV(:)          )  
!
XSNOWFREE_ALB = ZANOSNOW
!
!* averaged albedo
PALB =  XPSN(:) * ZASNOW              + (1.-XPSN(:)) * ZANOSNOW
!* averaged emissivity
PEMIS=  XPSN(:) * ZESNOW              + (1.-XPSN(:)) * ZENOSNOW
!* averaged surface radiative temperature
!  (recomputed from emitted long wave)
PTS  =((XPSN(:) * ZESNOW * ZTSSNOW**4 + (1.-XPSN(:)) * ZENOSNOW * ZTSNOSNOW**4) / PEMIS)**0.25
IF (LHOOK) CALL DR_HOOK('GARDEN_PROPERTIES',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GARDEN_PROPERTIES

