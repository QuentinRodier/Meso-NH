!     #######################################################################################
      SUBROUTINE UPDATE_ESM_TEB_n(TOP, BOP, IM, TPN, NT, NB, GDM, GRM, KI,KSW,PZENITH,PSW_BANDS,&
                                  PDIR_ALB,PSCA_ALB,PEMIS,PTSRAD,PTSURF)
!     #######################################################################################
!
!!****  *UPDATE_ESM_TEB_n* - routine to update TEB radiative properties in Earth
!!                           System Model after the call to OASIS coupler in order 
!!                           to close the energy budget between radiative scheme and surfex
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
!!     C. Lebeaupin Brossier
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2015
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!

USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_TEB_PANEL_n, ONLY : TEB_PANEL_t
USE MODD_TEB_n, ONLY : TEB_NP_t
USE MODD_BEM_n, ONLY : BEM_NP_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t, TEB_GREENROOF_MODEL_t
!
USE MODD_SURF_PAR,      ONLY: XUNDEF
USE MODD_CSTS,          ONLY : XPI
USE MODD_SURFEX_n,      ONLY : ISBA_MODEL_t

USE MODD_URBTREE,       ONLY : XTAU_SWHV
!
USE MODI_TEB_VEG_PROPERTIES
USE MODI_AVERAGED_TSRAD_TEB
USE MODI_AVERAGED_ALBEDO_TEB
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(TEB_PANEL_t), INTENT(INOUT) :: TPN
TYPE(TEB_NP_t), INTENT(INOUT) :: NT
TYPE(BEM_NP_t), INTENT(INOUT) :: NB
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
!
INTEGER,                            INTENT(IN)  :: KI        ! number of points
INTEGER,                            INTENT(IN)  :: KSW       ! number of short-wave spectral bands
!
REAL,             DIMENSION(KI),    INTENT(IN)  :: PZENITH   ! solar zenithal angle
REAL,             DIMENSION(KSW),   INTENT(IN)  :: PSW_BANDS ! middle wavelength of each band
!
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB  ! direct albedo for each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB  ! diffuse albedo for each band
REAL,             DIMENSION(KI),    INTENT(OUT) :: PEMIS     ! emissivity
!
REAL,             DIMENSION(KI),    INTENT(OUT) :: PTSRAD    ! radiative temperature
REAL,             DIMENSION(KI),    INTENT(OUT) :: PTSURF    ! surface temperature
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                         :: ILU              ! sizes of TEB arrays
INTEGER                         :: ISWB        ! number of shortwave spectral bands
INTEGER                         :: JSWB        ! loop on shortwave spectral bands
!
REAL, DIMENSION(:), ALLOCATABLE :: ZDIR_ALB         ! direct town albedo
REAL, DIMENSION(:), ALLOCATABLE :: ZSCA_ALB         ! diffuse town albedo
!
!              local variables for urban green areas
REAL, DIMENSION(KI,KSW)         :: ZDIR_ALB_GARDEN  ! direct  albedo for each band
REAL, DIMENSION(KI,KSW)         :: ZSCA_ALB_GARDEN  ! diffuse albedo for each band
REAL, DIMENSION(KI,KSW)         :: ZDIR_SW          ! direct  SW for each band
REAL, DIMENSION(KI,KSW)         :: ZSCA_SW          ! diffuse SW for each band
REAL, DIMENSION(KI)             :: ZEMIS_GARDEN     ! emissivity
REAL, DIMENSION(KI)             :: ZALB_GARDEN      ! albedo
REAL, DIMENSION(KI)             :: ZTS_GARDEN       ! radiative temperature
!
REAL, DIMENSION(KI)             :: ZEMIS_GREENROOF     ! emissivity
REAL, DIMENSION(KI)             :: ZALB_GREENROOF      ! albedo
REAL, DIMENSION(KI)             :: ZTS_GREENROOF       ! radiative temperature
!
REAL, DIMENSION(KI)             :: ZALB_HVEG         ! albedo of urban trees
REAL, DIMENSION(KI)             :: ZTRANS_HVCR      ! transmissivity for all crown of high veg

!
REAL, DIMENSION(KI)             :: ZAZIM  !** strong simplification: to change
REAL, DIMENSION(KI)             :: ZWGT   !** weight sum
!
INTEGER                         :: JP
!
!-------------------------------------------------------------------------------
!
!
!*       1.   Emissivity, radiative temperature and surf temperature
!             ------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_TEB_N',0,ZHOOK_HANDLE)
! *copy from init_tebn *
ILU = SIZE(TOP%XCOVER,1)
!
PTSURF(:)=0.
ZWGT(:)=0.
!
DO JP=1,TOP%NTEB_PATCH
!
  IF (TOP%LGARDEN) THEN
    ZDIR_SW=0. ! night as first guess for albedo computation
    ZSCA_SW=0. !
    CALL TEB_VEG_PROPERTIES(NT%AL(JP)%XGARDEN, GDM%O, GDM%NPE%AL(JP), &
                           ZDIR_SW, ZSCA_SW, PSW_BANDS, KSW,      &
                           ZTS_GARDEN, ZEMIS_GARDEN, ZALB_GARDEN )      
  ELSE
    ZALB_GARDEN = XUNDEF
    ZEMIS_GARDEN= XUNDEF
    ZTS_GARDEN  = XUNDEF
  END IF
!
  IF (TOP%LGREENROOF) THEN
    ZDIR_SW=0. ! night as first guess for albedo computation
    ZSCA_SW=0. !
    CALL TEB_VEG_PROPERTIES(NT%AL(JP)%XGREENROOF, GRM%O, GRM%NPE%AL(JP),         & 
                            ZDIR_SW, ZSCA_SW, PSW_BANDS, KSW,              &
                            ZTS_GREENROOF, ZEMIS_GREENROOF, ZALB_GREENROOF )  
  ELSE
    ZALB_GREENROOF  = XUNDEF
    ZEMIS_GREENROOF = XUNDEF
    ZTS_GREENROOF   = XUNDEF
  END IF
!
!* averaged emissivity and radiative temperature
!
  CALL AVERAGED_TSRAD_TEB(NT%AL(JP), NB%AL(JP), ZEMIS_GARDEN, ZTS_GARDEN,  &
                        ZEMIS_GREENROOF, ZTS_GREENROOF, GDM%NPE%AL(JP)%XEMIS, GDM%NPE%AL(JP)%XTV,&
                        PEMIS, PTSRAD  )
!* averaged surface temperature
!*      - CLB: to verify
  PTSURF(:)=PTSURF(:)+NT%AL(JP)%XROAD(:)*NT%AL(JP)%XT_ROAD(:,1)+NT%AL(JP)%XBLD(:)*NT%AL(JP)%XT_ROOF(:,1)&
            +NT%AL(JP)%XWALL_O_HOR(:)*NT%AL(JP)%XT_WALL_A(:,1)
  ZWGT(:)=ZWGT(:) +NT%AL(JP)%XROAD(:)+NT%AL(JP)%XBLD(:)+NT%AL(JP)%XWALL_O_HOR(:)
!
  IF (TOP%LGARDEN) THEN
    PTSURF(:)=PTSURF(:)+NT%AL(JP)%XGARDEN(:)*ZTS_GARDEN(:)
    ZWGT(:) = ZWGT(:) + NT%AL(JP)%XGARDEN(:)
  ENDIF
  IF (TOP%LGREENROOF) THEN
    PTSURF(:)=PTSURF(:)+NT%AL(JP)%XGREENROOF(:)*ZTS_GREENROOF(:)
    ZWGT(:) = ZWGT(:) + NT%AL(JP)%XGREENROOF(:)
  ENDIF
!* 
!
!
!*       2.     Visible and near-infra-red Radiative fields:
!               -------------------------------------------
!
  ALLOCATE(ZDIR_ALB(ILU))
  ALLOCATE(ZSCA_ALB(ILU))
!
  ZALB_HVEG(:) = 0.5 * (GDM%NPEHV%AL(JP)%XALBNIR_VEG + GDM%NPEHV%AL(JP)%XALBVIS_VEG)
  ZTRANS_HVCR(:) = EXP(-XTAU_SWHV * GDM%NPE%AL(JP)%XLAI)
  ZAZIM=XPI !PAZIM?
  CALL AVERAGED_ALBEDO_TEB(TOP,BOP,KI,KSW,NT%AL(JP),TPN,NB%AL(JP),IM%GDP,PZENITH,ZAZIM,   &
                           ZTRANS_HVCR, NB%AL(JP)%XSHAD_BEHAV_ANYWAY, NB%AL(JP)%XSHAD_BEHAV_ADAPTI,&
                           ZALB_GARDEN, ZALB_GREENROOF,ZALB_HVEG,ZDIR_ALB, ZSCA_ALB)  
!
  ISWB=SIZE(PSW_BANDS)
  DO JSWB=1,ISWB
    PDIR_ALB(:,JSWB) = ZDIR_ALB(:)
    PSCA_ALB(:,JSWB) = ZSCA_ALB(:)
  END DO
!
  DEALLOCATE(ZDIR_ALB)
  DEALLOCATE(ZSCA_ALB)
!
ENDDO 
!
! - verif? 
PTSURF(:) = PTSURF(:)/ZWGT(:)
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_TEB_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UPDATE_ESM_TEB_n
