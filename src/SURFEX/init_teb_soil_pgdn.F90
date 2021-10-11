!#############################################################
SUBROUTINE INIT_TEB_SOIL_PGD_n(HPROGRAM, TOP, T, GDM, KI, PPGD_CLAY, PPGD_SAND)
!#############################################################
!
!!****  *INIT_TEB_SOIL_PGD_n* - routine to initialize thermal and hydrological
!!                              characteristics of road and building soil columns
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
!!	A. Lemonsu  *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_TEB_n,           ONLY : TEB_t
USE MODD_TEB_OPTION_n,    ONLY : TEB_OPTIONS_t
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODE_SOIL
!
USE MODI_HEATCAPZ
USE MODI_THRMCONDZ
USE MODI_ABOR1_SFX
USE MODI_READ_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(TEB_OPTIONS_t)     , INTENT(INOUT) :: TOP
TYPE(TEB_t)             , INTENT(INOUT) :: T
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
INTEGER                 , INTENT(IN)    :: KI             ! Number of grid points
REAL, DIMENSION(:),       INTENT(IN)    :: PPGD_CLAY      ! Clay fraction read in PGD file
REAL, DIMENSION(:),       INTENT(IN)    :: PPGD_SAND      ! Clay fraction read in PGD file
CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
CHARACTER(LEN=4) :: HPEDOTF
INTEGER          :: JLAYER         ! loop increment
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_SOIL_PGD_n',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
!*       2.      General soil characteristics
!                ----------------------------
!
ALLOCATE(T%XCLAY_ROAD    (KI,TOP%NTEB_SOIL))
ALLOCATE(T%XSAND_ROAD    (KI,TOP%NTEB_SOIL))
ALLOCATE(T%XCLAY_BLD     (KI,TOP%NTEB_SOIL))
ALLOCATE(T%XSAND_BLD     (KI,TOP%NTEB_SOIL))
!
!
!* clay and sand fractions are read in PGD.txt
!  (only 1 level is defined in PGD.txt)
!
DO JLAYER=1,TOP%NTEB_SOIL
  T%XCLAY_ROAD(:,JLAYER) = PPGD_CLAY(:)
  T%XSAND_ROAD(:,JLAYER) = PPGD_SAND(:)
  T%XCLAY_BLD (:,JLAYER) = PPGD_CLAY(:)
  T%XSAND_BLD (:,JLAYER) = PPGD_SAND(:)
ENDDO
!
!-------------------------------------------------------------------------------
!
!*       3.      Soil hydraulic characteristics for soil under roads/buildings
!                -------------------------------------------------------------
!
ALLOCATE(T%XCONDSAT_ROAD (KI,TOP%NTEB_SOIL))
ALLOCATE(T%XMPOTSAT_ROAD (KI,TOP%NTEB_SOIL))
ALLOCATE(T%XBCOEF_ROAD   (KI,TOP%NTEB_SOIL))
ALLOCATE(T%XWWILT_ROAD   (KI,TOP%NTEB_SOIL)) ! wilting point
ALLOCATE(T%XWFC_ROAD     (KI,TOP%NTEB_SOIL)) ! field capacity
ALLOCATE(T%XWSAT_ROAD    (KI,TOP%NTEB_SOIL)) ! saturation
!
ALLOCATE(T%XCONDSAT_BLD  (KI,TOP%NTEB_SOIL))
ALLOCATE(T%XMPOTSAT_BLD  (KI,TOP%NTEB_SOIL))
ALLOCATE(T%XBCOEF_BLD    (KI,TOP%NTEB_SOIL))
ALLOCATE(T%XWWILT_BLD    (KI,TOP%NTEB_SOIL)) ! wilting point
ALLOCATE(T%XWFC_BLD      (KI,TOP%NTEB_SOIL)) ! field capacity
ALLOCATE(T%XWSAT_BLD     (KI,TOP%NTEB_SOIL)) ! saturation
!
IF (GDM%O%CPEDOTF=='CH78'.OR.GDM%O%CPEDOTF=='CO84'.OR.GDM%O%CPEDOTF=='CP88'.OR.GDM%O%CPEDOTF=='WO99') THEN
  HPEDOTF = GDM%O%CPEDOTF
ELSE
  HPEDOTF = 'CH78'
ENDIF
!
DO JLAYER=1,TOP%NTEB_SOIL
  T%XBCOEF_ROAD  (:,JLAYER) = BCOEF_FUNC     (T%XCLAY_ROAD(:,JLAYER),T%XSAND_ROAD(:,JLAYER),HPEDOTF)
  T%XMPOTSAT_ROAD(:,JLAYER) = MATPOTSAT_FUNC (T%XCLAY_ROAD(:,JLAYER),T%XSAND_ROAD(:,JLAYER),HPEDOTF)
  T%XCONDSAT_ROAD(:,JLAYER) = HYDCONDSAT_FUNC(T%XCLAY_ROAD(:,JLAYER),T%XSAND_ROAD(:,JLAYER),HPEDOTF)
  T%XWSAT_ROAD   (:,JLAYER) = WSAT_FUNC      (T%XCLAY_ROAD(:,JLAYER),T%XSAND_ROAD(:,JLAYER),HPEDOTF)
  T%XWWILT_ROAD  (:,JLAYER) = WWILT_FUNC     (T%XCLAY_ROAD(:,JLAYER),T%XSAND_ROAD(:,JLAYER),HPEDOTF)
  T%XBCOEF_BLD   (:,JLAYER) = BCOEF_FUNC     (T%XCLAY_BLD (:,JLAYER),T%XSAND_BLD (:,JLAYER),HPEDOTF)
  T%XMPOTSAT_BLD (:,JLAYER) = MATPOTSAT_FUNC (T%XCLAY_BLD (:,JLAYER),T%XSAND_BLD (:,JLAYER),HPEDOTF)
  T%XCONDSAT_BLD (:,JLAYER) = HYDCONDSAT_FUNC(T%XCLAY_BLD (:,JLAYER),T%XSAND_BLD (:,JLAYER),HPEDOTF)
  T%XWSAT_BLD    (:,JLAYER) = WSAT_FUNC      (T%XCLAY_BLD (:,JLAYER),T%XSAND_BLD (:,JLAYER),HPEDOTF)
  T%XWWILT_BLD   (:,JLAYER) = WWILT_FUNC     (T%XCLAY_BLD (:,JLAYER),T%XSAND_BLD (:,JLAYER),HPEDOTF)
END DO
!
T%XWFC_ROAD(:,:) = W33_FUNC(T%XCLAY_ROAD(:,:),T%XSAND_ROAD(:,:),HPEDOTF)
T%XWFC_BLD (:,:) = W33_FUNC(T%XCLAY_BLD (:,:),T%XSAND_BLD (:,:),HPEDOTF)
!
!-------------------------------------------------------------------------------
!
!*       4.      Soil thermal characteristics for soil under buildings and roads
!                ---------------------------------------------------------------
!
ALLOCATE(T%XCONDDRY_ROAD (KI,TOP%NTEB_SOIL))
ALLOCATE(T%XCONDSLD_ROAD (KI,TOP%NTEB_SOIL))
ALLOCATE(T%XHCAPSOIL_ROAD(KI,TOP%NTEB_SOIL))
ALLOCATE(T%XCONDDRY_BLD  (KI,TOP%NTEB_SOIL))
ALLOCATE(T%XCONDSLD_BLD  (KI,TOP%NTEB_SOIL))
ALLOCATE(T%XHCAPSOIL_BLD (KI,TOP%NTEB_SOIL))
!
CALL THRMCONDZ(T%XSAND_ROAD,T%XWSAT_ROAD,T%XCONDDRY_ROAD,T%XCONDSLD_ROAD)
CALL THRMCONDZ(T%XSAND_BLD ,T%XWSAT_BLD ,T%XCONDDRY_BLD ,T%XCONDSLD_BLD )
CALL HEATCAPZ (T%XSAND_ROAD,T%XHCAPSOIL_ROAD)
CALL HEATCAPZ (T%XSAND_BLD ,T%XHCAPSOIL_BLD )
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_SOIL_PGD_n',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_TEB_SOIL_PGD_n
