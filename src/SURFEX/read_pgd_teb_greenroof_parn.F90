!     #########
      SUBROUTINE READ_PGD_TEB_GREENROOF_PAR_n(HPROGRAM)
!     ################################################
!
!!****  *READ_PGD_TEB_GREENROOF_PAR_n* - reads ISBA physiographic fields
!!                        
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      P. Le Moigne 12/2004 : add type of photosynthesis 
!!      C. de Munck  02/2012 : added parameterisation for sedum species under NVT_TROG 
!-------------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS,                 ONLY : XDAY
USE MODD_SURF_PAR,             ONLY : XUNDEF
USE MODD_TEB_GRID_n,           ONLY : NDIM
USE MODD_DATA_COVER_PAR,       ONLY : NVEGTYPE, NVT_GRAS, NVT_TROG
USE MODD_TEB_GREENROOF_n,      ONLY : NLAYER_GR, CTYP_GR, NTIME_GR,                             &
                                      XSAND_GR, XCLAY_GR
USE MODD_DATA_TEB_GREENROOF_n, ONLY : XPAR_LAI_GR,                                              &
                                      XPAR_OM_GR, XPAR_SAND_GR, XPAR_CLAY_GR,                   & 
                                      XPAR_VEG, XPAR_LAI,XPAR_RSMIN,XPAR_GAMMA,XPAR_WRMAX_CF,   &
                                      XPAR_RGL,XPAR_CV,XPAR_DG,XPAR_Z0,XPAR_Z0_O_Z0H,           &
                                      XPAR_ALBNIR_VEG,XPAR_ALBVIS_VEG, XPAR_ALBUV_VEG,          &
                                      XPAR_ALBNIR_SOIL,XPAR_ALBVIS_SOIL, XPAR_ALBUV_SOIL,       &
                                      XPAR_ALBNIR_DRY,XPAR_ALBVIS_DRY, XPAR_ALBUV_DRY,          &
                                      XPAR_ALBNIR_WET,XPAR_ALBVIS_WET, XPAR_ALBUV_WET,          &
                                      XPAR_EMIS,XPAR_DICE,                                      &
                                      XPAR_VEGTYPE,XPAR_ROOTFRAC,                               &
                                      XPAR_GMES,XPAR_BSLAI,XPAR_LAIMIN,XPAR_SEFOLD,XPAR_GC,     &
                                      XPAR_DMAX, XPAR_F2I, LDATA_STRESS, XPAR_H_TREE,XPAR_RE25, &
                                      XPAR_CE_NITRO,XPAR_CF_NITRO,XPAR_CNA_NITRO  
!paramètres ci-dessus à initialiser pour les GR (sauf XPAR_OM_GR, XPAR_SAND_GR, XPAR_CLAY_GR qui sont lues) 
USE MODD_PREP_TEB_GREENROOF,   ONLY : NGRID_LEVEL, XGRID_SOIL
!
USE MODI_READ_SURF
USE MODI_VEG_FROM_LAI
USE MODI_Z0V_FROM_LAI
USE MODI_EMIS_FROM_VEG
USE MODI_DRY_WET_SOIL_ALBEDOS
USE MODI_SOIL_ALBEDO
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                               :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12)                     :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100)                    :: YCOMMENT       ! Comment string
INTEGER                               :: JI             ! loop index
INTEGER                               :: JTIME          ! loop index
INTEGER                               :: JLAYER         ! loop index
!
REAL, DIMENSION(NDIM)                 :: ZDATA_WG1
REAL, DIMENSION(NDIM)                 :: ZDATA_WGSAT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.    Reading of PGD file
!              --------------------
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GREENROOF_PAR_N',0,ZHOOK_HANDLE)
!
YRECFM='GR_NTIME'
 CALL READ_SURF(HPROGRAM,YRECFM,NTIME_GR,IRESP)
!
YRECFM='GR_LAYER'
 CALL READ_SURF(HPROGRAM,YRECFM,NLAYER_GR,IRESP)
!
! Read type of green roof
YRECFM='D_TYPE_GR'
 CALL READ_SURF(HPROGRAM,YRECFM,CTYP_GR,IRESP)
!
! Read green roof OM fraction
ALLOCATE(XPAR_OM_GR     (NDIM,NLAYER_GR))
DO JLAYER=1,NLAYER_GR
  !WRITE(YRECFM,FMT='(A8,I1.1)') 'D_OM_GR0',JLAYER
  WRITE(YRECFM,FMT='(A7,I2.2)') 'D_OM_GR',JLAYER
  CALL READ_SURF(HPROGRAM,YRECFM,XPAR_OM_GR(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
! Read green roof SAND fraction
ALLOCATE(XPAR_SAND_GR   (NDIM,NLAYER_GR))
DO JLAYER=1,NLAYER_GR
  !WRITE(YRECFM,FMT='(A10,I1.1)') 'D_SAND_GR0',JLAYER
  WRITE(YRECFM,FMT='(A9,I2.2)') 'D_SAND_GR',JLAYER
  CALL READ_SURF(HPROGRAM,YRECFM,XPAR_SAND_GR(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
! Read green roof CLAY fraction
ALLOCATE(XPAR_CLAY_GR   (NDIM,NLAYER_GR))
DO JLAYER=1,NLAYER_GR
  !WRITE(YRECFM,FMT='(A10,I1.1)') 'D_CLAY_GR0',JLAYER
  WRITE(YRECFM,FMT='(A9,I2.2)') 'D_CLAY_GR',JLAYER
  CALL READ_SURF(HPROGRAM,YRECFM,XPAR_CLAY_GR(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
! Read green roof LAI
ALLOCATE(XPAR_LAI_GR    (NDIM,NTIME_GR))
DO JTIME=1,NTIME_GR
  WRITE(YRECFM,FMT='(A8,I2.2)') 'D_LAI_GR',JTIME
  CALL READ_SURF(HPROGRAM,YRECFM,XPAR_LAI_GR(:,JTIME),IRESP,HCOMMENT=YCOMMENT)
END DO
!
!
!-------------------------------------------------------------------------------
!
!*       2.    Definition of ISBA parameters
!              -----------------------------
!
ALLOCATE(XPAR_LAI        (NDIM,NTIME_GR))
ALLOCATE(XPAR_VEG        (NDIM,NTIME_GR))
ALLOCATE(XPAR_RSMIN      (NDIM))
ALLOCATE(XPAR_GAMMA      (NDIM))
ALLOCATE(XPAR_WRMAX_CF   (NDIM))
ALLOCATE(XPAR_RGL        (NDIM))
ALLOCATE(XPAR_CV         (NDIM))
ALLOCATE(XPAR_DG         (NDIM,NLAYER_GR))
ALLOCATE(XPAR_ROOTFRAC   (NDIM,NLAYER_GR))
ALLOCATE(XPAR_DICE       (NDIM))
ALLOCATE(XPAR_Z0         (NDIM,NTIME_GR))
ALLOCATE(XPAR_Z0_O_Z0H   (NDIM))
ALLOCATE(XPAR_ALBNIR_VEG (NDIM))
ALLOCATE(XPAR_ALBVIS_VEG (NDIM))
ALLOCATE(XPAR_ALBUV_VEG  (NDIM))
ALLOCATE(XPAR_ALBNIR_SOIL(NDIM))
ALLOCATE(XPAR_ALBVIS_SOIL(NDIM))
ALLOCATE(XPAR_ALBUV_SOIL (NDIM))
ALLOCATE(XPAR_ALBNIR_DRY (NDIM))
ALLOCATE(XPAR_ALBVIS_DRY (NDIM))
ALLOCATE(XPAR_ALBUV_DRY  (NDIM))
ALLOCATE(XPAR_ALBNIR_WET (NDIM))
ALLOCATE(XPAR_ALBVIS_WET (NDIM))
ALLOCATE(XPAR_ALBUV_WET  (NDIM))
ALLOCATE(XPAR_EMIS       (NDIM,NTIME_GR))
ALLOCATE(XPAR_VEGTYPE    (NDIM,NVEGTYPE))
ALLOCATE(XPAR_GMES       (NDIM))
ALLOCATE(XPAR_RE25       (NDIM))
ALLOCATE(XPAR_BSLAI      (NDIM))
ALLOCATE(XPAR_LAIMIN     (NDIM))
ALLOCATE(XPAR_SEFOLD     (NDIM))
ALLOCATE(XPAR_GC         (NDIM))
ALLOCATE(XPAR_DMAX       (NDIM))
ALLOCATE(XPAR_F2I        (NDIM))
ALLOCATE(LDATA_STRESS    (NDIM))
ALLOCATE(XPAR_H_TREE     (NDIM))
ALLOCATE(XPAR_CE_NITRO   (NDIM))
ALLOCATE(XPAR_CF_NITRO   (NDIM))
ALLOCATE(XPAR_CNA_NITRO  (NDIM))
!
XPAR_LAI          (:,:) = XUNDEF
XPAR_VEG          (:,:) = XUNDEF
XPAR_RSMIN          (:) = XUNDEF
XPAR_GAMMA          (:) = XUNDEF
XPAR_WRMAX_CF       (:) = XUNDEF
XPAR_RGL            (:) = XUNDEF
XPAR_CV             (:) = XUNDEF
XPAR_DG           (:,:) = XUNDEF
XPAR_DICE           (:) = XUNDEF
XPAR_ROOTFRAC     (:,:) = XUNDEF
XPAR_Z0           (:,:) = XUNDEF
XPAR_Z0_O_Z0H       (:) = XUNDEF
XPAR_ALBNIR_VEG     (:) = XUNDEF
XPAR_ALBVIS_VEG     (:) = XUNDEF
XPAR_ALBUV_VEG      (:) = XUNDEF
XPAR_ALBNIR_SOIL    (:) = XUNDEF
XPAR_ALBVIS_SOIL    (:) = XUNDEF
XPAR_ALBUV_SOIL     (:) = XUNDEF
XPAR_ALBNIR_DRY     (:) = XUNDEF
XPAR_ALBVIS_DRY     (:) = XUNDEF
XPAR_ALBUV_DRY      (:) = XUNDEF
XPAR_ALBNIR_WET     (:) = XUNDEF
XPAR_ALBVIS_WET     (:) = XUNDEF
XPAR_ALBUV_WET      (:) = XUNDEF
XPAR_EMIS         (:,:) = XUNDEF
XPAR_VEGTYPE      (:,:) = XUNDEF
XPAR_GMES           (:) = XUNDEF
XPAR_RE25           (:) = XUNDEF
XPAR_BSLAI          (:) = XUNDEF
XPAR_LAIMIN         (:) = XUNDEF
XPAR_SEFOLD         (:) = XUNDEF
XPAR_GC             (:) = XUNDEF
XPAR_DMAX           (:) = XUNDEF
XPAR_F2I            (:) = XUNDEF
LDATA_STRESS        (:) = .FALSE.
XPAR_H_TREE         (:) = XUNDEF
XPAR_CE_NITRO       (:) = XUNDEF
XPAR_CF_NITRO       (:) = XUNDEF
XPAR_CNA_NITRO      (:) = XUNDEF
!
!---------------------------------------------------------------------------
! Vegtypes adapted to greenroofs:
!--------------------------------
! NPATCH = 1 
! 2D cases : all greenroofs have same vegetation (defined by CTYP_GR)
! (CTYP_GR == 'GRASS') <=> NVT_GRASS (10)
!  ** OR **
! (CTYP_GR == 'SEDUM') <=> NVT_TROG (11)
! NB1: => no aggregation of vegetype parameters needed 
! NB2: Functions existing for gardens are used for initial greenroofs
!      This will need to be refined specifically for greenroofs
!
XPAR_VEGTYPE(:,:) = 0.
IF (CTYP_GR == 'GRASS') XPAR_VEGTYPE(:, NVT_GRAS) = 1.
IF (CTYP_GR == 'SEDUM') XPAR_VEGTYPE(:, NVT_TROG) = 1.
!--------------------------------------------------------------------------
!
! Dry/Wet soil albedos: (* Will need to account for XOM_GR eventually *)
!CALL DRY_WET_SOIL_ALBEDOS_1D(XSAND_GR(:,1),XCLAY_GR(:,1),                         &
 CALL DRY_WET_SOIL_ALBEDOS_1D(XPAR_SAND_GR(:,1),XPAR_CLAY_GR(:,1),              &
                               XPAR_VEGTYPE,                                   &
                               XPAR_ALBNIR_DRY,XPAR_ALBVIS_DRY,XPAR_ALBUV_DRY, &
                               XPAR_ALBNIR_WET,XPAR_ALBVIS_WET,XPAR_ALBUV_WET  ) 
!
! Critical normilized soil water content for stress parameterisation
XPAR_F2I(:) = 0.3
!
! Ratio between roughness length for momentum and heat
XPAR_Z0_O_Z0H(:) = 10.
!
! Defensive/offensive strategy (1/0)
LDATA_STRESS(:) = .FALSE. 
!
DO JI=1,NDIM
! 
! Vegetation albedo: near-IR, visible, and UV albedo
! * Will need to be adapted to greenroof GRASS and SEDUM species *
! * vérifier si/où l'abedo ds l'UV est utilisé *
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_ALBNIR_VEG(JI)= 0.3
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_ALBNIR_VEG(JI)= 0.154 ! mesures ONERA/Doya (2011)

 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_ALBVIS_VEG(JI)= 0.10
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_ALBVIS_VEG(JI)= 0.154 ! mesures ONERA/Doya (2011)

 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_ALBUV_VEG(JI) = 0.0800
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_ALBUV_VEG(JI) = 0.1250
!
! Soil albedo (* Will need to be refined for greenroofs - cf OM fraction *)
 ZDATA_WGSAT(:) = 0.
 ZDATA_WG1  (:) = 0.
 CALL SOIL_ALBEDO('DRY',                                              &
                    ZDATA_WGSAT, ZDATA_WG1,                           &
                    XPAR_ALBVIS_DRY, XPAR_ALBNIR_DRY, XPAR_ALBUV_DRY, &
                    XPAR_ALBVIS_WET, XPAR_ALBNIR_WET, XPAR_ALBUV_WET, &
                    XPAR_ALBVIS_SOIL,XPAR_ALBNIR_SOIL,XPAR_ALBUV_SOIL )  
!
! Min stomatal resistance  
 !IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_RSMIN(JI)= 40 (dans isba & garden)
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_RSMIN(JI)= 120  ! for GRASS
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_RSMIN(JI)= 150. ! for SEDUM
 !IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_RSMIN(JI)= 120.
! 
! Gamma parameter 
! (* Check if values needs to be refined for GRASS and SEDUM *)
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_GAMMA(JI)= 0.
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_GAMMA(JI)= 0.
!
! Wrmax_cf 
! (* Check if needs to be refined for GRASS and SEDUM greenroofs *)
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_WRMAX_CF(JI)= 0.2
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_WRMAX_CF(JI)= 0.2
!
! Rgl 
! (* Check if needs to be refined for GRASS and SEDUM greenroofs *)
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_RGL(JI)= 100.
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_RGL(JI)= 100.
!
! Cv 
! (* Check if needs to be refined for GRASS and SEDUM greenroofs *)
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_CV(JI)= 2.E-5
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_CV(JI)= 2.E-5
!
!! Mesophyll conductance (m s-1) 
! (* Check if needs to be refined for GRASS and SEDUM greenroofs *)
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_GMES(JI)= 0.020
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_GMES(JI)= 0.020
 !IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_GMES(JI)= 0.003
!
! Ecosystem Respiration (kg/kg.m.s-1)
! (* Check if needs to be refined for GRASS and SEDUM greenroofs *)
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0.  )  XPAR_RE25(JI)= 3.0E-7
 IF(XPAR_VEGTYPE(JI,NVT_TROG  )>0.)  XPAR_RE25(JI)= 3.0E-7
!
! Cuticular conductance (m s-1)
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_GC(JI)= 0.00025
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_GC(JI)= 0.00025        
!
! Ratio d(biomass)/d(lai) (kg/m2)
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_BSLAI(JI)= 0.36
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_BSLAI(JI)= 0.06
!
! Maximum air saturation deficit tolerate by vegetation (kg/kg)
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_DMAX(JI)= 0.1
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_DMAX(JI)= 0.1
!
! e-folding time for senescence (days)
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_SEFOLD(JI)=  90.* XDAY
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_SEFOLD(JI)=  60.* XDAY
!
! Minimum LAI (m2/m2)
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_LAIMIN (JI) = 0.3
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_LAIMIN (JI) = 0.3
!
! Leaf aera ratio sensitivity to nitrogen concentration
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_CE_NITRO(JI)= 5.56
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_CE_NITRO(JI)= 3.79
!
! Lethal minimum value of leaf area ratio
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_CF_NITRO(JI)=  6.73
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_CF_NITRO(JI)=  9.84
!
! Nitrogen concentration of active biomass
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_CNA_NITRO(JI)= 1.9
 IF(XPAR_VEGTYPE(JI,NVT_TROG )>0.)  XPAR_CNA_NITRO(JI)= 1.3
!
! Depth of greenroof ground layers
 XPAR_DG(JI, 1) = XGRID_SOIL(NGRID_LEVEL - 5)
 XPAR_DG(JI, 2) = XGRID_SOIL(NGRID_LEVEL - 4)
 XPAR_DG(JI, 3) = XGRID_SOIL(NGRID_LEVEL - 3)
 XPAR_DG(JI, 4) = XGRID_SOIL(NGRID_LEVEL - 2)
 XPAR_DG(JI, 5) = XGRID_SOIL(NGRID_LEVEL - 1)
 XPAR_DG(JI, 6) = XGRID_SOIL(NGRID_LEVEL - 0)
!
! Root fractions
 XPAR_ROOTFRAC(JI, 1)  = 0.04
 XPAR_ROOTFRAC(JI, 2)  = 0.36
 XPAR_ROOTFRAC(JI, 3)  = 0.68
 XPAR_ROOTFRAC(JI, 4)  = 1.
 XPAR_ROOTFRAC(JI, 5)  = 1.
 XPAR_ROOTFRAC(JI, 6)  = 1.
!
! Depth of the soil column for the calculation of the frozen soil fraction (m)
 XPAR_DICE(JI) = XPAR_DG(JI,1) 
!
DO JTIME=1,NTIME_GR
! Leaf Area Index
 XPAR_LAI(JI,JTIME) = XPAR_LAI_GR(JI,JTIME)

! Fraction of vegetation on greenroof
!* Will need to be refined for greenroofs *)
  !XPAR_VEG (JI,1,JTIME) = VEG_FROM_LAI (XPAR_LAI_GR(JI,JTIME),   &
  !                                       XPAR_VEGTYPE(JI,:))  
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )   XPAR_VEG (JI,JTIME) = 0.9
 !IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )   XPAR_VEG (JI,JTIME) = 1.0
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )   XPAR_VEG (JI,JTIME) = 0.95

! Roughness length for momentum
!* Will need to be refined for greenroofs *)
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )   XPAR_Z0 (JI,JTIME) = 0.01
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )   XPAR_Z0 (JI,JTIME) = 0.01
 !                                        
! Emissivity
!* Will need to be refined for greenroofs *)
  !XPAR_EMIS (JI,1,JTIME) = EMIS_FROM_VEG (XPAR_VEG    (JI,1,JTIME),&
  !                                         XPAR_VEGTYPE(JI,:))  
 IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )   XPAR_EMIS (JI,JTIME) = 0.95 
 IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )   XPAR_EMIS (JI,JTIME) = 0.83 ! Feng. et al. (2010)

END DO
!
ENDDO
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GREENROOF_PAR_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_TEB_GREENROOF_PAR_n
