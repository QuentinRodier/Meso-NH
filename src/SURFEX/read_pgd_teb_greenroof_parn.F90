!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_TEB_GREENROOF_PAR_n (&
                                                DTGR, TGRO, TG, &
                                               HPROGRAM)
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
!!      V. Masson   *Meteo France*
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
!
!
!
!
USE MODD_DATA_TEB_GREENROOF_n, ONLY : DATA_TEB_GREENROOF_t
USE MODD_TEB_GREENROOF_OPTION_n, ONLY : TEB_GREENROOF_OPTIONS_t
USE MODD_TEB_GRID_n, ONLY : TEB_GRID_t
!
USE MODD_CSTS,                 ONLY : XDAY
USE MODD_SURF_PAR,             ONLY : XUNDEF
USE MODD_DATA_COVER_PAR,       ONLY : NVEGTYPE, NVT_GRAS, NVT_TROG
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
!
!
!
TYPE(DATA_TEB_GREENROOF_t), INTENT(INOUT) :: DTGR
TYPE(TEB_GREENROOF_OPTIONS_t), INTENT(INOUT) :: TGRO
TYPE(TEB_GRID_t), INTENT(INOUT) :: TG
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
REAL, DIMENSION(TG%NDIM)                 :: ZDATA_WG1
REAL, DIMENSION(TG%NDIM)                 :: ZDATA_WGSAT
!
LOGICAL :: GAGRI_TO_GRASS
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.    Reading of PGD file
!              --------------------
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GREENROOF_PAR_N',0,ZHOOK_HANDLE)
!
GAGRI_TO_GRASS=.FALSE.
!
YRECFM='GR_NTIME'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,TGRO%NTIME_GR,IRESP)
!
YRECFM='GR_LAYER'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,TGRO%NLAYER_GR,IRESP)
!
! Read type of green roof
YRECFM='D_TYPE_GR'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,TGRO%CTYP_GR,IRESP)
!
! Read green roof OM fraction
ALLOCATE(DTGR%XPAR_OM_GR     (TG%NDIM,TGRO%NLAYER_GR))
DO JLAYER=1,TGRO%NLAYER_GR
  !WRITE(YRECFM,FMT='(A8,I1.1)') 'D_OM_GR0',JLAYER
  WRITE(YRECFM,FMT='(A7,I2.2)') 'D_OM_GR',JLAYER
  CALL READ_SURF(&
                HPROGRAM,YRECFM,DTGR%XPAR_OM_GR(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
! Read green roof SAND fraction
ALLOCATE(DTGR%XPAR_SAND_GR   (TG%NDIM,TGRO%NLAYER_GR))
DO JLAYER=1,TGRO%NLAYER_GR
  !WRITE(YRECFM,FMT='(A10,I1.1)') 'D_SAND_GR0',JLAYER
  WRITE(YRECFM,FMT='(A9,I2.2)') 'D_SAND_GR',JLAYER
  CALL READ_SURF(&
                HPROGRAM,YRECFM,DTGR%XPAR_SAND_GR(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
! Read green roof CLAY fraction
ALLOCATE(DTGR%XPAR_CLAY_GR   (TG%NDIM,TGRO%NLAYER_GR))
DO JLAYER=1,TGRO%NLAYER_GR
  !WRITE(YRECFM,FMT='(A10,I1.1)') 'D_CLAY_GR0',JLAYER
  WRITE(YRECFM,FMT='(A9,I2.2)') 'D_CLAY_GR',JLAYER
  CALL READ_SURF(&
                HPROGRAM,YRECFM,DTGR%XPAR_CLAY_GR(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
! Read green roof LAI
ALLOCATE(DTGR%XPAR_LAI_GR    (TG%NDIM,TGRO%NTIME_GR))
DO JTIME=1,TGRO%NTIME_GR
  WRITE(YRECFM,FMT='(A8,I2.2)') 'D_LAI_GR',JTIME
  CALL READ_SURF(&
                HPROGRAM,YRECFM,DTGR%XPAR_LAI_GR(:,JTIME),IRESP,HCOMMENT=YCOMMENT)
END DO
!
!
!-------------------------------------------------------------------------------
!
!*       2.    Definition of ISBA parameters
!              -----------------------------
!
ALLOCATE(DTGR%XPAR_LAI        (TG%NDIM,TGRO%NTIME_GR))
ALLOCATE(DTGR%XPAR_VEG        (TG%NDIM,TGRO%NTIME_GR))
ALLOCATE(DTGR%XPAR_RSMIN      (TG%NDIM))
ALLOCATE(DTGR%XPAR_GAMMA      (TG%NDIM))
ALLOCATE(DTGR%XPAR_WRMAX_CF   (TG%NDIM))
ALLOCATE(DTGR%XPAR_RGL        (TG%NDIM))
ALLOCATE(DTGR%XPAR_CV         (TG%NDIM))
ALLOCATE(DTGR%XPAR_DG         (TG%NDIM,TGRO%NLAYER_GR))
ALLOCATE(DTGR%XPAR_ROOTFRAC   (TG%NDIM,TGRO%NLAYER_GR))
ALLOCATE(DTGR%XPAR_DICE       (TG%NDIM))
ALLOCATE(DTGR%XPAR_Z0         (TG%NDIM,TGRO%NTIME_GR))
ALLOCATE(DTGR%XPAR_Z0_O_Z0H   (TG%NDIM))
ALLOCATE(DTGR%XPAR_ALBNIR_VEG (TG%NDIM))
ALLOCATE(DTGR%XPAR_ALBVIS_VEG (TG%NDIM))
ALLOCATE(DTGR%XPAR_ALBUV_VEG  (TG%NDIM))
ALLOCATE(DTGR%XPAR_ALBNIR_SOIL(TG%NDIM))
ALLOCATE(DTGR%XPAR_ALBVIS_SOIL(TG%NDIM))
ALLOCATE(DTGR%XPAR_ALBUV_SOIL (TG%NDIM))
ALLOCATE(DTGR%XPAR_ALBNIR_DRY (TG%NDIM))
ALLOCATE(DTGR%XPAR_ALBVIS_DRY (TG%NDIM))
ALLOCATE(DTGR%XPAR_ALBUV_DRY  (TG%NDIM))
ALLOCATE(DTGR%XPAR_ALBNIR_WET (TG%NDIM))
ALLOCATE(DTGR%XPAR_ALBVIS_WET (TG%NDIM))
ALLOCATE(DTGR%XPAR_ALBUV_WET  (TG%NDIM))
ALLOCATE(DTGR%XPAR_EMIS       (TG%NDIM,TGRO%NTIME_GR))
ALLOCATE(DTGR%XPAR_VEGTYPE    (TG%NDIM,NVEGTYPE))
ALLOCATE(DTGR%XPAR_GMES       (TG%NDIM))
ALLOCATE(DTGR%XPAR_RE25       (TG%NDIM))
ALLOCATE(DTGR%XPAR_BSLAI      (TG%NDIM))
ALLOCATE(DTGR%XPAR_LAIMIN     (TG%NDIM))
ALLOCATE(DTGR%XPAR_SEFOLD     (TG%NDIM))
ALLOCATE(DTGR%XPAR_GC         (TG%NDIM))
ALLOCATE(DTGR%XPAR_DMAX       (TG%NDIM))
ALLOCATE(DTGR%XPAR_F2I        (TG%NDIM))
ALLOCATE(DTGR%LDATA_STRESS    (TG%NDIM))
ALLOCATE(DTGR%XPAR_H_TREE     (TG%NDIM))
ALLOCATE(DTGR%XPAR_CE_NITRO   (TG%NDIM))
ALLOCATE(DTGR%XPAR_CF_NITRO   (TG%NDIM))
ALLOCATE(DTGR%XPAR_CNA_NITRO  (TG%NDIM))
!
DTGR%XPAR_LAI          (:,:) = XUNDEF
DTGR%XPAR_VEG          (:,:) = XUNDEF
DTGR%XPAR_RSMIN          (:) = XUNDEF
DTGR%XPAR_GAMMA          (:) = XUNDEF
DTGR%XPAR_WRMAX_CF       (:) = XUNDEF
DTGR%XPAR_RGL            (:) = XUNDEF
DTGR%XPAR_CV             (:) = XUNDEF
DTGR%XPAR_DG           (:,:) = XUNDEF
DTGR%XPAR_DICE           (:) = XUNDEF
DTGR%XPAR_ROOTFRAC     (:,:) = XUNDEF
DTGR%XPAR_Z0           (:,:) = XUNDEF
DTGR%XPAR_Z0_O_Z0H       (:) = XUNDEF
DTGR%XPAR_ALBNIR_VEG     (:) = XUNDEF
DTGR%XPAR_ALBVIS_VEG     (:) = XUNDEF
DTGR%XPAR_ALBUV_VEG      (:) = XUNDEF
DTGR%XPAR_ALBNIR_SOIL    (:) = XUNDEF
DTGR%XPAR_ALBVIS_SOIL    (:) = XUNDEF
DTGR%XPAR_ALBUV_SOIL     (:) = XUNDEF
DTGR%XPAR_ALBNIR_DRY     (:) = XUNDEF
DTGR%XPAR_ALBVIS_DRY     (:) = XUNDEF
DTGR%XPAR_ALBUV_DRY      (:) = XUNDEF
DTGR%XPAR_ALBNIR_WET     (:) = XUNDEF
DTGR%XPAR_ALBVIS_WET     (:) = XUNDEF
DTGR%XPAR_ALBUV_WET      (:) = XUNDEF
DTGR%XPAR_EMIS         (:,:) = XUNDEF
DTGR%XPAR_VEGTYPE      (:,:) = XUNDEF
DTGR%XPAR_GMES           (:) = XUNDEF
DTGR%XPAR_RE25           (:) = XUNDEF
DTGR%XPAR_BSLAI          (:) = XUNDEF
DTGR%XPAR_LAIMIN         (:) = XUNDEF
DTGR%XPAR_SEFOLD         (:) = XUNDEF
DTGR%XPAR_GC             (:) = XUNDEF
DTGR%XPAR_DMAX           (:) = XUNDEF
DTGR%XPAR_F2I            (:) = XUNDEF
DTGR%LDATA_STRESS        (:) = .FALSE.
DTGR%XPAR_H_TREE         (:) = XUNDEF
DTGR%XPAR_CE_NITRO       (:) = XUNDEF
DTGR%XPAR_CF_NITRO       (:) = XUNDEF
DTGR%XPAR_CNA_NITRO      (:) = XUNDEF
!
!---------------------------------------------------------------------------
! Vegtypes adapted to greenroofs:
!--------------------------------
! NPATCH = 1 
! 2D cases : all greenroofs have same vegetation (defined by CTYP_GR)
! (CTYP_GR == 'GRASS') <=> NVT_GRAS (10)
!  ** OR **
! (CTYP_GR == 'SEDUM') <=> NVT_TROG (11)
! NB1: => no aggregation of vegetype parameters needed 
! NB2: Functions existing for gardens are used for initial greenroofs
!      This will need to be refined specifically for greenroofs
!
DTGR%XPAR_VEGTYPE(:,:) = 0.
IF (TGRO%CTYP_GR == 'GRASS') DTGR%XPAR_VEGTYPE(:, NVT_GRAS) = 1.
IF (TGRO%CTYP_GR == 'SEDUM') DTGR%XPAR_VEGTYPE(:, NVT_TROG) = 1.
!--------------------------------------------------------------------------
!
! Dry/Wet soil albedos: (* Will need to account for XOM_GR eventually *)
!CALL DRY_WET_SOIL_ALBEDOS_1D(XSAND_GR(:,1),XCLAY_GR(:,1),                         &
 CALL DRY_WET_SOIL_ALBEDOS_1D(DTGR%XPAR_SAND_GR(:,1),DTGR%XPAR_CLAY_GR(:,1),              &
                               DTGR%XPAR_VEGTYPE,                                   &
                               DTGR%XPAR_ALBNIR_DRY,DTGR%XPAR_ALBVIS_DRY,DTGR%XPAR_ALBUV_DRY, &
                               DTGR%XPAR_ALBNIR_WET,DTGR%XPAR_ALBVIS_WET,DTGR%XPAR_ALBUV_WET  ) 
!
! Critical normilized soil water content for stress parameterisation
DTGR%XPAR_F2I(:) = 0.3
!
! Ratio between roughness length for momentum and heat
DTGR%XPAR_Z0_O_Z0H(:) = 10.
!
! Defensive/offensive strategy (1/0)
DTGR%LDATA_STRESS(:) = .FALSE. 
!
DO JI=1,TG%NDIM
! 
! Vegetation albedo: near-IR, visible, and UV albedo
! * Will need to be adapted to greenroof GRASS and SEDUM species *
! * vérifier si/où l'abedo ds l'UV est utilisé *
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_ALBNIR_VEG(JI)= 0.3
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_ALBNIR_VEG(JI)= 0.154 ! mesures ONERA/Doya (2011)

 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_ALBVIS_VEG(JI)= 0.10
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_ALBVIS_VEG(JI)= 0.154 ! mesures ONERA/Doya (2011)

 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_ALBUV_VEG(JI) = 0.0800
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_ALBUV_VEG(JI) = 0.1250
!
! Soil albedo (* Will need to be refined for greenroofs - cf OM fraction *)
 ZDATA_WGSAT(:) = 0.
 ZDATA_WG1  (:) = 0.
 CALL SOIL_ALBEDO('DRY',                                              &
                    ZDATA_WGSAT, ZDATA_WG1,                           &
                    DTGR%XPAR_ALBVIS_DRY, DTGR%XPAR_ALBNIR_DRY, DTGR%XPAR_ALBUV_DRY, &
                    DTGR%XPAR_ALBVIS_WET, DTGR%XPAR_ALBNIR_WET, DTGR%XPAR_ALBUV_WET, &
                    DTGR%XPAR_ALBVIS_SOIL,DTGR%XPAR_ALBNIR_SOIL,DTGR%XPAR_ALBUV_SOIL )  
!
! Min stomatal resistance  
 !IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_RSMIN(JI)= 40 (dans isba & garden)
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_RSMIN(JI)= 120  ! for GRASS
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_RSMIN(JI)= 150. ! for SEDUM
 !IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_RSMIN(JI)= 120.
! 
! Gamma parameter 
! (* Check if values needs to be refined for GRASS and SEDUM *)
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_GAMMA(JI)= 0.
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_GAMMA(JI)= 0.
!
! Wrmax_cf 
! (* Check if needs to be refined for GRASS and SEDUM greenroofs *)
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_WRMAX_CF(JI)= 0.2
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_WRMAX_CF(JI)= 0.2
!
! Rgl 
! (* Check if needs to be refined for GRASS and SEDUM greenroofs *)
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_RGL(JI)= 100.
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_RGL(JI)= 100.
!
! Cv 
! (* Check if needs to be refined for GRASS and SEDUM greenroofs *)
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_CV(JI)= 2.E-5
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_CV(JI)= 2.E-5
!
!! Mesophyll conductance (m s-1) 
! (* Check if needs to be refined for GRASS and SEDUM greenroofs *)
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_GMES(JI)= 0.020
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_GMES(JI)= 0.020
 !IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_GMES(JI)= 0.003
!
! Ecosystem Respiration (kg/kg.m.s-1)
! (* Check if needs to be refined for GRASS and SEDUM greenroofs *)
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0.  )  DTGR%XPAR_RE25(JI)= 3.0E-7
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG  )>0.)  DTGR%XPAR_RE25(JI)= 3.0E-7
!
! Cuticular conductance (m s-1)
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_GC(JI)= 0.00025
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_GC(JI)= 0.00025        
!
! Ratio d(biomass)/d(lai) (kg/m2)
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_BSLAI(JI)= 0.36
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_BSLAI(JI)= 0.06
!
! Maximum air saturation deficit tolerate by vegetation (kg/kg)
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_DMAX(JI)= 0.1
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_DMAX(JI)= 0.1
!
! e-folding time for senescence (days)
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_SEFOLD(JI)=  90.* XDAY
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_SEFOLD(JI)=  60.* XDAY
!
! Minimum LAI (m2/m2)
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_LAIMIN (JI) = 0.3
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_LAIMIN (JI) = 0.3
!
! Leaf aera ratio sensitivity to nitrogen concentration
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_CE_NITRO(JI)= 5.56
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_CE_NITRO(JI)= 3.79
!
! Lethal minimum value of leaf area ratio
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_CF_NITRO(JI)=  6.73
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTGR%XPAR_CF_NITRO(JI)=  9.84
!
! Nitrogen concentration of active biomass
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTGR%XPAR_CNA_NITRO(JI)= 1.9
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG )>0.)  DTGR%XPAR_CNA_NITRO(JI)= 1.3
!
! Depth of greenroof ground layers
 DTGR%XPAR_DG(JI, 1) = XGRID_SOIL(NGRID_LEVEL - 5)
 DTGR%XPAR_DG(JI, 2) = XGRID_SOIL(NGRID_LEVEL - 4)
 DTGR%XPAR_DG(JI, 3) = XGRID_SOIL(NGRID_LEVEL - 3)
 DTGR%XPAR_DG(JI, 4) = XGRID_SOIL(NGRID_LEVEL - 2)
 DTGR%XPAR_DG(JI, 5) = XGRID_SOIL(NGRID_LEVEL - 1)
 DTGR%XPAR_DG(JI, 6) = XGRID_SOIL(NGRID_LEVEL - 0)
!
! Root fractions
 DTGR%XPAR_ROOTFRAC(JI, 1)  = 0.04
 DTGR%XPAR_ROOTFRAC(JI, 2)  = 0.36
 DTGR%XPAR_ROOTFRAC(JI, 3)  = 0.68
 DTGR%XPAR_ROOTFRAC(JI, 4)  = 1.
 DTGR%XPAR_ROOTFRAC(JI, 5)  = 1.
 DTGR%XPAR_ROOTFRAC(JI, 6)  = 1.
!
! Depth of the soil column for the calculation of the frozen soil fraction (m)
 DTGR%XPAR_DICE(JI) = DTGR%XPAR_DG(JI,1) 
!
DO JTIME=1,TGRO%NTIME_GR
! Leaf Area Index
 DTGR%XPAR_LAI(JI,JTIME) = DTGR%XPAR_LAI_GR(JI,JTIME)

! Fraction of vegetation on greenroof
!* Will need to be refined for greenroofs *)
  !XPAR_VEG (JI,1,JTIME) = VEG_FROM_LAI (XPAR_LAI_GR(JI,JTIME),   &
  !                                       XPAR_VEGTYPE(JI,:),GAGRI_TO_GRASS)  
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )   DTGR%XPAR_VEG (JI,JTIME) = 0.9
 !IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )   XPAR_VEG (JI,JTIME) = 1.0
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )   DTGR%XPAR_VEG (JI,JTIME) = 0.95

! Roughness length for momentum
!* Will need to be refined for greenroofs *)
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )   DTGR%XPAR_Z0 (JI,JTIME) = 0.01
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )   DTGR%XPAR_Z0 (JI,JTIME) = 0.01
 !                                        
! Emissivity
!* Will need to be refined for greenroofs *)
  !XPAR_EMIS (JI,1,JTIME) = EMIS_FROM_VEG (XPAR_VEG    (JI,1,JTIME),&
  !                                         XPAR_VEGTYPE(JI,:))  
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )   DTGR%XPAR_EMIS (JI,JTIME) = 0.95 
 IF(DTGR%XPAR_VEGTYPE(JI,NVT_TROG)>0. )   DTGR%XPAR_EMIS (JI,JTIME) = 0.83 ! Feng. et al. (2010)

END DO
!
ENDDO
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GREENROOF_PAR_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_TEB_GREENROOF_PAR_n
