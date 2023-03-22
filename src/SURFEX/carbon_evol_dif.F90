!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE CARBON_EVOL_DIF(IO, KK, PK, PEK, DK, DEK, DMK, HPROGRAM, PTSTEP,& 
                               PRHOA, PCO2, PPS, PTA, PRESP_BIOMASS_INST       )
!   ###############################################################
!!****  *CARBON EVOL*
!!
!!    PURPOSE
!!    -------
!!
!!    Soil Carbon evolution based on Morel et al. (2019). 
!!            
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      Gibelin et al. 2008, AFM
!!      Morel et al. (2019) JAMES
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Decharme       * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    22/06/20
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n,   ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,           ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_n,           ONLY : DIAG_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_CO2V_PAR,       ONLY : XMC, XMO2, XMCO2, XMCH4, XPCCO2, &
                                XGTOKG, XALPHA_DOC, XKGTOG
USE MODD_SOILGAS_PAR,    ONLY : XO2_ATMO
USE MODD_CSTS,           ONLY : XTT, XMD
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_CONTROL_MOIST_FUNC
USE MODI_CONTROL_TEMP_FUNC
!
USE MODI_CARBON_LITTER_SURFACE
USE MODI_CARBON_SOILDIF_PROPERTIES
USE MODI_CARBON_SOILDIF_FLUXES
USE MODI_CARBON_LITTER_SOILDIF
USE MODI_CARBON_SOILDIF
USE MODI_CARBON_DYNAMIC
!
USE MODI_GAS_SOILDIF_PROPERTIES
USE MODI_GAS_SOILDIF_FLUXES
USE MODI_GAS_SOILDIF
!
USE MODI_GET_LUOUT
!
USE YOMHOOK   ,ONLY : LHOOK, DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
!*      0.1    declarations of arguments
!
!
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: IO
TYPE(ISBA_K_t),         INTENT(INOUT) :: KK
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
TYPE(DIAG_t),           INTENT(INOUT) :: DK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
CHARACTER(LEN=6),       INTENT(IN)    :: HPROGRAM  ! program calling surf. schemes
!
REAL,                   INTENT(IN)    :: PTSTEP                 ! time step
!
REAL, DIMENSION(:),     INTENT(IN)    :: PRHOA         ! air density (kg/m3)
REAL, DIMENSION(:),     INTENT(IN)    :: PCO2          ! Atmospĥeric CO2 concentration per unit of air mass (kg/kg)
REAL, DIMENSION(:),     INTENT(IN)    :: PPS           ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(:),     INTENT(IN)    :: PTA           ! air temperature forcing               (K)
!
REAL, DIMENSION(:,:),   INTENT(IN)    :: PRESP_BIOMASS_INST ! instantaneous respiration of biomass (kgCO2/m2/s)
!
!
!-------------------------------------------------------------------------------
!
!*      0.2 local parameters
!
REAL, PARAMETER                       :: ZDTOP  = 0.1     ! Top depth m
!
REAL, PARAMETER                       :: ZTIMEMAX  = 300. ! s  Maximum timescale without time spliting
!
REAL, PARAMETER                       :: ZCH4_ATMO = 1.8  ! ppmv (must be in the forcing file)
!
CHARACTER(LEN=6), PARAMETER           :: CEBUL    = 'ADVECT' ! Method to average water and air diffusivity
                                                             ! 'ADVECT' = Ebullution as an advective layer-by-layer transport (Morel et al. 2019)
                                                             ! 'DIRECT' = Ebullution as direct emissions
!
!-------------------------------------------------------------------------------
!                                                          
!*      0.3    declarations of local variables
!
REAL, DIMENSION(SIZE(PRHOA))             :: ZTG_LIT                ! Surface litter temperature    (K)
REAL, DIMENSION(SIZE(PRHOA))             :: ZMOIST_LIT, ZMOIST_WRK ! Surface litter moisture       (-)
REAL, DIMENSION(SIZE(PRHOA))             :: ZSAT_LIT, ZSAT_WRK     ! Surface litter moisture       (-)
REAL, DIMENSION(SIZE(PRHOA))             :: ZDBIO                  !Bioturbation limit (m)
REAL, DIMENSION(SIZE(PRHOA))             :: ZDCRYO                 !Cryoturbation limit (m)
!
REAL, DIMENSION(SIZE(PRHOA))             :: ZFDOC_LITTER_SURF, ZFDOC_LITTER_SOIL, ZFDOC_SOILCARBON
!
REAL, DIMENSION(SIZE(PRHOA))             :: ZRESP_HETERO_SOIL   ! gCO2/m2/s
REAL, DIMENSION(SIZE(PRHOA))             :: ZRESP_HETERO_LITTER ! gCO2/m2/s
REAL, DIMENSION(SIZE(PRHOA))             :: ZFLUX_O2_SOIL       ! gO2/m2/s
REAL, DIMENSION(SIZE(PRHOA))             :: ZFLUX_CH4_SOIL      ! gCH4/m2/s
!
REAL, DIMENSION(SIZE(PRHOA))             :: ZCONTROL_TEMP_SURF, ZCONTROL_MOIST_SURF, ZCONTROL_LEACH_SURF
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZWGHT_SURF   ! Weight surface litter for DIF (m)   

REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZCONTROL_TEMP, ZCONTROL_MOIST, ZCONTROL_LEACH_SOIL, &
                                                    ZCONTROL_TEMP_MG, ZCONTROL_MG
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZDIFBIO  !Bioturbation diffusivity profile (m2/s)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZDIFCRYO !Cryoturbation diffusivity profile (m2/s)
!
REAL, DIMENSION(SIZE(PEK%XSOILDIF_LITTER,1),SIZE(PEK%XSOILDIF_LITTER,2),SIZE(PEK%XSOILDIF_LITTER,3)) :: ZFOXIC_LITTER   ! oxic flux out of soil litter pools by layers (gC/m**2)
REAL, DIMENSION(SIZE(PEK%XSOILDIF_LITTER,1),SIZE(PEK%XSOILDIF_LITTER,2),SIZE(PEK%XSOILDIF_LITTER,3)) :: ZFMG_LITTER     ! methanogenesis flux out of soil litter pools by layers (gC/m**2)
REAL, DIMENSION(SIZE(PEK%XSOILDIF_LITTER,1),SIZE(PEK%XSOILDIF_LITTER,2),SIZE(PEK%XSOILDIF_LITTER,3)) :: ZFLEACH_LITTER  ! leaching flux out of soil litter pools by layers (gC/m**2)
!
REAL, DIMENSION(SIZE(PEK%XSOILDIF_CARB,1),SIZE(PEK%XSOILDIF_CARB,2),SIZE(PEK%XSOILDIF_CARB,3)) :: ZSOILCARBON_INPUT ! quantity of carbon going into carbon pools from litter decomposition (gC/m**2/s)
REAL, DIMENSION(SIZE(PEK%XSOILDIF_CARB,1),SIZE(PEK%XSOILDIF_CARB,2),SIZE(PEK%XSOILDIF_CARB,3)) :: ZFOXIC_SOC        ! oxic flux out of carbon pools by layers (gC/m**2)
REAL, DIMENSION(SIZE(PEK%XSOILDIF_CARB,1),SIZE(PEK%XSOILDIF_CARB,2),SIZE(PEK%XSOILDIF_CARB,3)) :: ZFLEACH_SOC       ! leaching flux out of carbon pools by layers (gC/m**2)
!
REAL, DIMENSION(SIZE(PEK%XSOILDIF_CARB,1),SIZE(PEK%XSOILDIF_CARB,2))  :: ZFMG_SOC          ! methanogenesis flux out of carbon pools by layers (gC/m**2)
!
!-------------------------------------------------------------------------------
!
!*      0.6    declarations of local variables for soil gas scheme
!
REAL, DIMENSION(SIZE(PEK%XTG,1))                 :: ZO2_ATM        ! O2 atmospheric concentrations for boundary conditions (g/m3 air)
REAL, DIMENSION(SIZE(PEK%XTG,1))                 :: ZCO2_ATM       ! CO2 atmospheric concentrations for boundary conditions (g/m3 air)
REAL, DIMENSION(SIZE(PEK%XTG,1))                 :: ZCH4_ATM       ! CH4 atmospheric concentrations for boundary conditions (g/m3 air)
!
REAL, DIMENSION(SIZE(PEK%XTG,1))                 :: ZPI_AERENCHYMA ! Vegetation (aerenchyma) permeability for PMT
!
REAL, DIMENSION(SIZE(PEK%XTG,1))                 :: ZKO2_AVEG      ! O2 gas conductivity coefficient in the air into plant media (m/s)
REAL, DIMENSION(SIZE(PEK%XTG,1))                 :: ZKCO2_AVEG     ! CO2 gas conductivity coefficient in the air into plant media (m/s)
REAL, DIMENSION(SIZE(PEK%XTG,1))                 :: ZKCH4_AVEG     ! CH4 gas conductivity coefficient in the air into plant media (m/s)
!
REAL, DIMENSION(SIZE(PEK%XTG,1))                 :: ZKO2_SURF      ! Bulk O2 gas conductivity coefficient at soil/snow/atmosphere interface (m/s)
REAL, DIMENSION(SIZE(PEK%XTG,1))                 :: ZKCO2_SURF     ! Bulk CO2 gas conductivity coefficient at soil/snow/atmosphere interface (m/s)
REAL, DIMENSION(SIZE(PEK%XTG,1))                 :: ZKCH4_SURF     ! Bulk CH4 gas conductivity coefficient at soil/snow/atmosphere interface (m/s)
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZRO2_OXIC      ! O2 consumed during oxic decomposition (gO2/m2/s)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZRCO2_OXIC     ! CO2 produced during oxic decomposition (gCO2/m2/s)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZRCH4_MG       ! CH4 produced during methanogenesis (gCH4/m2/s)
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZRO2_MT        ! O2 consumed during methanotrophy (gO2/m2/s)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZRCO2_MT       ! CO2 produced during methanotrophy (gCO2/m2/s)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZRCH4_MT       ! CH4 consumed during methanotrophy (gCH4/m2/s)
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZPO2           ! O2 total soil porosity (m3 Air /m3 Soil)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZPCO2          ! CO2 total soil porosity (m3 Air /m3 Soil)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZPCH4          ! CH4 total soil porosity (m3 Air /m3 Soil)
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZHCC_O2        ! Henry's constant for O2 (-)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZHCC_CO2       ! Henry's constant for CO2(-)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZHCC_CH4       ! Henry's constant for CH4(-)
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZKO2_AROOT     ! O2 gas conductivity coefficient in the air into root media (m2/s)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZKCO2_AROOT    ! CO2 gas conductivity coefficient in the air into root media (m2/s)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZKCH4_AROOT    ! CH4 gas conductivity coefficient in the air into root media (m2/s)
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZDIFFO2        ! Bulk O2 gas diffusion coefficient in the soil (m2/s)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZDIFFCO2       ! Bulk CO2 gas diffusion coefficient in the soil (m2/s)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZDIFFCH4       ! Bulk CH4 gas diffusion coefficient in the soil (m2/s)
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZXEBU          ! CH4 ebullition concentration threshold (g/m3 Air)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZVBUBBLE       ! Ebulition bubble veliocity (m/s)
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZCOEF_EVAP_O2  ! flux coefficient for O2 transported by evapotranspiration (s-1)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZCOEF_EVAP_CO2 ! flux coefficient for CO2 transported by evapotranspiration (s-1)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZCOEF_EVAP_CH4 ! flux coefficient for CH4 transported by evapotranspiration (s-1)
!
REAL, DIMENSION(SIZE(PEK%XTG,1))                 :: ZCOEF_SURF_O2  ! soil/snow/atmosphere interface flux coefficient for O2 (s-1)
REAL, DIMENSION(SIZE(PEK%XTG,1))                 :: ZCOEF_SURF_CO2 ! soil/snow/atmosphere interface flux coefficient for CO2 (s-1)
REAL, DIMENSION(SIZE(PEK%XTG,1))                 :: ZCOEF_SURF_CH4 ! soil/snow/atmosphere interface flux coefficient for CH4 (s-1)
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZCOEF_PMT_O2   ! PMT coefficient for O2 (s-1)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZCOEF_PMT_CO2  ! PMT coefficient for CO2 (s-1)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZCOEF_PMT_CH4  ! PMT coefficient for CH4 (s-1)
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZBUBBLE_OUT_CH4! gCH4/m2/s soil
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZBUBBLE_IN_CH4 ! gCH4/m2/s soil
!
! local variables for Time splitting gas diffusion scheme
!
REAL, DIMENSION(SIZE(PEK%XTG,1)) :: ZRESP_HETERO_SOIL_SPLIT,ZFLUX_CH4_SOIL_SPLIT,   &
                                    ZFLUX_O2_SOIL_SPLIT,                            &
                                    ZSURF_O2_SPLIT,ZSURF_CO2_SPLIT,ZSURF_CH4_SPLIT, &
                                    ZEVAP_O2_SPLIT,ZEVAP_CO2_SPLIT,ZEVAP_CH4_SPLIT, &
                                    ZPMT_O2_SPLIT,ZPMT_CO2_SPLIT,ZPMT_CH4_SPLIT,    &
                                    ZEBU_CH4_SPLIT,                                 &
                                    ZFCONS_O2_SPLIT,ZFPROD_CO2_SPLIT,               &
                                    ZFMT_CH4_SPLIT,ZFMG_CH4_SPLIT
!
!-------------------------------------------------------------------------------
!
!*      0.6    declarations of local dimenssion
!
INTEGER  :: INDT, JDT   ! Time splitting indicies
!
INTEGER  :: INI, INL, JI, JL, JJ, IDEPTH, INB, INC
!
INTEGER :: ILUOUT
!
REAL    :: ZTSTEP, ZNDT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! correspondence between array indices and biomass compartments
! LEAF = 1
! STRUCT_ACT = 2
! STRUCT_PAS = 3
! STRUCT_BELOW = 4
! WOOD_ABOVE = 5
! WOOD_BELOW = 6
!
! correspondence between array indices and litter type 
! LT_METABOLIC = 1
! LT_STRUCTURAL = 2
!
! correspondence between array indices and soil carbon pools
! SL_ACTIVE = 1
! SL_SLOW = 2
! SL_PASSIVE = 3
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_EVOL_DIF',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*      1.     Preliminaries
!              -------------
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!Dimenssions
!
INI=SIZE(PEK%XSOILDIF_CARB,1)
INL=SIZE(PEK%XSOILDIF_CARB,2)
INC=SIZE(PEK%XSOILDIF_CARB,3)
INB=SIZE(PRESP_BIOMASS_INST,2)
!
!Initialise local and output variables
!
CALL INIT_VAR
!
!Atmospheric concentrations for boundary conditions (gX/m3 air)
!
ZO2_ATM (:) = PRHOA(:) * (XO2_ATMO*XMO2*XKGTOG/(XMD*1.E2))
ZCO2_ATM(:) = PRHOA(:) * PCO2(:)*XKGTOG
ZCH4_ATM(:) = PRHOA(:) * (ZCH4_ATMO*XMCH4*XKGTOG/(XMD*1.E6))
!
!-------------------------------------------------------------------------------
!
!*      2.     Autotrophic respiration
!              -----------------------
!
DO JJ=1,INB
   DEK%XRESP_AUTO(:) = DEK%XRESP_AUTO (:) + PRESP_BIOMASS_INST(:,JJ)
ENDDO
!
!-------------------------------------------------------------------------------
!
!*      3.     Surface litter respiration
!              --------------------------
!
! Surface litter physical properties
!
ZTG_LIT   (:) = 0.0
ZMOIST_LIT(:) = 0.0
ZSAT_LIT  (:) = 0.0
ZMOIST_WRK(:) = 0.0
ZSAT_WRK  (:) = 0.0
!
DO JL=1,INL
   DO JI=1,INI     
!   
      IF(JL<=PK%NWG_LAYER(JI))THEN
!              
        ZWGHT_SURF(JI,JL)=MIN(PK%XDZG(JI,JL),MAX(0.0,ZDTOP-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))/ZDTOP
!      
        ZTG_LIT(JI)=ZTG_LIT(JI)+PEK%XTG(JI,JL)*ZWGHT_SURF(JI,JL)
!      
        ZMOIST_LIT(JI)=ZMOIST_LIT(JI)+(PEK%XWG (JI,JL)-KK%XWWILT(JI,JL))*ZWGHT_SURF(JI,JL)
        ZMOIST_WRK(JI)=ZMOIST_WRK(JI)+(KK%XWFC(JI,JL)-KK%XWWILT(JI,JL))*ZWGHT_SURF(JI,JL)
!       
        ZSAT_LIT(JI)=ZSAT_LIT(JI)+(PEK%XWG  (JI,JL)-KK%XWFC(JI,JL))*ZWGHT_SURF(JI,JL)
        ZSAT_WRK(JI)=ZSAT_WRK(JI)+(KK%XWSAT(JI,JL)-KK%XWFC(JI,JL))*ZWGHT_SURF(JI,JL)
!      
      ENDIF
!      
   ENDDO
ENDDO 
!
WHERE(ZMOIST_WRK(:) > 0.0) ZMOIST_LIT(:) = MIN(1.0,MAX(0.0,ZMOIST_LIT(:)/ZMOIST_WRK(:)))
WHERE(ZSAT_WRK  (:) > 0.0) ZSAT_LIT  (:) = MIN(1.0,MAX(0.0,ZSAT_LIT  (:)/ZSAT_WRK  (:)))
!
ZCONTROL_TEMP_SURF (:) = CONTROL_TEMP_FUNC(ZTG_LIT(:)-XTT)
ZCONTROL_MOIST_SURF(:) = CONTROL_MOIST_FUNC(ZMOIST_LIT(:),ZSAT_LIT(:))
!
IF(IO%LCLEACH)THEN
  ZCONTROL_LEACH_SURF(:) = XALPHA_DOC * MAX(KK%XFSAT(:),KK%XFFLOOD(:))
ENDIF
!
! Surface respiration
!
CALL CARBON_LITTER_SURFACE(PTSTEP, IO%LCLEACH,                                          &
                           PK%XTURNOVER, PEK%XSURFACE_LITTER, PEK%XSURFACE_LIGNIN_STRUC,&
                           ZCONTROL_TEMP_SURF, ZCONTROL_MOIST_SURF, ZCONTROL_LEACH_SURF,&
                           ZRESP_HETERO_LITTER, ZSOILCARBON_INPUT(:,1,:),               &
                           ZFDOC_LITTER_SURF                                            )
!
! Surface litter heterotrophic respiration (gC/m2/s -> gCO2/m2/s)
!
ZRESP_HETERO_LITTER(:) = ZRESP_HETERO_LITTER(:) * (XMCO2/XMC)
!
!-------------------------------------------------------------------------------
!
!*      3.     Soil physical and chemical properties
!              -------------------------------------
!
! Soil gas properties for soil gas scheme
!
IF(IO%LSOILGAS)THEN
!
  CALL GAS_SOILDIF_PROPERTIES(KK, PK, PEK, DMK, CEBUL,           &
                              PTA, PRHOA, PPS, ZO2_ATM,          &
                              ZPI_AERENCHYMA, ZPO2, ZPCO2, ZPCH4,&
                              ZHCC_O2,ZHCC_CO2,ZHCC_CH4,         &
                              ZKO2_AVEG,ZKCO2_AVEG,ZKCH4_AVEG,   &
                              ZKO2_AROOT,ZKCO2_AROOT,ZKCH4_AROOT,&
                              ZKO2_SURF,ZKCO2_SURF,ZKCH4_SURF,   &
                              ZDIFFO2,ZDIFFCO2,ZDIFFCH4,         &
                              ZCONTROL_TEMP_MG,ZCONTROL_MG,      &
                              ZXEBU,ZVBUBBLE                     )
!
ENDIF
!
! Soil physical properties for soil carbon scheme
!
CALL CARBON_SOILDIF_PROPERTIES(IO, KK, PK, PEK, ZCONTROL_TEMP,    &
                               ZCONTROL_MOIST,ZCONTROL_LEACH_SOIL,&
                               ZDBIO,ZDIFBIO,ZDCRYO,ZDIFCRYO      )
!
!-------------------------------------------------------------------------------
!
!*      4.     Soil Organic Carbon content evolution
!              -------------------------------------
!
!Calculates carbon oxic, anoxic and leaching fluxes out of soil litter and carbon pools
!
CALL CARBON_SOILDIF_FLUXES (IO, KK, PK, PEK, DMK, PTSTEP,                    &
                            ZCONTROL_TEMP,ZCONTROL_MOIST,ZCONTROL_LEACH_SOIL,&
                            ZCONTROL_TEMP_MG,ZCONTROL_MG,ZPO2,ZPCO2,         &
                            ZFOXIC_LITTER,ZFMG_LITTER,ZFLEACH_LITTER,        &
                            ZFOXIC_SOC,ZFMG_SOC,ZFLEACH_SOC                  )

!
!Calculates litter evolution in the soil
!
CALL CARBON_LITTER_SOILDIF(IO, PK, PEK, PTSTEP,                      &
                           ZFOXIC_LITTER,ZFMG_LITTER,ZFLEACH_LITTER, &
                           ZRESP_HETERO_SOIL,ZSOILCARBON_INPUT,      &
                           ZRO2_OXIC,ZRCO2_OXIC,ZRCH4_MG,            &
                           ZFDOC_LITTER_SOIL                         )
!
!Calculates soil carbon pools evolution
!
CALL CARBON_SOILDIF(IO, KK, PK, PEK, PTSTEP, ZSOILCARBON_INPUT,&
                    ZFOXIC_SOC, ZFMG_SOC, ZFLEACH_SOC,         &
                    ZRO2_OXIC, ZRCO2_OXIC, ZRCH4_MG,           &
                    ZRESP_HETERO_SOIL, ZFDOC_SOILCARBON        )
!
!
!Soil Carbon vertical dynamic following Morel et al. (2019)
!
IF(IO%LADVECT_SOC.OR.IO%LCRYOTURB.OR.IO%LBIOTURB)THEN       
  CALL CARBON_DYNAMIC(IO, KK, PK, PEK, ILUOUT, PTSTEP, &
                      ZDBIO, ZDIFBIO, ZDCRYO, ZDIFCRYO )
ENDIF
!
!-------------------------------------------------------------------------------
!
!*      5.     Soil Gas content evolution following Morel et al. (2019)
!              --------------------------------------------------------
!
IF(IO%LSOILGAS)THEN
!
! Calculates coefficient of fluxes out of gas reservoirs and methanotrophy
!
  CALL GAS_SOILDIF_FLUXES(IO, KK, PK, PEK, DK, DEK, DMK, PTSTEP, CEBUL,    &
                          ZPO2, ZPCO2, ZPCH4, ZHCC_O2, ZHCC_CO2, ZHCC_CH4, &
                          ZPI_AERENCHYMA, ZO2_ATM, ZXEBU,                  &
                          ZKO2_AVEG, ZKCO2_AVEG, ZKCH4_AVEG,               &
                          ZKO2_AROOT,ZKCO2_AROOT,ZKCH4_AROOT,              &
                          ZKO2_SURF, ZKCO2_SURF, ZKCH4_SURF,               &
                          ZCOEF_EVAP_O2, ZCOEF_EVAP_CO2, ZCOEF_EVAP_CH4,   &
                          ZCOEF_SURF_O2, ZCOEF_SURF_CO2, ZCOEF_SURF_CH4,   &
                          ZCOEF_PMT_O2, ZCOEF_PMT_CO2, ZCOEF_PMT_CH4,      &
                          ZRO2_MT, ZRCO2_MT, ZRCH4_MT,                     &
                          ZBUBBLE_OUT_CH4, ZBUBBLE_IN_CH4                  )
!
! Soil gas diffusions
!
! -----------------------------------------------------------------
! Time splitting for *large time steps* since surface gas diffusion
! could be problematic for surface sine layer for very dry soil 
! ------------------------------------------------------------------
!
  INDT = 1
  IF(PTSTEP>ZTIMEMAX)THEN
    INDT = MAX(1,NINT(PTSTEP/ZTIMEMAX))
  ENDIF
!
  ZNDT    = REAL(INDT)
  ZTSTEP  = PTSTEP/ZNDT
!

  DO JDT = 1,INDT
!
     CALL GAS_SOILDIF(IO, PK, PEK, ILUOUT, ZTSTEP,                    &
                      ZO2_ATM,ZCO2_ATM,ZCH4_ATM,ZPO2,ZPCO2,ZPCH4,     &
                      ZDIFFO2,ZDIFFCO2,ZDIFFCH4,ZRO2_OXIC,ZRCO2_OXIC, &
                      ZRCH4_MG,ZRO2_MT,ZRCO2_MT,ZRCH4_MT,             &
                      ZCOEF_EVAP_O2,ZCOEF_EVAP_CO2,ZCOEF_EVAP_CH4,    &
                      ZCOEF_SURF_O2,ZCOEF_SURF_CO2,ZCOEF_SURF_CH4,    &
                      ZCOEF_PMT_O2,ZCOEF_PMT_CO2,ZCOEF_PMT_CH4,ZXEBU, &
                      ZVBUBBLE,ZBUBBLE_OUT_CH4,ZBUBBLE_IN_CH4,        &
                      ZRESP_HETERO_SOIL_SPLIT,                        &
                      ZFLUX_O2_SOIL_SPLIT,ZFLUX_CH4_SOIL_SPLIT,       &
                      ZSURF_O2_SPLIT,ZSURF_CO2_SPLIT,ZSURF_CH4_SPLIT, &
                      ZEVAP_O2_SPLIT,ZEVAP_CO2_SPLIT,ZEVAP_CH4_SPLIT, &
                      ZPMT_O2_SPLIT,ZPMT_CO2_SPLIT,ZPMT_CH4_SPLIT,    &
                      ZEBU_CH4_SPLIT,                                 &
                      ZFCONS_O2_SPLIT,ZFPROD_CO2_SPLIT,               &
                      ZFMT_CH4_SPLIT,ZFMG_CH4_SPLIT                   )
!
     ZRESP_HETERO_SOIL(:) = ZRESP_HETERO_SOIL(:) + ZRESP_HETERO_SOIL_SPLIT(:)/ZNDT
     ZFLUX_O2_SOIL    (:) = ZFLUX_O2_SOIL    (:) + ZFLUX_O2_SOIL_SPLIT    (:)/ZNDT
     ZFLUX_CH4_SOIL   (:) = ZFLUX_CH4_SOIL   (:) + ZFLUX_CH4_SOIL_SPLIT   (:)/ZNDT
!
     DEK%XSURF_O2 (:) = DEK%XSURF_O2 (:) + ZSURF_O2_SPLIT (:)/ZNDT
     DEK%XSURF_CO2(:) = DEK%XSURF_CO2(:) + ZSURF_CO2_SPLIT(:)/ZNDT
     DEK%XSURF_CH4(:) = DEK%XSURF_CH4(:) + ZSURF_CH4_SPLIT(:)/ZNDT
!
     DEK%XEVAP_O2 (:) = DEK%XEVAP_O2 (:) + ZEVAP_O2_SPLIT (:)/ZNDT
     DEK%XEVAP_CO2(:) = DEK%XEVAP_CO2(:) + ZEVAP_CO2_SPLIT(:)/ZNDT
     DEK%XEVAP_CH4(:) = DEK%XEVAP_CH4(:) + ZEVAP_CH4_SPLIT(:)/ZNDT
!
     DEK%XPMT_O2 (:) = DEK%XPMT_O2 (:) + ZPMT_O2_SPLIT (:)/ZNDT
     DEK%XPMT_CO2(:) = DEK%XPMT_CO2(:) + ZPMT_CO2_SPLIT(:)/ZNDT
     DEK%XPMT_CH4(:) = DEK%XPMT_CH4(:) + ZPMT_CH4_SPLIT(:)/ZNDT
!
     DEK%XEBU_CH4(:) = DEK%XEBU_CH4(:) + ZEBU_CH4_SPLIT(:)/ZNDT
!
     DEK%XFCONS_O2 (:) = DEK%XFCONS_O2 (:) + ZFCONS_O2_SPLIT (:)/ZNDT
     DEK%XFPROD_CO2(:) = DEK%XFPROD_CO2(:) + ZFPROD_CO2_SPLIT(:)/ZNDT
     DEK%XFMT_CH4  (:) = DEK%XFMT_CH4  (:) + ZFMT_CH4_SPLIT  (:)/ZNDT
     DEK%XFMG_CH4  (:) = DEK%XFMG_CH4  (:) + ZFMG_CH4_SPLIT  (:)/ZNDT
!
  ENDDO
!
! -----------------------------------------------------------------
! End Time splitting
! -----------------------------------------------------------------
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*      7.     Final diagnostics
!              -----------------
!
! Total vegetation turnover (kgC/m2/s)
!
DO JL=1,INB
   DO JI=1,INI
      DEK%XTURNVTOT(JI) = DEK%XTURNVTOT(JI) + PK%XTURNOVER(JI,JL) * XGTOKG
   ENDDO
ENDDO
!
! Total soil carbon input from litter (kgC/m2/s)
!
DO JJ=1,INC
   DO JL=1,INL
       DO JI=1,INI
          DEK%XFLTOSCARB(JI) = DEK%XFLTOSCARB(JI) + ZSOILCARBON_INPUT(JI,JL,JJ) * XGTOKG
       ENDDO
   ENDDO
ENDDO
!
! Surface litter heterotrophic respiration (gCO2/m2/s -> kgCO2/m2/s)
!  
DEK%XRESPLIT  (:)= ZRESP_HETERO_LITTER(:) * XGTOKG
!
! Soil heterotrophic respiration (gCO2/m2/s -> kgCO2/m2/s)
!
DEK%XRESPSCARB(:)= ZRESP_HETERO_SOIL(:) * XGTOKG
!
! Total ecosystem respiration
!
DEK%XRESP_ECO (:)= DEK%XRESPLIT(:) + DEK%XRESPSCARB(:) + DEK%XRESP_AUTO(:)
!
! Dissolved organic carbon (gC/m2/s -> kgC/m2/s)
!
IF(IO%LCLEACH)THEN
  DEK%XFDOCLIT(:)= ZFDOC_LITTER_SURF(:) * XGTOKG
  DEK%XFDOC   (:)=(ZFDOC_LITTER_SURF(:)+ZFDOC_LITTER_SOIL(:)+ZFDOC_SOILCARBON(:)) * XGTOKG
ENDIF
!
!-------------------------------------------------------------------------------
!
!*      8.     Final diagnostics for soil gas scheme
!              -----------------
!
IF(IO%LSOILGAS)THEN
!
! gX/m3 air -> gX/m3 soil (X = O2 ; CO2 ; CH4)
!
  DMK%XSOILO2 (:,:) = PEK%XSGASO2 (:,:) * ZPO2 (:,:) 
  DMK%XSOILCO2(:,:) = PEK%XSGASCO2(:,:) * ZPCO2(:,:) 
  DMK%XSOILCH4(:,:) = PEK%XSGASCH4(:,:) * ZPCH4(:,:) 
!
! flux (kgX/m2/s)
!
  DEK%XO2FLUX (:) = ZFLUX_O2_SOIL (:) * XGTOKG
  DEK%XCH4FLUX(:) = ZFLUX_CH4_SOIL(:) * XGTOKG
!
  DEK%XSURF_O2 (:) = DEK%XSURF_O2 (:) * XGTOKG
  DEK%XSURF_CO2(:) = DEK%XSURF_CO2(:) * XGTOKG
  DEK%XSURF_CH4(:) = DEK%XSURF_CH4(:) * XGTOKG
!
  DEK%XEVAP_O2 (:) = DEK%XEVAP_O2 (:) * XGTOKG
  DEK%XEVAP_CO2(:) = DEK%XEVAP_CO2(:) * XGTOKG
  DEK%XEVAP_CH4(:) = DEK%XEVAP_CH4(:) * XGTOKG
!
  DEK%XPMT_O2 (:) = DEK%XPMT_O2 (:) * XGTOKG
  DEK%XPMT_CO2(:) = DEK%XPMT_CO2(:) * XGTOKG
  DEK%XPMT_CH4(:) = DEK%XPMT_CH4(:) * XGTOKG
!
  DEK%XEBU_CH4(:) = DEK%XEBU_CH4(:) * XGTOKG
!
  DEK%XFCONS_O2 (:) = DEK%XFCONS_O2 (:) * XGTOKG
  DEK%XFPROD_CO2(:) = DEK%XFPROD_CO2(:) * XGTOKG
  DEK%XFMT_CH4  (:) = DEK%XFMT_CH4  (:) * XGTOKG
  DEK%XFMG_CH4  (:) = DEK%XFMG_CH4  (:) * XGTOKG
!
! Oxic decomposition by layer (kgX/m2/s)
!
  DMK%XOXIC_O2 (:,:) = ZRO2_OXIC (:,:) * XGTOKG
  DMK%XOXIC_CO2(:,:) = ZRCO2_OXIC(:,:) * XGTOKG
!
! Methanogenesis and methanotrophy by layer (kgCH4/m2/s)
!
  DMK%XMG_CH4(:,:) = ZRCH4_MG(:,:) * XGTOKG
  DMK%XMT_CH4(:,:) = ZRCH4_MT(:,:) * XGTOKG
!
ENDIF
!
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CARBON_EVOL_DIF',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
CONTAINS
!
!-----------------------------------------------------------------
!
SUBROUTINE INIT_VAR
!
IMPLICIT NONE
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CARBON_EVOL_DIF:INIT_VAR',0,ZHOOK_HANDLE)
!
!Output variables
!
DEK%XRESP_AUTO(:) = 0.0
DEK%XRESP_ECO (:) = 0.0
DEK%XTURNVTOT (:) = 0.0
DEK%XFLTOSCARB(:) = 0.0
DEK%XRESPSCARB(:) = 0.0
DEK%XRESPLIT  (:) = 0.0
DEK%XFDOCLIT  (:) = 0.0
DEK%XFDOC     (:) = 0.0
!
DEK%XSURF_O2 (:) = 0.0
DEK%XSURF_CO2(:) = 0.0
DEK%XSURF_CH4(:) = 0.0
!
DEK%XEVAP_O2 (:) = 0.0
DEK%XEVAP_CO2(:) = 0.0
DEK%XEVAP_CH4(:) = 0.0
!
DEK%XPMT_O2 (:) = 0.0
DEK%XPMT_CO2(:) = 0.0
DEK%XPMT_CH4(:) = 0.0
!
DEK%XEBU_CH4(:) = 0.0
!
DEK%XFCONS_O2 (:) = 0.0
DEK%XFPROD_CO2(:) = 0.0
DEK%XFMT_CH4  (:) = 0.0
DEK%XFMG_CH4  (:) = 0.0
!
DEK%XCH4FLUX (:) = 0.0
DEK%XO2FLUX  (:) = 0.0
!
DMK%XTSOILPOOL(:,:) = 0.0
DMK%XSOILO2   (:,:) = 0.0
DMK%XSOILCO2  (:,:) = 0.0
DMK%XSOILCH4  (:,:) = 0.0
DMK%XOXIC_O2  (:,:) = 0.0
DMK%XOXIC_CO2 (:,:) = 0.0
DMK%XMG_CH4   (:,:) = 0.0
DMK%XMT_CH4   (:,:) = 0.0
!
!Local soil carbon an litter variables
!
ZWGHT_SURF(:,:) = XUNDEF
!
ZSOILCARBON_INPUT(:,:,:) = 0.0
!
ZCONTROL_TEMP   (:,:) = 0.0
ZCONTROL_MOIST  (:,:) = 0.0
ZCONTROL_TEMP_MG(:,:) = 0.0
ZCONTROL_MG     (:,:) = 0.0
!
ZCONTROL_LEACH_SURF(:  ) = 0.0
ZCONTROL_LEACH_SOIL(:,:) = 0.0
!
ZRESP_HETERO_SOIL   (:) = 0.0
ZRESP_HETERO_LITTER (:) = 0.0
ZFLUX_O2_SOIL       (:) = 0.0
ZFLUX_CH4_SOIL      (:) = 0.0
!
ZFDOC_LITTER_SURF(:) = 0.0
ZFDOC_LITTER_SOIL(:) = 0.0
ZFDOC_SOILCARBON (:) = 0.0
!
ZFOXIC_LITTER (:,:,:) = 0.0
ZFMG_LITTER   (:,:,:) = 0.0
ZFLEACH_LITTER(:,:,:) = 0.0
!
ZFOXIC_SOC (:,:,:) = 0.0
ZFLEACH_SOC(:,:,:) = 0.0
!
ZFMG_SOC(:,:) = 0.0
!
ZDBIO (:) = 0.0
ZDCRYO(:) = 0.0
!
ZDIFBIO (:,:) = 0.0
ZDIFCRYO(:,:) = 0.0
!
!Local gas variables
!
ZPI_AERENCHYMA(:) = 0.0
!
ZKO2_AVEG (:) = 0.0
ZKCO2_AVEG(:) = 0.0
ZKCH4_AVEG(:) = 0.0
!
ZKO2_SURF (:) = 0.0
ZKCO2_SURF(:) = 0.0
ZKCH4_SURF(:) = 0.0
!
ZRO2_OXIC (:,:) = 0.0
ZRCO2_OXIC(:,:) = 0.0
!
ZRCH4_MG(:,:) = 0.0
!
ZRO2_MT (:,:) = 0.0
ZRCO2_MT(:,:) = 0.0
ZRCH4_MT(:,:) = 0.0
!
ZPO2 (:,:) = 0.0
ZPCO2(:,:) = 0.0
ZPCH4(:,:) = 0.0
!
ZHCC_O2 (:,:) = 0.0
ZHCC_CO2(:,:) = 0.0
ZHCC_CH4(:,:) = 0.0
!
ZKO2_AROOT (:,:) = 0.0
ZKCO2_AROOT(:,:) = 0.0
ZKCH4_AROOT(:,:) = 0.0
!
ZDIFFO2 (:,:) = 0.0
ZDIFFCO2(:,:) = 0.0
ZDIFFCH4(:,:) = 0.0
!
ZXEBU   (:,:) = 0.0
ZVBUBBLE(:,:) = 0.0
!
ZCOEF_EVAP_O2 (:,:) = 0.0
ZCOEF_EVAP_CO2(:,:) = 0.0
ZCOEF_EVAP_CH4(:,:) = 0.0
!
ZCOEF_SURF_O2 (:) = 0.0
ZCOEF_SURF_CO2(:) = 0.0
ZCOEF_SURF_CH4(:) = 0.0
!
ZCOEF_PMT_O2 (:,:) = 0.0
ZCOEF_PMT_CO2(:,:) = 0.0
ZCOEF_PMT_CH4(:,:) = 0.0
!
ZRESP_HETERO_SOIL_SPLIT(:) = 0.0
ZFLUX_O2_SOIL_SPLIT    (:) = 0.0
ZFLUX_CH4_SOIL_SPLIT   (:) = 0.0
!
ZSURF_O2_SPLIT (:) = 0.0
ZSURF_CO2_SPLIT(:) = 0.0
ZSURF_CH4_SPLIT(:) = 0.0
!
ZEVAP_O2_SPLIT (:) = 0.0
ZEVAP_CO2_SPLIT(:) = 0.0
ZEVAP_CH4_SPLIT(:) = 0.0
!
ZPMT_O2_SPLIT (:) = 0.0
ZPMT_CO2_SPLIT(:) = 0.0
ZPMT_CH4_SPLIT(:) = 0.0
!
ZEBU_CH4_SPLIT(:) = 0.0
!
ZFCONS_O2_SPLIT (:) = 0.0
ZFPROD_CO2_SPLIT(:) = 0.0
ZFMT_CH4_SPLIT  (:) = 0.0
ZFMG_CH4_SPLIT  (:) = 0.0
!
ZBUBBLE_OUT_CH4(:,:) = 0.0
ZBUBBLE_IN_CH4 (:,:) = 0.0
!
IF (LHOOK) CALL DR_HOOK('CARBON_EVOL_DIF:INIT_VAR',1,ZHOOK_HANDLE)

END SUBROUTINE INIT_VAR
!
!-----------------------------------------------------------------
END SUBROUTINE CARBON_EVOL_DIF
