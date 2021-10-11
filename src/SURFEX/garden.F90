!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE GARDEN (DTCO, G, T, TOP, TIR, AT, DMT, DTV, GB, DK, DEK, DMK, GDO, S, K, P, PEK, &
                       HIMPLICIT_WIND, TPTIME, PTSUN, PPEW_A_COEF, PPEW_B_COEF, &
                       PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,      &
                       PTSTEP, PZREF, PTA, PQA, PEXNS, PRHOA, PCO2, PPS, PRR,   &
                       PSR, PZENITH, PSW, PLW, PLE_HVEG, PVMOD, PALBNIR_TVEG, &
                       PALBVIS_TVEG, PALBNIR_TSOIL, PALBVIS_TSOIL,              &
                       PSFCO2, PUW,                                             &
                       PAC, PQSAT, PTSRAD, PAC_AGG, PHU_AGG, PIRRIG,            &
                       PNOC_RF_RD, PDEEP_FLUX                                   )
!   ##########################################################################
!
!!****  *GARDEN*  
!!
!!    PURPOSE
!!    -------
!
!!call the vegetation scheme (ISBA) inside TEB
!     
!!**  METHOD
!     ------
!
!
!!    EXTERNAL
!!    --------
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
!!    AUTHOR
!!    ------
!!
!!      A. Lemonsu          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!    Original    05/2009
!     B. decharme 04/2013 : variables for surf/atm coupling
!                           dummy for water table / surface coupling
!!    P. Samuelsson  10/2014  Introduced dummy variables in call to ISBA for MEB
!!    E. Redon       06/2017  Add net IR rad received by urban trees
!!    M. Goret       07/2017  Replace the if statement on GDM%TVG%CPHOTO by select case
!!    M. Goret       08/2017  Add garden diagnostics fill in
!!    P. Tulet       06/2016 add RN leaves to call ISBA (MEGAN coupling)
!!    A. Druel       02/2019 - transmit NPAR_VEG_IRR_USE for irrigation
!!    C. de Munck et E. Bernard 10/2019  Added runoff of roads and roofs not connected to sewer 
!!                                       to garden irrigation (hydro) 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_n,      ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n,       ONLY : DATA_ISBA_t
USE MODD_SFX_GRID_n,        ONLY : GRID_t
USE MODD_SSO_n,             ONLY : SSO_t, SSO_INIT
USE MODD_TEB_n,             ONLY : TEB_t
USE MODD_TEB_OPTION_n,      ONLY : TEB_OPTIONS_t
USE MODD_DIAG_MISC_TEB_n,   ONLY : DIAG_MISC_TEB_t
USE MODD_SURF_ATM_n,        ONLY : SURF_ATM_t
!
USE MODD_DATA_ISBA_n,       ONLY : DATA_ISBA_t
USE MODD_GR_BIOG_n,         ONLY : GR_BIOG_t
!
USE MODD_DIAG_n,            ONLY : DIAG_t
USE MODD_DIAG_EVAP_ISBA_n,  ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n,  ONLY : DIAG_MISC_ISBA_t
!
USE MODD_TEB_IRRIG_n,       ONLY : TEB_IRRIG_t
!
USE MODD_ISBA_OPTIONS_n,    ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,            ONLY : ISBA_S_t, ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_AGRI_n,            ONLY : AGRI_t, AGRI_INIT
USE MODD_AGRI,              ONLY : LAGRIP, LIRRIGMODE
!
USE MODD_TYPE_DATE_SURF,    ONLY: DATE_TIME
USE MODD_SURF_PAR,          ONLY: XUNDEF
USE MODD_CSTS,              ONLY: XCPD
USE MODD_ISBA_PAR,          ONLY: XCVHEATF
!
USE MODD_SURF_ATM_TURB_n,   ONLY : SURF_ATM_TURB_t
!
USE MODI_ISBA
USE MODI_VEGETATION_UPDATE
USE MODE_THERMOS
!
USE MODI_FLAG_TEB_VEG_n
USE MODI_CARBON_EVOL
USE MODI_VEGETATION_EVOL
USE MODI_TEB_IRRIG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    Declarations of arguments
!
!
!
TYPE(DATA_COVER_t),     INTENT(INOUT) :: DTCO
TYPE(GRID_t),           INTENT(INOUT) :: G
TYPE(TEB_t),            INTENT(INOUT) :: T
TYPE(TEB_OPTIONS_t),    INTENT(INOUT) :: TOP
!
TYPE(DATA_ISBA_t),      INTENT(INOUT) :: DTV
TYPE(GR_BIOG_t),        INTENT(INOUT) :: GB
!
TYPE(DIAG_t),           INTENT(INOUT) :: DK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
TYPE(DIAG_MISC_TEB_t),  INTENT(INOUT) :: DMT
!
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: GDO
TYPE(ISBA_S_t),         INTENT(INOUT) :: S
TYPE(ISBA_K_t),         INTENT(INOUT) :: K
TYPE(ISBA_P_t),         INTENT(INOUT) :: P
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
!
TYPE(TEB_IRRIG_t),      INTENT(INOUT) :: TIR
!
TYPE(SURF_ATM_TURB_t),  INTENT(IN)    :: AT         ! atmospheric turbulence parameters
!
 CHARACTER(LEN=*),    INTENT(IN)    :: HIMPLICIT_WIND   ! wind implicitation option
!                                                     ! 'OLD' = direct
!                                                     ! 'NEW' = Taylor serie, order 1
TYPE(DATE_TIME)     , INTENT(IN)    :: TPTIME             ! current date and time from teb
REAL, DIMENSION(:)  , INTENT(IN)    :: PTSUN              ! solar time      (s from midnight)
REAL, DIMENSION(:)  , INTENT(IN)    :: PPEW_A_COEF        ! implicit coefficients
REAL, DIMENSION(:)  , INTENT(IN)    :: PPEW_B_COEF        ! for wind coupling
REAL, DIMENSION(:)  , INTENT(IN)    :: PPEQ_A_COEF        ! implicit coefficients
REAL, DIMENSION(:)  , INTENT(IN)    :: PPEQ_B_COEF        ! for humidity
REAL, DIMENSION(:)  , INTENT(IN)    :: PPET_A_COEF        ! implicit coefficients
REAL, DIMENSION(:)  , INTENT(IN)    :: PPET_B_COEF        ! for temperature
REAL                , INTENT(IN)    :: PTSTEP             ! time step
REAL, DIMENSION(:)  , INTENT(IN)    :: PZREF              ! height of atm. var. near the road
REAL, DIMENSION(:)  , INTENT(IN)    :: PTA                ! temp. near the road
REAL, DIMENSION(:)  , INTENT(IN)    :: PQA                ! hum. near the road
REAL, DIMENSION(:)  , INTENT(IN)    :: PPS                ! pressure at the surface
REAL, DIMENSION(:)  , INTENT(IN)    :: PEXNS              ! surface exner function
REAL, DIMENSION(:)  , INTENT(IN)    :: PRHOA              ! air density at the lowest level
REAL, DIMENSION(:)  , INTENT(IN)    :: PCO2               ! CO2 concentration in the air    (kg/m3)
REAL, DIMENSION(:)  , INTENT(IN)    :: PRR                ! rain rate
REAL, DIMENSION(:)  , INTENT(IN)    :: PSR                ! snow rate
REAL, DIMENSION(:)  , INTENT(IN)    :: PZENITH            ! solar zenithal angle
REAL, DIMENSION(:)  , INTENT(IN)    :: PSW                ! incoming total solar rad on an horizontal surface
REAL, DIMENSION(:)  , INTENT(IN)    :: PLW                ! atmospheric infrared radiation
REAL, DIMENSION(:,:), INTENT(IN)    :: PLE_HVEG           ! Latent Heat flux from high veg
REAL, DIMENSION(:)  , INTENT(IN)    :: PVMOD              ! wind near the road
REAL, DIMENSION(:)  , INTENT(IN)    :: PALBNIR_TVEG       ! nearIR  veg tot albedo
REAL, DIMENSION(:)  , INTENT(IN)    :: PALBVIS_TVEG       ! visible veg tot albedo
REAL, DIMENSION(:)  , INTENT(IN)    :: PALBNIR_TSOIL      ! nearIR  soil tot albedo
REAL, DIMENSION(:)  , INTENT(IN)    :: PALBVIS_TSOIL      ! visible soil tot albedo
REAL, DIMENSION(:)  , INTENT(IN)    :: PNOC_RF_RD         ! runoff from roofs and roads not connected to sewer for garden irrigation (kg/m2/s)
!
REAL, DIMENSION(:)  , INTENT(OUT)   :: PSFCO2      ! flux of CO2 positive toward the atmosphere (m/s*kg_CO2/kg_air)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PUW         ! friction flux (m2/s2)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PAC         ! aerodynamical conductance
REAL, DIMENSION(:)  , INTENT(OUT)   :: PQSAT       ! saturation humidity
REAL, DIMENSION(:)  , INTENT(OUT)   :: PTSRAD      ! garden radiative surface temp. (snow free)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PAC_AGG     ! aggreg. aeodynamic resistance for green areas for latent heat flux
REAL, DIMENSION(:)  , INTENT(OUT)   :: PHU_AGG     ! aggreg. relative humidity for green areas for latent heat flux
REAL, DIMENSION(:)  , INTENT(OUT)   :: PIRRIG      ! garden summer irrigation rate
!
REAL, DIMENSION(:),   INTENT(OUT) :: PDEEP_FLUX ! Heat flux at base of the deep soil
!
!*      0.2    Declarations of local variables
!
TYPE(SSO_t)  :: YSS
TYPE(AGRI_t) :: YAG
!
REAL, DIMENSION(SIZE(PPS))    :: ZDIRCOSZW           ! orography slope cosine (=1 in TEB)
REAL, DIMENSION(SIZE(PPS),GDO%NNBIOMASS) :: ZRESP_BIOMASS_INST       ! instantaneous biomass respiration (kgCO2/kgair m/s)
REAL, DIMENSION(SIZE(PPS))    :: ZUSTAR
REAL, DIMENSION(SIZE(PPS))    :: ZIRRIG
REAL, DIMENSION(SIZE(PPS))    :: ZSLOPEDIR           ! slope direction (=-1 in TEB)
REAL, DIMENSION(SIZE(PPS))    :: ZWINDDIR            ! wind direction (=-1 in TEB)
INTEGER, DIMENSION(SIZE(PPS)) :: KTAB_SYT            ! array of index containing
                                                     ! opposite direction for Sytron  (=0 in TEB)
!
REAL, DIMENSION(SIZE(PPS),1) :: ZP_DIR_SW ! spectral direct and diffuse irradiance used in snow cro 
REAL, DIMENSION(SIZE(PPS),1) :: ZP_SCA_SW !
!
!  temperatures
!
REAL, DIMENSION(SIZE(PPS)) :: ZTA ! estimate of air temperature at future time
!                                 ! step as if modified by ISBA flux alone.
!
!  surfaces relative fractions
!  for flood
REAL, DIMENSION(SIZE(PPS)) :: ZEMISF
!
!  variables for deep soil temperature
REAL, DIMENSION(SIZE(PPS)) :: ZTDEEP_A
!
REAL, DIMENSION(SIZE(PPS)) :: ZRNSHADE, ZRNSUNLIT ! RN leaves
!
! Dummy variables for MEB:
REAL, DIMENSION(SIZE(PPS)) :: ZP_MEB_SCA_SW, ZPALPHAN, ZZ0G_WITHOUT_SNOW, &
                              ZZ0_MEBV, ZZ0H_MEBV, ZZ0EFF_MEBV, ZZ0_MEBN, &
                              ZZ0H_MEBN, ZZ0EFF_MEBN
REAL, DIMENSION(SIZE(PPS)) :: ZP_ANGL_NORM   ! angle between the normal to the surface and the sun used in snowcro
INTEGER                    :: ILU
INTEGER :: JL, JI
LOGICAL :: GMASK, GALB, GECOSG
LOGICAL :: GUPDATED
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*      1.     various initialisations
!              -----------------------
!
IF (LHOOK) CALL DR_HOOK('GARDEN',0,ZHOOK_HANDLE)
ILU = SIZE(PPS)
!
ZDIRCOSZW = 1.
ZSLOPEDIR = -1.
ZWINDDIR  = -1.
!
KTAB_SYT=0
!
CALL SSO_INIT(YSS)
!
CALL AGRI_INIT(YAG)
!
ZIRRIG(:) = 0.
!
!-------------------------------------------------------------------------------
!
!*      2.     Treatment of green areas
!              ------------------------
!*      2.1    Automatic irrigation
!              --------------------
!
CALL TEB_IRRIG(TIR%LPAR_GD_IRRIG, PTSTEP, TPTIME%TDATE%MONTH, PTSUN,        &
               TIR%XGD_START_MONTH, TIR%XGD_END_MONTH, TIR%XGD_START_HOUR,  &
               TIR%XGD_END_HOUR, TIR%XGD_24H_IRRIG, PIRRIG           ) 
!
ZIRRIG(:) = PIRRIG(:) + PNOC_RF_RD(:)
! --------------------------------------------------------------------------------------
! Vegetation update (in case of non-interactive vegetation):
! --------------------------------------------------------------------------------------
!
S%TTIME = TPTIME
!
GUPDATED=.FALSE.
GALB = .FALSE.
!
IF (GDO%CPHOTO=='NIT'.OR.GDO%CPHOTO=='NCB') GALB = .TRUE.
!
CALL VEGETATION_UPDATE(DTCO, DTV, G%NDIM, GDO, K, P, PEK, 1, 1, 1,           &
                       PTSTEP, S%TTIME, TOP%XCOVER, TOP%LCOVER, .FALSE., .FALSE., .FALSE.,  &
                       'GRD', GALB, YSS, GUPDATED, OABSENT=(T%XGARDEN==0.), OHG=TOP%LGARDEN,&
                       OHV=TOP%CURBTREE/='NONE'                               )
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!* in urban areas, dynamical roughness length is influenced by local obstacles due to
!  the heterogeneity of the urban fabric (Lemonsu, Redon et al 2021)
!
IF (TOP%CZ0EFF_GD=='LR21') THEN 
  PEK%XZ0(:) = MAX (PEK%XZ0(:), 0.3)
END IF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
DK%XZ0 (:) = PEK%XZ0(:)
DK%XZ0H(:) = PEK%XZ0(:) / P%XZ0_O_Z0H(:)
!
DK%XZ0EFF(:) =  PEK%XZ0(:)
!
!-------------------------------------------------------------------------------
!
!*      2.2    Call ISBA for green areas
!              -------------------------
!
ZP_DIR_SW=XUNDEF
ZP_SCA_SW=XUNDEF
!
ALLOCATE(GB%XIACAN(SIZE(PPS),SIZE(S%XABC)))
!
 CALL ISBA(GDO, K, P, PEK, G, YAG, DK, DEK, DMK, TPTIME, S%XPOI, S%XABC,       &
           GB%XIACAN, .FALSE., PTSTEP, HIMPLICIT_WIND, PZREF, PZREF,           &
           ZDIRCOSZW, XCVHEATF, ZSLOPEDIR, PEK%TSNOW%GRAN2(:,:),               &
           PEK%TSNOW%GRAN2(:,:), PTA, PQA, PEXNS, PRHOA, PPS, PEXNS, PRR, PSR, &
           PZENITH,ZP_ANGL_NORM,ZP_MEB_SCA_SW, PSW, PLW, PLE_HVEG, PVMOD,      &
           ZWINDDIR, PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF, PPEQ_A_COEF,&
           PPET_B_COEF, PPEQ_B_COEF, AT, PALBNIR_TVEG, PALBVIS_TVEG, PALBNIR_TSOIL,&
           PALBVIS_TSOIL, ZPALPHAN, ZZ0G_WITHOUT_SNOW, ZZ0_MEBV, ZZ0H_MEBV,    &
           ZZ0EFF_MEBV, ZZ0_MEBN, ZZ0H_MEBN, ZZ0EFF_MEBN, ZTDEEP_A, PCO2,      &
           K%XFFG(:), K%XFFV(:), ZEMISF, ZUSTAR, PAC_AGG, PHU_AGG,             &
           ZRESP_BIOMASS_INST, PDEEP_FLUX, ZIRRIG,  DTV%NPAR_VEG_IRR_USE,      &
           KTAB_SYT, ZP_DIR_SW, ZP_SCA_SW, ZRNSHADE, ZRNSUNLIT )
!
DEALLOCATE(GB%XIACAN)
!
IF (PEK%TSNOW%SCHEME=='3-L' .OR. PEK%TSNOW%SCHEME=='CRO') PEK%TSNOW%TS(:)= DMK%XSNOWTEMP(:,1)
!
IF (GDO%LTR_ML) THEN
  GMASK = ( TPTIME%TIME - PTSTEP < 0. ) .AND. ( TPTIME%TIME >= 0. )
  IF (GMASK) THEN
    ALLOCATE(DMK%XDFAPARC(ILU),DMK%XDFAPIRC(ILU),DMK%XDLAI_EFFC(ILU))
    DMK%XDFAPARC  (:) = PEK%XFAPARC   (:) / PEK%XMUS (:)
    DMK%XDFAPIRC  (:) = PEK%XFAPIRC   (:) / PEK%XMUS (:)
    DMK%XDLAI_EFFC(:) = PEK%XLAI_EFFC (:) / PEK%XMUS (:)
  ENDIF
ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Vegetation evolution for interactive LAI
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (GDO%CPHOTO=='NIT') THEN
  CALL VEGETATION_EVOL(GDO, DTV, P, PEK, .FALSE., PTSTEP, TPTIME%TDATE%MONTH, TPTIME%TDATE%DAY, & ! OAGRIP = FALSE
                       TPTIME%TIME, G%XLAT, PRHOA, PCO2, YSS, ZRESP_BIOMASS_INST, .FALSE.)        ! LBIOM_REAP  
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Diagnostic of respiration carbon fluxes and soil carbon evolution
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
PSFCO2        (:) = 0.
DEK%XRESP_ECO (:) = 0.
DEK%XRESP_AUTO(:) = 0.
!
IF (GDO%CPHOTO/='NON' .AND. GDO%CRESPSL/='NON' .AND. ANY(PEK%XLAI(:)/=XUNDEF)) THEN
  CALL CARBON_EVOL(GDO, K, P, PEK, DEK, PTSTEP, PRHOA, ZRESP_BIOMASS_INST  )
  ! calculation of vegetation CO2 flux
  PSFCO2(:) = - DEK%XGPP(:) + DEK%XRESP_ECO(:)
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! SWI for garden
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
DO JL=1,SIZE(PEK%XWG,2)
  DO JI=1,SIZE(PEK%XWG,1)
    IF(PEK%XWG (JI,JL)/=XUNDEF)THEN    
      DMT%XSWI (JI,JL) = (PEK%XWG (JI,JL) - K%XWWILT(JI,JL)) / (K%XWFC(JI,JL) - K%XWWILT(JI,JL))
    ENDIF
  ENDDO
ENDDO

!*      4.     Set undefined values for points where there is no garden
!              --------------------------------------------------------
!
! This way, these points are clearly flaged, and one will not try to interpret
! the values for those points
!
 CALL FLAG_TEB_VEG_n(PEK, GDO, T%XGARDEN, 2)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      9.     Fields required for TEB
!              -----------------------
!
WHERE (T%XGARDEN/=0.)
  !
  ! energy balance
  !
  DK%XLE(:) = PEK%XLE(:)
  !
  ! Estimate of green area aerodynamic conductance recomputed from heat flux,
  ! surface (radiative) temp. and forcing air temperature (estimated at future time step)
  ZTA = PPET_B_COEF + PPET_A_COEF * DK%XH
  PAC = 0.
  WHERE (DK%XTSRAD /= ZTA)
    PAC(:)   = MAX(DK%XH(:) / XCPD / PRHOA(:) / (DK%XTSRAD - ZTA) , 0.)
  ENDWHERE
  !
  ! Snow fraction for green areas
  DMT%FSNOW_GD(:) = PEK%XPSNV(:)
  !
  ! Humidity of saturation for green areas
  PQSAT(:) = QSAT(PEK%XTG(:,1),PPS(:))
  !
  !* friction flux
  PUW(:)    = -ZUSTAR(:)**2
  !
ELSEWHERE
  !
  DK%XRN     (:) = XUNDEF
  DK%XH      (:) = XUNDEF
  DK%XLE     (:) = XUNDEF
  DK%XGFLUX  (:) = XUNDEF
  DK%XEVAP   (:) = XUNDEF  
  DEK%XRUNOFF(:) = XUNDEF
  !
  PAC    (:) = XUNDEF
  PQSAT  (:) = XUNDEF
  PUW    (:) = XUNDEF
  !
END WHERE
!
!
PTSRAD(:) = DK%XTSRAD(:)
!
IF (LHOOK) CALL DR_HOOK('GARDEN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE GARDEN
