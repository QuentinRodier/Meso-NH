!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE GREENROOF (DTCO, DTI, IG, I, TG, T, TOP, TVG, DTGD, TIR, GRM,  &
                          HIMPLICIT_WIND, TPTIME, PTSUN, PPEW_A_COEF, PPEW_B_COEF,    &
                PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,                  &
                PTSTEP, PZREF, PUREF,                                                &
                PTA, PQA, PEXNS, PEXNA,PRHOA, PCO2, PPS, PRR, PSR, PZENITH,          &
                PSW,PLW, PVMOD,                                                      &
                PALBNIR_TVEG, PALBVIS_TVEG, PALBNIR_TSOIL, PALBVIS_TSOIL,            &                
                PRN_GREENROOF,PH_GREENROOF,PLE_GREENROOF,PGFLUX_GREENROOF,           &
                PSFCO2,PEVAP_GREENROOF, PUW_GREENROOF,                               &
                PAC_GREENROOF,PQSAT_GREENROOF,PTS_GREENROOF,                         &
                PAC_AGG_GREENROOF, PHU_AGG_GREENROOF,PDEEP_FLUX,                     &
                PRUNOFF_GREENROOF, PDRAIN_GREENROOF, PIRRIG_GREENROOF                )  
!   ##################################################################################
!
!!****  *GREENROOF*  
!!
!!    PURPOSE
!!    -------
!!
!!    call the vegetation scheme (ISBA) inside TEB for greenroofs
!!     
!!**  METHOD
!!     ------
!!    based on subroutine "garden" 
!!
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
!!    Based on subroutine "garden"
!!      
!!    AUTHOR
!!    ------
!!
!!      C. de Munck & A. Lemonsu          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!     Original    09/2011 
!     C. de Munck   02/2013  irrigation (drip irrigation)
!     B. decharme 04/2013 : Variables required in TEB to allow coupling with AROME/ALADIN/ARPEGE
!                           phasing call isba
!                           calculation of vegetation CO2 flux
!                           dummy for water table / surface coupling
!!    P. Samuelsson  10/2014  Introduced dummy variables in call to ISBA for MEB
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_TEB_GRID_n, ONLY : TEB_GRID_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
USE MODD_DATA_TEB_GARDEN_n, ONLY : DATA_TEB_GARDEN_t
USE MODD_TEB_IRRIG_n, ONLY : TEB_IRRIG_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
!
!
USE MODD_SURF_PAR,             ONLY: XUNDEF
USE MODD_TYPE_DATE_SURF,       ONLY: DATE_TIME
USE MODD_CSTS,                 ONLY: XCPD


!
USE MODI_ISBA
USE MODI_VEGETATION_UPDATE_GREENROOF
USE MODI_VEGETATION_EVOL
USE MODI_CARBON_EVOL
USE MODE_THERMOS
USE MODI_ROOF_IMPL_COEF
USE MODI_TEB_IRRIG
!
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(TEB_GRID_t), INTENT(INOUT) :: TG
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
TYPE(DATA_TEB_GARDEN_t), INTENT(INOUT) :: DTGD
TYPE(TEB_IRRIG_t), INTENT(INOUT) :: TIR
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM

!
 CHARACTER(LEN=*),     INTENT(IN)  :: HIMPLICIT_WIND   ! wind implicitation option
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
REAL, DIMENSION(:)  , INTENT(IN)    :: PZREF              ! height of the first atmospheric level
REAL, DIMENSION(:)  , INTENT(IN)    :: PUREF              ! reference height for the wind
REAL, DIMENSION(:)  , INTENT(IN)    :: PTA                ! temperature at first atm. level 
REAL, DIMENSION(:)  , INTENT(IN)    :: PQA                ! specific humidity at first atm. level
REAL, DIMENSION(:)  , INTENT(IN)    :: PPS                ! pressure at the surface
REAL, DIMENSION(:)  , INTENT(IN)    :: PEXNA              ! Exner function at first atm. level
REAL, DIMENSION(:)  , INTENT(IN)    :: PEXNS              ! surface Exner function
REAL, DIMENSION(:)  , INTENT(IN)    :: PRHOA              ! air density at the lowest level
REAL, DIMENSION(:)  , INTENT(IN)    :: PCO2               ! CO2 concentration in the air    (kg/m3)
REAL, DIMENSION(:)  , INTENT(IN)    :: PRR                ! rain rate
REAL, DIMENSION(:)  , INTENT(IN)    :: PSR                ! snow rate
REAL, DIMENSION(:)  , INTENT(IN)    :: PZENITH            ! solar zenithal angle
REAL, DIMENSION(:)  , INTENT(IN)    :: PSW                ! incoming total solar rad on an horizontal surface
REAL, DIMENSION(:)  , INTENT(IN)    :: PLW                ! atmospheric infrared radiation
REAL, DIMENSION(:)  , INTENT(IN)    :: PVMOD              ! module of horizontal wind near first atm. level
REAL, DIMENSION(:)  , INTENT(IN)    :: PALBNIR_TVEG       ! nearIR  veg tot albedo
REAL, DIMENSION(:)  , INTENT(IN)    :: PALBVIS_TVEG       ! visible veg tot albedo
REAL, DIMENSION(:)  , INTENT(IN)    :: PALBNIR_TSOIL      ! nearIR  soil tot albedo
REAL, DIMENSION(:)  , INTENT(IN)    :: PALBVIS_TSOIL      ! visible soil tot albedo
!
REAL, DIMENSION(:)  , INTENT(OUT)   :: PRN_GREENROOF         ! net radiation over greenroofs
REAL, DIMENSION(:)  , INTENT(OUT)   :: PH_GREENROOF          ! sensible heat flux over greenroofs
REAL, DIMENSION(:)  , INTENT(OUT)   :: PLE_GREENROOF         ! latent heat flux over greenroofs
REAL, DIMENSION(:)  , INTENT(OUT)   :: PGFLUX_GREENROOF      ! flux through the greenroofs
REAL, DIMENSION(:)  , INTENT(OUT)   :: PSFCO2                ! flux of greenroof CO2       (m/s*kg_CO2/kg_air)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PEVAP_GREENROOF       ! total evaporation over greenroofs (kg/m2/s)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PUW_GREENROOF         ! friction flux (m2/s2)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PAC_GREENROOF         ! greenroof aerodynamical conductance
REAL, DIMENSION(:)  , INTENT(OUT)   :: PQSAT_GREENROOF       ! saturation humidity
REAL, DIMENSION(:)  , INTENT(INOUT) :: PTS_GREENROOF         ! greenroof radiative surface temp. (snow free)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PAC_AGG_GREENROOF     ! aggreg. aeodynamic resistance for greenroofs for latent heat flux
REAL, DIMENSION(:)  , INTENT(OUT)   :: PHU_AGG_GREENROOF     ! aggreg. relative humidity for greenroofs for latent heat flux
REAL, DIMENSION(:)  , INTENT(OUT)   :: PDEEP_FLUX            ! Heat Flux at the bottom layer of the greenroof
REAL, DIMENSION(:)  , INTENT(OUT)   :: PRUNOFF_GREENROOF     ! greenroof surface runoff
REAL, DIMENSION(:)  , INTENT(OUT)   :: PDRAIN_GREENROOF      ! greenroof total (vertical) drainage
REAL, DIMENSION(:)  , INTENT(OUT)   :: PIRRIG_GREENROOF      ! greenroof summer irrigation rate
!
!
!*      0.2    Declarations of local variables
!
 CHARACTER(LEN=3)                     :: HRAIN               ! Rainfall spatial distribution ('DEF','SGH')
LOGICAL                              :: OFLOOD              ! Activation of the flooding scheme
LOGICAL                              :: OTEMP_ARP           ! True  = time-varying force-restore soil temperature (as in ARPEGE)
                                                            ! False = No time-varying force-restore soil temperature (Default)
LOGICAL                              :: OGLACIER            ! True = Over permanent snow and ice, 
!                                                             initialise WGI=WSAT,
!                                                             Hsnow>=10m and allow 0.8<SNOALB<0.85
                                                            ! False = No specific treatment
REAL, DIMENSION(0)                   :: ZSODELX             ! Pulsation for each layer (Only used if LTEMP_ARP=True)
REAL, DIMENSION(SIZE(PPS))           :: ZMUF                ! fraction of the grid cell reached by the rainfall
REAL, DIMENSION(SIZE(PPS))           :: ZFSAT               ! Topmodel saturated fraction
REAL, DIMENSION(SIZE(PPS),GRM%TGRO%NLAYER_GR) :: ZTOPQS              ! Topmodel (SGH not used in TEB) lateral subsurface flow by layer
REAL, DIMENSION(SIZE(PPS))           :: ZQSB                ! Topmodel (SGH not used in TEB) output lateral subsurface
REAL, DIMENSION(SIZE(PPS))           :: ZFWTD               ! grid-cell fraction of water table to rise
REAL, DIMENSION(SIZE(PPS))           :: ZWTD                ! water table depth from TRIP or MODCOU
!
REAL, DIMENSION(SIZE(PPS))           :: ZDIRCOSZW           ! orography slope cosine (=1 in TEB)
REAL, DIMENSION(SIZE(PPS),TVG%NNBIOMASS) :: ZRESP_BIOMASS_INST  ! instantaneous biomass respiration (kgCO2/kgair m/s)
!
!  temperatures & thermal conductivities
!
REAL, DIMENSION(SIZE(PPS))           :: ZTA          ! estimate of air temperature at future time
!                                                    ! step as if modified by ISBA flux alone.
!
!   desactivated diag
!
REAL, DIMENSION(SIZE(PPS))   :: ZRN_ISBA      ! net radiative flux from snow-free surface 
REAL, DIMENSION(SIZE(PPS))   :: ZH_ISBA       ! sensible heat flux from snow-free surface 
REAL, DIMENSION(SIZE(PPS))   :: ZLEI_ISBA     ! baresoil evaporation from snow-free surface 
REAL, DIMENSION(SIZE(PPS))   :: ZLEG_ISBA     ! baresoil evaporation from snow-free surface 
REAL, DIMENSION(SIZE(PPS))   :: ZLEGI_ISBA    ! baresoil sublimation from snow-free surface 
REAL, DIMENSION(SIZE(PPS))   :: ZLEV_ISBA     ! total evapotranspiration from vegetation over 
REAL, DIMENSION(SIZE(PPS))   :: ZLETR_ISBA    ! transpiration from snow-free surface 
REAL, DIMENSION(SIZE(PPS))   :: ZUSTAR_ISBA   ! friction velocity from snow-free surface
REAL, DIMENSION(SIZE(PPS))   :: ZLER_ISBA     ! evaporation from canopy water interception 
REAL, DIMENSION(SIZE(PPS))   :: ZLE_ISBA      ! latent heat flux from snow-free surface 
REAL, DIMENSION(SIZE(PPS))   :: ZGFLUX_ISBA   ! net energy flux into the snow-free surface 
REAL, DIMENSION(SIZE(PPS))   :: ZRNSNOW       ! net radiative flux from snow (ISBA-ES:3-L)    (W/m2)
REAL, DIMENSION(SIZE(PPS))   :: ZHSNOW        ! sensible heat flux from snow (ISBA-ES:3-L)    (W/m2)
REAL, DIMENSION(SIZE(PPS))   :: ZHPSNOW       ! heat release from rainfall (ISBA-ES:3-L)      (W/m2)
REAL, DIMENSION(SIZE(PPS))   :: ZGFLUXSNOW    ! net surface energy flux into snowpack (ISBA-ES:3-L)(W/m2)
REAL, DIMENSION(SIZE(PPS))   :: ZUSTARSNOW    ! friction velocity  over snow (ISBA-ES:3-L)    (m/s)
REAL, DIMENSION(SIZE(PPS))   :: ZGRNDFLUX     ! soil/snow interface heat flux (ISBA-ES:3-L)   (W/m2)
REAL, DIMENSION(SIZE(PPS))   :: ZSRSFC        ! snowfall over snowpack (ISBA-ES:3-L)          (kg/m2/s)
REAL, DIMENSION(SIZE(PPS))   :: ZRRSFC        ! rainfall over snowpack (ISBA-ES:3-L)          (kg/m2/s)
REAL, DIMENSION(SIZE(PPS))   :: ZLESL         ! snowpack evaporation (ISBA-ES:3-L)            (W/m2)
REAL, DIMENSION(SIZE(PPS))   :: ZCDSNOW       ! snow drag coefficient (ISBA-ES:3-L)           (-)
REAL, DIMENSION(SIZE(PPS))   :: ZCHSNOW       ! heat turbulent transfer coefficient           (-)
!
REAL, DIMENSION(SIZE(PPS))   :: ZCG
REAL, DIMENSION(SIZE(PPS))   :: ZC1
REAL, DIMENSION(SIZE(PPS))   :: ZC2
REAL, DIMENSION(SIZE(PPS))   :: ZWGEQ
REAL, DIMENSION(SIZE(PPS))   :: ZCT
REAL, DIMENSION(SIZE(PPS))   :: ZRS
REAL, DIMENSION(SIZE(PPS))   :: ZCH
REAL, DIMENSION(SIZE(PPS))   :: ZCD
REAL, DIMENSION(SIZE(PPS))   :: ZCDN
REAL, DIMENSION(SIZE(PPS))   :: ZRI
REAL, DIMENSION(SIZE(PPS))   :: ZHU
REAL, DIMENSION(SIZE(PPS))   :: ZHUG
REAL, DIMENSION(SIZE(PPS))   :: ZRN
REAL, DIMENSION(SIZE(PPS))   :: ZH
REAL, DIMENSION(SIZE(PPS))   :: ZLEI
REAL, DIMENSION(SIZE(PPS))   :: ZLEG
REAL, DIMENSION(SIZE(PPS))   :: ZLEGI
REAL, DIMENSION(SIZE(PPS))   :: ZLEV
REAL, DIMENSION(SIZE(PPS))   :: ZLES
REAL, DIMENSION(SIZE(PPS))   :: ZLER
REAL, DIMENSION(SIZE(PPS))   :: ZLETR
REAL, DIMENSION(SIZE(PPS))   :: ZEVAP
REAL, DIMENSION(SIZE(PPS))   :: ZSUBL
REAL, DIMENSION(SIZE(PPS))   :: ZGFLUX
REAL, DIMENSION(SIZE(PPS))   :: ZRESTORE
REAL, DIMENSION(SIZE(PPS))   :: ZUSTAR
REAL, DIMENSION(SIZE(PPS))   :: ZDRAIN
REAL, DIMENSION(SIZE(PPS))   :: ZRUNOFF
REAL, DIMENSION(SIZE(PPS))   :: ZMELT
REAL, DIMENSION(SIZE(PPS),GRM%TGR%CUR%TSNOW%NLAYER) :: ZSNOWTEMP
REAL, DIMENSION(SIZE(PPS),GRM%TGR%CUR%TSNOW%NLAYER) :: ZSNOWLIQ
REAL, DIMENSION(SIZE(PPS),GRM%TGR%CUR%TSNOW%NLAYER) :: ZSNOWDZ
REAL, DIMENSION(SIZE(PPS))   :: ZSNOWHMASS
REAL, DIMENSION(SIZE(PPS))   :: ZMELTADV
REAL, DIMENSION(SIZE(PPS),3) :: ZIACAN
REAL, DIMENSION(SIZE(PPS))   :: ZQS
REAL, DIMENSION(SIZE(PPS))   :: ZHV
REAL, DIMENSION(SIZE(PPS))   :: ZHORT
REAL, DIMENSION(SIZE(PPS))   :: ZDRIP
REAL, DIMENSION(SIZE(PPS))   :: ZTS
REAL, DIMENSION(SIZE(PPS))   :: ZRRVEG
REAL, DIMENSION(SIZE(PPS))   :: ZALBT
REAL, DIMENSION(SIZE(PPS))   :: ZEMIST
REAL, DIMENSION(SIZE(PPS))   :: ZGPP
REAL, DIMENSION(SIZE(PPS))   :: ZRESP_AUTO
REAL, DIMENSION(SIZE(PPS))   :: ZRESP_ECO
REAL, DIMENSION(SIZE(PPS)) :: ZFAPAR
REAL, DIMENSION(SIZE(PPS)) :: ZFAPIR
REAL, DIMENSION(SIZE(PPS)) :: ZFAPARC
REAL, DIMENSION(SIZE(PPS)) :: ZFAPIRC
REAL, DIMENSION(SIZE(PPS)) :: ZLAI_EFFC
REAL, DIMENSION(SIZE(PPS)) :: ZMUS
REAL, DIMENSION(SIZE(PPS)) :: ZFAPAR_BS
REAL, DIMENSION(SIZE(PPS)) :: ZFAPIR_BS
REAL, DIMENSION(SIZE(PPS)) :: ZIRRIG_FLUX
REAL, DIMENSION(0,0,0)     :: ZLITTER
REAL, DIMENSION(0,0)       :: ZSOILCARB, ZLIGNIN_STRUC, ZTURNOVER
!
REAL, DIMENSION(SIZE(PPS)) :: ZSNDRIFT
!
!  surfaces relative fractions
!
REAL, DIMENSION(SIZE(PPS)) :: ZFFG
REAL, DIMENSION(SIZE(PPS)) :: ZFFV
REAL, DIMENSION(SIZE(PPS)) :: ZFF
REAL, DIMENSION(SIZE(PPS)) :: ZALBF
REAL, DIMENSION(SIZE(PPS)) :: ZEMISF
REAL, DIMENSION(SIZE(PPS)) :: ZFFROZEN
REAL, DIMENSION(SIZE(PPS)) :: ZFFLOOD
REAL, DIMENSION(SIZE(PPS)) :: ZPIFLOOD
REAL, DIMENSION(SIZE(PPS)) :: ZIFLOOD
REAL, DIMENSION(SIZE(PPS)) :: ZPFLOOD
REAL, DIMENSION(SIZE(PPS)) :: ZLEFLOOD
REAL, DIMENSION(SIZE(PPS)) :: ZLEIFLOOD
REAL, DIMENSION(SIZE(PPS)) :: ZFFG_NOSNOW
REAL, DIMENSION(SIZE(PPS)) :: ZFFV_NOSNOW
!
!  variables for irrigation
REAL, DIMENSION(SIZE(PPS)) :: ZIRRIG
REAL, DIMENSION(SIZE(PPS)) :: ZWATSUP
REAL, DIMENSION(SIZE(PPS)) :: ZTHRESHOLDSPT
LOGICAL, DIMENSION(SIZE(PPS)) :: GIRRIGATE
LOGICAL, DIMENSION(SIZE(PPS)) :: GIRRIDAY
!
!  variables for deep soil
!
REAL, DIMENSION(SIZE(PPS)) :: ZGAMMAT  ! not used
REAL, DIMENSION(SIZE(PPS)) :: ZTDEEP_A
REAL, DIMENSION(SIZE(PPS)) :: ZTDEEP_B
!
REAL, DIMENSION(0) :: ZAOSIP  ! A/S for increasing x
REAL, DIMENSION(0) :: ZAOSIM  ! A/S for decreasing x
REAL, DIMENSION(0) :: ZAOSJP  ! A/S for increasing y
REAL, DIMENSION(0) :: ZAOSJM  ! A/S for decreasing y
REAL, DIMENSION(0) :: ZHO2IP  ! h/2 for increasing x
REAL, DIMENSION(0) :: ZHO2IM  ! h/2 for decreasing x
REAL, DIMENSION(0) :: ZHO2JP  ! h/2 for increasing y
REAL, DIMENSION(0) :: ZHO2JM  ! h/2 for decreasing y
REAL, DIMENSION(0) :: ZZ0EFFIP! roughness length for increasing x
REAL, DIMENSION(0) :: ZZ0EFFIM! roughness length for decreasing x
REAL, DIMENSION(0) :: ZZ0EFFJP! roughness length for increasing y
REAL, DIMENSION(0) :: ZZ0EFFJM! roughness length for decreasing y
REAL, DIMENSION(0) :: ZTAU_WOOD  ! residence time in wood (s)
REAL, DIMENSION(0,0) :: ZINCREASE
!
! Dummy variables for MEB:
LOGICAL,PARAMETER::OMEB=.FALSE.
LOGICAL,PARAMETER::OFORC_MEASURE=.FALSE.
LOGICAL,PARAMETER::OMEB_LITTER=.FALSE.
LOGICAL,PARAMETER::OMEB_GNDRES=.FALSE.
REAL, DIMENSION(SIZE(PPS)) :: ZP_MEB_SCA_SW, &
          ZP_RGLV, ZP_GAMMAV, ZP_RSMINV,                                                  &
          ZP_WRMAX_CFV, ZP_LAIV,                                                          &
          ZP_BSLAI,ZP_LAIMIN,ZP_H_VEG,ZPALPHAN,                                           &
          ZZ0G_WITHOUT_SNOW,                                                              &
          ZZ0_MEBV,ZZ0H_MEBV,ZZ0EFF_MEBV,                                                 &
          ZZ0_MEBN,ZZ0H_MEBN,ZZ0EFF_MEBN,                                                 &
          ZP_ALBNIR_VEG, ZP_ALBVIS_VEG,                                                   &
          ZP_ALBNIR_SOIL, ZP_ALBVIS_SOIL, ZP_GNDLITTER
REAL, DIMENSION(SIZE(GRM%TGRP%XROOTFRAC,1),SIZE(GRM%TGRP%XROOTFRAC,2)) :: ZP_ROOTFRACV
REAL, DIMENSION(SIZE(PPS)) :: ZP_WRL,ZP_WRLI,ZP_WRVN,ZP_TV,ZP_TL
REAL, DIMENSION(SIZE(PPS)) :: ZP_TC,ZP_QC
REAL, DIMENSION(SIZE(PPS)) :: ZP_SWNET_V, ZP_SWNET_G, ZP_SWNET_N, ZP_SWNET_NS,       &
          ZP_LWNET_V, ZP_LWNET_G, ZP_LWNET_N,                    &
          ZP_LEVCV, ZP_LESC, ZP_H_V_C, ZP_H_G_C,                          &
          ZP_LETRGV, ZP_LETRCV, ZP_LERGV, ZP_LELITTER, ZP_LELITTERI,      &
          ZP_DRIPLIT,ZP_RRLIT, ZP_LERCV, ZP_H_C_A, ZP_H_N_C,   &
          ZP_LE_C_A, ZP_LE_V_C, ZP_LE_G_C, ZP_LE_N_C,                     &
          ZP_EVAP_N_C, ZP_EVAP_G_C,                                       & 
          ZP_SR_GN, ZP_MELTCV, ZP_FRZCV,                                  &
          ZP_SWDOWN_GN, ZP_LWDOWN_GN
!
TYPE (DATE_TIME),   DIMENSION(0) :: TPSEED ! seeding date
TYPE (DATE_TIME),   DIMENSION(0) :: TPREAP ! reaping date
!
INTEGER                    :: ILU
!
LOGICAL :: GAGRI_TO_GRASS
!
!Snow options
LOGICAL :: GSNOWDRIFT,GSNOWDRIFT_SUBLIM,GSNOW_ABS_ZENITH
 CHARACTER(3) :: YSNOWMETAMO,YSNOWRAD
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*      1.     various initialisations
!              -----------------------
!
IF (LHOOK) CALL DR_HOOK('GREENROOF',0,ZHOOK_HANDLE)
ILU = SIZE(PPS)
!
ZDIRCOSZW = 1.
!
HRAIN     = 'DEF'
OFLOOD    = .FALSE.
OTEMP_ARP = .FALSE.
OGLACIER  = .FALSE.
ZMUF      = 0.
ZFSAT     = 0.
ZTOPQS    = 0.
ZQSB      = 0.
ZFWTD     = 0.
ZWTD      = XUNDEF
ZSNDRIFT  = 0.
!
GAGRI_TO_GRASS = .FALSE.
!
! Snow options
GSNOWDRIFT=.TRUE.
GSNOWDRIFT_SUBLIM=.FALSE.
GSNOW_ABS_ZENITH=.FALSE.
YSNOWMETAMO="B92"
YSNOWRAD="B92"
!
! Van genuchten parameter (not yet inplemented)
!
!*      1.3    flood
!              -----
!
ZFFG          = 0.
ZFFV          = 0.
ZFF           = 0.
ZFFROZEN      = 0.
ZALBF         = 0.
ZEMISF        = 0.
ZIFLOOD       = 0.
ZPFLOOD       = 0.
ZFFLOOD       = 0.
ZPIFLOOD      = 0.
ZLEFLOOD      = 0.
ZLEIFLOOD     = 0.
ZFFG_NOSNOW   = 0.
ZFFV_NOSNOW   = 0.
!
!* irrigation (not implemented)
!
ZIRRIG        = 0.
ZWATSUP       = 0.
ZTHRESHOLDSPT = 0.
GIRRIGATE     = .FALSE.
GIRRIDAY      = .FALSE.
!
!* automatic summer irrigation 
!
PIRRIG_GREENROOF(:) = 0.
!
!* deep soil implicitation with roof
!
ZGAMMAT = XUNDEF
 CALL ROOF_IMPL_COEF(PTSTEP,TOP%NROOF_LAYER, T%CUR%XD_ROOF, T%CUR%XTC_ROOF, T%CUR%XHC_ROOF, &
                     T%CUR%XT_ROOF, ZTDEEP_A,ZTDEEP_B)
!
!-------------------------------------------------------------------------------
!
!* Variables required in TEB to allow coupling
!  with AROME/ALADIN/ARPEGE as LE or EVAP
!
ZLEI  = 0.0 ! sublimation heat flux (W/m2)
ZSUBL = 0.0 ! sublimation (kg/m2/s)
ZTS   = 0.0 ! surface temperature (K) (non-radiative)
!
!-------------------------------------------------------------------------------
!
!*      9.     Treatment of green areas
!              ------------------------
!
!radiative temperature diagnostic
!-------------------------------
!
!*      9.1    Summer irrigation 
!              ------------------
!
!* irrigation automatique de type goutte à goutte (arrosage du sol seulement)
!
 CALL TEB_IRRIG(TIR%LPAR_GR_IRRIG, PTSTEP, TPTIME%TDATE%MONTH, PTSUN, &
               TIR%XGR_START_MONTH, TIR%XGR_END_MONTH, TIR%XGR_START_HOUR,   &
               TIR%XGR_END_HOUR, TIR%XGR_24H_IRRIG, PIRRIG_GREENROOF     )
!
!*      9.2    Call ISBA for greenroofs
!              ------------------------
!
 CALL ISBA(GRM%TGRO%CISBA_GR, TVG%CPHOTO, GRM%TGRO%LTR_ML_GR, 'WSAT', GRM%TGRO%CKSAT_GR,     &
          HRAIN, GRM%TGRO%CHORT_GR, TVG%CC1DRY, GRM%TGRO%CSCOND_GR, GRM%TGR%CUR%TSNOW%SCHEME, &
          TVG%CSNOWRES, TVG%CCPSURF, TVG%CSOILFRZ, TVG%CDIFSFCOND, TPTIME, OFLOOD, &
          OTEMP_ARP, OGLACIER, OMEB, OFORC_MEASURE, OMEB_LITTER, OMEB_GNDRES, PTSTEP,        &
          HIMPLICIT_WIND, GAGRI_TO_GRASS,                          &
          GSNOWDRIFT,GSNOWDRIFT_SUBLIM,GSNOW_ABS_ZENITH,           &
          YSNOWMETAMO,YSNOWRAD,                                    &          
          TVG%XCGMAX, PZREF, PUREF, ZDIRCOSZW, PTA,         &
          PQA, PEXNA, PRHOA, PPS, PEXNS,  PRR, PSR, PZENITH,    &
          ZP_MEB_SCA_SW,                                           &
          PSW, PLW, PVMOD, PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF, &
          PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF, GRM%TGRP%XRSMIN, GRM%TGRP%XRGL, GRM%TGRP%XGAMMA,&
          GRM%TGRP%XCV, GRM%TGRP%XRUNOFFD, GRM%TGRP%XSOILWGHT, &
          GRM%TGRO%NLAYER_HORT_GR, GRM%TGRO%NLAYER_DUN_GR,    &
          PALBNIR_TVEG, PALBVIS_TVEG, PALBNIR_TSOIL, PALBVIS_TSOIL,   &
          GRM%TGR%CUR%XSNOWFREE_ALB, GRM%TGRP%XWRMAX_CF, GRM%TGRPE%CUR%XVEG, &
          GRM%TGRPE%CUR%XLAI, GRM%TGRPE%CUR%XEMIS, GRM%TGRPE%CUR%XZ0,        &
          GRM%TGRPE%CUR%XZ0/GRM%TGRP%XZ0_O_Z0H, GRM%TGRP%XVEGTYPE, GRM%TGRPE%CUR%XZ0,    &
          ZP_RGLV, ZP_GAMMAV, ZP_RSMINV,                              &
          ZP_ROOTFRACV, ZP_WRMAX_CFV, ZP_LAIV,                        &
          ZP_BSLAI,ZP_LAIMIN,ZP_H_VEG,ZPALPHAN,                       &
          ZZ0G_WITHOUT_SNOW,                                          &
          ZZ0_MEBV,ZZ0H_MEBV,ZZ0EFF_MEBV,                             &
          ZZ0_MEBN,ZZ0H_MEBN,ZZ0EFF_MEBN, ZP_GNDLITTER,               &
          GRM%TGRP%XRUNOFFB_GR, GRM%TGRP%XCGSAT, GRM%TGRP%XC1SAT,     &
          GRM%TGRP%XC2REF, GRM%TGRP%XC3, GRM%TGRP%XC4B, GRM%TGRP%XC4REF, GRM%TGRP%XACOEF, &
          GRM%TGRP%XPCOEF, GRM%TGRP%XTAUICE, GRM%TGRP%XWDRAIN_GR,&
          ZTDEEP_A, ZTDEEP_B, ZGAMMAT, GRM%TGR%CUR%XPSN, GRM%TGR%CUR%XPSNG, &
          GRM%TGR%CUR%XPSNV, GRM%TGR%CUR%XPSNV_A,   &
          GRM%TGR%CUR%XSNOWFREE_ALB_VEG, GRM%TGR%CUR%XSNOWFREE_ALB_SOIL, ZIRRIG, ZWATSUP,     &
          ZTHRESHOLDSPT, GIRRIGATE, GIRRIDAY, GRM%TGRP%LSTRESS, GRM%TGRP%XGC, GRM%TGRP%XF2I,     &
          GRM%TGRP%XDMAX, GRM%TGRP%XAH, GRM%TGRP%XBH, PCO2, GRM%TGRP%XGMES, &
          GRM%TGRP%XPOI, GRM%TGRP%XFZERO, GRM%TGRP%XEPSO, GRM%TGRP%XGAMM,   &
          GRM%TGRP%XQDGAMM, GRM%TGRP%XQDGMES, GRM%TGRP%XT1GMES, GRM%TGRP%XT2GMES, &
          GRM%TGRP%XAMAX, GRM%TGRP%XQDAMAX, GRM%TGRP%XT1AMAX,&
          GRM%TGRP%XT2AMAX, GRM%TGRP%XABC, GRM%TGRP%XDG, GRM%TGRP%XDZG, GRM%TGRP%XDZDIF, &
          GRM%TGRP%NWG_LAYER, GRM%TGRP%XROOTFRAC,  &
          GRM%TGRP%XWFC, GRM%TGRP%XWWILT, GRM%TGRP%XWSAT, GRM%TGRP%XBCOEF,  GRM%TGRP%XCONDSAT, &
          GRM%TGRP%XMPOTSAT, GRM%TGRP%XHCAPSOIL, GRM%TGRP%XCONDDRY, GRM%TGRP%XCONDSLD, &
          GRM%TGRP%XD_ICE, GRM%TGRP%XKSAT_ICE, ZMUF, ZFF,&
          ZFFG, ZFFV, ZFFG_NOSNOW, ZFFV_NOSNOW, ZFFROZEN,  ZALBF,     &
          ZEMISF, ZFFLOOD, ZPIFLOOD, ZIFLOOD, ZPFLOOD, ZLEFLOOD,      &
          ZLEIFLOOD, ZSODELX, TG%XLAT, TG%XLON, GRM%TGR%CUR%XTG, GRM%TGR%CUR%XWG, &
          GRM%TGR%CUR%XWGI, GRM%TGRP%XPCPS, GRM%TGRP%XPLVTT, GRM%TGRP%XPLSTT, GRM%TGR%CUR%XWR, &
          ZP_WRL,ZP_WRLI,ZP_WRVN,ZP_TV, ZP_TL,                                &
          GRM%TGR%CUR%XRESA, GRM%TGR%CUR%XANFM, ZFSAT, GRM%TGR%CUR%TSNOW%ALB(:,1),             &
          GRM%TGR%CUR%TSNOW%ALBVIS(:,1), GRM%TGR%CUR%TSNOW%ALBNIR(:,1), GRM%TGR%CUR%TSNOW%ALBFIR(:,1),    &
          GRM%TGR%CUR%TSNOW%WSNOW(:,:,1), GRM%TGR%CUR%TSNOW%HEAT(:,:,1), GRM%TGR%CUR%TSNOW%RHO(:,:,1),    &
          GRM%TGR%CUR%TSNOW%GRAN1(:,:,1), GRM%TGR%CUR%TSNOW%GRAN2(:,:,1), GRM%TGR%CUR%TSNOW%HIST(:,:,1),  &
          GRM%TGR%CUR%TSNOW%AGE(:,:,1), ZGRNDFLUX, ZHPSNOW, ZSNOWHMASS,           &
          ZRNSNOW, ZHSNOW,  ZGFLUXSNOW, ZUSTARSNOW,                   &
          ZSRSFC, ZRRSFC, ZLESL, GRM%TGR%CUR%TSNOW%EMIS(:,1), ZCDSNOW, ZCHSNOW,   &
          PTS_GREENROOF, ZTS, ZHV, ZQS, ZSNOWTEMP, ZSNOWLIQ, ZSNOWDZ, &
          ZCG, ZC1, ZC2, ZWGEQ, ZCT, ZCH, ZCD, ZCDN, ZRI, ZHU, ZHUG,  &
          ZEMIST, ZALBT, ZRS, GRM%TGR%CUR%XLE,  ZRN, ZH, ZLEI, ZLEGI, ZLEG, ZLEV, &
          ZLES, ZLER, ZLETR, ZEVAP, ZGFLUX, ZRESTORE, ZUSTAR, ZDRAIN, &
          ZRUNOFF, ZMELT, ZMELTADV,                                   &
          ZP_TC,ZP_QC,                                                &
          ZRN_ISBA, ZH_ISBA, ZLEG_ISBA,                               &
          ZLEGI_ISBA, ZLEV_ISBA, ZLETR_ISBA, ZUSTAR_ISBA, ZLER_ISBA,  &
          ZLE_ISBA, ZLEI_ISBA, ZGFLUX_ISBA, ZHORT, ZDRIP, ZRRVEG,     &
          PAC_AGG_GREENROOF, PHU_AGG_GREENROOF, ZFAPARC, ZFAPIRC, ZMUS,     &
          ZLAI_EFFC, GRM%TGR%CUR%XAN, GRM%TGR%CUR%XANDAY, ZRESP_BIOMASS_INST, ZIACAN, GRM%TGRP%XANF,   &
          ZGPP, ZFAPAR, ZFAPIR, ZFAPAR_BS, ZFAPIR_BS, ZIRRIG_FLUX,    &
          PDEEP_FLUX,                                                 &
          ZP_SWNET_V, ZP_SWNET_G, ZP_SWNET_N, ZP_SWNET_NS,            &
          ZP_LWNET_V, ZP_LWNET_G, ZP_LWNET_N,                         &
          ZP_LEVCV, ZP_LESC, ZP_H_V_C, ZP_H_G_C,                      &
          ZP_LETRGV, ZP_LETRCV, ZP_LERGV, ZP_LELITTER, ZP_LELITTERI,  &
          ZP_DRIPLIT,ZP_RRLIT, ZP_LERCV, ZP_H_C_A, ZP_H_N_C,   &
          ZP_LE_C_A, ZP_LE_V_C, ZP_LE_G_C, ZP_LE_N_C,                 &
          ZP_EVAP_N_C, ZP_EVAP_G_C,                                   & 
          ZP_SR_GN, ZP_MELTCV, ZP_FRZCV,                              &
          ZP_SWDOWN_GN, ZP_LWDOWN_GN,                                 &
          PIRRIG_GREENROOF, ZTOPQS, ZQSB, ZSUBL, ZFWTD, ZWTD, ZSNDRIFT)  
!
PRUNOFF_GREENROOF(:) = ZRUNOFF(:)
PDRAIN_GREENROOF(:)  = ZDRAIN(:)

!
IF (GRM%TGR%CUR%TSNOW%SCHEME=='3-L' .OR. GRM%TGR%CUR%TSNOW%SCHEME=='CRO') GRM%TGR%CUR%TSNOW%TS(:,1)=ZSNOWTEMP(:,1)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Diagnostic of respiration carbon fluxes and soil carbon evolution
!
! --------------------------------------------------------------------------------------
! Vegetation update (in case of non-interactive vegetation):
! --------------------------------------------------------------------------------------
!
IF (TVG%CPHOTO=='NON' .OR. TVG%CPHOTO=='AGS' .OR. TVG%CPHOTO=='AST') THEN
     CALL VEGETATION_UPDATE_GREENROOF(DTCO, DTI, IG, I, T, TOP, TVG, DTGD, GRM,  &
                                      TPTIME,PTSTEP,ILU)
END IF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Vegetation evolution for interactive LAI
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (TVG%CPHOTO=='LAI' .OR. TVG%CPHOTO=='LST' .OR. TVG%CPHOTO=='NIT') THEN
  CALL VEGETATION_EVOL(GRM%TGRO%CISBA_GR, TVG%CPHOTO, TVG%CRESPSL, TVG%CALBEDO, .FALSE., &
                       GRM%TGRO%LTR_ML_GR, TVG%LNITRO_DILU, GAGRI_TO_GRASS,  &
                       .FALSE., .FALSE., .FALSE.,     &
                       PTSTEP, TPTIME%TDATE%MONTH, TPTIME%TDATE%DAY, 1,    &
                       TPTIME%TIME, TG%XLAT, PRHOA, GRM%TGRP%XDG, GRM%TGRP%XDZG, &
                       GRM%TGRP%NWG_LAYER, GRM%TGR%CUR%XTG, GRM%TGRP%XALBNIR_VEG, &
                       GRM%TGRP%XALBVIS_VEG, GRM%TGRP%XALBUV_VEG, GRM%TGRP%XALBNIR_SOIL, &
                       GRM%TGRP%XALBVIS_SOIL, GRM%TGRP%XALBUV_SOIL, GRM%TGRP%XVEGTYPE, &
                       GRM%TGRP%XSEFOLD, GRM%TGRP%XANMAX, GRM%TGRP%XH_TREE, GRM%TGRP%XBSLAI, &
                       GRM%TGRP%XLAIMIN, PCO2, GRM%TGRP%XCE_NITRO, GRM%TGRP%XCF_NITRO, &
                       GRM%TGRP%XCNA_NITRO, GRM%TGRP%XBSLAI_NITRO, GRM%TGRP%XGMES, &
                       ZTAU_WOOD, TPSEED, TPREAP, ZAOSIP, ZAOSIM, ZAOSJP, ZAOSJM,   &
                       ZHO2IP, ZHO2IM, ZHO2JP, ZHO2JM, ZZ0EFFIP,           &
                       ZZ0EFFIM, ZZ0EFFJP, ZZ0EFFJM, GRM%TGRPE%CUR%XLAI, GRM%TGRPE%CUR%XVEG,  &
                       GRM%TGRPE%CUR%XZ0, GRM%TGRPE%CUR%XALBNIR, GRM%TGRPE%CUR%XALBVIS, &
                       GRM%TGRPE%CUR%XALBUV, GRM%TGRPE%CUR%XEMIS, GRM%TGR%CUR%XANFM, &
                       GRM%TGR%CUR%XANDAY, GRM%TGR%CUR%XBIOMASS, GRM%TGR%CUR%XRESP_BIOMASS,        &
                       ZRESP_BIOMASS_INST, ZINCREASE, ZTURNOVER             )          
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
PSFCO2    (:)=0.
ZRESP_ECO (:)=0.
ZRESP_AUTO(:)=0.
!
IF (TVG%CPHOTO/='NON' .AND. TVG%CRESPSL/='NON' .AND. ANY(GRM%TGRPE%CUR%XLAI(:)/=XUNDEF)) THEN
  ! faire intervenir le type de vegetation du greenroof ? (CTYP_GR)
  CALL CARBON_EVOL(TVG%CISBA, TVG%CRESPSL, TVG%CPHOTO, PTSTEP, 1,               &
                     PRHOA, GRM%TGR%CUR%XTG, GRM%TGR%CUR%XWG, GRM%TGRP%XWFC, &
                     GRM%TGRP%XWWILT, GRM%TGRP%XWSAT, GRM%TGRP%XSAND_GR,&
                     GRM%TGRP%XDG, GRM%TGRP%XDZG, GRM%TGRP%NWG_LAYER,                 &                   
                     GRM%TGRP%XRE25, GRM%TGRPE%CUR%XLAI, ZRESP_BIOMASS_INST, ZTURNOVER,    &
                     ZLITTER, ZLIGNIN_STRUC , ZSOILCARB,            &
                     ZRESP_AUTO, ZRESP_ECO                          ) 
  ! calculation of vegetation CO2 flux
  ! Positive toward the atmosphere
  PSFCO2(:) = ZRESP_ECO(:) - ZGPP(:)
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      9.     Fields required for TEB
!              -----------------------
!
! energy balance
!
 PRN_GREENROOF    (:) = ZRN    (:)
 PH_GREENROOF     (:) = ZH     (:)
 PLE_GREENROOF    (:) = GRM%TGR%CUR%XLE    (:)
 PGFLUX_GREENROOF (:) = ZGFLUX (:)
 PEVAP_GREENROOF  (:) = ZEVAP  (:)
!
!
! Estimate of green area aerodynamic conductance recomputed from heat flux,
! surface (radiative) temp. and forcing air temperature (estimated at future time step)
 ZTA = PPET_B_COEF + PPET_A_COEF * PH_GREENROOF
 PAC_GREENROOF = 0.
 WHERE (PTS_GREENROOF /= ZTA)
   PAC_GREENROOF(:)   = MAX(PH_GREENROOF(:) / XCPD / PRHOA(:) / (PTS_GREENROOF - ZTA) , 0.)
 ENDWHERE
!
! Humidity of saturation for green areas
 PQSAT_GREENROOF(:) = QSAT(GRM%TGR%CUR%XTG(:,1),PPS(:))
!
!* friction flux
  PUW_GREENROOF(:)    = -ZUSTAR(:)**2
IF (LHOOK) CALL DR_HOOK('GREENROOF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE GREENROOF
