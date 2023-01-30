!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE COUPLING_TEB_n (DTCO, DST, SLT, TOP, SPAOP, SB, G, CHT, NT, TPN, TIR, BOP, NB, TD, AT, &
                           GDM, GRM, HM, HPROGRAM, HCOUPLING, PTSTEP, KYEAR, KMONTH,   &
                           KDAY, PTIME, KI, KSV, KSW, KLEV, PTSUN, PZENITH, PAZIM,     &
                           PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV, &
                           PRAIN, PSN, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA,     &
                           PTKE, PSFTQ, PSFTQ_SURF, PSFTQ_WALL, PSFTQ_ROOF, PSFTH,     &
                           PSFTH_SURF, PSFTH_WALL, PSFTH_ROOF, PCD_ROOF, PSFTS, PSFCO2,&
                           PSFU, PSFV, PTRAD, PDIR_ALB, PSCA_ALB, PEMIS, PTSURF, PZ0,  &
                           PZ0H, PQSURF, PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF,        &
                           PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF, HTEST )
!     ###############################################################################
!
!!****  *COUPLING_TEB_n * - Driver for TEB 
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!                  10/2005 (G.Pigeon) transfer of domestic heating
!!      S. Riette   06/2009 Initialisation of XT, XQ, XU and XTKE on canopy levels
!!      S. Riette   01/2010 Use of interpol_sbl to compute 10m wind diagnostic
!!      G. Pigeon   09/2012 CCH_BEM, ROUGH_WALL, ROUGH_ROOF for building conv. coef
!!      G. Pigeon   10/2012 XF_WIN_WIN as arg. of TEB_GARDEN
!!      B. Decharme 09/2012 New wind implicitation
!!      J. Escobar  09/2012 KI not allowed without-interface , replace by KI
!!      V. Masson   08/2013 adds solar panels & occupation calendar
!!      B. Decharme 04/2013 new coupling variables
!!      M. Goret    02/2017 add heating fractions and CO2 conversion factors as arg. of TEB_GARDEN
!!      M. Goret    03/2017 add traffic flux modulation
!!      A. Lemonsu  06/2017 utci calculations with urban trees
!!      M. Goret    04/2017 suppress PEFF_HEAT as arg. of TEB_GARDEN
!!      M. Goret    07/2017 move CO2 flux diagnostics from DGT to DGMT
!!      M. Goret    07/2017 add heating energy consumption by source
!!      M. Goret    07/2017 add anthropogenic flux diagnostics
!!      M. Goret    09/2017 add diagnostic of heat storage link to snow
!!      M. Goret    10/2017 add hot water 
!!      R. Schoetter   2017 Verification of energy conservation
!!      V. Masson   04.2020 completes energy check for high vegetation IR exchanges
!!---------------------------------------------------------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DST_n, ONLY : DST_t
USE MODD_SLT_n, ONLY : SLT_t
!
USE MODD_CH_TEB_n, ONLY : CH_TEB_t
USE MODD_CANOPY_n, ONLY: CANOPY_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_SPARTACUS_OPTION_n, ONLY : SPARTACUS_OPTIONS_t
USE MODD_TEB_PANEL_n, ONLY : TEB_PANEL_t
USE MODD_TEB_IRRIG_n, ONLY : TEB_IRRIG_t
USE MODD_TEB_n, ONLY : TEB_NP_t
USE MODD_SURFEX_n, ONLY : TEB_DIAG_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_BEM_n, ONLY : BEM_NP_t
USE MODD_DATA_TEB_n, ONLY : DATA_TEB_t
!
USE MODD_CHECK_TEB, ONLY : CHECK_TEB_t
!
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_HYDRO_MODEL_t
!
USE MODD_REPROD_OPER, ONLY : CIMPLICIT_WIND
!
USE MODD_CSTS, ONLY : XRD, XCPD, XP00, XLVTT, XSURF_EPSILON, &
                              XPI, XKARMAN, XG, XTT
USE MODD_SURF_PAR, ONLY : XUNDEF
!                            
USE MODD_DST_SURF
USE MODD_SLT_SURF
!
USE MODD_SURF_ATM_TURB_n, ONLY : SURF_ATM_TURB_t
!
USE MODE_DSLT_SURF
USE MODE_THERMOS
USE MODE_SBLS
!
USE MODI_ABOR1_SFX
USE MODI_ADD_FORECAST_TO_DATE_SURF
USE MODI_ALLOC_CHECK_TEB
USE MODI_AVERAGE_RAD
USE MODI_CANOPY_EVOL
USE MODI_CANOPY_GRID_UPDATE
USE MODI_CH_AER_DEP
USE MODI_CH_DEP_TOWN
USE MODI_CHECK_TEB
USE MODI_CUMUL_DIAG_TEB_n
USE MODI_DIAG_INLINE_TEB_n
USE MODI_DEALLOC_CHECK_TEB
USE MODI_DSLT_DEP
USE MODI_INTERPOL_SBL
USE MODI_SM10
USE MODI_TOWN_ENERGY_BALANCE
USE MODI_TEB_CANOPY
USE MODI_TRAFFIC_FLUX_MODULATION
USE MODI_UTCI_TEB
USE MODI_UTCIC_STRESS
! 
USE YOMHOOK, ONLY : LHOOK, DR_HOOK
USE PARKIND1, ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DST_t), INTENT(INOUT) :: DST
TYPE(SLT_t), INTENT(INOUT) :: SLT
!
TYPE(CH_TEB_t), INTENT(INOUT) :: CHT 
TYPE(CANOPY_t), INTENT(INOUT) :: SB
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(SPARTACUS_OPTIONS_t), INTENT(INOUT) :: SPAOP
TYPE(TEB_PANEL_t), INTENT(INOUT) :: TPN
TYPE(TEB_IRRIG_t), INTENT(INOUT) :: TIR
TYPE(TEB_NP_t), INTENT(INOUT) :: NT
!
TYPE(TEB_DIAG_t), INTENT(INOUT) :: TD
!
TYPE(SURF_ATM_TURB_t), INTENT(IN) :: AT         ! atmospheric turbulence parameters
!
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP 
TYPE(BEM_NP_t), INTENT(INOUT) :: NB
!
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
TYPE(TEB_HYDRO_MODEL_t), INTENT(INOUT) :: HM
!
CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
CHARACTER(LEN=1),    INTENT(IN)  :: HCOUPLING ! type of coupling
                                              ! 'E' : explicit
                                              ! 'I' : implicit
INTEGER,             INTENT(IN)  :: KYEAR     ! current year (UTC)
INTEGER,             INTENT(IN)  :: KMONTH    ! current month (UTC)
INTEGER,             INTENT(IN)  :: KDAY      ! current day (UTC)
REAL,                INTENT(IN)  :: PTIME     ! current time since midnight (UTC, s)
INTEGER,             INTENT(IN)  :: KI        ! number of points
INTEGER,             INTENT(IN)  :: KSV       ! number of scalars
INTEGER,             INTENT(IN)  :: KSW       ! number of short-wave spectral bands
INTEGER,             INTENT(IN)  :: KLEV      ! number of atmospheric levels to couple
REAL, DIMENSION(KI), INTENT(IN)  :: PTSUN     ! solar time                    (s from midnight)
REAL,                INTENT(IN)  :: PTSTEP    ! atmospheric time-step                 (s)
REAL, DIMENSION(KI,KLEV), INTENT(IN)  :: PZREF     ! height of T,q forcing                 (m)
REAL, DIMENSION(KI,KLEV), INTENT(IN)  :: PUREF     ! height of wind forcing                (m)
!
REAL, DIMENSION(KI,KLEV), INTENT(IN)  :: PTA       ! air temperature forcing               (K)
REAL, DIMENSION(KI,KLEV), INTENT(IN)  :: PQA       ! air humidity forcing                  (kg/m3)
REAL, DIMENSION(KI,KLEV), INTENT(IN)  :: PRHOA     ! air density                           (kg/m3)
REAL, DIMENSION(KI,KSV),INTENT(IN) :: PSV     ! scalar variables
!                                             ! chemistry:   first char. in HSV: '#'  (molecule/m3)
!                                             !
 CHARACTER(LEN=6), DIMENSION(KSV),INTENT(IN):: HSV  ! name of all scalar variables
REAL, DIMENSION(KI,KLEV), INTENT(IN)  :: PU        ! zonal wind                            (m/s)
REAL, DIMENSION(KI,KLEV), INTENT(IN)  :: PV        ! meridian wind                         (m/s)
REAL, DIMENSION(KI,KSW),INTENT(IN) :: PDIR_SW ! direct  solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI,KSW),INTENT(IN) :: PSCA_SW ! diffuse solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KSW),INTENT(IN)  :: PSW_BANDS ! mean wavelength of each shortwave band (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH   ! zenithal angle       (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PAZIM     ! azimuthal angle      (radian from North, clockwise)
REAL, DIMENSION(KI), INTENT(IN)  :: PLW       ! longwave radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI), INTENT(IN)  :: PPS       ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(KI,KLEV), INTENT(IN)  :: PPA  ! pressure at forcing level             (Pa)
REAL, DIMENSION(KI,KLEV), INTENT(IN)  :: PTKE ! Turbulent kinetic energy at forcing level (m2/s2)
REAL, DIMENSION(KI), INTENT(IN)  :: PZS       ! atmospheric model orography           (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PCO2      ! CO2 concentration in the air          (kg/m3)
REAL, DIMENSION(KI), INTENT(INOUT)  :: PSN    ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KI), INTENT(INOUT)  :: PRAIN  ! liquid precipitation                  (kg/m2/s)
!
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTH     ! flux of heat                          (W/m2)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTH_SURF
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTH_WALL
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTH_ROOF
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTQ     ! flux of water vapor                   (kg/m2/s)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTQ_SURF 
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTQ_WALL
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTQ_ROOF
REAL, DIMENSION(KI), INTENT(OUT) :: PSFU      ! zonal momentum flux                   (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFV      ! meridian momentum flux                (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PCD_ROOF  ! Drag coefficient for roofs multiplied by roof density (-)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFCO2    ! flux of CO2                           (m/s*kg_CO2/kg_air)
REAL, DIMENSION(KI,KSV),INTENT(OUT):: PSFTS   ! flux of scalar var.                   (kg/m2/s)
!
REAL, DIMENSION(KI), INTENT(OUT) :: PTRAD     ! radiative temperature                 (K)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PDIR_ALB! direct albedo for each spectral band  (-)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PSCA_ALB! diffuse albedo for each spectral band (-)
REAL, DIMENSION(KI), INTENT(OUT) :: PEMIS     ! emissivity                            (-)
!
REAL, DIMENSION(KI), INTENT(OUT) :: PTSURF    ! surface effective temperature         (K)
REAL, DIMENSION(KI), INTENT(OUT) :: PZ0       ! roughness length for momentum         (m)
REAL, DIMENSION(KI), INTENT(OUT) :: PZ0H      ! roughness length for heat             (m)
REAL, DIMENSION(KI), INTENT(OUT) :: PQSURF    ! specific humidity at surface          (kg/kg)
!
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_A_COEF! implicit coefficients
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_B_COEF! needed if HCOUPLING='I'
REAL, DIMENSION(KI), INTENT(IN) :: PPET_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPET_B_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_B_COEF
CHARACTER(LEN=2),    INTENT(IN) :: HTEST ! must be equal to 'OK'
!
!*      0.2    declarations of local variables
!
INTEGER              :: JSWB       ! loop counter on shortwave spectral bands
!         
REAL, DIMENSION(KI)  :: ZQA       ! specific humidity                 (kg/kg)
REAL, DIMENSION(KI)  :: ZEXNA     ! Exner function at forcing level
REAL, DIMENSION(KI)  :: ZEXNS     ! Exner function at surface level
REAL, DIMENSION(KI,KLEV) :: ZWIND ! wind
INTEGER :: ZCTL
!
! Ouput Diagnostics:
!
REAL, DIMENSION(KI)  :: ZU_CANYON   ! wind in canyon
REAL, DIMENSION(KI)  :: ZT_CANYON   ! temperature in canyon
REAL, DIMENSION(KI)  :: ZQ_CANYON   ! specific humidity in canyon
REAL, DIMENSION(KI)  :: ZT_CAN      ! temperature in canyon       (evolving in TEB)
REAL, DIMENSION(KI)  :: ZQ_CAN      ! specific humidity in canyon (evolving in TEB)
REAL, DIMENSION(KI)  :: ZTA_HVEG    ! temperature in canyon       at tree level
REAL, DIMENSION(KI)  :: ZQA_HVEG    ! specific humidity in canyon at tree level
REAL, DIMENSION(KI)  :: ZTS_HVEG    ! temperature of high vegetation
!
REAL, DIMENSION(KI)  :: ZPEW_A_COEF   ! implicit coefficients
REAL, DIMENSION(KI)  :: ZPEW_B_COEF   ! needed if HCOUPLING='I'
!
REAL, DIMENSION(KI) :: ZT_LOWCAN  ! temperature at lowest canyon level (K)
REAL, DIMENSION(KI) :: ZQ_LOWCAN  ! humidity    at lowest canyon level (kg/kg)
REAL, DIMENSION(KI) :: ZU_LOWCAN  ! wind speed at lowest canyon level (m/s)
REAL, DIMENSION(KI) :: ZZ_LOWCAN  ! height      of lowest canyon level (m)
!
REAL, DIMENSION(KI) :: ZPEW_A_COEF_LOWCAN   ! implicit coefficients for wind coupling
REAL, DIMENSION(KI) :: ZPEW_B_COEF_LOWCAN   ! between first canopy level and road
!
REAL, DIMENSION(KI) :: ZTA        ! temperature at canyon level just above roof (K)
REAL, DIMENSION(KI) :: ZPA        ! pressure    at canyon level just above roof (K)
REAL, DIMENSION(KI) :: ZUA        ! wind        at canyon level just above roof (m/s)
REAL, DIMENSION(KI) :: ZUREF      ! height      of canyon level just above roof (m)
REAL, DIMENSION(KI) :: ZZREF      ! height      of canyon level just above roof (m)
!
REAL, DIMENSION(KI)  :: ZDIR_SW       ! total direct SW
REAL, DIMENSION(KI)  :: ZSCA_SW       ! total diffuse SW
REAL, DIMENSION(KI) :: ZAVG_SCA_SW
REAL, DIMENSION(KI) :: ZAVG_DIR_SW 
REAL, DIMENSION(KI) :: ZAVG_DIR_SW_ROAD
!
REAL, DIMENSION(KI)  :: ZAVG_H_WL
REAL, DIMENSION(KI)  :: ZAVG_E_WL
!
REAL, DIMENSION(KI,BOP%NBEMCOMP)  :: ZAVG_TI_BLD
REAL, DIMENSION(KI,BOP%NBEMCOMP)  :: ZAVG_QI_BLD
!
REAL, DIMENSION(KI)  :: ZRN_GRND    ! net radiation on ground built surf
REAL, DIMENSION(KI)  :: ZH_GRND     ! sensible heat flux on ground built surf
REAL, DIMENSION(KI)  :: ZLE_GRND    ! latent heat flux on ground built surf
REAL, DIMENSION(KI)  :: ZGFLX_GRND ! storage flux in ground built surf
REAL, DIMENSION(KI)  :: ZUW_GRND      ! momentum flux for ground built surf
REAL, DIMENSION(KI)  :: ZDUWDU_GRND   !
REAL, DIMENSION(KI) :: ZEMIT_LW_HVEG
REAL, DIMENSION(KI)  :: ZAVG_UW_GRND
REAL, DIMENSION(KI)  :: ZAVG_DUWDU_GRND
REAL, DIMENSION(:,:), ALLOCATABLE :: ZAVG_DH_HVEG
REAL, DIMENSION(:,:), ALLOCATABLE :: ZAVG_DE_HVEG
REAL, DIMENSION(KI)  :: ZAVG_AC_GRND
REAL, DIMENSION(KI)  :: ZAVG_AC_GRND_WAT
REAL, DIMENSION(KI) :: ZSCA_SW_SKY  ! diff solar rad from the sky received by people (incl attenuation by trees)
REAL, DIMENSION(KI) :: ZLW_RAD_SKY  ! IR rad from the sky received by people (incl attenuation by trees)
!
REAL, DIMENSION(KI)  :: ZRESA_TOWN          ! aerodynamical resistance
REAL, DIMENSION(KI)  :: ZAC_GRND            ! ground built surf aerodynamical conductance
REAL, DIMENSION(KI)  :: ZAC_GRND_WAT        ! ground built surf water aerodynamical conductance
!
REAL, DIMENSION(KI)  :: ZLEW_RF   ! latent heat flux on snowfree roof
REAL, DIMENSION(KI)  :: ZRNSN_RF  ! net radiation over snow
REAL, DIMENSION(KI)  :: ZHSN_RF   ! sensible heat flux over snow
REAL, DIMENSION(KI)  :: ZLESN_RF  ! latent heat flux over snow
REAL, DIMENSION(KI)  :: ZGSN_RF   ! flux under the snow
REAL, DIMENSION(KI)  :: ZMELT_RF    ! snow melt
REAL, DIMENSION(KI)  :: ZUW_RF      ! momentum flux for roofs
REAL, DIMENSION(KI)  :: ZDUWDU_RF   !
REAL, DIMENSION(KI)  :: ZAVG_UW_RF
REAL, DIMENSION(KI)  :: ZAVG_DUWDU_RF
REAL, DIMENSION(KI)  :: ZAVG_H_RF
REAL, DIMENSION(KI)  :: ZAVG_E_RF
!
REAL, DIMENSION(KI)  :: ZLEW_RD   ! latent heat flux on snowfree road
REAL, DIMENSION(KI)  :: ZRNSN_RD  ! net radiation over snow
REAL, DIMENSION(KI)  :: ZHSN_RD   ! sensible heat flux over snow
REAL, DIMENSION(KI)  :: ZLESN_RD  ! latent heat flux over snow
REAL, DIMENSION(KI)  :: ZGSN_RD   ! flux under the snow
REAL, DIMENSION(KI)  :: ZMELT_RD    ! snow melt
REAL, DIMENSION(KI)  :: ZAC_RD      ! road aerodynamical conductance
REAL, DIMENSION(KI)  :: ZAC_RD_WAT  ! road water aerodynamical conductance
!
REAL, DIMENSION(KI)  :: ZAC_GD    ! green area aerodynamical conductance
REAL, DIMENSION(KI)  :: ZAC_GD_WAT! green area water aerodynamical conductance
!
REAL, DIMENSION(KI)  :: ZAC_GRF ! green roof aerodynamical conductance
REAL, DIMENSION(KI)  :: ZAC_GRF_WAT! green roof water aerodynamical conductance
!
REAL, DIMENSION(KI)  :: ZTRAD         ! radiative temperature for current patch
REAL, DIMENSION(KI)  :: ZEMIS         ! emissivity for current patch
REAL, DIMENSION(KI,TOP%NTEB_PATCH) :: ZTRAD_PATCH ! radiative temperature for each patch
REAL, DIMENSION(KI,TOP%NTEB_PATCH) :: ZEMIS_PATCH ! emissivity for each patch
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZLAD_CAN  ! vertical profile of Leaf Area Density on canopy grid
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDH_HVEG ! sensible heat flux from trees discretized on caopy grid
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDLE_HVEG! latent heat flux from trees discretized on caopy grid
!
REAL, DIMENSION(KI)  :: ZDIR_ALB      ! direct albedo of town
REAL, DIMENSION(KI)  :: ZSCA_ALB      ! diffuse albedo of town
REAL, DIMENSION(KI,KSW,TOP%NTEB_PATCH) :: ZDIR_ALB_PATCH ! direct albedo per wavelength and patch
REAL, DIMENSION(KI,KSW,TOP%NTEB_PATCH) :: ZSCA_ALB_PATCH ! diffuse albedo per wavelength and patch
!
REAL, DIMENSION(KI)  :: ZRI             ! Richardson number
REAL, DIMENSION(KI)  :: ZCD             ! drag coefficient
REAL, DIMENSION(KI)  :: ZCDN            ! neutral drag coefficient
REAL, DIMENSION(KI)  :: ZCH             ! heat drag
REAL, DIMENSION(KI)  :: ZRN             ! net radiation over town
REAL, DIMENSION(KI)  :: ZH              ! sensible heat flux over town
REAL, DIMENSION(KI)  :: ZH_TOWN_SURF    ! sensible heat flux over town, surface level
REAL, DIMENSION(KI)  :: ZH_TOWN_WALL    ! sensible heat flux over town, wall level
REAL, DIMENSION(KI)  :: ZH_TOWN_ROOF    ! sensible heat flux over town, roof level
REAL, DIMENSION(KI)  :: ZLE             ! latent heat flux over town
REAL, DIMENSION(KI)  :: ZGFLX           ! flux through the ground
REAL, DIMENSION(KI)  :: ZQF             ! anthropogenic flux over town
REAL, DIMENSION(KI)  :: ZEVAP           ! evaporation (km/m2/s)
REAL, DIMENSION(KI)  :: ZEVAP_TOWN_SURF ! evaporation flux, surface level (kg/m2/s)
REAL, DIMENSION(KI)  :: ZEVAP_TOWN_WALL ! evaporation flux, wall level (kg/m2/s)
REAL, DIMENSION(KI)  :: ZEVAP_TOWN_ROOF ! evaporation flux, roof level (kg/m2/s)
!
REAL, DIMENSION(KI)  :: ZAVG_T_CANYON ! temperature in canyon for town 
REAL, DIMENSION(KI)  :: ZAVG_Q_CANYON ! specific humidity in canyon for town
REAL, DIMENSION(KI)  :: ZAVG_CD       ! aggregated drag coefficient
REAL, DIMENSION(KI)  :: ZAVG_CDN      ! aggregated neutral drag coefficient
REAL, DIMENSION(KI)  :: ZAVG_RI       ! aggregated Richardson number
REAL, DIMENSION(KI)  :: ZAVG_CH       ! aggregated Heat transfer coefficient
!
! new local variables after BEM
!
REAL, DIMENSION(KI) :: ZUSTAR        ! friction velocity
REAL, DIMENSION(KI) :: ZSFU          ! momentum flux for patch (U direction)
REAL, DIMENSION(KI) :: ZSFV          ! momentum flux for patch (V direction)
REAL, DIMENSION(KI) :: ZH_TRAFFIC    ! anthropogenic sensible
!                                     ! heat fluxes due to traffic
REAL, DIMENSION(KI) :: ZLE_TRAFFIC   ! anthropogenic latent
!                                     ! heat fluxes due to traffic
REAL, DIMENSION(KI) :: ZTRAF_MODULATION ! modulation of traffic CO2 flux as a function of month, day and hour
REAL, DIMENSION(KI) :: ZPOP_MODULATION  ! modulation of CO2 flux due to metabolism as a function of month, day and hour
REAL, DIMENSION(KI) :: ZREF_SW_HVEG        ! total solar rad reflected from high veg
REAL, DIMENSION(KI) :: ZAVG_Z0_TOWN
REAL, DIMENSION(KI) :: ZAVG_RESA_TOWN
REAL, DIMENSION(KI) :: ZAVG_USTAR        ! town avegared Ustar
REAL, DIMENSION(KI) :: ZAVG_BLD          ! town averaged building fraction
REAL, DIMENSION(KI) :: ZAVG_BLD_HEIGHT   ! town averaged building height
REAL, DIMENSION(KI) :: ZAVG_WL_O_HOR     ! town averaged Wall/hor ratio
REAL, DIMENSION(KI) :: ZAVG_CAN_HW_RATIO ! town averaged road aspect ratio
REAL, DIMENSION(KI) :: ZAVG_TAU_SR
REAL, DIMENSION(KI) :: ZAVG_H
REAL, DIMENSION(KI) :: ZAVG_LE
REAL, DIMENSION(KI) :: ZAVG_RN
REAL, DIMENSION(KI) :: ZAVG_GFLX
REAL, DIMENSION(KI) :: ZAVG_QF
REAL, DIMENSION(KI) :: ZAVG_REF_SW_GRND
REAL, DIMENSION(KI) :: ZAVG_REF_SW_FAC
REAL, DIMENSION(KI) :: ZAVG_REF_SW_HVEG
REAL, DIMENSION(KI) :: ZSCA_SW_GROUND_DOWN
REAL, DIMENSION(KI) :: ZSCA_SW_GROUND_UP
REAL, DIMENSION(KI) :: ZSCA_SW_GROUND_HOR
REAL, DIMENSION(KI) :: ZLW_GROUND_DOWN
REAL, DIMENSION(KI) :: ZLW_GROUND_HOR
REAL, DIMENSION(KI) :: ZAVG_SCA_SW_GROUND_DOWN
REAL, DIMENSION(KI) :: ZAVG_SCA_SW_GROUND_UP
REAL, DIMENSION(KI) :: ZAVG_SCA_SW_GROUND_HOR
REAL, DIMENSION(KI) :: ZAVG_LW_GROUND_DOWN
REAL, DIMENSION(KI) :: ZAVG_LW_GROUND_HOR
REAL, DIMENSION(KI) :: ZAVG_EMIT_LW_FAC
REAL, DIMENSION(KI) :: ZAVG_EMIT_LW_GRND
REAL, DIMENSION(KI) :: ZAVG_EMIT_LW_HVEG
REAL, DIMENSION(KI) :: ZAVG_LW_RAD_SKY
REAL, DIMENSION(KI) :: ZAVG_SCA_SW_SKY
REAL, DIMENSION(KI,BOP%NBEMCOMP) :: ZAVG_T_RAD_IND
REAL, DIMENSION(KI)  :: ZAVG_URBTREE
REAL, DIMENSION(:,:), ALLOCATABLE :: ZAVG_LAD_CAN
REAL, DIMENSION(KI) :: ZAVG_ROAD_SHADE
REAL, DIMENSION(KI) :: ZU_UTCI ! wind speed for the UTCI calculation (m/s)
REAL, DIMENSION(KI) :: ZT_UTCI ! temperature for the UTCI calculation (m/s)
REAL, DIMENSION(KI) :: ZQ_UTCI ! specific humidity for the UTCI calculation (m/s)
REAL, DIMENSION(KI) :: ZALFAU   ! V+(1) = alfa u'w'(1) + beta
REAL, DIMENSION(KI) :: ZBETAU   ! V+(1) = alfa u'w'(1) + beta
REAL, DIMENSION(KI) :: ZALFAT   ! Th+(1) = alfa w'th'(1) + beta
REAL, DIMENSION(KI) :: ZBETAT   ! Th+(1) = alfa w'th'(1) + beta
REAL, DIMENSION(KI) :: ZALFAQ   ! Q+(1) = alfa w'q'(1) + beta
REAL, DIMENSION(KI) :: ZBETAQ   ! Q+(1) = alfa w'q'(1) + beta
!***** CANOPY  *****
REAL, DIMENSION(KI) :: ZWAKE      ! reduction of average wind speed
!                                              ! in canyon due to direction average.
REAL, DIMENSION(KI) :: ZSFLUX_U  ! Surface flux u'w' (m2/s2)
REAL, DIMENSION(KI) :: ZSFLUX_T  ! Surface flux w'T' (mK/s)
REAL, DIMENSION(KI) :: ZSFLUX_Q  ! Surface flux w'q' (kgm2/s)
REAL, DIMENSION(KI,SB%NLVL)   :: ZFORC_U   ! tendency due to drag force for wind
REAL, DIMENSION(KI,SB%NLVL)   :: ZDFORC_UDU! formal derivative of
!                                              ! tendency due to drag force for wind
REAL, DIMENSION(KI,SB%NLVL)   :: ZFORC_E   ! tendency due to drag force for TKE
REAL, DIMENSION(KI,SB%NLVL)   :: ZDFORC_EDE! formal derivative of
!                                              ! tendency due to drag force for TKE
REAL, DIMENSION(KI,SB%NLVL)   :: ZFORC_T   ! tendency due to drag force for Temp
REAL, DIMENSION(KI,SB%NLVL)   :: ZDFORC_TDT! formal derivative of
!                                              ! tendency due to drag force for Temp
REAL, DIMENSION(KI,SB%NLVL)   :: ZFORC_Q   ! tendency due to drag force for hum
REAL, DIMENSION(KI,SB%NLVL)   :: ZDFORC_QDQ! formal derivative of
!                                           ! tendency due to drag force for hum.
REAL, DIMENSION(KI) :: ZLMO       ! Monin-Obukhov length at canopy height (m)
REAL, DIMENSION(KI,SB%NLVL)   :: ZL         ! Mixing length generic profile at mid levels
!
REAL, DIMENSION(KI) :: ZCOEF
REAL, DIMENSION(KI) :: ZAVG_USTAR_ROOF
REAL, DIMENSION(KI) :: ZNET_UP_DOWN
!
REAL :: ZCONVERTFACM0_SLT, ZCONVERTFACM0_DST
REAL :: ZCONVERTFACM3_SLT, ZCONVERTFACM3_DST
REAL :: ZCONVERTFACM6_SLT, ZCONVERTFACM6_DST
!
INTEGER :: JI
INTEGER :: JLAYER
INTEGER :: JCOMP
INTEGER :: JJ
!
INTEGER :: ICHECK
REAL :: ZWEIGHT
!
TYPE(CHECK_TEB_t)                 :: CT
!
! number of TEB patches
!
INTEGER                    :: JP, IBEG, IEND ! loop counter
INTEGER                    :: ILUOUT     ! Unit number
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
! Preliminaries:
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('COUPLING_TEB_N',0,ZHOOK_HANDLE)
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('COUPLING_TEBN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
! Set very low values of snow and rain rate to 0.0
!
WHERE(PSN(:).LT.1.0e-9) PSN(:)=0.0
WHERE(PRAIN(:).LT.1.0e-9) PRAIN(:)=0.0
!
!-------------------------------------------------------------------------------------
!
CT%LCHECK_TEB = TOP%LCHECK_TEB
CT%XCHECK_PROCESS = TOP%XEPS_BDGT_FAC
CT%XCHECK_ALL     = TOP%XEPS_BDGT_GLOB
!
IF (CT%LCHECK_TEB) CALL ALLOC_CHECK_TEB(CT, KI, BOP%NBEMCOMP)
!
! scalar fluxes
!
PSFTS(:,:) = 0.
!
! broadband radiative fluxes
!
ZDIR_SW(:) = 0.
ZSCA_SW(:) = 0.
!
DO JSWB=1,KSW
  !add directionnal contrib from scattered radiation
  !
  ZDIR_SW(:) = ZDIR_SW(:) + PDIR_SW(:,JSWB)
  ZSCA_SW(:) = ZSCA_SW(:) + PSCA_SW(:,JSWB)
  !
ENDDO
!
! specific humidity (conversion from kg/m3 to kg/kg)
!
ZQA(:) = PQA(:,1) / PRHOA(:,1)
!
! wind speed
!
ZWIND(:,:) = SQRT(PU(:,:)**2+PV(:,:)**2)
!
! method of wind coupling
!
IF (HCOUPLING=='I') THEN
  ZPEW_A_COEF = PPEW_A_COEF
  ZPEW_B_COEF = PPEW_B_COEF
ELSE
  ZPEW_A_COEF =  0.
  ZPEW_B_COEF =  ZWIND(:,1)
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Time evolution
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
DO JP=1,TOP%NTEB_PATCH
   CALL TRAFFIC_FLUX_MODULATION (TOP, PTSUN, &
          NT%AL(JP)%XDELTA_LEGAL_TIME,NT%AL(JP)%NDELTA_LEGAL_TIME, &
          NT%AL(JP)%XTIME_OF_CHANGE, NT%AL(JP)%LTIME_OF_CHANGE,    &
          G%XLON, HPROGRAM,ZTRAF_MODULATION,ZPOP_MODULATION)
   !
   ZH_TRAFFIC(:)  = NT%AL(JP)%XH_TRAFFIC  * ZTRAF_MODULATION
   ZLE_TRAFFIC(:) = NT%AL(JP)%XLE_TRAFFIC * ZTRAF_MODULATION
   !
END DO
!,' (K)'
TOP%TTIME%TIME = TOP%TTIME%TIME + PTSTEP
 CALL ADD_FORECAST_TO_DATE_SURF(TOP%TTIME%TDATE%YEAR, TOP%TTIME%TDATE%MONTH,&
                                TOP%TTIME%TDATE%DAY, TOP%TTIME%TIME)
!
!--------------------------------------------------------------------------------------
!  Canyon forcing for TEB
!--------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
! Town averaged quantities to force canopy atmospheric layers
!-------------------------------------------------------------------------------------
!
DO JP=1,TOP%NTEB_PATCH
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_BLD,         NT%AL(JP)%XBLD         )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_BLD_HEIGHT,  NT%AL(JP)%XBLD_HEIGHT  )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_WL_O_HOR,    NT%AL(JP)%XWALL_O_HOR  )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_CAN_HW_RATIO,NT%AL(JP)%XCAN_HW_RATIO)
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_Z0_TOWN     ,NT%AL(JP)%XZ0_TOWN     )
END DO
!
! Allocate local canopy variables
!
ALLOCATE(ZAVG_DH_HVEG(KI,SB%NLVL))
ALLOCATE(ZAVG_DE_HVEG(KI,SB%NLVL))
ALLOCATE(ZLAD_CAN(KI,SB%NLVL))
ALLOCATE(ZDH_HVEG(KI,SB%NLVL))
ALLOCATE(ZDLE_HVEG(KI,SB%NLVL))
ALLOCATE(ZAVG_LAD_CAN(KI,SB%NLVL))
!
IF (TOP%LCANOPY) THEN
  !
  !
  !-------------------------------------------------------------------------------------
  ! Updates canopy vertical grid as a function of forcing height
  ! and coupling (single level or multi-level) 
  !-------------------------------------------------------------------------------------
  !
  IF (TOP%LATM_CANOPY) THEN
    !
    ! Check whether lowest forcing level not too low
    ! compared to roughness lengths assumed in the TEB routines
    !
    IF ((MINVAL(PZREF).LT.0.1).OR.(MINVAL(PUREF).LT.0.1)) THEN
       CALL ABOR1_SFX("COUPLING_TEBN: Too low value for reference height")
    ENDIF
    !
    ! The number of canopy levels is set to the number of levels from the atmospheric model
    !
    SB%NLVL = KLEV
    !
    ! On first time step: change size of TEB canopy variables 
    ! to match with atmospheric model grid.
    !
    IF (SIZE(SB%XZ,2).NE.KLEV) THEN
      !
      DEALLOCATE(SB%XZ)
      DEALLOCATE(SB%XZF)
      DEALLOCATE(SB%XDZ)
      DEALLOCATE(SB%XT)
      DEALLOCATE(SB%XQ)
      DEALLOCATE(SB%XP)
      DEALLOCATE(SB%XU)
      DEALLOCATE(SB%XTKE)
      DEALLOCATE(SB%XLMO)
      DEALLOCATE(SB%XLM)
      DEALLOCATE(SB%XLEPS)
      DEALLOCATE(SB%XU_MEAN)
      DEALLOCATE(SB%XT_MEAN)
      DEALLOCATE(SB%XQ_MEAN)
      DEALLOCATE(SB%XRH_MEAN)
      DEALLOCATE(SB%XP_MEAN)
      !
      ALLOCATE(SB%XZ(KI,KLEV))
      ALLOCATE(SB%XZF(KI,KLEV))
      ALLOCATE(SB%XDZ(KI,KLEV))
      ALLOCATE(SB%XT(KI,KLEV))
      ALLOCATE(SB%XQ(KI,KLEV))
      ALLOCATE(SB%XP(KI,KLEV))
      ALLOCATE(SB%XU(KI,KLEV))
      ALLOCATE(SB%XTKE(KI,KLEV))
      ALLOCATE(SB%XLMO(KI,KLEV))
      ALLOCATE(SB%XLM(KI,KLEV))
      ALLOCATE(SB%XLEPS(KI,KLEV))
      ALLOCATE(SB%XU_MEAN(KI,KLEV))
      ALLOCATE(SB%XT_MEAN(KI,KLEV))
      ALLOCATE(SB%XQ_MEAN(KI,KLEV))
      ALLOCATE(SB%XRH_MEAN(KI,KLEV))
      ALLOCATE(SB%XP_MEAN(KI,KLEV))
      !
      DEALLOCATE(ZAVG_DH_HVEG)
      DEALLOCATE(ZAVG_DE_HVEG)
      DEALLOCATE(ZLAD_CAN)
      DEALLOCATE(ZDH_HVEG)
      DEALLOCATE(ZDLE_HVEG)
      DEALLOCATE(ZAVG_LAD_CAN)
      !
      ALLOCATE(ZAVG_DH_HVEG(KI,KLEV))
      ALLOCATE(ZAVG_DE_HVEG(KI,KLEV))
      ALLOCATE(ZLAD_CAN(KI,KLEV))
      ALLOCATE(ZDH_HVEG(KI,KLEV))
      ALLOCATE(ZDLE_HVEG(KI,KLEV))
      ALLOCATE(ZAVG_LAD_CAN(KI,KLEV))
      !
      ! The variables not used with this option are initialised with the undefined value
      !
      SB%XLMO(:,:)  = XUNDEF
      SB%XLM(:,:)   = XUNDEF
      SB%XLEPS(:,:) = XUNDEF
      !
    ENDIF
    !
    ! The height of the middle of the canopy levels equals the 
    ! scalar level heights from the atmospheric model
    !
    SB%XZ(:,:) = PZREF(:,:)
    !
    ! The height of the bottom and top of the canopy levels 
    !
    SB%XZF(:,:) = XUNDEF
    SB%XZF(:,1) = 0.
    !
    DO JLAYER=2,KLEV
      SB%XZF(:,JLAYER) = 2.*SB%XZ(:,JLAYER-1) - SB%XZF(:,JLAYER-1)
    ENDDO
    !
    ! Calculate the layer depths (variable located at full levels)
    !
    SB%XDZ(:,:) = -XUNDEF
    DO JLAYER=1,SB%NLVL-1
       SB%XDZ(:,JLAYER) = SB%XZF(:,JLAYER+1) - SB%XZF(:,JLAYER)
    ENDDO
    !
    ! The prognostic canopy variables are set equal to the atmospheric variables
    !
    SB%XT(:,:) = PTA(:,:)
    SB%XQ(:,:) = PQA(:,:)
    SB%XP(:,:) = PPA(:,:)
    SB%XU(:,:) = ZWIND(:,:)
    SB%XTKE(:,:) = PTKE(:,:)
    !
    ! For the variables close to the surface, the first atmospheric level is taken
    !
    ZZ_LOWCAN(:) = PZREF(:,1)
    ZU_LOWCAN(:) = ZWIND(:,1)
    ZT_LOWCAN(:) = PTA(:,1)
    ZQ_LOWCAN(:) = PQA(:,1)/PRHOA(:,1)
    ZPEW_A_COEF_LOWCAN(:) = 0.0
    ZPEW_B_COEF_LOWCAN(:) = ZU_LOWCAN(:)
    !
    ! For the variables above the roof, the corresponding 
    ! atmospheric values are assigned.
    ! However, the level must be at least 0.5 m higher than the roof, since
    ! in urban drag a roof roughness length of 0.15 m is hardcoded
    ! Otherwise the next higher level is taken.
    !
    DO JJ=1,KI
      !
      ICHECK=0
      !
      DO JLAYER=1,(SB%NLVL-1)
         !
         IF ( (SB%XZ(JJ,JLAYER  ).LE.ZAVG_BLD_HEIGHT(JJ)) .AND. &
              (SB%XZ(JJ,JLAYER+1).GT.ZAVG_BLD_HEIGHT(JJ)) ) THEN
            !
            ICHECK=ICHECK+1
            !
            IF ( (SB%XZ(JJ,JLAYER+1) - ZAVG_BLD_HEIGHT(JJ) ) .GT. 0.5 ) THEN
               !
               ZUREF(JJ) = SB%XZ(JJ,JLAYER+1) - ZAVG_BLD_HEIGHT(JJ)
               ZZREF(JJ) = SB%XZ(JJ,JLAYER+1) - ZAVG_BLD_HEIGHT(JJ)
               ZTA(JJ)   = SB%XT(JJ,JLAYER+1)
               ZQA(JJ)   = SB%XQ(JJ,JLAYER+1)/PRHOA(JJ,JLAYER+1)
               ZPA(JJ)   = SB%XP(JJ,JLAYER+1)
               ZUA(JJ)   = SB%XU(JJ,JLAYER+1)
               !
            ELSE 
               !
               ZUREF(JJ) = SB%XZ(JJ,JLAYER+2) - ZAVG_BLD_HEIGHT(JJ)
               ZZREF(JJ) = SB%XZ(JJ,JLAYER+2) - ZAVG_BLD_HEIGHT(JJ)
               ZTA(JJ)   = SB%XT(JJ,JLAYER+2)
               ZQA(JJ)   = SB%XQ(JJ,JLAYER+2)/PRHOA(JJ,JLAYER+2)
               ZPA(JJ)   = SB%XP(JJ,JLAYER+2)
               ZUA(JJ)   = SB%XU(JJ,JLAYER+2)
               !
            ENDIF
            !
         ENDIF
         !
      ENDDO
      !
      IF (ICHECK.NE.1) THEN
         CALL ABOR1_SFX("COUPLING_TEBN: Roof level could not be attributed")
      ENDIF
      !
    ENDDO
    !
    ! For the canyon variables, a weighted average over all 
    ! atmospheric levels intersecting the buildings is calculated
    ! The in-canyon variability of temperature, humidity and wind speed
    ! is therefore neglected, which leads to uncertainties due to
    ! the non-linearity of the exchange coeffients.
    !
    DO JJ=1,KI
       !
       ICHECK  = 0
       ZWEIGHT = 0
       !
       ZU_CANYON(JJ) = 0.0 
       ZT_CANYON(JJ) = 0.0
       ZQ_CANYON(JJ) = 0.0
       !
       DO JLAYER=1,(SB%NLVL-1)
          !
          IF ( (SB%XZ(JJ,JLAYER  ) .LE. ZAVG_BLD_HEIGHT(JJ)) .AND. &
               (SB%XZ(JJ,JLAYER+1) .LT. ZAVG_BLD_HEIGHT(JJ)) ) THEN
             !
             ZWEIGHT = ZWEIGHT + (SB%XZF(JJ,JLAYER+1)-SB%XZF(JJ,JLAYER))
             !
             ZU_CANYON(JJ) = ZU_CANYON(JJ) + SB%XU(JJ,JLAYER) * (SB%XZF(JJ,JLAYER+1)-SB%XZF(JJ,JLAYER))
             ZT_CANYON(JJ) = ZT_CANYON(JJ) + SB%XT(JJ,JLAYER) * (SB%XZF(JJ,JLAYER+1)-SB%XZF(JJ,JLAYER))
             ZQ_CANYON(JJ) = ZQ_CANYON(JJ) + (SB%XQ(JJ,JLAYER) / PRHOA(JJ,JLAYER)) * (SB%XZF(JJ,JLAYER+1)-SB%XZF(JJ,JLAYER))
             !
          ELSE IF ( (SB%XZ(JJ,JLAYER  ) .LE. ZAVG_BLD_HEIGHT(JJ)) .AND. &
                    (SB%XZ(JJ,JLAYER+1) .GT. ZAVG_BLD_HEIGHT(JJ)) ) THEN
             !
             ZWEIGHT = ZWEIGHT + (ZAVG_BLD_HEIGHT(JJ)-SB%XZF(JJ,JLAYER))
             !
             ZU_CANYON(JJ) = ZU_CANYON(JJ) + SB%XU(JJ,JLAYER) * (ZAVG_BLD_HEIGHT(JJ)-SB%XZF(JJ,JLAYER))
             ZT_CANYON(JJ) = ZT_CANYON(JJ) + SB%XT(JJ,JLAYER) * (ZAVG_BLD_HEIGHT(JJ)-SB%XZF(JJ,JLAYER))
             ZQ_CANYON(JJ) = ZQ_CANYON(JJ) + (SB%XQ(JJ,JLAYER) / PRHOA(JJ,JLAYER)) * (ZAVG_BLD_HEIGHT(JJ)-SB%XZF(JJ,JLAYER))
             !
             ICHECK=ICHECK+1
             !
          ENDIF
          !
       ENDDO
       !
       IF (ICHECK.NE.1) THEN
          CALL ABOR1_SFX ("COUPLING_TEBN: Roof level could not be attributed")
       ENDIF
       !
       IF (ABS(ZWEIGHT-ZAVG_BLD_HEIGHT(JJ)).GT.1.0E-6) THEN
          CALL ABOR1_SFX ("COUPLING_TEBN: Wrong weights for canyon levels")
       ENDIF
       !
       ZU_CANYON(JJ) = ZU_CANYON(JJ) / ZWEIGHT
       ZT_CANYON(JJ) = ZT_CANYON(JJ) / ZWEIGHT
       ZQ_CANYON(JJ) = ZQ_CANYON(JJ) / ZWEIGHT
       !
    ENDDO
    !
  ELSE
  !
  ! Make sure this part is not used with multi level forcing
  !
  IF (KLEV.NE.1) THEN
    CALL ABOR1_SFX("COUPLING_TEBN: TEB canopy only available with single level coupling")
  ENDIF
  !
  !* determines where is the forcing level and modifies the upper levels of the canopy grid
  !
  CALL CANOPY_GRID_UPDATE(KI, ZAVG_BLD_HEIGHT, ZAVG_BLD_HEIGHT+PUREF(:,1), SB)
  !
  !* Initialisations of T, Q, TKE and wind at first time step
  !
  IF(ANY(SB%XT(:,:) == XUNDEF)) THEN
    DO JLAYER=1,SB%NLVL
      SB%XT(:,JLAYER) = PTA(:,1)
      SB%XQ(:,JLAYER) = PQA(:,1)
      SB%XU(:,JLAYER) = 2./XPI * ZWIND(:,1)                                  &
              * LOG( (          2.* NT%AL(1)%XBLD_HEIGHT(:)/3.) / NT%AL(1)%XZ0_TOWN(:))   &
              / LOG( (PUREF(:,1)+ 2.* NT%AL(1)%XBLD_HEIGHT(:)/3.) / NT%AL(1)%XZ0_TOWN(:))
    END  DO
    SB%XTKE(:,:) = 1.
  ENDIF
  !
  !* default forcing above roof: forcing level
  ZUREF(:) = PUREF(:,1)
  ZZREF(:) = PZREF(:,1)
  ZUA(:)   = SB%XU(:,SB%NLVL)
  ZTA(:)   = SB%XT(:,SB%NLVL)
  ZQA(:)   = SB%XQ(:,SB%NLVL)/PRHOA(:,1)
  ZPA(:)   = SB%XP(:,SB%NLVL)
  !* for the time being, only one value is kept for wall in-canyon forcing, in the middle of the canyon
  ZU_CANYON(:) = ZUA(:)
  ZT_CANYON(:) = ZTA(:)
  ZQ_CANYON(:) = ZQA(:)
  DO JLAYER=1,SB%NLVL-1
    DO JI=1,KI
      !* finds middle canyon layer
      IF (SB%XZ(JI,JLAYER)<ZAVG_BLD_HEIGHT(JI)/2. .AND. SB%XZ(JI,JLAYER+1)>=ZAVG_BLD_HEIGHT(JI)/2.) THEN
        ZCOEF(JI) = (ZAVG_BLD_HEIGHT(JI)/2.-SB%XZ(JI,JLAYER))/(SB%XZ(JI,JLAYER+1)-SB%XZ(JI,JLAYER))
        ZU_CANYON(JI) = SB%XU(JI,JLAYER) + ZCOEF(JI) * (SB%XU(JI,JLAYER+1)-SB%XU(JI,JLAYER))
        ZT_CANYON(JI) = SB%XT(JI,JLAYER) + ZCOEF(JI) * (SB%XT(JI,JLAYER+1)-SB%XT(JI,JLAYER))
        ZQ_CANYON(JI) =(SB%XQ(JI,JLAYER) + ZCOEF(JI) * (SB%XQ(JI,JLAYER+1)-SB%XQ(JI,JLAYER)))/PRHOA(JI,1)
      END IF
      !* finds layer just above roof (at least 1m above roof)
      IF (SB%XZ(JI,JLAYER)<ZAVG_BLD_HEIGHT(JI)+1. .AND. SB%XZ(JI,JLAYER+1)>=ZAVG_BLD_HEIGHT(JI)+1.) THEN
        ZUREF(JI) = SB%XZ(JI,JLAYER+1) - ZAVG_BLD_HEIGHT(JI)
        ZZREF(JI) = SB%XZ(JI,JLAYER+1) - ZAVG_BLD_HEIGHT(JI)
        ZTA  (JI) = SB%XT(JI,JLAYER+1)
        ZQA  (JI) = SB%XQ(JI,JLAYER+1)/PRHOA(JI,1)
        ZUA  (JI) = MAX(SB%XU(JI,JLAYER+1) - 2.*SQRT(SB%XTKE(JI,JLAYER+1)) , SB%XU(JI,JLAYER+1)/3.)
        ZPA  (JI) = SB%XP(JI,JLAYER+1)
        ZLMO (JI) = SB%XLMO(JI,JLAYER+1)
      END IF
    END DO
  END DO
  !
  ZU_CANYON= MAX(ZU_CANYON,0.2)
  ZU_LOWCAN=SB%XU(:,1)
  ZT_LOWCAN=SB%XT(:,1)
  ZQ_LOWCAN=SB%XQ(:,1) / PRHOA(:,1)
  ZZ_LOWCAN=SB%XZ(:,1)
  WHERE(ZPA==XUNDEF) ZPA = PPA(:,1)   ! security for first time step
  !
  !-------------------------------------------------------------------------------------
  ! determine the vertical profile for mixing and dissipative lengths (at full levels)
  !-------------------------------------------------------------------------------------
  !
  IF (TOP%CURB_LM.EQ.'SM10') THEN
     !
     ! Computation of the urban mixing length following Santiago and Martilli (2010)
     !
     CALL SM10(SB%XZ, ZAVG_BLD_HEIGHT, ZAVG_BLD, ZL)
     !
  ELSE IF (TOP%CURB_LM.EQ.'LMEZ') THEN
     !
     ! The urban mixing length equals to the height above ground
     !
     ZL(:,:) = SB%XZ(:,:)
     !
  ELSE
     CALL ABOR1_SFX("COUPLING_TEBN: No rule for computation of urban mixing length")
  ENDIF
  !
  !-------------------------------------------------------------------------------------
  ! computes coefficients for implicitation
  !-------------------------------------------------------------------------------------
  !
  ZAVG_UW_GRND(:)    = 0.
  ZAVG_DUWDU_GRND(:) = 0.
  ZAVG_UW_RF(:)      = 0.
  ZAVG_DUWDU_RF(:)   = 0.
  ZAVG_H_WL(:)       = 0.
  ZAVG_H_RF(:)       = 0.
  ZAVG_E_WL(:)       = 0.
  ZAVG_E_RF(:)       = 0.
  ZAVG_DH_HVEG(:,:)  = 0.
  ZAVG_DE_HVEG(:,:)  = 0.
  ZAVG_URBTREE(:)    = 0.
  ZAVG_LAD_CAN(:,:)  = 0.
  ZAVG_AC_GRND(:)    = 0.
  ZAVG_AC_GRND_WAT(:)= 0.
  ZSFLUX_U(:)        = 0.
  ZSFLUX_T(:)        = 0.
  ZSFLUX_Q(:)        = 0.
  !
  DO JLAYER=1,SB%NLVL-1
     !* Monin-Obuhkov theory not used inside the urban canopy
     ! => neutral mixing  if layer is below : (roof level +1 meter)
     WHERE (SB%XZ(:,JLAYER)<=ZAVG_BLD_HEIGHT(:)+1.) SB%XLMO(:,JLAYER) = XUNDEF
  ENDDO
  !
  !* computes tendencies on wind and Tke due to canopy
  CALL TEB_CANOPY(KI,SB, ZAVG_BLD,ZAVG_BLD_HEIGHT,ZAVG_WL_O_HOR, PPA(:,1), PRHOA(:,1), &
                 ZAVG_DUWDU_GRND, ZAVG_UW_RF, ZAVG_DUWDU_RF, ZAVG_H_WL, ZAVG_E_WL, &
                 ZAVG_H_RF, ZAVG_E_RF, ZAVG_DH_HVEG, ZAVG_DE_HVEG,                  &
                 ZAVG_AC_GRND,ZAVG_AC_GRND_WAT,                                            &
                 ZAVG_URBTREE,ZAVG_LAD_CAN, ZFORC_U,                         &
                 ZDFORC_UDU, ZFORC_E, ZDFORC_EDE, ZFORC_T, ZDFORC_TDT, ZFORC_Q, &
                 ZDFORC_QDQ )
  !
  !* computes coefficients for implicitation
  CALL CANOPY_EVOL(SB, KI, PTSTEP, 1, ZL, ZWIND(:,1), PTA(:,1), PQA(:,1), PPA(:,1), PRHOA(:,1), &
                   ZSFLUX_U, ZSFLUX_T, ZSFLUX_Q, ZFORC_U, ZDFORC_UDU,   &
                   ZFORC_E, ZDFORC_EDE, ZFORC_T, ZDFORC_TDT, ZFORC_Q,   &
                   ZDFORC_QDQ, SB%XLM, SB%XLEPS, ZAVG_USTAR, ZALFAU,  &
                   ZBETAU, ZALFAT, ZBETAT, ZALFAQ, ZBETAQ)
  !
  ZPEW_A_COEF_LOWCAN = - ZALFAU / PRHOA(:,1)
  ZPEW_B_COEF_LOWCAN = ZBETAU
  !
  ENDIF  ! Related to multi-level coupling (LATM_CANOPY)
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
ELSE              ! no canopy case
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  DO JI=1,KI
    !* skimming flow for h/w>1 (maximum effect of direction on wind in the canyon);
    !* isolated flow for h/w<0.5 (wind is the same in large streets for all dir.)
    !* wake flow between.
    !
    ZWAKE(JI)= 1. + (2./XPI-1.) * 2. * (ZAVG_CAN_HW_RATIO(JI)-0.5)
    ZWAKE(JI)= MAX(MIN(ZWAKE(JI),1.),2./XPI)
    !
    !* Estimation of canyon wind speed from wind just above roof level
    !  (at 1.33h). Wind at 1.33h is estimated using the log law.
    !
    IF (ZAVG_BLD_HEIGHT(JI) .GT. 0.) THEN
      ZU_CANYON(JI) = ZWAKE(JI) * EXP(-ZAVG_CAN_HW_RATIO(JI)/4.) * ZWIND(JI,1)     &
                * LOG( (           2.* ZAVG_BLD_HEIGHT(JI)/3.) / ZAVG_Z0_TOWN(JI))   &
                / LOG( (PUREF(JI,1)+ 2.* ZAVG_BLD_HEIGHT(JI)/3.) / ZAVG_Z0_TOWN(JI))
      ZZ_LOWCAN(JI) = ZAVG_BLD_HEIGHT(JI) / 2.
    ELSE
      ZU_CANYON(JI) = ZWIND(JI,1)
      ZZ_LOWCAN(JI) = PZREF(JI,1)
    ENDIF
  END DO
  !
  !* Without SBL scheme, canyon air is assumed at mid height
  !
  ! Check for negative humidity
  !
  IF (MINVAL(NT%AL(1)%XQ_CANYON).LT.-XSURF_EPSILON) THEN
      CALL GET_LUOUT(HPROGRAM,ILUOUT)
      WRITE(ILUOUT,*) "NT%AL(1)%Q_CANYON : ",NT%AL(1)%XQ_CANYON
      CALL FLUSH(ILUOUT)
      CALL ABOR1_SFX("Negative humidity in canyon")
  ENDIF
  !
  !* Without SBL scheme, canyon air is assumed at mid height
  ZU_LOWCAN = ZU_CANYON
  ZT_LOWCAN = NT%AL(1)%XT_CANYON
  ZQ_LOWCAN = NT%AL(1)%XQ_CANYON
  ZT_CANYON = NT%AL(1)%XT_CANYON
  ZQ_CANYON = NT%AL(1)%XQ_CANYON
  ZUREF     = PUREF(:,1)
  ZZREF     = PZREF(:,1)
  ZTA       = PTA(:,1)
  ZUA       = ZWIND(:,1)
  ZPA       = PPA(:,1)
  ZPEW_A_COEF_LOWCAN =  0.
  ZPEW_B_COEF_LOWCAN =  ZU_CANYON
END IF
!
! Exner functions
!
ZEXNS(:) = (PPS(:)/XP00)**(XRD/XCPD)
ZEXNA(:) = (ZPA(:)/XP00)**(XRD/XCPD)
!
!--------------------------------------------------------------------------------------
! Over Urban surfaces/towns:
!--------------------------------------------------------------------------------------
!
!--------------------------------------------------------------------------------------
! LOOP on TEB PATCHES
!--------------------------------------------------------------------------------------
DO JP = 1,TOP%NTEB_PATCH
  !
  ZT_CAN = ZT_CANYON
  ZQ_CAN = ZQ_CANYON
  !
  IF (TOP%LCANOPY) THEN
     NT%AL(JP)%XT_CANYON(:) = ZT_CANYON(:)
     NT%AL(JP)%XQ_CANYON(:) = ZQ_CANYON(:)
  END IF
  !
  ZLESN_RF(:) = 0.
  ZLESN_RD(:) = 0.
  TD%NDMT%AL(JP)%XG_GREENROOF_ROOF(:) = 0.
  !
  ! Compute Air temperature at tree level inside the canyon
  !
  IF (TOP%LGARDEN .AND. TOP%CURBTREE/='NONE') THEN
     ! air temperature
     IF (TOP%LCANOPY) THEN
        DO JI=1,SIZE(GDM%PHV%XH_LAI_MAX)
           ZCTL=0
           DO JLAYER=1,SB%NLVL-1
              !* finds middle of tree crown
              !+marine condition si XH_LAI_MAX < XZ(1)
              IF (GDM%PHV%XH_LAI_MAX(JI) < SB%XZ(JI,1)) THEN
                 ZCTL=1
                 ZTA_HVEG(JI) = SB%XT(JI,1)
                 ZQA_HVEG(JI) = SB%XQ(JI,1)
              ENDIF
              !- marine
              IF (SB%XZ(JI,JLAYER)  < GDM%PHV%XH_LAI_MAX(JI) .AND. & 
                  SB%XZ(JI,JLAYER+1)>=GDM%PHV%XH_LAI_MAX(JI)) THEN
                 ZCTL=1
                 ZCOEF(JI)    = (GDM%PHV%XH_LAI_MAX(JI)-SB%XZ(JI,JLAYER))/ (SB%XZ(JI,JLAYER+1)-SB%XZ(JI,JLAYER))
                 ZTA_HVEG(JI) = SB%XT(JI,JLAYER) + ZCOEF(JI)*(SB%XT(JI,JLAYER+1)-SB%XT(JI,JLAYER))
                 ZQA_HVEG(JI) = SB%XQ(JI,JLAYER) + ZCOEF(JI)*(SB%XQ(JI,JLAYER+1)-SB%XQ(JI,JLAYER))
              ENDIF
           ENDDO
           IF (ZCTL.NE.1) THEN
              print*,' CHECK 2 GDM%PHV%XH_LAI_MAX(',JI,') = ',GDM%PHV%XH_LAI_MAX(JI)
              CALL ABOR1_SFX("COUPLING_TEBN: Tree forcing temperature not attributed")
           ENDIF
        ENDDO
     ELSE
        ZTA_HVEG = ZT_CAN
        ZQA_HVEG = ZQ_CAN
     ENDIF
     ! tree leaves temperature
     ZTS_HVEG(:) = GDM%NPEHV%AL(JP)%XTV(:)
  ELSE
     ZTA_HVEG    = XUNDEF
     ZQA_HVEG    = XUNDEF
     ZTS_HVEG(:) = XUNDEF
  ENDIF
  !
  ! Storage of soil water depths in urban soils
  !
  IF (TOP%LURBHYDRO .AND. CT%LCHECK_TEB) THEN
     CT%XWATER_ROAD  (:)=0.0
     CT%XWATER_BLD   (:)=0.0
     CT%XWATER_GARDEN(:)=0.0
     DO JLAYER=1,SIZE(NT%AL(JP)%XT_ROAD,2)
        CT%XWATER_ROAD(:)   = CT%XWATER_ROAD(:)   +  NT%AL(JP)%XROAD(:)     * &
                             NT%AL(JP)%XD_ROAD(:,JLAYER) * HM%NTH%AL(JP)%XWG_ROAD(:,JLAYER)
        CT%XWATER_BLD (:)   = CT%XWATER_BLD (:)   +  NT%AL(JP)%XBLD (:)     * &
                             NT%AL(JP)%XD_ROAD(:,JLAYER) * HM%NTH%AL(JP)%XWG_BLD (:,JLAYER)
        CT%XWATER_GARDEN(:) = CT%XWATER_GARDEN(:) +  NT%AL(JP)%XGARDEN(:)   * &
                             NT%AL(JP)%XD_ROAD(:,JLAYER) * GDM%NPE%AL(JP)%XWG(:,JLAYER)
     ENDDO
  ENDIF
  !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Call the physical routines of TEB (including gardens and greenroofs)
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
  IF (MINVAL(ZQ_CAN).LT.-XSURF_EPSILON) THEN
     CALL ABOR1_SFX("COUPLING_TEBN: Negative humidity in canyon")
  ENDIF
  !
  CALL TOWN_ENERGY_BALANCE(DTCO, G, TOP, SPAOP, NT%AL(JP), BOP, NB%AL(JP), TPN, TIR, TD%NDMT%AL(JP), GDM, GRM, &
       HM, SB, CT, JP, HPROGRAM, CIMPLICIT_WIND, PTSUN, ZT_CAN, ZQ_CAN, ZU_CANYON, ZT_LOWCAN,  &
       ZQ_LOWCAN, ZU_LOWCAN, ZZ_LOWCAN, ZTA_HVEG, ZQA_HVEG,                                    &
       ZPEW_A_COEF, ZPEW_B_COEF, ZPEW_A_COEF_LOWCAN,                                           &
       ZPEW_B_COEF_LOWCAN, AT, PPS, NB%AL(JP)%XPSOLD, ZPA, ZEXNS, ZEXNA, ZTA, ZQA, PRHOA(:,1),     &
       PCO2, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, KSW, PZENITH, PAZIM, PRAIN, PSN, ZZREF,         &
       ZUREF, ZUA, ZH_TRAFFIC, ZLE_TRAFFIC, PTSTEP, ZLEW_RF, ZLEW_RD, ZRNSN_RF, ZHSN_RF,       &
       ZLESN_RF, ZGSN_RF, ZMELT_RF, ZRNSN_RD, ZHSN_RD, ZLESN_RD, ZGSN_RD, ZMELT_RD, ZRN_GRND,  &
       ZH_GRND, ZLE_GRND, ZGFLX_GRND, ZRN, ZH, ZH_TOWN_SURF, ZH_TOWN_WALL, ZH_TOWN_ROOF, ZLE,  &
       ZGFLX, ZQF, ZEVAP, ZEVAP_TOWN_SURF, ZEVAP_TOWN_WALL,ZEVAP_TOWN_ROOF, ZUW_GRND, ZUW_RF,  &
       ZDUWDU_GRND, ZDUWDU_RF, ZUSTAR, ZCD, ZCDN, ZCH, ZRI, ZTRAD, ZEMIS, ZDIR_ALB, ZSCA_ALB,  &
       ZRESA_TOWN, ZAC_RD, ZAC_GD, ZAC_GRF, ZAC_RD_WAT, ZAC_GD_WAT, ZAC_GRF_WAT, KDAY, &
       ZEMIT_LW_HVEG, TD%NDMT%AL(JP)%XREF_SW_GRND, TD%NDMT%AL(JP)%XREF_SW_FAC, ZREF_SW_HVEG,   &
       PTIME, TD%NDMT%AL(JP)%XDN_ROOF,TD%NDMT%AL(JP)%XDN_ROAD, ZTS_HVEG,                       &
       TD%NDMT%AL(JP)%XTS_GD, TD%NDMT%AL(JP)%XTS_GR, ZLAD_CAN, ZTRAF_MODULATION,               &
       ZPOP_MODULATION, ZDH_HVEG, ZDLE_HVEG, ZSCA_SW_SKY, ZLW_RAD_SKY,                         &
       ZSCA_SW_GROUND_DOWN, ZSCA_SW_GROUND_UP, ZSCA_SW_GROUND_HOR, ZLW_GROUND_DOWN,            &
       ZLW_GROUND_HOR, "OK" )
  !
  TD%NDMT%AL(JP)%XU_LOWCAN=ZU_LOWCAN
  !
  IF (TOP%CBEM=='BEM') THEN
     !
     ! The internal heat release as well as the heating and cooling
     ! energy demand are converted from W/m²(bld) to W/m²(urb).
     !
     TD%NDMT%AL(JP)%XQINOUT(:)    = NT%AL(JP)%XBLD(:) * TD%NDMT%AL(JP)%XQINOUT(:)
     TD%NDMT%AL(JP)%XQINOUTSEN(:) = NT%AL(JP)%XBLD(:) * TD%NDMT%AL(JP)%XQINOUTSEN(:)
     TD%NDMT%AL(JP)%XQINOUTLAT(:) = NT%AL(JP)%XBLD(:) * TD%NDMT%AL(JP)%XQINOUTLAT(:)
     !
     TD%NDMT%AL(JP)%XHVAC_COOL(:) = NT%AL(JP)%XBLD(:) * TD%NDMT%AL(JP)%XHVAC_COOL(:)
     TD%NDMT%AL(JP)%XHVAC_HEAT(:) = NT%AL(JP)%XBLD(:) * TD%NDMT%AL(JP)%XHVAC_HEAT(:)
     !
     TD%NDMT%AL(JP)%XHVAC_HEAT_ELEC   = NT%AL(JP)%XBLD(:) * TD%NDMT%AL(JP)%XHVAC_HEAT_ELEC
     TD%NDMT%AL(JP)%XHVAC_HEAT_GAS    = NT%AL(JP)%XBLD(:) * TD%NDMT%AL(JP)%XHVAC_HEAT_GAS
     TD%NDMT%AL(JP)%XHVAC_HEAT_FUEL   = NT%AL(JP)%XBLD(:) * TD%NDMT%AL(JP)%XHVAC_HEAT_FUEL
     TD%NDMT%AL(JP)%XHVAC_HEAT_OTHER  = NT%AL(JP)%XBLD(:) * TD%NDMT%AL(JP)%XHVAC_HEAT_OTHER
     !
     DO JCOMP=1,BOP%NBEMCOMP
         TD%NDMT%AL(JP)%XCOMP_QINOUT   (:,JCOMP) = NT%AL(JP)%XBLD(:) * TD%NDMT%AL(JP)%XCOMP_QINOUT(:,JCOMP)
         TD%NDMT%AL(JP)%XCOMP_HVAC_COOL(:,JCOMP) = NT%AL(JP)%XBLD(:) * TD%NDMT%AL(JP)%XCOMP_HVAC_COOL(:,JCOMP)
         TD%NDMT%AL(JP)%XCOMP_HVAC_HEAT(:,JCOMP) = NT%AL(JP)%XBLD(:) * TD%NDMT%AL(JP)%XCOMP_HVAC_HEAT(:,JCOMP)
     ENDDO
     !
  ENDIF
  !
  IF (.NOT. TOP%LCANOPY) THEN
    !
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_T_CANYON,ZT_CAN)
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_Q_CANYON,ZQ_CAN)
    !
    ! Momentum fluxes
    !
    ZSFU = 0.
    ZSFV = 0.
    DO JJ=1,SIZE(PU,1)
      IF (ZWIND(JJ,1)>0.) THEN
        ZCOEF(JJ) = - PRHOA(JJ,1) * ZUSTAR(JJ)**2 / ZWIND(JJ,1)
        ZSFU(JJ) = ZCOEF(JJ) * PU(JJ,1)
        ZSFV(JJ) = ZCOEF(JJ) * PV(JJ,1)
      ENDIF
    ENDDO
    CALL ADD_PATCH_CONTRIB(JP,PSFU,ZSFU)
    CALL ADD_PATCH_CONTRIB(JP,PSFV,ZSFV)
    !
  ENDIF
  !
  IF (CT%LCHECK_TEB) THEN
     CT%XH = ZH
     CT%XLE= ZLE
     CT%XRN=ZRN
  END IF
  !-------------------------------------------------------------------------------------
  ! Outputs:
  !-------------------------------------------------------------------------------------
  !
  ! Grid box average fluxes/properties: Arguments and standard diagnostics
  !
  CALL ADD_PATCH_CONTRIB(JP,PSFTH,ZH)
  CALL ADD_PATCH_CONTRIB(JP,PSFTH_SURF,ZH_TOWN_SURF)
  CALL ADD_PATCH_CONTRIB(JP,PSFTH_WALL,ZH_TOWN_WALL)
  CALL ADD_PATCH_CONTRIB(JP,PSFTH_ROOF,ZH_TOWN_ROOF)  
  !
  CALL ADD_PATCH_CONTRIB(JP,PSFTQ,ZEVAP)
  CALL ADD_PATCH_CONTRIB(JP,PSFTQ_SURF,ZEVAP_TOWN_SURF)
  CALL ADD_PATCH_CONTRIB(JP,PSFTQ_WALL,ZEVAP_TOWN_WALL)
  CALL ADD_PATCH_CONTRIB(JP,PSFTQ_ROOF,ZEVAP_TOWN_ROOF)
  !
  CALL ADD_PATCH_CONTRIB(JP,PSFCO2,TD%NDMT%AL(JP)%XSFCO2)
  !
  ! Albedo for each wavelength and patch
  !
  DO JSWB=1,SIZE(PSW_BANDS)
     DO JJ=1,SIZE(ZDIR_ALB)
        ZDIR_ALB_PATCH(JJ,JSWB,JP) = ZDIR_ALB(JJ)
        ZSCA_ALB_PATCH(JJ,JSWB,JP) = ZSCA_ALB(JJ)
     ENDDO
  END DO
  !
  ! emissivity and radiative temperature
  !
  ZEMIS_PATCH(:,JP) = ZEMIS
  ZTRAD_PATCH(:,JP) = ZTRAD
  !
  ! computes some aggregated diagnostics
  !
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_CD ,ZCD )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_CDN,ZCDN)
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_RI ,ZRI )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_CH ,ZCH )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_RN ,ZRN )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_H  ,ZH  )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_LE ,ZLE )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_GFLX ,ZGFLX )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_QF ,ZQF )
  !
  !* warning: aerodynamical resistance does not yet take into account gardens
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_RESA_TOWN,1./ZRESA_TOWN)
  IF (JP==TOP%NTEB_PATCH) ZAVG_RESA_TOWN = 1./ZAVG_RESA_TOWN
  !
  !
  ! Use the modulated fields of traffic and industry releases for the output
  !
  TD%NDMT%AL(JP)%XH_TRAFFIC_OUT(:)   = ZH_TRAFFIC(:)
  TD%NDMT%AL(JP)%XLE_TRAFFIC_OUT(:)  = ZLE_TRAFFIC(:)
  TD%NDMT%AL(JP)%XH_INDUSTRY_OUT (:) = NT%AL(JP)%XH_INDUSTRY(:)
  TD%NDMT%AL(JP)%XLE_INDUSTRY_OUT(:) = NT%AL(JP)%XLE_INDUSTRY(:)
  !
  !
  ! ###############################################################
  ! ###############################################################
  ! Verification of energy conservation
  ! ###############################################################
  ! ###############################################################
  !
  IF (CT%LCHECK_TEB) CALL CHECK_TEB (TOP, BOP, NT, NB, TD, TPN, TIR, GDM, GRM, HM, CT, &
                                     HPROGRAM, KI, JP, PTSTEP, PTSUN, PRAIN, PSN       )
  

  ! Additionnal security to ensure no problem during spin-up
  ! Only possible when LCHECK_TEB=F

IF (.NOT.CT%LCHECK_TEB) THEN
WHERE(NT%AL(JP)%XT_ROOF .GT. XTT+100.)
NT%AL(JP)%XT_ROOF(:,:) = 99.9
END WHERE
  WHERE(NT%AL(JP)%XT_WALL_A .GT. XTT+100.)
NT%AL(JP)%XT_WALL_A(:,:) = 99.9
END WHERE
  WHERE(NT%AL(JP)%XT_ROAD .GT. XTT+100.)
NT%AL(JP)%XT_ROAD(:,:) = 99.9
END WHERE
END IF

  !
  !
  ! Check for realistic temperatures (only if LCHECK_TEB)
  !
  IF ((CT%LCHECK_TEB) .AND.  ANY(NT%AL(JP)%XT_ROOF .GT. XTT+100.) .OR. ANY(NT%AL(JP)%XT_WALL_A .GT. XTT+100. ) &
             .OR. ANY(NT%AL(JP)%XT_ROAD .GT. XTT+100. )) THEN
      CALL GET_LUOUT(HPROGRAM,ILUOUT)
      WRITE(ILUOUT,*) '--------------------------------------------------------'
      WRITE(ILUOUT,*) ' coupling_tebn : date and time (UTC) = ', KYEAR, KMONTH, KDAY, PTIME
      DO JLAYER=1,SIZE(NT%AL(JP)%XT_ROOF,2)
         WRITE(ILUOUT,*) " coupling_tebn : NT%AL(JP)%XT_ROOF  (:,",JLAYER,") = ", NT%AL(JP)%XT_ROOF(:,JLAYER)
      END DO
      WRITE(ILUOUT,*) ' '
      DO JLAYER=1,SIZE(NT%AL(JP)%XT_ROAD,2)
         WRITE(ILUOUT,*) " coupling_tebn : NT%AL(JP)%XT_ROAD  (:,",JLAYER,") = ", NT%AL(JP)%XT_ROAD(:,JLAYER)
      END DO
      WRITE(ILUOUT,*) ' '
      DO JLAYER=1,SIZE(NT%AL(JP)%XT_WALL_A,2)
         WRITE(ILUOUT,*) " coupling_tebn : NT%AL(JP)%XT_WALL_A(:,",JLAYER,") = ", NT%AL(JP)%XT_WALL_A(:,JLAYER)
      END DO
      CALL FLUSH(ILUOUT)
      CALL ABOR1_SFX("Irrealistic temperature reached for Roof, Road or Wall.")
  ENDIF
  !
  !-------------------------------------------------------------------------------------
  ! Diagnostics on each patch
  !-------------------------------------------------------------------------------------
  !
  IF (TD%MTO%LSURF_MISC_BUDGET) THEN
    !
    ! cumulated diagnostics 
    ! ---------------------
    !
    CALL CUMUL_DIAG_TEB_n(TD%NDMTC%AL(JP),  TD%NDMT%AL(JP),  &
                          GDM%VD%ND%AL(JP), GDM%VD%NDC%AL(JP), GDM%VD%NDEC%AL(JP), GDM%VD%NDE%AL(JP), &
                          GRM%VD%ND%AL(JP), GRM%VD%NDC%AL(JP), GRM%VD%NDEC%AL(JP), GRM%VD%NDE%AL(JP), TOP, PTSTEP, PRAIN, PSN)
    !
    IF (TOP%LURBHYDRO) THEN 
      CALL BUDGET_HYDRO_n(TD%NDMTC%AL(JP), TD%NDMT%AL(JP), &
                        GDM%VD%NDC%AL(JP), GDM%VD%NDEC%AL(JP), GDM%VD%NDE%AL(JP), GRM%VD%NDC%AL(JP), GRM%VD%NDEC%AL(JP), &
                        NT%AL(JP), GDM%P, GDM%NPE%AL(JP), HM%NTH%AL(JP), TOP) 
    ENDIF
  ENDIF
  !
  !-------------------------------------------------------------------------------------
  ! Computes averaged parameters necessary for UTCI
  !-------------------------------------------------------------------------------------
  !
  IF (TD%O%N2M >0 .AND. TD%DU%LUTCI) THEN
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_REF_SW_GRND ,TD%NDMT%AL(JP)%XREF_SW_GRND )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_REF_SW_FAC  ,TD%NDMT%AL(JP)%XREF_SW_FAC )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_REF_SW_HVEG ,ZREF_SW_HVEG )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_SCA_SW      ,ZSCA_SW      )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_DIR_SW      ,ZDIR_SW      )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_SCA_SW_GROUND_DOWN, ZSCA_SW_GROUND_DOWN)
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_SCA_SW_GROUND_UP  , ZSCA_SW_GROUND_UP)
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_SCA_SW_GROUND_HOR , ZSCA_SW_GROUND_HOR)
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_LW_GROUND_DOWN, ZLW_GROUND_DOWN)
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_LW_GROUND_HOR , ZLW_GROUND_HOR)    
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_DIR_SW_ROAD ,TD%NDMT%AL(JP)%XDIR_SW_ROAD )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_EMIT_LW_FAC ,TD%NDMT%AL(JP)%XEMIT_LW_FAC )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_EMIT_LW_GRND,TD%NDMT%AL(JP)%XEMIT_LW_GRND)
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_EMIT_LW_HVEG,ZEMIT_LW_HVEG)
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_SCA_SW_SKY  ,ZSCA_SW_SKY  )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_LW_RAD_SKY  ,ZLW_RAD_SKY  )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_ROAD_SHADE  ,TD%NDMT%AL(JP)%XROAD_SHADE  )
    !
    IF (TOP%LGARDEN .AND. TOP%CURBTREE/='NONE') THEN
        CALL ADD_PATCH_CONTRIB(JP,ZAVG_TAU_SR   ,NT%AL(JP)%XTAU_SR) 
    ELSE
        ZAVG_TAU_SR(:)   = 1.   
    ENDIF  
    !
    DO JCOMP=1,BOP%NBEMCOMP
        CALL ADD_PATCH_CONTRIB(JP, ZAVG_T_RAD_IND(:,JCOMP), TD%NDMT%AL(JP)%XT_RAD_IND(:,JCOMP) )
        CALL ADD_PATCH_CONTRIB(JP, ZAVG_TI_BLD(:,JCOMP)   , NB%AL(JP)%XTI_BLD(:,JCOMP) )
        CALL ADD_PATCH_CONTRIB(JP, ZAVG_QI_BLD(:,JCOMP)   , NB%AL(JP)%XQI_BLD(:,JCOMP) )
    ENDDO
    !
  ENDIF
  !
  !-------------------------------------------------------------------------------------
  ! Use of the canopy version of TEB
  !-------------------------------------------------------------------------------------
  !
  IF (TOP%LCANOPY) THEN
    !
    !-------------------------------------------------------------------------------------
    ! Town averaged quantities to force canopy atmospheric layers
    !-------------------------------------------------------------------------------------
    !
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_DUWDU_GRND ,ZDUWDU_GRND )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_UW_RF ,ZUW_RF)
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_DUWDU_RF ,ZDUWDU_RF)
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_H_WL ,0.5*(TD%NDMT%AL(JP)%XH_WALL_A+TD%NDMT%AL(JP)%XH_WALL_B))
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_E_WL ,(0.5*(TD%NDMT%AL(JP)%XLE_WALL_A + TD%NDMT%AL(JP)%XLE_WALL_B))/XLVTT)
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_H_RF ,(TD%NDMT%AL(JP)%XH_ROOF+NT%AL(JP)%XH_INDUSTRY))
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_E_RF ,(TD%NDMT%AL(JP)%XLE_ROOF+NT%AL(JP)%XLE_INDUSTRY)/XLVTT)
    IF (TOP%LGARDEN .AND. TOP%CURBTREE/='NONE') THEN
        CALL ADD_PATCH_CONTRIB(JP,ZAVG_URBTREE,NT%AL(JP)%XURBTREE   )
        !   Average of turbulent fluxes and LAD profile on TEB patchs and CANOPY layers
        !
        DO JLAYER=1,SB%NLVL
           CALL ADD_PATCH_CONTRIB(JP,ZAVG_DH_HVEG(:,JLAYER),ZDH_HVEG (:,JLAYER))
           CALL ADD_PATCH_CONTRIB(JP,ZAVG_DE_HVEG(:,JLAYER),ZDLE_HVEG(:,JLAYER)/XLVTT)
           CALL ADD_PATCH_CONTRIB(JP,ZAVG_LAD_CAN(:,JLAYER),ZLAD_CAN(:,JLAYER))
        ENDDO
    ELSE
        ZAVG_URBTREE(:)   = 0.
        ZAVG_DH_HVEG(:,:) = 0.
        ZAVG_DE_HVEG(:,:) = 0.
        ZAVG_LAD_CAN(:,:) = 0.
    ENDIF
    !
    !-------------------------------------------------------------------------------------
    ! Computes the impact of canopy and surfaces on air
    !-------------------------------------------------------------------------------------
    !
    ZAC_GRND    (:) = (NT%AL(JP)%XROAD(:)*ZAC_RD    (:) + &
             NT%AL(JP)%XGARDEN(:)*ZAC_GD    (:)) / (NT%AL(JP)%XROAD(:)+NT%AL(JP)%XGARDEN(:))
    ZAC_GRND_WAT(:) = (NT%AL(JP)%XROAD(:)*ZAC_RD_WAT(:) + &
             NT%AL(JP)%XGARDEN(:)*ZAC_GD_WAT(:)) / (NT%AL(JP)%XROAD(:)+NT%AL(JP)%XGARDEN(:))
    !
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_AC_GRND     ,ZAC_GRND    )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_AC_GRND_WAT ,ZAC_GRND_WAT)
    CALL ADD_PATCH_CONTRIB(JP,ZSFLUX_U ,ZUW_GRND * (1.-NT%AL(JP)%XBLD))
    CALL ADD_PATCH_CONTRIB(JP,ZSFLUX_T ,ZH_GRND  * (1.-NT%AL(JP)%XBLD)/XCPD/PRHOA(:,1))
    CALL ADD_PATCH_CONTRIB(JP,ZSFLUX_Q ,ZLE_GRND * (1.-NT%AL(JP)%XBLD)/XLVTT)
    !
  END IF
  !
  !-------------------------------------------------------------------------------------
  ! end of loop on TEB patches
END DO
!-------------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------------
!* Evolution of canopy air if canopy option is active
!-------------------------------------------------------------------------------------
!
!
IF (TOP%LCANOPY) THEN
   !
   IF (.NOT.TOP%LATM_CANOPY) THEN
     !
     !-------------------------------------------------------------------------------------
     !* Impact of TEB fluxes on the air
     !-------------------------------------------------------------------------------------
     !
     CALL TEB_CANOPY(KI, SB, ZAVG_BLD, ZAVG_BLD_HEIGHT, ZAVG_WL_O_HOR, PPA(:,1), PRHOA(:,1), &
                     ZAVG_DUWDU_GRND, ZAVG_UW_RF, ZAVG_DUWDU_RF, ZAVG_H_WL,         &
                     ZAVG_E_WL, ZAVG_H_RF, ZAVG_E_RF, ZAVG_DH_HVEG, ZAVG_DE_HVEG,   &
                     ZAVG_AC_GRND,ZAVG_AC_GRND_WAT, ZAVG_URBTREE, ZAVG_LAD_CAN, ZFORC_U, &
                     ZDFORC_UDU, ZFORC_E, ZDFORC_EDE, ZFORC_T, ZDFORC_TDT, ZFORC_Q, &
                     ZDFORC_QDQ )
     !
     !-------------------------------------------------------------------------------------
     !* Evolution of canopy air due to these impacts
     !-------------------------------------------------------------------------------------
     !
     CALL CANOPY_EVOL(SB, KI, PTSTEP, 2, ZL, ZWIND(:,1), PTA(:,1), PQA(:,1), PPA(:,1), PRHOA(:,1),  &
                      ZSFLUX_U, ZSFLUX_T, ZSFLUX_Q, ZFORC_U, ZDFORC_UDU,    &
                      ZFORC_E, ZDFORC_EDE, ZFORC_T, ZDFORC_TDT, ZFORC_Q,    &
                      ZDFORC_QDQ, SB%XLM, SB%XLEPS, ZAVG_USTAR, ZALFAU,   &
                      ZBETAU, ZALFAT, ZBETAT, ZALFAQ, ZBETAQ      )
     !
     ! Robert: 
     ! Since not all calculations related to the canopy are implicit it is possible
     ! that unrealistic (even negative) values of humidity in the canopy occur. 
     ! For this reason, a pragmatic correction is implemented here in the case
     ! where the absolute humidity of the canopy deviates strongly from the 
     ! absolute humidity of the forcing.
     ! In the long term all computations related to the canopy should be implicited.
     !
     DO JLAYER=1,SB%NLVL
       !
       WHERE ( SB%XQ(:,JLAYER).LT.(0.3*PQA(:,1)) )
          SB%XQ(:,JLAYER) = 0.3 * PQA(:,1)
       ELSEWHERE ( SB%XQ(:,JLAYER).GT.(3.0*PQA(:,1)) )
          SB%XQ(:,JLAYER) = 3.0 * PQA(:,1)
       END WHERE
       !
     ENDDO
     !
     !-------------------------------------------------------------------------------------
     ! Momentum fluxes in the case canopy is active
     !-------------------------------------------------------------------------------------
     !
   ENDIF
   !
   PSFU=0.
   PSFV=0.
   ZAVG_Z0_TOWN(:) = MIN(ZAVG_Z0_TOWN(:),PUREF(:,1)*0.5)
   ZAVG_CDN=(XKARMAN/LOG(PUREF(:,1)/ZAVG_Z0_TOWN(:)))**2
   ZAVG_CD = ZAVG_CDN
   ZAVG_RI = 0.
   !
   PCD_ROOF(:)        = 0.0
   IF (TOP%LATM_CANOPY) THEN
      !
      ZAVG_USTAR(:)      = SQRT(ABS(ZSFLUX_U))
      ZAVG_USTAR_ROOF(:) = SQRT(ABS(ZAVG_UW_RF))
      !
      DO JJ=1,KI
         IF (ZUA(JJ)>0.) THEN
            PCD_ROOF(JJ) = ZAVG_BLD(JJ)*ZAVG_USTAR_ROOF(JJ)**2 / ZUA(JJ)**2
         ENDIF
      ENDDO
      !
   ENDIF
   !
   DO JJ=1,SIZE(PU,1)
     IF (ZWIND(JJ,1)>0.) THEN
       ZCOEF(JJ) = - PRHOA(JJ,1) * ZAVG_USTAR(JJ)**2 / ZWIND(JJ,1)
       PSFU(JJ) = ZCOEF(JJ) * PU(JJ,1)
       PSFV(JJ) = ZCOEF(JJ) * PV(JJ,1)
       ZAVG_CD(JJ) = ZAVG_USTAR(JJ)**2 / ZWIND(JJ,1)**2
       ZAVG_RI(JJ) = -XG/PTA(JJ,1)*ZSFLUX_T(JJ)/ZAVG_USTAR(JJ)**4
     ENDIF
   ENDDO
   !
   !-------------------------------------------------------------------------------------
   !* Update of canyon parameters at the end of the time step for the consistance of diagnostics
   !-------------------------------------------------------------------------------------
   !
   IF (.NOT.TOP%LATM_CANOPY) THEN
      !
      DO JLAYER=1,SB%NLVL-1
         DO JI=1,KI
            !* finds middle canyon layer
            IF (SB%XZ(JI,JLAYER)<ZAVG_BLD_HEIGHT(JI)/2. .AND. &
                SB%XZ(JI,JLAYER+1)>=ZAVG_BLD_HEIGHT(JI)/2.) THEN
               ZCOEF(JI) = (ZAVG_BLD_HEIGHT(JI)/2.-SB%XZ(JI,JLAYER))/(SB%XZ(JI,JLAYER+1)-SB%XZ(JI,JLAYER))
               ZU_CANYON(JI) = SB%XU(JI,JLAYER) + ZCOEF(JI) * (SB%XU(JI,JLAYER+1)-SB%XU(JI,JLAYER))
               ZT_CANYON(JI) = SB%XT(JI,JLAYER) + ZCOEF(JI) * (SB%XT(JI,JLAYER+1)-SB%XT(JI,JLAYER))
               ZQ_CANYON(JI) =(SB%XQ(JI,JLAYER) + ZCOEF(JI) * &
                 (SB%XQ(JI,JLAYER+1)-SB%XQ(JI,JLAYER)))/PRHOA(JI,1)
            END IF
         END DO
      END DO
      ZU_CANYON= MAX(ZU_CANYON,0.2)
      !
      DO JP=1,TOP%NTEB_PATCH
         NT%AL(JP)%XT_CANYON(:) = ZT_CANYON(:)
         NT%AL(JP)%XQ_CANYON(:) = ZQ_CANYON(:)
      ENDDO
      !
   ENDIF
   !
   !-------------------------------------------------------------------------------------
   ! End of specific case with canopy option
   !-------------------------------------------------------------------------------------
   !
END IF
!
!-------------------------------------------------------------------------------------
! Outputs:
!-------------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------------
!Radiative properties should be at time t+1 (see by the atmosphere) in order to close
!the energy budget between surfex and the atmosphere. It is not the case here
!for ALB and EMIS
!-------------------------------------------------------------------------------------
!
 CALL AVERAGE_RAD(TOP%XTEB_PATCH, ZDIR_ALB_PATCH, ZSCA_ALB_PATCH, ZEMIS_PATCH, &
                  ZTRAD_PATCH, PDIR_ALB, PSCA_ALB, PEMIS, PTRAD )
!
!-------------------------------------------------------------------------------
!Physical properties see by the atmosphere in order to close the energy budget 
!between surfex and the atmosphere. All variables should be at t+1 but very 
!difficult to do. Maybe it will be done later. However, Ts can be at time t+1
!-------------------------------------------------------------------------------
!
PTSURF (:) = PTRAD         (:) ! Should be the surface effective temperature; not radative
PZ0    (:) = ZAVG_Z0_TOWN  (:) ! Should account for ISBA (greenroof and garden) Z0
PZ0H   (:) = PZ0 (:) / 200.    ! Should account for ISBA (greenroof and garden) Z0
PQSURF (:) = NT%AL(1)%XQ_CANYON(:) ! Should account for ISBA (greenroof and garden) Qs
!
!-------------------------------------------------------------------------------------
! Scalar fluxes:
!-------------------------------------------------------------------------------------
!
ZAVG_USTAR    (:) = SQRT(SQRT(PSFU**2+PSFV**2))
!
!
IF (CHT%SVT%NBEQ>0) THEN

  IBEG = CHT%SVT%NSV_CHSBEG
  IEND = CHT%SVT%NSV_CHSEND
  IF (CHT%CCH_DRY_DEP == "WES89") THEN
      CALL CH_DEP_TOWN(ZAVG_RESA_TOWN,  ZAVG_USTAR, PTA(:,1), PTRAD, ZAVG_WL_O_HOR,&
                       PSV(:,IBEG:IEND), CHT%SVT%CSV(IBEG:IEND), CHT%XDEP(:,1:CHT%SVT%NBEQ)  )

    DO JI=IBEG,IEND
!cdir nodep
      DO JJ=1,SIZE(PSFTS,1)
        PSFTS(JJ,JI) = - PSV(JJ,JI) * CHT%XDEP(JJ,JI-IBEG+1)
      ENDDO
    ENDDO

    IF (CHT%SVT%NAEREQ > 0 ) THEN

      IBEG = CHT%SVT%NSV_AERBEG
      IEND = CHT%SVT%NSV_AEREND

      CALL CH_AER_DEP(PSV(:,IBEG:IEND), PSFTS(:,IBEG:IEND), &
                      ZAVG_USTAR, ZAVG_RESA_TOWN, PTA(:,1), PRHOA(:,1))   
    END IF

  ELSE

    IBEG = CHT%SVT%NSV_CHSBEG
    IEND = CHT%SVT%NSV_CHSEND

    DO JI=IBEG,IEND
      PSFTS(:,JI) =0.
    ENDDO

    IBEG = CHT%SVT%NSV_AERBEG
    IEND = CHT%SVT%NSV_AEREND

    IF(IBEG.LT.IEND) THEN
      DO JI=IBEG,IEND
        PSFTS(:,JI) =0.
      ENDDO
    ENDIF
  ENDIF

ENDIF

IF (CHT%SVT%NDSTEQ>0) THEN
  ! Blindage à enlever lorsque que TEB aura été corrigé
  ZUSTAR(:)     = MIN(ZUSTAR(:), 10.)
  ZRESA_TOWN(:) = MAX(ZRESA_TOWN(:), 10.)
  !
  IBEG = CHT%SVT%NSV_DSTBEG
  IEND = CHT%SVT%NSV_DSTEND
  !
  CALL DSLT_DEP(PSV(:,IBEG:IEND), PSFTS(:,IBEG:IEND), ZUSTAR, ZRESA_TOWN, PTA(:,1), PRHOA(:,1), &
          DST%XEMISSIG_DST, DST%XEMISRADIUS_DST, JPMODE_DST, XDENSITY_DST, &
          XMOLARWEIGHT_DST, ZCONVERTFACM0_DST, ZCONVERTFACM6_DST,          &
          ZCONVERTFACM3_DST, LVARSIG_DST, LRGFIX_DST, CVERMOD  )  

  CALL MASSFLUX2MOMENTFLUX(         &
    PSFTS(:,IBEG:IEND),             & !I/O ![kg/m2/sec] In: flux of only mass, out: flux of moments
    PRHOA(:,1),                     & !I [kg/m3] air density
    DST%XEMISRADIUS_DST,            &!I [um] emitted radius for the modes (max 3)
    DST%XEMISSIG_DST,               &!I [-] emitted sigma for the different modes (max 3)
    NDSTMDE,                        &
    ZCONVERTFACM0_DST,              &
    ZCONVERTFACM6_DST,              &
    ZCONVERTFACM3_DST,              &
    LVARSIG_DST, LRGFIX_DST         )  
ENDIF
IF (CHT%SVT%NSLTEQ>0) THEN
   !
   IBEG = CHT%SVT%NSV_SLTBEG
   IEND = CHT%SVT%NSV_SLTEND
   !
   CALL DSLT_DEP(PSV(:,IBEG:IEND), PSFTS(:,IBEG:IEND), ZUSTAR, ZRESA_TOWN, PTA(:,1), PRHOA(:,1), &
          SLT%XEMISSIG_SLT, SLT%XEMISRADIUS_SLT, JPMODE_SLT, XDENSITY_SLT, &
          XMOLARWEIGHT_SLT, ZCONVERTFACM0_SLT, ZCONVERTFACM6_SLT,          &
          ZCONVERTFACM3_SLT, LVARSIG_SLT, LRGFIX_SLT, CVERMOD  )  

   CALL MASSFLUX2MOMENTFLUX(         &
    PSFTS(:,IBEG:IEND),             & !I/O ![kg/m2/sec] In: flux of only mass, out: flux of moments
    PRHOA(:,1),                     & !I [kg/m3] air density
    SLT%XEMISRADIUS_SLT,            &!I [um] emitted radius for the modes (max 3)
    SLT%XEMISSIG_SLT,               &!I [-] emitted sigma for the different modes (max 3)
    NSLTMDE,                        &
    ZCONVERTFACM0_SLT,              &
    ZCONVERTFACM6_SLT,              &
    ZCONVERTFACM3_SLT,              &
    LVARSIG_SLT, LRGFIX_SLT         ) 
ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Inline diagnostics
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
CALL DIAG_INLINE_TEB_n(TD%O, TD%D, SB, NT%AL(1), TOP%LCANOPY, PTA(:,1), PTRAD, ZQA, &
     PPA(:,1), PPS, PRHOA(:,1), PU(:,1), PV(:,1), ZWIND(:,1), PZREF(:,1), PUREF(:,1),    &
     ZAVG_CD, ZAVG_CDN, ZAVG_RI, ZAVG_CH, ZAVG_Z0_TOWN, PTRAD, PEMIS, PDIR_ALB,     &
     PSCA_ALB, PLW, PDIR_SW, PSCA_SW,  PSFTH, PSFTQ, PSFU, PSFV, PSFCO2, ZAVG_RN,   &
     ZAVG_H, ZAVG_LE, ZAVG_GFLX, ZAVG_QF )
!
!-------------------------------------------------------------------------------------
! Stores Canyon air and humidity if historical option of TEB is active
!-------------------------------------------------------------------------------------
!
IF (.NOT. TOP%LCANOPY) THEN
  DO JP=1,TOP%NTEB_PATCH
    NT%AL(JP)%XT_CANYON(:) = ZAVG_T_CANYON(:)
    NT%AL(JP)%XQ_CANYON(:) = ZAVG_Q_CANYON(:)
  END DO
END IF
!          
!-------------------------------------------------------------------------------------
! Thermal confort index
!-------------------------------------------------------------------------------------
!
IF (TD%DU%LUTCI .AND. TD%O%N2M >0) THEN
  !
  ! Wind speed for UTCI is in 10 m above ground
  !
  DO JJ=1,KI
    IF (TD%D%XZON10M(JJ)/=XUNDEF) THEN
      ZU_UTCI(JJ) = SQRT(TD%D%XZON10M(JJ)**2+TD%D%XMER10M(JJ)**2)
    ELSE
      ZU_UTCI(JJ) = ZWIND(JJ,1)
    ENDIF
  ENDDO
  !
  ! Temperature and specific humidity for UTCI is in
  ! 1 m above ground in the case the SBL scheme is active.
  ! Otherwise, due to the lack of appropriate diagnostics,
  ! the canyon average values are taken.
  !
  IF (TOP%LCANOPY) THEN
     !
     CALL INTERPOL_SBL(SB%XZ(:,:),SB%XT(:,:),1.0,ZT_UTCI(:))
     !
     CALL INTERPOL_SBL(SB%XZ(:,:),SB%XQ(:,:),1.0,ZQ_UTCI(:))
     ZQ_UTCI(:) = ZQ_UTCI(:) / PRHOA(:,1)
     !
  ELSE
     ZT_UTCI(:) = ZAVG_T_CANYON(:)
     ZQ_UTCI(:) = ZAVG_Q_CANYON(:)
  ENDIF
  !
  DO JCOMP=1,BOP%NBEMCOMP
     !
     CALL UTCI_TEB(NT%AL(1), TD%DU, TOP, JCOMP, HPROGRAM, ZAVG_TI_BLD(:,JCOMP), ZAVG_QI_BLD(:,JCOMP), &
          ZU_UTCI, ZT_UTCI, ZQ_UTCI, PPS, ZAVG_REF_SW_GRND, ZAVG_REF_SW_FAC, ZAVG_SCA_SW,             &
          ZAVG_DIR_SW, PZENITH, ZAVG_EMIT_LW_FAC, ZAVG_EMIT_LW_GRND, ZAVG_EMIT_LW_HVEG,               &
          ZAVG_SCA_SW_SKY, ZAVG_LW_RAD_SKY, PLW, ZAVG_T_RAD_IND(:,JCOMP), ZAVG_TAU_SR,                &
          ZAVG_SCA_SW_GROUND_DOWN, ZAVG_SCA_SW_GROUND_UP, ZAVG_SCA_SW_GROUND_HOR, ZAVG_LW_GROUND_DOWN,&
          ZAVG_LW_GROUND_HOR, "OK" )
     !
     CALL UTCIC_STRESS(PTSTEP, TD%DU%XUTCI_IN(:,JCOMP), TD%DU%XUTCIC_IN(:,:,JCOMP) )
     !
  ENDDO
  !
  ! Aggregated outdoor UTCI and mean radiant temperature according to sun and shade fractions
  DO JJ=1,KI
    IF (ZAVG_DIR_SW(JJ).GT.0.) THEN
       TD%DU%XUTCI_OUTAGG(JJ) = TD%DU%XUTCI_OUTSUN  (JJ)*(   ZAVG_DIR_SW_ROAD(JJ)/ZAVG_DIR_SW(JJ)) &
                              + TD%DU%XUTCI_OUTSHADE(JJ)*(1.-ZAVG_DIR_SW_ROAD(JJ)/ZAVG_DIR_SW(JJ))
       TD%DU%XTRAD_AGG   (JJ) = TD%DU%XTRAD_SUN     (JJ)*(   ZAVG_DIR_SW_ROAD(JJ)/ZAVG_DIR_SW(JJ)) &
                              + TD%DU%XTRAD_SHADE   (JJ)*(1.-ZAVG_DIR_SW_ROAD(JJ)/ZAVG_DIR_SW(JJ))
    ELSE
       TD%DU%XUTCI_OUTAGG(JJ) = TD%DU%XUTCI_OUTSHADE(JJ)
       TD%DU%XTRAD_AGG   (JJ) = TD%DU%XTRAD_SHADE   (JJ)
    ENDIF
  ENDDO
  !
  ! Mean UTCI and TRAD
  !
  TD%DU%NCOUNT_UTCI_STEP    = TD%DU%NCOUNT_UTCI_STEP    + 1
  TD%DU%XUTCI_OUTSUN_MEAN   = TD%DU%XUTCI_OUTSUN_MEAN   + TD%DU%XUTCI_OUTSUN  
  TD%DU%XUTCI_OUTSHADE_MEAN = TD%DU%XUTCI_OUTSHADE_MEAN + TD%DU%XUTCI_OUTSHADE
  TD%DU%XTRAD_SUN_MEAN      = TD%DU%XTRAD_SUN_MEAN      + TD%DU%XTRAD_SUN
  TD%DU%XTRAD_SHADE_MEAN    = TD%DU%XTRAD_SHADE_MEAN    + TD%DU%XTRAD_SHADE
  !
  CALL UTCIC_STRESS(PTSTEP,TD%DU%XUTCI_OUTSUN  ,TD%DU%XUTCIC_OUTSUN  )
  CALL UTCIC_STRESS(PTSTEP,TD%DU%XUTCI_OUTSHADE,TD%DU%XUTCIC_OUTSHADE)
  CALL UTCIC_STRESS(PTSTEP,TD%DU%XUTCI_OUTAGG  ,TD%DU%XUTCIC_OUTAGG  )
  !
ELSE IF (TD%DU%LUTCI) THEN
  TD%DU%XUTCI_IN    (:,:) = XUNDEF
  TD%DU%XUTCI_OUTSUN  (:) = XUNDEF
  TD%DU%XUTCI_OUTSHADE(:) = XUNDEF
  TD%DU%XUTCI_OUTAGG  (:) = XUNDEF
  TD%DU%XUTCI_OUTSUN_MEAN  (:) = XUNDEF
  TD%DU%XUTCI_OUTSHADE_MEAN(:) = XUNDEF
  TD%DU%XTRAD_SUN(:) = XUNDEF
  TD%DU%XTRAD_SHADE(:) = XUNDEF
  TD%DU%XTRAD_SUN_MEAN(:) = XUNDEF
  TD%DU%XTRAD_SHADE_MEAN(:) = XUNDEF
  TD%DU%XUTCIC_IN    (:,:,:) = XUNDEF
  TD%DU%XUTCIC_OUTSUN  (:,:) = XUNDEF
  TD%DU%XUTCIC_OUTSHADE(:,:) = XUNDEF
  TD%DU%XUTCIC_OUTAGG  (:,:) = XUNDEF
ENDIF
!
IF (TOP%CBEM.EQ."BEM") THEN
   !
   DO JP=1,TOP%NTEB_PATCH
      !
      ! Update auxiliairy variable for pressure at previous time step.
      !
      NB%AL(JP)%XPSOLD(:)=PPS(:)
      !
      ! Determine the switch for shading status
      ! during vacancy at 7:00 solar time. 
      !
      DO JJ=1,SIZE(NB%AL(JP)%XSHADVACSW,1)
         DO JCOMP=1,BOP%NBEMCOMP
            IF ( (PTSUN(JJ).GE.7.0*3600.0).AND.(PTSUN(JJ).LT.(7.0*3600.0+PTSTEP) ) ) THEN
               IF ((NB%AL(JP)%XTI_BLD(JJ,JCOMP).GT.NB%AL(JP)%XTDESV(JJ)).AND. &
                    (NB%AL(JP)%XTI_BLD(JJ,JCOMP).GT.NB%AL(JP)%XTHEAT_OCCD(JJ,JCOMP))) THEN
                  NB%AL(JP)%XSHADVACSW(JJ,JCOMP)=1.0
               ELSE
                  NB%AL(JP)%XSHADVACSW(JJ,JCOMP)=0.0
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      !
      ! Determine the switch for ventilation status
      ! during night at 22:00 solar time.
      ! This status change might not be reasonable for all building uses
      !
      DO JJ=1,SIZE(NB%AL(JP)%XVENTNIGSW,1)
         DO JCOMP=1,BOP%NBEMCOMP
            IF ( (PTSUN(JJ).GE.22.0*3600.0).AND.(PTSUN(JJ).LT.(22.0*3600.0+PTSTEP) ) ) THEN
               IF ((NB%AL(JP)%XTI_BLD(JJ,JCOMP).GT.NB%AL(JP)%XTDESV(JJ)).AND. &
                    (NB%AL(JP)%XTI_BLD(JJ,JCOMP).GT.NB%AL(JP)%XTHEAT_OCCD(JJ,JCOMP))) THEN
                  NB%AL(JP)%XVENTNIGSW(JJ,JCOMP) = 1.0
               ELSE
                  NB%AL(JP)%XVENTNIGSW(JJ,JCOMP) = 0.0
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      !
   ENDDO
   !
ENDIF

!
! ###############################################################
! Verification of radiation budget
! FIXME: Commented at the moment.
! ###############################################################
!
!IF (SIZE(TD%D%XSWD)>0) THEN
!  DO JJ = 1, SIZE(ZAVG_RN)
!   !
!   ZNET_UP_DOWN(JJ) = TD%D%XSWD(JJ) - TD%D%XSWU(JJ) + TD%D%XLWD(JJ) - TD%D%XLWU(JJ)
!   !
!   IF ( ISNAN(ZNET_UP_DOWN(JJ)) .OR. ISNAN(ZAVG_RN(JJ)) ) THEN
!      CALL GET_LUOUT(HPROGRAM,ILUOUT)
!      WRITE(ILUOUT,*) "ZNET_UP_DOWN(JJ) ",ZNET_UP_DOWN(JJ)
!      WRITE(ILUOUT,*) "ZAVG_RN     (JJ) ",ZAVG_RN(JJ)
!      CALL FLUSH(ILUOUT)
!      CALL ABOR1_SFX("CHECK_TEB: NAN in radiation budget diagnostics, check report")
!   ENDIF
!   !
!   IF ( ABS(ZNET_UP_DOWN(JJ)-ZAVG_RN(JJ)).GT. 1.) THEN
!      WRITE(ILUOUT,*) "                                      "
!      WRITE(ILUOUT,*) "Large incoherence in radiation budget (larger than 1W/m2) "
!      WRITE(ILUOUT,*) "                                      "
!      WRITE(ILUOUT,*) "TD%D%XSWD(JJ)    ",TD%D%XSWD(JJ)
!      WRITE(ILUOUT,*) "TD%D%XSWU(JJ)    ",TD%D%XSWU(JJ)
!      WRITE(ILUOUT,*) "TD%D%XLWD(JJ)    ",TD%D%XLWD(JJ)
!      WRITE(ILUOUT,*) "TD%D%XLWU(JJ)    ",TD%D%XLWU(JJ)      
!      WRITE(ILUOUT,*) "Diagnostics should be equal from radiative and energy balance point of views:"
!      WRITE(ILUOUT,*) "ZNET_UP_DOWN(JJ) ",ZNET_UP_DOWN(JJ)
!      WRITE(ILUOUT,*) "ZAVG_RN(JJ)      ",ZAVG_RN(JJ)
!      WRITE(ILUOUT,*) 
!      CALL FLUSH(ILUOUT)
!      ! CALL ABOR1_SFX("COUPLING_TEBn: Radiation budget is not closed for at least one point, check report")
!     EXIT
!   ENDIF
!   !
!  ENDDO
!END IF
!
IF (CT%LCHECK_TEB) CALL DEALLOC_CHECK_TEB(CT)
!
IF (LHOOK) CALL DR_HOOK('COUPLING_TEB_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
CONTAINS
SUBROUTINE ADD_PATCH_CONTRIB(JP,PAVG,PFIELD)
INTEGER, INTENT(IN) :: JP
REAL, DIMENSION(:), INTENT(INOUT) :: PAVG
REAL, DIMENSION(:), INTENT(IN)    :: PFIELD
!
IF (JP==1) PAVG = 0.
PAVG = PAVG + TOP%XTEB_PATCH(:,JP) * PFIELD(:)
!
END SUBROUTINE ADD_PATCH_CONTRIB
!-------------------------------------------------------------------------------------
!
END SUBROUTINE COUPLING_TEB_n


