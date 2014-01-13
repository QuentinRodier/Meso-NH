!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE COUPLING_TEB_n(HPROGRAM, HCOUPLING,                                             &
               PTSTEP, KYEAR, KMONTH, KDAY, PTIME, KI, KSV, KSW, PTSUN, PZENITH, PAZIM,    &
               PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV,                 &
               PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA,                   &
               PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,                                    &
               PTRAD, PDIR_ALB, PSCA_ALB, PEMIS,                                           &
               PPEW_A_COEF, PPEW_B_COEF,                                                   &
               PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,                         &
               HTEST                                                                       )
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
!!---------------------------------------------------------------
!
!
USE MODD_CSTS,         ONLY : XRD, XCPD, XP00, XLVTT, XPI, XKARMAN, XG
USE MODD_SURF_PAR,     ONLY : XUNDEF
!
USE MODD_SURF_ATM,     ONLY : CIMPLICIT_WIND
!
USE MODD_TEB_n,        ONLY : LGARDEN, LGREENROOF,                                     &
                              CBEM, TTIME,LCANOPY,CZ0H,CROAD_DIR,CWALL_OPT,            &
                              XT_CANYON, XQ_CANYON,                                    &
                              XT_ROOF, XT_ROAD, XT_WALL_A, XT_WALL_B,                  &
                              XWS_ROOF, XWS_ROAD,                                      &
                              TSNOW_ROOF, TSNOW_ROAD,                                  &
                              XH_TRAFFIC, XLE_TRAFFIC, XH_INDUSTRY, XLE_INDUSTRY,      &
                              XZ0_TOWN, XBLD, XGARDEN, XROAD_DIR, XROAD, XGREENROOF,   &
                              XBLD_HEIGHT, XWALL_O_HOR, XCAN_HW_RATIO,                 &
                              XROAD_O_GRND, XGARDEN_O_GRND, XWALL_O_GRND,              &
                              XALB_ROOF, XEMIS_ROOF, XHC_ROOF,XTC_ROOF, XD_ROOF,       &
                              XALB_ROAD, XEMIS_ROAD, XHC_ROAD,XTC_ROAD, XD_ROAD,       &
                              XALB_WALL, XEMIS_WALL, XHC_WALL,XTC_WALL, XD_WALL,       &
                              XSVF_ROAD, XSVF_WALL,                                    &
                              XSVF_GARDEN, XWALL_O_BLD,                                &
                              XQSAT_ROOF, XQSAT_ROAD, XDELT_ROOF, XDELT_ROAD,          &
                              NTEB_PATCH, XTEB_PATCH, CCH_BEM, XROUGH_ROOF, XROUGH_WALL                       
!
USE MODD_BEM_n,        ONLY : XHC_FLOOR, XTC_FLOOR, XD_FLOOR, XTCOOL_TARGET,           &
                              XTHEAT_TARGET, XF_WASTE_CAN, XEFF_HEAT, XTI_BLD,         &
                              XT_FLOOR, XT_MASS, XQIN, XQIN_FRAD, XSHGC, XSHGC_SH,     &
                              XU_WIN, XGR, XINF, CCOOL_COIL, CHEAT_COIL,               &
                              XF_WATER_COND, XAUX_MAX, XQIN_FLAT,                      &
                              XHR_TARGET, XT_WIN2, XQI_BLD, XV_VENT, XCAP_SYS_HEAT,    &
                              XCAP_SYS_RAT, XT_ADP, XM_SYS_RAT, XCOP_RAT, XT_WIN1,     &
                              XALB_WIN, XABS_WIN, XT_SIZE_MAX, XT_SIZE_MIN, XUGG_WIN,  &
                              LSHADE, CNATVENT, LSHAD_DAY, LNATVENT_NIGHT,             &
                              XN_FLOOR, XGLAZ_O_BLD, XMASS_O_BLD, XFLOOR_HW_RATIO,     &
                              XF_FLOOR_MASS, XF_FLOOR_WALL, XF_FLOOR_WIN,              &
                              XF_FLOOR_ROOF, XF_WALL_FLOOR, XF_WALL_MASS,              &
                              XF_WALL_WIN, XF_WIN_FLOOR, XF_WIN_MASS, XF_WIN_WALL,     &
                              XF_MASS_FLOOR, XF_MASS_WALL, XF_MASS_WIN, &
                              XTRAN_WIN, XF_WIN_WIN
                               
USE MODD_CH_TEB_n,     ONLY : CSV, CCH_DRY_DEP, XDEP, NBEQ, NSV_CHSBEG, NSV_CHSEND,    &
                              NSV_DSTBEG, NSV_DSTEND, NAEREQ, NDSTEQ, NSLTEQ,          &
                              NSV_AERBEG, NSV_AEREND, NSV_SLTBEG, NSV_SLTEND
USE MODD_TEB_CANOPY_n, ONLY : XZ, XU, NLVL, XTKE, XT, XQ,                              &
                              XLMO, XLM, XLEPS,XZF, XDZ, XDZF, XP
USE MODD_DIAG_TEB_n,   ONLY : N2M, XZON10M, XMER10M
USE MODD_DIAG_UTCI_TEB_n, ONLY : LUTCI, XUTCI_IN, XUTCI_OUTSUN,          &
                                 XUTCI_OUTSHADE, XTRAD_SUN, XTRAD_SHADE
USE MODD_DST_n,        ONLY : XEMISRADIUS_DST, XEMISSIG_DST
USE MODD_SLT_n,        ONLY : XEMISRADIUS_SLT, XEMISSIG_SLT
USE MODD_DST_SURF
USE MODD_SLT_SURF
!
!
USE MODE_DSLT_SURF
USE MODE_THERMOS
USE MODE_SBLS
!
USE MODI_GOTO_TEB
USE MODI_AVERAGE_RAD
USE MODI_SM10
USE MODI_ADD_FORECAST_TO_DATE_SURF
USE MODI_DIAG_INLINE_TEB_n
USE MODI_DIAG_MISC_TEB_n
USE MODI_CH_AER_DEP
USE MODI_CH_DEP_TOWN
USE MODI_DSLT_DEP
USE MODI_TEB_GARDEN
USE MODI_TEB_CANOPY
! 
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_CANOPY_EVOL
USE MODI_CANOPY_GRID_UPDATE
USE MODI_UTCI_TEB
USE MODI_CIRCUMSOLAR_RAD
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
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
REAL, DIMENSION(KI), INTENT(IN)  :: PTSUN     ! solar time                    (s from midnight)
REAL,                INTENT(IN)  :: PTSTEP    ! atmospheric time-step                 (s)
REAL, DIMENSION(KI), INTENT(IN)  :: PZREF     ! height of T,q forcing                 (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PUREF     ! height of wind forcing                (m)
!
REAL, DIMENSION(KI), INTENT(IN)  :: PTA       ! air temperature forcing               (K)
REAL, DIMENSION(KI), INTENT(IN)  :: PQA       ! air humidity forcing                  (kg/m3)
REAL, DIMENSION(KI), INTENT(IN)  :: PRHOA     ! air density                           (kg/m3)
REAL, DIMENSION(KI,KSV),INTENT(IN) :: PSV     ! scalar variables
!                                             ! chemistry:   first char. in HSV: '#'  (molecule/m3)
!                                             !
 CHARACTER(LEN=6), DIMENSION(KSV),INTENT(IN):: HSV  ! name of all scalar variables
REAL, DIMENSION(KI), INTENT(IN)  :: PU        ! zonal wind                            (m/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PV        ! meridian wind                         (m/s)
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
REAL, DIMENSION(KI), INTENT(IN)  :: PPA       ! pressure at forcing level             (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PZS       ! atmospheric model orography           (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PCO2      ! CO2 concentration in the air          (kg/m3)
REAL, DIMENSION(KI), INTENT(IN)  :: PSNOW     ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PRAIN     ! liquid precipitation                  (kg/m2/s)
!
!
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTH     ! flux of heat                          (W/m2)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTQ     ! flux of water vapor                   (kg/m2/s)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFU      ! zonal momentum flux                   (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFV      ! meridian momentum flux                (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFCO2    ! flux of CO2                           (kg/m2/s)
REAL, DIMENSION(KI,KSV),INTENT(OUT):: PSFTS   ! flux of scalar var.                   (kg/m2/s)
!
REAL, DIMENSION(KI), INTENT(OUT) :: PTRAD     ! radiative temperature                 (K)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PDIR_ALB! direct albedo for each spectral band  (-)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PSCA_ALB! diffuse albedo for each spectral band (-)
REAL, DIMENSION(KI), INTENT(OUT) :: PEMIS     ! emissivity                            (-)
!
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_A_COEF! implicit coefficients
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_B_COEF! needed if HCOUPLING='I'
REAL, DIMENSION(KI), INTENT(IN) :: PPET_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPET_B_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_B_COEF
 CHARACTER(LEN=2),    INTENT(IN) :: HTEST ! must be equal to 'OK'
!
!
!*      0.2    declarations of local variables
!
INTEGER                     :: JSWB        ! loop counter on shortwave spectral bands
!         
REAL, DIMENSION(KI)  :: ZQA         ! specific humidity                 (kg/kg)
REAL, DIMENSION(KI)  :: ZEXNA       ! Exner function at forcing level
REAL, DIMENSION(KI)  :: ZEXNS       ! Exner function at surface level
REAL, DIMENSION(KI)  :: ZWIND       ! wind
!
! Ouput Diagnostics:
!
REAL, DIMENSION(KI)  :: ZU_CANYON   ! wind in canyon
REAL, DIMENSION(KI)  :: ZT_CANYON   ! temperature in canyon
REAL, DIMENSION(KI)  :: ZQ_CANYON   ! specific humidity in canyon
REAL, DIMENSION(KI)  :: ZT_CAN      ! temperature in canyon       (evolving in TEB)
REAL, DIMENSION(KI)  :: ZQ_CAN      ! specific humidity in canyon (evolving in TEB)
!
REAL, DIMENSION(KI)  :: ZRN_ROOF    ! net radiation on roof
REAL, DIMENSION(KI)  :: ZH_ROOF     ! sensible heat flux on roof
REAL, DIMENSION(KI)  :: ZLE_ROOF    ! latent heat flux on roof
REAL, DIMENSION(KI)  :: ZLEW_ROOF   ! latent heat flux on snowfree roof
REAL, DIMENSION(KI)  :: ZGFLUX_ROOF ! storage flux in roof
REAL, DIMENSION(KI)  :: ZRUNOFF_ROOF! water runoff from roof
REAL, DIMENSION(KI)  :: ZRN_ROAD    ! net radiation on road
REAL, DIMENSION(KI)  :: ZH_ROAD     ! sensible heat flux on road
REAL, DIMENSION(KI)  :: ZLE_ROAD    ! latent heat flux on road
REAL, DIMENSION(KI)  :: ZLEW_ROAD   ! latent heat flux on snowfree road
REAL, DIMENSION(KI)  :: ZGFLUX_ROAD ! storage flux in road
REAL, DIMENSION(KI)  :: ZRUNOFF_ROAD! water runoff from road
REAL, DIMENSION(KI)  :: ZRN_WALL_A  ! net radiation on walls
REAL, DIMENSION(KI)  :: ZH_WALL_A   ! sensible heat flux on walls
REAL, DIMENSION(KI)  :: ZLE_WALL_A  ! latent heat flux on walls
REAL, DIMENSION(KI)  :: ZGFLUX_WALL_A!storage flux in walls
REAL, DIMENSION(KI)  :: ZRN_WALL_B  ! net radiation on walls
REAL, DIMENSION(KI)  :: ZH_WALL_B   ! sensible heat flux on walls
REAL, DIMENSION(KI)  :: ZLE_WALL_B  ! latent heat flux on walls
REAL, DIMENSION(KI)  :: ZGFLUX_WALL_B!storage flux in walls
REAL, DIMENSION(KI)  :: ZRN_GARDEN  ! net radiation on green areas
REAL, DIMENSION(KI)  :: ZH_GARDEN   ! sensible heat flux on green areas
REAL, DIMENSION(KI)  :: ZLE_GARDEN  ! latent heat flux on green areas
REAL, DIMENSION(KI)  :: ZGFLUX_GARDEN!storage flux in green areas
REAL, DIMENSION(KI)  :: ZRN_GREENROOF! net radiation on green roofs
REAL, DIMENSION(KI)  :: ZH_GREENROOF ! sensible heat flux on green roofs
REAL, DIMENSION(KI)  :: ZLE_GREENROOF! latent heat flux on green roofs
REAL, DIMENSION(KI)  :: ZGFLUX_GREENROOF    ! storage flux in green roofs
REAL, DIMENSION(KI)  :: ZG_GREENROOF_ROOF   ! heat flux between base of greenroof
REAL, DIMENSION(KI)  :: ZRUNOFF_GREENROOF   ! water runoff from green roof
REAL, DIMENSION(KI)  :: ZDRAIN_GREENROOF    ! water drainage from green roof
REAL, DIMENSION(KI)  :: ZRN_STRLROOF        ! net radiation on structural roof
REAL, DIMENSION(KI)  :: ZH_STRLROOF         ! sensible heat flux on structural roof
REAL, DIMENSION(KI)  :: ZLE_STRLROOF        ! latent heat flux on structural roof
REAL, DIMENSION(KI)  :: ZGFLUX_STRLROOF     ! storage flux in structural roof
REAL, DIMENSION(KI)  :: ZRN_BLT     ! net radiation on built surf 
REAL, DIMENSION(KI)  :: ZH_BLT      ! sensible heat flux on built surf 
REAL, DIMENSION(KI)  :: ZLE_BLT     ! latent heat flux on built surf 
REAL, DIMENSION(KI)  :: ZGFLUX_BLT  ! storage flux in built surf 
REAL, DIMENSION(KI)  :: ZRN_GRND    ! net radiation on ground built surf
REAL, DIMENSION(KI)  :: ZH_GRND     ! sensible heat flux on ground built surf
REAL, DIMENSION(KI)  :: ZLE_GRND    ! latent heat flux on ground built surf
REAL, DIMENSION(KI)  :: ZGFLUX_GRND ! storage flux in ground built surf
REAL, DIMENSION(KI)  :: ZRNSNOW_ROOF  ! net radiation over snow
REAL, DIMENSION(KI)  :: ZHSNOW_ROOF   ! sensible heat flux over snow
REAL, DIMENSION(KI)  :: ZLESNOW_ROOF  ! latent heat flux over snow
REAL, DIMENSION(KI)  :: ZGSNOW_ROOF   ! flux under the snow
REAL, DIMENSION(KI)  :: ZMELT_ROOF    ! snow melt
REAL, DIMENSION(KI)  :: ZRNSNOW_ROAD  ! net radiation over snow
REAL, DIMENSION(KI)  :: ZHSNOW_ROAD   ! sensible heat flux over snow
REAL, DIMENSION(KI)  :: ZLESNOW_ROAD  ! latent heat flux over snow
REAL, DIMENSION(KI)  :: ZGSNOW_ROAD   ! flux under the snow
REAL, DIMENSION(KI)  :: ZMELT_ROAD    ! snow melt
!
REAL, DIMENSION(KI)  :: ZTRAD         ! radiative temperature for current patch
REAL, DIMENSION(KI)  :: ZEMIS         ! emissivity for current patch
REAL, DIMENSION(KI,NTEB_PATCH) :: ZTRAD_PATCH ! radiative temperature for each patch
REAL, DIMENSION(KI,NTEB_PATCH) :: ZEMIS_PATCH ! emissivity for each patch
REAL, DIMENSION(KI,KSW,NTEB_PATCH) :: ZDIR_ALB_PATCH ! direct albedo per wavelength and patch
REAL, DIMENSION(KI,KSW,NTEB_PATCH) :: ZSCA_ALB_PATCH ! diffuse albedo per wavelength and patch
!
REAL, DIMENSION(KI)  :: ZRN           ! net radiation over town
REAL, DIMENSION(KI)  :: ZH            ! sensible heat flux over town
REAL, DIMENSION(KI)  :: ZLE           ! latent heat flux over town
REAL, DIMENSION(KI)  :: ZGFLUX        ! flux through the ground
REAL, DIMENSION(KI)  :: ZSFCO2        ! CO2 flux over town
REAL, DIMENSION(KI)  :: ZQF_BLD       ! domestic heating
REAL, DIMENSION(KI)  :: ZFLX_BLD      ! flux from bld
REAL, DIMENSION(KI)  :: ZDQS_TOWN     ! storage inside town materials
REAL, DIMENSION(KI)  :: ZQF_TOWN      ! total anthropogenic heat
REAL, DIMENSION(KI)  :: ZEVAP         ! evaporation (km/m2/s)
REAL, DIMENSION(KI)  :: ZRUNOFF       ! runoff over the ground
REAL, DIMENSION(KI)  :: ZCD           ! drag coefficient
REAL, DIMENSION(KI)  :: ZCDN          ! neutral drag coefficient
REAL, DIMENSION(KI)  :: ZCH           ! heat drag
REAL, DIMENSION(KI)  :: ZRI           ! Richardson number
REAL, DIMENSION(KI)  :: ZUW_GRND      ! momentum flux for ground built surf
REAL, DIMENSION(KI)  :: ZUW_ROOF      ! momentum flux for roofs
REAL, DIMENSION(KI)  :: ZDUWDU_GRND   !
REAL, DIMENSION(KI)  :: ZDUWDU_ROOF   !
REAL, DIMENSION(KI)  :: ZUSTAR        ! friction velocity
REAL, DIMENSION(KI)  :: ZSFU          ! momentum flux for patch (U direction)
REAL, DIMENSION(KI)  :: ZSFV          ! momentum flux for patch (V direction)
REAL, DIMENSION(KI)  :: ZAVG_DIR_ALB  ! direct albedo of town
REAL, DIMENSION(KI)  :: ZAVG_SCA_ALB  ! diffuse albedo of town
REAL, DIMENSION(KI)  :: ZAVG_T_CANYON ! temperature in canyon for town 
REAL, DIMENSION(KI)  :: ZAVG_Q_CANYON ! specific humidity in canyon for town
!
REAL, DIMENSION(KI)  :: ZAVG_CD       ! aggregated drag coefficient
REAL, DIMENSION(KI)  :: ZAVG_CDN      ! aggregated neutral drag coefficient
REAL, DIMENSION(KI)  :: ZAVG_RI       ! aggregated Richardson number
REAL, DIMENSION(KI)  :: ZAVG_CH       ! aggregated Heat transfer coefficient
!
REAL, DIMENSION(KI)  :: ZDIR_ALB      ! direct albedo of town
REAL, DIMENSION(KI)  :: ZSCA_ALB      ! diffuse albedo of town
!
REAL, DIMENSION(KI)  :: ZH_TRAFFIC    ! anthropogenic sensible
!                                            ! heat fluxes due to traffic
REAL, DIMENSION(KI)  :: ZLE_TRAFFIC   ! anthropogenic latent
!                                            ! heat fluxes due to traffic
REAL, DIMENSION(KI)  :: ZRESA_TOWN    ! aerodynamical resistance
REAL, DIMENSION(KI)  :: ZAC_ROAD      ! road aerodynamical conductance
REAL, DIMENSION(KI)  :: ZAC_GARDEN    ! green area aerodynamical conductance
REAL, DIMENSION(KI)  :: ZAC_GRND      ! ground built surf aerodynamical conductance
REAL, DIMENSION(KI)  :: ZAC_GREENROOF ! green roof aerodynamical conductance
REAL, DIMENSION(KI)  :: ZAC_ROAD_WAT  ! road water aerodynamical conductance
REAL, DIMENSION(KI)  :: ZAC_GARDEN_WAT! green area water aerodynamical conductance
REAL, DIMENSION(KI)  :: ZAC_GRND_WAT  ! ground built surf water aerodynamical conductance
REAL, DIMENSION(KI)  :: ZAC_GREENROOF_WAT! green roof water aerodynamical conductance
REAL, DIMENSION(KI,1):: ZESNOW_GARDEN    ! green area snow emissivity
!
REAL                        :: ZBEGIN_TRAFFIC_TIME ! start traffic time (solar time, s)
REAL                        :: ZEND_TRAFFIC_TIME   ! end traffic time   (solar time, s)
REAL, DIMENSION(KI)  :: ZDIR_SW       ! total direct SW
REAL, DIMENSION(KI)  :: ZSCA_SW       ! total diffuse SW
REAL, DIMENSION(KI)  :: ZPEW_A_COEF   ! implicit coefficients
REAL, DIMENSION(KI)  :: ZPEW_B_COEF   ! needed if HCOUPLING='I'

!***** CANOPY  *****
REAL, DIMENSION(KI)        :: ZSFLUX_U  ! Surface flux u'w' (m2/s2)
REAL, DIMENSION(KI)        :: ZSFLUX_T  ! Surface flux w'T' (mK/s)
REAL, DIMENSION(KI)        :: ZSFLUX_Q  ! Surface flux w'q' (kgm2/s)
REAL, DIMENSION(KI,NLVL)   :: ZFORC_U   ! tendency due to drag force for wind
REAL, DIMENSION(KI,NLVL)   :: ZDFORC_UDU! formal derivative of
!                                              ! tendency due to drag force for wind
REAL, DIMENSION(KI,NLVL)   :: ZFORC_E   ! tendency due to drag force for TKE
REAL, DIMENSION(KI,NLVL)   :: ZDFORC_EDE! formal derivative of
!                                              ! tendency due to drag force for TKE
REAL, DIMENSION(KI,NLVL)   :: ZFORC_T   ! tendency due to drag force for Temp
REAL, DIMENSION(KI,NLVL)   :: ZDFORC_TDT! formal derivative of
!                                              ! tendency due to drag force for Temp
REAL, DIMENSION(KI,NLVL)   :: ZFORC_Q   ! tendency due to drag force for hum
REAL, DIMENSION(KI,NLVL)   :: ZDFORC_QDQ! formal derivative of
!                                              ! tendency due to drag force for hum.

REAL, DIMENSION(KI)        :: ZAVG_UW_GRND
REAL, DIMENSION(KI)        :: ZAVG_DUWDU_GRND
REAL, DIMENSION(KI)        :: ZAVG_UW_ROOF
REAL, DIMENSION(KI)        :: ZAVG_DUWDU_ROOF
REAL, DIMENSION(KI)        :: ZAVG_H_GRND
REAL, DIMENSION(KI)        :: ZAVG_H_WALL
REAL, DIMENSION(KI)        :: ZAVG_H_ROOF
REAL, DIMENSION(KI)        :: ZAVG_E_GRND
REAL, DIMENSION(KI)        :: ZAVG_E_ROOF
REAL, DIMENSION(KI)        :: ZAVG_AC_GRND
REAL, DIMENSION(KI)        :: ZAVG_AC_GRND_WAT
REAL, DIMENSION(KI)        :: ZAVG_Z0_TOWN
REAL, DIMENSION(KI)        :: ZAVG_RESA_TOWN
REAL, DIMENSION(KI)        :: ZAVG_USTAR        ! town avegared Ustar
REAL, DIMENSION(KI)        :: ZAVG_BLD          ! town averaged building fraction
REAL, DIMENSION(KI)        :: ZAVG_BLD_HEIGHT   ! town averaged building height
REAL, DIMENSION(KI)        :: ZAVG_WALL_O_HOR   ! town averaged Wall/hor ratio
REAL, DIMENSION(KI)        :: ZAVG_CAN_HW_RATIO ! town averaged road aspect ratio
REAL, DIMENSION(KI)        :: ZAVG_H
REAL, DIMENSION(KI)        :: ZAVG_LE
REAL, DIMENSION(KI)        :: ZAVG_RN
REAL, DIMENSION(KI)        :: ZAVG_GFLUX
REAL, DIMENSION(KI)        :: ZAVG_REF_SW_GRND
REAL, DIMENSION(KI)        :: ZAVG_REF_SW_FAC
REAL, DIMENSION(KI)        :: ZAVG_SCA_SW
REAL, DIMENSION(KI)        :: ZAVG_DIR_SW 
REAL, DIMENSION(KI)        :: ZAVG_EMIT_LW_FAC
REAL, DIMENSION(KI)        :: ZAVG_EMIT_LW_GRND
REAL, DIMENSION(KI)        :: ZAVG_T_RAD_IND
REAL, DIMENSION(KI)        :: ZT_LOWCAN  ! temperature at lowest canyon level (K)
REAL, DIMENSION(KI)        :: ZQ_LOWCAN  ! humidity    at lowest canyon level (kg/kg)
REAL, DIMENSION(KI)        :: ZU_LOWCAN  ! wind        at lowest canyon level (m/s)
REAL, DIMENSION(KI)        :: ZZ_LOWCAN  ! height      of lowest canyon level (m)
REAL, DIMENSION(KI)        :: ZPEW_A_COEF_LOWCAN   ! implicit coefficients for wind coupling
REAL, DIMENSION(KI)        :: ZPEW_B_COEF_LOWCAN   ! between first canopy level and road
REAL, DIMENSION(KI)        :: ZTA        ! temperature at canyon level just above roof (K)
REAL, DIMENSION(KI)        :: ZPA        ! pressure    at canyon level just above roof (K)
REAL, DIMENSION(KI)        :: ZUA        ! wind        at canyon level just above roof (m/s)
REAL, DIMENSION(KI)        :: ZUREF      ! height      of canyon level just above roof (m)
REAL, DIMENSION(KI)        :: ZZREF      ! height      of canyon level just above roof (m)
REAL, DIMENSION(KI)        :: ZLAMBDA_F  ! frontal density (-)
REAL, DIMENSION(KI)        :: ZLMO       ! Monin-Obukhov length at canopy height (m)
REAL, DIMENSION(KI,NLVL)   :: ZL         ! Mixing length generic profile at mid levels
!
! absorbed solar and infra-red radiation by road, wall and roof
!                                                      
REAL, DIMENSION(KI) :: ZABS_SW_ROAD
REAL, DIMENSION(KI) :: ZABS_SW_WALL_A
REAL, DIMENSION(KI) :: ZABS_SW_WALL_B
REAL, DIMENSION(KI) :: ZABS_SW_ROOF
REAL, DIMENSION(KI) :: ZABS_SW_GARDEN
REAL, DIMENSION(KI) :: ZABS_SW_GREENROOF
REAL, DIMENSION(KI) :: ZABS_SW_SNOW_ROAD
REAL, DIMENSION(KI) :: ZABS_SW_SNOW_ROOF
REAL, DIMENSION(KI) :: ZABS_LW_SNOW_ROAD
REAL, DIMENSION(KI) :: ZABS_LW_SNOW_ROOF
REAL, DIMENSION(KI) :: ZABS_LW_ROAD
REAL, DIMENSION(KI) :: ZABS_LW_WALL_A
REAL, DIMENSION(KI) :: ZABS_LW_WALL_B
REAL, DIMENSION(KI) :: ZABS_LW_ROOF
REAL, DIMENSION(KI) :: ZABS_LW_GARDEN 
REAL, DIMENSION(KI) :: ZABS_LW_GREENROOF
!
REAL, DIMENSION(KI)        :: ZU_UTCI ! wind speed for the UTCI calculation (m/s) 

REAL, DIMENSION(KI)        :: ZALFAU   ! V+(1) = alfa u'w'(1) + beta
REAL, DIMENSION(KI)        :: ZBETAU   ! V+(1) = alfa u'w'(1) + beta
REAL, DIMENSION(KI)        :: ZALFAT   ! Th+(1) = alfa w'th'(1) + beta
REAL, DIMENSION(KI)        :: ZBETAT   ! Th+(1) = alfa w'th'(1) + beta
REAL, DIMENSION(KI)        :: ZALFAQ   ! Q+(1) = alfa w'q'(1) + beta
REAL, DIMENSION(KI)        :: ZBETAQ   ! Q+(1) = alfa w'q'(1) + beta
!***** CANOPY  *****
REAL, DIMENSION(KI)        :: ZWAKE      ! reduction of average wind speed
!                                              ! in canyon due to direction average.
! new local variables after BEM
!
REAL, DIMENSION(KI) :: ZCAP_SYS
REAL, DIMENSION(KI) :: ZM_SYS
REAL, DIMENSION(KI) :: ZCOP
REAL, DIMENSION(KI) :: ZQ_SYS
REAL, DIMENSION(KI) :: ZT_SYS
REAL, DIMENSION(KI) :: ZTR_SW_WIN
REAL, DIMENSION(KI) :: ZFAN_POWER
REAL, DIMENSION(KI) :: ZABS_SW_WIN
REAL, DIMENSION(KI) :: ZABS_LW_WIN
REAL, DIMENSION(KI) :: ZH_BLD_COOL
REAL, DIMENSION(KI) :: ZT_BLD_COOL
REAL, DIMENSION(KI) :: ZH_BLD_HEAT
REAL, DIMENSION(KI) :: ZLE_BLD_COOL
REAL, DIMENSION(KI) :: ZLE_BLD_HEAT  
REAL, DIMENSION(KI) :: ZH_WASTE
REAL, DIMENSION(KI) :: ZLE_WASTE
REAL, DIMENSION(KI) :: ZHVAC_COOL  
REAL, DIMENSION(KI) :: ZHVAC_HEAT

!new local variables for UTCI calculation
REAL, DIMENSION(KI) :: ZEMIT_LW_GRND
REAL, DIMENSION(KI) :: ZEMIT_LW_FAC
REAL, DIMENSION(KI) :: ZT_RAD_IND   ! Indoor mean radiant temperature [K]
REAL, DIMENSION(KI) :: ZREF_SW_GRND ! total solar rad reflected from ground
REAL, DIMENSION(KI) :: ZREF_SW_FAC  ! total solar rad reflected from facade
REAL, DIMENSION(KI) :: ZHU_BLD
REAL, DIMENSION(KI) :: ZAVG_TI_BLD
REAL, DIMENSION(KI) :: ZAVG_QI_BLD
REAL, DIMENSION(KI) :: ZF1_o_B
REAL, DIMENSION(KI,SIZE(PDIR_SW,2))  :: ZDIR_SWB ! total direct SW per band
REAL, DIMENSION(KI,SIZE(PSCA_SW,2))  :: ZSCA_SWB ! total diffuse SW per band
!
REAL, DIMENSION(KI)        :: ZCOEF
!
REAL                       :: ZCONVERTFACM0_SLT, ZCONVERTFACM0_DST
REAL                       :: ZCONVERTFACM3_SLT, ZCONVERTFACM3_DST
REAL                       :: ZCONVERTFACM6_SLT, ZCONVERTFACM6_DST
!
INTEGER                           :: JI
INTEGER                           :: JLAYER
INTEGER                           :: JJ
!
! number of TEB patches
!
INTEGER                    :: JTEB_PATCH ! loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
! Preliminaries:
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('COUPLING_TEB_N',0,ZHOOK_HANDLE)
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('COUPLING_TEBN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF

!-------------------------------------------------------------------------------------
!
! scalar fluxes
!
PSFTS(:,:) = 0.
!
! broadband radiative fluxes
!
ZDIR_SW(:) = 0.
ZSCA_SW(:) = 0.
DO JSWB=1,KSW
  !add directionnal contrib from scattered radiation
  CALL CIRCUMSOLAR_RAD(PDIR_SW(:,JSWB), PSCA_SW(:,JSWB), PZENITH, ZF1_o_B)
  ZDIR_SWB(:,JSWB) = PDIR_SW(:,JSWB) + PSCA_SW(:,JSWB) * ZF1_o_B
  ZSCA_SWB(:,JSWB) = PSCA_SW(:,JSWB) * (1. - ZF1_o_B)
  !add directionnal contrib from scattered radiation
  DO JJ=1,SIZE(PDIR_SW,1)
    ZDIR_SW(JJ) = ZDIR_SW(JJ) + ZDIR_SWB(JJ,JSWB)
    ZSCA_SW(JJ) = ZSCA_SW(JJ) + ZSCA_SWB(JJ,JSWB)
  ENDDO
END DO
!
DO JJ=1,KI
! specific humidity (conversion from kg/m3 to kg/kg)
!
  ZQA(JJ) = PQA(JJ) / PRHOA(JJ)
!
! wind
!
  ZWIND(JJ) = SQRT(PU(JJ)**2+PV(JJ)**2)
!
ENDDO
! method of wind coupling
!
IF (HCOUPLING=='I') THEN
  ZPEW_A_COEF = PPEW_A_COEF
  ZPEW_B_COEF = PPEW_B_COEF
ELSE
  ZPEW_A_COEF =  0.
  ZPEW_B_COEF =  ZWIND
END IF
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Time evolution
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
TTIME%TIME = TTIME%TIME + PTSTEP
 CALL ADD_FORECAST_TO_DATE_SURF(TTIME%TDATE%YEAR,TTIME%TDATE%MONTH,TTIME%TDATE%DAY,TTIME%TIME)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Anthropogenic fluxes (except building heating)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ZBEGIN_TRAFFIC_TIME = 21600.
ZEND_TRAFFIC_TIME   = 64800.
!
WHERE(       PTSUN>ZBEGIN_TRAFFIC_TIME   &
      .AND.  PTSUN<ZEND_TRAFFIC_TIME     )
  ZH_TRAFFIC  (:) = XH_TRAFFIC   (:)
  ZLE_TRAFFIC (:) = XLE_TRAFFIC  (:)
ELSEWHERE
  ZH_TRAFFIC  (:) = 0.
  ZLE_TRAFFIC (:) = 0.   
END WHERE
!
!--------------------------------------------------------------------------------------
!  Canyon forcing for TEB
!--------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
! Town averaged quantities to force canopy atmospheric layers
!-------------------------------------------------------------------------------------

DO JTEB_PATCH=1,NTEB_PATCH
  CALL GOTO_TEB(JTEB_PATCH)
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_BLD,         XBLD         )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_BLD_HEIGHT,  XBLD_HEIGHT  )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_WALL_O_HOR,  XWALL_O_HOR  )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_CAN_HW_RATIO,XCAN_HW_RATIO)
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_Z0_TOWN,     XZ0_TOWN     )
END DO
!
IF (LCANOPY) THEN
!-------------------------------------------------------------------------------------
! Updates canopy vertical grid as a function of forcing height
!-------------------------------------------------------------------------------------
!
!* determines where is the forcing level and modifies the upper levels of the canopy grid
!
  CALL CANOPY_GRID_UPDATE(KI,NLVL,ZAVG_BLD_HEIGHT,ZAVG_BLD_HEIGHT+PUREF,XZ,XZF,XDZ,XDZF)
!
!* Initialisations of T, Q, TKE and wind at first time step
!

  IF(ANY(XT(:,:) == XUNDEF)) THEN
    DO JLAYER=1,NLVL
      XT(:,JLAYER) = PTA(:)
      XQ(:,JLAYER) = PQA(:)
      XU(:,JLAYER) = 2./XPI * ZWIND(:)                                  &
              * LOG( (          2.* XBLD_HEIGHT(:)/3.) / XZ0_TOWN(:))   &
              / LOG( (PUREF(:)+ 2.* XBLD_HEIGHT(:)/3.) / XZ0_TOWN(:))
    END  DO
    XTKE(:,:) = 1.
  ENDIF
!
!* default forcing above roof: forcing level
ZUREF(:)     = PUREF(:)
ZZREF(:)     = PZREF(:)
ZUA(:)       = XU(:,NLVL)
ZTA(:)       = XT(:,NLVL)
ZQA(:)       = XQ(:,NLVL)/PRHOA(:)
ZPA(:)       = XP(:,NLVL)
!* for the time being, only one value is kept for wall in-canyon forcing, in the middle of the canyon
ZU_CANYON(:) = ZUA(:)
ZT_CANYON(:) = ZTA(:)
ZQ_CANYON(:) = ZQA(:)
  DO JLAYER=1,NLVL-1
    DO JI=1,KI
      !* finds middle canyon layer
      IF (XZ(JI,JLAYER)<ZAVG_BLD_HEIGHT(JI)/2. .AND. XZ(JI,JLAYER+1)>=ZAVG_BLD_HEIGHT(JI)/2.) THEN
        ZCOEF(JI) = (ZAVG_BLD_HEIGHT(JI)/2.-XZ(JI,JLAYER))/(XZ(JI,JLAYER+1)-XZ(JI,JLAYER))
        ZU_CANYON(JI) = XU(JI,JLAYER) + ZCOEF(JI) * (XU(JI,JLAYER+1)-XU(JI,JLAYER))
        ZT_CANYON(JI) = XT(JI,JLAYER) + ZCOEF(JI) * (XT(JI,JLAYER+1)-XT(JI,JLAYER))
        ZQ_CANYON(JI) =(XQ(JI,JLAYER) + ZCOEF(JI) * (XQ(JI,JLAYER+1)-XQ(JI,JLAYER)))/PRHOA(JI)
      END IF
      !* finds layer just above roof (at least 1m above roof)
      IF (XZ(JI,JLAYER)<ZAVG_BLD_HEIGHT(JI)+1. .AND. XZ(JI,JLAYER+1)>=ZAVG_BLD_HEIGHT(JI)+1.) THEN
        ZUREF(JI) = XZ(JI,JLAYER+1) - ZAVG_BLD_HEIGHT(JI)
        ZZREF(JI) = XZ(JI,JLAYER+1) - ZAVG_BLD_HEIGHT(JI)
        ZTA  (JI) = XT(JI,JLAYER+1)
        ZQA  (JI) = XQ(JI,JLAYER+1)/PRHOA(JI)
        !ZUA  (JI) = XU(JI,JLAYER+1)
        ZUA  (JI) = MAX(XU(JI,JLAYER+1) - 2.*SQRT(XTKE(JI,JLAYER+1)) , XU(JI,JLAYER+1)/3.)
        ZPA  (JI) = XP(JI,JLAYER+1)
        ZLMO (JI) = XLMO(JI,JLAYER+1)
      END IF
    END DO
  END DO
  ZU_CANYON= MAX(ZU_CANYON,0.2)
  ZU_LOWCAN=XU(:,1)
  ZT_LOWCAN=XT(:,1)
  ZQ_LOWCAN=XQ(:,1) / PRHOA(:)
  ZZ_LOWCAN=XZ(:,1)
  WHERE(ZPA==XUNDEF) ZPA = PPA   ! security for first time step
!
!-------------------------------------------------------------------------------------
! determine the vertical profile for mixing and dissipative lengths (at full levels)
!-------------------------------------------------------------------------------------
!
! frontal density
  ZLAMBDA_F(:) = ZAVG_CAN_HW_RATIO*ZAVG_BLD / (0.5*XPI)
!
  CALL SM10(XZ,ZAVG_BLD_HEIGHT,ZLAMBDA_F,ZL)
!
!-------------------------------------------------------------------------------------
! computes coefficients for implicitation
!-------------------------------------------------------------------------------------
!
  ZAVG_UW_GRND(:)      = 0.
  ZAVG_DUWDU_GRND(:)   = 0.
  ZAVG_UW_ROOF(:)      = 0.
  ZAVG_DUWDU_ROOF(:)   = 0.
  ZAVG_H_GRND(:)       = 0.
  ZAVG_H_WALL(:)       = 0.
  ZAVG_H_ROOF(:)       = 0.
  ZAVG_E_GRND(:)       = 0.
  ZAVG_E_ROOF(:)       = 0.
  ZAVG_AC_GRND(:)      = 0.
  ZAVG_AC_GRND_WAT(:)  = 0.
  ZSFLUX_U(:)          = 0.
  ZSFLUX_T(:)          = 0.
  ZSFLUX_Q(:)          = 0.
!
  DO JLAYER=1,NLVL-1
      !* Monin-Obuhkov theory not used inside the urban canopy
      ! => neutral mixing  if layer is below : (roof level +1 meter)
      WHERE (XZ(:,JLAYER)<=ZAVG_BLD_HEIGHT(:)+1.) XLMO(:,JLAYER) = XUNDEF
  ENDDO
!
!
!* computes tendencies on wind and Tke due to canopy
 CALL TEB_CANOPY(KI,NLVL,XZ,XZF,XDZ,XDZF,ZAVG_BLD,ZAVG_BLD_HEIGHT,ZAVG_WALL_O_HOR,     &
                PPA,PRHOA,XU,                                                         &
                ZAVG_DUWDU_GRND, ZAVG_UW_ROOF, ZAVG_DUWDU_ROOF,                       &
                ZAVG_H_WALL,ZAVG_H_ROOF,ZAVG_E_ROOF,ZAVG_AC_GRND,ZAVG_AC_GRND_WAT,    &
                ZFORC_U,ZDFORC_UDU,ZFORC_E,ZDFORC_EDE,ZFORC_T,ZDFORC_TDT,ZFORC_Q,ZDFORC_QDQ)
!
!* computes coefficients for implicitation
  CALL CANOPY_EVOL(KI,NLVL,PTSTEP,1,                         &
                     ZL,ZWIND,PTA,PQA,PPA,PRHOA,             &
                     ZSFLUX_U,ZSFLUX_T,ZSFLUX_Q,             &
                     ZFORC_U,ZDFORC_UDU,ZFORC_E,ZDFORC_EDE,  &
                     ZFORC_T,ZDFORC_TDT,ZFORC_Q,ZDFORC_QDQ,  &
                     XZ,XZF,XDZ,XDZF,XU,XTKE,XT,XQ,XLMO,     &
                     XLM,XLEPS,XP,ZAVG_USTAR,                &
                     ZALFAU,ZBETAU,ZALFAT,ZBETAT,ZALFAQ,ZBETAQ)
!
  ZPEW_A_COEF_LOWCAN = - ZALFAU / PRHOA
  ZPEW_B_COEF_LOWCAN = ZBETAU  
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
    ZU_CANYON(JI) = ZWAKE(JI) * EXP(-ZAVG_CAN_HW_RATIO(JI)/4.) * ZWIND(JI)     &
              * LOG( (           2.* ZAVG_BLD_HEIGHT(JI)/3.) / ZAVG_Z0_TOWN(JI))   &
              / LOG( (PUREF(JI)+ 2.* ZAVG_BLD_HEIGHT(JI)/3.) / ZAVG_Z0_TOWN(JI))
    ZZ_LOWCAN(JI) = ZAVG_BLD_HEIGHT(JI) / 2.
   ELSE
    ZU_CANYON(JI) = ZWIND(JI)
    ZZ_LOWCAN(JI) = PZREF(JI)
   ENDIF
 END DO
!
!* Without SBL scheme, canyon air is assumed at mid height
  ZU_LOWCAN=ZU_CANYON
  ZT_LOWCAN=XT_CANYON
  ZQ_LOWCAN=XQ_CANYON
  ZT_CANYON=XT_CANYON
  ZQ_CANYON=XQ_CANYON
  ZUREF    =PUREF
  ZZREF    =PZREF
  ZTA      =PTA
  ZUA      =ZWIND
  ZPA      =PPA
  ZPEW_A_COEF_LOWCAN =  0.
  ZPEW_B_COEF_LOWCAN =  ZU_CANYON
END IF
!
! Exner functions
!
ZEXNS     (:) = (PPS(:)/XP00)**(XRD/XCPD)
ZEXNA     (:) = (ZPA(:)/XP00)**(XRD/XCPD)

!--------------------------------------------------------------------------------------
! Over Urban surfaces/towns:
!--------------------------------------------------------------------------------------
!
DO JTEB_PATCH=1,NTEB_PATCH
 CALL GOTO_TEB(JTEB_PATCH)
!
ZT_CAN=ZT_CANYON
ZQ_CAN=ZQ_CANYON
!
IF (LCANOPY) THEN
  XT_CANYON(:) = ZT_CANYON(:)
  XQ_CANYON(:) = ZQ_CANYON(:)
END IF
!
ZLESNOW_ROOF(:) = 0.
ZLESNOW_ROAD(:) = 0.
ZG_GREENROOF_ROOF(:) = 0.
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Reinitialize shading of windows when changing day
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (CBEM=='BEM') &
WHERE (PTSUN .LT. PTSTEP + 1E-3) LSHAD_DAY(:) = .FALSE.
!
!
 CALL TEB_GARDEN      (LGARDEN, LGREENROOF, CZ0H, CIMPLICIT_WIND, CROAD_DIR, CWALL_OPT,&
                      TTIME, PTSUN, ZT_CAN, ZQ_CAN, ZU_CANYON,                         &
                      ZT_LOWCAN, ZQ_LOWCAN, ZU_LOWCAN, ZZ_LOWCAN,                      &
                      XTI_BLD,                                                         &
                      XT_ROOF, XT_ROAD, XT_WALL_A, XT_WALL_B, XWS_ROOF,XWS_ROAD,       &
                      TSNOW_ROOF%SCHEME,                                               &
                      TSNOW_ROOF%WSNOW(:,:,1), TSNOW_ROOF%T(:,:,1),                    &
                      TSNOW_ROOF%RHO(:,:,1), TSNOW_ROOF%ALB(:,1),                      &
                      TSNOW_ROOF%TS(:,1), TSNOW_ROOF%EMIS(:,1),                        &
                      TSNOW_ROAD%SCHEME,                                               &
                      TSNOW_ROAD%WSNOW(:,:,1), TSNOW_ROAD%T(:,:,1),                    &
                      TSNOW_ROAD%RHO(:,:,1), TSNOW_ROAD%ALB(:,1),                      &
                      TSNOW_ROAD%TS(:,1), TSNOW_ROAD%EMIS(:,1),                        &
                      ZPEW_A_COEF, ZPEW_B_COEF,                                        &
                      ZPEW_A_COEF_LOWCAN, ZPEW_B_COEF_LOWCAN,                          &
                      PPS, ZPA, ZEXNS, ZEXNA, ZTA, ZQA, PRHOA, PCO2,                   &
                      PLW, ZDIR_SWB, ZSCA_SWB, PSW_BANDS, KSW, PZENITH, PAZIM,         &
                      PRAIN, PSNOW, ZZREF, ZUREF, ZUA,                                 &
                      ZH_TRAFFIC, ZLE_TRAFFIC, XH_INDUSTRY, XLE_INDUSTRY,              &
                      PTSTEP,                                                          &
                      XZ0_TOWN,                                                        &
                      XBLD, XGARDEN, XROAD_DIR, XROAD, XGREENROOF,                     &
                      XBLD_HEIGHT, XWALL_O_HOR, XCAN_HW_RATIO,                         &
                      XROAD_O_GRND, XGARDEN_O_GRND, XWALL_O_GRND,                      &
                      XALB_ROOF, XEMIS_ROOF,                                           &
                      XHC_ROOF,XTC_ROOF,XD_ROOF,                                       &
                      XALB_ROAD, XEMIS_ROAD, XSVF_ROAD,                                &
                      XHC_ROAD,XTC_ROAD,XD_ROAD,                                       &
                      XALB_WALL, XEMIS_WALL, XSVF_WALL,                                &
                      XSVF_GARDEN,                                                     &
                      XHC_WALL,XTC_WALL,XD_WALL,                                       &
                      ZRN_ROOF, ZH_ROOF, ZLE_ROOF, ZLEW_ROOF, ZGFLUX_ROOF,             &
                      ZRUNOFF_ROOF,                                                    &
                      ZRN_ROAD, ZH_ROAD, ZLE_ROAD, ZLEW_ROAD, ZGFLUX_ROAD,             &
                      ZRUNOFF_ROAD,                                                    &
                      ZRN_WALL_A, ZH_WALL_A, ZLE_WALL_A, ZGFLUX_WALL_A,                &
                      ZRN_WALL_B, ZH_WALL_B, ZLE_WALL_B, ZGFLUX_WALL_B,                &
                      ZRN_GARDEN,ZH_GARDEN,ZLE_GARDEN,ZGFLUX_GARDEN,                   &
                      ZRN_GREENROOF,ZH_GREENROOF,ZLE_GREENROOF,ZGFLUX_GREENROOF,       &
                      ZRN_STRLROOF,ZH_STRLROOF,ZLE_STRLROOF,ZGFLUX_STRLROOF,           &
                      ZRN_BLT,ZH_BLT,ZLE_BLT,ZGFLUX_BLT,                               &
                      ZRNSNOW_ROOF, ZHSNOW_ROOF, ZLESNOW_ROOF, ZGSNOW_ROOF,            &
                      ZMELT_ROOF,                                                      &
                      ZRNSNOW_ROAD, ZHSNOW_ROAD, ZLESNOW_ROAD, ZGSNOW_ROAD,            &
                      ZMELT_ROAD,                                                      &
                      ZRN_GRND, ZH_GRND, ZLE_GRND, ZGFLUX_GRND,                        &
                      ZRN, ZH, ZLE, ZGFLUX, ZEVAP, ZRUNOFF, ZSFCO2,                    &
                      ZUW_GRND, ZUW_ROOF, ZDUWDU_GRND, ZDUWDU_ROOF,                    &
                      ZUSTAR, ZCD, ZCDN, ZCH, ZRI,                                     &
                      ZTRAD, ZEMIS, ZDIR_ALB, ZSCA_ALB, ZRESA_TOWN, ZDQS_TOWN,         &
                      ZQF_TOWN, ZQF_BLD,                                               &
                      ZFLX_BLD, ZAC_ROAD, ZAC_GARDEN, ZAC_GREENROOF,                   &
                      ZAC_ROAD_WAT, ZAC_GARDEN_WAT, ZAC_GREENROOF_WAT,                 &
                      ZABS_SW_ROOF,ZABS_LW_ROOF,                                       &
                      ZABS_SW_SNOW_ROOF,ZABS_LW_SNOW_ROOF,                             &
                      ZABS_SW_ROAD,ZABS_LW_ROAD,                                       &
                      ZABS_SW_SNOW_ROAD,ZABS_LW_SNOW_ROAD,                             &
                      ZABS_SW_WALL_A, ZABS_LW_WALL_A,                                  &
                      ZABS_SW_WALL_B, ZABS_LW_WALL_B,                                  &
                      ZABS_SW_GARDEN,ZABS_LW_GARDEN,                                   &
                      ZABS_SW_GREENROOF,ZABS_LW_GREENROOF, ZG_GREENROOF_ROOF,          &
                      ZRUNOFF_GREENROOF, ZDRAIN_GREENROOF,                             &
                      CCOOL_COIL, XF_WATER_COND, CHEAT_COIL, CNATVENT,                 &
                      KDAY, XAUX_MAX, XT_FLOOR, XT_MASS, ZH_BLD_COOL,                  &
                      ZT_BLD_COOL, ZH_BLD_HEAT, ZLE_BLD_COOL, ZLE_BLD_HEAT, ZH_WASTE,  &
                      ZLE_WASTE, XF_WASTE_CAN, ZHVAC_COOL, ZHVAC_HEAT, XQIN, XQIN_FRAD,&
                      XQIN_FLAT, XGR, XEFF_HEAT, XINF, XTCOOL_TARGET,                  &
                      XTHEAT_TARGET, XHR_TARGET, XT_WIN2, XQI_BLD, XV_VENT,            &
                      XCAP_SYS_HEAT, XCAP_SYS_RAT, XT_ADP, XM_SYS_RAT, XCOP_RAT,       &
                      ZCAP_SYS, ZM_SYS, ZCOP, ZQ_SYS, ZT_SYS, ZTR_SW_WIN, ZFAN_POWER,  &
                      XHC_FLOOR, XTC_FLOOR, XD_FLOOR, XT_WIN1, ZABS_SW_WIN,            &
                      ZABS_LW_WIN, XSHGC, XSHGC_SH, XUGG_WIN, XALB_WIN, XABS_WIN,      &
                      ZEMIT_LW_FAC, ZEMIT_LW_GRND, ZT_RAD_IND, ZREF_SW_GRND,           &
                      ZREF_SW_FAC, ZHU_BLD, PTIME, LSHADE, LSHAD_DAY, LNATVENT_NIGHT,  &
                      CBEM, XN_FLOOR, XWALL_O_BLD, XGLAZ_O_BLD, XMASS_O_BLD,           &
                      XFLOOR_HW_RATIO,                                                 &
                      XF_FLOOR_MASS, XF_FLOOR_WALL, XF_FLOOR_WIN,                      &
                      XF_FLOOR_ROOF, XF_WALL_FLOOR, XF_WALL_MASS,                      &
                      XF_WALL_WIN, XF_WIN_FLOOR, XF_WIN_MASS, XF_WIN_WALL,             &
                      XF_MASS_FLOOR, XF_MASS_WALL, XF_MASS_WIN, LCANOPY, XTRAN_WIN,    &
                      CCH_BEM, XROUGH_ROOF, XROUGH_WALL, XF_WIN_WIN                    )


!
IF (.NOT. LCANOPY) THEN
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_T_CANYON,ZT_CAN)
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_Q_CANYON,ZQ_CAN)
!
! Momentum fluxes
!
  ZSFU = 0.
  ZSFV = 0.
  DO JJ=1,SIZE(PU)
    IF (ZWIND(JJ)>0.) THEN
      ZCOEF(JJ) = - PRHOA(JJ) * ZUSTAR(JJ)**2 / ZWIND(JJ)
      ZSFU(JJ) = ZCOEF(JJ) * PU(JJ)
      ZSFV(JJ) = ZCOEF(JJ) * PV(JJ)
    ENDIF
  ENDDO
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,PSFU,ZSFU)
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,PSFV,ZSFV)
!
ENDIF
!
!-------------------------------------------------------------------------------------
! Outputs:
!-------------------------------------------------------------------------------------
!
! Grid box average fluxes/properties: Arguments and standard diagnostics
!
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,PSFTH,ZH)
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,PSFTQ,ZEVAP)
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,PSFCO2,ZSFCO2)
!
!
! Albedo for each wavelength and patch
!
DO JSWB=1,SIZE(PSW_BANDS)
  DO JJ=1,SIZE(ZDIR_ALB)
    ZDIR_ALB_PATCH(JJ,JSWB,JTEB_PATCH) = ZDIR_ALB(JJ)
    ZSCA_ALB_PATCH(JJ,JSWB,JTEB_PATCH) = ZSCA_ALB(JJ)
  ENDDO
END DO
!
! emissivity and radiative temperature
!
ZEMIS_PATCH(:,JTEB_PATCH) = ZEMIS
ZTRAD_PATCH(:,JTEB_PATCH) = ZTRAD
!
! computes some aggregated diagnostics
!
 IF (.NOT. LCANOPY) THEN
   CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_CD ,ZCD )
   CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_CDN,ZCDN)
   CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_RI ,ZRI )
 ENDIF
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_CH ,ZCH )
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_RN ,ZRN )
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_H  ,ZH  )
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_LE ,ZLE )
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_GFLUX ,ZGFLUX )
!
!* warning: aerodynamical resistance does not yet take into account gardens
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_RESA_TOWN,1./ZRESA_TOWN)
IF (JTEB_PATCH==NTEB_PATCH) ZAVG_RESA_TOWN = 1./ZAVG_RESA_TOWN
!
!-------------------------------------------------------------------------------------
! Diagnostics on each patch
!-------------------------------------------------------------------------------------
!
 CALL DIAG_MISC_TEB_n(PTSTEP, ZDQS_TOWN, ZQF_BLD, ZQF_TOWN, ZFLX_BLD,           &
                     ZRN_ROAD, ZH_ROAD, ZLE_ROAD, ZGFLUX_ROAD,                 &
                     ZRN_WALL_A, ZH_WALL_A, ZGFLUX_WALL_A,                     &
                     ZRN_WALL_B, ZH_WALL_B, ZGFLUX_WALL_B,                     &
                     ZRN_ROOF, ZH_ROOF, ZLE_ROOF, ZGFLUX_ROOF, ZRUNOFF,        &
                     ZRN_STRLROOF, ZH_STRLROOF, ZLE_STRLROOF, ZGFLUX_STRLROOF, &
                     ZRN_GREENROOF, ZH_GREENROOF,                              &
                     ZLE_GREENROOF, ZGFLUX_GREENROOF, ZG_GREENROOF_ROOF,       &
                     ZRUNOFF_GREENROOF, ZDRAIN_GREENROOF,                      &
                     ZRN_GARDEN,ZH_GARDEN,ZLE_GARDEN,ZGFLUX_GARDEN,            &
                     ZRN_BLT,ZH_BLT,ZLE_BLT,ZGFLUX_BLT,                        &
                     ZABS_SW_ROOF,ZABS_LW_ROOF,                                &
                     ZABS_SW_SNOW_ROOF,ZABS_LW_SNOW_ROOF,                      &
                     ZABS_SW_ROAD,ZABS_LW_ROAD,                                &
                     ZABS_SW_SNOW_ROAD,ZABS_LW_SNOW_ROAD,                      &
                     ZABS_SW_WALL_A, ZABS_LW_WALL_A, ZABS_SW_WALL_B,           &
                     ZABS_LW_WALL_B,                                           &
                     ZABS_SW_GARDEN,ZABS_LW_GARDEN,                            &
                     ZABS_SW_GREENROOF,ZABS_LW_GREENROOF,                      &
                     ZH_BLD_COOL, ZT_BLD_COOL,                                 &     
                     ZH_BLD_HEAT, ZLE_BLD_COOL, ZLE_BLD_HEAT,                  &
                     ZH_WASTE, ZLE_WASTE, ZHVAC_COOL,                          &
                     ZHVAC_HEAT, ZCAP_SYS, ZM_SYS, ZCOP,                       &
                     ZQ_SYS, ZT_SYS, ZTR_SW_WIN, ZFAN_POWER,                   &
                     ZABS_SW_WIN, ZABS_LW_WIN                                  )
!
!
!-------------------------------------------------------------------------------------
! Computes averaged parameters necessary for UTCI
!-------------------------------------------------------------------------------------
!
IF (N2M >0 .AND. LUTCI) THEN
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_REF_SW_GRND ,ZREF_SW_GRND )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_REF_SW_FAC  ,ZREF_SW_FAC  )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_SCA_SW      ,ZSCA_SW      )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_DIR_SW      ,ZDIR_SW      )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_EMIT_LW_FAC ,ZEMIT_LW_FAC )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_EMIT_LW_GRND,ZEMIT_LW_GRND)
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_T_RAD_IND   ,ZT_RAD_IND   )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_TI_BLD      ,XTI_BLD      )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_QI_BLD      ,XQI_BLD      )
END IF
!
!-------------------------------------------------------------------------------------
! Use of the canopy version of TEB
!-------------------------------------------------------------------------------------
!
IF (LCANOPY) THEN
!-------------------------------------------------------------------------------------
! Town averaged quantities to force canopy atmospheric layers
!-------------------------------------------------------------------------------------

 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_DUWDU_GRND ,ZDUWDU_GRND )
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_UW_ROOF ,ZUW_ROOF)
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_DUWDU_ROOF ,ZDUWDU_ROOF)
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_H_WALL ,0.5*(ZH_WALL_A+ZH_WALL_B))
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_H_ROOF ,(ZH_ROOF+XH_INDUSTRY))
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_E_ROOF ,(ZLE_ROOF+XLE_INDUSTRY)/XLVTT)
!
!-------------------------------------------------------------------------------------
! Computes the impact of canopy and surfaces on air
!-------------------------------------------------------------------------------------
!
ZAC_GRND    (:) = (XROAD(:)*ZAC_ROAD    (:) + XGARDEN(:)*ZAC_GARDEN    (:)) / (XROAD(:)+XGARDEN(:))
ZAC_GRND_WAT(:) = (XROAD(:)*ZAC_ROAD_WAT(:) + XGARDEN(:)*ZAC_GARDEN_WAT(:)) / (XROAD(:)+XGARDEN(:))
!
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_AC_GRND     ,ZAC_GRND    )
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_AC_GRND_WAT ,ZAC_GRND_WAT)
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZSFLUX_U ,ZUW_GRND * (1.-XBLD))
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZSFLUX_T ,ZH_GRND  * (1.-XBLD)/XCPD/PRHOA)
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZSFLUX_Q ,ZLE_GRND * (1.-XBLD)/XLVTT)
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
IF (LCANOPY) THEN
!
!-------------------------------------------------------------------------------------
!* Impact of TEB fluxes on the air
!-------------------------------------------------------------------------------------
!
 CALL TEB_CANOPY(KI,NLVL,XZ,XZF,XDZ,XDZF,ZAVG_BLD,ZAVG_BLD_HEIGHT,ZAVG_WALL_O_HOR,     &
                PPA,PRHOA,XU,                                                         &
                ZAVG_DUWDU_GRND, ZAVG_UW_ROOF, ZAVG_DUWDU_ROOF,                       &
                ZAVG_H_WALL,ZAVG_H_ROOF,ZAVG_E_ROOF,ZAVG_AC_GRND,ZAVG_AC_GRND_WAT,    &
                ZFORC_U,ZDFORC_UDU,ZFORC_E,ZDFORC_EDE,ZFORC_T,ZDFORC_TDT,ZFORC_Q,ZDFORC_QDQ)
!
!-------------------------------------------------------------------------------------
!* Evolution of canopy air due to these impacts
!-------------------------------------------------------------------------------------
!
 CALL CANOPY_EVOL(KI,NLVL,PTSTEP,2,                                            &
                 ZL,ZWIND,PTA,PQA,PPA,PRHOA,                                  &
                 ZSFLUX_U,ZSFLUX_T,ZSFLUX_Q,                                  &
                 ZFORC_U,ZDFORC_UDU,ZFORC_E,ZDFORC_EDE,                       &
                 ZFORC_T,ZDFORC_TDT,ZFORC_Q,ZDFORC_QDQ,                       &
                 XZ,XZF,XDZ,XDZF,XU,XTKE,XT,XQ,XLMO,XLM,XLEPS,XP,             &
                 ZAVG_USTAR,                                                  &
                 ZALFAU,ZBETAU,ZALFAT,ZBETAT,ZALFAQ,ZBETAQ                    )
!
!
!-------------------------------------------------------------------------------------
! Momentum fluxes in the case canopy is active
!-------------------------------------------------------------------------------------
!
PSFU=0.
PSFV=0.
ZAVG_Z0_TOWN(:) = MIN(ZAVG_Z0_TOWN(:),PUREF(:)*0.5)
ZAVG_CDN=(XKARMAN/LOG(PUREF(:)/ZAVG_Z0_TOWN(:)))**2
ZAVG_CD = ZAVG_CDN
ZAVG_RI = 0.
DO JJ=1,SIZE(PU)
  IF (ZWIND(JJ)>0.) THEN
    ZCOEF(JJ) = - PRHOA(JJ) * ZAVG_USTAR(JJ)**2 / ZWIND(JJ)
    PSFU(JJ) = ZCOEF(JJ) * PU(JJ)
    PSFV(JJ) = ZCOEF(JJ) * PV(JJ)
    ZAVG_CD(JJ) = ZAVG_USTAR(JJ)**2 / ZWIND(JJ)**2
    ZAVG_RI(JJ) = -XG/PTA(JJ)*ZSFLUX_T(JJ)/ZAVG_USTAR(JJ)**4
  ENDIF
ENDDO
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
! Albedo, Emissivity and fraction at time t+1
!
 CALL AVERAGE_RAD(XTEB_PATCH,                                              &
                 ZDIR_ALB_PATCH, ZSCA_ALB_PATCH, ZEMIS_PATCH, ZTRAD_PATCH,&
                 PDIR_ALB,       PSCA_ALB,       PEMIS,       PTRAD       )

!
!-------------------------------------------------------------------------------------
! Scalar fluxes:
!-------------------------------------------------------------------------------------
!
ZAVG_USTAR    (:) = SQRT(SQRT(PSFU**2+PSFV**2))
!
!
IF (NBEQ>0) THEN
  IF (CCH_DRY_DEP == "WES89") THEN
    CALL CH_DEP_TOWN(ZAVG_RESA_TOWN,  ZAVG_USTAR, PTA, PTRAD, ZAVG_WALL_O_HOR,&
                     PSV(:,NSV_CHSBEG:NSV_CHSEND),        &
                     CSV(NSV_CHSBEG:NSV_CHSEND),             &
                     XDEP(:,1:NBEQ)  )
   
    DO JI=NSV_CHSBEG,NSV_CHSEND
!cdir nodep
      DO JJ=1,SIZE(PSFTS,1)
        PSFTS(JJ,JI) = - PSV(JJ,JI) * XDEP(JJ,JI-NSV_CHSBEG+1)
      ENDDO
    ENDDO

    IF (NAEREQ > 0 ) THEN
      CALL CH_AER_DEP(PSV(:,NSV_AERBEG:NSV_AEREND),&
                         PSFTS(:,NSV_AERBEG:NSV_AEREND),&
                         ZAVG_USTAR,ZAVG_RESA_TOWN,PTA,PRHOA)   
    END IF

  ELSE
    DO JI=NSV_CHSBEG,NSV_CHSEND
      PSFTS(:,JI) =0.
    ENDDO
    IF(NSV_AERBEG.LT.NSV_AEREND) THEN
      DO JI=NSV_AERBEG,NSV_AEREND
        PSFTS(:,JI) =0.
      ENDDO
    ENDIF
  ENDIF
ENDIF

IF (NDSTEQ>0) THEN
  CALL DSLT_DEP(PSV(:,NSV_DSTBEG:NSV_DSTEND), PSFTS(:,NSV_DSTBEG:NSV_DSTEND),   &
                ZUSTAR, ZRESA_TOWN, PTA, PRHOA, XEMISSIG_DST, XEMISRADIUS_DST,  &
                JPMODE_DST, XDENSITY_DST, XMOLARWEIGHT_DST, ZCONVERTFACM0_DST,  &
                ZCONVERTFACM6_DST, ZCONVERTFACM3_DST, LVARSIG_DST, LRGFIX_DST,  &
                CVERMOD  )  

  CALL MASSFLUX2MOMENTFLUX(         &
    PSFTS(:,NSV_DSTBEG:NSV_DSTEND), & !I/O ![kg/m2/sec] In: flux of only mass, out: flux of moments
    PRHOA,                          & !I [kg/m3] air density
    XEMISRADIUS_DST,                &!I [um] emitted radius for the modes (max 3)
    XEMISSIG_DST,                   &!I [-] emitted sigma for the different modes (max 3)
    NDSTMDE,                        &
    ZCONVERTFACM0_DST,              &
    ZCONVERTFACM6_DST,              &
    ZCONVERTFACM3_DST,              &
    LVARSIG_DST, LRGFIX_DST         )  
ENDIF
IF (NSLTEQ>0) THEN
  CALL DSLT_DEP(PSV(:,NSV_SLTBEG:NSV_SLTEND), PSFTS(:,NSV_SLTBEG:NSV_SLTEND),   &
                ZUSTAR, ZRESA_TOWN, PTA, PRHOA, XEMISSIG_SLT, XEMISRADIUS_SLT,  &
                JPMODE_SLT, XDENSITY_SLT, XMOLARWEIGHT_SLT, ZCONVERTFACM0_SLT,  &
                ZCONVERTFACM6_SLT, ZCONVERTFACM3_SLT, LVARSIG_SLT, LRGFIX_SLT,  &
                CVERMOD  )  

  CALL MASSFLUX2MOMENTFLUX(         &
    PSFTS(:,NSV_SLTBEG:NSV_SLTEND), & !I/O ![kg/m2/sec] In: flux of only mass, out: flux of moments
    PRHOA,                          & !I [kg/m3] air density
    XEMISRADIUS_SLT,                &!I [um] emitted radius for the modes (max 3)
    XEMISSIG_SLT,                   &!I [-] emitted sigma for the different modes (max 3)
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
 CALL DIAG_INLINE_TEB_n(LCANOPY, PTA, PTRAD, ZQA, PPA, PPS, PRHOA,              &
                       PU, PV, ZWIND, PZREF, PUREF,                            &
                       ZAVG_CD, ZAVG_CDN, ZAVG_RI, ZAVG_CH, ZAVG_Z0_TOWN,      &
                       PTRAD, PEMIS, PDIR_ALB, PSCA_ALB,                       &
                       PLW, ZDIR_SWB, ZSCA_SWB,                                  &
                       PSFTH, PSFTQ, PSFU, PSFV, PSFCO2,                       &
                       ZAVG_RN, ZAVG_H, ZAVG_LE, ZAVG_GFLUX                    )
!
!-------------------------------------------------------------------------------------
! Stores Canyon air and humidity if historical option of TEB is active
!-------------------------------------------------------------------------------------
!
IF (.NOT. LCANOPY) THEN
  DO JTEB_PATCH=1,NTEB_PATCH
    CALL GOTO_TEB(JTEB_PATCH)
    XT_CANYON(:) = ZAVG_T_CANYON(:)
    XQ_CANYON(:) = ZAVG_Q_CANYON(:)
  END DO
END IF
!          
!-------------------------------------------------------------------------------------
! Thermal confort index
!-------------------------------------------------------------------------------------
!
IF (LUTCI .AND. N2M >0) THEN
  DO JJ=1,KI
    IF (XZON10M(JJ)/=XUNDEF) THEN
      ZU_UTCI(JJ) = SQRT(XZON10M(JJ)**2+XMER10M(JJ)**2)
    ELSE
      ZU_UTCI(JJ) = ZWIND(JJ)
    ENDIF
  ENDDO
 CALL UTCI_TEB(XT_CANYON, XQ_CANYON, ZAVG_TI_BLD, ZAVG_QI_BLD, ZU_UTCI, PPS, ZAVG_REF_SW_GRND,&
     ZAVG_REF_SW_FAC, ZAVG_SCA_SW, ZAVG_DIR_SW, PZENITH, ZAVG_EMIT_LW_FAC, ZAVG_EMIT_LW_GRND, PLW,   &
     ZAVG_T_RAD_IND, XBLD, XBLD_HEIGHT, XWALL_O_HOR, XUTCI_IN, XUTCI_OUTSUN,         &
     XUTCI_OUTSHADE, XTRAD_SUN, XTRAD_SHADE                                      )       
ELSE IF (LUTCI) THEN
  XUTCI_IN(:) = XUNDEF
  XUTCI_OUTSUN(:) = XUNDEF
  XUTCI_OUTSHADE(:) = XUNDEF
  XTRAD_SUN(:) = XUNDEF
  XTRAD_SHADE(:) = XUNDEF
ENDIF

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
IF (JTEB_PATCH==1) PAVG = 0.
PAVG = PAVG + XTEB_PATCH(:,JP) * PFIELD(:)
!
END SUBROUTINE ADD_PATCH_CONTRIB
!-------------------------------------------------------------------------------------
!
END SUBROUTINE COUPLING_TEB_n


