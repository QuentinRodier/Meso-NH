!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE COUPLING_TEB_n (DTCO, DTI, IG, I, DST, SLT, TM, GDM, GRM, &
                           HPROGRAM, HCOUPLING,                                             &
               PTSTEP, KYEAR, KMONTH, KDAY, PTIME, KI, KSV, KSW, PTSUN, PZENITH, PAZIM,    &
               PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV,                 &
               PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA,                   &
               PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,                                    &
               PTRAD, PDIR_ALB, PSCA_ALB, PEMIS, PTSURF, PZ0, PZ0H, PQSURF,                &
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
!!      V. Masson   08/2013 adds solar panels & occupation calendar
!!      B. Decharme  04/2013 new coupling variables
!!---------------------------------------------------------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_DST_n, ONLY : DST_t
USE MODD_SLT_n, ONLY : SLT_t
USE MODD_SURFEX_n, ONLY : TEB_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
!
USE MODD_REPROD_OPER, ONLY : CIMPLICIT_WIND
!
USE MODD_CSTS,         ONLY : XRD, XCPD, XP00, XLVTT, XPI, XKARMAN, XG
USE MODD_SURF_PAR,     ONLY : XUNDEF
!
!                              
USE MODD_DST_SURF
USE MODD_SLT_SURF
!
!
USE MODE_DSLT_SURF
USE MODE_THERMOS
USE MODE_SBLS
!
USE MODI_GOTO_WRAPPER_TEB_PATCH
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
USE MODI_UTCIC_STRESS
USE MODI_CIRCUMSOLAR_RAD
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(DST_t), INTENT(INOUT) :: DST
TYPE(SLT_t), INTENT(INOUT) :: SLT
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
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
REAL, DIMENSION(KI)  :: ZIRRIG_ROAD ! water supply from road summer watering
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
REAL, DIMENSION(KI)  :: ZRUNOFF_GARDEN!runoff over green areas
REAL, DIMENSION(KI)  :: ZDRAIN_GARDEN !drainage over green areas
REAL, DIMENSION(KI)  :: ZIRRIG_GARDEN !water supply from garden summer irrigation 
REAL, DIMENSION(KI)  :: ZRN_GREENROOF! net radiation on green roofs
REAL, DIMENSION(KI)  :: ZH_GREENROOF ! sensible heat flux on green roofs
REAL, DIMENSION(KI)  :: ZLE_GREENROOF! latent heat flux on green roofs
REAL, DIMENSION(KI)  :: ZGFLUX_GREENROOF    ! storage flux in green roofs
REAL, DIMENSION(KI)  :: ZG_GREENROOF_ROOF   ! heat flux between base of greenroof
REAL, DIMENSION(KI)  :: ZRUNOFF_GREENROOF   ! water runoff from green roof
REAL, DIMENSION(KI)  :: ZDRAIN_GREENROOF    ! water drainage from green roof
REAL, DIMENSION(KI)  :: ZIRRIG_GREENROOF    ! water supply from green roof summer irrigation 
REAL, DIMENSION(KI)  :: ZRN_STRLROOF        ! net radiation on structural roof
REAL, DIMENSION(KI)  :: ZH_STRLROOF         ! sensible heat flux on structural roof
REAL, DIMENSION(KI)  :: ZLE_STRLROOF        ! latent heat flux on structural roof
REAL, DIMENSION(KI)  :: ZGFLUX_STRLROOF     ! storage flux in structural roof
REAL, DIMENSION(KI)  :: ZRUNOFF_STRLROOF    ! runoff over structural roof
REAL, DIMENSION(KI)  :: ZH_PANEL    ! sensible heat flux on solar panel
REAL, DIMENSION(KI)  :: ZTHER_PROD_PANEL ! thermal      energy production from solar panel (W/m2 panel)
REAL, DIMENSION(KI)  :: ZPHOT_PROD_PANEL ! photovoltaic energy production from solar panel (W/m2 panel)
REAL, DIMENSION(KI)  :: ZPROD_PANEL      ! averaged     energy production from solar panel (W/m2 panel)
REAL, DIMENSION(KI)  :: ZTHER_PROD_BLD   ! thermal      energy production from solar panel (W/m2 bld)
REAL, DIMENSION(KI)  :: ZPHOT_PROD_BLD   ! photovoltaic energy production from solar panel (W/m2 bld)
REAL, DIMENSION(KI)  :: ZPROD_BLD        ! averaged     energy production from solar panel (W/m2 bld)
REAL, DIMENSION(KI)  :: ZRN_PANEL   ! net radiation of solar panel
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
REAL, DIMENSION(KI,TM%TOP%NTEB_PATCH) :: ZTRAD_PATCH ! radiative temperature for each patch
REAL, DIMENSION(KI,TM%TOP%NTEB_PATCH) :: ZEMIS_PATCH ! emissivity for each patch
REAL, DIMENSION(KI,KSW,TM%TOP%NTEB_PATCH) :: ZDIR_ALB_PATCH ! direct albedo per wavelength and patch
REAL, DIMENSION(KI,KSW,TM%TOP%NTEB_PATCH) :: ZSCA_ALB_PATCH ! diffuse albedo per wavelength and patch
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
REAL, DIMENSION(KI)  :: ZRUNOFF_TOWN  ! runoff over the ground
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
REAL, DIMENSION(KI,TM%TCP%NLVL)   :: ZFORC_U   ! tendency due to drag force for wind
REAL, DIMENSION(KI,TM%TCP%NLVL)   :: ZDFORC_UDU! formal derivative of
!                                              ! tendency due to drag force for wind
REAL, DIMENSION(KI,TM%TCP%NLVL)   :: ZFORC_E   ! tendency due to drag force for TKE
REAL, DIMENSION(KI,TM%TCP%NLVL)   :: ZDFORC_EDE! formal derivative of
!                                              ! tendency due to drag force for TKE
REAL, DIMENSION(KI,TM%TCP%NLVL)   :: ZFORC_T   ! tendency due to drag force for Temp
REAL, DIMENSION(KI,TM%TCP%NLVL)   :: ZDFORC_TDT! formal derivative of
!                                              ! tendency due to drag force for Temp
REAL, DIMENSION(KI,TM%TCP%NLVL)   :: ZFORC_Q   ! tendency due to drag force for hum
REAL, DIMENSION(KI,TM%TCP%NLVL)   :: ZDFORC_QDQ! formal derivative of
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
REAL, DIMENSION(KI,TM%TCP%NLVL)   :: ZL         ! Mixing length generic profile at mid levels
!
! absorbed solar and infra-red radiation by road, wall and roof
!                                                      
REAL, DIMENSION(KI) :: ZABS_SW_ROAD
REAL, DIMENSION(KI) :: ZABS_SW_WALL_A
REAL, DIMENSION(KI) :: ZABS_SW_WALL_B
REAL, DIMENSION(KI) :: ZABS_SW_ROOF
REAL, DIMENSION(KI) :: ZABS_SW_GARDEN
REAL, DIMENSION(KI) :: ZABS_SW_GREENROOF
REAL, DIMENSION(KI) :: ZABS_SW_PANEL
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
REAL, DIMENSION(KI) :: ZABS_LW_PANEL
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
! occupation of buildings
REAL, DIMENSION(SIZE(PTA)) :: ZCUR_TCOOL_TARGET ! Cooling target temperature at current time
REAL, DIMENSION(SIZE(PTA)) :: ZCUR_THEAT_TARGET ! Heating target temperature at current time
REAL, DIMENSION(SIZE(PTA)) :: ZCUR_QIN          ! Internal heat gains        at current time
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
TM%TOP%TTIME%TIME = TM%TOP%TTIME%TIME + PTSTEP
 CALL ADD_FORECAST_TO_DATE_SURF(TM%TOP%TTIME%TDATE%YEAR,TM%TOP%TTIME%TDATE%MONTH,&
                TM%TOP%TTIME%TDATE%DAY,TM%TOP%TTIME%TIME)
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
  ZH_TRAFFIC  (:) = TM%T%CUR%XH_TRAFFIC   (:)
  ZLE_TRAFFIC (:) = TM%T%CUR%XLE_TRAFFIC  (:)
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

DO JTEB_PATCH=1,TM%TOP%NTEB_PATCH
  CALL GOTO_WRAPPER_TEB_PATCH(TM%B, TM%DGCT, TM%DGMT, TM%T, &
                   GDM%TGD, GDM%TGDPE, GRM%TGR, GRM%TGRPE, JTEB_PATCH)
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_BLD,         TM%T%CUR%XBLD         )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_BLD_HEIGHT,  TM%T%CUR%XBLD_HEIGHT  )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_WALL_O_HOR,  TM%T%CUR%XWALL_O_HOR  )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_CAN_HW_RATIO,TM%T%CUR%XCAN_HW_RATIO)
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_Z0_TOWN,     TM%T%CUR%XZ0_TOWN     )
END DO
!
IF (TM%TOP%LCANOPY) THEN
!-------------------------------------------------------------------------------------
! Updates canopy vertical grid as a function of forcing height
!-------------------------------------------------------------------------------------
!
!* determines where is the forcing level and modifies the upper levels of the canopy grid
!
  CALL CANOPY_GRID_UPDATE(KI,TM%TCP%NLVL,ZAVG_BLD_HEIGHT,ZAVG_BLD_HEIGHT+PUREF,&
                          TM%TCP%XZ,TM%TCP%XZF,TM%TCP%XDZ,TM%TCP%XDZF)
!
!* Initialisations of T, Q, TKE and wind at first time step
!

  IF(ANY(TM%TCP%XT(:,:) == XUNDEF)) THEN
    DO JLAYER=1,TM%TCP%NLVL
      TM%TCP%XT(:,JLAYER) = PTA(:)
      TM%TCP%XQ(:,JLAYER) = PQA(:)
      TM%TCP%XU(:,JLAYER) = 2./XPI * ZWIND(:)                                  &
              * LOG( (          2.* TM%T%CUR%XBLD_HEIGHT(:)/3.) / TM%T%CUR%XZ0_TOWN(:))   &
              / LOG( (PUREF(:)+ 2.* TM%T%CUR%XBLD_HEIGHT(:)/3.) / TM%T%CUR%XZ0_TOWN(:))
    END  DO
    TM%TCP%XTKE(:,:) = 1.
  ENDIF
!
!* default forcing above roof: forcing level
ZUREF(:)     = PUREF(:)
ZZREF(:)     = PZREF(:)
ZUA(:)       = TM%TCP%XU(:,TM%TCP%NLVL)
ZTA(:)       = TM%TCP%XT(:,TM%TCP%NLVL)
ZQA(:)       = TM%TCP%XQ(:,TM%TCP%NLVL)/PRHOA(:)
ZPA(:)       = TM%TCP%XP(:,TM%TCP%NLVL)
!* for the time being, only one value is kept for wall in-canyon forcing, in the middle of the canyon
ZU_CANYON(:) = ZUA(:)
ZT_CANYON(:) = ZTA(:)
ZQ_CANYON(:) = ZQA(:)
  DO JLAYER=1,TM%TCP%NLVL-1
    DO JI=1,KI
      !* finds middle canyon layer
      IF (TM%TCP%XZ(JI,JLAYER)<ZAVG_BLD_HEIGHT(JI)/2. .AND. &
          TM%TCP%XZ(JI,JLAYER+1)>=ZAVG_BLD_HEIGHT(JI)/2.) THEN
        ZCOEF(JI) = (ZAVG_BLD_HEIGHT(JI)/2.-TM%TCP%XZ(JI,JLAYER))/(TM%TCP%XZ(JI,JLAYER+1)-TM%TCP%XZ(JI,JLAYER))
        ZU_CANYON(JI) = TM%TCP%XU(JI,JLAYER) + ZCOEF(JI) * (TM%TCP%XU(JI,JLAYER+1)-TM%TCP%XU(JI,JLAYER))
        ZT_CANYON(JI) = TM%TCP%XT(JI,JLAYER) + ZCOEF(JI) * (TM%TCP%XT(JI,JLAYER+1)-TM%TCP%XT(JI,JLAYER))
        ZQ_CANYON(JI) =(TM%TCP%XQ(JI,JLAYER) + ZCOEF(JI) * &
                        (TM%TCP%XQ(JI,JLAYER+1)-TM%TCP%XQ(JI,JLAYER)))/PRHOA(JI)
      END IF
      !* finds layer just above roof (at least 1m above roof)
      IF (TM%TCP%XZ(JI,JLAYER)<ZAVG_BLD_HEIGHT(JI)+1. .AND. &
          TM%TCP%XZ(JI,JLAYER+1)>=ZAVG_BLD_HEIGHT(JI)+1.) THEN
        ZUREF(JI) = TM%TCP%XZ(JI,JLAYER+1) - ZAVG_BLD_HEIGHT(JI)
        ZZREF(JI) = TM%TCP%XZ(JI,JLAYER+1) - ZAVG_BLD_HEIGHT(JI)
        ZTA  (JI) = TM%TCP%XT(JI,JLAYER+1)
        ZQA  (JI) = TM%TCP%XQ(JI,JLAYER+1)/PRHOA(JI)
        !ZUA  (JI) = XU(JI,JLAYER+1)
        ZUA  (JI) = MAX(TM%TCP%XU(JI,JLAYER+1) - 2.*SQRT(TM%TCP%XTKE(JI,JLAYER+1)) , TM%TCP%XU(JI,JLAYER+1)/3.)
        ZPA  (JI) = TM%TCP%XP(JI,JLAYER+1)
        ZLMO (JI) = TM%TCP%XLMO(JI,JLAYER+1)
      END IF
    END DO
  END DO
  ZU_CANYON= MAX(ZU_CANYON,0.2)
  ZU_LOWCAN=TM%TCP%XU(:,1)
  ZT_LOWCAN=TM%TCP%XT(:,1)
  ZQ_LOWCAN=TM%TCP%XQ(:,1) / PRHOA(:)
  ZZ_LOWCAN=TM%TCP%XZ(:,1)
  WHERE(ZPA==XUNDEF) ZPA = PPA   ! security for first time step
!
!-------------------------------------------------------------------------------------
! determine the vertical profile for mixing and dissipative lengths (at full levels)
!-------------------------------------------------------------------------------------
!
! frontal density
  ZLAMBDA_F(:) = ZAVG_CAN_HW_RATIO*ZAVG_BLD / (0.5*XPI)
!
  CALL SM10(TM%TCP%XZ,ZAVG_BLD_HEIGHT,ZLAMBDA_F,ZL)
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
  DO JLAYER=1,TM%TCP%NLVL-1
      !* Monin-Obuhkov theory not used inside the urban canopy
      ! => neutral mixing  if layer is below : (roof level +1 meter)
      WHERE (TM%TCP%XZ(:,JLAYER)<=ZAVG_BLD_HEIGHT(:)+1.) TM%TCP%XLMO(:,JLAYER) = XUNDEF
  ENDDO
!
!
!* computes tendencies on wind and Tke due to canopy
 CALL TEB_CANOPY(KI,TM%TCP%NLVL,TM%TCP%XZ,TM%TCP%XZF,TM%TCP%XDZ,TM%TCP%XDZF,&
                 ZAVG_BLD,ZAVG_BLD_HEIGHT,ZAVG_WALL_O_HOR,     &
                PPA,PRHOA,TM%TCP%XU,                                                         &
                ZAVG_DUWDU_GRND, ZAVG_UW_ROOF, ZAVG_DUWDU_ROOF,                       &
                ZAVG_H_WALL,ZAVG_H_ROOF,ZAVG_E_ROOF,ZAVG_AC_GRND,ZAVG_AC_GRND_WAT,    &
                ZFORC_U,ZDFORC_UDU,ZFORC_E,ZDFORC_EDE,ZFORC_T,ZDFORC_TDT,ZFORC_Q,ZDFORC_QDQ)
!
!* computes coefficients for implicitation
  CALL CANOPY_EVOL(KI,TM%TCP%NLVL,PTSTEP,1,                  &
                     ZL,ZWIND,PTA,PQA,PPA,PRHOA,             &
                     ZSFLUX_U,ZSFLUX_T,ZSFLUX_Q,             &
                     ZFORC_U,ZDFORC_UDU,ZFORC_E,ZDFORC_EDE,  &
                     ZFORC_T,ZDFORC_TDT,ZFORC_Q,ZDFORC_QDQ,  &
                     TM%TCP%XZ,TM%TCP%XZF,TM%TCP%XDZ,TM%TCP%XDZF,TM%TCP%XU,&
                     TM%TCP%XTKE,TM%TCP%XT,TM%TCP%XQ,TM%TCP%XLMO,     &
                     TM%TCP%XLM,TM%TCP%XLEPS,TM%TCP%XP,ZAVG_USTAR,                &
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
  ZT_LOWCAN=TM%T%CUR%XT_CANYON
  ZQ_LOWCAN=TM%T%CUR%XQ_CANYON
  ZT_CANYON=TM%T%CUR%XT_CANYON
  ZQ_CANYON=TM%T%CUR%XQ_CANYON
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
DO JTEB_PATCH=1,TM%TOP%NTEB_PATCH
  CALL GOTO_WRAPPER_TEB_PATCH(TM%B, TM%DGCT, TM%DGMT, TM%T, &
                   GDM%TGD, GDM%TGDPE, GRM%TGR, GRM%TGRPE, JTEB_PATCH)
!
ZT_CAN=ZT_CANYON
ZQ_CAN=ZQ_CANYON
!
IF (TM%TOP%LCANOPY) THEN
  TM%T%CUR%XT_CANYON(:) = ZT_CANYON(:)
  TM%T%CUR%XQ_CANYON(:) = ZQ_CANYON(:)
END IF
!
ZLESNOW_ROOF(:) = 0.
ZLESNOW_ROAD(:) = 0.
ZG_GREENROOF_ROOF(:) = 0.
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Call the physical routines of TEB (including gardens & greenroofs)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL TEB_GARDEN(DTCO, DTI, IG, I, TM, GDM, GRM,  &
                 TM%TOP%LGARDEN, TM%TOP%LGREENROOF, TM%TOP%LSOLAR_PANEL,         &
                 TM%TOP%CZ0H, CIMPLICIT_WIND, TM%TOP%CROAD_DIR, TM%TOP%CWALL_OPT, &
                 TM%TOP%TTIME, PTSUN, ZT_CAN, ZQ_CAN, ZU_CANYON,                  &
                 ZT_LOWCAN, ZQ_LOWCAN, ZU_LOWCAN, ZZ_LOWCAN,                      &
                 TM%B%CUR%XTI_BLD,TM%T%CUR%XT_ROOF, TM%T%CUR%XT_ROAD,             &
                 TM%T%CUR%XT_WALL_A, TM%T%CUR%XT_WALL_B, TM%T%CUR%XWS_ROOF,       &
                 TM%T%CUR%XWS_ROAD,TM%T%CUR%TSNOW_ROOF%SCHEME,                    &
                 TM%T%CUR%TSNOW_ROOF%WSNOW(:,:,1), TM%T%CUR%TSNOW_ROOF%T(:,:,1),  &
                 TM%T%CUR%TSNOW_ROOF%RHO(:,:,1), TM%T%CUR%TSNOW_ROOF%ALB(:,1),    &
                 TM%T%CUR%TSNOW_ROOF%TS(:,1), TM%T%CUR%TSNOW_ROOF%EMIS(:,1),      &
                 TM%T%CUR%TSNOW_ROAD%SCHEME,                                      &
                 TM%T%CUR%TSNOW_ROAD%WSNOW(:,:,1), TM%T%CUR%TSNOW_ROAD%T(:,:,1),  &
                 TM%T%CUR%TSNOW_ROAD%RHO(:,:,1), TM%T%CUR%TSNOW_ROAD%ALB(:,1),    &
                 TM%T%CUR%TSNOW_ROAD%TS(:,1), TM%T%CUR%TSNOW_ROAD%EMIS(:,1),      &
                 ZPEW_A_COEF, ZPEW_B_COEF,ZPEW_A_COEF_LOWCAN, ZPEW_B_COEF_LOWCAN, &
                 PPS, ZPA, ZEXNS, ZEXNA, ZTA, ZQA, PRHOA, PCO2,                   &
                 PLW, ZDIR_SWB, ZSCA_SWB, PSW_BANDS, KSW, PZENITH, PAZIM,         &
                 PRAIN, PSNOW, ZZREF, ZUREF, ZUA,                                 &
                 ZH_TRAFFIC, ZLE_TRAFFIC, TM%T%CUR%XH_INDUSTRY, TM%T%CUR%XLE_INDUSTRY, &
                 PTSTEP,                                                          &
                 TM%T%CUR%XZ0_TOWN, TM%T%CUR%XBLD, TM%T%CUR%XGARDEN,              &
                 TM%T%CUR%XROAD_DIR, TM%T%CUR%XROAD, TM%T%CUR%XGREENROOF,         &
                 TM%T%CUR%XBLD_HEIGHT, TM%T%CUR%XWALL_O_HOR, TM%T%CUR%XCAN_HW_RATIO,  &
                 TM%T%CUR%XROAD_O_GRND, TM%T%CUR%XGARDEN_O_GRND, TM%T%CUR%XWALL_O_GRND,&
                 TM%T%CUR%XALB_ROOF, TM%T%CUR%XEMIS_ROOF, TM%T%CUR%XHC_ROOF,      &
                 TM%T%CUR%XTC_ROOF,TM%T%CUR%XD_ROOF,TM%T%CUR%XALB_ROAD,           &
                 TM%T%CUR%XEMIS_ROAD, TM%T%CUR%XSVF_ROAD,TM%T%CUR%XHC_ROAD,       &
                 TM%T%CUR%XTC_ROAD,TM%T%CUR%XD_ROAD,TM%T%CUR%XALB_WALL,           &
                 TM%T%CUR%XEMIS_WALL, TM%T%CUR%XSVF_WALL,TM%T%CUR%XSVF_GARDEN,    &
                 TM%T%CUR%XHC_WALL,TM%T%CUR%XTC_WALL,TM%T%CUR%XD_WALL,            &
                      ZRN_ROOF, ZH_ROOF, ZLE_ROOF, ZLEW_ROOF, ZGFLUX_ROOF,             &
                      ZRUNOFF_ROOF,                                                    &
                      ZRN_ROAD, ZH_ROAD, ZLE_ROAD, ZLEW_ROAD, ZGFLUX_ROAD,             &
                      ZRUNOFF_ROAD,                                                    &
                      ZRN_WALL_A, ZH_WALL_A, ZLE_WALL_A, ZGFLUX_WALL_A,                &
                      ZRN_WALL_B, ZH_WALL_B, ZLE_WALL_B, ZGFLUX_WALL_B,                &
                      ZRN_GARDEN,ZH_GARDEN,ZLE_GARDEN,ZGFLUX_GARDEN,                   &
                      ZRUNOFF_GARDEN, ZDRAIN_GARDEN, ZIRRIG_GARDEN,                    &
                      ZRN_GREENROOF,ZH_GREENROOF,ZLE_GREENROOF,ZGFLUX_GREENROOF,       &
                      ZRN_STRLROOF,ZH_STRLROOF,ZLE_STRLROOF,ZGFLUX_STRLROOF,           &
                      ZRUNOFF_STRLROOF,                                                &
                      ZRN_BLT,ZH_BLT,ZLE_BLT,ZGFLUX_BLT,                               &
                      ZRNSNOW_ROOF, ZHSNOW_ROOF, ZLESNOW_ROOF, ZGSNOW_ROOF,            &
                      ZMELT_ROOF,                                                      &
                      ZRNSNOW_ROAD, ZHSNOW_ROAD, ZLESNOW_ROAD, ZGSNOW_ROAD,            &
                      ZMELT_ROAD,                                                      &
                      ZRN_GRND, ZH_GRND, ZLE_GRND, ZGFLUX_GRND,                        &
                      ZRN, ZH, ZLE, ZGFLUX, ZEVAP, ZRUNOFF_TOWN, ZSFCO2,               &
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
                      ZABS_SW_PANEL,ZABS_LW_PANEL,                                     &
                      ZABS_SW_GARDEN,ZABS_LW_GARDEN,                                   &
                      ZABS_SW_GREENROOF,ZABS_LW_GREENROOF, ZG_GREENROOF_ROOF,          &
                      ZRUNOFF_GREENROOF, ZDRAIN_GREENROOF,                             &
                      ZIRRIG_GREENROOF, TM%BOP%CCOOL_COIL, TM%B%CUR%XF_WATER_COND,     &
                      TM%BOP%CHEAT_COIL,TM%B%CUR%CNATVENT,KDAY, TM%B%CUR%XAUX_MAX,     &
                      TM%B%CUR%XT_FLOOR, TM%B%CUR%XT_MASS, ZH_BLD_COOL,ZT_BLD_COOL,    &
                      ZH_BLD_HEAT, ZLE_BLD_COOL, ZLE_BLD_HEAT, ZH_WASTE, ZLE_WASTE,    &
                      TM%B%CUR%XF_WASTE_CAN, ZHVAC_COOL, ZHVAC_HEAT, TM%B%CUR%XQIN,    &
                      TM%B%CUR%XQIN_FRAD, TM%B%CUR%XQIN_FLAT, TM%B%CUR%XGR,            &
                      TM%B%CUR%XEFF_HEAT, TM%B%CUR%XINF, TM%B%CUR%XTCOOL_TARGET,       &
                      TM%B%CUR%XTHEAT_TARGET, TM%B%CUR%XHR_TARGET, TM%B%CUR%XT_WIN2,   &
                      TM%B%CUR%XQI_BLD, TM%B%CUR%XV_VENT,TM%B%CUR%XCAP_SYS_HEAT,       &
                      TM%B%CUR%XCAP_SYS_RAT, TM%B%CUR%XT_ADP, TM%B%CUR%XM_SYS_RAT,     &
                      TM%B%CUR%XCOP_RAT, ZCAP_SYS, ZM_SYS, ZCOP, ZQ_SYS, ZT_SYS,       &
                      ZTR_SW_WIN, ZFAN_POWER, TM%B%CUR%XHC_FLOOR, TM%B%CUR%XTC_FLOOR,  &
                      TM%B%CUR%XD_FLOOR, TM%B%CUR%XT_WIN1, ZABS_SW_WIN, ZABS_LW_WIN,   &
                      TM%B%CUR%XSHGC, TM%B%CUR%XSHGC_SH, TM%B%CUR%XUGG_WIN,            &
                      TM%B%CUR%XALB_WIN, TM%B%CUR%XABS_WIN, ZEMIT_LW_FAC, ZEMIT_LW_GRND,&
                      ZT_RAD_IND, ZREF_SW_GRND,ZREF_SW_FAC, ZHU_BLD, PTIME,            &
                      TM%B%CUR%LSHADE, TM%B%CUR%LSHAD_DAY, TM%B%CUR%LNATVENT_NIGHT,    &
                      TM%TOP%CBEM, TM%B%CUR%XN_FLOOR, TM%T%CUR%XWALL_O_BLD,            &
                      TM%B%CUR%XGLAZ_O_BLD, TM%B%CUR%XMASS_O_BLD, TM%B%CUR%XFLOOR_HW_RATIO,&
                      TM%B%CUR%XF_FLOOR_MASS, TM%B%CUR%XF_FLOOR_WALL, TM%B%CUR%XF_FLOOR_WIN,&
                      TM%B%CUR%XF_FLOOR_ROOF, TM%B%CUR%XF_WALL_FLOOR, TM%B%CUR%XF_WALL_MASS,&
                      TM%B%CUR%XF_WALL_WIN, TM%B%CUR%XF_WIN_FLOOR, TM%B%CUR%XF_WIN_MASS, &
                      TM%B%CUR%XF_WIN_WALL, TM%B%CUR%XF_MASS_FLOOR, TM%B%CUR%XF_MASS_WALL, &
                      TM%B%CUR%XF_MASS_WIN, TM%TOP%LCANOPY, TM%B%CUR%XTRAN_WIN,    &
                      TM%TOP%CCH_BEM, TM%T%CUR%XROUGH_ROOF, TM%T%CUR%XROUGH_WALL, &
                      TM%B%CUR%XF_WIN_WIN, GDM%TIR%LPAR_RD_IRRIG, GDM%TIR%XRD_START_MONTH, &
                      GDM%TIR%XRD_END_MONTH, GDM%TIR%XRD_START_HOUR, GDM%TIR%XRD_END_HOUR, &
                      GDM%TIR%XRD_24H_IRRIG, ZIRRIG_ROAD, TM%TPN%XEMIS_PANEL, &
                      TM%TPN%XALB_PANEL, TM%TPN%XEFF_PANEL, TM%TPN%XFRAC_PANEL, &
                      TM%T%CUR%XRESIDENTIAL, ZTHER_PROD_PANEL, ZPHOT_PROD_PANEL, &
                      ZPROD_PANEL, ZTHER_PROD_BLD  , ZPHOT_PROD_BLD, ZPROD_BLD,      &
                      TM%TPN%XTHER_PRODC_DAY, ZH_PANEL, ZRN_PANEL,                 &
                      TM%T%CUR%XDT_RES, TM%T%CUR%XDT_OFF,                          &
                      ZCUR_TCOOL_TARGET, ZCUR_THEAT_TARGET, ZCUR_QIN                   )


!
IF (.NOT. TM%TOP%LCANOPY) THEN
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
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_CD ,ZCD )
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_CDN,ZCDN)
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_RI ,ZRI )
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_CH ,ZCH )
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_RN ,ZRN )
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_H  ,ZH  )
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_LE ,ZLE )
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_GFLUX ,ZGFLUX )
!
!* warning: aerodynamical resistance does not yet take into account gardens
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_RESA_TOWN,1./ZRESA_TOWN)
IF (JTEB_PATCH==TM%TOP%NTEB_PATCH) ZAVG_RESA_TOWN = 1./ZAVG_RESA_TOWN
!
!-------------------------------------------------------------------------------------
! Diagnostics on each patch
!-------------------------------------------------------------------------------------
!
 CALL DIAG_MISC_TEB_n(TM%DGCT, TM%DGMT, TM%DGMTO, TM%TOP, &
                      PTSTEP, ZDQS_TOWN, ZQF_BLD, ZQF_TOWN, ZFLX_BLD,           &
                     ZRUNOFF_TOWN,                                             &
                     ZRN_ROAD, ZH_ROAD, ZLE_ROAD, ZGFLUX_ROAD,                 &
                     ZRUNOFF_ROAD, ZIRRIG_ROAD,                                &
                     ZRN_WALL_A, ZH_WALL_A, ZGFLUX_WALL_A,                     &
                     ZRN_WALL_B, ZH_WALL_B, ZGFLUX_WALL_B,                     &
                     ZRN_ROOF, ZH_ROOF, ZLE_ROOF, ZGFLUX_ROOF, ZRUNOFF_ROOF,   &
                     ZRN_STRLROOF, ZH_STRLROOF, ZLE_STRLROOF, ZGFLUX_STRLROOF, &
                     ZRUNOFF_STRLROOF,                                         &
                     ZRN_GREENROOF, ZH_GREENROOF,                              &
                     ZLE_GREENROOF, ZGFLUX_GREENROOF, ZG_GREENROOF_ROOF,       &
                     ZRUNOFF_GREENROOF, ZDRAIN_GREENROOF,ZIRRIG_GREENROOF,     &
                     ZRN_GARDEN,ZH_GARDEN,ZLE_GARDEN,ZGFLUX_GARDEN,            &
                     ZRUNOFF_GARDEN, ZDRAIN_GARDEN, ZIRRIG_GARDEN,             &
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
                     ZABS_SW_WIN, ZABS_LW_WIN,                                 &
                     ZCUR_TCOOL_TARGET, ZCUR_THEAT_TARGET, ZCUR_QIN,           &
                     ZABS_SW_PANEL, ZABS_LW_PANEL, ZRN_PANEL,                  &
                     ZH_PANEL, ZTHER_PROD_PANEL, ZPHOT_PROD_PANEL, ZPROD_PANEL,&
                     ZTHER_PROD_BLD, ZPHOT_PROD_BLD                            )
!
!
!-------------------------------------------------------------------------------------
! Computes averaged parameters necessary for UTCI
!-------------------------------------------------------------------------------------
!
IF (TM%DGT%N2M >0 .AND. TM%DGUT%LUTCI) THEN
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_REF_SW_GRND ,ZREF_SW_GRND )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_REF_SW_FAC  ,ZREF_SW_FAC  )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_SCA_SW      ,ZSCA_SW      )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_DIR_SW      ,ZDIR_SW      )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_EMIT_LW_FAC ,ZEMIT_LW_FAC )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_EMIT_LW_GRND,ZEMIT_LW_GRND)
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_T_RAD_IND   ,ZT_RAD_IND   )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_TI_BLD      ,TM%B%CUR%XTI_BLD      )
  CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_QI_BLD      ,TM%B%CUR%XQI_BLD      )
END IF
!
!-------------------------------------------------------------------------------------
! Use of the canopy version of TEB
!-------------------------------------------------------------------------------------
!
IF (TM%TOP%LCANOPY) THEN
!-------------------------------------------------------------------------------------
! Town averaged quantities to force canopy atmospheric layers
!-------------------------------------------------------------------------------------

 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_DUWDU_GRND ,ZDUWDU_GRND )
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_UW_ROOF ,ZUW_ROOF)
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_DUWDU_ROOF ,ZDUWDU_ROOF)
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_H_WALL ,0.5*(ZH_WALL_A+ZH_WALL_B))
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_H_ROOF ,(ZH_ROOF+TM%T%CUR%XH_INDUSTRY))
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_E_ROOF ,(ZLE_ROOF+TM%T%CUR%XLE_INDUSTRY)/XLVTT)
!
!-------------------------------------------------------------------------------------
! Computes the impact of canopy and surfaces on air
!-------------------------------------------------------------------------------------
!
ZAC_GRND    (:) = (TM%T%CUR%XROAD(:)*ZAC_ROAD    (:) + &
                TM%T%CUR%XGARDEN(:)*ZAC_GARDEN    (:)) / (TM%T%CUR%XROAD(:)+TM%T%CUR%XGARDEN(:))
ZAC_GRND_WAT(:) = (TM%T%CUR%XROAD(:)*ZAC_ROAD_WAT(:) + &
                TM%T%CUR%XGARDEN(:)*ZAC_GARDEN_WAT(:)) / (TM%T%CUR%XROAD(:)+TM%T%CUR%XGARDEN(:))
!
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_AC_GRND     ,ZAC_GRND    )
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZAVG_AC_GRND_WAT ,ZAC_GRND_WAT)
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZSFLUX_U ,ZUW_GRND * (1.-TM%T%CUR%XBLD))
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZSFLUX_T ,ZH_GRND  * (1.-TM%T%CUR%XBLD)/XCPD/PRHOA)
 CALL ADD_PATCH_CONTRIB(JTEB_PATCH,ZSFLUX_Q ,ZLE_GRND * (1.-TM%T%CUR%XBLD)/XLVTT)
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
IF (TM%TOP%LCANOPY) THEN
!
!-------------------------------------------------------------------------------------
!* Impact of TEB fluxes on the air
!-------------------------------------------------------------------------------------
!
 CALL TEB_CANOPY(KI,TM%TCP%NLVL,TM%TCP%XZ,TM%TCP%XZF,TM%TCP%XDZ,TM%TCP%XDZF,ZAVG_BLD,&
                 ZAVG_BLD_HEIGHT,ZAVG_WALL_O_HOR, PPA,PRHOA,TM%TCP%XU,                &
                ZAVG_DUWDU_GRND, ZAVG_UW_ROOF, ZAVG_DUWDU_ROOF,                       &
                ZAVG_H_WALL,ZAVG_H_ROOF,ZAVG_E_ROOF,ZAVG_AC_GRND,ZAVG_AC_GRND_WAT,    &
                ZFORC_U,ZDFORC_UDU,ZFORC_E,ZDFORC_EDE,ZFORC_T,ZDFORC_TDT,ZFORC_Q,ZDFORC_QDQ)
!
!-------------------------------------------------------------------------------------
!* Evolution of canopy air due to these impacts
!-------------------------------------------------------------------------------------
!
 CALL CANOPY_EVOL(KI,TM%TCP%NLVL,PTSTEP,2,                                    &
                 ZL,ZWIND,PTA,PQA,PPA,PRHOA,                                  &
                 ZSFLUX_U,ZSFLUX_T,ZSFLUX_Q,                                  &
                 ZFORC_U,ZDFORC_UDU,ZFORC_E,ZDFORC_EDE,                       &
                 ZFORC_T,ZDFORC_TDT,ZFORC_Q,ZDFORC_QDQ,                       &
                 TM%TCP%XZ,TM%TCP%XZF,TM%TCP%XDZ,TM%TCP%XDZF,TM%TCP%XU,       &
                 TM%TCP%XTKE,TM%TCP%XT,TM%TCP%XQ,TM%TCP%XLMO,TM%TCP%XLM,      &
                 TM%TCP%XLEPS,TM%TCP%XP,             &
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
!-------------------------------------------------------------------------------------
!Radiative properties should be at time t+1 (see by the atmosphere) in order to close
!the energy budget between surfex and the atmosphere. It is not the case here
!for ALB and EMIS
!-------------------------------------------------------------------------------------
!
 CALL AVERAGE_RAD(TM%TOP%XTEB_PATCH,                                      &
                 ZDIR_ALB_PATCH, ZSCA_ALB_PATCH, ZEMIS_PATCH, ZTRAD_PATCH,&
                 PDIR_ALB,       PSCA_ALB,       PEMIS,       PTRAD       )
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
PQSURF (:) = TM%T%CUR%XQ_CANYON     (:) ! Should account for ISBA (greenroof and garden) Qs
!
!-------------------------------------------------------------------------------------
! Scalar fluxes:
!-------------------------------------------------------------------------------------
!
ZAVG_USTAR    (:) = SQRT(SQRT(PSFU**2+PSFV**2))
!
!
IF (TM%CHT%SVT%NBEQ>0) THEN
  IF (TM%CHT%CCH_DRY_DEP == "WES89") THEN
    CALL CH_DEP_TOWN(ZAVG_RESA_TOWN,  ZAVG_USTAR, PTA, PTRAD, ZAVG_WALL_O_HOR,&
                     PSV(:,TM%CHT%SVT%NSV_CHSBEG:TM%CHT%SVT%NSV_CHSEND),        &
                     TM%CHT%SVT%CSV(TM%CHT%SVT%NSV_CHSBEG:TM%CHT%SVT%NSV_CHSEND),  &
                     TM%CHT%XDEP(:,1:TM%CHT%SVT%NBEQ)  )
   
    DO JI=TM%CHT%SVT%NSV_CHSBEG,TM%CHT%SVT%NSV_CHSEND
!cdir nodep
      DO JJ=1,SIZE(PSFTS,1)
        PSFTS(JJ,JI) = - PSV(JJ,JI) * TM%CHT%XDEP(JJ,JI-TM%CHT%SVT%NSV_CHSBEG+1)
      ENDDO
    ENDDO

    IF (TM%CHT%SVT%NAEREQ > 0 ) THEN
      CALL CH_AER_DEP(PSV(:,TM%CHT%SVT%NSV_AERBEG:TM%CHT%SVT%NSV_AEREND),&
                         PSFTS(:,TM%CHT%SVT%NSV_AERBEG:TM%CHT%SVT%NSV_AEREND),&
                         ZAVG_USTAR,ZAVG_RESA_TOWN,PTA,PRHOA)   
    END IF

  ELSE
    DO JI=TM%CHT%SVT%NSV_CHSBEG,TM%CHT%SVT%NSV_CHSEND
      PSFTS(:,JI) =0.
    ENDDO
    IF(TM%CHT%SVT%NSV_AERBEG.LT.TM%CHT%SVT%NSV_AEREND) THEN
      DO JI=TM%CHT%SVT%NSV_AERBEG,TM%CHT%SVT%NSV_AEREND
        PSFTS(:,JI) =0.
      ENDDO
    ENDIF
  ENDIF
ENDIF

IF (TM%CHT%SVT%NDSTEQ>0) THEN
  ! Blindage à enlever lorsque que TEB aura été corrigé
  ZUSTAR(:)     = MIN(ZUSTAR(:), 10.)
  ZRESA_TOWN(:) = MAX(ZRESA_TOWN(:), 10.)
  !
  CALL DSLT_DEP(PSV(:,TM%CHT%SVT%NSV_DSTBEG:TM%CHT%SVT%NSV_DSTEND), &
                PSFTS(:,TM%CHT%SVT%NSV_DSTBEG:TM%CHT%SVT%NSV_DSTEND),   &
                ZUSTAR, ZRESA_TOWN, PTA, PRHOA, DST%XEMISSIG_DST, DST%XEMISRADIUS_DST,  &
                JPMODE_DST, XDENSITY_DST, XMOLARWEIGHT_DST, ZCONVERTFACM0_DST,  &
                ZCONVERTFACM6_DST, ZCONVERTFACM3_DST, LVARSIG_DST, LRGFIX_DST,  &
                CVERMOD  )  

  CALL MASSFLUX2MOMENTFLUX(         &
    PSFTS(:,TM%CHT%SVT%NSV_DSTBEG:TM%CHT%SVT%NSV_DSTEND), & !I/O ![kg/m2/sec] In: flux of only mass, out: flux of moments
    PRHOA,                          & !I [kg/m3] air density
    DST%XEMISRADIUS_DST,                &!I [um] emitted radius for the modes (max 3)
    DST%XEMISSIG_DST,                   &!I [-] emitted sigma for the different modes (max 3)
    NDSTMDE,                        &
    ZCONVERTFACM0_DST,              &
    ZCONVERTFACM6_DST,              &
    ZCONVERTFACM3_DST,              &
    LVARSIG_DST, LRGFIX_DST         )  
ENDIF
IF (TM%CHT%SVT%NSLTEQ>0) THEN
  CALL DSLT_DEP(PSV(:,TM%CHT%SVT%NSV_SLTBEG:TM%CHT%SVT%NSV_SLTEND), &
                PSFTS(:,TM%CHT%SVT%NSV_SLTBEG:TM%CHT%SVT%NSV_SLTEND),   &
                ZUSTAR, ZRESA_TOWN, PTA, PRHOA, SLT%XEMISSIG_SLT, SLT%XEMISRADIUS_SLT,  &
                JPMODE_SLT, XDENSITY_SLT, XMOLARWEIGHT_SLT, ZCONVERTFACM0_SLT,  &
                ZCONVERTFACM6_SLT, ZCONVERTFACM3_SLT, LVARSIG_SLT, LRGFIX_SLT,  &
                CVERMOD  )  

  CALL MASSFLUX2MOMENTFLUX(         &
    PSFTS(:,TM%CHT%SVT%NSV_SLTBEG:TM%CHT%SVT%NSV_SLTEND), & !I/O ![kg/m2/sec] In: flux of only mass, out: flux of moments
    PRHOA,                          & !I [kg/m3] air density
    SLT%XEMISRADIUS_SLT,                &!I [um] emitted radius for the modes (max 3)
    SLT%XEMISSIG_SLT,                   &!I [-] emitted sigma for the different modes (max 3)
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
 CALL DIAG_INLINE_TEB_n(TM%DGT, TM%TCP, TM%T, &
                        TM%TOP%LCANOPY, PTA, PTRAD, ZQA, PPA, PPS, PRHOA,       &
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
IF (.NOT. TM%TOP%LCANOPY) THEN
  DO JTEB_PATCH=1,TM%TOP%NTEB_PATCH
    CALL GOTO_WRAPPER_TEB_PATCH(TM%B, TM%DGCT, TM%DGMT, TM%T, &
                                GDM%TGD, GDM%TGDPE, GRM%TGR, GRM%TGRPE, JTEB_PATCH)
    TM%T%CUR%XT_CANYON(:) = ZAVG_T_CANYON(:)
    TM%T%CUR%XQ_CANYON(:) = ZAVG_Q_CANYON(:)
  END DO
END IF
!          
!-------------------------------------------------------------------------------------
! Thermal confort index
!-------------------------------------------------------------------------------------
!
IF (TM%DGUT%LUTCI .AND. TM%DGT%N2M >0) THEN
  DO JJ=1,KI
    IF (TM%DGT%XZON10M(JJ)/=XUNDEF) THEN
      ZU_UTCI(JJ) = SQRT(TM%DGT%XZON10M(JJ)**2+TM%DGT%XMER10M(JJ)**2)
    ELSE
      ZU_UTCI(JJ) = ZWIND(JJ)
    ENDIF
  ENDDO
 CALL UTCI_TEB(TM%T%CUR%XT_CANYON, TM%T%CUR%XQ_CANYON, ZAVG_TI_BLD, ZAVG_QI_BLD, ZU_UTCI, &
     PPS, ZAVG_REF_SW_GRND,ZAVG_REF_SW_FAC, ZAVG_SCA_SW, ZAVG_DIR_SW, PZENITH, &
     ZAVG_EMIT_LW_FAC, ZAVG_EMIT_LW_GRND, PLW, ZAVG_T_RAD_IND, TM%T%CUR%XBLD, &
     TM%T%CUR%XBLD_HEIGHT, TM%T%CUR%XWALL_O_HOR, TM%DGUT%XUTCI_IN, TM%DGUT%XUTCI_OUTSUN,     &
     TM%DGUT%XUTCI_OUTSHADE, TM%DGUT%XTRAD_SUN, TM%DGUT%XTRAD_SHADE                           )
 CALL UTCIC_STRESS(PTSTEP,TM%DGUT%XUTCI_IN      ,TM%DGUT%XUTCIC_IN      )
 CALL UTCIC_STRESS(PTSTEP,TM%DGUT%XUTCI_OUTSUN  ,TM%DGUT%XUTCIC_OUTSUN  )
 CALL UTCIC_STRESS(PTSTEP,TM%DGUT%XUTCI_OUTSHADE,TM%DGUT%XUTCIC_OUTSHADE)
ELSE IF (TM%DGUT%LUTCI) THEN
  TM%DGUT%XUTCI_IN(:) = XUNDEF
  TM%DGUT%XUTCI_OUTSUN(:) = XUNDEF
  TM%DGUT%XUTCI_OUTSHADE(:) = XUNDEF
  TM%DGUT%XTRAD_SUN(:) = XUNDEF
  TM%DGUT%XTRAD_SHADE(:) = XUNDEF
  TM%DGUT%XUTCIC_IN(:,:) = XUNDEF
  TM%DGUT%XUTCIC_OUTSUN(:,:) = XUNDEF
  TM%DGUT%XUTCIC_OUTSHADE(:,:) = XUNDEF
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
PAVG = PAVG + TM%TOP%XTEB_PATCH(:,JP) * PFIELD(:)
!
END SUBROUTINE ADD_PATCH_CONTRIB
!-------------------------------------------------------------------------------------
!
END SUBROUTINE COUPLING_TEB_n


