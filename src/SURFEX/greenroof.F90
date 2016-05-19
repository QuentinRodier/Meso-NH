!     #########
    SUBROUTINE GREENROOF(HIMPLICIT_WIND, TPTIME, PTSUN, PPEW_A_COEF, PPEW_B_COEF,    &
                PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,                  &
                PTSTEP, PZREF, PUREF,                                                &
                PTA, PQA, PEXNS, PEXNA,PRHOA, PCO2, PPS, PRR, PSR, PZENITH,          &
                PSW,PLW, PVMOD,                                                      &
                PRN_GREENROOF,PH_GREENROOF,PLE_GREENROOF,PGFLUX_GREENROOF,           &
                PSFCO2,PEVAP_GREENROOF, PUW_GREENROOF,                               &
                PAC_GREENROOF,PQSAT_GREENROOF,PTS_GREENROOF,                         &
                PAC_AGG_GREENROOF, PHU_AGG_GREENROOF,PDEEP_FLUX,                     &
                PRUNOFF_GREENROOF, PDRAIN_GREENROOF                                  )  
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
!!	C. de Munck & A. Lemonsu          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!     Original    09/2011 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,             ONLY: XUNDEF
USE MODD_TYPE_DATE_SURF,       ONLY: DATE_TIME
USE MODD_CSTS,                 ONLY: XCPD
USE MODD_TEB_n,                ONLY: LECOCLIMAP, XCOVER, XT_ROOF
USE MODD_TEB_GRID_n,        ONLY: XLAT, XLON
USE MODD_TEB_VEG_n,            ONLY: CPHOTO, CC1DRY, NNBIOMASS, CRESPSL, &
                                     CALBEDO, CSOILFRZ, CDIFSFCOND, CCPSURF,  &
                                     CSNOWRES, XCGMAX, CISBA
USE MODD_TEB_GREENROOF_n,      ONLY: LSTRESS, CSOC_GR, LTR_ML_GR,             &
                                     CISBA_GR, CRUNOFF_GR, CSCOND_GR,         &
                                     CKSAT_GR, CHORT_GR,                      &
                                     NLAYER_GR, NLAYER_HORT_GR, NLAYER_DUN_GR,&
                                     TSNOW, XSOILWGHT,                        &
                                     XVEGTYPE, NWG_LAYER, XDZG, XDZDIF,       &
                                     XEMIS, XVEG, XLAI, XWRMAX_CF, XRSMIN,    &
                                     XGAMMA, XCV, XRGL, XRUNOFFD,             &
                                     XZ0, XZ0_O_Z0H, XRUNOFFB_GR, XWDRAIN_GR, &
                                     XCGSAT, XC1SAT, XC2REF, XC3, XC4B,       &
                                     XC4REF, XACOEF, XPCOEF, XTAUICE,         &
                                     XWR, XRESA, XAN,                         &
                                     XANFM, XANDAY, XABC, XPOI,               &
                                     XFZERO, XEPSO, XGAMM, XQDGAMM,           &
                                     XGMES, XQDGMES, XT1GMES, XT2GMES,        &
                                     XRESP_BIOMASS, XBSLAI, XLAIMIN, XSEFOLD, &
                                     XAMAX, XQDAMAX, XT1AMAX, XT2AMAX,        &
                                     XF2I, XGC, XAH, XBH, XDMAX,              &
                                     XDG, XROOTFRAC, XTG, XWG, XWGI, XPCPS,   &
                                     XPLVTT, XPLSTT, XWFC, XWWILT, XWSAT,     &
                                     XBCOEF, XCONDSAT, XMPOTSAT, XHCAPSOIL,   &
                                     XCE_NITRO, XCF_NITRO, XCNA_NITRO,        &
                                     XCONDDRY, XCONDSLD, XRE25,               &
                                     XKSAT_ICE, XD_ICE,                       &
                                     XALBNIR, XALBVIS, XALBUV,                &
                                     XALBNIR_VEG, XALBVIS_VEG, XALBUV_VEG,    &
                                     XALBNIR_SOIL, XALBVIS_SOIL, XALBUV_SOIL, &
                                     XALBNIR_TVEG, XALBVIS_TVEG,    &
                                     XALBNIR_TSOIL, XALBVIS_TSOIL,  &
                                     XLE, XANF, XSAND_GR,                     &
                                     XPSN, XPSNV, XPSNG, XPSNV_A,             &
                                     XSNOWFREE_ALB_VEG, XSNOWFREE_ALB_SOIL,   &
                                     XSNOWFREE_ALB,                           &
                                     XANMAX, XBIOMASS,                        &
                                     XBSLAI_NITRO, XH_TREE
!
USE MODI_ISBA
USE MODI_VEGETATION_UPDATE_GREENROOF
USE MODI_VEGETATION_EVOL
USE MODI_CARBON_EVOL
USE MODE_THERMOS
USE MODI_ROOF_IMPL_COEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    Declarations of arguments
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
REAL, DIMENSION(:)  , INTENT(IN)    :: PZREF              ! height of the first atmospheric level                                                !
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

REAL, DIMENSION(:)  , INTENT(OUT)   :: PRN_GREENROOF         ! net radiation over greenroofs
REAL, DIMENSION(:)  , INTENT(OUT)   :: PH_GREENROOF          ! sensible heat flux over greenroofs
REAL, DIMENSION(:)  , INTENT(OUT)   :: PLE_GREENROOF         ! latent heat flux over greenroofs
REAL, DIMENSION(:)  , INTENT(OUT)   :: PGFLUX_GREENROOF      ! flux through the greenroofs
REAL, DIMENSION(:)  , INTENT(OUT)   :: PSFCO2                ! flux of greenroof CO2       (kg/m2/s)
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
REAL, DIMENSION(0)                   :: PSODELX             ! Pulsation for each layer (Only used if LTEMP_ARP=True)
REAL, DIMENSION(SIZE(PPS))           :: PMUF                ! fraction of the grid cell reached by the rainfall
REAL, DIMENSION(SIZE(PPS))           :: PFSAT               ! Topmodel saturated fraction
REAL, DIMENSION(SIZE(PPS))           :: ZDIRCOSZW           ! orography slope cosine (=1 in TEB)
REAL, DIMENSION(SIZE(PPS),NNBIOMASS) :: ZRESP_BIOMASS_INST  ! instantaneous biomass respiration (kgCO2/kgair m/s)
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
REAL, DIMENSION(SIZE(PPS))   :: ZSMELTFLUX    ! energy removed from soil/vegetation surface
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
REAL, DIMENSION(SIZE(PPS))   :: ZGFLUX
REAL, DIMENSION(SIZE(PPS))   :: ZRESTORE
REAL, DIMENSION(SIZE(PPS))   :: ZUSTAR
REAL, DIMENSION(SIZE(PPS))   :: ZDRAIN
REAL, DIMENSION(SIZE(PPS))   :: ZRUNOFF
REAL, DIMENSION(SIZE(PPS))   :: ZMELT
REAL, DIMENSION(SIZE(PPS),TSNOW%NLAYER) :: ZSNOWTEMP
REAL, DIMENSION(SIZE(PPS),TSNOW%NLAYER) :: ZSNOWLIQ
REAL, DIMENSION(SIZE(PPS),TSNOW%NLAYER) :: ZSNOWDZ
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
TYPE (DATE_TIME),   DIMENSION(0) :: TPSEED ! seeding date
TYPE (DATE_TIME),   DIMENSION(0) :: TPREAP ! reaping date
!
INTEGER                    :: ILU
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
PMUF      = 0.
PFSAT     = 0.
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
!* deep soil implicitation with roof
!
ZGAMMAT = XUNDEF
 CALL ROOF_IMPL_COEF(PTSTEP,ZTDEEP_A,ZTDEEP_B)
!
!-------------------------------------------------------------------------------
!
!*      9.     Treatment of green areas
!              ------------------------
!
!radiative temperature diagnostic
!-------------------------------
!
!*      9.2    Call ISBA for greenroofs
!              ------------------------
!
 CALL ISBA(CISBA_GR, CPHOTO, LTR_ML_GR, 'WSAT', CKSAT_GR, CSOC_GR, &
          HRAIN, CHORT_GR, CC1DRY, CSCOND_GR, TSNOW%SCHEME, &
          CSNOWRES, CCPSURF, CSOILFRZ, CDIFSFCOND, TPTIME, OFLOOD, &
          OTEMP_ARP, OGLACIER, PTSTEP, HIMPLICIT_WIND, &
          XCGMAX, PZREF, PUREF, ZDIRCOSZW, PTA,         &
          PQA, PEXNA, PRHOA, PPS, PEXNS,  PRR, PSR, PZENITH,    &
          PSW, PLW, PVMOD, PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF, &
          PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF, XRSMIN, XRGL, XGAMMA,&
          XCV, XRUNOFFD, XSOILWGHT, NLAYER_HORT_GR, NLAYER_DUN_GR,    &
          XALBNIR_TVEG, XALBVIS_TVEG, XALBNIR_TSOIL, XALBVIS_TSOIL,   &
          XSNOWFREE_ALB, XWRMAX_CF, XVEG, XLAI, XEMIS, XZ0,           &
          XZ0/XZ0_O_Z0H, XVEGTYPE, XZ0, XRUNOFFB_GR, XCGSAT, XC1SAT,     &
          XC2REF, XC3, XC4B, XC4REF, XACOEF, XPCOEF, XTAUICE, XWDRAIN_GR,&
          ZTDEEP_A, ZTDEEP_B, ZGAMMAT, XPSN, XPSNG, XPSNV, XPSNV_A,   &
          XSNOWFREE_ALB_VEG, XSNOWFREE_ALB_SOIL, ZIRRIG, ZWATSUP,     &
          ZTHRESHOLDSPT, GIRRIGATE, GIRRIDAY, LSTRESS, XGC, XF2I,     &
          XDMAX, XAH, XBH, PCO2, XGMES, XPOI, XFZERO, XEPSO, XGAMM,   &
          XQDGAMM, XQDGMES, XT1GMES, XT2GMES, XAMAX, XQDAMAX, XT1AMAX,&
          XT2AMAX, XABC, XDG, XDZG, XDZDIF, NWG_LAYER, XROOTFRAC,  &
          XWFC, XWWILT, XWSAT, XBCOEF,  XCONDSAT, XMPOTSAT,        &
          XHCAPSOIL, XCONDDRY, XCONDSLD, XD_ICE, XKSAT_ICE, PMUF, ZFF,&
          ZFFG, ZFFV, ZFFG_NOSNOW, ZFFV_NOSNOW, ZFFROZEN,  ZALBF,     &
          ZEMISF, ZFFLOOD, ZPIFLOOD, ZIFLOOD, ZPFLOOD, ZLEFLOOD,      &
          ZLEIFLOOD, PSODELX, XLAT, XLON, XTG, XWG, XWGI, XPCPS,      &
          XPLVTT, XPLSTT, XWR, XRESA, XANFM, PFSAT, TSNOW%ALB(:,1),   &
          TSNOW%WSNOW(:,:,1), TSNOW%HEAT(:,:,1), TSNOW%RHO(:,:,1),    &
          TSNOW%GRAN1(:,:,1), TSNOW%GRAN2(:,:,1), TSNOW%HIST(:,:,1),  &
          TSNOW%AGE(:,:,1), ZGRNDFLUX, ZHPSNOW, ZSNOWHMASS,           &
          ZSMELTFLUX, ZRNSNOW, ZHSNOW,  ZGFLUXSNOW, ZUSTARSNOW,       &
          ZSRSFC, ZRRSFC, ZLESL, TSNOW%EMIS(:,1), ZCDSNOW, ZCHSNOW,   &
          PTS_GREENROOF, ZTS, ZHV, ZQS, ZSNOWTEMP, ZSNOWLIQ, ZSNOWDZ,    &
          ZCG, ZC1, ZC2, ZWGEQ, ZCT, ZCH, ZCD, ZCDN, ZRI, ZHU, ZHUG,  &
          ZEMIST, ZALBT, ZRS, XLE,  ZRN, ZH, ZLEI, ZLEGI, ZLEG, ZLEV, &
          ZLES, ZLER, ZLETR, ZEVAP, ZGFLUX, ZRESTORE, ZUSTAR, ZDRAIN, &
          ZRUNOFF, ZMELT, ZMELTADV, ZRN_ISBA, ZH_ISBA, ZLEG_ISBA,     &
          ZLEGI_ISBA, ZLEV_ISBA, ZLETR_ISBA, ZUSTAR_ISBA, ZLER_ISBA,  &
          ZLE_ISBA, ZLEI_ISBA, ZGFLUX_ISBA, ZHORT, ZDRIP, ZRRVEG,     &
          PAC_AGG_GREENROOF, PHU_AGG_GREENROOF, ZFAPARC, ZFAPIRC, ZMUS,     &
          ZLAI_EFFC, XAN, XANDAY, ZRESP_BIOMASS_INST, ZIACAN, XANF,   &
          ZGPP, ZFAPAR, ZFAPIR, ZFAPAR_BS, ZFAPIR_BS, ZIRRIG_FLUX,    &
          PDEEP_FLUX                                                  )  
!
PRUNOFF_GREENROOF(:) = ZRUNOFF(:)
PDRAIN_GREENROOF(:)  = ZDRAIN(:)

!
IF (TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') TSNOW%TS(:,1)=ZSNOWTEMP(:,1)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Diagnostic of respiration carbon fluxes and soil carbon evolution
!
! --------------------------------------------------------------------------------------
! Vegetation update (in case of non-interactive vegetation):
! --------------------------------------------------------------------------------------
!
IF (CPHOTO=='NON' .OR. CPHOTO=='AGS' .OR. CPHOTO=='AST') THEN
     CALL VEGETATION_UPDATE_GREENROOF(TPTIME,PTSTEP,ILU)
END IF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Vegetation evolution for interactive LAI
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (CPHOTO=='LAI' .OR. CPHOTO=='LST' .OR. CPHOTO=='NIT') THEN
  CALL VEGETATION_EVOL(CISBA_GR, CPHOTO, CRESPSL, CALBEDO, .FALSE., LTR_ML_GR,&
                       PTSTEP, TPTIME%TDATE%MONTH, TPTIME%TDATE%DAY, 1,    &
                       TPTIME%TIME, XLAT, PRHOA, XDG, XDZG, NWG_LAYER,     & 
                       XTG, XALBNIR_VEG, XALBVIS_VEG, XALBUV_VEG,          &
                       XALBNIR_SOIL, XALBVIS_SOIL, XALBUV_SOIL,            &
                       XVEGTYPE, XSEFOLD, XANMAX, XH_TREE, XBSLAI,         &
                       XLAIMIN, PCO2, XCE_NITRO, XCF_NITRO, XCNA_NITRO,    &
                       XBSLAI_NITRO, XGMES, ZTAU_WOOD, TPSEED,             &
                       TPREAP, ZAOSIP, ZAOSIM, ZAOSJP, ZAOSJM,             &
                       ZHO2IP, ZHO2IM, ZHO2JP, ZHO2JM, ZZ0EFFIP,           &
                       ZZ0EFFIM, ZZ0EFFJP, ZZ0EFFJM, XLAI, XVEG,           &
                       XZ0, XALBNIR, XALBVIS, XALBUV, XEMIS,               &
                       XANFM, XANDAY, XBIOMASS, XRESP_BIOMASS,             &
                       ZRESP_BIOMASS_INST, ZINCREASE, ZTURNOVER             )          
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
PSFCO2(:)=0.
!
IF (CPHOTO/='NON' .AND. CRESPSL/='NON' .AND. ANY(XLAI(:)/=XUNDEF)) THEN
  ! faire intervenir le type de vegetation du greenroof ? (CTYP_GR)
  CALL CARBON_EVOL(CISBA, CRESPSL, CPHOTO, PTSTEP, 1,               &
                     PRHOA, XTG, XWG, XWFC, XWWILT, XWSAT, XSAND_GR,&
                     XDG, XDZG, NWG_LAYER,                          &                   
                     XRE25, XLAI, ZRESP_BIOMASS_INST, ZTURNOVER,    &
                     ZLITTER, ZLIGNIN_STRUC , ZSOILCARB,            &
                     ZRESP_AUTO, ZRESP_ECO                          ) 
  ! calculation of vegetation CO2 flux
  PSFCO2(:) = ZGPP(:) - ZRESP_ECO(:)
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
 PLE_GREENROOF    (:) = XLE    (:)
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
 PQSAT_GREENROOF(:) = QSAT(XTG(:,1),PPS(:))
!
!* friction flux
  PUW_GREENROOF(:)    = -ZUSTAR(:)**2
IF (LHOOK) CALL DR_HOOK('GREENROOF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE GREENROOF
