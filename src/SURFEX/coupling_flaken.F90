!     ###############################################################################
SUBROUTINE COUPLING_FLAKE_n(HPROGRAM, HCOUPLING,                                         &
                 PTSTEP, KYEAR, KMONTH, KDAY, PTIME, KI, KSV, KSW, PTSUN, PZENITH,       &
                 PZENITH2, PAZIM, PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV, PCO2, &
                 HSV, PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA,          &
                 PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,                                &
                 PTRAD, PDIR_ALB, PSCA_ALB, PEMIS,                                       &
                 PPEW_A_COEF, PPEW_B_COEF,                                               &
                 PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,                     &
                 HTEST                                                                   )  
!     ###############################################################################

!
!!****  *COUPLING_FLAKE_n * - Driver for FLAKE scheme for lakes
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
!!      V. Masson   05/2009 Implicitation of momentum fluxes
!!      B. Decharme 01/2010 Add XTT in water_flux
!!      V. Masson   11/2011 Ch limited to 1.E-7 in all cases and Cd coming from
!!                          Flake_interface routine if computed by flake
!!      B. Decharme 09/2012 New wind implicitation
!!      P. Le Moigne 10/2012 ECUME option for FLake. Remove wind threshold
!!------------------------------------------------------------------
!
USE MODD_SURF_ATM, ONLY : CIMPLICIT_WIND
!
USE MODD_CSTS,     ONLY : XRD, XCPD, XP00, XLVTT, XKARMAN, XTT, XTTS
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODD_FLAKE_n,  ONLY :   TTIME         , XEMIS         , XWATER_DEPTH  , &
                            XWATER_FETCH  , XT_BS         , XDEPTH_BS     , &
                            XCORIO        , XDIR_ALB      , XSCA_ALB      , &
                            XICE_ALB      , XSNOW_ALB     , XEXTCOEF_WATER, &
                            XEXTCOEF_ICE  , XEXTCOEF_SNOW , XT_SNOW       , &
                            XT_ICE        , XT_MNW        , XT_WML        , &
                            XT_BOT        , XT_B1         , XCT           , &
                            XH_SNOW       , XH_ICE        , XH_ML         , &
                            XH_B1         , XTS           , XZ0           , &
                            XUSTAR        , LSEDIMENTS    , CFLK_FLUX     , &
                            CFLK_ALB      , XICHCE        , LPRECIP       , &
                            LPWEBB
!                          
!salgado - keep the same ch_ routines and modules used in watflux_n
USE MODD_CH_WATFLUX_n, ONLY : CSV, CCH_DRY_DEP, XDEP, NBEQ, NSV_CHSBEG, NSV_CHSEND,&
                                NSV_DSTBEG, NSV_DSTEND, NAEREQ, NDSTEQ, NSLTEQ, &
                                NSV_AERBEG, NSV_AEREND, NSV_SLTBEG, NSV_SLTEND  
!
USE MODD_SLT_SURF
USE MODD_DST_SURF
USE MODD_SLT_n,       ONLY: XEMISRADIUS_SLT,XEMISSIG_SLT
USE MODD_DST_n,       ONLY: XEMISRADIUS_DST,XEMISSIG_DST
!
USE MODE_DSLT_SURF
USE MODE_THERMOS
! 
USE MODI_WATER_FLUX
USE MODI_ECUME_SEAFLUX
USE MODI_ADD_FORECAST_TO_DATE_SURF
USE MODI_DIAG_INLINE_FLAKE_n 
USE MODI_DIAG_MISC_FLAKE_n
USE MODI_CH_AER_DEP
USE MODI_CH_DEP_WATER
USE MODI_DSLT_DEP
USE MODI_FLAKE_ALBEDO
USE MODI_UPDATE_RAD_SEAWAT
USE MODI_ABOR1_SFX
USE MODI_FLAKE_INTERFACE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
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
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH   ! zenithal angle at t         (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH2  ! zenithal angle at t+1       (radian from the vertical)
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
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_A_COEF! implicit coefficients   (m2s/kg)
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_B_COEF! needed if HCOUPLING='I' (m/s)
REAL, DIMENSION(KI), INTENT(IN) :: PPET_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPET_B_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_B_COEF
 CHARACTER(LEN=2),    INTENT(IN) :: HTEST ! must be equal to 'OK'
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(KI,KSW) :: ZDIR_ALB   ! Direct albedo at time t , 
REAL, DIMENSION(KI,KSW) :: ZSCA_ALB   ! Diffuse albedo at time t
!
REAL, DIMENSION(KI)     :: ZEMIS      ! Emissivity at time t
REAL, DIMENSION(KI)     :: ZTRAD      ! Radiative temperature at time t
REAL, DIMENSION(KI)  :: ZALB   ! surface albedo
!
REAL, DIMENSION(KI)  :: ZEXNA  ! Exner function at forcing level
REAL, DIMENSION(KI)  :: ZEXNS  ! Exner function at surface level
!
REAL, DIMENSION(KI)  :: ZWIND  ! Wind
REAL, DIMENSION(KI)  :: ZGLOBAL_SW    ! Solar radiation flux at the surface (W/m2) 
REAL, DIMENSION(KI)  :: ZQA    ! Air specific humidity (kg/kg)
!
REAL, DIMENSION(KI)  :: ZUSTAR ! friction velocity (m/s)
REAL, DIMENSION(KI)  :: ZUSTAR2! square of friction velocity (m2/s2)
REAL, DIMENSION(KI)  :: ZSFM   ! flux of momentum (Pa)
!
REAL, DIMENSION(KI)  :: ZRESA_WATER ! aerodynamical resistance
!
!salgado only for inline diagnostics - not used for the moment
!                                      flake don't have it
REAL, DIMENSION(KI)  :: ZCD    ! Drag coefficient
REAL, DIMENSION(KI)  :: ZCDN   ! Neutral Drag coefficient
REAL, DIMENSION(KI)  :: ZCH    ! Heat transfer coefficient
REAL, DIMENSION(KI)  :: ZCE    ! Heat transfer coefficient
REAL, DIMENSION(KI)  :: ZRI    ! Richardson number
REAL, DIMENSION(KI)  :: ZHU    ! Near surface relative humidity
REAL, DIMENSION(KI)  :: ZZ0    ! roughness length
REAL, DIMENSION(KI)  :: ZZ0H   ! heat roughness length
REAL, DIMENSION(KI)  :: ZQSAT  ! humidity at saturation
REAL, DIMENSION(KI)  :: ZTSTEP ! time-step
!
REAL, DIMENSION(KI)     :: ZMASK      !
!
REAL                       :: ZCONVERTFACM0_SLT, ZCONVERTFACM0_DST
REAL                       :: ZCONVERTFACM3_SLT, ZCONVERTFACM3_DST
REAL                       :: ZCONVERTFACM6_SLT, ZCONVERTFACM6_DST
!
INTEGER                     :: ISWB   ! number of shortwave spectral bands
INTEGER                     :: JSWB   ! loop counter on shortwave spectral bands
!  
INTEGER                 :: ISIZE_WATER! number of points of lake water 
INTEGER                 :: ISIZE_ICE  ! and of lake ice
!
INTEGER                     :: ILUOUT ! output logical unit
!
LOGICAL                 :: GPWG = .FALSE.
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
! Preliminaries:
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('COUPLING_FLAKE_N',0,ZHOOK_HANDLE)
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('COUPLING_FLAKEN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!-------------------------------------------------------------------------------------
! Variables needed by flake:
!-------------------------------------------------------------------------------------
!
ZDIR_ALB   (:,:) = XUNDEF
ZSCA_ALB   (:,:) = XUNDEF
ZEMIS      (:)   = XUNDEF
ZTRAD      (:)   = XUNDEF
!
ZTSTEP(:) = PTSTEP
!
ZEXNS(:)     = (PPS(:)/XP00)**(XRD/XCPD)
ZEXNA(:)     = (PPA(:)/XP00)**(XRD/XCPD)
!
!
ZWIND(:) = SQRT(PU(:)**2+PV(:)**2)
!
ZQA(:) = PQA/PRHOA
!
PSFTS(:,:) = 0.
!
ZHU = 1.
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Time evolution
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
TTIME%TIME = TTIME%TIME + PTSTEP
 CALL ADD_FORECAST_TO_DATE_SURF(TTIME%TDATE%YEAR,TTIME%TDATE%MONTH,TTIME%TDATE%DAY,TTIME%TIME)
!
!----------------------------------------
ZMASK(:) = XTS(:) - XTTS
ISIZE_WATER = COUNT(ZMASK(:)>=0.)
ISIZE_ICE = SIZE(XTS) - ISIZE_WATER
!
PSFU = 0.
PSFV = 0.
ZSFM = 0.
!
SELECT CASE (CFLK_FLUX)
   CASE ('DEF  ')
    CALL WATER_FLUX(XZ0,                                           &
                  PTA, ZEXNA, PRHOA, XTS, ZEXNS, PQA,PRAIN, PSNOW, &
                  XTT, ZWIND, PZREF, PUREF,                        &
                  PPS, ZQSAT,                                      &
                  PSFTH, PSFTQ, ZUSTAR,                            &
                  ZCD, ZCDN, ZCH, ZRI, ZRESA_WATER, ZZ0H           )  

  CASE ('ECUME')
    CALL ECUME_SEAFLUX(XZ0, ZMASK, ISIZE_WATER, ISIZE_ICE,         &
                      PTA, ZEXNA ,PRHOA, XTS, ZEXNS, ZQA, PRAIN,   &
                      PSNOW,                                       &
                      ZWIND, PZREF, PUREF,                         &
                      PPS, XICHCE, LPRECIP,LPWEBB, GPWG, ZQSAT,    &
                      PSFTH, PSFTQ, ZUSTAR,                        &
                      ZCD, ZCDN, ZCH, ZCE, ZRI, ZRESA_WATER, ZZ0H  )

END SELECT
!
IF (CFLK_FLUX=='DEF  ' .OR. CFLK_FLUX=='ECUME') THEN
!
    IF(CIMPLICIT_WIND=='OLD')THEN    
!     old implicitation (m2/s2)
      ZUSTAR2(:) = (ZCD(:)*ZWIND(:)*PPEW_B_COEF(:))/            &
                   (1.0-PRHOA(:)*ZCD(:)*ZWIND(:)*PPEW_A_COEF(:)) 
    ELSE
!     new implicitation (m2/s2)            
      ZUSTAR2(:) = (ZCD(:)*ZWIND(:)*(2.*PPEW_B_COEF(:)-ZWIND(:))) /&
                   (1.0-2.0*PRHOA(:)*ZCD(:)*ZWIND(:)*PPEW_A_COEF(:)) 
            

!                   
      ZWIND(:) = PRHOA(:)*PPEW_A_COEF(:)*ZUSTAR2(:) + PPEW_B_COEF(:)
      ZWIND(:) = MAX(ZWIND(:),0.)
!
      WHERE(PPEW_A_COEF(:)/= 0.)
            ZUSTAR2(:) = MAX( ( ZWIND(:) - PPEW_B_COEF(:) ) / (PRHOA(:)*PPEW_A_COEF(:)), 0.)
      ENDWHERE
!                   
    ENDIF
!    
    WHERE (ZWIND(:)>0.)            
      ZSFM(:) = - PRHOA(:) * ZUSTAR2(:)
      PSFU(:) = ZSFM(:) * PU(:) / ZWIND(:)
      PSFV(:) = ZSFM(:) * PV(:) / ZWIND(:)
    END WHERE
!    
!   PSFTQ become temporarly the flux of heat flux (W/m2)
    PSFTQ = PSFTQ * XLVTT
!
ELSE
   ZUSTAR(:) = XUSTAR(:)
ENDIF
!
!--------------------------------------------------------------------------------------
! Call FLake 
! to compute Fluxes over water if CFLK_FLUX=='FLAKE'
! to actualize FLake variables, namely water surface temperature
!--------------------------------------------------------------------------------------
!
ZZ0 = XZ0
!
!----------------------------------------
!radiative properties at t
!----------------------------------------
!
ISWB = SIZE(PSW_BANDS)
!
DO JSWB=1,ISWB 
  ZDIR_ALB(:,JSWB) = XDIR_ALB(:)
  ZSCA_ALB(:,JSWB) = XSCA_ALB(:)
END DO
!
ZEMIS  = XEMIS
!
 CALL FLAKE_ALBEDO(PDIR_SW,PSCA_SW,KSW,ZDIR_ALB,ZSCA_ALB,ZGLOBAL_SW,ZALB)
!
 CALL FLAKE_INTERFACE( KI, &
! Atmospheric forcing:
                       PSNOW, ZGLOBAL_SW, PLW, PUREF, PZREF, ZWIND, PTA, ZQA, PPS, &
! Constant parameters
                       XWATER_DEPTH, XWATER_FETCH, XDEPTH_BS, XT_BS, XCORIO,&
                       ZTSTEP,                                              &
! surface albedo
                       ZEMIS, ZALB,                                         &
! Parameters that may change (constants for the moment)
                       XICE_ALB, XSNOW_ALB, XEXTCOEF_WATER,                 &
                       XEXTCOEF_ICE, XEXTCOEF_SNOW,                         &
! Flake variables
                       XT_SNOW, XT_ICE, XT_MNW, XT_WML, XT_BOT, XT_B1, XCT, &
                       XH_SNOW, XH_ICE, XH_ML, XH_B1, XTS,                  &
! Surface heat and momentum fluxes
                       PSFTH, PSFTQ, ZSFM, ZZ0, ZZ0H, ZRI, ZUSTAR, ZCD,     &
! Flags              
                       LSEDIMENTS, CFLK_FLUX, PPEW_A_COEF, PPEW_B_COEF,     &
                       PRHOA, CIMPLICIT_WIND                                )
!
IF (CFLK_FLUX=='FLAKE') then 
    XZ0 = ZZ0
ENDIF
!
!-------------------------------------------------------------------------------------
! Outputs:
!-------------------------------------------------------------------------------------
!
! Momentum fluxes
!
IF (CFLK_FLUX=='FLAKE') THEN
   PSFU = 0.
   PSFV = 0.
  WHERE (ZWIND(:)>0.)
    PSFU(:) = ZSFM(:) * PU(:) / ZWIND(:)
    PSFV(:) = ZSFM(:) * PV(:) / ZWIND(:)
  END WHERE
  !
  ! 
  ! ZUSTAR and ZRESA_WATER are not in Flake but are needed to the ch_* routines
  !
  ZUSTAR(:)       = SQRT (ABS(ZSFM(:))/PRHOA(:))
  ZEXNS (:)       = (PPS(:)/XP00)**(XRD/XCPD)
  ZEXNA (:)       = (PPA(:)/XP00)**(XRD/XCPD)
  ZRESA_WATER=2.E4
  WHERE (PSFTH/=0.)
  ZRESA_WATER (:) = XCPD * PRHOA(:) * (XTS(:) - PTA(:) * ZEXNS(:)/ZEXNA(:)) &
                     / (PSFTH(:) * ZEXNS(:))  
  END WHERE
  !
ENDIF
!                               
XUSTAR(:) = ZUSTAR(:)
!                               
! flux of water vapor (kg/m2/s)
PSFTQ = PSFTQ / XLVTT
!
! CO2 flux
!
PSFCO2(:)       =  0.0    ! Assumes no CO2 emission over water bodies
!
!----------------------------------------
!radiative properties at t
!----------------------------------------
!
ZTRAD  = XTS
!
!-------------------------------------------------------------------------------------
! Scalar fluxes:
!-------------------------------------------------------------------------------------
!
!
!salgado The scalar fluxes are computed as in watflux
IF (NBEQ>0) THEN
  IF (CCH_DRY_DEP == "WES89") THEN
    CALL CH_DEP_WATER  (ZRESA_WATER, ZUSTAR, PTA, PTRAD,      &
                          PSV(:,NSV_CHSBEG:NSV_CHSEND),       &
                          CSV(NSV_CHSBEG:NSV_CHSEND),         &
                          XDEP(:,1:NBEQ) )  

   PSFTS(:,NSV_CHSBEG:NSV_CHSEND) = - PSV(:,NSV_CHSBEG:NSV_CHSEND)  &
                                               * XDEP(:,1:NBEQ)  
     IF (NAEREQ > 0 ) THEN
        CALL CH_AER_DEP(PSV(:,NSV_AERBEG:NSV_AEREND),&
                          PSFTS(:,NSV_AERBEG:NSV_AEREND),&
                          ZUSTAR,ZRESA_WATER,PTA,PRHOA)     
      END IF

  ELSE
    PSFTS(:,NSV_CHSBEG:NSV_CHSEND) =0.
    IF(NSV_AERBEG.LT.NSV_AEREND) PSFTS(:,NSV_AERBEG:NSV_AEREND) =0.
  ENDIF
ENDIF

IF (NDSTEQ>0) THEN
  CALL DSLT_DEP(PSV(:,NSV_DSTBEG:NSV_DSTEND), PSFTS(:,NSV_DSTBEG:NSV_DSTEND),   &
                ZUSTAR, ZRESA_WATER, PTA, PRHOA, XEMISSIG_DST, XEMISRADIUS_DST, &
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
                ZUSTAR, ZRESA_WATER, PTA, PRHOA, XEMISSIG_SLT, XEMISRADIUS_SLT, &
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
IF (CFLK_FLUX=='FLAKE') THEN  !compute some variables not present in FLake code
  ZCH = 1.E-7
!
  WHERE (ABS((XTS(:) - PTA(:) * ZEXNS(:)/ZEXNA(:))) > 1.E-2 .AND. ZWIND(:)/=0.)
     ZCH = MAX(1.E-7,PSFTH / (XCPD * PRHOA(:) * ZWIND(:) * (XTS(:) - PTA(:) * ZEXNS(:)/ZEXNA(:))) * ZEXNS(:))
  END WHERE
!
!
  ZCDN = (XKARMAN/LOG(PUREF(:)/XZ0(:)))**2
!
  ZQSAT(:) = QSAT(XTS(:),PPS(:))
ENDIF
!
 CALL DIAG_INLINE_FLAKE_n(PTA, XTS, ZQA, PPA, PPS, PRHOA, PU, PV, PZREF, PUREF,  &
                         ZCD, ZCDN, ZCH, ZRI, ZHU, XZ0, ZZ0H, ZQSAT,            &
                         PSFTH, PSFTQ, PSFU, PSFV, PDIR_SW, PSCA_SW, PLW,       &
                         ZDIR_ALB, ZSCA_ALB, ZEMIS, ZTRAD                       )  
!
!-------------------------------------------------------------------------------------
!
 CALL DIAG_MISC_FLAKE_n(XT_WML,XT_BOT,XH_ML,XCT,XWATER_DEPTH)
!
 CALL UPDATE_RAD_SEAWAT(CFLK_ALB,XTS,PZENITH2,XTT,XEMIS,XDIR_ALB, &
                       XSCA_ALB,PDIR_ALB,PSCA_ALB,PEMIS,PTRAD    )
!                         
IF (LHOOK) CALL DR_HOOK('COUPLING_FLAKE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE COUPLING_FLAKE_n
