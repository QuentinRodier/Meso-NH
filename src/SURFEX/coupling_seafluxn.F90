!     ###############################################################################
SUBROUTINE COUPLING_SEAFLUX_n(HPROGRAM, HCOUPLING,                                           &
                 PTSTEP, KYEAR, KMONTH, KDAY, PTIME, KI, KSV, KSW, PTSUN, PZENITH, PZENITH2, &
                 PAZIM, PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV,          &
                 PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA,                   &
                 PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,                                    &
                 PTRAD, PDIR_ALB, PSCA_ALB, PEMIS,                                           &
                 PPEW_A_COEF, PPEW_B_COEF,                                                   &
                 PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,                         &
                 HTEST                                                                       )  
!     ###############################################################################
!
!!****  *COUPLING_SEAFLUX_n * - Driver of the WATER_FLUX scheme for sea   
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
!!      Modified    01/2006 : sea flux parameterization.
!!      Modified    09/2006 : P. Tulet Introduce Sea salt aerosol Emission/Deposition
!!      Modified    03/2009 : B. Decharme SST could change during a run => ALB and EMIS 
!!      Modified    05/2009 : V. Masson : implicitation of momentum fluxes
!!      Modified    09/2009 : B. Decharme Radiative properties at time t+1
!!      Modified    01/2010 : B. Decharme Add XTTS
!!      Modified    09/2012 : B. Decharme New wind implicitation
!!      Modified    10/2012 : P. Le Moigne CMO1D update
!!---------------------------------------------------------------------
!
USE MODD_CSTS,       ONLY : XRD, XCPD, XP00, XLVTT, XTT, XTTS, XDAY
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SURF_ATM,   ONLY : LCPL_ESM, CIMPLICIT_WIND
!
USE MODD_DATA_SEAFLUX_n,  ONLY : LSST_DATA
USE MODD_SEAFLUX_n,  ONLY : XSST, XTICE, XZ0, XDIR_ALB, XSCA_ALB, XEMIS, TTIME, &
                              CSEA_ALB, CSEA_FLUX, XUMER, XVMER, LINTERPOL_SST, &
                              XICHCE, LPRECIP, LPWEBB , LPWG

USE MODD_OCEAN_n, ONLY : LMERCATOR                            
USE MODD_CH_SEAFLUX_n, ONLY : CSV, CCH_DRY_DEP, XDEP, NBEQ, NSV_CHSBEG, NSV_CHSEND,&
                                NSV_DSTBEG, NSV_DSTEND, NAEREQ, NDSTEQ, NSLTEQ, &
                                NSV_AERBEG, NSV_AEREND, NSV_SLTBEG, NSV_SLTEND  
!
USE MODI_WATER_FLUX
USE MODI_MR98
USE MODI_ECUME_SEAFLUX
USE MODI_COARE30_SEAFLUX
USE MODI_ADD_FORECAST_TO_DATE_SURF
USE MODI_MOD1D_n
USE MODI_DIAG_INLINE_SEAFLUX_n
USE MODI_CH_AER_DEP
USE MODI_CH_DEP_WATER
USE MODI_DSLT_DEP
USE MODI_SST_UPDATE
USE MODI_INTERPOL_SST_MTH
USE MODI_UPDATE_RAD_SEAWAT
!
USE MODE_DSLT_SURF
USE MODD_DST_SURF
USE MODD_SLT_SURF
USE MODD_DST_n,    ONLY: XEMISRADIUS_DST, XEMISSIG_DST
USE MODD_SLT_n,    ONLY: XEMISRADIUS_SLT, XEMISSIG_SLT
! 
USE MODD_SEAFLUX_GRID_n, ONLY : XLAT
USE MODD_OCEAN_CSTS,   ONLY : NOCKMIN
USE MODD_OCEAN_REL_n,      ONLY : XSEAT_REL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_COUPLING_ICEFLUX_n
!
USE MODI_COUPLING_SLT_n
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
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH   ! zenithal angle at t  (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH2  ! zenithal angle at t+1(radian from the vertical)
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
REAL, DIMENSION(KI,KSW) :: ZDIR_ALB   ! Direct albedo at time t
REAL, DIMENSION(KI,KSW) :: ZSCA_ALB   ! Diffuse albedo at time t
!
REAL, DIMENSION(KI) :: ZEXNA      ! Exner function at forcing level
REAL, DIMENSION(KI) :: ZEXNS      ! Exner function at surface level
REAL, DIMENSION(KI) :: ZU         ! zonal wind
REAL, DIMENSION(KI) :: ZV         ! meridian wind
REAL, DIMENSION(KI) :: ZWIND      ! Wind
REAL, DIMENSION(KI) :: ZCD        ! Drag coefficient
REAL, DIMENSION(KI) :: ZCDN       ! Neutral Drag coefficient
REAL, DIMENSION(KI) :: ZCH        ! Heat transfer coefficient
REAL, DIMENSION(KI) :: ZCE        ! Vaporization heat transfer coefficient
REAL, DIMENSION(KI) :: ZRI        ! Richardson number
REAL, DIMENSION(KI) :: ZHU        ! Near surface relative humidity
REAL, DIMENSION(KI) :: ZRESA_SEA  ! aerodynamical resistance
REAL, DIMENSION(KI) :: ZUSTAR     ! friction velocity (m/s)
REAL, DIMENSION(KI) :: ZUSTAR2    ! square of friction velocity (m2/s2)
REAL, DIMENSION(KI) :: ZZ0H       ! heat roughness length over sea
REAL, DIMENSION(KI) :: ZQSAT      ! humidity at saturation
REAL, DIMENSION(KI) :: ZQA        ! specific humidity (kg/kg)
REAL, DIMENSION(KI) :: ZEMIS      ! Emissivity at time t
REAL, DIMENSION(KI) :: ZTRAD      ! Radiative temperature at time t
REAL, DIMENSION(KI) :: ZSFTH_ICE  ! Sea ice flux of heat
REAL, DIMENSION(KI) :: ZSFTQ_ICE  ! Sea ice flux of ice sublimation
!
REAL, DIMENSION(KI)              :: ZMASK
!
REAL                             :: ZCONVERTFACM0_SLT, ZCONVERTFACM0_DST
REAL                             :: ZCONVERTFACM3_SLT, ZCONVERTFACM3_DST
REAL                             :: ZCONVERTFACM6_SLT, ZCONVERTFACM6_DST
!
INTEGER                          :: ISIZE_WATER  ! number of points of sea water 
INTEGER                          :: ISIZE_ICE    ! and of sea ice
!
INTEGER                          :: ISWB       ! number of shortwave spectral bands
INTEGER                          :: JSWB       ! loop counter on shortwave spectral bands
INTEGER                          :: ISLT       ! number of sea salt variable
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
! Preliminaries:
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('COUPLING_SEAFLUX_N',0,ZHOOK_HANDLE)
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('COUPLING_SEAFLUXN: FATAL ERROR DURING ARGUMENT TRANSFER')        
END IF
!-------------------------------------------------------------------------------------
!
ZEXNA    (:) = XUNDEF
ZEXNS    (:) = XUNDEF
ZU       (:) = XUNDEF
ZV       (:) = XUNDEF
ZWIND    (:) = XUNDEF
ZCD      (:) = XUNDEF    
ZCDN     (:) = XUNDEF
ZCH      (:) = XUNDEF
ZCE      (:) = XUNDEF
ZRI      (:) = XUNDEF
ZHU      (:) = XUNDEF
ZRESA_SEA(:) = XUNDEF
ZUSTAR   (:) = XUNDEF
ZUSTAR2  (:) = XUNDEF
ZZ0H     (:) = XUNDEF
ZQSAT    (:) = XUNDEF
ZEMIS    (:) = XUNDEF
ZTRAD    (:) = XUNDEF
ZDIR_ALB (:,:) = XUNDEF
ZSCA_ALB (:,:) = XUNDEF
!
IF(LCPL_ESM)THEN
  ZSFTQ_ICE(:) = XUNDEF
  ZSFTH_ICE(:) = XUNDEF
ENDIF
!
!-------------------------------------------------------------------------------------
!
ZEXNS(:)     = (PPS(:)/XP00)**(XRD/XCPD)
ZEXNA(:)     = (PPA(:)/XP00)**(XRD/XCPD)
!
IF(LCPL_ESM)THEN 
  !Sea currents are taken into account
  ZU(:)=PU(:)-XUMER(:)
  ZV(:)=PV(:)-XVMER(:)
ELSE
  ZU(:)=PU(:)
  ZV(:)=PV(:)        
ENDIF
!
ZWIND(:) = SQRT(ZU(:)**2+ZV(:)**2)
!
PSFTS(:,:) = 0.
!
ZHU = 1.
!
ZQA(:) = PQA(:) / PRHOA(:)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! update sea surface temperature
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (LSST_DATA .AND. (.NOT. LMERCATOR)) CALL SST_UPDATE(XSST, TTIME)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Time evolution
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
TTIME%TIME = TTIME%TIME + PTSTEP
 CALL ADD_FORECAST_TO_DATE_SURF(TTIME%TDATE%YEAR,TTIME%TDATE%MONTH,TTIME%TDATE%DAY,TTIME%TIME)
!
!--------------------------------------------------------------------------------------
! Fluxes over water according to Charnock formulae
!--------------------------------------------------------------------------------------
!
ZMASK(:) = XSST(:) - XTTS
ISIZE_WATER = COUNT(ZMASK(:)>=0.)
ISIZE_ICE = SIZE(XSST) - ISIZE_WATER
!
SELECT CASE (CSEA_FLUX)
  CASE ('DIRECT')
    CALL WATER_FLUX(XZ0,                                              &
                      PTA, ZEXNA, PRHOA, XSST, ZEXNS, ZQA, PRAIN,     &
                      PSNOW, XTTS,                                    &
                      ZWIND, PZREF, PUREF,                            &
                      PPS, ZQSAT,                                     &
                      PSFTH, PSFTQ, ZUSTAR,                           &
                      ZCD, ZCDN, ZCH, ZRI, ZRESA_SEA, ZZ0H            )  
  CASE ('ITERAT')
    CALL MR98      (XZ0,                                              &
                      PTA, ZEXNA, PRHOA, XSST, ZEXNS, ZQA,            &
                      XTTS,                                           &
                      ZWIND, PZREF, PUREF,                            &
                      PPS, ZQSAT,                                     &
                      PSFTH, PSFTQ, ZUSTAR,                           &
                      ZCD, ZCDN, ZCH, ZRI, ZRESA_SEA, ZZ0H            )  

  CASE ('ECUME ')
    CALL ECUME_SEAFLUX(XZ0, ZMASK, ISIZE_WATER, ISIZE_ICE,            &
                      PTA, ZEXNA ,PRHOA, XSST, ZEXNS, ZQA, PRAIN,     &
                      PSNOW,                                          &
                      ZWIND, PZREF, PUREF,                            &
                      PPS, XICHCE, LPRECIP, LPWEBB, LPWG, ZQSAT,      &
                      PSFTH, PSFTQ, ZUSTAR,                           &
                      ZCD, ZCDN, ZCH, ZCE, ZRI, ZRESA_SEA, ZZ0H       )  

  CASE ('COARE3')
    CALL COARE30_SEAFLUX(XZ0, ZMASK, ISIZE_WATER, ISIZE_ICE,            &
                      PTA, ZEXNA ,PRHOA, XSST, ZEXNS, ZQA, PRAIN,       &
                      PSNOW,                                            &
                      ZWIND, PZREF, PUREF,                              &
                      PPS, ZQSAT,                                       &
                      PSFTH, PSFTQ, ZUSTAR,                             &
                      ZCD, ZCDN, ZCH, ZCE, ZRI, ZRESA_SEA, ZZ0H       )  
END SELECT
!                
!-------------------------------------------------------------------------------------
! Outputs:
!-------------------------------------------------------------------------------------
!
! Momentum fluxes
!
IF(CIMPLICIT_WIND=='OLD')THEN
! old implicitation (m2/s2)
  ZUSTAR2(:) = (ZCD(:)*ZWIND(:)*PPEW_B_COEF(:)) /            &
              (1.0-PRHOA(:)*ZCD(:)*ZWIND(:)*PPEW_A_COEF(:))
ELSE
! new implicitation (m2/s2)
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
PSFU = 0.
PSFV = 0.
WHERE (ZWIND(:)>0.)
  PSFU(:) = - PRHOA(:) * ZUSTAR2(:) * ZU(:) / ZWIND(:)
  PSFV(:) = - PRHOA(:) * ZUSTAR2(:) * ZV(:) / ZWIND(:)
END WHERE
!
! CO2 flux
!
! PSFCO2 = E * deltapCO2 
! According to Wanninkhof (medium hypothesis) : 
! E = 1.13.10^-3 * WIND^2 CO2mol.m-2.yr-1.µatm-1 
!   = 1.13.10^-3 * WIND^2 * Mco2.10^-3 * (1/365*24*3600)
! deltapCO2 = -8.7 µatm (Table 1 half hypothesis)

PSFCO2(:) = - ZWIND(:)**2 * 1.13E-3 * 8.7 * 44.E-3 / ( 365*24*3600 )
!
!
!-------------------------------------------------------------------------------------
!radiative properties at time t
!-------------------------------------------------------------------------------------
!
ISWB = SIZE(PSW_BANDS)
!
DO JSWB=1,ISWB
  ZDIR_ALB(:,JSWB) = XDIR_ALB(:)
  ZSCA_ALB(:,JSWB) = XSCA_ALB(:)
END DO
!
ZEMIS  = XEMIS
ZTRAD  = XSST
!
!-------------------------------------------------------------------------------------
!Specific fields for GELATO when using earth system model 
!(intermediate step before explicit sea and ice fluxes comutation)
!-------------------------------------------------------------------------------------
!
IF(LCPL_ESM)THEN
  CALL COUPLING_ICEFLUX_n(KI, PTA, ZEXNA, PRHOA, XTICE, ZEXNS, &
                            ZQA, PRAIN, PSNOW, ZWIND, PZREF, PUREF,    &
                            PPS, XSST, XTTS, ZSFTH_ICE, ZSFTQ_ICE      )  
ENDIF
!
!-------------------------------------------------------------------------------------
! Scalar fluxes:
!-------------------------------------------------------------------------------------
!
IF (NBEQ>0) THEN
  IF (CCH_DRY_DEP == "WES89") THEN

    CALL CH_DEP_WATER  (ZRESA_SEA, ZUSTAR, PTA, ZTRAD,      &
                          PSV(:,NSV_CHSBEG:NSV_CHSEND),       &
                          CSV(NSV_CHSBEG:NSV_CHSEND),         &
                          XDEP(:,1:NBEQ) )  

   PSFTS(:,NSV_CHSBEG:NSV_CHSEND) = - PSV(:,NSV_CHSBEG:NSV_CHSEND)  &
                                               * XDEP(:,1:NBEQ)  
     IF (NAEREQ > 0 ) THEN
        CALL CH_AER_DEP(PSV(:,NSV_AERBEG:NSV_AEREND),&
                          PSFTS(:,NSV_AERBEG:NSV_AEREND),&
                          ZUSTAR,ZRESA_SEA,PTA,PRHOA)     
      END IF

  ELSE
    PSFTS(:,NSV_CHSBEG:NSV_CHSEND) =0.
    IF (NSV_AEREND.GT.NSV_AERBEG)     PSFTS(:,NSV_AERBEG:NSV_AEREND) =0.
  ENDIF
ENDIF
!
IF (NSLTEQ>0) THEN
  ISLT = NSV_SLTEND - NSV_SLTBEG + 1

  CALL COUPLING_SLT_n(           &
       SIZE(ZUSTAR,1),           & !I [nbr] number of sea point
       ISLT,                     & !I [nbr] number of sea salt variables
       ZWIND,                    & !I [m/s] wind velocity
       PSFTS(:,NSV_SLTBEG:NSV_SLTEND) )   
ENDIF
!
IF (NDSTEQ>0) THEN
  CALL DSLT_DEP(PSV(:,NSV_DSTBEG:NSV_DSTEND), PSFTS(:,NSV_DSTBEG:NSV_DSTEND),   &
                ZUSTAR, ZRESA_SEA, PTA, PRHOA, XEMISSIG_DST, XEMISRADIUS_DST,   &
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
                ZUSTAR, ZRESA_SEA, PTA, PRHOA, XEMISSIG_SLT, XEMISRADIUS_SLT,   &
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
!-------------------------------------------------------------------------------
! OCEANIC COUPLING
!-------------------------------------------------------------------------------
  IF (LMERCATOR) THEN

    ! Update SST reference profile for relaxation purpose
    IF (LSST_DATA) THEN
      CALL SST_UPDATE(XSEAT_REL(:,NOCKMIN+1), TTIME)
      !
      ! Convert to degree C for ocean model
      XSEAT_REL(:,NOCKMIN+1) = XSEAT_REL(:,NOCKMIN+1) - XTT
    ENDIF
    !
    CALL MOD1D_n(HPROGRAM,PTIME,ZEMIS(:),ZDIR_ALB(:,1:KSW),ZSCA_ALB(:,1:KSW),&
                 PLW(:),PSCA_SW(:,1:KSW),PDIR_SW(:,1:KSW),PSFTH(:),          &
                 PSFTQ(:),PSFU(:),PSFV(:),PRAIN(:),XSST(:))
   
  ENDIF
!
!-------------------------------------------------------------------------------
! Inline diagnostics at time t
!-------------------------------------------------------------------------------
!
 CALL DIAG_INLINE_SEAFLUX_n(PTSTEP, PTA, XSST, ZQA, PPA, PPS, PRHOA, PU, PV, PZREF,&
                             PUREF,ZCD, ZCDN, ZCH, ZCE, ZRI, ZHU, XZ0, ZZ0H, ZQSAT, &
                             PSFTH, PSFTQ, PSFU, PSFV, PDIR_SW, PSCA_SW, PLW,       &
                             ZDIR_ALB, ZSCA_ALB, ZEMIS, ZTRAD, PRAIN, PSNOW,        &
                             XTICE, ZSFTH_ICE, ZSFTQ_ICE                            )  
!
!-------------------------------------------------------------------------------
!Radiative properties at time t+1 (see by the atmosphere) in order to close
!the energy budget between surfex and the atmosphere
!-------------------------------------------------------------------------------
!
IF (LINTERPOL_SST.AND.MOD(TTIME%TIME,XDAY) == 0.) THEN
   CALL INTERPOL_SST_MTH(TTIME%TDATE%YEAR,TTIME%TDATE%MONTH,TTIME%TDATE%DAY,XSST)
ENDIF
!
 CALL UPDATE_RAD_SEAWAT(CSEA_ALB,XSST,PZENITH2,XTTS,XEMIS,XDIR_ALB, &
                         XSCA_ALB,PDIR_ALB,PSCA_ALB,PEMIS,PTRAD      )  
!
!=======================================================================================
!
IF (LHOOK) CALL DR_HOOK('COUPLING_SEAFLUX_N',1,ZHOOK_HANDLE)
END SUBROUTINE COUPLING_SEAFLUX_n
