!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE COUPLING_ISBA_n (DTCO, UG, U, USS, IM, DTGD, DTGR, TGRO, DST, SLT,   &
                             HPROGRAM, HCOUPLING,                                              &
                 PTSTEP, KYEAR, KMONTH, KDAY, PTIME, KI, KSV, KSW, PTSUN, PZENITH, PZENITH2, &
                 PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV,                 &
                 PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA,                   &
                 PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,                                    &
                 PTRAD, PDIR_ALB, PSCA_ALB, PEMIS, PTSURF, PZ0, PZ0H, PQSURF,                &
                 PPEW_A_COEF, PPEW_B_COEF,                                                   &
                 PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,                         &
                 HTEST                                                                       )  
!     ###############################################################################
!
!!****  *COUPLING_ISBA_n * - Driver for ISBA time step   
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!! First, all actions dependant on each patch is donbe independantly
!!     (loop on patches)
!! Second, actions common to all patches (e.g. prescription of new vegetation)
!! Third, energy fluxes are averaged
!!
!! Nota that chemical fluxes are also treated.
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
!!      P Le Moigne 11/2004 add new diagnostics for isba
!!      A.Bogatchev 09/2005 EBA snow option
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      P Le Moigne 02/2006 z0h with snow
!!      P.Le Moigne 06/2006 seeding and irrigation
!!      B. Decharme   2008  reset the subgrid topographic effect on the forcing
!!                          PSNV allways <= PSNG
!!                          News diag
!!                          Flooding scheme and allows TRIP variables coupling
!!      A.L. Gibelin 04/2009 : Add respiration diagnostics
!!      A.L. Gibelin 04/2009 : BIOMASS and RESP_BIOMASS arrays 
!!      A.L. Gibelin 04/2009 : TAU_WOOD for NCB option 
!!      A.L. Gibelin 05/2009 : Add carbon spinup
!!      A.L. Gibelin 06/2009 : Soil carbon variables for CNT option
!!      A.L. Gibelin 07/2009 : Suppress RDK and transform GPP as a diagnostic
!!      A.L. Gibelin 07/2009 : Suppress PPST and PPSTF as outputs
!!        S.Lafont   01/2011 : add PTSTEP as arg of diag_misc
!!       B.Decharme  09/2012 : Bug in hydro_glacier calculation with ES or Crocus
!!                             New wind implicitation
!!                             New soil carbon spinup and diag
!!                             Isba budget
!!      F. Bouttier  01/2013 : Apply random perturbations for ensembles
!!      B. Decharme  04/2013 new coupling variables
!!                           Subsurface runoff if SGH (DIF option only)
!!                   07/2013 Surface / Water table depth coupling
!!      P Samuelsson 10/2014 : MEB
!!      P. LeMoigne  12/2014 EBA scheme update
!!      R. Seferian  05/2015 : Add coupling fiels to vegetation_evol call
!!      B. Decharme    01/16 : Bug with flood budget
!!      B. Decharme    01/16 : Bug when vegetation veg, z0 and emis are imposed whith interactive vegetation
!!-------------------------------------------------------------------
!
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
USE MODD_DATA_TEB_GARDEN_n, ONLY : DATA_TEB_GARDEN_t
USE MODD_DATA_TEB_GREENROOF_n, ONLY : DATA_TEB_GREENROOF_t
USE MODD_TEB_GREENROOF_OPTION_n, ONLY : TEB_GREENROOF_OPTIONS_t
USE MODD_DST_n, ONLY : DST_t
USE MODD_SLT_n, ONLY : SLT_t
!
USE MODD_REPROD_OPER, ONLY : CIMPLICIT_WIND
!
USE MODD_CSTS,         ONLY : XRD, XRV, XP00, XCPD, XPI, XAVOGADRO, XMD
USE MODD_CO2V_PAR,     ONLY : XMCO2, XSPIN_CO2
!
USE MODD_SURF_PAR,     ONLY : XUNDEF
USE MODD_SNOW_PAR,     ONLY : XZ0SN
USE MODD_TYPE_DATE_SURF
!
USE MODD_SURF_ATM,    ONLY : LNOSOF
!
USE MODD_DST_SURF
USE MODD_SLT_SURF
USE MODE_DSLT_SURF
USE MODE_MEB
!
!
!                         

USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK
!
USE MODD_AGRI,           ONLY : LAGRIP
USE MODD_DEEPSOIL,       ONLY : LDEEPSOIL
!
#ifdef TOPD
USE MODD_COUPLING_TOPD,  ONLY : LCOUPL_TOPD, NMASKT_PATCH
#endif
!
USE MODI_IRRIGATION_UPDATE
USE MODI_ADD_FORECAST_TO_DATE_SURF
USE MODI_Z0EFF
USE MODI_ISBA
USE MODI_AVERAGE_FLUX
USE MODI_AVERAGE_PHY
USE MODI_AVERAGE_RAD
USE MODI_AVERAGE_DIAG_ISBA_n
USE MODI_VEGETATION_EVOL
USE MODI_VEGETATION_UPDATE
USE MODI_ALBEDO_VEG_UPDATE
USE MODI_CARBON_EVOL
USE MODI_SUBSCALE_Z0EFF
USE MODI_SOIL_ALBEDO
USE MODI_ALBEDO
USE MODI_DIAG_INLINE_ISBA_n
USE MODI_DIAG_EVAP_ISBA_n
USE MODI_DIAG_MISC_ISBA_n
!
USE MODI_UPDATE_RAD_ISBA_n
USE MODI_DEEPSOIL_UPDATE
USE MODI_ISBA_SGH_UPDATE
USE MODI_ISBA_FLOOD_PROPERTIES
USE MODI_DIAG_CPL_ESM_ISBA
USE MODI_HYDRO_GLACIER
USE MODI_ISBA_ALBEDO
USE MODI_CARBON_SPINUP
USE MODI_PACK_ISBA_PATCH_n    
USE MODI_PACK_ISBA_PATCH_GET_SIZE_n
USE MODI_PACK_CH_ISBA_PATCH_n     
USE MODI_PACK_DIAG_PATCH_n
USE MODI_PACK_DIAG_PATCH_GET_SIZE_n
USE MODI_UNPACK_ISBA_PATCH_n     
USE MODI_UNPACK_CH_ISBA_PATCH_n     
USE MODI_UNPACK_DIAG_PATCH_n     
USE MODI_CH_AER_DEP
USE MODI_ABOR1_SFX
USE MODI_AVERAGE_DIAG_EVAP_ISBA_n
USE MODI_AVERAGE_DIAG_MISC_ISBA_n
USE MODI_CH_BVOCEM_n
USE MODI_SOILEMISNO_n
USE MODI_CH_DEP_ISBA
USE MODI_DSLT_DEP
USE MODI_COUPLING_DST_n
USE MODI_COUPLING_SURF_TOPD
USE MODI_ISBA_BUDGET_INIT
USE MODI_ISBA_BUDGET
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
TYPE(DATA_TEB_GARDEN_t), INTENT(INOUT) :: DTGD
TYPE(DATA_TEB_GREENROOF_t), INTENT(INOUT) :: DTGR
TYPE(TEB_GREENROOF_OPTIONS_t), INTENT(INOUT) :: TGRO
TYPE(DST_t), INTENT(INOUT) :: DST
TYPE(SLT_t), INTENT(INOUT) :: SLT
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
!   
 CHARACTER(LEN=6), DIMENSION(KSV),INTENT(IN):: HSV  ! name of all scalar variables!
REAL, DIMENSION(KI), INTENT(IN)  :: PU        ! zonal wind                            (m/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PV        ! meridian wind                         (m/s)
REAL, DIMENSION(KI,KSW),INTENT(IN) :: PDIR_SW ! direct  solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI,KSW),INTENT(IN) :: PSCA_SW ! diffuse solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KSW),INTENT(IN)  :: PSW_BANDS ! mean wavelength of each shortwave band (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH   ! zenithal angle at t  (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH2  ! zenithal angle at t+1(radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PLW       ! longwave radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI), INTENT(IN)  :: PPS       ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PPA       ! pressure at forcing level             (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PZS       ! atmospheric model orography           (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PCO2      ! CO2 concentration in the air          (kg_CO2/m3)
REAL, DIMENSION(KI), INTENT(IN)  :: PSNOW     ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PRAIN     ! liquid precipitation                  (kg/m2/s)
!
!
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTH     ! flux of heat                          (W/m2)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTQ     ! flux of water vapor                   (kg/m2/s)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFU      ! zonal momentum flux                   (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFV      ! meridian momentum flux                (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFCO2    ! flux of CO2 positive toward the atmosphere (m/s*kg_CO2/kg_air)
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
!* forcing variables
!
REAL, DIMENSION(KI)     :: ZWIND    ! lowest atmospheric level wind speed           (m/s)
REAL, DIMENSION(KI)     :: ZDIR     ! wind direction                        (rad from N clockwise)
REAL, DIMENSION(KI)     :: ZEXNA    ! Exner function at lowest atmospheric level    (-)
REAL, DIMENSION(KI)     :: ZEXNS    ! Exner function at surface                     (-)
REAL, DIMENSION(KI)     :: ZALFA    ! Wind direction                                (-)
REAL, DIMENSION(KI)     :: ZQA      ! specific humidity                             (kg/kg)
REAL, DIMENSION(KI)     :: ZCO2     ! CO2 concentration                             (kg/kg)
REAL                    :: ZSPINCO2 ! CO2 concentration                             (ppmv)
REAL, DIMENSION(KI)     :: ZPEQ_A_COEF ! specific humidity implicit
REAL, DIMENSION(KI)     :: ZPEQ_B_COEF ! coefficients (hum. in kg/kg)
!
INTEGER                 ::ISPINEND
!
! Patch outputs:
!
REAL, DIMENSION(KI,IM%I%NPATCH) :: ZSFTH_TILE     ! surface heat flux (W/m2)
REAL, DIMENSION(KI,IM%I%NPATCH) :: ZSFTQ_TILE     ! surface vapor flux (kg/m2/s)
REAL, DIMENSION(KI,IM%I%NPATCH) :: ZSFCO2_TILE    ! surface CO2 flux positive toward the atmosphere (m/s*kg_CO2/kg_air)
REAL, DIMENSION(KI,IM%I%NPATCH) :: ZSFU_TILE      ! zonal momentum flux
REAL, DIMENSION(KI,IM%I%NPATCH) :: ZSFV_TILE      ! meridian momentum flux
REAL, DIMENSION(KI,IM%I%NPATCH) :: ZTRAD_TILE     ! radiative surface temperature
REAL, DIMENSION(KI,IM%I%NPATCH) :: ZEMIS_TILE     ! emissivity
REAL, DIMENSION(KI,IM%I%NPATCH) :: ZTSURF_TILE    ! surface effective temperature
REAL, DIMENSION(KI,IM%I%NPATCH) :: ZZ0_TILE       ! roughness length for momentum
REAL, DIMENSION(KI,IM%I%NPATCH) :: ZZ0H_TILE      ! roughness length for heat
REAL, DIMENSION(KI,IM%I%NPATCH) :: ZQSURF_TILE    ! specific humidity at surface
REAL, DIMENSION(KI,KSW,IM%I%NPATCH) :: ZDIR_ALB_TILE  ! direct albedo
REAL, DIMENSION(KI,KSW,IM%I%NPATCH) :: ZSCA_ALB_TILE  ! diffuse albedo
REAL, DIMENSION(KI,KSV,IM%I%NPATCH) :: ZSFTS_TILE     ! scalar surface flux
!
REAL, DIMENSION(KI, IM%I%NPATCH) :: ZCPL_DRAIN     ! For the coupling with TRIP
REAL, DIMENSION(KI, IM%I%NPATCH) :: ZCPL_RUNOFF    ! For the coupling with TRIP
REAL, DIMENSION(KI, IM%I%NPATCH) :: ZCPL_EFLOOD    ! For the coupling with TRIP
REAL, DIMENSION(KI, IM%I%NPATCH) :: ZCPL_PFLOOD    ! For the coupling with TRIP
REAL, DIMENSION(KI, IM%I%NPATCH) :: ZCPL_IFLOOD    ! For the coupling with TRIP
REAL, DIMENSION(KI, IM%I%NPATCH) :: ZCPL_ICEFLUX
!
! for chemical computations
!
REAL, DIMENSION(KI, IM%I%NPATCH) :: ZSW_FORBIO
!
REAL                       :: ZCONVERTFACM0_SLT, ZCONVERTFACM0_DST
REAL                       :: ZCONVERTFACM3_SLT, ZCONVERTFACM3_DST
REAL                       :: ZCONVERTFACM6_SLT, ZCONVERTFACM6_DST
!
! dimensions and loop counters
!
INTEGER :: ISWB   ! number of spectral shortwave bands
INTEGER :: JSWB   ! loop on number of spectral shortwave bands
INTEGER :: JPATCH ! loop on patches
INTEGER :: JSV, IDST, IMOMENT, II
INTEGER :: JLAYER, JMODE, JSV_IDX
!
! logical units
!
INTEGER :: JJ
LOGICAL :: LUPDATED              ! T if VEGETATION_UPDATE has reset fields
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! --------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('COUPLING_ISBA_N',0,ZHOOK_HANDLE)
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('COUPLING_ISBAN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
! --------------------------------------------------------------------------------------
!
!*      1.     Initializations
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Allocations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ZSFTH_TILE   (:,:)   = XUNDEF
ZSFTQ_TILE   (:,:)   = XUNDEF
ZSFCO2_TILE  (:,:)   = XUNDEF
ZSFU_TILE    (:,:)   = XUNDEF
ZSFV_TILE    (:,:)   = XUNDEF
ZTRAD_TILE   (:,:)   = XUNDEF
ZEMIS_TILE   (:,:)   = XUNDEF
ZDIR_ALB_TILE(:,:,:) = XUNDEF
ZSCA_ALB_TILE(:,:,:) = XUNDEF
ZTSURF_TILE  (:,:)   = XUNDEF
ZZ0_TILE     (:,:)   = XUNDEF
ZZ0H_TILE    (:,:)   = XUNDEF
ZQSURF_TILE  (:,:)   = XUNDEF
!
ZSFTS_TILE(:,:,:) = 0.
!
ZCPL_DRAIN(:,:)   = 0.0
ZCPL_RUNOFF(:,:)  = 0.0
ZCPL_EFLOOD(:,:)  = 0.0
ZCPL_PFLOOD(:,:)  = 0.0
ZCPL_IFLOOD(:,:)  = 0.0
ZCPL_ICEFLUX(:,:) = 0.0
!
ZSW_FORBIO(:,:)   =  XUNDEF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Forcing Modifications:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ZDIR=0.
!
DO JJ=1,SIZE(PQA) 
! specific humidity (conversion from kg/m3 to kg/kg)
!
  ZQA(JJ) = PQA(JJ) / PRHOA(JJ)
  ZPEQ_A_COEF(JJ) = PPEQ_A_COEF(JJ) / PRHOA(JJ)
  ZPEQ_B_COEF(JJ) = PPEQ_B_COEF(JJ) / PRHOA(JJ)
!
  ZCO2(JJ) = PCO2(JJ) / PRHOA(JJ)
!
! Other forcing variables depending on incoming forcing (argument list)JJ
!
  ZEXNS(JJ)   = (PPS(JJ)/XP00)**(XRD/XCPD)
  ZEXNA(JJ)   = (PPA(JJ)/XP00)**(XRD/XCPD)
!
!* wind strength
!
  ZWIND(JJ) = SQRT(PU(JJ)**2+PV(JJ)**2)
!
!* wind direction
!
  IF (ZWIND(JJ)>0.)  ZDIR(JJ)=ATAN2(PU(JJ),PV(JJ))
!
!* angle between z0eff J axis and wind direction (rad., clockwise)
!
  ZALFA(JJ) = ZDIR(JJ) - IM%I%XZ0EFFJPDIR(JJ) * XPI/180.

  IF (ZALFA(JJ)<-XPI) ZALFA(JJ) = ZALFA(JJ) + 2.*XPI
  IF (ZALFA(JJ)>=XPI) ZALFA(JJ) = ZALFA(JJ) - 2.*XPI
!
ENDDO
!
!* number of shortwave spectral bands
!
ISWB = KSW
!
!* irrigation
!
IF (LAGRIP .AND. (IM%I%CPHOTO=='LAI' .OR. IM%I%CPHOTO=='LST' .OR. &
                IM%I%CPHOTO=='NIT'.OR. IM%I%CPHOTO=='NCB') ) THEN
   CALL IRRIGATION_UPDATE(IM%AG, &
                          IM%I%XIRRIG,PTSTEP,KMONTH,KDAY,PTIME,               &
                            IM%I%TSEED(:,:)%TDATE%MONTH,IM%I%TSEED(:,:)%TDATE%DAY,   &
                            IM%I%TREAP(:,:)%TDATE%MONTH,IM%I%TREAP(:,:)%TDATE%DAY    )  
ENDIF
!
!* Actualization of the SGH variable (Fmu, Fsat)
!
 CALL ISBA_SGH_UPDATE(IM%IG, IM%I, &
                      IM%I%CISBA,IM%I%CRUNOFF,IM%I%CRAIN,PRAIN,IM%I%XMUF,IM%I%XFSAT,IM%I%XTOPQS)
!
!
!* Actualization of deep soil characteristics
!
IF (LDEEPSOIL) THEN
   CALL DEEPSOIL_UPDATE(IM%I, &
                        IM%I%TTIME%TDATE%MONTH)
ENDIF
!
!* Actualization of soil and wood carbon spinup
!
! During soil carbon spinup with ISBA-CC: 
!        (1) Atmospheric CO2 concentration fixed to Pre-industrial CO2 consentration XCO2_START
!        (2) Atmospheric CO2 concentration rampin up from XCO2_START to XCO2_END
!
IF(IM%I%LSPINUPCARBS.OR.IM%I%LSPINUPCARBW)THEN
!
  ISPINEND=IM%I%NNBYEARSPINS-NINT(IM%I%NNBYEARSPINS*XSPIN_CO2)
!  
  IM%I%LAGRI_TO_GRASS = .FALSE.
!
  IF ( IM%I%LSPINUPCARBS .AND. (IM%I%NNBYEARSOLD <= ISPINEND) ) THEN
!
   IM%I%LAGRI_TO_GRASS = .TRUE.
!
   ZCO2(:) = IM%I%XCO2_START * 1.E-6 * XMCO2 / XMD
!
  ELSEIF(IM%I%LSPINUPCARBS .AND. (IM%I%NNBYEARSOLD > ISPINEND) .AND. &
                  (IM%I%NNBYEARSOLD <= IM%I%NNBYEARSPINS) )THEN
!
   ZSPINCO2 = IM%I%XCO2_START + (IM%I%XCO2_END-IM%I%XCO2_START) * &
                REAL(IM%I%NNBYEARSOLD - ISPINEND) / REAL(IM%I%NNBYEARSPINS - ISPINEND)
!
   ZCO2 (:) = ZSPINCO2 * 1.E-6 * XMCO2 / XMD
!
  ENDIF
!
  CALL CARBON_SPINUP(IM%I%TTIME%TDATE%MONTH,IM%I%TTIME%TDATE%DAY,IM%I%TTIME%TIME,       &
                     IM%I%LSPINUPCARBS, IM%I%LSPINUPCARBW, IM%I%XSPINMAXS, IM%I%XSPINMAXW,   &
                     IM%I%NNBYEARSPINS, IM%I%NNBYEARSPINW, IM%I%NNBYEARSOLD, IM%I%CPHOTO,    &
                     IM%I%CRESPSL, IM%I%NSPINS, IM%I%NSPINW                             )
!
ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Time evolution
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IM%I%TTIME%TIME = IM%I%TTIME%TIME + PTSTEP
 CALL ADD_FORECAST_TO_DATE_SURF(IM%I%TTIME%TDATE%YEAR,IM%I%TTIME%TDATE%MONTH,&
                IM%I%TTIME%TDATE%DAY,IM%I%TTIME%TIME)
!
! --------------------------------------------------------------------------------------
!
!*      2.     Physical evolution
!
! --------------------------------------------------------------------------------------
! Patch Dependent Calculations
! --------------------------------------------------------------------------------------
!
PATCH_LOOP: DO JPATCH=1,IM%I%NPATCH
!
  IF (IM%I%NSIZE_NATURE_P(JPATCH) == 0 ) CYCLE
!
! Pack dummy arguments for each patch:
!
#ifdef TOPD
  IF (LCOUPL_TOPD)NMASKT_PATCH(:)=IM%I%NR_NATURE_P(:,JPATCH)
#endif
  CALL TREAT_PATCH(IM%I%NSIZE_NATURE_P(JPATCH),IM%I%NR_NATURE_P(:,JPATCH))
!
ENDDO PATCH_LOOP
!
! --------------------------------------------------------------------------------------
! SFX - RRM coupling update if used :
! --------------------------------------------------------------------------------------
!
IF(IM%I%LCPL_RRM)THEN
  CALL DIAG_CPL_ESM_ISBA(IM%I, &
                         PTSTEP,ZCPL_DRAIN,ZCPL_RUNOFF,ZCPL_EFLOOD, &
                           ZCPL_PFLOOD,ZCPL_IFLOOD,ZCPL_ICEFLUX     )  
ENDIF
!
! --------------------------------------------------------------------------------------
! Vegetation update (in case of non-interactive vegetation):
! Or
! Vegetation albedo only update (in case of interactive vegetation):
! --------------------------------------------------------------------------------------
!
LUPDATED=.FALSE.
IF ((IM%I%CPHOTO=='NON' .OR. IM%I%CPHOTO=='AGS' .OR. IM%I%CPHOTO=='AST') .AND. IM%I%LVEGUPD) THEN
     CALL VEGETATION_UPDATE(DTCO, IM%DTI, DTGD, DTGR, IM%IG, IM%I, TGRO, &
                            PTSTEP,IM%I%TTIME,IM%I%XCOVER, IM%I%LCOVER,                 &
                         IM%I%CISBA,IM%I%LECOCLIMAP,IM%I%CPHOTO,LAGRIP,IM%I%LTR_ML,'NAT',    &
                         IM%I%XLAI,IM%I%XVEG,IM%I%XZ0,                                  &
                         IM%I%XALBNIR,IM%I%XALBVIS,IM%I%XALBUV,IM%I%XEMIS,                   &
                         IM%I%XRSMIN,IM%I%XGAMMA,IM%I%XWRMAX_CF,                        &
                         IM%I%XRGL,IM%I%XCV,                                       &
                         IM%I%XGMES,IM%I%XBSLAI,IM%I%XLAIMIN,IM%I%XSEFOLD,IM%I%XGC,IM%I%XDMAX,    &
                         IM%I%XF2I, IM%I%LSTRESS,                                  &
                         IM%I%XAOSIP,IM%I%XAOSIM,IM%I%XAOSJP,IM%I%XAOSJM,                    &
                         IM%I%XHO2IP,IM%I%XHO2IM,IM%I%XHO2JP,IM%I%XHO2JM,                    &
                         IM%I%XZ0EFFIP,IM%I%XZ0EFFIM,IM%I%XZ0EFFJP,IM%I%XZ0EFFJM,            &
                         IM%I%CALBEDO, IM%I%XALBNIR_VEG, IM%I%XALBVIS_VEG, IM%I%XALBUV_VEG,  &
                         IM%I%XALBNIR_SOIL, IM%I%XALBVIS_SOIL, IM%I%XALBUV_SOIL,        &
                         IM%I%XCE_NITRO, IM%I%XCF_NITRO, IM%I%XCNA_NITRO,               &
                         IM%I%TSEED, IM%I%TREAP, IM%I%XWATSUP, IM%I%XIRRIG,                  &
                         IM%I%XGNDLITTER, IM%I%XRGLGV,IM%I%XGAMMAGV,                     &
                         IM%I%XRSMINGV, IM%I%XWRMAX_CFGV,                          &
                         IM%I%XH_VEG, IM%I%XLAIGV, IM%I%XZ0LITTER, LUPDATED             )  
!
ELSEIF ((IM%I%CPHOTO=='LAI'.OR.IM%I%CPHOTO=='LST'.OR.IM%I%CPHOTO=='NIT'.OR.IM%I%CPHOTO=='NCB').AND.IM%I%LVEGUPD) THEN
!
  CALL ALBEDO_VEG_UPDATE(DTCO, IM%DTI, IM%IG, IM%I, &
                         PTSTEP,IM%I%TTIME,IM%I%XCOVER, IM%I%LCOVER,                    &
                         IM%I%CISBA,IM%I%LECOCLIMAP,IM%I%CPHOTO,LAGRIP,IM%I%LTR_ML,'NAT',    &
                         IM%I%XVEG,IM%I%XALBNIR,IM%I%XALBVIS,IM%I%XALBUV,                    &
                         IM%I%CALBEDO, IM%I%XALBNIR_VEG, IM%I%XALBVIS_VEG, IM%I%XALBUV_VEG,  &
                         IM%I%XALBNIR_SOIL, IM%I%XALBVIS_SOIL, IM%I%XALBUV_SOIL         )
END IF
!
IF(IM%I%LPERTSURF.AND.LUPDATED) THEN
  ! random perturbation for ensembles:
  ! reset these fields to their original values, as in compute_isba_parameters
  IM%I%XVEG(:,1) = IM%I%XPERTVEG(:)
  IM%I%XLAI(:,1) = IM%I%XPERTLAI(:)
  IM%I%XCV(:,1)  = IM%I%XPERTCV(:)
  ! reapply original perturbation patterns
  WHERE(IM%I%XALBNIR(:,1)/=XUNDEF)  IM%I%XALBNIR(:,1) =IM%I%XALBNIR(:,1) *( 1.+ IM%I%XPERTALB(:) )
  WHERE(IM%I%XALBVIS(:,1)/=XUNDEF)  IM%I%XALBVIS(:,1) =IM%I%XALBVIS(:,1) *( 1.+ IM%I%XPERTALB(:) )
  WHERE(IM%I%XALBUV(:,1)/=XUNDEF)   IM%I%XALBUV(:,1)  =IM%I%XALBUV(:,1)  *( 1.+ IM%I%XPERTALB(:) )
  WHERE(IM%I%XZ0(:,1)/=XUNDEF)      IM%I%XZ0(:,1)     =IM%I%XZ0(:,1)     *( 1.+ IM%I%XPERTZ0(:) )
  WHERE(IM%I%XZ0EFFIP(:,1)/=XUNDEF) IM%I%XZ0EFFIP(:,1)=IM%I%XZ0EFFIP(:,1)*( 1.+ IM%I%XPERTZ0(:) )
  WHERE(IM%I%XZ0EFFIM(:,1)/=XUNDEF) IM%I%XZ0EFFIM(:,1)=IM%I%XZ0EFFIM(:,1)*( 1.+ IM%I%XPERTZ0(:) )
  WHERE(IM%I%XZ0EFFJP(:,1)/=XUNDEF) IM%I%XZ0EFFJP(:,1)=IM%I%XZ0EFFJP(:,1)*( 1.+ IM%I%XPERTZ0(:) )
  WHERE(IM%I%XZ0EFFJM(:,1)/=XUNDEF) IM%I%XZ0EFFJM(:,1)=IM%I%XZ0EFFJM(:,1)*( 1.+ IM%I%XPERTZ0(:) )

ENDIF
!
! --------------------------------------------------------------------------------------
! Outputs for the atmospheric model or update the snow/flood fraction at time t+1
! --------------------------------------------------------------------------------------
! Grid box average fluxes/properties: Arguments and standard diagnostics at time t+1
!
 CALL AVERAGE_FLUX(IM%I%XPATCH,                                             &
                  ZSFTH_TILE, ZSFTQ_TILE, ZSFTS_TILE, ZSFCO2_TILE,    &
                  ZSFU_TILE, ZSFV_TILE,                               &                   
                  PSFTH, PSFTQ, PSFTS, PSFCO2,                        &
                  PSFU, PSFV                                          )  
!
!
!-------------------------------------------------------------------------------
!Physical properties see by the atmosphere in order to close the energy budget 
!between surfex and the atmosphere. All variables should be at t+1 but very 
!difficult to do. Maybe it will be done later. However, Ts is at time t+1
!-------------------------------------------------------------------------------
!   
 CALL AVERAGE_PHY(IM%I%XPATCH,                                         &
                  ZTSURF_TILE, ZZ0_TILE, ZZ0H_TILE, ZQSURF_TILE,  &    
                  PUREF, PZREF, PTSURF, PZ0, PZ0H, PQSURF         )
!
!-------------------------------------------------------------------------------------
!Radiative properties at time t+1 (see by the atmosphere) in order to close
!the energy budget between surfex and the atmosphere
!-------------------------------------------------------------------------------------
!
 CALL UPDATE_RAD_ISBA_n(IM%I, &
                        IM%I%LFLOOD, IM%I%TSNOW%SCHEME, PZENITH2, PSW_BANDS,      &
                       IM%I%XVEG, IM%I%XLAI, IM%I%XZ0,                                 &
                       IM%I%LMEB_PATCH,IM%I%XLAIGV,IM%I%XGNDLITTER,IM%I%XZ0LITTER,IM%I%XH_VEG,   &
                       IM%I%XALBNIR, IM%I%XALBVIS, IM%I%XALBUV, IM%I%XEMIS,                 &
                       ZDIR_ALB_TILE,ZSCA_ALB_TILE,ZEMIS_TILE,          &
                       PDIR_SW, PSCA_SW,                                &
                       IM%I%XALBNIR_VEG, IM%I%XALBNIR_SOIL,                       &
                       IM%I%XALBVIS_VEG, IM%I%XALBVIS_SOIL                        )
!
 CALL AVERAGE_RAD(IM%I%XPATCH,                                              &
                 ZDIR_ALB_TILE, ZSCA_ALB_TILE, ZEMIS_TILE, ZTRAD_TILE, &
                 PDIR_ALB,      PSCA_ALB,      IM%I%XEMIS_NAT,  IM%I%XTSRAD_NAT  )  
!
PEMIS = IM%I%XEMIS_NAT
PTRAD = IM%I%XTSRAD_NAT
!
!-------------------------------------------------------------------------------------
!
! Any additional diagnostics (stored in MODD_DIAG_ISBA_n)
!
 CALL AVERAGE_DIAG_ISBA_n(IM%DGEI, IM%DGI, IM%I, &
                          PUREF,PZREF,PSFCO2,PTRAD)
!
! Cumulated diagnostics (stored in MODD_DIAG_EVAP_ISBA_n)
!
 CALL AVERAGE_DIAG_EVAP_ISBA_n(IM%DGEI, IM%I, &
                               PTSTEP,PRAIN,PSNOW)
!
! Miscellaneous diagnostics (stored in MODD_DIAG_MISC_ISBA_n)
!
 CALL AVERAGE_DIAG_MISC_ISBA_n(IM%DGMI, IM%I)
!
!--------------------------------------------------------------------------------------
!
 CALL COUPLING_SURF_TOPD(IM%DGEI, IM%DGMI, IM%IG, IM%I, UG, U, &
                         HPROGRAM,U%NDIM_FULL)
!
! --------------------------------------------------------------------------------------
! Snow/Flood fractions, albedo and emissivity update :
! --------------------------------------------------------------------------------------
!
! --------------------------------------------------------------------------------------
! Chemical fluxes :
! --------------------------------------------------------------------------------------
!
IF (IM%CHI%SVI%NBEQ>0 .AND. IM%CHI%LCH_BIO_FLUX) THEN
 CALL CH_BVOCEM_n(IM%CHI, IM%GB, IM%I, &
                  ZSW_FORBIO,PRHOA,PSFTS)
ENDIF
!
!SOILNOX
IF (IM%CHI%LCH_NO_FLUX) THEN
  CALL SOILEMISNO_n(IM%GB, IM%I, &
                    PU,PV)
ENDIF
!
!==========================================================================================
!
IF (LHOOK) CALL DR_HOOK('COUPLING_ISBA_N',1,ZHOOK_HANDLE)
 CONTAINS
!
!=======================================================================================
SUBROUTINE TREAT_PATCH(KSIZE,KMASK)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)               :: KSIZE
INTEGER, INTENT(IN), DIMENSION(KI) :: KMASK
!
REAL, DIMENSION(KSIZE) :: ZP_ZREF    ! height of T,q forcing                 (m)
REAL, DIMENSION(KSIZE) :: ZP_UREF    ! height of wind forcing                (m)
REAL, DIMENSION(KSIZE) :: ZP_U       ! zonal wind                            (m/s)
REAL, DIMENSION(KSIZE) :: ZP_V       ! meridian wind                         (m/s)
REAL, DIMENSION(KSIZE) :: ZP_WIND    ! wind                                  (m/s)
REAL, DIMENSION(KSIZE) :: ZP_DIR     ! wind direction                        (rad from N clockwise)
REAL, DIMENSION(KSIZE) :: ZP_QA      ! air specific humidity forcing         (kg/kg)
REAL, DIMENSION(KSIZE) :: ZP_TA      ! air temperature forcing               (K)
REAL, DIMENSION(KSIZE) :: ZP_CO2     ! CO2 concentration in the air          (kg/kg)
REAL, DIMENSION(KSIZE,KSV) :: ZP_SV      ! scalar concentration in the air       (kg/kg)
REAL, DIMENSION(KSIZE) :: ZP_ZENITH  ! zenithal angle        radian from the vertical)
REAL, DIMENSION(KSIZE) :: ZP_PEW_A_COEF ! implicit coefficients
REAL, DIMENSION(KSIZE) :: ZP_PEW_B_COEF ! needed if HCOUPLING='I'
REAL, DIMENSION(KSIZE) :: ZP_PET_A_COEF
REAL, DIMENSION(KSIZE) :: ZP_PET_B_COEF
REAL, DIMENSION(KSIZE) :: ZP_PEQ_A_COEF
REAL, DIMENSION(KSIZE) :: ZP_PEQ_B_COEF
REAL, DIMENSION(KSIZE) :: ZP_RAIN    ! liquid precipitation                  (kg/m2/s)
REAL, DIMENSION(KSIZE) :: ZP_SNOW    ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KSIZE) :: ZP_LW      ! longwave radiation (W/m2)
REAL, DIMENSION(KSIZE,ISWB) :: ZP_DIR_SW  ! direct  solar radiation (W/m2)
REAL, DIMENSION(KSIZE,ISWB) :: ZP_SCA_SW  ! diffuse solar radiation (W/m2)
REAL, DIMENSION(KSIZE) :: ZP_PS      ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(KSIZE) :: ZP_PA      ! pressure at forcing level             (Pa)
REAL, DIMENSION(KSIZE) :: ZP_ZS      ! atmospheric model orography           (m)
REAL, DIMENSION(KSIZE) :: ZP_SFTQ    ! flux of water vapor <w'q'>            (kg.m-2.s-1)
REAL, DIMENSION(KSIZE) :: ZP_SFTH    ! flux of temperature <w'T'>            (W/m2)
REAL, DIMENSION(KSIZE,KSV) :: ZP_SFTS    ! flux of scalar      <w'sv'>           (mkg/kg/s)
REAL, DIMENSION(KSIZE) :: ZP_SFCO2   ! flux of CO2 positive toward the atmosphere (m/s*kg_CO2/kg_air)
REAL, DIMENSION(KSIZE) :: ZP_USTAR   ! friction velocity                     (m/s)
REAL, DIMENSION(KSIZE) :: ZP_SFU     ! zonal momentum flux                   (pa)
REAL, DIMENSION(KSIZE) :: ZP_SFV     ! meridian momentum flux                (pa)
REAL, DIMENSION(KSIZE) :: ZP_TRAD    ! radiative temperature                 (K)
REAL, DIMENSION(KSIZE) :: ZP_TSURF   ! surface effective temperature (K)
REAL, DIMENSION(KSIZE) :: ZP_Z0      ! roughness length for momentum (m)
REAL, DIMENSION(KSIZE) :: ZP_Z0H     ! roughness length for heat     (m)
REAL, DIMENSION(KSIZE) :: ZP_QSURF   ! specific humidity at surface  (kg/kg)
!
!*  other forcing variables (packed for each patch)
!
REAL, DIMENSION(KSIZE) :: ZP_RHOA    ! lowest atmospheric level air density          (kg/m3)
REAL, DIMENSION(KSIZE) :: ZP_EXNA    ! Exner function at lowest atmospheric level    (-)
REAL, DIMENSION(KSIZE) :: ZP_EXNS    ! Exner function at surface                     (-)
REAL, DIMENSION(KSIZE) :: ZP_ALFA    ! Wind direction   (-)
!
!*  working variables (packed for each patch)
!
REAL, DIMENSION(KSIZE)      :: ZP_ALBNIR_TVEG         ! total vegetation albedo in ir
REAL, DIMENSION(KSIZE)      :: ZP_ALBNIR_TSOIL        ! total soil albedo in ir
REAL, DIMENSION(KSIZE)      :: ZP_ALBVIS_TVEG         ! total vegetation albedo in vis
REAL, DIMENSION(KSIZE)      :: ZP_ALBVIS_TSOIL        ! total soil albedo in vis
REAL, DIMENSION(KSIZE) :: ZP_EMIS                      ! emissivity
REAL, DIMENSION(KSIZE) :: ZP_GLOBAL_SW                 ! global incoming SW rad.
REAL, DIMENSION(KSIZE) :: ZP_SLOPE_COS                 ! typical slope in the grid cosine
!
REAL, DIMENSION(KSIZE) :: ZP_Z0FLOOD  !Floodplain 
REAL, DIMENSION(KSIZE) :: ZP_FFGNOS   !Floodplain fraction over the ground without snow
REAL, DIMENSION(KSIZE) :: ZP_FFVNOS   !Floodplain fraction over vegetation without snow
!
REAL, DIMENSION(KSIZE,IM%I%NNBIOMASS) :: ZP_RESP_BIOMASS_INST         ! instantaneous biomass respiration (kgCO2/kgair m/s)
!
!*  Aggregated coeffs for evaporative flux calculations
!
REAL, DIMENSION(KSIZE) :: ZP_AC_AGG      ! aggregated aerodynamic resistance
REAL, DIMENSION(KSIZE) :: ZP_HU_AGG      ! aggregated relative humidity
!
!*  For multi-energy balance
!
REAL, DIMENSION(KSIZE) :: ZPALPHAN                     ! snow/canopy transition coefficient
REAL, DIMENSION(KSIZE) :: ZSNOWDEPTH                   ! total snow depth
REAL, DIMENSION(KSIZE) :: ZZ0G_WITHOUT_SNOW            ! roughness length for momentum at snow-free canopy floor
REAL, DIMENSION(KSIZE) :: ZZ0_MEBV                     ! roughness length for momentum over MEB vegetation part of patch
REAL, DIMENSION(KSIZE) :: ZZ0H_MEBV                    ! roughness length for heat over MEB vegetation part of path
REAL, DIMENSION(KSIZE) :: ZZ0EFF_MEBV                  ! roughness length for momentum over MEB vegetation part of patch
REAL, DIMENSION(KSIZE) :: ZZ0_MEBN                     ! roughness length for momentum over MEB snow part of patch
REAL, DIMENSION(KSIZE) :: ZZ0H_MEBN                    ! roughness length for heat over MEB snow part of path
REAL, DIMENSION(KSIZE) :: ZZ0EFF_MEBN                  ! roughness length for momentum over MEB snow part of patch
! Temporary
REAL, DIMENSION(KSIZE) :: ZP_MEB_SCA_SW                ! diffuse incoming SW rad.
!
!*  ISBA water and energy budget
!
REAL, DIMENSION(KSIZE) :: ZP_WG_INI
REAL, DIMENSION(KSIZE) :: ZP_WGI_INI
REAL, DIMENSION(KSIZE) :: ZP_WR_INI
REAL, DIMENSION(KSIZE) :: ZP_SWE_INI
!
! miscellaneous
!
REAL, DIMENSION(KSIZE)               :: ZP_DEEP_FLUX ! Flux at the bottom of the soil
REAL, DIMENSION(KSIZE)               :: ZP_TDEEP_A   ! coefficient for implicitation of Tdeep
REAL, DIMENSION(KSIZE)               :: ZIRRIG_GR    ! green roof ground irrigation rate 
!
! For multi-energy balance
LOGICAL :: GMEB  ! True if multi-energy balance should be used for the specific patch
!
INTEGER :: JJ, JI, JK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('COUPLING_ISBA_n:TREAT_PATCH',0,ZHOOK_HANDLE)
!
!--------------------------------------------------------------------------------------
!
! Pack isba forcing outputs
!
IF (IM%I%NPATCH==1) THEN
   ZP_ZENITH(:)     = PZENITH     (:)
   ZP_ZREF(:)       = PZREF       (:)
   ZP_UREF(:)       = PUREF       (:)
   ZP_WIND(:)       = ZWIND       (:)
   ZP_U(:)          = PU          (:)
   ZP_V(:)          = PV          (:)
   ZP_DIR(:)        = ZDIR        (:)
   ZP_QA(:)         = ZQA         (:)
   ZP_TA(:)         = PTA         (:)
   ZP_CO2(:)        = ZCO2        (:)
   ZP_SV(:,:)       = PSV         (:,:)
   ZP_PEW_A_COEF(:) = PPEW_A_COEF (:)
   ZP_PEW_B_COEF(:) = PPEW_B_COEF (:)
   ZP_PET_A_COEF(:) = PPET_A_COEF (:)
   ZP_PET_B_COEF(:) = PPET_B_COEF (:)
   ZP_PEQ_A_COEF(:) = ZPEQ_A_COEF (:)
   ZP_PEQ_B_COEF(:) = ZPEQ_B_COEF (:)
   ZP_RAIN(:)       = PRAIN       (:)
   ZP_SNOW(:)       = PSNOW       (:)
   ZP_LW(:)         = PLW         (:)
   ZP_DIR_SW(:,:)   = PDIR_SW     (:,:)
   ZP_SCA_SW(:,:)   = PSCA_SW     (:,:)
   ZP_PS(:)         = PPS         (:)
   ZP_PA(:)         = PPA         (:)
   ZP_ZS(:)         = PZS         (:)
!
   ZP_RHOA(:)       = PRHOA       (:)
   ZP_EXNA(:)       = ZEXNA       (:)
   ZP_EXNS(:)       = ZEXNS       (:)
   ZP_ALFA(:)       = ZALFA       (:)
ELSE
!cdir nodep
!cdir unroll=8
  DO JJ=1,KSIZE
   JI = KMASK(JJ)
   ZP_ZENITH(JJ)     = PZENITH     (JI)
   ZP_ZREF(JJ)       = PZREF       (JI)
   ZP_UREF(JJ)       = PUREF       (JI)
   ZP_WIND(JJ)       = ZWIND       (JI)
   ZP_U(JJ)          = PU          (JI)
   ZP_V(JJ)          = PV          (JI)
   ZP_DIR(JJ)        = ZDIR        (JI)
   ZP_QA(JJ)         = ZQA         (JI)
   ZP_TA(JJ)         = PTA         (JI)
   ZP_CO2(JJ)        = ZCO2        (JI)
   ZP_PEW_A_COEF(JJ) = PPEW_A_COEF (JI)
   ZP_PEW_B_COEF(JJ) = PPEW_B_COEF (JI)
   ZP_PET_A_COEF(JJ) = PPET_A_COEF (JI)
   ZP_PET_B_COEF(JJ) = PPET_B_COEF (JI)
   ZP_PEQ_A_COEF(JJ) = ZPEQ_A_COEF (JI)
   ZP_PEQ_B_COEF(JJ) = ZPEQ_B_COEF (JI)
   ZP_RAIN(JJ)       = PRAIN       (JI)
   ZP_SNOW(JJ)       = PSNOW       (JI)
   ZP_LW(JJ)         = PLW         (JI)
   ZP_PS(JJ)         = PPS         (JI)
   ZP_PA(JJ)         = PPA         (JI)
   ZP_ZS(JJ)         = PZS         (JI)
!
   ZP_RHOA(JJ)       = PRHOA       (JI)
   ZP_EXNA(JJ)       = ZEXNA       (JI)
   ZP_EXNS(JJ)       = ZEXNS       (JI)
   ZP_ALFA(JJ)       = ZALFA       (JI)
  ENDDO
!
  DO JK=1,KSV
!cdir nodep
!cdir unroll=8
    DO JJ=1,KSIZE
      JI=KMASK(JJ)
      ZP_SV(JJ,JK) = PSV(JI,JK)
    ENDDO
  ENDDO
!
  DO JK=1,SIZE(PDIR_SW,2)
!cdir nodep
!cdir unroll=8
    DO JJ=1,KSIZE
      JI=KMASK(JJ)
      ZP_DIR_SW(JJ,JK) = PDIR_SW (JI,JK)
      ZP_SCA_SW(JJ,JK) = PSCA_SW (JI,JK)
    ENDDO
  ENDDO
!
ENDIF
!
!--------------------------------------------------------------------------------------
!
! For multi-energy balance
   GMEB=IM%I%LMEB_PATCH(JPATCH)
!
! Pack ISBA input and prognostic variables (modd_isban) for each patch:
!
 CALL PACK_ISBA_PATCH_GET_SIZE_n(IM%I, IM%PKI, &
                                 JPATCH)
!
 CALL PACK_DIAG_PATCH_GET_SIZE_n(IM%DGEI, IM%DGI, IM%DGMI, IM%I, IM%PKDI, &
                                 JPATCH)
!
 CALL PACK_ISBA_PATCH_n(IM%AG, IM%IG, IM%I, IM%PKI, &
                        KMASK,KSIZE,JPATCH)     
!
! Pack chemistry input and prognostic variables (modd_ch_isban) for each patch:
!
IF (IM%CHI%SVI%NBEQ>0) THEN
  IF( IM%CHI%CCH_DRY_DEP == "WES89") THEN
    CALL PACK_CH_ISBA_PATCH_n(IM%CHI, IM%PKCI, &
                              KMASK,KSIZE,IM%I%NPATCH,JPATCH)     
  END IF
END IF
!
! Allocate ISBA diagnostics for each patch:
!
 CALL PACK_DIAG_PATCH_n(IM%DGEI, IM%DGI, IM%DGMI, IM%I, IM%PKDI, &
                        KSIZE,ISWB,JPATCH)     
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Cosine of the slope typically encoutered in the grid mesh (including subgrid orography)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ZP_SLOPE_COS(:) = 1./SQRT(1.+IM%PKI%XP_SSO_SLOPE(:)**2)
IF(LNOSOF)ZP_SLOPE_COS(:) = 1.0
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Snow fractions
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! now caculated at the initialization and at the end of the time step 
! (see update_frac_alb_emis_isban.f90) in order to close the energy budget
! between surfex and the atmosphere. This fact do not change the offline runs.
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! No implicitation of Tdeep
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ZP_TDEEP_A = 0.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Flood properties 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF(IM%I%LFLOOD)THEN
  CALL ISBA_FLOOD_PROPERTIES(IM%PKI%XP_LAI,IM%PKI%XP_FFLOOD,IM%PKI%XP_FFROZEN,  &
                             ZP_Z0FLOOD,ZP_FFGNOS,ZP_FFVNOS)  
ELSE
  ZP_Z0FLOOD = XUNDEF
  ZP_FFGNOS  = 0.0
  ZP_FFVNOS  = 0.0
ENDIF
!
! For multi-energy balance
   IF(GMEB)THEN
     ZSNOWDEPTH(:) = SUM(IM%PKI%XP_SNOWSWE(:,:)/IM%PKI%XP_SNOWRHO(:,:),2)
     ZPALPHAN(:)=MEBPALPHAN(ZSNOWDEPTH,IM%PKI%XP_H_VEG)
   ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Surface Roughness lengths (m):
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!* effective roughness
!
 CALL Z0EFF(IM%I, &
            IM%I%CROUGH, GMEB, ZP_ALFA, ZP_ZREF, ZP_UREF, &
            IM%PKI%XP_Z0, IM%PKI%XP_Z0REL, IM%PKI%XP_PSN,   &
            ZPALPHAN,IM%PKI%XP_Z0LITTER, IM%PKI%XP_SNOWSWE(:,1),           &
     IM%PKI%XP_Z0EFFIP,IM%PKI%XP_Z0EFFIM,IM%PKI%XP_Z0EFFJP, &
     IM%PKI%XP_Z0EFFJM, IM%PKI%XP_FF, ZP_Z0FLOOD,     &
     IM%PKI%XP_AOSIP,IM%PKI%XP_AOSIM,IM%PKI%XP_AOSJP,IM%PKI%XP_AOSJM,         &
     IM%PKI%XP_HO2IP,IM%PKI%XP_HO2IM,IM%PKI%XP_HO2JP,IM%PKI%XP_HO2JM,        &
     IM%PKI%XP_Z0_O_Z0H, IM%PKDI%XP_Z0_WITH_SNOW, IM%PKDI%XP_Z0H_WITH_SNOW, &
     IM%PKDI%XP_Z0EFF, ZZ0G_WITHOUT_SNOW,                                 &
     ZZ0_MEBV,ZZ0H_MEBV,ZZ0EFF_MEBV,                                     &
     ZZ0_MEBN,ZZ0H_MEBN,ZZ0EFF_MEBN                                      )
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Shortwave computations for outputs (albedo for radiative scheme)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! now caculated at the initialization and at the end of the time step 
! (see update_frac_alb_emis_isban.f90) in order to close the energy budget
! between surfex and the atmosphere. This fact do not change the offline runs.
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Shortwave computations for ISBA inputs (global snow-free albedo)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
! ISBA needs global incoming solar radiation: it currently does
! not distinguish between the scattered and direct components,
! or between different wavelengths.
!
!
!* Snow-free surface albedo for each wavelength
!
 CALL ISBA_ALBEDO(IM%I%TSNOW%SCHEME, IM%I%LTR_ML, GMEB,                &
                   ZP_DIR_SW, ZP_SCA_SW, PSW_BANDS,ISWB,                 &
                   IM%PKI%XP_ALBNIR, IM%PKI%XP_ALBVIS, IM%PKI%XP_ALBUV,                       &
                   IM%PKI%XP_ALBNIR_VEG, IM%PKI%XP_ALBVIS_VEG, IM%PKI%XP_ALBUV_VEG,           &
                   IM%PKI%XP_ALBNIR_SOIL, IM%PKI%XP_ALBVIS_SOIL, IM%PKI%XP_ALBUV_SOIL,        &
                   IM%PKI%XP_ALBF, IM%PKI%XP_FFV, IM%PKI%XP_FFG,    & 
                   ZP_GLOBAL_SW, IM%PKDI%XP_SNOWFREE_ALB, IM%PKDI%XP_SNOWFREE_ALB_VEG,   &
                   IM%PKDI%XP_SNOWFREE_ALB_SOIL, ZP_MEB_SCA_SW,                  &
                   ZP_ALBNIR_TVEG, ZP_ALBVIS_TVEG,                       &
                   ZP_ALBNIR_TSOIL, ZP_ALBVIS_TSOIL                      )  
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Intialize computation of ISBA water and energy budget
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL ISBA_BUDGET_INIT(IM%DGEI, &
                       IM%I%CISBA,IM%I%TSNOW%SCHEME,            &
                      IM%PKI%XP_WG,IM%PKI%XP_WGI,IM%PKI%XP_WR,IM%PKI%XP_SNOWSWE, &
                      IM%PKI%XP_DG, IM%PKI%XP_DZG, ZP_WG_INI,      &
                      ZP_WGI_INI, ZP_WR_INI,         &
                      ZP_SWE_INI                     )
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Over Natural Land Surfaces:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ZIRRIG_GR(:)= 0.
!
 CALL ISBA(IM%I%CISBA, IM%I%CPHOTO, IM%I%LTR_ML, IM%I%CRUNOFF, IM%I%CKSAT, IM%I%CRAIN, &
           IM%I%CHORT, IM%I%CC1DRY, IM%I%CSCOND, IM%I%TSNOW%SCHEME, IM%I%CSNOWRES, &
           IM%I%CCPSURF, IM%I%CSOILFRZ, IM%I%CDIFSFCOND, IM%I%TTIME, IM%I%LFLOOD, &
           IM%I%LTEMP_ARP, IM%I%LGLACIER, GMEB, IM%I%LFORC_MEASURE, IM%I%LMEB_LITTER, &
           IM%I%LMEB_GNDRES, PTSTEP, &
           CIMPLICIT_WIND, IM%I%LAGRI_TO_GRASS, IM%I%LSNOWDRIFT, IM%I%LSNOWDRIFT_SUBLIM, &
           IM%I%LSNOW_ABS_ZENITH, IM%I%CSNOWMETAMO, IM%I%CSNOWRAD, IM%I%XCGMAX, ZP_ZREF, &
           ZP_UREF, ZP_SLOPE_COS, ZP_TA, ZP_QA, ZP_EXNA, ZP_RHOA, ZP_PS, ZP_EXNS, ZP_RAIN, &
           ZP_SNOW, ZP_ZENITH, ZP_MEB_SCA_SW, ZP_GLOBAL_SW, ZP_LW, ZP_WIND, ZP_PEW_A_COEF, &
           ZP_PEW_B_COEF, ZP_PET_A_COEF, ZP_PEQ_A_COEF,  ZP_PET_B_COEF, ZP_PEQ_B_COEF, &
           IM%PKI%XP_RSMIN, IM%PKI%XP_RGL, IM%PKI%XP_GAMMA, IM%PKI%XP_CV, IM%PKI%XP_RUNOFFD, &
           IM%PKI%XP_SOILWGHT, IM%I%NLAYER_HORT, IM%I%NLAYER_DUN, ZP_ALBNIR_TVEG, ZP_ALBVIS_TVEG,  &
           ZP_ALBNIR_TSOIL, ZP_ALBVIS_TSOIL, IM%PKDI%XP_SNOWFREE_ALB, IM%PKI%XP_WRMAX_CF, &
           IM%PKI%XP_VEG, IM%PKI%XP_LAI, IM%PKI%XP_EMIS, IM%PKDI%XP_Z0_WITH_SNOW, &
           IM%PKDI%XP_Z0H_WITH_SNOW, IM%PKI%XP_VEGTYPE_PATCH, IM%PKDI%XP_Z0EFF,   &
           IM%PKI%XP_RGLV, IM%PKI%XP_GAMMAV, IM%PKI%XP_RSMINV, &
           IM%PKI%XP_ROOTFRACV, IM%PKI%XP_WRMAX_CFV, IM%PKI%XP_LAIV, IM%PKI%XP_BSLAI, &
           IM%PKI%XP_LAIMIN,IM%PKI%XP_H_VEG,ZPALPHAN, ZZ0G_WITHOUT_SNOW, ZZ0_MEBV,     &
           ZZ0H_MEBV,ZZ0EFF_MEBV, ZZ0_MEBN,ZZ0H_MEBN,ZZ0EFF_MEBN, IM%PKI%XP_GNDLITTER,  &
           IM%PKI%XP_RUNOFFB, IM%PKI%XP_CGSAT, IM%PKI%XP_C1SAT, IM%PKI%XP_C2REF, &
           IM%PKI%XP_C3, IM%PKI%XP_C4B, IM%PKI%XP_C4REF, IM%PKI%XP_ACOEF, IM%PKI%XP_PCOEF, &
           IM%PKI%XP_TAUICE, IM%PKI%XP_WDRAIN, ZP_TDEEP_A, IM%PKI%XP_TDEEP, IM%PKI%XP_GAMMAT,  &
           IM%PKI%XP_PSN, IM%PKI%XP_PSNG, IM%PKI%XP_PSNV, IM%PKI%XP_PSNV_A, &
           IM%PKDI%XP_SNOWFREE_ALB_VEG, IM%PKDI%XP_SNOWFREE_ALB_SOIL, IM%PKI%XP_IRRIG, &
           IM%PKI%XP_WATSUP, IM%PKI%XP_THRESHOLD, IM%PKI%XP_LIRRIGATE, IM%PKI%XP_LIRRIDAY, &
           IM%PKI%LP_STRESS, IM%PKI%XP_GC, IM%PKI%XP_F2I, IM%PKI%XP_DMAX, IM%PKI%XP_AH, &
           IM%PKI%XP_BH, ZP_CO2, IM%PKI%XP_GMES, IM%I%XPOI, IM%PKI%XP_FZERO, IM%PKI%XP_EPSO, &
           IM%PKI%XP_GAMM, IM%PKI%XP_QDGAMM, IM%PKI%XP_QDGMES, IM%PKI%XP_T1GMES, IM%PKI%XP_T2GMES, &
           IM%PKI%XP_AMAX, IM%PKI%XP_QDAMAX,  IM%PKI%XP_T1AMAX, IM%PKI%XP_T2AMAX, IM%I%XABC, &
           IM%PKI%XP_DG, IM%PKI%XP_DZG, IM%PKI%XP_DZDIF, IM%PKI%NK_WG_LAYER, IM%PKI%XP_ROOTFRAC, &
           IM%PKI%XP_WFC, IM%PKI%XP_WWILT, IM%PKI%XP_WSAT, IM%PKI%XP_BCOEF, IM%PKI%XP_CONDSAT, &
           IM%PKI%XP_MPOTSAT, IM%PKI%XP_HCAPSOIL, IM%PKI%XP_CONDDRY, IM%PKI%XP_CONDSLD, IM%PKI%XP_D_ICE, &
           IM%PKI%XP_KSAT_ICE, IM%PKI%XP_MUF, IM%PKI%XP_FF, IM%PKI%XP_FFG, IM%PKI%XP_FFV, ZP_FFGNOS,  &
           ZP_FFVNOS, IM%PKI%XP_FFROZEN, IM%PKI%XP_ALBF, IM%PKI%XP_EMISF, IM%PKI%XP_FFLOOD, IM%PKI%XP_PIFLOOD, &
           IM%PKDI%XP_IFLOOD, IM%PKDI%XP_PFLOOD, IM%PKDI%XP_LE_FLOOD, IM%PKDI%XP_LEI_FLOOD, IM%I%XSODELX, &
           IM%PKI%XP_LAT, IM%PKI%XP_LON, IM%PKI%XP_TG, IM%PKI%XP_WG, IM%PKI%XP_WGI, IM%PKI%XP_CPS, &
           IM%PKI%XP_LVTT, IM%PKI%XP_LSTT, IM%PKI%XP_WR, IM%PKI%XP_WRL, IM%PKI%XP_WRLI, IM%PKI%XP_WRVN, &
           IM%PKI%XP_TV, IM%PKI%XP_TL, &
           IM%PKI%XP_RESA, IM%PKI%XP_ANFM, IM%PKI%XP_FSAT, IM%PKI%XP_SNOWALB, IM%PKI%XP_SNOWALBVIS, &
           IM%PKI%XP_SNOWALBNIR, IM%PKI%XP_SNOWALBFIR, IM%PKI%XP_SNOWSWE, IM%PKI%XP_SNOWHEAT, &
           IM%PKI%XP_SNOWRHO, IM%PKI%XP_SNOWGRAN1, IM%PKI%XP_SNOWGRAN2, IM%PKI%XP_SNOWHIST, IM%PKI%XP_SNOWAGE, &
           IM%PKDI%XP_GRNDFLUX, IM%PKDI%XP_HPSNOW, IM%PKDI%XP_SNOWHMASS, IM%PKDI%XP_RNSNOW, IM%PKDI%XP_HSNOW, &
           IM%PKDI%XP_GFLUXSNOW, IM%PKDI%XP_USTARSNOW, IM%PKDI%XP_SRSFC, IM%PKDI%XP_RRSFC, IM%PKDI%XP_LESL,   &
           IM%PKI%XP_SNOWEMIS, IM%PKDI%XP_CDSNOW, IM%PKDI%XP_CHSNOW, IM%PKDI%XP_TSRAD, IM%PKDI%XP_TS, &
           IM%PKDI%XP_HV, IM%PKDI%XP_QS, IM%PKDI%XP_SNOWTEMP, IM%PKDI%XP_SNOWLIQ, IM%PKDI%XP_SNOWDZ, &
           IM%PKDI%XP_CG, IM%PKDI%XP_C1, IM%PKDI%XP_C2, IM%PKDI%XP_WGEQ, IM%PKDI%XP_CT, IM%PKDI%XP_CH, IM%PKDI%XP_CD, &
           IM%PKDI%XP_CDN, IM%PKDI%XP_RI, IM%PKDI%XP_HU, IM%PKDI%XP_HUG, ZP_EMIS, IM%PKDI%XP_ALBT, IM%PKDI%XP_RS, &
           IM%PKI%XP_LE, IM%PKDI%XP_RN, IM%PKDI%XP_H, IM%PKDI%XP_LEI, IM%PKDI%XP_LEGI, IM%PKDI%XP_LEG, IM%PKDI%XP_LEV, &
           IM%PKDI%XP_LES, IM%PKDI%XP_LER, IM%PKDI%XP_LETR, IM%PKDI%XP_EVAP, IM%PKDI%XP_GFLUX, IM%PKDI%XP_RESTORE, &
           ZP_USTAR, IM%PKDI%XP_DRAIN, IM%PKDI%XP_RUNOFF, IM%PKDI%XP_MELT, IM%PKDI%XP_MELTADV, IM%PKI%XP_TC, &
           IM%PKI%XP_QC, IM%PKDI%XP_RN_ISBA, IM%PKDI%XP_H_ISBA, IM%PKDI%XP_LEG_ISBA, IM%PKDI%XP_LEGI_ISBA, &
           IM%PKDI%XP_LEV_ISBA, IM%PKDI%XP_LETR_ISBA, IM%PKDI%XP_USTAR_ISBA, IM%PKDI%XP_LER_ISBA, IM%PKDI%XP_LE_ISBA, &
           IM%PKDI%XP_LEI_ISBA, IM%PKDI%XP_GFLUX_ISBA, IM%PKDI%XP_HORT, IM%PKDI%XP_DRIP, IM%PKDI%XP_RRVEG, &
           ZP_AC_AGG, ZP_HU_AGG, IM%PKI%XP_FAPARC, IM%PKI%XP_FAPIRC, IM%PKI%XP_MUS, IM%PKI%XP_LAI_EFFC, IM%PKI%XP_AN,   &
           IM%PKI%XP_ANDAY, ZP_RESP_BIOMASS_INST, IM%PKDI%XP_IACAN, IM%PKI%XP_ANF, IM%PKDI%XP_GPP, IM%PKDI%XP_FAPAR, &
           IM%PKDI%XP_FAPIR, IM%PKDI%XP_FAPAR_BS, IM%PKDI%XP_FAPIR_BS, IM%PKDI%XP_IRRIG_FLUX, ZP_DEEP_FLUX,  &
           IM%PKDI%XP_SWNET_V, IM%PKDI%XP_SWNET_G, IM%PKDI%XP_SWNET_N, IM%PKDI%XP_SWNET_NS, IM%PKDI%XP_LWNET_V, &
           IM%PKDI%XP_LWNET_G, IM%PKDI%XP_LWNET_N, IM%PKDI%XP_LEVCV, IM%PKDI%XP_LESC, IM%PKDI%XP_H_V_C, &
           IM%PKDI%XP_H_G_C, IM%PKDI%XP_LETRGV, IM%PKDI%XP_LETRCV, IM%PKDI%XP_LERGV, IM%PKDI%XP_LELITTER, &
           IM%PKDI%XP_LELITTERI,IM%PKDI%XP_DRIPLIT,IM%PKDI%XP_RRLIT, IM%PKDI%XP_LERCV, IM%PKDI%XP_H_C_A, &
           IM%PKDI%XP_H_N_C, IM%PKDI%XP_LE_C_A, IM%PKDI%XP_LE_V_C, IM%PKDI%XP_LE_G_C,IM%PKDI%XP_LE_N_C, &
           IM%PKDI%XP_EVAP_N_C, IM%PKDI%XP_EVAP_G_C, IM%PKDI%XP_SR_GN, IM%PKDI%XP_MELTCV, IM%PKDI%XP_FRZCV,   &
           IM%PKDI%XP_SWDOWN_GN, IM%PKDI%XP_LWDOWN_GN, ZIRRIG_GR, IM%PKI%XP_TOPQS, IM%PKDI%XP_QSB, IM%PKDI%XP_SUBL, &
           IM%PKI%XP_FWTD, IM%PKI%XP_WTD, IM%PKDI%XP_SNDRIFT               )
!
ZP_TRAD=IM%PKDI%XP_TSRAD
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Glacier : ice runoff flux (especally for Earth System Model)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF(IM%I%LGLACIER)THEN
!           
  CALL HYDRO_GLACIER(IM%I, &
                     PTSTEP,ZP_SNOW,IM%PKI%XP_SNOWRHO,IM%PKI%XP_SNOWSWE,IM%PKI%XP_ICE_STO,IM%PKDI%XP_ICEFLUX)
!     
ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Calculation of ISBA water and energy budget (and time tendencies of each reservoir)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL ISBA_BUDGET(IM%DGEI, &
                 IM%I%CISBA,IM%I%TSNOW%SCHEME,IM%I%LGLACIER,PTSTEP,          &
                 IM%PKI%XP_WG,IM%PKI%XP_WGI,IM%PKI%XP_WR,IM%PKI%XP_SNOWSWE,  &
                 IM%PKI%XP_DG,IM%PKI%XP_DZG,ZP_WG_INI,ZP_WGI_INI,ZP_WR_INI,  &
                 ZP_SWE_INI,ZP_RAIN,ZP_SNOW,IM%PKDI%XP_EVAP,IM%PKDI%XP_DRAIN,&
                 IM%PKDI%XP_RUNOFF,IM%PKDI%XP_IFLOOD,IM%PKDI%XP_PFLOOD,      &
                 IM%PKDI%XP_LE_FLOOD, IM%PKDI%XP_LEI_FLOOD,                  &
                 IM%PKDI%XP_ICEFLUX,IM%PKDI%XP_IRRIG_FLUX,IM%PKDI%XP_SNDRIFT,&
                 IM%PKI%XP_LVTT, IM%PKI%XP_LSTT,                             &
                 IM%PKDI%XP_DWG,IM%PKDI%XP_DWGI,IM%PKDI%XP_DWR,              &
                 IM%PKDI%XP_DSWE,IM%PKDI%XP_WATBUD                           )
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Evolution of soil albedo, when depending on surface soil wetness:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (IM%I%CALBEDO=='EVOL' .AND. IM%I%LECOCLIMAP) THEN
  CALL SOIL_ALBEDO(IM%I%CALBEDO,                                    &
                   IM%PKI%XP_WSAT(:,1),IM%PKI%XP_WG(:,1),                    &
                   IM%PKI%XP_ALBVIS_DRY,IM%PKI%XP_ALBNIR_DRY,IM%PKI%XP_ALBUV_DRY,   &
                   IM%PKI%XP_ALBVIS_WET,IM%PKI%XP_ALBNIR_WET,IM%PKI%XP_ALBUV_WET,   &
                   IM%PKI%XP_ALBVIS_SOIL,IM%PKI%XP_ALBNIR_SOIL,IM%PKI%XP_ALBUV_SOIL )  
  !
  CALL ALBEDO(IM%I%CALBEDO,                                          &
              IM%PKI%XP_ALBVIS_VEG,IM%PKI%XP_ALBNIR_VEG,IM%PKI%XP_ALBUV_VEG,IM%PKI%XP_VEG,  &
              IM%PKI%XP_ALBVIS_SOIL,IM%PKI%XP_ALBNIR_SOIL,IM%PKI%XP_ALBUV_SOIL,      &
              IM%PKI%XP_ALBVIS,IM%PKI%XP_ALBNIR,IM%PKI%XP_ALBUV                      )  
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Vegetation evolution for interactive LAI
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (IM%I%CPHOTO=='LAI' .OR. IM%I%CPHOTO=='LST' .OR. IM%I%CPHOTO=='NIT' .OR. IM%I%CPHOTO=='NCB') THEN
  CALL VEGETATION_EVOL(IM%I%CISBA, IM%I%CPHOTO, IM%I%CRESPSL, IM%I%CALBEDO, LAGRIP, IM%I%LTR_ML,           &
                       IM%I%LNITRO_DILU, IM%I%LAGRI_TO_GRASS, IM%DTI%LIMP_VEG, IM%DTI%LIMP_Z0, IM%DTI%LIMP_EMIS, &
                       PTSTEP, KMONTH, KDAY, IM%I%NSPINW, PTIME, IM%PKI%XP_LAT, ZP_RHOA,      &
                       IM%PKI%XP_DG, IM%PKI%XP_DZG, IM%PKI%NK_WG_LAYER,                         &                       
                       IM%PKI%XP_TG, IM%PKI%XP_ALBNIR_VEG, IM%PKI%XP_ALBVIS_VEG, IM%PKI%XP_ALBUV_VEG,         &
                       IM%PKI%XP_ALBNIR_SOIL, IM%PKI%XP_ALBVIS_SOIL, IM%PKI%XP_ALBUV_SOIL,             &
                       IM%PKI%XP_VEGTYPE_PATCH, IM%PKI%XP_SEFOLD, IM%PKI%XP_ANMAX, IM%PKI%XP_H_TREE, IM%PKI%XP_BSLAI,&
                       IM%PKI%XP_LAIMIN, ZP_CO2, IM%PKI%XP_CE_NITRO, IM%PKI%XP_CF_NITRO, IM%PKI%XP_CNA_NITRO, &
                       IM%PKI%XP_BSLAI_NITRO, IM%PKI%XP_GMES, IM%PKI%XP_TAU_WOOD, IM%PKI%TP_SEED,             &
                       IM%PKI%TP_REAP, IM%PKI%XP_AOSIP, IM%PKI%XP_AOSIM, IM%PKI%XP_AOSJP, IM%PKI%XP_AOSJM,           &
                       IM%PKI%XP_HO2IP, IM%PKI%XP_HO2IM, IM%PKI%XP_HO2JP, IM%PKI%XP_HO2JM, IM%PKI%XP_Z0EFFIP,        &
                       IM%PKI%XP_Z0EFFIM, IM%PKI%XP_Z0EFFJP, IM%PKI%XP_Z0EFFJM, IM%PKI%XP_LAI, IM%PKI%XP_VEG,        &
                       IM%PKI%XP_Z0, IM%PKI%XP_ALBNIR, IM%PKI%XP_ALBVIS, IM%PKI%XP_ALBUV, IM%PKI%XP_EMIS,            &
                       IM%PKI%XP_ANFM, IM%PKI%XP_ANDAY, IM%PKI%XP_BIOMASS, IM%PKI%XP_RESP_BIOMASS,            &
                       ZP_RESP_BIOMASS_INST, IM%PKI%XP_INCREASE, IM%PKI%XP_TURNOVER, &
                       ! add optional for accurate dependency to nitrogen
                       ! limitation
                        PSWDIR=ZP_GLOBAL_SW ) 
END IF
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Diagnostic of respiration carbon fluxes and soil carbon evolution
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ZP_SFCO2    (:)=0.
IM%PKDI%XP_RESP_ECO (:)=0.
IM%PKDI%XP_RESP_AUTO(:)=0.
!
IF ( IM%I%CPHOTO/='NON' .AND. IM%I%CRESPSL/='NON' .AND. ANY(IM%PKI%XP_LAI(:)/=XUNDEF) ) THEN
  CALL CARBON_EVOL(IM%I%CISBA, IM%I%CRESPSL, IM%I%CPHOTO, PTSTEP, IM%I%NSPINS,                   &
                   ZP_RHOA, IM%PKI%XP_TG, IM%PKI%XP_WG, IM%PKI%XP_WFC, IM%PKI%XP_WWILT, IM%PKI%XP_WSAT, IM%PKI%XP_SAND,&
                   IM%PKI%XP_DG, IM%PKI%XP_DZG, IM%PKI%NK_WG_LAYER,                               &                   
                   IM%PKI%XP_RE25, IM%PKI%XP_LAI, ZP_RESP_BIOMASS_INST, IM%PKI%XP_TURNOVER,       &
                   IM%PKI%XP_LITTER, IM%PKI%XP_LIGNIN_STRUC , IM%PKI%XP_SOILCARB,                 &
                   IM%PKDI%XP_RESP_AUTO, IM%PKDI%XP_RESP_ECO                                 )  
  ! calculation of vegetation CO2 flux
  ! Positive toward the atmosphere
  ZP_SFCO2(:) = IM%PKDI%XP_RESP_ECO(:) - IM%PKDI%XP_GPP(:)  
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Reset effecitve roughness lentgh to its nominal value when snow has just disappeared
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL SUBSCALE_Z0EFF(IM%PKI%XP_AOSIP,IM%PKI%XP_AOSIM,IM%PKI%XP_AOSJP,IM%PKI%XP_AOSJM,            &
                    IM%PKI%XP_HO2IP,IM%PKI%XP_HO2IM,IM%PKI%XP_HO2JP,IM%PKI%XP_HO2JM,IM%PKI%XP_Z0,      &
                    IM%PKI%XP_Z0EFFIP,IM%PKI%XP_Z0EFFIM,IM%PKI%XP_Z0EFFJP,IM%PKI%XP_Z0EFFJM,    &
                    OMASK=(IM%PKI%XP_SNOWSWE(:,1)==0. .AND. IM%PKI%XP_PSN(:)>0.)  )   
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Turbulent fluxes
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ZP_SFTH(:) = IM%PKDI%XP_H(:)
ZP_SFTQ(:) = IM%PKDI%XP_EVAP(:)

ZP_SFU (:) = 0.
ZP_SFV (:) = 0.
WHERE (ZP_WIND>0.)
  ZP_SFU (:) = - ZP_U(:)/ZP_WIND(:) * ZP_USTAR(:)**2 * ZP_RHOA(:)
  ZP_SFV (:) = - ZP_V(:)/ZP_WIND(:) * ZP_USTAR(:)**2 * ZP_RHOA(:)
END WHERE
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Scalar fluxes
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ZP_SFTS(:,:) = 0.
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
! --------------------------------------------------------------------------------------
! Chemical dry deposition :
! --------------------------------------------------------------------------------------
IF (IM%CHI%SVI%NBEQ>0) THEN
  IF( IM%CHI%CCH_DRY_DEP == "WES89") THEN

    CALL CH_DEP_ISBA    (ZP_USTAR, IM%PKDI%XP_HU, IM%PKI%XP_PSN,             &
                        IM%PKI%XP_VEG, IM%PKI%XP_LAI, IM%PKI%XP_SAND, IM%PKI%XP_CLAY, IM%PKI%XP_RESA, &
                        IM%PKDI%XP_RS(:),  IM%PKI%XP_Z0(:),                       &
                        ZP_TA, ZP_PA, ZP_TRAD(:),                  &
                        IM%PKI%XP_VEGTYPE_PATCH(:,NVT_NO),                &
                        IM%PKI%XP_VEGTYPE_PATCH(:,NVT_ROCK),              &
                        IM%CHI%SVI%CSV(IM%CHI%SVI%NSV_CHSBEG:IM%CHI%SVI%NSV_CHSEND),                &
                        IM%PKCI%XP_SOILRC_SO2,  IM%PKCI%XP_SOILRC_O3 ,             &
                        IM%PKCI%XP_DEP(:,1:IM%CHI%SVI%NBEQ)                           )  
 
    ZP_SFTS(:,IM%CHI%SVI%NSV_CHSBEG:IM%CHI%SVI%NSV_CHSEND) = - ZP_SV(:,IM%CHI%SVI%NSV_CHSBEG:IM%CHI%SVI%NSV_CHSEND)  &
                                                    * IM%PKCI%XP_DEP(:,1:IM%CHI%SVI%NBEQ)  
    IF (IM%CHI%SVI%NAEREQ > 0 ) THEN
      CALL CH_AER_DEP(ZP_SV(:,IM%CHI%SVI%NSV_AERBEG:IM%CHI%SVI%NSV_AEREND),&
                           ZP_SFTS(:,IM%CHI%SVI%NSV_AERBEG:IM%CHI%SVI%NSV_AEREND),&
                           ZP_USTAR, IM%PKI%XP_RESA,ZP_TA,ZP_RHOA)     
    END IF
  ELSE
    ZP_SFTS(:,IM%CHI%SVI%NSV_CHSBEG:IM%CHI%SVI%NSV_CHSEND) = 0.
    ZP_SFTS(:,IM%CHI%SVI%NSV_AERBEG:IM%CHI%SVI%NSV_AEREND) = 0.
  ENDIF
ENDIF
!
! --------------------------------------------------------------------------------------
! Dust deposition and emission:
! --------------------------------------------------------------------------------------
!
IF(IM%CHI%SVI%NDSTEQ>0)THEN
  IDST = IM%CHI%SVI%NSV_DSTEND - IM%CHI%SVI%NSV_DSTBEG + 1

  CALL COUPLING_DST_n(DST, IM%PKI, &
            HPROGRAM,                    &!I [char] Name of program
            KSIZE,      &!I [nbr] number of points in patch
            IDST,                        &!I [nbr] number of dust emissions variables
            JPATCH,                      &!I [idx] patch in question
            IM%PKI%XP_CLAY(:,1),                &!I [frc] mass fraction clay in first soil layer
            ZP_PS,                       &!I [Pa] surface pressure
            IM%PKDI%XP_TS,                       &!I [K] surface temperature
            ZP_QA,                       &!I [kg/kg] specific humidity
            IM%PKI%XP_RESA,                     &!I [s/m] atmospheric resistance
            ZP_RHOA,                     &!I [kg/m3] atmospheric density
            IM%PKI%XP_SAND(:,1),                &!I [frc] mass fraction of sand in first soil layer
            ZP_PA,                       &!I [K] Atmospheric pressure
            ZP_TA,                       &!I [K] Atmospheric temperature
            IM%PKI%XP_TG(:,1),                  &!I [K] Ground temperature
            ZP_U,                        &!I [m/s] zonal wind at atmospheric height 
            ZP_UREF,                     &!I [m] reference height of wind
            ZP_V,                        &!I [m/s] meridional wind at atmospheric height
            IM%PKI%XP_WG(:,1),                  &!I [m3/m3] ground volumetric water content
            IM%PKI%XP_WSAT(:,1),                &!I [m3/m3] saturation volumetric water content
            ZP_ZREF,                     &!I [m] reference height of wind
            IM%PKDI%XP_CD,                       &!I [] Drag Coefficient for momentum
            IM%PKDI%XP_CDN,                      &!I [] Drag neutral Coefficient for momentum
            IM%PKDI%XP_CH,                       &!I [] drag coefficient for heat
            IM%PKDI%XP_RI,                       &!I [] Richardson number
            IM%PKDI%XP_Z0H_WITH_SNOW,            &!I [frc] Z0 (heat) with snow
            ZP_SFTS(:,IM%CHI%SVI%NSV_DSTBEG:IM%CHI%SVI%NSV_DSTEND)  &!O [kg/m2/sec] flux of dust            
            )  
!
   IF (IM%CHI%SVI%NSV_AEREND > 0)  THEN ! case of dust/ anthropogenic aerosols coupling
     DO JMODE=1,NDSTMDE
       !
       !Make index which is 0 for first mode, 3 for second, 6 for third etc
       IF (LVARSIG_DST) THEN
         JSV_IDX = (JMODE-1)*3
       ELSE IF (LRGFIX_DST) THEN
         JSV_IDX = JMODE-2
       ELSE
         JSV_IDX = (JMODE-1)*2
       END IF
       !
       DO JSV=1, size(HSV)
         IF ((TRIM(HSV(JSV)) == "@DSTI").AND.(JMODE==3)) THEN 
           ! add dust flux and conversion kg/m2/s into molec.m2/s
           ZP_SFTS(:,JSV) = ZP_SFTS(:,JSV) + ZP_SFTS(:,IM%CHI%SVI%NSV_DSTBEG-1+JSV_IDX+2)*XAVOGADRO/XMOLARWEIGHT_DST
         END IF
         IF ( (TRIM(HSV(JSV)) == "@DSTJ").AND.(JMODE==2)) THEN 
           ! add dust flux and conversion kg/m2/sec into molec.m2/s
           ZP_SFTS(:,JSV) = ZP_SFTS(:,JSV) + ZP_SFTS(:,IM%CHI%SVI%NSV_DSTBEG-1+JSV_IDX+2)*XAVOGADRO/XMOLARWEIGHT_DST
         END IF
       END DO
       !
     END DO
    END IF
!    
!Modify fluxes due to dry deposition, we introduce a negative flux where dust is lost
  CALL DSLT_DEP(ZP_SV(:,IM%CHI%SVI%NSV_DSTBEG:IM%CHI%SVI%NSV_DSTEND), &
                ZP_SFTS(:,IM%CHI%SVI%NSV_DSTBEG:IM%CHI%SVI%NSV_DSTEND), &
                ZP_USTAR, IM%PKI%XP_RESA, ZP_TA, ZP_RHOA, DST%XEMISSIG_DST, DST%XEMISRADIUS_DST, &
                JPMODE_DST, XDENSITY_DST, XMOLARWEIGHT_DST, ZCONVERTFACM0_DST,    &
                ZCONVERTFACM6_DST, ZCONVERTFACM3_DST, LVARSIG_DST, LRGFIX_DST,    &
                CVERMOD  )
!
!Transfer these fluxes to fluxes understandable by all moments
  CALL MASSFLUX2MOMENTFLUX(           &
    ZP_SFTS(:,IM%CHI%SVI%NSV_DSTBEG:IM%CHI%SVI%NSV_DSTEND), & !I/O ![kg/m2/sec] In: flux of only mass, out: flux of moments
    ZP_RHOA,                          & !I [kg/m3] air density
    DST%XEMISRADIUS_DST,                  &!I [um] emitted radius for the modes (max 3)
    DST%XEMISSIG_DST,                     &!I [-] emitted sigma for the different modes (max 3)
    NDSTMDE,                          &
    ZCONVERTFACM0_DST,                &
    ZCONVERTFACM6_DST,                &
    ZCONVERTFACM3_DST,                &
    LVARSIG_DST, LRGFIX_DST           )   
!
ENDIF !Check on CDSTYN
!
! --------------------------------------------------------------------------------------
! Sea Salt deposition
! --------------------------------------------------------------------------------------
!
IF (IM%CHI%SVI%NSLTEQ>0) THEN
  CALL DSLT_DEP(ZP_SV(:,IM%CHI%SVI%NSV_SLTBEG:IM%CHI%SVI%NSV_SLTEND), &
                ZP_SFTS(:,IM%CHI%SVI%NSV_SLTBEG:IM%CHI%SVI%NSV_SLTEND), &
                ZP_USTAR, IM%PKI%XP_RESA, ZP_TA, ZP_RHOA, SLT%XEMISSIG_SLT, SLT%XEMISRADIUS_SLT, &
                JPMODE_SLT, XDENSITY_SLT, XMOLARWEIGHT_SLT, ZCONVERTFACM0_SLT,    &
                ZCONVERTFACM6_SLT, ZCONVERTFACM3_SLT, LVARSIG_SLT, LRGFIX_SLT,    &
                CVERMOD  )  

  CALL MASSFLUX2MOMENTFLUX(           &
    ZP_SFTS(:,IM%CHI%SVI%NSV_SLTBEG:IM%CHI%SVI%NSV_SLTEND), & !I/O ![kg/m2/sec] In: flux of only mass, out: flux of moments
    ZP_RHOA,                          & !I [kg/m3] air density
    SLT%XEMISRADIUS_SLT,                  &!I [um] emitted radius for the modes (max 3)
    SLT%XEMISSIG_SLT,                     &!I [-] emitted sigma for the different modes (max 3)
    NSLTMDE,                          &
    ZCONVERTFACM0_SLT,                &
    ZCONVERTFACM6_SLT,                &
    ZCONVERTFACM3_SLT,                &
    LVARSIG_SLT, LRGFIX_SLT         ) 
ENDIF !Check on CSLTYN
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Inline diagnostics
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL DIAG_INLINE_ISBA_n(IM%DGEI, IM%DGI, IM%I, IM%PKDI, &
                         ZP_TA, IM%PKDI%XP_TS, ZP_QA, ZP_PA, ZP_PS, ZP_RHOA, ZP_U, ZP_V,       &
                          ZP_ZREF, ZP_UREF,                                            &
                          IM%PKDI%XP_CD, IM%PKDI%XP_CDN, IM%PKDI%XP_CH, IM%PKDI%XP_RI, &
                          IM%PKDI%XP_HU, IM%PKDI%XP_Z0_WITH_SNOW,         &
                          IM%PKDI%XP_Z0H_WITH_SNOW, IM%PKDI%XP_Z0EFF,                                  &
                          ZP_SFTH, ZP_SFTQ, ZP_SFU, ZP_SFV, IM%PKDI%XP_QS,                     &
                          IM%PKI%XP_DIR_ALB_WITH_SNOW, IM%PKI%XP_SCA_ALB_WITH_SNOW,                  &
                          ZP_DIR_SW, ZP_SCA_SW, ZP_LW, IM%PKDI%XP_RN                           )  
!
!
!-------------------------------------------------------------------------------
!Physical properties see by the atmosphere in order to close the energy budget 
!between surfex and the atmosphere. All variables should be at t+1 but very 
!difficult to do. Maybe it will be done later. However, Ts can be at time t+1
!-------------------------------------------------------------------------------
!
ZP_TSURF (:) = IM%PKDI%XP_TS (:)
ZP_Z0    (:) = IM%PKDI%XP_Z0_WITH_SNOW (:)
ZP_Z0H   (:) = IM%PKDI%XP_Z0H_WITH_SNOW(:)
ZP_QSURF (:) = IM%PKDI%XP_QS (:)
!
!-------------------------------------------------------------------------------
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Isba offline diagnostics for each patch
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL DIAG_EVAP_ISBA_n(IM%DGEI, IM%DGI, IM%I, IM%PKDI, IM%PKI, &
                       IM%I%CPHOTO,PTSTEP,KMASK,KSIZE,JPATCH,ZP_RHOA)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Isba offline diagnostics for miscellaneous terms over each patch
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL DIAG_MISC_ISBA_n(IM%DGMI, IM%PKDI, &
                       PTSTEP, IM%I%CISBA, IM%I%CPHOTO, IM%I%TSNOW%SCHEME, LAGRIP, IM%I%LTR_ML,    &
                      PTIME, KSIZE, JPATCH, KMASK, IM%PKI%XP_THRESHOLD,              &
                      IM%PKI%XP_PSN, IM%PKI%XP_PSNG, IM%PKI%XP_PSNV, IM%PKI%XP_FF, IM%PKI%XP_FFG, IM%PKI%XP_FFV,        &
                      IM%PKI%XP_WG, IM%PKI%XP_WGI, IM%PKI%XP_WFC, IM%PKI%XP_WWILT, IM%PKI%XP_SNOWSWE, IM%PKI%XP_SNOWRHO,&
                      IM%PKI%XP_FAPARC, IM%PKI%XP_FAPIRC, IM%PKI%XP_LAI_EFFC, IM%PKI%XP_MUS, IM%PKI%XP_FSAT,     &
                      IM%PKI%XP_DG, IM%PKI%XP_TG       )                  
!
! Unpack ISBA diagnostics (modd_diag_isban) for each patch:ISIZE_MAX = MAXVAL(NSIZE_NATURE_P)

!  (MUST be done BEFORE UNPACK_ISBA_PATCH, because of XP_LE)
!
 CALL UNPACK_DIAG_PATCH_n(IM%DGI, IM%GB, IM%I, IM%PKDI, IM%PKI, &
                          KMASK,KSIZE,IM%I%NPATCH,JPATCH, &
                           ZCPL_DRAIN,ZCPL_RUNOFF,ZCPL_EFLOOD,ZCPL_PFLOOD,           &
                           ZCPL_IFLOOD, ZCPL_ICEFLUX)  
!
! for chemical deposition
!
IF (IM%CHI%SVI%NBEQ>0) THEN
  IF( IM%CHI%CCH_DRY_DEP == "WES89") THEN
    CALL UNPACK_CH_ISBA_PATCH_n(IM%CHI, IM%PKCI, &
                                KMASK,KSIZE,IM%I%NPATCH,JPATCH)     
  END IF
END IF
!
! Unpack ISBA variables (modd_isban) for each patch:
!
 CALL UNPACK_ISBA_PATCH_n(IM%AG, IM%I, IM%PKI, &
                          KMASK,KSIZE,JPATCH)
!
!----------------------------------------------------------------------
!
! for further chemical biogenic emissions
!
IF (IM%CHI%SVI%NBEQ>0 .AND. IM%CHI%LCH_BIO_FLUX) THEN
  !
  DO JJ=1,KSIZE
    ZSW_FORBIO(KMASK(JJ),JPATCH) = 0.
  ENDDO
  !
  DO JSWB=1,ISWB
!cdir nodep
!cdir unroll=8
    DO JJ=1,KSIZE
      ZSW_FORBIO(KMASK(JJ),JPATCH) = ZSW_FORBIO(KMASK(JJ),JPATCH)              &
                                     + ZP_DIR_SW(JJ,JSWB) + ZP_SCA_SW(JJ,JSWB)  
    ENDDO
  ENDDO
  !
ENDIF
!----------------------------------------------------------------------
!
! Unpack output dummy arguments for each patch:
!
IF (IM%I%NPATCH==1) THEN
   ZSFTQ_TILE      (:,JPATCH)  = ZP_SFTQ      (:)
   ZSFTH_TILE      (:,JPATCH)  = ZP_SFTH      (:)
   ZSFTS_TILE      (:,:,JPATCH)= ZP_SFTS      (:,:)
   ZSFCO2_TILE     (:,JPATCH)  = ZP_SFCO2     (:)
   ZSFU_TILE       (:,JPATCH)  = ZP_SFU       (:)
   ZSFV_TILE       (:,JPATCH)  = ZP_SFV       (:)
   ZTRAD_TILE      (:,JPATCH)  = ZP_TRAD      (:)
   ZTSURF_TILE     (:,JPATCH)  = ZP_TSURF     (:)
   ZZ0_TILE        (:,JPATCH)  = ZP_Z0        (:)
   ZZ0H_TILE       (:,JPATCH)  = ZP_Z0H       (:)
   ZQSURF_TILE     (:,JPATCH)  = ZP_QSURF     (:)   
ELSE
!cdir nodep
!cdir unroll=8
 DO JJ=1,KSIZE
   JI = KMASK(JJ)
   ZSFTQ_TILE      (JI,JPATCH)  = ZP_SFTQ      (JJ)
   ZSFTH_TILE      (JI,JPATCH)  = ZP_SFTH      (JJ)
   ZSFCO2_TILE     (JI,JPATCH)  = ZP_SFCO2     (JJ)
   ZSFU_TILE       (JI,JPATCH)  = ZP_SFU       (JJ)
   ZSFV_TILE       (JI,JPATCH)  = ZP_SFV       (JJ)
   ZTRAD_TILE      (JI,JPATCH)  = ZP_TRAD      (JJ)
   ZTSURF_TILE     (JI,JPATCH)  = ZP_TSURF     (JJ)
   ZZ0_TILE        (JI,JPATCH)  = ZP_Z0        (JJ)
   ZZ0H_TILE       (JI,JPATCH)  = ZP_Z0H       (JJ)
   ZQSURF_TILE     (JI,JPATCH)  = ZP_QSURF     (JJ)   
 ENDDO
!
!cdir nodep
!cdir unroll=8
  DO JK=1,SIZE(ZP_SFTS,2)
    DO JJ=1,KSIZE
      JI=KMASK(JJ)    
      ZSFTS_TILE      (JI,JK,JPATCH)= ZP_SFTS      (JJ,JK)
    ENDDO
  ENDDO
ENDIF
!
!----------------------------------------------------------------------
!
! Get output dust flux if we are calculating dust
IF (NDSTMDE .GE. 1) IMOMENT = INT(IDST / NDSTMDE)
IF (IM%CHI%SVI%NDSTEQ>0) THEN
  DO JSV = 1,NDSTMDE
    IF (IMOMENT == 1) THEN
      DST%XSFDST(:,JSV,JPATCH)=ZSFTS_TILE(:,NDST_MDEBEG+JSV-1,JPATCH)
    ELSE
      DST%XSFDST(:,JSV,JPATCH)=ZSFTS_TILE(:,NDST_MDEBEG+(JSV-1)*IMOMENT+1,JPATCH)
    END IF

    DST%XSFDSTM(:,JSV,JPATCH)=DST%XSFDSTM(:,JSV,JPATCH) + DST%XSFDST(:,JSV,JPATCH) * PTSTEP
  ENDDO
ENDIF
!
IF (LHOOK) CALL DR_HOOK('COUPLING_ISBA_n:TREAT_PATCH',1,ZHOOK_HANDLE)
!
END SUBROUTINE TREAT_PATCH
!==========================================================================================
END SUBROUTINE COUPLING_ISBA_n
