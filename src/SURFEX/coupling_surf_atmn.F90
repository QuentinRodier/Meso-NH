!     #################################################################################
SUBROUTINE COUPLING_SURF_ATM_n(HPROGRAM, HCOUPLING, PTIMEC,                                &
               PTSTEP, KYEAR, KMONTH, KDAY, PTIME, KI, KSV, KSW, PTSUN, PZENITH,           &
               PZENITH2,PAZIM,PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV, PCO2,       &
               HSV, PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA,              &
               PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,                                    &
               PTRAD, PDIR_ALB, PSCA_ALB, PEMIS,                                           &
               PPEW_A_COEF, PPEW_B_COEF,                                                   &
               PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,                         &
               HTEST                                                                       )
!     #################################################################################
!
!!****  *COUPLING_INLAND_WATER_n * - Driver to call the schemes for the 
!!       four surface types (SEA, WATER, NATURE, TOWN)
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
!!      Modified    09/2011 by S.Queguiner: Add total CO2 surface flux (anthropo+biogenic) as diagnostic
!!      Modified    11/2011 by S.Queguiner: Add total Chemical surface flux (anthropo) as diagnostic
!!-------------------------------------------------------------
!
USE MODD_SURF_CONF,      ONLY : CPROGNAME
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_CSTS,           ONLY : XP00, XCPD, XRD, XAVOGADRO
USE MODD_SURF_ATM_GRID_n,ONLY : XLON
USE MODD_SURF_ATM_n,     ONLY : NDIM_SEA, NDIM_WATER, NDIM_TOWN, NDIM_NATURE, &
                                NSIZE_SEA, NSIZE_WATER, NSIZE_TOWN, NSIZE_NATURE, &
                                NR_SEA,    NR_WATER,    NR_TOWN,    NR_NATURE,    &
                                XSEA,      XWATER,      XTOWN,      XNATURE,      &
                                TTIME, NSIZE_FULL
USE MODD_SURF_ATM_SSO_n, ONLY : CROUGH
USE MODD_DATA_COVER_PAR, ONLY : NTILESFC
USE MODD_SV_n,           ONLY : NBEQ,NSV_CHSBEG,NSV_CHSEND, &
                                NDSTEQ,NSV_DSTBEG,NSV_DSTEND,&
                                NAEREQ,NSV_AERBEG,NSV_AEREND, CSV
!
USE MODD_CH_SURF_n,      ONLY : LCH_SURF_EMIS, LCH_EMIS, CCH_EMIS
USE MODD_CH_EMIS_FIELD_n,ONLY : TSEMISS
!
USE MODD_SURFEX_MPI, ONLY : XTIME_SEA, XTIME_WATER, XTIME_NATURE, XTIME_TOWN
!
USE MODI_ADD_FORECAST_TO_DATE_SURF
USE MODI_AVERAGE_FLUX
USE MODI_AVERAGE_RAD
USE MODI_DIAG_INLINE_SURF_ATM_n
USE MODI_CH_EMISSION_FLUX_n
USE MODI_CH_EMISSION_SNAP_n
USE MODI_CH_EMISSION_TO_ATM_n
USE MODI_SSO_Z0_FRICTION_n
USE MODI_SSO_BE04_FRICTION_n
!
USE MODI_RW_PRECIP_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_COUPLING_INLAND_WATER_n
!
USE MODI_COUPLING_NATURE_n
!
USE MODI_COUPLING_SEA_n
!
USE MODI_COUPLING_TOWN_n
!
IMPLICIT NONE
!
#ifndef NOMPI
INCLUDE 'mpif.h'
#endif
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=1),    INTENT(IN)  :: HCOUPLING ! type of coupling
                                              ! 'E' : explicit
                                              ! 'I' : implicit
REAL,                INTENT(IN)  :: PTIMEC    ! cumulated time since beginning of simulation
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
REAL, DIMENSION(KI), INTENT(INOUT) :: PSNOW     ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KI), INTENT(INOUT) :: PRAIN     ! liquid precipitation                  (kg/m2/s)
!
!
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTH     ! flux of heat                          (W/m2)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTQ     ! flux of water vapor                   (kg/m2/s)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFU      ! zonal momentum flux                   (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFV      ! meridian momentum flux                (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFCO2    ! flux of CO2                           (kg/m2/s)
REAL, DIMENSION(KI,KSV),INTENT(OUT):: PSFTS   ! flux of scalar var.                   (kg/m2/s)
!
REAL, DIMENSION(KI), INTENT(INOUT) :: PTRAD     ! radiative temperature                 (K)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PDIR_ALB! direct albedo for each spectral band  (-)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PSCA_ALB! diffuse albedo for each spectral band (-)
REAL, DIMENSION(KI), INTENT(INOUT) :: PEMIS     ! emissivity                            (-)
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
INTEGER :: JTILE                        ! loop on type of surface
LOGICAL :: GNATURE, GTOWN, GWATER, GSEA ! .T. if the corresponding surface is represented
INTEGER :: ISWB                         ! number of shortwave spectral bands
!
REAL, DIMENSION(KI)  :: ZPEW_A_COEF ! implicit coefficients
REAL, DIMENSION(KI)  :: ZPEW_B_COEF ! needed if HCOUPLING='I'
REAL, DIMENSION(KI)  :: ZPET_A_COEF
REAL, DIMENSION(KI)  :: ZPEQ_A_COEF
REAL, DIMENSION(KI)  :: ZPET_B_COEF
REAL, DIMENSION(KI)  :: ZPEQ_B_COEF
!
! Tile outputs:
!
REAL, DIMENSION(KI,NTILESFC) :: ZSFTH_TILE     ! surface heat flux (Km/s)
REAL, DIMENSION(KI,NTILESFC) :: ZSFTQ_TILE     ! surface vapor flux (kgm/kg/s)
REAL, DIMENSION(KI,KSV,NTILESFC) :: ZSFTS_TILE ! scalar surface flux
REAL, DIMENSION(KI,NTILESFC) :: ZSFCO2_TILE    ! surface CO2 flux
REAL, DIMENSION(KI,NTILESFC) :: ZSFU_TILE      ! zonal momentum flux
REAL, DIMENSION(KI,NTILESFC) :: ZSFV_TILE      ! meridian momentum flux
REAL, DIMENSION(KI,NTILESFC) :: ZTRAD_TILE     ! radiative surface temperature
REAL, DIMENSION(KI,NTILESFC) :: ZEMIS_TILE     ! emissivity
REAL, DIMENSION(KI,NTILESFC) :: ZFRAC_TILE     ! fraction of each surface type
!
REAL, DIMENSION(KI,KSW,NTILESFC) :: ZDIR_ALB_TILE ! direct albedo
REAL, DIMENSION(KI,KSW,NTILESFC) :: ZSCA_ALB_TILE ! diffuse albedo
!
REAL :: XTIME0
!
INTEGER :: IINDEXEND
INTEGER :: INBTS, JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('COUPLING_SURF_ATM_N',0,ZHOOK_HANDLE)
CPROGNAME=HPROGRAM
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('COUPLING_SURF_ATMN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Time evolution
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
TTIME%TIME = TTIME%TIME + PTSTEP
 CALL ADD_FORECAST_TO_DATE_SURF(TTIME%TDATE%YEAR,TTIME%TDATE%MONTH,TTIME%TDATE%DAY,TTIME%TIME)
!
!-------------------------------------------------------------------------------------
! Preliminaries: Tile related operations
!-------------------------------------------------------------------------------------
! FLAGS for the various surfaces:
!
GSEA      = NDIM_SEA    >0
GWATER    = NDIM_WATER  >0
GTOWN     = NDIM_TOWN   >0
GNATURE   = NDIM_NATURE >0

!
! Tile counter:
!
JTILE     = 0 
!
! Number of shortwave spectral bands
!
ISWB = SIZE(PSW_BANDS)
!
! Initialization: Outputs to atmosphere over each tile:
!
ZSFTH_TILE(:,:)       = XUNDEF
ZTRAD_TILE(:,:)       = XUNDEF
ZDIR_ALB_TILE(:,:,:)  = XUNDEF
ZSCA_ALB_TILE(:,:,:)  = XUNDEF
ZEMIS_TILE(:,:)       = XUNDEF
ZSFTQ_TILE(:,:)       = XUNDEF
ZSFTS_TILE(:,:,:)     = 0.
ZSFCO2_TILE(:,:)      = 0.
ZSFU_TILE(:,:)        = XUNDEF
ZSFV_TILE(:,:)        = XUNDEF
!
! Fractions for each tile:
!
ZFRAC_TILE(:,:)    = 0.0

!
! initialization of implicit coefficients:
!
IF (HCOUPLING=='I') THEN
  ZPEW_A_COEF = PPEW_A_COEF
  ZPEW_B_COEF = PPEW_B_COEF
  ZPET_A_COEF = PPET_A_COEF
  ZPEQ_A_COEF = PPEQ_A_COEF
  ZPET_B_COEF = PPET_B_COEF
  ZPEQ_B_COEF = PPEQ_B_COEF
ELSE
  ZPEW_A_COEF = 0.
  ZPEW_B_COEF = SQRT(PU**2+PV**2)
  ZPET_A_COEF = XUNDEF
  ZPET_B_COEF = XUNDEF
  ZPEQ_A_COEF = XUNDEF
  ZPEQ_B_COEF = XUNDEF        
END IF
!
!--------------------------------------------------------------------------------------
! Initialize/Save precip and zenith field for a ARPEGE/ALADIN run
!--------------------------------------------------------------------------------------
!
 CALL RW_PRECIP_n(HPROGRAM,PRAIN,PSNOW)
!
!--------------------------------------------------------------------------------------
! Call ALMA interfaces for sea, water, nature and town here...
!--------------------------------------------------------------------------------------
!
#ifndef NOMPI
XTIME0 = MPI_WTIME()
#endif
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! SEA Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
! first, pack vector...then call ALMA routine
!
JTILE = JTILE + 1
!
IF(GSEA)THEN
!
  ZFRAC_TILE(:,JTILE) = XSEA(:)
!
  CALL TREAT_SURF(JTILE,NSIZE_SEA,NR_SEA)
!
ENDIF
!
#ifndef NOMPI
XTIME_SEA = XTIME_SEA + (MPI_WTIME() - XTIME0)*100./MAX(1,NSIZE_SEA)
XTIME0 = MPI_WTIME()
#endif
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! INLAND WATER Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE = JTILE + 1
!
IF(GWATER)THEN
!
  ZFRAC_TILE(:,JTILE) = XWATER(:)
!
  CALL TREAT_SURF(JTILE,NSIZE_WATER,NR_WATER)
!
ENDIF 
!
#ifndef NOMPI
XTIME_WATER = XTIME_WATER + (MPI_WTIME() - XTIME0)*100./MAX(1,NSIZE_WATER)
XTIME0 = MPI_WTIME()
#endif
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! NATURAL SURFACE Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE = JTILE + 1
!
IF(GNATURE)THEN
!
  ZFRAC_TILE(:,JTILE) = XNATURE(:)
!
  CALL TREAT_SURF(JTILE,NSIZE_NATURE,NR_NATURE)
!
ENDIF 
!
#ifndef NOMPI
XTIME_NATURE = XTIME_NATURE + (MPI_WTIME() - XTIME0)*100./MAX(1,NSIZE_NATURE)
XTIME0 = MPI_WTIME()
#endif
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! URBAN Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE = JTILE + 1
!
IF(GTOWN)THEN
!
  ZFRAC_TILE(:,JTILE) = XTOWN(:)
!
  CALL TREAT_SURF(JTILE,NSIZE_TOWN,NR_TOWN)
!
ENDIF 
!
#ifndef NOMPI
XTIME_TOWN = XTIME_TOWN + (MPI_WTIME() - XTIME0)*100./MAX(1,NSIZE_TOWN)
#endif
!
! - - - - -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Grid box average fluxes/properties:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
 CALL AVERAGE_FLUX(ZFRAC_TILE,                              &
                  ZSFTH_TILE, ZSFTQ_TILE,                  &
                  ZSFTS_TILE, ZSFCO2_TILE,                 &
                  ZSFU_TILE, ZSFV_TILE,                    &
                  PSFTH, PSFTQ, PSFTS, PSFCO2,             &
                  PSFU, PSFV                               )
!
! - - - - -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Chemical Emissions:                  
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
IF ((NBEQ > 0).AND.(LCH_SURF_EMIS)) THEN
  IF (CCH_EMIS=='AGGR') THEN
    IF (NSV_AEREND < 0)  THEN
      IINDEXEND = NSV_CHSEND ! case only gas chemistry
    ELSE
      IINDEXEND = NSV_AEREND ! case aerosol + gas chemistry
    ENDIF
    INBTS=0
    DO JI=1,SIZE(TSEMISS)
      IF (SIZE(TSEMISS(JI)%NETIMES).GT.INBTS) INBTS=SIZE(TSEMISS(JI)%NETIMES)
    ENDDO
    CALL CH_EMISSION_FLUX_n(HPROGRAM,PTIME,PSFTS(:,NSV_CHSBEG:IINDEXEND),PRHOA,PTSTEP,INBTS)
  ELSE IF (CCH_EMIS=='SNAP') THEN
    CALL CH_EMISSION_SNAP_n(HPROGRAM,NSIZE_FULL,PTIME,PTSUN,KYEAR,KMONTH,KDAY,PRHOA,XLON)
    CALL CH_EMISSION_TO_ATM_n(PSFTS,PRHOA)
  END IF
END IF
!
WHERE(PSFTS(:,:)==XUNDEF)  PSFTS(:,:)=0.
! - - - - -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! CO2 Flux : adds biogenic and anthropogenic emissions
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! CO2 FLUXES  : PSFTS  in molecules/m2/s
!               PSFCO2 in kgCO2/kgair*m/s = *PRHOA kgCO2/m2/s
!               PSFCO2 in kgCO2/m2/s      = *Navogadro*1E3/Mco2(44g/mol) molecules/m2/s
!
DO JI=1,SIZE(PSV,2)
  IF(TRIM(ADJUSTL(CSV(JI)))=="CO2") THEN
    ! CO2 Flux (Antrop + biog) (molec*m2/s)
    PSFTS(:,JI) = PSFTS(:,JI) + PSFCO2(:)*PRHOA(:)*(XAVOGADRO/44.)*1E3
    ! CO2 Flux (Antrop + biog) (kgCO2/kgair*m/s)
    PSFCO2(:)   = PSFTS(:,JI)/(PRHOA(:)*(XAVOGADRO/44.)*1E3)
  END IF
END DO
!
!
! - - - - -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Radiative fluxes
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
 CALL AVERAGE_RAD(ZFRAC_TILE,                                           &
                 ZDIR_ALB_TILE, ZSCA_ALB_TILE, ZEMIS_TILE, ZTRAD_TILE, &
                 PDIR_ALB,      PSCA_ALB,      PEMIS,      PTRAD       )
!
! - - - - -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Orographic friction
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
!* adds friction due to subscale orography to momentum fluxes
!  but only over continental area
!
IF (CROUGH=="Z01D" .OR. CROUGH=="Z04D") THEN
  CALL SSO_Z0_FRICTION_n(XSEA,PUREF,PRHOA,PU,PV,ZPEW_A_COEF,ZPEW_B_COEF,PSFU,PSFV)
ELSE IF (CROUGH=="BE04") THEN
  CALL SSO_BE04_FRICTION_n(PTSTEP,XSEA,PUREF,PRHOA,PU,PV,PSFU,PSFV)
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Inline diagnostics for full surface
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
 CALL DIAG_INLINE_SURF_ATM_n(PUREF, PZREF, PPS, PRHOA, PTRAD, PEMIS, PSFU, PSFV, PSFCO2)
IF (LHOOK) CALL DR_HOOK('COUPLING_SURF_ATM_N',1,ZHOOK_HANDLE)
!
!=======================================================================================
CONTAINS
!=======================================================================================
SUBROUTINE TREAT_SURF(KTILE,KSIZE,KMASK)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)               :: KTILE
INTEGER, INTENT(IN)               :: KSIZE
INTEGER, INTENT(IN), DIMENSION(KI) :: KMASK
!
REAL, DIMENSION(KSIZE) :: ZP_TSUN     ! solar time                    (s from midnight)
REAL, DIMENSION(KSIZE) :: ZP_ZREF     ! height of T,q forcing                 (m)
REAL, DIMENSION(KSIZE) :: ZP_UREF     ! height of wind forcing                (m)
!
REAL, DIMENSION(KSIZE) :: ZP_TA       ! air temperature forcing               (K)
REAL, DIMENSION(KSIZE) :: ZP_QA       ! air specific humidity forcing         (kg/m3)
REAL, DIMENSION(KSIZE) :: ZP_RHOA     ! air density                           (kg/m3)
REAL, DIMENSION(KSIZE) :: ZP_U        ! zonal wind                            (m/s)
REAL, DIMENSION(KSIZE) :: ZP_V        ! meridian wind                         (m/s)
REAL, DIMENSION(KSIZE,ISWB) :: ZP_DIR_SW   ! direct  solar radiation (on horizontal surf.)
!                                              !                                       (W/m2)
REAL, DIMENSION(KSIZE,ISWB) :: ZP_SCA_SW   ! diffuse solar radiation (on horizontal surf.)
!                                              !                                       (W/m2)
REAL, DIMENSION(KSIZE) :: ZP_ZENITH   ! zenithal angle at t  (radian from the vertical)
REAL, DIMENSION(KSIZE) :: ZP_ZENITH2  ! zenithal angle at t+1(radian from the vertical)
REAL, DIMENSION(KSIZE) :: ZP_AZIM     ! azimuthal angle      (radian from North, clockwise)
REAL, DIMENSION(KSIZE) :: ZP_LW       ! longwave radiation (on horizontal surf.)
!                                              !                                       (W/m2)
REAL, DIMENSION(KSIZE) :: ZP_PS       ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(KSIZE) :: ZP_PA       ! pressure at forcing level             (Pa)
REAL, DIMENSION(KSIZE) :: ZP_ZS       ! atmospheric model orography           (m)
REAL, DIMENSION(KSIZE) :: ZP_CO2      ! CO2 concentration in the air          (kg/m3)
REAL, DIMENSION(KSIZE,KSV) :: ZP_SV       ! scalar concentration in the air
REAL, DIMENSION(KSIZE) :: ZP_SNOW     ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KSIZE) :: ZP_RAIN     ! liquid precipitation                  (kg/m2/s)
!
REAL, DIMENSION(KSIZE) :: ZP_SFTH     ! flux of heat                          (W/m2)
REAL, DIMENSION(KSIZE) :: ZP_SFTQ     ! flux of water vapor                   (kg/m2/s)
REAL, DIMENSION(KSIZE) :: ZP_SFU      ! zonal momentum flux                   (m/s)
REAL, DIMENSION(KSIZE) :: ZP_SFV      ! meridian momentum flux                (m/s)
REAL, DIMENSION(KSIZE) :: ZP_SFCO2    ! flux of CO2                           (kg/m2/s)
REAL, DIMENSION(KSIZE,KSV) :: ZP_SFTS     ! flux of scalar
!
REAL, DIMENSION(KSIZE) :: ZP_TRAD     ! radiative temperature                 (K)
REAL, DIMENSION(KSIZE,ISWB) :: ZP_DIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(KSIZE,ISWB) :: ZP_SCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(KSIZE) :: ZP_EMIS     ! emissivity   
!
REAL, DIMENSION(KSIZE) :: ZP_PEW_A_COEF ! implicit coefficients
REAL, DIMENSION(KSIZE) :: ZP_PEW_B_COEF ! needed if HCOUPLING='I'
REAL, DIMENSION(KSIZE) :: ZP_PET_A_COEF
REAL, DIMENSION(KSIZE) :: ZP_PEQ_A_COEF
REAL, DIMENSION(KSIZE) :: ZP_PET_B_COEF
REAL, DIMENSION(KSIZE) :: ZP_PEQ_B_COEF
INTEGER :: JJ, JK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('COUPLING_SURF_ATM_n:TREAT_SURF',0,ZHOOK_HANDLE)
!
!--------------------------------------------------------------------------------------------
!
!cdir nodep
!cdir unroll=8
DO JJ=1,KSIZE
  JI = KMASK(JJ)
  ZP_TSUN(JJ)       = PTSUN       (JI)
  ZP_ZENITH(JJ)     = PZENITH     (JI)
  ZP_ZENITH2(JJ)    = PZENITH2    (JI)
  ZP_AZIM  (JJ)     = PAZIM       (JI)
  ZP_ZREF(JJ)       = PZREF       (JI)
  ZP_UREF(JJ)       = PUREF       (JI)
  ZP_U(JJ)          = PU          (JI)
  ZP_V(JJ)          = PV          (JI)
  ZP_QA(JJ)         = PQA         (JI)
  ZP_TA(JJ)         = PTA         (JI)
  ZP_RHOA(JJ)       = PRHOA       (JI)
  ZP_CO2(JJ)        = PCO2        (JI)
  ZP_RAIN(JJ)       = PRAIN       (JI)
  ZP_SNOW(JJ)       = PSNOW       (JI)
  ZP_LW(JJ)         = PLW         (JI)
  ZP_PS(JJ)         = PPS         (JI)
  ZP_PA(JJ)         = PPA         (JI)
  ZP_ZS(JJ)         = PZS         (JI)
ENDDO
!
DO JK=1,SIZE(PSV,2)
!cdir nodep
!cdir unroll=8
  DO JJ=1,KSIZE
    JI = KMASK(JJ) 
    ZP_SV(JJ,JK)       = PSV         (JI,JK)
  ENDDO
ENDDO
!
DO JK=1,ISWB
!cdir nodep
!cdir unroll=8
  DO JJ=1,KSIZE    
    JI = KMASK(JJ)
    ZP_DIR_SW(JJ,JK)   = PDIR_SW     (JI,JK)
    ZP_SCA_SW(JJ,JK)   = PSCA_SW     (JI,JK)
  ENDDO
ENDDO  
!
!cdir nodep
!cdir unroll=8
DO JJ=1,KSIZE
  JI = KMASK(JJ)
  ZP_PEW_A_COEF(JJ) = ZPEW_A_COEF (JI)
  ZP_PEW_B_COEF(JJ) = ZPEW_B_COEF (JI)
  ZP_PET_A_COEF(JJ) = ZPET_A_COEF (JI)
  ZP_PET_B_COEF(JJ) = ZPET_B_COEF (JI)
  ZP_PEQ_A_COEF(JJ) = ZPEQ_A_COEF (JI)
  ZP_PEQ_B_COEF(JJ) = ZPEQ_B_COEF (JI)
ENDDO
!
!--------------------------------------------------------------------------------------------
!
IF (KTILE==1) THEN
  !
  CALL COUPLING_SEA_n(HPROGRAM, HCOUPLING, PTIMEC,                                           &
              PTSTEP, KYEAR, KMONTH, KDAY, PTIME,                                            &
              NSIZE_SEA, KSV, KSW,                                                           &
              ZP_TSUN, ZP_ZENITH, ZP_ZENITH2,ZP_AZIM,                                        &
              ZP_ZREF, ZP_UREF, ZP_ZS, ZP_U, ZP_V, ZP_QA, ZP_TA, ZP_RHOA, ZP_SV, ZP_CO2, HSV,&
              ZP_RAIN, ZP_SNOW, ZP_LW, ZP_DIR_SW, ZP_SCA_SW, PSW_BANDS, ZP_PS, ZP_PA,        &
              ZP_SFTQ, ZP_SFTH, ZP_SFTS, ZP_SFCO2, ZP_SFU, ZP_SFV,                           &
              ZP_TRAD, ZP_DIR_ALB, ZP_SCA_ALB, ZP_EMIS,                                      &
              ZP_PEW_A_COEF, ZP_PEW_B_COEF,                                                  &
              ZP_PET_A_COEF, ZP_PEQ_A_COEF, ZP_PET_B_COEF, ZP_PEQ_B_COEF,                    &
              'OK'                                                                           )
  !
ELSEIF (KTILE==2) THEN
  !
  CALL COUPLING_INLAND_WATER_n(HPROGRAM, HCOUPLING, PTIMEC,                                   &
               PTSTEP, KYEAR, KMONTH, KDAY, PTIME,                                            &
               NSIZE_WATER, KSV, KSW,                                                         &
               ZP_TSUN, ZP_ZENITH, ZP_ZENITH2,ZP_AZIM,                                        &
               ZP_ZREF, ZP_UREF, ZP_ZS, ZP_U, ZP_V, ZP_QA, ZP_TA, ZP_RHOA, ZP_SV, ZP_CO2, HSV,&
               ZP_RAIN, ZP_SNOW, ZP_LW, ZP_DIR_SW, ZP_SCA_SW, PSW_BANDS, ZP_PS, ZP_PA,        &
               ZP_SFTQ, ZP_SFTH, ZP_SFTS, ZP_SFCO2, ZP_SFU, ZP_SFV,                           &
               ZP_TRAD, ZP_DIR_ALB, ZP_SCA_ALB, ZP_EMIS,                                      &
               ZP_PEW_A_COEF, ZP_PEW_B_COEF,                                                  &
               ZP_PET_A_COEF, ZP_PEQ_A_COEF, ZP_PET_B_COEF, ZP_PEQ_B_COEF,                    &
               'OK'                                                                           )
  !
ELSEIF (KTILE==3) THEN
  !
  CALL COUPLING_NATURE_n(HPROGRAM, HCOUPLING, PTIMEC,                                         &
               PTSTEP, KYEAR, KMONTH, KDAY, PTIME,                                            &
               NSIZE_NATURE, KSV, KSW,                                                        &
               ZP_TSUN, ZP_ZENITH, ZP_ZENITH2,ZP_AZIM,                                        &
               ZP_ZREF, ZP_UREF, ZP_ZS, ZP_U, ZP_V, ZP_QA, ZP_TA, ZP_RHOA, ZP_SV, ZP_CO2, HSV,&
               ZP_RAIN, ZP_SNOW, ZP_LW, ZP_DIR_SW, ZP_SCA_SW, PSW_BANDS, ZP_PS, ZP_PA,        &
               ZP_SFTQ, ZP_SFTH, ZP_SFTS, ZP_SFCO2, ZP_SFU, ZP_SFV,                           &
               ZP_TRAD, ZP_DIR_ALB, ZP_SCA_ALB, ZP_EMIS,                                      &
               ZP_PEW_A_COEF, ZP_PEW_B_COEF,                                                  &
               ZP_PET_A_COEF, ZP_PEQ_A_COEF, ZP_PET_B_COEF, ZP_PEQ_B_COEF,                    &
               'OK'                                                                           )
  !
ELSEIF (KTILE==4) THEN
  !
  CALL COUPLING_TOWN_n(HPROGRAM, HCOUPLING, PTIMEC,                                           &
               PTSTEP, KYEAR, KMONTH, KDAY, PTIME,                                            &
               NSIZE_TOWN, KSV, KSW,                                                          &
               ZP_TSUN, ZP_ZENITH, ZP_AZIM,                                                   &
               ZP_ZREF, ZP_UREF, ZP_ZS, ZP_U, ZP_V, ZP_QA, ZP_TA, ZP_RHOA, ZP_SV, ZP_CO2, HSV,&
               ZP_RAIN, ZP_SNOW, ZP_LW, ZP_DIR_SW, ZP_SCA_SW, PSW_BANDS, ZP_PS, ZP_PA,        &
               ZP_SFTQ, ZP_SFTH, ZP_SFTS, ZP_SFCO2, ZP_SFU, ZP_SFV,                           &
               ZP_TRAD, ZP_DIR_ALB, ZP_SCA_ALB, ZP_EMIS,                                      &
               ZP_PEW_A_COEF, ZP_PEW_B_COEF,                                                  &
               ZP_PET_A_COEF, ZP_PEQ_A_COEF, ZP_PET_B_COEF, ZP_PEQ_B_COEF,                    &
               'OK'                                                                           )
  !
ENDIF
!
!----------------------------------------------------------------------------------------------
!
!cdir nodep
!cdir unroll=8
DO JJ=1,KSIZE
   JI=KMASK(JJ) 
   ZSFTQ_TILE      (JI,KTILE)  = ZP_SFTQ      (JJ)
   ZSFTH_TILE      (JI,KTILE)  = ZP_SFTH      (JJ)
   ZSFCO2_TILE     (JI,KTILE)  = ZP_SFCO2     (JJ)
   ZSFU_TILE       (JI,KTILE)  = ZP_SFU       (JJ)
   ZSFV_TILE       (JI,KTILE)  = ZP_SFV       (JJ)
   ZTRAD_TILE      (JI,KTILE)  = ZP_TRAD      (JJ)
   ZEMIS_TILE      (JI,KTILE)  = ZP_EMIS      (JJ)
ENDDO
!
DO JI=1,SIZE(ZP_SFTS,2)
!cdir nodep
!cdir unroll=8
  DO JJ=1,KSIZE    
    ZSFTS_TILE      (KMASK(JJ),JI,KTILE)= ZP_SFTS      (JJ,JI)
  ENDDO
ENDDO
!
DO JI=1,SIZE(ZP_DIR_ALB,2)
!cdir nodep
!cdir unroll=8
  DO JJ=1,KSIZE   
    ZDIR_ALB_TILE   (KMASK(JJ),JI,KTILE)= ZP_DIR_ALB   (JJ,JI)
    ZSCA_ALB_TILE   (KMASK(JJ),JI,KTILE)= ZP_SCA_ALB   (JJ,JI)
  ENDDO
ENDDO
!
!----------------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('COUPLING_SURF_ATM_n:TREAT_SURF',1,ZHOOK_HANDLE)
!
END SUBROUTINE TREAT_SURF
!=======================================================================================
END SUBROUTINE COUPLING_SURF_ATM_n
