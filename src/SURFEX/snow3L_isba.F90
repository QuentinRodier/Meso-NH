!     #########
SUBROUTINE SNOW3L_ISBA(HISBA, HSNOW_ISBA, HSNOWRES, OGLACIER, HIMPLICIT_WIND,                &
                         TPTIME, PTSTEP, PVEGTYPE,                                           &
                         PSNOWSWE, PSNOWHEAT, PSNOWRHO, PSNOWALB,                            &
                         PSNOWGRAN1, PSNOWGRAN2, PSNOWHIST,PSNOWAGE,                         &
                         PTG, PCG, PCT, PSOILCONDZ,                                          &
                         PPS, PTA, PSW_RAD, PQA, PVMOD, PLW_RAD, PRR, PSR,                   &
                         PRHOA, PUREF, PEXNS, PEXNA, PDIRCOSZW,                              &
                         PZREF, PZ0NAT, PZ0EFF, PZ0HNAT, PALB, PD_G1,                        &
                         PPEW_A_COEF, PPEW_B_COEF,                                           &
                         PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,                 &
                         PTHRUFAL, PGRNDFLUX, PEVAPCOR,                                      &
                         PRNSNOW, PHSNOW, PGFLUXSNOW, PHPSNOW, PLES3L, PLEL3L, PEVAP,        &
                         PUSTARSNOW,                                                         &
                         PPSN, PSRSFC, PRRSFC, PSMELTFLUX,                                   &
                         PEMISNOW, PCDSNOW, PCHSNOW, PSNOWTEMP, PSNOWLIQ, PSNOWDZ,           &
                         PSNOWHMASS, PRI, PZENITH, PLAT, PLON                                )  
!     ######################################################################################
!
!!****  *SNOW3L_ISBA*  
!!
!!    PURPOSE
!!    -------
!
!     3-Layer snow scheme option (Boone and Etchevers 1999)
!     This routine is NOT called as snow depth goes below
!     a critical threshold which is vanishingly small.
!     This routine acts as an interface between SNOW3L and ISBA.
!     
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Boone and Etchevers (1999)
!!    Belair (1995)
!!    Noilhan and Planton (1989)
!!    Noilhan and Mahfouf (1996)
!!      
!!    AUTHOR
!!    ------
!!	A. Boone           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        7/99  Boone
!!      Packing added   4/00  Masson & Boone
!!      z0h and snow    2/06  LeMoigne
!!
!!      Modified by B. Decharme  (03/2009): Consistency with Arpege permanent
!!                                          snow/ice treatment
!!      Modified by A. Boone     (04/2010): Implicit coupling with atmosphere permitted.
!!
!!      Modified by B. Decharme  (04/2010): check suspicious low temperature for ES and CROCUS
!!
!-------------------------------------------------------------------------------
!
USE MODD_CSTS,       ONLY : XTT, XPI, XDAY, XLMTT
USE MODD_SNOW_PAR,   ONLY : XRHOSMAX_ES, XSNOWDMIN, XRHOSMIN_ES
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_TYPE_DATE_SURF, ONLY: DATE_TIME
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
!
USE MODI_SNOW3L
USE MODI_SNOWCRO
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!                                      PTSTEP    = time step of the integration
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PVEGTYPE ! fraction of each vegetation
!
 CHARACTER(LEN=*),     INTENT(IN)    :: HISBA
!                                      HISBA     = FLAG to use Force-Restore or DIFfusion
!                                      soil heat and mass transfer method
!
 CHARACTER(LEN=*),     INTENT(IN)    :: HSNOW_ISBA
!                                      HSNOW_ISBA = FLAG to use SNOW3L or not 
!                                      (or default FR method)
!
 CHARACTER(LEN=*),     INTENT(IN)    :: HSNOWRES
!                                      HSNOWRES  = ISBA-SNOW3L turbulant exchange option
!                                      'DEF' = Default: Louis (ISBA: Noilhan and Mahfouf 1996)
!                                      'RIL' = Limit Richarson number under very stable 
!                                              conditions (currently testing)
!
LOGICAL, INTENT(IN)                 :: OGLACIER   ! True = Over permanent snow and ice, 
!                                                     initialise WGI=WSAT,
!                                                     Hsnow>=10m and allow 0.8<SNOALB<0.85
!
 CHARACTER(LEN=*),     INTENT(IN)  :: HIMPLICIT_WIND   ! wind implicitation option
!                                                     ! 'OLD' = direct
!                                                     ! 'NEW' = Taylor serie, order 1
!
TYPE(DATE_TIME), INTENT(IN)         :: TPTIME     ! current date and time
!
!
REAL, DIMENSION(:), INTENT(IN)      :: PTG, PD_G1, PCG, PCT, &
                                       PSOILCONDZ  
!                                      PTG       = Surface soil temperature (effective 
!                                                  temperature the of layer lying below snow)
!                                      PD_G1     = Assumed first soil layer thickness (m)
!                                                  Used to calculate ground/snow heat flux
!                                      PCG       = area-averaged soil heat capacity [(K m2)/J]
!                                      PCT       = area-averaged surface heat capacity [(K m2)/J]
!                                      PSOILCONDZ= soil thermal conductivity (W m-1 K-1)
!
REAL, DIMENSION(:), INTENT(IN)      :: PPS, PTA, PSW_RAD, PQA,                       &
                                       PVMOD, PLW_RAD, PSR, PRR  
!                                      PSW_RAD = incoming solar radiation (W/m2)
!                                      PLW_RAD = atmospheric infrared radiation (W/m2)
!                                      PRR     = rain rate [kg/(m2 s)]
!                                      PSR     = snow rate (SWE) [kg/(m2 s)]
!                                      PTA     = atmospheric temperature at level za (K)
!                                      PVMOD   = modulus of the wind parallel to the orography (m/s)
!                                      PPS     = surface pressure
!                                      PQA     = atmospheric specific humidity
!                                                at level za
!
REAL, DIMENSION(:), INTENT(IN)      :: PZREF, PUREF, PEXNS, PEXNA, PDIRCOSZW, PRHOA, PZ0NAT, PZ0EFF, PZ0HNAT, PALB
!                                      PZ0EFF    = roughness length for momentum 
!                                      PZ0NAT    = grid box average roughness length
!                                      PZ0HNAT   = grid box average roughness length
!                                      PZREF     = reference height of the first
!                                                  atmospheric level
!                                      PUREF     = reference height of the wind
!                                      PRHOA     = air density
!                                      PEXNS     = Exner function at surface
!                                      PEXNA     = Exner function at lowest atmos level
!                                      PDIRCOSZW = Cosinus of the angle between the 
!                                                  normal to the surface and the vertical
!                                      PALB      = soil/vegetation albedo
!
REAL, DIMENSION(:), INTENT(IN)      :: PPSN
!                                      PPSN  = Snow cover fraction (total) 
!
REAL, DIMENSION(:), INTENT(IN)      :: PPEW_A_COEF, PPEW_B_COEF,                   &
                                       PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF,      &
                                       PPEQ_B_COEF  
!                                      PPEW_A_COEF = wind coefficient
!                                      PPEW_B_COEF = wind coefficient
!                                      PPET_A_COEF = A-air temperature coefficient
!                                      PPET_B_COEF = B-air temperature coefficient
!                                      PPEQ_A_COEF = A-air specific humidity coefficient
!                                      PPEQ_B_COEF = B-air specific humidity coefficient                         !
REAL, DIMENSION(:), INTENT(INOUT)   :: PSNOWALB
!                                      PSNOWALB = Prognostic surface snow albedo
!                                                 (does not include anything but
!                                                 the actual snow cover)
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWHEAT, PSNOWRHO, PSNOWSWE
!                                      PSNOWHEAT = Snow layer(s) heat content (J/m3)
!                                      PSNOWRHO  = Snow layer(s) averaged density (kg/m3)
!                                      PSNOWSWE  = Snow layer(s) liquid Water Equivalent (SWE:kg m-2)
!
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWGRAN1, PSNOWGRAN2, PSNOWHIST
!                                      PSNOWGRAN1 = Snow layer(s) grain parameter 1
!                                      PSNOWGRAN2 = Snow layer(s) grain parameter 2
!                                      PSNOWHIST  = Snow layer(s) grain historical parameter
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWAGE  ! Snow grain age
!
!
REAL, DIMENSION(:), INTENT(OUT)     :: PTHRUFAL, PGRNDFLUX, PEVAPCOR, PSNOWHMASS
!                                      PTHRUFAL  = rate that liquid water leaves snow pack: 
!                                                  paritioned into soil infiltration/runoff 
!                                                  by ISBA [kg/(m2 s)]
!                                      PGRNDFLUX = soil/snow interface heat flux (W/m2)
!                                      PEVAPCOR  = evaporation/sublimation correction term:
!                                                  extract any evaporation exceeding the
!                                                  actual snow cover (as snow vanishes)
!                                                  and apply it as a surface soil water
!                                                  sink. [kg/(m2 s)]
!                                      PSNOWHMASS = heat content change due to mass
!                                                   changes in snowpack (J/m2): for budget
!                                                   calculations only.
!
REAL, DIMENSION(:), INTENT(OUT)     :: PRNSNOW, PHSNOW, PGFLUXSNOW, PLES3L, PLEL3L,     &
                                       PHPSNOW, PUSTARSNOW, PEMISNOW, PCDSNOW,          &
                                       PCHSNOW, PEVAP  
!                                      PLES3L      = evaporation heat flux from snow (W/m2)
!                                      PLEL3L      = sublimation (W/m2)
!                                      PHPSNOW     = heat release from rainfall (W/m2)
!                                      PRNSNOW     = net radiative flux from snow (W/m2)
!                                      PHSNOW      = sensible heat flux from snow (W/m2)
!                                      PGFLUXSNOW  = net heat flux from snow (W/m2)
!                                      PUSTARSNOW  = friction velocity over snow (m/s)
!                                      PEMISNOW    = snow surface emissivity
!                                      PCDSNOW     = drag coefficient for momentum over snow
!                                      PCHSNOW     = drag coefficient for heat over snow
!                                      PEVAP       = total evaporative flux from snow (kg/m2/s)
!
REAL, DIMENSION(:), INTENT(OUT)     :: PSRSFC, PRRSFC, PSMELTFLUX
!                                      PSRSFC = snow rate on soil/veg surface when SNOW3L in use
!                                      PRRSFC = rain rate on soil/veg surface when SNOW3L in use
!                                      PSMELTFLUX = heat flux from soil/vegetation surface
!                                               to melt thin snow cover when it vanishes (W m-2)
!
REAL, DIMENSION(:,:), INTENT(OUT)   :: PSNOWLIQ, PSNOWTEMP, PSNOWDZ
!                                      PSNOWLIQ  = Snow layer(s) liquid water content (m)
!                                      PSNOWTEMP = Snow layer(s) temperature (m)
!                                      PSNOWDZ   = Snow layer(s) thickness (m)
!
REAL, DIMENSION(:), INTENT(OUT)     :: PRI
!                                      PRI = Ridcharson number
!
! ajout_EB pour prendre en compte angle zenithal du soleil dans LRAD
! puis plus tard dans LALB
REAL, DIMENSION(:), INTENT(IN)      :: PZENITH    ! solar zenith angle
REAL, DIMENSION(:), INTENT(IN)      :: PLAT
REAL, DIMENSION(:), INTENT(IN)      :: PLON
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER                     :: ZCHECK_TEMP = 100.0 
!                                      Limit to check suspicious low temperature (K)
!
INTEGER                             :: JWRK, JJ ! Loop control
!
INTEGER                             :: INLVLS   ! maximum number of snow layers
!
REAL, DIMENSION(SIZE(PTA))          :: ZRRSNOW, ZSOILCOND, ZSNOW, ZSNOWFALL, &
                                       ZSNOWABLAT_DELTA, ZSNOWSWE_1D, ZSNOWD  
!                                      ZSOILCOND    = soil thermal conductivity [W/(m K)]
!                                      ZRRSNOW      = rain rate over snow [kg/(m2 s)]
!                                      ZSNOW        = snow depth (m) 
!                                      ZSNOWFALL    = minimum equivalent snow depth
!                                                     for snow falling during the
!                                                     current time step (m)
!                                      ZSNOWABLAT_DELTA = FLAG =1 if snow ablates completely
!                                                     during current time step, else=0
!                                      ZSNOWSWE_1D  = TOTAL snowpack SWE (kg m-2)
!                                      ZSNOWD       = snow depth
!
!*      0.3    declarations of packed  variables
!
INTEGER                            :: ISIZE_SNOW ! number of points where computations are done
INTEGER, DIMENSION(SIZE(PTA))      :: NMASK      ! indices correspondance between arrays
REAL(KIND=JPRB) :: ZHOOK_HANDLE
! - - ---------------------------------------------------
!
!*       0.     Initialize variables:
!               ---------------------
!
IF (LHOOK) CALL DR_HOOK('SNOW3L_ISBA',0,ZHOOK_HANDLE)
PGRNDFLUX(:)   = 0.0
PTHRUFAL(:)    = 0.0
PEVAPCOR(:)    = 0.0
PLES3L(:)      = 0.0
PLEL3L(:)      = 0.0
PEVAP(:)       = 0.0
PRNSNOW(:)     = 0.0
PHSNOW(:)      = 0.0
PGFLUXSNOW(:)  = 0.0
PHPSNOW(:)     = 0.0
PSNOWHMASS(:)  = 0.0
PUSTARSNOW(:)  = 0.0
PSRSFC(:)      = PSR(:)         ! these are snow and rain rates passed to ISBA,
PRRSFC(:)      = PRR(:)         ! so initialize here if SNOW3L not used:
PEMISNOW(:)    = 1.0
PSMELTFLUX(:)  = 0.0
PCDSNOW(:)     = 0.0
PRI(:)         = XUNDEF
!
ZSNOW(:)       = 0.0
ZSNOWSWE_1D(:) = 0.0
ZSOILCOND(:)   = 0.0
ZRRSNOW(:)     = 0.0
ZSNOWFALL(:)   = 0.0
ZSNOWABLAT_DELTA(:) = 0.0
PSNOWTEMP(:,:) = XTT
PSNOWLIQ(:,:)  = 0.0
PSNOWDZ(:,:)   = 0.0
!
INLVLS         = SIZE(PSNOWSWE(:,:),2)                         
!
!
! Use ISBA-SNOW3L or NOT: NOTE that if explicit soil diffusion method in use,
! then *must* use explicit snow model:
!
IF (HSNOW_ISBA=='3-L' .OR. HISBA == 'DIF' .OR. HSNOW_ISBA == 'CRO') THEN
!
! - Snow and rain falling onto the 3-L grid space:
!
  PSRSFC(:)=0.0
  ZSNOW(:)=0.
  ZSNOWSWE_1D(:)=0.
  DO JJ=1,SIZE(PSR)

    ZRRSNOW(JJ)        = PPSN(JJ)*PRR(JJ)
    PRRSFC(JJ)         = PRR(JJ) - ZRRSNOW(JJ)
!
    ZSNOWFALL(JJ)      = PSR(JJ)*PTSTEP/XRHOSMAX_ES    ! maximum possible snowfall depth (m)
!
  ENDDO
!
! Calculate preliminary snow depth (m)

  DO JWRK=1,SIZE(PSNOWSWE,2)
    DO JJ=1,SIZE(PSNOWSWE,1)
      ZSNOW(JJ)           = ZSNOW(JJ)       + PSNOWSWE(JJ,JWRK)/PSNOWRHO(JJ,JWRK)
      ZSNOWSWE_1D(JJ)     = ZSNOWSWE_1D(JJ) + PSNOWSWE(JJ,JWRK)
    END DO
  ENDDO

  IF(HISBA == 'DIF')THEN
    ZSOILCOND(:)   = PSOILCONDZ(:)
  ELSE
!
! - Soil thermal conductivity
!   is implicit in Force-Restore soil method, so it
!   must be backed-out of surface thermal coefficients
!   (Etchevers and Martin 1997):
!
    ZSOILCOND(:)    = 4.*XPI/( PCG(:)*PCG(:)*XDAY/(PD_G1(:)*PCT(:)) )

  ENDIF
!
! ===============================================================
! === Packing: Only call snow model when there is snow on the surface
!              exceeding a minimum threshold OR if the equivalent
!              snow depth falling during the current time step exceeds 
!              this limit.
!
! counts the number of points where the computations will be made
!
!
  ISIZE_SNOW = 0
  NMASK(:) = 0
  !
  DO JJ=1,SIZE(ZSNOW)
    IF (ZSNOW(JJ) >= XSNOWDMIN .OR. ZSNOWFALL(JJ) >= XSNOWDMIN) THEN
      ISIZE_SNOW = ISIZE_SNOW + 1
      NMASK(ISIZE_SNOW) = JJ
    ENDIF
  ENDDO
  !
  IF (ISIZE_SNOW>0) CALL CALL_MODEL(ISIZE_SNOW,INLVLS,NMASK)
  !
! ===============================================================
! - Remove trace amounts of snow and reinitialize snow prognostic variables
!   if snow cover is ablated:
!
  ZSNOWABLAT_DELTA(:)    = 0.0
  ZSNOWD(:) = 0.
  DO JWRK=1,SIZE(PSNOWSWE,2)
    DO JJ=1,SIZE(PSNOWSWE,1)
      ZSNOWD(JJ) = ZSNOWD(JJ) + PSNOWSWE(JJ,JWRK)/PSNOWRHO(JJ,JWRK)
    ENDDO
  END DO
  WHERE(ZSNOWD(:) < XSNOWDMIN*1.1)
    PTHRUFAL(:)         = ZSNOWSWE_1D(:)/PTSTEP + PSR(:) ! kg m-2 s-1   Conserve mass
    PSMELTFLUX(:)       = -PTHRUFAL(:)*XLMTT             ! W m-2        Conserve Energy
    PLEL3L(:)           = 0.0
    PLES3L(:)           = 0.0
    PEVAP(:)            = 0.0
    PSRSFC(:)           = 0.0
    PRRSFC(:)           = PRR(:)
    ZRRSNOW(:)          = 0.0
    ZSNOWABLAT_DELTA(:) = 1.0
    PSNOWALB(:)         = XUNDEF
  END WHERE

  DO JWRK=1,INLVLS
    DO JJ=1,SIZE(PSNOWSWE,1)
      PSNOWSWE(JJ,JWRK)  = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWSWE(JJ,JWRK)
      PSNOWHEAT(JJ,JWRK) = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWHEAT(JJ,JWRK)
      PSNOWRHO(JJ,JWRK)  = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWRHO(JJ,JWRK)  + &
                              ZSNOWABLAT_DELTA(JJ)*XRHOSMIN_ES  
      PSNOWTEMP(JJ,JWRK) = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWTEMP(JJ,JWRK) + &
                              ZSNOWABLAT_DELTA(JJ)*XTT  
      PSNOWLIQ(JJ,JWRK)  = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWLIQ(JJ,JWRK)        
      PSNOWDZ(JJ,JWRK)   = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWDZ(JJ,JWRK)
    ENDDO
  ENDDO  
  
  IF (HSNOW_ISBA=='CRO') THEN
    DO JWRK=1,INLVLS
      DO JJ=1,SIZE(PSNOWGRAN1,1)
        PSNOWGRAN1(JJ,JWRK)  = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWGRAN1(JJ,JWRK) 
        PSNOWGRAN2(JJ,JWRK)  = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWGRAN2(JJ,JWRK)
        PSNOWHIST(JJ,JWRK)   = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWHIST(JJ,JWRK)
        PSNOWAGE (JJ,JWRK)   = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWAGE (JJ,JWRK)
      ENDDO
    ENDDO
  ENDIF 
!
! ===============================================================
! check suspicious low temperature
!
  DO JWRK=1,INLVLS
     DO JJ=1,SIZE(PSNOWSWE,1)
        IF(PSNOWSWE(JJ,JWRK)>0.0.AND.PSNOWTEMP(JJ,JWRK)<ZCHECK_TEMP)THEN
           write(*,*) 'Suspicious low temperature :',JJ,JWRK,PSNOWTEMP(JJ,JWRK)
           write(*,*) 'XLAT=',PLAT(JJ),'XLON=',PLON(JJ)
           write(*,*) PSNOWSWE (JJ,1:INLVLS)
           write(*,*) PSNOWDZ  (JJ,1:INLVLS)
           write(*,*) PSNOWRHO (JJ,1:INLVLS)
           write(*,*) PSNOWTEMP(JJ,1:INLVLS)
           CALL ABOR1_SFX('SNOW3L_ISBA: erreur tempe snow')                
        ENDIF
     ENDDO
  ENDDO
!
! ===============================================================
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('SNOW3L_ISBA',1,ZHOOK_HANDLE)
!
CONTAINS
!
SUBROUTINE CALL_MODEL(KSIZE1,KSIZE2,KMASK)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KSIZE1
INTEGER, INTENT(IN) :: KSIZE2
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
!
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWSWE
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWDZ
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWRHO
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWHEAT
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWTEMP
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWLIQ
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWGRAN1
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWGRAN2
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWHIST
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWAGE
REAL, DIMENSION(KSIZE1)        :: ZP_SNOWALB
REAL, DIMENSION(KSIZE1)        :: ZP_PS
REAL, DIMENSION(KSIZE1)        :: ZP_SRSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_RRSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_PSN3L
REAL, DIMENSION(KSIZE1)        :: ZP_TA
REAL, DIMENSION(KSIZE1)        :: ZP_TG
REAL, DIMENSION(KSIZE1)        :: ZP_SW_RAD
REAL, DIMENSION(KSIZE1)        :: ZP_QA
REAL, DIMENSION(KSIZE1)        :: ZP_VMOD
REAL, DIMENSION(KSIZE1)        :: ZP_LW_RAD
REAL, DIMENSION(KSIZE1)        :: ZP_RHOA
REAL, DIMENSION(KSIZE1)        :: ZP_UREF
REAL, DIMENSION(KSIZE1)        :: ZP_EXNS
REAL, DIMENSION(KSIZE1)        :: ZP_EXNA
REAL, DIMENSION(KSIZE1)        :: ZP_DIRCOSZW
REAL, DIMENSION(KSIZE1)        :: ZP_ZREF
REAL, DIMENSION(KSIZE1)        :: ZP_Z0NAT
REAL, DIMENSION(KSIZE1)        :: ZP_Z0HNAT
REAL, DIMENSION(KSIZE1)        :: ZP_Z0EFF
REAL, DIMENSION(KSIZE1)        :: ZP_ALB
REAL, DIMENSION(KSIZE1)        :: ZP_SOILCOND
REAL, DIMENSION(KSIZE1)        :: ZP_D_G
REAL, DIMENSION(KSIZE1)        :: ZP_THRUFAL
REAL, DIMENSION(KSIZE1)        :: ZP_GRNDFLUX
REAL, DIMENSION(KSIZE1)        :: ZP_EVAPCOR
REAL, DIMENSION(KSIZE1)        :: ZP_RNSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_HSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_GFLUXSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_HPSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_LES3L
REAL, DIMENSION(KSIZE1)        :: ZP_LEL3L
REAL, DIMENSION(KSIZE1)        :: ZP_EVAP
REAL, DIMENSION(KSIZE1)        :: ZP_RI
REAL, DIMENSION(KSIZE1)        :: ZP_EMISNOW
REAL, DIMENSION(KSIZE1)        :: ZP_CDSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_USTARSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_CHSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_SNOWHMASS
REAL, DIMENSION(KSIZE1)        :: ZP_VEGTYPE
REAL, DIMENSION(KSIZE1)        :: ZP_PEW_A_COEF
REAL, DIMENSION(KSIZE1)        :: ZP_PEW_B_COEF
REAL, DIMENSION(KSIZE1)        :: ZP_PET_A_COEF
REAL, DIMENSION(KSIZE1)        :: ZP_PET_B_COEF
REAL, DIMENSION(KSIZE1)        :: ZP_PEQ_A_COEF
REAL, DIMENSION(KSIZE1)        :: ZP_PEQ_B_COEF
REAL, DIMENSION(KSIZE1)        :: ZP_ZENITH
REAL, DIMENSION(KSIZE1)        :: ZP_LAT,ZP_LON
!
INTEGER :: JWRK, JJ, JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SNOW3L_ISBA:CALL_MODEL',0,ZHOOK_HANDLE)
!
! pack the variables
!
DO JWRK=1,KSIZE2
  DO JJ=1,KSIZE1
    JI = KMASK(JJ)
    ZP_SNOWSWE (JJ,JWRK) = PSNOWSWE (JI,JWRK)
    ZP_SNOWRHO (JJ,JWRK) = PSNOWRHO (JI,JWRK)
    ZP_SNOWHEAT(JJ,JWRK) = PSNOWHEAT(JI,JWRK)
    ZP_SNOWTEMP(JJ,JWRK) = PSNOWTEMP(JI,JWRK)
    ZP_SNOWLIQ (JJ,JWRK) = PSNOWLIQ (JI,JWRK)
    ZP_SNOWDZ  (JJ,JWRK) = PSNOWDZ  (JI,JWRK)
  ENDDO
ENDDO 
!
IF (HSNOW_ISBA=='CRO') THEN
  DO JWRK=1,KSIZE2
    DO JJ=1,KSIZE1
      JI = KMASK(JJ)
      ZP_SNOWGRAN1(JJ,JWRK) = PSNOWGRAN1 (JI,JWRK)
      ZP_SNOWGRAN2(JJ,JWRK) = PSNOWGRAN2 (JI,JWRK)
      ZP_SNOWHIST (JJ,JWRK) = PSNOWHIST  (JI,JWRK)
      ZP_SNOWAGE  (JJ,JWRK) = PSNOWAGE   (JI,JWRK)
    ENDDO
  ENDDO
ELSE
  DO JWRK=1,KSIZE2
    DO JJ=1,KSIZE1
      ZP_SNOWGRAN1(JJ,JWRK) = XUNDEF
      ZP_SNOWGRAN2(JJ,JWRK) = XUNDEF
      ZP_SNOWHIST (JJ,JWRK) = XUNDEF
      ZP_SNOWAGE  (JJ,JWRK) = XUNDEF
    ENDDO
  ENDDO
ENDIF
!  
DO JJ=1,KSIZE1
  JI = KMASK(JJ)
  ZP_SNOWALB (JJ) = PSNOWALB (JI)    
  ZP_PS      (JJ) = PPS      (JI)
  ZP_SRSNOW  (JJ) = PSR      (JI)
  ZP_RRSNOW  (JJ) = ZRRSNOW  (JI)
  ZP_PSN3L   (JJ) = PPSN     (JI)
  ZP_TA      (JJ) = PTA      (JI)
  ZP_TG      (JJ) = PTG      (JI)
  ZP_SW_RAD  (JJ) = PSW_RAD  (JI)
  ZP_QA      (JJ) = PQA      (JI)
  ZP_VMOD    (JJ) = PVMOD    (JI)
  ZP_LW_RAD  (JJ) = PLW_RAD  (JI)
  ZP_RHOA    (JJ) = PRHOA    (JI)
  ZP_UREF    (JJ) = PUREF    (JI)
  ZP_EXNS    (JJ) = PEXNS    (JI)
  ZP_EXNA    (JJ) = PEXNA    (JI)
  ZP_DIRCOSZW(JJ) = PDIRCOSZW(JI)
  ZP_ZREF    (JJ) = PZREF    (JI)
  ZP_Z0NAT   (JJ) = PZ0NAT   (JI)
  ZP_Z0HNAT  (JJ) = PZ0HNAT  (JI)
  ZP_Z0EFF   (JJ) = PZ0EFF   (JI)
  ZP_ALB     (JJ) = PALB     (JI)
  ZP_SOILCOND(JJ) = ZSOILCOND(JI)
  ZP_D_G     (JJ) = PD_G1    (JI)
  !  
  ZP_PEW_A_COEF(JJ) = PPEW_A_COEF(JI)
  ZP_PEW_B_COEF(JJ) = PPEW_B_COEF(JI)
  ZP_PET_A_COEF(JJ) = PPET_A_COEF(JI)
  ZP_PEQ_A_COEF(JJ) = PPEQ_A_COEF(JI)      
  ZP_PET_B_COEF(JJ) = PPET_B_COEF(JI)
  ZP_PEQ_B_COEF(JJ) = PPEQ_B_COEF(JI)
  !
  ZP_LAT  (JJ) = PLAT(JI)
  ZP_LON  (JJ) = PLON(JI)
  ZP_ZENITH(JJ) = PZENITH  (JI)
ENDDO
!
DO JJ=1,KSIZE1
  JI = KMASK(JJ)
  ZP_VEGTYPE (JJ) = PVEGTYPE (JI,NVT_SNOW)
ENDDO
!
! ===============================================================
! conversion of snow heat from J/m3 into J/m2
WHERE(ZP_SNOWSWE(:,:)>0.) &
  ZP_SNOWHEAT(:,:) = ZP_SNOWHEAT(:,:) / ZP_SNOWRHO (:,:) * ZP_SNOWSWE (:,:)  
! ===============================================================
!
! Call ISBA-SNOW3L model:  
!  
IF (HSNOW_ISBA=='CRO') THEN 

  CALL SNOWCRO(HSNOWRES, TPTIME, OGLACIER, HIMPLICIT_WIND,                 &
             ZP_PEW_A_COEF, ZP_PEW_B_COEF,                                 &
             ZP_PET_A_COEF, ZP_PEQ_A_COEF, ZP_PET_B_COEF, ZP_PEQ_B_COEF,   &
             ZP_SNOWSWE,ZP_SNOWRHO, ZP_SNOWHEAT, ZP_SNOWALB,               &
             ZP_SNOWGRAN1, ZP_SNOWGRAN2, ZP_SNOWHIST, ZP_SNOWAGE, PTSTEP,  &
             ZP_PS, ZP_SRSNOW, ZP_RRSNOW ,ZP_PSN3L, ZP_TA, ZP_TG,          &
             ZP_SW_RAD, ZP_QA, ZP_VMOD, ZP_LW_RAD, ZP_RHOA, ZP_UREF,       &
             ZP_EXNS, ZP_EXNA, ZP_DIRCOSZW, ZP_ZREF, ZP_Z0NAT, ZP_Z0EFF,   &
             ZP_Z0HNAT, ZP_ALB, ZP_SOILCOND, ZP_D_G,ZP_SNOWLIQ,            &
             ZP_SNOWTEMP, ZP_SNOWDZ, ZP_THRUFAL, ZP_GRNDFLUX, ZP_EVAPCOR,  &
             ZP_RNSNOW, ZP_HSNOW, ZP_GFLUXSNOW, ZP_HPSNOW, ZP_LES3L,       &
             ZP_LEL3L, ZP_EVAP, ZP_RI, ZP_EMISNOW, ZP_CDSNOW, ZP_USTARSNOW,&
             ZP_CHSNOW, ZP_SNOWHMASS, ZP_VEGTYPE, ZP_ZENITH, ZP_LAT, ZP_LON)    

ELSE 

  CALL SNOW3L(HSNOWRES, TPTIME, OGLACIER, HIMPLICIT_WIND,                  &
             ZP_PEW_A_COEF, ZP_PEW_B_COEF,                                 &
             ZP_PET_A_COEF, ZP_PEQ_A_COEF,ZP_PET_B_COEF, ZP_PEQ_B_COEF,    &
             ZP_SNOWSWE, ZP_SNOWRHO, ZP_SNOWHEAT, ZP_SNOWALB,              &
             ZP_SNOWGRAN1, ZP_SNOWGRAN2, ZP_SNOWHIST, ZP_SNOWAGE, PTSTEP,  &
             ZP_PS, ZP_SRSNOW, ZP_RRSNOW, ZP_PSN3L, ZP_TA,ZP_TG,           &
             ZP_SW_RAD, ZP_QA, ZP_VMOD, ZP_LW_RAD, ZP_RHOA, ZP_UREF,       &
             ZP_EXNS, ZP_EXNA, ZP_DIRCOSZW, ZP_ZREF, ZP_Z0NAT, ZP_Z0EFF,   &
             ZP_Z0HNAT, ZP_ALB, ZP_SOILCOND, ZP_D_G, ZP_SNOWLIQ,           &
             ZP_SNOWTEMP, ZP_SNOWDZ, ZP_THRUFAL, ZP_GRNDFLUX ,ZP_EVAPCOR,  &
             ZP_RNSNOW, ZP_HSNOW, ZP_GFLUXSNOW, ZP_HPSNOW, ZP_LES3L,       &
             ZP_LEL3L, ZP_EVAP, ZP_RI, ZP_EMISNOW, ZP_CDSNOW, ZP_USTARSNOW,&
             ZP_CHSNOW, ZP_SNOWHMASS, ZP_VEGTYPE, ZP_ZENITH, ZP_LAT, ZP_LON)  

ENDIF
!
! ===============================================================
! conversion of snow heat from J/m2 into J/m3
WHERE(ZP_SNOWSWE(:,:)>0.) &
  ZP_SNOWHEAT(:,:) = ZP_SNOWHEAT(:,:)* ZP_SNOWRHO (:,:)  / ZP_SNOWSWE (:,:)  
! ===============================================================
! === Packing:
!
! unpack variables
!
DO JWRK=1,KSIZE2
  DO JJ=1,KSIZE1
    JI = KMASK(JJ)
    PSNOWSWE  (JI,JWRK) = ZP_SNOWSWE  (JJ,JWRK)
    PSNOWRHO  (JI,JWRK) = ZP_SNOWRHO  (JJ,JWRK)
    PSNOWHEAT (JI,JWRK) = ZP_SNOWHEAT (JJ,JWRK)
    PSNOWTEMP (JI,JWRK) = ZP_SNOWTEMP (JJ,JWRK)
    PSNOWLIQ  (JI,JWRK) = ZP_SNOWLIQ  (JJ,JWRK)
    PSNOWDZ   (JI,JWRK) = ZP_SNOWDZ   (JJ,JWRK)
  ENDDO
ENDDO
!
IF (HSNOW_ISBA=='CRO') THEN
  DO JWRK=1,KSIZE2
    DO JJ=1,KSIZE1
      JI = KMASK(JJ)
      PSNOWGRAN1(JI,JWRK) = ZP_SNOWGRAN1(JJ,JWRK)
      PSNOWGRAN2(JI,JWRK) = ZP_SNOWGRAN2(JJ,JWRK)
      PSNOWHIST (JI,JWRK) = ZP_SNOWHIST (JJ,JWRK)
      PSNOWAGE  (JI,JWRK) = ZP_SNOWAGE  (JJ,JWRK)
    ENDDO
  ENDDO
ENDIF
!
DO JJ=1,KSIZE1
  JI = KMASK(JJ)
  PSNOWALB  (JI)   = ZP_SNOWALB  (JJ)
  PTHRUFAL  (JI)   = ZP_THRUFAL  (JJ)
  PGRNDFLUX (JI)   = ZP_GRNDFLUX (JJ)
  PEVAPCOR  (JI)   = ZP_EVAPCOR  (JJ)
  PRNSNOW   (JI)   = ZP_RNSNOW   (JJ)
  PHSNOW    (JI)   = ZP_HSNOW    (JJ)
  PGFLUXSNOW(JI)   = ZP_GFLUXSNOW(JJ)
  PHPSNOW   (JI)   = ZP_HPSNOW   (JJ)
  PLES3L    (JI)   = ZP_LES3L    (JJ)
  PLEL3L    (JI)   = ZP_LEL3L    (JJ)
  PEVAP     (JI)   = ZP_EVAP     (JJ)
  PRI       (JI)   = ZP_RI       (JJ)
  PEMISNOW  (JI)   = ZP_EMISNOW  (JJ)
  PCDSNOW   (JI)   = ZP_CDSNOW   (JJ)
  PUSTARSNOW(JI)   = ZP_USTARSNOW(JJ)
  PCHSNOW   (JI)   = ZP_CHSNOW   (JJ)
  PSNOWHMASS(JI)   = ZP_SNOWHMASS(JJ)
ENDDO
!
IF (LHOOK) CALL DR_HOOK('SNOW3L_ISBA:CALL_MODEL',1,ZHOOK_HANDLE)
!
END SUBROUTINE CALL_MODEL
!
END SUBROUTINE SNOW3L_ISBA
