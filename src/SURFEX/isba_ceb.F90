!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#########
SUBROUTINE ISBA_CEB(HISBA, HSNOW_ISBA, HCPSURF, OFLOOD, OTEMP_ARP, HIMPLICIT_WIND, &
                    PTSTEP, PSODELX, PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF,        &
                    PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF, PSNOWALB,               &
                    PSW_RAD, PLW_RAD, PWG, PWGI, PEXNS, PEXNA, PTA, PVMOD,         &
                    PQA, PRR, PSR, PPS, PRS, PVEG, PZ0_WITH_SNOW, PZ0EFF,          &
                    PZ0H_WITH_SNOW, PWFC, PWSAT, PPSN, PPSNG, PPSNV, PZREF,        &
                    PUREF, PDIRCOSZW, PF5, PFFG, PFFV, PFF, PFFG_NOSNOW,           &
                    PFFV_NOSNOW, PWR, PRHOA, PEMIS, PALB, PCT, PCS, PCG,           &
                    PD_G, PDZG, PDZDIF, PSOILCONDZ, PSOILHCAPZ,  PFROZEN1,         &
                    PTDEEP_A, PTDEEP_B, PGAMMAT,  PPSNV_A, PSNOWFREE_ALB_VEG,      &
                    PSNOWFREE_ALB_SOIL, PGRNDFLUX, PFLSN_COR,                      &
                    PSNOW_THRUFAL, PFFROZEN, PFALB, PFEMIS, PSNOWSWE, PSRSFC,      &
                    PTG, PRESA, PLVTT, PLSTT, PCPS, PDELTA, PCH, PCD, PCDN,        &
                    PRI, PHUG, PHUGI, PHV, PHU, PQS, PALBT, PEMIST, PDEEP_FLUX,    &
                    PRN, PH, PLE, PLEG, PLEGI, PLEV, PLES, PLER, PLETR, PEVAP,     &
                    PGFLUX, PMELTADV, PMELT, PRESTORE, PUSTAR, PLE_FLOOD,          &
                    PLEI_FLOOD, PSNOWTEMP, PAC_AGG, PHU_AGG                        )
!     ##########################################################################
!
!
!!****  *ISBA_CEB*  
!!
!!    PURPOSE
!!    -------
!       Monitor for the calculation of the surface composit energy budget
!!      call of drag, e_budget, and isba_fluxes
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/03/95 
!!
!!      (B. Decharme)   03/16 Bug : limitation of Er for Interception reservoir
!!                                  PTSTEP insted of ZTSTEP in drag.F90
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODD_SURF_ATM,   ONLY : LCPL_ARP
!
USE MODI_DRAG
USE MODI_E_BUDGET
USE MODI_ISBA_FLUXES
!
USE MODE_THERMOS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!              -------------------------
!
! in
!
!
 CHARACTER(LEN=*),     INTENT(IN) :: HISBA      ! type of ISBA version:
!                                              ! '2-L' (default)
!                                              ! '3-L'
!                                              ! 'DIF'
 CHARACTER(LEN=*),     INTENT(IN) :: HSNOW_ISBA ! 'DEF' = Default F-R snow scheme
!                                              !         (Douville et al. 1995)
!                                              ! '3-L' = 3-L snow scheme (option)
!                                              !         (Boone and Etchevers 2000)
 CHARACTER(LEN=*),     INTENT(IN) :: HCPSURF    ! option for specific heat Cp:
!                                              ! 'DRY' = dry Cp
!                                              ! 'HUM' = Cp as a function of qs
!
LOGICAL, INTENT(IN)              :: OFLOOD     ! Activation of the flooding scheme
LOGICAL, INTENT(IN)              :: OTEMP_ARP  ! True  = time-varying force-restore soil temperature (as in ARPEGE)
                                               ! False = No time-varying force-restore soil temperature (Default)
!
 CHARACTER(LEN=*),    INTENT(IN) :: HIMPLICIT_WIND   ! wind implicitation option
!                                                    ! 'OLD' = direct
!                                                    ! 'NEW' = Taylor serie, order 1
!
REAL,                 INTENT(IN) :: PTSTEP    ! timestep of the integration
!
REAL, DIMENSION(:),   INTENT(IN) ::  PSODELX  ! Pulsation for each layer (Only used if LTEMP_ARP=True)
!
REAL, DIMENSION(:),  INTENT(IN)  :: PPEW_A_COEF, PPEW_B_COEF,                   &
                                    PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF,      &
                                    PPEQ_B_COEF  
!                                  PPEW_A_COEF = A-wind coefficient (m2s/kg)
!                                  PPEW_B_COEF = B-wind coefficient (m/s)
!                                  PPET_A_COEF = A-air temperature coefficient
!                                  PPET_B_COEF = B-air temperature coefficient
!                                  PPEQ_A_COEF = A-air specific humidity coefficient
!                                  PPEQ_B_COEF = B-air specific humidity coefficient
!
REAL, DIMENSION(:), INTENT(IN)   :: PSNOWALB
!                                     prognostic variables at time 't-dt'
!                                     PSNOWALB = albedo of the snow
!
REAL, DIMENSION(:,:), INTENT(IN) :: PWG, PWGI, PWFC, PWSAT
!                                     PWG     = near-surface volumetric water content
!                                     PWGI    = near-surface frozen volumetric water content
!                                     PWFC = field capacity volumetric water content
!                                     PWSAT = volumetric water content at saturation
!
REAL, DIMENSION(:), INTENT(IN)   :: PSW_RAD, PLW_RAD
!                                     PSW_RAD = incoming solar radiation
!                                     PLW_RAD = atmospheric infrared radiation

REAL, DIMENSION(:), INTENT(IN)   :: PEXNA, PEXNS, PTA, PVMOD, PQA, PRR, PSR, PPS
!                                     PEXNA= Exner function near surface atmospheric variables
!                                     PEXNS   = Exner function at the surface
!                                     PTA = 2m temperature
!                                     PVMOD = module of the horizontal wind
!                                             NOTE it should be input as >= 1. (m)
!                                     PQA = specific humidity
!                                     PPS = surface pressure
!                                     PRR = rain rate    
!                                     PSR = snow rate             
!
REAL, DIMENSION(:), INTENT(IN)   :: PRS, PVEG
!                                     PRS = stomatal resistance
!                                     PVEG = vegetation fraction
REAL, DIMENSION(:), INTENT(IN)   :: PPSN, PPSNG, PPSNV
!                                     PPSN = grid fraction covered by snow
!                                     PPSNG = fraction of the bare ground covered
!                                             by snow
!                                     PPSNV = fraction of the vegetation covered
!                                             by snow
REAL, DIMENSION(:), INTENT(IN)   :: PZREF, PUREF
!                                     PZREF = reference height of the first
!                                             atmospheric level 
!                                     PUREF = reference height of the wind
!                                             NOTE this is different from ZZREF
!                                             ONLY in stand-alone/forced mode,
!                                             NOT when coupled to a model (MesoNH)
!
REAL, DIMENSION(:), INTENT(IN)    :: PZ0EFF         ! roughness length for momentum
REAL, DIMENSION(:), INTENT(IN)    :: PZ0_WITH_SNOW  ! roughness length for momentum
!                                                   ! (with snow taken into account)
REAL, DIMENSION(:), INTENT(IN)    :: PZ0H_WITH_SNOW ! roughness length for heat
!                                                   ! (with snow taken into account)
!
REAL, DIMENSION(:), INTENT(IN)    :: PF5
!                                     PF5 = water stress function for Hv
REAL, DIMENSION(:), INTENT(IN)    :: PDIRCOSZW 
!                                     PDIRCOSZW = Cosinus of the angle between the normal to the surface and the vertical
!
REAL, DIMENSION(:), INTENT(IN)   :: PFFV, PFF, PFFG, PFFG_NOSNOW, PFFV_NOSNOW
!                                   PFFG = Floodplain fraction over ground
!                                   PFFV = Floodplain fraction over vegetation
!                                   PFF  = Floodplain fraction at the surface
!
REAL, DIMENSION(:), INTENT(IN)   :: PWR, PRHOA
!                                   PWR = liquid water retained on the foliage
!                                   PRHOA = near-ground air density
!
REAL, DIMENSION(:), INTENT(IN)   :: PEMIS, PALB, PCT, PCS, PCG
!                                     PEMIS = emissivity
!                                     PALB = albedo
!                                     PCT = area-averaged heat capacity
!                                     PCS    = heat capacity of the snow (K m2 J-1)
!                                     PCG    = heat capacity of the soil (K m2 J-1)
REAL, DIMENSION(:), INTENT(IN)   :: PGRNDFLUX, PFLSN_COR, PSNOW_THRUFAL 
!                                     PGRNDFLUX = soil/snow interface flux (W/m2) using
!                                                 ISBA-SNOW3L option
!                                     PFLSN_COR = soil/snow interface correction flux to conserve energy (W/m2)
!                                     PSNOW_THRUFAL  = snow runoff/melt leaving pack and available
!                                                  at the surface for runoff or infiltration
!                                                  [kg/(m2 s)]
REAL, DIMENSION(:,:), INTENT(IN)  :: PD_G,  PSOILCONDZ, PSOILHCAPZ
!                                     PD_G      = Depth of bottom of Soil layers (m)
!                                     PSOILCONDZ= ISBA-DF Soil conductivity profile  [W/(m K)]
!                                     PSOILHCAPZ=ISBA-DF Soil heat capacity profile [J/(m3 K)]
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PDZG       ! soil layers thicknesses (DIF option) (m)
REAL, DIMENSION(:,:), INTENT(IN)  :: PDZDIF     ! distance between consecuative layer mid-points (DIF option) (m)
!
REAL, DIMENSION(:), INTENT(IN)    :: PFROZEN1
!                                     PFROZEN1 = ice fraction in supurficial soil
!
REAL, DIMENSION(:), INTENT(IN)     :: PTDEEP_A, PTDEEP_B, PGAMMAT
!                                      PTDEEP_A = Deep soil temperature
!                                                 coefficient depending on flux
!                                      PTDEEP_B = Deep soil temperature (prescribed)
!                                               which models heating/cooling from
!                                               below the diurnal wave penetration
!                                               (surface temperature) depth. If it
!                                               is FLAGGED as undefined, then the zero
!                                               flux lower BC is applied.
!                                      Tdeep = PTDEEP_B + PTDEEP_A * PDEEP_FLUX
!                                              (with PDEEP_FLUX in W/m2)
!                                     PGAMMAT  = Deep soil heat transfer coefficient:
!                                                assuming homogeneous soil so that
!                                                this can be prescribed in units of 
!                                                (1/days): associated time scale with
!                                                PTDEEP.
!
REAL, DIMENSION(:), INTENT(IN)      :: PSNOWFREE_ALB_VEG  !snow free albedo of vegetation for EBA
REAL, DIMENSION(:), INTENT(IN)      :: PSNOWFREE_ALB_SOIL !snow free albedo of soil for EBA option
REAL, DIMENSION(:), INTENT(IN)      :: PPSNV_A !fraction of the the vegetation covered by snow for EBA scheme
!
REAL, DIMENSION(:), INTENT(IN)   :: PFALB, PFEMIS, PFFROZEN
!                                   PFALB = Floodplain albedo
!                                   PFEMIS= Floodplain emis
!
REAL, DIMENSION(:), INTENT(IN)   :: PSNOWSWE
!                                   PSNOWSWE = equivalent water content of
!                                              the D95 snow reservoir (kg m-2)
!
REAL, DIMENSION(:), INTENT(IN)   :: PSRSFC  ! Snow rate falling outside of snow
!                                             covered grid area [kg/(m2 s)]
!
!inout
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PTG
!                                      surface temperature
REAL, DIMENSION(:),   INTENT(INOUT) :: PRESA
!                                      aerodynamic surface resistance
REAL, DIMENSION(:),   INTENT(INOUT) :: PLVTT
!                                      PLVTT=Vaporization heat
REAL, DIMENSION(:),   INTENT(INOUT) :: PLSTT
!                                      PLSTT=Sublimation heat
REAL, DIMENSION(:),   INTENT(INOUT) :: PCPS
!                                      PCPS=specific heat at surface
REAL, DIMENSION(:),   INTENT(INOUT) :: PDELTA
!                                      PDELTA = fraction of the foliage covered
!                                              by intercepted water
!
!out
!
REAL, DIMENSION(:), INTENT(OUT)  :: PCH, PCD, PCDN, PRI
!                                     PCH = drag coefficient for heat
!                                     PCD = drag coefficient for momentum
!                                     PCDN= neutral drag coefficient for momentum
!                                     PRI = Richardson number
!
REAL, DIMENSION(:), INTENT(OUT)  :: PHUG, PHUGI, PHV, PHU, PQS
!                                     PHUG  = ground relative humidity
!                                     PHUGI = ground (ice) relative humidity
!                                     PHV = Halstead coefficient
!                                     PHU = relative humidity at the surface
!                                     PQS = humidity at surface
!
REAL, DIMENSION(:), INTENT(OUT)  :: PALBT, PEMIST
!                                     PALBT  = averaged albedo
!                                     PEMIST = averaged emissivity
!
REAL, DIMENSION(:), INTENT(OUT)   :: PDEEP_FLUX ! Heat flux at bottom of ISBA (W/m2)
!
REAL, DIMENSION(:), INTENT(OUT)     :: PRN, PH, PLE, PLEG, PLEV, PLES
REAL, DIMENSION(:), INTENT(OUT)     :: PLER, PLETR, PEVAP, PGFLUX, PMELTADV, PMELT, PRESTORE
!                                     PRN = net radiation at the surface
!                                     PH = sensible heat flux
!                                     PLE = latent heat flux
!                                     PLEG = latent heat flux from the soil surface
!                                     PLEV = latent heat flux from the vegetation
!                                     PLES = latent heat flux from the snow
!                                     PLER = direct evaporation from the fraction
!                                            delta of the foliage
!                                     PLETR = transpiration of the remaining
!                                             part of the leaves
!                                     PEVAP = total evaporative flux (kg/m2/s)
!                                     PGFLUX = ground flux
!                                     PMELTADV = heat advection by melting snow
!                                                (acts to restore temperature to
!                                                 melting point) (W/m2)
!                                     PMELT = melting rate of snow (kg m-2 s-1)
!                                     PRESTORE = surface restore flux (W m-2)
!
REAL, DIMENSION(:), INTENT(OUT)     :: PLEGI, PUSTAR
!                                      PLEGI   = sublimation component of the 
!                                                latent heat flux from the soil surface
!                                      PUSTAR  = friction velocity
!
REAL, DIMENSION(:), INTENT(OUT)     :: PLE_FLOOD, PLEI_FLOOD !Floodplains latent heat flux [W/m2]
REAL, DIMENSION(:), INTENT(OUT)     :: PSNOWTEMP  ! snow layer temperatures (K)
!
REAL, DIMENSION(:),  INTENT(OUT) :: PAC_AGG  ! aggregated aerodynamic conductance
                                     ! for evaporative flux calculations
REAL, DIMENSION(:),  INTENT(OUT) :: PHU_AGG  ! aggregated relative humidity
                                    ! for evaporative flux calculations
!
!
!*      0.2    declarations of local parameters
!
REAL, PARAMETER            :: ZDEPTH_COR = 0.6
!                             ZDEPTH_COR = depth over which the correction flux is applied
!
REAL, PARAMETER            :: ZDTG1_COR = 10.0 
!                             ZDTG1_COR = Delta temperature limit to comput the correction flux (K)
!
!
!*      0.3    declarations of local variables
!
REAL, DIMENSION(SIZE(PTA)) :: ZQSAT     ! expression for the saturation specific humidity 
!
REAL, DIMENSION(SIZE(PTA)) :: ZDQSAT    ! expression for the saturation specific humidity derivative
!
REAL, DIMENSION(SIZE(PTA)) :: ZTA_IC, ZQA_IC, ZUSTAR2_IC ! TA, QA and friction updated values
!                                                        ! if implicit coupling with atmosphere used.
!
REAL, DIMENSION(SIZE(PTA)) :: ZLEG_DELTA  ! soil evaporation delta fn
REAL, DIMENSION(SIZE(PTA)) :: ZLEGI_DELTA ! soil sublimation delta fn
!
REAL, DIMENSION(SIZE(PTA)) :: ZT2M     ! restore temperature before time integration (K)
REAL, DIMENSION(SIZE(PTA)) :: ZTSM     ! surface temperature before time integration (K)
!
REAL, DIMENSION(SIZE(PTG,1),SIZE(PTG,2)) :: ZFLUX_COR, ZLAYERHCAP
!                                           ZFLUX_COR = correction flux by layer to conserve energy (W/m2)
!
REAL, DIMENSION(SIZE(PTA)) :: ZGRNDFLUX, ZTOTALHCAP, ZWORK
!
INTEGER                    :: INI, INL, JI, JL
LOGICAL                    :: LEXPLICIT_SNOW
!
!*      0.4    declarations of local time spliting variables
!
! Working arrays for flux averaging over time split
!
REAL, DIMENSION(SIZE(PTA)) :: ZDEEP_FLUX, ZLE_FLOOD, ZLEI_FLOOD, &
                              ZRN, ZH, ZLE, ZLEG, ZLEV,  &
                              ZLES, ZLER, ZLETR, ZEVAP,      &
                              ZGFLUX, ZMELTADV, ZMELT,           &
                              ZRESTORE, ZLEGI, ZUSTAR2,          &
                              ZAC_AGG, ZHU_AGG
!
REAL, DIMENSION(SIZE(PTA)) :: ZDEEP_FLUX_SUM, ZLE_FLOOD_SUM, ZLEI_FLOOD_SUM, &
                              ZRN_SUM, ZH_SUM, ZLE_SUM, ZLEG_SUM, ZLEV_SUM,  &
                              ZLES_SUM, ZLER_SUM, ZLETR_SUM, ZEVAP_SUM,      &
                              ZGFLUX_SUM, ZMELTADV_SUM, ZMELT_SUM,           &
                              ZRESTORE_SUM, ZLEGI_SUM, ZUSTAR2_SUM,          &
                              ZAC_AGG_SUM, ZHU_AGG_SUM
!
REAL, PARAMETER            :: ZTSPLIT  = 300. ! s Minimum time tstep required to time-split energy budget
INTEGER                    :: ITSPLIT, JSPLIT
REAL                       :: ZTSTEP, ZNSPLIT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_CEB',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*      1.0    Preliminaries
!              -------------
!
INI=SIZE(PTG,1)
INL=SIZE(PTG,2)
!
!local init
!
ZQSAT      (:) = XUNDEF
ZTA_IC     (:) = XUNDEF
ZQA_IC     (:) = XUNDEF
ZUSTAR2_IC (:) = XUNDEF
ZLEG_DELTA (:) = XUNDEF
ZLEGI_DELTA(:) = XUNDEF
ZDQSAT     (:) = XUNDEF
!
ZDEEP_FLUX(:) = XUNDEF
ZLE_FLOOD (:) = XUNDEF
ZLEI_FLOOD(:) = XUNDEF
ZRN       (:) = XUNDEF
ZH        (:) = XUNDEF
ZLE       (:) = XUNDEF
ZLEG      (:) = XUNDEF
ZLEV      (:) = XUNDEF
ZLES      (:) = XUNDEF
ZLER      (:) = XUNDEF
ZLETR     (:) = XUNDEF
ZEVAP     (:) = XUNDEF
ZGFLUX    (:) = XUNDEF
ZMELTADV  (:) = XUNDEF
ZMELT     (:) = XUNDEF
ZRESTORE  (:) = XUNDEF
ZLEGI     (:) = XUNDEF
ZAC_AGG   (:) = XUNDEF
ZHU_AGG   (:) = XUNDEF
!
ZUSTAR2_SUM   (:) = 0.0
ZEVAP_SUM     (:) = 0.0
!
ZRN_SUM       (:) = 0.0
ZH_SUM        (:) = 0.0
ZGFLUX_SUM    (:) = 0.0
ZLE_SUM       (:) = 0.0
!
ZLEG_SUM      (:) = 0.0
ZLEGI_SUM     (:) = 0.0
ZLEV_SUM      (:) = 0.0
ZLES_SUM      (:) = 0.0
ZLER_SUM      (:) = 0.0
ZLETR_SUM     (:) = 0.0
ZLE_FLOOD_SUM (:) = 0.0
ZLEI_FLOOD_SUM(:) = 0.0
!
ZDEEP_FLUX_SUM(:) = 0.0
ZMELTADV_SUM  (:) = 0.0
ZMELT_SUM     (:) = 0.0
ZRESTORE_SUM  (:) = 0.0
ZAC_AGG_SUM   (:) = 0.0
ZHU_AGG_SUM   (:) = 0.0
!
!-------------------------------------------------------------------------------
!
ZGRNDFLUX(:  ) = PGRNDFLUX(:)
!
ZFLUX_COR(:,:) = 0.0
!
!-------------------------------------------------------------------------------
!
!*      2.0    Correction flux to conserv energy budget
!              ----------------------------------------
!
LEXPLICIT_SNOW=(HSNOW_ISBA == '3-L' .OR. HSNOW_ISBA == 'CRO')
!
IF(LEXPLICIT_SNOW.AND.HISBA/='DIF')THEN
!
  ZFLUX_COR(:,1)=PPSN(:)*PFLSN_COR(:)
!
ELSEIF(LEXPLICIT_SNOW.AND.HISBA=='DIF')THEN
!
  ZLAYERHCAP  (:,:) = 0.0
  ZTOTALHCAP    (:) = 0.0
!
! To conserv energy, the correction flux is distributed at least
! over the first layers of the soil, ZDEPTH_COR. This method prevent 
! numerical oscillations especially when explicit snow vanishes
!
  ZWORK(:)=MIN(PD_G(:,INL),ZDEPTH_COR)
!
  ZLAYERHCAP(:,1)= 1.0/PCT(:)
  ZTOTALHCAP(:  )= 1.0/PCT(:)
  DO JL=2,INL
     DO JI=1,INI
        ZLAYERHCAP(JI,JL)=PSOILHCAPZ(JI,JL)*MIN(PDZG(JI,JL),MAX(0.0,ZWORK(JI)-PD_G(JI,JL)+PDZG(JI,JL)))
        ZTOTALHCAP(JI   )=ZTOTALHCAP(JI)+ZLAYERHCAP(JI,JL)
     ENDDO
  ENDDO
!
  DO JL=1,INL
     DO JI=1,INI
        IF(ZTOTALHCAP(JI)>0.0)THEN
          ZFLUX_COR(JI,JL)=PPSN(JI)*PFLSN_COR(JI)*ZLAYERHCAP(JI,JL)/ZTOTALHCAP(JI)
        ENDIF
    ENDDO
  ENDDO
!
! The second correction is computed if the delta temperature
! due to snow/soil ground flux is superior to ZDTG1_COR (K)
! Especially relevant when PPSN ~ 1 over vegetated area
!
  ZWORK(:)=PTSTEP*PCT(:)*PPSN(:)*ABS(PGRNDFLUX(:))
!
  WHERE(ZTOTALHCAP(:)>0.0.AND.ZWORK(:)>=ZDTG1_COR)
       ZGRNDFLUX(:) = PGRNDFLUX(:)*ZLAYERHCAP(:,1)/ZTOTALHCAP(:)
  ENDWHERE
!
  DO JL=2,INL
     DO JI=1,INI
        IF(ZTOTALHCAP(JI)>0.0.AND.ZWORK(JI)>=ZDTG1_COR)THEN
          ZFLUX_COR(JI,JL)=ZFLUX_COR(JI,JL)+PPSN(JI)*PGRNDFLUX(JI) &
                                           *ZLAYERHCAP(JI,JL)/ZTOTALHCAP(JI)
        ENDIF
     ENDDO
  ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
!
IF(LCPL_ARP)THEN
  ITSPLIT = 1
ELSE
  ITSPLIT  = MAX(1,NINT(PTSTEP/ZTSPLIT))  ! number of split-time steps
ENDIF
!
ZNSPLIT  = REAL(ITSPLIT)
!
ZTSTEP  = PTSTEP/ZNSPLIT             ! split time step
!
!-------------------------------------------------------------------------------
!
DO JSPLIT=1,ITSPLIT
!
!  Save surface and sub-surface temperature values at beginning of time step for 
!  budget and flux calculations:
!
   ZTSM(:) = PTG(:,1)
   ZT2M(:) = PTG(:,2)
!
!
!*      3.0    Aerodynamic drag and heat transfer coefficients
!              -----------------------------------------------
!
!  In DRAG, we use the timestep of ISBA (PTSTEP) and not the split time step (ZTSTEP)
!  because diagnostic canopy evaporation (Er) must be consistent with PWR water
!  mass to limit negative dripping in hydro_veg
!
   CALL DRAG(HISBA, HSNOW_ISBA, HCPSURF, PTSTEP,                                         &
             PTG(:,1), PWG(:,1), PWGI(:,1), PEXNS, PEXNA, PTA, PVMOD, PQA, PRR, PSR,     &
             PPS, PRS, PVEG, PZ0_WITH_SNOW, PZ0EFF, PZ0H_WITH_SNOW,                      &
             PWFC(:,1), PWSAT(:,1), PPSNG, PPSNV, PZREF, PUREF,                          &
             PDIRCOSZW, PDELTA, PF5, PRESA,  PCH, PCD, PCDN, PRI, PHUG, PHUGI,           &
             PHV, PHU, PCPS, PQS, PFFG, PFFV, PFF, PFFG_NOSNOW, PFFV_NOSNOW,             &
             ZLEG_DELTA, ZLEGI_DELTA, PWR, PRHOA, PLVTT, PQSAT=ZQSAT                     )  
!
!
!
!*      4.0    Resolution of the surface and soil energy budget
!              ------------------------------------------------
!
   CALL E_BUDGET(HISBA, HSNOW_ISBA, OFLOOD, OTEMP_ARP, HIMPLICIT_WIND,                   &
                 ZTSTEP, PSODELX, PUREF,                                                 &
                 PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF,        &
                 PPEQ_B_COEF, PVMOD, PCD, PTG, ZTSM, ZT2M, PSNOWALB, PSW_RAD, PLW_RAD,   &
                 PTA, PQA, PPS, PRHOA, PEXNS, PEXNA, PCPS, PLVTT, PLSTT,  PVEG,          &
                 PHUG, PHUGI, PHV, ZLEG_DELTA, ZLEGI_DELTA, PEMIS, PALB, PRESA,          &
                 PCT, PCG, PPSN, PPSNV, PPSNG, ZGRNDFLUX, ZFLUX_COR,                     &
                 PD_G, PDZG, PDZDIF, PSOILCONDZ, PSOILHCAPZ,  PALBT, PEMIST,             &
                 ZQSAT, ZDQSAT, PFROZEN1, PTDEEP_A, PTDEEP_B, PGAMMAT,                   &
                 ZTA_IC, ZQA_IC, ZUSTAR2_IC,                                             &
                 PSNOWFREE_ALB_VEG, PPSNV_A, PSNOWFREE_ALB_SOIL,                         &
                 PFFG, PFFV, PFF, PFFROZEN, PFALB, PFEMIS, ZDEEP_FLUX, ZRESTORE          )
!
!
!*      5.0    Energy and momentum fluxes
!              --------------------------
!
!*******************************************************************************
! WARNING: at this stage, ZALBT and ZEMIST have two different meanings according
!          to the ISBA snow-scheme option:
!  'D95' : they represent aggregated (snow + flood + snow-flood-free) albedo and emissivity
!  '3-L' : they represent                    flood + snow-flood-free  albedo and emissivity
!*******************************************************************************
!
   CALL ISBA_FLUXES(HISBA, HSNOW_ISBA, OTEMP_ARP, ZTSTEP, PSODELX,                       &
                    PSW_RAD, PLW_RAD, ZTA_IC, ZQA_IC,                                    &
                    PRHOA, PEXNS, PEXNA, PCPS, PLVTT, PLSTT,                             &
                    PVEG, PHUG, PHUGI, PHV, ZLEG_DELTA, ZLEGI_DELTA, PDELTA, PRESA,      &
                    PF5, PRS, PCS, PCG, PCT, PSNOWSWE, ZTSM, ZT2M,                       &
                    PPSN, PPSNV, PPSNG, PFROZEN1,                                        &
                    PALBT, PEMIST, ZQSAT, ZDQSAT, PSNOW_THRUFAL,                         &
                    ZRN, ZH, ZLE, ZLEG, ZLEGI, ZLEV, ZLES, ZLER, ZLETR, ZEVAP, ZGFLUX,   &
                    ZMELTADV, ZMELT,                                                     &
                    PSOILCONDZ,  PD_G, PDZG, PTG,                                        &
                    PSRSFC, PPSNV_A, PFFG, PFFV, PFF, PFFROZEN,                          &
                    ZLE_FLOOD, ZLEI_FLOOD, PSNOWTEMP                                     ) 
!
!
!*      6.0    Aggregated coefficients
!              -----------------------
!
!  Compute aggregated coefficients for evaporation
!  Sum(LEV+LEG+LEGI+LES) = ACagg * Lv * RHOA * (HUagg.Qsat - Qa)
!
   ZAC_AGG(:) =   1. / PRESA(:) / PLVTT(:)                                &
        * ( PLVTT(:)*    PVEG(:) *(1.-PPSNV(:))                 *PHV(:)   &
          + PLVTT(:)*(1.-PVEG(:))*(1.-PPSNG(:))*(1.-PFROZEN1(:))          &
          + PLSTT(:)*(1.-PVEG(:))*(1.-PPSNG(:))*    PFROZEN1(:)           &
          + PLSTT(:)*                 PPSN (:)                            )  
!
   WHERE(ZAC_AGG(:)>0.0)
         ZHU_AGG(:) =   1. / (PRESA(:) * ZAC_AGG(:)) / PLVTT(:)                 &
              * ( PLVTT(:)*    PVEG(:) *(1.-PPSNV(:))                 *PHV(:)   &
                + PLVTT(:)*(1.-PVEG(:))*(1.-PPSNG(:))*(1.-PFROZEN1(:))*PHUG(:)  &
                + PLSTT(:)*(1.-PVEG(:))*(1.-PPSNG(:))*    PFROZEN1(:) *PHUGI(:) &
                + PLSTT(:)*                 PPSN (:)                            )  
   ENDWHERE
!
   ZUSTAR2_SUM   (:) = ZUSTAR2_SUM   (:) + ZUSTAR2_IC(:)
!
   ZEVAP_SUM     (:) = ZEVAP_SUM     (:) + ZEVAP     (:)
!
   ZRN_SUM       (:) = ZRN_SUM       (:) + ZRN       (:)
   ZH_SUM        (:) = ZH_SUM        (:) + ZH        (:)
   ZGFLUX_SUM    (:) = ZGFLUX_SUM    (:) + ZGFLUX    (:)
   ZLE_SUM       (:) = ZLE_SUM       (:) + ZLE       (:)
!
   ZLEG_SUM      (:) = ZLEG_SUM      (:) + ZLEG      (:)
   ZLEGI_SUM     (:) = ZLEGI_SUM     (:) + ZLEGI     (:)   
   ZLEV_SUM      (:) = ZLEV_SUM      (:) + ZLEV      (:)
   ZLES_SUM      (:) = ZLES_SUM      (:) + ZLES      (:)
   ZLER_SUM      (:) = ZLER_SUM      (:) + ZLER      (:)
   ZLETR_SUM     (:) = ZLETR_SUM     (:) + ZLETR     (:)
   ZLE_FLOOD_SUM (:) = ZLE_FLOOD_SUM (:) + ZLE_FLOOD (:)
   ZLEI_FLOOD_SUM(:) = ZLEI_FLOOD_SUM(:) + ZLEI_FLOOD(:)   
!
   ZDEEP_FLUX_SUM(:) = ZDEEP_FLUX_SUM(:) + ZDEEP_FLUX(:)
   ZMELTADV_SUM  (:) = ZMELTADV_SUM  (:) + ZMELTADV  (:)
   ZMELT_SUM     (:) = ZMELT_SUM     (:) + ZMELT     (:)
   ZRESTORE_SUM  (:) = ZRESTORE_SUM  (:) + ZRESTORE  (:)
   ZAC_AGG_SUM   (:) = ZAC_AGG_SUM   (:) + ZAC_AGG   (:)
   ZHU_AGG_SUM   (:) = ZHU_AGG_SUM   (:) + ZHU_AGG   (:)
!
!-------------------------------------------------------------------------------
!
ENDDO
!
PUSTAR    (:) = SQRT(ZUSTAR2_SUM(:)/ZNSPLIT)
!
PEVAP     (:) = ZEVAP_SUM     (:) / ZNSPLIT
!
PRN       (:) = ZRN_SUM       (:) / ZNSPLIT
PH        (:) = ZH_SUM        (:) / ZNSPLIT
PGFLUX    (:) = ZGFLUX_SUM    (:) / ZNSPLIT
PLE       (:) = ZLE_SUM       (:) / ZNSPLIT
!
PLEG      (:) = ZLEG_SUM      (:) / ZNSPLIT
PLEGI     (:) = ZLEGI_SUM     (:) / ZNSPLIT
PLEV      (:) = ZLEV_SUM      (:) / ZNSPLIT
PLES      (:) = ZLES_SUM      (:) / ZNSPLIT
PLER      (:) = ZLER_SUM      (:) / ZNSPLIT
PLETR     (:) = ZLETR_SUM     (:) / ZNSPLIT
PLE_FLOOD (:) = ZLE_FLOOD_SUM (:) / ZNSPLIT
PLEI_FLOOD(:) = ZLEI_FLOOD_SUM(:) / ZNSPLIT
!
PDEEP_FLUX(:) = ZDEEP_FLUX_SUM(:) / ZNSPLIT
PMELTADV  (:) = ZMELTADV_SUM  (:) / ZNSPLIT
PMELT     (:) = ZMELT_SUM     (:) / ZNSPLIT
PRESTORE  (:) = ZRESTORE_SUM  (:) / ZNSPLIT
PAC_AGG   (:) = ZAC_AGG_SUM   (:) / ZNSPLIT
PHU_AGG   (:) = ZHU_AGG_SUM   (:) / ZNSPLIT
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_CEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ISBA_CEB
