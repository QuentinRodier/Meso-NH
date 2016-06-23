!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE HYDRO(HISBA, HSNOW_ISBA, HRUNOFF, HSOILFRZ, OMEB, OGLACIER,&
                         OFLOOD, PTSTEP, PVEGTYPE,                          &
                         PRR, PSR, PLEV, PLETR, PLEG, PLES,                 &
                         PRUNOFFB, PWDRAIN,                                 &
                         PC1, PC2, PC3, PC4B, PC4REF, PWGEQ, PCG, PCT,      &
                         PVEG, PLAI, PWRMAX, PMELT, PTAUICE, PLEGI,         &
                         PRUNOFFD, PSOILWGHT, KLAYER_HORT, KLAYER_DUN,      &
                         PPSNV, PPSNG,                                      &
                         PSNOW_THRUFAL, PEVAPCOR, PSUBVCOR,                 &
                         PWR, PSOILHCAPZ,                                   &
                         PSNOWSWE, PSNOWALB, PSNOWRHO,                      &
                         PBCOEF, PWSAT, PCONDSAT, PMPOTSAT, PWFC,           &
                         PWWILT, PF2WGHT, PF2, PD_G, PDZG, PDZDIF, PPS,     &
                         PWG, PWGI, PTG, KWG_LAYER,                         &
                         PDRAIN, PRUNOFF, PTOPQS,                           &
                         PIRRIG, PWATSUP, PTHRESHOLD, LIRRIDAY, LIRRIGATE,  &
                         HKSAT, HRAIN, HHORT, PMUF, PFSAT, PKSAT_ICE,       &
                         PD_ICE, PHORTON, PDRIP, PFFG, PFFV , PFFLOOD,      &
                         PPIFLOOD, PIFLOOD, PPFLOOD, PRRVEG, PIRRIG_FLUX,   &
                         PIRRIG_GR, PQSB, PFWTD, PWTD,                      &
                         PDELHEATG, PDELHEATG_SFC,                          &
                         PDELPHASEG, PDELPHASEG_SFC, PLVTT, PLSTT           )
!     #####################################################################
!
!!****  *HYDRO*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the evolution of the water variables, i.e., the superficial
!     and deep-soil volumetric water content (wg and w2), the equivalent
!     liquid water retained in the vegetation canopy (Wr), the equivalent
!     water of the snow canopy (Ws), and also of the albedo and density of
!     the snow (i.e., SNOWALB and SNOWRHO).  Also determine the runoff and drainage
!     into the soil.
!         
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!      
!!    AUTHOR
!!    ------
!!
!!      S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original    14/03/95 
!!                  31/08/98 (V. Masson and F. Habets) add Dumenil et Todini
!!                           runoff scheme
!!                  31/08/98 (V. Masson and A. Boone) add the third soil-water
!!                           reservoir (WG3,D3)
!!                  19/07/05 (P. LeMoigne) bug in runoff computation if isba-2L
!!                  10/10/05 (P. LeMoigne) bug in hydro-soil calling sequence
!!                  25/05/08 (B. Decharme) Add floodplains
!!                  27/11/09 (A. Boone)    Add possibility to do time-splitting when
!!                                         calling hydro_soildif (DIF option only)
!!                                         for *very* large time steps (30min to 1h+).
!!                                         For *usual* sized time steps, time step
!!                                         NOT split.
!!                     08/11 (B. Decharme) DIF optimization
!!                     09/12 (B. Decharme) Bug in wg2 ice energy budget
!!                     10/12 (B. Decharme) EVAPCOR snow correction in DIF
!!                                         Add diag IRRIG_FLUX
!!                     04/13 (B. Decharme) Pass soil phase changes routines here
!!                                         Apply physical limits on wg in hydro_soil.F90
!!                                         Subsurface runoff if SGH (DIF option only)
!!                                         water table / surface coupling
!!                  02/2013  (C. de Munck) specified irrigation rate of ground added
!!                  10/2014  (A. Boone)    MEB added
!!                  07/15    (B. Decharme) Numerical adjustement for F2 soilstress function
!!                  03/16    (B. Decharme) Limit flood infiltration
!!                  05/2016  (B. Decharme) Bug : no Horton runoff in hydr_soildif because is already 
!!                                               computed in hydro_sgh if required
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,      ONLY : XRHOLW, XDAY, XTT, XLSTT, XLMTT
USE MODD_ISBA_PAR,  ONLY : XWGMIN, XDENOM_MIN
USE MODD_SURF_PAR,  ONLY : XUNDEF, NUNDEF
!
#ifdef TOPD
USE MODD_COUPLING_TOPD, ONLY : LCOUPL_TOPD, XAS_NATURE, XATOP, XRUNOFF_TOP, NMASKT_PATCH
#endif
!
USE MODI_HYDRO_VEG
USE MODI_HYDRO_SNOW
USE MODI_HYDRO_SOIL
USE MODI_HYDRO_SOILDIF                                          
USE MODI_HYDRO_SGH
USE MODI_ICE_SOILDIF              
USE MODI_ICE_SOILFR              
!
USE MODE_THERMOS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
 CHARACTER(LEN=*),     INTENT(IN)   :: HISBA   ! type of ISBA version:
!                                             ! '2-L' (default)
!                                             ! '3-L'
!                                             ! 'DIF'   ISBA-DF
 CHARACTER(LEN=*),     INTENT(IN)  :: HSNOW_ISBA ! 'DEF' = Default F-R snow scheme
!                                               !         (Douville et al. 1995)
!                                               ! '3-L' = 3-L snow scheme (option)
!                                               !         (Boone and Etchevers 2001)
 CHARACTER(LEN=*),     INTENT(IN)   :: HRUNOFF ! surface runoff formulation
!                                             ! 'WSAT'
!                                             ! 'DT92'
!                                             ! 'SGH ' Topmodel
 CHARACTER(LEN=*),   INTENT(IN)      :: HSOILFRZ   ! soil freezing-physics option
!                                                 ! 'DEF'   Default (Boone et al. 2000; Giard and Bazile 2000)
!                                                 ! 'LWT'   phase changes as above, but relation between unfrozen 
!                                                         water and temperature considered
LOGICAL, INTENT(IN)                :: OGLACIER ! True = Over permanent snow and ice, 
!                                                initialise WGI=WSAT,
!                                                Hsnow>=10m and allow 0.8<SNOALB<0.85
!                                                False = No specific treatment
!
LOGICAL, INTENT(IN)                :: OMEB   ! True  = patch with multi-energy balance 
!                                            ! False = patch with classical (composite) ISBA 
!
LOGICAL, INTENT(IN)                :: OFLOOD ! Flood scheme 
!
REAL, INTENT(IN)                    :: PTSTEP
!                                      timestep of the integration
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PVEGTYPE ! fraction of each vegetation
!
REAL, DIMENSION(:), INTENT(IN)    :: PRR, PSR, PLEV, PLETR, PLEG, PLES
!                                      PRR = rain rate
!                                      PSR = snow rate
!                                      PLEV = latent heat of evaporation over vegetation
!                                      PLETR = evapotranspiration of the vegetation
!                                      PLEG = latent heat of evaporation over the ground
!                                      PLES = latent heat of sublimation over the snow
!
REAL, DIMENSION(:), INTENT(IN)    :: PRUNOFFB      ! slope of the runoff curve
REAL, DIMENSION(:), INTENT(IN)    :: PLVTT, PLSTT  ! latent heat of vaporization and sublimation (J/kg)  
REAL, DIMENSION(:), INTENT(IN)    :: PWDRAIN       ! minimum Wg for drainage (m3/m3)
!
REAL, DIMENSION(:), INTENT(IN)    :: PC1, PC2, PWGEQ, PCG, PCT
REAL, DIMENSION(:,:), INTENT(IN)  :: PC3
!                                      soil coefficients
!                                      C1, C2 = coefficients for the moisture calculations
!                                      C3 = coefficient for WG2 calculation
!                                      PWGEQ = equilibrium surface volumetric moisture
!                                      PCG = soil heat capacity
!                                      PCT = grid-averaged heat capacity
!
REAL, DIMENSION(:), INTENT(IN)    :: PVEG, PLAI, PRUNOFFD, PWRMAX
!                                      PVEG = fraction of vegetation 
!                                      PLAI = Leaf Area Index 
!                                      PRUNOFFD = depth over which sub-grid runoff calculated (m)
!                                      PWRMAX = maximum equivalent water content
!                                               in the vegetation canopy
!
REAL, DIMENSION(:), INTENT(IN)    :: PC4B, PC4REF
!                                      PC4REF, PC4B = fiiting soil paramter for vertical diffusion (C4)
!
REAL, DIMENSION(:), INTENT(IN)    :: PPSNV, PPSNG
!                                      PPSNV = vegetation covered by snow
!                                      PPSNG = baresoil covered by snow
!
REAL, DIMENSION(:), INTENT(IN)    :: PTAUICE, PLEGI
!                                    PTAUICE = characteristic time scale for soil water
!                                                phase changes (s) 
!                                    PLEGI   = surface soil ice sublimation 
!
REAL, DIMENSION(:), INTENT(IN)    :: PSNOW_THRUFAL, PEVAPCOR, PSUBVCOR
!                                    PSNOW_THRUFAL = rate that liquid water leaves snow pack: 
!                                               *ISBA-ES* [kg/(m2 s)]
!                                    PEVAPCOR = correction if evaporation from snow exceeds
!                                               actual amount on the surface [kg/(m2 s)]
!                                    PSUBVCOR = correction if sublimation from snow intercepted 
!                                               on the MEB canopy exceeds snow available as it 
!                                               disappears [kg/(m2 s)]
!
REAL, DIMENSION(:), INTENT(IN)    :: PPS, PF2                                       
!                                    PPS  = surface pressure (Pa)
!                                    PF2  = total water stress factor (-)
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PWSAT, PCONDSAT, PWFC, PD_G, PF2WGHT, PWWILT
!                                    PD_G   = Depth of bottom of Soil layers (m)
!                                    PWFC     = field capacity profile (m3/m3)
!                                    PWWILT   = wilting point profile (m3/m3)
!                                    PWSAT    = porosity profile (m3/m3)
!                                    PCONDSAT = hydraulic conductivity at saturation (m/s)
!                                    PF2WGHT   = water stress factor (profile) (-)

REAL, DIMENSION(:,:), INTENT(IN)  :: PDZDIF, PDZG
!                                    PDZDIF = distance between consecuative layer mid-points
!                                    PDZG   = soil layers thicknesses
!
REAL, DIMENSION(:,:), INTENT(IN) :: PSOILHCAPZ
!                                   PSOILHCAPZ = ISBA-DF Soil heat capacity profile [J/(m3 K)]
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PSOILWGHT  ! ISBA-DIF: weights for vertical
!                                               ! integration of soil water and properties
INTEGER,             INTENT(IN)   :: KLAYER_HORT! DIF optimization
INTEGER,             INTENT(IN)   :: KLAYER_DUN ! DIF optimization
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PMPOTSAT,PBCOEF
!                                    PMPOTSAT = matric potential at saturation (m)
!                                    PBCOEF   = slope of the retention curve (-)
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PTG
!                                    PTG   = soil layer average temperatures (K)
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PWGI, PWG
!                                      PWGI  = soil frozen volumetric water content (m3/m3)
!                                      PWG  = soil liquid volumetric water content (m3/m3)
!                                      Prognostic variables of ISBA at 't-dt'
!                                      PWGI(:,1) = surface-soil volumetric ice content
!                                      PWGI(:,2) = deep-soil volumetric ice content
!
INTEGER, DIMENSION(:), INTENT(IN) :: KWG_LAYER  
!                                    KWG_LAYER = Number of soil moisture layers (DIF option)
!
REAL, DIMENSION(:), INTENT(INOUT) :: PDELHEATG, PDELHEATG_SFC
!                                      PDELHEATG_SFC = change in heat storage of the surface soil layer over the current time step (W m-2)
!                                      PDELHEATG     = change in heat storage of the entire soil column over the current time step (W m-2)
!
REAL, DIMENSION(:), INTENT(INOUT) :: PWR, PSNOWSWE, PSNOWALB, PSNOWRHO
REAL, DIMENSION(:), INTENT(INOUT) :: PMELT
REAL, DIMENSION(:), INTENT(OUT)   :: PDRAIN, PRUNOFF
!                                      PWR = liquid water retained on the foliage
!                                             of the vegetation at time 't+dt'
!                                      PSNOWSWE = equivalent water content of the
!                                             snow reservoir at time 't+dt'
!                                      PSNOWALB = albedo of the snow at 't+dt'
!                                      PSNOWRHO = density of the snow at 't+dt' 
!                                      PMELT = melting rate of the snow
!
REAL, DIMENSION(:), INTENT(OUT)   :: PDELPHASEG, PDELPHASEG_SFC
!                                     PDELPHASEG     = latent heating due to soil freeze-thaw in the entire soil column  (W m-2)
!                                     PDELPHASEG_SFC = latent heating due to soil freeze-thaw in the surface soil layer  (W m-2)
!
!
REAL   ,DIMENSION(:),INTENT(IN)    :: PIRRIG
REAL   ,DIMENSION(:),INTENT(IN)    :: PWATSUP
REAL   ,DIMENSION(:),INTENT(IN)    :: PTHRESHOLD
LOGICAL,DIMENSION(:),INTENT(INOUT) :: LIRRIDAY
LOGICAL,DIMENSION(:),INTENT(IN)    :: LIRRIGATE
REAL   ,DIMENSION(:),INTENT(INOUT) :: PIRRIG_FLUX ! irrigation rate (kg/m2/s)
REAL   ,DIMENSION(:),INTENT(IN)    :: PIRRIG_GR ! ground irrigation rate (kg/m2/s)
!
!
!
 CHARACTER(LEN=*),     INTENT(IN)   :: HKSAT   ! soil hydraulic profil option
!                                             ! 'DEF'  = ISBA homogenous soil
!                                             ! 'SGH'  = ksat exponential decay
!
 CHARACTER(LEN=*), INTENT(IN)       :: HRAIN   ! Rainfall spatial distribution
                                              ! 'DEF' = No rainfall spatial distribution
                                              ! 'SGH' = Rainfall exponential spatial distribution
                                              ! 
!
 CHARACTER(LEN=*), INTENT(IN)       :: HHORT   ! Horton runoff
                                              ! 'DEF' = no Horton runoff
                                              ! 'SGH' = Horton runoff
!                                        
REAL, DIMENSION(:),  INTENT(IN)   :: PD_ICE   !depth of the soil column for the calculation
!                                              of the frozen soil fraction (m)
REAL, DIMENSION(:),  INTENT(IN)   :: PKSAT_ICE!hydraulic conductivity at saturation (m/s)
!                                            
REAL, DIMENSION(:),  INTENT(IN)   :: PMUF     !fraction of the grid cell reached by the rainfall
REAL, DIMENSION(:),  INTENT(INOUT):: PFSAT    !Topmodel/dt92 saturated fraction
!
REAL, DIMENSION(:),  INTENT(OUT)  :: PHORTON   !Horton runoff (kg/m2/s)
!
REAL, DIMENSION(:),  INTENT(INOUT) :: PDRIP    !Dripping from the vegetation (kg/m2/s)
REAL, DIMENSION(:),  INTENT(INOUT) :: PRRVEG   !Precip. intercepted by vegetation (kg/m2/s)
!
REAL, DIMENSION(:),  INTENT(IN)    :: PFFG,PFFV
REAL, DIMENSION(:),  INTENT(IN)    :: PFFLOOD  !Floodplain effective fraction
REAL, DIMENSION(:),  INTENT(IN)    :: PPIFLOOD !Floodplain potential infiltration [kg/m²/s]
!
REAL, DIMENSION(:), INTENT(INOUT)  :: PIFLOOD  !Floodplain real infiltration      [kg/m²/s]
REAL, DIMENSION(:), INTENT(INOUT)  :: PPFLOOD  !Floodplain interception           [kg/m²/s]
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PTOPQS   !Topmodel (HRUNOFF=SGH) subsurface flow by layer (m/s)
REAL, DIMENSION(:),   INTENT(OUT)  :: PQSB     !Topmodel (HRUNOFF=SGH) lateral subsurface flow [kg/m²/s]
!
REAL, DIMENSION(:), INTENT(IN)     :: PFWTD    !grid-cell fraction of water table to rise
REAL, DIMENSION(:), INTENT(IN)     :: PWTD     !water table depth (m)
!
!*      0.2    declarations of local variables
!
!
INTEGER                         :: JJ, JL      ! loop control                                       
INTEGER                         :: INDT, JDT   ! Time splitting indicies
INTEGER                         :: INI, INL, IDEPTH ! (ISBA-DF option)
!
REAL                            :: ZTSTEP      ! maximum time split time step (<= PTSTEP)
!                                              ! ONLY used for DIF option.
!
REAL, DIMENSION(SIZE(PVEG))     :: ZPG, ZPG_MELT, ZDUNNE,                            &
                                   ZLEV, ZLEG, ZLEGI, ZLETR, ZPSNV,                  &
                                   ZRR, ZDG3, ZWG3, ZWSAT_AVG, ZWWILT_AVG, ZWFC_AVG, &
                                   ZRUNOFF, ZDRAIN, ZHORTON, ZEVAPCOR, ZQSB 
!                                      Prognostic variables of ISBA at 't-dt'
!                                      ZPG = total water reaching the ground
!                                      ZPG_MELT = snowmelt reaching the ground 
!                                      ZDUNNE  = Dunne runoff
!                                 ZLEV, ZLEG, ZLEGI, ZLETR = Evapotranspiration amounts
!                                      from the non-explicit snow area *ISBA-ES*
!                                 ZPSNV = used to calculate interception of liquid
!                                      water by the vegetation in FR snow method:
!                                      For ES snow method, precipitation already modified
!                                      so set this to zero here for this option.
!                                 ZWSAT_AVG, ZWWILT_AVG, ZWFC_AVG = Average water and ice content
!                                      values over the soil depth D2 (for calculating surface runoff)
!                                 ZDG3, ZWG3, ZRUNOFF, ZDRAIN, ZQSB and ZHORTON are working variables only used for DIF option
!                                 ZEVAPCOR = correction if evaporation from snow exceeds
!                                               actual amount on the surface [m/s]
!
REAL, DIMENSION(SIZE(PVEG))     :: ZDWGI1, ZDWGI2, ZKSFC_IVEG
!                                      ZDWGI1 = surface layer liquid water equivalent 
!                                               volumetric ice content time tendency
!                                      ZDWGI2 = deep-soil layer liquid water equivalent 
!                                               volumetric ice content time tendency
!                                      ZKSFC_IVEG = non-dimensional vegetation insolation coefficient
!
REAL, DIMENSION(SIZE(PVEG))    :: ZWGI_EXCESS, ZF2
!                                 ZWGI_EXCESS = Soil ice excess water content
!                                 ZF2         = Soilstress function for transpiration
!
REAL, DIMENSION(SIZE(PWG,1),SIZE(PWG,2)) :: ZQSAT, ZQSATI, ZTI, ZPS
!                                           For specific humidity at saturation computation (ISBA-DIF)
!
REAL, DIMENSION(SIZE(PWG,1),SIZE(PWG,2)) :: ZWGI0
!                                      ZWGI0 = initial soil ice content (m3 m-3) before update
!                                              for budget diagnostics     
!
!*      0.3    declarations of local parameters
!
REAL, PARAMETER             :: ZINSOLFRZ_VEG = 0.20  ! (-)       Vegetation insolation coefficient
!
REAL, PARAMETER             :: ZINSOLFRZ_LAI = 30.0  ! (m2 m-2)  Vegetation insolation coefficient

REAL, PARAMETER             :: ZTIMEMAX      = 900.  ! s  Maximum timescale without time spliting
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       0.     Initialization:
!               ---------------
!
IF (LHOOK) CALL DR_HOOK('HYDRO',0,ZHOOK_HANDLE)
!
JDT    = 0
INDT   = 0
ZTSTEP = 0.0
!
ZPG(:)           = 0.0
ZPG_MELT(:)      = 0.0
ZDUNNE(:)        = 0.0
!
ZWSAT_AVG(:)     = 0.0
ZWWILT_AVG(:)    = 0.0
ZWFC_AVG(:)      = 0.0
!
ZRR(:)           = PRR(:)
!
ZDRAIN(:)        = 0.
ZHORTON(:)       = 0.
ZRUNOFF(:)       = 0.
ZWGI_EXCESS(:)   = 0.
ZEVAPCOR(:)      = 0.
ZQSB    (:)      = 0.
!
PDRAIN(:)        = 0.
PRUNOFF(:)       = 0.
PHORTON(:)       = 0.
PQSB   (:)       = 0.
!
PDELPHASEG(:)    = 0.0
PDELPHASEG_SFC(:)= 0.0
ZWGI0(:,:)       = 0.0
!
ZF2(:) = MAX(XDENOM_MIN,PF2(:))
!
! Initialize evaporation components: variable definitions
! depend on snow or explicit canopy scheme:
!
IF(OMEB)THEN
!
! MEB uses explicit snow scheme by default, but fluxes already aggregated
! for snow and floods so no need to multiply by fractions here. 
!
   ZLEV(:)          = PLEV(:)
   ZLETR(:)         = PLETR(:)
   ZLEG(:)          = PLEG(:)
   ZLEGI(:)         = PLEGI(:)
   ZPSNV(:)         = 0.0
!
   ZEVAPCOR(:)      = PEVAPCOR(:) + PSUBVCOR(:)
!
ELSE
!
! Initialize evaporation components: variable definitions
! depend on snow scheme:
!
   IF(HSNOW_ISBA == '3-L' .OR. HSNOW_ISBA == 'CRO' .OR. HISBA == 'DIF')THEN
      ZLEV(:)          = (1.0-PPSNV(:)-PFFV(:)) * PLEV(:)
      ZLETR(:)         = (1.0-PPSNV(:)-PFFV(:)) * PLETR(:)
      ZLEG(:)          = (1.0-PPSNG(:)-PFFG(:)) * PLEG(:)
      ZLEGI(:)         = (1.0-PPSNG(:)-PFFG(:)) * PLEGI(:)
      ZPSNV(:)         = 0.0
   ELSE
      ZLEV(:)          = PLEV(:)
      ZLETR(:)         = PLETR(:)
      ZLEG(:)          = PLEG(:)
      ZLEGI(:)         = PLEGI(:)
      ZPSNV(:)         = PPSNV(:)+PFFV(:)
   ENDIF
!
   ZEVAPCOR(:)         = PEVAPCOR(:) 

ENDIF
!
! Initialize average soil hydrological parameters
! over the entire soil column: if Isba Force-Restore
! is in use, then parameter profile is constant
! so simply use first element of this array: if
! the Diffusion option is in force, the relevant
! calculation is done later within this routine.
!
IF(HISBA == '2-L' .OR. HISBA == '3-L')THEN  
   ZWSAT_AVG(:)     = PWSAT(:,1)
   ZWWILT_AVG(:)    = PWWILT(:,1)
   ZWFC_AVG(:)      = PWFC(:,1)
ENDIF
!
IF (HISBA == '3-L') THEN                                   
   ZDG3(:) = PD_G(:,3)
   ZWG3(:) = PWG (:,3)
ELSE
   ZDG3(:) = XUNDEF
   ZWG3(:) = XUNDEF
END IF
!
!-------------------------------------------------------------------------------
!
!*       1.     EVOLUTION OF THE EQUIVALENT WATER CONTENT Wr
!               --------------------------------------------
!
!
!
IF(.NOT.OMEB)THEN ! Canopy Int & Irrig Already accounted for if MEB in use.
!
   PIRRIG_FLUX(:)=0.0
!
!* add irrigation over vegetation to liquid precipitation (rr)
!
!
   IF (SIZE(LIRRIGATE)>0) THEN
      WHERE (LIRRIGATE(:) .AND. PIRRIG(:)>0. .AND. PIRRIG(:) /= XUNDEF .AND. (PF2(:)<PTHRESHOLD(:)) )
         PIRRIG_FLUX(:) = PWATSUP(:) / XDAY           
         ZRR        (:) = ZRR(:) + PWATSUP(:) / XDAY
         LIRRIDAY   (:) = .TRUE.           
      END WHERE
   ENDIF
!
!* interception reservoir and dripping computation
!
   CALL HYDRO_VEG(HRAIN, PTSTEP, PMUF,                    &
                   ZRR, ZLEV, ZLETR, PVEG, ZPSNV,         &
                   PWR, PWRMAX, ZPG, PDRIP, PRRVEG, PLVTT ) 
!
!
!
ELSE
!
! For MEB case, interception interactions already computed and PRR represents
! water falling (drip and not intercepted by vegetation) outside of snow covered
! areas. Part for snow covered areas (net outflow at base of snowpack) accounted
! for in PSNOW_THRUFAL.
!
   ZPG(:) = PRR(:)
!
ENDIF
!
!* add irrigation over ground to potential soil infiltration (pg)
!
PIRRIG_FLUX(:) = PIRRIG_FLUX(:) + PIRRIG_GR(:)
!
ZPG(:) = ZPG(:) + PIRRIG_GR(:)
!
!-------------------------------------------------------------------------------
!
!*       2.     EVOLUTION OF THE EQUIVALENT WATER CONTENT snowSWE 
!               -------------------------------------------------
!
!*       3.     EVOLUTION OF SNOW ALBEDO 
!               ------------------------
!
!*       4.     EVOLUTION OF SNOW DENSITY 
!               -------------------------
!
! Boone and Etchevers '3-L' snow option
IF(HSNOW_ISBA == '3-L' .OR. HSNOW_ISBA == 'CRO' .OR. HISBA == 'DIF')THEN
!
  ZPG_MELT(:)   = ZPG_MELT(:)   + PSNOW_THRUFAL(:)          ! [kg/(m2 s)]
!
! Note that 'melt' is referred to as rain and meltwater
! running off from the snowpack in a timestep for ISBA-ES,
! not the actual amount of ice converted to liquid.
!
  PMELT(:) = PMELT(:) + PSNOW_THRUFAL(:)          ! [kg/(m2 s)]
!
ELSE
  !
  CALL HYDRO_SNOW(OGLACIER, PTSTEP, PVEGTYPE,          &
                  PSR, PLES, PMELT,                    &
                  PSNOWSWE, PSNOWALB, PSNOWRHO,ZPG_MELT)
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       5.     Sub Grid Hydrology
!               ------------------
!
! - Dunne runoff  : Dumenil et Todini (1992) or Topmodel
! - Horton runoff : Direct or exponential precipitation distribution
! - Floodplains interception and infiltration
!
 CALL HYDRO_SGH(HISBA,HRUNOFF,HRAIN,HHORT,        &
                 PTSTEP,PD_G,PDZG,                &
                 PWSAT,PWFC,PWWILT,               &
                 PWG, PWGI, KWG_LAYER,            &
                 ZPG, ZPG_MELT, PMUF,             &
                 PCONDSAT, PBCOEF,                &
                 PMPOTSAT, PKSAT_ICE, PD_ICE,     &
                 PFSAT, PHORTON, ZDUNNE, PFFLOOD, &
                 PPIFLOOD, PIFLOOD, PPFLOOD,      &
                 PRUNOFFB, PRUNOFFD, PCG,         &
                 PSOILWGHT, OFLOOD, KLAYER_HORT,  &
                 KLAYER_DUN                       )         
!
!-------------------------------------------------------------------------------
!
!*       6.     EVOLUTION OF THE SOIL WATER CONTENT
!               -----------------------------------
!
!*       7.     EFFECT OF MELTING/FREEZING ON SOIL ICE AND LIQUID WATER CONTENTS
!               ----------------------------------------------------------------
!
!*       8.     DRAINAGE FROM THE DEEP SOIL
!               ---------------------------
!
!*      9.     RUN-OFF 
!               -------
!                                     when the soil water exceeds saturation, 
!                                     there is fast-time-response runoff
!
!
! -----------------------------------------------------------------
! Time splitting parameter for *very large time steps* since Richard
! and/or soil freezing equations are very non-linear 
! NOTE for NWP/GCM type applications, the time step is generally not split
! (usually just for offline applications with a time step on order of 
! 15 minutes to an hour for example)
! ------------------------------------------------------------------
!
INDT = 1
IF(PTSTEP>=ZTIMEMAX)THEN
  INDT = MAX(2,NINT(PTSTEP/ZTIMEMAX))
ENDIF
!
ZTSTEP  = PTSTEP/REAL(INDT)
!
! ------------------------------------------------------------------
! The values for the two coefficients (multiplied by VEG and LAI) 
! in the expression below are from 
! Giard and Bazile (2000), Mon. Wea. Rev.: they model the effect of insolation due to
! vegetation cover. This used by both 'DEF' (code blocks 3.-4.) and 'DIF' options.
! ------------------------------------------------------------------
!
WHERE(PLAI(:)/=XUNDEF .AND. PVEG(:)/=0.)
    ZKSFC_IVEG(:) = (1.0-ZINSOLFRZ_VEG*PVEG(:)) * (1.0-(PLAI(:)/ZINSOLFRZ_LAI))
ELSEWHERE
    ZKSFC_IVEG(:) = 1.0 ! No vegetation
ENDWHERE
!
!
ZWGI0 (:,:) = PWGI(:,:) ! save initial ice content before phase changes and sublimation
!
IF (HISBA=='DIF') THEN                
!
  INI = SIZE(PD_G(:,:),1)
  INL = MAXVAL(KWG_LAYER(:))
!
! Initialize some field
! ---------------------
!
  ZPS(:,:)=XUNDEF
  ZTI(:,:)=XUNDEF
  DO JL=1,INL
     DO JJ=1,INI
        IDEPTH=KWG_LAYER(JJ)
        IF(JL<=IDEPTH)THEN
          ZPS(JJ,JL) = PPS(JJ)
          ZTI(JJ,JL) = MIN(XTT,PTG(JJ,JL))
        ENDIF
     ENDDO
  ENDDO
!
! Compute specific humidity at saturation for the vapor conductivity
! ------------------------------------------------------------------
!
  ZQSAT (:,:) = QSAT (PTG(:,:),ZPS(:,:),KWG_LAYER(:),INL)
  ZQSATI(:,:) = QSATI(ZTI(:,:),ZPS(:,:),KWG_LAYER(:),INL)
!
! Soil water sink terms: convert from (W m-2) and (kg m-2 s-1) to (m s-1)
! ------------------------------------------------------------------
!
  ZPG     (:) =  ZPG    (:)        / XRHOLW
  ZEVAPCOR(:) = ZEVAPCOR(:)        / XRHOLW
  ZLEG    (:) =  ZLEG   (:)        /(XRHOLW*PLVTT(:))
  ZLETR   (:) = (ZLETR  (:)/ZF2(:))/(XRHOLW*PLVTT(:))
  ZLEGI   (:) = ZLEGI   (:)        /(XRHOLW*PLSTT(:))
!
  DO JDT = 1,INDT
!                      
     CALL HYDRO_SOILDIF(HRUNOFF, ZTSTEP,                            &
                PBCOEF, PWSAT, PCONDSAT, PMPOTSAT, PWFC,            &
                PD_G, PDZG, PDZDIF, ZPG, ZLETR, ZLEG, ZEVAPCOR,     &
                PF2WGHT, PWG, PWGI, PTG, PPS, ZQSAT, ZQSATI,        &
                ZDRAIN, ZHORTON, KWG_LAYER, INL,                    &
                PTOPQS, ZQSB, PFWTD, PWTD                           )
!
     CALL ICE_SOILDIF(ZTSTEP, PTAUICE, ZKSFC_IVEG, ZLEGI,    &
                     PSOILHCAPZ, PWSAT, PMPOTSAT, PBCOEF,    &
                     PTG, PWGI, PWG, KWG_LAYER,              &
                     PDZG,  ZWGI_EXCESS                      )
!
     PDRAIN (:)  = PDRAIN (:) + (ZDRAIN(:)+ZQSB(:)+ZWGI_EXCESS(:))/REAL(INDT)
     PQSB   (:)  = PQSB   (:) + ZQSB   (:)/REAL(INDT)
     PHORTON(:)  = PHORTON(:) + ZHORTON(:)/REAL(INDT)
!
! Output diagnostics:
! Compute latent heating from phase change only in surface layer and total soil column,
! then adjust surface and total soil heat content to maintain balance.
!
     PDELPHASEG_SFC(:)    = (PWGI(:,1)-ZWGI0(:,1))*(XLMTT*XRHOLW/PTSTEP)*PDZG(:,1) + ZLEGI(:)*(XRHOLW*XLSTT)
     PDELPHASEG(:)        = PDELPHASEG_SFC(:)
     DO JL=2,INL
        DO JJ=1,INI
           PDELPHASEG(JJ) = PDELPHASEG(JJ) + (PWGI(JJ,JL)-ZWGI0(JJ,JL))*(XLMTT*XRHOLW/PTSTEP)*PDZG(JJ,JL)
        ENDDO
     ENDDO
     PDELHEATG_SFC(:)     = PDELHEATG_SFC(:) + PDELPHASEG_SFC(:)
     PDELHEATG(:)         = PDELHEATG(:)     + PDELPHASEG(:)

  ENDDO
!
ELSE
!
  DO JDT = 1,INDT
!
!   Only layer 1 and 2 are used for soil freezing (ZWG3 not used)
    CALL ICE_SOILFR(HSNOW_ISBA, HSOILFRZ, ZTSTEP, ZKSFC_IVEG, PCG, PCT,  &
                    PPSNG, PFFG, PTAUICE, ZDWGI1, ZDWGI2, PWSAT,         &
                    PMPOTSAT, PBCOEF, PD_G, PTG, PWGI, PWG               )
!
    CALL HYDRO_SOIL(HISBA,                                           &
                    ZTSTEP,                                          &
                    ZLETR, ZLEG, ZPG, ZEVAPCOR,                      &
                    PWDRAIN,                                         &
                    PC1, PC2, PC3, PC4B, PC4REF, PWGEQ,              &
                    PD_G(:,2), ZDG3, ZWSAT_AVG, ZWFC_AVG,            &
                    ZDWGI1, ZDWGI2, ZLEGI, PD_G(:,1), PCG, PCT,      &
                    PTG(:,1), PTG(:,2),                              &
                    PWG(:,1), PWG(:,2), ZWG3(:),                     &
                    PWGI(:,1), PWGI(:,2),                            &
                    ZRUNOFF,ZDRAIN,HKSAT,ZWWILT_AVG                  )
!
    PDRAIN (:)  = PDRAIN (:) + ZDRAIN (:)/REAL(INDT)
    PRUNOFF(:)  = PRUNOFF(:) + ZRUNOFF(:)/REAL(INDT)
!    
  ENDDO
!
! Output diagnostics:
! Compute latent heating from phase change only in surface layer and total soil column,
! then adjust surface and total soil heat content to maintain balance.
!
  PDELPHASEG_SFC(:)    = (PWGI(:,1)-ZWGI0(:,1))*(XLMTT*XRHOLW/PTSTEP)*PD_G(:,1) + ZLEGI(:)
  PDELPHASEG(:)        = (PWGI(:,2)-ZWGI0(:,2))*(XLMTT*XRHOLW/PTSTEP)*PD_G(:,2)
  PDELHEATG_SFC(:)     = PDELHEATG_SFC(:) + PDELPHASEG_SFC(:)
  PDELHEATG(:)         = PDELHEATG(:)     + PDELPHASEG(:)
!
  IF (HISBA == '3-L') PWG(:,3) = ZWG3(:)
!
#ifdef TOPD
  IF (LCOUPL_TOPD) THEN
    !runoff topo cumule (kg/m²)
    DO JJ=1,SIZE(NMASKT_PATCH)
      IF  (NMASKT_PATCH(JJ)/=0) THEN
        IF ( XATOP(NMASKT_PATCH(JJ))/=XUNDEF) THEN
          XRUNOFF_TOP(NMASKT_PATCH(JJ)) = XRUNOFF_TOP(NMASKT_PATCH(JJ)) + &
                                          (PRUNOFF(JJ)+ PHORTON(JJ))*XATOP(NMASKT_PATCH(JJ))*PTSTEP
          IF (HRUNOFF=='TOPD') THEN
            XRUNOFF_TOP(NMASKT_PATCH(JJ)) = XRUNOFF_TOP(NMASKT_PATCH(JJ)) + ZDUNNE(JJ)*PTSTEP
          ELSE
            ! ZDUNNE contains only saturated pixels on mesh so only catchment
            XRUNOFF_TOP(NMASKT_PATCH(JJ)) = XRUNOFF_TOP(NMASKT_PATCH(JJ)) + ZDUNNE(JJ)*XATOP(NMASKT_PATCH(JJ))*PTSTEP
          ENDIF  
        ENDIF
      ENDIF
      ! ZDUNNE concerns all the mesh so not only catchment =>*XATOP
    ENDDO
  ENDIF
#endif
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
! Add sub-grid surface and subsurface runoff to saturation excess:
!
PRUNOFF(:) = PRUNOFF(:) + ZDUNNE(:) + PHORTON(:)
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('HYDRO',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE HYDRO
