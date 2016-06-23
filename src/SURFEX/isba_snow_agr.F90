!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE ISBA_SNOW_AGR( HSNOW_ISBA, OMEB,                                &
                         PEXNS, PEXNA, PTA, PQA, PZREF, PUREF, PDIRCOSZW, PVMOD, &
                         PZ0EFF, PZ0, PZ0H, PRR, PSR,                            &
                         PEMIS, PALB, PPSN, PPSNG, PPSNV,                        &
                         PRN, PH, PLE, PLEI, PLEG, PLEGI, PLEV, PLES, PLER,      &
                         PLETR, PEVAP, PSUBL, PGFLUX, PLVTT, PLSTT,              &
                         PUSTAR,                                                 &
                         PLES3L, PLEL3L, PEVAP3L,                                &
                         PSWNET_V, PSWNET_G, PLWNET_V, PLWNET_G, PH_V, PH_G,     &
                         PLEV_V_C, PLETR_V_C, PLES_V_C,                          &
                         PQS3L, PALB3L,                                          &
                         PRNSNOW, PHSNOW, PHPSNOW,                               &
                         PSWNETSNOW, PSWNETSNOWS, PLWNETSNOW,                    &
                         PGFLUXSNOW, PGSFCSNOW, PUSTARSNOW,                      &
                         PZGRNDFLUX, PFLSN_COR, PGRNDFLUX, PLESL,                &
                         PEMISNOW,                                               &
                         PSNOWTEMP, PTS_RAD, PTS, PRI, PQS, PHU,                 &
                         PCD, PCDN, PCH, PSNOWHMASS,                             &
                         PRN_ISBA, PH_ISBA, PLEG_ISBA, PLEGI_ISBA, PLEV_ISBA,    &
                         PLETR_ISBA, PUSTAR_ISBA, PLER_ISBA, PLE_ISBA,           &
                         PLEI_ISBA, PGFLUX_ISBA, PMELTADV, PTG,                  &
                         PEMIST, PALBT, PLE_FLOOD, PLEI_FLOOD,                   &
                         PFFG, PFFV, PFF, PPALPHAN, PTC, OMEB_LITTER, PLELITTER, &
                         PLELITTERI                                              )
!     ##########################################################################
!
!
!!****  *ISBA_SNOW_AGR* aggregates snow free and snow fluxes
!!
!!    PURPOSE
!!    -------
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
!!      V. Masson           * Meteo-France *
!!      (following A. Boone)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/03/95 
!!      B. Decharme 01/2009  Floodplains 
!!      B. Decharme 01/2010  Effective surface temperature (for diag)
!!      B. Decharme 09/2012  Bug total sublimation flux: no PLESL
!!      B. Decharme 04/2013  Bug wrong radiative temperature
!!                           Sublimation diag flux
!!                           Qs for 3l or crocus (needed for coupling with atm)
!!      A. Boone    11/2014  MEB
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!              -------------------------
!
!
!* general variables
!  -----------------
!
 CHARACTER(LEN=*),     INTENT(IN)  :: HSNOW_ISBA ! 'DEF' = Default F-R snow scheme
!                                               !         (Douville et al. 1995)
!                                               ! '3-L' = 3-L snow scheme (option)
!                                               !         (Boone and Etchevers 2000)
!
LOGICAL,              INTENT(IN)  :: OMEB       ! True = patch with multi-energy balance 
!                                               ! False = patch with classical ISBA 
!
!* surface and atmospheric parameters
!  ----------------------------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PEXNS     ! Exner function at the surface
REAL, DIMENSION(:), INTENT(IN)  :: PEXNA     ! Exner function
REAL, DIMENSION(:), INTENT(IN)  :: PTA       ! air temperature
REAL, DIMENSION(:), INTENT(IN)  :: PQA       ! air specific humidity
REAL, DIMENSION(:), INTENT(IN)  :: PZREF     ! reference height of the first atmospheric level
REAL, DIMENSION(:), INTENT(IN)  :: PUREF     ! reference height of the wind
REAL, DIMENSION(:), INTENT(IN)  :: PDIRCOSZW ! Cosinus of the angle between the normal to the surface and the vertical
REAL, DIMENSION(:), INTENT(IN)  :: PVMOD     ! module of the horizontal wind
REAL, DIMENSION(:), INTENT(IN)  :: PZ0EFF    ! effective roughness length
REAL, DIMENSION(:), INTENT(IN)  :: PZ0       ! roughness length for momentum
REAL, DIMENSION(:), INTENT(IN)  :: PZ0H      ! roughness length for heat
REAL, DIMENSION(:), INTENT(IN)  :: PRR       ! Rain rate (in kg/m2/s)
REAL, DIMENSION(:), INTENT(IN)  :: PSR       ! Snow rate (in kg/m2/s)
!
!* surface parameters
!  ------------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PALB       ! albedo 
REAL, DIMENSION(:), INTENT(IN)  :: PEMIS      ! emissivity
!  'D95'     : represents aggregated (snow + flood + snow-flood-free) albedo and emissivity
!  '3-L'     : represents                    flood + snow-flood-free  albedo and emissivity
!  'MEB+3-L' : represents aggregated (snow + flood + snow-flood-free) albedo and emissivity
!
!
!* snow fractions
!  --------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PPSN       ! fraction of the grid covered
!                                             ! by snow
REAL, DIMENSION(:), INTENT(IN)  :: PPSNG      ! fraction of the the bare
!                                             ! ground covered by snow
REAL, DIMENSION(:), INTENT(IN)  :: PPSNV      ! fraction of the the veg.
!                                             ! covered by snow
REAL, DIMENSION(:), INTENT(IN)  :: PPALPHAN   ! fraction of the the explicit veg.
!                                             ! canopy buried by snow
!
!
!* ISBA-SNOW3L variables/parameters:
!  ---------------------------------
!
! Prognostic variables:
!
REAL, DIMENSION(:),   INTENT(IN) :: PALB3L      ! Snow albedo
REAL, DIMENSION(:),   INTENT(IN) :: PQS3L       ! Surface humidity
! 
! Diagnostics:
!
REAL, DIMENSION(:), INTENT(IN)    :: PZGRNDFLUX ! snow/soil-biomass interface flux (W/m2)
REAL, DIMENSION(:), INTENT(IN)    :: PFLSN_COR  ! snow/soil-biomass correction flux (W/m2)
!
REAL, DIMENSION(:), INTENT(INOUT) :: PGRNDFLUX  ! snow/soil-biomass interface flux (W/m2)
!
REAL, DIMENSION(:), INTENT(INOUT) :: PHPSNOW    ! heat release from rainfall (W/m2)
REAL, DIMENSION(:), INTENT(INOUT) :: PSNOWHMASS ! snow heat content change from mass changes (J/m2)
REAL, DIMENSION(:), INTENT(INOUT) :: PRNSNOW    ! net radiative flux from snow (W/m2)
REAL, DIMENSION(:), INTENT(INOUT) :: PSWNETSNOW ! net shortwave snow radiative flux (W/m2)
REAL, DIMENSION(:), INTENT(INOUT) :: PSWNETSNOWS! net shortwave snow radiative flux in sfc layer (W/m2)
REAL, DIMENSION(:), INTENT(INOUT) :: PLWNETSNOW ! net longwave snow radiative flux (W/m2)
REAL, DIMENSION(:), INTENT(INOUT) :: PHSNOW     ! sensible heat flux from snow (W/m2)
REAL, DIMENSION(:), INTENT(INOUT) :: PGFLUXSNOW ! net heat flux from snow (W/m2)
REAL, DIMENSION(:), INTENT(INOUT) :: PGSFCSNOW  ! heat flux from snow sfc to sub sfc layers (W/m2)
REAL, DIMENSION(:), INTENT(INOUT) :: PSWNET_V   ! net shortwave radiation of vegetation canopy 
REAL, DIMENSION(:), INTENT(INOUT) :: PSWNET_G   ! net shortwave radiation of (below snow) surface
REAL, DIMENSION(:), INTENT(INOUT) :: PLWNET_V   ! net longwave radiation of vegetation canopy 
REAL, DIMENSION(:), INTENT(INOUT) :: PLWNET_G   ! net longwave radiation of (below snow) surface
REAL, DIMENSION(:), INTENT(IN)    :: PUSTARSNOW ! friction velocity
REAL, DIMENSION(:), INTENT(OUT)   :: PLESL      ! Evaporation (liquid) from wet snow (W/m2)
REAL, DIMENSION(:), INTENT(IN)    :: PEMISNOW   ! snow surface emissivity
REAL, DIMENSION(:), INTENT(OUT)   :: PTS_RAD    ! effective radiative temperature 
!                                                 of the natural surface (K)
REAL, DIMENSION(:), INTENT(OUT)   :: PTS        ! effective surface temperature 
REAL, DIMENSION(:), INTENT(IN)    :: PSNOWTEMP  ! snow layer sfc temperature (K)
REAL, DIMENSION(:), INTENT(IN)    :: PLES3L     ! sublimation from ISBA-ES(3L)
REAL, DIMENSION(:), INTENT(IN)    :: PLEL3L     ! evaporation heat flux of water in the snow (W/m2)
REAL, DIMENSION(:), INTENT(INOUT) :: PEVAP3L    ! evaporation flux over snow from ISBA-ES (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN)    :: PLVTT, PLSTT    
!
!
! Prognostic variables:
!
REAL, DIMENSION(:),   INTENT(IN)  :: PTG              ! soil sfc layer average temperatures    (K)
REAL, DIMENSION(:),   INTENT(IN)  :: PTC              ! canopy air temperature                 (K)
!
!
!* diagnostic variables
!  --------------------
!
REAL, DIMENSION(:), INTENT(INOUT) :: PEMIST   ! total surface emissivity
REAL, DIMENSION(:), INTENT(INOUT) :: PALBT    ! total surface albedo
!
!* surface fluxes
!  --------------
!
REAL, DIMENSION(:), INTENT(IN)    :: PLEV_V_C ! MEB: total evapotranspiration (no snow) from 
!                                             !  vegetation canopy overstory [W/m2]
REAL, DIMENSION(:), INTENT(IN)    :: PLES_V_C ! MEB: total (intercepted) snow sublimation from 
!                                             !  vegetation canopy overstory [W/m2]
REAL, DIMENSION(:), INTENT(IN)    :: PLETR_V_C! MEB: transpiration from overstory canopy 
!                                             !  vegetation [W/m2]
REAL, DIMENSION(:), INTENT(INOUT) :: PRN      ! net radiation
REAL, DIMENSION(:), INTENT(INOUT) :: PH       ! sensible heat flux
REAL, DIMENSION(:), INTENT(INOUT) :: PH_V     ! sensible heat flux from explicit veg canopy
REAL, DIMENSION(:), INTENT(INOUT) :: PH_G     ! sensible heat flux from surface (below snow)
REAL, DIMENSION(:), INTENT(INOUT) :: PLE      ! total latent heat flux
REAL, DIMENSION(:), INTENT(OUT)   :: PLEI     ! sublimation latent heat flux
REAL, DIMENSION(:), INTENT(INOUT) :: PLEGI    ! latent heat of sublimation over frozen soil
REAL, DIMENSION(:), INTENT(INOUT) :: PLEG     ! latent heat of evaporation
REAL, DIMENSION(:), INTENT(IN)    :: PLELITTERI! sublimation of water in litter reservoir
REAL, DIMENSION(:), INTENT(IN)    :: PLELITTER !sublimation of water in litter reservoir
LOGICAL, INTENT(IN)               :: OMEB_LITTER !True = litter option activated
!                                             ! over the ground
REAL, DIMENSION(:), INTENT(INOUT) :: PLEV     ! latent heat of evaporation
!                                             ! over the vegetation
REAL, DIMENSION(:), INTENT(INOUT) :: PLES     ! latent heat of sublimation
!                                             ! over the snow
REAL, DIMENSION(:), INTENT(INOUT) :: PLER     ! latent heat of the fraction
!                                             ! delta of water retained on the
!                                             ! foliage of the vegetation
REAL, DIMENSION(:), INTENT(INOUT) :: PLETR    ! evapotranspiration of the rest
!                                             ! of the vegetation
REAL, DIMENSION(:), INTENT(INOUT) :: PEVAP    ! total evaporative flux (kg/m2/s)
REAL, DIMENSION(:), INTENT(INOUT) :: PSUBL    ! sublimation flux (kg/m2/s)
REAL, DIMENSION(:), INTENT(INOUT) :: PGFLUX   ! flux through the ground
REAL, DIMENSION(:), INTENT(INOUT) :: PUSTAR   ! friction velocity
REAL, DIMENSION(:), INTENT(INOUT) :: PMELTADV ! advection heat flux from snowmelt (W/m2)
!
! The following surface fluxes are from snow-free portion of grid
! box when the ISBA-ES option is ON. Otherwise, they are equal
! to the same variables without the _ISBA extension.
!
REAL, DIMENSION(:), INTENT(OUT) :: PRN_ISBA   ! net radiation
REAL, DIMENSION(:), INTENT(OUT) :: PH_ISBA    ! sensible heat flux
REAL, DIMENSION(:), INTENT(OUT) :: PLEG_ISBA  ! latent heat of evaporation (ground)
REAL, DIMENSION(:), INTENT(OUT) :: PLEGI_ISBA ! latent heat of sublimation (ground)
REAL, DIMENSION(:), INTENT(OUT) :: PLEV_ISBA  ! latent heat of evaporation (vegetation)
REAL, DIMENSION(:), INTENT(OUT) :: PLETR_ISBA ! latent heat of evaporation (transpiration)
REAL, DIMENSION(:), INTENT(OUT) :: PUSTAR_ISBA! friction velocity
REAL, DIMENSION(:), INTENT(OUT) :: PLER_ISBA  ! latent heat of evaporation (plant interception)
REAL, DIMENSION(:), INTENT(OUT) :: PLE_ISBA   ! total latent heat flux 
REAL, DIMENSION(:), INTENT(OUT) :: PLEI_ISBA  ! sublimation latent heat flux 
REAL, DIMENSION(:), INTENT(OUT) :: PGFLUX_ISBA! flux through the ground
!
REAL, DIMENSION(:), INTENT(IN)    :: PFFG,PFFV,PFF
REAL, DIMENSION(:), INTENT(INOUT) :: PLE_FLOOD, PLEI_FLOOD ! Flood evaporation
!
REAL, DIMENSION(:), INTENT(INOUT) :: PRI       ! grid-area Ridcharson number
REAL, DIMENSION(:), INTENT(INOUT) :: PQS       ! grid-area Surface humidity
REAL, DIMENSION(:), INTENT(INOUT) :: PHU       ! grid-area near surface humidity
REAL, DIMENSION(:), INTENT(INOUT) :: PCH       ! grid-area drag coefficient for heat
REAL, DIMENSION(:), INTENT(INOUT) :: PCD       ! grid-area drag coefficient for momentum
REAL, DIMENSION(:), INTENT(INOUT) :: PCDN      ! grid-area neutral drag coefficient for momentum
!
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PTA))       :: ZWORK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_SNOW_AGR',0,ZHOOK_HANDLE)
!
ZWORK(:) = 0.
!
IF(OMEB)THEN
!
! Snow free (ground-based snow) diagnostics: canopy and ground blended (W m-2):
! NOTE that the effects of snow cover *fraction* are implicitly *included* in these fluxes 
! so do NOT multiply by snow fraction.

   PRN_ISBA(:)    = PSWNET_V(:) + PSWNET_G(:) + PLWNET_V(:) + PLWNET_G(:)
   PH_ISBA(:)     = PH_V(:) + PH_G(:)
   IF (OMEB_LITTER) THEN
    PLEG_ISBA(:)   = PLELITTER(:)
    PLEGI_ISBA(:)  = PLELITTERI(:)
    PLEG(:)        = PLELITTER(:)
    PLEGI(:)       = PLELITTERI(:)
   ELSE
    PLEG_ISBA(:)   = PLEG(:)
    PLEGI_ISBA(:)  = PLEGI(:)
   ENDIF
   PLEI_ISBA(:)   = PLEGI(:) + PLEI_FLOOD(:) + PLES(:) + PLES_V_C(:)
   PLEV_ISBA(:)   = PLEV_V_C(:)
   PLETR_ISBA(:)  = PLETR_V_C(:) 
   PUSTAR_ISBA(:) = PUSTAR(:)                  ! NOTE for now, this is same as total Ustar (includes snow)
! LER does not include intercepted snow sublimation
   PLER_ISBA(:)   = PLEV_V_C(:) - PLETR_V_C(:) 
! LE includes intercepted snow sublimation
   PLE_ISBA(:)    = PLEG_ISBA(:) + PLEGI_ISBA(:) + PLEV_ISBA(:) + PLE_FLOOD(:) + PLES_V_C(:) + PLEI_FLOOD(:)
   PGFLUX_ISBA(:) = PRN_ISBA(:) - PH_ISBA(:) - PLE_ISBA(:)
!
   PEMIST(:)      = PEMIS(:)
!
! Effective surface temperature (for diag): for MEB:

   ZWORK(:)       =  PPALPHAN(:)*PPSN(:)
   PTS(:)         = (1.0 - ZWORK(:))*PTC(:) + ZWORK(:)*PSNOWTEMP(:)
!
! Total heat FLUX into snow/soil/vegetation surface:
!
   PGFLUX(:)      = PRN(:) - PH(:) - PLE(:) + PHPSNOW(:) 
!
ELSE
!
! * 2. Using an explicit snow scheme option with composite soil/veg ISBA:
!      ------------------------------------------------------------------
!
   IF(HSNOW_ISBA == '3-L' .OR. HSNOW_ISBA == 'CRO')THEN
!
!     Save fluxes from Force-Restore snow/explicit snow-free
!     portion of grid box (vegetation/soil):
!
      PRN_ISBA(:)    = PRN(:)
      PH_ISBA(:)     = PH(:)
      PLEG_ISBA(:)   = PLEG(:)
      PLEGI_ISBA(:)  = PLEGI(:)
      PLEV_ISBA(:)   = PLEV(:)
      PLETR_ISBA(:)  = PLETR(:)
      PUSTAR_ISBA(:) = PUSTAR(:)
      PLER_ISBA(:)   = PLER(:) 
      PLE_ISBA(:)    = PLE(:)
      PGFLUX_ISBA(:) = PGFLUX(:)
!  
      PLEI_ISBA(:)   = PLEGI(:)+PLEI_FLOOD(:)+PLES(:)
!
!     Effective surface temperature (for diag):
!
      PTS(:)       = (1.-PPSN(:))*PTG(:)+PPSN(:)*PSNOWTEMP(:)
!
!     Effective surface radiating temperature:
!
      PALBT (:)    = PALB (:)*(1.-PPSN(:)) + PPSN(:)*PALB3L  (:)
      PEMIST(:)    = PEMIS(:)*(1.-PPSN(:)) + PPSN(:)*PEMISNOW(:)
!  
      PTS_RAD(:)   = ( ((1.-PPSN(:))*PEMIS   (:)*PTG      (:)**4 +   &
                            PPSN(:) *PEMISNOW(:)*PSNOWTEMP(:)**4     &
                            )/PEMIST(:) )**(0.25)  
!
!     Calculate actual fluxes from snow-free natural
!     portion of surface: NET flux from surface is the sum of
!     fluxes from snow free and snow covered portions
!     of natural portion of grid box when *ISBA-ES* in force.
!     when NOT in use, then these fluxes equal those above.
!
      PRN(:)       = (1.-PPSN(:))  * PRN(:)   + PPSN(:) * PRNSNOW(:)
      PH(:)        = (1.-PPSN(:))  * PH(:)    + PPSN(:) * PHSNOW(:)
!  
      PLEG(:)      = (1.-PPSNG(:)-PFFG(:)) * PLEG(:)  
      PLEGI(:)     = (1.-PPSNG(:)-PFFG(:)) * PLEGI(:)  
      PLEV(:)      = (1.-PPSNV(:)-PFFV(:)) * PLEV(:)   
      PLETR(:)     = (1.-PPSNV(:)-PFFV(:)) * PLETR(:)  
      PLER(:)      = (1.-PPSNV(:)-PFFV(:)) * PLER(:)  
!
!     Total evapotranspiration flux (kg/m2/s):
!
      PEVAP(:)     = (PLEV(:) + PLEG(:))/PLVTT(:) + PLEGI(:)/PLSTT(:) + PLE_FLOOD(:)/PLVTT(:) + &
                      PLEI_FLOOD(:)/PLSTT(:) + PPSN(:) * PEVAP3L(:)
!
!     ISBA-ES/SNOW3L fluxes:
!
      PLES(:)       =                           PPSN(:) * PLES3L(:)
      PLESL(:)      =                           PPSN(:) * PLEL3L(:)
      PRNSNOW(:)    =                           PPSN(:) * PRNSNOW(:)
      PHSNOW(:)     =                           PPSN(:) * PHSNOW(:)
      PGFLUXSNOW(:) =                           PPSN(:) * PGFLUXSNOW(:)
      PSNOWHMASS(:) =                           PPSN(:) * PSNOWHMASS(:)  ! (J m-2)
      PHPSNOW(:)    =                           PPSN(:) * PHPSNOW(:)
      PGSFCSNOW(:)  =                           PPSN(:) * PGSFCSNOW(:)
      PSWNETSNOW(:) =                           PPSN(:) * PSWNETSNOW(:)
      PSWNETSNOWS(:)=                           PPSN(:) * PSWNETSNOWS(:)
      PEVAP3L(:)    =                           PPSN(:) * PEVAP3L(:)
!
!     Total heat flux between snow and soil
!
      PGRNDFLUX(:) =                            PPSN(:) * (PZGRNDFLUX(:)+PFLSN_COR(:))
      PMELTADV(:)  =                            PPSN(:) * PMELTADV(:)
!
!     Total evaporative flux (W/m2) :
!
      PLE(:)       = PLEG(:) + PLEV(:) + PLES(:) + PLESL(:) + PLEGI(:) + PLE_FLOOD(:) + PLEI_FLOOD(:)
!
!     Total sublimation flux (W/m2) :
!
      PLEI(:)      = PLES(:) + PLEGI(:) + PLEI_FLOOD(:)
!
!     Total sublimation flux (kg/m2/s):
!
      PSUBL(:)     = PLEI(:)/PLSTT(:)
!
!     Total FLUX into snow/soil/vegetation surface:
!
      PGFLUX(:)    = PRN(:) - PH(:) - PLE(:) + PHPSNOW(:)  
!
!     surface humidity:
!
      PQS(:)       = (1.-PPSN(:))  * PQS(:)   + PPSN(:) * PQS3L(:)
!
!     near-surface humidity :
!  
      PHU(:)       = (1.-PPSN(:))  * PHU(:)   + PPSN(:)
!
!     Momentum fluxes:
!
      PUSTAR(:)    = SQRT( (1.-PPSN(:))  * PUSTAR(:)**2  + PPSN(:) * PUSTARSNOW(:)**2 )
!
!     Richardson number and Drag coeff:
!
      CALL COMPUT_RI_DRAG
!
   ELSE
!
      PTS    (:)  = PTG  (:)
      PTS_RAD(:)  = PTG  (:)
      PALBT  (:)  = PALB (:)
      PEMIST (:)  = PEMIS(:)
!  
!     Total sublimation flux (W/m2) :
      PLEI   (:)  = PLES(:) + PLEGI(:) + PLEI_FLOOD(:)
!
!     Total sublimation flux (kg/m2/s):
      PSUBL  (:)  = PLEI(:)/PLSTT(:)
!
   ENDIF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('ISBA_SNOW_AGR',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
 CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE COMPUT_RI_DRAG
!
USE MODD_SURF_ATM, ONLY : LDRAG_COEF_ARP, LRRGUST_ARP,   &
                          XRRSCALE, XRRGAMMA, XUTILGUST  
!
USE MODI_SURFACE_RI
USE MODI_SURFACE_AERO_COND
USE MODI_SURFACE_CD
USE MODI_SURFACE_CDCH_1DARP
USE MODI_WIND_THRESHOLD
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PTA)) :: ZFP, ZRRCOR, ZVMOD, ZAC, ZRA
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_SNOW_AGR:COMPUT_RI_DRAG',0,ZHOOK_HANDLE)
!
! * Richardson number
!
 CALL SURFACE_RI(PTS, PQS, PEXNS, PEXNA, PTA, PQA,  &
                PZREF, PUREF, PDIRCOSZW, PVMOD, PRI)  
!
! * Wind check
!
ZVMOD = WIND_THRESHOLD(PVMOD,PUREF)
!
! * Drag coefficient for heat and momentum
!
IF (LDRAG_COEF_ARP) THEN
   CALL SURFACE_CDCH_1DARP(PZREF, PZ0EFF, PZ0H, ZVMOD, PTA, PTG, &
                             PQA, PQS, PCD, PCDN, PCH              )
ELSE
   CALL SURFACE_AERO_COND(PRI, PZREF, PUREF, ZVMOD, PZ0, PZ0H, ZAC, ZRA, PCH)
   CALL SURFACE_CD(PRI, PZREF, PUREF, PZ0EFF, PZ0H, PCD, PCDN)
ENDIF
!
IF (LRRGUST_ARP) THEN
   ZFP(:)=MAX(0.0,PRR(:)+PSR(:))
   ZRRCOR(:)=SQRT(1.0+((((ZFP(:)/(ZFP(:)+XRRSCALE))**XRRGAMMA)*XUTILGUST)**2) &
       /(PCD(:)*ZVMOD(:)**2))  
   PCD  = PCD  * ZRRCOR
   PCH  = PCH  * ZRRCOR
   PCDN = PCDN * ZRRCOR
ENDIF
!
IF (LHOOK) CALL DR_HOOK('ISBA_SNOW_AGR:COMPUT_RI_DRAG',1,ZHOOK_HANDLE)
!
END SUBROUTINE COMPUT_RI_DRAG
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ISBA_SNOW_AGR
