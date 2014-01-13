!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE ISBA_SNOW_AGR( HSNOW_ISBA,                                    &
                         PEMIS, PALB,                                            &
                         PPSN, PPSNG, PPSNV,                                     &
                         PRN, PH, PLE, PLEI, PLEG, PLEGI, PLEV, PLES, PLER,      &
                         PLETR, PEVAP, PGFLUX, PLVTT, PLSTT,                     &
                         PUSTAR,                                                 &
                         PLES3L, PLEL3L, PEVAP3L,                                &
                         PRI3L, PALB3L,                                          &
                         PRNSNOW, PHSNOW,  PHPSNOW,                              &
                         PGFLUXSNOW, PUSTARSNOW,                                 &
                         PGRNDFLUX, PLESL,                                       &
                         PEMISNOW,                                               &
                         PSNOWTEMP, PTS_RAD, PTS, PRI, PSNOWHMASS,               &
                         PRN_ISBA, PH_ISBA, PLEG_ISBA, PLEGI_ISBA, PLEV_ISBA,    &
                         PLETR_ISBA, PUSTAR_ISBA, PLER_ISBA, PLE_ISBA,           &
                         PLEI_ISBA, PGFLUX_ISBA, PMELTADV, PTG,                  &
                         PEMIST, PALBT, PLE_FLOOD, PLEI_FLOOD, PFFG,             &
                         PFFV, PFF                                               )  
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
!!	V. Masson           * Meteo-France *
!!      (following A. Boone)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/03/95 
!!      B. Decharme 01/2009  Floodplains 
!!      B. Decharme 01/2010  Effective surface temperature (for diag)
!!      B. Decharme 09/2012  Bug total sublimation flux: no PLESL
!!      B. Decharme 04/2013  Bug wrong radiative temperature

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
!* surface parameters
!  ------------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PALB       ! albedo 
REAL, DIMENSION(:), INTENT(IN)  :: PEMIS      ! emissivity
!  'D95' : they represent aggregated (snow + flood + snow-flood-free) albedo and emissivity
!  '3-L' : they represent                    flood + snow-flood-free  albedo and emissivity
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
!
!
!* ISBA-SNOW3L variables/parameters:
!  ---------------------------------
!
! Prognostic variables:
!
REAL, DIMENSION(:),   INTENT(IN) :: PALB3L      ! Snow albedo
REAL, DIMENSION(:),   INTENT(IN) :: PRI3L       ! Snow Ridcharson number
! 
! Diagnostics:
!
REAL, DIMENSION(:), INTENT(INOUT) :: PGRNDFLUX  ! snow/soil-biomass interface flux (W/m2)
!
REAL, DIMENSION(:), INTENT(INOUT) :: PHPSNOW    ! heat release from rainfall (W/m2)
REAL, DIMENSION(:), INTENT(INOUT) :: PSNOWHMASS ! snow heat content change from mass changes (J/m2)
REAL, DIMENSION(:), INTENT(INOUT) :: PRNSNOW    ! net radiative flux from snow (W/m2)
REAL, DIMENSION(:), INTENT(INOUT) :: PHSNOW     ! sensible heat flux from snow (W/m2)
REAL, DIMENSION(:), INTENT(INOUT) :: PGFLUXSNOW ! net heat flux from snow (W/m2)
REAL, DIMENSION(:), INTENT(IN)    :: PUSTARSNOW ! friction velocity
REAL, DIMENSION(:), INTENT(OUT)   :: PLESL      ! Evaporation (liquid) from wet snow (W/m2)
REAL, DIMENSION(:), INTENT(IN)    :: PEMISNOW   ! snow surface emissivity
REAL, DIMENSION(:), INTENT(OUT)   :: PTS_RAD    ! effective radiative temperature 
!                                                 of the natural surface (K)
REAL, DIMENSION(:), INTENT(OUT)   :: PTS        ! effective surface temperature 
REAL, DIMENSION(:,:), INTENT(IN)  :: PSNOWTEMP  ! snow layer temperatures (K)
REAL, DIMENSION(:), INTENT(IN)    :: PLES3L   ! sublimation from ISBA-ES(3L)
REAL, DIMENSION(:), INTENT(IN)    :: PLEL3L   ! evaporation heat flux of water in the snow (W/m2)
REAL, DIMENSION(:), INTENT(IN)    :: PEVAP3L  ! evaporation flux over snow from ISBA-ES (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN)    :: PLVTT, PLSTT    
!
!
! Prognostic variables:
!
REAL, DIMENSION(:,:), INTENT(IN) :: PTG                 ! soil layer average temperatures        (K)
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
REAL, DIMENSION(:), INTENT(INOUT) :: PRN      ! net radiation
REAL, DIMENSION(:), INTENT(INOUT) :: PH       ! sensible heat flux
REAL, DIMENSION(:), INTENT(INOUT) :: PLE      ! total latent heat flux
REAL, DIMENSION(:), INTENT(OUT)   :: PLEI     ! sublimation latent heat flux
REAL, DIMENSION(:), INTENT(INOUT) :: PLEGI    ! latent heat of sublimation over frozen soil
REAL, DIMENSION(:), INTENT(INOUT) :: PLEG     ! latent heat of evaporation
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
REAL, DIMENSION(:),   INTENT(OUT) :: PRI       ! Total Ridcharson number
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_SNOW_AGR',0,ZHOOK_HANDLE)
IF(HSNOW_ISBA == '3-L' .OR. HSNOW_ISBA == 'CRO')THEN
!
! Save fluxes from Force-Restore snow/explicit snow-free
! portion of grid box (vegetation/soil):
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
! Effective surface temperature (for diag):
!
  PTS(:)       = (1.-PPSN(:))*PTG(:,1)+PPSN(:)*PSNOWTEMP(:,1)
!
! Effective surface radiating temperature:
!
  PALBT (:)    = PALB (:)*(1.-PPSN(:)) + PPSN(:)*PALB3L  (:)
  PEMIST(:)    = PEMIS(:)*(1.-PPSN(:)) + PPSN(:)*PEMISNOW(:)
!  
  PTS_RAD(:)   = ( ((1.-PPSN(:))*PEMIS   (:)*PTG      (:,1)**4 +       &
                        PPSN(:) *PEMISNOW(:)*PSNOWTEMP(:,1)**4  )/PEMIST(:) )**(0.25)  
!
! Calculate actual fluxes from snow-free natural
! portion of surface: NET flux from surface is the sum of
! fluxes from snow free and snow covered portions
! of natural portion of grid box when *ISBA-ES* in force.
! when NOT in use, then these fluxes equal those above.
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
! Total evapotranspiration flux (kg/m2/s):
!
  PEVAP(:)     = (PLEV(:) + PLEG(:))/PLVTT(:) + PLEGI(:)/PLSTT(:) + PLE_FLOOD(:)/PLVTT(:) + &
                    PLEI_FLOOD(:)/PLSTT(:) + PPSN(:) * PEVAP3L(:)
!
! Momentum fluxes:
!
  PUSTAR(:)    = SQRT( (1.-PPSN(:))  * PUSTAR(:)**2  + PPSN(:) * PUSTARSNOW(:)**2 )
!
! ISBA-ES/SNOW3L fluxes:
!
  PLES(:)      =                            PPSN(:) * PLES3L(:)
  PLESL(:)     =                            PPSN(:) * PLEL3L(:)
  PRNSNOW(:)   =                            PPSN(:) * PRNSNOW(:)
  PHSNOW(:)    =                            PPSN(:) * PHSNOW(:)
  PGFLUXSNOW(:)=                            PPSN(:) * PGFLUXSNOW(:)
  PSNOWHMASS(:)=                            PPSN(:) * PSNOWHMASS(:)  ! (J m-2)
  PHPSNOW(:)   =                            PPSN(:) * PHPSNOW(:)

! Total heat flux between snow and soil
!
  PGRNDFLUX(:) =                            PPSN(:) * PGRNDFLUX(:) 
  PMELTADV(:)  =                            PPSN(:) * PMELTADV(:)
!
! Total evaporative flux (W/m2) :
!
  PLE(:)       = PLEG(:) + PLEV(:) + PLES(:) + PLESL(:) + PLEGI(:) + PLE_FLOOD(:) + PLEI_FLOOD(:)
!
! Total sublimation flux (W/m2) :
!
  PLEI(:)      = PLES(:) + PLEGI(:) + PLEI_FLOOD(:)
!
! Total FLUX into snow/soil/vegetation surface:
!
  PGFLUX(:)    = PRN(:) - PH(:) - PLE(:) + PHPSNOW(:) 
!
! Ridcharson number:
!
  PRI(:)       = (1.-PPSN(:))  * PRI(:)   + PPSN(:) * PRI3L(:)  
!
ELSE
!
  PTS    (:)  = PTG  (:,1)
  PTS_RAD(:)  = PTG  (:,1)
  PALBT  (:)  = PALB (:)
  PEMIST (:)  = PEMIS(:)
!  
! Total sublimation flux (W/m2) :
  PLEI   (:)  = PLES(:) + PLEGI(:) + PLEI_FLOOD(:)
!
ENDIF
IF (LHOOK) CALL DR_HOOK('ISBA_SNOW_AGR',1,ZHOOK_HANDLE)
!
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ISBA_SNOW_AGR
