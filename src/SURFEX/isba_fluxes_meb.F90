!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########################################################################
      SUBROUTINE ISBA_FLUXES_MEB(                                                             &
           PRHOA,                                                                             &
           PSIGMA_F,PSIGMA_FN,                                                                &
           PEMIS_N,                                                                           &
           PRNET_V,PRNET_G,PRNET_N,                                                           &
           PSWNET_V,PSWNET_G,PSWNET_N,                                                        &
           PLWNET_V,PLWNET_G,PLWNET_N,                                                        &
           PLWNET_V_DTV,PLWNET_V_DTG,PLWNET_V_DTN,                                            &
           PLWNET_G_DTV,PLWNET_G_DTG,PLWNET_G_DTN,                                            &
           PLWNET_N_DTV,PLWNET_N_DTG,PLWNET_N_DTN,                                            &
           PTHRMA_TA,PTHRMB_TA,PTHRMA_TC,PTHRMB_TC,                                           &
           PTHRMA_TG,PTHRMB_TG,PTHRMA_TV,PTHRMB_TV,PTHRMA_TN,PTHRMB_TN,                       &
           PQSAT_G,PQSAT_V,PQSATI_N,                                                          &
           PFF,PPSN,PPSNA,PPSNCV,PFROZEN1,PFFROZEN,                                           &
           PLEG_DELTA,PLEGI_DELTA,PHUG,PHUGI,PHVG,PHVN,                                       &
           PFLXC_C_A,PFLXC_G_C,PFLXC_VG_C,PFLXC_VN_C,PFLXC_N_C,PFLXC_N_A,                     &
           PFLXC_MOM,PFLXC_V_C,PHVGS,PHVNS,                                                   &
           PTG,PTV,PTN,                                                                       &
           PDQSAT_G,PDQSAT_V,PDQSATI_N,                                                       & 
           PTC,PQC,PTA_IC,PQA_IC,                                                             &
           PDELTA_V,                                                                          &
           PDELTAT_G,PDELTAT_V,PDELTAT_N,                                                     &
           PSW_UP,PSW_RAD,PLW_RAD,                                                            &
           PRNET,PLW_UP,                                                                      &
           PH_C_A,PH_V_C,PH_G_C,PH_N_C,PH_N_A,PH_N,PH,                                        &
           PLE_C_A,PLE_V_C,PLE_G_C,PLE_N_C,                                                   &
           PEVAP_C_A,PLEV_V_C,PEVAP_G_C,PEVAP_N_C,PEVAP_N_A,                                  &
           PEVAP,PSUBL,PLETR_V_C,PLER_V_C,PLEG,PLEGI,                                         &
           PLE_FLOOD,PLEI_FLOOD,PLES,PLEL,                                                    &
           PEVAPN,PLES_V_C,PLETR,PLER,PLEV,PLE,PLEI,PTS_RAD,PEMIS,PLSTT                       )
!     ##########################################################################
!
!!****  *ISBA_FLXUES_MEB*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the implicit fluxes for implicit or explicit atmospheric
!     coupling and fluxes needed by hydrology, soil and snow routines.
!     finally, compute soil phase changes.
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
!!    * to be done * (2011)
!!      
!!    AUTHOR
!!    ------
!!
!!      A. Boone           * Meteo-France *
!!      P. Samuelsson      * SMHI *
!!      S. Gollvik         * SMHI * 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    22/01/11 
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_PAR,       ONLY : XEMISSOIL, XEMISVEG
USE MODD_CSTS,           ONLY : XLVTT, XLSTT, XSTEFAN
!
USE MODI_ISBA_EMIS_MEB
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:),   INTENT(IN)   :: PRHOA
!                                     PRHOA = reference level air density (kg m-3)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PSWNET_V, PSWNET_G, PSWNET_N
!                                     PSWNET_G = Understory-ground net SW radiation explicit term (W m-2)
!                                     PSWNET_V = Vegetation canopy net SW radiation explicit term (W m-2)
!                                     PSWNET_N = Ground-based snow net SW radiation explicit term (W m-2)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PSIGMA_F, PSIGMA_FN, PEMIS_N
!
REAL, DIMENSION(:),   INTENT(IN)   :: PLWNET_V_DTV, PLWNET_V_DTG, PLWNET_V_DTN
!                                     PLWNET_V_DTV, PLWNET_V_DTG, PLWNET_V_DTN = Vegetation canopy net LW radiation 
!                                     derivatives w/r/t surface temperature(s) (W m-2 K-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PLWNET_G_DTV, PLWNET_G_DTG, PLWNET_G_DTN
!                                     PLWNET_G_DTV, PLWNET_G_DTG, PLWNET_G_DTN = Understory-ground net LW radiation 
!                                          derivatives w/r/t surface temperature(s) (W m-2 K-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PLWNET_N_DTV, PLWNET_N_DTG, PLWNET_N_DTN
!                                     PLWNET_N_DTV, PLWNET_N_DTG, PLWNET_N_DTN = Ground-based snow net LW radiation 
!                                          derivatives w/r/t surface temperature(s) (W m-2 K-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PTHRMA_TA, PTHRMB_TA, PTHRMA_TC, PTHRMB_TC,                     &
                                      PTHRMA_TG, PTHRMB_TG, PTHRMA_TV, PTHRMB_TV, PTHRMA_TN, PTHRMB_TN
!                                     PTHRMA_TA                                                    (J kg-1 K-1)
!                                     PTHRMB_TA = linear transform coefficinets for atmospheric
!                                                 thermal variable for lowest atmospheric level.   (J kg-1)
!                                                 Transform T to dry static energy or enthalpy.
!                                     PTHRMA_TC                                                    (J kg-1 K-1)
!                                     PTHRMB_TC = linear transform coefficinets for atmospheric
!                                                 thermal variable for canopy air                  (J kg-1)
!                                                 Transform T to dry static energy or enthalpy.
!                                     PTHRMA_TG,V,N                                                (J kg-1 K-1)
!                                     PTHRMB_TG,V,N = linear transform coefficinets for atmospheric
!                                                 thermal variable for surfaces (G, V, and N)      (J kg-1)
!                                                 Transform T to dry static energy or enthalpy.
!
REAL, DIMENSION(:),   INTENT(IN)   :: PQSAT_G, PQSAT_V, PQSATI_N
!                                     PQSAT_G  = saturation specific humidity for understory surface    (kg kg-1)
!                                     PQSAT_V  = saturation specific humidity for the vegetation canopy (kg kg-1)
!                                     PQSATI_N = saturation specific humidity over ice for the snowpack (kg kg-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PFF, PPSN, PPSNA, PPSNCV, PFROZEN1, PFFROZEN, PLSTT
!                                     PFF      = total flooded fraction                                        (-) 
!                                     PPSN     = fraction of snow on ground and understory vegetation          (-)
!                                     PPSNA    = fraction of vegetation canopy buried by ground-based snowpack (-)
!                                     PPSNCV   = fraction of vegetation canopy covered by intercepted snow     (-)
!                                     PFROZEN1 = frozen fraction of surface ground layer                       (-)
!                                     PFFROZEN = frozen fraction of flooded zone                               (-)
!                                     PLSTT    = effecitve latent heat of sublimation                       (J/kg)
!
!
REAL, DIMENSION(:),   INTENT(IN)   :: PLEG_DELTA, PLEGI_DELTA, PHUGI, PHUG, PHVG, PHVN
!                                     PHUG = relative humidity of the soil                                     (-)                         
!                                     PHVG = Halstead coefficient of non-buried (snow) canopy vegetation       (-)                         
!                                     PHVN = Halstead coefficient of paritally-buried (snow) canopy vegetation (-)                         
!
REAL, DIMENSION(:),   INTENT(IN)   :: PFLXC_C_A, PFLXC_G_C, PFLXC_VG_C, PFLXC_VN_C, PFLXC_N_C, PFLXC_N_A,   &
                                      PFLXC_V_C, PFLXC_MOM
!                                     PFLXC_C_A  = Flux form heat transfer coefficient: canopy air to atmosphere (kg m-2 s-1)
!                                     PFLXC_G_C  = As above, but for : ground-understory to canopy air           (kg m-2 s-1)
!                                     PFLXC_VG_C = As above, but for : non-snow buried canopy to canopy air      (kg m-2 s-1)
!                                     PFLXC_VN_C = As above, but for : partially snow-buried canopy air to canopy 
!                                                  air                                                           (kg m-2 s-1)
!                                     PFLXC_V_C  = As above, but for : bulk vegetation canopy to canopy air      (kg m-2 s-1)
!                                     PFLXC_N_C  = As above, but for : ground-based snow to atmosphere           (kg m-2 s-1)
!                                     PFLXC_N_A  = As above, but for : ground-based snow to canopy air           (kg m-2 s-1)
!                                     PFLXC_MOM  = flux form drag transfer coefficient: canopy air to atmosphere (kg m-2 s-1)
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PTN
!                                     PTN    = Ground-based snow temperature profile (K)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PTV
!                                     PTV    = Vegetation canopy temperature (K)
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PTG
!                                     PTG    = Soil temperature profile (K)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PDQSAT_G, PDQSAT_V, PDQSATI_N
!                                     PQSAT_G  = saturation specific humidity derivative for understory 
!                                                surface               (kg kg-1 K-1)
!                                     PQSAT_V  = saturation specific humidity derivative for the vegetation 
!                                                canopy                (kg kg-1 K-1)
!                                     PQSATI_N = saturation specific humidity derivative over ice for the 
!                                                ground-based snowpack (kg kg-1 K-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PHVGS, PHVNS
!                                     PHVGS     = Dimensionless pseudo humidity factor for computing vapor
!                                                 fluxes from the non-buried part of the canopy to the canopy air    (-)
!                                     PHVNS     = Dimensionless pseudo humidity factor for computing vapor
!                                                 fluxes from the partly-buried part of the canopy to the canopy air (-)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PTC, PQC, PTA_IC, PQA_IC
!                                     PTC       = Canopy air space temperature       (K)
!                                     PQC       = Canopy air space specific humidity (kg kg-1)
!                                     PTA_IC    = Near-ground air temperature        (K)
!                                     PQA_IC    = Near-ground air specific humidity  (kg kg-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PSW_UP, PSW_RAD, PLW_RAD
!                                     PSW_UP  = total upwelling shortwave radiation from the surface at the atmosphere (W m-2)
!                                     PSW_RAD = downwelling shortwave radiation from the atmosphere above the canopy  (W m-2)
!                                     PLW_RAD = downwelling longwave radiation from the atmosphere above the canopy  (W m-2)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PDELTA_V
!                                     PDELTA_V = Explicit canopy interception fraction                 (-)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PDELTAT_V, PDELTAT_N, PDELTAT_G
!                                     PDELTAT_V = Time change in vegetation canopy temperature (K)
!                                     PDELTAT_N = Time change in snowpack surface temperature  (K)
!                                     PDELTAT_G = Time change in soil surface temperature      (K)
!
REAL, DIMENSION(:),   INTENT(INOUT):: PLWNET_V, PLWNET_G, PLWNET_N
!                                     PLWNET_G = Understory-ground net LW radiation implicit term output (W m-2)
!                                     PLWNET_V = Vegetation canopy net LW radiation implicit term output (W m-2)
!                                     PLWNET_N = Ground-based snow net LW radiation implicit term output (W m-2)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PRNET_V, PRNET_G, PRNET_N
!                                     PRNET_G = Understory-ground net radiation (W m-2)
!                                     PRNET_V = Vegetation canopy net radiation (W m-2)
!                                     PRNET_N = Ground-based snow net radiation (W m-2)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PRNET, PLW_UP  
!                                     PRNET   = total net radiation of snow, understory and canopy (W m-2)
!                                     PLW_UP  = total net longwave upwelling radiation to the atmosphere  (W m-2)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PH_C_A, PH_V_C, PH_G_C, PH_N_C, PH_N, PH, PH_N_A
!                                     PH_C_A  = Sensible heat flux: canopy air space to overlying atmosphere (W m-2)
!                                     PH_V_C  = Sensible heat flux: vegetation canopy to canopy air space  (W m-2)
!                                     PH_G_C  = Sensible heat flux: understory (soil & vegetation) to canopy air space  (W m-2)
!                                     PH_N_C  = Sensible heat flux: ground based snowpack to canopy air space  (W m-2)
!                                     PH_N    = Sensible heat flux: ground based snowpack to both canopy air space and overlying atmosphere (W m-2)
!                                     PH      = Sensible heat flux: total net sensible heat flux from surface to atmosphere (W m-2)
!                                     PH_N_A  = Sensible heat flux: ground based snowpack to overlying atmosphere (W m-2)
!
REAL, DIMENSION(:),   INTENT(OUT)  ::  PLE_C_A, PLE_V_C, PLE_G_C, PLE_N_C
!                                     PLE_C_A = Latent heat flux: canopy air space to overlying atmosphere (W m-2)
!                                     PLE_V_C = Latent heat flux: vegetation canopy to canopy air space  (W m-2)
!                                     PLE_G_C = Latent heat flux: understory (soil & vegetation) to canopy air space  (W m-2)
!                                     PLE_N_C = Latent heat flux: ground based snowpack to canopy air space  (W m-2)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PEVAP_C_A, PLEV_V_C, PEVAP_G_C, PEVAP_N_C, PEVAP_N_A,            &  
                                      PEVAP, PSUBL, PEVAPN
!                                     PEVAP_C_A = Water flux: canopy air space to overlying atmosphere (kg m-2 s-1)
!                                     PLEV_V_C  = Water flux: vegetation canopy to canopy air space (kg m-2 s-1)
!                                     PEVAP_G_C = Water flux: understory (soil & vegetation) to canopy air space (kg m-2 s-1)
!                                     PEVAP_N_C = Water flux: ground based snowpack to canopy air space (kg m-2 s-1)
!                                     PEVAP_N_A = Water flux: ground based snowpack to overlying atmosphere (kg m-2 s-1)
!                                     PEVAP     = Water flux: total net water flux from surface to atmosphere  (kg m-2 s-1)
!                                     PSUBL     = Water flux: total sublimation flux (kg/m2/s)
!                                     PEVAPN    = Water flux: ground based snowpack to both canopy air space and overlying atmosphere (kg m-2 s-1)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PLETR_V_C, PLER_V_C, PLEG, PLEGI, PLE_FLOOD,        &
                                      PLEI_FLOOD, PLES, PLEL, PLES_V_C, PLETR, PLEV, PLE, PLEI, PLER
!                                     PLETR_V_C   = Latent heat flux: transpiration from the canopy (overstory) vegetation to canopy air (W m-2)
!                                     PLER_V_C    = Latent heat flux: evaporation of intercepted water from the canopy (overstory) vegetation to canopy air (W m-2)
!                                     PLES_V_C    = Latent heat flux: sublimation of canopy intercepted snowpack to canopy air (W m-2)
!                                     PLEG        = Latent heat flux: baresoil evaporation (W m-2)
!                                     PLEGI       = Latent heat flux: baresoil sublimation (W m-2)
!                                     PLE_FLOOD   = Latent heat flux: evaporation from flooded areas (W m-2)
!                                     PLEI_FLOOD  = Latent heat flux: sublimation from ice-covered flooded areas (W m-2)
!                                     PLEL        = Latent heat flux: net evaporation from ground-based snowpack to canopy air and overlying atmosphere (W m-2)
!                                     PLES        = Latent heat flux: net sublimation from ground-based snowpack to canopy air and overlying atmosphere (W m-2)
!                                     PLETR       = Latent heat flux: net transpiration from understory and overstory (canopy) (W m-2)
!                                     PLEV        = Latent heat flux: net evapotranspiration from understory and canopy vegetation (W m-2)
!                                     PLE         = Latent heat flux: net evapotranspiration (W m-2)
!                                     PLEI        = Latent heat flux: net sublimation (W m-2)
!                                     PLER        = Latent heat flux: net evaporation from intercepted water from understory and canopy vegetation (W m-2) 
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PTS_RAD, PEMIS
!                                     PTS_RAD     = Net surface radiative temperature: computed using aggregated effective sfc emissivity 
!                                                   backed out from LWup: this is done to ensure cosistency between LWup, Ts_rad and effective sfc Emis (K) 
!                                     PEMIS       = effective (aggregated) net surface emissivity (-)
!
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PTV))         :: ZFFF, ZWORK
!                                     ZFFF  = working variables to help distinguish between soil and snow hydrolology and intercepted water reservoirs (-)
!                                     ZWORK = working array
!
REAL, DIMENSION(SIZE(PTV))         :: ZSAIR, ZSAIRC
!                                     ZSAIR   = atmospheric value of the therodynamic variable
!                                     ZSAIRC  = canopy air value of the therodynamic variable
!
REAL, DIMENSION(SIZE(PTV))         :: ZEVAP_V_C
!                                     ZEVAP_V_C = Water flux: Evapotranspiration vapor flux from the vegetation canopy (kg m-2 s-1)
!
REAL, DIMENSION(SIZE(PTV))         :: ZQSATN_V, ZQSATIN_N, ZQSATN_G
!                                     ZQSATN_V  = saturation specific humidity (over water) for the vegetation canopy (kg kg-1)
!                                     ZQSATIN_N = saturation specific humidity (over ice) for the snow (kg kg-1)
!                                                 NOTE that liquid water can only exist when the snowpack T=XTT in the model, 
!                                                 and at the freezing point, the value is the same over ice and water, therefore
!                                                 over snow, we do not need to explicitly consider a "ZQSATN_N"
!                                     ZQSATN_G  = saturation specific humidity (over water) for the understory (kg kg-1)
!                
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       0.     Initialization:
!               ---------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_FLUXES_MEB',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!*       1.     Radiative Fluxes
!               ----------------
!
! LWnet: transform from explicit to implicit (i.e. at time t+dt)
!
PLWNET_V(:)  = PLWNET_V(:) + PLWNET_V_DTV(:)*PDELTAT_V(:)   &
                           + PLWNET_V_DTG(:)*PDELTAT_G(:)   &
                           + PLWNET_V_DTN(:)*PDELTAT_N(:)

PLWNET_G(:)  = PLWNET_G(:) + PLWNET_G_DTV(:)*PDELTAT_V(:)   &
                           + PLWNET_G_DTG(:)*PDELTAT_G(:)   &
                           + PLWNET_G_DTN(:)*PDELTAT_N(:)

PLWNET_N(:)  = PLWNET_N(:) + PLWNET_N_DTV(:)*PDELTAT_V(:)   &
                           + PLWNET_N_DTG(:)*PDELTAT_G(:)   &
                           + PLWNET_N_DTN(:)*PDELTAT_N(:)
!
! LWup at t+dt
!
PLW_UP(:)   = PLW_RAD(:) - (PLWNET_V(:) + PLWNET_G(:) + PLWNET_N(:))
!
!
! Effective emissivity:
!
 CALL ISBA_EMIS_MEB(PPSN, PPSNA, PSIGMA_F, PSIGMA_FN,  &
                    PEMIS_N, PEMIS                     )
!
! Now compute the effective radiative temperature while
! imposing the constraint: 
!
!    LW_RAD * (1 - EMIS ) + EMIS * XSTEFAN * TS_RAD**4 = LWUP
!
! Using the effective emissivity ensures that the upwelling radiation from the surface (RHS)
! model will be equal to the upwelling radiation computed in the atmospheric model (LHS)
! (i.e. LWUP is consistent with EMIS & TS_RAD), thereby insuring energy conservation from
! the surface to the atmosphere. Solving the above equation for
! the radiative T gives:
!
PTS_RAD(:)    = ((PLW_UP(:) - PLW_RAD(:)*(1.0-PEMIS(:)))/(XSTEFAN*PEMIS(:)))**0.25
!
!
! Rnet (t+dt)
!
PRNET_V(:)  = PSWNET_V(:) + PLWNET_V(:)
!
PRNET_G(:)  = PSWNET_G(:) + PLWNET_G(:)
!
PRNET_N(:)  = PSWNET_N(:) + PLWNET_N(:)
!
!
! total Rnet (t+dt):
!
PRNET(:)    = PRNET_G(:) + PRNET_V(:) + PRNET_N(:)
!
!
!*       2.a    Implicit (Turbulent) Sensible Heat Fluxes
!               -----------------------------------------

! First get input thermo variable (could be enthalpy (air heat capacity x potential temperature or dry static energy)

ZSAIR(:)  = PTHRMB_TA(:)    + PTHRMA_TA(:)   *PTA_IC(:)
ZSAIRC(:) = PTHRMB_TC(:)    + PTHRMA_TC(:)   *PTC(:)

! Sensible heat fluxes (W m-2):
! - Canopy air to atmosphere, vegetation canopy to canopy air (implicitly includes from canopy intercepted snow), 
!   understory-ground to canopy air,
!   ground-based snow to canopy air, ground-based snow to atmosphere:

PH_C_A(:)  = PFLXC_C_A(:) *( ZSAIRC(:)                            - ZSAIR(:)  )*(1.0 - PPSN(:)*PPSNA(:))
PH_V_C(:)  = PFLXC_V_C(:) *( PTHRMB_TV(:) + PTHRMA_TV(:)*PTV(:)   - ZSAIRC(:) ) 
PH_G_C(:)  = PFLXC_G_C(:) *( PTHRMB_TG(:) + PTHRMA_TG(:)*PTG(:,1) - ZSAIRC(:) )*(1.0-PPSN(:))
PH_N_C(:)  = PFLXC_N_C(:) *( PTHRMB_TN(:) + PTHRMA_TN(:)*PTN(:,1) - ZSAIRC(:) )*     PPSN(:) *(1.0-PPSNA(:)) 
PH_N_A(:)  = PFLXC_N_A(:) *( PTHRMB_TN(:) + PTHRMA_TN(:)*PTN(:,1) - ZSAIR(:)  )*     PPSN(:) *     PPSNA(:) 

! - Net sensible heat flux from ground-based snow (to the canopy and the atmosphere (from
!   the buried-vegetation canopy fraction)) (W m-2) 

PH_N(:)    = PH_N_C(:) + PH_N_A(:) 

! FINAL sensible heat flux to the atmosphere (W m-2):

PH(:)      = PH_C_A(:) + PH_N_A(:)

!
!*       2.b    Implicit (Turbulent) Vapor and Latent Heat Fluxes
!               -------------------------------------------------
! Note, to convert any of the latent heat fluxes back to vapor fluxes, 
! simply divide by XLVTT, even sublimation fluxes as XLSTT already accounted for.

! - first get 'new' surface specific humidities, qsatn, at time t+dt:

ZQSATN_G(:)  =        PQSAT_G(:)  + PDQSAT_G(:)  * PDELTAT_G(:)
ZQSATN_V(:)  =        PQSAT_V(:)  + PDQSAT_V(:)  * PDELTAT_V(:)
ZQSATIN_N(:) =        PQSATI_N(:) + PDQSATI_N(:) * PDELTAT_N(:)

! additional sfc diagnostics needed for soil and snow hydrolology and intercepted water reservoirs:

ZFFF(:)       = PFF(:)*( 1.0 - PFFROZEN(:)*(1.0 - (XLSTT/XLVTT)) )

! - Evaporation and Sublimation latent heat fluxes from the soil, respectively:
! (kg m-2 s-1)

ZWORK(:)      = (1.-PPSN(:)-ZFFF(:)) * PFLXC_G_C(:)

PLEG(:)       = ZWORK(:)*PLEG_DELTA(:) *( PHUG(:) *ZQSATN_G(:) - PQC(:) )*(1.-PFROZEN1(:))*XLVTT

PLEGI(:)      = ZWORK(:)*PLEGI_DELTA(:)*( PHUGI(:)*ZQSATN_G(:) - PQC(:) )*    PFROZEN1(:) *XLSTT

! - Latent heat flux from frozen and unfrozen flooded zones (W m-2)

ZWORK(:)      = PFF(:) * PFLXC_G_C(:)*( ZQSATN_G(:) - PQC(:) )
PLE_FLOOD(:)  = ZWORK(:) * (1.-PFFROZEN(:))* XLVTT 
PLEI_FLOOD(:) = ZWORK(:) *     PFFROZEN(:) * XLSTT 

! - Evapotranspiration vapor flux from the vegetation canopy (kg m-2 s-1)

ZEVAP_V_C(:) = (1.-PPSNCV(:)) * PHVGS(:) * PFLXC_V_C(:)*( ZQSATN_V(:) - PQC(:) )

! - Latent heat flux from the canopy (liquid) water interception reservoir (W m-2)

PLER_V_C(:)   = ( (1.-PPSNA(:))*PPSN(:) * PFLXC_VN_C(:)    +                      &
                           (1.-PPSN(:))* PFLXC_VG_C(:)  ) *                       &
                 XLVTT * (1.-PPSNCV(:))* PDELTA_V(:) * ( ZQSATN_V(:) - PQC(:) )

! - latent heat flux from transpiration from the canopy (W m-2)

PLETR_V_C(:) = ZEVAP_V_C(:) * XLVTT - PLER_V_C(:) 

! Snow sublimation and evaporation latent heat flux from canopy-intercepted snow (W m-2)

PLES_V_C(:)  =  PPSNCV(:) * XLSTT * PHVNS(:) * PFLXC_V_C(:)*( ZQSATN_V(:) - PQC(:) )

! - Total latent heat flux (evapotranspiration) from the vegetation to the canopy air space (W m-2)
!   *without* sublimation (for TOTAL evapotranspiration and sublimation, add PLESC here)

PLEV_V_C(:)  = XLVTT*ZEVAP_V_C(:) 

! - Total latent heat flux from vegetation canopy overstory to canopy air space
!   (including transpiration, liquid water store, canopy snow sublimation):

PLE_V_C(:)   = PLEV_V_C(:) + PLES_V_C(:)

! - Vapor flux from the ground-based snowpack to the canopy air (kg m-2 s-1):

PEVAP_N_C(:) = PFLXC_N_C(:)*(ZQSATIN_N(:) - PQC(:))*PPSN(:)*(1.0-PPSNA(:))*(XLSTT/XLVTT)

PLE_N_C(:)   = XLVTT*PEVAP_N_C(:) ! W m-2

! - latent heat flux from transpiration from canopy veg (evapotranspiration)

PLETR(:)     = PLETR_V_C(:)

! Total latent heat flux from transpiration from understory veg and canopy veg (evapotranspiration and sublimation)
!   and intercepted water on both reservoirs (W m-2) 

PLEV(:)      = PLETR(:) + PLER_V_C(:) 

! Total latent heat flux from intercepted water (canopy and understory vegetation):
! (does not include intercepted snow sublimation): W m-2

PLER(:)      = PLER_V_C(:)

! - Vapor flux from the ground-based snowpack (part burying the canopy vegetation) to the atmosphere (kg m-2 s-1):

PEVAP_N_A(:) = PFLXC_N_A(:) *( ZQSATIN_N(:) - PQA_IC(:))*       PPSN(:)*     PPSNA(:) *(XLSTT/XLVTT)

! - Net Snow (groud-based) sublimation latent heat flux (W m-2) to the canopy air space and the overlying atmosphere:

PLES(:)      = ( PFLXC_N_C(:) *( ZQSATIN_N(:) - PQC(:)   )*       PPSN(:)*(1.0-PPSNA(:))  +         &
                 PFLXC_N_A(:) *( ZQSATIN_N(:) - PQA_IC(:))*       PPSN(:)*     PPSNA(:) ) * XLSTT

! - Net Snow evaporation (liquid water) latent heat flux (W m-2)

PLEL(:)      = XLVTT*(PEVAP_N_C(:) + PEVAP_N_A(:)) - PLES(:)

! - Total mass flux from ground-based snowpack (kg m-2 s-1)

PEVAPN(:)    = (PLEL(:) + PLES(:))/XLVTT

! - Total snow-free vapor flux from the understory (flooded areas, baresoil and understory vegetation)
!   to the canopy air space (W m-2 and kg m-2 s-1, respectively):

PLE_G_C(:)   = PLE_FLOOD(:) + PLEI_FLOOD(:) + PLEGI(:) + PLEG(:) 

PEVAP_G_C(:) = PLE_G_C(:)/XLVTT 

! - Net vapor flux from canopy air to the atmosphere (kg m-2 s-1)

PEVAP_C_A(:) = PFLXC_C_A(:) *( PQC(:) - PQA_IC(:))*(1.0 - PPSN(:)*PPSNA(:))

PLE_C_A(:)   = XLVTT * PEVAP_C_A(:) ! W m-2

! FINAL net vapor flux from the surface to the Atmosphere:
! - Net vapor flux from canopy air and exposed ground based snow (from part of snow 
!   burying the vegetation canopy) to the atmosphere (kg m-2 s-1)
!
PEVAP(:)     = PEVAP_C_A(:) + PEVAP_N_A(:)   
! 
! Total latent heat flux of surface/snow/vegetation: W m-2
!
PLE(:)       = PEVAP(:)*XLVTT                    
!
! Total sublimation from the surface/snow/vegetation: W m-2
!
PLEI(:)      = PLES(:) + PLEGI(:) + PLEI_FLOOD(:)
!
! Total sublimation from the surface/snow/vegetation: kg m-2 s-1
!
PSUBL(:)     = PLEI(:)/PLSTT(:)
!
IF (LHOOK) CALL DR_HOOK('ISBA_FLUXES_MEB',1,ZHOOK_HANDLE)
!
END SUBROUTINE ISBA_FLUXES_MEB


