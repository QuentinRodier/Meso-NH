!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!########################
MODULE MODD_DIAG_EVAP_ISBA_n
!########################
!
!!****  *MODD_DIAG_ISBA - declaration of packed surface parameters for ISBA scheme
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       07/11/03
!!      P. Samuelsson  04/2012   MEB
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE DIAG_EVAP_ISBA_t
!------------------------------------------------------------------------------
!
  LOGICAL :: LSURF_EVAP_BUDGET   ! flag for all terms of evaporation
  LOGICAL :: LSURF_BUDGETC       ! flag for surface cumulated energy budget
  LOGICAL :: LRESET_BUDGETC      ! flag for surface cumulated energy budget
  LOGICAL :: LWATER_BUDGET       ! flag for isba water budget including input  
                                 ! fluxes (rain and snow) and reservoir tendencies
!
!* variables for each patch
!
  REAL, POINTER, DIMENSION(:,:) :: XLEG          ! latent heat of evaporation over the ground   (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLEGI         ! surface soil ice sublimation                 (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLEV          ! latent heat of evaporation over vegetation   (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLES          ! latent heat of sublimation over the snow     (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLESL         ! latent heat of evaporation over the snow     (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLER          ! evaporation from canopy water interception   (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLETR         ! evapotranspiration of the vegetation         (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XEVAP         ! evapotranspiration                           (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XSUBL         ! sublimation                                  (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XSNDRIFT      ! blowing snow sublimation (ES or Crocus)      (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XDRAIN        ! soil drainage flux                           (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XQSB          ! lateral subsurface flux (dif option)         (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XRUNOFF       ! sub-grid and supersaturation runoff          (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XHORT         ! sub-grid Horton runoff from the SGH scheme   (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XRRVEG        !  precipitation intercepted by the vegetation (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XMELT         ! snow melt                                    (kg/m2/s)      
  REAL, POINTER, DIMENSION(:,:) :: XIFLOOD       ! Floodplains infiltration                     (kg/m2/s)      
  REAL, POINTER, DIMENSION(:,:) :: XPFLOOD       ! Precipitation intercepted by the floodplains (kg/m2/s)      
  REAL, POINTER, DIMENSION(:,:) :: XLE_FLOOD     ! Floodplains evapotration                     (W/m2)      
  REAL, POINTER, DIMENSION(:,:) :: XLEI_FLOOD    ! Floodplains evapotration                     (W/m2)      
  REAL, POINTER, DIMENSION(:,:) :: XDRIP         ! dripping from the vegetation reservoir       (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XIRRIG_FLUX   ! irrigation rate (as soil input)              (kg/m2/s)
!  
  REAL, POINTER, DIMENSION(:,:) :: XGPP          ! Gross Primary Production                     (kgCO2/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XRESP_AUTO    ! Autotrophic respiration                      (kgCO2/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XRESP_ECO     ! Ecosystem respiration                        (kgCO2/m2/s)
!
  REAL, POINTER, DIMENSION(:,:) :: XLEVCV        ! MEB: total evapotranspiration from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLESC         ! MEB: total snow sublimation from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLETRGV       ! MEB: transpiration from understory vegetation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLETRCV       ! MEB: transpiration from overstory canopy vegetation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLERGV        ! MEB: interception evaporation from understory vegetation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLELITTER        ! MEB: interception evaporation from understory vegetation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLELITTERI        ! MEB: interception evaporation from understory vegetation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XDRIPLIT       ! 
  REAL, POINTER, DIMENSION(:,:) :: XRRLIT         ! 
  REAL, POINTER, DIMENSION(:,:) :: XLERCV        ! MEB: interception evaporation from overstory canopy vegetation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLE_V_C       ! MEB: latent heat flux from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLE_G_C       ! MEB: latent heat flux from understory [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLE_C_A       ! MEB: latent heat flux from canopy air space to the atmosphere [W/m2] 
                                                 !      NOTE total latent heat flux to the atmosphere also possibly 
                                                 !      includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:,:) :: XLE_N_C       ! MEB: latent heat flux from the snow on the ground [W/m2]
                                                 !      NOTE total latent heat flux from the snowpack
                                                 !      possibly includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:,:) :: XSWNET_V      ! MEB: net vegetation canopy shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XSWNET_G      ! MEB: net ground shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XSWNET_N      ! MEB: net snow shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XSWNET_NS     ! MEB: net snow shortwave radiation for *surface* layer 
                                                 !     (i.e. net snow shortwave radiation less absorbed radiation) [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLWNET_V      ! MEB: net vegetation canopy longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLWNET_G      ! MEB: net ground longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLWNET_N      ! MEB: net snow longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XH_V_C        ! MEB: sensible heat flux from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XH_G_C        ! MEB: sensible heat flux from understory [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XH_C_A        ! MEB: sensible heat flux from canopy air space to the atmosphere [W/m2] 
                                                 !      NOTE total sensible heat flux to the atmosphere also possibly 
                                                 !      includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:,:) :: XH_N_C        ! MEB: sensible heat flux from the snow on the ground [W/m2]
                                                 !      NOTE total sensible heat flux from the snowpack
                                                 !      possibly includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:,:) :: XSR_GN        ! MEB: snow unloading rate from the overstory reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:,:) :: XMELTCV       ! MEB: snow melt rate from the overstory snow reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:,:) :: XFRZCV        ! MEB: snow refreeze rate from the overstory snow reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:,:) :: XSWDOWN_GN    ! MEB: total shortwave radiation transmitted through the canopy
                                                 !      reaching the snowpack/ground understory [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLWDOWN_GN    ! MEB: total shortwave radiation transmitted through and emitted by the canopy
                                                 !      reaching the snowpack/ground understory (explicit part) [W/m2]
!
  REAL, POINTER, DIMENSION(:,:) :: XDWG          ! liquid soil moisture time tendencies         (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XDWGI         ! solid soil moisture time tendencies          (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XDWR          ! canopy water time tendencies                 (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XDSWE         ! snow water equivalent time tendencies        (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XWATBUD       ! ISBA water budget                            (kg/m2/s)
!
!* average variables
!
  REAL, POINTER, DIMENSION(:)   :: XAVG_LEG      ! latent heat of evaporation over the ground   (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LEGI     ! surface soil ice sublimation                 (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LEV      ! latent heat of evaporation over vegetation   (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LES      ! latent heat of sublimation over the snow     (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LESL     ! latent heat of evaporation over the snow     (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LER      ! evaporation from canopy water interception   (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LETR     ! evapotranspiration of the vegetation         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_EVAP     ! evapotranspiration                           (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_SUBL     ! sublimation                                  (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_SNDRIFT  ! blowing snow sublimation (ES or Crocus)      (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_DRAIN    ! soil drainage flux                           (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_QSB      ! lateral subsurface flux (dif option)         (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_RUNOFF   ! sub-grid and supersaturation runoff          (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_HORT     ! sub-grid Horton runoff from the SGH scheme   (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_DRIP     ! dripping from the vegetation reservoir       (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_MELT     ! snow melt                                    (kg/m2/s)      
  REAL, POINTER, DIMENSION(:)   :: XAVG_IFLOOD   ! Floodplains infiltration                     (kg/m2/s)      
  REAL, POINTER, DIMENSION(:)   :: XAVG_PFLOOD   ! Precipitation intercepted by the floodplains (kg/m2/s)      
  REAL, POINTER, DIMENSION(:)   :: XAVG_LE_FLOOD ! Floodplains evapotration                     (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LEI_FLOOD! Floodplains evapotration                     (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_RRVEG    ! precipitation intercepted by the vegetation  (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_IRRIG_FLUX! irrigation rate (as soil input)             (kg/m2/s)
!  
  REAL, POINTER, DIMENSION(:)   :: XAVG_GPP      ! Gross Primary Production                     (kgCO2/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_RESP_AUTO! Autotrophic respiration                      (kgCO2/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_RESP_ECO ! Ecosystem respiration                        (kgCO2/m2/s)
!
  REAL, POINTER, DIMENSION(:) :: XAVG_LEVCV        ! MEB: total evapotranspiration from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LESC         ! MEB: total snow sublimation from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LETRGV       ! MEB: transpiration from understory vegetation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LETRCV       ! MEB: transpiration from overstory canopy vegetation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LERGV        ! MEB: interception evaporation from understory vegetation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LELITTER        ! MEB: interception evaporation from understory vegetation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LELITTERI        ! MEB: interception evaporation from understory vegetation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_DRIPLIT      ! 
  REAL, POINTER, DIMENSION(:) :: XAVG_RRLIT        !
  REAL, POINTER, DIMENSION(:) :: XAVG_LERCV        ! MEB: interception evaporation from overstory canopy vegetation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LE_V_C       ! MEB: latent heat flux from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LE_G_C       ! MEB: latent heat flux from understory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LE_C_A       ! MEB: latent heat flux from canopy air space to the atmosphere [W/m2] 
                                                 !      NOTE total latent heat flux to the atmosphere also possibly 
                                                 !      includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:) :: XAVG_LE_N_C       ! MEB: latent heat flux from the snow on the ground [W/m2]
                                                 !      NOTE total latent heat flux from the snowpack
                                                 !      possibly includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:) :: XAVG_SWNET_V      ! MEB: net vegetation canopy shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_SWNET_G      ! MEB: net ground shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_SWNET_N      ! MEB: net snow shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_SWNET_NS     ! MEB: net snow shortwave radiation for *surface* layer 
                                                 !     (i.e. net snow shortwave radiation less absorbed radiation) [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LWNET_V      ! MEB: net vegetation canopy longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LWNET_G      ! MEB: net ground longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LWNET_N      ! MEB: net snow longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_H_V_C        ! MEB: sensible heat flux from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_H_G_C        ! MEB: sensible heat flux from understory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_H_C_A        ! MEB: sensible heat flux from canopy air space to the atmosphere [W/m2] 
                                                 !      NOTE total sensible heat flux to the atmosphere also possibly 
                                                 !      includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:) :: XAVG_H_N_C        ! MEB: sensible heat flux from the snow on the ground [W/m2]
                                                 !      NOTE total sensible heat flux from the snowpack
                                                 !      possibly includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:) :: XAVG_SR_GN        ! MEB: snow unloading rate from the overstory reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:) :: XAVG_MELTCV       ! MEB: snow melt rate from the overstory snow reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:) :: XAVG_FRZCV        ! MEB: snow refreeze rate from the overstory snow reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:) :: XAVG_SWDOWN_GN    ! MEB: total shortwave radiation transmitted through the canopy
                                                 !      reaching the snowpack/ground understory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LWDOWN_GN    ! MEB: total shortwave radiation transmitted through and emitted by the canopy
                                                 !      reaching the snowpack/ground understory (explicit part) [W/m2]
!
  REAL, POINTER, DIMENSION(:)   :: XRAINFALL     ! input rainfall rate for LWATER_BUDGET        (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XSNOWFALL     ! input snowfall rate for LWATER_BUDGET        (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_DWG      ! liquid soil moisture time tendencies         (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_DWGI     ! solid soil moisture time tendencies          (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_DWR      ! canopy water time tendencies                 (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_DSWE     ! snow water equivalent time tendencies        (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_WATBUD   ! ISBA water budget                            (kg/m2/s)
!  
!* budget summation variables for each patch
!
  REAL, POINTER, DIMENSION(:,:) :: XRNC          ! net radiation at surface                     (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XHC           ! sensible heat flux                           (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLEC          ! total latent heat flux                       (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLEIC         ! sublimation latent heat flux                 (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XGFLUXC       ! net soil-vegetation flux                     (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLEGC         ! latent heat of evaporation over the ground   (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLEGIC        ! surface soil ice sublimation                 (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLEVC         ! latent heat of evaporation over vegetation   (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLESAC         ! latent heat of sublimation over the snow     (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLESLC        ! latent heat of evaporation over the snow     (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLERC         ! evaporation from canopy water interception   (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLETRC        ! evapotranspiration of the vegetation         (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XEVAPC        ! evapotranspiration                           (kg/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSUBLC        ! sublimation                                  (kg/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSNDRIFTC     ! blowing snow sublimation (ES or Crocus)      (kg/m2)
  REAL, POINTER, DIMENSION(:,:) :: XDRAINC       ! soil drainage flux                           (kg/m2) 
  REAL, POINTER, DIMENSION(:,:) :: XQSBC         ! lateral subsurface flux (dif option)         (kg/m2) 
  REAL, POINTER, DIMENSION(:,:) :: XRUNOFFC      ! sub-grid and supersaturation runoff          (kg/m2)
  REAL, POINTER, DIMENSION(:,:) :: XHORTC        ! sub-grid Horton runoff from the SGH scheme   (kg/m2)
  REAL, POINTER, DIMENSION(:,:) :: XDRIPC        ! dripping from the vegetation reservoir       (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:) :: XMELTC        ! snow melt                                    (kg/m2)      
  REAL, POINTER, DIMENSION(:,:) :: XIFLOODC      ! Floodplains infiltration                     (kg/m2)      
  REAL, POINTER, DIMENSION(:,:) :: XPFLOODC      ! Precipitation intercepted by the floodplains (kg/m2)      
  REAL, POINTER, DIMENSION(:,:) :: XLE_FLOODC    ! Floodplains evapotration                     (J/m2)  
  REAL, POINTER, DIMENSION(:,:) :: XLEI_FLOODC   ! Floodplains evapotration                     (J/m2)  
  REAL, POINTER, DIMENSION(:,:) :: XICEFLUXC     ! ice calving flux                             (kg/m2)
  REAL, POINTER, DIMENSION(:,:) :: XRRVEGC       ! precipitation intercepted by the vegetation  (kg/m2)
  REAL, POINTER, DIMENSION(:,:) :: XIRRIG_FLUXC  ! irrigation rate (as soil input)              (kg/m2)
!  
  REAL, POINTER, DIMENSION(:,:) :: XGPPC         ! Gross Primary Production                     (kgCO2/m2)
  REAL, POINTER, DIMENSION(:,:) :: XRESPC_AUTO   ! Autotrophic respiration                      (kgCO2/m2)
  REAL, POINTER, DIMENSION(:,:) :: XRESPC_ECO    ! Ecosystem respiration                        (kgCO2/m2)
!
  REAL, POINTER, DIMENSION(:,:) :: XLEVCVC        ! MEB: total evapotranspiration from vegetation canopy overstory [J/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLESCC         ! MEB: total snow sublimation from vegetation canopy overstory [J/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLETRGVC       ! MEB: transpiration from understory vegetation [J/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLETRCVC       ! MEB: transpiration from overstory canopy vegetation [J/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLERGVC        ! MEB: interception evaporation from understory vegetation [J/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLERCVC        ! MEB: interception evaporation from overstory canopy vegetation [J/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLE_V_CC       ! MEB: latent heat flux from vegetation canopy overstory [J/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLE_G_CC       ! MEB: latent heat flux from understory [J/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLE_C_AC       ! MEB: latent heat flux from canopy air space to the atmosphere [J/m2] 
                                                  !      NOTE total latent heat flux to the atmosphere also possibly 
                                                  !      includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:,:) :: XLE_N_CC       ! MEB: latent heat flux from the snow on the ground [J/m2]
                                                  !      NOTE total latent heat flux from the snowpack
                                                  !      possibly includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:,:) :: XSWNET_VC      ! MEB: net vegetation canopy shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XSWNET_GC      ! MEB: net ground shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XSWNET_NC      ! MEB: net snow shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XSWNET_NSC     ! MEB: net snow shortwave radiation for *surface* layer 
                                                  !     (i.e. net snow shortwave radiation less absorbed radiation) [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLWNET_VC      ! MEB: net vegetation canopy longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLWNET_GC      ! MEB: net ground longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLWNET_NC      ! MEB: net snow longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XH_V_CC        ! MEB: sensible heat flux from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XH_G_CC        ! MEB: sensible heat flux from understory [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XH_C_AC        ! MEB: sensible heat flux from canopy air space to the atmosphere [W/m2] 
                                                  !      NOTE total sensible heat flux to the atmosphere also possibly 
                                                  !      includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:,:) :: XH_N_CC        ! MEB: sensible heat flux from the snow on the ground [W/m2]
                                                  !      NOTE total sensible heat flux from the snowpack
                                                  !      possibly includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:,:) :: XSR_GNC        ! MEB: snow unloading rate from the overstory reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:,:) :: XMELTCVC       ! MEB: snow melt rate from the overstory snow reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:,:) :: XFRZCVC        ! MEB: snow refreeze rate from the overstory snow reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:,:) :: XSWDOWN_GNC    ! MEB: total shortwave radiation transmitted through the canopy
                                                  !      reaching the snowpack/ground understory [W/m2]
  REAL, POINTER, DIMENSION(:,:) :: XLWDOWN_GNC    ! MEB: total shortwave radiation transmitted through and emitted by the canopy
                                                  !      reaching the snowpack/ground understory (explicit part) [W/m2]
!
  REAL, POINTER, DIMENSION(:,:) :: XDWGC         ! liquid soil moisture time tendencies         (kg/m2)
  REAL, POINTER, DIMENSION(:,:) :: XDWGIC        ! solid soil moisture time tendencies          (kg/m2)
  REAL, POINTER, DIMENSION(:,:) :: XDWRC         ! canopy water time tendencies                 (kg/m2)
  REAL, POINTER, DIMENSION(:,:) :: XDSWEC        ! snow water equivalent time tendencies        (kg/m2)
  REAL, POINTER, DIMENSION(:,:) :: XWATBUDC      ! ISBA water budget                            (kg/m2)
!
!* average budget summation variables
!
  REAL, POINTER, DIMENSION(:)   :: XAVG_RNC       ! net radiation at surface                     (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_HC        ! sensible heat flux                           (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LEC       ! total latent heat flux                       (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LEIC      ! sublimation latent heat flux                 (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_GFLUXC    ! net soil-vegetation flux                     (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LEGC      ! latent heat of evaporation over the ground   (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LEGIC     ! surface soil ice sublimation                 (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LEVC      ! latent heat of evaporation over vegetation   (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LESAC      ! latent heat of sublimation over the snow     (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LESLC     ! latent heat of evaporation over the snow     (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LERC      ! evaporation from canopy water interception   (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LETRC     ! evapotranspiration of the vegetation         (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_EVAPC     ! evapotranspiration                           (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_SUBLC     ! sublimation                                  (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_SNDRIFTC  ! blowing snow sublimation (ES or Crocus)      (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_DRAINC    ! soil drainage flux                           (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_QSBC      ! lateral subsurface flux (dif option)         (kg/m2) 
  REAL, POINTER, DIMENSION(:)   :: XAVG_RUNOFFC   ! sub-grid and supersaturation runoff          (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_HORTC     ! sub-grid Horton runoff from the SGH scheme   (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_DRIPC     ! dripping from the vegetation reservoir       (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_MELTC     ! snow melt                                    (kg/m2)   
  REAL, POINTER, DIMENSION(:)   :: XAVG_IFLOODC   ! Floodplains infiltration                     (kg/m2)      
  REAL, POINTER, DIMENSION(:)   :: XAVG_PFLOODC   ! Precipitation intercepted by the floodplains (kg/m2)      
  REAL, POINTER, DIMENSION(:)   :: XAVG_LE_FLOODC ! Floodplains evapotration                     (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LEI_FLOODC! Floodplains evapotration                     (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_ICEFLUXC  ! ice calving flux                             (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_RRVEGC    ! precipitation intercepted by the vegetation  (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_IRRIG_FLUXC! irrigation rate (as soil input)             (kg/m2)
!  
  REAL, POINTER, DIMENSION(:)   :: XAVG_GPPC      ! Gross Primary Production                     (kgCO2/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_RESPC_AUTO! Autotrophic respiration                      (kgCO2/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_RESPC_ECO ! Ecosystem respiration                        (kgCO2/m2)  
!
  REAL, POINTER, DIMENSION(:) :: XAVG_LEVCVC        ! MEB: total evapotranspiration from vegetation canopy overstory [J/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LESCC         ! MEB: total snow sublimation from vegetation canopy overstory [J/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LETRGVC       ! MEB: transpiration from understory vegetation [J/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LETRCVC       ! MEB: transpiration from overstory canopy vegetation [J/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LERGVC        ! MEB: interception evaporation from understory vegetation [J/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LERCVC        ! MEB: interception evaporation from overstory canopy vegetation [J/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LE_V_CC       ! MEB: latent heat flux from vegetation canopy overstory [J/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LE_G_CC       ! MEB: latent heat flux from understory [J/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LE_C_AC       ! MEB: latent heat flux from canopy air space to the atmosphere [J/m2] 
                                                  !      NOTE total latent heat flux to the atmosphere also possibly 
                                                  !      includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:) :: XAVG_LE_N_CC       ! MEB: latent heat flux from the snow on the ground [J/m2]
                                                  !      NOTE total latent heat flux from the snowpack
                                                  !      possibly includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:) :: XAVG_SWNET_VC      ! MEB: net vegetation canopy shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_SWNET_GC      ! MEB: net ground shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_SWNET_NC      ! MEB: net snow shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_SWNET_NSC     ! MEB: net snow shortwave radiation for *surface* layer 
                                                  !     (i.e. net snow shortwave radiation less absorbed radiation) [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LWNET_VC      ! MEB: net vegetation canopy longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LWNET_GC      ! MEB: net ground longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LWNET_NC      ! MEB: net snow longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_H_V_CC        ! MEB: sensible heat flux from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_H_G_CC        ! MEB: sensible heat flux from understory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_H_C_AC        ! MEB: sensible heat flux from canopy air space to the atmosphere [W/m2] 
                                                  !      NOTE total sensible heat flux to the atmosphere also possibly 
                                                  !      includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:) :: XAVG_H_N_CC        ! MEB: sensible heat flux from the snow on the ground [W/m2]
                                                  !      NOTE total sensible heat flux from the snowpack
                                                  !      possibly includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:) :: XAVG_SR_GNC        ! MEB: snow unloading rate from the overstory reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:) :: XAVG_MELTCVC       ! MEB: snow melt rate from the overstory snow reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:) :: XAVG_FRZCVC        ! MEB: snow refreeze rate from the overstory snow reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:) :: XAVG_SWDOWN_GNC    ! MEB: total shortwave radiation transmitted through the canopy
                                                  !      reaching the snowpack/ground understory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XAVG_LWDOWN_GNC    ! MEB: total shortwave radiation transmitted through and emitted by the canopy
                                                  !      reaching the snowpack/ground understory (explicit part) [W/m2]
!
  REAL, POINTER, DIMENSION(:)   :: XRAINFALLC     ! input rainfall rate for LWATER_BUDGET        (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XSNOWFALLC     ! input snowfall rate for LWATER_BUDGET        (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_DWGC      ! liquid soil moisture time tendencies         (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_DWGIC     ! solid soil moisture time tendencies          (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_DWRC      ! canopy water time tendencies                 (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_DSWEC     ! snow water equivalent time tendencies        (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_WATBUDC   ! ISBA water budget                            (kg/m2)
! 
!------------------------------------------------------------------------------
!

END TYPE DIAG_EVAP_ISBA_t



 CONTAINS
!




SUBROUTINE DIAG_EVAP_ISBA_INIT(YDIAG_EVAP_ISBA)
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: YDIAG_EVAP_ISBA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_EVAP_ISBA_N:DIAG_EVAP_ISBA_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YDIAG_EVAP_ISBA%XLEG)
  NULLIFY(YDIAG_EVAP_ISBA%XLEGI)
  NULLIFY(YDIAG_EVAP_ISBA%XLEV)
  NULLIFY(YDIAG_EVAP_ISBA%XLES)
  NULLIFY(YDIAG_EVAP_ISBA%XLESL)
  NULLIFY(YDIAG_EVAP_ISBA%XLER)
  NULLIFY(YDIAG_EVAP_ISBA%XLETR)
  NULLIFY(YDIAG_EVAP_ISBA%XEVAP)
  NULLIFY(YDIAG_EVAP_ISBA%XSUBL)
  NULLIFY(YDIAG_EVAP_ISBA%XSNDRIFT)
  NULLIFY(YDIAG_EVAP_ISBA%XDRAIN)
  NULLIFY(YDIAG_EVAP_ISBA%XQSB)
  NULLIFY(YDIAG_EVAP_ISBA%XRUNOFF)
  NULLIFY(YDIAG_EVAP_ISBA%XHORT)
  NULLIFY(YDIAG_EVAP_ISBA%XRRVEG)
  NULLIFY(YDIAG_EVAP_ISBA%XMELT)
  NULLIFY(YDIAG_EVAP_ISBA%XIFLOOD)
  NULLIFY(YDIAG_EVAP_ISBA%XPFLOOD)
  NULLIFY(YDIAG_EVAP_ISBA%XLE_FLOOD)
  NULLIFY(YDIAG_EVAP_ISBA%XLEI_FLOOD)
!
  NULLIFY(YDIAG_EVAP_ISBA%XLEVCV)
  NULLIFY(YDIAG_EVAP_ISBA%XLESC)
  NULLIFY(YDIAG_EVAP_ISBA%XLETRGV)
  NULLIFY(YDIAG_EVAP_ISBA%XLETRCV)
  NULLIFY(YDIAG_EVAP_ISBA%XLERGV)
  NULLIFY(YDIAG_EVAP_ISBA%XLELITTER)
  NULLIFY(YDIAG_EVAP_ISBA%XLELITTERI)
  NULLIFY(YDIAG_EVAP_ISBA%XDRIPLIT)
  NULLIFY(YDIAG_EVAP_ISBA%XRRLIT)
  NULLIFY(YDIAG_EVAP_ISBA%XLERCV)
  NULLIFY(YDIAG_EVAP_ISBA%XLE_V_C)
  NULLIFY(YDIAG_EVAP_ISBA%XLE_G_C)
  NULLIFY(YDIAG_EVAP_ISBA%XLE_C_A)
  NULLIFY(YDIAG_EVAP_ISBA%XLE_N_C)
!
  NULLIFY(YDIAG_EVAP_ISBA%XSWNET_V)
  NULLIFY(YDIAG_EVAP_ISBA%XSWNET_G)
  NULLIFY(YDIAG_EVAP_ISBA%XSWNET_N)
  NULLIFY(YDIAG_EVAP_ISBA%XSWNET_NS)
  NULLIFY(YDIAG_EVAP_ISBA%XLWNET_V)
  NULLIFY(YDIAG_EVAP_ISBA%XLWNET_G)
  NULLIFY(YDIAG_EVAP_ISBA%XLWNET_N)
  NULLIFY(YDIAG_EVAP_ISBA%XSWDOWN_GN)
  NULLIFY(YDIAG_EVAP_ISBA%XLWDOWN_GN)
  NULLIFY(YDIAG_EVAP_ISBA%XH_V_C)
  NULLIFY(YDIAG_EVAP_ISBA%XH_G_C)
  NULLIFY(YDIAG_EVAP_ISBA%XH_C_A)
  NULLIFY(YDIAG_EVAP_ISBA%XH_N_C)
  NULLIFY(YDIAG_EVAP_ISBA%XSR_GN)
  NULLIFY(YDIAG_EVAP_ISBA%XMELTCV)
  NULLIFY(YDIAG_EVAP_ISBA%XFRZCV)
!
  NULLIFY(YDIAG_EVAP_ISBA%XDRIP)
  NULLIFY(YDIAG_EVAP_ISBA%XIRRIG_FLUX)
  NULLIFY(YDIAG_EVAP_ISBA%XGPP)
  NULLIFY(YDIAG_EVAP_ISBA%XRESP_AUTO)
  NULLIFY(YDIAG_EVAP_ISBA%XRESP_ECO)  
  NULLIFY(YDIAG_EVAP_ISBA%XDWG)
  NULLIFY(YDIAG_EVAP_ISBA%XDWGI)
  NULLIFY(YDIAG_EVAP_ISBA%XDWR)
  NULLIFY(YDIAG_EVAP_ISBA%XDSWE)
  NULLIFY(YDIAG_EVAP_ISBA%XWATBUD)  
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LEG)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LEGI)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LEV)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LES)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LESL)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LER)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LETR)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_EVAP)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SUBL)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SNDRIFT)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_DRAIN)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_QSB)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_RUNOFF)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_HORT)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_DRIP)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_MELT)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_IFLOOD)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_PFLOOD)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LE_FLOOD)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LEI_FLOOD)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_RRVEG)
!
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LEVCV)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LESC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LETRGV)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LETRCV)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LERGV)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LELITTER)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LELITTERI)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_DRIPLIT)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_RRLIT)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LERCV)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LE_V_C)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LE_G_C)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LE_C_A)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LE_N_C)
!
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SWNET_V)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SWNET_G)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SWNET_N)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SWNET_NS)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LWNET_V)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LWNET_G)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LWNET_N)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SWDOWN_GN)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LWDOWN_GN)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_H_V_C)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_H_G_C)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_H_C_A)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_H_N_C)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SR_GN)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_MELTCV)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_FRZCV)
!
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_IRRIG_FLUX)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_GPP)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_RESP_AUTO)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_RESP_ECO)
  NULLIFY(YDIAG_EVAP_ISBA%XRAINFALL)
  NULLIFY(YDIAG_EVAP_ISBA%XSNOWFALL)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_DWG)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_DWGI)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_DWR)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_DSWE)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_WATBUD)
  NULLIFY(YDIAG_EVAP_ISBA%XRNC)
  NULLIFY(YDIAG_EVAP_ISBA%XHC)
  NULLIFY(YDIAG_EVAP_ISBA%XLEC)
  NULLIFY(YDIAG_EVAP_ISBA%XLEIC)
  NULLIFY(YDIAG_EVAP_ISBA%XGFLUXC)
  NULLIFY(YDIAG_EVAP_ISBA%XLEGC)
  NULLIFY(YDIAG_EVAP_ISBA%XLEGIC)
  NULLIFY(YDIAG_EVAP_ISBA%XLEVC)
  NULLIFY(YDIAG_EVAP_ISBA%XLESAC)
  NULLIFY(YDIAG_EVAP_ISBA%XLESLC)
  NULLIFY(YDIAG_EVAP_ISBA%XLERC)
  NULLIFY(YDIAG_EVAP_ISBA%XLETRC)
  NULLIFY(YDIAG_EVAP_ISBA%XEVAPC)
  NULLIFY(YDIAG_EVAP_ISBA%XSUBLC)
  NULLIFY(YDIAG_EVAP_ISBA%XSNDRIFTC)
  NULLIFY(YDIAG_EVAP_ISBA%XDRAINC)
  NULLIFY(YDIAG_EVAP_ISBA%XQSBC)
  NULLIFY(YDIAG_EVAP_ISBA%XRUNOFFC)
  NULLIFY(YDIAG_EVAP_ISBA%XHORTC)
  NULLIFY(YDIAG_EVAP_ISBA%XDRIPC)
  NULLIFY(YDIAG_EVAP_ISBA%XMELTC)
  NULLIFY(YDIAG_EVAP_ISBA%XIFLOODC)
  NULLIFY(YDIAG_EVAP_ISBA%XPFLOODC)
  NULLIFY(YDIAG_EVAP_ISBA%XLE_FLOODC)
  NULLIFY(YDIAG_EVAP_ISBA%XLEI_FLOODC)
  NULLIFY(YDIAG_EVAP_ISBA%XICEFLUXC)
  NULLIFY(YDIAG_EVAP_ISBA%XRRVEGC)
!
  NULLIFY(YDIAG_EVAP_ISBA%XLEVCVC)
  NULLIFY(YDIAG_EVAP_ISBA%XLESCC)
  NULLIFY(YDIAG_EVAP_ISBA%XLETRGVC)
  NULLIFY(YDIAG_EVAP_ISBA%XLETRCVC)
  NULLIFY(YDIAG_EVAP_ISBA%XLERGVC)
  NULLIFY(YDIAG_EVAP_ISBA%XLERCVC)
  NULLIFY(YDIAG_EVAP_ISBA%XLE_V_CC)
  NULLIFY(YDIAG_EVAP_ISBA%XLE_G_CC)
  NULLIFY(YDIAG_EVAP_ISBA%XLE_C_AC)
  NULLIFY(YDIAG_EVAP_ISBA%XLE_N_CC)
!
  NULLIFY(YDIAG_EVAP_ISBA%XSWNET_VC)
  NULLIFY(YDIAG_EVAP_ISBA%XSWNET_GC)
  NULLIFY(YDIAG_EVAP_ISBA%XSWNET_NC)
  NULLIFY(YDIAG_EVAP_ISBA%XSWNET_NSC)
  NULLIFY(YDIAG_EVAP_ISBA%XLWNET_VC)
  NULLIFY(YDIAG_EVAP_ISBA%XLWNET_GC)
  NULLIFY(YDIAG_EVAP_ISBA%XLWNET_NC)
  NULLIFY(YDIAG_EVAP_ISBA%XSWDOWN_GNC)
  NULLIFY(YDIAG_EVAP_ISBA%XLWDOWN_GNC)
  NULLIFY(YDIAG_EVAP_ISBA%XH_V_CC)
  NULLIFY(YDIAG_EVAP_ISBA%XH_G_CC)
  NULLIFY(YDIAG_EVAP_ISBA%XH_C_AC)
  NULLIFY(YDIAG_EVAP_ISBA%XH_N_CC)
  NULLIFY(YDIAG_EVAP_ISBA%XSR_GNC)
  NULLIFY(YDIAG_EVAP_ISBA%XMELTCVC)
  NULLIFY(YDIAG_EVAP_ISBA%XFRZCVC)
!
  NULLIFY(YDIAG_EVAP_ISBA%XIRRIG_FLUXC)
  NULLIFY(YDIAG_EVAP_ISBA%XGPPC)
  NULLIFY(YDIAG_EVAP_ISBA%XRESPC_AUTO)
  NULLIFY(YDIAG_EVAP_ISBA%XRESPC_ECO) 
  NULLIFY(YDIAG_EVAP_ISBA%XDWGC)
  NULLIFY(YDIAG_EVAP_ISBA%XDWGIC)
  NULLIFY(YDIAG_EVAP_ISBA%XDWRC)
  NULLIFY(YDIAG_EVAP_ISBA%XDSWEC)
  NULLIFY(YDIAG_EVAP_ISBA%XWATBUDC) 
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_RNC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_HC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LEC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LEIC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_GFLUXC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LEGC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LEGIC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LEVC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LESAC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LESLC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LERC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LETRC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_EVAPC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SUBLC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SNDRIFTC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_DRAINC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_QSBC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_RUNOFFC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_HORTC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_DRIPC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_MELTC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_IFLOODC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_PFLOODC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LE_FLOODC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LEI_FLOODC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_ICEFLUXC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_RRVEGC)
!
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LEVCVC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LESCC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LETRGVC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LETRCVC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LERGVC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LERCVC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LE_V_CC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LE_G_CC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LE_C_AC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LE_N_CC)
!
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SWNET_VC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SWNET_GC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SWNET_NC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SWNET_NSC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LWNET_VC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LWNET_GC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LWNET_NC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SWDOWN_GNC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_LWDOWN_GNC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_H_V_CC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_H_G_CC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_H_C_AC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_H_N_CC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_SR_GNC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_MELTCVC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_FRZCVC)
!
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_IRRIG_FLUXC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_GPPC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_RESPC_AUTO)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_RESPC_ECO)  
  NULLIFY(YDIAG_EVAP_ISBA%XRAINFALLC)
  NULLIFY(YDIAG_EVAP_ISBA%XSNOWFALLC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_DWGC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_DWGIC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_DWRC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_DSWEC)
  NULLIFY(YDIAG_EVAP_ISBA%XAVG_WATBUDC)  
YDIAG_EVAP_ISBA%LSURF_EVAP_BUDGET=.FALSE.
YDIAG_EVAP_ISBA%LSURF_BUDGETC=.FALSE.
YDIAG_EVAP_ISBA%LRESET_BUDGETC=.FALSE.
YDIAG_EVAP_ISBA%LWATER_BUDGET=.FALSE.
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_EVAP_ISBA_N:DIAG_EVAP_ISBA_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DIAG_EVAP_ISBA_INIT


END MODULE MODD_DIAG_EVAP_ISBA_n
