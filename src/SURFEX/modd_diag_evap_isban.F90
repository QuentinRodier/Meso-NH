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
!!      R. Séférian    08/2016   Riverine, carbon leaching and land use module
!!      C. Delire      08/2016   Add turnover diags
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
  LOGICAL :: LWATER_BUDGET       ! flag for isba water budget including input  
                                 ! fluxes (rain and snow) and reservoir tendencies
  LOGICAL :: LENERGY_BUDGET      ! flag for isba energy budget including surface and snow budgets
!
!* variables for each patch
!
  REAL, POINTER, DIMENSION(:)   :: XLEG          ! latent heat of evaporation over the ground                  (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLEGI         ! latent heat of surface soil ice sublimation                 (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLEV          ! latent heat of evaporation over vegetation                  (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLES          ! latent heat of sublimation over the snow                    (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLESL         ! latent heat of evaporation over the snow                    (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLER          ! latent heat of evaporation from canopy water interception   (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLETR         ! latent heat of evapotranspiration of the vegetation         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLE_FLOOD     ! latent heat of Floodplains evapotration                     (W/m2)      
  REAL, POINTER, DIMENSION(:)   :: XLEI_FLOOD    ! latent heat of Floodplains evapotration                     (W/m2) 
!
  REAL, POINTER, DIMENSION(:)   :: XEPOT         ! potential evaporatio    (kg/m2/s)
!
  REAL, POINTER, DIMENSION(:)   :: XUSTAR        ! friction velocity from snow-free surface (ISBA-ES:3-L) (m/s)  
  REAL, POINTER, DIMENSION(:)   :: XSNDRIFT      ! blowing snow sublimation (ES or Crocus)      (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XDRAIN        ! soil drainage flux                           (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XQSB          ! lateral subsurface flux (dif option)         (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XRUNOFF       ! sub-grid and supersaturation runoff          (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XHORT         ! sub-grid Horton runoff from the SGH scheme   (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XRRVEG        !  precipitation intercepted by the vegetation (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XMELT         ! snow melt leaving the snowpack               (kg/m2/s) 
  REAL, POINTER, DIMENSION(:)   :: XMELTSTOT     ! snow melt over the entire snowpack           (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XSNREFREEZ    ! refreezing of water in the entire snowpack   (kg/m2/s)      
!  
  REAL, POINTER, DIMENSION(:)   :: XMELTADV      ! advective energy from snow melt water (ISBA-ES:3-L) (W/m2)
!
  REAL, POINTER, DIMENSION(:)   :: XIFLOOD       ! Floodplains infiltration                     (kg/m2/s)      
  REAL, POINTER, DIMENSION(:)   :: XPFLOOD       ! Precipitation intercepted by the floodplains (kg/m2/s)      
  REAL, POINTER, DIMENSION(:)   :: XICEFLUX      ! ice calving flux                             (kg/m2/s)  
  REAL, POINTER, DIMENSION(:)   :: XDRIP         ! dripping from the vegetation reservoir       (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XIRRIG_FLUX   ! irrigation rate (as soil input)              (kg/m2/s)
!
  REAL, POINTER, DIMENSION(:)   :: XLELITTER     ! MEB: interception evaporation from understory vegetation [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XLELITTERI    ! MEB: interception evaporation from understory vegetation [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XDRIPLIT      ! 
  REAL, POINTER, DIMENSION(:)   :: XRRLIT        ! 
!
  REAL, POINTER, DIMENSION(:)   :: XLEV_CV        ! MEB: total evapotranspiration from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XLES_CV        ! XLESC MEB: total snow sublimation from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XLETR_CV       ! MEB: transpiration from overstory canopy vegetation [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XLER_CV        ! MEB: interception evaporation from overstory canopy vegetation [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XLE_CV         ! XLE_V_C MEB: latent heat flux from vegetation canopy overstory [W/m2]  
  REAL, POINTER, DIMENSION(:)   :: XH_CV          ! H_V_C MEB: sensible heat flux from vegetation canopy overstory [W/m2] 
  REAL, POINTER, DIMENSION(:)   :: XMELT_CV       ! MEB: snow melt rate from the overstory snow reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:)   :: XFRZ_CV        ! MEB: snow refreeze rate from the overstory snow reservoir [kg/m2/s]  
!
  REAL, POINTER, DIMENSION(:)   :: XLETR_GV       ! MEB: transpiration from understory vegetation [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XLER_GV        ! MEB: interception evaporation from understory vegetation [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XLE_GV         ! LEG_C MEB: latent heat flux from understory [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XH_GV          ! H_G_C MEB: sensible heat flux from understory [W/m2]  
!
  REAL, POINTER, DIMENSION(:)   :: XLE_GN         ! LE_N_C MEB: latent heat flux from the snow on the ground [W/m2]
                                                  !      NOTE total latent heat flux from the snowpack
                                                  !      possibly includes a contribution from snow covering the canopy  
  REAL, POINTER, DIMENSION(:)   :: XEVAP_GN       ! EVAP_N_C MEB: Total evap from snow on the ground to canopy air space  [kg/m2/s]
  REAL, POINTER, DIMENSION(:)   :: XH_GN          ! H_N_C MEB: sensible heat flux from the snow on the ground [W/m2]
                                                  !      NOTE total sensible heat flux from the snowpack
                                                  !      possibly includes a contribution from snow covering the canopy   
  REAL, POINTER, DIMENSION(:)   :: XSR_GN         ! MEB: snow unloading rate from the overstory reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:)   :: XSWDOWN_GN     ! MEB: total shortwave radiation transmitted through the canopy
                                                  !      reaching the snowpack/ground understory [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XLWDOWN_GN     ! MEB: total shortwave radiation transmitted through and emitted by the canopy
                                                  !      reaching the snowpack/ground understory (explicit part) [W/m2]                                               
!
  REAL, POINTER, DIMENSION(:)   :: XEVAP_G        ! EVAP_G_C MEB: Total evap from ground to canopy air space [kg/m2/s]
  REAL, POINTER, DIMENSION(:)   :: XLE_CA         ! LE_C_A MEB: latent heat flux from canopy air space to the atmosphere [W/m2] 
                                                  !      NOTE total latent heat flux to the atmosphere also possibly 
                                                  !      includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:)   :: XH_CA          ! H_C_A MEB: sensible heat flux from canopy air space to the atmosphere [W/m2] 
                                                  !      NOTE total sensible heat flux to the atmosphere also possibly 
                                                  !      includes a contribution from snow covering the canopy
                                       
!
  REAL, POINTER, DIMENSION(:)   :: XSWNET_N      ! net snow shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XSWNET_NS     ! net snow shortwave radiation for *surface* layer 
                                                 ! (i.e. net snow shortwave radiation less absorbed radiation) [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XLWNET_N      ! net snow longwave radiation [W/m2]

  REAL, POINTER, DIMENSION(:)   :: XSWUP         ! MEB: net *total* (surface) upwelling shortwave radiation to atmosphere [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XLWUP         ! MEB: net *total* (surface) upwelling longwave radiation to atmosphere [W/m2]     

  REAL, POINTER, DIMENSION(:)   :: XSWNET_V      ! MEB: net vegetation canopy shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XSWNET_G      ! MEB: net ground shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XLWNET_V      ! MEB: net vegetation canopy longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:)   :: XLWNET_G      ! MEB: net ground longwave radiation [W/m2]
!
  REAL, POINTER, DIMENSION(:)   :: XDWG          ! liquid soil moisture time tendencies         (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XDWGI         ! solid soil moisture time tendencies          (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XDWR          ! canopy water time tendencies                 (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XDSWE         ! snow water equivalent time tendencies        (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XDSWFREE      ! free surface water time tendencies           (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XWATBUD       ! ISBA water budget                            (kg/m2/s)
!
  REAL, POINTER, DIMENSION(:)   :: XRAINFALL     ! input rainfall rate for LWATER_BUDGET        (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XSNOWFALL     ! input snowfall rate for LWATER_BUDGET        (kg/m2/s)
!
  REAL, POINTER, DIMENSION(:)   :: XNRJBUD       ! ISBA energy budget                           (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XNRJBUD_SFC   ! ISBA energy budget at surface                (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XGRNDFLUX     ! ISBA soil/snow interface heat flux           (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XRESTORE      ! ISBA surface energy budget restore term      (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XRESTOREN     ! ISBA surface snow energy budget restore term (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XDELHEATG     ! ISBA change in soil heat storage             (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XDELHEATN     ! ISBA change in snow heat storage             (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XDELPHASEG    ! ISBA soil energy of fusion (solid to liquid) (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XDELPHASEN    ! ISBA snow energy of fusion (solid to liquid) (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XDELHEATG_SFC ! ISBA change in soil surface heat storage     (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XDELHEATN_SFC ! ISBA change in snow surface heat storage     (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XDELPHASEG_SFC! ISBA soil surface energy of fusion (solid to liquid) (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XDELPHASEN_SFC! ISBA snow surface energy of fusion (solid to liquid) (W/m2)
!
! Snowfree diagnostics
!
  REAL, POINTER, DIMENSION(:)   :: XRN_SN_FR
  REAL, POINTER, DIMENSION(:)   :: XH_SN_FR
  REAL, POINTER, DIMENSION(:)   :: XLEI_SN_FR
  REAL, POINTER, DIMENSION(:)   :: XLE_SN_FR
  REAL, POINTER, DIMENSION(:)   :: XGFLUX_SN_FR
!  
  REAL, POINTER, DIMENSION(:)   :: XLEG_SN_FR
  REAL, POINTER, DIMENSION(:)   :: XLEGI_SN_FR
  REAL, POINTER, DIMENSION(:)   :: XLEV_SN_FR
  REAL, POINTER, DIMENSION(:)   :: XLETR_SN_FR
  REAL, POINTER, DIMENSION(:)   :: XUSTAR_SN_FR
  REAL, POINTER, DIMENSION(:)   :: XLER_SN_FR
!  
  REAL, POINTER, DIMENSION(:)   :: XSNFREE_SWU ! upward snow-free short wave radiation (W/m2)
!
! ISBA-CC
!
  REAL, POINTER, DIMENSION(:)   :: XGPP          ! Gross Primary Production          (kgCO2/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XRESP_AUTO    ! Autotrophic respiration           (kgCO2/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XRESP_ECO     ! Ecosystem respiration             (kgCO2/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XTURNVTOT     ! Total biomass turnover            (kgC/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XFLTOSCARB    ! transfer of carbon litter to soil (kgC/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XRESPSCARB    ! Hetero respiration from soil      (kgCO2/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XRESPLIT      ! Hetero respiration from litter    (kgCO2/m2/s)
!
  REAL, POINTER, DIMENSION(:)   :: XFDOCLIT      ! Surface dissolved organic carbon flux (kgC/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XFDOC         ! Total dissolved organic carbon flux   (kgC/m2/s)
!
  REAL, POINTER, DIMENSION(:)   :: XFIRECO2      ! carbon emitted into the atmosphere by fire       (kgC/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XFIREBCS      ! black carbon emitted into the atmosphere by fire (kgC/m2/s) 
  REAL, POINTER, DIMENSION(:)   :: XFIRETURNOVER ! conversion of biomass to litter                  (kgC/m2/s) 
!
  REAL, POINTER, DIMENSION(:)   :: XFHARVEST     ! Harvested carbon flux (kgC/m2/s)
!
! Soil gas scheme
!
  REAL, POINTER, DIMENSION(:) :: XO2FLUX       ! O2 flux from soil and vegetation (kgO2/m2/s)
  REAL, POINTER, DIMENSION(:) :: XCH4FLUX      ! CH4 flux from soil and vegetation (kgCH4/m2/s)
!
  REAL, POINTER, DIMENSION(:) :: XSURF_O2      ! O2 transported by diffusion at soil/atmosphere interface (kgO2/m2/s)
  REAL, POINTER, DIMENSION(:) :: XSURF_CO2     ! CO2 transported by diffusion at soil/atmosphere interface (kgCO2/m2/s)
  REAL, POINTER, DIMENSION(:) :: XSURF_CH4     ! CH4 transported by diffusion at soil/atmosphere interface (kgCH4/m2/s)
  REAL, POINTER, DIMENSION(:) :: XEVAP_O2      ! O2 transported by evapotranspiration (kgO2/m2/s)
  REAL, POINTER, DIMENSION(:) :: XEVAP_CO2     ! CO2 transported by evapotranspiration (kgCO2/m2/s)
  REAL, POINTER, DIMENSION(:) :: XEVAP_CH4     ! CH4 transported by evapotranspiration (kgCH4/m2/s)
  REAL, POINTER, DIMENSION(:) :: XPMT_O2       ! O2 transported by plant (kgO2/m2/s)
  REAL, POINTER, DIMENSION(:) :: XPMT_CO2      ! CO2 transported by plant (kgCO2/m2/s)
  REAL, POINTER, DIMENSION(:) :: XPMT_CH4      ! CH4 transported by plant (kgCH4/m2/s)
  REAL, POINTER, DIMENSION(:) :: XEBU_CH4      ! CH4 transported by ebullition (kgCH4/m2/s)
!
  REAL, POINTER, DIMENSION(:) :: XFCONS_O2     ! O2 consumed during oxic decomposition and methanotrophy (kgO2/m2/s)
  REAL, POINTER, DIMENSION(:) :: XFPROD_CO2    ! CO2 produced during oxic decomposition and methanotrophy (kgCO2/m2/s)
  REAL, POINTER, DIMENSION(:) :: XFMT_CH4      ! CH4 consumed during methanotrophy (kgCH4/m2/s)
  REAL, POINTER, DIMENSION(:) :: XFMG_CH4      ! CH4 produced during methanogenesis (kgCH4/m2/s)
!
!------------------------------------------------------------------------------
!
END TYPE DIAG_EVAP_ISBA_t
!
TYPE DIAG_EVAP_ISBA_NP_t
!
TYPE(DIAG_EVAP_ISBA_t), POINTER :: AL(:)=>NULL()
!
END TYPE DIAG_EVAP_ISBA_NP_t
!
!------------------------------------------------------------------------------
CONTAINS
!------------------------------------------------------------------------------
!
SUBROUTINE DIAG_EVAP_ISBA_NP_INIT(YNDIAG_EVAP_ISBA,KPATCH)
!
TYPE(DIAG_EVAP_ISBA_NP_t), INTENT(INOUT) :: YNDIAG_EVAP_ISBA
!
INTEGER, INTENT(IN) :: KPATCH
!
INTEGER :: JP
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_EVAP_ISBA_N:DIAG_EVAP_ISBA_NP_INIT",0,ZHOOK_HANDLE)
!
IF (.NOT.ASSOCIATED(YNDIAG_EVAP_ISBA%AL)) THEN
   ALLOCATE(YNDIAG_EVAP_ISBA%AL(KPATCH))
   DO JP=1,KPATCH
      CALL DIAG_EVAP_ISBA_INIT(YNDIAG_EVAP_ISBA%AL(JP))
   ENDDO
ELSE
   DO JP=1,KPATCH
      CALL DIAG_EVAP_ISBA_INIT(YNDIAG_EVAP_ISBA%AL(JP))
   ENDDO
   DEALLOCATE(YNDIAG_EVAP_ISBA%AL)
ENDIF  
!
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_EVAP_ISBA_N:DIAG_EVAP_ISBA_NP_INIT",1,ZHOOK_HANDLE)
!
END SUBROUTINE DIAG_EVAP_ISBA_NP_INIT
!
!------------------------------------------------------------------------------
!
SUBROUTINE DIAG_EVAP_ISBA_INIT(DE)
!
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_EVAP_ISBA_N:DIAG_EVAP_ISBA_INIT",0,ZHOOK_HANDLE)
!
NULLIFY(DE%XLEG)
NULLIFY(DE%XLEGI)
NULLIFY(DE%XLEV)
NULLIFY(DE%XLES)
NULLIFY(DE%XLESL)
NULLIFY(DE%XLER)
NULLIFY(DE%XLETR)
NULLIFY(DE%XLE_FLOOD)
NULLIFY(DE%XLEI_FLOOD)
!
NULLIFY(DE%XEPOT)
!
NULLIFY(DE%XUSTAR)
NULLIFY(DE%XSNDRIFT)
NULLIFY(DE%XDRAIN)
NULLIFY(DE%XQSB)
NULLIFY(DE%XRUNOFF)
NULLIFY(DE%XHORT)
NULLIFY(DE%XRRVEG)
NULLIFY(DE%XMELT)
NULLIFY(DE%XMELTSTOT)
NULLIFY(DE%XSNREFREEZ)
NULLIFY(DE%XMELTADV)
NULLIFY(DE%XIFLOOD)
NULLIFY(DE%XPFLOOD)
!
NULLIFY(DE%XICEFLUX)  
!
NULLIFY(DE%XLELITTER)
NULLIFY(DE%XLELITTERI)
NULLIFY(DE%XDRIPLIT)
NULLIFY(DE%XRRLIT)

NULLIFY(DE%XLEV_CV)
NULLIFY(DE%XLES_CV)
NULLIFY(DE%XLETR_CV)
NULLIFY(DE%XLER_CV)
NULLIFY(DE%XLE_CV)
NULLIFY(DE%XH_CV)
NULLIFY(DE%XMELT_CV)
NULLIFY(DE%XFRZ_CV)  

NULLIFY(DE%XLETR_GV)
NULLIFY(DE%XLER_GV)
NULLIFY(DE%XLE_GV)  
NULLIFY(DE%XH_GV)  

NULLIFY(DE%XLE_GN)  
NULLIFY(DE%XEVAP_GN)
NULLIFY(DE%XH_GN)  
NULLIFY(DE%XSR_GN)  
NULLIFY(DE%XSWDOWN_GN)
NULLIFY(DE%XLWDOWN_GN)  

NULLIFY(DE%XEVAP_G)  
NULLIFY(DE%XLE_CA)
NULLIFY(DE%XH_CA)
  
NULLIFY(DE%XSWUP)
NULLIFY(DE%XLWUP)
  
NULLIFY(DE%XSWNET_V)
NULLIFY(DE%XSWNET_G)
NULLIFY(DE%XSWNET_N)
NULLIFY(DE%XSWNET_NS)
NULLIFY(DE%XLWNET_V)
NULLIFY(DE%XLWNET_G)
NULLIFY(DE%XLWNET_N)
!
NULLIFY(DE%XDRIP)
NULLIFY(DE%XIRRIG_FLUX)
NULLIFY(DE%XGPP)
NULLIFY(DE%XRESP_AUTO)
NULLIFY(DE%XRESP_ECO)  
NULLIFY(DE%XTURNVTOT)
NULLIFY(DE%XFLTOSCARB)
NULLIFY(DE%XRESPSCARB)
NULLIFY(DE%XRESPLIT)
!
NULLIFY(DE%XFDOCLIT)
NULLIFY(DE%XFDOC)
NULLIFY(DE%XFIRECO2)
NULLIFY(DE%XFIREBCS) 
NULLIFY(DE%XFIRETURNOVER) 
NULLIFY(DE%XFHARVEST)
!
NULLIFY(DE%XDWG)
NULLIFY(DE%XDWGI)
NULLIFY(DE%XDWR)
NULLIFY(DE%XDSWE)
NULLIFY(DE%XDSWFREE)
NULLIFY(DE%XWATBUD)  
!
NULLIFY(DE%XRAINFALL)
NULLIFY(DE%XSNOWFALL)
!
NULLIFY(DE%XRN_SN_FR)
NULLIFY(DE%XH_SN_FR)
NULLIFY(DE%XLEI_SN_FR)
NULLIFY(DE%XLE_SN_FR)
NULLIFY(DE%XGFLUX_SN_FR)
NULLIFY(DE%XLEG_SN_FR)
NULLIFY(DE%XLEGI_SN_FR)
NULLIFY(DE%XLEV_SN_FR)
NULLIFY(DE%XLETR_SN_FR)
NULLIFY(DE%XUSTAR_SN_FR)
NULLIFY(DE%XLER_SN_FR)
!
NULLIFY(DE%XSNFREE_SWU)
!
NULLIFY(DE%XNRJBUD)
NULLIFY(DE%XNRJBUD_SFC)
NULLIFY(DE%XGRNDFLUX)
NULLIFY(DE%XRESTORE)
NULLIFY(DE%XRESTOREN)
NULLIFY(DE%XDELHEATG)
NULLIFY(DE%XDELHEATN)
NULLIFY(DE%XDELPHASEG)
NULLIFY(DE%XDELPHASEN)
NULLIFY(DE%XDELHEATG_SFC)
NULLIFY(DE%XDELHEATN_SFC)
NULLIFY(DE%XDELPHASEG_SFC)
NULLIFY(DE%XDELPHASEN_SFC)
! 
NULLIFY(DE%XO2FLUX)
NULLIFY(DE%XCH4FLUX)
!
NULLIFY(DE%XSURF_O2)
NULLIFY(DE%XSURF_CO2)
NULLIFY(DE%XSURF_CH4)
NULLIFY(DE%XEVAP_O2)
NULLIFY(DE%XEVAP_CO2)
NULLIFY(DE%XEVAP_CH4)
NULLIFY(DE%XPMT_O2)
NULLIFY(DE%XPMT_CO2)
NULLIFY(DE%XPMT_CH4)
NULLIFY(DE%XEBU_CH4)
!
NULLIFY(DE%XFCONS_O2)
NULLIFY(DE%XFPROD_CO2)
NULLIFY(DE%XFMT_CH4)
NULLIFY(DE%XFMG_CH4)
!
DE%LSURF_EVAP_BUDGET=.FALSE.
DE%LWATER_BUDGET    =.FALSE.
DE%LENERGY_BUDGET   =.FALSE.
!
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_EVAP_ISBA_N:DIAG_EVAP_ISBA_INIT",1,ZHOOK_HANDLE)
!
END SUBROUTINE DIAG_EVAP_ISBA_INIT
!
!------------------------------------------------------------------------------
!
END MODULE MODD_DIAG_EVAP_ISBA_n
