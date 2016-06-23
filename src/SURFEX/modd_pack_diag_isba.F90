!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!######################
MODULE MODD_PACK_DIAG_ISBA
!######################
!
!!****  *MODD_PACK_DIAG_ISBA - declaration of packed diagnostics for ISBA scheme
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2004
!!      Modified       10/2004 by P. Le Moigne: add Halstead coefficient
!!      Modified       11/2009 by S. Senesi: add precipitation intercepted by the vegetation (XP_RRVEG)
!!      Modified       04-09 by A.L. Gibelin  : Add carbon diagnostics
!!      Modified       10-14 by P. Samuelsson: MEB
!
!*       0.   DECLARATIONS
!             ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!------------------------------------------------------------------------------
!
TYPE PACK_DIAG_ISBA_t

  INTEGER :: NSIZE_SIMPLE
  INTEGER :: NSIZE_GROUND
  INTEGER :: NSIZE_SNOW
  INTEGER :: NSIZE_KSW
  INTEGER :: NSIZE_ABC
  INTEGER :: NSIZE_0
  INTEGER :: NSIZE_00
  REAL, POINTER, DIMENSION(:,:) :: XBLOCK_SIMPLE
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_GROUND
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_SNOW
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_KSW
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_ABC
  REAL, POINTER, DIMENSION(:,:) :: XBLOCK_0
  REAL, POINTER, DIMENSION(:,:,:) :: XBLOCK_00
!
  REAL, POINTER, DIMENSION(:) :: XP_RNSNOW    ! net radiative flux from snow (ISBA-ES:3-L)    (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_HSNOW     ! sensible heat flux from snow (ISBA-ES:3-L)    (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_HPSNOW    ! heat release from rainfall (ISBA-ES:3-L)      (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_GFLUXSNOW ! net surface energy flux into snowpack      
!                                               ! (ISBA-ES:3-L)                                 (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_USTARSNOW ! friction velocity  over snow 
!                                               ! (ISBA-ES:3-L)                                 (m/s)
  REAL, POINTER, DIMENSION(:) :: XP_GRNDFLUX  ! soil/snow interface heat flux (ISBA-ES:3-L)   (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_SRSFC     ! snowfall over snowpack (ISBA-ES:3-L)          (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_RRSFC     ! rainfall over snowpack (ISBA-ES:3-L)          (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_LESL      ! snowpack evaporation (ISBA-ES:3-L)            (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_CDSNOW    ! snow drag coefficient (ISBA-ES:3-L)           (-)
  REAL, POINTER, DIMENSION(:) :: XP_CHSNOW    ! heat turbulent transfer coefficient 
!                                               ! (ISBA-ES:3-L)                                 (-)
  REAL, POINTER, DIMENSION(:,:)::XP_SNOWTEMP  ! snow temperature profile (ISBA-ES:3-L)        (K)
  REAL, POINTER, DIMENSION(:,:)::XP_SNOWLIQ   ! snow liquid water profile (ISBA-ES:3-L)       (m)
  REAL, POINTER, DIMENSION(:,:)::XP_SNOWDZ    ! snow layer thicknesses                        (m)
  REAL, POINTER, DIMENSION(:) :: XP_SNOWHMASS ! heat content change due to mass
!                                               ! changes in snowpack: for budget
!                                               ! calculations only. (ISBA-ES:3-L)              (J/m2)
  REAL, POINTER, DIMENSION(:) :: XP_RN_ISBA   ! net radiative flux from snow-free surface 
!                                               ! (ISBA-ES:3-L)                                 (W/m2) 
  REAL, POINTER, DIMENSION(:) :: XP_H_ISBA    ! sensible heat flux from snow-free surface 
!                                               ! (ISBA-ES:3-L)                                 (W/m2) 
  REAL, POINTER, DIMENSION(:) :: XP_LEG_ISBA  ! baresoil evaporation from snow-free surface 
!                                               ! (ISBA-ES:3-L)                                 (W/m2) 
  REAL, POINTER, DIMENSION(:) :: XP_LEGI_ISBA ! baresoil sublimation from snow-free surface 
!                                               ! (ISBA-ES:3-L)                                 (W/m2) 
  REAL, POINTER, DIMENSION(:) :: XP_LEV_ISBA  ! total evapotranspiration from vegetation over 
!                                               ! snow-free surface (ISBA-ES:3-L)               (W/m2) 
  REAL, POINTER, DIMENSION(:) :: XP_LETR_ISBA ! transpiration from snow-free surface 
!                                               ! (ISBA-ES:3-L)                                 (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_USTAR_ISBA! friction velocity from snow-free 
!                                               ! surface (ISBA-ES:3-L)                         (m/s)
  REAL, POINTER, DIMENSION(:) :: XP_LER_ISBA  ! evaporation from canopy water interception 
!                                               ! store over snow-free surface (ISBA-ES:3-L)    (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_LE_ISBA   ! total latent heat flux from snow-free surface 
  REAL, POINTER, DIMENSION(:) :: XP_LEI_ISBA  ! sublimation latent heat flux from snow-free surface 
!                                               ! (ISBA-ES:3-L)                                 (W/m2) 
  REAL, POINTER, DIMENSION(:) :: XP_GFLUX_ISBA! net energy flux into the snow-free surface 
!                                               ! (ISBA-ES:3-L)                                 (W/m2) 
  REAL, POINTER, DIMENSION(:) :: XP_MELTADV   ! advective energy from snow melt water 
!                                               ! (ISBA-ES:3-L)                                 (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_CH        ! thermal diffusion coefficient                 (W/s)
  REAL, POINTER, DIMENSION(:) :: XP_CE        ! transfer coefficient for vapor                (W/s/K)
  REAL, POINTER, DIMENSION(:) :: XP_CD        ! drag coefficient                              (-)
  REAL, POINTER, DIMENSION(:) :: XP_CDN       ! neutral drag coefficient                      (-)
  REAL, POINTER, DIMENSION(:) :: XP_RI        ! Bulk-Richardson number                        (-)
  REAL, POINTER, DIMENSION(:) :: XP_HU        ! area averaged surface humidity coefficient    (-)
  REAL, POINTER, DIMENSION(:) :: XP_HUG       ! baresoil surface humidity coefficient         (-)
  REAL, POINTER, DIMENSION(:) :: XP_HV        ! Halstead coefficient                          (-)
!
  REAL, POINTER, DIMENSION(:) :: XP_ALBT      ! Total Albedo                                  (-)
!
  REAL, POINTER, DIMENSION(:) :: XP_RN        ! net radiation at surface                      (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_H         ! sensible heat flux                            (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_LEG       ! baresoil evaporation                          (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_LEGI      ! baresoil sublimation                          (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_LEV       ! total evapotranspiration from vegetation      (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_LES       ! snow sublimation                              (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_LER       ! evaporation from canopy water interception 
!                                               ! store                                         (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_LETR      ! transpiration                                 (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_EVAP      ! evapotranspiration                            (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_SUBL      ! sublimation                                   (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_SNDRIFT   ! blowing snow sublimation (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_LEI       ! sublimation latent heat flux                  (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_GFLUX     ! net soil-vegetation flux                      (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_RESTORE   ! surface energy budget restore term            (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_DRAIN     ! soil drainage flux                            (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_QSB       ! lateral subsurface flux                       (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_RUNOFF    ! sub-grid and supersaturation runoff           (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_MELT      ! melting rate of the snow                      (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_SNOWFREE_ALB ! snow-free global albedo                    (-)
  REAL, POINTER, DIMENSION(:) :: XP_SNOWFREE_ALB_VEG ! snow-free global  albedo of vegetation
  REAL, POINTER, DIMENSION(:) :: XP_SNOWFREE_ALB_SOIL! snow-free soil albedo
  REAL, POINTER, DIMENSION(:) :: XP_Z0_WITH_SNOW ! total roughness length (including snow)    (m)
  REAL, POINTER, DIMENSION(:) :: XP_Z0H_WITH_SNOW! roughness length for heat (including snow) (m)
  REAL, POINTER, DIMENSION(:) :: XP_Z0EFF     ! effective roughness length (with relief added)(m)
!
  REAL, POINTER, DIMENSION(:,:)::XP_IACAN     ! PAR in the canopy at different gauss level    (micmolphot/m2/s)
!
  REAL, POINTER, DIMENSION(:) :: XP_CG        ! heat capacity of the ground
  REAL, POINTER, DIMENSION(:) :: XP_C1        ! coefficients for the moisure
  REAL, POINTER, DIMENSION(:) :: XP_C2        ! equation.
  REAL, POINTER, DIMENSION(:) :: XP_WGEQ      ! equilibrium volumetric water
!                                               ! content
  REAL, POINTER, DIMENSION(:) :: XP_CT        ! area-averaged heat capacity
  REAL, POINTER, DIMENSION(:) :: XP_RS        ! stomatal resistance                            (s/m)
!
!------------------------------------------------------------------------------
!
  REAL, POINTER, DIMENSION(:) :: XP_TS        ! Surface temperature                            (K)
  REAL, POINTER, DIMENSION(:) :: XP_TSRAD     ! Radiative surface temperature                  (K)
  REAL, POINTER, DIMENSION(:) :: XP_T2M       ! Air temperature       at 2 meters              (K)
  REAL, POINTER, DIMENSION(:) :: XP_Q2M       ! Air spec. humidity    at 2 meters              (kg/kg)
  REAL, POINTER, DIMENSION(:) :: XP_HU2M      ! Air rela. humidity    at 2 meters              (-)
  REAL, POINTER, DIMENSION(:) :: XP_ZON10M    ! zonal Wind at 10 meters                        (m/s)
  REAL, POINTER, DIMENSION(:) :: XP_MER10M    ! meridian Wind at 10 meters                     (m/s)
!
!------------------------------------------------------------------------------
!
  REAL, POINTER, DIMENSION(:)   :: XP_QS      ! humidity at surface                            (Kg/kg)
  REAL, POINTER, DIMENSION(:,:) :: XP_SWI     ! soil wetness index profile                     (-)
  REAL, POINTER, DIMENSION(:,:) :: XP_TSWI    ! total soil wetness index profile               (-)
!
!------------------------------------------------------------------------------
!
  REAL, POINTER, DIMENSION(:) :: XP_TWSNOW     ! total snow reservoir (kg/m2)
  REAL, POINTER, DIMENSION(:) :: XP_TDSNOW     ! total snow height (m)
!
!------------------------------------------------------------------------------
!
  REAL, POINTER, DIMENSION(:) :: XP_SWD       ! downward short wave radiation    (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_SWU       ! upward short wave radiation      (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XP_SWBD    ! downward short wave radiation by spectral band   (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XP_SWBU    ! upward short wave radiation by spectral band (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_LWD       ! downward long wave radiation     (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_LWU       ! upward long wave radiation       (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_FMU       ! horizontal momentum flux zonal   (m2/s2)
  REAL, POINTER, DIMENSION(:) :: XP_FMV       ! horizontal momentum flux meridian (m2/s2)
!
!------------------------------------------------------------------------------
!
  REAL, POINTER, DIMENSION(:) :: XP_HORT      ! sub-grid Horton runoff from the SGH scheme   (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_DRIP      ! dripping from the vegetation reservoir       (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_IFLOOD    ! flood infiltration                           (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_PFLOOD    ! precipitation intercepted by the floodplains (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_LE_FLOOD  ! flood evaporation                            (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_LEI_FLOOD ! frozen flood evaporation                     (W/m2)
  REAL, POINTER, DIMENSION(:) :: XP_ICEFLUX
  REAL, POINTER, DIMENSION(:) :: XP_RRVEG     ! precipitation intercepted by the vegetation   (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_IRRIG_FLUX! irrigation rate                               (kg/m2/s)
!
!------------------------------------------------------------------------------
!
  REAL, POINTER, DIMENSION(:) :: XP_GPP         ! Gross primary production (kgCO2/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_RESP_AUTO   ! Autotrophic respiration  (kgCO2/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_RESP_ECO    ! Ecosystem respiration    (kgCO2/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_FAPAR       ! Fapar of vegetation
  REAL, POINTER, DIMENSION(:) :: XP_FAPIR       ! Fapir of vegetation 
  REAL, POINTER, DIMENSION(:) :: XP_FAPAR_BS    ! Fapar of bare soil
  REAL, POINTER, DIMENSION(:) :: XP_FAPIR_BS    ! Fapir of bare soil
!
!------------------------------------------------------------------------------
!
!* diagnostic variables for multi-energy balance (MEB)
!  ---------------------------------------------------
!
  REAL, POINTER, DIMENSION(:) :: XP_SWUP          ! MEB: net *total* (surface) upwelling shortwave radiation to atmosphere [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_SWNET_V       ! MEB: net vegetation canopy shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_SWNET_G       ! MEB: net ground shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_SWNET_N       ! MEB: net snow shortwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_SWNET_NS      ! MEB: net snow shortwave radiation for *surface* layer 
                                                !     (i.e. net snow shortwave radiation less absorbed radiation) [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_LWUP          ! MEB: net *total* (surface) upwelling longwave radiation to atmosphere [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_LWNET_V       ! MEB: net vegetation canopy longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_LWNET_G       ! MEB: net ground longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_LWNET_N       ! MEB: net snow longwave radiation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_LEVCV         ! MEB: total evapotranspiration from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_LESC          ! MEB: total snow sublimation from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_H_V_C         ! MEB: sensible heat flux from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_H_G_C         ! MEB: sensible heat flux from understory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_LETRGV        ! MEB: transpiration from understory vegetation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_LETRCV        ! MEB: transpiration from overstory canopy vegetation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_LERGV         ! MEB: interception evaporation from understory vegetation [W/m2  
  REAL, POINTER, DIMENSION(:) :: XP_LELITTER      ! MEB: water evaporation from litter[W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_LELITTERI     ! MEB: ice sublimation from litter [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_DRIPLIT       ! 
  REAL, POINTER, DIMENSION(:) :: XP_RRLIT         ! 
  REAL, POINTER, DIMENSION(:) :: XP_LERCV         ! MEB: interception evaporation from overstory canopy vegetation [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_H_C_A         ! MEB: sensible heat flux from canopy air space to the atmosphere [W/m2] 
  REAL, POINTER, DIMENSION(:) :: XP_H_N_C         ! MEB: sensible heat flux from the snow on the ground [W/m2]
                                                !      NOTE total sensible heat flux to the atmosphere also possibly 
                                                !      includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:) :: XP_LE_V_C        ! MEB: latent heat flux from vegetation canopy overstory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_LE_G_C        ! MEB: latent heat flux from understory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_LE_C_A        ! MEB: latent heat flux from canopy air space to the atmosphere [W/m2] 
                                                !      NOTE total latent heat flux to the atmosphere also possibly 
                                                !      includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:) :: XP_LE_N_C        ! MEB: latent heat flux from the snow on the ground [W/m2]
                                                !      NOTE total latent heat flux from the snowpack
                                                !      possibly includes a contribution from snow covering the canopy
  REAL, POINTER, DIMENSION(:) :: XP_EVAP_N_C      ! MEB: Total evap from snow on the ground to canopy air space  [kg/m2/s]
  REAL, POINTER, DIMENSION(:) :: XP_EVAP_G_C      ! MEB: Total evap from ground to canopy air space [kg/m2/s]
  REAL, POINTER, DIMENSION(:) :: XP_SR_GN         ! MEB: total snow reacing the ground snow [kg/m2/s]
  REAL, POINTER, DIMENSION(:) :: XP_MELTCV        ! MEB: snow melt rate from the overstory snow reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:) :: XP_FRZCV         ! MEB: snow refreeze rate from the overstory snow reservoir [kg/m2/s]
  REAL, POINTER, DIMENSION(:) :: XP_SWDOWN_GN     ! MEB: total shortwave radiation transmitted through the canopy
                                                !      reaching the snowpack/ground understory [W/m2]
  REAL, POINTER, DIMENSION(:) :: XP_LWDOWN_GN     ! MEB: total shortwave radiation transmitted through and emitted by the canopy
!                                               !      reaching the snowpack/ground understory (explicit part) [W/m2]
!
!------------------------------------------------------------------------------
!
  REAL, POINTER, DIMENSION(:) :: XP_DWG         ! liquid soil moisture time tendencies  (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_DWGI        ! solid soil moisture time tendencies   (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_DWR         ! canopy water time tendencies          (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_DSWE        ! snow water equivalent time tendencies (kg/m2/s)
  REAL, POINTER, DIMENSION(:) :: XP_WATBUD      ! ISBA water budget                     (kg/m2/s)

END TYPE PACK_DIAG_ISBA_t
!
!-------------------------------------------------------------------------------
!


 CONTAINS

!
!




SUBROUTINE PACK_DIAG_ISBA_INIT(YPACK_DIAG_ISBA)
TYPE(PACK_DIAG_ISBA_t), INTENT(INOUT) :: YPACK_DIAG_ISBA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_PACK_DIAG_ISBA_N:PACK_DIAG_ISBA_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YPACK_DIAG_ISBA%XBLOCK_SIMPLE)
  NULLIFY(YPACK_DIAG_ISBA%XBLOCK_GROUND)
  NULLIFY(YPACK_DIAG_ISBA%XBLOCK_SNOW)
  NULLIFY(YPACK_DIAG_ISBA%XBLOCK_KSW)
  NULLIFY(YPACK_DIAG_ISBA%XBLOCK_ABC)
  NULLIFY(YPACK_DIAG_ISBA%XBLOCK_0)
  NULLIFY(YPACK_DIAG_ISBA%XBLOCK_00)
  NULLIFY(YPACK_DIAG_ISBA%XP_RNSNOW)
  NULLIFY(YPACK_DIAG_ISBA%XP_HSNOW)
  NULLIFY(YPACK_DIAG_ISBA%XP_HPSNOW)
  NULLIFY(YPACK_DIAG_ISBA%XP_GFLUXSNOW)
  NULLIFY(YPACK_DIAG_ISBA%XP_USTARSNOW)
  NULLIFY(YPACK_DIAG_ISBA%XP_GRNDFLUX)
  NULLIFY(YPACK_DIAG_ISBA%XP_SRSFC)
  NULLIFY(YPACK_DIAG_ISBA%XP_RRSFC)
  NULLIFY(YPACK_DIAG_ISBA%XP_LESL)
  NULLIFY(YPACK_DIAG_ISBA%XP_CDSNOW)
  NULLIFY(YPACK_DIAG_ISBA%XP_CHSNOW)
  NULLIFY(YPACK_DIAG_ISBA%XP_SNOWTEMP)
  NULLIFY(YPACK_DIAG_ISBA%XP_SNOWLIQ)
  NULLIFY(YPACK_DIAG_ISBA%XP_SNOWDZ)
  NULLIFY(YPACK_DIAG_ISBA%XP_SNOWHMASS)
  NULLIFY(YPACK_DIAG_ISBA%XP_RN_ISBA)
  NULLIFY(YPACK_DIAG_ISBA%XP_H_ISBA)
  NULLIFY(YPACK_DIAG_ISBA%XP_LEG_ISBA)
  NULLIFY(YPACK_DIAG_ISBA%XP_LEGI_ISBA)
  NULLIFY(YPACK_DIAG_ISBA%XP_LEV_ISBA)
  NULLIFY(YPACK_DIAG_ISBA%XP_LETR_ISBA)
  NULLIFY(YPACK_DIAG_ISBA%XP_USTAR_ISBA)
  NULLIFY(YPACK_DIAG_ISBA%XP_LER_ISBA)
  NULLIFY(YPACK_DIAG_ISBA%XP_LE_ISBA)
  NULLIFY(YPACK_DIAG_ISBA%XP_LEI_ISBA)
  NULLIFY(YPACK_DIAG_ISBA%XP_GFLUX_ISBA)
  NULLIFY(YPACK_DIAG_ISBA%XP_MELTADV)
  NULLIFY(YPACK_DIAG_ISBA%XP_CH)
  NULLIFY(YPACK_DIAG_ISBA%XP_CE)
  NULLIFY(YPACK_DIAG_ISBA%XP_CD)
  NULLIFY(YPACK_DIAG_ISBA%XP_CDN)
  NULLIFY(YPACK_DIAG_ISBA%XP_RI)
  NULLIFY(YPACK_DIAG_ISBA%XP_HU)
  NULLIFY(YPACK_DIAG_ISBA%XP_HUG)
  NULLIFY(YPACK_DIAG_ISBA%XP_HV)
  NULLIFY(YPACK_DIAG_ISBA%XP_ALBT)
  NULLIFY(YPACK_DIAG_ISBA%XP_RN)
  NULLIFY(YPACK_DIAG_ISBA%XP_H)
  NULLIFY(YPACK_DIAG_ISBA%XP_LEG)
  NULLIFY(YPACK_DIAG_ISBA%XP_LEGI)
  NULLIFY(YPACK_DIAG_ISBA%XP_LEV)
  NULLIFY(YPACK_DIAG_ISBA%XP_LES)
  NULLIFY(YPACK_DIAG_ISBA%XP_LER)
  NULLIFY(YPACK_DIAG_ISBA%XP_LETR)
  NULLIFY(YPACK_DIAG_ISBA%XP_EVAP)
  NULLIFY(YPACK_DIAG_ISBA%XP_SUBL)
  NULLIFY(YPACK_DIAG_ISBA%XP_SNDRIFT)
  NULLIFY(YPACK_DIAG_ISBA%XP_LEI)
  NULLIFY(YPACK_DIAG_ISBA%XP_GFLUX)
  NULLIFY(YPACK_DIAG_ISBA%XP_RESTORE)
  NULLIFY(YPACK_DIAG_ISBA%XP_DRAIN)
  NULLIFY(YPACK_DIAG_ISBA%XP_QSB)
  NULLIFY(YPACK_DIAG_ISBA%XP_RUNOFF)
  NULLIFY(YPACK_DIAG_ISBA%XP_MELT)
  NULLIFY(YPACK_DIAG_ISBA%XP_SNOWFREE_ALB)
  NULLIFY(YPACK_DIAG_ISBA%XP_SNOWFREE_ALB_VEG)
  NULLIFY(YPACK_DIAG_ISBA%XP_SNOWFREE_ALB_SOIL)
  NULLIFY(YPACK_DIAG_ISBA%XP_Z0_WITH_SNOW)
  NULLIFY(YPACK_DIAG_ISBA%XP_Z0H_WITH_SNOW)
  NULLIFY(YPACK_DIAG_ISBA%XP_Z0EFF)
  NULLIFY(YPACK_DIAG_ISBA%XP_IACAN)
  NULLIFY(YPACK_DIAG_ISBA%XP_CG)
  NULLIFY(YPACK_DIAG_ISBA%XP_C1)
  NULLIFY(YPACK_DIAG_ISBA%XP_C2)
  NULLIFY(YPACK_DIAG_ISBA%XP_WGEQ)
  NULLIFY(YPACK_DIAG_ISBA%XP_CT)
  NULLIFY(YPACK_DIAG_ISBA%XP_RS)
  NULLIFY(YPACK_DIAG_ISBA%XP_TS)
  NULLIFY(YPACK_DIAG_ISBA%XP_TSRAD)
  NULLIFY(YPACK_DIAG_ISBA%XP_T2M)
  NULLIFY(YPACK_DIAG_ISBA%XP_Q2M)
  NULLIFY(YPACK_DIAG_ISBA%XP_HU2M)
  NULLIFY(YPACK_DIAG_ISBA%XP_ZON10M)
  NULLIFY(YPACK_DIAG_ISBA%XP_MER10M)
  NULLIFY(YPACK_DIAG_ISBA%XP_QS)
  NULLIFY(YPACK_DIAG_ISBA%XP_SWI)
  NULLIFY(YPACK_DIAG_ISBA%XP_TSWI)
  NULLIFY(YPACK_DIAG_ISBA%XP_TWSNOW)
  NULLIFY(YPACK_DIAG_ISBA%XP_TDSNOW)
  NULLIFY(YPACK_DIAG_ISBA%XP_SWD)
  NULLIFY(YPACK_DIAG_ISBA%XP_SWU)
  NULLIFY(YPACK_DIAG_ISBA%XP_SWBD)
  NULLIFY(YPACK_DIAG_ISBA%XP_SWBU)
  NULLIFY(YPACK_DIAG_ISBA%XP_LWD)
  NULLIFY(YPACK_DIAG_ISBA%XP_LWU)
  NULLIFY(YPACK_DIAG_ISBA%XP_FMU)
  NULLIFY(YPACK_DIAG_ISBA%XP_FMV)
  NULLIFY(YPACK_DIAG_ISBA%XP_HORT)
  NULLIFY(YPACK_DIAG_ISBA%XP_DRIP)
  NULLIFY(YPACK_DIAG_ISBA%XP_IFLOOD)
  NULLIFY(YPACK_DIAG_ISBA%XP_PFLOOD)
  NULLIFY(YPACK_DIAG_ISBA%XP_LE_FLOOD)
  NULLIFY(YPACK_DIAG_ISBA%XP_LEI_FLOOD)
  NULLIFY(YPACK_DIAG_ISBA%XP_ICEFLUX)
  NULLIFY(YPACK_DIAG_ISBA%XP_RRVEG)
  NULLIFY(YPACK_DIAG_ISBA%XP_IRRIG_FLUX)
  NULLIFY(YPACK_DIAG_ISBA%XP_GPP)
  NULLIFY(YPACK_DIAG_ISBA%XP_RESP_AUTO)
  NULLIFY(YPACK_DIAG_ISBA%XP_RESP_ECO)
  NULLIFY(YPACK_DIAG_ISBA%XP_FAPAR)
  NULLIFY(YPACK_DIAG_ISBA%XP_FAPIR)
  NULLIFY(YPACK_DIAG_ISBA%XP_FAPAR_BS)
  NULLIFY(YPACK_DIAG_ISBA%XP_FAPIR_BS)
  NULLIFY(YPACK_DIAG_ISBA%XP_SWUP)
  NULLIFY(YPACK_DIAG_ISBA%XP_SWNET_V)
  NULLIFY(YPACK_DIAG_ISBA%XP_SWNET_G)
  NULLIFY(YPACK_DIAG_ISBA%XP_SWNET_N)
  NULLIFY(YPACK_DIAG_ISBA%XP_SWNET_NS)
  NULLIFY(YPACK_DIAG_ISBA%XP_LWUP)
  NULLIFY(YPACK_DIAG_ISBA%XP_LWNET_V)
  NULLIFY(YPACK_DIAG_ISBA%XP_LWNET_G)
  NULLIFY(YPACK_DIAG_ISBA%XP_LWNET_N)
  NULLIFY(YPACK_DIAG_ISBA%XP_LEVCV)
  NULLIFY(YPACK_DIAG_ISBA%XP_LESC)
  NULLIFY(YPACK_DIAG_ISBA%XP_H_V_C)
  NULLIFY(YPACK_DIAG_ISBA%XP_H_G_C)
  NULLIFY(YPACK_DIAG_ISBA%XP_LETRGV)
  NULLIFY(YPACK_DIAG_ISBA%XP_LETRCV)
  NULLIFY(YPACK_DIAG_ISBA%XP_LERGV)
  NULLIFY(YPACK_DIAG_ISBA%XP_LELITTER)
  NULLIFY(YPACK_DIAG_ISBA%XP_LELITTERI)
  NULLIFY(YPACK_DIAG_ISBA%XP_DRIPLIT)
  NULLIFY(YPACK_DIAG_ISBA%XP_RRLIT)
  NULLIFY(YPACK_DIAG_ISBA%XP_LERCV)
  NULLIFY(YPACK_DIAG_ISBA%XP_H_C_A)
  NULLIFY(YPACK_DIAG_ISBA%XP_H_N_C)
  NULLIFY(YPACK_DIAG_ISBA%XP_LE_V_C)
  NULLIFY(YPACK_DIAG_ISBA%XP_LE_G_C)
  NULLIFY(YPACK_DIAG_ISBA%XP_LE_C_A)
  NULLIFY(YPACK_DIAG_ISBA%XP_LE_N_C)
  NULLIFY(YPACK_DIAG_ISBA%XP_EVAP_N_C)
  NULLIFY(YPACK_DIAG_ISBA%XP_EVAP_G_C)
  NULLIFY(YPACK_DIAG_ISBA%XP_SR_GN)
  NULLIFY(YPACK_DIAG_ISBA%XP_MELTCV)
  NULLIFY(YPACK_DIAG_ISBA%XP_FRZCV)
  NULLIFY(YPACK_DIAG_ISBA%XP_SWDOWN_GN)
  NULLIFY(YPACK_DIAG_ISBA%XP_LWDOWN_GN)
  NULLIFY(YPACK_DIAG_ISBA%XP_DWG)
  NULLIFY(YPACK_DIAG_ISBA%XP_DWGI)
  NULLIFY(YPACK_DIAG_ISBA%XP_DWR)
  NULLIFY(YPACK_DIAG_ISBA%XP_DSWE)
  NULLIFY(YPACK_DIAG_ISBA%XP_WATBUD)
IF (LHOOK) CALL DR_HOOK("MODD_PACK_DIAG_ISBA_N:PACK_DIAG_ISBA_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE PACK_DIAG_ISBA_INIT


END MODULE MODD_PACK_DIAG_ISBA
