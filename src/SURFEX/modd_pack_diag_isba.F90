!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
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
!!	V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2004
!!      Modified       10/2004 by P. Le Moigne: add Halstead coefficient
!!      Modified       11/2009 by S. Senesi: add precipitation intercepted by the vegetation (XP_RRVEG)
!!      Modified       04-09 by A.L. Gibelin  : Add carbon diagnostics
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!------------------------------------------------------------------------------
!
INTEGER :: NSIZE_SIMPLE
!$OMP THREADPRIVATE(NSIZE_SIMPLE)
INTEGER :: NSIZE_GROUND
!$OMP THREADPRIVATE(NSIZE_GROUND)
INTEGER :: NSIZE_SNOW
!$OMP THREADPRIVATE(NSIZE_SNOW)
INTEGER :: NSIZE_KSW
!$OMP THREADPRIVATE(NSIZE_KSW)
INTEGER :: NSIZE_ABC
!$OMP THREADPRIVATE(NSIZE_ABC)
INTEGER :: NSIZE_0
!$OMP THREADPRIVATE(NSIZE_0)
INTEGER :: NSIZE_00
!$OMP THREADPRIVATE(NSIZE_00)
REAL, ALLOCATABLE, DIMENSION(:,:), TARGET :: XBLOCK_SIMPLE
!$OMP THREADPRIVATE(XBLOCK_SIMPLE)
REAL, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: XBLOCK_GROUND
!$OMP THREADPRIVATE(XBLOCK_GROUND)
REAL, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: XBLOCK_SNOW
!$OMP THREADPRIVATE(XBLOCK_SNOW)
REAL, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: XBLOCK_KSW
!$OMP THREADPRIVATE(XBLOCK_KSW)
REAL, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: XBLOCK_ABC
!$OMP THREADPRIVATE(XBLOCK_ABC)
REAL, ALLOCATABLE, DIMENSION(:,:), TARGET :: XBLOCK_0
!$OMP THREADPRIVATE(XBLOCK_0)
REAL, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: XBLOCK_00
!$OMP THREADPRIVATE(XBLOCK_00)
!
REAL, POINTER, DIMENSION(:) :: XP_RNSNOW    ! net radiative flux from snow (ISBA-ES:3-L)    (W/m2)
!$OMP THREADPRIVATE(XP_RNSNOW)
REAL, POINTER, DIMENSION(:) :: XP_HSNOW     ! sensible heat flux from snow (ISBA-ES:3-L)    (W/m2)
!$OMP THREADPRIVATE(XP_HSNOW)
REAL, POINTER, DIMENSION(:) :: XP_HPSNOW    ! heat release from rainfall (ISBA-ES:3-L)      (W/m2)
!$OMP THREADPRIVATE(XP_HPSNOW)
REAL, POINTER, DIMENSION(:) :: XP_SMELTFLUX ! energy removed from soil/vegetation surface
!$OMP THREADPRIVATE(XP_SMELTFLUX)
REAL, POINTER, DIMENSION(:) :: XP_GFLUXSNOW ! net surface energy flux into snowpack      
!$OMP THREADPRIVATE(XP_GFLUXSNOW)
!                                               ! (ISBA-ES:3-L)                                 (W/m2)
REAL, POINTER, DIMENSION(:) :: XP_USTARSNOW ! friction velocity  over snow 
!$OMP THREADPRIVATE(XP_USTARSNOW)
!                                               ! (ISBA-ES:3-L)                                 (m/s)
REAL, POINTER, DIMENSION(:) :: XP_GRNDFLUX  ! soil/snow interface heat flux (ISBA-ES:3-L)   (W/m2)
!$OMP THREADPRIVATE(XP_GRNDFLUX)
REAL, POINTER, DIMENSION(:) :: XP_SRSFC     ! snowfall over snowpack (ISBA-ES:3-L)          (kg/m2/s)
!$OMP THREADPRIVATE(XP_SRSFC)
REAL, POINTER, DIMENSION(:) :: XP_RRSFC     ! rainfall over snowpack (ISBA-ES:3-L)          (kg/m2/s)
!$OMP THREADPRIVATE(XP_RRSFC)
REAL, POINTER, DIMENSION(:) :: XP_LESL      ! snowpack evaporation (ISBA-ES:3-L)            (W/m2)
!$OMP THREADPRIVATE(XP_LESL)
REAL, POINTER, DIMENSION(:) :: XP_CDSNOW    ! snow drag coefficient (ISBA-ES:3-L)           (-)
!$OMP THREADPRIVATE(XP_CDSNOW)
REAL, POINTER, DIMENSION(:) :: XP_CHSNOW    ! heat turbulent transfer coefficient 
!$OMP THREADPRIVATE(XP_CHSNOW)
!                                               ! (ISBA-ES:3-L)                                 (-)
REAL, POINTER, DIMENSION(:,:)::XP_SNOWTEMP  ! snow temperature profile (ISBA-ES:3-L)        (K)
!$OMP THREADPRIVATE(XP_SNOWTEMP)
REAL, POINTER, DIMENSION(:,:)::XP_SNOWLIQ   ! snow liquid water profile (ISBA-ES:3-L)       (m)
!$OMP THREADPRIVATE(XP_SNOWLIQ)
REAL, POINTER, DIMENSION(:,:)::XP_SNOWDZ    ! snow layer thicknesses                        (m)
!$OMP THREADPRIVATE(XP_SNOWDZ)
REAL, POINTER, DIMENSION(:) :: XP_SNOWHMASS ! heat content change due to mass
!$OMP THREADPRIVATE(XP_SNOWHMASS)
!                                               ! changes in snowpack: for budget
!                                               ! calculations only. (ISBA-ES:3-L)              (J/m2)
REAL, POINTER, DIMENSION(:) :: XP_RN_ISBA   ! net radiative flux from snow-free surface 
!$OMP THREADPRIVATE(XP_RN_ISBA)
!                                               ! (ISBA-ES:3-L)                                 (W/m2) 
REAL, POINTER, DIMENSION(:) :: XP_H_ISBA    ! sensible heat flux from snow-free surface 
!$OMP THREADPRIVATE(XP_H_ISBA)
!                                               ! (ISBA-ES:3-L)                                 (W/m2) 
REAL, POINTER, DIMENSION(:) :: XP_LEG_ISBA  ! baresoil evaporation from snow-free surface 
!$OMP THREADPRIVATE(XP_LEG_ISBA)
!                                               ! (ISBA-ES:3-L)                                 (W/m2) 
REAL, POINTER, DIMENSION(:) :: XP_LEGI_ISBA ! baresoil sublimation from snow-free surface 
!$OMP THREADPRIVATE(XP_LEGI_ISBA)
!                                               ! (ISBA-ES:3-L)                                 (W/m2) 
REAL, POINTER, DIMENSION(:) :: XP_LEV_ISBA  ! total evapotranspiration from vegetation over 
!$OMP THREADPRIVATE(XP_LEV_ISBA)
!                                               ! snow-free surface (ISBA-ES:3-L)               (W/m2) 
REAL, POINTER, DIMENSION(:) :: XP_LETR_ISBA ! transpiration from snow-free surface 
!$OMP THREADPRIVATE(XP_LETR_ISBA)
!                                               ! (ISBA-ES:3-L)                                 (W/m2)
REAL, POINTER, DIMENSION(:) :: XP_USTAR_ISBA! friction velocity from snow-free 
!$OMP THREADPRIVATE(XP_USTAR_ISBA)
!                                               ! surface (ISBA-ES:3-L)                         (m/s)
REAL, POINTER, DIMENSION(:) :: XP_LER_ISBA  ! evaporation from canopy water interception 
!$OMP THREADPRIVATE(XP_LER_ISBA)
!                                               ! store over snow-free surface (ISBA-ES:3-L)    (W/m2)
REAL, POINTER, DIMENSION(:) :: XP_LE_ISBA   ! total latent heat flux from snow-free surface 
!$OMP THREADPRIVATE(XP_LE_ISBA)
REAL, POINTER, DIMENSION(:) :: XP_LEI_ISBA  ! sublimation latent heat flux from snow-free surface 
!$OMP THREADPRIVATE(XP_LEI_ISBA)
!                                               ! (ISBA-ES:3-L)                                 (W/m2) 
REAL, POINTER, DIMENSION(:) :: XP_GFLUX_ISBA! net energy flux into the snow-free surface 
!$OMP THREADPRIVATE(XP_GFLUX_ISBA)
!                                               ! (ISBA-ES:3-L)                                 (W/m2) 
REAL, POINTER, DIMENSION(:) :: XP_MELTADV   ! advective energy from snow melt water 
!$OMP THREADPRIVATE(XP_MELTADV)
!                                               ! (ISBA-ES:3-L)                                 (W/m2)
REAL, POINTER, DIMENSION(:) :: XP_CH        ! thermal diffusion coefficient                 (W/s)
!$OMP THREADPRIVATE(XP_CH)
REAL, POINTER, DIMENSION(:) :: XP_CE        ! transfer coefficient for vapor                (W/s/K)
!$OMP THREADPRIVATE(XP_CE)
REAL, POINTER, DIMENSION(:) :: XP_CD        ! drag coefficient                              (-)
!$OMP THREADPRIVATE(XP_CD)
REAL, POINTER, DIMENSION(:) :: XP_CDN       ! neutral drag coefficient                      (-)
!$OMP THREADPRIVATE(XP_CDN)
REAL, POINTER, DIMENSION(:) :: XP_RI        ! Bulk-Richardson number                        (-)
!$OMP THREADPRIVATE(XP_RI)
REAL, POINTER, DIMENSION(:) :: XP_HU        ! area averaged surface humidity coefficient    (-)
!$OMP THREADPRIVATE(XP_HU)
REAL, POINTER, DIMENSION(:) :: XP_HUG       ! baresoil surface humidity coefficient         (-)
!$OMP THREADPRIVATE(XP_HUG)
REAL, POINTER, DIMENSION(:) :: XP_HV        ! Halstead coefficient                          (-)
!$OMP THREADPRIVATE(XP_HV)
!
REAL, POINTER, DIMENSION(:) :: XP_ALBT      ! Total Albedo                                  (-)
!$OMP THREADPRIVATE(XP_ALBT)
!
REAL, POINTER, DIMENSION(:) :: XP_RN        ! net radiation at surface                      (W/m2)
!$OMP THREADPRIVATE(XP_RN)
REAL, POINTER, DIMENSION(:) :: XP_H         ! sensible heat flux                            (W/m2)
!$OMP THREADPRIVATE(XP_H)
REAL, POINTER, DIMENSION(:) :: XP_LEG       ! baresoil evaporation                          (W/m2)
!$OMP THREADPRIVATE(XP_LEG)
REAL, POINTER, DIMENSION(:) :: XP_LEGI      ! baresoil sublimation                          (W/m2)
!$OMP THREADPRIVATE(XP_LEGI)
REAL, POINTER, DIMENSION(:) :: XP_LEV       ! total evapotranspiration from vegetation      (W/m2)
!$OMP THREADPRIVATE(XP_LEV)
REAL, POINTER, DIMENSION(:) :: XP_LES       ! snow sublimation                              (W/m2)
!$OMP THREADPRIVATE(XP_LES)
REAL, POINTER, DIMENSION(:) :: XP_LER       ! evaporation from canopy water interception 
!$OMP THREADPRIVATE(XP_LER)
!                                               ! store                                         (W/m2)
REAL, POINTER, DIMENSION(:) :: XP_LETR      ! transpiration                                 (W/m2)
!$OMP THREADPRIVATE(XP_LETR)
REAL, POINTER, DIMENSION(:) :: XP_EVAP      ! evapotranspiration                            (kg/m2/s)
!$OMP THREADPRIVATE(XP_EVAP)
REAL, POINTER, DIMENSION(:) :: XP_LEI       ! sublimation latent heat flux from snow-free surface (W/m2)
!$OMP THREADPRIVATE(XP_LEI)
REAL, POINTER, DIMENSION(:) :: XP_GFLUX     ! net soil-vegetation flux                      (W/m2)
!$OMP THREADPRIVATE(XP_GFLUX)
REAL, POINTER, DIMENSION(:) :: XP_RESTORE   ! surface energy budget restore term            (W/m2)
!$OMP THREADPRIVATE(XP_RESTORE)
REAL, POINTER, DIMENSION(:) :: XP_DRAIN     ! soil drainage flux                            (kg/m2/s)
!$OMP THREADPRIVATE(XP_DRAIN)
REAL, POINTER, DIMENSION(:) :: XP_RUNOFF    ! sub-grid and supersaturation runoff           (kg/m2/s)
!$OMP THREADPRIVATE(XP_RUNOFF)
REAL, POINTER, DIMENSION(:) :: XP_MELT      ! melting rate of the snow                      (kg/m2/s)
!$OMP THREADPRIVATE(XP_MELT)
REAL, POINTER, DIMENSION(:) :: XP_SNOWFREE_ALB ! snow-free global albedo                    (-)
!$OMP THREADPRIVATE(XP_SNOWFREE_ALB)
REAL, POINTER, DIMENSION(:) :: XP_SNOWFREE_ALB_VEG ! snow-free global  albedo of vegetation
!$OMP THREADPRIVATE(XP_SNOWFREE_ALB_VEG)
REAL, POINTER, DIMENSION(:) :: XP_SNOWFREE_ALB_SOIL! snow-free soil albedo
!$OMP THREADPRIVATE(XP_SNOWFREE_ALB_SOIL)
REAL, POINTER, DIMENSION(:) :: XP_Z0_WITH_SNOW ! total roughness length (including snow)    (m)
!$OMP THREADPRIVATE(XP_Z0_WITH_SNOW)
REAL, POINTER, DIMENSION(:) :: XP_Z0H_WITH_SNOW! roughness length for heat (including snow) (m)
!$OMP THREADPRIVATE(XP_Z0H_WITH_SNOW)
REAL, POINTER, DIMENSION(:) :: XP_Z0EFF     ! effective roughness length (with relief added)(m)
!$OMP THREADPRIVATE(XP_Z0EFF)
!
REAL, POINTER, DIMENSION(:,:)::XP_IACAN     ! PAR in the canopy at different gauss level    (micmolphot/m2/s)
!$OMP THREADPRIVATE(XP_IACAN)
!
REAL, POINTER, DIMENSION(:) :: XP_CG        ! heat capacity of the ground
!$OMP THREADPRIVATE(XP_CG)
REAL, POINTER, DIMENSION(:) :: XP_C1        ! coefficients for the moisure
!$OMP THREADPRIVATE(XP_C1)
REAL, POINTER, DIMENSION(:) :: XP_C2        ! equation.
!$OMP THREADPRIVATE(XP_C2)
REAL, POINTER, DIMENSION(:) :: XP_WGEQ      ! equilibrium volumetric water
!$OMP THREADPRIVATE(XP_WGEQ)
!                                               ! content
REAL, POINTER, DIMENSION(:) :: XP_CT        ! area-averaged heat capacity
!$OMP THREADPRIVATE(XP_CT)
REAL, POINTER, DIMENSION(:) :: XP_RS        ! stomatal resistance                            (s/m)
!$OMP THREADPRIVATE(XP_RS)
!------------------------------------------------------------------------------
!
REAL, POINTER, DIMENSION(:) :: XP_TS        ! Surface temperature                            (K)
!$OMP THREADPRIVATE(XP_TS)
REAL, POINTER, DIMENSION(:) :: XP_TSRAD     ! Radiative surface temperature                  (K)
!$OMP THREADPRIVATE(XP_TSRAD)
REAL, POINTER, DIMENSION(:) :: XP_T2M       ! Air temperature       at 2 meters              (K)
!$OMP THREADPRIVATE(XP_T2M)
REAL, POINTER, DIMENSION(:) :: XP_Q2M       ! Air spec. humidity    at 2 meters              (kg/kg)
!$OMP THREADPRIVATE(XP_Q2M)
REAL, POINTER, DIMENSION(:) :: XP_HU2M      ! Air rela. humidity    at 2 meters              (-)
!$OMP THREADPRIVATE(XP_HU2M)
REAL, POINTER, DIMENSION(:) :: XP_ZON10M    ! zonal Wind at 10 meters                        (m/s)
!$OMP THREADPRIVATE(XP_ZON10M)
REAL, POINTER, DIMENSION(:) :: XP_MER10M    ! meridian Wind at 10 meters                     (m/s)
!$OMP THREADPRIVATE(XP_MER10M)
!
!------------------------------------------------------------------------------
!
REAL, POINTER, DIMENSION(:)   :: XP_QS      ! humidity at surface                            (Kg/kg)
!$OMP THREADPRIVATE(XP_QS)
REAL, POINTER, DIMENSION(:,:) :: XP_SWI     ! soil wetness index profile                     (-)
!$OMP THREADPRIVATE(XP_SWI)
REAL, POINTER, DIMENSION(:,:) :: XP_TSWI    ! total soil wetness index profile               (-)
!$OMP THREADPRIVATE(XP_TSWI)
!
!------------------------------------------------------------------------------
!
REAL, POINTER, DIMENSION(:) :: XP_TWSNOW     ! total snow reservoir (kg/m2)
!$OMP THREADPRIVATE(XP_TWSNOW)
REAL, POINTER, DIMENSION(:) :: XP_TDSNOW     ! total snow height (m)
!$OMP THREADPRIVATE(XP_TDSNOW)
!
!------------------------------------------------------------------------------
!
REAL, POINTER, DIMENSION(:) :: XP_SWD       ! downward short wave radiation    (W/m2)
!$OMP THREADPRIVATE(XP_SWD)
REAL, POINTER, DIMENSION(:) :: XP_SWU       ! upward short wave radiation      (W/m2)
!$OMP THREADPRIVATE(XP_SWU)
REAL, POINTER, DIMENSION(:,:) :: XP_SWBD    ! downward short wave radiation by spectral band   (W/m2)
!$OMP THREADPRIVATE(XP_SWBD)
REAL, POINTER, DIMENSION(:,:) :: XP_SWBU    ! upward short wave radiation by spectral band (W/m2)
!$OMP THREADPRIVATE(XP_SWBU)
REAL, POINTER, DIMENSION(:) :: XP_LWD       ! downward long wave radiation     (W/m2)
!$OMP THREADPRIVATE(XP_LWD)
REAL, POINTER, DIMENSION(:) :: XP_LWU       ! upward long wave radiation       (W/m2)
!$OMP THREADPRIVATE(XP_LWU)
REAL, POINTER, DIMENSION(:) :: XP_FMU       ! horizontal momentum flux zonal   (m2/s2)
!$OMP THREADPRIVATE(XP_FMU)
REAL, POINTER, DIMENSION(:) :: XP_FMV       ! horizontal momentum flux meridian (m2/s2)
!$OMP THREADPRIVATE(XP_FMV)
!
!------------------------------------------------------------------------------
!
REAL, POINTER, DIMENSION(:) :: XP_HORT      ! sub-grid Horton runoff from the SGH scheme   (kg/m2/s)
!$OMP THREADPRIVATE(XP_HORT)
REAL, POINTER, DIMENSION(:) :: XP_DRIP      ! dripping from the vegetation reservoir       (kg/m2/s)
!$OMP THREADPRIVATE(XP_DRIP)
REAL, POINTER, DIMENSION(:) :: XP_IFLOOD    ! flood infiltration                           (kg/m2/s)
!$OMP THREADPRIVATE(XP_IFLOOD)
REAL, POINTER, DIMENSION(:) :: XP_PFLOOD    ! precipitation intercepted by the floodplains (kg/m2/s)
!$OMP THREADPRIVATE(XP_PFLOOD)
REAL, POINTER, DIMENSION(:) :: XP_LE_FLOOD  ! flood evaporation                            (W/m2)
!$OMP THREADPRIVATE(XP_LE_FLOOD)
REAL, POINTER, DIMENSION(:) :: XP_LEI_FLOOD ! frozen flood evaporation                     (W/m2)
!$OMP THREADPRIVATE(XP_LEI_FLOOD)
REAL, POINTER, DIMENSION(:) :: XP_ICEFLUX
!$OMP THREADPRIVATE(XP_ICEFLUX)
REAL, POINTER, DIMENSION(:) :: XP_RRVEG     ! precipitation intercepted by the vegetation   (kg/m2/s)
!$OMP THREADPRIVATE(XP_RRVEG)
REAL, POINTER, DIMENSION(:) :: XP_IRRIG_FLUX! irrigation rate                               (kg/m2/s)
!$OMP THREADPRIVATE(XP_IRRIG_FLUX)
!
!------------------------------------------------------------------------------
!
REAL, POINTER, DIMENSION(:) :: XP_GPP         ! Gross primary production (kgCO2/m2/s)
!$OMP THREADPRIVATE(XP_GPP)
REAL, POINTER, DIMENSION(:) :: XP_RESP_AUTO   ! Autotrophic respiration  (kgCO2/m2/s)
!$OMP THREADPRIVATE(XP_RESP_AUTO)
REAL, POINTER, DIMENSION(:) :: XP_RESP_ECO    ! Ecosystem respiration    (kgCO2/m2/s)
!$OMP THREADPRIVATE(XP_RESP_ECO)
REAL, POINTER, DIMENSION(:) :: XP_FAPAR       ! Fapar of vegetation
!$OMP THREADPRIVATE(XP_FAPAR)
REAL, POINTER, DIMENSION(:) :: XP_FAPIR       ! Fapir of vegetation 
!$OMP THREADPRIVATE(XP_FAPIR)
REAL, POINTER, DIMENSION(:) :: XP_FAPAR_BS    ! Fapar of bare soil
!$OMP THREADPRIVATE(XP_FAPAR_BS)
REAL, POINTER, DIMENSION(:) :: XP_FAPIR_BS    ! Fapir of bare soil
!$OMP THREADPRIVATE(XP_FAPIR_BS)
!
!------------------------------------------------------------------------------
!
REAL, POINTER, DIMENSION(:) :: XP_DWG         ! liquid soil moisture time tendencies  (kg/m2/s)
!$OMP THREADPRIVATE(XP_DWG)
REAL, POINTER, DIMENSION(:) :: XP_DWGI        ! solid soil moisture time tendencies   (kg/m2/s)
!$OMP THREADPRIVATE(XP_DWGI)
REAL, POINTER, DIMENSION(:) :: XP_DWR         ! canopy water time tendencies          (kg/m2/s)
!$OMP THREADPRIVATE(XP_DWR)
REAL, POINTER, DIMENSION(:) :: XP_DSWE        ! snow water equivalent time tendencies (kg/m2/s)
!$OMP THREADPRIVATE(XP_DSWE)
REAL, POINTER, DIMENSION(:) :: XP_WATBUD      ! ISBA water budget                     (kg/m2/s)
!$OMP THREADPRIVATE(XP_WATBUD)
!
!------------------------------------------------------------------------------
!
END MODULE MODD_PACK_DIAG_ISBA


