!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################
      MODULE MODD_DIAG_SURF_ATM_n
!     ######################
!
!!****  *MODD_DIAG_SURF_ATM - declaration of diagnostics for the surface
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
!!      Modified    01/2006 : sea flux parameterization.
!!      Modified    04/2009 : precip for/from restart file.
!!      Modified    08/2009 : BUDGETC for all tiles
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TYPE_DATE_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE DIAG_SURF_ATM_t
!------------------------------------------------------------------------------
!
  REAL    :: XDIAG_TSTEP  ! time step for diagnostics writing
!
  INTEGER :: N2M          ! flag for 2 meters (and 10 meters) quantities
  LOGICAL :: LT2MMW       ! flag to perform modified weighting of 2m temperature
  LOGICAL :: L2M_MIN_ZS   ! flag for 2 meters quantities evaluated on
!                         ! the minimum orographyy of the grid 
  LOGICAL :: LSURF_BUDGET ! flag for surface energy budget
  LOGICAL :: LRAD_BUDGET  ! flag for radiative energy budget      
  LOGICAL :: LCOEF        ! flag for transfer coefficients
  LOGICAL :: LSURF_VARS   ! flag for surface variables
  LOGICAL :: LFRAC        ! flag for writing fractions of each four tiles
  LOGICAL :: LDIAG_GRID   ! flag for mean grid diag
  LOGICAL :: LSURF_BUDGETC       ! flag for surface cumulated energy budget
  LOGICAL :: LRESET_BUDGETC      ! flag for surface cumulated energy budget
  LOGICAL :: LREAD_BUDGETC       ! flag for surface cumulated energy budget
  LOGICAL :: LPROVAR_TO_DIAG     ! switch to write (or not) prognostic variable
                                 ! and allows puting field in diagnostics 
  LOGICAL    :: LSELECT          ! switch to control which fields are written
                                 ! (only those whose naem appears in in text array)
!  
  TYPE(DATE_TIME):: TIME_BUDGETC
!                                  
  CHARACTER(LEN=12), POINTER, DIMENSION(:) :: CSELECT  ! Name of ouput fields if LSELECT=true
!
!* variables for each tile
!
  REAL, POINTER, DIMENSION(:,:) :: XRI_TILE     ! Bulk-Richardson number           (-)
  REAL, POINTER, DIMENSION(:,:) :: XCD_TILE     ! drag coefficient for wind        (W/s2)
  REAL, POINTER, DIMENSION(:,:) :: XCH_TILE     ! drag coefficient for heat        (W/s)
  REAL, POINTER, DIMENSION(:,:) :: XCE_TILE     ! drag coefficient for vapor       (W/s/K)
  REAL, POINTER, DIMENSION(:,:) :: XRN_TILE     ! net radiation at surface         (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XH_TILE      ! sensible heat flux               (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLE_TILE     ! total latent heat flux           (W/m2) 
  REAL, POINTER, DIMENSION(:,:) :: XLEI_TILE    ! sublimation latent heat flux     (W/m2) 
  REAL, POINTER, DIMENSION(:,:) :: XGFLUX_TILE  ! net soil-vegetation flux         (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XEVAP_TILE   ! total evapotranspiration         (kg/m2/s) 
  REAL, POINTER, DIMENSION(:,:) :: XSUBL_TILE   ! sublimation                      (kg/m2/s) 
  REAL, POINTER, DIMENSION(:,:) :: XTS_TILE     ! surface temperature              (K)
  REAL, POINTER, DIMENSION(:,:) :: XT2M_TILE    ! air temperature at 2 meters      (K)
  REAL, POINTER, DIMENSION(:,:) :: XT2M_MIN_TILE! Minimum air temperature at 2 meters (K)
  REAL, POINTER, DIMENSION(:,:) :: XT2M_MAX_TILE! Maximum air temperature at 2 meters (K)
  REAL, POINTER, DIMENSION(:,:) :: XQ2M_TILE    ! air humidity at 2 meters         (kg/kg)
  REAL, POINTER, DIMENSION(:,:) :: XHU2M_TILE   ! air relative humidity at 2 meters(-)
  REAL, POINTER, DIMENSION(:,:) :: XHU2M_MIN_TILE! Minimum air relative humidity at 2 meters(-)
  REAL, POINTER, DIMENSION(:,:) :: XHU2M_MAX_TILE! Maximum air relative humidity at 2 meters(-)
  REAL, POINTER, DIMENSION(:,:) :: XZON10M_TILE ! zonal wind at 10 meters          (m/s)
  REAL, POINTER, DIMENSION(:,:) :: XMER10M_TILE ! meridian wind at 10 meters       (m/s)
  REAL, POINTER, DIMENSION(:,:) :: XWIND10M_TILE! wind at 10 meters                (m/s)
  REAL, POINTER, DIMENSION(:,:) :: XWIND10M_MAX_TILE ! Maximumwind at 10 meters    (m/s)
  REAL, POINTER, DIMENSION(:,:) :: XQS_TILE
  REAL, POINTER, DIMENSION(:,:) :: XZ0_TILE     ! roughness length for momentum    (m)
  REAL, POINTER, DIMENSION(:,:) :: XZ0H_TILE    ! roughness length for heat        (m)
  REAL, POINTER, DIMENSION(:,:) :: XSWD_TILE    ! short wave downward radiation (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWU_TILE    ! short wave upward radiation (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLWD_TILE    ! longt wave downward radiation (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLWU_TILE    ! longt wave upward radiation (W/m2)
  REAL, POINTER, DIMENSION(:,:,:) :: XSWBD_TILE ! short wave downward radiation by spectral band(W/m2)
  REAL, POINTER, DIMENSION(:,:,:) :: XSWBU_TILE ! short wave upward radiation by spectral band(W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XFMU_TILE    ! zonal friction
  REAL, POINTER, DIMENSION(:,:) :: XFMV_TILE    ! meridian friction
!
!* Cumulated variables for each tile
!
  REAL, POINTER, DIMENSION(:,:) :: XRNC_TILE     ! net radiation at surface         (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XHC_TILE      ! sensible heat flux               (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLEC_TILE     ! total latent heat flux           (J/m2) 
  REAL, POINTER, DIMENSION(:,:) :: XLEIC_TILE    ! sublimation latent heat flux     (J/m2) 
  REAL, POINTER, DIMENSION(:,:) :: XGFLUXC_TILE  ! net soil-vegetation flux         (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XEVAPC_TILE   ! total evapotranspiration         (kg/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSUBLC_TILE   ! sublimation                      (kg/m2) 
  REAL, POINTER, DIMENSION(:,:) :: XSWDC_TILE    ! short wave downward radiation    (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWUC_TILE    ! short wave upward radiation      (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLWDC_TILE    ! longt wave downward radiation    (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XLWUC_TILE    ! longt wave upward radiation      (J/m2)
  REAL, POINTER, DIMENSION(:,:) :: XFMUC_TILE    ! zonal friction
  REAL, POINTER, DIMENSION(:,:) :: XFMVC_TILE    ! meridian friction
!
!* averaged variables
!
  REAL, POINTER, DIMENSION(:)   :: XAVG_RI      ! Bulk-Richardson number           (-)
  REAL, POINTER, DIMENSION(:)   :: XAVG_CD       ! drag coefficient for wind        (W/s2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_CH       ! drag coefficient for heat        (W/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_CE       ! drag coefficient for vapor       (W/s/K)
  REAL, POINTER, DIMENSION(:)   :: XAVG_RN      ! net radiation at surface         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_H       ! sensible heat flux               (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LE      ! total latent heat flux           (W/m2) 
  REAL, POINTER, DIMENSION(:)   :: XAVG_LEI     ! sublimation latent heat flux     (W/m2) 
  REAL, POINTER, DIMENSION(:)   :: XAVG_GFLUX   ! net soil-vegetation flux         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_EVAP    ! total evapotranspiration         (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_SUBL    ! sublimation                      (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_TS      ! surface temperature              (K)
  REAL, POINTER, DIMENSION(:)   :: XAVG_T2M     ! air temperature at 2 meters      (K)
  REAL, POINTER, DIMENSION(:)   :: XAVG_Q2M     ! air humidity at 2 meters         (kg/kg)
  REAL, POINTER, DIMENSION(:)   :: XAVG_HU2M    ! air relative humidity at 2 meters(-)
  REAL, POINTER, DIMENSION(:)   :: XAVG_ZON10M  ! zonal wind at 10 meters          (m/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_MER10M  ! meridian wind at 10 meters       (m/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_SFCO2   ! CO2 flux                         (m/s*kg_CO2/kg_air)  
  REAL, POINTER, DIMENSION(:)   :: XAVG_T2M_MIN_ZS ! air temperature at 2 meters   (K)
  REAL, POINTER, DIMENSION(:)   :: XAVG_Q2M_MIN_ZS ! air humidity at 2 meters      (kg/kg)
  REAL, POINTER, DIMENSION(:)   :: XAVG_HU2M_MIN_ZS! air relative humidity at 2 m  (-)
  REAL, POINTER, DIMENSION(:)   :: XPS          ! air pressure at the surface      (Pa)
  REAL, POINTER, DIMENSION(:)   :: XRHOA        ! air density  at the surface      (kg/m3)
  REAL, POINTER, DIMENSION(:)   :: XAVG_QS
  REAL, POINTER, DIMENSION(:)   :: XAVG_Z0      ! roughness length for momentum    (m)
  REAL, POINTER, DIMENSION(:)   :: XAVG_Z0H     ! roughness length for heat        (m)
  REAL, POINTER, DIMENSION(:)   :: XAVG_SWD     ! short wave downward radiation (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_SWU     ! short wave upward radiation (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LWD     ! longt wave downward radiation (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LWU     ! longt wave upward radiation (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XAVG_SWBD    ! short wave downward radiation by spectral band(W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XAVG_SWBU    ! short wave upward radiation by spectral band(W/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_FMU     ! zonal friction
  REAL, POINTER, DIMENSION(:)   :: XAVG_FMV     ! meridian friction
  REAL, POINTER, DIMENSION(:)   :: XSSO_FMU     ! zonal friction    (with SSO)     (Pa)
  REAL, POINTER, DIMENSION(:)   :: XSSO_FMV     ! meridian friction (with SSO)     (Pa)
!
  REAL, POINTER, DIMENSION(:)   :: XDIAG_UREF   ! reference height for momentum    (m)
  REAL, POINTER, DIMENSION(:)   :: XDIAG_ZREF   ! reference height for heat        (m)
  REAL, POINTER, DIMENSION(:)   :: XDIAG_TRAD   ! radiative temperature at t       (K)
  REAL, POINTER, DIMENSION(:)   :: XDIAG_EMIS   ! surface emissivity at t          (-)
!
  REAL, POINTER, DIMENSION(:)   :: XAVG_T2M_MIN ! Minimun air temperature at 2 meters      (K)
  REAL, POINTER, DIMENSION(:)   :: XAVG_T2M_MAX ! Maximum air temperature at 2 meters      (K)
  REAL, POINTER, DIMENSION(:)   :: XAVG_HU2M_MIN! Minimun air relative humidity at 2 meters(-)
  REAL, POINTER, DIMENSION(:)   :: XAVG_HU2M_MAX! Maximum air relative humidity at 2 meters(-)
  REAL, POINTER, DIMENSION(:)   :: XAVG_WIND10M ! wind at 10 meters                      (m/s)
  REAL, POINTER, DIMENSION(:)   :: XAVG_WIND10M_MAX ! Maximum wind at 10 meters          (m/s)
!
!* cumulated averaged variables
!
  REAL, POINTER, DIMENSION(:)   :: XAVG_RNC      ! net radiation at surface         (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_HC       ! sensible heat flux               (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LEC      ! total latent heat flux           (J/m2) 
  REAL, POINTER, DIMENSION(:)   :: XAVG_LEIC     ! sublimation latent heat flux     (J/m2) 
  REAL, POINTER, DIMENSION(:)   :: XAVG_GFLUXC   ! net soil-vegetation flux         (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_EVAPC    ! total evapotranspiration         (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_SUBLC    ! sublimation                      (kg/m2)  
  REAL, POINTER, DIMENSION(:)   :: XAVG_SWDC     ! short wave downward radiation    (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_SWUC     ! short wave upward radiation      (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LWDC     ! longt wave downward radiation    (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_LWUC     ! longt wave upward radiation      (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XAVG_FMUC     ! zonal friction
  REAL, POINTER, DIMENSION(:)   :: XAVG_FMVC     ! meridian friction
!
!------------------------------------------------------------------------------
!

END TYPE DIAG_SURF_ATM_t



 CONTAINS

!


!

SUBROUTINE DIAG_SURF_ATM_INIT(YDIAG_SURF_ATM)
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: YDIAG_SURF_ATM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_SURF_ATM_N:DIAG_SURF_ATM_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YDIAG_SURF_ATM%XRI_TILE)
  NULLIFY(YDIAG_SURF_ATM%XCD_TILE)
  NULLIFY(YDIAG_SURF_ATM%XCH_TILE)
  NULLIFY(YDIAG_SURF_ATM%XCE_TILE)
  NULLIFY(YDIAG_SURF_ATM%XRN_TILE)
  NULLIFY(YDIAG_SURF_ATM%XH_TILE)
  NULLIFY(YDIAG_SURF_ATM%XLE_TILE)
  NULLIFY(YDIAG_SURF_ATM%XLEI_TILE)
  NULLIFY(YDIAG_SURF_ATM%XGFLUX_TILE)
  NULLIFY(YDIAG_SURF_ATM%XEVAP_TILE)
  NULLIFY(YDIAG_SURF_ATM%XSUBL_TILE)
  NULLIFY(YDIAG_SURF_ATM%XTS_TILE)
  NULLIFY(YDIAG_SURF_ATM%XT2M_TILE)
  NULLIFY(YDIAG_SURF_ATM%XT2M_MIN_TILE)
  NULLIFY(YDIAG_SURF_ATM%XT2M_MAX_TILE)
  NULLIFY(YDIAG_SURF_ATM%XQ2M_TILE)
  NULLIFY(YDIAG_SURF_ATM%XHU2M_TILE)
  NULLIFY(YDIAG_SURF_ATM%XHU2M_MIN_TILE)
  NULLIFY(YDIAG_SURF_ATM%XHU2M_MAX_TILE)
  NULLIFY(YDIAG_SURF_ATM%XZON10M_TILE)
  NULLIFY(YDIAG_SURF_ATM%XMER10M_TILE)
  NULLIFY(YDIAG_SURF_ATM%XWIND10M_TILE)
  NULLIFY(YDIAG_SURF_ATM%XWIND10M_MAX_TILE)
  NULLIFY(YDIAG_SURF_ATM%XQS_TILE)
  NULLIFY(YDIAG_SURF_ATM%XZ0_TILE)
  NULLIFY(YDIAG_SURF_ATM%XZ0H_TILE)
  NULLIFY(YDIAG_SURF_ATM%XSWD_TILE)
  NULLIFY(YDIAG_SURF_ATM%XSWU_TILE)
  NULLIFY(YDIAG_SURF_ATM%XLWD_TILE)
  NULLIFY(YDIAG_SURF_ATM%XLWU_TILE)
  NULLIFY(YDIAG_SURF_ATM%XSWBD_TILE)
  NULLIFY(YDIAG_SURF_ATM%XSWBU_TILE)
  NULLIFY(YDIAG_SURF_ATM%XFMU_TILE)
  NULLIFY(YDIAG_SURF_ATM%XFMV_TILE)
  NULLIFY(YDIAG_SURF_ATM%XRNC_TILE)
  NULLIFY(YDIAG_SURF_ATM%XHC_TILE)
  NULLIFY(YDIAG_SURF_ATM%XLEC_TILE)
  NULLIFY(YDIAG_SURF_ATM%XLEIC_TILE)
  NULLIFY(YDIAG_SURF_ATM%XGFLUXC_TILE)
  NULLIFY(YDIAG_SURF_ATM%XEVAPC_TILE)
  NULLIFY(YDIAG_SURF_ATM%XSUBLC_TILE)
  NULLIFY(YDIAG_SURF_ATM%XSWDC_TILE)
  NULLIFY(YDIAG_SURF_ATM%XSWUC_TILE)
  NULLIFY(YDIAG_SURF_ATM%XLWDC_TILE)
  NULLIFY(YDIAG_SURF_ATM%XLWUC_TILE)
  NULLIFY(YDIAG_SURF_ATM%XFMUC_TILE)
  NULLIFY(YDIAG_SURF_ATM%XFMVC_TILE)
  NULLIFY(YDIAG_SURF_ATM%XAVG_RI)
  NULLIFY(YDIAG_SURF_ATM%XAVG_CD)
  NULLIFY(YDIAG_SURF_ATM%XAVG_CH)
  NULLIFY(YDIAG_SURF_ATM%XAVG_CE)
  NULLIFY(YDIAG_SURF_ATM%XAVG_RN)
  NULLIFY(YDIAG_SURF_ATM%XAVG_H)
  NULLIFY(YDIAG_SURF_ATM%XAVG_LE)
  NULLIFY(YDIAG_SURF_ATM%XAVG_LEI)
  NULLIFY(YDIAG_SURF_ATM%XAVG_GFLUX)
  NULLIFY(YDIAG_SURF_ATM%XAVG_EVAP)
  NULLIFY(YDIAG_SURF_ATM%XAVG_SUBL)  
  NULLIFY(YDIAG_SURF_ATM%XAVG_TS)
  NULLIFY(YDIAG_SURF_ATM%XAVG_T2M)
  NULLIFY(YDIAG_SURF_ATM%XAVG_Q2M)
  NULLIFY(YDIAG_SURF_ATM%XAVG_HU2M)
  NULLIFY(YDIAG_SURF_ATM%XAVG_ZON10M)
  NULLIFY(YDIAG_SURF_ATM%XAVG_MER10M)
  NULLIFY(YDIAG_SURF_ATM%XAVG_SFCO2)
  NULLIFY(YDIAG_SURF_ATM%XAVG_T2M_MIN_ZS)
  NULLIFY(YDIAG_SURF_ATM%XAVG_Q2M_MIN_ZS)
  NULLIFY(YDIAG_SURF_ATM%XAVG_HU2M_MIN_ZS)
  NULLIFY(YDIAG_SURF_ATM%XPS)
  NULLIFY(YDIAG_SURF_ATM%XRHOA)
  NULLIFY(YDIAG_SURF_ATM%XAVG_QS)
  NULLIFY(YDIAG_SURF_ATM%XAVG_Z0)
  NULLIFY(YDIAG_SURF_ATM%XAVG_Z0H)
  NULLIFY(YDIAG_SURF_ATM%XAVG_SWD)
  NULLIFY(YDIAG_SURF_ATM%XAVG_SWU)
  NULLIFY(YDIAG_SURF_ATM%XAVG_LWD)
  NULLIFY(YDIAG_SURF_ATM%XAVG_LWU)
  NULLIFY(YDIAG_SURF_ATM%XAVG_SWBD)
  NULLIFY(YDIAG_SURF_ATM%XAVG_SWBU)
  NULLIFY(YDIAG_SURF_ATM%XAVG_FMU)
  NULLIFY(YDIAG_SURF_ATM%XAVG_FMV)
  NULLIFY(YDIAG_SURF_ATM%XSSO_FMU)
  NULLIFY(YDIAG_SURF_ATM%XSSO_FMV)
  NULLIFY(YDIAG_SURF_ATM%XDIAG_UREF)
  NULLIFY(YDIAG_SURF_ATM%XDIAG_ZREF)
  NULLIFY(YDIAG_SURF_ATM%XDIAG_TRAD)
  NULLIFY(YDIAG_SURF_ATM%XDIAG_EMIS)
  NULLIFY(YDIAG_SURF_ATM%XAVG_T2M_MIN)
  NULLIFY(YDIAG_SURF_ATM%XAVG_T2M_MAX)
  NULLIFY(YDIAG_SURF_ATM%XAVG_HU2M_MIN)
  NULLIFY(YDIAG_SURF_ATM%XAVG_HU2M_MAX)
  NULLIFY(YDIAG_SURF_ATM%XAVG_WIND10M)
  NULLIFY(YDIAG_SURF_ATM%XAVG_WIND10M_MAX)
  NULLIFY(YDIAG_SURF_ATM%XAVG_RNC)
  NULLIFY(YDIAG_SURF_ATM%XAVG_HC)
  NULLIFY(YDIAG_SURF_ATM%XAVG_LEC)
  NULLIFY(YDIAG_SURF_ATM%XAVG_LEIC)
  NULLIFY(YDIAG_SURF_ATM%XAVG_GFLUXC)
  NULLIFY(YDIAG_SURF_ATM%XAVG_EVAPC)
  NULLIFY(YDIAG_SURF_ATM%XAVG_SUBLC)
  NULLIFY(YDIAG_SURF_ATM%XAVG_SWDC)
  NULLIFY(YDIAG_SURF_ATM%XAVG_SWUC)
  NULLIFY(YDIAG_SURF_ATM%XAVG_LWDC)
  NULLIFY(YDIAG_SURF_ATM%XAVG_LWUC)
  NULLIFY(YDIAG_SURF_ATM%XAVG_FMUC)
  NULLIFY(YDIAG_SURF_ATM%XAVG_FMVC)
  NULLIFY(YDIAG_SURF_ATM%CSELECT)
YDIAG_SURF_ATM%XDIAG_TSTEP=0.
YDIAG_SURF_ATM%N2M=0
YDIAG_SURF_ATM%LT2MMW=.FALSE.
YDIAG_SURF_ATM%L2M_MIN_ZS=.FALSE.
YDIAG_SURF_ATM%LSURF_BUDGET=.FALSE.
YDIAG_SURF_ATM%LRAD_BUDGET=.FALSE.
YDIAG_SURF_ATM%LCOEF=.FALSE.
YDIAG_SURF_ATM%LSURF_VARS=.FALSE.
YDIAG_SURF_ATM%LFRAC=.FALSE.
YDIAG_SURF_ATM%LDIAG_GRID=.FALSE.
YDIAG_SURF_ATM%LSURF_BUDGETC=.FALSE.
YDIAG_SURF_ATM%LRESET_BUDGETC=.FALSE.
YDIAG_SURF_ATM%LREAD_BUDGETC=.FALSE.
YDIAG_SURF_ATM%LPROVAR_TO_DIAG=.FALSE.
YDIAG_SURF_ATM%LSELECT=.FALSE.
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_SURF_ATM_N:DIAG_SURF_ATM_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DIAG_SURF_ATM_INIT


END MODULE MODD_DIAG_SURF_ATM_n
