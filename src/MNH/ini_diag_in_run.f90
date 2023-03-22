!MNH_LIC Copyright 2003-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      #########################
MODULE MODI_INI_DIAG_IN_RUN
!      #########################
!
INTERFACE
!
      SUBROUTINE INI_DIAG_IN_RUN(KIU,KJU,KKU,OFLYER,OSTATION,OPROFILER)
!
INTEGER,            INTENT(IN) :: KIU     ! number of points in X direction
INTEGER,            INTENT(IN) :: KJU     ! number of points in Y direction
INTEGER,            INTENT(IN) :: KKU     ! number of points in Z direction
LOGICAL,            INTENT(IN) :: OFLYER   ! flag for aircraft or balloon
LOGICAL,            INTENT(IN) :: OSTATION ! flag for surface station
LOGICAL,            INTENT(IN) :: OPROFILER! flag for profiler
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_DIAG_IN_RUN
!
END INTERFACE
!
END MODULE MODI_INI_DIAG_IN_RUN
!
!     ###############################################################
      SUBROUTINE INI_DIAG_IN_RUN(KIU,KJU,KKU,OFLYER,OSTATION,OPROFILER)
!     ###############################################################
!
!
!!****  *INI_DIAG_IN_RUN* - 
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!    
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      Valery Masson             * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!     Original 11/2003
!  Q. Libois      02/2018: ECRAD
!  S. Bielli      02/2019: sea salt: significant sea wave height influences salt emission; 5 salt modes
!  P. Wautelet 30/09/2022: bugfix: use XUNDEF from SURFEX for surface variables computed by SURFEX
! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_CONF, ONLY : CPROGRAM
USE MODD_DIAG_IN_RUN
USE MODD_PARAMETERS, ONLY : XUNDEF
USE MODD_SURF_PAR,   ONLY: XUNDEF_SFX => XUNDEF
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
INTEGER,            INTENT(IN) :: KIU      ! number of points in X direction
INTEGER,            INTENT(IN) :: KJU      ! number of points in Y direction
INTEGER,            INTENT(IN) :: KKU      ! number of points in Z direction
LOGICAL,            INTENT(IN) :: OFLYER   ! flag for aircraft or balloon
LOGICAL,            INTENT(IN) :: OSTATION ! flag for surface station
LOGICAL,            INTENT(IN) :: OPROFILER! flag for profiler
!
!-------------------------------------------------------------------------------
!
IF (OFLYER .OR. OSTATION .OR. OPROFILER .OR. CPROGRAM=='DIAG  ') THEN
  LDIAG_IN_RUN = .TRUE.
ELSE
  LDIAG_IN_RUN = .FALSE.
END IF
!
IF (LDIAG_IN_RUN) THEN
  ALLOCATE(XCURRENT_RN    (KIU,KJU))! net radiation
  ALLOCATE(XCURRENT_H     (KIU,KJU))! sensible heat flux
  ALLOCATE(XCURRENT_LE    (KIU,KJU))! Total latent heat flux
  ALLOCATE(XCURRENT_LEI   (KIU,KJU))! Solid latent heat flux  
  ALLOCATE(XCURRENT_GFLUX (KIU,KJU))! ground flux
  ALLOCATE(XCURRENT_LWD   (KIU,KJU))! incoming longwave at the surface
  ALLOCATE(XCURRENT_LWU   (KIU,KJU))! outcoming longwave at the surface
  ALLOCATE(XCURRENT_SWD   (KIU,KJU))! incoming Shortwave at the surface
  ALLOCATE(XCURRENT_SWU   (KIU,KJU))! outcoming Shortwave at the surface
  ALLOCATE(XCURRENT_SWDIR (KIU,KJU))! incoming Shortwave direct at the surface
  ALLOCATE(XCURRENT_SWDIFF(KIU,KJU))! incoming Shortwave diffuse at the surface  
  ALLOCATE(XCURRENT_T2M   (KIU,KJU))! temperature at 2m
  ALLOCATE(XCURRENT_Q2M   (KIU,KJU))! humidity at 2m
  ALLOCATE(XCURRENT_HU2M   (KIU,KJU))! humidity at 2m
  ALLOCATE(XCURRENT_ZON10M(KIU,KJU))! zonal wind at 10m
  ALLOCATE(XCURRENT_MER10M(KIU,KJU))! meridian wind at 10m
  ALLOCATE(XCURRENT_DSTAOD(KIU,KJU))! dust aerosol optical depth
  ALLOCATE(XCURRENT_SFCO2 (KIU,KJU))! CO2 Surface flux
  ALLOCATE(XCURRENT_TKE_DISS(KIU,KJU,KKU)) ! Tke dissipation rate
  ALLOCATE(XCURRENT_SLTAOD(KIU,KJU))! Salt aerosol optical depth
  ALLOCATE(XCURRENT_ZWS(KIU,KJU)) ! Significant height of waves
  !
  !
  XCURRENT_RN    = XUNDEF_SFX
  XCURRENT_H     = XUNDEF_SFX
  XCURRENT_LE    = XUNDEF_SFX
  XCURRENT_LEI   = XUNDEF_SFX
  XCURRENT_GFLUX = XUNDEF_SFX
  XCURRENT_LWD   = XUNDEF
  XCURRENT_LWU   = XUNDEF
  XCURRENT_SWD   = XUNDEF
  XCURRENT_SWU   = XUNDEF
  XCURRENT_SWDIR = XUNDEF
  XCURRENT_SWDIFF= XUNDEF  
  XCURRENT_T2M   = XUNDEF_SFX
  XCURRENT_Q2M   = XUNDEF_SFX
  XCURRENT_HU2M  = XUNDEF_SFX
  XCURRENT_ZON10M= XUNDEF_SFX
  XCURRENT_MER10M= XUNDEF_SFX
  XCURRENT_DSTAOD= XUNDEF
  XCURRENT_SFCO2 = XUNDEF_SFX
  XCURRENT_TKE_DISS = XUNDEF
  XCURRENT_SLTAOD= XUNDEF
  XCURRENT_ZWS = XUNDEF_SFX
ELSE
  ALLOCATE(XCURRENT_RN    (0,0))! net radiation
  ALLOCATE(XCURRENT_H     (0,0))! sensible heat flux
  ALLOCATE(XCURRENT_LE    (0,0))! Total latent heat flux
  ALLOCATE(XCURRENT_LEI   (0,0))! Solid latent heat flux  
  ALLOCATE(XCURRENT_GFLUX (0,0))! ground flux
  ALLOCATE(XCURRENT_LWD   (0,0))! incoming longwave at the surface
  ALLOCATE(XCURRENT_LWU   (0,0))! outcoming longwave at the surface
  ALLOCATE(XCURRENT_SWD   (0,0))! incoming Shortwave at the surface
  ALLOCATE(XCURRENT_SWU   (0,0))! outcoming Shortwave at the surface
  ALLOCATE(XCURRENT_SWDIR (0,0))! incoming Shortwave direct at the surface
  ALLOCATE(XCURRENT_SWDIFF(0,0))! incoming Shortwave diffuse at the surface  
  ALLOCATE(XCURRENT_T2M   (0,0))! temperature at 2m
  ALLOCATE(XCURRENT_Q2M   (0,0))! humidity at 2m
  ALLOCATE(XCURRENT_HU2M  (0,0))! humidity at 2m
  ALLOCATE(XCURRENT_ZON10M(0,0))! zonal wind at 10m
  ALLOCATE(XCURRENT_MER10M(0,0))! meridian wind at 10m
  ALLOCATE(XCURRENT_DSTAOD(0,0))! dust aerosol optical depth
  ALLOCATE(XCURRENT_SFCO2 (0,0))! CO2 Surface flux
  ALLOCATE(XCURRENT_TKE_DISS(0,0,0)) ! Tke dissipation rate
  ALLOCATE(XCURRENT_SLTAOD(0,0))! Salt aerosol optical depth
  ALLOCATE(XCURRENT_ZWS(0,0))! Significant height of waves
END IF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_DIAG_IN_RUN
!
