!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 profiler 2006/10/24 10:07:46
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
!!     Original 11/2003
!!
!! --------------------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
USE MODD_CONF, ONLY : CPROGRAM
USE MODD_PARAMETERS, ONLY : XUNDEF
USE MODD_DIAG_IN_RUN
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
  ALLOCATE(XCURRENT_LW    (KIU,KJU))! incoming longwave at the surface
  ALLOCATE(XCURRENT_SW    (KIU,KJU))! incoming Shortwave at the surface
  ALLOCATE(XCURRENT_T2M   (KIU,KJU))! temperature at 2m
  ALLOCATE(XCURRENT_Q2M   (KIU,KJU))! humidity at 2m
  ALLOCATE(XCURRENT_HU2M   (KIU,KJU))! humidity at 2m
  ALLOCATE(XCURRENT_ZON10M(KIU,KJU))! zonal wind at 10m
  ALLOCATE(XCURRENT_MER10M(KIU,KJU))! meridian wind at 10m
  ALLOCATE(XCURRENT_DSTAOD(KIU,KJU))! dust aerosol optical depth
  ALLOCATE(XCURRENT_SFCO2 (KIU,KJU))! CO2 Surface flux
  ALLOCATE(XCURRENT_TKE_DISS(KIU,KJU,KKU)) ! Tke dissipation rate
  !
  !
  XCURRENT_RN    = XUNDEF
  XCURRENT_H     = XUNDEF
  XCURRENT_LE    = XUNDEF
  XCURRENT_LEI   = XUNDEF  
  XCURRENT_GFLUX = XUNDEF
  XCURRENT_LW    = XUNDEF
  XCURRENT_SW    = XUNDEF
  XCURRENT_T2M   = XUNDEF
  XCURRENT_Q2M   = XUNDEF
  XCURRENT_HU2M  = XUNDEF
  XCURRENT_ZON10M= XUNDEF
  XCURRENT_MER10M= XUNDEF
  XCURRENT_DSTAOD= XUNDEF
  XCURRENT_SFCO2 = XUNDEF
  XCURRENT_TKE_DISS = XUNDEF
ELSE
  ALLOCATE(XCURRENT_RN    (0,0))! net radiation
  ALLOCATE(XCURRENT_H     (0,0))! sensible heat flux
  ALLOCATE(XCURRENT_LE    (0,0))! Total latent heat flux
  ALLOCATE(XCURRENT_LEI   (0,0))! Solid latent heat flux  
  ALLOCATE(XCURRENT_GFLUX (0,0))! ground flux
  ALLOCATE(XCURRENT_LW    (0,0))! incoming longwave at the surface
  ALLOCATE(XCURRENT_SW    (0,0))! incoming Shortwave at the surface
  ALLOCATE(XCURRENT_T2M   (0,0))! temperature at 2m
  ALLOCATE(XCURRENT_Q2M   (0,0))! humidity at 2m
  ALLOCATE(XCURRENT_HU2M  (0,0))! humidity at 2m
  ALLOCATE(XCURRENT_ZON10M(0,0))! zonal wind at 10m
  ALLOCATE(XCURRENT_MER10M(0,0))! meridian wind at 10m
  ALLOCATE(XCURRENT_DSTAOD(0,0))! dust aerosol optical depth
  ALLOCATE(XCURRENT_SFCO2 (0,0))! CO2 Surface flux
  ALLOCATE(XCURRENT_TKE_DISS(0,0,0)) ! Tke dissipation rate
END IF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_DIAG_IN_RUN
!
