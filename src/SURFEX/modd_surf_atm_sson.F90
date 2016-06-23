!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ####################
      MODULE MODD_SURF_ATM_SSO_n
!     ######################
!
!!****  *MODD_SURF_ATM_SSO - declaration of surface parameters related to orography
!!
!!    PURPOSE
!!    -------
!     Declaration of surface parameters
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
!!      V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2004
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE SURF_ATM_SSO_t
!
!-----------------------------------------------------------------------------------------------------
!
! Type of roughness
!
 CHARACTER(LEN=4) :: CROUGH     ! type of orographic roughness
!                              ! 'NONE'
                               ! 'Z01D'
                               ! 'Z04D'
                               ! 'BE04'

!-----------------------------------------------------------------------------------------------------
!
! Subgrid orography parameters
!
  REAL, DIMENSION(:), POINTER :: XAOSIP,XAOSIM,XAOSJP,XAOSJM
! directional A/S quantities in 4 coordinate directions
! (IP: i index up;  IM: i index down;  JP: j index up;  JM: j index down)
! They are used in soil routines to compute effective roughness length
!
  REAL, DIMENSION(:), POINTER :: XHO2IP,XHO2IM,XHO2JP,XHO2JM
! directional h/2 quantities in 4 coordinate directions
! (IP: i index up;  IM: i index down;  JP: j index up;  JM: j index down)
! They are used in soil routines to compute effective roughness length
!
  REAL, DIMENSION(:), POINTER :: XZ0EFFIP,XZ0EFFIM,XZ0EFFJP,XZ0EFFJM
! directional total roughness lenghts in 4 coordinate directions
! (IP: i index up;  IM: i index down;  JP: j index up;  JM: j index down)
!
  REAL, DIMENSION(:), POINTER   :: XZ0EFFJPDIR    ! heading of J direction (deg from N clockwise)

  REAL, DIMENSION(:), POINTER   :: XZ0REL         ! relief roughness length                 (m)
!
  REAL, DIMENSION(:), POINTER   :: XSSO_SLOPE         ! slope of S.S.O.
  REAL, DIMENSION(:), POINTER   :: XSSO_ANIS          ! anisotropy of S.S.O.
  REAL, DIMENSION(:), POINTER   :: XSSO_DIR           ! direction of S.S.O. (deg from N clockwise) 
  REAL, DIMENSION(:), POINTER   :: XSSO_STDEV         ! S.S.O. standard deviation           (m)
!
!
  REAL, DIMENSION(:), POINTER   :: XAVG_ZS        ! averaged orography                      (m)
  REAL, DIMENSION(:), POINTER   :: XSIL_ZS        ! silhouette orography                    (m)
  REAL, DIMENSION(:), POINTER   :: XMAX_ZS        ! maximum subgrid orography               (m)
  REAL, DIMENSION(:), POINTER   :: XMIN_ZS        ! minimum subgrid orography               (m)
! Zo threshold
  REAL   :: XFRACZ0                                ! Z0=Min(Z0, Href/XFRACZ0)
  REAL   :: XCOEFBE                                ! Beljaars coefficient         
!-----------------------------------------------------------------------------------------------------
!
!


END TYPE SURF_ATM_SSO_t



 CONTAINS

!




SUBROUTINE SURF_ATM_SSO_INIT(YSURF_ATM_SSO)
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: YSURF_ATM_SSO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_SURF_ATM_SSO_N:SURF_ATM_SSO_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YSURF_ATM_SSO%XAOSIP)
  NULLIFY(YSURF_ATM_SSO%XAOSIM)
  NULLIFY(YSURF_ATM_SSO%XAOSJP)
  NULLIFY(YSURF_ATM_SSO%XAOSJM)
  NULLIFY(YSURF_ATM_SSO%XHO2IP)
  NULLIFY(YSURF_ATM_SSO%XHO2IM)
  NULLIFY(YSURF_ATM_SSO%XHO2JP)
  NULLIFY(YSURF_ATM_SSO%XHO2JM)
  NULLIFY(YSURF_ATM_SSO%XZ0EFFIP)
  NULLIFY(YSURF_ATM_SSO%XZ0EFFIM)
  NULLIFY(YSURF_ATM_SSO%XZ0EFFJP)
  NULLIFY(YSURF_ATM_SSO%XZ0EFFJM)
  NULLIFY(YSURF_ATM_SSO%XZ0EFFJPDIR)  
  NULLIFY(YSURF_ATM_SSO%XZ0REL)
  NULLIFY(YSURF_ATM_SSO%XSSO_SLOPE)
  NULLIFY(YSURF_ATM_SSO%XSSO_ANIS)
  NULLIFY(YSURF_ATM_SSO%XSSO_DIR)
  NULLIFY(YSURF_ATM_SSO%XSSO_STDEV)
  NULLIFY(YSURF_ATM_SSO%XAVG_ZS)
  NULLIFY(YSURF_ATM_SSO%XSIL_ZS)
  NULLIFY(YSURF_ATM_SSO%XMAX_ZS)
  NULLIFY(YSURF_ATM_SSO%XMIN_ZS)
YSURF_ATM_SSO%CROUGH=' '
YSURF_ATM_SSO%XFRACZ0=2.
YSURF_ATM_SSO%XCOEFBE=2.
IF (LHOOK) CALL DR_HOOK("MODD_SURF_ATM_SSO_N:SURF_ATM_SSO_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE SURF_ATM_SSO_INIT


END MODULE MODD_SURF_ATM_SSO_n
