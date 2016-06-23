!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_WATFLUX_SBL_n
!     ################
!
!!****  *MODD_WATFLUX_SBL_n - declaration of surface parameters for SBL
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       07/2006
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE WATFLUX_SBL_t
!
  INTEGER                       :: NLVL ! number      of levels in SBL
  REAL, POINTER, DIMENSION(:,:) :: XZ   ! height of middle of each level grid(m)
  REAL, POINTER, DIMENSION(:,:) :: XU   ! wind        at each level in SBL   (m/s)
  REAL, POINTER, DIMENSION(:,:) :: XT   ! temperature at each level in SBL   (m/s)
  REAL, POINTER, DIMENSION(:,:) :: XQ   ! humidity    at each level in SBL   (kg/m3)
  REAL, POINTER, DIMENSION(:,:) :: XTKE ! Tke         at each level in SBL   (m2/s2)
  REAL, POINTER, DIMENSION(:)   :: XLMO ! Monin-Obhukov length               (m)
  REAL, POINTER, DIMENSION(:,:) :: XP   ! pressure    at each level in SBL   (kg/m3)
!
  REAL, POINTER, DIMENSION(:,:) :: XDZ  ! depth       of each level in SBL   (m)
  REAL, POINTER, DIMENSION(:,:) :: XZF  ! height of bottom of each level grid(m)
  REAL, POINTER, DIMENSION(:,:) :: XDZF ! depth between  each level in SBL   (m)
!
END TYPE WATFLUX_SBL_t



 CONTAINS

!




SUBROUTINE WATFLUX_SBL_INIT(YWATFLUX_SBL)
TYPE(WATFLUX_SBL_t), INTENT(INOUT) :: YWATFLUX_SBL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_WATFLUX_SBL_N:WATFLUX_SBL_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YWATFLUX_SBL%XZ)
  NULLIFY(YWATFLUX_SBL%XU)
  NULLIFY(YWATFLUX_SBL%XT)
  NULLIFY(YWATFLUX_SBL%XQ)
  NULLIFY(YWATFLUX_SBL%XTKE)
  NULLIFY(YWATFLUX_SBL%XLMO)
  NULLIFY(YWATFLUX_SBL%XP)
  NULLIFY(YWATFLUX_SBL%XDZ)
  NULLIFY(YWATFLUX_SBL%XZF)
  NULLIFY(YWATFLUX_SBL%XDZF)
YWATFLUX_SBL%NLVL=0
IF (LHOOK) CALL DR_HOOK("MODD_WATFLUX_SBL_N:WATFLUX_SBL_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE WATFLUX_SBL_INIT


END MODULE MODD_WATFLUX_SBL_n
