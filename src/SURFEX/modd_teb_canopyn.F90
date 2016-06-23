!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_TEB_CANOPY_n
!     ################
!
!!****  *MODD_TEB_CANOPY_n - declaration of surface parameters for urban canopy
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

TYPE TEB_CANOPY_t
!
  INTEGER                       :: NLVL ! number      of levels in canopy
  REAL, POINTER, DIMENSION(:,:) :: XZ   ! height of middle of each level grid   (m)
  REAL, POINTER, DIMENSION(:,:) :: XU   ! wind        at each level in canopy   (m/s)
  REAL, POINTER, DIMENSION(:,:) :: XT   ! temperature at each level in canopy   (m/s)
  REAL, POINTER, DIMENSION(:,:) :: XQ   ! humidity    at each level in canopy   (kg/m3)
  REAL, POINTER, DIMENSION(:,:) :: XTKE ! Tke         at each level in canopy   (m2/s2)
  REAL, POINTER, DIMENSION(:,:) :: XLMO ! Monin-Obhukov length                  (m)
  REAL, POINTER, DIMENSION(:,:) :: XLM  ! Mixing lentgh                         (m)
  REAL, POINTER, DIMENSION(:,:) :: XLEPS! Dissipative length                    (m)
  REAL, POINTER, DIMENSION(:,:) :: XP   ! pressure    at each level in canopy   (kg/m3)
!
  REAL, POINTER, DIMENSION(:,:) :: XDZ  ! depth       of each level in canopy   (m)
  REAL, POINTER, DIMENSION(:,:) :: XZF  ! height of bottom of each level grid   (m)
  REAL, POINTER, DIMENSION(:,:) :: XDZF ! depth between  each level in canopy   (m)
!
END TYPE TEB_CANOPY_t



 CONTAINS

!




SUBROUTINE TEB_CANOPY_INIT(YTEB_CANOPY)
TYPE(TEB_CANOPY_t), INTENT(INOUT) :: YTEB_CANOPY
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_CANOPY_N:TEB_CANOPY_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YTEB_CANOPY%XZ)
  NULLIFY(YTEB_CANOPY%XU)
  NULLIFY(YTEB_CANOPY%XT)
  NULLIFY(YTEB_CANOPY%XQ)
  NULLIFY(YTEB_CANOPY%XTKE)
  NULLIFY(YTEB_CANOPY%XLMO)
  NULLIFY(YTEB_CANOPY%XLM)
  NULLIFY(YTEB_CANOPY%XLEPS)
  NULLIFY(YTEB_CANOPY%XP)
  NULLIFY(YTEB_CANOPY%XDZ)
  NULLIFY(YTEB_CANOPY%XZF)
  NULLIFY(YTEB_CANOPY%XDZF)
YTEB_CANOPY%NLVL=0
IF (LHOOK) CALL DR_HOOK("MODD_TEB_CANOPY_N:TEB_CANOPY_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_CANOPY_INIT


END MODULE MODD_TEB_CANOPY_n
