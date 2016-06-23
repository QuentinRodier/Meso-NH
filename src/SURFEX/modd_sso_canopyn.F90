!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_SSO_CANOPY_n
!     ################
!
!!****  *MODD_SSO_CANOPY_n - declaration of surface parameters for
!                                orographic canopy
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
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE SSO_CANOPY_t
!
INTEGER                       :: NLVL ! number      of levels in canopy
REAL, POINTER, DIMENSION(:,:) :: XZ   ! height of middle of each level grid   (m)
REAL, POINTER, DIMENSION(:,:) :: XU   ! wind        at each level in canopy   (m/s)
REAL, POINTER, DIMENSION(:,:) :: XTKE ! Tke         at each level in canopy   (m2/s2)
!
REAL, POINTER, DIMENSION(:,:) :: XDZ  ! depth       of each level in canopy   (m)
REAL, POINTER, DIMENSION(:,:) :: XZF  ! height of bottom of each level grid   (m)

REAL, POINTER, DIMENSION(:,:) :: XDZF ! depth between  each level in canopy   (m)
!
END TYPE SSO_CANOPY_t



 CONTAINS

!





SUBROUTINE SSO_CANOPY_INIT(YSSO_CANOPY)
TYPE(SSO_CANOPY_t), INTENT(INOUT) :: YSSO_CANOPY
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_SSO_CANOPY_N:SSO_CANOPY_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YSSO_CANOPY%XZ)
  NULLIFY(YSSO_CANOPY%XU)
  NULLIFY(YSSO_CANOPY%XTKE)
  NULLIFY(YSSO_CANOPY%XDZ)
  NULLIFY(YSSO_CANOPY%XZF)
  NULLIFY(YSSO_CANOPY%XDZF)
YSSO_CANOPY%NLVL=0
IF (LHOOK) CALL DR_HOOK("MODD_SSO_CANOPY_N:SSO_CANOPY_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE SSO_CANOPY_INIT



END MODULE MODD_SSO_CANOPY_n
