!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #####################
      MODULE MODD_GR_BIOG_n
!     ######################
!
!!
!!!!****  *MODD_GR_BIOG_n* - Declaration of variables for biogenic emissions 
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!    AUTHOR
!!    ------
!!      F. Solmon  *LA*
!!
!!    MODIFICATIONS
!!    -------------
!!    P. Tulet  30/07/03 externalisation of biogenics fluxes
!!     
!*       0.   DECLARATIONS
!             ----------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE GR_BIOG_t
!
!
!
!* Emission potential for isoprene and monoterpenes 
  REAL, DIMENSION(:), POINTER :: XISOPOT 
  REAL, DIMENSION(:), POINTER :: XMONOPOT 
!
!* Radiation at different level(cf Gauss) in the canopy
  REAL, DIMENSION(:,:,:),POINTER ::XIACAN ! PAR at 3 gauss level for each patch
!
!* XFISO  = isoprene emission flux (ppp.m.s-1)
!  XFMONO = monoterpenes emission flux (ppp m s-1)
  REAL, DIMENSION(:), POINTER :: XFISO, XFMONO
!SOILNOX
  REAL, DIMENSION(:), POINTER :: XNOFLUX
!
!
END TYPE GR_BIOG_t



 CONTAINS

!




SUBROUTINE GR_BIOG_INIT(YGR_BIOG)
TYPE(GR_BIOG_t), INTENT(INOUT) :: YGR_BIOG
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_GR_BIOG_N:GR_BIOG_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YGR_BIOG%XISOPOT)
  NULLIFY(YGR_BIOG%XMONOPOT)
  NULLIFY(YGR_BIOG%XIACAN)
  NULLIFY(YGR_BIOG%XFISO)
  NULLIFY(YGR_BIOG%XFMONO)
  NULLIFY(YGR_BIOG%XNOFLUX)
IF (LHOOK) CALL DR_HOOK("MODD_GR_BIOG_N:GR_BIOG_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE GR_BIOG_INIT


END MODULE MODD_GR_BIOG_n
