!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_VER_SEAFLUX
!     #################################################################################
!
!!****  *PREP_VER_SEAFLUX* - change in SEAFLUX variables due to altitude change
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     S. Malardel 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!
!
USE MODD_SEAFLUX_n,   ONLY : XZS, XSST 
USE MODD_PREP,   ONLY : XZS_LS, XT_CLIM_GRAD
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!*      0.2    declarations of local variables
!
!
!-------------------------------------------------------------------------------------
!
!*      1.3    SST
!

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('PREP_VER_SEAFLUX',0,ZHOOK_HANDLE)
XSST = XSST  + XT_CLIM_GRAD  * (XZS - XZS_LS)
IF (LHOOK) CALL DR_HOOK('PREP_VER_SEAFLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_VER_SEAFLUX
