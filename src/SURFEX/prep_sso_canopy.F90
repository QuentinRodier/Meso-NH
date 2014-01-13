!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE PREP_SSO_CANOPY(KDIM)
!     #################################################################################
!
!!****  *PREP_SSO_CANOPY* - prepares SSO canopy fields
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
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2006
!!      S. Riette   06/2009 XT, XU, XQ, XTKE are set to XUNDEF
!!                          No more argument needed
!!      E. Martin   01/2012 XUNDEF fields are no more written in PREP file
!!------------------------------------------------------------------
!
USE MODD_SSO_CANOPY_n,   ONLY : NLVL, XZ, XU, XTKE
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN) :: KDIM ! 1D physical dimension

!
!*      0.2    declarations of local variables
!
INTEGER :: JLAYER
INTEGER :: ILU      ! number of points
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZZF    ! altitudes at half levels
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.    number of levels (MUST be at least equal to 2)
!             ----------------
!
IF (LHOOK) CALL DR_HOOK('PREP_SSO_CANOPY',0,ZHOOK_HANDLE)
NLVL = 6
!
!*      2.    height of half levels (where turbulent fluxes will be)
!             ---------------------
!
!* Warning :   ZZF(:,1)   MUST BE ZERO
ALLOCATE(ZZF(KDIM,NLVL))
ZZF(:,1) = 0.
ZZF(:,2) = 1
ZZF(:,3) = 3.
ZZF(:,4) = 5.
ZZF(:,5) = 8.
ZZF(:,6) = 12.

ALLOCATE(XZ(KDIM,NLVL))
DO JLAYER=1,NLVL-1
  XZ(:,JLAYER) = 0.5 * (ZZF(:,JLAYER)+ZZF(:,JLAYER+1))
END DO
XZ(:,NLVL) = 1.5 * ZZF(:,NLVL) - 0.5 * ZZF(:,NLVL-1)
!
DEALLOCATE(ZZF)
!
!*      3.    wind in canopy (m/s)
!             --------------
!
ALLOCATE(XU(KDIM,NLVL))
XU(:,:) = XUNDEF
!
!*      4.    Tke in canopy (m2/s2)
!             -------------
!
ALLOCATE(XTKE(KDIM,NLVL))
XTKE(:,:) = XUNDEF
!
IF (LHOOK) CALL DR_HOOK('PREP_SSO_CANOPY',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_SSO_CANOPY
