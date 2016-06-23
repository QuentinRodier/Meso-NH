!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE SET_SSO_LEVELS (SSCP, &
                           KDIM)
!     #################################################################################
!
!!****  *SET_SSO_LEVELS* - prepares SSO canopy fields
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
!
USE MODD_SSO_CANOPY_n, ONLY : SSO_CANOPY_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(SSO_CANOPY_t), INTENT(INOUT) :: SSCP
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
IF (LHOOK) CALL DR_HOOK('SET_SSO_LEVELS',0,ZHOOK_HANDLE)
SSCP%NLVL = 6
!
!*      2.    height of half levels (where turbulent fluxes will be)
!             ---------------------
!
!* Warning :   ZZF(:,1)   MUST BE ZERO
ALLOCATE(ZZF(KDIM,SSCP%NLVL))
ZZF(:,1) = 0.
ZZF(:,2) = 1
ZZF(:,3) = 3.
ZZF(:,4) = 5.
ZZF(:,5) = 8.
ZZF(:,6) = 12.

ALLOCATE(SSCP%XZ(KDIM,SSCP%NLVL))
DO JLAYER=1,SSCP%NLVL-1
  SSCP%XZ(:,JLAYER) = 0.5 * (ZZF(:,JLAYER)+ZZF(:,JLAYER+1))
END DO
SSCP%XZ(:,SSCP%NLVL) = 1.5 * ZZF(:,SSCP%NLVL) - 0.5 * ZZF(:,SSCP%NLVL-1)
!
DEALLOCATE(ZZF)
!
!*      3.    wind in canopy (m/s)
!             --------------
!
ALLOCATE(SSCP%XU(KDIM,SSCP%NLVL))
SSCP%XU(:,:) = XUNDEF
!
!*      4.    Tke in canopy (m2/s2)
!             -------------
!
ALLOCATE(SSCP%XTKE(KDIM,SSCP%NLVL))
SSCP%XTKE(:,:) = XUNDEF
!
IF (LHOOK) CALL DR_HOOK('SET_SSO_LEVELS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE SET_SSO_LEVELS
