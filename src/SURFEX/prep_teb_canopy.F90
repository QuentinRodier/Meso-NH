!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB_CANOPY()
!     #################################################################################
!
!!****  *PREP_TEB_CANOPY* - prepares TEB canopy fields
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
USE MODD_TEB_GRID_n,     ONLY : NDIM
USE MODD_TEB_CANOPY_n,   ONLY : NLVL, XZ
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
INTEGER :: JLAYER
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZZF ! altitudes at half levels
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_CANOPY',0,ZHOOK_HANDLE)
!
!*      1.    number of levels (MUST be at least equal to 2)
!             ----------------
!
NLVL = 6
!
!*      2.    height of half levels (where turbulent fluxes will be)
!             ---------------------
!
!* Warning :   ZZF(:,1)   MUST BE ZERO
ALLOCATE(ZZF(NDIM,NLVL))
ZZF(:,1) = 0.
ZZF(:,2) = 1.
ZZF(:,3) = 3.
ZZF(:,4) = 5.
ZZF(:,5) = 8.
ZZF(:,6) = 12.

ALLOCATE(XZ(NDIM,NLVL))
DO JLAYER=1,NLVL-1
  XZ(:,JLAYER) = 0.5 * (ZZF(:,JLAYER)+ZZF(:,JLAYER+1))
END DO
XZ(:,NLVL) = 1.5 * ZZF(:,NLVL) - 0.5 * ZZF(:,NLVL-1)
!
DEALLOCATE(ZZF)
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_CANOPY',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_TEB_CANOPY
