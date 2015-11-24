!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ################
      MODULE MODD_GRID_CONF_PROJ
!     ################
!
!!****  *MODD_GRID_CONF_PROJ - declaration of Arome gris characteristics
!!
!!    PURPOSE
!!    -------
!     Used if CINGRID_TYPE = 'CONF PROJ '
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
!!	V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       20/09/02
!!        M.Faivre     2014
!!        M.Moge       10/2015 fixing bugs from M.Faivre
!
!*       0.   DECLARATIONS
!             ------------
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

REAL, DIMENSION(:), ALLOCATABLE    :: XX  ! X coordinate (meters)
REAL, DIMENSION(:), ALLOCATABLE    :: XY ! Y coordinate (meters)

TYPE GRID_CONF_PROJ_t
!
!!REAL, DIMENSION(:), POINTER    :: XX=>NULL()  ! X coordinate (meters)
!!REAL, DIMENSION(:), POINTER    :: XY=>NULL()  ! Y coordinate (meters)
INTEGER             :: NX  ! number of points in X direction
INTEGER             :: NY  ! number of points in Y direction
!
REAL                :: XLAT0  ! reference latitude
REAL                :: XLON0  ! reference longitude
REAL                :: XLATORI! origin latitude
REAL                :: XLONORI! origin longitude
REAL                :: XRPK   ! projection parameter for the conformal projection
REAL                :: XBETA  ! rotation   parameter for the conformal projection
REAL                :: XLATC ! centre latitude
REAL                :: XLONC ! centre longitude
!
END type GRID_CONF_PROJ_t


TYPE(GRID_CONF_PROJ_t), ALLOCATABLE, TARGET, SAVE :: GRID_CONF_PROJ_MODEL(:)
!
!!!!!!!!!!!!!!!!!!!! LOCAL VARIABLE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!!REAL, DIMENSION(:), POINTER    :: XX=>NULL()  ! X coordinate (meters)
!!REAL, DIMENSION(:), POINTER    :: XY=>NULL()  ! Y coordinate (meters)
INTEGER           ,POINTER     :: NX=>NULL()  ! number of points in X direction
INTEGER           ,POINTER     :: NY=>NULL()  ! number of points in Y direction
!
REAL              ,POINTER     :: XLAT0=>NULL()  ! reference latitude
REAL              ,POINTER     :: XLON0=>NULL()  ! reference longitude
REAL              ,POINTER     :: XLATORI=>NULL()! origin latitude
REAL              ,POINTER     :: XLONORI=>NULL()! origin longitude
REAL              ,POINTER     :: XRPK=>NULL()   ! projection parameter for the conformal projection
REAL              ,POINTER     :: XBETA=>NULL()  ! rotation   parameter for the conformal projection
REAL              ,POINTER     :: XLATC=>NULL() ! centre latitude
REAL              ,POINTER     :: XLONC=>NULL() ! centre longitude

CONTAINS

SUBROUTINE GRID_CONF_PROJ_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
! Save current state for allocated arrays
!leave out of structure XX nd XY since aloocated and deallocatd in PGCP
!!GRID_CONF_PROJ_MODEL(KFROM)%XX=>XX
!!GRID_CONF_PROJ_MODEL(KFROM)%XY=>XY
!
! Current model is set to model KTO
!!XX=>GRID_CONF_PROJ_MODEL(KTO)%XX
!!XY=>GRID_CONF_PROJ_MODEL(KTO)%XY
NX=>GRID_CONF_PROJ_MODEL(KTO)%NX
NY=>GRID_CONF_PROJ_MODEL(KTO)%NY
XLAT0=>GRID_CONF_PROJ_MODEL(KTO)%XLAT0
XLON0=>GRID_CONF_PROJ_MODEL(KTO)%XLON0
XLATORI=>GRID_CONF_PROJ_MODEL(KTO)%XLATORI
XLONORI=>GRID_CONF_PROJ_MODEL(KTO)%XLONORI
XRPK=>GRID_CONF_PROJ_MODEL(KTO)%XRPK
XBETA=>GRID_CONF_PROJ_MODEL(KTO)%XBETA
XLATC=>GRID_CONF_PROJ_MODEL(KTO)%XLATC
XLONC=>GRID_CONF_PROJ_MODEL(KTO)%XLONC
!
END SUBROUTINE GRID_CONF_PROJ_GOTO_MODEL
!
SUBROUTINE GRID_CONF_PROJ_ALLOC(KMODEL)
INTEGER, INTENT(IN) :: KMODEL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_GRID_CONF_PROJ_N:GRID_CONF_PROJ_ALLOC",0,ZHOOK_HANDLE)
ALLOCATE(GRID_CONF_PROJ_MODEL(KMODEL))
IF( KMODEL > 0 ) THEN
  GRID_CONF_PROJ_MODEL(:)%NX = 0
  GRID_CONF_PROJ_MODEL(:)%NY = 0
  GRID_CONF_PROJ_MODEL(:)%XLAT0 = 0.
  GRID_CONF_PROJ_MODEL(:)%XLON0 = 0.
  GRID_CONF_PROJ_MODEL(:)%XLATORI = 0.
  GRID_CONF_PROJ_MODEL(:)%XLONORI = 0.
  GRID_CONF_PROJ_MODEL(:)%XRPK = 0.
  GRID_CONF_PROJ_MODEL(:)%XBETA = 0.
  GRID_CONF_PROJ_MODEL(:)%XLATC = 0.
  GRID_CONF_PROJ_MODEL(:)%XLONC = 0.
ENDIF
IF (LHOOK) CALL DR_HOOK("MODD_WATFLUX_N:WATFLUX_ALLOC",1,ZHOOK_HANDLE)
END SUBROUTINE GRID_CONF_PROJ_ALLOC
!
END MODULE MODD_GRID_CONF_PROJ
