!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ##################
      MODULE MODD_TEB_GRID_n
!     ##################
!
!!****  *MODD_TEB_GRID - declaration of TEB grid
!!
!!    PURPOSE
!!    -------
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
!!	V. Masson   *Meteo France*
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

TYPE TEB_GRID_t
!-------------------------------------------------------------------------------
!
! Grid definition
!
  INTEGER                         :: NDIM        ! number of points
  CHARACTER(LEN=10)               :: CGRID       ! grid type
!                                              ! "NONE        " : no grid computations
!                                              ! "CONF PROJ   " : conformal projection
!                                              ! "SURF ATM    " : town points of surf. atm. grid
!
  REAL, POINTER,     DIMENSION(:) :: XGRID_PAR   ! lits of parameters used to define the grid
!                                              ! (depends on value of CGRID)
!
!-------------------------------------------------------------------------------
!
! General surface parameters:
!
  REAL, POINTER, DIMENSION(:) :: XLAT        ! latitude (degrees +North)               (-)
  REAL, POINTER, DIMENSION(:) :: XLON        ! longitude (degrees +East)               (-)
  REAL, POINTER, DIMENSION(:) :: XMESH_SIZE  ! mesh size                               (m2)
!-------------------------------------------------------------------------------
!

END TYPE TEB_GRID_t

TYPE(TEB_GRID_t), ALLOCATABLE, TARGET, SAVE :: TEB_GRID_MODEL(:)

INTEGER, POINTER :: NDIM=>NULL()
!$OMP THREADPRIVATE(NDIM)
 CHARACTER(LEN=10), POINTER :: CGRID=>NULL()
!$OMP THREADPRIVATE(CGRID)
REAL, POINTER,     DIMENSION(:) :: XGRID_PAR=>NULL()
!$OMP THREADPRIVATE(XGRID_PAR)
REAL, POINTER, DIMENSION(:) :: XLAT=>NULL()
!$OMP THREADPRIVATE(XLAT)
REAL, POINTER, DIMENSION(:) :: XLON=>NULL()
!$OMP THREADPRIVATE(XLON)
REAL, POINTER, DIMENSION(:) :: XMESH_SIZE=>NULL()
!$OMP THREADPRIVATE(XMESH_SIZE)

CONTAINS

SUBROUTINE TEB_GRID_GOTO_MODEL(KFROM, KTO, LKFROM)
LOGICAL, INTENT(IN) :: LKFROM
INTEGER, INTENT(IN) :: KFROM, KTO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Save current state for allocated arrays
IF (LKFROM) THEN
TEB_GRID_MODEL(KFROM)%XGRID_PAR=>XGRID_PAR
TEB_GRID_MODEL(KFROM)%XLAT=>XLAT
TEB_GRID_MODEL(KFROM)%XLON=>XLON
TEB_GRID_MODEL(KFROM)%XMESH_SIZE=>XMESH_SIZE
ENDIF
!
! Current model is set to model KTO
IF (LHOOK) CALL DR_HOOK('MODD_TEB_GRID_N:TEB_GRID_GOTO_MODEL',0,ZHOOK_HANDLE)
NDIM=>TEB_GRID_MODEL(KTO)%NDIM
CGRID=>TEB_GRID_MODEL(KTO)%CGRID
XGRID_PAR=>TEB_GRID_MODEL(KTO)%XGRID_PAR
XLAT=>TEB_GRID_MODEL(KTO)%XLAT
XLON=>TEB_GRID_MODEL(KTO)%XLON
XMESH_SIZE=>TEB_GRID_MODEL(KTO)%XMESH_SIZE
IF (LHOOK) CALL DR_HOOK('MODD_TEB_GRID_N:TEB_GRID_GOTO_MODEL',1,ZHOOK_HANDLE)

END SUBROUTINE TEB_GRID_GOTO_MODEL

SUBROUTINE TEB_GRID_ALLOC(KMODEL)
INTEGER, INTENT(IN) :: KMODEL
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GRID_N:TEB_GRID_ALLOC",0,ZHOOK_HANDLE)
ALLOCATE(TEB_GRID_MODEL(KMODEL))
DO J=1,KMODEL
  NULLIFY(TEB_GRID_MODEL(J)%XGRID_PAR)
  NULLIFY(TEB_GRID_MODEL(J)%XLAT)
  NULLIFY(TEB_GRID_MODEL(J)%XLON)
  NULLIFY(TEB_GRID_MODEL(J)%XMESH_SIZE)
ENDDO
TEB_GRID_MODEL(:)%NDIM=0
TEB_GRID_MODEL(:)%CGRID=' '
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GRID_N:TEB_GRID_ALLOC",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GRID_ALLOC

SUBROUTINE TEB_GRID_DEALLO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GRID_N:TEB_GRID_DEALLO",0,ZHOOK_HANDLE)
IF (ALLOCATED(TEB_GRID_MODEL)) DEALLOCATE(TEB_GRID_MODEL)
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GRID_N:TEB_GRID_DEALLO",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GRID_DEALLO

END MODULE MODD_TEB_GRID_n
