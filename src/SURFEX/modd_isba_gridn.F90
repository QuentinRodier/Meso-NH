!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODD_ISBA_GRID_n
!     ##################
!
!!****  *MODD_ISBA - declaration of grid for ISBA scheme
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
!!      V. Masson   *Meteo France*
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

TYPE ISBA_GRID_t
!-------------------------------------------------------------------------------
!
! Grid definition
!
  INTEGER                         :: NDIM        ! number of points
  CHARACTER(LEN=10)               :: CGRID       ! grid type
!                                              ! "NONE        " : no grid computations
!                                              ! "CONF PROJ   " : conformal projection
!                                              ! "SURF ATM    " : nature points of surf. atm. grid
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

END TYPE ISBA_GRID_t



 CONTAINS

!




SUBROUTINE ISBA_GRID_INIT(YISBA_GRID)
TYPE(ISBA_GRID_t), INTENT(INOUT) :: YISBA_GRID
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_ISBA_GRID_N:ISBA_GRID_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YISBA_GRID%XGRID_PAR)
  NULLIFY(YISBA_GRID%XLAT)
  NULLIFY(YISBA_GRID%XLON)
  NULLIFY(YISBA_GRID%XMESH_SIZE)
YISBA_GRID%NDIM=0
YISBA_GRID%CGRID=' '
IF (LHOOK) CALL DR_HOOK("MODD_ISBA_GRID_N:ISBA_GRID_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE ISBA_GRID_INIT


END MODULE MODD_ISBA_GRID_n
