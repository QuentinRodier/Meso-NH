!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################################################
#ifdef MNH_PARALLEL
      SUBROUTINE GET_SURF_GRID_DIM_n (UG, &
                                      HGRID,ORECT,KDIM1,KDIM2,&
                                      KGRID_PAR,PGRID_PAR)
#else
      SUBROUTINE GET_SURF_GRID_DIM_n (UG, &
                                      HGRID,ORECT,KDIM1,KDIM2)
#endif                              
!     #######################################################
!
!!**** *GET_SURF_GRID_DIM_n* get the grid mesh dimensions
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!      M.Moge    02/2015 Passing KGRID_PAR,PGRID_PAR as input parameters, instead of using XGRID_PAR, NGRID_PAR from MODD_SURF_ATM_GRID_n
!!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_GRID_DIM
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
 CHARACTER(LEN=10),               INTENT(OUT)   :: HGRID     ! grid type
LOGICAL,                         INTENT(OUT)   :: ORECT     ! T if rectangular grid
INTEGER,                         INTENT(OUT)   :: KDIM1     ! 1st dimension
INTEGER,                         INTENT(OUT)   :: KDIM2     ! 2nd dimension
REAL(KIND=JPRB) :: ZHOOK_HANDLE
#ifdef MNH_PARALLEL
INTEGER,                         INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
REAL,    DIMENSION(KGRID_PAR),   INTENT(IN)    :: PGRID_PAR ! grid parameters
#endif
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_SURF_GRID_DIM_N',0,ZHOOK_HANDLE)
HGRID = UG%CGRID
!
#ifdef MNH_PARALLEL
 CALL GET_GRID_DIM(UG%CGRID,KGRID_PAR,PGRID_PAR,ORECT,KDIM1,KDIM2)
#else
  CALL GET_GRID_DIM(UG%CGRID,UG%NGRID_PAR,UG%XGRID_PAR,ORECT,KDIM1,KDIM2)
#endif  
IF (LHOOK) CALL DR_HOOK('GET_SURF_GRID_DIM_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_SURF_GRID_DIM_n
