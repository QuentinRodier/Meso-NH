!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!!     ###########################################################
      SUBROUTINE EXTEND_GRID_ON_HALO(HPROGRAM,KGRID_PAR,PGRID_PAR)
!     ###########################################################
!!
!!    PURPOSE
!!    -------
!!   This program extends a splitted PGD grid on the SURFEX halo
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    M.Moge                   CNRS - LA
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     01/03/2015
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_ATM_GRID_n, ONLY : CGRID!, XGRID_PAR, NGRID_PAR
USE MODD_SURF_ATM_n,      ONLY : NDIM_FULL, NSIZE_FULL
!
USE MODI_EXTEND_GRID_ON_HALO_CONF_PROJ
USE MODI_EXTEND_GRID_ON_HALO_CARTESIAN
USE MODI_GET_SIZE_FULL_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM ! program calling
INTEGER,            INTENT(INOUT)  :: KGRID_PAR ! size of PGRID_PAR pointer
REAL, DIMENSION(:), POINTER, INTENT(INOUT) :: PGRID_PAR ! parameters defining this grid
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
 CHARACTER(LEN=100) :: YCOMMENT
INTEGER :: IRESP ! error return code
INTEGER :: IHALO
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SPLIT_GRID',0,ZHOOK_HANDLE)
!
SELECT CASE(CGRID)

  CASE('CONF PROJ ')
    CALL EXTEND_GRID_ON_HALO_CONF_PROJ(HPROGRAM,NDIM_FULL,NSIZE_FULL,KGRID_PAR,PGRID_PAR)
  CASE('CARTESIAN ')
    CALL EXTEND_GRID_ON_HALO_CARTESIAN(HPROGRAM,NDIM_FULL,NSIZE_FULL,KGRID_PAR,PGRID_PAR)
  CASE DEFAULT
    CALL GET_SIZE_FULL_n(HPROGRAM,NDIM_FULL,NSIZE_FULL)

END SELECT
!

IF (LHOOK) CALL DR_HOOK('SPLIT_GRID',1,ZHOOK_HANDLE)
!_______________________________________________________________________________
!
END SUBROUTINE EXTEND_GRID_ON_HALO
