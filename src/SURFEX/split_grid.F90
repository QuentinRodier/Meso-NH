!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ###########################################################
#ifdef MNH_PARALLEL
      SUBROUTINE SPLIT_GRID(HPROGRAM,KGRID_PAR,PGRID_PAR,KHALO)
#else
      SUBROUTINE SPLIT_GRID(HPROGRAM)
#endif
!     ###########################################################
!!
!!    PURPOSE
!!    -------
!!   This program splits a PGD grid on several processors (according to host program)
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
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     08/11
!!    Modification 01/03/2015 pass KGRID_PAR,PGRID_PAR,KHALO as arguments (M.Moge)
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
#ifdef MNH_PARALLEL
USE MODD_SURF_ATM_GRID_n, ONLY : CGRID
#else
USE MODD_SURF_ATM_GRID_n, ONLY : CGRID, XGRID_PAR, NGRID_PAR
#endif
USE MODD_SURF_ATM_n,      ONLY : NDIM_FULL, NSIZE_FULL
!
USE MODI_SPLIT_GRID_CONF_PROJ
USE MODI_SPLIT_GRID_CARTESIAN
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
#ifdef MNH_PARALLEL
INTEGER,            INTENT(INOUT)  :: KGRID_PAR ! size of PGRID_PAR pointer
REAL, DIMENSION(:), POINTER, INTENT(INOUT) :: PGRID_PAR ! parameters defining this grid
INTEGER,            INTENT(IN), OPTIONAL   :: KHALO ! size of the Halo
#endif
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
#ifdef MNH_PARALLEL
IF (PRESENT(KHALO)) THEN
  IHALO = KHALO
ELSE
  IHALO = 0
ENDIF
#else
IHALO = 0
#endif
!
SELECT CASE(CGRID)

#ifdef MNH_PARALLEL
  CASE('CONF PROJ ')
    CALL SPLIT_GRID_CONF_PROJ(HPROGRAM,NDIM_FULL,NSIZE_FULL,KGRID_PAR,PGRID_PAR,IHALO)
  CASE('CARTESIAN ')
    CALL SPLIT_GRID_CARTESIAN(HPROGRAM,NDIM_FULL,NSIZE_FULL,KGRID_PAR,PGRID_PAR,IHALO)
  CASE DEFAULT
    CALL GET_SIZE_FULL_n(HPROGRAM,NDIM_FULL,NSIZE_FULL)
#else
  CASE('CONF PROJ ')
    CALL SPLIT_GRID_CONF_PROJ(HPROGRAM,NDIM_FULL,NSIZE_FULL,NGRID_PAR,XGRID_PAR)
  CASE('CARTESIAN ')
    CALL SPLIT_GRID_CARTESIAN(HPROGRAM,NDIM_FULL,NSIZE_FULL,NGRID_PAR,XGRID_PAR)
  CASE DEFAULT
    CALL GET_SIZE_FULL_n(HPROGRAM,NDIM_FULL,NSIZE_FULL)
#endif

END SELECT
!

IF (LHOOK) CALL DR_HOOK('SPLIT_GRID',1,ZHOOK_HANDLE)
!_______________________________________________________________________________
!
END SUBROUTINE SPLIT_GRID
