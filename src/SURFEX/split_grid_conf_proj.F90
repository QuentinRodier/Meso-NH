!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ###########################################################
#ifdef MNH_PARALLEL
      SUBROUTINE SPLIT_GRID_CONF_PROJ(HPROGRAM,KDIM_FULL,KSIZE_FULL,KGRID_PAR,PGRID_PAR,KHALO)
#else
      SUBROUTINE SPLIT_GRID_CONF_PROJ(HPROGRAM,KDIM_FULL,KSIZE_FULL,KGRID_PAR,PGRID_PAR)
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
!!      M.Moge     02/15  using PGRID_PAR(11) instead of KDIM_FULL
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODE_GRIDTYPE_CONF_PROJ
USE MODE_SPLIT_GRID_PARAMETER
!
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
 CHARACTER(LEN=6),   INTENT(IN)    :: HPROGRAM  ! host program 
INTEGER,            INTENT(IN)    :: KDIM_FULL ! total number of points
INTEGER,            INTENT(OUT)   :: KSIZE_FULL! number of points on this processor
INTEGER,            INTENT(INOUT) :: KGRID_PAR ! size of PGRID_PAR pointer
REAL, DIMENSION(:), POINTER       :: PGRID_PAR ! parameters defining this grid
#ifdef MNH_PARALLEL
INTEGER,            INTENT(IN)    :: KHALO ! size of the Halo
#endif
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!* original grid
REAL                            :: ZLAT0, ZLON0, ZRPK, ZBETA, ZLATOR, ZLONOR
INTEGER                         :: IIMAX, IJMAX
REAL, DIMENSION(PGRID_PAR(11))      :: ZX, ZY, ZDX, ZDY
!
!* splitted grid on processor
INTEGER                         :: IIMAX_SPLIT, IJMAX_SPLIT
REAL, DIMENSION(:), ALLOCATABLE :: ZX_SPLIT, ZY_SPLIT, ZDX_SPLIT, ZDY_SPLIT
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SPLIT_GRID_CONF_PROJ',0,ZHOOK_HANDLE)
!
!*    1.      Gets Parameters of the Grid
!
 CALL GET_GRIDTYPE_CONF_PROJ(PGRID_PAR,ZLAT0,ZLON0,ZRPK,ZBETA,&
                            ZLATOR,ZLONOR,IIMAX,IJMAX,        &
                            ZX,ZY,ZDX,ZDY                     )
!
!
!*    2.      Splits the (pertinent) parameters of the grid
!
#ifdef MNH_PARALLEL
 CALL SPLIT_GRID_PARAMETERN0(HPROGRAM,'CONF PROJ ','IMAX  ',KHALO,IIMAX,IIMAX_SPLIT)
 CALL SPLIT_GRID_PARAMETERN0(HPROGRAM,'CONF PROJ ','JMAX  ',KHALO,IJMAX,IJMAX_SPLIT)
#else
 CALL SPLIT_GRID_PARAMETERN0(HPROGRAM,'CONF PROJ ','IMAX  ',IIMAX,IIMAX_SPLIT)
 CALL SPLIT_GRID_PARAMETERN0(HPROGRAM,'CONF PROJ ','JMAX  ',IJMAX,IJMAX_SPLIT)
#endif
!
KSIZE_FULL = IIMAX_SPLIT * IJMAX_SPLIT
!
ALLOCATE(ZX_SPLIT (KSIZE_FULL))
ALLOCATE(ZY_SPLIT (KSIZE_FULL))
ALLOCATE(ZDX_SPLIT(KSIZE_FULL))
ALLOCATE(ZDY_SPLIT(KSIZE_FULL))
#ifdef MNH_PARALLEL
 CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','XX    ',SIZE(ZX),KSIZE_FULL,IIMAX,IJMAX,KHALO,ZX,ZX_SPLIT)
 CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','YY    ',SIZE(ZY),KSIZE_FULL,IIMAX,IJMAX,KHALO,ZY,ZY_SPLIT)
 CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','DX    ',SIZE(ZDX),KSIZE_FULL,IIMAX,IJMAX,KHALO,ZDX,ZDX_SPLIT)
 CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','DY    ',SIZE(ZDY),KSIZE_FULL,IIMAX,IJMAX,KHALO,ZDY,ZDY_SPLIT)
#else
 CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','XX    ',KDIM_FULL,KSIZE_FULL,ZX,ZX_SPLIT)
 CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','YY    ',KDIM_FULL,KSIZE_FULL,ZY,ZY_SPLIT)
 CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','DX    ',KDIM_FULL,KSIZE_FULL,ZDX,ZDX_SPLIT)
 CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','DY    ',KDIM_FULL,KSIZE_FULL,ZDY,ZDY_SPLIT)
#endif
!
!
!*    3.      Stores Parameters of the Grid in grid pointer
!
NULLIFY(PGRID_PAR)
 CALL PUT_GRIDTYPE_CONF_PROJ(PGRID_PAR,ZLAT0,ZLON0,ZRPK,ZBETA,       &
                            ZLATOR,ZLONOR,IIMAX_SPLIT,IJMAX_SPLIT,  &
                            ZX_SPLIT,ZY_SPLIT,ZDX_SPLIT,ZDY_SPLIT   )
                            !
!
KGRID_PAR = SIZE(PGRID_PAR)
!
DEALLOCATE(ZX_SPLIT )
DEALLOCATE(ZY_SPLIT )
DEALLOCATE(ZDX_SPLIT)
DEALLOCATE(ZDY_SPLIT)
!
IF (LHOOK) CALL DR_HOOK('SPLIT_GRID_CONF_PROJ',1,ZHOOK_HANDLE)
!_______________________________________________________________________________
!
END SUBROUTINE SPLIT_GRID_CONF_PROJ
