!     ###########################################################
      SUBROUTINE SPLIT_GRID_CONF_PROJ(HPROGRAM,KDIM_FULL,KSIZE_FULL,KGRID_PAR,PGRID_PAR)
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
REAL, DIMENSION(KDIM_FULL)      :: ZX, ZY, ZDX, ZDY
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
                            ZLATOR,ZLONOR,IIMAX,IJMAX,       &
                            ZX,ZY,ZDX,ZDY                    )
!
!
!*    2.      Splits the (pertinent) parameters of the grid
!
 CALL SPLIT_GRID_PARAMETERN0(HPROGRAM,'CONF PROJ ','IMAX  ',IIMAX,IIMAX_SPLIT)
 CALL SPLIT_GRID_PARAMETERN0(HPROGRAM,'CONF PROJ ','JMAX  ',IJMAX,IJMAX_SPLIT)
!
KSIZE_FULL = IIMAX_SPLIT * IJMAX_SPLIT
!
ALLOCATE(ZX_SPLIT (KSIZE_FULL))
ALLOCATE(ZY_SPLIT (KSIZE_FULL))
ALLOCATE(ZDX_SPLIT(KSIZE_FULL))
ALLOCATE(ZDY_SPLIT(KSIZE_FULL))
 CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','XX    ',KDIM_FULL,KSIZE_FULL,ZX,ZX_SPLIT)
 CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','YY    ',KDIM_FULL,KSIZE_FULL,ZY,ZY_SPLIT)
 CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','DX    ',KDIM_FULL,KSIZE_FULL,ZDX,ZDX_SPLIT)
 CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','DY    ',KDIM_FULL,KSIZE_FULL,ZDY,ZDY_SPLIT)
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
