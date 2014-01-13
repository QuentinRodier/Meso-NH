!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE READ_NAM_GRID_CARTESIAN(HPROGRAM,KGRID_PAR,KL,PGRID_PAR)
!     ################################################################
!
!!****  *READ_NAM_GRID_CARTESIAN* - routine to read in namelist the horizontal grid
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_POS_SURF
!
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_GET_LUOUT
!
USE MODE_GRIDTYPE_CARTESIAN
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),           INTENT(IN)    :: HPROGRAM   ! calling program
INTEGER,                    INTENT(INOUT) :: KGRID_PAR  ! size of PGRID_PAR
INTEGER,                    INTENT(OUT)   :: KL         ! number of points
REAL, DIMENSION(KGRID_PAR), INTENT(OUT)   :: PGRID_PAR  ! parameters defining this grid
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT ! output listing logical unit
INTEGER :: ILUNAM ! namelist file  logical unit
INTEGER :: JI, JJ ! loop counters
INTEGER :: JL     ! loop counter

REAL, DIMENSION(:),   ALLOCATABLE :: ZX       ! X conformal coordinate of grid mesh
REAL, DIMENSION(:),   ALLOCATABLE :: ZY       ! Y conformal coordinate of grid mesh
REAL, DIMENSION(:),   ALLOCATABLE :: ZDX      ! X grid mesh size
REAL, DIMENSION(:),   ALLOCATABLE :: ZDY      ! Y grid mesh size
!
!*       0.3   Declarations of namelist
!              ------------------------
!
REAL    :: XLAT0    ! reference latitude
REAL    :: XLON0    ! reference longitude
INTEGER :: NIMAX    ! number of points in I direction
INTEGER :: NJMAX    ! number of points in J direction
REAL    :: XDX      ! increment in X direction (in meters)
REAL    :: XDY      ! increment in Y direction (in meters)
!
REAL, DIMENSION(:), POINTER :: ZGRID_PAR
!
LOGICAL :: GFOUND
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_CARTESIAN/XLAT0, XLON0, NIMAX, NJMAX, XDX, XDY
!
!------------------------------------------------------------------------------
!
!*       1.    opening of namelist
! 
IF (LHOOK) CALL DR_HOOK('READ_NAM_GRID_CARTESIAN',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
!---------------------------------------------------------------------------
!
!*       2.    Reading of projection parameters
!              --------------------------------
!
 CALL POSNAM(ILUNAM,'NAM_CARTESIAN',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_CARTESIAN)
!
!---------------------------------------------------------------------------
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!---------------------------------------------------------------------------
!
!*       3.    Number of points
!              ----------------
!
KL = NIMAX * NJMAX
!
!---------------------------------------------------------------------------
!
!*       3.    Array of X and Y coordinates
!              ----------------------------
!
!
ALLOCATE(ZX(KL))
ALLOCATE(ZY(KL))
DO JJ=1,NJMAX
  DO JI=1,NIMAX
    JL = JI + (JJ-1) * NIMAX
    ZX(JL) = FLOAT(JI) * XDX
    ZY(JL) = FLOAT(JJ) * XDY
  END DO
END DO
!
!---------------------------------------------------------------------------
!
!*       4.    Array of X and Y increments
!              ---------------------------
!
ALLOCATE(ZDX(KL))
ALLOCATE(ZDY(KL))
ZDX(:) = XDX
ZDY(:) = XDY
!
!---------------------------------------------------------------------------
!
!*       8.    All this information stored into pointer PGRID_PAR
!              --------------------------------------------------
!
 CALL PUT_GRIDTYPE_CARTESIAN(ZGRID_PAR,XLAT0,XLON0,               &
                              NIMAX,NJMAX,                         &
                              ZX,ZY,ZDX,ZDY                        )  
!
!---------------------------------------------------------------------------
DEALLOCATE(ZX)
DEALLOCATE(ZY)
DEALLOCATE(ZDX)
DEALLOCATE(ZDY)
!---------------------------------------------------------------------------
!
!* 1st call : initializes dimension
!
IF (KGRID_PAR==0) THEN
  KGRID_PAR = SIZE(ZGRID_PAR)
!
ELSE
!
!* 2nd call : initializes grid array
!
  PGRID_PAR(:) = 0.
  PGRID_PAR(:) = ZGRID_PAR
END IF
!
DEALLOCATE(ZGRID_PAR)
IF (LHOOK) CALL DR_HOOK('READ_NAM_GRID_CARTESIAN',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_GRID_CARTESIAN
