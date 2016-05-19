!     #########
      SUBROUTINE PREP_OUTPUT_GRID(KLUOUT,HGRID,PGRID_PAR,PLAT,PLON)
!     #######################################
!!
!!    PURPOSE
!!    -------
!!    Computes variables used for interpolation
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
!!    Original     01/2004
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_GET_GRID_COORD
!
USE MODD_PREP, ONLY : XLAT_OUT, XLON_OUT, XX_OUT, XY_OUT, LINTERP
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
INTEGER,           INTENT(IN)  :: KLUOUT     ! output listing logical unit
 CHARACTER(LEN=10), INTENT(IN)  :: HGRID      ! grid type
REAL, DIMENSION(:), POINTER    :: PGRID_PAR  ! parameters defining this grid
REAL, DIMENSION(:),INTENT(IN)  :: PLAT       ! latitudes
REAL, DIMENSION(:),INTENT(IN)  :: PLON       ! longitudes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_OUTPUT_GRID',0,ZHOOK_HANDLE)
IF (.NOT.ALLOCATED(XLAT_OUT)) ALLOCATE(XLAT_OUT(SIZE(PLAT)))
IF (.NOT.ALLOCATED(XLON_OUT)) ALLOCATE(XLON_OUT(SIZE(PLAT)))
IF (.NOT.ALLOCATED(XX_OUT)) ALLOCATE(XX_OUT  (SIZE(PLAT)))
IF (.NOT.ALLOCATED(XY_OUT)) ALLOCATE(XY_OUT  (SIZE(PLAT)))
!
IF (.NOT.ALLOCATED(LINTERP)) ALLOCATE(LINTERP (SIZE(PLAT)))

XLAT_OUT = PLAT
XLON_OUT = PLON
LINTERP  = .TRUE.
!
 CALL GET_GRID_COORD(KLUOUT,XX_OUT,XY_OUT,SIZE(PLAT),HGRID,PGRID_PAR)
IF (LHOOK) CALL DR_HOOK('PREP_OUTPUT_GRID',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PREP_OUTPUT_GRID
