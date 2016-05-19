!     #########
      SUBROUTINE DEFAULT_GRID(HPROGRAM,HGRID)
!     ######################################
!!
!!    PURPOSE
!!    -------
!!   set default for the surface GRID.
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
!!    Original     13/10/03
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef MNH
USE MODI_DEFAULT_GRID_MNH
#endif
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling READ_PGD
 CHARACTER(LEN=10), INTENT(OUT) :: HGRID    ! type of grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_GRID',0,ZHOOK_HANDLE)
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL DEFAULT_GRID_MNH(HGRID)
#endif
ELSE
  HGRID = 'NONE      '
END IF
IF (LHOOK) CALL DR_HOOK('DEFAULT_GRID',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_GRID
