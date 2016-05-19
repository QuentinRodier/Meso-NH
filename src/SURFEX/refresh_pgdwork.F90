!     ##########################
      SUBROUTINE REFRESH_PGDWORK
!     ##########################
!
!!**** *REFRESH_PGDWORK* ! refreshes arrays used in PGD work module
!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2008
!!
!
USE MODD_PGDWORK,  ONLY : XSSQO, LSSQO, XSUMVAL, XSUMVAL2, XSUMCOVER, NSIZE
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!----------------------------------------------------------------------------
!
!*    1.     Cover array
!            -----------
!

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('REFRESH_PGDWORK',0,ZHOOK_HANDLE)
IF (ALLOCATED(XSUMCOVER)) THEN
  XSUMCOVER=0.
END IF
!----------------------------------------------------------------------------
!
!*    2.     General arrays
!            --------------
!
IF (ALLOCATED(XSUMVAL)) THEN
  XSUMVAL=0.
END IF
IF (ALLOCATED(XSUMVAL2)) THEN
  XSUMVAL2=0.
END IF
IF (ALLOCATED(NSIZE)) THEN
  NSIZE=0
END IF
!----------------------------------------------------------------------------
!
!*    3.     Subgrid arrays
!            --------------
!
IF (ALLOCATED(LSSQO)) THEN
  XSSQO(:,:,:) = -99999.
  LSSQO(:,:,:) = .FALSE.
END IF
IF (LHOOK) CALL DR_HOOK('REFRESH_PGDWORK',1,ZHOOK_HANDLE)
!
!----------------------------------------------------------------------------
!
END SUBROUTINE REFRESH_PGDWORK
