!     #########################
      SUBROUTINE AVERAGE2_LDB(PPGDARRAY,HTYPE,KSTAT)
!     #########################
!
!!**** *AVERAGE2_LDB* 
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
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
!!    S. Faroux         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    17/02/11
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGDWORK,   ONLY : NSIZE, XTNG
USE MODD_DATA_LAKE, ONLY : XBOUNDGRADDEPTH_LDB, XBOUNDGRADSTATUS_LDB, &
                           XCENTRGRADDEPTH_LDB, NCENTRGRADSTATUS_LDB, &
                           XSMALL_DUMMY
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
REAL,    DIMENSION(:), INTENT(OUT) :: PPGDARRAY
 CHARACTER(LEN=1), INTENT(IN) :: HTYPE
INTEGER, INTENT(IN)          :: KSTAT
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL, DIMENSION(:), ALLOCATABLE :: ZBOUND, ZCENTR
REAL :: ZFRAC, ZMAX, ZPDF, ZAVE
!
INTEGER :: IGRAD_MODE
INTEGER :: JGRAD, JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*    1.     Average values
!            --------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE2_LDB',0,ZHOOK_HANDLE)
!
SELECT CASE (HTYPE)
!
  CASE('D')
    ALLOCATE(ZBOUND(SIZE(XBOUNDGRADDEPTH_LDB)))
    ZBOUND(:) = XBOUNDGRADDEPTH_LDB(:)
    ALLOCATE(ZCENTR(SIZE(XCENTRGRADDEPTH_LDB)))
    ZCENTR(:) = XCENTRGRADDEPTH_LDB(:)    
!
  CASE('S')
    ALLOCATE(ZBOUND(SIZE(XBOUNDGRADSTATUS_LDB)))
    ZBOUND(:) = XBOUNDGRADSTATUS_LDB(:)
    ALLOCATE(ZCENTR(SIZE(NCENTRGRADSTATUS_LDB)))
    ZCENTR(:) = NCENTRGRADSTATUS_LDB(:)
!
  CASE DEFAULT
    CALL ABOR1_SFX("AVERAGE1_LDB: HTYPE NOT SUPPORTED")
!
END SELECT
!
!
DO JI = 1,SIZE(XTNG,1)
  !
  DO JGRAD = 1,SIZE(XTNG,2)
    IF (NSIZE(JI).NE.0) XTNG(JI,JGRAD) = XTNG(JI,JGRAD)/NSIZE(JI)
  ENDDO
  !
  !2 because first centre is for values lower than 0
  ZFRAC = SUM(XTNG(JI,2:SIZE(XTNG,2)))
  !
  ZMAX = XSMALL_DUMMY
  IGRAD_MODE = 2
  !
  IF (KSTAT.EQ.1) THEN
    !
    DO JGRAD = 2, SIZE(XTNG,2)
      ZPDF = XTNG(JI,JGRAD) / (ZBOUND(JGRAD)-ZBOUND(JGRAD-1))
      IF (ZPDF.GT.ZMAX) THEN
        ZMAX = ZPDF
        IGRAD_MODE = JGRAD
      ENDIF
    ENDDO
    !
    IF (ZFRAC.GT.0.) THEN
      PPGDARRAY(JI) = ZCENTR(IGRAD_MODE)
    ELSE
      PPGDARRAY(JI) = 0.
    ENDIF
    !
  ELSEIF (KSTAT.EQ.2) THEN
    !
    ZAVE = 0.
    DO JGRAD = 2, SIZE(XTNG,2)
      ZAVE = ZAVE + ZCENTR(JGRAD) * XTNG(JI,JGRAD)
    ENDDO
    !
    IF (ZFRAC.LT.0.00001) THEN
      PPGDARRAY(JI) = 0.
    ELSE
      PPGDARRAY(JI) = ZAVE / ZFRAC
    ENDIF 
    !
  ENDIF
  !
ENDDO
!
DEALLOCATE(ZBOUND)
DEALLOCATE(ZCENTR)
!
IF (LHOOK) CALL DR_HOOK('AVERAGE2_LDB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE2_LDB
