!     #########
      SUBROUTINE READ_NAM_WRITE_COVER_TEX(HPROGRAM)
!     ##############################################################
!
USE MODN_WRITE_COVER_TEX
!
USE MODE_POS_SURF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*****************************************************************************
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_WRITE_COVER_TEX',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
 CALL POSNAM(ILUNAM,'NAM_WRITE_COVER_TEX',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_WRITE_COVER_TEX)
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
IF (LHOOK) CALL DR_HOOK('READ_NAM_WRITE_COVER_TEX',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_NAM_WRITE_COVER_TEX
