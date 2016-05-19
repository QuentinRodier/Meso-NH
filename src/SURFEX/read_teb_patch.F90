!     #######################
      SUBROUTINE READ_TEB_PATCH(HPROGRAM,KTEB_PATCH)
!     #######################
!
USE MODI_READ_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
 CHARACTER(LEN=6),     INTENT(IN)    :: HPROGRAM  ! program calling surf. schemes
INTEGER,              INTENT(OUT)   :: KTEB_PATCH! number of TEB patches
!
!
!* local variables
!  ---------------
!
 CHARACTER(LEN=12) :: YRECFM     ! Name of the article to be read
INTEGER           :: IRESP      ! reading return code
!
INTEGER           :: IVERSION   ! surface version
INTEGER           :: IBUGFIX    ! surface bugfix
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_TEB_PATCH',0,ZHOOK_HANDLE)
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
YRECFM='BUG'
 CALL READ_SURF(HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
IF (IVERSION<7 .OR. (IVERSION==7 .AND. IBUGFIX<=2)) THEN
  KTEB_PATCH = 1
ELSE
  YRECFM='TEB_PATCH'
  CALL READ_SURF(HPROGRAM,YRECFM,KTEB_PATCH,IRESP)
END IF
IF (LHOOK) CALL DR_HOOK('READ_TEB_PATCH',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_TEB_PATCH
