!     ######################################################################
      SUBROUTINE READ_ARRANGE_COVER(HPROGRAM,OWATER_TO_NATURE,OTOWN_TO_ROCK)
!     ######################################################################
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
LOGICAL,           INTENT(OUT) :: OWATER_TO_NATURE ! T: Change Wetland treated as inland water into nature
LOGICAL,           INTENT(OUT) :: OTOWN_TO_ROCK    ! T: Change Town into Rock 
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
IF (LHOOK) CALL DR_HOOK('READ_ARRANGE_COVER',0,ZHOOK_HANDLE)
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
IF (IVERSION<5) THEN
  OWATER_TO_NATURE = .FALSE.
  OTOWN_TO_ROCK    = .FALSE.
ELSE
  YRECFM='WATER_TO_NAT'
  CALL READ_SURF(HPROGRAM,YRECFM,OWATER_TO_NATURE,IRESP)
  YRECFM='TOWN_TO_ROCK'
  CALL READ_SURF(HPROGRAM,YRECFM,OTOWN_TO_ROCK,IRESP)
END IF
IF (LHOOK) CALL DR_HOOK('READ_ARRANGE_COVER',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_ARRANGE_COVER
