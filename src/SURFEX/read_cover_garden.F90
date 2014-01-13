!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ######################################################################
      SUBROUTINE READ_COVER_GARDEN(HPROGRAM,OGARDEN)
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
LOGICAL,           INTENT(OUT) :: OGARDEN   ! T: Definition of urban green areas
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
IF (LHOOK) CALL DR_HOOK('READ_COVER_GARDEN',0,ZHOOK_HANDLE)
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
IF (IVERSION<=5) THEN
  OGARDEN = .FALSE.
ELSE
  YRECFM='GARDEN'
  CALL READ_SURF(HPROGRAM,YRECFM,OGARDEN,IRESP)
END IF
IF (LHOOK) CALL DR_HOOK('READ_COVER_GARDEN',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_COVER_GARDEN
