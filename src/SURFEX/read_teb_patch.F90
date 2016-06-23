!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################
      SUBROUTINE READ_TEB_PATCH (&
                                 HFILEPGD,HFILEPGDTYPE,KTEB_PATCH)
!     #######################
!
!
!
!
USE MODI_READ_SURF
USE MODI_TOWN_PRESENCE
!
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
!
!
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of file
INTEGER,            INTENT(OUT) :: KTEB_PATCH! number of TEB patches
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
LOGICAL           :: GTOWN
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_TEB_PATCH',0,ZHOOK_HANDLE)
!
 CALL OPEN_AUX_IO_SURF(&
                      HFILEPGD,HFILEPGDTYPE,'FULL  ')
YRECFM='VERSION'
 CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,IVERSION,IRESP)
YRECFM='BUG'
 CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,IBUGFIX,IRESP)
!
 CALL TOWN_PRESENCE(&
                   HFILEPGDTYPE,GTOWN)
 CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
IF (IVERSION<7 .OR. (IVERSION==7 .AND. IBUGFIX<=2).OR..NOT.GTOWN) THEN
  KTEB_PATCH = 1
ELSE
  YRECFM='TEB_PATCH'
  CALL OPEN_AUX_IO_SURF(&
                      HFILEPGD,HFILEPGDTYPE,'TOWN  ')
  CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,KTEB_PATCH,IRESP)
  CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
END IF
!
IF (LHOOK) CALL DR_HOOK('READ_TEB_PATCH',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_TEB_PATCH
