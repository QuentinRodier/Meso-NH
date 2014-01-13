!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!##################
MODULE MODI_WRITE_SURF
!##################
!
  INTERFACE WRITE_SURF
!
     SUBROUTINE WRITE_SURFX0(HPROGRAM,HREC,PFIELD,KRESP,HCOMMENT)
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=*),  INTENT(IN) :: HREC     ! name of the article to be written
REAL,              INTENT(IN) :: PFIELD   ! real scalar to be written
INTEGER,           INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears 
 CHARACTER(LEN=100),INTENT(IN) :: HCOMMENT ! Comment string
!
END SUBROUTINE WRITE_SURFX0
!
     SUBROUTINE WRITE_SURFX1(HPROGRAM,HREC,PFIELD,KRESP,HCOMMENT,HDIR)
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be written
REAL, DIMENSION(:), INTENT(IN)  :: PFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string
 CHARACTER(LEN=1),OPTIONAL,INTENT(IN)  :: HDIR ! type of field :
!                                             ! 'H' : field with
!                                             !       horizontal spatial dim.
!                                             ! '-' : no horizontal dim.
END SUBROUTINE WRITE_SURFX1
!
     SUBROUTINE WRITE_SURFX2(HPROGRAM,HREC,PFIELD,KRESP,HCOMMENT,HDIR)
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),     INTENT(IN)  :: HREC     ! name of the article to be written
REAL, DIMENSION(:,:), INTENT(IN)  :: PFIELD   ! array containing the data field
INTEGER,              INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),   INTENT(IN)  :: HCOMMENT ! Comment string
 CHARACTER(LEN=1),OPTIONAL,INTENT(IN)  :: HDIR ! type of field :
!                                             ! 'H' : field with
!                                             !       horizontal spatial dim.
!                                             ! '-' : no horizontal dim.
END SUBROUTINE WRITE_SURFX2
!
      SUBROUTINE WRITE_SURFX2COV(HPROGRAM,HREC,PFIELD,OFLAG,KRESP,HCOMMENT,HDIR)
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),     INTENT(IN)  :: HREC     ! name of the article to be read
REAL, DIMENSION(:,:), INTENT(IN)  :: PFIELD   ! array containing the data field
LOGICAL,DIMENSION(:), INTENT(IN)  :: OFLAG  ! mask for array filling
INTEGER,              INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),   INTENT(IN)  :: HCOMMENT ! Comment string
 CHARACTER(LEN=1),OPTIONAL,INTENT(IN)  :: HDIR ! type of field :
!                                             ! 'H' : field with
!                                             !       horizontal spatial dim.
!                                             ! '-' : no horizontal dim.
END SUBROUTINE WRITE_SURFX2COV
!
     SUBROUTINE WRITE_SURFN0(HPROGRAM,HREC,KFIELD,KRESP,HCOMMENT)
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be written
INTEGER,            INTENT(IN)  :: KFIELD   ! integer to be written
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string
!
END SUBROUTINE WRITE_SURFN0
!
     SUBROUTINE WRITE_SURFN1(HPROGRAM,HREC,KFIELD,KRESP,HCOMMENT,HDIR)
 CHARACTER(LEN=6),      INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),      INTENT(IN)  :: HREC     ! name of the article to be written
INTEGER, DIMENSION(:), INTENT(IN)  :: KFIELD   ! integer to be written
INTEGER,               INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),    INTENT(IN)  :: HCOMMENT ! Comment string
 CHARACTER(LEN=1),OPTIONAL,INTENT(IN)  :: HDIR ! type of field :
!                                             ! 'H' : field with
!                                             !       horizontal spatial dim.
!                                             ! '-' : no horizontal dim.
END SUBROUTINE WRITE_SURFN1
!
     SUBROUTINE WRITE_SURFC0(HPROGRAM,HREC,HFIELD,KRESP,HCOMMENT)
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),    INTENT(IN)  :: HREC     ! name of the article to be written
 CHARACTER(LEN=*),    INTENT(IN)  :: HFIELD   ! caracter to be written
INTEGER,             INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),  INTENT(IN)  :: HCOMMENT ! Comment string
!
END SUBROUTINE WRITE_SURFC0
!
      SUBROUTINE WRITE_SURFL0(HPROGRAM,HREC,OFIELD,KRESP,HCOMMENT)
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be written
LOGICAL,            INTENT(IN)  :: OFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string
!
END SUBROUTINE WRITE_SURFL0
!
      SUBROUTINE WRITE_SURFL1(HPROGRAM,HREC,OFIELD,KRESP,HCOMMENT,HDIR)
 CHARACTER(LEN=6),      INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),      INTENT(IN)  :: HREC     ! name of the article to be written
LOGICAL, DIMENSION(:), INTENT(IN)  :: OFIELD   ! array containing the data field
INTEGER,               INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),    INTENT(IN)  :: HCOMMENT ! Comment string
 CHARACTER(LEN=1),OPTIONAL,INTENT(IN)  :: HDIR ! type of field :
!                                             ! 'H' : field with
!                                             !       horizontal spatial dim.
!                                             ! '-' : no horizontal dim.
END SUBROUTINE WRITE_SURFL1
!
      SUBROUTINE WRITE_SURFT0(HPROGRAM,HREC,TFIELD,KRESP,HCOMMENT)
!
USE MODD_TYPE_DATE_SURF
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be written
TYPE (DATE_TIME),   INTENT(IN)  :: TFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string
!
END SUBROUTINE WRITE_SURFT0
!
      SUBROUTINE WRITE_SURFT1(HPROGRAM,HREC,TFIELD,KRESP,HCOMMENT)
!
USE MODD_TYPE_DATE_SURF
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be written
TYPE (DATE_TIME), DIMENSION(:), INTENT(IN)  :: TFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string
!
END SUBROUTINE WRITE_SURFT1
!
      SUBROUTINE WRITE_SURFT2(HPROGRAM,HREC,TFIELD,KRESP,HCOMMENT)
!
USE MODD_TYPE_DATE_SURF
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be written
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(IN)  :: TFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string
!
END SUBROUTINE WRITE_SURFT2
!
END INTERFACE
!
END MODULE MODI_WRITE_SURF
!
!     #############################################################
      SUBROUTINE WRITE_SURFX0(HPROGRAM,HREC,PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITEX0* - routine to write a real scalar
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE, WLOG_MPI
!
#ifdef OL
USE MODE_WRITE_SURF_OL, ONLY: WRITE_SURF0_OL, WRITE_SURF0_TIME_OL
#endif
#ifdef LFI
USE MODE_WRITE_SURF_LFI, ONLY: WRITE_SURF0_LFI
#endif
#ifdef TXT
USE MODE_WRITE_SURF_TXT, ONLY: WRITE_SURF0_TXT
#endif
#ifdef BIN
USE MODE_WRITE_SURF_BIN, ONLY: WRITE_SURF0_BIN
#endif
#ifdef ASC
USE MODE_WRITE_SURF_ASC, ONLY: WRITE_SURF0_ASC
#endif
#ifdef FA
USE MODE_WRITE_SURF_FA, ONLY: WRITE_SURF0_FA
#endif
#ifdef MNH
USE MODI_WRITE_SURFX0_MNH
#endif
!
USE MODI_TEST_RECORD_LEN
!
IMPLICIT NONE
!
#ifndef NOMPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=*),  INTENT(IN) :: HREC     ! name of the article to be written
REAL,              INTENT(IN) :: PFIELD   ! real scalar to be written
INTEGER,           INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears 
 CHARACTER(LEN=100),INTENT(IN) :: HCOMMENT ! Comment string
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=12)  :: YREC
LOGICAL :: LNOWRITE
REAL   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFX0',0,ZHOOK_HANDLE)
!
YREC = HREC
!
 CALL TEST_RECORD_LEN(HPROGRAM,YREC,LNOWRITE)
IF(LNOWRITE .AND. LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFX0',1,ZHOOK_HANDLE)
IF(LNOWRITE)RETURN
!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL WRITE_SURFX0_MNH(YREC,PFIELD,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef ARO
  CALL WRITE_SURFX0_ARO(YREC,PFIELD,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (NRANK==NPIO) THEN
  !
#ifndef NOMPI
  XTIME0 = MPI_WTIME()
#endif  
  !
!$OMP SINGLE
  !
  IF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
    CALL WRITE_SURF0_ASC(YREC,PFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='FA    ') THEN
#ifdef FA
    CALL WRITE_SURF0_FA(YREC,PFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
    IF (YREC=='time') THEN
      CALL WRITE_SURF0_TIME_OL(PFIELD,KRESP,HCOMMENT)
    ELSE
      CALL WRITE_SURF0_OL(YREC,PFIELD,KRESP,HCOMMENT)
    ENDIF
#endif
  ENDIF
  !
  IF (HPROGRAM=='TEXTE ') THEN
#ifdef TXT
    CALL WRITE_SURF0_TXT(YREC,PFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='BINARY') THEN
#ifdef BIN
    CALL WRITE_SURF0_BIN(YREC,PFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
    CALL WRITE_SURF0_LFI(YREC,PFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
!$OMP END SINGLE
  !
#ifndef NOMPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFX0',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX0
!
!     #############################################################
      SUBROUTINE WRITE_SURFX1(HPROGRAM,HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *WRITEX1* - routine to fill a real 1D array for the externalised surface 
!
USE MODD_SURFEX_MPI, ONLY : WLOG_MPI
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef OL
USE MODE_WRITE_SURF_OL, ONLY: WRITE_SURFN_OL
#endif
#ifdef ASC
USE MODE_WRITE_SURF_ASC, ONLY: WRITE_SURFN_ASC
#endif
#ifdef TXT
USE MODE_WRITE_SURF_TXT, ONLY: WRITE_SURFN_TXT
#endif
#ifdef BIN
USE MODE_WRITE_SURF_BIN, ONLY: WRITE_SURFN_BIN
#endif
#ifdef FA
USE MODE_WRITE_SURF_FA, ONLY: WRITE_SURFN_FA
#endif
#ifdef LFI
USE MODE_WRITE_SURF_LFI, ONLY: WRITE_SURFN_LFI
#endif
#ifdef MNH
USE MODI_WRITE_SURFX1_MNH
#endif
!
USE MODI_TEST_RECORD_LEN
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be written
REAL, DIMENSION(:), INTENT(IN)  :: PFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string
 CHARACTER(LEN=1),OPTIONAL,INTENT(IN)  :: HDIR ! type of field :
!                                             ! 'H' : field with
!                                             !       horizontal spatial dim.
!                                             ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=12)  :: YREC
INTEGER            :: IL
 CHARACTER(LEN=1)   :: YDIR
LOGICAL :: LNOWRITE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFX1',0,ZHOOK_HANDLE)
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
IL = SIZE(PFIELD)
!
 CALL TEST_RECORD_LEN(HPROGRAM,YREC,LNOWRITE)
IF(LNOWRITE .AND. LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFX1',1,ZHOOK_HANDLE)
IF(LNOWRITE)RETURN
!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL WRITE_SURFX1_MNH(YREC,IL,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef ARO
  CALL WRITE_SURFX1_ARO(YREC,IL,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
  CALL WRITE_SURFN_OL(YREC,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='TEXTE ') THEN
#ifdef TXT
  CALL WRITE_SURFN_TXT(YREC,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='BINARY') THEN
#ifdef BIN
  CALL WRITE_SURFN_BIN(YREC,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
  CALL WRITE_SURFN_LFI(YREC,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
  CALL WRITE_SURFN_ASC(YREC,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='FA    ') THEN
#ifdef FA
  CALL WRITE_SURFN_FA(YREC,IL,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFX1',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX1
!
!     #############################################################
      SUBROUTINE WRITE_SURFX2(HPROGRAM,HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *WRITEX2* - routine to fill a real 2D array for the externalised surface 
!
USE MODD_SURFEX_MPI, ONLY : WLOG_MPI
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef OL
USE MODE_WRITE_SURF_OL, ONLY: WRITE_SURFN_OL
#endif
#ifdef TXT
USE MODE_WRITE_SURF_TXT, ONLY: WRITE_SURFN_TXT
#endif
#ifdef BIN
USE MODE_WRITE_SURF_BIN, ONLY: WRITE_SURFN_BIN
#endif
#ifdef LFI
USE MODE_WRITE_SURF_LFI, ONLY: WRITE_SURFN_LFI
#endif
#ifdef ASC
USE MODE_WRITE_SURF_ASC, ONLY: WRITE_SURFN_ASC
#endif
#ifdef FA
USE MODE_WRITE_SURF_FA, ONLY: WRITE_SURFN_FA
#endif
#ifdef MNH
USE MODI_WRITE_SURFX2_MNH
#endif
!
USE MODI_TEST_RECORD_LEN
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),     INTENT(IN)  :: HREC     ! name of the article to be written
REAL, DIMENSION(:,:), INTENT(IN)  :: PFIELD   ! array containing the data field
INTEGER,              INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),   INTENT(IN)  :: HCOMMENT ! Comment string
 CHARACTER(LEN=1),OPTIONAL,INTENT(IN)  :: HDIR ! type of field :
!                                             ! 'H' : field with
!                                             !       horizontal spatial dim.
!                                             ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=12)  :: YREC
INTEGER            :: IL1
INTEGER            :: IL2
 CHARACTER(LEN=1)   :: YDIR
LOGICAL :: LNOWRITE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFX2',0,ZHOOK_HANDLE)
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
IL1  = SIZE(PFIELD,1)
IL2  = SIZE(PFIELD,2)
!
 CALL TEST_RECORD_LEN(HPROGRAM,YREC,LNOWRITE)
IF(LNOWRITE .AND. LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFX2',1,ZHOOK_HANDLE)
IF(LNOWRITE)RETURN
!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL WRITE_SURFX2_MNH(YREC,IL1,IL2,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef ARO
  CALL WRITE_SURFX2_ARO(YREC,IL1,IL2,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
  CALL WRITE_SURFN_OL(YREC,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='TEXTE ') THEN
#ifdef TXT
  CALL WRITE_SURFN_TXT(YREC,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='BINARY') THEN
#ifdef BIN
  CALL WRITE_SURFN_BIN(YREC,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
  CALL WRITE_SURFN_LFI(YREC,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
  CALL WRITE_SURFN_ASC(YREC,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='FA    ') THEN
#ifdef FA
  CALL WRITE_SURFN_FA(YREC,IL1,IL2,PFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFX2',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX2
!
!     #############################################################
      SUBROUTINE WRITE_SURFX2COV(HPROGRAM,HREC,PFIELD,OFLAG,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READX2* - routine to fill a real 2D array for the externalised surface 
!
USE MODD_SURFEX_MPI, ONLY : WLOG_MPI
#ifdef OL
USE MODE_WRITE_SURF_OL, ONLY: WRITE_SURFN_OL
#endif
#ifdef TXT
USE MODE_WRITE_SURF_TXT, ONLY: WRITE_SURFN_TXT
#endif
#ifdef BIN
USE MODE_WRITE_SURF_BIN, ONLY: WRITE_SURFN_BIN
#endif
#ifdef LFI
USE MODE_WRITE_SURF_LFI, ONLY: WRITE_SURFN_LFI, WRITE_SURF0_LFI
#endif
#ifdef ASC
USE MODE_WRITE_SURF_ASC, ONLY: WRITE_SURFN_ASC
#endif
#ifdef FA
USE MODE_WRITE_SURF_FA, ONLY: WRITE_SURFN_FA
#endif
#ifdef MNH        
USE MODI_WRITE_SURFX2COV_MNH
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),     INTENT(IN)  :: HREC     ! name of the article to be read
REAL, DIMENSION(:,:), INTENT(IN)  :: PFIELD   ! array containing the data field
LOGICAL,DIMENSION(:), INTENT(IN)  :: OFLAG  ! mask for array filling
INTEGER,              INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),   INTENT(IN)  :: HCOMMENT ! Comment string
 CHARACTER(LEN=1),OPTIONAL,INTENT(IN)  :: HDIR ! type of field :
!                                             ! 'H' : field with
!                                             !       horizontal spatial dim.
!                                             ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=12)  :: YREC
 CHARACTER(LEN=100) :: YCOMMENT
INTEGER            :: IL1
INTEGER            :: IL2
 CHARACTER(LEN=1)   :: YDIR
INTEGER            :: JCOVER
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFX2L',0,ZHOOK_HANDLE)
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
IL1  = SIZE(PFIELD,1)
IL2  = SIZE(PFIELD,2)
!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH        
    CALL WRITE_SURFX2COV_MNH(YREC,IL1,IL2,PFIELD,OFLAG,KRESP,HCOMMENT,YDIR)
#endif
ELSE
  !
  IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
    YREC = 'COVER_PACKED'
    CALL WRITE_SURF0_LFI(YREC,.FALSE.,KRESP,YCOMMENT)
#endif
  END IF
  !
  DO JCOVER=1,IL2
    !
    WRITE(YREC,'(A5,I3.3)') 'COVER',JCOVER
    YCOMMENT='X_Y_'//YREC
    IF (.NOT. OFLAG(JCOVER)) CYCLE
    !
     IF (HPROGRAM=='AROME ') THEN
#ifdef ARO        
      CALL WRITE_SURFX1_ARO(YREC,IL1,PFIELD(:,JCOVER),KRESP,YCOMMENT,YDIR)
#endif  
    ENDIF
    !   
    IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
      CALL WRITE_SURFN_OL(YREC,PFIELD(:,JCOVER),KRESP,YCOMMENT,YDIR)
#endif
    ENDIF
    !
    IF (HPROGRAM=='TEXTE ') THEN
#ifdef TXT
      CALL WRITE_SURFN_TXT(YREC,PFIELD(:,JCOVER),KRESP,YCOMMENT,YDIR)
#endif
    ENDIF
    !
    IF (HPROGRAM=='BINARY') THEN
#ifdef BIN
      CALL WRITE_SURFN_TXT(YREC,PFIELD(:,JCOVER),KRESP,YCOMMENT,YDIR)
#endif
    ENDIF
    !    
    IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
      CALL WRITE_SURFN_LFI(YREC,PFIELD(:,JCOVER),KRESP,YCOMMENT,YDIR)
#endif
    ENDIF
    !    
    IF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
      CALL WRITE_SURFN_ASC(YREC,PFIELD(:,JCOVER),KRESP,YCOMMENT,YDIR)
#endif
    ENDIF
    !
    IF (HPROGRAM=='FA    ') THEN
#ifdef FA
      CALL WRITE_SURFN_FA(YREC,IL1,PFIELD(:,JCOVER),KRESP,YCOMMENT,YDIR)
#endif
    ENDIF
    !
  END DO
END IF
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFX2L',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX2COV
!
!     #############################################################
      SUBROUTINE WRITE_SURFN0(HPROGRAM,HREC,KFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITEN0* - routine to write an integer
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE, WLOG_MPI
!
#ifdef OL
USE MODE_WRITE_SURF_OL, ONLY: WRITE_SURF0_OL
#endif
#ifdef ASC
USE MODE_WRITE_SURF_ASC, ONLY: WRITE_SURF0_ASC
#endif
#ifdef TXT
USE MODE_WRITE_SURF_TXT, ONLY: WRITE_SURF0_TXT
#endif
#ifdef BIN
USE MODE_WRITE_SURF_BIN, ONLY: WRITE_SURF0_BIN
#endif
#ifdef FA
USE MODE_WRITE_SURF_FA, ONLY: WRITE_SURF0_FA
#endif
#ifdef LFI
USE MODE_WRITE_SURF_LFI, ONLY: WRITE_SURF0_LFI
#endif
#ifdef MNH
USE MODI_WRITE_SURFN0_MNH
#endif
!
USE MODI_TEST_RECORD_LEN
!
IMPLICIT NONE
!
#ifndef NOMPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be written
INTEGER,            INTENT(IN)  :: KFIELD   ! integer to be written
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=12)  :: YREC
LOGICAL :: LNOWRITE
REAL   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFN0',0,ZHOOK_HANDLE)
!
YREC = HREC
!
 CALL TEST_RECORD_LEN(HPROGRAM,YREC,LNOWRITE)
IF(LNOWRITE .AND. LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFN0',1,ZHOOK_HANDLE)
IF(LNOWRITE)RETURN

!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL WRITE_SURFN0_MNH(YREC,KFIELD,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef ARO
  CALL WRITE_SURFN0_ARO(YREC,KFIELD,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (NRANK==NPIO) THEN
  !
#ifndef NOMPI
  XTIME0 = MPI_WTIME()
#endif
  !
!$OMP SINGLE
!  
  IF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
    CALL WRITE_SURF0_ASC(YREC,KFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='FA    ') THEN
#ifdef FA
    CALL WRITE_SURF0_FA(YREC,KFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
    CALL WRITE_SURF0_OL(YREC,KFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='TEXTE ') THEN
#ifdef TXT
    CALL WRITE_SURF0_TXT(YREC,KFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='BINARY') THEN
#ifdef BIN
    CALL WRITE_SURF0_BIN(YREC,KFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
    CALL WRITE_SURF0_LFI(YREC,KFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
!$OMP END SINGLE 
  !
#ifndef NOMPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFN0',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFN0

!     #############################################################
      SUBROUTINE WRITE_SURFN1(HPROGRAM,HREC,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *WRITEN0* - routine to write an integer
!
USE MODD_SURFEX_MPI, ONLY : WLOG_MPI
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef OL
USE MODE_WRITE_SURF_OL, ONLY: WRITE_SURFN_OL
#endif
#ifdef ASC
USE MODE_WRITE_SURF_ASC, ONLY: WRITE_SURFN_ASC
#endif
#ifdef TXT
USE MODE_WRITE_SURF_TXT, ONLY: WRITE_SURFN_TXT
#endif
#ifdef BIN
USE MODE_WRITE_SURF_BIN, ONLY: WRITE_SURFN_BIN
#endif
#ifdef FA
USE MODE_WRITE_SURF_FA, ONLY: WRITE_SURFN_FA
#endif
#ifdef LFI
USE MODE_WRITE_SURF_LFI, ONLY: WRITE_SURFN_LFI
#endif
#ifdef MNH
USE MODI_WRITE_SURFN1_MNH
#endif
!
USE MODI_TEST_RECORD_LEN
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),      INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),      INTENT(IN)  :: HREC     ! name of the article to be written
INTEGER, DIMENSION(:), INTENT(IN)  :: KFIELD   ! integer to be written
INTEGER,               INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),    INTENT(IN)  :: HCOMMENT ! Comment string
 CHARACTER(LEN=1),OPTIONAL,INTENT(IN)  :: HDIR ! type of field :
!                                             ! 'H' : field with
!                                             !       horizontal spatial dim.
!                                             ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=12)  :: YREC
INTEGER            :: IL
 CHARACTER(LEN=1)   :: YDIR
LOGICAL :: LNOWRITE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFN1',0,ZHOOK_HANDLE)
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
IL = SIZE(KFIELD)
!
 CALL TEST_RECORD_LEN(HPROGRAM,YREC,LNOWRITE)
IF(LNOWRITE .AND. LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFN1',1,ZHOOK_HANDLE)
IF(LNOWRITE)RETURN
!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL WRITE_SURFN1_MNH(YREC,IL,KFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef ARO
  CALL WRITE_SURFN1_ARO(YREC,IL,KFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
  CALL WRITE_SURFN_OL(YREC,KFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='TEXTE ') THEN
#ifdef TXT
  CALL WRITE_SURFN_TXT(YREC,KFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='BINARY') THEN
#ifdef BIN
  CALL WRITE_SURFN_BIN(YREC,KFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
  CALL WRITE_SURFN_LFI(YREC,KFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
  CALL WRITE_SURFN_ASC(YREC,KFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='FA    ') THEN
#ifdef FA
  CALL WRITE_SURFN_FA(YREC,IL,KFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFN1',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFN1
!
!     #############################################################
      SUBROUTINE WRITE_SURFC0(HPROGRAM,HREC,HFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITEC0* - routine to write an integer
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE, WLOG_MPI
!
#ifdef OL
USE MODE_WRITE_SURF_OL, ONLY: WRITE_SURF0_OL
#endif
#ifdef ASC
USE MODE_WRITE_SURF_ASC, ONLY: WRITE_SURF0_ASC
#endif
#ifdef TXT
USE MODE_WRITE_SURF_TXT, ONLY: WRITE_SURF0_TXT
#endif
#ifdef BIN
USE MODE_WRITE_SURF_BIN, ONLY: WRITE_SURF0_BIN
#endif
#ifdef FA
USE MODE_WRITE_SURF_FA, ONLY: WRITE_SURF0_FA
#endif
#ifdef LFI
USE MODE_WRITE_SURF_LFI, ONLY: WRITE_SURF0_LFI
#endif
#ifdef MNH
USE MODI_WRITE_SURFC0_MNH
#endif
!
USE MODI_TEST_RECORD_LEN
!
IMPLICIT NONE
!
#ifndef NOMPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),    INTENT(IN)  :: HREC     ! name of the article to be written
 CHARACTER(LEN=*),    INTENT(IN)  :: HFIELD   ! caracter to be written
INTEGER,             INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),  INTENT(IN)  :: HCOMMENT ! Comment string
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=12)  :: YREC
 CHARACTER(LEN=40)  :: YFIELD
LOGICAL :: LNOWRITE
REAL   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFC0',0,ZHOOK_HANDLE)
!
YREC = HREC
YFIELD = "                                        "
YFIELD(1:LEN(HFIELD)) = HFIELD
!
 CALL TEST_RECORD_LEN(HPROGRAM,YREC,LNOWRITE)
IF(LNOWRITE .AND. LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFC0',1,ZHOOK_HANDLE)
IF(LNOWRITE)RETURN
!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL WRITE_SURFC0_MNH(YREC,YFIELD,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef ARO
  CALL WRITE_SURFC0_ARO(YREC,YFIELD,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (NRANK==NPIO) THEN
  !
#ifndef NOMPI
  XTIME0 = MPI_WTIME()
#endif
  !
!$OMP SINGLE  
  !
  IF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
    CALL WRITE_SURF0_ASC(YREC,YFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='FA    ') THEN
#ifdef FA
    CALL WRITE_SURF0_FA(YREC,YFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
    CALL WRITE_SURF0_OL(YREC,YFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='TEXTE ') THEN
#ifdef TXT
    CALL WRITE_SURF0_TXT(YREC,YFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='BINARY') THEN
#ifdef BIN
    CALL WRITE_SURF0_BIN(YREC,YFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
    CALL WRITE_SURF0_LFI(YREC,YFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
!$OMP END SINGLE 
  !
#ifndef NOMPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !
ENDIF
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFC0',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFC0
!
!     #############################################################
      SUBROUTINE WRITE_SURFL0(HPROGRAM,HREC,OFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITEL0* - routine to write a logical
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE, WLOG_MPI
!
#ifdef OL
USE MODE_WRITE_SURF_OL, ONLY: WRITE_SURF0_OL
#endif
#ifdef ASC
USE MODE_WRITE_SURF_ASC, ONLY: WRITE_SURF0_ASC
#endif
#ifdef TXT
USE MODE_WRITE_SURF_TXT, ONLY: WRITE_SURF0_TXT
#endif
#ifdef BIN
USE MODE_WRITE_SURF_BIN, ONLY: WRITE_SURF0_BIN
#endif
#ifdef FA
USE MODE_WRITE_SURF_FA, ONLY: WRITE_SURF0_FA
#endif
#ifdef LFI
USE MODE_WRITE_SURF_LFI, ONLY: WRITE_SURF0_LFI
#endif
#ifdef MNH
USE MODI_WRITE_SURFL0_MNH
#endif
!
USE MODI_TEST_RECORD_LEN
!
IMPLICIT NONE
!
#ifndef NOMPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be written
LOGICAL,            INTENT(IN)  :: OFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=12)  :: YREC
LOGICAL :: LNOWRITE
REAL   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFL0',0,ZHOOK_HANDLE)
!
YREC = HREC
!
 CALL TEST_RECORD_LEN(HPROGRAM,YREC,LNOWRITE)
IF(LNOWRITE .AND. LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFL0',1,ZHOOK_HANDLE)
IF(LNOWRITE)RETURN
!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL WRITE_SURFL0_MNH(YREC,OFIELD,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef ARO
  CALL WRITE_SURFL0_ARO(YREC,OFIELD,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (NRANK==NPIO) THEN
  !
#ifndef NOMPI
  XTIME0 = MPI_WTIME() 
#endif 
  !
!$OMP SINGLE
  !  
  IF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
    CALL WRITE_SURF0_ASC(YREC,OFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='FA    ') THEN
#ifdef FA
    CALL WRITE_SURF0_FA(YREC,OFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
    CALL WRITE_SURF0_OL(YREC,OFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='TEXTE ') THEN
#ifdef TXT
    CALL WRITE_SURF0_TXT(YREC,OFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='BINARY') THEN
#ifdef BIN
    CALL WRITE_SURF0_BIN(YREC,OFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
    CALL WRITE_SURF0_LFI(YREC,OFIELD,KRESP,HCOMMENT)
#endif
  ENDIF
  !
!$OMP END SINGLE 
  !
#ifndef NOMPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFL0',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFL0
!
!     #############################################################
      SUBROUTINE WRITE_SURFL1(HPROGRAM,HREC,OFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *WRITEL1* - routine to write a logical array
!
USE MODD_SURFEX_MPI, ONLY : WLOG_MPI
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef OL
USE MODE_WRITE_SURF_OL, ONLY: WRITE_SURFN_OL
#endif
#ifdef ASC
USE MODE_WRITE_SURF_ASC, ONLY: WRITE_SURFN_ASC
#endif
#ifdef TXT
USE MODE_WRITE_SURF_TXT, ONLY: WRITE_SURFN_TXT
#endif
#ifdef BIN
USE MODE_WRITE_SURF_BIN, ONLY: WRITE_SURFN_BIN
#endif
#ifdef FA
USE MODE_WRITE_SURF_FA, ONLY: WRITE_SURFN_FA
#endif
#ifdef LFI
USE MODE_WRITE_SURF_LFI, ONLY: WRITE_SURFN_LFI
#endif
#ifdef MNH
USE MODI_WRITE_SURFL1_MNH
#endif
!
USE MODI_TEST_RECORD_LEN
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),      INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),      INTENT(IN)  :: HREC     ! name of the article to be written
LOGICAL, DIMENSION(:), INTENT(IN)  :: OFIELD   ! array containing the data field
INTEGER,               INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),    INTENT(IN)  :: HCOMMENT ! Comment string
 CHARACTER(LEN=1),OPTIONAL,INTENT(IN)  :: HDIR ! type of field :
!                                             ! 'H' : field with
!                                             !       horizontal spatial dim.
!                                             ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=12)  :: YREC
INTEGER            :: IL
 CHARACTER(LEN=1)   :: YDIR
LOGICAL :: LNOWRITE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFL1',0,ZHOOK_HANDLE)
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
IL   = SIZE(OFIELD)
!
 CALL TEST_RECORD_LEN(HPROGRAM,YREC,LNOWRITE)
IF(LNOWRITE .AND. LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFL1',1,ZHOOK_HANDLE)
IF(LNOWRITE)RETURN
!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL WRITE_SURFL1_MNH(YREC,IL,OFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef ARO
  CALL WRITE_SURFL1_ARO(YREC,IL,OFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
  CALL WRITE_SURFN_OL(YREC,OFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='TEXTE ') THEN
#ifdef TXT
  CALL WRITE_SURFN_TXT(YREC,OFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='BINARY') THEN
#ifdef BIN
  CALL WRITE_SURFN_BIN(YREC,OFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
  CALL WRITE_SURFN_LFI(YREC,OFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
  CALL WRITE_SURFN_ASC(YREC,OFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='FA    ') THEN
#ifdef FA
  CALL WRITE_SURFN_FA(YREC,IL,OFIELD,KRESP,HCOMMENT,YDIR)
#endif
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFL1',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFL1
!
!     #############################################################
      SUBROUTINE WRITE_SURFT0(HPROGRAM,HREC,TFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITET0* - routine to write a MESO-NH date_time scalar
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE, WLOG_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef OL
USE MODE_WRITE_SURF_OL, ONLY: WRITE_SURFT_OL
#endif
#ifdef ASC
USE MODE_WRITE_SURF_ASC, ONLY: WRITE_SURFT_ASC
#endif
#ifdef TXT
USE MODE_WRITE_SURF_TXT, ONLY: WRITE_SURFT_TXT
#endif
#ifdef BIN
USE MODE_WRITE_SURF_BIN, ONLY: WRITE_SURFT_BIN
#endif
#ifdef FA
USE MODE_WRITE_SURF_FA, ONLY: WRITE_SURFT_FA
#endif
#ifdef LFI
USE MODE_WRITE_SURF_LFI, ONLY: WRITE_SURFT_LFI
#endif
#ifdef MNH
USE MODI_WRITE_SURFT0_MNH
#endif
!
USE MODI_TEST_RECORD_LEN
!
IMPLICIT NONE
!
#ifndef NOMPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be written
TYPE (DATE_TIME),   INTENT(IN)  :: TFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=12)  :: YREC
REAL    :: ZTIME
REAL   :: XTIME0
INTEGER :: IDAY
INTEGER :: IMONTH
INTEGER :: IYEAR
LOGICAL :: LNOWRITE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFT0',0,ZHOOK_HANDLE)
!
YREC = HREC
!
IYEAR  = TFIELD%TDATE%YEAR
IMONTH = TFIELD%TDATE%MONTH
IDAY   = TFIELD%TDATE%DAY
ZTIME  = TFIELD%TIME
!
 CALL TEST_RECORD_LEN(HPROGRAM,YREC,LNOWRITE)
IF(LNOWRITE .AND. LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFT0',1,ZHOOK_HANDLE)
IF(LNOWRITE)RETURN
!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL WRITE_SURFT0_MNH(YREC,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef ARO
  CALL WRITE_SURFT0_ARO(YREC,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (NRANK==NPIO) THEN
  !
#ifndef NOMPI
  XTIME0 = MPI_WTIME()
#endif
  !
!$OMP SINGLE
  !  
  IF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
    CALL WRITE_SURFT_ASC(YREC,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='FA    ') THEN
#ifdef FA
    CALL WRITE_SURFT_FA(YREC,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
    CALL WRITE_SURFT_OL(YREC,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='TEXTE ') THEN
#ifdef TXT
    CALL WRITE_SURFT_TXT(YREC,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='BINARY') THEN
#ifdef BIN
    CALL WRITE_SURFT_BIN(YREC,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif
  ENDIF
  !
  IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
    CALL WRITE_SURFT_LFI(YREC,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif
  ENDIF
  !
!$OMP END SINGLE 
  !
#ifndef NOMPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFT0',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFT0
!
!     #############################################################
      SUBROUTINE WRITE_SURFT1(HPROGRAM,HREC,TFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READT2* - routine to read a MESO-NH date_time array
!
USE MODD_SURFEX_MPI, ONLY : WLOG_MPI
USE MODD_TYPE_DATE_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef ASC
USE MODE_WRITE_SURF_ASC, ONLY: WRITE_SURFT_ASC
#endif
#ifdef LFI
USE MODE_WRITE_SURF_LFI, ONLY: WRITE_SURFT_LFI
#endif
#ifdef MNH
USE MODI_WRITE_SURFT1_MNH
#endif
!
USE MODI_ABOR1_SFX
USE MODI_TEST_RECORD_LEN
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be written
TYPE (DATE_TIME), DIMENSION(:), INTENT(IN)  :: TFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=12)  :: YREC
INTEGER :: IL1
REAL ,   DIMENSION(SIZE(TFIELD,1)) :: ZTIME
INTEGER, DIMENSION(SIZE(TFIELD,1)) :: IDAY
INTEGER, DIMENSION(SIZE(TFIELD,1)) :: IMONTH
INTEGER, DIMENSION(SIZE(TFIELD,1)) :: IYEAR
LOGICAL :: LNOWRITE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFT1',0,ZHOOK_HANDLE)
!
YREC = HREC
IL1  = SIZE(TFIELD,1)
!
IYEAR (:) = TFIELD(:)%TDATE%YEAR
IMONTH(:) = TFIELD(:)%TDATE%MONTH
IDAY  (:) = TFIELD(:)%TDATE%DAY
ZTIME (:) = TFIELD(:)%TIME
!
 CALL TEST_RECORD_LEN(HPROGRAM,YREC,LNOWRITE)
IF(LNOWRITE .AND. LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFT1',1,ZHOOK_HANDLE)
IF(LNOWRITE)RETURN
!
IF (HPROGRAM=='MESONH') THEN
   !G .TANGUY 03/2009
   !CALL ABOR1_SFX('WRITE_SURFT1: NOT AVAILABLE FOR MESONH')
#ifdef MNH
   CALL WRITE_SURFT1_MNH(YREC,IL1,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef ARO
  CALL WRITE_SURFT1_ARO(YREC,IL1,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif
ENDIF
!
!IF (HPROGRAM=='OFFLIN') THEN
!  CALL ABOR1_SFX('WRITE_SURFT1: NOT AVAILABLE FOR OFFLIN')
!ENDIF
!
!plm IF (HPROGRAM=='TEXTE ') THEN
!plm   CALL WRITE_SURFT1_TXT(YREC,IL1,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
!plm ENDIF
!
IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
  CALL WRITE_SURFT_LFI(YREC,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif        
ENDIF
!
IF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
  CALL WRITE_SURFT_ASC(YREC,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (HPROGRAM=='FA    ') THEN
  CALL ABOR1_SFX('WRITE_SURFT1: NOT AVAILABLE FOR FA')
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFT1',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFT1
!
!     #############################################################
      SUBROUTINE WRITE_SURFT2(HPROGRAM,HREC,TFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITET2* - routine to write a MESO-NH date_time array
!
USE MODD_SURFEX_MPI, ONLY : WLOG_MPI
USE MODD_TYPE_DATE_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef ASC
USE MODE_WRITE_SURF_ASC, ONLY: WRITE_SURFT_ASC
#endif
#ifdef TXT
USE MODE_WRITE_SURF_TXT, ONLY: WRITE_SURFT_TXT
#endif
#ifdef BIN
USE MODE_WRITE_SURF_BIN, ONLY: WRITE_SURFT_BIN
#endif
#ifdef FA
USE MODE_WRITE_SURF_FA, ONLY: WRITE_SURFT_FA
#endif
#ifdef LFI
USE MODE_WRITE_SURF_LFI, ONLY: WRITE_SURFT_LFI
#endif
!
USE MODI_ABOR1_SFX
USE MODI_TEST_RECORD_LEN
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be written
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(IN)  :: TFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=12)  :: YREC
INTEGER :: IL1, IL2
REAL ,   DIMENSION(SIZE(TFIELD,1),SIZE(TFIELD,2)) :: ZTIME
INTEGER, DIMENSION(SIZE(TFIELD,1),SIZE(TFIELD,2)) :: IDAY
INTEGER, DIMENSION(SIZE(TFIELD,1),SIZE(TFIELD,2)) :: IMONTH
INTEGER, DIMENSION(SIZE(TFIELD,1),SIZE(TFIELD,2)) :: IYEAR
LOGICAL :: LNOWRITE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFT2',0,ZHOOK_HANDLE)
!
YREC = HREC
IL1  = SIZE(TFIELD,1)
IL2  = SIZE(TFIELD,2)
!
IYEAR (:,:) = TFIELD(:,:)%TDATE%YEAR
IMONTH(:,:) = TFIELD(:,:)%TDATE%MONTH
IDAY  (:,:) = TFIELD(:,:)%TDATE%DAY
ZTIME (:,:) = TFIELD(:,:)%TIME
!
 CALL TEST_RECORD_LEN(HPROGRAM,YREC,LNOWRITE)
IF(LNOWRITE .AND. LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFT2',1,ZHOOK_HANDLE)
IF(LNOWRITE)RETURN
!
IF (HPROGRAM=='MESONH') THEN
  CALL ABOR1_SFX('WRITE_SURFT2: NOT AVAILABLE FOR MESONH')
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
  CALL ABOR1_SFX('WRITE_SURFT2: NOT AVAILABLE FOR AROME')
ENDIF
!
!IF (HPROGRAM=='OFFLIN') THEN
!  CALL ABOR1_SFX('WRITE_SURFT2: NOT AVAILABLE FOR OFFLIN')
!ENDIF
!
IF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
  CALL WRITE_SURFT_LFI(YREC,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif        
ENDIF
!
IF (HPROGRAM=='TEXTE ') THEN
#ifdef TXT
  CALL WRITE_SURFT_TXT(YREC,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (HPROGRAM=='BINARY') THEN
#ifdef BIN
  CALL WRITE_SURFT_BIN(YREC,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
  CALL WRITE_SURFT_ASC(YREC,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (HPROGRAM=='FA    ') THEN
#ifdef FA
  CALL WRITE_SURFT_FA(YREC,IL1,IL2,IYEAR,IMONTH,IDAY,ZTIME,KRESP,HCOMMENT)
#endif
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_SURF:WRITE_SURFT2',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFT2
