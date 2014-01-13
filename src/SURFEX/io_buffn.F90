!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #######################################################
      SUBROUTINE IO_BUFF_n(HREC,HACTION,OKNOWN)
!     #######################################################
!
!!****  *IO_BUFF_n* - function to check if the field has already been read/written
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/2007 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_IO_BUFF_n, ONLY : CREC, NREC
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=12),  INTENT(IN) :: HREC     ! field to read or write
 CHARACTER(LEN=1),   INTENT(IN) :: HACTION  ! 'R' : file being read
                                           ! 'W' : file being written
!
LOGICAL,            INTENT(OUT):: OKNOWN   ! T : field has already been read/written
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JLOOP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('IO_BUFF_N',0,ZHOOK_HANDLE)
IF (HACTION=='R' .OR. HACTION=='W') THEN
  OKNOWN=.FALSE.
  DO JLOOP=1,NREC
    OKNOWN=(HREC==CREC(JLOOP))
    IF (OKNOWN .AND. LHOOK) CALL DR_HOOK('IO_BUFF_N',1,ZHOOK_HANDLE)
    IF (OKNOWN) RETURN
  END DO
  NREC=NREC+1
  CREC(NREC)=HREC
END IF
IF (LHOOK) CALL DR_HOOK('IO_BUFF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE IO_BUFF_n
