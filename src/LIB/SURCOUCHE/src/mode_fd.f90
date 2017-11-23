!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------

MODULE MODE_FD_ll
USE MODD_NETCDF, ONLY : IOCDF

IMPLICIT NONE 

PRIVATE

INTEGER, PARAMETER :: JPFINL = 32 ! File name length

TYPE FD_ll
  CHARACTER(LEN=JPFINL)    :: NAME  ! File name 
  INTEGER                  :: FLU   ! Fortran logical unit
  INTEGER                  :: COMM  ! Local MPI communicator
  CHARACTER(LEN=15)        :: MODE  ! Flag mode acces ('distrib','global','specific')   
  INTEGER                  :: OWNER ! I/O Processor number 
!JUANZ
  INTEGER                  :: NB_PROCIO = 1
!JUANZ
  TYPE(FD_ll),     POINTER :: NEXT
END TYPE FD_ll

TYPE(FD_ll), POINTER, SAVE :: TFDLIST

INTERFACE GETFD
  MODULE PROCEDURE GETFD_FILE, GETFD_COMM
END INTERFACE

PUBLIC JPFINL,FD_ll,GETFD,INITFD,NEWFD,DELFD,DISPLAY_FDLIST

CONTAINS 

SUBROUTINE INITFD()

NULLIFY(TFDLIST)
END SUBROUTINE INITFD

FUNCTION GETFD_FILE(HFILE)

TYPE(FD_ll), POINTER :: GETFD_FILE
CHARACTER(LEN=*)     :: HFILE

TYPE(FD_ll), POINTER :: TZFD

TZFD=>TFDLIST 
DO WHILE(ASSOCIATED(TZFD))
  IF (TZFD%NAME == HFILE) EXIT
  TZFD=>TZFD%NEXT
END DO

GETFD_FILE=>TZFD

END FUNCTION GETFD_FILE

FUNCTION GETFD_COMM(KCOMM)

TYPE(FD_ll), POINTER :: GETFD_COMM
INTEGER              :: KCOMM

TYPE(FD_ll), POINTER :: TZFD

TZFD=>TFDLIST 
DO WHILE(ASSOCIATED(TZFD))
  IF (TZFD%COMM == KCOMM) EXIT
  TZFD=>TZFD%NEXT
END DO

GETFD_COMM=>TZFD

END FUNCTION GETFD_COMM

FUNCTION NEWFD()

TYPE(FD_ll), POINTER :: NEWFD
TYPE(FD_ll), POINTER :: TZFD
INTEGER              :: IRESP

ALLOCATE(TZFD,STAT=IRESP)
IF (IRESP > 0) THEN 
  !       CALL ABORT()
  PRINT *, 'NEWFD : Erreur d"allocation memoire...'
END IF

!! Add TZFD to top of list TPFDLIST
TZFD%NEXT=>TFDLIST
TFDLIST=>TZFD

NEWFD=>TZFD
END FUNCTION NEWFD

SUBROUTINE DELFD(TPFD)

TYPE(FD_ll), POINTER :: TPFD

TYPE(FD_ll), POINTER :: TZPREV

!! BEWARE: TPFD must exist in TPFDLIST


IF (ASSOCIATED(TPFD,TFDLIST)) THEN
  !! TPFD is the first element of TPFDLIST
  TFDLIST=>TFDLIST%NEXT
ELSE
  TZPREV=>TFDLIST
  DO WHILE(.NOT. ASSOCIATED(TZPREV%NEXT,TPFD))
    TZPREV=>TZPREV%NEXT
  END DO
  TZPREV%NEXT=>TPFD%NEXT
END IF

!    PRINT *,'Desallocation de ', TRIM(TPFD%NAME),' realisee...'
DEALLOCATE(TPFD)

END SUBROUTINE DELFD

SUBROUTINE DISPLAY_FDLIST()

TYPE(FD_ll), POINTER :: TZFD

TZFD=>TFDLIST
DO WHILE(ASSOCIATED(TZFD))
  PRINT *, TZFD%NAME, ': ',TZFD%FLU
  TZFD=>TZFD%NEXT
END DO

END SUBROUTINE DISPLAY_FDLIST

END MODULE MODE_FD_ll
