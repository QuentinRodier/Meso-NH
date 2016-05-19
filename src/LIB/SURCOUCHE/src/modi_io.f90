!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------

MODULE MODI_IO_ll
!
INTERFACE 
  SUBROUTINE INITIO_ll()
  END SUBROUTINE INITIO_ll

  SUBROUTINE OPEN_ll(UNIT,FILE,MODE,LFIPAR,COMM,STATUS,ACCESS,  &
       IOSTAT,FORM,RECL,BLANK,POSITION,ACTION,DELIM,PAD)
  USE MODD_IO_ll, ONLY : LFIPARAM
  INTEGER,         INTENT(OUT)           :: UNIT  !! Different from
                                                  !! fortran OPEN
  CHARACTER(len=*),INTENT(IN),  OPTIONAL :: FILE
  CHARACTER(len=*),INTENT(IN),  OPTIONAL :: MODE
  TYPE(LFIPARAM),  POINTER,     OPTIONAL :: LFIPAR
  CHARACTER(len=*),INTENT(IN),  OPTIONAL :: STATUS
  CHARACTER(len=*),INTENT(IN),  OPTIONAL :: ACCESS
  INTEGER,         INTENT(OUT)           :: IOSTAT
  CHARACTER(len=*),INTENT(IN),  OPTIONAL :: FORM
  INTEGER,         INTENT(IN),  OPTIONAL :: RECL
  CHARACTER(len=*),INTENT(IN),  OPTIONAL :: BLANK
  CHARACTER(len=*),INTENT(IN),  OPTIONAL :: POSITION
  CHARACTER(len=*),INTENT(IN)            :: ACTION
  CHARACTER(len=*),INTENT(IN),  OPTIONAL :: DELIM
  CHARACTER(len=*),INTENT(IN),  OPTIONAL :: PAD
  INTEGER,         INTENT(IN),  OPTIONAL :: COMM
  END SUBROUTINE OPEN_ll
  
  SUBROUTINE CLOSE_ll(HFILE,IOSTAT,STATUS)
  CHARACTER(LEN=*), INTENT(IN)            :: HFILE
  INTEGER,          INTENT(OUT), OPTIONAL :: IOSTAT
  CHARACTER(LEN=*), INTENT(IN),  OPTIONAL :: STATUS
  END SUBROUTINE CLOSE_ll

  SUBROUTINE FLUSH_ll(HFILE,IRESP)
  CHARACTER(LEN=*), INTENT(IN)            :: HFILE
  INTEGER,          INTENT(OUT), OPTIONAL :: IRESP
  END SUBROUTINE FLUSH_ll

END INTERFACE
!
END MODULE MODI_IO_ll
