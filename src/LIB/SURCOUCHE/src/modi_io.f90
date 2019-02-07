!MNH_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 06/02/2019: simplify OPEN_ll and do somme assignments at a more logical place
!  P. Wautelet 07/02/2019: remove OPARALLELIO argument from open and close files subroutines
!                          (nsubfiles_ioz is now determined in IO_FILE_ADD2LIST)
!-----------------------------------------------------------------

MODULE MODI_IO_ll
!
INTERFACE 
  SUBROUTINE INITIO_ll()
  END SUBROUTINE INITIO_ll

  SUBROUTINE OPEN_ll(&
       TPFILE,  &
       MODE,    &
       STATUS,  &
       IOSTAT,  &
       POSITION,&
       DELIM,    &
       HPROGRAM_ORIG)

    USE MODD_IO_ll, ONLY : TFILEDATA

    TYPE(TFILEDATA), INTENT(INOUT)         :: TPFILE
    CHARACTER(len=*),INTENT(IN),  OPTIONAL :: MODE
    CHARACTER(len=*),INTENT(IN),  OPTIONAL :: STATUS
    INTEGER,         INTENT(OUT)           :: IOSTAT
    CHARACTER(len=*),INTENT(IN),  OPTIONAL :: POSITION
    CHARACTER(len=*),INTENT(IN),  OPTIONAL :: DELIM
    CHARACTER(LEN=*),INTENT(IN),  OPTIONAL :: HPROGRAM_ORIG !To emulate a file coming from this program
  END SUBROUTINE OPEN_ll

  SUBROUTINE CLOSE_ll(TPFILE,IOSTAT,HPROGRAM_ORIG)
  USE MODD_IO_ll, ONLY : TFILEDATA

    TYPE(TFILEDATA),  INTENT(IN)            :: TPFILE
    INTEGER,          INTENT(OUT), OPTIONAL :: IOSTAT
    CHARACTER(LEN=*), INTENT(IN),  OPTIONAL :: HPROGRAM_ORIG !To emulate a file coming from this program
 END SUBROUTINE CLOSE_ll

END INTERFACE
!
END MODULE MODI_IO_ll
