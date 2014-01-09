!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 chimie 2006/05/18 13:07:25
!-----------------------------------------------------------------
!!    ######################### 
      MODULE MODI_CH_OPEN_INPUT
!!    ######################### 
!!
INTERFACE
!!
SUBROUTINE CH_OPEN_INPUT(HCHEM_INPUT_FILE,HKEYWORD,KCHANNEL,KLUOUT,KVERB)
IMPLICIT NONE
CHARACTER(LEN=*), INTENT(IN) :: HCHEM_INPUT_FILE ! general purpose input file
CHARACTER(LEN=*), INTENT(IN) :: HKEYWORD         ! keyword for positioning
INTEGER         , INTENT(OUT):: KCHANNEL         ! I/O channel to choose
INTEGER,          INTENT(IN) :: KLUOUT           ! output listing channel
INTEGER,          INTENT(IN) :: KVERB            ! verbosity level
END SUBROUTINE CH_OPEN_INPUT
!!
END INTERFACE
!!
END MODULE MODI_CH_OPEN_INPUT
!!
!!    ######################################################################### 
      SUBROUTINE CH_OPEN_INPUT(HCHEM_INPUT_FILE,HKEYWORD,KCHANNEL,KLUOUT,KVERB)
!!    #########################################################################
!!
!!*** *CH_OPEN_INPUT*
!!
!!    PURPOSE
!!    -------
!       Open the general purpose chemical input file and position the
!     file pointer after the indicated keyword.
!!
!!**  METHOD
!!    ------
!!      An unused input channel is selected using OPEN_ll.
!!    The file HCHEM_INPUT_FILE will be rewinded
!!    at each call and data will be read in using (A8)-format until the
!!    given keyword is found. The following comment line will then
!!    be read and printed and the input channel number will be returned.
!!    After reading the needed data, the user must assure that the file
!!    will be closed and that the unit will be freed using CLOSE_ll.
!!
!!    REFERENCE
!!    ---------
!!    MesoNH book 2
!!
!!    AUTHOR
!!    ------
!!    K. Suhre   *Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 03/11/95
!!    05/08/96 (K. Suhre) restructured
!!    11/08/98 (N. Asencio) add parallel code
!!
!!    EXTERNAL
!!    --------
!!    OPEN_ll  ! attribute a free I/O unit
USE MODE_IO_ll
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!    none
!!
!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
CHARACTER(LEN=*), INTENT(IN) :: HCHEM_INPUT_FILE ! general purpose input file
CHARACTER(LEN=*), INTENT(IN) :: HKEYWORD         ! keyword for positioning
INTEGER         , INTENT(OUT):: KCHANNEL         ! I/O channel to choose
INTEGER,          INTENT(IN) :: KLUOUT           ! output listing channel
INTEGER,          INTENT(IN) :: KVERB            ! verbosity level
!
!*      0.2    declarations of local variables
!
CHARACTER(LEN=79) :: YIN ! character string for line-by-line read
INTEGER :: IFAIL         ! return code from OPEN_ll
!
!-------------------------------------------------------------------------------
!
!
!*       1.    FIND A FREE I/O CHANNEL
!              -----------------------
!
IF (KVERB >= 5) WRITE(KLUOUT,*) "CH_OPEN_INPUT: opening file ", HCHEM_INPUT_FILE
CALL OPEN_ll(UNIT=KCHANNEL,FILE=HCHEM_INPUT_FILE,IOSTAT=IFAIL ,MODE=GLOBAL,  &
             POSITION='REWIND', ACTION='READ', STATUS='OLD', FORM='FORMATTED')
!
IF (IFAIL /= 0) THEN
  WRITE(KLUOUT,*) "CH_OPEN_INPUT ERROR: unable to open file", HCHEM_INPUT_FILE
  WRITE(KLUOUT,*) "                     OPEN_ll return code is: ", IFAIL
  WRITE(KLUOUT,*) "                     the program will be stopped now"
  ! callabortstop
  CALL ABORT
  STOP
END IF
!
!-------------------------------------------------------------------------------
!
!*       2.    READ INPUT DATA UNTIL KEYWORD IS FOUND
!              --------------------------------------
!
! open the input file
REWIND(KCHANNEL)
WRITE(KLUOUT,*) "CH_OPEN_INPUT: opened channel ", KCHANNEL, &
                " for file ", HCHEM_INPUT_FILE
!
! read general comment line and print it
READ(KCHANNEL,"(A)") YIN
IF (KVERB >= 5) WRITE(KLUOUT, *) YIN
!
search_key : DO
  READ(KCHANNEL,"(A8)", END=100) YIN
  IF (HKEYWORD(1:8) .EQ. YIN(1:8)) EXIT search_key
ENDDO search_key
!
! read specific comment line and print it
IF (KVERB >= 5) WRITE(KLUOUT,*) &
   "Keyword ", HKEYWORD(1:8), " has been found, the specific comment is:"
READ(KCHANNEL,"(A)") YIN
IF (KVERB >= 5) WRITE(KLUOUT, *) YIN
!
RETURN
!
!-------------------------------------------------------------------------------
!
!*       3.    IF THE KEYWORD HAS NOT BEEN FOUND, STOP
!              ---------------------------------------
!
100 CONTINUE
WRITE(KLUOUT,*) "CH_OPEN_INPUT-Error: Keyword ", HKEYWORD(1:8), " not found."
! callabortstop
CALL ABORT
STOP "Program stopped"
!
END SUBROUTINE CH_OPEN_INPUT
