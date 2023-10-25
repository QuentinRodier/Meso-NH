!MNH_LIC Copyright 1993-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!!    ###############
      MODULE MODE_POS
!!    ###############
!!
implicit none
!!
!!
CONTAINS
!!
!!    ###########################################
      SUBROUTINE POSNAM( TPFILE, HDNAML, OFOUND )
!!    ###########################################
!!
!!*** *POSNAM*
!!
!!    PURPOSE
!!    -------
!     To position namelist file at correct place for reading
!     namelist CDNAML.
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENT
!!    -----------------
!!
!!    REFERENCE
!!    ----------
!!       ECMWF Research Department documentation of the IFS
!!
!!    AUTHOR
!!    -------
!!       Mats Hamrud *ECMWF*
!!
!!    MODIFICATIONS
!!    --------------
!!       Original : 22/06/93
!!       I. Mallet  15/10/01     adaptation to MesoNH (F90 norm)
!  P. Wautelet 10/04/2019: replace ABORT and STOP calls by Print_msg
!  P. Wautelet 05/04/2023: POSNAM: modernisation + improvements
!------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
USE MODD_IO,    ONLY: TFILEDATA

USE MODE_MSG
USE MODE_TOOLS, ONLY: Upcase
!
!*       0.1   Declarations of arguments
!
TYPE(TFILEDATA),  INTENT(IN) :: TPFILE
CHARACTER(LEN=*), INTENT(IN) :: HDNAML
LOGICAL,          INTENT(OUT):: OFOUND
!
!*       0.2   Declarations of local variables
!
CHARACTER(LEN=100)            :: YERRORMSG
CHARACTER(LEN=120)            :: YLINE
CHARACTER(LEN=1)              :: YLTEST
CHARACTER(LEN=:), ALLOCATABLE :: YDNAML
INTEGER                       :: ILU
INTEGER                       :: ILEN, IND1, IRET
INTEGER                       :: IVERBLVL


OFOUND = .FALSE.

IF ( .NOT.TPFILE%LOPENED ) THEN
  call Print_msg( NVERB_ERROR, 'IO', 'POSNAM', Trim( TPFILE%CNAME ) // ' not opened' )
  RETURN
END IF

ILU = TPFILE%NLU

IF ( TPFILE%CTYPE == 'DES' ) THEN
  IVERBLVL = NVERB_DEBUG
ELSE IF ( TPFILE%CTYPE == 'NML' ) THEN
  IVERBLVL = NVERB_INFO
ELSE
  ! Check if there is an associated .des file
  ! and use it if available
  IF ( ASSOCIATED( TPFILE%TDESFILE ) ) THEN
    IVERBLVL = NVERB_DEBUG
    IF ( .NOT.TPFILE%TDESFILE%LOPENED ) THEN
      call Print_msg( NVERB_ERROR, 'IO', 'POSNAM', Trim( TPFILE%TDESFILE%CNAME ) // ' not opened' )
      RETURN
    END IF
    ILU = TPFILE%TDESFILE%NLU
  ELSE
    IVERBLVL = NVERB_INFO
    call Print_msg( NVERB_WARNING, 'IO', 'POSNAM', Trim( TPFILE%CNAME ) &
                    // ': unexpected filetype: ' // Trim( TPFILE%CTYPE ) )
  END IF
END IF

REWIND( ILU )

ILEN = LEN( HDNAML )
ALLOCATE( CHARACTER(LEN=ILEN) :: YDNAML )
YDNAML = Upcase( HDNAML ) ! Force namelist name into upper case (read namelist name will also be upcased)

search_nam : DO
  READ ( UNIT=ILU, FMT='(A)', IOSTAT=IRET, IOMSG=YERRORMSG, END=100 ) YLINE
  IF (IRET /= 0 ) THEN
    call Print_msg( NVERB_ERROR, 'IO', 'POSNAM', Trim( HDNAML) // ': read error:' // Trim( YERRORMSG ) )
  ELSE
    YLINE = Upcase( YLINE )
    IND1 = INDEX( YLINE, '&'//YDNAML )
    IF( IND1 /= 0 ) THEN
      IF( IND1 > 1 ) THEN
        IF ( LEN_TRIM( YLINE(:IND1-1) ) /= 0 ) THEN
          ! Check that it is really the beginning of a namelist and that is not a comment
          ! Nothing but spaces is allowed here
          call Print_msg( NVERB_DEBUG, 'IO', 'POSNAM', 'invalid header or commented namelist: ' // Trim ( YLINE) )
          CYCLE
        END IF
      END IF
      YLTEST = YLINE(IND1+ILEN+1:IND1+ILEN+1)
      IF( YLTEST == ' ' ) EXIT search_nam
    END IF
  END IF
END DO search_nam

BACKSPACE( ILU )
OFOUND = .TRUE.
call Print_msg( IVERBLVL, 'IO', 'POSNAM', Trim( TPFILE%CNAME ) // ': namelist ' // Trim( HDNAML ) // ' found' )

RETURN

! end of file: namelist name not found
100  CONTINUE
call Print_msg( IVERBLVL, 'IO', 'POSNAM', Trim( TPFILE%CNAME ) // ': namelist ' // Trim( HDNAML ) &
                 // ' not found: default values used if required' )

!------------------------------------------------------------------
END SUBROUTINE POSNAM
!!
!!
!!    ################################################
      SUBROUTINE POSKEY(KULNAM,KLUOUT,HKEYWD1,HKEYWD2)
!!    ################################################
!!
!!*** *POSKEY*
!!
!!    PURPOSE
!!    -------
!     To position namelist file at correct place after reading
!     keyword HKEYWD
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENT
!!    -----------------
!!
!!    REFERENCE
!!    ----------
!!
!!    AUTHOR
!!    -------
!!       I. Mallet *Meteo-France*
!!
!!    MODIFICATIONS
!!    --------------
!!       Original : 15/10/01
!------------------------------------------------------------------------------
!
use mode_msg
!
!*       0.    DECLARATIONS
!              ------------
!
!*       0.1   Declarations of arguments
!
INTEGER,                    INTENT(IN) :: KULNAM
INTEGER,                    INTENT(IN) :: KLUOUT
CHARACTER(LEN=*),           INTENT(IN) :: HKEYWD1
CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: HKEYWD2
!
!*       0.2   Declarations of local variables
!
CHARACTER(LEN=120) :: YLINE
INTEGER            :: ILEN1, ILEN2, IRET
!
!
!*       1.    POSITION FILE
!              -------------
!
REWIND(KULNAM)
ILEN1=LEN(HKEYWD1)
IF (PRESENT(HKEYWD2)) ILEN2=LEN(HKEYWD2)
!
search_key : DO
      YLINE=' '
      READ(UNIT=KULNAM,FMT='(A)',IOSTAT=IRET,END=100) YLINE
      IF (IRET /=0 ) THEN
         WRITE(KLUOUT,FMT=*) '-> error when reading line from unit ',KULNAM
      ELSE
        YLINE=ADJUSTL(YLINE)
        IF (YLINE(1:ILEN1) .EQ. HKEYWD1(1:ILEN1)) EXIT search_key
      ENDIF
ENDDO search_key
!
WRITE(KLUOUT,FMT=*) '-- keyword ',HKEYWD1,' found'
!
RETURN
!
! end of file: keyword not found
100  CONTINUE
IF (.NOT.PRESENT(HKEYWD2)) THEN
  call Print_msg( NVERB_FATAL, 'GEN', 'POSKEY', 'keyword '//trim(HKEYWD1)//' not found' )
ELSE
!
!*       2.    SECOND KEYWORD: POSITION FILE
!              -----------------------------
!
  REWIND(KULNAM)
  search_key2 : DO
      YLINE=' '
      READ(UNIT=KULNAM,FMT='(A)',IOSTAT=IRET,END=101) YLINE
      IF (IRET /=0 ) THEN
        WRITE(KLUOUT,FMT=*) '-> error when reading line from unit ',KULNAM
      ELSE
        YLINE=ADJUSTL(YLINE)
        IF (YLINE(1:ILEN2) .EQ. HKEYWD2(1:ILEN2)) EXIT search_key2
      ENDIF
  ENDDO search_key2
  WRITE(KLUOUT,FMT=*) '-- keyword ',HKEYWD2,' found'
  RETURN
END IF
! end of file: scd keyword not found
101  CONTINUE
call Print_msg( NVERB_FATAL, 'GEN', 'POSKEY', 'keyword '//trim(HKEYWD2)//' not found' )
!------------------------------------------------------------------
END SUBROUTINE POSKEY
!
END MODULE MODE_POS
