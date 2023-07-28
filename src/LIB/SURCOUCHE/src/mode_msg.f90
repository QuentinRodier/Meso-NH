!MNH_LIC Copyright 2017-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author(s)
!  P. Wautelet 24/01/2017
! Modifications:
!  P. Wautelet 27/02/2019: module extracted from mode_io.f90
!  P. Wautelet 04/04/2019: force write on stderr for all processes in print_msg if abort
!  P. Wautelet 02/07/2019: flush messages also for files opened with newunit (logical unit can be negative)
!  P. Wautelet 17/01/2020: add 'BUD' category for Print_msg
!  P. Wautelet 08/04/2020: add multiline Print_msg
!  P. Wautelet 01/07/2021: add counters for the number of prints + subroutine Msg_stats
!  P. Wautelet 01/07/2022: add olocal optional argument to force Print_msg on current process
!-----------------------------------------------------------------
module mode_msg

use modd_io, only: NVERB_FATAL, NVERB_ERROR, NVERB_WARNING, NVERB_INFO, NVERB_DEBUG

implicit none

integer, parameter :: NMSGLGTMAX   = 100 ! Maximum length for a message
integer, parameter :: NMSGLLINEMAX = 10  ! Maximum number of lines for a message

character(len=NMSGLGTMAX), dimension(NMSGLLINEMAX) :: cmnhmsg = ''

integer, save :: nfatal   = 0
integer, save :: nerror   = 0
integer, save :: nwarning = 0
integer, save :: ninfo    = 0
integer, save :: ndebug   = 0

interface Print_msg
  module procedure Print_msg_1line, Print_msg_multi_cmnhmsg, Print_msg_multi
end interface Print_msg


contains

subroutine Print_msg_1line( kverb, hdomain, hsubr, hmsg, olocal )
  integer,           intent(in) :: kverb   !Verbosity level
  character(len=*),  intent(in) :: hdomain !Domain/category of message
  character(len=*),  intent(in) :: hsubr   !Subroutine/function name
  character(len=*),  intent(in) :: hmsg    !Message
  logical, optional, intent(in) :: olocal  !true to force print on this process (if verbosity level is high enough)

  call Print_msg_multi( kverb, hdomain, hsubr, [hmsg], olocal )

end subroutine Print_msg_1line


subroutine Print_msg_multi_cmnhmsg( kverb, hdomain, hsubr, olocal )

  integer,           intent(in) :: kverb   !Verbosity level
  character(len=*),  intent(in) :: hdomain !Domain/category of message
  character(len=*),  intent(in) :: hsubr   !Subroutine/function name
  logical, optional, intent(in) :: olocal  !true to force print on this process (if verbosity level is high enough)

  integer :: ilines

  !Find the last non empty line
  ilines = size( cmnhmsg )
  do while ( len_trim( cmnhmsg(ilines) ) == 0 )
    ilines = ilines - 1
  end do

  call Print_msg_multi( kverb, hdomain, hsubr, cmnhmsg(1 : ilines) )

  !Empty the message buffer
  !This is necessary especially if the next call contain a shorter message
  cmnhmsg(1 : ilines) = ''

end subroutine Print_msg_multi_cmnhmsg


subroutine Print_msg_multi( KVERB, HDOMAIN, HSUBR, HMSG, OLOCAL )
!
USE ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT
!
USE MODD_CONF,       ONLY: CPROGRAM
USE MODD_IO,         ONLY: NBUD_VERB, NBUD_ABORT_LEVEL, NIO_VERB, NIO_ABORT_LEVEL, NGEN_VERB, NGEN_ABORT_LEVEL, &
                           LVERB_OUTLST, LVERB_STDOUT, LVERB_ALLPRC, TFILE_OUTPUTLISTING
USE MODD_LUNIT,      ONLY: TLUOUT0
USE MODD_VAR_ll,     ONLY: IP, NMNH_COMM_WORLD, NPROC
!
use modi_tools_c
!
INTEGER,                        INTENT(IN) :: KVERB   !Verbosity level
CHARACTER(LEN=*),               INTENT(IN) :: HDOMAIN !Domain/category of message
CHARACTER(LEN=*),               INTENT(IN) :: HSUBR   !Subroutine/function name
CHARACTER(LEN=*), dimension(:), INTENT(IN) :: HMSG    !Message
LOGICAL, OPTIONAL,              INTENT(IN) :: OLOCAL  !true to force print on this process (if verbosity level is high enough)
!
character(len=2)  :: ysz
CHARACTER(LEN=2)  :: YPRC
character(len=8)  :: yprcnb
CHARACTER(LEN=9)  :: YPRE
CHARACTER(LEN=30) :: YSUBR
character(len=:), allocatable :: yformat
INTEGER :: IERR, IMAXVERB,IABORTLEVEL
INTEGER :: ILU
integer :: ji
integer :: ilines
logical :: glocal
LOGICAL :: GWRITE_OUTLST,GWRITE_STDOUT

if ( present( olocal ) ) then
  glocal = olocal
else
  glocal = .false.
end if

!Determine if the process will write
GWRITE_OUTLST = .FALSE.
GWRITE_STDOUT = .FALSE.
IF ( IP == 1 .OR. LVERB_ALLPRC .OR. GLOCAL ) THEN
  IF (LVERB_OUTLST) GWRITE_OUTLST = .TRUE.
  IF (LVERB_STDOUT) GWRITE_STDOUT = .TRUE.
END IF
!
!Check if the output file is available
ILU = -1
IF (ASSOCIATED(TFILE_OUTPUTLISTING)) THEN
  IF (TFILE_OUTPUTLISTING%LOPENED) THEN
    ILU = TFILE_OUTPUTLISTING%NLU
  ELSE
    GWRITE_OUTLST = .FALSE.
    IF (GWRITE_STDOUT) WRITE(UNIT=OUTPUT_UNIT,FMT=*) 'TFILE_OUTPUTLISTING not opened'
  END IF
ELSE
! should disappear except at the beginning of a run
  GWRITE_OUTLST = .FALSE.
  IF (GWRITE_STDOUT .AND. CPROGRAM/='LFICDF') WRITE(UNIT=OUTPUT_UNIT,FMT=*) 'TFILE_OUTPUTLISTING not associated'
END IF
!
SELECT CASE(HDOMAIN)
  CASE('BUD')
    !Budget messages
    IMAXVERB    = NBUD_VERB
    IABORTLEVEL = NBUD_ABORT_LEVEL
  CASE('IO')
    IMAXVERB    = NIO_VERB
    IABORTLEVEL = NIO_ABORT_LEVEL
  CASE ('GEN')
    IMAXVERB    = NGEN_VERB
    IABORTLEVEL = NGEN_ABORT_LEVEL
  CASE DEFAULT
    IF (GWRITE_STDOUT) WRITE(UNIT=OUTPUT_UNIT,FMT=*) 'ERROR: PRINT_MSG: wrong message category (',TRIM(HDOMAIN),')'
    IF (GWRITE_OUTLST) WRITE(UNIT=ILU,        FMT=*) 'ERROR: PRINT_MSG: wrong message category (',TRIM(HDOMAIN),')'
    RETURN
END SELECT
!
IF (KVERB>IMAXVERB) RETURN

ilines = size( hmsg )

SELECT CASE(KVERB)
  CASE(NVERB_FATAL)
    YPRE='FATAL:   '
    nfatal = nfatal + 1
  CASE(NVERB_ERROR)
    YPRE='ERROR:   '
    nerror = nerror + 1
  CASE(NVERB_WARNING)
    YPRE='WARNING: '
    nwarning = nwarning + 1
  CASE(NVERB_INFO)
    YPRE='INFO:    '
    ninfo = ninfo + 1
  CASE(NVERB_DEBUG)
    YPRE='DEBUG:   '
    ndebug = ndebug + 1
  CASE DEFAULT
    IF (GWRITE_STDOUT) WRITE(UNIT=OUTPUT_UNIT,FMT=*) 'ERROR: PRINT_MSG: wrong verbosity level'
    IF (GWRITE_OUTLST) WRITE(UNIT=ILU,        FMT=*) 'ERROR: PRINT_MSG: wrong verbosity level'
END SELECT
!
YSUBR=TRIM(HSUBR)//':'

if ( ilines < 10 ) then
  ysz = 'I1'
else if ( ilines < 100 ) then
  ysz = 'I2'
else if ( ilines < 1000 ) then
  ysz = 'I3'
else
  ysz = 'I4'
end if

if ( lverb_allprc .or. glocal ) then
  if ( nproc < 10 ) then
    yprc = 'I1'
  else if ( nproc < 100 ) then
    yprc = 'I2'
  else if ( nproc < 1000 ) then
    yprc = 'I3'
  else if ( nproc < 10000 ) then
    yprc = 'I4'
  else if ( nproc < 100000 ) then
    yprc = 'I5'
  else if ( nproc < 1000000 ) then
    yprc = 'I6'
  else if ( nproc < 10000000 ) then
    yprc = 'I7'
  else
    yprc = 'I8'
  end if

  if ( gwrite_stdout ) then
    if ( ilines == 1 ) then
      yformat = '(' // yprc // ','': '',a9,a30,a)'
      Write( unit = output_unit, fmt = yformat ) ip - 1, ypre, ysubr, hmsg
    else
      yformat = '(' // yprc // ','': '',a9,a30,' // ysz // ',''/'',' // ysz // ','': '',a)'
      do ji = 1, ilines
        Write( unit = output_unit, fmt = yformat ) ip - 1, ypre, ysubr, ji, ilines, Trim( hmsg(ji) )
      end do
    end if
  end if
  if ( gwrite_outlst ) then
    if ( ilines == 1 ) then
      yformat = '(' // yprc // ','': '',a9,a30,a)'
      Write( unit = ilu, fmt = yformat) ip - 1, ypre, ysubr, hmsg
    else
      yformat = '(' // yprc // ','': '',a9,a30,' // ysz // ',''/'',' // ysz // ','': '',a)'
      do ji = 1, ilines
        Write( unit = ilu, fmt = yformat) ip - 1, ypre, ysubr, ji, ilines, Trim( hmsg(ji) )
      end do
    end if
  end if
else
  if ( gwrite_stdout ) then
    if ( ilines == 1 ) then
        Write( unit = output_unit, fmt = "('0: ',a9,a30,a)" ) ypre, ysubr, Trim( hmsg(1) )
    else
      yformat = '("0: ",a9,a30,' // ysz // ',''/'',' // ysz // ','': '',a)'
      do ji = 1, ilines
        Write(unit = output_unit, fmt = yformat ) ypre, ysubr, ji, ilines, Trim( hmsg(ji) )
      end do
    end if
  end if
  if ( gwrite_outlst ) then
    if ( ilines == 1 ) then
        Write( unit = ilu, fmt = "('0: ',a9,a30,a)") ypre, ysubr, Trim( hmsg(1) )
    else
      yformat = '("0: ",a9,a30,' // ysz // ',''/'',' // ysz // ','': '',a)'
      do ji = 1, ilines
        Write( unit = ilu, fmt = yformat) ypre, ysubr, ji, ilines, Trim( hmsg(ji) )
      end do
    end if
  end if
end if
!
IF (KVERB<=IABORTLEVEL) THEN
  Write( yprcnb, '( i8 )' ) ip - 1

  IF (GWRITE_STDOUT) WRITE(UNIT=OUTPUT_UNIT,FMT=*) 'ABORT asked by application '//TRIM(CPROGRAM)
  IF (GWRITE_OUTLST) WRITE(UNIT=ILU,        FMT=*) 'ABORT asked by application '//TRIM(CPROGRAM)
  !Every process write on the error unit. This is necessary if the abort is done by an other process than 0.
  WRITE(UNIT=ERROR_UNIT,FMT="(A8,': ',A9,A30,A)") ADJUSTL(yprcnb),YPRE,YSUBR,HMSG
  WRITE(UNIT=ERROR_UNIT,FMT="(A8,': ',A)")        ADJUSTL(yprcnb),'ABORT asked by application '//TRIM(CPROGRAM)
#if 0
  !Problem: loop dependency between MODE_MSG and MODE_FM (IO_FILE_CLOSE_ll call PRINT_MSG)
  NIO_VERB = 0 !To not get further messages (ABORT should be the last for readability)
  IF ( ILU /= -1 ) CALL IO_FILE_CLOSE_ll(TFILE_OUTPUTLISTING) !To flush it
#else
  IF ( ILU /= -1 ) FLUSH(UNIT=ILU) !OK in F2003
  IF (ASSOCIATED(TLUOUT0)) FLUSH(UNIT=TLUOUT0%NLU)
#endif
  !Add a sleep to ensure that the process(es) that have to write to stderr and to file
  !have enough time before an other process calls mpi_abort
  CALL SLEEP_C(5)
  !
  CALL MPI_ABORT(NMNH_COMM_WORLD, -10, IERR)
  CALL ABORT
END IF
!
end subroutine Print_msg_multi


subroutine Msg_stats()

character(len=10) :: ydebug
character(len=10) :: yinfo
character(len=10) :: ywarning
character(len=10) :: yerror
character(len=10) :: yfatal

Write( ydebug,   '( I10 )' ) ndebug
Write( yinfo,    '( I10 )' ) ninfo
Write( ywarning, '( I10 )' ) nwarning
Write( yerror,   '( I10 )' ) nerror
Write( yfatal,   '( I10 )' ) nfatal

Write( cmnhmsg(1), '( A )'      ) 'Number of calls to Print_msg (with actual printing):'
Write( cmnhmsg(2), '( A, A10 )' ) '  Calls with level DEBUG:   ', ydebug
Write( cmnhmsg(3), '( A, A10 )' ) '  Calls with level INFO:    ', yinfo
Write( cmnhmsg(4), '( A, A10 )' ) '  Calls with level WARNING: ', ywarning
Write( cmnhmsg(5), '( A, A10 )' ) '  Calls with level ERROR:   ', yerror
Write( cmnhmsg(6), '( A, A10 )' ) '  Calls with level FATAL:   ', yfatal

call Print_msg( NVERB_INFO, 'GEN', 'Msg_stats' )

end subroutine Msg_stats


end module mode_msg
