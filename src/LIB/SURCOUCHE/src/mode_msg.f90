!MNH_LIC Copyright 2017-2020 CNRS, Meteo-France and Universite Paul Sabatier
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
!  P. Wautelet 08/04/2020: add multiline Print_msg
!-----------------------------------------------------------------
module mode_msg

use modd_io, only: NVERB_FATAL, NVERB_ERROR, NVERB_WARNING, NVERB_INFO, NVERB_DEBUG

implicit none

integer, parameter :: NMSGLGTMAX   = 100 ! Maximum length for a message
integer, parameter :: NMSGLLINEMAX = 10  ! Maximum number of lines for a message

character(len=NMSGLGTMAX), dimension(NMSGLLINEMAX) :: cmnhmsg = ''

interface Print_msg
  module procedure Print_msg_1line, Print_msg_multi_cmnhmsg, Print_msg_multi
end interface Print_msg


contains

subroutine Print_msg_1line( kverb, hdomain, hsubr, hmsg )
  integer,          intent(in) :: kverb   !Verbosity level
  character(len=*), intent(in) :: hdomain !Domain/category of message
  character(len=*), intent(in) :: hsubr   !Subroutine/function name
  character(len=*), intent(in) :: hmsg    !Message

  call Print_msg_multi( kverb, hdomain, hsubr, [hmsg] )

end subroutine Print_msg_1line


subroutine Print_msg_multi_cmnhmsg( kverb, hdomain, hsubr )

  integer,          intent(in) :: kverb   !Verbosity level
  character(len=*), intent(in) :: hdomain !Domain/category of message
  character(len=*), intent(in) :: hsubr   !Subroutine/function name

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


subroutine Print_msg_multi( KVERB, HDOMAIN, HSUBR, HMSG )
!
USE ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT
!
USE MODD_CONF,       ONLY: CPROGRAM
USE MODD_IO,         ONLY: NIO_VERB, NIO_ABORT_LEVEL, NGEN_VERB, NGEN_ABORT_LEVEL, &
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
!
character(len=2)  :: ysz
CHARACTER(LEN=2)  :: YPRC
CHARACTER(LEN=9)  :: YPRE
CHARACTER(LEN=30) :: YSUBR
character(len=:), allocatable :: yformat
INTEGER :: IERR, IMAXVERB,IABORTLEVEL
INTEGER :: ILU
integer :: ji
integer :: ilines
LOGICAL :: GWRITE_OUTLST,GWRITE_STDOUT
!
!Determine if the process will write
GWRITE_OUTLST = .FALSE.
GWRITE_STDOUT = .FALSE.
IF (IP == 1 .OR. LVERB_ALLPRC) THEN
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
!PW: TODO?: temporary to detect non-initialisation
! should disappear except at the beginning of a run
  GWRITE_OUTLST = .FALSE.
  IF (GWRITE_STDOUT .AND. CPROGRAM/='LFICDF') WRITE(UNIT=OUTPUT_UNIT,FMT=*) 'TFILE_OUTPUTLISTING not associated'
END IF
!
SELECT CASE(HDOMAIN)
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
  CASE(NVERB_ERROR)
    YPRE='ERROR:   '
  CASE(NVERB_WARNING)
    YPRE='WARNING: '
  CASE(NVERB_INFO)
    YPRE='INFO:    '
  CASE(NVERB_DEBUG)
    YPRE='DEBUG:   '
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

if ( lverb_allprc ) then
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
      Write( unit = output_unit, fmt = yformat ) ip, ypre, ysubr, hmsg
    else
      yformat = '(' // yprc // ','': '',a9,a30,' // ysz // ',''/'',' // ysz // ','': '',a)'
      do ji = 1, ilines
        Write( unit = output_unit, fmt = yformat ) ip, ypre, ysubr, ji, ilines, Trim( hmsg(ji) )
      end do
    end if
  end if
  if ( gwrite_outlst ) then
    if ( ilines == 1 ) then
      yformat = '(' // yprc // ','': '',a9,a30,a)'
      Write( unit = ilu, fmt = yformat) ip, ypre, ysubr, hmsg
    else
      yformat = '(' // yprc // ','': '',a9,a30,' // ysz // ',''/'',' // ysz // ','': '',a)'
      do ji = 1, ilines
        Write( unit = ilu, fmt = yformat) ip, ypre, ysubr, ji, ilines, Trim( hmsg(ji) )
      end do
    end if
  end if
else
  if ( gwrite_stdout ) then
    if ( ilines == 1 ) then
        Write( unit = output_unit, fmt = "(a9,a30,a)" ) ypre, ysubr, Trim( hmsg(1) )
    else
      yformat = '(a9,a30,' // ysz // ',''/'',' // ysz // ','': '',a)'
      do ji = 1, ilines
        Write(unit = output_unit, fmt = yformat ) ypre, ysubr, ji, ilines, Trim( hmsg(ji) )
      end do
    end if
  end if
  if ( gwrite_outlst ) then
    if ( ilines == 1 ) then
        Write( unit = ilu, fmt = "(a9,a30,a)") ypre, ysubr, Trim( hmsg(1) )
    else
      yformat = '(a9,a30,' // ysz // ',''/'',' // ysz // ','': '',a)'
      do ji = 1, ilines
        Write( unit = ilu, fmt = yformat) ypre, ysubr, ji, ilines, Trim( hmsg(ji) )
      end do
    end if
  end if
end if
!
IF (KVERB<=IABORTLEVEL) THEN
  IF (GWRITE_STDOUT) WRITE(UNIT=OUTPUT_UNIT,FMT=*) 'ABORT asked by application '//TRIM(CPROGRAM)
  IF (GWRITE_OUTLST) WRITE(UNIT=ILU,        FMT=*) 'ABORT asked by application '//TRIM(CPROGRAM)
  !Every process write on the error unit. This is necessary if the abort is done by an other process than 0.
  WRITE(UNIT=ERROR_UNIT,FMT="(A8,': ',A9,A30,A)") ADJUSTL(YPRC),YPRE,YSUBR,HMSG
  WRITE(UNIT=ERROR_UNIT,FMT="(A8,': ',A)")        ADJUSTL(YPRC),'ABORT asked by application '//TRIM(CPROGRAM)
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

end module mode_msg
