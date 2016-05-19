! $Source$
!-----------------------------------------------------------------
          SUBROUTINE SYSTEM_MNH(HCOMMAND)
!!
!!    PURPOSE
!!    -------
!!    This subroutine writes the 1 command line HCOMMAND
!!    in the file pipe_name and flushes the buffer.
USE MODE_IO_ll
!!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
!
          CHARACTER(LEN=*)    :: HCOMMAND
!
!*       0.2   Declaration of local variables
!              ------------------------------
#if defined(MNH_LINUX) || defined(MNH_SP4)
         CHARACTER(LEN=*),PARAMETER :: CFILE="file_for_xtransfer"
#else
#if !defined(MNH_SX5)
         CHARACTER(LEN=*),PARAMETER :: CFILE="file_for_fujitransfer"
#else
         CHARACTER(LEN=*),PARAMETER :: CFILE="file_for_nectransfer"
#endif
#endif
         INTEGER                    :: IUNIT
!
!
!
          IUNIT=IONEWFLU()
          OPEN(UNIT=IUNIT,FILE=CFILE,ACCESS="sequential" &
&             ,FORM="formatted",POSITION="append")
          WRITE(IUNIT,*) HCOMMAND
          CLOSE(IUNIT)

          END SUBROUTINE SYSTEM_MNH

