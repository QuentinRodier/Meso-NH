!MNH_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author(s)
!  D. Gazen
! Modifications:
!  J. Escobar  19/08/2005: bug argument optinonel ACCESS --> YACCESS
!  J. Escobar  22/05/2008: bug mode SPECIFIC in OPEN_ll
!  J. Escobar  05/11/2009: allow JPMAX_UNIT=48 open files
!  J. Escobar  18/10/2010: bug with PGI compiler on ADJUSTL
!  P. Wautelet 04/02/2016: bug with DELIM='NONE' and GCC 5.2/5.3
!  D.Gazen     April 2016: change error message
!  P. Wautelet May 2016  : use netCDF Fortran module
!  P. Wautelet July 2016 : added type OUTBAK
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  J. Pianezze 01/08/2016: add LOASIS flag
!  P. Wautelet 13/12/2018: moved some operations to new mode_io_*_nc4 modules
!  P. Wautelet 10/01/2019: bug correction: close correctly Z-split files
!  P. Wautelet 10/01/2019: use NEWUNIT argument of OPEN
!                          + move IOFREEFLU and IONEWFLU to mode_io_file_lfi.f90
!                          + move management of NNCID and NLFIFLU to the nc4 and lfi subroutines
!  P. Wautelet 10/01/2019: bug: modify some metadata before open calls
!  P. Wautelet 21/01/2019: add LIO_ALLOW_NO_BACKUP and LIO_NO_WRITE to modd_io_ll to allow
!                                 to disable writes (for bench purposes)
!  P. Wautelet 06/02/2019: simplify OPEN_ll and do somme assignments at a more logical place
!  P. Wautelet 07/02/2019: remove OPARALLELIO argument from open and close files subroutines
!                          (nsubfiles_ioz is now determined in IO_FILE_ADD2LIST)
!  P. Wautelet 14/02/2019: move UPCASE function to tools.f90
!  P. Wautelet 19/02/2019: simplification/restructuration/cleaning of open/close subroutines (TBCto be continued)
!
!-----------------------------------------------------------------
MODULE MODE_IO_ll

  USE MODD_MPIF
  USE MODD_VAR_ll, ONLY : NMNH_COMM_WORLD

  USE MODE_MSG

  IMPLICIT NONE 

  PRIVATE

  LOGICAL,SAVE :: GCONFIO = .FALSE. ! Turn TRUE when SET_CONFIO_ll is called.

  PUBLIC INITIO_ll,OPEN_ll,CLOSE_ll
  PUBLIC SET_CONFIO_ll,GCONFIO

CONTAINS 

  SUBROUTINE SET_CONFIO_ll()
    USE MODN_CONFIO, only: LCDF4, LLFIOUT, LLFIREAD

    !Use MODN_CONFIO namelist variables
    CALL SET_CONFIO_INTERN_ll(LCDF4, LLFIOUT, LLFIREAD)
  END SUBROUTINE SET_CONFIO_ll

  SUBROUTINE SET_CONFIO_INTERN_ll(OIOCDF4, OLFIOUT, OLFIREAD)
    USE MODD_IO_ll, ONLY : LIOCDF4, LLFIOUT, LLFIREAD, LIO_ALLOW_NO_BACKUP, LIO_NO_WRITE

    LOGICAL, INTENT(IN) :: OIOCDF4, OLFIOUT, OLFIREAD

    CALL PRINT_MSG(NVERB_DEBUG,'IO','SET_CONFIO_ll','called')

    IF (GCONFIO) THEN
      CALL PRINT_MSG(NVERB_WARNING,'IO','SET_CONFIO_ll','already called (ignoring this call)')
    ELSE
#if defined(MNH_IOCDF4)
      LIOCDF4  = OIOCDF4
      LLFIOUT  = OLFIOUT
      LLFIREAD = OLFIREAD

      IF (.NOT.LIOCDF4 .AND. .NOT.LLFIOUT) THEN
        CALL PRINT_MSG(NVERB_WARNING,'IO','SET_CONFIO_ll','output format forced to netCDF')
        LIOCDF4 = .TRUE.
      END IF
#else
      LIOCDF4  = .FALSE.
      LLFIOUT  = .TRUE.
      LLFIREAD = .TRUE.
#endif
      GCONFIO = .TRUE.

      ! Set LIO_ALLOW_NO_BACKUP=.true. if writes are disabled (to be coherent)
      IF (LIO_NO_WRITE) THEN
        CALL PRINT_MSG(NVERB_WARNING,'IO','SET_CONFIO_ll','file writes are disabled')
        LIO_ALLOW_NO_BACKUP = .true.
      END IF
    END IF

  END SUBROUTINE SET_CONFIO_INTERN_ll

  SUBROUTINE INITIO_ll()
    USE MODD_IO_ll,     only: CNULLFILE, GSMONOPROC, ISIOP, ISNPROC, ISP, NNULLUNIT

    USE MODE_MNH_WORLD, ONLY: INIT_NMNH_COMM_WORLD

    IMPLICIT NONE

    INTEGER :: IERR, IOS
    character(len=256) :: yioerrmsg

    CALL PRINT_MSG(NVERB_DEBUG,'IO','INITIO_ll','called')

    CALL INIT_NMNH_COMM_WORLD(IERR)
    IF (IERR .NE.0) CALL PRINT_MSG(NVERB_FATAL,'IO','INITIO_ll','problem with remapping of NMNH_COMM_WORLD')

    !! Now MPI is initialized for sure

    !! Default number for Processor I/O
    ISIOP = 1

    !! Get number of allocated processors
    CALL MPI_COMM_SIZE(NMNH_COMM_WORLD, ISNPROC,IERR)
    IF (ISNPROC==1) GSMONOPROC = .TRUE.

    !! Store proc number
    CALL MPI_COMM_RANK(NMNH_COMM_WORLD, ISP, IERR)
    ISP = ISP + 1

    !! Open /dev/null for GLOBAL mode
#if defined(DEV_NULL)
    OPEN(NEWUNIT=NNULLUNIT,FILE=CNULLFILE  ,ACTION='WRITE',IOSTAT=IOS, IOMSG=yioerrmsg)
#else
    OPEN(NEWUNIT=NNULLUNIT,STATUS='SCRATCH',ACTION='WRITE',IOSTAT=IOS, IOMSG=yioerrmsg)
#endif
    IF (IOS /= 0) THEN
       CALL PRINT_MSG(NVERB_FATAL,'IO','INITIO_ll','problem opening /dev/null :'//trim(yioerrmsg))
    END IF
  END SUBROUTINE INITIO_ll


  SUBROUTINE OPEN_ll(TPFILE, KRESP, HMODE, HSTATUS, HPOSITION, HDELIM, HPROGRAM_ORIG)

  USE MODD_IO_ll
#if defined(MNH_IOCDF4)
  use mode_io_file_nc4,         only: io_create_file_nc4, io_open_file_nc4
#endif
  use mode_io_file_lfi,         only: io_create_file_lfi, io_open_file_lfi
  USE MODE_IO_MANAGE_STRUCT,    ONLY: IO_FILE_ADD2LIST, IO_FILE_FIND_BYNAME
  use mode_io_tools,            only: io_rank
  use mode_tools,               only: upcase

    TYPE(TFILEDATA),            INTENT(INOUT) :: TPFILE
    INTEGER,                    INTENT(OUT)   :: KRESP
    CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: HMODE
    CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: HSTATUS
    CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: HPOSITION
    CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: HDELIM
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: HPROGRAM_ORIG !To emulate a file coming from this program
    !
    ! local var
    !
    INTEGER, PARAMETER :: RECL_DEF = 10000
    !
    CHARACTER(len=5)             :: YFILE
    CHARACTER(len=20)            :: YSTATUS
    CHARACTER(len=20)            :: YPOSITION
    CHARACTER(len=20)            :: YDELIM
    CHARACTER(len=20)            :: YACTION
    CHARACTER(len=20)            :: YMODE
    CHARACTER(LEN=256)           :: YIOERRMSG
    CHARACTER(LEN=:),ALLOCATABLE :: YPREFILENAME !To store the directory + filename
    INTEGER                      :: IFILE, IRANK_PROCIO
    INTEGER                      :: YRECL
    INTEGER                      :: IOS, IRESP
    TYPE(TFILEDATA),POINTER      :: TZSPLITFILE

    CALL PRINT_MSG(NVERB_DEBUG,'IO','OPEN_ll','opening '//TRIM(TPFILE%CNAME)//' for '//TRIM(TPFILE%CMODE))

    IOS = 0

    IF (PRESENT(HMODE)) THEN
       YMODE = HMODE
       YMODE = UPCASE(TRIM(ADJUSTL(YMODE)))
    ELSE
       YMODE = 'GLOBAL'         ! Default Mode
    END IF

    YACTION = TPFILE%CMODE
    YACTION = UPCASE(TRIM(ADJUSTL(YACTION)))
    IF (YACTION /= "READ" .AND. YACTION /= "WRITE") THEN
       KRESP = 99
       TPFILE%NLU = -1
       CALL PRINT_MSG(NVERB_ERROR,'IO','OPEN_ll','action='//TRIM(YACTION)//' not supported')
       RETURN
    END IF

    IF (.NOT. ANY(YMODE == (/'GLOBAL     ','SPECIFIC   ', 'IO_ZSPLIT  '/))) THEN
       KRESP = 99
       TPFILE%NLU = -1
       CALL PRINT_MSG(NVERB_ERROR,'IO','OPEN_ll','ymode='//TRIM(YMODE)//' not supported')
       RETURN
    END IF

    IF (PRESENT(HSTATUS)) THEN
       YSTATUS=HSTATUS
    ELSE
       YSTATUS='UNKNOWN'
    ENDIF

    IF (TPFILE%NRECL == -1) THEN
      YRECL = RECL_DEF
    ELSE
      YRECL = TPFILE%NRECL
    END IF

    IF (PRESENT(HPOSITION)) THEN
       YPOSITION=HPOSITION
    ELSE
       YPOSITION='ASIS'
    ENDIF
    IF (PRESENT(HDELIM)) THEN
       YDELIM=HDELIM
    ELSE
       YDELIM='NONE'
    ENDIF

    IF (ALLOCATED(TPFILE%CDIRNAME)) THEN
      IF(LEN_TRIM(TPFILE%CDIRNAME)>0) THEN
        YPREFILENAME = TRIM(TPFILE%CDIRNAME)//'/'//TRIM(TPFILE%CNAME)
      ELSE
        YPREFILENAME = TRIM(TPFILE%CNAME)
      END IF
    ELSE
      YPREFILENAME = TRIM(TPFILE%CNAME)
    END IF

    SELECT CASE(YMODE)

    CASE('GLOBAL')
       IF (YACTION == 'READ') THEN
          TPFILE%NMASTER_RANK  = -1
          TPFILE%LMASTER       = .TRUE. !Every process read the file
          TPFILE%LMULTIMASTERS = .TRUE.
       ELSE
          IF (TPFILE%CTYPE=='OUTPUTLISTING') THEN
            IF (LVERB_ALLPRC) THEN
              TPFILE%NMASTER_RANK  = -1
              TPFILE%LMASTER       = .TRUE. !Every process may write in the file
              TPFILE%LMULTIMASTERS = .TRUE.
            ELSE
              TPFILE%NMASTER_RANK  = ISIOP
              TPFILE%LMASTER       = (ISP == ISIOP)
              TPFILE%LMULTIMASTERS = .FALSE.
            END IF
          ELSE
            TPFILE%NMASTER_RANK  = ISIOP
            TPFILE%LMASTER       = (ISP == ISIOP)
            TPFILE%LMULTIMASTERS = .FALSE.
          END IF
       END IF
       TPFILE%NSUBFILES_IOZ = 0

       IF (TPFILE%LMASTER) THEN
          !! I/O processor case
          !JUAN : 31/03/2000 modif pour acces direct
          IF (TPFILE%CACCESS=='STREAM') THEN
             OPEN(NEWUNIT=TPFILE%NLU,     &
                  FILE=TRIM(YPREFILENAME),&
                  STATUS=YSTATUS,         &
                  ACCESS=TPFILE%CACCESS,  &
                  IOSTAT=IOS,             &
                  IOMSG=YIOERRMSG,        &
                  FORM=TPFILE%CFORM,      &
                  ACTION=YACTION)
          ELSEIF (TPFILE%CACCESS=='DIRECT') THEN
             OPEN(NEWUNIT=TPFILE%NLU,     &
                  FILE=TRIM(YPREFILENAME),&
                  STATUS=YSTATUS,         &
                  ACCESS=TPFILE%CACCESS,  &
                  IOSTAT=IOS,             &
                  IOMSG=YIOERRMSG,        &
                  FORM=TPFILE%CFORM,      &
                  RECL=YRECL,             &
                  ACTION=YACTION)
          ELSE
             IF (TPFILE%CFORM=="FORMATTED") THEN
               IF (YACTION=='READ') THEN
                OPEN(NEWUNIT=TPFILE%NLU,     &
                     FILE=TRIM(YPREFILENAME),&
                     STATUS=YSTATUS,         &
                     ACCESS=TPFILE%CACCESS,  &
                     IOSTAT=IOS,             &
                     IOMSG=YIOERRMSG,        &
                     FORM=TPFILE%CFORM,      &
                     RECL=YRECL,             &
                     POSITION=YPOSITION,     &
                     ACTION=YACTION)
                     !DELIM=YDELIM,          & !Philippe: commented because bug with GCC 5.X
               ELSE
                OPEN(NEWUNIT=TPFILE%NLU,     &
                     FILE=TRIM(YPREFILENAME),&
                     STATUS=YSTATUS,         &
                     ACCESS=TPFILE%CACCESS,  &
                     IOSTAT=IOS,             &
                     IOMSG=YIOERRMSG,        &
                     FORM=TPFILE%CFORM,      &
                     RECL=YRECL,             &
                     POSITION=YPOSITION,     &
                     ACTION=YACTION,         &
                     DELIM=YDELIM)
               ENDIF
             ELSE
                OPEN(NEWUNIT=TPFILE%NLU,     &
                     FILE=TRIM(YPREFILENAME),&
                     STATUS=YSTATUS,         &
                     ACCESS=TPFILE%CACCESS,  &
                     IOSTAT=IOS,             &
                     IOMSG=YIOERRMSG,        &
                     FORM=TPFILE%CFORM,      &
                     RECL=YRECL,             &
                     POSITION=YPOSITION,     &
                     ACTION=YACTION)
             ENDIF
          ENDIF

          IF (IOS/=0) CALL PRINT_MSG(NVERB_FATAL,'IO','OPEN_ll','Problem when opening '//TRIM(YPREFILENAME)//': '//TRIM(YIOERRMSG))
       ELSE
          !! NON I/O processors case
          IOS = 0
          TPFILE%NLU = NNULLUNIT
       END IF


    CASE('SPECIFIC')
       TPFILE%NMASTER_RANK  = -1
       TPFILE%LMASTER       = .TRUE. !Every process use the file
       TPFILE%LMULTIMASTERS = .TRUE.
       TPFILE%NSUBFILES_IOZ = 0

       IF (TPFILE%CACCESS=='DIRECT') THEN
          OPEN(NEWUNIT=TPFILE%NLU,                    &
               FILE=TRIM(YPREFILENAME)//SUFFIX(".P"), &
               STATUS=YSTATUS,                        &
               ACCESS=TPFILE%CACCESS,                 &
               IOSTAT=IOS,                            &
               IOMSG=YIOERRMSG,                       &
               FORM=TPFILE%CFORM,                     &
               RECL=YRECL,                            &
               ACTION=YACTION)
       ELSE
        IF (YACTION=='READ') THEN
          OPEN(NEWUNIT=TPFILE%NLU,                     &
               FILE=TRIM(YPREFILENAME)//SUFFIX(".P"),  &
               STATUS=YSTATUS,                         &
               ACCESS=TPFILE%CACCESS,                  &
               IOSTAT=IOS,                             &
               IOMSG=YIOERRMSG,                        &
               FORM=TPFILE%CFORM,                      &
               RECL=YRECL,                             &
               POSITION=YPOSITION,                     &
               ACTION=YACTION)
               !DELIM=YDELIM,         & !Philippe: commented because bug with GCC 5.X
         ELSE
          OPEN(NEWUNIT=TPFILE%NLU,                     &
               FILE=TRIM(YPREFILENAME)//SUFFIX(".P"),  &
               STATUS=YSTATUS,                         &
               ACCESS=TPFILE%CACCESS,                  &
               IOSTAT=IOS,                             &
               IOMSG=YIOERRMSG,                        &
               FORM=TPFILE%CFORM,                      &
               RECL=YRECL,                             &
               POSITION=YPOSITION,                     &
               ACTION=YACTION,                         &
               DELIM=YDELIM)
         ENDIF
       ENDIF

       IF (IOS/=0) CALL PRINT_MSG(NVERB_FATAL,'IO','OPEN_ll','Problem when opening '//TRIM(YPREFILENAME)//': '//TRIM(YIOERRMSG))



    CASE('IO_ZSPLIT')
       TPFILE%NMASTER_RANK  = ISIOP
       TPFILE%LMASTER       = (ISP == ISIOP)
       TPFILE%LMULTIMASTERS = .FALSE.

#if defined(MNH_IOCDF4)
       IF (TPFILE%LMASTER .AND. (TPFILE%CFORMAT=='LFI' .OR. TPFILE%CFORMAT=='LFICDF4') ) THEN
#else
       IF (TPFILE%LMASTER) THEN
#endif
       ELSE 
          !! NON I/O processors OR netCDF read case
          IOS = 0
       END IF

       IF (TPFILE%NSUBFILES_IOZ > 0) THEN
          IF (.NOT.ALLOCATED(TPFILE%TFILES_IOZ)) THEN
            ALLOCATE(TPFILE%TFILES_IOZ(TPFILE%NSUBFILES_IOZ))
          ELSE IF ( SIZE(TPFILE%TFILES_IOZ) /= TPFILE%NSUBFILES_IOZ ) THEN
            CALL PRINT_MSG(NVERB_FATAL,'IO','OPEN_ll','SIZE(TPFILE%TFILES_IOZ) /= TPFILE%NSUBFILES_IOZ for '//TRIM(TPFILE%CNAME))
          END IF
          DO IFILE=1,TPFILE%NSUBFILES_IOZ
             IRANK_PROCIO = 1 + IO_RANK(IFILE-1,ISNPROC,TPFILE%NSUBFILES_IOZ)
             WRITE(YFILE ,'(".Z",i3.3)') IFILE

             CALL IO_FILE_FIND_BYNAME(TRIM(TPFILE%CNAME)//TRIM(YFILE),TZSPLITFILE,IRESP)

             IF (IRESP/=0) THEN !File not yet in filelist => add it (nothing to do if already in list)
               IF (ALLOCATED(TPFILE%CDIRNAME)) THEN
                 CALL IO_FILE_ADD2LIST(TZSPLITFILE,TRIM(TPFILE%CNAME)//TRIM(YFILE),TPFILE%CTYPE,TPFILE%CMODE,        &
                                       HDIRNAME=TPFILE%CDIRNAME,                                                     &
                                       KLFINPRAR=TPFILE%NLFINPRAR,KLFITYPE=TPFILE%NLFITYPE,KLFIVERB=TPFILE%NLFIVERB, &
                                       HFORMAT=TPFILE%CFORMAT)
               ELSE
                 CALL IO_FILE_ADD2LIST(TZSPLITFILE,TRIM(TPFILE%CNAME)//TRIM(YFILE),TPFILE%CTYPE,TPFILE%CMODE,        &
                                       KLFINPRAR=TPFILE%NLFINPRAR,KLFITYPE=TPFILE%NLFITYPE,KLFIVERB=TPFILE%NLFIVERB, &
                                       HFORMAT=TPFILE%CFORMAT)
               END IF
             END IF

             IF (ALLOCATED(TPFILE%CDIRNAME)) THEN
               IF (LEN_TRIM(TZSPLITFILE%CDIRNAME)>0) THEN
                 YPREFILENAME = TRIM(TZSPLITFILE%CDIRNAME)//'/'//TRIM(TZSPLITFILE%CNAME)
               ELSE
                 YPREFILENAME = TRIM(TZSPLITFILE%CNAME)
               END IF
             ELSE
               YPREFILENAME = TRIM(TZSPLITFILE%CNAME)
             END IF

             TPFILE%TFILES_IOZ(IFILE)%TFILE => TZSPLITFILE
             !Done outside of the previous IF to prevent problems with .OUT files
             TZSPLITFILE%NMPICOMM      = NMNH_COMM_WORLD
             TZSPLITFILE%NMASTER_RANK  = IRANK_PROCIO
             TZSPLITFILE%LMASTER       = (ISP == IRANK_PROCIO)
             TZSPLITFILE%LMULTIMASTERS = .FALSE.
             TZSPLITFILE%NSUBFILES_IOZ = 0

             ! Must be done BEFORE the call to io_open_file_* because we need to read things in these subroutines
             TZSPLITFILE%LOPENED = .TRUE.
             TZSPLITFILE%NOPEN         = TZSPLITFILE%NOPEN         + 1
             TZSPLITFILE%NOPEN_CURRENT = TZSPLITFILE%NOPEN_CURRENT + 1

#if defined(MNH_IOCDF4)
             IF (TZSPLITFILE%CFORMAT=='NETCDF4' .OR. TZSPLITFILE%CFORMAT=='LFICDF4') THEN
                IF (YACTION == 'READ') THEN
                   ! Open netCDF File for reading
                   call io_open_file_nc4(tzsplitfile)
                   IOS = 0
                END IF

                IF (YACTION == 'WRITE') THEN
                   ! Create netCDF File for writing
                   call io_create_file_nc4(TZSPLITFILE, hprogram_orig=HPROGRAM_ORIG)
                   IOS = 0
                END IF
             END IF
#endif
             IF (TZSPLITFILE%CFORMAT=='LFI' .OR. TZSPLITFILE%CFORMAT=='LFICDF4') THEN
                SELECT CASE (YACTION)
                  CASE('READ')
                    call io_open_file_lfi(tzsplitfile,iresp)
                  CASE('WRITE')
                    call io_create_file_lfi(tzsplitfile,iresp)
                END SELECT
             ENDIF
             !
          ENDDO
       END IF


    END SELECT

    TPFILE%NMPICOMM = NMNH_COMM_WORLD

    KRESP = IOS

  CONTAINS
    FUNCTION SUFFIX(HEXT)

      CHARACTER(len=*)             :: HEXT
      CHARACTER(len=LEN(HEXT)+3)   :: SUFFIX

      WRITE(SUFFIX,'(A,i3.3)') TRIM(HEXT), ISP

    END FUNCTION SUFFIX

  END SUBROUTINE OPEN_ll


  SUBROUTINE CLOSE_ll(TPFILE,KRESP)
    USE MODD_IO_ll

    TYPE(TFILEDATA),  INTENT(IN)  :: TPFILE
    INTEGER,          INTENT(OUT) :: KRESP

    character(len=256)      :: yioerrmsg
    INTEGER                 :: IRESP

    CALL PRINT_MSG(NVERB_DEBUG,'IO','CLOSE_ll','closing '//TRIM(TPFILE%CNAME))

    IRESP       = 0

    IF (TPFILE%LMASTER) THEN
      IF (TPFILE%NLU/=-1 .AND. TPFILE%NLU/=NNULLUNIT) THEN
        CLOSE(UNIT=TPFILE%NLU, STATUS='KEEP', IOSTAT=IRESP, IOMSG=yioerrmsg)
      END IF
    END IF

    !Warning and not error or fatal if close fails to allow continuation of execution
    IF (IRESP/=0) CALL PRINT_MSG(NVERB_WARNING,'IO','CLOSE_ll','Problem when closing '//TRIM(TPFILE%CNAME)//': '//TRIM(YIOERRMSG))

    KRESP = IRESP

  END SUBROUTINE CLOSE_ll

END MODULE MODE_IO_ll


MODULE MODE_MSG
!
USE MODD_IO_ll, ONLY : NVERB_FATAL,NVERB_ERROR,NVERB_WARNING,NVERB_INFO,NVERB_DEBUG
!
IMPLICIT NONE
!
CONTAINS
!
SUBROUTINE PRINT_MSG(KVERB,HDOMAIN,HSUBR,HMSG)
!
USE ISO_FORTRAN_ENV, ONLY : ERROR_UNIT, OUTPUT_UNIT
!
USE MODD_CONF,   ONLY : CPROGRAM
USE MODD_IO_ll,  ONLY : NIO_VERB,NIO_ABORT_LEVEL,NGEN_VERB,NGEN_ABORT_LEVEL, &
                        LVERB_OUTLST, LVERB_STDOUT, LVERB_ALLPRC, TFILE_OUTPUTLISTING
USE MODD_LUNIT,  ONLY : TLUOUT0
USE MODD_VAR_ll, ONLY : IP, NMNH_COMM_WORLD
!
use modi_tools_c
!
!USE MODE_FM,     ONLY : IO_FILE_CLOSE_ll
!
INTEGER,         INTENT(IN) :: KVERB   !Verbosity level
CHARACTER(LEN=*),INTENT(IN) :: HDOMAIN !Domain/category of message
CHARACTER(LEN=*),INTENT(IN) :: HSUBR   !Subroutine/function name
CHARACTER(LEN=*),INTENT(IN) :: HMSG    !Message
!
CHARACTER(LEN=8)  :: YPRC
CHARACTER(LEN=9)  :: YPRE
CHARACTER(LEN=30) :: YSUBR
INTEGER :: IERR, IMAXVERB,IABORTLEVEL
INTEGER :: ILU
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
YPRC=''
IF (LVERB_ALLPRC) WRITE(YPRC,'( I8 )') IP-1
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
!
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
IF (LVERB_ALLPRC) THEN
  IF (GWRITE_STDOUT) WRITE(UNIT=OUTPUT_UNIT,FMT="(A8,': ',A9,A30,A)") ADJUSTL(YPRC),YPRE,YSUBR,HMSG
  IF (GWRITE_OUTLST) WRITE(UNIT=ILU,        FMT="(A8,': ',A9,A30,A)") ADJUSTL(YPRC),YPRE,YSUBR,HMSG
ELSE
  IF (GWRITE_STDOUT) WRITE(UNIT=OUTPUT_UNIT,FMT="(A9,A30,A)") YPRE,YSUBR,HMSG
  IF (GWRITE_OUTLST) WRITE(UNIT=ILU,        FMT="(A9,A30,A)") YPRE,YSUBR,HMSG
END IF
!
IF (KVERB<=IABORTLEVEL) THEN
  IF (IP==1) WRITE(UNIT=ERROR_UNIT,FMT=*) 'ABORT asked by application '//TRIM(CPROGRAM)
  IF (GWRITE_STDOUT) WRITE(UNIT=OUTPUT_UNIT,FMT=*) 'ABORT asked by application '//TRIM(CPROGRAM)
  IF (GWRITE_OUTLST) WRITE(UNIT=ILU,        FMT=*) 'ABORT asked by application '//TRIM(CPROGRAM)
#if 0
  !Problem: loop dependency between MODE_MSG and MODE_FM (IO_FILE_CLOSE_ll call PRINT_MSG)
  NIO_VERB = 0 !To not get further messages (ABORT should be the last for readability)
  IF (ILU>0) CALL IO_FILE_CLOSE_ll(TFILE_OUTPUTLISTING) !To flush it
#else
  IF (ILU>0) FLUSH(UNIT=ILU) !OK in F2003
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
END SUBROUTINE PRINT_MSG
!
END MODULE MODE_MSG
