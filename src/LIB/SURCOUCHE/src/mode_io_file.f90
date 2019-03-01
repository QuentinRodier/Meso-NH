!MNH_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author(s)
!  D. Gazen, P. Wautelet
! Modifications:
!  J. Escobar  19/08/2005: bug argument optinonel ACCESS --> YACCESS
!  J. Escobar  22/05/2008: bug mode SPECIFIC in OPEN_ll
!  J. Escobar  05/11/2009: allow JPMAX_UNIT=48 open files
!  J. Escobar  18/10/2010: bug with PGI compiler on ADJUSTL
!  P. Wautelet 04/02/2016: bug with DELIM='NONE' and GCC 5.2/5.3
!  D. Gazen    April 2016: change error message
!  P. Wautelet May 2016  : use netCDF Fortran module
!  P. Wautelet July 2016 : added type OUTBAK
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  J. Pianezze 01/08/2016: add LOASIS flag
!  P. Wautelet 29/10/2018: better detection of older MNH version numbers
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
!  P. Wautelet 07/02/2019: force TYPE to a known value for IO_FILE_ADD2LIST
!  P. Wautelet 14/02/2019: move UPCASE function to tools.f90
!  P. Wautelet 19/02/2019: simplification/restructuration/cleaning of open/close subroutines (TBCto be continued)
!  P. Wautelet 27/02/2019: use recursive calls to open/close DES files
!  P. Wautelet 27/02/2019: remove CLOSE_ll subroutine
!  P. Wautelet 01/03/2019: move open/close subroutines to mode_io_file.f90
!
!-----------------------------------------------------------------
module mode_io_file

use modd_io_ll, only: tfiledata

use mode_msg

implicit none

private

public :: IO_File_close_ll, IO_File_open_ll

interface IO_File_close_ll
  module procedure IO_File_close
end interface

interface IO_File_open_ll
  module procedure IO_File_open
end interface


contains


recursive SUBROUTINE IO_File_open(TPFILE,KRESP,HPOSITION,HSTATUS,HPROGRAM_ORIG)
!
USE MODD_CONF,             ONLY: CPROGRAM
USE MODD_IO_ll,            ONLY: LIO_NO_WRITE
!
USE MODE_IO_ll,            ONLY: GCONFIO
USE MODE_IO_MANAGE_STRUCT, ONLY: IO_FILE_ADD2LIST, IO_FILE_FIND_BYNAME
!
TYPE(TFILEDATA), POINTER, INTENT(INOUT)         :: TPFILE ! File structure
INTEGER,                  INTENT(OUT), OPTIONAL :: KRESP  ! Return code
CHARACTER(LEN=*),         INTENT(IN),  OPTIONAL :: HPOSITION
CHARACTER(LEN=*),         INTENT(IN),  OPTIONAL :: HSTATUS
CHARACTER(LEN=*),         INTENT(IN),  OPTIONAL :: HPROGRAM_ORIG !To emulate a file coming from this program
!
INTEGER                  :: IRESP
TYPE(TFILEDATA), POINTER :: TZFILE_DES
TYPE(TFILEDATA), POINTER :: TZFILE_DUMMY
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_File_open','opening '//TRIM(TPFILE%CNAME)//' for '//TRIM(TPFILE%CMODE)// &
               ' (filetype='//TRIM(TPFILE%CTYPE)//')')
!
IF (.NOT.ASSOCIATED(TPFILE)) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_File_open','TPFILE is not associated')
!
IF ( LIO_NO_WRITE .AND. TPFILE%CMODE == 'WRITE' .AND. TPFILE%CTYPE/='OUTPUTLISTING') THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_open','opening file '//TRIM(TPFILE%CNAME)// &
                                                      ' in write mode but LIO_NO_WRITE is set')
END IF
!
TZFILE_DES   => NULL()
TZFILE_DUMMY => NULL()
!
TPFILE%NOPEN         = TPFILE%NOPEN + 1
TPFILE%NOPEN_CURRENT = TPFILE%NOPEN_CURRENT + 1
!
IF (TPFILE%LOPENED) THEN
  CALL PRINT_MSG(NVERB_INFO,'IO','IO_File_open','file '//TRIM(TPFILE%CNAME)//' is already in open state')
  RETURN
END IF
!
TPFILE%LOPENED       = .TRUE.
!
!Check if file is in filelist
CALL IO_FILE_FIND_BYNAME(TRIM(TPFILE%CNAME),TZFILE_DUMMY,IRESP)
IF (IRESP/=0) CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_open','file '//TRIM(TPFILE%CNAME)//' not in filelist')
!
SELECT CASE(TPFILE%CTYPE)
  !Chemistry input files
  CASE('CHEMINPUT')
    CALL OPEN_ll(TPFILE,IRESP,HPOSITION='REWIND',HSTATUS='OLD',HMODE='GLOBAL')


  !Chemistry tabulation files
  CASE('CHEMTAB')
    CALL OPEN_ll(TPFILE,IRESP,HMODE='GLOBAL')


  !DES files
  CASE('DES')
    CALL OPEN_ll(TPFILE,IRESP,HDELIM='QUOTE')


  !GPS files
  CASE('GPS')
    CALL OPEN_ll(TPFILE,IRESP,HMODE='SPECIFIC')


  !Meteo files
  CASE('METEO')
   CALL OPEN_ll(TPFILE,IRESP,HMODE='GLOBAL')


  !Namelist files
  CASE('NML')
    CALL OPEN_ll(TPFILE,IRESP,HDELIM='QUOTE',HMODE='GLOBAL')


  !OUTPUTLISTING files
  CASE('OUTPUTLISTING')
    CALL OPEN_ll(TPFILE,IRESP,HMODE='GLOBAL')


  !SURFACE_DATA files
  CASE('SURFACE_DATA')
    CALL OPEN_ll(TPFILE,IRESP,HMODE='GLOBAL')


  !Text files
  CASE('TXT')
    CALL OPEN_ll(TPFILE,IRESP,HPOSITION=HPOSITION,HSTATUS=HSTATUS,HMODE='GLOBAL')


  !MesoNH files
  !Remark: 'MNH' is more general than MNHBACKUP and could be in fact a MNHBACKUP file
  CASE ('MNH', 'MNHBACKUP', 'MNHDIACHRONIC', 'MNHDIAG', 'MNHOUTPUT', 'PGD')
    if (.not.GCONFIO) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_File_open','SET_CONFIO_ll must be called before IO_File_open')
    !Do not open '.des' file if OUTPUT
    IF(TPFILE%CTYPE/='MNHOUTPUT' .AND. CPROGRAM/='LFICDF') THEN
      !OOLD=T because the file may already be in the list
      CALL IO_FILE_ADD2LIST(TZFILE_DES,TRIM(TPFILE%CNAME)//'.des','DES',TPFILE%CMODE,TPDATAFILE=TPFILE,OOLD=.TRUE.)
      CALL IO_File_open(TZFILE_DES,HPROGRAM_ORIG=HPROGRAM_ORIG)
    ENDIF

    CALL OPEN_ll(TPFILE,IRESP,HMODE='IO_ZSPLIT',HPROGRAM_ORIG=HPROGRAM_ORIG)

    call IO_File_check_format_exist( tpfile )

    call IO_File_open_format( tpfile )


  CASE DEFAULT
    call print_msg(NVERB_FATAL,'IO','IO_File_open','invalid type '//trim(tpfile%ctype)//' for file '//trim(tpfile%cname))
END SELECT
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_File_open


SUBROUTINE OPEN_ll(TPFILE, KRESP, HMODE, HSTATUS, HPOSITION, HDELIM, HPROGRAM_ORIG)

  use modd_io_ll,  only: ISNPROC, ISP, LVERB_ALLPRC, nio_rank, NNULLUNIT
  use modd_var_ll, only : nmnh_comm_world

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
              TPFILE%NMASTER_RANK  = nio_rank
              TPFILE%LMASTER       = (ISP == nio_rank)
              TPFILE%LMULTIMASTERS = .FALSE.
            END IF
          ELSE
            TPFILE%NMASTER_RANK  = nio_rank
            TPFILE%LMASTER       = (ISP == nio_rank)
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
       TPFILE%NMASTER_RANK  = nio_rank
       TPFILE%LMASTER       = (ISP == nio_rank)
       TPFILE%LMULTIMASTERS = .FALSE.

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
                END IF

                IF (YACTION == 'WRITE') THEN
                   ! Create netCDF File for writing
                   call io_create_file_nc4(TZSPLITFILE, hprogram_orig=HPROGRAM_ORIG)
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


recursive SUBROUTINE IO_File_close(TPFILE,KRESP,HPROGRAM_ORIG)
!
USE MODD_CONF,             ONLY: CPROGRAM
USE MODD_IO_ll,            ONLY: NNULLUNIT

use mode_io_file_lfi,      only: io_close_file_lfi
#if defined(MNH_IOCDF4)
use mode_io_file_nc4,      only: io_close_file_nc4
use mode_io_write_nc4,     only: io_write_coordvar_nc4
#endif
USE MODE_IO_MANAGE_STRUCT, ONLY: IO_FILE_FIND_BYNAME
!
TYPE(TFILEDATA),            INTENT(INOUT) :: TPFILE ! File structure
INTEGER,          OPTIONAL, INTENT(OUT)   :: KRESP  ! Return code
CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: HPROGRAM_ORIG !To emulate a file coming from this program
!
character(len=256)      :: yioerrmsg
INTEGER                 :: IRESP, JI
TYPE(TFILEDATA),POINTER :: TZFILE_DES
TYPE(TFILEDATA),POINTER :: TZFILE_IOZ
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_File_close','closing '//TRIM(TPFILE%CNAME))
!
IF (.NOT.TPFILE%LOPENED) THEN
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_close','trying to close a file not opened: '//TRIM(TPFILE%CNAME))
  RETURN
ENDIF
!
IF (TPFILE%NOPEN_CURRENT>1) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_File_close',TRIM(TPFILE%CNAME)// &
                 ': decrementing NOPEN_CURRENT (still opened after this call)')
  TPFILE%NOPEN_CURRENT = TPFILE%NOPEN_CURRENT - 1
  TPFILE%NCLOSE        = TPFILE%NCLOSE        + 1
  !
  DO JI = 1,TPFILE%NSUBFILES_IOZ
    TZFILE_IOZ => TPFILE%TFILES_IOZ(JI)%TFILE
    TZFILE_IOZ%NOPEN_CURRENT = TZFILE_IOZ%NOPEN_CURRENT - 1
    TZFILE_IOZ%NCLOSE        = TZFILE_IOZ%NCLOSE        + 1
  END DO
  !
  RETURN
END IF
!
SELECT CASE(TPFILE%CTYPE)
  CASE('CHEMINPUT','CHEMTAB','DES','GPS','METEO','NML','OUTPUTLISTING','SURFACE_DATA','TXT')
    IF (TPFILE%LMASTER) THEN
      IF (TPFILE%NLU/=-1 .AND. TPFILE%NLU/=NNULLUNIT) THEN
        CLOSE(UNIT=TPFILE%NLU, STATUS='KEEP', IOSTAT=IRESP, IOMSG=yioerrmsg)
      END IF
    END IF

    !Warning and not error or fatal if close fails to allow continuation of execution
    IF (IRESP/=0) CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_close','Problem when closing ' &
                                 //TRIM(TPFILE%CNAME)//': '//TRIM(YIOERRMSG))

    TPFILE%NLU = -1


  !MesoNH files
  !Remark: 'MNH' is more general than MNHBACKUP and could be in fact a MNHBACKUP file
  CASE ('MNH', 'MNHBACKUP', 'MNHDIACHRONIC', 'MNHDIAG', 'MNHOUTPUT', 'PGD')
    !Do not close (non-existing) '.des' file if OUTPUT
    IF(TPFILE%CTYPE/='OUTPUT' .AND. CPROGRAM/='LFICDF') THEN
      CALL IO_FILE_FIND_BYNAME(TRIM(TPFILE%CNAME)//'.des',TZFILE_DES,IRESP)
      IF (IRESP/=0) CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_close','file '//TRIM(TPFILE%CNAME)//'.des not in filelist')
      CALL IO_File_close(TZFILE_DES,KRESP=IRESP,HPROGRAM_ORIG=HPROGRAM_ORIG)
    ENDIF
    !
#if defined(MNH_IOCDF4)
    !Write coordinates variables in NetCDF file
    IF (TPFILE%CMODE == 'WRITE' .AND. (TPFILE%CFORMAT=='NETCDF4' .OR. TPFILE%CFORMAT=='LFICDF4')) THEN
      CALL IO_WRITE_COORDVAR_NC4(TPFILE,HPROGRAM_ORIG=HPROGRAM_ORIG)
    END IF
#endif

    if (tpfile%lmaster) then
      if (tpfile%cformat == 'LFI'     .or. tpfile%cformat == 'LFICDF4') call io_close_file_lfi(tpfile,iresp)
#if defined(MNH_IOCDF4)
      if (tpfile%cformat == 'NETCDF4' .or. tpfile%cformat == 'LFICDF4') call io_close_file_nc4(tpfile,iresp)
#endif
    end if
    !
    CALL IO_ADD2TRANSFER_LIST(TPFILE)
    !
    SUBFILES: DO JI = 1,TPFILE%NSUBFILES_IOZ
      TZFILE_IOZ => TPFILE%TFILES_IOZ(JI)%TFILE
      IF (.NOT.TZFILE_IOZ%LOPENED) &
        CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_close','file '//TRIM(TZFILE_IOZ%CNAME)//' is not opened')
      IF (TZFILE_IOZ%NOPEN_CURRENT/=1) &
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_close','file '//TRIM(TZFILE_IOZ%CNAME)//&
                       ' is currently opened 0 or several times (expected only 1)')
      TZFILE_IOZ%LOPENED       = .FALSE.
      TZFILE_IOZ%NOPEN_CURRENT = 0
      TZFILE_IOZ%NCLOSE        = TZFILE_IOZ%NCLOSE + 1
#if defined(MNH_IOCDF4)
      !Write coordinates variables in netCDF file
      IF (TZFILE_IOZ%CMODE == 'WRITE' .AND. (TZFILE_IOZ%CFORMAT=='NETCDF4' .OR. TZFILE_IOZ%CFORMAT=='LFICDF4')) THEN
        CALL IO_WRITE_COORDVAR_NC4(TZFILE_IOZ,HPROGRAM_ORIG=HPROGRAM_ORIG)
      END IF
#endif
      IF (TZFILE_IOZ%LMASTER) THEN
        if (tzfile_ioz%cformat == 'LFI'     .or. tzfile_ioz%cformat == 'LFICDF4') call io_close_file_lfi(tzfile_ioz,iresp)
#if defined(MNH_IOCDF4)
        if (tzfile_ioz%cformat == 'NETCDF4' .or. tzfile_ioz%cformat == 'LFICDF4') call io_close_file_nc4(tzfile_ioz,iresp)
#endif
      END IF
    END DO SUBFILES


  CASE DEFAULT
    call print_msg(NVERB_FATAL,'IO','IO_File_close','invalid type '//trim(tpfile%ctype)//' for file '//trim(tpfile%cname))
END SELECT
!
TPFILE%LOPENED       = .FALSE.
TPFILE%NOPEN_CURRENT = 0
TPFILE%NCLOSE        = TPFILE%NCLOSE + 1
!
IF (PRESENT(KRESP)) KRESP=IRESP
!
END SUBROUTINE IO_File_close


subroutine IO_ADD2TRANSFER_LIST(TPFILE)

USE MODD_CONF,  ONLY : CPROGRAM

USE MODI_SYSTEM_MNH

TYPE(TFILEDATA), INTENT(INOUT) :: TPFILE ! File structure

CHARACTER(len=:),allocatable :: YFILEM  ! name of the file
CHARACTER(len=:),allocatable :: YCPIO
CHARACTER(len=:),allocatable :: YTRANS
CHARACTER(LEN=100)           :: YCOMMAND
INTEGER, SAVE                :: ICPT = 0

YFILEM  = TPFILE%CNAME

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_ADD2TRANSFER_LIST','called for '//TRIM(YFILEM))

IF (TPFILE%LMASTER .AND. CPROGRAM/='LFICDF') THEN
  !! Write in pipe
#if defined(MNH_SX5)
  YTRANS='nectransfer.x'
#else
  YTRANS='xtransfer.x'
#endif

  SELECT CASE (TPFILE%NLFITYPE)
    CASE(:-1,3:)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_ADD2TRANSFER_LIST',TRIM(YFILEM)//': incorrect NLFITYPE')
    CASE(0)
      YCPIO='NIL'
    CASE(1)
      YCPIO='MESONH'
    CASE(2)
      CALL PRINT_MSG(NVERB_INFO,'IO','IO_ADD2TRANSFER_LIST','file '//TRIM(YFILEM)//' not transferred')
  END SELECT

  if (TPFILE%NLFITYPE==0 .or. TPFILE%NLFITYPE==1) then
    ICPT=ICPT+1
    WRITE (YCOMMAND,'(A," ",A," ",A," >> OUTPUT_TRANSFER",I3.3,"  2>&1 &")') YTRANS,YCPIO,TRIM(YFILEM),ICPT
    CALL PRINT_MSG(NVERB_INFO,'IO','IO_ADD2TRANSFER_LIST','YCOMMAND='//TRIM(YCOMMAND))
    CALL SYSTEM_MNH(YCOMMAND)
  end if
END IF

end subroutine IO_ADD2TRANSFER_LIST


subroutine IO_File_check_format_exist( tpfile )

type(tfiledata), intent(inout) :: tpfile ! File structure

logical :: gexist_lfi, gexist_nc4


call Print_msg( NVERB_DEBUG, 'IO', 'IO_File_check_format_exist', 'called for '//TRIM(tpfile%cname) )

IF (TPFILE%LMASTER) THEN
  ! Proc I/O case
  INQUIRE(FILE=TRIM(TPFILE%CNAME)//'.lfi',EXIST=GEXIST_LFI)
  INQUIRE(FILE=TRIM(TPFILE%CNAME)//'.nc', EXIST=GEXIST_NC4)

  MODE: if ( tpfile%cmode == 'READ' ) then
    IF (.NOT.GEXIST_LFI .AND. .NOT.GEXIST_NC4) &
      CALL PRINT_MSG(NVERB_FATAL,'IO','IO_File_check_format_exist',TRIM(TPFILE%CNAME)//': no .nc or .lfi file')

    SELECT CASE (TRIM(TPFILE%CFORMAT))
      CASE ('NETCDF4')
        IF (.NOT.GEXIST_NC4 .AND. GEXIST_LFI) THEN
          CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_check_format_exist',TRIM(TPFILE%CNAME)// &
                         ': .nc file does not exist but .lfi exists -> forced to LFI')
          TPFILE%CFORMAT='LFI'
        END IF
      CASE ('LFI')
        IF (.NOT.GEXIST_LFI .AND. GEXIST_NC4) THEN
          CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_check_format_exist',TRIM(TPFILE%CNAME)// &
                         ': .lfi file does not exist but .nc exists -> forced to NETCDF4')
          TPFILE%CFORMAT='NETCDF4'
        END IF
      CASE ('LFICDF4')
        IF (GEXIST_NC4) THEN
          CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_check_format_exist',TRIM(TPFILE%CNAME)// &
                         ': LFICDF4 format is not allowed in READ mode -> forced to NETCDF4')
          TPFILE%CFORMAT='NETCDF4'
        ELSE IF (GEXIST_LFI) THEN
          CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_check_format_exist',TRIM(TPFILE%CNAME)// &
                         ': LFICDF4 format is not allowed in READ mode -> forced to LFI')
          TPFILE%CFORMAT='LFI'
        END IF
      CASE DEFAULT
        IF (GEXIST_NC4) THEN
          CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_check_format_exist',TRIM(TPFILE%CNAME)// &
                         ': invalid fileformat (-> forced to NETCDF4 if no abort)')
          TPFILE%CFORMAT='NETCDF4'
        ELSE IF (GEXIST_LFI) THEN
          CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_check_format_exist',TRIM(TPFILE%CNAME)// &
                         ': invalid fileformat (-> forced to LFI if no abort)')
          TPFILE%CFORMAT='LFI'
        END IF
    END SELECT
  end if MODE
END IF

end subroutine IO_File_check_format_exist


subroutine IO_File_open_format( tpfile, hprogram_orig )

#if defined(MNH_IOCDF4)
use mode_io_file_nc4, only: io_create_file_nc4, io_open_file_nc4
#endif
use mode_io_file_lfi, only: io_create_file_lfi, io_open_file_lfi

type(tfiledata),            intent(inout) :: tpfile ! File structure
character(len=*), optional, intent(in)    :: hprogram_orig !To emulate a file coming from this program

integer :: iresp


call Print_msg( NVERB_DEBUG, 'IO', 'IO_File_open_format', 'called for '//TRIM(tpfile%cname) )

#if defined(MNH_IOCDF4)
    IF (TPFILE%CFORMAT=='NETCDF4' .OR. TPFILE%CFORMAT=='LFICDF4') THEN
      SELECT CASE (TPFILE%CMODE)
        CASE('READ')
          call io_open_file_nc4(tpfile)
        CASE('WRITE')
          call io_create_file_nc4(TPFILE, hprogram_orig=HPROGRAM_ORIG)
      END SELECT
    END IF
#endif

    IF (TPFILE%CFORMAT=='LFI' .OR. TPFILE%CFORMAT=='LFICDF4') THEN
      SELECT CASE (TPFILE%CMODE)
        CASE('READ')
          call io_open_file_lfi(tpfile,iresp)
        CASE('WRITE')
          call io_create_file_lfi(tpfile,iresp)
      END SELECT
    END IF

end subroutine IO_File_open_format

end module mode_io_file
