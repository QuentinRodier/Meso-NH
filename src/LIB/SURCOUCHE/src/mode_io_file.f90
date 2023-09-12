!MNH_LIC Copyright 1994-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author(s)
!  D. Gazen, P. Wautelet
! Modifications:
!  J. Escobar  19/08/2005: bug argument optinonel ACCESS --> YACCESS
!  J. Escobar  22/05/2008: bug mode SPECIFIC in IO_File_doopen
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
!  P. Wautelet 06/02/2019: simplify IO_File_doopen and do somme assignments at a more logical place
!  P. Wautelet 07/02/2019: remove OPARALLELIO argument from open and close files subroutines
!                          (nsubfiles_ioz is now determined in IO_File_add2list)
!  P. Wautelet 07/02/2019: force TYPE to a known value for IO_File_add2list
!  P. Wautelet 14/02/2019: move UPCASE function to tools.f90
!  P. Wautelet 19/02/2019: simplification/restructuration/cleaning of open/close subroutines (TBCto be continued)
!  P. Wautelet 27/02/2019: use recursive calls to open/close DES files
!  P. Wautelet 27/02/2019: remove CLOSE_ll subroutine
!  P. Wautelet 01/03/2019: move open/close subroutines to mode_io_file.f90
!  P. Wautelet 05/03/2019: rename IO subroutines and modules
!  P. Wautelet 12/03/2019: simplify opening of IO split files
!  P. Wautelet 05/09/2019: disable IO_Coordvar_write_nc4 for Z-split files
!  P. Wautelet 01/10/2020: bugfix: add missing initializations for IRESP
!  P. Wautelet 19/08/2022: bugfix: IO_File_check_format_exist: broadcast cformat if changed
!  P. Wautelet 13/01/2023: IO_File_close: add optional dummy argument TPDTMODELN to force written model time
!-----------------------------------------------------------------
module mode_io_file

use modd_io, only: tfiledata

use mode_msg

implicit none

private

public :: IO_File_close, IO_File_open


contains


recursive SUBROUTINE IO_File_open(TPFILE,KRESP,kmasterrank, HPOSITION,HSTATUS,HPROGRAM_ORIG)
!
USE MODD_CONF,             ONLY: CPROGRAM
USE MODD_IO,               ONLY: ISNPROC, LIO_NO_WRITE
!
use mode_io,               only: gconfio
use mode_io_manage_struct, only: IO_File_add2list, IO_File_find_byname
use mode_io_tools,         only: IO_Rank_master_get
!
TYPE(TFILEDATA),  POINTER,  INTENT(INOUT) :: TPFILE ! File structure
INTEGER,          optional, INTENT(OUT)   :: KRESP  ! Return code
integer,          optional, intent(in)    :: kmasterrank !Rank of the master process
CHARACTER(LEN=*), optional, INTENT(IN)    :: HPOSITION
CHARACTER(LEN=*), optional, INTENT(IN)    :: HSTATUS
CHARACTER(LEN=*), optional, INTENT(IN)    :: HPROGRAM_ORIG !To emulate a file coming from this program
!
CHARACTER(len=5)         :: YFILE
INTEGER                  :: IFILE, IRANK_PROCIO
INTEGER                  :: IRESP
TYPE(TFILEDATA), POINTER :: TZFILE_DES
TYPE(TFILEDATA), POINTER :: TZFILE_DUMMY
TYPE(TFILEDATA), POINTER :: TZFILE_SPLIT
!
iresp = 0
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
TZFILE_SPLIT => NULL()
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
CALL IO_File_find_byname(TRIM(TPFILE%CNAME),TZFILE_DUMMY,IRESP)
IF (IRESP/=0) CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_open','file '//TRIM(TPFILE%CNAME)//' not in filelist')
!
SELECT CASE(TPFILE%CTYPE)
  !Chemistry input files
  CASE('CHEMINPUT')
    CALL IO_File_doopen(TPFILE,IRESP,HPOSITION='REWIND',HSTATUS='OLD',HMODE='GLOBAL')


  !Chemistry tabulation files
  CASE('CHEMTAB')
    CALL IO_File_doopen(TPFILE,IRESP,HMODE='GLOBAL')


  !DES files
  CASE('DES')
    CALL IO_File_doopen(TPFILE,IRESP,HDELIM='QUOTE')


  !GPS files
  CASE('GPS')
    CALL IO_File_doopen(TPFILE,IRESP,HMODE='SPECIFIC')


  !Meteo files
  CASE('METEO')
   CALL IO_File_doopen(TPFILE,IRESP,HMODE='GLOBAL')


  !Namelist files
  CASE('NML')
    CALL IO_File_doopen(TPFILE,IRESP,HDELIM='QUOTE',HMODE='GLOBAL')


  !OUTPUTLISTING files
  CASE('OUTPUTLISTING')
    CALL IO_File_doopen(TPFILE,IRESP,HMODE='GLOBAL')


  !SURFACE_DATA files
  CASE('SURFACE_DATA')
    CALL IO_File_doopen(TPFILE,IRESP,HMODE='GLOBAL')


  !Text files
  CASE('TXT')
    CALL IO_File_doopen(TPFILE,IRESP,HPOSITION=HPOSITION,HSTATUS=HSTATUS,HMODE='GLOBAL')


  !MesoNH files
  !Remark: 'MNH' is more general than MNHBACKUP and could be in fact a MNHBACKUP file
  CASE ('MNH', 'MNHBACKUP', 'MNHDIACHRONIC', 'MNHDIAG', 'MNHOUTPUT', 'PGD')
    if (.not.GCONFIO) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_File_open','IO_Config_set must be called before IO_File_open')

    !Do not open '.des' file if OUTPUT or if is a "subfile" (tmainfile is associated)
    IF(TPFILE%CTYPE/='MNHOUTPUT' .AND. CPROGRAM/='LFICDF' .and. .not.associated(tpfile%tmainfile) ) THEN
      !OOLD=T because the file may already be in the list
      CALL IO_File_add2list(TZFILE_DES,TRIM(TPFILE%CNAME)//'.des','DES',TPFILE%CMODE,TPDATAFILE=TPFILE,OOLD=.TRUE.)
      CALL IO_File_open(TZFILE_DES,HPROGRAM_ORIG=HPROGRAM_ORIG)
    ENDIF

    !Manage split files
    IF (TPFILE%NSUBFILES_IOZ > 0) THEN
      IF (.NOT.ALLOCATED(TPFILE%TFILES_IOZ)) THEN
        ALLOCATE(TPFILE%TFILES_IOZ(TPFILE%NSUBFILES_IOZ))
      ELSE IF ( SIZE(TPFILE%TFILES_IOZ) /= TPFILE%NSUBFILES_IOZ ) THEN
        CALL PRINT_MSG(NVERB_FATAL,'IO','IO_File_open','SIZE(TPFILE%TFILES_IOZ) /= TPFILE%NSUBFILES_IOZ for '//TRIM(TPFILE%CNAME))
      END IF

      DO IFILE=1,TPFILE%NSUBFILES_IOZ
        IRANK_PROCIO = 1 + IO_Rank_master_get( IFILE-1, ISNPROC, TPFILE%NSUBFILES_IOZ )
        WRITE(YFILE ,'(".Z",i3.3)') IFILE

        tzfile_split => null()
        CALL IO_File_find_byname(TRIM(TPFILE%CNAME)//TRIM(YFILE),TZFILE_SPLIT,IRESP)

        IF (IRESP/=0) THEN !File not yet in filelist => add it (nothing to do if already in list)
          IF (ALLOCATED(TPFILE%CDIRNAME)) THEN
            call IO_File_add2list( tzfile_split, trim(tpfile%cname)//trim(yfile), tpfile%ctype, tpfile%cmode,            &
                                   hdirname = tpfile%cdirname,                                                           &
                                   klfinprar = tpfile%nlfinprar, klfitype = tpfile%nlfitype, klfiverb = tpfile%nlfiverb, &
                                   hformat = tpfile%cformat,                                                             &
                                   osplit_ioz=.false. )
          ELSE
            call IO_File_add2list( tzfile_split, trim(tpfile%cname)//trim(yfile), tpfile%ctype, tpfile%cmode,            &
                                   klfinprar = tpfile%nlfinprar, klfitype = tpfile%nlfitype, klfiverb = tpfile%nlfiverb, &
                                   hformat = tpfile%cformat,                                                             &
                                   osplit_ioz=.false. )
           END IF

          TZFILE_SPLIT%TMAINFILE => TPFILE
        END IF

        TPFILE%TFILES_IOZ(IFILE)%TFILE => TZFILE_SPLIT

        CALL IO_File_open(TZFILE_SPLIT, kmasterrank=IRANK_PROCIO,HPROGRAM_ORIG=HPROGRAM_ORIG)
      END DO
    end if

    CALL IO_File_doopen(TPFILE,IRESP,kmasterrank=kmasterrank,HMODE='MASTER',HPROGRAM_ORIG=HPROGRAM_ORIG)


  CASE DEFAULT
    call print_msg(NVERB_FATAL,'IO','IO_File_open','invalid type '//trim(tpfile%ctype)//' for file '//trim(tpfile%cname))
END SELECT
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_File_open


SUBROUTINE IO_File_doopen(TPFILE, KRESP, kmasterrank, HMODE, HSTATUS, HPOSITION, HDELIM, HPROGRAM_ORIG)

use modd_io,     only: ISP, LVERB_ALLPRC, nio_rank, NNULLUNIT
use modd_var_ll, only: nmnh_comm_world

use mode_tools,  only: upcase

TYPE(TFILEDATA), pointer,   INTENT(INOUT) :: TPFILE
INTEGER,                    INTENT(OUT)   :: KRESP
integer,          optional, intent(in)    :: kmasterrank !Rank of the master process
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
CHARACTER(len=20)            :: YSTATUS
CHARACTER(len=20)            :: YPOSITION
CHARACTER(len=20)            :: YDELIM
CHARACTER(len=20)            :: YACTION
CHARACTER(len=20)            :: YMODE
CHARACTER(LEN=256)           :: YIOERRMSG
CHARACTER(LEN=:),ALLOCATABLE :: YPREFILENAME !To store the directory + filename
integer                      :: imasterrank
INTEGER                      :: irecl
INTEGER                      :: IOS

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_File_doopen','opening '//TRIM(TPFILE%CNAME)//' for '//TRIM(TPFILE%CMODE))

IOS = 0

if ( present( kmasterrank ) ) then
  imasterrank = kmasterrank
else
  imasterrank = nio_rank
end if

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
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_doopen','action='//TRIM(YACTION)//' not supported')
  RETURN
END IF

if (       trim(ymode) /= 'GLOBAL'    .and. trim(ymode) /= 'SPECIFIC' &
     .and. trim(ymode) /= 'IO_ZSPLIT' .and. trim(ymode) /= 'MASTER'   ) then
  KRESP = 99
  TPFILE%NLU = -1
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_doopen','ymode='//TRIM(YMODE)//' not supported')
  RETURN
end if

IF (PRESENT(HSTATUS)) THEN
  YSTATUS=HSTATUS
ELSE
  YSTATUS='UNKNOWN'
ENDIF

IF (TPFILE%NRECL == -1) THEN
  irecl = RECL_DEF
ELSE
  irecl = TPFILE%NRECL
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

!NMPICOMM must be set before this select case (necessary for case MASTER)
TPFILE%NMPICOMM = NMNH_COMM_WORLD

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
          TPFILE%NMASTER_RANK  = imasterrank
          TPFILE%LMASTER       = (ISP == imasterrank)
          TPFILE%LMULTIMASTERS = .FALSE.
        END IF
      ELSE
        TPFILE%NMASTER_RANK  = imasterrank
        TPFILE%LMASTER       = (ISP == imasterrank)
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
             RECL=irecl,             &
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
                 RECL=irecl,             &
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
                 RECL=irecl,             &
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
               RECL=irecl,             &
               POSITION=YPOSITION,     &
               ACTION=YACTION)
        ENDIF
      ENDIF

      IF ( IOS /= 0 ) &
        CALL PRINT_MSG(NVERB_FATAL,'IO','IO_File_doopen','Problem when opening '//TRIM(YPREFILENAME)//': '//TRIM(YIOERRMSG))
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
           RECL=irecl,                            &
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
             RECL=irecl,                             &
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
             RECL=irecl,                             &
             POSITION=YPOSITION,                     &
             ACTION=YACTION,                         &
             DELIM=YDELIM)
      ENDIF
    ENDIF

    IF ( IOS /= 0 ) &
      CALL PRINT_MSG(NVERB_FATAL,'IO','IO_File_doopen','Problem when opening '//TRIM(YPREFILENAME)//': '//TRIM(YIOERRMSG))


  case ( 'MASTER' )
    tpfile%nmaster_rank  = imasterrank
    tpfile%lmaster       = (isp == imasterrank)
    tpfile%lmultimasters = .false.

    call IO_File_check_format_exist( tpfile )

    call IO_File_open_format( tpfile, hprogram_orig=hprogram_orig )
END SELECT

KRESP = IOS

CONTAINS

FUNCTION SUFFIX(HEXT)

  CHARACTER(len=*)             :: HEXT
  CHARACTER(len=LEN(HEXT)+3)   :: SUFFIX

  if ( isp > 999 ) call Print_msg(NVERB_FATAL,'IO','IO_File_doopen','SUFFIX: ISP>999')

  WRITE(SUFFIX,'(A,i3.3)') TRIM(HEXT), ISP

END FUNCTION SUFFIX

END SUBROUTINE IO_File_doopen


recursive SUBROUTINE IO_File_close( TPFILE, KRESP, HPROGRAM_ORIG, TPDTMODELN )
!
use modd_conf,             only: cprogram
use modd_io,               only: nnullunit
use modd_type_date,        only: date_time

use mode_io_file_lfi,      only: IO_File_close_lfi
#ifdef MNH_IOCDF4
use mode_io_file_nc4,      only: IO_File_close_nc4
use mode_io_write_nc4,     only: IO_Coordvar_write_nc4
#endif
use mode_io_manage_struct, only: IO_File_find_byname
!
TYPE(TFILEDATA),            INTENT(INOUT) :: TPFILE ! File structure
INTEGER,          OPTIONAL, INTENT(OUT)   :: KRESP  ! Return code
CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: HPROGRAM_ORIG !To emulate a file coming from this program
TYPE(DATE_TIME),  OPTIONAL, INTENT(IN)    :: TPDTMODELN    !Time of model (to force model date written in file)
character(len=256)      :: yioerrmsg
INTEGER                 :: IRESP, JI
TYPE(TFILEDATA),POINTER :: TZFILE_DES
TYPE(TFILEDATA),POINTER :: TZFILE_IOZ
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_File_close','closing '//TRIM(TPFILE%CNAME))

iresp = 0

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
    IF(TPFILE%CTYPE/='MNHOUTPUT' .AND. CPROGRAM/='LFICDF') THEN
      CALL IO_File_find_byname(TRIM(TPFILE%CNAME)//'.des',TZFILE_DES,IRESP)
      IF (IRESP/=0) CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_close','file '//TRIM(TPFILE%CNAME)//'.des not in filelist')
      CALL IO_File_close(TZFILE_DES,KRESP=IRESP,HPROGRAM_ORIG=HPROGRAM_ORIG)
    ENDIF
    !
#ifdef MNH_IOCDF4
    !Write coordinates variables in NetCDF file
    IF (TPFILE%CMODE == 'WRITE' .AND. (TPFILE%CFORMAT=='NETCDF4' .OR. TPFILE%CFORMAT=='LFICDF4')) THEN
      CALL IO_Coordvar_write_nc4( TPFILE, HPROGRAM_ORIG = HPROGRAM_ORIG, TPDTMODELN = TPDTMODELN )
    END IF
#endif

    if (tpfile%lmaster) then
      if (tpfile%cformat == 'LFI'     .or. tpfile%cformat == 'LFICDF4') call IO_File_close_lfi(tpfile,iresp)
#ifdef MNH_IOCDF4
      if (tpfile%cformat == 'NETCDF4' .or. tpfile%cformat == 'LFICDF4') call IO_File_close_nc4(tpfile,iresp)
#endif
    end if
    !
    CALL IO_Transfer_list_addto(TPFILE)
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
#ifdef MNH_IOCDF4
!Remark: IO_Coordvar_write_nc4 disabled (for the moment) for Z-split files
!        because it introduce a serialization due to MPI communications inside the call
!        WARNING: if uncommented, please modify IO_Coordvar_write_nc4 to enable block concerning gdealloc
!       !Write coordinates variables in netCDF file
!       IF (TZFILE_IOZ%CMODE == 'WRITE' .AND. (TZFILE_IOZ%CFORMAT=='NETCDF4' .OR. TZFILE_IOZ%CFORMAT=='LFICDF4')) THEN
!         CALL IO_Coordvar_write_nc4(TZFILE_IOZ,HPROGRAM_ORIG=HPROGRAM_ORIG)
!       END IF
#endif
      IF (TZFILE_IOZ%LMASTER) THEN
        if (tzfile_ioz%cformat == 'LFI'     .or. tzfile_ioz%cformat == 'LFICDF4') call IO_File_close_lfi(tzfile_ioz,iresp)
#ifdef MNH_IOCDF4
        if (tzfile_ioz%cformat == 'NETCDF4' .or. tzfile_ioz%cformat == 'LFICDF4') call IO_File_close_nc4(tzfile_ioz,iresp)
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


subroutine IO_Transfer_list_addto(TPFILE)

USE MODD_CONF,  ONLY: CPROGRAM

USE MODI_SYSTEM_MNH

TYPE(TFILEDATA), INTENT(INOUT) :: TPFILE ! File structure

CHARACTER(len=:),allocatable :: YFILEM  ! name of the file
CHARACTER(len=:),allocatable :: YCPIO
CHARACTER(len=:),allocatable :: YTRANS
CHARACTER(LEN=100)           :: YCOMMAND
INTEGER, SAVE                :: ICPT = 0

YFILEM  = TPFILE%CNAME

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Transfer_list_addto','called for '//TRIM(YFILEM))

IF (TPFILE%LMASTER .AND. CPROGRAM/='LFICDF') THEN
  !! Write in pipe
#if defined(MNH_SX5)
  YTRANS='nectransfer.x'
#else
  YTRANS='xtransfer.x'
#endif

  SELECT CASE (TPFILE%NLFITYPE)
    CASE(:-1,3:)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Transfer_list_addto',TRIM(YFILEM)//': incorrect NLFITYPE')
    CASE(0)
      YCPIO='NIL'
    CASE(1)
      YCPIO='MESONH'
    CASE(2)
      CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Transfer_list_addto','file '//TRIM(YFILEM)//' not transferred')
  END SELECT

  if (TPFILE%NLFITYPE==0 .or. TPFILE%NLFITYPE==1) then
    ICPT=ICPT+1
    WRITE (YCOMMAND,'(A," ",A," ",A," >> OUTPUT_TRANSFER",I3.3,"  2>&1 &")') YTRANS,YCPIO,TRIM(YFILEM),ICPT
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Transfer_list_addto','YCOMMAND='//TRIM(YCOMMAND))
    CALL SYSTEM_MNH(YCOMMAND)
  end if
END IF

end subroutine IO_Transfer_list_addto


subroutine IO_File_check_format_exist( tpfile )
use modd_mpif

type(tfiledata), intent(inout) :: tpfile ! File structure

integer :: ierr
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

if ( tpfile%cmode == 'READ' ) &
  call MPI_BCAST( tpfile%cformat, Len( tpfile%cformat ), MPI_CHARACTER, tpfile%nmaster_rank - 1, tpfile%nmpicomm, ierr )

end subroutine IO_File_check_format_exist


subroutine IO_File_open_format( tpfile, hprogram_orig )

#ifdef MNH_IOCDF4
use mode_io_file_nc4, only: IO_File_create_nc4, IO_File_open_nc4
#endif
use mode_io_file_lfi, only: IO_File_create_lfi, IO_File_open_lfi

type(tfiledata),            intent(inout) :: tpfile ! File structure
character(len=*), optional, intent(in)    :: hprogram_orig !To emulate a file coming from this program

integer :: iresp


call Print_msg( NVERB_DEBUG, 'IO', 'IO_File_open_format', 'called for '//TRIM(tpfile%cname) )

#ifdef MNH_IOCDF4
    IF (TPFILE%CFORMAT=='NETCDF4' .OR. TPFILE%CFORMAT=='LFICDF4') THEN
      SELECT CASE (TPFILE%CMODE)
        CASE('READ')
          call IO_File_open_nc4(tpfile)
        CASE('WRITE')
          call IO_File_create_nc4(TPFILE, hprogram_orig=HPROGRAM_ORIG)
      END SELECT
    END IF
#endif

    IF (TPFILE%CFORMAT=='LFI' .OR. TPFILE%CFORMAT=='LFICDF4') THEN
      SELECT CASE (TPFILE%CMODE)
        CASE('READ')
          call IO_File_open_lfi(tpfile,iresp)
        CASE('WRITE')
          call IO_File_create_lfi(tpfile,iresp)
      END SELECT
    END IF

end subroutine IO_File_open_format

end module mode_io_file
