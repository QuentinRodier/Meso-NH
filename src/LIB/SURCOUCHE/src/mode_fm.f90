!MNH_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author(s):
!
! Modifications:
!  D. Gazen    April 2016: change error message
!  P. Wautelet May 2016  : use NetCDF Fortran module
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 29/10/2018: better detection of older MNH version numbers
!  P. Wautelet 13/12/2018: moved some operations to new mode_io_*_nc4 modules
!  P. Wautelet 10/01/2019: use NEWUNIT argument of OPEN + move management
!                          of NNCID and NLFIFLU to the nc4 and lfi subroutines
!  P. Wautelet 21/01/2019: add LIO_ALLOW_NO_BACKUP and LIO_NO_WRITE to modd_io_ll
!                          to allow to disable writes (for bench purposes)
!  P. Wautelet 06/02/2019: simplify OPEN_ll and do somme assignments at a more logical place
!  P. Wautelet 07/02/2019: force TYPE to a known value for IO_FILE_ADD2LIST
!  P. Wautelet 07/02/2019: remove OPARALLELIO argument from open and close files subroutines
!                          (nsubfiles_ioz is now determined in IO_FILE_ADD2LIST)
!  P. Wautelet 19/02/2019: simplification/restructuration/cleaning of open/close subroutines (TBCto be continued)
!  P. Wautelet 27/02/2019: use recursive calls to open/close DES files
!-----------------------------------------------------------------

MODULE MODE_FM
USE MODE_MSG

IMPLICIT NONE 

PRIVATE 

PUBLIC SET_FMPACK_ll
PUBLIC IO_FILE_OPEN_ll, IO_FILE_CLOSE_ll

CONTAINS 

SUBROUTINE SET_FMPACK_ll(O1D,O2D,OPACK)
USE MODD_IO_ll,  ONLY: LPACK, L1D, L2D
USE MODD_VAR_ll, ONLY: IP

IMPLICIT NONE 

LOGICAL, INTENT(IN) :: O1D,O2D,OPACK

LPACK = OPACK
L1D   = O1D
L2D   = O2D

IF ( IP == 1 ) PRINT *,'INIT L1D,L2D,LPACK = ',L1D,L2D,LPACK

END SUBROUTINE SET_FMPACK_ll

recursive SUBROUTINE IO_FILE_OPEN_ll(TPFILE,KRESP,HPOSITION,HSTATUS,HPROGRAM_ORIG)
!
USE MODD_CONF,             ONLY: CPROGRAM
USE MODD_IO_ll,            ONLY: LIO_NO_WRITE, TFILEDATA
!
USE MODE_IO_ll,            ONLY: GCONFIO, OPEN_ll
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
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_FILE_OPEN_ll','opening '//TRIM(TPFILE%CNAME)//' for '//TRIM(TPFILE%CMODE)// &
               ' (filetype='//TRIM(TPFILE%CTYPE)//')')
!
IF (.NOT.ASSOCIATED(TPFILE)) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_FILE_OPEN_ll','TPFILE is not associated')
!
IF ( LIO_NO_WRITE .AND. TPFILE%CMODE == 'WRITE' .AND. TPFILE%CTYPE/='OUTPUTLISTING') THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_FILE_OPEN_ll','opening file '//TRIM(TPFILE%CNAME)// &
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
  CALL PRINT_MSG(NVERB_INFO,'IO','IO_FILE_OPEN_ll','file '//TRIM(TPFILE%CNAME)//' is already in open state')
  RETURN
END IF
!
TPFILE%LOPENED       = .TRUE.
!
!Check if file is in filelist
CALL IO_FILE_FIND_BYNAME(TRIM(TPFILE%CNAME),TZFILE_DUMMY,IRESP)
IF (IRESP/=0) CALL PRINT_MSG(NVERB_ERROR,'IO','IO_FILE_OPEN_ll','file '//TRIM(TPFILE%CNAME)//' not in filelist')
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
    if (.not.GCONFIO) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_FILE_OPEN_ll','SET_CONFIO_ll must be called before IO_FILE_OPEN_ll')
    !Do not open '.des' file if OUTPUT
    IF(TPFILE%CTYPE/='MNHOUTPUT' .AND. CPROGRAM/='LFICDF') THEN
      !OOLD=T because the file may already be in the list
      CALL IO_FILE_ADD2LIST(TZFILE_DES,TRIM(TPFILE%CNAME)//'.des','DES',TPFILE%CMODE,TPDATAFILE=TPFILE,OOLD=.TRUE.)
      CALL IO_FILE_OPEN_ll(TZFILE_DES,HPROGRAM_ORIG=HPROGRAM_ORIG)
    ENDIF
    !
    CALL FMOPEN_ll(TPFILE,IRESP,HPROGRAM_ORIG=HPROGRAM_ORIG)


  CASE DEFAULT
    call print_msg(NVERB_FATAL,'IO','IO_FILE_OPEN_ll','invalid type '//trim(tpfile%ctype)//' for file '//trim(tpfile%cname))
END SELECT
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_FILE_OPEN_ll


SUBROUTINE FMOPEN_ll(TPFILE,KRESP,HPROGRAM_ORIG)

USE MODD_IO_ll,       ONLY: TFILEDATA

#if defined(MNH_IOCDF4)
use mode_io_file_nc4, only: io_create_file_nc4, io_open_file_nc4
#endif
use mode_io_file_lfi, only: io_create_file_lfi, io_open_file_lfi
USE MODE_IO_ll,       ONLY: OPEN_ll, GCONFIO

TYPE(TFILEDATA),            INTENT(INOUT) :: TPFILE ! File structure
INTEGER,                    INTENT(OUT)   :: KRESP  ! return-code
CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: HPROGRAM_ORIG !To emulate a file coming from this program
!
!   Local variables
!
INTEGER               :: IRESP
CHARACTER(LEN=7)      :: YACTION ! Action upon the file ('READ' or 'WRITE')
CHARACTER(LEN=8)      :: YRESP
LOGICAL               :: GEXIST_LFI, GEXIST_NC4

YACTION = TPFILE%CMODE

CALL PRINT_MSG(NVERB_DEBUG,'IO','FMOPEN_ll','opening '//TRIM(TPFILE%CNAME)//' for '//TRIM(YACTION))

IF (.NOT. GCONFIO) THEN
   PRINT *, 'FMOPEN_ll Aborting... Please, ensure to call SET_CONFIO_ll before &
        &the first FMOPEN_ll call.'
   STOP
END IF

IRESP  = 0

CALL OPEN_ll(TPFILE,IRESP,HMODE='IO_ZSPLIT',HPROGRAM_ORIG=HPROGRAM_ORIG)

IF (TPFILE%LMASTER) THEN
  ! Proc I/O case
  INQUIRE(FILE=TRIM(TPFILE%CNAME)//'.lfi',EXIST=GEXIST_LFI)
  INQUIRE(FILE=TRIM(TPFILE%CNAME)//'.nc',EXIST=GEXIST_NC4)

  IF (YACTION == 'READ') THEN
    IF (.NOT.GEXIST_LFI .AND. .NOT.GEXIST_NC4) &
      CALL PRINT_MSG(NVERB_FATAL,'IO','FMOPEN_ll',TRIM(TPFILE%CNAME)//': no .nc or .lfi file')

    SELECT CASE (TRIM(TPFILE%CFORMAT))
      CASE ('NETCDF4')
        IF (.NOT.GEXIST_NC4 .AND. GEXIST_LFI) THEN
          CALL PRINT_MSG(NVERB_WARNING,'IO','FMOPEN_ll',TRIM(TPFILE%CNAME)// &
                         ': .nc file does not exist but .lfi exists -> forced to LFI')
          TPFILE%CFORMAT='LFI'
        END IF
      CASE ('LFI')
        IF (.NOT.GEXIST_LFI .AND. GEXIST_NC4) THEN
          CALL PRINT_MSG(NVERB_WARNING,'IO','FMOPEN_ll',TRIM(TPFILE%CNAME)// &
                         ': .lfi file does not exist but .nc exists -> forced to NETCDF4')
          TPFILE%CFORMAT='NETCDF4'
        END IF
      CASE ('LFICDF4')
        IF (GEXIST_NC4) THEN
          CALL PRINT_MSG(NVERB_WARNING,'IO','FMOPEN_ll',TRIM(TPFILE%CNAME)// &
                         ': LFICDF4 format is not allowed in READ mode -> forced to NETCDF4')
          TPFILE%CFORMAT='NETCDF4'
        ELSE IF (GEXIST_LFI) THEN
          CALL PRINT_MSG(NVERB_WARNING,'IO','FMOPEN_ll',TRIM(TPFILE%CNAME)// &
                         ': LFICDF4 format is not allowed in READ mode -> forced to LFI')
          TPFILE%CFORMAT='LFI'
        END IF
      CASE DEFAULT
        IF (GEXIST_NC4) THEN
          CALL PRINT_MSG(NVERB_ERROR,'IO','FMOPEN_ll',TRIM(TPFILE%CNAME)// &
                         ': invalid fileformat (-> forced to NETCDF4 if no abort)')
          TPFILE%CFORMAT='NETCDF4'
        ELSE IF (GEXIST_LFI) THEN
          CALL PRINT_MSG(NVERB_ERROR,'IO','FMOPEN_ll',TRIM(TPFILE%CNAME)// &
                         ': invalid fileformat (-> forced to LFI if no abort)')
          TPFILE%CFORMAT='LFI'
        END IF
    END SELECT
  END IF
END IF

#if defined(MNH_IOCDF4)
IF (TPFILE%CFORMAT=='NETCDF4' .OR. TPFILE%CFORMAT=='LFICDF4') THEN
  SELECT CASE (YACTION)
    CASE('READ')
      call io_open_file_nc4(tpfile)
    CASE('WRITE')
      call io_create_file_nc4(TPFILE, hprogram_orig=HPROGRAM_ORIG)
  END SELECT
END IF
#endif

IF (TPFILE%CFORMAT=='LFI' .OR. TPFILE%CFORMAT=='LFICDF4') THEN
  SELECT CASE (YACTION)
    CASE('READ')
      call io_open_file_lfi(tpfile,iresp)
    CASE('WRITE')
      call io_create_file_lfi(tpfile,iresp)
  END SELECT
END IF

IF ( IRESP /= 0 )  THEN
  WRITE(YRESP,"( I0 )") IRESP
  CALL PRINT_MSG(NVERB_ERROR,'IO','FMOPEN_ll',TRIM(TPFILE%CNAME)//': exit with IRESP='//TRIM(YRESP))
END IF

KRESP=IRESP

END SUBROUTINE FMOPEN_ll


recursive SUBROUTINE IO_FILE_CLOSE_ll(TPFILE,KRESP,HPROGRAM_ORIG)
!
USE MODD_CONF,             ONLY: CPROGRAM
USE MODD_IO_ll,            ONLY: TFILEDATA

use mode_io_file_lfi,      only: io_close_file_lfi
#if defined(MNH_IOCDF4)
use mode_io_file_nc4,      only: io_close_file_nc4
use mode_io_write_nc4,     only: io_write_coordvar_nc4
#endif
USE MODE_IO_ll,            ONLY: CLOSE_ll
USE MODE_IO_MANAGE_STRUCT, ONLY: IO_FILE_FIND_BYNAME
!
TYPE(TFILEDATA),            INTENT(INOUT) :: TPFILE ! File structure
INTEGER,          OPTIONAL, INTENT(OUT)   :: KRESP  ! Return code
CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: HPROGRAM_ORIG !To emulate a file coming from this program
!
INTEGER                 :: IRESP, JI
TYPE(TFILEDATA),POINTER :: TZFILE_DES
TYPE(TFILEDATA),POINTER :: TZFILE_IOZ
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_FILE_CLOSE_ll','closing '//TRIM(TPFILE%CNAME))
!
IF (.NOT.TPFILE%LOPENED) THEN
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_FILE_CLOSE_ll','trying to close a file not opened: '//TRIM(TPFILE%CNAME))
  RETURN
ENDIF
!
IF (TPFILE%NOPEN_CURRENT>1) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_FILE_CLOSE_ll',TRIM(TPFILE%CNAME)// &
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
    CALL CLOSE_ll(TPFILE,IRESP)
    !
    TPFILE%NLU = -1

  CASE DEFAULT
    !Do not close (non-existing) '.des' file if OUTPUT
    IF(TPFILE%CTYPE/='OUTPUT' .AND. CPROGRAM/='LFICDF') THEN
      CALL IO_FILE_FIND_BYNAME(TRIM(TPFILE%CNAME)//'.des',TZFILE_DES,IRESP)
      IF (IRESP/=0) CALL PRINT_MSG(NVERB_ERROR,'IO','IO_FILE_CLOSE_ll','file '//TRIM(TPFILE%CNAME)//'.des not in filelist')
      CALL IO_FILE_CLOSE_ll(TZFILE_DES,KRESP=IRESP,HPROGRAM_ORIG=HPROGRAM_ORIG)
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
        CALL PRINT_MSG(NVERB_ERROR,'IO','IO_FILE_CLOSE_ll','file '//TRIM(TZFILE_IOZ%CNAME)//' is not opened')
      IF (TZFILE_IOZ%NOPEN_CURRENT/=1) &
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_FILE_CLOSE_ll','file '//TRIM(TZFILE_IOZ%CNAME)//&
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
END SELECT
!
TPFILE%LOPENED       = .FALSE.
TPFILE%NOPEN_CURRENT = 0
TPFILE%NCLOSE        = TPFILE%NCLOSE + 1
!
IF (PRESENT(KRESP)) KRESP=IRESP
!
END SUBROUTINE IO_FILE_CLOSE_ll


subroutine IO_ADD2TRANSFER_LIST(TPFILE)

USE MODD_CONF,  ONLY : CPROGRAM
USE MODD_IO_ll, ONLY : TFILEDATA

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

END MODULE MODE_FM
