!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!Correction :
!  D.Gazen   : avril 2016 change error message
!  P. Wautelet : may 2016: use NetCDF Fortran module
!-----------------------------------------------------------------
!-----------------------------------------------------------------

MODULE MODE_FM
USE MODD_ERRCODES
USE MODD_MPIF

USE MODE_MSG

IMPLICIT NONE 

PRIVATE 

INTEGER, PARAMETER :: JPPIPE = 10
!INCLUDE 'mpif.h'

PUBLIC SET_FMPACK_ll,FMLOOK_ll
PUBLIC IO_FILE_OPEN_ll, IO_FILE_CLOSE_ll

CONTAINS 

SUBROUTINE SET_FMPACK_ll(O1D,O2D,OPACK)
USE MODD_IO_ll, ONLY : LPACK,L1D,L2D
!JUAN
USE MODD_VAR_ll, ONLY : IP
!JUAN

IMPLICIT NONE 

LOGICAL, INTENT(IN) :: O1D,O2D,OPACK

LPACK = OPACK
L1D   = O1D
L2D   = O2D

IF ( IP .EQ. 1 ) PRINT *,'INIT L1D,L2D,LPACK = ',L1D,L2D,LPACK

END SUBROUTINE SET_FMPACK_ll

SUBROUTINE FMATTR_ll(HFILEM,HFIPRI,KNUMBR,KRESP)
!JUANZ
USE MODD_VAR_ll, ONLY : NMNH_COMM_WORLD
!JUANZ
USE MODD_IO_ll, ONLY : GSMONOPROC, ISIOP, ISTDOUT
USE MODE_FD_ll, ONLY : FD_ll,GETFD,NEWFD
USE MODE_IO_ll, ONLY : IONEWFLU
CHARACTER(LEN=*), INTENT(IN)  :: HFILEM
CHARACTER(LEN=*), INTENT(IN)  :: HFIPRI
INTEGER,          INTENT(OUT) :: KNUMBR
INTEGER,          INTENT(OUT) :: KRESP

TYPE(FD_ll), POINTER :: TZFD, TZFIPRI
TYPE(FD_ll), POINTER :: TZJUAN

IF (GSMONOPROC) THEN ! sequential execution
   TZJUAN=>GETFD(HFILEM)
   IF (.NOT. ASSOCIATED(TZJUAN)) THEN
    !! File is not already opened : GOOD
    !! Add a new FD element
    TZFD=>NEWFD()
    TZFD%NAME = HFILEM
    TZFD%FLU   = IONEWFLU()
    !
    KNUMBR = TZFD%FLU
    KRESP  = NOERROR
  ELSE 
    !! Error : File already associated to a fortran logical unit
    TZFIPRI=>GETFD(HFIPRI)
    IF (ASSOCIATED(TZFIPRI)) THEN
      WRITE(TZFIPRI%FLU,*) 'Error FMATTR_ll : file '&
           & ,TRIM(HFILEM),' already opened'
    ELSE 
      WRITE(ISTDOUT,*) 'Error FMLOOK_ll : file ',TRIM(HFILEM)&
           & ,' already opened'
    END IF
    KRESP   = IOERROR
    KNUMBR  = -1  
  END IF
END IF

END SUBROUTINE FMATTR_ll

SUBROUTINE FMLOOK_ll(HFILEM,HFIPRI,KNUMBR,KRESP)
USE MODD_IO_ll, ONLY : ISTDOUT
USE MODE_FD_ll, ONLY : FD_ll,GETFD
CHARACTER(LEN=*), INTENT(IN)  :: HFILEM
CHARACTER(LEN=*), INTENT(IN)  :: HFIPRI
INTEGER,          INTENT(OUT) :: KNUMBR
INTEGER,          INTENT(OUT) :: KRESP

TYPE(FD_ll), POINTER :: TZFD, TZFIPRI

TZFD=>GETFD(HFILEM)
IF (ASSOCIATED(TZFD)) THEN
  KNUMBR = TZFD%FLU
  KRESP  = NOERROR
ELSE 
  IF (HFILEM == HFIPRI) THEN
    KNUMBR = ISTDOUT
    KRESP  = NOERROR
  ELSE
    TZFIPRI=>GETFD(HFIPRI)
    IF (ASSOCIATED(TZFIPRI)) THEN
      WRITE(TZFIPRI%FLU,*) 'Error FMLOOK_ll : file '&
           & ,TRIM(HFILEM),' not found'
    ELSE 
      WRITE(ISTDOUT,*) 'Error FMLOOK_ll : file ',TRIM(HFILEM)&
           & ,' not found'
    END IF
    KRESP   = IOERROR
    KNUMBR  = -1
  END IF
END IF

END SUBROUTINE FMLOOK_ll

SUBROUTINE IO_FILE_OPEN_ll(TPFILE,KRESP,OPARALLELIO,HPOSITION,HSTATUS)
!
USE MODD_CONF,  ONLY: NMNHVERSION
USE MODD_IO_ll, ONLY: LIOCDF4,LLFIOUT,LLFIREAD,TFILEDATA
USE MODE_FD_ll, ONLY: FD_ll,GETFD
USE MODE_FIELD, ONLY: TFIELDDATA,TYPEINT
USE MODE_FMREAD
USE MODE_IO_ll, ONLY : OPEN_ll
USE MODE_IO_MANAGE_STRUCT, ONLY: IO_FILE_ADD2LIST,IO_FILE_FIND_BYNAME
!
TYPE(TFILEDATA),POINTER,INTENT(INOUT)         :: TPFILE ! File structure
INTEGER,                INTENT(OUT), OPTIONAL :: KRESP  ! Return code
LOGICAL,                INTENT(IN),  OPTIONAL :: OPARALLELIO
CHARACTER(LEN=*),       INTENT(IN),  OPTIONAL :: HPOSITION
CHARACTER(LEN=*),       INTENT(IN),  OPTIONAL :: HSTATUS
!
INTEGER :: IRESP,IRESP2
INTEGER :: IMASDEV,IBUGFIX
INTEGER,DIMENSION(3)    :: IMNHVERSION
CHARACTER(LEN=12)       :: YMNHVERSION_FILE,YMNHVERSION_CURR
TYPE(FD_ll), POINTER    :: TZFDLFI
TYPE(TFIELDDATA)        :: TZFIELD
TYPE(TFILEDATA),POINTER :: TZFILE_DES
TYPE(TFILEDATA),POINTER :: TZFILE_DUMMY
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_FILE_OPEN_ll','opening '//TRIM(TPFILE%CNAME)//' for '//TRIM(TPFILE%CMODE)// &
               ' (filetype='//TRIM(TPFILE%CTYPE)//')')
!
IF (.NOT.ASSOCIATED(TPFILE)) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_FILE_OPEN_ll','TPFILE is not associated')
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
    CALL OPEN_ll(TPFILE,IOSTAT=IRESP,FORM='FORMATTED',POSITION='REWIND',STATUS='OLD',MODE='GLOBAL')


  !Chemistry tabulation files
  CASE('CHEMTAB')
    CALL OPEN_ll(TPFILE,IOSTAT=IRESP,FORM='FORMATTED',MODE='GLOBAL')


  !GPS files
  CASE('GPS')
    CALL OPEN_ll(TPFILE,IOSTAT=IRESP,FORM='FORMATTED',MODE='SPECIFIC')


  !Meteo files
  CASE('METEO')
   CALL OPEN_ll(TPFILE,IOSTAT=IRESP,FORM='UNFORMATTED',MODE='GLOBAL',RECL=100000000)


  !Namelist files
  CASE('NML')
    CALL OPEN_ll(TPFILE,IOSTAT=IRESP,DELIM='QUOTE',MODE='GLOBAL')


  !OUTPUTLISTING files
  CASE('OUTPUTLISTING')
    CALL OPEN_ll(TPFILE,IOSTAT=IRESP,FORM='FORMATTED',MODE='GLOBAL')


  !SURFACE_DATA files
  CASE('SURFACE_DATA')
    IF (TPFILE%CFORM=='FORMATTED') THEN
      CALL OPEN_ll(TPFILE,IOSTAT=IRESP,FORM=TPFILE%CFORM,MODE='GLOBAL')
    ELSE IF (TPFILE%CACCESS=='DIRECT') THEN
      CALL OPEN_ll(TPFILE,IOSTAT=IRESP,FORM=TPFILE%CFORM,ACCESS=TPFILE%CACCESS,RECL=TPFILE%NRECL,MODE='GLOBAL')
    ELSE
      CALL OPEN_ll(TPFILE,IOSTAT=IRESP,FORM=TPFILE%CFORM,MODE='GLOBAL')
    END IF


  !Text files
  CASE('TXT')
    IF(TPFILE%NRECL>0) THEN
      CALL OPEN_ll(TPFILE,IOSTAT=IRESP,FORM='FORMATTED',POSITION=HPOSITION,STATUS=HSTATUS,RECL=TPFILE%NRECL,MODE='GLOBAL')
    ELSE
      CALL OPEN_ll(TPFILE,IOSTAT=IRESP,FORM='FORMATTED',POSITION=HPOSITION,STATUS=HSTATUS,MODE='GLOBAL')
    END IF


  CASE DEFAULT
    !Do not open '.des' file if OUTPUT
    IF(TPFILE%CTYPE/='OUTPUT') THEN
      CALL IO_FILE_ADD2LIST(TZFILE_DES,TRIM(TPFILE%CNAME)//'.des','DES',TPFILE%CMODE,OOLD=.TRUE.) !OOLD=T because the file may already be in the list
      CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_FILE_OPEN_ll','OPEN_ll for '//TRIM(TPFILE%CNAME)//'.des')
      CALL OPEN_ll(TZFILE_DES,FORM='FORMATTED',DELIM='QUOTE',IOSTAT=IRESP,RECL=1024*8,OPARALLELIO=OPARALLELIO)
      TZFILE_DES%LOPENED       = .TRUE.
      TZFILE_DES%NOPEN_CURRENT = TZFILE_DES%NOPEN_CURRENT + 1
      TZFILE_DES%NOPEN         = TZFILE_DES%NOPEN + 1
    ENDIF
    !
    CALL FMOPEN_ll(TPFILE,IRESP,OPARALLELIO=OPARALLELIO)
    !
    TZFDLFI=>GETFD(ADJUSTL(TRIM(TPFILE%CNAME)//'.lfi'))
    IF (TRIM(TPFILE%CMODE) == 'READ' .AND. TPFILE%LMASTER) THEN
      IF (LIOCDF4 .AND. .NOT.LLFIREAD) THEN
        IF (TPFILE%NNCID<0) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_FILE_OPEN_ll','invalid NNCID for '//TRIM(TPFILE%CNAME))
      ELSE
        TPFILE%NLFIFLU = TZFDLFI%FLU
        IF (TPFILE%NLFIFLU<0) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_FILE_OPEN_ll','invalid NLFIFLU for '//TRIM(TPFILE%CNAME))
      ENDIF
    ELSE IF (TRIM(TPFILE%CMODE) == 'WRITE' .AND. TPFILE%LMASTER) THEN
      IF (LIOCDF4) THEN
        IF (TPFILE%NNCID<0) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_FILE_OPEN_ll','invalid NNCID for '//TRIM(TPFILE%CNAME))
      END IF
      IF (.NOT.LIOCDF4 .OR. LLFIOUT) THEN
        TPFILE%NLFIFLU = TZFDLFI%FLU
        IF (TPFILE%NLFIFLU<0) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_FILE_OPEN_ll','invalid NLFIFLU for '//TRIM(TPFILE%CNAME))
      END IF
    ELSE IF (TRIM(TPFILE%CMODE) /= 'READ' .AND. TRIM(TPFILE%CMODE) /= 'WRITE') THEN
      CALL PRINT_MSG(NVERB_FATAL,'IO','IO_FILE_OPEN_ll','unknown opening mode ('//TRIM(TPFILE%CMODE)//') for '//TRIM(TPFILE%CNAME))
    END IF
    !
    !Compare MNHVERSION of file with current version
    IF (TRIM(TPFILE%CMODE) == 'READ') THEN
      IMNHVERSION(:) = 0
      !Use TZFIELD because TFIELDLIST could be not initialised
      TZFIELD%CMNHNAME   = 'MNHVERSION'
      TZFIELD%CSTDNAME   = ''
      TZFIELD%CLONGNAME  = 'MesoNH version'
      TZFIELD%CUNITS     = ''
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = ''
      TZFIELD%NGRID      = 0
      TZFIELD%NTYPE      = TYPEINT
      TZFIELD%NDIMS      = 1
      CALL IO_READ_FIELD(TPFILE,TZFIELD,IMNHVERSION,IRESP2)
      IF (IRESP2/=0) THEN
        TZFIELD%CMNHNAME   = 'MASDEV'
        TZFIELD%CLONGNAME  = 'MesoNH version (without bugfix)'
        CALL IO_READ_FIELD(TPFILE,TZFIELD,IMASDEV,IRESP2)
        IF (IRESP2/=0) THEN
          CALL PRINT_MSG(NVERB_WARNING,'IO','IO_FILE_OPEN_ll','unknown MASDEV version for '//TRIM(TPFILE%CNAME))
        ELSE
          IMNHVERSION(1)=IMASDEV/10
          IMNHVERSION(2)=MOD(IMASDEV,10)
        END IF
        !
        TZFIELD%CMNHNAME   = 'BUGFIX'
        TZFIELD%CLONGNAME  = 'MesoNH bugfix number'
        CALL IO_READ_FIELD(TPFILE,TZFIELD,IBUGFIX,IRESP2)
        IF (IRESP2/=0) THEN
          CALL PRINT_MSG(NVERB_WARNING,'IO','IO_FILE_OPEN_ll','unknown BUGFIX version for '//TRIM(TPFILE%CNAME))
        ELSE
          IMNHVERSION(3)=IBUGFIX
        END IF
      END IF
      !
      WRITE(YMNHVERSION_FILE,"( I0,'.',I0,'.',I0 )" ) IMNHVERSION(1),IMNHVERSION(2),IMNHVERSION(3)
      WRITE(YMNHVERSION_CURR,"( I0,'.',I0,'.',I0 )" ) NMNHVERSION(1),NMNHVERSION(2),NMNHVERSION(3)
      !
      IF ( IMNHVERSION(1)==0 .AND. IMNHVERSION(2)==0 .AND. IMNHVERSION(3)==0 ) THEN
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_FILE_OPEN_ll','file '//TRIM(TPFILE%CNAME)//&
                      ' was written with an unknown version of MesoNH')
      ELSE IF (  IMNHVERSION(1)< NMNHVERSION(1) .OR. &
                (IMNHVERSION(1)==NMNHVERSION(1) .AND. IMNHVERSION(2)< NMNHVERSION(2)) .OR. &
                (IMNHVERSION(1)==NMNHVERSION(1) .AND. IMNHVERSION(2)==NMNHVERSION(2) .AND. IMNHVERSION(3)<NMNHVERSION(3)) ) THEN
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_FILE_OPEN_ll','file '//TRIM(TPFILE%CNAME)//&
                      ' was written with an older version of MesoNH ('//TRIM(YMNHVERSION_FILE)//&
                      ' instead of '//TRIM(YMNHVERSION_CURR)//')')
      ELSE IF (  IMNHVERSION(1)> NMNHVERSION(1) .OR. &
                (IMNHVERSION(1)==NMNHVERSION(1) .AND. IMNHVERSION(2)> NMNHVERSION(2)) .OR. &
                (IMNHVERSION(1)==NMNHVERSION(1) .AND. IMNHVERSION(2)==NMNHVERSION(2) .AND. IMNHVERSION(3)>NMNHVERSION(3)) ) THEN
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_FILE_OPEN_ll','file '//TRIM(TPFILE%CNAME)//&
                      ' was written with a more recent version of MesoNH ('//TRIM(YMNHVERSION_FILE)//&
                      ' instead of '//TRIM(YMNHVERSION_CURR)//')')
      ELSE
        CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_FILE_OPEN_ll','file '//TRIM(TPFILE%CNAME)//&
                      ' was written with the same version of MesoNH ('//TRIM(YMNHVERSION_CURR)//')')
      END IF
    END IF
END SELECT
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_FILE_OPEN_ll

SUBROUTINE FMOPEN_ll(TPFILE,KRESP,OPARALLELIO)
USE MODD_IO_ll, ONLY : ISTDOUT,LIOCDF4,LLFIOUT,LLFIREAD,TFILEDATA
USE MODE_FD_ll, ONLY : JPFINL
USE MODE_IO_ll, ONLY : OPEN_ll,GCONFIO
!JUANZ
USE MODD_CONFZ,ONLY  : NB_PROCIO_R,NB_PROCIO_W
!JUANZ
#if defined(MNH_IOCDF4)
USE MODD_NETCDF, ONLY:IDCDF_KIND
USE MODE_NETCDF
#endif
TYPE(TFILEDATA), INTENT(INOUT) :: TPFILE ! File structure
INTEGER,         INTENT(OUT)   :: KRESP  ! return-code
LOGICAL,         INTENT(IN),  OPTIONAL :: OPARALLELIO
!
!   Local variables
!
INTEGER                 :: IFTYPE  ! type of FM-file
INTEGER                 :: IROWF,IRESP,IFMFNL
CHARACTER(LEN=7)        :: YACTION ! Action upon the file ('READ' or 'WRITE')
CHARACTER(LEN=28)       :: YFILEM  ! name of the file
CHARACTER(LEN=8)        :: YRESP
LOGICAL                 :: GSTATS
LOGICAL, SAVE           :: GSFIRST=.TRUE.
LOGICAL :: GNAMFI,GFATER,GNEWFI
INTEGER :: IERR
!JUAN
INTEGER(KIND=LFI_INT) :: IRESOU,INUMBR8
INTEGER(KIND=LFI_INT) :: IMELEV,INPRAR
INTEGER(KIND=LFI_INT) :: ININAR ! Number of articles present in LFI file (unused here)
LOGICAL               :: GNAMFI8,GFATER8,GSTATS8
INTEGER               :: INB_PROCIO
!JUAN
LOGICAL               :: GPARALLELIO
#if defined(MNH_IOCDF4)
INTEGER(KIND=IDCDF_KIND) :: INCERR
#endif

YACTION = TPFILE%CMODE
YFILEM  = TPFILE%CNAME

CALL PRINT_MSG(NVERB_DEBUG,'IO','FMOPEN_ll','opening '//TRIM(YFILEM)//' for '//TRIM(YACTION))

IF ( PRESENT(OPARALLELIO) ) THEN
  GPARALLELIO = OPARALLELIO
ELSE  !par defaut on active les IO paralleles en Z si possible
  GPARALLELIO = .TRUE.
ENDIF

IF (.NOT. GCONFIO) THEN
   PRINT *, 'FMOPEN_ll Aborting... Please, ensure to call SET_CONFIO_ll before &
        &the first FMOPEN_ll call.'
   STOP
END IF

INPRAR = TPFILE%NLFINPRAR
IROWF  = 0
IRESP  = 0

SELECT CASE (TPFILE%NLFIVERB)
CASE(:2)
  GSTATS = .FALSE.
  IMELEV=0
CASE(3:6)
  GSTATS = .FALSE.
  IMELEV=1
CASE(7:9)
  GSTATS = .FALSE.
  IMELEV=2
CASE(10:)
  GSTATS = .TRUE.
  IMELEV=2
END SELECT

IFMFNL=JPFINL-4
IROWF=LEN_TRIM(YFILEM)

IF (IROWF.EQ.0) THEN
  IRESP=-45
  GOTO 1000
ELSEIF (IROWF.GT.IFMFNL) THEN
  IRESP=-49
  GOTO 1000
ENDIF

 SELECT CASE (YACTION)
 CASE('READ')
    INB_PROCIO = NB_PROCIO_R
 CASE('WRITE')
    INB_PROCIO = NB_PROCIO_W
 END SELECT
CALL OPEN_ll(TPFILE,STATUS="UNKNOWN",MODE='IO_ZSPLIT',IOSTAT=IRESP,     &
             KNB_PROCIO=INB_PROCIO,KMELEV=IMELEV,OPARALLELIO=GPARALLELIO)

IF (IRESP /= 0) GOTO 1000

IF (TPFILE%LMASTER) THEN
  ! Proc I/O case
  IF (GSFIRST) THEN
    GSFIRST = .FALSE.
    OPEN(UNIT=JPPIPE,FILE='pipe_name',FORM='FORMATTED')
  END IF

#if defined(MNH_IOCDF4)
  IF (LIOCDF4) THEN
     IF (YACTION == 'READ' .AND. .NOT. LLFIREAD) THEN
        !! Open NetCDF File for reading
        TPFILE%TNCDIMS => NEWIOCDF()
        CALL PRINT_MSG(NVERB_DEBUG,'IO','FMOPEN_ll','NF90_OPEN for '//TRIM(YFILEM)//'.nc')
        INCERR = NF90_OPEN(ADJUSTL(TRIM(YFILEM))//".nc", NF90_NOWRITE, TPFILE%NNCID)
        IF (INCERR /= NF90_NOERR) THEN
           !PRINT *, 'FMOPEN_ll, NF90_OPEN error : ', NF90_STRERROR(INCERR)
           PRINT *, 'Error in opening (FMOPEN_ll/NF90_OPEN) ', TRIM(YFILEM)//'.nc', ' : ', NF90_STRERROR(INCERR)
           STOP
        END IF
        TPFILE%TNCDIMS%NCID = TPFILE%NNCID
     END IF
     
     IF (YACTION == 'WRITE') THEN
        TPFILE%TNCDIMS => NEWIOCDF()
        CALL PRINT_MSG(NVERB_DEBUG,'IO','FMOPEN_ll','NF90_CREATE for '//TRIM(YFILEM)//'.nc')
        INCERR = NF90_CREATE(ADJUSTL(TRIM(YFILEM))//".nc", &
             &IOR(NF90_CLOBBER,NF90_NETCDF4), TPFILE%NNCID)
        IF (INCERR /= NF90_NOERR) THEN
           !PRINT *, 'FMOPEN_ll, NF90_CREATE error : ', NF90_STRERROR(INCERR)
           PRINT *, 'Error in opening (FMOPEN_ll/NF90_CREATE) ', TRIM(YFILEM)//'.nc', ' : ', NF90_STRERROR(INCERR)
           STOP
        END IF
        TPFILE%TNCDIMS%NCID = TPFILE%NNCID
     END IF
  END IF
#endif
  
  IF (.NOT. LIOCDF4 .OR. (YACTION=='WRITE' .AND. LLFIOUT) &
       &            .OR. (YACTION=='READ'  .AND. LLFIREAD)) THEN
     ! LFI Case
     IRESOU = 0
     GNAMFI = .TRUE.
     GFATER = .TRUE.
     !
     INUMBR8 = TPFILE%NLFIFLU
     GNAMFI8 = GNAMFI
     GFATER8 = GFATER
     GSTATS8 = GSTATS
     !
     CALL LFIOUV(IRESOU,     &
          INUMBR8,           &
          GNAMFI8,           &
          TRIM(YFILEM)//'.lfi',  &
          "UNKNOWN",         &
          GFATER8,           &
          GSTATS8,           &
          IMELEV,            &
          INPRAR,            &
          ININAR)
     
  IF (IRESOU /= 0 ) THEN
        IRESP = IRESOU
     ENDIF
  END IF

  !
  !*      6.    TEST IF FILE IS NEWLY DEFINED
  !
  
  GNEWFI=(ININAR==0).OR.(IMELEV<2)
  IF (.NOT.GNEWFI) THEN
    WRITE (ISTDOUT,*) ' file ',TRIM(YFILEM)//'.lfi',' previously created with LFI'
  ENDIF
END IF
! Broadcast ERROR
CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
IF (IRESP /= 0) GOTO 1000


1000 CONTINUE

IF (IRESP.NE.0)  THEN
  WRITE(YRESP,"( I0 )") IRESP
  CALL PRINT_MSG(NVERB_ERROR,'IO','FMOPEN_ll',TRIM(YFILEM)//': exit with IRESP='//TRIM(YRESP))
END IF

KRESP=IRESP

END SUBROUTINE FMOPEN_ll
  
SUBROUTINE IO_FILE_CLOSE_ll(TPFILE,KRESP,OPARALLELIO)
!
USE MODD_IO_ll, ONLY: TFILEDATA
USE MODE_IO_ll, ONLY : CLOSE_ll
USE MODE_IO_MANAGE_STRUCT, ONLY: IO_FILE_FIND_BYNAME
!
TYPE(TFILEDATA),  INTENT(INOUT)         :: TPFILE ! File structure
INTEGER,          INTENT(OUT), OPTIONAL :: KRESP  ! Return code
LOGICAL,          INTENT(IN),  OPTIONAL :: OPARALLELIO
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
  !Chemistry input files
  CASE('CHEMINPUT')
    CALL CLOSE_ll(TPFILE,IOSTAT=IRESP)
    !
    TPFILE%NLU = -1


  !Chemistry tabulation files
  CASE('CHEMTAB')
    CALL CLOSE_ll(TPFILE,IOSTAT=IRESP)
    !
    TPFILE%NLU = -1


  !GPS files
  CASE('GPS')
    CALL CLOSE_ll(TPFILE,IOSTAT=IRESP)
    !
    TPFILE%NLU = -1


  !Meteo files
  CASE('METEO')
    CALL CLOSE_ll(TPFILE,IOSTAT=IRESP)
    !
    TPFILE%NLU = -1


  !Namelist files
  CASE('NML')
    CALL CLOSE_ll(TPFILE,IOSTAT=IRESP)
    !
    TPFILE%NLU = -1


  !OUTPUTLISTING files
  CASE('OUTPUTLISTING')
    CALL CLOSE_ll(TPFILE,IOSTAT=IRESP,OPARALLELIO=.FALSE.)
    !
    TPFILE%NLU = -1


  !SURFACE_DATA files
  CASE('SURFACE_DATA')
    CALL CLOSE_ll(TPFILE,IOSTAT=IRESP)
    !
    TPFILE%NLU = -1


  !Text files
  CASE('TXT')
    CALL CLOSE_ll(TPFILE,IOSTAT=IRESP)
    !
    TPFILE%NLU = -1


  CASE DEFAULT
    !Do not close (non-existing) '.des' file if OUTPUT
    IF(TPFILE%CTYPE/='OUTPUT') THEN
      CALL IO_FILE_FIND_BYNAME(TRIM(TPFILE%CNAME)//'.des',TZFILE_DES,IRESP)
      IF (IRESP/=0) CALL PRINT_MSG(NVERB_ERROR,'IO','IO_FILE_CLOSE_ll','file '//TRIM(TPFILE%CNAME)//'.des not in filelist')
      !
      TZFILE_DES%NOPEN_CURRENT = TZFILE_DES%NOPEN_CURRENT - 1
      TZFILE_DES%NCLOSE        = TZFILE_DES%NCLOSE + 1
      !
      IF (TZFILE_DES%NOPEN_CURRENT==0) THEN
        CALL CLOSE_ll(TZFILE_DES,IOSTAT=IRESP,STATUS='KEEP')
        TZFILE_DES%LOPENED = .FALSE.
        TZFILE_DES%NLU     = -1
      END IF
    ENDIF
    !
    CALL FMCLOS_ll(TPFILE,'KEEP',KRESP=IRESP,OPARALLELIO=OPARALLELIO)
    !
    TPFILE%NLFIFLU = -1
    TPFILE%NNCID   = -1
    !
    DO JI = 1,TPFILE%NSUBFILES_IOZ
      TZFILE_IOZ => TPFILE%TFILES_IOZ(JI)%TFILE
      IF (.NOT.TZFILE_IOZ%LOPENED) &
        CALL PRINT_MSG(NVERB_ERROR,'IO','IO_FILE_CLOSE_ll','file '//TRIM(TZFILE_IOZ%CNAME)//' is not opened')
      IF (TZFILE_IOZ%NOPEN_CURRENT/=1) &
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_FILE_CLOSE_ll','file '//TRIM(TZFILE_IOZ%CNAME)//&
                       ' is currently opened 0 or several times (expected only 1)')
      TZFILE_IOZ%LOPENED       = .FALSE.
      TZFILE_IOZ%NOPEN_CURRENT = 0
      TZFILE_IOZ%NCLOSE        = TZFILE_IOZ%NCLOSE + 1
      TZFILE_IOZ%NLFIFLU       = -1
      TZFILE_IOZ%NNCID         = -1
    END DO
END SELECT
!
TPFILE%LOPENED       = .FALSE.
TPFILE%NOPEN_CURRENT = 0
TPFILE%NCLOSE        = TPFILE%NCLOSE + 1
!
IF (PRESENT(KRESP)) KRESP=IRESP
!
END SUBROUTINE IO_FILE_CLOSE_ll

SUBROUTINE FMCLOS_ll(TPFILE,HSTATU,KRESP,OPARALLELIO)
!
!!    MODIFICATIONS
!!    -------------
!
!!      J.Escobar   18/10/10   bug with PGI compiler on ADJUSTL
!-------------------------------------------------------------------------------
USE MODD_IO_ll, ONLY : TFILEDATA
USE MODE_FD_ll, ONLY : JPFINL
USE MODE_IO_ll, ONLY : CLOSE_ll,UPCASE
#if !defined(MNH_SGI)
USE MODI_SYSTEM_MNH
#endif
#if defined(MNH_IOCDF4)
USE MODE_NETCDF
#endif
TYPE(TFILEDATA),      INTENT(IN) :: TPFILE ! File structure
CHARACTER(LEN=*),     INTENT(IN) :: HSTATU ! status for the closed file
INTEGER,              INTENT(OUT), OPTIONAL :: KRESP   ! return-code if problems araised
LOGICAL,              INTENT(IN),  OPTIONAL :: OPARALLELIO

INTEGER              ::IRESP,IROWF,IFMFNL
CHARACTER(LEN=28)    :: YFILEM  ! name of the file
CHARACTER(LEN=7)     ::YSTATU
LOGICAL              ::GSTATU
CHARACTER(LEN=8)        :: YRESP
CHARACTER(LEN=10)       ::YCPIO
CHARACTER(LEN=14)       ::YTRANS
CHARACTER(LEN=100)      ::YCOMMAND
INTEGER                 :: IERR, IFITYP
INTEGER, SAVE           :: ICPT=0
INTEGER(KIND=LFI_INT) :: IRESP8
LOGICAL :: GPARALLELIO

YFILEM  = TPFILE%CNAME

CALL PRINT_MSG(NVERB_DEBUG,'IO','FMCLOS_ll','closing '//TRIM(YFILEM))

IF ( PRESENT(OPARALLELIO) ) THEN
  GPARALLELIO = OPARALLELIO
ELSE
  GPARALLELIO = .TRUE.  !par defaut on active les IO paralleles en Z si possible
ENDIF

IRESP  = 0
IROWF  = 0

IFMFNL=JPFINL-4

IROWF=LEN_TRIM(YFILEM)

IF (IROWF.EQ.0) THEN
  IRESP=-59
  GOTO 1000
ELSEIF (IROWF.GT.IFMFNL) THEN
  IRESP=-60
  GOTO 1000
ENDIF

IF (LEN(HSTATU).LE.0) THEN
  IRESP=-41
  GOTO 1000
ELSE
  YSTATU = HSTATU
  YSTATU = UPCASE(TRIM(ADJUSTL(YSTATU)))
  GSTATU=YSTATU=='KEEP'.OR.YSTATU=='DELETE'
  IF (.NOT. GSTATU) THEN
    YSTATU='DEFAULT'
  ENDIF
ENDIF

IF (TPFILE%LMASTER) THEN
  IF (TPFILE%NLFIFLU > 0) THEN
     CALL LFIFER(IRESP8,TPFILE%NLFIFLU,YSTATU)
     IRESP = IRESP8
  END IF
  IF (ASSOCIATED(TPFILE%TNCDIMS)) CALL CLEANIOCDF(TPFILE%TNCDIMS)
  IF (IRESP == 0) THEN
    !! Write in pipe
#if defined(MNH_LINUX) || defined(MNH_SP4)
    YTRANS='xtransfer.x'
#elif defined(MNH_SX5)
    YTRANS='nectransfer.x'
#else
    YTRANS='fujitransfer.x'
#endif
    IFITYP = TPFILE%NLFITYPE
    
    SELECT CASE (IFITYP)
    CASE(:-1)
      IRESP=-66
      GOTO 500
    CASE(0)
      YCPIO='NIL'
    CASE(1)
      YCPIO='MESONH'
    CASE(2)
      PRINT *,'FILE ',YFILEM,' NOT TRANSFERED'
      GOTO 500
    CASE(3:)
      IRESP=-66
      GOTO 500
    END SELECT
!   WRITE (YCOMMAND,*) YTRANS,' ',YCPIO,' ',YFILEM
#if defined(MNH_LINUX) || defined(MNH_VPP) || defined(MNH_SX5) ||  defined(MNH_SP4)
    ICPT=ICPT+1
    WRITE (YCOMMAND,'(A," ",A," ",A," >> OUTPUT_TRANSFER",I3.3,"  2>&1 &")') TRIM(YTRANS),TRIM(YCPIO),TRIM(YFILEM),ICPT
!JUAN jusqu'a MASDEV4_4    WRITE (YCOMMAND,'(A," ",A," ",A,"  ")') TRIM(YTRANS),TRIM(YCPIO),TRIM(YFILEM)
#endif
#if defined(MNH_SGI)
    WRITE (YCOMMAND,'(A," ",A," ",A," &")') TRIM(YTRANS),TRIM(YCPIO),TRIM(YFILEM)
#endif

    PRINT *,'YCOMMAND =',YCOMMAND
#if !defined(MNH_SGI)
    CALL SYSTEM_MNH(YCOMMAND)
#endif
  END IF
END IF

500 CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
IF (IRESP /= 0) GOTO 1000

CALL CLOSE_ll(TPFILE,IOSTAT=IRESP,STATUS=YSTATU,OPARALLELIO=GPARALLELIO)

1000 CONTINUE

IF (IRESP.NE.0)  THEN
  WRITE(YRESP,"( I0 )") IRESP
  CALL PRINT_MSG(NVERB_ERROR,'IO','FMCLOS_ll',TRIM(YFILEM)//': exit with IRESP='//TRIM(YRESP))
END IF

IF (PRESENT(KRESP)) KRESP=IRESP

! format: 14c for fujitransfer.x and mesonh/nil
!         32c for file name
! if you have to change this format one day, don't forget the blank after 1H
! 20 FORMAT(A14,1H ,A10,1H ,A32,1H ,A1)
!
END SUBROUTINE FMCLOS_ll

END MODULE MODE_FM
