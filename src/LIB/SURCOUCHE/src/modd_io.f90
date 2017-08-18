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
!-----------------------------------------------------------------
!-----------------------------------------------------------------

MODULE MODD_IO_ll
!
USE MODD_NETCDF, ONLY: IDCDF_KIND
!
IMPLICIT NONE 
!
!
INTEGER, PARAMETER :: NVERB_NO=0, NVERB_FATAL=1, NVERB_ERROR=2, NVERB_WARNING=3, NVERB_INFO=4, NVERB_DEBUG=5


INTEGER, SAVE :: ISTDOUT, ISTDERR

INTEGER, SAVE :: ISIOP   !! IOproc number
INTEGER, SAVE :: ISP     !! Actual proc number
INTEGER, SAVE :: ISNPROC !! Total number of allocated processors 
LOGICAL, SAVE :: GSMONOPROC = .FALSE. !! True if sequential execution (ISNPROC = 1) 

LOGICAL, SAVE :: L1D   = .FALSE. ! TRUE if 1D model version
LOGICAL, SAVE :: L2D   = .FALSE. ! TRUE if 2D model version
LOGICAL, SAVE :: LPACK = .FALSE. ! TRUE if FM compression occurs in 1D or 2D model version

LOGICAL, SAVE :: LIOCDF4    = .FALSE. ! TRUE will enable full NetCDF4 (HDF5) I/O support
LOGICAL, SAVE :: LLFIOUT    = .FALSE. ! TRUE will also force LFI output when LIOCDF4 is on (debug only)  
LOGICAL, SAVE :: LLFIREAD   = .FALSE. ! TRUE will force LFI read (instead of NetCDF) when LIOCDF4 is on (debug only)  

INTEGER, SAVE :: NIO_VERB        = NVERB_WARNING ! Verbosity level for IO
INTEGER, SAVE :: NIO_ABORT_LEVEL = NVERB_ERROR   ! Level of IO error necessary to force stop of application

INTEGER, SAVE :: NGEN_VERB        = NVERB_WARNING ! Verbosity level for 'GEN' (generic) messages
INTEGER, SAVE :: NGEN_ABORT_LEVEL = NVERB_ERROR   ! Level of 'GEN' error necessary to force stop of application

TYPE LFIPARAM
  INTEGER :: FITYP   ! FM File Type (used in FMCLOSE)
END TYPE LFIPARAM

!Structure containing one pointer to a file
!Useful to create arrays of pointers to files
TYPE TFILE_ELT
  TYPE(TFILEDATA),POINTER :: TFILE
END TYPE TFILE_ELT

!Structure describing the characteristics of an output or a backup
TYPE TOUTBAK
  INTEGER           :: NID = -1     !Backup number
  INTEGER           :: NSTEP        !Timestep number
  REAL              :: XTIME        !Time from start of the segment (in seconds and rounded to a timestep)
  INTEGER           :: NOUTDAD = -1 !Index of the corresponding dad file (file with same time)
  TYPE(TFILEDATA),POINTER :: TFILE => NULL() !Corresponding file
  TYPE(TFILE_ELT),DIMENSION(:),ALLOCATABLE :: TFILE_IOZ !Corresponding Z-splitted files
  INTEGER,DIMENSION(:),POINTER :: NFIELDLIST => NULL() !List of the fields to read or write
END TYPE TOUTBAK

!Structure describing the characteristics of a file
TYPE TFILEDATA
  CHARACTER(LEN=28) :: CNAME               !Filename
  CHARACTER(LEN=13) :: CTYPE   = "UNKNOWN" !Filetype (backup, output, prepidealcase...)
  CHARACTER(LEN=7)  :: CFORMAT = "UNKNOWN" !Fileformat (NETCDF4, LFI...)
  CHARACTER(LEN=7)  :: CMODE   = "UNKNOWN" !Opening mode (read, write...)
  LOGICAL           :: LOPENED = .FALSE.   !Is the file opened
  INTEGER           :: NOPEN_CURRENT = 0   !Number of times the file is currently opened (several opens without close are allowed)
  INTEGER           :: NOPEN   = 0         !Number of times the file has been opened (during the current execution)
  INTEGER           :: NCLOSE  = 0         !Number of times the file has been closed (during the current execution)
  !
  ! Fields for LFI files
  INTEGER :: NLFINPRAR = 0  !Number of predicted articles of the LFI file (non crucial)
  INTEGER :: NLFITYPE  = -1 !Type of the file (used to generate list of files to transfers)
  INTEGER :: NLFIVERB  = 1  !LFI verbosity level
  INTEGER :: NLFIFLU   = -1 !File identifier
  !
  ! Fields for netCDF files
  INTEGER(KIND=IDCDF_KIND) :: NNCID = -1 !File identifier
  LOGICAL                  :: LNCREDUCE_FLOAT_PRECISION = .FALSE. ! Reduce the precision of floats to single precision
                                                                  ! instead of double precision
  LOGICAL                  :: LNCCOMPRESS = .FALSE. ! Do compression on fields
  INTEGER(KIND=IDCDF_KIND) :: NNCCOMPRESS_LEVEL = 0 ! Compression level
  !
  TYPE(TFILEDATA),POINTER :: TDADFILE   => NULL() !Corresponding dad file
  TYPE(TFILEDATA),POINTER :: TFILE_PREV => NULL()
  TYPE(TFILEDATA),POINTER :: TFILE_NEXT => NULL()
END TYPE TFILEDATA

!Structure containing a pointer to a file (useful to create arrays of pointers to files)
TYPE TPTR2FILE
  TYPE(TFILEDATA),POINTER :: TZFILE => NULL()
END TYPE

TYPE(TFILEDATA),POINTER,SAVE :: TFILE_FIRST => NULL()
TYPE(TFILEDATA),POINTER,SAVE :: TFILE_LAST  => NULL()

TYPE(TFILEDATA),POINTER,SAVE :: TFILE_SURFEX  => NULL() !Pointer used to find the file used when writing SURFEX fields in write_surf_mnh.f90

TYPE(TFILEDATA),TARGET, SAVE :: TFILE_DUMMY = TFILEDATA(CNAME="dummy") !Non existing file which can be used as a dummy target

END MODULE MODD_IO_ll
