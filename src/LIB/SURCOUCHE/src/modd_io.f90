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
IMPLICIT NONE 
!
!
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
LOGICAL, SAVE :: LDEFLATEX2 = .FALSE. ! TRUE to enable Zlib deflate compression on X2 fields  

TYPE LFIPARAM
  INTEGER :: FITYP   ! FM File Type (used in FMCLOSE)
END TYPE LFIPARAM

!Structure describing the characteristics of an output or a backup
TYPE TOUTBAK
  INTEGER           :: NBAKID = -1  !Backup number
  INTEGER           :: NOUTID = -1  !Output number
  INTEGER           :: NSTEP        !Timestep number
  REAL              :: XTIME        !Time from start of the segment (in seconds and rounded to a timestep)
  CHARACTER(LEN=28) :: CFILENAME    !Filename
  INTEGER           :: NOUTDAD = -1 !Index of the corresponding dad file (file with same time)
  CHARACTER(LEN=28) :: CDADFILENAME !Filename of dad
  TYPE(TFILEDATA),POINTER :: TFILE => NULL() !Corresponding file
END TYPE TOUTBAK

!Structure describing the characteristics of a file
TYPE TFILEDATA
  CHARACTER(LEN=7)  :: CFILETYPE = "UNKNOWN" !Filetype (backup, output...)
  TYPE(TFILEDATA),POINTER :: TFILE_PREV => NULL()
  TYPE(TFILEDATA),POINTER :: TFILE_NEXT => NULL()
END TYPE TFILEDATA

TYPE(TFILEDATA),POINTER,SAVE :: TFILE_FIRST => NULL()
TYPE(TFILEDATA),POINTER,SAVE :: TFILE_LAST  => NULL()

END MODULE MODD_IO_ll
