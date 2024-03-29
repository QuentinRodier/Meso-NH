!MNH_LIC Copyright 1994-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_OPEN_PRC_FILES
!     ##########################
!
INTERFACE
      SUBROUTINE OPEN_PRC_FILES(TPPRE_REAL1FILE,HATMFILE,HATMFILETYPE,TPATMFILE, &
                                                HCHEMFILE,HCHEMFILETYPE, &
                                                HSURFFILE,HSURFFILETYPE, &
                                                HPGDFILE,TPPGDFILE,      &
                                                HCAMSFILE,HCAMSFILETYPE)
!
USE MODD_IO,         ONLY: TFILEDATA
USE MODD_PARAMETERS, ONLY: NFILENAMELGTMAX
!
TYPE(TFILEDATA), POINTER,       INTENT(OUT) :: TPPRE_REAL1FILE ! PRE_REAL1 file
CHARACTER(LEN=NFILENAMELGTMAX), INTENT(OUT) :: HATMFILE     ! name of the input atmospheric file
CHARACTER(LEN=6),               INTENT(OUT) :: HATMFILETYPE ! type of the input atmospheric file
TYPE(TFILEDATA), POINTER,       INTENT(OUT) :: TPATMFILE ! physiographic data file
CHARACTER(LEN=NFILENAMELGTMAX), INTENT(OUT) :: HCHEMFILE    ! name of the input chemical file
CHARACTER(LEN=6),               INTENT(OUT) :: HCHEMFILETYPE! type of the input chemical file
CHARACTER(LEN=NFILENAMELGTMAX), INTENT(OUT) :: HSURFFILE    ! name of the input surface file
CHARACTER(LEN=6),               INTENT(OUT) :: HSURFFILETYPE! type of the input surface file
CHARACTER(LEN=NFILENAMELGTMAX), INTENT(OUT) :: HPGDFILE     ! name of the physiographic data file
TYPE(TFILEDATA), POINTER,       INTENT(OUT) :: TPPGDFILE ! physiographic data file
CHARACTER(LEN=NFILENAMELGTMAX), INTENT(OUT) :: HCAMSFILE    ! name of the input CAMS file
CHARACTER(LEN=6),               INTENT(OUT) :: HCAMSFILETYPE! type of the input CAMS file
!
END SUBROUTINE OPEN_PRC_FILES
END INTERFACE
END MODULE MODI_OPEN_PRC_FILES
!
!     ###############################################################
      SUBROUTINE OPEN_PRC_FILES(TPPRE_REAL1FILE,HATMFILE,HATMFILETYPE,TPATMFILE, &
                                                HCHEMFILE,HCHEMFILETYPE, &
                                                HSURFFILE,HSURFFILETYPE, &
                                                HPGDFILE,TPPGDFILE,      &
                                                HCAMSFILE,HCAMSFILETYPE)
!     ###############################################################
!
!!****  *OPEN_PRC_FILES* - openning of the files used in PREP_REAL_CASE
!!
!!
!!    PURPOSE
!!    -------
!!
!!    This routine creates TLUOUT0
!!    This routine read in 'PRE_REAL1.nam' the names of the files used in
!!    PREP_REAL_CASE: Aladin or Mesonh input file, physiographic data file,
!!    output listing file and MESO-NH output file.
!!    This routine opens these files (except the Aladin file) and reads the
!!    control variable of verbosity level NVERB.
!!
!!**  METHOD
!!    ------
!!
!!    CAUTION:
!!    This routine supposes the name of the namelist file is 'PRE_REAL1.nam'.
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_CONF      : contains configuration variables for all models.
!!         NVERB    : verbosity level for output-listing
!!      Module MODD_LUNIT     :  contains logical unit names for all models
!!         TLUOUT0  : output-listing
!!      Module MODD_LUNIT1    :
!!         CINIFILE : name of MESO-NH file
!!
!!    REFERENCE
!!    ---------
!!
!!      Book 2
!!
!!    AUTHOR
!!    ------
!!
!!      V.Masson  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     31/12/94
!!      Modification 31/01/96 Possibility to initialize the atmospheric fields
!!                            with a FM file (V. Masson)
!!      Modification 01/08/97 opening of CINIFILE at the end of PREP_REAL_CASE
!!                            (V. Masson)
!!      Modification 15/10/01 allow namelists in different orders (I. Mallet)
!!      J.ESCOBAR    12/11/2008  Improve checking --> add STATUS=OLD in open_ll(PRE_REAL1.nam,...
!!      J.Escobar : 19/04/2016 : Pb IOZ/NETCDF , missing OPARALLELIO=.FALSE. for PGD files
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!!      P. Wautelet  01/02/2019 added missing initialization to NULL for files with OUT intent
!  P. Wautelet 07/02/2019: force TYPE to a known value for IO_File_add2list
!  P. Wautelet 07/02/2019: remove OPARALLELIO argument from open and close files subroutines
!                          (nsubfiles_ioz is now determined in IO_File_add2list)
!  P. Wautelet 14/02/2019: remove CLUOUT/CLUOUT0 and associated variables
!  B. Vie         06/2021: LIMA - CAMS coupling
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CONF  ! declaration modules
USE MODD_CONF_n
!JUAN Z_SPLITTING
!USE MODD_CONFZ
!JUAN Z_SPLITTING
USE MODD_IO,               ONLY: TFILE_OUTPUTLISTING, TFILEDATA
USE MODD_LUNIT
USE MODD_LUNIT_n, CINIFILE_n=>CINIFILE , CINIFILEPGD_n=>CINIFILEPGD
USE MODD_PARAMETERS,       ONLY: NFILENAMELGTMAX
!
!
USE MODE_IO,               only: IO_Config_set
USE MODE_IO_FILE,          only: IO_File_open
USE MODE_IO_MANAGE_STRUCT, only: IO_File_add2list
USE MODE_MSG
USE MODE_POS
!
USE MODN_CONFIO, ONLY : NAM_CONFIO
!JUAN Z_SPLITTING
USE MODN_CONFZ
!JUAN Z_SPLITTING
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
!
TYPE(TFILEDATA), POINTER,       INTENT(OUT) :: TPPRE_REAL1FILE ! PRE_REAL1 file
CHARACTER(LEN=NFILENAMELGTMAX), INTENT(OUT) :: HATMFILE     ! name of the input atmospheric file
CHARACTER(LEN=6),               INTENT(OUT) :: HATMFILETYPE ! type of the input atmospheric file
TYPE(TFILEDATA), POINTER,       INTENT(OUT) :: TPATMFILE ! physiographic data file
CHARACTER(LEN=NFILENAMELGTMAX), INTENT(OUT) :: HCHEMFILE    ! name of the input chemical file
CHARACTER(LEN=6),               INTENT(OUT) :: HCHEMFILETYPE! type of the input chemical file
CHARACTER(LEN=NFILENAMELGTMAX), INTENT(OUT) :: HSURFFILE    ! name of the input surface file
CHARACTER(LEN=6),               INTENT(OUT) :: HSURFFILETYPE! type of the input surface file
CHARACTER(LEN=NFILENAMELGTMAX), INTENT(OUT) :: HPGDFILE     ! name of the physiographic data file
TYPE(TFILEDATA), POINTER,       INTENT(OUT) :: TPPGDFILE ! physiographic data file
CHARACTER(LEN=NFILENAMELGTMAX), INTENT(OUT) :: HCAMSFILE    ! name of the input CAMS file
CHARACTER(LEN=6),               INTENT(OUT) :: HCAMSFILETYPE! type of the input CAMS file
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
INTEGER :: IRESP      ! return-code if problems eraised
INTEGER :: IPRE_REAL1 ! logical unit for file PRE_REAL1
INTEGER :: ILUOUT0    ! logical unit for listing file
LOGICAL :: GFOUND     ! Return code when searching namelist
INTEGER :: ILEN
!
CHARACTER(LEN=NFILENAMELGTMAX) :: CINIFILE ! re-declaration of this model variable for namelist
!
!*       0.3   Declaration of namelists
!              ------------------------
!
NAMELIST/NAM_FILE_NAMES/ HATMFILE,HATMFILETYPE,HCHEMFILE,HCHEMFILETYPE, &
                         HSURFFILE,HSURFFILETYPE,HPGDFILE,CINIFILE, &
                         HCAMSFILE,HCAMSFILETYPE
!-------------------------------------------------------------------------------
!
!*       1.    SET DEFAULT NAMES
!              -----------------
!
HATMFILE=''
HATMFILETYPE='MESONH'
HCHEMFILE=''
HCHEMFILETYPE='MESONH'
HSURFFILE=''
HSURFFILETYPE='MESONH'
HCAMSFILE=''
HCAMSFILETYPE='NETCDF'
!
!-------------------------------------------------------------------------------
!
!*       2.    OPENNING OF THE OUTPUT LISTING FILE
!              -----------------------------------
!
CALL IO_File_add2list(TLUOUT0,'OUTPUT_LISTING0','OUTPUTLISTING','WRITE')
CALL IO_File_open(TLUOUT0)
!Set output file for PRINT_MSG
TFILE_OUTPUTLISTING => TLUOUT0
!
ILUOUT0=TLUOUT0%NLU
!
IF (NVERB>=5) WRITE(ILUOUT0,*) 'Routine OPEN_PRC_FILES started'
!-------------------------------------------------------------------------------
!
!*       3.    OPENNING OF PRE_REAL1.nam
!              -------------------------
!
TPPRE_REAL1FILE => NULL()
CALL IO_File_add2list(TPPRE_REAL1FILE,'PRE_REAL1.nam','NML','READ')
CALL IO_File_open(TPPRE_REAL1FILE,KRESP=IRESP)
IPRE_REAL1=TPPRE_REAL1FILE%NLU
IF (IRESP.NE.0 ) THEN
   !callabortstop
   CALL PRINT_MSG(NVERB_FATAL,'GEN','OPEN_PRC_FILES','file PRE_REAL1.nam not found')
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       4.    READING THE OTHER FILE NAMES
!              ----------------------------
!
!JUANZ
CALL POSNAM( TPPRE_REAL1FILE, 'NAM_CONFZ', GFOUND )
IF (GFOUND) READ(UNIT=IPRE_REAL1,NML=NAM_CONFZ)
!JUANZ
CALL POSNAM( TPPRE_REAL1FILE, 'NAM_CONFIO', GFOUND )
IF (GFOUND) READ(UNIT=IPRE_REAL1,NML=NAM_CONFIO)
CALL IO_Config_set()
!
CINIFILE = CINIFILE_n
CALL POSNAM( TPPRE_REAL1FILE, 'NAM_FILE_NAMES', GFOUND )
IF (GFOUND) READ(UNIT=IPRE_REAL1,NML=NAM_FILE_NAMES)
CINIFILE_n = CINIFILE
!
WRITE(ILUOUT0,*) 'HATMFILE= ', TRIM(HATMFILE)
!
IF (HCHEMFILE==HATMFILE) HCHEMFILE=''
IF (LEN_TRIM(HCHEMFILE)>0 .AND. HATMFILETYPE/='GRIBEX') THEN
!callabortstop
  CALL PRINT_MSG(NVERB_FATAL,'GEN','OPEN_PRC_FILES',&
                 'Additional CHEMical file is only possible when ATMospheric file is of GRIBEX type')
END IF
WRITE(ILUOUT0,*) 'HCHEMFILE=', TRIM(HCHEMFILE)
!
WRITE(ILUOUT0,*) 'HCAMSFILE=', TRIM(HCAMSFILE)
!
IF ( LEN_TRIM( HSURFFILE ) == 0 ) THEN
  HSURFFILE     = HATMFILE
  HSURFFILETYPE = HATMFILETYPE
END IF
WRITE(ILUOUT0,*) 'HSURFFILE=', TRIM(HSURFFILE)
!
CINIFILEPGD_n = HPGDFILE
IF (LEN_TRIM(HPGDFILE)==0) THEN
!  IF (HATMFILETYPE=='MESONH') THEN
!    HPGDFILE = HATMFILE
!    WRITE(ILUOUT0,*) 'HPGDFILE set to ', HPGDFILE
!  ELSE
    CALL PRINT_MSG(NVERB_FATAL,'GEN','OPEN_PRC_FILES',&
                   'You need the HPGDFILE file when starting from a large-scale file')
!  END IF
ELSE
!-------------------------------------------------------------------------------
!
!*       5.    OPENING THE PHYSIOGRAPHIC DATA FILE
!              -----------------------------------
!
  TPPGDFILE => NULL()
  CALL IO_File_add2list(TPPGDFILE,TRIM(HPGDFILE),'PGD','READ',KLFITYPE=2,KLFIVERB=NVERB)
  CALL IO_File_open(TPPGDFILE,IRESP)
  IF (IRESP/=0) THEN
!callabortstop
    CALL PRINT_MSG(NVERB_FATAL,'GEN','OPEN_PRC_FILES',' problem during opening of PGD file '//TRIM(HPGDFILE))
  END IF
END IF
!
WRITE(ILUOUT0,*) 'HPGDFILE= ', TRIM(HPGDFILE)
!-------------------------------------------------------------------------------
!
!*       6.    INPUT ATMOSPHERIC FILE
!              ----------------------
!
!*       6.1   ATTRIBUTION OF LOGICAL UNITS TO ALADIN FILES
!              --------------------------------------------
!
!  because of new parallel IO, FMATTR must be called just before opening the Aladin file
!
!*       6.2   OPENING INPUT MESONH FILE
!              -------------------------
!
!  done during INIT
!
!-------------------------------------------------------------------------------
!
WRITE(ILUOUT0,*) 'Routine OPEN_PRC_FILES completed'
!
END SUBROUTINE OPEN_PRC_FILES
