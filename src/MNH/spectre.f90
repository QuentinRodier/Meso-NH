!MNH_LIC Copyright 2011-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ######spl
      PROGRAM SPECTRE
!     ############
!
!!****
!!
!!    PURPOSE
!!    -------
!!    compute energy spectra from a MESONH file
!!
!!
!!
!!
!! Modifications:
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 06/07/2021: use FINALIZE_MNH
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_CONF
USE MODD_IO,               only: TFILEDATA
USE MODD_LUNIT_n
USE MODD_TIME_n
USE MODD_DIM_ll
USE MODD_PARAMETERS,       ONLY: NFILENAMELGTMAX
USE MODD_SPECTRE
!
USE MODI_SPECTRE_MESONH
USE MODI_SPECTRE_AROME
!
USE MODE_FINALIZE_MNH,     only: FINALIZE_MNH
USE MODE_IO,               only: IO_Config_set, IO_Init
USE MODE_IO_FILE,          only: IO_File_close, IO_File_open
USE MODE_IO_MANAGE_STRUCT, only: IO_File_add2list
USE MODE_MODELN_HANDLER
USE MODE_MSG
USE MODE_POS
!USE MODD_TYPE_DATE
USE MODI_VERSION
!
USE MODN_CONFZ
USE MODN_CONFIO, ONLY : NAM_CONFIO
!
IMPLICIT NONE
!
!*       0.1   declarations of local variables
!
CHARACTER (LEN=NFILENAMELGTMAX), DIMENSION(1) :: YINIFILE ! names of the INPUT FM-file
CHARACTER (LEN=NFILENAMELGTMAX)               :: YOUTFILE ! names of the OUTPUT FM-file
INTEGER                          :: IRESP         ! return code in FM routines
INTEGER                          :: ILUOUT0       ! Logical unit number for the output listing
INTEGER                          :: ILUNAM        ! Logical unit numbers for the namelist file
                                                  ! and for output_listing file
LOGICAL                          :: GFOUND        ! Return code when searching namelist
!
REAL,DIMENSION(:,:,:),ALLOCATABLE:: ZWORK         ! work array
REAL,DIMENSION(:,:,:),ALLOCATABLE:: ZWORKAROME    ! work array
INTEGER :: NI,NJ,NK
REAL    ::XDELTAX,XDELTAY
TYPE(TFILEDATA),POINTER :: TZNMLFILE  => NULL()
!
NAMELIST/NAM_SPECTRE/ LSPECTRE_U,LSPECTRE_V,LSPECTRE_W,LSPECTRE_TH,LSPECTRE_RV,&
                      LSPECTRE_LSU,LSPECTRE_LSV,LSPECTRE_LSW,LSPECTRE_LSTH,LSPECTRE_LSRV,LSMOOTH
!
NAMELIST/NAM_SPECTRE_FILE/ YINIFILE,CTYPEFILE,YOUTFILE,LSTAT 
NAMELIST/NAM_ZOOM_SPECTRE/ LZOOM,NITOT,NJTOT,NXDEB,NYDEB 
NAMELIST/NAM_DOMAIN_AROME/ NI,NJ,NK,XDELTAX,XDELTAY
!
!-------------------------------------------------------------------------------
!
!*       0.0   Initializations
!              ---------------
!
!
CALL GOTO_MODEL(1)
!
CALL VERSION
CPROGRAM='SPEC  '
!
CALL IO_Init()
!
! initialization 
YINIFILE(:)   = ''
CTYPEFILE     = 'MESONH'
LSPECTRE_U    = .FALSE.
LSPECTRE_V    = .FALSE.
LSPECTRE_W    = .FALSE.
LSPECTRE_TH   = .FALSE.
LSPECTRE_RV   = .FALSE.
LSPECTRE_LSU  = .FALSE.
LSPECTRE_LSV  = .FALSE.
LSPECTRE_LSW  = .FALSE.
LSPECTRE_LSTH = .FALSE.
LSPECTRE_LSRV = .FALSE.
LSMOOTH       = .FALSE.
LZOOM         = .FALSE.
YOUTFILE      = ''
LSTAT         = .FALSE.
NI=750
NJ=720
NK=60
XDELTAX=2500.
XDELTAY=2500.
!
!
!-------------------------------------------------------------------------------
!
!*       1.0   Namelist reading
!              ----------------
!
PRINT*, ' '
PRINT*, '*********************************************************************'
PRINT*, '*********************************************************************'
PRINT*, ' '
!
CALL IO_File_add2list(TZNMLFILE,'SPEC1.nam','NML','READ')
CALL IO_File_open(TZNMLFILE)
ILUNAM = TZNMLFILE%NLU
!
PRINT*, 'READ THE SPEC1.NAM FILE'
!
CALL POSNAM( TZNMLFILE, 'NAM_SPECTRE', GFOUND )
IF (GFOUND) THEN
  READ(UNIT=ILUNAM,NML=NAM_SPECTRE)
  PRINT*, '  namelist NAM_SPECTRE read'
END IF
!
!
CALL POSNAM( TZNMLFILE, 'NAM_SPECTRE_FILE', GFOUND )
IF (GFOUND) THEN
  READ(UNIT=ILUNAM,NML=NAM_SPECTRE_FILE)
  PRINT*, '  namelist NAM_SPECTRE_FILE read'
END IF
!
CALL POSNAM( TZNMLFILE, 'NAM_ZOOM_SPECTRE', GFOUND )
IF (GFOUND) THEN
  READ(UNIT=ILUNAM,NML=NAM_ZOOM_SPECTRE)
  PRINT*, '  namelist NAM_ZOOM_SPECTRE read'
END IF
!
CALL POSNAM( TZNMLFILE, 'NAM_DOMAIN_AROME', GFOUND )
IF (GFOUND) THEN
  READ(UNIT=ILUNAM,NML=NAM_DOMAIN_AROME)
  PRINT*, '  namelist NAM_DOMAIN_AROME read'
END IF
!
CALL POSNAM( TZNMLFILE, 'NAM_CONFZ', GFOUND )
IF (GFOUND) THEN
  READ(UNIT=ILUNAM,NML=NAM_CONFZ)
  PRINT*, '  namelist NAM_CONFZ read'
END IF
!
CALL POSNAM( TZNMLFILE, 'NAM_CONFIO', GFOUND )
IF (GFOUND) THEN
  READ(UNIT=ILUNAM,NML=NAM_CONFIO)
  PRINT*, '  namelist NAM_CONFIO read'
END IF
CALL IO_Config_set()
!
CALL IO_File_close(TZNMLFILE)
!
CINIFILE = YINIFILE(1)
!
!-------------------------------------------------------------------------------
!
!*       2.0   file
!              -----------
!
IF ( LEN_TRIM(CINIFILE)==0 ) THEN
  CALL PRINT_MSG(NVERB_FATAL,'GEN','SPECTRE','LEN_TRIM(CINIFILE)==0')
ENDIF
!
IF ( LEN_TRIM(YOUTFILE)==0 ) THEN
  WRITE(YOUTFILE,FMT='(A,A)') "spectra_",TRIM(ADJUSTL(CINIFILE))
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.0   Fields initialization and spectra computation
!
IF (CTYPEFILE=='MESONH') THEN
  CALL SPECTRE_MESONH(YOUTFILE)
  !
  CALL IO_File_close(LUNIT_MODEL(1)%TINIFILE)
ELSEIF (CTYPEFILE=='AROME ')THEN
 CALL SPECTRE_AROME(CINIFILE,YOUTFILE,XDELTAX,XDELTAY,NI,NJ,NK)
ELSE
  print*,"This type of file is not accept for SPECTRE PROGRAM"
ENDIF
!
!-------------------------------------------------------------------------------
!
!*      4.    FINALIZE THE PARALLEL SESSION
!              -----------------------------
!
CALL FINALIZE_MNH()

PRINT*, ' '
PRINT*, '****************************************************'
PRINT*, '*            EXIT  SPECTRE CORRECTLY          *'
PRINT*, '****************************************************'
PRINT*, ' '
!-------------------------------------------------------------------------------
END PROGRAM SPECTRE

