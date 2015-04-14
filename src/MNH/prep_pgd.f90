!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!     ################
      PROGRAM PREP_PGD
!     ################
!!
!!    PURPOSE
!!    -------
!!   This program prepares the physiographic data fields.
!!
!!    METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    F. Mereyde                  Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     21/07/95
!!    Modification 26/07/95       Treatment of orography and subgrid-scale
!!                                orography roughness length (V. Masson)
!!    Modification 22/05/96       Variable CSTORAGE_TYPE (V. Masson)
!!    Modification 25/05/96       Modification of splines, correction on z0rel
!!                                and set limits for some surface varaibles
!!    Modification 12/06/96       Treatment of a rare case for ZPGDZ0EFF (Masson)
!!    Modification 22/11/96       removes the filtering. It will have to be
!!                                performed in ADVANCED_PREP_PGD (Masson)
!!    Modification 15/03/99       **** MAJOR MODIFICATION **** (Masson)
!!                                PGD fields are now defined from the cover
!!                                type fractions in the grid meshes
!!                                User can still include its own data, and
!!                                even additional (dummy) fields
!!    Modificatio 06/00           patch approach, for vegetation related variable (Solmon/Masson)
!                                  averaging is performed on subclass(=patch) of nature
!!                08/03/01        add chemical emission treatment (D.Gazen)
!!    Modification 15/10/01       allow namelists in different orders (I.Mallet)
!!
!!                                ################################
!!    MODIFICATION 13/10/03       EXTERNALIZED VERSION (V. Masson)
!!                                ################################
!!    J.Escobar    4/04/2008      Improve checking --> add STATUS=OLD in open_ll(PRE_PGD1.nam,...
!!
!!    Modification 30/03/2012     Add NAM_NCOUT for netcdf output (S.Bielli)
!!    S.Bielli     23/04/2014     supress writing of LAt and LON in NETCDF case
!!    S.Bielli     20/11/2014     add writing of LAt and LON in NETCDF case
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_CONF,   ONLY : CPROGRAM, NMASDEV, NBUGFIX, CBIBUSER, &
                        L1D, L2D, LPACK , LCARTESIAN
USE MODD_CONF_n,ONLY : CSTORAGE_TYPE
USE MODD_LUNIT,  ONLY : CLUOUT0, COUTFMFILE
USE MODD_PARAMETERS, ONLY : XUNDEF
USE MODD_IO_ll,   ONLY : GSMONOPROC
USE MODD_IO_SURF_MNH, ONLY : NHALO
!
USE MODE_POS
USE MODE_FMWRIT
USE MODE_IO_ll
USE MODE_MODELN_HANDLER
!
USE MODI_ZSMT_PGD
!
!JUAN
USE MODN_CONFZ
!JUAN
USE MODN_CONFIO
!
USE MODI_ALLOC_SURFEX
USE MODI_READ_ALL_NAMELISTS
USE MODI_GOTO_SURFEX
USE MODI_VERSION
USE MODI_PGD_GRID_SURF_ATM
USE MODI_SPLIT_GRID
USE MODI_PGD_SURF_ATM
USE MODI_WRITE_PGD_SURF_ATM_N
USE MODI_DEALLOC_SURFEX
!
USE MODD_SURF_ATM_GRID_n,     ONLY : XLON, XLAT
#ifdef MNH_NCWRIT
USE MODN_NCOUT
USE MODE_UTIL
USE MODE_FMREAD
#endif
!
IMPLICIT NONE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: IRESP    ! return code for I/O
INTEGER :: ILUOUT0
INTEGER :: ILUNAM
INTEGER :: IINFO_LL
INTEGER :: ININAR
LOGICAL :: GFOUND
CHARACTER(LEN=28) :: YDAD     =' '        ! name of dad of input FM file
CHARACTER(LEN=28) :: CPGDFILE ='PGDFILE'  ! name of the output file
INTEGER           :: NZSFILTER=1          ! number of iteration for filter for fine   orography
INTEGER           :: NSLEVE   =12         ! number of iteration for filter for smooth orography
REAL              :: XSMOOTH_ZS = XUNDEF  ! optional uniform smooth orography for SLEVE coordinate
!#ifdef MNH_NCWRIT
REAL, DIMENSION(:,:),ALLOCATABLE   :: ZWORK ! work array for lat and lon reshape
REAL, DIMENSION(:,:),ALLOCATABLE   :: ZWORK_LAT ! work array for lat and lon reshape
REAL, DIMENSION(:,:),ALLOCATABLE   :: ZWORK_LON ! work array for lat and lon reshape
REAL, DIMENSION(:,:),ALLOCATABLE   :: ZZS ! work array for lat and lon reshape
CHARACTER(LEN=16) :: YRECFM   ! name of record
INTEGER           :: IGRID    ! grid location
INTEGER           :: ILENCH   ! length of comment string
CHARACTER(LEN=100):: YCOMMENT ! comment string
INTEGER           :: IIMAX, IJMAX
!#endif
!
NAMELIST/NAM_PGDFILE/CPGDFILE, NHALO
NAMELIST/NAM_ZSFILTER/NZSFILTER
NAMELIST/NAM_SLEVE/NSLEVE, XSMOOTH_ZS
!------------------------------------------------------------------------------
!
!
CPROGRAM='PGD   '
!
!
!*    1.      Set default names and parallelized I/O
!             --------------------------------------
!
CALL INITIO_ll()
!
NHALO=15
!
CLUOUT0='OUTPUT_LISTING0'                    ! Name of the output-listing.
!
CALL OPEN_ll(UNIT=ILUOUT0,FILE=CLUOUT0,IOSTAT=IRESP,    &
             FORM='FORMATTED',ACTION='WRITE',MODE=GLOBAL)
!
!JUAN
CALL OPEN_ll(UNIT=ILUNAM,FILE='PRE_PGD1.nam',IOSTAT=IRESP,    &
             FORM='FORMATTED',ACTION='READ',STATUS='OLD',MODE=GLOBAL)
IF (IRESP.NE.0 ) THEN
PRINT "('PREP_PGD :: IRESP=',I6,' --> file PRE_PGD1.nam not found ')", IRESP
 !callabortstop
CALL CLOSE_ll(CLUOUT0,IOSTAT=IRESP)
CALL ABORT
STOP
ENDIF
!JUAN

CALL POSNAM(ILUNAM,'NAM_PGDFILE',GFOUND)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_PGDFILE)
CALL POSNAM(ILUNAM,'NAM_ZSFILTER',GFOUND)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_ZSFILTER)
CALL POSNAM(ILUNAM,'NAM_SLEVE',GFOUND)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_SLEVE)
!JUANZ
CALL POSNAM(ILUNAM,'NAM_CONFZ',GFOUND)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_CONFZ)
!JUANZ
CALL POSNAM(ILUNAM,'NAM_CONFIO',GFOUND)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_CONFIO)
CALL SET_CONFIO_ll(LCDF4, LLFIOUT, LLFIREAD)
!SB
#ifdef MNH_NCWRIT
CALL POSNAM(ILUNAM,'NAM_NCOUT',GFOUND)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_NCOUT)
#endif
!
CALL CLOSE_ll('PRE_PGD1.nam')
!
!
CALL ALLOC_SURFEX(1)
CALL READ_ALL_NAMELISTS('MESONH','PRE',.FALSE.)
!
CALL GOTO_MODEL(1)
CALL GOTO_SURFEX(1,.TRUE.)
!
CALL VERSION
CSTORAGE_TYPE = 'PG'
!
CALL INI_CST
!
!
!*    2.      Preparation of surface physiographic fields
!             -------------------------------------------
!
!*            Initializes the grid
!             --------------------
! 
CALL PGD_GRID_SURF_ATM('MESONH','                            ','      ',.FALSE.)
!
CALL SPLIT_GRID('MESONH')
!
!
!*            Initializes all physiographic fields
!             ------------------------------------
!
CALL PGD_SURF_ATM('MESONH','                            ','      ',.FALSE.)
!
!
!*    3.      Writes the physiographic fields
!             -------------------------------
!
COUTFMFILE = CPGDFILE
CALL FMOPEN_ll(COUTFMFILE,'WRITE',CLUOUT0,1,1,5,ININAR,IRESP)
!
CALL FMWRIT(COUTFMFILE,'MASDEV      ',CLUOUT0,'--',NMASDEV,0,1,' ',IRESP)
CALL FMWRIT(COUTFMFILE,'BUGFIX      ',CLUOUT0,'--',NBUGFIX,0,1,' ',IRESP)
CALL FMWRIT(COUTFMFILE,'BIBUSER     ',CLUOUT0,'--',CBIBUSER,0,1,' ',IRESP)
CALL FMWRIT(COUTFMFILE,'PROGRAM     ',CLUOUT0,'--',CPROGRAM,0,1,' ',IRESP)
CALL FMWRIT(COUTFMFILE,'STORAGE_TYPE',CLUOUT0,'--',CSTORAGE_TYPE,0,1,' ',IRESP)
CALL FMWRIT(COUTFMFILE,'MY_NAME     ',CLUOUT0,'--',COUTFMFILE,0,1,' ',IRESP)
CALL FMWRIT(COUTFMFILE,'DAD_NAME    ',CLUOUT0,'--',YDAD,0,1,' ',IRESP)
CALL FMWRIT(COUTFMFILE,'SURF        ',CLUOUT0,'--','EXTE',0,1,' ',IRESP)
CALL FMWRIT(COUTFMFILE,'L1D         ',CLUOUT0,'--',L1D,0,1,' ',IRESP)
CALL FMWRIT(COUTFMFILE,'L2D         ',CLUOUT0,'--',L2D,0,1,' ',IRESP)
CALL FMWRIT(COUTFMFILE,'PACK        ',CLUOUT0,'--',LPACK,0,1,' ',IRESP)
!
#ifdef MNH_NCWRIT
NC_WRITE = LNETCDF
CALL WRITE_PGD_SURF_ATM_n('MESONH')
IF (LNETCDF.AND..NOT.LCARTESIAN) THEN
  LLFIFM = .FALSE.
!!!! WRITE LAT and LON
  CALL GET_DIM_PHYS_ll('B',IIMAX,IJMAX)
  ALLOCATE(ZWORK(IIMAX+NHALO*2,IJMAX+NHALO*2))
  ALLOCATE(ZWORK_LAT(IIMAX+2,IJMAX+2))
  ALLOCATE(ZWORK_LON(IIMAX+2,IJMAX+2))
  ZWORK=RESHAPE(XLAT, (/ (IIMAX+NHALO*2),(IJMAX+NHALO*2) /) )
  ZWORK_LAT=ZWORK(NHALO:(IIMAX+NHALO+1),NHALO:(IJMAX+NHALO+1))
!!
CALL FMWRIT(COUTFMFILE,'LAT',CLUOUT0,'XY',ZWORK_LAT,1,21,'X_Y_latitude (degree)',IRESP)
  ZWORK=RESHAPE(XLON, (/ IIMAX+NHALO*2,IJMAX+NHALO*2 /) )
  ZWORK_LON=ZWORK(NHALO:(IIMAX+NHALO+1),NHALO:(IJMAX+NHALO+1))
CALL FMWRIT(COUTFMFILE,'LON',CLUOUT0,'XY',ZWORK_LON,1,22,'X_Y_longitude (degree)',IRESP)
  DEALLOCATE(ZWORK)
  LLFIFM = .TRUE.
END IF
!*    4.      Computes and writes smooth orography for SLEVE coordinate
!             ---------------------------------------------------------
CALL ZSMT_PGD(COUTFMFILE,NZSFILTER,NSLEVE,XSMOOTH_ZS)

IF ( LNETCDF ) THEN
  DEF_NC=.FALSE.
  CALL WRITE_PGD_SURF_ATM_n('MESONH')
  IF (LNETCDF.AND..NOT.LCARTESIAN) THEN
    LLFIFM = .FALSE.
!!!! WRITE LAT and LON
    CALL FMWRIT(COUTFMFILE,'LAT',CLUOUT0,'XY',ZWORK_LAT,1,21,'X_Y_latitude (degree)',IRESP)
    CALL FMWRIT(COUTFMFILE,'LON',CLUOUT0,'XY',ZWORK_LON,1,22,'X_Y_longitude (degree)',IRESP)
  END IF
  ALLOCATE(ZZS(IIMAX+2,IJMAX+2))
!!!!  writes smooth orography for SLEVE coordinate in netcdf
  YRECFM = 'ZS              '
  CALL FMREAD(COUTFMFILE,YRECFM,CLUOUT0,'XY',ZZS,IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMWRIT(COUTFMFILE,'ZS',CLUOUT0,'XY',ZZS,IGRID,ILENCH,YCOMMENT,IRESP)
  YRECFM = 'ZSMT            '
  CALL FMREAD(COUTFMFILE,YRECFM,CLUOUT0,'XY',ZZS,IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMWRIT(COUTFMFILE,'ZSMT',CLUOUT0,'XY',ZZS,IGRID,ILENCH,YCOMMENT,IRESP)
  DEF_NC=.TRUE.
  NC_WRITE = .FALSE.
END IF
#else
CALL WRITE_PGD_SURF_ATM_n('MESONH')
!*    4.      Computes and writes smooth orography for SLEVE coordinate
!             ---------------------------------------------------------
CALL ZSMT_PGD(COUTFMFILE,NZSFILTER,NSLEVE,XSMOOTH_ZS)
IF (.NOT.LCARTESIAN) THEN
!!!! WRITE LAT and LON
   CALL GET_DIM_PHYS_ll('B',IIMAX,IJMAX)
   ALLOCATE(ZWORK(IIMAX+NHALO*2,IJMAX+NHALO*2))
   ALLOCATE(ZWORK_LAT(IIMAX+2,IJMAX+2))
   ALLOCATE(ZWORK_LON(IIMAX+2,IJMAX+2))
   ZWORK=RESHAPE(XLAT, (/ (IIMAX+NHALO*2),(IJMAX+NHALO*2) /) )
   ZWORK_LAT=ZWORK(NHALO:(IIMAX+NHALO+1),NHALO:(IJMAX+NHALO+1))
   ZWORK=RESHAPE(XLON, (/ IIMAX+NHALO*2,IJMAX+NHALO*2 /) )
   ZWORK_LON=ZWORK(NHALO:(IIMAX+NHALO+1),NHALO:(IJMAX+NHALO+1))
   YRECFM='LAT'
   YCOMMENT='X_Y_latitude (degree)'
   IGRID=1
   ILENCH=LEN(YCOMMENT)
   CALL FMWRIT(COUTFMFILE,YRECFM,CLUOUT0,'XY',ZWORK_LAT,IGRID,ILENCH,YCOMMENT,IRESP)
   
   YRECFM='LON'
   YCOMMENT='X_Y_longitude (degree)'
   IGRID=1
   ILENCH=LEN(YCOMMENT)
   CALL FMWRIT(COUTFMFILE,YRECFM,CLUOUT0,'XY',ZWORK_LON,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
#endif
!
!
WRITE(ILUOUT0,*)
WRITE(ILUOUT0,*) '***************************'
WRITE(ILUOUT0,*) '* PREP_PGD ends correctly *'
WRITE(ILUOUT0,*) '***************************'
!
!*    6.      Close parallelized I/O
!             ----------------------
!
CALL CLOSE_ll(CLUOUT0,IOSTAT=IRESP)
CALL FMCLOS_ll(COUTFMFILE,'KEEP',CLUOUT0,IRESP)
!
CALL END_PARA_ll(IINFO_ll)
!
CALL DEALLOC_SURFEX
!
!-------------------------------------------------------------------------------
!
END PROGRAM PREP_PGD
