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
!!    M.Moge       01/03/2015     use MPPDB + SPLIT_GRID is now called in PGD_GRID. Here we extend 
!!                                the new grid on the halo with EXTEND_GRID_ON_HALO (M.Moge)
!!    M.Moge          06/2015     write NDXRATIO,NDYRATIO,NXSIZE,NYSIZE,NXOR,NYOR in .lfi output file
!!    J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!!    J.Escobar : 05/10/2015 : missing JPHEXT for LAT/LON/ZS/ZSMT writing
!!    M.Moge          11/2015     disable the creation of files on multiple 
!!                                Z-levels when using parallel IO for PREP_PGD
!!    06/2016     (G.Delautier) phasage surfex 8
!!    P.Wautelet : 08/07/2016 : removed MNH_NCWRIT define
!!    10/2016    (S.Faroux S.Bielli) correction for NHALO=0
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_CONF,   ONLY : CPROGRAM, L1D, L2D, LPACK, LCARTESIAN
USE MODD_CONF_n,ONLY : CSTORAGE_TYPE
USE MODD_LUNIT,  ONLY : CLUOUT0
USE MODD_PARAMETERS, ONLY : XUNDEF
USE MODD_IO_ll,   ONLY : GSMONOPROC,LIOCDF4,LLFIOUT,TFILEDATA,TFILE_SURFEX
USE MODD_IO_SURF_MNH, ONLY : NHALO
USE MODD_SPAWN, ONLY : NDXRATIO,NDYRATIO,NXSIZE,NYSIZE,NXOR,NYOR
!
USE MODE_POS
USE MODE_FMWRIT
USE MODE_IO_ll
USE MODE_FM
USE MODE_MODELN_HANDLER
USE MODE_MSG
USE MODE_FIELD
!
USE MODI_ZSMT_PGD
!
!JUAN
USE MODN_CONFZ
USE MODD_PARAMETERS, ONLY : JPHEXT  
USE MODD_CONF, ONLY       : NHALO_CONF_MNH => NHALO
!JUAN
!
USE MODI_READ_ALL_NAMELISTS
USE MODI_VERSION
USE MODI_PGD_GRID_SURF_ATM
USE MODI_SPLIT_GRID
USE MODI_PGD_SURF_ATM
USE MODI_WRITE_PGD_SURF_ATM_N
USE MODD_MNH_SURFEX_n
!
USE MODE_MPPDB
USE MODI_EXTEND_GRID_ON_HALO
!
USE MODN_CONFIO, ONLY : NAM_CONFIO
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
LOGICAL           :: LHSLOP=.FALSE.       ! filtering of slopes higher than XHSLOP   
REAL              :: XHSLOP=1.2           ! if LHSLOP filtering of slopes higher than XHSLOP   
INTEGER           :: NSLEVE   =12         ! number of iteration for filter for smooth orography
REAL              :: XSMOOTH_ZS = XUNDEF  ! optional uniform smooth orography for SLEVE coordinate
REAL, DIMENSION(:,:),ALLOCATABLE   :: ZWORK ! work array for lat and lon reshape
REAL, DIMENSION(:,:),ALLOCATABLE   :: ZWORK_LAT ! work array for lat and lon reshape
REAL, DIMENSION(:,:),ALLOCATABLE   :: ZWORK_LON ! work array for lat and lon reshape
CHARACTER(LEN=16) :: YRECFM   ! name of record
INTEGER           :: IGRID    ! grid location
INTEGER           :: ILENCH   ! length of comment string
CHARACTER(LEN=100):: YCOMMENT ! comment string
INTEGER           :: IIMAX, IJMAX
INTEGER           :: NHALO_MNH 
TYPE(TFILEDATA),TARGET :: TZFILE
!
NAMELIST/NAM_PGDFILE/CPGDFILE, NHALO
NAMELIST/NAM_ZSFILTER/NZSFILTER,LHSLOP,XHSLOP
NAMELIST/NAM_SLEVE/NSLEVE, XSMOOTH_ZS
NAMELIST/NAM_CONF_PGD/JPHEXT, NHALO_MNH
!------------------------------------------------------------------------------
!
CALL MPPDB_INIT()
!
CPROGRAM='PGD   '
!
!
CALL MPPDB_INIT()
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
CALL POSNAM(ILUNAM,'NAM_CONF_PGD',GFOUND)
IF (GFOUND) THEN
   NHALO_MNH = NHALO_CONF_MNH
   READ(UNIT=ILUNAM,NML=NAM_CONF_PGD)
   NHALO_CONF_MNH = NHALO_MNH
ENDIF
!JUANZ
CALL POSNAM(ILUNAM,'NAM_CONFIO',GFOUND)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_CONFIO)
CALL SET_CONFIO_ll()
!
CALL CLOSE_ll('PRE_PGD1.nam')
!
!
CALL SURFEX_ALLOC_LIST(1)
YSURF_CUR => YSURF_LIST(1)
CALL READ_ALL_NAMELISTS(YSURF_CUR,'MESONH','PRE',.FALSE.)
!
CALL INI_FIELD_LIST(1)
!
CALL GOTO_MODEL(1)
CALL GOTO_SURFEX(1)
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
CALL PGD_GRID_SURF_ATM(YSURF_CUR%UG, YSURF_CUR%U,YSURF_CUR%GCP,'MESONH',&
                       '                            ','      ',.FALSE.)
!
CALL EXTEND_GRID_ON_HALO('MESONH',YSURF_CUR%UG, YSURF_CUR%U,&
        YSURF_CUR%UG%NGRID_PAR, YSURF_CUR%UG%XGRID_PAR)
!
!
!*            Initializes all physiographic fields
!             ------------------------------------
!
CALL PGD_SURF_ATM(YSURF_CUR,'MESONH','                            ','      ',.FALSE.)
!
!
!*    3.      Writes the physiographic fields
!             -------------------------------
!
TZFILE%CNAME      = CPGDFILE
TZFILE%CTYPE      = 'PREPPGD'
IF (LIOCDF4) THEN
  IF (.NOT.LLFIOUT) THEN
    TZFILE%CFORMAT= 'NETCDF4'
  ELSE
    TZFILE%CFORMAT= 'LFICDF4'
    TZFILE%NLFINPRAR= 1
  END IF
ELSE IF (LLFIOUT) THEN
  TZFILE%CFORMAT  = 'LFI'
  TZFILE%NLFINPRAR= 1
ELSE
  CALL PRINT_MSG(NVERB_FATAL,'IO','PREP_PGD','unknown backup/output fileformat')
ENDIF
TZFILE%CMODE      = 'WRITE'
TZFILE%NLFITYPE   = 1
TZFILE%NLFIVERB   = 5
!
CALL IO_FILE_OPEN_ll(TZFILE,CLUOUT0,IRESP,OPARALLELIO=.FALSE.)
!
CALL IO_WRITE_HEADER(TZFILE,CLUOUT0)
!
CALL IO_WRITE_FIELD(TZFILE,'SURF',CLUOUT0,IRESP,'EXTE')
CALL IO_WRITE_FIELD(TZFILE,'L1D', CLUOUT0,IRESP,L1D)
CALL IO_WRITE_FIELD(TZFILE,'L2D', CLUOUT0,IRESP,L2D)
CALL IO_WRITE_FIELD(TZFILE,'PACK',CLUOUT0,IRESP,LPACK)
IF ( NDXRATIO <= 0 .AND. NDYRATIO <= 0 ) THEN
  NDXRATIO = 1
  NDYRATIO = 1
ENDIF
IF ( NXSIZE < 0 .AND. NYSIZE < 0 ) THEN
  NXSIZE = 0
  NYSIZE = 0
ENDIF
IF ( NXOR <= 0 .AND. NYOR <= 0 ) THEN
  NXOR = 1
  NYOR = 1
ENDIF
CALL IO_WRITE_FIELD(TZFILE,'DXRATIO',CLUOUT0,IRESP,NDXRATIO)
CALL IO_WRITE_FIELD(TZFILE,'DYRATIO',CLUOUT0,IRESP,NDYRATIO)
CALL IO_WRITE_FIELD(TZFILE,'XSIZE',  CLUOUT0,IRESP,NXSIZE)
CALL IO_WRITE_FIELD(TZFILE,'YSIZE',  CLUOUT0,IRESP,NYSIZE)
CALL IO_WRITE_FIELD(TZFILE,'XOR',    CLUOUT0,IRESP,NXOR)
CALL IO_WRITE_FIELD(TZFILE,'YOR',    CLUOUT0,IRESP,NYOR)
CALL IO_WRITE_FIELD(TZFILE,'JPHEXT', CLUOUT0,IRESP,JPHEXT)
!
TFILE_SURFEX => TZFILE
CALL WRITE_PGD_SURF_ATM_n(YSURF_CUR,'MESONH')
NULLIFY(TFILE_SURFEX) !Probably not necessary
!
!*    4.      Computes and writes smooth orography for SLEVE coordinate
!             ---------------------------------------------------------
CALL ZSMT_PGD(TZFILE,NZSFILTER,NSLEVE,XSMOOTH_ZS)
!
IF (.NOT.LCARTESIAN) THEN
!!!! WRITE LAT and LON
   CALL GET_DIM_PHYS_ll('B',IIMAX,IJMAX)
   ALLOCATE(ZWORK(IIMAX+NHALO*2,IJMAX+NHALO*2))
   ALLOCATE(ZWORK_LAT(IIMAX+2*JPHEXT,IJMAX+2*JPHEXT))
   ALLOCATE(ZWORK_LON(IIMAX+2*JPHEXT,IJMAX+2*JPHEXT))
   ZWORK=RESHAPE(YSURF_CUR%UG%XLAT, (/ (IIMAX+NHALO*2),(IJMAX+NHALO*2) /) )
   IF (NHALO/=0) THEN   
     ZWORK_LAT=ZWORK(NHALO:(IIMAX+NHALO+1),NHALO:(IJMAX+NHALO+1))
   ELSE
     ZWORK_LAT(2:IIMAX+1,2:IJMAX+1)=ZWORK
     ZWORK_LAT(1,:) = ZWORK_LAT(2,:)
     ZWORK_LAT(IIMAX+2,:) = ZWORK_LAT(IIMAX+1,:)
     ZWORK_LAT(:,1) = ZWORK_LAT(:,2)
     ZWORK_LAT(:,IJMAX+2) = ZWORK_LAT(:,IJMAX+1)
   ENDIF
   ZWORK=RESHAPE(YSURF_CUR%UG%XLON, (/ IIMAX+NHALO*2,IJMAX+NHALO*2 /) )
   IF (NHALO/=0) THEN   
     ZWORK_LON=ZWORK(NHALO:(IIMAX+NHALO+1),NHALO:(IJMAX+NHALO+1))
   ELSE
     ZWORK_LON(2:IIMAX+1,2:IJMAX+1)=ZWORK
     ZWORK_LON(1,:) = ZWORK_LON(2,:)
     ZWORK_LON(IIMAX+2,:) = ZWORK_LON(IIMAX+1,:)
     ZWORK_LON(:,1) = ZWORK_LON(:,2)
     ZWORK_LON(:,IJMAX+2) = ZWORK_LON(:,IJMAX+1)           
   ENDIF   
   CALL IO_WRITE_FIELD(TZFILE,'LAT',CLUOUT0,IRESP,ZWORK_LAT)
   CALL IO_WRITE_FIELD(TZFILE,'LON',CLUOUT0,IRESP,ZWORK_LON)
   !
   DEALLOCATE(ZWORK,ZWORK_LAT,ZWORK_LON)
END IF
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
CALL CLOSE_ll(CLUOUT0,IOSTAT=IRESP,OPARALLELIO=.FALSE.)
CALL IO_FILE_CLOSE_ll(TZFILE,CLUOUT0,IRESP,OPARALLELIO=.FALSE.)
!
CALL END_PARA_ll(IINFO_ll)
!
CALL SURFEX_DEALLO_LIST
!
!-------------------------------------------------------------------------------
!
END PROGRAM PREP_PGD
