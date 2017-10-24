!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_7 BUG1 2007/06/15 17:47:30
!-----------------------------------------------------------------
!     ################
      PROGRAM ZOOM_PGD
!     ################
!!
!!    PURPOSE
!!    -------
!!   This program zooms the physiographic data fields.
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
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     march 2005
!!    10/10/2011  J.Escobar call INI_PARAZ_ll
!!    30/03/2012  S.Bielli  Add NAM_NCOUT
!!  06/2016     (G.Delautier) phasage surfex 8
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_CONF,   ONLY : CPROGRAM, NMASDEV, NBUGFIX, CBIBUSER, &
                        L1D, L2D, LPACK
USE MODD_LUNIT,  ONLY : CLUOUT0, COUTFMFILE, CPGDFILE
USE MODD_PARAMETERS, ONLY : XUNDEF, NUNDEF, JPVEXT, JPHEXT, JPMODELMAX
USE MODD_PARAM_n,     ONLY : CSURF
USE MODD_DIM_n,       ONLY : NIMAX, NJMAX
USE MODD_CONF_n,   ONLY : CSTORAGE_TYPE
!
USE MODE_POS
USE MODE_FMWRIT
USE MODE_FMREAD
USE MODE_IO_ll
USE MODE_ll
USE MODE_MODELN_HANDLER
!
USE MODI_READ_HGRID
USE MODI_WRITE_HGRID
USE MODI_SET_SUBDOMAIN
!JUANZ
USE MODE_SPLITTINGZ_ll
!JUANZ
!
USE MODI_VERSION
USE MODI_READ_ALL_NAMELISTS
USE MODI_ZOOM_PGD_SURF_ATM
USE MODI_WRITE_PGD_SURF_ATM_N
USE MODD_MNH_SURFEX_n
!
#ifdef MNH_NCWRIT
USE MODN_NCOUT
#endif
USE MODN_CONFIO
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
INTEGER :: IINFO_ll
INTEGER :: ININAR
INTEGER :: IGRID
INTEGER :: ILENCH
CHARACTER (LEN=LEN_HREC) :: YRECFM   
CHARACTER(LEN=100) :: YCOMMENT
CHARACTER(LEN=28)  :: YZOOMFILE             ! name of the output file
CHARACTER(LEN=2)   :: YZOOMNBR
CHARACTER(LEN=28)  :: YMY_NAME,YDAD_NAME
CHARACTER(LEN=28)  :: YPGDFILE
CHARACTER(LEN=2)   :: YSTORAGE_TYPE
CHARACTER(LEN=28) :: YNAMELIST        
LOGICAL :: GFOUND
INTEGER            :: IXOR_DAD,IYOR_DAD  ! compared to Dad file, if any
INTEGER            :: IXOR,IYOR          ! given or computed
INTEGER            :: IDXRATIO,IDYRATIO
!
REAL,  DIMENSION(:,:), ALLOCATABLE :: ZZS1,ZZSMT1,ZZS2,ZZSMT2
!
NAMELIST/NAM_PGDFILE/CPGDFILE,YZOOMFILE,YZOOMNBR
!------------------------------------------------------------------------------
!
CALL GOTO_MODEL(1)
CALL VERSION
CPROGRAM='ZOOMPG'
CSTORAGE_TYPE = 'PG'
!
CALL INI_CST
! 
!
!*    1.      Set default names and parallelized I/O
!             --------------------------------------
!
CALL INITIO_ll()
!
CLUOUT0='OUTPUT_LISTING0'                    ! name of the output-listing
!
CALL OPEN_ll(UNIT=ILUOUT0,FILE=CLUOUT0,IOSTAT=IRESP,    &
             FORM='FORMATTED',ACTION='WRITE',MODE=GLOBAL)
!
YNAMELIST = 'PRE_ZOOM1.nam'                  ! name of the namelist file
 
CALL OPEN_ll(UNIT=ILUNAM,FILE=YNAMELIST,IOSTAT=IRESP,    &
             FORM='FORMATTED',ACTION='READ',MODE=GLOBAL)
!
CPGDFILE  = 'PGDFILE'                         ! name of the input file
YZOOMFILE = ''
YZOOMNBR  = '00'
CALL POSNAM(ILUNAM,'NAM_PGDFILE',GFOUND,ILUOUT0)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_PGDFILE)
#ifdef MNH_NCWRIT
CALL POSNAM(ILUNAM,'NAM_NCOUT',GFOUND,ILUOUT0)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_NCOUT)
#endif
CALL POSNAM(ILUNAM,'NAM_CONFIO',GFOUND,ILUOUT0)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_CONFIO)
CALL SET_CONFIO_ll(LCDF4, LLFIOUT, LLFIREAD)
!
CALL CLOSE_ll(YNAMELIST,IOSTAT=IRESP)
!
!------------------------------------------------------------------------------
!
!*    2.      ZOOM OF PGD DOMAIN
!             ------------------
!
!*    2.1     Open PGD file
!             -------------
!
CALL FMOPEN_ll(CPGDFILE,'READ',CLUOUT0,1,2,5,ININAR,IRESP)
!
!*    2.2     Reading of initial grid
!             -----------------------
!
CALL READ_HGRID(1,CPGDFILE,YMY_NAME,YDAD_NAME,YSTORAGE_TYPE)
!
! NIMAX, NJMAX: size of input domain
ALLOCATE(ZZS1  (NIMAX+2*JPHEXT,NJMAX+2*JPHEXT))
ALLOCATE(ZZSMT1(NIMAX+2*JPHEXT,NJMAX+2*JPHEXT))
YRECFM='ZS'
CALL FMREAD(CPGDFILE,YRECFM,CLUOUT0,'XY',ZZS1,IGRID,ILENCH,YCOMMENT,IRESP)
YRECFM='ZSMT'
CALL FMREAD(CPGDFILE,YRECFM,CLUOUT0,'XY',ZZSMT1,IGRID,ILENCH,YCOMMENT,IRESP)
!
!*    2.3     Define subdomain
!             ----------------
!
CALL SET_SUBDOMAIN(YNAMELIST,CPGDFILE,IXOR_DAD,IYOR_DAD,IXOR,IYOR,IDXRATIO,IDYRATIO)
! NIMAX, NJMAX: size of output domain
!
CALL SET_JP_ll(JPMODELMAX,JPHEXT,JPVEXT,JPHEXT)
CALL SET_DAD0_ll()
CALL SET_DIM_ll(NIMAX, NJMAX, 1)
CALL SET_LBX_ll('OPEN',1)
CALL SET_LBY_ll('OPEN', 1)
CALL SET_XRATIO_ll(1, 1)
CALL SET_YRATIO_ll(1, 1)
CALL SET_XOR_ll(1, 1)
CALL SET_XEND_ll(NIMAX+2*JPHEXT, 1)
CALL SET_YOR_ll(1, 1)
CALL SET_YEND_ll(NJMAX+2*JPHEXT, 1)
CALL SET_DAD_ll(0, 1)
!JUANZ CALL INI_PARA_ll(IINFO_ll)
CALL INI_PARAZ_ll(IINFO_ll)
!
!
!*    2.4     Writing of final grid
!             ---------------------
!
IF ( (LEN_TRIM(YZOOMFILE) == 0) .OR. (ADJUSTL(YZOOMFILE) == ADJUSTL(CPGDFILE)) ) THEN
  YZOOMFILE=ADJUSTL(ADJUSTR(CPGDFILE)//'.z'//ADJUSTL(YZOOMNBR))
END IF
!
CALL FMOPEN_ll(YZOOMFILE,'WRITE',CLUOUT0,1,1,5,ININAR,IRESP)
CALL WRITE_HGRID(1,YZOOMFILE,YDAD_NAME)
!
!*    2.5     Preparation of surface physiographic fields
!             -------------------------------------------
!
CALL FMREAD(CPGDFILE,'SURF        ',CLUOUT0,'--',CSURF,IGRID,ILENCH,YCOMMENT,IRESP)
CALL FMCLOS_ll(CPGDFILE,'KEEP',CLUOUT0,IRESP)
!
!
IF (CSURF=='EXTE') THEN
  CALL SURFEX_ALLOC_LIST(1)
  YSURF_CUR => YSURF_LIST(1)
  CALL READ_ALL_NAMELISTS(YSURF_CUR,'MESONH','PRE',.FALSE.)      
  YPGDFILE   = CPGDFILE
  CPGDFILE   = YZOOMFILE
  COUTFMFILE = YZOOMFILE
  CALL GOTO_SURFEX(1)
  CALL ZOOM_PGD_SURF_ATM(YSURF_CUR,'MESONH',YPGDFILE,'MESONH',YZOOMFILE,'MESONH')
!
!*    2.6     Writes the physiographic fields
!             -------------------------------
!
  CALL WRITE_PGD_SURF_ATM_n(YSURF_CUR,'MESONH')
ELSE
  ALLOCATE(ZZS2(NIMAX+2*JPHEXT,NJMAX+2*JPHEXT))
  ZZS2(:,:)=ZZS1(IXOR:IXOR+NIMAX+2*JPHEXT-1,IYOR:IYOR+NJMAX+2*JPHEXT-1)
  YRECFM='ZS'
  YCOMMENT='X_Y_orography (METERS)'
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(YZOOMFILE,YRECFM,CLUOUT0,'XY',ZZS2,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
ALLOCATE(ZZSMT2(NIMAX+2*JPHEXT,NJMAX+2*JPHEXT))
ZZSMT2(:,:)=ZZSMT1(IXOR:IXOR+NIMAX+2*JPHEXT-1,IYOR:IYOR+NJMAX+2*JPHEXT-1)
YRECFM='ZSMT'
YCOMMENT='X_Y_smooth orography (METERS)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(YZOOMFILE,YRECFM,CLUOUT0,'XY',ZZSMT2,IGRID,ILENCH,YCOMMENT,IRESP)
!
!*    2.7     Write configuration variables in the output file
!             ------------------------------------------------
!
CALL FMWRIT(YZOOMFILE,'MASDEV      ',CLUOUT0,'--',NMASDEV,0,1,' ',IRESP)
CALL FMWRIT(YZOOMFILE,'BUGFIX      ',CLUOUT0,'--',NBUGFIX,0,1,' ',IRESP)
CALL FMWRIT(YZOOMFILE,'BIBUSER     ',CLUOUT0,'--',CBIBUSER,0,1,' ',IRESP)
CALL FMWRIT(YZOOMFILE,'PROGRAM     ',CLUOUT0,'--',CPROGRAM,0,1,' ',IRESP)
CALL FMWRIT(YZOOMFILE,'STORAGE_TYPE',CLUOUT0,'--',CSTORAGE_TYPE,0,1,' ',IRESP)
CALL FMWRIT(YZOOMFILE,'MY_NAME     ',CLUOUT0,'--',YZOOMFILE,0,1,' ',IRESP)
CALL FMWRIT(YZOOMFILE,'DAD_NAME    ',CLUOUT0,'--',YDAD_NAME,0,1,' ',IRESP)
CALL FMWRIT(YZOOMFILE,'DXRATIO     ',CLUOUT0,'--',IDXRATIO,0,1,' ',IRESP)
CALL FMWRIT(YZOOMFILE,'DYRATIO     ',CLUOUT0,'--',IDYRATIO,0,1,' ',IRESP)
CALL FMWRIT(YZOOMFILE,'XOR         ',CLUOUT0,'--',IXOR_DAD,0,1,' ',IRESP)
CALL FMWRIT(YZOOMFILE,'YOR         ',CLUOUT0,'--',IYOR_DAD,0,1,' ',IRESP)
CALL FMWRIT(YZOOMFILE,'L1D         ',CLUOUT0,'--',L1D,0,1,' ',IRESP)
CALL FMWRIT(YZOOMFILE,'L2D         ',CLUOUT0,'--',L2D,0,1,' ',IRESP)
CALL FMWRIT(YZOOMFILE,'PACK        ',CLUOUT0,'--',LPACK,0,1,' ',IRESP)
CALL FMWRIT(YZOOMFILE,'SURF        ',CLUOUT0,'--',CSURF,0,1,' ',IRESP)
CALL FMCLOS_ll(YZOOMFILE,'KEEP',CLUOUT0,IRESP)
!
!*    2.8     Shift to new PGD file
!             ---------------------
!
CPGDFILE = YZOOMFILE
!                       
!------------------------------------------------------------------------------
!
!*    3.     CLOSE PARALLELIZED I/O
!            ----------------------
!
WRITE(ILUOUT0,*)
WRITE(ILUOUT0,*) '***************************'
WRITE(ILUOUT0,*) '* ZOOM_PGD ends correctly *'
WRITE(ILUOUT0,*) '***************************'
!
CALL CLOSE_ll(CLUOUT0,IOSTAT=IRESP)
!
CALL END_PARA_ll(IINFO_ll)

IF (CSURF=='EXTE') CALL SURFEX_DEALLO_LIST
!
!-------------------------------------------------------------------------------
!
END PROGRAM ZOOM_PGD
