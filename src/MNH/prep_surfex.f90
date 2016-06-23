!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
---------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_7 BUG1 2007/06/15 17:47:27
!-----------------------------------------------------------------
!     #############################
      PROGRAM PREP_SURFEX
!     #############################
!
!!****  *PREP_SURFEX* - program to write an initial FM file from real case
!!                                situation containing only surface fields.
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
!!      Original    12/2004 (P. Le Moigne)
!!    10/10/2011  J.Escobar call INI_PARAZ_ll
!!  06/2016     (G.Delautier) phasage surfex 8
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_FM
USE MODE_IO_ll
USE MODE_ll
USE MODE_FMREAD
USE MODE_FMWRIT
!
USE MODI_OPEN_PRC_FILES
!
USE MODD_PARAMETERS, ONLY : JPMODELMAX,JPHEXT,JPVEXT, NUNDEF, XUNDEF
USE MODD_LUNIT,      ONLY : CLUOUT0, CPGDFILE,COUTFMFILE
USE MODD_CONF,       ONLY : CPROGRAM,NMASDEV,NBUGFIX,CBIBUSER,&
                            L1D, L2D, LPACK
USE MODD_LUNIT_n,     ONLY : CINIFILE
USE MODD_TIME_n,      ONLY : TDTCUR
USE MODD_CONF_n,     ONLY : CSTORAGE_TYPE
!
USE MODE_MODELN_HANDLER
!JUANZ
USE MODE_SPLITTINGZ_ll
!JUANZ
USE MODI_VERSION
USE MODI_READ_ALL_NAMELISTS
USE MODD_MNH_SURFEX_n
!
IMPLICIT NONE
!
!*       0.1   Declaration of local variables
!              ------------------------------
!
CHARACTER(LEN=28)     :: YPRE_REAL1      ! name of the PRE_REAL1 file
CHARACTER(LEN=28)     :: YATMFILE        ! name of the Atmospheric file
CHARACTER(LEN=6)      :: YATMFILETYPE    ! type of the Atmospheric file
CHARACTER(LEN=28)     :: YCHEMFILE       ! name of the Chemical file (not used)
CHARACTER(LEN=6)      :: YCHEMFILETYPE   ! type of the Chemical file (not used)
CHARACTER(LEN=28)     :: YSURFFILE       ! name of the Surface file (not used)
CHARACTER(LEN=6)      :: YSURFFILETYPE   ! type of the Surface file (not used)
CHARACTER(LEN=28)     :: YPGDFILE        ! name of the physiographic data
!                                        ! file
!
!* file management variables and counters
!
INTEGER               :: ILUOUT0         ! logical unit for listing file
INTEGER               :: IPRE_REAL1      ! logical unit for namelist file
INTEGER               :: IRESP           ! return code in FM routines
INTEGER               :: ININAR          ! number of articles initially
                                                  ! present in a FM file
!
INTEGER               :: IINFO_ll        ! return code of // routines
CHARACTER (LEN=100)   :: HCOMMENT
CHARACTER(LEN=28)     :: YDAD ='                            '
! name of dad of input FM file
INTEGER               :: II, IJ, IGRID, ILENGTH
!
!-------------------------------------------------------------------------------
!
!
!*       1.    SET DEFAULT VALUES
!              ------------------
!
CALL GOTO_MODEL(1)
!
CALL VERSION
CPROGRAM='REAL  '
CSTORAGE_TYPE='SU'
!
!-------------------------------------------------------------------------------
!
!*       2.    OPENNING OF THE FILES
!              ---------------------
CALL INITIO_ll()
!
CALL OPEN_PRC_FILES(YPRE_REAL1,YATMFILE, YATMFILETYPE  &
                              ,YCHEMFILE,YCHEMFILETYPE &
                              ,YSURFFILE,YSURFFILETYPE &
                              ,YPGDFILE)
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
!
CPGDFILE = YPGDFILE
CALL FMOPEN_ll(CINIFILE,'WRITE',CLUOUT0,0,1,1,ININAR,IRESP)
!
!-------------------------------------------------------------------------------
!
!*       3.    INITIALIZATION OF PHYSICAL CONSTANTS
!              ------------------------------------
!
CALL INI_CST
!
!-------------------------------------------------------------------------------
!
!*       4.    READING OF NAMELIST
!              -------------------
!
!*       4.1   reading of configuration variables
!
CALL FMLOOK_ll(YPRE_REAL1,CLUOUT0,IPRE_REAL1,IRESP)
!
CALL CLOSE_ll(YPRE_REAL1, IOSTAT=IRESP)

!*       4.2   reading of values of some configuration variables in namelist
!
CALL FMREAD(CPGDFILE,'IMAX',CLUOUT0,'--',II,IGRID,ILENGTH,HCOMMENT,IRESP)
CALL FMREAD(CPGDFILE,'JMAX',CLUOUT0,'--',IJ,IGRID,ILENGTH,HCOMMENT,IRESP)
CALL SET_JP_ll(JPMODELMAX,JPHEXT,JPVEXT,JPHEXT)
CALL SET_DAD0_ll()
CALL SET_DIM_ll(II, IJ, 1)
CALL SET_LBX_ll('OPEN',1)
CALL SET_LBY_ll('OPEN', 1)
CALL SET_XRATIO_ll(1, 1)
CALL SET_YRATIO_ll(1, 1)
CALL SET_XOR_ll(1, 1)
CALL SET_XEND_ll(II+2*JPHEXT, 1)
CALL SET_YOR_ll(1, 1)
CALL SET_YEND_ll(IJ+2*JPHEXT, 1)
CALL SET_DAD_ll(0, 1)
!JUANZ CALL INI_PARA_ll(IINFO_ll)
CALL INI_PARAZ_ll(IINFO_ll)
!
!-------------------------------------------------------------------------------
!
!
!*       5.    PREPARATION OF SURFACE FIELDS
!              -----------------------------
!
!* reading of date
!
IF (YATMFILETYPE=='MESONH') THEN
  CALL FMOPEN_ll(YATMFILE,'READ',CLUOUT0,0,1,1,ININAR,IRESP)
  CALL FMREAD(YATMFILE,'DTCUR',CLUOUT0,'--',TDTCUR,IGRID,ILENGTH,HCOMMENT,IRESP)
  CALL FMCLOS_ll(YATMFILE,'KEEP',CLUOUT0,IRESP)
ELSE
  TDTCUR%TDATE%YEAR = NUNDEF
  TDTCUR%TDATE%MONTH= NUNDEF
  TDTCUR%TDATE%DAY  = NUNDEF
  TDTCUR%TIME       = XUNDEF
END IF
!
CALL SURFEX_ALLOC_LIST(1)
YSURF_CUR => YSURF_LIST(1)
CALL READ_ALL_NAMELISTS(YSURF_CUR,'MESONH','PRE',.FALSE.)
CALL GOTO_SURFEX(1)
!
CALL PREP_SURF_MNH(YATMFILE,YATMFILETYPE)
!
!-------------------------------------------------------------------------------
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
!-------------------------------------------------------------------------------
WRITE(ILUOUT0,*) ' '
WRITE(ILUOUT0,*) '----------------------------------'
WRITE(ILUOUT0,*) '|                                |'
WRITE(ILUOUT0,*) '|   PREP_SURFEX ends correctly   |'
WRITE(ILUOUT0,*) '|                                |'
WRITE(ILUOUT0,*) '----------------------------------'
CALL CLOSE_ll(CLUOUT0, IOSTAT=IRESP)
CALL FMCLOS_ll(CINIFILE,'KEEP',CLUOUT0,IRESP)
!
CALL END_PARA_ll(IINFO_ll)
CALL SURFEX_DEALLO_LIST
!-------------------------------------------------------------------------------
!
END PROGRAM PREP_SURFEX
