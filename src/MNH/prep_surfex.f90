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
USE MODD_CONF,        ONLY : CPROGRAM,NMASDEV,NBUGFIX,CBIBUSER,&
                             L1D, L2D, LPACK
USE MODD_CONF_n,      ONLY : CSTORAGE_TYPE
USE MODD_IO_ll,       ONLY : TFILEDATA, LIOCDF4, LLFIOUT, TFILE_SURFEX
USE MODD_LUNIT,       ONLY : CLUOUT0, CPGDFILE,COUTFMFILE
USE MODD_LUNIT_n,     ONLY : CINIFILE
USE MODD_MNH_SURFEX_n
USE MODD_PARAMETERS,  ONLY : JPMODELMAX,JPHEXT,JPVEXT, NUNDEF, XUNDEF
USE MODD_TIME_n,      ONLY : TDTCUR
!
USE MODE_FIELD
USE MODE_FM
USE MODE_FMREAD
USE MODE_FMWRIT
USE MODE_IO_ll
USE MODE_ll
USE MODE_MODELN_HANDLER
USE MODE_SPLITTINGZ_ll
!
USE MODI_OPEN_PRC_FILES
USE MODI_READ_ALL_NAMELISTS
USE MODI_VERSION
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
INTEGER               :: II, IJ, IGRID, ILENGTH
!
TYPE(TFILEDATA),TARGET :: TZFILE
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
!
TZFILE%CNAME      = CINIFILE
TZFILE%CTYPE      = 'PREPSURFEX'
print *,'PW: LIOCDF4=',LIOCDF4
IF (LIOCDF4) THEN
  IF (.NOT.LLFIOUT) THEN
    TZFILE%CFORMAT='NETCDF4'
  ELSE
    TZFILE%CFORMAT='LFICDF4'
    TZFILE%NLFINPRAR= 0
  END IF
ELSE IF (LLFIOUT) THEN
  TZFILE%CFORMAT='LFI'
  TZFILE%NLFINPRAR= 0
ELSE
  CALL PRINT_MSG(NVERB_FATAL,'IO','PREP_REAL_CASE','unknown backup/output fileformat')
ENDIF
TZFILE%CMODE      = 'WRITE'
TZFILE%NLFITYPE   = 1
TZFILE%NLFIVERB   = 1
!
CALL IO_FILE_OPEN_ll(TZFILE,CLUOUT0,IRESP)
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
CALL INI_FIELD_LIST(1)
!
CALL INI_FIELD_SCALARS()
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
TFILE_SURFEX => TZFILE
CALL PREP_SURF_MNH(YATMFILE,YATMFILETYPE)
NULLIFY(TFILE_SURFEX)
!
!-------------------------------------------------------------------------------
!
CALL IO_WRITE_HEADER(TZFILE,CLUOUT0)
CALL IO_WRITE_FIELD(TZFILE,'SURF',CLUOUT0,IRESP,'EXTE')
CALL IO_WRITE_FIELD(TZFILE,'L1D', CLUOUT0,IRESP,L1D)
CALL IO_WRITE_FIELD(TZFILE,'L2D', CLUOUT0,IRESP,L2D)
CALL IO_WRITE_FIELD(TZFILE,'PACK',CLUOUT0,IRESP,LPACK)
!
!-------------------------------------------------------------------------------
WRITE(ILUOUT0,*) ' '
WRITE(ILUOUT0,*) '----------------------------------'
WRITE(ILUOUT0,*) '|                                |'
WRITE(ILUOUT0,*) '|   PREP_SURFEX ends correctly   |'
WRITE(ILUOUT0,*) '|                                |'
WRITE(ILUOUT0,*) '----------------------------------'
CALL CLOSE_ll(CLUOUT0, IOSTAT=IRESP)
CALL IO_FILE_CLOSE_ll(TZFILE,CLUOUT0,IRESP)
!
CALL END_PARA_ll(IINFO_ll)
CALL SURFEX_DEALLO_LIST
!-------------------------------------------------------------------------------
!
END PROGRAM PREP_SURFEX
