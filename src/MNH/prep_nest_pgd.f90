!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_7 BUG1 2007/06/22 12:37:08
!-----------------------------------------------------------------
!     #####################
      PROGRAM PREP_NEST_PGD
!     #####################
!
!!****  *PREP_NEST_PGD* - to make coherent pgd files for nesting
!!
!!    PURPOSE
!!    -------
!!
!!       The purpose of this program is to prepare pgd files with which
!!       nesting can be performed. A pgd file must be coherent with its
!!       father:
!!         The average of orography of fine model on each of its father grid
!!       mesh must be the same as its father orography.
!!
!!       All the pgd files are read at the begining of the program,
!!       then they are checked, and recursively, the orography of a father
!!       is replaced by the averaged orography from ist son.
!!
!!       The control data are given in the namelist file PRE_NEST.nam
!!
!! &NAM_NEST_PGD1 CPGD='coarser model' /
!! &NAM_NEST_PGD2 CPGD='medium model' , IDAD=1 /
!! &NAM_NEST_PGD3 CPGD='medium model' , IDAD=1 /
!! &NAM_NEST_PGD4 CPGD='fine model' , IDAD=2 /
!! &NAM_NEST_PGD5 CPGD='fine model' , IDAD=2 /
!! &NAM_NEST_PGD6 CPGD='fine model' , IDAD=3 /
!! &NAM_NEST_PGD7 CPGD='very fine model' , IDAD=6 /
!! &NAM_NEST_PGD8 CPGD='very very fine model' , IDAD=7 /
!!
!!        In each namelist is given the name of the pgd file, and the number
!!      of its father. This one MUST be smaller.
!!        There is one output file for each input file, with the suffix
!!      '.nest' added at the end of the file name (even if the file has not
!!      been changed).
!!
!!        In the case of the namelist above, one obtain something like:
!!
!!   +----------------------------------------------------------+
!!   |                                                 1        |
!!   |   +-----------------------+                              |
!!   |   |                    2  |                              |
!!   |   |                       |                              |
!!   |   |              +-+      |                              |
!!   |   | +-------+    |5|      |   +-----------------------+  |
!!   |   | |  4    |    +-+      |   |   +----------+     3  |  |
!!   |   | +-------+             |   |   |+------+ 6|        |  |
!!   |   +-----------------------+   |   || +-+ 7|  |        |  |
!!   |                               |   || |8|  |  |        |  |
!!   |                               |   || +-+  |  |        |  |
!!   |                               |   |+------+  |        |  |
!!   |                               |   +----------+        |  |
!!   |                               +-----------------------+  |
!!   +----------------------------------------------------------+
!!
!!
!!**  METHOD
!!    ------
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
!!      Book 2
!!
!!    AUTHOR
!!    ------
!!	
!!      V.Masson  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    26/09/95
!!                  30/07/97 (Masson) split of mode_lfifm_pgd
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PARAMETERS
USE MODD_CONF
USE MODD_CST
USE MODD_LUNIT
USE MODD_NESTING
USE MODD_CONF_n
!
USE MODI_OPEN_NESTPGD_FILES
USE MODI_RETRIEVE1_NEST_INFO_n
USE MODI_DEFINE_MASK_n
USE MODI_NEST_FIELD_n
USE MODI_NEST_ZSMT_n
USE MODI_READ_HGRID
USE MODI_WRITE_ZSMT_n
!
USE MODE_FM
USE MODE_FMREAD
USE MODE_FMWRIT
USE MODE_IO_ll
USE MODE_ll
!
USE MODE_MODELN_HANDLER
!
USE MODI_VERSION
USE MODI_ALLOC_SURFEX
USE MODI_READ_ALL_NAMELISTS
USE MODI_GOTO_SURFEX
USE MODI_INIT_HORGRID_ll_n
USE MODI_INIT_PGD_SURF_ATM
USE MODI_WRITE_PGD_SURF_ATM_N
USE MODI_DEALLOC_SURFEX
!
#ifdef MNH_NCWRIT
USE MODD_SURF_ATM_GRID_n,     ONLY : XLON, XLAT
USE MODN_NCOUT
USE MODE_UTIL
#endif
!
IMPLICIT NONE
!
!*       0.1   Declaration of local variables
!              ------------------------------
!
CHARACTER(LEN=28), DIMENSION(JPMODELMAX) :: CPGD     ! name of input  pgd files
CHARACTER(LEN=28), DIMENSION(JPMODELMAX) :: CNESTPGD ! name of output pgd files
!
INTEGER, DIMENSION(JPMODELMAX) :: NXSIZE   ! number of grid points for each model
INTEGER, DIMENSION(JPMODELMAX) :: NYSIZE   ! in x and y-directions
                                           ! relatively to its father grid
!
INTEGER                        :: ILUOUT0, IRESP
INTEGER                        :: IINFO_ll ! return code of // routines
INTEGER                        :: JPGD     ! loop control
CHARACTER(LEN=28)              :: YMY_NAME,YDAD_NAME
CHARACTER(LEN=2)               :: YSTORAGE_TYPE
CHARACTER(LEN=100)             :: YCOMMENT
INTEGER                        :: IGRID, ILENCH
LOGICAL, DIMENSION(JPMODELMAX) :: L1D_ALL  ! Flag for      1D conf. for each PGD
LOGICAL, DIMENSION(JPMODELMAX) :: L2D_ALL  ! Flag for      2D conf. for each PGD
LOGICAL, DIMENSION(JPMODELMAX) :: LPACK_ALL! Flag for packing conf. for each PGD

!
INTEGER                        :: JTIME,ITIME
!
!-------------------------------------------------------------------------------
!
!
CALL VERSION
CPROGRAM='NESPGD'
!
CALL INITIO_ll()
CALL SET_JP_ll(JPMODELMAX,JPHEXT,JPVEXT,JPHEXT)
!
!*       1.    INITIALIZATION OF PHYSICAL CONSTANTS
!              ------------------------------------
!
CALL INI_CST
!
!-------------------------------------------------------------------------------
!
!*       2.    OPENNING OF THE FILES
!              ---------------------
!
NVERB=1
!
CALL OPEN_NESTPGD_FILES(CPGD,CNESTPGD)
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
!
CALL ALLOC_SURFEX(NMODEL)
CALL READ_ALL_NAMELISTS('MESONH','PRE',.FALSE.)
!
!-------------------------------------------------------------------------------
!
!*       3.    READING OF THE GRIDS
!              --------------------
!
DO JPGD=1,NMODEL
  CALL GOTO_MODEL(JPGD)
  CALL GOTO_SURFEX(JPGD,.TRUE.)
  CALL FMREAD(CPGD(JPGD),'L1D         ',CLUOUT0,'--',L1D_ALL(JPGD),IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(CPGD(JPGD),'L2D         ',CLUOUT0,'--',L2D_ALL(JPGD),IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(CPGD(JPGD),'PACK        ',CLUOUT0,'--',LPACK_ALL(JPGD),IGRID,ILENCH,YCOMMENT,IRESP)
  CALL SET_FMPACK_ll(L1D_ALL(JPGD),L2D_ALL(JPGD),LPACK_ALL(JPGD))
  CALL READ_HGRID(JPGD,CPGD(JPGD),YMY_NAME,YDAD_NAME,YSTORAGE_TYPE)
  CSTORAGE_TYPE='PG'
END DO
!
!
!-------------------------------------------------------------------------------
!
!*       4.    TESTS ON THE GRIDS
!              ------------------
!
NXOR_ALL(:)=0
NYOR_ALL(:)=0
NXEND_ALL(:)=0
NYEND_ALL(:)=0
NXSIZE(:)=0
NYSIZE(:)=0
NDXRATIO_ALL(:)=0
NDYRATIO_ALL(:)=0
!
!* loop in this order, to make coherent all the coordinate arrays with model 1
!
DO JPGD=2,NMODEL
  CALL RETRIEVE1_NEST_INFO_n(NDAD(JPGD),JPGD,                               &
                          NXOR_ALL(JPGD),NYOR_ALL(JPGD),                 &
                          NXSIZE(JPGD),NYSIZE(JPGD),                     &
                          NDXRATIO_ALL(JPGD),NDYRATIO_ALL(JPGD))
  NXEND_ALL(JPGD)=NXOR_ALL(JPGD)+NXSIZE(JPGD)+1
  NYEND_ALL(JPGD)=NYOR_ALL(JPGD)+NYSIZE(JPGD)+1
END DO
!
!-------------------------------------------------------------------------------
!
!*       5.    MASKS DEFINITIONS
!              -----------------
!
DO JPGD=1,NMODEL
  CALL GOTO_SURFEX(JPGD,.TRUE.)
  CALL GOTO_MODEL(JPGD)
  CALL INIT_HORGRID_ll_n()
  CALL DEFINE_MASK_n()
END DO
!
!-------------------------------------------------------------------------------
!
!*       6.    MODIFICATION OF OROGRAPHY
!              -------------------------
!
WRITE(ILUOUT0,FMT=*)
WRITE(ILUOUT0,FMT=*) 'field ZS   of all models'
DO JPGD=NMODEL,1,-1
  CALL GOTO_MODEL(JPGD)
  CALL GOTO_SURFEX(JPGD,.TRUE.)
  CALL NEST_FIELD_n('ZS    ')
END DO
!
! *** Adaptation of smooth topography for SLEVE coordinate
!
WRITE(ILUOUT0,FMT=*)
WRITE(ILUOUT0,FMT=*) 'field ZSMT of all models'
DO JPGD=1,NMODEL
  CALL GOTO_MODEL(JPGD)
  CALL GOTO_SURFEX(JPGD,.TRUE.)
  CALL NEST_ZSMT_n('ZSMT  ')
END DO

!
!-------------------------------------------------------------------------------
!
!*       7.    SURFACE FIELDS READING
!              ----------------------
!
DO JPGD=1,NMODEL
  IF (LEN_TRIM(CPGD(JPGD))>0) THEN
    CALL GO_TOMODEL_ll(JPGD,IINFO_ll)
    CPGDFILE = CPGD(JPGD)
    CALL GOTO_MODEL(JPGD)
    CALL GOTO_SURFEX(JPGD,.TRUE.)
    CALL INIT_PGD_SURF_ATM('MESONH','PGD',                         &
         '                            ','      ',&
         NUNDEF,NUNDEF,NUNDEF,XUNDEF             )
  END IF
END DO
!
!-------------------------------------------------------------------------------
!
!*       8.    MODIFICATION OF OROGRAPHY
!              -------------------------
!
DO JPGD=1,NMODEL
  CALL GOTO_MODEL(JPGD)
  CALL GOTO_SURFEX(JPGD,.TRUE.)
  CALL MNHPUT_ZS_n
END DO
!
!-------------------------------------------------------------------------------
!
!*      10.    SURFACE FIELDS WRITING
!              ----------------------
!
DO JPGD=1,NMODEL
  CALL GO_TOMODEL_ll(JPGD,IINFO_ll)
  CPGDFILE   = CPGD(JPGD)
  COUTFMFILE = CNESTPGD(JPGD)
  CALL GOTO_MODEL(JPGD)
  CALL GOTO_SURFEX(JPGD,.TRUE.)
#ifdef MNH_NCWRIT
  NC_WRITE = LNETCDF
  CALL WRITE_PGD_SURF_ATM_n('MESONH')
  IF (LNETCDF.AND..NOT.LCARTESIAN) THEN
    LLFIFM = .FALSE.
    LLFIFM = .TRUE.
  END IF
!*      11.    SMOOTH OROGRAPHY WRITING
  CALL WRITE_ZSMT_n(CNESTPGD(JPGD))
  IF ( LNETCDF ) THEN
    DEF_NC=.FALSE.
    CALL WRITE_PGD_SURF_ATM_n('MESONH')
!*      11.    SMOOTH OROGRAPHY WRITING
    CALL WRITE_ZSMT_n(CNESTPGD(JPGD))
    DEF_NC=.TRUE.
    NC_WRITE = .FALSE.
  END IF
#else
  CALL WRITE_PGD_SURF_ATM_n('MESONH')
  CALL WRITE_ZSMT_n(CNESTPGD(JPGD))
#endif
END DO
!
!-------------------------------------------------------------------------------
!
!*      12.    Write configuration variables in the output file
!              ------------------------------------------------
!
!
DO JPGD=1,NMODEL
  CALL FMWRIT(CNESTPGD(JPGD),'MASDEV      ',CLUOUT0,'--',NMASDEV,0,1,' ',IRESP)
  CALL FMWRIT(CNESTPGD(JPGD),'BUGFIX      ',CLUOUT0,'--',NBUGFIX,0,1,' ',IRESP)
  CALL FMWRIT(CNESTPGD(JPGD),'BIBUSER     ',CLUOUT0,'--',CBIBUSER,0,1,' ',IRESP)
  CALL FMWRIT(CNESTPGD(JPGD),'PROGRAM     ',CLUOUT0,'--',CPROGRAM,0,1,' ',IRESP)
  CALL FMWRIT(CNESTPGD(JPGD),'STORAGE_TYPE',CLUOUT0,'--',CSTORAGE_TYPE,0,1,' ',IRESP)
  CALL FMWRIT(CNESTPGD(JPGD),'MY_NAME     ',CLUOUT0,'--',CNESTPGD(JPGD),0,1,' ',IRESP)
  IF (NDAD(JPGD)>=1) THEN
    YDAD_NAME = CNESTPGD(NDAD(JPGD))
  ELSE
    YDAD_NAME = '                           '
  END IF
  CALL FMWRIT(CNESTPGD(JPGD),'DAD_NAME    ',CLUOUT0,'--',YDAD_NAME,0,1,' ',IRESP)
  IF (LEN_TRIM(YDAD_NAME)>0) THEN
    CALL FMWRIT(CNESTPGD(JPGD),'DXRATIO     ',CLUOUT0,'--',NDXRATIO_ALL(JPGD),0,1,' ',IRESP)
    CALL FMWRIT(CNESTPGD(JPGD),'DYRATIO     ',CLUOUT0,'--',NDYRATIO_ALL(JPGD),0,1,' ',IRESP)
    CALL FMWRIT(CNESTPGD(JPGD),'XOR         ',CLUOUT0,'--',NXOR_ALL(JPGD),0,1,' ',IRESP)
    CALL FMWRIT(CNESTPGD(JPGD),'YOR         ',CLUOUT0,'--',NYOR_ALL(JPGD),0,1,' ',IRESP)
  END IF
  CALL FMWRIT(CNESTPGD(JPGD),'SURF        ',CLUOUT0,'--','EXTE',0,1,' ',IRESP)
  CALL FMWRIT(CNESTPGD(JPGD),'L1D         ',CLUOUT0,'--',L1D_ALL(JPGD),0,1,' ',IRESP)
  CALL FMWRIT(CNESTPGD(JPGD),'L2D         ',CLUOUT0,'--',L2D_ALL(JPGD),0,1,' ',IRESP)
  CALL FMWRIT(CNESTPGD(JPGD),'PACK        ',CLUOUT0,'--',LPACK_ALL(JPGD),0,1,' ',IRESP)
END DO
!
!-------------------------------------------------------------------------------
!
!*      13.    CLOSING OF THE FILES
!              --------------------
!
DO JPGD=1,NMODEL
  CALL FMCLOS_ll(CPGD    (JPGD),'KEEP',CLUOUT0,IRESP)
  CALL FMCLOS_ll(CNESTPGD(JPGD),'KEEP',CLUOUT0,IRESP)
END DO
!
!* loop to spare enough time to transfer commands before end of program
ITIME=0
DO JTIME=1,1000000
  ITIME=ITIME+1
END DO
!-------------------------------------------------------------------------------
!
!*      12.    EPILOGUE
!              --------
!
WRITE(ILUOUT0,FMT=*)
WRITE(ILUOUT0,FMT=*) '************************************************'
WRITE(ILUOUT0,FMT=*) '* PREP_NEST_PGD: PREP_NEST_PGD ends correctly. *'
WRITE(ILUOUT0,FMT=*) '************************************************'
CALL CLOSE_ll(CLUOUT0)
!
!-------------------------------------------------------------------------------
!
!*      10.    FINALIZE THE PARALLEL SESSION
!              -----------------------------
!
CALL END_PARA_ll(IINFO_ll)
!
CALL DEALLOC_SURFEX
!
!-------------------------------------------------------------------------------

END PROGRAM PREP_NEST_PGD
