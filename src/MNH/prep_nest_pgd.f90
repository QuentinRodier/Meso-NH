!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
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
!!                  2014 (M.Faivre)
!!                  06/2015 (M.Moge) parallelization 
!!      J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!!      J.Escobar : 19/04/2016 : Pb IOZ/NETCDF , missing OPARALLELIO=.FALSE. for PGD files
!!  06/2016     (G.Delautier) phasage surfex 8
!!      P.Wautelet : 08/07/2016 : removed MNH_NCWRIT define
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PARAMETERS
USE MODD_CONF
USE MODD_CST
USE MODD_GRID_n, ONLY : XZSMT
USE MODD_LUNIT
USE MODD_NESTING
USE MODD_CONF_n
USE MODD_IO_ll, ONLY : TFILEDATA, TFILE_SURFEX
!
USE MODI_OPEN_NESTPGD_FILES
USE MODI_RETRIEVE1_NEST_INFO_n
USE MODI_DEFINE_MASK_n
USE MODI_NEST_FIELD_n
USE MODI_NEST_ZSMT_n
USE MODI_READ_HGRID
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
USE MODI_READ_ALL_NAMELISTS
USE MODI_INIT_HORGRID_ll_n
USE MODI_INIT_PGD_SURF_ATM
USE MODI_WRITE_PGD_SURF_ATM_N
USE MODD_MNH_SURFEX_n
!
USE MODE_SPLITTINGZ_ll, ONLY : INI_PARAZ_ll
USE MODD_VAR_ll, ONLY : NPROC, IP, NMNH_COMM_WORLD
USE MODE_MNH_WORLD, ONLY : INIT_NMNH_COMM_WORLD
USE MODE_MPPDB
!
USE MODD_DIM_n

IMPLICIT NONE
!
!*       0.1   Declaration of local variables
!              ------------------------------
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
INTEGER                        :: IIMAX,IJMAX,IKMAX
INTEGER                        :: IDXRATIO,IDYRATIO
INTEGER                        :: IDAD
INTEGER                        :: II
LOGICAL     :: GISINIT
!
TYPE(TFILEDATA),DIMENSION(:),ALLOCATABLE        :: TZFILEPGD     ! Input  PGD files
TYPE(TFILEDATA),DIMENSION(:),ALLOCATABLE,TARGET :: TZFILENESTPGD ! Output PGD files
!
!-------------------------------------------------------------------------------
!
CALL MPPDB_INIT()
!
CALL MPPDB_INIT()
!
CALL VERSION
CPROGRAM='NESPGD'
!
CALL INITIO_ll()
!!$CALL SET_JP_ll(JPMODELMAX,JPHEXT,JPVEXT,JPHEXT)
!
!*       1.    INITIALIZATION OF PHYSICAL CONSTANTS
!              ------------------------------------
!
CALL INI_CST
!
!-------------------------------------------------------------------------------
!
!*       2.    OPENING OF THE FILES
!              ---------------------
!
NVERB=1
!
CALL OPEN_NESTPGD_FILES(TZFILEPGD,TZFILENESTPGD)
CALL SET_JP_ll(JPMODELMAX,JPHEXT,JPVEXT,JPHEXT)
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
!
CALL SURFEX_ALLOC_LIST(NMODEL)
YSURF_CUR => YSURF_LIST(1)
CALL READ_ALL_NAMELISTS(YSURF_CUR,'MESONH','PRE',.FALSE.)
!
!-------------------------------------------------------------------------------
!
!*       3.    READING OF THE GRIDS
!              --------------------
!
! INITIALIZE MPI :
IINFO_ll = 0
CALL MPI_INITIALIZED(GISINIT, IINFO_ll)
IF (.NOT. GISINIT) THEN
  CALL INIT_NMNH_COMM_WORLD(IINFO_ll)
END IF
CALL MPI_COMM_RANK(NMNH_COMM_WORLD, IP, IINFO_ll)
IP = IP+1
CALL MPI_COMM_SIZE(NMNH_COMM_WORLD, NPROC, IINFO_ll)
!
CALL SET_DAD0_ll()
DO JPGD=1,NMODEL
  ! read and set dimensions and ratios of model JPGD
  CALL FMREAD(TZFILEPGD(JPGD)%CNAME,'IMAX',CLUOUT0,'--',IIMAX,IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(TZFILEPGD(JPGD)%CNAME,'JMAX',CLUOUT0,'--',IJMAX,IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(TZFILEPGD(JPGD)%CNAME,'DXRATIO',CLUOUT0,'--',NDXRATIO_ALL(JPGD),IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(TZFILEPGD(JPGD)%CNAME,'DYRATIO',CLUOUT0,'--',NDYRATIO_ALL(JPGD),IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(TZFILEPGD(JPGD)%CNAME,'XSIZE',CLUOUT0,'--',NXSIZE(JPGD),IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(TZFILEPGD(JPGD)%CNAME,'YSIZE',CLUOUT0,'--',NYSIZE(JPGD),IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(TZFILEPGD(JPGD)%CNAME,'XOR',CLUOUT0,'--',NXOR_ALL(JPGD),IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(TZFILEPGD(JPGD)%CNAME,'YOR',CLUOUT0,'--',NYOR_ALL(JPGD),IGRID,ILENCH,YCOMMENT,IRESP)
  CALL SET_DIM_ll(IIMAX, IJMAX, 1)
  ! compute origin and end of local subdomain of model JPGD
  ! initialize variables from MODD_NESTING, origin and end of global model JPGD in coordinates of its father
  IF ( NDAD(JPGD) > 0 ) THEN
    NXEND_ALL(JPGD) = NXOR_ALL(JPGD) + NXSIZE(JPGD) - 1 + 2*JPHEXT
    NYEND_ALL(JPGD) = NYOR_ALL(JPGD) + NYSIZE(JPGD) - 1 + 2*JPHEXT
  ELSE  ! this is not a son model
    NXOR_ALL(JPGD) = 1
    NXEND_ALL(JPGD) = IIMAX+2*JPHEXT
    NYOR_ALL(JPGD) = 1
    NYEND_ALL(JPGD) = IJMAX+2*JPHEXT
    NDXRATIO_ALL(JPGD) = 1
    NDYRATIO_ALL(JPGD) = 1
  ENDIF
  ! initialize variables from MODD_DIM_ll, origin and end of global model JPGD in coordinates of its father
  CALL SET_XOR_ll(NXOR_ALL(JPGD), JPGD)
  CALL SET_XEND_ll(NXEND_ALL(JPGD), JPGD)
  CALL SET_YOR_ll(NYOR_ALL(JPGD), JPGD)
  CALL SET_YEND_ll(NYEND_ALL(JPGD), JPGD)
  ! set the father model of model JPGD
! set MODD_NESTING::NDAD using MODD_DIM_ll::NDAD
! MODD_DIM_ll::NDAD was filled in OPEN_NESTPGD_FILES
  CALL SET_DAD_ll(NDAD(JPGD), JPGD)
  ! set the ratio of model JPGD in MODD_DIM_ll
  CALL SET_XRATIO_ll(NDXRATIO_ALL(JPGD), JPGD)
  CALL SET_YRATIO_ll(NDYRATIO_ALL(JPGD), JPGD)
END DO
!
! reading of the grids
!
  CALL SET_DIM_ll(NXEND_ALL(1)-NXOR_ALL(1)+1-2*JPHEXT, NYEND_ALL(1)-NYOR_ALL(1)+1-2*JPHEXT, 1) 
  CALL INI_PARAZ_ll(IINFO_ll)
DO JPGD=1,NMODEL
  CALL GOTO_MODEL(JPGD)
  CALL GO_TOMODEL_ll(JPGD,IINFO_ll)
  CALL GOTO_SURFEX(JPGD)
  CALL FMREAD(TZFILEPGD(JPGD)%CNAME,'L1D         ',CLUOUT0,'--',L1D_ALL(JPGD),IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(TZFILEPGD(JPGD)%CNAME,'L2D         ',CLUOUT0,'--',L2D_ALL(JPGD),IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(TZFILEPGD(JPGD)%CNAME,'PACK        ',CLUOUT0,'--',LPACK_ALL(JPGD),IGRID,ILENCH,YCOMMENT,IRESP)
  CALL SET_FMPACK_ll(L1D_ALL(JPGD),L2D_ALL(JPGD),LPACK_ALL(JPGD))
  CALL READ_HGRID(JPGD,TZFILEPGD(JPGD)%CNAME,YMY_NAME,YDAD_NAME,YSTORAGE_TYPE)
  CSTORAGE_TYPE='PG'
END DO
  CALL INI_PARAZ_ll(IINFO_ll)
!
!-------------------------------------------------------------------------------
!
!*       5.    MASKS DEFINITIONS
!              -----------------
!

DO JPGD=1,NMODEL
  CALL GOTO_SURFEX(JPGD)
  CALL GOTO_MODEL(JPGD)
  CALL GO_TOMODEL_ll(JPGD,IINFO_ll)
!!$  CALL INIT_HORGRID_ll_n()
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
  CALL GO_TOMODEL_ll(JPGD,IINFO_ll)
  CALL GOTO_SURFEX(JPGD)
  CALL NEST_FIELD_n('ZS    ')
END DO
!
! *** Adaptation of smooth topography for SLEVE coordinate
!
WRITE(ILUOUT0,FMT=*)
WRITE(ILUOUT0,FMT=*) 'field ZSMT of all models'
DO JPGD=1,NMODEL
  CALL GOTO_MODEL(JPGD)
  CALL GO_TOMODEL_ll(JPGD,IINFO_ll)
  CALL GOTO_SURFEX(JPGD)
  CALL NEST_ZSMT_n('ZSMT  ')
END DO

!
!-------------------------------------------------------------------------------
!
!*       7.    SURFACE FIELDS READING
!              ----------------------
!
DO JPGD=1,NMODEL
  IF (LEN_TRIM(TZFILEPGD(JPGD)%CNAME)>0) THEN
    CALL GO_TOMODEL_ll(JPGD,IINFO_ll)
    CPGDFILE = TZFILEPGD(JPGD)%CNAME
    CALL GOTO_MODEL(JPGD)
    CALL GOTO_SURFEX(JPGD)
    CALL INIT_PGD_SURF_ATM(YSURF_CUR,'MESONH','PGD',                         &
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
  CALL GO_TOMODEL_ll(JPGD,IINFO_ll)
  CALL GOTO_SURFEX(JPGD)
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
  CPGDFILE   = TZFILEPGD(JPGD)%CNAME
  COUTFMFILE = TZFILENESTPGD(JPGD)%CNAME
  CALL GOTO_MODEL(JPGD)
  CALL GOTO_SURFEX(JPGD)
  TFILE_SURFEX => TZFILENESTPGD(JPGD)
  CALL WRITE_PGD_SURF_ATM_n(YSURF_CUR,'MESONH')
  NULLIFY(TFILE_SURFEX)
  CALL IO_WRITE_FIELD(TZFILENESTPGD(JPGD),'ZSMT',XZSMT)
END DO
!
!-------------------------------------------------------------------------------
!
!*      12.    Write configuration variables in the output file
!              ------------------------------------------------
!
!
DO JPGD=1,NMODEL
  CALL IO_WRITE_HEADER(TZFILENESTPGD(JPGD))
  IF ( ASSOCIATED(TZFILENESTPGD(JPGD)%TDADFILE) ) THEN
    CALL IO_WRITE_FIELD(TZFILENESTPGD(JPGD),'DXRATIO',NDXRATIO_ALL(JPGD))
    CALL IO_WRITE_FIELD(TZFILENESTPGD(JPGD),'DYRATIO',NDYRATIO_ALL(JPGD))
    CALL IO_WRITE_FIELD(TZFILENESTPGD(JPGD),'XOR',    NXOR_ALL(JPGD))
    CALL IO_WRITE_FIELD(TZFILENESTPGD(JPGD),'YOR',    NYOR_ALL(JPGD))
  END IF
  CALL IO_WRITE_FIELD(TZFILENESTPGD(JPGD),'SURF',  'EXTE')
  CALL IO_WRITE_FIELD(TZFILENESTPGD(JPGD),'L1D',   L1D_ALL(JPGD))
  CALL IO_WRITE_FIELD(TZFILENESTPGD(JPGD),'L2D',   L2D_ALL(JPGD))
  CALL IO_WRITE_FIELD(TZFILENESTPGD(JPGD),'PACK',  LPACK_ALL(JPGD))
  CALL IO_WRITE_FIELD(TZFILENESTPGD(JPGD),'JPHEXT',JPHEXT)
END DO
!
!-------------------------------------------------------------------------------
!
!*      13.    CLOSING OF THE FILES
!              --------------------
!
DO JPGD=1,NMODEL
  CALL IO_FILE_CLOSE_ll(TZFILEPGD(JPGD),    CLUOUT0,IRESP,OPARALLELIO=.FALSE.)
  CALL IO_FILE_CLOSE_ll(TZFILENESTPGD(JPGD),CLUOUT0,IRESP,OPARALLELIO=.FALSE.)
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
CALL SURFEX_DEALLO_LIST
!
!-------------------------------------------------------------------------------

END PROGRAM PREP_NEST_PGD
