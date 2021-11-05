!MNH_LIC Copyright 1999-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!#########################
MODULE MODI_INI_SIZE_SPAWN
!#########################
!
INTERFACE
!
SUBROUTINE INI_SIZE_SPAWN(HLBCX,HLBCY,HPRESOPT,KITR,TPINIFILE)
!
USE MODD_IO, ONLY : TFILEDATA
!
CHARACTER (LEN=4),DIMENSION(2), INTENT(IN)    :: HLBCX,HLBCY ! LBC types for model1
CHARACTER (LEN=5),              INTENT(IN)    :: HPRESOPT    ! Pressure solver option of model1
INTEGER,                        INTENT(IN)    :: KITR        ! Iterations of pressure solver of model1
TYPE(TFILEDATA),                INTENT(IN)    :: TPINIFILE   ! Model 1 file
!
END SUBROUTINE INI_SIZE_SPAWN
!
END INTERFACE
!
END MODULE MODI_INI_SIZE_SPAWN
!
!
!     ##############################################################
      SUBROUTINE INI_SIZE_SPAWN(HLBCX,HLBCY,HPRESOPT,KITR,TPINIFILE)
!     ##############################################################
!
!!****  *INI_SIZE_SPAWN * - subroutine to compute dimensions and position of model 2,
!!                          initialize its LBC and call the // initialisation routines
!!                          and fill variables in MODD_PGDGRID before possibly testing
!!                          coherence between model 1 and spawned grid
!
!!    PURPOSE
!!    -------
!!
!!      This subroutine is only for spawning purpose. It ends the initialization of the
!!      MODD_SPAWN variables corresponding to the model2 configuration and call
!!      // routines .
!
!!    EXTERNAL
!!    --------
!!    DEFAULT_DESFM2
!!    IO_File_open
!!    READ_HGRID
!!    IO_File_close
!!    RETRIEVE_NEST_INFO
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    AUTHOR
!!    ------
!!
!!       P. Jabouille     * METEO-FRANCE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original     13/07/99
!!         M.Faivre  2014
!!         M.Moge    07/2015  bug fix : files opened multiple times
!!         M.Moge    08/2015  bug fix : turning the special case for // case into general case in part 1.4
!!         J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1
!!         J.Escobar : 19/04/2016 : Pb IOZ/NETCDF , missing OPARALLELIO=.FALSE. for PGD files
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 07/02/2019: force TYPE to a known value for IO_File_add2list
!  P. Wautelet 07/02/2019: remove OPARALLELIO argument from open and close files subroutines
!                          (nsubfiles_ioz is now determined in IO_File_add2list)
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CONF
USE MODD_DIM_n,            ONLY: DIM_MODEL
USE MODD_DYN_n,            ONLY: CPRESOPT, NITR
use modd_field,            only: tfieldmetadata, tfieldlist
USE MODD_GRID
USE MODD_GRID_n
USE MODD_IO,               ONLY: ISNPROC, ISP, TFILEDATA
USE MODD_LBC_n
USE MODD_LUNIT_n
USE MODD_PARAMETERS
USE MODD_PGDDIM
USE MODD_PGDGRID
USE MODD_SPAWN
USE MODD_VAR_ll,           ONLY: YSPLITTING
!
use mode_field,            only: Find_field_id_from_mnhname
USE MODE_GRIDPROJ
USE MODE_IO_FIELD_READ,    only: IO_Field_read
USE MODE_IO_FILE,          only: IO_File_close, IO_File_open
USE MODE_IO_MANAGE_STRUCT, ONLY: IO_File_add2list
USE MODE_ll
USE MODE_MSG
USE MODE_MODELN_HANDLER
USE MODE_SPLITTINGZ_ll
!
USE MODI_COMPARE_DAD
USE MODI_DEFAULT_DESFM_n   ! Only for model 2
USE MODI_READ_HGRID
USE MODI_RETRIEVE1_NEST_INFO_n
!
IMPLICIT NONE
!
!*       0.1  Declarations of dummy arguments :
!
CHARACTER (LEN=4),DIMENSION(2), INTENT(IN)    :: HLBCX,HLBCY ! LBC types for model1
CHARACTER (LEN=5),              INTENT(IN)    :: HPRESOPT    ! Pressure solver option of model1
INTEGER,                        INTENT(IN)    :: KITR        ! Iterations of pressure solver of model1
TYPE(TFILEDATA),                INTENT(IN)    :: TPINIFILE   ! Model 1 file
!
!*       0.2  Declarations of local variables :
!
INTEGER :: IRESP    ! Return codes in FM routines
INTEGER :: ILUOUT   ! Logical unit number for the output listing
!
CHARACTER (LEN=5)  :: YPRESOPT        ! Pressure solver option of model 1
INTEGER            :: IITR            ! Iterations of pressure solver of model 1
CHARACTER (LEN=28) :: YMY_NAME, YDAD_NAME
CHARACTER (LEN=2)  :: YSTORAGE_TYPE
INTEGER            :: IID, IMI
!
!$20140602
INTEGER            :: IIU, IJU
INTEGER            :: IINFO_ll    ! return code of // routines
INTEGER            :: NIMAX, NJMAX
CHARACTER(LEN=28), DIMENSION(JPMODELMAX) :: CPGD     ! name of input  pgd files
LOGICAL, DIMENSION(JPMODELMAX) :: L1D_ALL  ! Flag for      1D conf. for each PGD
LOGICAL, DIMENSION(JPMODELMAX) :: L2D_ALL  ! Flag for      2D conf. for each PGD
LOGICAL, DIMENSION(JPMODELMAX) :: LPACK_ALL! Flag for packing conf. for each PGD
INTEGER            :: IDIMX, IDIMY, IIB, IJB, IIE, IJE
!$
REAL :: ZLATOR, ZLONOR, ZXHATM, ZYHATM
INTEGER :: IIMAX_ll,IJMAX_ll
TYPE(TFIELDMETADATA)    :: TZFIELD
TYPE(TFILEDATA),POINTER :: TZDOMAIN => NULL()
!
!
IMI = GET_CURRENT_MODEL_INDEX()
CALL GOTO_MODEL(2)
!
ILUOUT = TLUOUT%NLU
!
!*   1.    INITIALIZATIONS :
!
!*     1.1   set default values :
!
YPRESOPT = HPRESOPT
IITR     = KITR
CALL DEFAULT_DESFM_n(2)
CPRESOPT = YPRESOPT
NITR     = IITR
!
IF (NDXRATIO==NUNDEF) NDXRATIO=1
IF (NDYRATIO==NUNDEF) NDYRATIO=1
IF (NXSIZE  ==NUNDEF) NXSIZE  =DIM_MODEL(1)%NIMAX_ll
IF (NYSIZE  ==NUNDEF) NYSIZE  =DIM_MODEL(1)%NJMAX_ll
IF (NXOR    ==NUNDEF) NXOR    =1
IF (NYOR    ==NUNDEF) NYOR    =1
!
!*     1.2   special cases :
!
IF (L1D) THEN
  NXSIZE=1
  NDXRATIO=1
ENDIF
!
IF (L1D .OR. L2D) THEN
  NYSIZE=1
  NDYRATIO=1
ENDIF
!
IF (LBAL_ONLY) THEN
  NDXRATIO=1
  NDYRATIO=1
  NXSIZE  =DIM_MODEL(1)%NIMAX_ll
  NYSIZE  =DIM_MODEL(1)%NJMAX_ll
  NXOR    =1
  NYOR    =1
  IF (LEN_TRIM(CDADSPAFILE) >0 ) THEN
    IF (LEN_TRIM(CDADINIFILE) == 0 ) THEN
!callabortstop
      CALL PRINT_MSG(NVERB_FATAL,'GEN','INI_SIZE_SPAWN','YDADINIFILE not initialized in namelist NAM_LUNIT2_SPA')
    ELSE
      CALL IO_Field_read(TPINIFILE,'DAD_NAME',YDAD_NAME)
      IF (ADJUSTL(ADJUSTR(YDAD_NAME)) .NE. ADJUSTL(ADJUSTR(CDADINIFILE))) THEN
        WRITE(ILUOUT,*) 'ERROR in INI_SIZE_SPAWN: YDADINIFILE is NOT the DAD of model 1'
        WRITE(ILUOUT,*) ' YDADINIFILE='//TRIM(CDADINIFILE)
        WRITE(ILUOUT,*) ' DAD_NAME of model1='//TRIM(YDAD_NAME)
!callabortstop
        CALL PRINT_MSG(NVERB_FATAL,'GEN','INI_SIZE_SPAWN','')
      ELSE
        !
        CALL COMPARE_DAD(CDADINIFILE,CDADSPAFILE,IRESP)
        IF (IRESP .NE. 0) THEN
!callabortstop
          CALL PRINT_MSG(NVERB_FATAL,'GEN','INI_SIZE_SPAWN','unable to replace the DAD of model 1 with YDADSPAFILE')
        ENDIF
        !
      ENDIF
    ENDIF
  ENDIF
ENDIF
!
!
!*     1.3   set some variables related to model 1 grid
!
IF (LEN_TRIM(CDOMAIN)>0) THEN
!
  CALL IO_Field_read(TPINIFILE,'LAT0',  XLAT0)
  CALL IO_Field_read(TPINIFILE,'LON0',  XLON0)
  CALL IO_Field_read(TPINIFILE,'RPK',   XRPK)
  CALL IO_Field_read(TPINIFILE,'BETA',  XBETA)
  CALL IO_Field_read(TPINIFILE,'LATORI',XPGDLATOR)
  CALL IO_Field_read(TPINIFILE,'LONORI',XPGDLONOR)
  !
  !$20140602 INSERT BIG MODIF JUAN May27
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!*     1.4   read grid in file CDOMAIN if available :
! initialize grid2 dims, xor, xend and ratio so to initialize in INI_CHILD 
! structures TCRRT_COMDATA%T_CHILDREN%T_SPLITB and TCRRT_PROCONF%T_CHILDREN
!$20140602 add condition on npproc
  CALL IO_File_add2list(TZDOMAIN,TRIM(CDOMAIN),'PGD','READ',KLFITYPE=2,KLFIVERB=NVERB)
  CALL IO_File_open(TZDOMAIN)
  !
  CALL IO_Field_read(TZDOMAIN,'DXRATIO',NDXRATIO)
  CALL IO_Field_read(TZDOMAIN,'DYRATIO',NDYRATIO)
  CALL IO_Field_read(TZDOMAIN,'XOR',    NXOR)
  CALL IO_Field_read(TZDOMAIN,'YOR',    NYOR)
  CALL IO_Field_read(TZDOMAIN,'IMAX',   IIMAX_ll)
  CALL IO_Field_read(TZDOMAIN,'JMAX',   IJMAX_ll)
  NXEND=NXOR+IIMAX_ll/NDXRATIO+2*JPHEXT-1
  NYEND=NYOR+IJMAX_ll/NDYRATIO+2*JPHEXT-1
  !
  !*   1.5    CALL OF INITIALIZATION PARALLEL ROUTINES
  !
  CALL SET_LBX_ll(CLBCX(1), 2)
  CALL SET_LBY_ll(CLBCY(1), 2)
  CALL SET_XRATIO_ll(NDXRATIO, 2)
  CALL SET_YRATIO_ll(NDYRATIO, 2)
  CALL SET_XOR_ll(NXOR, 2)
  CALL SET_XEND_ll(NXEND, 2)
  CALL SET_YOR_ll(NYOR, 2)
  CALL SET_YEND_ll(NYEND, 2)
  CALL SET_DAD_ll(1, 2)
  !
  CALL INI_PARAZ_ll(IINFO_ll)
  ! get dimensions of father model
  CALL GET_DIM_PHYS_ll( YSPLITTING, DIM_MODEL(1)%NIMAX, DIM_MODEL(1)%NJMAX )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !$
  ALLOCATE(XPGDXHAT(DIM_MODEL(1)%NIMAX+2*JPHEXT))
  CALL IO_Field_read(TPINIFILE,'XHAT',XPGDXHAT)
  !
  ALLOCATE(XPGDYHAT(DIM_MODEL(1)%NJMAX+2*JPHEXT))
  CALL IO_Field_read(TPINIFILE,'YHAT',XPGDYHAT)
  !
  IF (TPINIFILE%NMNHVERSION(1)<4 .OR. (TPINIFILE%NMNHVERSION(1)==4 .AND. TPINIFILE%NMNHVERSION(2)<=5)) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('LONORI',IID,IRESP)
    TZFIELD = TFIELDMETADATA( TFIELDLIST(IID) )
    TZFIELD%CMNHNAME = 'LONOR'
    CALL IO_Field_read(TPINIFILE,TZFIELD,XPGDLONOR)
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME('LATORI',IID,IRESP)
    TZFIELD = TFIELDMETADATA( TFIELDLIST(IID) )
    TZFIELD%CMNHNAME = 'LATOR'
    CALL IO_Field_read(TPINIFILE,TZFIELD,XPGDLATOR)
    !
    ZXHATM = - 0.5 * (XPGDXHAT(1)+XPGDXHAT(2))
    ZYHATM = - 0.5 * (XPGDYHAT(1)+XPGDYHAT(2))
    CALL SM_LATLON(XPGDLATOR,XPGDLONOR,ZXHATM,ZYHATM,ZLATOR,ZLONOR)
    XPGDLATOR = ZLATOR
    XPGDLONOR = ZLONOR
  END IF
  !
!
!*     1.4   read grid in file CDOMAIN if available :
!
  CALL READ_HGRID(2,TZDOMAIN,YMY_NAME,YDAD_NAME,YSTORAGE_TYPE)
  CALL IO_File_close(TZDOMAIN)
  CALL RETRIEVE1_NEST_INFO_n(1,2,NXOR,NYOR,NXSIZE,NYSIZE,NDXRATIO,NDYRATIO)
  DEALLOCATE(XZS,XZSMT,XXHAT,XYHAT)
!
END IF
!
!*     1.5   Position of model 2 domain relative to model 1 
!
NXEND = NXOR + NXSIZE +2*JPHEXT -1
NYEND = NYOR + NYSIZE +2*JPHEXT -1
!
!*     1.6  model 2 LBC   (caution: implicitely JPHEXT = 1)
!
CLBCX(:) = 'OPEN'
IF (NXOR  == 1          .AND. NXEND    == DIM_MODEL(1)%NIMAX_ll+2*JPHEXT) CLBCX(:) = HLBCX(:)
IF (NXOR  == 1          .AND. HLBCX(1) == 'WALL')     CLBCX(1) = 'WALL'
IF (NXEND == DIM_MODEL(1)%NIMAX_ll+2*JPHEXT .AND. HLBCX(2) == 'WALL')     CLBCX(2) = 'WALL'
!
CLBCY(:) = 'OPEN'
IF (NYOR  == 1          .AND. NYEND    == DIM_MODEL(1)%NJMAX_ll+2*JPHEXT) CLBCY(:) = HLBCY(:)
IF (NYOR  == 1          .AND. HLBCY(1) == 'WALL')     CLBCY(1) = 'WALL'
IF (NYEND == DIM_MODEL(1)%NJMAX_ll+2*JPHEXT .AND. HLBCY(2) == 'WALL')     CLBCY(2) = 'WALL'
!
!
!*   2    CALL OF INITIALIZATION PARALLEL ROUTINES
!
CALL SET_LBX_ll(CLBCX(1), 2)
CALL SET_LBY_ll(CLBCY(1), 2)
CALL SET_XRATIO_ll(NDXRATIO, 2)
CALL SET_YRATIO_ll(NDYRATIO, 2)
CALL SET_XOR_ll(NXOR, 2)
CALL SET_XEND_ll(NXEND, 2)
CALL SET_YOR_ll(NYOR, 2)
CALL SET_YEND_ll(NYEND, 2)
CALL SET_DAD_ll(1, 2)
!
CALL GOTO_MODEL(IMI)
!
END SUBROUTINE INI_SIZE_SPAWN
