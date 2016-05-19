!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 spawn 2006/05/23 15:45:38
!-----------------------------------------------------------------
!#########################
MODULE MODI_INI_SIZE_SPAWN
!#########################
!
INTERFACE
!
SUBROUTINE INI_SIZE_SPAWN(HLBCX,HLBCY,HPRESOPT,KITR,HINIFILE)
!
CHARACTER (LEN=4),DIMENSION(2), INTENT(IN)  :: HLBCX,HLBCY !  LBC types for model1
CHARACTER (LEN=5),  INTENT(IN)              :: HPRESOPT        ! Pressure solver option of model1
INTEGER,            INTENT(IN)              :: KITR            ! Iterations of pressure solver of model1
CHARACTER (LEN=*),  INTENT(IN)              :: HINIFILE ! name of the model 1 file
!
END SUBROUTINE INI_SIZE_SPAWN
!
END INTERFACE
!
END MODULE MODI_INI_SIZE_SPAWN
!
!
!     #############################################################
      SUBROUTINE INI_SIZE_SPAWN(HLBCX,HLBCY,HPRESOPT,KITR,HINIFILE)
!     #############################################################
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
!!    FMLOOK_ll
!!    DEFAULT_DESFM2
!!    FMOPEN_ll
!!    READ_HGRID
!!    FMCLOS_ll
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
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SPAWN
USE MODD_PARAMETERS
USE MODD_CONF
USE MODD_GRID
USE MODD_PGDGRID
USE MODD_PGDDIM
!
USE MODE_ll
USE MODE_IO_ll
USE MODE_FM
USE MODE_FMREAD
USE MODE_GRIDPROJ
!
USE MODD_DIM_n, ONLY : DIM_MODEL
!
USE MODD_DYN_n, ONLY : CPRESOPT, NITR
USE MODD_LBC_n
USE MODD_GRID_n
USE MODD_LUNIT_n
!
USE MODI_DEFAULT_DESFM_n   ! Only for model 2
USE MODI_READ_HGRID
USE MODI_RETRIEVE1_NEST_INFO_n
USE MODI_COMPARE_DAD
USE MODE_MODELN_HANDLER
!
!
IMPLICIT NONE
!
!*       0.1  Declarations of dummy arguments :
!
CHARACTER (LEN=4),DIMENSION(2), INTENT(IN)  :: HLBCX,HLBCY !  LBC types for model1
CHARACTER (LEN=5),  INTENT(IN)              :: HPRESOPT        ! Pressure solver option of model1
INTEGER,            INTENT(IN)              :: KITR            ! Iterations of pressure solver of model1
CHARACTER (LEN=*),  INTENT(IN)              :: HINIFILE ! name of the model 1 file
!
!*       0.2  Declarations of local variables :
!
INTEGER :: IRESP    ! Return codes in FM routines
INTEGER :: ILUOUT   ! Logical unit number for the output listing
INTEGER :: ININAR   ! Number of articles present in the LFIFM file
INTEGER :: IMASDEV
CHARACTER(LEN=2)    :: YDIR   ! Type  of the data field in LFIFM file
!
CHARACTER (LEN=28) :: YINIFILE        ! Name of the model 1 FM-file
CHARACTER (LEN=5)  :: YPRESOPT        ! Pressure solver option of model 1
INTEGER            :: IITR            ! Iterations of pressure solver of model 1
CHARACTER (LEN=28) :: YMY_NAME, YDAD_NAME
CHARACTER (LEN=2)  :: YSTORAGE_TYPE
CHARACTER (LEN=16) :: YRECFM
INTEGER            :: ILENCH, IGRID
CHARACTER (LEN=100):: YCOMMENT
INTEGER            :: IMI
!
!-------------------------------------------------------------------------------
REAL :: ZLATOR, ZLONOR, ZXHATM, ZYHATM
!-------------------------------------------------------------------------------
!
IMI = GET_CURRENT_MODEL_INDEX()
CALL GOTO_MODEL(2)
!
CALL FMLOOK_ll(CLUOUT,CLUOUT,ILUOUT,IRESP)
!
!*   1.    INITIALIZATIONS :
!
!*     1.1   set default values :
!
YINIFILE = CINIFILE
YPRESOPT = HPRESOPT
IITR     = KITR
CALL DEFAULT_DESFM_n(2)
CINIFILE = YINIFILE
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
      WRITE(ILUOUT,*) 'ERROR in INI_SIZE_SPAWN: YDADINIFILE not initialized in namelist NAM_LUNIT2_SPA'
!callabortstop
      CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
      CALL ABORT
      STOP
    ELSE
      YRECFM='DAD_NAME'
      YDIR='--'
      CALL FMREAD(HINIFILE,YRECFM,CLUOUT,YDIR,YDAD_NAME,IGRID,ILENCH,YCOMMENT,IRESP)
      IF (ADJUSTL(ADJUSTR(YDAD_NAME)) .NE. ADJUSTL(ADJUSTR(CDADINIFILE))) THEN
        WRITE(ILUOUT,*) 'ERROR in INI_SIZE_SPAWN: YDADINIFILE is NOT the DAD of model 1'
        WRITE(ILUOUT,*) ' YDADINIFILE='//TRIM(CDADINIFILE)
        WRITE(ILUOUT,*) ' DAD_NAME of model1='//TRIM(YDAD_NAME)
!callabortstop
        CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
        CALL ABORT
        STOP
      ELSE
        !
        CALL COMPARE_DAD(CDADINIFILE,CDADSPAFILE,IRESP)
        IF (IRESP .NE. 0) THEN
          WRITE(ILUOUT,*) 'ERROR in INI_SIZE_SPAWN: Unable to replace the DAD of model 1 with YDADSPAFILE'
!callabortstop
          CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
          CALL ABORT
          STOP
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
  YRECFM='LON0'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,CLUOUT,YDIR,XLON0,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='LAT0'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,CLUOUT,YDIR,XLAT0,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='BETA'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,CLUOUT,YDIR,XBETA,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='RPK'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,CLUOUT,YDIR,XRPK,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='LONORI'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,CLUOUT,YDIR,XPGDLONOR,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='LATORI'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,CLUOUT,YDIR,XPGDLATOR,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  ALLOCATE(XPGDXHAT(DIM_MODEL(1)%NIMAX_ll+2*JPHEXT))
  YRECFM='XHAT'
  YDIR='XX'
  CALL FMREAD(HINIFILE,YRECFM,CLUOUT,YDIR,XPGDXHAT,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  ALLOCATE(XPGDYHAT(DIM_MODEL(1)%NJMAX_ll+2*JPHEXT))
  YRECFM='YHAT'
  YDIR='YY'
  CALL FMREAD(HINIFILE,YRECFM,CLUOUT,YDIR,XPGDYHAT,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='MASDEV' 
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,CLUOUT,YDIR,IMASDEV,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  IF (IMASDEV<=45) THEN
    CALL FMREAD(HINIFILE,'LATOR',CLUOUT,'--',XPGDLATOR,IGRID,ILENCH,YCOMMENT,IRESP)
    CALL FMREAD(HINIFILE,'LONOR',CLUOUT,'--',XPGDLONOR,IGRID,ILENCH,YCOMMENT,IRESP)
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
  CALL FMOPEN_ll(CDOMAIN,'READ',CLUOUT,0,2,NVERB,ININAR,IRESP)
  CALL READ_HGRID(2,CDOMAIN,YMY_NAME,YDAD_NAME,YSTORAGE_TYPE)
  CALL FMCLOS_ll(CDOMAIN,'KEEP',CLUOUT,IRESP)
  CALL RETRIEVE1_NEST_INFO_n(1,2,NXOR,NYOR,NXSIZE,NYSIZE,NDXRATIO,NDYRATIO)
  DEALLOCATE(XZS,XZSMT,XXHAT,XYHAT)
!
END IF
!
!*     1.5   Position of model 2 domain relative to model 1 
!
NXEND = NXOR + JPHEXT + NXSIZE
NYEND = NYOR + JPHEXT + NYSIZE
!
!*     1.6  model 2 LBC   (caution: implicitely JPHEXT = 1)
!
CLBCX(:) = 'OPEN'
IF (NXOR  == 1          .AND. NXEND    == DIM_MODEL(1)%NIMAX_ll+2) CLBCX(:) = HLBCX(:)
IF (NXOR  == 1          .AND. HLBCX(1) == 'WALL')     CLBCX(1) = 'WALL'
IF (NXEND == DIM_MODEL(1)%NIMAX_ll+2 .AND. HLBCX(2) == 'WALL')     CLBCX(2) = 'WALL'
!
CLBCY(:) = 'OPEN'
IF (NYOR  == 1          .AND. NYEND    == DIM_MODEL(1)%NJMAX_ll+2) CLBCY(:) = HLBCY(:)
IF (NYOR  == 1          .AND. HLBCY(1) == 'WALL')     CLBCY(1) = 'WALL'
IF (NYEND == DIM_MODEL(1)%NJMAX_ll+2 .AND. HLBCY(2) == 'WALL')     CLBCY(2) = 'WALL'
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
