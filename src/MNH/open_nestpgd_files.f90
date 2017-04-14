!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
!-----------------------------------------------------------------
!#############################
MODULE MODI_OPEN_NESTPGD_FILES
!#############################
!
INTERFACE
      SUBROUTINE OPEN_NESTPGD_FILES(TPFILEPGD,TPFILENESTPGD)
!
USE MODD_IO_ll, ONLY : TFILEDATA
!
TYPE(TFILEDATA),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: TPFILEPGD     ! Input  PGD files
TYPE(TFILEDATA),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: TPFILENESTPGD ! Output PGD files
!
END SUBROUTINE OPEN_NESTPGD_FILES
END INTERFACE
END MODULE MODI_OPEN_NESTPGD_FILES
!     ######################################################
      SUBROUTINE OPEN_NESTPGD_FILES(TPFILEPGD,TPFILENESTPGD)
!     ######################################################
!
!!****  *OPEN_NESTPGD_FILES* - openning of the files used in PREP_NEST_PGD
!!                         
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    CAUTION:
!!    This routine supposes the name of the namelist file is 'PRE_NEST_PGD1.nam'.
!!
!!    EXTERNAL
!!    --------
!!
!!    Routine FMOPEN
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_LUNIT     :  contains logical unit names for all models
!!         CLUOUT0  : name of output-listing
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
!!      Original     26/09/96
!!                   30/07/97 (Masson) group MODI_OPEN_LUOUTn
!!                   15/10/01 (I.Mallet) allow namelists in different orders
!!                   07/06/2010 (J.escobar from Ivan Ristic) bug PGI
!!                   30/12/2012 (S.Bielli) Add NAM_NCOUT for netcdf output
!!    J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!!                   11/2015 (M.Moge) disable the creation of files on multiple 
!!                                 Z-levels when using parallel IO for PREP_PGD
!!                   01/2016 (M.Moge) Bug fix : open the output file using Z-parallel IO
!!                   J.Escobar : 19/04/2016 : Pb IOZ/NETCDF , missing OPARALLELIO=.FALSE. for PGD files
!!    P.Wautelet : 08/07/2016 : removed MNH_NCWRIT define
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_LUNIT
USE MODD_CONF
USE MODD_NESTING
USE MODD_PARAMETERS
USE MODD_IO_ll, ONLY : TFILEDATA
!
USE MODI_OPEN_LUOUTn
!
USE MODE_FIELD, ONLY : INI_FIELD_LIST
USE MODE_IO_ll
USE MODE_FM
USE MODE_POS
USE MODE_MSG
!
USE MODE_MODELN_HANDLER
!
USE MODN_CONFIO
!
USE MODD_PARAMETERS, ONLY : JPHEXT  
USE MODD_CONF, ONLY       : NHALO_CONF_MNH => NHALO
!
USE  MODN_CONFZ
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
!
TYPE(TFILEDATA),DIMENSION(:),ALLOCATABLE,       INTENT(OUT) :: TPFILEPGD     ! Input  PGD files
TYPE(TFILEDATA),DIMENSION(:),ALLOCATABLE,TARGET,INTENT(OUT) :: TPFILENESTPGD ! Output PGD files
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
INTEGER :: IRESP      ! return-code if problems eraised
INTEGER :: ILUOUT0    ! logical unit for listing file
LOGICAL :: GFOUND     ! Return code when searching namelist
!
CHARACTER(LEN=28) :: HPRE_NEST_PGD ! name of namelist file
INTEGER           :: IPRE_NEST_PGD ! logical unit of namelist file
!
CHARACTER(LEN=28) :: YPGD      ! name of the pgd file for each model
CHARACTER(LEN=28) :: YLUOUT    ! name of output listing file for each model
CHARACTER(LEN=2)  :: YNEST     ! to define the output pgd file names
CHARACTER(LEN=28) :: YPGD1, YPGD2, YPGD3, YPGD4, &
                     YPGD5, YPGD6, YPGD7, YPGD8
!                    ! name of all pgd files
!                    ! in the namelist
INTEGER           :: IDAD    ! father of one model
INTEGER           :: JPGD    ! loop counter
LOGICAL           :: GADD    !
INTEGER           :: NHALO_MNH
!
INTEGER           :: ILUOUT  ! Logical unit number for the EXSPA file
!
!*       0.3   Declaration of namelists
!              ------------------------
!
NAMELIST/NAM_PGD1/ YPGD1
NAMELIST/NAM_PGD2/ YPGD2, IDAD
NAMELIST/NAM_PGD3/ YPGD3, IDAD
NAMELIST/NAM_PGD4/ YPGD4, IDAD
NAMELIST/NAM_PGD5/ YPGD5, IDAD
NAMELIST/NAM_PGD6/ YPGD6, IDAD
NAMELIST/NAM_PGD7/ YPGD7, IDAD
NAMELIST/NAM_PGD8/ YPGD8, IDAD
NAMELIST/NAM_NEST_PGD/ YNEST
NAMELIST/NAM_CONF_NEST/JPHEXT, NHALO_MNH
!-------------------------------------------------------------------------------
!
!*       1.    SET DEFAULT NAMES
!              -----------------
!
HPRE_NEST_PGD='PRE_NEST_PGD1.nam'
CLUOUT0='OUTPUT_LISTING0'
!
!-------------------------------------------------------------------------------
!
!*       2.    OPENNING OF CLUOUT0
!              -------------------
!
CALL OPEN_ll(UNIT=ILUOUT0,FILE=CLUOUT0,IOSTAT=IRESP,FORM='FORMATTED',ACTION='WRITE', &
     MODE=GLOBAL)
!
!-------------------------------------------------------------------------------
!
!*       3.    OPENNING OF PRE_NEST_PGD1.nam
!              -----------------------------
!
CALL OPEN_ll(UNIT=IPRE_NEST_PGD,FILE=HPRE_NEST_PGD,IOSTAT=IRESP,FORM='FORMATTED',ACTION='READ', &
     MODE=GLOBAL)
!reading of NAM_CONFZ
CALL FMLOOK_ll(HPRE_NEST_PGD,HPRE_NEST_PGD,ILUOUT,IRESP)
CALL POSNAM(IPRE_NEST_PGD,'NAM_CONFZ',GFOUND)
IF (GFOUND) READ(UNIT=IPRE_NEST_PGD,NML=NAM_CONFZ)
!
!JUAN
CALL POSNAM(IPRE_NEST_PGD,'NAM_CONF_NEST',GFOUND)
IF (GFOUND) THEN
   NHALO_MNH = NHALO_CONF_MNH
   READ(UNIT=IPRE_NEST_PGD,NML=NAM_CONF_NEST)
   NHALO_CONF_MNH = NHALO_MNH
END IF
!JUAN
!
!-------------------------------------------------------------------------------
!
!*       4.    READING OF THE OTHER FILE NAMES
!              -------------------------------
!
YPGD1='                            '
YPGD2='                            '
YPGD3='                            '
YPGD4='                            '
YPGD5='                            '
YPGD6='                            '
YPGD7='                            '
YPGD8='                            '
NDAD(:)=0
GADD=.TRUE.
!
DO JPGD=1,JPMODELMAX
  IDAD=0
  IF (JPGD==1) THEN
    CALL POSNAM(IPRE_NEST_PGD,'NAM_PGD1',GFOUND)
    IF (GFOUND) READ(UNIT=IPRE_NEST_PGD,NML=NAM_PGD1)
  END IF
  IF (JPGD==2) THEN
    CALL POSNAM(IPRE_NEST_PGD,'NAM_PGD2',GFOUND)
    IF (GFOUND) READ(UNIT=IPRE_NEST_PGD,NML=NAM_PGD2)
  END IF
  IF (JPGD==3) THEN
    CALL POSNAM(IPRE_NEST_PGD,'NAM_PGD3',GFOUND)
    IF (GFOUND) READ(UNIT=IPRE_NEST_PGD,NML=NAM_PGD3)
  END IF
  IF (JPGD==4) THEN
    CALL POSNAM(IPRE_NEST_PGD,'NAM_PGD4',GFOUND)
    IF (GFOUND) READ(UNIT=IPRE_NEST_PGD,NML=NAM_PGD4)
  END IF
  IF (JPGD==5) THEN
    CALL POSNAM(IPRE_NEST_PGD,'NAM_PGD5',GFOUND)
    IF (GFOUND) READ(UNIT=IPRE_NEST_PGD,NML=NAM_PGD5)
  END IF
  IF (JPGD==6) THEN
    CALL POSNAM(IPRE_NEST_PGD,'NAM_PGD6',GFOUND)
    IF (GFOUND) READ(UNIT=IPRE_NEST_PGD,NML=NAM_PGD6)
  END IF
  IF (JPGD==7) THEN
    CALL POSNAM(IPRE_NEST_PGD,'NAM_PGD7',GFOUND)
    IF (GFOUND) READ(UNIT=IPRE_NEST_PGD,NML=NAM_PGD7)
  END IF
  IF (JPGD==8) THEN
    CALL POSNAM(IPRE_NEST_PGD,'NAM_PGD8',GFOUND)
    IF (GFOUND) READ(UNIT=IPRE_NEST_PGD,NML=NAM_PGD8)
  END IF
  !
  IF (JPGD==1) YPGD=YPGD1
  IF (JPGD==2) YPGD=YPGD2
  IF (JPGD==3) YPGD=YPGD3
  IF (JPGD==4) YPGD=YPGD4
  IF (JPGD==5) YPGD=YPGD5
  IF (JPGD==6) YPGD=YPGD6
  IF (JPGD==7) YPGD=YPGD7
  IF (JPGD==8) YPGD=YPGD8
  !
  IF (LEN_TRIM(YPGD) == 0) THEN
    IF (JPGD==1) THEN
      WRITE(ILUOUT0,*) 'No pgd file was present for model 1 in namelist NAM_PGD1'
!callabortstop
      CALL CLOSE_ll(CLUOUT0,IOSTAT=IRESP)
      CALL ABORT
      STOP
    ELSE
      GADD=.FALSE.
      CYCLE
    END IF
  END IF
  !
  IF ( (IDAD<1 .OR. IDAD>JPMODELMAX) .AND. (JPGD>1) ) THEN
      WRITE(ILUOUT0,*) 'No father indicated for model ',JPGD,' in namelist NAM_PGD',JPGD
!callabortstop
      CALL CLOSE_ll(CLUOUT0,IOSTAT=IRESP)
      CALL ABORT
      STOP
  END IF
  !
  IF (GADD) THEN
    NMODEL=JPGD
    !
    IF (IDAD>=JPGD) THEN
      WRITE(ILUOUT0,*) 'pgd files are not correctly ordered:'
      WRITE(ILUOUT0,*) ' in namelist NAM_PGD',JPGD,' was found IDAD= ', IDAD
!callabortstop
      CALL CLOSE_ll(CLUOUT0,IOSTAT=IRESP)
      CALL ABORT
      STOP
    END IF
    !
    NDAD(JPGD)=IDAD
  END IF
END DO
!
!-------------------------------------------------------------------------------
!
!*       5.    NAMES OF OUTPUT PGD FILES
!              -------------------------
!
CALL POSNAM(IPRE_NEST_PGD,'NAM_NEST_PGD',GFOUND,ILUOUT0)
IF (GFOUND) READ(UNIT=IPRE_NEST_PGD,NML=NAM_NEST_PGD)
!
CALL POSNAM(IPRE_NEST_PGD,'NAM_CONFIO',GFOUND,ILUOUT0)
IF (GFOUND) READ(UNIT=IPRE_NEST_PGD,NML=NAM_CONFIO)
CALL SET_CONFIO_ll(LCDF4, LLFIOUT, LLFIREAD)
!
ALLOCATE(TPFILEPGD    (NMODEL))
ALLOCATE(TPFILENESTPGD(NMODEL))
!
IF (NMODEL>=1) TPFILEPGD(1)%CNAME = TRIM(YPGD1)
IF (NMODEL>=2) TPFILEPGD(2)%CNAME = TRIM(YPGD2)
IF (NMODEL>=3) TPFILEPGD(3)%CNAME = TRIM(YPGD3)
IF (NMODEL>=4) TPFILEPGD(4)%CNAME = TRIM(YPGD4)
IF (NMODEL>=5) TPFILEPGD(5)%CNAME = TRIM(YPGD5)
IF (NMODEL>=6) TPFILEPGD(6)%CNAME = TRIM(YPGD6)
IF (NMODEL>=7) TPFILEPGD(7)%CNAME = TRIM(YPGD7)
IF (NMODEL>=8) TPFILEPGD(8)%CNAME = TRIM(YPGD8)
!
DO JPGD=1,NMODEL
  TPFILENESTPGD(JPGD)%CNAME = TRIM(TPFILEPGD(JPGD)%CNAME)//'.nest'//ADJUSTL(YNEST)
  IF (NDAD(JPGD)>=1) THEN
    TPFILENESTPGD(JPGD)%TDADFILE => TPFILENESTPGD(NDAD(JPGD))
  ELSE
    NULLIFY(TPFILENESTPGD(JPGD)%TDADFILE)
  END IF
END DO
!
TPFILEPGD(:)    %CTYPE = 'PREPPGD'
TPFILENESTPGD(:)%CTYPE = 'PREPNESTPGD'
IF (LCDF4) THEN
  IF (.NOT.LLFIOUT) THEN
    TPFILEPGD(:)    %CFORMAT = 'NETCDF4'
    TPFILENESTPGD(:)%CFORMAT = 'NETCDF4'
  ELSE
    TPFILEPGD(:)    %CFORMAT = 'LFICDF4'
    TPFILENESTPGD(:)%CFORMAT = 'LFICDF4'
  END IF
ELSE IF (LLFIOUT) THEN
  TPFILEPGD(:)    %CFORMAT = 'LFI'
  TPFILENESTPGD(:)%CFORMAT = 'LFI'
ELSE
  CALL PRINT_MSG(NVERB_FATAL,'IO','OPEN_NESTPGD_FILES','unknown backup/output fileformat')
ENDIF
TPFILEPGD(:)    %CMODE    = 'READ'
TPFILENESTPGD(:)%CMODE    = 'WRITE'
TPFILEPGD(:)    %NLFITYPE = 2
TPFILENESTPGD(:)%NLFITYPE = 1
TPFILEPGD(:)    %NLFIVERB = NVERB
TPFILENESTPGD(:)%NLFIVERB = NVERB
!
!-------------------------------------------------------------------------------
CALL CLOSE_ll(HPRE_NEST_PGD)
!-------------------------------------------------------------------------------
!
!*       6.    OPENING OF INPUT AND OUTPUT PGD FILES
!              -------------------------------------
!
DO JPGD=1,NMODEL
  CALL IO_FILE_OPEN_ll(TPFILEPGD(JPGD),    CLUOUT0,IRESP,OPARALLELIO=.FALSE.)
  CALL IO_FILE_OPEN_ll(TPFILENESTPGD(JPGD),CLUOUT0,IRESP,OPARALLELIO=.FALSE.)
END DO
!
!-------------------------------------------------------------------------------
!
!*       7.    OPENING OF OUTPUT LISTING FILES FOR ALL MODELS
!              ----------------------------------------------
!
CALL INI_FIELD_LIST()
!
DO JPGD=1,NMODEL
  CALL GOTO_MODEL(JPGD)
  WRITE(YLUOUT,'("OUTPUT_LISTING",I0)') JPGD
  CALL OPEN_LUOUT_n(YLUOUT)
END DO
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_NESTPGD_FILES
