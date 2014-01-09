!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_7 BUG1 2007/06/15 17:47:27
!-----------------------------------------------------------------
!     ###################
      MODULE MODI_INI_SEG_n
!     ###################
!
INTERFACE
!
SUBROUTINE INI_SEG_n(KMI,HLUOUT,HINIFILE,HINIFILEPGD,PTSTEP_ALL)
INTEGER,              INTENT(IN)  :: KMI      ! Model index

CHARACTER(LEN=16), INTENT(OUT)     :: HLUOUT  ! name of the  listing-
                                              ! output of nested models
CHARACTER (LEN=28),   INTENT(OUT)  :: HINIFILE! name of
                                              ! the initial file
CHARACTER (LEN=28),   INTENT(OUT)  :: HINIFILEPGD
REAL,DIMENSION(:),  INTENT(INOUT) :: PTSTEP_ALL ! Time STEP of ALL models
END SUBROUTINE INI_SEG_n
!
END INTERFACE
!
END MODULE MODI_INI_SEG_n
!
!
!
!
!     #############################################################
      SUBROUTINE INI_SEG_n(KMI,HLUOUT,HINIFILE,HINIFILEPGD,PTSTEP_ALL)
!     #############################################################
!
!!****  *INI_SEG_n * - routine to read and update the descriptor files for 
!!                   model KMI
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to read the descriptor files in the
!     following order :
!         - DESFM file which gives informations about the initial file  
!     (i.e. the description of the segment that produced the initial file 
!     or the description of the preinitialisation that created the initial file)
!         - EXSEG file which gives informations about the segment to perform.
!
!      Informations in EXSEG file are completed by DESFM file informations
!      and if the informations are not in DESFM file, they are  set 
!      to default values. 
!
!        The descriptor file EXSEG corresponding to the segment of simulation 
!      to be performed, is then updated with the combined informations.
!      We also store in the updated EXSEG file, the informations on the status
!      of the different variables ( skip, init, read) in the namelist NAM_GETn,
!      which will be read in the INI_MODELn routine to properly initiliaze the
!      model n. Except this last namelist, the informations written in this
!      EXSEG file, will be identical to the NAMELIST section of the descriptive
!      part of the FM files containing the model outputs.
!
!        In order not to duplicate the routines called by ini_seg, we use the
!      modules modd, corresponding to the first model to store the informations
!      read on the different files ( DESFM and EXSEG ). The final filling of 
!      the modules modd (MODD_CONFn ....) will be realized in the subroutine 
!      INI_MODELn. The goal of the INI_SEG_n part of the initialization is to 
!      built the final EXSEG, which will be associated to the LFI files 
!      generated during the segment ( and therefore not to fill the modd). 
!       
!
!!**  METHOD
!!    ------
!!      For a nested model of index KMI :
!!         - Logical unit numbers  are associated to output-listing file and
!!      descriptor EXSEG  file by FMATTR. Then these files are   opened.
!!      The  name of the initial file is read in EXSEG file.     
!!         - Default values are supplied for variables in descriptor files
!!      (by DEFAULT_DESFM).
!!         - The Initial file (LFIFM + DESFM) is opened by FMOPEN.
!!         - The descriptor DESFM file is read (by READ_DESFM_n). 
!!         - The descriptor file EXSEG is read (by READ_EXSEG_n) and coherence
!!      between the initial file and the description of segment is also checked 
!!      in this routine.
!!         - If there is more than one model the EXSEG file is updated
!!     (by WRITE_DESFM$n).  This routine prints also EXSEG content on
!!     output-listing. 
!!         - If there is only one model (i.e. no grid-nesting),
!!      EXSEG  file is also closed (logical unit number associated with this
!!      file is also released by FMFREE).   
!!     
!!       
!!
!!    EXTERNAL
!!    --------
!!      FMATTR        : to associate a logical unit number to a file 
!!      FMOPEN        : to open descriptor file or LFI file
!!      DEFAULT_DESFM1: to set default values
!!      READ_DESFM_n    : to read a DESFM file 
!!      READ_EXSEG_n    : to read a EXSEG file
!!      WRITE_DESFM1   : to write the DESFM part of the future outputs
!!      FMFREE        : to release a logical unit number linked to a file
!!
!!      Module MODI_DEFAULT_DESFM    : Interface for routine DEFAULT_DESFM
!!      Module MODI_READ_DESFM_n    : Interface for routine  READ_DESFM_n
!!      Module MODI_READ_EXSEG_n    : Interface for routine  READ_EXSEG_n
!!      Module MODI_WRITE_DESFM1   : Interface for routine  WRITE_DESFM1
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_LUNIT  : contains names and logical unit numbers
!!
!!      Module MODD_CONF   : contains configuration variables
!!         CCONF      : Configuration of models
!!         NMODEL     : Number of nested models           
!!         NVERB      : Level of informations on output-listing
!!                          0 for minimum of prints
!!                          5 for intermediate level of prints
!!                         10 for maximum of prints 
!!
!!      Module MODN_LUNIT1 : contains declarations of namelist NAMLUNITMN
!!                           and module MODD_LUNIT1 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation (routine INI_SEG)
!!      
!!
!!    AUTHOR
!!    ------
!!	V. Ducrocq       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original         07/06/94 
!!      Modification     26/10/94  remove the NAM_GETn from the namelist present
!!                                 in the EXSEG file  (J.Stein)
!!                       11/01/95  change the read_exseg and desfm CALLS to add
!!                                 the G1D switch
!!                       15/02/95  add the HTURBLEN information (J. Cuxart)
!!                       18/08/95  Time STEP change          (J. P. Lafore)
!!                       02/10/95  add the radiation control (J. Stein)
!!                       18/03/96  remove the no write option for WRITE_DESFM
!!                                  (J. Stein)
!!                       11/04/96  add the ice conc. control (J.-P. Pinty)
!!                       11/01/97  add the deep convection control (J.-P. Pinty)
!!                       17/07/96  correction for WRITE_DESFM1 call (J. P. Lafore)              
!!                       22/07/96  PTSTEP_ALL introduction for nesting (J. P. Lafore)
!!                        7/08/98    //     (V. Ducrocq)
!!                       02/08/99  remove unused argument for read_desfm (J. Stein)
!!                       15/03/99  test on execution program (V. Masson)
!!                       15/11/00  Add YCLOUD (J.-P. Pinty)
!!                       01/03/01  Add GUSECHEM (D. Gazen)
!!                       15/10/01  namelists in different orders (I.Mallet)
!!                       25/11/02  Add YELEC (P. Jabouille)
!!                       01/2004   externalization of surface (V. Masson)
!!                       01/2005   add GDUST, GSALT, and GORILAM (P. Tulet)
!!                       04/2010   add GUSECHAQ, GCH_PH (M. Leriche)
!!                       09/2010   add GUSECHIC (M. Leriche)
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
USE MODD_LUNIT
USE MODD_CONF
USE MODN_CONFZ
USE MODD_CONF_n, ONLY : CSTORAGE_TYPE
USE MODD_LUNIT_n, ONLY : CINIFILE_n=> CINIFILE,CINIFILEPGD_n=> CINIFILEPGD
USE MODN_LUNIT_n
USE MODD_PARAMETERS
USE MODD_DYN
USE MODD_REF,   ONLY : LBOUSS
USE MODD_IO_ll, ONLY : GSMONOPROC
!
USE MODE_FMREAD
USE MODE_FM
USE MODE_IO_ll
USE MODE_POS
!
USE MODI_DEFAULT_DESFM_n
USE MODI_READ_DESFM_n
USE MODI_READ_EXSEG_n
USE MODI_WRITE_DESFM_n
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments 
!
INTEGER,              INTENT(IN)  :: KMI      ! Model index

CHARACTER(LEN=16), INTENT(OUT)     :: HLUOUT  ! name of the  listing-
                                              ! output of nested models
CHARACTER (LEN=28),   INTENT(OUT)  :: HINIFILE! name of
                                              ! the initial file
CHARACTER (LEN=28),   INTENT(OUT)  :: HINIFILEPGD                                              
REAL,DIMENSION(:),  INTENT(INOUT) :: PTSTEP_ALL ! Time STEP of ALL models
!
!*       0.1   declarations of local variables
!
LOGICAL            :: GFOUND              ! Return code when searching namelist
CHARACTER (LEN=32) :: YEXSEG,YDESFM               ! name of descriptor files
CHARACTER (LEN=28) :: YINIFILE                    ! name of initial file
CHARACTER (LEN=2)  :: YMI                         ! string for model index
INTEGER            :: IMASDEV                     ! version of MESOHN file  
INTEGER            :: ILUOUT                      ! Logical unit number
                                                  ! associated with CLUOUT 
                                                  !
INTEGER            :: IRESP,ILUSEG,ININAR         ! File management variables
INTEGER            :: IGRID,ILENCH                !             "
CHARACTER(LEN=100) :: YCOMMENT                    !             "
CHARACTER(LEN=2)   :: YDIR    ! Type  of the data field in LFIFM file
CHARACTER (LEN=16) :: YRECFM  ! Name of the desired field in LFIFM file
CHARACTER (LEN=5)  :: YCONF                       ! Local variables which have
LOGICAL            :: GFLAT                       ! the same definition as the
LOGICAL            :: GUSERV,GUSERC,GUSERR,GUSERI ! variables in module MODD_CONF,
LOGICAL            :: GUSERS,GUSERG,GUSERH,GUSECI ! MODD_CONFn, MODD_PARAMn,
LOGICAL            :: GUSECHEM                    ! flag for chemistry
LOGICAL            :: GUSECHAQ                    ! flag for aq. phase chemistry
LOGICAL            :: GUSECHIC                    ! flag for ice phase chemistry
LOGICAL            :: GCH_PH                      ! flag for pH 
LOGICAL            :: GCH_CONV_LINOX
LOGICAL            :: GDUST
LOGICAL,DIMENSION(JPMODELMAX) :: GDEPOS_DST, GDEPOS_SLT, GDEPOS_AER
LOGICAL            :: GSALT
LOGICAL            :: GORILAM
LOGICAL            :: GLG
LOGICAL            :: GPASPOL
LOGICAL            :: GCONDSAMP
                                                  ! These variables
                                                  ! are used to locally store 
INTEGER            :: ISV                         ! the value read in DESFM 
INTEGER            :: IRIMX,IRIMY                 ! number of points for the
                                                  ! horizontal relaxation
CHARACTER (LEN=4)  :: YTURB                       ! file in order to check the 
CHARACTER (LEN=4)  :: YRAD                        ! corresponding informations
CHARACTER (LEN=4)  :: YTOM                        ! read in EXSEG file.
LOGICAL            :: GRMC01
CHARACTER (LEN=4)  :: YDCONV
CHARACTER (LEN=4)  :: YSCONV
CHARACTER (LEN=4)  :: YCLOUD
CHARACTER (LEN=4)  :: YELEC
CHARACTER (LEN=3)  :: YEQNSYS
!
!-------------------------------------------------------------------------------
!
!*       1.    OPEN OUPUT-LISTING FILE AND EXSEG FILE
!              --------------------------------------
!
WRITE(YMI,'(I2.0)') KMI
HLUOUT='OUTPUT_LISTING'//ADJUSTL(YMI)

CALL OPEN_ll(UNIT=ILUOUT,FILE=HLUOUT,IOSTAT=IRESP,FORM='FORMATTED',ACTION='WRITE', &
     MODE=GLOBAL)
!
WRITE(UNIT=ILUOUT,FMT='(50("*"),/,"*",17X,"MODEL ",I1," LISTING",16X,"*",/,  &
            & 50("*"))') KMI
!
IF (CPROGRAM=='MESONH') THEN
  YEXSEG='EXSEG'//TRIM(ADJUSTL(YMI))//'.nam'

  CALL OPEN_ll(UNIT=ILUSEG,FILE=YEXSEG,IOSTAT=IRESP,ACTION='READ', &
       DELIM='QUOTE',MODE=GLOBAL)
!
!*       1.3   SPAWNING or SPEC or REAL program case
!              ---------------------
!
ELSE IF (CPROGRAM=='SPAWN ' .OR. CPROGRAM=='REAL  '.OR. CPROGRAM=='SPEC  ') THEN
  YINIFILE    = CINIFILE_n
  HINIFILE    = CINIFILE_n
  HINIFILEPGD = CINIFILEPGD_n
  YEXSEG=TRIM(ADJUSTL(CINIFILE_n))//'.des'
  CALL FMOPEN_ll(CINIFILE_n,'READ',HLUOUT,0,2,NVERB,ININAR,IRESP)
  CALL FMLOOK_ll(YEXSEG,CLUOUT0,ILUSEG,IRESP)
!
  IF (CPROGRAM=='SPAWN ') THEN
    IF (.NOT.GSMONOPROC) THEN
      WRITE(ILUOUT,FMT=*) 'SPAWNING : THIS PROGRAM HAS TO BE &
                      & PERFORMED WITH MONOPROCESSOR MODE'
      WRITE(ILUOUT,FMT=*) '-> JOB ABORTED'
!callabortstop
      CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
      CALL ABORT
      STOP
    ENDIF
  ENDIF
!
!*       1.3bis   DIAG program case
!
ELSE IF (CPROGRAM=='DIAG  ') THEN
  YINIFILE    = CINIFILE_n
  HINIFILE    = CINIFILE_n
  YEXSEG=TRIM(ADJUSTL(CINIFILE_n))//'.des'
  CALL FMOPEN_ll(CINIFILE_n,'READ',HLUOUT,0,2,NVERB,ININAR,IRESP)
  CALL FMLOOK_ll(YEXSEG,CLUOUT0,ILUSEG,IRESP)
  HINIFILEPGD=CINIFILEPGD_n
!   
!*       1.4   Other program cases
!              -------------------
!
ELSE
  WRITE(UNIT=ILUOUT,FMT=*) 'INI_SEG_n should not be called for CPROGRAM=',CPROGRAM
  WRITE(UNIT=ILUOUT,FMT=*) '-> STOP'
!callabortstop
  CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
  CALL ABORT
  STOP
ENDIF
!
!-------------------------------------------------------------------------------
!
!*      2.    SET DEFAULT VALUES
!             ------------------
!
CALL DEFAULT_DESFM_n(KMI)
!
!-------------------------------------------------------------------------------
!
!*       3.    READ INITIAL FILE NAME AND OPEN INITIAL FILE
!              --------------------------------------------
!
CALL POSNAM(ILUSEG,'NAM_LUNITN',GFOUND)
IF (GFOUND) THEN 
  CALL INIT_NAM_LUNITn
  READ(UNIT=ILUSEG,NML=NAM_LUNITn)
  CALL UPDATE_NAM_LUNITn
END IF

IF (CPROGRAM=='MESONH') THEN
   IF (KMI.EQ.1) THEN
      CALL POSNAM(ILUSEG,'NAM_CONFZ',GFOUND,ILUOUT)
      IF (GFOUND) READ(UNIT=ILUSEG,NML=NAM_CONFZ)
   END IF
  HINIFILEPGD=CINIFILEPGD_n
  HINIFILE=CINIFILE_n
  CALL FMOPEN_ll(HINIFILE,'READ',HLUOUT,0,2,NVERB,ININAR,IRESP)
END IF
!
!-------------------------------------------------------------------------------
!
!*      4.    READ in the LFI file SOME VARIABLES of MODD_CONF
!             ------------------------------------------------
!
! Read the storage type
  YRECFM = 'STORAGE_TYPE'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'--',CSTORAGE_TYPE,IGRID,ILENCH,YCOMMENT,IRESP)
  IF (IRESP /= 0) THEN
    WRITE(ILUOUT,FMT=9002) YRECFM,IRESP
!callabortstop
    CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
    CALL ABORT
    STOP
  END IF
IF (KMI == 1) THEN 
! Read the geometry kind 
  YRECFM = 'CARTESIAN'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,LCARTESIAN,IGRID,ILENCH,YCOMMENT,IRESP)
! Read the thinshell approximation
  YRECFM = 'THINSHELL'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,LTHINSHELL,IGRID,ILENCH,YCOMMENT,IRESP)
!
  CALL FMREAD(HINIFILE,'MASDEV',HLUOUT,'--',IMASDEV,IGRID,ILENCH,YCOMMENT,IRESP)
  IF (IMASDEV>=46) THEN
   YRECFM = 'L1D'
   YDIR='--'
   CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,L1D,IGRID,ILENCH,YCOMMENT,IRESP)
   IF (IRESP/=0)  L1D=.FALSE.
!
   YRECFM = 'L2D'
   YDIR='--'
   CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,L2D,IGRID,ILENCH,YCOMMENT,IRESP)
   IF (IRESP/=0)  L2D=.FALSE.
!
   YRECFM = 'PACK'
   YDIR='--'
   CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,LPACK,IGRID,ILENCH,YCOMMENT,IRESP)
   IF (IRESP/=0) LPACK=.TRUE.
  ELSE
   L1D=.FALSE.
   L2D=.FALSE.
   LPACK=.TRUE.
  END IF
  IF (IMASDEV>=410) THEN
   YRECFM = 'LBOUSS'
   YDIR='--'
   CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,LBOUSS,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
!
END IF
!
!-------------------------------------------------------------------------------
!
!*      5.    READ DESFM FILE
!             ---------------
!
YDESFM=TRIM(ADJUSTL(HINIFILE))//'.des'
!
CALL READ_DESFM_n(KMI,YDESFM,HLUOUT,YCONF,GFLAT,GUSERV,GUSERC,              &
                GUSERR,GUSERI,GUSECI,GUSERS,GUSERG,GUSERH,GUSECHEM,GUSECHAQ,&
                GUSECHIC,GCH_PH,GCH_CONV_LINOX,GSALT,GDEPOS_SLT,GDUST,      &
                GDEPOS_DST, GORILAM,  &
                GDEPOS_AER, GLG, GPASPOL, GCONDSAMP, IRIMX,IRIMY,ISV,       &
                YTURB,YTOM,GRMC01,YRAD,YDCONV,YSCONV,YCLOUD,YELEC,YEQNSYS   )
!
!
!-------------------------------------------------------------------------------
!
!*      6.    READ EXSEG FILE
!             ---------------
!   We pass by arguments the informations read in DESFM descriptor to the
! routine which read related informations in the EXSEG descriptor in order to 
! check coherence between both informations.
!
CALL READ_EXSEG_n(KMI,YEXSEG,HLUOUT,YCONF,GFLAT,GUSERV,GUSERC,              &
                GUSERR,GUSERI,GUSECI,GUSERS,GUSERG,GUSERH,GUSECHEM,         &
                GUSECHAQ,GUSECHIC,GCH_PH,                                   &
                GCH_CONV_LINOX,GSALT,GDEPOS_SLT,GDUST,GDEPOS_DST,           &
                GORILAM,GDEPOS_AER,GLG,GPASPOL, GCONDSAMP, IRIMX,IRIMY,ISV, &
                YTURB,YTOM,GRMC01,YRAD,YDCONV,YSCONV,YCLOUD,YELEC,YEQNSYS,  &
                PTSTEP_ALL,CSTORAGE_TYPE,CINIFILEPGD_n                      )
!
!
IF (CPROGRAM=='SPAWN ' .OR. CPROGRAM=='DIAG  ' .OR. CPROGRAM=='SPEC  '      &
     .OR. CPROGRAM=='REAL  ') THEN
  CINIFILE_n    = YINIFILE
  HINIFILE    = YINIFILE
  CCPLFILE(:) = '                            '
  NMODEL=1
  LSTEADYLS=.TRUE.
END IF
!
IF (CPROGRAM=='MESONH') THEN
  HINIFILEPGD=CINIFILEPGD_n
END IF
!-------------------------------------------------------------------------------
!
!*      7.    CLOSE  FILES
!             ------------
!
IF (CPROGRAM=='MESONH') CALL CLOSE_ll(YEXSEG,IOSTAT=IRESP)
!
!-------------------------------------------------------------------------------
9002  FORMAT(/,'FATAL ERROR IN INI_SEG_n: pb to read ',A16,' IRESP=',I3)
!
END SUBROUTINE INI_SEG_n
