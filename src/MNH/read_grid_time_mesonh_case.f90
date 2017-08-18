!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 prep_real 2006/05/18 13:07:25
!-----------------------------------------------------------------
!#####################################
MODULE MODI_READ_GRID_TIME_MESONH_CASE
!#####################################
INTERFACE
      SUBROUTINE READ_GRID_TIME_MESONH_CASE(HFMFILE,KXOR_LS,KYOR_LS,HDAD_NAME)
!
CHARACTER(LEN=*),  INTENT(IN) :: HFMFILE  ! name of the input Mesonh file
INTEGER          , INTENT(OUT):: KXOR_LS  ! I and J shifts between PGD file
INTEGER          , INTENT(OUT):: KYOR_LS  !   and LS atmospheric file
CHARACTER(LEN=*),  INTENT(OUT):: HDAD_NAME! dad name of the FM file
!!
END SUBROUTINE READ_GRID_TIME_MESONH_CASE
END INTERFACE
END MODULE MODI_READ_GRID_TIME_MESONH_CASE
!     #######################################################################
      SUBROUTINE READ_GRID_TIME_MESONH_CASE(HFMFILE,KXOR_LS,KYOR_LS,HDAD_NAME)
!     #######################################################################
!
!!****  *READ_GRID_TIME_MESONH_CASE* - initializes general informations and checks the
!!                       compatibility between the Mesonh file  and physiographic
!!                       data file
!! 
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    function FMLOOK  :to retrieve a logical unit number associated with a file
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_CONF      : contains configuration variables for all models.
!!         NVERB : verbosity level for output-listing
!!         LTHINSHELL
!!      Module MODD_LUNIT     :  contains logical unit names for all models
!!         CLUOUT0 : name of output-listing
!!      Module MODD_GRID
!!         XBETA   : rotation of the domain
!!         XRPK    : parameter of projection
!!         XLAT0   : latitude reference for the projection
!!         XLON0   : longitude reference for the projection
!!      Module MODD_TIME1
!!         TDTCUR
!!         TDTMOD
!!      Module MODD_TIME : to define the time type
!!         TDTEXP
!!         TDTSEG
!!         
!!
!!    REFERENCE
!!    ---------
!!
!!      Book 2
!!
!!
!!      Quelques elements sur les routines de traitement des fichiers ALADIN
!!      (some elements about the routines for the ALADIN files)
!!      V. Ducrocq 
!!      23 december 1994
!!      Available at CNRM/GMME
!!
!!    AUTHOR
!!    ------
!!	
!!      V.Masson  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/02/96
!!                  08/10/96 (V. Masson) add LTHINSHELL
!!                  25/10/96 (V. Masson) add deallocations
!!                  21/09/99 (V. Masson) possibility to have atm. file included
!!                                       in the PGD file
!!                  01/2004  (V. Masson) removes surface (externalization)
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CONF        ! declaration modules
USE MODD_GRID
USE MODD_GRID_n
USE MODD_IO_ll,  ONLY: TFILEDATA
USE MODD_LUNIT
USE MODD_PARAMETERS
USE MODD_PREP_REAL
USE MODD_TIME
USE MODD_TIME_n
!
USE MODE_FM
USE MODE_FMREAD
USE MODE_IO_MANAGE_STRUCT, ONLY : IO_FILE_FIND_BYNAME
!
USE MODI_DEFAULT_SLEVE
USE MODI_READ_HGRID
USE MODI_RETRIEVE2_NEST_INFO_n
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
CHARACTER(LEN=*),  INTENT(IN) :: HFMFILE  ! name of the input Mesonh file
INTEGER          , INTENT(OUT):: KXOR_LS  ! I and J shifts between PGD file
INTEGER          , INTENT(OUT):: KYOR_LS  !   and LS atmospheric file
CHARACTER(LEN=*),  INTENT(OUT):: HDAD_NAME! dad name of the FM file
!
!*       0.2   Declaration of local variables
!              ------------------------------
REAL               :: ZEPS       ! a little number
INTEGER            :: IRESP      ! return-code if problem eraised in FMLOOK
INTEGER            :: ILUOUT0    ! logical number for listing file
INTEGER            :: IGRID      ! grid point indicator 
INTEGER            :: ILENCH     ! length of comment string 
INTEGER            :: IMASDEV    ! Masdev version

CHARACTER(LEN=100) :: YCOMMENT   ! comment string
CHARACTER(LEN=16)  :: YRECFM     ! Name of the article to be read
INTEGER            :: JL         ! loop controls
INTEGER            :: ILMAX_LS
!
!        0.2.1 local geographical variables in or deduced from the Mesonh file
!              ---------------------------------------------------------------
!
REAL         :: ZLAT0_LS   ! reference latitude
REAL         :: ZLON0_LS   ! reference longitude
REAL         :: ZRPK_LS    ! parameter for projection 
REAL         :: ZBETA_LS   ! angle of rotation of the domain
!
CHARACTER(LEN=28) :: YMY_NAME, YDAD_NAME
CHARACTER(LEN=2)  :: YTYPE
!
!        0.2.2 local variables linking PGD and atm.MESONH files
!              ------------------------------------------------
!
INTEGER :: IXSIZE        ! X-direction size of atm. file
INTEGER :: IYSIZE        ! Y-direction size of atm. file
INTEGER :: IDXRATIO      ! X-direction grid mesh ratios, MUST be 1
INTEGER :: IDYRATIO      ! Y-direction grid mesh ratios, MUST be 1
!
!
TYPE(TFILEDATA),POINTER :: TZFMFILE => NULL()
!-------------------------------------------------------------------------------
!
!*       1.    INITIALIZATIONS
!              ---------------
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
ZEPS=1.E-10
!
!*       1.1   Original FMfile name
!              --------------------
!
YRECFM = 'DAD_NAME'
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',HDAD_NAME,IGRID,ILENCH,YCOMMENT,IRESP)
!
!-------------------------------------------------------------------------------
!
!*       2.    READING OF GEOGRAPHICAL, GRID AND TIME DATA ON THE MESONH FILE
!              --------------------------------------------------------------
!
!*       2.1    Projection :
!               ----------
!
YRECFM='LON0'
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',ZLON0_LS,IGRID,ILENCH,YCOMMENT,IRESP)
ZLON0_LS =ZLON0_LS +NINT((XLON0    -ZLON0_LS )/360.)*360.
! 
YRECFM='RPK'
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',ZRPK_LS,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='LAT0'
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',ZLAT0_LS,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='BETA'
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',ZBETA_LS,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (     (ABS(ZLAT0_LS-XLAT0)>ZEPS*MAX(1.,ABS(XLAT0)))               &
   .OR.  (ABS(ZLON0_LS-XLON0)>ZEPS*MAX(1.,ABS(XLON0)))               &
   .OR.  (ABS(ABS(ZRPK_LS)-ABS(XRPK))>ZEPS*MAX(1.,ABS(XRPK)))        &
   .OR.  (ABS(ZBETA_LS-XBETA)>ZEPS*MAX(1.,ABS(XBETA)))               ) THEN
!
  WRITE(ILUOUT0,FMT=*) ' '
  WRITE(ILUOUT0,FMT=*) '***************************************************************'
  WRITE(ILUOUT0,FMT=*) 'Projection are different between MESONH input file and PGD file'
  WRITE(ILUOUT0,FMT=*) 'You must recompute a PGD file with PREP_PGD,'
  WRITE(ILUOUT0,FMT=*) 'using the input MESONH file to define its domain.'
  WRITE(ILUOUT0,FMT=*) '***************************************************************'
  WRITE(ILUOUT0,FMT=*) ' '
  WRITE(ILUOUT0,FMT=*) '        input file     physiographic data'
  WRITE(ILUOUT0,1) 'LAT0  ',ZLAT0_LS, ' ',XLAT0
  WRITE(ILUOUT0,1) 'LON0  ',ZLON0_LS, ' ',XLON0
  WRITE(ILUOUT0,1) 'RPK   ',ZRPK_LS,  ' ',XRPK
  WRITE(ILUOUT0,1) 'BETA  ',ZBETA_LS, ' ',XBETA
END IF
!
!*       2.2    Horizontal grid:
!               ---------------
!
!PW: TODO: temporary: look for file from its name
!     TPFMFILE should be passed in arguments
CALL IO_FILE_FIND_BYNAME(HFMFILE,TZFMFILE,IRESP)
CALL READ_HGRID(1,TZFMFILE,YMY_NAME,YDAD_NAME,YTYPE)
CALL RETRIEVE2_NEST_INFO_n(1,0,KXOR_LS,KYOR_LS,IXSIZE,IYSIZE,IDXRATIO,IDYRATIO)
!
IF (IDXRATIO/=1 .OR. IDYRATIO/=1) THEN
  WRITE(ILUOUT0,FMT=*) ' '
  WRITE(ILUOUT0,FMT=*) '***************************************************************'
  WRITE(ILUOUT0,FMT=*) 'Resolution is different between MESONH input file and PGD file'
  WRITE(ILUOUT0,FMT=*) 'You must either:'
  WRITE(ILUOUT0,FMT=*) ' * recompute a PGD file with PREP_PGD,'
  WRITE(ILUOUT0,FMT=*) '    using the input MESONH file to define its domain.'
  WRITE(ILUOUT0,FMT=*) ' * or interpolate the atm. MESONH file with SPAWNING.'
  WRITE(ILUOUT0,FMT=*) '***************************************************************'
  WRITE(ILUOUT0,FMT=*) ' '
END IF
!
!*       2.3    Vertical grid :
!               -------------
!
!
YRECFM='KMAX'
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',ILMAX_LS,IGRID,ILENCH,YCOMMENT,IRESP)
!
ILMAX_LS=ILMAX_LS+2*JPVEXT
!
YRECFM='ZHAT'
ALLOCATE(XZHAT_LS(ILMAX_LS))
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',XZHAT_LS,IGRID,ILENCH,YCOMMENT,IRESP)
!
CALL FMREAD(HFMFILE,'MASDEV',CLUOUT0,'--',IMASDEV,IGRID,ILENCH,YCOMMENT,IRESP)
CALL DEFAULT_SLEVE(LSLEVE_LS,XLEN1_LS,XLEN2_LS)
IF (IMASDEV<=46) THEN
  LSLEVE_LS = .FALSE.
ELSE
  CALL FMREAD(HFMFILE,'SLEVE',CLUOUT0,'--',LSLEVE_LS,IGRID,ILENCH,YCOMMENT,IRESP)
  IF (LSLEVE_LS) THEN
    CALL FMREAD(HFMFILE,'LEN1',CLUOUT0,'--',XLEN1_LS,IGRID,ILENCH,YCOMMENT,IRESP)
    CALL FMREAD(HFMFILE,'LEN2',CLUOUT0,'--',XLEN2_LS,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
END IF
!
YRECFM='THINSHELL'
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',LTHINSHELL,IGRID,ILENCH,YCOMMENT,IRESP)
!
!*       2.5    Time variables :
!               --------------
!
YRECFM='DTCUR' 
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',TDTCUR,IGRID,ILENCH,YCOMMENT,IRESP)
!
TDTMOD=TDTCUR
TDTSEG=TDTCUR
TDTEXP=TDTCUR
!
!-------------------------------------------------------------------------------
!
WRITE(ILUOUT0,*) 'input MESONH vertical levels: function zhat'
DO JL=1,SIZE(XZHAT_LS)
  WRITE(ILUOUT0,'(I2,1X,F12.5)') JL,XZHAT_LS(JL)
ENDDO
WRITE(ILUOUT0,*)
WRITE(ILUOUT0,*) 'Routine READ_GRID_TIME_MESONH_CASE completed'
!
!-------------------------------------------------------------------------------
!
1 FORMAT(A6,F16.9,A1,F16.9,A1,F16.9)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_GRID_TIME_MESONH_CASE
