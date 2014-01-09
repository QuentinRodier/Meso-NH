!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 init 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ######################
      MODULE MODI_READ_HGRID
!     ######################
INTERFACE
      SUBROUTINE READ_HGRID(KMI,HFMFILE,HMY_NAME,HDAD_NAME,HSTORAGE_TYPE)
!
INTEGER,           INTENT(IN)  :: KMI          ! model index
CHARACTER (LEN=*), INTENT(IN)  :: HFMFILE     ! name of the file n
CHARACTER(LEN=28), INTENT(OUT) :: HMY_NAME     ! True Name of FM-file
CHARACTER(LEN=28), INTENT(OUT) :: HDAD_NAME    ! Name of father
CHARACTER(LEN=2) , INTENT(OUT) :: HSTORAGE_TYPE
!
END SUBROUTINE READ_HGRID
END INTERFACE
END MODULE MODI_READ_HGRID
!
!     ####################################################################
      SUBROUTINE READ_HGRID(KMI,HFMFILE,HMY_NAME,HDAD_NAME,HSTORAGE_TYPE)
!     ####################################################################
!
!!****  *READ_HGRID* - to read grid information in FM file into PGD modules
!!                     (KMI==0) or for model KMI
!!
!!    PURPOSE
!!    -------
!!
!!    CAUTION : if used to fill the MODD_PGD... modules, the projection
!!              definition module MODD_GRID will be eraised without test:
!!              In this case (KMI==0), it is used to define the
!!              projection.
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      FMREAD   : to read data in LFIFM file
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_GRID : contains projection definition
!!        XLAT0
!!        XLON0
!!        XRPK
!!        XBETA
!!        XLATORI
!!        XLONORI
!!      Module MODD_GRID$n : contains domain definition
!!        XXHAT
!!        XYHAT
!!      Module MODD_DIM$n : contains domain size
!!        NIMAX
!!        NJMAX
!!      Module MODD_PARAMETERS :
!!        JPHEXT
!!      Module MODD_LUNIT :
!!        CLUOUT0
!!
!!    REFERENCE
!!    ---------
!!      Book2 of the documentation
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        26/09/96
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODE_MODELN_HANDLER
USE MODI_READ_HGRID_n
!
USE MODD_PGDDIM
USE MODD_PGDGRID
USE MODD_GRID
USE MODD_PARAMETERS
USE MODD_LUNIT
!
USE MODE_FMREAD
USE MODE_GRIDPROJ
USE MODE_IO_ll
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
INTEGER,           INTENT(IN)  :: KMI          ! model index
CHARACTER (LEN=*), INTENT(IN)  :: HFMFILE     ! name of the file n
CHARACTER(LEN=28), INTENT(OUT) :: HMY_NAME     ! True Name of FM-file
CHARACTER(LEN=28), INTENT(OUT) :: HDAD_NAME    ! Name of father
CHARACTER(LEN=2) , INTENT(OUT) :: HSTORAGE_TYPE
!
!
!*       0.2   declarations of local variables
!
CHARACTER(LEN=16)      :: YRECFM
INTEGER                :: IGRID,ILENCH,IRESP
CHARACTER(LEN=100)     :: YCOMMENT
INTEGER                :: IMASDEV
INTEGER                :: IMI
LOGICAL                :: G1D,G2D,GPACK
!-------------------------------------------------------------------------------
REAL :: ZLATOR, ZLONOR, ZXHATM, ZYHATM
!-------------------------------------------------------------------------------
!
!*       1.     TEST ON MODEL INDEX
!               -------------------
!
! KMI may be 0 !
IF (KMI<0 .OR. KMI>JPMODELMAX) THEN
   !callabortstop
  CALL CLOSE_ll(CLUOUT0,IOSTAT=IRESP)
  CALL ABORT
  STOP
ENDIF
IF (KMI/=0) THEN
  IMI = GET_CURRENT_MODEL_INDEX()
  CALL GOTO_MODEL(KMI)
  CALL READ_HGRID_n(HFMFILE,HMY_NAME,HDAD_NAME,HSTORAGE_TYPE)
  CALL GOTO_MODEL(IMI)
  RETURN
END IF
!
!*       2.     READING IN MODD_PGD...
!               ----------------------
!
!*       2.1    General information :
!               -------------------
!
YRECFM='MY_NAME'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',HMY_NAME,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='DAD_NAME'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',HDAD_NAME,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='STORAGE_TYPE'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',HSTORAGE_TYPE,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='MASDEV'
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',IMASDEV,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
!*       2.2    Grid information :
!               ----------------
!
CALL FMREAD(HFMFILE,'LAT0',CLUOUT0,'--',XLAT0,IGRID,ILENCH,YCOMMENT,IRESP)
CALL FMREAD(HFMFILE,'LON0',CLUOUT0,'--',XLON0,IGRID,ILENCH,YCOMMENT,IRESP)
CALL FMREAD(HFMFILE,'RPK',CLUOUT0,'--',XRPK,IGRID,ILENCH,YCOMMENT,IRESP)
CALL FMREAD(HFMFILE,'BETA',CLUOUT0,'--',XBETA,IGRID,ILENCH,YCOMMENT,IRESP)
CALL FMREAD(HFMFILE,'LATORI',CLUOUT0,'--',XPGDLATOR,IGRID,ILENCH,YCOMMENT,IRESP)
CALL FMREAD(HFMFILE,'LONORI',CLUOUT0,'--',XPGDLONOR,IGRID,ILENCH,YCOMMENT,IRESP)
CALL FMREAD(HFMFILE,'IMAX',CLUOUT0,'--',NPGDIMAX,IGRID,ILENCH,YCOMMENT,IRESP)
CALL FMREAD(HFMFILE,'JMAX',CLUOUT0,'--',NPGDJMAX,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (.NOT.(ALLOCATED(XPGDXHAT))) ALLOCATE(XPGDXHAT(NPGDIMAX+2*JPHEXT))
IF (.NOT.(ALLOCATED(XPGDYHAT))) ALLOCATE(XPGDYHAT(NPGDJMAX+2*JPHEXT))
CALL FMREAD(HFMFILE,'XHAT',CLUOUT0,'--',XPGDXHAT,IGRID,ILENCH,YCOMMENT,IRESP)
CALL FMREAD(HFMFILE,'YHAT',CLUOUT0,'--',XPGDYHAT,IGRID,ILENCH,YCOMMENT,IRESP)
!
!*       3.   Read the configuration (MODD_CONF)
!
CALL FMREAD(HFMFILE,'L1D',CLUOUT0,'--',G1D,IGRID,ILENCH,YCOMMENT,IRESP)
IF (IRESP/=0) THEN
  G1D=.FALSE.
  IF( (NPGDIMAX == 1).AND.(NPGDJMAX == 1) ) G1D=.TRUE.
ENDIF
!
CALL FMREAD(HFMFILE,'L2D',CLUOUT0,'--',G2D,IGRID,ILENCH,YCOMMENT,IRESP)
IF (IRESP/=0) THEN
  G2D=.FALSE.
  IF( (NPGDIMAX /= 1).AND.(NPGDJMAX == 1) ) G2D=.TRUE.
ENDIF
!
CALL FMREAD(HFMFILE,'PACK',CLUOUT0,'--',GPACK,IGRID,ILENCH,YCOMMENT,IRESP)
IF (IRESP/=0) GPACK=.TRUE.
!
CALL SET_FMPACK_ll(G1D,G2D,GPACK)
!-------------------------------------------------------------------------------
IF (IMASDEV<=45) THEN
  CALL FMREAD(HFMFILE,'LATOR',CLUOUT0,'--',XPGDLATOR,IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(HFMFILE,'LONOR',CLUOUT0,'--',XPGDLONOR,IGRID,ILENCH,YCOMMENT,IRESP)
  ZXHATM = - 0.5 * (XPGDXHAT(1)+XPGDXHAT(2))
  ZYHATM = - 0.5 * (XPGDYHAT(1)+XPGDYHAT(2))
  CALL SM_LATLON(XPGDLATOR,XPGDLONOR,ZXHATM,ZYHATM,ZLATOR,ZLONOR)
  XPGDLATOR = ZLATOR
  XPGDLONOR = ZLONOR
END IF
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_HGRID
