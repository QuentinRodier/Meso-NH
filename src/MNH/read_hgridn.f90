!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
!-----------------------------------------------------------------
!     #######################
      MODULE MODI_READ_HGRID_n
!     #######################
!
INTERFACE
      SUBROUTINE READ_HGRID_n(HFMFILE,HMY_NAME,HDAD_NAME,HSTORAGE_TYPE)
!
CHARACTER (LEN=*), INTENT(IN)  :: HFMFILE     ! name of the file n
CHARACTER(LEN=28), INTENT(OUT) :: HMY_NAME     ! True Name of FM-file
CHARACTER(LEN=28), INTENT(OUT) :: HDAD_NAME    ! Name of father
CHARACTER(LEN=2) , INTENT(OUT) :: HSTORAGE_TYPE
!
END SUBROUTINE READ_HGRID_n
!
END INTERFACE
END MODULE MODI_READ_HGRID_n
!
!     #################################################################
      SUBROUTINE READ_HGRID_n(HFMFILE,HMY_NAME,HDAD_NAME,HSTORAGE_TYPE)
!     #################################################################
!
!!****  *READ_HGRID_n* - to read grid information in FM file of model $n
!!
!!    PURPOSE
!!    -------
!
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
!!      Module MODD_GRID_n : contains domain definition
!!        XXHAT
!!        XYHAT
!!      Module MODD_DIM_n : contains domain size
!!        NIMAX
!!        NJMAX
!!      Module MODD_PARAMETERS :
!!        JPHEXT
!!      Module MODD_LUNIT :
!!        CLUOUT
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
!!   J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_PARAMETERS, ONLY : JPHEXT, JPVEXT, JPMODELMAX
USE MODD_GRID
USE MODD_GRID_n
USE MODD_DIM_n
USE MODD_LUNIT_n
USE MODD_CONF
!
USE MODE_FM
USE MODE_FMREAD
USE MODE_MODELN_HANDLER
USE MODE_IO_ll
!
USE MODE_GRIDPROJ
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
CHARACTER (LEN=*), INTENT(IN)  :: HFMFILE     ! name of the file n
CHARACTER(LEN=28), INTENT(OUT) :: HMY_NAME     ! True Name of FM-file
CHARACTER(LEN=28), INTENT(OUT) :: HDAD_NAME    ! Name of father
CHARACTER(LEN=2) , INTENT(OUT) :: HSTORAGE_TYPE
!
!*       0.2   declarations of local variables
!
INTEGER             :: ILUOUT
CHARACTER (LEN=16)  :: YRECFM
INTEGER             :: ILENCH, IGRID, IRESP
CHARACTER (LEN=100) :: YCOMMENT
REAL                :: ZLAT0,ZLON0,ZRPK,ZBETA
REAL                :: ZEPS = 1.E-10
INTEGER             :: IMASDEV
INTEGER             :: IMI
!
!-------------------------------------------------------------------------------
REAL :: ZLATOR, ZLONOR, ZXHATM, ZYHATM
!-------------------------------------------------------------------------------
!JUAN REALZ
INTEGER             :: IIU,IJU
INTEGER             :: NIMAX2,NJMAX2
!JUAN REALZ
INTEGER             :: IJPHEXT
!
CALL FMLOOK_ll(CLUOUT,CLUOUT,ILUOUT,IRESP)
!
!*       1.     General information :
!               -------------------
!
YRECFM='MY_NAME'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMREAD(HFMFILE,YRECFM,CLUOUT,'--',HMY_NAME,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='DAD_NAME'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMREAD(HFMFILE,YRECFM,CLUOUT,'--',HDAD_NAME,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='STORAGE_TYPE'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMREAD(HFMFILE,YRECFM,CLUOUT,'--',HSTORAGE_TYPE,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='MASDEV'
CALL FMREAD(HFMFILE,YRECFM,CLUOUT,'--',IMASDEV,IGRID,ILENCH,YCOMMENT,IRESP)
!
!*       2.     Grid information :
!               ----------------
!
IF(IMASDEV<=45 .AND. HSTORAGE_TYPE == 'PG') THEN
  LCARTESIAN=.FALSE.
ELSE
  CALL FMREAD(HFMFILE,'CARTESIAN',CLUOUT,'--',LCARTESIAN,IGRID,ILENCH,YCOMMENT,IRESP)
ENDIF
CALL FMREAD(HFMFILE,'LAT0',CLUOUT,'--',ZLAT0,IGRID,ILENCH,YCOMMENT,IRESP)
CALL FMREAD(HFMFILE,'LON0',CLUOUT,'--',ZLON0,IGRID,ILENCH,YCOMMENT,IRESP)
CALL FMREAD(HFMFILE,'BETA',CLUOUT,'--',ZBETA,IGRID,ILENCH,YCOMMENT,IRESP)
IF(IRESP/=0) ZBETA=0.
IF (.NOT.LCARTESIAN ) THEN
  CALL FMREAD(HFMFILE,'RPK',CLUOUT,'--',ZRPK,IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(HFMFILE,'LATORI',CLUOUT,'--',XLATORI,IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(HFMFILE,'LONORI',CLUOUT,'--',XLONORI,IGRID,ILENCH,YCOMMENT,IRESP)
ENDIF
!
IMI = GET_CURRENT_MODEL_INDEX()
IF (IMI == 1) THEN
  XLAT0=ZLAT0
  XLON0=ZLON0
  XBETA=ZBETA
  IF (.NOT.LCARTESIAN) XRPK=ZRPK
ELSE
  IF (     ABS(XLAT0-ZLAT0)> ZEPS .OR. ABS(XLON0-ZLON0)> ZEPS  &
                                  .OR. ABS(XBETA-ZBETA)> ZEPS  ) THEN
    WRITE(ILUOUT,*) 'projections are different in the two input files:'
    WRITE(ILUOUT,*) 'model ',IMI,' : XLAT0= ',ZLAT0,' XLON0= ',ZLON0, &
                                               ' XBETA= ',ZBETA
    WRITE(ILUOUT,*) 'model 1 : XLAT0= ',XLAT0,' XLON0= ',XLON0, &
                                              ' XBETA= ',XBETA
 !callabortstop
    CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
    CALL ABORT
    STOP
  END IF
  IF (.NOT.LCARTESIAN ) THEN
    IF ( ABS(XRPK-ZRPK)> ZEPS ) THEN
      WRITE(ILUOUT,*) 'projections are different in the two input files:'
      WRITE(ILUOUT,*) 'model ',IMI,' : XRPK= ',ZRPK
      WRITE(ILUOUT,*) 'model 1 : XRPK= ',XRPK
 !callabortstop
      CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
      CALL ABORT
      STOP
    END IF
  END IF
END IF
!
IF (CPROGRAM/='IDEAL ') THEN
  !* WARNING : the following initialization of dimensions is ONLY valid for 
  !            monoprocessor runs, or if :
  !            a) NIMAX_ll, NJMAX_ll, and corresponding NIMAX_ll, NJMAX_ll are
  !               correctly initialized in later routines (e.g. spawn_model2.f90)
  !            b) and arrays XXHAT, XYHAT, XZS, XZSMT are deallocated after this 
  !               routine (as in ini_size_spawn.f90)
  CALL FMREAD(HFMFILE,'IMAX',CLUOUT,'--',NIMAX,IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(HFMFILE,'JMAX',CLUOUT,'--',NJMAX,IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(HFMFILE,'JPHEXT',CLUOUT,'--',IJPHEXT,IGRID,ILENCH,YCOMMENT,IRESP)
  IF ( IJPHEXT .NE. JPHEXT ) THEN
     IF (CPROGRAM == 'REAL' ) THEN
        WRITE(ILUOUT,FMT=*) ' READ_HGRID_N : JPHEXT in PRE_REAL1.nam/NAM_REAL_CONF ( or default value )&
             JPHEXT=',JPHEXT
     ELSE
        WRITE(ILUOUT,FMT=*) ' READ_HGRID_N : JPHEXT in PRE_NEST_PGD1.nam/NAM_CONF_NEST ( or default value )&
             JPHEXT=',JPHEXT
     END IF

     WRITE(ILUOUT,FMT=*) ' different from PGD files=',HFMFILE ,' value JPHEXT=',IJPHEXT
     WRITE(ILUOUT,FMT=*) '-> JOB ABORTED'
     CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
     CALL ABORT  
     STOP   
  END IF
END IF
!
!*       2.1  Read the configuration (MODD_CONF)
!
IF (IMI == 1) THEN   
  CALL FMREAD(HFMFILE,'L1D',CLUOUT,'--',L1D,IGRID,ILENCH,YCOMMENT,IRESP)
  IF (IRESP/=0) THEN
    L1D=.FALSE.
    IF( (NIMAX == 1).AND.(NJMAX == 1) ) L1D=.TRUE.
  ENDIF
!
  CALL FMREAD(HFMFILE,'L2D',CLUOUT,'--',L2D,IGRID,ILENCH,YCOMMENT,IRESP)
  IF (IRESP/=0) THEN
    L2D=.FALSE.
    IF( (NIMAX /= 1).AND.(NJMAX == 1) ) L2D=.TRUE.
  ENDIF
!
  CALL FMREAD(HFMFILE,'PACK',CLUOUT,'--',LPACK,IGRID,ILENCH,YCOMMENT,IRESP)
  IF (IRESP/=0) LPACK=.TRUE.
!  CALL SET_FMPACK_ll(L1D,L2D,LPACK)
END IF
!
!*       2.2    Grid information :
!               ----------------
!JUAN REALZ
IF ( CPROGRAM .EQ. "REAL  " ) THEN
CALL GET_DIM_EXT_ll('B',IIU,IJU)
CALL GET_DIM_PHYS_ll('B',NIMAX,NJMAX)
IF (.NOT. (ASSOCIATED(XXHAT))) ALLOCATE(XXHAT(IIU))
IF (.NOT. (ASSOCIATED(XYHAT))) ALLOCATE(XYHAT(IJU))
ELSE
IF (.NOT. (ASSOCIATED(XXHAT))) ALLOCATE(XXHAT(NIMAX+2*JPHEXT))
IF (.NOT. (ASSOCIATED(XYHAT))) ALLOCATE(XYHAT(NJMAX+2*JPHEXT))
ENDIF
!JUAN REALZ

CALL FMREAD(HFMFILE,'XHAT',CLUOUT,'XX',XXHAT,IGRID,ILENCH,YCOMMENT,IRESP)
CALL FMREAD(HFMFILE,'YHAT',CLUOUT,'YY',XYHAT,IGRID,ILENCH,YCOMMENT,IRESP)
!JUAN REALZ
IF ( CPROGRAM .EQ. "REAL  " ) THEN
IF (.NOT. (ASSOCIATED(XZS))) ALLOCATE(XZS(IIU,IJU))
ELSE
IF (.NOT. (ASSOCIATED(XZS))) ALLOCATE(XZS(NIMAX+2*JPHEXT,NJMAX+2*JPHEXT))
ENDIF
!JUAN REALZ

CALL FMREAD(HFMFILE,'ZS',CLUOUT,'XY',XZS,IGRID,ILENCH,YCOMMENT,IRESP)

!JUAN REALZ
IF ( CPROGRAM .EQ. "REAL  " ) THEN
IF (.NOT. (ASSOCIATED(XZSMT))) ALLOCATE(XZSMT(IIU,IJU))
ELSE
IF (.NOT. (ASSOCIATED(XZSMT))) ALLOCATE(XZSMT(NIMAX+2*JPHEXT,NJMAX+2*JPHEXT))
ENDIF
!JUAN REALZ

IF (IMASDEV<=46) THEN
  XZSMT = XZS
ELSE
  CALL FMREAD(HFMFILE,'ZSMT',CLUOUT,'XY',XZSMT,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
!-------------------------------------------------------------------------------
IF (IMASDEV<=45) THEN
  CALL FMREAD(HFMFILE,'LATOR',CLUOUT,'--',XLATORI,IGRID,ILENCH,YCOMMENT,IRESP)
  CALL FMREAD(HFMFILE,'LONOR',CLUOUT,'--',XLONORI,IGRID,ILENCH,YCOMMENT,IRESP)
  ZXHATM = - 0.5 * (XXHAT(1)+XXHAT(2))
  ZYHATM = - 0.5 * (XYHAT(1)+XYHAT(2))
  CALL SM_LATLON(XLATORI,XLONORI,ZXHATM,ZYHATM,ZLATOR,ZLONOR)
  XLATORI = ZLATOR
  XLONORI = ZLONOR
END IF
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_HGRID_n
