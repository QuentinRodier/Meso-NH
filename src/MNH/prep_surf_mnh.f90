!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 prep_real 2006/06/16 17:22:03
!-----------------------------------------------------------------
!     #############################
      MODULE MODI_PREP_SURF_MNH
!     #############################
INTERFACE
      SUBROUTINE PREP_SURF_MNH(HATMFILE,HATMFILETYPE)
!
CHARACTER(LEN=28), INTENT(IN)    :: HATMFILE    ! name of the Atmospheric file
CHARACTER(LEN=6),  INTENT(IN)    :: HATMFILETYPE! type of the Atmospheric file
!
END SUBROUTINE PREP_SURF_MNH
!
END INTERFACE
END MODULE MODI_PREP_SURF_MNH
!
!     #######################################################
      SUBROUTINE PREP_SURF_MNH(HATMFILE,HATMFILETYPE)
!     #######################################################
!
!!****  *PREP_SURF_MNH* - calls surface field preparation
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!     V. Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!------------------------------------------------------------------
!

!
USE MODD_LUNIT_n,     ONLY : CINIFILE
USE MODD_TIME_n,      ONLY : TDTCUR
USE MODD_LUNIT,       ONLY : CLUOUT0, COUTFMFILE, CPGDFILE
USE MODD_IO_SURF_MNH, ONLY : COUTFILE, CFILE
USE MODE_FM
!
USE MODI_INIT_PGD_SURF_ATM
USE MODI_PREP_SURF_ATM
USE MODI_WRITE_SURF_ATM_N
USE MODI_WRITE_DIAG_SURF_ATM_N
!
#ifdef MNH_NCWRIT
USE MODN_NCOUT
USE MODE_UTIL
#endif
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=28), INTENT(IN)   :: HATMFILE    ! name of the Atmospheric file
CHARACTER(LEN=6),  INTENT(IN)   :: HATMFILETYPE! type of the Atmospheric file
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
CHARACTER(LEN=28)  :: YPGDFILE  ='                            '  ! name of the pgd file
CHARACTER(LEN=6)   :: YPGDFILETYPE ='      '                     ! type of the pgd file
INTEGER  :: ILUOUT0  ! logical unit for listing file
INTEGER  :: IRESP    ! return code in FM routines
CHARACTER(LEN=6) :: YATMFILETYPE    ! type of the Atmospheric file
!------------------------------------------------------------------
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
WRITE(ILUOUT0,*) '***************************************************'
WRITE(ILUOUT0,*) '***************** EXTERNALIZED SURFACE ************'
WRITE(ILUOUT0,*) '***************************************************'
!
!
COUTFILE   = CINIFILE
COUTFMFILE = CINIFILE
!
YATMFILETYPE = HATMFILETYPE
IF(YATMFILETYPE=='GRIBEX') YATMFILETYPE='GRIB  '
IF (LEN_TRIM(HATMFILE)==0) YATMFILETYPE='      '
!
CALL INIT_PGD_SURF_ATM('MESONH','PRE',HATMFILE,YATMFILETYPE,  &
                       TDTCUR%TDATE%YEAR, TDTCUR%TDATE%MONTH, &
                       TDTCUR%TDATE%DAY, TDTCUR%TIME          )
CALL PREP_SURF_ATM('MESONH',HATMFILE,YATMFILETYPE,HATMFILE,YATMFILETYPE)
#ifdef MNH_NCWRIT
NC_WRITE=LNETCDF
NC_FILE='sf2'
CALL WRITE_SURF_ATM_n('MESONH','PRE',.FALSE.)
CALL WRITE_DIAG_SURF_ATM_n('MESONH','PRE')
IF ( LNETCDF ) THEN
  DEF_NC=.FALSE.
  CALL WRITE_SURF_ATM_n('MESONH','PRE',.FALSE.)
  CALL WRITE_DIAG_SURF_ATM_n('MESONH','PRE')
  DEF_NC=.TRUE.
  NC_WRITE = .FALSE.
END IF
#else
CALL WRITE_SURF_ATM_n('MESONH','PRE',.FALSE.)
CALL WRITE_DIAG_SURF_ATM_n('MESONH','PRE')
#endif

!
!----------------------------------------------------------
!
END SUBROUTINE PREP_SURF_MNH
