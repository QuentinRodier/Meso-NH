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
!!  06/2016     (G.Delautier) phasage surfex 8
!!    P.Wautelet : 08/07/2016 : removed MNH_NCWRIT define
!!  01/2018      (G.Delautier) SURFEX 8.1
!------------------------------------------------------------------
!
USE MODD_IO_SURF_MNH, ONLY : COUTFILE
USE MODD_LUNIT,       ONLY : TLUOUT0, TOUTDATAFILE
USE MODD_LUNIT_n,     ONLY : CINIFILE, TINIFILE
USE MODD_MNH_SURFEX_n
USE MODD_TIME_n,      ONLY : TDTCUR
!
USE MODE_PREP_CTL, ONLY : PREP_CTL
!
USE MODI_INIT_PGD_SURF_ATM
USE MODI_PREP_SURF_ATM
USE MODI_WRITE_DIAG_SURF_ATM_N
USE MODI_WRITE_SURF_ATM_N
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
TYPE (PREP_CTL) :: YLCTL
!
CHARACTER(LEN=28)  :: YPGDFILE  ='                            '  ! name of the pgd file
CHARACTER(LEN=6)   :: YPGDFILETYPE ='      '                     ! type of the pgd file
INTEGER            :: ILUOUT0  ! logical unit for listing file
CHARACTER(LEN=6)   :: YATMFILETYPE    ! type of the Atmospheric file
!------------------------------------------------------------------
!
ILUOUT0 = TLUOUT0%NLU
WRITE(ILUOUT0,*) '***************************************************'
WRITE(ILUOUT0,*) '***************** EXTERNALIZED SURFACE ************'
WRITE(ILUOUT0,*) '***************************************************'
!
!
COUTFILE   = CINIFILE
TOUTDATAFILE => TINIFILE
!
YATMFILETYPE = HATMFILETYPE
IF(YATMFILETYPE=='GRIBEX') YATMFILETYPE='GRIB  '
IF (LEN_TRIM(HATMFILE)==0) YATMFILETYPE='      '
!
CALL INIT_PGD_SURF_ATM(YSURF_CUR,'MESONH','PRE',HATMFILE,YATMFILETYPE,  &
                       TDTCUR%TDATE%YEAR, TDTCUR%TDATE%MONTH, &
                       TDTCUR%TDATE%DAY, TDTCUR%TIME          )
CALL PREP_SURF_ATM(YSURF_CUR,'MESONH',HATMFILE,YATMFILETYPE,HATMFILE,YATMFILETYPE,YLCTL)
CALL WRITE_SURF_ATM_n(YSURF_CUR,'MESONH','PRE',.FALSE.)
CALL WRITE_DIAG_SURF_ATM_n(YSURF_CUR,'MESONH','PRE')
!
!----------------------------------------------------------
!
END SUBROUTINE PREP_SURF_MNH
