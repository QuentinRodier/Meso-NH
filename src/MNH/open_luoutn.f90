!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 prep_nest_pgd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!######################
MODULE MODI_OPEN_LUOUTn
!######################
!

INTERFACE
      SUBROUTINE OPEN_LUOUT_n(HLUOUT)
!
CHARACTER(LEN=28), INTENT(IN) :: HLUOUT ! name of the output listing file
END SUBROUTINE OPEN_LUOUT_n
!
END INTERFACE
END MODULE MODI_OPEN_LUOUTn
!     ##############################
      SUBROUTINE OPEN_LUOUT_n(HLUOUT)
!     ##############################
!
!!****  *OPEN_LUOUT_n* - openning of listing file of model _n
!!                         
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    Routine FMATTR
!!    Routine FMOPEN
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_LUNIT     :  contains logical unit names for all models
!!         CLUOUT0  : name of output-listing
!!      Module MODD_LUNIT_n   :  contains logical unit names for all models
!!         CLUOUT   : name of output-listing for model _n
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
!!      Original     30/09/96
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_IO_ll
!
USE MODD_LUNIT
USE MODD_LUNIT_n
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
!
CHARACTER(LEN=28), INTENT(IN) :: HLUOUT ! name of the output listing file
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
INTEGER :: IRESP      ! return-code if problems eraised
INTEGER :: ILUOUT     ! logical unit for listing file for model _n
!-------------------------------------------------------------------------------
!
CLUOUT=HLUOUT
CALL OPEN_ll(UNIT=ILUOUT,FILE=CLUOUT,IOSTAT=IRESP,FORM='FORMATTED',ACTION='WRITE', &
     MODE=GLOBAL)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_LUOUT_n
