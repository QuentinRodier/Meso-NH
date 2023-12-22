!MNH_LIC Copyright 1994-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ###################
      MODULE MODN_LUNIT_n
!     ###################
!
!!****  *MODN_LUNIT$n* - declaration of namelist NAM_LUNITn
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to specify  the namelist NAM_LUNITn 
!     which contains names for Meso-NH files (FM-file)   
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_LUNIT$n : contains declaration of logical unit names
!!            CINIFILE   : name of the FM-file used to initialize the model $n
!!            CCPLFILE   : Names of the FM-files used to couple the model 1
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (MODD_LUNITn)
!!          
!!    AUTHOR
!!    ------
!!	V. Ducrocq   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       1/06/94                      
!!      Modification  10/03/95 (I.Mallet)   add the coupling files names 
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPCPLFILEMAX, NFILENAMELGTMAX
USE MODD_LUNIT_n,    ONLY: CINIFILE_n    => CINIFILE,    &
                           CINIFILEPGD_n => CINIFILEPGD, &
                           CCPLFILE_n    => CCPLFILE
!
IMPLICIT NONE
!
SAVE
!
CHARACTER(LEN=NFILENAMELGTMAX)                          :: CINIFILE
CHARACTER(LEN=NFILENAMELGTMAX)                          :: CINIFILEPGD
CHARACTER(LEN=NFILENAMELGTMAX), DIMENSION(JPCPLFILEMAX) :: CCPLFILE
!
NAMELIST/NAM_LUNITn/CINIFILE,CINIFILEPGD,CCPLFILE
!
CONTAINS
!
SUBROUTINE INIT_NAM_LUNITn
  CINIFILE = CINIFILE_n
  CINIFILEPGD = CINIFILEPGD_n  
  CCPLFILE = CCPLFILE_n
END SUBROUTINE INIT_NAM_LUNITn

SUBROUTINE UPDATE_NAM_LUNITn
  CINIFILE_n = CINIFILE
  CINIFILEPGD_n = CINIFILEPGD  
  CCPLFILE_n = CCPLFILE
END SUBROUTINE UPDATE_NAM_LUNITn

END MODULE MODN_LUNIT_n
