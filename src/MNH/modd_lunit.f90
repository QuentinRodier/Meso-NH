!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     #################
      MODULE MODD_LUNIT
!     #################
!
!!****  *MODD_LUNIT* - declaration of names and logical unit numbers of files 
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the 
!     logical unit numbers  of  output file for all models.    
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (MODD_LUNIT)
!!          
!!    AUTHOR
!!    ------
!!	V. Ducrocq   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/05/94                      
!!      V. Masson   01/2004 add file names for use in externalized surface!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE 
CHARACTER(LEN=16),SAVE :: CLUOUT0    ! Name of output_listing file
CHARACTER(LEN=28),SAVE :: COUTFMFILE ! name of the output FM-file being written
CHARACTER(LEN=28),SAVE :: CPGDFILE   ! name of the PGD file for PREP_REAL_CASE
!
END MODULE MODD_LUNIT
