!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! NEC0 masdev4_7 2007/06/16 01:41:59
!-----------------------------------------------------------------
!     ###################
      MODULE MODD_LUNIT_n
!     ###################
!
!!****  *MODD_LUNIT$n* - declaration of names and logical unit numbers of files 
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the names 
!     for the initial Meso-NH files  
!     and also the  generic names  for the output files for model n.    
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
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
!!      Original      05/05/94  
!!      Modification  20/10/94 (J.Stein) add the output files                    
!!      Modification  10/03/95 (I.Mallet)   add the coupling files names 
!!      Modification  25/09/95 (J.Stein) add the output diachronic file                    
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX, JPCPLFILEMAX 
IMPLICIT NONE

TYPE LUNIT_t
! 
  CHARACTER(LEN=28) :: CINIFILE      ! Name of the input FM-file
  CHARACTER(LEN=28) :: CINIFILEPGD   ! Name of the PGD associated to input FM-file  
  CHARACTER(LEN=24) :: COUTFILE      ! Generic name of the output FM-files
  CHARACTER(LEN=28) :: CFMDIAC       ! diachronic output FM-file 
!
  CHARACTER(LEN=16) :: CLUOUT        ! Name of output_listing file
!JUAN
  CHARACTER(LEN=28),DIMENSION(:),POINTER :: CCPLFILE =>NULL() ! Names of the 
                                                           ! coupling FM-files
!JUAN
!
END TYPE LUNIT_t

TYPE(LUNIT_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: LUNIT_MODEL
LOGICAL      , DIMENSION(JPMODELMAX),         SAVE :: LUNIT_FIRST_CALL = .TRUE.
CHARACTER(LEN=4),SAVE :: CMASK_SURFEX='PREP'
!
CHARACTER(LEN=28), POINTER :: CINIFILE=>NULL()
CHARACTER(LEN=28), POINTER :: CINIFILEPGD=>NULL()
CHARACTER(LEN=24), POINTER :: COUTFILE=>NULL()
CHARACTER(LEN=28), POINTER :: CFMDIAC=>NULL()
CHARACTER(LEN=16), POINTER :: CLUOUT=>NULL()
CHARACTER(LEN=28),DIMENSION(:), POINTER :: CCPLFILE=>NULL()

CONTAINS

SUBROUTINE LUNIT_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
!JUAN
IF (LUNIT_FIRST_CALL(KTO)) THEN
ALLOCATE (LUNIT_MODEL(KTO)%CCPLFILE(JPCPLFILEMAX))
LUNIT_FIRST_CALL(KTO) = .FALSE.
ENDIF
!JUAN

! Save current state for allocated arrays
!
! Current model is set to model KTO
CINIFILE=>LUNIT_MODEL(KTO)%CINIFILE
CINIFILEPGD=>LUNIT_MODEL(KTO)%CINIFILEPGD
COUTFILE=>LUNIT_MODEL(KTO)%COUTFILE
CFMDIAC=>LUNIT_MODEL(KTO)%CFMDIAC
CLUOUT=>LUNIT_MODEL(KTO)%CLUOUT
CCPLFILE=>LUNIT_MODEL(KTO)%CCPLFILE

END SUBROUTINE LUNIT_GOTO_MODEL

END MODULE MODD_LUNIT_n
