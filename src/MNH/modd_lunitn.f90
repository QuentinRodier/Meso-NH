!MNH_LIC Copyright 1994-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
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
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 14/02/2019: remove CLUOUT/CLUOUT0 and associated variables
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE MODD_IO,         ONLY: TFILEDATA, TPTR2FILE
USE MODD_PARAMETERS, ONLY: JPMODELMAX, NFILENAMELGTMAX

IMPLICIT NONE

INTEGER, PARAMETER :: NPCPLFILEMAX = 1000 ! Maximum allowed number of CouPLing FILEs

TYPE LUNIT_t
! 
  CHARACTER(LEN=NFILENAMELGTMAX) :: CINIFILE = 'INIFILE'    ! Name of the input FM-file
  TYPE(TFILEDATA),POINTER :: TINIFILE => NULL() ! input FM-file
  CHARACTER(LEN=NFILENAMELGTMAX) :: CINIFILEPGD = '' ! Name of the PGD associated to input FM-file
  TYPE(TFILEDATA),POINTER :: TINIFILEPGD => NULL() ! PGD associated to input FM-file
  TYPE(TFILEDATA),POINTER :: TDIAFILE => NULL() ! diachronic output file
  TYPE(TFILEDATA),POINTER :: TLUOUT => NULL() ! output_listing file
  CHARACTER(LEN=NFILENAMELGTMAX), DIMENSION(:), POINTER :: CCPLFILE =>NULL() ! Names of the coupling FM-files
  TYPE(TPTR2FILE),DIMENSION(:),POINTER :: TCPLFILE => NULL() ! Coupling files
!
END TYPE LUNIT_t

TYPE(LUNIT_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: LUNIT_MODEL
LOGICAL      , DIMENSION(JPMODELMAX),         SAVE :: LUNIT_FIRST_CALL = .TRUE.
CHARACTER(LEN=4),SAVE :: CMASK_SURFEX='PREP'
!
CHARACTER(LEN=NFILENAMELGTMAX), POINTER :: CINIFILE=>NULL()
TYPE(TFILEDATA),   POINTER :: TINIFILE => NULL()
CHARACTER(LEN=NFILENAMELGTMAX), POINTER :: CINIFILEPGD =>NULL()
TYPE(TFILEDATA),   POINTER :: TINIFILEPGD => NULL()
TYPE(TFILEDATA),   POINTER :: TDIAFILE=>NULL()
TYPE(TFILEDATA),   POINTER :: TLUOUT=>NULL()
CHARACTER(LEN=NFILENAMELGTMAX), DIMENSION(:), POINTER :: CCPLFILE=>NULL()
TYPE(TPTR2FILE),  DIMENSION(:), POINTER :: TCPLFILE=>NULL()

CONTAINS

SUBROUTINE LUNIT_GOTO_MODEL(KFROM, KTO)
!
USE MODD_IO, ONLY : TFILE_OUTPUTLISTING
!
INTEGER, INTENT(IN) :: KFROM, KTO
!
!JUAN
IF (LUNIT_FIRST_CALL(KTO)) THEN
ALLOCATE (LUNIT_MODEL(KTO)%CCPLFILE(NPCPLFILEMAX))
ALLOCATE (LUNIT_MODEL(KTO)%TCPLFILE(NPCPLFILEMAX))
LUNIT_FIRST_CALL(KTO) = .FALSE.
ENDIF
!JUAN
!
TFILE_OUTPUTLISTING => LUNIT_MODEL(KTO)%TLUOUT
!
! Save current state for allocated arrays
!
! Current model is set to model KTO
CINIFILE=>LUNIT_MODEL(KTO)%CINIFILE
TINIFILE=>LUNIT_MODEL(KTO)%TINIFILE
CINIFILEPGD=>LUNIT_MODEL(KTO)%CINIFILEPGD
TINIFILEPGD=>LUNIT_MODEL(KTO)%TINIFILEPGD
TDIAFILE=>LUNIT_MODEL(KTO)%TDIAFILE
TLUOUT=>LUNIT_MODEL(KTO)%TLUOUT
CCPLFILE=>LUNIT_MODEL(KTO)%CCPLFILE
TCPLFILE=>LUNIT_MODEL(KTO)%TCPLFILE

END SUBROUTINE LUNIT_GOTO_MODEL

END MODULE MODD_LUNIT_n
