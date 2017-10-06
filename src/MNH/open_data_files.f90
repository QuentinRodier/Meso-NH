!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 prep_pgd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ###########################
      MODULE MODI_OPEN_DATA_FILES
!     ###########################
INTERFACE
      SUBROUTINE OPEN_DATA_FILES(HFILETYPE,HFILENAME,HLOCFILENAME,OLOCTRANSFER)
      
!
CHARACTER(LEN=6),  INTENT(IN)    :: HFILETYPE     ! Type of the data file
CHARACTER(LEN=28), INTENT(IN)    :: HFILENAME     ! Name of the field file.
CHARACTER(LEN=28), INTENT(INOUT) :: HLOCFILENAME  ! Name of the truncated field file.
LOGICAL,           INTENT(INOUT) :: OLOCTRANSFER  ! .T. : local file is saved
!
END SUBROUTINE OPEN_DATA_FILES
END INTERFACE
END MODULE MODI_OPEN_DATA_FILES
!
!
!     #########################################################################
      SUBROUTINE OPEN_DATA_FILES(HFILETYPE,HFILENAME,HLOCFILENAME,OLOCTRANSFER)
!     #########################################################################
!
!!**** *OPEN_DATA_FILES* open the data files
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/03/97
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODE_FM
USE MODE_IO_ll
!
USE MODD_LUNIT
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
CHARACTER(LEN=6),  INTENT(IN)    :: HFILETYPE     ! Type of the data file
CHARACTER(LEN=28), INTENT(IN)    :: HFILENAME     ! Name of the field file.
CHARACTER(LEN=28), INTENT(INOUT) :: HLOCFILENAME  ! Name of the truncated field file.
LOGICAL,           INTENT(INOUT) :: OLOCTRANSFER  ! .T. : local file is saved
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: IRESP, ILUOUT0
INTEGER :: ININAR
INTEGER :: IGLB, ILOC      ! logical units for global and local files
!-------------------------------------------------------------------------------
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
!
!*    1.      Open the global file
!             --------------------
!
IF (HFILETYPE=='MESONH') THEN
  CALL FMOPEN_ll(HFILENAME,'READ',0,2,5,ININAR,IRESP)
ELSE IF (HFILETYPE=='BINLLV') THEN
  CALL OPEN_ll(UNIT=IGLB,FILE=HFILENAME,IOSTAT=IRESP,STATUS="OLD",ACTION='READ', &
          FORM="UNFORMATTED", POSITION="REWIND",MODE=GLOBAL)
ELSE IF (HFILETYPE=='LATLON') THEN
  CALL OPEN_ll(UNIT=IGLB,FILE=HFILENAME,STATUS="OLD",IOSTAT=IRESP,FORM="FORMATTED", &
               POSITION="REWIND",ACTION='READ',MODE=GLOBAL)
ENDIF
!
!*    2.      Open the local file
!             -------------------
!
IF (.NOT. OLOCTRANSFER) RETURN
!
IF (HLOCFILENAME == HFILENAME) THEN
  HLOCFILENAME='lc'//HFILENAME
  OLOCTRANSFER=.FALSE.
  WRITE(ILUOUT0,*) 'local data file ',HFILENAME,' would have the same name as input file'
  WRITE(ILUOUT0,*) 'It will not be transfered'
END IF 
IF (HFILETYPE=='BINLLV') THEN
  CALL OPEN_ll(UNIT=ILOC,FILE=HLOCFILENAME,STATUS="NEW",IOSTAT=IRESP,FORM="UNFORMATTED",   &
               POSITION="REWIND",ACTION="WRITE", MODE=GLOBAL)
ELSE IF (HFILETYPE=='LATLON') THEN
  CALL OPEN_ll(UNIT=ILOC,FILE=HLOCFILENAME,STATUS="NEW",IOSTAT=IRESP,FORM="FORMATTED",   &
               POSITION="REWIND",ACTION="WRITE", MODE=GLOBAL)
ENDIF
!
IF (HFILETYPE=='MESONH') THEN
  OLOCTRANSFER=.FALSE.
  ! in the mesonh input file case, ylocfilename is used
  ! to store the name of the field to read in the file
END IF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_DATA_FILES
