!MNH_LIC Copyright 1997-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #####################
      MODULE MODI_DUMMY_GR_INDEX
!     #####################
INTERFACE
      FUNCTION DUMMY_GR_INDEX(HFIELD,HDUMMY_GR_NAME) RESULT(KINDEX)
!
CHARACTER(LEN=*),               INTENT(IN) :: HFIELD         ! name of PGD field
CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HDUMMY_GR_NAME ! names of PGD field
INTEGER                                    :: KINDEX         ! index of this field
!
END FUNCTION DUMMY_GR_INDEX
END INTERFACE
END MODULE MODI_DUMMY_GR_INDEX
!
!     #########################################
      FUNCTION DUMMY_GR_INDEX(HFIELD,HDUMMY_GR_NAME) RESULT(KINDEX)
!     #########################################
!
!!
!!    PURPOSE
!!    -------
!!
!!     routine to retrive the index of a PGD field in LNOCLASS_PGD array
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
!!    Original    15/12/97
!  P. Wautelet 10/04/2019: replace ABORT and STOP calls by Print_msg
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
use mode_msg
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
CHARACTER(LEN=*),               INTENT(IN) :: HFIELD         ! name of PGD field
CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HDUMMY_GR_NAME ! names of PGD field
INTEGER                                    :: KINDEX         ! index of this field
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: JDUMMY ! loop control
!-------------------------------------------------------------------------------
!
DO JDUMMY=1,1000
  IF (HFIELD==HDUMMY_GR_NAME(JDUMMY)) THEN
    KINDEX = JDUMMY
    RETURN
  END IF
  IF (LEN_TRIM(HFIELD)==0) THEN
    call Print_msg(NVERB_FATAL,'GEN','DUMMY_GR_INDEX','LEN_TRIM(HFIELD)=0')
  ENDIF
END DO
!-------------------------------------------------------------------------------
!
END FUNCTION DUMMY_GR_INDEX
