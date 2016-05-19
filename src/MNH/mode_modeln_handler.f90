!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! NEC0 masdev4_7 2007/06/16 01:41:59
!-----------------------------------------------------------------
MODULE MODE_MODELN_HANDLER
IMPLICIT NONE 

INTEGER, SAVE, PRIVATE     :: ICURRENT_MODEL = -1

CONTAINS 

FUNCTION GET_CURRENT_MODEL_INDEX()
INTEGER :: GET_CURRENT_MODEL_INDEX
!!
GET_CURRENT_MODEL_INDEX = ICURRENT_MODEL
!!
END FUNCTION GET_CURRENT_MODEL_INDEX

SUBROUTINE GOTO_MODEL(KMI)
!JUAN
USE MODI_GOTO_MODEL_WRAPPER
!JUAN
INTEGER, INTENT(IN) :: KMI
!!
IF (ICURRENT_MODEL == -1) THEN
  ICURRENT_MODEL = 1 ! Default model index
  CALL GOTO_MODEL_WRAPPER(ICURRENT_MODEL, KMI)
  ICURRENT_MODEL = KMI
ELSE
  IF (ICURRENT_MODEL /= KMI) THEN
!   Switch to model KMI, only if necessary
    CALL GOTO_MODEL_WRAPPER(ICURRENT_MODEL, KMI)
    ICURRENT_MODEL = KMI 
  END IF
END IF
!!
END SUBROUTINE GOTO_MODEL

END MODULE MODE_MODELN_HANDLER
