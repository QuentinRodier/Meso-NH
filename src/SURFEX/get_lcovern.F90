!     #########
      SUBROUTINE GET_LCOVER_n(HPROGRAM,KCOVER,OCOVER)
!     ########################################
!
!!****  *GET_LCOVER_n* - routine to get some surface fields
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_GET_LUOUT
!
USE MODD_SURF_ATM_n,     ONLY : LCOVER
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),           INTENT(IN)  :: HPROGRAM
INTEGER,                    INTENT(IN)  :: KCOVER  ! number of covers
LOGICAL, DIMENSION(KCOVER), INTENT(OUT) :: OCOVER  ! cover types
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_LCOVER_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF ( SIZE(OCOVER) /= SIZE(LCOVER) ) THEN
  WRITE(ILUOUT,*) 'try to get COVER field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (OCOVER) :', SIZE(OCOVER)
  WRITE(ILUOUT,*) 'size of field inthe surface                     (LCOVER) :', SIZE(LCOVER)
  CALL ABOR1_SFX('GET_LCOVERN: (1) LCOVER SIZE NOT CORRECT')
ELSE
  OCOVER = LCOVER
END IF
IF (LHOOK) CALL DR_HOOK('GET_LCOVER_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_LCOVER_n
