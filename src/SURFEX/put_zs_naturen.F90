!     ###########################################
      SUBROUTINE PUT_ZS_NATURE_n(HPROGRAM,KI,PZS)
!     ###########################################
!
!!****  *PUT_ZS_SURF_ATM_n* - routine to modify nature oropgraphy using atmospheric
!                    model orography
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
!!	P. Le Moigne   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2007
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_GET_LUOUT
!
USE MODD_ISBA_n,     ONLY : XZS
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
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM
INTEGER,             INTENT(IN)  :: KI      ! horizontal dim. of cover
REAL, DIMENSION(KI), INTENT(IN)  :: PZS     ! orography
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PUT_ZS_NATURE_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF ( SIZE(PZS) /= SIZE(XZS) ) THEN
  WRITE(ILUOUT,*) 'try to get ZS field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PZS) :', SIZE(PZS)
  WRITE(ILUOUT,*) 'size of field over nature                       (XZS) :', SIZE(XZS)
  CALL ABOR1_SFX('PUT_ZS_NATUREN: GET ZS FROM ATMOSPHERIC MODEL: SIZE NOT CORRECT')
ELSE
  XZS = PZS
END IF
IF (LHOOK) CALL DR_HOOK('PUT_ZS_NATURE_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE PUT_ZS_NATURE_n
