!     ########################################
      SUBROUTINE GET_SSO_STDEV_n(HPROGRAM,KI,PSSO_STDEV)
!     ########################################
!
!!****  *GET_SSO_STDEV_n* - routine to get some surface fields
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
!!	S. Riette   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2010
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
!
USE MODD_SURF_ATM_SSO_n,     ONLY : XSSO_STDEV
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM
INTEGER,             INTENT(IN)  :: KI      ! horizontal dim. of cover
REAL, DIMENSION(KI), INTENT(OUT) :: PSSO_STDEV     ! S.S.O. standard deviation (m)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_SSO_STDEV_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF ( SIZE(PSSO_STDEV) /= SIZE(XSSO_STDEV) ) THEN
  WRITE(ILUOUT,*) 'try to get SSO_STDEV field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PSSO_STDEV) :', SIZE(PSSO_STDEV)
  WRITE(ILUOUT,*) 'size of field inthe surface                     (XSSO_STDEV) :', SIZE(XSSO_STDEV)
  CALL ABOR1_SFX('GET_SSO_STDEVN: SSO_STDEV SIZE NOT CORRECT')
ELSE
  PSSO_STDEV = XSSO_STDEV
END IF
IF (LHOOK) CALL DR_HOOK('GET_SSO_STDEV_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_SSO_STDEV_n
