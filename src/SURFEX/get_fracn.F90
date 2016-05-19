!     #########
      SUBROUTINE GET_FRAC_n(HPROGRAM,KI,PSEA,PWATER,PNATURE,PTOWN)
!     ########################################
!
!!****  *GET_FRAC_n* - routine to get some surface fields
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
USE MODD_SURF_ATM_n,     ONLY : XSEA, XWATER, XNATURE, XTOWN
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
 CHARACTER(LEN=6),     INTENT(IN)            :: HPROGRAM
INTEGER,              INTENT(IN)            :: KI      ! number of points
REAL, DIMENSION(KI),  INTENT(OUT)           :: PSEA    ! sea    fraction
REAL, DIMENSION(KI),  INTENT(OUT)           :: PWATER  ! water  fraction
REAL, DIMENSION(KI),  INTENT(OUT)           :: PNATURE ! nature fraction
REAL, DIMENSION(KI),  INTENT(OUT)           :: PTOWN   ! town   fraction
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_FRAC_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF ( SIZE(PSEA) /= SIZE(XSEA) ) THEN
  WRITE(ILUOUT,*) 'try to get SEA field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PSEA) :', SIZE(PSEA)
  WRITE(ILUOUT,*) 'size of field inthe surface                     (XSEA) :', SIZE(XSEA)
  CALL ABOR1_SFX('GET_FRACN: SEA SIZE NOT CORRECT')
ELSE
  PSEA    = XSEA
  PNATURE = XNATURE
  PWATER  = XWATER
  PTOWN   = XTOWN
END IF
IF (LHOOK) CALL DR_HOOK('GET_FRAC_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_FRAC_n
