!     #########
      SUBROUTINE PGD_INLAND_WATER(HPROGRAM)
!     #############################################################
!
!!****  *PGD_INLAND_WATER* - routine to choose initialization of lake scheme
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
!!      Original    03/2004
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_ATM_n, ONLY : CWATER
!
USE MODI_PGD_WATFLUX
USE MODI_PGD_FLAKE
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
 CHARACTER(LEN=6),                INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
!*       2.     Selection of surface scheme
!               ---------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_INLAND_WATER',0,ZHOOK_HANDLE)
IF (CWATER=='NONE  ') THEN
  IF (LHOOK) CALL DR_HOOK('PGD_INLAND_WATER',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (CWATER=='FLUX  ') THEN
  IF (LHOOK) CALL DR_HOOK('PGD_INLAND_WATER',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (CWATER=='WATFLX') THEN
  CALL PGD_WATFLUX(HPROGRAM)
ELSE IF (CWATER=='FLAKE ') THEN
  CALL PGD_FLAKE(HPROGRAM)
END IF
IF (LHOOK) CALL DR_HOOK('PGD_INLAND_WATER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_INLAND_WATER
