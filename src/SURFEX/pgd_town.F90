!     #########
      SUBROUTINE PGD_TOWN(HPROGRAM,OECOCLIMAP,OGARDEN)
!     #############################################################
!
!!****  *PGD_TOWN* - routine to choose initialization of urban scheme
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
USE MODD_SURF_ATM_n, ONLY : CTOWN
!
USE MODI_PGD_TEB
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
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM   ! program calling surf. schemes
LOGICAL,          INTENT(IN)  :: OECOCLIMAP ! T if parameters are computed with ecoclimap
!                                           ! F if all parameters must be specified
LOGICAL,          INTENT(IN)  :: OGARDEN    ! T if urban green areas
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
IF (LHOOK) CALL DR_HOOK('PGD_TOWN',0,ZHOOK_HANDLE)
IF (CTOWN=='NONE  ') THEN
  IF (LHOOK) CALL DR_HOOK('PGD_TOWN',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (CTOWN=='FLUX  ') THEN
  IF (LHOOK) CALL DR_HOOK('PGD_TOWN',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (CTOWN=='TEB   ') THEN
  CALL PGD_TEB(HPROGRAM,OECOCLIMAP,OGARDEN)
END IF
IF (LHOOK) CALL DR_HOOK('PGD_TOWN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_TOWN
