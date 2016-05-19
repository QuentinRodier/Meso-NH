!     #########
      FUNCTION ALBEDO_TA96(PZENITH) RESULT(PDIR_ALB)
!     ##################################################################
!
!!****  *ALBEDO_TA96*  
!!
!!    PURPOSE
!!    -------
!       computes the direct albedo over open water
!
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!	V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    19/09/03
!       
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_WATER_PAR,  ONLY : XALBCOEF_TA96
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!              -------------------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PZENITH     ! zenithal angle (radian)
!
REAL, DIMENSION(SIZE(PZENITH))  :: PDIR_ALB    ! direct albedo on water
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ALBEDO_TA96',0,ZHOOK_HANDLE)
PDIR_ALB(:) = XALBCOEF_TA96/(1.1*(MAX(COS(PZENITH(:)),0.))**1.4+0.15)
IF (LHOOK) CALL DR_HOOK('ALBEDO_TA96',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END FUNCTION ALBEDO_TA96
