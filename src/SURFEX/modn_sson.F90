!##################
MODULE MODN_SSO_n
!##################
!
!!****  *MODN_SSO_n* - declaration of namelist NAM_SSO_n
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify  the namelist NAM_SSO_n
!     which concern the roughness parameterization for orography.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!       
!!    AUTHOR
!!    ------
!!	V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2010                    
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_SURF_ATM_SSO_n, ONLY: &
         CROUGH_n => CROUGH,   &
         XFRACZ0_n => XFRACZ0, &
         XCOEFBE_n => XCOEFBE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=4)  :: CROUGH
REAL              :: XFRACZ0
REAL              :: XCOEFBE
!
NAMELIST/NAM_SSOn/CROUGH, XFRACZ0, XCOEFBE
!
CONTAINS
!
SUBROUTINE INIT_NAM_SSOn
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_SSO_N:INIT_NAM_SSON',0,ZHOOK_HANDLE)
  CROUGH = CROUGH_n
  XFRACZ0= XFRACZ0_n  
  XCOEFBE= XCOEFBE_n  
  IF (LHOOK) CALL DR_HOOK('MODN_SSO_N:INIT_NAM_SSON',1,ZHOOK_HANDLE)
END SUBROUTINE INIT_NAM_SSOn

SUBROUTINE UPDATE_NAM_SSOn
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_SSO_N:UPDATE_NAM_SSON',0,ZHOOK_HANDLE)
  CROUGH_n = CROUGH
  XFRACZ0_n= XFRACZ0  
  XCOEFBE_n= XCOEFBE  
  IF (LHOOK) CALL DR_HOOK('MODN_SSO_N:UPDATE_NAM_SSON',1,ZHOOK_HANDLE)
END SUBROUTINE UPDATE_NAM_SSOn

END MODULE MODN_SSO_n
