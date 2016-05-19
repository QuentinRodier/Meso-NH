!     #########
SUBROUTINE PREP_VER_FLAKE
!     #################################################################################
!
!!****  *PREP_VER_FLAKE* - change in FLAKE var. due to altitude change
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     S. Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      09.2010, E. Kourzeneva: Make not possible to shift the lake profile
!!                              in vertical, just to shift the lake surface 
!!                              temperature and then to set the default lake profile
!!------------------------------------------------------------------
!

!
USE MODD_FLAKE_n,    ONLY : XZS, XTS  
!
USE MODD_PREP,       ONLY : XZS_LS, XT_CLIM_GRAD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(:), ALLOCATABLE :: ZTS_LS ! large-scale water temperature
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PREP_VER_FLAKE',0,ZHOOK_HANDLE)

!       1. Check if the shift is needed at all
IF((ABS(MAXVAL(XZS)) < 0.001).AND.(ABS(MINVAL(XZS))< 0.001)) &
        CALL DR_HOOK('PREP_VER_FLAKE',1,ZHOOK_HANDLE)
IF((ABS(MAXVAL(XZS)) < 0.001).AND.(ABS(MINVAL(XZS))< 0.001)) RETURN
!
!*      2.  Shift surface temperature of water
!
ALLOCATE(ZTS_LS(SIZE(XTS)))
!
ZTS_LS = XTS
!
XTS = ZTS_LS  + XT_CLIM_GRAD  * (XZS - XZS_LS)
!
DEALLOCATE(ZTS_LS)
!
IF (LHOOK) CALL DR_HOOK('PREP_VER_FLAKE',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_VER_FLAKE
