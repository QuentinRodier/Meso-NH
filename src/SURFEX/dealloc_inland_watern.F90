!     #########
SUBROUTINE DEALLOC_INLAND_WATER_n
!     ###############################################################################
!
!!****  *DEALLOC_INLAND_WATER_n * - Deallocate all arrays
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
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!
!
USE MODD_SURF_ATM_n, ONLY : CWATER
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_DEALLOC_FLAKE_n
!
USE MODI_DEALLOC_IDEAL_FLUX
!
USE MODI_DEALLOC_WATFLUX_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEALLOC_INLAND_WATER_N',0,ZHOOK_HANDLE)
IF (CWATER=='WATFLX') THEN
  CALL DEALLOC_WATFLUX_n
ELSE IF (CWATER=='FLAKE ') THEN
  CALL DEALLOC_FLAKE_n   
ELSE IF (CWATER=='FLUX  ') THEN
  CALL DEALLOC_IDEAL_FLUX
END IF
IF (LHOOK) CALL DR_HOOK('DEALLOC_INLAND_WATER_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_INLAND_WATER_n
