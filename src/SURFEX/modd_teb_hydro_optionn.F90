!##################
MODULE MODD_TEB_HYDRO_OPTION_n
!##################
!
!!****  *MODD_TEB_HYDRO_OPTION - declaration of ISBA scheme packed surface parameters for urban hydrology
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	A. Lemonsu *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       02/2013
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TYPE_SNOW
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE TEB_HYDRO_OPTIONS_t
!-------------------------------------------------------------------------------
!
! type of initialization of vegetation: from cover types (ecoclimap) or parameters prescribed
!
  INTEGER                         :: NLAYER_HYDRO     ! number of ground layers
  REAL, POINTER, DIMENSION(:)     :: XSOILGRID_HYDRO  ! Soil layer grid as reference for DIF
!-------------------------------------------------------------------------------
!
END TYPE TEB_HYDRO_OPTIONS_t

CONTAINS

SUBROUTINE TEB_HYDRO_OPTIONS_INIT(YTEB_HYDRO_OPTIONS)
TYPE(TEB_HYDRO_OPTIONS_t), INTENT(INOUT) :: YTEB_HYDRO_OPTIONS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_HYDRO_N:TEB_HYDRO_OPTIONS_INIT",0,ZHOOK_HANDLE)
YTEB_HYDRO_OPTIONS%NLAYER_HYDRO=0
NULLIFY(YTEB_HYDRO_OPTIONS%XSOILGRID_HYDRO)
IF (LHOOK) CALL DR_HOOK("MODD_TEB_HYDRO_N:TEB_HYDRO_OPTIONS_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_HYDRO_OPTIONS_INIT


END MODULE MODD_TEB_HYDRO_OPTION_n
