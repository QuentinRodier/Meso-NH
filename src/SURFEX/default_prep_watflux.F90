!     #########
      SUBROUTINE DEFAULT_PREP_WATFLUX
!     ###########################
!
!!****  *DEFAULT_PREP_WATFLUX* - routine to set default values for the configuration for WATFLUX fields preparation
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
!!	S. Malardel   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PREP_WATFLUX,   ONLY : CFILE_WATFLX, CTYPE, CFILEPGD_WATFLX, CTYPEPGD, XTS_WATER_UNIF

USE MODN_PREP_WATFLUX,   ONLY : LWAT_SBL

USE MODD_SURF_PAR,   ONLY : XUNDEF
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
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_WATFLUX',0,ZHOOK_HANDLE)
CFILE_WATFLX = '                          '
CTYPE        = 'GRIB  '
!
CFILEPGD_WATFLX = '                          '
CTYPEPGD        = '      '
!
XTS_WATER_UNIF = XUNDEF
!
LWAT_SBL = .FALSE.
IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_WATFLUX',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_PREP_WATFLUX
