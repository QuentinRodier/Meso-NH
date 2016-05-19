!     #########
SUBROUTINE GET_CONF_TRIP_n(OFLOOD_TRIP, PTSTEP_COUPLING)
!#######################################################
!
!!**** *GET_CONF_TRIP_n* get the configuration of TRIP
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Decharme         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    05/2008
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_TRIP_n,           ONLY : LFLOODT, XTSTEP_COUPLING
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
LOGICAL,INTENT(OUT), OPTIONAL :: OFLOOD_TRIP
REAL,   INTENT(OUT), OPTIONAL :: PTSTEP_COUPLING
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_CONF_TRIP_N',0,ZHOOK_HANDLE)
IF(PRESENT(OFLOOD_TRIP)) OFLOOD_TRIP = LFLOODT
!
IF(PRESENT(PTSTEP_COUPLING))PTSTEP_COUPLING = XTSTEP_COUPLING
IF (LHOOK) CALL DR_HOOK('GET_CONF_TRIP_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_CONF_TRIP_n
