!     #########
      SUBROUTINE INIT_TRIP_PAR 
!     ########################
!
!
!!****  *INIT_TRIP_PAR* - Initialization of TRIP parameters
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to specify  the 
!     parameters related to the TRIP RRM. 
!
!!
!!      
!!
!!    AUTHOR
!!    ------
!!	B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original  22/05/08
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TRIP_PAR
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('INIT_TRIP_PAR',0,ZHOOK_HANDLE)
XTRIP_UNDEF = 1.0E20
NTRIPTAB    = 1000
!
XM_EXP      = 2.0/3.0
!
XRHOLW_T    = 1000.0
XDAY_T      = 86400.0
XSEA_T      = 135.3E12
XYEAR_T     = 365.0
!
XRAD_T      = 6371229.
XPI_T       = 2.*ASIN(1.)
IF (LHOOK) CALL DR_HOOK('INIT_TRIP_PAR',1,ZHOOK_HANDLE)
!
!--------------------------------------------------------------------------------
!
END SUBROUTINE INIT_TRIP_PAR
