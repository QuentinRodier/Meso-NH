!######################
MODULE MODD_TRIP_PAR
!######################
!
!!****  *MODD_TRIP_PAR* - declaration of TRIP parameters
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the 
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
IMPLICIT NONE
!
INTEGER, SAVE :: NTRIPTAB
!
REAL, SAVE    :: XM_EXP
!
REAL, SAVE    :: XTRIP_UNDEF
REAL, SAVE    :: XRHOLW_T
REAL, SAVE    :: XDAY_T 
REAL, SAVE    :: XSEA_T 
REAL, SAVE    :: XYEAR_T 
!
REAL, SAVE    :: XRAD_T
REAL, SAVE    :: XPI_T 
!
!--------------------------------------------------------------------------------
!
END MODULE MODD_TRIP_PAR












