!     #####################
      MODULE MODD_CH_ISBA
!     ######################
!
!!
!!    PURPOSE
!!    -------
!     
!   
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!
!!    AUTHOR
!!    ------
!!  P. Tulet   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!  16/07/03 (P. Tulet)  restructured for externalization
!------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
REAL, SAVE  :: XRCSANDSO2            ! SO2 sand surface resistance
REAL, SAVE  :: XRCSANDO3             ! O3  sand surface resistance
REAL, SAVE  :: XRCCLAYSO2            ! SO2 clay surface resistance
REAL, SAVE  :: XRCCLAYO3             ! O3  clay surface resistance
REAL, SAVE  :: XRCSNOWSO2            ! SO2 snow surface resistance
REAL, SAVE  :: XRCSNOWO3             ! O3  snow surface resistance
REAL, SAVE  :: XLANDREXT             ! land type for external leaf resistance
!
END MODULE MODD_CH_ISBA


