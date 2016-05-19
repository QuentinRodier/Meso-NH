!     ################
      MODULE MODD_CANOPY_TURB
!     ################
!
!!****  *MODD_CANOPY_TURB - declaration of surface parameters for urban canopy
!!
!!    PURPOSE
!!    -------
!     Declaration of surface parameters
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       07/2006
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!---------------------------------------------------------------------------
REAL,SAVE :: XTKEMIN   ! mimimum value for the TKE
REAL,SAVE :: XCEP      ! Constant for wind pressure-correlations
REAL,SAVE :: XCED      ! constant into the dissipation term of the TKE eq.
REAL,SAVE :: XALPSBL   ! constant linking TKE and friction velocity in the SBL
REAL,SAVE :: XA0       ! Constant a0 for wind pressure-correlations
REAL,SAVE :: XCTP      ! Constant for temperature and vapor pressure-correlations
!---------------------------------------------------------------------------
REAL,SAVE :: XCMFS     ! constant for the momentum flux due to shear   
!
REAL,SAVE :: XCSHF        ! constant for the sensible heat flux 
!
REAL,SAVE :: XASBL     ! constant used to define mixing length in the SBL
!
!---------------------------------------------------------------------------
!
END MODULE MODD_CANOPY_TURB
