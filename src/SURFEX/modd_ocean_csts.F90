!     ###############
      MODULE MODD_OCEAN_CSTS      
!     ###############
!
!!****  *MODD_OCEAN_CSTS* - declaration of Physic constants for ocean
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the 
!     Physics constants for ocean.    
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
!!      C. Lebeaupin Brossier   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2008
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE 
!
INTEGER, SAVE :: NOCKMIN  !first ocean level indice
INTEGER, SAVE :: NOCKMAX  ! last ocean level indice
!
REAL,SAVE :: XOCEAN_TSTEP       ! time step of the oceanic 1D model
REAL,SAVE :: XRHOSW,XRHOSWREF   ! densities of seawater
REAL,SAVE :: XCPSW              ! Cp (for seawater)
!
REAL,SAVE :: XGAMA              ! fraction of Coriolis parameter in time 
!
REAL,SAVE :: XZCE,XCKL          ! turbulence constants
!
REAL,SAVE :: XR                 ! from Jerlov's (1976)
REAL,SAVE :: XD1,XD2            ! optical water C type I
REAL,SAVE :: XALBEDOSW          ! Albedo of seawater
!
!
END MODULE MODD_OCEAN_CSTS
