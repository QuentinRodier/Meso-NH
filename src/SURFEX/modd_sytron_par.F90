!     ######################
      MODULE MODD_SYTRON_PAR
!     ######################
!
!!****  *MODD_SNOW_PAR* - declaration of parameters related
!!                          to the Sytron model
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the 
!     parameters related to the snow resistribution scheme Sytron 
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    Durand et al. (2001)      
!!
!!    AUTHOR
!!    ------
!!	V. Vionnet   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       11/2014                    
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE

INTEGER, DIMENSION(:),ALLOCATABLE :: NTAB_SYT !  Array on index defining opposite points

REAL, PARAMETER, DIMENSION(1)      :: XSLOPE_SYT = (/40./)  ! List of slope where snow redistribution must be computed (in degree) 

REAL, PARAMETER                       :: XANGLE_LIM = 10 ! Limit angle for wind speed projection 
REAL, PARAMETER                       :: XV1        = 0.5 ! parameter to control wind speed
REAL, PARAMETER                       :: XV2        = 0.8 ! parameter to control wind speed
REAL, PARAMETER                       :: XV3        = 2.5 ! parameter to control wind speed
REAL, PARAMETER                       :: XV4        = 0.2 ! parameter to control wind speed
REAL, PARAMETER                       :: XV5        = 0.5 ! parameter to control wind speed
REAL, PARAMETER                       :: XRHMI      = 0.001 ! minimal snow depth(m)

! Parameters used for SYVTAUX

REAL, PARAMETER       :: XVMIN = 4.    ! Minimal wind speed (m/s)
REAL, PARAMETER       :: XVMAX = 10.   ! Maximal wind speed (m/s)
REAL, PARAMETER       :: XVMINS = 5.    ! Minimal wind speed for sublimation (m/s)
REAL, PARAMETER       :: XVMAXS = 20.   ! Maximal wind speed for sublimation (m/s)
REAL, PARAMETER       :: XPOSALT = 0.8 ! Contribution of saltation to the total
                                       ! erosion flux (-) 
REAL, PARAMETER       :: XCOSALT = 0.150 ! Time contant integrating the
                           ! different relevant length scales of saltation (s) 
REAL, PARAMETER       :: XCOSUSP = 1.34E-04
REAL, PARAMETER       :: XPUSUSP = 4.13 ! Exponent for wind speed in the law for
                                        ! suspension rate (-)
REAL, PARAMETER       :: XCOSUBL = 0.05
REAL, PARAMETER       :: XHREF = 80.
REAL, PARAMETER       :: XTREF = 263.15
REAL, PARAMETER       :: XCMIN = 0.1
REAL, PARAMETER       :: XCMAX = 10.

! Parameters for SYVTASS
REAL, PARAMETER      :: XRHOMAX = 350. ! Maximum snow density 
REAL, PARAMETER      :: XRHOMIN = 30.  ! Minimum snow density 
REAL, PARAMETER      :: XRHOTO  = 2.5 * 3600.  ! Snow compaction rate (s)

!
!------------------------------------------------------------------------------
!
END MODULE MODD_SYTRON_PAR












