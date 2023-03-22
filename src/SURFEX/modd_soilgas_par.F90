!     ####################
      MODULE MODD_SOILGAS_PAR
!     ####################
!
!!*****MODD_SOILGAS_PAR*  
!!
!!    PURPOSE
!!    -------
!!    Supply constants and parameter values for Morel et al. (2019) soil gas scheme
!!     
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!	B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    16/02/2021 
!!     
!!
!-------------------------------------------------------------------------------
!
IMPLICIT NONE
!
! molar gas constant (J.K−1.mol−1)
!
REAL, PARAMETER                :: XRGAS = 8.31446
!
! Bunsen coefficient
!
REAL, PARAMETER                :: XBUNSEN_O2 = 0.0296
REAL, PARAMETER                :: XBUNSEN_CO2 = 0.749
REAL, PARAMETER                :: XBUNSEN_CH4 = 0.0318
!
! Coefficients for gas diffusivity in air
!
REAL, PARAMETER                :: XA_O2_A = 0.1759E-4
REAL, PARAMETER                :: XB_O2_A = 1.17E-7
!
REAL, PARAMETER                :: XA_CO2_A = 0.1325E-4
REAL, PARAMETER                :: XB_CO2_A = 0.9E-7
!
REAL, PARAMETER                :: XA_CH4_A = 0.1875E-4
REAL, PARAMETER                :: XB_CH4_A = 1.30E-7
!
! Coefficients for gas diffusivity in water
!
REAL, PARAMETER                :: XA_O2_W = 1.172E-9
REAL, PARAMETER                :: XB_O2_W = 3.443E-11
REAL, PARAMETER                :: XC_O2_W = 5.048E-13
!
REAL, PARAMETER                :: XA_CO2_W = 0.939E-9
REAL, PARAMETER                :: XB_CO2_W = 2.671E-11
REAL, PARAMETER                :: XC_CO2_W = 4.095E-13
!
REAL, PARAMETER                :: XA_CH4_W = 0.9798E-9
REAL, PARAMETER                :: XB_CH4_W = 2.986E-11
REAL, PARAMETER                :: XC_CH4_W = 4.381E-13
!
! Ratio between Methanogenesis and Oxic decomposition under anoxic condition (DK 2008)
!
REAL, PARAMETER                :: XRATIO_MG = 10. ! Eq (6) DK (2008)
!
! Constants for methanotrophy (s)
!
REAL, PARAMETER                :: XTAU_CH4_MT = 86400.       ! Time constant
REAL, PARAMETER                :: XCT1_MT     = 1.4350845253 ! ln(4.2)
REAL, PARAMETER                :: XCT2_MT     = 18.7
REAL, PARAMETER                :: XCT3_MT     = 10.
REAL, PARAMETER                :: XKO2        = 2.0          ! Michhaelis-Menten kinematic
!
! O2 concentration in air near the surface (%)
!
REAL, PARAMETER                :: XO2_ATMO  = 20.946
!
! CH4 saturated mixing ratio for ebullition
!
REAL, PARAMETER                :: XRCH4 = 0.15 
!
! Bubble velocity for ebullition (m/s)
!
REAL, PARAMETER                :: XBUBBLEV = 1.0E-3
!
! root (aerenchyma) porosity
!
REAL, PARAMETER                :: XROOTPOROSITY  = 0.3
!
! ratio of root lenght to depth
!
REAL, PARAMETER                :: XROOTRATIO = 3.0
!
! Amount of gas not consumed by plants
!
REAL, PARAMETER                :: XALPHA_PMT_O2  = 0.3
REAL, PARAMETER                :: XALPHA_PMT_CO2 = 1.0
REAL, PARAMETER                :: XALPHA_PMT_CH4 = 1.0     
!
! Vegetation (aerenchyma) permeability (eq. 29) in Morel et al. (2019)
!
REAL, PARAMETER, DIMENSION(19) :: XPI_AERENCHYMA = (/0.00, & ! NVT_NO
                                                     0.00, & ! NVT_ROCK
                                                     0.00, & ! NVT_SNOW
                                                     1.00, & ! NVT_TEBD
                                                     1.00, & ! NVT_BONE
                                                     1.00, & ! NVT_TRBE
                                                     1.00, & ! NVT_C3
                                                     1.00, & ! NVT_C4
                                                     1.00, & ! NVT_IRR
                                                     1.00, & ! NVT_GRAS
                                                     1.00, & ! NVT_TROG
                                                     1.00, & ! NVT_PARK
                                                     1.00, & ! NVT_TRBD
                                                     1.00, & ! NVT_TEBE
                                                     1.00, & ! NVT_TENE
                                                     1.00, & ! NVT_BOBD
                                                     1.00, & ! NVT_BOND
                                                     1.00, & ! NVT_BOGR
                                                     1.00 /) ! NVT_SHRB
!
!-----------------------------------------------------------------------------------------------
END MODULE MODD_SOILGAS_PAR
