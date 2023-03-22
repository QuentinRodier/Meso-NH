!     ########################
      MODULE MODD_SOILCARB_PAR
!     ########################
!
!!*****MODD_SOILCARB_PAR*  
!!
!!    PURPOSE
!!    -------
!!    Supply constants and parameter values for dynamic soil carbon scheme
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
! bioturbation diffusivity from Johson et al. (2014, EPSL)
!
REAL, PARAMETER                :: XBIOREF = 0.195E-3 !(m2/year)
!
! bioturbation diffusivity e-folding depth from Jackson (1990) root profile : b = -1/(100*ln(root_ext))
!
REAL, PARAMETER, DIMENSION(19) :: XZBIO   = (/0.00, & ! NVT_NO
                                              0.00, & ! NVT_ROCK
                                              0.00, & ! NVT_SNOW
                                              0.29, & ! NVT_TEBD
                                              0.17, & ! NVT_BONE
                                              0.26, & ! NVT_TRBE
                                              0.25, & ! NVT_C3
                                              0.35, & ! NVT_C4
                                              0.35, & ! NVT_IRR
                                              0.17, & ! NVT_GRAS
                                              0.35, & ! NVT_TROG
                                              0.17, & ! NVT_PARK
                                              0.25, & ! NVT_TRBD
                                              0.29, & ! NVT_TEBE
                                              0.41, & ! NVT_TENE
                                              0.17, & ! NVT_BOBD
                                              0.17, & ! NVT_BOND
                                              0.11, & ! NVT_BOGR
                                              0.27 /) ! NVT_SHRB
!
REAL, PARAMETER                :: XROOTBIO = 0.95 ! Bioturbation limit parameter equivalent to 95% of root profile
!
REAL, PARAMETER                :: XDBIO    = 0.2  ! Bioturbation at least over 0.2 m
!
! cryoturbation diffusivity from Koven et al. (2009 & 2013)
!
REAL, PARAMETER                :: XDCRYO     = 0.1    ! cryoturbation active if k*alt > 0.1 m
!
REAL, PARAMETER                :: XDMAX_CRYO = 4.0    ! cryoturbation depth limit (m)
!
REAL, PARAMETER                :: XCRYOREF   = 0.5E-3 !(m2/year)
!
REAL, PARAMETER                :: XMAXALT    = 1.0    ! maximum depth where Cryoturbation is potential
!
REAL, PARAMETER                :: XKALT      = 3.0    ! maximum mixing depth ratio
!
! Advection constant
!
REAL, PARAMETER                :: XADVREF    = 2.0E-3 ! (m/an) Morel 2019  : 2.0 mm/an
                                                      !        Guenet 2013 : 0.5 mm/an
!
!-----------------------------------------------------------------------------------------------
END MODULE MODD_SOILCARB_PAR
