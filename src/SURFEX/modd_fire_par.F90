!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
MODULE MODD_FIRE_PAR
!     ##############################################################
!
!!**** *MODD_FIRE_PAR*  declaration of prognostic variables related
!!                             to the vegetation parameterization
!!
!!    PURPOSE
!!    -------
!!     The purpose of this declarative module is to specify  the 
!!     parameters related to dgvm (dynamical vegetation model).
!!     contains physical dgvm constantes
!!
!!    METHOD  
!!    ------
!!    
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    R. Alkama          Meteo-France 
!! 
!!
!!    MODIFICATION
!!    ------------
!!      Origin 08/2015
!!       
!----------------------------------------------------------------------------
!
IMPLICIT NONE
!
! Time scale for memory of the fire index (days).
!
REAL, PARAMETER    :: XTAU_FIRE = 7. 
!-
! biomass : indices
!-
INTEGER, PARAMETER :: ILEAF            = 1
INTEGER, PARAMETER :: ISAPABOVEACTIVE  = 2  
INTEGER, PARAMETER :: ISAPABOVEPASSIVE = 3
INTEGER, PARAMETER :: IROOT            = 4
INTEGER, PARAMETER :: IWOODABOVE       = 5
INTEGER, PARAMETER :: IWOODBELOW       = 6
!-
! Critical litter quantity for fire
!
REAL, PARAMETER    :: XLITTER_CRIT = 200.
REAL, PARAMETER    :: XLITTER_MAX  = 300.
REAL, PARAMETER    :: XTEMP_LITTER_CRIT = 20. ! °C
!
!-
! Epsilon to detect a near zero floating point
!
REAL, PARAMETER    :: XMIN_FIRE = 1.E-8  
!-
!
!! The following tables of parameters for ISBA 19s PFTs
!! are in the following order :
!!NVT_NO   = 1   ! 1  ! no vegetation (smooth)
!!NVT_ROCK = 2   ! 2  ! no vegetation (rocks)
!!NVT_SNOW = 3   ! 3  ! permanent snow and ice
!!NVT_TEBD = 4   ! 4  ! temperate broadleaf cold-deciduous summergreen (TREE)
!!NVT_BONE = 5   ! 5  ! boreal needleleaf evergreen  (CONI)
!!NVT_TRBE = 6   ! 6  ! tropical broadleaf evergreen (EVER)
!!NVT_C3   = 7   ! 7  ! C3 cultures types
!!NVT_C4   = 8   ! 8  ! C4 cultures types
!!NVT_IRR  = 9   ! 9  ! irrigated crops
!!NVT_GRAS =10   !10  ! grassland
!!NVT_TROG =11   !11  ! tropical grassland
!!NVT_PARK =12   !12  ! peat bogs, parks and gardens (irrigated grass)
!!NVT_TRBD =13   ! 4  ! tropical broadleaf deciduous (TREE)
!!NVT_TEBE =14   ! 4  ! temperate broadleaf evergreen (TREE)
!!NVT_TENE =15   ! 5  ! temperate needleleaf evergreen (CONI)
!!NVT_BOBD =16   ! 4  ! boreal broadleaf cold-deciduous summergreen (TREE)
!!NVT_BOND =17   ! 5  ! boreal needleleaf cold-deciduous summergreen (CONI)
!!NVT_BOGR =18   !10  ! boreal grass (GRAS)
!!NVT_SHRB =19   ! 4  ! shrub (TREE)
!  
! Is the vegetation type a tree ?
!
REAL, PARAMETER, DIMENSION(19) :: XTREE =      &
 & (/ 0., 0., 0., 1., 1., 1., 0., 0., 0., 0.,  &
 &    0., 0., 1., 1., 1., 1., 1., 0., 1.      /)
!
! natural patches
!
REAL, PARAMETER, DIMENSION(19) :: XNATURAL =  &
 & (/ 0., 0., 0., 1., 1., 1., 0., 0., 0., 1., &
 &    1., 0., 1., 1., 1., 1., 1., 1., 1.     /)
!
! flamability: critical fraction of water holding capacity
!
REAL, PARAMETER, DIMENSION(19)      :: XFLAM =          &
 & (/ 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, &
 &    1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0      /)
!
! fire emissions efficacy
! in kg C/kg DM
! for CO2
!
REAL, PARAMETER, DIMENSION(19)      :: XFEECO2 =        &
 &  (/ 0.000,0.000,0.000,1.572,1.572,1.636,0.000,0.000, &
 &     0.000,1.646,1.646,0.000,1.636,1.572,1.572,1.572, &
 &     1.572,1.646,1.572 /)
!
! for black carbon
!
REAL, PARAMETER, DIMENSION(19)      :: XFEEBSC =              &
 &  (/ 0.0, 0.0, 0.0, 0.56e-3,0.56e-3,0.52e-3,0.46e-3,        &
 &     0.46e-3,0.46e-3,0.46e-3,0.46e-3, 0.57e-3,0.52e-3,      &
 &     0.56e-3,0.56e-3,0.56e-3,0.56e-3,0.46e-3,0.56e-3 /)
!
!  Combustion completness factor for Leaf, stem (li et al. 2012)
!
REAL, PARAMETER, DIMENSION(19)      :: XCCLEAF =         &
 & (/ 0.00, 0.00, 0.00, 0.70, 0.75, 0.70, 0.85, 0.85, 0.85, 0.85, &
 &    0.85, 0.00, 0.70, 0.70, 0.75, 0.70, 0.75, 0.85, 0.80       /)
!
REAL, PARAMETER, DIMENSION(19)      :: XCCSTEM =         &
 & (/ 0.00, 0.00, 0.00, 0.10, 0.20, 0.15, 0.00, 0.00, 0.00, 0.00, &
 &    0.00, 0.00, 0.10, 0.15, 0.20, 0.15, 0.20, 0.00, 0.30       /)
!
!  Combustion completness factor for Litter (li et al. 2012)
!
REAL, PARAMETER, DIMENSION(19)      :: XCCLITTER =       &
 & (/ 0.00, 0.00, 0.00, 0.45, 0.55, 0.50, 0.85, 0.85, 0.85, 0.85, &
 &    0.85, 0.00, 0.45, 0.50, 0.55, 0.50, 0.55, 0.85, 0.60       /)
!
!  Tissue-mortality factor for Leaf, stem, root (li et al. 2012)
!
REAL, PARAMETER, DIMENSION(19)      :: XMLEAF =         &
 & (/ 0.00, 0.00, 0.00, 0.70, 0.75, 0.70, 0.85, 0.85, 0.85, 0.85, &
 &    0.85, 0.00, 0.70, 0.70, 0.75, 0.70, 0.75, 0.85, 0.80       /)
!
REAL, PARAMETER, DIMENSION(19)      :: XMSTEM =         &
 & (/ 0.00, 0.00, 0.00, 0.55, 0.65, 0.60, 0.00, 0.00, 0.00, 0.00, &
 &    0.00, 0.00, 0.55, 0.60, 0.65, 0.60, 0.65, 0.00, 0.70       /)
!
REAL, PARAMETER, DIMENSION(19)      :: XMROOT =         &
 & (/ 0.00, 0.00, 0.00, 0.07, 0.13, 0.10, 0.20, 0.20, 0.20, 0.20, &
 &    0.20, 0.00, 0.07, 0.10, 0.13, 0.10, 0.10, 0.20, 0.15       /)
!
END MODULE MODD_FIRE_PAR
