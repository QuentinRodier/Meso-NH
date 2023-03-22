!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
MODULE MODD_LANDUSE_PAR
!     ##############################################################
!
!!**** *MODD_LANDUSE*  declaration of prognostic variables related
!!                             to the vegetation parameterization
!!
!!    PURPOSE
!!    -------
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
!!      Origin  03/2013
!!    R. Séférian 08/2015
!!       
!----------------------------------------------------------------------------
!
IMPLICIT NONE
!
!----------------------------------------------------------------------------
!* Defines Rules for attributing PFTS
!----------------------------------------------------------------------------
!
INTEGER, PARAMETER, DIMENSION(3) :: NECO_NO_VEG = (/1, & ! NVT_NO
                                                    2, & ! NVT_ROCK
                                                    3 /) ! NVT_SNOW
!
INTEGER, PARAMETER, DIMENSION(9) :: NECO_TALL_VEG = (/ 4, & !NVT_TEBD 
                                                       5, & !NVT_BONE
                                                       6, & !NVT_TRBE
                                                      13, & !NVT_TRBD
                                                      14, & !NVT_TEBE
                                                      15, & !NVT_TENE
                                                      16, & !NVT_BOBD
                                                      17, & !NVT_BOND
                                                      19 /) !NVT_SHRB
!
INTEGER, PARAMETER, DIMENSION(7) :: NECO_LOW_VEG = (/10, & !NVT_GRAS
                                                     11, & !NVT_TROG
                                                     18, & !NVT_BOGR
                                                      7, & !NVT_C3
                                                      8, & !NVT_C4
                                                      9, & !NVT_IRR
                                                     12 /) !NVT_PARK
!        
!----------------------------------------------------------------------------
! * Coefficient for Cprod export
!----------------------------------------------------------------------------
!
!! The following tables of parameters for ISBA 19s PFTs are in the following order :
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
! Coeff of biomass export for the year
REAL   , PARAMETER, DIMENSION(19) :: XEXPORT_COEF_ANNUAL =                  &
                                     (/ 0.000, 0.000, 0.000, 0.597, 0.597,  &
                                        0.597, 0.597, 0.597, 0.597, 0.597,  &
                                        0.597, 0.597, 0.597, 0.597, 0.597,  &
                                        0.597, 0.597, 0.597, 0.597 /)
!
! Coeff of biomass export for the decade
REAL   , PARAMETER, DIMENSION(19) :: XEXPORT_COEF_DECADAL =                 &
                                     (/ 0.000, 0.000, 0.000, 0.299, 0.299,  &
                                        0.299, 0.403, 0.403, 0.403 ,0.403,  &
                                        0.403, 0.403, 0.299, 0.299, 0.299,  &
                                        0.299, 0.299, 0.403, 0.299 /)
!
! Coeff of biomass export for the century
REAL   , PARAMETER, DIMENSION(19) :: XEXPORT_COEF_CENTURY =                 &
                                     (/ 0.000, 0.000, 0.000, 0.104, 0.104,  &
                                        0.104, 0.000, 0.000, 0.000, 0.000,  &
                                        0.000, 0.000, 0.104, 0.104, 0.104,  &
                                        0.104, 0.104, 0.000, 0.104 /)
!
! -------------------------------
!
END MODULE MODD_LANDUSE_PAR

