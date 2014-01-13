!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ######################
      MODULE MODD_DATA_COVER_PAR
!     ######################
!
!!****  *MODD_DATA_COVER_PAR* - declaration of prognostic variables related
!!                          to the ground parameterization
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the 
!     parameters related to the surface parameterization interface.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!	S. Belair   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       29/04/95                      
!!      (V.Masson)     05/10/98+ add XCDZ0EFF, XRHOSMIN, XRHOSMAX
!!      (V.Masson)     15/03/99 add number of layers
!!      (A.Boone)      02/05/02 add ISBA-ES parameters
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!------------------------------------------------------------------------------
!
! Maximum number of cover classes
INTEGER, PARAMETER    :: JPCOVER  = 573
!last class number of EUROPE
INTEGER, PARAMETER    :: NCOVER_ECO1_END    = 255
!firt class number of EUROPE
INTEGER, PARAMETER    :: NCOVER_ECO2_START  = 301
!
INTEGER                                :: NVEGTYPE
! number of vegetation types
!
INTEGER                                :: NVT_C4
INTEGER                                :: NVT_TREE
INTEGER                                :: NVT_CONI
INTEGER                                :: NVT_EVER
INTEGER                                :: NVT_GRAS
INTEGER                                :: NVT_TROG
INTEGER                                :: NVT_C3
INTEGER                                :: NVT_NO
INTEGER                                :: NVT_ROCK
INTEGER                                :: NVT_SNOW
INTEGER                                :: NVT_IRR
INTEGER                                :: NVT_PARK
! indexes of these types (C4 cultures types, Broadleaf TREEs, CONIferous trees,
!                         EVERgreen broadleaf trees, GRASsland, TROpical Grass,
!                         C3 CULTures types, NO vegetation, ROCKs,
!                         IRRigated crops, irrigated PARKs gardens or peat bogs)
!
!---------------------------------------------------------------------------------
!
! Ecosystem corresponding to bare soil or permanent snow
!
INTEGER                                :: NBARE_SOIL
INTEGER                                :: NROCK
INTEGER                                :: NPERMSNOW
!--------------------------------------------------------------------------------- 
!
! Free water bodies
!
INTEGER, DIMENSION(2)                  :: NSEA
INTEGER, DIMENSION(3)                  :: NWATER
!
!---------------------------------------------------------------------------------
!
! maximum number of tiles used at any grid point
!
INTEGER                                :: NTILESFC = 4
!
!---------------------------------------------------------------------------------
!
!* building layers in the ecoclimap data
!
INTEGER                                :: NDATA_ROOF_LAYER
INTEGER                                :: NDATA_ROAD_LAYER
INTEGER                                :: NDATA_WALL_LAYER
INTEGER                                :: NDATA_FLOOR_LAYER
!
!---------------------------------------------------------------------------------
!* names of all ecosystems
!
 CHARACTER(LEN=60), DIMENSION(JPCOVER,2) :: CNAMES
!---------------------------------------------------------------------------------
!
! reference height for CDN averaging
!
REAL                                :: XCDREF = 20.
!
!---------------------------------------------------------------------------------
END MODULE MODD_DATA_COVER_PAR












