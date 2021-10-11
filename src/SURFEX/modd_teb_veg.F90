!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###################
      MODULE MODD_TEB_VEG
!     ###################
!
!!****  *MODD_TEB_VEG * - declaration of constant parameters

!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare the 
!       constant flags for agricultural practices, assimilation scheme,
!       ST and soil water ice contents & deep soil fields
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
!!      C. de Munck & A. Lemonsu  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!!      K.Chancibault/A.Lemonsu 01/2016   Soilgrid description for urban hydrology
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE 
!
INTEGER, PARAMETER       :: NTIME_GR_MAX  = 1         ! Max NTIME for greenroofs
INTEGER, PARAMETER       :: NLAYER_GR_MAX = 6         ! Max number of soil layers for greenroofs
!
INTEGER, PARAMETER       :: NTEB_SOIL1 = 12           ! Number of soil layers Option2
INTEGER, PARAMETER       :: NTEB_SOIL2 = 14           ! Number of soil layers Option3 
!
INTEGER, PARAMETER       :: NTEB_ROAD1 = 5            ! Number of structural road layers Option2
INTEGER, PARAMETER       :: NTEB_ROAD2 = 9            ! Number of structural road layers Option3

!                                                     ! After Bouiloud (2006) :
REAL,    PARAMETER       :: XBCOEF_STR_ROAD   = 1.        ! Default value for subgrid drainage for structural road
REAL,    PARAMETER       :: XMPOTSAT_STR_ROAD = -0.39     ! Matric potential at saturation for structural road
REAL,    PARAMETER       :: XCONDSAT_STR_ROAD = 1.0E-08   ! Hydraulic conductivity at saturation for structural road
REAL,    PARAMETER       :: XWSAT_STR_ROAD    = 0.06      ! Water content at saturation for structural road
REAL,    PARAMETER       :: XWFC_STR_ROAD     = 0.04      ! Field capacity volumetric water content for structural road
REAL,    PARAMETER       :: XWWILT_STR_ROAD   = 0.02      ! Wilting point water content for structural road
!
REAL, DIMENSION(NTEB_SOIL1), PARAMETER :: XTEB_SOILGRID1 = (/0.001 ,0.01  ,0.04  ,0.10  ,0.20  ,0.40  ,&
                                                             0.60  ,0.80  ,1.00  ,1.50  ,2.00  ,3.00  /)
REAL, DIMENSION(NTEB_SOIL2), PARAMETER :: XTEB_SOILGRID2 = (/0.001 ,0.005 ,0.0435,0.0820,0.210 ,0.338 ,&
                                                             0.466 ,0.594 ,0.768 ,0.80  ,1.00  ,1.50  ,&
                                                             2.00  ,3.00                              /)
!
END MODULE MODD_TEB_VEG
