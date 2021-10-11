!     ###################
      MODULE MODD_TEB_STRUCT_ROAD
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
!!	C. de Munck & A. Lemonsu  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!!      K.Chancibault/A.Lemonsu 01/2016   Soilgrid description for urban hydrology
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE 
!
!                                                     ! After Bouiloud (2006) :
REAL,    PARAMETER   :: XBCOEF_COAT_ROAD   = 1.        ! Default value for subgrid drainage for road coating
REAL,    PARAMETER   :: XMPOTSAT_COAT_ROAD = -0.39     ! Matric potential at saturation for road coating
REAL,    PARAMETER   :: XCONDSAT_COAT_ROAD = 1.0E-08   ! Hydraulic conductivity at saturation for road coating
REAL,    PARAMETER   :: XWSAT_COAT_ROAD    = 0.06      ! Water content at saturation for road coating
REAL,    PARAMETER   :: XWFC_COAT_ROAD     = 0.04      ! Field capacity volumetric water content for road coating
REAL,    PARAMETER   :: XWWILT_COAT_ROAD   = 0.02      ! Wilting point water content for road coating
!
REAL,    PARAMETER   :: XHC_COAT_ROAD      = 1.94 * 1.E6 ! Heat capacity for road coating
REAL,    PARAMETER   :: XTC_COAT_ROAD      = 0.75      ! thermal capacity for road coating

REAL,    PARAMETER   :: XBCOEF_BASE_ROAD   = 1. ! TBC       ! Default value for subgrid drainage for road basement
REAL,    PARAMETER   :: XMPOTSAT_BASE_ROAD = -0.39 ! TBC       ! Matric potential at saturation for road basement
REAL,    PARAMETER   :: XCONDSAT_BASE_ROAD = 1.E-8 ! TBC       ! Hydraulic conductivity at saturation for road basement
REAL,    PARAMETER   :: XWSAT_BASE_ROAD    = 0.06 ! TBC       ! Water content at saturation for road basement
REAL,    PARAMETER   :: XWFC_BASE_ROAD     = 0.04 ! TBC       ! Field capacity volumetric water content for road basement
REAL,    PARAMETER   :: XWWILT_BASE_ROAD   = 0.02 ! TBC       ! Wilting point water content for road basement
!
REAL,    PARAMETER   :: XHC_BASE_ROAD      = 1.28 * 1.E6 ! Heat capacity for road basement
REAL,    PARAMETER   :: XTC_BASE_ROAD      = 0.25      ! thermal capacity for road basement
!
END MODULE MODD_TEB_STRUCT_ROAD
