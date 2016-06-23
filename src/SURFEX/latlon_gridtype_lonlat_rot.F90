!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################################################################
      SUBROUTINE LATLON_GRIDTYPE_LONLAT_ROT(KGRID_PAR,KL,PGRID_PAR,PLAT,PLON,PMESH_SIZE,PDIR)
!     #########################################################################
!
!!****  *LATLON_GRIDTYPE_LONLAT_ROT* - routine to compute the horizontal geographic fields
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      P. Samuelsson  SMHI
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/2012 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS,     ONLY : XPI, XRADIUS
!
USE MODE_GRIDTYPE_LONLAT_ROT
!RJ: missing modi
USE MODI_REGROT_LONLAT_ROT
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                    INTENT(IN)  :: KGRID_PAR  ! size of PGRID_PAR
INTEGER,                    INTENT(IN)  :: KL         ! number of points
REAL, DIMENSION(KGRID_PAR), INTENT(IN)  :: PGRID_PAR  ! parameters defining this grid
REAL, DIMENSION(KL),        INTENT(OUT) :: PLAT       ! latitude  (degrees)
REAL, DIMENSION(KL),        INTENT(OUT) :: PLON       ! longitude (degrees)
REAL, DIMENSION(KL),        INTENT(OUT) :: PMESH_SIZE ! mesh size (m2)
REAL, DIMENSION(KL),        INTENT(OUT) :: PDIR ! direction of main grid Y axis (deg. from N, clockwise)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL    :: ZWEST   ! West longitude in rotated grid (degrees)
REAL    :: ZSOUTH  ! South latitude in rotated grid  (degrees)
REAL    :: ZDLON   ! Longitudal grid spacing  (degrees)
REAL    :: ZDLAT   ! Latitudal grid spacing  (degrees)
REAL    :: ZPOLON  ! Longitude of rotated pole (degrees)
REAL    :: ZPOLAT  ! Latitude of rotated pole  (degrees)
INTEGER :: ILON    ! number of points in longitude
INTEGER :: ILAT    ! number of points in latitude
!
REAL, DIMENSION(KL) :: ZLON, ZLAT     ! rotated longitude, latitude
INTEGER :: JLON, JLAT, JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Grid parameters
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_LONLAT_ROT',0,ZHOOK_HANDLE)
!
 CALL GET_GRIDTYPE_LONLAT_ROT(PGRID_PAR,                                 &
                               ZWEST,ZSOUTH,ZDLON,ZDLAT,ZPOLON,ZPOLAT,  &
                               ILON,ILAT,PLON=PLON,PLAT=PLAT            )  
!
!-----------------------------------------------------------------------------
!
!*       2.    Compute grid size
!              -----------------
!
 CALL REGROT_LONLAT_ROT(PLON,PLAT,ZLON,ZLAT,    &
                             KL,1,KL,1,        &
                             ZPOLON,ZPOLAT,1   )  
!
PMESH_SIZE(:) = ( XPI * XRADIUS /180. )**2 * ZDLAT * ZDLON * COS(ZLAT(:)*XPI/180.)
!
!-----------------------------------------------------------------------------
!
!*       4.    Direction of of grid from North for each grid point
!              ---------------------------------------------------
!
PDIR(:) = 0.
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_LONLAT_ROT',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE LATLON_GRIDTYPE_LONLAT_ROT

