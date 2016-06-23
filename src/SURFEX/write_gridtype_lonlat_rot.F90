!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################
      SUBROUTINE WRITE_GRIDTYPE_LONLAT_ROT (DGU, U, &
                                            HPROGRAM,KLU,KGRID_PAR,PGRID_PAR,KRESP)
!     #################################################################
!
!!****  *WRITE_GRIDTYPE_LONLAT_ROT* - routine to write the horizontal grid
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
!
!
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODI_WRITE_SURF
!
USE MODE_GRIDTYPE_LONLAT_ROT
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
!
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),           INTENT(IN)  :: HPROGRAM   ! calling program
INTEGER,                    INTENT(IN)  :: KLU        ! number of points
INTEGER,                    INTENT(IN)  :: KGRID_PAR  ! size of PGRID_PAR
REAL, DIMENSION(KGRID_PAR), INTENT(IN)  :: PGRID_PAR  ! parameters defining this grid
INTEGER,                    INTENT(OUT) :: KRESP      ! error return code
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
INTEGER :: IL      ! number of points
REAL, DIMENSION(:), ALLOCATABLE :: ZLON ! longitude of points
REAL, DIMENSION(:), ALLOCATABLE :: ZLAT ! latitude  of points
!
 CHARACTER(LEN=100)                :: YCOMMENT ! comment written in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Grid parameters
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_GRIDTYPE_LONLAT_ROT',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_LONLAT_ROT(PGRID_PAR,                                 &
                               ZWEST,ZSOUTH,ZDLON,ZDLAT,ZPOLON,ZPOLAT,  &
                               ILON,ILAT,IL                             )  
!
ALLOCATE(ZLON(IL))
ALLOCATE(ZLAT(IL))
 CALL GET_GRIDTYPE_LONLAT_ROT(PGRID_PAR,PLON=ZLON,PLAT=ZLAT)
!
!---------------------------------------------------------------------------
!
!*       2.    Writing of the grid definition parameters
!              -----------------------------------------
!
YCOMMENT=' '
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'WEST',ZWEST,KRESP,YCOMMENT)
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'SOUTH',ZSOUTH,KRESP,YCOMMENT)
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'DLON',ZDLON,KRESP,YCOMMENT)
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'DLAT',ZDLAT,KRESP,YCOMMENT)
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'POLON',ZPOLON,KRESP,YCOMMENT)
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'POLAT',ZPOLAT,KRESP,YCOMMENT)
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'NLON',ILON,KRESP,YCOMMENT)
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'NLAT',ILAT,KRESP,YCOMMENT)
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'REG_LON',ZLON,KRESP,YCOMMENT)
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'REG_LAT',ZLAT,KRESP,YCOMMENT)
!---------------------------------------------------------------------------
DEALLOCATE(ZLON)
DEALLOCATE(ZLAT)
IF (LHOOK) CALL DR_HOOK('WRITE_GRIDTYPE_LONLAT_ROT',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE WRITE_GRIDTYPE_LONLAT_ROT
