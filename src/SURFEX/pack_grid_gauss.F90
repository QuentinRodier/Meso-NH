!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!##############################################################
SUBROUTINE PACK_GRID_GAUSS(KMASK_SIZE,KMASK,KGRID_PAR1,PGRID_PAR1,KGRID_PAR2,OPACK,PGRID_PAR2)
!##############################################################
!
!!**** *PACK_GRID_GAUSS* packs the grid definition vector
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!    (B. Decharme)  2008 pack mesh area  
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_PACK_SAME_RANK
USE MODE_GRIDTYPE_GAUSS
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                        INTENT(IN)    :: KMASK_SIZE ! size of mask
INTEGER, DIMENSION(KMASK_SIZE), INTENT(IN)    :: KMASK      ! mask used
INTEGER,                        INTENT(IN)    :: KGRID_PAR1 ! size of input grid vector
REAL,    DIMENSION(KGRID_PAR1), INTENT(IN)    :: PGRID_PAR1 ! parameters of input grid
INTEGER,                        INTENT(INOUT) :: KGRID_PAR2 ! size of output grid vector
LOGICAL,                        INTENT(IN)    :: OPACK      ! flag to pack the grid vector
REAL,    DIMENSION(KGRID_PAR2), INTENT(OUT)   :: PGRID_PAR2 ! parameters of output grid
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER :: INLATI   ! number of pseudo-latitudes
REAL    :: ZLAPO    ! latitude  of the rotated pole (deg)
REAL    :: ZLOPO    ! longitude of the rotated pole (deg)
REAL    :: ZCODIL   ! stretching factor (must be greater than or equal to 1)
INTEGER, DIMENSION(:), ALLOCATABLE :: INLOPA ! number of pseudo-longitudes on each
                                             ! pseudo-latitude circle
REAL, DIMENSION(:), ALLOCATABLE    :: ZLAT1     ! latitude of all grid points
REAL, DIMENSION(:), ALLOCATABLE    :: ZLON1     ! longitude of all grid points
REAL, DIMENSION(:), ALLOCATABLE    :: ZLAT2     ! latitude of subset of grid points
REAL, DIMENSION(:), ALLOCATABLE    :: ZLON2     ! longitude of subset of grid points
REAL, DIMENSION(:), ALLOCATABLE    :: ZLAT_XY1  ! pseudo-latitude of all grid points
REAL, DIMENSION(:), ALLOCATABLE    :: ZLON_XY1  ! pseudo-longitude of all grid points
REAL, DIMENSION(:), ALLOCATABLE    :: ZLAT_XY2  ! pseudo-latitude of subset of grid points
REAL, DIMENSION(:), ALLOCATABLE    :: ZLON_XY2  ! pseudo-longitude of subset of grid points
REAL, DIMENSION(:), ALLOCATABLE    :: ZMESH_SIZE1! 
REAL, DIMENSION(:), ALLOCATABLE    :: ZMESH_SIZE2! 
INTEGER                            :: IL        ! total number of points

REAL, DIMENSION(:), POINTER       :: ZGRID_PAR2 ! parameters of output grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*    1.     Computes grid parameters
!            ------------------------
!
IF (LHOOK) CALL DR_HOOK('PACK_GRID_GAUSS',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_GAUSS(PGRID_PAR1,INLATI,KL=IL)
!
ALLOCATE(INLOPA(INLATI))
ALLOCATE(ZLAT_XY1(IL))
ALLOCATE(ZLON_XY1(IL))
ALLOCATE(ZLAT1(IL))
ALLOCATE(ZLON1(IL))
ALLOCATE(ZMESH_SIZE1(IL))
!
 CALL GET_GRIDTYPE_GAUSS(PGRID_PAR1,INLATI,ZLAPO,ZLOPO,ZCODIL,INLOPA(:), &
                          IL,ZLAT1,ZLON1,ZLAT_XY1,ZLON_XY1,ZMESH_SIZE1)  
!
!----------------------------------------------------------------------------
!
!*    2.     Packs latitude and longitude arrays
!            -----------------------------------
!
!
ALLOCATE(ZLAT_XY2(KMASK_SIZE))
ALLOCATE(ZLON_XY2(KMASK_SIZE))
ALLOCATE(ZLAT2(KMASK_SIZE))
ALLOCATE(ZLON2(KMASK_SIZE))
ALLOCATE(ZMESH_SIZE2(KMASK_SIZE))
!
 CALL PACK_SAME_RANK(KMASK,ZLAT_XY1,ZLAT_XY2)
 CALL PACK_SAME_RANK(KMASK,ZLON_XY1,ZLON_XY2)
 CALL PACK_SAME_RANK(KMASK,ZLAT1,ZLAT2)
 CALL PACK_SAME_RANK(KMASK,ZLON1,ZLON2)
 CALL PACK_SAME_RANK(KMASK,ZMESH_SIZE1,ZMESH_SIZE2)
!
DEALLOCATE(ZLAT_XY1)
DEALLOCATE(ZLON_XY1)
DEALLOCATE(ZLAT1)
DEALLOCATE(ZLON1)
DEALLOCATE(ZMESH_SIZE1)
!
!----------------------------------------------------------------------------
!
!*    3.     Stores data in new grid vector
!            ------------------------------
!
 CALL PUT_GRIDTYPE_GAUSS(ZGRID_PAR2,INLATI,ZLAPO,ZLOPO,ZCODIL,INLOPA(:), &
                          KMASK_SIZE,ZLAT2,ZLON2,ZLAT_XY2,ZLON_XY2,ZMESH_SIZE2)  
!
DEALLOCATE(ZLAT_XY2)
DEALLOCATE(ZLON_XY2)
DEALLOCATE(ZLAT2)
DEALLOCATE(ZLON2)
DEALLOCATE(ZMESH_SIZE2)
!----------------------------------------------------------------------------
! 
IF (OPACK) THEN
  PGRID_PAR2(:) = ZGRID_PAR2(:)
ELSE
  KGRID_PAR2    = SIZE(ZGRID_PAR2(:))
END IF
!
DEALLOCATE(ZGRID_PAR2)
DEALLOCATE(INLOPA)
IF (LHOOK) CALL DR_HOOK('PACK_GRID_GAUSS',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PACK_GRID_GAUSS
