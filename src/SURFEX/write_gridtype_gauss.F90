!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!#################################################################
SUBROUTINE WRITE_GRIDTYPE_GAUSS(HPROGRAM,KLU,KGRID_PAR,PGRID_PAR,KRESP)
!#################################################################
!
!!****  *WRITE_GRIDTYPE_GAUSS* - routine to write the horizontal grid
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_WRITE_SURF
!
USE MODE_GRIDTYPE_GAUSS
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
 CHARACTER(LEN=6),           INTENT(IN)  :: HPROGRAM   ! calling program
INTEGER,                    INTENT(IN)  :: KLU        ! number of points
INTEGER,                    INTENT(IN)  :: KGRID_PAR  ! size of PGRID_PAR
REAL, DIMENSION(KGRID_PAR), INTENT(IN)  :: PGRID_PAR  ! parameters defining this grid
INTEGER,                    INTENT(OUT) :: KRESP      ! error return code
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: INLATI  ! number of pseudo-latitudes
REAL    :: ZLAPO   ! latitude  of the rotated pole (deg)
REAL    :: ZLOPO   ! longitude of the rotated pole (deg)
REAL    :: ZCODIL  ! stretching factor (must be greater than or equal to 1)
INTEGER, DIMENSION(:), ALLOCATABLE :: INLOPA ! number of pseudo-longitudes on each
                                             ! pseudo-latitude circle
REAL,    DIMENSION(KLU) :: ZLAT ! latitudes
REAL,    DIMENSION(KLU) :: ZLON ! longitudes
REAL,    DIMENSION(KLU) :: ZLAT_XY
REAL,    DIMENSION(KLU) :: ZLON_XY
REAL,    DIMENSION(KLU) :: ZMESH_SIZE

INTEGER                            :: IL    ! total number of points
!
 CHARACTER(LEN=100)                :: YCOMMENT ! comment written in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Projection and 2D grid parameters
!              ---------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_GRIDTYPE_GAUSS',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_GAUSS(PGRID_PAR,INLATI)
!
ALLOCATE(INLOPA(INLATI))
 CALL GET_GRIDTYPE_GAUSS(PGRID_PAR,INLATI,ZLAPO,ZLOPO,ZCODIL,INLOPA(:),IL,&
                          ZLAT,ZLON,ZLAT_XY,ZLON_XY,ZMESH_SIZE)  
!
!---------------------------------------------------------------------------
!
!*       2.    Writing of the grid definition parameters
!              -----------------------------------------
!
YCOMMENT=' '
 CALL WRITE_SURF(HPROGRAM,'NLATI',INLATI,KRESP,YCOMMENT)
 CALL WRITE_SURF(HPROGRAM,'LAPO',ZLAPO, KRESP,YCOMMENT)
 CALL WRITE_SURF(HPROGRAM,'LOPO',ZLOPO,KRESP,YCOMMENT)
 CALL WRITE_SURF(HPROGRAM,'CODIL',ZCODIL,KRESP,YCOMMENT)
 CALL WRITE_SURF(HPROGRAM,'NLOPA',INLOPA(:),KRESP,YCOMMENT,HDIR='-')
 CALL WRITE_SURF(HPROGRAM,'LATGAUSS',ZLAT(:),KRESP,YCOMMENT)
 CALL WRITE_SURF(HPROGRAM,'LONGAUSS',ZLON(:),KRESP,YCOMMENT)
 CALL WRITE_SURF(HPROGRAM,'LAT_G_XY',ZLAT_XY(:),KRESP,YCOMMENT)
 CALL WRITE_SURF(HPROGRAM,'LON_G_XY',ZLON_XY(:),KRESP,YCOMMENT)
 CALL WRITE_SURF(HPROGRAM,'MESHGAUSS',ZMESH_SIZE(:),KRESP,YCOMMENT)
!
!---------------------------------------------------------------------------
DEALLOCATE(INLOPA)
IF (LHOOK) CALL DR_HOOK('WRITE_GRIDTYPE_GAUSS',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE WRITE_GRIDTYPE_GAUSS
