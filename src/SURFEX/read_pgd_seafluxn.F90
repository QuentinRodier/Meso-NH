!     #########
      SUBROUTINE READ_PGD_SEAFLUX_n(HPROGRAM)
!     #########################################
!
!!****  *READ_PGD_SEAFLUX_n* - routine to read SEAFLUX physiographic fields
!!
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
!!      Original    01/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
USE MODD_SEAFLUX_n,      ONLY : XCOVER, XZS, XSEABATHY, TTIME, LCOVER
USE MODD_SEAFLUX_GRID_n, ONLY : XLAT, XLON, XMESH_SIZE, CGRID, XGRID_PAR, NDIM
USE MODD_DATA_SEAFLUX_n, ONLY : LSST_DATA
!
!
USE MODI_READ_SURF
USE MODI_READ_GRID
USE MODI_READ_LCOVER
USE MODI_READ_PGD_SEAFLUX_PAR_n
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! Error code after redding
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
!
INTEGER           :: IVERSION   ! surface version
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_SEAFLUX_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_SEA'
 CALL GET_TYPE_DIM_n('SEA   ',NDIM)
!
!
!*       2.     Physiographic data fields:
!               -------------------------
!
!* cover classes
!
ALLOCATE(LCOVER(JPCOVER))
 CALL READ_LCOVER(HPROGRAM,LCOVER)
!
ALLOCATE(XCOVER(NDIM,JPCOVER))
 CALL READ_SURF(HPROGRAM,'COVER',XCOVER(:,:),LCOVER,IRESP)
!
!* orography
!
ALLOCATE(XZS(NDIM))
XZS(:) = 0.
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
!* bathymetry
!
ALLOCATE(XSEABATHY(NDIM))
IF (IVERSION<=3) THEN
  XSEABATHY(:) = 300.
ELSE
  YRECFM='BATHY'
  CALL READ_SURF(HPROGRAM,YRECFM,XSEABATHY(:),IRESP)
END IF
!
!* latitude, longitude 
!
ALLOCATE(XLAT      (NDIM))
ALLOCATE(XLON      (NDIM))
ALLOCATE(XMESH_SIZE(NDIM))
 CALL READ_GRID(HPROGRAM,CGRID,XGRID_PAR,XLAT,XLON,XMESH_SIZE,IRESP)
!
!
!* sst
!
!
IF (IVERSION<3) THEN
  LSST_DATA = .FALSE.
ELSE
  YRECFM='SST_DATA'
  CALL READ_SURF(HPROGRAM,YRECFM,LSST_DATA,IRESP)
END IF
!
IF (LSST_DATA) CALL READ_PGD_SEAFLUX_PAR_n(HPROGRAM,NDIM)
IF (LHOOK) CALL DR_HOOK('READ_PGD_SEAFLUX_N',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------!
END SUBROUTINE READ_PGD_SEAFLUX_n
