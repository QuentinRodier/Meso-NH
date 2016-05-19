!     #########
      SUBROUTINE PGD_SEAFLUX(HPROGRAM)
!     ##############################################################
!
!!**** *PGD_SEAFLUX* monitor for averaging and interpolations of SEAFLUX physiographic fields
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!    Lebeaupin-B C. 01/2008 : include bathymetry
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGD_GRID,       ONLY : NL
USE MODD_DATA_COVER_PAR,  ONLY : JPCOVER
USE MODD_SEAFLUX_n,       ONLY : XCOVER, LCOVER, XZS
USE MODD_SEAFLUX_GRID_n,  ONLY : CGRID, XGRID_PAR, XLAT, XLON, XMESH_SIZE, NDIM
USE MODD_DATA_SEAFLUX_n,  ONLY : LSST_DATA
!
USE MODI_READ_NAM_PGD_SEABATHY
USE MODI_PGD_BATHYFIELD
!
USE MODI_GET_SURF_SIZE_n
USE MODI_PACK_PGD
USE MODI_PACK_PGD_SEAFLUX
USE MODI_PGD_SEAFLUX_PAR
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
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(NL)               :: ZSEABATHY ! bathymetry on all surface points
!
!*    0.3    Declaration of namelists
!            ------------------------
!
 CHARACTER(LEN=28)        :: YSEABATHY         ! file name for bathymetrie
 CHARACTER(LEN=6)         :: YSEABATHYFILETYPE ! bathymetry data file type
 CHARACTER(LEN=28)        :: YNCVARNAME        ! variable to read in netcdf
                                              ! file
REAL                     :: XUNIF_SEABATHY    ! uniform value of bathymetry
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_SEAFLUX',0,ZHOOK_HANDLE)
 CALL READ_NAM_PGD_SEABATHY(HPROGRAM,YSEABATHY,YSEABATHYFILETYPE,YNCVARNAME,&
       XUNIF_SEABATHY)  
!
!-------------------------------------------------------------------------------
!
!*    3.      Coherence of options
!             --------------------
!
!-------------------------------------------------------------------------------
!
!*    4.      Bathymetry
!             ----------
!
 CALL PGD_BATHYFIELD(HPROGRAM,'bathymetry','SEA',YSEABATHY,YSEABATHYFILETYPE,&
       YNCVARNAME,XUNIF_SEABATHY,ZSEABATHY(:))  
!-------------------------------------------------------------------------------
!
!*    5.      Number of points and packing
!             ----------------------------
!
 CALL GET_SURF_SIZE_n('SEA   ',NDIM)
!
ALLOCATE(LCOVER     (JPCOVER))
ALLOCATE(XCOVER     (NDIM,JPCOVER))
ALLOCATE(XZS        (NDIM))
ALLOCATE(XLAT       (NDIM))
ALLOCATE(XLON       (NDIM))
ALLOCATE(XMESH_SIZE (NDIM))
!
 CALL PACK_PGD(HPROGRAM, 'SEA   ',                    &
                CGRID,  XGRID_PAR,                     &
                LCOVER, XCOVER, XZS,                   &
                XLAT, XLON, XMESH_SIZE                 )  
!
 CALL PACK_PGD_SEAFLUX(HPROGRAM, ZSEABATHY)
!
 CALL PGD_SEAFLUX_PAR(HPROGRAM,LSST_DATA)
IF (LHOOK) CALL DR_HOOK('PGD_SEAFLUX',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_SEAFLUX
