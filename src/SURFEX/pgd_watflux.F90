!     #########
      SUBROUTINE PGD_WATFLUX(HPROGRAM)
!     ##############################################################
!
!!**** *PGD_WATFLUX* monitor for averaging and interpolations of WATFLUX physiographic fields
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
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
USE MODD_WATFLUX_n,       ONLY : XCOVER, LCOVER, XZS
USE MODD_WATFLUX_GRID_n,  ONLY : CGRID, XGRID_PAR, XLAT, XLON, XMESH_SIZE, NDIM
!
USE MODI_GET_SURF_SIZE_n
USE MODI_PACK_PGD
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_WRITE_COVER_TEX_WATER
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!*    0.3    Declaration of namelists
!            ------------------------
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
!-------------------------------------------------------------------------------
!
!*    3.      Coherence of options
!             --------------------
!
!-------------------------------------------------------------------------------
!
!*    4.      Number of points and packing
!             ----------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_WATFLUX',0,ZHOOK_HANDLE)
 CALL GET_SURF_SIZE_n('WATER ',NDIM)
!
ALLOCATE(LCOVER     (JPCOVER))
ALLOCATE(XCOVER     (NDIM,JPCOVER))
ALLOCATE(XZS        (NDIM))
ALLOCATE(XLAT       (NDIM))
ALLOCATE(XLON       (NDIM))
ALLOCATE(XMESH_SIZE (NDIM))
!
 CALL PACK_PGD(HPROGRAM, 'WATER ',                    &
                CGRID,  XGRID_PAR,                     &
                LCOVER, XCOVER, XZS,                   &
                XLAT, XLON, XMESH_SIZE                 )  
!
!-------------------------------------------------------------------------------
 CALL WRITE_COVER_TEX_WATER
IF (LHOOK) CALL DR_HOOK('PGD_WATFLUX',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_WATFLUX
