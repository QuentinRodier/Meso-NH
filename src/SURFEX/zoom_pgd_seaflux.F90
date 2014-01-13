!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ######spl
      SUBROUTINE ZOOM_PGD_SEAFLUX(HPROGRAM,HINIFILE,HINIFILETYPE,HFILE,HFILETYPE)
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
!!    P. Le Moigne     Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2008
!!    G. TANGUY   03/2009 : add reading and interpolation of XDATA_SST and 
!!                          TDATA_SST in the case LDATA_SST=T
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_PAR,  ONLY : JPCOVER
USE MODD_PREP,             ONLY : CINGRID_TYPE, CINTERP_TYPE, LINTERP
!
USE MODD_SEAFLUX_n,       ONLY : XCOVER, LCOVER, XZS, XSEABATHY
USE MODD_SEAFLUX_GRID_n,  ONLY : CGRID, XGRID_PAR, XLAT, XLON, XMESH_SIZE, NDIM
USE MODD_DATA_SEAFLUX_n,    ONLY : LSST_DATA, NTIME, XDATA_SST, TDATA_SST
!
USE MODI_GET_LUOUT
USE MODI_OPEN_AUX_IO_SURF
USE MODI_GET_SURF_SIZE_n
USE MODI_PACK_PGD
USE MODI_PREP_GRID_EXTERN
USE MODI_PREP_OUTPUT_GRID
USE MODI_READ_SURF
USE MODI_HOR_INTERPOL
USE MODI_READ_PGD_SEAFLUX_PAR_n
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_CLEAN_PREP_OUTPUT_GRID
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM    ! Type of program
 CHARACTER(LEN=28),    INTENT(IN)  :: HINIFILE    ! input atmospheric file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HINIFILETYPE! input atmospheric file type
 CHARACTER(LEN=28),    INTENT(IN)  :: HFILE       ! output file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HFILETYPE   ! output file type
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!*    0.3    Declaration of namelists
!            ------------------------
!
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZSEABATHY, ZWORK
INTEGER :: ILUOUT
INTEGER :: INI
INTEGER :: IRESP
INTEGER           :: JTIME          ! loop index
INTEGER           :: IVERSION, IBUGFIX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_SEAFLUX',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*      1.     Preparation of IO for reading in the file
!              -----------------------------------------
!
!* Note that all points are read, even those without physical meaning.
!  These points will not be used during the horizontal interpolation step.
!  Their value must be defined as XUNDEF.
!
!
 CALL OPEN_AUX_IO_SURF(HINIFILE,HINIFILETYPE,'FULL  ')
!
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
 CALL PACK_PGD(HPROGRAM, 'SEA   ',                      &
                CGRID,  XGRID_PAR, LCOVER,             &
                XCOVER, XZS,                           &
                XLAT, XLON, XMESH_SIZE                 )  
!
!------------------------------------------------------------------------------
!
!*      2.     Reading of grid
!              ---------------
!
 CALL PREP_GRID_EXTERN(HINIFILETYPE,ILUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
!
 CALL PREP_OUTPUT_GRID(ILUOUT,CGRID,XGRID_PAR,XLAT,XLON)
!
!* mask where interpolations must be done
!
LINTERP(:) = .TRUE.
!
!------------------------------------------------------------------------------
!
!*      3.     Reading of fields
!              -----------------
!
ALLOCATE(ZSEABATHY(INI,1))
 CALL READ_SURF(HPROGRAM,'BATHY',ZSEABATHY(:,1),IRESP,HDIR='A')
!
ALLOCATE(ZWORK(NDIM,1))
 CALL HOR_INTERPOL(ILUOUT,ZSEABATHY(:,1:1),ZWORK(:,1:1)) 
ALLOCATE(XSEABATHY (NDIM))
XSEABATHY(:) = ZWORK(:,1)
DEALLOCATE(ZSEABATHY,ZWORK)
!
!============================================================
! G. TANGUY 03/2009
! reading of fields for SST_DATA
 CALL READ_SURF(HPROGRAM,'SST_DATA',LSST_DATA,IRESP)
!
IF (LSST_DATA) CALL READ_PGD_SEAFLUX_PAR_n(HPROGRAM,INI,HDIR='A')
!
!============================================================
!
 CALL CLOSE_AUX_IO_SURF(HINIFILE,HINIFILETYPE)
!
!============================================================
!
 CALL CLEAN_PREP_OUTPUT_GRID
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_SEAFLUX',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE ZOOM_PGD_SEAFLUX
