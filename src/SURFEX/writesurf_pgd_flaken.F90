!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_PGD_FLAKE_n(HPROGRAM)
!     ###################################################
!
!!****  *WRITESURF_PGD_FLAKE_n* - writes FLAKE fields
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
!!      B. Decharme 07/2011 : delete argument HWRITE
!!      M. Moge     02/2015 parallelization using MPI_ALLREDUCE
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_FLAKE_n,      ONLY : XZS,XCOVER,LCOVER, &
      XWATER_DEPTH,XWATER_FETCH,XT_BS,XDEPTH_BS,XEXTCOEF_WATER  
USE MODD_FLAKE_GRID_n, ONLY : XLAT, XLON, XMESH_SIZE, CGRID, XGRID_PAR
!
USE MODI_WRITE_SURF
USE MODI_WRITE_GRID
!
USE MODI_WRITE_LCOVER
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!*       2.     Physiographic data fields:
!               -------------------------
!
!* cover classes
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_FLAKE_N',0,ZHOOK_HANDLE)
!
CALL WRITE_LCOVER(HPROGRAM,LCOVER)
!
YCOMMENT='COVER FIELDS'
 CALL WRITE_SURF(HPROGRAM,'COVER',XCOVER(:,:),LCOVER,IRESP,HCOMMENT=YCOMMENT)
!
!* orography
!
YRECFM='ZS'
YCOMMENT='ZS'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XZS(:),IRESP,HCOMMENT=YCOMMENT)
!
!* latitude, longitude
!
 CALL WRITE_GRID(HPROGRAM,CGRID,XGRID_PAR,XLAT,XLON,XMESH_SIZE,IRESP)
!
!* FLake parameters
!
YRECFM='WATER_DEPTH'
YCOMMENT='X_Y_'//YRECFM//' (m)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XWATER_DEPTH(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='WATER_FETCH'
YCOMMENT='X_Y_'//YRECFM//' (m)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XWATER_FETCH(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='T_BS'
YCOMMENT='X_Y_'//YRECFM//' (K)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XT_BS(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='DEPTH_BS'
YCOMMENT='X_Y_'//YRECFM//' (m)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XDEPTH_BS(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='EXTCOEF_WAT'
YCOMMENT='X_Y_'//YRECFM//'    '
 CALL WRITE_SURF(HPROGRAM,YRECFM,XEXTCOEF_WATER(:),IRESP,HCOMMENT=YCOMMENT)
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_FLAKE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_FLAKE_n
