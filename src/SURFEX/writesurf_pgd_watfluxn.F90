!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_PGD_WATFLUX_n(HPROGRAM)
!     ###################################################
!
!!****  *WRITESURF_PGD_WATFLUX_n* - writes WATFLUX fields
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_WATFLUX_n,      ONLY : XZS,XCOVER,LCOVER
USE MODD_WATFLUX_GRID_n, ONLY : XLAT, XLON, XMESH_SIZE, CGRID, XGRID_PAR
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODI_WRITE_SURF
USE MODI_WRITE_GRID
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
INTEGER           :: JCOVER         ! loop index
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
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_WATFLUX_N',0,ZHOOK_HANDLE)
YRECFM='COVER_LIST'
YCOMMENT='(LOGICAL LIST)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,LCOVER(:),IRESP,HCOMMENT=YCOMMENT,HDIR='-')
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
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_WATFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_WATFLUX_n
