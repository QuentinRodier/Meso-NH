!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PREP_GRID_CONF_PROJ(HFILETYPE,HINTERP_TYPE,KNI)
!     ##########################################################################
!
!!****  *PREP_GRID_CONF_PROJ* - reads EXTERNALIZED Surface grid.
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
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      V. Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   06/2003
!-------------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODI_READ_SURF
!
USE MODD_GRID_CONF_PROJ, ONLY : XX, XY, NX, NY, XLAT0, XLON0, XLATORI, &
                                  XLONORI, XRPK, XBETA  
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* 0.1. Declaration of arguments
!       ------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)    :: HFILETYPE    ! file type
 CHARACTER(LEN=6),  INTENT(OUT)   :: HINTERP_TYPE ! Grid type
INTEGER,           INTENT(OUT)   :: KNI          ! number of points
!
!* 0.2 Declaration of local variables
!      ------------------------------
!
 CHARACTER(LEN=12) :: YRECFM    ! Name of the article to be read
INTEGER           :: IRESP
!
!
INTEGER           :: JL        ! loop counter
REAL, DIMENSION(:), ALLOCATABLE :: ZW ! work array
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------------
!
!*   1 Projection
!      ----------
!
IF (LHOOK) CALL DR_HOOK('PREP_GRID_CONF_PROJ',0,ZHOOK_HANDLE)
YRECFM = 'LAT0'
 CALL READ_SURF(HFILETYPE,YRECFM,XLAT0,IRESP)
YRECFM = 'LON0'
 CALL READ_SURF(HFILETYPE,YRECFM,XLON0,IRESP)
YRECFM = 'RPK'
 CALL READ_SURF(HFILETYPE,YRECFM,XRPK,IRESP)
YRECFM = 'BETA'
 CALL READ_SURF(HFILETYPE,YRECFM,XBETA,IRESP)
!
!-----------------------------------------------------------------------
!
!*   2 Grid
!      ----
!
YRECFM = 'LATORI'
 CALL READ_SURF(HFILETYPE,YRECFM,XLATORI,IRESP)
YRECFM = 'LONORI'
 CALL READ_SURF(HFILETYPE,YRECFM,XLONORI,IRESP)
!
YRECFM = 'IMAX'
 CALL READ_SURF(HFILETYPE,YRECFM,NX,IRESP)
YRECFM = 'JMAX'
 CALL READ_SURF(HFILETYPE,YRECFM,NY,IRESP)
!
KNI = NX * NY
!
ALLOCATE(ZW(KNI))
!
IF (ALLOCATED(XX)) DEALLOCATE(XX)
ALLOCATE(XX(NX))
YRECFM = 'XX'
 CALL READ_SURF(HFILETYPE,YRECFM,ZW,IRESP,HDIR='A')
XX = ZW(1:NX)


IF (ALLOCATED(XY)) DEALLOCATE(XY)
ALLOCATE(XY(NY))
YRECFM = 'YY'
 CALL READ_SURF(HFILETYPE,YRECFM,ZW,IRESP,HDIR='A')
DO JL=1,KNI
  IF (MOD(JL,NX)==0) XY(JL/NX) = ZW(JL)
END DO
DEALLOCATE(ZW)
!
!-----------------------------------------------------------------------
HINTERP_TYPE = 'BILIN '
IF (LHOOK) CALL DR_HOOK('PREP_GRID_CONF_PROJ',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------------
!
END SUBROUTINE PREP_GRID_CONF_PROJ
