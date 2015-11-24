!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE HOR_INTERPOL_CARTESIAN_1COV(KLUOUT,PFIELDIN,PFIELDOUT)
!     #################################################################################
!
!!****  *HOR_INTERPOL_CARTESIAN_1COV * - Interpolation from a CARTESIAN grid on 1 vertical level only
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     M.Moge (adapted from HOR_INTERPOL_CARTESIAN)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2015
!!------------------------------------------------------------------
!     #################################################################################
!
!
USE MODD_PREP,           ONLY : XLAT_OUT, XLON_OUT, XX_OUT, XY_OUT, LINTERP
USE MODD_GRID_CARTESIAN, ONLY : XX, XY, NX, NY
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODE_GRIDTYPE_CARTESIAN
USE MODI_BILIN
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL, DIMENSION(:), INTENT(IN)  :: PFIELDIN  ! field to interpolate horizontally
REAL, DIMENSION(:), INTENT(OUT) :: PFIELDOUT ! interpolated field
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(:),   ALLOCATABLE :: ZX       ! X coordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZY       ! Y coordinate
INTEGER                           :: INO      ! output number of points
REAL, DIMENSION(:,:), ALLOCATABLE :: ZFIELDIN ! input field
!
INTEGER                           :: JI       ! loop index
INTEGER                           :: JJ       ! loop index
INTEGER                           :: JL       ! loop index
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------------
!
!*      1.    Allocations
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_CARTESIAN_1COV',0,ZHOOK_HANDLE)
INO = SIZE(XX_OUT)
!
ALLOCATE(ZX      (INO))
ALLOCATE(ZY      (INO))
!
!*      2.    Transformation of latitudes/longitudes into metric coordinates of output grid
!
!* WARNING : here, because the input grid is not geographic, one assumes that
!            coordinates are coherent between input and output grid, but without
!            any way to check it.
!
ZX = XX_OUT
ZY = XY_OUT
!
!
!*      3.    Put input field on its squared grid
!
ALLOCATE(ZFIELDIN(NX,NY))
!
DO JJ=1,NY
  DO JI=1,NX
    ZFIELDIN(JI,JJ) = PFIELDIN(JI+NX*(JJ-1))
  END DO
END DO
!
!*      4.    Interpolation with bilinear
!
CALL BILIN(KLUOUT,XX,XY,ZFIELDIN(:,:),ZX,ZY,PFIELDOUT(:),LINTERP)
!
!
!*      5.    Deallocations
!
!
DEALLOCATE(ZX)
DEALLOCATE(ZY)
DEALLOCATE(ZFIELDIN)
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_CARTESIAN_1COV',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE HOR_INTERPOL_CARTESIAN_1COV
