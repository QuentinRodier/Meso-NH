!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE HOR_INTERPOL_AROME(KLUOUT,PFIELDIN,PFIELDOUT)
!     #################################################################################
!
!!****  *HOR_INTERPOL_AROME * - Interpolation from an AROME grid
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
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!
!
!
USE MODD_PREP,       ONLY : XLAT_OUT, XLON_OUT,LINTERP 
USE MODD_GRID_AROME, ONLY : XX, XY, NX, NY, XLAT0, XLON0, XLATOR, XLONOR, XRPK, XBETA
USE MODD_GRID_GRIB,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODE_GRIDTYPE_CONF_PROJ
USE MODI_HORIBL_SURF
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
REAL, DIMENSION(:,:), INTENT(IN)  :: PFIELDIN  ! field to interpolate horizontally
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELDOUT ! interpolated field
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(:), ALLOCATABLE :: ZX       ! X coordinate
REAL, DIMENSION(:), ALLOCATABLE :: ZY       ! Y coordinate
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASKIN  ! input mask
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASKOUT ! output mask
INTEGER                         :: INO      ! output number of points
INTEGER                         :: JL       ! loop counter
INTEGER, DIMENSION(:), ALLOCATABLE  :: IX  ! number of points on each line
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.    Allocations
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_AROME',0,ZHOOK_HANDLE)
INO = SIZE(XLAT_OUT)
!
ALLOCATE(IMASKIN (NNI))
!
ALLOCATE(ZX      (INO))
ALLOCATE(ZY      (INO))
ALLOCATE(IMASKOUT(INO))
IMASKOUT = 1
ALLOCATE(IX(NY))
IX=NX
!
!*      2.    Transformation of latitudes/longitudes into metric coordinates of input grid
!
 CALL XY_CONF_PROJ(XLAT0,XLON0,XRPK,XBETA,XLATOR,XLONOR,ZX,ZY,XLAT_OUT,XLON_OUT)
!
!
!*      3.    Input mask
!
DO JL=1,SIZE(PFIELDIN,2)
  IMASKIN(:) = 1
  WHERE(PFIELDIN(:,JL)==XUNDEF) IMASKIN = 0
!
!
!*      4.    Interpolation with horibl
!
  CALL HORIBL_SURF(0.,0.,XY,XX,NY,IX,NNI,PFIELDIN(:,JL),INO,ZX,ZY,PFIELDOUT(:,JL), &
                     .FALSE.,KLUOUT,LINTERP,IMASKIN,IMASKOUT)  
END DO
!
!*      5.    Deallocations
!
DEALLOCATE(ZX)
DEALLOCATE(IX)
DEALLOCATE(ZY)
DEALLOCATE(IMASKIN )
DEALLOCATE(IMASKOUT)
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_AROME',1,ZHOOK_HANDLE)

!-------------------------------------------------------------------------------------
END SUBROUTINE HOR_INTERPOL_AROME
