!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE HOR_INTERPOL_LATLON_1COV(KLUOUT,PFIELDIN,PFIELDOUT)
!     #################################################################################
!
!!****  *HOR_INTERPOL_LATLON_1COV* - Interpolation from a lat/lon regular grid on 1 vertical level only
!!
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
!!     M.Moge (adapted from  HOR_INTERPOL_LATLON)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     06/2015
!!  
!!------------------------------------------------------------------
!
!
!
USE MODD_PREP,             ONLY : XLAT_OUT, XLON_OUT, LINTERP
USE MODD_GRID_LATLONREGUL, ONLY : XILAT1, XILON1, XILAT2, XILON2,    &
                                  NINLAT, NINLON, NILENGTH,XILATARRAY  
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_HORIBL_SURF
USE MODI_ADAPT_HORIBL_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
REAL, DIMENSION(:), INTENT(IN)    :: PFIELDIN  ! field to interpolate horizontally
REAL, DIMENSION(:), INTENT(OUT)   :: PFIELDOUT ! interpolated field
!
!*      0.2    declarations of local variables
!
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASKIN  ! input mask
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASKOUT ! output mask
INTEGER                            :: INO      ! output number of points
INTEGER                            :: JL       ! loop counter
LOGICAL                            :: GREGULAR
REAL                               :: ZDLAT,ZDLAT_REG
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!*      1.    Allocations
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_LATLON1COV',0,ZHOOK_HANDLE)
!
INO = SIZE(XLAT_OUT)
!
ALLOCATE(IMASKIN(NILENGTH))
!
ALLOCATE(IMASKOUT(INO))
!
!*      2.    Initializations
!
GREGULAR= .TRUE.
!
IMASKOUT = 1
!
ZDLAT_REG = (XILAT2-XILAT1)/REAL(NINLAT-1)
!
DO JL=2,NINLAT
   ZDLAT=XILATARRAY(JL)-XILATARRAY(JL-1)
   IF(ZDLAT/=ZDLAT_REG)THEN
     GREGULAR=.FALSE.
   ENDIF
ENDDO
!
!
!*      3. Interpolation with horibl
!
IF(GREGULAR)THEN
     IMASKIN(:) = 1
     WHERE(PFIELDIN(:)==XUNDEF) IMASKIN(:) = 0
     CALL HORIBL_SURF(XILAT1,XILON1,XILAT2,XILON2,NINLAT,NINLON,NILENGTH,            &
                      PFIELDIN(:),INO,XLON_OUT,XLAT_OUT,PFIELDOUT(:),.FALSE.,  &
                      KLUOUT,LINTERP,IMASKIN,IMASKOUT)
ELSE 
     IMASKIN(:) = 1
     WHERE(PFIELDIN(:)==XUNDEF) IMASKIN(:) = 0
     CALL ADAPT_HORIBL_SURF(XILATARRAY,XILAT1,XILON1,XILAT2,XILON2,NINLAT,NINLON,NILENGTH, &
                            PFIELDIN(:),INO,XLON_OUT,XLAT_OUT,PFIELDOUT(:),.FALSE.,  &
                            KLUOUT,LINTERP,IMASKIN,IMASKOUT)
ENDIF
!
!*      6.    Deallocations
!
IF (ALLOCATED(IMASKIN )) DEALLOCATE(IMASKIN )
IF (ALLOCATED(IMASKOUT)) DEALLOCATE(IMASKOUT)
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_LATLON_1COV',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE HOR_INTERPOL_LATLON_1COV
