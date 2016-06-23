!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE HOR_INTERPOL_LATLON(KLUOUT,PFIELDIN,PFIELDOUT)
!     #################################################################################
!
!!****  *HOR_INTERPOL_LATLON* - Interpolation from a lat/lon regular grid
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
!!     C. Lebeaupin Brossier 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!     B. Decharme  07/2014 use usual HORIBL_SURF for regular lat/lon grid
!!                         (ADAPT_HORIBL_SURF is not up to date and is wrong
!!                          for interpolation from a coarse grid to a finer)
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
REAL, DIMENSION(:,:), INTENT(IN)    :: PFIELDIN  ! field to interpolate horizontally
REAL, DIMENSION(:,:), INTENT(OUT)   :: PFIELDOUT ! interpolated field
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
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_LATLON',0,ZHOOK_HANDLE)
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
  DO JL=1,SIZE(PFIELDIN,2)
     IMASKIN(:) = 1
     WHERE(PFIELDIN(:,JL)==XUNDEF) IMASKIN(:) = 0
     CALL HORIBL_SURF(XILAT1,XILON1,XILAT2,XILON2,NINLAT,NINLON,NILENGTH,            &
                      PFIELDIN(:,JL),INO,XLON_OUT,XLAT_OUT,PFIELDOUT(:,JL),.FALSE.,  &
                      KLUOUT,LINTERP,IMASKIN,IMASKOUT)
  ENDDO
ELSE 
  DO JL=1,SIZE(PFIELDIN,2)
     IMASKIN(:) = 1
     WHERE(PFIELDIN(:,JL)==XUNDEF) IMASKIN(:) = 0
     CALL ADAPT_HORIBL_SURF(XILATARRAY,XILAT1,XILON1,XILAT2,XILON2,NINLAT,NINLON,NILENGTH, &
                            PFIELDIN(:,JL),INO,XLON_OUT,XLAT_OUT,PFIELDOUT(:,JL),.FALSE.,  &
                            KLUOUT,LINTERP,IMASKIN,IMASKOUT)
  ENDDO 
ENDIF
!
!*      6.    Deallocations
!
IF (ALLOCATED(IMASKIN )) DEALLOCATE(IMASKIN )
IF (ALLOCATED(IMASKOUT)) DEALLOCATE(IMASKOUT)
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_LATLON',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE HOR_INTERPOL_LATLON
