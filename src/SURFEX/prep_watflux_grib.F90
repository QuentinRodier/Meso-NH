!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_WATFLUX_GRIB(HPROGRAM,HSURF,HFILE,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_WATFLUX_GRIB* - prepares WATFLUX field from operational GRIB
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
!!     S. Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!
!
USE MODE_READ_GRIB
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_GRIB_GRID
!
USE MODD_PREP,       ONLY : CINGRID_TYPE, CINTERP_TYPE
USE MODD_GRID_GRIB,  ONLY : CGRIB_FILE
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:), POINTER    :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
TYPE (DATE_TIME)                :: TZTIME_GRIB    ! current date and time
 CHARACTER(LEN=6)              :: YINMODEL ! model from which GRIB file originates
REAL, DIMENSION(:)  ,POINTER:: ZMASK => NULL()      ! Land mask
REAL, DIMENSION(:), POINTER :: ZFIELD => NULL()   ! field read
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Reading of grid
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('PREP_WATFLUX_GRIB',0,ZHOOK_HANDLE)
!
IF (TRIM(HFILE).NE.CGRIB_FILE) CGRIB_FILE=""
!
 CALL PREP_GRIB_GRID(HFILE,KLUOUT,YINMODEL,CINGRID_TYPE,TZTIME_GRIB)
!
 CALL READ_GRIB_LAND_MASK(HFILE,KLUOUT,YINMODEL,ZMASK)
!
!
!*      2.     Reading of field
!              ----------------
!
!--------------------
SELECT CASE(HSURF)
!--------------------
!
!* 1.  Orography
!      ---------
!
  CASE('ZS     ')
    SELECT CASE (YINMODEL)
      CASE ('ECMWF ','ARPEGE','ALADIN','MOCAGE')
        CALL READ_GRIB_ZS_LAND(HFILE,KLUOUT,YINMODEL,ZMASK,ZFIELD)
        ALLOCATE(PFIELD(SIZE(ZFIELD),1))
        PFIELD(:,1) = ZFIELD(:)
        DEALLOCATE(ZFIELD)
    END SELECT
!
!* 3.  Temperature profiles
!      --------------------
!
  CASE('TSWATER')
    SELECT CASE (YINMODEL)
      CASE ('ECMWF ','ARPEGE','ALADIN','MOCAGE')
        CALL READ_GRIB_T2(HFILE,KLUOUT,YINMODEL,ZMASK,ZFIELD)
        ALLOCATE(PFIELD(SIZE(ZFIELD),1))
        PFIELD(:,1) = ZFIELD(:)
        DEALLOCATE(ZFIELD)
    END SELECT

END SELECT
!
DEALLOCATE(ZMASK)
!
!*      4.     Interpolation method
!              --------------------
!
CINTERP_TYPE='HORIBL'
!
IF (LHOOK) CALL DR_HOOK('PREP_WATFLUX_GRIB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_WATFLUX_GRIB
