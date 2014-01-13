!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB_GREENROOF_GRIB(HPROGRAM,HSURF,HFILE,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_TEB_GREENROOF_GRIB* - initializes ISBA fields from operational GRIB
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!    Based on "prep_teb_garden_grib"
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!    A. Lemonsu & C. de Munck 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!!------------------------------------------------------------------
!

!
USE MODE_READ_GRIB
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_GRIB_GRID
USE MODI_INTERP_GRID
!
USE MODD_PREP,               ONLY : CINGRID_TYPE, CINTERP_TYPE
USE MODD_PREP_TEB_GREENROOF, ONLY : XGRID_SOIL, XWR_DEF
USE MODD_DATA_COVER_PAR,     ONLY : NVEGTYPE
USE MODD_SURF_PAR,           ONLY : XUNDEF
USE MODD_GRID_GRIB,          ONLY : CGRIB_FILE, NNI
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:,:), POINTER  :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
TYPE (DATE_TIME)                :: TZTIME_GRIB          ! current date and time
 CHARACTER(LEN=6)                :: YINMODEL             ! model from which GRIB file originates
REAL, DIMENSION(:)  , POINTER   :: ZMASK => NULL()      ! Land mask
REAL, DIMENSION(:,:), POINTER   :: ZFIELD => NULL()     ! field read
REAL, DIMENSION(:),   POINTER   :: ZFIELD1D => NULL()   ! field read
REAL, DIMENSION(:,:), POINTER   :: ZD => NULL()         ! depth of field in the soil
INTEGER                         :: JVEGTYPE             ! loop counter on vegtypes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Reading of grid
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GREENROOF_GRIB',0,ZHOOK_HANDLE)
!
IF (TRIM(HFILE).NE.CGRIB_FILE) CGRIB_FILE=""
!
 CALL PREP_GRIB_GRID(HFILE,KLUOUT,YINMODEL,CINGRID_TYPE,TZTIME_GRIB)
!
 CALL READ_GRIB_LAND_MASK(HFILE,KLUOUT,YINMODEL,ZMASK)
!
!*      2.     Reading of field
!              ----------------
!
!*      3.     Transformation into physical quantity to be interpolated
!              --------------------------------------------------------
!
SELECT CASE(HSURF)
!
!*      3.1    Profile of temperature in the soil
!
  CASE('TG    ')
     !* reading of the profile and its depth definition
     SELECT CASE(YINMODEL)
       CASE('ECMWF ')
         CALL READ_GRIB_TG_ECMWF(HFILE,KLUOUT,YINMODEL,ZMASK,ZFIELD,ZD)
       CASE('ARPEGE','ALADIN','MOCAGE')
         CALL READ_GRIB_TG_METEO_FRANCE(HFILE,KLUOUT,YINMODEL,ZMASK,ZFIELD,ZD)
       CASE('HIRLAM')
         CALL READ_GRIB_TG_HIRLAM(HFILE,KLUOUT,YINMODEL,ZMASK,ZFIELD,ZD)
     END SELECT
     CALL SOIL_PROFILE_GRIB

  CASE('WG    ')
     !* reading of the profile and its depth definition
     SELECT CASE(YINMODEL)
       CASE('ECMWF ')
         CALL READ_GRIB_WG_ECMWF(HFILE,KLUOUT,YINMODEL,ZMASK,ZFIELD,ZD)
       CASE('ARPEGE','ALADIN','MOCAGE')
         CALL READ_GRIB_WG_METEO_FRANCE(HFILE,KLUOUT,YINMODEL,ZMASK,ZFIELD,ZD)
       CASE('HIRLAM')
         CALL READ_GRIB_WG_HIRLAM(HFILE,KLUOUT,YINMODEL,ZMASK,ZFIELD,ZD)
     END SELECT
     CALL SOIL_PROFILE_GRIB


!*      3.3    Profile of soil ice content

  CASE('WGI   ')    
     !* reading of the profile and its depth definition
     SELECT CASE(YINMODEL)
       CASE('ECMWF ')
         CALL READ_GRIB_WGI_ECMWF(HFILE,KLUOUT,YINMODEL,ZMASK,ZFIELD,ZD)
       CASE('ARPEGE','ALADIN','MOCAGE')
         CALL READ_GRIB_WGI_METEO_FRANCE(HFILE,KLUOUT,YINMODEL,ZMASK,ZFIELD,ZD)
       CASE('HIRLAM')
         CALL READ_GRIB_WGI_HIRLAM(HFILE,KLUOUT,ZFIELD,ZD)
     END SELECT
     CALL SOIL_PROFILE_GRIB
!
!*      3.4    Water content intercepted on leaves, LAI
!
  CASE('WR     ')
     ALLOCATE(PFIELD(NNI,1,NVEGTYPE))
     PFIELD(:,:,:) = XWR_DEF
!
  CASE('LAI    ')
     ALLOCATE(PFIELD(NNI,1,NVEGTYPE))
     PFIELD(:,:,:) = XUNDEF
!
!
!*      3.5    Other fields
!
  CASE('ZS     ')
     CALL READ_GRIB_ZS_LAND(HFILE,KLUOUT,YINMODEL,ZMASK,ZFIELD1D)
     ALLOCATE(PFIELD(SIZE(ZFIELD1D,1),1,1))
     PFIELD(:,1,1)=ZFIELD1D(:)
     DEALLOCATE(ZFIELD1D)

  CASE DEFAULT
     CALL ABOR1_SFX('PREP_TEB_GREENROOF_GRIB: OPTION NOT SUPPORTED - '//HSURF)

END SELECT
!
DEALLOCATE(ZMASK)
!
!*      4.     Interpolation method
!              --------------------
!
CINTERP_TYPE='HORIBL'
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GREENROOF_GRIB',1,ZHOOK_HANDLE)
CONTAINS
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
SUBROUTINE SOIL_PROFILE_GRIB
!-------------------------------------------------------------------------------------
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZOUT   ! work array
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
     !
     !* interpolation on fine vertical grid
     IF (LHOOK) CALL DR_HOOK('SOIL_PROFILE_GRIB',0,ZHOOK_HANDLE)
     ALLOCATE(ZOUT  (SIZE(ZFIELD,1),SIZE(XGRID_SOIL)))
     CALL INTERP_GRID(ZD,ZFIELD,XGRID_SOIL,ZOUT)
     !
     !* extends definition to all vegtypes.
     ALLOCATE(PFIELD(SIZE(ZFIELD,1),SIZE(XGRID_SOIL),NVEGTYPE))
     DO JVEGTYPE=1,NVEGTYPE
       PFIELD(:,:,JVEGTYPE)=ZOUT(:,:)
     END DO
     !* end
     DEALLOCATE(ZOUT)
     DEALLOCATE(ZFIELD)
     DEALLOCATE(ZD)
IF (LHOOK) CALL DR_HOOK('SOIL_PROFILE_GRIB',1,ZHOOK_HANDLE)

END SUBROUTINE SOIL_PROFILE_GRIB
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_TEB_GREENROOF_GRIB
