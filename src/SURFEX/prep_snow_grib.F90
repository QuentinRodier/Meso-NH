!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_SNOW_GRIB(HPROGRAM,HSURF,HFILE,KLUOUT,KLAYER,PFIELD)
!     #################################################################################
!
!!****  *PREP_SNOW_GRIB* - prepares snow field from operational GRIB
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
!!      Original     01/2004
!!      C. Ardilouze 06/2013 read snow albedo and density (for Erai-land)
!!------------------------------------------------------------------
!
!
USE MODE_READ_GRIB
USE MODE_SNOW3L
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_GRIB_GRID
USE MODI_SNOW_T_WLIQ_TO_HEAT
!
USE MODD_PREP,           ONLY : CINGRID_TYPE, CINTERP_TYPE
USE MODD_PREP_SNOW,      ONLY : NGRID_LEVEL, XGRID_SNOW
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_GRID_GRIB,      ONLY : CGRIB_FILE, NNI
USE MODD_SNOW_PAR,       ONLY : XANSMIN, XANSMAX, XRHOSMAX
USE MODD_CSTS,           ONLY : XTT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)    :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=10),   INTENT(IN)   :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)    :: HFILE     ! name of file
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
INTEGER,            INTENT(IN)    :: KLAYER    ! Number of layer of output snow scheme
REAL,DIMENSION(:,:,:), POINTER    :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
TYPE (DATE_TIME)                :: TZTIME_GRIB    ! current date and time
 CHARACTER(LEN=6)                :: YINMODEL       ! model from which GRIB file originates
REAL, DIMENSION(:)  , POINTER   :: ZMASK => NULL()          ! Land mask
REAL, DIMENSION(:),   POINTER   :: ZFIELD1D => NULL()       ! field read
REAL, DIMENSION(:),   POINTER   :: ZHEAT => NULL()          ! heat in snow
REAL, DIMENSION(:),   POINTER   :: ZRHO => NULL()          ! density of snow
INTEGER                         :: JVEGTYPE       ! loop counter on vegtypes
INTEGER                         :: JLAYER         ! loop on snow fine grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Reading of grid
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('PREP_SNOW_GRIB',0,ZHOOK_HANDLE)
!
IF (TRIM(HFILE).NE.CGRIB_FILE) CGRIB_FILE=""
!
 CALL PREP_GRIB_GRID(HFILE,KLUOUT,YINMODEL,CINGRID_TYPE,TZTIME_GRIB)
!
 CALL READ_GRIB_LAND_MASK(HFILE,KLUOUT,YINMODEL,ZMASK)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading of the physical field for urban areas
!              ---------------------------------------------
!
IF (HSURF(7:8)=='RO') THEN
  ! 
  SELECT CASE(HSURF(1:3))
    CASE('DEP')
      ALLOCATE(PFIELD(NNI,KLAYER,1))
    CASE('ALB','WWW')
      ALLOCATE(PFIELD(NNI,1,1))
    CASE('HEA','RHO')
      ALLOCATE(PFIELD(NNI,NGRID_LEVEL,1))
  END SELECT
  !
  PFIELD(:,:,:) = 0.
!
!-------------------------------------------------------------------------------------
!
!*      3.     Reading of the physical field for vegetated areas
!              -------------------------------------------------
!
ELSE
!
  SELECT CASE(HSURF(1:3))
!
!*      3.1    Total snow content (kg/m2)
!
  CASE('WWW')
     CALL READ_GRIB_SNOW_VEG_AND_DEPTH(HFILE,KLUOUT,YINMODEL,ZMASK,PSNV=ZFIELD1D)
     !
     ALLOCATE(PFIELD(SIZE(ZFIELD1D),1,NVEGTYPE))
     DO JVEGTYPE=1,NVEGTYPE
       PFIELD(:,1,JVEGTYPE)=ZFIELD1D(:)
     END DO
     DEALLOCATE(ZFIELD1D)
!
!
!*      3.2    Total snow depth (m)
!
  CASE('DEP')
     CALL READ_GRIB_SNOW_VEG_AND_DEPTH(HFILE,KLUOUT,YINMODEL,ZMASK,PSNVD=ZFIELD1D)
     !
     ALLOCATE(PFIELD(SIZE(ZFIELD1D),KLAYER,NVEGTYPE))
     DO JVEGTYPE=1,NVEGTYPE
        CALL SNOW3LGRID(PFIELD(:,:,JVEGTYPE),ZFIELD1D(:))
     END DO
     DEALLOCATE(ZFIELD1D)
!
!
!*      3.3    Profile of heat in the snow
!
  CASE('HEA')
     !* read temperature
     CALL READ_GRIB_TS(HFILE,KLUOUT,YINMODEL,ZMASK,ZFIELD1D)
     WHERE (ZFIELD1D/=XUNDEF) ZFIELD1D(:) = MIN(ZFIELD1D,XTT)
     !* assumes no liquid water in the snow
     ALLOCATE(ZHEAT(SIZE(ZFIELD1D)))
     CALL READ_GRIB_SNOW_DEN(HFILE,KLUOUT,YINMODEL,ZMASK,ZRHO) 
     WHERE(ZFIELD1D(:)==XUNDEF)ZRHO(:)=XUNDEF
     !
     CALL SNOW_T_WLIQ_TO_HEAT(ZHEAT,ZRHO,ZFIELD1D)
     !
     ALLOCATE(PFIELD(SIZE(ZFIELD1D),NGRID_LEVEL,NVEGTYPE))
     DO JVEGTYPE=1,NVEGTYPE
       DO JLAYER=1,NGRID_LEVEL
         PFIELD(:,JLAYER,JVEGTYPE)=ZHEAT(:)
       END DO
     END DO
     DEALLOCATE(ZFIELD1D)
     DEALLOCATE(ZHEAT   )
     DEALLOCATE(ZRHO    )
!
!*      3.4    Albedo
!
  CASE('ALB')    
    CALL READ_GRIB_SNOW_ALB(HFILE,KLUOUT,YINMODEL,ZMASK,ZFIELD1D)      
    ALLOCATE(PFIELD(SIZE(ZFIELD1D),1,NVEGTYPE))
    DO JVEGTYPE=1,NVEGTYPE
      PFIELD(:,1,JVEGTYPE)=ZFIELD1D(:)
    END DO
    DEALLOCATE(ZFIELD1D)
!
!*      3.5    Density
!
  CASE('RHO')    
    CALL READ_GRIB_SNOW_DEN(HFILE,KLUOUT,YINMODEL,ZMASK,ZFIELD1D)      
    ALLOCATE(PFIELD(SIZE(ZFIELD1D),1,NVEGTYPE))
    DO JVEGTYPE=1,NVEGTYPE
      PFIELD(:,1,JVEGTYPE)=ZFIELD1D(:)
    END DO
    DEALLOCATE(ZFIELD1D)
!
!*      3.6    SG1: initial grain is partially rounded
!
  CASE('SG1')
    ALLOCATE(PFIELD(NNI,NGRID_LEVEL,NVEGTYPE))
    PFIELD = -20
!
!*      3.7    SG2: initial grain is partially rounded
!
  CASE('SG2')
    ALLOCATE(PFIELD(NNI,NGRID_LEVEL,NVEGTYPE))
    PFIELD = 80
!
!*      3.8    AGE: snow is 3-days old
!
  CASE('AGE')
    ALLOCATE(PFIELD(NNI,NGRID_LEVEL,NVEGTYPE))
    PFIELD = 3
!
!*      3.9    HIS: 0 by default
!
  CASE('HIS')
    ALLOCATE(PFIELD(NNI,NGRID_LEVEL,NVEGTYPE))
    PFIELD = 0
!
  END SELECT
!
END IF
!
DEALLOCATE(ZMASK)
!
!-------------------------------------------------------------------------------------
!
!*      4.     Interpolation method
!              --------------------
!
 CINTERP_TYPE='HORIBL'
!
IF (LHOOK) CALL DR_HOOK('PREP_SNOW_GRIB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_SNOW_GRIB
