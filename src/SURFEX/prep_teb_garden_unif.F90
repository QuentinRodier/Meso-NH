!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB_GARDEN_UNIF(KLUOUT,HISBA,PTEB_SOILGRID,HSURF,PFIELD)
!     #################################################################################
!
!!****  *PREP_TEB_GARDEN_UNIF* - prepares ISBA field from prescribed values
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
!!      C. de Munck 01/2020 PFIELD transformed to 2D pointer for linear interpolation
!!------------------------------------------------------------------
!
!
USE MODD_PREP,           ONLY : CINTERP_TYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PREP_TEB_GARDEN,ONLY : XHUG_SURF_GD, XHUG_ROOT_GD, XHUG_DEEP_GD,       &
                                  XHUGI_SURF_GD, XHUGI_ROOT_GD, XHUGI_DEEP_GD,  &
                                  XTG_SURF_GD, XTG_ROOT_GD, XTG_DEEP_GD,        &
                                  XWR_DEF  
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,            INTENT(IN)  :: KLUOUT    ! output listing logical unit
 CHARACTER(LEN=3),   INTENT(IN)  :: HISBA     
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
REAL, DIMENSION(:,:), POINTER   :: PFIELD    ! field to interpolate horizontally
REAL,          DIMENSION(:)     :: PTEB_SOILGRID    
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GARDEN_UNIF',0,ZHOOK_HANDLE)
SELECT CASE(HSURF)
!
!*      3.0    Orography
!
  CASE('ZS     ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = 0.
   
!
!*      3.1    Profile of soil relative humidity
!
  CASE('WG     ')
    IF (HISBA=='DIF') THEN 
      ALLOCATE(PFIELD(1,SIZE(PTEB_SOILGRID)))
      CALL PUT_UNIF_ON_REF_GRID('WG     ',PTEB_SOILGRID)
    ELSE  
      ALLOCATE(PFIELD(1,3))
      PFIELD(:,1) = XHUG_SURF_GD
      PFIELD(:,2) = XHUG_ROOT_GD
      PFIELD(:,3) = XHUG_DEEP_GD
    ENDIF

!*      3.2    Profile of soil humidity for ice

  CASE('WGI    ')
    IF (HISBA=='DIF') THEN 
      ALLOCATE(PFIELD(1,SIZE(PTEB_SOILGRID)))
      CALL PUT_UNIF_ON_REF_GRID('WGI    ',PTEB_SOILGRID)
    ELSE  
      ALLOCATE(PFIELD(1,3))
      PFIELD(:,1) = XHUGI_SURF_GD
      PFIELD(:,2) = XHUGI_ROOT_GD
      PFIELD(:,3) = XHUGI_DEEP_GD
    ENDIF

!*      3.3    Profile of temperatures

  CASE('TG     ')
    IF (HISBA=='DIF') THEN 
      ALLOCATE(PFIELD(1,SIZE(PTEB_SOILGRID)))
      CALL PUT_UNIF_ON_REF_GRID('TG     ',PTEB_SOILGRID)
    ELSE  
      ALLOCATE(PFIELD(1,3))
      PFIELD(:,1) = XTG_SURF_GD
      PFIELD(:,2) = XTG_ROOT_GD
      PFIELD(:,3) = XTG_DEEP_GD
    ENDIF

!*      3.4    Other quantities

  CASE('WR     ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XWR_DEF

  CASE('LAI    ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XUNDEF

END SELECT
!
!*      4.     Interpolation method
!              --------------------
!
CINTERP_TYPE='UNIF  '
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GARDEN_UNIF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
CONTAINS
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
SUBROUTINE PUT_UNIF_ON_REF_GRID(HSURFTYPE,PGRID)
!-------------------------------------------------------------------------------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODI_INTERP_GRID
!
 CHARACTER(LEN=7),   INTENT(IN) :: HSURFTYPE ! surface type
REAL, DIMENSION(:), INTENT(IN) :: PGRID     ! reference grid
!
REAL               :: ZTS! surface temperature
REAL               :: ZTI! internal temperature
REAL, DIMENSION(1,2) :: ZT ! temperature profile
REAL, DIMENSION(1,2) :: ZD ! normalized depth profile
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------

!* get surface temperature

IF (LHOOK) CALL DR_HOOK('PUT_UNIF_ON_REF_GRID',0,ZHOOK_HANDLE)
SELECT CASE(HSURFTYPE)
  !
  CASE('TG     ')
    ZTS = XTG_SURF_GD
  CASE('WG     ')
    ZTS = XHUG_SURF_GD
  CASE('WGI    ')
    ZTS = XHUGI_SURF_GD
    !
END SELECT
!
!* get deep road or building interior temperature
!
SELECT CASE(HSURFTYPE)
  !
  CASE('TG     ')
    ZTI = XTG_DEEP_GD
  CASE('WG     ')
    ZTI = XHUG_DEEP_GD
  CASE('WGI    ')
    ZTI = XHUGI_SURF_GD
  !
END SELECT

!* group all this information in one profile

ZT(1,1) = ZTS
ZT(1,2) = ZTI

ZD(1,1) = 0.
ZD(1,2) = 1.
!
!* interpolate this field on the required grid
!
 CALL INTERP_GRID(ZD,ZT,PGRID,PFIELD)
!
IF (LHOOK) CALL DR_HOOK('PUT_UNIF_ON_REF_GRID',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PUT_UNIF_ON_REF_GRID
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_TEB_GARDEN_UNIF
