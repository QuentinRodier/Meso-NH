!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_VER_TEB
!     #################################################################################
!
!!****  *PREP_VER_TEB* - change in TEB variables due to altitude change
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
USE MODD_TEB_n,   ONLY : XZS, XT_CANYON, XQ_CANYON, XT_ROAD, XT_ROOF, XT_WALL_A, XT_WALL_B, &
                          TSNOW_ROOF, TSNOW_ROAD, XTI_ROAD, XD_WALL, XD_ROOF, CBEM
USE MODD_BEM_n,   ONLY : XT_FLOOR, XT_MASS, XD_FLOOR                          
USE MODD_PREP,   ONLY : XZS_LS, XT_CLIM_GRAD
USE MODD_CSTS,   ONLY : XRD, XG, XP00
!
USE MODE_THERMOS
USE MODI_PREP_VER_SNOW
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!*      0.2    declarations of local variables
!
INTEGER                         :: JL        ! loop counter
REAL, DIMENSION(:), ALLOCATABLE :: ZT0       ! estimated temperature at sea level
REAL, DIMENSION(:), ALLOCATABLE :: ZP_LS     ! estimated pressure at XZS_LS
REAL, DIMENSION(:), ALLOCATABLE :: ZT_LS     ! temperature at XZS_LS
REAL, DIMENSION(:), ALLOCATABLE :: ZP        ! estimated pressure at XZS
REAL, DIMENSION(:,:), ALLOCATABLE :: ZGRID   ! wall or roof grid
REAL, DIMENSION(:),   ALLOCATABLE :: ZD      ! wall or roof total thickness
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.1    Water reservoirs
!
!* nothing done
!
!*      1.2    Building temperature
!
!* nothing done
!
!*      1.3    Road deep temperature
!
IF (LHOOK) CALL DR_HOOK('PREP_VER_TEB',0,ZHOOK_HANDLE)
XTI_ROAD = XTI_ROAD  + XT_CLIM_GRAD  * (XZS - XZS_LS)
!
!*      1.4    Road Temperature profile
!
DO JL=1,SIZE(XT_ROAD,2)
  XT_ROAD(:,JL) = XT_ROAD(:,JL) + XT_CLIM_GRAD  * (XZS - XZS_LS)
END DO
!
!*      1.5    Wall Temperature profile
!
!* wall grid
ALLOCATE(ZD   (SIZE(XD_WALL,1)))
ALLOCATE(ZGRID(SIZE(XD_WALL,1),SIZE(XD_WALL,2)))
ZGRID(:,:) = 0.
ZD   (:)   = 0.
!
DO JL=1,SIZE(XD_WALL,2)
  ZGRID(:,JL) = ZD(:) + XD_WALL(:,JL)/2.
  ZD   (:)    = ZD(:) + XD_WALL(:,JL)
END DO
!
!* surface temperature shift is given by climatological gradient
!* shift of temperatures within the wall is attenuated
!* shift is zero from internal wall to half of wall
DO JL=1,SIZE(XT_WALL_A,2)
  XT_WALL_A(:,JL) = XT_WALL_A(:,JL) + XT_CLIM_GRAD  * (XZS - XZS_LS) &
                                     * MAX(1.-2.*ZGRID(:,JL)/ZD(:),0.)  
  XT_WALL_B(:,JL) = XT_WALL_B(:,JL) + XT_CLIM_GRAD  * (XZS - XZS_LS) &
                                     * MAX(1.-2.*ZGRID(:,JL)/ZD(:),0.)  
END DO
!
DEALLOCATE(ZD)
DEALLOCATE(ZGRID)
!
!*      1.6    Roof Temperature profile
!
!* roof grid
ALLOCATE(ZD   (SIZE(XD_ROOF,1)))
ALLOCATE(ZGRID(SIZE(XD_ROOF,1),SIZE(XD_ROOF,2)))
ZGRID(:,:) = 0.
ZD   (:)   = 0.
!
DO JL=1,SIZE(XD_ROOF,2)
  ZGRID(:,JL) = ZD(:) + XD_ROOF(:,JL)/2.
  ZD   (:)    = ZD(:) + XD_ROOF(:,JL)
END DO
!
!* surface temperature shift is given by climatological gradient
!* shift of temperatures within the wall is attenuated
!* shift is zero from internal wall to half of wall
DO JL=1,SIZE(XT_ROOF,2)
  XT_ROOF(:,JL) = XT_ROOF(:,JL) + XT_CLIM_GRAD  * (XZS - XZS_LS) &
                                   * MAX(1.-2.*ZGRID(:,JL)/ZD(:),0.)  
END DO
!
DEALLOCATE(ZD)
DEALLOCATE(ZGRID)
!
!
IF (CBEM=='BEM') THEN
  !
  !*      1.6bis Floor Temperature profile
  !
  !* Floor grid
  ALLOCATE(ZD   (SIZE(XD_FLOOR,1)))
  ALLOCATE(ZGRID(SIZE(XD_FLOOR,1),SIZE(XD_FLOOR,2)))
  ZGRID(:,:) = 0.
  ZD   (:)   = 0.
  !
  DO JL=1,SIZE(XD_FLOOR,2)
    ZGRID(:,JL) = ZD(:) + XD_FLOOR(:,JL)/2.
    ZD   (:)    = ZD(:) + XD_FLOOR(:,JL)
  END DO
  !
  !* deep ground temperature shift is given by climatological gradient
  !* shift of temperatures within the floor is attenuated
  !* shift is zero from internal floor layer to half of floor
  DO JL=1,SIZE(XT_FLOOR,2)
    XT_FLOOR(:,JL) = XT_FLOOR(:,JL) + XT_CLIM_GRAD  * (XZS - XZS_LS) &
                                   * MAX(2.*ZGRID(:,JL)/ZD(:)-1.,0.)
  END DO
  !
  DEALLOCATE(ZD)
  DEALLOCATE(ZGRID)
  !
  !*      1.6bis Mass Temperature profile
  !
  !* mass grid
  ALLOCATE(ZD   (SIZE(XD_FLOOR,1)))
  ALLOCATE(ZGRID(SIZE(XD_FLOOR,1),SIZE(XD_FLOOR,2)))
  ZGRID(:,:) = 0.
  ZD   (:)   = 0.
  !
  DO JL=1,SIZE(XD_FLOOR,2)
    ZGRID(:,JL) = ZD(:) + XD_FLOOR(:,JL)/2.
    ZD   (:)    = ZD(:) + XD_FLOOR(:,JL)
  END DO
  !
  !* deep ground temperature shift is given by climatological gradient
  !* shift of temperatures within the floor is attenuated
  !* shift is zero from internal floor layer to half of floor
  DO JL=1,SIZE(XT_MASS,2)
    XT_MASS(:,JL) = XT_MASS(:,JL) + XT_CLIM_GRAD  * (XZS - XZS_LS) &
                                   * MAX(2.*ZGRID(:,JL)/ZD(:)-1.,0.)
  END DO
  !
  DEALLOCATE(ZD)
  DEALLOCATE(ZGRID)
  !
ENDIF
!
!*      1.7    Snow variables
!
 CALL PREP_VER_SNOW(TSNOW_ROOF,XZS_LS,XZS)
 CALL PREP_VER_SNOW(TSNOW_ROAD,XZS_LS,XZS)
!
!
!*      1.8    Canyon air temperature
!
!* estimation of temperature at sea level
!
ALLOCATE(ZT0(SIZE(XQ_CANYON)))
ZT0 = XT_CANYON - XT_CLIM_GRAD * XZS_LS
!
!* shift of canyon air temperature
!
ALLOCATE(ZT_LS(SIZE(XQ_CANYON)))
ZT_LS = XT_CANYON
!
XT_CANYON = XT_CANYON  + XT_CLIM_GRAD  * (XZS - XZS_LS)
!
!*      1.9    Canyon air humidity
!
!
!
!* estimation of pressure at large-scale orography
!
ALLOCATE(ZP_LS(SIZE(XQ_CANYON)))
ZP_LS = XP00 * EXP(-(XG/XRD/ZT0)*XZS_LS+(XG*XT_CLIM_GRAD/(2.*XRD*ZT0))*XZS_LS**2)
!
!* estimation of pressure at output orography
!
ALLOCATE(ZP(SIZE(XQ_CANYON)))
ZP    = XP00 * EXP(-(XG/XRD/ZT0)*XZS   +(XG*XT_CLIM_GRAD/(2.*XRD*ZT0))*XZS   **2)
!
!* conservation of estimated relative humidity
!
XQ_CANYON = XQ_CANYON * QSAT(XT_CANYON,ZP) / QSAT(ZT_LS,ZP_LS)
!
DEALLOCATE(ZP_LS)
DEALLOCATE(ZP   )
DEALLOCATE(ZT0  )
DEALLOCATE(ZT_LS)
IF (LHOOK) CALL DR_HOOK('PREP_VER_TEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_VER_TEB
