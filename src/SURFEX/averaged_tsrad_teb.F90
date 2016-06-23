!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGED_TSRAD_TEB(PEMIS_ROOF, PTS_ROOF,           &
                                    PEMIS_ROAD, PTS_ROAD,           &
                                    PEMIS_WALL, PTS_WALL_A,         &
                                    PTS_WALL_B,                     &
                                    PEMIS_GARDEN, PTS_GARDEN,       &
                                    PEMIS_GREENROOF, PTS_GREENROOF, &
                                    TSNOW_ROOF, TSNOW_ROAD,         &
                                    PROAD, PFRAC_GR, PGARDEN, PBLD, &
                                    PWALL_O_HOR, PSVF_ROAD,         &
                                    PSVF_WALL, PSVF_GARDEN,         &
                                    PEMIS, PTSRAD, PT_WIN1,         &
                                    PGR                             ) 
!     ###################################################
!
!!**** *AVERAGED_TSRAD_TEB* computes averaged emissivity and radiative surface
!!                          temperature for TEB scheme
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!    09/2012     C. de Munck, A. Lemonsu : add green roofs
!!
!!    Original    01/2004
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_TYPE_SNOW
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_CSTS,     ONLY : XSTEFAN
!
USE MODI_URBAN_LW_COEF
!
USE MODE_SURF_SNOW_FRAC
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
REAL, DIMENSION(:), INTENT(IN) :: PEMIS_ROOF     ! roof emissivity
REAL, DIMENSION(:), INTENT(IN) :: PTS_ROOF       ! roof surface temperature
REAL, DIMENSION(:), INTENT(IN) :: PEMIS_ROAD     ! road emissivity
REAL, DIMENSION(:), INTENT(IN) :: PTS_ROAD       ! road surface temperature
REAL, DIMENSION(:), INTENT(IN) :: PEMIS_WALL     ! wall emissivity
REAL, DIMENSION(:), INTENT(IN) :: PTS_WALL_A     ! wall surface temperature
REAL, DIMENSION(:), INTENT(IN) :: PTS_WALL_B     ! wall surface temperature
REAL, DIMENSION(:), INTENT(IN) :: PEMIS_GARDEN   ! green area emissivity (snowfree)
REAL, DIMENSION(:), INTENT(IN) :: PTS_GREENROOF  ! green roof surf. temp.
REAL, DIMENSION(:), INTENT(IN) :: PEMIS_GREENROOF! green roof emissivity (snowfree)
REAL, DIMENSION(:), INTENT(IN) :: PTS_GARDEN     ! green area surf. temp.
TYPE(SURF_SNOW),    INTENT(IN) :: TSNOW_ROOF     ! snow on roofs
TYPE(SURF_SNOW),    INTENT(IN) :: TSNOW_ROAD     ! snow on roads
REAL, DIMENSION(:), INTENT(IN) :: PROAD          ! road fraction
REAL, DIMENSION(:), INTENT(IN) :: PFRAC_GR       ! green roof fraction
REAL, DIMENSION(:), INTENT(IN) :: PGARDEN        ! green area fraction
REAL, DIMENSION(:), INTENT(IN) :: PBLD           ! building fraction
REAL, DIMENSION(:), INTENT(IN) :: PWALL_O_HOR    ! vertical surf. / horizontal surf.
REAL, DIMENSION(:), INTENT(IN) :: PSVF_ROAD      ! sky-view-factor from roads
REAL, DIMENSION(:), INTENT(IN) :: PSVF_WALL      ! sky-view-factor from walls
REAL, DIMENSION(:), INTENT(IN) :: PSVF_GARDEN    ! sky-view-factor from green areas
REAL, DIMENSION(:), INTENT(OUT):: PEMIS          ! averaged emissivity (all tiles)
REAL, DIMENSION(:), INTENT(OUT):: PTSRAD         ! averaged radiaitve temp. (all tiles)
REAL, DIMENSION(:), INTENT(IN) :: PT_WIN1        !
REAL, DIMENSION(:), INTENT(IN) :: PGR            !
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(SIZE(PEMIS_ROOF)) :: ZDN_ROOF       ! snow fraction 
REAL, DIMENSION(SIZE(PEMIS_ROOF)) :: ZDN_ROAD       ! on the surface
REAL, DIMENSION(SIZE(PBLD)) :: ZDF_ROOF       ! free-snow fraction 
REAL, DIMENSION(SIZE(PBLD)) :: ZDF_ROAD       ! on the surface
LOGICAL, DIMENSION(SIZE(PBLD)) :: GMASK       ! .false. (= no snow precip.)
!
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_WA_TO_WB   ! longwave exchange coefficients
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_WA_TO_R
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_WB_TO_R
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_WA_TO_NR
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_WB_TO_NR
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_WA_TO_G
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_WB_TO_G
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_WA_TO_WIN
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_WB_TO_WIN
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_R_TO_WA
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_R_TO_WB
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_R_TO_WIN
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_G_TO_WA
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_G_TO_WB
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_G_TO_WIN
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_S_TO_WA
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_S_TO_WB
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_S_TO_R
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_S_TO_NR
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_S_TO_G
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_S_TO_WIN
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_WIN_TO_WA
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_WIN_TO_WB
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_WIN_TO_R
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_WIN_TO_NR
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_WIN_TO_G
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_NR_TO_WA
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_NR_TO_WB
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_NR_TO_WIN
!
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_RAD          ! incoming LW to mimic
!                                               ! radiation behaviour of town
REAL, DIMENSION(SIZE(PBLD)) :: ZABS_LW_WALL     ! longwave absorbed by walls
REAL, DIMENSION(SIZE(PBLD)) :: ZABS_LW_ROAD     ! longwave absorbed by roads
REAL, DIMENSION(SIZE(PBLD)) :: ZABS_LW_ROOF     ! longwave absorbed by roofs
REAL, DIMENSION(SIZE(PBLD)) :: ZABS_LW_SNOW_ROAD! longwave absorbed by snow
REAL, DIMENSION(SIZE(PBLD)) :: ZABS_LW_SNOW_ROOF! on roads and roofs
REAL, DIMENSION(SIZE(PBLD)) :: ZABS_LW_GARDEN   ! longwave absorbed by gardens
REAL, DIMENSION(SIZE(PBLD)) :: ZABS_LW_GREENROOF! longwave absorbed by green roofs
REAL, DIMENSION(SIZE(PBLD)) :: ZLW_UP           ! outgoing longwave
!
REAL, DIMENSION(SIZE(PBLD)) :: ZT_SKY
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* snow fractions
!  --------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGED_TSRAD_TEB',0,ZHOOK_HANDLE)
GMASK(:) = .FALSE.
 CALL SNOW_FRAC_ROAD(TSNOW_ROAD%WSNOW(:,1,1),GMASK,ZDN_ROAD,ZDF_ROAD)
 CALL SNOW_FRAC_ROOF(TSNOW_ROOF%WSNOW(:,1,1),GMASK,ZDN_ROOF,ZDF_ROOF)
!
! fixed incoming LW (W/m2)
ZLW_RAD(:)= XSTEFAN * (PTS_ROAD(:) ** 4)
!
! LW absorbed by roofs
ZABS_LW_ROOF(:) = PEMIS_ROOF(:) * (ZLW_RAD(:) - XSTEFAN * PTS_ROOF(:)**4)
!
!* LW absorbed by snow on roof
ZABS_LW_SNOW_ROOF(:) = TSNOW_ROOF%EMIS(:,1) * (ZLW_RAD(:) - XSTEFAN * TSNOW_ROOF%TS(:,1)**4)
!
!* town averaged emissivity
PEMIS(:) = PBLD(:) * (1.-PFRAC_GR(:)) * (ZDF_ROOF(:)*PEMIS_ROOF     (:)    &
                                       + ZDN_ROOF(:)*TSNOW_ROOF%EMIS(:,1)) &
         + PBLD(:) *     PFRAC_GR(:)  *              PEMIS_GREENROOF(:)

!
!* long-wave trapping coefficients
!  -------------------------------
!
   ZT_SKY(:) = (ZLW_RAD(:)/XSTEFAN)**0.25
   !
   CALL URBAN_LW_COEF(PGR, PBLD, ZLW_RAD,                                &
                      PEMIS_ROAD, PSVF_ROAD, PEMIS_WALL, PSVF_WALL,      &
                      PEMIS_GARDEN, PROAD, PGARDEN,                      &
                      TSNOW_ROAD%EMIS(:,1),                              &
                      PTS_ROAD, PTS_WALL_A, PTS_WALL_B,                  &
                      PTS_ROAD, PTS_GARDEN, PT_WIN1,                     &  
                      ZLW_WA_TO_WB, ZLW_WA_TO_R, ZLW_WB_TO_R,            &
                      ZLW_WA_TO_NR,ZLW_WB_TO_NR,                         &
                      ZLW_WA_TO_G, ZLW_WB_TO_G,                          &
                      ZLW_WA_TO_WIN, ZLW_WB_TO_WIN,                      &
                      ZLW_R_TO_WA, ZLW_R_TO_WB, ZLW_R_TO_WIN,            &
                      ZLW_G_TO_WA, ZLW_G_TO_WB, ZLW_G_TO_WIN,            &
                      ZLW_S_TO_WA, ZLW_S_TO_WB, ZLW_S_TO_R,              &
                      ZLW_S_TO_NR, ZLW_S_TO_G, ZLW_S_TO_WIN,             &
                      ZLW_WIN_TO_WA, ZLW_WIN_TO_WB, ZLW_WIN_TO_R,        &
                      ZLW_WIN_TO_NR, ZLW_WIN_TO_G,                       &
                      ZLW_NR_TO_WA, ZLW_NR_TO_WB, ZLW_NR_TO_WIN          )
   !
   !
   !* town averaged emissivity
   !  ------------------------
   !
   PEMIS(:) =  PEMIS(:)                                                    &
              + PROAD(:)*PSVF_ROAD(:)* (ZDF_ROAD(:)* PEMIS_ROAD(:)         &
                                      + ZDN_ROAD(:)* TSNOW_ROAD%EMIS(:,1)) &
              + PWALL_O_HOR(:)       * PSVF_WALL(:)   * PEMIS_WALL(:)      &
              + PGARDEN(:)           * PSVF_GARDEN(:) * PEMIS_GARDEN(:)
   
   !
   ! LW absorbed by roads
   ZABS_LW_ROAD(:) =  ZLW_S_TO_R  (:) * (ZT_SKY    (:) - PTS_ROAD(:)) &
                    + ZLW_WA_TO_R (:) * (PTS_WALL_A(:) - PTS_ROAD(:)) &
                    + ZLW_WB_TO_R (:) * (PTS_WALL_B(:) - PTS_ROAD(:)) &
                    + ZLW_WIN_TO_R(:) * (PT_WIN1   (:) - PTS_ROAD(:))
   
   !
   ! LW absorbed by walls
   ZABS_LW_WALL(:) =( ZLW_S_TO_WA(:)  * (ZT_SKY(:)    - PTS_WALL_A(:)) &
                    + ZLW_R_TO_WA(:)  * (PTS_ROAD(:)  - PTS_WALL_A(:)) &
                    + ZLW_G_TO_WA(:)  * (PTS_GARDEN(:)- PTS_WALL_A(:)) &
                    + ZLW_WIN_TO_WA(:)* (PT_WIN1(:)   - PTS_WALL_A(:)) &
                    + ZLW_S_TO_WB(:)  * (ZT_SKY(:)    - PTS_WALL_B(:)) &
                    + ZLW_R_TO_WB(:)  * (PTS_ROAD(:)  - PTS_WALL_B(:)) &
                    + ZLW_G_TO_WB(:)  * (PTS_GARDEN(:)- PTS_WALL_B(:)) &
                    + ZLW_WIN_TO_WB(:)* (PT_WIN1(:)   - PTS_WALL_B(:)))&
                   * 0.5
   
   !
   !* LW absorbed by snow on road
   ZABS_LW_SNOW_ROAD(:) =  ZLW_S_TO_R   (:) * (ZT_SKY(:)     - TSNOW_ROAD%TS(:,1)) &
                         + ZLW_WA_TO_NR (:) * (PTS_WALL_A(:) - TSNOW_ROAD%TS(:,1)) &
                         + ZLW_WB_TO_NR (:) * (PTS_WALL_B(:) - TSNOW_ROAD%TS(:,1)) &
                         + ZLW_WIN_TO_NR(:) * (PT_WIN1(:)    - TSNOW_ROAD%TS(:,1))
   !
   !* LW absorbed by gardens
   ZABS_LW_GARDEN(:) =  ZLW_S_TO_G  (:)*(ZT_SKY    (:)-PTS_GARDEN(:)) &
                      + ZLW_WA_TO_G (:)*(PTS_WALL_A(:)-PTS_GARDEN(:)) &
                      + ZLW_WB_TO_G (:)*(PTS_WALL_B(:)-PTS_GARDEN(:)) &
                      + ZLW_WIN_TO_G(:)*(PT_WIN1   (:)-PTS_GARDEN(:))
   !
   !* LW absorbed by green roofs
ZABS_LW_GREENROOF(:) = PEMIS_GREENROOF(:) * (ZLW_RAD(:) - XSTEFAN * PTS_GREENROOF(:)** 4)
   
!
!* outgoing longwave radiation
ZLW_UP(:) = ZLW_RAD(:)                                                     &
          - ( PBLD(:) *(1.-PFRAC_GR(:))*ZDF_ROOF(:)*ZABS_LW_ROOF     (:)   &
             +PBLD(:) *(1.-PFRAC_GR(:))*ZDN_ROOF(:)*ZABS_LW_SNOW_ROOF(:)   &
             +PBLD(:) *    PFRAC_GR(:)             *ZABS_LW_GREENROOF(:)   &
             +PROAD(:)                 *ZDF_ROAD(:)*ZABS_LW_ROAD     (:)   &
             +PROAD(:)                 *ZDN_ROAD(:)*ZABS_LW_SNOW_ROAD(:)   &
             +PWALL_O_HOR(:)                       *ZABS_LW_WALL     (:)   &
             +PGARDEN(:)                           *ZABS_LW_GARDEN   (:))
!
!* town radiative surface temperature
PTSRAD(:)   = ((ZLW_UP(:) - ZLW_RAD(:)*(1.-PEMIS(:))) /PEMIS(:)/XSTEFAN)**0.25
!
IF (LHOOK) CALL DR_HOOK('AVERAGED_TSRAD_TEB',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGED_TSRAD_TEB
