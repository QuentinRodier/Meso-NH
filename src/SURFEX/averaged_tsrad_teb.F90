!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGED_TSRAD_TEB(T, B, PEMIS_GARDEN, PTS_GARDEN, PEMIS_GREENROOF, &
                                    PTS_GREENROOF, PEMIS_HVEG, PTS_HVEG, PEMIS, PTSRAD )
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
!!    10/2016     P. Marguinaud : Port to single precision
!!
!!    Original    01/2004
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_LW_COEF, ONLY : LW_COEF_t
!
USE MODD_TYPE_SNOW
!
USE MODD_TEB_PAR,  ONLY : XEMIS_WIN_CST
USE MODD_ISBA_PAR, ONLY : XEMISVEG
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_CSTS,     ONLY : XSTEFAN
!
USE MODI_URBAN_LW_COEF
USE MODI_ALLOC_LW_COEF
USE MODI_DEALLOC_LW_COEF
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
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(BEM_t), INTENT(INOUT) :: B
!
REAL, DIMENSION(:), INTENT(IN) :: PEMIS_GARDEN   ! green area emissivity (snowfree)
REAL, DIMENSION(:), INTENT(IN) :: PTS_GARDEN     ! green area surf. temp.
REAL, DIMENSION(:), INTENT(IN) :: PEMIS_GREENROOF! green roof emissivity (snowfree)
REAL, DIMENSION(:), INTENT(IN) :: PTS_GREENROOF  ! green roof surf. temp.
REAL, DIMENSION(:), INTENT(IN) :: PEMIS_HVEG     ! high vegetation emissivity
REAL, DIMENSION(:), INTENT(IN) :: PTS_HVEG       ! high vegetation surf. temp.
REAL, DIMENSION(:), INTENT(OUT):: PEMIS          ! averaged emissivity (all tiles)
REAL, DIMENSION(:), INTENT(OUT):: PTSRAD         ! averaged radiaitve temp. (all tiles)
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
TYPE(LW_COEF_t) :: LW
!
REAL, DIMENSION(SIZE(T%XEMIS_ROOF)) :: ZDN_ROOF       ! snow fraction 
REAL, DIMENSION(SIZE(T%XEMIS_ROOF)) :: ZDN_ROAD       ! on the surface
REAL, DIMENSION(SIZE(T%XBLD)) :: ZDF_ROOF       ! free-snow fraction 
REAL, DIMENSION(SIZE(T%XBLD)) :: ZDF_ROAD       ! on the surface
LOGICAL, DIMENSION(SIZE(T%XBLD)) :: GMASK       ! .false. (= no snow precip.)
!
REAL, DIMENSION(SIZE(T%XBLD)) :: ZEMIS_WIN      ! windows emissivity
REAL, DIMENSION(SIZE(T%XBLD)) :: ZEMIS_HVEG     ! high vegetation emissivity
!
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_RAD          ! incoming LW to mimic
!                                               ! radiation behaviour of town
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_WALL     ! longwave absorbed by walls
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_WIN      ! longwave absorbed by windows
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_ROAD     ! longwave absorbed by roads
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_ROOF     ! longwave absorbed by roofs
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_SNOW_ROAD! longwave absorbed by snow
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_SNOW_ROOF! on roads and roofs
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_GARDEN   ! longwave absorbed by gardens
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_GREENROOF! longwave absorbed by green roofs
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_HVEG     ! longwave absorbed by high vegetation
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_SKY      ! longwave outgoing (=absorbed by sky)
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_UP           ! outgoing longwave
!
REAL, DIMENSION(SIZE(T%XBLD)) :: ZT_SKY            ! Sky temperature
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
CALL ALLOC_LW_COEF(LW,SIZE(T%XBLD))
!
!* emissivities
!  ------------
!
ZEMIS_HVEG = XEMISVEG
ZEMIS_WIN  = XEMIS_WIN_CST
!
!* snow fractions
!  --------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGED_TSRAD_TEB',0,ZHOOK_HANDLE)
GMASK(:) = .FALSE.
 CALL SNOW_FRAC_ROAD(T%TSNOW_ROAD%WSNOW(:,1),GMASK,ZDN_ROAD,ZDF_ROAD)
 CALL SNOW_FRAC_ROOF(T%TSNOW_ROOF%WSNOW(:,1),GMASK,ZDN_ROOF,ZDF_ROOF)
!
! fixed incoming LW (W/m2)
ZLW_RAD(:)= XSTEFAN * (T%XT_ROAD(:,1) ** 4)
!
! LW absorbed by roofs
ZABS_LW_ROOF(:) = T%XEMIS_ROOF(:) * (ZLW_RAD(:) - XSTEFAN * T%XT_ROOF(:,1)**4)
!
!* LW absorbed by snow on roof
WHERE (T%TSNOW_ROOF%TS(:) /= XUNDEF .AND. T%TSNOW_ROOF%EMIS(:) /= XUNDEF)
  ZABS_LW_SNOW_ROOF(:) = T%TSNOW_ROOF%EMIS(:) * (ZLW_RAD(:) - XSTEFAN * T%TSNOW_ROOF%TS(:)**4)
ELSEWHERE
  ZABS_LW_SNOW_ROOF(:) = 0.
ENDWHERE
!
!* town averaged emissivity (roof part)
PEMIS(:) = T%XBLD(:) * (1.-T%XGREENROOF(:)) * (ZDF_ROOF(:)*T%XEMIS_ROOF     (:)    &
                                       + ZDN_ROOF(:)*T%TSNOW_ROOF%EMIS(:)) &
         + T%XBLD(:) *     T%XGREENROOF(:)  *              PEMIS_GREENROOF(:)

!
!* long-wave trapping coefficients
!  -------------------------------
!
   ZT_SKY(:) = (ZLW_RAD(:)/XSTEFAN)**0.25
   !
   CALL URBAN_LW_COEF( B, T, LW, ZLW_RAD, PEMIS_GARDEN, T%XT_ROAD(:,1), PTS_GARDEN,&
                             PEMIS_HVEG, PTS_HVEG                                  )
   !
   !
   !* town averaged emissivity (adds canyon)
   !  ------------------------
   !
   PEMIS(:) =  PEMIS(:)                                                                &
              + T%XROAD(:)       * T%XSVF_RS(:) * (ZDF_ROAD(:) * T%XEMIS_ROAD(:)       &
                                                 + ZDN_ROAD(:) * T%TSNOW_ROAD%EMIS(:)) &
              + T%XWALL_O_HOR(:) * T%XSVF_WS(:) * (1. - B%XGR(:)) * T%XEMIS_WALL(:)    &
              + T%XWALL_O_HOR(:) * T%XSVF_WS(:) *       B%XGR(:)  * ZEMIS_WIN(:)       &
              + T%XGARDEN(:)     * T%XSVF_RS(:)                   * PEMIS_GARDEN(:)    &
              + T%XURBTREE  (:)  * (1. - T%XBLD(:))               * ZEMIS_HVEG(:)    
   !
   ! LW absorbed by roads
   ZABS_LW_ROAD(:) =  LW%XLW_S_TO_R  (:) * (ZT_SKY       (:) - T%XT_ROAD(:,1)) &
                    + LW%XLW_WA_TO_R (:) * (T%XT_WALL_A(:,1) - T%XT_ROAD(:,1)) &
                    + LW%XLW_WB_TO_R (:) * (T%XT_WALL_B(:,1) - T%XT_ROAD(:,1)) &
                    + LW%XLW_WIN_TO_R(:) * (B%XT_WIN1    (:) - T%XT_ROAD(:,1)) &
                    + LW%XLW_HV_TO_R (:) * (PTS_HVEG     (:) - T%XT_ROAD(:,1)) 
   
   !
   ! LW absorbed by walls
   ZABS_LW_WALL(:) =( LW%XLW_S_TO_WA  (:) * (ZT_SKY         (:)   - T%XT_WALL_A(:,1)) &
                    + LW%XLW_R_TO_WA  (:) * (T%XT_ROAD      (:,1) - T%XT_WALL_A(:,1)) * ZDF_ROAD(:) &
                    + LW%XLW_NR_TO_WA (:) * (T%TSNOW_ROAD%TS(:)   - T%XT_WALL_A(:,1)) * ZDN_ROAD(:) &
                    + LW%XLW_G_TO_WA  (:) * (PTS_GARDEN     (:)   - T%XT_WALL_A(:,1)) &
                    + LW%XLW_HV_TO_WA (:) * (PTS_HVEG       (:)   - T%XT_WALL_A(:,1)) &
                    + LW%XLW_WIN_TO_WA(:) * (B%XT_WIN1      (:)   - T%XT_WALL_A(:,1)) &
                    + LW%XLW_S_TO_WB  (:) * (ZT_SKY         (:)   - T%XT_WALL_B(:,1)) &
                    + LW%XLW_R_TO_WB  (:) * (T%XT_ROAD      (:,1) - T%XT_WALL_B(:,1)) * ZDF_ROAD(:) &
                    + LW%XLW_NR_TO_WB (:) * (T%TSNOW_ROAD%TS(:)   - T%XT_WALL_B(:,1)) * ZDN_ROAD(:) &
                    + LW%XLW_G_TO_WB  (:) * (PTS_GARDEN     (:)   - T%XT_WALL_B(:,1)) &
                    + LW%XLW_HV_TO_WB (:) * (PTS_HVEG       (:)   - T%XT_WALL_B(:,1)) &
                    + LW%XLW_WIN_TO_WB(:) * (B%XT_WIN1      (:)   - T%XT_WALL_B(:,1)))&
                   * 0.5
   
   !
   !* LW absorbed by windows
   ZABS_LW_WIN(:) =   LW%XLW_S_TO_WIN (:) * (ZT_SKY         (:)   - B%XT_WIN1(:)) &
                    + LW%XLW_R_TO_WIN (:) * (T%XT_ROAD      (:,1) - B%XT_WIN1(:)) * ZDF_ROAD(:) &
                    + LW%XLW_NR_TO_WIN(:) * (T%TSNOW_ROAD%TS(:)   - B%XT_WIN1(:)) * ZDN_ROAD(:) &
                    + LW%XLW_G_TO_WIN (:) * (PTS_GARDEN     (:)   - B%XT_WIN1(:)) &
                    + LW%XLW_HV_TO_WIN(:) * (PTS_HVEG       (:)   - B%XT_WIN1(:)) &
                    + LW%XLW_WA_TO_WIN(:) * (T%XT_WALL_A    (:,1) - B%XT_WIN1(:)) &
                    + LW%XLW_WB_TO_WIN(:) * (T%XT_WALL_B    (:,1) - B%XT_WIN1(:))
   !
   !* LW absorbed by snow on road
   ZABS_LW_SNOW_ROAD(:) =  LW%XLW_S_TO_R   (:) * (ZT_SKY(:)        - T%TSNOW_ROAD%TS(:)) &
                         + LW%XLW_WA_TO_NR (:) * (T%XT_WALL_A(:,1) - T%TSNOW_ROAD%TS(:)) &
                         + LW%XLW_WB_TO_NR (:) * (T%XT_WALL_B(:,1) - T%TSNOW_ROAD%TS(:)) &
                         + LW%XLW_WIN_TO_NR(:) * (B%XT_WIN1(:)     - T%TSNOW_ROAD%TS(:)) &
                         + LW%XLW_HV_TO_NR (:) * (PTS_HVEG  (:)    - T%TSNOW_ROAD%TS(:)) 
   !
   !* LW absorbed by gardens
   WHERE (PTS_GARDEN (:) /= XUNDEF)
     ZABS_LW_GARDEN(:) =  LW%XLW_S_TO_G  (:)*(ZT_SKY       (:)-PTS_GARDEN(:)) &
                        + LW%XLW_WA_TO_G (:)*(T%XT_WALL_A(:,1)-PTS_GARDEN(:)) &
                        + LW%XLW_WB_TO_G (:)*(T%XT_WALL_B(:,1)-PTS_GARDEN(:)) &
                        + LW%XLW_WIN_TO_G(:)*(B%XT_WIN1    (:)-PTS_GARDEN(:)) &
                        + LW%XLW_HV_TO_G (:)*(PTS_HVEG     (:)-PTS_GARDEN(:))
   ELSEWHERE
     ZABS_LW_GARDEN(:) = 0.
   ENDWHERE
   !
   !* LW absorbed by the high vegetation
   WHERE (PTS_HVEG (:) /= XUNDEF)
     ZABS_LW_HVEG(:) =  LW%XLW_S_TO_HV  (:) * (ZT_SKY         (:)   - PTS_HVEG(:)) &
                      + LW%XLW_WA_TO_HV (:) * (T%XT_WALL_A    (:,1) - PTS_HVEG(:)) &
                      + LW%XLW_WB_TO_HV (:) * (T%XT_WALL_B    (:,1) - PTS_HVEG(:)) &
                      + LW%XLW_WIN_TO_HV(:) * (B%XT_WIN1      (:)   - PTS_HVEG(:)) &
                      + LW%XLW_R_TO_HV  (:) * (T%XT_ROAD      (:,1) - PTS_HVEG(:)) * ZDF_ROAD(:) &
                      + LW%XLW_NR_TO_HV (:) * (T%TSNOW_ROAD%TS(:)   - PTS_HVEG(:)) * ZDN_ROAD(:) &
                      + LW%XLW_G_TO_HV  (:) * (PTS_GARDEN     (:)   - PTS_HVEG(:))
   ELSEWHERE
     ZABS_LW_HVEG(:) = 0.
   ENDWHERE 
   !
   !* LW absorbed by the sky
   ZABS_LW_SKY(:) =   LW%XLW_WA_TO_S (:) * (T%XT_WALL_A    (:,1) - ZT_SKY(:)) &
                    + LW%XLW_WB_TO_S (:) * (T%XT_WALL_B    (:,1) - ZT_SKY(:)) &
                    + LW%XLW_WIN_TO_S(:) * (B%XT_WIN1      (:)   - ZT_SKY(:)) &
                    + LW%XLW_R_TO_S  (:) * (T%XT_ROAD      (:,1) - ZT_SKY(:)) * ZDF_ROAD(:) &
                    + LW%XLW_NR_TO_S (:) * (T%TSNOW_ROAD%TS(:)   - ZT_SKY(:)) * ZDN_ROAD(:) &
                    + LW%XLW_G_TO_S  (:) * (PTS_GARDEN     (:)   - ZT_SKY(:)) &
                    + LW%XLW_HV_TO_S (:) * (PTS_HVEG       (:)   - ZT_SKY(:))
   !
   !* LW absorbed by green roofs
   WHERE (PTS_GREENROOF (:) /= XUNDEF)
     ZABS_LW_GREENROOF(:) = PEMIS_GREENROOF(:) * (ZLW_RAD(:) - XSTEFAN * PTS_GREENROOF(:)** 4)
   ELSEWHERE
     ZABS_LW_GREENROOF(:) = 0.
   ENDWHERE
   
!
!* outgoing longwave radiation
ZLW_UP(:) = ZLW_RAD(:)                                                           &
          - ( T%XBLD(:) *(1.-T%XGREENROOF(:))*ZDF_ROOF(:)*ZABS_LW_ROOF     (:)   &
             +T%XBLD(:) *(1.-T%XGREENROOF(:))*ZDN_ROOF(:)*ZABS_LW_SNOW_ROOF(:)   &
             +T%XBLD(:) *    T%XGREENROOF(:)             *ZABS_LW_GREENROOF(:)   &
             +T%XROAD(:)                 *ZDF_ROAD(:)*ZABS_LW_ROAD     (:)       &
             +T%XROAD(:)                 *ZDN_ROAD(:)*ZABS_LW_SNOW_ROAD(:)       &
             +T%XWALL_O_HOR(:) *(1. - B%XGR(:))        *ZABS_LW_WALL     (:)     &
             +T%XWALL_O_HOR(:) *      B%XGR(:)         *ZABS_LW_WIN      (:)     &
             +T%XGARDEN(:)                           *ZABS_LW_GARDEN   (:)       &
             +T%XURBTREE(:)    *(1. - T%XBLD(:))       *ZABS_LW_HVEG     (:)  )
!
!* town radiative surface temperature
PTSRAD(:)   = ((ZLW_UP(:) - ZLW_RAD(:)*(1.-PEMIS(:))) /PEMIS(:)/XSTEFAN)**0.25
!
CALL DEALLOC_LW_COEF(LW)
!
IF (LHOOK) CALL DR_HOOK('AVERAGED_TSRAD_TEB',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGED_TSRAD_TEB
