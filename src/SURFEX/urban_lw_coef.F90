!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   ##########################################################################
    SUBROUTINE URBAN_LW_COEF(B, T, LW, PLW_RAD, PEMIS_G, PTS_SR, PTS_G,               &
                             PEMIS_HV, PTS_HV)
!   ##########################################################################
!
!!****  *URBAN_LW_COEF*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the coefficients before each of the temperatures in the
!     radiative budgets
!         
!     
!!**  METHOD
!     ------
!
!
!
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    MODD_CST
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/09/98 
!!      P. Marguinaud (Oct 2016) : Port to single precision
!!      V. Masson   04.2020 correct IR exchanges with High vegetation
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_LW_COEF, ONLY : LW_COEF_t
!
USE MODD_TEB_PAR, ONLY : XEMIS_WIN_CST
USE MODD_CSTS,ONLY : XSTEFAN
USE MODD_SURF_PAR,ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(LW_COEF_t), INTENT(INOUT) :: LW
!
REAL, DIMENSION(:), INTENT(IN)  :: PLW_RAD  ! incoming LW radiation
REAL, DIMENSION(:), INTENT(IN)  :: PEMIS_G  ! GARDEN area emissivity
REAL, DIMENSION(:), INTENT(IN)  :: PEMIS_HV ! high vegetation emissivity
!
REAL, DIMENSION(:), INTENT(IN)  :: PTS_G    ! garden surface temperature
REAL, DIMENSION(:), INTENT(IN)  :: PTS_SR   ! snow surface temperature
REAL, DIMENSION(:), INTENT(IN)  :: PTS_HV   ! high vegetation foliage temperature
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(T%XBLD))  :: ZT_S        ! sky temperature
REAL, DIMENSION(SIZE(T%XBLD))  :: ZEMIS_WIN   ! window emissivity
REAL, DIMENSION(SIZE(T%XBLD))  :: ZROAD       ! road   fraction relative to canyon fraction
REAL, DIMENSION(SIZE(T%XBLD))  :: ZGARDEN     ! garden fraction relative to canyon fraction
!
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_W_W
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_W_WIN
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_W_R
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_W_G
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_W_NR
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_W_HV
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_WIN_W
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_WIN_R
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_WIN_G
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_WIN_NR
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_WIN_HV
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_R_W
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_R_WIN
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_R_HV
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_G_W
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_G_WIN
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_G_HV
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_S_W
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_S_R
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_S_G
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_S_NR
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_S_WIN
REAL, DIMENSION(SIZE(T%XBLD))  :: ZF_S_HV
!
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('URBAN_LW_COEF',0,ZHOOK_HANDLE)
!
ZT_S(:) = (PLW_RAD(:)/XSTEFAN)**0.25
!
ZEMIS_WIN(:) = XEMIS_WIN_CST
!
DO JJ=1,SIZE(T%XROAD)
  !
  ZROAD(JJ)   = T%XROAD(JJ)   / (1.-T%XBLD(JJ))
  ZGARDEN(JJ) = T%XGARDEN(JJ) / (1.-T%XBLD(JJ))
  !
  ZF_W_R(JJ)     = T%XSVF_WR(JJ) * ZROAD(JJ)  ! without considering snow cover at this stage (done in wall_layer_e_budget) 
  ZF_W_G(JJ)     = T%XSVF_WR(JJ) * ZGARDEN(JJ)
  ZF_W_NR(JJ)    = ZF_W_R (JJ)                                  ! without considering snow cover at this stage (done in wall_layer_e_budget)   
  ZF_W_W(JJ)     = T%XSVF_WW(JJ) * (1.-B%XGR(JJ))
  ZF_W_WIN(JJ)   = T%XSVF_WW(JJ) *     B%XGR(JJ)
  ! 
  ZF_WIN_R(JJ)   = ZF_W_R (JJ)
  ZF_WIN_G(JJ)   = ZF_W_G (JJ)
  ZF_WIN_NR(JJ)  = ZF_W_NR(JJ)
  ZF_WIN_W(JJ)   = ZF_W_W (JJ)
  !
  ZF_R_W(JJ)     = T%XSVF_RW(JJ) * (1.-B%XGR(JJ)) * 0.5 ! one wall can emit towards the road - idem for garden
  ZF_R_WIN(JJ)   = T%XSVF_RW(JJ) *     B%XGR(JJ)
  !
  ZF_G_W(JJ)     = ZF_R_W(JJ) 
  ZF_G_WIN(JJ)   = ZF_R_WIN(JJ)
  !
  ZF_S_R(JJ)     = T%XSVF_RS(JJ) * ZROAD(JJ)   ! without considering snow cover at this stage (done in wall_layer_e_budget) 
  ZF_S_G(JJ)     = T%XSVF_RS(JJ) * ZGARDEN(JJ)
  ZF_S_NR(JJ)    = ZF_S_R(JJ)                                  ! without considering snow cover at this stage (done in wall_layer_e_budget)   
  ZF_S_W  (JJ)   = T%XSVF_SW(JJ) * (1.-B%XGR(JJ)) * 0.5
  ZF_S_WIN(JJ)   = T%XSVF_SW(JJ) *     B%XGR(JJ)
!
  ZF_W_HV(JJ)    = T%XSVF_WT(JJ) * 0.5 * T%XURBTREE(JJ) ! we corrected the hveg contribution by 0.5 because svf_wt is defined on 360° or 2pi
  ZF_WIN_HV(JJ)  = ZF_W_HV(JJ)
  ZF_R_HV(JJ)    = T%XSVF_RT(JJ) * T%XURBTREE(JJ)
  ZF_G_HV(JJ)    = ZF_R_HV(JJ)
  ZF_S_HV(JJ)    = T%XSVF_ST(JJ) * T%XURBTREE(JJ)
!
! considering middle of foliage layer in tau terms
  !
  LW%XLW_WA_TO_WB(JJ)  = ZLW(T%XEMIS_WALL(JJ),T%XEMIS_WALL(JJ),ZF_W_W(JJ),  T%XT_WALL_A(JJ,1),T%XT_WALL_B(JJ,1),T%XTAU_WW(JJ))
  LW%XLW_WA_TO_R(JJ)   = ZLW(T%XEMIS_WALL(JJ),T%XEMIS_ROAD(JJ),ZF_R_W(JJ),  T%XT_WALL_A(JJ,1),T%XT_ROAD(JJ,1),  T%XTAU_WR(JJ))
  LW%XLW_WA_TO_WIN(JJ) = ZLW(T%XEMIS_WALL(JJ),ZEMIS_WIN(JJ),   ZF_WIN_W(JJ),T%XT_WALL_A(JJ,1),B%XT_WIN1(JJ),    T%XTAU_WW(JJ))
  LW%XLW_WA_TO_S(JJ)   = ZLW(T%XEMIS_WALL(JJ),1.,              ZF_S_W(JJ),  T%XT_WALL_A(JJ,1),ZT_S(JJ),         T%XTAU_SW(JJ))
  !
  LW%XLW_WB_TO_R(JJ)   = ZLW(T%XEMIS_WALL(JJ),T%XEMIS_ROAD(JJ),ZF_R_W(JJ),  T%XT_WALL_B(JJ,1),T%XT_ROAD(JJ,1),T%XTAU_WR(JJ))
  LW%XLW_WB_TO_WIN(JJ) = ZLW(T%XEMIS_WALL(JJ),ZEMIS_WIN(JJ),   ZF_WIN_W(JJ),T%XT_WALL_B(JJ,1),B%XT_WIN1(JJ),  T%XTAU_WW(JJ))
  LW%XLW_WB_TO_S(JJ)   = ZLW(T%XEMIS_WALL(JJ),1.,              ZF_S_W(JJ),  T%XT_WALL_B(JJ,1),ZT_S(JJ),       T%XTAU_SW(JJ))
  !
  LW%XLW_R_TO_WA(JJ) = ZLW(T%XEMIS_ROAD(JJ),T%XEMIS_WALL(JJ),ZF_W_R(JJ),  T%XT_ROAD(JJ,1),T%XT_WALL_A(JJ,1),T%XTAU_WR(JJ))
  LW%XLW_R_TO_WB(JJ) = ZLW(T%XEMIS_ROAD(JJ),T%XEMIS_WALL(JJ),ZF_W_R(JJ),  T%XT_ROAD(JJ,1),T%XT_WALL_B(JJ,1),T%XTAU_WR(JJ))
  LW%XLW_R_TO_WIN(JJ)= ZLW(T%XEMIS_ROAD(JJ),ZEMIS_WIN(JJ),   ZF_WIN_R(JJ),T%XT_ROAD(JJ,1),B%XT_WIN1(JJ),    T%XTAU_WR(JJ))
  LW%XLW_R_TO_S(JJ)  = ZLW(T%XEMIS_ROAD(JJ),1.,              ZF_S_R(JJ),  T%XT_ROAD(JJ,1),ZT_S(JJ),         T%XTAU_SR(JJ))

  !  
  LW%XLW_WIN_TO_WA(JJ) = ZLW(ZEMIS_WIN(JJ),T%XEMIS_WALL(JJ), ZF_W_WIN(JJ), B%XT_WIN1(JJ),T%XT_WALL_A(JJ,1),T%XTAU_WW(JJ))
  LW%XLW_WIN_TO_WB(JJ) = ZLW(ZEMIS_WIN(JJ),T%XEMIS_WALL(JJ), ZF_W_WIN(JJ), B%XT_WIN1(JJ),T%XT_WALL_B(JJ,1),T%XTAU_WW(JJ))
  LW%XLW_WIN_TO_R(JJ)  = ZLW(ZEMIS_WIN(JJ),T%XEMIS_ROAD(JJ), ZF_R_WIN(JJ), B%XT_WIN1(JJ),T%XT_ROAD(JJ,1)  ,T%XTAU_WR(JJ))
  LW%XLW_WIN_TO_S(JJ)  = ZLW(ZEMIS_WIN(JJ),1.,               ZF_S_WIN(JJ), B%XT_WIN1(JJ),ZT_S(JJ),         T%XTAU_WR(JJ))
  !
  LW%XLW_S_TO_WA(JJ)  = ZLW(1.,T%XEMIS_WALL(JJ),  T%XSVF_WS(JJ),ZT_S(JJ),T%XT_WALL_A(JJ,1),T%XTAU_SW(JJ))
  LW%XLW_S_TO_WB(JJ)  = ZLW(1.,T%XEMIS_WALL(JJ),  T%XSVF_WS(JJ),ZT_S(JJ),T%XT_WALL_B(JJ,1),T%XTAU_SW(JJ))
  LW%XLW_S_TO_R(JJ)   = ZLW(1.,T%XEMIS_ROAD(JJ),  T%XSVF_RS(JJ),ZT_S(JJ),T%XT_ROAD(JJ,1),  T%XTAU_SR(JJ))  
  LW%XLW_S_TO_WIN(JJ) = ZLW(1.,ZEMIS_WIN(JJ),     T%XSVF_WS(JJ),ZT_S(JJ),B%XT_WIN1(JJ),    T%XTAU_SW(JJ))
  !
  IF (SIZE(PTS_G)>0) THEN
    LW%XLW_WA_TO_G(JJ)   = ZLW(T%XEMIS_WALL(JJ),PEMIS_G(JJ),     ZF_G_W(JJ),  T%XT_WALL_A(JJ,1),PTS_G(JJ),T%XTAU_WR(JJ))
    LW%XLW_WB_TO_G(JJ)   = ZLW(T%XEMIS_WALL(JJ),PEMIS_G(JJ),     ZF_G_W(JJ),  T%XT_WALL_B(JJ,1),PTS_G(JJ),T%XTAU_WR(JJ))
    !
    LW%XLW_G_TO_WA(JJ) = ZLW(PEMIS_G(JJ),  T%XEMIS_WALL(JJ),ZF_W_G(JJ),   PTS_G(JJ),    T%XT_WALL_A(JJ,1),T%XTAU_WR(JJ))
    LW%XLW_G_TO_WB(JJ) = ZLW(PEMIS_G(JJ),  T%XEMIS_WALL(JJ),ZF_W_G(JJ),   PTS_G(JJ),    T%XT_WALL_B(JJ,1),T%XTAU_WR(JJ))
    LW%XLW_G_TO_WIN(JJ)= ZLW(PEMIS_G(JJ),  ZEMIS_WIN(JJ),   ZF_WIN_G(JJ), PTS_G(JJ),    B%XT_WIN1(JJ),    T%XTAU_WR(JJ))
    LW%XLW_G_TO_S(JJ)  = ZLW(PEMIS_G(JJ),  1.,              ZF_S_G(JJ),   PTS_G(JJ),    ZT_S(JJ),         T%XTAU_SR(JJ))
    LW%XLW_WIN_TO_G(JJ)= ZLW(ZEMIS_WIN(JJ),PEMIS_G(JJ),     ZF_G_WIN(JJ), B%XT_WIN1(JJ),PTS_G(JJ),        T%XTAU_WR(JJ))
    LW%XLW_S_TO_G(JJ)  = ZLW(1.,           PEMIS_G(JJ),     T%XSVF_RS(JJ),ZT_S(JJ),     PTS_G(JJ),        T%XTAU_SR(JJ))
  ELSE
    LW%XLW_WA_TO_G(JJ) = 0.
    LW%XLW_WB_TO_G(JJ) = 0.
    !
    LW%XLW_G_TO_WA(JJ) = 0.
    LW%XLW_G_TO_WB(JJ) = 0.
    LW%XLW_G_TO_WIN(JJ)= 0.
    LW%XLW_G_TO_S(JJ)  = 0.
    LW%XLW_WIN_TO_G(JJ)= 0.
    LW%XLW_S_TO_G(JJ)  = 0.
  END IF

  IF (T%XURBTREE(JJ) .GT. 0.) THEN
    LW%XLW_HV_TO_WA(JJ)=   ZLW(PEMIS_HV(JJ),T%XEMIS_WALL(JJ),T%XSVF_WW(JJ),       PTS_HV(JJ),T%XT_WALL_A(JJ,1),(1.-T%XTAU_WW(JJ))) & ! opposite wall + window sector
                      + ZLW(PEMIS_HV(JJ),T%XEMIS_WALL(JJ),T%XSVF_WR(JJ),       PTS_HV(JJ),T%XT_WALL_A(JJ,1),(1.-T%XTAU_WR(JJ))) & ! ground sector
                      + ZLW(PEMIS_HV(JJ),T%XEMIS_WALL(JJ),T%XSVF_WS(JJ),       PTS_HV(JJ),T%XT_WALL_A(JJ,1),(1.-T%XTAU_SW(JJ)))   ! sky sector

    LW%XLW_HV_TO_WB(JJ)=   ZLW(PEMIS_HV(JJ),T%XEMIS_WALL(JJ),T%XSVF_WW(JJ),       PTS_HV(JJ),T%XT_WALL_B(JJ,1),(1.-T%XTAU_WW(JJ))) & ! opposite wall + window sector
                      + ZLW(PEMIS_HV(JJ),T%XEMIS_WALL(JJ),T%XSVF_WR(JJ),       PTS_HV(JJ),T%XT_WALL_B(JJ,1),(1.-T%XTAU_WR(JJ))) & ! ground sector
                      + ZLW(PEMIS_HV(JJ),T%XEMIS_WALL(JJ),T%XSVF_WS(JJ),       PTS_HV(JJ),T%XT_WALL_B(JJ,1),(1.-T%XTAU_SW(JJ)))   ! sky sector

    LW%XLW_HV_TO_WIN(JJ)=  ZLW(PEMIS_HV(JJ),ZEMIS_WIN(JJ),   T%XSVF_WW(JJ),       PTS_HV(JJ),B%XT_WIN1(JJ),    (1.-T%XTAU_WW(JJ))) & ! opposite wall + window sector
                      + ZLW(PEMIS_HV(JJ),ZEMIS_WIN(JJ),   T%XSVF_WR(JJ),       PTS_HV(JJ),B%XT_WIN1(JJ),    (1.-T%XTAU_WR(JJ))) & ! ground sector
                      + ZLW(PEMIS_HV(JJ),ZEMIS_WIN(JJ),   T%XSVF_WS(JJ),       PTS_HV(JJ),B%XT_WIN1(JJ),    (1.-T%XTAU_SW(JJ)))   !  sky sector

    LW%XLW_HV_TO_R(JJ) =   2.* ZLW(PEMIS_HV(JJ),T%XEMIS_ROAD(JJ),T%XSVF_RW(JJ),   PTS_HV(JJ),T%XT_ROAD(JJ,1),  (1.-T%XTAU_WR(JJ))) & ! 2 walls and window sector
                          + ZLW(PEMIS_HV(JJ),T%XEMIS_ROAD(JJ),T%XSVF_RS(JJ),   PTS_HV(JJ),T%XT_ROAD(JJ,1),  (1.-T%XTAU_SR(JJ)))   ! sky sector

    LW%XLW_HV_TO_G(JJ) =   2.* ZLW(PEMIS_HV(JJ),PEMIS_G(JJ),     T%XSVF_RW(JJ),   PTS_HV(JJ),PTS_G(JJ),        (1.-T%XTAU_WR(JJ))) &
                          + ZLW(PEMIS_HV(JJ),PEMIS_G(JJ),     T%XSVF_RS(JJ),   PTS_HV(JJ),PTS_G(JJ),        (1.-T%XTAU_SR(JJ)))  

    LW%XLW_HV_TO_S(JJ) =   2.* ZLW(PEMIS_HV(JJ),1.,              ZF_S_W(JJ),      PTS_HV(JJ),ZT_S(JJ),         (1.-T%XTAU_SW(JJ))) &
                          + ZLW(PEMIS_HV(JJ),1.,              ZF_S_R(JJ),      PTS_HV(JJ),ZT_S(JJ),         (1.-T%XTAU_SR(JJ)))

    LW%XLW_R_TO_HV(JJ)   = LW%XLW_HV_TO_R(JJ)  * ZROAD(JJ)   / T%XURBTREE(JJ)                              ! to insure conservation of energy exchanged
    LW%XLW_G_TO_HV(JJ)   = LW%XLW_HV_TO_G(JJ)  * ZGARDEN(JJ) / T%XURBTREE(JJ)                              !
    LW%XLW_S_TO_HV(JJ)   = LW%XLW_HV_TO_S(JJ)                / T%XURBTREE(JJ)                              !
    LW%XLW_WA_TO_HV(JJ)  = LW%XLW_HV_TO_WA(JJ) * (1.-B%XGR(JJ)) * 0.5 * T%XWALL_O_HOR(JJ) / T%XFRAC_HVEG(JJ) ! 
    LW%XLW_WB_TO_HV(JJ)  = LW%XLW_HV_TO_WB(JJ) * (1.-B%XGR(JJ)) * 0.5 * T%XWALL_O_HOR(JJ) / T%XFRAC_HVEG(JJ) ! 
    LW%XLW_WIN_TO_HV(JJ) = LW%XLW_HV_TO_WIN(JJ)*     B%XGR(JJ)        * T%XWALL_O_HOR(JJ) / T%XFRAC_HVEG(JJ) ! 
  ELSE
    LW%XLW_WA_TO_HV(JJ)  = 0.
    LW%XLW_WB_TO_HV(JJ)  = 0.
    LW%XLW_R_TO_HV(JJ)   = 0.
    LW%XLW_G_TO_HV(JJ)   = 0.
    LW%XLW_WIN_TO_HV(JJ) = 0.
    LW%XLW_S_TO_HV(JJ)   = 0.
    LW%XLW_HV_TO_WA(JJ)  = 0.
    LW%XLW_HV_TO_WB(JJ)  = 0.
    LW%XLW_HV_TO_R(JJ)   = 0.
    LW%XLW_HV_TO_G(JJ)   = 0.
    LW%XLW_HV_TO_WIN(JJ) = 0.
    LW%XLW_HV_TO_S(JJ)   = 0.
  ENDIF
  !
  IF (PTS_SR(JJ) .EQ. XUNDEF) THEN
    LW%XLW_WA_TO_NR (JJ) = 0.
    LW%XLW_WB_TO_NR (JJ) = 0.
    LW%XLW_HV_TO_NR(JJ)  = 0.
    LW%XLW_WIN_TO_NR(JJ) = 0.
    LW%XLW_S_TO_NR  (JJ) = 0.
!
    LW%XLW_NR_TO_WA (JJ) = 0.
    LW%XLW_NR_TO_WB (JJ) = 0.
    LW%XLW_NR_TO_HV(JJ)  = 0.
    LW%XLW_NR_TO_WIN(JJ) = 0.
    LW%XLW_NR_TO_S(JJ)   = 0.
!
    LW%XLW_HV_TO_NR(JJ) = 0.
    LW%XLW_NR_TO_HV(JJ) = 0.
  ELSE
    LW%XLW_WA_TO_NR(JJ) = ZLW(T%XEMIS_WALL(JJ),T%TSNOW_ROAD%EMIS(JJ),ZF_R_W(JJ),T%XT_WALL_A(JJ,1),PTS_SR(JJ),T%XTAU_WR(JJ))
    LW%XLW_WB_TO_NR(JJ) = ZLW(T%XEMIS_WALL(JJ),T%TSNOW_ROAD%EMIS(JJ),ZF_R_W(JJ),T%XT_WALL_B(JJ,1),PTS_SR(JJ),T%XTAU_WR(JJ))
    LW%XLW_NR_TO_WA(JJ) = ZLW(T%TSNOW_ROAD%EMIS(JJ),T%XEMIS_WALL(JJ),ZF_W_NR(JJ),PTS_SR(JJ),T%XT_WALL_A(JJ,1),T%XTAU_WR(JJ))    
    LW%XLW_NR_TO_WB(JJ) = ZLW(T%TSNOW_ROAD%EMIS(JJ),T%XEMIS_WALL(JJ),ZF_W_NR(JJ),PTS_SR(JJ),T%XT_WALL_B(JJ,1),T%XTAU_WR(JJ))    
    LW%XLW_WIN_TO_NR(JJ)= ZLW(ZEMIS_WIN(JJ),T%TSNOW_ROAD%EMIS(JJ),ZF_R_WIN(JJ),B%XT_WIN1(JJ),PTS_SR(JJ),T%XTAU_WR(JJ))
    LW%XLW_NR_TO_WIN(JJ)= ZLW(T%TSNOW_ROAD%EMIS(JJ),ZEMIS_WIN(JJ),ZF_WIN_NR(JJ),PTS_SR(JJ),B%XT_WIN1(JJ),T%XTAU_WR(JJ))
    LW%XLW_S_TO_NR(JJ) = ZLW(1.,T%TSNOW_ROAD%EMIS(JJ),T%XSVF_RS(JJ),ZT_S(JJ),PTS_SR(JJ),T%XTAU_WR(JJ))
!
    LW%XLW_WIN_TO_R(JJ)= ZLW(ZEMIS_WIN(JJ),T%XEMIS_ROAD(JJ),ZF_R_WIN(JJ),B%XT_WIN1(JJ),T%XT_ROAD(JJ,1),T%XTAU_WR(JJ))
    LW%XLW_R_TO_WIN(JJ)= ZLW(T%XEMIS_ROAD(JJ),ZEMIS_WIN(JJ),ZF_WIN_R(JJ),T%XT_ROAD(JJ,1),B%XT_WIN1(JJ),T%XTAU_WR(JJ))
!
    IF (T%XURBTREE(JJ) .GT. 0.) THEN
       LW%XLW_HV_TO_NR(JJ) =  2.* ZLW(PEMIS_HV(JJ),T%TSNOW_ROAD%EMIS(JJ),T%XSVF_RW(JJ), PTS_HV(JJ),PTS_SR(JJ),(1.-T%XTAU_WR(JJ))) & ! 2 walls and window sector
                             + ZLW(PEMIS_HV(JJ),T%TSNOW_ROAD%EMIS(JJ),T%XSVF_RS(JJ),  PTS_HV(JJ),PTS_SR(JJ),  (1.-T%XTAU_SR(JJ)))   ! sky sector
       LW%XLW_NR_TO_HV(JJ) = LW%XLW_HV_TO_NR(JJ)  * ZROAD(JJ)   / T%XURBTREE(JJ)                                                           ! to insure conservation of energy exchanged
    ELSE
      LW%XLW_HV_TO_NR(JJ) = 0.
      LW%XLW_NR_TO_HV(JJ) = 0.
    ENDIF
!
  ENDIF
ENDDO
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('URBAN_LW_COEF',1,ZHOOK_HANDLE)
!
CONTAINS

REAL FUNCTION ZLW (PE1, PE2, PF, PT1, PT2, ZTAU)
REAL, INTENT (IN) :: PE1, PE2, PF, PT1, PT2, ZTAU

IF (PE1 == XUNDEF .OR. PE2 == XUNDEF .OR. PF == XUNDEF .OR. PT1 == XUNDEF &
     .OR. PT2 == XUNDEF .OR. ZTAU == XUNDEF) THEN
  ZLW = 0.
ELSE
  ZLW = 4.*XSTEFAN*PE1*PE2*PF*ZTAU*((PT1+PT2)/2.)**3
ENDIF

END FUNCTION ZLW
!
END SUBROUTINE URBAN_LW_COEF
