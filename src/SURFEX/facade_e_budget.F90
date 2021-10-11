!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE FACADE_E_BUDGET(TOP, BOP, T, B, DMT, CT, LW, HPROGRAM, PTSTEP, PDN_RD, PRHOA, PAC_WL, PAC_BLD, &
                           PDIAG_TI_RF, PLW_RAD, PPS, PEXNS, PT_CANYON, PTS_RD, PTSN_RD, PTS_GD, PTS_HV,&
                           PFLX_BLD_WL_A, PDQS_WL_A,                                &
                           PFLX_BLD_WL_B, PDQS_WL_B, PEMIT_LW_FAC,                  &
                           PRADHT_IN, PRAD_RF_WL, PRAD_RF_WIN, PRAD_WL_FL,          &
                           PRAD_WL_MA, PRAD_WIN_FL, PRAD_WIN_MA, PCONV_WL_BLD,      &
                           PCONV_WIN_BLD, PAC_WIN,  PLOAD_IN_WL, PLOAD_IN_WIN,      &
                           PSUMDIFIMP                                               )
!
!   ##########################################################################
!
!!****  *FACADE_E_BUDGET*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the evolution of wall and window temperature from
!     wall_layer_e_budget et window_e_budget
!         
!     
!!**  METHOD
!     ------
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
!!      G. Pigeon           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/11/2011 
!!      G. Pigeon     /09/2012: new conv. coef for indoor/outdoor 
!!      G. Pigeon     /10/2012: new arg. : solar heat load for indoor
!!      E.Redon/A.Lmeonsu /01/2016: contribution of high vegetation
!!      V. Masson   04.2020 completes energy check for high vegetation IR exchanges
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
USE MODD_CHECK_TEB, ONLY : CHECK_TEB_t
USE MODD_LW_COEF, ONLY : LW_COEF_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_TEB_PAR, ONLY : XEMIS_WIN_CST
!
USE MODI_WALL_LAYER_E_BUDGET
USE MODI_WINDOW_E_BUDGET
USE YOMHOOK, ONLY : LHOOK,   DR_HOOK
USE PARKIND1,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DMT
TYPE(CHECK_TEB_t), INTENT(INOUT) :: CT
TYPE(LW_COEF_t), INTENT(IN) :: LW
!
CHARACTER(LEN=6),   INTENT(IN)    :: HPROGRAM     ! program calling surf. schemes
REAL,               INTENT(IN)    :: PTSTEP       ! time step
REAL, DIMENSION(:), INTENT(IN)    :: PDN_RD     ! snow-covered fraction on roads
REAL, DIMENSION(:), INTENT(IN)    :: PRHOA        ! rho
REAL, DIMENSION(:), INTENT(IN)    :: PAC_WL     ! aerodynamical conductance
!                                                 ! between wall and canyon
REAL, DIMENSION(:), INTENT(IN)    :: PAC_BLD      ! aerodynamical conductance
                                                  ! inside the building itself
REAL, DIMENSION(:), INTENT(IN)    :: PDIAG_TI_RF
REAL, DIMENSION(:), INTENT(IN)    :: PLW_RAD      ! atmospheric infrared radiation
REAL, DIMENSION(:), INTENT(IN)    :: PPS          ! pressure at the surface
REAL, DIMENSION(:), INTENT(IN)    :: PEXNS        ! surface Exner function
REAL, DIMENSION(:), INTENT(IN)    :: PT_CANYON    ! air canyon temperature
REAL, DIMENSION(:), INTENT(IN)    :: PTS_RD     ! road surface temperature
REAL, DIMENSION(:), INTENT(IN)    :: PTSN_RD  ! road snow temperature
REAL, DIMENSION(:), INTENT(IN)    :: PTS_GD   ! green area surface temperature
REAL, DIMENSION(:), INTENT(IN)    :: PTS_HV     ! high veg surface temperature
!
REAL, DIMENSION(:), INTENT(OUT)   :: PFLX_BLD_WL_A! flux from bld to wall
REAL, DIMENSION(:), INTENT(INOUT) :: PDQS_WL_A    ! heat storage inside the wall 
REAL, DIMENSION(:), INTENT(OUT)   :: PFLX_BLD_WL_B! flux from bld to wall
REAL, DIMENSION(:), INTENT(INOUT) :: PDQS_WL_B    ! heat storage inside the wall 
REAL, DIMENSION(:), INTENT(OUT)   :: PEMIT_LW_FAC ! LW flux emitted by the facade (W/m2 of facade)
REAL, DIMENSION(:,:), INTENT(IN)  :: PRADHT_IN     ! Indoor radiant heat transfer coefficient
                                                    ! [W K-1 m-2]
REAL, DIMENSION(:), INTENT(IN)    :: PRAD_RF_WL ! rad. fluxes from roof to wall [W m-2(roof)]
REAL, DIMENSION(:), INTENT(IN)    :: PRAD_RF_WIN  ! rad. fluxes from roof to win [W m-2(roof)]
REAL, DIMENSION(:,:), INTENT(OUT) :: PRAD_WL_FL! rad. fluxes from wall to floor [W m-2(wall)]
REAL, DIMENSION(:,:), INTENT(OUT) :: PRAD_WL_MA ! rad. fluxes from wall to mass [W m-2(wall)]
REAL, DIMENSION(:,:), INTENT(OUT) :: PRAD_WIN_FL ! rad. fluxes from window to floor [W m-2(win)]
REAL, DIMENSION(:,:), INTENT(OUT) :: PRAD_WIN_MA  ! rad. fluxes from window to mass [W m-2(win)]
REAL, DIMENSION(:,:), INTENT(OUT) :: PCONV_WL_BLD ! conv. fluxes from wall to bld [W m-2(wall)]
REAL, DIMENSION(:,:), INTENT(OUT) :: PCONV_WIN_BLD  ! conv. fluxes from window to bld [W m-2(wind.)]
REAL, DIMENSION(:),   INTENT(IN)  :: PAC_WIN        ! window aerodynamic conductance
!
REAL, DIMENSION(:), INTENT(IN)    :: PLOAD_IN_WL  ! solar + inter. heat gains W/m2 [wall]
REAL, DIMENSION(:), INTENT(IN)    :: PLOAD_IN_WIN   ! solar + inter. heat gains W/m2 [win]
REAL, DIMENSION(:), INTENT(OUT)   :: PSUMDIFIMP   ! Energy imbalance due to implicitation [W/m²(urb)]
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(T%XBLD)) :: ZRAD_WL_WIN   ! rad. fluxes between averaged wall and win [W m-2(wall)]
REAL, DIMENSION(SIZE(T%XBLD)) :: ZRAD_WL_A_WIN ! rad. fluxes between one wall and win [W m-2(wall)]
REAL, DIMENSION(SIZE(T%XBLD)) :: ZEMIS_WIN     ! window emissivity
REAL, DIMENSION(SIZE(T%XBLD)) :: ZEMIT_LW_WIN  ! Longwave radiation emitted by the window [W m-2(window)]
REAL, DIMENSION(SIZE(T%XBLD)) :: ZEMIT_LW_WL_A ! Longwave radiation emitted by the wall [W m-2(wall)]
REAL, DIMENSION(SIZE(T%XBLD)) :: ZEMIT_LW_WL_B ! Longwave radiation emitted by the wall [W m-2(wall)]
REAL, DIMENSION(SIZE(T%XBLD)) :: ZTS_WL_A    ! surface temperature of wall A at previous time-step
REAL, DIMENSION(SIZE(T%XBLD)) :: ZTS_WL_B    ! surface temperature of wall B at previous time-step
REAL, DIMENSION(SIZE(T%XBLD)) :: ZTI_WL_A    ! internal temperature of wall A at previous time-step
REAL, DIMENSION(SIZE(T%XBLD)) :: ZTI_WL_B    ! internal temperature of wall B at previous time-step
REAL, DIMENSION(SIZE(T%XBLD)) :: ZTS_WL      ! surface temperature of averaged wall at new time-step
REAL, DIMENSION(SIZE(T%XBLD),SIZE(B%XTI_BLD,2)) :: ZRAD_WL_FL   ! rad. fluxes from wall to floor [W m-2(wall)]
REAL, DIMENSION(SIZE(T%XBLD),SIZE(B%XTI_BLD,2)) :: ZRAD_WL_MA   ! rad. fluxes from wall to mass [W m-2(wall)]
REAL, DIMENSION(SIZE(T%XBLD),SIZE(B%XTI_BLD,2)) :: ZCONV_WL_BLD ! conv. fluxes from wall to bld [W m-2(wall)]
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_W_TO_WIN   ! Radiative heat transfer coeff wall-window [W K-1 m-2]
REAL, DIMENSION(SIZE(T%XBLD)) :: ZSUMDIFIMP      ! Energy imbalance due to implicitation [W m-2(urb)]
!
REAL, DIMENSION(SIZE(T%XBLD))   :: ZLW_RD_TO_WLA
REAL, DIMENSION(SIZE(T%XBLD))   :: ZLW_GD_TO_WLA
REAL, DIMENSION(SIZE(T%XBLD))   :: ZLW_HV_TO_WLA
REAL, DIMENSION(SIZE(T%XBLD))   :: ZLW_SN_TO_WLA
REAL, DIMENSION(SIZE(T%XBLD))   :: ZLW_WLB_TO_WLA
REAL, DIMENSION(SIZE(T%XBLD))   :: ZLW_WIND_TO_WLA
REAL, DIMENSION(SIZE(T%XBLD))   :: ZLW_RD_TO_WLB
REAL, DIMENSION(SIZE(T%XBLD))   :: ZLW_GD_TO_WLB
REAL, DIMENSION(SIZE(T%XBLD))   :: ZLW_HV_TO_WLB
REAL, DIMENSION(SIZE(T%XBLD))   :: ZLW_SN_TO_WLB
REAL, DIMENSION(SIZE(T%XBLD))   :: ZLW_WLA_TO_WLB
REAL, DIMENSION(SIZE(T%XBLD))   :: ZLW_WIND_TO_WLB

INTEGER                        :: IWL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FACADE_E_BUDGET',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
PRAD_WL_FL= XUNDEF
PRAD_WL_MA = XUNDEF
PRAD_WIN_FL = XUNDEF
PRAD_WIN_MA  = XUNDEF
PCONV_WL_BLD = XUNDEF
PCONV_WIN_BLD  = XUNDEF
!
!* surface temperature of the opposite wall
!  ----------------------------------------
!
ZTS_WL_A(:) = T%XT_WALL_A(:,1)
ZTS_WL_B(:) = T%XT_WALL_B(:,1)
IWL = SIZE(T%XT_WALL_A,2)
ZTI_WL_A(:) = T%XT_WALL_A(:,IWL)
ZTI_WL_B(:) = T%XT_WALL_B(:,IWL)
!
! *Convection heat transfer coefficients [W m-2 K-1] from EP Engineering Reference
! --------------------------------------------------------------------------------
!
!
! *opaque balance of the facade for wall A
!  ---------------------------------------
!
 CALL WALL_LAYER_E_BUDGET(TOP, BOP, T, B, CT, HPROGRAM, T%XT_WALL_A, ZTS_WL_B, ZTI_WL_B, PTSTEP, PDN_RD, &
                          PRHOA, PAC_WL, PAC_BLD, PDIAG_TI_RF, PLW_RAD, PPS, PEXNS, DMT%XABS_SW_WALL_A,  &
                          DMT%XABS_LW_WALL_A, PT_CANYON, PTS_RD, PTSN_RD, PTS_GD, PTS_HV, LW%XLW_WA_TO_WB, &
                          LW%XLW_R_TO_WA, LW%XLW_G_TO_WA, LW%XLW_NR_TO_WA, LW%XLW_WIN_TO_WA,      &
                          LW%XLW_S_TO_WA, LW%XLW_HV_TO_WA, PFLX_BLD_WL_A, PDQS_WL_A, DMT%XABS_LW_WALL_A, &
                          ZEMIT_LW_WL_A, DMT%XH_WALL_A, PRADHT_IN, PRAD_RF_WL, &
                          ZRAD_WL_A_WIN, ZRAD_WL_FL, ZRAD_WL_MA, ZCONV_WL_BLD,        &
                          PLOAD_IN_WL, ZSUMDIFIMP,         &
                          ZLW_RD_TO_WLA, ZLW_GD_TO_WLA, ZLW_HV_TO_WLA, ZLW_SN_TO_WLA,   &
                          ZLW_WLB_TO_WLA, ZLW_WIND_TO_WLA)
!
PSUMDIFIMP   = 0.5 * ZSUMDIFIMP
PRAD_WL_FL   = 0.5 * ZRAD_WL_FL
PRAD_WL_MA   = 0.5 * ZRAD_WL_MA 
PCONV_WL_BLD = 0.5 * ZCONV_WL_BLD 
ZRAD_WL_WIN  = 0.5 * ZRAD_WL_A_WIN
!
IF (CT%LCHECK_TEB.AND..NOT.TOP%LEXPLW) THEN
  CT%XLW_ROAD_TO_WALA   = ZLW_RD_TO_WLA
  CT%XLW_GARD_TO_WALA   = ZLW_GD_TO_WLA
  CT%XLW_HV_TO_WALA     = ZLW_HV_TO_WLA
  CT%XLW_SNOW_TO_WALA   = ZLW_SN_TO_WLA
  CT%XLW_WALB_TO_WALA   = ZLW_WLB_TO_WLA
  CT%XLW_WIND_TO_WALA   = ZLW_WIND_TO_WLA
END IF
!
! *opaque balance of the facade for wall B
!  ---------------------------------------
!
IF (TOP%CWALL_OPT/='UNIF') THEN
 CALL WALL_LAYER_E_BUDGET(TOP, BOP, T, B, CT, HPROGRAM, T%XT_WALL_B, ZTS_WL_A, ZTI_WL_A, PTSTEP, PDN_RD,   &
                          PRHOA, PAC_WL, PAC_BLD, PDIAG_TI_RF, PLW_RAD, PPS, PEXNS, DMT%XABS_SW_WALL_B,    &
                          DMT%XABS_LW_WALL_B, PT_CANYON, PTS_RD, PTSN_RD, PTS_GD, PTS_HV, LW%XLW_WA_TO_WB, &
                          LW%XLW_R_TO_WB, LW%XLW_G_TO_WB, LW%XLW_NR_TO_WB, LW%XLW_WIN_TO_WB,               &
                          LW%XLW_S_TO_WB, LW%XLW_HV_TO_WB, PFLX_BLD_WL_B, PDQS_WL_B, DMT%XABS_LW_WALL_B,   &
                          ZEMIT_LW_WL_B, DMT%XH_WALL_B, PRADHT_IN, PRAD_RF_WL,                             &
                          ZRAD_WL_A_WIN, ZRAD_WL_FL, ZRAD_WL_MA, ZCONV_WL_BLD,                             &
                          PLOAD_IN_WL, ZSUMDIFIMP,                                                         &
                          ZLW_RD_TO_WLB, ZLW_GD_TO_WLB, ZLW_HV_TO_WLB, ZLW_SN_TO_WLB,                      &
                          ZLW_WLA_TO_WLB, ZLW_WIND_TO_WLB)
!
  IF (CT%LCHECK_TEB.AND..NOT.TOP%LEXPLW) THEN
    CT%XLW_ROAD_TO_WALB   = ZLW_RD_TO_WLB
    CT%XLW_GARD_TO_WALB   = ZLW_GD_TO_WLB
    CT%XLW_HV_TO_WALB     = ZLW_HV_TO_WLB
    CT%XLW_SNOW_TO_WALB   = ZLW_SN_TO_WLB
    CT%XLW_WALA_TO_WALB   = ZLW_WLA_TO_WLB
    CT%XLW_WIND_TO_WALB   = ZLW_WIND_TO_WLB
  END IF
ELSE
  T%XT_WALL_B    = T%XT_WALL_A
  DMT%XH_WALL_B        = DMT%XH_WALL_A
  DMT%XABS_LW_WALL_B   = DMT%XABS_LW_WALL_A
  PDQS_WL_B      = PDQS_WL_A
  PFLX_BLD_WL_B  = PFLX_BLD_WL_A
  ZEMIT_LW_WL_B  = ZEMIT_LW_WL_A
  IF (CT%LCHECK_TEB.AND..NOT.TOP%LEXPLW) THEN
    CT%XLW_ROAD_TO_WALB   = CT%XLW_ROAD_TO_WALA
    CT%XLW_GARD_TO_WALB   = CT%XLW_GARD_TO_WALA
    CT%XLW_HV_TO_WALB     = CT%XLW_HV_TO_WALA
    CT%XLW_SNOW_TO_WALB   = CT%XLW_SNOW_TO_WALA
    CT%XLW_WALA_TO_WALB   = 0.
    CT%XLW_WIND_TO_WALB   = CT%XLW_WIND_TO_WALA
  END IF
END IF
!
PSUMDIFIMP   = PSUMDIFIMP   + 0.5 * ZSUMDIFIMP
PRAD_WL_FL   = PRAD_WL_FL   + 0.5 * ZRAD_WL_FL
PRAD_WL_MA   = PRAD_WL_MA   + 0.5 * ZRAD_WL_MA 
PCONV_WL_BLD = PCONV_WL_BLD + 0.5 * ZCONV_WL_BLD 
ZRAD_WL_WIN  = ZRAD_WL_WIN  + 0.5 * ZRAD_WL_A_WIN
!
!-------------------------------------------------------------------------------
!
! *Energy Balance for windows (averaged on both walls)
!  ---------------------------------------------------
!
IF (TOP%CBEM == 'DEF') THEN
   IF (CT%LCHECK_TEB) THEN
      CT%XLW_GARD_TO_WIND (:) = 0.0
      CT%XLW_HV_TO_WIND   (:) = 0.0
      CT%XLW_ROAD_TO_WIND (:) = 0.0
      CT%XLW_WALL_TO_WIND (:) = 0.0
      CT%XLW_SNOW_TO_WIND (:) = 0.0
   END IF
   ZEMIT_LW_WIN  (:) = 0.0
   DMT%XABS_LW_WIN   (:) = XUNDEF
END IF
!
IF (TOP%CBEM == 'BEM') THEN
   ZEMIS_WIN(:) = XEMIS_WIN_CST
   ZLW_W_TO_WIN(:) = 0.5 * (LW%XLW_WA_TO_WIN(:) + LW%XLW_WB_TO_WIN(:))
   ZTS_WL(:) = XUNDEF
   WHERE (ZLW_W_TO_WIN(:)>0.) &
   ZTS_WL(:) = 0.5 * ( LW%XLW_WA_TO_WIN(:)*T%XT_WALL_A(:,1)+LW%XLW_WB_TO_WIN(:)*T%XT_WALL_B(:,1) )&
                   / ZLW_W_TO_WIN(:)
   CALL WINDOW_E_BUDGET(T, B, CT, TOP, DMT, ZEMIS_WIN, ZLW_W_TO_WIN, LW%XLW_R_TO_WIN, LW%XLW_G_TO_WIN, &
                        LW%XLW_NR_TO_WIN, LW%XLW_S_TO_WIN, LW%XLW_HV_TO_WIN, PRAD_RF_WIN, ZRAD_WL_WIN, &
                        DMT%XABS_SW_WIN, PLW_RAD, PAC_WIN, PRADHT_IN, PRHOA, PDN_RD,  PT_CANYON,       &
                        ZTS_WL, PTS_RD, PTSN_RD, PTS_GD, PTS_HV, PRAD_WIN_FL, PRAD_WIN_MA,             &
                        PCONV_WIN_BLD, ZEMIT_LW_WIN, PLOAD_IN_WIN                                      )
ENDIF

!*        wall, and win emitted LW radiation on snow-free surfaces
!         ----------------------------------------------------------------
!
!
PEMIT_LW_FAC (:) = 0.5*(ZEMIT_LW_WL_A(:)+ZEMIT_LW_WL_B(:)) * (1-B%XGR(:)) +  ZEMIT_LW_WIN(:) * B%XGR(:)
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FACADE_E_BUDGET',1,ZHOOK_HANDLE)
END SUBROUTINE FACADE_E_BUDGET
