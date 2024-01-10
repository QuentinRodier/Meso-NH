!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   ##########################################################################
    SUBROUTINE WALL_LAYER_E_BUDGET(TOP, BOP, T, B, CT, HPROGRAM, PT_WL, PTS_WL_B, PTI_WL_B, PTSTEP, PDN_RD,     &
                                   PRHOA, PAC_WL, PAC_BLD, PDIAG_TI_RF, PLW_RAD, PPS, PEXNS, PABS_SW_WL,  &
                                   PABS_LW_EXPLICIT_WL, PT_CANYON, PTS_RD, PTSNOW_RD, PTS_GD, PTS_HV,           &
                                   PLW_WA_TO_WB, PLW_R_TO_W, PLW_G_TO_W, PLW_NR_TO_W,        &
                                   PLW_WIN_TO_W, PLW_S_TO_W, PLW_HV_TO_W, PFLX_BLD_WL, PDQS_WL,           &
                                   PABS_LW_WL, PEMIT_LW_WL, PH_WL, PRADHT_IN,       &
                                   PRAD_RF_WL, PRAD_WL_WIN, PRAD_WL_FL, PRAD_WL_MA,          &
                                   PCONV_WL_BLD, PLOAD_IN_WL, &
                                  PSUMDIFIMP,           &
                                   PLW_RD_TO_WL, PLW_GD_TO_WL, PLW_HV_TO_WL,       &
                                   PLW_SN_TO_WL, PLW_WL_TO_WL, PLW_WIN_TO_WL )  
!   ##########################################################################
!
!!****  *ROAD_WALL_LAYER_E_BUDGET*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the evoultion of roads and walls surface temperatures
!         
!     
!!**  METHOD
!     ------
!
!    6 : equations for evolution of Ts_road and Ts_wall simultaneously
!        *************************************************************
!
!     dTw_k(t) / dt = 1/(dw_k*Cw_k) * (- 2*Kw_k-1*(Tw_k-Tw_k-1)/(dw_k-1 +dw_k) 
!                                      - 2*Kw_k  *(Tw_k-Tw_k+1)/(dw_k+1 +dw_k) )
!
!     dTw_1(t) / dt = 1/(dw_1*Cw_1) * (  Rn_w - H_w - LE_w 
!                                      - 2*Kw_1*(Tw_1-Tw_2)/(dw_1 +dw_2)       )
!
!
!       with
!
!   K*_k  = (d*_k+ d*_k+1)/(d*_k/k*_k+ d*_k+1/k*_k+1)
!
!   Rn_w = abs_Rg_w 
!  - sigma * emis_w                                                   * Ts_w**4 (t+dt)
!  +         emis_w                       *      SVF_w                * LWR
!  + sigma * emis_w * emis_r              *      SVF_w                * Ts_r**4 (t+dt)
!  + sigma * emis_w * emis_w              * (1-2*SVF_w)               * Ts_w**4 (t+dt)
!  +         emis_w            (1-emis_r) *      SVF_r  *      SVF_w  * LWR
!  +         emis_w            (1-emis_w) *      SVF_w  * (1-2*SVF_w) * LWR
!  + sigma * emis_w * emis_w * (1-emis_r) *      SVF_w  * (1-  SVF_r) * Ts_w**4 (t+dt)
!  + sigma * emis_w * emis_w * (1-emis_w) * (1-2*SVF_w) * (1-2*SVF_w) * Ts_w**4 (t+dt)
!  + sigma * emis_w * emis_r * (1-emis_w) *      SVF_w  * (1-2*SVF_w) * Ts_r**4 (t+dt)
!
!  H_w  = rho Cp CH V ( Ts_w (t+dt) - Ta_canyon )
!
!  LE_w = rho Lv CH V ( qs_w (t+dt) - qa_canyon )
!
!
! The system is implicited (or semi-implicited).
!
! ZIMPL=1    ---> implicit system
! ZIMPL=0.5  ---> semi-implicit system
! ZIMPL=0    ---> explicit system
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
!!      Original    23/01/98 
!!                  21/11/01 (V. Masson and A. Lemonsu) bug of latent flux
!!                           for very strong evaporation (all reservoir emptied
!!                           in one time-step)
!!                     02/11 (V. Masson) splits the routine for road and walls separately
!!                     01/12 (V. Masson) separates the 2 walls
!!                     09/12 (G. Pigeon) modif internal convective coef convection
!!                     10/12 (G. Pigeon) add solar heat gain of indoor wall
!!                     01/16 (E.Redon/A.Lemonsu) add high vegetation contribution
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
USE MODD_CHECK_TEB, ONLY : CHECK_TEB_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_CSTS,ONLY : XCPD, XSTEFAN
!
USE MODI_LAYER_E_BUDGET_GET_COEF
USE MODI_LAYER_E_BUDGET
USE MODE_CONV_DOE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
USE MODI_ABOR1_SFX
USE MODI_GET_LUOUT
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(CHECK_TEB_t), INTENT(INOUT) :: CT
!
CHARACTER(LEN=6), INTENT(IN)      :: HPROGRAM     ! program calling surf. schemes
REAL, DIMENSION(:,:), INTENT(INOUT) :: PT_WL    ! wall layers temperatures
REAL, DIMENSION(:),   INTENT(IN)  :: PTS_WL_B ! opposite wall surface temperature
REAL, DIMENSION(:),   INTENT(IN)  :: PTI_WL_B ! opposite wall internal temperature
REAL,               INTENT(IN)    :: PTSTEP       ! time step
REAL, DIMENSION(:), INTENT(IN)    :: PDN_RD     ! snow-covered fraction on roads
REAL, DIMENSION(:), INTENT(IN)    :: PRHOA        ! rho
REAL, DIMENSION(:), INTENT(IN)    :: PAC_WL     ! aerodynamical conductance [m/s]
!                                                 ! between wall and canyon
REAL, DIMENSION(:), INTENT(IN)    :: PAC_BLD      ! aerodynamical conductance
                                                  ! inside the building itself
REAL, DIMENSION(:), INTENT(IN)    :: PDIAG_TI_RF
REAL, DIMENSION(:), INTENT(IN)    :: PLW_RAD      ! atmospheric infrared radiation
REAL, DIMENSION(:), INTENT(IN)    :: PPS          ! pressure at the surface
REAL, DIMENSION(:), INTENT(IN)    :: PEXNS        ! surface Exner function
REAL, DIMENSION(:), INTENT(IN)    :: PABS_SW_WL ! absorbed solar radiation
REAL, DIMENSION(:), INTENT(IN)    :: PABS_LW_EXPLICIT_WL ! absorbed longwave radiation when explicit calculation
REAL, DIMENSION(:), INTENT(IN)    :: PT_CANYON    ! air canyon temperature
REAL, DIMENSION(:), INTENT(IN)    :: PTS_RD     ! road surface temperature
REAL, DIMENSION(:), INTENT(IN)    :: PTSNOW_RD  ! road snow temperature
REAL, DIMENSION(:), INTENT(IN)    :: PTS_GD   ! green area surface temperature
REAL, DIMENSION(:), INTENT(IN)    :: PTS_HV     ! high vegetation surface temperature
!
REAL, DIMENSION(:), INTENT(IN)    :: PLW_WA_TO_WB ! LW interactions wall  -> opposite wall
REAL, DIMENSION(:), INTENT(IN)    :: PLW_R_TO_W   ! LW interactions road -> wall ; DEF formulation 
REAL, DIMENSION(:), INTENT(IN)    :: PLW_G_TO_W   ! LW interactions garden -> wall ; DEF formulation
REAL, DIMENSION(:), INTENT(IN)    :: PLW_S_TO_W   ! LW interactions sky   -> wall 
REAL, DIMENSION(:), INTENT(IN)    :: PLW_HV_TO_W  ! Radiative heat transfer coeff high veg-wall [W K-1 m-2]
REAL, DIMENSION(:), INTENT(IN)    :: PLW_NR_TO_W  ! LW interactions road(snow) -> wall 
REAL, DIMENSION(:), INTENT(IN)    :: PLW_WIN_TO_W ! Radiative heat transfer coeff wall-window 
                                                  ! [W K-1 m-2]
!
REAL, DIMENSION(:), INTENT(OUT)   :: PFLX_BLD_WL! flux from bld to wall
REAL, DIMENSION(:), INTENT(INOUT) :: PDQS_WL    ! heat storage inside the wall 
REAL, DIMENSION(:), INTENT(OUT)   :: PABS_LW_WL ! absorbed infrared rad. [W m-2(wall)]
REAL, DIMENSION(:), INTENT(OUT)   :: PEMIT_LW_WL  ! LW flux emitted by the wall [W m-2(wall)]
REAL, DIMENSION(:), INTENT(OUT)   :: PH_WL      ! Sensible heat flux from wall to air [W/m2(wall)]
                                                  ! wall = facade - glazing
REAL, DIMENSION(:,:),   INTENT(IN)  :: PRADHT_IN     ! Indoor radiant heat transfer coefficient
                                                    ! [W K-1 m-2]
REAL, DIMENSION(:), INTENT(IN)    :: PRAD_RF_WL ! rad. fluxes from roof to wall [W m-2(roof)]
REAL, DIMENSION(:), INTENT(OUT)   :: PRAD_WL_WIN  ! rad. fluxes from wall to win  [W m-2(wall)]
REAL, DIMENSION(:,:), INTENT(OUT)   :: PRAD_WL_FL! rad. fluxes from wall to floor [W m-2(wall)]
REAL, DIMENSION(:,:), INTENT(OUT)   :: PRAD_WL_MA ! rad. fluxes from wall to mass [W m-2(wall)]
REAL, DIMENSION(:,:), INTENT(OUT)   :: PCONV_WL_BLD ! conv. fluxes from wall to bld [W m-2(wall)]

REAL, DIMENSION(:), INTENT(IN)    :: PLOAD_IN_WL  ! LOAD from solar heat gain + rad int. gains  W/m2 [Wall]
REAL, DIMENSION(:), INTENT(OUT)   :: PSUMDIFIMP     ! Total error made due to implicitation [W m-2(urb)]
!
REAL, DIMENSION(:), INTENT(OUT)   :: PLW_RD_TO_WL
REAL, DIMENSION(:), INTENT(OUT)   :: PLW_GD_TO_WL
REAL, DIMENSION(:), INTENT(OUT)   :: PLW_HV_TO_WL
REAL, DIMENSION(:), INTENT(OUT)   :: PLW_SN_TO_WL
REAL, DIMENSION(:), INTENT(OUT)   :: PLW_WL_TO_WL
REAL, DIMENSION(:), INTENT(OUT)   :: PLW_WIN_TO_WL
!
!*      0.2    declarations of local variables
!
!
REAL :: ZIMPL=1.0      ! implicit coefficient
REAL :: ZEXPL=0.0      ! explicit coefficient
!
REAL, DIMENSION(SIZE(PPS),SIZE(PT_WL,2)) ::  ZA,& ! lower diag.
                                               ZB,& ! main  diag.
                                               ZC,& ! upper diag.
                                               ZY   ! r.h.s.                       
!
REAL, DIMENSION(SIZE(PPS)) :: ZIMB_WL    ! wall residual energy imbalance for verification [W m-2]
REAL, DIMENSION(SIZE(PPS)) :: ZMTC_O_D_WL_IN
REAL, DIMENSION(SIZE(PPS)) :: ZDF_RD    ! Road snow free fraction
REAL, DIMENSION(SIZE(PPS)) :: ZRHO_ACF_W  ! rho * conductance
!                                         !     * snow-free f.
!
REAL, DIMENSION(SIZE(PPS)) :: ZDIF_RAD_WL_RF ! diff between the rad flux that should
! receive the wall from the roof and what it really receives
!
REAL, DIMENSION(SIZE(PPS)) :: ZDIF_RAD_WL_WL ! diff between the rad flux that should
! receive the wall from the wall and what it really receives
!
!
! thermal capacity times layer depth
REAL, DIMENSION(SIZE(PPS)) :: ZTS_WL       ! wall surface temperature
REAL, DIMENSION(SIZE(PPS)) :: ZTI_WL       ! wall indoor surface temperature
REAL, DIMENSION(SIZE(PPS)) :: ZTI_WL_OLD   ! wall indoor surface temperature, previous time step
REAL, DIMENSION(SIZE(PPS)) :: ZTI_WL_CONV  ! wall indoor surface temperature for conv. flux
REAL, DIMENSION(SIZE(PPS)) :: ZT_SKY         ! sky temperature [K]
!
REAL, DIMENSION(SIZE(PPS)) :: ZRAD_WL_RF     ! rad flux between the wall and the roof computed for the wall balance
REAL, DIMENSION(SIZE(PPS)) :: ZRAD_WL_WL     ! rad flux between the wall and the roof computed for the wall balance

REAL, DIMENSION(SIZE(PPS)) :: ZF_WL_WL       ! View factor wall-wall inside the building
REAL, DIMENSION(SIZE(PPS),BOP%NBEMCOMP) :: ZCHTC_IN_WL  ! indoor convective heat transfer coeff wall [W m-2 K-1]
REAL, DIMENSION(SIZE(PPS)) :: ZCHTC_IN_WL_EFF ! indoor convective heat transfer coeff wall [W m-2 K-1]
REAL, DIMENSION(SIZE(PPS)) :: ZCH_MULT_TI_BLD
REAL, DIMENSION(SIZE(PPS)) :: ZRAD_MULT_TFL
REAL, DIMENSION(SIZE(PPS)) :: ZRAD_MULT_TMA
REAL, DIMENSION(SIZE(PPS)) :: ZRADHT_EFF
REAL, DIMENSION(SIZE(PPS),BOP%NBEMCOMP) :: ZCOMP_RAD_WL_WIN 
REAL, DIMENSION(SIZE(PPS),BOP%NBEMCOMP) :: ZCOMP_RAD_WL_WL 
REAL, DIMENSION(SIZE(PPS)) :: ZCONV_WL_BLD
REAL, DIMENSION(SIZE(PPS)) :: ZRAD_WL_FL
REAL, DIMENSION(SIZE(PPS)) :: ZRAD_WL_MA
REAL :: ZIMP_LW
!
INTEGER :: IWL_LAYER           ! number of wall layers
INTEGER :: JJ                    ! loop counter
INTEGER :: JCOMP          ! loop counter
INTEGER :: ILUOUT                ! logical unit of output file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WALL_LAYER_E_BUDGET',0,ZHOOK_HANDLE)
!
! Multiplier for implicit calculation of longwave radiation
!
IF (TOP%LEXPLW) THEN
   ZIMP_LW=0.0
ELSE
   ZIMP_LW=1.0
ENDIF
!
PFLX_BLD_WL  = XUNDEF
ZIMB_WL      = XUNDEF
PRAD_WL_WIN  = XUNDEF
PRAD_WL_FL   = XUNDEF
PRAD_WL_MA   = XUNDEF
PCONV_WL_BLD = XUNDEF
!
IF (.NOT.TOP%LEXPLW) THEN 
   PLW_RD_TO_WL(:) = XUNDEF
   PLW_GD_TO_WL(:) = XUNDEF
   PLW_HV_TO_WL(:) = XUNDEF
   PLW_SN_TO_WL(:) = XUNDEF
   PLW_WL_TO_WL(:) = XUNDEF
   PLW_WIN_TO_WL(:)= XUNDEF
ENDIF
!
CALL LAYER_E_BUDGET_GET_COEF( PT_WL, PTSTEP, ZIMPL, T%XHC_WALL, T%XTC_WALL, T%XD_WALL, &
                              ZA, ZB, ZC, ZY )
!
IWL_LAYER = SIZE(PT_WL,2)
!
DO JJ=1,SIZE(PDN_RD)
  !
  ZDF_RD(JJ) = 1. - PDN_RD(JJ)
  !
  ZTS_WL    (JJ) = PT_WL(JJ,1)
  ZTI_WL_OLD(JJ) = PT_WL(JJ, IWL_LAYER)
  !
  !*      2.1    outdoor convective flux properties 
  !              ----------------------------------
  !
  ZRHO_ACF_W (JJ) = PRHOA(JJ) * PAC_WL(JJ)
  !
  !*      2.2    Sky temperature
  !              ---------------
  !
  ZT_SKY(JJ) = (PLW_RAD(JJ)/XSTEFAN)**0.25
  !  
  !*      2.3    indoor average thermal conductivity
  !              -----------------------------------
  !
  IF (TOP%CBEM .EQ. "DEF") THEN
    ZMTC_O_D_WL_IN(JJ) = 2. * T%XTC_WALL(JJ,IWL_LAYER) / T%XD_WALL (JJ,IWL_LAYER)
    ZMTC_O_D_WL_IN(JJ) = 1./(  1./ZMTC_O_D_WL_IN(JJ)  + 1./(XCPD*PRHOA(JJ)*PAC_BLD(JJ)))
  ENDIF
ENDDO  
  !  
  !*      2.4    indoor convective coefficient
  !              -----------------------------
  !
  DO JCOMP=1,SIZE(B%XFRACOMP,2)
     ZCHTC_IN_WL(:,JCOMP) = CHTC_VERT_DOE(PT_WL(:,IWL_LAYER), B%XTI_BLD(:,JCOMP))
     DO JJ=1,SIZE(B%XFRACOMP,1)
        ZCHTC_IN_WL(JJ,JCOMP) = MAX(1.,ZCHTC_IN_WL(JJ,JCOMP))
     ENDDO
  ENDDO
  !
  ! Calculation of effective coefficients in order to simplify the
  ! structure of the equation in the multi-compartiment case
  !
  IF (TOP%CBEM=="BEM") THEN
    !
    ZCHTC_IN_WL_EFF(:)   = 0.0
    ZRADHT_EFF(:)        = 0.0
    ZCH_MULT_TI_BLD(:)   = 0.0
    ZRAD_MULT_TFL(:)     = 0.0
    ZRAD_MULT_TMA(:)     = 0.0
    !
    DO JCOMP=1,BOP%NBEMCOMP
       ZRADHT_EFF(:)     = ZRADHT_EFF(:)        + B%XFRACOMP(:,JCOMP) * PRADHT_IN(:,JCOMP)
       ZCHTC_IN_WL_EFF(:)= ZCHTC_IN_WL_EFF(:)   + B%XFRACOMP(:,JCOMP) * ZCHTC_IN_WL(:,JCOMP)
       ZCH_MULT_TI_BLD(:)= ZCH_MULT_TI_BLD(:)   + B%XFRACOMP(:,JCOMP) * ZCHTC_IN_WL(:,JCOMP)   * B%XTI_BLD(:,JCOMP)
       ZRAD_MULT_TFL(:)  = ZRAD_MULT_TFL(:)     + B%XFRACOMP(:,JCOMP) * PRADHT_IN(:,JCOMP)     * B%XT_FLOOR(:,1,JCOMP)
       ZRAD_MULT_TMA(:)  = ZRAD_MULT_TMA(:)     + B%XFRACOMP(:,JCOMP) * PRADHT_IN(:,JCOMP)     * B%XT_MASS(:,1,JCOMP)
    ENDDO
  END IF
!
!-------------------------------------------------------------------------------
!
!*      3.    Outer wall layer coefficients
!             ------------------------------
!
DO JJ=1,SIZE(PT_WL,1)
  !
  ZB(JJ,1) = ZB(JJ,1) + ZIMPL * XCPD/PEXNS(JJ) * ZRHO_ACF_W(JJ)
  !
  ZY(JJ,1) = ZY(JJ,1) + PABS_SW_WL(JJ)                           &
                      + (1.0 -ZIMP_LW) * PABS_LW_EXPLICIT_WL(JJ) &
                      + XCPD/PEXNS(JJ) * ZRHO_ACF_W(JJ) *        &
                        ( PT_CANYON(JJ) - ZEXPL * ZTS_WL(JJ) )
  !
  ZB(JJ,1) = ZB(JJ,1)                                       &
    + ZIMPL * ZIMP_LW * ( PLW_S_TO_W(JJ) + PLW_WA_TO_WB(JJ) &
    + ZDF_RD(JJ)*PLW_R_TO_W(JJ)                             &
    + PLW_G_TO_W(JJ)                                        &
    + PLW_HV_TO_W(JJ)                                       &
    + PDN_RD(JJ) * PLW_NR_TO_W(JJ)                          &
    + PLW_WIN_TO_W(JJ) )    
  !
  ZY(JJ,1) = ZY(JJ,1) + ZIMP_LW * (                                            &
                        PLW_S_TO_W(JJ) * (ZT_SKY(JJ)    - ZEXPL * ZTS_WL(JJ))  &
                    + PLW_WA_TO_WB(JJ) * (PTS_WL_B(JJ)  - ZEXPL * ZTS_WL(JJ))  &
       + ZDF_RD(JJ) *  PLW_R_TO_W(JJ)  * (PTS_RD(JJ)    - ZEXPL * ZTS_WL(JJ))  &
       + PDN_RD(JJ) *  PLW_NR_TO_W(JJ) * (PTSNOW_RD(JJ) - ZEXPL * ZTS_WL(JJ))  &
                     + PLW_WIN_TO_W(JJ)* (B%XT_WIN1(JJ) - ZEXPL * ZTS_WL(JJ)) )
  !
  IF (SIZE(PTS_GD)>0) THEN
    ZY(JJ,1) = ZY(JJ,1) + ZIMP_LW * PLW_G_TO_W(JJ)  * (PTS_GD(JJ)   - ZEXPL * ZTS_WL(JJ))
  ENDIF 
    !
  IF (SIZE(PTS_HV)>0) THEN
    ZY(JJ,1) = ZY(JJ,1) + ZIMP_LW * PLW_HV_TO_W(JJ) * (PTS_HV(JJ)   - ZEXPL * ZTS_WL(JJ)) 
  ENDIF 
  !
ENDDO
!
!-------------------------------------------------------------------------------
!
!*      4.    Inside wall layer coefficients
!             -----------------------------
!
DO JJ=1,SIZE(PT_WL,1)
  !                
  IF (TOP%CBEM=="DEF") THEN
    !
    ZB(JJ,IWL_LAYER) = ZB(JJ,IWL_LAYER) + ZIMPL * ZMTC_O_D_WL_IN(JJ)
    !
    ZY(JJ,IWL_LAYER) = ZY(JJ,IWL_LAYER) &
                        + ZMTC_O_D_WL_IN(JJ) * B%XTI_BLD(JJ,1) &
                        - ZEXPL * ZMTC_O_D_WL_IN(JJ) * PT_WL(JJ,IWL_LAYER)
    !
  ELSEIF (TOP%CBEM=="BEM") THEN
    !
    ZF_WL_WL (JJ) = 1. - B%XF_WALL_MASS(JJ) - B%XF_WALL_WIN(JJ) - 2.*B%XF_WALL_FLOOR(JJ) 
    !
     ZB(JJ,IWL_LAYER) = ZB(JJ,IWL_LAYER) + ZIMPL *               &
                        (ZCHTC_IN_WL_EFF(JJ) * 4./3. + ZRADHT_EFF(JJ) *           &
                      (  B%XF_WALL_MASS(JJ) +     B%XF_WALL_WIN  (JJ) &
                          + ZF_WL_WL(JJ) + 2 * B%XF_WALL_FLOOR(JJ)))
    !
    ZY(JJ,IWL_LAYER) = ZY(JJ,IWL_LAYER) +                                         &
       ZCH_MULT_TI_BLD(JJ)                -                                       &
       1./3. * ZCHTC_IN_WL_EFF(JJ) * PT_WL(JJ, IWL_LAYER) * (4*ZEXPL-1) +         &
       ZRADHT_EFF(JJ) * (                                                         &
       B%XF_WALL_MASS (JJ) * (                 - ZEXPL * PT_WL(JJ,IWL_LAYER))   + &
       B%XF_WALL_WIN  (JJ) * (B%XT_WIN2 (JJ)   - ZEXPL * PT_WL(JJ,IWL_LAYER))   + &
       B%XF_WALL_FLOOR(JJ) * (                 - ZEXPL * PT_WL(JJ,IWL_LAYER))   + &
       ZF_WL_WL       (JJ) * (PTI_WL_B(JJ)     - ZEXPL * PT_WL(JJ,IWL_LAYER))   + &
       B%XF_WALL_FLOOR(JJ) * (PDIAG_TI_RF(JJ)  - ZEXPL * PT_WL(JJ,IWL_LAYER)) ) + &
       B%XF_WALL_MASS (JJ) * ZRAD_MULT_TMA(JJ)                                  + &
       B%XF_WALL_FLOOR(JJ) * ZRAD_MULT_TFL(JJ)                                  + &
       PLOAD_IN_WL(JJ)
    !
  ENDIF
  !
END DO
!
!-------------------------------------------------------------------------------
!
!*      5.    heat conduction calculation
!             ---------------------------
!
 CALL LAYER_E_BUDGET( PT_WL, PTSTEP, ZIMPL, T%XHC_WALL, T%XTC_WALL, T%XD_WALL, &
                     ZA, ZB, ZC, ZY, PDQS_WL )
!
!-------------------------------------------------------------------------------
!
!*   6.   diagnostics of flux echanged with the wall
!         ------------------------------------------
!
!
!* radiative surface temperature used during the energy balance
ZTS_WL(:) = ZIMPL * PT_WL(:,1) + ZEXPL * ZTS_WL(:)
!
IF (.NOT.TOP%LEXPLW) THEN
   !
   PABS_LW_WL(:) = PLW_S_TO_W  (:) * (ZT_SKY   (:) - ZTS_WL(:)) + &
        ZDF_RD(:) *PLW_R_TO_W  (:) * (PTS_RD   (:) - ZTS_WL(:)) + &
                   PLW_WA_TO_WB(:) * (PTS_WL_B (:) - ZTS_WL(:)) + &
                   PLW_WIN_TO_W(:) * (B%XT_WIN1(:) - ZTS_WL(:)) + &
        PDN_RD(:) *PLW_NR_TO_W (:) * (PTSNOW_RD(:) - ZTS_WL(:))
   !
   IF (SIZE(PTS_GD)>0) THEN
      PABS_LW_WL(:) = PABS_LW_WL(:) + PLW_G_TO_W  (:) * (PTS_GD   (:) - ZTS_WL(:))
   ENDIF
   !
   IF (SIZE(PTS_HV)>0) THEN
      PABS_LW_WL(:) = PABS_LW_WL(:) + PLW_HV_TO_W (:) * (PTS_HV   (:) - ZTS_WL(:))
   ENDIF
   !
   ! Diagnostics for longwave radiation
   ! exchanges between the different surfaces (W/m²(urb))
   !
   PLW_RD_TO_WL (:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)*ZDF_RD(:)*PLW_R_TO_W(:) *(PTS_RD(:)   -ZTS_WL(:))
   PLW_SN_TO_WL (:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)*PDN_RD(:)*PLW_NR_TO_W(:)*(PTSNOW_RD(:)-ZTS_WL(:))
   PLW_WL_TO_WL (:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)*PLW_WA_TO_WB(:)         *(PTS_WL_B(:) -ZTS_WL(:))
   PLW_WIN_TO_WL(:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)*PLW_WIN_TO_W(:)         *(B%XT_WIN1(:)-ZTS_WL(:))
   !
   IF (SIZE(PTS_GD)>0) THEN
      PLW_GD_TO_WL (:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)*PLW_G_TO_W(:)         *(PTS_GD(:)   -ZTS_WL(:))
   END IF
   !
   IF (SIZE(PTS_HV)>0) THEN
      PLW_HV_TO_WL (:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)*PLW_HV_TO_W(:)        *(PTS_HV(:)   -ZTS_WL(:))
   END IF
   !
ELSE
   PABS_LW_WL(:) = PABS_LW_EXPLICIT_WL(:)
ENDIF
!
! Emitted lw flux
!
IF (TOP%LSPARTACUS) THEN
   PEMIT_LW_WL(:) = XUNDEF
ELSE
   PEMIT_LW_WL(:) = XSTEFAN * PT_WL(:,1)**4 + &
        (1.0 - T%XEMIS_WALL(:))/T%XEMIS_WALL(:) * PABS_LW_WL(:)
ENDIF
!
!* sensible heat flux to outdoor
!
PH_WL(:) = ZRHO_ACF_W(:) * XCPD/PEXNS(:) *  &
             ( ZIMPL*PT_WL(:,1) + ZEXPL*ZTS_WL(:) - PT_CANYON(:) )
!
! The temperature at the inside layer of the wall actually used
!
ZTI_WL(:) = ZEXPL * ZTI_WL_OLD(:) + ZIMPL * PT_WL(:,IWL_LAYER)
!
IF (TOP%CBEM=='DEF') THEN
   PFLX_BLD_WL(:) = ZMTC_O_D_WL_IN(:) * (B%XTI_BLD(:,1) - ZTI_WL(:))
ENDIF
!
IF (TOP%CBEM=='BEM') THEN
    !
    !compute ZTI_WALL used in flux calculation
    ZTI_WL_CONV(:) = 4./3. * ZIMPL * PT_WL(:,IWL_LAYER) + 1./3. * ZTI_WL_OLD(:) * (4 * ZEXPL -1.)
    !
    ZRAD_WL_RF(:)=ZRADHT_EFF(:)*(ZTI_WL(:)-PDIAG_TI_RF(:))
    !
    ! Calculate energy imbalance due to impliciation [W/m²(urb)]
    !
    ! Robert: The energy imbalance terms need to be multiplied by
    !         the respective view factors
    !
    ! - Wall surface temperature at previous time step
    ZDIF_RAD_WL_WL(:) = (T%XWALL_O_BLD(:)*T%XBLD(:))*ZF_WL_WL(:)*ZRADHT_EFF(:)*(ZTI_WL(:)-ZTI_WL_OLD(:))
    !
    ! - wall and roof surface temperature
    ZDIF_RAD_WL_RF(:) = T%XBLD(:)*B%XF_FLOOR_WALL(:)*(ZRAD_WL_RF(:) + PRAD_RF_WL(:))
    !
    ! Calculate sum of the energy imbalance due to implicitation (W/m²(urb))
    ! This sum will be added to the waste heat flux
    !
    PSUMDIFIMP(:)=ZDIF_RAD_WL_WL(:)+ZDIF_RAD_WL_RF(:)
    !
    !compute exchanged fluxes with other surfaces for which the balance is done after
    !
    ZRAD_WL_FL   (:) = 0.0
    ZRAD_WL_MA   (:) = 0.0
    PRAD_WL_WIN  (:) = 0.0
    ZRAD_WL_WL   (:) = 0.0
    ZCONV_WL_BLD (:) = 0.0
    !     
    DO JCOMP=1,SIZE(B%XFRACOMP,2)
       !
       PRAD_WL_FL        (:,JCOMP) = PRADHT_IN(:,JCOMP)     * ( ZTI_WL(:)      - B%XT_FLOOR(:,1,JCOMP)  )
       PRAD_WL_MA        (:,JCOMP) = PRADHT_IN(:,JCOMP)     * ( ZTI_WL(:)      - B%XT_MASS (:,1,JCOMP))
       ZCOMP_RAD_WL_WIN  (:,JCOMP) = PRADHT_IN(:,JCOMP)     * ( ZTI_WL(:)      - B%XT_WIN2 (:)        )
       ZCOMP_RAD_WL_WL   (:,JCOMP) = PRADHT_IN(:,JCOMP)     * ( ZTI_WL(:)      - PTI_WL_B(:)          )
       PCONV_WL_BLD      (:,JCOMP) = ZCHTC_IN_WL(:,JCOMP) * ( ZTI_WL_CONV(:) - B%XTI_BLD (:,JCOMP)  )
       !
       ZRAD_WL_FL  (:) = ZRAD_WL_FL   (:) + B%XFRACOMP(:,JCOMP) * PRAD_WL_FL      (:,JCOMP)
       ZRAD_WL_MA  (:) = ZRAD_WL_MA   (:) + B%XFRACOMP(:,JCOMP) * PRAD_WL_MA      (:,JCOMP)
       PRAD_WL_WIN (:) = PRAD_WL_WIN  (:) + B%XFRACOMP(:,JCOMP) * ZCOMP_RAD_WL_WIN(:,JCOMP)
       ZRAD_WL_WL  (:) = ZRAD_WL_WL   (:) + B%XFRACOMP(:,JCOMP) * ZCOMP_RAD_WL_WL (:,JCOMP)
       ZCONV_WL_BLD(:) = ZCONV_WL_BLD (:) + B%XFRACOMP(:,JCOMP) * PCONV_WL_BLD    (:,JCOMP)
       !
    ENDDO
    !
    PFLX_BLD_WL  (:) =                       - &
       B%XF_WALL_MASS (:) * ZRAD_WL_MA   (:) - &
       B%XF_WALL_WIN  (:) * PRAD_WL_WIN  (:) - &
       B%XF_WALL_FLOOR(:) * ZRAD_WL_FL   (:) - &
       ZF_WL_WL       (:) * ZRAD_WL_WL   (:) - &
       B%XF_WALL_FLOOR(:) * ZRAD_WL_RF   (:) - &
       ZCONV_WL_BLD(:)                       + &
       PLOAD_IN_WL(:)
    !
    ZIMB_WL(:) = PABS_SW_WL(:) + &
       PABS_LW_WL(:) - &
       PDQS_WL(:)    - &
       PH_WL(:)      + &
       PFLX_BLD_WL(:)
    !
   DO JJ=1,SIZE(ZIMB_WL)
      !
      IF (ISNAN(ZIMB_WL(JJ))) CALL ABOR1_SFX("NAN detected in wall_layer_e_budget")
      !
      IF (CT%LCHECK_TEB .AND. (ABS(ZIMB_WL(JJ)).GT.CT%XCHECK_PROCESS)) THEN
         !
         CALL GET_LUOUT(HPROGRAM,ILUOUT)
         !
         WRITE(ILUOUT,*) "                         "
         WRITE(ILUOUT,*) "In wall_layer_e_budget : "
         WRITE(ILUOUT,*) "JJ                     : ",JJ
         WRITE(ILUOUT,*) "ABS_SW (W/m²(wall))    : ",PABS_SW_WL(JJ)
         WRITE(ILUOUT,*) "ABS_LW (W/m²(wall))    : ",PABS_LW_WL(JJ)
         WRITE(ILUOUT,*) "DQS    (W/m²(wall))    : ",PDQS_WL(JJ)
         WRITE(ILUOUT,*) "H      (W/m²(wall))    : ",PH_WL(JJ)
         WRITE(ILUOUT,*) "FLXBLD (W/m²(wall))    : ",PFLX_BLD_WL(JJ)
         WRITE(ILUOUT,*) "---------------------------------------------"
         WRITE(ILUOUT,*) "ZIMB   (W/m²(wall))    : ",ZIMB_WL(JJ)
         CALL FLUSH(ILUOUT)
         !
         CALL ABOR1_SFX('Too large energy budget imbalance of wall')
         !
      ENDIF
   ENDDO
ELSE
   PSUMDIFIMP(:)=0.0
ENDIF
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WALL_LAYER_E_BUDGET',1,ZHOOK_HANDLE)
!
END SUBROUTINE WALL_LAYER_E_BUDGET
