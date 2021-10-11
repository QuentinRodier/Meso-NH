!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE ROOF_LAYER_E_BUDGET(TOP, BOP, T, B, CT, DMT, HPROGRAM, PQSAT_ROOF, PAC_BLD, PTSTEP, PDN_ROOF,   &
                                   PRHOA, PAC_ROOF, PAC_ROOF_WAT, PLW_RAD, PPS,        &
                                   PDELT_ROOF, PTA, PQA, PEXNA, PEXNS, PABS_SW_ROOF,   &
                                   PGSNOW_ROOF, PFLX_BLD_ROOF, PDQS_ROOF, PABS_LW_ROOF,&
                                   PHFREE_ROOF, PLEFREE_ROOF,                          &
                                   PG_GREENROOF_ROOF, PRADHT_IN, PTS_FLOOR, PTI_WALL,  &
                                   PRAD_ROOF_WALL, PRAD_ROOF_WIN, PRAD_ROOF_FLOOR,     &
                                   PRAD_ROOF_MASS, PCONV_ROOF_BLD, PRR, PLOAD_IN_ROOF,&
                                   PLEFLIM_ROOF, PDIAG_TI_ROOF                      )
!   ##########################################################################
!
!!****  *ROOF_LAYER_E_BUDGET*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the evoultion of surface temperature of roofs
!         
!     
!!**  METHOD
!     ------
!
!
!
!
!    5 : equation for evolution of Ts_roof
!        *********************************
!
!     dTt_1(t) / dt = 1/(dt_1*Ct_1) * (  Rn - H - LE
!                                      - 2*Kt_1*(Tt_1-Tt_2)/(dt_1 +dt_2)       )
!
!     dTt_k(t) / dt = 1/(dt_k*Ct_k) * (- 2*Kt_k-1*(Tt_k-Tt_k-1)/(dt_k-1 +dt_k) 
!                                      - 2*Kt_k  *(Tt_k-Tt_k+1)/(dt_k+1 +dt_k) )
!
!       with
!
!       K*_k  = (d*_k+ d*_k+1)/(d*_k/k*_k+ d*_k+1/k*_k+1)
!
!       Rn = (dir_Rg + sca_Rg) (1-a) + emis * ( Rat - sigma Ts**4 (t+dt) )
!
!       H  = rho Cp CH V ( Ts (t+dt) - Tas )
!
!       LE = rho Lv CH V ( qs (t+dt) - qas )
!
!      where the as subscript denotes atmospheric values at ground level
!      (and not at first half level)
!
!      The tridiagonal systel is linearized with
!
!       using      Ts**4 (t+dt) = Ts**4 (t) + 4*Ts**3 (t) * ( Ts(t+dt) - Ts(t) )
!
!       and  qs (t+dt) = Hu(t) * qsat(t) + Hu(t) dqsat/dT * ( Ts(t+dt) - Ts(t) )
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
!!                  17/10/05 (G. Pigeon) computation of storage inside the roofs
!!                  26/04/12 (G. Pigeon) add term of heating of rain (new arg PRR+XCL)
!!                     09/12 (G. Pigeon) modif of indoor conv. coef and implicit calculation
!!                     10/12 (G. Pigeon) add indoor solar heat load
!!                     02/19 (V. Masson) bug in sensible heat of rain when snow is present
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
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
!
USE MODD_SURF_PAR,  ONLY : XUNDEF
USE MODD_CSTS,ONLY : XCPD, XLVTT, XSTEFAN, XCL
!
USE MODE_THERMOS
!
USE MODI_LAYER_E_BUDGET
USE MODI_LAYER_E_BUDGET_GET_COEF
USE MODE_CONV_DOE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
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
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DMT
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM     ! program calling
REAL, DIMENSION(:), INTENT(INOUT) :: PQSAT_ROOF     ! q_sat(Ts)
REAL, DIMENSION(:), INTENT(IN)    :: PAC_BLD        ! aerodynamical resistance
                                                    ! inside building itself
REAL,               INTENT(IN)    :: PTSTEP         ! time step
REAL, DIMENSION(:), INTENT(IN)    :: PDN_ROOF       ! roof snow fraction
REAL, DIMENSION(:), INTENT(IN)    :: PRHOA          ! air density
REAL, DIMENSION(:), INTENT(IN)    :: PAC_ROOF       ! aerodynamical conductance
REAL, DIMENSION(:), INTENT(IN)    :: PAC_ROOF_WAT   ! aerodynamical conductance (for water)
REAL, DIMENSION(:), INTENT(IN)    :: PLW_RAD        ! atmospheric infrared radiation
REAL, DIMENSION(:), INTENT(IN)    :: PPS            ! pressure at the surface
REAL, DIMENSION(:), INTENT(IN)    :: PDELT_ROOF     ! fraction of water on roof
REAL, DIMENSION(:), INTENT(IN)    :: PTA            ! air temperature at roof level
REAL, DIMENSION(:), INTENT(IN)    :: PQA            ! air specific humidity
                                                    ! at roof level
REAL, DIMENSION(:), INTENT(IN)    :: PEXNA          ! exner function
REAL, DIMENSION(:), INTENT(IN)    :: PEXNS          ! surface exner function
REAL, DIMENSION(:), INTENT(IN)    :: PABS_SW_ROOF   ! absorbed solar radiation
REAL, DIMENSION(:), INTENT(IN)    :: PGSNOW_ROOF    ! roof snow conduction
!                                                   ! heat fluxes at mantel
!                                                   ! base
REAL, DIMENSION(:), INTENT(IN)    :: PG_GREENROOF_ROOF ! heat conduction flux
!                                                        between greenroof and
!                                                        structural roof
REAL, DIMENSION(:), INTENT(OUT)   :: PFLX_BLD_ROOF  ! flux from bld to roof
REAL, DIMENSION(:), INTENT(OUT)   :: PDQS_ROOF      ! heat storage inside the roofs
REAL, DIMENSION(:), INTENT(OUT)   :: PABS_LW_ROOF   ! absorbed infra-red rad.
REAL, DIMENSION(:), INTENT(OUT)   :: PHFREE_ROOF    ! sensible heat flux of the
                                                    ! snow free part of the roof
REAL, DIMENSION(:), INTENT(OUT)   :: PLEFREE_ROOF   ! latent heat flux of the
                                                    ! snow free part of the roof
REAL, DIMENSION(:,:), INTENT(IN)  :: PRADHT_IN      ! Indoor radiant heat transfer coefficient
                                                    ! [W K-1 m-2]
REAL, DIMENSION(:,:), INTENT(IN)  :: PTS_FLOOR      ! surf. floor temp. (contact with bld air)
REAL, DIMENSION(:), INTENT(IN)    :: PTI_WALL       ! indoor wall temp.
REAL, DIMENSION(:), INTENT(OUT)   :: PRAD_ROOF_WALL ! rad. fluxes from roof to wall [W m-2(roof)]
REAL, DIMENSION(:), INTENT(OUT)   :: PRAD_ROOF_WIN  ! rad. fluxes from roof to win [W m-2(roof)]
REAL, DIMENSION(:,:), INTENT(OUT)   :: PRAD_ROOF_FLOOR! rad. fluxes from roof to floor [W m-2(roof)]
REAL, DIMENSION(:,:), INTENT(OUT)   :: PRAD_ROOF_MASS ! rad. fluxes from roof to mass [W m-2(roof)]
REAL, DIMENSION(:,:), INTENT(OUT)   :: PCONV_ROOF_BLD ! conv. fluxes from roof to bld [W m-2(roof)]
REAL, DIMENSION(:), INTENT(IN)    :: PRR ! rain rate [kg m-2 s-1]
REAL, DIMENSION(:), INTENT(IN)    :: PLOAD_IN_ROOF ! solar + int heat gain on roof W/m2 [roof]
REAL, DIMENSION(:), INTENT(OUT)   :: PLEFLIM_ROOF   ! Excess latent heat flux put into the waste heat flux [W m-2(roof)]
REAL, DIMENSION(:), INTENT(OUT)   :: PDIAG_TI_ROOF
!
!*      0.2    declarations of local variables
!
REAL :: ZIMPL = 1.0     ! implicit coefficient
REAL :: ZEXPL = 0.0     ! explicit coefficient
REAL :: ZIMP_LW = 1.0   ! implicit calculation of longwave radiation
!
REAL, DIMENSION(SIZE(PTA)) :: ZDF_ROOF ! snow-free fraction
REAL, DIMENSION(SIZE(PTA),SIZE(T%XT_ROOF,2)) :: ZA,& ! lower diag.
                                              ZB,& ! main  diag.
                                              ZC,& ! upper diag.
                                              ZY   ! r.h.s.
!
REAL, DIMENSION(SIZE(PTA)) :: ZDQSAT_ROOF      ! dq_sat/dTs
REAL, DIMENSION(SIZE(PTA)) :: ZRHO_ACF_ROOF    ! conductance * rho
REAL, DIMENSION(SIZE(PTA)) :: ZRHO_ACF_ROOF_WAT! conductance * rho (for water)
REAL, DIMENSION(SIZE(PTA)) :: ZMTC_O_D_ROOF_IN ! thermal capacity times layer depth
REAL, DIMENSION(SIZE(PTA)) :: ZTS_ROOF         ! roof surface temperature at previous time step
REAL, DIMENSION(SIZE(PTA)) :: ZTRAD_ROOF       ! roof radiative surface temperature at intermediate time step
REAL, DIMENSION(SIZE(PTA)) :: ZTAER_ROOF       ! roof aerodyn. surface temperature at intermediate time step
REAL, DIMENSION(SIZE(PTA)) :: ZTI_ROOF         ! temperature of internal roof layer used for radiative exchanges
REAL, DIMENSION(SIZE(PTA)) :: ZTI_ROOF_CONV    ! temperature of internal roof layer used for convective exchanges
REAL, DIMENSION(SIZE(PTA),BOP%NBEMCOMP) :: ZCHTC_IN_ROOF      ! Indoor roof convec heat transfer coefficient
                                                            ! [W K-1 m-2(bld)]
!
REAL, DIMENSION(SIZE(PTA)) :: ZCHTC_IN_ROOF_EFF
REAL, DIMENSION(SIZE(PTA)) :: ZCH_MULT_TI_BLD
REAL, DIMENSION(SIZE(PTA)) :: ZRADHT_EFF
REAL, DIMENSION(SIZE(PTA)) :: ZRAD_MULT_TFLOOR
REAL, DIMENSION(SIZE(PTA)) :: ZRAD_MULT_TMASS
REAL, DIMENSION(SIZE(PTA)) :: ZRAD_ROOF_FLOOR
REAL, DIMENSION(SIZE(PTA)) :: ZRAD_ROOF_MASS
REAL, DIMENSION(SIZE(PTA)) :: ZCONV_ROOF_BLD
REAL, DIMENSION(SIZE(PTA)) :: ZHEAT_RR_ROOF  ! heat used too cool/heat the rain from the roof [W m-2(roof)]
REAL, DIMENSION(SIZE(PPS),BOP%NBEMCOMP) :: ZCOMP_RAD_ROOF_WIN 
REAL, DIMENSION(SIZE(PPS),BOP%NBEMCOMP) :: ZCOMP_RAD_ROOF_WALL 
REAL, DIMENSION(SIZE(PTA)) :: ZWATROOFMAX       ! Maximum available roof water [W m-2 (bld)]
REAL, DIMENSION(SIZE(PTA)) :: ZIMB_ROOF         ! residual energy imbalance of the roof for
!
INTEGER :: JJ
INTEGER :: IROOF_LAYER ! number of roof layers
INTEGER :: JCOMP       ! compartment counter
INTEGER :: ILUOUT      ! logical unit of output file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ROOF_LAYER_E_BUDGET',0,ZHOOK_HANDLE)
!
! Multiplier for implicit calculation of longwave radiation
!
IF (TOP%LEXPLW) THEN
   ZIMP_LW=0.0
ELSE
   ZIMP_LW=1.0
ENDIF
!
! Calculate maximum water on roof available for evaporation (unit W/m²).
! The latent heat flux must not exceed this value.
!
ZWATROOFMAX(:)=XLVTT*T%XWS_ROOF(:)/PTSTEP
!
PRAD_ROOF_WALL(:) = XUNDEF
PRAD_ROOF_WIN(:)  = XUNDEF
PRAD_ROOF_FLOOR(:,:)= XUNDEF
PRAD_ROOF_MASS(:,:) = XUNDEF
PCONV_ROOF_BLD(:,:) = XUNDEF
!
! *Convection heat transfer coefficients [W m-2 K-1] from EP Engineering Reference
!
IROOF_LAYER = SIZE(T%XT_ROOF,2)
!
DO JCOMP=1,SIZE(B%XFRACOMP,2)
   ZCHTC_IN_ROOF(:,JCOMP) = CHTC_DOWN_DOE(T%XT_ROOF(:,IROOF_LAYER), B%XTI_BLD(:,JCOMP))
   DO JJ=1,SIZE(B%XFRACOMP,1)
      ZCHTC_IN_ROOF(JJ,JCOMP) = MAX(1.,ZCHTC_IN_ROOF(JJ,JCOMP))
   ENDDO
ENDDO
!
! Calculation of effective coefficients in order to simplify the
! structure of the equation in the multi-compartment case
!
IF (TOP%CBEM=='BEM') THEN
  ZCHTC_IN_ROOF_EFF(:) = 0.0
  ZRADHT_EFF(:)        = 0.0
  ZCH_MULT_TI_BLD(:)   = 0.0
  ZRAD_MULT_TFLOOR(:)  = 0.0
  ZRAD_MULT_TMASS(:)   = 0.0
!
  DO JCOMP=1,SIZE(B%XFRACOMP,2)
     ZCHTC_IN_ROOF_EFF(:) = ZCHTC_IN_ROOF_EFF(:) + B%XFRACOMP(:,JCOMP) * ZCHTC_IN_ROOF(:,JCOMP)
     ZRADHT_EFF(:)        = ZRADHT_EFF(:)        + B%XFRACOMP(:,JCOMP) * PRADHT_IN(:,JCOMP)
     ZCH_MULT_TI_BLD(:)   = ZCH_MULT_TI_BLD(:)  + B%XFRACOMP(:,JCOMP) * ZCHTC_IN_ROOF(:,JCOMP) * B%XTI_BLD(:,JCOMP)
     ZRAD_MULT_TFLOOR(:)  = ZRAD_MULT_TFLOOR(:) + B%XFRACOMP(:,JCOMP) * PRADHT_IN(:,JCOMP) * PTS_FLOOR(:,JCOMP)
     ZRAD_MULT_TMASS(:)   = ZRAD_MULT_TMASS(:)  + B%XFRACOMP(:,JCOMP) * PRADHT_IN(:,JCOMP) * B%XT_MASS (:,1,JCOMP)
  ENDDO
END IF
!
 CALL LAYER_E_BUDGET_GET_COEF(T%XT_ROOF, PTSTEP, ZIMPL, T%XHC_ROOF, T%XTC_ROOF, T%XD_ROOF, &
                              ZA, ZB, ZC, ZY )
!
!
DO JJ=1,SIZE(PDN_ROOF)
  !
  ZDF_ROOF(JJ) = 1. - PDN_ROOF(JJ)
  !
  ZTS_ROOF(JJ) = T%XT_ROOF(JJ,1)
  ZTI_ROOF(JJ) = T%XT_ROOF(JJ,IROOF_LAYER)
  !
  !*      2.     Roof Ts coefficients
  !              --------------------
  !
  ZRHO_ACF_ROOF    (JJ) = PRHOA(JJ) * PAC_ROOF    (JJ)
  ZRHO_ACF_ROOF_WAT(JJ) = PRHOA(JJ) * PAC_ROOF_WAT(JJ)
  !
  IF (TOP%CBEM .EQ. 'DEF') THEN
    ZMTC_O_D_ROOF_IN(JJ) = 2. * T%XTC_ROOF(JJ,IROOF_LAYER) / T%XD_ROOF (JJ,IROOF_LAYER)
    ZMTC_O_D_ROOF_IN(JJ) = 1./(  1./ZMTC_O_D_ROOF_IN(JJ) + 1./(XCPD*PRHOA(JJ)*PAC_BLD(JJ)) ) 
  ENDIF
  !
ENDDO
!
!*      2.1    dqsat/dTs, and humidity for roofs
!              ---------------------------------
!
ZDQSAT_ROOF(:) = DQSAT(ZTS_ROOF(:),PPS(:),PQSAT_ROOF(:))
!
!*      2.2    coefficients
!              ------------
! 
DO JJ=1,SIZE(T%XT_ROOF,1)
  !
  ZB(JJ,1) = ZB(JJ,1) + ZDF_ROOF(JJ) * (1.-T%XGREENROOF(JJ)) * (                           &
                        ZIMPL * ( XCPD/PEXNS(JJ) * ZRHO_ACF_ROOF(JJ)                       &
                        + XLVTT * ZRHO_ACF_ROOF_WAT(JJ) * PDELT_ROOF(JJ) * ZDQSAT_ROOF(JJ) &
                        + ZIMP_LW * XSTEFAN * T%XEMIS_ROOF(JJ) * 4.*ZTS_ROOF(JJ)**3))      &
                        +  ZIMPL * PRR(JJ) * XCL  !! heating/cooling of rain 
  !
  ZY(JJ,1) = ZY(JJ,1) + (1.-T%XGREENROOF(JJ))                                                            &
                      * (PDN_ROOF(JJ)*PGSNOW_ROOF(JJ) + ZDF_ROOF(JJ) * ( PABS_SW_ROOF(JJ)                &
                        + (1.0 - ZIMP_LW) * DMT%XABS_LW_ROOF(JJ)                                         &
                        + XCPD * ZRHO_ACF_ROOF(JJ) * ( PTA(JJ)/PEXNA(JJ) - ZEXPL*ZTS_ROOF(JJ)/PEXNS(JJ)) &
                        + ZIMP_LW * T%XEMIS_ROOF(JJ) * PLW_RAD(JJ)                                       &
                        + XLVTT * ZRHO_ACF_ROOF_WAT(JJ) * PDELT_ROOF(JJ)                                 &
                          * ( PQA(JJ) - PQSAT_ROOF(JJ) + ZIMPL * ZDQSAT_ROOF(JJ) * ZTS_ROOF(JJ) )        &
                        + ZIMP_LW * XSTEFAN * T%XEMIS_ROOF(JJ) * ZTS_ROOF(JJ)**4 * ( 3.*ZIMPL-ZEXPL ) )) &
                        + PRR(JJ) * XCL * (PTA(JJ) - ZEXPL * ZTS_ROOF(JJ)) & !! heating/cooling of rain
                        + T%XGREENROOF(JJ)*PG_GREENROOF_ROOF(JJ)
  !
  IF (TOP%CBEM=="DEF") THEN
    !
    ZB(JJ,IROOF_LAYER) = ZB(JJ,IROOF_LAYER) + ZIMPL * ZMTC_O_D_ROOF_IN(JJ)
    !
    ZY(JJ,IROOF_LAYER) = ZY(JJ,IROOF_LAYER) &
                         + ZMTC_O_D_ROOF_IN(JJ) * B%XTI_BLD(JJ,1) &
                         - ZEXPL * ZMTC_O_D_ROOF_IN(JJ) * T%XT_ROOF(JJ,IROOF_LAYER)
    !
  ELSEIF (TOP%CBEM=="BEM") THEN
    !
    ZB(JJ, IROOF_LAYER) = ZB(JJ,IROOF_LAYER) + ZIMPL * &
                         (ZCHTC_IN_ROOF_EFF(JJ) * 4./3. + ZRADHT_EFF(JJ) * &
                         (B%XF_FLOOR_MASS(JJ) + B%XF_FLOOR_WIN(JJ) + &
                          B%XF_FLOOR_WALL(JJ) + B%XF_FLOOR_ROOF(JJ) ))

    ZY(JJ,IROOF_LAYER) = ZY(JJ,IROOF_LAYER) + &
       ZCH_MULT_TI_BLD(JJ)                - &
       1./3. * ZCHTC_IN_ROOF_EFF(JJ) * T%XT_ROOF(JJ,IROOF_LAYER)*(4*ZEXPL - 1.) + &
       ZRADHT_EFF(JJ) * (                                                    &
          B%XF_FLOOR_WIN (JJ) * (B%XT_WIN2 (JJ) - ZEXPL * T%XT_ROOF(JJ,IROOF_LAYER))   +   &
          B%XF_FLOOR_WALL(JJ) * (PTI_WALL(JJ)   - ZEXPL * T%XT_ROOF(JJ,IROOF_LAYER))   +   &
          B%XF_FLOOR_MASS(JJ) * (               - ZEXPL * T%XT_ROOF(JJ,IROOF_LAYER))   +   &
          B%XF_FLOOR_ROOF(JJ) * (               - ZEXPL * T%XT_ROOF(JJ,IROOF_LAYER)) ) +   &
          B%XF_FLOOR_MASS(JJ) * ZRAD_MULT_TMASS (JJ)                                   +   &
          B%XF_FLOOR_ROOF(JJ) * ZRAD_MULT_TFLOOR(JJ)                                   +   &
          PLOAD_IN_ROOF(JJ)
    !
  ENDIF
  !
ENDDO
!
!
 CALL LAYER_E_BUDGET( T%XT_ROOF, PTSTEP, ZIMPL, T%XHC_ROOF, T%XTC_ROOF, T%XD_ROOF, &
                     ZA, ZB, ZC, ZY, PDQS_ROOF )
!
!-------------------------------------------------------------------------------
!
!*     diagnostic: computation of flux between bld and internal roof layer
DO JJ=1,SIZE(T%XT_ROOF,1)
  !
  ZTI_ROOF_CONV(JJ) = 4./3. * ZIMPL * T%XT_ROOF(JJ, IROOF_LAYER) + 1./3. * ZTI_ROOF(JJ) * (4*ZEXPL -1.)
  ZTI_ROOF(JJ) = ZEXPL * ZTI_ROOF(JJ) + ZIMPL * T%XT_ROOF(JJ, IROOF_LAYER) 
  SELECT CASE(TOP%CBEM)
  CASE("DEF")
     PFLX_BLD_ROOF(JJ) = ZMTC_O_D_ROOF_IN(JJ) * (B%XTI_BLD(JJ,1) - ZTI_ROOF(JJ))
  CASE("BEM")
   !
   ! Robert:
   ! The fluxes are diagnosed separately for the different compartments
   !
   PDIAG_TI_ROOF(:) = ZTI_ROOF(:)
   !
   PRAD_ROOF_WALL(JJ)  = 0.0
   PRAD_ROOF_WIN(JJ)   = 0.0
   ZRAD_ROOF_FLOOR(JJ) = 0.0
   ZRAD_ROOF_MASS(JJ)  = 0.0
   ZCONV_ROOF_BLD(JJ)  = 0.0
   !
   DO JCOMP=1,BOP%NBEMCOMP
      !
      ZCOMP_RAD_ROOF_WALL(JJ,JCOMP)  = PRADHT_IN(JJ,JCOMP)     * ( ZTI_ROOF(JJ)      - PTI_WALL (JJ) )
      ZCOMP_RAD_ROOF_WIN(JJ,JCOMP)   = PRADHT_IN(JJ,JCOMP)     * ( ZTI_ROOF(JJ)      - B%XT_WIN2  (JJ) )
      PRAD_ROOF_FLOOR(JJ,JCOMP)      = PRADHT_IN(JJ,JCOMP)     * ( ZTI_ROOF(JJ)      - PTS_FLOOR(JJ,JCOMP) )
      PRAD_ROOF_MASS(JJ,JCOMP)       = PRADHT_IN(JJ,JCOMP)     * ( ZTI_ROOF(JJ)      - B%XT_MASS(JJ,1,JCOMP) )
      PCONV_ROOF_BLD(JJ,JCOMP)       = ZCHTC_IN_ROOF(JJ,JCOMP) * ( ZTI_ROOF_CONV(JJ) - B%XTI_BLD(JJ,JCOMP) )
      !
      ZRAD_ROOF_FLOOR(JJ) = ZRAD_ROOF_FLOOR(JJ) + B%XFRACOMP(JJ,JCOMP) * PRAD_ROOF_FLOOR(JJ,JCOMP)
      ZRAD_ROOF_MASS(JJ)  = ZRAD_ROOF_MASS(JJ)  + B%XFRACOMP(JJ,JCOMP) * PRAD_ROOF_MASS(JJ,JCOMP)
      PRAD_ROOF_WALL(JJ)  = PRAD_ROOF_WALL(JJ)  + B%XFRACOMP(JJ,JCOMP) * ZCOMP_RAD_ROOF_WALL(JJ,JCOMP)
      PRAD_ROOF_WIN(JJ)   = PRAD_ROOF_WIN(JJ)   + B%XFRACOMP(JJ,JCOMP) * ZCOMP_RAD_ROOF_WIN(JJ,JCOMP)
      ZCONV_ROOF_BLD(JJ)  = ZCONV_ROOF_BLD(JJ)  + B%XFRACOMP(JJ,JCOMP) * PCONV_ROOF_BLD(JJ,JCOMP)
      !
   ENDDO
   !
   ! Robert: The radiative fluxes need to be multiplied 
   !         with the respective view factors
   !
   PFLX_BLD_ROOF(JJ)  =                         - &
                 B%XF_FLOOR_WALL(JJ) * PRAD_ROOF_WALL(JJ)  - &
                 B%XF_FLOOR_WIN (JJ) * PRAD_ROOF_WIN(JJ)   - &
                 B%XF_FLOOR_ROOF(JJ) * ZRAD_ROOF_FLOOR(JJ) - &
                 B%XF_FLOOR_MASS(JJ) * ZRAD_ROOF_MASS(JJ)  - &
                 ZCONV_ROOF_BLD(JJ)                      + &
                 PLOAD_IN_ROOF(JJ) 
   !
  ENDSELECT
  !
  !*      8.     Infra-red radiation absorbed by roofs
  !              -------------------------------------
  !
  IF (TOP%LEXPLW) THEN
     PABS_LW_ROOF(JJ) = DMT%XABS_LW_ROOF(JJ)
  ELSE
     !
     ZTRAD_ROOF(JJ) = ( ZTS_ROOF(JJ)**4 + &
                     4.*ZIMPL*ZTS_ROOF(JJ)**3 * (T%XT_ROOF(JJ,1) - ZTS_ROOF(JJ)) )**0.25
     !
     !* absorbed LW
     PABS_LW_ROOF(JJ) = T%XEMIS_ROOF(JJ) * (PLW_RAD(JJ) - XSTEFAN * ZTRAD_ROOF(JJ)** 4)
     !
  ENDIF
  !
  !*      9.     Sensible heat flux between snow free roof and air
  !              -------------------------------------------------
  !
  !* aerodynamic surface temperature at the intermediate time step
  ZTAER_ROOF(JJ) = ZEXPL * ZTS_ROOF(JJ) + ZIMPL * T%XT_ROOF(JJ,1)
  PHFREE_ROOF(JJ) = ZRHO_ACF_ROOF(JJ) * XCPD * &
                   ( ZTAER_ROOF(JJ)/PEXNS(JJ) - PTA(JJ)/PEXNA(JJ) )
  !
  ZHEAT_RR_ROOF(JJ) = PRR(JJ) * XCL * (ZTAER_ROOF(JJ) - PTA(JJ))
  !
  !*      10.     Latent heat flux between snow free roof and air
  !              -------------------------------------------------
  !
  PLEFREE_ROOF(JJ) = ZRHO_ACF_ROOF_WAT(JJ) * XLVTT * PDELT_ROOF(JJ) * &
                     ( PQSAT_ROOF(JJ) - PQA(JJ) +                     &
                       ZIMPL * ZDQSAT_ROOF(JJ) * (T%XT_ROOF(JJ,1) - ZTS_ROOF(JJ)) ) 

  ! Limitation of PLEFREE_ROOF(JJ) to the maximum 
  ! available water on the snow-free roof
  ! The excess latent heat flux is added to the waste heat flux
  !
  IF (PLEFREE_ROOF(JJ).GT.ZWATROOFMAX(JJ)) THEN
     PLEFLIM_ROOF(JJ)=PLEFREE_ROOF(JJ)-ZWATROOFMAX(JJ)
     PLEFREE_ROOF(JJ)=ZWATROOFMAX(JJ)
  ELSE
     PLEFLIM_ROOF(JJ)=0.0
  ENDIF
  !
  !
  !      13.     Energy imbalance for verification
  !              ---------------------------------
  !
  IF (CT%LCHECK_TEB) CT%XHEAT_RR_ROOF = ZHEAT_RR_ROOF
  !
  ZIMB_ROOF(JJ) = (1.-T%XGREENROOF(JJ)) * ZDF_ROOF (JJ) * PABS_SW_ROOF(JJ) + &
                  (1.-T%XGREENROOF(JJ)) * ZDF_ROOF (JJ) * PABS_LW_ROOF(JJ) - &
                  PDQS_ROOF(JJ)                                            - &
                  (1.-T%XGREENROOF(JJ)) * ZDF_ROOF (JJ) * PHFREE_ROOF(JJ)  - &
                  (1.-T%XGREENROOF(JJ)) * ZDF_ROOF (JJ) * PLEFLIM_ROOF(JJ) - &
                  (1.-T%XGREENROOF(JJ)) * ZDF_ROOF (JJ) * PLEFREE_ROOF(JJ) + &
                  (1.-T%XGREENROOF(JJ)) * PDN_ROOF (JJ) * PGSNOW_ROOF(JJ)  + &
                  PFLX_BLD_ROOF(JJ)                                        - &
                  ZHEAT_RR_ROOF(JJ)                                        + &
                  T%XGREENROOF(JJ)      * PG_GREENROOF_ROOF(JJ)
  !
  IF ( ISNAN(ZIMB_ROOF(JJ)) .OR. (ABS(ZIMB_ROOF(JJ)).GT.1.0E-6) ) THEN
     !
     CALL GET_LUOUT(HPROGRAM,ILUOUT)
     !
     WRITE(ILUOUT,*) "                                       "
     WRITE(ILUOUT,*) "In roof_layer_e_budget :               "
     WRITE(ILUOUT,*) "JJ                     : ",JJ
     WRITE(ILUOUT,*) "ABS_SW (W/m²(roof))    : ",(1.-T%XGREENROOF(JJ)) * ZDF_ROOF (JJ) * PABS_SW_ROOF(JJ)
     WRITE(ILUOUT,*) "ABS_LW (W/m²(roof))    : ",(1.-T%XGREENROOF(JJ)) * ZDF_ROOF (JJ) * PABS_LW_ROOF(JJ)
     WRITE(ILUOUT,*) "DQS    (W/m²(roof))    : ",PDQS_ROOF(JJ)
     WRITE(ILUOUT,*) "HFREE  (W/m²(roof))    : ",(1.-T%XGREENROOF(JJ)) * ZDF_ROOF (JJ) * PHFREE_ROOF(JJ)
     WRITE(ILUOUT,*) "LEFREE (W/m²(roof))    : ",(1.-T%XGREENROOF(JJ)) * ZDF_ROOF (JJ) * PLEFREE_ROOF(JJ) 
     WRITE(ILUOUT,*) "LELIM  (W/m²(roof))    : ",(1.-T%XGREENROOF(JJ)) * ZDF_ROOF (JJ) * PLEFLIM_ROOF(JJ)
     WRITE(ILUOUT,*) "GSNOW  (W/m²(roof))    : ",(1.-T%XGREENROOF(JJ)) * PDN_ROOF (JJ) * PGSNOW_ROOF(JJ)
     WRITE(ILUOUT,*) "FLXBLD (W/m²(roof))    : ",PFLX_BLD_ROOF(JJ)
     WRITE(ILUOUT,*) "HEATRR (W/m²(roof))    : ",ZHEAT_RR_ROOF(JJ)
     WRITE(ILUOUT,*) "FLXGRE (W/m²(roof))    : ",T%XGREENROOF(JJ)*PG_GREENROOF_ROOF(JJ)
     WRITE(ILUOUT,*) "---------------------------------------------"
     WRITE(ILUOUT,*) "ZIMB (W/m²(roof))      : ",ZIMB_ROOF(JJ)
     CALL FLUSH(ILUOUT)
     !
     CALL ABOR1_SFX('NAN values or too large energy budget imbalance of roof, check report')
     !
  ENDIF
ENDDO
!
!*      11.     New saturated specified humidity near the roof surface
!              ------------------------------------------------------
!
PQSAT_ROOF(:) =  QSAT(T%XT_ROOF(:,1),PPS(:))
!
!-------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ROOF_LAYER_E_BUDGET',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------
!
END SUBROUTINE ROOF_LAYER_E_BUDGET
