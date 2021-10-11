!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE WINDOW_E_BUDGET(T, B, CT, TOP, DMT, PEMIS_WIN, PLW_W_TO_WIN,      &
     PLW_R_TO_WIN, PLW_G_TO_WIN, PLW_NR_TO_WIN, PLW_S_TO_WIN, PLW_HV_TO_WIN, &
     PRAD_RF_WIN, PRAD_WL_WIN, PABS_SW_WIN, PLW_RAD, PAC_WIN, PRADHT_IN,     &
     PRHOA, PDN_RD, PT_CANYON, PTS_WL, PTS_RD, PTSN_RD, PTS_GD, PTS_HV,      &
     PRAD_WIN_FL, PRAD_WIN_MA, PCONV_WIN_BLD, PEMIT_LW_WIN, PLOAD_IN_WIN )
!
!#########################################################################################################
!
!!****  *WINDOW_E_BUDGET*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the evoultion of window temperature
!         
!     
!!**  METHOD
!     ------
!
! window is supposed double pane with no thermal capacity
! a steady state energy balance is applied to both layers that exchanges U_WIN(T1 - T2)
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
!!      B. Bueno           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2011 
!!      G. Pigeon   10/11/2011 exteranlized window balance from wall_layer_e_budget to window_e_budget and use fluxes from layers for
!                              which the balance has been computed before (roof and wall) and returns the fluxes to the compartments
!                              computed afterwards (floor, mass, bld)
!!      G. Pigeon      09/2012 new indoor conv. coef
!!      G. Pigeon      10/2012 separate abs from outdoor on both side of window
!!                            + add loads for indoor face of the window
!!      E.Redon/A.Lemonsu   01/16 add high vegetation contribution
!!      V. Masson   04.2020 completes energy check for high vegetation IR exchanges & implicitation bug with high vegetation
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_CHECK_TEB, ONLY : CHECK_TEB_t
USE MODD_CSTS,ONLY : XCPD, XSTEFAN
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_PAR, ONLY: XEMIS_WIN_CST
!
USE MODE_CONV_DOE
!
USE YOMHOOK, ONLY : LHOOK, DR_HOOK
USE PARKIND1, ONLY : JPRB
!
  IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(CHECK_TEB_t), INTENT(INOUT) :: CT
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DMT
!
REAL, DIMENSION(:), INTENT(IN) :: PEMIS_WIN    ! window emissivity
REAL, DIMENSION(:), INTENT(IN) :: PLW_W_TO_WIN ! Radiative heat transfer coeff window-wall outdoor [W K-1 m-2] 
REAL, DIMENSION(:), INTENT(IN) :: PLW_R_TO_WIN ! Radiative heat transfer coeff window-road  [W K-1 m-2]
REAL, DIMENSION(:), INTENT(IN) :: PLW_G_TO_WIN ! Radiative heat transfer coeff window-garden
REAL, DIMENSION(:), INTENT(IN) :: PLW_NR_TO_WIN! Radiative heat transfer coeff window-snow
REAL, DIMENSION(:), INTENT(IN) :: PLW_S_TO_WIN ! Radiative heat transfer coeff window-sky  [W K-1 m-2]
REAL, DIMENSION(:), INTENT(IN) :: PLW_HV_TO_WIN! Radiative heat transfer coeff window-high veg
REAL, DIMENSION(:), INTENT(IN) :: PRAD_RF_WIN  ! rad. fluxes from roof to win [W m-2(roof)]
REAL, DIMENSION(:), INTENT(IN) :: PRAD_WL_WIN  ! rad. fluxes from wall to win [W m-2(roof)]
REAL, DIMENSION(:), INTENT(IN) :: PABS_SW_WIN  ! window absorbed shortwave radiation [W m-2]
REAL, DIMENSION(:), INTENT(IN) :: PLW_RAD      ! atmospheric infrared radiation
REAL, DIMENSION(:), INTENT(IN)  :: PAC_WIN     ! aerodynamical conductance between window and canyon
REAL, DIMENSION(:,:), INTENT(IN) :: PRADHT_IN  ! Indoor radiant heat transfer coefficient [W K-1 m-2]
REAL, DIMENSION(:), INTENT(IN) :: PRHOA        ! rho
REAL, DIMENSION(:), INTENT(IN) :: PDN_RD       ! snow-covered fraction on roads
REAL, DIMENSION(:), INTENT(IN) :: PT_CANYON    ! air canyon temperature
REAL, DIMENSION(:), INTENT(IN) :: PTS_WL       ! wall outdoor surface temperature
REAL, DIMENSION(:), INTENT(IN) :: PTS_RD       ! road surface temperature
REAL, DIMENSION(:), INTENT(IN) :: PTSN_RD      ! road snow temperature
REAL, DIMENSION(:), INTENT(IN) :: PTS_GD       ! green area surface temperature
REAL, DIMENSION(:), INTENT(IN)  :: PTS_HV      ! high veg surface temperature
REAL, DIMENSION(:,:), INTENT(OUT) :: PRAD_WIN_FL    ! rad. fluxes from window to floor [W m-2(window)]
REAL, DIMENSION(:,:), INTENT(OUT) :: PRAD_WIN_MA    ! rad. fluxes from window to mass [W m-2(window)]
REAL, DIMENSION(:,:), INTENT(OUT) :: PCONV_WIN_BLD  ! conv. fluxes from window to bld [W m-2(window)]
REAL, DIMENSION(:), INTENT(OUT) :: PEMIT_LW_WIN     ! Longwave radiation emitted by the window [W m-2(window)]
REAL, DIMENSION(:), INTENT(IN) :: PLOAD_IN_WIN      ! solar + internal heat gain 
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PT_CANYON)) :: ZT_SKY         ! sky temperature [K]
REAL, DIMENSION(SIZE(PT_CANYON)) :: ZA11           !
REAL, DIMENSION(SIZE(PT_CANYON)) :: ZA12           ! 
REAL, DIMENSION(SIZE(PT_CANYON)) :: ZA21           !
REAL, DIMENSION(SIZE(PT_CANYON)) :: ZA22           !
REAL, DIMENSION(SIZE(PT_CANYON)) :: ZB1            ! calculations
REAL, DIMENSION(SIZE(PT_CANYON)) :: ZB2            ! auxiliar variables for window temperature
REAL, DIMENSION(SIZE(PT_CANYON),SIZE(PRADHT_IN,2)) :: ZCHTC_IN_WIN   ! indoor convective heat transfer coef. window [W m-2 K-1]
REAL, DIMENSION(SIZE(PT_CANYON)) :: ZCHTC_IN_WIN_EFF
REAL, DIMENSION(SIZE(PT_CANYON)) :: ZRADHT_EFF
REAL, DIMENSION(SIZE(PT_CANYON)) :: ZCH_MULT_TI_BLD
REAL, DIMENSION(SIZE(PT_CANYON)) :: ZRAD_MULT_TFL
REAL, DIMENSION(SIZE(PT_CANYON)) :: ZRAD_MULT_TMA
REAL, DIMENSION(SIZE(PT_CANYON)) :: ZT_WIN1
REAL, DIMENSION(SIZE(PT_CANYON)) :: ZLW_IMPL_COEF
!
INTEGER :: JJ
INTEGER :: JCOMP
!
!*      Preliminaries
!       -------------
!
! Convective heat transfer coefficient between indoor air and internal part of window 
!
DO JCOMP=1,SIZE(B%XFRACOMP,2)
   ZCHTC_IN_WIN(:,JCOMP) = CHTC_VERT_DOE(B%XT_WIN2(:),B%XTI_BLD(:,JCOMP))
   DO JJ=1,SIZE(B%XFRACOMP,1)
      ZCHTC_IN_WIN(JJ,JCOMP) = MAX(1.,ZCHTC_IN_WIN(JJ,JCOMP))
   ENDDO
ENDDO
!
! Calculation of effective coefficients in order to simplify the
! structure of the equation in the multi-compartiment case
!
ZCHTC_IN_WIN_EFF(:) = 0.0
ZRADHT_EFF(:)       = 0.0
ZCH_MULT_TI_BLD(:)  = 0.0
ZRAD_MULT_TFL(:)    = 0.0
ZRAD_MULT_TMA(:)    = 0.0
!
DO JCOMP=1,SIZE(B%XFRACOMP,2)
   ZCHTC_IN_WIN_EFF(:) = ZCHTC_IN_WIN_EFF(:) + B%XFRACOMP(:,JCOMP) * ZCHTC_IN_WIN(:,JCOMP)
   ZRADHT_EFF(:)       = ZRADHT_EFF(:)       + B%XFRACOMP(:,JCOMP) * PRADHT_IN(:,JCOMP)
   ZCH_MULT_TI_BLD(:)  = ZCH_MULT_TI_BLD(:)  + B%XFRACOMP(:,JCOMP) * ZCHTC_IN_WIN(:,JCOMP) * B%XTI_BLD(:,JCOMP)
   ZRAD_MULT_TFL  (:)  = ZRAD_MULT_TFL  (:)  + B%XFRACOMP(:,JCOMP) * PRADHT_IN(:,JCOMP)    * B%XT_FLOOR(:,1,JCOMP)
   ZRAD_MULT_TMA  (:)  = ZRAD_MULT_TMA  (:)  + B%XFRACOMP(:,JCOMP) * PRADHT_IN(:,JCOMP)    * B%XT_MASS (:,1,JCOMP)
ENDDO
!
! Sky temperature
!
ZT_SKY(:) = (PLW_RAD(:)/XSTEFAN)**0.25
!
! Explicit calculation of longwave absorption by the window
! in the case it has not yet been done for all longwave exchanges
!
IF (.NOT.TOP%LEXPLW) THEN
   CALL ABS_LW_WIN
ENDIF
!
! Twin1 at the beginning of the time step
!
ZT_WIN1 = B%XT_WIN1
!
!----------------------------------------------------------------------------------------------------
!
! LW absorbed radiation by the window at equilibirum state at end of time step will be estimated as:
!
! DMT%XABS_LW_WIN - 4. * XEMIS_WIN_CST * XSTEFAN * ZT_WIN1**3 * (B%XT_WIN1 - ZT_WIN1)
!
! This corresponds to the linearization of the absorbed radiation, supposing that only emmited radiation depends 
! on the surface temperature of the window, with the window emissivity of XEMIS_WIN_CST.
!
ZLW_IMPL_COEF = 4. * XEMIS_WIN_CST * XSTEFAN * ZT_WIN1**3 
!
! The system of 2 coupled equations is written as follows
!
! T1*a11 + T2*a12 = b1
! T1*a21 + T2*a22 = b1
!
! so A*(T1,T2) = (b1,b2)
!
! Definition of the matrix and vector coefficients
!
ZA11(:) = - PAC_WIN(:) * PRHOA(:) * XCPD &
          - B%XUGG_WIN(:)                &
          - ZLW_IMPL_COEF
!
ZA12 = B%XUGG_WIN(:)
ZA21 = B%XUGG_WIN(:)
!
ZA22(:) = - ZCHTC_IN_WIN_EFF(:)               &
          - B%XUGG_WIN(:)                     &
          - ZRADHT_EFF(:) * B%XF_WIN_MASS (:) &
          - ZRADHT_EFF(:) * B%XF_WIN_FLOOR(:) 
!
ZB1(:)  = - PAC_WIN(:) * PRHOA(:) * XCPD * PT_CANYON(:)  &
          - PABS_SW_WIN  (:) /2.                         &
          - DMT%XABS_LW_WIN(:) - ZLW_IMPL_COEF * ZT_WIN1 
!
ZB2(:) = - ZCH_MULT_TI_BLD(:)                   &
         - PABS_SW_WIN(:) /2.                   &
         - PLOAD_IN_WIN(:)                      &
         - B%XF_WIN_MASS(:)  * ZRAD_MULT_TMA(:) &
         - B%XF_WIN_WALL(:)  * PRAD_WL_WIN(:)   &
         - B%XF_WIN_FLOOR(:) * PRAD_RF_WIN(:)   &
         - B%XF_WIN_FLOOR(:) * ZRAD_MULT_TFL(:)
!
! The matrix A is then inverted and multiplied with the vector B
! to yield the solution for T1 and T2.
!
! (T1,T2) = A-1 * (b1,b2)
!
! Outdoor window surface temperature
!
B%XT_WIN1(:) = (ZA22*ZB1-ZA12*ZB2)/(ZA11*ZA22-ZA12*ZA21)
!
! Indoor window surface temperature
!
B%XT_WIN2(:) = (-ZA21*ZB1+ZA11*ZB2)/(ZA11*ZA22-ZA12*ZA21)
!
! Diagnose and correct violation of energy conservation
!
DMT%XABS_LW_WIN = DMT%XABS_LW_WIN - ZLW_IMPL_COEF * (B%XT_WIN1 - ZT_WIN1)
!
! Outdoor infrared radiation emited by the window
!
IF (TOP%LSPARTACUS) THEN
   PEMIT_LW_WIN(:) = XUNDEF
ELSE
   PEMIT_LW_WIN(:) = XSTEFAN * B%XT_WIN1(:)**4 + (1 - PEMIS_WIN(:))/PEMIS_WIN(:) * DMT%XABS_LW_WIN(:)
ENDIF
!
! Radiative and convective exchanges
! The fluxes need to be diagnosed separately for the different compartments
!
DO JCOMP=1,SIZE(B%XFRACOMP,2)
   PCONV_WIN_BLD (:,JCOMP) = ZCHTC_IN_WIN(:,JCOMP) * ( B%XT_WIN2(:) - B%XTI_BLD (:,JCOMP) )
   PRAD_WIN_FL   (:,JCOMP) = PRADHT_IN   (:,JCOMP) * ( B%XT_WIN2(:) - B%XT_FLOOR(:,1,JCOMP) )
   PRAD_WIN_MA   (:,JCOMP) = PRADHT_IN   (:,JCOMP) * ( B%XT_WIN2(:) - B%XT_MASS (:,1,JCOMP) )
ENDDO
!
CONTAINS
!
SUBROUTINE ABS_LW_WIN
!
  DMT%XABS_LW_WIN(:)    = PLW_S_TO_WIN (:) * (ZT_SKY (:) - B%XT_WIN1(:)) + &
        (1.-PDN_RD(:)) *  PLW_R_TO_WIN (:) * (PTS_RD (:) - B%XT_WIN1(:)) + &
                          PLW_W_TO_WIN (:) * (PTS_WL (:) - B%XT_WIN1(:)) + &
              PDN_RD(:) * PLW_NR_TO_WIN(:) * (PTSN_RD(:) - B%XT_WIN1(:))
!
  IF (SIZE(PTS_GD)>0) THEN
     DMT%XABS_LW_WIN(:) = DMT%XABS_LW_WIN(:) +  PLW_G_TO_WIN (:) * (PTS_GD (:) - B%XT_WIN1(:))      
  ENDIF
!
  IF (SIZE(PTS_HV)>0) THEN
     DMT%XABS_LW_WIN(:) = DMT%XABS_LW_WIN(:) +  PLW_HV_TO_WIN(:) * (PTS_HV (:) - B%XT_WIN1(:))      
  ENDIF
!
! Diagnostics for longwave radiation exchanges between the different surfaces (W/m²(urb))
!
  IF (CT%LCHECK_TEB) THEN
     CT%XLW_ROAD_TO_WIND (:)=T%XWALL_O_HOR(:)*B%XGR(:)*(1.-PDN_RD(:))*PLW_R_TO_WIN(:) *(PTS_RD(:) -B%XT_WIN1(:))
     CT%XLW_SNOW_TO_WIND (:)=T%XWALL_O_HOR(:)*B%XGR(:)*PDN_RD(:)     *PLW_NR_TO_WIN(:)*(PTSN_RD(:)-B%XT_WIN1(:))
     CT%XLW_WALL_TO_WIND (:)=T%XWALL_O_HOR(:)*B%XGR(:)               *PLW_W_TO_WIN(:) *(PTS_WL(:) -B%XT_WIN1(:))
  ENDIF
!
  IF (SIZE(PTS_GD)>0) THEN
     IF (CT%LCHECK_TEB) CT%XLW_GARD_TO_WIND (:)=T%XWALL_O_HOR(:)*B%XGR(:)*PLW_G_TO_WIN(:) *(PTS_GD(:)-B%XT_WIN1(:))
  ENDIF
!
  IF (SIZE(PTS_HV)>0) THEN
     IF (CT%LCHECK_TEB) CT%XLW_HV_TO_WIND (:)=T%XWALL_O_HOR(:)*B%XGR(:)*PLW_HV_TO_WIN(:) *(PTS_HV(:)-B%XT_WIN1(:))
  ENDIF
!
END SUBROUTINE ABS_LW_WIN
! 
END SUBROUTINE WINDOW_E_BUDGET
