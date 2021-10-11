!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   ##########################################################################
    SUBROUTINE MASS_LAYER_E_BUDGET(B, CT, HPROGRAM, PTSTEP, KCOMP, PFLX_BLD_MA, PDQS_MA, PRADHT_IN, &
                                   PRAD_WL_MA, PRAD_RF_MA, PRAD_WIN_MA, PLOAD_MA,       &
                                   PRAD_FL_MA, PCONV_MA_BLD                  )
!   ##########################################################################
!
!!****  *MASS_LAYER_E_BUDGET*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the evoultion of building floor temperatures
!         
!     
!!**  METHOD
!     ------
!
!    6 : equations for evolution of Ts_floor 
!        *************************************************************
!
!     dTf_k(t) / dt = 1/(df_k*Cf_k) * (- 2*Kf_k-1*(Tf_k-Tf_k-1)/(df_k-1 +df_k) 
!                                      - 2*Kf_k  *(Tf_k-Tf_k+1)/(df_k+1 +df_k) )
!
!     dTf_1(t) / dt = 1/(df_1*Cf_1) * (- 2*Kw_1*(Tw_1-Tw_2)/(dw_1 +dw_2))
!
!       with
!
!   K*_k  = (d*_k+ d*_k+1)/(d*_k/k*_k+ d*_k+1/k*_k+1)
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
!!      G. Pigeon           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    11/11
!!      G. Pigeon   09/12 modif internal convection coef
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_CHECK_TEB, ONLY : CHECK_TEB_t
!
USE MODI_LAYER_E_BUDGET_GET_COEF
USE MODI_LAYER_E_BUDGET
USE MODE_CONV_DOE
USE MODI_GET_LUOUT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(CHECK_TEB_t), INTENT(INOUT) :: CT
!
CHARACTER(LEN=6), INTENT(IN)        :: HPROGRAM     ! program calling surf. schemes
REAL,                 INTENT(IN)    :: PTSTEP       ! time step
INTEGER,              INTENT(IN)    :: KCOMP        ! compartiment index
REAL, DIMENSION(:),   INTENT(OUT)  :: PFLX_BLD_MA !flux from building to floor
REAL, DIMENSION(:),   INTENT(OUT) :: PDQS_MA !heat storage inside the floor
REAL, DIMENSION(:), INTENT(IN)    :: PRADHT_IN      ! Indoor radiant heat transfer coefficient
                                                    ! [W K-1 m-2]
REAL, DIMENSION(:), INTENT(IN)    :: PRAD_RF_MA ! rad. fluxes from roof to floor[W m-2(roof)]
REAL, DIMENSION(:), INTENT(IN)    :: PRAD_WL_MA ! rad. fluxes from wall to floor[W m-2(wall)]
REAL, DIMENSION(:), INTENT(IN)    :: PRAD_WIN_MA  ! rad. fluxes from win to floor[W m-2(win)]
REAL, DIMENSION(:), INTENT(IN)    :: PLOAD_MA ! solar and internal load to the floor
REAL, DIMENSION(:), INTENT(IN)    :: PRAD_FL_MA  ! rad. fluxes from floor to mass [W m-2(floor)]
REAL, DIMENSION(:), INTENT(OUT)   :: PCONV_MA_BLD  ! conv. fluxes from floor to bld [W m-2(floor)]
!
!*      0.2    declarations of local variables
!
!
REAL :: ZIMPL=1.0      ! implicit coefficient
REAL :: ZEXPL=0.0      ! explicit coefficient
!
REAL, DIMENSION(SIZE(B%XT_MASS,1),SIZE(B%XT_MASS,2)) :: ZA,& ! lower diag.
                                                      ZB,& ! main  diag.
                                                      ZC,& ! upper diag.
                                                      ZY   ! r.h.s.
!
REAL, DIMENSION(SIZE(B%XT_MASS,1)) :: ZIMB_MA !floor energy residual imbalance for verification
REAL, DIMENSION(SIZE(B%XT_MASS,1)) :: ZTS_MA  ! surf. mass temp.
                                              ! used during calculation
REAL, DIMENSION(SIZE(B%XT_MASS,1)) :: ZTS_MA_CONV  ! surf. mass temp. used for conv flux
REAL, DIMENSION(SIZE(B%XT_MASS,1)) :: ZCHTC_IN_MA ! Indoor floor convec heat transfer coefficient
                                                ! [W K-1 m-2(bld)]
INTEGER :: JJ
INTEGER :: ILUOUT  ! Logical unit of output file

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MASS_LAYER_E_BUDGET',0,ZHOOK_HANDLE)
!
! *Convection heat transfer coefficients [W m-2 K-1] from EP Engineering Reference
!
ZCHTC_IN_MA(:) = CHTC_VERT_DOE(B%XT_MASS(:,1,KCOMP), B%XTI_BLD(:,KCOMP)) 
DO JJ=1,SIZE(ZCHTC_IN_MA)
   ZCHTC_IN_MA(JJ) = MAX(1., ZCHTC_IN_MA(JJ))
ENDDO
!
 CALL LAYER_E_BUDGET_GET_COEF( B%XT_MASS(:,:,KCOMP), PTSTEP, ZIMPL, B%XHC_MASS, B%XTC_MASS, B%XD_MASS, &
                              ZA, ZB, ZC, ZY )
!
ZTS_MA(:) = B%XT_MASS(:,1,KCOMP) 

ZB(:,1) = ZB(:,1) + ZIMPL * 4./3. * ZCHTC_IN_MA(:)

ZY(:,1) = ZY(:,1)  &
   + ZCHTC_IN_MA(:) * (B%XTI_BLD(:,KCOMP) - 1./3. * B%XT_MASS(:, 1,KCOMP) * (4 * ZEXPL -1.))  &
   + B%XF_MASS_WIN  (:) * PRAD_WIN_MA(:) + B%XF_MASS_WALL (:) * PRAD_WL_MA(:)     &
   + B%XF_MASS_FLOOR (:) * (PRAD_RF_MA(:) +PRAD_FL_MA(:)) + PLOAD_MA(:)
!
 CALL LAYER_E_BUDGET( B%XT_MASS(:,:,KCOMP), PTSTEP, ZIMPL, B%XHC_MASS, B%XTC_MASS, B%XD_MASS, &
                     ZA, ZB, ZC, ZY, PDQS_MA )
!
!*      calculation of temperature used in energy balance calculation
!       -------------------------------------------------------------
!
ZTS_MA_CONV(:) = ZIMPL * 4./3. * B%XT_MASS(:,1,KCOMP) +1./3 * ZTS_MA(:) * (4 * ZEXPL -1.)
ZTS_MA(:) = ZEXPL * ZTS_MA(:) + ZIMPL * B%XT_MASS(:,1,KCOMP)
!
!*      calculation of convection flux between mass and building air
!       ------------------------------------------------------------
!
PCONV_MA_BLD(:) = ZCHTC_IN_MA(:) * (ZTS_MA_CONV(:) - B%XTI_BLD(:,KCOMP))
!
!*      For diagnostics calculation of flux exchanged between the mass and the
!       indoor
!       ------------------------------------------------
!
PFLX_BLD_MA(:) = - PCONV_MA_BLD(:) + B%XF_MASS_WIN  (:) * PRAD_WIN_MA(:)  &
       + B%XF_MASS_WALL (:) * PRAD_WL_MA(:)                               &
       + B%XF_MASS_FLOOR (:) * (PRAD_RF_MA(:) + PRAD_FL_MA(:))  + PLOAD_MA(:)
!
!*      Floor residual energy imbalance for verification
!       ------------------------------------------------
!
ZIMB_MA(:) = PFLX_BLD_MA(:) - PDQS_MA(:)
!
DO JJ=1,SIZE(ZIMB_MA)
   !
   IF (ISNAN(ZIMB_MA(JJ))) CALL ABOR1_SFX("NAN detected in mass_layer_e_budget")
   !
   IF (ABS(ZIMB_MA(JJ)).GT.CT%XCHECK_PROCESS) THEN
      !
      CALL GET_LUOUT(HPROGRAM,ILUOUT)
      !
      WRITE(ILUOUT,*) "                                       "
      WRITE(ILUOUT,*) "In mass_layer_e_budget :               "
      WRITE(ILUOUT,*) "JJ                 : ",JJ
      WRITE(ILUOUT,*) "Conv. (W/m²(mass)) : ",PCONV_MA_BLD(JJ)
      WRITE(ILUOUT,*) "Wind. (W/m²(mass)) : ",B%XF_MASS_WIN  (JJ) * PRAD_WIN_MA(JJ)
      WRITE(ILUOUT,*) "DQS   (W/m²(mass)) : ",PDQS_MA(JJ)
      WRITE(ILUOUT,*) "Wall  (W/m²(mass)) : ",B%XF_MASS_WALL (JJ) * PRAD_WL_MA(JJ)    
      WRITE(ILUOUT,*) "Floor (W/m²(mass)) : ",B%XF_MASS_FLOOR (JJ) * (PRAD_RF_MA(JJ) + PRAD_FL_MA(JJ))
      WRITE(ILUOUT,*) "Load  (W/m²(mass)) : ",PLOAD_MA(JJ)
      WRITE(ILUOUT,*) "------------------------------"
      WRITE(ILUOUT,*) "ZIMB (W/m²(mass))  : ",ZIMB_MA(JJ)
      CALL FLUSH(ILUOUT)
      !
      CALL ABOR1_SFX('Too large energy imbalance of building internal mass')
      !
   ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MASS_LAYER_E_BUDGET',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE MASS_LAYER_E_BUDGET
