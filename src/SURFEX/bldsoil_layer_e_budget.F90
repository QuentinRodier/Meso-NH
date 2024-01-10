!   ##########################################################################
    SUBROUTINE BLDSOIL_LAYER_E_BUDGET(HPROGRAM, OCHECK_TEB, PCHECK_PROCESS, PT_BLD, PTSTEP, PG_FLOOR,  &
                                      PHC_BLD, PTC_BLD, PD_BLD, PBLD )
!   ##########################################################################
!
!!****  *BLDSOIL_LAYER_E_BUDGET*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the evoultion of roads surface temperatures
!         
!     
!!**  METHOD
!     ------
!
!    6 : equations for evolution of Ts_road 
!        **********************************
!
!
!     dTr_1(t) / dt = 1/(dr_1*Cr_1) * (  Rn_r - H_r - LE_r 
!                                      - 2*Kr_1*(Tr_1-Tr_2)/(dr_1 +dr_2)       )
!
!     dTr_k(t) / dt = 1/(dr_k*Cr_k) * (- 2*Kr_k-1*(Tr_k-Tr_k-1)/(dr_k-1 +dr_k) 
!                                      - 2*Kr_k  *(Tr_k-Tr_k+1)/(dr_k+1 +dr_k) )
!
!       with
!
!   K*_k  = (d*_k+ d*_k+1)/(d*_k/k*_k+ d*_k+1/k*_k+1)
!
!   Rn_r = abs_Rg_r
!  - sigma * emis_r                                                   * Ts_r**4 (t+dt)
!  +         emis_r                       *    SVF_r                  * LWR
!  + sigma * emis_r * emis_w              * (1-SVF_r)                 * Ts_w**4 (t+dt)
!  +         emis_r            (1-emis_w) * (1-SVF_r)   *      SVF_w  * LWR
!  + sigma * emis_r * emis_w * (1-emis_w) * (1-SVF_r)   * (1-2*SVF_w) * Ts_w**4 (t+dt)
!  + sigma * emis_r * emis_r * (1-emis_w) * (1-SVF_r)   *      SVF_w  * Ts_r**4 (t+dt)
!
!  H_r  = rho Cp CH V ( Ts_r (t+dt) - Ta_canyon )
!
!  LE_r = rho Lv CH V ( qs_r (t+dt) - qa_canyon )
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
!!	V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/01/98 
!!                  21/11/01 (V. Masson and A. Lemonsu) bug of latent flux
!!                           for very strong evaporation (all reservoir emptied
!!                           in one time-step)
!!                     02/11 (V. Masson) split of the routine for roads and walls separately
!!      G. Pigeon      09/2012: add heating/cooling of rain from air temperature
!!                             to surface road temp. for the road energy budget 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODE_THERMOS
!
USE MODI_LAYER_E_BUDGET
USE MODI_LAYER_E_BUDGET_GET_COEF
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
!
CHARACTER(LEN=6), INTENT(IN)        :: HPROGRAM     ! program calling surf. schemes
REAL,             INTENT(IN)        :: PCHECK_PROCESS! limit for budget imbalance test
LOGICAL,              INTENT(IN)    :: OCHECK_TEB  ! Flag to check energy budget for TEB
REAL, DIMENSION(:,:), INTENT(INOUT) :: PT_BLD      ! floor/soil layers temperatures under buildings
REAL,                 INTENT(IN)    :: PTSTEP         ! time step
REAL, DIMENSION(:)  , INTENT(IN)    :: PG_FLOOR       ! heat flux from floor
REAL, DIMENSION(:,:), INTENT(IN)    :: PHC_BLD     ! heat capacity for floor/soil layers under buildings
REAL, DIMENSION(:,:), INTENT(IN)    :: PTC_BLD     ! thermal conductivity for floor/soil layers under buildings
REAL, DIMENSION(:,:), INTENT(IN)    :: PD_BLD      ! thickness of floor/soil layers under buildings
REAL, DIMENSION(:)  , INTENT(IN)    :: PBLD           ! Building fraction
!
!*      0.2    declarations of local variables
!
REAL :: ZIMPL=1.0 ! implicit coefficient
!
REAL, DIMENSION(SIZE(PT_BLD ,1),SIZE(PT_BLD ,2)) :: ZA,& ! lower diag.
                                                          ZB,& ! main  diag.
                                                          ZC,& ! upper diag.
                                                          ZY   ! r.h.s.
!
REAL, DIMENSION(SIZE(PT_BLD,1)) :: ZTS_BLD    ! soil surface temperature (in contact with floor)
REAL, DIMENSION(SIZE(PT_BLD,1)) :: ZDQS_BLD   ! storage heat flux inside soil
INTEGER                            :: IBLD_LAYER    ! number of road layers
INTEGER                            :: JJ            ! loop counter
INTEGER                            :: ILUOUT        ! Unit number
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('BLDSOIL_LAYER_E_BUDGET',0,ZHOOK_HANDLE)
!
CALL LAYER_E_BUDGET_GET_COEF( PT_BLD, PTSTEP, ZIMPL,          &
                              PHC_BLD, PTC_BLD, PD_BLD, &
                              ZA, ZB, ZC, ZY )
!
!*      1.     Layer thermal properties
!              ------------------------
!
IBLD_LAYER = SIZE(PT_BLD,2)
!
!*      2.     Surface temperature
  !            -------------------

ZTS_BLD (:) = PT_BLD(:,1)
!
!*      3.     First layers coefficients (in contact with floor)
!              -------------------------------------------------
!
ZB(:,1) = ZB(:,1)
ZY(:,1) = ZY(:,1) + PG_FLOOR(:)
!
CALL LAYER_E_BUDGET( PT_BLD, PTSTEP, ZIMPL,          &
                     PHC_BLD, PTC_BLD, PD_BLD, &
                     ZA, ZB, ZC, ZY, ZDQS_BLD )
!
! Robert: The storage term must be equal to the flux
!
IF (OCHECK_TEB) THEN
  DO JJ=1,SIZE(PG_FLOOR,1)
     !
     IF (ISNAN(ZDQS_BLD(JJ))) CALL ABOR1_SFX("NAN detected in bldsoil_layer_e_budget")
     IF (ISNAN(PG_FLOOR(JJ))) CALL ABOR1_SFX("NAN detected in bldsoil_layer_e_budget")
     !
     IF (ABS(ZDQS_BLD(JJ)-PG_FLOOR(JJ)).GT.PCHECK_PROCESS) THEN
        !
        CALL GET_LUOUT(HPROGRAM,ILUOUT)
        !
        WRITE(ILUOUT,*) "                            "
        WRITE(ILUOUT,*) "In bldsoil_layer_e_budget   "
        WRITE(ILUOUT,*) "JJ              : ",JJ
        WRITE(ILUOUT,*) "ZDQS_BLD(JJ) : ",ZDQS_BLD(JJ)
        WRITE(ILUOUT,*) "PG_FLOOR(JJ)    : ",PG_FLOOR(JJ)
        CALL FLUSH(ILUOUT)
        CALL ABOR1_SFX("Violation of energy conservation in bldsoil_layer_e_budget")
        !
     ENDIF
     !
  ENDDO
ENDIF
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('BLDSOIL_LAYER_E_BUDGET',1,ZHOOK_HANDLE)
!
END SUBROUTINE BLDSOIL_LAYER_E_BUDGET

