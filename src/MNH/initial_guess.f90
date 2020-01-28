!MNH_LIC Copyright 1994-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #########################
      MODULE MODI_INITIAL_GUESS
!     #########################
!
INTERFACE
!
      SUBROUTINE INITIAL_GUESS ( KRR, KSV, KTCOUNT,PRHODJ, KMI, PTSTEP,        &
                         PRUS, PRVS, PRWS, PRTHS, PRRS, PRTKES, PRSVS,         &
                         PUT, PVT, PWT, PTHT, PRT, PTKET, PSVT )
!
INTEGER,                  INTENT(IN)  :: KRR     ! Number of moist variables
INTEGER,                  INTENT(IN)  :: KSV     ! Number of Scalar Variables
INTEGER,                  INTENT(IN)  :: KTCOUNT ! Temporal loop COUNTer
                                                 ! (=1 at the segment beginning)
INTEGER,                  INTENT(IN)  :: KMI     ! Model index
!
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PRHODJ         ! (Rho) dry * Jacobian
!
REAL,                     INTENT(IN)  :: PTSTEP !  timestep 
!
REAL, DIMENSION(:,:,:),   INTENT(OUT) :: PRUS, PRVS, PRWS         ! Source
REAL, DIMENSION(:,:,:),   INTENT(OUT) :: PRTHS, PRTKES
REAL, DIMENSION(:,:,:,:), INTENT(OUT) :: PRRS, PRSVS              !  terms
!
! variables at time t (needed for PPM schemes)
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PUT, PVT, PWT
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PTHT, PTKET
REAL, DIMENSION(:,:,:,:), INTENT(IN)  :: PRT, PSVT
!
END SUBROUTINE INITIAL_GUESS
!
END INTERFACE
!
END MODULE MODI_INITIAL_GUESS 
!
!     #########################################################################
      SUBROUTINE INITIAL_GUESS ( KRR, KSV, KTCOUNT,PRHODJ, KMI, PTSTEP,        &
                         PRUS, PRVS, PRWS, PRTHS, PRRS, PRTKES, PRSVS,         &
                         PUT, PVT, PWT, PTHT, PRT, PTKET, PSVT )
!     #########################################################################
!
!!****  *INITIAL_GUESS * - routine to initialize the source terms
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to integrate the prognostic variables
!!    at t-dt into their respective source terms.
!!
!!**  METHOD
!!    ------
!!      The fields at t-dt divided by 2*TSTEP (1*TSTEP for the first time step
!!    in case of START configuration) are initializing the source term arrays.
!!      The different sources terms are initialized for the budget computations.
!!     
!!
!!    EXTERNAL
!!    --------
!!      MXM,MYM,MZM : Mean Shuman operators in the x,y,z directions
!!      BUDGET      : Stores the different budget components
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_CONF   : contains configuration variable 
!!           CCONF :  Configuration of models
!!    MODULE MODD_BUDGET:
!!         NBUMOD       : model in which budget is calculated
!!         CBUTYPE      : type of desired budget
!!                          'CART' for cartesian box configuration
!!                          'MASK' for budget zone defined by a mask 
!!                          'NONE'  ' for no budget
!!         LBU_BEG      : logical for budget begnning
!!                       .TRUE. = budget begining
!!                       .FALSE. = no budget begining
!!         NBUPROCCTR   : process counter used for each budget variable
!!         Switches for budgets activations:
!!         
!!         LBU_RU       : logical for budget of RU (wind component along x)
!!                        .TRUE. = budget of RU         
!!                        .FALSE. = no budget of RU 
!!         LBU_RV       : logical for budget of RV (wind component along y)
!!                        .TRUE. = budget of RV         
!!                        .FALSE. = no budget of RV 
!!         LBU_RW        : logical for budget of RW (wind component along z)
!!                        .TRUE. = budget of RW         
!!                        .FALSE. = no budget of RW 
!!         LBU_RTH      : logical for budget of RTH (potential temperature)
!!                        .TRUE. = budget of RTH        
!!                        .FALSE. = no budget of RTH
!!         LBU_RTKE     : logical for budget of RTKE (turbulent kinetic energy)
!!                        .TRUE. = budget of RTKE       
!!                        .FALSE. = no budget of RTKE
!!         LBU_RRV      : logical for budget of RRV (water vapor)
!!                        .TRUE. = budget of RRV 
!!                        .FALSE. = no budget of RRV 
!!         LBU_RRC      : logical for budget of RRC (cloud water)
!!                        .TRUE. = budget of RRC 
!!                        .FALSE. = no budget of RRC 
!!         LBU_RRR      : logical for budget of RRR (rain water)
!!                        .TRUE. = budget of RRR 
!!                        .FALSE. = no budget of RRR 
!!         LBU_RRI      : logical for budget of RRI (ice)
!!                        .TRUE. = budget of RRI 
!!                        .FALSE. = no budget of RRI 
!!         LBU_RRS      : logical for budget of RRS (snow)
!!                        .TRUE. = budget of RRS 
!!                        .FALSE. = no budget of RRS 
!!         LBU_RRG      : logical for budget of RRG (graupel)
!!                        .TRUE. = budget of RRG 
!!                        .FALSE. = no budget of RRG 
!!         LBU_RRH      : logical for budget of RRH (hail)
!!                        .TRUE. = budget of RRH 
!!                        .FALSE. = no budget of RRH 
!!         LBU_RSV      : logical for budget of RSVx (scalar variable)
!!                        .TRUE. = budget of RSVx 
!!                        .FALSE. = no budget of RSVx
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation ( routine INITIAL_GUESS )
!!
!!    AUTHOR
!!    ------
!!  	J.-P. Pinty      * Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    21/07/94 
!!                  20/03/95 (J.Stein) : remove R from the historical variables
!!                  01/04/95 (Ph. Hereil J. Nicolau) add the budget computation
!!                  16/10/95 (J. Stein)     change the budget calls 
!!                  19/12/96 (J.-P. Pinty)  update the budget calls 
!!                  06/11/02 (V. Masson)    update the budget calls 
!!                  20/05/06                Remove KEPS
!!                  10/09    (C.Lac)        FIT for variables advected with PPM
!!                  04/13    (C.Lac)        FIT for all variables 
!  J. Escobar)    07/2019: add reproductiblity test => MPPDB_CHECK( PRRS/RT/RHO )
!  P. Wautelet 28/01/2020: use the new data structures and subroutines for budgets for U
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_BLOWSNOW
USE MODD_BLOWSNOW_n
use modd_budget,     only: lbudget_u,  lbudget_v,  lbudget_w,  lbudget_th, lbudget_tke, lbudget_rv,  lbudget_rc, &
                           lbudget_rr, lbudget_ri, lbudget_rs, lbudget_rg, lbudget_rh,  lbudget_sv,              &
                           NBUDGET_U,  NBUDGET_V,  NBUDGET_W,  NBUDGET_TH, NBUDGET_TKE, NBUDGET_RV,  NBUDGET_RC, &
                           NBUDGET_RR, NBUDGET_RI, NBUDGET_RS, NBUDGET_RG, NBUDGET_RH,  NBUDGET_SV1,             &
                           lbu_beg, lbu_enable, nbuctr_actv, nbuprocctr, tbudgets
USE MODD_CONF
USE MODD_GRID_n

use mode_budget,     only: Budget_store_init, Budget_store_end
USE MODE_MPPDB

USE MODI_BUDGET
USE MODI_SHUMAN
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
INTEGER,                  INTENT(IN)  :: KRR     ! Number of moist variables
INTEGER,                  INTENT(IN)  :: KSV     ! Number of Scalar Variables
INTEGER,                  INTENT(IN)  :: KTCOUNT ! Temporal loop COUNTer
                                                 ! (=1 at the segment beginning)
INTEGER,                  INTENT(IN)  :: KMI     ! Model index
!
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PRHODJ         ! (Rho) dry * Jacobian
!
REAL,                     INTENT(IN)  :: PTSTEP !  timestep 
!
REAL, DIMENSION(:,:,:),   INTENT(OUT) :: PRUS, PRVS, PRWS         ! Source
REAL, DIMENSION(:,:,:),   INTENT(OUT) :: PRTHS, PRTKES
REAL, DIMENSION(:,:,:,:), INTENT(OUT) :: PRRS, PRSVS  !  terms
!
!
! variables at time t (needed for PPM schemes)
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PUT, PVT, PWT
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PTHT, PTKET
REAL, DIMENSION(:,:,:,:), INTENT(IN)  :: PRT, PSVT
!
!*       0.2   declarations of local variables
!
INTEGER                               :: JRR, JSV
INTEGER                               :: IKU
REAL                                  :: ZINVTSTEP
!
!-------------------------------------------------------------------------------
!
IKU=SIZE(XZHAT)
!*       1.     COMPUTES THE INVERSE OF THE APPLICABLE TIMESTEP
!   	        -----------------------------------------------
!
ZINVTSTEP = 1./PTSTEP                          
!
!
!*       2.     COMPUTES THE FIRST SOURCE TERMS
!   	        -------------------------------
! 
! *** momentum
! forward-in-time time-marching scheme
PRUS = PUT * ZINVTSTEP * MXM(PRHODJ)
PRVS = PVT * ZINVTSTEP * MYM(PRHODJ)
PRWS = PWT * ZINVTSTEP * MZM(1,IKU,1,PRHODJ)
!
! *** meteorological variables
!
PRTHS(:,:,:) = PTHT(:,:,:) * ZINVTSTEP * PRHODJ(:,:,:)
IF (SIZE(PTKET,1) /= 0) THEN 
  PRTKES(:,:,:) = PTKET(:,:,:) * ZINVTSTEP * PRHODJ(:,:,:)
END IF
!
! Case with KRR moist variables 
DO JRR=1,KRR
  PRRS(:,:,:,JRR) = PRT(:,:,:,JRR) * ZINVTSTEP * PRHODJ(:,:,:) 
END DO
CALL MPPDB_CHECK3DM("initial_guess:PRRS/RT/RHO",PRECISION,PRRS(:,:,:,1) , PRT(:,:,:,1) , PRHODJ)
!
! *** passive tracers
!
! Case with KSV Scalar Variables
DO JSV=1,KSV
  PRSVS(:,:,:,JSV) = PSVT(:,:,:,JSV) * ZINVTSTEP * PRHODJ(:,:,:)
END DO
!
IF(LBLOWSNOW) THEN
  DO JSV=1,(NBLOWSNOW_2D)
    XRSNWCANOS(:,:,JSV) = XSNWCANO(:,:,JSV) * ZINVTSTEP * PRHODJ(:,:,1)
  END DO
END IF
!
IF (LBU_ENABLE) THEN
  IF (LBU_BEG) THEN
    NBUPROCCTR(:)=1
    NBUCTR_ACTV(:)=1

    !Remark: does not need a call to Budget_store_init because the budget array is overwritten for this source term
    if ( lbudget_u ) call Budget_store_end( tbudgets(NBUDGET_U), 'INIF', prus )

    IF (LBUDGET_V)   CALL BUDGET( PRVS,          NBUDGET_V,   'INIF_BU_RV'   )
    IF (LBUDGET_W)   CALL BUDGET( PRWS,          NBUDGET_W,   'INIF_BU_RW'   )
    IF (LBUDGET_TH)  CALL BUDGET( PRTHS,         NBUDGET_TH,  'INIF_BU_RTH'  )
    IF (LBUDGET_TKE) CALL BUDGET( PRTKES,        NBUDGET_TKE, 'INIF_BU_RTKE' )
    IF (LBUDGET_RV)  CALL BUDGET( PRRS(:,:,:,1), NBUDGET_RV,  'INIF_BU_RRV'  )
    IF (LBUDGET_RC)  CALL BUDGET( PRRS(:,:,:,2), NBUDGET_RC,  'INIF_BU_RRC'  )
    IF (LBUDGET_RR)  CALL BUDGET( PRRS(:,:,:,3), NBUDGET_RR,  'INIF_BU_RRR'  )
    IF (LBUDGET_RI)  CALL BUDGET( PRRS(:,:,:,4), NBUDGET_RI,  'INIF_BU_RRI'  )
    IF (LBUDGET_RS)  CALL BUDGET( PRRS(:,:,:,5), NBUDGET_RS,  'INIF_BU_RRS'  )
    IF (LBUDGET_RG)  CALL BUDGET( PRRS(:,:,:,6), NBUDGET_RG,  'INIF_BU_RRG'  )
    IF (LBUDGET_RH)  CALL BUDGET( PRRS(:,:,:,7), NBUDGET_RH,  'INIF_BU_RRH'  )
    DO JSV=1,KSV
      IF (LBUDGET_SV) CALL BUDGET( PRSVS(:,:,:,JSV), JSV + NBUDGET_SV1 - 1, 'INIF_BU_RSV' )
    END DO
!
    NBUPROCCTR(:)=2
    NBUCTR_ACTV(:)=2
!
    IF (LBUDGET_V)   CALL BUDGET( PRVS,          NBUDGET_V,   'ENDF_BU_RV'   )
    IF (LBUDGET_W)   CALL BUDGET( PRWS,          NBUDGET_W,   'ENDF_BU_RW'   )
    IF (LBUDGET_TH)  CALL BUDGET( PRTHS,         NBUDGET_TH,  'ENDF_BU_RTH'  )
    IF (LBUDGET_TKE) CALL BUDGET( PRTKES,        NBUDGET_TKE, 'ENDF_BU_RTKE' )
    IF (LBUDGET_RV)  CALL BUDGET( PRRS(:,:,:,1), NBUDGET_RV,  'ENDF_BU_RRV'  )
    IF (LBUDGET_RC)  CALL BUDGET( PRRS(:,:,:,2), NBUDGET_RC,  'ENDF_BU_RRC'  )
    IF (LBUDGET_RR)  CALL BUDGET( PRRS(:,:,:,3), NBUDGET_RR,  'ENDF_BU_RRR'  )
    IF (LBUDGET_RI)  CALL BUDGET( PRRS(:,:,:,4), NBUDGET_RI,  'ENDF_BU_RRI'  )
    IF (LBUDGET_RS)  CALL BUDGET( PRRS(:,:,:,5), NBUDGET_RS,  'ENDF_BU_RRS'  )
    IF (LBUDGET_RG)  CALL BUDGET( PRRS(:,:,:,6), NBUDGET_RG,  'ENDF_BU_RRG'  )
    IF (LBUDGET_RH)  CALL BUDGET( PRRS(:,:,:,7), NBUDGET_RH,  'ENDF_BU_RRH'  )
    DO JSV=1,KSV
      IF (LBUDGET_SV) CALL BUDGET( PRSVS(:,:,:,JSV), JSV + NBUDGET_SV1 - 1, 'ENDF_BU_RSV' )
    END DO
  END IF
!
  NBUPROCCTR(:)=4
  NBUCTR_ACTV(:)=4
!
!  stores the Asselin source term
!
  !The Asselin source term is computed from the end of the previous time step to now
  !Therefore, it has to be stored only if not the 1st timestep of the budget
  if ( .not. lbu_beg ) then
    if ( lbudget_u ) call Budget_store_end( tbudgets(NBUDGET_U), 'ASSE', prus )
  end if

  IF (LBUDGET_V)   CALL BUDGET( PRVS,          NBUDGET_V,   'ASSE_BU_RV'   )
  IF (LBUDGET_W)   CALL BUDGET( PRWS,          NBUDGET_W,   'ASSE_BU_RW'   )
  IF (LBUDGET_TH)  CALL BUDGET( PRTHS,         NBUDGET_TH,  'ASSE_BU_RTH'  )
  IF (LBUDGET_TKE) CALL BUDGET( PRTKES,        NBUDGET_TKE, 'ASSE_BU_RTKE' )
  IF (LBUDGET_RV)  CALL BUDGET( PRRS(:,:,:,1), NBUDGET_RV,  'ASSE_BU_RRV'  )
  IF (LBUDGET_RC)  CALL BUDGET( PRRS(:,:,:,2), NBUDGET_RC,  'ASSE_BU_RRC'  )
  IF (LBUDGET_RR)  CALL BUDGET( PRRS(:,:,:,3), NBUDGET_RR,  'ASSE_BU_RRR'  )
  IF (LBUDGET_RI)  CALL BUDGET( PRRS(:,:,:,4), NBUDGET_RI,  'ASSE_BU_RRI'  )
  IF (LBUDGET_RS)  CALL BUDGET( PRRS(:,:,:,5), NBUDGET_RS,  'ASSE_BU_RRS'  )
  IF (LBUDGET_RG)  CALL BUDGET( PRRS(:,:,:,6), NBUDGET_RG,  'ASSE_BU_RRG'  )
  IF (LBUDGET_RH)  CALL BUDGET( PRRS(:,:,:,7), NBUDGET_RH,  'ASSE_BU_RRH'  )
  DO JSV=1,KSV
    IF (LBUDGET_SV) CALL BUDGET( PRSVS(:,:,:,JSV), JSV + NBUDGET_SV1 - 1, 'ASSE_BU_RSV' )
  END DO
END IF

LBU_BEG=.FALSE.

!-------------------------------------------------------------------------------
!
END SUBROUTINE INITIAL_GUESS
