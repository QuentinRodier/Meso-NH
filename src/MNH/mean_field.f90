!MNH_LIC Copyright 2009-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_MEAN_FIELD
!     ##########################
!
!
INTERFACE

      SUBROUTINE MEAN_FIELD( PUT, PVT, PWT, PTHT, PTKET, PPABST, PSVT )

REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT, PVT, PWT   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT, PTKET   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PSVT   ! Passive scalar variables

END SUBROUTINE MEAN_FIELD

END INTERFACE

END MODULE MODI_MEAN_FIELD
!
!     #################################################################
      SUBROUTINE MEAN_FIELD( PUT, PVT, PWT, PTHT, PTKET, PPABST, PSVT )
!     #################################################################
!
!!****  *MEAN_FIELD * -
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     P. Aumond 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      07/2009
!!      (C.Lac)       09/2016 Max values
!!      (PA.Joulin)   12/2020 Wind turbine variables
!!      (E. Jezequel) 11/2022 Welford algorithm and covariances
!!      (H. Toumi)    09/2022: add ADR
!!---------------------------------------------------------------
!
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_ll
USE MODD_MEAN_FIELD_n
USE MODD_PARAM_n
USE MODD_MEAN_FIELD
USE MODD_CST
USE MODD_PASPOL
! * EOL
USE MODD_EOL_MAIN, ONLY: LMAIN_EOL, CMETH_EOL, NMODEL_EOL
USE MODD_EOL_SHARED_IO, ONLY: XTHRUT, XTORQT, XPOWT, XAOA_GLB
USE MODD_EOL_SHARED_IO, ONLY: XTHRU_SUM, XTORQ_SUM, XPOW_SUM, XAOA_SUM
USE MODD_EOL_ADR, ONLY: XFAERO_RA_GLB, XFAERO_RA_SUM
USE MODD_EOL_ADR, ONLY: XFAERO_BLEQ_RA_GLB, XFAERO_BLEQ_RA_SUM
USE MODD_EOL_ADR, ONLY: XAOA_BLEQ_GLB, XAOA_BLEQ_SUM
USE MODD_EOL_ALM, ONLY: XFAERO_RE_SUM, XFAERO_RE_GLB
!
USE MODE_MODELN_HANDLER
USE MODI_UPDATE_WELFORD
!
IMPLICIT NONE

!*       0.1   Declarations of dummy arguments :
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT, PVT, PWT   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT, PTKET   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PSVT

!
!*       0.2   Declarations of local variables :
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) ::  ZTEMPT
INTEGER           :: IIU,IJU,IKU,IIB,IJB,IKB,IIE,IJE,IKE ! Arrays bounds
INTEGER           :: JI,JJ,JK   ! Loop indexes
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZUMEAN_OLD
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZWMEAN_OLD
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZTHMEAN_OLD
!
INTEGER :: IMI !Current model index
!
!
!-----------------------------------------------------------------------
!
!*       0.     ARRAYS BOUNDS INITIALIZATION
!
IIU=SIZE(PTHT,1)
IJU=SIZE(PTHT,2)
IKU=SIZE(PTHT,3)
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
IKB=1+JPVEXT
IKE=IKU-JPVEXT
!
!-----------------------------------------------------------------------
!
!*       1. MEAN
!
   ZTEMPT = PTHT*(((PPABST)/XP00)**(XRD/XCPD))
!
   IF (LPASPOL)  XSVT_MEAN  = PSVT + XSVT_MEAN
   IF (CTURB/='NONE') XTKEM_MEAN = PTKET + XTKEM_MEAN
!
!  Wind turbine variables
   IMI = GET_CURRENT_MODEL_INDEX()
   IF (LMAIN_EOL .AND. IMI==NMODEL_EOL) THEN
    SELECT CASE(CMETH_EOL)
     CASE('ADNR') ! Actuator Disc Non-Rotating
      XTHRU_SUM       = XTHRUT        + XTHRU_SUM
     CASE('ADR') ! Actuator Disc with Rotation
      XAOA_SUM        = XAOA_GLB      + XAOA_SUM
      XFAERO_RA_SUM   = XFAERO_RA_GLB + XFAERO_RA_SUM
      XTHRU_SUM       = XTHRUT        + XTHRU_SUM
      XTORQ_SUM       = XTORQT        + XTORQ_SUM
      XPOW_SUM        = XPOWT         + XPOW_SUM
      XAOA_BLEQ_SUM       = XAOA_BLEQ_GLB      + XAOA_BLEQ_SUM
      XFAERO_BLEQ_RA_SUM  = XFAERO_BLEQ_RA_GLB + XFAERO_BLEQ_RA_SUM
     CASE('ALM') ! Actuator Line Method
      XAOA_SUM        = XAOA_GLB      + XAOA_SUM
      XFAERO_RE_SUM   = XFAERO_RE_GLB + XFAERO_RE_SUM
      XTHRU_SUM       = XTHRUT        + XTHRU_SUM
      XTORQ_SUM       = XTORQT        + XTORQ_SUM
      XPOW_SUM        = XPOWT         + XPOW_SUM
    END SELECT
   END IF
!
   MEAN_COUNT = MEAN_COUNT + 1
!
!  Save old mean values for covariance computations
!
   ZUMEAN_OLD  = XUM_MEAN
   ZWMEAN_OLD  = XWM_MEAN
   ZTHMEAN_OLD = XTHM_MEAN
!  Welford method for variables whom we compute variances
!
   CALL UPDATE_WELFORD(MEAN_COUNT,XUM_MEAN,XU2_M2,PUT)
   CALL UPDATE_WELFORD(MEAN_COUNT,XVM_MEAN,XV2_M2,PVT)
   CALL UPDATE_WELFORD(MEAN_COUNT,XWM_MEAN,XW2_M2,PWT)
   CALL UPDATE_WELFORD(MEAN_COUNT,XTHM_MEAN,XTH2_M2,PTHT)
   CALL UPDATE_WELFORD(MEAN_COUNT,XTEMPM_MEAN,XTEMP2_M2,ZTEMPT)
   CALL UPDATE_WELFORD(MEAN_COUNT,XPABSM_MEAN,XPABS2_M2,PPABST)
!
!  Welford method for covariances
!
   IF (LCOV_FIELD) THEN
     XUV_MEAN  = XUV_MEAN + (PUT-ZUMEAN_OLD)*(PVT-XVM_MEAN)
     XUW_MEAN  = XUW_MEAN + (PUT-ZUMEAN_OLD)*(PWT-XWM_MEAN)
     XVW_MEAN  = XVW_MEAN + (PWT-ZWMEAN_OLD)*(PVT-XVM_MEAN)
     XWTH_MEAN = XVW_MEAN + (PWT-ZWMEAN_OLD)*(PTHT-XTHM_MEAN)
   END IF
!
!-----------------------------------------------------------------------
!
!*       2. MAX
!
  DO JK=IKB,IKE
   DO JJ=IJB,IJE
    DO JI=IIB,IIE
      XUM_MAX(JI,JJ,JK) = MAX(XUM_MAX(JI,JJ,JK),PUT(JI,JJ,JK))
      XVM_MAX(JI,JJ,JK) = MAX(XVM_MAX(JI,JJ,JK),PVT(JI,JJ,JK))
      XWM_MAX(JI,JJ,JK) = MAX(XWM_MAX(JI,JJ,JK),PWT(JI,JJ,JK))
      XTHM_MAX(JI,JJ,JK) = MAX(XTHM_MAX(JI,JJ,JK),PTHT(JI,JJ,JK))
      XTEMPM_MAX(JI,JJ,JK) = MAX(XTEMPM_MAX(JI,JJ,JK),ZTEMPT(JI,JJ,JK))
      IF (CTURB/='NONE') XTKEM_MAX(JI,JJ,JK) =  &
              MAX(XTKEM_MAX(JI,JJ,JK),PTKET(JI,JJ,JK))
      XPABSM_MAX(JI,JJ,JK) = MAX(XPABSM_MAX(JI,JJ,JK),PPABST(JI,JJ,JK))
    END DO
   END DO
  END DO
!
END SUBROUTINE MEAN_FIELD
