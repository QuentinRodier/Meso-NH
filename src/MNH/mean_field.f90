!MNH_LIC Copyright 2009-2023 CNRS, Meteo-France and Universite Paul Sabatier
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

      SUBROUTINE MEAN_FIELD( PUT, PVT, PWT, PTHT, PTKET, PPABST, PRT, PSVT )

REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT, PVT, PWT   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT, PTKET   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST   ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRT      ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PSVT   ! Passive scalar variables

END SUBROUTINE MEAN_FIELD

END INTERFACE

END MODULE MODI_MEAN_FIELD
!
!     ######################################################################
      SUBROUTINE MEAN_FIELD( PUT, PVT, PWT, PTHT, PTKET, PPABST, PRT, PSVT )
!     ######################################################################
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
!!      Original       07/2009
!!      (C.Lac)        09/2016 Max values
!!      (PA.Joulin)    12/2020 Wind turbine variables
!!      (R. Schoetter) 12/2021 adds humidity and other mean diagnostics
!!      (E. Jezequel)  11/2022 Welford algorithm and covariances
!!      (H. Toumi)     09/2022: add ADR
!!---------------------------------------------------------------
!
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_ll
USE MODD_CONF_n, ONLY: LUSERV
USE MODD_MEAN_FIELD_n
USE MODD_PARAM_n
USE MODD_MEAN_FIELD
USE MODD_CST
USE MODD_PASPOL
USE MODE_THERMO
USE MODI_SHUMAN
!
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
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRT      ! variables
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PSVT

!
!*       0.2   Declarations of local variables :
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) ::  ZTEMPT
INTEGER           :: IIU,IJU,IKU,IIB,IJB,IKB,IIE,IJE,IKE ! Arrays bounds
INTEGER           :: JI,JJ,JK   ! Loop indexes
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZUMEAN_OLD
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWMEAN_OLD
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZTHMEAN_OLD
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZRT
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZQSAT_W
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZQSAT_I
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZQACT
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZRH_W
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZRH_I
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZRH_P
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFRAC
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) ::  ZAUX_WIFF
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) ::  ZAUX_WIDD
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZRH_W_MAXCOL
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZRH_I_MAXCOL
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZRH_P_MAXCOL
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
!
IF(LUSERV) THEN
  ALLOCATE( ZRT    (IIU, IJU, IKU) )
  ALLOCATE( ZQSAT_W(IIU, IJU, IKU) )
  ALLOCATE( ZQSAT_I(IIU, IJU, IKU) )
  ALLOCATE( ZQACT  (IIU, IJU, IKU) )
  ALLOCATE( ZRH_W  (IIU, IJU, IKU) )
  ALLOCATE( ZRH_I  (IIU, IJU, IKU) )
  ALLOCATE( ZRH_P  (IIU, IJU, IKU) )
  ALLOCATE( ZFRAC  (IIU, IJU, IKU) )
  ALLOCATE( ZRH_W_MAXCOL( IIU, IJU) )
  ALLOCATE( ZRH_I_MAXCOL( IIU, IJU) )
  ALLOCATE( ZRH_P_MAXCOL( IIU, IJU) )

  ! Calculation of saturation specific humidity over water/ice
  !
  ZQSAT_W = QSAT (ZTEMPT, PPABST)
  ZQSAT_I = QSATI(ZTEMPT, PPABST)
  !
  ! Conversion mixing ratio -> specfic humidity
  !
  ZRT(:,:,:) = -9999.0
  WHERE (PRT(:,:,:).LT.1.0E-6)
     ZRT(:,:,:) = 1.0E-6
  ELSEWHERE
     ZRT(:,:,:) = PRT(:,:,:)
  ENDWHERE
  !
  ZQACT(:,:,:) = 1.0 / ( 1.0 + 1.0/ZRT(:,:,:) )
  !
  ! Calculation of relative humidity with respect to water/ice
  !
  ZRH_W(:,:,:) = 100.0*ZQACT(:,:,:)/ZQSAT_W(:,:,:)
  ZRH_I(:,:,:) = 100.0*ZQACT(:,:,:)/ZQSAT_I(:,:,:)
  !
  ! Fractional partitioning between liquid and solid cloud water
  ! as assumed in condensations
  !
  ZFRAC(:,:,:) = ( XTT - ZTEMPT(:,:,:) ) / 20.
  ZFRAC(:,:,:) = MAX( 0., MIN(1., ZFRAC(:,:,:) ) )
  !
  ! Calculation of weighted average between water and ice value
  !
  ZRH_P(:,:,:) = ZFRAC(:,:,:) * ZRH_I(:,:,:) + (1.0-ZFRAC(:,:,:)) * ZRH_W(:,:,:)
  !
  ! Calculation of the column maximum of relative humidity
  !
  ZRH_W_MAXCOL(:,:) = MAXVAL(ZRH_W(:,:,:),DIM=3)
  ZRH_I_MAXCOL(:,:) = MAXVAL(ZRH_I(:,:,:),DIM=3)
  ZRH_P_MAXCOL(:,:) = MAXVAL(ZRH_P(:,:,:),DIM=3)
  XQ_MEAN     = XQ_MEAN + ZQACT
  XRH_W_MEAN    = XRH_W_MEAN + ZRH_W
  XRH_I_MEAN    = XRH_I_MEAN + ZRH_I
  XRH_P_MEAN    = XRH_P_MEAN + ZRH_P
  XRH_W_MAXCOL_MEAN = XRH_W_MAXCOL_MEAN + ZRH_W_MAXCOL
  XRH_I_MAXCOL_MEAN = XRH_I_MAXCOL_MEAN + ZRH_I_MAXCOL
  XRH_P_MAXCOL_MEAN = XRH_P_MAXCOL_MEAN + ZRH_P_MAXCOL

  DEALLOCATE( ZRT, ZQSAT_W, ZQSAT_I, ZQACT, ZRH_W, ZRH_I, ZRH_P, ZFRAC )
  DEALLOCATE( ZRH_W_MAXCOL, ZRH_I_MAXCOL, ZRH_P_MAXCOL )
END IF

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
   IF (LCOV_FIELD) THEN
      ALLOCATE( ZUMEAN_OLD (IIU, IJU, IKU) )
      ALLOCATE( ZWMEAN_OLD (IIU, IJU, IKU) )
      ALLOCATE( ZTHMEAN_OLD(IIU, IJU, IKU) )
      ZUMEAN_OLD (:,:,:) = XUM_MEAN (:,:,:)
      ZWMEAN_OLD (:,:,:) = XWM_MEAN (:,:,:)
      ZTHMEAN_OLD(:,:,:) = XTHM_MEAN(:,:,:)
   END IF
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
     DEALLOCATE( ZUMEAN_OLD, ZWMEAN_OLD, ZTHMEAN_OLD )
   END IF
!
!-----------------------------------------------------------------------
!
!*       2. MAX
!
  !
  ! Calculation of horizontal wind speed for maximum wind speed diagnostics
  !
  ZAUX_WIFF(:,:,:) = SQRT(MXF(PUT(:,:,:))**2 + MYF(PVT(:,:,:))**2)
  ZAUX_WIDD(:,:,:) = 180.0 + (90.0 - 180.0*ATAN2(MYF(PVT(:,:,:)),MXF(PUT(:,:,:)))/XPI)
  !
  WHERE (ZAUX_WIDD(:,:,:).GT.360.0) ZAUX_WIDD(:,:,:) = ZAUX_WIDD(:,:,:) - 360.0
  !
  ! Get maximum diagnostics
  !
  DO JK=IKB,IKE
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        !
        XUM_MAX(JI,JJ,JK) = MAX(XUM_MAX(JI,JJ,JK),PUT(JI,JJ,JK))
        XVM_MAX(JI,JJ,JK) = MAX(XVM_MAX(JI,JJ,JK),PVT(JI,JJ,JK))
        XWM_MAX(JI,JJ,JK) = MAX(XWM_MAX(JI,JJ,JK),PWT(JI,JJ,JK))
        XTHM_MAX(JI,JJ,JK) = MAX(XTHM_MAX(JI,JJ,JK),PTHT(JI,JJ,JK))
        XTEMPM_MAX(JI,JJ,JK) = MAX(XTEMPM_MAX(JI,JJ,JK),ZTEMPT(JI,JJ,JK))
        IF (CTURB/='NONE') XTKEM_MAX(JI,JJ,JK) =  &
           MAX(XTKEM_MAX(JI,JJ,JK),PTKET(JI,JJ,JK))
          XPABSM_MAX(JI,JJ,JK) = MAX(XPABSM_MAX(JI,JJ,JK),PPABST(JI,JJ,JK))
        !
        IF (ZAUX_WIFF(JI,JJ,JK).GE.XWIFF_MAX(JI,JJ,JK)) THEN
           XWIFF_MAX(JI,JJ,JK) = ZAUX_WIFF(JI,JJ,JK)
           XWIDD_MAX(JI,JJ,JK) = ZAUX_WIDD(JI,JJ,JK)
        ENDIF
        !
      ENDDO
    ENDDO
  ENDDO
!
END SUBROUTINE MEAN_FIELD
