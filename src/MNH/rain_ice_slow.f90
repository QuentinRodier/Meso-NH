!MNH_LIC Copyright 1995-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 25/02/2019: split rain_ice (cleaner and easier to maintain/debug)
!-----------------------------------------------------------------
MODULE MODE_RAIN_ICE_SLOW

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: RAIN_ICE_SLOW

CONTAINS

SUBROUTINE RAIN_ICE_SLOW(OMICRO, PINVTSTEP, PRHODREF,                      &
                         PRCT, PRRT, PRIT, PRST, PRGT, PRHODJ, PZT, PPRES, &
                         PLSFACT, PLVFACT,                                 &
                         PSSI, PRHODJ3D, PTHS3D, PRVS3D,                   &
                         PRVS, PRCS, PRRS, PRIS, PRSS, PRGS, PTHS,         &
                         PAI, PCJ, PKA, PDV, PLBDAS, PLBDAG)
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_BUDGET
USE MODD_CST
USE MODD_RAIN_ICE_DESCR
USE MODD_RAIN_ICE_PARAM
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
LOGICAL,  DIMENSION(:,:,:), intent(in)    :: OMICRO   ! Test where to compute all processes
REAL,                       intent(in)    :: PINVTSTEP
REAL,     DIMENSION(:),     intent(in)    :: PRHODREF ! RHO Dry REFerence
REAL,     DIMENSION(:),     intent(in)    :: PRCT     ! Cloud water m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRRT     ! Rain water m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRIT     ! Pristine ice m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRST     ! Snow/aggregate m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRGT     ! Graupel m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRHODJ   ! RHO times Jacobian
REAL,     DIMENSION(:),     intent(in)    :: PZT      ! Temperature
REAL,     DIMENSION(:),     intent(in)    :: PPRES    ! Pressure
REAL,     DIMENSION(:),     intent(in)    :: PLSFACT  ! L_s/(Pi_ref*C_ph)
REAL,     DIMENSION(:),     intent(in)    :: PLVFACT  ! L_v/(Pi_ref*C_ph)
REAL,     DIMENSION(:),     intent(in)    :: PSSI     ! Supersaturation over ice
REAL,     DIMENSION(:,:,:), INTENT(IN)    :: PRHODJ3D ! Dry density * Jacobian
REAL,     DIMENSION(:,:,:), INTENT(IN)    :: PTHS3D   ! Theta source
REAL,     DIMENSION(:,:,:), INTENT(IN)    :: PRVS3D   ! Water vapor m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRVS     ! Water vapor m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRCS     ! Cloud water m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRRS     ! Rain water m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRIS     ! Pristine ice m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRSS     ! Snow/aggregate m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRGS     ! Graupel m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PTHS     ! Theta source
REAL,     DIMENSION(:),     intent(OUT)   :: PAI      ! Thermodynamical function
REAL,     DIMENSION(:),     intent(OUT)   :: PCJ      ! Function to compute the ventilation coefficient
REAL,     DIMENSION(:),     intent(OUT)   :: PKA      ! Thermal conductivity of the air
REAL,     DIMENSION(:),     intent(OUT)   :: PDV      ! Diffusivity of water vapor in the air
REAL,     DIMENSION(:),     intent(OUT)   :: PLBDAS   ! Slope parameter of the aggregate distribution
REAL,     DIMENSION(:),     intent(OUT)   :: PLBDAG   ! Slope parameter of the graupel   distribution
!
!*       0.2  declaration of local variables
!
REAL, DIMENSION(size(PRHODREF)) :: ZZW ! Work array
REAL, DIMENSION(size(PRHODREF)) :: ZCRIAUTI ! Snow-to-ice autoconversion thres.
!
!-------------------------------------------------------------------------------
!
!
!*       3.2     compute the homogeneous nucleation source: RCHONI
!
  ZZW(:) = 0.0
  WHERE( (PZT(:)<XTT-35.0) .AND. (PRCT(:)>XRTMIN(2)) .AND. (PRCS(:)>0.) )
    ZZW(:) = MIN( PRCS(:),XHON*PRHODREF(:)*PRCT(:)       &
                                 *EXP( MIN(XMNH_HUGE_12_LOG,XALPHA3*(PZT(:)-XTT)-XBETA3) ) )
                                 ! *EXP( XALPHA3*(PZT(:)-XTT)-XBETA3 ) )
    PRIS(:) = PRIS(:) + ZZW(:)
    PRCS(:) = PRCS(:) - ZZW(:)
    PTHS(:) = PTHS(:) + ZZW(:)*(PLSFACT(:)-PLVFACT(:)) ! f(L_f*(RCHONI))
  ENDWHERE
!
  IF (LBUDGET_TH) CALL BUDGET (                                                &
                 UNPACK(PTHS(:),MASK=OMICRO(:,:,:),FIELD=PTHS3D)*PRHODJ3D(:,:,:),  &
                                                              4,'HON_BU_RTH')
  IF (LBUDGET_RC) CALL BUDGET (                                                &
                     UNPACK(PRCS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),   &
                                                              7,'HON_BU_RRC')
  IF (LBUDGET_RI) CALL BUDGET (                                                &
                     UNPACK(PRIS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),   &
                                                              9,'HON_BU_RRI')
!
!*       3.3     compute the spontaneous freezing source: RRHONG
!
  ZZW(:) = 0.0
  WHERE( (PZT(:)<XTT-35.0) .AND. (PRRT(:)>XRTMIN(3)) .AND. (PRRS(:)>0.) )
    ZZW(:) = MIN( PRRS(:),PRRT(:)* PINVTSTEP )
    PRGS(:) = PRGS(:) + ZZW(:)
    PRRS(:) = PRRS(:) - ZZW(:)
    PTHS(:) = PTHS(:) + ZZW(:)*(PLSFACT(:)-PLVFACT(:)) ! f(L_f*(RRHONG))
  ENDWHERE
!
  IF (LBUDGET_TH) CALL BUDGET (                                                &
                 UNPACK(PTHS(:),MASK=OMICRO(:,:,:),FIELD=PTHS3D)*PRHODJ3D(:,:,:),  &
                                                              4,'SFR_BU_RTH')
  IF (LBUDGET_RR) CALL BUDGET (                                                &
                     UNPACK(PRRS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),   &
                                                              8,'SFR_BU_RRR')
  IF (LBUDGET_RG) CALL BUDGET (                                                &
                     UNPACK(PRGS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),   &
                                                             11,'SFR_BU_RRG')
!
!*       3.4    compute the deposition, aggregation and autoconversion sources
!
  PKA(:) = 2.38E-2 + 0.0071E-2 * ( PZT(:) - XTT )          ! k_a
  PDV(:) = 0.211E-4 * (PZT(:)/XTT)**1.94 * (XP00/PPRES(:)) ! D_v
!
!*       3.4.1  compute the thermodynamical function A_i(T,P)
!*              and the c^prime_j (in the ventilation factor)
!

  PAI(:) = EXP( XALPI - XBETAI/PZT(:) - XGAMI*ALOG(PZT(:) ) ) ! es_i
  PAI(:) = ( XLSTT + (XCPV-XCI)*(PZT(:)-XTT) )**2 / (PKA(:)*XRV*PZT(:)**2) &
                                 + ( XRV*PZT(:) ) / (PDV(:)*PAI(:))
  PCJ(:) = XSCFAC * PRHODREF(:)**0.3 / SQRT( 1.718E-5+0.0049E-5*(PZT(:)-XTT) )
!
!*       3.4.2  compute the riming-conversion of r_c for r_i production: RCAUTI
!
!  ZZW(:) = 0.0
!  ZTIMAUTIC = SQRT( XTIMAUTI*XTIMAUTC )
!  WHERE ( (PRCT(:)>0.0) .AND. (PRIT(:)>0.0) .AND. (PRCS(:)>0.0) )
!    ZZW(:) = MIN( PRCS(:),ZTIMAUTIC * MAX( SQRT( PRIT(:)*PRCT(:) ),0.0 ) )
!    PRIS(:) = PRIS(:) + ZZW(:)
!    PRCS(:) = PRCS(:) - ZZW(:)
!    PTHS(:) = PTHS(:) + ZZW(:)*(PLSFACT(:)-PLVFACT(:)) ! f(L_f*(RCAUTI))
!  END WHERE
!
!*       3.4.3  compute the deposition on r_s: RVDEPS
!
  WHERE ( PRST(:)>0.0 )
    PLBDAS(:)  = MIN( XLBDAS_MAX,                                           &
                      XLBS*( PRHODREF(:)*MAX( PRST(:),XRTMIN(5) ) )**XLBEXS )
  END WHERE
  ZZW(:) = 0.0
  WHERE ( (PRST(:)>XRTMIN(5)) .AND. (PRSS(:)>0.0) )
    ZZW(:) = ( PSSI(:)/(PRHODREF(:)*PAI(:)) ) *                               &
             ( X0DEPS*PLBDAS(:)**XEX0DEPS + X1DEPS*PCJ(:)*PLBDAS(:)**XEX1DEPS )
    ZZW(:) =         MIN( PRVS(:),ZZW(:)      )*(0.5+SIGN(0.5,ZZW(:))) &
                   - MIN( PRSS(:),ABS(ZZW(:)) )*(0.5-SIGN(0.5,ZZW(:)))
    PRSS(:) = PRSS(:) + ZZW(:)
    PRVS(:) = PRVS(:) - ZZW(:)
    PTHS(:) = PTHS(:) + ZZW(:)*PLSFACT(:)
  END WHERE
  IF (LBUDGET_TH) CALL BUDGET (                                                &
                 UNPACK(PTHS(:),MASK=OMICRO(:,:,:),FIELD=PTHS3D)*PRHODJ3D(:,:,:),  &
                                                              4,'DEPS_BU_RTH')
  IF (LBUDGET_RV) CALL BUDGET (                                                &
                 UNPACK(PRVS(:),MASK=OMICRO(:,:,:),FIELD=PRVS3D)*PRHODJ3D(:,:,:),  &
                                                              6,'DEPS_BU_RRV')
  IF (LBUDGET_RS) CALL BUDGET (                                                &
                     UNPACK(PRSS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),   &
                                                             10,'DEPS_BU_RRS')
!
!*       3.4.4  compute the aggregation on r_s: RIAGGS
!
  ZZW(:) = 0.0
  WHERE ( (PRIT(:)>XRTMIN(4)) .AND. (PRST(:)>XRTMIN(5)) .AND. (PRIS(:)>0.0) )
    ZZW(:) = MIN( PRIS(:),XFIAGGS * EXP( XCOLEXIS*(PZT(:)-XTT) ) &
                                  * PRIT(:)                      &
                                  * PLBDAS(:)**XEXIAGGS          &
                                  * PRHODREF(:)**(-XCEXVT)       )
    PRSS(:)  = PRSS(:)  + ZZW(:)
    PRIS(:)  = PRIS(:)  - ZZW(:)
  END WHERE
  IF (LBUDGET_RI) CALL BUDGET (                                                 &
                     UNPACK(PRIS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                              9,'AGGS_BU_RRI')
  IF (LBUDGET_RS) CALL BUDGET (                                                 &
                     UNPACK(PRSS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                             10,'AGGS_BU_RRS')
!
!*       3.4.5  compute the autoconversion of r_i for r_s production: RIAUTS
!
!  ZCRIAUTI(:)=MIN(XCRIAUTI,10**(0.06*(PZT(:)-XTT)-3.5))
  ZCRIAUTI(:)=MIN(XCRIAUTI,10**(XACRIAUTI*(PZT(:)-XTT)+XBCRIAUTI))
  ZZW(:) = 0.0
  WHERE ( (PRIT(:)>XRTMIN(4)) .AND. (PRIS(:)>0.0) )
    ZZW(:) = MIN( PRIS(:),XTIMAUTI * EXP( XTEXAUTI*(PZT(:)-XTT) ) &
                            * MAX( PRIT(:)-ZCRIAUTI(:),0.0 ) )
    PRSS(:)  = PRSS(:)  + ZZW(:)
    PRIS(:)  = PRIS(:)  - ZZW(:)
  END WHERE
  IF (LBUDGET_RI) CALL BUDGET (                                                 &
                     UNPACK(PRIS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                              9,'AUTS_BU_RRI')
  IF (LBUDGET_RS) CALL BUDGET (                                                 &
                     UNPACK(PRSS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                             10,'AUTS_BU_RRS')
!
!*       3.4.6  compute the deposition on r_g: RVDEPG
!
!
  WHERE ( PRGT(:)>0.0 )
    PLBDAG(:)  = XLBG*( PRHODREF(:)*MAX( PRGT(:),XRTMIN(6) ) )**XLBEXG
  END WHERE
  ZZW(:) = 0.0
  WHERE ( (PRGT(:)>XRTMIN(6)) .AND. (PRGS(:)>0.0) )
    ZZW(:) = ( PSSI(:)/(PRHODREF(:)*PAI(:)) ) *                               &
             ( X0DEPG*PLBDAG(:)**XEX0DEPG + X1DEPG*PCJ(:)*PLBDAG(:)**XEX1DEPG )
    ZZW(:) =         MIN( PRVS(:),ZZW(:)      )*(0.5+SIGN(0.5,ZZW(:))) &
                   - MIN( PRGS(:),ABS(ZZW(:)) )*(0.5-SIGN(0.5,ZZW(:)))
    PRGS(:) = PRGS(:) + ZZW(:)
    PRVS(:) = PRVS(:) - ZZW(:)
    PTHS(:) = PTHS(:) + ZZW(:)*PLSFACT(:)
  END WHERE
  IF (LBUDGET_TH) CALL BUDGET (                                                 &
                 UNPACK(PTHS(:),MASK=OMICRO(:,:,:),FIELD=PTHS3D)*PRHODJ3D(:,:,:),   &
                                                              4,'DEPG_BU_RTH')
  IF (LBUDGET_RV) CALL BUDGET (                                                 &
                 UNPACK(PRVS(:),MASK=OMICRO(:,:,:),FIELD=PRVS3D)*PRHODJ3D(:,:,:),   &
                                                              6,'DEPG_BU_RRV')
  IF (LBUDGET_RG) CALL BUDGET (                                                 &
                     UNPACK(PRGS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                             11,'DEPG_BU_RRG')
!
END SUBROUTINE RAIN_ICE_SLOW

END MODULE MODE_RAIN_ICE_SLOW
