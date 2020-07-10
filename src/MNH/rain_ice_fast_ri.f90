!MNH_LIC Copyright 1995-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 25/02/2019: split rain_ice (cleaner and easier to maintain/debug)
!  P. Wautelet 05/06/2019: optimisations
!-----------------------------------------------------------------
MODULE MODE_RAIN_ICE_FAST_RI

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: RAIN_ICE_FAST_RI

CONTAINS

SUBROUTINE RAIN_ICE_FAST_RI(OMICRO, PRHODREF, PRIT, PRHODJ, PZT, PSSI, PLSFACT, PLVFACT, &
                            PAI, PCJ, PRHODJ3D, PTHS3D, PRCS3D, PRIS3D, PCIT, PRCS, PRIS, PTHS)
!
!*      0. DECLARATIONS
!          ------------
!
use MODD_BUDGET,         only: LBUDGET_RC, LBUDGET_RI, LBUDGET_TH
use MODD_CST,            only: XTT
use MODD_RAIN_ICE_DESCR, only: XDI, XLBEXI, XLBI, XRTMIN
use MODD_RAIN_ICE_PARAM, only: X0DEPI, X2DEPI
!
use MODI_BUDGET
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
LOGICAL,  DIMENSION(:,:,:), intent(in)    :: OMICRO   ! Test where to compute all processes
REAL,     DIMENSION(:),     intent(in)    :: PRHODREF ! RHO Dry REFerence
REAL,     DIMENSION(:),     intent(in)    :: PRIT     ! Pristine ice m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRHODJ   ! RHO times Jacobian
REAL,     DIMENSION(:),     intent(in)    :: PZT      ! Temperature
REAL,     DIMENSION(:),     intent(in)    :: PSSI     ! Supersaturation over ice
REAL,     DIMENSION(:),     intent(in)    :: PLSFACT  ! L_s/(Pi_ref*C_ph)
REAL,     DIMENSION(:),     intent(in)    :: PLVFACT  ! L_v/(Pi_ref*C_ph)
REAL,     DIMENSION(:),     intent(in)    :: PAI      ! Thermodynamical function
REAL,     DIMENSION(:),     intent(in)    :: PCJ      ! Function to compute the ventilation coefficient
REAL,     DIMENSION(:,:,:), INTENT(IN)    :: PRHODJ3D ! Dry density * Jacobian
REAL,     DIMENSION(:,:,:), INTENT(IN)    :: PTHS3D   ! Theta source
REAL,     DIMENSION(:,:,:), INTENT(IN)    :: PRCS3D   ! Cloud vapor m.r. source
REAL,     DIMENSION(:,:,:), INTENT(IN)    :: PRIS3D   ! Ice vapor m.r. source
REAL,     DIMENSION(:),     intent(inout) :: PCIT     ! Pristine ice conc. at t
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRCS     ! Cloud water m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRIS     ! Pristine ice m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PTHS     ! Theta source
!
!*       0.2  declaration of local variables
!
REAL, DIMENSION(size(PRHODREF)) :: ZZW  ! Work array
!-------------------------------------------------------------------------------
!
!*       7.1    cloud ice melting
!
  WHERE( PRIS(:)>0.0 .AND. PZT(:)>XTT )
    PRCS(:) = PRCS(:) + PRIS(:)
    PTHS(:) = PTHS(:) - PRIS(:)*(PLSFACT(:)-PLVFACT(:)) ! f(L_f*(-RIMLTC))
    PRIS(:) = 0.0
    PCIT(:) = 0.0
  END WHERE
  IF (LBUDGET_TH) CALL BUDGET (                                                   &
                 UNPACK(PTHS(:),MASK=OMICRO(:,:,:),FIELD=PTHS3D)*PRHODJ3D(:,:,:), &
                                                              4,'IMLT_BU_RTH')
  IF (LBUDGET_RC) CALL BUDGET (                                                   &
                 UNPACK(PRCS(:),MASK=OMICRO(:,:,:),FIELD=PRCS3D)*PRHODJ3D(:,:,:), &
                                                              7,'IMLT_BU_RRC')
  IF (LBUDGET_RI) CALL BUDGET (                                                   &
                 UNPACK(PRIS(:),MASK=OMICRO(:,:,:),FIELD=PRIS3D)*PRHODJ3D(:,:,:), &
                                                              9,'IMLT_BU_RRI')
!
!*       7.2    Bergeron-Findeisen effect: RCBERI
!
  WHERE( PRCS(:)>0.0 .AND. PSSI(:)>0.0 .AND. PRIT(:)>XRTMIN(4) .AND. PCIT(:)>0.0 )
    ZZW(:) = MIN(1.E8,XLBI*( PRHODREF(:)*PRIT(:)/PCIT(:) )**XLBEXI) ! Lbda_i
    ZZW(:) = MIN( PRCS(:),( PSSI(:) / (PRHODREF(:)*PAI(:)) ) * PCIT(:) * &
                  ( X0DEPI/ZZW(:) + X2DEPI*PCJ(:)*PCJ(:)/ZZW(:)**(XDI+2.0) ) )
    PRCS(:) = PRCS(:) - ZZW(:)
    PRIS(:) = PRIS(:) + ZZW(:)
    PTHS(:) = PTHS(:) + ZZW(:)*(PLSFACT(:)-PLVFACT(:)) ! f(L_f*(RCBERI))
  END WHERE
  IF (LBUDGET_TH) CALL BUDGET (                                                   &
                 UNPACK(PTHS(:),MASK=OMICRO(:,:,:),FIELD=PTHS3D)*PRHODJ3D(:,:,:), &
                                                             4,'BERFI_BU_RTH')
  IF (LBUDGET_RC) CALL BUDGET (                                                   &
                 UNPACK(PRCS(:),MASK=OMICRO(:,:,:),FIELD=PRCS3D)*PRHODJ3D(:,:,:), &
                                                             7,'BERFI_BU_RRC')
  IF (LBUDGET_RI) CALL BUDGET (                                                   &
                 UNPACK(PRIS(:),MASK=OMICRO(:,:,:),FIELD=PRIS3D)*PRHODJ3D(:,:,:), &
                                                             9,'BERFI_BU_RRI')
!
END SUBROUTINE RAIN_ICE_FAST_RI

END MODULE MODE_RAIN_ICE_FAST_RI
