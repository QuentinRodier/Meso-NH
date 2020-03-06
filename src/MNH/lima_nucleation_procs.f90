!MNH_LIC Copyright 2013-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-------------------------------------------------------------------------------
!      ###############################
       MODULE MODI_LIMA_NUCLEATION_PROCS
!      ###############################
!
INTERFACE
   SUBROUTINE LIMA_NUCLEATION_PROCS (PTSTEP, TPFILE, OCLOSE_OUT, PRHODJ,          &
                                     PRHODREF, PEXNREF, PPABST, PT, PTM, PW_NU,    &
                                     PTHT, PRVT, PRCT, PRRT, PRIT, PRST, PRGT,     &
                                     PCCT, PCRT, PCIT,                             &
                                     PNFT, PNAT, PIFT, PINT, PNIT, PNHT            )
!
USE MODD_IO, ONLY: TFILEDATA
!
REAL,                     INTENT(IN)    :: PTSTEP     ! Double Time step
TYPE(TFILEDATA),          INTENT(IN)    :: TPFILE     ! Output file
LOGICAL,                  INTENT(IN)    :: OCLOSE_OUT ! Conditional closure of 
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ     ! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF   ! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PEXNREF    ! Reference Exner function
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST     ! abs. pressure at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PT         ! Temperature
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTM        ! Temperature at time t-dt
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PW_NU      ! updraft velocity used for
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PTHT       ! Theta at t 
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRVT       ! Water vapor m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRCT       ! Cloud water m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRRT       ! Rain water m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRIT       ! Pristine ice m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRST       ! Snow m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRGT       ! Graupel m.r. at t
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCCT       ! Cloud water conc. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCRT       ! Rain water conc. at t
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCIT       ! Prinstine ice conc. at t
!
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PNFT       ! CCN C. available at t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PNAT       ! CCN C. activated at t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PIFT       ! IFN C. available at t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PINT       ! IFN C. activated at t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PNIT       ! Coated IFN activated at t
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PNHT       ! CCN hom freezing
!
END SUBROUTINE LIMA_NUCLEATION_PROCS
END INTERFACE
END MODULE MODI_LIMA_NUCLEATION_PROCS
!     #############################################################################
SUBROUTINE LIMA_NUCLEATION_PROCS (PTSTEP, TPFILE, OCLOSE_OUT, PRHODJ,                &
                                  PRHODREF, PEXNREF, PPABST, PT, PTM, PW_NU,          &
                                  PTHT, PRVT, PRCT, PRRT, PRIT, PRST, PRGT,           &
                                  PCCT, PCRT, PCIT,                                   &
                                  PNFT, PNAT, PIFT, PINT, PNIT, PNHT                  )
!     #############################################################################
!
!!    PURPOSE
!!    -------
!!      Compute nucleation processes for the time-splitted version of LIMA
!!
!!    AUTHOR
!!    ------
!!      B.    Vié        * CNRM *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original             15/03/2018
!  P. Wautelet 27/02/2020: bugfix: PNFT was not updated after LIMA_CCN_HOM_FREEZING
!  P. Wautelet 27/02/2020: add Z_TH_HINC variable (for budgets)
!  P. Wautelet    02/2020: use the new data structures and subroutines for budgets
!-------------------------------------------------------------------------------
!
use modd_budget,     only: lbu_enable, lbudget_th, lbudget_rv, lbudget_rc, lbudget_rr,  &
                           lbudget_ri, lbudget_rs, lbudget_rg, lbudget_rh, lbudget_sv,  &
                           NBUDGET_TH, NBUDGET_RV, NBUDGET_RC, NBUDGET_RI, NBUDGET_SV1, &
                           tbudgets
USE MODD_IO,         ONLY: TFILEDATA
USE MODD_NSV,        ONLY : NSV_LIMA_NC, NSV_LIMA_NR, NSV_LIMA_CCN_FREE,                &
                            NSV_LIMA_NI, NSV_LIMA_IFN_FREE
USE MODD_PARAM_LIMA, ONLY : LCOLD, LNUCL, LMEYERS, LSNOW, LWARM, LACTI, LRAIN, LHHONI,  &
                            NMOD_CCN, NMOD_IFN, NMOD_IMM

use mode_budget,     only: Budget_store_add, Budget_store_init, Budget_store_end

USE MODI_LIMA_CCN_ACTIVATION
USE MODI_LIMA_CCN_HOM_FREEZING
USE MODI_LIMA_MEYERS_NUCLEATION
USE MODI_LIMA_PHILLIPS_IFN_NUCLEATION
!
!-------------------------------------------------------------------------------
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
REAL,                     INTENT(IN)    :: PTSTEP     ! Double Time step
TYPE(TFILEDATA),          INTENT(IN)    :: TPFILE     ! Output file
LOGICAL,                  INTENT(IN)    :: OCLOSE_OUT ! Conditional closure of 
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ     ! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF   ! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PEXNREF    ! Reference Exner function
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST     ! abs. pressure at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PT         ! Temperature
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTM        ! Temperature at time t-dt
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PW_NU      ! updraft velocity used for
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PTHT       ! Theta at t 
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRVT       ! Water vapor m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRCT       ! Cloud water m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRRT       ! Rain water m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRIT       ! Rain water m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRST       ! Rain water m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRGT       ! Rain water m.r. at t
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCCT       ! Cloud water conc. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCRT       ! Rain water conc. at t
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCIT       ! Prinstine ice conc. at t
!
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PNFT       ! CCN C. available at t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PNAT       ! CCN C. activated at t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PIFT       ! IFN C. available at t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PINT       ! IFN C. activated at t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PNIT       ! Coated IFN activated at t
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PNHT       ! CCN hom. freezing
!
!-------------------------------------------------------------------------------
!
REAL, DIMENSION(SIZE(PT,1),SIZE(PT,2),SIZE(PT,3))          :: Z_TH_HIND, Z_RI_HIND, Z_CI_HIND, Z_TH_HINC, Z_RC_HINC, Z_CC_HINC
!
integer :: idx
INTEGER :: JL
!
!-------------------------------------------------------------------------------
!
IF ( LWARM .AND. LACTI .AND. NMOD_CCN >=1 ) THEN
  if ( lbu_enable ) then
    if ( lbudget_th ) call Budget_store_init( tbudgets(NBUDGET_TH), 'HENU', ptht(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_rv ) call Budget_store_init( tbudgets(NBUDGET_RV), 'HENU', prvt(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_rc ) call Budget_store_init( tbudgets(NBUDGET_RC), 'HENU', prct(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_sv ) then
      call Budget_store_init( tbudgets(NBUDGET_SV1 - 1 + nsv_lima_nc), 'HENU', pcct(:, :, :) * prhodj(:, :, :) / ptstep )
      do jl = 1, nmod_ccn
        idx = NBUDGET_SV1 - 1 + nsv_lima_ccn_free - 1 + jl
        call Budget_store_init( tbudgets(idx), 'HENU', pnft(:, :, :, jl) * prhodj(:, :, :) / ptstep )
      end do
    end if
  end if

   CALL LIMA_CCN_ACTIVATION (PTSTEP, TPFILE, OCLOSE_OUT,                 &
                             PRHODREF, PEXNREF, PPABST, PT, PTM, PW_NU,   &
                             PTHT, PRVT, PRCT, PCCT, PRRT, PNFT, PNAT)

  if ( lbu_enable ) then
    if ( lbudget_th ) call Budget_store_end( tbudgets(NBUDGET_TH), 'HENU', ptht(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_rv ) call Budget_store_end( tbudgets(NBUDGET_RV), 'HENU', prvt(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_rc ) call Budget_store_end( tbudgets(NBUDGET_RC), 'HENU', prct(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_sv ) then
      call Budget_store_end( tbudgets(NBUDGET_SV1 - 1 + nsv_lima_nc), 'HENU', pcct(:, :, :) * prhodj(:, :, :) / ptstep )
      do jl = 1, nmod_ccn
        idx = NBUDGET_SV1 - 1 + nsv_lima_ccn_free - 1 + jl
        call Budget_store_end( tbudgets(idx), 'HENU', pnft(:, :, :, jl) * prhodj(:, :, :) / ptstep )
      end do
    end if
  end if
END IF
!
!-------------------------------------------------------------------------------
!
IF ( LCOLD .AND. LNUCL .AND. .NOT.LMEYERS .AND. NMOD_IFN >= 1 ) THEN
  if ( lbu_enable ) then
    if ( lbudget_sv ) then
      do jl = 1, nmod_ifn
        idx = NBUDGET_SV1 - 1 + nsv_lima_ifn_free - 1 + jl
        call Budget_store_init( tbudgets(idx), 'HIND', pift(:, :, :, jl) * prhodj(:, :, :) / ptstep )
      end do
    end if
  end if

   CALL LIMA_PHILLIPS_IFN_NUCLEATION (PTSTEP,                                           &
                                      PRHODREF, PEXNREF, PPABST,                        &
                                      PTHT, PRVT, PRCT, PRRT, PRIT, PRST, PRGT,         &
                                      PCCT, PCIT, PNAT, PIFT, PINT, PNIT,               &
                                      Z_TH_HIND, Z_RI_HIND, Z_CI_HIND,                  &
                                      Z_TH_HINC, Z_RC_HINC, Z_CC_HINC                   )

  if ( lbu_enable ) then
    if ( lbudget_th ) call Budget_store_add( tbudgets(NBUDGET_TH), 'HIND',  z_th_hind(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_rv ) call Budget_store_add( tbudgets(NBUDGET_RV), 'HIND', -z_ri_hind(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_ri ) call Budget_store_add( tbudgets(NBUDGET_RI), 'HIND',  z_ri_hind(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_sv ) then
      call Budget_store_add( tbudgets(NBUDGET_SV1 - 1 + nsv_lima_ni), 'HIND', z_ci_hind(:, :, :) * prhodj(:, :, :) / ptstep )
      do jl = 1, nmod_ifn
        idx = NBUDGET_SV1 - 1 + nsv_lima_ifn_free - 1 + jl
        call Budget_store_end( tbudgets(idx), 'HIND', pift(:, :, :, jl) * prhodj(:, :, :) / ptstep )
      end do
    end if

    if ( lbudget_th ) call Budget_store_add( tbudgets(NBUDGET_TH), 'HINC',  z_th_hinc(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_rc ) call Budget_store_add( tbudgets(NBUDGET_RC), 'HINC',  z_rc_hinc(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_ri ) call Budget_store_add( tbudgets(NBUDGET_RI), 'HINC', -z_rc_hinc(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_sv ) then
      call Budget_store_add( tbudgets(NBUDGET_SV1 - 1 + nsv_lima_nc), 'HINC',  z_cc_hinc(:, :, :) * prhodj(:, :, :) / ptstep )
      call Budget_store_add( tbudgets(NBUDGET_SV1 - 1 + nsv_lima_ni), 'HINC', -z_cc_hinc(:, :, :) * prhodj(:, :, :) / ptstep )
    end if
  end if
END IF
!
!-------------------------------------------------------------------------------
!
IF (LCOLD .AND. LNUCL .AND. LMEYERS) THEN
   CALL LIMA_MEYERS_NUCLEATION (PTSTEP,                                     &
                                PRHODREF, PEXNREF, PPABST,                  &
                                PTHT, PRVT, PRCT, PRRT, PRIT, PRST, PRGT,   &
                                PCCT, PCIT, PINT,                           &
                                Z_TH_HIND, Z_RI_HIND, Z_CI_HIND,            &
                                Z_TH_HINC, Z_RC_HINC, Z_CC_HINC             )

  if ( lbu_enable ) then
    if ( lbudget_th ) call Budget_store_add( tbudgets(NBUDGET_TH), 'HIND',  z_th_hind(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_rv ) call Budget_store_add( tbudgets(NBUDGET_RV), 'HIND', -z_ri_hind(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_ri ) call Budget_store_add( tbudgets(NBUDGET_RI), 'HIND',  z_ri_hind(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_sv ) &
      call Budget_store_add( tbudgets(NBUDGET_SV1 - 1 + nsv_lima_ni), 'HIND', z_ci_hind(:, :, :) * prhodj(:, :, :) / ptstep )

    if ( lbudget_th ) call Budget_store_add( tbudgets(NBUDGET_TH), 'HINC',  z_th_hinc(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_rc ) call Budget_store_add( tbudgets(NBUDGET_RC), 'HINC',  z_rc_hinc(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_ri ) call Budget_store_add( tbudgets(NBUDGET_RI), 'HINC', -z_rc_hinc(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_sv ) then
      call Budget_store_add( tbudgets(NBUDGET_SV1 - 1 + nsv_lima_nc), 'HINC',  z_cc_hinc(:, :, :) * prhodj(:, :, :) / ptstep )
      call Budget_store_add( tbudgets(NBUDGET_SV1 - 1 + nsv_lima_ni), 'HINC', -z_cc_hinc(:, :, :) * prhodj(:, :, :) / ptstep )
    end if
  end if
END IF
!
!-------------------------------------------------------------------------------
!
IF ( LCOLD .AND. LNUCL .AND. LHHONI .AND. NMOD_CCN >= 1) THEN
  if ( lbu_enable ) then
    if ( lbudget_th ) call Budget_store_init( tbudgets(NBUDGET_TH), 'HONH', PTHT(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_rv ) call Budget_store_init( tbudgets(NBUDGET_RV), 'HONH', PRVT(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_ri ) call Budget_store_init( tbudgets(NBUDGET_RI), 'HONH', PRIT(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_sv ) then
      call Budget_store_init( tbudgets(NBUDGET_SV1 - 1 + nsv_lima_ni), 'HONH', PCIT(:, :, :) * prhodj(:, :, :) / ptstep )
      do jl = 1, nmod_ccn
        idx = NBUDGET_SV1 - 1 + nsv_lima_ccn_free - 1 + jl
        call Budget_store_end( tbudgets(idx), 'HONH', PNFT(:, :, :, jl) * prhodj(:, :, :) / ptstep )
      end do
    end if
  end if

  CALL LIMA_CCN_HOM_FREEZING (PRHODREF, PEXNREF, PPABST, PW_NU,            &
                               PTHT, PRVT, PRCT, PRRT, PRIT, PRST, PRGT,    &
                               PCCT, PCRT, PCIT, PNFT, PNHT                 )

  if ( lbu_enable ) then
    if ( lbudget_th ) call Budget_store_end( tbudgets(NBUDGET_TH), 'HONH', PTHT(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_rv ) call Budget_store_end( tbudgets(NBUDGET_RV), 'HONH', PRVT(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_ri ) call Budget_store_end( tbudgets(NBUDGET_RI), 'HONH', PRIT(:, :, :) * prhodj(:, :, :) / ptstep )
    if ( lbudget_sv ) then
      call Budget_store_end( tbudgets(NBUDGET_SV1 - 1 + nsv_lima_ni), 'HONH', PCIT(:, :, :) * prhodj(:, :, :) / ptstep )
      do jl = 1, nmod_ccn
        idx = NBUDGET_SV1 - 1 + nsv_lima_ccn_free - 1 + jl
        call Budget_store_end( tbudgets(idx), 'HONH', PNFT(:, :, :, jl) * prhodj(:, :, :) / ptstep )
      end do
    end if
  end if
ENDIF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE LIMA_NUCLEATION_PROCS
