!MNH_LIC Copyright 2002-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!################################
module mode_write_les_rt_budget_n
!################################

implicit none

private

public :: Write_les_rt_budget_n

contains

!#############################################
subroutine  Write_les_rt_budget_n( tpdiafile )
!#############################################
!
!
!!****  *Write_les_rt_budget_n* writes the LES final diagnostics for model _n
!!                         
!!
!!    PURPOSE
!!    -------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      V. Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original         06/11/02
!  P. Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 15/10/2020: restructure Les_diachro calls to use tfield_metadata_base type
! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------

use modd_cst,         only: xg
use modd_field,       only: NMNHDIM_BUDGET_LES_LEVEL, NMNHDIM_BUDGET_LES_TIME, &
                            NMNHDIM_BUDGET_TERM, NMNHDIM_UNUSED,               &
                            tfield_metadata_base, TYPEREAL
use modd_io,          only: tfiledata
use modd_les,         only: cles_norm_type, nles_k, xles_temp_mean_start, xles_temp_mean_end, xles_temp_sampling
use modd_les_n,       only: nles_times,                                                                                   &
                            xles_bu_res_rt2, xles_bu_res_thlrt, xles_bu_res_wrt,                                          &
                            xles_mean_drtdz, xles_mean_dthldz, xles_mean_thv, xles_mean_w,                                &
                            xles_res_ddxa_rt_sbg_uart, xles_res_ddxa_rt_sbg_uaw, xles_res_ddxa_w_sbg_uart,                &
                            xles_res_ddxa_rt_sbg_uathl, xles_res_ddxa_thl_sbg_uart,                                       &
                            xles_res_ddz_rt_sbg_w2, xles_res_w_sbg_wrt, xles_res_w_sbg_rt2, xles_res_w_sbg_thlrt,         &
                            xles_subgrid_diss_rt2, xles_subgrid_diss_thlrt, xles_subgrid_rt2, xles_subgrid_rtpz,          &
                            xles_subgrid_rtthv, xles_subgrid_thlrt, xles_subgrid_w2, xles_subgrid_wrt, xles_subgrid_wrt2, &
                            xles_subgrid_w2rt, xles_subgrid_wthl, xles_subgrid_wthlrt,                                    &
                            xles_z
use modd_les_budget,  only: NLES_RELA, NLES_RAD,  NLES_GRAV, NLES_COR, NLES_MICR, NLES_HTURB, NLES_VTURB, NLES_FORC, &
                            NLES_PRES, NLES_DIFF, NLES_PREF, NLES_DP,  NLES_TR,   NLES_TEND,  NLES_ADVM,  NLES_NEST, NLES_MISC
use modd_parameters,  only: XUNDEF

use mode_les_diachro, only: Les_diachro

IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
TYPE(TFILEDATA),  INTENT(IN) :: TPDIAFILE ! file to write
!
!
!*      0.2  declaration of local variables
!
integer, parameter :: NMAX_ILES = 40

INTEGER :: ILES
INTEGER :: ILES_STA
INTEGER :: JLES
INTEGER :: ILES_P1, ILES_P2
!
INTEGER :: JK ! vertical loop counter
INTEGER :: JT ! temporal loop counter
!
CHARACTER(len=9), DIMENSION(NMAX_ILES) :: YSUBTITLE
CHARACTER(len=8)                       :: YGROUP
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZLES_BUDGET
!
logical                    :: gdoavg  ! Compute and store time average
logical                    :: gdonorm ! Compute and store normalized field
type(tfield_metadata_base) :: tzfield
!-------------------------------------------------------------------------------
!
!*          Initializations
!            ---------------
!
ALLOCATE(ZLES_BUDGET(NLES_K,NLES_TIMES,NMAX_ILES))
!
ZLES_BUDGET(:,:,:) = XUNDEF

tzfield%ngrid = 0 !Not on the Arakawa grid
tzfield%ntype = TYPEREAL
tzfield%ndims = 3
tzfield%ndimlist(1)  = NMNHDIM_BUDGET_LES_LEVEL
tzfield%ndimlist(2)  = NMNHDIM_BUDGET_LES_TIME
tzfield%ndimlist(3)  = NMNHDIM_BUDGET_TERM
tzfield%ndimlist(4:) = NMNHDIM_UNUSED

gdoavg  = xles_temp_mean_start /= XUNDEF .and. xles_temp_mean_end /= XUNDEF
gdonorm = Trim( cles_norm_type ) /= 'NONE'

!-------------------------------------------------------------------------------
!
!*      2.  total water variance budget
!           ---------------------------
!
!
YGROUP= 'BU_RT2  '
ILES=0
ILES_STA=ILES
!
!
!* 2.1 production by mean gradients
!      ----------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' SBG DP M'
ILES_P1=ILES
!
ZLES_BUDGET(:,:,ILES)= - 2. * XLES_SUBGRID_WRt(:,:,1) * XLES_MEAN_dRtdz(:,:,1)
!
!
!* 2.3 production by resolved gradients
!      --------------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' SBG DP R'
!
ZLES_BUDGET(:,:,ILES)= - 2. * XLES_RES_ddxa_Rt_SBG_UaRt(:,:,1)  &
                          - ZLES_BUDGET(:,:,ILES_P1)
!
!
!* 2.3 turbulent transport
!      -------------------
!
IF ( ANY(XLES_SUBGRID_WRt2(:,:,1)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' SBG TR  '
!
DO JK=2,NLES_K-1
  ZLES_BUDGET(JK,:,ILES) = - ( XLES_SUBGRID_WRt2 (JK+1,:,1)      &
                              -XLES_SUBGRID_WRt2 (JK-1,:,1)    ) &
                           / ( XLES_Z            (JK+1)          &
                              -XLES_Z            (JK-1)        )
END DO
!
ZLES_BUDGET(1     ,:,ILES) = ZLES_BUDGET(2       ,:,ILES)
ZLES_BUDGET(NLES_K,:,ILES) = ZLES_BUDGET(NLES_K-1,:,ILES)
END IF
!
!* 2.4 dissipation
!      -----------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' DISS    '
!
ZLES_BUDGET(:,:,ILES) =  XLES_SUBGRID_DISS_Rt2(:,:,1)
!
!
!* 2.5 residual of subgrid budget
!      --------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' SBG RESI'
!
ZLES_BUDGET(:,:,ILES) = 0.
DO JLES=ILES_STA+1,ILES-1
  ZLES_BUDGET(:,:,ILES) = ZLES_BUDGET(:,:,ILES) - ZLES_BUDGET(:,:,JLES)
END DO
!
ILES_STA=ILES
!
!* 2.6 tendency
!      --------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES TEND'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Rt2(:,:,NLES_TEND)
!
!
!* 2.7 advection
!      ---------
!
IF ( ANY(XLES_BU_RES_Rt2(:,:,NLES_ADVM)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES ADV '
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Rt2(:,:,NLES_ADVM)
END IF
!
!* 2.8 forcing
!      -------
!
IF ( ANY(XLES_BU_RES_Rt2(:,:,NLES_FORC)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES FORC'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Rt2(:,:,NLES_FORC)
END IF
!
!
!* 2.9 production
!      ----------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES DP  '
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Rt2(:,:,NLES_DP)

!
!* 2.10 turbulent transport
!       -------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES TR  '
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Rt2(:,:,NLES_TR)
!
!
!* 2.11 effect of subgrid scale motions on the resolved flow
!       ----------------------------------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES SBGT'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Rt2(:,:,NLES_VTURB) + XLES_BU_RES_Rt2(:,:,NLES_HTURB)
!
!
!* 2.11 effect of diffusion
!       -------------------
!
IF ( ANY(XLES_BU_RES_Rt2(:,:,NLES_DIFF)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES NUMD'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Rt2(:,:,NLES_DIFF)
END IF
!
!* 2.11 effect of relaxation
!       --------------------
!
IF ( ANY(XLES_BU_RES_Rt2(:,:,NLES_RELA)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES RELA'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Rt2(:,:,NLES_RELA)
END IF
!
!* 2.11 effect of nesting
!       -----------------
!
IF ( ANY(XLES_BU_RES_Rt2(:,:,NLES_NEST)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES NEST'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Rt2(:,:,NLES_NEST)
END IF
!
!* 2.11 other effects
!       -------------
!
IF ( ANY( XLES_BU_RES_Rt2(:,:,NLES_MISC) &
         +XLES_BU_RES_Rt2(:,:,NLES_MICR)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES MISC'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Rt2(:,:,NLES_MISC) &
                      + XLES_BU_RES_Rt2(:,:,NLES_MICR)
END IF
!
!
!* 2.12 residual of resolved budget
!       ---------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES RESI'
!
ZLES_BUDGET(:,:,ILES) = 0.
DO JLES=ILES_STA+1,ILES-1
  ZLES_BUDGET(:,:,ILES) = ZLES_BUDGET(:,:,ILES) - ZLES_BUDGET(:,:,JLES)
END DO
!
!* 2.13 neglected term: tendency
!       ------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' NSG TEND'
!
ZLES_BUDGET(:,:,ILES) = 0.
IF (NLES_TIMES>2) THEN
  DO JK=1,NLES_K
    DO JT=2,NLES_TIMES-1
      ZLES_BUDGET(JK,JT,ILES) =- ( XLES_SUBGRID_Rt2 (JK  ,JT+1,1) &
                                 - XLES_SUBGRID_Rt2 (JK  ,JT-1,1))&
                              / (2.* XLES_TEMP_SAMPLING)
    END DO
    ZLES_BUDGET(JK,1         ,ILES) = ZLES_BUDGET(JK,2           ,ILES)
    ZLES_BUDGET(JK,NLES_TIMES,ILES) = ZLES_BUDGET(JK,NLES_TIMES-1,ILES)
  END DO
END IF
!
!* 2.14 neglected term: advection for subgrid quantity
!       ----------------------------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' NSG ADVM'
!
DO JK=2,NLES_K-1
  ZLES_BUDGET(JK,:,ILES)=  -XLES_MEAN_W(JK,:,1)                &
                         * ( XLES_SUBGRID_Rt2(JK+1,:,1)        &
                            -XLES_SUBGRID_Rt2(JK-1,:,1)      ) &
                         / ( XLES_Z          (JK+1)            &
                            -XLES_Z          (JK-1)          )
END DO
!
ZLES_BUDGET(1     ,:,ILES) = ZLES_BUDGET(2       ,:,ILES)
ZLES_BUDGET(NLES_K,:,ILES) = ZLES_BUDGET(NLES_K-1,:,ILES)
!
!* 2.15 neglected term: advection for subgrid quantity
!       ----------------------------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' NSG ADVR'
!
DO JK=2,NLES_K-1
  ZLES_BUDGET(JK,:,ILES)= - ( XLES_RES_W_SBG_Rt2   (JK+1,:,1)    &
                             -XLES_RES_W_SBG_Rt2   (JK-1,:,1)  ) &
                          / ( XLES_Z           (JK+1)            &
                             -XLES_Z           (JK-1)          )
END DO
!
ZLES_BUDGET(1     ,:,ILES) = ZLES_BUDGET(2       ,:,ILES)
ZLES_BUDGET(NLES_K,:,ILES) = ZLES_BUDGET(NLES_K-1,:,ILES)
!
!
!* 2.16 writing
!       -------
!
tzfield%cmnhname  = ygroup
tzfield%cstdname  = ''
tzfield%clongname = ygroup
tzfield%ccomment  = 'Rt variance budget'
tzfield%cunits    = 'kg2 kg-2 s-1'

call Les_diachro( tpdiafile, tzfield, gdoavg, gdonorm, zles_budget(:, :, :iles), hsuffixes = ysubtitle(:iles) )

!-------------------------------------------------------------------------------
!
!*      3.  total water flux budget
!           -----------------------
!
!
YGROUP= 'BU_WRT  '
ILES=0
ILES_STA=ILES
!
!* 3.1 production by mean gradients
!     -----------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' SBG DP M'
ILES_P1=ILES
!
ZLES_BUDGET(:,:,ILES) =  - XLES_SUBGRID_W2(:,:,1) * XLES_MEAN_dRtdz(:,:,1)
!
!
!* 3.2 production by gradient of resolved motions
!     -------------------------------------------
!
ILES=ILES+1
YSUBTITLE(2) = ' SBG DP R'
ILES_P2=ILES
!
ZLES_BUDGET(:,:,ILES)=- XLES_RES_ddz_Rt_SBG_W2(:,:,1) &
                      - ZLES_BUDGET(:,:,ILES_P1)
!
!
!
!* 3.3 turbulent transport
!      -------------------
!
IF ( ANY(XLES_SUBGRID_W2Rt(:,:,1)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' SBG TR  '
!
DO JK=2,NLES_K-1
  ZLES_BUDGET(JK,:,ILES) = - ( XLES_SUBGRID_W2Rt (JK+1,:,1)       &
                              -XLES_SUBGRID_W2Rt (JK-1,:,1)     ) &
                           / ( XLES_Z           (JK+1)            &
                              -XLES_Z           (JK-1)          )
END DO
!
ZLES_BUDGET(1     ,:,ILES) = ZLES_BUDGET(2       ,:,ILES)
ZLES_BUDGET(NLES_K,:,ILES) = ZLES_BUDGET(NLES_K-1,:,ILES)
END IF
!
!
!* 3.4 presso-correlations
!      -------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' SBG PRES'
!
ZLES_BUDGET(:,:,ILES) =  XLES_SUBGRID_RtPz(:,:,1)
!
!
!* 3.5 thermal production
!      ------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' SBG TP  '
!
ZLES_BUDGET(:,:,ILES) =  XG * XLES_SUBGRID_RtThv(:,:,1)   &
                            / XLES_MEAN_Thv     (:,:,1)
!
!
!* 3.6 dissipation
!      -----------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' DISS    '
!
ZLES_BUDGET(:,:,ILES) = 0.
!
!
!* 3.7 residual of subgrid budget
!      --------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' SBG RESI'
!
ZLES_BUDGET(:,:,ILES) = 0.
DO JLES=ILES_STA+1,ILES-1
  ZLES_BUDGET(:,:,ILES) = ZLES_BUDGET(:,:,ILES) - ZLES_BUDGET(:,:,JLES)
END DO
!
ILES_STA=ILES
!
!* 3.8 tendency
!      --------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES TEND'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WRt(:,:,NLES_TEND)
!
!* 3.9 advection
!      ---------
!
IF ( ANY(XLES_BU_RES_WRt(:,:,NLES_ADVM)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES ADV '
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WRt(:,:,NLES_ADVM)
END IF
!
!* 3.10 forcing
!       -------
!
IF ( ANY(XLES_BU_RES_WRt(:,:,NLES_FORC)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES FORC'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WRt(:,:,NLES_FORC)
END IF
!
!* 3.11 production by temperature gradient (and vertical wind gradient)
!       ----------------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES DP  '
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WRt(:,:,NLES_DP)
!
!* 3.12 turbulent transport
!       -------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES TR  '
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WRt(:,:,NLES_TR)
!
!
!* 3.13 presso-correlations
!       -------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES PRES'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WRt(:,:,NLES_PRES)
!
!
!* 3.14 thermal production
!       ------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES TP  '
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WRt(:,:,NLES_GRAV)
!
!
!* 3.15 effect of subgrid scale motions on the resolved flow
!       ----------------------------------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES SBGT'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WRt(:,:,NLES_VTURB) + XLES_BU_RES_WRt(:,:,NLES_HTURB)
!
!* 3.15 effect of Coriolis
!       ------------------
!
IF ( ANY(XLES_BU_RES_WRt(:,:,NLES_COR)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES CORI'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WRt(:,:,NLES_COR)
END IF
!
!* 3.15 effect of diffusion
!       -------------------
!
IF ( ANY(XLES_BU_RES_WRt(:,:,NLES_DIFF)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES NUMD'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WRt(:,:,NLES_DIFF)
END IF
!
!* 3.15 effect of relaxation
!       --------------------
!
IF ( ANY(XLES_BU_RES_WRt(:,:,NLES_RELA)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES RELA'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WRt(:,:,NLES_RELA)
END IF
!
!* 3.15 effect of nesting
!       -----------------
!
IF ( ANY(XLES_BU_RES_WRt(:,:,NLES_NEST)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES NEST'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WRt(:,:,NLES_NEST)
END IF
!
!* 3.15 other effects
!       -------------
!
IF ( ANY( XLES_BU_RES_WRt(:,:,NLES_MISC) &
         +XLES_BU_RES_WRt(:,:,NLES_MICR) /= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES MISC'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WRt(:,:,NLES_MISC) &
                      + XLES_BU_RES_WRt(:,:,NLES_MICR)
END IF
!
!* 3.16 residual of resolved Wthl budget
!       --------------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES RESI'
!
ZLES_BUDGET(:,:,ILES) = 0.
DO JLES=ILES_STA+1,ILES-1
  ZLES_BUDGET(:,:,ILES) = ZLES_BUDGET(:,:,ILES) - ZLES_BUDGET(:,:,JLES)
END DO
!
!* 3.17 neglected term: tendency
!       ------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' SBG TEND'
!
ZLES_BUDGET(:,:,ILES) = 0.
IF (NLES_TIMES>2) THEN
  DO JK=1,NLES_K
    DO JT=2,NLES_TIMES-1
      ZLES_BUDGET(JK,JT,ILES) =- ( XLES_SUBGRID_WRt (JK  ,JT+1,1) &
                                 - XLES_SUBGRID_WRt (JK  ,JT-1,1))&
                                 / (2.* XLES_TEMP_SAMPLING)
    END DO
    ZLES_BUDGET(JK,1         ,ILES) = ZLES_BUDGET(JK,2           ,ILES)
    ZLES_BUDGET(JK,NLES_TIMES,ILES) = ZLES_BUDGET(JK,NLES_TIMES-1,ILES)
  END DO
END IF
!
!
!
!* 3.18 neglected terms : advection for subgrid quantity
!       ------------------------------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' NSG ADVM'
!
DO JK=2,NLES_K-1
  ZLES_BUDGET(JK,:,ILES)= - XLES_MEAN_W(JK,:,1)                              &
                              * ( XLES_SUBGRID_WRt(JK+1,:,1)                 &
                                 -XLES_SUBGRID_WRt(JK-1,:,1)               ) &
                             /  ( XLES_Z          (JK+1)                     &
                                 -XLES_Z          (JK-1)                   )
END DO
!
ZLES_BUDGET(1     ,:,ILES) = ZLES_BUDGET(2       ,:,ILES)
ZLES_BUDGET(NLES_K,:,ILES) = ZLES_BUDGET(NLES_K-1,:,ILES)
!
!* 3.19 neglected terms : advection for subgrid quantity
!       ------------------------------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' NSG ADVR'
!
DO JK=2,NLES_K-1
  ZLES_BUDGET(JK,:,ILES)= - ( XLES_RES_W_SBG_WRt(JK+1,:,1)   &
                             -XLES_RES_W_SBG_WRt(JK-1,:,1) ) &
                          / ( XLES_Z          (JK+1)         &
                             -XLES_Z          (JK-1)       )
END DO
!
ZLES_BUDGET(1     ,:,ILES) = ZLES_BUDGET(2       ,:,ILES)
ZLES_BUDGET(NLES_K,:,ILES) = ZLES_BUDGET(NLES_K-1,:,ILES)
!
!* 3.20 neglected terms : production by gradient of vertical velocity for subgrid quantity
!       ----------------------------------------------------------------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' NSG DPGW'
!
ZLES_BUDGET(:,:,ILES)=- XLES_RES_ddxa_W_SBG_UaRt(:,:,1)
!
!
!* 3.21 neglected terms : production by hor. gradient of Thl for subgrid quantity
!       -------------------------------------------------------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' NSG DPGT'
!
ZLES_BUDGET(:,:,ILES)=-XLES_RES_ddxa_Rt_SBG_UaW(:,:,1)       &
                      -ZLES_BUDGET(:,:,ILES_P1) -ZLES_BUDGET(:,:,ILES_P2)
!
!
!* 3.22 writing
!       -------
!
tzfield%cmnhname  = ygroup
tzfield%cstdname  = ''
tzfield%clongname = ygroup
tzfield%ccomment  = 'Rt flux budget'
tzfield%cunits    = 'm kg kg-1 s-2'

call Les_diachro( tpdiafile, tzfield, gdoavg, gdonorm, zles_budget(:, :, :iles), hsuffixes = ysubtitle(:iles) )

!-------------------------------------------------------------------------------
!
!*      4.  liquid potential temperature - total water covariance budget
!           ------------------------------------------------------------
!
!
YGROUP= 'BU_THLR '
ILES=0
ILES_STA=ILES
!
!
!* 2.1 production by mean gradients
!      ----------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' SBG DP M'
ILES_P1=ILES
!
ZLES_BUDGET(:,:,ILES)=-XLES_SUBGRID_WRt (:,:,1) * XLES_MEAN_dThldz(:,:,1) &
                      -XLES_SUBGRID_WThl(:,:,1) * XLES_MEAN_dRtdz (:,:,1)
!
!
!* 2.3 production by resolved gradients
!      --------------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' SBG DP R'
!
ZLES_BUDGET(:,:,ILES)= - XLES_RES_ddxa_Rt_SBG_UaThl(:,:,1)  &
                       - XLES_RES_ddxa_Thl_SBG_UaRt(:,:,1)  &
                       - ZLES_BUDGET(:,:,ILES_P1)
!
!
!* 2.3 turbulent transport
!      -------------------
!
IF ( ANY(XLES_SUBGRID_WThlRt(:,:,1)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' SBG TR  '
!
DO JK=2,NLES_K-1
  ZLES_BUDGET(JK,:,ILES) = - ( XLES_SUBGRID_WThlRt (JK+1,:,1)      &
                              -XLES_SUBGRID_WThlRt (JK-1,:,1)    ) &
                           / ( XLES_Z              (JK+1)          &
                              -XLES_Z              (JK-1)        )
END DO
!
ZLES_BUDGET(1     ,:,ILES) = ZLES_BUDGET(2       ,:,ILES)
ZLES_BUDGET(NLES_K,:,ILES) = ZLES_BUDGET(NLES_K-1,:,ILES)
END IF
!
!* 2.4 dissipation
!      -----------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' DISS    '
!
ZLES_BUDGET(:,:,ILES) =  XLES_SUBGRID_DISS_ThlRt(:,:,1)
!
!
!* 2.5 residual of subgrid budget
!      --------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' SBG RESI'
!
ZLES_BUDGET(:,:,ILES) = 0.
DO JLES=ILES_STA+1,ILES-1
  ZLES_BUDGET(:,:,ILES) = ZLES_BUDGET(:,:,ILES) - ZLES_BUDGET(:,:,JLES)
END DO
!
ILES_STA=ILES
!
!* 2.6 tendency
!      --------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES TEND'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_ThlRt(:,:,NLES_TEND)
!
!
!* 2.7 advection
!      ---------
!
IF ( ANY(XLES_BU_RES_ThlRt(:,:,NLES_ADVM)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(7) = ' RES ADV '
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_ThlRt(:,:,NLES_ADVM)
END IF
!
!* 2.8 forcing
!      -------
!
IF ( ANY(XLES_BU_RES_ThlRt(:,:,NLES_FORC)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES FORC'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_ThlRt(:,:,NLES_FORC)
END IF
!
!* 2.9 production
!      ----------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES DP  '
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_ThlRt(:,:,NLES_DP)
!
!* 2.10 turbulent transport
!       -------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES TR  '
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_ThlRt(:,:,NLES_TR)
!
!
!* 2.11 effect of subgrid scale motions on the resolved flow
!       ----------------------------------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES SBGT'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_ThlRt(:,:,NLES_VTURB) + XLES_BU_RES_ThlRt(:,:,NLES_HTURB)
!
!
!* 2.11 effect of diffusion
!       -------------------
!
IF ( ANY(XLES_BU_RES_ThlRt(:,:,NLES_DIFF)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES NUMD'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_ThlRt(:,:,NLES_DIFF)
END IF
!
!* 2.11 effect of relaxation
!       --------------------
!
IF ( ANY(XLES_BU_RES_ThlRt(:,:,NLES_RELA)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES RELA'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_ThlRt(:,:,NLES_RELA)
END IF
!
!* 2.11 effect of nesting
!       -----------------
!
IF ( ANY(XLES_BU_RES_ThlRt(:,:,NLES_NEST)/= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES NEST'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_ThlRt(:,:,NLES_NEST)
END IF
!
!* 2.11 miscellaneous effects
!       ---------------------
!
IF ( ANY( XLES_BU_RES_ThlRt(:,:,NLES_MISC) &
         +XLES_BU_RES_ThlRt(:,:,NLES_PREF) &
         +XLES_BU_RES_ThlRt(:,:,NLES_RAD ) &
         +XLES_BU_RES_ThlRt(:,:,NLES_MICR) /= 0.) ) THEN
ILES=ILES+1
YSUBTITLE(ILES) = ' RES MISC'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_ThlRt(:,:,NLES_MISC) &
                      + XLES_BU_RES_ThlRt(:,:,NLES_PREF) &
                      + XLES_BU_RES_ThlRt(:,:,NLES_RAD ) &
                      + XLES_BU_RES_ThlRt(:,:,NLES_MICR)
END IF
!
!
!* 2.12 residual of resolved budget
!       ---------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' RES RESI'
!
ZLES_BUDGET(:,:,ILES) = 0.
DO JLES=ILES_STA+1,ILES-1
  ZLES_BUDGET(:,:,ILES) = ZLES_BUDGET(:,:,ILES) - ZLES_BUDGET(:,:,JLES)
END DO
!
!* 2.13 neglected term: tendency
!       ------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' NSG TEND'
!
ZLES_BUDGET(:,:,ILES) = 0.
IF (NLES_TIMES>2) THEN
  DO JK=1,NLES_K
    DO JT=2,NLES_TIMES-1
      ZLES_BUDGET(JK,JT,ILES) =- ( XLES_SUBGRID_ThlRt (JK  ,JT+1,1) &
                                 - XLES_SUBGRID_ThlRt (JK  ,JT-1,1))&
                                / (2.* XLES_TEMP_SAMPLING)
    END DO
    ZLES_BUDGET(JK,1         ,ILES) = ZLES_BUDGET(JK,2           ,ILES)
    ZLES_BUDGET(JK,NLES_TIMES,ILES) = ZLES_BUDGET(JK,NLES_TIMES-1,ILES)
  END DO
END IF
!
!* 2.14 neglected term: advection for subgrid quantity
!       ----------------------------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' NSG ADVM'
!
DO JK=2,NLES_K-1
  ZLES_BUDGET(JK,:,ILES)=  -XLES_MEAN_W(JK,:,1)                &
                         * ( XLES_SUBGRID_ThlRt(JK+1,:,1)      &
                            -XLES_SUBGRID_ThlRt(JK-1,:,1)    ) &
                         / ( XLES_Z          (JK+1)            &
                            -XLES_Z          (JK-1)          )
END DO
!
ZLES_BUDGET(1     ,:,ILES) = ZLES_BUDGET(2       ,:,ILES)
ZLES_BUDGET(NLES_K,:,ILES) = ZLES_BUDGET(NLES_K-1,:,ILES)
!
!* 2.15 neglected term: advection for subgrid quantity
!       ----------------------------------------------
!
ILES=ILES+1
YSUBTITLE(ILES) = ' NSG ADVR'
!
DO JK=2,NLES_K-1
  ZLES_BUDGET(JK,:,ILES)= - ( XLES_RES_W_SBG_ThlRt (JK+1,:,1)    &
                             -XLES_RES_W_SBG_ThlRt (JK-1,:,1)  ) &
                          / ( XLES_Z           (JK+1)            &
                             -XLES_Z           (JK-1)          )
END DO
!
ZLES_BUDGET(1     ,:,ILES) = ZLES_BUDGET(2       ,:,ILES)
ZLES_BUDGET(NLES_K,:,ILES) = ZLES_BUDGET(NLES_K-1,:,ILES)
!
!
!* 2.16 writing
!       -------
!
tzfield%cmnhname  = ygroup
tzfield%cstdname  = ''
tzfield%clongname = ygroup
tzfield%ccomment  = 'Thl-Rt covariance budget'
tzfield%cunits    = 'K kg kg-1 s-1'

call Les_diachro( tpdiafile, tzfield, gdoavg, gdonorm, zles_budget(:, :, :iles), hsuffixes = ysubtitle(:iles) )

!-------------------------------------------------------------------------------
!
DEALLOCATE(ZLES_BUDGET)
!
!-------------------------------------------------------------------------------
!
end subroutine Write_les_rt_budget_n

end module mode_write_les_rt_budget_n
