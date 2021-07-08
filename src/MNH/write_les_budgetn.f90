!MNH_LIC Copyright 2000-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!#############################
module mode_write_les_budget_n
!#############################

implicit none

private

public :: Write_les_budget_n

contains

!##########################################
subroutine  Write_les_budget_n( tpdiafile )
!##########################################
!
!
!!****  *Write_les_budget_n* writes the LES final diagnostics for model _n
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
!!      Original   07/02/00
!!                 06/11/02 (V. Masson) new LES budgets
!  P. Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 15/10/2020: restructure Les_diachro calls to use tfield_metadata_base type
!  JL Redelsperger 03/21 modif buoyancy flix for OCEAN LES case  
! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------

use modd_conf_n,      only: luserv
use modd_cst,         only: xg, xalphaoc
use modd_dyn_n,       only: locean
use modd_field,       only: NMNHDIM_BUDGET_LES_LEVEL, NMNHDIM_BUDGET_LES_TIME, &
                            NMNHDIM_BUDGET_TERM, NMNHDIM_UNUSED,               &
                            tfield_metadata_base, TYPEREAL
use modd_io,          only: tfiledata
use modd_les,         only: cles_norm_type, nles_k, xles_temp_mean_start, xles_temp_mean_end, xles_temp_sampling
use modd_les_n,       only: nles_times,                                                                                   &
                            xles_bu_res_ke, xles_bu_res_thl2, xles_bu_res_wthl,                                           &
                            xles_bu_sbg_tke,                                                                              &
                            xles_mean_dthldz, xles_mean_dudz, xles_mean_dvdz, xles_mean_dwdz,                             &
                            xles_mean_th, xles_mean_thv, xles_mean_w,                                                     &
                            xles_res_ddxa_thl_sbg_uaw, xles_res_ddxa_w_sbg_uathl, xles_res_ddxa_thl_sbg_uathl,            &
                            xles_res_ddz_thl_sbg_w2,                                                                      &
                            xles_res_w_sbg_thl2, xles_res_w_sbg_wthl,                                                     &
                            xles_subgrid_diss_thl2,                                                                       &
                            xles_subgrid_thl2, xles_subgrid_thlpz, xles_subgrid_thlthv, xles_subgrid_w2,                  &
                            xles_subgrid_w2thl, xles_subgrid_wthl, xles_subgrid_wthl2, xles_subgrid_wu, xles_subgrid_wv,  &
                            xles_z
use modd_les_budget,  only: NLES_RELA, NLES_RAD,  NLES_GRAV, NLES_COR,  NLES_MICR, NLES_HTURB, NLES_VTURB, NLES_FORC,     &
                            NLES_PRES, NLES_DIFF, NLES_CURV, NLES_PREF, NLES_DP,   NLES_TP,    NLES_TR,    NLES_DISS,     &
                            NLES_TEND,  NLES_ADVR, NLES_ADVM,  NLES_NEST, NLES_MISC
use modd_parameters,  only: XUNDEF

use mode_les_diachro, only: Les_diachro

IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
TYPE(TFILEDATA),  INTENT(IN) :: TPDIAFILE! file to write
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
CHARACTER(len=9),   DIMENSION(NMAX_ILES) :: YFIELDNAMES
CHARACTER(len=100), DIMENSION(NMAX_ILES) :: YFIELDCOMMENTS
character(len=:), allocatable          :: ygroup
character(len=:), allocatable          :: ygroupcomment
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
ZLES_BUDGET=XUNDEF
YFIELDNAMES(:)=' '

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
!*      1.  total (resolved+subgrid) kinetic energy budget
!            ------------------------------------
!
ygroup = 'BU_KE'
ygroupcomment = 'Total (resolved+subgrid) kinetic energy budget'
ILES=0
ILES_STA=ILES
!
!* 1.1 tendency
!     --------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_TEND'
YFIELDCOMMENTS(ILES) = 'subgrid tendency'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_SBG_TKE(:,:,NLES_TEND)
!
!
!* 1.2 dynamic production by mean wind gradient
!     ---------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_DP_M'
YFIELDCOMMENTS(ILES) = 'subgrid dynamic production by mean gradient'
!
ZLES_BUDGET(:,:,ILES)= - XLES_SUBGRID_WU (:,:,1) * XLES_MEAN_DUDZ(:,:,1)  &
                       - XLES_SUBGRID_WV (:,:,1) * XLES_MEAN_DVDZ(:,:,1)  &
                       - XLES_SUBGRID_W2 (:,:,1) * XLES_MEAN_DWDZ(:,:,1)
!
!* 1.3 production by wind gradient of resolved motions
!     ------------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_DP_R'
YFIELDCOMMENTS(ILES) = 'subgrid dynamic production by resolved fluctuations'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_SBG_TKE(:,:,NLES_DP) - ZLES_BUDGET(:,:,2)
!
!
!
!* 1.4 advection
!      ---------
!
IF ( ANY(XLES_BU_SBG_TKE(:,:,NLES_ADVM)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_ADVM'
YFIELDCOMMENTS(ILES) = 'subgrid advection by mean flow'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_SBG_TKE(:,:,NLES_ADVM)
END IF
!
!
!* 1.5 forcing
!      -------
!
IF ( ANY(XLES_BU_SBG_TKE(:,:,NLES_FORC)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_FORC'
YFIELDCOMMENTS(ILES) = 'subgrid advection by large-scale W forcing'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_SBG_TKE(:,:,NLES_FORC)
END IF
!
!
!* 1.6 turbulent transport
!      -------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_TR'
YFIELDCOMMENTS(ILES) = 'subgrid turbulent transport'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_SBG_TKE(:,:,NLES_TR)
!
!
!* 1.7 advection by resolved motions
!      -----------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_ADVR'
YFIELDCOMMENTS(ILES) = 'subgrid advection by resolved flow'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_SBG_TKE(:,:,NLES_ADVR)
!
!
!* 1.8 presso-correlations
!     ----------------
!
IF ( ANY(XLES_BU_SBG_TKE(:,:,NLES_PRES)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_PRES'
YFIELDCOMMENTS(ILES) = 'subgrid pressure-correlation'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_SBG_TKE(:,:,NLES_PRES)
END IF
!
!
!* 1.9 thermal production
!      ------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_TP'
YFIELDCOMMENTS(ILES) = 'subgrid thermal production'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_SBG_TKE(:,:,NLES_TP)
!
!
!* 1.10 dissipation
!       -----------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_DISS'
YFIELDCOMMENTS(ILES) = 'subgrid dissipation'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_SBG_TKE(:,:,NLES_DISS)
!
!
!* 1.11 effect of diffusion
!       -------------------
!
IF ( ANY(XLES_BU_SBG_TKE(:,:,NLES_DIFF)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_NUMD'
YFIELDCOMMENTS(ILES) = 'subgrid numerical diffusion'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_SBG_TKE(:,:,NLES_DIFF)
END IF
!
!* 1.12 effect of relaxation
!       --------------------
!
IF ( ANY(XLES_BU_SBG_TKE(:,:,NLES_RELA)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_RELA'
YFIELDCOMMENTS(ILES) = 'subgrid sponge layer relaxation'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_SBG_TKE(:,:,NLES_RELA)
END IF
!
!* 1.13 effect of nesting
!       -----------------
!
IF ( ANY(XLES_BU_SBG_TKE(:,:,NLES_NEST)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_NEST'
YFIELDCOMMENTS(ILES) = 'subgrid average from smaller nested models'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_SBG_TKE(:,:,NLES_NEST)
END IF
!
!
!* 1.14 other effects
!       -------------
!
IF ( ANY(XLES_BU_SBG_TKE(:,:,NLES_MISC)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_MISC'
YFIELDCOMMENTS(ILES) = 'subgrid: other effects'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_SBG_TKE(:,:,NLES_MISC)
END IF
!
!
!* 1.15 residual of subgrid budget
!       --------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_RESI'
YFIELDCOMMENTS(ILES) = 'residual of subgrid budget'
!
ZLES_BUDGET(:,:,ILES) = 0.
DO JLES=ILES_STA+1,ILES-1
  ZLES_BUDGET(:,:,ILES) = ZLES_BUDGET(:,:,ILES) - ZLES_BUDGET(:,:,JLES)
END DO
!
ILES_STA=ILES
!
!* 1.16 tendency
!       --------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_TEND'
YFIELDCOMMENTS(ILES) = 'resolved tendency'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_KE(:,:,NLES_TEND)
!
!
!* 1.17 advection
!       ---------
!
IF ( ANY(XLES_BU_RES_KE(:,:,NLES_ADVM)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_ADV'
YFIELDCOMMENTS(ILES) = 'resolved advection by mean flow'
!
ZLES_BUDGET(:,:,ILES) =  XLES_BU_RES_Ke(:,:,NLES_ADVM)
END IF
!
!
!* 1.18 forcing
!       -------
!
IF ( ANY(XLES_BU_RES_KE(:,:,NLES_FORC)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_FORC'
YFIELDCOMMENTS(ILES) = 'resolved advection by large-scale W forcing'
!
ZLES_BUDGET(:,:,ILES) =  XLES_BU_RES_Ke(:,:,NLES_FORC)
END IF
!
!
!* 1.19 dynamic production by mean wind gradient
!       --------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_DP'
YFIELDCOMMENTS(ILES) = 'resolved dynamic production by mean gradient'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Ke(:,:,NLES_DP)
!
!
!* 1.20 turbulent transport
!       -------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_TR'
YFIELDCOMMENTS(ILES) = 'turbulent transport of resolved flux by itself'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_KE(:,:,NLES_TR)
!
!
!
!* 1.21 presso-correlations
!       -------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_PRES'
YFIELDCOMMENTS(ILES) = 'resolved pressure-correlation'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_KE(:,:,NLES_PRES)
!
!
!* 1.22 thermal production
!       ------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_TP'
YFIELDCOMMENTS(ILES) = 'resolved thermal production'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_KE(:,:,NLES_GRAV)
!
!
!* 1.23 effect of subgrid scale motions on the resolved flow
!       ----------------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_SBGT'
YFIELDCOMMENTS(ILES) = 'resolved sink due to subgrid turbulence'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_KE(:,:,NLES_VTURB) + XLES_BU_RES_KE(:,:,NLES_HTURB)
!
!* 1.24 effect of Coriolis (must be zero)
!       ------------------
!
IF ( ANY(XLES_BU_RES_KE(:,:,NLES_COR)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_CORI'
YFIELDCOMMENTS(ILES) = 'resolved Coriolis effect'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_KE(:,:,NLES_COR)
END IF
!
!* 1.25 effect of diffusion
!       -------------------
!
IF ( ANY(XLES_BU_RES_KE(:,:,NLES_DIFF)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_NUMD'
YFIELDCOMMENTS(ILES) = 'resolved numerical diffusion'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_KE(:,:,NLES_DIFF)
END IF
!
!* 1.26 effect of relaxation
!       --------------------
!
IF ( ANY(XLES_BU_RES_KE(:,:,NLES_RELA)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_RELA'
YFIELDCOMMENTS(ILES) = 'resolved sponge layer relaxation'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_KE(:,:,NLES_RELA)
END IF
!
!* 1.27 effect of nesting
!       -----------------
!
IF ( ANY(XLES_BU_RES_KE(:,:,NLES_NEST)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_NEST'
YFIELDCOMMENTS(ILES) = 'resolved average from smaller nested models'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_KE(:,:,NLES_NEST)
END IF
!
!
!* 1.28 other effects
!       -------------
!
IF ( ANY( XLES_BU_RES_KE(:,:,NLES_MISC) &
         +XLES_BU_RES_KE(:,:,NLES_CURV) /= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_MISC'
YFIELDCOMMENTS(ILES) = 'resolved: other effects'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_KE(:,:,NLES_MISC)  &
                      + XLES_BU_RES_KE(:,:,NLES_CURV)
END IF
!
!* 1.29 residual of resolved Ke budget
!       ------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_RESI'
YFIELDCOMMENTS(ILES) = 'residual of resolved budget'
!
ZLES_BUDGET(:,:,ILES) = 0.
DO JLES=ILES_STA+1,ILES-1
  ZLES_BUDGET(:,:,ILES) = ZLES_BUDGET(:,:,ILES) - ZLES_BUDGET(:,:,JLES)
END DO
!
!* 1.30 writing
!       -------
!
!
tzfield%cmnhname  = ygroup !cmnhname will be overwritten by yfieldnames(:) in Les_diachro
tzfield%cstdname  = ''
tzfield%clongname = ygroup !clongname will be overwritten by yfieldnames(:) in Les_diachro
tzfield%ccomment  = 'resolved KE budget' !ccomment will be completed with yfieldnames(:) in Les_diachro
tzfield%cunits    = 'm2 s-3'

call Les_diachro( tpdiafile, tzfield, ygroup, ygroupcomment, gdoavg, gdonorm, zles_budget(:, :, :iles), &
                  hfieldnames = yfieldnames(:iles), hfieldcomments = yfieldcomments(:iles) )

!-------------------------------------------------------------------------------
!
!
!*      2.  temperature variance budget
!           ---------------------------
!
ygroup = 'BU_THL2'
ygroupcomment = 'Temperature variance budget'
ILES=0
!
ILES_STA=ILES
!
!* 2.1 dynamic production by mean gradients
!      ----------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_DP_M'
YFIELDCOMMENTS(ILES) = 'subgrid dynamic production by mean gradient'
ILES_P1=ILES
!
ZLES_BUDGET(:,:,ILES)= - 2. * XLES_SUBGRID_WThl(:,:,1) * XLES_MEAN_dThldz(:,:,1)
!
!
!* 2.2 turbulent transport
!      -------------------
!
IF ( ANY(XLES_SUBGRID_WThl2(:,:,1)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_TR'
YFIELDCOMMENTS(ILES) = 'subgrid turbulent transport'
!
DO JK=2,NLES_K-1
  ZLES_BUDGET(JK,:,ILES) = - ( XLES_SUBGRID_WThl2 (JK+1,:,1)      &
                              -XLES_SUBGRID_WThl2 (JK-1,:,1)    ) &
                           / ( XLES_Z            (JK+1)           &
                              -XLES_Z            (JK-1)         )
END DO
!
ZLES_BUDGET(1     ,:,ILES) = ZLES_BUDGET(2       ,:,ILES)
ZLES_BUDGET(NLES_K,:,ILES) = ZLES_BUDGET(NLES_K-1,:,ILES)
END IF
!
!* 2.3 production by resolved gradients
!      --------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_DP_R'
YFIELDCOMMENTS(ILES) = 'subgrid dynamic production by resolved fluctuations'
ILES_P2=ILES
!
ZLES_BUDGET(:,:,ILES)= - 2. * XLES_RES_ddxa_Thl_SBG_UaThl(:,:,1)  &
                       - ZLES_BUDGET(:,:,ILES_P1)
!
!
!* 2.4 dissipation
!      -----------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_DISS'
YFIELDCOMMENTS(ILES) = 'subgrid dissipation'
!
ZLES_BUDGET(:,:,ILES) =  XLES_SUBGRID_DISS_Thl2(:,:,1)
!
!
!* 2.5 residual of subgrid budget
!      --------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_RESI'
YFIELDCOMMENTS(ILES) = 'residual of subgrid budget'
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
YFIELDNAMES(ILES)    = 'RES_TEND'
YFIELDCOMMENTS(ILES) = 'resolved tendency'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Thl2(:,:,NLES_TEND)
!
!
!* 2.7 advection
!      ---------
!
IF ( ANY(XLES_BU_RES_Thl2(:,:,NLES_ADVM)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_ADV'
YFIELDCOMMENTS(ILES) = 'resolved advection by mean flow'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Thl2(:,:,NLES_ADVM)
END IF
!
!
!* 2.8 forcing
!      -------
!
IF ( ANY(XLES_BU_RES_Thl2(:,:,NLES_FORC)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_FORC'
YFIELDCOMMENTS(ILES) = 'resolved advection by large-scale W forcing'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Thl2(:,:,NLES_FORC)
END IF
!
!
!* 2.9 production
!      ----------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_DP'
YFIELDCOMMENTS(ILES) = 'resolved dynamic production by mean gradient'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Thl2(:,:,NLES_DP)

!
!* 2.10 turbulent transport
!       -------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_TR'
YFIELDCOMMENTS(ILES) = 'turbulent transport of resolved flux by itself'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Thl2(:,:,NLES_TR)
!
!
!* 2.11 effect of subgrid scale motions on the resolved flow
!       ----------------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_SBGT'
YFIELDCOMMENTS(ILES) = 'resolved sink due to subgrid turbulence'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Thl2(:,:,NLES_VTURB) + XLES_BU_RES_Thl2(:,:,NLES_HTURB)
!
!* 2.12 effect of diffusion
!       -------------------
!
IF ( ANY(XLES_BU_RES_Thl2(:,:,NLES_DIFF)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_NUMD'
YFIELDCOMMENTS(ILES) = 'resolved numerical diffusion'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Thl2(:,:,NLES_DIFF)
END IF
!
!* 2.13 effect of relaxation
!       --------------------
!
IF ( ANY(XLES_BU_RES_Thl2(:,:,NLES_RELA)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_RELA'
YFIELDCOMMENTS(ILES) = 'resolved sponge layer relaxation'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Thl2(:,:,NLES_RELA)
END IF
!
!* 2.14 effect of nesting
!       -----------------
!
IF ( ANY(XLES_BU_RES_Thl2(:,:,NLES_NEST)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_NEST'
YFIELDCOMMENTS(ILES) = 'resolved average from smaller nested models'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Thl2(:,:,NLES_NEST)
END IF
!
!* 2.15 other effects
!       -------------
!
IF ( ANY( XLES_BU_RES_Thl2(:,:,NLES_MISC) &
         +XLES_BU_RES_Thl2(:,:,NLES_RAD ) &
         +XLES_BU_RES_Thl2(:,:,NLES_MICR) &
         + XLES_BU_RES_Thl2(:,:,NLES_PREF) /= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_MISC'
YFIELDCOMMENTS(ILES) = 'resolved: other effects'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_Thl2(:,:,NLES_MISC) &
                      + XLES_BU_RES_Thl2(:,:,NLES_RAD ) &
                      + XLES_BU_RES_Thl2(:,:,NLES_MICR) &
                      + XLES_BU_RES_Thl2(:,:,NLES_PREF)
END IF
!
!
!* 2.16 residual of resolved budget
!       ---------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_RESI'
YFIELDCOMMENTS(ILES) = 'residual of resolved budget'
!
ZLES_BUDGET(:,:,ILES) = 0.
DO JLES=ILES_STA+1,ILES-1
  ZLES_BUDGET(:,:,ILES) = ZLES_BUDGET(:,:,ILES) - ZLES_BUDGET(:,:,JLES)
END DO
!
!* 2.17 neglected term: tendency
!       ------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_TEND'
YFIELDCOMMENTS(ILES) = 'neglected tendency'
!
ZLES_BUDGET(:,:,ILES) = 0.
IF (NLES_TIMES>2) THEN
  DO JK=1,NLES_K
    DO JT=2,NLES_TIMES-1
      ZLES_BUDGET(JK,JT,ILES) =- ( XLES_SUBGRID_Thl2 (JK  ,JT+1,1) &
                                 - XLES_SUBGRID_Thl2 (JK  ,JT-1,1))&
                                / (2.* XLES_TEMP_SAMPLING)
    END DO
    ZLES_BUDGET(JK,1         ,ILES) = ZLES_BUDGET(JK,2           ,ILES)
    ZLES_BUDGET(JK,NLES_TIMES,ILES) = ZLES_BUDGET(JK,NLES_TIMES-1,ILES)
  END DO
END IF
!
!* 2.18 neglected term: advection for subgrid quantity
!       ----------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_ADVM'
YFIELDCOMMENTS(ILES) = 'neglected advection by mean flow'
!
DO JK=2,NLES_K-1
  ZLES_BUDGET(JK,:,ILES)=  -XLES_MEAN_W(JK,:,1)                 &
                         * ( XLES_SUBGRID_Thl2(JK+1,:,1)        &
                            -XLES_SUBGRID_Thl2(JK-1,:,1)      ) &
                         / ( XLES_Z           (JK+1)            &
                            -XLES_Z           (JK-1)          )
END DO
!
ZLES_BUDGET(1     ,:,ILES) = ZLES_BUDGET(2       ,:,ILES)
ZLES_BUDGET(NLES_K,:,ILES) = ZLES_BUDGET(NLES_K-1,:,ILES)
!
!* 2.19 neglected term: advection for subgrid quantity
!       ----------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_ADVR'
YFIELDCOMMENTS(ILES) = 'neglected advection by resolved flow'
!
DO JK=2,NLES_K-1
  ZLES_BUDGET(JK,:,ILES)= - ( XLES_RES_W_SBG_Thl2   (JK+1,:,1)    &
                             -XLES_RES_W_SBG_Thl2   (JK-1,:,1)  ) &
                          / ( XLES_Z            (JK+1)            &
                             -XLES_Z            (JK-1)          )
END DO
!
ZLES_BUDGET(1     ,:,ILES) = ZLES_BUDGET(2       ,:,ILES)
ZLES_BUDGET(NLES_K,:,ILES) = ZLES_BUDGET(NLES_K-1,:,ILES)
!
!
!* 2.16 writing
!       -------
!
!
tzfield%cmnhname  = ygroup !cmnhname will be overwritten by yfieldnames(:) in Les_diachro
tzfield%cstdname  = ''
tzfield%clongname = ygroup !clongname will be overwritten by yfieldnames(:) in Les_diachro
tzfield%ccomment  = 'thetal variance budget' !ccomment will be completed with yfieldnames(:) in Les_diachro
tzfield%cunits    = 'K2 s-1'

call Les_diachro( tpdiafile, tzfield, ygroup, ygroupcomment, gdoavg, gdonorm, zles_budget(:, :, :iles), &
                  hfieldnames = yfieldnames(:iles), hfieldcomments = yfieldcomments(:iles) )

!-------------------------------------------------------------------------------
!
!*      3.  temperature flux budget
!            ---------------------
!
ygroup = 'BU_WTHL'
ygroupcomment = 'Temperature flux budget'
ILES=0
!
ILES_STA=ILES
!
!* 3.1 dynamic production by mean gradients
!     -----------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_DP_M'
YFIELDCOMMENTS(ILES) = 'subgrid dynamic production by mean gradient'
ILES_P1=ILES
!
ZLES_BUDGET(:,:,ILES) =  - XLES_SUBGRID_W2(:,:,1) * XLES_MEAN_dThldz(:,:,1)
!
!
!* 3.2 production by gradient of resolved motions
!     -------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_DP_R'
YFIELDCOMMENTS(ILES) = 'subgrid dynamic production by resolved fluctuations'
!
ZLES_BUDGET(:,:,ILES)=- XLES_RES_ddz_Thl_SBG_W2(:,:,1) &
                      - ZLES_BUDGET(:,:,ILES_P1)
!
!
!* 3.3 turbulent transport
!      -------------------
!
IF ( ANY(XLES_SUBGRID_W2Thl(:,:,1)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_TR'
YFIELDCOMMENTS(ILES) = 'subgrid turbulent transport'
!
DO JK=2,NLES_K-1
  ZLES_BUDGET(JK,:,ILES) = - ( XLES_SUBGRID_W2Thl (JK+1,:,1)       &
                              -XLES_SUBGRID_W2Thl (JK-1,:,1)     ) &
                           / ( XLES_Z            (JK+1)            &
                              -XLES_Z            (JK-1)          )
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
IF ( ANY(XLES_SUBGRID_ThlPz(:,:,1)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_PRES'
YFIELDCOMMENTS(ILES) = 'subgrid pressure-correlation'
!
ZLES_BUDGET(:,:,ILES) =  XLES_SUBGRID_ThlPz(:,:,1)
END IF
!
!
!* 3.5 thermal production
!      ------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_TP'
YFIELDCOMMENTS(ILES) = 'subgrid thermal production'
!
IF (LUSERV) THEN
  ZLES_BUDGET(:,:,ILEs) =  XG * XLES_SUBGRID_ThlThv(:,:,1)   &
                              / XLES_MEAN_Thv      (:,:,1)
ELSE
  ZLES_BUDGET(:,:,ILES) =  XG * XLES_SUBGRID_ThlThv(:,:,1)   &
                              / XLES_MEAN_Th       (:,:,1)
END IF
IF (LOCEAN) THEN
  ZLES_BUDGET(:,:,ILES) =  XG * XLES_SUBGRID_ThlThv(:,:,1) *XALPHAOC 
END IF
!
!* 3.6 residual of subgrid budget
!      --------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_RESI'
YFIELDCOMMENTS(ILES) = 'residual of subgrid budget'
!
ZLES_BUDGET(:,:,ILES) = 0.
DO JLES=ILES_STA+1,ILES-1
  ZLES_BUDGET(:,:,ILES) = ZLES_BUDGET(:,:,ILES) - ZLES_BUDGET(:,:,JLES)
END DO
!
ILES_STA=ILES
!
!* 3.7 tendency
!      --------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_TEND'
YFIELDCOMMENTS(ILES) = 'resolved tendency'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WThl(:,:,NLES_TEND)
!
!* 3.8 advection
!      ---------
!
IF ( ANY(XLES_BU_RES_WThl(:,:,NLES_ADVM)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_ADV'
YFIELDCOMMENTS(ILES) = 'resolved advection by mean flow'
!
ZLES_BUDGET(:,:,ILES) =  XLES_BU_RES_WThl(:,:,NLES_ADVM)
END IF
!
!* 3.9  forcing
!       -------
!
IF ( ANY(XLES_BU_RES_WThl(:,:,NLES_FORC)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_FORC'
YFIELDCOMMENTS(ILES) = 'resolved advection by large-scale W forcing'
!
ZLES_BUDGET(:,:,ILES) =  XLES_BU_RES_WThl(:,:,NLES_FORC)
END IF
!
!
!* 3.10 production by temperature gradient (and vertical wind gradient)
!       ----------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_DP'
YFIELDCOMMENTS(ILES) = 'resolved dynamic production by mean gradient'
!
ZLES_BUDGET(:,:,ILES) =  XLES_BU_RES_WThl(:,:,NLES_DP)
!
!* 3.11 turbulent transport
!       -------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_TR'
YFIELDCOMMENTS(ILES) = 'turbulent transport of resolved flux by itself'
!
ZLES_BUDGET(:,:,ILES) =  XLES_BU_RES_WThl(:,:,NLES_TR)
!
!
!* 3.12 presso-correlations
!       -------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_PRES'
YFIELDCOMMENTS(ILES) = 'resolved pressure-correlation'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WThl(:,:,NLES_PRES)
!
!
!* 3.13 thermal production
!       ------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_TP'
YFIELDCOMMENTS(ILES) = 'resolved thermal production'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WThl(:,:,NLES_GRAV)
!
!
!* 3.14 effect of subgrid scale motions on the resolved flow
!       ----------------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_SBGT'
YFIELDCOMMENTS(ILES) = 'resolved sink due to subgrid turbulence'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WThl(:,:,NLES_VTURB) + XLES_BU_RES_WThl(:,:,NLES_HTURB)
!
!* 3.15 effect of Coriolis
!       ------------------
!
IF ( ANY(XLES_BU_RES_WThl(:,:,NLES_COR)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_CORI'
YFIELDCOMMENTS(ILES) = 'resolved Coriolis effect'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WThl(:,:,NLES_COR)
END IF
!
!* 3.16 effect of diffusion
!       -------------------
!
IF ( ANY(XLES_BU_RES_WThl(:,:,NLES_DIFF)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_NUMD'
YFIELDCOMMENTS(ILES) = 'resolved numerical diffusion'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WThl(:,:,NLES_DIFF)
END IF
!
!* 3.17 effect of relaxation
!       --------------------
!
IF ( ANY(XLES_BU_RES_WThl(:,:,NLES_RELA)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_RELA'
YFIELDCOMMENTS(ILES) = 'resolved sponge layer relaxation'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WThl(:,:,NLES_RELA)
END IF
!
!* 3.18 effect of nesting
!       -----------------
!
IF ( ANY(XLES_BU_RES_WThl(:,:,NLES_NEST)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_NEST'
YFIELDCOMMENTS(ILES) = 'resolved average from smaller nested models'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WThl(:,:,NLES_NEST)
END IF
!
!* 3.19 other effects
!       -------------
!
IF ( ANY( XLES_BU_RES_WThl(:,:,NLES_MISC) &
         +XLES_BU_RES_WThl(:,:,NLES_RAD ) &
         +XLES_BU_RES_WThl(:,:,NLES_MICR) &
         +XLES_BU_RES_WThl(:,:,NLES_PREF) &
         +XLES_BU_RES_WThl(:,:,NLES_CURV) /= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_MISC'
YFIELDCOMMENTS(ILES) = 'resolved: other effects'
!
ZLES_BUDGET(:,:,ILES) = XLES_BU_RES_WThl(:,:,NLES_MISC) &
                      + XLES_BU_RES_WThl(:,:,NLES_RAD ) &
                      + XLES_BU_RES_WThl(:,:,NLES_MICR) &
                      + XLES_BU_RES_WThl(:,:,NLES_PREF) &
                      + XLES_BU_RES_WThl(:,:,NLES_CURV)
END IF
!
!
!* 3.20 residual of resolved Wthl budget
!       --------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_RESI'
YFIELDCOMMENTS(ILES) = 'residual of resolved budget'
!
ZLES_BUDGET(:,:,ILES) = 0.
DO JLES=ILES_STA+1,ILES-1
  ZLES_BUDGET(:,:,ILES) = ZLES_BUDGET(:,:,ILES) - ZLES_BUDGET(:,:,JLES)
END DO
!
!* 3.21 neglected term: tendency
!       ------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_TEND'
YFIELDCOMMENTS(ILES) = 'neglected tendency'
!
ZLES_BUDGET(:,:,ILES) = 0.
IF (NLES_TIMES>2) THEN
  DO JK=1,NLES_K
    DO JT=2,NLES_TIMES-1
      ZLES_BUDGET(JK,JT,ILES) =- ( XLES_SUBGRID_WThl (JK  ,JT+1,1) &
                               - XLES_SUBGRID_WThl (JK  ,JT-1,1))&
                               / (2.* XLES_TEMP_SAMPLING)
    END DO
    ZLES_BUDGET(JK,1         ,ILES) = ZLES_BUDGET(JK,2           ,ILES)
    ZLES_BUDGET(JK,NLES_TIMES,ILES) = ZLES_BUDGET(JK,NLES_TIMES-1,ILES)
  END DO
END IF
!
!
!
!* 3.22 neglected terms : advection for subgrid quantity
!       ------------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_ADVM'
YFIELDCOMMENTS(ILES) = 'neglected advection by mean flow'
!
DO JK=2,NLES_K-1
  ZLES_BUDGET(JK,:,ILES)= - XLES_MEAN_W(JK,:,1)                               &
                              * ( XLES_SUBGRID_WThl(JK+1,:,1)                 &
                                 -XLES_SUBGRID_WThl(JK-1,:,1)               ) &
                             /  ( XLES_Z          (JK+1)                      &
                                 -XLES_Z          (JK-1)                    )
END DO
!
ZLES_BUDGET(1     ,:,ILES) = ZLES_BUDGET(2       ,:,ILES)
ZLES_BUDGET(NLES_K,:,ILES) = ZLES_BUDGET(NLES_K-1,:,ILES)
!
!* 3.23 neglected terms : advection for subgrid quantity
!       ------------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_ADVR'
YFIELDCOMMENTS(ILES) = 'neglected advection by resolved flow'
!
DO JK=2,NLES_K-1
  ZLES_BUDGET(JK,:,ILES)= - ( XLES_RES_W_SBG_WThl(JK+1,:,1)   &
                             -XLES_RES_W_SBG_WThl(JK-1,:,1) ) &
                          / ( XLES_Z          (JK+1)          &
                             -XLES_Z          (JK-1)        )
END DO
!
ZLES_BUDGET(1     ,:,ILES) = ZLES_BUDGET(2       ,:,ILES)
ZLES_BUDGET(NLES_K,:,ILES) = ZLES_BUDGET(NLES_K-1,:,ILES)
!
!* 3.24 neglected terms : production by gradient of vertical velocity for subgrid quantity
!       ----------------------------------------------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_DPGW'
YFIELDCOMMENTS(ILES) = 'neglected production by gradient of vertical velocity for subgrid quantity'
!
ZLES_BUDGET(:,:,ILES)=- XLES_RES_ddxa_W_SBG_UaThl(:,:,1)
!
!
!* 3.25 neglected terms : production by hor. gradient of Thl for subgrid quantity
!       -------------------------------------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_DPGT'
YFIELDCOMMENTS(ILES) = 'neglected production by horizontal gradient of Thl for subgrid quantity'
!
ZLES_BUDGET(:,:,ILES)=-XLES_RES_ddxa_Thl_SBG_UaW(:,:,1)       &
                      -ZLES_BUDGET(:,:,ILES_P1) -ZLES_BUDGET(:,:,ILES_P2)

!
!
!* 3.22 writing
!       -------
!
!
tzfield%cmnhname  = ygroup !cmnhname will be overwritten by yfieldnames(:) in Les_diachro
tzfield%cstdname  = ''
tzfield%clongname = ygroup !clongname will be overwritten by yfieldnames(:) in Les_diachro
tzfield%ccomment  = 'thetal flux budget' !ccomment will be completed with yfieldnames(:) in Les_diachro
tzfield%cunits    = 'm K s-2'

call Les_diachro( tpdiafile, tzfield, ygroup, ygroupcomment, gdoavg, gdonorm, zles_budget(:, :, :iles), &
                  hfieldnames = yfieldnames(:iles), hfieldcomments = yfieldcomments(:iles) )

!-------------------------------------------------------------------------------
!
DEALLOCATE(ZLES_BUDGET)
!
!-------------------------------------------------------------------------------
!
end subroutine Write_les_budget_n

end module mode_write_les_budget_n
