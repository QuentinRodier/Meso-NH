!MNH_LIC Copyright 2002-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!################################
module mode_write_les_sv_budget_n
!################################

implicit none

private

public :: Write_les_sv_budget_n

contains

!############################################
subroutine Write_les_sv_budget_n( tpdiafile )
!############################################
!
!
!!****  *Write_les_sv_budget_n* writes the LES final diagnostics for model _n
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
!  P. Wautelet 14/10/2020: restructure Les_diachro calls to use tfieldmetadata_base type
! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
use modd_conf_n,      only: luserv
use modd_cst,         only: xg
use modd_field,       only: NMNHDIM_BUDGET_LES_LEVEL, NMNHDIM_BUDGET_LES_TIME, NMNHDIM_BUDGET_LES_SV, &
                            NMNHDIM_BUDGET_TERM, NMNHDIM_UNUSED,                                      &
                            tfieldmetadata_base, TYPEREAL
use modd_io,          only: tfiledata
use modd_les,         only: cles_norm_type, nles_k, xles_temp_mean_start, xles_temp_mean_end, xles_temp_sampling
use modd_les_n,       only: nles_times,                                                                     &
                            xles_bu_res_sv2, xles_bu_res_wsv,                                               &
                            xles_mean_dsvdz, xles_mean_dwdz, xles_mean_th, xles_mean_thv, xles_mean_w,      &
                            xles_res_ddxa_sv_sbg_uasv, xles_res_ddxa_sv_sbg_uaw, xles_res_ddxa_w_sbg_uasv,  &
                            xles_res_ddz_sv_sbg_w2, xles_res_w_sbg_wsv, xles_res_w_sbg_sv2,                 &
                            xles_subgrid_diss_sv2, xles_subgrid_sv2, xles_subgrid_svpz, xles_subgrid_svthv, &
                            xles_subgrid_w2, xles_subgrid_wsv, xles_subgrid_wsv2, xles_subgrid_w2sv,        &
                            xles_z
use modd_les_budget,  only: NLES_RELA, NLES_GRAV, NLES_COR, NLES_HTURB, NLES_VTURB, NLES_FORC, NLES_PRES, &
                            NLES_DIFF, NLES_DP, NLES_TR, NLES_TEND, NLES_ADVM, NLES_NEST, NLES_MISC
use modd_nsv,         only: nsv
use modd_parameters,  only: XUNDEF

use mode_les_diachro, only: Les_diachro

IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
TYPE(TFILEDATA),  INTENT(IN) :: TPDIAFILE ! file to write
!
!*      0.2  declaration of local variables
!
integer, parameter :: NMAX_ILES = 50

INTEGER :: ILES
INTEGER :: ILES_STA
INTEGER :: JLES
INTEGER :: ILES_P1, ILES_P2
!
INTEGER :: JK ! vertical loop counter
INTEGER :: JT ! temporal loop counter
INTEGER :: JSV! scalar loop counter
INTEGER :: JP ! process loop counter
!
CHARACTER(len=9),   DIMENSION(NMAX_ILES) :: YFIELDNAMES
CHARACTER(len=100), DIMENSION(NMAX_ILES) :: YFIELDCOMMENTS
character(len=:),   allocatable          :: ygroup
character(len=:),   allocatable          :: ygroupcomment
!
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZLES_BUDGET
!
logical                   :: gdoavg  ! Compute and store time average
logical                   :: gdonorm ! Compute and store normalized field
type(tfieldmetadata_base) :: tzfield
!-------------------------------------------------------------------------------
!
!*          Initializations
!            ---------------
!
ALLOCATE(ZLES_BUDGET(NLES_K,NLES_TIMES,NMAX_ILES,NSV))
!
ZLES_BUDGET(:,:,:,:) = XUNDEF
!-------------------------------------------------------------------------------
!
!*      2.  total scalar variance budget
!           ----------------------------
!
!
ygroup = 'BU_SV2'
ygroupcomment = 'Total scalar variance budget'
!
ILES=0
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
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV)= - 2. * XLES_SUBGRID_WSv(:,:,1,JSV) * XLES_MEAN_dSvdz(:,:,1,JSV)
END DO
!
!
!* 2.2 production by resolved gradients
!      --------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_DP_R'
YFIELDCOMMENTS(ILES) = 'subgrid dynamic production by resolved fluctuations'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV)= - 2. * XLES_RES_ddxa_Sv_SBG_UaSv(:,:,1,JSV)  &
                             - ZLES_BUDGET(:,:,ILES_P1,JSV)
END DO
!
!
!* 2.3 turbulent transport
!      -------------------
!
IF ( ANY(XLES_SUBGRID_WSv2(:,:,1,:)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_TR'
YFIELDCOMMENTS(ILES) = 'subgrid turbulent transport'
!
DO JSV=1,NSV
  DO JK=2,NLES_K-1
    ZLES_BUDGET(JK,:,ILES,JSV) = - ( XLES_SUBGRID_WSv2 (JK+1,:,1,JSV)  &
                                    -XLES_SUBGRID_WSv2 (JK-1,:,1,JSV)) &
                            / ( XLES_Z            (JK+1)          &
                               -XLES_Z            (JK-1)        )
  END DO
  ZLES_BUDGET(1     ,:,ILES,JSV) = ZLES_BUDGET(2       ,:,ILES,JSV)
  ZLES_BUDGET(NLES_K,:,ILES,JSV) = ZLES_BUDGET(NLES_K-1,:,ILES,JSV)
END DO
END IF
!
!
!* 2.4 dissipation
!      -----------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_DISS'
YFIELDCOMMENTS(ILES) = 'subgrid dissipation'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) =  XLES_SUBGRID_DISS_Sv2(:,:,1,JSV)
END DO
!
!
!* 2.5 residual of subgrid budget
!      --------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_RESI'
YFIELDCOMMENTS(ILES) = 'residual of subgrid budget'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = 0.
  DO JLES=ILES_STA+1,ILES-1
    ZLES_BUDGET(:,:,ILES,JSV) = ZLES_BUDGET(:,:,ILES,JSV) - ZLES_BUDGET(:,:,JLES,JSV)
  END DO
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
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_Sv2(:,:,NLES_TEND,JSV)
END DO
!
!* 2.7 advection
!      ---------
!
IF ( ANY(XLES_BU_RES_Sv2(:,:,NLES_ADVM,:)/= 0.) ) THEN
  ILES=ILES+1
  YFIELDNAMES(ILES)    = 'RES_ADV'
  YFIELDCOMMENTS(ILES) = 'resolved advection by mean flow'
!
  DO JSV=1,NSV
    ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_Sv2(:,:,NLES_ADVM,JSV)
  END DO
END IF
!
!* 2.8 forcing
!      -------
!
IF ( ANY(XLES_BU_RES_Sv2(:,:,NLES_FORC,:)/= 0.) ) THEN
  ILES=ILES+1
  YFIELDNAMES(ILES)    = 'RES_FORC'
  YFIELDCOMMENTS(ILES) = 'resolved advection by large-scale W forcing'
!
  DO JSV=1,NSV
    ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_Sv2(:,:,NLES_FORC,JSV)
  END DO
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
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_Sv2(:,:,NLES_DP,JSV)
END DO
!
!* 2.10 turbulent transport
!       -------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_TR'
YFIELDCOMMENTS(ILES) = 'turbulent transport of resolved flux by itself'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_Sv2(:,:,NLES_TR,JSV)
END DO
!
!
!* 2.11 effect of subgrid scale motions on the resolved flow
!       ----------------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_SBGT'
YFIELDCOMMENTS(ILES) = 'resolved sink due to subgrid turbulence'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_Sv2(:,:,NLES_VTURB,JSV) &
                            + XLES_BU_RES_Sv2(:,:,NLES_HTURB,JSV)
END DO
!
!* 2.11 effect of diffusion
!       -------------------
!
IF ( ANY(XLES_BU_RES_Sv2(:,:,NLES_DIFF,:)/= 0.) ) THEN
  ILES=ILES+1
  YFIELDNAMES(ILES)    = 'RES_NUMD'
  YFIELDCOMMENTS(ILES) = 'resolved numerical diffusion'
!
  DO JSV=1,NSV
    ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_Sv2(:,:,NLES_DIFF,JSV)
  END DO
END IF
!
!* 2.11 effect of relaxation
!       --------------------
!
IF ( ANY(XLES_BU_RES_Sv2(:,:,NLES_RELA,:)/= 0.) ) THEN
  ILES=ILES+1
  YFIELDNAMES(ILES)    = 'RES_RELA'
  YFIELDCOMMENTS(ILES) = 'resolved sponge layer relaxation'
!
  DO JSV=1,NSV
    ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_Sv2(:,:,NLES_RELA,JSV)
  END DO
END IF
!
!* 2.11 effect of nesting
!       -----------------
!
IF ( ANY(XLES_BU_RES_Sv2(:,:,NLES_NEST,:)/= 0.) ) THEN
  ILES=ILES+1
  YFIELDNAMES(ILES)    = 'RES_NEST'
  YFIELDCOMMENTS(ILES) = 'resolved average from smaller nested models'
!
  DO JSV=1,NSV
    ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_Sv2(:,:,NLES_NEST,JSV)
  END DO
END IF
!
!* 2.11 other effects
!       -------------
!
IF ( ANY(XLES_BU_RES_Sv2(:,:,NLES_MISC,:)/= 0.) ) THEN
  ILES=ILES+1
  YFIELDNAMES(ILES)    = 'RES_MISC'
  YFIELDCOMMENTS(ILES) = 'resolved: other effects'
  !
  DO JSV=1,NSV
    ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_Sv2(:,:,NLES_MISC,JSV)
  END DO
END IF
!
!* 2.12 residual of resolved budget
!       ---------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_RESI'
YFIELDCOMMENTS(ILES) = 'residual of resolved budget'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = 0.
  DO JLES=ILES_STA+1,ILES-1
    ZLES_BUDGET(:,:,ILES,JSV) = ZLES_BUDGET(:,:,ILES,JSV) - ZLES_BUDGET(:,:,JLES,JSV)
  END DO
END DO
!
!* 2.13 neglected term: tendency
!       ------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_TEND'
YFIELDCOMMENTS(ILES) = 'neglected tendency'
!
IF (NLES_TIMES>2) THEN
  DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = 0.
    DO JK=1,NLES_K
      DO JT=2,NLES_TIMES-1
        ZLES_BUDGET(JK,JT,ILES,JSV) =- ( XLES_SUBGRID_Sv2 (JK  ,JT+1,1,JSV) &
                                       - XLES_SUBGRID_Sv2 (JK  ,JT-1,1,JSV))&
                                      / (2.* XLES_TEMP_SAMPLING)
      END DO
      ZLES_BUDGET(JK,1         ,ILES,JSV) = ZLES_BUDGET(JK,2           ,ILES,JSV)
      ZLES_BUDGET(JK,NLES_TIMES,ILES,JSV) = ZLES_BUDGET(JK,NLES_TIMES-1,ILES,JSV)
    END DO
  END DO
END IF
!
!* 2.14 neglected term: advection for subgrid quantity
!       ----------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_ADVM'
YFIELDCOMMENTS(ILES) = 'neglected advection by mean flow'
!
DO JSV=1,NSV
  DO JK=2,NLES_K-1
    ZLES_BUDGET(JK,:,ILES,JSV)=  -XLES_MEAN_W(JK,:,1)                &
                               * ( XLES_SUBGRID_Sv2(JK+1,:,1,JSV)    &
                                  -XLES_SUBGRID_Sv2(JK-1,:,1,JSV)  ) &
                               / ( XLES_Z          (JK+1)            &
                                  -XLES_Z          (JK-1)          )
  END DO
  ZLES_BUDGET(1     ,:,ILES,JSV) = ZLES_BUDGET(2       ,:,ILES,JSV)
  ZLES_BUDGET(NLES_K,:,ILES,JSV) = ZLES_BUDGET(NLES_K-1,:,ILES,JSV)
END DO
!
!* 2.15 neglected term: advection for subgrid quantity
!       ----------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_ADVR'
YFIELDCOMMENTS(ILES) = 'neglected advection by resolved flow'
!
DO JSV=1,NSV
  DO JK=2,NLES_K-1
    ZLES_BUDGET(JK,:,ILES,JSV)=-( XLES_RES_W_SBG_Sv2   (JK+1,:,1,JSV)    &
                                 -XLES_RES_W_SBG_Sv2   (JK-1,:,1,JSV)  ) &
                              / ( XLES_Z           (JK+1)                &
                                 -XLES_Z           (JK-1)              )
  END DO
  ZLES_BUDGET(1     ,:,ILES,JSV) = ZLES_BUDGET(2       ,:,ILES,JSV)
  ZLES_BUDGET(NLES_K,:,ILES,JSV) = ZLES_BUDGET(NLES_K-1,:,ILES,JSV)
END DO
!
!
!* 2.16 writing
!       -------
!
tzfield%ngrid = 0 !Not on the Arakawa grid
tzfield%ntype = TYPEREAL

tzfield%cmnhname  = ygroup !cmnhname will be overwritten by yfieldnames(:) in Les_diachro
tzfield%cstdname  = ''
tzfield%clongname = ygroup !clongname will be overwritten by yfieldnames(:) in Les_diachro
tzfield%ccomment  = 'Sv variance budget' !ccomment will be completed with yfieldnames(:) in Les_diachro
tzfield%cunits    = 'kg2 kg-2 s-1'

tzfield%ndims = 4
tzfield%ndimlist(1)  = NMNHDIM_BUDGET_LES_LEVEL
tzfield%ndimlist(2)  = NMNHDIM_BUDGET_LES_TIME
tzfield%ndimlist(3)  = NMNHDIM_BUDGET_TERM
tzfield%ndimlist(4)  = NMNHDIM_BUDGET_LES_SV
tzfield%ndimlist(5:) = NMNHDIM_UNUSED

gdoavg  = xles_temp_mean_start /= XUNDEF .and. xles_temp_mean_end /= XUNDEF
gdonorm = trim(cles_norm_type) /= 'NONE'

call Les_diachro( tpdiafile, tzfield, ygroup, ygroupcomment, gdoavg, gdonorm, zles_budget(:, :, :iles, :), &
                  hfieldnames = yfieldnames(:iles), hfieldcomments = yfieldcomments(:iles) )

!-------------------------------------------------------------------------------
!
!*      3.  total water flux budget
!           -----------------------
!
!
ygroup = 'BU_WSV'
ygroupcomment = 'Total water flux budget'
!
!
ILES=0
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
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) =  - XLES_SUBGRID_W2 (:,:,1)     * XLES_MEAN_DSvDZ(:,:,1,JSV) &
                               - XLES_SUBGRID_WSv(:,:,1,JSV) * XLES_MEAN_DWDZ (:,:,1)
END DO
!
!
!* 3.2 production by gradient of resolved motions
!     -------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_DP_R'
YFIELDCOMMENTS(ILES) = 'subgrid dynamic production by resolved fluctuations'
ILES_P2=ILES
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV)=- XLES_RES_ddz_Sv_SBG_W2(:,:,1,JSV) &
                            - ZLES_BUDGET(:,:,ILES_P1,JSV)
END DO
!
!
!
!* 3.3 turbulent transport
!      -------------------
!
IF ( ANY(XLES_SUBGRID_W2Sv(:,:,1,:)/= 0.) ) THEN
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_TR'
YFIELDCOMMENTS(ILES) = 'subgrid turbulent transport'
!
DO JSV=1,NSV
  DO JK=2,NLES_K-1
    ZLES_BUDGET(JK,:,ILES,JSV) = - ( XLES_SUBGRID_W2Sv (JK+1,:,1,JSV)  &
                                    -XLES_SUBGRID_W2Sv (JK-1,:,1,JSV)) &
                                 / ( XLES_Z            (JK+1)          &
                                    -XLES_Z            (JK-1)        )
  END DO
  ZLES_BUDGET(1     ,:,ILES,JSV) = ZLES_BUDGET(2       ,:,ILES,JSV)
  ZLES_BUDGET(NLES_K,:,ILES,JSV) = ZLES_BUDGET(NLES_K-1,:,ILES,JSV)
END DO
END IF
!
!
!* 3.4 presso-correlations
!      -------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_PRES'
YFIELDCOMMENTS(ILES) = 'subgrid pressure-correlation'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) =  XLES_SUBGRID_SvPz(:,:,1,JSV)
END DO
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
  DO JSV=1,NSV
    ZLES_BUDGET(:,:,ILES,JSV) =  XG * XLES_SUBGRID_SvThv(:,:,1,JSV)   &
                                    / XLES_MEAN_Thv     (:,:,1)
  END DO
ELSE
  DO JSV=1,NSV
    ZLES_BUDGET(:,:,ILES,JSV) =  XG * XLES_SUBGRID_SvThv(:,:,1,JSV)   &
                                    / XLES_MEAN_Th      (:,:,1)
  END DO
END IF
!
!
!* 3.6 dissipation
!      -----------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_DISS'
YFIELDCOMMENTS(ILES) = 'subgrid dissipation'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = 0.
END DO
!
!
!* 3.7 residual of subgrid budget
!      --------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'SBG_RESI'
YFIELDCOMMENTS(ILES) = 'residual of subgrid budget'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = 0.
  DO JLES=ILES_STA+1,ILES-1
    ZLES_BUDGET(:,:,ILES,JSV) = ZLES_BUDGET(:,:,ILES,JSV) - ZLES_BUDGET(:,:,JLES,JSV)
  END DO
END DO
!
ILES_STA=ILES
!
!* 3.8 tendency
!      --------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_TEND'
YFIELDCOMMENTS(ILES) = 'resolved tendency'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_WSv(:,:,NLES_TEND,JSV)
END DO
!
!* 3.9 advection
!      ---------
!
IF ( ANY(XLES_BU_RES_WSv(:,:,NLES_ADVM,:)/= 0.) ) THEN
  ILES=ILES+1
  YFIELDNAMES(ILES)    = 'RES_ADV'
  YFIELDCOMMENTS(ILES) = 'resolved advection by mean flow'
  !
  DO JSV=1,NSV
    ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_WSv(:,:,NLES_ADVM,JSV)
  END DO
END IF
!
!* 3.10 forcing
!       -------
!
IF ( ANY(XLES_BU_RES_WSv(:,:,NLES_FORC,:)/= 0.) ) THEN
  ILES=ILES+1
  YFIELDNAMES(ILES)    = 'RES_FORC'
  YFIELDCOMMENTS(ILES) = 'resolved advection by large-scale W forcing'
  !
  DO JSV=1,NSV
    ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_WSv(:,:,NLES_FORC,JSV)
  END DO
END IF
!
!* 3.11 production by temperature gradient (and vertical wind gradient)
!       ----------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_DP'
YFIELDCOMMENTS(ILES) = 'resolved dynamic production by mean gradient'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_WSv(:,:,NLES_DP,JSV)
END DO
!
!* 3.12 turbulent transport
!       -------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_TR'
YFIELDCOMMENTS(ILES) = 'turbulent transport of resolved flux by itself'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_WSv(:,:,NLES_TR,JSV)
END DO
!
!
!* 3.13 presso-correlations
!       -------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_PRES'
YFIELDCOMMENTS(ILES) = 'resolved pressure-correlation'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_WSv(:,:,NLES_PRES,JSV)
END DO
!
!
!* 3.14 thermal production
!       ------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_TP'
YFIELDCOMMENTS(ILES) = 'resolved thermal production'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_WSv(:,:,NLES_GRAV,JSV)
END DO
!
!
!* 3.15 effect of subgrid scale motions on the resolved flow
!       ----------------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_SBGT'
YFIELDCOMMENTS(ILES) = 'resolved sink due to subgrid turbulence'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_WSv(:,:,NLES_VTURB,JSV) + XLES_BU_RES_WSv(:,:,NLES_HTURB,JSV)
END DO
!
!* 3.15 effect of Coriolis
!       ------------------
!
IF ( ANY(XLES_BU_RES_WSv(:,:,NLES_COR,:)/= 0.) ) THEN
  ILES=ILES+1
  YFIELDNAMES(ILES)    = 'RES_CORI'
  YFIELDCOMMENTS(ILES) = 'resolved Coriolis effect'
  !
  DO JSV=1,NSV
    ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_WSv(:,:,NLES_COR,JSV)
  END DO
END IF
!
!* 3.15 effect of diffusion
!       -------------------
!
IF ( ANY(XLES_BU_RES_WSv(:,:,NLES_DIFF,:)/= 0.) ) THEN
  ILES=ILES+1
  YFIELDNAMES(ILES)    = 'RES_NUMD'
  YFIELDCOMMENTS(ILES) = 'resolved numerical diffusion'
  !
  DO JSV=1,NSV
    ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_WSv(:,:,NLES_DIFF,JSV)
  END DO
END IF
!
!* 3.15 effect of relaxation
!       --------------------
!
IF ( ANY(XLES_BU_RES_WSv(:,:,NLES_RELA,:)/= 0.) ) THEN
  ILES=ILES+1
  YFIELDNAMES(ILES)    = 'RES_RELA'
  YFIELDCOMMENTS(ILES) = 'resolved sponge layer relaxation'
  !
  DO JSV=1,NSV
    ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_WSv(:,:,NLES_RELA,JSV)
  END DO
END IF
!
!* 3.15 effect of nesting
!       -----------------
!
IF ( ANY(XLES_BU_RES_WSv(:,:,NLES_NEST,:)/= 0.) ) THEN
  ILES=ILES+1
  YFIELDNAMES(ILES)    = 'RES_NEST'
  YFIELDCOMMENTS(ILES) = 'resolved average from smaller nested models'
  !
  DO JSV=1,NSV
    ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_WSv(:,:,NLES_NEST,JSV)
  END DO
END IF
!
!* 3.15 other effects
!       -------------
!
IF ( ANY(XLES_BU_RES_WSv(:,:,NLES_MISC,:)/= 0.) ) THEN
  ILES=ILES+1
  YFIELDNAMES(ILES)    = 'RES_MISC'
  YFIELDCOMMENTS(ILES) = 'resolved: other effects'
  !
  DO JSV=1,NSV
    ZLES_BUDGET(:,:,ILES,JSV) = XLES_BU_RES_WSv(:,:,NLES_MISC,JSV)
  END DO
END IF
!
!* 3.16 residual of resolved WSv budget
!       -------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'RES_RESI'
YFIELDCOMMENTS(ILES) = 'residual of resolved budget'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = 0.
  DO JLES=ILES_STA+1,ILES-1
    ZLES_BUDGET(:,:,ILES,JSV) = ZLES_BUDGET(:,:,ILES,JSV) - ZLES_BUDGET(:,:,JLES,JSV)
  END DO
END DO
!
!* 3.17 neglected term: tendency
!       ------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_TEND'
YFIELDCOMMENTS(ILES) = 'neglected tendency'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV) = 0.
  IF (NLES_TIMES>2) THEN
    DO JK=1,NLES_K
      DO JT=2,NLES_TIMES-1
        ZLES_BUDGET(JK,JT,ILES,JSV) =- ( XLES_SUBGRID_WSv (JK  ,JT+1,1,JSV) &
                                       - XLES_SUBGRID_WSv (JK  ,JT-1,1,JSV))&
                                       / (2.* XLES_TEMP_SAMPLING)
      END DO
      ZLES_BUDGET(JK,1         ,ILES,JSV) = ZLES_BUDGET(JK,2           ,ILES,JSV)
      ZLES_BUDGET(JK,NLES_TIMES,ILES,JSV) = ZLES_BUDGET(JK,NLES_TIMES-1,ILES,JSV)
    END DO
  END IF
END DO
!
!
!
!* 3.18 neglected terms : advection for subgrid quantity
!       ------------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_ADVM'
YFIELDCOMMENTS(ILES) = 'neglected advection by mean flow'
!
DO JSV=1,NSV
  DO JK=2,NLES_K-1
    ZLES_BUDGET(JK,:,ILES,JSV)= - XLES_MEAN_W(JK,:,1)                       &
                                  * ( XLES_SUBGRID_WSv(JK+1,:,1,JSV)        &
                                     -XLES_SUBGRID_WSv(JK-1,:,1,JSV)      ) &
                                 /  ( XLES_Z          (JK+1)                &
                                     -XLES_Z          (JK-1)              )
  END DO
  ZLES_BUDGET(1     ,:,ILES,JSV) = ZLES_BUDGET(2       ,:,ILES,JSV)
  ZLES_BUDGET(NLES_K,:,ILES,JSV) = ZLES_BUDGET(NLES_K-1,:,ILES,JSV)
END DO
!
!* 3.19 neglected terms : advection for subgrid quantity
!       ------------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_ADVR'
YFIELDCOMMENTS(ILES) = 'neglected advection by resolved flow'
!
DO JSV=1,NSV
  DO JK=2,NLES_K-1
    ZLES_BUDGET(JK,:,ILES,JSV)=-( XLES_RES_W_SBG_WSv(JK+1,:,1,JSV)   &
                                 -XLES_RES_W_SBG_WSv(JK-1,:,1,JSV) ) &
                              / ( XLES_Z          (JK+1)             &
                                 -XLES_Z          (JK-1)           )
  END DO
  ZLES_BUDGET(1     ,:,ILES,JSV) = ZLES_BUDGET(2       ,:,ILES,JSV)
  ZLES_BUDGET(NLES_K,:,ILES,JSV) = ZLES_BUDGET(NLES_K-1,:,ILES,JSV)
END DO
!
!* 3.20 neglected terms : production by gradient of vertical velocity for subgrid quantity
!       ----------------------------------------------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_DPGW'
YFIELDCOMMENTS(ILES) = 'neglected production by gradient of vertical velocity for subgrid quantity'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV)=- XLES_RES_ddxa_W_SBG_UaSv(:,:,1,JSV)
END DO
!
!
!* 3.21 neglected terms : production by hor. gradient of Thl for subgrid quantity
!       -------------------------------------------------------------------------
!
ILES=ILES+1
YFIELDNAMES(ILES)    = 'NSG_DPGT'
YFIELDCOMMENTS(ILES) = 'neglected production by horizontal gradient of Thl for subgrid quantity'
!
DO JSV=1,NSV
  ZLES_BUDGET(:,:,ILES,JSV)=-XLES_RES_ddxa_Sv_SBG_UaW(:,:,1,JSV)       &
                            -ZLES_BUDGET(:,:,ILES_P1,JSV) -ZLES_BUDGET(:,:,ILES_P2,JSV)
END DO
!
!
!* 3.22 writing
!       -------
!
tzfield%ngrid = 0 !Not on the Arakawa grid
tzfield%ntype = TYPEREAL

tzfield%cmnhname  = ygroup !cmnhname will be overwritten by yfieldnames(:) in Les_diachro
tzfield%cstdname  = ''
tzfield%clongname = ygroup !clongname will be overwritten by yfieldnames(:) in Les_diachro
tzfield%ccomment  = 'Sv flux budget' !ccomment will be completed with yfieldnames(:) in Les_diachro
tzfield%cunits    = 'm kg kg-1 s-2'

tzfield%ndims = 4
tzfield%ndimlist(1)  = NMNHDIM_BUDGET_LES_LEVEL
tzfield%ndimlist(2)  = NMNHDIM_BUDGET_LES_TIME
tzfield%ndimlist(3)  = NMNHDIM_BUDGET_TERM
tzfield%ndimlist(4)  = NMNHDIM_BUDGET_LES_SV
tzfield%ndimlist(5:) = NMNHDIM_UNUSED

gdoavg  = xles_temp_mean_start /= XUNDEF .and. xles_temp_mean_end /= XUNDEF
gdonorm = trim(cles_norm_type) /= 'NONE'

call Les_diachro( tpdiafile, tzfield, ygroup, ygroupcomment, gdoavg, gdonorm, zles_budget(:, :, :iles, :), &
                  hfieldnames = yfieldnames(:iles), hfieldcomments = yfieldcomments(:iles) )

!-------------------------------------------------------------------------------
!
DEALLOCATE(ZLES_BUDGET)
!
!-------------------------------------------------------------------------------
!
end subroutine Write_les_sv_budget_n

end module mode_write_les_sv_budget_n
