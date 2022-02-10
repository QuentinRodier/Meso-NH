
!Note de phasage pour Méso-NH: dans resolved_cloud, il faut:
! - récuperer le tbudgets de modd_budget pour le passer ici, kbudgets vaut size(tbudgets)
! - passer OCND2=.FALSE.
! - passer à KPROMA la même valeur que KSIZE (bug sinon)
! - créer des tableaux temporaires pour PSIGS, PINDEP, PINPRC pour gérer les options


!MNH_LIC Copyright 1995-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ######spl
       MODULE MODI_RAIN_ICE_RED
!      ########################
!
INTERFACE
      SUBROUTINE RAIN_ICE_RED ( D, CST, PARAMI, ICEP, ICED, BUCONF,               &
                            KPROMA, KSIZE,                                        &
                            OSEDIC, OCND2, HSEDIM, HSUBG_AUCV_RC, HSUBG_AUCV_RI,  &
                            OWARM,                                                &
                            PTSTEP, KRR, ODMICRO, PEXN,                           &
                            PDZZ, PRHODJ, PRHODREF, PEXNREF, PPABST, PCIT, PCLDFR,&
                            PHLC_HRC, PHLC_HCF, PHLI_HRI, PHLI_HCF,               &
                            PTHT, PRVT, PRCT, PRRT, PRIT, PRST,                   &
                            PRGT, PTHS, PRVS, PRCS, PRRS, PRIS, PRSS, PRGS,       &
                            PINPRC,PINPRR, PEVAP3D,                               &
                            PINPRS, PINPRG, PINDEP, PRAINFR, PSIGS, PSEA, PTOWN,  &
                            PRHT, PRHS, PINPRH, PFPR,                             &
                            TBUDGETS, KBUDGETS)
!
!
USE MODD_BUDGET, ONLY: TBUDGETDATA, TBUDGETCONF_t
USE MODD_DIMPHYEX,   ONLY: DIMPHYEX_t
USE MODD_CST,            ONLY: CST_t
USE MODD_PARAM_ICE,      ONLY: PARAM_ICE_t
USE MODD_RAIN_ICE_DESCR, ONLY: RAIN_ICE_DESCR_t
USE MODD_RAIN_ICE_PARAM, ONLY: RAIN_ICE_PARAM_t
TYPE(DIMPHYEX_t),         INTENT(IN)    :: D
TYPE(CST_t),              INTENT(IN)    :: CST
TYPE(PARAM_ICE_t),        INTENT(IN)    :: PARAMI
TYPE(RAIN_ICE_PARAM_t),   INTENT(IN)    :: ICEP
TYPE(RAIN_ICE_DESCR_t),   INTENT(IN)    :: ICED
TYPE(TBUDGETCONF_t),      INTENT(IN)    :: BUCONF
INTEGER,                  INTENT(IN)    :: KPROMA ! cache-blocking factor for microphysic loop
INTEGER,                  INTENT(IN)    :: KSIZE
LOGICAL,                  INTENT(IN)    :: OSEDIC ! Switch for droplet sedim.
LOGICAL                                 :: OCND2  ! Logical switch to separate liquid and ice
CHARACTER(LEN=4),         INTENT(IN)    :: HSEDIM ! Sedimentation scheme
CHARACTER(LEN=4),         INTENT(IN)    :: HSUBG_AUCV_RC ! Switch for rc->rr Subgrid autoconversion
                                        ! Kind of Subgrid autoconversion method
CHARACTER(LEN=80),        INTENT(IN)    :: HSUBG_AUCV_RI ! Switch for ri->rs Subgrid autoconversion
                                        ! Kind of Subgrid autoconversion method
LOGICAL,                  INTENT(IN)    :: OWARM   ! .TRUE. allows raindrops to
                                                   !   form by warm processes
                                                   !      (Kessler scheme)
!
REAL,                     INTENT(IN)    :: PTSTEP  ! Double Time step
                                                   ! (single if cold start)
INTEGER,                  INTENT(IN)    :: KRR     ! Number of moist variable
LOGICAL, DIMENSION(D%NIT,D%NJT,D%NKT), INTENT(IN)   :: ODMICRO ! mask to limit computation
!
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PEXN    ! Exner function
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PDZZ    ! Layer thikness (m)
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRHODJ  ! Dry density * Jacobian
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRHODREF! Reference density
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PEXNREF ! Reference Exner function
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PPABST  ! absolute pressure at t
!
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PCIT    ! Pristine ice n.c. at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PCLDFR  ! Cloud fraction
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PHLC_HRC
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PHLC_HCF
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PHLI_HRI
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PHLI_HCF
!
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PTHT    ! Theta at time t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRVT    ! Water vapor m.r. at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRCT    ! Cloud water m.r. at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRRT    ! Rain water m.r. at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRIT    ! Pristine ice m.r. at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRST    ! Snow/aggregate m.r. at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRGT    ! Graupel/hail m.r. at t
!
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PTHS    ! Theta source
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PRVS    ! Water vapor m.r. source
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PRCS    ! Cloud water m.r. source
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PRRS    ! Rain water m.r. source
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PRIS    ! Pristine ice m.r. source
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PRSS    ! Snow/aggregate m.r. source
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PRGS    ! Graupel m.r. source

!
REAL, DIMENSION(D%NIT,D%NJT), INTENT(OUT)           :: PINPRC! Cloud instant precip
REAL, DIMENSION(D%NIT,D%NJT), INTENT(OUT)       :: PINPRR! Rain instant precip
REAL, DIMENSION(D%NIT,D%NJT,D%NKT), INTENT(OUT)     :: PEVAP3D! Rain evap profile
REAL, DIMENSION(D%NIT,D%NJT), INTENT(OUT)       :: PINPRS! Snow instant precip
REAL, DIMENSION(D%NIT,D%NJT), INTENT(OUT)       :: PINPRG! Graupel instant precip
REAL, DIMENSION(D%NIT,D%NJT), INTENT(OUT)           :: PINDEP  ! Cloud instant deposition
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(OUT) :: PRAINFR
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)        :: PSIGS   ! Sigma_s at t
REAL, DIMENSION(D%NIT,D%NJT), OPTIONAL, INTENT(IN)        :: PSEA ! Sea Mask
REAL, DIMENSION(D%NIT,D%NJT), OPTIONAL, INTENT(IN)        :: PTOWN! Fraction that is town
REAL, DIMENSION(D%NIT,D%NJT,D%NKT), OPTIONAL,  INTENT(IN)    :: PRHT    ! Hail m.r. at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT), OPTIONAL,  INTENT(INOUT) :: PRHS    ! Hail m.r. source
REAL, DIMENSION(D%NIT,D%NJT), OPTIONAL, INTENT(OUT)      :: PINPRH! Hail instant precip
REAL, DIMENSION(D%NIT,D%NJT,D%NKT,KRR), OPTIONAL, INTENT(OUT)  :: PFPR ! upper-air precipitation fluxes
!
TYPE(TBUDGETDATA), OPTIONAL, DIMENSION(KBUDGETS), INTENT(INOUT) :: TBUDGETS
INTEGER, INTENT(IN) : KBUDGETS
!
END SUBROUTINE RAIN_ICE_RED
END INTERFACE
END MODULE MODI_RAIN_ICE_RED
!     ######spl
      SUBROUTINE RAIN_ICE_RED (D, CST, PARAMI, ICEP, ICED, BUCONF,                &
                            KPROMA, KSIZE,                                        &
                            OSEDIC, OCND2, HSEDIM, HSUBG_AUCV_RC, HSUBG_AUCV_RI,  &
                            OWARM, KKA, KKU, KKL,                                 &
                            PTSTEP, KRR, ODMICRO, PEXN,                           &
                            PDZZ, PRHODJ, PRHODREF, PEXNREF, PPABST, PCIT, PCLDFR,&
                            PHLC_HRC, PHLC_HCF, PHLI_HRI,  PHLI_HCF,              &
                            PTHT, PRVT, PRCT, PRRT, PRIT, PRST,                   &
                            PRGT, PTHS, PRVS, PRCS, PRRS, PRIS, PRSS, PRGS,       &
                            PINPRC, PINPRR, PEVAP3D,                              &
                            PINPRS, PINPRG, PINDEP, PRAINFR, PSIGS, PSEA, PTOWN,  &
                            PRHT, PRHS, PINPRH, PFPR,                             &
                            TBUDGETS, KBUDGETS)
!     ######################################################################
!
!!****  * -  compute the explicit microphysical sources
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to compute the slow microphysical sources
!!    which can be computed explicitly
!!
!!
!!**  METHOD
!!    ------
!!      The autoconversion computation follows Kessler (1969).
!!      The sedimentation rate is computed with a time spliting technique and
!!    an upstream scheme, written as a difference of non-advective fluxes. This
!!    source term is added to the future instant ( split-implicit process ).
!!      The others microphysical processes are evaluated at the central instant
!!    (split-explicit process ): autoconversion, accretion and rain evaporation.
!!      These last 3 terms are bounded in order not to create negative values
!!    for the water species at the future instant.
!!
!!    EXTERNAL
!!    --------
!!      None
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_PARAMETERS
!!          JPHEXT       : Horizontal external points number
!!          JPVEXT       : Vertical external points number
!!      Module MODD_CONF :
!!          CCONF configuration of the model for the first time step
!!      Module MODD_CST
!!          XP00               ! Reference pressure
!!          XRD,XRV            ! Gaz  constant for dry air, vapor
!!          XMD,XMV            ! Molecular weight for dry air, vapor
!!          XCPD               ! Cpd (dry air)
!!          XCL                ! Cl (liquid)
!!          XCI                ! Ci (solid)
!!          XTT                ! Triple point temperature
!!          XLVTT              ! Vaporization heat constant
!!          XALPW,XBETAW,XGAMW ! Constants for saturation vapor pressure
!!                               function over liquid water
!!          XALPI,XBETAI,XGAMI ! Constants for saturation vapor pressure
!!                               function over solid ice
!!      Module MODD_BUDGET:
!!         NBUMOD       : model in which budget is calculated
!!         CBUTYPE      : type of desired budget
!!                          'CART' for cartesian box configuration
!!                          'MASK' for budget zone defined by a mask
!!                          'NONE'  ' for no budget
!!         LBU_RTH      : logical for budget of RTH (potential temperature)
!!                        .TRUE. = budget of RTH
!!                        .FALSE. = no budget of RTH
!!         LBU_RRV      : logical for budget of RRV (water vapor)
!!                        .TRUE. = budget of RRV
!!                        .FALSE. = no budget of RRV
!!         LBU_RRC      : logical for budget of RRC (cloud water)
!!                        .TRUE. = budget of RRC
!!                        .FALSE. = no budget of RRC
!!         LBU_RRI      : logical for budget of RRI (cloud ice)
!!                        .TRUE. = budget of RRI
!!                        .FALSE. = no budget of RRI
!!         LBU_RRR      : logical for budget of RRR (rain water)
!!                        .TRUE. = budget of RRR
!!                        .FALSE. = no budget of RRR
!!         LBU_RRS      : logical for budget of RRS (aggregates)
!!                        .TRUE. = budget of RRS
!!                        .FALSE. = no budget of RRS
!!         LBU_RRG      : logical for budget of RRG (graupeln)
!!                        .TRUE. = budget of RRG
!!                        .FALSE. = no budget of RRG
!!
!!    REFERENCE
!!    ---------
!!
!!      Book1 and Book2 of documentation ( routine RAIN_ICE )
!!
!!    AUTHOR
!!    ------
!!      J.-P. Pinty      * Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/11/95
!!      (J.Viviand) 04/02/97  debug accumulated prcipitation & convert
!!                            precipitation rate in m/s
!!      (J.-P. Pinty) 17/02/97  add budget calls
!!      (J.-P. Pinty) 17/11/97  set ice sedim. for cirrus ice, reset RCHONI
!!                              and RRHONG, reverse order for DEALLOCATE
!!      (J.-P. Pinty) 11/02/98  correction of the air dynamical viscosity and
!!                              add advance of the budget calls
!!      (J.-P. Pinty) 18/05/98  correction of the air density in the RIAUTS
!!                              process
!!      (J.-P. Pinty) 18/11/98  split the main routine
!!      (V. Masson)   18/11/98  bug in IVEC1 and IVEC2 upper limits
!!      (J. Escobar & J.-P. Pinty)
!!                    11/12/98  contains and rewrite count+pack
!!      (J. Stein & J.-P. Pinty)
!!                    14/10/99  correction for very small RIT
!!      (J. Escobar & J.-P. Pinty)
!!                    24/07/00  correction for very samll m.r. in
!!                              the sedimentation subroutine
!!      (M. Tomasini) 11/05/01  Autoconversion of rc into rr modification to take
!!                              into account the subgrid variance
!!                              (cf Redelsperger & Sommeria JAS 86)
!!      (G. Molinie)  21/05/99  bug in RRCFRIG process, RHODREF**(-1) missing
!!                              in RSRIMCG
!!      (G. Molinie & J.-P. Pinty)
!!                    21/06/99  bug in RACCS process
!!      (P. Jabouille) 27/05/04 safety test for case where esw/i(T)> pabs (~Z>40km)
!!      (J-.P. Chaboureau) 12/02/05  temperature depending ice-to-snow autocon-
!                              version threshold (Chaboureau and Pinty GRL 2006)
!!      (J.-P. Pinty) 01/01/O1  add the hail category and correction of the
!!                              wet growth rate of the graupeln
!!      (S.Remy & C.Lac) 06/06 Add the cloud sedimentation
!!      (S.Remy & C.Lac) 06/06 Sedimentation becoming the last process
!!      to settle the precipitating species created during the current time step
!!      (S.Remy & C.Lac) 06/06 Modification of the algorithm of sedimentation
!!      to settle n times the precipitating species created during Dt/n instead
!!      of Dt
!!      (C.Lac) 11/06 Optimization of the sedimentation loop for NEC
!!      (J.Escobar) 18/01/2008 Parallel Bug in Budget when IMICRO >= 1
!!                  --> Path inhibit this test by IMICRO >= 0 allway true
!!      (Y.Seity) 03/2008 Add Statistic sedimentation
!!      (Y.Seity) 10/2009 Added condition for the raindrop accretion of the aggregates
!!         into graupeln process (5.2.6) to avoid negative graupel mixing ratio
!!      (V.Masson, C.Lac) 09/2010 Correction in split sedimentation for
!!                                reproducibility
!!      (S. Riette) Oct 2010 Better vectorisation of RAIN_ICE_SEDIMENTATION_STAT
!!      (Y. Seity), 02-2012  add possibility to run with reversed vertical levels
!!      (L. Bengtsson), 02-2013 Passing in land/sea mask and town fraction in
!!                      order to use different cloud droplet number conc. over
!!                      land, sea and urban areas in the cloud sedimentation.
!!      (D. Degrauwe), 2013-11: Export upper-air precipitation fluxes PFPR.
!!      (S. Riette) Nov 2013 Protection against null sigma
!!      (C. Lac) FIT temporal scheme : instant M removed
!!      (JP Pinty), 01-2014 : ICE4 : partial reconversion of hail to graupel
!!              July, 2015 (O.Nuissier/F.Duffourg) Add microphysics diagnostic for
!!                                      aircraft, ballon and profiler
!!      J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1
!!      C.Lac : 10/2016 : add droplet deposition
!!      C.Lac : 01/2017 : correction on droplet deposition
!!      J.Escobar : 10/2017 : for real*4 , limit exp() in RAIN_ICE_SLOW with XMNH_HUGE_12_LOG
!!      (C. Abiven, Y. Léauté, V. Seigner, S. Riette) Phasing of Turner rain subgrid param
!!      (S. Riette) Source code split into several files
!!                  02/2019 C.Lac add rain fraction as an output field
!  P. Wautelet 10/04/2019: replace ABORT and STOP calls by Print_msg
!  P. Wautelet 28/05/2019: move COUNTJV function to tools.f90
!  P. Wautelet 29/05/2019: remove PACK/UNPACK intrinsics (to get more performance and better OpenACC support)
!  P. Wautelet 17/01/2020: move Quicksort to tools.f90
!  P. Wautelet    02/2020: use the new data structures and subroutines for budgets
!  P. Wautelet 25/02/2020: bugfix: add missing budget: WETH_BU_RRG
!!     R. El Khatib 24-Aug-2021 Optimizations
!-----------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK

USE MODD_DIMPHYEX,   ONLY: DIMPHYEX_t
USE MODD_BUDGET,         ONLY: TBUDGETDATA, TBUDGETCONF_t, NBUDGET_TH, NBUDGET_RV, NBUDGET_RC, &
                               NBUDGET_RI, NBUDGET_RR, NBUDGET_RS, NBUDGET_RG, NBUDGET_RH
USE MODD_CST,            ONLY: CST_t
USE MODD_PARAM_ICE,      ONLY: PARAM_ICE_t
USE MODD_RAIN_ICE_DESCR, ONLY: RAIN_ICE_DESCR_t
USE MODD_RAIN_ICE_PARAM, ONLY: RAIN_ICE_PARAM_t
USE MODD_FIELDS_ADDRESS, ONLY : & ! common fields adress
      & ITH,     & ! Potential temperature
      & IRV,     & ! Water vapor
      & IRC,     & ! Cloud water
      & IRR,     & ! Rain water
      & IRI,     & ! Pristine ice
      & IRS,     & ! Snow/aggregate
      & IRG,     & ! Graupel
      & IRH        ! Hail

USE MODE_BUDGET,         ONLY: BUDGET_STORE_ADD, BUDGET_STORE_INIT, BUDGET_STORE_END
USE MODE_MSG,            ONLY: PRINT_MSG, NVERB_FATAL

USE MODE_ICE4_RAINFR_VERT, ONLY: ICE4_RAINFR_VERT
USE MODE_ICE4_SEDIMENTATION_STAT, ONLY: ICE4_SEDIMENTATION_STAT
USE MODE_ICE4_SEDIMENTATION_SPLIT, ONLY: ICE4_SEDIMENTATION_SPLIT
USE MODE_ICE4_SEDIMENTATION_SPLIT_MOMENTUM, ONLY: ICE4_SEDIMENTATION_SPLIT_MOMENTUM
USE MODE_ICE4_TENDENCIES, ONLY: ICE4_TENDENCIES
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!
!
TYPE(DIMPHYEX_t),         INTENT(IN)    :: D
TYPE(CST_t),              INTENT(IN)    :: CST
TYPE(PARAM_ICE_t),        INTENT(IN)    :: PARAMI
TYPE(RAIN_ICE_PARAM_t),   INTENT(IN)    :: ICEP
TYPE(RAIN_ICE_DESCR_t),   INTENT(IN)    :: ICED
TYPE(TBUDGETCONF_t),      INTENT(IN)    :: BUCONF
INTEGER,                  INTENT(IN)    :: KPROMA ! cache-blocking factor for microphysic loop
INTEGER,                  INTENT(IN)    :: KSIZE
LOGICAL,                  INTENT(IN)    :: OSEDIC ! Switch for droplet sedim.
LOGICAL                                 :: OCND2  ! Logical switch to separate liquid and ice
CHARACTER(LEN=4),         INTENT(IN)    :: HSEDIM ! Sedimentation scheme
CHARACTER(LEN=4),         INTENT(IN)    :: HSUBG_AUCV_RC ! Kind of Subgrid autoconversion method
CHARACTER(LEN=80),        INTENT(IN)    :: HSUBG_AUCV_RI ! Kind of Subgrid autoconversion method
LOGICAL,                  INTENT(IN)    :: OWARM   ! .TRUE. allows raindrops to
                                                   !   form by warm processes
                                                   !      (Kessler scheme)
REAL,                     INTENT(IN)    :: PTSTEP  ! Double Time step (single if cold start)
INTEGER,                  INTENT(IN)    :: KRR     ! Number of moist variable
LOGICAL, DIMENSION(D%NIT,D%NJT,D%NKT), INTENT(IN)   :: ODMICRO ! mask to limit computation
!
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PEXN    ! Exner function
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PDZZ    ! Layer thikness (m)
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRHODJ  ! Dry density * Jacobian
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRHODREF! Reference density
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PEXNREF ! Reference Exner function
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PPABST  ! absolute pressure at t
!
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PCIT    ! Pristine ice n.c. at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PCLDFR  ! Convective Mass Flux Cloud fraction
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PHLC_HRC
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PHLC_HCF
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PHLI_HRI
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PHLI_HCF
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PTHT    ! Theta at time t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRVT    ! Water vapor m.r. at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRCT    ! Cloud water m.r. at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRRT    ! Rain water m.r. at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRIT    ! Pristine ice m.r. at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRST    ! Snow/aggregate m.r. at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PRGT    ! Graupel/hail m.r. at t
!
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PTHS    ! Theta source
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PRVS    ! Water vapor m.r. source
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PRCS    ! Cloud water m.r. source
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PRRS    ! Rain water m.r. source
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PRIS    ! Pristine ice m.r. source
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PRSS    ! Snow/aggregate m.r. source
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(INOUT) :: PRGS    ! Graupel m.r. source
!
REAL, DIMENSION(D%NIT,D%NJT), INTENT(OUT)       :: PINPRC! Cloud instant precip
REAL, DIMENSION(D%NIT,D%NJT), INTENT(OUT)       :: PINPRR! Rain instant precip
REAL, DIMENSION(D%NIT,D%NJT,D%NKT), INTENT(OUT)     :: PEVAP3D! Rain evap profile
REAL, DIMENSION(D%NIT,D%NJT), INTENT(OUT)       :: PINPRS! Snow instant precip
REAL, DIMENSION(D%NIT,D%NJT), INTENT(OUT)       :: PINPRG! Graupel instant precip
REAL, DIMENSION(D%NIT,D%NJT),     INTENT(OUT)       :: PINDEP  ! Cloud instant deposition
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(OUT) :: PRAINFR !Precipitation fraction
REAL, DIMENSION(D%NIT,D%NJT,D%NKT),   INTENT(IN)    :: PSIGS   ! Sigma_s at t
REAL, DIMENSION(D%NIT,D%NJT), OPTIONAL, INTENT(IN)        :: PSEA ! Sea Mask
REAL, DIMENSION(D%NIT,D%NJT), OPTIONAL, INTENT(IN)        :: PTOWN! Fraction that is town
REAL, DIMENSION(D%NIT,D%NJT,D%NKT), OPTIONAL,  INTENT(IN)    :: PRHT    ! Hail m.r. at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT), OPTIONAL,  INTENT(INOUT) :: PRHS    ! Hail m.r. source
REAL, DIMENSION(D%NIT,D%NJT), OPTIONAL, INTENT(OUT)      :: PINPRH! Hail instant precip
REAL, DIMENSION(D%NIT,D%NJT,D%NKT,KRR), OPTIONAL, INTENT(OUT)  :: PFPR ! upper-air precipitation fluxes
!
TYPE(TBUDGETDATA), DIMENSION(KBUDGETS), INTENT(INOUT) :: TBUDGETS
INTEGER, INTENT(IN) :: KBUDGETS
!
!*       0.2   Declarations of local variables :
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
INTEGER :: JI, JJ, JK
INTEGER :: ISTI, ISTJ, ISTK
!
!Arrays for nucleation call outisde of ODMICRO points
REAL,    DIMENSION(D%NIT, D%NJT, D%NKT) :: ZW ! work array
REAL,    DIMENSION(D%NIT, D%NJT, D%NKT) :: ZT ! Temperature
REAL, DIMENSION(D%NIT, D%NJT, D%NKT) :: ZZ_RVHENI_MR, & ! heterogeneous nucleation mixing ratio change
                                & ZZ_RVHENI       ! heterogeneous nucleation
REAL, DIMENSION(MERGE(D%NIT, 0, BUCONF%LBU_ENABLE), &
               &MERGE(D%NJT, 0, BUCONF%LBU_ENABLE), &
               &MERGE(D%NKT, 0, BUCONF%LBU_ENABLE)) :: ZW1, ZW2, ZW3, ZW4, ZW5, ZW6 !Work arrays
REAL, DIMENSION(D%NIT, D%NJT, D%NKT) :: ZZ_LVFACT, ZZ_LSFACT, ZZ_DIFF
!
REAL, DIMENSION(D%NIT,D%NJT,D%NKT) :: ZRCT    ! Cloud water m.r. source at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT) :: ZRRT    ! Rain water m.r. source at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT) :: ZRIT    ! Pristine ice m.r. source at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT) :: ZRST    ! Snow/aggregate m.r. source at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT) :: ZRGT    ! Graupel m.r. source at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT) :: ZRHT    ! Hail m.r. source at t
REAL, DIMENSION(D%NIT,D%NJT,D%NKT) :: ZCITOUT ! Output value for CIT

!Diagnostics
REAL, DIMENSION(SIZE(PTHT,1),SIZE(PTHT,2)) :: ZINPRI ! Pristine ice instant precip
!
LOGICAL :: GEXT_TEND
LOGICAL :: LSOFT ! Must we really compute tendencies or only adjust them to new T variables
INTEGER :: INB_ITER_MAX ! Maximum number of iterations (with real tendencies computation)
REAL :: ZW0D
REAL :: ZTSTEP ! length of sub-timestep in case of time splitting
REAL :: ZINV_TSTEP ! Inverse ov PTSTEP
REAL :: ZTIME_THRESHOLD ! Time to reach threshold
!For total tendencies computation
REAL, DIMENSION(SIZE(PTHT,1),SIZE(PTHT,2),SIZE(PTHT,3),0:7) :: ZWR
!
!Output packed total mixing ratio change (for budgets only)
REAL, DIMENSION(KSIZE) :: ZTOT_RVHENI, & ! heterogeneous nucleation mixing ratio change
                        & ZTOT_RCHONI, & ! Homogeneous nucleation
                        & ZTOT_RRHONG, & ! Spontaneous freezing mixing ratio change
                        & ZTOT_RVDEPS, & ! Deposition on r_s,
                        & ZTOT_RIAGGS, & ! Aggregation on r_s
                        & ZTOT_RIAUTS, & ! Autoconversion of r_i for r_s production
                        & ZTOT_RVDEPG, & ! Deposition on r_g
                        & ZTOT_RCAUTR,  & ! Autoconversion of r_c for r_r production
                        & ZTOT_RCACCR, & ! Accretion of r_c for r_r production
                        & ZTOT_RREVAV, & ! Evaporation of r_r
                        & ZTOT_RCRIMSS, ZTOT_RCRIMSG, ZTOT_RSRIMCG, & ! Cloud droplet riming of the aggregates
                        & ZTOT_RIMLTC, & ! Cloud ice melting mixing ratio change
                        & ZTOT_RCBERI, & ! Bergeron-Findeisen effect
                        & ZTOT_RHMLTR, & ! Melting of the hailstones
                        & ZTOT_RSMLTG, & ! Conversion-Melting of the aggregates
                        & ZTOT_RCMLTSR, & ! Cloud droplet collection onto aggregates by positive temperature
                        & ZTOT_RRACCSS, ZTOT_RRACCSG, ZTOT_RSACCRG, & ! Rain accretion onto the aggregates
                        & ZTOT_RICFRRG, ZTOT_RRCFRIG, ZTOT_RICFRR, & ! Rain contact freezing
                        & ZTOT_RCWETG, ZTOT_RIWETG, ZTOT_RRWETG, ZTOT_RSWETG, &  ! Graupel wet growth
                        & ZTOT_RCDRYG, ZTOT_RIDRYG, ZTOT_RRDRYG, ZTOT_RSDRYG, &  ! Graupel dry growth
                        & ZTOT_RWETGH, & ! Conversion of graupel into hail
                        & ZTOT_RGMLTR, & ! Melting of the graupel
                        & ZTOT_RCWETH, ZTOT_RIWETH, ZTOT_RSWETH, ZTOT_RGWETH, ZTOT_RRWETH, & ! Dry growth of hailstone
                        & ZTOT_RCDRYH, ZTOT_RIDRYH, ZTOT_RSDRYH, ZTOT_RRDRYH, ZTOT_RGDRYH, & ! Wet growth of hailstone
                        & ZTOT_RDRYHG    ! Conversion of hailstone into graupel
!
!For packing
INTEGER :: IMICRO ! Case r_x>0 locations
INTEGER :: JL, JV
REAL, DIMENSION(KPROMA) :: ZTIME ! Current integration time (starts with 0 and ends with PTSTEP)
REAL, DIMENSION(KPROMA) :: &
                        & ZMAXTIME, & ! Time on which we can apply the current tendencies
                        & ZTIME_LASTCALL, &     ! Integration time when last tendecies call has been done
                        & ZCOMPUTE, & ! 1. for points where we must compute tendencies, 0. elsewhere
                        & ZSSI,     &
                        & ZCIT,     & ! Pristine ice conc. at t
                        & ZRHODREF, & ! RHO Dry REFerence
                        & ZZT,      & ! Temperature
                        & ZPRES,    & ! Pressure
                        & ZEXN,     & ! EXNer Pressure
                        & ZLSFACT,  & ! L_s/(Pi*C_ph)
                        & ZLVFACT,  & ! L_v/(Pi*C_ph)
                        & ZSIGMA_RC,& ! Standard deviation of rc at time t
                        & ZCF,      & ! Cloud fraction
                        & ZHLC_HCF, & ! HLCLOUDS : fraction of High Cloud Fraction in grid
                        & ZHLC_LCF, & ! HLCLOUDS : fraction of Low  Cloud Fraction in grid
                                      !    note that ZCF = ZHLC_HCF + ZHLC_LCF
                        & ZHLC_HRC, & ! HLCLOUDS : LWC that is High LWC in grid
                        & ZHLC_LRC, & ! HLCLOUDS : LWC that is Low  LWC in grid
                                      !    note that ZRC = ZHLC_HRC + ZHLC_LRC
                        & ZHLI_HCF, &
                        & ZHLI_LCF, &
                        & ZHLI_HRI, &
                        & ZHLI_LRI
!
!Output packed tendencies (for budgets only)
REAL, DIMENSION(KPROMA) :: ZRVHENI_MR, & ! heterogeneous nucleation mixing ratio change
                        & ZRCHONI, & ! Homogeneous nucleation
                        & ZRRHONG_MR, & ! Spontaneous freezing mixing ratio change
                        & ZRVDEPS, & ! Deposition on r_s,
                        & ZRIAGGS, & ! Aggregation on r_s
                        & ZRIAUTS, & ! Autoconversion of r_i for r_s production
                        & ZRVDEPG, & ! Deposition on r_g
                        & ZRCAUTR,  & ! Autoconversion of r_c for r_r production
                        & ZRCACCR, & ! Accretion of r_c for r_r production
                        & ZRREVAV, & ! Evaporation of r_r
                        & ZRIMLTC_MR, & ! Cloud ice melting mixing ratio change
                        & ZRCBERI, & ! Bergeron-Findeisen effect
                        & ZRHMLTR, & ! Melting of the hailstones
                        & ZRSMLTG, & ! Conversion-Melting of the aggregates
                        & ZRCMLTSR, & ! Cloud droplet collection onto aggregates by positive temperature
                        & ZRRACCSS, ZRRACCSG, ZRSACCRG, & ! Rain accretion onto the aggregates
                        & ZRCRIMSS, ZRCRIMSG, ZRSRIMCG, ZRSRIMCG_MR, & ! Cloud droplet riming of the aggregates
                        & ZRICFRRG, ZRRCFRIG, ZRICFRR, & ! Rain contact freezing
                        & ZRCWETG, ZRIWETG, ZRRWETG, ZRSWETG, &  ! Graupel wet growth
                        & ZRCDRYG, ZRIDRYG, ZRRDRYG, ZRSDRYG, &  ! Graupel dry growth
                        & ZRWETGH, & ! Conversion of graupel into hail
                        & ZRWETGH_MR, & ! Conversion of graupel into hail, mr change
                        & ZRGMLTR, & ! Melting of the graupel
                        & ZRCWETH, ZRIWETH, ZRSWETH, ZRGWETH, ZRRWETH, & ! Dry growth of hailstone
                        & ZRCDRYH, ZRIDRYH, ZRSDRYH, ZRRDRYH, ZRGDRYH, & ! Wet growth of hailstone
                        & ZRDRYHG    ! Conversion of hailstone into graupel
!
!For mixing-ratio-splitting
LOGICAL :: LLCPZ0RT
REAL :: ZTIME_THRESHOLD1D(KPROMA) ! Time to reach threshold
REAL, DIMENSION(KPROMA, KRR) :: Z0RT ! Mixing-ratios at the beginig of the current loop
!
REAL, DIMENSION(KPROMA,0:7) :: &
                        & ZVART, & !Packed variables
                        & ZEXTPK, & !To take into acount external tendencies inside the splitting
                        & ZA, ZB
!
REAL, DIMENSION(KPROMA, 8) :: ZRS_TEND, ZRG_TEND
REAL, DIMENSION(KPROMA,10) :: ZRH_TEND

INTEGER, DIMENSION(KPROMA) :: &
                       & I1,I2,I3, & ! Used to replace the COUNT and PACK intrinsics on variables
                       & IITER       ! Number of iterations done (with real tendencies computation)
INTEGER, DIMENSION(KSIZE) :: I1TOT, I2TOT, I3TOT ! Used to replace the COUNT and PACK intrinsics
!
REAL, DIMENSION(KPROMA) :: ZSUM2, ZMAXB
REAL :: ZDEVIDE, ZX, ZRICE
!
INTEGER :: IC, JMICRO
LOGICAL :: LLSIGMA_RC, LL_ANY_ITER, LL_AUCV_ADJU

!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('RAIN_ICE', 0, ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
IF(OCND2) THEN
  CALL PRINT_MSG(NVERB_FATAL, 'GEN', 'RAIN_ICE', 'OCND2 OPTION NOT CODED IN THIS RAIN_ICE VERSION')
END IF
IF(KPROMA /= KSIZE) THEN
  CALL PRINT_MSG(NVERB_FATAL, 'GEN', 'RAIN_ICE', 'For now, KPROMA must be equal to KSIZE, see code for explanation')
  ! Microphyscs was optimized by introducing chunks of KPROMA size
  ! Thus, in ice4_tendencies, the 1D array represent only a fraction of the points where microphisical species are present
  ! We cannot rebuild the entire 3D arrays in the subroutine, so we cannot call ice4_rainfr_vert in it
  ! A solution would be to suppress optimisation in this case by setting KPROMA=KSIZE in rain_ice
  ! Another solution would be to compute column by column?
  ! Another one would be to cut tendencies in 3 parts: before rainfr_vert, rainfr_vert, after rainfr_vert
ENDIF
!
!*       1.     COMPUTE THE LOOP BOUNDS
!               -----------------------
!
ZINV_TSTEP=1./PTSTEP
GEXT_TEND=.TRUE.
!
! LSFACT and LVFACT without exner
DO JK = D%NKTB,D%NKTE
  DO JJ = D%NJB,D%NJE
    DO JI = D%NIB,D%NIE
      IF (KRR==7) THEN
        ZRICE=PRIT(JI,JJ,JK)+PRST(JI,JJ,JK)+PRGT(JI,JJ,JK)+PRHT(JI,JJ,JK)
      ELSE
        ZRICE=PRIT(JI,JJ,JK)+PRST(JI,JJ,JK)+PRGT(JI,JJ,JK)
      ENDIF
      ZDEVIDE = CST%XCPD + CST%XCPV*PRVT(JI,JJ,JK) + CST%XCL*(PRCT(JI,JJ,JK)+PRRT(JI,JJ,JK)) + CST%XCI*ZRICE
      ZT(JI,JJ,JK) = PTHT(JI,JJ,JK) * PEXN(JI,JJ,JK)
      ZZ_LSFACT(JI,JJ,JK)=(CST%XLSTT+(CST%XCPV-CST%XCI)*(ZT(JI,JJ,JK)-CST%XTT)) / ZDEVIDE
      ZZ_LVFACT(JI,JJ,JK)=(CST%XLVTT+(CST%XCPV-CST%XCL)*(ZT(JI,JJ,JK)-CST%XTT)) / ZDEVIDE
    ENDDO
  ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
!*       2.     COMPUTE THE SEDIMENTATION (RS) SOURCE
!               -------------------------------------
!
IF(.NOT. PARAMI%LSEDIM_AFTER) THEN
  !
  !*       2.1     sedimentation
  !
  IF (BUCONF%LBUDGET_RC .AND. OSEDIC) CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RC), 'SEDI', PRCS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RR)              CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RR), 'SEDI', PRRS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RI)              CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RI), 'SEDI', PRIS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RS)              CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RS), 'SEDI', PRSS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RG)              CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RG), 'SEDI', PRGS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RH .AND. KRR==7) CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RH), 'SEDI', PRHS(:, :, :) * PRHODJ(:, :, :))

  IF(HSEDIM=='STAT') THEN
    IF(KRR==7) THEN
      DO JK = D%NKTB,D%NKTE
        DO JJ = D%NJB,D%NJE
          DO JI = D%NIB,D%NIE
            ZRCT(JI,JJ,JK)=PRCS(JI,JJ,JK)*PTSTEP
            ZRRT(JI,JJ,JK)=PRRS(JI,JJ,JK)*PTSTEP
            ZRIT(JI,JJ,JK)=PRIS(JI,JJ,JK)*PTSTEP
            ZRST(JI,JJ,JK)=PRSS(JI,JJ,JK)*PTSTEP
            ZRGT(JI,JJ,JK)=PRGS(JI,JJ,JK)*PTSTEP
            ZRHT(JI,JJ,JK)=PRHS(JI,JJ,JK)*PTSTEP
          ENDDO
        ENDDO
      ENDDO
      CALL ICE4_SEDIMENTATION_STAT(D, CST, ICEP, ICED, &
                                  &PTSTEP, KRR, OSEDIC, PDZZ, &
                                  &PRHODREF, PPABST, PTHT, PRHODJ, &
                                  &PRCS, ZRCT, PRRS, ZRRT, PRIS, ZRIT,&
                                  &PRSS, ZRST, PRGS, ZRGT,&
                                  &PINPRC, PINPRR, ZINPRI, PINPRS, PINPRG, &
                                  &PSEA=PSEA, PTOWN=PTOWN, &
                                  &PINPRH=PINPRH, PRHT=ZRHT, PRHS=PRHS, PFPR=PFPR)
    ELSE
      DO JK = D%NKTB,D%NKTE
        DO JJ = D%NJB,D%NJE
          DO JI = D%NIB,D%NIE
            ZRCT(JI,JJ,JK)=PRCS(JI,JJ,JK)*PTSTEP
            ZRRT(JI,JJ,JK)=PRRS(JI,JJ,JK)*PTSTEP
            ZRIT(JI,JJ,JK)=PRIS(JI,JJ,JK)*PTSTEP
            ZRST(JI,JJ,JK)=PRSS(JI,JJ,JK)*PTSTEP
            ZRGT(JI,JJ,JK)=PRGS(JI,JJ,JK)*PTSTEP
          ENDDO
        ENDDO
      ENDDO
      CALL ICE4_SEDIMENTATION_STAT(D, CST, ICEP, ICED, &
                                  &PTSTEP, KRR, OSEDIC, PDZZ, &
                                  &PRHODREF, PPABST, PTHT, PRHODJ, &
                                  &PRCS, ZRCT, PRRS, ZRRT, PRIS, ZRIT,&
                                  &PRSS, ZRST, PRGS, ZRGT,&
                                  &PINPRC, PINPRR, ZINPRI, PINPRS, PINPRG, &
                                  &PSEA=PSEA, PTOWN=PTOWN, &
                                  &PFPR=PFPR)
    ENDIF
    PINPRS(D%NIB:D%NIE, D%NJB:D%NJE) = PINPRS(D%NIB:D%NIE, D%NJB:D%NJE) + ZINPRI(D%NIB:D%NIE, D%NJB:D%NJE)
    !No negativity correction here as we apply sedimentation on PR.S*PTSTEP variables
  ELSEIF(HSEDIM=='SPLI') THEN
    IF(KRR==7) THEN
      CALL ICE4_SEDIMENTATION_SPLIT(D, CST, ICEP, ICED, PARAMI, &
                                   &PTSTEP, KRR, OSEDIC, PDZZ, &
                                   &PRHODREF, PPABST, PTHT, PRHODJ, &
                                   &PRCS, PRCT, PRRS, PRRT, PRIS, PRIT, PRSS, PRST, PRGS, PRGT,&
                                   &PINPRC, PINPRR, ZINPRI, PINPRS, PINPRG, &
                                   &PSEA=PSEA, PTOWN=PTOWN, &
                                   &PINPRH=PINPRH, PRHT=PRHT, PRHS=PRHS, PFPR=PFPR)
    ELSE
      CALL ICE4_SEDIMENTATION_SPLIT(D, CST, ICEP, ICED, PARAMI, &
                                   &PTSTEP, KRR, OSEDIC, PDZZ, &
                                   &PRHODREF, PPABST, PTHT, PRHODJ, &
                                   &PRCS, PRCT, PRRS, PRRT, PRIS, PRIT, PRSS, PRST, PRGS, PRGT,&
                                   &PINPRC, PINPRR, ZINPRI, PINPRS, PINPRG, &
                                   &PSEA=PSEA, PTOWN=PTOWN, &
                                   &PFPR=PFPR)
    ENDIF
    PINPRS(D%NIB:D%NIE, D%NJB:D%NJE) = PINPRS(D%NIB:D%NIE, D%NJB:D%NJE) + ZINPRI(D%NIB:D%NIE, D%NJB:D%NJE)
    !We correct negativities with conservation
    !SPLI algorith uses a time-splitting. Inside the loop a temporary m.r. is used.
    !   It is initialized with the m.r. at T and is modified by two tendencies:
    !   sedimentation tendency and an external tendency which represents all other
    !   processes (mainly advection and microphysical processes). If both tendencies
    !   are negative, sedimentation can remove a species at a given sub-timestep. From
    !   this point sedimentation stops for the remaining sub-timesteps but the other tendency
    !   will be still active and will lead to negative values.
    !   We could prevent the algorithm to not consume too much a species, instead we apply
    !   a correction here.
    CALL CORRECT_NEGATIVITIES(D, KRR, PRVS, PRCS, PRRS, &
                             &PRIS, PRSS, PRGS, &
                             &PTHS, ZZ_LVFACT, ZZ_LSFACT, PRHS)
  ELSEIF(HSEDIM=='NONE') THEN
  ELSE
    CALL PRINT_MSG(NVERB_FATAL, 'GEN', 'RAIN_ICE', 'no sedimentation scheme for HSEDIM='//HSEDIM)
  END IF






!!!!! ajouter momentum














  !
  !*       2.2     budget storage
  !
  IF (BUCONF%LBUDGET_RC .AND. OSEDIC) CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RC), 'SEDI', PRCS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RR)              CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RR), 'SEDI', PRRS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RI)              CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RI), 'SEDI', PRIS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RS)              CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RS), 'SEDI', PRSS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RG)              CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RG), 'SEDI', PRGS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RH .AND. KRR==7) CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RH), 'SEDI', PRHS(:, :, :) * PRHODJ(:, :, :))
ENDIF
!

DO JK = D%NKTB,D%NKTE
  !Backup of T variables
  ZWR(:,:,JK,IRV)=PRVT(:,:,JK)
  ZWR(:,:,JK,IRC)=PRCT(:,:,JK)
  ZWR(:,:,JK,IRR)=PRRT(:,:,JK)
  ZWR(:,:,JK,IRI)=PRIT(:,:,JK)
  ZWR(:,:,JK,IRS)=PRST(:,:,JK)
  ZWR(:,:,JK,IRG)=PRGT(:,:,JK)
  IF (KRR==7) THEN
    ZWR(:,:,JK,IRH)=PRHT(:,:,JK)
  ELSE
    ZWR(:,:,JK,IRH)=0.
  ENDIF

  !Preset for output 3D variables
  IF(OWARM) THEN
    PEVAP3D(:,:,JK)=0.
  ENDIF
  PRAINFR(:,:,JK)=0.
  ZCITOUT(:,:,JK)=0.
ENDDO

IF(BUCONF%LBU_ENABLE) THEN
  ZTOT_RVHENI(:)=0.
  ZTOT_RCHONI(:)=0.
  ZTOT_RRHONG(:)=0.
  ZTOT_RVDEPS(:)=0.
  ZTOT_RIAGGS(:)=0.
  ZTOT_RIAUTS(:)=0.
  ZTOT_RVDEPG(:)=0.
  ZTOT_RCAUTR(:)=0.
  ZTOT_RCACCR(:)=0.
  ZTOT_RREVAV(:)=0.
  ZTOT_RCRIMSS(:)=0.
  ZTOT_RCRIMSG(:)=0.
  ZTOT_RSRIMCG(:)=0.
  ZTOT_RIMLTC(:)=0.
  ZTOT_RCBERI(:)=0.
  ZTOT_RHMLTR(:)=0.
  ZTOT_RSMLTG(:)=0.
  ZTOT_RCMLTSR(:)=0.
  ZTOT_RRACCSS(:)=0.
  ZTOT_RRACCSG(:)=0.
  ZTOT_RSACCRG(:)=0.
  ZTOT_RICFRRG(:)=0.
  ZTOT_RRCFRIG(:)=0.
  ZTOT_RICFRR(:)=0.
  ZTOT_RCWETG(:)=0.
  ZTOT_RIWETG(:)=0.
  ZTOT_RRWETG(:)=0.
  ZTOT_RSWETG(:)=0.
  ZTOT_RCDRYG(:)=0.
  ZTOT_RIDRYG(:)=0.
  ZTOT_RRDRYG(:)=0.
  ZTOT_RSDRYG(:)=0.
  ZTOT_RWETGH(:)=0.
  ZTOT_RGMLTR(:)=0.
  ZTOT_RCWETH(:)=0.
  ZTOT_RIWETH(:)=0.
  ZTOT_RSWETH(:)=0.
  ZTOT_RGWETH(:)=0.
  ZTOT_RRWETH(:)=0.
  ZTOT_RCDRYH(:)=0.
  ZTOT_RIDRYH(:)=0.
  ZTOT_RSDRYH(:)=0.
  ZTOT_RRDRYH(:)=0.
  ZTOT_RGDRYH(:)=0.
  ZTOT_RDRYHG(:)=0.
ENDIF

!-------------------------------------------------------------------------------
!  optimization by looking for locations where
!  the microphysical fields are larger than a minimal value only !!!
!
IF (KSIZE /= COUNT(ODMICRO)) THEN
    CALL PRINT_MSG(NVERB_FATAL, 'GEN', 'RAIN_ICE', 'RAIN_ICE : KSIZE /= COUNT(ODMICRO)')
ENDIF

IF (KSIZE > 0) THEN

  !Maximum number of iterations
  !We only count real iterations (those for which we *compute* tendencies)
  INB_ITER_MAX=PARAMI%NMAXITER
  IF(PARAMI%XTSTEP_TS/=0.)THEN
    INB_ITER_MAX=MAX(1, INT(PTSTEP/PARAMI%XTSTEP_TS)) !At least the number of iterations needed for the time-splitting
    ZTSTEP=PTSTEP/INB_ITER_MAX
    INB_ITER_MAX=MAX(PARAMI%NMAXITER, INB_ITER_MAX) !For the case XMRSTEP/=0. at the same time
  ENDIF

!===============================================================================================================
! Cache-blocking loop :

  LLSIGMA_RC=(HSUBG_AUCV_RC=='PDF ' .AND. PARAMI%CSUBG_PR_PDF=='SIGM')
  LL_AUCV_ADJU=(HSUBG_AUCV_RC=='ADJU' .OR. HSUBG_AUCV_RI=='ADJU')

  ! starting indexes :
  IC=0
  ISTK=D%NKTB
  ISTJ=D%NJB
  ISTI=D%NIB

  DO JMICRO=1,KSIZE,KPROMA

    IMICRO=MIN(KPROMA,KSIZE-JMICRO+1)
!
!*       3.     PACKING
!               --------

    ! Setup packing parameters
    OUTER_LOOP: DO JK = ISTK, D%NKTE
      DO JJ = ISTJ, D%NJE
        IF (ANY(ODMICRO(:,JJ,JK))) THEN
          DO JI = ISTI, D%NIE
            IF (ODMICRO(JI,JJ,JK)) THEN
              IC=IC+1
              ! Initialization of variables in packed format :
              ZVART(IC, ITH)=PTHT(JI, JJ, JK)
              ZVART(IC, IRV)=PRVT(JI, JJ, JK)
              ZVART(IC, IRC)=PRCT(JI, JJ, JK)
              ZVART(IC, IRR)=PRRT(JI, JJ, JK)
              ZVART(IC, IRI)=PRIT(JI, JJ, JK)
              ZVART(IC, IRS)=PRST(JI, JJ, JK)
              ZVART(IC, IRG)=PRGT(JI, JJ, JK)
              IF (KRR==7) THEN
                ZVART(IC, IRH)=PRHT(JI, JJ, JK)
              ENDIF
              IF (GEXT_TEND) THEN
                !The th tendency is not related to a mixing ratio change, there is no exn/exnref issue here
                ZEXTPK(IC, ITH)=PTHS(JI, JJ, JK)
                ZEXTPK(IC, IRV)=PRVS(JI, JJ, JK)
                ZEXTPK(IC, IRC)=PRCS(JI, JJ, JK)
                ZEXTPK(IC, IRR)=PRRS(JI, JJ, JK)
                ZEXTPK(IC, IRI)=PRIS(JI, JJ, JK)
                ZEXTPK(IC, IRS)=PRSS(JI, JJ, JK)
                ZEXTPK(IC, IRG)=PRGS(JI, JJ, JK)
                IF (KRR==7) THEN
                  ZEXTPK(IC, IRH)=PRHS(JI, JJ, JK)
                ENDIF
              ENDIF
              ZCIT       (IC)=PCIT    (JI, JJ, JK)
              ZCF        (IC)=PCLDFR  (JI, JJ, JK)
              ZRHODREF   (IC)=PRHODREF(JI, JJ, JK)
              ZPRES      (IC)=PPABST  (JI, JJ, JK)
              ZEXN       (IC)=PEXN    (JI, JJ, JK)
              IF(LLSIGMA_RC) THEN
                ZSIGMA_RC(IC)=PSIGS   (JI, JJ, JK)
              ENDIF
              IF (LL_AUCV_ADJU) THEN
                ZHLC_HCF(IC) = PHLC_HCF(JI, JJ, JK)
                ZHLC_HRC(IC) = PHLC_HRC(JI, JJ, JK)
                ZHLI_HCF(IC) = PHLI_HCF(JI, JJ, JK)
                ZHLI_HRI(IC) = PHLI_HRI(JI, JJ, JK)
              ENDIF
              ! Save indices for later usages:
              I1(IC) = JI
              I2(IC) = JJ
              I3(IC) = JK
              I1TOT(JMICRO+IC-1)=JI
              I2TOT(JMICRO+IC-1)=JJ
              I3TOT(JMICRO+IC-1)=JK
              IF (IC==IMICRO) THEN
                ! the end of the chunk has been reached, then reset the starting index :
                ISTI=JI+1
                IF (ISTI <= D%NIE) THEN
                  ISTJ=JJ
                  ISTK=JK
                ELSE
                  ! end of line, restart from 1 and increment upper loop
                  ISTI=D%NIB
                  ISTJ=JJ+1
                  IF (ISTJ <= D%NJE) THEN
                    ISTK=JK
                  ELSE
                    ! end of line, restart from 1 and increment upper loop
                    ISTJ=D%NJB
                    ISTK=JK+1
                    IF (ISTK > D%NKTE) THEN
                      ! end of line, restart from 1
                      ISTK=D%NKTB
                    ENDIF
                  ENDIF
                ENDIF
                IC=0
                EXIT OUTER_LOOP
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        ! restart inner loop on JI :
        ISTI=D%NIB
      ENDDO
      ! restart inner loop on JJ :
      ISTJ=D%NJB
    ENDDO OUTER_LOOP

    IF (GEXT_TEND) THEN
      DO JV=0, KRR
        DO JL=1, IMICRO
          ZEXTPK(JL, JV)=ZEXTPK(JL, JV)-ZVART(JL, JV)*ZINV_TSTEP
        ENDDO
      ENDDO
    ENDIF
    IF (LLSIGMA_RC) THEN
      DO JL=1, IMICRO
        ZSIGMA_RC(JL)=ZSIGMA_RC(JL)*2.
      ENDDO 
    ENDIF
    IF (LL_AUCV_ADJU) THEN
      DO JL=1, IMICRO
        ZHLC_LRC(JL) = ZVART(JL, IRC) - ZHLC_HRC(JL)
        ZHLI_LRI(JL) = ZVART(JL, IRI) - ZHLI_HRI(JL)
        IF(ZVART(JL, IRC)>0.) THEN
          ZHLC_LCF(JL) = ZCF(JL)- ZHLC_HCF(JL)
        ELSE
          ZHLC_LCF(JL)=0.
        ENDIF
        IF(ZVART(JL, IRI)>0.) THEN
          ZHLI_LCF(JL) = ZCF(JL)- ZHLI_HCF(JL)
        ELSE
          ZHLI_LCF(JL)=0.
        ENDIF
      ENDDO
    ENDIF

!-------------------------------------------------------------------------------
!
!*       4.     LOOP
!               ----
!
    IITER(1:IMICRO)=0
    ZTIME(1:IMICRO)=0. ! Current integration time (all points may have a different integration time)

    DO WHILE(ANY(ZTIME(1:IMICRO)<PTSTEP)) ! Loop to *really* compute tendencies

      IF(PARAMI%XTSTEP_TS/=0.) THEN
        ! In this case we need to remember the time when tendencies were computed
        ! because when time has evolved more than a limit, we must re-compute tendencies
        ZTIME_LASTCALL(1:IMICRO)=ZTIME(1:IMICRO)
      ENDIF
      DO JL=1, IMICRO
        IF (ZTIME(JL) < PTSTEP) THEN
          ZCOMPUTE(JL)=1. ! Computation (1.) only for points for which integration time has not reached the timestep
          IITER(JL)=IITER(JL)+1
        ELSE
          ZCOMPUTE(JL)=0.
        ENDIF
      ENDDO
      LL_ANY_ITER=ANY(IITER(1:IMICRO) < INB_ITER_MAX)
      LLCPZ0RT=.TRUE.
      LSOFT=.FALSE. ! We *really* compute the tendencies

      DO WHILE(ANY(ZCOMPUTE(1:IMICRO)==1.)) ! Loop to adjust tendencies when we cross the 0°C or when a species disappears
!$OMP SIMD
        DO JL=1, IMICRO
          ZSUM2(JL)=SUM(ZVART(JL,IRI:KRR))
        ENDDO
        DO JL=1, IMICRO
          ZDEVIDE=(CST%XCPD + CST%XCPV*ZVART(JL, IRV) + CST%XCL*(ZVART(JL, IRC)+ZVART(JL, IRR)) + CST%XCI*ZSUM2(JL)) * ZEXN(JL)
          ZZT(JL) = ZVART(JL, ITH) * ZEXN(JL)
          ZLSFACT(JL)=(CST%XLSTT+(CST%XCPV-CST%XCI)*(ZZT(JL)-CST%XTT)) / ZDEVIDE
          ZLVFACT(JL)=(CST%XLVTT+(CST%XCPV-CST%XCL)*(ZZT(JL)-CST%XTT)) / ZDEVIDE
        ENDDO
        !
        !***       4.1 Tendencies computation
        !
        ! Tendencies are *really* computed when LSOFT==.FALSE. and only adjusted otherwise
    CALL ICE4_TENDENCIES(D, CST, PARAMI, ICEP, ICED, &
                        &KPROMA, IMICRO, &
                        &KRR, LSOFT, ZCOMPUTE, &
                        &OWARM, PARAMI%CSUBG_RC_RR_ACCR, PARAMI%CSUBG_RR_EVAP, &
                        &HSUBG_AUCV_RC, HSUBG_AUCV_RI, PARAMI%CSUBG_PR_PDF, &
                        &ZEXN, ZRHODREF, ZLVFACT, ZLSFACT, I1, I2, I3, &
                        &ZPRES, ZCF, ZSIGMA_RC, &
                        &ZCIT, &
                        &ZZT, ZVART, &
                        &ZRVHENI_MR, ZRRHONG_MR, ZRIMLTC_MR, ZRSRIMCG_MR, &
                        &ZRCHONI, ZRVDEPS, ZRIAGGS, ZRIAUTS, ZRVDEPG, &
                        &ZRCAUTR, ZRCACCR, ZRREVAV, &
                        &ZRCRIMSS, ZRCRIMSG, ZRSRIMCG, ZRRACCSS, ZRRACCSG, ZRSACCRG, ZRSMLTG, ZRCMLTSR, &
                        &ZRICFRRG, ZRRCFRIG, ZRICFRR, ZRCWETG, ZRIWETG, ZRRWETG, ZRSWETG, &
                        &ZRCDRYG, ZRIDRYG, ZRRDRYG, ZRSDRYG, ZRWETGH, ZRWETGH_MR, ZRGMLTR, &
                        &ZRCWETH, ZRIWETH, ZRSWETH, ZRGWETH, ZRRWETH, &
                        &ZRCDRYH, ZRIDRYH, ZRSDRYH, ZRRDRYH, ZRGDRYH, ZRDRYHG, ZRHMLTR, &
                        &ZRCBERI, &
                        &ZRS_TEND, ZRG_TEND, ZRH_TEND, ZSSI, &
                        &ZA, ZB, &
                        &ZHLC_HCF, ZHLC_LCF, ZHLC_HRC, ZHLC_LRC, &
                        &ZHLI_HCF, ZHLI_LCF, ZHLI_HRI, ZHLI_LRI, PRAINFR)

        ! External tendencies
        IF(GEXT_TEND) THEN
          DO JV=0, KRR
            DO JL=1, IMICRO
              ZA(JL, JV) = ZA(JL, JV) + ZEXTPK(JL, JV)
            ENDDO
          ENDDO
        ENDIF
        !
        !***       4.2 Integration time
        !
        ! If we can, we shall use these tendencies until the end of the timestep
        DO JL=1, IMICRO
          ZMAXTIME(JL)=ZCOMPUTE(JL) * (PTSTEP-ZTIME(JL)) ! Remaining time until the end of the timestep
        ENDDO

        !We need to adjust tendencies when temperature reaches 0
        IF(PARAMI%LFEEDBACKT) THEN
          DO JL=1, IMICRO
            !Is ZB(:, ITH) enough to change temperature sign?
            ZX=CST%XTT/ZEXN(JL)
            IF ((ZVART(JL, ITH) - ZX) * (ZVART(JL, ITH) + ZB(JL, ITH) - ZX) < 0.) THEN
              ZMAXTIME(JL)=0.
            ENDIF
            !Can ZA(:, ITH) make temperature change of sign?
            IF (ABS(ZA(JL,ITH)) > 1.E-20 ) THEN
              ZTIME_THRESHOLD=(ZX - ZB(JL, ITH) - ZVART(JL, ITH))/ZA(JL, ITH)
              IF (ZTIME_THRESHOLD > 0.) THEN
                ZMAXTIME(JL)=MIN(ZMAXTIME(JL), ZTIME_THRESHOLD)
              ENDIF
            ENDIF
          ENDDO
        ENDIF

        !We need to adjust tendencies when a species disappears
        !When a species is missing, only the external tendencies can be negative (and we must keep track of it)
        DO JV=1, KRR
          DO JL=1, IMICRO
            IF (ZA(JL, JV) < -1.E-20 .AND. ZVART(JL, JV) > ICED%XRTMIN(JV)) THEN
              ZMAXTIME(JL)=MIN(ZMAXTIME(JL), -(ZB(JL, JV)+ZVART(JL, JV))/ZA(JL, JV))
            ENDIF
          ENDDO
        ENDDO

        !We stop when the end of the timestep is reached
        DO JL=1, IMICRO
          IF (ZTIME(JL)+ZMAXTIME(JL) >= PTSTEP) THEN
            ZCOMPUTE(JL)=0.
          ENDIF
        ENDDO
        !We must recompute tendencies when the end of the sub-timestep is reached
        IF (PARAMI%XTSTEP_TS/=0.) THEN
          DO JL=1, IMICRO
            IF ((IITER(JL) < INB_ITER_MAX) .AND. (ZTIME(JL)+ZMAXTIME(JL) > ZTIME_LASTCALL(JL)+ZTSTEP)) THEN
              ZMAXTIME(JL)=ZTIME_LASTCALL(JL)-ZTIME(JL)+ZTSTEP
              ZCOMPUTE(JL)=0.
            ENDIF
          ENDDO
        ENDIF

        !We must recompute tendencies when the maximum allowed change is reached
        !When a species is missing, only the external tendencies can be active and we do not want to recompute
        !the microphysical tendencies when external tendencies are negative (results won't change because species was already missing)
        IF (PARAMI%XMRSTEP/=0.) THEN
          IF (LL_ANY_ITER) THEN
            ! In this case we need to remember the initial mixing ratios used to compute the tendencies
            ! because when mixing ratio has evolved more than a threshold, we must re-compute tendencies
            ! Thus, at first iteration (ie when LLCPZ0RT=.TRUE.) we copy ZVART into Z0RT
            DO JV=1,KRR
              IF (LLCPZ0RT) Z0RT(1:IMICRO, JV)=ZVART(1:IMICRO, JV)
              DO JL=1, IMICRO
                IF (IITER(JL)<INB_ITER_MAX .AND. ABS(ZA(JL,JV))>1.E-20) THEN
                  ZTIME_THRESHOLD1D(JL)=(SIGN(1., ZA(JL, JV))*PARAMI%XMRSTEP+Z0RT(JL, JV)-ZVART(JL, JV)-ZB(JL, JV))/ZA(JL, JV)
                ELSE
                  ZTIME_THRESHOLD1D(JL)=-1.
                ENDIF
              ENDDO
              DO JL=1, IMICRO
                IF (ZTIME_THRESHOLD1D(JL)>=0 .AND. ZTIME_THRESHOLD1D(JL)<ZMAXTIME(JL) .AND. (ZVART(JL, JV)>ICED%XRTMIN(JV) .OR. ZA(JL, JV)>0.)) THEN
                  ZMAXTIME(JL)=MIN(ZMAXTIME(JL), ZTIME_THRESHOLD1D(JL))
                  ZCOMPUTE(JL)=0.
                ENDIF
              ENDDO
            ENDDO
            LLCPZ0RT=.FALSE.
!$OMP SIMD
            DO JL=1,IMICRO
              ZMAXB(JL)=MAXVAL(ABS(ZB(JL,1:KRR)))
            ENDDO
            DO JL=1, IMICRO
              IF (IITER(JL)<INB_ITER_MAX .AND. ZMAXB(JL)>PARAMI%XMRSTEP) THEN
                ZMAXTIME(JL)=0.
                ZCOMPUTE(JL)=0.
              ENDIF
            ENDDO
          ENDIF ! LL_ANY_ITER
        ENDIF ! XMRSTEP/=0.
        !
        !***       4.3 New values of variables for next iteration
        !
        DO JV=0, KRR
          DO JL=1, IMICRO
            ZVART(JL, JV)=ZVART(JL, JV)+ZA(JL, JV)*ZMAXTIME(JL)+ZB(JL, JV)
          ENDDO
        ENDDO
        DO JL=1, IMICRO
          IF (ZVART(JL,IRI)==0.) ZCIT(JL) = 0.
          ZTIME(JL)=ZTIME(JL)+ZMAXTIME(JL)
        ENDDO

        !
        !***       4.4 Mixing ratio change due to each process
        !
        IF(BUCONF%LBU_ENABLE) THEN
          DO JL=1, IMICRO
            ZTOT_RVHENI (JMICRO+JL-1)=ZTOT_RVHENI (JMICRO+JL-1)+ZRVHENI_MR(JL)
            ZTOT_RCHONI (JMICRO+JL-1)=ZTOT_RCHONI (JMICRO+JL-1)+ZRCHONI   (JL)*ZMAXTIME(JL)
            ZTOT_RRHONG (JMICRO+JL-1)=ZTOT_RRHONG (JMICRO+JL-1)+ZRRHONG_MR(JL)
            ZTOT_RVDEPS (JMICRO+JL-1)=ZTOT_RVDEPS (JMICRO+JL-1)+ZRVDEPS   (JL)*ZMAXTIME(JL)
            ZTOT_RIAGGS (JMICRO+JL-1)=ZTOT_RIAGGS (JMICRO+JL-1)+ZRIAGGS   (JL)*ZMAXTIME(JL)
            ZTOT_RIAUTS (JMICRO+JL-1)=ZTOT_RIAUTS (JMICRO+JL-1)+ZRIAUTS   (JL)*ZMAXTIME(JL)
            ZTOT_RVDEPG (JMICRO+JL-1)=ZTOT_RVDEPG (JMICRO+JL-1)+ZRVDEPG   (JL)*ZMAXTIME(JL)
            ZTOT_RCAUTR (JMICRO+JL-1)=ZTOT_RCAUTR (JMICRO+JL-1)+ZRCAUTR   (JL)*ZMAXTIME(JL)
            ZTOT_RCACCR (JMICRO+JL-1)=ZTOT_RCACCR (JMICRO+JL-1)+ZRCACCR   (JL)*ZMAXTIME(JL)
            ZTOT_RREVAV (JMICRO+JL-1)=ZTOT_RREVAV (JMICRO+JL-1)+ZRREVAV   (JL)*ZMAXTIME(JL)
            ZTOT_RCRIMSS(JMICRO+JL-1)=ZTOT_RCRIMSS(JMICRO+JL-1)+ZRCRIMSS  (JL)*ZMAXTIME(JL)
            ZTOT_RCRIMSG(JMICRO+JL-1)=ZTOT_RCRIMSG(JMICRO+JL-1)+ZRCRIMSG  (JL)*ZMAXTIME(JL)
            ZTOT_RSRIMCG(JMICRO+JL-1)=ZTOT_RSRIMCG(JMICRO+JL-1)+ZRSRIMCG  (JL)*ZMAXTIME(JL)+ZRSRIMCG_MR(JL)
            ZTOT_RRACCSS(JMICRO+JL-1)=ZTOT_RRACCSS(JMICRO+JL-1)+ZRRACCSS  (JL)*ZMAXTIME(JL)
            ZTOT_RRACCSG(JMICRO+JL-1)=ZTOT_RRACCSG(JMICRO+JL-1)+ZRRACCSG  (JL)*ZMAXTIME(JL)
            ZTOT_RSACCRG(JMICRO+JL-1)=ZTOT_RSACCRG(JMICRO+JL-1)+ZRSACCRG  (JL)*ZMAXTIME(JL)
            ZTOT_RSMLTG (JMICRO+JL-1)=ZTOT_RSMLTG (JMICRO+JL-1)+ZRSMLTG   (JL)*ZMAXTIME(JL)
            ZTOT_RCMLTSR(JMICRO+JL-1)=ZTOT_RCMLTSR(JMICRO+JL-1)+ZRCMLTSR  (JL)*ZMAXTIME(JL)
            ZTOT_RICFRRG(JMICRO+JL-1)=ZTOT_RICFRRG(JMICRO+JL-1)+ZRICFRRG  (JL)*ZMAXTIME(JL)
            ZTOT_RRCFRIG(JMICRO+JL-1)=ZTOT_RRCFRIG(JMICRO+JL-1)+ZRRCFRIG  (JL)*ZMAXTIME(JL)
            ZTOT_RICFRR (JMICRO+JL-1)=ZTOT_RICFRR (JMICRO+JL-1)+ZRICFRR   (JL)*ZMAXTIME(JL)
            ZTOT_RCWETG (JMICRO+JL-1)=ZTOT_RCWETG (JMICRO+JL-1)+ZRCWETG   (JL)*ZMAXTIME(JL)
            ZTOT_RIWETG (JMICRO+JL-1)=ZTOT_RIWETG (JMICRO+JL-1)+ZRIWETG   (JL)*ZMAXTIME(JL)
            ZTOT_RRWETG (JMICRO+JL-1)=ZTOT_RRWETG (JMICRO+JL-1)+ZRRWETG   (JL)*ZMAXTIME(JL)
            ZTOT_RSWETG (JMICRO+JL-1)=ZTOT_RSWETG (JMICRO+JL-1)+ZRSWETG   (JL)*ZMAXTIME(JL)
            ZTOT_RWETGH (JMICRO+JL-1)=ZTOT_RWETGH (JMICRO+JL-1)+ZRWETGH   (JL)*ZMAXTIME(JL)+ZRWETGH_MR(JL)
            ZTOT_RCDRYG (JMICRO+JL-1)=ZTOT_RCDRYG (JMICRO+JL-1)+ZRCDRYG   (JL)*ZMAXTIME(JL)
            ZTOT_RIDRYG (JMICRO+JL-1)=ZTOT_RIDRYG (JMICRO+JL-1)+ZRIDRYG   (JL)*ZMAXTIME(JL)
            ZTOT_RRDRYG (JMICRO+JL-1)=ZTOT_RRDRYG (JMICRO+JL-1)+ZRRDRYG   (JL)*ZMAXTIME(JL)
            ZTOT_RSDRYG (JMICRO+JL-1)=ZTOT_RSDRYG (JMICRO+JL-1)+ZRSDRYG   (JL)*ZMAXTIME(JL)
            ZTOT_RGMLTR (JMICRO+JL-1)=ZTOT_RGMLTR (JMICRO+JL-1)+ZRGMLTR   (JL)*ZMAXTIME(JL)
            ZTOT_RCWETH (JMICRO+JL-1)=ZTOT_RCWETH (JMICRO+JL-1)+ZRCWETH   (JL)*ZMAXTIME(JL)
            ZTOT_RIWETH (JMICRO+JL-1)=ZTOT_RIWETH (JMICRO+JL-1)+ZRIWETH   (JL)*ZMAXTIME(JL)
            ZTOT_RSWETH (JMICRO+JL-1)=ZTOT_RSWETH (JMICRO+JL-1)+ZRSWETH   (JL)*ZMAXTIME(JL)
            ZTOT_RGWETH (JMICRO+JL-1)=ZTOT_RGWETH (JMICRO+JL-1)+ZRGWETH   (JL)*ZMAXTIME(JL)
            ZTOT_RRWETH (JMICRO+JL-1)=ZTOT_RRWETH (JMICRO+JL-1)+ZRRWETH   (JL)*ZMAXTIME(JL)
            ZTOT_RCDRYH (JMICRO+JL-1)=ZTOT_RCDRYH (JMICRO+JL-1)+ZRCDRYH   (JL)*ZMAXTIME(JL)
            ZTOT_RIDRYH (JMICRO+JL-1)=ZTOT_RIDRYH (JMICRO+JL-1)+ZRIDRYH   (JL)*ZMAXTIME(JL)
            ZTOT_RSDRYH (JMICRO+JL-1)=ZTOT_RSDRYH (JMICRO+JL-1)+ZRSDRYH   (JL)*ZMAXTIME(JL)
            ZTOT_RRDRYH (JMICRO+JL-1)=ZTOT_RRDRYH (JMICRO+JL-1)+ZRRDRYH   (JL)*ZMAXTIME(JL)
            ZTOT_RGDRYH (JMICRO+JL-1)=ZTOT_RGDRYH (JMICRO+JL-1)+ZRGDRYH   (JL)*ZMAXTIME(JL)
            ZTOT_RDRYHG (JMICRO+JL-1)=ZTOT_RDRYHG (JMICRO+JL-1)+ZRDRYHG   (JL)*ZMAXTIME(JL)
            ZTOT_RHMLTR (JMICRO+JL-1)=ZTOT_RHMLTR (JMICRO+JL-1)+ZRHMLTR   (JL)*ZMAXTIME(JL)
            ZTOT_RIMLTC (JMICRO+JL-1)=ZTOT_RIMLTC (JMICRO+JL-1)+ZRIMLTC_MR(JL)
            ZTOT_RCBERI (JMICRO+JL-1)=ZTOT_RCBERI (JMICRO+JL-1)+ZRCBERI   (JL)*ZMAXTIME(JL)
          ENDDO
        ENDIF
        !
        !***       4.5 Next loop
        !
        LSOFT=.TRUE. ! We try to adjust tendencies (inner while loop)
      ENDDO
    ENDDO

    IF(GEXT_TEND) THEN
      !Z..T variables contain the external tendency, we substract it
      DO JV=0, KRR
        DO JL=1, IMICRO
          ZVART(JL, JV) = ZVART(JL, JV) - ZEXTPK(JL, JV) * PTSTEP
        ENDDO
      ENDDO
    ENDIF

!-------------------------------------------------------------------------------
!
!*       5.     UNPACKING DIAGNOSTICS
!               ---------------------
!
    DO JL=1, IMICRO
      ZCITOUT  (I1(JL),I2(JL),I3(JL))=ZCIT   (JL)
      IF(OWARM) THEN
        PEVAP3D(I1(JL),I2(JL),I3(JL))=ZRREVAV(JL)
      ENDIF
      ZWR(I1(JL),I2(JL),I3(JL),IRV)=ZVART(JL, IRV)
      ZWR(I1(JL),I2(JL),I3(JL),IRC)=ZVART(JL, IRC)
      ZWR(I1(JL),I2(JL),I3(JL),IRR)=ZVART(JL, IRR)
      ZWR(I1(JL),I2(JL),I3(JL),IRI)=ZVART(JL, IRI)
      ZWR(I1(JL),I2(JL),I3(JL),IRS)=ZVART(JL, IRS)
      ZWR(I1(JL),I2(JL),I3(JL),IRG)=ZVART(JL, IRG)
      IF (KRR==7) THEN
        ZWR(I1(JL),I2(JL),I3(JL),IRH)=ZVART(JL, IRH)
      ENDIF
    ENDDO

  ENDDO ! JMICRO
ENDIF ! KSIZE > 0
PCIT(:,:,:)=ZCITOUT(:,:,:)

!==========================================================================================================


!
!*       6.     COMPUTES THE SLOW COLD PROCESS SOURCES OUTSIDE OF ODMICRO POINTS
!               ----------------------------------------------------------------
!
DO JK=D%NKTB,D%NKTE
  DO JJ=D%NJB,D%NJE
!DIR$ VECTOR ALWAYS
    DO CONCURRENT (JI=D%NIB:D%NIE)
      IF (.NOT. ODMICRO(JI, JJ, JK)) THEN
        ZW0D=ZZ_LSFACT(JI, JJ, JK)/PEXN(JI, JJ, JK)
      ENDIF
      CALL ICE4_NUCLEATION_ELEM(.NOT. ODMICRO(JI, JJ, JK), &
                                PTHT(JI, JJ, JK), PPABST(JI, JJ, JK), PRHODREF(JI, JJ, JK), &
                                PEXN(JI, JJ, JK), ZW0D, ZT(JI, JJ, JK), &
                                PRVT(JI, JJ, JK), &
                                PCIT(JI, JJ, JK), ZZ_RVHENI_MR(JI, JJ, JK))
    ENDDO
  ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
!*       7.     TOTAL TENDENCIES
!               ----------------
!
!
!***     7.1    total tendencies limited by available species
!
DO JK = D%NKTB, D%NKTE
  DO JJ = D%NJB, D%NJE
    DO CONCURRENT (JI=D%NIB:D%NIE)
      !LV/LS
      ZZ_LSFACT(JI,JJ,JK)=ZZ_LSFACT(JI,JJ,JK)/PEXNREF(JI,JJ,JK)
      ZZ_LVFACT(JI,JJ,JK)=ZZ_LVFACT(JI,JJ,JK)/PEXNREF(JI,JJ,JK)

      !Tendency dure to nucleation on non ODMICRO points
      ZZ_RVHENI(JI,JJ,JK) = MIN(PRVS(JI,JJ,JK), ZZ_RVHENI_MR(JI,JJ,JK)/PTSTEP)

      !Hydrometeor tendencies is the difference between old state and new state (can be negative)
      ZWR(JI,JJ,JK,IRV)=(ZWR(JI,JJ,JK,IRV)-PRVT(JI,JJ,JK))*ZINV_TSTEP
      ZWR(JI,JJ,JK,IRC)=(ZWR(JI,JJ,JK,IRC)-PRCT(JI,JJ,JK))*ZINV_TSTEP
      ZWR(JI,JJ,JK,IRR)=(ZWR(JI,JJ,JK,IRR)-PRRT(JI,JJ,JK))*ZINV_TSTEP
      ZWR(JI,JJ,JK,IRI)=(ZWR(JI,JJ,JK,IRI)-PRIT(JI,JJ,JK))*ZINV_TSTEP
      ZWR(JI,JJ,JK,IRS)=(ZWR(JI,JJ,JK,IRS)-PRST(JI,JJ,JK))*ZINV_TSTEP
      ZWR(JI,JJ,JK,IRG)=(ZWR(JI,JJ,JK,IRG)-PRGT(JI,JJ,JK))*ZINV_TSTEP
      IF(KRR==7) THEN
        ZWR(JI,JJ,JK,IRH)=(ZWR(JI,JJ,JK,IRH)-PRHT(JI,JJ,JK))*ZINV_TSTEP
      ENDIF

      !Theta tendency computed from hydrometeors tendencies
      ZWR(JI,JJ,JK, ITH) = (ZWR(JI,JJ,JK,IRC)+ZWR(JI,JJ,JK,IRR))*ZZ_LVFACT(JI,JJ,JK)+ &
                         & (ZWR(JI,JJ,JK,IRI)+ZWR(JI,JJ,JK,IRS)+ZWR(JI,JJ,JK,IRG)+ &
                         &  ZWR(JI,JJ,JK,IRH))*ZZ_LSFACT(JI,JJ,JK)

      !We apply these tendencies to the S variables
      !including the nucleation part
      PTHS(JI,JJ,JK) = PTHS(JI,JJ,JK) + ZWR(JI,JJ,JK,ITH)+ZZ_RVHENI(JI,JJ,JK)*ZZ_LSFACT(JI,JJ,JK)
      PRVS(JI,JJ,JK) = PRVS(JI,JJ,JK) + ZWR(JI,JJ,JK,IRV)-ZZ_RVHENI(JI,JJ,JK)
      PRCS(JI,JJ,JK) = PRCS(JI,JJ,JK) + ZWR(JI,JJ,JK,IRC)
      PRRS(JI,JJ,JK) = PRRS(JI,JJ,JK) + ZWR(JI,JJ,JK,IRR)
      PRIS(JI,JJ,JK) = PRIS(JI,JJ,JK) + ZWR(JI,JJ,JK,IRI)+ZZ_RVHENI(JI,JJ,JK)
      PRSS(JI,JJ,JK) = PRSS(JI,JJ,JK) + ZWR(JI,JJ,JK,IRS)
      PRGS(JI,JJ,JK) = PRGS(JI,JJ,JK) + ZWR(JI,JJ,JK,IRG)
      IF (KRR==7) THEN
        PRHS(JI,JJ,JK) = PRHS(JI,JJ,JK) + ZWR(JI,JJ,JK,IRH)
      ENDIF
    ENDDO
  ENDDO
ENDDO

!
!***     7.2    LBU_ENABLE case
!
IF(BUCONF%LBU_ENABLE) THEN
  IF (BUCONF%LBUDGET_TH) THEN
    ZZ_DIFF(:,:,:)=0.
    DO JK = D%NKTB, D%NKTE
      DO JJ = D%NJB, D%NJE
        DO JI = D%NIB, D%NIE
          ZZ_DIFF(JI, JJ, JK) = ZZ_LSFACT(JI, JJ, JK) - ZZ_LVFACT(JI, JJ, JK)
        ENDDO
      ENDDO
    ENDDO
  END IF

  ZW(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RVHENI(JL) * ZINV_TSTEP
  END DO
  DO JK = D%NKTB, D%NKTE
    DO JJ = D%NJB, D%NJE
      DO JI = D%NIB, D%NIE
        ZW(JI,JJ,JK)=ZW(JI,JJ,JK)+ZZ_RVHENI(JI,JJ,JK)
      ENDDO
    ENDDO
  ENDDO
  IF (BUCONF%LBUDGET_TH) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'HENU',  ZW(:, :, :)*ZZ_LSFACT(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RV) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RV), 'HENU', -ZW(:, :, :)                   *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RI) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RI), 'HENU',  ZW(:, :, :)                   *PRHODJ(:, :, :))

  ZW(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RCHONI(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_TH) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'HON',  ZW(:, :, :)*ZZ_DIFF(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RC) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RC), 'HON', -ZW(:, :, :)                 *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RI) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RI), 'HON',  ZW(:, :, :)                 *PRHODJ(:, :, :))

  ZW(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RRHONG(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_TH) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'SFR',  ZW(:, :, :)*ZZ_DIFF(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RR) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RR), 'SFR', -ZW(:, :, :)                 *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RG) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RG), 'SFR',  ZW(:, :, :)                 *PRHODJ(:, :, :))

  ZW(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RVDEPS(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_TH) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'DEPS',  ZW(:, :, :)*ZZ_LSFACT(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RV) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RV), 'DEPS', -ZW(:, :, :)                   *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RS) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RS), 'DEPS',  ZW(:, :, :)                   *PRHODJ(:, :, :))

  ZW(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RIAGGS(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_RI) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RI), 'AGGS', -ZW(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RS) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RS), 'AGGS',  ZW(:, :, :)*PRHODJ(:, :, :))

  ZW(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RIAUTS(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_RI) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RI), 'AUTS', -ZW(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RS) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RS), 'AUTS',  ZW(:, :, :)*PRHODJ(:, :, :))

  ZW(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RVDEPG(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_TH) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'DEPG',  ZW(:, :, :)*ZZ_LSFACT(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RV) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RV), 'DEPG', -ZW(:, :, :)                   *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RG) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RG), 'DEPG',  ZW(:, :, :)                   *PRHODJ(:, :, :))

  IF(OWARM) THEN
    ZW(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RCAUTR(JL) * ZINV_TSTEP
    END DO
    IF (BUCONF%LBUDGET_RC) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RC), 'AUTO', -ZW(:, :, :)*PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RR) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RR), 'AUTO',  ZW(:, :, :)*PRHODJ(:, :, :))

    ZW(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RCACCR(JL) * ZINV_TSTEP
    END DO
    IF (BUCONF%LBUDGET_RC) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RC), 'ACCR', -ZW(:, :, :)*PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RR) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RR), 'ACCR',  ZW(:, :, :)*PRHODJ(:, :, :))

    ZW(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RREVAV(JL) * ZINV_TSTEP
    END DO
    IF (BUCONF%LBUDGET_TH) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'REVA', -ZW(:, :, :)*ZZ_LVFACT(:, :, :)*PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RV) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RV), 'REVA',  ZW(:, :, :)                   *PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RR) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RR), 'REVA', -ZW(:, :, :)                   *PRHODJ(:, :, :))
  ENDIF

  ZW1(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW1(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RCRIMSS(JL) * ZINV_TSTEP
  END DO
  ZW2(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW2(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RCRIMSG(JL) * ZINV_TSTEP
  END DO
  ZW3(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW3(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RSRIMCG(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_TH) &
    CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'RIM', (ZW1(:, :, :)+ZW2(:, :, :))*ZZ_DIFF(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RC) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RC), 'RIM', (-ZW1(:, :, :)-ZW2(:, :, :))*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RS) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RS), 'RIM', ( ZW1(:, :, :)-ZW3(:, :, :))*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RG) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RG), 'RIM', ( ZW2(:, :, :)+ZW3(:, :, :))*PRHODJ(:, :, :))

  ZW1(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW1(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RRACCSS(JL) * ZINV_TSTEP
  END DO
  ZW2(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW2(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RRACCSG(JL) * ZINV_TSTEP
  END DO
  ZW3(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW3(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RSACCRG(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_TH) &
    CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'ACC', (ZW1(:, :, :)+ZW2(:, :, :) )*ZZ_DIFF(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RR) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RR), 'ACC', (-ZW1(:, :, :)-ZW2(:, :, :))*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RS) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RS), 'ACC', ( ZW1(:, :, :)-ZW3(:, :, :))*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RG) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RG), 'ACC', ( ZW2(:, :, :)+ZW3(:, :, :))*PRHODJ(:, :, :))

  ZW(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RSMLTG(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_RS) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RS), 'CMEL', -ZW(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RG) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RG), 'CMEL',  ZW(:, :, :)*PRHODJ(:, :, :))
  ZW(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RCMLTSR(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_RC) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RC), 'CMEL', -ZW(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RR) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RR), 'CMEL',  ZW(:, :, :)*PRHODJ(:, :, :))

  ZW1(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW1(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RICFRRG(JL) * ZINV_TSTEP
  END DO
  ZW2(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW2(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RRCFRIG(JL) * ZINV_TSTEP
  END DO
  ZW3(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW3(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RICFRR(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_TH) &
    CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'CFRZ', ZW2(:, :, :)*ZZ_DIFF(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RR) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RR), 'CFRZ', (-ZW2(:, :, :)+ZW3(:, :, :))*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RI) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RI), 'CFRZ', (-ZW1(:, :, :)-ZW3(:, :, :))*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RG) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RG), 'CFRZ', ( ZW1(:, :, :)+ZW2(:, :, :))*PRHODJ(:, :, :))

  ZW1(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW1(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RCWETG(JL) * ZINV_TSTEP
  END DO
  ZW2(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW2(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RRWETG(JL) * ZINV_TSTEP
  END DO
  ZW3(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW3(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RIWETG(JL) * ZINV_TSTEP
  END DO
  ZW4(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW4(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RSWETG(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_TH) &
    CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'WETG', (ZW1(:, :, :)+ZW2(:, :, :))*ZZ_DIFF(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RC) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RC), 'WETG', -zw1(:, :, :)                 *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RR) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RR), 'WETG', -zw2(:, :, :)                 *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RI) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RI), 'WETG', -zw3(:, :, :)                 *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RS) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RS), 'WETG', -zw4(:, :, :)                 *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RG) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RG), 'WETG', (ZW1(:, :, :)+ZW2(:, :, :)+ZW3(:, :, :)+ZW4(:, :, :)) &
                                                                      &                             *PRHODJ(:, :, :))

  IF(KRR==7) THEN
    ZW(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RWETGH(JL) * ZINV_TSTEP
    END DO
    IF (BUCONF%LBUDGET_RG) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RG), 'GHCV', -ZW(:, :, :)*PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RH) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RH), 'GHCV',  ZW(:, :, :)*PRHODJ(:, :, :))
  END IF

  ZW1(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW1(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RCDRYG(JL) * ZINV_TSTEP
  END DO
  ZW2(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW2(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RRDRYG(JL) * ZINV_TSTEP
  END DO
  ZW3(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW3(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RIDRYG(JL) * ZINV_TSTEP
  END DO
  ZW4(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW4(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RSDRYG(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_TH) &
    CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'DRYG', (ZW1(:, :, :)+ZW2(:, :, :) )*ZZ_DIFF(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RC) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RC), 'DRYG', -zw1(:, :, :)                  *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RR) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RR), 'DRYG', -zw2(:, :, :)                  *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RI) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RI), 'DRYG', -zw3(:, :, :)                  *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RS) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RS), 'DRYG', -zw4(:, :, :)                  *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RG) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RG), 'DRYG', (ZW1(:, :, :)+ZW2(:, :, :)+ZW3(:, :, :)+ZW4(:, :, :)) &
                                                                      &                              *PRHODJ(:, :, :))

  ZW(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RGMLTR(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_TH) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'GMLT', -ZW(:, :, :)*ZZ_DIFF(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RR) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RR), 'GMLT',  ZW(:, :, :)                 *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RG) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RG), 'GMLT', -ZW(:, :, :)                 *PRHODJ(:, :, :))

  IF(KRR==7) THEN
    ZW1(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW1(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RCWETH(JL) * ZINV_TSTEP
    END DO
    ZW2(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW2(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RRWETH(JL) * ZINV_TSTEP
    END DO
    ZW3(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW3(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RIWETH(JL) * ZINV_TSTEP
    END DO
    ZW4(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW4(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RSWETH(JL) * ZINV_TSTEP
    END DO
    ZW5(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW5(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RGWETH(JL) * ZINV_TSTEP
    END DO
    IF (BUCONF%LBUDGET_TH) &
      CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'WETH', (ZW1(:, :, :)+ZW2(:, :, :))*ZZ_DIFF(:, :, :)*PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RC) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RC), 'WETH', -ZW1(:, :, :)                 *PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RR) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RR), 'WETH', -ZW2(:, :, :)                 *PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RI) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RI), 'WETH', -ZW3(:, :, :)                 *PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RS) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RS), 'WETH', -ZW4(:, :, :)                 *PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RG) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RG), 'WETH', -ZW5(:, :, :)                 *PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RH) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RH), 'WETH', (ZW1(:, :, :)+ZW2(:, :, :)+ZW3(:, :, :)+ &
                                                                        &ZW4(:, :, :)+ZW5(:, :, : ))  *PRHODJ(:, :, :))

    ZW(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RGWETH(JL) * ZINV_TSTEP
    END DO
    IF (BUCONF%LBUDGET_RG) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RG), 'HGCV', -ZW(:, :, :)*PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RH) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RH), 'HGCV',  ZW(:, :, :)*PRHODJ(:, :, :))

    ZW1(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW1(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RCDRYH(JL) * ZINV_TSTEP
    END DO
    ZW2(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW2(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RRDRYH(JL) * ZINV_TSTEP
    END DO
    ZW3(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW3(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RIDRYH(JL) * ZINV_TSTEP
    END DO
    ZW4(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW4(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RSDRYH(JL) * ZINV_TSTEP
    END DO
    ZW5(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW5(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RGDRYH(JL) * ZINV_TSTEP
    END DO
    ZW6(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW6(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RDRYHG(JL) * ZINV_TSTEP
    END DO
    IF (BUCONF%LBUDGET_TH) &
      CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'DRYH', (ZW1(:, :, :)+ZW2(:, :, :))*ZZ_DIFF(:, :, :)*PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RC) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RC), 'DRYH', -ZW1(:, :, :)                 *PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RR) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RR), 'DRYH', -ZW2(:, :, :)                 *PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RI) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RI), 'DRYH', -ZW3(:, :, :)                 *PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RS) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RS), 'DRYH', -ZW4(:, :, :)                 *PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RG) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RG), 'DRYH', (-ZW5(:, :, :)+ZW6(:, :, : )) *PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RH) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RH), 'DRYH', (ZW1(:, :, :)+ZW2(:, :, :)+ZW3(:, :, :)+   &
                                                                        &ZW4(:, :, :)+ZW5(:, :, : )-ZW6(:, :, :)) &
                                                                        &                             *PRHODJ(:, :, :))

    ZW(:,:,:) = 0.
    DO JL=1, KSIZE
      ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RHMLTR(JL) * ZINV_TSTEP
    END DO
    IF (BUCONF%LBUDGET_TH) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'HMLT', -ZW(:, :, :)*ZZ_DIFF(:, :, :)*PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RR) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RR), 'HMLT',  ZW(:, :, :)                 *PRHODJ(:, :, :))
    IF (BUCONF%LBUDGET_RH) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RH), 'HMLT', -ZW(:, :, :)                 *PRHODJ(:, :, :))
  ENDIF

  ZW(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RIMLTC(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_TH) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'IMLT', -ZW(:, :, :)*ZZ_DIFF(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RC) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RC), 'IMLT',  ZW(:, :, :)                 *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RI) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RI), 'IMLT', -ZW(:, :, :)                 *PRHODJ(:, :, :))

  ZW(:,:,:) = 0.
  DO JL=1, KSIZE
    ZW(I1TOT(JL), I2TOT(JL), I3TOT(JL)) = ZTOT_RCBERI(JL) * ZINV_TSTEP
  END DO
  IF (BUCONF%LBUDGET_TH) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_TH), 'BERFI',  ZW(:, :, :)*ZZ_DIFF(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RC) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RC), 'BERFI', -ZW(:, :, :)                 *PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RI) CALL BUDGET_STORE_ADD(TBUDGETS(NBUDGET_RI), 'BERFI',  ZW(:, :, :)                 *PRHODJ(:, :, :))

ENDIF
!
!***     7.3    Final tendencies
!
IF (BUCONF%LBU_ENABLE) THEN
  IF (BUCONF%LBUDGET_TH) CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_TH), 'CORR', PTHS(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RV) CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RV), 'CORR', PRVS(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RC) CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RC), 'CORR', PRCS(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RR) CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RR), 'CORR', PRSS(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RI) CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RI), 'CORR', PRIS(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RS) CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RS), 'CORR', PRSS(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RG) CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RG), 'CORR', PRGS(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RH .AND. KRR==7) CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RH), 'CORR', PRHS(:, :, :)*PRHODJ(:, :, :))
END IF

!NOTE:
!  This call cannot be moved before the preeceding budget calls because,
!  with AROME, the BUDGET_STORE_INIT does nothing. The equivalent is done only
!  once before the physics call and copies of the S variables evolve automatically
!  internally to the budget (DDH) machinery at each BUDGET_STORE_ADD and
!  BUDGET_STORE_END calls. Thus, the difference between the DDH internal version
!  of the S variables and the S variables used in the folowing BUDGET_STORE_END
!  call must only be due to the correction of negativities.
!
!We correct negativities with conservation
CALL CORRECT_NEGATIVITIES(D, KRR, PRVS, PRCS, PRRS, &
                         &PRIS, PRSS, PRGS, &
                         &PTHS, ZZ_LVFACT, ZZ_LSFACT, PRHS)

IF (BUCONF%LBU_ENABLE) THEN
  IF (BUCONF%LBUDGET_TH) CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_TH), 'CORR', PTHS(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RV) CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RV), 'CORR', PRVS(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RC) CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RC), 'CORR', PRCS(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RR) CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RR), 'CORR', PRRS(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RI) CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RI), 'CORR', PRIS(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RS) CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RS), 'CORR', PRSS(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RG) CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RG), 'CORR', PRGS(:, :, :)*PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RH .AND. KRR==7) CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RH), 'CORR', PRHS(:, :, :)*PRHODJ(:, :, :))
END IF
!
!-------------------------------------------------------------------------------
!
!*       8.     COMPUTE THE SEDIMENTATION (RS) SOURCE
!               -------------------------------------
!
IF(PARAMI%LSEDIM_AFTER) THEN
  !
  !*       8.1     sedimentation
  !
  IF (BUCONF%LBUDGET_RC .AND. OSEDIC) CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RC), 'SEDI', PRCS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RR)              CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RR), 'SEDI', PRRS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RI)              CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RI), 'SEDI', PRIS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RS)              CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RS), 'SEDI', PRSS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RG)              CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RG), 'SEDI', PRGS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RH .AND. KRR==7) CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RH), 'SEDI', PRHS(:, :, :) * PRHODJ(:, :, :))

  IF(HSEDIM=='STAT') THEN
    IF (KRR==7) THEN
      DO JK = D%NKTB,D%NKTE
        DO JJ = D%NJB,D%NJE
          DO JI = D%NIB,D%NIE
            ZRCT(JI,JJ,JK)=PRCS(JI,JJ,JK)*PTSTEP
            ZRRT(JI,JJ,JK)=PRRS(JI,JJ,JK)*PTSTEP
            ZRIT(JI,JJ,JK)=PRIS(JI,JJ,JK)*PTSTEP
            ZRST(JI,JJ,JK)=PRSS(JI,JJ,JK)*PTSTEP
            ZRGT(JI,JJ,JK)=PRGS(JI,JJ,JK)*PTSTEP
            ZRHT(JI,JJ,JK)=PRHS(JI,JJ,JK)*PTSTEP
          ENDDO
        ENDDO
      ENDDO
      CALL ICE4_SEDIMENTATION_STAT(D, CST, ICEP, ICED, &
                                  &PTSTEP, KRR, OSEDIC, PDZZ, &
                                  &PRHODREF, PPABST, PTHT, PRHODJ, &
                                  &PRCS, ZRCT, PRRS, ZRRT, PRIS, ZRIT,&
                                  &PRSS, ZRST, PRGS, ZRGT,&
                                  &PINPRC, PINPRR, ZINPRI, PINPRS, PINPRG, &
                                  &PSEA=PSEA, PTOWN=PTOWN, &
                                  &PINPRH=PINPRH, PRHT=ZRHT, PRHS=PRHS, PFPR=PFPR)
    ELSE
      DO JK = D%NKTB,D%NKTE
        DO JJ = D%NJB,D%NJE
          DO JI = D%NIB,D%NIE
            ZRCT(JI,JJ,JK)=PRCS(JI,JJ,JK)*PTSTEP
            ZRRT(JI,JJ,JK)=PRRS(JI,JJ,JK)*PTSTEP
            ZRIT(JI,JJ,JK)=PRIS(JI,JJ,JK)*PTSTEP
            ZRST(JI,JJ,JK)=PRSS(JI,JJ,JK)*PTSTEP
            ZRGT(JI,JJ,JK)=PRGS(JI,JJ,JK)*PTSTEP
          ENDDO
        ENDDO
      ENDDO
      CALL ICE4_SEDIMENTATION_STAT(D, CST, ICEP, ICED, &
                                  &PTSTEP, KRR, OSEDIC, PDZZ, &
                                  &PRHODREF, PPABST, PTHT, PRHODJ, &
                                  &PRCS, ZRCT, PRRS, ZRRT, PRIS, ZRIT,&
                                  &PRSS, ZRST, PRGS, ZRGT,&
                                  &PINPRC, PINPRR, ZINPRI, PINPRS, PINPRG, &
                                  &PSEA=PSEA, PTOWN=PTOWN, &
                                  &PFPR=PFPR)
    ENDIF
    PINPRS(D%NIB:D%NIE, D%NJB:D%NJE) = PINPRS(D%NIB:D%NIE, D%NJB:D%NJE) + ZINPRI(D%NIB:D%NIE, D%NJB:D%NJE)
    !No negativity correction here as we apply sedimentation on PR.S*PTSTEP variables
  ELSEIF(HSEDIM=='SPLI') THEN
    !SR: It *seems* that we must have two separate calls for ifort
    IF(KRR==7) THEN
      CALL ICE4_SEDIMENTATION_SPLIT(D, CST, ICEP, ICED, PARAMI, &
                                   &PTSTEP, KRR, OSEDIC, PDZZ, &
                                   &PRHODREF, PPABST, PTHT, PRHODJ, &
                                   &PRCS, PRCT, PRRS, PRRT, PRIS, PRIT, PRSS, PRST, PRGS, PRGT,&
                                   &PINPRC, PINPRR, ZINPRI, PINPRS, PINPRG, &
                                   &PSEA=PSEA, PTOWN=PTOWN, &
                                   &PINPRH=PINPRH, PRHT=PRHT, PRHS=PRHS, PFPR=PFPR)
    ELSE
      CALL ICE4_SEDIMENTATION_SPLIT(D, CST, ICEP, ICED, PARAMI, &
                                   &PTSTEP, KRR, OSEDIC, PDZZ, &
                                   &PRHODREF, PPABST, PTHT, PRHODJ, &
                                   &PRCS, PRCT, PRRS, PRRT, PRIS, PRIT, PRSS, PRST, PRGS, PRGT,&
                                   &PINPRC, PINPRR, ZINPRI, PINPRS, PINPRG, &
                                   &PSEA=PSEA, PTOWN=PTOWN, &
                                   &PFPR=PFPR)
    ENDIF
    PINPRS(D%NIB:D%NIE, D%NJB:D%NJE) = PINPRS(D%NIB:D%NIE, D%NJB:D%NJE) + ZINPRI(D%NIB:D%NIE, D%NJB:D%NJE)
    !We correct negativities with conservation
    !SPLI algorith uses a time-splitting. Inside the loop a temporary m.r. is used.
    !   It is initialized with the m.r. at T and is modified by two tendencies:
    !   sedimentation tendency and an external tendency which represents all other
    !   processes (mainly advection and microphysical processes). If both tendencies
    !   are negative, sedimentation can remove a species at a given sub-timestep. From
    !   this point sedimentation stops for the remaining sub-timesteps but the other tendency
    !   will be still active and will lead to negative values.
    !   We could prevent the algorithm to not consume too much a species, instead we apply
    !   a correction here.
    CALL CORRECT_NEGATIVITIES(D, KRR, PRVS, PRCS, PRRS, &
                             &PRIS, PRSS, PRGS, &
                             &PTHS, ZZ_LVFACT, ZZ_LSFACT, PRHS)
  ELSE
    CALL PRINT_MSG(NVERB_FATAL, 'GEN', 'RAIN_ICE', 'no sedimentation scheme for HSEDIM='//HSEDIM)
  END IF
  !
  !*       8.2     budget storage
  !
  IF (BUCONF%LBUDGET_RC .AND. OSEDIC)   CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RC), 'SEDI', PRCS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RR)                CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RR), 'SEDI', PRRS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RI)                CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RI), 'SEDI', PRIS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RS)                CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RS), 'SEDI', PRSS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RG)                CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RG), 'SEDI', PRGS(:, :, :) * PRHODJ(:, :, :))
  IF (BUCONF%LBUDGET_RH .AND. KRR==7)   CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RH), 'SEDI', PRHS(:, :, :) * PRHODJ(:, :, :))

  !"sedimentation" of rain fraction
  IF (PRESENT(PRHS)) THEN
    CALL ICE4_RAINFR_VERT(D, ICED, PRAINFR, PRRS(:,:,:)*PTSTEP, &
                       &PRSS(:,:,:)*PTSTEP, PRGS(:,:,:)*PTSTEP, PRHS(:,:,:)*PTSTEP)
  ELSE
    CALL ICE4_RAINFR_VERT(D, ICED, PRAINFR, PRRS(:,:,:)*PTSTEP, &
                       &PRSS(:,:,:)*PTSTEP, PRGS(:,:,:)*PTSTEP)
  ENDIF
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       9.     COMPUTE THE FOG DEPOSITION TERM 
!               -------------------------------------
!
IF (PARAMI%LDEPOSC) THEN !cloud water deposition on vegetation
  IF (BUCONF%LBU_ENABLE .AND. BUCONF%LBUDGET_RC) &
     & CALL BUDGET_STORE_INIT(TBUDGETS(NBUDGET_RC), 'DEPO', PRCS(:, :, :)*PRHODJ(:, :, :))

  PINDEP(:,:)=0.
  DO JJ = D%NJB, D%NJE
!DEC$ IVDEP
    DO JI = D%NIB, D%NIE
      PINDEP(JI, JJ) = PARAMI%XVDEPOSC * PRCT(JI, JJ, D%NKB) * PRHODREF(JI, JJ, D%NKB) / CST%XRHOLW
      PRCS(JI, JJ, D%NKB) = PRCS(JI, JJ, D%NKB) - PARAMI%XVDEPOSC * PRCT(JI, JJ, D%NKB) / PDZZ(JI, JJ, D%NKB)
      PINPRC(JI, JJ) = PINPRC(JI, JJ) + PINDEP(JI, JJ)
    ENDDO
  ENDDO

  IF (BUCONF%LBU_ENABLE .AND. BUCONF%LBUDGET_RC) &
     & CALL BUDGET_STORE_END(TBUDGETS(NBUDGET_RC), 'DEPO', PRCS(:, :, :)*PRHODJ(:, :, :))
ENDIF

IF (LHOOK) CALL DR_HOOK('RAIN_ICE', 1, ZHOOK_HANDLE)
!
CONTAINS
  !
  SUBROUTINE CORRECT_NEGATIVITIES(D, KRR, PRV, PRC, PRR, &
                                 &PRI, PRS, PRG, &
                                 &PTH, PLVFACT, PLSFACT, PRH)
  !
  IMPLICIT NONE
  !
  TYPE(DIMPHYEX_t),                     INTENT(IN)    :: D
  INTEGER,                              INTENT(IN)    :: KRR
  REAL, DIMENSION(D%NIT, D%NJT, D%NKT), INTENT(INOUT) :: PRV, PRC, PRR, PRI, PRS, PRG, PTH
  REAL, DIMENSION(D%NIT, D%NJT, D%NKT), INTENT(IN)    :: PLVFACT, PLSFACT
  REAL, DIMENSION(D%NIT, D%NJT, D%NKT), OPTIONAL, INTENT(INOUT) :: PRH
  !
  REAL :: ZW
  INTEGER :: JI, JJ, JK
  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  !
  IF (LHOOK) CALL DR_HOOK('RAIN_ICE:CORRECT_NEGATIVITIES', 0, ZHOOK_HANDLE)
  !
  !We correct negativities with conservation
  DO JK = D%NKTB, D%NKTE
    DO JJ = D%NJB, D%NJE
      DO JI = D%NIB, D%NIE
        ! 1) deal with negative values for mixing ratio, except for vapor
        ZW           =PRC(JI,JJ,JK)-MAX(PRC(JI,JJ,JK), 0.)
        PRV(JI,JJ,JK)=PRV(JI,JJ,JK)+ZW
        PTH(JI,JJ,JK)=PTH(JI,JJ,JK)-ZW*PLVFACT(JI,JJ,JK)
        PRC(JI,JJ,JK)=PRC(JI,JJ,JK)-ZW

        ZW           =PRR(JI,JJ,JK)-MAX(PRR(JI,JJ,JK), 0.)
        PRV(JI,JJ,JK)=PRV(JI,JJ,JK)+ZW
        PTH(JI,JJ,JK)=PTH(JI,JJ,JK)-ZW*PLVFACT(JI,JJ,JK)
        PRR(JI,JJ,JK)=PRR(JI,JJ,JK)-ZW

        ZW           =PRI(JI,JJ,JK)-MAX(PRI(JI,JJ,JK), 0.)
        PRV(JI,JJ,JK)=PRV(JI,JJ,JK)+ZW
        PTH(JI,JJ,JK)=PTH(JI,JJ,JK)-ZW*PLSFACT(JI,JJ,JK)
        PRI(JI,JJ,JK)=PRI(JI,JJ,JK)-ZW

        ZW           =PRS(JI,JJ,JK)-MAX(PRS(JI,JJ,JK), 0.)
        PRV(JI,JJ,JK)=PRV(JI,JJ,JK)+ZW
        PTH(JI,JJ,JK)=PTH(JI,JJ,JK)-ZW*PLSFACT(JI,JJ,JK)
        PRS(JI,JJ,JK)=PRS(JI,JJ,JK)-ZW

        ZW           =PRG(JI,JJ,JK)-MAX(PRG(JI,JJ,JK), 0.)
        PRV(JI,JJ,JK)=PRV(JI,JJ,JK)+ZW
        PTH(JI,JJ,JK)=PTH(JI,JJ,JK)-ZW*PLSFACT(JI,JJ,JK)
        PRG(JI,JJ,JK)=PRG(JI,JJ,JK)-ZW

        IF(KRR==7) THEN
          ZW           =PRH(JI,JJ,JK)-MAX(PRH(JI,JJ,JK), 0.)
          PRV(JI,JJ,JK)=PRV(JI,JJ,JK)+ZW
          PTH(JI,JJ,JK)=PTH(JI,JJ,JK)-ZW*PLSFACT(JI,JJ,JK)
          PRH(JI,JJ,JK)=PRH(JI,JJ,JK)-ZW
        ENDIF

        ! 2) deal with negative vapor mixing ratio

        ! for rc and ri, we keep ice fraction constant
        ZW=MIN(1., MAX(ICED%XRTMIN(1)-PRV(JI,JJ,JK), 0.) / &
                  &MAX(PRC(JI,JJ,JK)+PRI(JI,JJ,JK), 1.E-20)) ! Proportion of rc+ri to convert into rv
        PTH(JI,JJ,JK)=PTH(JI,JJ,JK)-ZW* &
                     &(PRC(JI,JJ,JK)*PLVFACT(JI,JJ,JK)+PRI(JI,JJ,JK)*PLSFACT(JI,JJ,JK))
        PRV(JI,JJ,JK)=PRV(JI,JJ,JK)+ZW*(PRC(JI,JJ,JK)+PRI(JI,JJ,JK))
        PRC(JI,JJ,JK)=(1.-ZW)*PRC(JI,JJ,JK)
        PRI(JI,JJ,JK)=(1.-ZW)*PRI(JI,JJ,JK)

        ZW=MIN(MAX(PRR(JI,JJ,JK), 0.), &
              &MAX(ICED%XRTMIN(1)-PRV(JI,JJ,JK), 0.)) ! Quantity of rr to convert into rv
        PRV(JI,JJ,JK)=PRV(JI,JJ,JK)+ZW
        PRR(JI,JJ,JK)=PRR(JI,JJ,JK)-ZW
        PTH(JI,JJ,JK)=PTH(JI,JJ,JK)-ZW*PLVFACT(JI,JJ,JK)

        ZW=MIN(MAX(PRS(JI,JJ,JK), 0.), &
              &MAX(ICED%XRTMIN(1)-PRV(JI,JJ,JK), 0.)) ! Quantity of rs to convert into rv
        PRV(JI,JJ,JK)=PRV(JI,JJ,JK)+ZW
        PRS(JI,JJ,JK)=PRS(JI,JJ,JK)-ZW
        PTH(JI,JJ,JK)=PTH(JI,JJ,JK)-ZW*PLSFACT(JI,JJ,JK)

        ZW=MIN(MAX(PRG(JI,JJ,JK), 0.), &
              &MAX(ICED%XRTMIN(1)-PRV(JI,JJ,JK), 0.)) ! Quantity of rg to convert into rv
        PRV(JI,JJ,JK)=PRV(JI,JJ,JK)+ZW
        PRG(JI,JJ,JK)=PRG(JI,JJ,JK)-ZW
        PTH(JI,JJ,JK)=PTH(JI,JJ,JK)-ZW*PLSFACT(JI,JJ,JK)

        IF(KRR==7) THEN
          ZW=MIN(MAX(PRH(JI,JJ,JK), 0.), &
                &MAX(ICED%XRTMIN(1)-PRV(JI,JJ,JK), 0.)) ! Quantity of rh to convert into rv
          PRV(JI,JJ,JK)=PRV(JI,JJ,JK)+ZW
          PRH(JI,JJ,JK)=PRH(JI,JJ,JK)-ZW
          PTH(JI,JJ,JK)=PTH(JI,JJ,JK)-ZW*PLSFACT(JI,JJ,JK)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  !
  IF (LHOOK) CALL DR_HOOK('RAIN_ICE:CORRECT_NEGATIVITIES', 1, ZHOOK_HANDLE)
  !
  END SUBROUTINE CORRECT_NEGATIVITIES
!
INCLUDE "ice4_nucleation_elem.func.h"
!
END SUBROUTINE RAIN_ICE_RED
