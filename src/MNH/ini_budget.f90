!MNH_LIC Copyright 1995-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 17/08/2020: add Budget_preallocate subroutine
!-----------------------------------------------------------------
module mode_ini_budget

  implicit none

  private

  public :: Budget_preallocate, Ini_budget

contains

subroutine Budget_preallocate()

use modd_budget, only: nbudgets, tbudgets,                                         &
                       NBUDGET_U, NBUDGET_V, NBUDGET_W, NBUDGET_TH, NBUDGET_TKE,   &
                       NBUDGET_RV, NBUDGET_RC, NBUDGET_RR, NBUDGET_RI, NBUDGET_RS, &
                       NBUDGET_RG, NBUDGET_RH, NBUDGET_SV1
use modd_nsv,    only: nsv

use mode_msg

character(len=3) :: ybudgetnum
integer          :: ibudget
integer          :: jsv

call Print_msg( NVERB_DEBUG, 'BUD', 'Budget_preallocate', 'called' )

if ( allocated( tbudgets ) ) then
  call Print_msg( NVERB_WARNING, 'BUD', 'Budget_preallocate', 'tbudgets already allocated' )
  return
end if

nbudgets = NBUDGET_SV1 - 1 + nsv
allocate( tbudgets( nbudgets ) )

tbudgets(NBUDGET_U)%cname    = "BU_RU"
tbudgets(NBUDGET_U)%ccomment = "Budget for U"
tbudgets(NBUDGET_U)%nid      = NBUDGET_U

tbudgets(NBUDGET_V)%cname    = "BU_RV"
tbudgets(NBUDGET_V)%ccomment = "Budget for V"
tbudgets(NBUDGET_V)%nid      = NBUDGET_V

tbudgets(NBUDGET_W)%cname    = "BU_RW"
tbudgets(NBUDGET_W)%ccomment = "Budget for W"
tbudgets(NBUDGET_W)%nid      = NBUDGET_W

tbudgets(NBUDGET_TH)%cname    = "BU_RTH"
tbudgets(NBUDGET_TH)%ccomment = "Budget for potential temperature"
tbudgets(NBUDGET_TH)%nid      = NBUDGET_TH

tbudgets(NBUDGET_TKE)%cname    = "BU_RTKE"
tbudgets(NBUDGET_TKE)%ccomment = "Budget for turbulent kinetic energy"
tbudgets(NBUDGET_TKE)%nid      = NBUDGET_TKE

tbudgets(NBUDGET_RV)%cname    = "BU_RRV"
tbudgets(NBUDGET_RV)%ccomment = "Budget for water vapor mixing ratio"
tbudgets(NBUDGET_RV)%nid      = NBUDGET_RV

tbudgets(NBUDGET_RC)%cname    = "BU_RRC"
tbudgets(NBUDGET_RC)%ccomment = "Budget for cloud water mixing ratio"
tbudgets(NBUDGET_RC)%nid      = NBUDGET_RC

tbudgets(NBUDGET_RR)%cname    = "BU_RRR"
tbudgets(NBUDGET_RR)%ccomment = "Budget for rain water mixing ratio"
tbudgets(NBUDGET_RR)%nid      = NBUDGET_RR

tbudgets(NBUDGET_RI)%cname    = "BU_RRI"
tbudgets(NBUDGET_RI)%ccomment = "Budget for cloud ice mixing ratio"
tbudgets(NBUDGET_RI)%nid      = NBUDGET_RI

tbudgets(NBUDGET_RS)%cname    = "BU_RRS"
tbudgets(NBUDGET_RS)%ccomment = "Budget for snow/aggregate mixing ratio"
tbudgets(NBUDGET_RS)%nid      = NBUDGET_RS

tbudgets(NBUDGET_RG)%cname    = "BU_RRG"
tbudgets(NBUDGET_RG)%ccomment = "Budget for graupel mixing ratio"
tbudgets(NBUDGET_RG)%nid      = NBUDGET_RG

tbudgets(NBUDGET_RH)%cname    = "BU_RRH"
tbudgets(NBUDGET_RH)%ccomment = "Budget for hail mixing ratio"
tbudgets(NBUDGET_RH)%nid      = NBUDGET_RH

do jsv = 1, nsv
  ibudget = NBUDGET_SV1 - 1 + jsv
  write ( ybudgetnum, '( i3.3 )' ) jsv
  tbudgets(ibudget)%cname    = 'BU_RSV_' // ybudgetnum
  tbudgets(ibudget)%ccomment = 'Budget for scalar variable ' // ybudgetnum
  tbudgets(ibudget)%nid      = ibudget
end do


end subroutine Budget_preallocate


!     #################################################################
      SUBROUTINE Ini_budget(KLUOUT,PTSTEP,KSV,KRR,                    &
      ONUMDIFU,ONUMDIFTH,ONUMDIFSV,                                   &
      OHORELAX_UVWTH,OHORELAX_RV,OHORELAX_RC,OHORELAX_RR,             &
      OHORELAX_RI,OHORELAX_RS, OHORELAX_RG, OHORELAX_RH,OHORELAX_TKE, &
      OHORELAX_SV, OVE_RELAX, ove_relax_grd, OCHTRANS,                &
      ONUDGING,ODRAGTREE,ODEPOTREE,                                   &
      HRAD,HDCONV,HSCONV,HTURB,HTURBDIM,HCLOUD                        )
!     #################################################################
!
!!****  *INI_BUDGET* - routine to initialize the parameters for the budgets
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to set or compute the parameters used
!     by the MESONH budgets. Names of files for budget recording are processed 
!     and storage arrays are initialized.               
!
!!**  METHOD
!!    ------
!!      The essential of information is passed by modules. The choice of budgets 
!!    and processes set by the user as integers is converted in "actions" 
!!    readable  by the subroutine BUDGET under the form of string characters. 
!!    For each complete process composed of several elementary processes, names 
!!    of elementary processes are concatenated in order to have an explicit name
!!    in the comment of the recording file for budget. 
!!
!!      
!!    EXTERNAL
!!    --------   
!!      None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      Modules MODD_*
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation (routine INI_BUDGET)
!!      
!!
!!    AUTHOR
!!    ------
!!	P. Hereil      * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        01/03/95 
!!      J. Stein        25/06/95  put the sources in phase with the code
!!      J. Stein        20/07/95  reset to FALSE of all the switches when
!!                                CBUTYPE /= MASK or CART
!!      J. Stein        26/06/96  add the new sources + add the increment between
!!                                2 active processes
!!      J.-P. Pinty     13/12/96  Allowance of multiple SVs
!!      J.-P. Pinty     11/01/97  Includes deep convection ice and forcing processes
!!      J.-P. Lafore    10/02/98  Allocation of the RHODJs for budget
!!      V. Ducrocq      04/06/99  //  
!!      N. Asencio      18/06/99  // MASK case : delete KIMAX and KJMAX arguments,
!!                                GET_DIM_EXT_ll initializes the dimensions of the
!!                                extended local domain.
!!                                LBU_MASK and NBUSURF are allocated on the extended
!!                                local domain.
!!                                add 3 local variables IBUDIM1,IBUDIM2,IBUDIM3
!!                                to define the dimensions of the budget arrays
!!                                in the different cases CART and MASK
!!      J.-P. Pinty     23/09/00  add budget for C2R2
!!      V. Masson       18/11/02  add budget for 2way nesting
!!      O.Geoffroy      03/2006   Add KHKO scheme
!!      J.-P. Pinty     22/04/97  add the explicit hail processes
!!      C.Lac           10/08/07  Add ADV for PPM without contribution
!!                                of each direction
!!      C. Barthe       19/11/09  Add atmospheric electricity
!!      C.Lac           01/07/11  Add vegetation drag        
!!      P. Peyrille, M. Tomasini : include in the forcing term the 2D forcing
!!                                terms in term 2DFRC search for modif PP . but Not very clean! 
!!      C .Lac          27/05/14    add negative corrections for chemical species
!!      C.Lac           29/01/15  Correction for NSV_USER
!!      J.Escobar       02/10/2015 modif for JPHEXT(JPVEXT) variable  
!!      C.Lac           04/12/15  Correction for LSUPSAT 
!  C. Lac         04/2016: negative contribution to the budget split between advection, turbulence and microphysics for KHKO/C2R2
!  C. Barthe      01/2016: add budget for LIMA
!  C. Lac         10/2016: add budget for droplet deposition
!  S. Riette      11/2016: new budgets for ICE3/ICE4
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 10/04/2019: replace ABORT and STOP calls by Print_msg
!  P. Wautelet 15/11/2019: remove unused CBURECORD variable
!  P. Wautelet 24/02/2020: bugfix: corrected condition for budget NCDEPITH
!  P. Wautelet 26/02/2020: bugfix: rename CEVA->REVA for budget for raindrop evaporation in C2R2 (necessary after commit 4ed805fc)
!  P. Wautelet 26/02/2020: bugfix: add missing condition on OCOLD for NSEDIRH budget in LIMA case
!  P. Wautelet 02-03/2020: use the new data structures and subroutines for budgets
!  B. Vie      02/03/2020: LIMA negativity checks after turbulence, advection and microphysics budgets
!  P .Wautelet 09/03/2020: add missing budgets for electricity
!  P. Wautelet 25/03/2020: add missing ove_relax_grd
!  P. Wautelet 23/04/2020: add nid in tbudgetdata datatype
!  P. Wautelet + Benoit Vié 11/06/2020: improve removal of negative scalar variables + adapt the corresponding budgets
!  P. Wautelet 30/06/2020: use NADVSV when possible
!  P. Wautelet 30/06/2020: add NNETURSV, NNEADVSV and NNECONSV variables
!  P. Wautelet 06/07/2020: bugfix: add condition on HTURB for NETUR sources for SV budgets
!  P. Wautelet 08/12/2020: add nbusubwrite and nbutotwrite
!  P. Wautelet 11/01/2021: ignore xbuwri for cartesian boxes (write at every xbulen interval)
!  P. Wautelet 01/02/2021: bugfix: add missing CEDS source terms for SV budgets
!  P. Wautelet 02/02/2021: budgets: add missing source terms for SV budgets in LIMA
!  P. Wautelet 03/02/2021: budgets: add new source if LIMA splitting: CORR2
!  P. Wautelet 10/02/2021: budgets: add missing sources for NSV_C2R2BEG+3 budget
!  P. Wautelet 11/02/2021: budgets: add missing term SCAV for NSV_LIMA_SCAVMASS budget
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------ 
!
use modd_2d_frc,        only: l2d_adv_frc, l2d_rel_frc
use modd_budget
use modd_ch_aerosol,    only: lorilam
use modd_conf,          only: l1d, lcartesian, lforcing, lthinshell, nmodel
use modd_dust,          only: ldust
use modd_dyn,           only: lcorio, xseglen
use modd_dyn_n,         only: xtstep
use modd_elec_descr,    only: linductive, lrelax2fw_ion
use modd_field,         only: TYPEREAL
use modd_nsv,           only: nsv_aerbeg, nsv_aerend, nsv_aerdepbeg, nsv_aerdepend, nsv_c2r2beg, nsv_c2r2end,      &
                              nsv_chembeg, nsv_chemend, nsv_chicbeg, nsv_chicend, nsv_csbeg, nsv_csend,            &
                              nsv_dstbeg, nsv_dstend, nsv_dstdepbeg, nsv_dstdepend, nsv_elecbeg, nsv_elecend,      &
#ifdef MNH_FOREFIRE
                              nsv_ffbeg, nsv_ffend,                                                                &
#endif
                              nsv_lgbeg, nsv_lgend,                                                                &
                              nsv_lima_beg, nsv_lima_end, nsv_lima_ccn_acti, nsv_lima_ccn_free, nsv_lima_hom_haze, &
                              nsv_lima_ifn_free, nsv_lima_ifn_nucl, nsv_lima_imm_nucl,                             &
                              nsv_lima_nc, nsv_lima_nr, nsv_lima_ni, nsv_lima_scavmass,                            &
                              nsv_lnoxbeg, nsv_lnoxend, nsv_ppbeg, nsv_ppend,                                      &
                              nsv_sltbeg, nsv_sltend, nsv_sltdepbeg, nsv_sltdepend, nsv_snwbeg, nsv_snwend,        &
                              nsv_user
use modd_parameters,   only: jphext
use modd_param_c2r2,   only: ldepoc_c2r2 => ldepoc, lrain_c2r2 => lrain, lsedc_c2r2 => lsedc, lsupsat_c2r2 => lsupsat
use modd_param_ice,    only: ladj_after, ladj_before, ldeposc_ice => ldeposc, lred, lsedic_ice => lsedic, lwarm_ice => lwarm
use modd_param_n,      only: cactccn, celec
use modd_param_lima,   only: laero_mass_lima => laero_mass, lacti_lima => lacti, lcold_lima => lcold, ldepoc_lima => ldepoc, &
                             lhail_lima => lhail, lhhoni_lima => lhhoni, lmeyers_lima => lmeyers, lnucl_lima => lnucl,  &
                             lptsplit,                                                                                  &
                             lrain_lima => lrain, lscav_lima => lscav, lsedc_lima => lsedc, lsedi_lima => lsedi,        &
                             lsnow_lima => lsnow, lwarm_lima => lwarm,                                                  &
                             nmod_ccn, nmod_ifn, nmod_imm
use modd_salt,         only: lsalt
use modd_viscosity,    only: lvisc, lvisc_r, lvisc_sv, lvisc_th, lvisc_uvw

USE MODE_ll
USE MODE_MSG

IMPLICIT NONE
!
!*       0.1   declarations of argument
!
!
INTEGER,         INTENT(IN) :: KLUOUT   ! Logical unit number for prints
REAL, INTENT(IN)    :: PTSTEP           ! time step
INTEGER, INTENT(IN) :: KSV              ! number of scalar variables
INTEGER, INTENT(IN) :: KRR              ! number of moist variables
LOGICAL, INTENT(IN) :: ONUMDIFU         ! switch to activate the numerical
                                        ! diffusion for momentum
LOGICAL, INTENT(IN) :: ONUMDIFTH        ! for meteorological scalar variables
LOGICAL, INTENT(IN) :: ONUMDIFSV        ! for tracer scalar variables
LOGICAL, INTENT(IN) :: OHORELAX_UVWTH  ! switch for the
                       ! horizontal relaxation for U,V,W,TH
LOGICAL, INTENT(IN) :: OHORELAX_RV     ! switch for the
                       ! horizontal relaxation for Rv
LOGICAL, INTENT(IN) :: OHORELAX_RC     ! switch for the
                       ! horizontal relaxation for Rc
LOGICAL, INTENT(IN) :: OHORELAX_RR     ! switch for the
                       ! horizontal relaxation for Rr
LOGICAL, INTENT(IN) :: OHORELAX_RI     ! switch for the
                       ! horizontal relaxation for Ri
LOGICAL, INTENT(IN) :: OHORELAX_RS     ! switch for the
                       ! horizontal relaxation for Rs
LOGICAL, INTENT(IN) :: OHORELAX_RG     ! switch for the
                       ! horizontal relaxation for Rg
LOGICAL, INTENT(IN) :: OHORELAX_RH     ! switch for the
                       ! horizontal relaxation for Rh
LOGICAL, INTENT(IN) :: OHORELAX_TKE    ! switch for the
                       ! horizontal relaxation for tke
LOGICAL,DIMENSION(:),INTENT(IN):: OHORELAX_SV     ! switch for the
                       ! horizontal relaxation for scalar variables
LOGICAL, INTENT(IN) :: OVE_RELAX        ! switch to activate the vertical
                                        ! relaxation
logical, intent(in) :: ove_relax_grd    ! switch to activate the vertical
                                        ! relaxation to the lowest verticals
LOGICAL, INTENT(IN) :: OCHTRANS         ! switch to activate convective
                                        !transport for SV
LOGICAL, INTENT(IN) :: ONUDGING         ! switch to activate nudging
LOGICAL, INTENT(IN) :: ODRAGTREE        ! switch to activate vegetation drag
LOGICAL, INTENT(IN) :: ODEPOTREE        ! switch to activate droplet deposition on tree
CHARACTER (LEN=*), INTENT(IN) :: HRAD   ! type of the radiation scheme
CHARACTER (LEN=*), INTENT(IN) :: HDCONV ! type of the deep convection scheme
CHARACTER (LEN=*), INTENT(IN) :: HSCONV ! type of the shallow convection scheme
CHARACTER (LEN=*), INTENT(IN) :: HTURB  ! type of the turbulence scheme
CHARACTER (LEN=*), INTENT(IN) :: HTURBDIM! dimensionnality of the turbulence 
                                        ! scheme
CHARACTER (LEN=*), INTENT(IN) :: HCLOUD ! type of microphysical scheme
!
!*       0.2   declarations of local variables
!
real, parameter :: ITOL = 1e-6

INTEGER :: JI, JJ, JK , JJJ                               ! loop indices
INTEGER :: IIMAX_ll, IJMAX_ll ! size of the physical global domain
INTEGER :: IIU, IJU                                       ! size along x and y directions
                                                          ! of the extended subdomain
INTEGER :: IBUDIM1                                        ! first dimension of the budget arrays
                                                          ! = NBUIMAX in CART case
                                                          ! = NBUKMAX in MASK case
INTEGER :: IBUDIM2                                        ! second dimension of the budget arrays
                                                          ! = NBUJMAX in CART case
                                                          ! = nbusubwrite in MASK case
INTEGER :: IBUDIM3                                        ! third dimension of the budget arrays
                                                          ! = NBUKMAX in CART case
                                                          ! = NBUMASK in MASK case
character(len=3)    :: ybudgetnum
INTEGER             :: JSV               ! loop indice for the SVs
INTEGER             :: IINFO_ll ! return status of the interface routine
integer             :: ibudget
integer             :: isourcesmax          ! Maximum number of source terms in a budget
integer             :: igroup
logical             :: gcond
logical             :: gtmp
type(tbusourcedata) :: tzsource ! Used to prepare metadate of source terms

call Print_msg( NVERB_DEBUG, 'BUD', 'Ini_budget', 'called' )
!
!*       1.    COMPUTE BUDGET VARIABLES
!              ------------------------
!
NBUSTEP = NINT (XBULEN / PTSTEP)
NBUTSHIFT=0
!
!  common dimension for all CBUTYPE values
!
IF (LBU_KCP) THEN
  NBUKMAX = 1
ELSE
  NBUKMAX = NBUKH - NBUKL +1
END IF
!
if ( cbutype == 'CART' .or. cbutype == 'MASK' ) then
  !Check if xbulen is a multiple of xtstep (within tolerance)
  if ( Abs( Nint( xbulen / xtstep ) * xtstep - xbulen ) > ( ITOL * xtstep ) ) &
    call Print_msg( NVERB_WARNING, 'BUD', 'Ini_budget', 'xbulen is not a multiple of xtstep' )

  if ( cbutype == 'CART' ) then
    !Check if xseglen is a multiple of xbulen (within tolerance)
    if ( Abs( Nint( xseglen / xbulen ) * xbulen - xseglen ) > ( ITOL * xseglen ) ) &
      call Print_msg( NVERB_WARNING, 'BUD', 'Ini_budget', 'xseglen is not a multiple of xbulen' )

    !Write cartesian budgets every xbulen time period (do not take xbuwri into account)
    xbuwri = xbulen

    nbusubwrite = 1                                      !Number of budget time average periods for each write
    nbutotwrite = nbusubwrite * Nint( xseglen / xbulen ) !Total number of budget time average periods
  else if ( cbutype == 'MASK' ) then
    !Check if xbuwri is a multiple of xtstep (within tolerance)
    if ( Abs( Nint( xbuwri / xtstep ) * xtstep - xbuwri ) > ( ITOL * xtstep ) ) &
      call Print_msg( NVERB_WARNING, 'BUD', 'Ini_budget', 'xbuwri is not a multiple of xtstep' )

    !Check if xbuwri is a multiple of xbulen (within tolerance)
    if ( Abs( Nint( xbuwri / xbulen ) * xbulen - xbuwri ) > ( ITOL * xbulen ) ) &
      call Print_msg( NVERB_WARNING, 'BUD', 'Ini_budget', 'xbuwri is not a multiple of xbulen' )

    !Check if xseglen is a multiple of xbuwri (within tolerance)
    if ( Abs( Nint( xseglen / xbuwri ) * xbuwri - xseglen ) > ( ITOL * xseglen ) ) &
      call Print_msg( NVERB_WARNING, 'BUD', 'Ini_budget', 'xseglen is not a multiple of xbuwri' )

    nbusubwrite = Nint ( xbuwri / xbulen )               !Number of budget time average periods for each write
    nbutotwrite = nbusubwrite * Nint( xseglen / xbuwri ) !Total number of budget time average periods
  end if
end if

IF (CBUTYPE=='CART') THEN              ! cartesian case only
!
  IF ( NBUIH < NBUIL ) CALL Print_msg( NVERB_ERROR, 'BUD', 'Ini_budget', 'NBUIH < NBUIL' )
  IF (LBU_ICP) THEN
    NBUIMAX_ll = 1
  ELSE
    NBUIMAX_ll = NBUIH - NBUIL +1
  END IF

  IF ( NBUJH < NBUJL ) CALL Print_msg( NVERB_ERROR, 'BUD', 'Ini_budget', 'NBUJH < NBUJL' )
  IF (LBU_JCP) THEN
    NBUJMAX_ll = 1
  ELSE
    NBUJMAX_ll = NBUJH - NBUJL +1
  END IF

  IF ( NBUKH < NBUKL ) CALL Print_msg( NVERB_ERROR, 'BUD', 'Ini_budget', 'NBUKH < NBUKL' )
!
  CALL GET_INTERSECTION_ll(NBUIL+JPHEXT,NBUJL+JPHEXT,NBUIH+JPHEXT,NBUJH+JPHEXT, &
      NBUSIL,NBUSJL,NBUSIH,NBUSJH,"PHYS",IINFO_ll)
  IF ( IINFO_ll /= 1 ) THEN ! 
    IF (LBU_ICP) THEN 
      NBUIMAX = 1
    ELSE
      NBUIMAX = NBUSIH - NBUSIL +1
    END IF
    IF (LBU_JCP) THEN 
      NBUJMAX = 1
    ELSE
      NBUJMAX =  NBUSJH - NBUSJL +1
    END IF
  ELSE ! the intersection is void 
    CBUTYPE='SKIP'  ! no budget on this processor       
    NBUIMAX = 0     ! in order to allocate void arrays
    NBUJMAX = 0
  ENDIF
! three first dimensions of budget arrays in cart and skip cases
   IBUDIM1=NBUIMAX
   IBUDIM2=NBUJMAX
   IBUDIM3=NBUKMAX
! these variables are not be used 
   NBUMASK=-1
!
ELSEIF (CBUTYPE=='MASK') THEN          ! mask case only 
!
  LBU_ENABLE=.TRUE.
                                    ! result on the FM_FILE
  NBUTIME = 1

  CALL GET_DIM_EXT_ll ('B', IIU,IJU)
  ALLOCATE( LBU_MASK( IIU ,IJU, NBUMASK) )
  LBU_MASK(:,:,:)=.FALSE.
  ALLOCATE( NBUSURF( IIU, IJU, NBUMASK, nbusubwrite) )
  NBUSURF(:,:,:,:) = 0
!
! three first dimensions of budget arrays in mask case
!  the order of the dimensions are the order expected in WRITE_DIACHRO routine:
!  x,y,z,time,mask,processus  and in this case x and y are missing
!  first dimension of the arrays : dimension along K
!  second dimension of the arrays : number of the budget time period
!  third dimension of the arrays : number of the budget masks zones
  IBUDIM1=NBUKMAX
  IBUDIM2=nbusubwrite
  IBUDIM3=NBUMASK
! these variables are not used in this case
  NBUIMAX=-1
  NBUJMAX=-1
! the beginning and the end along x and y direction : global extended domain
 ! get dimensions of the physical global domain
   CALL GET_GLOBALDIMS_ll (IIMAX_ll,IJMAX_ll)
   NBUIL=1
   NBUIH=IIMAX_ll + 2 * JPHEXT
   NBUJL=1 
   NBUJH=IJMAX_ll + 2 * JPHEXT
!
ELSE                      ! default case
!
  LBU_ENABLE=.FALSE.
  NBUIMAX = -1
  NBUJMAX = -1
  LBU_RU = .FALSE.
  LBU_RV = .FALSE.
  LBU_RW = .FALSE.
  LBU_RTH= .FALSE.
  LBU_RTKE= .FALSE.
  LBU_RRV= .FALSE.
  LBU_RRC= .FALSE.
  LBU_RRR= .FALSE.
  LBU_RRI= .FALSE.
  LBU_RRS= .FALSE.
  LBU_RRG= .FALSE.
  LBU_RRH= .FALSE.
  LBU_RSV= .FALSE.
!
! three first dimensions of budget arrays in default case
  IBUDIM1=0
  IBUDIM2=0
  IBUDIM3=0
!
END IF  
!
!
!-------------------------------------------------------------------------------
!
!*       2.    ALLOCATE MEMORY FOR BUDGET ARRAYS AND INITIALIZE
!              ------------------------------------------------
!
LBU_BEG =.TRUE. 
!
!-------------------------------------------------------------------------------
!
!*       3.    INITALIZE VARIABLES
!              -------------------
!
!Create intermediate variable to store rhodj for scalar variables
if ( lbu_rth .or. lbu_rtke .or. lbu_rrv .or. lbu_rrc .or. lbu_rrr .or. &
     lbu_rri .or. lbu_rrs  .or. lbu_rrg .or. lbu_rrh .or. lbu_rsv      ) then
  allocate( tburhodj )

  tburhodj%cmnhname  = 'RhodJS'
  tburhodj%cstdname  = ''
  tburhodj%clongname = 'RhodJS'
  tburhodj%cunits    = 'kg'
  tburhodj%ccomment  = 'RhodJ for Scalars variables'
  tburhodj%ngrid     = 1
  tburhodj%ntype     = TYPEREAL
  tburhodj%ndims     = 3

  allocate( tburhodj%xdata(ibudim1, ibudim2, ibudim3) )
  tburhodj%xdata(:, :, :) = 0.
end if


tzsource%ntype    = TYPEREAL
tzsource%ndims    = 3

! Budget of RU
tbudgets(NBUDGET_U)%lenabled = lbu_ru

if ( lbu_ru ) then
  allocate( tbudgets(NBUDGET_U)%trhodj )

  tbudgets(NBUDGET_U)%trhodj%cmnhname  = 'RhodJX'
  tbudgets(NBUDGET_U)%trhodj%cstdname  = ''
  tbudgets(NBUDGET_U)%trhodj%clongname = 'RhodJX'
  tbudgets(NBUDGET_U)%trhodj%cunits    = 'kg'
  tbudgets(NBUDGET_U)%trhodj%ccomment  = 'RhodJ for momentum along X axis'
  tbudgets(NBUDGET_U)%trhodj%ngrid     = 2
  tbudgets(NBUDGET_U)%trhodj%ntype     = TYPEREAL
  tbudgets(NBUDGET_U)%trhodj%ndims     = 3

  allocate( tbudgets(NBUDGET_U)%trhodj%xdata(ibudim1, ibudim2, ibudim3) )
  tbudgets(NBUDGET_U)%trhodj%xdata(:, :, :) = 0.

  !Allocate all basic source terms (used or not)
  !The size should be large enough (bigger than necessary is OK)
  isourcesmax = 18
  tbudgets(NBUDGET_U)%nsourcesmax = isourcesmax
  allocate( tbudgets(NBUDGET_U)%tsources(isourcesmax) )

  allocate( tbudgets(NBUDGET_U)%xtmpstore(ibudim1, ibudim2, ibudim3) )

  tbudgets(NBUDGET_U)%tsources(:)%ngroup = 0

  tzsource%ccomment = 'Budget of momentum along X axis'
  tzsource%ngrid    = 2

  tzsource%cunits   = 'm s-1'

  gcond = .true.
  tzsource%cmnhname  = 'INIF'
  tzsource%clongname = 'initial state'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'ENDF'
  tzsource%clongname = 'final state'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'AVEF'
  tzsource%clongname = 'averaged state'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .false. )

  tzsource%cunits   = 'm s-2'

  gcond = .true.
  tzsource%cmnhname  = 'ASSE'
  tzsource%clongname = 'time filter (Asselin)'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, nasseu )

  gcond = nmodel > 1
  tzsource%cmnhname  = 'NEST'
  tzsource%clongname = 'nesting'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, nnestu )

  gcond = lforcing
  tzsource%cmnhname  = 'FRC'
  tzsource%clongname = 'forcing'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, nfrcu )

  gcond = onudging
  tzsource%cmnhname  = 'NUD'
  tzsource%clongname = 'nudging'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, nnudu )

  gcond = .not.l1d .and. .not.lcartesian
  tzsource%cmnhname  = 'CURV'
  tzsource%clongname = 'curvature'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, ncurvu )

  gcond = lcorio
  tzsource%cmnhname  = 'COR'
  tzsource%clongname = 'Coriolis'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, ncoru )

  gcond = onumdifu
  tzsource%cmnhname  = 'DIF'
  tzsource%clongname = 'numerical diffusion'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, ndifu )

  gcond = ohorelax_uvwth .or. ove_relax .or. ove_relax_grd
  tzsource%cmnhname  = 'REL'
  tzsource%clongname = 'relaxation'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, nrelu )

  gcond = odragtree
  tzsource%cmnhname  = 'DRAG'
  tzsource%clongname = 'drag force'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, ndragu )

  gcond = hturb == 'TKEL'
  tzsource%cmnhname  = 'VTURB'
  tzsource%clongname = 'vertical turbulent diffusion'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, nvturbu )

  gcond = hturb == 'TKEL' .and. HTURBDIM == '3DIM'
  tzsource%cmnhname  = 'HTURB'
  tzsource%clongname = 'horizontal turbulent diffusion'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, nhturbu )

  gcond = hsconv == 'EDKF'
  tzsource%cmnhname  = 'MAFL'
  tzsource%clongname = 'mass flux'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, nmaflu )

  gcond = lvisc .and. lvisc_uvw
  tzsource%cmnhname  = 'VISC'
  tzsource%clongname = 'viscosity'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, nviscu )

  gcond = .true.
  tzsource%cmnhname  = 'ADV'
  tzsource%clongname = 'advection'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, nadvu )

  gcond = .true.
  tzsource%cmnhname  = 'PRES'
  tzsource%clongname = 'pressure'
  call Budget_source_add( tbudgets(NBUDGET_U), tzsource, gcond, npresu )
end if

! Budget of RV
tbudgets(NBUDGET_V)%lenabled = lbu_rv

if ( lbu_rv ) then
  allocate( tbudgets(NBUDGET_V)%trhodj )

  tbudgets(NBUDGET_V)%trhodj%cmnhname  = 'RhodJY'
  tbudgets(NBUDGET_V)%trhodj%cstdname  = ''
  tbudgets(NBUDGET_V)%trhodj%clongname = 'RhodJY'
  tbudgets(NBUDGET_V)%trhodj%cunits    = 'kg'
  tbudgets(NBUDGET_V)%trhodj%ccomment  = 'RhodJ for momentum along Y axis'
  tbudgets(NBUDGET_V)%trhodj%ngrid     = 3
  tbudgets(NBUDGET_V)%trhodj%ntype     = TYPEREAL
  tbudgets(NBUDGET_V)%trhodj%ndims     = 3

  allocate( tbudgets(NBUDGET_V)%trhodj%xdata(ibudim1, ibudim2, ibudim3) )
  tbudgets(NBUDGET_V)%trhodj%xdata(:, :, :) = 0.

  !Allocate all basic source terms (used or not)
  !The size should be large enough (bigger than necessary is OK)
  isourcesmax = 18
  tbudgets(NBUDGET_V)%nsourcesmax = isourcesmax
  allocate( tbudgets(NBUDGET_V)%tsources(isourcesmax) )

  allocate( tbudgets(NBUDGET_V)%xtmpstore(ibudim1, ibudim2, ibudim3) )

  tbudgets(NBUDGET_V)%tsources(:)%ngroup = 0

  tzsource%ccomment = 'Budget of momentum along Y axis'
  tzsource%ngrid    = 3

  tzsource%cunits   = 'm s-1'

  gcond = .true.
  tzsource%cmnhname  = 'INIF'
  tzsource%clongname = 'initial state'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'ENDF'
  tzsource%clongname = 'final state'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'AVEF'
  tzsource%clongname = 'averaged state'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .false. )

  tzsource%cunits   = 'm s-2'

  gcond = .true.
  tzsource%cmnhname  = 'ASSE'
  tzsource%clongname = 'time filter (Asselin)'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, nassev )

  gcond = nmodel > 1
  tzsource%cmnhname  = 'NEST'
  tzsource%clongname = 'nesting'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, nnestv )

  gcond = lforcing
  tzsource%cmnhname  = 'FRC'
  tzsource%clongname = 'forcing'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, nfrcv )

  gcond = onudging
  tzsource%cmnhname  = 'NUD'
  tzsource%clongname = 'nudging'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, nnudv )

  gcond = .not.l1d .and. .not.lcartesian
  tzsource%cmnhname  = 'CURV'
  tzsource%clongname = 'curvature'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, ncurvv )

  gcond = lcorio
  tzsource%cmnhname  = 'COR'
  tzsource%clongname = 'Coriolis'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, ncorv )

  gcond = onumdifu
  tzsource%cmnhname  = 'DIF'
  tzsource%clongname = 'numerical diffusion'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, ndifv )

  gcond = ohorelax_uvwth .or. ove_relax .or. ove_relax_grd
  tzsource%cmnhname  = 'REL'
  tzsource%clongname = 'relaxation'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, nrelv )

  gcond = odragtree
  tzsource%cmnhname  = 'DRAG'
  tzsource%clongname = 'drag force'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, ndragv )

  gcond = hturb == 'TKEL'
  tzsource%cmnhname  = 'VTURB'
  tzsource%clongname = 'vertical turbulent diffusion'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, nvturbv )

  gcond = hturb == 'TKEL' .and. HTURBDIM == '3DIM'
  tzsource%cmnhname  = 'HTURB'
  tzsource%clongname = 'horizontal turbulent diffusion'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, nhturbv )

  gcond = hsconv == 'EDKF'
  tzsource%cmnhname  = 'MAFL'
  tzsource%clongname = 'mass flux'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, nmaflv )

  gcond = lvisc .and. lvisc_uvw
  tzsource%cmnhname  = 'VISC'
  tzsource%clongname = 'viscosity'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, nviscv )

  gcond = .true.
  tzsource%cmnhname  = 'ADV'
  tzsource%clongname = 'advection'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, nadvv )

  gcond = .true.
  tzsource%cmnhname  = 'PRES'
  tzsource%clongname = 'pressure'
  call Budget_source_add( tbudgets(NBUDGET_V), tzsource, gcond, npresv )
end if

! Budget of RW
tbudgets(NBUDGET_W)%lenabled = lbu_rw

if ( lbu_rw ) then
  allocate( tbudgets(NBUDGET_W)%trhodj )

  tbudgets(NBUDGET_W)%trhodj%cmnhname  = 'RhodJZ'
  tbudgets(NBUDGET_W)%trhodj%cstdname  = ''
  tbudgets(NBUDGET_W)%trhodj%clongname = 'RhodJZ'
  tbudgets(NBUDGET_W)%trhodj%cunits    = 'kg'
  tbudgets(NBUDGET_W)%trhodj%ccomment  = 'RhodJ for momentum along Y axis'
  tbudgets(NBUDGET_W)%trhodj%ngrid     = 4
  tbudgets(NBUDGET_W)%trhodj%ntype     = TYPEREAL
  tbudgets(NBUDGET_W)%trhodj%ndims     = 3

  allocate( tbudgets(NBUDGET_W)%trhodj%xdata(ibudim1, ibudim2, ibudim3) )
  tbudgets(NBUDGET_W)%trhodj%xdata(:, :, :) = 0.

  !Allocate all basic source terms (used or not)
  !The size should be large enough (bigger than necessary is OK)
  isourcesmax = 17
  tbudgets(NBUDGET_W)%nsourcesmax = isourcesmax
  allocate( tbudgets(NBUDGET_W)%tsources(isourcesmax) )

  allocate( tbudgets(NBUDGET_W)%xtmpstore(ibudim1, ibudim2, ibudim3) )

  tbudgets(NBUDGET_W)%tsources(:)%ngroup = 0

  tzsource%ccomment = 'Budget of momentum along Z axis'
  tzsource%ngrid    = 4

  tzsource%cunits   = 'm s-1'

  gcond = .true.
  tzsource%cmnhname  = 'INIF'
  tzsource%clongname = 'initial state'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'ENDF'
  tzsource%clongname = 'final state'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'AVEF'
  tzsource%clongname = 'averaged state'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .false. )

  tzsource%cunits   = 'm s-2'

  gcond = .true.
  tzsource%cmnhname  = 'ASSE'
  tzsource%clongname = 'time filter (Asselin)'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, nassew )

  gcond = nmodel > 1
  tzsource%cmnhname  = 'NEST'
  tzsource%clongname = 'nesting'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, nnestw )

  gcond = lforcing
  tzsource%cmnhname  = 'FRC'
  tzsource%clongname = 'forcing'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, nfrcw )

  gcond = onudging
  tzsource%cmnhname  = 'NUD'
  tzsource%clongname = 'nudging'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, nnudw )

  gcond = .not.l1d .and. .not.lcartesian .and. .not.lthinshell
  tzsource%cmnhname  = 'CURV'
  tzsource%clongname = 'curvature'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, ncurvw )

  gcond = lcorio .and. .not.l1d .and. .not.lthinshell
  tzsource%cmnhname  = 'COR'
  tzsource%clongname = 'Coriolis'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, ncorw )

  gcond = onumdifu
  tzsource%cmnhname  = 'DIF'
  tzsource%clongname = 'numerical diffusion'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, ndifw )

  gcond = ohorelax_uvwth .or. ove_relax .or. ove_relax_grd
  tzsource%cmnhname  = 'REL'
  tzsource%clongname = 'relaxation'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, nrelw )

  gcond = hturb == 'TKEL'
  tzsource%cmnhname  = 'VTURB'
  tzsource%clongname = 'vertical turbulent diffusion'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, nvturbw )

  gcond = hturb == 'TKEL' .and. HTURBDIM == '3DIM'
  tzsource%cmnhname  = 'HTURB'
  tzsource%clongname = 'horizontal turbulent diffusion'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, nhturbw )

  gcond = lvisc .and. lvisc_uvw
  tzsource%cmnhname  = 'VISC'
  tzsource%clongname = 'viscosity'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, nviscw )

  gcond = .true.
  tzsource%cmnhname  = 'GRAV'
  tzsource%clongname = 'gravity'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, ngravw )

  gcond = .true.
  tzsource%cmnhname  = 'ADV'
  tzsource%clongname = 'advection'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, nadvw )

  gcond = .true.
  tzsource%cmnhname  = 'PRES'
  tzsource%clongname = 'pressure'
  call Budget_source_add( tbudgets(NBUDGET_W), tzsource, gcond, npresw )
end if

! Budget of RTH
tbudgets(NBUDGET_TH)%lenabled = lbu_rth

if ( lbu_rth ) then
  tbudgets(NBUDGET_TH)%trhodj => tburhodj

  !Allocate all basic source terms (used or not)
  !The size should be large enough (bigger than necessary is OK)
  isourcesmax = 52
  tbudgets(NBUDGET_TH)%nsourcesmax = isourcesmax
  allocate( tbudgets(NBUDGET_TH)%tsources(isourcesmax) )

  allocate( tbudgets(NBUDGET_TH)%xtmpstore(ibudim1, ibudim2, ibudim3) )

  tbudgets(NBUDGET_TH)%tsources(:)%ngroup = 0

  tzsource%ccomment = 'Budget of potential temperature'
  tzsource%ngrid    = 1

  tzsource%cunits   = 'K'

  gcond = .true.
  tzsource%cmnhname  = 'INIF'
  tzsource%clongname = 'initial state'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'ENDF'
  tzsource%clongname = 'final state'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'AVEF'
  tzsource%clongname = 'averaged state'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .false. )

  tzsource%cunits   = 'K s-1'

  gcond = .true.
  tzsource%cmnhname  = 'ASSE'
  tzsource%clongname = 'time filter (Asselin)'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nasseth )

  gcond = nmodel > 1
  tzsource%cmnhname  = 'NEST'
  tzsource%clongname = 'nesting'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nnestth )

  gcond = lforcing
  tzsource%cmnhname  = 'FRC'
  tzsource%clongname = 'forcing'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nfrcth )

  gcond = l2d_adv_frc
  tzsource%cmnhname  = '2DADV'
  tzsource%clongname = 'advective forcing'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, n2dadvth )

  gcond = l2d_rel_frc
  tzsource%cmnhname  = '2DREL'
  tzsource%clongname = 'relaxation forcing'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, n2drelth )

  gcond = onudging
  tzsource%cmnhname  = 'NUD'
  tzsource%clongname = 'nudging'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nnudth )

  gcond = krr > 0 .and. .not.l1d
  tzsource%cmnhname  = 'PREF'
  tzsource%clongname = 'reference pressure'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nprefth )

  gcond = onumdifth
  tzsource%cmnhname  = 'DIF'
  tzsource%clongname = 'numerical diffusion'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, ndifth )

  gcond = ohorelax_uvwth .or. ove_relax .or. ove_relax_grd
  tzsource%cmnhname  = 'REL'
  tzsource%clongname = 'relaxation'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nrelth )

  gcond = hrad /= 'NONE'
  tzsource%cmnhname  = 'RAD'
  tzsource%clongname = 'radiation'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nradth )

  gcond = hdconv == 'KAFR' .OR. hsconv == 'KAFR'
  tzsource%cmnhname  = 'DCONV'
  tzsource%clongname = 'KAFR convection'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, ndconvth )

  gcond = hturb == 'TKEL'
  tzsource%cmnhname  = 'VTURB'
  tzsource%clongname = 'vertical turbulent diffusion'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nvturbth )

  gcond = hturb == 'TKEL' .and. HTURBDIM == '3DIM'
  tzsource%cmnhname  = 'HTURB'
  tzsource%clongname = 'horizontal turbulent diffusion'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nhturbth )

  gcond = hturb == 'TKEL'
  tzsource%cmnhname  = 'DISSH'
  tzsource%clongname = 'dissipation'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, ndisshth )

  gcond = hturb == 'TKEL' .and. (      hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
                                  .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' )
  tzsource%cmnhname  = 'NETUR'
  tzsource%clongname = 'negative correction induced by turbulence'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nneturth )

  gcond = hsconv == 'EDKF'
  tzsource%cmnhname  = 'MAFL'
  tzsource%clongname = 'mass flux'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nmaflth )

  gcond = lvisc .and. lvisc_th
  tzsource%cmnhname  = 'VISC'
  tzsource%clongname = 'viscosity'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nviscth )

  gcond = .true.
  tzsource%cmnhname  = 'ADV'
  tzsource%clongname = 'total advection'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nadvth )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEADV'
  tzsource%clongname = 'negative correction induced by advection'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nneadvth )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEGA'
  tzsource%clongname = 'negative correction'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nnegath )

  gcond = hcloud == 'LIMA' .and. lptsplit
  tzsource%cmnhname  = 'SEDI'
  tzsource%clongname = 'heat transport by hydrometeors sedimentation'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nsedith )

  gtmp = cactccn == 'ABRK' .and. (lorilam .or. ldust .or. lsalt )
  gcond =      ( hcloud      == 'LIMA' .and. lwarm_lima .and. lacti_lima .and. nmod_ccn >= 1 )     &
          .or.   hcloud(1:3) == 'ICE'                                                         &
          .or. ( hcloud      == 'C2R2' .and. ( gtmp .or. .not.lsupsat_c2r2 ) ) &
          .or. ( hcloud      == 'KHKO' .and. ( gtmp .or. .not.lsupsat_c2r2 ) )
  tzsource%cmnhname  = 'HENU'
  tzsource%clongname = 'heterogeneous nucleation'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nhenuth )

  gcond =      ( hcloud      == 'LIMA' .and. ( ( .not. lptsplit .and. lwarm_lima .and. lrain_lima ) .or. lptsplit ) ) &
          .or. ( hcloud(1:3) == 'ICE'  .and. lwarm_ice )                                                              &
          .or. ( hcloud      == 'C2R2' .and. lrain_c2r2 )                                                             &
          .or. ( hcloud      == 'KHKO' .and. lrain_c2r2 )                                                             &
          .or.   hcloud      == 'KESS'
  tzsource%cmnhname  = 'REVA'
  tzsource%clongname = 'rain evaporation'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nrevath )

  gcond = hcloud == 'LIMA' .and. lcold_lima .and. lnucl_lima
  tzsource%cmnhname  = 'HIND'
  tzsource%clongname = 'heterogeneous nucleation by deposition'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nhindth )

  gcond = hcloud == 'LIMA' .and. lcold_lima .and. lnucl_lima
  tzsource%cmnhname  = 'HINC'
  tzsource%clongname = 'heterogeneous nucleation by contact'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nhincth )

  gcond = hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'HON'
  tzsource%clongname = 'homogeneous nucleation'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nhonth )

  gcond = hcloud == 'LIMA' .and. lcold_lima .and. lnucl_lima .and. lhhoni_lima .and. nmod_ccn >= 1
  tzsource%cmnhname  = 'HONH'
  tzsource%clongname = 'haze homogeneous nucleation'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nhonhth )

  gcond = hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lnucl_lima ) )
  tzsource%cmnhname  = 'HONC'
  tzsource%clongname = 'droplet homogeneous freezing'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nhoncth )

  gcond = hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lnucl_lima .and. lrain_lima ) )
  tzsource%cmnhname  = 'HONR'
  tzsource%clongname = 'raindrop homogeneous freezing'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nhonrth )

  gcond = hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'SFR'
  tzsource%clongname = 'spontaneous freezing'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nsfrth )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'DEPS'
  tzsource%clongname = 'deposition on snow'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, ndepsth )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'DEPG'
  tzsource%clongname = 'deposition on graupel'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, ndepgth )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'IMLT'
  tzsource%clongname = 'ice melting'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nimltth )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'BERFI'
  tzsource%clongname = 'Bergeron-Findeisen'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nberfith )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'RIM'
  tzsource%clongname = 'riming of cloud droplets'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nrimth )

  gcond =      ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima .and. lrain_lima ) ) ) &
          .or.   hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'ACC'
  tzsource%clongname = 'accretion of rain'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, naccth )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'CFRZ'
  tzsource%clongname = 'conversion freezing of rain'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, ncfrzth )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'WETG'
  tzsource%clongname = 'wet growth of graupel'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nwetgth )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'DRYG'
  tzsource%clongname = 'dry growth of graupel'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, ndrygth )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'GMLT'
  tzsource%clongname = 'graupel melting'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, ngmltth )

  gcond =      ( hcloud == 'LIMA' .and. .not.lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima .and. lsnow_lima ) &
          .or.   hcloud == 'ICE4'
  tzsource%cmnhname  = 'WETH'
  tzsource%clongname = 'wet growth of hail'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nwethth )

  gcond = hcloud == 'ICE4' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'DRYH'
  tzsource%clongname = 'dry growth of hail'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, ndryhth )

  gcond =      ( hcloud == 'LIMA' .and. .not.lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima .and. lsnow_lima ) &
          .or.   hcloud == 'ICE4'
  tzsource%cmnhname  = 'HMLT'
  tzsource%clongname = 'melting of hail'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nhmltth )

  gcond = hcloud(1:3) == 'ICE' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'CORR'
  tzsource%clongname = 'correction'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, ncorrth )

  gcond = hcloud == 'LIMA'
  tzsource%cmnhname  = 'CEDS'
  tzsource%clongname = 'adjustment to saturation'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, ncedsth )

  gcond = hcloud(1:3) == 'ICE' .and. lred .and. ladj_before .and. celec == 'NONE'
  tzsource%cmnhname  = 'ADJU'
  tzsource%clongname = ''
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nadjuth )

  gcond = hcloud(1:3) == 'ICE' .and. ( .not. lred .or. ( lred .and. ladj_after ) .or. celec /= 'NONE' )
  tzsource%cmnhname  = 'CDEPI'
  tzsource%clongname = 'deposition on ice'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, ncdepith )

  gcond = hcloud == 'C2R2' .or. hcloud == 'KHKO' .or. hcloud == 'KESS' .or. hcloud == 'REVE'
  tzsource%cmnhname  = 'COND'
  tzsource%clongname = 'vapor condensation or cloud water evaporation'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, ncondth )

  gcond = (      hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4'   &
            .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' ) &
          .and. celec == 'NONE'
  tzsource%cmnhname  = 'NECON'
  tzsource%clongname = 'negative correction induced by condensation'
  call Budget_source_add( tbudgets(NBUDGET_TH), tzsource, gcond, nneconth )
end if

! Budget of RTKE
tbudgets(NBUDGET_TKE)%lenabled = lbu_rtke

if ( lbu_rtke ) then
  tbudgets(NBUDGET_TKE)%trhodj => tburhodj

  !Allocate all basic source terms (used or not)
  !The size should be large enough (bigger than necessary is OK)
  isourcesmax = 13
  tbudgets(NBUDGET_TKE)%nsourcesmax = isourcesmax
  allocate( tbudgets(NBUDGET_TKE)%tsources(isourcesmax) )

  allocate( tbudgets(NBUDGET_TKE)%xtmpstore(ibudim1, ibudim2, ibudim3) )

  tbudgets(NBUDGET_TKE)%tsources(:)%ngroup = 0

  tzsource%ccomment = 'Budget of turbulent kinetic energy'
  tzsource%ngrid    = 1

  tzsource%cunits   = 'm2 s-1'

  gcond = .true.
  tzsource%cmnhname  = 'INIF'
  tzsource%clongname = 'initial state'
  call Budget_source_add( tbudgets(NBUDGET_TKE), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'ENDF'
  tzsource%clongname = 'final state'
  call Budget_source_add( tbudgets(NBUDGET_TKE), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'AVEF'
  tzsource%clongname = 'averaged state'
  call Budget_source_add( tbudgets(NBUDGET_TKE), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .false. )

  tzsource%cunits   = 'm2 s-3'

  gcond = .true.
  tzsource%cmnhname  = 'ASSE'
  tzsource%clongname = 'time filter (Asselin)'
  call Budget_source_add( tbudgets(NBUDGET_TKE), tzsource, gcond, nassetke )

  gcond = lforcing
  tzsource%cmnhname  = 'FRC'
  tzsource%clongname = 'forcing'
  call Budget_source_add( tbudgets(NBUDGET_TKE), tzsource, gcond, nfrctke )

  gcond = onumdifth
  tzsource%cmnhname  = 'DIF'
  tzsource%clongname = 'numerical diffusion'
  call Budget_source_add( tbudgets(NBUDGET_TKE), tzsource, gcond, ndiftke )

  gcond = ohorelax_tke
  tzsource%cmnhname  = 'REL'
  tzsource%clongname = 'relaxation'
  call Budget_source_add( tbudgets(NBUDGET_TKE), tzsource, gcond, nreltke )

  gcond = odragtree
  tzsource%cmnhname  = 'DRAG'
  tzsource%clongname = 'drag force'
  call Budget_source_add( tbudgets(NBUDGET_TKE), tzsource, gcond, ndragtke )

  gcond = hturb == 'TKEL'
  tzsource%cmnhname  = 'DP'
  tzsource%clongname = 'dynamic production'
  call Budget_source_add( tbudgets(NBUDGET_TKE), tzsource, gcond, ndptke )

  gcond = hturb == 'TKEL'
  tzsource%cmnhname  = 'TP'
  tzsource%clongname = 'thermal production'
  call Budget_source_add( tbudgets(NBUDGET_TKE), tzsource, gcond, ntptke )

  gcond = hturb == 'TKEL'
  tzsource%cmnhname  = 'DISS'
  tzsource%clongname = 'dissipation of TKE'
  call Budget_source_add( tbudgets(NBUDGET_TKE), tzsource, gcond, ndisstke )

  gcond = hturb == 'TKEL'
  tzsource%cmnhname  = 'TR'
  tzsource%clongname = 'turbulent transport'
  call Budget_source_add( tbudgets(NBUDGET_TKE), tzsource, gcond, ntrtke )

  gcond = hturb == 'TKEL'
  tzsource%cmnhname  = 'ADV'
  tzsource%clongname = 'total advection'
  call Budget_source_add( tbudgets(NBUDGET_TKE), tzsource, gcond, nadvtke )

end if

! Budget of RRV
tbudgets(NBUDGET_RV)%lenabled = lbu_rrv .and. krr >= 1

if ( tbudgets(NBUDGET_RV)%lenabled ) then
  tbudgets(NBUDGET_RV)%trhodj => tburhodj

  !Allocate all basic source terms (used or not)
  !The size should be large enough (bigger than necessary is OK)
  isourcesmax = 33
  tbudgets(NBUDGET_RV)%nsourcesmax = isourcesmax
  allocate( tbudgets(NBUDGET_RV)%tsources(isourcesmax) )

  allocate( tbudgets(NBUDGET_RV)%xtmpstore(ibudim1, ibudim2, ibudim3) )

  tbudgets(NBUDGET_RV)%tsources(:)%ngroup = 0

  tzsource%ccomment = 'Budget of water vapor mixing ratio'
  tzsource%ngrid    = 1

  tzsource%cunits   = 'kg kg-1'

  gcond = .true.
  tzsource%cmnhname  = 'INIF'
  tzsource%clongname = 'initial state'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'ENDF'
  tzsource%clongname = 'final state'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'AVEF'
  tzsource%clongname = 'averaged state'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .false. )

  tzsource%cunits   = 's-1'

  gcond = .true.
  tzsource%cmnhname  = 'ASSE'
  tzsource%clongname = 'time filter (Asselin)'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nasserv )

  gcond = nmodel > 1
  tzsource%cmnhname  = 'NEST'
  tzsource%clongname = 'nesting'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nnestrv )

  gcond = lforcing
  tzsource%cmnhname  = 'FRC'
  tzsource%clongname = 'forcing'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nfrcrv )

  gcond = l2d_adv_frc
  tzsource%cmnhname  = '2DADV'
  tzsource%clongname = 'advective forcing'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, n2dadvrv )

  gcond = l2d_rel_frc
  tzsource%cmnhname  = '2DREL'
  tzsource%clongname = 'relaxation forcing'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, n2drelrv )

  gcond = onudging
  tzsource%cmnhname  = 'NUD'
  tzsource%clongname = 'nudging'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nnudrv )

  gcond = onumdifth
  tzsource%cmnhname  = 'DIF'
  tzsource%clongname = 'numerical diffusion'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, ndifrv )

  gcond = ohorelax_rv
  tzsource%cmnhname  = 'REL'
  tzsource%clongname = 'relaxation'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nrelrv )

  gcond = hdconv == 'KAFR' .OR. hsconv == 'KAFR'
  tzsource%cmnhname  = 'DCONV'
  tzsource%clongname = 'KAFR convection'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, ndconvrv )

  gcond = hturb == 'TKEL'
  tzsource%cmnhname  = 'VTURB'
  tzsource%clongname = 'vertical turbulent diffusion'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nvturbrv )

  gcond = hturb == 'TKEL' .and. HTURBDIM == '3DIM'
  tzsource%cmnhname  = 'HTURB'
  tzsource%clongname = 'horizontal turbulent diffusion'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nhturbrv )

  gcond = hturb == 'TKEL' .and. (      hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
                                  .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' )
  tzsource%cmnhname  = 'NETUR'
  tzsource%clongname = 'negative correction induced by turbulence'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nneturrv )

  gcond = hsconv == 'EDKF'
  tzsource%cmnhname  = 'MAFL'
  tzsource%clongname = 'mass flux'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nmaflrv )

  gcond = lvisc .and. lvisc_r
  tzsource%cmnhname  = 'VISC'
  tzsource%clongname = 'viscosity'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nviscrv )

  gcond = .true.
  tzsource%cmnhname  = 'ADV'
  tzsource%clongname = 'total advection'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nadvrv )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEADV'
  tzsource%clongname = 'negative correction induced by advection'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nneadvrv )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEGA'
  tzsource%clongname = 'negative correction'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nnegarv )

  gtmp = cactccn == 'ABRK' .and. (lorilam .or. ldust .or. lsalt )
  gcond =      ( hcloud      == 'LIMA' .and. lwarm_lima .and. lacti_lima .and. nmod_ccn >= 1 )     &
          .or.   hcloud(1:3) == 'ICE'                                                              &
          .or. ( hcloud      == 'C2R2' .and. ( gtmp .or. .not.lsupsat_c2r2 ) ) &
          .or. ( hcloud      == 'KHKO' .and. ( gtmp .or. .not.lsupsat_c2r2 ) )
  tzsource%cmnhname  = 'HENU'
  tzsource%clongname = 'heterogeneous nucleation'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nhenurv )

  gcond =      ( hcloud      == 'LIMA' .and. ( ( .not. lptsplit .and. lwarm_lima .and. lrain_lima ) .or. lptsplit ) ) &
          .or. ( hcloud(1:3) == 'ICE'  .and. lwarm_ice )                                                              &
          .or. ( hcloud      == 'C2R2' .and. lrain_c2r2 )                                                             &
          .or. ( hcloud      == 'KHKO' .and. lrain_c2r2 )                                                             &
          .or.   hcloud      == 'KESS'
  tzsource%cmnhname  = 'REVA'
  tzsource%clongname = 'rain evaporation'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nrevarv )

  gcond = hcloud == 'LIMA' .and. lcold_lima .and. lnucl_lima
  tzsource%cmnhname  = 'HIND'
  tzsource%clongname = 'heterogeneous nucleation by deposition'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nhindrv )

  gcond = hcloud == 'LIMA' .and. lcold_lima .and. lnucl_lima .and. lhhoni_lima .and. nmod_ccn >= 1
  tzsource%cmnhname  = 'HONH'
  tzsource%clongname = 'haze homogeneous nucleation'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nhonhrv )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'DEPS'
  tzsource%clongname = 'deposition on snow'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, ndepsrv )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'DEPG'
  tzsource%clongname = 'deposition on graupel'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, ndepgrv )

  gcond = hcloud == 'LIMA'
  tzsource%cmnhname  = 'CEDS'
  tzsource%clongname = 'adjustment to saturation'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, ncedsrv )

  gcond = hcloud(1:3) == 'ICE' .and. lred .and. ladj_before .and. celec == 'NONE'
  tzsource%cmnhname  = 'ADJU'
  tzsource%clongname = 'adjustment before'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nadjurv )

  gcond = hcloud == 'C2R2' .or. hcloud == 'KHKO' .or. hcloud == 'KESS' .or. hcloud == 'REVE'
  tzsource%cmnhname  = 'COND'
  tzsource%clongname = 'vapor condensation or cloud water evaporation'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, ncondrv )

  gcond = hcloud(1:3) == 'ICE' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'CORR'
  tzsource%clongname = 'correction'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, ncorrrv )

  gcond = hcloud(1:3) == 'ICE' .and. ( .not. lred .or. ( lred .and. ladj_after ) .or. celec /= 'NONE' )
  tzsource%cmnhname  = 'CDEPI'
  tzsource%clongname = 'deposition on ice'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, ncdepirv )

  gcond = hcloud == 'LIMA' .and. lptsplit
  tzsource%cmnhname  = 'CORR2'
  tzsource%clongname = 'supplementary correction inside LIMA splitting'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, ncorr2rv )

  gcond = (      hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4'   &
            .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' ) &
          .and. celec == 'NONE'
  tzsource%cmnhname  = 'NECON'
  tzsource%clongname = 'negative correction induced by condensation'
  call Budget_source_add( tbudgets(NBUDGET_RV), tzsource, gcond, nneconrv )
end if

! Budget of RRC
tbudgets(NBUDGET_RC)%lenabled = lbu_rrc .and. krr >= 2

if ( tbudgets(NBUDGET_RC)%lenabled ) then
  if ( hcloud(1:3) == 'ICE' .and. lred .and. lsedic_ice .and. ldeposc_ice ) &
    call Print_msg( NVERB_WARNING, 'BUD', 'Ini_budget', 'lred=T + lsedic=T + ldeposc=T:'// &
                                                        'DEPO and SEDI source terms are mixed and stored in SEDI' )

  tbudgets(NBUDGET_RC)%trhodj => tburhodj

  !Allocate all basic source terms (used or not)
  !The size should be large enough (bigger than necessary is OK)
  isourcesmax = 43
  tbudgets(NBUDGET_RC)%nsourcesmax = isourcesmax
  allocate( tbudgets(NBUDGET_RC)%tsources(isourcesmax) )

  allocate( tbudgets(NBUDGET_RC)%xtmpstore(ibudim1, ibudim2, ibudim3) )

  tbudgets(NBUDGET_RC)%tsources(:)%ngroup = 0

  tzsource%ccomment = 'Budget of cloud water mixing ratio'
  tzsource%ngrid    = 1

  tzsource%cunits   = 'kg kg-1'

  gcond = .true.
  tzsource%cmnhname  = 'INIF'
  tzsource%clongname = 'initial state'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'ENDF'
  tzsource%clongname = 'final state'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'AVEF'
  tzsource%clongname = 'averaged state'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .false. )

  tzsource%cunits   = 's-1'

  gcond = .true.
  tzsource%cmnhname  = 'ASSE'
  tzsource%clongname = 'time filter (Asselin)'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nasserc )

  gcond = nmodel > 1
  tzsource%cmnhname  = 'NEST'
  tzsource%clongname = 'nesting'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nnestrc )

  gcond = lforcing
  tzsource%cmnhname  = 'FRC'
  tzsource%clongname = 'forcing'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nfrcrc )

  gcond = onumdifth
  tzsource%cmnhname  = 'DIF'
  tzsource%clongname = 'numerical diffusion'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, ndifrc )

  gcond = ohorelax_rc
  tzsource%cmnhname  = 'REL'
  tzsource%clongname = 'relaxation'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nrelrc )

  gcond = hdconv == 'KAFR' .OR. hsconv == 'KAFR'
  tzsource%cmnhname  = 'DCONV'
  tzsource%clongname = 'KAFR convection'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, ndconvrc )

  gcond = odragtree .and. odepotree
  tzsource%cmnhname  = 'DEPOTR'
  tzsource%clongname = 'tree droplet deposition'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, ndepotrrc )

  gcond = hturb == 'TKEL'
  tzsource%cmnhname  = 'VTURB'
  tzsource%clongname = 'vertical turbulent diffusion'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nvturbrc )

  gcond = hturb == 'TKEL' .and. HTURBDIM == '3DIM'
  tzsource%cmnhname  = 'HTURB'
  tzsource%clongname = 'horizontal turbulent diffusion'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nhturbrc )

  gcond = hturb == 'TKEL' .and. (      hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
                                  .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' )
  tzsource%cmnhname  = 'NETUR'
  tzsource%clongname = 'negative correction induced by turbulence'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nneturrc )

  gcond = lvisc .and. lvisc_r
  tzsource%cmnhname  = 'VISC'
  tzsource%clongname = 'viscosity'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nviscrc )

  gcond = .true.
  tzsource%cmnhname  = 'ADV'
  tzsource%clongname = 'total advection'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nadvrc )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEADV'
  tzsource%clongname = 'negative correction induced by advection'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nneadvrc )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEGA'
  tzsource%clongname = 'negative correction'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nnegarc )

  gcond =       ( hcloud      == 'LIMA' .and. lptsplit .and. lwarm_lima .and. lrain_lima ) &
           .or. ( hcloud(1:3) == 'ICE' .and. lred .and. celec == 'NONE' )
  tzsource%cmnhname  = 'CORR'
  tzsource%clongname = 'correction'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, ncorrrc )

  gcond =    ( hcloud      == 'LIMA' .and. lwarm_lima .and. lsedc_lima ) &
        .or. ( hcloud(1:3) == 'ICE'  .and. lsedic_ice )                  &
        .or. ( hcloud      == 'C2R2' .and. lsedc_c2r2 )                  &
        .or. ( hcloud      == 'KHKO' .and. lsedc_c2r2 )
  tzsource%cmnhname  = 'SEDI'
  tzsource%clongname = 'sedimentation of cloud'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nsedirc )

  gcond =      ( hcloud == 'LIMA' .and. lwarm_lima .and. ldepoc_lima ) &
          .or. ( hcloud      == 'C2R2' .and. ldepoc_c2r2 )             &
          .or. ( hcloud      == 'KHKO' .and. ldepoc_c2r2 )             &
          .or. ( hcloud(1:3) == 'ICE'  .and. ldeposc_ice .and. celec == 'NONE' )
  tzsource%cmnhname  = 'DEPO'
  tzsource%clongname = 'surface droplet deposition'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, ndeporc )

  gcond = hcloud == 'LIMA' .and. lptsplit .and. lwarm_lima .and. lrain_lima
  tzsource%cmnhname  = 'R2C1'
  tzsource%clongname = 'rain to cloud change after sedimentation'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nr2c1rc )

  gtmp = cactccn == 'ABRK' .and. (lorilam .or. ldust .or. lsalt )
  gcond =      ( hcloud      == 'LIMA' .and. lwarm_lima .and. lacti_lima .and. nmod_ccn >= 1 )     &
          .or. ( hcloud      == 'C2R2' .and. ( gtmp .or. .not.lsupsat_c2r2 ) ) &
          .or. ( hcloud      == 'KHKO' .and. ( gtmp .or. .not.lsupsat_c2r2 ) )
  tzsource%cmnhname  = 'HENU'
  tzsource%clongname = 'CCN activation'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nhenurc )

  gcond = hcloud == 'LIMA' .and. lcold_lima .and. lnucl_lima
  tzsource%cmnhname  = 'HINC'
  tzsource%clongname = 'heterogeneous nucleation by contact'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nhincrc )

  gcond = hcloud(1:3) == 'ICE' .and. lred .and. ladj_before .and. celec == 'NONE'
  tzsource%cmnhname  = 'ADJU'
  tzsource%clongname = 'adjustment to saturation'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nadjurc )

  gcond = hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'HON'
  tzsource%clongname = 'homogeneous nucleation'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nhonrc )

  gcond =       ( hcloud      == 'LIMA' .and. ( lptsplit .or. ( lwarm_lima .and. lrain_lima ) ) ) &
           .or.   hcloud      == 'KESS'                                                           &
           .or. ( hcloud(1:3) == 'ICE'  .and. lwarm_ice )                                         &
           .or. ( hcloud      == 'C2R2' .and. lrain_c2r2 )                                        &
           .or. ( hcloud      == 'KHKO' .and. lrain_c2r2 )
  tzsource%cmnhname  = 'AUTO'
  tzsource%clongname = 'autoconversion into rain'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nautorc )

  gcond =       ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lwarm_lima .and. lrain_lima ) ) ) &
           .or.   hcloud      == 'KESS'                                                      &
           .or. ( hcloud(1:3) == 'ICE'  .and. lwarm_ice )                                    &
           .or. ( hcloud      == 'C2R2' .and. lrain_c2r2 )                                   &
           .or. ( hcloud      == 'KHKO' .and. lrain_c2r2 )
  tzsource%cmnhname  = 'ACCR'
  tzsource%clongname = 'accretion'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, naccrrc )

  gcond =  hcloud == 'LIMA' .and. ( lptsplit .or. ( lwarm_lima .and. lrain_lima ) )
  tzsource%cmnhname  = 'REVA'
  tzsource%clongname = 'rain evaporation'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nrevarc )

  gcond = hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lnucl_lima ) )
  tzsource%cmnhname  = 'HONC'
  tzsource%clongname = 'droplet homogeneous freezing'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nhoncrc )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'IMLT'
  tzsource%clongname = 'ice melting'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nimltrc )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'BERFI'
  tzsource%clongname = 'Bergeron-Findeisen'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nberfirc )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'RIM'
  tzsource%clongname = 'riming of cloud water'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nrimrc )

  gcond = hcloud(1:3) == 'ICE' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'CMEL'
  tzsource%clongname = 'collection by snow and conversion into rain with T>XTT on ice'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, ncmelrc )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'WETG'
  tzsource%clongname = 'wet growth of graupel'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nwetgrc )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'DRYG'
  tzsource%clongname = 'dry growth of graupel'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, ndrygrc )

  gcond = hcloud == 'LIMA' .and. lptsplit
  tzsource%cmnhname  = 'CVRC'
  tzsource%clongname = 'rain to cloud change after other microphysical processes'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, ncvrcrc )

  gcond =      ( hcloud == 'LIMA' .and. .not.lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima .and. lsnow_lima ) &
          .or.   hcloud == 'ICE4'
  tzsource%cmnhname  = 'WETH'
  tzsource%clongname = 'wet growth of hail'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nwethrc )

  gcond = hcloud == 'ICE4' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'DRYH'
  tzsource%clongname = 'dry growth of hail'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, ndryhrc )

  gcond = hcloud == 'LIMA'
  tzsource%cmnhname  = 'CEDS'
  tzsource%clongname = 'adjustment to saturation'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, ncedsrc )

  gcond = hcloud(1:3) == 'ICE' .and. ( .not. lred .or. ( lred .and. ladj_after ) .or. celec /= 'NONE' )
  tzsource%cmnhname  = 'CDEPI'
  tzsource%clongname = 'condensation/deposition on ice'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, ncdepirc )

  gcond = hcloud == 'C2R2' .or. hcloud == 'KHKO' .or. hcloud == 'KESS' .or. hcloud == 'REVE'
  tzsource%cmnhname  = 'COND'
  tzsource%clongname = 'vapor condensation or cloud water evaporation'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, ncondrc )

  gcond = hcloud == 'LIMA' .and. lptsplit
  tzsource%cmnhname  = 'CORR2'
  tzsource%clongname = 'supplementary correction inside LIMA splitting'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, 1 )

  gcond = (      hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4'   &
            .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' ) &
          .and. celec == 'NONE'
  tzsource%cmnhname  = 'NECON'
  tzsource%clongname = 'negative correction induced by condensation'
  call Budget_source_add( tbudgets(NBUDGET_RC), tzsource, gcond, nneconrc )
end if

! Budget of RRR
tbudgets(NBUDGET_RR)%lenabled = lbu_rrr .and. krr >= 3

if ( tbudgets(NBUDGET_RR)%lenabled ) then
  tbudgets(NBUDGET_RR)%trhodj => tburhodj

  !Allocate all basic source terms (used or not)
  !The size should be large enough (bigger than necessary is OK)
  isourcesmax = 33
  tbudgets(NBUDGET_RR)%nsourcesmax = isourcesmax
  allocate( tbudgets(NBUDGET_RR)%tsources(isourcesmax) )

  allocate( tbudgets(NBUDGET_RR)%xtmpstore(ibudim1, ibudim2, ibudim3) )

  tbudgets(NBUDGET_RR)%tsources(:)%ngroup = 0

  tzsource%ccomment = 'Budget of rain water mixing ratio'
  tzsource%ngrid    = 1

  tzsource%cunits   = 'kg kg-1'

  gcond = .true.
  tzsource%cmnhname  = 'INIF'
  tzsource%clongname = 'initial state'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'ENDF'
  tzsource%clongname = 'final state'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'AVEF'
  tzsource%clongname = 'averaged state'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .false. )

  tzsource%cunits   = 's-1'

  gcond = .true.
  tzsource%cmnhname  = 'ASSE'
  tzsource%clongname = 'time filter (Asselin)'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nasserr )

  gcond = nmodel > 1
  tzsource%cmnhname  = 'NEST'
  tzsource%clongname = 'nesting'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nnestrr )

  gcond = lforcing
  tzsource%cmnhname  = 'FRC'
  tzsource%clongname = 'forcing'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nfrcrr )

  gcond = onumdifth
  tzsource%cmnhname  = 'DIF'
  tzsource%clongname = 'numerical diffusion'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, ndifrr )

  gcond = ohorelax_rr
  tzsource%cmnhname  = 'REL'
  tzsource%clongname = 'relaxation'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nrelrr )

  gcond = hturb == 'TKEL' .and. ( hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' )
  tzsource%cmnhname  = 'NETUR'
  tzsource%clongname = 'negative correction induced by turbulence'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nneturrr )

  gcond = lvisc .and. lvisc_r
  tzsource%cmnhname  = 'VISC'
  tzsource%clongname = 'viscosity'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nviscrr )

  gcond = .true.
  tzsource%cmnhname  = 'ADV'
  tzsource%clongname = 'total advection'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nadvrr )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEADV'
  tzsource%clongname = 'negative correction induced by advection'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nneadvrr )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEGA'
  tzsource%clongname = 'negative correction'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nnegarr )

  gcond =       ( hcloud      == 'LIMA' .and. lptsplit .and. lwarm_lima .and. lrain_lima ) &
           .or. ( hcloud(1:3) == 'ICE' .and. lred .and. celec == 'NONE' )
  tzsource%cmnhname  = 'CORR'
  tzsource%clongname = 'correction'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, ncorrrr )

  gcond =      ( hcloud      == 'LIMA' .and. lwarm_lima .and. lrain_lima ) &
          .or.   hcloud      == 'KESS'                                     &
          .or.   hcloud(1:3) == 'ICE'                                      &
          .or.   hcloud      == 'C2R2'                                     &
          .or.   hcloud      == 'KHKO'
  tzsource%cmnhname  = 'SEDI'
  tzsource%clongname = 'sedimentation of rain drops'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nsedirr )

  gcond = hcloud == 'LIMA' .and. lptsplit .and. lwarm_lima .and. lrain_lima
  tzsource%cmnhname  = 'R2C1'
  tzsource%clongname = 'rain to cloud after sedimentation'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nr2c1rr )

  gcond =       ( hcloud      == 'LIMA' .and. ( lptsplit .or. ( lwarm_lima .and. lrain_lima ) ) ) &
           .or.   hcloud      == 'KESS'                                                           &
           .or. ( hcloud(1:3) == 'ICE'  .and. lwarm_ice )                                         &
           .or. ( hcloud      == 'C2R2' .and. lrain_c2r2 )                                        &
           .or. ( hcloud      == 'KHKO' .and. lrain_c2r2 )
  tzsource%cmnhname  = 'AUTO'
  tzsource%clongname = 'autoconversion into rain drops'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nautorr )

  gcond =       ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lwarm_lima .and. lrain_lima ) ) ) &
           .or.   hcloud      == 'KESS'                                                      &
           .or. ( hcloud(1:3) == 'ICE'  .and. lwarm_ice )                                    &
           .or. ( hcloud      == 'C2R2' .and. lrain_c2r2 )                                   &
           .or. ( hcloud      == 'KHKO' .and. lrain_c2r2 )
  tzsource%cmnhname  = 'ACCR'
  tzsource%clongname = 'accretion of cloud droplets'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, naccrrr )

  gcond =      ( hcloud      == 'LIMA' .and. ( lptsplit .or. ( lwarm_lima .and. lrain_lima ) ) ) &
          .or.   hcloud      == 'KESS'                                                           &
          .or. ( hcloud(1:3) == 'ICE'  .and. lwarm_ice )                                         &
          .or. ( hcloud      == 'C2R2' .and. lrain_c2r2 )                                        &
          .or. ( hcloud      == 'KHKO' .and. lrain_c2r2 )
  tzsource%cmnhname  = 'REVA'
  tzsource%clongname = 'rain evaporation'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nrevarr )

  gcond = hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lnucl_lima .and. lrain_lima ) )
  tzsource%cmnhname  = 'HONR'
  tzsource%clongname = 'rain homogeneous freezing'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nhonrrr )


  gcond =      ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima .and. lrain_lima) ) ) &
          .or.   hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'ACC'
  tzsource%clongname = 'accretion of rain water on aggregates'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, naccrr )

  gcond = hcloud(1:3) == 'ICE' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'CMEL'
  tzsource%clongname = 'collection of droplets by snow and conversion into rain'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, ncmelrr )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'CFRZ'
  tzsource%clongname = 'conversion freezing of rain drops'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, ncfrzrr )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'WETG'
  tzsource%clongname = 'wet growth of graupel'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nwetgrr )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'DRYG'
  tzsource%clongname = 'dry growth of graupel'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, ndrygrr )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'GMLT'
  tzsource%clongname = 'graupel melting'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, ngmltrr )

  gcond = hcloud == 'LIMA' .and. lptsplit
  tzsource%cmnhname  = 'CVRC'
  tzsource%clongname = 'rain to cloud change after other microphysical processes'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, ncvrcrr )

  gcond =      ( hcloud == 'LIMA' .and. .not.lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima .and. lsnow_lima ) &
          .or.   hcloud == 'ICE4'
  tzsource%cmnhname  = 'WETH'
  tzsource%clongname = 'wet growth of hail'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nwethrr )

  gcond = hcloud == 'ICE4' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'DRYH'
  tzsource%clongname = 'dry growth of hail'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, ndryhrr )

  gcond =      ( hcloud == 'LIMA' .and. .not.lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima .and. lsnow_lima ) &
          .or.   hcloud == 'ICE4'
  tzsource%cmnhname  = 'HMLT'
  tzsource%clongname = 'melting of hail'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nhmltrr )

  gcond = hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'SFR'
  tzsource%clongname = 'spontaneous freezing'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nsfrrr )

  gcond = hcloud == 'LIMA' .and. lptsplit
  tzsource%cmnhname  = 'CORR2'
  tzsource%clongname = 'supplementary correction inside LIMA splitting'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, ncorr2rr )

  gcond = (      hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4'   &
            .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' ) &
          .and. celec == 'NONE'
  tzsource%cmnhname  = 'NECON'
  tzsource%clongname = 'negative correction induced by condensation'
  call Budget_source_add( tbudgets(NBUDGET_RR), tzsource, gcond, nneconrr )
end if

! Budget of RRI
tbudgets(NBUDGET_RI)%lenabled = lbu_rri .and. krr >= 4

if ( tbudgets(NBUDGET_RI)%lenabled ) then
  tbudgets(NBUDGET_RI)%trhodj => tburhodj

  !Allocate all basic source terms (used or not)
  !The size should be large enough (bigger than necessary is OK)
  isourcesmax = 42
  tbudgets(NBUDGET_RI)%nsourcesmax = isourcesmax
  allocate( tbudgets(NBUDGET_RI)%tsources(isourcesmax) )

  allocate( tbudgets(NBUDGET_RI)%xtmpstore(ibudim1, ibudim2, ibudim3) )

  tbudgets(NBUDGET_RI)%tsources(:)%ngroup = 0

  tzsource%ccomment = 'Budget of cloud ice mixing ratio'
  tzsource%ngrid    = 1

  tzsource%cunits   = 'kg kg-1'

  gcond = .true.
  tzsource%cmnhname  = 'INIF'
  tzsource%clongname = 'initial state'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'ENDF'
  tzsource%clongname = 'final state'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'AVEF'
  tzsource%clongname = 'averaged state'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .false. )

  tzsource%cunits   = 's-1'

  gcond = .true.
  tzsource%cmnhname  = 'ASSE'
  tzsource%clongname = 'time filter (Asselin)'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nasseri )

  gcond = nmodel > 1
  tzsource%cmnhname  = 'NEST'
  tzsource%clongname = 'nesting'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nnestri )

  gcond = lforcing
  tzsource%cmnhname  = 'FRC'
  tzsource%clongname = 'forcing'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nfrcri )

  gcond = onumdifth
  tzsource%cmnhname  = 'DIF'
  tzsource%clongname = 'numerical diffusion'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, ndifri )

  gcond = ohorelax_ri
  tzsource%cmnhname  = 'REL'
  tzsource%clongname = 'relaxation'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nrelri )

  gcond = hdconv == 'KAFR' .OR. hsconv == 'KAFR'
  tzsource%cmnhname  = 'DCONV'
  tzsource%clongname = 'KAFR convection'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, ndconvri )

  gcond = hturb == 'TKEL'
  tzsource%cmnhname  = 'VTURB'
  tzsource%clongname = 'vertical turbulent diffusion'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nvturbri )

  gcond = hturb == 'TKEL' .and. HTURBDIM == '3DIM'
  tzsource%cmnhname  = 'HTURB'
  tzsource%clongname = 'horizontal turbulent diffusion'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nhturbri )

  gcond = hturb == 'TKEL' .and. (      hcloud == 'ICE3' .or. hcloud == 'ICE4' .or. hcloud == 'LIMA' )
  tzsource%cmnhname  = 'NETUR'
  tzsource%clongname = 'negative correction induced by turbulence'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nneturri )

  gcond = lvisc .and. lvisc_r
  tzsource%cmnhname  = 'VISC'
  tzsource%clongname = 'viscosity'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nviscri )

  gcond = .true.
  tzsource%cmnhname  = 'ADV'
  tzsource%clongname = 'total advection'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nadvri )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEADV'
  tzsource%clongname = 'negative correction induced by advection'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nneadvri )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEGA'
  tzsource%clongname = 'negative correction'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nnegari )

  gcond =       ( hcloud      == 'LIMA' .and. lptsplit .and. lcold_lima .and. lsnow_lima ) &
           .or. ( hcloud(1:3) == 'ICE' .and. lred .and. celec == 'NONE' )
  tzsource%cmnhname  = 'CORR'
  tzsource%clongname = 'correction'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, ncorrri )

  gcond = hcloud(1:3) == 'ICE' .and. lred .and. ladj_before .and. celec == 'NONE'
  tzsource%cmnhname  = 'ADJU'
  tzsource%clongname = 'adjustment before on ice'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nadjuri )

  gcond =      ( hcloud      == 'LIMA' .and. lcold_lima .and. lsedi_lima ) &
          .or.   hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'SEDI'
  tzsource%clongname = 'sedimentation of rain drops'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nsediri )

  gcond =  hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'HENU'
  tzsource%clongname = 'heterogeneous nucleation'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nhenuri )

  gcond = hcloud == 'LIMA' .and. lcold_lima .and. lnucl_lima
  tzsource%cmnhname  = 'HIND'
  tzsource%clongname = 'heterogeneous nucleation by deposition'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nhindri )

  gcond = hcloud == 'LIMA' .and. lcold_lima .and. lnucl_lima
  tzsource%cmnhname  = 'HINC'
  tzsource%clongname = 'heterogeneous nucleation by contact'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nhincri )

  gcond = hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'HON'
  tzsource%clongname = 'homogeneous nucleation'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nhonri )

  gcond = hcloud == 'LIMA' .and. lcold_lima .and. lnucl_lima .and. lhhoni_lima .and. nmod_ccn >= 1
  tzsource%cmnhname  = 'HONH'
  tzsource%clongname = 'haze homogeneous nucleation'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nhonhri )

  gcond = hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lnucl_lima ) )
  tzsource%cmnhname  = 'HONC'
  tzsource%clongname = 'droplet homogeneous nucleation'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nhoncri )

  gcond = hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lsnow_lima ) )
  tzsource%cmnhname  = 'CNVI'
  tzsource%clongname = 'conversion of snow to cloud ice'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, ncnviri )

  gcond = hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lsnow_lima ) )
  tzsource%cmnhname  = 'CNVS'
  tzsource%clongname = 'conversion of pristine ice to snow'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, ncnvsri )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'AGGS'
  tzsource%clongname = 'aggregation of snow'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, naggsri )

  gcond = hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'AUTS'
  tzsource%clongname = 'autoconversion of ice'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nautsri )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'IMLT'
  tzsource%clongname = 'ice melting'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nimltri )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'BERFI'
  tzsource%clongname = 'Bergeron-Findeisen'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nberfiri )

  gcond = hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) )
  tzsource%cmnhname  = 'HMS'
  tzsource%clongname = 'Hallett-Mossop ice multiplication process due to snow riming'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nhmsri )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'CFRZ'
  tzsource%clongname = 'conversion freezing of rain drops'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, ncfrzri )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'WETG'
  tzsource%clongname = 'wet growth of graupel'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nwetgri )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'DRYG'
  tzsource%clongname = 'dry growth of graupel'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, ndrygri )

  gcond = hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) )
  tzsource%cmnhname  = 'HMG'
  tzsource%clongname = 'Hallett-Mossop ice multiplication process due to graupel riming'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nhmgri )

  gcond =      ( hcloud == 'LIMA' .and. .not.lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima .and. lsnow_lima ) &
          .or.   hcloud == 'ICE4'
  tzsource%cmnhname  = 'WETH'
  tzsource%clongname = 'wet growth of hail'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nwethri )

  gcond = hcloud == 'ICE4' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'DRYH'
  tzsource%clongname = 'dry growth of hail'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, ndryhri )

  gcond = hcloud == 'LIMA'
  tzsource%cmnhname  = 'CEDS'
  tzsource%clongname = 'adjustment to saturation'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, ncedsri )

  gcond = hcloud(1:3) == 'ICE' .and. ( .not. lred .or. ( lred .and. ladj_after ) .or. celec /= 'NONE' )
  tzsource%cmnhname  = 'CDEPI'
  tzsource%clongname = 'condensation/deposition on ice'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, ncdepiri )

  gcond = hcloud == 'LIMA' .and. lptsplit
  tzsource%cmnhname  = 'CORR2'
  tzsource%clongname = 'supplementary correction inside LIMA splitting'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, ncorr2ri )

  gcond = (      hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4'   &
            .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' ) &
          .and. celec == 'NONE'
  tzsource%cmnhname  = 'NECON'
  tzsource%clongname = 'negative correction induced by condensation'
  call Budget_source_add( tbudgets(NBUDGET_RI), tzsource, gcond, nneconri )
end if

! Budget of RRS
tbudgets(NBUDGET_RS)%lenabled = lbu_rrs .and. krr >= 5

if ( tbudgets(NBUDGET_RS)%lenabled ) then
  tbudgets(NBUDGET_RS)%trhodj => tburhodj

  !Allocate all basic source terms (used or not)
  !The size should be large enough (bigger than necessary is OK)
  isourcesmax = 28
  tbudgets(NBUDGET_RS)%nsourcesmax = isourcesmax
  allocate( tbudgets(NBUDGET_RS)%tsources(isourcesmax) )

  allocate( tbudgets(NBUDGET_RS)%xtmpstore(ibudim1, ibudim2, ibudim3) )

  tbudgets(NBUDGET_RS)%tsources(:)%ngroup = 0

  tzsource%ccomment = 'Budget of snow/aggregate mixing ratio'
  tzsource%ngrid    = 1

  tzsource%cunits   = 'kg kg-1'

  gcond = .true.
  tzsource%cmnhname  = 'INIF'
  tzsource%clongname = 'initial state'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'ENDF'
  tzsource%clongname = 'final state'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'AVEF'
  tzsource%clongname = 'averaged state'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .false. )

  tzsource%cunits   = 's-1'

  gcond = .true.
  tzsource%cmnhname  = 'ASSE'
  tzsource%clongname = 'time filter (Asselin)'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nassers )

  gcond = nmodel > 1
  tzsource%cmnhname  = 'NEST'
  tzsource%clongname = 'nesting'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nnestrs )

  gcond = lforcing
  tzsource%cmnhname  = 'FRC'
  tzsource%clongname = 'forcing'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nfrcrs )

  gcond = onumdifth
  tzsource%cmnhname  = 'DIF'
  tzsource%clongname = 'numerical diffusion'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, ndifrs )

  gcond = ohorelax_rs
  tzsource%cmnhname  = 'REL'
  tzsource%clongname = 'relaxation'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nrelrs )

!   gcond = hturb == 'TKEL' .and. (      hcloud == 'ICE3' .or. hcloud == 'ICE4' &
!                                   .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' )
!   tzsource%cmnhname  = 'NETUR'
!   tzsource%clongname = 'negative correction induced by turbulence'
!   call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nneturrs )

  gcond = lvisc .and. lvisc_r
  tzsource%cmnhname  = 'VISC'
  tzsource%clongname = 'viscosity'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nviscrs )

  gcond = .true.
  tzsource%cmnhname  = 'ADV'
  tzsource%clongname = 'total advection'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nadvrs )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEADV'
  tzsource%clongname = 'negative correction induced by advection'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nneadvrs )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEGA'
  tzsource%clongname = 'negative correction'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nnegars )

  gcond =       ( hcloud      == 'LIMA' .and. lptsplit .and. lcold_lima .and. lsnow_lima ) &
           .or. ( hcloud(1:3) == 'ICE' .and. lred .and. celec == 'NONE' )
  tzsource%cmnhname  = 'CORR'
  tzsource%clongname = 'correction'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, ncorrrs )

  gcond =      ( hcloud      == 'LIMA' .and. lcold_lima .and. lsnow_lima ) &
          .or.   hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'SEDI'
  tzsource%clongname = 'sedimentation'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nsedirs )

  gcond = hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lsnow_lima ) )
  tzsource%cmnhname  = 'CNVI'
  tzsource%clongname = 'conversion of snow to cloud ice'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, ncnvirs )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'DEPS'
  tzsource%clongname = 'deposition on snow'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, ndepsrs )

  gcond = hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lsnow_lima ) )
  tzsource%cmnhname  = 'CNVS'
  tzsource%clongname = 'conversion of pristine ice to snow'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, ncnvsrs )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'AGGS'
  tzsource%clongname = 'aggregation of snow'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, naggsrs )

  gcond = hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'AUTS'
  tzsource%clongname = 'autoconversion of ice'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nautsrs )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'RIM'
  tzsource%clongname = 'riming of cloud water'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nrimrs )

  gcond = hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima ) )
  tzsource%cmnhname  = 'HMS'
  tzsource%clongname = 'Hallett-Mossop ice multiplication process due to snow riming'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nhmsrs )

  gcond =       ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima .and. lrain_lima) ) ) &
           .or.   hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'ACC'
  tzsource%clongname = 'accretion of rain water'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, naccrs )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'CMEL'
  tzsource%clongname = 'conversion melting'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, ncmelrs )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'WETG'
  tzsource%clongname = 'wet growth of graupel'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nwetgrs )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'DRYG'
  tzsource%clongname = 'dry growth of graupel'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, ndrygrs )

  gcond =      ( hcloud == 'LIMA' .and. .not.lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima .and. lsnow_lima ) &
          .or.   hcloud == 'ICE4'
  tzsource%cmnhname  = 'WETH'
  tzsource%clongname = 'wet growth of hail'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nwethrs )

  gcond = hcloud == 'ICE4' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'DRYH'
  tzsource%clongname = 'dry growth of hail'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, ndryhrs )

  gcond = (      hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4'   &
            .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' ) &
          .and. celec == 'NONE'
  tzsource%cmnhname  = 'NECON'
  tzsource%clongname = 'negative correction induced by condensation'
  call Budget_source_add( tbudgets(NBUDGET_RS), tzsource, gcond, nneconrs )
end if

! Budget of RRG
tbudgets(NBUDGET_RG)%lenabled = lbu_rrg .and. krr >= 6

if ( tbudgets(NBUDGET_RG)%lenabled ) then
  tbudgets(NBUDGET_RG)%trhodj => tburhodj

  !Allocate all basic source terms (used or not)
  !The size should be large enough (bigger than necessary is OK)
  isourcesmax = 31
  tbudgets(NBUDGET_RG)%nsourcesmax = isourcesmax
  allocate( tbudgets(NBUDGET_RG)%tsources(isourcesmax) )

  allocate( tbudgets(NBUDGET_RG)%xtmpstore(ibudim1, ibudim2, ibudim3) )

  tbudgets(NBUDGET_RG)%tsources(:)%ngroup = 0

  tzsource%ccomment = 'Budget of graupel mixing ratio'
  tzsource%ngrid    = 1

  tzsource%cunits   = 'kg kg-1'

  gcond = .true.
  tzsource%cmnhname  = 'INIF'
  tzsource%clongname = 'initial state'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'ENDF'
  tzsource%clongname = 'final state'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'AVEF'
  tzsource%clongname = 'averaged state'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .false. )

  tzsource%cunits   = 's-1'

  gcond = .true.
  tzsource%cmnhname  = 'ASSE'
  tzsource%clongname = 'time filter (Asselin)'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nasserg )

  gcond = nmodel > 1
  tzsource%cmnhname  = 'NEST'
  tzsource%clongname = 'nesting'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nnestrg )

  gcond = lforcing
  tzsource%cmnhname  = 'FRC'
  tzsource%clongname = 'forcing'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nfrcrg )

  gcond = onumdifth
  tzsource%cmnhname  = 'DIF'
  tzsource%clongname = 'numerical diffusion'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, ndifrg )

  gcond = ohorelax_rg
  tzsource%cmnhname  = 'REL'
  tzsource%clongname = 'relaxation'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nrelrg )

!   gcond = hturb == 'TKEL' .and. (      hcloud == 'ICE3' .or. hcloud == 'ICE4' &
!                                   .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' )
!   tzsource%cmnhname  = 'NETUR'
!   tzsource%clongname = 'negative correction induced by turbulence'
!   call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nneturrg )

  gcond = lvisc .and. lvisc_r
  tzsource%cmnhname  = 'VISC'
  tzsource%clongname = 'viscosity'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nviscrg )

  gcond = .true.
  tzsource%cmnhname  = 'ADV'
  tzsource%clongname = 'total advection'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nadvrg )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEADV'
  tzsource%clongname = 'negative correction induced by advection'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nneadvrg )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEGA'
  tzsource%clongname = 'negative correction'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nnegarg )

  gcond = hcloud(1:3) == 'ICE' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'CORR'
  tzsource%clongname = 'correction'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, ncorrrg )

  gcond =      ( hcloud      == 'LIMA' .and. lcold_lima .and. lsnow_lima ) &
          .or.   hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'SEDI'
  tzsource%clongname = 'sedimentation'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nsedirg )

  gcond = hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lnucl_lima .and. lrain_lima ) )
  tzsource%cmnhname  = 'HONR'
  tzsource%clongname = 'rain homogeneous freezing'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nhonrrg )

  gcond = hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'SFR'
  tzsource%clongname = 'spontaneous freezing'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nsfrrg )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'DEPG'
  tzsource%clongname = 'deposition on graupel'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, ndepgrg )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima .and. lsnow_lima ) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'RIM'
  tzsource%clongname = 'riming of cloud water'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nrimrg )

  gcond =      ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima .and. lrain_lima) ) ) &
          .or.   hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'ACC'
  tzsource%clongname = 'rain accretion on graupel'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, naccrg )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'CMEL'
  tzsource%clongname = 'conversion melting of snow'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, ncmelrg )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'CFRZ'
  tzsource%clongname = 'conversion freezing of rain'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, ncfrzrg )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'WETG'
  tzsource%clongname = 'wet growth of graupel'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nwetgrg )

  gcond = hcloud == 'ICE4' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'GHCV'
  tzsource%clongname = 'graupel to hail conversion'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nghcvrg )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'DRYG'
  tzsource%clongname = 'dry growth of graupel'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, ndrygrg )

  gcond = hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima ) )
  tzsource%cmnhname  = 'HMG'
  tzsource%clongname = 'Hallett-Mossop ice multiplication process due to graupel riming'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nhmgrg )

  gcond = ( hcloud == 'LIMA' .and. ( lptsplit .or. (lcold_lima .and. lwarm_lima .and. lsnow_lima) ) ) .or. hcloud(1:3) == 'ICE'
  tzsource%cmnhname  = 'GMLT'
  tzsource%clongname = 'graupel melting'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, ngmltrg )

  gcond =      ( hcloud == 'LIMA' .and. .not.lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima .and. lsnow_lima ) &
          .or.   hcloud == 'ICE4'
  tzsource%cmnhname  = 'WETH'
  tzsource%clongname = 'wet growth of hail'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nwethrg )

  gcond = hcloud == 'LIMA' .and. .not.lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima .and. lsnow_lima
  tzsource%cmnhname  = 'COHG'
  tzsource%clongname = 'conversion of hail to graupel'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, ncohgrg )

  gcond = hcloud == 'ICE4' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'HGCV'
  tzsource%clongname = 'hail to graupel conversion'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nhgcvrg )

  gcond = hcloud == 'ICE4' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'DRYH'
  tzsource%clongname = 'dry growth of hail'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, ndryhrg )

  gcond = (      hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4'   &
            .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' ) &
          .and. celec == 'NONE'
  tzsource%cmnhname  = 'NECON'
  tzsource%clongname = 'negative correction induced by condensation'
  call Budget_source_add( tbudgets(NBUDGET_RG), tzsource, gcond, nneconrg )
end if

! Budget of RRH
tbudgets(NBUDGET_RH)%lenabled = lbu_rrh .and. krr >= 7

if ( tbudgets(NBUDGET_RH)%lenabled ) then
  tbudgets(NBUDGET_RH)%trhodj => tburhodj

  !Allocate all basic source terms (used or not)
  !The size should be large enough (bigger than necessary is OK)
  isourcesmax = 22
  tbudgets(NBUDGET_RH)%nsourcesmax = isourcesmax
  allocate( tbudgets(NBUDGET_RH)%tsources(isourcesmax) )

  allocate( tbudgets(NBUDGET_RH)%xtmpstore(ibudim1, ibudim2, ibudim3) )

  tbudgets(NBUDGET_RH)%tsources(:)%ngroup = 0

  tzsource%ccomment = 'Budget of graupel mixing ratio'
  tzsource%ngrid    = 1

  tzsource%cunits   = 'kg kg-1'

  gcond = .true.
  tzsource%cmnhname  = 'INIF'
  tzsource%clongname = 'initial state'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'ENDF'
  tzsource%clongname = 'final state'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

  gcond = .true.
  tzsource%cmnhname  = 'AVEF'
  tzsource%clongname = 'averaged state'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .false. )

  tzsource%cunits   = 's-1'

  gcond = .true.
  tzsource%cmnhname  = 'ASSE'
  tzsource%clongname = 'time filter (Asselin)'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nasserh )

  gcond = nmodel > 1
  tzsource%cmnhname  = 'NEST'
  tzsource%clongname = 'nesting'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nnestrh )

  gcond = lforcing
  tzsource%cmnhname  = 'FRC'
  tzsource%clongname = 'forcing'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nfrcrh )

  gcond = onumdifth
  tzsource%cmnhname  = 'DIF'
  tzsource%clongname = 'numerical diffusion'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, ndifrh )

  gcond = ohorelax_rh
  tzsource%cmnhname  = 'REL'
  tzsource%clongname = 'relaxation'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nrelrh )

!   gcond = hturb == 'TKEL' .and. (      hcloud == 'ICE3' .or. hcloud == 'ICE4' &
!                                   .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' )
!   tzsource%cmnhname  = 'NETUR'
!   tzsource%clongname = 'negative correction induced by turbulence'
!   call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nneturrh )

  gcond = lvisc .and. lvisc_r
  tzsource%cmnhname  = 'VISC'
  tzsource%clongname = 'viscosity'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nviscrh )

  gcond = .true.
  tzsource%cmnhname  = 'ADV'
  tzsource%clongname = 'total advection'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nadvrh )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEADV'
  tzsource%clongname = 'negative correction induced by advection'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nneadvrh )

  gcond =       hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' &
           .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA'
  tzsource%cmnhname  = 'NEGA'
  tzsource%clongname = 'negative correction'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nnegarh )

  gcond =      ( hcloud == 'LIMA' .and. lcold_lima .and. lhail_lima ) &
          .or.   hcloud == 'ICE4'
  tzsource%cmnhname  = 'SEDI'
  tzsource%clongname = 'sedimentation'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nsedirh )

  gcond = hcloud == 'ICE4' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'GHCV'
  tzsource%clongname = 'graupel to hail conversion'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nghcvrh )

  gcond =      ( hcloud == 'LIMA' .and. lhail_lima .and. ( lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lsnow_lima ) ) ) &
          .or. ( hcloud == 'ICE4' .and. ( .not. lred .or. celec /= 'NONE' ) )
  tzsource%cmnhname  = 'WETG'
  tzsource%clongname = 'wet growth of graupel'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nwetgrh )

  gcond =      ( hcloud == 'LIMA' .and. .not.lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima  .and. lsnow_lima ) &
          .or.   hcloud == 'ICE4'
  tzsource%cmnhname  = 'WETH'
  tzsource%clongname = 'wet growth of hail'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nwethrh )

  gcond = hcloud == 'LIMA' .and. .not.lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima  .and. lsnow_lima
  tzsource%cmnhname  = 'COHG'
  tzsource%clongname = 'conversion from hail to graupel'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, ncohgrh )

  gcond = hcloud == 'ICE4' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'HGCV'
  tzsource%clongname = 'hail to graupel conversion'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nhgcvrh )

  gcond = hcloud == 'ICE4' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'DRYH'
  tzsource%clongname = 'dry growth of hail'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, ndryhrh )

  gcond =      ( hcloud == 'LIMA' .and. .not. lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima  .and. lsnow_lima ) &
          .or.   hcloud == 'ICE4'
  tzsource%cmnhname  = 'HMLT'
  tzsource%clongname = 'melting of hail'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nhmltrh )

  gcond = hcloud == 'ICE4' .and. lred .and. celec == 'NONE'
  tzsource%cmnhname  = 'CORR'
  tzsource%clongname = 'correction'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, ncorrrh )

  gcond = (      hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4'   &
            .or. hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' ) &
          .and. celec == 'NONE'
  tzsource%cmnhname  = 'NECON'
  tzsource%clongname = 'negative correction induced by condensation'
  call Budget_source_add( tbudgets(NBUDGET_RH), tzsource, gcond, nneconrh )
end if

! Budgets of RSV (scalar variables)

if ( ksv > 999 ) call Print_msg( NVERB_FATAL, 'BUD', 'Ini_budget', 'number of scalar variables > 999' )

SV_BUDGETS: do jsv = 1, ksv
  ibudget = NBUDGET_SV1 - 1 + jsv
  write ( ybudgetnum, '( i3.3 )' ) jsv

  tbudgets(ibudget)%lenabled = lbu_rsv

  if ( lbu_rsv ) then
    tbudgets(ibudget)%trhodj => tburhodj

    !Allocate all basic source terms (used or not)
    !The size should be large enough (bigger than necessary is OK)
    isourcesmax = 38
    tbudgets(ibudget)%nsourcesmax = isourcesmax
    allocate( tbudgets(ibudget)%tsources(isourcesmax) )

    allocate( tbudgets(ibudget)%xtmpstore(ibudim1, ibudim2, ibudim3) )

    tbudgets(ibudget)%tsources(:)%ngroup = 0

    tzsource%ccomment = 'Budget of scalar variable ' // ybudgetnum
    tzsource%ngrid    = 1

    tzsource%cunits   = '1'

    gcond = .true.
    tzsource%cmnhname  = 'INIF'
    tzsource%clongname = 'initial state'
    call Budget_source_add( tbudgets(ibudget), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

    gcond = .true.
    tzsource%cmnhname  = 'ENDF'
    tzsource%clongname = 'final state'
    call Budget_source_add( tbudgets(ibudget), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .true. )

    gcond = .true.
    tzsource%cmnhname  = 'AVEF'
    tzsource%clongname = 'averaged state'
    call Budget_source_add( tbudgets(ibudget), tzsource, gcond, 1, odonotinit = .true., ooverwrite = .false. )

    tzsource%cunits   = 's-1'

    gcond = .true.
    tzsource%cmnhname  = 'ASSE'
    tzsource%clongname = 'time filter (Asselin)'
    call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nassesv )

    gcond = nmodel > 1
    tzsource%cmnhname  = 'NEST'
    tzsource%clongname = 'nesting'
    call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nnestsv )

    gcond = lforcing
    tzsource%cmnhname  = 'FRC'
    tzsource%clongname = 'forcing'
    call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nfrcsv )

    gcond = onumdifsv
    tzsource%cmnhname  = 'DIF'
    tzsource%clongname = 'numerical diffusion'
    call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndifsv )

    gcond = ohorelax_sv( jsv ) .or. ( celec /= 'NONE' .and. lrelax2fw_ion .and. (jsv == nsv_elecbeg .or. jsv == nsv_elecend ) )
    tzsource%cmnhname  = 'REL'
    tzsource%clongname = 'relaxation'
    call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nrelsv )

    gcond = ( hdconv == 'KAFR' .or. hsconv == 'KAFR' ) .and. ochtrans
    tzsource%cmnhname  = 'DCONV'
    tzsource%clongname = 'KAFR convection'
    call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndconvsv )

    gcond = hturb == 'TKEL'
    tzsource%cmnhname  = 'VTURB'
    tzsource%clongname = 'vertical turbulent diffusion'
    call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nvturbsv )

    gcond = hturb == 'TKEL' .and. HTURBDIM == '3DIM'
    tzsource%cmnhname  = 'HTURB'
    tzsource%clongname = 'horizontal turbulent diffusion'
    call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nhturbsv )

    gcond = hsconv == 'EDKF'
    tzsource%cmnhname  = 'MAFL'
    tzsource%clongname = 'mass flux'
    call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nmaflsv )

    gcond = lvisc .and. lvisc_sv
    tzsource%cmnhname  = 'VISC'
    tzsource%clongname = 'viscosity'
    call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nviscsv )

    gcond = .true.
    tzsource%cmnhname  = 'ADV'
    tzsource%clongname = 'total advection'
    call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nadvsv )

    gcond = .true.
    tzsource%cmnhname  = 'NEGA2'
    tzsource%clongname = 'negative correction'
    call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nnega2sv )

    ! Add specific source terms to different scalar variables
    igroup = 1
    SV_VAR: if ( jsv <= nsv_user ) then
      ! nsv_user case
      ! Nothing to do

    else if ( jsv >= nsv_c2r2beg .and. jsv <= nsv_c2r2end ) then SV_VAR
      ! C2R2 or KHKO Case

      ! Source terms in common for all C2R2/KHKO budgets
      gcond = hturb == 'TKEL'
      tzsource%cmnhname  = 'NETUR'
      tzsource%clongname = 'negative correction induced by turbulence'
      call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nnetursv )

      gcond = .true.
      tzsource%cmnhname  = 'NEADV'
      tzsource%clongname = 'negative correction induced by advection'
      call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nneadvsv )

      gcond = .true.
      tzsource%cmnhname  = 'NEGA'
      tzsource%clongname = 'negative correction'
      call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nnegasv )

      gcond = .true.
      tzsource%cmnhname  = 'NECON'
      tzsource%clongname = 'negative correction induced by condensation'
      call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nneconsv )

      ! Source terms specific to each budget
      SV_C2R2: select case( jsv - nsv_c2r2beg + 1 )
        case ( 1 ) SV_C2R2
          ! Concentration of activated nuclei
          gtmp = cactccn == 'ABRK' .and. (lorilam .or. ldust .or. lsalt )
          gcond =  gtmp .or. ( .not.gtmp .and. .not.lsupsat_c2r2 )
          tzsource%cmnhname  = 'HENU'
          tzsource%clongname = 'CCN activation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

          gcond = .true.
          tzsource%cmnhname  = 'CEVA'
          tzsource%clongname = 'evaporation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )


        case ( 2 ) SV_C2R2
          ! Concentration of cloud droplets
          gcond = odragtree .and. odepotree
          tzsource%cmnhname  = 'DEPOTR'
          tzsource%clongname = 'tree droplet deposition'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndepotrsv )

          gtmp = cactccn == 'ABRK' .and. (lorilam .or. ldust .or. lsalt )
          gcond =  gtmp .or. ( .not.gtmp .and. .not.lsupsat_c2r2 )
          tzsource%cmnhname  = 'HENU'
          tzsource%clongname = 'CCN activation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

          gcond = lrain_c2r2
          tzsource%cmnhname  = 'SELF'
          tzsource%clongname = 'self-collection of cloud droplets'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

          gcond = lrain_c2r2
          tzsource%cmnhname  = 'ACCR'
          tzsource%clongname = 'accretion'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

          gcond = lsedc_c2r2
          tzsource%cmnhname  = 'SEDI'
          tzsource%clongname = 'sedimentation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

          gcond = ldepoc_c2r2
          tzsource%cmnhname  = 'DEPO'
          tzsource%clongname = 'surface droplet deposition'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

          gcond = .true.
          tzsource%cmnhname  = 'CEVA'
          tzsource%clongname = 'evaporation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )


        case ( 3 ) SV_C2R2
          ! Concentration of raindrops

          gcond = lrain_c2r2
          tzsource%cmnhname  = 'AUTO'
          tzsource%clongname = 'autoconversion into rain'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

          gcond = hcloud /= 'KHKO'
          tzsource%cmnhname  = 'SCBU'
          tzsource%clongname = 'self collection - coalescence/break-up'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

          gcond = lrain_c2r2
          tzsource%cmnhname  = 'REVA'
          tzsource%clongname = 'rain evaporation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

          gcond = lrain_c2r2
          tzsource%cmnhname  = 'BRKU'
          tzsource%clongname = 'spontaneous break-up'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

          gcond = .true.
          tzsource%cmnhname  = 'SEDI'
          tzsource%clongname = 'sedimentation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )


        case ( 4 ) SV_C2R2
          ! Supersaturation
          gcond = .true.
          tzsource%cmnhname  = 'CEVA'
          tzsource%clongname = 'evaporation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

      end select SV_C2R2


    else if ( jsv >= nsv_lima_beg .and. jsv <= nsv_lima_end ) then SV_VAR
      ! LIMA case

      ! Source terms in common for all LIMA budgets
      gcond = hturb == 'TKEL'
      tzsource%cmnhname  = 'NETUR'
      tzsource%clongname = 'negative correction induced by turbulence'
      call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nnetursv )

      gcond = .true.
      tzsource%cmnhname  = 'NEADV'
      tzsource%clongname = 'negative correction induced by advection'
      call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nneadvsv )

      gcond = .true.
      tzsource%cmnhname  = 'NEGA'
      tzsource%clongname = 'negative correction'
      call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nnegasv )

      gcond = .true.
      tzsource%cmnhname  = 'NECON'
      tzsource%clongname = 'negative correction induced by condensation'
      call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nneconsv )


      ! Source terms specific to each budget
      SV_LIMA: if ( jsv == nsv_lima_nc ) then
        ! Cloud droplets concentration
          gcond = odragtree .and. odepotree
          tzsource%cmnhname  = 'DEPOTR'
          tzsource%clongname = 'tree droplet deposition'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndepotrsv )

        gcond = lptsplit .and. lwarm_lima  .and. lrain_lima
        tzsource%cmnhname  = 'CORR'
        tzsource%clongname = 'correction'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lwarm_lima  .and. lsedc_lima
        tzsource%cmnhname  = 'SEDI'
        tzsource%clongname = 'sedimentation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lwarm_lima  .and. ldepoc_lima
        tzsource%cmnhname  = 'DEPO'
        tzsource%clongname = 'surface droplet deposition'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .and. lwarm_lima  .and. lrain_lima
        tzsource%cmnhname  = 'R2C1'
        tzsource%clongname = 'rain to cloud change after sedimentation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lwarm_lima  .and. lacti_lima .and. nmod_ccn >= 1
        tzsource%cmnhname  = 'HENU'
        tzsource%clongname = 'CCN activation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lcold_lima .and. lnucl_lima
        tzsource%cmnhname  = 'HINC'
        tzsource%clongname = 'heterogeneous nucleation by contact'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lwarm_lima  .and. lrain_lima
        tzsource%cmnhname  = 'SELF'
        tzsource%clongname = 'self-collection of cloud droplets'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lwarm_lima  .and. lrain_lima )
        tzsource%cmnhname  = 'AUTO'
        tzsource%clongname = 'autoconversion into rain'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lwarm_lima  .and. lrain_lima )
        tzsource%cmnhname  = 'ACCR'
        tzsource%clongname = 'accretion'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lwarm_lima  .and. lrain_lima )
        tzsource%cmnhname  = 'REVA'
        tzsource%clongname = 'rain evaporation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lnucl_lima )
        tzsource%cmnhname  = 'HONC'
        tzsource%clongname = 'droplet homogeneous nucleation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  )
        tzsource%cmnhname  = 'IMLT'
        tzsource%clongname = 'ice melting'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lsnow_lima )
        tzsource%cmnhname  = 'RIM'
        tzsource%clongname = 'riming of cloud water'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lsnow_lima )
        tzsource%cmnhname  = 'WETG'
        tzsource%clongname = 'wet growth of graupel'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lsnow_lima )
        tzsource%cmnhname  = 'DRYG'
        tzsource%clongname = 'dry growth of graupel'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit
        tzsource%cmnhname  = 'CVRC'
        tzsource%clongname = 'rain to cloud change after other microphysical processes'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = .not.lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima  .and. lsnow_lima
        tzsource%cmnhname  = 'WETH'
        tzsource%clongname = 'wet growth of hail'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = hcloud == 'LIMA' .and. lptsplit
        tzsource%cmnhname  = 'CORR2'
        tzsource%clongname = 'supplementary correction inside LIMA splitting'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = .true.
        tzsource%cmnhname  = 'CEDS'
        tzsource%clongname = 'adjustment to saturation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )


      else if ( jsv == nsv_lima_nr ) then SV_LIMA
        ! Rain drops concentration
        gcond = lptsplit .and. lwarm_lima  .and. lrain_lima
        tzsource%cmnhname  = 'CORR'
        tzsource%clongname = 'correction'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lwarm_lima  .and. lrain_lima
        tzsource%cmnhname  = 'SEDI'
        tzsource%clongname = 'sedimentation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .and. lwarm_lima  .and. lrain_lima
        tzsource%cmnhname  = 'R2C1'
        tzsource%clongname = 'rain to cloud change after sedimentation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. (lwarm_lima  .and. lrain_lima)
        tzsource%cmnhname  = 'AUTO'
        tzsource%clongname = 'autoconversion into rain'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. (lwarm_lima  .and. lrain_lima)
        tzsource%cmnhname  = 'SCBU'
        tzsource%clongname = 'self collection - coalescence/break-up'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. (lwarm_lima  .and. lrain_lima)
        tzsource%cmnhname  = 'REVA'
        tzsource%clongname = 'rain evaporation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. (lwarm_lima  .and. lrain_lima)
        tzsource%cmnhname  = 'BRKU'
        tzsource%clongname = 'spontaneous break-up'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lrain_lima .and. lnucl_lima )
        tzsource%cmnhname  = 'HONR'
        tzsource%clongname = 'rain homogeneous freezing'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lsnow_lima .and. lrain_lima )
        tzsource%cmnhname  = 'ACC'
        tzsource%clongname = 'accretion of rain water'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lsnow_lima )
        tzsource%cmnhname  = 'CFRZ'
        tzsource%clongname = 'conversion freezing of rain'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lsnow_lima )
        tzsource%cmnhname  = 'WETG'
        tzsource%clongname = 'wet growth of graupel'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lsnow_lima )
        tzsource%cmnhname  = 'DRYG'
        tzsource%clongname = 'dry growth of graupel'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lsnow_lima )
        tzsource%cmnhname  = 'GMLT'
        tzsource%clongname = 'graupel melting'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit
        tzsource%cmnhname  = 'CVRC'
        tzsource%clongname = 'rain to cloud change after other microphysical processes'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = .not.lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima  .and. lsnow_lima
        tzsource%cmnhname  = 'WETH'
        tzsource%clongname = 'wet growth of hail'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = .not.lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima  .and. lsnow_lima
        tzsource%cmnhname  = 'HMLT'
        tzsource%clongname = 'hail melting'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = hcloud == 'LIMA' .and. lptsplit
        tzsource%cmnhname  = 'CORR2'
        tzsource%clongname = 'supplementary correction inside LIMA splitting'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )


      else if ( jsv >= nsv_lima_ccn_free .and. jsv <= nsv_lima_ccn_free + nmod_ccn - 1 ) then SV_LIMA
        ! Free CCN concentration
        gcond = lwarm_lima  .and. lacti_lima .and. nmod_ccn >= 1
        tzsource%cmnhname  = 'HENU'
        tzsource%clongname = 'CCN activation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lcold_lima .and. lnucl_lima .and. lhhoni_lima .and. nmod_ccn >= 1
        tzsource%cmnhname  = 'HONH'
        tzsource%clongname = 'haze homogeneous nucleation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lwarm_lima
        tzsource%cmnhname  = 'CEDS'
        tzsource%clongname = 'adjustment to saturation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lscav_lima
        tzsource%cmnhname  = 'SCAV'
        tzsource%clongname = 'scavenging'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )


      else if ( jsv >= nsv_lima_ccn_acti .and. jsv <= nsv_lima_ccn_acti + nmod_ccn - 1 ) then SV_LIMA
        ! Activated CCN concentration
        gcond = lwarm_lima  .and. lacti_lima .and. nmod_ccn >= 1
        tzsource%cmnhname  = 'HENU'
        tzsource%clongname = 'CCN activation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lcold_lima .and. lnucl_lima .and. .not. lmeyers_lima
        tzsource%cmnhname  = 'HINC'
        tzsource%clongname = 'heterogeneous nucleation by contact'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lwarm_lima
        tzsource%cmnhname  = 'CEDS'
        tzsource%clongname = 'adjustment to saturation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )


      else if ( jsv == nsv_lima_scavmass ) then SV_LIMA
        ! Scavenged mass variable
        gcond = lscav_lima .and. laero_mass_lima
        tzsource%cmnhname  = 'SCAV'
        tzsource%clongname = 'scavenging'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lscav_lima .and. laero_mass_lima
        tzsource%cmnhname  = 'CEDS'
        tzsource%clongname = 'adjustment to saturation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )


      else if ( jsv == nsv_lima_ni ) then SV_LIMA
        ! Pristine ice crystals concentration
        gcond = lptsplit .and. lcold_lima .and. lsnow_lima
        tzsource%cmnhname  = 'CORR'
        tzsource%clongname = 'correction'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lcold_lima .and. lsedi_lima
        tzsource%cmnhname  = 'SEDI'
        tzsource%clongname = 'sedimentation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lcold_lima .and. lnucl_lima
        tzsource%cmnhname  = 'HIND'
        tzsource%clongname = 'heterogeneous nucleation by deposition'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lcold_lima .and. lnucl_lima
        tzsource%cmnhname  = 'HINC'
        tzsource%clongname = 'heterogeneous nucleation by contact'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lcold_lima .and. lnucl_lima .and. lhhoni_lima .and. nmod_ccn >= 1
        tzsource%cmnhname  = 'HONH'
        tzsource%clongname = 'haze homogeneous nucleation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lnucl_lima )
        tzsource%cmnhname  = 'HONC'
        tzsource%clongname = 'droplet homogeneous nucleation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lsnow_lima )
        tzsource%cmnhname  = 'CNVI'
        tzsource%clongname = 'conversion of snow to cloud ice'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lsnow_lima )
        tzsource%cmnhname  = 'CNVS'
        tzsource%clongname = 'conversion of pristine ice to snow'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lsnow_lima )
        tzsource%cmnhname  = 'AGGS'
        tzsource%clongname = 'aggregation of snow'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  )
        tzsource%cmnhname  = 'IMLT'
        tzsource%clongname = 'ice melting'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lsnow_lima )
        tzsource%cmnhname  = 'HMS'
        tzsource%clongname = 'Hallett-Mossop ice multiplication process due to snow riming'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lsnow_lima )
        tzsource%cmnhname  = 'CFRZ'
        tzsource%clongname = 'conversion freezing of rain'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lsnow_lima )
        tzsource%cmnhname  = 'WETG'
        tzsource%clongname = 'wet growth of graupel'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lsnow_lima )
        tzsource%cmnhname  = 'DRYG'
        tzsource%clongname = 'dry growth of graupel'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima  .and. lsnow_lima )
        tzsource%cmnhname  = 'HMG'
        tzsource%clongname = 'Hallett-Mossop ice multiplication process due to graupel riming'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = .not.lptsplit .and. lhail_lima .and. lcold_lima .and. lwarm_lima  .and. lsnow_lima
        tzsource%cmnhname  = 'WETH'
        tzsource%clongname = 'wet growth of hail'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = .true.
        tzsource%cmnhname  = 'CEDS'
        tzsource%clongname = 'adjustment to saturation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = hcloud == 'LIMA' .and. lptsplit
        tzsource%cmnhname  = 'CORR2'
        tzsource%clongname = 'supplementary correction inside LIMA splitting'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )


      else if ( jsv >= nsv_lima_ifn_free .and. jsv <= nsv_lima_ifn_free + nmod_ifn - 1 ) then SV_LIMA
        ! Free IFN concentration
        gcond = lcold_lima .and. lnucl_lima .and. .not. lmeyers_lima
        tzsource%cmnhname  = 'HIND'
        tzsource%clongname = 'heterogeneous nucleation by deposition'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lcold_lima
        tzsource%cmnhname  = 'CEDS'
        tzsource%clongname = 'adjustment to saturation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lscav_lima
        tzsource%cmnhname  = 'SCAV'
        tzsource%clongname = 'scavenging'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )


      else if ( jsv >= nsv_lima_ifn_nucl .and. jsv <= nsv_lima_ifn_nucl + nmod_ifn - 1 ) then SV_LIMA
        ! Nucleated IFN concentration
        gcond = lcold_lima .and. lnucl_lima                                                     &
                .and. ( ( lmeyers_lima .and. jsv == nsv_lima_ifn_nucl ) .or. .not. lmeyers_lima )
        tzsource%cmnhname  = 'HIND'
        tzsource%clongname = 'heterogeneous nucleation by deposition'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lcold_lima .and. lnucl_lima .and. lmeyers_lima .and. jsv == nsv_lima_ifn_nucl
        tzsource%cmnhname  = 'HINC'
        tzsource%clongname = 'heterogeneous nucleation by contact'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lptsplit .or. ( lcold_lima .and. lwarm_lima )
        tzsource%cmnhname  = 'IMLT'
        tzsource%clongname = 'ice melting'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lcold_lima
        tzsource%cmnhname  = 'CEDS'
        tzsource%clongname = 'adjustment to saturation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )


      else if ( jsv >= nsv_lima_imm_nucl .and. jsv <= nsv_lima_imm_nucl + nmod_imm - 1 ) then SV_LIMA
        ! Nucleated IMM concentration
        gcond = lcold_lima .and. lnucl_lima .and. .not. lmeyers_lima
        tzsource%cmnhname  = 'HINC'
        tzsource%clongname = 'heterogeneous nucleation by contact'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

        gcond = lcold_lima
        tzsource%cmnhname  = 'CEDS'
        tzsource%clongname = 'adjustment to saturation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )


      else if ( jsv == nsv_lima_hom_haze ) then SV_LIMA
        ! Homogeneous freezing of CCN
        gcond = lcold_lima .and. lnucl_lima  .and.                                                     &
                (      ( .not.lptsplit .and. ( ( lhhoni_lima .and. nmod_ccn >= 1 ) .or. lwarm_lima ) ) &
                  .or. (      lptsplit .and.   ( lhhoni_lima .and. nmod_ccn >= 1 )                 ) )
        tzsource%cmnhname  = 'HONH'
        tzsource%clongname = 'haze homogeneous nucleation'
        call Budget_source_add( tbudgets(ibudget), tzsource, gcond, igroup )

    end if SV_LIMA


    else if ( jsv >= nsv_elecbeg .and. jsv <= nsv_elecend ) then SV_VAR
      ! Electricity case
      gcond = .true.
      tzsource%cmnhname  = 'NEGA'
      tzsource%clongname = 'negative correction'
      call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nnegasv )

      SV_ELEC: select case( jsv - nsv_elecbeg + 1 )
        case ( 1 ) SV_ELEC
          ! volumetric charge of water vapor
          gcond = .true.
          tzsource%cmnhname  = 'DRIFT'
          tzsource%clongname = 'ion drift motion'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndriftqv )

          gcond = .true.
          tzsource%cmnhname  = 'CORAY'
          tzsource%clongname = 'cosmic ray source'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ncorayqv )

          gcond = .true.
          tzsource%cmnhname  = 'DEPS'
          tzsource%clongname = 'deposition on snow'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndepsqv )

          gcond = .true.
          tzsource%cmnhname  = 'DEPG'
          tzsource%clongname = 'deposition on graupel'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndepgqv )

          gcond = lwarm_ice
          tzsource%cmnhname  = 'REVA'
          tzsource%clongname = 'rain evaporation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nrevaqv )

          gcond = .true.
          tzsource%cmnhname  = 'CDEPI'
          tzsource%clongname = 'condensation/deposition on ice'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ncdepiqv )

          gcond = .true.
          tzsource%cmnhname  = 'NEUT'
          tzsource%clongname = 'NEUT'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nneutqv )


        case ( 2 ) SV_ELEC
          ! volumetric charge of cloud droplets
          gcond = .true.
          tzsource%cmnhname  = 'HON'
          tzsource%clongname = 'homogeneous nucleation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nhonqc )

          gcond = lwarm_ice
          tzsource%cmnhname  = 'AUTO'
          tzsource%clongname = 'autoconversion into rain'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nautoqc )

          gcond = lwarm_ice
          tzsource%cmnhname  = 'ACCR'
          tzsource%clongname = 'accretion'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, naccrqc )

          gcond = .true.
          tzsource%cmnhname  = 'RIM'
          tzsource%clongname = 'riming of cloud water'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nrimqc )

          gcond = .true.
          tzsource%cmnhname  = 'WETG'
          tzsource%clongname = 'wet growth of graupel'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nwetgqc )

          gcond = .true.
          tzsource%cmnhname  = 'DRYG'
          tzsource%clongname = 'dry growth of graupel'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndrygqc )

          gcond = linductive
          tzsource%cmnhname  = 'INCG'
          tzsource%clongname = 'inductive charge transfer between cloud droplets and graupel'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nincgqc )

          gcond = hcloud == 'ICE4'
          tzsource%cmnhname  = 'WETH'
          tzsource%clongname = 'wet growth of hail'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nwethqc )

          gcond = .true.
          tzsource%cmnhname  = 'IMLT'
          tzsource%clongname = 'ice melting'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nimltqc )

          gcond = .true.
          tzsource%cmnhname  = 'BERFI'
          tzsource%clongname = 'Bergeron-Findeisen'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nberfiqc )

          gcond = lsedic_ice
          tzsource%cmnhname  = 'SEDI'
          tzsource%clongname = 'sedimentation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nsediqc )

          gcond = .true.
          tzsource%cmnhname  = 'CDEPI'
          tzsource%clongname = 'condensation/deposition on ice'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ncdepiqc )

          gcond = .true.
          tzsource%cmnhname  = 'NEUT'
          tzsource%clongname = 'NEUT'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nneutqc )


        case ( 3 ) SV_ELEC
          ! volumetric charge of rain drops
          gcond = .true.
          tzsource%cmnhname  = 'SFR'
          tzsource%clongname = 'spontaneous freezing'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nsfrqr )

          gcond = lwarm_ice
          tzsource%cmnhname  = 'AUTO'
          tzsource%clongname = 'autoconversion into rain'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nautoqr )

          gcond = lwarm_ice
          tzsource%cmnhname  = 'ACCR'
          tzsource%clongname = 'accretion'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, naccrqr )

          gcond = lwarm_ice
          tzsource%cmnhname  = 'REVA'
          tzsource%clongname = 'rain evaporation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nrevaqr )

          gcond = .true.
          tzsource%cmnhname  = 'ACC'
          tzsource%clongname = 'accretion of rain water'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, naccqr )

          gcond = .true.
          tzsource%cmnhname  = 'CFRZ'
          tzsource%clongname = 'conversion freezing of rain'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ncfrzqr )

          gcond = .true.
          tzsource%cmnhname  = 'WETG'
          tzsource%clongname = 'wet growth of graupel'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nwetgqr )

          gcond = .true.
          tzsource%cmnhname  = 'DRYG'
          tzsource%clongname = 'dry growth of graupel'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndrygqr )

          gcond = .true.
          tzsource%cmnhname  = 'GMLT'
          tzsource%clongname = 'graupel melting'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ngmltqr )

          gcond = hcloud == 'ICE4'
          tzsource%cmnhname  = 'WETH'
          tzsource%clongname = 'wet growth of hail'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nwethqr )

          gcond = hcloud == 'ICE4'
          tzsource%cmnhname  = 'HMLT'
          tzsource%clongname = 'melting of hail'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nhmltqr )

          gcond = .true.
          tzsource%cmnhname  = 'SEDI'
          tzsource%clongname = 'sedimentation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nsediqr )

          gcond = .true.
          tzsource%cmnhname  = 'NEUT'
          tzsource%clongname = 'NEUT'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nneutqr )

        case ( 4 ) SV_ELEC
          ! volumetric charge of ice crystals
          gcond = .true.
          tzsource%cmnhname  = 'HON'
          tzsource%clongname = 'homogeneous nucleation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nhonqi )

          gcond = .true.
          tzsource%cmnhname  = 'AGGS'
          tzsource%clongname = 'aggregation of snow'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, naggsqi )

          gcond = .true.
          tzsource%cmnhname  = 'AUTS'
          tzsource%clongname = 'autoconversion of ice'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nautsqi )

          gcond = .true.
          tzsource%cmnhname  = 'CFRZ'
          tzsource%clongname = 'conversion freezing of rain'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ncfrzqi )

          gcond = .true.
          tzsource%cmnhname  = 'WETG'
          tzsource%clongname = 'wet growth of graupel'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nwetgqi )

          gcond = .true.
          tzsource%cmnhname  = 'DRYG'
          tzsource%clongname = 'dry growth of graupel'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndrygqi )

          gcond = hcloud == 'ICE4'
          tzsource%cmnhname  = 'WETH'
          tzsource%clongname = 'wet growth of hail'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nwethqi )

          gcond = .true.
          tzsource%cmnhname  = 'IMLT'
          tzsource%clongname = 'ice melting'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nimltqi )

          gcond = .true.
          tzsource%cmnhname  = 'BERFI'
          tzsource%clongname = 'Bergeron-Findeisen'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nberfiqi )

          gcond = .true.
          tzsource%cmnhname  = 'NIIS'
          tzsource%clongname = 'non-inductive charge separation due to ice-snow collisions'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nniisqi )

          gcond = .true.
          tzsource%cmnhname  = 'SEDI'
          tzsource%clongname = 'sedimentation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nsediqi )

          gcond = .true.
          tzsource%cmnhname  = 'CDEPI'
          tzsource%clongname = 'condensation/deposition on ice'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ncdepiqi )

          gcond = .true.
          tzsource%cmnhname  = 'NEUT'
          tzsource%clongname = 'NEUT'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nneutqi )


        case ( 5 ) SV_ELEC
          ! volumetric charge of snow
          gcond = .true.
          tzsource%cmnhname  = 'DEPS'
          tzsource%clongname = 'deposition on snow'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndepsqs )

          gcond = .true.
          tzsource%cmnhname  = 'AGGS'
          tzsource%clongname = 'aggregation of snow'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, naggsqs )

          gcond = .true.
          tzsource%cmnhname  = 'AUTS'
          tzsource%clongname = 'autoconversion of ice'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nautsqs )

          gcond = .true.
          tzsource%cmnhname  = 'RIM'
          tzsource%clongname = 'riming of cloud water'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nrimqs )

          gcond = .true.
          tzsource%cmnhname  = 'ACC'
          tzsource%clongname = 'accretion of rain water'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, naccqs )

           gcond = .true.
           tzsource%cmnhname  = 'CMEL'
           tzsource%clongname = 'conversion melting'
           call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ncmelqs )

          gcond = .true.
          tzsource%cmnhname  = 'WETG'
          tzsource%clongname = 'wet growth of graupel'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nwetgqs )

          gcond = .true.
          tzsource%cmnhname  = 'DRYG'
          tzsource%clongname = 'dry growth of graupel'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndrygqs )

          gcond = .true.
          tzsource%cmnhname  = 'NIIS'
          tzsource%clongname = 'non-inductive charge separation due to ice-snow collisions'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nniisqs )

          gcond = hcloud == 'ICE4'
          tzsource%cmnhname  = 'WETH'
          tzsource%clongname = 'wet growth of hail'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nwethqs )

          gcond = .true.
          tzsource%cmnhname  = 'SEDI'
          tzsource%clongname = 'sedimentation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nsediqs )

          gcond = .true.
          tzsource%cmnhname  = 'NEUT'
          tzsource%clongname = 'NEUT'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nneutqs )


        case ( 6 ) SV_ELEC
          ! volumetric charge of graupel
          gcond = .true.
          tzsource%cmnhname  = 'SFR'
          tzsource%clongname = 'spontaneous freezing'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nsfrqg )

          gcond = .true.
          tzsource%cmnhname  = 'DEPG'
          tzsource%clongname = 'deposition on graupel'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndepgqg )

          gcond = .true.
          tzsource%cmnhname  = 'RIM'
          tzsource%clongname = 'riming of cloud water'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nrimqg )

          gcond = .true.
          tzsource%cmnhname  = 'ACC'
          tzsource%clongname = 'accretion of rain water'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, naccqg )

           gcond = .true.
           tzsource%cmnhname  = 'CMEL'
           tzsource%clongname = 'conversion melting'
           call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ncmelqg )

          gcond = .true.
          tzsource%cmnhname  = 'CFRZ'
          tzsource%clongname = 'conversion freezing of rain'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ncfrzqg )

          gcond = .true.
          tzsource%cmnhname  = 'WETG'
          tzsource%clongname = 'wet growth of graupel'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nwetgqg )

          gcond = .true.
          tzsource%cmnhname  = 'DRYG'
          tzsource%clongname = 'dry growth of graupel'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndrygqg )

          gcond = linductive
          tzsource%cmnhname  = 'INCG'
          tzsource%clongname = 'inductive charge transfer between cloud droplets and graupel'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nincgqg )

          gcond = .true.
          tzsource%cmnhname  = 'GMLT'
          tzsource%clongname = 'graupel melting'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ngmltqg )

          gcond = hcloud == 'ICE4'
          tzsource%cmnhname  = 'WETH'
          tzsource%clongname = 'wet growth of hail'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nwethqg )

          gcond = .true.
          tzsource%cmnhname  = 'SEDI'
          tzsource%clongname = 'sedimentation'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nsediqg )

          gcond = .true.
          tzsource%cmnhname  = 'NEUT'
          tzsource%clongname = 'NEUT'
          call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nneutqg )


        case ( 7: ) SV_ELEC
          if ( ( hcloud == 'ICE4' .and. ( jsv - nsv_elecbeg + 1 ) == 7 ) ) then
            ! volumetric charge of hail
            gcond = .true.
            tzsource%cmnhname  = 'WETG'
            tzsource%clongname = 'wet growth of graupel'
            call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nwetgqh )

            gcond = .true.
            tzsource%cmnhname  = 'WETH'
            tzsource%clongname = 'wet growth of hail'
            call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nwethqh )

            gcond = .true.
            tzsource%cmnhname  = 'HMLT'
            tzsource%clongname = 'melting of hail'
            call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nhmltqh )

            gcond = .true.
            tzsource%cmnhname  = 'SEDI'
            tzsource%clongname = 'sedimentation'
            call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nsediqh )

            gcond = .true.
            tzsource%cmnhname  = 'NEUT'
            tzsource%clongname = 'NEUT'
            call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nneutqh )

          else if (      ( hcloud == 'ICE3' .and. ( jsv - nsv_elecbeg + 1 ) == 7 ) &
                    .or. ( hcloud == 'ICE4' .and. ( jsv - nsv_elecbeg + 1 ) == 8 ) ) then
            ! Negative ions (NSV_ELECEND case)
            gcond = .true.
            tzsource%cmnhname  = 'DRIFT'
            tzsource%clongname = 'ion drift motion'
            call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndriftni )

            gcond = .true.
            tzsource%cmnhname  = 'CORAY'
            tzsource%clongname = 'cosmic ray source'
            call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ncorayni )

            gcond = .true.
            tzsource%cmnhname  = 'DEPS'
            tzsource%clongname = 'deposition on snow'
            call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndepsni )

            gcond = .true.
            tzsource%cmnhname  = 'DEPG'
            tzsource%clongname = 'deposition on graupel'
            call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ndepgni )

            gcond = lwarm_ice
            tzsource%cmnhname  = 'REVA'
            tzsource%clongname = 'rain evaporation'
            call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nrevani )

            gcond = .true.
            tzsource%cmnhname  = 'CDEPI'
            tzsource%clongname = 'condensation/deposition on ice'
            call Budget_source_add( tbudgets(ibudget), tzsource, gcond, ncdepini )

            gcond = .true.
            tzsource%cmnhname  = 'NEUT'
            tzsource%clongname = 'NEUT'
            call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nneutni )

          else
            call Print_msg( NVERB_FATAL, 'BUD', 'Ini_budget', 'unknown electricity budget' )
          end if

      end select SV_ELEC


    else if ( jsv >= nsv_lgbeg .and. jsv <= nsv_lgend ) then SV_VAR
      !Lagrangian variables


    else if ( jsv >= nsv_ppbeg .and. jsv <= nsv_ppend ) then SV_VAR
      !Passive pollutants


#ifdef MNH_FOREFIRE
    else if ( jsv >= nsv_ffbeg .and. jsv <= nsv_ffend ) then SV_VAR
      !Forefire

#endif
    else if ( jsv >= nsv_csbeg .and. jsv <= nsv_csend ) then SV_VAR
      !Conditional sampling


    else if ( jsv >= nsv_chembeg .and. jsv <= nsv_chemend ) then SV_VAR
      !Chemical case
      gcond = .true.
      tzsource%cmnhname  = 'CHEM'
      tzsource%clongname = 'chemistry activity'
      call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nchemsv )

      gcond = .true.
      tzsource%cmnhname  = 'NEGA'
      tzsource%clongname = 'negative correction'
      call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nnegasv )


    else if ( jsv >= nsv_chicbeg .and. jsv <= nsv_chicend ) then SV_VAR
      !Ice phase chemistry


    else if ( jsv >= nsv_aerbeg .and. jsv <= nsv_aerend ) then SV_VAR
      !Chemical aerosol case
      gcond = lorilam
      tzsource%cmnhname  = 'NEGA'
      tzsource%clongname = 'negative correction'
      call Budget_source_add( tbudgets(ibudget), tzsource, gcond, nnegasv )

    else if ( jsv >= nsv_aerdepbeg .and. jsv <= nsv_aerdepend ) then SV_VAR
      !Aerosol wet deposition

    else if ( jsv >= nsv_dstbeg .and. jsv <= nsv_dstend ) then SV_VAR
      !Dust

    else if ( jsv >= nsv_dstdepbeg .and. jsv <= nsv_dstdepend ) then SV_VAR
      !Dust wet deposition

    else if ( jsv >= nsv_sltbeg .and. jsv <= nsv_sltend ) then SV_VAR
      !Salt

    else if ( jsv >= nsv_sltdepbeg .and. jsv <= nsv_sltdepend ) then SV_VAR
      !Salt wet deposition

    else if ( jsv >= nsv_snwbeg .and. jsv <= nsv_snwend ) then SV_VAR
      !Snow

    else if ( jsv >= nsv_lnoxbeg .and. jsv <= nsv_lnoxend ) then SV_VAR
      !LiNOX passive tracer

    else SV_VAR
      call Print_msg( NVERB_FATAL, 'BUD', 'Ini_budget', 'unknown scalar variable' )
    end if SV_VAR
  end if
end do SV_BUDGETS

IF (CBUTYPE=='CART') THEN
  WRITE(UNIT=KLUOUT, FMT= '(2/,"DESCRIPTION OF THE BUDGET BOX")' )
  WRITE(UNIT=KLUOUT, FMT= '("BUIL = ",I4.4)' ) NBUIL
  WRITE(UNIT=KLUOUT, FMT= '("BUIH = ",I4.4)' ) NBUIH
  WRITE(UNIT=KLUOUT, FMT= '("BUJL = ",I4.4)' ) NBUJL
  WRITE(UNIT=KLUOUT, FMT= '("BUJH = ",I4.4)' ) NBUJH
  WRITE(UNIT=KLUOUT, FMT= '("BUKL = ",I4.4)' ) NBUKL
  WRITE(UNIT=KLUOUT, FMT= '("BUKH = ",I4.4)' ) NBUKH
  WRITE(UNIT=KLUOUT, FMT= '("BUIMAX = ",I4.4)' ) NBUIMAX
  WRITE(UNIT=KLUOUT, FMT= '("BUJMAX = ",I4.4)' ) NBUJMAX
  WRITE(UNIT=KLUOUT, FMT= '("BUKMAX = ",I4.4)' ) NBUKMAX
END IF
IF (CBUTYPE=='MASK') THEN
  WRITE(UNIT=KLUOUT, FMT= '(2/,"DESCRIPTION OF THE BUDGET MASK")' )
  WRITE(UNIT=KLUOUT, FMT= '("BUIL = ",I4.4)' ) NBUIL
  WRITE(UNIT=KLUOUT, FMT= '("BUIH = ",I4.4)' ) NBUIH
  WRITE(UNIT=KLUOUT, FMT= '("BUJL = ",I4.4)' ) NBUJL
  WRITE(UNIT=KLUOUT, FMT= '("BUJH = ",I4.4)' ) NBUJH
  WRITE(UNIT=KLUOUT, FMT= '("BUKL = ",I4.4)' ) NBUKL
  WRITE(UNIT=KLUOUT, FMT= '("BUKH = ",I4.4)' ) NBUKH
  WRITE(UNIT=KLUOUT, FMT= '("BUKMAX = ",I4.4)' ) NBUKMAX
  WRITE(UNIT=KLUOUT, FMT= '("BUSUBWRITE = ",I4.4)' ) NBUSUBWRITE
  WRITE(UNIT=KLUOUT, FMT= '("BUMASK = ",I4.4)' ) NBUMASK
END IF

call Ini_budget_groups( tbudgets, ibudim1, ibudim2, ibudim3 )

end subroutine Ini_budget


subroutine Budget_source_add( tpbudget, tpsource, ocond, kgroupin, odonotinit, ooverwrite )
  use modd_budget, only: tbudgetdata, tbusourcedata

  use mode_msg

  type(tbudgetdata),   intent(inout) :: tpbudget
  type(tbusourcedata), intent(in)    :: tpsource ! Metadata basis
  logical,             intent(in)    :: ocond    ! Necessary condition for availability of the source term
  integer,             intent(in)    :: kgroupin ! Requested group for the source term
  logical, optional,   intent(in)    :: odonotinit
  logical, optional,   intent(in)    :: ooverwrite

  integer :: isourcenumber

  call Print_msg( NVERB_DEBUG, 'BUD', 'Budget_source_add', 'called for '//trim( tpbudget%cname )//': '//trim( tpsource%cmnhname ) )

  isourcenumber = tpbudget%nsources + 1
  if ( isourcenumber > tpbudget%nsourcesmax ) then
    call Print_msg( NVERB_FATAL, 'BUD', 'Budget_source_add', 'insufficient number of source terms' )
  else
    tpbudget%nsources = tpbudget%nsources + 1
  end if

  ! Copy metadata from provided tpsource
  ! Modifications to source term metadata done with the other dummy arguments
  tpbudget%tsources(isourcenumber) = tpsource

  if( ocond ) then
    tpbudget%tsources(isourcenumber)%ngroup = kgroupin
  else
    tpbudget%tsources(isourcenumber)%ngroup = 0
    if ( kgroupin/=0 ) call Print_msg( NVERB_WARNING, 'BUD', 'Budget_source_add', 'source term '//trim( tpbudget%cname ) &
                                       //': '//trim( tpbudget%tsources(isourcenumber)%cmnhname )//' not available' )
  end if

  if ( present( odonotinit ) ) tpbudget%tsources(isourcenumber)%ldonotinit = odonotinit

  if ( present( ooverwrite ) ) tpbudget%tsources(isourcenumber)%loverwrite = ooverwrite
end subroutine Budget_source_add


subroutine Ini_budget_groups( tpbudgets, kbudim1, kbudim2, kbudim3 )
  use modd_budget,     only: tbudgetdata
  use modd_field,      only: TYPEINT, TYPEREAL
  use modd_parameters, only: NMNHNAMELGTMAX, NSTDNAMELGTMAX

  use mode_msg
  use mode_tools,  only: Quicksort

  type(tbudgetdata), dimension(:), intent(inout) :: tpbudgets
  integer,                         intent(in)    :: kbudim1
  integer,                         intent(in)    :: kbudim2
  integer,                         intent(in)    :: kbudim3

  character(len=NMNHNAMELGTMAX)      :: ymnhname
  character(len=NSTDNAMELGTMAX)      :: ystdname
  character(len=32)                  :: ylongname
  character(len=40)                  :: yunits
  character(len=100)                 :: ycomment
  integer                            :: ji, jj, jk
  integer                            :: isources ! Number of source terms in a budget
  integer                            :: inbgroups ! Number of budget groups
  integer                            :: ival
  integer                            :: icount
  integer                            :: ivalmax, ivalmin
  integer                            :: igrid
  integer                            :: itype
  integer                            :: idims
  integer, dimension(:), allocatable :: igroups ! Temporary array to store sorted group numbers
  integer, dimension(:), allocatable :: ipos    ! Temporary array to store initial position of group numbers
  real                               :: zval
  real                               :: zvalmax, zvalmin

  call Print_msg( NVERB_DEBUG, 'BUD', 'Ini_budget_groups', 'called' )

  BUDGETS: do ji = 1, size( tpbudgets )
    ENABLED: if ( tpbudgets(ji)%lenabled ) then
      isources = size( tpbudgets(ji)%tsources )
      do jj = 1, isources
        ! Check if ngroup is an allowed value
        if ( tpbudgets(ji)%tsources(jj)%ngroup < 0 ) then
          call Print_msg( NVERB_ERROR, 'BUD', 'Ini_budget', 'negative group value is not allowed' )
          tpbudgets(ji)%tsources(jj)%ngroup = 0
        end if

        if ( tpbudgets(ji)%tsources(jj)%ngroup > 0 ) tpbudgets(ji)%tsources(jj)%lenabled = .true.
      end do

      !Count the number of groups of source terms
      !ngroup=1 is for individual entries, >1 values are groups
      allocate( igroups(isources ) )
      allocate( ipos   (isources ) )
      igroups(:) = tpbudgets(ji)%tsources(:)%ngroup
      ipos(:) = [ ( jj, jj = 1, isources ) ]

      !Sort the group list number
      call Quicksort( igroups, 1, isources, ipos )

      !Count the number of different groups
      !and renumber the entries (from 1 to inbgroups)
      inbgroups = 0
      ival = igroups(1)
      if ( igroups(1) /= 0 ) then
        inbgroups = 1
        igroups(1) = inbgroups
      end if
      do jj = 2, isources
        if ( igroups(jj) == 1 ) then
          inbgroups = inbgroups + 1
          igroups(jj) = inbgroups
        else if ( igroups(jj) > 0 ) then
          if ( igroups(jj) /= ival ) then
            ival = igroups(jj)
            inbgroups = inbgroups + 1
          end if
          igroups(jj) = inbgroups
        end if
      end do

      !Write the igroups values to the budget structure
      do jj = 1, isources
        tpbudgets(ji)%tsources(ipos(jj))%ngroup = igroups(jj)
      end do

      !Allocate the group structure + populate it
      tpbudgets(ji)%ngroups = inbgroups
      allocate( tpbudgets(ji)%tgroups(inbgroups) )

      do jj = 1, inbgroups
        !Search the list of sources for each group
        !not the most efficient algorithm but do the job
        icount = 0
        do jk = 1, isources
          if ( tpbudgets(ji)%tsources(jk)%ngroup == jj ) then
            icount = icount + 1
            ipos(icount) = jk !ipos is reused as a temporary work array
          end if
        end do
        tpbudgets(ji)%tgroups(jj)%nsources = icount

        allocate( tpbudgets(ji)%tgroups(jj)%nsourcelist(icount) )
        tpbudgets(ji)%tgroups(jj)%nsourcelist(:) = ipos(1 : icount)

        ! Set the name of the field
        ymnhname = tpbudgets(ji)%tsources(ipos(1))%cmnhname
        do jk = 2, tpbudgets(ji)%tgroups(jj)%nsources
          ymnhname = trim( ymnhname ) // '_' // trim( tpbudgets(ji)%tsources(ipos(jk))%cmnhname )
        end do
        tpbudgets(ji)%tgroups(jj)%cmnhname = ymnhname

        ! Set the standard name (CF convention)
        if ( tpbudgets(ji)%tgroups(jj)%nsources == 1 ) then
          ystdname = tpbudgets(ji)%tsources(ipos(1))%cstdname
        else
          ! The CF standard name is probably wrong if combining several source terms => set to ''
          ystdname = ''
        end if
        tpbudgets(ji)%tgroups(jj)%cstdname = ystdname

        ! Set the long name (CF convention)
        ylongname = tpbudgets(ji)%tsources(ipos(1))%clongname
        do jk = 2, tpbudgets(ji)%tgroups(jj)%nsources
          ylongname = trim( ylongname ) // ' + ' // tpbudgets(ji)%tsources(ipos(jk))%clongname
        end do
        tpbudgets(ji)%tgroups(jj)%clongname = ylongname

        ! Set the units
        yunits = tpbudgets(ji)%tsources(ipos(1))%cunits
        do jk = 2, tpbudgets(ji)%tgroups(jj)%nsources
          if ( trim( yunits ) /= trim( tpbudgets(ji)%tsources(ipos(jk))%cunits ) ) then
            call Print_msg( NVERB_WARNING, 'BUD', 'Ini_budget',                               &
                            'incompatible units for the different source terms of the group ' &
                            //trim( tpbudgets(ji)%tgroups(jj)%cmnhname ) )
            yunits = 'unknown'
          end if
        end do
        tpbudgets(ji)%tgroups(jj)%cunits = yunits

        ! Set the comment
        ! It is composed of the source comment followed by the clongnames of the different sources
        ycomment = trim( tpbudgets(ji)%tsources(ipos(1))%ccomment ) // ': '// trim( tpbudgets(ji)%tsources(ipos(1))%clongname )
        do jk = 2, tpbudgets(ji)%tgroups(jj)%nsources
          ycomment = trim( ycomment ) // ', ' // trim( tpbudgets(ji)%tsources(ipos(jk))%clongname )
        end do
        ycomment = trim( ycomment ) // ' source term'
        if ( tpbudgets(ji)%tgroups(jj)%nsources > 1 ) ycomment = trim( ycomment ) // 's'
        tpbudgets(ji)%tgroups(jj)%ccomment = ycomment

        ! Set the Arakawa grid
        igrid = tpbudgets(ji)%tsources(ipos(1))%ngrid
        do jk = 2, tpbudgets(ji)%tgroups(jj)%nsources
          if ( igrid /= tpbudgets(ji)%tsources(ipos(jk))%ngrid ) then
            call Print_msg( NVERB_WARNING, 'BUD', 'Ini_budget',                                             &
                            'different Arakawa grid positions for the different source terms of the group ' &
                            //trim( tpbudgets(ji)%tgroups(jj)%cmnhname ) )
          end if
        end do
        tpbudgets(ji)%tgroups(jj)%ngrid = igrid

        ! Set the data type
        itype = tpbudgets(ji)%tsources(ipos(1))%ntype
        do jk = 2, tpbudgets(ji)%tgroups(jj)%nsources
          if ( itype /= tpbudgets(ji)%tsources(ipos(jk))%ntype ) then
            call Print_msg( NVERB_FATAL, 'BUD', 'Ini_budget',                                             &
                            'incompatible data types for the different source terms of the group ' &
                            //trim( tpbudgets(ji)%tgroups(jj)%cmnhname ) )
          end if
        end do
        tpbudgets(ji)%tgroups(jj)%ntype = itype

        ! Set the number of dimensions
        idims = tpbudgets(ji)%tsources(ipos(1))%ndims
        do jk = 2, tpbudgets(ji)%tgroups(jj)%nsources
          if ( idims /= tpbudgets(ji)%tsources(ipos(jk))%ndims ) then
            call Print_msg( NVERB_FATAL, 'BUD', 'Ini_budget',                                             &
                            'incompatible number of dimensions for the different source terms of the group ' &
                            //trim( tpbudgets(ji)%tgroups(jj)%cmnhname ) )
          end if
        end do
        tpbudgets(ji)%tgroups(jj)%ndims = idims

        ! Set the fill values
        if ( tpbudgets(ji)%tgroups(jj)%ntype == TYPEINT ) then
          ival = tpbudgets(ji)%tsources(ipos(1))%nfillvalue
          do jk = 2, tpbudgets(ji)%tgroups(jj)%nsources
            if ( ival /= tpbudgets(ji)%tsources(ipos(jk))%nfillvalue ) then
              call Print_msg( NVERB_WARNING, 'BUD', 'Ini_budget',                                             &
                              'different (integer) fill values for the different source terms of the group ' &
                              //trim( tpbudgets(ji)%tgroups(jj)%cmnhname ) )
            end if
          end do
          tpbudgets(ji)%tgroups(jj)%nfillvalue = ival
        end if

        if ( tpbudgets(ji)%tgroups(jj)%ntype == TYPEREAL ) then
          zval = tpbudgets(ji)%tsources(ipos(1))%xfillvalue
          do jk = 2, tpbudgets(ji)%tgroups(jj)%nsources
            if ( zval /= tpbudgets(ji)%tsources(ipos(jk))%xfillvalue ) then
              call Print_msg( NVERB_WARNING, 'BUD', 'Ini_budget',                                             &
                              'different (real) fill values for the different source terms of the group ' &
                              //trim( tpbudgets(ji)%tgroups(jj)%cmnhname ) )
            end if
          end do
          tpbudgets(ji)%tgroups(jj)%xfillvalue = zval
        end if

        ! Set the valid min/max values
        ! Take the min or max of all the sources
        ! Maybe, it would be better to take the sum? (if same sign, if not already the maximum allowed value for this type)
        if ( tpbudgets(ji)%tgroups(jj)%ntype == TYPEINT ) then
          ivalmin = tpbudgets(ji)%tsources(ipos(1))%nvalidmin
          ivalmax = tpbudgets(ji)%tsources(ipos(1))%nvalidmax
          do jk = 2, tpbudgets(ji)%tgroups(jj)%nsources
            ivalmin = min( ivalmin, tpbudgets(ji)%tsources(ipos(jk))%nvalidmin )
            ivalmax = max( ivalmax, tpbudgets(ji)%tsources(ipos(jk))%nvalidmax )
          end do
          tpbudgets(ji)%tgroups(jj)%nvalidmin = ivalmin
          tpbudgets(ji)%tgroups(jj)%nvalidmax = ivalmax
        end if

        if ( tpbudgets(ji)%tgroups(jj)%ntype == TYPEREAL ) then
          zvalmin = tpbudgets(ji)%tsources(ipos(1))%xvalidmin
          zvalmax = tpbudgets(ji)%tsources(ipos(1))%xvalidmax
          do jk = 2, tpbudgets(ji)%tgroups(jj)%nsources
            zvalmin = min( zvalmin, tpbudgets(ji)%tsources(ipos(jk))%xvalidmin )
            zvalmax = max( zvalmax, tpbudgets(ji)%tsources(ipos(jk))%xvalidmax )
          end do
          tpbudgets(ji)%tgroups(jj)%xvalidmin = zvalmin
          tpbudgets(ji)%tgroups(jj)%xvalidmax = zvalmax
        end if

        allocate( tpbudgets(ji)%tgroups(jj)%xdata(kbudim1, kbudim2, kbudim3 ) )
        tpbudgets(ji)%tgroups(jj)%xdata(:, :, :) = 0.
      end do

      deallocate( igroups )
      deallocate( ipos )

      !Check that a group does not contain more than 1 source term with ldonotinit=.true.
      do jj = 1, inbgroups
        if ( tpbudgets(ji)%tgroups(jj)%nsources > 1 ) then
          do jk = 1, tpbudgets(ji)%tgroups(jj)%nsources
            if ( tpbudgets(ji)%tsources(tpbudgets(ji)%tgroups(jj)%nsourcelist(jk) )%ldonotinit ) &
              call Print_msg( NVERB_FATAL, 'BUD', 'Ini_budget', &
                              'a group with more than 1 source term may not contain sources with ldonotinit=true' )
            if ( tpbudgets(ji)%tsources(tpbudgets(ji)%tgroups(jj)%nsourcelist(jk) )%loverwrite ) &
              call Print_msg( NVERB_FATAL, 'BUD', 'Ini_budget', &
                              'a group with more than 1 source term may not contain sources with loverwrite=true' )
          end do
        end if
      end do

    end if ENABLED
  end do BUDGETS

end subroutine Ini_budget_groups

end module mode_ini_budget
