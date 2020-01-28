!MNH_LIC Copyright 1995-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
module mode_ini_budget

  implicit none

  private

  public :: Ini_budget

contains

!     #################################################################
      SUBROUTINE INI_BUDGET(KLUOUT,PTSTEP,KSV,KRR,            &
      ONUMDIFU,ONUMDIFTH,ONUMDIFSV,                                   &
      OHORELAX_UVWTH,OHORELAX_RV,OHORELAX_RC,OHORELAX_RR,             &
      OHORELAX_RI,OHORELAX_RS, OHORELAX_RG, OHORELAX_RH,OHORELAX_TKE, & 
      OHORELAX_SV,OVE_RELAX,OCHTRANS,ONUDGING,ODRAGTREE,ODEPOTREE,    &
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
!!      Module MODD_PARAMETERS: JPBUMAX,JPBUPROMAX
!!
!!      Module MODD_CONF: CCONF        
!!
!!      Module MODD_DYN:  XSEGLEN 
!!
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
!!                                LBU_MASK and XBUSURF are allocated on the extended
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
!!                   04/2016 (C.LAC) negative contribution to the budget splitted between advection, turbulence and microphysics for KHKO/C2R2
!!      C. Barthe       01/2016   Add budget for LIMA
!!      C.Lac          10/2016   Add budget for droplet deposition
!!      S. Riette        11/2016  New budgets for ICE3/ICE4
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 10/04/2019: replace ABORT and STOP calls by Print_msg
!  P. Wautelet 15/11/2019: remove unused CBURECORD variable
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------ 
!
USE MODD_PARAMETERS
USE MODD_BUDGET
USE MODD_DYN
USE MODD_CONF
USE MODD_PARAM_ICE
USE MODD_PARAM_C2R2
USE MODD_ELEC_DESCR, ONLY : LINDUCTIVE
USE MODD_2D_FRC
USE MODD_PARAM_LIMA, ONLY : OWARM=>LWARM, OCOLD=>LCOLD, OSEDI=>LSEDI,   &
                            OHHONI=>LHHONI, ORAIN=>LRAIN, OSEDC=>LSEDC, &
                            ONUCL=>LNUCL, OACTI=>LACTI, OSNOW=>LSNOW,   &
                            OHAIL=>LHAIL, OSCAV=>LSCAV, OMEYERS=>LMEYERS,&
                            ODEPOC=>LDEPOC, OPTSPLIT=>LPTSPLIT,          &
                            NMOD_CCN
!
USE MODE_ll
USE MODE_MSG
!
IMPLICIT NONE
!
!*       0.1   declarations of argument
!
!
INTEGER,         INTENT(IN) :: KLUOUT   ! Logical unit number for prints
REAL, INTENT(IN) :: PTSTEP              ! time step
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
INTEGER, DIMENSION(JPBUMAX,JPBUPROMAX+1) :: IPROACTV      ! switches set by the
                                                          ! user for process 
                                                          ! activation
INTEGER :: JI, JJ, JK , JJJ                               ! loop indices
INTEGER :: IIMAX_ll, IJMAX_ll ! size of the physical global domain
INTEGER :: IPROC                                          ! counter for processes
INTEGER :: IIU, IJU                                       ! size along x and y directions
                                                          ! of the extended subdomain
INTEGER :: IBUDIM1                                        ! first dimension of the budget arrays
                                                          ! = NBUIMAX in CART case
                                                          ! = NBUKMAX in MASK case
INTEGER :: IBUDIM2                                        ! second dimension of the budget arrays
                                                          ! = NBUJMAX in CART case
                                                          ! = NBUWRNB in MASK case
INTEGER :: IBUDIM3                                        ! third dimension of the budget arrays
                                                          ! = NBUKMAX in CART case
                                                          ! = NBUMASK in MASK case
LOGICAL :: GERROR                                         ! switch for error in
                                                          ! budget specifcation
CHARACTER(LEN=7), DIMENSION(JPBUMAX) :: YEND_COMMENT      ! last part of comment
                                                          ! for budgets records
CHARACTER(LEN=6), DIMENSION(JPBUMAX,JPBUPROMAX) :: YWORK2 ! used for 
                                                          ! concatenattion of  
                                                          ! comments for budgets
CHARACTER(LEN=40)                  :: YSTRING
INTEGER                            :: ILEN
INTEGER :: JSV               ! loop indice for the SVs
INTEGER :: IBUPROCNBR_SV_MAX ! Max number of processes for the SVs
INTEGER :: ILAST_PROC_NBR    ! Index of the last process number
INTEGER :: IINFO_ll ! return status of the interface routine
INTEGER :: IRESP   ! Return code of FM-routines
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! the lines below must be update as soon as MODD_BUDGET is updated
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-------------------------------------------------------------------------------
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
IF (CBUTYPE=='CART') THEN              ! cartesian case only
!
  NBUWRNB = NINT (XBUWRI / XBULEN)  ! only after NBUWRNB budget periods, we write the
                                    ! result on the FM_FILE   
  IF (LBU_ICP) THEN 
    NBUIMAX_ll = 1
  ELSE
    NBUIMAX_ll = NBUIH - NBUIL +1
  END IF
  IF (LBU_JCP) THEN 
    NBUJMAX_ll = 1
  ELSE
    NBUJMAX_ll = NBUJH - NBUJL +1
  END IF
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
  NBUWRNB = NINT (XBUWRI / XBULEN)  ! only after NBUWRNB budget periods, we write the
                                    ! result on the FM_FILE
  NBUTIME = 1

  CALL GET_DIM_EXT_ll ('B', IIU,IJU)
  ALLOCATE( LBU_MASK( IIU ,IJU, NBUMASK) )
  LBU_MASK(:,:,:)=.FALSE.
  ALLOCATE( XBUSURF( IIU, IJU, NBUMASK, NBUWRNB) )
  XBUSURF(:,:,:,:) = 0.
!
! three first dimensions of budget arrays in mask case
!  the order of the dimensions are the order expected in WRITE_DIACHRO routine:
!  x,y,z,time,mask,processus  and in this case x and y are missing
!  first dimension of the arrays : dimension along K
!  second dimension of the arrays : number of the budget time period
!  third dimension of the arrays : number of the budget masks zones
  IBUDIM1=NBUKMAX
  IBUDIM2=NBUWRNB
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
ALLOCATE( NBUPROCNBR(JPBUMAX) )
ALLOCATE( NBUPROCCTR(JPBUMAX) )
ALLOCATE( CBUACTION(JPBUMAX, JPBUPROMAX) )
ALLOCATE( CBUCOMMENT(JPBUMAX, JPBUPROMAX) )
NBUPROCCTR(:) = 0
NBUCTR_ACTV(:) = 0
NBUPROCNBR(:) = 0
CBUACTION(:,:) = 'OF' 
CBUCOMMENT(:,:) = ' '
LBU_BEG =.TRUE. 
!
!-------------------------------------------------------------------------------
!
!*       3.    INITALIZE VARIABLES
!              -------------------
!
IPROACTV(:,:) = 3
IPROACTV(:,4) = 1
IPROACTV(:,JPBUPROMAX+1) = 0
GERROR=.FALSE.
YWORK2(:,:) = ' '
YEND_COMMENT(:) = ' '
!
!                        Budget of RU
IF (LBU_RU) THEN
  YWORK2(NBUDGET_U, 1) = 'INIF_'

  YWORK2(NBUDGET_U, 2) = 'ENDF_'

  YWORK2(NBUDGET_U, 3) = 'AVEF_'

  IPROC=4
  YWORK2(NBUDGET_U, IPROC) = 'ASSE_'
  IPROACTV(NBUDGET_U, IPROC) = NASSEU

  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'NEST_'
  IF( NMODEL>1 ) IPROACTV(NBUDGET_U, IPROC) = NNESTU

  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'FRC_'
  IF( LFORCING ) IPROACTV(NBUDGET_U, IPROC)  = NFRCU

  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'NUD_'
  IF( ONUDGING ) IPROACTV(NBUDGET_U, IPROC)  = NNUDU

  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'CURV_'
  IF ( .NOT. LCARTESIAN ) THEN
    IPROACTV(NBUDGET_U, IPROC) = NCURVU
  ELSE
    IPROACTV(NBUDGET_U, IPROC) = 4
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'COR_'
  IF ( LCORIO ) THEN
    IPROACTV(NBUDGET_U, IPROC) = NCORU
  ELSE
    IPROACTV(NBUDGET_U, IPROC) = 4
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'DIF_'
  IF ( ONUMDIFU ) IPROACTV(NBUDGET_U, IPROC) = NDIFU

  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'REL_'
  IF ( OHORELAX_UVWTH .OR. OVE_RELAX ) THEN
    IPROACTV(NBUDGET_U, IPROC) = NRELU
  ELSE
    IF(OVE_RELAX .OR. OHORELAX_UVWTH .OR. OHORELAX_RV .OR.                 &
     OHORELAX_RC .OR. OHORELAX_RR .OR. OHORELAX_RI .OR. OHORELAX_RS .OR.   &
     OHORELAX_RG .OR. OHORELAX_RH .OR. OHORELAX_TKE .OR. ANY(OHORELAX_SV)) THEN
      IPROACTV(NBUDGET_U, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_U, IPROC) = 3
    END IF
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'DRAG_'
  IF( ODRAGTREE ) IPROACTV(NBUDGET_U, IPROC)  = NDRAGU

  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'VTURB_'
  IF ( HTURB /= 'NONE' ) IPROACTV(NBUDGET_U, IPROC) = NVTURBU

  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'HTURB_'
  IF ( HTURB /= 'NONE' .AND. HTURBDIM == '3DIM' ) THEN
    IPROACTV(NBUDGET_U, IPROC) = NHTURBU
  ELSE
    IF ( HTURB /= 'NONE' ) THEN
      IPROACTV(NBUDGET_U, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_U, IPROC) = 3
    END IF
  END IF 

  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'MAFL_'
  IF ( HSCONV == 'EDKF' ) IPROACTV(NBUDGET_U, IPROC) = NMAFLU

  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'ADV_'
  IPROACTV(NBUDGET_U, IPROC) = NADVU

  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'PRES_'
  IPROACTV(NBUDGET_U, IPROC) = NPRESU
!
  YEND_COMMENT(NBUDGET_U) = 'BU_RU'
  NBUPROCNBR(NBUDGET_U) = 3 !Initial number of budgets, will be increazed later if necessary
!
  CBUACTION(NBUDGET_U, 1) = 'IG'
  CBUACTION(NBUDGET_U, 2) = 'CC'
  CBUACTION(NBUDGET_U, 3) = 'ES'
!
  DO JJ=1,3
    CBUCOMMENT(NBUDGET_U, JJ) = ADJUSTL( ADJUSTR( YWORK2(NBUDGET_U, JJ) ) // &
                                ADJUSTL( YEND_COMMENT(NBUDGET_U) ) )
  END DO
!
END IF
!
!                        Budget of RV
IF (LBU_RV) THEN
  YWORK2(NBUDGET_V, 1) = 'INIF_'

  YWORK2(NBUDGET_V, 2) = 'ENDF_'

  YWORK2(NBUDGET_V, 3) = 'AVEF_'

  IPROC=4
  YWORK2(NBUDGET_V, IPROC) = 'ASSE_'
  IPROACTV(NBUDGET_V, IPROC) = NASSEV

  IPROC=IPROC+1
  YWORK2(NBUDGET_V, IPROC) = 'NEST_'
  IF( NMODEL>1 ) IPROACTV(NBUDGET_V, IPROC) = NNESTV

  IPROC=IPROC+1
  YWORK2(NBUDGET_V, IPROC) = 'FRC_'
  IF( LFORCING ) IPROACTV(NBUDGET_V, IPROC)  = NFRCV

  IPROC=IPROC+1
  YWORK2(NBUDGET_V, IPROC) = 'NUD_'
  IF( ONUDGING ) IPROACTV(NBUDGET_V, IPROC)  = NNUDV

  IPROC=IPROC+1
  YWORK2(NBUDGET_V, IPROC) = 'CURV_'
  IF ( .NOT. LCARTESIAN ) THEN
    IPROACTV(NBUDGET_V, IPROC) = NCURVV
  ELSE
    IPROACTV(NBUDGET_V, IPROC) = 4
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_V, IPROC) = 'COR_'
  IF ( LCORIO ) THEN
    IPROACTV(NBUDGET_V, IPROC) = NCORV
  ELSE
    IPROACTV(NBUDGET_V, IPROC) = 4
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_V, IPROC) = 'DIF_'
  IF ( ONUMDIFU ) IPROACTV(NBUDGET_V, IPROC) = NDIFV

  IPROC=IPROC+1
  YWORK2(NBUDGET_V, IPROC) = 'REL_'
  IF ( OHORELAX_UVWTH .OR. OVE_RELAX ) THEN
    IPROACTV(NBUDGET_V, IPROC) = NRELV
  ELSE
    IF(OVE_RELAX .OR. OHORELAX_UVWTH .OR. OHORELAX_RV .OR.                 &
     OHORELAX_RC .OR. OHORELAX_RR .OR. OHORELAX_RI .OR. OHORELAX_RS .OR.   &
     OHORELAX_RG .OR. OHORELAX_RH .OR. OHORELAX_TKE .OR. ANY(OHORELAX_SV)) THEN
      IPROACTV(NBUDGET_V, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_V, IPROC) = 3
    END IF
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_V, IPROC) = 'DRAG_'
  IF( ODRAGTREE ) IPROACTV(NBUDGET_V, IPROC)  = NDRAGV

  IPROC=IPROC+1
  YWORK2(NBUDGET_V, IPROC) = 'VTURB_'
  IF ( HTURB /= 'NONE' ) IPROACTV(NBUDGET_V, IPROC) = NVTURBV

  IPROC=IPROC+1
  YWORK2(NBUDGET_V, IPROC) = 'HTURB_'
  IF ( HTURB /= 'NONE' .AND. HTURBDIM == '3DIM' ) THEN
    IPROACTV(NBUDGET_V, IPROC) = NHTURBV
  ELSE
    IF ( HTURB /= 'NONE' ) THEN
      IPROACTV(NBUDGET_V, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_V, IPROC) = 3
    END IF
  END IF 

  IPROC=IPROC+1
  YWORK2(NBUDGET_V, IPROC) = 'MAFL_'
  IF ( HSCONV == 'EDKF' ) IPROACTV(NBUDGET_V, IPROC) = NMAFLV

  IPROC=IPROC+1
  YWORK2(NBUDGET_V, IPROC) = 'ADV_'
  IPROACTV(NBUDGET_V, IPROC) = NADVV

  IPROC=IPROC+1
  YWORK2(NBUDGET_V, IPROC) = 'PRES_'
  IPROACTV(NBUDGET_V, IPROC) = NPRESV

  YEND_COMMENT(NBUDGET_V) = 'BU_RV'
  NBUPROCNBR(NBUDGET_V) = 3
!
  CBUACTION(NBUDGET_V, 1) = 'IG'
  CBUACTION(NBUDGET_V, 2) = 'CC'
  CBUACTION(NBUDGET_V, 3) = 'ES'
!
  DO JJ=1,3
    CBUCOMMENT(NBUDGET_V, JJ) = ADJUSTL( ADJUSTR( YWORK2(NBUDGET_V, JJ) ) // &
                                ADJUSTL( YEND_COMMENT(NBUDGET_V) ) )
  END DO
!
END IF
!
!                        Budget of RW
IF (LBU_RW) THEN
  YWORK2(NBUDGET_W, 1) = 'INIF_'

  YWORK2(NBUDGET_W, 2) = 'ENDF_'

  YWORK2(NBUDGET_W, 3) = 'AVEF_'

  IPROC=4
  YWORK2(NBUDGET_W, IPROC) = 'ASSE_'
  IPROACTV(NBUDGET_W, IPROC) = NASSEW

  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'NEST_'
  IF( NMODEL>1 ) IPROACTV(NBUDGET_W, IPROC) = NNESTW

  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'FRC_'
  IF( LFORCING ) IPROACTV(NBUDGET_W, IPROC)  = NFRCW

  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'NUD_'
  IF( ONUDGING ) IPROACTV(NBUDGET_W, IPROC)  = NNUDW

  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'CURV_'
  IF ( .NOT. LCARTESIAN ) THEN
    IPROACTV(NBUDGET_W, IPROC) = NCURVW
  ELSE
    IPROACTV(NBUDGET_W, IPROC) = 4
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'COR_'
  IF ( LCORIO ) THEN
    IPROACTV(NBUDGET_W, IPROC) = NCORW
  ELSE
    IPROACTV(NBUDGET_W, IPROC) = 4
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'DIF_'
  IF ( ONUMDIFU ) IPROACTV(NBUDGET_W, IPROC) = NDIFW

  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'REL_'
  IF ( OHORELAX_UVWTH .OR. OVE_RELAX ) THEN
    IPROACTV(NBUDGET_W, IPROC) = NRELW
  ELSE
    IF(OVE_RELAX .OR. OHORELAX_UVWTH .OR. OHORELAX_RV .OR.                 &
     OHORELAX_RC .OR. OHORELAX_RR .OR. OHORELAX_RI .OR. OHORELAX_RS .OR.   &
     OHORELAX_RG .OR. OHORELAX_RH .OR. OHORELAX_TKE .OR. ANY(OHORELAX_SV)) THEN
      IPROACTV(NBUDGET_W, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_W, IPROC) = 3
    END IF
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'VTURB_'
  IF ( HTURB /= 'NONE' ) IPROACTV(NBUDGET_W, IPROC) = NVTURBW

  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'HTURB_'
  IF ( HTURB /= 'NONE' .AND. HTURBDIM == '3DIM' ) THEN
    IPROACTV(NBUDGET_W, IPROC) = NHTURBW
  ELSE
    IF ( HTURB /= 'NONE' ) THEN
      IPROACTV(NBUDGET_W, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_W, IPROC) = 3
    END IF
  END IF 

  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'GRAV_'
  IPROACTV(NBUDGET_W, IPROC) = NGRAVW

  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'ADV_'
  IPROACTV(NBUDGET_W, IPROC) = NADVW

  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'PRES_'
  IPROACTV(NBUDGET_W, IPROC) = NPRESW

  YEND_COMMENT(NBUDGET_W) = 'BU_RW'
  NBUPROCNBR(NBUDGET_W) = 3
!
  CBUACTION(NBUDGET_W, 1) = 'IG'
  CBUACTION(NBUDGET_W, 2) = 'CC'
  CBUACTION(NBUDGET_W, 3) = 'ES'
!
  DO JJ=1,3
    CBUCOMMENT(NBUDGET_W, JJ) = ADJUSTL( ADJUSTR( YWORK2(NBUDGET_W, JJ) ) // &
                                ADJUSTL( YEND_COMMENT(NBUDGET_W) ) )
  END DO
!
END IF
!
!                        Budget of RTH
IF (LBU_RTH) THEN
  YWORK2(NBUDGET_TH, 1) = 'INIF_'

  YWORK2(NBUDGET_TH, 2) = 'ENDF_'

  YWORK2(NBUDGET_TH, 3) = 'AVEF_'

  IPROC=4
  YWORK2(NBUDGET_TH, IPROC) = 'ASSE_'
  IPROACTV(NBUDGET_TH, IPROC) = NASSETH

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'NEST_'
  IF( NMODEL>1 ) IPROACTV(NBUDGET_TH, IPROC) = NNESTTH

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'FRC_'
  IF( LFORCING ) IPROACTV(NBUDGET_TH, IPROC)  = NFRCTH

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = '2DADV_'
  IF( L2D_ADV_FRC ) IPROACTV(NBUDGET_TH, IPROC)  = N2DADVTH

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = '2DREL_'
  IF( L2D_REL_FRC ) IPROACTV(NBUDGET_TH, IPROC)  = N2DRELTH

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'NUD_'
  IF( ONUDGING ) IPROACTV(NBUDGET_TH, IPROC)  = NNUDTH

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'PREF_'
  IF ( KRR > 0 ) THEN
    IF(.NOT.L1D) IPROACTV(NBUDGET_TH, IPROC) = NPREFTH
  ELSE
    IPROACTV(NBUDGET_TH, IPROC) = 4
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'DIF_'
  IF ( ONUMDIFTH ) IPROACTV(NBUDGET_TH, IPROC) = NDIFTH

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'REL_'
  IF ( OHORELAX_UVWTH .OR. OVE_RELAX ) THEN
    IPROACTV(NBUDGET_TH, IPROC) = NRELTH
  ELSE
    IF(OVE_RELAX .OR. OHORELAX_UVWTH .OR. OHORELAX_RV .OR.                 &
     OHORELAX_RC .OR. OHORELAX_RR .OR. OHORELAX_RI .OR. OHORELAX_RS .OR.   &
     OHORELAX_RG .OR. OHORELAX_RH .OR. OHORELAX_TKE .OR. ANY(OHORELAX_SV)) THEN
      IPROACTV(NBUDGET_TH, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_TH, IPROC) = 3
    END IF
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'RAD_'
  IF ( HRAD /= 'NONE' ) IPROACTV(NBUDGET_TH, IPROC) = NRADTH

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'DCONV_'
  IF ( HDCONV /= 'NONE' .OR. HSCONV == 'KAFR') IPROACTV(NBUDGET_TH, IPROC) = NDCONVTH

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'VTURB_'
  IF ( HTURB /= 'NONE' ) IPROACTV(NBUDGET_TH, IPROC) = NVTURBTH

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'HTURB_'
  IF ( HTURB /= 'NONE' .AND. HTURBDIM == '3DIM' ) THEN
    IPROACTV(NBUDGET_TH, IPROC) = NHTURBTH
  ELSE
    IF ( HTURB /= 'NONE' ) THEN
      IPROACTV(NBUDGET_TH, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_TH, IPROC) = 3
    END IF
  END IF 

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'DISSH_'
  IF (HTURB /= 'NONE')     IPROACTV(NBUDGET_TH, IPROC) = NDISSHTH

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'NETUR_'
  IF (HTURB /= 'NONE' .AND. ( (HCLOUD == 'KHKO') .OR.  (HCLOUD == 'C2R2'))) &
  IPROACTV(NBUDGET_TH, IPROC) = NNETURTH

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'MAFL_'
  IF ( HSCONV == 'EDKF' ) IPROACTV(NBUDGET_TH, IPROC) = NMAFLTH

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'ADV_'
  IPROACTV(NBUDGET_TH, IPROC) = NADVTH

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'NEADV_'
  IF ((HCLOUD == 'KHKO')  .OR.  (HCLOUD == 'C2R2'))  IPROACTV(NBUDGET_TH, IPROC) = NNEADVTH

  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'NEGA_'
  IF (HCLOUD /= 'NONE' .AND. HCLOUD /= 'KHKO' .AND. HCLOUD /= 'C2R2') &
          IPROACTV(NBUDGET_TH, IPROC) = NNEGATH

  IF (HCLOUD == 'LIMA') THEN
    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'SEDI_'
    IF (OPTSPLIT)                                           IPROACTV(NBUDGET_TH, IPROC) = NSEDITH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'HENU_'
    IF (OWARM .AND. OACTI .AND. NMOD_CCN.GE.1)              IPROACTV(NBUDGET_TH, IPROC) = NHENUTH

    IF (.NOT.OPTSPLIT) THEN
      IPROC=IPROC+1
      YWORK2(NBUDGET_TH, IPROC) = 'REVA_'
      IF (OWARM .AND. ORAIN)                                IPROACTV(NBUDGET_TH, IPROC) = NREVATH
    END IF

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'HIND_'
    IF (OCOLD .AND. ONUCL)                                  IPROACTV(NBUDGET_TH, IPROC) = NHINDTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'HINC_'
    IF (OCOLD .AND. ONUCL)                                  IPROACTV(NBUDGET_TH, IPROC) = NHINCTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'HONH_'
    IF (OCOLD .AND. ONUCL .AND. OHHONI .AND. NMOD_CCN.GE.1) IPROACTV(NBUDGET_TH, IPROC) = NHONHTH

    IF (OPTSPLIT) THEN
      IPROC=IPROC+1
      YWORK2(NBUDGET_TH, IPROC) = 'REVA_'
                                                            IPROACTV(NBUDGET_TH, IPROC) = NREVATH
    END IF

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'HONC_'
    IF (OCOLD .AND. OWARM .AND. ONUCL)                      IPROACTV(NBUDGET_TH, IPROC) = NHONCTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'HONR_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. ONUCL .AND. ORAIN))      IPROACTV(NBUDGET_TH, IPROC) = NHONRTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'DEPS_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))                  IPROACTV(NBUDGET_TH, IPROC) = NDEPSTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'DEPG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_TH, IPROC) = NDEPGTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'IMLT_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM))                  IPROACTV(NBUDGET_TH, IPROC) = NIMLTTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'BERFI_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM))                  IPROACTV(NBUDGET_TH, IPROC) = NBERFITH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'RIM_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_TH, IPROC) = NRIMTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'ACC_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW .AND. ORAIN))      IPROACTV(NBUDGET_TH, IPROC) = NACCTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'CFRZ_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_TH, IPROC) = NCFRZTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'WETG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_TH, IPROC) = NWETGTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'DRYG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_TH, IPROC) = NDRYGTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'GMLT_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_TH, IPROC) = NGMLTTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'WETH_'
    IF (.NOT.OPTSPLIT .AND. OHAIL)                          IPROACTV(NBUDGET_TH, IPROC) = NWETHTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'HMLT_'
    IF (.NOT.OPTSPLIT .AND. OHAIL)                          IPROACTV(NBUDGET_TH, IPROC) = NHMLTTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'CEDS_'
                                                            IPROACTV(NBUDGET_TH, IPROC) = NCEDSTH
  ELSE
    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'ADJU_'
    IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LADJ_BEFORE) IPROACTV(NBUDGET_TH, IPROC) = NADJUTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'HENU_'
    IF (HCLOUD(1:3) == 'ICE' .OR. (HCLOUD == 'C2R2' .AND. (.NOT. LSUPSAT)) &
          .OR. ( HCLOUD == 'KHKO' .AND. (.NOT. LSUPSAT)) ) &
      IPROACTV(NBUDGET_TH, IPROC) = NHENUTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'HON_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NHONTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'SFR_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NSFRTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'DEPS_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NDEPSTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'DEPG_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NDEPGTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'REVA_'
    IF (((HCLOUD(1:3) == 'ICE') .AND. LWARM) .OR. ((HCLOUD == 'C2R2' &
       .OR. HCLOUD == 'KHKO') .AND. LRAIN) .OR. HCLOUD(1:3) == 'KES')             &
       IPROACTV(NBUDGET_TH, IPROC) = NREVATH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'RIM_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NRIMTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'ACC_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NACCTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'CFRZ_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NCFRZTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'WETG_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NWETGTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'DRYG_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NDRYGTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'GMLT_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NGMLTTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'WETH_'
    IF (HCLOUD == 'ICE4') IPROACTV(NBUDGET_TH, IPROC) = NWETHTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'DRYH_'
    IF (HCLOUD == 'ICE4'.AND. LRED) IPROACTV(NBUDGET_TH, IPROC) = NDRYHTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'HMLT_'
    IF (HCLOUD == 'ICE4') IPROACTV(NBUDGET_TH, IPROC) = NHMLTTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'IMLT_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NIMLTTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'BERFI_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NBERFITH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'CORR_'
    IF (HCLOUD(1:3) == 'ICE' .AND. LRED) IPROACTV(NBUDGET_TH, IPROC) = NCORRTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'CDEPI_'
    IF (HCLOUD(1:3) == 'ICE' .AND. .NOT. LRED .OR. (LRED .AND. LADJ_AFTER)) &
        IPROACTV(NBUDGET_TH, IPROC) = NCDEPITH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'COND_'
    IF (HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO' .OR. HCLOUD(1:3) == 'KES' .OR.   &
    HCLOUD == 'REVE')   IPROACTV(NBUDGET_TH, IPROC) = NCONDTH

    IPROC=IPROC+1
    YWORK2(NBUDGET_TH, IPROC) = 'NECON_'
    IF ((HCLOUD == 'KHKO')  .OR.  (HCLOUD == 'C2R2'))&
        IPROACTV(NBUDGET_TH, IPROC) = NNECONTH
  END IF

  YEND_COMMENT(NBUDGET_TH) = 'BU_RTH'
  NBUPROCNBR(NBUDGET_TH) = 3
!
  CBUACTION(NBUDGET_TH, 1) = 'IG'
  CBUACTION(NBUDGET_TH, 2) = 'CC'
  CBUACTION(NBUDGET_TH, 3) = 'ES'
!
  DO JJ=1,3
    CBUCOMMENT(NBUDGET_TH, JJ) = ADJUSTL( ADJUSTR( YWORK2(NBUDGET_TH, JJ) ) // &
                                 ADJUSTL( YEND_COMMENT(NBUDGET_TH) ) )
  END DO
!
END IF
!
!                        Budget of RTKE
IF (LBU_RTKE) THEN
  YWORK2(NBUDGET_TKE, 1) = 'INIF_'

  YWORK2(NBUDGET_TKE, 2) = 'ENDF_'

  YWORK2(NBUDGET_TKE, 3) = 'AVEF_'

  IPROC=4
  YWORK2(NBUDGET_TKE, IPROC) = 'ASSE_'
  IPROACTV(NBUDGET_TKE, IPROC) = NASSETKE

  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'FRC_'
  IF( LFORCING ) IPROACTV(NBUDGET_TKE, IPROC)  = NFRCTKE

  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'DIF_'
  IF ( ONUMDIFTH ) IPROACTV(NBUDGET_TKE, IPROC) = NDIFTKE

  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'REL_'
  IF ( OHORELAX_TKE ) THEN
    IPROACTV(NBUDGET_TKE, IPROC) = NRELTKE
  ELSE
    IF(OVE_RELAX .OR. OHORELAX_UVWTH .OR. OHORELAX_RV .OR.                 &
     OHORELAX_RC .OR. OHORELAX_RR .OR. OHORELAX_RI .OR. OHORELAX_RS .OR.   &
     OHORELAX_RG .OR. OHORELAX_RH .OR. OHORELAX_TKE .OR. ANY(OHORELAX_SV)) THEN
      IPROACTV(NBUDGET_TKE, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_TKE, IPROC) = 3
    END IF
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'DRAG_'
  IF( ODRAGTREE ) IPROACTV(NBUDGET_TKE, IPROC)  = NDRAGTKE

  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'DP_'
  IPROACTV(NBUDGET_TKE, IPROC) = NDPTKE

  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'TP_'
  IPROACTV(NBUDGET_TKE, IPROC) = NTPTKE

  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'DISS_'
  IPROACTV(NBUDGET_TKE, IPROC) = NDISSTKE

  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'TR_'
  IPROACTV(NBUDGET_TKE, IPROC) = NTRTKE

  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'ADV_'
  IPROACTV(NBUDGET_TKE, IPROC) = NADVTKE

  YEND_COMMENT(NBUDGET_TKE) = 'BU_RTKE'
  NBUPROCNBR(NBUDGET_TKE) = 3
!
  CBUACTION(NBUDGET_TKE, 1) = 'IG'
  CBUACTION(NBUDGET_TKE, 2) = 'CC'
  CBUACTION(NBUDGET_TKE, 3) = 'ES'
!
  DO JJ=1,3
    CBUCOMMENT(NBUDGET_TKE, JJ) = ADJUSTL( ADJUSTR( YWORK2(NBUDGET_TKE, JJ) ) // &
                                  ADJUSTL( YEND_COMMENT(NBUDGET_TKE) ) )
  END DO
!
END IF
!
!                        Budget of RRV
IF (LBU_RRV) THEN
  YWORK2(NBUDGET_RV, 1) = 'INIF_'

  YWORK2(NBUDGET_RV, 2) = 'ENDF_'

  YWORK2(NBUDGET_RV, 3) = 'AVEF_'

  IPROC=4
  YWORK2(NBUDGET_RV, IPROC) = 'ASSE_'
  IPROACTV(NBUDGET_RV, IPROC) = NASSERV

  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'NEST_'
  IF( NMODEL>1 ) IPROACTV(NBUDGET_RV, IPROC) = NNESTRV

  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'FRC_'
  IF( LFORCING ) IPROACTV(NBUDGET_RV, IPROC)  = NFRCRV

  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = '2DADV_'
  IF( L2D_ADV_FRC ) IPROACTV(NBUDGET_RV, IPROC)  = N2DADVRV

  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = '2DREL_'
  IF( L2D_REL_FRC ) IPROACTV(NBUDGET_RV, IPROC)  = N2DRELRV

  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'NUD_'
  IF( ONUDGING ) IPROACTV(NBUDGET_RV, IPROC)  = NNUDRV

  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'DIF_'
  IF ( ONUMDIFTH ) IPROACTV(NBUDGET_RV, IPROC) = NDIFRV

  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'REL_'
  IF ( OHORELAX_RV .OR. OVE_RELAX ) THEN
    IPROACTV(NBUDGET_RV, IPROC) = NRELRV
  ELSE
    IF(OVE_RELAX .OR. OHORELAX_UVWTH .OR. OHORELAX_RV .OR.                 &
     OHORELAX_RC .OR. OHORELAX_RR .OR. OHORELAX_RI .OR. OHORELAX_RS .OR.   &
     OHORELAX_RG .OR. OHORELAX_RH .OR. OHORELAX_TKE .OR. ANY(OHORELAX_SV)) THEN
      IPROACTV(NBUDGET_RV, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_RV, IPROC) = 3
    END IF
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'DCONV_'
  IF ( HDCONV /= 'NONE' .OR. HSCONV == 'KAFR') IPROACTV(NBUDGET_RV, IPROC) = NDCONVRV

  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'VTURB_'
  IF ( HTURB /= 'NONE' ) IPROACTV(NBUDGET_RV, IPROC) = NVTURBRV

  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'HTURB_'
  IF ( HTURB /= 'NONE' .AND. HTURBDIM == '3DIM' ) THEN
    IPROACTV(NBUDGET_RV, IPROC) = NHTURBRV
  ELSE
    IF ( HTURB /= 'NONE' ) THEN
      IPROACTV(NBUDGET_RV, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_RV, IPROC) = 3
    END IF
  END IF 

  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'NETUR_'
  IF (HTURB /= 'NONE' .AND. ( (HCLOUD == 'KHKO') .OR.  (HCLOUD == 'C2R2'))) &
   IPROACTV(NBUDGET_RV, IPROC) = NNETURRV

  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'MAFL_'
  IF ( HSCONV == 'EDKF' ) IPROACTV(NBUDGET_RV, IPROC) = NMAFLRV

  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'ADV_'
  IPROACTV(NBUDGET_RV, IPROC) = NADVRV

  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'NEADV_'
  IF ((HCLOUD == 'KHKO')  .OR.  (HCLOUD == 'C2R2'))  IPROACTV(NBUDGET_RV, IPROC) = NNEADVRV

  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'NEGA_'
  IF (HCLOUD /= 'NONE' .AND. HCLOUD /= 'KHKO' .AND. HCLOUD /= 'C2R2') &
     IPROACTV(NBUDGET_RV, IPROC) = NNEGARV

  IF (HCLOUD == 'LIMA') THEN
    IPROC=IPROC+1
    YWORK2(NBUDGET_RV, IPROC) = 'HENU_'
    IF (OWARM .AND. OACTI .AND. NMOD_CCN.GE.1)              IPROACTV(NBUDGET_RV, IPROC) = NHENURV

    IF (.NOT.OPTSPLIT) THEN
      IPROC=IPROC+1
      YWORK2(NBUDGET_RV, IPROC) = 'REVA_'
      IF (OWARM .AND. ORAIN)                                IPROACTV(NBUDGET_RV, IPROC) = NREVARV
    END IF

    IPROC=IPROC+1
    YWORK2(NBUDGET_RV, IPROC) = 'HIND_'
    IF (OCOLD .AND. ONUCL)                                  IPROACTV(NBUDGET_RV, IPROC) = NHINDRV

    IPROC=IPROC+1
    YWORK2(NBUDGET_RV, IPROC) = 'HONH_'
    IF (OCOLD .AND. ONUCL .AND. OHHONI .AND. NMOD_CCN.GE.1) IPROACTV(NBUDGET_RV, IPROC) = NHONHRV

    IF (OPTSPLIT) THEN
      IPROC=IPROC+1
      YWORK2(NBUDGET_RV, IPROC) = 'REVA_'
                                                            IPROACTV(NBUDGET_RV, IPROC) = NREVARV
    END IF

    IPROC=IPROC+1
    YWORK2(NBUDGET_RV, IPROC) = 'DEPS_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))                  IPROACTV(NBUDGET_RV, IPROC) = NDEPSRV

    IPROC=IPROC+1
    YWORK2(NBUDGET_RV, IPROC) = 'DEPG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_RV, IPROC) = NDEPGRV

    IPROC=IPROC+1
    YWORK2(NBUDGET_RV, IPROC) = 'CEDS_'
                                                            IPROACTV(NBUDGET_RV, IPROC) = NCEDSRV

  ELSE
    IPROC=IPROC+1
    YWORK2(NBUDGET_RV, IPROC) = 'ADJU_'
    IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LADJ_BEFORE) &
            IPROACTV(NBUDGET_RV, IPROC) = NADJURV

    IPROC=IPROC+1
    YWORK2(NBUDGET_RV, IPROC) = 'HENU_'
    IF ((HCLOUD == 'C2R2'  .AND. (.NOT. LSUPSAT)) &
            .OR. ( HCLOUD == 'KHKO' .AND. (.NOT. LSUPSAT)) &
            .OR. HCLOUD(1:3) == 'ICE')  &
          IPROACTV(NBUDGET_RV, IPROC) = NHENURV

    IPROC=IPROC+1
    YWORK2(NBUDGET_RV, IPROC) = 'DEPS_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RV, IPROC) = NDEPSRV

    IPROC=IPROC+1
    YWORK2(NBUDGET_RV, IPROC) = 'DEPG_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RV, IPROC) = NDEPGRV

    IPROC=IPROC+1
    YWORK2(NBUDGET_RV, IPROC) = 'REVA_'
    IF (HCLOUD(1:3) == 'KES' .OR. ((HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO') .AND. LRAIN) .OR. &
        ((HCLOUD(1:3) == 'ICE') .AND. LWARM)) IPROACTV(NBUDGET_RV, IPROC) = NREVARV

    IPROC=IPROC+1
    YWORK2(NBUDGET_RV, IPROC) = 'COND_'
    IF (HCLOUD(1:3) == 'KES'.OR. HCLOUD == 'REVE' .OR. &
        HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO' ) IPROACTV(NBUDGET_RV, IPROC) = NCONDRV

    IPROC=IPROC+1
    YWORK2(NBUDGET_RV, IPROC) = 'CORR_'
    IF (HCLOUD(1:3) == 'ICE' .AND. LRED) IPROACTV(NBUDGET_RV, IPROC) = NCORRRV

    IPROC=IPROC+1
    YWORK2(NBUDGET_RV, IPROC) = 'CDEPI_'
    IF (HCLOUD(1:3) == 'ICE' .AND. (.NOT. LRED .OR. (LRED .AND. LADJ_AFTER) )) &
            IPROACTV(NBUDGET_RV, IPROC) = NCDEPIRV

    IPROC=IPROC+1
    YWORK2(NBUDGET_RV, IPROC) = 'NECON_'
    IF ( (HCLOUD == 'KHKO' )  .OR.  (HCLOUD == 'C2R2')) &
            IPROACTV(NBUDGET_RV, IPROC) = NNECONRV
  END IF

  YEND_COMMENT(NBUDGET_RV) = 'BU_RRV'
  NBUPROCNBR(NBUDGET_RV) = 3
!
  CBUACTION(NBUDGET_RV, 1) = 'IG'
  CBUACTION(NBUDGET_RV, 2) = 'CC'
  CBUACTION(NBUDGET_RV, 3) = 'ES'
!
  DO JJ=1,3
    CBUCOMMENT(NBUDGET_RV, JJ) = ADJUSTL( ADJUSTR( YWORK2(NBUDGET_RV, JJ) ) // &
                                 ADJUSTL( YEND_COMMENT(NBUDGET_RV) ) )
  END DO
!
END IF
!
!                        Budget of RRC
IF (LBU_RRC) THEN
  YWORK2(NBUDGET_RC, 1) = 'INIF_'

  YWORK2(NBUDGET_RC, 2) = 'ENDF_'

  YWORK2(NBUDGET_RC, 3) = 'AVEF_'

  IPROC=4
  YWORK2(NBUDGET_RC, IPROC) = 'ASSE_'
  IPROACTV(NBUDGET_RC, IPROC) = NASSERC

  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'NEST_'
  IF( NMODEL>1 ) IPROACTV(NBUDGET_RC, IPROC) = NNESTRC

  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'FRC_'
  IF( LFORCING ) IPROACTV(NBUDGET_RC, IPROC)  = NFRCRC

  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'DIF_'
  IF ( ONUMDIFTH ) IPROACTV(NBUDGET_RC, IPROC) = NDIFRC

  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'REL_'
  IF ( OHORELAX_RC ) THEN
    IPROACTV(NBUDGET_RC, IPROC) = NRELRC
  ELSE
    IF(OVE_RELAX .OR. OHORELAX_UVWTH .OR. OHORELAX_RV .OR.                 &
     OHORELAX_RC .OR. OHORELAX_RR .OR. OHORELAX_RI .OR. OHORELAX_RS .OR.   &
     OHORELAX_RG .OR. OHORELAX_RH .OR. OHORELAX_TKE .OR. ANY(OHORELAX_SV)) THEN
      IPROACTV(NBUDGET_RC, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_RC, IPROC) = 3
    END IF
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'DCONV_'
  IF( HDCONV /= 'NONE' .OR. HSCONV == 'KAFR') IPROACTV(NBUDGET_RC, IPROC)  = NDCONVRC

  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'DEPOTR'
  IF( ODRAGTREE .AND. ODEPOTREE ) IPROACTV(NBUDGET_RC, IPROC) = NDEPOTRRC

  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'VTURB_'
  IF ( HTURB /= 'NONE' ) IPROACTV(NBUDGET_RC, IPROC) = NVTURBRC

  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'HTURB_'
  IF ( HTURB /= 'NONE' .AND. HTURBDIM == '3DIM' ) THEN
    IPROACTV(NBUDGET_RC, IPROC) = NHTURBRC
  ELSE 
    IF ( HTURB /= 'NONE' ) THEN
      IPROACTV(NBUDGET_RC, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_RC, IPROC) = 3
    END IF
  END IF 

  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'NETUR_'
  IF (HTURB /= 'NONE' .AND. ( (HCLOUD == 'KHKO') .OR.  (HCLOUD == 'C2R2'))) &
      IPROACTV(NBUDGET_RC, IPROC) = NNETURRC

  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'ADV_'
  IPROACTV(NBUDGET_RC, IPROC) = NADVRC

  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'NEADV_'
  IF ((HCLOUD == 'KHKO')  .OR.  (HCLOUD == 'C2R2'))  &
      IPROACTV(NBUDGET_RC, IPROC) = NNEADVRC

  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'NEGA_'
  IF (HCLOUD /= 'NONE' .AND. HCLOUD /= 'KHKO' .AND. HCLOUD /= 'C2R2') &
     IPROACTV(NBUDGET_RC, IPROC)  = NNEGARC

  IF (HCLOUD == 'LIMA') THEN
    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'CORR_'
    IF (OPTSPLIT .AND. OWARM .AND. ORAIN)              IPROACTV(NBUDGET_RC, IPROC) = NCORRRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'SEDI_'
    IF (OWARM .AND. OSEDC)                             IPROACTV(NBUDGET_RC, IPROC) = NSEDIRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'DEPO_'
    IF (OWARM .AND. ODEPOC)                            IPROACTV(NBUDGET_RC, IPROC) = NDEPORC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'R2C1_'
    IF (OPTSPLIT .AND. OWARM .AND. ORAIN)              IPROACTV(NBUDGET_RC, IPROC) = NR2C1RC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'HENU_'
    IF (OWARM .AND. OACTI .AND. NMOD_CCN.GE.1)         IPROACTV(NBUDGET_RC, IPROC) = NHENURC

    IF (OPTSPLIT) THEN
      IPROC=IPROC+1
      YWORK2(NBUDGET_RC, IPROC) = 'HINC_'
      IF (OCOLD .AND. ONUCL)                          IPROACTV(NBUDGET_RC, IPROC) = NHINCRC
    END IF

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'AUTO_'
    IF (OPTSPLIT .OR. (OWARM .AND. ORAIN))             IPROACTV(NBUDGET_RC, IPROC) = NAUTORC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'ACCR_'
    IF (OPTSPLIT .OR. (OWARM .AND. ORAIN))             IPROACTV(NBUDGET_RC, IPROC) = NACCRRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'REVA_'
    IF (OPTSPLIT .OR. (OWARM .AND. ORAIN))             IPROACTV(NBUDGET_RC, IPROC) = NREVARC

    IF (.NOT.OPTSPLIT) THEN
      IPROC=IPROC+1
      YWORK2(NBUDGET_RC, IPROC) = 'HINC_'
      IF (OCOLD .AND. ONUCL)                          IPROACTV(NBUDGET_RC, IPROC) = NHINCRC
    END IF

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'HONC_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. ONUCL)) IPROACTV(NBUDGET_RC, IPROC) = NHONCRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'IMLT_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM))             IPROACTV(NBUDGET_RC, IPROC) = NIMLTRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'BERFI_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM))             IPROACTV(NBUDGET_RC, IPROC) = NBERFIRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'RIM_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RC, IPROC) = NRIMRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'WETG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RC, IPROC) = NWETGRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'DRYG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RC, IPROC) = NDRYGRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'CVRC_'
    IF (OPTSPLIT)                                      IPROACTV(NBUDGET_RC, IPROC) = NCVRCRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'WETH_'
    IF (.NOT.OPTSPLIT .AND. OHAIL)                     IPROACTV(NBUDGET_RC, IPROC) = NWETHRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'CEDS_'
                                                       IPROACTV(NBUDGET_RC, IPROC) = NCEDSRC
  ELSE
    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'ACCR_'
    IF (HCLOUD(1:3) == 'KES' )            IPROACTV(NBUDGET_RC, IPROC  ) = NACCRRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'AUTO_'
    IF (HCLOUD(1:3) == 'KES' )            IPROACTV(NBUDGET_RC, IPROC) = NAUTORC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'HENU_'
    IF ((HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO')  .AND. (.NOT. LSUPSAT)) &
            IPROACTV(NBUDGET_RC, IPROC) = NHENURC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'ADJU_'
    IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LADJ_BEFORE) &
            IPROACTV(NBUDGET_RC, IPROC) = NADJURC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'SEDI_'
    IF (HCLOUD(1:3) == 'ICE' .AND. LSEDIC .AND. &
            LRED .AND. (.NOT. LSEDIM_AFTER)) &
       IPROACTV(NBUDGET_RC, IPROC) = NSEDIRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'HON_'
    IF (HCLOUD(1:3) == 'ICE' ) IPROACTV(NBUDGET_RC, IPROC) = NHONRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'AUTO_'
    IF (((HCLOUD(1:3) == 'ICE' ) .AND. LWARM) .OR.  ((HCLOUD == 'C2R2' .OR. &
          HCLOUD == 'KHKO') .AND. LRAIN)) IPROACTV(NBUDGET_RC, IPROC) = NAUTORC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'ACCR_'
    IF (((HCLOUD(1:3) == 'ICE' ) .AND. LWARM) .OR.  ((HCLOUD == 'C2R2' .OR. &
          HCLOUD == 'KHKO') .AND. LRAIN)) IPROACTV(NBUDGET_RC, IPROC) = NACCRRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'RIM_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RC, IPROC) = NRIMRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'CMEL_'
    IF (HCLOUD(1:3) == 'ICE'  .AND. LRED) IPROACTV(NBUDGET_RC, IPROC) = NCMELRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'WETG_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RC, IPROC) = NWETGRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'DRYG_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RC, IPROC) = NDRYGRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'WETH_'
    IF (HCLOUD == 'ICE4')  IPROACTV(NBUDGET_RC, IPROC) = NWETHRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'DRYH_'
    IF (HCLOUD == 'ICE4' .AND. LRED)  IPROACTV(NBUDGET_RC, IPROC) = NDRYHRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'IMLT_'
    IF (HCLOUD(1:3) == 'ICE' ) IPROACTV(NBUDGET_RC, IPROC) = NIMLTRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'BERFI_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RC, IPROC) = NBERFIRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'CORR_'
    IF (HCLOUD(1:3) == 'ICE' .AND. LRED) IPROACTV(NBUDGET_RC, IPROC) = NCORRRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'SEDI_'
    IF (((HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO').AND. LSEDC) .OR.   &
       (HCLOUD(1:3) == 'ICE' .AND. LSEDIC .AND. .NOT. LRED) .OR. &
       (HCLOUD(1:3) == 'ICE' .AND. LSEDIC .AND. LRED .AND. LSEDIM_AFTER)) &
       IPROACTV(NBUDGET_RC, IPROC) = NSEDIRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'DEPO_'
    IF (((HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO').AND. LDEPOC) .OR.   &
       (HCLOUD(1:3) == 'ICE' .AND. LDEPOSC)) IPROACTV(NBUDGET_RC, IPROC) = NDEPORC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'CDEPI_'
    IF (HCLOUD(1:3) == 'ICE' .AND. (.NOT. LRED .OR. (LRED .AND. LADJ_AFTER) )) &
            IPROACTV(NBUDGET_RC, IPROC) = NCDEPIRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'COND_'
    IF (HCLOUD == 'C2R2'.OR. HCLOUD == 'KHKO' .OR. &
        HCLOUD(1:3) == 'KES'.OR. HCLOUD == 'REVE') IPROACTV(NBUDGET_RC, IPROC) = NCONDRC

    IPROC=IPROC+1
    YWORK2(NBUDGET_RC, IPROC) = 'NECON_'
    IF ( (HCLOUD == 'KHKO' )  .OR.  (HCLOUD == 'C2R2')) &
       IPROACTV(NBUDGET_RC, IPROC) = NNECONRC
  END IF

  YEND_COMMENT(NBUDGET_RC) = 'BU_RRC'
  NBUPROCNBR(NBUDGET_RC) = 3
!
  CBUACTION(NBUDGET_RC, 1) = 'IG'
  CBUACTION(NBUDGET_RC, 2) = 'CC'
  CBUACTION(NBUDGET_RC, 3) = 'ES'
!
  DO JJ=1,3
    CBUCOMMENT(NBUDGET_RC, JJ) = ADJUSTL( ADJUSTR( YWORK2(NBUDGET_RC, JJ) ) // &
                                 ADJUSTL( YEND_COMMENT(NBUDGET_RC) ) )
  END DO
!
END IF
!
!                        Budget of RRR
IF (LBU_RRR) THEN
  YWORK2(NBUDGET_RR, 1) = 'INIF_'

  YWORK2(NBUDGET_RR, 2) = 'ENDF_'

  YWORK2(NBUDGET_RR, 3) = 'AVEF_'

  IPROC=4
  YWORK2(NBUDGET_RR, IPROC) = 'ASSE_'
  IPROACTV(NBUDGET_RR, IPROC) = NASSERR

  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'NEST_'
  IF( NMODEL>1 ) IPROACTV(NBUDGET_RR, IPROC) = NNESTRR

  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'FRC_'
  IF( LFORCING ) IPROACTV(NBUDGET_RR, IPROC)  = NFRCRR

  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'DIF_'
  IF ( ONUMDIFTH ) IPROACTV(NBUDGET_RR, IPROC) = NDIFRR

  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'REL_'
  IF ( OHORELAX_RR ) THEN
    IPROACTV(NBUDGET_RR, IPROC) = NRELRR
  ELSE
    IF(OVE_RELAX .OR. OHORELAX_UVWTH .OR. OHORELAX_RV .OR.                 &
     OHORELAX_RC .OR. OHORELAX_RR .OR. OHORELAX_RI .OR. OHORELAX_RS .OR.   &
     OHORELAX_RG .OR. OHORELAX_RH .OR. OHORELAX_TKE .OR. ANY(OHORELAX_SV)) THEN
      IPROACTV(NBUDGET_RR, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_RR, IPROC) = 3
    END IF
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'ADV_'
  IPROACTV(NBUDGET_RR, IPROC) = NADVRR

  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'NEGA_'
  IF ( HCLOUD /= 'NONE' ) IPROACTV(NBUDGET_RR, IPROC) = NNEGARR

  IF (HCLOUD == 'LIMA') THEN
    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'CORR_'
    IF (OPTSPLIT .AND. OWARM .AND. ORAIN)              IPROACTV(NBUDGET_RR, IPROC) = NCORRRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'SEDI_'
    IF (OWARM .AND. ORAIN)                             IPROACTV(NBUDGET_RR, IPROC) = NSEDIRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'R2C1_'
    IF (OPTSPLIT .AND. OWARM .AND. ORAIN)              IPROACTV(NBUDGET_RR, IPROC) = NR2C1RR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'AUTO_'
    IF (OPTSPLIT .OR. (OWARM .AND. ORAIN))             IPROACTV(NBUDGET_RR, IPROC) = NAUTORR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'ACCR_'
    IF (OPTSPLIT .OR. (OWARM .AND. ORAIN))             IPROACTV(NBUDGET_RR, IPROC) = NACCRRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'REVA_'
    IF (OPTSPLIT .OR. (OWARM .AND. ORAIN))             IPROACTV(NBUDGET_RR, IPROC) = NREVARR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'HONR_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. ONUCL .AND. ORAIN)) IPROACTV(NBUDGET_RR, IPROC) = NHONRRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'ACC_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW .AND. ORAIN)) IPROACTV(NBUDGET_RR, IPROC) = NACCRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'CFRZ_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RR, IPROC) = NCFRZRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'WETG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RR, IPROC) = NWETGRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'DRYG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RR, IPROC) = NDRYGRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'GMLT_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RR, IPROC) = NGMLTRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'CVRC_'
    IF (OPTSPLIT)                                      IPROACTV(NBUDGET_RR, IPROC) = NCVRCRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'WETH_'
    IF (.NOT.OPTSPLIT .AND. OHAIL)                     IPROACTV(NBUDGET_RR, IPROC) = NWETHRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'HMLT_'
    IF (.NOT.OPTSPLIT .AND. OHAIL)                     IPROACTV(NBUDGET_RR, IPROC) = NHMLTRR
  ELSE
    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'SEDI_'
    IF (HCLOUD(1:3) == 'KES' )   IPROACTV(NBUDGET_RR, IPROC) = NSEDIRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'ACCR_'
    IF (HCLOUD(1:3) == 'KES' )   IPROACTV(NBUDGET_RR, IPROC) = NACCRRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'AUTO_'
    IF (HCLOUD(1:3) == 'KES' )   IPROACTV(NBUDGET_RR, IPROC) = NAUTORR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'REVA_'
    IF (HCLOUD(1:3) == 'KES' )   IPROACTV(NBUDGET_RR, IPROC) = NREVARR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'SEDI_'
    IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. (.NOT. LSEDIM_AFTER)) &
            IPROACTV(NBUDGET_RR, IPROC) = NSEDIRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'SFR_'
    IF (HCLOUD(1:3) == 'ICE' ) IPROACTV(NBUDGET_RR, IPROC) = NSFRRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'AUTO_'
    IF ((HCLOUD(1:3) == 'ICE' ) .AND. LWARM) &
         IPROACTV(NBUDGET_RR, IPROC) = NAUTORR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'ACCR_'
    IF ((HCLOUD(1:3) == 'ICE' ) .AND. LWARM) &
         IPROACTV(NBUDGET_RR, IPROC) = NACCRRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'REVA_'
    IF ((HCLOUD(1:3) == 'ICE' ) .AND. LWARM) &
         IPROACTV(NBUDGET_RR, IPROC) = NREVARR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'ACC_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RR, IPROC) = NACCRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'CMEL_'
    IF (HCLOUD(1:3) == 'ICE' .AND. LRED) IPROACTV(NBUDGET_RR, IPROC) = NCMELRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'CFRZ_'
    IF (HCLOUD(1:3) == 'ICE' ) IPROACTV(NBUDGET_RR, IPROC) = NCFRZRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'WETG_'
    IF (HCLOUD(1:3) == 'ICE' ) IPROACTV(NBUDGET_RR, IPROC) = NWETGRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'DRYG_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RR, IPROC) = NDRYGRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'GMLT_'
    IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RR, IPROC) = NGMLTRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'WETH_'
    IF (HCLOUD == 'ICE4') IPROACTV(NBUDGET_RR, IPROC) = NWETHRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'DRYH_'
    IF (HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RR, IPROC) = NDRYHRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'HMLT_'
    IF (HCLOUD == 'ICE4') IPROACTV(NBUDGET_RR, IPROC) = NHMLTRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'CORR_'
    IF (HCLOUD(1:3) == 'ICE' .AND. LRED) IPROACTV(NBUDGET_RR, IPROC) = NCORRRR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'AUTO_'
    IF ((HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO') .AND. LRAIN)  IPROACTV(NBUDGET_RR, IPROC) = NAUTORR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'ACCR_'
    IF ((HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO') .AND. LRAIN)  IPROACTV(NBUDGET_RR, IPROC) = NACCRRR
    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'REVA_'
    IF ((HCLOUD == 'C2R2'.OR. HCLOUD == 'KHKO') .AND. LRAIN)   IPROACTV(NBUDGET_RR, IPROC) = NREVARR

    IPROC=IPROC+1
    YWORK2(NBUDGET_RR, IPROC) = 'SEDI_'
    IF (HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO' .OR. &
    (HCLOUD(1:3) == 'ICE' .AND. .NOT. (LRED)) .OR. &
    (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LSEDIM_AFTER)) &
      IPROACTV(NBUDGET_RR, IPROC) = NSEDIRR
  END IF

  YEND_COMMENT(NBUDGET_RR) = 'BU_RRR'
  NBUPROCNBR(NBUDGET_RR) = 3
!
  CBUACTION(NBUDGET_RR, 1) = 'IG'
  CBUACTION(NBUDGET_RR, 2) = 'CC'
  CBUACTION(NBUDGET_RR, 3) = 'ES'
!
  DO JJ=1,3
    CBUCOMMENT(NBUDGET_RR, JJ) = ADJUSTL( ADJUSTR( YWORK2(NBUDGET_RR, JJ) ) // &
                                 ADJUSTL( YEND_COMMENT(NBUDGET_RR) ) )
  END DO
!
END IF
!
!                        Budget of RRI
IF (LBU_RRI) THEN
  YWORK2(NBUDGET_RI, 1) = 'INIF_'

  YWORK2(NBUDGET_RI, 2) = 'ENDF_'

  YWORK2(NBUDGET_RI, 3) = 'AVEF_'

  IPROC=4
  YWORK2(NBUDGET_RI, IPROC) = 'ASSE_'
  IPROACTV(NBUDGET_RI, IPROC) = NASSERI

  IPROC=IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'NEST_'
  IF( NMODEL>1 ) IPROACTV(NBUDGET_RI, IPROC) = NNESTRI

  IPROC=IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'FRC_'
  IF( LFORCING ) IPROACTV(NBUDGET_RI, IPROC)  = NFRCRI

  IPROC=IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'DIF_'
  IF( ONUMDIFTH ) IPROACTV(NBUDGET_RI, IPROC) = NDIFRI

  IPROC=IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'REL_'
  IF ( OHORELAX_RI ) THEN
    IPROACTV(NBUDGET_RI, IPROC) = NRELRI
  ELSE
    IF(OVE_RELAX .OR. OHORELAX_UVWTH .OR. OHORELAX_RV .OR.                 &
     OHORELAX_RC .OR. OHORELAX_RR .OR. OHORELAX_RI .OR. OHORELAX_RS .OR.   &
     OHORELAX_RG .OR. OHORELAX_RH .OR. OHORELAX_TKE .OR. ANY(OHORELAX_SV)) THEN
      IPROACTV(NBUDGET_RI, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_RI, IPROC) = 3
    END IF
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'DCONV_'
  IF( HDCONV /= 'NONE' .OR. HSCONV == 'KAFR') IPROACTV(NBUDGET_RI, IPROC) = NDCONVRI

  IPROC=IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'VTURB_'
  IF ( HTURB /=  'NONE' ) IPROACTV(NBUDGET_RI, IPROC) = NVTURBRI

  IPROC=IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'HTURB_'
  IF ( HTURB /= 'NONE' .AND. HTURBDIM == '3DIM' ) THEN
    IPROACTV(NBUDGET_RI, IPROC) = NHTURBRI
  ELSE 
    IF ( HTURB /= 'NONE' ) THEN
      IPROACTV(NBUDGET_RI, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_RI, IPROC) = 3
    END IF
  END IF 

  IPROC=IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'ADV_'
  IPROACTV(NBUDGET_RI, IPROC) = NADVRI

  IPROC=IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'NEGA_'
  IF( HCLOUD /= 'NONE' ) IPROACTV(NBUDGET_RI, IPROC) = NNEGARI

  IF (HCLOUD=='LIMA') THEN
    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'CORR_'
    IF (OPTSPLIT .AND. OCOLD .AND. OSNOW)                   IPROACTV(NBUDGET_RI, IPROC) = NCORRRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'SEDI_'
    IF (OCOLD .AND. OSEDI)                                  IPROACTV(NBUDGET_RI, IPROC) = NSEDIRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'HIND_'
    IF (OCOLD .AND. ONUCL)                                  IPROACTV(NBUDGET_RI, IPROC) = NHINDRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'HINC_'
    IF (OCOLD .AND. ONUCL)                                  IPROACTV(NBUDGET_RI, IPROC) = NHINCRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'HONH_'
    IF (OCOLD .AND. ONUCL .AND. OHHONI .AND. NMOD_CCN.GE.1) IPROACTV(NBUDGET_RI, IPROC) = NHONHRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'HONC_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. ONUCL))      IPROACTV(NBUDGET_RI, IPROC) = NHONCRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'CNVI_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))                  IPROACTV(NBUDGET_RI, IPROC) = NCNVIRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'CNVS_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))                  IPROACTV(NBUDGET_RI, IPROC) = NCNVSRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'AGGS_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))                  IPROACTV(NBUDGET_RI, IPROC) = NAGGSRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'IMLT_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM))                  IPROACTV(NBUDGET_RI, IPROC) = NIMLTRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'BERFI_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM))                  IPROACTV(NBUDGET_RI, IPROC) = NBERFIRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'HMS_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_RI, IPROC) = NHMSRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'CFRZ_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_RI, IPROC) = NCFRZRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'WETG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_RI, IPROC) = NWETGRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'DRYG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_RI, IPROC) = NDRYGRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'HMG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_RI, IPROC) = NHMGRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'WETH_'
    IF (.NOT.OPTSPLIT .AND. OHAIL) IPROACTV(NBUDGET_RI, IPROC) = NWETHRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'CEDS_'
                                      IPROACTV(NBUDGET_RI, IPROC) = NCEDSRI
  ELSE
    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'ADJU_'
    IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LADJ_BEFORE) IPROACTV(NBUDGET_RI, IPROC) = NADJURI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'SEDI_'
    IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. (.NOT. LSEDIM_AFTER)) &
            IPROACTV(NBUDGET_RI, IPROC) = NSEDIRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'HENU_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NHENURI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'HON_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NHONRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'AGGS_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NAGGSRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'AUTS_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NAUTSRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'CFRZ_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NCFRZRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'WETG_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NWETGRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'DRYG_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NDRYGRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'WETH_'
    IF( HCLOUD == 'ICE4' ) IPROACTV(NBUDGET_RI, IPROC) = NWETHRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'DRYH_'
    IF( HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RI, IPROC) = NDRYHRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'IMLT_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NIMLTRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'BERFI_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NBERFIRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'CORR_'
    IF( HCLOUD(1:3) == 'ICE' .AND. LRED) IPROACTV(NBUDGET_RI, IPROC) = NCORRRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'SEDI_'
    IF ((HCLOUD(1:3) == 'ICE' .AND. .NOT. LRED).OR. &
    (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LSEDIM_AFTER)) &
            IPROACTV(NBUDGET_RI, IPROC) = NSEDIRI

    IPROC=IPROC+1
    YWORK2(NBUDGET_RI, IPROC) = 'CDEPI_'
    IF (HCLOUD(1:3) == 'ICE' .AND. (.NOT. LRED .OR. (LRED .AND. LADJ_AFTER) )) &
            IPROACTV(NBUDGET_RI, IPROC) = NCDEPIRI
  END IF

  YEND_COMMENT(NBUDGET_RI) = 'BU_RRI'
  NBUPROCNBR(NBUDGET_RI) = 3
!
  CBUACTION(NBUDGET_RI, 1) = 'IG'
  CBUACTION(NBUDGET_RI, 2) = 'CC'
  CBUACTION(NBUDGET_RI, 3) = 'ES'
!
  DO JJ=1,3
    CBUCOMMENT(NBUDGET_RI, JJ) = ADJUSTL( ADJUSTR( YWORK2(NBUDGET_RI, JJ) ) // &
                                 ADJUSTL( YEND_COMMENT(NBUDGET_RI) ) )
  END DO
!
END IF
!
!                        Budget of RRS
IF (LBU_RRS) THEN
  YWORK2(NBUDGET_RS, 1) = 'INIF_'

  YWORK2(NBUDGET_RS, 2) = 'ENDF_'

  YWORK2(NBUDGET_RS, 3) = 'AVEF_'

  IPROC=4
  YWORK2(NBUDGET_RS, IPROC) = 'ASSE_'
  IPROACTV(NBUDGET_RS, IPROC) = NASSERS

  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'NEST_'
  IF( NMODEL>1 ) IPROACTV(NBUDGET_RS, IPROC) = NNESTRS

  IPROC=IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'FRC_'
  IF( LFORCING )  IPROACTV(NBUDGET_RS, IPROC)  = NFRCRS

  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'DIF_'
  IF( ONUMDIFTH ) IPROACTV(NBUDGET_RS, IPROC) = NDIFRS

  IPROC=IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'REL_'
  IF ( OHORELAX_RS ) THEN
    IPROACTV(NBUDGET_RS, IPROC) = NRELRS
  ELSE
    IF(OVE_RELAX .OR. OHORELAX_UVWTH .OR. OHORELAX_RV .OR.                 &
     OHORELAX_RC .OR. OHORELAX_RR .OR. OHORELAX_RI .OR. OHORELAX_RS .OR.   &
     OHORELAX_RG .OR. OHORELAX_RH .OR. OHORELAX_TKE .OR. ANY(OHORELAX_SV)) THEN
      IPROACTV(NBUDGET_RS, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_RS, IPROC) = 3
    END IF
  END IF

  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'ADV_'
  IPROACTV(NBUDGET_RS, IPROC) = NADVRS

  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'NEGA_'
  IF( HCLOUD /= 'NONE' ) IPROACTV(NBUDGET_RS, IPROC) = NNEGARS

  IF (HCLOUD=='LIMA') THEN
    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'CORR_'
    IF (OPTSPLIT .AND. OCOLD .AND. OSNOW)              IPROACTV(NBUDGET_RS, IPROC) = NCORRRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'SEDI_'
    IF (OCOLD .AND. OSNOW)                             IPROACTV(NBUDGET_RS, IPROC) = NSEDIRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'CNVI_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))             IPROACTV(NBUDGET_RS, IPROC) = NCNVIRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'DEPS_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))             IPROACTV(NBUDGET_RS, IPROC) = NDEPSRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'CNVS_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))             IPROACTV(NBUDGET_RS, IPROC) = NCNVSRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'AGGS_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))             IPROACTV(NBUDGET_RS, IPROC) = NAGGSRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'RIM_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RS, IPROC) = NRIMRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'HMS_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RS, IPROC) = NHMSRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'ACC_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW .AND. ORAIN)) IPROACTV(NBUDGET_RS, IPROC) = NACCRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'CMEL_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RS, IPROC) = NCMELRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'WETG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RS, IPROC) = NWETGRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'DRYG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RS, IPROC) = NDRYGRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'WETH_'
    IF (.NOT.OPTSPLIT .AND. OHAIL)                     IPROACTV(NBUDGET_RS, IPROC) = NWETHRS
  ELSE
    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'SEDI_'
    IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. (.NOT. LSEDIM_AFTER)) &
            IPROACTV(NBUDGET_RS, IPROC) = NSEDIRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'DEPS_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NDEPSRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'AGGS_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NAGGSRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'AUTS_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NAUTSRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'RIM_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NRIMRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'ACC_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NACCRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'CMEL_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NCMELRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'WETG_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NWETGRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'DRYG_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NDRYGRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'WETH_'
    IF( HCLOUD == 'ICE4') IPROACTV(NBUDGET_RS, IPROC) = NWETHRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'DRYH_'
    IF( HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RS, IPROC) = NDRYHRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'CORR_'
    IF( HCLOUD(1:3) == 'ICE' .AND. LRED) IPROACTV(NBUDGET_RS, IPROC) = NCORRRS

    IPROC=IPROC+1
    YWORK2(NBUDGET_RS, IPROC) = 'SEDI_'
    IF ((HCLOUD(1:3) == 'ICE' .AND. .NOT. LRED).OR. &
       (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LSEDIM_AFTER)) &
            IPROACTV(NBUDGET_RS, IPROC) = NSEDIRS
  END IF

  YEND_COMMENT(NBUDGET_RS) = 'BU_RRS'
  NBUPROCNBR(NBUDGET_RS) = 3
!
  CBUACTION(NBUDGET_RS, 1) = 'IG'
  CBUACTION(NBUDGET_RS, 2) = 'CC'
  CBUACTION(NBUDGET_RS, 3) = 'ES'
!
  DO JJ=1,3
    CBUCOMMENT(NBUDGET_RS, JJ) = ADJUSTL( ADJUSTR( YWORK2(NBUDGET_RS, JJ) ) // &
                                 ADJUSTL( YEND_COMMENT(NBUDGET_RS) ) )
  END DO
!
END IF
!
!                        Budget of RRG
IF (LBU_RRG) THEN
  YWORK2(NBUDGET_RG, 1) = 'INIF_'

  YWORK2(NBUDGET_RG, 2) = 'ENDF_'

  YWORK2(NBUDGET_RG, 3) = 'AVEF_'

  IPROC=4
  YWORK2(NBUDGET_RG, IPROC) = 'ASSE_'
  IPROACTV(NBUDGET_RG, IPROC) = NASSERG

  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'NEST_'
  IF( NMODEL>1 ) IPROACTV(NBUDGET_RG, IPROC) = NNESTRG

  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'FRC_'
  IF( LFORCING ) IPROACTV(NBUDGET_RG, IPROC)  = NFRCRG

  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'DIF_'
  IF( ONUMDIFTH ) IPROACTV(NBUDGET_RG, IPROC) = NDIFRG

  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'REL_'
  IF ( OHORELAX_RG ) THEN
    IPROACTV(NBUDGET_RG, IPROC) = NRELRG
  ELSE
    IF(OVE_RELAX .OR. OHORELAX_UVWTH .OR. OHORELAX_RV .OR.                 &
     OHORELAX_RC .OR. OHORELAX_RR .OR. OHORELAX_RI .OR. OHORELAX_RS .OR.   &
     OHORELAX_RG .OR. OHORELAX_RH .OR. OHORELAX_TKE .OR. ANY(OHORELAX_SV)) THEN
      IPROACTV(NBUDGET_RG, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_RG, IPROC) = 3
    END IF
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'ADV_'
  IPROACTV(NBUDGET_RG, IPROC) = NADVRG

  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'NEGA_'
  IF( HCLOUD /= 'NONE'  ) IPROACTV(NBUDGET_RG, IPROC) = NNEGARG

  IF (HCLOUD=='LIMA') THEN
    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'SEDI_'
    IF (OCOLD .AND. OSNOW)                                         IPROACTV(NBUDGET_RG, IPROC) = NSEDIRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'HONR_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. ONUCL .AND. ORAIN)) IPROACTV(NBUDGET_RG, IPROC) = NHONRRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'DEPG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NDEPGRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'RIM_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NRIMRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'ACC_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. ORAIN .AND. OSNOW)) IPROACTV(NBUDGET_RG, IPROC) = NACCRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'CMEL_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NCMELRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'CFRZ_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NCFRZRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'WETG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NWETGRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'DRYG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NDRYGRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'HMG_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NHMGRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'GMLT_'
    IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NGMLTRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'WETH_'
    IF (.NOT.OPTSPLIT .AND. OHAIL)                                 IPROACTV(NBUDGET_RG, IPROC) = NWETHRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'COHG_'
    IF (.NOT.OPTSPLIT .AND. OHAIL)                                 IPROACTV(NBUDGET_RG, IPROC) = NCOHGRG
  ELSE
    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'SEDI_'
    IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. (.NOT. LSEDIM_AFTER)) &
            IPROACTV(NBUDGET_RG, IPROC) = NSEDIRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC)= 'SFR_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NSFRRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'DEPG_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NDEPGRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'RIM_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NRIMRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'ACC_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NACCRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'CMEL_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NCMELRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'CFRZ_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NCFRZRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'WETG_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NWETGRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'GHCV_'
    IF( HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RG, IPROC) = NGHCVRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'DRYG_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NDRYGRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'GMLT_'
    IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NGMLTRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'WETH_'
    IF( HCLOUD == 'ICE4' .AND. .NOT. LRED ) IPROACTV(NBUDGET_RG, IPROC) = NWETHRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'HGCV_'
    IF( HCLOUD == 'ICE4' .AND. LRED ) IPROACTV(NBUDGET_RG, IPROC) = NHGCVRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'DRYH_'
    IF( HCLOUD == 'ICE4'  .AND. LRED) IPROACTV(NBUDGET_RG, IPROC) = NDRYHRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'CORR_'
    IF( HCLOUD(1:3) == 'ICE'  .AND. LRED) IPROACTV(NBUDGET_RG, IPROC) = NCORRRG

    IPROC=IPROC+1
    YWORK2(NBUDGET_RG, IPROC) = 'SEDI_'
    IF ((HCLOUD(1:3) == 'ICE' .AND. .NOT. LRED).OR. &
       (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LSEDIM_AFTER)) &
            IPROACTV(NBUDGET_RG, IPROC) = NSEDIRG
  END IF

  YEND_COMMENT(NBUDGET_RG) = 'BU_RRG'
  NBUPROCNBR(NBUDGET_RG) = 3
!
  CBUACTION(NBUDGET_RG, 1) = 'IG'
  CBUACTION(NBUDGET_RG, 2) = 'CC'
  CBUACTION(NBUDGET_RG, 3) = 'ES'
!
  DO JJ=1,3
    CBUCOMMENT(NBUDGET_RG, JJ) = ADJUSTL( ADJUSTR( YWORK2(NBUDGET_RG, JJ) ) // &
                                 ADJUSTL( YEND_COMMENT(NBUDGET_RG) ) )
  END DO
!
END IF
!
!                        Budget of RRH
IF (LBU_RRH) THEN
  YWORK2(NBUDGET_RH, 1) = 'INIF_'

  YWORK2(NBUDGET_RH, 2) = 'ENDF_'

  YWORK2(NBUDGET_RH, 3) = 'AVEF_'

  IPROC=4
  YWORK2(NBUDGET_RH, IPROC) = 'ASSE_'
  IPROACTV(NBUDGET_RH, IPROC) = NASSERH

  IPROC=IPROC+1
  YWORK2(NBUDGET_RH, IPROC) = 'NEST_'
  IF( NMODEL>1 ) THEN
    IPROACTV(NBUDGET_RH, IPROC) = NNESTRH
  ELSE
    IPROACTV(NBUDGET_RH, IPROC) = 3
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_RH, IPROC) = 'FRC_'
   IF( LFORCING ) THEN
    IPROACTV(NBUDGET_RH, IPROC)  = NFRCRH
  ELSE
    IPROACTV(NBUDGET_RH, IPROC)  = 3
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_RH, IPROC) = 'DIF_'
  IF( ONUMDIFTH ) THEN
    IPROACTV(NBUDGET_RH, IPROC) = NDIFRH
  ELSE
    IPROACTV(NBUDGET_RH, IPROC) = 3
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_RH, IPROC) = 'REL_'
  IF ( OHORELAX_RH ) THEN
    IPROACTV(NBUDGET_RH, IPROC) = NRELRH
  ELSE
    IF(OVE_RELAX .OR. OHORELAX_UVWTH .OR. OHORELAX_RV .OR.                 &
     OHORELAX_RC .OR. OHORELAX_RR .OR. OHORELAX_RI .OR. OHORELAX_RS .OR.   &
     OHORELAX_RG .OR. OHORELAX_RH .OR. OHORELAX_TKE .OR. ANY(OHORELAX_SV)) THEN
      IPROACTV(NBUDGET_RH, IPROC) = 4
    ELSE
      IPROACTV(NBUDGET_RH, IPROC) = 3
    END IF
  END IF

  IPROC=IPROC+1
  YWORK2(NBUDGET_RH, IPROC) = 'ADV_'
  IPROACTV(NBUDGET_RH, IPROC) = NADVRH

  IPROC=IPROC+1
  YWORK2(NBUDGET_RH, IPROC) = 'NEGA_'
  IF( HCLOUD /= 'NONE' ) THEN
    IPROACTV(NBUDGET_RH, IPROC) = NNEGARH
  ELSE
    IPROACTV(NBUDGET_RH, IPROC) = 3
  END IF
!
  IF (HCLOUD=='LIMA' .AND. OHAIL) THEN
    IPROC=IPROC+1
    YWORK2(NBUDGET_RH, IPROC) = 'SEDI_'
    IPROACTV(NBUDGET_RH, IPROC) = NSEDIRH

    IPROC=IPROC+1
    YWORK2(NBUDGET_RH, IPROC) = 'WETG_'
    IPROACTV(NBUDGET_RH, IPROC) = NWETGRH
    IPROC=IPROC+1
    YWORK2(NBUDGET_RH, IPROC) = 'WETH_'
    IF (.NOT.OPTSPLIT) IPROACTV(NBUDGET_RH, IPROC) = NWETHRH

    IPROC=IPROC+1
    YWORK2(NBUDGET_RH, IPROC) = 'COHG_'
    IF (.NOT.OPTSPLIT) IPROACTV(NBUDGET_RH, IPROC) = NCOHGRH

    IPROC=IPROC+1
    YWORK2(NBUDGET_RH, IPROC) = 'HMLT_'
    IF (.NOT.OPTSPLIT) IPROACTV(NBUDGET_RH, IPROC) = NHMLTRH
  ELSE
    IPROC=IPROC+1
    YWORK2(NBUDGET_RH, IPROC) = 'SEDI_'
    IF( HCLOUD == 'ICE4' .AND. LRED .AND. .NOT. LSEDIM_AFTER) &
            IPROACTV(NBUDGET_RH, IPROC) = NSEDIRH

    IPROC=IPROC+1
    YWORK2(NBUDGET_RH, IPROC) = 'GHCV_'
    IF( HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RH, IPROC) = NGHCVRH

    IPROC=IPROC+1
    YWORK2(NBUDGET_RH, IPROC) = 'WETG_'
    IF( HCLOUD == 'ICE4' .AND. .NOT. LRED) IPROACTV(NBUDGET_RH, IPROC) = NWETGRH

    IPROC=IPROC+1
    YWORK2(NBUDGET_RH, IPROC) = 'WETH_'
    IF( HCLOUD == 'ICE4') IPROACTV(NBUDGET_RH, IPROC) = NWETHRH

    IPROC=IPROC+1
    YWORK2(NBUDGET_RH, IPROC) = 'HGCV_'
    IF( HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RH, IPROC) = NHGCVRH

    IPROC=IPROC+1
    YWORK2(NBUDGET_RH, IPROC) = 'DRYH_'
    IF( HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RH, IPROC) = NDRYHRH

    IPROC=IPROC+1
    YWORK2(NBUDGET_RH, IPROC) = 'HMLT_'
    IF( HCLOUD == 'ICE4' ) IPROACTV(NBUDGET_RH, IPROC) = NHMLTRH

    IPROC=IPROC+1
    YWORK2(NBUDGET_RH, IPROC) = 'CORR_'
    IF( HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RH, IPROC) = NCORRRH

    IPROC=IPROC+1
    YWORK2(NBUDGET_RH, IPROC) = 'SEDI_'
    IF ((HCLOUD(1:3) == 'ICE' .AND. .NOT. LRED).OR. &
       (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LSEDIM_AFTER)) &
            IPROACTV(NBUDGET_RH, IPROC) = NSEDIRH
  END IF

  YEND_COMMENT(NBUDGET_RH) = 'BU_RRH'
  NBUPROCNBR(NBUDGET_RH) = 3
!
  CBUACTION(NBUDGET_RH, 1) = 'IG'
  CBUACTION(NBUDGET_RH, 2) = 'CC'
  CBUACTION(NBUDGET_RH, 3) = 'ES'
!  
  DO JJ=1,3
    CBUCOMMENT(NBUDGET_RH, JJ) = ADJUSTL( ADJUSTR( YWORK2(NBUDGET_RH, JJ) ) // &
                                 ADJUSTL( YEND_COMMENT(NBUDGET_RH) ) )
  END DO
!

END IF
!
!                        Budget of RSV
IF (LBU_RSV) THEN
  IBUPROCNBR_SV_MAX = 0 ! initialize the Max nunmber of processes for the SVs
  DO JSV = 1,KSV
    YWORK2(NBUDGET_SV1 - 1 + JSV, 1) = 'INIF_'

    YWORK2(NBUDGET_SV1 - 1 + JSV, 2) = 'ENDF_'

    YWORK2(NBUDGET_SV1 - 1 + JSV, 3) = 'AVEF_'

    IPROC=4
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'ASSE_'
    IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = NASSESV

    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'NEST_'
    IF( NMODEL>1 ) IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = NNESTSV

    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'FRC_'
    IF( LFORCING ) IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC)  = NFRCSV

    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'DIF_'
    IF ( ONUMDIFSV ) IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = NDIFSV

    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'REL_'
    IF ( OHORELAX_SV(JSV) ) THEN
      IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = NRELSV
    ELSE
    IF(OVE_RELAX .OR. OHORELAX_UVWTH .OR. OHORELAX_RV .OR.                 &
     OHORELAX_RC .OR. OHORELAX_RR .OR. OHORELAX_RI .OR. OHORELAX_RS .OR.   &
     OHORELAX_RG .OR. OHORELAX_RH .OR. OHORELAX_TKE .OR. ANY(OHORELAX_SV)) THEN
        IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = 4
      ELSE
        IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = 3
      END IF
    END IF

    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'DCONV_'
    IF ( (HDCONV /= 'NONE' .OR. HSCONV == 'KAFR') .AND. OCHTRANS ) &
        IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = NDCONVSV

    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'DEPOTR'
    IF ( ODRAGTREE .AND. ODEPOTREE  ) &
        IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = NDEPOTRSV

    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'VTURB_'
    IF ( HTURB /= 'NONE' ) IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = NVTURBSV

    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'HTURB_'
    IF ( HTURB /= 'NONE' .AND. HTURBDIM == '3DIM' ) THEN
      IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = NHTURBSV
    ELSE
      IF ( HTURB /= 'NONE' ) THEN
        IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = 4
      ELSE
        IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = 3
      END IF
    END IF

    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'MAFL_'
    IF ( HSCONV == 'EDKF' ) IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC)= NMAFLSV

    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'ADV_'
    IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC)= NADVSV
!
!
! complete with the budget of other processes
!
    IPROC=IPROC+1
    ILAST_PROC_NBR = IPROC
    CALL BUDGET_OTHERPROC_SV
!
    YEND_COMMENT(NBUDGET_SV1 - 1 + JSV) = 'BU_RSV'
    IBUPROCNBR_SV_MAX   = MAX( IBUPROCNBR_SV_MAX, ILAST_PROC_NBR )
    NBUPROCNBR(NBUDGET_SV1 - 1 + JSV) = 3
!
    CBUACTION(NBUDGET_SV1 - 1 + JSV, 1) = 'IG'
    CBUACTION(NBUDGET_SV1 - 1 + JSV, 2) = 'CC'
    CBUACTION(NBUDGET_SV1 - 1 + JSV, 3) = 'ES'
!
    DO JJ=1,3
      CBUCOMMENT(NBUDGET_SV1 - 1 + JSV, JJ) = ADJUSTL( ADJUSTR( YWORK2(NBUDGET_SV1 - 1 + JSV, JJ) ) // &
                                              ADJUSTL( YEND_COMMENT(NBUDGET_SV1 - 1 + JSV) ) )
    END DO
  END DO
!
END IF

!-------------------------------------------------------------------------------
!*       4.    COMPUTE THE INCREMENT BETWEEN TWO ACTIVE SOURCES 
!              ------------------------------------------------
!
NBUINC(:,:) = 1
DO JI = 1, JPBUMAX
  DO JJ = 4, JPBUPROMAX-1
    DO JK = JJ+1,JPBUPROMAX
      IF ( IPROACTV(JI,JK) /= 3 ) EXIT
      NBUINC(JI,JJ) = NBUINC(JI,JJ) +1
    END DO 
  END DO
END DO
!
!-------------------------------------------------------------------------------
!*       5.    COMPUTE PROCESSES ACTIONS AND NAMES OF BUDGET OUTPUT ARRAYS 
!              -----------------------------------------------------------
!
!
DO JI=1,JPBUMAX                                ! loop on the allowed budgets names of recording files
  IF (IPROACTV(JI,4) >= 2) THEN
    WRITE(UNIT=KLUOUT,FMT= '("Error in budget specification of ",A7,/," &
    & The first source either is the first element of a group of sources or &
    & is not considered")')  YEND_COMMENT(JI)
    WRITE(UNIT=KLUOUT,FMT= '("change this namelist element ")')  
    GERROR = .TRUE.
  END IF
!    
  DO JJ=4,JPBUPROMAX                           ! loop on the allowed processes
    IF (IPROACTV(JI,JJ) == 0) THEN
      IF(IPROACTV(JI,JJ+NBUINC(JI,JJ)) == 0) THEN
        CBUACTION(JI,JJ)='OF'
      ELSE IF (IPROACTV(JI,JJ+NBUINC(JI,JJ)) == 1) THEN
        CBUACTION(JI,JJ)='CC'
      ELSE IF (IPROACTV(JI,JJ+NBUINC(JI,JJ)) == 2) THEN
        WRITE(UNIT=KLUOUT,FMT= '("Error in budget specification of ",A15)') &
        ADJUSTL( ADJUSTR(YWORK2(JI,JJ+NBUINC(JI,JJ)))//ADJUSTL(YEND_COMMENT(JI)))
        WRITE(UNIT=KLUOUT,FMT= '("change this namelist ")')
        GERROR = .TRUE.
      END IF
    ELSE IF (IPROACTV(JI,JJ) <= 2) THEN
      DO JJJ = JJ+NBUINC(JI,JJ), JPBUPROMAX
         IF(IPROACTV(JI,JJJ) /= 3 .AND. IPROACTV(JI,JJJ) /= 4) EXIT
      END DO
!
      IF (IPROACTV(JI,JJJ) == 1) THEN
        NBUPROCNBR(JI) = NBUPROCNBR(JI)+1
        CBUACTION(JI,JJ) = 'DC'
        CBUCOMMENT(JI,NBUPROCNBR(JI)) =                            ADJUSTL(    &
                                   ADJUSTR( CBUCOMMENT(JI,NBUPROCNBR(JI)) ) // &
                                   ADJUSTL( ADJUSTR( YWORK2(JI,JJ) ) //        &
                                            ADJUSTL( YEND_COMMENT(JI) ) ) )
      ELSE IF (IPROACTV(JI,JJJ) == 0) THEN
        NBUPROCNBR(JI) = NBUPROCNBR(JI)+1
        CBUACTION(JI,JJ) = 'DD'
        CBUCOMMENT(JI,NBUPROCNBR(JI)) =                            ADJUSTL(    &
                                   ADJUSTR( CBUCOMMENT(JI,NBUPROCNBR(JI)) ) // &
                                   ADJUSTL( ADJUSTR( YWORK2(JI,JJ) ) //        &
                                            ADJUSTL( YEND_COMMENT(JI) ) ) )
      ELSE IF (IPROACTV(JI,JJJ) == 2) THEN
        CBUACTION(JI,JJ) = 'NO'
        CBUCOMMENT(JI,NBUPROCNBR(JI)+1) =           ADJUSTL(                   &
                                  ADJUSTR( CBUCOMMENT(JI,NBUPROCNBR(JI)+1)) // &
                                  ADJUSTL( YWORK2(JI,JJ) ) )
      END IF
    ELSEIF (IPROACTV(JI,JJ) == 3) THEN
      CBUACTION(JI,JJ) = 'RM'
    ELSEIF (IPROACTV(JI,JJ) == 4) THEN
      CBUACTION(JI,JJ) = 'OF'
    ELSE
      WRITE(UNIT=KLUOUT,FMT= '("Error in budget specification of ",A7)') &
            YEND_COMMENT(JI)
      WRITE(UNIT=KLUOUT,FMT= '("change this namelist ")')
      GERROR = .TRUE.
    END IF
  END DO
END DO
!   writes on output the explicit chain of sources for all the budgets
DO JI=1,JPBUMAX                            ! loop over the allowed budgets
  YSTRING = ADJUSTL( YEND_COMMENT(JI) )
  ILEN    = LEN_TRIM(YSTRING)
  IF( ILEN /= 0 ) THEN
    IF( JI < NBUDGET_SV1 ) THEN
      WRITE (UNIT=KLUOUT,FMT='(/,"budget ",A7," with ",I2," vectors")')        &
                                                  YSTRING(1:ILEN),NBUPROCNBR(JI)
      DO JJ=1,3
        YSTRING = CBUCOMMENT(JI,JJ)
        ILEN    = LEN_TRIM(YSTRING)
        WRITE (UNIT=KLUOUT,FMT='(12X,A40)')         YSTRING(1:ILEN)
      END DO
      DO JJ=4,NBUPROCNBR(JI)                  ! loop over the allowed processes
        YSTRING = CBUCOMMENT(JI,JJ)
        ILEN    = LEN_TRIM(YSTRING)
        WRITE (UNIT=KLUOUT,FMT='(20X,A40)')         YSTRING(1:ILEN)
      END DO
    ELSE
      WRITE (UNIT=KLUOUT,                                                      &
             FMT='(/,"budget ",A7," (number ",I3,") with ",I2," vectors")')    &
                                   YSTRING(1:ILEN),JI-NBUDGET_SV1+1,NBUPROCNBR(JI)
      DO JJ=1,3
        YSTRING = CBUCOMMENT(JI,JJ)
        ILEN    = LEN_TRIM(YSTRING)
        WRITE (UNIT=KLUOUT,FMT='(12X,A40)')         YSTRING(1:ILEN)
      END DO
      DO JJ=4,NBUPROCNBR(JI)                  ! loop over the allowed processes
        YSTRING = CBUCOMMENT(JI,JJ)
        ILEN    = LEN_TRIM(YSTRING)
        WRITE (UNIT=KLUOUT,FMT='(20X,A40)')         YSTRING(1:ILEN)
      END DO
    END IF
  END IF
END DO
!
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
  WRITE(UNIT=KLUOUT, FMT= '("BUWRNB = ",I4.4)' ) NBUWRNB
  WRITE(UNIT=KLUOUT, FMT= '("BUMASK = ",I4.4)' ) NBUMASK
END IF
IF (GERROR) THEN
  call Print_msg( NVERB_FATAL, 'BUD', 'INI_BUDGET', '' )
ENDIF
!-------------------------------------------------------------------------------
!*       5.    ALLOCATE MEMORY FOR BUDGET STORAGE ARRAYS
!              -----------------------------------------
IF (LBU_RU) THEN
  ALLOCATE ( XBURU(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(NBUDGET_U)) )
  XBURU(:,:,:,:)=0.
  ALLOCATE ( XBURHODJU(IBUDIM1, IBUDIM2, IBUDIM3) )
  XBURHODJU(:,:,:)=0.
END IF
!
IF (LBU_RV) THEN
  ALLOCATE ( XBURV(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(NBUDGET_V)) )
  XBURV(:,:,:,:)=0.
  ALLOCATE ( XBURHODJV(IBUDIM1, IBUDIM2, IBUDIM3) )
  XBURHODJV(:,:,:)=0.
END IF
!
IF (LBU_RW) THEN
  ALLOCATE ( XBURW(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(NBUDGET_W)) )
  XBURW(:,:,:,:)=0.
  ALLOCATE ( XBURHODJW(IBUDIM1, IBUDIM2, IBUDIM3) )
  XBURHODJW(:,:,:)=0.
END IF
!
IF (LBU_RTH .OR. LBU_RTKE .OR. LBU_RRV .OR. LBU_RRC .OR. LBU_RRR .OR. &
    LBU_RRI .OR. LBU_RRS  .OR. LBU_RRG .OR. LBU_RRH .OR. LBU_RSV      ) THEN
  ALLOCATE ( XBURHODJ(IBUDIM1, IBUDIM2, IBUDIM3) )
  XBURHODJ(:,:,:)=0.
END IF
!
IF (LBU_RTH) THEN
  ALLOCATE ( XBURTH(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(NBUDGET_TH)) )
  XBURTH(:,:,:,:)=0.
END IF
!
IF (LBU_RTKE) THEN
  ALLOCATE ( XBURTKE(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(NBUDGET_TKE)) )
  XBURTKE(:,:,:,:)=0.
END IF
!
IF (LBU_RRV) THEN
  ALLOCATE ( XBURRV(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(NBUDGET_RV)) )
  XBURRV(:,:,:,:)=0.
END IF
!
IF (LBU_RRC) THEN
  ALLOCATE ( XBURRC(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(NBUDGET_RC)) )
  XBURRC(:,:,:,:)=0.
END IF
!
IF (LBU_RRR) THEN
  ALLOCATE ( XBURRR(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(NBUDGET_RR)) )
  XBURRR(:,:,:,:)=0.
END IF
!
IF (LBU_RRI) THEN
  ALLOCATE ( XBURRI(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(NBUDGET_RI)) )
  XBURRI(:,:,:,:)=0.
END IF
!
IF (LBU_RRS) THEN
  ALLOCATE ( XBURRS(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(NBUDGET_RS)) )
  XBURRS(:,:,:,:)=0.
END IF
!
IF (LBU_RRG) THEN
  ALLOCATE ( XBURRG(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(NBUDGET_RG)) )
  XBURRG(:,:,:,:)=0.
END IF
!
IF (LBU_RRH) THEN
  ALLOCATE ( XBURRH(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(NBUDGET_RH)) )
  XBURRH(:,:,:,:)=0.
END IF
!
IF (LBU_RSV) THEN
  ALLOCATE ( XBURSV(IBUDIM1, IBUDIM2, IBUDIM3, IBUPROCNBR_SV_MAX, KSV) )
  XBURSV(:,:,:,:,:)=0.
END IF
!
CONTAINS
! ##############################
  SUBROUTINE BUDGET_OTHERPROC_SV
! ##############################
!
!
USE MODD_NSV
USE MODD_PARAM_LIMA, ONLY : NMOD_CCN, NMOD_IFN, NMOD_IMM
!
  IF (JSV <= NSV_USER) THEN
    ! NSV_USER Case
!   SELECT CASE(JSV)
!   CASE (1)
!     ILAST_PROC_NBR = ILAST_PROC_NBR + 1
!     YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'PROC1_'
!     IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
!     ILAST_PROC_NBR = ILAST_PROC_NBR + 1
!     YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'PROC2_'
!     IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
!   CASE (2)
!     ILAST_PROC_NBR = ILAST_PROC_NBR + 1
!     YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'PROC3_'
!     IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
!     ILAST_PROC_NBR = ILAST_PROC_NBR + 1
!     YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'PROC4_'
!     IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
!   END SELECT
    !
  ELSEIF (JSV >= NSV_C2R2BEG .AND. JSV <= NSV_C2R2END) THEN  
    ! C2R2 or KHKO Case
    SELECT CASE(JSV-NSV_C2R2BEG+1)
    CASE (1)                               ! Concentration of activated nuclei
     IF (.NOT. LSUPSAT) THEN
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HENU_'
     END IF
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CEVA_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
    CASE (2)                               ! Concentration of cloud droplets
     IF (.NOT. LSUPSAT) THEN
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HENU_'
     END IF
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'SELF_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'ACCR_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      IF (LSEDC) THEN
        ILAST_PROC_NBR = ILAST_PROC_NBR + 1
        YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'SEDI_'
        IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (LDEPOC) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'DEPO_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CEVA_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
    CASE (3)                               ! Concentration of raindrops
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'AUTO_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      IF (HCLOUD /= 'KHKO') THEN
       ILAST_PROC_NBR = ILAST_PROC_NBR + 1
       YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'SCBU_'
       IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CEVA_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'BRKU_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'SEDI_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
    END SELECT
    !
ELSEIF (JSV >= NSV_LIMA_BEG .AND. JSV <= NSV_LIMA_END) THEN  
   ! LIMA case
   IF (JSV == NSV_LIMA_NC) THEN
      ! Cloud droplets conc.
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'NEGA_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      IF (OPTSPLIT .AND. OWARM .AND. ORAIN) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CORR_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OSEDC) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'SEDI_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (ODEPOC) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'DEPO_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .AND. ORAIN) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'R2C1_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OWARM .AND. OACTI .AND. NMOD_CCN.GE.1) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HENU_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OCOLD .AND. ONUCL .AND. OPTSPLIT) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HINC_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OWARM .AND. ORAIN)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'SELF_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. ORAIN) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'AUTO_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. ORAIN) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'ACCR_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. ORAIN) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'REVA_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OCOLD .AND. ONUCL .AND. .NOT.OPTSPLIT) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HINC_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. ONUCL)) THEN         
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HONC_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'IMLT_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'RIM_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'WETG_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'DRYG_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CVRC_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (.NOT.OPTSPLIT .AND. OHAIL) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'WETH_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CEDS_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      !
   ELSE IF (JSV == NSV_LIMA_NR) THEN
      ! Rain drops conc.
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'NEGA_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      IF (OPTSPLIT .AND. OWARM .AND. ORAIN) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CORR_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OWARM .AND. ORAIN) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'SEDI_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .AND. ORAIN) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'R2C1_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. ORAIN) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'AUTO_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. ORAIN) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'SCBU_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. ORAIN) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'REVA_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. ORAIN) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'BRKU_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. ORAIN .AND. ONUCL)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HONR_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'ACC_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CFRZ_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'WETG_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'DRYG_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'GMLT_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CVRC_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (.NOT.OPTSPLIT .AND. OHAIL) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'WETH_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (.NOT.OPTSPLIT .AND. OHAIL) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HMLT_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      !
   ELSE IF (JSV.GE.NSV_LIMA_CCN_FREE .AND. JSV.LT.(NSV_LIMA_CCN_FREE+NMOD_CCN)) THEN
      ! Free CCN conc.
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'NEGA_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      IF (OWARM .AND. OACTI .AND. NMOD_CCN.GE.1) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HENU_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OCOLD .AND. ONUCL .AND. OHHONI .AND. NMOD_CCN.GE.1) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HONH_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CEDS_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      IF (OSCAV) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'SCAV_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      !
   ELSE IF (JSV.GE.NSV_LIMA_CCN_ACTI .AND. JSV.LT.(NSV_LIMA_CCN_ACTI+NMOD_CCN)) THEN
      ! Activated CCN conc.

   ELSE IF (JSV == NSV_LIMA_NI) THEN
      ! Pristine ice crystals conc.
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'NEGA_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      IF (OPTSPLIT .AND. OCOLD .AND. OSNOW) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CORR_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OSEDI) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'SEDI_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OCOLD .AND. ONUCL) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HIND_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OCOLD .AND. ONUCL) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HINC_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OCOLD .AND. ONUCL .AND. OHHONI .AND. NMOD_CCN.GE.1) THEN         
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HONH_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. ONUCL)) THEN         
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HONC_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CNVI_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CNVS_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'AGGS_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'IMLT_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HMS_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CFRZ_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'WETG_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'DRYG_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HMG_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      IF (.NOT.OPTSPLIT .AND. OHAIL) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'WETH_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CEDS_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      !
   ELSE IF (JSV.GE.NSV_LIMA_IFN_FREE .AND. JSV.LT.(NSV_LIMA_IFN_FREE+NMOD_IFN)) THEN
      ! Free IFN conc.
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'NEGA_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      IF (OCOLD .AND. ONUCL .AND. (.NOT.OMEYERS)) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HIND_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CEDS_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      IF (OSCAV) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'SCAV_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF
      !
   ELSE IF (JSV.GE.NSV_LIMA_IFN_NUCL .AND. JSV.LT.(NSV_LIMA_IFN_NUCL+NMOD_IFN)) THEN
      ! Nucleated IFN conc.

   ELSE IF (JSV.GE.NSV_LIMA_IMM_NUCL .AND. JSV.LT.(NSV_LIMA_IMM_NUCL+NMOD_IMM)) THEN
      ! Nucleated IMM conc.

   ELSE IF (JSV == NSV_LIMA_HOM_HAZE) THEN
      ! Homogeneous freezing of CCN
      IF (OCOLD .AND. ONUCL .AND. OWARM .AND. OHHONI) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'HONH_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 1
      END IF

   END IF




ELSEIF (JSV >= NSV_ELECBEG .AND. JSV <= NSV_ELECEND) THEN
   SELECT CASE(JSV-NSV_ELECBEG+1)
   CASE(1)  ! volumetric charge of water vapor
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'DEPS_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NDEPSQV
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'DEPG_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NDEPGQV
      IF (LWARM) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'REVA_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NREVAQV
      END IF
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'DEPI_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NDEPIQV
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'NEUT_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NNEUTQV
   CASE(2)  ! volumetric charge of cloud droplets
      IF (LWARM) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'AUTO_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NAUTOQC
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'ACCR_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NACCRQC
      END IF
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'RIM_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NRIMQC
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'WETG_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NWETGQC
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'DRYG_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NDRYGQC
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'IMLT_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NIMLTQC
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'BERFI_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NBERFIQC
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'DEPI_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NDEPIQC
      IF (LINDUCTIVE) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'IND_'
      END IF
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NINDQC
      IF (LSEDIC) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'SEDI_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NSEDIQC
      END IF
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'NEUT_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NNEUTQC
   CASE(3)  ! volumetric charge of rain drops
      IF (LWARM) THEN
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'AUTO_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NAUTOQR
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'ACCR_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NACCRQR
         ILAST_PROC_NBR = ILAST_PROC_NBR + 1
         YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'REVA_'
         IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NREVAQR
      END IF
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'ACC_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NACCQR
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'CFRZ_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NCFRZQR
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'WETG_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NWETGQR
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'DRYG_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NDRYGQR
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'GMLT_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NGMLTQR
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'SEDI_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NSEDIQR
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'NEUT_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NNEUTQR
   CASE(4)  ! volumetric charge of ice crystals
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'AGGS_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NAGGSQI
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'AUTS_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NAUTSQI
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'CFRZ_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NCFRZQI
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'WETG_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NWETGQI
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'DRYG_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NDRYGQI
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'IMLT_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NIMLTQI
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'BERFI_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NBERFIQI
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'DEPI_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NDEPIQI
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'NIIS_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NNIISQI
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'SEDI_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NSEDIQI
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'NEUT_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NNEUTQI
   CASE(5)  ! volumetric charge of snow
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'DEPS_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NDEPSQS
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'AGGS_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NAGGSQS
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'AUTS_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NAUTSQS
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'RIM_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NRIMQS
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'ACC_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NACCQS
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'CMEL_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NCMELQS
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'WETG_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NWETGQS
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'DRYG_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NDRYGQS
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'NIIS_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NNIISQS
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'SEDI_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NSEDIQS
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'NEUT_'
   CASE(6)  ! volumetric charge of graupel
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'DEPG_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NDEPGQG
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'RIM_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NRIMQG
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'ACC_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NACCQG
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'CMEL_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NCMELQG
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'CFRZ_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NCFRZQG
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'WETG_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NWETGQG
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'DRYG_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NDRYGQG
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'GMLT_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NGMLTQG
      IF (LINDUCTIVE) THEN
        ILAST_PROC_NBR = ILAST_PROC_NBR + 1
        YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'IND_'
        IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NINDQG
      END IF
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'SEDI_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NSEDIQG
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = 'NEUT_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NNEUTQG
    CASE(7)  ! volumetric charge of hail
! add budget for hail volumetric charge
    END SELECT
!
ELSE IF (JSV >= NSV_CHEMBEG .AND. JSV <= NSV_CHEMEND) THEN
    ! Chemical Case
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'CHEM_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NCHEMSV
    ! other processes
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'ADV_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NADVSV
      ILAST_PROC_NBR = ILAST_PROC_NBR + 1
      YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'NEGA_'
      IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NNEGASV
 !
  ELSE
    ! other processes
    ! ILAST_PROC_NBR = ILAST_PROC_NBR + 1
    ! YWORK2(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR)= 'ADV_'
    ! IPROACTV(NBUDGET_SV1 - 1 + JSV, ILAST_PROC_NBR) = NADVSV
  END IF
  !
  END SUBROUTINE BUDGET_OTHERPROC_SV
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_BUDGET


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
