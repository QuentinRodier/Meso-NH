!MNH_LIC Copyright 1995-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ######################
      MODULE MODI_INI_BUDGET
!     ###################### 
INTERFACE
      SUBROUTINE INI_BUDGET(KLUOUT,PTSTEP,KSV,KRR,            &
      ONUMDIFU,ONUMDIFTH,ONUMDIFSV,                                   &
      OHORELAX_UVWTH,OHORELAX_RV,OHORELAX_RC,OHORELAX_RR,             &
      OHORELAX_RI,OHORELAX_RS, OHORELAX_RG, OHORELAX_RH,OHORELAX_TKE, & 
      OHORELAX_SV,OVE_RELAX,OCHTRANS,ONUDGING,ODRAGTREE,ODEPOTREE,    &
      HRAD,HDCONV,HSCONV,HTURB,HTURBDIM,HCLOUD                        )
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
CHARACTER (LEN=*), INTENT(IN) :: HSCONV ! type of the deep convection scheme
CHARACTER (LEN=*), INTENT(IN) :: HTURB  ! type of the turbulence scheme
CHARACTER (LEN=*), INTENT(IN) :: HTURBDIM! dimensionnality of the turbulence 
                                        ! scheme
CHARACTER (LEN=*), INTENT(IN) :: HCLOUD ! type of microphysical scheme
!
      END SUBROUTINE INI_BUDGET
!
END INTERFACE
!
END MODULE MODI_INI_BUDGET
!
!
!
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
INTEGER :: ITEN                                           ! tens for CBURECORD
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
ALLOCATE( CBURECORD(JPBUMAX, JPBUPROMAX) )
NBUPROCCTR(:) = 0
NBUCTR_ACTV(:) = 0
NBUPROCNBR(:) = 0
CBUACTION(:,:) = 'OF' 
CBURECORD(:,:) = ' '
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
  IPROC=4
  IPROACTV(NBUDGET_U, IPROC) = NASSEU
  IPROC=IPROC+1
  IF( NMODEL>1 ) IPROACTV(NBUDGET_U, IPROC) = NNESTU
  IPROC=IPROC+1
  IF( LFORCING ) IPROACTV(NBUDGET_U, IPROC)  = NFRCU
  IPROC=IPROC+1
  IF( ONUDGING ) IPROACTV(NBUDGET_U, IPROC)  = NNUDU
  IPROC=IPROC+1
  IF ( .NOT. LCARTESIAN ) THEN
    IPROACTV(NBUDGET_U, IPROC) = NCURVU
  ELSE
    IPROACTV(NBUDGET_U, IPROC) = 4
  END IF
  IPROC=IPROC+1
  IF ( LCORIO ) THEN  
    IPROACTV(NBUDGET_U, IPROC) = NCORU
  ELSE
    IPROACTV(NBUDGET_U, IPROC) = 4
  END IF
  IPROC=IPROC+1
  IF ( ONUMDIFU ) IPROACTV(NBUDGET_U, IPROC) = NDIFU
  IPROC=IPROC+1
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
  IF( ODRAGTREE ) IPROACTV(NBUDGET_U, IPROC)  = NDRAGU
  IPROC=IPROC+1
  IF ( HTURB /= 'NONE' ) IPROACTV(NBUDGET_U, IPROC) = NVTURBU
  IPROC=IPROC+1
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
  IF ( HSCONV == 'EDKF' ) IPROACTV(NBUDGET_U, IPROC) = NMAFLU
  IPROC=IPROC+1
  IPROACTV(NBUDGET_U, IPROC) = NADVU
  IPROC=IPROC+1
  IPROACTV(NBUDGET_U, IPROC) = NPRESU
!
  YWORK2(NBUDGET_U, 1) = 'INIF_'
  YWORK2(NBUDGET_U, 2) = 'ENDF_'
  YWORK2(NBUDGET_U, 3) = 'AVEF_'
  IPROC=4
  YWORK2(NBUDGET_U, IPROC) = 'ASSE_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'NEST_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'FRC_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'NUD_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'CURV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'COR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'DIF_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'REL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'DRAG_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'VTURB_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'HTURB_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'MAFL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'ADV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_U, IPROC) = 'PRES_'
!
  YEND_COMMENT(NBUDGET_U) = 'BU_RU'
  NBUPROCNBR(NBUDGET_U) = 3
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
  IPROC=4
  IPROACTV(NBUDGET_V, IPROC) = NASSEV
  IPROC=IPROC+1 
  IF( NMODEL>1 ) IPROACTV(NBUDGET_V, IPROC) = NNESTV
  IPROC=IPROC+1 
  IF( LFORCING ) IPROACTV(NBUDGET_V, IPROC)  = NFRCV
  IPROC=IPROC+1 
  IF( ONUDGING ) IPROACTV(NBUDGET_V, IPROC)  = NNUDV
  IPROC=IPROC+1
  IF ( .NOT. LCARTESIAN ) THEN
    IPROACTV(NBUDGET_V, IPROC) = NCURVV
  ELSE
    IPROACTV(NBUDGET_V, IPROC) = 4
  END IF
  IPROC=IPROC+1 
  IF ( LCORIO ) THEN  
    IPROACTV(NBUDGET_V, IPROC) = NCORV
  ELSE
    IPROACTV(NBUDGET_V, IPROC) = 4
  END IF
  IPROC=IPROC+1 
  IF ( ONUMDIFU ) IPROACTV(NBUDGET_V, IPROC) = NDIFV
  IPROC=IPROC+1 
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
  IF( ODRAGTREE ) IPROACTV(NBUDGET_V, IPROC)  = NDRAGV
  IPROC=IPROC+1
  IF ( HTURB /= 'NONE' ) IPROACTV(NBUDGET_V, IPROC) = NVTURBV
  IPROC=IPROC+1 
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
  IF ( HSCONV == 'EDKF' ) IPROACTV(NBUDGET_V, IPROC) = NMAFLV
  IPROC=IPROC+1 
  IPROACTV(NBUDGET_V, IPROC) = NADVV
  IPROC=IPROC+1
  IPROACTV(NBUDGET_V, IPROC) = NPRESV
!
  YWORK2(NBUDGET_V, 1) = 'INIF_'
  YWORK2(NBUDGET_V, 2) = 'ENDF_'
  YWORK2(NBUDGET_V, 3) = 'AVEF_'
  IPROC=4
  YWORK2(NBUDGET_V, IPROC) = 'ASSE_'
  IPROC=IPROC+1 
  YWORK2(NBUDGET_V, IPROC) = 'NEST_'
  IPROC=IPROC+1 
  YWORK2(NBUDGET_V, IPROC) = 'FRC_'
  IPROC=IPROC+1 
  YWORK2(NBUDGET_V, IPROC) = 'NUD_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_V, IPROC) = 'CURV_'
  IPROC=IPROC+1 
  YWORK2(NBUDGET_V, IPROC) = 'COR_'
  IPROC=IPROC+1 
  YWORK2(NBUDGET_V, IPROC) = 'DIF_'
  IPROC=IPROC+1 
  YWORK2(NBUDGET_V, IPROC) = 'REL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_V, IPROC) = 'DRAG_'
  IPROC=IPROC+1 
  YWORK2(NBUDGET_V, IPROC) = 'VTURB_'
  IPROC=IPROC+1 
  YWORK2(NBUDGET_V, IPROC) = 'HTURB_'
  IPROC=IPROC+1 
  YWORK2(NBUDGET_V, IPROC) = 'MAFL_'
  IPROC=IPROC+1 
  YWORK2(NBUDGET_V, IPROC) = 'ADV_'
  IPROC=IPROC+1 
  YWORK2(NBUDGET_V, IPROC) = 'PRES_'
!
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
  IPROC=4
  IPROACTV(NBUDGET_W, IPROC) = NASSEW
  IPROC=IPROC+1
  IF( NMODEL>1 ) IPROACTV(NBUDGET_W, IPROC) = NNESTW
  IPROC=IPROC+1
  IF( LFORCING ) IPROACTV(NBUDGET_W, IPROC)  = NFRCW
  IPROC=IPROC+1
  IF( ONUDGING ) IPROACTV(NBUDGET_W, IPROC)  = NNUDW
  IPROC=IPROC+1
  IF ( .NOT. LCARTESIAN ) THEN
    IPROACTV(NBUDGET_W, IPROC) = NCURVW
  ELSE
    IPROACTV(NBUDGET_W, IPROC) = 4
  END IF
  IPROC=IPROC+1
  IF ( LCORIO ) THEN  
    IPROACTV(NBUDGET_W, IPROC) = NCORW
  ELSE
    IPROACTV(NBUDGET_W, IPROC) = 4
  END IF
  IPROC=IPROC+1 
  IF ( ONUMDIFU ) IPROACTV(NBUDGET_W, IPROC) = NDIFW
  IPROC=IPROC+1
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
  IF ( HTURB /= 'NONE' ) IPROACTV(NBUDGET_W, IPROC) = NVTURBW
  IPROC=IPROC+1
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
  IPROACTV(NBUDGET_W, IPROC) = NGRAVW
  IPROC=IPROC+1
  IPROACTV(NBUDGET_W, IPROC) = NADVW
  IPROC=IPROC+1
  IPROACTV(NBUDGET_W, IPROC) = NPRESW
!
  YWORK2(NBUDGET_W, 1) = 'INIF_'
  YWORK2(NBUDGET_W, 2) = 'ENDF_'
  YWORK2(NBUDGET_W, 3) = 'AVEF_'
  IPROC=4
  YWORK2(NBUDGET_W, IPROC) = 'ASSE_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'NEST_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'FRC_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'NUD_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'CURV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'COR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'DIF_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'REL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'VTURB_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'HTURB_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'GRAV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'ADV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_W, IPROC) = 'PRES_'
!
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
  IPROC=4
  IPROACTV(NBUDGET_TH, IPROC) = NASSETH
  IPROC=IPROC+1
  IF( NMODEL>1 ) IPROACTV(NBUDGET_TH, IPROC) = NNESTTH
  IPROC=IPROC+1
  IF( LFORCING ) IPROACTV(NBUDGET_TH, IPROC)  = NFRCTH
  IPROC=IPROC+1
  IF( L2D_ADV_FRC ) IPROACTV(NBUDGET_TH, IPROC)  = N2DADVTH
  IPROC=IPROC+1
  IF( L2D_REL_FRC ) IPROACTV(NBUDGET_TH, IPROC)  = N2DRELTH
  IPROC=IPROC+1
  IF( ONUDGING ) IPROACTV(NBUDGET_TH, IPROC)  = NNUDTH
  IPROC=IPROC+1
  IF ( KRR > 0 ) THEN
    IF(.NOT.L1D) IPROACTV(NBUDGET_TH, IPROC) = NPREFTH
  ELSE
    IPROACTV(NBUDGET_TH, IPROC) = 4
  END IF
  IPROC=IPROC+1
  IF ( ONUMDIFTH ) IPROACTV(NBUDGET_TH, IPROC) = NDIFTH
  IPROC=IPROC+1
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
  IF ( HRAD /= 'NONE' ) IPROACTV(NBUDGET_TH, IPROC) = NRADTH
  IPROC=IPROC+1
  IF ( HDCONV /= 'NONE' .OR. HSCONV == 'KAFR') IPROACTV(NBUDGET_TH, IPROC) = NDCONVTH
  IPROC=IPROC+1
  IF ( HTURB /= 'NONE' ) IPROACTV(NBUDGET_TH, IPROC) = NVTURBTH
  IPROC=IPROC+1
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
  IF (HTURB /= 'NONE')     IPROACTV(NBUDGET_TH, IPROC) = NDISSHTH
  IPROC=IPROC+1
  IF (HTURB /= 'NONE' .AND. ( (HCLOUD == 'KHKO') .OR.  (HCLOUD == 'C2R2'))) &
          IPROACTV(NBUDGET_TH, IPROC) = NNETURTH
  IPROC=IPROC+1
  IF ( HSCONV == 'EDKF' ) IPROACTV(NBUDGET_TH, IPROC) = NMAFLTH
  IPROC=IPROC+1
  IPROACTV(NBUDGET_TH, IPROC) = NADVTH
  IPROC=IPROC+1
  IF ((HCLOUD == 'KHKO')  .OR.  (HCLOUD == 'C2R2'))  IPROACTV(NBUDGET_TH, IPROC) = NNEADVTH
  IPROC=IPROC+1
  IF (HCLOUD /= 'NONE' .AND. HCLOUD /= 'KHKO' .AND. HCLOUD /= 'C2R2') &
          IPROACTV(NBUDGET_TH, IPROC) = NNEGATH
  IPROC=IPROC+1
  IF (HCLOUD == 'LIMA') THEN
     IF (OPTSPLIT)                                           IPROACTV(NBUDGET_TH, IPROC) = NSEDITH
     IPROC=IPROC+1
     IF (OWARM .AND. OACTI .AND. NMOD_CCN.GE.1)              IPROACTV(NBUDGET_TH, IPROC) = NHENUTH
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT) THEN
        IF (OWARM .AND. ORAIN)                               IPROACTV(NBUDGET_TH, IPROC) = NREVATH
        IPROC=IPROC+1
     END IF
     IF (OCOLD .AND. ONUCL)                                  IPROACTV(NBUDGET_TH, IPROC) = NHINDTH
     IPROC=IPROC+1
     IF (OCOLD .AND. ONUCL)                                  IPROACTV(NBUDGET_TH, IPROC) = NHINCTH
     IPROC=IPROC+1
     IF (OCOLD .AND. ONUCL .AND. OHHONI .AND. NMOD_CCN.GE.1) IPROACTV(NBUDGET_TH, IPROC) = NHONHTH
     IPROC=IPROC+1
     IF (OPTSPLIT) THEN
                                                             IPROACTV(NBUDGET_TH, IPROC) = NREVATH
        IPROC=IPROC+1
     END IF
     IF (OCOLD .AND. OWARM .AND. ONUCL)                      IPROACTV(NBUDGET_TH, IPROC) = NHONCTH
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. ONUCL .AND. ORAIN))      IPROACTV(NBUDGET_TH, IPROC) = NHONRTH
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))                  IPROACTV(NBUDGET_TH, IPROC) = NDEPSTH
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_TH, IPROC) = NDEPGTH
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM))                  IPROACTV(NBUDGET_TH, IPROC) = NIMLTTH
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM))                  IPROACTV(NBUDGET_TH, IPROC) = NBERFITH
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_TH, IPROC) = NRIMTH
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW .AND. ORAIN))      IPROACTV(NBUDGET_TH, IPROC) = NACCTH
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_TH, IPROC) = NCFRZTH
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_TH, IPROC) = NWETGTH
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_TH, IPROC) = NDRYGTH
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_TH, IPROC) = NGMLTTH
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT .AND. OHAIL)                          IPROACTV(NBUDGET_TH, IPROC) = NWETHTH
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT .AND. OHAIL)                          IPROACTV(NBUDGET_TH, IPROC) = NHMLTTH
     IPROC=IPROC+1
                                                             IPROACTV(NBUDGET_TH, IPROC) = NCEDSTH
     IPROC=IPROC+1
  ELSE
    IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LADJ_BEFORE) IPROACTV(NBUDGET_TH, IPROC) = NADJUTH
    IPROC=IPROC+1
    IF (HCLOUD(1:3) == 'ICE' .OR. (HCLOUD == 'C2R2' .AND. (.NOT. LSUPSAT)) &
          .OR. ( HCLOUD == 'KHKO' .AND. (.NOT. LSUPSAT)) ) &
      IPROACTV(NBUDGET_TH, IPROC) = NHENUTH
      IPROC=IPROC+1
      IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NHONTH
      IPROC=IPROC+1
      IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NSFRTH
      IPROC=IPROC+1
      IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NDEPSTH
      IPROC=IPROC+1
      IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NDEPGTH
      IPROC=IPROC+1
      IF (((HCLOUD(1:3) == 'ICE') .AND. LWARM) .OR. ((HCLOUD == 'C2R2' &
         .OR. HCLOUD == 'KHKO') .AND. LRAIN) .OR. HCLOUD(1:3) == 'KES')             &
         IPROACTV(NBUDGET_TH, IPROC) = NREVATH
      IPROC=IPROC+1
      IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NRIMTH
      IPROC=IPROC+1
      IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NACCTH
      IPROC=IPROC+1
      IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NCFRZTH
      IPROC=IPROC+1
      IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NWETGTH
      IPROC=IPROC+1
      IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NDRYGTH
      IPROC=IPROC+1
      IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NGMLTTH
      IPROC=IPROC+1
      IF (HCLOUD == 'ICE4') IPROACTV(NBUDGET_TH, IPROC) = NWETHTH
      IPROC=IPROC+1
      IF (HCLOUD == 'ICE4'.AND. LRED) IPROACTV(NBUDGET_TH, IPROC) = NDRYHTH
      IPROC=IPROC+1
      IF (HCLOUD == 'ICE4') IPROACTV(NBUDGET_TH, IPROC) = NHMLTTH
      IPROC=IPROC+1
      IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NIMLTTH
      IPROC=IPROC+1
      IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_TH, IPROC) = NBERFITH
      IPROC=IPROC+1
      IF (HCLOUD(1:3) == 'ICE' .AND. LRED) IPROACTV(NBUDGET_TH, IPROC) = NCORRTH
      IPROC=IPROC+1
      IF (HCLOUD(1:3) == 'ICE' .AND. .NOT. LRED .OR. (LRED .AND. LADJ_AFTER)) &
          IPROACTV(NBUDGET_TH, IPROC) = NCDEPITH
      IPROC=IPROC+1
      IF (HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO' .OR. HCLOUD(1:3) == 'KES' .OR.   &
      HCLOUD == 'REVE')   IPROACTV(NBUDGET_TH, IPROC) = NCONDTH
      IPROC=IPROC+1
      IF ((HCLOUD == 'KHKO')  .OR.  (HCLOUD == 'C2R2'))&
          IPROACTV(NBUDGET_TH, IPROC) = NNECONTH
    END IF
!
  YWORK2(NBUDGET_TH, 1) = 'INIF_'
  YWORK2(NBUDGET_TH, 2) = 'ENDF_'
  YWORK2(NBUDGET_TH, 3) = 'AVEF_'
  IPROC=4
  YWORK2(NBUDGET_TH, IPROC) = 'ASSE_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'NEST_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'FRC_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = '2DADV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = '2DREL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'NUD_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'PREF_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'DIF_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'REL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'RAD_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'DCONV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'VTURB_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'HTURB_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'DISSH_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'NETUR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'MAFL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'ADV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'NEADV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TH, IPROC) = 'NEGA_'
  IPROC=IPROC+1

  IF (HCLOUD == 'LIMA') THEN
     YWORK2(NBUDGET_TH, IPROC) = 'SEDI_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'HENU_'
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT) THEN
        YWORK2(NBUDGET_TH, IPROC) = 'REVA_'
        IPROC=IPROC+1
     END IF
     YWORK2(NBUDGET_TH, IPROC) = 'HIND_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'HINC_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'HONH_'
     IPROC=IPROC+1
     IF (OPTSPLIT) THEN
        YWORK2(NBUDGET_TH, IPROC) = 'REVA_'
        IPROC=IPROC+1
     END IF
     YWORK2(NBUDGET_TH, IPROC) = 'HONC_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'HONR_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'DEPS_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'DEPG_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'IMLT_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'BERFI_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'RIM_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'ACC_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'CFRZ_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'WETG_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'DRYG_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'GMLT_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'WETH_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'HMLT_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'CEDS_'
     IPROC=IPROC+1
  ELSE
     YWORK2(NBUDGET_TH, IPROC) = 'ADJU_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'HENU_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'HON_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'SFR_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'DEPS_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'DEPG_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'REVA_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'RIM_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'ACC_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'CFRZ_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'WETG_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'DRYG_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'GMLT_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'WETH_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'DRYH_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'HMLT_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'IMLT_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'BERFI_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'CORR_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'CDEPI_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'COND_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_TH, IPROC) = 'NECON_'
  END IF
!
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
  IPROC=4
  IPROACTV(NBUDGET_TKE, IPROC) = NASSETKE
  IPROC=IPROC+1
  IF( LFORCING ) IPROACTV(NBUDGET_TKE, IPROC)  = NFRCTKE
  IPROC=IPROC+1
  IF ( ONUMDIFTH ) IPROACTV(NBUDGET_TKE, IPROC) = NDIFTKE
  IPROC=IPROC+1
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
  IF( ODRAGTREE ) IPROACTV(NBUDGET_TKE, IPROC)  = NDRAGTKE
  IPROC=IPROC+1
  IPROACTV(NBUDGET_TKE, IPROC) = NDPTKE
  IPROC=IPROC+1
  IPROACTV(NBUDGET_TKE, IPROC) = NTPTKE
  IPROC=IPROC+1
  IPROACTV(NBUDGET_TKE, IPROC) = NDISSTKE
  IPROC=IPROC+1 
  IPROACTV(NBUDGET_TKE, IPROC) = NTRTKE
  IPROC=IPROC+1
  IPROACTV(NBUDGET_TKE, IPROC) = NADVTKE
!
  YWORK2(NBUDGET_TKE, 1) = 'INIF_'
  YWORK2(NBUDGET_TKE, 2) = 'ENDF_'
  YWORK2(NBUDGET_TKE, 3) = 'AVEF_'
  IPROC=4
  YWORK2(NBUDGET_TKE, IPROC) = 'ASSE_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'FRC_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'DIF_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'REL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'DRAG_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'DP_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'TP_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'DISS_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'TR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_TKE, IPROC) = 'ADV_'
!
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
  IPROC=4
  IPROACTV(NBUDGET_RV, IPROC) = NASSERV
  IPROC=IPROC+1 
  IF( NMODEL>1 ) IPROACTV(NBUDGET_RV, IPROC) = NNESTRV
  IPROC=IPROC+1
  IF( LFORCING ) IPROACTV(NBUDGET_RV, IPROC)  = NFRCRV
  IPROC=IPROC+1
  IF( L2D_ADV_FRC ) IPROACTV(NBUDGET_RV, IPROC)  = N2DADVRV
  IPROC=IPROC+1
  IF( L2D_REL_FRC ) IPROACTV(NBUDGET_RV, IPROC)  = N2DRELRV
  IPROC=IPROC+1 
  IF( ONUDGING ) IPROACTV(NBUDGET_RV, IPROC)  = NNUDRV
  IPROC=IPROC+1 
  IF ( ONUMDIFTH ) IPROACTV(NBUDGET_RV, IPROC) = NDIFRV
  IPROC=IPROC+1 
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
  IF ( HDCONV /= 'NONE' .OR. HSCONV == 'KAFR') IPROACTV(NBUDGET_RV, IPROC) = NDCONVRV
  IPROC=IPROC+1 
  IF ( HTURB /= 'NONE' ) IPROACTV(NBUDGET_RV, IPROC) = NVTURBRV
  IPROC=IPROC+1 
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
  IF (HTURB /= 'NONE' .AND. ( (HCLOUD == 'KHKO') .OR.  (HCLOUD == 'C2R2'))) &
   IPROACTV(NBUDGET_RV, IPROC) = NNETURRV
  IPROC=IPROC+1 
  IF ( HSCONV == 'EDKF' ) IPROACTV(NBUDGET_RV, IPROC) = NMAFLRV
  IPROC=IPROC+1 
  IPROACTV(NBUDGET_RV, IPROC) = NADVRV
  IPROC=IPROC+1   
  IF ((HCLOUD == 'KHKO')  .OR.  (HCLOUD == 'C2R2'))  IPROACTV(NBUDGET_RV, IPROC) = NNEADVRV
  IPROC=IPROC+1 
  IF (HCLOUD /= 'NONE' .AND. HCLOUD /= 'KHKO' .AND. HCLOUD /= 'C2R2') &
     IPROACTV(NBUDGET_RV, IPROC) = NNEGARV
  IPROC=IPROC+1   

  IF (HCLOUD == 'LIMA') THEN
     IF (OWARM .AND. OACTI .AND. NMOD_CCN.GE.1)              IPROACTV(NBUDGET_RV, IPROC) = NHENURV
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT) THEN
        IF (OWARM .AND. ORAIN)                               IPROACTV(NBUDGET_RV, IPROC) = NREVARV
        IPROC=IPROC+1
     END IF
     IF (OCOLD .AND. ONUCL)                                  IPROACTV(NBUDGET_RV, IPROC) = NHINDRV
     IPROC=IPROC+1
     IF (OCOLD .AND. ONUCL .AND. OHHONI .AND. NMOD_CCN.GE.1) IPROACTV(NBUDGET_RV, IPROC) = NHONHRV
     IPROC=IPROC+1
     IF (OPTSPLIT) THEN
                                                             IPROACTV(NBUDGET_RV, IPROC) = NREVARV
        IPROC=IPROC+1
     END IF
     IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))                  IPROACTV(NBUDGET_RV, IPROC) = NDEPSRV
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_RV, IPROC) = NDEPGRV
     IPROC=IPROC+1
                                                             IPROACTV(NBUDGET_RV, IPROC) = NCEDSRV
     IPROC=IPROC+1

  ELSE
  IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LADJ_BEFORE) &
          IPROACTV(NBUDGET_RV, IPROC) = NADJURV
  IPROC=IPROC+1
  IF ((HCLOUD == 'C2R2'  .AND. (.NOT. LSUPSAT)) &
          .OR. ( HCLOUD == 'KHKO' .AND. (.NOT. LSUPSAT)) &
          .OR. HCLOUD(1:3) == 'ICE')  &
        IPROACTV(NBUDGET_RV, IPROC) = NHENURV
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RV, IPROC) = NDEPSRV
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RV, IPROC) = NDEPGRV
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'KES' .OR. ((HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO') .AND. LRAIN) .OR. &
      ((HCLOUD(1:3) == 'ICE') .AND. LWARM)) IPROACTV(NBUDGET_RV, IPROC) = NREVARV
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'KES'.OR. HCLOUD == 'REVE' .OR. &
      HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO' ) IPROACTV(NBUDGET_RV, IPROC) = NCONDRV
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'ICE' .AND. LRED) IPROACTV(NBUDGET_RV, IPROC) = NCORRRV
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'ICE' .AND. (.NOT. LRED .OR. (LRED .AND. LADJ_AFTER) )) &
          IPROACTV(NBUDGET_RV, IPROC) = NCDEPIRV
  IPROC=IPROC+1
  IF ( (HCLOUD == 'KHKO' )  .OR.  (HCLOUD == 'C2R2')) &
          IPROACTV(NBUDGET_RV, IPROC) = NNECONRV
  IPROC=IPROC+1
END IF

!
  YWORK2(NBUDGET_RV, 1) = 'INIF_'
  YWORK2(NBUDGET_RV, 2) = 'ENDF_'
  YWORK2(NBUDGET_RV, 3) = 'AVEF_'
  IPROC=4
  YWORK2(NBUDGET_RV, IPROC) = 'ASSE_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'NEST_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'FRC_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = '2DADV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = '2DREL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'NUD_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'DIF_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'REL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'DCONV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'VTURB_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'HTURB_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'NETUR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'MAFL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'ADV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'NEADV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'NEGA_'
  IPROC=IPROC+1
IF (HCLOUD == 'LIMA') THEN
 YWORK2(NBUDGET_RV, IPROC) = 'HENU_'
  IPROC=IPROC+1
  IF (.NOT.OPTSPLIT) THEN
     YWORK2(NBUDGET_RV, IPROC) = 'REVA_'
     IPROC=IPROC+1
  END IF
  YWORK2(NBUDGET_RV, IPROC) = 'HIND_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'HONH_'
  IPROC=IPROC+1
  IF (OPTSPLIT) THEN
     YWORK2(NBUDGET_RV, IPROC) = 'REVA_'
     IPROC=IPROC+1
  END IF
  YWORK2(NBUDGET_RV, IPROC) = 'DEPS_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'DEPG_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'CEDS_'
ELSE
  YWORK2(NBUDGET_RV, IPROC) = 'ADJU_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'HENU_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'DEPS_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'DEPG_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'REVA_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'COND_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'CORR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'CDEPI_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RV, IPROC) = 'NECON_'
END IF
!
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
  IPROC=4
  IPROACTV(NBUDGET_RC, IPROC) = NASSERC
  IPROC=IPROC+1
  IF( NMODEL>1 ) IPROACTV(NBUDGET_RC, IPROC) = NNESTRC
  IPROC=IPROC+1
  IF( LFORCING ) IPROACTV(NBUDGET_RC, IPROC)  = NFRCRC
  IPROC=IPROC+1
  IF ( ONUMDIFTH ) IPROACTV(NBUDGET_RC, IPROC) = NDIFRC
  IPROC=IPROC+1
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
  IF( HDCONV /= 'NONE' .OR. HSCONV == 'KAFR') IPROACTV(NBUDGET_RC, IPROC)  = NDCONVRC
  IPROC=IPROC+1
  IF( ODRAGTREE .AND. ODEPOTREE ) IPROACTV(NBUDGET_RC, IPROC) = NDEPOTRRC
  IPROC=IPROC+1
  IF ( HTURB /= 'NONE' ) IPROACTV(NBUDGET_RC, IPROC) = NVTURBRC
  IPROC=IPROC+1
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
  IF (HTURB /= 'NONE' .AND. ( (HCLOUD == 'KHKO') .OR.  (HCLOUD == 'C2R2'))) &
      IPROACTV(NBUDGET_RC, IPROC) = NNETURRC
  IPROC=IPROC+1
  IPROACTV(NBUDGET_RC, IPROC) = NADVRC
  IPROC=IPROC+1
  IF ((HCLOUD == 'KHKO')  .OR.  (HCLOUD == 'C2R2'))  &
      IPROACTV(NBUDGET_RC, IPROC) = NNEADVRC
  IPROC=IPROC+1
  IF (HCLOUD /= 'NONE' .AND. HCLOUD /= 'KHKO' .AND. HCLOUD /= 'C2R2') &
     IPROACTV(NBUDGET_RC, IPROC)  = NNEGARC
  IPROC=IPROC+1

  IF (HCLOUD == 'LIMA') THEN
     IF (OPTSPLIT .AND. OWARM .AND. ORAIN)              IPROACTV(NBUDGET_RC, IPROC) = NCORRRC
     IPROC=IPROC+1
     IF (OWARM .AND. OSEDC)                             IPROACTV(NBUDGET_RC, IPROC) = NSEDIRC
     IPROC=IPROC+1
     IF (OWARM .AND. ODEPOC)                            IPROACTV(NBUDGET_RC, IPROC) = NDEPORC
     IPROC=IPROC+1
     IF (OPTSPLIT .AND. OWARM .AND. ORAIN)              IPROACTV(NBUDGET_RC, IPROC) = NR2C1RC
     IPROC=IPROC+1
     IF (OWARM .AND. OACTI .AND. NMOD_CCN.GE.1)         IPROACTV(NBUDGET_RC, IPROC) = NHENURC
     IPROC=IPROC+1
     IF (OPTSPLIT) THEN
        IF (OCOLD .AND. ONUCL)                          IPROACTV(NBUDGET_RC, IPROC) = NHINCRC
        IPROC=IPROC+1
     END IF
     IF (OPTSPLIT .OR. (OWARM .AND. ORAIN))             IPROACTV(NBUDGET_RC, IPROC) = NAUTORC
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OWARM .AND. ORAIN))             IPROACTV(NBUDGET_RC, IPROC) = NACCRRC
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OWARM .AND. ORAIN))             IPROACTV(NBUDGET_RC, IPROC) = NREVARC
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT) THEN
        IF (OCOLD .AND. ONUCL)                          IPROACTV(NBUDGET_RC, IPROC) = NHINCRC
        IPROC=IPROC+1
     END IF
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. ONUCL)) IPROACTV(NBUDGET_RC, IPROC) = NHONCRC
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM))             IPROACTV(NBUDGET_RC, IPROC) = NIMLTRC
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM))             IPROACTV(NBUDGET_RC, IPROC) = NBERFIRC
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RC, IPROC) = NRIMRC
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RC, IPROC) = NWETGRC
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RC, IPROC) = NDRYGRC
     IPROC=IPROC+1
     IF (OPTSPLIT)                                      IPROACTV(NBUDGET_RC, IPROC) = NCVRCRC
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT .AND. OHAIL)                     IPROACTV(NBUDGET_RC, IPROC) = NWETHRC
     IPROC=IPROC+1
                                                        IPROACTV(NBUDGET_RC, IPROC) = NCEDSRC
     IPROC=IPROC+1
  ELSE
  IF (HCLOUD(1:3) == 'KES' )            IPROACTV(NBUDGET_RC, IPROC  ) = NACCRRC
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'KES' )            IPROACTV(NBUDGET_RC, IPROC) = NAUTORC
  IPROC=IPROC+1
  IF ((HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO')  .AND. (.NOT. LSUPSAT)) &
          IPROACTV(NBUDGET_RC, IPROC) = NHENURC
  IPROC=IPROC+1
!
  IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LADJ_BEFORE) &
          IPROACTV(NBUDGET_RC, IPROC) = NADJURC
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'ICE' .AND. LSEDIC .AND. &
          LRED .AND. (.NOT. LSEDIM_AFTER)) &
     IPROACTV(NBUDGET_RC, IPROC) = NSEDIRC
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'ICE' ) IPROACTV(NBUDGET_RC, IPROC) = NHONRC
  IPROC=IPROC+1
  IF (((HCLOUD(1:3) == 'ICE' ) .AND. LWARM) .OR.  ((HCLOUD == 'C2R2' .OR. &
        HCLOUD == 'KHKO') .AND. LRAIN)) IPROACTV(NBUDGET_RC, IPROC) = NAUTORC
  IPROC=IPROC+1
  !modif
  IF (((HCLOUD(1:3) == 'ICE' ) .AND. LWARM) .OR.  ((HCLOUD == 'C2R2' .OR. &
        HCLOUD == 'KHKO') .AND. LRAIN)) IPROACTV(NBUDGET_RC, IPROC) = NACCRRC
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RC, IPROC) = NRIMRC
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'ICE'  .AND. LRED) IPROACTV(NBUDGET_RC, IPROC) = NCMELRC
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RC, IPROC) = NWETGRC
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RC, IPROC) = NDRYGRC
  IPROC=IPROC+1
  IF (HCLOUD == 'ICE4')  IPROACTV(NBUDGET_RC, IPROC) = NWETHRC
  IPROC=IPROC+1
  IF (HCLOUD == 'ICE4' .AND. LRED)  IPROACTV(NBUDGET_RC, IPROC) = NDRYHRC
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'ICE' ) IPROACTV(NBUDGET_RC, IPROC) = NIMLTRC
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RC, IPROC) = NBERFIRC
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'ICE' .AND. LRED) IPROACTV(NBUDGET_RC, IPROC) = NCORRRC
  IPROC=IPROC+1
  IF (((HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO').AND. LSEDC) .OR.   &
     (HCLOUD(1:3) == 'ICE' .AND. LSEDIC .AND. .NOT. LRED) .OR. &
     (HCLOUD(1:3) == 'ICE' .AND. LSEDIC .AND. LRED .AND. LSEDIM_AFTER)) &
     IPROACTV(NBUDGET_RC, IPROC) = NSEDIRC
  IPROC=IPROC+1
  IF (((HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO').AND. LDEPOC) .OR.   &
     (HCLOUD(1:3) == 'ICE' .AND. LDEPOSC)) IPROACTV(NBUDGET_RC, IPROC) = NDEPORC
  IPROC=IPROC+1
  IF (HCLOUD(1:3) == 'ICE' .AND. (.NOT. LRED .OR. (LRED .AND. LADJ_AFTER) )) &
          IPROACTV(NBUDGET_RC, IPROC) = NCDEPIRC
  IPROC=IPROC+1
  IF (HCLOUD == 'C2R2'.OR. HCLOUD == 'KHKO' .OR. &
      HCLOUD(1:3) == 'KES'.OR. HCLOUD == 'REVE') IPROACTV(NBUDGET_RC, IPROC) = NCONDRC
  IPROC=IPROC+1
  IF ( (HCLOUD == 'KHKO' )  .OR.  (HCLOUD == 'C2R2')) &
     IPROACTV(NBUDGET_RC, IPROC) = NNECONRC
  IPROC=IPROC+1
  END IF

!
  YWORK2(NBUDGET_RC, 1) = 'INIF_'
  YWORK2(NBUDGET_RC, 2) = 'ENDF_'
  YWORK2(NBUDGET_RC, 3) = 'AVEF_'
  IPROC=4
  YWORK2(NBUDGET_RC, IPROC) = 'ASSE_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'NEST_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'FRC_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'DIF_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'REL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'DCONV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'DEPOTR'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'VTURB_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'HTURB_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'NETUR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'ADV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'NEADV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RC, IPROC) = 'NEGA_'
  IPROC=IPROC+1

  IF (HCLOUD == 'LIMA') THEN
     YWORK2(NBUDGET_RC, IPROC) = 'CORR_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'SEDI_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'DEPO_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'R2C1_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'HENU_'
     IPROC=IPROC+1
     IF (OPTSPLIT) THEN
        YWORK2(NBUDGET_RC, IPROC) = 'HINC_'
        IPROC=IPROC+1
     END IF
     YWORK2(NBUDGET_RC, IPROC) = 'AUTO_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'ACCR_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'REVA_'
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT) THEN
        YWORK2(NBUDGET_RC, IPROC) = 'HINC_'
        IPROC=IPROC+1
     END IF
     YWORK2(NBUDGET_RC, IPROC) = 'HONC_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'IMLT_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'BERFI_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'RIM_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'WETG_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'DRYG_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'CVRC_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'WETH_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'CEDS_'
  ELSE
     YWORK2(NBUDGET_RC, IPROC) = 'ACCR_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'AUTO_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'HENU_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'ADJU_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'SEDI_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'HON_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'AUTO_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'ACCR_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'RIM_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'CMEL_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'WETG_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'DRYG_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'WETH_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'DRYH_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'IMLT_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'BERFI_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'CORR_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'SEDI_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'DEPO_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'CDEPI_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'COND_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RC, IPROC) = 'NECON_'
  END IF
!
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
  IPROC=4
  IPROACTV(NBUDGET_RR, IPROC) = NASSERR
  IPROC=IPROC+1
  IF( NMODEL>1 ) IPROACTV(NBUDGET_RR, IPROC) = NNESTRR
  IPROC=IPROC+1
  IF( LFORCING ) IPROACTV(NBUDGET_RR, IPROC)  = NFRCRR
  IPROC=IPROC+1
  IF ( ONUMDIFTH ) IPROACTV(NBUDGET_RR, IPROC) = NDIFRR
  IPROC=IPROC+1
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
  IPROACTV(NBUDGET_RR, IPROC) = NADVRR
  IPROC=IPROC+1
  IF ( HCLOUD /= 'NONE' ) IPROACTV(NBUDGET_RR, IPROC) = NNEGARR
  IPROC=IPROC+1

  IF (HCLOUD == 'LIMA') THEN
     IF (OPTSPLIT .AND. OWARM .AND. ORAIN)              IPROACTV(NBUDGET_RR, IPROC) = NCORRRR
     IPROC=IPROC+1
     IF (OWARM .AND. ORAIN)                             IPROACTV(NBUDGET_RR, IPROC) = NSEDIRR
     IPROC=IPROC+1
     IF (OPTSPLIT .AND. OWARM .AND. ORAIN)              IPROACTV(NBUDGET_RR, IPROC) = NR2C1RR
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OWARM .AND. ORAIN))             IPROACTV(NBUDGET_RR, IPROC) = NAUTORR
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OWARM .AND. ORAIN))             IPROACTV(NBUDGET_RR, IPROC) = NACCRRR
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OWARM .AND. ORAIN))             IPROACTV(NBUDGET_RR, IPROC) = NREVARR
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. ONUCL .AND. ORAIN)) IPROACTV(NBUDGET_RR, IPROC) = NHONRRR
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW .AND. ORAIN)) IPROACTV(NBUDGET_RR, IPROC) = NACCRR
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RR, IPROC) = NCFRZRR
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RR, IPROC) = NWETGRR
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RR, IPROC) = NDRYGRR
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RR, IPROC) = NGMLTRR
     IPROC=IPROC+1
     IF (OPTSPLIT)                                      IPROACTV(NBUDGET_RR, IPROC) = NCVRCRR
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT .AND. OHAIL)                     IPROACTV(NBUDGET_RR, IPROC) = NWETHRR
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT .AND. OHAIL)                     IPROACTV(NBUDGET_RR, IPROC) = NHMLTRR
  ELSE
     IF (HCLOUD(1:3) == 'KES' )   IPROACTV(NBUDGET_RR, IPROC) = NSEDIRR
     IPROC=IPROC+1
     IF (HCLOUD(1:3) == 'KES' )   IPROACTV(NBUDGET_RR, IPROC) = NACCRRR
     IPROC=IPROC+1
     IF (HCLOUD(1:3) == 'KES' )   IPROACTV(NBUDGET_RR, IPROC) = NAUTORR
     IPROC=IPROC+1
     IF (HCLOUD(1:3) == 'KES' )   IPROACTV(NBUDGET_RR, IPROC) = NREVARR
     IPROC=IPROC+1
!
     IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. (.NOT. LSEDIM_AFTER)) &
             IPROACTV(NBUDGET_RR, IPROC) = NSEDIRR
     IPROC=IPROC+1
     IF (HCLOUD(1:3) == 'ICE' ) IPROACTV(NBUDGET_RR, IPROC) = NSFRRR
     IPROC=IPROC+1
     IF ((HCLOUD(1:3) == 'ICE' ) .AND. LWARM) &
          IPROACTV(NBUDGET_RR, IPROC) = NAUTORR
     IPROC=IPROC+1
     IF ((HCLOUD(1:3) == 'ICE' ) .AND. LWARM) &
          IPROACTV(NBUDGET_RR, IPROC) = NACCRRR
     IPROC=IPROC+1
     IF ((HCLOUD(1:3) == 'ICE' ) .AND. LWARM) &
          IPROACTV(NBUDGET_RR, IPROC) = NREVARR
     IPROC=IPROC+1
     IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RR, IPROC) = NACCRR
     IPROC=IPROC+1
     IF (HCLOUD(1:3) == 'ICE' .AND. LRED) IPROACTV(NBUDGET_RR, IPROC) = NCMELRR
     IPROC=IPROC+1
     IF (HCLOUD(1:3) == 'ICE' ) IPROACTV(NBUDGET_RR, IPROC) = NCFRZRR
     IPROC=IPROC+1
     IF (HCLOUD(1:3) == 'ICE' ) IPROACTV(NBUDGET_RR, IPROC) = NWETGRR
     IPROC=IPROC+1
     IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RR, IPROC) = NDRYGRR
     IPROC=IPROC+1
     IF (HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RR, IPROC) = NGMLTRR
     IPROC=IPROC+1
     IF (HCLOUD == 'ICE4') IPROACTV(NBUDGET_RR, IPROC) = NWETHRR
     IPROC=IPROC+1
     IF (HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RR, IPROC) = NDRYHRR
     IPROC=IPROC+1
     IF (HCLOUD == 'ICE4') IPROACTV(NBUDGET_RR, IPROC) = NHMLTRR
     IPROC=IPROC+1
     IF (HCLOUD(1:3) == 'ICE' .AND. LRED) IPROACTV(NBUDGET_RR, IPROC) = NCORRRR
     IPROC=IPROC+1
     IF ((HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO') .AND. LRAIN)  IPROACTV(NBUDGET_RR, IPROC) = NAUTORR
     IPROC=IPROC+1
     IF ((HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO') .AND. LRAIN)  IPROACTV(NBUDGET_RR, IPROC) = NACCRRR
     IPROC=IPROC+1
     IF ((HCLOUD == 'C2R2'.OR. HCLOUD == 'KHKO') .AND. LRAIN)   IPROACTV(NBUDGET_RR, IPROC) = NREVARR
     IPROC=IPROC+1
     IF (HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO' .OR. &
     (HCLOUD(1:3) == 'ICE' .AND. .NOT. (LRED)) .OR. &
     (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LSEDIM_AFTER)) &
       IPROACTV(NBUDGET_RR, IPROC) = NSEDIRR
     IPROC=IPROC+1
  END IF
!
  YWORK2(NBUDGET_RR, 1) = 'INIF_'
  YWORK2(NBUDGET_RR, 2) = 'ENDF_'
  YWORK2(NBUDGET_RR, 3) = 'AVEF_'
  IPROC=4
  YWORK2(NBUDGET_RR, IPROC) = 'ASSE_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'NEST_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'FRC_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'DIF_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'REL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'ADV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'NEGA_'
  IPROC=IPROC+1

IF (HCLOUD == 'LIMA') THEN
  YWORK2(NBUDGET_RR, IPROC) = 'CORR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'SEDI_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'R2C1_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'AUTO_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'ACCR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'REVA_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'HONR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'ACC_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'CFRZ_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'WETG_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'DRYG_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'GMLT_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'CVRC_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'WETH_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'HMLT_'
  IPROC=IPROC+1
ELSE
  YWORK2(NBUDGET_RR, IPROC) = 'SEDI_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'ACCR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'AUTO_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'REVA_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'SEDI_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'SFR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'AUTO_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'ACCR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'REVA_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'ACC_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'CMEL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'CFRZ_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'WETG_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'DRYG_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'GMLT_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'WETH_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'DRYH_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'HMLT_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'CORR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'AUTO_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'ACCR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'REVA_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RR, IPROC) = 'SEDI_'
END IF
!
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
  IPROC=4
  IPROACTV(NBUDGET_RI, IPROC) = NASSERI
  IPROC=IPROC+1
  IF( NMODEL>1 ) IPROACTV(NBUDGET_RI, IPROC) = NNESTRI
  IPROC=IPROC+1
  IF( LFORCING ) IPROACTV(NBUDGET_RI, IPROC)  = NFRCRI
  IPROC=IPROC+1
  IF( ONUMDIFTH ) IPROACTV(NBUDGET_RI, IPROC) = NDIFRI
  IPROC=IPROC+1
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
  IF( HDCONV /= 'NONE' .OR. HSCONV == 'KAFR') IPROACTV(NBUDGET_RI, IPROC) = NDCONVRI
  IPROC=IPROC+1
  IF ( HTURB /= 'NONE' ) IPROACTV(NBUDGET_RI, IPROC) = NVTURBRI
  IPROC=IPROC+1
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
  IPROACTV(NBUDGET_RI, IPROC) = NADVRI
  IPROC=IPROC+1
  IF( HCLOUD /= 'NONE' ) IPROACTV(NBUDGET_RI, IPROC) = NNEGARI
  IPROC=IPROC+1

  IF (HCLOUD=='LIMA') THEN
     IF (OPTSPLIT .AND. OCOLD .AND. OSNOW)                   IPROACTV(NBUDGET_RI, IPROC) = NCORRRI
     IPROC=IPROC+1
     IF (OCOLD .AND. OSEDI)                                  IPROACTV(NBUDGET_RI, IPROC) = NSEDIRI
     IPROC=IPROC+1
     IF (OCOLD .AND. ONUCL)                                  IPROACTV(NBUDGET_RI, IPROC) = NHINDRI
     IPROC=IPROC+1
     IF (OCOLD .AND. ONUCL)                                  IPROACTV(NBUDGET_RI, IPROC) = NHINCRI
     IPROC=IPROC+1
     IF (OCOLD .AND. ONUCL .AND. OHHONI .AND. NMOD_CCN.GE.1) IPROACTV(NBUDGET_RI, IPROC) = NHONHRI
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. ONUCL))      IPROACTV(NBUDGET_RI, IPROC) = NHONCRI
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))                  IPROACTV(NBUDGET_RI, IPROC) = NCNVIRI
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))                  IPROACTV(NBUDGET_RI, IPROC) = NCNVSRI
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))                  IPROACTV(NBUDGET_RI, IPROC) = NAGGSRI
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM))                  IPROACTV(NBUDGET_RI, IPROC) = NIMLTRI
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM))                  IPROACTV(NBUDGET_RI, IPROC) = NBERFIRI
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_RI, IPROC) = NHMSRI
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_RI, IPROC) = NCFRZRI
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_RI, IPROC) = NWETGRI
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_RI, IPROC) = NDRYGRI
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))      IPROACTV(NBUDGET_RI, IPROC) = NHMGRI
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT .AND. OHAIL) IPROACTV(NBUDGET_RI, IPROC) = NWETHRI
     IPROC=IPROC+1
                                       IPROACTV(NBUDGET_RI, IPROC) = NCEDSRI
  ELSE
     IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LADJ_BEFORE) IPROACTV(NBUDGET_RI, IPROC) = NADJURI
     IPROC=IPROC+1
     IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. (.NOT. LSEDIM_AFTER)) &
             IPROACTV(NBUDGET_RI, IPROC) = NSEDIRI
     IPROC=IPROC+1
     IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NHENURI
     IPROC=IPROC+1
     IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NHONRI
     IPROC=IPROC+1
     IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NAGGSRI
     IPROC=IPROC+1
     IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NAUTSRI
     IPROC=IPROC+1
     IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NCFRZRI
     IPROC=IPROC+1
     IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NWETGRI
     IPROC=IPROC+1
     IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NDRYGRI
     IPROC=IPROC+1
     IF( HCLOUD == 'ICE4' ) IPROACTV(NBUDGET_RI, IPROC) = NWETHRI
     IPROC=IPROC+1
     IF( HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RI, IPROC) = NDRYHRI
     IPROC=IPROC+1
     IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NIMLTRI
     IPROC=IPROC+1
     IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RI, IPROC) = NBERFIRI
     IPROC=IPROC+1
     IF( HCLOUD(1:3) == 'ICE' .AND. LRED) IPROACTV(NBUDGET_RI, IPROC) = NCORRRI
     IPROC=IPROC+1
     IF ((HCLOUD(1:3) == 'ICE' .AND. .NOT. LRED).OR. &
     (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LSEDIM_AFTER)) &
             IPROACTV(NBUDGET_RI, IPROC) = NSEDIRI
     IPROC=IPROC+1
     IF (HCLOUD(1:3) == 'ICE' .AND. (.NOT. LRED .OR. (LRED .AND. LADJ_AFTER) )) &
             IPROACTV(NBUDGET_RI, IPROC) = NCDEPIRI
     IPROC=IPROC+1
  END IF
!
  YWORK2(NBUDGET_RI, 1) = 'INIF_'
  YWORK2(NBUDGET_RI, 2) = 'ENDF_'
  YWORK2(NBUDGET_RI, 3) = 'AVEF_'
  IPROC=4
  YWORK2(NBUDGET_RI, IPROC) = 'ASSE_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'NEST_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'FRC_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'DIF_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'REL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'DCONV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'VTURB_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'HTURB_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'ADV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RI, IPROC) = 'NEGA_'
  IPROC=  IPROC+1
  IF (HCLOUD=='LIMA') THEN
     YWORK2(NBUDGET_RI, IPROC) = 'CORR_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'SEDI_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'HIND_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'HINC_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'HONH_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'HONC_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'CNVI_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'CNVS_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'AGGS_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'IMLT_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'BERFI_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'HMS_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'CFRZ_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'WETG_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'DRYG_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'HMG_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'WETH_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'CEDS_'
  ELSE
     YWORK2(NBUDGET_RI, IPROC) = 'ADJU_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'SEDI_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'HENU_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'HON_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'AGGS_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'AUTS_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'CFRZ_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'WETG_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'DRYG_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'WETH_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'DRYH_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'IMLT_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'BERFI_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'CORR_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'SEDI_'
     IPROC=  IPROC+1
     YWORK2(NBUDGET_RI, IPROC) = 'CDEPI_'
  END IF
!
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
  IPROC=4
  IPROACTV(NBUDGET_RS, IPROC) = NASSERS
  IPROC=  IPROC+1
  IF( NMODEL>1 ) IPROACTV(NBUDGET_RS, IPROC) = NNESTRS
  IPROC=IPROC+1
  IF( LFORCING )  IPROACTV(NBUDGET_RS, IPROC)  = NFRCRS
  IPROC=  IPROC+1
  IF( ONUMDIFTH ) IPROACTV(NBUDGET_RS, IPROC) = NDIFRS
  IPROC=IPROC+1
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
  IPROACTV(NBUDGET_RS, IPROC) = NADVRS
  IPROC=  IPROC+1
  IF( HCLOUD /= 'NONE' ) IPROACTV(NBUDGET_RS, IPROC) = NNEGARS
  IPROC=IPROC+1

IF (HCLOUD=='LIMA') THEN
     IF (OPTSPLIT .AND. OCOLD .AND. OSNOW)              IPROACTV(NBUDGET_RS, IPROC) = NCORRRS
     IPROC=IPROC+1
     IF (OCOLD .AND. OSNOW)                             IPROACTV(NBUDGET_RS, IPROC) = NSEDIRS
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))             IPROACTV(NBUDGET_RS, IPROC) = NCNVIRS
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))             IPROACTV(NBUDGET_RS, IPROC) = NDEPSRS
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))             IPROACTV(NBUDGET_RS, IPROC) = NCNVSRS
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OSNOW))             IPROACTV(NBUDGET_RS, IPROC) = NAGGSRS
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RS, IPROC) = NRIMRS
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RS, IPROC) = NHMSRS
     IPROC=IPROC+1            
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW .AND. ORAIN)) IPROACTV(NBUDGET_RS, IPROC) = NACCRS
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RS, IPROC) = NCMELRS
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RS, IPROC) = NWETGRS
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW)) IPROACTV(NBUDGET_RS, IPROC) = NDRYGRS
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT .AND. OHAIL)                     IPROACTV(NBUDGET_RS, IPROC) = NWETHRS
     IPROC=IPROC+1
 ELSE
  IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. (.NOT. LSEDIM_AFTER)) &
          IPROACTV(NBUDGET_RS, IPROC) = NSEDIRS
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NDEPSRS
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NAGGSRS
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NAUTSRS
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NRIMRS
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NACCRS
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NCMELRS
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NWETGRS
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RS, IPROC) = NDRYGRS
  IPROC=IPROC+1
  IF( HCLOUD == 'ICE4') IPROACTV(NBUDGET_RS, IPROC) = NWETHRS
  IPROC=IPROC+1
  IF( HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RS, IPROC) = NDRYHRS
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE' .AND. LRED) IPROACTV(NBUDGET_RS, IPROC) = NCORRRS
  IPROC=IPROC+1
  IF ((HCLOUD(1:3) == 'ICE' .AND. .NOT. LRED).OR. &
     (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LSEDIM_AFTER)) &
          IPROACTV(NBUDGET_RS, IPROC) = NSEDIRS
  IPROC=IPROC+1
END IF
!
  YWORK2(NBUDGET_RS, 1) = 'INIF_'
  YWORK2(NBUDGET_RS, 2) = 'ENDF_'
  YWORK2(NBUDGET_RS, 3) = 'AVEF_'
  IPROC= 4
  YWORK2(NBUDGET_RS, IPROC) = 'ASSE_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'NEST_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'FRC_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'DIF_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'REL_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'ADV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'NEGA_'
  IPROC=  IPROC+1
IF (HCLOUD=='LIMA') THEN
  YWORK2(NBUDGET_RS, IPROC) = 'CORR_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'SEDI_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'CNVI_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'DEPS_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'CNVS_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'AGGS_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'RIM_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'HMS_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'ACC_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'CMEL_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'WETG_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'DRYG_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'WETH_'
  IPROC=  IPROC+1
ELSE
  YWORK2(NBUDGET_RS, IPROC) = 'SEDI_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'DEPS_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'AGGS_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'AUTS_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'RIM_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'ACC_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'CMEL_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'WETG_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'DRYG_'
  IPROC=  IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'WETH_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'DRYH_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'CORR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RS, IPROC) = 'SEDI_'
END IF
!
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
  IPROC=4
  IPROACTV(NBUDGET_RG, IPROC) = NASSERG
  IPROC=IPROC+1
  IF( NMODEL>1 ) IPROACTV(NBUDGET_RG, IPROC) = NNESTRG
  IPROC=IPROC+1
  IF( LFORCING ) IPROACTV(NBUDGET_RG, IPROC)  = NFRCRG
  IPROC=IPROC+1
  IF( ONUMDIFTH ) IPROACTV(NBUDGET_RG, IPROC) = NDIFRG
  IPROC=IPROC+1
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
  IPROACTV(NBUDGET_RG, IPROC) = NADVRG
  IPROC=IPROC+1
  IF( HCLOUD /= 'NONE'  ) IPROACTV(NBUDGET_RG, IPROC) = NNEGARG
  IPROC=IPROC+1
IF (HCLOUD=='LIMA') THEN
     IF (OCOLD .AND. OSNOW)                                         IPROACTV(NBUDGET_RG, IPROC) = NSEDIRG
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. ONUCL .AND. ORAIN)) IPROACTV(NBUDGET_RG, IPROC) = NHONRRG
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NDEPGRG
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NRIMRG
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. ORAIN .AND. OSNOW)) IPROACTV(NBUDGET_RG, IPROC) = NACCRG
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NCMELRG
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NCFRZRG
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NWETGRG
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NDRYGRG
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NHMGRG
     IPROC=IPROC+1
     IF (OPTSPLIT .OR. (OCOLD .AND. OWARM .AND. OSNOW))             IPROACTV(NBUDGET_RG, IPROC) = NGMLTRG
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT .AND. OHAIL)                                 IPROACTV(NBUDGET_RG, IPROC) = NWETHRG
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT .AND. OHAIL)                                 IPROACTV(NBUDGET_RG, IPROC) = NCOHGRG
     IPROC=IPROC+1
ELSE
  IF (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. (.NOT. LSEDIM_AFTER)) &
          IPROACTV(NBUDGET_RG, IPROC) = NSEDIRG
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NSFRRG
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NDEPGRG
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NRIMRG
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NACCRG
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NCMELRG
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NCFRZRG
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NWETGRG
  IPROC=IPROC+1
  IF( HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RG, IPROC) = NGHCVRG
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NDRYGRG
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE') IPROACTV(NBUDGET_RG, IPROC) = NGMLTRG
  IPROC=IPROC+1
  IF( HCLOUD == 'ICE4' .AND. .NOT. LRED ) IPROACTV(NBUDGET_RG, IPROC) = NWETHRG
  IPROC=IPROC+1
  IF( HCLOUD == 'ICE4' .AND. LRED ) IPROACTV(NBUDGET_RG, IPROC) = NHGCVRG
  IPROC=IPROC+1
  IF( HCLOUD == 'ICE4'  .AND. LRED) IPROACTV(NBUDGET_RG, IPROC) = NDRYHRG
  IPROC=IPROC+1
  IF( HCLOUD(1:3) == 'ICE'  .AND. LRED) IPROACTV(NBUDGET_RG, IPROC) = NCORRRG
  IPROC=IPROC+1
  IF ((HCLOUD(1:3) == 'ICE' .AND. .NOT. LRED).OR. &
     (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LSEDIM_AFTER)) &
          IPROACTV(NBUDGET_RG, IPROC) = NSEDIRG
  IPROC=IPROC+1
END IF
!
  YWORK2(NBUDGET_RG, 1) = 'INIF_'
  YWORK2(NBUDGET_RG, 2) = 'ENDF_'
  YWORK2(NBUDGET_RG, 3) = 'AVEF_'
  IPROC=4
  YWORK2(NBUDGET_RG, IPROC) = 'ASSE_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'NEST_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'FRC_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'DIF_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'REL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'ADV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'NEGA_'
  IPROC=IPROC+1
IF (HCLOUD=='LIMA') THEN
  YWORK2(NBUDGET_RG, IPROC) = 'SEDI_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'HONR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'DEPG_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'RIM_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'ACC_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'CMEL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'CFRZ_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'WETG_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'DRYG_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'HMG_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'GMLT_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'WETH_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'COHG_'
ELSE
  YWORK2(NBUDGET_RG, IPROC) = 'SEDI_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC)= 'SFR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'DEPG_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'RIM_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'ACC_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'CMEL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'CFRZ_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'WETG_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'GHCV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'DRYG_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'GMLT_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'WETH_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'HGCV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'DRYH_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC) = 'CORR_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RG, IPROC)= 'SEDI_'
END IF
!
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
  IPROC=4
  IPROACTV(NBUDGET_RH, IPROC) = NASSERH
  IPROC=IPROC+1
  IF( NMODEL>1 ) THEN
    IPROACTV(NBUDGET_RH, IPROC) = NNESTRH
  ELSE
    IPROACTV(NBUDGET_RH, IPROC) = 3
  END IF
  IPROC=IPROC+1
   IF( LFORCING ) THEN
    IPROACTV(NBUDGET_RH, IPROC)  = NFRCRH
  ELSE
    IPROACTV(NBUDGET_RH, IPROC)  = 3
  END IF 
  IPROC=IPROC+1
  IF( ONUMDIFTH ) THEN
    IPROACTV(NBUDGET_RH, IPROC) = NDIFRH
  ELSE
    IPROACTV(NBUDGET_RH, IPROC) = 3
  END IF
  IPROC=IPROC+1
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
  IPROACTV(NBUDGET_RH, IPROC) = NADVRH
  IPROC=IPROC+1
  IF( HCLOUD /= 'NONE' ) THEN
    IPROACTV(NBUDGET_RH, IPROC) = NNEGARH
  ELSE
    IPROACTV(NBUDGET_RH, IPROC) = 3
  END IF
  IPROC=IPROC+1
!
  IF (HCLOUD=='LIMA' .AND. OHAIL) THEN
     IPROACTV(NBUDGET_RH, IPROC) = NSEDIRH
     IPROC=IPROC+1
     IPROACTV(NBUDGET_RH, IPROC) = NWETGRH
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT) IPROACTV(NBUDGET_RH, IPROC) = NWETHRH
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT) IPROACTV(NBUDGET_RH, IPROC) = NCOHGRH
     IPROC=IPROC+1
     IF (.NOT.OPTSPLIT) IPROACTV(NBUDGET_RH, IPROC) = NHMLTRH
  ELSE
  IF( HCLOUD == 'ICE4' .AND. LRED .AND. .NOT. LSEDIM_AFTER) &
          IPROACTV(NBUDGET_RH, IPROC) = NSEDIRH
  IPROC=IPROC+1
  IF( HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RH, IPROC) = NGHCVRH
  IPROC=IPROC+1
  IF( HCLOUD == 'ICE4' .AND. .NOT. LRED) IPROACTV(NBUDGET_RH, IPROC) = NWETGRH
  IPROC=IPROC+1
  IF( HCLOUD == 'ICE4') IPROACTV(NBUDGET_RH, IPROC) = NWETHRH
  IPROC=IPROC+1
  IF( HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RH, IPROC) = NHGCVRH
  IPROC=IPROC+1
  IF( HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RH, IPROC) = NDRYHRH
  IPROC=IPROC+1
  IF( HCLOUD == 'ICE4' ) IPROACTV(NBUDGET_RH, IPROC) = NHMLTRH
  IPROC=IPROC+1
  IF( HCLOUD == 'ICE4' .AND. LRED) IPROACTV(NBUDGET_RH, IPROC) = NCORRRH
  IPROC=IPROC+1
  IF ((HCLOUD(1:3) == 'ICE' .AND. .NOT. LRED).OR. &
     (HCLOUD(1:3) == 'ICE' .AND. LRED .AND. LSEDIM_AFTER)) &
          IPROACTV(NBUDGET_RH, IPROC) = NSEDIRH
  END IF
!
  YWORK2(NBUDGET_RH, 1) = 'INIF_'
  YWORK2(NBUDGET_RH, 2) = 'ENDF_'
  YWORK2(NBUDGET_RH, 3) = 'AVEF_'
  IPROC=4
  YWORK2(NBUDGET_RH, IPROC) = 'ASSE_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RH, IPROC) = 'NEST_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RH, IPROC) = 'FRC_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RH, IPROC) = 'DIF_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RH, IPROC) = 'REL_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RH, IPROC) = 'ADV_'
  IPROC=IPROC+1
  YWORK2(NBUDGET_RH, IPROC) = 'NEGA_'
  IPROC=IPROC+1
  IF (HCLOUD=='LIMA' .AND. OHAIL) THEN
     YWORK2(NBUDGET_RH, IPROC) = 'SEDI_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RH, IPROC) = 'WETG_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RH, IPROC) = 'WETH_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RH, IPROC) = 'COHG_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RH, IPROC) = 'HMLT_'
     IPROC=IPROC+1
  ELSE
     YWORK2(NBUDGET_RH, IPROC) = 'SEDI_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RH, IPROC) = 'GHCV_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RH, IPROC) = 'WETG_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RH, IPROC) = 'WETH_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RH, IPROC) = 'HGCV_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RH, IPROC) = 'DRYH_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RH, IPROC) = 'HMLT_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RH, IPROC) = 'CORR_'
     IPROC=IPROC+1
     YWORK2(NBUDGET_RH, IPROC) = 'SEDI_'
  END IF
!
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
    IPROC=4
    IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = NASSESV
    IPROC=IPROC+1
    IF( NMODEL>1 ) IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = NNESTSV
    IPROC=IPROC+1
    IF( LFORCING ) IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC)  = NFRCSV
    IPROC=IPROC+1
    IF ( ONUMDIFSV ) IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = NDIFSV
    IPROC=IPROC+1
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
    IF ( (HDCONV /= 'NONE' .OR. HSCONV == 'KAFR') .AND. OCHTRANS ) &
        IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = NDCONVSV
    IPROC=IPROC+1
    IF ( ODRAGTREE .AND. ODEPOTREE  ) &
        IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = NDEPOTRSV
    IPROC=IPROC+1    
    IF ( HTURB /= 'NONE' ) IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC) = NVTURBSV
    IPROC=IPROC+1
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
    IF ( HSCONV == 'EDKF' ) IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC)= NMAFLSV
    IPROC=IPROC+1
    IPROACTV(NBUDGET_SV1 - 1 + JSV, IPROC)= NADVSV
    IPROC=IPROC+1
!
    YWORK2(NBUDGET_SV1 - 1 + JSV, 1) = 'INIF_'
    YWORK2(NBUDGET_SV1 - 1 + JSV, 2) = 'ENDF_'
    YWORK2(NBUDGET_SV1 - 1 + JSV, 3) = 'AVEF_'
    IPROC=4
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'ASSE_'
    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'NEST_'
    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'FRC_'
    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'DIF_'
    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'REL_'
    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'DCONV_'
    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'DEPOTR'
    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'VTURB_'
    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'HTURB_'
    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'MAFL_'
    IPROC=IPROC+1
    YWORK2(NBUDGET_SV1 - 1 + JSV, IPROC) = 'ADV_'
!
! complete with the budget of other processes
!
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
DO JI=1,JPBUMAX                                ! loop on the allowed budgets
                                               ! names of recording files for:
  CBURECORD(JI,1) = ADJUSTL( CBUCOMMENT(JI,1) )   ! initial guess
  CBURECORD(JI,2) = ADJUSTL( CBUCOMMENT(JI,2) )   ! source cumul
  CBURECORD(JI,3) = ADJUSTL( CBUCOMMENT(JI,3) )   ! end step
!
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
        ITEN=INT(NBUPROCNBR(JI)/10)
        CBURECORD(JI,NBUPROCNBR(JI)) = 'S' // CHAR( ITEN + 48 )               &
                  // CHAR(  48+ MODULO( NBUPROCNBR(JI),10*MAX(1,ITEN) )  )    &
                  // '_' // ADJUSTL( YEND_COMMENT(JI) )
      ELSE IF (IPROACTV(JI,JJJ) == 0) THEN
        NBUPROCNBR(JI) = NBUPROCNBR(JI)+1
        CBUACTION(JI,JJ) = 'DD'
        CBUCOMMENT(JI,NBUPROCNBR(JI)) =                            ADJUSTL(    &
                                   ADJUSTR( CBUCOMMENT(JI,NBUPROCNBR(JI)) ) // &
                                   ADJUSTL( ADJUSTR( YWORK2(JI,JJ) ) //        &
                                            ADJUSTL( YEND_COMMENT(JI) ) ) )
        ITEN=INT(NBUPROCNBR(JI)/10)
        CBURECORD(JI,NBUPROCNBR(JI)) = 'S' // CHAR( ITEN + 48 )               &
                  // CHAR(  48+ MODULO( NBUPROCNBR(JI),10*MAX(1,ITEN) )  )    &
                  // '_' // ADJUSTL( YEND_COMMENT(JI) )
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
    IF( JI <= 12 ) THEN
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
                                            YSTRING(1:ILEN),JI-12,NBUPROCNBR(JI)
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
  call Print_msg( NVERB_FATAL, 'GEN', 'INI_BUDGET', '' )
ENDIF
!-------------------------------------------------------------------------------
!*       5.    ALLOCATE MEMORY FOR BUDGET STORAGE ARRAYS
!              -----------------------------------------
IF (LBU_RU) THEN
  ALLOCATE ( XBURU(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(1)) )
  XBURU(:,:,:,:)=0.
  ALLOCATE ( XBURHODJU(IBUDIM1, IBUDIM2, IBUDIM3) )
  XBURHODJU(:,:,:)=0.
END IF
!
IF (LBU_RV) THEN
  ALLOCATE ( XBURV(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(2)) )
  XBURV(:,:,:,:)=0.
  ALLOCATE ( XBURHODJV(IBUDIM1, IBUDIM2, IBUDIM3) )
  XBURHODJV(:,:,:)=0.
END IF
!
IF (LBU_RW) THEN
  ALLOCATE ( XBURW(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(3)) )
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
  ALLOCATE ( XBURTH(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(4)) )
  XBURTH(:,:,:,:)=0.
END IF
!
IF (LBU_RTKE) THEN
  ALLOCATE ( XBURTKE(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(5)) )
  XBURTKE(:,:,:,:)=0.
END IF
!
IF (LBU_RRV) THEN
  ALLOCATE ( XBURRV(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(6)) )
  XBURRV(:,:,:,:)=0.
END IF
!
IF (LBU_RRC) THEN
  ALLOCATE ( XBURRC(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(7)) )
  XBURRC(:,:,:,:)=0.
END IF
!
IF (LBU_RRR) THEN
  ALLOCATE ( XBURRR(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(8)) )
  XBURRR(:,:,:,:)=0.
END IF
!
IF (LBU_RRI) THEN
  ALLOCATE ( XBURRI(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(9)) )
  XBURRI(:,:,:,:)=0.
END IF
!
IF (LBU_RRS) THEN
  ALLOCATE ( XBURRS(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(10)) )
  XBURRS(:,:,:,:)=0.
END IF
!
IF (LBU_RRG) THEN
  ALLOCATE ( XBURRG(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(11)) )
  XBURRG(:,:,:,:)=0.
END IF
!
IF (LBU_RRH) THEN
  ALLOCATE ( XBURRH(IBUDIM1, IBUDIM2, IBUDIM3, NBUPROCNBR(12)) )
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
