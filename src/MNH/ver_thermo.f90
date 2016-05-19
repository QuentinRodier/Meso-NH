!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
!-----------------------------------------------------------------
!     ######spl
      MODULE MODI_VER_THERMO
!     ######################
INTERFACE
      SUBROUTINE VER_THERMO(OSHIFT,                                               &
                            PTHV_MX,PR_MX,PZS_LS,PZSMT_LS,PZMASS_MX,PZFLUX_MX,PPMHP_MX,PJ, &
                            PDXX,PDYY,PEXNTOP2D,PPSURF,PDIAG,                     &
                            PLSTH_MX,PLSRV_MX                                     )
!
LOGICAL,                  INTENT(IN)  :: OSHIFT     ! T: vertical shift of BL (used for GRIB file data)
!                                                   ! F: no vertical shift (used for MESONH data)
REAL,   DIMENSION(:,:,:), INTENT(IN)     :: PTHV_MX   ! thetav on mixed grid
REAL,   DIMENSION(:,:,:,:), INTENT(IN)   :: PR_MX     ! r on mixed grid
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PZMASS_MX ! mass point altitudes on
!                                                  ! mixed grid
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PZFLUX_MX ! flux point altitudes on
!                                                  ! mixed grid
REAL,   DIMENSION(:,:,:), INTENT(IN)     :: PPMHP_MX  ! pressure minus hyd. pressure
REAL,   DIMENSION(:,:)  , INTENT(IN)  :: PZS_LS    ! large scale orography
REAL,   DIMENSION(:,:)  , INTENT(IN)  :: PZSMT_LS  ! large scale smooth orography
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PJ        ! Jacobian
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PDXX      ! metric coefficient dxx
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PDYY      ! metric coefficient dyy
REAL,   DIMENSION(:,:)  , INTENT(IN)     :: PEXNTOP2D ! top Exner function
REAL,   DIMENSION(:,:)  , INTENT(OUT)     :: PPSURF    ! Surface pressure
REAL,                     INTENT(OUT) :: PDIAG     ! diagnostics computing time
REAL,DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: PLSTH_MX ! large scale potential temperature
REAL,DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: PLSRV_MX ! large scale vapor mixing ratios
!
END SUBROUTINE VER_THERMO
END INTERFACE
END MODULE MODI_VER_THERMO
!     ######spl
      SUBROUTINE VER_THERMO(OSHIFT,                                               &
                            PTHV_MX,PR_MX,PZS_LS,PZSMT_LS,PZMASS_MX,PZFLUX_MX,PPMHP_MX,PJ, &
                            PDXX,PDYY,PEXNTOP2D,PPSURF,PDIAG,                     &
                            PLSTH_MX,PLSRV_MX                                     )
!     ######################################################################
!
!!****  *VER_THERMO* - initializes the thermodynamic and reference state
!!                     fields in MESO-NH for a real case from Aladin fields.
!!
!!    PURPOSE
!!    -------
!!    This routine initializes the potential temperature and the vapor mixing
!!    ratio on the MESO-NH grid from the fields on the mixed grid
!!    defined by the altitudes of its mass points.
!!    The other mixing ratio, if any, are initialized to zero.
!!    The reference state variables and the total dry mass are also computed.
!!
!!**  METHOD
!!    ------
!!
!!  1 The initialization of thetav and rv is performed in VER_INT_THERMO
!!
!!  2 theta is deduced:
!!                   1 + rv
!!   theta=thetav *------------------
!!                   1 + Rv/Rd*rv
!!
!!  3 The reference anelastic state variables are computed in SET_REFZ
!!
!!  4 The total dry mass is computed in DRY_MASS
!!    ____
!!  5 rhod J* (XRHODJ) is computed in SET_REF
!!
!!  6 theta and rv are stored in XTHT and XRT
!!
!!    EXTERNAL
!!    --------
!!    subroutine VER_INT_THERMO : to initialize thetav and rv
!!    subroutine SET_REFZ       : to initialize the reference state 1D variables
!!    subroutine TOTAL_DMASS    : to compute the total dry mass
!!    subroutine SET_REF        : to compute  rhoJ
!!    subroutine FMLOOK         : to retrieve a logical unit number associated
!!                                with a file
!!
!!    Module MODI_VER_INT_THERMO: interface for subroutine VER_INT_THERMO
!!    Module MODI_SET_REFZ      : interface for subroutine SET_REFZ
!!    Module MODI_TOTAL_DMASS   : interface for subroutine TOTAL_DMASS
!!    Module MODI_SET_REF       : interface for subroutine SET_REF
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_CONF      : contains configuration variables for all models.
!!         NVERB   : verbosity level for output-listing
!!      Module MODD_CONF1     : contains configuration variables for model 1.
!!         NRR     : number of moist variables
!!      Module MODD_LUNIT     :  contains logical unit names for all models
!!         CLUOUT0 : name of output-listing
!!      Module MODD_CST       : contains physical constants
!!         XRD : gas constant for dry air
!!         XRV : gas constant for vapor
!!         XG  : gravity constant
!!      Module MODD_GRID1     : contains grid variables
!!         XZZ :
!!         XZHAT:
!!      Module MODD_REF1      : contains 3D reference state variables for model1
!!         XRHODJ      :
!!         XTHVREF     :
!!         XEXNREF     :
!!         XREFMASS    : Mass of the ref. atmosphere contained in the simulation
!                        domain
!!         XMASS_O_PHI0: normalization constant used in the PHI0 computation
!!         XLINMASS    : Lineic mass through open boundaries
!!      Module MODD_FIELD1    : contains prognostics  variables
!!         XTHM : theta (:,:,:)         at t-dt
!!         XRM  : r (:,:,:,:)           at t-dt
!!      Module MODD_LBC1 : contains declaration of lateral boundary conditions
!!         CLBCX   : X-direction LBC type at left(1) and right(2) boundaries
!!         CLBCY   : Y-direction LBC type at left(1) and right(2) boundaries
!!
!!    REFERENCE
!!    ---------
!!
!!      Book 2
!!
!!    AUTHOR
!!    ------
!!
!!      V.Masson  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    13/12/94
!!                  Sept. 21, 1995  (J.Stein and V.Masson) surface pressure
!!                  Jan.  09, 1996  (V. Masson) pressure function deduced from
!!                                  hydrostatic pressure
!!                  Aug.  20, 1996  (V. Masson) change call to DRY_MASS,
!!                                  correction in XTHM computation
!!                  Oct.  25, 1996  (V. Masson) add deallocations
!!                  Jan   15, 1997  (Stein,Lafore) Durran anelastic equation
!!                  Jun   10, 1997  (V. Masson) add NH pressure
!!                  Jan.  25, 1998  (Stein) add the LS fields' treatment
!!                  Jun.  06, 2006  (Mallet) replace DRY_MASS by TOTAL_DMASS
!!                     October 2009 (G. Tanguy) add ILENCH=LEN(YCOMMENT) after
!!                                              change of YCOMMENT
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_VER_INT_THERMO ! interface modules
USE MODI_SET_REFZ
USE MODI_TOTAL_DMASS
USE MODI_SET_REF
USE MODI_COMPUTE_EXNER_FROM_TOP
USE MODI_WATER_SUM
!
USE MODD_CONF           ! declaration modules
USE MODD_CONF_n
USE MODD_LUNIT
USE MODD_CST
USE MODD_FIELD_n, ONLY: XTHM,XRM,XPABSM,XDRYMASST
USE MODD_LSFIELD_n
USE MODD_DYN_n
USE MODD_REF_n
USE MODD_GRID_n
USE MODD_LBC_n
USE MODD_LUNIT_n
USE MODD_PARAMETERS
!
!
USE MODE_FMWRIT
USE MODE_FM
!JUAN REALZ
USE MODD_DIM_n
USE MODE_MPPDB
USE MODE_ll
USE MODE_EXTRAPOL
!JUAN REALZ
!
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
LOGICAL,                  INTENT(IN)  :: OSHIFT     ! T: vertical shift of BL (used for GRIB file data)
!                                                   ! F: no vertical shift (used for MESONH data)
REAL,   DIMENSION(:,:,:), INTENT(IN)     :: PTHV_MX   ! thetav on mixed grid
REAL,   DIMENSION(:,:,:,:), INTENT(IN)   :: PR_MX     ! r on mixed grid
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PZMASS_MX ! mass point altitudes on
!                                                  ! mixed grid
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PZFLUX_MX ! flux point altitudes on
!                                                  ! mixed grid
REAL,   DIMENSION(:,:,:), INTENT(IN)     :: PPMHP_MX  ! pressure minus hyd. pressure
REAL,   DIMENSION(:,:)  , INTENT(IN)  :: PZS_LS    ! mixed grid orography
REAL,   DIMENSION(:,:)  , INTENT(IN)  :: PZSMT_LS  ! large scale smooth orography
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PJ        ! Jacobian
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PDXX      ! metric coefficient dxx
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PDYY      ! metric coefficient dyy
REAL,   DIMENSION(:,:)  , INTENT(IN)     :: PEXNTOP2D ! top Exner function
REAL,   DIMENSION(:,:)  , INTENT(OUT)     :: PPSURF    ! Surface pressure
REAL,                     INTENT(OUT) :: PDIAG     ! diagnostics computing time
REAL,DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: PLSTH_MX ! large scale potential temperature
REAL,DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: PLSRV_MX ! large scale vapor mixing ratios
!
!*       0.2   Declaration of local variables
!              ------------------------------
INTEGER                                          :: ILUOUT0, IRESP
INTEGER                                          :: ILBX,ILBY
INTEGER                                          :: IIB,IIE,IIU
INTEGER                                          :: IJB,IJE,IJU
INTEGER                                          :: IKB,IKE,IKU
INTEGER                                          :: JRR
REAL, DIMENSION(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3)):: ZTHV
!                                                  ! virtual potential temperature
!                                                  ! on MESONH grid
REAL,DIMENSION(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3))   :: ZHEXNFLUX,ZHEXNMASS,ZPMHP
REAL,DIMENSION(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3))   :: ZRHOD,ZSUMRT
!
CHARACTER(LEN=100) :: YCOMMENT
INTEGER           :: ILENCH         ! ILENCH : length of comment string 
CHARACTER(LEN=16)  :: YRECFM
!JUAN REALZ
INTEGER      :: IINFO_ll
TYPE(LIST_ll), POINTER :: TZFIELDS_ll => NULL()  ! list of fields to exchange
!
INTEGER :: IISIZEXF,IJSIZEXF,IISIZEXFU,IJSIZEXFU     ! dimensions of the
INTEGER :: IISIZEX4,IJSIZEX4,IISIZEX2,IJSIZEX2       ! West-east LB arrays
INTEGER :: IISIZEYF,IJSIZEYF,IISIZEYFV,IJSIZEYFV     ! dimensions of the
INTEGER :: IISIZEY4,IJSIZEY4,IISIZEY2,IJSIZEY2       ! North-south LB arrays

!JUAN REALZ
!-------------------------------------------------------------------------------
!
IIB=JPHEXT+1
IIE=SIZE(PJ,1)-JPHEXT
IIU=SIZE(PJ,1)
IJB=JPHEXT+1
IJE=SIZE(PJ,2)-JPHEXT
IJU=SIZE(PJ,2)
IKB=JPVEXT+1
IKE=SIZE(PJ,3)-JPVEXT
IKU=SIZE(PJ,3)
!
!-------------------------------------------------------------------------------
!
!*       1.    SHIFT AND INTERPOLATION TO MESONH GRID
!              --------------------------------------
!
ALLOCATE(XTHM(IIU,IJU,IKU))
ALLOCATE(XRM(IIU,IJU,IKU,NRR))

CALL MPPDB_CHECK3D(PTHV_MX,"ver_thermo:PTHV_MX",PRECISION)
CALL MPPDB_CHECK3D(PR_MX(:,:,:,1),"ver_thermo:PR_MX",PRECISION)
CALL MPPDB_CHECK2D(PZS_LS,"ver_thermo:PZS_LS",PRECISION)
CALL MPPDB_CHECK2D(PZSMT_LS,"ver_thermo:PZSMT_LS",PRECISION)
CALL MPPDB_CHECK3D(PZMASS_MX,"ver_thermo:PZMASS_MX",PRECISION)
CALL MPPDB_CHECK3D(PZFLUX_MX,"ver_thermo:PZFLUX_MX",PRECISION)
CALL MPPDB_CHECK3D(PPMHP_MX,"ver_thermo:PPMHP_MX",PRECISION)
CALL MPPDB_CHECK2D(PEXNTOP2D,"ver_thermo:PEXNTOP2D",PRECISION)


IF ( PRESENT(PLSTH_MX)) THEN
  ALLOCATE(XLSTHM(IIU,IJU,IKU))
  ALLOCATE(XLSRVM(IIU,IJU,IKU))
  CALL MPPDB_CHECK3D(PLSTH_MX,"PLSTH_MX",PRECISION)
  CALL MPPDB_CHECK3D(PLSRV_MX,"PLSRV_MX",PRECISION)
  !
  CALL VER_INT_THERMO(OSHIFT,PTHV_MX,PR_MX,PZS_LS,PZSMT_LS,PZMASS_MX,PZFLUX_MX,PPMHP_MX,PEXNTOP2D, &
                      ZTHV,XRM,ZPMHP,PDIAG,PLSTH_MX,PLSRV_MX,XLSTHM,XLSRVM)
ELSE
  CALL VER_INT_THERMO(OSHIFT,PTHV_MX,PR_MX,PZS_LS,PZSMT_LS,PZMASS_MX,PZFLUX_MX,PPMHP_MX,PEXNTOP2D, &
                      ZTHV,XRM,ZPMHP,PDIAG)
END IF
!
XTHM(:,:,:)=ZTHV(:,:,:)*(1.+WATER_SUM(XRM(:,:,:,:)))/(1.+XRV/XRD*XRM(:,:,:,1))
!
ZTHV(:,:,1)=ZTHV(:,:,2)
XTHM(:,:,1)=XTHM(:,:,2)
XRM(:,:,1,:)=XRM(:,:,2,:)
!
IF (NRR>=3) THEN
  WHERE  (XRM(:,:,:,3)<1.E-20)
    XRM(:,:,:,3)=0.
  END WHERE
END IF

!CALL EXTRAPOL('W',XTHM)
!CALL EXTRAPOL('S',XTHM) 
CALL EXTRAPOL('E',XTHM)

CALL MPPDB_CHECK3D(XTHM,"VERTHERMO::XTHM",PRECISION)

DO JRR=1,SIZE(XRM,4)
  CALL EXTRAPOL('E',XRM(:,:,:,JRR))
END DO
!
IF (NVERB>=10) THEN
  YRECFM='THV'
  YCOMMENT='X_Y_Z_THV (K)'
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(CINIFILE,YRECFM,CLUOUT0,'XY',ZTHV(:,:,:),4,ILENCH,YCOMMENT,IRESP)
END IF
!-------------------------------------------------------------------------------
!
!*       2.    COMPUTATION OF 1D REFERENCE STATE VARIABLES
!              -------------------------------------------
!
CALL SET_REFZ(ZTHV,XRM(:,:,:,1))

CALL MPPDB_CHECK3D(ZTHV,"VERTHERMO::ZTHV",PRECISION)
CALL MPPDB_CHECK3D(XRM(:,:,:,1),"VERTHERMO::XRM",PRECISION)
!
!-------------------------------------------------------------------------------
!
!*       3.    COMPUTATION OF 3D REFERENCE STATE VARIABLES
!              -------------------------------------------
!
ALLOCATE(XRHODREF(IIU,IJU,IKU))
ALLOCATE(XTHVREF(IIU,IJU,IKU))
ALLOCATE(XRVREF(IIU,IJU,IKU))
ALLOCATE(XEXNREF(IIU,IJU,IKU))
ALLOCATE(XRHODJ(IIU,IJU,IKU))
CALL SET_REF(0,'NIL',CLUOUT0,XZZ,XZHAT,PJ,PDXX,PDYY,CLBCX,CLBCY,     &
             XREFMASS,XMASS_O_PHI0,XLINMASS,XRHODREF,XTHVREF,XRVREF, &
             XEXNREF,XRHODJ)

CALL ADD3DFIELD_ll(TZFIELDS_ll, XRHODREF)
CALL ADD3DFIELD_ll(TZFIELDS_ll, XTHVREF)
CALL ADD3DFIELD_ll(TZFIELDS_ll, XRVREF)
CALL ADD3DFIELD_ll(TZFIELDS_ll, XEXNREF)
CALL ADD3DFIELD_ll(TZFIELDS_ll, XRHODJ)
CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
CALL CLEANLIST_ll(TZFIELDS_ll)

CALL MPPDB_CHECK3D(XRHODREF,"VERTHERMO::XRHODREF",PRECISION)
CALL MPPDB_CHECK3D(XTHVREF,"VERTHERMO::XTHVREF",PRECISION)
CALL MPPDB_CHECK3D(XRVREF,"VERTHERMO::XRVREF",PRECISION)
CALL MPPDB_CHECK3D(XEXNREF,"VERTHERMO::XEXNREF",PRECISION)
CALL MPPDB_CHECK3D(XRHODJ,"VERTHERMO::XRHODJ",PRECISION)


!
!-------------------------------------------------------------------------------
!
!*       4.    PRESSURE
!              --------
!
CALL COMPUTE_EXNER_FROM_TOP(ZTHV,XZZ,PEXNTOP2D,ZHEXNFLUX,ZHEXNMASS)
!
PPSURF(:,:) = 1.5*ZPMHP(:,:,JPVEXT+1) - 0.5*ZPMHP(:,:,JPVEXT+2) &
             + XP00*ZHEXNFLUX(:,:,JPVEXT+1) ** (XCPD/XRD)
!
ALLOCATE(XPABSM(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3)))
XPABSM(:,:,:)=ZPMHP(:,:,:) + XP00*ZHEXNMASS(:,:,:) ** (XCPD/XRD)

CALL EXTRAPOL('E',XPABSM)
!
!-------------------------------------------------------------------------------
!
!*       5.    COMPUTATION OF TOTAL DRY MASS
!              -----------------------------
!
ZSUMRT(:,:,:) = 0.
DO JRR=1,SIZE(XRM,4)
  ZSUMRT(:,:,:) = ZSUMRT(:,:,:) + XRM(:,:,:,JRR)
END DO
!
ZRHOD(:,:,:)=XPABSM(:,:,:)/(XPABSM(:,:,:)/XP00)**(XRD/XCPD) &
            /(XRD*ZTHV(:,:,:)*(1.+ZSUMRT(:,:,:)))
!
CALL TOTAL_DMASS(CLUOUT0,PJ,ZRHOD,XDRYMASST)
!
!-------------------------------------------------------------------------------
!
!*       7.    LARGE SCALE FIELDS INITIALIZATIONS
!              ----------------------------------
!
                                 ! 3D case
!
  CALL GET_SIZEX_LB(CLUOUT,NIMAX_ll,NJMAX_ll,NRIMX,   &
       IISIZEXF,IJSIZEXF,IISIZEXFU,IJSIZEXFU,         &
       IISIZEX4,IJSIZEX4,IISIZEX2,IJSIZEX2)
  CALL GET_SIZEY_LB(CLUOUT,NIMAX_ll,NJMAX_ll,NRIMY,   &
       IISIZEYF,IJSIZEYF,IISIZEYFV,IJSIZEYFV,         &
       IISIZEY4,IJSIZEY4,IISIZEY2,IJSIZEY2)

IF ( .NOT. PRESENT(PLSTH_MX) ) THEN
  ALLOCATE(XLSTHM(IIU,IJU,IKU))
  ALLOCATE(XLSRVM(IIU,IJU,IKU))
  XLSTHM=XTHM
  XLSRVM=XRM(:,:,:,1)
END IF
! copy at the external levels
XLSTHM(:,:,IKB-1)=XLSTHM(:,:,IKB)
XLSTHM(:,:,IKE+1)=XLSTHM(:,:,IKE)
XLSRVM(:,:,IKB-1)=XLSRVM(:,:,IKB)
XLSRVM(:,:,IKE+1)=XLSRVM(:,:,IKE)
!
CALL EXTRAPOL('E',XLSTHM,XLSRVM)
!
IF ( LHORELAX_UVWTH ) THEN
    NSIZELBX_ll=2*NRIMX+2
    NSIZELBXU_ll=2*NRIMX+2
    NSIZELBY_ll=2*NRIMY+2
    NSIZELBYV_ll=2*NRIMY+2
   ALLOCATE(XLBXTHM(IISIZEXF,IJSIZEXF,IKU))
   ALLOCATE(XLBYTHM(IISIZEYF,IJSIZEYF,IKU))
!!$  ALLOCATE(XLBXTHM(2*NRIMX+2,IJU,IKU))
!!$  ALLOCATE(XLBYTHM(IIU,2*NRIMY+2,IKU))
ELSE
    NSIZELBX_ll=2
    NSIZELBXU_ll=4
    NSIZELBY_ll=2
    NSIZELBYV_ll=4
   ALLOCATE(XLBXTHM(IISIZEX2,IJSIZEX2,IKU))
   ALLOCATE(XLBYTHM(IISIZEY2,IJSIZEY2,IKU))
!!$  ALLOCATE(XLBXTHM(2,IJU,IKU))
!!$  ALLOCATE(XLBYTHM(IIU,2,IKU))
END IF
!
!!$ILBX=SIZE(XLBXTHM,1)/2-1
!!$XLBXTHM(1:ILBX+1,:,:)         = XTHM(IIB-1:IIB-1+ILBX,:,:)
!!$XLBXTHM(ILBX+2:2*ILBX+2,:,:)  = XTHM(IIE+1-ILBX:IIE+1,:,:)
!!$ILBY=SIZE(XLBYTHM,2)/2-1
!!$XLBYTHM(:,1:ILBY+1,:)        = XTHM(:,IJB-1:IJB-1+ILBY,:)
!!$XLBYTHM(:,ILBY+2:2*ILBY+2,:) = XTHM(:,IJE+1-ILBY:IJE+1,:)
!
IF ( NRR > 0 ) THEN
  IF (       LHORELAX_RV .OR. LHORELAX_RC .OR. LHORELAX_RR .OR. LHORELAX_RI    &
        .OR. LHORELAX_RS .OR. LHORELAX_RG .OR. LHORELAX_RH                     &
     ) THEN
!!$    ALLOCATE(XLBXRM(2*NRIMX+2,IJU,IKU,NRR))
!!$    ALLOCATE(XLBYRM(IIU,2*NRIMY+2,IKU,NRR))
!!$  ELSE
!!$    ALLOCATE(XLBXRM(2,IJU,IKU,NRR))
!!$    ALLOCATE(XLBYRM(IIU,2,IKU,NRR))
      NSIZELBXR_ll=2*NRIMX+2
      NSIZELBYR_ll=2*NRIMY+2
      ALLOCATE(XLBXRM(IISIZEXF,IJSIZEXF,IKU,NRR))
      ALLOCATE(XLBYRM(IISIZEYF,IJSIZEYF,IKU,NRR))
    ELSE
      NSIZELBXR_ll=2
      NSIZELBYR_ll=2
      ALLOCATE(XLBXRM(IISIZEX2,IJSIZEX2,IKU,NRR))
      ALLOCATE(XLBYRM(IISIZEY2,IJSIZEY2,IKU,NRR))
  ENDIF
  !
  IF (SIZE(XLBXRM) .NE. 0 ) THEN
     ILBX=SIZE(XLBXRM,1)/2-1
     XLBXRM(1:ILBX+1,:,:,:)         = XRM(IIB-1:IIB-1+ILBX,:,:,:)
     XLBXRM(ILBX+2:2*ILBX+2,:,:,:)  = XRM(IIE+1-ILBX:IIE+1,:,:,:)
  ENDIF
  IF (SIZE(XLBYRM) .NE. 0 ) THEN
     ILBY=SIZE(XLBYRM,2)/2-1
     XLBYRM(:,1:ILBY+1,:,:)        = XRM(:,IJB-1:IJB-1+ILBY,:,:)
     XLBYRM(:,ILBY+2:2*ILBY+2,:,:) = XRM(:,IJE+1-ILBY:IJE+1,:,:)
  ENDIF
ELSE
   NSIZELBXR_ll=0
   NSIZELBYR_ll=0
   ALLOCATE(XLBXRM(0,0,0,0))
   ALLOCATE(XLBYRM(0,0,0,0))
END IF
!
!!$NSIZELBXR_ll=SIZE(XLBXRM,1)
!!$NSIZELBYR_ll=SIZE(XLBYRM,2)   !! coding for one processor

ILBX=SIZE(XLBXTHM,1)
ILBY=SIZE(XLBYTHM,2)
IF(LWEST_ll() .AND. .NOT. L1D) THEN
  XLBXTHM(1:NRIMX+1,        :,:)   = XTHM(1:NRIMX+1,        :,:)
  XLBXRM(1:NRIMX+1,        :,:,:)   = XRM(1:NRIMX+1,        :,:,:)
ENDIF
IF(LEAST_ll() .AND. .NOT. L1D) THEN
  XLBXTHM(ILBX-NRIMX:ILBX,:,:)   = XTHM(IIU-NRIMX:IIU,    :,:)
  XLBXRM(ILBX-NRIMX:ILBX,:,:,:)   = XRM(IIU-NRIMX:IIU,    :,:,:)
ENDIF
IF(LSOUTH_ll() .AND. .NOT. L1D .AND. .NOT. L2D) THEN
  XLBYTHM(:,1:NRIMY+1,        :)    = XTHM(:,1:NRIMY+1,      :)
  XLBYRM(:,1:NRIMY+1,        :,:)   = XRM(:,1:NRIMY+1,      :,:)
ENDIF
IF(LNORTH_ll().AND. .NOT. L1D .AND. .NOT. L2D) THEN
  XLBYTHM(:,ILBY-NRIMY:ILBY,:)    = XTHM(:,IJU-NRIMY:IJU,  :)
  XLBYRM(:,ILBY-NRIMY:ILBY,:,:)   = XRM(:,IJU-NRIMY:IJU,  :,:)
ENDIF
!
!-------------------------------------------------------------------------------
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
WRITE(ILUOUT0,*) 'Routine VER_THERMO completed'
!
END SUBROUTINE VER_THERMO
