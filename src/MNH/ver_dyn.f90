!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
!-----------------------------------------------------------------
!     ######spl
      MODULE MODI_VER_DYN
!     ###################
INTERFACE
      SUBROUTINE VER_DYN(OSHIFT,                                                &
                         PU_MX,PV_MX,PW_MX,PRHOD_MX,PZFLUX_MX,PZMASS_MX,PZS_LS, &
                         PDXX,PDYY,PDZZ,PDZX,PDZY,PJ,HATMFILETYPE,              &
                         PLSU_MX,PLSV_MX,PLSW_MX                                )
!
LOGICAL,                  INTENT(IN)  :: OSHIFT     ! T: vertical shift of BL (used for GRIB file data)
!                                                   ! F: no vertical shift (used for MESONH data)
REAL,   DIMENSION(:,:,:), INTENT(IN)     :: PU_MX     ! U on LS grid
REAL,   DIMENSION(:,:,:), INTENT(IN)     :: PV_MX     ! V on LS grid
REAL,   DIMENSION(:,:,:), INTENT(IN)     :: PW_MX     ! W on LS grid
REAL,   DIMENSION(:,:,:), INTENT(IN)     :: PRHOD_MX  ! local rhod on mixed grid
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PZFLUX_MX ! altitude of pressure
!                                                  ! points on mixed grid
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PZMASS_MX ! altitude of mass
!                                                  ! points on mixed grid
REAL,   DIMENSION(:,:),   INTENT(IN)  :: PZS_LS    ! large scale orography
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PDXX      ! metric coefficient dxx
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PDYY      ! metric coefficient dyy
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PDZZ      ! metric coefficient dzz
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PDZX      ! metric coefficient dzx
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PDZY      ! metric coefficient dzy
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PJ        ! jacobian
CHARACTER(LEN=6)                  :: HATMFILETYPE  ! type of the Atmospheric file
REAL,DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: PLSU_MX ! large scale U component
REAL,DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: PLSV_MX ! large scale V component
REAL,DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: PLSW_MX ! large scale W component
!
END SUBROUTINE VER_DYN
END INTERFACE
END MODULE MODI_VER_DYN

!     ######spl
      SUBROUTINE VER_DYN(OSHIFT,                                               &
                         PU_MX,PV_MX,PW_MX,PRHOD_MX,PZFLUX_MX,PZMASS_MX,PZS_LS,&
                         PDXX,PDYY,PDZZ,PDZX,PDZY,PJ,HATMFILETYPE,             &
                         PLSU_MX,PLSV_MX,PLSW_MX                               )
!     ##########################################################################
!
!!****  *VER_DYN* -  initializes dynamical fields in MESO-NH for a real case
!!                   from Aladin fields.
!!
!!    PURPOSE
!!    -------
!!    This routine initializes the three components of the momentum
!!    on the MESO-NH Arakawa C-grid from the horizontal fields on the mixed
!!    Arakawa A-grid defined by the altitudes of its pressure and
!!    mass points.
!!
!!**  METHOD
!!    ------
!!
!!  1 The values of wind components are multiplied by the density to obtain
!!    the horizontal momentum components (ZRHODU_MX,ZRHODV_MX).
!!
!!  2 The first guess of horizontal momentum is initialized in VER_INT_DYN on
!!    the Arakawa A-grid (ZRHODUA,ZRHODVA).
!!
!!  3 The values on the Arakawa C-grid are deduced (ZRHODU,ZRHODV)
!!
!!  4 The first guess of vertical momentum is the large scale field, which is
!!    absurd near the ground.
!!    At the time being, there is a forcing to zero value in the upper quarter
!!    of the domain
!!
!!  5 The interpolated fields ZRHODJU, ZRHODJV are placed in the
!!    module MODD_FIELD1 in place of the prognostic variables XUM, XVM.
!!
!!
!!    EXTERNAL
!!    --------
!!    subroutine VER_INT_DYN    : to initialize the horizontal momentum
!!    subroutine WGUESS         : to initialize vertical momentum
!!    subroutine ANEL_BALANCE1  : to apply the anelastic correction
!!    functions MXM ,MYM ,MZM   : Shuman operators
!!    subroutine FMLOOK         : to retrieve a logical unit number associated
!!                                with a file
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_LUNIT     :  contains logical unit names for all models
!!         CLUOUT0 : name of output-listing
!!      Module MODD_FIELD1    : contains prognostics  variables
!!         XUM : U (:,:,:)         at t-dt
!!         XVM : V (:,:,:,:)       at t-dt
!!         XWM : w (:,:,:,:)       at t-dt
!!      Module MODD_LBC1      : contains lateral boundary conditions
!!         CLBCX   : X-direction LBC type at left(1) and right(2) boundaries
!!         CLBCY   : Y-direction LBC type at left(1) and right(2) boundaries
!!      Module MODD_REF1      : contains 3D reference state variables for model1
!!         XRHODJ  : rhod * Jacobian
!!         XTHVREF_STAR : THvref * (1 + Rvref)
!!      Module MODD_PARAMETERS:
!!         JPVEXT
!!      Module MODD_GRID1    :
!!         XZZ   : height of the w-points
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
!!      Original    15/12/94
!!      J.Stein and J.P. Lafore   18/04/96  change the anel_balance CALL
!!      J.Stein                   15/05/96  change the wguess CALL
!!      V.Masson                  11/10/96  L1D and L2D configurations
!!      V.Masson                  12/12/96  add LS vertical wind velocity
!!      Stein,Lafore              15/01/97  Durran anelastic equation
!!      V.Masson                  26/08/97  call to new linear vertical
!!                                          interpolation routine
!!      V.Masson                  24/11/97  use of the 3D dry density
!!      J.Stein                   20:01/98  add the LS field interpolation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_FM
!
USE MODI_COEF_VER_INTERP_LIN ! interface modules
USE MODI_VER_INTERP_LIN
USE MODI_WGUESS
USE MODI_VER_SHIFT
USE MODI_VER_INT_DYN
USE MODI_ANEL_BALANCE_n
USE MODI_SHUMAN
!
USE MODD_CONF           ! declaration modules
USE MODD_CST
USE MODD_LUNIT
USE MODD_FIELD_n, ONLY: XUM,XVM,XWM,XPABSM,XTHM,XRM
USE MODD_LSFIELD_n
USE MODD_LBC_n
USE MODD_REF_n
USE MODD_DYN_n
USE MODD_GRID_n
USE MODD_PARAMETERS
USE MODD_VER_INTERP_LIN
!JUAN REALZ
USE MODD_DIM_n
USE MODE_MPPDB
USE MODE_ll
USE MODE_EXTRAPOL
!JUAN REALZ
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
!
LOGICAL,                  INTENT(IN)  :: OSHIFT     ! T: vertical shift of BL (used for GRIB file data)
!                                                   ! F: no vertical shift (used for MESONH data)
REAL,   DIMENSION(:,:,:), INTENT(IN)     :: PU_MX     ! U on LS grid
REAL,   DIMENSION(:,:,:), INTENT(IN)     :: PV_MX     ! V on LS grid
REAL,   DIMENSION(:,:,:), INTENT(IN)     :: PW_MX     ! W on LS grid
REAL,   DIMENSION(:,:,:), INTENT(IN)     :: PRHOD_MX  ! local rhod on mixed grid
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PZFLUX_MX ! altitude of pressure
!                                                  ! points on mixed grid
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PZMASS_MX ! altitude of mass
!                                                  ! points on mixed grid
REAL,   DIMENSION(:,:),   INTENT(IN)  :: PZS_LS    ! large scale orography
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PDXX      ! metric coefficient dxx
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PDYY      ! metric coefficient dyy
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PDZZ      ! metric coefficient dzz
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PDZX      ! metric coefficient dzx
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PDZY      ! metric coefficient dzy
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PJ        ! jacobian
CHARACTER(LEN=6)                  :: HATMFILETYPE  ! type of the Atmospheric file
REAL,DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: PLSU_MX ! large scale U component
REAL,DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: PLSV_MX ! large scale V component
REAL,DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: PLSW_MX ! large scale W component
!
!*       0.2   Declaration of local variables
!              ------------------------------
REAL,DIMENSION(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3))    :: ZRHODU_MX, ZRHODV_MX
!                  ! horizontal momentum components on the mixed grid
REAL,DIMENSION(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3))    :: ZRHODUA, ZRHODVA
!                  ! horizontal momentum components on the MESONH Arakawa A grid
REAL,DIMENSION(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3))    :: ZRHODJU, ZRHODJV
!                  ! momentum components on the MESONH Arakawa C grid
REAL,DIMENSION(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3))    :: ZRHOD
!                  ! dry density on MESO-NH grid
REAL,DIMENSION(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3))    :: ZZFLUX_SH
                   ! shifted flux grid
REAL,DIMENSION(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3))    ::  ZCOEF
                  ! coefficient  for weight function
!
INTEGER :: IIB,IIE,IIU
INTEGER :: IJB,IJE,IJU
INTEGER :: IKB,IKE,IKU
INTEGER :: ILBX,ILBY
INTEGER :: IRESP   ! return code if problem eraised in FM routines
INTEGER :: ILUOUT0 ! logical unit for output listing for all models
!
INTEGER :: IISIZEXF,IJSIZEXF,IISIZEXFU,IJSIZEXFU     ! dimensions of the
INTEGER :: IISIZEX4,IJSIZEX4,IISIZEX2,IJSIZEX2       ! West-east LB arrays
INTEGER :: IISIZEYF,IJSIZEYF,IISIZEYFV,IJSIZEYFV     ! dimensions of the
INTEGER :: IISIZEY4,IJSIZEY4,IISIZEY2,IJSIZEY2       ! North-south LB arrays

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
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
!
!-------------------------------------------------------------------------------
!
!*       1.    COMPUTATION OF MOMENTUM ON THE MIXED GRID
!              -----------------------------------------
!
ZRHODU_MX=PU_MX*PRHOD_MX
ZRHODV_MX=PV_MX*PRHOD_MX
!
!-------------------------------------------------------------------------------
!
!*       2.    INITIALIZATION OF HORIZONTAL MOMENTUM ON MESO-NH ARAKAWA A-GRID
!              ---------------------------------------------------------------
!
CALL VER_INT_DYN(OSHIFT,ZRHODU_MX,ZRHODV_MX,PZFLUX_MX,PZMASS_MX,PZS_LS,ZRHODUA,ZRHODVA)
!
CALL EXTRAPOL('E',ZRHODUA,ZRHODVA)

CALL MPPDB_CHECK3D(ZRHODUA,"VERDYN::ZRHODUA",PRECISION)
CALL MPPDB_CHECK3D(ZRHODVA,"VERDYN::ZRHODVA",PRECISION)
!
!-------------------------------------------------------------------------------
!
!*       3.    CHANGE TO ARAKAWA C-GRID
!              ------------------------
!
ZRHODJU(:,:,:)=MXM(ZRHODUA(:,:,:)*PJ(:,:,:))
ZRHODJV(:,:,:)=MYM(ZRHODVA(:,:,:)*PJ(:,:,:))

CALL EXTRAPOL('W',ZRHODJU)
CALL EXTRAPOL('S',ZRHODJV)

CALL MPPDB_CHECK3D(ZRHODJU,"VERDYN::ZRHODJU",PRECISION)
CALL MPPDB_CHECK3D(ZRHODJV,"VERDYN::ZRHODJV",PRECISION)

!
!-------------------------------------------------------------------------------
!
!*       4.    STORAGE IN MODD_FIELD1
!              ----------------------
!
ALLOCATE(XUM(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3)))
ALLOCATE(XVM(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3)))
ALLOCATE(XWM(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3)))
!
ZRHOD(:,:,:)=XPABSM(:,:,:)/(XPABSM(:,:,:)/XP00)**(XRD/XCPD) &
            /(XRD*XTHM(:,:,:)*(1.+XRV/XRD*XRM(:,:,:,1)))
!
XUM(:,:,:)=ZRHODJU(:,:,:)/MXM(ZRHOD(:,:,:)*PJ(:,:,:))
XVM(:,:,:)=ZRHODJV(:,:,:)/MYM(ZRHOD(:,:,:)*PJ(:,:,:))

CALL EXTRAPOL('W',XUM)
CALL EXTRAPOL('S',XVM)

!
!
!-------------------------------------------------------------------------------
!
!*       5.    INITIALIZATION OF LS HORIZONTAL MOMENTUM ON MESO-NH ARAKAWA A-GRID
!              ------------------------------------------------------------------
!
IF( HATMFILETYPE == 'MESONH' ) THEN
  !
  ZRHODU_MX=PLSU_MX*PRHOD_MX
  ZRHODV_MX=PLSV_MX*PRHOD_MX
  CALL VER_INT_DYN(OSHIFT,ZRHODU_MX,ZRHODV_MX,PZFLUX_MX,PZMASS_MX,PZS_LS,ZRHODUA,ZRHODVA)
  !
  ALLOCATE(XLSUM(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3)))
  ALLOCATE(XLSVM(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3)))
  !
  ZRHODJU(:,:,:)=MXM(ZRHODUA(:,:,:)*PJ(:,:,:))
  ZRHODJV(:,:,:)=MYM(ZRHODVA(:,:,:)*PJ(:,:,:))
  !
  XLSUM(:,:,:)=ZRHODJU(:,:,:)/MXM(ZRHOD(:,:,:)*PJ(:,:,:))
  XLSVM(:,:,:)=ZRHODJV(:,:,:)/MYM(ZRHOD(:,:,:)*PJ(:,:,:))
  !
END IF
!
!-------------------------------------------------------------------------------
!
!*       5.    COMPUTATION OF FIRST GUESS OF W
!              -------------------------------
!
ZZFLUX_SH(:,:,:)=VER_SHIFT(PZFLUX_MX,PZS_LS,XZS)
CALL COEF_VER_INTERP_LIN(ZZFLUX_SH(:,:,:),XZZ(:,:,:))
XWM(:,:,:)=VER_INTERP_LIN(PW_MX(:,:,:),NKLIN(:,:,:),XCOEFLIN(:,:,:))
!
IF ( HATMFILETYPE == 'MESONH' ) THEN
  ALLOCATE(XLSWM(SIZE(PJ,1),SIZE(PJ,2),SIZE(PJ,3)))
  XLSWM(:,:,:)=VER_INTERP_LIN(PLSW_MX(:,:,:),NKLIN(:,:,:),XCOEFLIN(:,:,:))
END IF
!
DEALLOCATE(NKLIN)
DEALLOCATE(XCOEFLIN)
!
!*       5.2   forcing to zero value at top (to be removed when solver allows other values)
!
ZCOEF(:,:,:)=(       XZZ(:,:,:)           -SPREAD(XZZ(:,:,IKB),3,IKU)) &
            /(SPREAD(XZZ(:,:,IKE+1),3,IKU)-SPREAD(XZZ(:,:,IKB),3,IKU))
XWM(:,:,:)=XWM(:,:,:)*MAX(MIN( (4.-4.*ZCOEF(:,:,:)) ,1.),0.)
!-------------------------------------------------------------------------------
!
!*       6.    STORAGE OF LARGE SCALE FIELDS
!              ------------------------------
!
IF ( HATMFILETYPE == 'GRIBEX' ) THEN
  ALLOCATE(XLSUM(SIZE(XUM,1),SIZE(XUM,2),SIZE(XUM,3)))
  ALLOCATE(XLSVM(SIZE(XVM,1),SIZE(XVM,2),SIZE(XVM,3)))
  ALLOCATE(XLSWM(SIZE(XWM,1),SIZE(XWM,2),SIZE(XWM,3)))
  XLSUM(:,:,:)=XUM(:,:,:)
  XLSVM(:,:,:)=XVM(:,:,:)
  XLSWM(:,:,:)=XWM(:,:,:)
END IF
! enforce zero gradient along the vertical under and above the vertical
! boundaries
XLSUM(:,:,IKB-1)=XLSUM(:,:,IKB)
XLSUM(:,:,IKE+1)=XLSUM(:,:,IKE)
XLSVM(:,:,IKB-1)=XLSVM(:,:,IKB)
XLSVM(:,:,IKE+1)=XLSVM(:,:,IKE)
XLSWM(:,:,IKB-1)=XLSWM(:,:,IKB)
XLSWM(:,:,IKE+1)=XLSWM(:,:,IKE)

CALL EXTRAPOL('W',XLSUM)
CALL EXTRAPOL('E',XLSUM)
CALL EXTRAPOL('S',XLSVM)
CALL EXTRAPOL('E',XLSVM)



! FROM PREP_IDEAL_CASE
!
! 3D case
!
  CALL GET_SIZEX_LB(CLUOUT0,NIMAX_ll,NJMAX_ll,NRIMX,   &
       IISIZEXF,IJSIZEXF,IISIZEXFU,IJSIZEXFU,         &
       IISIZEX4,IJSIZEX4,IISIZEX2,IJSIZEX2)
  CALL GET_SIZEY_LB(CLUOUT0,NIMAX_ll,NJMAX_ll,NRIMY,   &
       IISIZEYF,IJSIZEYF,IISIZEYFV,IJSIZEYFV,         &
       IISIZEY4,IJSIZEY4,IISIZEY2,IJSIZEY2)

  IF ( LHORELAX_UVWTH ) THEN
    NSIZELBX_ll=2*NRIMX+2
    NSIZELBXU_ll=2*NRIMX+2
    NSIZELBY_ll=2*NRIMY+2
    NSIZELBYV_ll=2*NRIMY+2
    ALLOCATE(XLBXUM(IISIZEXFU,IJSIZEXFU,IKU))
    ALLOCATE(XLBYUM(IISIZEYF,IJSIZEYF,IKU))
    ALLOCATE(XLBXVM(IISIZEXF,IJSIZEXF,IKU))
    ALLOCATE(XLBYVM(IISIZEYFV,IJSIZEYFV,IKU))
    ALLOCATE(XLBXWM(IISIZEXF,IJSIZEXF,IKU))
    ALLOCATE(XLBYWM(IISIZEYF,IJSIZEYF,IKU))
    !ALLOCATE(XLBXTHM(IISIZEXF,IJSIZEXF,IKU))
    !ALLOCATE(XLBYTHM(IISIZEYF,IJSIZEYF,IKU))
  ELSE
    NSIZELBX_ll=2
    NSIZELBXU_ll=4
    NSIZELBY_ll=2
    NSIZELBYV_ll=4
    ALLOCATE(XLBXUM(IISIZEX4,IJSIZEX4,IKU))
    ALLOCATE(XLBYUM(IISIZEY2,IJSIZEY2,IKU))
    ALLOCATE(XLBXVM(IISIZEX2,IJSIZEX2,IKU))
    ALLOCATE(XLBYVM(IISIZEY4,IJSIZEY4,IKU))
    ALLOCATE(XLBXWM(IISIZEX2,IJSIZEX2,IKU))
    ALLOCATE(XLBYWM(IISIZEY2,IJSIZEY2,IKU))
    !ALLOCATE(XLBXTHM(IISIZEX2,IJSIZEX2,IKU))
    !ALLOCATE(XLBYTHM(IISIZEY2,IJSIZEY2,IKU))
  END IF  

ILBX=SIZE(XLBXUM,1)
ILBY=SIZE(XLBYUM,2)
IF(LWEST_ll() .AND. .NOT. L1D) THEN
  XLBXUM(1:NRIMX+1,        :,:)     = XUM(2:NRIMX+2,        :,:)
  XLBXVM(1:NRIMX+1,        :,:)     = XVM(1:NRIMX+1,        :,:)
  XLBXWM(1:NRIMX+1,        :,:)     = XWM(1:NRIMX+1,        :,:)

ENDIF
IF(LEAST_ll() .AND. .NOT. L1D) THEN
  XLBXUM(ILBX-NRIMX:ILBX,:,:)     = XUM(IIU-NRIMX:IIU,    :,:)
  XLBXVM(ILBX-NRIMX:ILBX,:,:)     = XVM(IIU-NRIMX:IIU,    :,:)
  XLBXWM(ILBX-NRIMX:ILBX,:,:)     = XWM(IIU-NRIMX:IIU,    :,:)

ENDIF
IF(LSOUTH_ll() .AND. .NOT. L1D .AND. .NOT. L2D) THEN
  XLBYUM(:,1:NRIMY+1,        :)     = XUM(:,1:NRIMY+1,      :)
  XLBYVM(:,1:NRIMY+1,        :)     = XVM(:,2:NRIMY+2,      :)
  XLBYWM(:,1:NRIMY+1,        :)     = XWM(:,1:NRIMY+1,  :)

ENDIF
IF(LNORTH_ll().AND. .NOT. L1D .AND. .NOT. L2D) THEN
  XLBYUM(:,ILBY-NRIMY:ILBY,:)     = XUM(:,IJU-NRIMY:IJU,  :)
  XLBYVM(:,ILBY-NRIMY:ILBY,:)     = XVM(:,IJU-NRIMY:IJU,  :)
  XLBYWM(:,ILBY-NRIMY:ILBY,:)     = XWM(:,IJU-NRIMY:IJU,  :)

ENDIF

!!$IF ( LHORELAX_UVWTH ) THEN
!!$  ALLOCATE(XLBXUM(2*NRIMX+2,IJU,IKU))
!!$  ALLOCATE(XLBYUM(IIU,2*NRIMY+2,IKU))
!!$  ALLOCATE(XLBXVM(2*NRIMX+2,IJU,IKU))
!!$  ALLOCATE(XLBYVM(IIU,2*NRIMY+2,IKU))
!!$  ALLOCATE(XLBXWM(2*NRIMX+2,IJU,IKU))
!!$  ALLOCATE(XLBYWM(IIU,2*NRIMY+2,IKU))
!!$ELSE
!!$  ALLOCATE(XLBXUM(4,IJU,IKU))
!!$  ALLOCATE(XLBYUM(IIU,2,IKU))
!!$  ALLOCATE(XLBXVM(2,IJU,IKU))
!!$  ALLOCATE(XLBYVM(IIU,4,IKU))
!!$  ALLOCATE(XLBXWM(2,IJU,IKU))
!!$  ALLOCATE(XLBYWM(IIU,2,IKU))
!!$END IF


!
!!$NSIZELBX_ll=SIZE(XLBXWM,1)
!!$NSIZELBXU_ll=SIZE(XLBXUM,1)
!!$NSIZELBY_ll=SIZE(XLBYWM,2)
!!$NSIZELBYV_ll=SIZE(XLBYVM,2)
!!$!
!!$!
!!$ILBX=SIZE(XLBXUM,1)/2-1
!!$ILBY=SIZE(XLBYUM,2)/2-1
!!$!
!!$IF(LWEST_ll() .AND. .NOT. L1D) THEN
!!$   XLBXUM(1:ILBX+1,:,:)        = XUM(  IIB:IIB+ILBX  ,:,:)
!!$   XLBXVM(1:ILBX+1,:,:)        = XVM(IIB-1:IIB-1+ILBX,:,:)
!!$   XLBXWM(1:ILBX+1,:,:)        = XWM(IIB-1:IIB-1+ILBX,:,:)
!!$ENDIF
!!$!
!!$!
!!$!
!!$ILBX=SIZE(XLBXVM,1)/2-1
!!$ILBY=SIZE(XLBYVM,2)/2-1
!!$!
!!$IF(LEAST_ll() .AND. .NOT. L1D) THEN
!!$   XLBXUM(ILBX+2:2*ILBX+2,:,:) = XUM(IIE+1-ILBX:IIE+1,:,:)
!!$   XLBXVM(ILBX+2:2*ILBX+2,:,:) = XVM(IIE+1-ILBX:IIE+1,:,:)
!!$   XLBXWM(ILBX+2:2*ILBX+2,:,:) = XWM(IIE+1-ILBX:IIE+1,:,:)
!!$END IF
!!$!
!!$!
!!$ILBX=SIZE(XLBXWM,1)/2-1
!!$ILBY=SIZE(XLBYWM,2)/2-1
!!$!
!!$IF(LSOUTH_ll() .AND. .NOT. L1D .AND. .NOT. L2D) THEN
!!$   XLBYUM(:,1:ILBY+1,:)        = XUM(:,IJB-1:IJB-1+ILBY,:)
!!$   XLBYVM(:,1:ILBY+1,:)        = XVM(:,IJB  :IJB+ILBY  ,:)
!!$   XLBYWM(:,1:ILBY+1,:)        = XWM(:,IJB-1:IJB-1+ILBY,:)
!!$ENDIF
!!$
!!$IF(LNORTH_ll().AND. .NOT. L1D .AND. .NOT. L2D) THEN
!!$   XLBYUM(:,ILBY+2:2*ILBY+2,:) = XUM(:,IJE+1-ILBY:IJE+1,:)
!!$   XLBYVM(:,ILBY+2:2*ILBY+2,:) = XVM(:,IJE+1-ILBY:IJE+1,:)
!!$   XLBYWM(:,ILBY+2:2*ILBY+2,:) = XWM(:,IJE+1-ILBY:IJE+1,:)
!!$END IF

CALL  MPPDB_CHECKLB(XLBXUM,"ver_dyn::XLBXUM::",PRECISION,'LBXU',NRIMX)
CALL  MPPDB_CHECKLB(XLBXVM,"ver_dyn::XLBXVM::",PRECISION,'LBXU',NRIMX)
CALL  MPPDB_CHECKLB(XLBXWM,"ver_dyn::XLBXWM::",PRECISION,'LBXU',NRIMX)


CALL  MPPDB_CHECKLB(XLBYUM,"ver_dyn::XLBYUM::",PRECISION,'LBYV',NRIMY)
CALL  MPPDB_CHECKLB(XLBYVM,"ver_dyn::XLBYVM::",PRECISION,'LBYV',NRIMY)
CALL  MPPDB_CHECKLB(XLBYWM,"ver_dyn::XLBYWM::",PRECISION,'LBYV',NRIMY)
!
!-------------------------------------------------------------------------------
!
WRITE(ILUOUT0,*) 'Routine VER_DYN completed.'
!
END SUBROUTINE VER_DYN
