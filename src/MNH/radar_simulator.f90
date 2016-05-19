!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$ $Date$
!-----------------------------------------------------------------
!      ###########################
       MODULE MODI_RADAR_SIMULATOR 
!      ###########################
!
INTERFACE
    SUBROUTINE RADAR_SIMULATOR(PUM,PVM,PWM,PRT,PCIT,PRHODREF,PTEMP,PPABSM,PREFL_CART,PLATLON)
!
REAL,DIMENSION(:,:,:),  INTENT(IN), TARGET :: PUM,PVM,PWM ! wind components
REAL,DIMENSION(:,:,:,:),INTENT(IN), TARGET  :: PRT  ! microphysical  mix. ratios at t
REAL,DIMENSION(:,:,:),  INTENT(IN), TARGET  :: PCIT ! pristine ice concentration at t
REAL,DIMENSION(:,:,:),  INTENT(IN), TARGET  :: PRHODREF ! density of the ref. state
REAL,DIMENSION(:,:,:),  INTENT(IN), TARGET  :: PTEMP    ! air temperature
REAL,DIMENSION(:,:,:),  INTENT(IN)  :: PPABSM   ! Absolute pressure
!
REAL,DIMENSION(:,:,:,:,:),INTENT(OUT) :: PREFL_CART ! radar reflectivity in dBZ on observation cartesian grid
REAL,DIMENSION(:,:,:),  INTENT(OUT) :: PLATLON! latlon of cartesian grid points
!
END SUBROUTINE RADAR_SIMULATOR
!
END INTERFACE
!
END MODULE MODI_RADAR_SIMULATOR
!
!     #########################################################################
      SUBROUTINE RADAR_SIMULATOR(PUM,PVM,PWM,PRT,PCIT,PRHODREF,PTEMP,PPABSM, &
           PREFL_CART,PLATLON)
!     #########################################################################
!
!!****  *RADAR_SIMULATOR * - computes some pertinent radar parameters on PPIs
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to compute the equivalent reflectivity
!!     of a mixed phase cloud on a PPI and then project it on a cartesian grid.
!!
!!**  METHOD
!!    ------
!!      First the geometry of radar data (radar, elevation, azimuth, range bin)
!!    is defined. Then ray paths are determined. Necessary model variables are 
!!    interpolated on each bin.
!!    A call to RADAR_SCATTERING yields reflectivities for each bin. In the end,
!!    reflectivities are interpolated on a cartesian grid.
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST
!!      Module MODD_REF
!!      Module MODD_RAIN_ICE_DESCR
!!      Module MODD_RAIN_ICE_PARAM
!!      Module MODD_PARAMETERS
!!      Module MODD_LUNIT
!
!!      Module MODE_IO_ll
!!      Module MODE_FM
!
!!      Module MODD_GR_FIELD_n
!!      Module MODD_GRID_n
!!      Module MODD_CONF_n
!!      Module MODD_GRID 
!!      Module MODE_GRIDPROJ
!!      Module MODD_RADAR 
!!      Module MODE_INTERPOL_BEAM
!!      Module MODE_FGAU  
!!      Module MODI_RADAR_SCATTERING
!!      Module MODI_SET_MSK 
!!      Module MODD_BUDGET
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation ( routine RADAR_SIMULATOR )
!!
!!    AUTHOR
!!    ------
!!      O. Caumont & V. Ducrocq      * Météo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    26/03/2004
!!      O. Caumont  14/09/2009 modifications to allow for polar outputs
!!      O. Caumont 11/02/2010 thresholding and conversion from linear to log values after interpolation instead of before.
!!      O. Caumont 01/2011 gate-to-gate path computations revised (formulation+efficiency); comments in outputs revised
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CST              , ONLY: XPI,XRD,XRV,XRADIUS
USE MODD_REF
USE MODD_RAIN_ICE_DESCR
USE MODD_RAIN_ICE_PARAM
USE MODD_PARAMETERS
USE MODD_LUNIT
!
USE MODE_IO_ll
USE MODE_FM
!
USE MODD_GR_FIELD_n
USE MODD_GRID_n
USE MODD_CONF_n
USE MODD_GRID             , ONLY: XLATORI,XLONORI,XRPK,XLAT0,XLON0,XBETA
USE MODE_GRIDPROJ
USE MODD_RADAR            , ONLY: XLAT_RAD,XLON_RAD,XALT_RAD,XDT_RAD,XELEV,&
     XX_INI,XY_INI,XZ_INI,XSTEP_RAD,NBRAD,NBELEV,NBAZIM,NBSTEPMAX,&
     NCURV_INTERPOL,LATT,LCART_RAD,NPTS_H,NPTS_V,LQUAD,XGRID,XVALGROUND,&
     NMAX,LREFR,LDNDZ,XREFLMIN
!
USE MODE_INTERPOL_BEAM
USE MODE_FGAU             , ONLY: GAULEG,GAUHER
USE MODI_RADAR_SCATTERING
! convective/stratiform
USE MODI_SET_MSK 
! /convective/stratiform
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
REAL,DIMENSION(:,:,:),  INTENT(IN), TARGET :: PUM,PVM,PWM ! wind components
REAL,DIMENSION(:,:,:,:),INTENT(IN), TARGET :: PRT  ! microphysical  mix. ratios at t
REAL,DIMENSION(:,:,:),  INTENT(IN), TARGET :: PCIT ! pristine ice concentration at t
REAL,DIMENSION(:,:,:),  INTENT(IN), TARGET :: PRHODREF ! density of the ref. state
REAL,DIMENSION(:,:,:),  INTENT(IN), TARGET :: PTEMP    ! air temperature
REAL,DIMENSION(:,:,:),  INTENT(IN)  :: PPABSM     ! Absolute pressure
!
REAL,DIMENSION(:,:,:,:,:),INTENT(OUT) :: PREFL_CART ! radar reflectivity in dBZ and other parameters on observation cartesian or polar grid
REAL,DIMENSION(:,:,:),    INTENT(OUT) :: PLATLON! latlon of cartesian grid points

!
!*       0.2   Declarations of local variables :
!
!
TYPE(PAMOD),DIMENSION(:),ALLOCATABLE :: TVARMOD ! array of pointers to grid-point model fields
TYPE(PARAD),DIMENSION(:),ALLOCATABLE :: TVARRAD ! array of pointers to model fields interpolated onto radar PPIs
REAL :: ZRDSRV      ! XRD/XRV
REAL :: ZRDSDG      ! PI/180
INTEGER :: IIB,IIE          ! Loop limits for coordinate X
INTEGER :: IJB,IJE          ! Loop limits for coordinate Y
INTEGER :: IKB,IKE          ! Loop limits for coordinate Z
INTEGER :: IIU,IJU,IKU      ! Loop variables of model 
INTEGER :: IIELV ! maximum number of elevations 
INTEGER :: ILUOUT0 ! Logical unit number for output-listing
INTEGER :: IRESP   ! Return code of FM-routines
INTEGER :: JI,JL,JEL,JAZ,JH,JV ! Loop variables of control
INTEGER :: IEL
INTEGER,DIMENSION(:,:,:,:),ALLOCATABLE :: IREFL_CART_NB!
INTEGER,DIMENSION(:,:,:,:),ALLOCATABLE :: IVDOP_CART_NB!
REAL,DIMENSION(:,:,:,:,:),ALLOCATABLE  :: ZZE ! gate equivalent reflectivity factor, ZDR, KDP, 
REAL,DIMENSION(:,:,:,:),ALLOCATABLE    ::  ZELEV        ! elevation in rad.
REAL,DIMENSION(:),  ALLOCATABLE    ::  ZAZIM_BASE   ! azimuth in rad. of the beam centre
REAL,DIMENSION(:,:,:),  ALLOCATABLE    ::  ZAZIM        ! azimuth in rad. of discretized beam
!
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE :: ZX_RAY   ! x positions of the points along the ray-tracing
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE :: ZY_RAY   ! y positions of the points along the ray-tracing
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE :: ZZ_RAY   ! z positions of the points along the ray_tracing
REAL, DIMENSION(:,:),        ALLOCATABLE :: ZLAT     ! latitude of the points 
REAL, DIMENSION(:,:),        ALLOCATABLE :: ZLON     ! longitude of the points 
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE :: ZDX_NAT ! x increment on the natural referential
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE :: ZDY_NAT ! y increment on the natural referential
REAL                                     :: ZRPK,ZLAT0,ZBETA,ZLON0 ! projection characteristics
REAL                                     :: ZCLAT0,ZSLAT0          ! cos and sin of lat0
REAL                                     :: ZMAP     ! Map factor
REAL                                     :: ZGAMMA,ZCOSG,ZSING  ! angle of projection and its cos and sin values
!
REAL, DIMENSION(:),          ALLOCATABLE :: ZXHATM ! X values of the mass points
REAL, DIMENSION(:),          ALLOCATABLE :: ZYHATM ! Y values of the mass points
REAL, DIMENSION(:,:,:),      ALLOCATABLE :: ZZM    ! Z values of the mass points
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE, TARGET :: ZT_RAY  ! temperature interpolated along the rays
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE, TARGET :: ZR_RAY  ! rain            mixing ratio interpolated along the rays
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE, TARGET :: ZI_RAY  ! pristine ice    mixing ratio interpolated along the rays
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE, TARGET :: ZS_RAY  ! snow/aggregates mixing ratio interpolated along the rays
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE, TARGET :: ZG_RAY  ! graupel         mixing ratio interpolated along the rays
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE, TARGET :: ZCIT_RAY  ! pristine ice concentration interpolated along the rays
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE, TARGET :: ZRHODREF_RAY
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE :: ZVDOP_RAY  
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE, TARGET :: ZUM_RAY,ZVM_RAY,ZWM_RAY 
!
REAL :: ZKE=4./3. ! COEFF OF EFFECTIVE RADIUS IN AIR REFRACTIVE INDEX COMPUTATION
REAL :: ZN0,ZN1 ! REFRACTIVE INDEX OF AIR
REAL :: ZDNDZ1 ! vertical gradient of refractive index of air
!

REAL,DIMENSION(:),ALLOCATABLE :: ZX_H,ZW_H ! Gauss-Hermite horizontal points & weights
REAL,DIMENSION(:),ALLOCATABLE :: ZX_V,ZW_V ! Gauss-Hermite vertical points & weights
 
INTEGER :: IXGRID,IYGRID
INTEGER :: INVAR
REAL :: r,h,alph

! convective/stratiform
LOGICAL,DIMENSION(:,:,:),ALLOCATABLE               :: GBU_MSK
REAL, DIMENSION(:,:,:),ALLOCATABLE, TARGET       :: ZBU_MASK
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE, TARGET :: ZBU_MASK_RAY
! refractivity
REAL, DIMENSION(:,:,:,:,:,:),ALLOCATABLE :: ZN_RAY,ZDNDZ_RAY ! refractivity and its vertical gradient in radar coordinates
REAL, DIMENSION(:,:,:),ALLOCATABLE       :: ZN,ZDNDZ
INTEGER :: IRFR,IDNZ                                    ! index of ZN_RAY,ZDNDZ_RAY in ZZE
INTEGER :: IVDOP,IHAS
INTEGER,PARAMETER :: IZER=5,IZEG=8,IATR=14,IATG=17
!
!-------------------------------------------------------------------------------
!
!
!*       1.     INITIALIZATION 
!   	        --------------
!
!
!*       1.1 IO and dimensions initialization
!   
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
!
IIU=SIZE(PTEMP,1)
IJU=SIZE(PTEMP,2)
IKU=SIZE(PTEMP,3)
IIB = JPHEXT + 1
IJB = JPHEXT + 1
IKB = JPVEXT + 1
IIE = IIU - JPHEXT
IJE = IJU - JPHEXT
IKE = IKU - JPVEXT
! convective/stratiform
ALLOCATE(GBU_MSK(IIU,IJU,4))
CALL SET_MSK(PRT,PRHODREF,GBU_MSK) ! on récupère GBU_MSK
ALLOCATE(ZBU_MASK(SIZE(PTEMP,1),SIZE(PTEMP,2),SIZE(PTEMP,3)))
ZBU_MASK(:,:,1)=0.
WHERE(GBU_MSK(:,:,2)) ! stratiform
   ZBU_MASK(:,:,1)=1.
END WHERE
WHERE(GBU_MSK(:,:,1)) ! convective
   ZBU_MASK(:,:,1)=2.
END WHERE
DEALLOCATE(GBU_MSK)
ZBU_MASK(:,:,:)=SPREAD(ZBU_MASK(:,:,1),DIM=3,NCOPIES=SIZE(PTEMP,3))
IIELV=MAXVAL(NBELEV(1:NBRAD))
!
!*       1.2 Some constants and parameters
!   
ZRDSRV=XRD/XRV       ! XRD/XRV
ZRDSDG=XPI/180.       ! PI/180
!
!*       1.3 beam characteristics initialization
!  
! azimuths 0°=N 90°=E
ALLOCATE(ZAZIM_BASE(NBAZIM),ZAZIM(NBRAD,NBAZIM,NPTS_H))
!
DO JAZ=1,NBAZIM
   IF(JAZ<=NMAX) THEN
      ZAZIM_BASE(JAZ)=ATAN((JAZ-.5)/NMAX)
   ELSE IF(JAZ<=3*NMAX) THEN
      ZAZIM_BASE(JAZ)=XPI/2+ATAN((-2*NMAX+JAZ-.5)/NMAX)
   ELSE IF(JAZ<=5*NMAX) THEN
      ZAZIM_BASE(JAZ)=XPI-ATAN((4*NMAX-JAZ+.5)/NMAX)
   ELSE IF(JAZ<=7*NMAX) THEN
      ZAZIM_BASE(JAZ)=3*XPI/2-ATAN((6*NMAX-JAZ+.5)/NMAX)
   ELSE 
      ZAZIM_BASE(JAZ)=2*XPI-ATAN((8*NMAX-JAZ+.5)/NMAX)
   END IF
END DO
ZAZIM(:,1:NBAZIM,:)=SPREAD(SPREAD(ZAZIM_BASE(1:NBAZIM),DIM=1,NCOPIES=NBRAD),DIM=3,NCOPIES=NPTS_H)
!  
! elevations 
ALLOCATE(ZELEV(NBRAD,IIELV,NBSTEPMAX+1,NPTS_V))
!
ZELEV(:,:,:,:)=SPREAD(SPREAD(XELEV(1:NBRAD,1:IIELV),DIM=3,NCOPIES=NBSTEPMAX+1),&
     DIM=4,NCOPIES=NPTS_V)
! 
! Discretization of the gate 
!
ALLOCATE(ZX_H((NPTS_H+1)/2),ZW_H((NPTS_H+1)/2))
ALLOCATE(ZX_V((NPTS_V+1)/2),ZW_V((NPTS_V+1)/2))
IF(LQUAD) THEN
   CALL GAULEG(NPTS_H,ZX_H,ZW_H)
   CALL GAULEG(NPTS_V,ZX_V,ZW_V)
   XDT_RAD(:)=XDT_RAD(:)/2.
ELSE
   CALL GAUHER(NPTS_H,ZX_H,ZW_H)
   CALL GAUHER(NPTS_V,ZX_V,ZW_V)
   XDT_RAD(:)=XDT_RAD(:)/SQRT(8.*LOG(2.)) ! variable change
END IF
!
DO JI=1,NBRAD
  IEL=NBELEV(JI)
  DO JV=1,NPTS_V
   ! to change if NPTS_V even
    ZELEV(1:NBRAD,1:IEL,:,JV)=ZELEV(1:NBRAD,1:IEL,:,JV)&
         +SIGN(ZX_V(ABS((2*JV-NPTS_V-1)/2)+1),2.*JV-NPTS_V-1.)* &
         SPREAD(SPREAD(XDT_RAD(1:NBRAD),DIM=2,NCOPIES=IIELV),DIM=3,NCOPIES=NBSTEPMAX+1)
  END DO
END DO

DO JH=1,NPTS_H
   ! to change if NPTS_H even
  ZAZIM(1:NBRAD,:,JH)=ZAZIM(1:NBRAD,:,JH)+SIGN(ZX_H(ABS((2*JH-NPTS_H-1)/2)+1),2.*JH-NPTS_H-1.)* &
       SPREAD(XDT_RAD(1:NBRAD),DIM=2,NCOPIES=NBAZIM)*ZRDSDG
END DO
ZELEV(:,:,:,:)=ZELEV(:,:,:,:)*ZRDSDG ! in radian
! initialisation of refractivity indices
IRFR=1    ! this is used down there in the interpolation part
IDNZ=1 ! this is used down there in the interpolation part
IHAS=10
IVDOP=9
IF(LREFR) IRFR=16 ! refractivity
IF(LDNDZ) THEN
   IF(LREFR) THEN
      IDNZ=17 ! refractivity vertical gradient
   ELSE
      IDNZ=16 ! refractivity vertical gradient
   END IF
END IF
IF(LATT) THEN
   IRFR=IRFR+8
   IDNZ=IDNZ+8
   IHAS=IHAS+8
END IF
!
!----------------------------------------------------------------------------------------
!*       2.    RAY TRACING DEFINITION
!              ----------------------
!
!*       2.1 some initializations for MESO-NH conformal projection
!
IF (XRPK<0.) THEN     ! projection from north pole 
  ZRPK=-XRPK
  ZLAT0=-XLAT0
  ZBETA=-XBETA
  ZLON0=XLON0+180.
  WRITE(0,*) 'projection from north pole'
  WRITE(0,*) 'ZRPK',ZRPK
  WRITE(0,*) 'ZLAT0',ZLAT0
  WRITE(0,*) 'ZBETA',ZBETA
  WRITE(0,*) 'ZLON0',ZLON0
ELSE                  ! projection from south pole
  ZRPK=XRPK
  ZLAT0=XLAT0
  ZBETA=XBETA
  ZLON0=XLON0
  WRITE(0,*) 'projection from south pole'
  WRITE(0,*) 'ZRPK:',ZRPK
  WRITE(0,*) 'ZLAT0:',ZLAT0
  WRITE(0,*) 'ZBETA:',ZBETA
  WRITE(0,*) 'ZLON0:',ZLON0
ENDIF
ZCLAT0 = COS(ZRDSDG*ZLAT0)
ZSLAT0 = SIN(ZRDSDG*ZLAT0)
!
!  Positions of the mass points in the MESO-NH conformal projection 
!
ALLOCATE(ZXHATM(IIU))
ALLOCATE(ZYHATM(IJU)) 
ALLOCATE(ZZM(IIU,IJU,IKU))
!
ZXHATM(1:IIU-1) = .5*(XXHAT(1:IIU-1)+XXHAT(2:IIU))
ZXHATM(IIU)     = 2.*XXHAT(IIU)-ZXHATM(IIU-1)
!
ZYHATM(1:IJU-1) = .5*(XYHAT(1:IJU-1)+XYHAT(2:IJU))
ZYHATM(IJU)     = 2.*XYHAT(IJU)-ZYHATM(IJU-1)
!
ZZM(:,:,1:IKU-1)= .5*(XZZ(:,:,1:IKU-1)+XZZ(:,:,2:IKU))
ZZM(:,:,IKU)= 2. * XZZ(:,:,IKU) - ZZM(:,:,IKU-1)
!
!*       2.2 initialization of the ray beginning
!
!
! position of the radar on the MESO-NH conformal projection grid
!
DO JI=1,NBRAD
   CALL SM_XYHAT(XLATORI,XLONORI,   & 
       XLAT_RAD(JI),XLON_RAD(JI),XX_INI(JI),XY_INI(JI))  
END DO
XZ_INI(:)=XALT_RAD(:)       ! z positions of the ground source signal 
!
ALLOCATE(ZX_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V),&
         ZY_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V),&
         ZZ_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V),&
         ZLAT(NPTS_H,NPTS_V),&
         ZLON(NPTS_H,NPTS_V),&
         ZDX_NAT(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V),&
         ZDY_NAT(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V))
!
ZX_RAY(:,:,:,:,:,:)=0.
ZY_RAY(:,:,:,:,:,:)=0.
ZZ_RAY(:,:,:,:,:,:)=0.
ZDX_NAT(:,:,:,:,:,:)=0.
ZDY_NAT(:,:,:,:,:,:)=0.
!
ZX_RAY(1:NBRAD,:,:,1,:,:)=SPREAD(SPREAD(SPREAD(SPREAD(XX_INI(1:NBRAD),DIM=2,NCOPIES=IIELV),&
     DIM=3,NCOPIES=NBAZIM),DIM=4,NCOPIES=NPTS_H),DIM=5,NCOPIES=NPTS_V)
ZY_RAY(1:NBRAD,:,:,1,:,:)=SPREAD(SPREAD(SPREAD(SPREAD(XY_INI(1:NBRAD),DIM=2,NCOPIES=IIELV), &
     DIM=3,NCOPIES=NBAZIM),DIM=4,NCOPIES=NPTS_H),DIM=5,NCOPIES=NPTS_V)
ZZ_RAY(1:NBRAD,:,:,1,:,:)=SPREAD(SPREAD(SPREAD(SPREAD(XZ_INI(1:NBRAD),DIM=2,NCOPIES=IIELV), &
     DIM=3,NCOPIES=NBAZIM),DIM=4,NCOPIES=NPTS_H),DIM=5,NCOPIES=NPTS_V)
! refractivity
IF(LREFR) ALLOCATE(ZN_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V))
IF(LDNDZ) ALLOCATE(ZDNDZ_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V))
IF(NCURV_INTERPOL == 1) THEN
  ALLOCATE(ZN(IIU,IJU,IKU),ZDNDZ(IIU,IJU,IKU))
  ZN(:,:,:)=1.+.776E-6*PPABSM(:,:,:)*( 1. + 4810.*PRT(:,:,:,1)/(ZRDSRV+PRT(:,:,:,1))/PTEMP(:,:,:) )/PTEMP(:,:,:)
  ! vertical gradient of n approximated using a centred finite difference scheme
  ZDNDZ(:,:,IKB+1:IKE-1)=((ZZM(:,:,IKB+1:IKE-1)-ZZM(:,:,IKB:IKE-2))*2*(ZN(:,:,IKB+2:IKE)-ZN(:,:,IKB+1:IKE-1)) &
    +(ZZM(:,:,IKB+2:IKE)-ZZM(:,:,IKB+1:IKE-1))**2*(ZN(:,:,IKB+1:IKE-1)-ZN(:,:,IKB:IKE-2))) &
    /(ZZM(:,:,IKB+1:IKE-1)-ZZM(:,:,IKB:IKE-2))/(ZZM(:,:,IKB+2:IKE)-ZZM(:,:,IKB+1:IKE-1))/(ZZM(:,:,IKB+2:IKE)-ZZM(:,:,IKB:IKE-2))
! + valeurs limites
  ZDNDZ(:,:,IKB)=(ZN(:,:,IKB+1)-ZN(:,:,IKB))/(ZZM(:,:,IKB+1)-ZZM(:,:,IKB))
  ZDNDZ(:,:,IKE)=(ZN(:,:,IKE)-ZN(:,:,IKE-1))/(ZZM(:,:,IKE)-ZZM(:,:,IKE-1))
ENDIF

!
!*       2.3  positions of the rays in the MESO-NH conformal projection
!
DO JI=1,NBRAD
   IEL=NBELEV(JI)
   WRITE(0,*) 'RADAR #',JI,'Number of ELEVATIONS: ',NBELEV(JI)
   WRITE(0,*) '  Elevations used:'
   DO JEL=1,IEL
      WRITE(0,*) "    ",ZELEV(JI,JEL,1,:)/ZRDSDG
      DO JAZ=1,NBAZIM
         label: DO JL=1,NBSTEPMAX
            ! SM_LATLON takes bidimensional arrays as arguments
            CALL SM_LATLON(XLATORI,XLONORI,   &   
                 ZX_RAY(JI,JEL,JAZ,JL,:,:), ZY_RAY(JI,JEL,JAZ,JL,:,:),ZLAT(:,:),ZLON(:,:))
            DO JH=1,NPTS_H
               DO JV=1,NPTS_V
                  !   
                  !*        Compute positions of the gates
                  !      
                  ! Compute local Map factor and other projection factors
                  IF(XRPK<0.)  ZLAT(JH,JV)=-ZLAT(JH,JV)     ! projection from north pole 
                  
                  IF(ABS(ZRPK-1.)>1.E-10 .AND. ABS(COS(ZRDSDG*ZLAT(JH,JV)))<1.E-10) THEN
                     WRITE(0,*) 'Error in projection : '
                     WRITE(0,*) 'pole in the domain, but not with stereopolar projection'
                     !callabortstop
                     CALL CLOSE_ll(CLUOUT0,IOSTAT=IRESP)
                     CALL ABORT
                     STOP
                  ENDIF
                  !
                  IF(ABS(ZCLAT0)<1.E-10 .AND. ABS(ZRPK-1.)<1.E-10) THEN
                     ZMAP = (1.+ZSLAT0)/(1.+SIN(ZRDSDG*ZLAT(JH,JV)))
                  ELSE IF(ABS(COS(ZRDSDG*ZLAT(JH,JV)))>1.E-10) THEN
                     ZMAP = ((ZCLAT0/COS(ZRDSDG*ZLAT(JH,JV)))**(1.-ZRPK))      &
                          * ((1.+ZSLAT0)/(1.+SIN(ZRDSDG*ZLAT(JH,JV))))**ZRPK
                  ELSE
                     ZMAP = (1.+ZSLAT0)/(1.+SIN(ZRDSDG*ZLAT(JH,JV)))
                  END IF
                  ZGAMMA=(ZRPK*(ZLON(JH,JV)-ZLON0)-ZBETA)*ZRDSDG
                  ZCOSG=COS(ZGAMMA)
                  ZSING=SIN(ZGAMMA)
                  
                  ! compute positions of radar gates (2 methods) : 
                  !  First method : gate-to-gate computation using the model's index of refraction
                  IF(NCURV_INTERPOL == 1) THEN 
                     ! first compute vertical position (height)          
                     ! compute the index of refraction at the radar gate boundaries
                     CALL INTERPOL_BEAM(ZN(:,:,:),ZN1,ZX_RAY(JI,JEL,JAZ,JL,JH,JV),&
                          ZY_RAY(JI,JEL,JAZ,JL,JH,JV),ZZ_RAY(JI,JEL,JAZ,JL,JH,JV),ZXHATM(:),ZYHATM(:),ZZM(:,:,:))
                     IF(LREFR) ZN_RAY(JI,JEL,JAZ,JL,JH,JV)=(ZN1-1.)*1.E6
                     IF(LDNDZ) THEN
                        IF(JL==1) THEN
                           ZDNDZ_RAY(JI,JEL,JAZ,JL,JH,JV)=0. ! this is not true, this is set to XVALGROUND afterwards
                        ELSE
                           ZDNDZ_RAY(JI,JEL,JAZ,JL,JH,JV)=(ZN1-ZN0)*1.E6/(ZZ_RAY(JI,JEL,JAZ,JL,1,1)-ZZ_RAY(JI,JEL,JAZ,JL-1,1,1))
                        END IF
                     END IF
                     IF(ZN1==-XUNDEF) THEN ! we are underground
                        ZZ_RAY(JI,JEL,JAZ,JL:NBSTEPMAX+1,:,:)=-XUNDEF ! rest of the ray is flagged undefined
                        EXIT label
                     ELSE
                        IF(JL > 1) THEN 
! next line to comment (std refraction)
!                           ZN1=ZN0-(ZZ_RAY(JI,JEL,JAZ,JL,1,1)-ZZ_RAY(JI,JEL,JAZ,JL-1,1,1))/(4.*XRADIUS)
                           IF(ZN0/ZN1*(XRADIUS+ZZ_RAY(JI,JEL,JAZ,JL-1,JH,JV))/(XRADIUS+ZZ_RAY(JI,JEL,JAZ,JL,JH,JV)) &
                                *COS(ZELEV(JI,JEL,JL-1,JV)) >= 1.) THEN ! it means the slope of the ray path is 0 relative to the Earth
                              ZELEV(JI,JEL,JL,JV)=-ACOS(2.-ZN0/ZN1*(XRADIUS+ZZ_RAY(JI,JEL,JAZ,JL-1,JH,JV)) &
                                   /(XRADIUS+ZZ_RAY(JI,JEL,JAZ,JL,JH,JV))*COS(ZELEV(JI,JEL,JL-1,JV)))
                           ELSE ! usual formula
                              ZELEV(JI,JEL,JL,JV)=ZELEV(JI,JEL,JL-1,JV)/ABS(ZELEV(JI,JEL,JL-1,JV))* &
                                   ACOS(ZN0/ZN1*(XRADIUS+ZZ_RAY(JI,JEL,JAZ,JL-1,JH,JV))/            &
                                   (XRADIUS+ZZ_RAY(JI,JEL,JAZ,JL,JH,JV))*COS(ZELEV(JI,JEL,JL-1,JV)))
                           END IF
                           ZDNDZ1=(ZN1-ZN0)/(ZZ_RAY(JI,JEL,JAZ,JL,1,1)-ZZ_RAY(JI,JEL,JAZ,JL-1,1,1))
                        ELSE ! for first gate DNDZ1 is the local value at radar
                           CALL INTERPOL_BEAM(ZDNDZ(:,:,:),ZDNDZ1,ZX_RAY(JI,JEL,JAZ,JL,JH,JV),&
                              ZY_RAY(JI,JEL,JAZ,JL,JH,JV),ZZ_RAY(JI,JEL,JAZ,JL,JH,JV),ZXHATM(:),ZYHATM(:),ZZM(:,:,:))
                        END IF
                        IF(ZDNDZ1>-ZN1/XRADIUS/COS(ZELEV(JI,JEL,JL,JV))) THEN
                          ZKE=1./(1.+XRADIUS/ZN1*ZDNDZ1*COS(ZELEV(JI,JEL,JL,JV)))
                        ELSE
                          ZKE=1./(1.-XRADIUS/ZN1*ZDNDZ1*COS(ZELEV(JI,JEL,JL,JV)))
                        END IF
                        ! éléments finis
!                        ZZ_RAY(JI,JEL,JAZ,JL+1,JH,JV)=SQRT(XSTEP_RAD**2+(XRADIUS+ZZ_RAY(JI,JEL,JAZ,JL,JH,JV))**2 &
!                             +2.*XSTEP_RAD*(XRADIUS+ZZ_RAY(JI,JEL,JAZ,JL,JH,JV))*SIN(ZELEV(JI,JEL,JL,JV)))-XRADIUS
                        ! Doviak & Zrnic
                        ZZ_RAY(JI,JEL,JAZ,JL+1,JH,JV)=ZZ_RAY(JI,JEL,JAZ,JL,JH,JV)+SQRT(XSTEP_RAD**2+(ZKE*XRADIUS)**2 &
                             +2.*XSTEP_RAD*ZKE*XRADIUS*SIN(ZELEV(JI,JEL,JL,JV)))-ZKE*XRADIUS
                        ZN0=ZN1
                        ! then compute horizontal position
                        ZDX_NAT(JI,JEL,JAZ,JL,JH,JV)=XRADIUS*ASIN(XSTEP_RAD/ &
                             (XRADIUS+ZZ_RAY(JI,JEL,JAZ,JL+1,JH,JV))*COS(ZELEV(JI,JEL,JL,JV)))*SIN(ZAZIM(JI,JAZ,JH))
                        ZDY_NAT(JI,JEL,JAZ,JL,JH,JV)=XRADIUS*ASIN(XSTEP_RAD/ &
                             (XRADIUS+ZZ_RAY(JI,JEL,JAZ,JL+1,JH,JV))*COS(ZELEV(JI,JEL,JL,JV)))*COS(ZAZIM(JI,JAZ,JH))
                        ZX_RAY(JI,JEL,JAZ,JL+1,JH,JV)=ZX_RAY(JI,JEL,JAZ,JL,JH,JV) & !!!
                             +  (ZMAP* XRADIUS *((ZDX_NAT(JI,JEL,JAZ,JL,JH,JV) * ZCOSG) - &
                             (ZDY_NAT(JI,JEL,JAZ,JL,JH,JV)* ZSING)  ) &
                             /(ZZ_RAY(JI,JEL,JAZ,JL,JH,JV) + XRADIUS))
                        ZY_RAY(JI,JEL,JAZ,JL+1,JH,JV)=ZY_RAY(JI,JEL,JAZ,JL,JH,JV) + &
                             (ZMAP* XRADIUS *((ZDX_NAT(JI,JEL,JAZ,JL,JH,JV) * ZSING) +  &
                             (ZDY_NAT(JI,JEL,JAZ,JL,JH,JV)* ZCOSG)  ) &
                             /(ZZ_RAY(JI,JEL,JAZ,JL,JH,JV) + XRADIUS))
                        !                  WRITE(0,*) 'ZY_RAY(',JI,JEL,JAZ,JL+1,')',ZY_RAY(JI,JEL,JAZ,JL+1)
                     END IF
                  ELSE 
                     ! effective Earth radius model Doviak & Zrnic 1993 (2.28b) p. 21
                     ! vertical position
                     ZZ_RAY(JI,JEL,JAZ,JL+1,JH,JV)=SQRT((JL*XSTEP_RAD)**2+(ZKE*XRADIUS)**2+ &
                          2.*JL*XSTEP_RAD*ZKE*XRADIUS*SIN(ZELEV(JI,JEL,1,JV)))-ZKE*XRADIUS+ &
                          ZZ_RAY(JI,JEL,JAZ,1,JH,JV)
                     ! This formula is given by Doviak & Zrnic 1993 (9.9 p. 307) 
                     ZELEV(JI,JEL,JL+1,JV)=ZELEV(JI,JEL,1,JV)+ATAN(JL*XSTEP_RAD*COS(ZELEV(JI,JEL,1,JV))&
                          /(ZKE*XRADIUS+JL*XSTEP_RAD*SIN(ZELEV(JI,JEL,1,JV))))
                     ! horizontal position (Doviak & Zrnic)
                     ZDX_NAT(JI,JEL,JAZ,JL,JH,JV)=ZKE*XRADIUS*ASIN(JL*XSTEP_RAD*COS(ZELEV(JI,JEL,JL,JV)) &
                          /(ZKE*XRADIUS+ZZ_RAY(JI,JEL,JAZ,JL+1,JH,JV)))*SIN(ZAZIM(JI,JAZ,JH))
                     ZDY_NAT(JI,JEL,JAZ,JL,JH,JV)=ZKE*XRADIUS*ASIN(JL*XSTEP_RAD*COS(ZELEV(JI,JEL,JL,JV)) &
                          /(ZKE*XRADIUS+ZZ_RAY(JI,JEL,JAZ,JL+1,JH,JV)))*COS(ZAZIM(JI,JAZ,JH))
                     ZX_RAY(JI,JEL,JAZ,JL+1,JH,JV)=ZX_RAY(JI,JEL,JAZ,1,JH,JV) & !!!
                          +  (ZMAP* XRADIUS *((ZDX_NAT(JI,JEL,JAZ,JL,JH,JV) * ZCOSG) - &
                          (ZDY_NAT(JI,JEL,JAZ,JL,JH,JV)* ZSING)  ) &
                          /(ZZ_RAY(JI,JEL,JAZ,JL,JH,JV) + XRADIUS))
                     ZY_RAY(JI,JEL,JAZ,JL+1,JH,JV)=ZY_RAY(JI,JEL,JAZ,1,JH,JV) + &
                          (ZMAP* XRADIUS *((ZDX_NAT(JI,JEL,JAZ,JL,JH,JV) * ZSING) +  &
                          (ZDY_NAT(JI,JEL,JAZ,JL,JH,JV)* ZCOSG)  ) &
                          /(ZZ_RAY(JI,JEL,JAZ,JL,JH,JV) + XRADIUS))
                  END IF
               END DO
            END DO
         END DO label
      END DO
   END DO
END DO
DEALLOCATE(ZLAT,ZLON)
DEALLOCATE(ZDX_NAT,ZDY_NAT)
IF(NCURV_INTERPOL == 1) DEALLOCATE(ZN,ZDNDZ)
! end of geometrical part ; I determined z[xyz]_ray
WRITE(0,*) 'BEAM DEFINITION DONE'
!
!-------------------------------------------------------------------------------
!*       3.    INTERPOLATION OF THE MODEL VARIABLES ON THE RAYS 
!              ------------------------------------------------
!  
!
!*       3.1  allocation of the arrays and initialization of the arrays of pointers 
!              (to avoid multiple calls to interpol_beam)
!
! 1: temperature; 2: rhodref, 3: rain mixing ratio; 4: r_i; 5: CIT; 6: r_s; 7: r_g; 8: convective/stratiform; 9: u; 10: v; 11: w
ALLOCATE(TVARMOD(NRR+5))
ALLOCATE(TVARRAD(NRR+5))
TVARMOD(1)%P=>PTEMP(:,:,:)
TVARMOD(2)%P=>PRHODREF(:,:,:)
ALLOCATE(ZT_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V),&
     ZRHODREF_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V))
TVARRAD(1)%P=>ZT_RAY(:,:,:,:,:,:)
TVARRAD(2)%P=>ZRHODREF_RAY(:,:,:,:,:,:)
INVAR=2
! raindrops
IF(SIZE(PRT,4)>2) THEN
  INVAR=INVAR+1
  TVARMOD(INVAR)%P=>PRT(:,:,:,3)
  ALLOCATE(ZR_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V))  
  TVARRAD(INVAR)%P=>ZR_RAY(:,:,:,:,:,:)
END IF
! pristine ice
IF (SIZE(PRT,4)>3) THEN
  INVAR=INVAR+1
  TVARMOD(INVAR)%P=>PRT(:,:,:,4)
  ALLOCATE(ZI_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V))
  TVARRAD(INVAR)%P=>ZI_RAY(:,:,:,:,:,:)
  INVAR=INVAR+1
  TVARMOD(INVAR)%P=>PCIT(:,:,:)
  ALLOCATE(ZCIT_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V))
  TVARRAD(5)%P=>ZCIT_RAY(:,:,:,:,:,:)
END IF
! snow
IF (SIZE(PRT,4)>4) THEN
  INVAR=INVAR+1
  TVARMOD(INVAR)%P=>PRT(:,:,:,5)
  ALLOCATE(ZS_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V))
  TVARRAD(INVAR)%P=>ZS_RAY(:,:,:,:,:,:)
END IF
! graupel
IF (SIZE(PRT,4)>5) THEN
  INVAR=INVAR+1
  TVARMOD(INVAR)%P=>PRT(:,:,:,6)
  ALLOCATE(ZG_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V))
  TVARRAD(INVAR)%P=>ZG_RAY(:,:,:,:,:,:)
END IF
! convective/stratiform
TVARMOD(INVAR+1)%P=>ZBU_MASK(:,:,:)
ALLOCATE(ZBU_MASK_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V))
TVARRAD(INVAR+1)%P=>ZBU_MASK_RAY(:,:,:,:,:,:)
! wind components
TVARMOD(INVAR+2)%P=>PUM(:,:,:)
TVARMOD(INVAR+3)%P=>PVM(:,:,:)
TVARMOD(INVAR+4)%P=>PWM(:,:,:)
ALLOCATE(ZUM_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V),&
     ZVM_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V),&
     ZWM_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V))
TVARRAD(INVAR+2)%P=>ZUM_RAY(:,:,:,:,:,:)
TVARRAD(INVAR+3)%P=>ZVM_RAY(:,:,:,:,:,:)
TVARRAD(INVAR+4)%P=>ZWM_RAY(:,:,:,:,:,:)

!*       3.2   interpolation of all model variables

CALL INTERPOL_BEAM(TVARMOD,TVARRAD,ZX_RAY(:,:,:,:,:,:),&
     ZY_RAY(:,:,:,:,:,:),ZZ_RAY(:,:,:,:,:,:),ZXHATM(:),ZYHATM(:),ZZM(:,:,:)) 
!
DEALLOCATE(ZBU_MASK)
DEALLOCATE(ZXHATM,ZYHATM,ZZM)
DEALLOCATE(ZX_RAY,ZY_RAY)
DEALLOCATE(TVARMOD,TVARRAD)
!
!Doppler velocities (unfolded): wind contribution
ALLOCATE(ZVDOP_RAY(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,NPTS_H,NPTS_V))
DO JI=1,NBRAD
  IEL=NBELEV(JI)
  DO JEL=1,IEL
     DO JAZ=1,NBAZIM
        DO JL=1,NBSTEPMAX+1
           DO JH=1,NPTS_H
              DO JV=1,NPTS_V
                 IF(ZUM_RAY(JI,JEL,JAZ,JL,JH,JV)/=-XUNDEF) THEN
                    ZVDOP_RAY(JI,JEL,JAZ,JL,JH,JV)=(ZUM_RAY(JI,JEL,JAZ,JL,JH,JV)*SIN(ZAZIM(JI,JAZ,JH))&
                         +ZVM_RAY(JI,JEL,JAZ,JL,JH,JV)*COS(ZAZIM(JI,JAZ,JH)))*COS(ZELEV(JI,JEL,JL,JV))&
                         +ZWM_RAY(JI,JEL,JAZ,JL,JH,JV)*SIN(ZELEV(JI,JEL,JL,JV))
                 ELSE
                    ZVDOP_RAY(JI,JEL,JAZ,JL,JH,JV)=-XUNDEF
                 END IF
              END DO
           END DO
        END DO
     END DO
  END DO
END DO
DEALLOCATE(ZAZIM,ZUM_RAY,ZVM_RAY,ZWM_RAY)

WRITE(0,*) 'INTERPOLATION OF MODEL VARIABLES DONE'
!
!-----------------------------------------------------------------------------------------
!*       4.    COMPUTING REFLECTIVITIES ALONG THE RAY BEAM (BACKSCATTERING  + ATTENUATION) 
!              ---------------------------------------------------------------------------    
ALLOCATE(ZZE(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,SIZE(PREFL_CART(:,:,:,:,:),5)))

CALL RADAR_SCATTERING(ZT_RAY,ZRHODREF_RAY,ZR_RAY,ZI_RAY,ZCIT_RAY,ZS_RAY,ZG_RAY,ZVDOP_RAY, &
     ZELEV,ZX_H,ZX_V,ZW_H,ZW_V,ZZE(:,:,:,:,1:IHAS-1),ZBU_MASK_RAY)

DEALLOCATE(ZVDOP_RAY)
! convective/stratiform
DEALLOCATE(ZBU_MASK_RAY)
! /convective/stratiform
! conversion discretised gates -> single point gates for other output fields
ZZE(:,:,:,:,IHAS)=ZZ_RAY(:,:,:,:,(NPTS_H+1)/2,(NPTS_V+1)/2) ! beam height
ZZE(:,:,:,:,IHAS+1)=ZRHODREF_RAY(:,:,:,:,(NPTS_H+1)/2,(NPTS_V+1)/2)*ZR_RAY(:,:,:,:,(NPTS_H+1)/2,(NPTS_V+1)/2) ! M_r
ZZE(:,:,:,:,IHAS+2)=ZRHODREF_RAY(:,:,:,:,(NPTS_H+1)/2,(NPTS_V+1)/2)*ZI_RAY(:,:,:,:,(NPTS_H+1)/2,(NPTS_V+1)/2) ! M_i
ZZE(:,:,:,:,IHAS+3)=ZRHODREF_RAY(:,:,:,:,(NPTS_H+1)/2,(NPTS_V+1)/2)*ZS_RAY(:,:,:,:,(NPTS_H+1)/2,(NPTS_V+1)/2) ! M_s
ZZE(:,:,:,:,IHAS+4)=ZRHODREF_RAY(:,:,:,:,(NPTS_H+1)/2,(NPTS_V+1)/2)*ZG_RAY(:,:,:,:,(NPTS_H+1)/2,(NPTS_V+1)/2) ! M_g
ZZE(:,:,:,:,IHAS+5)=ZCIT_RAY(:,:,:,:,(NPTS_H+1)/2,(NPTS_V+1)/2) ! CIT
IF(LREFR) THEN
   ZZE(:,:,:,:,IRFR)=ZN_RAY(:,:,:,:,(NPTS_H+1)/2,(NPTS_V+1)/2) ! refractivity
   ZZE(:,:,:,NBSTEPMAX+1,IRFR)=XVALGROUND
   DEALLOCATE(ZN_RAY)
END IF
IF(LDNDZ) THEN
   ZZE(:,:,:,:,IDNZ)=ZDNDZ_RAY(:,:,:,:,(NPTS_H+1)/2,(NPTS_v+1)/2) ! refractivity vertical gradient
   DEALLOCATE(ZDNDZ_RAY)
   ZZE(:,:,:,1,IDNZ)=XVALGROUND ! we can do it now
END IF
!
DEALLOCATE(ZELEV)
DEALLOCATE(ZW_H,ZW_V)
DEALLOCATE(ZX_H,ZX_V)
DEALLOCATE(ZT_RAY,ZRHODREF_RAY,ZZ_RAY)
IF(ALLOCATED(ZR_RAY)) DEALLOCATE(ZR_RAY)
IF(ALLOCATED(ZI_RAY)) DEALLOCATE(ZI_RAY,ZCIT_RAY)
IF(ALLOCATED(ZS_RAY)) DEALLOCATE(ZS_RAY)
IF(ALLOCATED(ZG_RAY)) DEALLOCATE(ZG_RAY)
!
!----------------------------------------------------------------------------------------------
!*       5.    INTERPOLATION ON THE CARTESIAN GRID 
!              -----------------------------------
IF (LCART_RAD) THEN
  ALLOCATE(IREFL_CART_NB(NBRAD,IIELV,2*NMAX,2*NMAX),IVDOP_CART_NB(NBRAD,IIELV,2*NMAX,2*NMAX))
  PREFL_CART(:,:,:,:,:)=0.
  IREFL_CART_NB(:,:,:,:)=0
  IVDOP_CART_NB(:,:,:,:)=0
!
!*       5.1  reflectivity on a cartesian grid (this is the way DSO/CMR creates BUFRs)
!  
  DO JI=1,NBRAD
    IEL=NBELEV(JI)
    DO JEL=1,IEL
      DO JAZ=1,NBAZIM
        DO JL=1,NBSTEPMAX+1
          IXGRID=CEILING(NMAX+((JL-1)*XSTEP_RAD*SIN(ZAZIM_BASE(JAZ))/XGRID))
          IYGRID=CEILING(NMAX+((JL-1)*XSTEP_RAD*COS(ZAZIM_BASE(JAZ))/XGRID))
          ! assigning polar grid values to cartesian grid
          IF(ZZE(JI,JEL,JAZ,JL,1)==XVALGROUND.OR.PREFL_CART(JI,JEL,IXGRID,IYGRID,1)==XVALGROUND &
               .OR.(LREFR.AND.ZZE(JI,JEL,JAZ,JL,IRFR)==XVALGROUND) &    ! case for refractivity at boundaries
               .OR.(LDNDZ.AND.ZZE(JI,JEL,JAZ,JL,IDNZ)==XVALGROUND) & ! case for refractivity gradient at origin
               ) THEN ! if any XVALGROUND in the pixel -> pixel set to XVALGROUND
            PREFL_CART(JI,JEL,IXGRID,IYGRID,:)=XVALGROUND
            IREFL_CART_NB(JI,JEL,IXGRID,IYGRID)=1
            IVDOP_CART_NB(JI,JEL,IXGRID,IYGRID)=1
          ELSE 
            PREFL_CART(JI,JEL,IXGRID,IYGRID,:IVDOP-1)=PREFL_CART(JI,JEL,IXGRID,IYGRID,:IVDOP-1) &
                 +ZZE(JI,JEL,JAZ,JL,:IVDOP-1)
            PREFL_CART(JI,JEL,IXGRID,IYGRID,IVDOP+1:)=PREFL_CART(JI,JEL,IXGRID,IYGRID,IVDOP+1:) &
                 +ZZE(JI,JEL,JAZ,JL,IVDOP+1:)
            IREFL_CART_NB(JI,JEL,IXGRID,IYGRID)=IREFL_CART_NB(JI,JEL,IXGRID,IYGRID)+1
            IF(ZZE(JI,JEL,JAZ,JL,IVDOP)/=-XUNDEF.AND.PREFL_CART(JI,JEL,IXGRID,IYGRID,IVDOP)/=XVALGROUND) THEN
               PREFL_CART(JI,JEL,IXGRID,IYGRID,IVDOP)=PREFL_CART(JI,JEL,IXGRID,IYGRID,IVDOP)+ZZE(JI,JEL,JAZ,JL,IVDOP)
               IVDOP_CART_NB(JI,JEL,IXGRID,IYGRID)=IVDOP_CART_NB(JI,JEL,IXGRID,IYGRID)+1
            ELSE
               PREFL_CART(JI,JEL,IXGRID,IYGRID,IVDOP)=XVALGROUND
               IVDOP_CART_NB(JI,JEL,IXGRID,IYGRID)=1
            END IF
          END IF
        END DO ! JL
      END DO !
    END DO
  END DO

DEALLOCATE(ZZE)


!*       5.2  writing cartesian grid output (averaging)
! Now out-of-range pixels are affected 0, and underground pixels are affected XVALGROUND
  DO JI=1,NBRAD
    IEL=NBELEV(JI)
    DO JEL=1,IEL
      DO JV=2*NMAX,1,-1
        DO JH=1,2*NMAX
          IF(IREFL_CART_NB(JI,JEL,JH,JV) == 0) THEN
            IF((JH+SIGN(.5,JH-.5-NMAX)-.5-NMAX)**2+(JV+SIGN(.5,JV-.5-NMAX)-.5-NMAX)**2>=NMAX**2) THEN 
               ! out of range
              PREFL_CART(JI,JEL,JH,JV,:)=XVALGROUND
            ELSE
              PREFL_CART(JI,JEL,JH,JV,:)=0.
              WRITE(*,*) "Warning: some pixels have no reflectivity; increase XGRID or decrease XSTEP_RAD"
            END IF
          ELSE
            PREFL_CART(JI,JEL,JH,JV,:)=PREFL_CART(JI,JEL,JH,JV,:)/IREFL_CART_NB(JI,JEL,JH,JV)
            IF(IVDOP_CART_NB(JI,JEL,JH,JV) == 0) THEN
               PREFL_CART(JI,JEL,JH,JV,IVDOP)=XVALGROUND
            ELSE
               PREFL_CART(JI,JEL,JH,JV,IVDOP)=PREFL_CART(JI,JEL,JH,JV,IVDOP) &
                    *IREFL_CART_NB(JI,JEL,JH,JV)/IVDOP_CART_NB(JI,JEL,JH,JV)
            END IF

						! thresholding and converting
            ! Unit conversion :             
						IF(PREFL_CART(JI,JEL,JH,JV,1) > 10**(XREFLMIN/10.) ) THEN  ! unit conversion (mm^6 m^{-3} -> dBZ)
               PREFL_CART(JI,JEL,JH,JV,1)=10.*LOG10(PREFL_CART(JI,JEL,JH,JV,1)) ! Z_equiv in dBZ
              IF(PREFL_CART(JI,JEL,JH,JV,2) > 0. ) THEN
								PREFL_CART(JI,JEL,JH,JV,2)=PREFL_CART(JI,JEL,JH,JV,1) &
								-10.*LOG10(PREFL_CART(JI,JEL,JH,JV,2)) ! Zdr=Z_HH-Z_VV  
							ELSE
								PREFL_CART(JI,JEL,JH,JV,2)=-XUNDEF
							ENDIF
              WHERE(PREFL_CART(JI,JEL,JH,JV,IZER:IZEG)> 10**(XREFLMIN/10.))
                 PREFL_CART(JI,JEL,JH,JV,IZER:IZEG)=10.*LOG10(PREFL_CART(JI,JEL,JH,JV,IZER:IZEG))
              ELSEWHERE
                 PREFL_CART(JI,JEL,JH,JV,IZER:IZEG)=-XUNDEF
              END WHERE
            ELSE IF(PREFL_CART(JI,JEL,JH,JV,1) >= 0.) THEN           ! few/no hydrometeor present
               PREFL_CART(JI,JEL,JH,JV,1:2)=-XUNDEF
               PREFL_CART(JI,JEL,JH,JV,IZER:IZEG)=-XUNDEF 
! Next case should not happen
!            ELSE                                       ! flag bin when underground                    
!               PZE(JI,JEL,JAZ,JL,1)=XVALGROUND
!               PZE(JI,JEL,JAZ,JL,IZER:IZEG)=XVALGROUND
            END IF

						IF(LATT) THEN
							 WHERE(PREFL_CART(JI,JEL,JH,JV,IATR:IATG)>0.)
								 PREFL_CART(JI,JEL,JH,JV,IATR:IATG)=10.*LOG10(PREFL_CART(JI,JEL,JH,JV,IATR:IATG))
               END WHERE
						ENDIF

          END IF

        END DO
      END DO
    END DO
  END DO
  WRITE(0,*) 'CARTESIAN GRID INTERPOLATION DONE'
  DEALLOCATE(IREFL_CART_NB,IVDOP_CART_NB)
!
!*       5.3  positions of the cartesian grid (as in R2, provided 'as is')
!
  DO JI=1,NBRAD
     DO JEL=1,IEL
        DO JV=1,2*NMAX
           DO JH=1,2*NMAX
              r=SQRT((JH-.5-NMAX)**2+(JV-.5-NMAX)**2)*XGRID
              h=XRADIUS+r*XELEV(JI,JEL)*ZRDSDG+r*r/(2.*ZKE*XRADIUS)
              alph=ACOS((XRADIUS*XRADIUS+h*h-r*r)/(2.*XRADIUS*h))
              
              PLATLON(JI,2*JH-1,JV)=ASIN(SIN(XLAT_RAD(JI)*ZRDSDG)*COS(alph)+COS(XLAT_RAD(JI)*ZRDSDG)*SIN(alph)* & ! LAT
                   (JV-.5-NMAX)/SQRT((JH-.5-NMAX)**2+(JV-.5-NMAX)**2))/ZRDSDG
              PLATLON(JI,2*JH,JV)=XLON_RAD(JI)+ASIN(SIN(alph)* & ! lon
                   (JH-.5-NMAX)/SQRT((JH-.5-NMAX)**2+(JV-.5-NMAX)**2)/ &
                   COS(PLATLON(JI,2*JH-1,JV)*ZRDSDG))/ZRDSDG
           END DO
        END DO
     END DO
  END DO

! polar output
ELSE
  PREFL_CART(:,:,:,:,:)=ZZE(:,:,:,:,:)

  DEALLOCATE(ZZE)

  DO JI=1,NBRAD
     DO JAZ=1,NBAZIM
        PLATLON(1,JAZ,1)=ZAZIM_BASE(JAZ)
     END DO
  END DO

END IF
DEALLOCATE(ZAZIM_BASE)
WRITE(0,*) 'ROUTINE RADAR_SIMULATOR COMPLETED'
END SUBROUTINE RADAR_SIMULATOR

