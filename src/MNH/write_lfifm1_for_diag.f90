!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source: /home/cvsroot/MNH-VX-Y-Z/src/MNH/write_lfifm1_for_diag.f90,v $ $Revision: 1.3.2.5.2.4.2.3.2.3.2.4 $
! masdev4_7 BUG1 2007/06/15 17:47:18
!-----------------------------------------------------------------
!################################
MODULE MODI_WRITE_LFIFM1_FOR_DIAG
!################################
INTERFACE
      SUBROUTINE WRITE_LFIFM1_FOR_DIAG(HFMFILE,HDADFILE)
!*       0.1   Declarations of arguments
!
CHARACTER(LEN=28), INTENT(IN) :: HFMFILE      ! Name of FM-file to write
CHARACTER(LEN=28), INTENT(IN) :: HDADFILE     ! corresponding FM-file name of 
                                              ! its DAD model
!
END SUBROUTINE WRITE_LFIFM1_FOR_DIAG
END INTERFACE
END MODULE MODI_WRITE_LFIFM1_FOR_DIAG
!
!     ##################################################
      SUBROUTINE WRITE_LFIFM1_FOR_DIAG(HFMFILE,HDADFILE)
!     ##################################################
!
!!****  *WRITE_LFIFM1* - routine to write a LFIFM file for model 1
!!
!!    PURPOSE
!!    -------
!        The purpose of this routine is to write an initial LFIFM File 
!     of name HFMFILE//'.lfi' with the FM routines.  
!
!!**  METHOD
!!    ------
!!      The data are written in the LFIFM file :
!!        - dimensions
!!        - grid variables
!!        - configuration variables
!!        - prognostic variables at time t and t-dt
!!        - 1D anelastic reference state
!!
!!      The localization on the model grid is also indicated :
!!
!!        IGRID = 1 for mass grid point
!!        IGRID = 2 for U grid point
!!        IGRID = 3 for V grid point
!!        IGRID = 4 for w grid point
!!        IGRID = 0 for meaningless case
!!          
!!
!!    EXTERNAL
!!    --------
!!      FMWRIT : FM-routine to write a record
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_DIM1   : contains dimensions
!!      Module MODD_TIME1   : contains time variables and uses MODD_TIME
!!      Module MODD_GRID    : contains spatial grid variables for all models
!!      Module MODD_GRID1 : contains spatial grid variables
!!      Module MODD_REF     : contains reference state variables
!!      Module MODD_LUNIT1: contains logical unit variables.
!!      Module MODD_CONF    : contains configuration variables for all models
!!      Module MODD_CONF1  : contains configuration variables
!!      Module MODD_FIELD1  : contains prognostic variables
!!      Module MODD_GR_FIELD1 : contains surface prognostic variables
!!      Module MODD_LSFIELD1  : contains Larger Scale variables
!!      Module MODD_PARAM1    : contains parameterization options
!!      Module MODD_TURB1    : contains turbulence options
!!      Module MODD_FRC    : contains forcing variables
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!  	V. Ducrocq   *Meteo France* 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/05/94 
!!       V. Ducrocq    27/06/94
!!       J.Stein       20/10/94 (name of the FMFILE)
!!       J.Stein       06/12/94 add the LS fields
!!       J.P. Lafore   09/01/95 add the DRYMASST
!!       J.Stein       20/01/95 add TKE and change the ycomment for the water
!!                              variables
!!       J.Stein       23/01/95 add a TKE switch and MODD_PARAM1
!!       J.Stein       16/03/95 remove R from the historical variables
!!       J.Stein       20/03/95 add the EPS var.
!!       J.Stein       30/06/95 add the variables related to the subgrid condens
!!       S. Belair     01/09/95 add surface variables and ground parameters
!!       J.-P. Pinty   15/09/95 add the radiation parameters
!!       J.Stein       23/01/96 add the TSZ0 option for the surface scheme
!!       M.Georgelin   13/12/95 add the forcing variables
!!       J.-P. Pinty   15/02/96 add external control for the forcing
!!       J.Stein P.Bougeault  15/03/96 add the cloud fraction and change the
!!                                     surface parameters for TSZ0 option
!!       J.Stein P.Jabouille  30/04/96 add the storage type
!!       J.Stein P.Jabouille  20/05/96 switch for XSIGS and XSRC
!!       J.Stein              10/10/96 change Xsrc into XSRCM and XRCT
!!       J.P. Lafore          30/07/96 add HFMFILE and HDADFILE writing
!!                                     corresponding to MY_NAME and DAD_NAME (for nesting)
!!       V.Masson             08/10/96 add LTHINSHELL
!!       J.-P. Pinty   15/12/96 add the microphysics (ice)
!!       J.-P. Pinty   11/01/97 add the deep convection
!!       J.-P. Pinty   27/01/97 split the recording of the SV array
!!       J.-P. Pinty   29/01/97 set recording of PRCONV and PACCONV in mm/h and
!!                                                         mm respectively
!!       J. Viviand    04/02/97 convert precipitation rates in mm/h
!!       P. Hereil     04/12/97 add the calculation of cloud top and moist PV
!!       P.Hereil N Asencio 3/02/98 add the calculation of  precipitation on large scale grid mesh
!!       N Asencio 2/10/98 suppress flux calculation if start file
!!       V Masson 25/11/98 places dummy arguments in module MODD_DIAG_FLAG
!!       V Masson 04/01/00 removes TSZ0 option
!!       J.-P. Pinty   29/11/02 add C3R5, ICE2, ICE4, CELEC
!!       V Masson 01/2004  removes surface (externalization)
!!       P. Tulet 01/2005   add dust, orilam
!!       M. Leriche 04/2007 add aqueous concentration in M
!!       O. Caumont 03/2008 add simulation of radar observations
!!       O. Caumont 14/09/2009 modifications to allow for polar outputs (radar diagnostics)
!!       October 2009 (G. Tanguy) add ILENCH=LEN(YCOMMENT) after
!!                                              change of YCOMMENT
!!       G. Tanguy  10/2009 add possibility to run radar after 
!!                          PREP_REAL_CASE with AROME
!!       O. Caumont 01/2011 [radar diagnostics] add control check for NMAX; revise comments
!!       O. Caumont 05/2011 [radar diagnostics] change output format
!!       G.Tanguy/ JP Pinty/ JP Chabureau 18/05/2011 : add lidar simulator
!!       S.Bielli 12/2012 : add latitude and longitude
!!       F. Duffourg 02/2013 : add new fields
!!      J.Escobar 21/03/2013: for HALOK get correctly local array dim/bound
!!       J. escobar 27/03/2014 : write LAT/LON only in not CARTESIAN case
!!       G.Delautier    2014 : remplace MODD_RAIN_C2R2_PARAM par MODD_RAIN_C2R2_KHKO_PARAM
!!       C. Augros 2014 : new radar simulator (T matrice)
!!       D.Ricard 2015 : add THETAES + POVOES  (LMOIST_ES=T)
!!      Modification    01/2016  (JP Pinty) Add LIMA
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DIM_n
USE MODD_CONF
USE MODD_CONF_n
USE MODD_GRID
USE MODD_GRID_n
USE MODD_METRICS_n
USE MODD_TIME
USE MODD_TIME_n
USE MODD_DYN_n
USE MODD_FIELD_n
USE MODD_GR_FIELD_n
USE MODD_LSFIELD_n
USE MODD_PARAM_n
USE MODD_CURVCOR_n
USE MODD_REF
USE MODD_REF_n
USE MODD_LUNIT, ONLY : CLUOUT0
USE MODD_LUNIT_n
USE MODD_TURB_n
USE MODD_RADIATIONS_n
USE MODD_FRC
USE MODD_PRECIP_n
USE MODD_CST
USE MODD_CLOUDPAR
USE MODD_DEEP_CONVECTION_n
USE MODD_PARAM_KAFR_n
USE MODD_NESTING
USE MODD_PARAMETERS
USE MODD_DIAG_FLAG
USE MODD_NSV
USE MODD_CH_M9_n,         ONLY : CNAMES, NEQAQ
USE MODD_RAIN_C2R2_DESCR, ONLY : C2R2NAMES
USE MODD_ICE_C1R3_DESCR,  ONLY : C1R3NAMES
USE MODD_ELEC_DESCR, ONLY : CELECNAMES
USE MODD_RAIN_C2R2_KHKO_PARAM
USE MODD_ICE_C1R3_PARAM
USE MODD_PARAM_ICE,       ONLY : LSEDIC
USE MODD_PARAM_LIMA,      ONLY : NMOD_CCN, NMOD_IFN, NMOD_IMM, NINDICE_CCN_IMM,&
                                 LSCAV, LHHONI, LAERO_MASS,                    &
                                 LLIMA_DIAG,                                   &
                                 NSPECIE, XMDIAM_IFN, XSIGMA_IFN, ZFRAC=>XFRAC,&
                                 XR_MEAN_CCN, XLOGSIG_CCN 
USE MODD_PARAM_LIMA_WARM, ONLY : CLIMA_WARM_CONC, CAERO_MASS
USE MODD_PARAM_LIMA_COLD, ONLY : CLIMA_COLD_CONC
USE MODD_LG,              ONLY : CLGNAMES
USE MODD_PASPOL,          ONLY : LPASPOL
USE MODD_CONDSAMP,        ONLY : LCONDSAMP
!
USE MODD_DIAG_FLAG
USE MODD_RADAR, ONLY: XLAT_RAD,XELEV,&
     XSTEP_RAD,NBRAD,NBELEV,NBAZIM,NBSTEPMAX,&
     NCURV_INTERPOL,LATT,LCART_RAD,NPTS_H,NPTS_V,XGRID,&
     LREFR,LDNDZ,NMAX,CNAME_RAD,NDIFF,&
     XLON_RAD,XALT_RAD,XLAM_RAD,XDT_RAD,LWBSCS,LWREFL
!
USE MODI_RADAR_SIMULATOR
!
USE MODD_DUST
USE MODD_CSTS_DUST
USE MODD_SALT
USE MODD_CH_AEROSOL
USE MODD_CH_AERO_n
USE MODD_CH_MNHC_n
USE MODE_DUST_PSD
USE MODE_SALT_PSD
USE MODE_AERO_PSD
USE MODI_GRADIENT_M
USE MODI_GRADIENT_W
USE MODI_GRADIENT_U
USE MODI_GRADIENT_V
USE MODI_SHUMAN
USE MODI_RADAR_RAIN_ICE
USE MODI_INI_RADAR
USE MODI_COMPUTE_MEAN_PRECIP
USE MODI_UV_TO_ZONAL_AND_MERID
USE MODI_CALCSOUND
USE MODI_FREE_ATM_PROFILE
USE MODI_GPS_ZENITH
USE MODI_GATHER_ll
!
USE MODE_GRIDPROJ
USE MODE_FMWRIT
USE MODE_ll
USE MODE_IO_ll
USE MODE_THERMO
USE MODE_MODELN_HANDLER
USE MODI_LIDAR
!
USE MODD_MPIF
USE MODD_VAR_ll
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!
CHARACTER(LEN=28), INTENT(IN) :: HFMFILE      ! Name of FM-file to write
CHARACTER(LEN=28), INTENT(IN) :: HDADFILE     ! corresponding FM-file name of 
                                              ! its DAD model
!
!*       0.2   Declarations of local variables
!
INTEGER           :: IRESP          ! return-code for the file routines 
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string 
!
CHARACTER(LEN=28) :: YFMFILE        ! Temporary variable to store FM-file name
CHARACTER(LEN=16) :: YRECFM         ! Name of the article to be written
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
!
CHARACTER(LEN=2)  :: YSTORAGE_TYPE  ! type of the new DIAG file ('DI')
!
CHARACTER(LEN=3)  :: YFRC           ! to mark the time of the forcing
CHARACTER(LEN=31) :: YFGRI          ! file name for GPS stations
!
INTEGER           :: IIU,IJU,IKU,IIB,IJB,IKB,IIE,IJE,IKE ! Arrays bounds
! 
INTEGER                :: IRR
INTEGER                :: JLOOP,JI,JJ,JK,JSV,JT,JH,JV,JEL    ! loop index
INTEGER :: IMI ! Current model index
! 
REAL :: ZRV_OV_RD !  XRV / XRD
REAL :: ZGAMREF   ! Standard atmosphere lapse rate (K/m)
REAL :: ZX0D      ! work real scalar  
REAL :: ZLATOR, ZLONOR ! geographical coordinates of 1st mass point
REAL :: ZXHATM, ZYHATM ! conformal    coordinates of 1st mass point
REAL, DIMENSION(:), ALLOCATABLE   :: ZXHAT_ll    !  Position x in the conformal
                                                 ! plane (array on the complete domain)
REAL, DIMENSION(:), ALLOCATABLE   :: ZYHAT_ll    !   Position y in the conformal
                                                 ! plane (array on the complete domain)
!
REAL,DIMENSION(SIZE(XTHT,1),SIZE(XTHT,2),SIZE(XTHT,3))  :: ZPOVO
REAL,DIMENSION(SIZE(XTHT,1),SIZE(XTHT,2),SIZE(XTHT,3))  :: ZTEMP
REAL,DIMENSION(SIZE(XTHT,1),SIZE(XTHT,2),SIZE(XTHT,3))  :: ZVOX,ZVOY,ZVOZ 
REAL,DIMENSION(SIZE(XTHT,1),SIZE(XTHT,2),SIZE(XTHT,3))  :: ZCORIOZ 
REAL,DIMENSION(SIZE(XTHT,1),SIZE(XTHT,2),SIZE(XTHT,3))  :: ZWORK31,ZWORK32
REAL,DIMENSION(SIZE(XTHT,1),SIZE(XTHT,2),SIZE(XTHT,3))  :: ZWORK33,ZWORK34
REAL,DIMENSION(SIZE(XTHT,1),SIZE(XTHT,2))               :: ZWORK21,ZWORK22
REAL,DIMENSION(SIZE(XTHT,1),SIZE(XTHT,2))               :: ZWORK23,ZWORK24
REAL,DIMENSION(:,:,:,:,:), ALLOCATABLE                  :: ZWORK42 ! reflectivity on a cartesian grid (PREFL_CART)
REAL,DIMENSION(:,:,:,:,:), ALLOCATABLE                  :: ZWORK42_BIS
REAL,DIMENSION(:,:,:), ALLOCATABLE                      :: ZWORK43 ! latlon coordinates of cartesian grid points (PLATLON)
REAL,DIMENSION(:,:,:), ALLOCATABLE                      :: ZPHI,ZTHETAE,ZTHETAV
REAL,DIMENSION(:,:,:), ALLOCATABLE                      :: ZTHETAES
INTEGER, DIMENSION(:,:), ALLOCATABLE                    :: IWORK1
integer :: ICURR,INBOUT,IERR
!
REAL,DIMENSION(SIZE(XSVT,1),SIZE(XSVT,2),SIZE(XSVT,3),NSP+NCARB+NSOA,JPMODE):: ZPTOTA
REAL,DIMENSION(SIZE(XSVT,1),SIZE(XSVT,2),SIZE(XSVT,3),NMODE_DST*2):: ZSDSTDEP
REAL,DIMENSION(SIZE(XSVT,1),SIZE(XSVT,2),SIZE(XSVT,3),NMODE_SLT*2):: ZSSLTDEP
REAL,DIMENSION(:,:,:,:), ALLOCATABLE  :: ZSIG_DST, ZRG_DST, ZN0_DST
REAL,DIMENSION(:,:,:,:), ALLOCATABLE  :: ZSIG_SLT, ZRG_SLT, ZN0_SLT
REAL,DIMENSION(:,:,:), ALLOCATABLE  :: ZRHOT, ZTMP ! work array

!ECRITURE DANS UN FICHIER ASCII DE RESULTATS 
!INITIALISATION DU NOM DE FICHIER CREE EN PARALLELE AVEC CELUI LFI
INTEGER :: ILURS
CHARACTER(LEN=32) :: YRS
CHARACTER(LEN=3),DIMENSION(:),ALLOCATABLE  :: YRAD
CHARACTER(LEN=2*INT(NBSTEPMAX*XSTEP_RAD/XGRID)*2*9+1), DIMENSION(:), ALLOCATABLE :: CLATLON
CHARACTER(LEN=2*9) :: CBUFFER
CHARACTER(LEN=4)  :: YELEV
CHARACTER(LEN=3)  :: YGRID_SIZE
INTEGER :: IEL,IIELV
CHARACTER(LEN=5)  :: YVIEW   ! Upward or Downward integration
INTEGER           :: IACCMODE
!-------------------------------------------------------------------------------
INTEGER :: IAUX ! work variable 
REAL,DIMENSION(SIZE(XTHT,1),SIZE(XTHT,2),SIZE(XTHT,3)) :: ZWORK35,ZWORK36
REAL,DIMENSION(SIZE(XTHT,1),SIZE(XTHT,2))              :: ZWORK25,ZWORK26
REAL    :: ZEAU ! Mean precipitable water
INTEGER, DIMENSION(SIZE(XZZ,1),SIZE(XZZ,2))          ::IKTOP ! level in which is the altitude 3000m
REAL, DIMENSION(SIZE(XZZ,1),SIZE(XZZ,2),SIZE(XZZ,3)) :: ZDELTAZ ! interval (m) between two levels K
INTEGER :: ILUOUT0 ! Logical unit number for output-listing
!
CHARACTER(LEN=2)  :: INDICE
INTEGER           :: I
!
! LIMA LIDAR
REAL,DIMENSION(:,:,:,:), ALLOCATABLE :: ZTMP1, ZTMP2, ZTMP3, ZTMP4
!
!-------------------------------------------------------------------------------
!
!*       0.     ARRAYS BOUNDS INITIALIZATION
!
CALL GET_DIM_EXT_ll ('B',IIU,IJU)
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
IKU=NKMAX+2*JPVEXT
IKB=1+JPVEXT
IKE=IKU-JPVEXT

IMI = GET_CURRENT_MODEL_INDEX()
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
!-------------------------------------------------------------------------------
!
!*       1.     WRITES IN THE LFI FILE
!               ---------------------- 
!
!*       1.0    HFMFILE and HDADFILE :
!
YRECFM='MASDEV'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',NMASDEV,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='BUGFIX'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',NBUGFIX,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='BIBUSER'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',CBIBUSER,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='PROGRAM'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',CPROGRAM,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='L1D'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',L1D,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='L2D'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',L2D,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='PACK'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',LPACK,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='MY_NAME'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',HFMFILE,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='DAD_NAME'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',HDADFILE,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (LEN_TRIM(HDADFILE)>0) THEN
  CALL FMWRIT(HFMFILE,'DXRATIO',CLUOUT,'--',NDXRATIO_ALL(1),0,ILENCH,YCOMMENT,IRESP)
  CALL FMWRIT(HFMFILE,'DYRATIO',CLUOUT,'--',NDYRATIO_ALL(1),0,ILENCH,YCOMMENT,IRESP)
  CALL FMWRIT(HFMFILE,'XOR',CLUOUT,'--',NXOR_ALL(1),0,ILENCH,YCOMMENT,IRESP)
  CALL FMWRIT(HFMFILE,'YOR',CLUOUT,'--',NYOR_ALL(1),0,ILENCH,YCOMMENT,IRESP)
END IF
!
YRECFM='SURF'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',CSURF,IGRID,ILENCH,YCOMMENT,IRESP)
!
!*       1.1    Type and Dimensions :
!
YRECFM='STORAGE_TYPE'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
YSTORAGE_TYPE='DI'
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',YSTORAGE_TYPE,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='IMAX'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',NIMAX_ll,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='JMAX'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',NJMAX_ll,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='KMAX'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',NKMAX,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='JPHEXT'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',JPHEXT,IGRID,ILENCH,YCOMMENT,IRESP)
!
!*       1.2    Grid variables :
!
IF (.NOT.LCARTESIAN) THEN
  YRECFM='RPK'
  YCOMMENT='projection parameter'
  IGRID=0
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XRPK,IGRID,ILENCH,YCOMMENT,IRESP)
! 
  YRECFM='LONORI'
  YCOMMENT='longitude of the origin point (DEGREES)'
  IGRID=0
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XLONORI,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='LATORI'
  YCOMMENT='latitude of the origin point (DEGREES)'
  IGRID=0
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XLATORI,IGRID,ILENCH,YCOMMENT,IRESP)
! 
!* diagnostic of 1st mass point
!
  ALLOCATE(ZXHAT_ll(NIMAX_ll+ 2 * JPHEXT),ZYHAT_ll(NJMAX_ll+2 * JPHEXT))
  CALL GATHERALL_FIELD_ll('XX',XXHAT,ZXHAT_ll,IRESP) !//
  CALL GATHERALL_FIELD_ll('YY',XYHAT,ZYHAT_ll,IRESP) !//
  ZXHATM = 0.5 * (ZXHAT_ll(1)+ZXHAT_ll(2))
  ZYHATM = 0.5 * (ZYHAT_ll(1)+ZYHAT_ll(2))
  CALL SM_LATLON(XLATORI,XLONORI,ZXHATM,ZYHATM,ZLATOR,ZLONOR)
  DEALLOCATE(ZXHAT_ll,ZYHAT_ll)
!
  YRECFM='LONOR'
  YCOMMENT='longitude of the mass point 1-1 (DEGREES)'
  IGRID=0
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',ZLONOR,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='LATOR'
  YCOMMENT='latitude of the mass point 1-1 (DEGREES)'
  IGRID=0
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',ZLATOR,IGRID,ILENCH,YCOMMENT,IRESP)
!
END IF 
!
YRECFM='THINSHELL'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',LTHINSHELL,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='LAT0'
YCOMMENT='reference latitude for conformal projection (DEGREES)'
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XLAT0,IGRID,ILENCH,YCOMMENT,IRESP)
! 
YRECFM='LON0'
YCOMMENT='reference longitude for conformal projection (DEGREES)'
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XLON0,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='BETA'
YCOMMENT='rotation angle (DEGREES)'
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XBETA,IGRID,ILENCH,YCOMMENT,IRESP)
! 
YRECFM='XHAT'
YCOMMENT='Position x in the conformal or cartesian plane (METERS)'
IGRID=2
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XX',XXHAT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='YHAT'
YCOMMENT='Position y in the conformal or cartesian plane (METERS)'
IGRID=3
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'YY',XYHAT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='ZHAT'
YCOMMENT='height level without orography (METERS)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XZHAT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='ZS'
YCOMMENT='orography (METERS)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XZS,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='ZSMT'
YCOMMENT='smooth orography (METERS)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XZSMT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='SLEVE'
YCOMMENT=' '
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',LSLEVE,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (LSLEVE) THEN
  YRECFM='LEN1'
  YCOMMENT=' '
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XLEN1,IGRID,ILENCH,YCOMMENT,IRESP)
  YRECFM='LEN2'
  YCOMMENT=' '
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XLEN2,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
YRECFM='DTCUR'
YCOMMENT=' '
ILENCH=LEN(YCOMMENT)
IGRID=0
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',TDTCUR,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='DTEXP'   ! array of rank 3 for date is written in file
YCOMMENT=' '
ILENCH=LEN(YCOMMENT)
IGRID=0
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',TDTEXP,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='DTMOD'    ! array of rank 3 for date is written in file
YCOMMENT=' '
ILENCH=LEN(YCOMMENT)
IGRID=0
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',TDTMOD,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='DTSEG'    ! array of rank 3 for date is written in file
IGRID=0
YCOMMENT=' '
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',TDTSEG,IGRID,ILENCH,YCOMMENT,IRESP)
!
!*       1.3    Configuration  variables :
!
YRECFM='CARTESIAN'
YCOMMENT='Logical for cartesian geometry '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',LCARTESIAN,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='LBOUSS'      
YCOMMENT='Logical for Boussinesq'           
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',LBOUSS,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (LCARTESIAN .AND. LWIND_ZM) THEN
  LWIND_ZM=.FALSE.
  PRINT*,'YOU ARE IN CARTESIAN GEOMETRY SO LWIND_ZM IS FORCED TO FALSE'
END IF
!*       1.4    Reference state variables :
!
YRECFM='RHOREFZ'
YCOMMENT='rhodz for reference state without orography (kg/m3)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XRHODREFZ,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='THVREFZ'
YCOMMENT='thetavz for reference state without orography (K)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XTHVREFZ,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='EXNTOP'
YCOMMENT='Exner function at model top'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XEXNTOP,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='RHODREF'
YCOMMENT='Dry density for reference state with orography (kg/m3)'
IGRID=1
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XRHODREF,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='THVREF'
YCOMMENT='Thetav for reference state with orography (K)'
IGRID=1
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XTHVREF,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
!*       1.5    Variables necessary for plots
!
! PABST,THT,POVOM for cross sections at constant pressure 
! level or constant theta level or constant PV level
!
IF (INDEX(CISO,'PR') /= 0) THEN
  YRECFM='PABST'
  YCOMMENT='X_Y_Z_ABSolute Pressure (Pa)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XPABST,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
IF (INDEX(CISO,'TK') /= 0) THEN
  YRECFM='THT'
  YCOMMENT='X_Y_Z_potential temperature (K)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XTHT,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
ZCORIOZ(:,:,:)=SPREAD( XCORIOZ(:,:),DIM=3,NCOPIES=IKU )
ZVOX(:,:,:)=GY_W_VW(1,IKU,1,XWT,XDYY,XDZZ,XDZY)-GZ_V_VW(1,IKU,1,XVT,XDZZ)
ZVOX(:,:,2)=ZVOX(:,:,3)
ZVOY(:,:,:)=GZ_U_UW(1,IKU,1,XUT,XDZZ)-GX_W_UW(1,IKU,1,XWT,XDXX,XDZZ,XDZX)
ZVOY(:,:,2)=ZVOY(:,:,3)
ZVOZ(:,:,:)=GX_V_UV(1,IKU,1,XVT,XDXX,XDZZ,XDZX)-GY_U_UV(1,IKU,1,XUT,XDYY,XDZZ,XDZY)
ZVOZ(:,:,2)=ZVOZ(:,:,3)
ZVOZ(:,:,1)=ZVOZ(:,:,3)
ZWORK31(:,:,:)=GX_M_M(1,IKU,1,XTHT,XDXX,XDZZ,XDZX)
ZWORK32(:,:,:)=GY_M_M(1,IKU,1,XTHT,XDYY,XDZZ,XDZY)
ZWORK33(:,:,:)=GZ_M_M(1,IKU,1,XTHT,XDZZ)
ZPOVO(:,:,:)= ZWORK31(:,:,:)*MZF(1,IKU,1,MYF(ZVOX(:,:,:)))     &
             + ZWORK32(:,:,:)*MZF(1,IKU,1,MXF(ZVOY(:,:,:)))     &
             + ZWORK33(:,:,:)*(MYF(MXF(ZVOZ(:,:,:))) + ZCORIOZ(:,:,:))
ZPOVO(:,:,:)= ZPOVO(:,:,:)*1E6/XRHODREF(:,:,:)
ZPOVO(:,:,1)  =-1.E+11
ZPOVO(:,:,IKU)=-1.E+11
IF (INDEX(CISO,'EV') /= 0) THEN
  YRECFM='POVOT'
  YCOMMENT='X_Y_Z_POtential VOrticity (PVU)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZPOVO,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
!
IF (LVAR_RS) THEN
  YRECFM='UT'
  YCOMMENT='X_Y_Z_U component of wind (m/s)'
  IGRID=2
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XUT,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='VT'
  YCOMMENT='X_Y_Z_V component of wind (m/s)'
  IGRID=3
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XVT,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  IF (LWIND_ZM) THEN
    YCOMMENT='X_Y_Z_Zonal and Meridian components of horizontal wind (M/S)'
    CALL UV_TO_ZONAL_AND_MERID(XUT,XVT,23, &
            HFMFILE=HFMFILE,HRECU='UM_ZM',HRECV='VM_ZM',HCOMMENT=YCOMMENT)
  END IF
  !
  YRECFM='WT'
  YCOMMENT='X_Y_Z_vertical wind (m/s)'
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XWT,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  !   write mixing ratio for water vapor required to plot radio-soundings
  !
  IF (LUSERV) THEN
    YRECFM='RVT' 
    YCOMMENT='X_Y_Z_Vapor mixing Ratio (KG/KG)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XRT(:,:,:,1),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
  END IF
END IF
!
!*   Latitude and Longitude arrays
!
IF (.NOT.LCARTESIAN) THEN
   YRECFM='LAT'
   YCOMMENT='X_Y_latitude (degrees)'
   IGRID=1
   ILENCH=LEN(YCOMMENT)
   CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XLAT,IGRID,ILENCH,YCOMMENT,IRESP)
   !
   YRECFM='LON'
   YCOMMENT='X_Y_longitude (degrees)'
   IGRID=1
   ILENCH=LEN(YCOMMENT)
   CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XLON,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
!
!-------------------------------------------------------------------------------
!
!*       1.6    Other pronostic variables
!
ZTEMP(:,:,:)=XTHT(:,:,:)*(XPABST(:,:,:)/ XP00) **(XRD/XCPD)
!
IF (LVAR_TURB) THEN
  IF (CTURB /= 'NONE') THEN
    YRECFM='TKET'
    YCOMMENT='X_Y_Z_Turbulent Kinetic Energy (M**2/S**2)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XTKET,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    IF( NRR > 1 ) THEN
      YRECFM='SRCT'
      YCOMMENT='X_Y_Z_normalized 2nd_order moment s_r_c/2Sigma_s2 (KG/KG**2)'
      IGRID=1
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XSRCT,IGRID,ILENCH,YCOMMENT,IRESP)
      !
      YRECFM='SIGS'
      YCOMMENT='X_Y_Z_Sigma_s from turbulence scheme (KG/KG**2)'
      IGRID=1
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XSIGS,IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
    ! 
    IF(CTOM=='TM06') THEN
      YRECFM='BL_DEPTH'
      YCOMMENT='X_Y_BL_DEPTH (M)'
      IGRID=1
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XBL_DEPTH,IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
  END IF
END IF
!
!* Rains
!
IF (LVAR_PR .AND. LUSERR .AND. SIZE(XINPRR)>0 ) THEN
  !
  ! explicit species
  !
  ZWORK21(:,:) = XINPRR(:,:)*3.6E6
  YRECFM      ='INPRR'
  YCOMMENT    ='X_Y_INstantaneous PRecipitation Rate (MM/H)'
  IGRID       =1
  ILENCH      =LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  ZWORK31(:,:,:)  = XINPRR3D(:,:,:)
  YRECFM      = 'INPRR3D'
  YCOMMENT    = 'X_Y_INstantaneous 3D Rain Precipitation flux (M/S)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH, &
                                             YCOMMENT,IRESP) ! unit conversion
!
  ZWORK31(:,:,:)  = XEVAP3D(:,:,:)
  YRECFM      = 'EVAP3D'
  YCOMMENT    = 'X_Y_INstantaneous 3D Rain Evaporation flux (KG/KG/S)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH, &
                                             YCOMMENT,IRESP) ! unit conversion
!
  ZWORK21(:,:) = XACPRR(:,:)*1.E3
  YRECFM      ='ACPRR'
  YCOMMENT    ='X_Y_ACcumulated PRecipitation Rate (MM)'
  IGRID       =1
  ILENCH      =LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  IF (CCLOUD(1:3) == 'ICE' .OR. CCLOUD == 'C2R2' .OR. CCLOUD == 'C3R5' .OR.&
      CCLOUD == 'KHKO' .OR. CCLOUD == 'LIMA') THEN 
    IF (SIZE(XINPRC) /= 0 ) THEN
      ZWORK21(:,:) = XINPRC(:,:)*3.6E6
      YRECFM      ='INPRC'
      YCOMMENT    ='X_Y_INstantaneous Cloud PRecipitation Rate (MM/H)'
      IGRID       =1
      ILENCH      =LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  !
      ZWORK21(:,:) = XACPRC(:,:)*1.E3
      YRECFM      ='ACPRC'
      YCOMMENT    ='X_Y_ACcumulated Cloud PRecipitation Rate (MM)'
      IGRID       =1
      ILENCH      =LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
    END IF 
  END IF 
  IF (CCLOUD(1:3) == 'ICE' .OR. CCLOUD == 'C3R5' .OR. CCLOUD == 'LIMA') THEN
    ZWORK21(:,:)  = XINPRS(:,:)*3.6E6
    YRECFM      = 'INPRS'
    YCOMMENT    = 'X_Y_INstantaneaous PRecipitation rate for Snow (MM/H)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  !
    ZWORK21(:,:)  = XACPRS(:,:)*1.0E3
    YRECFM      = 'ACPRS'
    YCOMMENT    = 'X_Y_ACccumuated PRecipitation rate for Snow (MM)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  !
    ZWORK21(:,:)  = XINPRG(:,:)*3.6E6
    YRECFM      = 'INPRG'
    YCOMMENT    = 'X_Y_INstantaneaous PRecipitation rate for Graupel (MM/H)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  !
    ZWORK21(:,:)  = XACPRG(:,:)*1.0E3
    YRECFM      = 'ACPRG'
    YCOMMENT    = 'X_Y_ACccumuated PRecipitation rate for Graupel (MM)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  !
    IF (SIZE(XINPRH) /= 0 ) THEN
      ZWORK21(:,:)  = XINPRH(:,:)*3.6E6
      YRECFM      = 'INPRH'
      YCOMMENT    = 'X_Y_INstantaneaous PRecipitation rate for Hail (MM/H)'
      IGRID       = 1
      ILENCH      = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
    !
      ZWORK21(:,:)  = XACPRH(:,:)*1.0E3
      YRECFM      = 'ACPRH'
      YCOMMENT    = 'X_Y_ACccumuated PRecipitation rate for Hail (MM)'
      IGRID       = 1
      ILENCH      = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
    ENDIF
  !
    ZWORK21(:,:) = (XINPRR(:,:) + XINPRS(:,:) + XINPRG(:,:))*3.6E6
    IF (SIZE(XINPRC) /= 0 ) &     
      ZWORK21(:,:) = ZWORK21(:,:) + XINPRC(:,:)*3.6E6          
    IF (SIZE(XINPRH) /= 0 ) &       
      ZWORK21(:,:) = ZWORK21(:,:) + XINPRH(:,:)*3.6E6
    YRECFM      = 'INPRT'
    YCOMMENT    = 'X_Y_Total INstantaneaous PRecipitation rate (MM/H)'
    YCOMMENT    = 'X_Y_INPRT (MM/H)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  !
    ZWORK21(:,:) = (XACPRR(:,:) + XACPRS(:,:) + XACPRG(:,:))*1.0E3
    IF (SIZE(XINPRC) /= 0 ) &      
      ZWORK21(:,:) = ZWORK21(:,:) + XACPRC(:,:)*1.0E3
    IF (SIZE(XINPRH) /= 0 ) &        
      ZWORK21(:,:) = ZWORK21(:,:) + XACPRH(:,:)*1.0E3
  !
    YRECFM      = 'ACPRT'
    YCOMMENT    = 'X_Y_Total ACcumulated PRecipitation rate (MM)'
    YCOMMENT    = 'X_Y_ACPRT (MM)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  END IF
  !
  !* Convective rain
  !
  IF (CDCONV /= 'NONE') THEN
    ZWORK21(:,:) = XPRCONV(:,:)*3.6E6 
    YRECFM      ='PRCONV'
    YCOMMENT    ='X_Y_CONVective instantaneous Precipitation Rate (MM/H)'
    IGRID       =1
    ILENCH      =LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  !
    ZWORK21(:,:) = XPACCONV(:,:)*1.E3 
    YRECFM      ='PACCONV'
    YCOMMENT    ='X_Y_CONVective ACcumulated Precipitation rate (MM)'
    IGRID       =1
    ILENCH      =LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  !
    ZWORK21(:,:) = XPRSCONV(:,:)*3.6E6 
    YRECFM      ='PRSCONV'
    YCOMMENT    ='X_Y_CONVective instantaneous Precipitation Rate for Snow (MM/H)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
END IF
IF (LVAR_PR ) THEN
  !Precipitable water in kg/m**2 
  ZWORK21(:,:) = 0.
  ZWORK22(:,:) = 0.
  ZWORK23(:,:) = 0.
  ZWORK31(:,:,:) = DZF(1,IKU,1,XZZ(:,:,:))
  DO JK = IKB,IKE
    !* Calcul de qtot
    IF  (CCLOUD(1:3) == 'ICE' .OR. CCLOUD == 'LIMA') THEN
      ZWORK23(IIB:IIE,IJB:IJE) = XRT(IIB:IIE,IJB:IJE,JK,1) + &
      XRT(IIB:IIE,IJB:IJE,JK,2) + XRT(IIB:IIE,IJB:IJE,JK,3) + &
      XRT(IIB:IIE,IJB:IJE,JK,4) + XRT(IIB:IIE,IJB:IJE,JK,5) + &
      XRT(IIB:IIE,IJB:IJE,JK,6)
    ELSE
      ZWORK23(IIB:IIE,IJB:IJE) = XRT(IIB:IIE,IJB:IJE,JK,1)
    ENDIF
    !* Calcul de l'eau precipitable
    ZWORK21(IIB:IIE,IJB:IJE)=XRHODREF(IIB:IIE,IJB:IJE,JK)* &
    ZWORK23(IIB:IIE,IJB:IJE)* ZWORK31(IIB:IIE,IJB:IJE,JK)
    !* Sum 
    ZWORK22(IIB:IIE,IJB:IJE) = ZWORK22(IIB:IIE,IJB:IJE)+ZWORK21(IIB:IIE,IJB:IJE)
    ZWORK21(:,:) = 0.
    ZWORK23(:,:) = 0.
  END DO
  !* Precipitable water in kg/m**2
  YRECFM='PRECIP_WAT'
  YCOMMENT='(kg/m²)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)  
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK22,IGRID,ILENCH,YCOMMENT,IRESP)
ENDIF
!
!
!* Flux d'humidité et d'hydrométéores
IF (LHU_FLX) THEN
  ZWORK35(:,:,:) = XRHODREF(:,:,:) * XRT(:,:,:,1)
  ZWORK31(:,:,:) = MXM(ZWORK35(:,:,:)) * XUT(:,:,:)
  ZWORK32(:,:,:) = MYM(ZWORK35(:,:,:)) * XVT(:,:,:)
  IF  (CCLOUD(1:3) == 'ICE' .OR. CCLOUD == 'LIMA') THEN
    ZWORK36(:,:,:) = ZWORK35(:,:,:) + XRHODREF(:,:,:) * (XRT(:,:,:,2) + &
    XRT(:,:,:,3) + XRT(:,:,:,4) + XRT(:,:,:,5) + XRT(:,:,:,6))
    ZWORK33(:,:,:) = MXM(ZWORK36(:,:,:)) * XUT(:,:,:)
    ZWORK34(:,:,:) = MYM(ZWORK36(:,:,:)) * XVT(:,:,:)
  ENDIF
  ZWORK35(:,:,:) = GX_U_M(1,IKU,1,ZWORK31,XDXX,XDZZ,XDZX) + GY_V_M(1,IKU,1,ZWORK32,XDYY,XDZZ,XDZY)
  ZWORK36(:,:,:) = GX_U_M(1,IKU,1,ZWORK33,XDXX,XDZZ,XDZX) + GY_V_M(1,IKU,1,ZWORK34,XDYY,XDZZ,XDZY)
  !
  ! Integration sur 3000 m
  !
  IKTOP(:,:)=0
  DO JK=1,IKU-1
    WHERE (((XZZ(:,:,JK) -XZS(:,:))<= 3000.0) .AND. ((XZZ(:,:,JK+1) -XZS(:,:))> 3000.0))
      IKTOP(:,:)=JK
    END WHERE
  END DO
  ZDELTAZ(:,:,:)=DZF(1,IKU,1,XZZ) 
  ZWORK21(:,:) = 0.
  ZWORK22(:,:) = 0.
  ZWORK25(:,:) = 0.  
  DO JJ=1,IJU
    DO JI=1,IIU
      IAUX=IKTOP(JI,JJ)
      DO JK=IKB,IAUX-1 
        ZWORK21(JI,JJ) = ZWORK21(JI,JJ) + ZWORK31(JI,JJ,JK) * ZDELTAZ(JI,JJ,JK)
        ZWORK22(JI,JJ) = ZWORK22(JI,JJ) + ZWORK32(JI,JJ,JK) * ZDELTAZ(JI,JJ,JK)
        ZWORK25(JI,JJ) = ZWORK25(JI,JJ) + ZWORK35(JI,JJ,JK) * ZDELTAZ(JI,JJ,JK)
      ENDDO
      IF (IAUX >= IKB) THEN
        ZDELTAZ(JI,JJ,IAUX)= 3000. - (XZZ(JI,JJ,IAUX) -XZS(JI,JJ))
        ZWORK21(JI,JJ) = ZWORK21(JI,JJ) + ZWORK31(JI,JJ,IAUX) * ZDELTAZ(JI,JJ,IAUX) 
        ZWORK22(JI,JJ) = ZWORK22(JI,JJ) + ZWORK32(JI,JJ,IAUX) * ZDELTAZ(JI,JJ,IAUX)
        ZWORK25(JI,JJ) = ZWORK25(JI,JJ) + ZWORK35(JI,JJ,IAUX) * ZDELTAZ(JI,JJ,IAUX)
      ENDIF
    ENDDO
  ENDDO
  IF  (CCLOUD(1:3) == 'ICE' .OR. CCLOUD == 'LIMA') THEN
    ZWORK23(:,:) = 0.
    ZWORK24(:,:) = 0.
    ZWORK26(:,:) = 0.
    DO JJ=1,IJU
      DO JI=1,IIU
        IAUX=IKTOP(JI,JJ)
        DO JK=IKB,IAUX-1 
          ZWORK23(JI,JJ) = ZWORK23(JI,JJ) + ZWORK33(JI,JJ,JK) * ZDELTAZ(JI,JJ,JK)
          ZWORK24(JI,JJ) = ZWORK24(JI,JJ) + ZWORK34(JI,JJ,JK) * ZDELTAZ(JI,JJ,JK)
          ZWORK26(JI,JJ) = ZWORK26(JI,JJ) + ZWORK36(JI,JJ,JK) * ZDELTAZ(JI,JJ,JK)
        ENDDO
        IF (IAUX >= IKB) THEN
          ZDELTAZ(JI,JJ,IAUX)= 3000. - (XZZ(JI,JJ,IAUX) -XZS(JI,JJ))
          ZWORK23(JI,JJ) = ZWORK23(JI,JJ) + ZWORK33(JI,JJ,IAUX) * ZDELTAZ(JI,JJ,IAUX) 
          ZWORK24(JI,JJ) = ZWORK24(JI,JJ) + ZWORK34(JI,JJ,IAUX) * ZDELTAZ(JI,JJ,IAUX)
          ZWORK26(JI,JJ) = ZWORK26(JI,JJ) + ZWORK36(JI,JJ,IAUX) * ZDELTAZ(JI,JJ,IAUX)
        ENDIF
      ENDDO
    ENDDO
  ENDIF
  ! Ecriture
  !  composantes U et V du flux surfacique d'humidité  
  YRECFM='UM90'
  YCOMMENT='(kg / s / m²)'
  IGRID=2
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
  !  
  YRECFM='VM90'
  YCOMMENT='(kg / s / m²)'
  IGRID=3
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK32,IGRID,ILENCH,YCOMMENT,IRESP)
  !  composantes U et V du flux d'humidité intégré sur 3000 metres
  YRECFM='UM91'
  YCOMMENT='(kg / s / m)'
  IGRID=2
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='VM91'
  YCOMMENT='(kg / s / m)'
  IGRID=3
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK22,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  !   Convergence d'humidité
  YRECFM='HMCONV'
  YCOMMENT='X_Y_Horizontal CONVergence of moisture flux (kg / s / m)'
  YCOMMENT='(kg / s / m^3)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK35*(-1),IGRID,ILENCH,YCOMMENT,IRESP)
  !
  !   Convergence d'humidité intégré sur 3000 mètres
  YRECFM='HMCONV3000'
  YCOMMENT='X_Y_Horizontal CONVergence of moisture flux (kg / s / m)'
  YCOMMENT='(kg / s / m^3)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK25*(-1),IGRID,ILENCH,YCOMMENT,IRESP)
  !
  IF  (CCLOUD(1:3) == 'ICE' .OR. CCLOUD == 'LIMA') THEN
    !  composantes U et V du flux surfacique d'hydrométéores  
    YRECFM='UM92'
    YCOMMENT='(kg / s / m²)'
    IGRID=2
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK33,IGRID,ILENCH,YCOMMENT,IRESP)
    ! 
    YRECFM='VM92'
    YCOMMENT='(kg / s / m²)'
    IGRID=3
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK34,IGRID,ILENCH,YCOMMENT,IRESP)
    !  composantes U et V du flux d'hydrométéores intégré sur 3000 metres
    YRECFM='UM93'
    YCOMMENT='(kg / s / m)'
    IGRID=2
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK23,IGRID,ILENCH,YCOMMENT,IRESP)
    YRECFM='VM93'
    YCOMMENT='(kg / s / m)'
    IGRID=3
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK24,IGRID,ILENCH,YCOMMENT,IRESP)
    !   Convergence d'hydrométéores
    YRECFM='HMCONV_TT'
    YCOMMENT='X_Y_Horizontal CONVergence of hydrometeor flux (kg / s / m)'
    YCOMMENT='(kg / s / m^3)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK36*(-1),IGRID,ILENCH,YCOMMENT,IRESP)
    !   Convergence d'hydrométéores intégré sur 3000 mètres
    YRECFM='HMCONV3000_TT'
    YCOMMENT='X_Y_Horizontal CONVergence of hydrometeor flux (kg / s / m)'
    YCOMMENT='(kg / s / m^3)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK26*(-1),IGRID,ILENCH,YCOMMENT,IRESP)
  ENDIF
ENDIF
!
!* Moist variables
!
IF (LVAR_MRW .OR. LLIMA_DIAG) THEN
  IF (NRR >=1) THEN
    IRR=0                                      ! Moist variables are written
    IGRID=1                                    ! individually in file
    IF (LUSERV) THEN
      IRR=IRR+1
      YRECFM= 'MRV'
      YCOMMENT='X_Y_Z_MRV (G/KG)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=XRT(:,:,:,IRR)*1.E3
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,  &
                  YCOMMENT,IRESP)
                 
    END IF
    IF (LUSERC) THEN
      IRR=IRR+1
      YRECFM= 'MRC'
      YCOMMENT='X_Y_Z_MRC (G/KG)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=XRT(:,:,:,IRR)*1.E3
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,  &
                  YCOMMENT,IRESP)
      YRECFM= 'VRC'
      YCOMMENT='X_Y_Z_VRC (vol/vol)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=XRT(:,:,:,IRR)*XRHODREF(:,:,:)/1.E3
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,  &
                  YCOMMENT,IRESP)
    END IF
    IF (LUSERR) THEN
      IRR=IRR+1
      YRECFM= 'MRR'
      YCOMMENT='X_Y_Z_MRR (G/KG)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=XRT(:,:,:,IRR)*1.E3
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,  &
                  YCOMMENT,IRESP)
      YRECFM= 'VRR'
      YCOMMENT='X_Y_Z_VRR (vol/vol)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=XRT(:,:,:,IRR)*XRHODREF(:,:,:)/1.E3
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,  &
                  YCOMMENT,IRESP)
    END IF
    IF (LUSERI) THEN
      IRR   = IRR+1
      YRECFM= 'MRI'
      YCOMMENT='X_Y_Z_MRI (G/KG)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=XRT(:,:,:,IRR)*1.E3
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,  &
                  YCOMMENT,IRESP)
      IF (LUSECI) THEN
        YRECFM= 'CIT'
        YCOMMENT='X_Y_Z_CIT (/M3)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XCIT(:,:,:),IGRID,ILENCH,  &
                    YCOMMENT,IRESP)
      END IF
    END IF
    IF (LUSERS) THEN
      IRR   = IRR+1
      YRECFM= 'MRS'
      YCOMMENT='X_Y_Z_MRS (G/KG)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=XRT(:,:,:,IRR)*1.E3
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,  &
                  YCOMMENT,IRESP)
    END IF
    IF (LUSERG) THEN
      IRR   = IRR+1
      YRECFM= 'MRG'
      YCOMMENT='X_Y_Z_RG (G/KG)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=XRT(:,:,:,IRR)*1.E3
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,  &
                  YCOMMENT,IRESP)
    END IF
    IF (LUSERH) THEN
      IRR   = IRR+1
      YRECFM= 'MRH'
      YCOMMENT='X_Y_Z_RH (G/KG)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=XRT(:,:,:,IRR)*1.E3
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    END IF
  END IF
END IF

!
!* Scalar Variables
!
! User scalar variables
IGRID=1                                     ! individually in the file
IF (LVAR_MRSV) THEN
  DO JSV = 1,NSV_USER
    WRITE(YRECFM,'(A4,I3.3)')'MRSV',JSV
    WRITE(YCOMMENT,'(A6,A4,I3.3,A7)')'X_Y_Z_','MRSV',JSV,' (G/KG)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=XSVT(:,:,:,JSV)*1.E3
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
         YCOMMENT,IRESP)
  END DO
END IF
! microphysical C2R2 scheme scalar variables
IF(LVAR_MRW) THEN
  DO JSV = NSV_C2R2BEG,NSV_C2R2END
    YRECFM=TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))//'T'
    IF (JSV < NSV_C2R2END) THEN
      WRITE(YCOMMENT,'(A6,A4,I3.3,A7)')'X_Y_Z_','MRSV',JSV,' (/CM3)'
    ZWORK31(:,:,:)=XSVT(:,:,:,JSV)*1.E-6
    ELSE
      WRITE(YCOMMENT,'(A6,A4,I3.3,A7)')'X_Y_Z_','MRSV',JSV,' (/L)'
      ZWORK31(:,:,:)=XSVT(:,:,:,JSV)*1.E-3
    ENDIF
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
         YCOMMENT,IRESP)
  END DO
  ! microphysical C3R5 scheme additional scalar variables
  DO JSV = NSV_C1R3BEG,NSV_C1R3END
    YRECFM=TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))//'T'
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (/L)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=XSVT(:,:,:,JSV)*1.E-3
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
         YCOMMENT,IRESP)   
  END DO
END IF
!
! microphysical LIMA scheme scalar variables
!
IF (LLIMA_DIAG) THEN
   DO JSV = NSV_LIMA_BEG,NSV_LIMA_END
! Nc
      IF (JSV .EQ. NSV_LIMA_NC) THEN
         YRECFM=TRIM(CLIMA_WARM_CONC(1))//'T'
         WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (/cm^3)'
      END IF
! Nr
      IF (JSV .EQ. NSV_LIMA_NR) THEN
         YRECFM=TRIM(CLIMA_WARM_CONC(2))//'T'
         WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (/cm^3)'
      END IF
! N CCN free
      IF (JSV .GE. NSV_LIMA_CCN_FREE .AND. JSV .LT. NSV_LIMA_CCN_ACTI) THEN
         WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_CCN_FREE + 1)
         YRECFM=TRIM(CLIMA_WARM_CONC(3))//INDICE//'T'
         WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (/cm^3)'
      END IF
! N CCN acti
      IF (JSV .GE. NSV_LIMA_CCN_ACTI .AND. JSV .LT. NSV_LIMA_CCN_ACTI + NMOD_CCN) THEN
         WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_CCN_ACTI + 1)
         YRECFM=TRIM(CLIMA_WARM_CONC(4))//INDICE//'T'
         WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (/cm^3)'
      END IF
! Scavenging
      IF (JSV .EQ. NSV_LIMA_SCAVMASS) THEN
         YRECFM=TRIM(CAERO_MASS(1))//'T'
         WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (kg/cm^3)'
      END IF
! Ni
      IF (JSV .EQ. NSV_LIMA_NI) THEN
         YRECFM=TRIM(CLIMA_COLD_CONC(1))//'T'
         WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (/cm^3)'
      END IF
! N IFN free
      IF (JSV .GE. NSV_LIMA_IFN_FREE .AND. JSV .LT. NSV_LIMA_IFN_NUCL) THEN
         WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_IFN_FREE + 1)
         YRECFM=TRIM(CLIMA_COLD_CONC(2))//INDICE//'T'
         WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (/cm^3)'
      END IF
! N IFN nucl
      IF (JSV .GE. NSV_LIMA_IFN_NUCL .AND. JSV .LT. NSV_LIMA_IFN_NUCL + NMOD_IFN) THEN
         WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_IFN_NUCL + 1)
         YRECFM=TRIM(CLIMA_COLD_CONC(3))//INDICE//'T'
         WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (/cm^3)'
      END IF
! N IMM nucl
      I = 0
      IF (JSV .GE. NSV_LIMA_IMM_NUCL .AND. JSV .LT. NSV_LIMA_IMM_NUCL + NMOD_IMM) THEN
         I = I + 1
         WRITE(INDICE,'(I2.2)')(NINDICE_CCN_IMM(I))
         YRECFM=TRIM(CLIMA_COLD_CONC(4))//INDICE//'T'
         WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (/cm^3)'
      END IF
! Hom. freez. of CCN
      IF (JSV .EQ. NSV_LIMA_HOM_HAZE) THEN
         YRECFM=TRIM(CLIMA_COLD_CONC(5))//'T'
         WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (/cm^3)'
      END IF
!
!
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=XSVT(:,:,:,JSV)*1.E-6*XRHODREF(:,:,:)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                  YCOMMENT,IRESP)
   END DO
!
   IF (LUSERC) THEN
      YRECFM= 'LWC'
      YCOMMENT='X_Y_Z_LWC (g/m^3)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=XRT(:,:,:,2)*1.E3*XRHODREF(:,:,:)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,  &
                  YCOMMENT,IRESP)
   END IF
!
   IF (LUSERI) THEN
      YRECFM= 'IWC'
      YCOMMENT='X_Y_Z_MRI (g/m^3)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=XRT(:,:,:,4)*1.E3*XRHODREF(:,:,:)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,  &
                  YCOMMENT,IRESP)
   END IF
!
END IF
!
! chemical scalar variables in gas phase PPBV
IF (LCHEMDIAG) THEN
  DO JSV = NSV_CHGSBEG,NSV_CHGSEND
    YRECFM=TRIM(UPCASE(CNAMES(JSV-NSV_CHGSBEG+1)))//'T'
    WRITE(YCOMMENT,'(A6,A4,I3.3,A7)')'X_Y_Z_','CHIM',JSV,' (ppbv)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=XSVT(:,:,:,JSV)*1.E9
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
         YCOMMENT,IRESP)
  END DO
END IF
IF (LCHAQDIAG) THEN    !aqueous concentration in M
  ZWORK31(:,:,:)=0.
  DO JSV = NSV_CHACBEG, NSV_CHACBEG-1+NEQAQ/2   !cloud water
    YRECFM=TRIM(CNAMES(JSV-NSV_CHACBEG+NSV_CHGS+1))//'M'
    WRITE(YCOMMENT,'(A6,A4,I3.3,A4)')'X_Y_Z_','CHAQ',JSV,' (M)'
    ILENCH=LEN(YCOMMENT)
    WHERE(((XRT(:,:,:,2)*XRHODREF(:,:,:))/1.e3) .GE. XRTMIN_AQ)
      ZWORK31(:,:,:)=(XSVT(:,:,:,JSV)*1000.)/(XMD*1.E+3*XRT(:,:,:,2))
    ENDWHERE
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
         YCOMMENT,IRESP)
  END DO
  ZWORK31(:,:,:)=0.
  DO JSV = NSV_CHACBEG+NEQAQ/2, NSV_CHACEND    !rain water
    YRECFM=TRIM(CNAMES(JSV-NSV_CHACBEG+NSV_CHGS+1))//'M'
    WRITE(YCOMMENT,'(A6,A4,I3.3,A4)')'X_Y_Z_','CHAQ',JSV,' (M)'
    ILENCH=LEN(YCOMMENT)
    WHERE(((XRT(:,:,:,3)*XRHODREF(:,:,:))/1.e3) .GE. XRTMIN_AQ)
      ZWORK31(:,:,:)=(XSVT(:,:,:,JSV)*1000.)/(XMD*1.E+3*XRT(:,:,:,3))
    ENDWHERE
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
         YCOMMENT,IRESP)
  END DO
!  ZWORK31(:,:,:)=0.
!  DO JSV = NSV_CHICBEG,NSV_CHICEND   ! ice phase
!    YRECFM=TRIM(CICNAMES(JSV-NSV_CHICBEG+1))//'T'
!    WRITE(YCOMMENT,'(A6,A4,I3.3,A4)')'X_Y_Z_','CHIC',JSV,' (M)'
!    ILENCH=LEN(YCOMMENT)
!    WHERE(((XRT(:,:,:,3)*XRHODREF(:,:,:))/1.e3) .GE. XRTMIN_AQ)
!      ZWORK31(:,:,:)=(XSVT(:,:,:,JSV)*1000.)/(XMD*1.E+3*XRT(:,:,:,3))
!    ENDWHERE
!    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
!         YCOMMENT,IRESP)
!  END DO
END IF

! Passive polluant scalar variables
IF (LPASPOL) THEN
  ALLOCATE(ZRHOT( SIZE(XTHT,1), SIZE(XTHT,2),SIZE(XTHT,3)))
  ALLOCATE(ZTMP( SIZE(XTHT,1), SIZE(XTHT,2),SIZE(XTHT,3)))
!
!*	Density                                          
!
  ZRHOT(:,:,:)=XPABST(:,:,:)/(XRD*XTHT(:,:,:)*((XPABST(:,:,:)/XP00)**(XRD/XCPD)))
!
!*	Conversion g/m3.
!
  ZRHOT(:,:,:)=ZRHOT(:,:,:)*1000.0

  DO JSV = 1,NSV_PP
    ZTMP(:,:,:)=ABS( XSVT(:,:,:,JSV+NSV_PPBEG-1)*ZRHOT(:,:,:) )
    WRITE(YRECFM,'(A3,I3.3)')'PPT',JSV
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','PPT',JSV,' (G/M3) '
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZTMP,IGRID,ILENCH,       &
     YCOMMENT,IRESP)
    !
  END DO
  DEALLOCATE(ZTMP)
  DEALLOCATE(ZRHOT)
END IF
! Conditional sampling variables
IF (LCONDSAMP) THEN
  DO JSV = NSV_CSBEG,NSV_CSEND
    WRITE(YRECFM,'(A3,I3.3)')'CST',JSV
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','CST',JSV,' () '
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XSVT(:,:,:,JSV),IGRID,ILENCH,       &
     YCOMMENT,IRESP)
    !
  END DO
END IF


! Lagrangian variables
IF (LTRAJ) THEN
  DO JSV = NSV_LGBEG,NSV_LGEND
    YRECFM=TRIM(CLGNAMES(JSV-NSV_LGBEG+1))//'T'
    WRITE(YCOMMENT,'(A6,A20,I3.3,A7)')'X_Y_Z_','Lagrangian variable ',JSV,' (M)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XSVT(:,:,:,JSV),IGRID,ILENCH,   &
                YCOMMENT,IRESP)
  END DO
  ! X coordinate
  DO JK=1,IKU
    DO JJ=1,IJU
      DO JI=1,IIU-1
       ZWORK31(JI,JJ,JK)=0.5*(XXHAT(JI)+XXHAT(JI+1))
      END DO
      ZWORK31(IIU,JJ,JK)=2.*ZWORK31(IIU-1,JJ,JK) - ZWORK31(IIU-2,JJ,JK)
    END DO
  END DO
  WRITE(YRECFM,'(A1)')'X'
  WRITE(YCOMMENT,'(A6,A13,A7)')'X_Y_Z_','X coordinate ',' (M)'
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
              YCOMMENT,IRESP)
  ! Y coordinate
  DO JK=1,IKU
    DO JI=1,IIU
      DO JJ=1,IJU-1
        ZWORK31(JI,JJ,JK)=0.5*(XYHAT(JJ)+XYHAT(JJ+1))
      END DO
      ZWORK31(JI,IJU,JK)=2.*ZWORK31(JI,IJU-1,JK) - ZWORK31(JI,IJU-2,JK)
    END DO
  END DO
  WRITE(YRECFM,'(A1)')'Y'
  WRITE(YCOMMENT,'(A6,A13,A7)')'X_Y_Z_','Y coordinate ',' (M)'
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
              YCOMMENT,IRESP)
END IF
! linox scalar variables
IF (.NOT.(LUSECHEM .OR. LCHEMDIAG) .AND. LCH_CONV_LINOX) THEN
  DO JSV = NSV_LNOXBEG,NSV_LNOXEND
    YRECFM='LINOXT'
    WRITE(YCOMMENT,'(A6,A4,I3.3,A7)')'X_Y_Z_','LNOX',JSV,' (ppbv)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=XSVT(:,:,:,JSV)*1.E9
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
         YCOMMENT,IRESP)
  END DO
END IF
IF (LELECDIAG .AND. CELEC .NE. "NONE") THEN
  DO JSV = NSV_ELECBEG,NSV_ELECEND
    YRECFM=TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))//'T'
    IF (JSV .GT. NSV_ELECBEG .AND. JSV .LT. NSV_ELECEND) THEN
      WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (C/m3)'
    ELSE
      WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (nb ions/m3)'
    END IF
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=XSVT(:,:,:,JSV) * XRHODREF(:,:,:)  ! C/kg --> C/m3
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
         YCOMMENT,IRESP)
  END DO
END IF
! Sea Salt variables
IF (LSALT) THEN
  IF(.NOT.ALLOCATED(ZSIG_SLT)) &
    ALLOCATE(ZSIG_SLT(SIZE(XSVT,1), SIZE(XSVT,2), SIZE(XSVT,3), NMODE_SLT))
  IF(.NOT.ALLOCATED(ZRG_SLT))  &
    ALLOCATE(ZRG_SLT(SIZE(XSVT,1), SIZE(XSVT,2), SIZE(XSVT,3), NMODE_SLT))
  IF(.NOT.ALLOCATED(ZN0_SLT))  &
    ALLOCATE(ZN0_SLT(SIZE(XSVT,1), SIZE(XSVT,2), SIZE(XSVT,3), NMODE_SLT))
  !
  DO JSV = NSV_SLTBEG,NSV_SLTEND
    YRECFM=TRIM(UPCASE(CSALTNAMES(JSV-NSV_SLTBEG+1)))//'T'
    WRITE(YCOMMENT,'(A6,A4,I3.3,A7)')'X_Y_Z_','SALT',JSV,' (ppbv)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=XSVT(:,:,:,JSV)*1.E9
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  END DO
  !
  CALL PPP2SALT(XSVT(:,:,:,NSV_SLTBEG:NSV_SLTEND),XRHODREF,&
               PSIG3D=ZSIG_SLT, PRG3D=ZRG_SLT, PN3D=ZN0_SLT)
  DO JJ=1,NMODE_SLT
    WRITE(YRECFM,'(A6,I1)')'SLTRGA',JJ
    WRITE(YCOMMENT,'(A18,I1,A5)')'RG (nb) SALT MODE ',JJ,' (um)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=ZRG_SLT(:,:,:,JJ)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !
    WRITE(YRECFM,'(A7,I1)')'SLTRGAM',JJ
    WRITE(YCOMMENT,'(A17,I1,A5)')'RG (m) SALT MODE ',JJ,' (um)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=ZRG_SLT(:,:,:,JJ) / (EXP(-3.*(LOG(ZSIG_SLT(:,:,:,JJ)))**2))
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !
    WRITE(YRECFM,'(A6,I1)')'SLTN0A',JJ
    WRITE(YCOMMENT,'(A13,I1,A7)')'N0 SALT MODE ',JJ,' (1/m3)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=ZN0_SLT(:,:,:,JJ)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !
    WRITE(YRECFM,'(A7,I1)')'SLTSIGA',JJ
    WRITE(YCOMMENT,'(A16,I1)')'SIGMA SALT MODE ',JJ
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=ZSIG_SLT(:,:,:,JJ)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !SALT MASS CONCENTRATION
    WRITE(YRECFM,'(A4,I1)')'SLTMSS',JJ
    WRITE(YCOMMENT,'(A14,I1,A7)')'MASSCONC MODE ',JJ,'(ug/m3)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)= ZN0_SLT(:,:,:,JJ)*4./3.*3.14*2500.*1e9 & !kg-->ug
       * (ZRG_SLT(:,:,:,JJ)**3)*1.d-18 &  !um-->m
       * exp(4.5*log(ZSIG_SLT(:,:,:,JJ))*log(ZSIG_SLT(:,:,:,JJ)))
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !SALT BURDEN (g/m2)
    ZWORK21(:,:)=0.0
    DO JK=IKB,IKE
      ZWORK31(:,:,JK) = ZWORK31(:,:,JK) *(XZZ(:,:,JK+1)-XZZ(:,:,JK))      &
                       *1.d-6 ! Convert to ug/m2-->g/m2 in each layer
    END DO
    DO JK=IKB,IKE
      DO JT=IJB,IJE
        DO JI=IIB,IIE
           ZWORK21(JI,JT)=ZWORK21(JI,JT)+ZWORK31(JI,JT,JK)
        ENDDO
      ENDDO
    ENDDO
    WRITE(YRECFM,'(A7,I1)')'SLTBRDN',JJ
    WRITE(YCOMMENT,'(A13,I1)')'BURDEN (g/m2)',JJ
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  ENDDO
END IF
IF (LSALT.AND.LDEPOS_SLT(IMI)) THEN
    ! 
     ZSSLTDEP=XSVT(:,:,:,NSV_SLTDEPBEG:NSV_SLTDEPEND)
     DO JSV = 1,NSV_SLTDEP   
      YRECFM=TRIM(UPCASE(CDESLTNAMES(JSV)))//'T'
       WRITE(YCOMMENT,'(A6,A4,I3.3,A7)')'X_Y_Z_','SALTDEP',JSV,' (ppbv)'
       ILENCH=LEN(YCOMMENT)
       ZWORK31(:,:,:)=ZSSLTDEP(:,:,:,JSV)*1.E9
       CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
     END DO
     DO JJ=1,NMODE_SLT           
            ZWORK31(:,:,:)=0.0
! FOR CLOUDS
        WRITE(YRECFM,'(A9,I1)')'SLTDEPN0A',JJ
        WRITE(YCOMMENT,'(A16,I1,A7)')'N0 DUSTDEP MODE ', &
                                   JJ,' (1/m3)'            
! CLOUD: CALCULATE MOMENT 3 FROM TOTAL AEROSOL MASS            
            ZWORK31(:,:,:)=ZSSLTDEP(:,:,:,JJ)  &!==>molec_{aer}/molec_{air}
            *(XMOLARWEIGHT_DUST/XMD)           &!==>kg_{aer}/kg_{air}
            *XRHODREF(:,:,:)                   &!==>kg_{aer}/m3_{air}
            *(1.d0/XDENSITY_DUST)              &!==>m3_{aer}/m3_{air}
            *XM3TOUM3                          &!==>um3_{aer}/m3_{air}
            /(XPI*4./3.)                        !==>um3_{aer}/m3_{air}
            !==>volume 3rd moment
!CLOUD: CALCULATE MOMENT 0 FROM DISPERSION AND MEAN RADIUS            
            ZWORK31(:,:,:)=  ZWORK31(:,:,:)/      &
                    ((ZRG_SLT(:,:,:,JJ)**3)*      &
                    EXP(4.5 * LOG(ZSIG_SLT(:,:,:,JJ))**2))
!CLOUD: RETURN TO CONCENTRATION #/m3
            ZWORK31(:,:,:)= ZWORK31(:,:,:) *   XMD/ &
                     (XAVOGADRO*XRHODREF(:,:,:))
!CLOUD:  Get number concentration (#/molec_{air}==>#/m3)  
             ZWORK31(:,:,:)=                         &
                    ZWORK31(:,:,:)                  & !#/molec_{air}
                    * XAVOGADRO                     & !==>#/mole
                    / XMD                           & !==>#/kg_{air}
                    * XRHODREF(:,:,:)                 !==>#/m3  
     ILENCH=LEN(YCOMMENT)               
     CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
! CLOUD:   DUST MASS CONCENTRATION
       WRITE(YRECFM,'(A9,I1)')'SLTDEPMSS',JJ
       WRITE(YCOMMENT,'(A17,I1,A7)')'DEPMASSCONC MODE ', &
                                 JJ,'(ug/m3)'
       ILENCH=LEN(YCOMMENT)
       ZWORK31(:,:,:)= ZWORK31(:,:,:)*4./3.*3.14*2500.*1e9 & !kg-->ug
          * (ZRG_SLT(:,:,:,JJ)**3)*1.d-18 &  !um-->m
          * exp(4.5*log(ZSIG_SLT(:,:,:,JJ))*log(ZSIG_SLT(:,:,:,JJ)))
       CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)                
!   FOR RAIN DROPS                
        WRITE(YRECFM,'(A9,I1)')'SLTDEPN0A',JJ+NMODE_SLT
        WRITE(YCOMMENT,'(A16,I1,A7)')'N0 DUSTDEP MODE ', &
                                   JJ+NMODE_SLT,' (1/m3)'
        ILENCH=LEN(YCOMMENT) 
        ZWORK31(:,:,:)=0.0
! RAIN: CALCULATE MOMENT 3 FROM TOTAL AEROSOL MASS                
        ZWORK31(:,:,:)=ZSSLTDEP(:,:,:,JJ+NMODE_SLT)  &!==>molec_{aer}/molec_{air}
            *(XMOLARWEIGHT_DUST/XMD)           &!==>kg_{aer}/kg_{air}
            *XRHODREF(:,:,:)                   &!==>kg_{aer}/m3_{air}
            *(1.d0/XDENSITY_DUST)              &!==>m3_{aer}/m3_{air}
            *XM3TOUM3                          &!==>um3_{aer}/m3_{air}
            /(XPI*4./3.)                        !==>um3_{aer}/m3_{air}
            !==>volume 3rd moment   
!RAIN: CALCULATE MOMENT 0 FROM DISPERSION AND MEAN RADIUS      
     ZWORK31(:,:,:)= ZWORK31(:,:,:)/ &
             ((ZRG_SLT(:,:,:,JJ)**3)*       &
              EXP(4.5 * LOG(ZSIG_SLT(:,:,:,JJ))**2))
!RAIN: RETURN TO CONCENTRATION #/m3       
      ZWORK31(:,:,:)= ZWORK31(:,:,:) *   XMD/ &
                (XAVOGADRO*XRHODREF(:,:,:))
!RAIN: Get number concentration (#/molec_{air}==>#/m3)  
             ZWORK31(:,:,:)=                         &
             ZWORK31(:,:,:)                  & !#/molec_{air}
              * XAVOGADRO                           & !==>#/mole
              / XMD                                 & !==>#/kg_{air}
              * XRHODREF(:,:,:)                       !==>#/m3
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
! RAIN:   DUST MASS CONCENTRATION
       WRITE(YRECFM,'(A9,I1)')'SLTDEPMSS',JJ+NMODE_SLT
       WRITE(YCOMMENT,'(A17,I1,A7)')'DEPMASSCONC MODE ', &
                                 JJ+NMODE_SLT,'(ug/m3)'
       ILENCH=LEN(YCOMMENT)
       ZWORK31(:,:,:)= ZWORK31(:,:,:)*4./3.*3.14*2500.*1e9 & !kg-->ug
          * (ZRG_SLT(:,:,:,JJ)**3)*1.d-18 &  !um-->m
          * exp(4.5*log(ZSIG_SLT(:,:,:,JJ))*log(ZSIG_SLT(:,:,:,JJ)))
       CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
     END DO       
!
 END IF
! Dust variables
IF (LDUST) THEN
  IF(.NOT.ALLOCATED(ZSIG_DST)) &
    ALLOCATE(ZSIG_DST(SIZE(XSVT,1), SIZE(XSVT,2), SIZE(XSVT,3), NMODE_DST))
  IF(.NOT.ALLOCATED(ZRG_DST))  &
    ALLOCATE(ZRG_DST(SIZE(XSVT,1), SIZE(XSVT,2), SIZE(XSVT,3), NMODE_DST))
  IF(.NOT.ALLOCATED(ZN0_DST))  &
    ALLOCATE(ZN0_DST(SIZE(XSVT,1), SIZE(XSVT,2), SIZE(XSVT,3), NMODE_DST))
  !
  DO JSV = NSV_DSTBEG,NSV_DSTEND
    YRECFM=TRIM(UPCASE(CDUSTNAMES(JSV-NSV_DSTBEG+1)))//'T'
    WRITE(YCOMMENT,'(A6,A4,I3.3,A7)')'X_Y_Z_','DUST',JSV,' (ppbv)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=XSVT(:,:,:,JSV)*1.E9
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  END DO
  !
  CALL PPP2DUST(XSVT(:,:,:,NSV_DSTBEG:NSV_DSTEND),XRHODREF,&
               PSIG3D=ZSIG_DST, PRG3D=ZRG_DST, PN3D=ZN0_DST)
  DO JJ=1,NMODE_DST
    WRITE(YRECFM,'(A6,I1)')'DSTRGA',JJ
    WRITE(YCOMMENT,'(A18,I1,A5)')'RG (nb) DUST MODE ',JJ,' (um)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=ZRG_DST(:,:,:,JJ)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !
    WRITE(YRECFM,'(A7,I1)')'DSTRGAM',JJ
    WRITE(YCOMMENT,'(A17,I1,A5)')'RG (m) DUST MODE ',JJ,' (um)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=ZRG_DST(:,:,:,JJ) / (EXP(-3.*(LOG(ZSIG_DST(:,:,:,JJ)))**2))
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !
    WRITE(YRECFM,'(A6,I1)')'DSTN0A',JJ
    WRITE(YCOMMENT,'(A13,I1,A7)')'N0 DUST MODE ',JJ,' (1/m3)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=ZN0_DST(:,:,:,JJ)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !
    WRITE(YRECFM,'(A7,I1)')'DSTSIGA',JJ
    WRITE(YCOMMENT,'(A16,I1)')'SIGMA DUST MODE ',JJ
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=ZSIG_DST(:,:,:,JJ)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !DUST MASS CONCENTRATION
    WRITE(YRECFM,'(A4,I1)')'DSTMSS',JJ
    WRITE(YCOMMENT,'(A14,I1,A7)')'MASSCONC MODE ',JJ,'(ug/m3)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)= ZN0_DST(:,:,:,JJ)*4./3.*3.14*2500.*1e9 & !kg-->ug
       * (ZRG_DST(:,:,:,JJ)**3)*1.d-18 &  !um-->m
       * exp(4.5*log(ZSIG_DST(:,:,:,JJ))*log(ZSIG_DST(:,:,:,JJ)))
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !DUST BURDEN (g/m2)
    ZWORK21(:,:)=0.0
    DO JK=IKB,IKE
      ZWORK31(:,:,JK) = ZWORK31(:,:,JK) *(XZZ(:,:,JK+1)-XZZ(:,:,JK))      &
                       *1.d-6 ! Convert to ug/m2-->g/m2 in each layer
    END DO
    DO JK=IKB,IKE
      DO JT=IJB,IJE
        DO JI=IIB,IIE
           ZWORK21(JI,JT)=ZWORK21(JI,JT)+ZWORK31(JI,JT,JK)
        ENDDO
      ENDDO
    ENDDO
    WRITE(YRECFM,'(A7,I1)')'DSTBRDN',JJ
    WRITE(YCOMMENT,'(A13,I1)')'BURDEN (g/m2)',JJ
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  ENDDO
END IF
IF (LDUST.AND.LDEPOS_DST(IMI)) THEN
    ! 
     ZSDSTDEP=XSVT(:,:,:,NSV_DSTDEPBEG:NSV_DSTDEPEND)
     DO JSV = 1,NSV_DSTDEP   
      YRECFM=TRIM(UPCASE(CDEDSTNAMES(JSV)))//'T'
       WRITE(YCOMMENT,'(A6,A4,I3.3,A7)')'X_Y_Z_','DUSTDEP',JSV,' (ppbv)'
       ILENCH=LEN(YCOMMENT)
       ZWORK31(:,:,:)=ZSDSTDEP(:,:,:,JSV)*1.E9
       CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
     END DO
     DO JJ=1,NMODE_DST           
            ZWORK31(:,:,:)=0.0
! FOR CLOUDS
        WRITE(YRECFM,'(A9,I1)')'DSTDEPN0A',JJ
        WRITE(YCOMMENT,'(A16,I1,A7)')'N0 DUSTDEP MODE ', &
                                   JJ,' (1/m3)'            
! CLOUD: CALCULATE MOMENT 3 FROM TOTAL AEROSOL MASS            
            ZWORK31(:,:,:)=ZSDSTDEP(:,:,:,JJ)  &!==>molec_{aer}/molec_{air}
            *(XMOLARWEIGHT_DUST/XMD)           &!==>kg_{aer}/kg_{air}
            *XRHODREF(:,:,:)                   &!==>kg_{aer}/m3_{air}
            *(1.d0/XDENSITY_DUST)              &!==>m3_{aer}/m3_{air}
            *XM3TOUM3                          &!==>um3_{aer}/m3_{air}
            /(XPI*4./3.)                        !==>um3_{aer}/m3_{air}
            !==>volume 3rd moment
!CLOUD: CALCULATE MOMENT 0 FROM DISPERSION AND MEAN RADIUS            
            ZWORK31(:,:,:)=  ZWORK31(:,:,:)/      &
                    ((ZRG_DST(:,:,:,JJ)**3)*      &
                    EXP(4.5 * LOG(ZSIG_DST(:,:,:,JJ))**2))
!CLOUD: RETURN TO CONCENTRATION #/m3
            ZWORK31(:,:,:)= ZWORK31(:,:,:) *   XMD/ &
                     (XAVOGADRO*XRHODREF(:,:,:))
!CLOUD:  Get number concentration (#/molec_{air}==>#/m3)  
             ZWORK31(:,:,:)=                         &
                    ZWORK31(:,:,:)                  & !#/molec_{air}
                    * XAVOGADRO                     & !==>#/mole
                    / XMD                           & !==>#/kg_{air}
                    * XRHODREF(:,:,:)                 !==>#/m3  
     ILENCH=LEN(YCOMMENT)               
     CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
! CLOUD:   DUST MASS CONCENTRATION
       WRITE(YRECFM,'(A9,I1)')'DSTDEPMSS',JJ
       WRITE(YCOMMENT,'(A17,I1,A7)')'DEPMASSCONC MODE ', &
                                 JJ,'(ug/m3)'
       ILENCH=LEN(YCOMMENT)
       ZWORK31(:,:,:)= ZWORK31(:,:,:)*4./3.*3.14*2500.*1e9 & !kg-->ug
          * (ZRG_DST(:,:,:,JJ)**3)*1.d-18 &  !um-->m
          * exp(4.5*log(ZSIG_DST(:,:,:,JJ))*log(ZSIG_DST(:,:,:,JJ)))
       CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)                
!   FOR RAIN DROPS                
        WRITE(YRECFM,'(A9,I1)')'DSTDEPN0A',JJ+NMODE_DST
        WRITE(YCOMMENT,'(A16,I1,A7)')'N0 DUSTDEP MODE ', &
                                   JJ+NMODE_DST,' (1/m3)'
        ILENCH=LEN(YCOMMENT) 
        ZWORK31(:,:,:)=0.0
! RAIN: CALCULATE MOMENT 3 FROM TOTAL AEROSOL MASS                
        ZWORK31(:,:,:)=ZSDSTDEP(:,:,:,JJ+NMODE_DST)  &!==>molec_{aer}/molec_{air}
            *(XMOLARWEIGHT_DUST/XMD)           &!==>kg_{aer}/kg_{air}
            *XRHODREF(:,:,:)                   &!==>kg_{aer}/m3_{air}
            *(1.d0/XDENSITY_DUST)              &!==>m3_{aer}/m3_{air}
            *XM3TOUM3                          &!==>um3_{aer}/m3_{air}
            /(XPI*4./3.)                        !==>um3_{aer}/m3_{air}
            !==>volume 3rd moment   
!RAIN: CALCULATE MOMENT 0 FROM DISPERSION AND MEAN RADIUS      
     ZWORK31(:,:,:)= ZWORK31(:,:,:)/ &
             ((ZRG_DST(:,:,:,JJ)**3)*       &
              EXP(4.5 * LOG(ZSIG_DST(:,:,:,JJ))**2))
!RAIN: RETURN TO CONCENTRATION #/m3       
      ZWORK31(:,:,:)= ZWORK31(:,:,:) *   XMD/ &
                (XAVOGADRO*XRHODREF(:,:,:))
!RAIN: Get number concentration (#/molec_{air}==>#/m3)  
             ZWORK31(:,:,:)=                         &
             ZWORK31(:,:,:)                  & !#/molec_{air}
              * XAVOGADRO                           & !==>#/mole
              / XMD                                 & !==>#/kg_{air}
              * XRHODREF(:,:,:)                       !==>#/m3
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
! RAIN:   DUST MASS CONCENTRATION
       WRITE(YRECFM,'(A9,I1)')'DSTDEPMSS',JJ+NMODE_DST
       WRITE(YCOMMENT,'(A17,I1,A7)')'DEPMASSCONC MODE ', &
                                 JJ+NMODE_DST,'(ug/m3)'
       ILENCH=LEN(YCOMMENT)
       ZWORK31(:,:,:)= ZWORK31(:,:,:)*4./3.*3.14*2500.*1e9 & !kg-->ug
          * (ZRG_DST(:,:,:,JJ)**3)*1.d-18 &  !um-->m
          * exp(4.5*log(ZSIG_DST(:,:,:,JJ))*log(ZSIG_DST(:,:,:,JJ)))
       CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
     END DO       
!
 END IF
! Aerosol
IF ((LCHEMDIAG).AND.(LORILAM).AND.(LUSECHEM)) THEN
  DO JSV = NSV_AERBEG,NSV_AEREND
    YRECFM=TRIM(UPCASE(CAERONAMES(JSV-NSV_AERBEG+1)))//'T'
    WRITE(YCOMMENT,'(A6,A4,I3.3,A7)')'X_Y_Z_','AERO',JSV,' (ppbv)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=XSVT(:,:,:,JSV)*1.E9
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
         YCOMMENT,IRESP)
  END DO
  !
  IF (.NOT.(ASSOCIATED(XN3D)))   &
    ALLOCATE(XN3D(SIZE(XSVT,1),SIZE(XSVT,2),SIZE(XSVT,3),JPMODE))
  IF (.NOT.(ASSOCIATED(XRG3D)))  &
    ALLOCATE(XRG3D(SIZE(XSVT,1),SIZE(XSVT,2),SIZE(XSVT,3),JPMODE))
  IF (.NOT.(ASSOCIATED(XSIG3D))) &
    ALLOCATE(XSIG3D(SIZE(XSVT,1),SIZE(XSVT,2),SIZE(XSVT,3),JPMODE))
  !
  CALL  PPP2AERO(XSVT(:,:,:,NSV_AERBEG:NSV_AEREND), XRHODREF, &
                 PSIG3D=XSIG3D, PRG3D=XRG3D, PN3D=XN3D, PCTOTA=ZPTOTA) 
  DO JJ=1,JPMODE
    WRITE(YRECFM,'(A3,I1)')'RGA',JJ
    WRITE(YCOMMENT,'(A21,I1,A5)')'RG (nb) AEROSOL MODE ',JJ,' (um)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=XRG3D(:,:,:,JJ)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !
    WRITE(YRECFM,'(A4,I1)')'RGAM',JJ
    WRITE(YCOMMENT,'(A20,I1,A5)')'RG (m) AEROSOL MODE ',JJ,' (um)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=XRG3D(:,:,:,JJ) / (EXP(-3.*(LOG(XSIG3D(:,:,:,JJ)))**2))
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !
    WRITE(YRECFM,'(A3,I1)')'N0A',JJ
    WRITE(YCOMMENT,'(A16,I1,A7)')'N0 AEROSOL MODE ',JJ,' (1/cc)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=XN3D(:,:,:,JJ)*1.E-6
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !
    WRITE(YRECFM,'(A4,I1)')'SIGA',JJ
    WRITE(YCOMMENT,'(A19,I1)')'SIGMA AEROSOL MODE ',JJ
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=XSIG3D(:,:,:,JJ)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !
    WRITE(YRECFM,'(A4,I1)')'MSO4',JJ
    WRITE(YCOMMENT,'(A22,I1,A5)')'MASS SO4 AEROSOL MODE ',JJ,'(ug/m3)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_SO4,JJ)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !
    WRITE(YRECFM,'(A4,I1)')'MNO3',JJ
    WRITE(YCOMMENT,'(A22,I1,A5)')'MASS NO3 AEROSOL MODE ',JJ,'(ug/m3)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_NO3,JJ)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !
    WRITE(YRECFM,'(A4,I1)')'MNH3',JJ
    WRITE(YCOMMENT,'(A22,I1,A5)')'MASS NH3 AEROSOL MODE ',JJ,'(ug/m3)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_NH3,JJ)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !
    WRITE(YRECFM,'(A4,I1)')'MH2O',JJ
    WRITE(YCOMMENT,'(A22,I1,A5)') 'MASS H2O AEROSOL MODE ',JJ,'(ug/m3)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_H2O,JJ)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !
    IF (NSOA .EQ. 10) THEN
      WRITE(YRECFM,'(A5,I1)')'MSOA1',JJ
      WRITE(YCOMMENT,'(A23,I1,A5)')'MASS SOA1 AEROSOL MODE ',JJ,'(ug/m3)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_SOA1,JJ)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                  YCOMMENT,IRESP)
      !
      WRITE(YRECFM,'(A5,I1)')'MSOA2',JJ
      WRITE(YCOMMENT,'(A23,I1,A5)')'MASS SOA2 AEROSOL MODE ',JJ,'(ug/m3)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_SOA2,JJ)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                  YCOMMENT,IRESP)
      !
      WRITE(YRECFM,'(A5,I1)')'MSOA3',JJ
      WRITE(YCOMMENT,'(A23,I1,A5)')'MASS SOA3 AEROSOL MODE ',JJ,'(ug/m3)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_SOA3,JJ)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
           YCOMMENT,IRESP)
      !
      WRITE(YRECFM,'(A5,I1)')'MSOA4',JJ
      WRITE(YCOMMENT,'(A23,I1,A5)')'MASS SOA4 AEROSOL MODE ',JJ,'(ug/m3)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_SOA4,JJ)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
           YCOMMENT,IRESP)
      !
      WRITE(YRECFM,'(A5,I1)')'MSOA5',JJ
      WRITE(YCOMMENT,'(A23,I1,A5)')'MASS SOA5 AEROSOL MODE ',JJ,'(ug/m3)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_SOA5,JJ)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                  YCOMMENT,IRESP)
      !
      WRITE(YRECFM,'(A5,I1)')'MSOA6',JJ
      WRITE(YCOMMENT,'(A23,I1,A5)')'MASS SOA6 AEROSOL MODE ',JJ,'(ug/m3)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_SOA6,JJ)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                  YCOMMENT,IRESP)
      !
      WRITE(YRECFM,'(A5,I1)')'MSOA7',JJ
      WRITE(YCOMMENT,'(A23,I1,A5)')'MASS SOA7 AEROSOL MODE ',JJ,'(ug/m3)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_SOA7,JJ)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                  YCOMMENT,IRESP)
      !
      WRITE(YRECFM,'(A5,I1)')'MSOA8',JJ
      WRITE(YCOMMENT,'(A23,I1,A5)')'MASS SOA8 AEROSOL MODE ',JJ,'(ug/m3)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_SOA8,JJ)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                  YCOMMENT,IRESP)
      !
      WRITE(YRECFM,'(A5,I1)')'MSOA9',JJ
      WRITE(YCOMMENT,'(A23,I1,A5)')'MASS SOA9 AEROSOL MODE ',JJ,'(ug/m3)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_SOA9,JJ)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                  YCOMMENT,IRESP)
      !
      WRITE(YRECFM,'(A6,I1)')'MSOA10',JJ
      WRITE(YCOMMENT,'(A23,I1,A5)')'MASS SOA10 AEROSOL MODE ',JJ,'(ug/m3)'
      ILENCH=LEN(YCOMMENT)
      ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_SOA10,JJ)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                  YCOMMENT,IRESP)
    END IF
    !
    WRITE(YRECFM,'(A3,I1)')'MOC',JJ
    WRITE(YCOMMENT,'(A21,I1,A5)')'MASS OC AEROSOL MODE ',JJ,'(ug/m3)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_OC,JJ)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    !
    WRITE(YRECFM,'(A3,I1)')'MBC',JJ
    WRITE(YCOMMENT,'(A21,I1,A5)')'MASS BC AEROSOL MODE ',JJ,'(ug/m3)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=ZPTOTA(:,:,:,JP_AER_BC,JJ)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  ENDDO
END IF
!
!* Large Scale variables
!
IF (LVAR_LS) THEN
  YRECFM='LSUM'
  YCOMMENT='X_Y_Z_Large Scale U component (M/S)'
  IGRID=2
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XLSUM,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='LSVM'
  YCOMMENT='X_Y_Z_Large Scale V component (M/S)'
  IGRID=3
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XLSVM,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  IF (LWIND_ZM) THEN
    YCOMMENT='X_Y_Z_Large Scale Zonal and Meridian components of horizontal wind (M/S)'
    CALL UV_TO_ZONAL_AND_MERID(XLSUM,XLSVM,23, &
              HFMFILE=HFMFILE,HRECU='LSUM_ZM',HRECV='LSVM_ZM',HCOMMENT=YCOMMENT)
  ENDIF
  !
  YRECFM='LSWM'
  YCOMMENT='X_Y_Z_Large Scale vertical wind (M/S)'
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XLSWM,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='LSTHM'
  YCOMMENT='X_Y_Z_Large Scale potential Temperature (K)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XLSTHM,IGRID,ILENCH,YCOMMENT,IRESP)
!
  IF (LUSERV) THEN
    YRECFM='LSMRV'
    YCOMMENT='X_Y_Z_Large Scale Vapor Mixing Ratio (G/KG)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=XLSRVM(:,:,:)*1.E3
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
END IF
!
!* Forcing variables
!
IF (LVAR_FRC .AND. LFORCING) THEN
!
  DO JT=1,NFRC
    WRITE (YFRC,'(I3.3)') JT
!
    YRECFM='UFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XUFRC(:,JT),IGRID,ILENCH,     &
                                                            YCOMMENT,IRESP)
!
    YRECFM='VFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XVFRC(:,JT),IGRID,ILENCH,     &
                                                            YCOMMENT,IRESP)
!
    YRECFM='WFRC'//YFRC
    YCOMMENT=' '
    IGRID=4
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XWFRC(:,JT),IGRID,ILENCH,     &
                                                            YCOMMENT,IRESP)
!
    YRECFM='THFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XTHFRC(:,JT),IGRID,ILENCH,    &
                                                            YCOMMENT,IRESP)
!
    YRECFM='RVFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XRVFRC(:,JT),IGRID,ILENCH,    &
                                                            YCOMMENT,IRESP)
!
    YRECFM='TENDTHFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XTENDTHFRC(:,JT),IGRID,ILENCH,  &
                                                            YCOMMENT,IRESP)
!
    YRECFM='TENDRVFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XTENDRVFRC(:,JT),IGRID,ILENCH,  &
                                                            YCOMMENT,IRESP)
!
    YRECFM='GXTHFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XGXTHFRC(:,JT),IGRID,ILENCH,  &
                                                            YCOMMENT,IRESP)
!
    YRECFM='GYTHFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XGYTHFRC(:,JT),IGRID,ILENCH,  &
                                                            YCOMMENT,IRESP)
!
    YRECFM='PGROUNDFRC'//YFRC
    YCOMMENT=' '
    IGRID=0
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',XPGROUNDFRC(JT),IGRID,ILENCH,  &
                                                            YCOMMENT,IRESP)
!
  END DO
END IF
!
!-------------------------------------------------------------------------------
!
!*       1.7    Some diagnostic variables
!
IF (LTPZH .OR. LCOREF) THEN
!
!* Temperature in celsius
  YRECFM='TEMP'
  YCOMMENT='X_Y_Z_TEMPerature (C)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  ZWORK31(:,:,:)=ZTEMP(:,:,:) - XTT
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
!
!* Pressure in hPa        
  YRECFM='PRES'
  YCOMMENT='X_Y_Z_PRESsure (hPa)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  ZWORK31(:,:,:)=XPABST(:,:,:)*1E-2
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
!
!* Geopotential in meters
  YRECFM='ALT'
  YCOMMENT='X_Y_Z_ALTitude (M)'
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XZZ,IGRID,ILENCH,YCOMMENT,IRESP)
!
!* Relative humidity in percent
  IF (LUSERV) THEN
    ZWORK31(:,:,:)=SM_FOES(ZTEMP(:,:,:))
    ZWORK33(:,:,:)=ZWORK31(:,:,:)
    ZWORK31(:,:,:)=(XMV/XMD)*ZWORK31(:,:,:)/(XPABST(:,:,:)-ZWORK31(:,:,:))
    ZWORK32(:,:,:)=100.*XRT(:,:,:,1)/ZWORK31(:,:,:)
    IF (CCLOUD(1:3) =='ICE' .OR. CCLOUD =='C3R5' .OR. CCLOUD == 'LIMA')  THEN
      WHERE ( ZTEMP(:,:,:)< XTT)
        ZWORK31(:,:,:) = EXP( XALPI - XBETAI/ZTEMP(:,:,:) &
                       - XGAMI*ALOG(ZTEMP(:,:,:)) ) !saturation over ice
        ZWORK33(:,:,:)=ZWORK31(:,:,:)
        ZWORK31(:,:,:)=(XMV/XMD)*ZWORK31(:,:,:)/(XPABST(:,:,:)-ZWORK31(:,:,:))
        ZWORK32(:,:,:)=100.*XRT(:,:,:,1)/ZWORK31(:,:,:)
      END WHERE
    END IF
    YRECFM='REHU'
    YCOMMENT='X_Y_Z_RElative HUmidity (%)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK32,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    ZWORK33(:,:,:)=ZWORK33(:,:,:)*ZWORK32(:,:,:)*1E-4
    YRECFM='VPRES'
    YCOMMENT='X_Y_Z_Vapor PRESsure (hPa)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK33,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    IF (LCOREF) THEN
      ZWORK33(:,:,:)=(77.6*( XPABST(:,:,:)*1E-2                &
                            +ZWORK33(:,:,:)*4810/ZTEMP(:,:,:)) &
                      -6*ZWORK33(:,:,:)                        )/ZTEMP(:,:,:)
      YRECFM='COREF'
      YCOMMENT='X_Y_Z_REFraction COindex (N-units)'
      IGRID=1
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK33,IGRID,ILENCH,YCOMMENT,IRESP)
      !
      ZWORK33(:,:,:)=ZWORK33(:,:,:)+MZF(1,IKU,1,XZZ(:,:,:))*1E6/XRADIUS
      YRECFM='MCOREF'
      YCOMMENT='X_Y_Z_Modified REFraction COindex (M-units)'
      IGRID=1
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK33,IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
ELSE
    PRINT*, 'NO WATER VAPOR IN ',HFMFILE,' RELATIVE HUMIDITY IS NOT COMPUTED'
  END IF
!
END IF
!
!-------------------------------------------------------------------------------
!
!* Virtual potential temperature
!
IF ( LMOIST_V .OR. LMSLP .OR. LBLTOP ) THEN
  ALLOCATE(ZTHETAV(IIU,IJU,IKU))
!
  IF(NRR > 0) THEN
!   compute the ratio : 1 + total water mass / dry air mass
    ZRV_OV_RD = XRV / XRD
    ZTHETAV(:,:,:) = 1. + XRT(:,:,:,1)
    DO JLOOP = 2,1+NRRL+NRRI                
      ZTHETAV(:,:,:) = ZTHETAV(:,:,:) + XRT(:,:,:,JLOOP)
    END DO
! compute the virtual potential temperature when water is present in any form
    ZTHETAV(:,:,:) = XTHT(:,:,:) * (1.+XRT(:,:,:,1)*ZRV_OV_RD) / ZTHETAV(:,:,:)
  ELSE
! compute the virtual potential temperature when water is absent
    ZTHETAV(:,:,:) = XTHT(:,:,:)
  END IF
!
  IF (LMOIST_V .AND. NRR > 0) THEN
! Virtual potential temperature
    YRECFM='THETAV'
    YCOMMENT='X_Y_Z_Virtual potential temperature (K)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZTHETAV,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
!
END IF
!
!-------------------------------------------------------------------------------
!
!* Thetae computation according eq.(21), (43) of Bolton 1980 (MWR108,p 1046-1053)
!
IF (( LMOIST_E .OR. LBV_FR ) .AND. (NRR>0)) THEN
  ALLOCATE(ZTHETAE(IIU,IJU,IKU))
  !
  ZWORK31(:,:,:) = MAX(XRT(:,:,:,1),1.E-10)
  ZTHETAE(:,:,:)= (    2840./                                          &
         (3.5*ALOG(XTHT(:,:,:)*( XPABST(:,:,:)/XP00 )**(XRD/XCPD)  )   &
         - ALOG( XPABST(:,:,:)*0.01*ZWORK31(:,:,:) / ( 0.622+ZWORK31(:,:,:) ) ) &
         -4.805   )    ) + 55.
  ZTHETAE(:,:,:)= XTHT(:,:,:) * EXP( (3376. / ZTHETAE(:,:,:) - 2.54)  &
                 *ZWORK31(:,:,:) *(1. +0.81 *ZWORK31(:,:,:)) )
!
  IF (LMOIST_E) THEN
    YRECFM='THETAE'
    YCOMMENT='X_Y_Z_Equivalent potential Temperature (K)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZTHETAE,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
END IF
!-------------------------------------------------------------------------------
!
!* Thetaes computation 
!
IF (LMOIST_ES .AND. (NRR>0)) THEN
  ALLOCATE(ZTHETAES(IIU,IJU,IKU))
  ZWORK31(:,:,:) = MAX(QSAT(ZTEMP(:,:,:),XPABST(:,:,:)),1.E-10)
  ZTHETAES(:,:,:)= (    2840./                                          &
       (3.5*ALOG(XTHT(:,:,:)*( XPABST(:,:,:)/XP00 )**(XRD/XCPD)  )   &
       - ALOG( XPABST(:,:,:)*0.01*ZWORK31(:,:,:) / ( 0.622+ZWORK31(:,:,:) ) ) &
       -4.805   )    ) + 55.
  ZTHETAES(:,:,:)= XTHT(:,:,:) * EXP( (3376. / ZTHETAE(:,:,:) - 2.54)  &
               *ZWORK31(:,:,:) *(1. +0.81 *ZWORK31(:,:,:)) )
  YRECFM='THETAES'
  YCOMMENT='X_Y_Z_Equivalent Saturated potential Temperature(K)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZTHETAES,IGRID,ILENCH,YCOMMENT,IRESP)
ENDIF
!
!-------------------------------------------------------------------------------
!
!* Vorticity quantities
!
IF (LVORT) THEN
! Vorticity x
  ZWORK31(:,:,:)=MYF(MZF(1,IKU,1,MXM(ZVOX(:,:,:))))
  YRECFM='UM1'
  YCOMMENT='X_Y_Z_x component of vorticity (/S)'
  IGRID=2
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
!    
! Vorticity y
  ZWORK32(:,:,:)=MZF(1,IKU,1,MXF(MYM(ZVOY(:,:,:))))
  YRECFM='VM1'
  YCOMMENT='X_Y_Z_y component of vorticity (/S)'
  IGRID=3
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK32,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  IF (LWIND_ZM) THEN
    YCOMMENT='X_Y_Z_Zonal and Meridian components of horizontal vorticity (M/S)'
    CALL UV_TO_ZONAL_AND_MERID(ZWORK31,ZWORK32,23, &
                HFMFILE=HFMFILE,HRECU='UM1_ZM',HRECV='VM1_ZM',HCOMMENT=YCOMMENT)
  ENDIF
!    
! Vorticity z
  ZWORK31(:,:,:)=MXF(MYF(MZM(1,IKU,1,ZVOZ(:,:,:))))
  YRECFM='WM1'
  YCOMMENT='X_Y_Z_relative vorticity (/S)'
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
!
! Absolute Vorticity 
  ZWORK31(:,:,:)=MYF(MXF(ZVOZ(:,:,:))) + ZCORIOZ(:,:,:)
  YRECFM='ABVOR'
  YCOMMENT='X_Y_Z_z ABsolute VORticity (/S)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
!
END IF
!    
IF ( LMEAN_POVO ) THEN 
  !
  ALLOCATE(IWORK1(SIZE(XTHT,1),SIZE(XTHT,2)))
  !
  IWORK1(:,:)=0
  ZWORK21(:,:)=0.
  IF (XMEAN_POVO(1)>XMEAN_POVO(2)) THEN
    XMEAN_POVO(1) = ZX0D
    XMEAN_POVO(2) = XMEAN_POVO(1)
    ZX0D          = XMEAN_POVO(2)
  END IF
  DO JK=IKB,IKE
    WHERE((XPABST(:,:,JK)>XMEAN_POVO(1)).AND.(XPABST(:,:,JK)<XMEAN_POVO(2)))
      ZWORK21(:,:)=ZWORK21(:,:)+ZPOVO(:,:,JK)
      IWORK1(:,:)=IWORK1(:,:)+1
    END WHERE
  END DO
  WHERE (IWORK1(:,:)>0) ZWORK21(:,:)=ZWORK21(:,:)/REAL( IWORK1(:,:) )
  YRECFM='MEAN_POVO'
  YCOMMENT='X_Y_Z_MEAN of POtential VOrticity (PVU)'
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
! Virtual Potential Vorticity in PV units
IF (LMOIST_V .AND. (NRR>0) ) THEN
  ZWORK31(:,:,:)=GX_M_M(1,IKU,1,ZTHETAV,XDXX,XDZZ,XDZX)
  ZWORK32(:,:,:)=GY_M_M(1,IKU,1,ZTHETAV,XDYY,XDZZ,XDZY)
  ZWORK33(:,:,:)=GZ_M_M(1,IKU,1,ZTHETAV,XDZZ)
  ZWORK34(:,:,:)= ZWORK31(:,:,:)*MZF(1,IKU,1,MYF(ZVOX(:,:,:)))     &
               + ZWORK32(:,:,:)*MZF(1,IKU,1,MXF(ZVOY(:,:,:)))     &
               + ZWORK33(:,:,:)*(MYF(MXF(ZVOZ(:,:,:))) + ZCORIOZ(:,:,:))
  ZWORK34(:,:,:)=ZWORK34(:,:,:)*1E6/XRHODREF(:,:,:)
  YRECFM='POVOV'
  YCOMMENT='X_Y_Z_Virtual POtential VOrticity (PVU)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK34,IGRID,ILENCH,YCOMMENT,IRESP)
!
  IF (LMEAN_POVO) THEN
    IWORK1(:,:)=0
    ZWORK21(:,:)=0.
    DO JK=IKB,IKE
      WHERE((XPABST(:,:,JK)>XMEAN_POVO(1)).AND.(XPABST(:,:,JK)<XMEAN_POVO(2)))
          ZWORK21(:,:)=ZWORK21(:,:)+ZWORK34(:,:,JK)
          IWORK1(:,:)=IWORK1(:,:)+1
      END WHERE
    END DO
    WHERE(IWORK1(:,:)>0) ZWORK21(:,:)=ZWORK21(:,:)/REAL( IWORK1(:,:) )
    YRECFM='MEAN_POVOV'
    YCOMMENT='X_Y_Z_MEAN of Virtual POtential VOrticity (PVU)'
    IGRID=4
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
END IF
!
! Equivalent Potential Vorticity in PV units
IF (LMOIST_E .AND. (NRR>0) ) THEN
!
  ZWORK31(:,:,:)=GX_M_M(1,IKU,1,ZTHETAE,XDXX,XDZZ,XDZX)
  ZWORK32(:,:,:)=GY_M_M(1,IKU,1,ZTHETAE,XDYY,XDZZ,XDZY)
  ZWORK33(:,:,:)=GZ_M_M(1,IKU,1,ZTHETAE,XDZZ)
  ZWORK34(:,:,:)= ZWORK31(:,:,:)*MZF(1,IKU,1,MYF(ZVOX(:,:,:)))     &
                + ZWORK32(:,:,:)*MZF(1,IKU,1,MXF(ZVOY(:,:,:)))     &
                + ZWORK33(:,:,:)*(MYF(MXF(ZVOZ(:,:,:))) + ZCORIOZ(:,:,:))
  ZWORK34(:,:,:)=ZWORK34(:,:,:)*1E6/XRHODREF(:,:,:)
  YRECFM='POVOE'
  YCOMMENT='X_Y_Z_Equivalent POtential VOrticity (PVU)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK34,IGRID,ILENCH,YCOMMENT,IRESP)
!
  IF (LMEAN_POVO) THEN
    IWORK1(:,:)=0
    ZWORK21(:,:)=0.
    DO JK=IKB,IKE
      WHERE((XPABST(:,:,JK)>XMEAN_POVO(1)).AND.(XPABST(:,:,JK)<XMEAN_POVO(2)))
        ZWORK21(:,:)=ZWORK21(:,:)+ZWORK34(:,:,JK)
        IWORK1(:,:)=IWORK1(:,:)+1
      END WHERE
    END DO
    WHERE(IWORK1(:,:)>0) ZWORK21(:,:)=ZWORK21(:,:)/REAL( IWORK1(:,:) )
    YRECFM='MEAN_POVOE'
    YCOMMENT='X_Y_Z_MEAN of Equivalent POtential VOrticity (PVU)'
    IGRID=4
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
    DEALLOCATE(IWORK1)
  END IF 
  !
END IF
!
! Equivalent Saturated Potential Vorticity in PV units
IF (LMOIST_ES .AND. (NRR>0) ) THEN
  ZWORK31(:,:,:)=GX_M_M(1,IKU,1,ZTHETAES,XDXX,XDZZ,XDZX)
  ZWORK32(:,:,:)=GY_M_M(1,IKU,1,ZTHETAES,XDYY,XDZZ,XDZY)
  ZWORK33(:,:,:)=GZ_M_M(1,IKU,1,ZTHETAES,XDZZ)
  ZWORK34(:,:,:)= ZWORK31(:,:,:)*MZF(1,IKU,1,MYF(ZVOX(:,:,:)))     &
                + ZWORK32(:,:,:)*MZF(1,IKU,1,MXF(ZVOY(:,:,:)))     &
                + ZWORK33(:,:,:)*(MYF(MXF(ZVOZ(:,:,:))) + ZCORIOZ(:,:,:))
  ZWORK34(:,:,:)=ZWORK34(:,:,:)*1E6/XRHODREF(:,:,:)
  YRECFM='POVOES'
  YCOMMENT='X_Y_Z_Equivalent Saturated POtential VOrticity (PVU)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK34,IGRID,ILENCH,YCOMMENT,IRESP)
ENDIF
!
!
!-------------------------------------------------------------------------------
!
!* Horizontal divergence
!
IF (LDIV) THEN
!
  ZWORK31=GX_U_M(1,IKU,1,XUT,XDXX,XDZZ,XDZX) + GY_V_M(1,IKU,1,XVT,XDYY,XDZZ,XDZY)
  YRECFM='HDIV'
  YCOMMENT='X_Y_Z_Horizontal DIVergence (/S)'
  YCOMMENT='(1/s)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
!
  IF (LUSERV) THEN
   YRECFM= 'HMDIV'
   YCOMMENT='X_Y_Z_Horizontal Moisture DIVergence HMDIV (KG/M3/S)'
   ILENCH=LEN(YCOMMENT)
   ZWORK31=MXM(XRHODREF*XRT(:,:,:,1))*XUT
   ZWORK32=MYM(XRHODREF*XRT(:,:,:,1))*XVT
   ZWORK33=GX_U_M(1,IKU,1,ZWORK31,XDXX,XDZZ,XDZX) + GY_V_M(1,IKU,1,ZWORK32,XDYY,XDZZ,XDZY)
   CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK33,IGRID,ILENCH,  &
        YCOMMENT,IRESP)
 END IF
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!* Geostrophic and Ageostrophic wind (m/s)
!
IF (LGEO .OR. LAGEO) THEN
  ALLOCATE(ZPHI(IIU,IJU,IKU))
  IF(CEQNSYS=='MAE' .OR. CEQNSYS=='DUR') THEN
    ZPHI(:,:,:)=(XPABST(:,:,:)/XP00)**(XRD/XCPD)-XEXNREF(:,:,:)
    !
    ZPHI(1,1,:)=2*ZPHI(1,2,:)-ZPHI(1,3,:)
    ZPHI(1,IJU,:)=2*ZPHI(1,IJU-1,:)-ZPHI(1,IJU-2,:)
    ZPHI(IIU,1,:)=2*ZPHI(IIU,2,:)-ZPHI(IIU,3,:)
    ZPHI(IIU,IJU,:)=2*ZPHI(IIU,IJU-1,:)-ZPHI(IIU,IJU-2,:)
    ZWORK31(:,:,:)=-MXM(GY_M_M(1,IKU,1,ZPHI,XDYY,XDZZ,XDZY)*XCPD*XTHVREF/ZCORIOZ)
    !
    ZPHI(1,1,:)=2*ZPHI(2,1,:)-ZPHI(3,1,:)
    ZPHI(IIU,1,:)=2*ZPHI(IIU-1,1,:)-ZPHI(IIU-2,1,:)
    ZPHI(1,IJU,:)=2*ZPHI(2,IJU,:)-ZPHI(3,IJU,:)
    ZPHI(IIU,IJU,:)=2*ZPHI(IIU-1,IJU,:)-ZPHI(IIU-2,IJU,:)
    ZWORK32(:,:,:)=MYM(GX_M_M(1,IKU,1,ZPHI,XDXX,XDZZ,XDZX)*XCPD*XTHVREF/ZCORIOZ)
  !
  ELSE IF(CEQNSYS=='LHE') THEN
    ZPHI(:,:,:)= ((XPABST(:,:,:)/XP00)**(XRD/XCPD)-XEXNREF(:,:,:))   &
               * XCPD * XTHVREF(:,:,:)
    !
    ZPHI(1,1,:)=2*ZPHI(1,2,:)-ZPHI(1,3,:)
    ZPHI(1,IJU,:)=2*ZPHI(1,IJU-1,:)-ZPHI(1,IJU-2,:)
    ZPHI(IIU,1,:)=2*ZPHI(IIU,2,:)-ZPHI(IIU,3,:)
    ZPHI(IIU,IJU,:)=2*ZPHI(IIU,IJU-1,:)-ZPHI(IIU,IJU-2,:)
    ZWORK31(:,:,:)=-MXM(GY_M_M(1,IKU,1,ZPHI,XDYY,XDZZ,XDZY)/ZCORIOZ)
    !
    ZPHI(1,1,:)=2*ZPHI(2,1,:)-ZPHI(3,1,:)
    ZPHI(IIU,1,:)=2*ZPHI(IIU-1,1,:)-ZPHI(IIU-2,1,:)
    ZPHI(1,IJU,:)=2*ZPHI(2,IJU,:)-ZPHI(3,IJU,:)
    ZPHI(IIU,IJU,:)=2*ZPHI(IIU-1,IJU,:)-ZPHI(IIU-2,IJU,:)
    ZWORK32(:,:,:)=MYM(GX_M_M(1,IKU,1,ZPHI,XDXX,XDZZ,XDZX)/ZCORIOZ)
  END IF
  DEALLOCATE(ZPHI)
!
  IF (LGEO) THEN 
    YRECFM='UM88'
    YCOMMENT='X_Y_Z_U component of GEOstrophic wind (m/s)'
    IGRID=2
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
! 
    YRECFM='VM88'
    YCOMMENT='X_Y_Z_V component of GEOstrophic wind (m/s)'
    IGRID=3
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK32,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    IF (LWIND_ZM) THEN
    YCOMMENT='X_Y_Z_Zonal and Meridian components of GEOstrophic wind (m/s)'
      CALL UV_TO_ZONAL_AND_MERID(ZWORK31,ZWORK32,23, &
              HFMFILE=HFMFILE,HRECU='UM88_ZM',HRECV='VM88_ZM',HCOMMENT=YCOMMENT)
    ENDIF
!
! wm necessary to plot vertical cross sections of wind vectors
    YRECFM='WM88'
    YCOMMENT='X_Y_Z_vertical wind (m/s)'
    YCOMMENT='(M/S)'
    IGRID=4
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XWT,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
!
  IF (LAGEO) THEN
    ZWORK31(:,:,:)=XUT(:,:,:)-ZWORK31(:,:,:)
    YRECFM='UM89'
    YCOMMENT='X_Y_Z_U component of AGEOstrophic wind (m/s)'
    IGRID=2
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
!
    ZWORK32(:,:,:)=XVT(:,:,:)-ZWORK32(:,:,:)
    YRECFM='VM89'
    YCOMMENT='X_Y_Z_V component of AGEOstrophic wind (m/s)'
    IGRID=3
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK32,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    IF (LWIND_ZM) THEN
      YCOMMENT='X_Y_Z_Zonal and Meridian components of AGEOstrophic wind (m/s)'
      CALL UV_TO_ZONAL_AND_MERID(ZWORK31,ZWORK32,23, &
              HFMFILE=HFMFILE,HRECU='UM89_ZM',HRECV='VM89_ZM',HCOMMENT=YCOMMENT)
    ENDIF
!
! wm necessary to plot vertical cross sections of wind vectors
    YRECFM='WM89'
    YCOMMENT='X_Y_Z_vertical wind (m/s)'
    IGRID=4
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XWT,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
!
END IF
!
!-------------------------------------------------------------------------------
!
!* Mean Sea Level Pressure in hPa   
!
IF (LMSLP) THEN
  ZGAMREF=-6.5E-3
!  Exner function at the first mass point
  ZWORK21(:,:) = (XPABST(:,:,IKB) /XP00)**(XRD/XCPD)
!  virtual temperature at the first mass point
  ZWORK21(:,:) = ZWORK21(:,:) * ZTHETAV(:,:,IKB)
!  virtual temperature at ground level
  ZWORK21(:,:) = ZWORK21(:,:) - ZGAMREF*((XZZ(:,:,IKB)+XZZ(:,:,IKB+1))/2.-XZS(:,:))
!  virtual temperature at sea level
  ZWORK22(:,:) = ZWORK21(:,:) - ZGAMREF*XZS(:,:)
!  average underground virtual temperature
  ZWORK22(:,:) = 0.5*(ZWORK21(:,:)+ZWORK22(:,:))
!  surface pressure
  ZWORK21(:,:) = ( XPABST(:,:,IKB) + XPABST(:,:,IKB-1) )*.5
!  sea level pressure (hPa)
  ZWORK22(:,:) = 1.E-2*ZWORK21(:,:)*EXP(XG*XZS(:,:)/(XRD*ZWORK22(:,:)))
!
  YRECFM='MSLP'
  YCOMMENT='X_Y_Mean Sea Level Pressure (hPa)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK22,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!-------------------------------------------------------------------------------
!
!* Vapor, cloud water and ice thickness
!
IF (LTHW) THEN
!
  ZWORK21(:,:) = 0.
  IF(SIZE(XRT,4)>=1)THEN
    DO JK = IKB,IKE
      ZWORK21(:,:) = ZWORK21(:,:)+XRHODREF(:,:,JK)*XRT(:,:,JK,1) * &
                     (XZZ(:,:,JK+1)-XZZ(:,:,JK))/XRHOLW
    END DO
    ZWORK21(:,:) = ZWORK21(:,:)*1000. ! vapor water in mm unit
    YRECFM      ='THVW'
    YCOMMENT    ='X_Y_THickness of Vapor Water (MM)'
    IGRID       =1
    ILENCH      =LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
  !
  ZWORK21(:,:) = 0.
  IF(SIZE(XRT,4)>=2)THEN
    DO JK = IKB,IKE
      ZWORK21(:,:) = ZWORK21(:,:)+XRHODREF(:,:,JK)*XRT(:,:,JK,2) * &
                     (XZZ(:,:,JK+1)-XZZ(:,:,JK))/XRHOLW
    END DO
    ZWORK21(:,:) = ZWORK21(:,:)*1000. ! cloud water in mm unit
    YRECFM      ='THCW'
    YCOMMENT    ='X_Y_THickness of Cloud Water (MM)'
    IGRID       =1
    ILENCH      =LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
  !
  ZWORK21(:,:) = 0.
  IF(SIZE(XRT,4)>=3)THEN
    DO JK = IKB,IKE
      ZWORK21(:,:) = ZWORK21(:,:)+XRHODREF(:,:,JK)*XRT(:,:,JK,3) * &
                     (XZZ(:,:,JK+1)-XZZ(:,:,JK))/XRHOLW
    END DO
    ZWORK21(:,:) = ZWORK21(:,:)*1000. ! rain water in mm unit
    YRECFM      ='THRW'
    YCOMMENT    ='X_Y_THickness of Rain Water (MM)'
    IGRID       =1
    ILENCH      =LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
  !
  ZWORK21(:,:)   = 0.
  IF(SIZE(XRT,4)>=4)THEN
    DO JK = IKB,IKE
      ZWORK21(:,:) = ZWORK21(:,:)+XRHODREF(:,:,JK)*XRT(:,:,JK,4) * &
                   (XZZ(:,:,JK+1)-XZZ(:,:,JK))/XRHOLW
    END DO
    ZWORK21(:,:) = ZWORK21(:,:)*1000.   ! ice thickness in mm unit
    YRECFM      ='THIC'
    YCOMMENT    ='X_Y_THickness of ICe (MM)'
    IGRID       =1
    ILENCH      =LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
  !
  ZWORK21(:,:)   = 0.
  IF(SIZE(XRT,4)>=5)THEN
    DO JK = IKB,IKE
      ZWORK21(:,:) = ZWORK21(:,:)+XRHODREF(:,:,JK)*XRT(:,:,JK,5) * &
                   (XZZ(:,:,JK+1)-XZZ(:,:,JK))/XRHOLW
    END DO
    ZWORK21(:,:) = ZWORK21(:,:)*1000.   ! snow thickness in mm unit
    YRECFM      ='THSN'
    YCOMMENT    ='X_Y_THickness of SNow (MM)'
    IGRID       =1
    ILENCH      =LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
  !
  ZWORK21(:,:)   = 0.
  IF(SIZE(XRT,4)>=6)THEN
    DO JK = IKB,IKE
      ZWORK21(:,:) = ZWORK21(:,:)+XRHODREF(:,:,JK)*XRT(:,:,JK,6) * &
                   (XZZ(:,:,JK+1)-XZZ(:,:,JK))/XRHOLW
    END DO
    ZWORK21(:,:) = ZWORK21(:,:)*1000.   ! graupel thickness in mm unit
    YRECFM      ='THGR'
    YCOMMENT    ='X_Y_THickness of GRaupel (MM)'
    IGRID       =1
    ILENCH      =LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
  !
  ZWORK21(:,:)   = 0.
  IF(SIZE(XRT,4)>=7)THEN
    DO JK = IKB,IKE
      ZWORK21(:,:) = ZWORK21(:,:)+XRHODREF(:,:,JK)*XRT(:,:,JK,7) * &
                   (XZZ(:,:,JK+1)-XZZ(:,:,JK))/XRHOLW
    END DO
    ZWORK21(:,:) = ZWORK21(:,:)*1000.   ! hail thickness in mm unit
    YRECFM      ='THHA'
    YCOMMENT    ='X_Y_THickness of HAil (MM)'
    IGRID       =1
    ILENCH      =LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
END IF
!
!-------------------------------------------------------------------------------
!
!* Accumulated and instantaneous total precip rates  in mm and mm/h
!
IF (LTOTAL_PR .AND. SIZE (XACPRR)>0 ) THEN
  YRECFM      ='ACTOPR'
  YCOMMENT    ='X_Y_ACccumulated TOtal Precipitation Rate (MM)'
  IGRID       =1
  ILENCH      =LEN(YCOMMENT)
  ZWORK21(:,:) = 0.
  !
  IF (LUSERR) THEN
    ZWORK21(:,:) = XACPRR(:,:)*1E3      
  END IF
  IF (CCLOUD(1:3) == 'ICE' .OR. CCLOUD == 'C3R5' .OR. CCLOUD == 'LIMA') THEN
    ZWORK21(:,:) = ZWORK21(:,:) + (XACPRS(:,:) + XACPRG(:,:))*1E3
    IF (SIZE(XINPRC) /= 0 ) &         
      ZWORK21(:,:) = ZWORK21(:,:) + XACPRC(:,:) *1E3
    IF (SIZE(XINPRH) /= 0 ) &        
      ZWORK21(:,:) = ZWORK21(:,:) + XACPRH(:,:) *1E3
  END IF
  IF (CCLOUD == 'C2R2' .OR. CCLOUD == 'C3R5' .OR. CCLOUD == 'KHKO' &
                                             .OR. CCLOUD == 'LIMA' ) THEN
    IF (SIZE(XINPRC) /= 0 ) &         
      ZWORK21(:,:) = ZWORK21(:,:) + XACPRC(:,:) *1E3
  END IF
  IF (CDCONV /= 'NONE') THEN
    ZWORK21(:,:) = ZWORK21(:,:) + XPACCONV(:,:)*1E3    
  END IF
  IF (LUSERR .OR. CCLOUD(1:3) == 'ICE' .OR. CCLOUD == 'C3R5' .OR. &
                  CCLOUD == 'LIMA' .OR. CDCONV /= 'NONE') THEN
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  ELSE
    PRINT * ,'YOU WANT TO COMPUTE THE ACCUMULATED RAIN'
    PRINT * ,'BUT NO RAIN IS PRESENT IN THE MODEL' 
  END IF
  ! 
  ! calculation of the mean accumulated precipitations in the mesh-grid of a 
  !large-scale model
  IF (LMEAN_PR .AND. LUSERR) THEN
    DO JK=1,SIZE(XMEAN_PR),2      
      IF (XMEAN_PR(JK) .NE. XUNDEF .AND. XMEAN_PR(JK+1) .NE. XUNDEF) THEN
        PRINT * ,'MEAN accumulated RAIN: GRID ', XMEAN_PR(JK), XMEAN_PR(JK+1)
        CALL COMPUTE_MEAN_PRECIP(ZWORK21,XMEAN_PR(JK:JK+1),ZWORK22,IGRID)
        !
        JI=INT(XMEAN_PR(JK))
        JJ=INT(XMEAN_PR(JK+1))
        WRITE(YRECFM,'(A9,2I2.2)')'LS_ACTOPR',JI,JJ
        YCOMMENT    ='X_Y_Large Scale ACccumulated TOtal Precipitation Rate (MM)'
        ILENCH      =LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK22,IGRID,ILENCH,YCOMMENT,IRESP)
      END IF
    END DO
    !
  END IF
  !
  !
  YRECFM      ='INTOPR'
  YCOMMENT    ='X_Y_INstantaneous TOtal Precipitation Rate (MM/H)'
  IGRID       =1
  ILENCH      =LEN(YCOMMENT)
  ZWORK21(:,:) = 0.
  !    
  IF (LUSERR) THEN
    ZWORK21(:,:) = XINPRR(:,:)*3.6E6      
  END IF
  IF (CCLOUD(1:3) == 'ICE' .OR. CCLOUD == 'C3R5' .OR. CCLOUD == 'LIMA') THEN
    ZWORK21(:,:) = ZWORK21(:,:) + (XINPRS(:,:) + XINPRG(:,:))*3.6E6
    IF (SIZE(XINPRC) /= 0 ) &      
      ZWORK21(:,:) = ZWORK21(:,:) + XINPRC(:,:) *3.6E6       
    IF (SIZE(XINPRH) /= 0 ) &      
      ZWORK21(:,:) = ZWORK21(:,:) + XINPRH(:,:) *3.6E6       
  END IF
  IF (CCLOUD == 'C2R2' .OR. CCLOUD == 'C3R5' .OR. CCLOUD == 'KHKO' &
                                             .OR. CCLOUD == 'LIMA' ) THEN
    IF (SIZE(XINPRC) /= 0 ) &         
      ZWORK21(:,:) = ZWORK21(:,:) + XINPRC(:,:) *3.6E6        
  END IF
  IF (CDCONV /= 'NONE') THEN
    ZWORK21(:,:) = ZWORK21(:,:) + XPRCONV(:,:)*3.6E6  
  END IF
  IF (LUSERR .OR. CCLOUD(1:3) == 'ICE' .OR. CCLOUD == 'C3R5' .OR. &
                  CCLOUD == 'LIMA' .OR. CDCONV /= 'NONE') THEN
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  ELSE
    PRINT * ,'YOU WANT TO COMPUTE THE RAIN RATE'
    PRINT * ,'BUT NO RAIN IS PRESENT IN THE MODEL' 
  END IF
!
  ! calculation of the mean instantaneous precipitations in the mesh-grid of a 
  ! large-scale model
  IF (LMEAN_PR .AND. LUSERR) THEN
    CALL COMPUTE_MEAN_PRECIP(ZWORK21,XMEAN_PR,ZWORK22,IGRID)
!
    YRECFM      ='LS_INTOPR'
    YCOMMENT    ='X_Y_Large Scale INstantaneous TOtal Precipitation Rate (MM/H)'
    ILENCH      =LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK22,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
!
END IF
!
!-------------------------------------------------------------------------------
!
!* CAPEMAX, CINMAX (corresponding to CAPEMAX), CAPE, CIN, DCAPE, VKE in J/kg
!
IF (NCAPE >=0 .AND. LUSERV) THEN
   ZWORK31(:,:,:) = XRT(:,:,:,1) * 1000.  ! vapour mixing ratio in g/kg
   ZWORK32(:,:,:)=0.0
   ZWORK33(:,:,:)=0.0
   ZWORK34(:,:,:)=0.0
   CALL CALCSOUND( XPABST(:,:,IKB:IKE)* 0.01 ,ZTEMP(:,:,IKB:IKE)- XTT, &
                   ZWORK31(:,:,IKB:IKE),                               &
                   ZWORK32(:,:,IKB:IKE),ZWORK33(:,:,IKB:IKE),          &
                   ZWORK34(:,:,IKB:IKE),ZWORK21,ZWORK22                )
  !
  YRECFM='CAPEMAX'
  YCOMMENT='X_Y_MAX of Convective Available Potential Energy (J/kg)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='CINMAX'
  YCOMMENT='X_Y_MAX of Convective INhibition energy (J/kg)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK22,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  IF (NCAPE >=1) THEN
    YRECFM='CAPE3D'
    YCOMMENT='X_Y_Z_Convective Available Potential Energy (J/kg)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK32,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    YRECFM='CIN3D'
    YCOMMENT='X_Y_Z_Convective INhibition energy (J/kg)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK33,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    YRECFM='DCAPE3D'
    YCOMMENT='X_Y_Z_ (J/kg)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK34,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
  !
  IF (NCAPE >=2) THEN
    ZWORK31(:,:,1:IKU-1)= 0.5*(XWT(:,:,1:IKU-1)+XWT(:,:,2:IKU))
    ZWORK31(:,:,IKU)    = 0.
    ZWORK31=0.5*ZWORK31**2
    !
    YRECFM='VKE'
    YCOMMENT='X_Y_Z_Vertical Kinetic Energy (J/kg)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
ENDIF
!
!-------------------------------------------------------------------------------
!
!* B-V frequency to assess thermal tropopause
!
IF (LBV_FR) THEN
  ZWORK32(:,:,:)=DZM(1,IKU,1,XTHT(:,:,:))/ MZM(1,IKU,1,XTHT(:,:,:))
  DO JK=1,IKU
   DO JJ=1,IJU
    DO JI=1,IIU
      IF(ZWORK32(JI,JJ,JK)<0.) THEN
        ZWORK31(JI,JJ,JK)= -1.*SQRT( ABS( XG*ZWORK32(JI,JJ,JK)/ XDZZ(JI,JJ,JK) ) )
      ELSE
        ZWORK31(JI,JJ,JK)= SQRT( ABS( XG*ZWORK32(JI,JJ,JK)/ XDZZ(JI,JJ,JK) ) )
      END IF
    ENDDO
   ENDDO
  ENDDO
  !
  YRECFM      ='BV'   
  YCOMMENT    ='X_Y_Z_Brunt-Vaissala frequency (/S)'
  IGRID       =4
  ILENCH      =LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
!  
  IF (NRR > 0) THEN
    ZWORK32(:,:,:)=DZM(1,IKU,1,ZTHETAE(:,:,:))/ MZM(1,IKU,1,ZTHETAE(:,:,:))
    DO JK=1,IKU
     DO JJ=1,IJU
      DO JI=1,IIU
        IF (ZWORK32(JI,JJ,JK)<0.) THEN
          ZWORK31(JI,JJ,JK)= -1.*SQRT( ABS( XG*ZWORK32(JI,JJ,JK)/ XDZZ(JI,JJ,JK) ) )
        ELSE
          ZWORK31(JI,JJ,JK)= SQRT( ABS( XG*ZWORK32(JI,JJ,JK)/ XDZZ(JI,JJ,JK) ) )
        END IF
      ENDDO
     ENDDO
    ENDDO
!
    YRECFM      ='BVE'   
    YCOMMENT    ='X_Y_Z_Equivalent Brunt-Vaissala frequency (/S)'
    IGRID       =4
    ILENCH      =LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
END IF
!
IF(ALLOCATED(ZTHETAE)) DEALLOCATE(ZTHETAE)
IF(ALLOCATED(ZTHETAES)) DEALLOCATE(ZTHETAES)
!-------------------------------------------------------------------------------
!
!* GPS synthetic ZTD, ZHD, ZWD 
!
IF ( NGPS>=0 ) THEN
  !  surface temperature
  ZGAMREF=-6.5E-3
  ZWORK21(:,:) = ZTEMP(:,:,IKB) - ZGAMREF*((XZZ(:,:,IKB)+XZZ(:,:,IKB+1))/2.-XZS(:,:))
  !
  YFGRI=ADJUSTL(ADJUSTR(HFMFILE)//'GPS')
  CALL GPS_ZENITH (YFGRI,XRT(:,:,:,1),ZTEMP,XPABST,ZWORK21,ZWORK22,ZWORK23,ZWORK24)    
  !
  YRECFM       ='ZTD'
  YCOMMENT     ='X_Y_Z_Zenithal Total Delay (m)'
  IGRID        =1
  ILENCH       =LEN(YCOMMENT)
  CALL FMWRIT (HFMFILE,YRECFM,CLUOUT,'XY',ZWORK22,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  IF (NGPS>=1) THEN
    YRECFM       ='ZHD'
    YCOMMENT     ='X_Y_Z_Zenithal Hydrostatic Delay (m)'
    IGRID        =1
    ILENCH       =LEN(YCOMMENT)
    CALL FMWRIT (HFMFILE,YRECFM,CLUOUT,'XY',ZWORK23,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    YRECFM       ='ZWD'
    YCOMMENT     ='X_Y_Z_Zenithal Wet Delay (m)'
    IGRID        =1
    ILENCH       =LEN(YCOMMENT)
    CALL FMWRIT (HFMFILE,YRECFM,CLUOUT,'XY',ZWORK24,IGRID,ILENCH,YCOMMENT,IRESP)
    !
  END IF
  !
END IF
!
!-------------------------------------------------------------------------------
!
!* Radar reflectivities
!
IF(LRADAR .AND. LUSERR) THEN
! CASE  PREP_REAL_CASE after arome
  IF (CCLOUD=='NONE') THEN
    DEALLOCATE(XCIT)
    ALLOCATE(XCIT(IIU,IJU,IKU))    
    XCIT(:,:,:)=800.
    CALL INI_RADAR('PLAT')
  ELSE IF (CCLOUD=='LIMA') THEN
    DEALLOCATE(XCIT)
    ALLOCATE(XCIT(IIU,IJU,IKU))    
    XCIT(:,:,:)=XSVT(:,:,:,NSV_LIMA_NI)
    CALL INI_RADAR('PLAT')
  END IF
!       
  IF (NVERSION_RAD == 1) THEN 
! original version of radar diagnostics 
      WRITE(ILUOUT0,*) 'radar diagnostics from RADAR_RAIN_ICE routine'
  IF (CCLOUD=='LIMA') THEN
  CALL RADAR_RAIN_ICE (XRT, XCIT, XRHODREF, ZTEMP, ZWORK31, ZWORK32, &
                       ZWORK33, ZWORK34,XSVT(:,:,:,NSV_LIMA_NR) )
  ELSE          
  CALL RADAR_RAIN_ICE (XRT, XCIT, XRHODREF, ZTEMP, ZWORK31, ZWORK32, &
                                                         ZWORK33, ZWORK34 )
  ENDIF                                                
!
  YRECFM       ='RARE'
  YCOMMENT     ='X_Y_Z_RAdar REflectivity (dBZ)'
  IGRID        =1
  ILENCH       =LEN(YCOMMENT)
  CALL FMWRIT (HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM       ='VDOP'
  YCOMMENT     ='X_Y_Z_radar DOPpler fall speed (M/S)'
  IGRID        =1
  ILENCH       =LEN(YCOMMENT)
  CALL FMWRIT (HFMFILE,YRECFM,CLUOUT,'XY',ZWORK32,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM       ='ZDR'
  YCOMMENT     ='X_Y_Z_Differential polar Reflectivity (dBZ)'
  IGRID        =1
  ILENCH       =LEN(YCOMMENT)
  CALL FMWRIT (HFMFILE,YRECFM,CLUOUT,'XY',ZWORK33,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM       ='KDP'
  YCOMMENT     ='X_Y_Z_Differential Phase Reflectivity (degree/km)'
  IGRID        =1
  ILENCH       =LEN(YCOMMENT)
  CALL FMWRIT (HFMFILE,YRECFM,CLUOUT,'XY',ZWORK34,IGRID,ILENCH,YCOMMENT,IRESP)
!
   ELSE 
    !
    WRITE(ILUOUT0,*) 'radar diagnostics from RADAR_SIMULATOR routine'
    
    NBRAD=COUNT(XLAT_RAD(:) /= XUNDEF)
    NMAX=INT(NBSTEPMAX*XSTEP_RAD/XGRID)
    IF(NBSTEPMAX*XSTEP_RAD/XGRID/=NMAX .AND. (LCART_RAD)) THEN
      WRITE(ILUOUT0,*) 'NBSTEPMAX*XSTEP_RAD/XGRID is not an integer; please choose another combination'
      CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
      CALL ABORT
      STOP
    ENDIF
    DO JI=1,NBRAD
       NBELEV(JI)=COUNT(XELEV(JI,:) /= XUNDEF)
       WRITE(ILUOUT0,*) 'Number of ELEVATIONS : ', NBELEV(JI), 'FOR RADAR:', JI
    END DO
    IIELV=MAXVAL(NBELEV(1:NBRAD))
    WRITE(ILUOUT0,*) 'Maximum number of ELEVATIONS',IIELV
    WRITE(ILUOUT0,*) 'YOU HAVE ASKED FOR ', NBRAD, 'RADARS'
    !
    IF (LCART_RAD) NBAZIM=8*NMAX  ! number of azimuths 
    WRITE(ILUOUT0,*) ' Number of AZIMUTHS : ', NBAZIM
    IF (LCART_RAD) THEN
      ALLOCATE(ZWORK43(NBRAD,4*NMAX,2*NMAX))
    ELSE
      ALLOCATE(ZWORK43(1,NBAZIM,1))
    END IF
!! Some controls...
    IF(NBRAD/=COUNT(XLON_RAD(:) /= XUNDEF).OR.NBRAD/=COUNT(XALT_RAD(:) /= XUNDEF).OR. &
       NBRAD/=COUNT(XLAM_RAD(:) /= XUNDEF).OR.NBRAD/=COUNT(XDT_RAD(:) /= XUNDEF).OR. &
       NBRAD/=COUNT(CNAME_RAD(:) /= "UNDEF")) THEN
      WRITE(ILUOUT0,*) "Error: inconsistency in DIAG1.nam."
 !callabortstop
      CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
      CALL ABORT
      STOP
    END IF
    IF(NCURV_INTERPOL==0.AND.(LREFR.OR.LDNDZ)) THEN
      LREFR=.FALSE.
      LDNDZ=.FALSE.
      WRITE(ILUOUT0,*) "Warning: cannot output refractivity nor its vertical gradient when NCURV_INTERPOL=0"
    END IF
    IF(MOD(NPTS_H,2)==0) THEN
      NPTS_H=NPTS_H+1
      WRITE(ILUOUT0,*) "Warning: NPTS_H has to be ODD. Setting it to ",NPTS_H
    END IF
    IF(MOD(NPTS_V,2)==0) THEN
      NPTS_V=NPTS_V+1
      WRITE(ILUOUT0,*) "Warning: NPTS_V has to be ODD. Setting it to ",NPTS_V
    END IF
    IF(LWBSCS.AND.LWREFL) THEN
      LWREFL=.FALSE.
      WRITE(ILUOUT0,*) "Warning: LWREFL cannot be set to .TRUE. if LWBSCS is also set to .TRUE.. Setting LWREFL to .FALSE.."
    END IF
    IF(CCLOUD=="LIMA" .AND. NDIFF/=7) THEN
      WRITE(ILUOUT0,*) " ERROR : NDIFF=",NDIFF," not available with CCLOUD=LIMA"
      CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
      CALL ABORT
      STOP
    END IF
    INBOUT=28 !28: Temperature + RHR, RHS, RHG, ZDA, ZDS, ZDG, KDR, KDS, KDG      
    IF (CCLOUD=='LIMA') INBOUT=INBOUT+1 ! rain concentration CRT
    IF(LREFR) INBOUT=INBOUT+1 !+refractivity
    IF(LDNDZ) INBOUT=INBOUT+1 !+refractivity vertical gradient 
    IF(LATT)  INBOUT=INBOUT+12 !+AER-AEG AVR-AVG (vertical specific attenuation) and ATR-ATG  
    WRITE(ILUOUT0,*) "Nombre de variables dans ZWORK42 en sortie de radar_simulator:",INBOUT

    IF (LCART_RAD) THEN
      ALLOCATE(ZWORK42(NBRAD,IIELV,2*NMAX,2*NMAX,INBOUT))
    ELSE
      ALLOCATE(ZWORK42(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,INBOUT))
      ALLOCATE(ZWORK42_BIS(NBRAD,IIELV,NBAZIM,NBSTEPMAX+1,INBOUT))
    END IF
    !
    IF (CCLOUD=='LIMA') THEN
      CALL RADAR_SIMULATOR(XUT,XVT,XWT,XRT,XSVT(:,:,:,NSV_LIMA_NI),XRHODREF,&
                      ZTEMP,XPABST,ZWORK42,ZWORK43,XSVT(:,:,:,NSV_LIMA_NR))
    ELSE ! ICE3
      CALL RADAR_SIMULATOR(XUT,XVT,XWT,XRT,XCIT,XRHODREF,ZTEMP,XPABSM,ZWORK42,ZWORK43)      
    ENDIF
    ALLOCATE(YRAD(INBOUT))
    YRAD(1:9)=(/"ZHH","ZDR","KDP","CSR","ZER","ZEI","ZES","ZEG","VRU"/)
    ICURR=10
    IF(LATT) THEN
      YRAD(ICURR:ICURR+11)=(/"AER","AEI","AES","AEG","AVR","AVI","AVS","AVG","ATR","ATI","ATS","ATG"/)
      ICURR=ICURR+12
    END IF
    YRAD(ICURR:ICURR+2)=(/"RHV","PDP","DHV"/)
    ICURR=ICURR+3
    YRAD(ICURR:ICURR+8)=(/"RHR","RHS","RHG","ZDA","ZDS","ZDG","KDR","KDS","KDG"/)
    ICURR=ICURR+9
    YRAD(ICURR:ICURR+6)=(/"HAS","M_R","M_I","M_S","M_G","CIT","TEM"/)
    ICURR=ICURR+7
    IF (CCLOUD=='LIMA') THEN
      YRAD(ICURR)="CRT"
      ICURR=ICURR+1
    ENDIF
    IF(LREFR) THEN
      YRAD(ICURR)="RFR"
      ICURR=ICURR+1
    END IF
    IF(LDNDZ) THEN
      YRAD(ICURR)="DNZ"
      ICURR=ICURR+1
    END IF
    IF (LCART_RAD) THEN
      DO JI=1,NBRAD
        IEL=NBELEV(JI)
        ! writing latlon in internal files 
        ALLOCATE(CLATLON(2*NMAX))
        CLATLON=""
        DO JV=2*NMAX,1,-1
          DO JH=1,2*NMAX
            WRITE(CBUFFER,'(2(f8.3,1X))') ZWORK43(JI,2*JH-1,JV),ZWORK43(JI,2*JH,JV)
                  CLATLON(JV)=TRIM(CLATLON(JV)) // " " // TRIM(CBUFFER)
          END DO
          CLATLON(JV)=TRIM(ADJUSTL(CLATLON(JV)))
        END DO            
        DO JEL=1,IEL
          WRITE(YELEV,'(I2.2,A1,I1.1)') FLOOR(XELEV(JI,JEL)),'.',&
                 INT(ANINT(10.*XELEV(JI,JEL))-10*INT(XELEV(JI,JEL)))
          WRITE(YGRID_SIZE,'(I3.3)') 2*NMAX
          DO JJ=1,SIZE(ZWORK42(:,:,:,:,:),5)
            YRS=YRAD(JJ)//CNAME_RAD(JI)(1:3)//YELEV//YGRID_SIZE//HFMFILE
            CALL OPEN_ll(UNIT=ILURS,FILE=YRS,IOSTAT=IRESP,STATUS="NEW",ACTION='WRITE', &
                         FORM="FORMATTED",RECL=8192)
            WRITE(ILURS,'(A,4F12.6,2I5)') '**domaine LATLON ',ZWORK43(JI,1,1),ZWORK43(JI,4*NMAX-1,2*NMAX), &
                  ZWORK43(JI,2,1),ZWORK43(JI,4*NMAX,2*NMAX),2*NMAX,2*NMAX !! HEADER
            DO JV=2*NMAX,1,-1
              DO JH=1,2*NMAX
                WRITE(ILURS,'(E11.5,1X)',ADVANCE='NO') ZWORK42(JI,JEL,JH,JV,JJ)
              END DO
              WRITE(ILURS,*) ''
            END DO
                  
            DO JV=2*NMAX,1,-1
              WRITE(ILURS,*) CLATLON(JV)
            END DO                  
            CALL CLOSE_ll(HFILE=YRS)
          END DO               
        END DO
        DEALLOCATE(CLATLON)
      END DO
    ELSE ! polar output
       CALL MPI_ALLREDUCE(ZWORK42, ZWORK42_BIS, SIZE(ZWORK42), MPI_PRECISION, MPI_MAX, NMNH_COMM_WORLD, IERR)
      DO JI=1,NBRAD
        IEL=NBELEV(JI)
        DO JEL=1,IEL
          WRITE(YELEV,'(I2.2,A1,I1.1)') FLOOR(XELEV(JI,JEL)),'.',&
                INT(ANINT(10.*XELEV(JI,JEL))-10*INT(XELEV(JI,JEL)))
          DO JJ=1,SIZE(ZWORK42(:,:,:,:,:),5)
            YRS="P"//YRAD(JJ)//CNAME_RAD(JI)(1:3)//YELEV//HFMFILE
            CALL OPEN_ll(UNIT=ILURS,FILE=YRS,IOSTAT=IRESP,ACTION='WRITE',MODE=GLOBAL)
            DO JH=1,NBAZIM
              DO JV=1,NBSTEPMAX+1
                WRITE(ILURS,"(F15.7)") ZWORK42_BIS(JI,JEL,JH,JV,JJ)
              END DO
            END DO                                    
            CALL CLOSE_ll(HFILE=YRS)
          END DO
        END DO
      END DO
    END IF !polar output
    DEALLOCATE(ZWORK42,ZWORK43)
   END IF
END IF
!
IF (LLIDAR) THEN
  PRINT *,'CALL LIDAR/RADAR with HFMFILE =',HFMFILE
  YVIEW='     '
  YVIEW=TRIM(CVIEW_LIDAR)
  PRINT *,'CVIEW_LIDAR REQUESTED ',YVIEW
  IF (YVIEW/='NADIR'.AND.YVIEW/='ZENIT') YVIEW='NADIR'
  PRINT *,'CVIEW_LIDAR USED ',YVIEW
  PRINT *,'XALT_LIDAR REQUESTED (m) ',XALT_LIDAR
  PRINT *,'XWVL_LIDAR REQUESTED (m) ',XWVL_LIDAR
  IF (XWVL_LIDAR==XUNDEF) XWVL_LIDAR=0.532E-6
  IF (XWVL_LIDAR<1.E-7.OR.XWVL_LIDAR>2.E-6) THEN
    PRINT *,'CAUTION: THE XWVL_LIDAR REQUESTED IS OUTSIDE THE USUAL RANGE'
    XWVL_LIDAR=0.532E-6
  ENDIF
  PRINT *,'XWVL_LIDAR USED (m) ',XWVL_LIDAR
!
  IF (LDUST) THEN
    IACCMODE=MIN(2,NMODE_DST)
    ALLOCATE(ZTMP1(SIZE(XSVT,1), SIZE(XSVT,2), SIZE(XSVT,3), 1))
    ALLOCATE(ZTMP2(SIZE(XSVT,1), SIZE(XSVT,2), SIZE(XSVT,3), 1))
    ALLOCATE(ZTMP3(SIZE(XSVT,1), SIZE(XSVT,2), SIZE(XSVT,3), 1))
    ZTMP1(:,:,:,1)=ZN0_DST(:,:,:,IACCMODE)
    ZTMP2(:,:,:,1)=ZRG_DST(:,:,:,IACCMODE)
    ZTMP3(:,:,:,1)=ZSIG_DST(:,:,:,IACCMODE)
    SELECT CASE ( CCLOUD )
    CASE('KESS','ICE2','ICE3','ICE4')
      CALL LIDAR(CCLOUD, YVIEW, XALT_LIDAR, XWVL_LIDAR, XZZ, XRHODREF, XCLDFR, &
                 XRT, ZWORK31, ZWORK32,                                        &
                 PDSTC=ZTMP1,                                                  &
                 PDSTD=ZTMP2,                                                  &
                 PDSTS=ZTMP3)
    CASE('C2R2')
      CALL LIDAR(CCLOUD, YVIEW, XALT_LIDAR, XWVL_LIDAR, XZZ, XRHODREF, XCLDFR, &
                 XRT, ZWORK31, ZWORK32,                                        &
                 PCT=XSVT(:,:,:,NSV_C2R2BEG+1:NSV_C2R2END),                    &
                 PDSTC=ZTMP1,                                                  &
                 PDSTD=ZTMP2,                                                  &
                 PDSTS=ZTMP3)
    CASE('C3R5')
      CALL LIDAR(CCLOUD, YVIEW, XALT_LIDAR, XWVL_LIDAR, XZZ, XRHODREF, XCLDFR, &
                 XRT, ZWORK31, ZWORK32,                                        &
                 PCT=XSVT(:,:,:,NSV_C2R2BEG+1:NSV_C1R3END-1),                  &
                 PDSTC=ZTMP1,                                                  &
                 PDSTD=ZTMP2,                                                  &
                 PDSTS=ZTMP3)
    CASE('LIMA')
! PCT(2) = droplets (3)=drops (4)=ice crystals
       ALLOCATE(ZTMP4(SIZE(XSVT,1), SIZE(XSVT,2), SIZE(XSVT,3), 4))
       ZTMP4(:,:,:,1)=0.
       ZTMP4(:,:,:,2)=XSVT(:,:,:,NSV_LIMA_NC)
       ZTMP4(:,:,:,3)=XSVT(:,:,:,NSV_LIMA_NR)
       ZTMP4(:,:,:,4)=XSVT(:,:,:,NSV_LIMA_NI)
!
       CALL LIDAR(CCLOUD, YVIEW, XALT_LIDAR, XWVL_LIDAR, XZZ, XRHODREF, XCLDFR,&
            XRT, ZWORK31, ZWORK32,                                  &
            PCT=ZTMP4,                            &
            PDSTC=ZTMP1,                          &
            PDSTD=ZTMP2,                          &
            PDSTS=ZTMP3)
!
    END SELECT
  ELSE
    SELECT CASE ( CCLOUD )
    CASE('KESS','ICE2','ICE3','ICE4')
      CALL LIDAR(CCLOUD, YVIEW, XALT_LIDAR, XWVL_LIDAR, XZZ, XRHODREF, XCLDFR, &
           XRT, ZWORK31, ZWORK32)
    CASE('C2R2')
      CALL LIDAR(CCLOUD, YVIEW, XALT_LIDAR, XWVL_LIDAR, XZZ, XRHODREF, XCLDFR, &
           XRT, ZWORK31, ZWORK32,                                  &
           PCT=XSVT(:,:,:,NSV_C2R2BEG+1:NSV_C2R2END))
    CASE('C3R5')
      CALL LIDAR(CCLOUD, YVIEW, XALT_LIDAR, XWVL_LIDAR, XZZ, XRHODREF, XCLDFR, &
           XRT, ZWORK31, ZWORK32,                                  &
           PCT=XSVT(:,:,:,NSV_C2R2BEG+1:NSV_C1R3END-1))
    CASE('LIMA')
! PCT(2) = droplets (3)=drops (4)=ice crystals
       ALLOCATE(ZTMP4(SIZE(XSVT,1), SIZE(XSVT,2), SIZE(XSVT,3), 4))
       ZTMP4(:,:,:,1)=0.
       ZTMP4(:,:,:,2)=XSVT(:,:,:,NSV_LIMA_NC)
       ZTMP4(:,:,:,3)=XSVT(:,:,:,NSV_LIMA_NR)
       ZTMP4(:,:,:,4)=XSVT(:,:,:,NSV_LIMA_NI)
!
       CALL LIDAR(CCLOUD, YVIEW, XALT_LIDAR, XWVL_LIDAR, XZZ, XRHODREF, XCLDFR,&
            XRT, ZWORK31, ZWORK32,                                  &
            PCT=ZTMP4)
    END SELECT
  ENDIF
!
  IF( ALLOCATED(ZTMP1) ) DEALLOCATE(ZTMP1)
  IF( ALLOCATED(ZTMP2) ) DEALLOCATE(ZTMP2)
  IF( ALLOCATED(ZTMP3) ) DEALLOCATE(ZTMP3)
  IF( ALLOCATED(ZTMP4) ) DEALLOCATE(ZTMP4)
!
  YRECFM       ='LIDAR'
  YCOMMENT     ='X_Y_Z_Normalized_Lidar_Profile (1/m/sr)'
  IGRID        =1
  ILENCH       =LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM       ='LIPAR'
  YCOMMENT     ='X_Y_Z_Particle_Lidar_Profile (1/m/sr)'
  IGRID        =1
  ILENCH       =LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK32,IGRID,ILENCH,YCOMMENT,IRESP)
!
END IF
!
!-------------------------------------------------------------------------------
!
!* Height of boundary layer
!
IF (LBLTOP) THEN
  ZGAMREF=3.5E-3 ! K/m
  ZWORK31(:,:,1:IKU-1)=0.5*(XZZ(:,:,1:IKU-1)+XZZ(:,:,2:IKU))
  ZWORK31(:,:,IKU)=2.*ZWORK31(:,:,IKU-1)-ZWORK31(:,:,IKU-2)
  YFMFILE=CINIFILE
  CINIFILE=HFMFILE
  CALL FREE_ATM_PROFILE(ZTHETAV,ZWORK31,XZS,XZSMT,ZGAMREF,ZWORK32,ZWORK33)
  CINIFILE=YFMFILE
END IF
!
IF (ALLOCATED(ZTHETAV)) DEALLOCATE(ZTHETAV)
!
!
!* Ligthning
!
IF ( LCH_CONV_LINOX ) THEN 
  YRECFM      = 'IC_RATE'
  YCOMMENT    = 'X_Y_IC_RATE (/s)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XIC_RATE,IGRID,ILENCH,YCOMMENT,IRESP)

  YRECFM      = 'CG_RATE'
  YCOMMENT    = 'X_Y_CG_RATE (/s)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XCG_RATE,IGRID,ILENCH,YCOMMENT,IRESP)

  YRECFM      = 'IC_TOTAL_NB'
  YCOMMENT    = 'X_Y_IC_TOTAL_NUMBER (no unit)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XIC_TOTAL_NUMBER,IGRID,ILENCH, &
       YCOMMENT,IRESP)

  YRECFM      = 'CG_TOTAL_NB'
  YCOMMENT    = 'X_Y_CG_TOTAL_NUMBER (no unit)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XCG_TOTAL_NUMBER,IGRID,ILENCH, &
       YCOMMENT,IRESP)
END IF
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!*       1.8    My own  variables :
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_LFIFM1_FOR_DIAG  
