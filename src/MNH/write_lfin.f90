!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_7 BUG1 2007/06/20 16:58:20
!-----------------------------------------------------------------
!     #########################
      MODULE MODI_WRITE_LFIFM_n
!     #########################
!
INTERFACE
!
SUBROUTINE WRITE_LFIFM_n(HFMFILE,HDADFILE)
CHARACTER(LEN=28), INTENT(IN) :: HFMFILE      ! Name of FM-file to write
CHARACTER(LEN=28), INTENT(IN) :: HDADFILE     ! corresponding FM-file name of 
                                              ! its DAD model
END SUBROUTINE WRITE_LFIFM_n
!
END INTERFACE
!
END MODULE MODI_WRITE_LFIFM_n
!
!
!     ##########################################
      SUBROUTINE WRITE_LFIFM_n(HFMFILE,HDADFILE)
!     ##########################################
!
!!****  *WRITE_LFIFM_n* - routine to write a LFIFM file for model $n
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
!!      WRITE_BALLOON_n : routine to write balloon records
!!      WRITE_LB_n : routine to write LB fields
!!      FMWRIT     : FM-routine to write a record
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_DIM_n   : contains dimensions
!!      Module MODD_TIME    : contains time variables for all models
!!      Module MODD_TIME_n   : contains time variables 
!!      Module MODD_GRID    : contains spatial grid variables for all models
!!      Module MODD_GRID_n : contains spatial grid variables
!!      Module MODD_REF     : contains reference state variables
!!      Module MODD_LUNIT_n: contains logical unit variables.
!!      Module MODD_CONF    : contains configuration variables for all models
!!      Module MODD_CONF_n  : contains configuration variables
!!      Module MODD_FIELD_n  : contains prognostic variables
!!      Module MODD_GR_FIELD_n : contains surface prognostic variables
!!      Module MODD_LSFIELD_n  : contains Larger Scale variables
!!      Module MODD_PARAM_n    : contains parameterization options
!!      Module MODD_TURB_n    : contains turbulence options
!!      Module MODD_FRC    : contains forcing variables
!!      Module MODD_DEEP_CONVECTION_n : contains deep convection tendencies
!!      Module MODD_PARAM_KAFR_n : contains configuration
!!      Module MODD_AIRCRAFT_BALLOON : contains balloon and aircraft variables
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
!!       J.Stein       23/01/95 add a TKE switch and MODD_PARAM_n
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
!!       V.Masson      08/10/96 add LTHINSHELL
!!       J.-P. Pinty   15/12/96 add the microphysics (ice)
!!       J.-P. Pinty   11/01/97 add the deep convection
!!       J.-P. Pinty   27/01/97 split the recording of the SV array
!!       J.-P. Pinty   29/01/97 set recording of PRCONV and PACCONV in mm/h and
!!                                                         mm respectively
!!       J. Viviand    04/02/97 convert precipitation rates in mm/h
!!       J.P. Lafore   25/11/96 resolution ratio and position for nesting
!!       J.P. Lafore   26/02/97 adding of "surfacic" LS fields
!!       J.Stein       22/06/97 use the absolute pressure
!!       V.Masson      09/07/97 add directional z0 and Subgrid-Scale Orography
!!       V.Masson      18/08/97 call to fmwrit directly with dates and strings
!!       J.Stein       22/10/97 add the LB fields for U,V,W, THETA, RV....
!!       P.Bechtold    24/01/98 add convective tracer tendencies
!!       P.Jabouille   15/10/98 //
!!       P.Jabouille   25/05/99 replace 'DTRAD_CLONLY' by 'DTRAD_CLLY' (size too long)
!!       J. Stein      20/05/98 remove NXEND and NYEND
!!       V. Masson     04/01/00 remove TSZ0 option
!!       P. Jabouille  03/04/00 write XCIT only for MESONH program
!!       K. Suhre      03/12/99 add chemical variable names                         
!        F.solmon /V.Masson   06/00 adapt for patch surface variables
!!       D.Gazen       22/01/01 use MODD_NSV and add names to scalar variables
!!       G.Jaubert     06/06/01 add Balloon current positions
!!       P.Jabouille   10/04/02 extra radiative surface flux
!!       J.-P. Pinty   29/11/02 add C3R5, ICE2, ICE4, CELEC
!!       V. Masson     01/2004  removes surface (externalization)
!!                     05/2006  Remove KEPS
!!       J. escobar    02/09/2009 missing YDIR for CLDFR variable
!!                     October 2009 (G. Tanguy) add ILENCH=LEN(YCOMMENT) after
!!                                              change of YCOMMENT
!!       P. Aumond     12/2009 Mean_UM,...
!!       M. Leriche    16/07/10 add ice phase chemical species
!!       C. Barthe     Jan. 2011  add diagnostics for elec
!!       J. Escobar    Feb. 2012  replace MINVAL/MAXVAL by MIN_ll/MAX_ll in OUTPUT_LISTING
!!       P.Peyrille    06/12 2D west african monsoon: ADV forcing and fluxes writing
!!                     AEROSOLS and ozone vertical distribution are also written
!!       M.Tomasini    06/12 2D west african monsoon: nesting for ADV forcing writing
!!       Pialat/Tulet  15/02/2012 add ForeFire variables
!!       J. Escobar    Mars 2014 , missing YDIR="XY" in 1.6 for tendencies fields 
!!                   
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
USE MODD_TIME
USE MODD_TIME_n
USE MODD_FIELD_n
USE MODD_MEAN_FIELD_n
USE MODD_DUMMY_GR_FIELD_n
USE MODD_LSFIELD_n
USE MODD_DYN_n
USE MODD_PARAM_n
USE MODD_REF
USE MODD_LUNIT_n
USE MODD_TURB_n
USE MODD_RADIATIONS_n,   ONLY : XDTHRAD, NCLEARCOL_TM1, XFLALWD, &
                                XZENITH, XDIR_ALB, XSCA_ALB, XEMIS, XTSRAD, &
                                XDIRSRFSWD, XSCAFLASWD, XDIRFLASWD, XAZIM
USE MODD_REF_n,  ONLY : XRHODREF
USE MODD_FRC
USE MODD_PRECIP_n
USE MODD_ELEC_n
USE MODD_CST
USE MODD_CLOUDPAR
USE MODD_DEEP_CONVECTION_n
USE MODD_PARAM_KAFR_n
USE MODD_NESTING
USE MODD_PARAMETERS
USE MODD_GR_FIELD_n
USE MODD_CH_MNHC_n,       ONLY: LUSECHEM,LCH_CONV_LINOX, &
                                LUSECHAQ,LUSECHIC,LCH_PH
USE MODD_CH_PH_n
USE MODD_CH_M9_n
USE MODD_RAIN_C2R2_DESCR, ONLY: C2R2NAMES
USE MODD_ICE_C1R3_DESCR,  ONLY: C1R3NAMES
USE MODD_ELEC_DESCR,      ONLY: CELECNAMES
USE MODD_LG,              ONLY: CLGNAMES
USE MODD_NSV
USE MODD_AIRCRAFT_BALLOON
USE MODD_HURR_CONF, ONLY: LFILTERING,CFILTERING,NDIAG_FILT
USE MODD_HURR_FIELD_n
USE MODD_PREP_REAL, ONLY: CDUMMY_2D, XDUMMY_2D
USE MODD_DUST
USE MODD_SALT
USE MODD_PASPOL
#ifdef MNH_FOREFIRE
USE MODD_FOREFIRE
#endif
USE MODD_CONDSAMP
USE MODD_CH_AEROSOL
!
USE MODE_FMWRIT
USE MODE_ll
USE MODE_IO_ll, ONLY: UPCASE,CLOSE_ll
USE MODE_GRIDPROJ
USE MODE_MODELN_HANDLER
!
USE MODI_GATHER_ll
USE MODI_WRITE_LB_n
USE MODI_WRITE_BALLOON_n
USE MODI_DUSTLFI_n
USE MODI_SALTLFI_n
USE MODI_CH_AER_REALLFI_n
!
#ifdef MNH_NCWRIT
USE MODN_NCOUT
USE MODE_UTIL
#endif
!
! Modif Eddy fluxes
USE MODD_DEF_EDDY_FLUX_n       ! Ajout PP
USE MODD_DEF_EDDYUV_FLUX_n     ! Ajout PP
USE MODD_LATZ_EDFLX            ! Ajout PP
!
USE MODD_2D_FRC                  ! Ajout PP
USE MODD_ADVFRC_n              ! Modif PP ADV FRC
USE MODD_RELFRC_n
!
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
INTEGER           :: ILUOUT         ! logical unit
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears 
                                    !in LFI subroutines at the open of the file              
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string 
!
CHARACTER(LEN=16) :: YRECFM         ! Name of the article to be written
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER (LEN=2) :: YDIR           ! Type of the data field
!
INTEGER           :: IRR            ! Index for moist variables
INTEGER           :: JSV            ! loop index for scalar variables
INTEGER           :: JSA            ! beginning of chemical-aerosol variables

! 
CHARACTER(LEN=3)  :: YFRC           ! to mark the time of the forcing
INTEGER           :: JT             ! loop index
!
INTEGER           :: JMOM, IMOMENTS, JMODE, ISV_NAME_IDX  ! dust modes
! 
REAL,DIMENSION(:,:), ALLOCATABLE  :: ZWORK2D     ! Working array
REAL,DIMENSION(:,:,:), ALLOCATABLE  :: ZWORK3D     ! Working array
!
REAL                              :: ZLATOR, ZLONOR ! geographical coordinates of 1st mass point
REAL                              :: ZXHATM, ZYHATM ! conformal    coordinates of 1st mass point
REAL, DIMENSION(:), ALLOCATABLE   :: ZXHAT_ll    !  Position x in the conformal
                                                 ! plane (array on the complete domain)
REAL, DIMENSION(:), ALLOCATABLE   :: ZYHAT_ll    !   Position y in the conformal
                                                 ! plane (array on the complete domain)
INTEGER :: IMI ! Current model index
!
INTEGER           :: ICH_NBR        ! to write number and names of scalar 
INTEGER,DIMENSION(:),ALLOCATABLE :: ICH_NAMES !(chem+aero+dust) variables
CHARACTER(LEN=16),DIMENSION(:),ALLOCATABLE :: YDSTNAMES,YCHNAMES, YSLTNAMES
INTEGER           :: ILREC,ILENG    !in NSV.DIM and NSV.TITRE
INTEGER           :: INFO_ll
INTEGER :: IKRAD
INTEGER           :: JI,JJ,JK   ! loop index
INTEGER           :: IIU,IJU,IKU,IIB,IJB,IKB,IIE,IJE,IKE ! Arrays bounds
!-------------------------------------------------------------------------------
!
!*	0. Initialization
!
IMI = GET_CURRENT_MODEL_INDEX()
!
CALL FMLOOK_ll(CLUOUT,CLUOUT,ILUOUT,IRESP)
!
ALLOCATE(ZWORK2D(SIZE(XTHT,1),SIZE(XTHT,2)))
ALLOCATE(ZWORK3D(SIZE(XTHT,1),SIZE(XTHT,2),SIZE(XTHT,3)))
!
!
!*       0.2     ARRAYS BOUNDS INITIALIZATION
!
IIU=NIMAX+2*JPHEXT
IJU=NJMAX+2*JPHEXT
IKU=NKMAX+2*JPVEXT
IIB=1+JPHEXT
IJB=1+JPHEXT
IKB=1+JPVEXT
IIE=IIU-JPHEXT
IJE=IJU-JPHEXT
IKE=IKU-JPVEXT
!
!*       1.     WRITES IN THE LFI FILE
! 
!
!*       1.0    HFMFILE and HDADFILE writing :
!
YDIR='--'
!
YRECFM='MASDEV'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,NMASDEV,IGRID,ILENCH,YCOMMENT,IRESP)
! 
YRECFM='BUGFIX'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,NBUGFIX,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='BIBUSER'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,CBIBUSER,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='PROGRAM'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,CPROGRAM,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
YRECFM='MY_NAME'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,HFMFILE,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='DAD_NAME'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,HDADFILE,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (LEN_TRIM(HDADFILE)>0) THEN
  CALL FMWRIT(HFMFILE,'DXRATIO',CLUOUT,YDIR,NDXRATIO_ALL(IMI),0,ILENCH,YCOMMENT,IRESP)
  CALL FMWRIT(HFMFILE,'DYRATIO',CLUOUT,YDIR,NDYRATIO_ALL(IMI),0,ILENCH,YCOMMENT,IRESP)
  CALL FMWRIT(HFMFILE,'XOR' ,CLUOUT,YDIR,NXOR_ALL(IMI) ,0,ILENCH,YCOMMENT,IRESP)
  CALL FMWRIT(HFMFILE,'YOR' ,CLUOUT,YDIR,NYOR_ALL(IMI) ,0,ILENCH,YCOMMENT,IRESP)
END IF
!
!*       1.1    Type and Dimensions :
!
YRECFM='STORAGE_TYPE'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,CSTORAGE_TYPE,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='IMAX'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,NIMAX_ll,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='JMAX'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,NJMAX_ll,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='KMAX'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,NKMAX,IGRID,ILENCH,YCOMMENT,IRESP)
!
!*       1.2    Grid variables :
!
IF (.NOT.LCARTESIAN) THEN
  YRECFM='RPK'
  YCOMMENT=' '
  IGRID=0
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRPK,IGRID,ILENCH,YCOMMENT,IRESP)
! 
  YRECFM='LONORI'
  YCOMMENT='DEGREES'
  IGRID=0
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XLONORI,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='LATORI'
  YCOMMENT='DEGREES'
  IGRID=0
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XLATORI,IGRID,ILENCH,YCOMMENT,IRESP)
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
  YCOMMENT='DEGREES'
  IGRID=0
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZLONOR,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='LATOR'
  YCOMMENT='DEGREES'
  IGRID=0
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZLATOR,IGRID,ILENCH,YCOMMENT,IRESP)
END IF 
!
YRECFM='THINSHELL'
YCOMMENT=' '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,LTHINSHELL,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='LAT0'
YCOMMENT='reference latitude for conformal projection (DEGREES)'
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XLAT0,IGRID,ILENCH,YCOMMENT,IRESP)
! 
YRECFM='LON0'
YCOMMENT='reference longitude for conformal projection (DEGREES)'
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XLON0,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='BETA'
YCOMMENT='rotation angle (DEGREES)'
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XBETA,IGRID,ILENCH,YCOMMENT,IRESP)
! 
YRECFM='XHAT'
YDIR='XX'
YCOMMENT='Position x in the conformal or cartesian plane (METERS)'
IGRID=2
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XXHAT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='YHAT'
YDIR='YY'
YCOMMENT='Position y in the conformal or cartesian plane (METERS)'
IGRID=3
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XYHAT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='ZHAT'
YDIR='--'
YCOMMENT='height level without orography (METERS)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XZHAT,IGRID,ILENCH,YCOMMENT,IRESP)
#ifdef MNH_NCWRIT
IF (LNETCDF.AND..NOT.LCARTESIAN) THEN
 LLFIFM = .FALSE.
 YRECFM='LAT'
 YCOMMENT='X_Y_latitude (degree)'
 IGRID=1
 ILENCH=LEN(YCOMMENT)
 CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XLAT,IGRID,ILENCH,YCOMMENT,IRESP)

 YRECFM='LON'
 YCOMMENT='X_Y_longitude (degree)'
 IGRID=1
 ILENCH=LEN(YCOMMENT)
 CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XLON,IGRID,ILENCH,YCOMMENT,IRESP)
 LLFIFM = .TRUE.
END IF
!
!*SB*MAY2012
!*SB* * WRITE ALT
IF (LNETCDF) THEN
 YRECFM='ALT'
 YCOMMENT='X_Y_Z_ALTitude (M)'
 IGRID=4
 ILENCH=LEN(YCOMMENT)
 CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XZZ,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
#endif 
!
YRECFM='ZS'
YDIR='XY'
YCOMMENT='orography (METERS)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XZS,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='ZSMT'
YDIR='XY'
YCOMMENT='smooth orography (METERS)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XZSMT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='SLEVE'
YDIR='--'
YCOMMENT=''
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,LSLEVE,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (LSLEVE) THEN
  YRECFM='LEN1'
  YDIR='--'
  YCOMMENT=''
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XLEN1,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='LEN2'
  YDIR='--'
  YCOMMENT=''
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XLEN2,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
YDIR='--'
!
YRECFM='DTCUR'
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,TDTCUR,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='DTEXP'
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,TDTEXP,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='DTMOD'
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,TDTMOD,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='DTSEG'
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,TDTSEG,IGRID,ILENCH,YCOMMENT,IRESP)
!
!*       1.3    Configuration  variables :
!
YRECFM='L1D'
YCOMMENT='  '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,L1D,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='L2D'
YCOMMENT='  '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,L2D,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='PACK'
YCOMMENT='  '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,LPACK,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='CARTESIAN'
YCOMMENT='Logical for cartesian geometry'
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,LCARTESIAN,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='LBOUSS'       
YCOMMENT='Logical for boussinesq'         
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,LBOUSS,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='SURF'
YCOMMENT='  '
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,CSURF,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='CPL_AROME'
YCOMMENT='Logical for arome coupling file'
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,LCPL_AROME,IGRID,ILENCH,YCOMMENT,IRESP)
!
!*       1.4    Prognostic variables :
!
YDIR='XY'
!
!*       1.4.1  Time t:
!   
YRECFM='UT'
YCOMMENT='X_Y_Z_U component of wind (m/s)'
IGRID=2
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XUT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='VT'
YCOMMENT='X_Y_Z_V component of wind (m/s)'
IGRID=3
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XVT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='WT'
YCOMMENT='X_Y_Z_vertical wind (m/s)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XWT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='THT'
YCOMMENT='X_Y_Z_potential temperature (K)'
IGRID=1
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XTHT,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (MEAN_COUNT /= 0) THEN
!
  YRECFM='UMMEAN'
  YCOMMENT='X_Y_Z_U component of mean wind (m/s)'
  IGRID=2
  ILENCH=LEN(YCOMMENT)
  ZWORK3D = XUM_MEAN/MEAN_COUNT
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='U2MEAN'
  YCOMMENT='X_Y_Z_U component of mean wind (m/s)'
  IGRID=2
  ILENCH=LEN(YCOMMENT)
  ZWORK3D = XU2_MEAN/MEAN_COUNT-XUM_MEAN**2/MEAN_COUNT**2
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='VMMEAN'
  YCOMMENT='X_Y_Z_V component of mean wind (m/s)'
  IGRID=3
  ILENCH=LEN(YCOMMENT)
  ZWORK3D = XVM_MEAN/MEAN_COUNT
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='V2MEAN'
  YCOMMENT='X_Y_Z_V component of mean wind (m/s)'
  IGRID=3
  ILENCH=LEN(YCOMMENT)
  ZWORK3D = XV2_MEAN/MEAN_COUNT-XVM_MEAN**2/MEAN_COUNT**2
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='WMMEAN'
  YCOMMENT='X_Y_Z_vertical mean wind (m/s)'
  IGRID=4
  ILENCH=LEN(YCOMMENT)
 ZWORK3D = XWM_MEAN/MEAN_COUNT
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='W2MEAN'
  YCOMMENT='X_Y_Z_vertical mean wind (m/s)'
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  ZWORK3D = XW2_MEAN/MEAN_COUNT-XWM_MEAN**2/MEAN_COUNT**2
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='THMMEAN'
  YCOMMENT='X_Y_Z_mean potential temperature (K)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  ZWORK3D = XTHM_MEAN/MEAN_COUNT
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='TH2MEAN'
  YCOMMENT='X_Y_Z_mean potential temperature (K)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  ZWORK3D = XTH2_MEAN/MEAN_COUNT-XTHM_MEAN**2/MEAN_COUNT**2
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='TEMPMMEAN'
  YCOMMENT='X_Y_Z_mean potential temperature (K)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  ZWORK3D= XTEMPM_MEAN/MEAN_COUNT
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='TEMP2MEAN'
  YCOMMENT='X_Y_Z_mean potential temperature (K)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  ZWORK3D = XTEMP2_MEAN/MEAN_COUNT-XTEMPM_MEAN**2/MEAN_COUNT**2
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='PABSMMEAN'
  YCOMMENT='X_Y_Z_mean ABSolute Pressure (Pa)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  ZWORK3D= XPABSM_MEAN/MEAN_COUNT
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='PABS2MEAN'
  YCOMMENT='X_Y_Z_mean ABSolute Pressure (Pa)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  ZWORK3D = XPABS2_MEAN/MEAN_COUNT-XPABSM_MEAN**2/MEAN_COUNT**2
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='TKEMMEAN'
  YCOMMENT='X_Y_Z_mean ABSolute Pressure (Pa)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  ZWORK3D= XTKEM_MEAN/MEAN_COUNT
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
END IF
!
!
IF (CTURB /= 'NONE') THEN
  YRECFM='TKET'
  YCOMMENT='X_Y_Z_Turbulent Kinetic Energy (M**2/S**2)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XTKET,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
!
!
YRECFM='PABST'
YCOMMENT='X_Y_Z_ABSolute Pressure (Pa)'
IGRID=1
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XPABST,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (NRR >=1) THEN
  IRR=0
  IGRID=1                                    ! individually in file 
  IF (LUSERV) THEN
    IRR   = IRR+1 
    YRECFM= 'RVT'
    YCOMMENT='X_Y_Z_Vapor mixing Ratio (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRT(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
  END IF 
  IF (LUSERC) THEN
    IRR   = IRR+1 
    YRECFM= 'RCT'
    YCOMMENT='X_Y_Z_Cloud mixing Ratio (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRT(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    WRITE (ILUOUT,*) IRR,' RC min-max ',MIN_ll(XRT(:,:,:,IRR),INFO_ll),MAX_ll(XRT(:,:,:,IRR),INFO_ll)
  END IF
  IF (LUSERR) THEN
    IRR   = IRR+1 
    YRECFM= 'RRT'
    YCOMMENT='X_Y_Z_Rain mixing Ratio (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRT(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    WRITE (ILUOUT,*) IRR,' RR min-max ',MIN_ll(XRT(:,:,:,IRR),INFO_ll),MAX_ll(XRT(:,:,:,IRR),INFO_ll)
  END IF 
  IF (LUSERI) THEN
    IRR   = IRR+1 
    YRECFM= 'RIT'
    YCOMMENT='X_Y_Z_Ice mixing Ratio (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRT(:,:,:,IRR),IGRID,ILENCH, &
                YCOMMENT,IRESP)
    WRITE (ILUOUT,*) IRR,' RI min-max ',MIN_ll(XRT(:,:,:,IRR),INFO_ll),MAX_ll(XRT(:,:,:,IRR),INFO_ll)
    IF ( CPROGRAM == 'MESONH' .AND. CCLOUD(1:3) == 'ICE') THEN
      YRECFM= 'CIT'
      YCOMMENT='X_Y_Z_Cloud Ice concentration (/M3)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XCIT(:,:,:),   IGRID,ILENCH,  &
                  YCOMMENT,IRESP)
    END IF
  END IF 
  IF (LUSERS) THEN
    IRR   = IRR+1 
    YRECFM= 'RST'
    YCOMMENT='X_Y_Z_Snow mixing Ratio (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRT(:,:,:,IRR),IGRID,ILENCH, &
                YCOMMENT,IRESP)
    WRITE (ILUOUT,*) IRR,' RS min-max ',MINVAL(XRT(:,:,:,IRR)),MAXVAL(XRT(:,:,:,IRR))
  END IF
  IF (LUSERG) THEN
    IRR   = IRR+1 
    YRECFM= 'RGT'
    YCOMMENT='X_Y_Z_Graupel mixing Ratio (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRT(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    WRITE (ILUOUT,*) IRR,' RG min-max ',MINVAL(XRT(:,:,:,IRR)),MAXVAL(XRT(:,:,:,IRR))
  END IF 
  IF (LUSERH) THEN
    IRR   = IRR+1 
    YRECFM= 'RHT'
    YCOMMENT='X_Y_Z_Hail mixing Ratio (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRT(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
  END IF 
END IF
!
IF (NSV >=1) THEN
  JSA=0
  IGRID=1                                       ! individually in the file
  ! User scalar variables
  DO JSV = 1,NSV_USER
    WRITE(YRECFM,'(A3,I3.3)')'SVT',JSV
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    JSA=JSA+1
  END DO
  ! microphysical C2R2 scheme scalar variables
  DO JSV = NSV_C2R2BEG,NSV_C2R2END
    YRECFM=TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))//'T'
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (/M3)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    JSA=JSA+1
  END DO
  ! microphysical C3R5 scheme additional scalar variables
  DO JSV = NSV_C1R3BEG,NSV_C1R3END
    YRECFM=TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))//'T'
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (/M3)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    JSA=JSA+1
  END DO
  ! electrical scalar variables
  DO JSV = NSV_ELECBEG,NSV_ELECEND
    YRECFM=TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))//'T'
    IF (JSV .GT. NSV_ELECBEG .AND. JSV .LT. NSV_ELECEND) THEN 
      WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (C/m3)'
    ELSE
      WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (nb ions/m3)'
    END IF
    ILENCH=LEN(YCOMMENT)
    ZWORK3D(:,:,:) = 0.
    ZWORK3D(:,:,:) = XSVT(:,:,:,JSV) * XRHODREF(:,:,:) ! C/kg --> C/m3
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D(:,:,:),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    JSA=JSA+1
  END DO
  IF (CELEC /= 'NONE') THEN
    YRECFM='EFIELDU'
    YCOMMENT='X_Y_Z_EFIELDU (V/m)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XEFIELDU(:,:,:),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
 !
    YRECFM='EFIELDV'
    YCOMMENT='X_Y_Z_EFIELDV (V/m)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XEFIELDV(:,:,:),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
 !
    YRECFM='EFIELDW'
    YCOMMENT='X_Y_Z_EFIELDW (V/m)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XEFIELDW(:,:,:),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
 !
    ZWORK3D(:,:,:) = 0.
    YRECFM='EMODULE'
    YCOMMENT='X_Y_Z_EMODULE (V/m)'
    ZWORK3D(:,:,:) = (XEFIELDU**2 + XEFIELDV**2 + XEFIELDW**2)**0.5
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D(:,:,:),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
 !
    ZWORK3D(:,:,:) = 0.
    YRECFM='NI_IAGGS'
    YCOMMENT='X_Y_Z_NI_IAGGS (pC/m3/s)'
    ILENCH=LEN(YCOMMENT)
    ZWORK3D(:,:,:) = XNI_IAGGS(:,:,:) * 1.E12
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D(:,:,:),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
 !
    ZWORK3D(:,:,:) = 0.
    YRECFM='NI_IDRYG'
    YCOMMENT='X_Y_Z_NI_IDRYG (pC/m3/s)'
    ILENCH=LEN(YCOMMENT)
    ZWORK3D(:,:,:) = XNI_IDRYG(:,:,:) * 1.E12
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D(:,:,:),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
 !
    ZWORK3D(:,:,:) = 0.
    YRECFM='NI_SDRYG'
    YCOMMENT='X_Y_Z_NI_SDRYG (pC/m3/s)'
    ILENCH=LEN(YCOMMENT)
    ZWORK3D(:,:,:) = XNI_SDRYG(:,:,:) * 1.E12
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D(:,:,:),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
 !
    ZWORK3D(:,:,:) = 0.
    YRECFM='INDUC_CG'
    YCOMMENT='X_Y_Z_INDUC_CG (pC/m3/s)'
    ILENCH=LEN(YCOMMENT)
    ZWORK3D(:,:,:) = XIND_RATE(:,:,:) * 1.E12
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D(:,:,:),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  END IF
  ! lagrangian variables
  DO JSV = NSV_LGBEG,NSV_LGEND
    YRECFM=TRIM(CLGNAMES(JSV-NSV_LGBEG+1))//'T'
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (M)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    JSA=JSA+1
  END DO
  ! Passive scalar variables        
 IF (LPASPOL) THEN
  DO JSV = NSV_PPBEG,NSV_PPEND
      WRITE(YRECFM,'(A3,I3.3)')'SVT',JSV
      WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (KG/KG)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                  YCOMMENT,IRESP)
      JSA=JSA+1
    END DO
  END IF
#ifdef MNH_FOREFIRE
  ! ForeFire scalar variables
 IF ( LFOREFIRE ) THEN
  DO JSV = NSV_FFBEG,NSV_FFEND
    WRITE(YRECFM,'(A3,I3.3)')'SVT',JSV
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  END DO
 END IF
#endif
  ! Conditional sampling variables  
 IF (LCONDSAMP) THEN
  DO JSV = NSV_CSBEG,NSV_CSEND
    WRITE(YRECFM,'(A3,I3.3)')'SVT',JSV
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
    JSA=JSA+1
  END DO
!
 END IF
  ! number of chemical variables (chem+aero+dust)
  ICH_NBR = 0
  IF (LUSECHEM) ICH_NBR = ICH_NBR +NSV_CHEMEND-NSV_CHEMBEG+1 
  IF (LUSECHIC) ICH_NBR = ICH_NBR +NSV_CHICEND-NSV_CHICBEG+1
  IF (.NOT.LUSECHEM.AND.LCH_CONV_LINOX) ICH_NBR = ICH_NBR + &
                                                  NSV_LNOXEND-NSV_LNOXBEG+1 
  IF (LORILAM)  ICH_NBR = ICH_NBR +NSV_AEREND -NSV_AERBEG+1 
  IF (LDUST)    ICH_NBR = ICH_NBR +NSV_DSTEND -NSV_DSTBEG+1
  IF (LDEPOS_DST(IMI))  ICH_NBR = ICH_NBR +NSV_DSTDEPEND -NSV_DSTDEPBEG+1 
  IF (LDEPOS_SLT(IMI))  ICH_NBR = ICH_NBR +NSV_SLTDEPEND -NSV_SLTDEPBEG+1 
  IF (LDEPOS_AER(IMI))  ICH_NBR = ICH_NBR +NSV_AERDEPEND -NSV_AERDEPBEG+1 
  IF (LSALT)    ICH_NBR = ICH_NBR +NSV_SLTEND -NSV_SLTBEG+1
  IF (ICH_NBR /=0) ALLOCATE(YCHNAMES(ICH_NBR))
  ! chemical scalar variables
  IF (LUSECHEM) THEN
    DO JSV = NSV_CHEMBEG,NSV_CHEMEND
      YRECFM=TRIM(UPCASE(CNAMES(JSV-NSV_CHEMBEG+1)))//'T'
      WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                  YCOMMENT,IRESP)
      YCHNAMES(JSV-JSA)=YRECFM(1:LEN_TRIM(YRECFM)-1) ! without T
    END DO
    IF (LUSECHIC) THEN
      DO JSV = NSV_CHICBEG,NSV_CHICEND
        YRECFM=TRIM(UPCASE(CICNAMES(JSV-NSV_CHICBEG+1)))//'T'
        WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                    YCOMMENT,IRESP)
        YCHNAMES(JSV-JSA)=YRECFM(1:LEN_TRIM(YRECFM)-1) ! without M
      END DO
    ENDIF
    IF (LUSECHAQ.AND.LCH_PH) THEN  ! pH values in cloud
      YRECFM = 'PHC'
      YCOMMENT='X_Y_Z_PHC'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XPHC,IGRID,ILENCH,YCOMMENT,IRESP)
      IF (NRR>=3) THEN
        YRECFM = 'PHR'
        YCOMMENT='X_Y_Z_PHR'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XPHR,IGRID,ILENCH,YCOMMENT,IRESP)
      ENDIF
    ENDIF
  ELSE IF (LCH_CONV_LINOX) THEN
    DO JSV = NSV_LNOXBEG,NSV_LNOXEND
      YRECFM='LINOXT'
      WRITE(YCOMMENT,'(A6,A3,I3.3,A)') 'X_Y_Z_','SVT',JSV,' (ppp)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH, &
                  YCOMMENT,IRESP)
      YCHNAMES(JSV-JSA)=YRECFM(1:LEN_TRIM(YRECFM)-1)
    END DO
  ENDIF  
  ! aerosol scalar variables
  IF (LORILAM) THEN
    IF ((CPROGRAM == 'REAL  ').AND.(NSV_AER > 1).AND.(IMI==1).AND.(LAERINIT))  &
      CALL CH_AER_REALLFI_n(XSVT(:,:,:,NSV_AERBEG:NSV_AEREND),XSVT(:,:,:,NSV_CHEMBEG-1+JP_CH_CO), XRHODREF)
    IF ((CPROGRAM == 'IDEAL ').AND.(NSV_AER > 1).AND.(IMI==1))  &
      CALL CH_AER_REALLFI_n(XSVT(:,:,:,NSV_AERBEG:NSV_AEREND),XSVT(:,:,:,NSV_CHEMBEG-1+JP_CH_CO),  XRHODREF)
    DO JSV = NSV_AERBEG,NSV_AEREND
      YRECFM=TRIM(UPCASE(CAERONAMES(JSV-NSV_AERBEG+1)))//'T'
      WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                  YCOMMENT,IRESP)
      IF (JSV==NSV_AERBEG) WRITE(ILUOUT,*)'MNHC: write_lfin:NSV_AERBEG ',JSV
      IF (JSV==NSV_AEREND) WRITE(ILUOUT,*)'MNHC: write_lfin:NSV_AEREND ',JSV
      YCHNAMES(JSV-JSA)=  YRECFM(1:LEN_TRIM(YRECFM)-1)
    END DO
     IF (LDEPOS_AER(IMI)) THEN        
      DO JSV = NSV_AERDEPBEG,NSV_AERDEPEND
        YRECFM=TRIM(CDEAERNAMES(JSV-NSV_AERDEPBEG+1))//'T'
        WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                    YCOMMENT,IRESP)
        IF (JSV==NSV_AERDEPBEG) WRITE(ILUOUT,*)'MNHC: write_lfin:NSV_AERDEPBEG ',JSV
        IF (JSV==NSV_AERDEPEND) WRITE(ILUOUT,*)'MNHC: write_lfin:NSV_AERDEPEND ',JSV
        YCHNAMES(JSV-JSA) = YRECFM(1:LEN_TRIM(YRECFM)-1)
      END DO   ! Loop on aq dust scalar variables      
    ENDIF
  END IF
  ! dust scalar variables
  IF (LDUST) THEN
    IF ((CPROGRAM == 'REAL  ').AND.(NSV_DST > 1).AND.(IMI==1).AND.(LDSTINIT)) &
      CALL DUSTLFI_n(XSVT(:,:,:,NSV_DSTBEG:NSV_DSTEND), XRHODREF)
    IF ((CPROGRAM == 'IDEAL ').AND.(NSV_DST > 1).AND.(IMI==1)) &
      CALL DUSTLFI_n(XSVT(:,:,:,NSV_DSTBEG:NSV_DSTEND), XRHODREF)
    !At this point, we have the tracer array in order of importance, i.e.
    !if mode 2 is most important it will occupy place 1-3 of XSVT  
   IF ((CPROGRAM == 'REAL  ').AND.((LDSTINIT).OR.(LDSTPRES)).OR.&
       (CPROGRAM == 'IDEAL ')               ) THEN
      ! In this case CDUSTNAMES is not allocated. We will use YPDUST_INI,
      !but remember that this variable does not follow JPDUSTORDER
      IMOMENTS = INT(NSV_DSTEND - NSV_DSTBEG+1)/NMODE_DST  
      !Should equal 3 at this point
      IF (IMOMENTS > 3) THEN
        WRITE(ILUOUT,*) 'Error in write_lfin: number of moments must equal or inferior to 3'
        WRITE(ILUOUT,*) NSV_DSTBEG, NSV_DSTEND,NMODE_DST,IMOMENTS
 !callabortstop
        CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
        CALL ABORT
        STOP
      END IF ! Test IMOMENTS
      ALLOCATE(YDSTNAMES(NSV_DSTEND - NSV_DSTBEG+1))

IF (IMOMENTS == 1) THEN

 DO JMODE=1, NMODE_DST
     ISV_NAME_IDX = (JPDUSTORDER(JMODE) - 1)*3 + 2
     JSV = (JMODE-1)*IMOMENTS  & !Number of moments previously counted
                +  1              & !Number of moments in this mode
                + (NSV_DSTBEG -1)      !Previous list of tracers
     YRECFM = TRIM(YPDUST_INI(ISV_NAME_IDX))//'T'  
     WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
     ILENCH=LEN(YCOMMENT)
     CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,&
                     YCOMMENT,IRESP)

          YDSTNAMES((JMODE-1)*IMOMENTS+1)=YRECFM(1:LEN_TRIM(YRECFM)-1)
 END DO ! Loop on mode
ELSE
 DO JMODE=1,NMODE_DST
   DO JMOM=1,IMOMENTS
    ISV_NAME_IDX = (JPDUSTORDER(JMODE) - 1)*3 + JMOM
    JSV = (JMODE-1)*IMOMENTS  & !Number of moments previously counted
                + JMOM               & !Number of moments in this mode
                + (NSV_DSTBEG -1)
    YRECFM = TRIM(YPDUST_INI(ISV_NAME_IDX))//'T'  !The refererence which will be written to file
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH, &
                     YCOMMENT,IRESP)
    YDSTNAMES((JMODE-1)*IMOMENTS+JMOM)=YRECFM(1:LEN_TRIM(YRECFM)-1)
   END DO ! Loop on moment
 END DO ! loop on mode
END IF ! Valeur IMOMENTS
!
      DO JSV = NSV_DSTBEG,NSV_DSTEND
        YCHNAMES(JSV-JSA) = YDSTNAMES(JSV-NSV_DSTBEG+1)
      END DO   
      DEALLOCATE(YDSTNAMES)
    ELSE 
      ! We are in the subprogram MESONH, CDUSTNAMES are allocated and are 
      !in the same order as the variables in XSVT (i.e. following JPDUSTORDER)
      DO JSV = NSV_DSTBEG,NSV_DSTEND
        YRECFM=TRIM(CDUSTNAMES(JSV-NSV_DSTBEG+1))//'T'
        WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                    YCOMMENT,IRESP)
        IF (JSV==NSV_DSTBEG) WRITE(ILUOUT,*)'MNHC: write_lfin:NSV_DSTBEG ',JSV
        IF (JSV==NSV_DSTEND) WRITE(ILUOUT,*)'MNHC: write_lfin:NSV_DSTEND ',JSV
        YCHNAMES(JSV-JSA) = YRECFM(1:LEN_TRIM(YRECFM)-1)
      END DO   ! Loop on dust scalar variables
    END IF 
     IF (LDEPOS_DST(IMI)) THEN        
      DO JSV = NSV_DSTDEPBEG,NSV_DSTDEPEND
        YRECFM=TRIM(CDEDSTNAMES(JSV-NSV_DSTDEPBEG+1))//'T'
        WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                    YCOMMENT,IRESP)
        IF (JSV==NSV_DSTDEPBEG) WRITE(ILUOUT,*)'MNHC: write_lfin:NSV_DSTDEPBEG ',JSV
        IF (JSV==NSV_DSTDEPEND) WRITE(ILUOUT,*)'MNHC: write_lfin:NSV_DSTDEPEND ',JSV
        YCHNAMES(JSV-JSA) = YRECFM(1:LEN_TRIM(YRECFM)-1)
      END DO   ! Loop on aq dust scalar variables      
    ENDIF
  ENDIF  
  ! sea salt scalar variables
  IF (LSALT) THEN
    IF ((CPROGRAM == 'REAL  ').AND.(NSV_SLT > 1).AND.(IMI==1).AND.(LSLTINIT)) &
      CALL SALTLFI_n(XSVT(:,:,:,NSV_SLTBEG:NSV_SLTEND), XRHODREF)
    IF ((CPROGRAM == 'IDEAL ').AND.(NSV_SLT > 1).AND.(IMI==1)) &
      CALL SALTLFI_n(XSVT(:,:,:,NSV_SLTBEG:NSV_SLTEND), XRHODREF)
    !At this point, we have the tracer array in order of importance, i.e.
    !if mode 2 is most important it will occupy place 1-3 of XSVT  
    IF (((CPROGRAM == 'REAL  ').AND.(LSLTINIT)).OR.&
        (CPROGRAM == 'IDEAL ')           ) THEN
      ! In this case CSALTNAMES is not allocated. We will use YPSALT_INI,
      !but remember that this variable does not follow JPSALTORDER
      IMOMENTS = INT(NSV_SLTEND - NSV_SLTBEG+1)/NMODE_SLT  
      !Should equal 3 at this point
      IF (IMOMENTS .NE. 3) THEN
        WRITE(ILUOUT,*) 'Error in write_lfin: number of moments must be 3'
        WRITE(ILUOUT,*) NSV_SLTBEG, NSV_SLTEND,NMODE_SLT,IMOMENTS
 !callabortstop
        CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
        CALL ABORT
        STOP
      END IF
      ALLOCATE(YSLTNAMES(NSV_SLTEND - NSV_SLTBEG+1))
      DO JMODE=1, NMODE_SLT
        DO JMOM = 1, IMOMENTS
          !Index from which names are picked
          ISV_NAME_IDX = (JPSALTORDER(JMODE)-1)*IMOMENTS + JMOM 
          !Index which counts in the XSVT
          JSV = (JMODE-1)*IMOMENTS      & !Number of moments previously counted
               + JMOM                   & !Number of moments in this mode
               + (NSV_SLTBEG -1)          !Previous list of tracers 
          YRECFM = TRIM(YPSALT_INI(ISV_NAME_IDX))//'T'  !The refererence which will be written to file
          WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
          ILENCH=LEN(YCOMMENT)
          CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH, &
                     YCOMMENT,IRESP)
          YSLTNAMES((JMODE-1)*IMOMENTS+JMOM)=YRECFM(1:LEN_TRIM(YRECFM)-1)
        END DO ! Loop on moments
      END DO   ! Loop on modes
      !
      DO JSV = NSV_SLTBEG,NSV_SLTEND
        YCHNAMES(JSV-JSA) = YSLTNAMES(JSV-NSV_SLTBEG+1)
      END DO   
      DEALLOCATE(YSLTNAMES)
    ELSE 
      ! We are in the subprogram MESONH, CSALTNAMES are allocated and are 
      !in the same order as the variables in XSVT (i.e. following JPSALTORDER)
      DO JSV = NSV_SLTBEG,NSV_SLTEND
        YRECFM=TRIM(CSALTNAMES(JSV-NSV_SLTBEG+1))//'T'
        WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                    YCOMMENT,IRESP)
        IF (JSV==NSV_SLTBEG) WRITE(ILUOUT,*)'MNHC: write_lfin:NSV_SLTBEG ',JSV
        IF (JSV==NSV_SLTEND) WRITE(ILUOUT,*)'MNHC: write_lfin:NSV_SLTEND ',JSV
        YCHNAMES(JSV-JSA) = YRECFM(1:LEN_TRIM(YRECFM)-1)
      END DO   ! Loop on sea salt scalar variables
    END IF 
     IF (LDEPOS_SLT(IMI)) THEN        
      DO JSV = NSV_SLTDEPBEG,NSV_SLTDEPEND
        YRECFM=TRIM(CDESLTNAMES(JSV-NSV_SLTDEPBEG+1))//'T'
        WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                    YCOMMENT,IRESP)
        IF (JSV==NSV_SLTDEPBEG) WRITE(ILUOUT,*)'MNHC: write_lfin:NSV_SLTDEPBEG ',JSV
        IF (JSV==NSV_SLTDEPEND) WRITE(ILUOUT,*)'MNHC: write_lfin:NSV_SLTDEPEND ',JSV
        YCHNAMES(JSV-JSA) = YRECFM(1:LEN_TRIM(YRECFM)-1)
      END DO   ! Loop on aq dust scalar variables      
    ENDIF
  ENDIF  
  !
  DO JSV=1,ICH_NBR
    WRITE(ILUOUT,*)JSV,TRIM(YCHNAMES(JSV))
  END DO
  YRECFM='NSV.DIM'
  YCOMMENT=' '
  IGRID=0
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',ICH_NBR,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  IF (ICH_NBR/=0) THEN
    YRECFM='NSV.TITRE'
    YCOMMENT=' '
    IGRID=0
    ILENCH=LEN(YCOMMENT)
    ILREC=LEN(YRECFM)
    ILENG=ILREC*ICH_NBR
    ALLOCATE(ICH_NAMES(ILENG))
    DO JSV = 1,ICH_NBR
      DO JT = 1,ILREC
        ICH_NAMES(ILREC*(JSV-1)+JT) = ICHAR(YCHNAMES(JSV)(JT:JT))
      ENDDO
    ENDDO
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',ICH_NAMES,IGRID,ILENCH,YCOMMENT,IRESP)
    DEALLOCATE(YCHNAMES,ICH_NAMES)
  END IF 
  !
  ! lagrangian variables
  DO JSV = NSV_LGBEG,NSV_LGEND
    YRECFM=TRIM(CLGNAMES(JSV-NSV_LGBEG+1))//'T'
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (M)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  END DO
END IF
!
!
YRECFM='LSUM'
YCOMMENT='X_Y_Z_Large Scale U component (M/S)'
IGRID=2
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XLSUM,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='LSVM'
YCOMMENT='X_Y_Z_Large Scale V component (M/S)'
IGRID=3
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XLSVM,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='LSWM'
YCOMMENT='X_Y_Z_Large Scale vertical wind (M/S)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XLSWM,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='LSTHM'
YCOMMENT='X_Y_Z_Large Scale potential Temperature (K)'
IGRID=1
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XLSTHM,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (LUSERV) THEN
  YRECFM='LSRVM'
  YCOMMENT='X_Y_Z_Large Scale Vapor Mixing Ratio (KG/KG)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XLSRVM,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
CALL WRITE_LB_n(HFMFILE)
!
!
IF (CSTORAGE_TYPE/='TT') THEN
!
!*       1.4.2  Time t:
!   
YRECFM='UT'
YCOMMENT='X_Y_Z_U component of wind (m/s)'
IGRID=2
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XUT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='VT'
YCOMMENT='X_Y_Z_V component of wind (m/s)'
IGRID=3
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XVT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='WT'
YCOMMENT='X_Y_Z_vertical wind (m/s)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XWT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='THT'
YCOMMENT='X_Y_Z_potential temperature (K)'
IGRID=1
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XTHT,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF(CTURB/='NONE') THEN
  YRECFM='TKET'
  YCOMMENT='X_Y_Z_Turbulent Kinetic Energy (M**2/S**2)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XTKET,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
!
YRECFM='PABST'
YCOMMENT='X_Y_Z_ABSolute Pressure (Pa)'
IGRID=1
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XPABST,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (NRR >=1) THEN
IRR=0
  IGRID=1                                    ! individually in file 
  IF (LUSERV) THEN
    IRR   = IRR+1 
    YRECFM= 'RVT'
    YCOMMENT='X_Y_Z_Vapor mixing Ratio (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRT(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
  END IF 
  IF (LUSERC) THEN
    IRR   = IRR+1 
    YRECFM= 'RCT'
    YCOMMENT='X_Y_Z_Cloud mixing Ratio (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRT(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
  END IF
  IF (LUSERR) THEN
    IRR   = IRR+1 
    YRECFM= 'RRT'
    YCOMMENT='X_Y_Z_Rain mixing Ratio (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRT(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
  END IF 
  IF (LUSERI) THEN
    IRR   = IRR+1 
    YRECFM= 'RIT'
    YCOMMENT='X_Y_Z_Ice mixing Ratio (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRT(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    IF ( CPROGRAM == 'MESONH' .AND. CCLOUD(1:3) == 'ICE') THEN
      YRECFM= 'CIT'
      YCOMMENT='X_Y_Z_Cloud Ice concentration (/M3)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XCIT(:,:,:),   IGRID,ILENCH,  &
                  YCOMMENT,IRESP)
    END IF
 END IF 
  IF (LUSERS) THEN
    IRR   = IRR+1 
    YRECFM= 'RST'
    YCOMMENT='X_Y_Z_Snow mixing Ratio (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRT(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
  END IF
  IF (LUSERG) THEN
    IRR   = IRR+1 
    YRECFM= 'RGT'
    YCOMMENT='X_Y_Z_Graupel mixing Ratio (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRT(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
  END IF 
  IF (LUSERH) THEN
    IRR   = IRR+1 
    YRECFM= 'RHT'
    YCOMMENT='X_Y_Z_Hail mixing Ratio (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRT(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
  END IF 
END IF
!
IF (NSV >=1) THEN
  IGRID=1                                       ! individually in the file
  ! User scalar variables
  DO JSV = 1,NSV_USER
    WRITE(YRECFM,'(A3,I3.3)')'SVT',JSV
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  END DO
  ! microphysical C2R2 scheme scalar variables
  DO JSV = NSV_C2R2BEG,NSV_C2R2END
    YRECFM=TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))//'T'
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  END DO
  ! microphysical C3R5 scheme additional scalar variables
  DO JSV = NSV_C1R3BEG,NSV_C1R3END
    YRECFM=TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))//'T'
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (/M3)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  END DO
  ! electrical scalar variables
  DO JSV = NSV_ELECBEG,NSV_ELECEND
    YRECFM=TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))//'T'
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (C/m3)'
    ILENCH=LEN(YCOMMENT)
    ZWORK3D(:,:,:) = 0.
    ZWORK3D(:,:,:) = XSVT(:,:,:,JSV) * XRHODREF(:,:,:) ! C/kg --> C/m3
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D(:,:,:),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  END DO

  ! Passive scalar variables        
 IF (LPASPOL) THEN
  DO JSV = NSV_PPBEG,NSV_PPEND
    WRITE(YRECFM,'(A3,I3.3)')'SVT',JSV
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  END DO
 END IF
#ifdef MNH_FOREFIRE
 IF (LFOREFIRE) THEN
  DO JSV = NSV_FFBEG,NSV_FFEND
    WRITE(YRECFM,'(A3,I3.3)')'SVT',JSV
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  END DO
 END IF
#endif
  ! Conditional sampling variables        
 IF (LCONDSAMP) THEN
  DO JSV = NSV_CSBEG,NSV_CSEND
    WRITE(YRECFM,'(A3,I3.3)')'SVT',JSV
    WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (KG/KG)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  END DO
 END IF
  ! chemical scalar variables
  IF (LUSECHEM) THEN
    DO JSV = NSV_CHEMBEG,NSV_CHEMEND
      YRECFM=TRIM(UPCASE(CNAMES(JSV-NSV_CHEMBEG+1)))//'T'
      WRITE(YCOMMENT,'(A6,A3,I3.3,A)') 'X_Y_Z_','SVT',JSV,' (ppp)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                  YCOMMENT,IRESP)
    END DO
    IF (LUSECHIC) THEN
      DO JSV = NSV_CHICBEG,NSV_CHICEND
        YRECFM=TRIM(UPCASE(CICNAMES(JSV-NSV_CHICBEG+1)))//'T'
        WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                    YCOMMENT,IRESP)
      END DO
    ENDIF
    IF (LUSECHAQ.AND.LCH_PH) THEN  ! pH values in cloud
      YRECFM = 'PHC'
      YCOMMENT='X_Y_Z_PHC'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XPHC,IGRID,ILENCH,YCOMMENT,IRESP)
      IF (NRR>=3) THEN
        YRECFM = 'PHR'
        YCOMMENT='X_Y_Z_PHR'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XPHR,IGRID,ILENCH,YCOMMENT,IRESP)
      ENDIF
    ENDIF
   ! linox scalar variables
  ELSE IF (LCH_CONV_LINOX) THEN
    DO JSV = NSV_LNOXBEG,NSV_LNOXEND
      YRECFM='LINOXT'
      WRITE(YCOMMENT,'(A6,A3,I3.3,A)') 'X_Y_Z_','SVT',JSV,' (ppp)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH, &
                  YCOMMENT,IRESP)
    END DO
  ENDIF  
  ! aerosol scalar variables
  IF (LORILAM) THEN
    IF ((CPROGRAM == 'REAL  ').AND.(NSV_AER > 1).AND.(IMI==1))  &
      CALL CH_AER_REALLFI_n(XSVT(:,:,:,NSV_AERBEG:NSV_AEREND),XSVT(:,:,:,NSV_CHEMBEG-1+JP_CH_CO), XRHODREF)
    IF ((CPROGRAM == 'IDEAL ').AND.(NSV_AER > 1).AND.(IMI==1))  &
      CALL CH_AER_REALLFI_n(XSVT(:,:,:,NSV_AERBEG:NSV_AEREND),XSVT(:,:,:,NSV_CHEMBEG-1+JP_CH_CO),  XRHODREF)
    DO JSV = NSV_AERBEG,NSV_AEREND
      YRECFM=TRIM(UPCASE(CAERONAMES(JSV-NSV_AERBEG+1)))//'T'
      WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                  YCOMMENT,IRESP)
    END DO
    IF (LDEPOS_AER(IMI)) THEN
      DO JSV = NSV_AERDEPBEG,NSV_AERDEPEND
        YRECFM=TRIM(CDEAERNAMES(JSV-NSV_AERDEPBEG+1))//'T'
        WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                    YCOMMENT,IRESP)
      END DO   ! Loop on aq dust scalar variables        
    ENDIF
  END IF
   ! dust scalar variables
  IF (LDUST) THEN
    IF ((CPROGRAM == 'REAL  ').AND.(NSV_DST > 1).AND.(IMI==1).AND.(LDSTINIT)) &
      CALL DUSTLFI_n(XSVT(:,:,:,NSV_DSTBEG:NSV_DSTEND), XRHODREF)
    IF ((CPROGRAM == 'IDEAL ').AND.(NSV_DST > 1).AND.(IMI==1)) &
      CALL DUSTLFI_n(XSVT(:,:,:,NSV_DSTBEG:NSV_DSTEND), XRHODREF)
    !At this point, we have the tracer array in order of importance, i.e.
    !if mode 2 is most important it will occupy place 1-3 of XSVM  
    IF ((CPROGRAM == 'REAL  ').OR.(CPROGRAM == 'IDEAL ')) THEN
      ! In this case CDUSTNAMES is not allocated. We will use YPDUST_INI,
      !but remember that this variable does not follow JPDUSTORDER
      IMOMENTS = INT(NSV_DSTEND - NSV_DSTBEG+1)/NMODE_DST  
      !Should equal 3 at this point
      IF (IMOMENTS > 3) THEN
        WRITE(ILUOUT,*) 'Error in write_lfin: number of moments must be 3'
        WRITE(ILUOUT,*) NSV_DSTBEG, NSV_DSTEND,NMODE_DST,IMOMENTS
 !callabortstop
        CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
        CALL ABORT
        STOP
      END IF
      DO JMODE=1, NMODE_DST
        DO JMOM = 1, IMOMENTS
          !Index from which names are picked
          ISV_NAME_IDX = (JPDUSTORDER(JMODE)-1)*IMOMENTS + JMOM 
          !Index which counts in the XSVT
          JSV = (JMODE-1)*IMOMENTS      & !Number of moments previously counted
               + JMOM                   & !Number of moments in this mode
               + (NSV_DSTBEG -1)          !Previous list of tracers 
          YRECFM = TRIM(YPDUST_INI(ISV_NAME_IDX))//'T'  !The refererence which will be written to file
          WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
          ILENCH=LEN(YCOMMENT)
          CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH, &
                      YCOMMENT,IRESP)
        END DO ! Loop on moments
      END DO   ! Loop on modes
      !
    ELSE 
      ! We are in the subprogram MESONH, CDUSTNAMES are allocated and are 
      !in the same order as the variables in XSVM (i.e. following JPDUSTORDER)
      DO JSV = NSV_DSTBEG,NSV_DSTEND
        YRECFM=TRIM(CDUSTNAMES(JSV-NSV_DSTBEG+1))//'T'
        WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                    YCOMMENT,IRESP)
      END DO   ! Loop on dust scalar variables
    END IF 
  ! Loop on aq dust scalar variables
    IF (LDEPOS_DST(IMI)) THEN
      DO JSV = NSV_DSTDEPBEG,NSV_DSTDEPEND
        YRECFM=TRIM(CDEDSTNAMES(JSV-NSV_DSTDEPBEG+1))//'T'
        WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                    YCOMMENT,IRESP)
      END DO   ! Loop on aq dust scalar variables        
    ENDIF
  ENDIF  
  !
   ! sea salt scalar variables
  IF (LSALT) THEN
    IF ((CPROGRAM == 'REAL  ').AND.(NSV_SLT > 1).AND.(IMI==1)) &
      CALL SALTLFI_n(XSVT(:,:,:,NSV_SLTBEG:NSV_SLTEND), XRHODREF)
    IF ((CPROGRAM == 'IDEAL ').AND.(NSV_SLT > 1).AND.(IMI==1)) &
      CALL SALTLFI_n(XSVT(:,:,:,NSV_SLTBEG:NSV_SLTEND), XRHODREF)
    !At this point, we have the tracer array in order of importance, i.e.
    !if mode 2 is most important it will occupy place 1-3 of XSVM  
    IF ((CPROGRAM == 'REAL  ').OR.(CPROGRAM == 'IDEAL ')) THEN
      ! In this case CSALTNAMES is not allocated. We will use YPSALT_INI,
      !but remember that this variable does not follow JPSALTORDER
      IMOMENTS = INT(NSV_SLTEND - NSV_SLTBEG+1)/NMODE_SLT  
      !Should equal 3 at this point
      IF (IMOMENTS .NE. 3) THEN
        WRITE(ILUOUT,*) 'Error in write_lfin: number of moments must be 3'
        WRITE(ILUOUT,*) NSV_SLTBEG, NSV_SLTEND,NMODE_SLT,IMOMENTS
 !callabortstop
        CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
        CALL ABORT
        STOP
      END IF
      DO JMODE=1, NMODE_SLT
        DO JMOM = 1, IMOMENTS
          !Index from which names are picked
          ISV_NAME_IDX = (JPSALTORDER(JMODE)-1)*IMOMENTS + JMOM 
          !Index which counts in the XSVT
          JSV = (JMODE-1)*IMOMENTS      & !Number of moments previously counted
               + JMOM                   & !Number of moments in this mode
               + (NSV_SLTBEG -1)          !Previous list of tracers 
          YRECFM = TRIM(YPSALT_INI(ISV_NAME_IDX))//'T'  !The refererence which will be written to file
          WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
          ILENCH=LEN(YCOMMENT)
          CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH, &
                      YCOMMENT,IRESP)
        END DO ! Loop on moments
      END DO   ! Loop on modes
      !
    ELSE 
      ! We are in the subprogram MESONH, CSALTNAMES are allocated and are 
      !in the same order as the variables in XSVM (i.e. following JPSALTORDER)
      DO JSV = NSV_SLTBEG,NSV_SLTEND
        YRECFM=TRIM(CSALTNAMES(JSV-NSV_SLTBEG+1))//'T'
        WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                    YCOMMENT,IRESP)
      END DO   ! Loop on sea salt scalar variables
    END IF 
    IF (LDEPOS_SLT(IMI)) THEN
      DO JSV = NSV_SLTDEPBEG,NSV_SLTDEPEND
        YRECFM=TRIM(CDESLTNAMES(JSV-NSV_SLTDEPBEG+1))//'T'
        WRITE(YCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (ppp)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                    YCOMMENT,IRESP)
      END DO   ! Loop on aq dust scalar variables        
    ENDIF
  ENDIF  
  !
   ! lagrangian variables
  DO JSV = NSV_LGBEG,NSV_LGEND
    YRECFM=TRIM(CLGNAMES(JSV-NSV_LGBEG+1))//'T'
    WRITE(YCOMMENT,'(A6,A3,I3.3,A)') 'X_Y_Z_','SVT',JSV,' (M)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,    &
                YCOMMENT,IRESP)
  END DO
END IF
!
END IF ! test on CSTORAGE_TYPE
!
YRECFM='DRYMASST'
YDIR='--'
YCOMMENT='Total Dry Mass (KG)'
IGRID=0
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDRYMASST,IGRID,ILENCH,YCOMMENT,IRESP)
!
YDIR='XY'
!
IF( CTURB /= 'NONE' .AND. CTOM=='TM06') THEN
  YRECFM='BL_DEPTH'
  YCOMMENT='X_Y_BL_DEPTH (M)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XBL_DEPTH,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
IF( CTURB /= 'NONE' .AND. LRMC01) THEN
  YRECFM='SBL_DEPTH'
  YCOMMENT='X_Y_SBL_DEPTH (M)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSBL_DEPTH,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
IF( CTURB /= 'NONE' .AND. (CPROGRAM == 'MESONH' .OR. CPROGRAM == 'DIAG')) THEN
  YRECFM='WTHVMF'
  YCOMMENT='X_Y_WTHVMF (mK/s)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XWTHVMF,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
!
IF( NRR > 1 .AND. CTURB /= 'NONE' ) THEN
  YRECFM='SRCT'
  YCOMMENT='X_Y_Z_normalized 2nd_order moment s_r_c/2Sigma_s2 (KG/KG**2)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSRCT,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='SIGS'
  YCOMMENT='X_Y_Z_Sigma_s from turbulence scheme (KG/KG**2)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSIGS,IGRID,ILENCH,YCOMMENT,IRESP)
!
END IF
!
!*       1.5    Reference state variables :
!
YDIR='--'
!
YRECFM='RHOREFZ'
YCOMMENT='rhodz for reference state without orography (kg/m3)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRHODREFZ,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='THVREFZ'
YCOMMENT='thetavz for reference state without orography (K)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XTHVREFZ,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='EXNTOP'
YCOMMENT='Exner function at model top'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XEXNTOP,IGRID,ILENCH,YCOMMENT,IRESP)
!
!*       1.6  Tendencies                                         
!
IF (CPROGRAM == 'MESONH') THEN

 YDIR='XY'
 YRECFM='US_PRES'
 YCOMMENT='X_Y_Z_US_PRES'                 
 IGRID=2
 ILENCH=LEN(YCOMMENT)
 CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRUS_PRES,IGRID,ILENCH,YCOMMENT,IRESP)
!
 YRECFM='VS_PRES'
 YCOMMENT='X_Y_Z_VS_PRES'                       
 IGRID=3
 ILENCH=LEN(YCOMMENT)
 CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRVS_PRES,IGRID,ILENCH,YCOMMENT,IRESP)
!
 YRECFM='WS_PRES'
 YCOMMENT='X_Y_Z_WS_PRES'             
 IGRID=4
 ILENCH=LEN(YCOMMENT)
 CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRWS_PRES,IGRID,ILENCH,YCOMMENT,IRESP)
!
 YRECFM='THS_CLD'
 YCOMMENT='X_Y_Z_THS_CLD'                 
 IGRID=1
 ILENCH=LEN(YCOMMENT)
 CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRTHS_CLD,IGRID,ILENCH,YCOMMENT,IRESP)
!
 IF (NRR >=1) THEN
   IRR=0
   IGRID=1               
   IF (LUSERV) THEN
    IRR   = IRR+1 
    YRECFM= 'RVS_CLD'
    YCOMMENT='X_Y_Z_RVS_CLD'                        
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRRS_CLD(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
   END IF 
   IF (LUSERC) THEN
    IRR   = IRR+1 
    YRECFM= 'RCS_CLD'
    YCOMMENT='X_Y_Z_RCS_CLD'                        
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRRS_CLD(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
   END IF 
   IF (LUSERR) THEN
    IRR   = IRR+1 
    YRECFM= 'RRS_CLD'
    YCOMMENT='X_Y_Z_RCS_CLD'                        
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRRS_CLD(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
   END IF 
   IF (LUSERI) THEN
    IRR   = IRR+1 
    YRECFM= 'RIS_CLD'
    YCOMMENT='X_Y_Z_RIS_CLD'                        
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRRS_CLD(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
   END IF 
   IF (LUSERS) THEN
    IRR   = IRR+1 
    YRECFM= 'RSS_CLD'
    YCOMMENT='X_Y_Z_RSS_CLD'                        
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRRS_CLD(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
   END IF 
   IF (LUSERG) THEN
    IRR   = IRR+1 
    YRECFM= 'RGS_CLD'
    YCOMMENT='X_Y_Z_RGS_CLD'                        
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRRS_CLD(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
   END IF 
   IF (LUSERH) THEN
    IRR   = IRR+1 
    YRECFM= 'RHS_CLD'
    YCOMMENT='X_Y_Z_RHS_CLD'                        
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRRS_CLD(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
   END IF 
 END IF 
END IF 
!
IF (NSV >=1) THEN
   DO JSV = NSV_C2R2BEG,NSV_C2R2END
    IF (JSV == NSV_C2R2BEG ) THEN
      YRECFM='RSVS_CLD1'
      YCOMMENT='X_Y_Z_RHS_CLD'                        
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRRS_CLD(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    END IF
    IF (JSV == NSV_C2R2END ) THEN
      YRECFM='RSVS_CLD2'
      YCOMMENT='X_Y_Z_RHS_CLD'                        
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRRS_CLD(:,:,:,IRR),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    END IF
   END DO
END IF
!
!*       1.8    Diagnostic variables related to the radiations
!
!
IF (CRAD /= 'NONE') THEN
!
  YDIR='--'
!
  YRECFM='DTRAD_FULL'
  YCOMMENT='-'
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,TDTRAD_FULL,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='DTRAD_CLLY'
  YCOMMENT='-'
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,TDTRAD_CLONLY,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YDIR='XY'
!
  YRECFM      = 'DTHRAD'
  YCOMMENT    = 'X_Y_Z_RADiative heating/cooling rate (K/s)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDTHRAD,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'FLALWD'
  YCOMMENT    = 'X_Y_Downward Long Waves on FLAT surface (W/M2)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XFLALWD,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'DIRFLASWD'
  YCOMMENT    = 'X_Y_DIRect Downward Long Waves on FLAT surface (W/M2)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDIRFLASWD,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'SCAFLASWD'
  YCOMMENT    = 'X_Y_SCAttered Downward Long Waves on FLAT surface (W/M2)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSCAFLASWD,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'DIRSRFSWD'
  YCOMMENT    = 'X_Y_DIRect Downward Long Waves (W/M2)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDIRSRFSWD,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'CLEARCOL_TM1'
  YCOMMENT    = 'TRACE OF CLOUD'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,NCLEARCOL_TM1,IGRID,ILENCH,YCOMMENT,IRESP) 
!
  YRECFM      = 'ZENITH'
  YCOMMENT    = 'X_Y_ZENITH (RAD)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XZENITH,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'AZIM'
  YCOMMENT    = 'X_Y_AZIMuth (RAD)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XAZIM,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'DIR_ALB'
  YCOMMENT    = 'X_Y_DIRect ALBedo (-)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDIR_ALB,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'SCA_ALB'
  YCOMMENT    = 'X_Y_SCAttered ALBedo (-)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XSCA_ALB,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'EMIS'
  YCOMMENT    = 'X_Y_EMISsivity (-)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XEMIS,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'TSRAD'
  YCOMMENT    = 'X_Y_RADiative Surface Temperature (K)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XTSRAD,IGRID,ILENCH,YCOMMENT,IRESP)
!
ENDIF
!
IF (NRR > 1 .AND. CPROGRAM == 'MESONH') THEN
  YRECFM='CLDFR'
  YCOMMENT='X_Y_Z_CLouD FRaction (0)'
  IGRID=1
  ILENCH=LEN(YCOMMENT) 
  YDIR='XY'
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XCLDFR,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
!
!*       1.9     Diagnostic variables related to deep convection
!
!
IF (CDCONV /= 'NONE' .OR. CSCONV == 'KAFR') THEN
!
! 
!
  YRECFM='DTDCONV'
  YDIR='--'
  YCOMMENT    = '-'
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,TDTDCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YDIR='XY'
!
  YRECFM      = 'COUNTCONV'
  YCOMMENT    = 'X_Y_COUNTCONV'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,NCOUNTCONV,IGRID,ILENCH,YCOMMENT, &
              IRESP)
!
!
  YRECFM      = 'DTHCONV'
  YCOMMENT    = 'X_Y_Z_CONVective heating/cooling rate (K/s)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDTHCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
  YRECFM      = 'DRVCONV'
  YCOMMENT    = 'X_Y_Z_CONVective R_v tendency (1/s)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDRVCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
  YRECFM      = 'DRCCONV'
  YCOMMENT    = 'X_Y_Z_CONVective R_c tendency (1/s)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDRCCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
  YRECFM      = 'DRICONV'
  YCOMMENT    = 'X_Y_Z_CONVective R_i tendency (1/s)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDRICONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
  YRECFM      = 'PRCONV'
  YCOMMENT    = 'X_Y_CONVective instantaneous Precipitation Rate (MM/H)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XPRCONV*3.6E6,IGRID,ILENCH, &
                                                        YCOMMENT,IRESP)
!
!
  YRECFM      = 'PACCONV'
  YCOMMENT    = 'X_Y_CONVective ACcumulated Precipitation rate (MM)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XPACCONV*1.0E3,IGRID,ILENCH, &
                                                         YCOMMENT,IRESP)
!
!
  YRECFM      = 'PRSCONV'
  YCOMMENT    = 'X_Y_CONVective instantaneous Precipitation Rate for Snow (MM/H)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XPRSCONV*3.6E6,IGRID,ILENCH, &
                                                        YCOMMENT,IRESP)
!
  IF ( LCH_CONV_LINOX ) THEN 
    YRECFM      = 'IC_RATE'
    YCOMMENT    = 'X_Y_IntraCloud lightning Rate (/s)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XIC_RATE,IGRID,ILENCH, &
                                                    YCOMMENT,IRESP)
    
    YRECFM      = 'CG_RATE'
    YCOMMENT    = 'X_Y_CloudGround lightning Rate (/s)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XCG_RATE,IGRID,ILENCH, &
                                                    YCOMMENT,IRESP)
    
    YRECFM      = 'IC_TOTAL_NB'
    YCOMMENT    = 'X_Y_IntraCloud lightning Number (-)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XIC_TOTAL_NUMBER,IGRID,ILENCH, &
                                                            YCOMMENT,IRESP)
    
    YRECFM      = 'CG_TOTAL_NB'
    YCOMMENT    = 'X_Y_CloudGround lightning Number (-)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XCG_TOTAL_NUMBER,IGRID,ILENCH, &
                                                            YCOMMENT,IRESP)
  END IF
!
  IF ( LCHTRANS .AND. NSV > 0 ) THEN
                                                ! scalar variables are recorded
    IGRID=1                                     ! individually in the file
    DO JSV = 1, NSV_USER
      WRITE(YRECFM,'(A7,I3.3)')'DSVCONV',JSV
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH      = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_C2R2BEG, NSV_C2R2END
      YRECFM = 'DSVCONV_'//TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_C1R3BEG, NSV_C1R3END
      YRECFM = 'DSVCONV_'//TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_ELECBEG, NSV_ELECEND
      YRECFM = 'DSVCONV_'//TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_PPBEG, NSV_PPEND
      WRITE(YRECFM,'(A7,I3.3)')'DSVCONV',JSV
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
#ifdef MNH_FOREFIRE
    IF (LFOREFIRE) THEN
     DO JSV = NSV_FFBEG, NSV_FFEND
      WRITE(YRECFM,'(A7,I3.3)')'DSVCONV',JSV
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
     ILENCH = LEN(YCOMMENT)
     CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDSVCONV(:,:,:,JSV),          &
       IGRID,ILENCH,YCOMMENT,IRESP)
     END DO
    END IF
#endif
    IF (LUSECHEM) THEN
      DO JSV = NSV_CHEMBEG, NSV_CHEMEND
        YRECFM = 'DSVCONV_'//TRIM(UPCASE(CNAMES(JSV-NSV_CHEMBEG+1)))
        WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
        ILENCH = LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDSVCONV(:,:,:,JSV),        &
                    IGRID,ILENCH,YCOMMENT,IRESP)
      END DO
      IF (LORILAM) THEN
        DO JSV = NSV_AERBEG, NSV_AEREND
          YRECFM = 'DSVCONV_'//TRIM(UPCASE(CAERONAMES(JSV-NSV_AERBEG+1)))
          WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
          ILENCH = LEN(YCOMMENT)
          CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDSVCONV(:,:,:,JSV),      &
                      IGRID,ILENCH,YCOMMENT,IRESP)
        END DO
      END IF
! linox scalar variables
    ELSE IF (LCH_CONV_LINOX) THEN
      DO JSV = NSV_LNOXBEG,NSV_LNOXEND
        YRECFM='DSVCONV_LINOX'
        WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDSVCONV(:,:,:,JSV),        &
                    IGRID,ILENCH,YCOMMENT,IRESP)
      END DO
    END IF
    DO JSV = NSV_LGBEG, NSV_LGEND
      YRECFM = 'DSVCONV_'//TRIM(CLGNAMES(JSV-NSV_LGBEG+1))
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_DSTBEG, NSV_DSTEND
      YRECFM = 'DSVCONV_'//TRIM(CDUSTNAMES(JSV-NSV_DSTBEG+1))
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_SLTBEG, NSV_SLTEND
      YRECFM = 'DSVCONV_'//TRIM(CSALTNAMES(JSV-NSV_SLTBEG+1))
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
  END IF
!
END IF
!
!
!*       1.10   Diagnostic variables related to the precipitations
!
IF (CPROGRAM /= 'IDEAL') THEN
  YDIR='XY'
!
  IF (ASSOCIATED(XINPRC)) THEN
  IF (SIZE(XINPRC) /= 0 ) THEN
    ZWORK2D(:,:)  = XINPRC(:,:)
    YRECFM      = 'INPRC'
    YCOMMENT    = 'X_Y_INstantaneous Cloud Precipitation Rain Rate (MM/H)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK2D*3.6E6,IGRID,ILENCH, &
                                               YCOMMENT,IRESP) ! unit conversion
!
    ZWORK2D(:,:)  = XACPRC(:,:)
    YRECFM      = 'ACPRC'
    YCOMMENT    = 'X_Y_ACcumulated Cloud Precipitation Rain Rate (MM)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK2D*1.0E3,IGRID,ILENCH, &
                                                 YCOMMENT,IRESP) ! unit conversion
  ENDIF
  ENDIF
!
  IF (ASSOCIATED(XINPRR)) THEN
  IF (SIZE(XINPRR) /= 0 ) THEN
    ZWORK2D(:,:)  = XINPRR(:,:)
    YRECFM      = 'INPRR'
    YCOMMENT    = 'X_Y_INstantaneous Precipitation Rain Rate (MM/H)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK2D*3.6E6,IGRID,ILENCH, &
                                               YCOMMENT,IRESP) ! unit conversion
!
    ZWORK3D(:,:,:)  = XINPRR3D(:,:,:)
    YRECFM      = 'INPRR3D'
    YCOMMENT    = 'X_Y_INstantaneous 3D Rain Precipitation flux (M/S)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D,IGRID,ILENCH, &
                                               YCOMMENT,IRESP) ! unit conversion
!
    ZWORK3D(:,:,:)  = XEVAP3D(:,:,:)
    YRECFM      = 'EVAP3D'
    YCOMMENT    = 'X_Y_INstantaneous 3D Rain Evaporation flux (KG/KG/S)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK3D,IGRID,ILENCH, &
                                               YCOMMENT,IRESP) ! unit conversion
!
    ZWORK2D(:,:)  = XACPRR(:,:)
    YRECFM      = 'ACPRR'
    YCOMMENT    = 'X_Y_ACcumulated Precipitation Rain Rate (MM)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK2D*1.0E3,IGRID,ILENCH, &
                                                 YCOMMENT,IRESP) ! unit conversion
  ENDIF
  ENDIF
!
  IF (ASSOCIATED(XINPRS)) THEN
  IF (SIZE(XINPRS) /= 0 ) THEN
    ZWORK2D(:,:)  = XINPRS(:,:)
    YRECFM      = 'INPRS'
    YCOMMENT    = 'X_Y_INstantaneous PRecipitation Snow Rate (MM/H)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK2D*3.6E6,IGRID,ILENCH, &
                                             YCOMMENT,IRESP) ! unit conversion
!
    ZWORK2D(:,:)  = XACPRS(:,:)
    YRECFM      = 'ACPRS'
    YCOMMENT    = 'X_Y_ACcumulated PRecipitation Snow Rate (MM)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK2D*1.0E3,IGRID,ILENCH, &
                                             YCOMMENT,IRESP) ! unit conversion
  END IF
  END IF
!
  IF (ASSOCIATED(XINPRG)) THEN
  IF (SIZE(XINPRG) /= 0 ) THEN
    ZWORK2D(:,:)  = XINPRG(:,:)
    YRECFM      = 'INPRG'
    YCOMMENT    = 'X_Y_INstantaneous PRecipitation Graupel Rate (MM/H)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK2D*3.6E6,IGRID,ILENCH, &
                                               YCOMMENT,IRESP) ! unit conversion
!
    ZWORK2D(:,:)  = XACPRG(:,:)
    YRECFM      = 'ACPRG'
    YCOMMENT    = 'X_Y_ACcumulated PRecipitation Graupel Rate (MM)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK2D*1.0E3,IGRID,ILENCH, &
                                               YCOMMENT,IRESP) ! unit conversion
  END IF
  END IF
!
  IF (ASSOCIATED(XINPRH)) THEN
  IF (SIZE(XINPRH) /= 0 ) THEN
    ZWORK2D(:,:)  = XINPRH(:,:)
    YRECFM      = 'INPRH'
    YCOMMENT    = 'X_Y_INstantaneous PRecipitation Hail Rate (MM/H)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK2D*3.6E6,IGRID,ILENCH, &
                                               YCOMMENT,IRESP) ! unit conversion
!
    ZWORK2D(:,:)  = XACPRH(:,:)
    YRECFM      = 'ACPRH'
    YCOMMENT    = 'X_Y_ACcumulated PRecipitation Hail Rate (MM)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK2D*1.0E3,IGRID,ILENCH, &
                                               YCOMMENT,IRESP) ! unit conversion
  ENDIF
  ENDIF
!
  IF (ASSOCIATED(XINPRS)) THEN
  IF (SIZE(XINPRS) /= 0 ) THEN
    ZWORK2D = XINPRR + XINPRS
    IF (SIZE(XINPRG) /= 0 ) ZWORK2D = ZWORK2D + XINPRG
    IF (SIZE(XINPRH) /= 0 ) ZWORK2D = ZWORK2D + XINPRH
    IF (SIZE(XINPRC) /= 0 ) ZWORK2D = ZWORK2D + XINPRC
    YRECFM      = 'INPRT'
    YCOMMENT    = 'X_Y_Total INstantaneaous PRecipitation rate (MM/H)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK2D*3.6E6,IGRID,ILENCH, &
                                             YCOMMENT,IRESP) ! unit conversion
!
    ZWORK2D = XACPRR + XACPRS
    IF (SIZE(XINPRG) /= 0 ) ZWORK2D = ZWORK2D + XACPRG
    IF (SIZE(XINPRH) /= 0 ) ZWORK2D = ZWORK2D + XACPRH
    IF (SIZE(XINPRC) /= 0 ) ZWORK2D = ZWORK2D + XACPRC
    YRECFM      = 'ACPRT'
    YCOMMENT    = 'X_Y_Total ACcumulated PRecipitation rate (MM)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZWORK2D*1.0E3,IGRID,ILENCH, &
                                             YCOMMENT,IRESP) ! unit conversion
  END IF
  END IF
!
END IF
!
!
!*       1.11   Forcing variables
!
!
IF (LFORCING) THEN
!
  YDIR='--'
!
  YRECFM='FRC'
  YCOMMENT=' '
  IGRID=0
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,NFRC,IGRID,ILENCH,YCOMMENT,IRESP)
!
  DO JT=1,NFRC
!
    WRITE (YFRC,'(I3.3)') JT
!
!
    YRECFM='DTFRC'//YFRC
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,TDTFRC(JT),IGRID,ILENCH, &
                                                            YCOMMENT,IRESP)
!
!
    YRECFM='UFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XUFRC(:,JT),IGRID,ILENCH,     &
                                                            YCOMMENT,IRESP)
!
!
    YRECFM='VFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XVFRC(:,JT),IGRID,ILENCH,     &
                                                            YCOMMENT,IRESP)
!
!
    YRECFM='WFRC'//YFRC
    YCOMMENT=' '
    IGRID=4
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XWFRC(:,JT),IGRID,ILENCH,     &
                                                            YCOMMENT,IRESP)
!
!
    YRECFM='THFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XTHFRC(:,JT),IGRID,ILENCH,    &
                                                            YCOMMENT,IRESP)
!
!
    YRECFM='RVFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRVFRC(:,JT),IGRID,ILENCH,    &
                                                            YCOMMENT,IRESP)
!
!
    YRECFM='TENDTHFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XTENDTHFRC(:,JT),IGRID,ILENCH,  &
                                                            YCOMMENT,IRESP)
!
!
    YRECFM='TENDRVFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XTENDRVFRC(:,JT),IGRID,ILENCH,  &
                                                            YCOMMENT,IRESP)
!
!
    YRECFM='GXTHFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XGXTHFRC(:,JT),IGRID,ILENCH,  &
                                                            YCOMMENT,IRESP)
!
!
    YRECFM='GYTHFRC'//YFRC
    YCOMMENT=' '
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XGYTHFRC(:,JT),IGRID,ILENCH,  &
                                                            YCOMMENT,IRESP)
!
!
    YRECFM='PGROUNDFRC'//YFRC
    YCOMMENT=' '
    IGRID=0
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XPGROUNDFRC(JT),IGRID,ILENCH,  &
                                                            YCOMMENT,IRESP)
!
  END DO
!
!
END IF
!
! -------------------------------------------------------------------------
IF ( L2D_ADV_FRC ) THEN
!
  YDIR='--'
!
  YRECFM='NADVFRC1'
  YCOMMENT=' '
  IGRID=0
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,NADVFRC,IGRID,ILENCH,YCOMMENT,IRESP)
!
  DO JT=1,NADVFRC
!
    WRITE (YFRC,'(I3.3)') JT
!
    YRECFM='DTADV'//YFRC
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,TDTADVFRC(JT),IGRID,ILENCH, &
                                                            YCOMMENT,IRESP)
!                                                                
    YRECFM='TH_ADV'//YFRC
    YCOMMENT='K/S'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
!    
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDTHFRC(:,:,:,JT),IGRID,ILENCH,  &
                                                            YCOMMENT,IRESP)
!    
    YRECFM='Q_ADV'//YFRC
    YCOMMENT='KG/KG/S'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
!    
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDRVFRC(:,:,:,JT),IGRID,ILENCH,  &
                                                            YCOMMENT,IRESP)
!
  ENDDO
ENDIF
!
IF ( L2D_REL_FRC ) THEN
!
  YDIR='--'
!
  YRECFM='NRELFRC1'
  YCOMMENT=' '
  IGRID=0
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,NRELFRC,IGRID,ILENCH,YCOMMENT,IRESP)
!
  DO JT=1,NRELFRC
!
    WRITE (YFRC,'(I3.3)') JT
!
    YRECFM='DTREL'//YFRC
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,TDTRELFRC(JT),IGRID,ILENCH, &
                                                            YCOMMENT,IRESP)
!                                                                
    YRECFM='TH_REL'//YFRC
    YCOMMENT='K'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
!    
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XTHREL(:,:,:,JT),IGRID,ILENCH,  &
                                                            YCOMMENT,IRESP)
!    
    YRECFM='Q_REL'//YFRC
    YCOMMENT='KG/KG'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
!    
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XRVREL(:,:,:,JT),IGRID,ILENCH,  &
                                                            YCOMMENT,IRESP)
!
  ENDDO
ENDIF
!
!*       1.11bis   Eddy Fluxes variables    ! Modif PP
!
YDIR='XY'
IF ( LTH_FLX ) THEN
   YRECFM='VT_FLX'
   YCOMMENT='K M/S'
   IGRID=2
   ILENCH=LEN(YCOMMENT)
   CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XVTH_FLUX_M,IGRID,ILENCH,YCOMMENT,IRESP)
!
   YRECFM='WT_FLX'
   YCOMMENT='K M/S'
   IGRID=4
   ILENCH=LEN(YCOMMENT)
   CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XWTH_FLUX_M,IGRID,ILENCH,YCOMMENT,IRESP)!
END IF
!
IF ( LUV_FLX) THEN
   YRECFM='VU_FLX'
   YCOMMENT='M/S**2'
   IGRID=1
   ILENCH=LEN(YCOMMENT)
   CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XVU_FLUX_M,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
!*       1.12   Balloon variables
!
!
IF (LFLYER) CALL WRITE_BALLOON_n(HFMFILE)
!
!
!*       1.13    Filtered variables for hurricane initialization
!
!
IF ( CPROGRAM=='REAL  ' ) THEN
  IF (LFILTERING) THEN
  !
    IF (NDIAG_FILT >=0) THEN
!
      YDIR='XY'
!
!             i) Total fields (TOT=BASIC+TOTDIS)
!
      YRECFM='UT15'
      YCOMMENT='X_Y_Z_U component of Total wind (m/s)'
      IGRID=2
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XUTOT,IGRID,ILENCH,YCOMMENT,IRESP)
!
      YRECFM='VT15'
      YCOMMENT='X_Y_Z_V component of Total wind (m/s)'
      IGRID=3
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XVTOT,IGRID,ILENCH,YCOMMENT,IRESP)
!
      YRECFM='TEMPTOT'
      YCOMMENT='X_Y_Z_TOTal TEMPerature (K)'
      IGRID=1
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XTTOT,IGRID,ILENCH,YCOMMENT,IRESP)
!
      IF (INDEX(CFILTERING,'P')/=0) THEN
        YRECFM='PRESTOT'
        YCOMMENT='X_Y_Z_TOTal PRESsure (Pa)'
        IGRID=1
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XPTOT,IGRID,ILENCH,YCOMMENT,IRESP)
      ENDIF
      IF (INDEX(CFILTERING,'Q')/=0) THEN
        YRECFM='HUMTOT'
        YCOMMENT='X_Y_Z_TOTal specific HUMidity (kg/kg)'
        IGRID=1
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XQTOT,IGRID,ILENCH,YCOMMENT,IRESP)
      ENDIF      
!
!             ii) Environmental fields (ENV=TOT-VORDIS)
!
      YRECFM='UT16'
      YCOMMENT='X_Y_Z_U component of Environmental wind (m/s)'
      IGRID=2
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XUENV,IGRID,ILENCH,YCOMMENT,IRESP)
!
      YRECFM='VT16'
      YCOMMENT='X_Y_Z_V component of Environmental wind (m/s)'
      IGRID=3
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XVENV,IGRID,ILENCH,YCOMMENT,IRESP)
!
      YRECFM='TEMPENV'
      YCOMMENT='X_Y_Z_ENVironmental TEMPerature (K)'
      IGRID=1
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XTENV,IGRID,ILENCH,YCOMMENT,IRESP)
!
      IF (INDEX(CFILTERING,'P')/=0) THEN
        YRECFM='PRESENV'
        YCOMMENT='X_Y_Z_ENVironmental PRESsure (Pa)'
        IGRID=1
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XPENV,IGRID,ILENCH,YCOMMENT,IRESP)
      ENDIF
      IF (INDEX(CFILTERING,'Q')/=0) THEN
        YRECFM='HUMENV'
        YCOMMENT='X_Y_Z_ENVironmental specific HUMidity (kg/kg)'
        IGRID=1
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XQENV,IGRID,ILENCH,YCOMMENT,IRESP)
      ENDIF      
!
    END IF
    IF (NDIAG_FILT >=1) THEN
!
!             iii) Basic (filtered) fields
!
      YRECFM='UT17'
      YCOMMENT='X_Y_Z_U component of Basic wind (m/s)'
      IGRID=2
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XUBASIC,IGRID,ILENCH,YCOMMENT,IRESP)
!
      YRECFM='VT17'
      YCOMMENT='X_Y_Z_V component of Basic wind (m/s)'
      IGRID=3
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XVBASIC,IGRID,ILENCH,YCOMMENT,IRESP)
!
      YRECFM='TEMPBAS'
      YCOMMENT='X_Y_Z_BASic TEMPerature (K)'
      IGRID=1
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XTBASIC,IGRID,ILENCH,YCOMMENT,IRESP)
!
      IF (INDEX(CFILTERING,'P')/=0) THEN
        YRECFM='PRESBAS'
        YCOMMENT='Pa'
        YCOMMENT='X_Y_Z_BASic PRESsure (Pa)'
        IGRID=1
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XPBASIC(:,:,1),IGRID,ILENCH,YCOMMENT,IRESP)
      ENDIF
!
      IF (INDEX(CFILTERING,'Q')/=0) THEN
        YRECFM='HUMBAS'
        YCOMMENT='X_Y_Z_BASic specific HUMidity (kg/kg)'
        IGRID=1
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XQBASIC,IGRID,ILENCH,YCOMMENT,IRESP)
      ENDIF
    END IF
    IF (NDIAG_FILT >=2) THEN
!
!             iv) Total disturbance tangential wind
!
      YRECFM='VTDIS'
      YCOMMENT='X_Y_Z_Total disturbance tangential wind (m/s)'
      IGRID=1
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XVTDIS,IGRID,ILENCH,YCOMMENT,IRESP)
!
    END IF
!
  END IF
!
!*       1.14    Dummy variables in PREP_REAL_CASE
!
  IF (ALLOCATED(CDUMMY_2D)) THEN
    YDIR='XY'
    DO JSA=1,SIZE(XDUMMY_2D,3)
      YRECFM=ADJUSTL(CDUMMY_2D(JSA))
      YCOMMENT='X_Y_Z_'//ADJUSTL(YRECFM)
      IGRID=1
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,XDUMMY_2D(:,:,JSA), &
                  IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
  END IF
!
END IF
!
!
DEALLOCATE(ZWORK2D,ZWORK3D)
!
!-------------------------------------------------------------------------------!
!
END SUBROUTINE WRITE_LFIFM_n  
