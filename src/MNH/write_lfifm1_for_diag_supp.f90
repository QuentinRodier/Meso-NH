!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$ $Date$
!-----------------------------------------------------------------
!     ######################################
      MODULE MODI_WRITE_LFIFM1_FOR_DIAG_SUPP
!     ######################################
INTERFACE
!
   SUBROUTINE WRITE_LFIFM1_FOR_DIAG_SUPP(HFMFILE)
!
!*       0.1   Declarations of arguments
!
CHARACTER(LEN=28), INTENT(IN) :: HFMFILE      ! Name of FM-file to write
!
END SUBROUTINE WRITE_LFIFM1_FOR_DIAG_SUPP
!
END INTERFACE
!
END MODULE MODI_WRITE_LFIFM1_FOR_DIAG_SUPP
!
!     ##############################################
      SUBROUTINE WRITE_LFIFM1_FOR_DIAG_SUPP(HFMFILE)
!     ##############################################
!
!!****  *WRITE_LFIFM1_FOR_DIAG_SUPP* - write records in the diag file
!!
!!    PURPOSE
!!    -------
!        The purpose of this routine is to write in the file
!     of name HFMFILE//'.lfi' with the FM routines.  
!
!!**  METHOD
!!    ------
!!      The data are written in the LFIFM file :
!!        - diagnostics from the convection
!!        - diagnostics from the radiatif transfer code
!!
!!      The localization on the model grid is also indicated :
!!        IGRID = 1 for mass grid point
!!        IGRID = 2 for U grid point
!!        IGRID = 3 for V grid point
!!        IGRID = 4 for w grid point
!!        IGRID = 0 for meaningless case
!!
!!    EXTERNAL
!!    --------
!!      FMWRIT : FM-routine to write a record
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!  	J. Stein   *Meteo France* 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    13/09/00
!!      N. Asencio  15/09/00 computation of temperature and height of clouds is moved
!!                           here and deleted in WRITE_LFIFM1_FOR_DIAG routine
!!      I. Mallet   02/11/00 add the call to RADTR_SATEL
!!      J.-P. Chaboureau 11/12/03 add call the CALL_RTTOV (table NRTTOVINFO to
!!              choose the platform, the satellite, the sensor for all channels 
!!              (see the table in rttov science and validation report) and the
!!              type of calculations in the namelist: 0 = tb, 1 = tb + jacobian,
!!              2 = tb + adjoint, 3 = tb + jacobian + adjoint)
!!      V. Masson   01/2004  removes surface (externalization)
!!      October 2009 (G. Tanguy) add ILENCH=LEN(YCOMMENT) after
!!                                              change of YCOMMENT
!!      October 2011 (C.Lac) FF10MAX  : interpolation of 10m wind
!!        between 2 Meso-NH levels if 10m is above the first atmospheric level
!!      2015 : D.Ricard add UM10/VM10 for LCARTESIAN=T cases
!!      J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!!      P.Tulet : Diag for salt and orilam
!!      J.-P. Chaboureau 07/03/2016 fix the dimensions of local arrays
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_ll
USE MODD_CST
USE MODD_PARAMETERS
USE MODD_CONF_n
USE MODD_CONF
USE MODD_DEEP_CONVECTION_n
USE MODD_DIM_n
USE MODD_FIELD_n
USE MODD_GRID_n
USE MODD_LUNIT_n
USE MODD_PARAM_n
USE MODD_PARAM_KAFR_n
USE MODD_PARAM_RAD_n
USE MODD_RADIATIONS_n
USE MODD_TIME_n
USE MODD_TURB_n
USE MODD_REF_n, ONLY: XRHODREF
USE MODD_DIAG_FLAG
USE MODD_NSV, ONLY : NSV,NSV_USER,NSV_C2R2BEG,NSV_C2R2END,             &
                     NSV_C1R3BEG, NSV_C1R3END,NSV_ELECBEG,NSV_ELECEND, &
                     NSV_CHEMBEG, NSV_CHEMEND,NSV_LGBEG,  NSV_LGEND
USE MODD_CH_M9_n,         ONLY: CNAMES
USE MODD_RAIN_C2R2_DESCR, ONLY: C2R2NAMES
USE MODD_ICE_C1R3_DESCR,  ONLY: C1R3NAMES
USE MODD_ELEC_DESCR,      ONLY: CELECNAMES
USE MODD_LG,              ONLY: CLGNAMES
USE MODD_DUST,            ONLY: LDUST
USE MODD_SALT,            ONLY: LSALT
USE MODD_CH_AEROSOL,      ONLY: LORILAM
USE MODD_RAD_TRANSF
USE MODD_DIAG_IN_RUN, ONLY: XCURRENT_ZON10M,XCURRENT_MER10M,           &
                            XCURRENT_SFCO2, XCURRENT_SW, XCURRENT_LW
!
USE MODD_DYN_n
USE MODD_CURVCOR_n
USE MODD_METRICS_n
USE MODD_DIAG_BLANK
USE MODI_PINTER
USE MODI_ZINTER
USE MODI_GRADIENT_M
USE MODI_GRADIENT_W
USE MODI_GRADIENT_U
USE MODI_GRADIENT_V
USE MODI_GRADIENT_UV
!
USE MODI_SHUMAN
USE MODI_CALL_RTTOV
USE MODI_RADTR_SATEL
USE MODI_UV_TO_ZONAL_AND_MERID
!
USE MODE_FMWRIT
!
USE MODI_GET_SURF_UNDEF
!
#ifdef MNH_NCWRIT
USE MODN_NCOUT
use mode_util
#endif
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!
CHARACTER(LEN=28), INTENT(IN) :: HFMFILE      ! Name of FM-file to write
!
!*       0.2   Declarations of local variables
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears 
                                    !  at the open of the file LFI routines 
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string 
!
CHARACTER(LEN=16) :: YRECFM         ! Name of the article to be written
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
!
INTEGER           :: IIU,IJU,IKU,IIB,IJB,IKB,IIE,IJE,IKE ! Arrays bounds
INTEGER           :: IKRAD  
! 
INTEGER           :: JI,JJ,JK,JSV   ! loop index
! 
! variables for Diagnostic variables related to deep convection
REAL,DIMENSION(:,:), ALLOCATABLE              :: ZWORK21,ZWORK22
!
! variables for computation of temperature and height of clouds
REAL :: ZCLMR ! value of mixing ratio tendency  for detection of cloud top
LOGICAL, DIMENSION(:,:), ALLOCATABLE          :: GMASK2 
INTEGER, DIMENSION(:,:), ALLOCATABLE          :: IWORK1, IWORK2
INTEGER, DIMENSION(:,:), ALLOCATABLE          :: ICL_HE_ST
REAL,    DIMENSION(:,:,:), ALLOCATABLE        :: ZWORK31,ZTEMP
!
! variables needed for the transfer radiatif diagnostic code
INTEGER :: ITOTGEO
INTEGER, DIMENSION (JPGEOST) :: INDGEO
CHARACTER(LEN=8), DIMENSION (JPGEOST) :: YNAM_SAT
REAL, DIMENSION(:,:), ALLOCATABLE :: ZIRBT, ZWVBT
REAL  :: ZUNDEF ! undefined value in SURFEX
!
! variables needed for 10m wind                                 
INTEGER :: ILEVEL
!
INTEGER :: IPRES, ITH
CHARACTER(LEN=4) :: YCAR4
CHARACTER(LEN=4), DIMENSION(SIZE(XISOPR)) :: YPRES
CHARACTER(LEN=4), DIMENSION(SIZE(XISOTH)) :: YTH
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWORK32,ZWORK33,ZWORK34,ZWRES,ZPRES,ZWTH
REAL, DIMENSION(:), ALLOCATABLE :: ZTH
REAL,DIMENSION(SIZE(XTHT,1),SIZE(XTHT,2),SIZE(XTHT,3))  :: ZPOVO
REAL,DIMENSION(SIZE(XTHT,1),SIZE(XTHT,2),SIZE(XTHT,3))  :: ZVOX,ZVOY,ZVOZ
REAL,DIMENSION(SIZE(XTHT,1),SIZE(XTHT,2),SIZE(XTHT,3))  :: ZCORIOZ
!-------------------------------------------------------------------------------
!
!*       0.     ARRAYS BOUNDS INITIALIZATION
!
IIU=SIZE(XTHT,1)
IJU=SIZE(XTHT,2)
IKU=SIZE(XTHT,3)
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
IKB=1+JPVEXT
IKE=IKU-JPVEXT
!
ALLOCATE(ZWORK21(IIU,IJU))
ALLOCATE(ZWORK31(IIU,IJU,IKU))
ALLOCATE(ZTEMP(IIU,IJU,IKU))
ZTEMP(:,:,:)=XTHT(:,:,:)*(XPABST(:,:,:)/ XP00) **(XRD/XCPD)
!
#ifdef MNH_NCWRIT
IF (LNETCDF.AND..NOT.LCARTESIAN) THEN
  YRECFM='LAT'
  YCOMMENT='X_Y_latitude (degree)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XLAT,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='LON'
  YCOMMENT='X_Y_longitude (degree)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XLON,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
#endif
!
!-------------------------------------------------------------------------------
!
!*       1.     DIAGNOSTIC RELATED TO CONVECTION
!               -------------------------------- 
!
!* Diagnostic variables related to deep convection
!
IF (NCONV_KF >= 0) THEN
!
  YRECFM      = 'CAPE'
  YCOMMENT    = 'X_Y_Convective Available Potentiel Energy (J/kg)'
  IGRID       = 4
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XCAPE,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'CLTOPCONV'                              ! top height (km) of
  ZWORK21(:,:)= 0.                                       ! convective clouds
  DO JJ=IJB,IJE
    DO JI=IIB,IIE
      IF (NCLTOPCONV(JI,JJ)/=0) ZWORK21(JI,JJ)= XZZ(JI,JJ,NCLTOPCONV(JI,JJ))/1.E3
    END DO
  END DO
  YCOMMENT    = 'X_Y_Top of Convective Cloud (km)'
  IGRID       = 4
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'CLBASCONV'                              ! base height (km) of
  ZWORK21(:,:)= 0.                                       ! convective clouds
  DO JJ=IJB,IJE
    DO JI=IIB,IIE
      IF (NCLBASCONV(JI,JJ)/=0) ZWORK21(JI,JJ)= XZZ(JI,JJ,NCLBASCONV(JI,JJ))/1.E3
    END DO
  END DO
  YCOMMENT    = 'X_Y_Base of Convective Cloud (km)'
  IGRID       = 4
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
!
END IF
IF (NCONV_KF >= 1) THEN
!
  YRECFM      = 'DTHCONV'
  YCOMMENT    = 'X_Y_Z_CONVective heating/cooling rate (K/s)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDTHCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'DRVCONV'
  YCOMMENT    = 'X_Y_Z_CONVective R_v tendency (1/s)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDRVCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'DRCCONV'
  YCOMMENT    = 'X_Y_Z_CONVective R_c tendency (1/s)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDRCCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'DRICONV'
  YCOMMENT    = 'X_Y_Z_CONVective R_i tendency (1/s)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDRICONV,IGRID,ILENCH,YCOMMENT,IRESP)
!  
  IF ( LCHTRANS .AND. NSV > 0 ) THEN
    IGRID=1                                    
    ! User scalar variables
    DO JSV = 1, NSV_USER
      WRITE(YRECFM,'(A7,I3.3)')'DSVCONV',JSV
      WRITE(YCOMMENT,'(A6,A2,I3.3,A26)')'X_Y_Z_','SV',JSV,' CONVective tendency (1/s)'
      ILENCH      = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDSVCONV(:,:,:,JSV),          &
                  IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    ! microphysical C2R2 scheme scalar variables
    DO JSV = NSV_C2R2BEG, NSV_C2R2END
      YRECFM = 'DSVCONV_'//TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))
      WRITE(YCOMMENT,'(A6,A,A26)')'X_Y_Z_',TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1)), &
                                  ' CONVective tendency (1/s)'
      ILENCH = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    ! microphysical C3R5 scheme additional scalar variables
    DO JSV = NSV_C1R3BEG,NSV_C1R3END
      YRECFM='DSVCONV_'//TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))
      WRITE(YCOMMENT,'(A6,A,A26)')'X_Y_Z_',TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1)), &
                                  ' CONVective tendency (1/s)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDSVCONV(:,:,:,JSV),           &
                  IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    ! electrical scalar variables
    DO JSV = NSV_ELECBEG,NSV_ELECEND
      YRECFM = 'DSVCONV_'//TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))
      WRITE(YCOMMENT,'(A6,A,A26)')'X_Y_Z_',TRIM(CELECNAMES(JSV-NSV_ELECBEG+1)),&
                                  ' CONVective tendency (1/s)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDSVCONV(:,:,:,JSV),           &
                  IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    ! chemical scalar variables
    DO JSV = NSV_CHEMBEG, NSV_CHEMEND
      YRECFM = 'DSVCONV_'//TRIM(CNAMES(JSV-NSV_CHEMBEG+1))
      WRITE(YCOMMENT,'(A6,A,A26)')'X_Y_Z_',TRIM(CNAMES(JSV-NSV_CHEMBEG+1)), &
                                  ' CONVective tendency (1/s)'
      ILENCH = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDSVCONV(:,:,:,JSV),           &
                  IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    ! lagrangian variables
    DO JSV = NSV_LGBEG,NSV_LGEND
      YRECFM='DSVCONV_'//TRIM(CLGNAMES(JSV-NSV_LGBEG+1))
       WRITE(YCOMMENT,'(A6,A,A26)')'X_Y_Z_',TRIM(CLGNAMES(JSV-NSV_LGBEG+1)), &
                                  ' CONVective tendency (1/s)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDSVCONV(:,:,:,JSV),           &
                  IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
  END IF
!
END IF
IF (NCONV_KF >= 2) THEN
!
  YRECFM      = 'PRLFLXCONV'
  YCOMMENT    = 'X_Y_Liquid Precipitation Convective Flux (m/s)'
  IGRID       = 4
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XPRLFLXCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'PRSFLXCONV'
  YCOMMENT    = 'X_Y_Solid Precipitation Convective Flux (m/s)'
  IGRID       = 4
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XPRSFLXCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'UMFCONV'
  YCOMMENT    = 'X_Y_Updraft Convective Mass Flux (kg/s m**2)'
  IGRID       = 4
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XUMFCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM      = 'DMFCONV'
  YCOMMENT    = 'X_Y_Downdraft Convective Mass Flux (kg/s m**2)'
  IGRID       = 4
  ILENCH      = LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDMFCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
END IF
!-------------------------------------------------------------------------------
!
!* Height and temperature of clouds top
!
IF (LCLD_COV .AND. LUSERC) THEN
  ALLOCATE(IWORK1(IIU,IJU),IWORK2(IIU,IJU))
  ALLOCATE(ICL_HE_ST(IIU,IJU))
  ALLOCATE(GMASK2(IIU,IJU))
  ALLOCATE(ZWORK22(IIU,IJU))
!
! Explicit clouds
!
  ICL_HE_ST(:,:)=IKB  !initialization
  IWORK1(:,:)=IKB     ! with the
  IWORK2(:,:)=IKB     ! ground values
  ZCLMR=1.E-4         ! detection of clouds for cloud mixing ratio > .1g/kg
!
  GMASK2(:,:)=.TRUE.
  ZWORK31(:,:,:)= MZM(1,IKU,1, XRT(:,:,:,2) ) ! cloud mixing ratio at zz levels
  DO JK=IKE,IKB,-1
    WHERE ( (GMASK2(:,:)).AND.(ZWORK31(:,:,JK)>ZCLMR) )
      GMASK2(:,:)=.FALSE.
      IWORK1(:,:)=JK
    END WHERE
  END DO
!
  IF (LUSERI) THEN
    GMASK2(:,:)=.TRUE.
    ZWORK31(:,:,:)= MZM(1,IKU,1, XRT(:,:,:,4) ) ! cloud mixing ratio at zz levels
    DO JK=IKE,IKB,-1
      WHERE ( (GMASK2(:,:)).AND.(ZWORK31(:,:,JK)>ZCLMR) )
        GMASK2(:,:)=.FALSE.
        IWORK2(:,:)=JK
      END WHERE
    END DO
  END IF
!
  ZWORK21(:,:)=0.
  DO JJ=IJB,IJE
   DO JI=IIB,IIE
     ICL_HE_ST(JI,JJ)=MAX(IWORK1(JI,JJ),IWORK2(JI,JJ) )
     ZWORK21(JI,JJ)  =XZZ(JI,JJ,ICL_HE_ST(JI,JJ)) ! height (m) of explicit clouds
   END DO
  END DO 
!
  WHERE ( ZWORK21(:,:)==XZZ(:,:,IKB) ) ZWORK21=0. ! set the height to 
                                                  ! 0 if there is no cloud
  ZWORK21(:,:)=ZWORK21(:,:)/1.E3            ! height (km) of explicit clouds
!
  YRECFM='HECL'
  YCOMMENT='X_Y_Height of Explicit CLoud top (km)'
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
!
!  Higher top of the different species of clouds
!
  IWORK1(:,:)=IKB  ! initialization with the ground values
  ZWORK31(:,:,:)=MZM(1,IKU,1,ZTEMP(:,:,:)) ! temperature (K) at zz levels
  IF(CRAD/='NONE')  ZWORK31(:,:,IKB)=XTSRAD(:,:)
  ZWORK21(:,:)=0.
  ZWORK22(:,:)=0.
  DO JJ=IJB,IJE
    DO JI=IIB,IIE
      IWORK1(JI,JJ)=ICL_HE_ST(JI,JJ)
      IF (NCONV_KF >=0) &
      IWORK1(JI,JJ)= MAX(ICL_HE_ST(JI,JJ),NCLTOPCONV(JI,JJ))
      ZWORK21(JI,JJ)= XZZ(JI,JJ,IWORK1(JI,JJ))         ! max. cloud height (m)
      ZWORK22(JI,JJ)= ZWORK31(JI,JJ,IWORK1(JI,JJ))-XTT ! cloud temperature (C)
    END DO
  END DO 
!
  IF (NCONV_KF <0) THEN
    PRINT*,'YOU DO NOT ASK FOR CONVECTIVE DIAGNOSTICS (NCONV_KF<0), SO'
    PRINT*,'  HC not written in FM-file (equal to HEC)'
  ELSE
    WHERE ( ZWORK21(:,:)==XZZ(:,:,IKB) ) ZWORK21(:,:)=0. ! set the height to 
                                                         ! 0 if there is no cloud
    ZWORK21(:,:)=ZWORK21(:,:)/1.E3                 ! max. cloud height (km)
!
    YRECFM='HCL'
    YCOMMENT='X_Y_Height of CLoud top (km)'
    IGRID=4
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  ENDIF
!
  YRECFM='TCL'
  YCOMMENT='X_Y_Temperature of CLoud top (C)'
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK22,IGRID,ILENCH,YCOMMENT,IRESP)
!
  YRECFM='CLDFR'
  YCOMMENT='X_Y_Z_Cloud Fraction (0)'
  IGRID=1
  ILENCH=LEN(YCOMMENT) 
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XCLDFR,IGRID,ILENCH,YCOMMENT,IRESP)
!
!  Visibility                                    
!
  ZWORK31(:,:,:)= 1.E4                ! 10 km for clear sky
  WHERE (XRT(:,:,:,2) > 0.)
    ZWORK31(:,:,:)=3.9E3/(144.7*(XRHODREF(:,:,:)*1.E3*XRT(:,:,:,2)/(1.+XRT(:,:,:,2)))**0.88)
  END WHERE
!
  YRECFM  ='VISI_HOR'
  YCOMMENT='X_Y_Z_VISI_HOR (m)'
  IGRID   = 1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)

  DEALLOCATE(IWORK1,IWORK2,ICL_HE_ST,GMASK2,ZWORK22)
END IF
!
!-------------------------------------------------------------------------------
!
!*       2.    DIAGNOSTIC RELATED TO RADIATIONS
!              --------------------------------
!
IF (NRAD_3D >= 0) THEN
  IF (CRAD /= 'NONE') THEN
!
    YRECFM      = 'DTHRAD'
    YCOMMENT    = 'X_Y_Z_RADiative heating/cooling rate (K/s)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDTHRAD,IGRID,ILENCH,YCOMMENT,IRESP)
!
    YRECFM      = 'FLALWD'
    YCOMMENT    = 'X_Y_Downward Long Waves on FLAT surface (W/M2)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XFLALWD,IGRID,ILENCH,YCOMMENT,IRESP)
!
    YRECFM      = 'DIRFLASWD'
    YCOMMENT    = 'X_Y_DIRect Downward Short Waves on FLAT surface (W/M2)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDIRFLASWD,IGRID,ILENCH,YCOMMENT,IRESP)
!
    YRECFM      = 'SCAFLASWD'
    YCOMMENT    = 'X_Y_SCAttered Downward Short Waves on FLAT surface (W/M2)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XSCAFLASWD,IGRID,ILENCH,YCOMMENT,IRESP)
!
    YRECFM      = 'DIRSRFSWD'
    YCOMMENT    = 'X_Y_DIRect Downward Short Waves (W/M2)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDIRSRFSWD,IGRID,ILENCH,YCOMMENT,IRESP)
!
    YRECFM      = 'CLEARCOL_TM1'
    YCOMMENT    = 'TRACE OF CLOUD'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',NCLEARCOL_TM1,IGRID,ILENCH,YCOMMENT,IRESP) 
!
    YRECFM      = 'ZENITH'
    YCOMMENT    = 'X_Y_ZENITH (RAD)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XZENITH,IGRID,ILENCH,YCOMMENT,IRESP)
!
    YRECFM      = 'AZIM'
    YCOMMENT    = 'X_Y_AZIMuth (RAD)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XAZIM,IGRID,ILENCH,YCOMMENT,IRESP)
!
    YRECFM      = 'DIR_ALB'
    YCOMMENT    = 'X_Y_DIRect ALBedo (-)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XDIR_ALB,IGRID,ILENCH,YCOMMENT,IRESP)
!
    YRECFM      = 'SCA_ALB'
    YCOMMENT    = 'X_Y_SCAttered ALBedo (-)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XSCA_ALB,IGRID,ILENCH,YCOMMENT,IRESP)
!
    YRECFM      = 'EMIS'
    YCOMMENT    = 'X_Y_EMISsivity (-)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XEMIS,IGRID,ILENCH,YCOMMENT,IRESP)
!
    YRECFM      = 'TSRAD'
    YCOMMENT    = 'X_Y_RADiative Surface Temperature (K)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XTSRAD,IGRID,ILENCH,YCOMMENT,IRESP)
!
  ELSE
    PRINT*,'YOU WANT DIAGNOSTICS RELATED TO RADIATION'
    PRINT*,' BUT NO RADIATIVE SCHEME WAS ACTIVATED IN THE MODEL'
  END IF
END IF
IF (NRAD_3D >= 1) THEN
  IF (LDUST) THEN
!Dust optical depth between two vertical levels
    YRECFM      = 'DSTAOD3D'
    YCOMMENT    = 'X_Y_Z_DuST Aerosol Optical Depth (m)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=0.
    DO JK=IKB,IKE
      IKRAD = JK - JPVEXT
      ZWORK31(:,:,JK)= XAER(:,:,IKRAD,3)
    END DO
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
!Dust optical depth
    ZWORK21(:,:)=0.0
    DO JK=IKB,IKE
      IKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          ZWORK21(JI,JJ)=ZWORK21(JI,JJ)+XAER(JI,JJ,IKRAD,3)
        ENDDO
      ENDDO
    ENDDO
    YRECFM      = 'DSTAOD2D'
    YCOMMENT    = 'X_Y_DuST Aerosol Optical Depth (m)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
!Dust extinction (optical depth per km)
    YRECFM      = 'DSTEXT'
    YCOMMENT    = 'X_Y_Z_DuST EXTinction (1/km) '
    ILENCH=LEN(YCOMMENT)
    DO JK=IKB,IKE
      IKRAD = JK - JPVEXT
      ZWORK31(:,:,JK)= XAER(:,:,IKRAD,3)/(XZZ(:,:,JK+1)-XZZ(:,:,JK))*1.D3
    ENDDO
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
  IF (LSALT) THEN
!Salt optical depth between two vertical levels
    YRECFM      = 'SLTAOD3D'
    YCOMMENT    = 'X_Y_Z_Salt Aerosol Optical Depth (m)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=0.
    DO JK=IKB,IKE
      IKRAD = JK - JPVEXT
      ZWORK31(:,:,JK)= XAER(:,:,IKRAD,2)
    END DO
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
!Salt optical depth
    ZWORK21(:,:)=0.0
    DO JK=IKB,IKE
      IKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          ZWORK21(JI,JJ)=ZWORK21(JI,JJ)+XAER(JI,JJ,IKRAD,2)
        ENDDO
      ENDDO
    ENDDO
    YRECFM      = 'SLTAOD2D'
    YCOMMENT    = 'X_Y_Salt Aerosol Optical Depth (m)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
!Salt extinction (optical depth per km)
    YRECFM      = 'SLTEXT'
    YCOMMENT    = 'X_Y_Z_Salt EXTinction (1/km) '
    ILENCH=LEN(YCOMMENT)
    DO JK=IKB,IKE
      IKRAD = JK - JPVEXT
      ZWORK31(:,:,JK)= XAER(:,:,IKRAD,2)/(XZZ(:,:,JK+1)-XZZ(:,:,JK))*1.D3
    ENDDO
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
  IF (LORILAM) THEN
!Orilam anthropogenic optical depth between two vertical levels
    YRECFM      = 'AERAOD3D'
    YCOMMENT    = 'X_Y_Z_Anthropogenic Aerosol Optical Depth (m)'
    ILENCH=LEN(YCOMMENT)
    ZWORK31(:,:,:)=0.
    DO JK=IKB,IKE
      IKRAD = JK - JPVEXT
      ZWORK31(:,:,JK)= XAER(:,:,IKRAD,4)
    END DO
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
!Orilam anthropogenic optical depth
    ZWORK21(:,:)=0.0
    DO JK=IKB,IKE
      IKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          ZWORK21(JI,JJ)=ZWORK21(JI,JJ)+XAER(JI,JJ,IKRAD,4)
        ENDDO
      ENDDO
    ENDDO
    YRECFM      = 'AERAOD2D'
    YCOMMENT    = 'X_Y_Anthropogenic Aerosol Optical Depth (m)'
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
!Orilam anthropogenic extinction (optical depth per km)
    YRECFM      = 'AEREXT'
    YCOMMENT    = 'X_Y_Z_Anthropogenic EXTinction (1/km) '
    ILENCH=LEN(YCOMMENT)
    DO JK=IKB,IKE
      IKRAD = JK - JPVEXT
      ZWORK31(:,:,JK)= XAER(:,:,IKRAD,4)/(XZZ(:,:,JK+1)-XZZ(:,:,JK))*1.D3
    ENDDO
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK31,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF

END IF
!
!-------------------------------------------------------------------------------
!
!* Brightness temperatures from the radiatif transfer code (Morcrette, 1991)
!
IF (LEN_TRIM(CRAD_SAT) /= 0 .AND. NRR /=0) THEN
  ALLOCATE (ZIRBT(IIU,IJU),ZWVBT(IIU,IJU))
  ITOTGEO=0
  IF (INDEX(CRAD_SAT,'GOES-E')   /= 0) THEN
    ITOTGEO= ITOTGEO+1
    INDGEO(ITOTGEO) = 1
    YNAM_SAT(ITOTGEO) = 'GOES-E'
  END IF
  IF (INDEX(CRAD_SAT,'GOES-W')   /= 0) THEN
    ITOTGEO= ITOTGEO+1
    INDGEO(ITOTGEO) = 2
    YNAM_SAT(ITOTGEO) = 'GOES-W'
  END IF
  IF (INDEX(CRAD_SAT,'GMS')      /= 0) THEN
    ITOTGEO= ITOTGEO+1
    INDGEO(ITOTGEO) = 3
    YNAM_SAT(ITOTGEO) = 'GMS'
  END IF
  IF (INDEX(CRAD_SAT,'INDSAT')   /= 0) THEN
    ITOTGEO= ITOTGEO+1
    INDGEO(ITOTGEO) = 4
    YNAM_SAT(ITOTGEO) = 'INDSAT'
  END IF
  IF (INDEX(CRAD_SAT,'METEOSAT') /= 0) THEN
    ITOTGEO= ITOTGEO+1
    INDGEO(ITOTGEO) = 5
    YNAM_SAT(ITOTGEO) = 'METEOSAT'
  END IF
  PRINT*,'YOU ASK FOR BRIGHTNESS TEMPERATURES FOR ',ITOTGEO,' SATELLITE(S)'
  IF (NRR==1) THEN
    PRINT*,' THERE IS ONLY VAPOR WATER IN YOUR ATMOSPHERE'
    PRINT*,' IRBT WILL NOT TAKE INTO ACCOUNT CLOUDS.'
  END IF
  !
  DO JI=1,ITOTGEO
    ZIRBT(:,:) = XUNDEF
    ZWVBT(:,:) = XUNDEF
    CALL RADTR_SATEL(TDTCUR%TDATE%YEAR,TDTCUR%TDATE%MONTH,TDTCUR%TDATE%DAY, &
                     TDTCUR%TIME, NDLON, NFLEV, NSTATM, NRAD_COLNBR, XEMIS, &
                     XCCO2, XTSRAD, XSTATM, XTHT, XRT, XPABST, XZZ,         &
                     XSIGS, XMFCONV, XCLDFR, LUSERI, LSIGMAS,               &
                     LSUBG_COND, LRAD_SUBG_COND, ZIRBT, ZWVBT,              &
                     INDGEO(JI),VSIGQSAT ) 
    !
    YRECFM      =TRIM(YNAM_SAT(JI))//'_IRBT'
    YCOMMENT    =TRIM(YNAM_SAT(JI))//' Infra-Red Brightness Temperature (K)'
    IGRID       =1
    ILENCH      =LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZIRBT,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    YRECFM      =TRIM(YNAM_SAT(JI))//'_WVBT'
    YCOMMENT    =TRIM(YNAM_SAT(JI))//' Water-Vapor Brightness Temperature (K)'
    IGRID       =1
    ILENCH      =LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWVBT,IGRID,ILENCH,YCOMMENT,IRESP)
  END DO
  DEALLOCATE(ZIRBT,ZWVBT)
END IF
!
!-------------------------------------------------------------------------------
!
!* Brightness temperatures from the Radiatif Transfer for Tiros Operational
! Vertical Sounder (RTTOV) code (version 8.7)
!
IF (NRTTOVINFO(1,1) /= NUNDEF) THEN
  PRINT*,'YOU ASK FOR BRIGHTNESS TEMPERATURE COMPUTED by RTTOV code'
  CALL CALL_RTTOV(NDLON, NFLEV, NSTATM, XEMIS, XTSRAD, XSTATM, XTHT, XRT,     &
                  XPABST, XZZ, XMFCONV, XCLDFR, XUT(:,:,IKB), XVT(:,:,IKB),   &
                  LUSERI, NRTTOVINFO, HFMFILE                                 )
END IF
!
!-------------------------------------------------------------------------------
!
!*       3.    DIAGNOSTIC RELATED TO SURFACE
!              -----------------------------
!
IF (CSURF=='EXTE') THEN
!! Since SURFEX7 (masdev49) XCURRENT_ZON10M and XCURRENT_MER10M
!! are equal to XUNDEF of SURFEX if the first atmospheric level
!! is under 10m
  CALL GET_SURF_UNDEF(ZUNDEF)
!
  ILEVEL=IKB 
  !While there are XUNDEF values and we aren't at model's top
  DO WHILE(ANY(XCURRENT_ZON10M(IIB:IIE,IJB:IJE)==ZUNDEF) .AND. (ILEVEL/=IKE-1) )

    !Where interpolation is needed and possible
    !(10m is between ILEVEL and ILEVEL+1 or 10m is below the bottom level)
    WHERE(XCURRENT_ZON10M(IIB:IIE,IJB:IJE)==ZUNDEF .AND. &
                   ( XZHAT(ILEVEL+1) + XZHAT(ILEVEL+2)) /2. >10.)

      !Interpolation between ILEVEL and ILEVEL+1
      XCURRENT_ZON10M(IIB:IIE,IJB:IJE)=XUT(IIB:IIE,IJB:IJE,ILEVEL) + &
            (XUT(IIB:IIE,IJB:IJE,ILEVEL+1)-XUT(IIB:IIE,IJB:IJE,ILEVEL)) * &
            ( 10.- (XZHAT(ILEVEL)+XZHAT(ILEVEL+1))/2. ) / &
           ( (XZHAT(ILEVEL+2)-XZHAT(ILEVEL)) /2.)
      XCURRENT_MER10M(IIB:IIE,IJB:IJE)=XVT(IIB:IIE,IJB:IJE,ILEVEL) + &
            (XVT(IIB:IIE,IJB:IJE,ILEVEL+1)-XVT(IIB:IIE,IJB:IJE,ILEVEL)) * &
            (10.- (XZHAT(ILEVEL)+XZHAT(ILEVEL+1))/2. ) / &                                    
           ( (XZHAT(ILEVEL+2)-XZHAT(ILEVEL)) /2.)
    END WHERE
    ILEVEL=ILEVEL+1 !level just higher
  END DO
  !

    YCOMMENT='X_Y_components of wind at 10m (m/s)'
    IGRID=0
    ! in this case (argument IGRID=0), input winds are ZONal and MERidien 
    !          and, output ones are in MesoNH grid   
    IF (.NOT. LCARTESIAN) THEN                                                 
      CALL UV_TO_ZONAL_AND_MERID(XCURRENT_ZON10M,XCURRENT_MER10M,IGRID,     &
            HFMFILE=HFMFILE,HRECU='UM10',HRECV='VM10',HCOMMENT=YCOMMENT)
    ELSE
     YRECFM      ='UM10'
     YCOMMENT    ='X_Y_UM10 (m/s)'
     IGRID       =1
     ILENCH      =LEN(YCOMMENT)
     CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XCURRENT_ZON10M,IGRID,ILENCH,YCOMMENT,IRESP)
     YRECFM      ='VM10'
     YCOMMENT    ='X_Y_VM10 (m/s)'
     IGRID       =1
     ILENCH      =LEN(YCOMMENT)
     CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XCURRENT_MER10M,IGRID,ILENCH,YCOMMENT,IRESP)
    ENDIF
      !
    IF (SIZE(XTKET)>0) THEN
     ZWORK21(:,:)= 0.    
     ZWORK21(:,:) = SQRT(XCURRENT_ZON10M(:,:)**2+XCURRENT_MER10M(:,:)**2)
     ZWORK21(:,:) =ZWORK21(:,:) + 4. * SQRT(XTKET(:,:,IKB))
     YRECFM      ='FF10MAX'
     YCOMMENT    ='X_Y_FF10MAX (m/s)'
     IGRID       =1
     ILENCH      =LEN(YCOMMENT)
     CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
  IF(ANY(XCURRENT_SFCO2/=XUNDEF))THEN
    YCOMMENT='CO2 flux (mg/m2/s)'
    ILENCH=LEN(YCOMMENT)
    YRECFM='SFCO2'
    IGRID=1
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XCURRENT_SFCO2,IGRID,ILENCH,   &
    YCOMMENT,IRESP)
  END IF
  !
  IF(ANY(XCURRENT_SW/=XUNDEF))THEN
    YCOMMENT='SW (W/m2)'
    ILENCH=LEN(YCOMMENT)
    YRECFM='SW'
    IGRID=1
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XCURRENT_SW,IGRID,ILENCH,      &
    YCOMMENT,IRESP)
  END IF
  !
  IF(ANY(XCURRENT_LW/=XUNDEF))THEN
    YCOMMENT='LW (W/m2)'
    ILENCH=LEN(YCOMMENT)
    YRECFM='LW'
    IGRID=1
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',XCURRENT_LW,IGRID,ILENCH,      &
    YCOMMENT,IRESP)
  END IF
END IF

! MODIF FP NOV 2012
!-------------------------------------------------------------------------------
!
!*       4.     DIAGNOSTIC ON PRESSURE LEVELS
!               -----------------------------
!
IF (LISOPR .AND. XISOPR(1)/=0.) THEN
!
!
ALLOCATE(ZWORK32(IIU,IJU,IKU))
ALLOCATE(ZWORK33(IIU,IJU,IKU))
ALLOCATE(ZWORK34(IIU,IJU,IKU))
!
! *************************************************
! Determine the pressure level where to interpolate
! *************************************************
  IPRES=0
  DO JI=1,SIZE(XISOPR)
    IF (XISOPR(JI)<=10..OR.XISOPR(JI)>1000.) EXIT
    IPRES=IPRES+1
    WRITE(YCAR4,'(I4)') INT(XISOPR(JI))
    YPRES(IPRES)=ADJUSTL(YCAR4)
  END DO

  ALLOCATE(ZWRES(IIU,IJU,IPRES))
  ZWRES(:,:,:)=XUNDEF
  ALLOCATE(ZPRES(IIU,IJU,IPRES))
  IPRES=0
  DO JI=1,SIZE(XISOPR)
    IF (XISOPR(JI)<=10..OR.XISOPR(JI)>1000.) EXIT
    IPRES=IPRES+1
    ZPRES(:,:,IPRES)=XISOPR(JI)*100.
  END DO
  PRINT *,'PRESSURE LEVELS WHERE TO INTERPOLATE=',ZPRES(1,1,:)
!
!*       Standard Variables
!
! *********************
! Potential Temperature
! *********************
  CALL PINTER(XTHT, XPABST, XZZ, ZTEMP, ZWRES, ZPRES, &
         IIU, IJU, IKU, IKB, IPRES, 'LOG', 'RHU.')
  DO JK=1,IPRES
    ZWORK21(:,:) = ZWRES(:,:,JK)
    YRECFM='THT'//TRIM(YPRES(JK))//'HPA'
    YCOMMENT='X_Y_potential temperature '//TRIM(YPRES(JK))//'hPa (K)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END DO
! *********************
! Wind
! *********************
  ZWORK31(:,:,:) = MXF(XUT(:,:,:))
  CALL PINTER(ZWORK31, XPABST, XZZ, ZTEMP, ZWRES, ZPRES, &
         IIU, IJU, IKU, IKB, IPRES, 'LOG', 'RHU.')
  DO JK=1,IPRES
    ZWORK21(:,:) = ZWRES(:,:,JK)
    YRECFM='UT'//TRIM(YPRES(JK))//'HPA'
    YCOMMENT='X_Y_U component of wind '//TRIM(YPRES(JK))//'hPa (m/s)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END DO
  !
  ZWORK31(:,:,:) = MYF(XVT(:,:,:))
  CALL PINTER(ZWORK31, XPABST, XZZ, ZTEMP, ZWRES, ZPRES, &
          IIU, IJU, IKU, IKB, IPRES, 'LOG', 'RHU.')
  DO JK=1,IPRES
    ZWORK21(:,:) = ZWRES(:,:,JK)
    YRECFM='VT'//TRIM(YPRES(JK))//'HPA'
    YCOMMENT='X_Y_V component of wind '//TRIM(YPRES(JK))//'hPa (m/s)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END DO
! *********************
! Water Vapour Mixing Ratio
! *********************
  CALL PINTER(XRT(:,:,:,1), XPABST, XZZ, ZTEMP, ZWRES, ZPRES, &
         IIU, IJU, IKU, IKB, IPRES, 'LOG', 'RHU.')
  DO JK=1,IPRES
    ZWORK21(:,:) = 1.E+3*ZWRES(:,:,JK)
    YRECFM='MRV'//TRIM(YPRES(JK))//'HPA'
    YCOMMENT='X_Y_Vapor Mixing Ratio '//TRIM(YPRES(JK))//'hPa (g/kg)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END DO
! *********************
! Geopotential in meters
! *********************
  ZWORK31(:,:,:) = MZF(1,IKU,1,XZZ(:,:,:))
  CALL PINTER(ZWORK31, XPABST, XZZ, ZTEMP, ZWRES, ZPRES, &
           IIU, IJU, IKU, IKB, IPRES, 'LOG', 'RHU.')
  DO JK=1,IPRES
    ZWORK21(:,:) = ZWRES(:,:,JK)
    YRECFM='ALT'//TRIM(YPRES(JK))//'HPA'
    YCOMMENT='X_Y_ALTitude '//TRIM(YPRES(JK))//'hPa (m)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END DO
!
  DEALLOCATE(ZWRES,ZPRES,ZWORK32,ZWORK33,ZWORK34)
END IF
!
!-------------------------------------------------------------------------------
!
!*       5.     DIAGNOSTIC ON POTENTIEL TEMPERATURE LEVELS
!               -----------------------------
!
IF (LISOTH .AND.XISOTH(1)/=0.) THEN
!
!
ALLOCATE(ZWORK32(IIU,IJU,IKU))
ALLOCATE(ZWORK33(IIU,IJU,IKU))
ALLOCATE(ZWORK34(IIU,IJU,IKU))
!
! *************************************************
! Determine the potentiel temperature level where to interpolate
! *************************************************
  ITH=0
  DO JI=1,SIZE(XISOTH)
    IF (XISOTH(JI)<=100..OR.XISOTH(JI)>1000.) EXIT
    ITH=ITH+1
    WRITE(YCAR4,'(I4)') INT(XISOTH(JI))
    YTH(ITH)=ADJUSTL(YCAR4)
  END DO

  ALLOCATE(ZWTH(IIU,IJU,ITH))
  ZWTH(:,:,:)=XUNDEF
  ALLOCATE(ZTH(ITH))
  ZTH(:) = XISOTH(1:ITH)

  PRINT *,'POTENTIAL TEMPERATURE LEVELS WHERE TO INTERPOLATE=',ZTH(:)
!
!*       Standard Variables
!
! *********************
! Pressure
! *********************
  CALL ZINTER(XPABST, XTHT, ZWTH, ZTH, IIU, IJU, IKU, IKB, ITH, XUNDEF)
  DO JK=1,ITH
    ZWORK21(:,:) = ZWTH(:,:,JK)
    YRECFM='PABST'//TRIM(YTH(JK))//'K'
    YCOMMENT='X_Y_pressure '//TRIM(YTH(JK))//'K (Pa)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END DO
! *********************
! Potential Vorticity
! *********************
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
  CALL ZINTER(ZPOVO, XTHT, ZWTH, ZTH, IIU, IJU, IKU, IKB, ITH, XUNDEF)
  DO JK=1,ITH
   ZWORK21(:,:) = ZWTH(:,:,JK)
   YRECFM='POVOT'//TRIM(YTH(JK))//'K'
   YCOMMENT='X_Y_POtential VOrticity '//TRIM(YTH(JK))//'K (PVU)'
   IGRID=1
   ILENCH=LEN(YCOMMENT)
   CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END DO
! *********************
! Wind
! *********************
  ZWORK31(:,:,:) = MXF(XUT(:,:,:))
  CALL ZINTER(ZWORK31, XTHT, ZWTH, ZTH, IIU, IJU, IKU, IKB, ITH, XUNDEF)
  DO JK=1,ITH
    ZWORK21(:,:) = ZWTH(:,:,JK)
    YRECFM='UT'//TRIM(YTH(JK))//'K'
    YCOMMENT='X_Y_U component of wind '//TRIM(YTH(JK))//'K (m/s)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END DO
  !
  ZWORK31(:,:,:) = MYF(XVT(:,:,:))
  CALL ZINTER(ZWORK31, XTHT, ZWTH, ZTH, IIU, IJU, IKU, IKB, ITH, XUNDEF)
  DO JK=1,ITH
    ZWORK21(:,:) = ZWTH(:,:,JK)
    YRECFM='VT'//TRIM(YTH(JK))//'K'
    YCOMMENT='X_Y_V component of wind '//TRIM(YTH(JK))//'K (m/s)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK21,IGRID,ILENCH,YCOMMENT,IRESP)
  END DO
!
  DEALLOCATE(ZWTH,ZTH,ZWORK32,ZWORK33,ZWORK34)
END IF
!
!
DEALLOCATE(ZWORK21,ZWORK31,ZTEMP)
!
END SUBROUTINE WRITE_LFIFM1_FOR_DIAG_SUPP 
