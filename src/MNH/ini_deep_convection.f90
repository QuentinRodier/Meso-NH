!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!     ###############################
      MODULE MODI_INI_DEEP_CONVECTION
!     ###############################
!
INTERFACE
!
      SUBROUTINE INI_DEEP_CONVECTION(HINIFILE,HLUOUT,OINIDCONV,TPDTCUR,        &
                                     KCOUNTCONV,PDTHCONV,PDRVCONV,PDRCCONV,    &
                                     PDRICONV,PPRCONV,PPRSCONV,PPACCONV,       &
                                     PUMFCONV,PDMFCONV,PMFCONV,PPRLFLXCONV,PPRSFLXCONV,&
                                     PCAPE,KCLTOPCONV,KCLBASCONV,              &
                                     TPDTDCONV, HGETSVCONV, PDSVCONV,          &
                                     OCH_CONV_LINOX, PIC_RATE, PCG_RATE,       &
                                     PIC_TOTAL_NUMBER, PCG_TOTAL_NUMBER        )
!
USE MODD_TIME
!
CHARACTER (LEN=*),      INTENT(IN) :: HINIFILE  ! Name of the initial file
CHARACTER (LEN=*),      INTENT(IN) :: HLUOUT    ! name for output-listing
                                                !  of nested models
LOGICAL,                INTENT(IN) :: OINIDCONV ! switch to initialize or read
TYPE (DATE_TIME),       INTENT(IN) :: TPDTCUR   ! Current date and time
CHARACTER (LEN=*),      INTENT(IN) :: HGETSVCONV ! GET indicator for SVCONV
!
TYPE (DATE_TIME),       INTENT(OUT):: TPDTDCONV ! date and time of the 
                                                ! last deep convection call
INTEGER, DIMENSION(:,:),INTENT(OUT):: KCOUNTCONV! convective counter(recompute
                                                ! tendency or keep it
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDTHCONV  ! convective theta tendency (K/s)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDRVCONV  ! convective r_v tendency (1/s)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDRCCONV  ! convective r_c tendency (1/s)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDRICONV  ! convective r_i tendency (1/s)
REAL, DIMENSION(:,:),   INTENT(OUT):: PPRCONV   ! total (liquid+solid) surf.
                                                ! precipitation tendency (m/s)
REAL, DIMENSION(:,:),   INTENT(OUT):: PPRSCONV  ! solid surface
                                                ! precipitation tendency (m/s)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PUMFCONV  ! updraft mass flux (kg/s m2)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDMFCONV  ! downdraft mass flux (kg/s m2)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PMFCONV   ! convective mass flux (kg/s m2)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PPRLFLXCONV!liquid precip flux (m/s)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PPRSFLXCONV!solid  precip flux (m/s)
REAL, DIMENSION(:,:),   INTENT(OUT):: PCAPE     ! CAPE (J)
INTEGER, DIMENSION(:,:),INTENT(OUT):: KCLTOPCONV! convective cloud top level 
INTEGER, DIMENSION(:,:),INTENT(OUT):: KCLBASCONV! convective cloud base level
REAL, DIMENSION(:,:),   INTENT(OUT):: PPACCONV  ! accumulated convective
                                                ! precipitation (m)
REAL, DIMENSION(:,:,:,:),INTENT(OUT):: PDSVCONV ! conv. tracer tendencies (1/s)
LOGICAL,                INTENT(IN)    :: OCH_CONV_LINOX ! Flag to compute LiNOx
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PIC_RATE ! IC lightning frequency
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PCG_RATE ! CG lightning frequency
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PIC_TOTAL_NUMBER ! Total number of IC 
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PCG_TOTAL_NUMBER ! Total number of CG
!
END SUBROUTINE INI_DEEP_CONVECTION
!
END INTERFACE
!
END MODULE MODI_INI_DEEP_CONVECTION
!     ##########################################################################
      SUBROUTINE INI_DEEP_CONVECTION(HINIFILE,HLUOUT,OINIDCONV,TPDTCUR,        &
                                     KCOUNTCONV,PDTHCONV,PDRVCONV,PDRCCONV,    &
                                     PDRICONV,PPRCONV,PPRSCONV,PPACCONV,       &
                                     PUMFCONV,PDMFCONV,PMFCONV,PPRLFLXCONV,PPRSFLXCONV,&
                                     PCAPE,KCLTOPCONV,KCLBASCONV,              &
                                     TPDTDCONV, HGETSVCONV, PDSVCONV,          &
                                     OCH_CONV_LINOX, PIC_RATE, PCG_RATE,       &
                                     PIC_TOTAL_NUMBER, PCG_TOTAL_NUMBER        )
!     ##########################################################################
!
!!**** Routine to initialize the convective tendencies and the
!!     convective counter
!!
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation ( routine INI_DEEP_CONVECTION )
!!
!!    AUTHOR
!!    ------
!!	  P. Bechtold      * Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    26/03/96
!!      Pinty J.-P. 29/01/97  Unit conversion for PPACCONV and PPRCONV
!!      Bechtold P. 24/01/98  Initialisation of tracer tendencies
!!      Asencio N.  13/08/98   parallel code: ILENG no longer used
!!      Bechtold P. 11/12/98  Drop PWSUBCONV and add surf solid precip. +
!!                            diagnostics
!!      D.Gazen       22/01/01 use MODD_NSV and add names to scalar variables
!!      P.Jabouille   04/04/02 add PMFCONV used for subgrid condensation
!!                    for a correct restart this variable has to be writen in FM file
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TIME
USE MODD_CONVPAR
USE MODD_CH_M9_n,         ONLY: CNAMES
USE MODD_RAIN_C2R2_DESCR, ONLY: C2R2NAMES
USE MODD_ICE_C1R3_DESCR,  ONLY : C1R3NAMES
USE MODD_ELEC_DESCR,      ONLY : CELECNAMES
USE MODD_LG,              ONLY: CLGNAMES
USE MODD_NSV, ONLY : NSV,NSV_USER,NSV_CHEMBEG,NSV_CHEMEND,NSV_C2R2BEG,NSV_C2R2END, &
                     NSV_LGBEG,NSV_LGEND,NSV_LNOXBEG,NSV_LNOXEND, &
                     NSV_DSTBEG,NSV_DSTEND, NSV_AERBEG,NSV_AEREND, &
                     NSV_SLTBEG,NSV_SLTEND, NSV_PPBEG,NSV_PPEND, &
                     NSV_C1R3BEG,NSV_C1R3END, NSV_ELECBEG,NSV_ELECEND
USE MODD_CH_AEROSOL, ONLY : CAERONAMES
USE MODD_DUST, ONLY : CDUSTNAMES
USE MODD_SALT, ONLY : CSALTNAMES
!
USE MODE_FM
!
USE MODE_FMREAD
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER (LEN=*),      INTENT(IN) :: HINIFILE  ! Name of the initial file
CHARACTER (LEN=*),      INTENT(IN) :: HLUOUT    ! name for output-listing
                                                !  of nested models
LOGICAL,                INTENT(IN) :: OINIDCONV ! switch to initialize or read
TYPE (DATE_TIME),       INTENT(IN) :: TPDTCUR   ! Current date and time
CHARACTER (LEN=*),      INTENT(IN) :: HGETSVCONV ! GET indicator for SVCONV
!
TYPE (DATE_TIME),       INTENT(OUT):: TPDTDCONV ! date and time of the 
                                                ! last deep convection call
INTEGER, DIMENSION(:,:),INTENT(OUT):: KCOUNTCONV! convective counter(recompute
                                                ! tendency or keep it
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDTHCONV  ! convective theta tendency (K/s)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDRVCONV  ! convective r_v tendency (1/s)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDRCCONV  ! convective r_c tendency (1/s)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDRICONV  ! convective r_i tendency (1/s)
REAL, DIMENSION(:,:),   INTENT(OUT):: PPRCONV   ! total (liquid+solid) surf.
                                                ! precipitation tendency (m/s)
REAL, DIMENSION(:,:),   INTENT(OUT):: PPACCONV  ! accumulated convective
                                                ! precipitation (m)
REAL, DIMENSION(:,:),   INTENT(OUT):: PPRSCONV  ! solid surface
                                                ! precipitation tendency (m/s)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PUMFCONV  ! updraft mass flux (kg/s m2)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDMFCONV  ! downdraft mass flux (kg/s m2)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PMFCONV   ! convective mass flux (kg/s m2)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PPRLFLXCONV!liquid precip flux (m/s)
REAL, DIMENSION(:,:,:), INTENT(OUT):: PPRSFLXCONV!solid  precip flux (m/s)
REAL, DIMENSION(:,:),   INTENT(OUT):: PCAPE     ! CAPE (J)
INTEGER, DIMENSION(:,:),INTENT(OUT):: KCLTOPCONV! convective cloud top level 
INTEGER, DIMENSION(:,:),INTENT(OUT):: KCLBASCONV! convective cloud base level
REAL, DIMENSION(:,:,:,:),INTENT(OUT):: PDSVCONV ! conv. tracer tendencies (1/s)
LOGICAL,                INTENT(IN)    :: OCH_CONV_LINOX ! Flag to compute LiNOx
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PIC_RATE ! IC lightning frequency
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PCG_RATE ! CG lightning frequency
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PIC_TOTAL_NUMBER ! Total number of IC 
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PCG_TOTAL_NUMBER ! Total number of CG
!
!
!
!*       0.2   declarations of local variables
!
!
INTEGER                :: ILUOUT
INTEGER                :: IGRID,ILENCH,IRESP  !   File 
CHARACTER (LEN=16)     :: YRECFM                    ! management
CHARACTER (LEN=100)    :: YCOMMENT                  ! variables  
INTEGER                :: JSV                       ! number of tracers
!
!-------------------------------------------------------------------------------
!
CALL FMLOOK_ll(HLUOUT,HLUOUT,ILUOUT,IRESP)
!
!*       1. INITIALIZE CONSTANTS USED IN DEEP CONVECTION PARAMETERIZATION
!	        -------------------------------------------------------------
!
! call of INI_CONVPAR is now in routine CONVECTION
!
!
!*       2. INITIALIZE CONVECTIVE TENDENCIES
!	        --------------------------------
!
PUMFCONV(:,:,:)  = 0.0
PDMFCONV(:,:,:)  = 0.0
PMFCONV(:,:,:)   = 0.0  ! warning, restart may be incorrect
PPRLFLXCONV(:,:,:)=0.0
PPRSFLXCONV(:,:,:)=0.0
PCAPE(:,:)       = 0.0
KCLTOPCONV(:,:)  = 0
KCLBASCONV(:,:)  = 0
!
IF ( OINIDCONV ) THEN
  TPDTDCONV        = TPDTCUR
  KCOUNTCONV(:,:)  = 1
  PDTHCONV(:,:,:)  = 0.0
  PDRVCONV(:,:,:)  = 0.0
  PDRCCONV(:,:,:)  = 0.0
  PDRICONV(:,:,:)  = 0.0
  PPRCONV(:,:)     = 0.0
  PPRSCONV(:,:)    = 0.0
  PPACCONV(:,:)    = 0.0
  PDSVCONV(:,:,:,:) = 0.0
  IF ( OCH_CONV_LINOX ) THEN
    PIC_RATE(:,:) = 0.
    PCG_RATE(:,:) = 0.
    PIC_TOTAL_NUMBER(:,:) = 0.
    PCG_TOTAL_NUMBER(:,:) = 0.
  END IF
!
ELSE
!
  YRECFM='DTDCONV' 
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'--',TPDTDCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
  YRECFM      = 'COUNTCONV'
  YCOMMENT    = 'X_Y_COUNTCONV'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',KCOUNTCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
  YRECFM      = 'DTHCONV'
  YCOMMENT    = 'X_Y_Z_DTHCONV (K/S)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDTHCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
  YRECFM      = 'DRVCONV'
  YCOMMENT    = 'X_Y_Z_DRVCONV (1/S)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDRVCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
  YRECFM      = 'DRCCONV'
  YCOMMENT    = 'X_Y_Z_DRCCONV (1/S)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDRCCONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
  YRECFM      = 'DRICONV'
  YCOMMENT    = 'X_Y_Z_DRICONV (1/S)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDRICONV,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
  YRECFM      = 'PRCONV'
  YCOMMENT    = 'X_Y_PRCONV (MM/H)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PPRCONV,IGRID,ILENCH,YCOMMENT,IRESP)
  PPRCONV=PPRCONV/(1000.*3600.) ! conversion into m/s units
!
!
  YRECFM      = 'PRSCONV'
  YCOMMENT    = 'X_Y_PRSCONV (MM/H)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PPRSCONV,IGRID,ILENCH,YCOMMENT,IRESP)
  PPRSCONV=PPRSCONV/(1000.*3600.) ! conversion into m/s units
!
!
  YRECFM      = 'PACCONV'
  YCOMMENT    = 'X_Y_PACCONV (MM)'
  IGRID       = 1
  ILENCH      = LEN(YCOMMENT)
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PPACCONV,IGRID,ILENCH,YCOMMENT,IRESP)
  PPACCONV=PPACCONV/1000.       ! conversion into m unit
!
  IF ( OCH_CONV_LINOX ) THEN
    YRECFM      = 'IC_RATE'
    YCOMMENT    = 'X_Y_IC_RATE (/s)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PIC_RATE,IGRID,ILENCH, &
                                                          YCOMMENT,IRESP)
!
!
    YRECFM      = 'CG_RATE'
    YCOMMENT    = 'X_Y_CG_RATE (/s)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PCG_RATE,IGRID,ILENCH, &
                                                          YCOMMENT,IRESP)
!
!
    YRECFM      = 'IC_TOTAL_NB'
    YCOMMENT    = 'X_Y_IC_TOTAL_NUMBER (no unit)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PIC_TOTAL_NUMBER,IGRID,ILENCH, &
                                                          YCOMMENT,IRESP)
!
!
    YRECFM      = 'CG_TOTAL_NB'
    YCOMMENT    = 'X_Y_CG_TOTAL_NUMBER (no unit)'
    IGRID       = 1
    ILENCH      = LEN(YCOMMENT)
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PCG_TOTAL_NUMBER,IGRID,ILENCH, &
                                                          YCOMMENT,IRESP)
  END IF
!
!
 SELECT CASE(HGETSVCONV)
  CASE('READ')
    IGRID=1      
    DO JSV = 1, NSV_USER
      WRITE(YRECFM,'(A7,I3.3)')'DSVCONV',JSV
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH      = LEN(YCOMMENT)
      CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_C2R2BEG, NSV_C2R2END
      YRECFM = 'DSVCONV_'//TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_C1R3BEG, NSV_C1R3END
      YRECFM = 'DSVCONV_'//TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_ELECBEG, NSV_ELECEND
      YRECFM = 'DSVCONV_'//TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_CHEMBEG, NSV_CHEMEND
      YRECFM = 'DSVCONV_'//TRIM(UPCASE(CNAMES(JSV-NSV_CHEMBEG+1)))
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_AERBEG, NSV_AEREND
      YRECFM = 'DSVCONV_'//TRIM(UPCASE(CAERONAMES(JSV-NSV_AERBEG+1)))
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_LNOXBEG,NSV_LNOXEND
      YRECFM='DSVCONV_LINOX'
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH=LEN(YCOMMENT)
      CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_DSTBEG, NSV_DSTEND
      YRECFM = 'DSVCONV_'//TRIM(UPCASE(CDUSTNAMES(JSV-NSV_DSTBEG+1)))
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_SLTBEG, NSV_SLTEND
      YRECFM = 'DSVCONV_'//TRIM(UPCASE(CSALTNAMES(JSV-NSV_SLTBEG+1)))
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_LGBEG, NSV_LGEND
      YRECFM = 'DSVCONV_'//TRIM(CLGNAMES(JSV-NSV_LGBEG+1))
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
    DO JSV = NSV_PPBEG, NSV_PPEND
      WRITE(YRECFM,'(A7,I3.3)')'DSVCONV',JSV
      WRITE(YCOMMENT,'(A6,A7,I3.3,A6)')'X_Y_Z_','DSVCONV',JSV,' (1/S)'
      ILENCH = LEN(YCOMMENT)
      CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDSVCONV(:,:,:,JSV),          &
           IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
 END SELECT
!
!
END IF
!  
CONTAINS
FUNCTION UPCASE(HSTRING)

CHARACTER(LEN=*)            :: HSTRING
CHARACTER(LEN=LEN(HSTRING)) :: UPCASE

INTEGER :: JC
INTEGER, PARAMETER :: IAMIN = IACHAR("a")
INTEGER, PARAMETER :: IAMAJ = IACHAR("A")

DO JC=1,LEN(HSTRING)
  IF (HSTRING(JC:JC) >= "a" .AND. HSTRING(JC:JC) <= "z") THEN
      UPCASE(JC:JC) = ACHAR(IACHAR(HSTRING(JC:JC)) - IAMIN + IAMAJ)
  ELSE
      UPCASE(JC:JC) = HSTRING(JC:JC)
  END IF
END DO

END FUNCTION UPCASE
!
END SUBROUTINE INI_DEEP_CONVECTION
