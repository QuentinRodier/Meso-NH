!MNH_LIC Copyright 1996-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ###############################
      MODULE MODI_INI_DEEP_CONVECTION
!     ###############################
!
INTERFACE
!
      SUBROUTINE INI_DEEP_CONVECTION(TPINIFILE,OINIDCONV,TPDTCUR,                       &
                                     KCOUNTCONV,PDTHCONV,PDRVCONV,PDRCCONV,             &
                                     PDRICONV,PPRCONV,PPRSCONV,PPACCONV,                &
                                     PUMFCONV,PDMFCONV,PMFCONV,PPRLFLXCONV,PPRSFLXCONV, &
                                     PCAPE,KCLTOPCONV,KCLBASCONV,                       &
                                     TPDTDCONV, HGETSVCONV, PDSVCONV,                   &
                                     OCH_CONV_LINOX, PIC_RATE, PCG_RATE,                &
                                     PIC_TOTAL_NUMBER, PCG_TOTAL_NUMBER                 )
!
USE MODD_IO, ONLY : TFILEDATA
USE MODD_TIME
!
TYPE(TFILEDATA),        INTENT(IN) :: TPINIFILE ! Initial file
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
!     ###################################################################################
      SUBROUTINE INI_DEEP_CONVECTION(TPINIFILE,OINIDCONV,TPDTCUR,                       &
                                     KCOUNTCONV,PDTHCONV,PDRVCONV,PDRCCONV,             &
                                     PDRICONV,PPRCONV,PPRSCONV,PPACCONV,                &
                                     PUMFCONV,PDMFCONV,PMFCONV,PPRLFLXCONV,PPRSFLXCONV, &
                                     PCAPE,KCLTOPCONV,KCLBASCONV,                       &
                                     TPDTDCONV, HGETSVCONV, PDSVCONV,                   &
                                     OCH_CONV_LINOX, PIC_RATE, PCG_RATE,                &
                                     PIC_TOTAL_NUMBER, PCG_TOTAL_NUMBER                 )
!     ###################################################################################
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
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 14/02/2019: remove CLUOUT/CLUOUT0 and associated variables
!  P. Wautelet 04/02/2022: use TSVLIST to manage metadata of scalar variables
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
use modd_field,           only: tfieldmetadata, tfieldlist, TYPEREAL
USE MODD_IO,              ONLY: TFILEDATA
USE MODD_NSV,             ONLY: NSV, NSV_USER, TSVLIST,                               &
                                NSV_AERDEPBEG, NSV_CHICBEG, NSV_CSBEG, NSV_DSTDEPBEG, &
                                NSV_LIMA_BEG, NSV_PPBEG, NSV_SLTDEPBEG, NSV_SNWBEG,   &
                                NSV_AERDEPEND, NSV_CHICEND, NSV_CSEND, NSV_DSTDEPEND, &
                                NSV_LIMA_END, NSV_PPEND, NSV_SLTDEPEND, NSV_SNWEND
USE MODD_TIME
!
use mode_field,           only: Find_field_id_from_mnhname
USE MODE_IO_FIELD_READ,   only: IO_Field_read
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
TYPE(TFILEDATA),        INTENT(IN) :: TPINIFILE ! Initial file
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
INTEGER              :: IID
INTEGER              :: IRESP
INTEGER              :: JSV     ! number of tracers
LOGICAL              :: GOLDFILEFORMAT
LOGICAL              :: GREAD
TYPE(TFIELDMETADATA) :: TZFIELD
!
!-------------------------------------------------------------------------------
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
!If TPINIFILE file was written with a MesoNH version < 5.6, some variables had different names or were not available
GOLDFILEFORMAT = (        TPINIFILE%NMNHVERSION(1) < 5                                       &
                   .OR. ( TPINIFILE%NMNHVERSION(1) == 5 .AND. TPINIFILE%NMNHVERSION(2) < 6 ) )

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
  CALL IO_Field_read(TPINIFILE,'DTDCONV',  TPDTDCONV)
  CALL IO_Field_read(TPINIFILE,'COUNTCONV',KCOUNTCONV)
  CALL IO_Field_read(TPINIFILE,'DTHCONV',  PDTHCONV)
  CALL IO_Field_read(TPINIFILE,'DRVCONV',  PDRVCONV)
  CALL IO_Field_read(TPINIFILE,'DRCCONV',  PDRCCONV)
  CALL IO_Field_read(TPINIFILE,'DRICONV',  PDRICONV)
!
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRCONV',IID,IRESP)
  TZFIELD = TFIELDMETADATA( TFIELDLIST(IID) )
  TZFIELD%CUNITS = 'mm hour-1'
  CALL IO_Field_read(TPINIFILE,TZFIELD,PPRCONV)
  PPRCONV=PPRCONV/(1000.*3600.) ! conversion into m/s units
!
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRSCONV',IID,IRESP)
  TZFIELD = TFIELDMETADATA( TFIELDLIST(IID) )
  TZFIELD%CUNITS = 'mm hour-1'
  CALL IO_Field_read(TPINIFILE,TZFIELD,PPRSCONV)
  PPRSCONV=PPRSCONV/(1000.*3600.) ! conversion into m/s units
!
  CALL FIND_FIELD_ID_FROM_MNHNAME('PACCONV',IID,IRESP)
  TZFIELD = TFIELDMETADATA( TFIELDLIST(IID) )
  TZFIELD%CUNITS = 'mm'
  CALL IO_Field_read(TPINIFILE,TZFIELD,PPACCONV)
  PPACCONV=PPACCONV/1000.       ! conversion into m unit
!
  IF ( OCH_CONV_LINOX ) THEN
    CALL IO_Field_read(TPINIFILE,'IC_RATE',    PIC_RATE)
    CALL IO_Field_read(TPINIFILE,'CG_RATE',    PCG_RATE)
    CALL IO_Field_read(TPINIFILE,'IC_TOTAL_NB',PIC_TOTAL_NUMBER)
    CALL IO_Field_read(TPINIFILE,'CG_TOTAL_NB',PCG_TOTAL_NUMBER)
  END IF
!
!
 GETSVCONV: SELECT CASE(HGETSVCONV)
  CASE('READ') GETSVCONV
    TZFIELD = TFIELDMETADATA(     &
      CMNHNAME   = 'generic for ini_deep_convection', & !Temporary name to ease identification
      CUNITS     = 's-1',         &
      CDIR       = 'XY',          &
      NGRID      = 1,             &
      NTYPE      = TYPEREAL,      &
      NDIMS      = 3,             &
      LTIMEDEP   = .TRUE.         )
    !
    DO JSV = 1, NSV
      GREAD = .TRUE.

      IF ( GOLDFILEFORMAT ) THEN
        IF ( ( JSV >= 1         .AND. JSV <= NSV_USER  ) .OR. &
             ( JSV >= NSV_PPBEG .AND. JSV <= NSV_PPEND )      ) THEN
          WRITE( TZFIELD%CMNHNAME, '( A7, I3.3 )' ) 'DSVCONV', JSV
          TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        ELSE IF ( ( JSV >= NSV_LIMA_BEG  .AND. JSV <= NSV_LIMA_END  ) .OR. &
                  ( JSV >= NSV_CSBEG     .AND. JSV <= NSV_CSEND     ) .OR. &
                  ( JSV >= NSV_CHICBEG   .AND. JSV <= NSV_CHICEND   ) .OR. &
                  ( JSV >= NSV_AERDEPBEG .AND. JSV <= NSV_AERDEPEND ) .OR. &
                  ( JSV >= NSV_DSTDEPBEG .AND. JSV <= NSV_DSTDEPEND ) .OR. &
                  ( JSV >= NSV_SLTDEPBEG .AND. JSV <= NSV_SLTDEPEND ) .OR. &
                  ( JSV >= NSV_SNWBEG    .AND. JSV <= NSV_SNWEND    )      ) THEN
          PDSVCONV(:,:,:,JSV) = 0.0
          GREAD = .FALSE. !This variable was not written in pre-5.6 files
        ELSE
          TZFIELD%CMNHNAME   = 'DSVCONV_' // TRIM( TSVLIST(JSV)%CMNHNAME )
          TZFIELD%CLONGNAME  = 'DSVCONV_' // TRIM( TSVLIST(JSV)%CLONGNAME )
        END IF
      ELSE
        TZFIELD%CMNHNAME   = 'DSVCONV_' // TRIM( TSVLIST(JSV)%CMNHNAME )
        TZFIELD%CLONGNAME  = 'DSVCONV_' // TRIM( TSVLIST(JSV)%CLONGNAME )
      END IF
      WRITE( TZFIELD%CCOMMENT, '( A, I3.3 )' )'X_Y_Z_DSVCONV', JSV
      IF ( GREAD ) CALL IO_Field_read( TPINIFILE, TZFIELD, PDSVCONV(:,:,:,JSV) )
    END DO

  CASE('INIT') GETSVCONV
    PDSVCONV(:,:,:,:) = 0.0

 END SELECT GETSVCONV
!
!
END IF
!
END SUBROUTINE INI_DEEP_CONVECTION
