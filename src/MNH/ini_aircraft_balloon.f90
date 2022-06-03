!MNH_LIC Copyright 2000-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 01/10/2020: bugfix: DEFAULT_FLYER: add missing default values
!  P. Wautelet    06/2022: reorganize flyers
!-----------------------------------------------------------------

!      #########################
MODULE MODI_INI_AIRCRAFT_BALLOON
!      #########################
!
INTERFACE
!
      SUBROUTINE INI_AIRCRAFT_BALLOON(TPINIFILE,                    &
                                      PTSTEP, TPDTSEG, PSEGLEN,     &
                                      KRR, KSV, KKU, OUSETKE,       &
                                      PLATOR, PLONOR                )
!
USE MODD_IO, ONLY: TFILEDATA
USE MODD_TYPE_DATE
!
TYPE(TFILEDATA),    INTENT(IN) :: TPINIFILE !Initial file
REAL,               INTENT(IN) :: PTSTEP  ! time step
TYPE(DATE_TIME),    INTENT(IN) :: TPDTSEG ! segment date and time
REAL,               INTENT(IN) :: PSEGLEN ! segment length
INTEGER,            INTENT(IN) :: KRR     ! number of moist variables
INTEGER,            INTENT(IN) :: KSV     ! number of scalar variables
INTEGER,            INTENT(IN) :: KKU     ! number of vertical levels 
LOGICAL,            INTENT(IN) :: OUSETKE ! flag to use tke
REAL,               INTENT(IN) :: PLATOR  ! latitude of origine point
REAL,               INTENT(IN) :: PLONOR  ! longitude of origine point
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_AIRCRAFT_BALLOON
!
END INTERFACE
!
END MODULE MODI_INI_AIRCRAFT_BALLOON
!
!     ###############################################################
      SUBROUTINE INI_AIRCRAFT_BALLOON(TPINIFILE,                    &
                                      PTSTEP, TPDTSEG, PSEGLEN,     &
                                      KRR, KSV, KKU, OUSETKE,       &
                                      PLATOR, PLONOR                )
!     ###############################################################
!
!
!!****  *INI_AIRCRAFT_BALLOON* -
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      Valery Masson             * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!     Original 15/05/2000
!!               Apr, 20 2001: G.Jaubert: use in diag  with stationnary fields
!!               March, 2013 : O.Caumont, C.Lac : add vertical profiles
!!               OCT,2016 : G.Delautier LIMA
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!!
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_AIRCRAFT_BALLOON
USE MODD_CONF
USE MODD_DIAG_FLAG
USE MODD_DYN_n
use modd_field,      only: tfieldmetadata, TYPEREAL
USE MODD_GRID
USE MODD_IO,         ONLY: TFILEDATA
USE MODD_LUNIT_n,    ONLY: TLUOUT
USE MODD_PARAM_n,    ONLY: CCLOUD
USE MODD_PARAMETERS
!
USE MODE_GRIDPROJ
USE MODE_ll
USE MODE_MODELN_HANDLER
USE MODE_MSG
!
USE MODI_INI_BALLOON
USE MODI_INI_AIRCRAFT
!
IMPLICIT NONE
!
!*      0.1  declarations of arguments
!
TYPE(TFILEDATA),    INTENT(IN) :: TPINIFILE !Initial file
REAL,               INTENT(IN) :: PTSTEP  ! time step
TYPE(DATE_TIME),    INTENT(IN) :: TPDTSEG ! segment date and time
REAL,               INTENT(IN) :: PSEGLEN ! segment length
INTEGER,            INTENT(IN) :: KRR     ! number of moist variables
INTEGER,            INTENT(IN) :: KSV     ! number of scalar variables
INTEGER,            INTENT(IN) :: KKU     ! number of vertical levels 
LOGICAL,            INTENT(IN) :: OUSETKE ! flag to use tke
REAL,               INTENT(IN) :: PLATOR  ! latitude of origine point
REAL,               INTENT(IN) :: PLONOR  ! longitude of origine point
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
INTEGER :: IMI    ! current model index
INTEGER :: ISTORE ! number of storage instants
INTEGER :: ILUOUT ! logical unit
INTEGER :: IRESP  ! return code
INTEGER :: JI
INTEGER :: JSEG   ! loop counter
TYPE(TFIELDMETADATA) :: TZFIELD
!
!----------------------------------------------------------------------------
!
IMI=GET_CURRENT_MODEL_INDEX()
ILUOUT = TLUOUT%NLU
!----------------------------------------------------------------------------
!
!*      1.   Default values
!            --------------
!
IF ( CPROGRAM == 'DIAG  ') THEN
  IF ( .NOT. LAIRCRAFT_BALLOON ) RETURN
  IF (NTIME_AIRCRAFT_BALLOON == NUNDEF .OR. XSTEP_AIRCRAFT_BALLOON == XUNDEF) THEN
    WRITE(ILUOUT,*) "NTIME_AIRCRAFT_BALLOON and/or  XSTEP_AIRCRAFT_BALLOON not initialized in DIAG "
    WRITE(ILUOUT,*) "No calculations for Balloons and Aircraft"
    LAIRCRAFT_BALLOON=.FALSE.
    RETURN
  ENDIF
ENDIF
!
!
IF ( IMI == 1 ) THEN
  LFLYER=.FALSE.
END IF
!
!----------------------------------------------------------------------------
!
!*      2.   Balloon initialization
!            ----------------------
IF (IMI == 1) CALL INI_BALLOON
!
DO JI = 1, NBALLOONS
  CALL INI_LAUNCH( JI, TBALLOONS(JI) )
END DO
!
!----------------------------------------------------------------------------
!
!*      3.   Aircraft initialization
!            -----------------------
!
IF (IMI == 1) CALL INI_AIRCRAFT
!
DO JI = 1, NAIRCRAFTS
  CALL INI_FLIGHT( JI, TAIRCRAFTS(JI) )
END DO
!
!----------------------------------------------------------------------------
!
!*      4.   Allocations of storage arrays
!            -----------------------------
!
IF (.NOT. LFLYER) RETURN
!
DO JI = 1, NBALLOONS
  CALL ALLOCATE_FLYER( TBALLOONS(JI) )
END DO
!
DO JI = 1, NAIRCRAFTS
  CALL ALLOCATE_FLYER( TAIRCRAFTS(JI) )
END DO
!
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!
CONTAINS
!
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE ALLOCATE_FLYER(TPFLYER)
!
!
CLASS(TFLYERDATA), INTENT(INOUT) :: TPFLYER
!
IF (TPFLYER%NMODEL > NMODEL) TPFLYER%NMODEL=0
IF (IMI /= TPFLYER%NMODEL .AND. .NOT. (IMI==1 .AND. TPFLYER%NMODEL==0) ) RETURN
!
IF ( CPROGRAM == 'DIAG  ' ) THEN
  ISTORE = INT ( NTIME_AIRCRAFT_BALLOON / TPFLYER%TFLYER_TIME%XTSTEP ) + 1
ELSE
  ISTORE = NINT ( ( PSEGLEN - DYN_MODEL(1)%XTSTEP ) / TPFLYER%TFLYER_TIME%XTSTEP ) + 1
ENDIF
!
IF (TPFLYER%NMODEL == 0) ISTORE=0
IF (TPFLYER%NMODEL > 0) THEN
  WRITE(ILUOUT,*) 'Aircraft or Balloon:',TPFLYER%CTITLE,' nmodel=',TPFLYER%NMODEL
ENDIF
!
!
allocate( tpflyer%tflyer_time%tpdates(istore) )
ALLOCATE(TPFLYER%XX   (ISTORE))
ALLOCATE(TPFLYER%XY   (ISTORE))
ALLOCATE(TPFLYER%XZ   (ISTORE))
ALLOCATE(TPFLYER%XLON (ISTORE))
ALLOCATE(TPFLYER%XLAT (ISTORE))
ALLOCATE(TPFLYER%XZON (ISTORE))
ALLOCATE(TPFLYER%XMER (ISTORE))
ALLOCATE(TPFLYER%XW   (ISTORE))
ALLOCATE(TPFLYER%XP   (ISTORE))
ALLOCATE(TPFLYER%XTH  (ISTORE))
ALLOCATE(TPFLYER%XR   (ISTORE,KRR))
ALLOCATE(TPFLYER%XSV  (ISTORE,KSV))
ALLOCATE(TPFLYER%XRTZ (ISTORE,KKU))
ALLOCATE(TPFLYER%XRZ  (ISTORE,KKU,KRR))
ALLOCATE(TPFLYER%XFFZ (ISTORE,KKU))
ALLOCATE(TPFLYER%XIWCZ(ISTORE,KKU))
ALLOCATE(TPFLYER%XLWCZ(ISTORE,KKU))
ALLOCATE(TPFLYER%XCIZ (ISTORE,KKU))
IF (CCLOUD=='LIMA') THEN
  ALLOCATE(TPFLYER%XCCZ(ISTORE,KKU))
  ALLOCATE(TPFLYER%XCRZ(ISTORE,KKU))
ENDIF
ALLOCATE(TPFLYER%XCRARE    (ISTORE,KKU))
ALLOCATE(TPFLYER%XCRARE_ATT(ISTORE,KKU))
ALLOCATE(TPFLYER%XWZ       (ISTORE,KKU))
ALLOCATE(TPFLYER%XZZ       (ISTORE,KKU))
IF (OUSETKE) THEN
  ALLOCATE(TPFLYER%XTKE(ISTORE))
ELSE
  ALLOCATE(TPFLYER%XTKE(0))
END IF
ALLOCATE(TPFLYER%XTKE_DISS(ISTORE))
ALLOCATE(TPFLYER%XTSRAD   (ISTORE))
ALLOCATE(TPFLYER%XZS      (ISTORE))
!
ALLOCATE(TPFLYER%XTHW_FLUX(ISTORE))
ALLOCATE(TPFLYER%XRCW_FLUX(ISTORE))
ALLOCATE(TPFLYER%XSVW_FLUX(ISTORE,KSV))
!
TPFLYER%XX   = XUNDEF
TPFLYER%XY   = XUNDEF
TPFLYER%XZ   = XUNDEF
TPFLYER%XLON = XUNDEF
TPFLYER%XLAT = XUNDEF
TPFLYER%XZON = XUNDEF
TPFLYER%XMER = XUNDEF
TPFLYER%XW   = XUNDEF
TPFLYER%XP   = XUNDEF
TPFLYER%XTH  = XUNDEF
TPFLYER%XR   = XUNDEF
TPFLYER%XSV  = XUNDEF
TPFLYER%XRTZ = XUNDEF
TPFLYER%XRZ  = XUNDEF
TPFLYER%XFFZ = XUNDEF
TPFLYER%XCIZ = XUNDEF
IF (CCLOUD=='LIMA') THEN
  TPFLYER%XCRZ = XUNDEF
  TPFLYER%XCCZ = XUNDEF
ENDIF
TPFLYER%XIWCZ      = XUNDEF
TPFLYER%XLWCZ      = XUNDEF
TPFLYER%XCRARE     = XUNDEF
TPFLYER%XCRARE_ATT = XUNDEF
TPFLYER%XWZ        = XUNDEF
TPFLYER%XZZ        = XUNDEF
TPFLYER%XTKE       = XUNDEF
TPFLYER%XTSRAD     = XUNDEF
TPFLYER%XZS        = XUNDEF
TPFLYER%XTKE_DISS  = XUNDEF
!
TPFLYER%XTHW_FLUX = XUNDEF
TPFLYER%XRCW_FLUX = XUNDEF
TPFLYER%XSVW_FLUX = XUNDEF

END SUBROUTINE ALLOCATE_FLYER
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE INI_LAUNCH(KNBR,TPFLYER)
!
use MODE_IO_FIELD_READ, only: IO_Field_read
!
INTEGER,             INTENT(IN)    :: KNBR
CLASS(TBALLOONDATA), INTENT(INOUT) :: TPFLYER
!
!
!
!*      0.2  declaration of local variables
!
REAL :: ZLAT ! latitude of the balloon
REAL :: ZLON ! longitude of the balloon
!
IF (TPFLYER%CMODEL == 'MOB' .AND. TPFLYER%NMODEL /= 0) TPFLYER%NMODEL=1
IF (TPFLYER%NMODEL > NMODEL) TPFLYER%NMODEL=0
IF ( IMI /= TPFLYER%NMODEL ) RETURN
!
LFLYER=.TRUE.
!
IF (TPFLYER%CTITLE=='          ') THEN
  WRITE(TPFLYER%CTITLE,FMT='(A6,I2.2)') TPFLYER%CTYPE,KNBR
END IF
!
IF ( CPROGRAM == 'MESONH' .OR. CPROGRAM == 'SPAWN ' .OR. CPROGRAM == 'REAL  ' ) THEN
  ! read the current location in the FM_FILE
  !
  TZFIELD = TFIELDMETADATA(                   &
    CMNHNAME   = TRIM(TPFLYER%CTITLE)//'LAT', &
    CSTDNAME   = '',                          &
    CLONGNAME  = TRIM(TPFLYER%CTITLE)//'LAT', &
    CUNITS     = 'degree',                    &
    CDIR       = '--',                        &
    CCOMMENT   = '',                          &
    NGRID      = 0,                           &
    NTYPE      = TYPEREAL,                    &
    NDIMS      = 0,                           &
    LTIMEDEP   = .TRUE.                       )
  CALL IO_Field_read(TPINIFILE,TZFIELD,ZLAT,IRESP)
  !
  IF ( IRESP /= 0 ) THEN
    WRITE(ILUOUT,*) "INI_LAUNCH: Initial location take for ",TPFLYER%CTITLE
  ELSE
    TZFIELD = TFIELDMETADATA(                   &
      CMNHNAME   = TRIM(TPFLYER%CTITLE)//'LON', &
      CSTDNAME   = '',                          &
      CLONGNAME  = TRIM(TPFLYER%CTITLE)//'LON', &
      CUNITS     = 'degree',                    &
      CDIR       = '--',                        &
      CCOMMENT   = '',                          &
      NGRID      = 0,                           &
      NTYPE      = TYPEREAL,                    &
      NDIMS      = 0,                           &
      LTIMEDEP   = .TRUE.                       )
    CALL IO_Field_read(TPINIFILE,TZFIELD,ZLON)
    !
    TZFIELD = TFIELDMETADATA(                   &
      CMNHNAME   = TRIM(TPFLYER%CTITLE)//'ALT', &
      CSTDNAME   = '',                          &
      CLONGNAME  = TRIM(TPFLYER%CTITLE)//'ALT', &
      CUNITS     = 'm',                         &
      CDIR       = '--',                        &
      CCOMMENT   = '',                          &
      NGRID      = 0,                           &
      NTYPE      = TYPEREAL,                    &
      NDIMS      = 0,                           &
      LTIMEDEP   = .TRUE.                       )
    CALL IO_Field_read(TPINIFILE,TZFIELD,TPFLYER%XZ_CUR)
    !
    TPFLYER%XP_CUR   = XUNDEF
    !
    TZFIELD = TFIELDMETADATA(                       &
      CMNHNAME   = TRIM(TPFLYER%CTITLE)//'WASCENT', &
      CSTDNAME   = '',                              &
      CLONGNAME  = TRIM(TPFLYER%CTITLE)//'WASCENT', &
      CUNITS     = 'm s-1',                         &
      CDIR       = '--',                            &
      CCOMMENT   = '',                              &
      NGRID      = 0,                               &
      NTYPE      = TYPEREAL,                        &
      NDIMS      = 0,                               &
      LTIMEDEP   = .TRUE.                           )
    CALL IO_Field_read(TPINIFILE,TZFIELD,TPFLYER%XWASCENT)
    !
    TZFIELD = TFIELDMETADATA(                   &
      CMNHNAME   = TRIM(TPFLYER%CTITLE)//'RHO', &
      CSTDNAME   = '',                          &
      CLONGNAME  = TRIM(TPFLYER%CTITLE)//'RHO', &
      CUNITS     = 'kg m-3',                    &
      CDIR       = '--',                        &
      CCOMMENT   = '',                          &
      NGRID      = 0,                           &
      NTYPE      = TYPEREAL,                    &
      NDIMS      = 0,                           &
      LTIMEDEP   = .TRUE.                       )
    CALL IO_Field_read(TPINIFILE,TZFIELD,TPFLYER%XRHO)
    !
    CALL SM_XYHAT( PLATOR, PLONOR, ZLAT, ZLON, TPFLYER%XX_CUR, TPFLYER%XY_CUR )
    TPFLYER%LFLY = .TRUE.
    WRITE(ILUOUT,*) &
    "INI_LAUNCH: Current location read in FM file for ",TPFLYER%CTITLE
    IF (TPFLYER%CTYPE== 'CVBALL') THEN
      WRITE(ILUOUT,*) &
       " Lat=",ZLAT," Lon=",ZLON," Alt=",TPFLYER%XZ_CUR," Wasc=",TPFLYER%XWASCENT
    ELSE IF (TPFLYER%CTYPE== 'ISODEN') THEN
      WRITE(ILUOUT,*) &
       " Lat=",ZLAT," Lon=",ZLON," Rho=",TPFLYER%XRHO
    END IF
    !
    TPFLYER%TFLYER_TIME%XTSTEP  = MAX ( PTSTEP, TPFLYER%TFLYER_TIME%XTSTEP )
  END IF
  !
ELSE IF (CPROGRAM == 'DIAG  ' ) THEN
  IF ( LAIRCRAFT_BALLOON ) THEN
    ! read the current location in MODD_DIAG_FLAG
    !
    ZLAT=XLAT_BALLOON(KNBR)
    ZLON=XLON_BALLOON(KNBR)
    TPFLYER%XZ_CUR=XALT_BALLOON(KNBR)
    IF (TPFLYER%XZ_CUR /= XUNDEF .AND. ZLAT /= XUNDEF .AND. ZLON /= XUNDEF ) THEN
      CALL SM_XYHAT( PLATOR, PLONOR, ZLAT, ZLON, TPFLYER%XX_CUR, TPFLYER%XY_CUR )
      TPFLYER%LFLY = .TRUE.
      WRITE(ILUOUT,*) &
      "INI_LAUNCH: Current location read in MODD_DIAG_FLAG for ",TPFLYER%CTITLE
      WRITE(ILUOUT,*) &
            " Lat=",ZLAT," Lon=",ZLON," Alt=",TPFLYER%XZ_CUR
    END IF
    !
    TPFLYER%TFLYER_TIME%XTSTEP  = MAX (XSTEP_AIRCRAFT_BALLOON , TPFLYER%TFLYER_TIME%XTSTEP )
  END IF
END IF
!
IF ( TPFLYER%XLATLAUNCH == XUNDEF .OR. TPFLYER%XLONLAUNCH == XUNDEF ) THEN
  CMNHMSG(1) = 'Error in balloon initial position (balloon ' // TRIM( TPFLYER%CTITLE ) // ' )'
  CMNHMSG(2) = 'either LATitude or LONgitude is not given'
  CMNHMSG(3) = 'Check your INI_BALLOON routine'
  CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_AIRCRAFT_BALLOON' )
END IF
!
CALL SM_XYHAT( PLATOR, PLONOR, TPFLYER%XLATLAUNCH, TPFLYER%XLONLAUNCH, TPFLYER%XXLAUNCH, TPFLYER%XYLAUNCH )
!
END SUBROUTINE INI_LAUNCH
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE INI_FLIGHT(KNBR,TPFLYER)
!
INTEGER,              INTENT(IN)    :: KNBR
CLASS(TAIRCRAFTDATA), INTENT(INOUT) :: TPFLYER
!
IF (TPFLYER%CMODEL == 'MOB' .AND. TPFLYER%NMODEL /= 0) TPFLYER%NMODEL=1
IF (TPFLYER%NMODEL > NMODEL) TPFLYER%NMODEL=0
IF ( IMI /= TPFLYER%NMODEL ) RETURN
!
LFLYER=.TRUE.
!
TPFLYER%TFLYER_TIME%XTSTEP  = MAX ( PTSTEP, TPFLYER%TFLYER_TIME%XTSTEP )

IF (TPFLYER%CTITLE=='          ') THEN
  WRITE(TPFLYER%CTITLE,FMT='(A6,I2.2)') TPFLYER%CTYPE,KNBR
END IF

IF ( TPFLYER%NSEG == 0 ) THEN
  CMNHMSG(1) = 'Error in aircraft flight path (aircraft ' // TRIM( TPFLYER%CTITLE ) // ' )'
  CMNHMSG(2) = 'There is ZERO flight segment defined.'
  CMNHMSG(3) = 'Check your INI_AIRCRAFT routine'
  CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_FLIGHT' )
END IF
!
IF ( ANY(TPFLYER%XSEGLAT(:)==XUNDEF) .OR. ANY(TPFLYER%XSEGLON(:)==XUNDEF) ) THEN
  CMNHMSG(1) = 'Error in aircraft flight path (aircraft ' // TRIM( TPFLYER%CTITLE ) // ' )'
  CMNHMSG(2) = 'either LATitude or LONgitude segment'
  CMNHMSG(3) = 'definiton is not complete.'
  CMNHMSG(4) = 'Check your INI_AIRCRAFT routine'
  CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_FLIGHT' )
END IF
!
ALLOCATE(TPFLYER%XSEGX(TPFLYER%NSEG+1))
ALLOCATE(TPFLYER%XSEGY(TPFLYER%NSEG+1))
!
DO JSEG=1,TPFLYER%NSEG+1
  CALL SM_XYHAT( PLATOR, PLONOR, TPFLYER%XSEGLAT(JSEG), TPFLYER%XSEGLON(JSEG), TPFLYER%XSEGX(JSEG), TPFLYER%XSEGY(JSEG) )
END DO
!
IF ( ANY(TPFLYER%XSEGTIME(:)==XUNDEF) ) THEN
  CMNHMSG(1) = 'Error in aircraft flight path (aircraft ' // TRIM( TPFLYER%CTITLE ) // ' )'
  CMNHMSG(2) = 'definiton of segment duration is not complete.'
  CMNHMSG(3) = 'Check your INI_AIRCRAFT routine'
  CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_AIRCRAFT_BALLOON' )
END IF

END SUBROUTINE INI_FLIGHT
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!
END SUBROUTINE INI_AIRCRAFT_BALLOON
