!MNH_LIC Copyright 2000-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 01/10/2020: bugfix: DEFAULT_FLYER: add missing default values
!  P. Wautelet    06/2022: reorganize flyers
!  P. Wautelet 25/08/2022: write balloon positions in netCDF4 files inside HDF5 groups
!-----------------------------------------------------------------

!###############################
MODULE MODE_INI_AIRCRAFT_BALLOON
!###############################

USE MODE_MSG

IMPLICIT NONE

PRIVATE

PUBLIC :: ALLOCATE_FLYER, DEALLOCATE_FLYER

PUBLIC :: INI_AIRCRAFT_BALLOON

CONTAINS

!     ############################################################
      SUBROUTINE INI_AIRCRAFT_BALLOON( TPINIFILE, PLATOR, PLONOR )
!     ############################################################
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
USE MODD_CONF,       ONLY: CPROGRAM
USE MODD_DIAG_FLAG,  ONLY: LAIRCRAFT_BALLOON, NTIME_AIRCRAFT_BALLOON, &
                           XALT_BALLOON, XLAT_BALLOON, XLON_BALLOON, XSTEP_AIRCRAFT_BALLOON
USE MODD_DYN_n,      ONLY: DYN_MODEL
USE MODD_IO,         ONLY: ISP, TFILEDATA
USE MODD_PARAMETERS, ONLY: NUNDEF
!
USE MODE_GRIDPROJ,       ONLY: SM_XYHAT
USE MODE_INI_AIRCRAFT,   ONLY: INI_AIRCRAFT
USE MODE_INI_BALLOON,    ONLY: INI_BALLOON
USE MODE_MODELN_HANDLER, ONLY: GET_CURRENT_MODEL_INDEX
!
!
IMPLICIT NONE
!
!*      0.1  declarations of arguments
!
TYPE(TFILEDATA),    INTENT(IN) :: TPINIFILE !Initial file
REAL,               INTENT(IN) :: PLATOR  ! latitude of origine point
REAL,               INTENT(IN) :: PLONOR  ! longitude of origine point
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
INTEGER :: IMI    ! current model index
INTEGER :: JI
!
!----------------------------------------------------------------------------
!
IMI=GET_CURRENT_MODEL_INDEX()
!----------------------------------------------------------------------------
!
!*      1.   Default values
!            --------------
!
IF ( CPROGRAM == 'DIAG  ') THEN
  IF ( .NOT. LAIRCRAFT_BALLOON ) RETURN
  IF (NTIME_AIRCRAFT_BALLOON == NUNDEF .OR. XSTEP_AIRCRAFT_BALLOON == XUNDEF) THEN
    CMNHMSG(1) = "NTIME_AIRCRAFT_BALLOON and/or XSTEP_AIRCRAFT_BALLOON not initialized in DIAG "
    CMNHMSG(2) = "No calculations for Balloons and Aircraft"
    CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_AIRCRAFT_BALLOON' )

    LAIRCRAFT_BALLOON=.FALSE.
    RETURN
  ENDIF
ENDIF

IF ( NAIRCRAFTS > 0 .OR. NBALLOONS > 0 ) LFLYER = .TRUE.
!
!----------------------------------------------------------------------------
!
!*      2.   Balloon initialization
!            ----------------------
IF ( IMI == 1 ) THEN
  ALLOCATE( NRANKCUR_BALLOON (NBALLOONS) ); NRANKCUR_BALLOON = NFLYER_DEFAULT_RANK
  ALLOCATE( NRANKNXT_BALLOON (NBALLOONS) ); NRANKNXT_BALLOON = NFLYER_DEFAULT_RANK

  ALLOCATE( TBALLOONS(NBALLOONS) )
END IF

! Flyers are at first only initialized on 1 process. Data will be transfered later on the right processes
IF ( ISP == NFLYER_DEFAULT_RANK ) THEN
  IF ( IMI == 1 ) CALL INI_BALLOON

  DO JI = 1, NBALLOONS
    CALL INI_LAUNCH( JI, TBALLOONS(JI)%TBALLOON )
  END DO
END IF
!
!----------------------------------------------------------------------------
!
!*      3.   Aircraft initialization
!            -----------------------
!
IF ( IMI == 1 ) THEN
  ALLOCATE( NRANKCUR_AIRCRAFT(NAIRCRAFTS) ); NRANKCUR_AIRCRAFT = NFLYER_DEFAULT_RANK
  ALLOCATE( NRANKNXT_AIRCRAFT(NAIRCRAFTS) ); NRANKNXT_AIRCRAFT = NFLYER_DEFAULT_RANK

  ALLOCATE( TAIRCRAFTS(NAIRCRAFTS) )
END IF

! Flyers are at first only initialized on 1 process. Data will be transfered later on the right processes
IF ( ISP == NFLYER_DEFAULT_RANK ) THEN
  IF ( IMI == 1 ) CALL INI_AIRCRAFT

  DO JI = 1, NAIRCRAFTS
    CALL INI_FLIGHT( JI, TAIRCRAFTS(JI)%TAIRCRAFT )
  END DO
END IF
!
!----------------------------------------------------------------------------
!
!*      4.   Allocations of storage arrays
!            -----------------------------
!
IF ( IMI == 1 .AND. ISP == NFLYER_DEFAULT_RANK ) THEN
  DO JI = 1, NBALLOONS
    CALL ALLOCATE_FLYER( TBALLOONS(JI)%TBALLOON )
  END DO

  DO JI = 1, NAIRCRAFTS
    CALL ALLOCATE_FLYER( TAIRCRAFTS(JI)%TAIRCRAFT )
  END DO
END IF
!
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!
CONTAINS
!
SUBROUTINE INI_LAUNCH(KNBR,TPFLYER)

#ifdef MNH_IOCDF4
USE NETCDF,             ONLY: NF90_INQ_NCID, NF90_NOERR
#endif

use modd_field,         only: tfieldmetadata, TYPEREAL
USE MODD_IO,            ONLY: GSMONOPROC, ISP, TFILEDATA
#ifdef MNH_IOCDF4
USE MODD_PRECISION,     ONLY: CDFINT
#endif
USE MODD_TIME_n,        ONLY: TDTCUR

use MODE_IO_FIELD_READ, only: IO_Field_read

INTEGER,             INTENT(IN)    :: KNBR
CLASS(TBALLOONDATA), INTENT(INOUT) :: TPFLYER

#ifdef MNH_IOCDF4
INTEGER(KIND=CDFINT) :: IGROUPID
INTEGER(KIND=CDFINT) :: ISTATUS
#endif
INTEGER :: IMODEL
INTEGER :: IRESP  ! return code
LOGICAL :: OMONOPROC_SAVE ! Copy of true value of GSMONOPROC
LOGICAL :: GREAD ! True if balloon position was read in synchronous file
REAL :: ZLAT ! latitude of the balloon
REAL :: ZLON ! longitude of the balloon
#ifdef MNH_IOCDF4
TYPE(TFILEDATA) :: TZFILE
#endif
TYPE(TFIELDMETADATA) :: TZFIELD

IF ( IMI /= TPFLYER%NMODEL ) RETURN

GREAD = .FALSE.

! Save GSMONOPROC value
OMONOPROC_SAVE = GSMONOPROC
! Force GSMONOPROC to true to allow IO_Field_read on only 1 process! (not very clean hack)
GSMONOPROC = .TRUE.

CALL SM_XYHAT( PLATOR, PLONOR, TPFLYER%XLATLAUNCH, TPFLYER%XLONLAUNCH, TPFLYER%XXLAUNCH, TPFLYER%XYLAUNCH )

IF ( CPROGRAM == 'MESONH' .OR. CPROGRAM == 'SPAWN ' .OR. CPROGRAM == 'REAL  ' ) THEN
  ! Read the current location in the synchronous file

  IF ( TPINIFILE%CFORMAT == 'LFI'                                                             &
       .OR. ( TPINIFILE%CFORMAT == 'NETCDF4' .AND.                                            &
              (        TPINIFILE%NMNHVERSION(1) < 5                                           &
                .OR. ( TPINIFILE%NMNHVERSION(1) == 5 .AND. TPINIFILE%NMNHVERSION(2) < 6 ) ) ) ) THEN
    ! Read in LFI file or in old format if netCDF (MesoNH < 5.6)
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

    IF ( IRESP == 0 ) THEN
      GREAD = .TRUE.

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

      TPFLYER%XP_CUR   = XUNDEF

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
    END IF
#ifdef MNH_IOCDF4
  ELSE
    ! Read in netCDF file (new structure since MesoNH 5.6)
    IF ( ISP /= TPINIFILE%NMASTER_RANK )  CALL PRINT_MSG( NVERB_ERROR, 'IO', 'INI_LAUNCH', 'process is not the file master process')

    ISTATUS = NF90_INQ_NCID( TPINIFILE%NNCID, TRIM( TPFLYER%CTITLE ), IGROUPID )

    IF ( ISTATUS == NF90_NOERR ) THEN
      GREAD = .TRUE.

      TZFILE = TPINIFILE
      TZFILE%NNCID = IGROUPID

      TZFIELD = TFIELDMETADATA(  &
        CMNHNAME   = 'LAT',      &
        CSTDNAME   = '',         &
        CLONGNAME  = 'LAT',      &
        CUNITS     = 'degree',   &
        CDIR       = '--',       &
        CCOMMENT   = 'latitude', &
        NGRID      = 0,          &
        NTYPE      = TYPEREAL,   &
        NDIMS      = 0,          &
        LTIMEDEP   = .TRUE.      )
      CALL IO_Field_read(TZFILE,TZFIELD,ZLAT)

      TZFIELD = TFIELDMETADATA(   &
        CMNHNAME   = 'LON',       &
        CSTDNAME   = '',          &
        CLONGNAME  = 'LON',       &
        CUNITS     = 'degree',    &
        CDIR       = '--',        &
        CCOMMENT   = 'longitude', &
        NGRID      = 0,           &
        NTYPE      = TYPEREAL,    &
        NDIMS      = 0,           &
        LTIMEDEP   = .TRUE.       )
      CALL IO_Field_read(TZFILE,TZFIELD,ZLON)

      TZFIELD = TFIELDMETADATA(  &
        CMNHNAME   = 'ALT',      &
        CSTDNAME   = '',         &
        CLONGNAME  = 'ALT',      &
        CUNITS     = 'm',        &
        CDIR       = '--',       &
        CCOMMENT   = 'altitude', &
        NGRID      = 0,          &
        NTYPE      = TYPEREAL,   &
        NDIMS      = 0,          &
        LTIMEDEP   = .TRUE.      )
      CALL IO_Field_read(TZFILE,TZFIELD,TPFLYER%XZ_CUR)

      TPFLYER%XP_CUR   = XUNDEF

      TZFIELD = TFIELDMETADATA(               &
        CMNHNAME   = 'WASCENT',               &
        CSTDNAME   = '',                      &
        CLONGNAME  = 'WASCENT',               &
        CUNITS     = 'm s-1',                 &
        CDIR       = '--',                    &
        CCOMMENT   = 'ascent vertical speed', &
        NGRID      = 0,                       &
        NTYPE      = TYPEREAL,                &
        NDIMS      = 0,                       &
        LTIMEDEP   = .TRUE.                   )
      CALL IO_Field_read(TZFILE,TZFIELD,TPFLYER%XWASCENT)

      TZFIELD = TFIELDMETADATA(     &
        CMNHNAME   = 'RHO',         &
        CSTDNAME   = '',            &
        CLONGNAME  = 'RHO',         &
        CUNITS     = 'kg m-3',      &
        CDIR       = '--',          &
        CCOMMENT   = 'air density', &
        NGRID      = 0,             &
        NTYPE      = TYPEREAL,      &
        NDIMS      = 0,             &
        LTIMEDEP   = .TRUE.         )
      CALL IO_Field_read(TZFILE,TZFIELD,TPFLYER%XRHO)
    END IF
#endif
  END IF

  IF ( GREAD ) THEN
    CALL SM_XYHAT( PLATOR, PLONOR, ZLAT, ZLON, TPFLYER%XX_CUR, TPFLYER%XY_CUR )

    TPFLYER%LFLY = .TRUE.
    TPFLYER%TPOS_CUR = TDTCUR

    CMNHMSG(1) = 'current location read from synchronous file for ' // TRIM( TPFLYER%CTITLE )
    IF (TPFLYER%CTYPE== 'CVBALL') THEN
      WRITE( CMNHMSG(2), * ) " Lat=", ZLAT, " Lon=", ZLON
      WRITE( CMNHMSG(3), * ) " Alt=", TPFLYER%XZ_CUR, " Wasc=", TPFLYER%XWASCENT
    ELSE IF (TPFLYER%CTYPE== 'ISODEN') THEN
      WRITE( CMNHMSG(2), * ) " Lat=", ZLAT, " Lon=", ZLON, " Rho=", TPFLYER%XRHO
    END IF
    CALL PRINT_MSG( NVERB_INFO, 'GEN', 'INI_LAUNCH' )
  ELSE
    ! The position is not found, data is not in the synchronous file
    ! Use the position given in namelist
    CALL PRINT_MSG( NVERB_INFO, 'GEN', 'INI_LAUNCH', 'initial location taken from namelist for ' // TRIM( TPFLYER%CTITLE ) )
  END IF

  ! Correct timestep if necessary
  ! This has to be done at first pass (when IMI=1) to have the correct value as soon as possible
  ! If 'MOB', set balloon store timestep to be at least the timestep of the coarser model (IMI=1) (with higher timestep)
  ! as the balloon can fly on any model
  ! If 'FIX', set balloon store timestep to be at least the timestep of its model
  ! It should also need to be a multiple of the model timestep
  IF ( IMI == 1 ) THEN
    IF ( TPFLYER%CMODEL == 'MOB' ) THEN
      IMODEL = 1
    ELSE
      IMODEL = TPFLYER%NMODEL
    END IF

    CALL FLYER_TIMESTEP_CORRECT( DYN_MODEL(IMODEL)%XTSTEP, TPFLYER )
  END IF
  !
ELSE IF ( CPROGRAM == 'DIAG  ' ) THEN
  IF ( LAIRCRAFT_BALLOON ) THEN
    ! read the current location in MODD_DIAG_FLAG
    !
    ZLAT=XLAT_BALLOON(KNBR)
    ZLON=XLON_BALLOON(KNBR)
    TPFLYER%XZ_CUR=XALT_BALLOON(KNBR)
    IF (TPFLYER%XZ_CUR /= XUNDEF .AND. ZLAT /= XUNDEF .AND. ZLON /= XUNDEF ) THEN
      CALL SM_XYHAT( PLATOR, PLONOR, ZLAT, ZLON, TPFLYER%XX_CUR, TPFLYER%XY_CUR )
      TPFLYER%LFLY = .TRUE.
      CMNHMSG(1) = 'current location read from MODD_DIAG_FLAG for ' // TRIM( TPFLYER%CTITLE )
      WRITE( CMNHMSG(2), * ) " Lat=", ZLAT, " Lon=", ZLON," Alt=",TPFLYER%XZ_CUR
      CALL PRINT_MSG( NVERB_INFO, 'GEN', 'INI_LAUNCH' )
    END IF
    !
    CALL FLYER_TIMESTEP_CORRECT( XSTEP_AIRCRAFT_BALLOON, TPFLYER )
  END IF
END IF

! Restore correct value of GSMONOPROC
GSMONOPROC = OMONOPROC_SAVE

END SUBROUTINE INI_LAUNCH
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE INI_FLIGHT(KNBR,TPFLYER)

INTEGER,              INTENT(IN)    :: KNBR
CLASS(TAIRCRAFTDATA), INTENT(INOUT) :: TPFLYER

INTEGER :: IMODEL
INTEGER :: JSEG   ! loop counter

IF ( IMI /= TPFLYER%NMODEL ) RETURN

! Correct timestep if necessary
! This has to be done at first pass (when IMI=1) to have the correct value as soon as possible
! If 'MOB', set balloon store timestep to be at least the timestep of the coarser model (IMI=1) (with higher timestep)
! as the balloon can fly on any model
! If 'FIX', set balloon store timestep to be at least the timestep of its model
! It should also need to be a multiple of the model timestep
IF ( IMI == 1 ) THEN
  IF ( TPFLYER%CMODEL == 'MOB' ) THEN
    IMODEL = 1
  ELSE
    IMODEL = TPFLYER%NMODEL
  END IF

  CALL FLYER_TIMESTEP_CORRECT( DYN_MODEL(IMODEL)%XTSTEP, TPFLYER )
END IF

ALLOCATE(TPFLYER%XPOSX(TPFLYER%NPOS))
ALLOCATE(TPFLYER%XPOSY(TPFLYER%NPOS))

DO JSEG = 1, TPFLYER%NPOS
  CALL SM_XYHAT( PLATOR, PLONOR, TPFLYER%XPOSLAT(JSEG), TPFLYER%XPOSLON(JSEG), TPFLYER%XPOSX(JSEG), TPFLYER%XPOSY(JSEG) )
END DO

END SUBROUTINE INI_FLIGHT
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE FLYER_TIMESTEP_CORRECT( PTSTEP_MODEL, TPFLYER )
! Timestep is set to a multiple of the PTSTEP_MODEL value
REAL,              INTENT(IN)    :: PTSTEP_MODEL
CLASS(TFLYERDATA), INTENT(INOUT) :: TPFLYER

REAL :: ZTSTEP_OLD

ZTSTEP_OLD = TPFLYER%TFLYER_TIME%XTSTEP

TPFLYER%TFLYER_TIME%XTSTEP = MAX ( PTSTEP_MODEL, TPFLYER%TFLYER_TIME%XTSTEP )
TPFLYER%TFLYER_TIME%XTSTEP = NINT( TPFLYER%TFLYER_TIME%XTSTEP / PTSTEP_MODEL ) * PTSTEP_MODEL

IF ( ABS( TPFLYER%TFLYER_TIME%XTSTEP - ZTSTEP_OLD ) > 1E-6 ) THEN
  WRITE( CMNHMSG(1), '( "Timestep for flyer ", A, " is set to ", EN12.3, " (instead of ", EN12.3, ")" )' ) &
         TPFLYER%CTITLE, TPFLYER%TFLYER_TIME%XTSTEP, ZTSTEP_OLD
  CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_LAUNCH' )
END IF

END SUBROUTINE FLYER_TIMESTEP_CORRECT
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!
END SUBROUTINE INI_AIRCRAFT_BALLOON

!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE ALLOCATE_FLYER( TPFLYER, KSTORE )

USE MODD_AIRCRAFT_BALLOON, ONLY: TFLYERDATA
USE MODD_CONF,             ONLY: CPROGRAM
USE MODD_CONF_n,           ONLY: NRR
USE MODD_DIAG_FLAG,        ONLY: NTIME_AIRCRAFT_BALLOON
USE MODD_DIM_n,            ONLY: NKMAX
USE MODD_DYN,              ONLY: XSEGLEN
USE MODD_DYN_n,            ONLY: DYN_MODEL
USE MODD_NSV,              ONLY: NSV
USE MODD_PARAMETERS,       ONLY: JPVEXT, NNEGUNDEF, XUNDEF
USE MODD_PARAM_n,          ONLY: CCLOUD, CTURB
USE MODD_SURF_PAR,         ONLY: XUNDEF_SFX => XUNDEF

IMPLICIT NONE

CLASS(TFLYERDATA), INTENT(INOUT) :: TPFLYER
INTEGER, OPTIONAL, INTENT(IN)    :: KSTORE

INTEGER :: IKU    ! number of vertical levels
INTEGER :: ISTORE ! number of storage instants

CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'ALLOCATE_FLYER', 'flyer: ' // TRIM(TPFLYER%CTITLE), OLOCAL = .TRUE. )

IKU = NKMAX + 2 * JPVEXT

IF ( PRESENT( KSTORE ) ) THEN
  ISTORE = KSTORE
ELSE
  IF ( CPROGRAM == 'DIAG  ' ) THEN
    ISTORE = INT ( NTIME_AIRCRAFT_BALLOON / TPFLYER%TFLYER_TIME%XTSTEP ) + 1
  ELSE
    ISTORE = NINT ( ( XSEGLEN - DYN_MODEL(1)%XTSTEP ) / TPFLYER%TFLYER_TIME%XTSTEP ) + 1
  ENDIF
END IF
!
ALLOCATE( TPFLYER%TFLYER_TIME%TPDATES(ISTORE) )
ALLOCATE( TPFLYER%NMODELHIST(ISTORE) )
ALLOCATE( TPFLYER%XX   (ISTORE) )
ALLOCATE( TPFLYER%XY   (ISTORE) )
ALLOCATE( TPFLYER%XZ   (ISTORE) )
ALLOCATE( TPFLYER%XLON (ISTORE) )
ALLOCATE( TPFLYER%XLAT (ISTORE) )
ALLOCATE( TPFLYER%XZON (ISTORE) )
ALLOCATE( TPFLYER%XMER (ISTORE) )
ALLOCATE( TPFLYER%XW   (ISTORE) )
ALLOCATE( TPFLYER%XP   (ISTORE) )
ALLOCATE( TPFLYER%XTH  (ISTORE) )
ALLOCATE( TPFLYER%XR   (ISTORE, NRR) )
ALLOCATE( TPFLYER%XSV  (ISTORE, NSV) )
ALLOCATE( TPFLYER%XRTZ (ISTORE, IKU) )
ALLOCATE( TPFLYER%XRZ  (ISTORE, IKU, NRR) )
ALLOCATE( TPFLYER%XFFZ (ISTORE, IKU) )
ALLOCATE( TPFLYER%XIWCZ(ISTORE, IKU) )
ALLOCATE( TPFLYER%XLWCZ(ISTORE, IKU) )
ALLOCATE( TPFLYER%XCIZ (ISTORE, IKU) )
IF ( CCLOUD == 'LIMA' ) THEN
  ALLOCATE( TPFLYER%XCCZ(ISTORE, IKU) )
  ALLOCATE( TPFLYER%XCRZ(ISTORE, IKU) )
ELSE
  ALLOCATE( TPFLYER%XCCZ(0, 0) )
  ALLOCATE( TPFLYER%XCRZ(0, 0) )
ENDIF
ALLOCATE( TPFLYER%XCRARE    (ISTORE, IKU) )
ALLOCATE( TPFLYER%XCRARE_ATT(ISTORE, IKU) )
ALLOCATE( TPFLYER%XWZ       (ISTORE, IKU) )
ALLOCATE( TPFLYER%XZZ       (ISTORE, IKU) )
IF ( CTURB == 'TKEL' ) THEN
  ALLOCATE( TPFLYER%XTKE(ISTORE) )
ELSE
  ALLOCATE( TPFLYER%XTKE(0) )
END IF
ALLOCATE( TPFLYER%XTKE_DISS(ISTORE) )
ALLOCATE( TPFLYER%XTSRAD   (ISTORE) )
ALLOCATE( TPFLYER%XZS      (ISTORE) )

ALLOCATE( TPFLYER%XTHW_FLUX(ISTORE) )
ALLOCATE( TPFLYER%XRCW_FLUX(ISTORE) )
ALLOCATE( TPFLYER%XSVW_FLUX(ISTORE, NSV) )

TPFLYER%NMODELHIST = NNEGUNDEF
TPFLYER%XX    = XUNDEF
TPFLYER%XY    = XUNDEF
TPFLYER%XZ    = XUNDEF
TPFLYER%XLON  = XUNDEF
TPFLYER%XLAT  = XUNDEF
TPFLYER%XZON  = XUNDEF
TPFLYER%XMER  = XUNDEF
TPFLYER%XW    = XUNDEF
TPFLYER%XP    = XUNDEF
TPFLYER%XTH   = XUNDEF
TPFLYER%XR    = XUNDEF
TPFLYER%XSV   = XUNDEF
TPFLYER%XRTZ  = XUNDEF
TPFLYER%XRZ   = XUNDEF
TPFLYER%XFFZ  = XUNDEF
TPFLYER%XIWCZ = XUNDEF
TPFLYER%XLWCZ = XUNDEF
TPFLYER%XCIZ  = XUNDEF
TPFLYER%XCCZ  = XUNDEF
TPFLYER%XCRZ  = XUNDEF
TPFLYER%XCRARE     = XUNDEF
TPFLYER%XCRARE_ATT = XUNDEF
TPFLYER%XWZ        = XUNDEF
TPFLYER%XZZ        = XUNDEF
TPFLYER%XTKE       = XUNDEF
TPFLYER%XTKE_DISS  = XUNDEF
TPFLYER%XTSRAD     = XUNDEF_SFX
TPFLYER%XZS        = XUNDEF

TPFLYER%XTHW_FLUX = XUNDEF
TPFLYER%XRCW_FLUX = XUNDEF
TPFLYER%XSVW_FLUX = XUNDEF

END SUBROUTINE ALLOCATE_FLYER
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE DEALLOCATE_FLYER( TPFLYER )

USE MODD_AIRCRAFT_BALLOON, ONLY: TAIRCRAFTDATA, TFLYERDATA

IMPLICIT NONE

CLASS(TFLYERDATA), INTENT(INOUT) :: TPFLYER

CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'DEALLOCATE_FLYER', 'flyer: ' // TRIM(TPFLYER%CTITLE), OLOCAL = .TRUE. )

DEALLOCATE( TPFLYER%TFLYER_TIME%TPDATES )
DEALLOCATE( TPFLYER%NMODELHIST )
DEALLOCATE( TPFLYER%XX         )
DEALLOCATE( TPFLYER%XY         )
DEALLOCATE( TPFLYER%XZ         )
DEALLOCATE( TPFLYER%XLON       )
DEALLOCATE( TPFLYER%XLAT       )
DEALLOCATE( TPFLYER%XZON       )
DEALLOCATE( TPFLYER%XMER       )
DEALLOCATE( TPFLYER%XW         )
DEALLOCATE( TPFLYER%XP         )
DEALLOCATE( TPFLYER%XTH        )
DEALLOCATE( TPFLYER%XR         )
DEALLOCATE( TPFLYER%XSV        )
DEALLOCATE( TPFLYER%XRTZ       )
DEALLOCATE( TPFLYER%XRZ        )
DEALLOCATE( TPFLYER%XFFZ       )
DEALLOCATE( TPFLYER%XIWCZ      )
DEALLOCATE( TPFLYER%XLWCZ      )
DEALLOCATE( TPFLYER%XCIZ       )
DEALLOCATE( TPFLYER%XCCZ       )
DEALLOCATE( TPFLYER%XCRZ       )
DEALLOCATE( TPFLYER%XCRARE     )
DEALLOCATE( TPFLYER%XCRARE_ATT )
DEALLOCATE( TPFLYER%XWZ        )
DEALLOCATE( TPFLYER%XZZ        )
DEALLOCATE( TPFLYER%XTKE       )
DEALLOCATE( TPFLYER%XTKE_DISS  )
DEALLOCATE( TPFLYER%XTSRAD     )
DEALLOCATE( TPFLYER%XZS        )

DEALLOCATE( TPFLYER%XTHW_FLUX )
DEALLOCATE( TPFLYER%XRCW_FLUX )
DEALLOCATE( TPFLYER%XSVW_FLUX )

SELECT TYPE( TPFLYER )
  CLASS IS ( TAIRCRAFTDATA )
    DEALLOCATE( TPFLYER%XPOSLAT  )
    DEALLOCATE( TPFLYER%XPOSLON  )
    DEALLOCATE( TPFLYER%XPOSX    )
    DEALLOCATE( TPFLYER%XPOSY    )
    IF ( TPFLYER%LALTDEF ) THEN
      DEALLOCATE( TPFLYER%XPOSP  )
    ELSE
      DEALLOCATE( TPFLYER%XPOSZ  )
    END IF
    DEALLOCATE( TPFLYER%XPOSTIME )
END SELECT

END SUBROUTINE DEALLOCATE_FLYER
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------

END MODULE MODE_INI_AIRCRAFT_BALLOON
