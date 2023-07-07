!MNH_LIC Copyright 2001-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!##########################
MODULE MODE_WRITE_BALLOON_n
!##########################
!
IMPLICIT NONE

PRIVATE

PUBLIC :: WRITE_BALLOON_n

CONTAINS

!     ###################################
      SUBROUTINE WRITE_BALLOON_n(TPFILE)
!     ###################################
!
!!****  *WRITE_BALLOON_n* - routine to write balloon records in a LFIFM file
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      FMWRIT     : FM-routine to write a record
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_AIRCRAFT_BALLOON_n : contains balloon and aircraft variables
!!      Module MODD_GRID_n : contains spatial grid variables
!!      Module MODD_LUNIT_n   : contains logical unit variables
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!  	G.Jaubert   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/06/01 
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet    06/2022: reorganize flyers
!  P. Wautelet 25/08/2022: write balloon positions in netCDF4 files inside HDF5 groups
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_AIRCRAFT_BALLOON, only: NBALLOONS, NRANKCUR_BALLOON, TBALLOONS
USE MODD_IO,               ONLY: GSMONOPROC, ISP, TFILEDATA
!
USE MODE_MODELN_HANDLER,       ONLY: GET_CURRENT_MODEL_INDEX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!
TYPE(TFILEDATA),   INTENT(IN) :: TPFILE ! File characteristics
!
!*       0.2   Declarations of local variables
!
!
INTEGER :: IMI
INTEGER :: JI
LOGICAL :: OMONOPROC_SAVE ! Copy of true value of GSMONOPROC

IMI = GET_CURRENT_MODEL_INDEX()

! Save GSMONOPROC value
OMONOPROC_SAVE = GSMONOPROC
! Force GSMONOPROC to true to allow IO_Field_write on only 1 process! (not very clean hack)
GSMONOPROC = .TRUE.

DO JI = 1, NBALLOONS
  ! The balloon data is only available on the process where it is physically located => transfer it if necessary

  ! Send data from owner to writer if necessary
  IF ( ISP == NRANKCUR_BALLOON(JI) .AND. NRANKCUR_BALLOON(JI) /= TPFILE%NMASTER_RANK ) THEN
    CALL TBALLOONS(JI)%TBALLOON%SEND( KTO = TPFILE%NMASTER_RANK, OSEND_SIZE_TO_RECEIVER = .TRUE. )
  END IF

  IF ( ISP == TPFILE%NMASTER_RANK ) THEN
    ! Receive data from owner if not available on the writer process
    IF ( NRANKCUR_BALLOON(JI) /= TPFILE%NMASTER_RANK ) THEN
      ALLOCATE( TBALLOONS(JI)%TBALLOON )
      CALL TBALLOONS(JI)%TBALLOON%RECV_ALLOCATE( KFROM = NRANKCUR_BALLOON(JI), ORECV_SIZE_FROM_OWNER = .TRUE. )
    END IF

    ! Write data (only if flyer is on the current model)
    ! It will also be written in the ancestry model files
    IF ( TBALLOONS(JI)%TBALLOON%NMODEL == IMI ) CALL WRITE_BALLOON_POSITION( TPFILE, TBALLOONS(JI)%TBALLOON )

    ! Free ballon data if it was not stored on this process
    IF ( NRANKCUR_BALLOON(JI) /= TPFILE%NMASTER_RANK ) THEN
      CALL TBALLOONS(JI)%TBALLOON%DATA_ARRAYS_DEALLOCATE()
      DEALLOCATE( TBALLOONS(JI)%TBALLOON )
    END IF
  END IF
END DO

! Restore correct value of GSMONOPROC
GSMONOPROC = OMONOPROC_SAVE

END SUBROUTINE WRITE_BALLOON_n
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
RECURSIVE SUBROUTINE WRITE_BALLOON_POSITION( TPFILE, TPFLYER )
!
#ifdef MNH_IOCDF4
use NETCDF,              only: NF90_DEF_GRP, NF90_GLOBAL, NF90_INQ_NCID, NF90_NOERR, NF90_PUT_ATT
#endif

USE MODD_AIRCRAFT_BALLOON
use modd_field,          only: tfieldmetadata, TYPEREAL
USE MODD_GRID,           ONLY: XLONORI, XLATORI
use modd_io,             only: isp, tfiledata
#ifdef MNH_IOCDF4
use modd_precision,      only: CDFINT
#endif
USE MODD_TIME_n,         ONLY: TDTCUR

USE MODE_DATETIME
USE MODE_GRIDPROJ,       ONLY: SM_LATLON
USE MODE_IO_FIELD_WRITE, only: IO_Field_write
#ifdef MNH_IOCDF4
use mode_io_tools_nc4,   only: IO_Err_handle_nc4
#endif
use mode_msg

TYPE(TFILEDATA),    INTENT(IN) :: TPFILE ! File characteristics
TYPE(TBALLOONDATA), INTENT(IN) :: TPFLYER

#ifdef MNH_IOCDF4
integer(kind=CDFINT) :: igroupid
integer(kind=CDFINT) :: istatus
#endif
REAL                 :: ZLAT          ! latitude of the balloon
REAL                 :: ZLON          ! longitude of the balloon
type(tfiledata)      :: tzfile
TYPE(TFIELDMETADATA) :: TZFIELD

! Do not write balloon position if not yet in fly or crashed
IF ( .NOT.TPFLYER%LFLY .OR. TPFLYER%LCRASH ) RETURN

! Check if current model time is the same as the time corresponding to the balloon position
IF ( ABS( TDTCUR - TPFLYER%TPOS_CUR ) > 1.e-6 ) &
  call Print_msg( NVERB_WARNING, 'IO', 'WRITE_BALLOON_POSITION', 'position time does not corresponds to current time for balloon ' &
  // Trim( tpflyer%cname ) )

! Recursive call up to grand parent file
! This way balloon position is also available on all ancestry model files (useful for restart with different number of models)
! This is done by a recursive call instead of a more standard loop on all the models to ensure that the balloon position
! corresponds to the correct instant.
IF ( ASSOCIATED( TPFILE%TDADFILE ) ) THEN
  IF ( TRIM( TPFILE%TDADFILE%CNAME ) /= TRIM( TPFILE%CNAME ) ) CALL WRITE_BALLOON_POSITION( TPFILE%TDADFILE, TPFLYER )
END IF

CALL SM_LATLON( XLATORI, XLONORI, TPFLYER%XX_CUR, TPFLYER%XY_CUR, ZLAT, ZLON )

#ifdef MNH_IOLFI
IF ( TPFILE%CFORMAT == 'LFI' .OR. TPFILE%CFORMAT == 'LFICDF4' ) THEN
  ! Write current balloon position for LFI files (netCDF uses an other structure)
  TZFILE = TPFILE
  TZFILE%CFORMAT = 'LFI'

  TZFIELD = TFIELDMETADATA(                   &
    CMNHNAME   = TRIM(TPFLYER%CNAME)//'LAT',  &
    CSTDNAME   = '',                          &
    CLONGNAME  = TRIM(TPFLYER%CNAME)//'LAT',  &
    CUNITS     = 'degree',                    &
    CDIR       = '--',                        &
    CCOMMENT   = '',                          &
    NGRID      = 0,                           &
    NTYPE      = TYPEREAL,                    &
    NDIMS      = 0,                           &
    LTIMEDEP   = .TRUE.                       )
  CALL IO_Field_write(TZFILE,TZFIELD,ZLAT)

  TZFIELD = TFIELDMETADATA(                   &
    CMNHNAME   = TRIM(TPFLYER%CNAME)//'LON',  &
    CSTDNAME   = '',                          &
    CLONGNAME  = TRIM(TPFLYER%CNAME)//'LON',  &
    CUNITS     = 'degree',                    &
    CDIR       = '--',                        &
    CCOMMENT   = '',                          &
    NGRID      = 0,                           &
    NTYPE      = TYPEREAL,                    &
    NDIMS      = 0,                           &
    LTIMEDEP   = .TRUE.                       )
  CALL IO_Field_write(TZFILE,TZFIELD,ZLON)

  TZFIELD = TFIELDMETADATA(                   &
    CMNHNAME   = TRIM(TPFLYER%CNAME)//'ALT',  &
    CSTDNAME   = '',                          &
    CLONGNAME  = TRIM(TPFLYER%CNAME)//'ALT',  &
    CUNITS     = 'm',                         &
    CDIR       = '--',                        &
    CCOMMENT   = '',                          &
    NGRID      = 0,                           &
    NTYPE      = TYPEREAL,                    &
    NDIMS      = 0,                           &
    LTIMEDEP   = .TRUE.                       )
  CALL IO_Field_write(TZFILE,TZFIELD,TPFLYER%XZ_CUR)

  TZFIELD = TFIELDMETADATA(                       &
    CMNHNAME   = TRIM(TPFLYER%CNAME)//'WASCENT',  &
    CSTDNAME   = '',                              &
    CLONGNAME  = TRIM(TPFLYER%CNAME)//'WASCENT',  &
    CUNITS     = 'm s-1',                         &
    CDIR       = '--',                            &
    CCOMMENT   = '',                              &
    NGRID      = 0,                               &
    NTYPE      = TYPEREAL,                        &
    NDIMS      = 0,                               &
    LTIMEDEP   = .TRUE.                           )
  CALL IO_Field_write(TZFILE,TZFIELD,TPFLYER%XWASCENT)

  TZFIELD = TFIELDMETADATA(                   &
    CMNHNAME   = TRIM(TPFLYER%CNAME)//'RHO',  &
    CSTDNAME   = '',                          &
    CLONGNAME  = TRIM(TPFLYER%CNAME)//'RHO',  &
    CUNITS     = 'kg m-3',                    &
    CDIR       = '--',                        &
    CCOMMENT   = '',                          &
    NGRID      = 0,                           &
    NTYPE      = TYPEREAL,                    &
    NDIMS      = 0,                           &
    LTIMEDEP   = .TRUE.                       )
  CALL IO_Field_write(TZFILE,TZFIELD,TPFLYER%XRHO)
END IF
#endif

#ifdef MNH_IOCDF4
IF ( TPFILE%CFORMAT == 'NETCDF4' .OR. TPFILE%CFORMAT == 'LFICDF4' ) THEN
  ! Write current balloon position for netCDF files
  ! Each balloon position is written inside an HDF5 group
  TZFILE = TPFILE
  TZFILE%CFORMAT = 'NETCDF4'

  if ( isp == tzfile%nmaster_rank ) then
    istatus = NF90_INQ_NCID( tzfile%nncid, Trim( tpflyer%cname ), igroupid )
    if ( istatus == NF90_NOERR ) then
      ! The group already exists (should not)
      call Print_msg( NVERB_WARNING, 'IO', 'WRITE_BALLOON_POSITION', 'group '// Trim( tpflyer%cname ) // ' already exists' )
    else
      ! Create the group
      istatus = NF90_DEF_GRP( tzfile%nncid, Trim( tpflyer%cname ), igroupid )
      if ( istatus /= NF90_NOERR ) &
        call IO_Err_handle_nc4( istatus, 'WRITE_BALLOON_POSITION', 'NF90_DEF_GRP', 'for ' // Trim( tpflyer%cname ) )

      ! Add a comment attribute
      istatus = NF90_PUT_ATT( igroupid, NF90_GLOBAL, 'comment', 'Current position of balloon '// Trim( tpflyer%cname ) )
      if (istatus /= NF90_NOERR ) &
        call IO_Err_handle_nc4( istatus, 'WRITE_BALLOON_POSITION', 'NF90_PUT_ATT', 'comment for '// Trim( tpflyer%cname ) )
    end if
  end if

  tzfile%nncid = igroupid

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
  CALL IO_Field_write(TZFILE,TZFIELD,ZLAT)

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
  CALL IO_Field_write(TZFILE,TZFIELD,ZLON)

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
  CALL IO_Field_write(TZFILE,TZFIELD,TPFLYER%XZ_CUR)

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
  CALL IO_Field_write(TZFILE,TZFIELD,TPFLYER%XWASCENT)

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
  CALL IO_Field_write(TZFILE,TZFIELD,TPFLYER%XRHO)
END IF
#endif

END SUBROUTINE WRITE_BALLOON_POSITION
!-------------------------------------------------------------------------------

END MODULE MODE_WRITE_BALLOON_n
