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
USE MODE_AIRCRAFT_BALLOON,     ONLY: FLYER_RECV_AND_ALLOCATE, FLYER_SEND
USE MODE_INI_AIRCRAFT_BALLOON, ONLY: DEALLOCATE_FLYER
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
INTEGER :: JI
LOGICAL :: OMONOPROC_SAVE ! Copy of true value of GSMONOPROC

! Save GSMONOPROC value
OMONOPROC_SAVE = GSMONOPROC
! Force GSMONOPROC to true to allow IO_Field_write on only 1 process! (not very clean hack)
GSMONOPROC = .TRUE.

DO JI = 1, NBALLOONS
  ! The balloon data is only available on the process where it is physically located => transfer it if necessary

  ! Send data from owner to writer if necessary
  IF ( ISP == NRANKCUR_BALLOON(JI) .AND. NRANKCUR_BALLOON(JI) /= TPFILE%NMASTER_RANK ) THEN
    CALL FLYER_SEND( TBALLOONS(JI)%TBALLOON, TPFILE%NMASTER_RANK )
  END IF

  IF ( ISP == TPFILE%NMASTER_RANK ) THEN
    ! Receive data from owner if not available on the writer process
    IF ( NRANKCUR_BALLOON(JI) /= TPFILE%NMASTER_RANK ) THEN
      ALLOCATE( TBALLOONS(JI)%TBALLOON )
      CALL FLYER_RECV_AND_ALLOCATE( TBALLOONS(JI)%TBALLOON, NRANKCUR_BALLOON(JI) )
    END IF

    ! Write data
    IF ( TBALLOONS(JI)%TBALLOON%LFLY ) CALL WRITE_BALLOON_POSITION( TPFILE, TBALLOONS(JI)%TBALLOON )

    ! Free ballon data if it was not stored on this process
    IF ( NRANKCUR_BALLOON(JI) /= TPFILE%NMASTER_RANK ) THEN
      CALL DEALLOCATE_FLYER( TBALLOONS(JI)%TBALLOON )
      DEALLOCATE( TBALLOONS(JI)%TBALLOON )
    END IF
  END IF
END DO

! Restore correct value of GSMONOPROC
GSMONOPROC = OMONOPROC_SAVE

END SUBROUTINE WRITE_BALLOON_n
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
SUBROUTINE WRITE_BALLOON_POSITION( TPFILE, TPFLYER )
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

USE MODE_GRIDPROJ,       ONLY: SM_LATLON
USE MODE_IO_FIELD_WRITE, only: IO_Field_write
#ifdef MNH_IOCDF4
use mode_io_tools_nc4,   only: IO_Err_handle_nc4
#endif
use mode_msg

TYPE(TFILEDATA),    INTENT(IN) :: TPFILE ! File characteristics
TYPE(TBALLOONDATA), INTENT(IN) :: TPFLYER
!
!
!*       0.2   Declarations of local variables
!
#ifdef MNH_IOCDF4
integer(kind=CDFINT) :: igroupid
integer(kind=CDFINT) :: istatus
#endif
REAL                 :: ZLAT          ! latitude of the balloon
REAL                 :: ZLON          ! longitude of the balloon
type(tfiledata)      :: tzfile
TYPE(TFIELDMETADATA) :: TZFIELD

CALL SM_LATLON( XLATORI, XLONORI, TPFLYER%XX_CUR, TPFLYER%XY_CUR, ZLAT, ZLON )

#ifdef MNH_IOLFI
IF ( TPFILE%CFORMAT == 'LFI' .OR. TPFILE%CFORMAT == 'LFICDF4' ) THEN
  ! Write current balloon position for LFI files (netCDF uses an other structure)
  TZFILE = TPFILE
  TZFILE%CFORMAT = 'LFI'

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
  CALL IO_Field_write(TZFILE,TZFIELD,ZLAT)

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
  CALL IO_Field_write(TZFILE,TZFIELD,ZLON)

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
  CALL IO_Field_write(TZFILE,TZFIELD,TPFLYER%XZ_CUR)

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
  CALL IO_Field_write(TZFILE,TZFIELD,TPFLYER%XWASCENT)

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
    istatus = NF90_INQ_NCID( tzfile%nncid, Trim( tpflyer%ctitle ), igroupid )
    if ( istatus == NF90_NOERR ) then
      ! The group already exists (should not)
      call Print_msg( NVERB_WARNING, 'IO', 'WRITE_BALLOON_POSITION', 'group '// Trim( tpflyer%ctitle ) // ' already exists' )
    else
      ! Create the group
      istatus = NF90_DEF_GRP( tzfile%nncid, Trim( tpflyer%ctitle ), igroupid )
      if ( istatus /= NF90_NOERR ) &
        call IO_Err_handle_nc4( istatus, 'WRITE_BALLOON_POSITION', 'NF90_DEF_GRP', 'for ' // Trim( tpflyer%ctitle ) )

      ! Add a comment attribute
      istatus = NF90_PUT_ATT( igroupid, NF90_GLOBAL, 'comment', 'Current position of balloon '// Trim( tpflyer%ctitle ) )
      if (istatus /= NF90_NOERR ) &
        call IO_Err_handle_nc4( istatus, 'WRITE_BALLOON_POSITION', 'NF90_PUT_ATT', 'comment for '// Trim( tpflyer%ctitle ) )
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
