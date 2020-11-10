!MNH_LIC Copyright 1994-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet may 2016  : use NetCDF Fortran module
!  J. Escobar  14/12/2017: correction for MNH_INT=8
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/12/2018: split of mode_netcdf into multiple modules/files
!  P. Wautelet 10/01/2019: replace handle_err by IO_Err_handle_nc4 for better netCDF error messages
!  P. Wautelet 11/01/2019: NVERB_INFO->NVERB_WARNING for zero size fields
!  P. Wautelet 01/02/2019: IO_Coordvar_write_nc4: bug: use of non-associated pointers (PIOCDF%DIM_Nx_y)
!  P. Wautelet 05/03/2019: rename IO subroutines and modules
!  P. Wautelet 12/07/2019: add support for 1D array of dates
!  P. Wautelet 10/09/2019: IO_Coordvar_write_nc4: split communication and file write operations
!                          + no more process coordination for Z-split files
!  P. Wautelet 18/09/2019: correct support of 64bit integers (MNH_INT=8)
!  P. Wautelet 19/09/2019: temporary workaround for netCDF bug if MNH_INT=8 (if netCDF fortran < 4.4.5)
!  P. Wautelet 11/02/2020: add 'dims' attribute in IO_Write_field_header_split_nc4
!  P. Wautelet 25/06/2020: remove workaround for netCDF bug (see 19/09/2019)
!  P. Wautelet 14/09/2020: IO_Coordvar_write_nc4: do not store 'time' coordinate in diachronic files
!  P. Wautelet 22/09/2020: add ldimreduced field to allow reduction in the number of dimensions of fields (used by 2D simulations)
!  P. Wautelet 10/11/2020: new data structures for netCDF dimensions
!-----------------------------------------------------------------
#ifdef MNH_IOCDF4
module mode_io_write_nc4

use modd_field,        only: tfielddata
use modd_io,           only: gsmonoproc, tfiledata
use modd_precision,    only: CDFINT, MNHINT_NF90, MNHREAL_MPI, MNHREAL_NF90

use mode_io_tools_nc4, only: IO_Mnhname_clean, IO_Vdims_fill_nc4, IO_Dim_find_create_nc4, IO_Strdimid_get_nc4, IO_Err_handle_nc4
use mode_msg

use NETCDF,            only: NF90_CHAR, NF90_FLOAT, NF90_INT1,                                    &
                             NF90_GLOBAL, NF90_NOERR,                                             &
                             NF90_DEF_VAR, NF90_DEF_VAR_DEFLATE, NF90_GET_ATT, NF90_INQ_VARID,    &
                             NF90_INQUIRE_ATTRIBUTE, NF90_PUT_ATT, NF90_PUT_VAR

implicit none

private

public :: IO_Coordvar_write_nc4, IO_Field_write_nc4, IO_Header_write_nc4
public :: IO_Field_header_split_write_nc4

INTERFACE IO_Field_write_nc4
   MODULE PROCEDURE IO_Field_write_nc4_X0,IO_Field_write_nc4_X1, &
                    IO_Field_write_nc4_X2,IO_Field_write_nc4_X3, &
                    IO_Field_write_nc4_X4,IO_Field_write_nc4_X5, &
                    IO_Field_write_nc4_X6,                       &
                    IO_Field_write_nc4_N0,IO_Field_write_nc4_N1, &
                    IO_Field_write_nc4_N2,IO_Field_write_nc4_N3, &
                    IO_Field_write_nc4_L0,IO_Field_write_nc4_L1, &
                    IO_Field_write_nc4_C0,IO_Field_write_nc4_C1, &
                    IO_Field_write_nc4_T0,IO_Field_write_nc4_T1
END INTERFACE IO_Field_write_nc4

integer,parameter :: NSTRINGCHUNKSIZE = 16 !Dimension of the chunks of strings
                                           !(to limit the number of dimensions for strings)

integer(kind=CDFINT),parameter :: SHUFFLE = 1 !Set to 1 for (usually) better compression
integer(kind=CDFINT),parameter :: DEFLATE = 1

contains

subroutine IO_Field_header_split_write_nc4( tpfile, tpfield, knblocks )
use modd_field,      only: TYPEREAL
use modd_parameters, only: jphext

use mode_tools_ll,   only: Get_globaldims_ll

type(tfiledata),       intent(in) :: tpfile
type(tfielddata),      intent(in) :: tpfield
integer,               intent(in) :: knblocks

character(len=len(tpfield%cmnhname))  :: yvarname
integer                               :: iimax, ijmax
integer(kind=CDFINT)              :: istatus
integer(kind=CDFINT)              :: incid
integer(kind=CDFINT)              :: ivarid
integer(kind=CDFINT),dimension(3) :: ishape

call Print_msg( NVERB_DEBUG, 'IO', 'IO_Field_header_split_write_nc4', 'called for field '//trim( tpfield%cmnhname ) )

if ( tpfield%ntype /= TYPEREAL ) then
  call Print_msg( NVERB_ERROR, 'IO', 'IO_Field_header_split_write_nc4', 'invalid ntype for field '//trim( tpfield%cmnhname ) )
  return
end if

! Get the Netcdf file ID
incid = tpfile%nncid

call IO_Mnhname_clean( tpfield%cmnhname, yvarname )

istatus = NF90_INQ_VARID( incid, yvarname, ivarid )
if ( istatus /= NF90_NOERR ) then

  istatus = NF90_DEF_VAR( incid, yvarname, MNHREAL_NF90, ivarid)

  if ( tpfield%ndims /= 3 ) call Print_msg( NVERB_FATAL, 'IO', 'IO_Field_header_split_write_nc4', &
                  trim( tpfile%cname )//': '//trim( yvarname )//': NDIMS should be 3' )

  if ( tpfield%cdir /= 'XY' ) call Print_msg( NVERB_FATAL, 'IO', 'IO_Field_header_split_write_nc4', &
                  trim( tpfile%cname )//': '//trim( yvarname )//': CDIR should be XY' )

  call Get_globaldims_ll( iimax, ijmax )
  ishape(1) = int( iimax + 2 * jphext, kind = CDFINT )
  ishape(2) = int( ijmax + 2 * jphext, kind = CDFINT )
  ishape(3) = knblocks
  call IO_Field_attr_write_nc4( tpfile, tpfield, ivarid, .false., kshape = ishape )

  if ( istatus /= NF90_NOERR ) call IO_Err_handle_nc4( istatus, 'IO_Field_header_split_write_nc4', 'NF90_DEF_VAR', trim(yvarname) )

  istatus = NF90_PUT_ATT( incid, ivarid,'split_variable', 'yes')
  if ( istatus /= NF90_NOERR ) call IO_Err_handle_nc4( istatus, 'IO_Field_header_split_write_nc4', 'NF90_PUT_ATT', &
                                                     'split_variable for '//trim( tpfield%cmnhname ) )

  istatus = NF90_PUT_ATT( incid, ivarid,'split_mode', 'Z')
  if ( istatus /= NF90_NOERR ) call IO_Err_handle_nc4( istatus, 'IO_Field_header_split_write_nc4', 'NF90_PUT_ATT', &
                                                     'split_mode for '//trim( tpfield%cmnhname ) )

  istatus = NF90_PUT_ATT( incid, ivarid,'split_nblocks', knblocks )
  if ( istatus /= NF90_NOERR ) call IO_Err_handle_nc4( istatus, 'IO_Field_header_split_write_nc4', 'NF90_PUT_ATT', &
                                                     'split_nblocks for '//trim( tpfield%cmnhname ) )

  istatus = NF90_PUT_ATT( incid, ivarid,'split_nfiles', tpfile%nsubfiles_ioz )
  if ( istatus /= NF90_NOERR ) call IO_Err_handle_nc4( istatus, 'IO_Field_header_split_write_nc4', 'NF90_PUT_ATT', &
                                                     'split_nfiles for '//trim( tpfield%cmnhname ) )

  istatus = NF90_PUT_ATT( incid, ivarid,'split_distribution', 'round-robin' )
  if ( istatus /= NF90_NOERR ) call IO_Err_handle_nc4( istatus, 'IO_Field_header_split_write_nc4', 'NF90_PUT_ATT', &
                                                     'split_distribution for '//trim( tpfield%cmnhname ) )

  istatus = NF90_PUT_ATT( incid, ivarid,'ndims', tpfield%ndims )
  if ( istatus /= NF90_NOERR ) call IO_Err_handle_nc4( istatus, 'IO_Field_header_split_write_nc4', 'NF90_PUT_ATT', &
                                                     'ndims for '//trim( tpfield%cmnhname ) )

  istatus = NF90_PUT_ATT( incid, ivarid,'dims', ishape )
  if ( istatus /= NF90_NOERR ) call IO_Err_handle_nc4( istatus, 'IO_Field_header_split_write_nc4', 'NF90_PUT_ATT', &
                                                     'dims for '//trim( tpfield%cmnhname ) )

  if ( tpfield%ltimedep ) then
    istatus = NF90_PUT_ATT( incid, ivarid,'time_dependent', 'yes' )
  else
    istatus = NF90_PUT_ATT( incid, ivarid,'time_dependent', 'no' )
  end if
  if ( istatus /= NF90_NOERR ) call IO_Err_handle_nc4( istatus, 'IO_Field_header_split_write_nc4', 'NF90_PUT_ATT', &
                                                     'time_dependent for '//trim( tpfield%cmnhname ) )
else
  call Print_msg( NVERB_WARNING, 'IO', 'IO_Field_header_split_write_nc4', &
                  trim( tpfile%cname )//': '//trim( yvarname )//' already defined' )
end if

end subroutine IO_Field_header_split_write_nc4

SUBROUTINE IO_Field_attr_write_nc4(TPFILE,TPFIELD,KVARID,OEXISTED,KSHAPE,HCALENDAR,OISCOORD)
!
USE MODD_CONF,   ONLY: CPROGRAM, LCARTESIAN
USE MODD_CONF_n, ONLY: CSTORAGE_TYPE
use modd_field,  only: NMNHDIM_ARAKAWA, TYPEINT, TYPEREAL
!
TYPE(TFILEDATA),                              INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),                             INTENT(IN) :: TPFIELD
INTEGER(KIND=CDFINT),                         INTENT(IN) :: KVARID
LOGICAL,                                      INTENT(IN) :: OEXISTED !True if variable was already defined
INTEGER(KIND=CDFINT), DIMENSION(:), OPTIONAL, INTENT(IN) :: KSHAPE
CHARACTER(LEN=*),                   OPTIONAL, INTENT(IN) :: HCALENDAR
LOGICAL,                            OPTIONAL, INTENT(IN) :: OISCOORD   ! Is a coordinate variable (->do not write coordinates attribute)
!
INTEGER(KIND=CDFINT)         :: INCID
INTEGER(KIND=CDFINT)         :: STATUS
CHARACTER(LEN=:),ALLOCATABLE :: YCOORDS
LOGICAL                      :: GISCOORD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_write_nc4','called for field '//TRIM(TPFIELD%CMNHNAME))
!
IF(LEN_TRIM(TPFIELD%CSTDNAME)==0 .AND. LEN_TRIM(TPFIELD%CLONGNAME)==0) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_attr_write_nc4','at least long_name or standard_name must be provided &
  &to respect CF-convention for variable '//TRIM(TPFIELD%CMNHNAME))
ENDIF
!
IF (TPFIELD%NDIMS>1 .AND. .NOT.PRESENT(KSHAPE)) &
  CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Field_attr_write_nc4','KSHAPE not provided for '//TRIM(TPFIELD%CMNHNAME))
!
IF (PRESENT(OISCOORD)) THEN
  GISCOORD = OISCOORD
ELSE
  GISCOORD = .FALSE.
END IF
!
INCID = TPFILE%NNCID
!
! Standard_name attribute definition (CF convention)
IF(LEN_TRIM(TPFIELD%CSTDNAME)==0) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_write_nc4','TPFIELD%CSTDNAME not set for variable '//TRIM(TPFIELD%CMNHNAME))
ELSE
  STATUS = NF90_PUT_ATT(INCID, KVARID,'standard_name', TRIM(TPFIELD%CSTDNAME))
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_attr_write_nc4','NF90_PUT_ATT','standard_name for ' &
                                                   //trim(TPFIELD%CMNHNAME))
ENDIF
!
! Long_name attribute definition (CF convention)
IF(LEN_TRIM(TPFIELD%CLONGNAME)==0) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_write_nc4','TPFIELD%CLONGNAME not set for variable '//TRIM(TPFIELD%CMNHNAME))
ELSE
  STATUS = NF90_PUT_ATT(INCID, KVARID,'long_name', TRIM(TPFIELD%CLONGNAME))
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_attr_write_nc4','NF90_PUT_ATT','long_name for ' &
                                                   //trim(TPFIELD%CMNHNAME))
ENDIF
!
! Canonical units attribute definition (CF convention)
IF(LEN_TRIM(TPFIELD%CUNITS)==0) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_write_nc4','TPFIELD%CUNITS not set for variable '//TRIM(TPFIELD%CMNHNAME))
ELSE
  STATUS = NF90_PUT_ATT(INCID, KVARID,'units', TRIM(TPFIELD%CUNITS))
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_attr_write_nc4','NF90_PUT_ATT','units for ' &
                                                   //trim(TPFIELD%CMNHNAME))
ENDIF
!
! GRID attribute definition
IF(TPFIELD%NGRID<0) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_write_nc4','TPFIELD%NGRID not set for variable '//TRIM(TPFIELD%CMNHNAME))
!Do not write GRID attribute if NGRID=0
ELSE IF (TPFIELD%NGRID>0) THEN
  STATUS = NF90_PUT_ATT(INCID, KVARID, 'grid', TPFIELD%NGRID)
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_attr_write_nc4','NF90_PUT_ATT','grid for ' &
                                                   //trim(TPFIELD%CMNHNAME))
ENDIF
!
! COMMENT attribute definition
IF(LEN_TRIM(TPFIELD%CCOMMENT)==0) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_write_nc4','TPFIELD%CCOMMENT not set for variable '//TRIM(TPFIELD%CMNHNAME))
ELSE
  STATUS = NF90_PUT_ATT(INCID, KVARID,'comment', TRIM(TPFIELD%CCOMMENT))
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_attr_write_nc4','NF90_PUT_ATT','comment for ' &
                                                   //trim(TPFIELD%CMNHNAME))
ENDIF
!
! Calendar (CF convention)
IF(PRESENT(HCALENDAR)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_write_nc4','CALENDAR provided for variable '//TRIM(TPFIELD%CMNHNAME))
  STATUS = NF90_PUT_ATT(INCID, KVARID,'calendar', TRIM(HCALENDAR))
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_attr_write_nc4','NF90_PUT_ATT','calendar for ' &
                                                   //trim(TPFIELD%CMNHNAME))
ENDIF
!
! Coordinates (CF convention)
IF (.NOT.GISCOORD) THEN
  !0D: nothing to do
  !1D: no direct correspondance with latitude(_x)/longitude(_x) 2D variables => nothing to do
  IF (.NOT.LCARTESIAN .AND. TPFIELD%NDIMS>1 .AND. TPFIELD%NGRID/=0) THEN
    IF (TPFIELD%CDIR=='XY') THEN
      if (       kshape(1) == tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(tpfield%ngrid,1) )%nlen &
           .and. kshape(2) == tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(tpfield%ngrid,2) )%nlen ) then
        SELECT CASE(TPFIELD%NGRID)
          CASE (0) !Not on Arakawa grid
            !Nothing to do
          CASE (1) !Mass point
            YCOORDS='latitude longitude'
          CASE (2) !u point
            YCOORDS='latitude_u longitude_u'
          CASE (3) !v point
            YCOORDS='latitude_v longitude_v'
          CASE (4) !w point
            YCOORDS='latitude longitude'
          CASE (5) !xi vorticity point (=f point =uv point)
            YCOORDS='latitude_f longitude_f'
          CASE (6) !eta vorticity point (=uw point)
            YCOORDS='latitude_u longitude_u'
          CASE (7) !zeta vorticity point (=vw point)
            YCOORDS='latitude_v longitude_v'
          CASE (8) !fw point (=uvw point)
            YCOORDS='latitude_f longitude_f'
          CASE DEFAULT
            CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_attr_write_nc4','invalid NGRID for variable '//TRIM(TPFIELD%CMNHNAME))
        END SELECT
        !
        STATUS = NF90_PUT_ATT(INCID, KVARID,'coordinates',YCOORDS)
        IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_attr_write_nc4','NF90_PUT_ATT','coordinates')
        DEALLOCATE(YCOORDS)
      ELSE
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_attr_write_nc4','coordinates not implemented for variable ' &
                                                                    //TRIM(TPFIELD%CMNHNAME))
      END IF
    ELSE
      !No YCOORDS for CDIR/='XY'
    END IF
  END IF
ENDIF
!
IF(TPFIELD%NTYPE==TYPEINT .AND. TPFIELD%NDIMS>0) THEN
  IF (TPFIELD%NFILLVALUE>=TPFIELD%NVALIDMIN .AND. TPFIELD%NFILLVALUE<=TPFIELD%NVALIDMAX) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_attr_write_nc4','_FillValue is not outside of valid_min - valid_max'// &
                                                                'interval for variable '//TRIM(TPFIELD%CMNHNAME))
  !
  ! Fillvalue (CF/COMODO convention)
  ! Remarks: * the attribute '_FillValue' is also recognized by the netCDF library
  !            and is used when pre-filling a variable
  !          * it cannot be modified if some data has already been written (->check OEXISTED)
  !BUG: NF90_PUT_ATT does not work for NF90_INT64 and _FillValue attribute if netCDF-fortran version < 4.4.5 (bug in netCDF)
  !     (see https://github.com/Unidata/netcdf-fortran/issues/62)
  IF(.NOT.OEXISTED) THEN
    STATUS = NF90_PUT_ATT(INCID, KVARID,'_FillValue', TPFIELD%NFILLVALUE)
    IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_attr_write_nc4','NF90_PUT_ATT','_FillValue')
  END IF
  !
  ! Valid_min/max (CF/COMODO convention)
  STATUS = NF90_PUT_ATT(INCID, KVARID,'valid_min', TPFIELD%NVALIDMIN)
    IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_attr_write_nc4','NF90_PUT_ATT','valid_min')
  !
  STATUS = NF90_PUT_ATT(INCID, KVARID,'valid_max',TPFIELD%NVALIDMAX)
    IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_attr_write_nc4','NF90_PUT_ATT','valid_max')
ENDIF
!
IF(TPFIELD%NTYPE==TYPEREAL .AND. TPFIELD%NDIMS>0) THEN
  IF (TPFIELD%XFILLVALUE>=TPFIELD%XVALIDMIN .AND. TPFIELD%XFILLVALUE<=TPFIELD%XVALIDMAX) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_attr_write_nc4','_FillValue is not outside of valid_min - valid_max'// &
                                                                'interval for variable '//TRIM(TPFIELD%CMNHNAME))
  !
  ! Fillvalue (CF/COMODO convention)
  ! Remarks: * the attribute '_FillValue' is also recognized by the netCDF library
  !            and is used when pre-filling a variable
  !          * it cannot be modified if some data has already been written (->check OEXISTED)
  IF(.NOT.OEXISTED) THEN
    IF (TPFILE%LNCREDUCE_FLOAT_PRECISION) THEN
      STATUS = NF90_PUT_ATT(INCID, KVARID,'_FillValue', REAL(TPFIELD%XFILLVALUE,KIND=4))
    ELSE
      STATUS = NF90_PUT_ATT(INCID, KVARID,'_FillValue', TPFIELD%XFILLVALUE)
    END IF
    IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_attr_write_nc4','NF90_PUT_ATT','_FillValue')
  END IF
  !
  ! Valid_min/max (CF/COMODO convention)
  IF (TPFILE%LNCREDUCE_FLOAT_PRECISION) THEN
    STATUS = NF90_PUT_ATT(INCID, KVARID,'valid_min', REAL(TPFIELD%XVALIDMIN,KIND=4))
  ELSE
    STATUS = NF90_PUT_ATT(INCID, KVARID,'valid_min', TPFIELD%XVALIDMIN)
  END IF
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_attr_write_nc4','NF90_PUT_ATT','valid_min')
  !
  IF (TPFILE%LNCREDUCE_FLOAT_PRECISION) THEN
    STATUS = NF90_PUT_ATT(INCID, KVARID,'valid_max', REAL(TPFIELD%XVALIDMAX,KIND=4))
  ELSE
    STATUS = NF90_PUT_ATT(INCID, KVARID,'valid_max',TPFIELD%XVALIDMAX)
  END IF
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_attr_write_nc4','NF90_PUT_ATT','valid_max')
ENDIF
!
END SUBROUTINE IO_Field_attr_write_nc4


SUBROUTINE IO_Field_write_nc4_X0(TPFILE,TPFIELD,PFIELD,KRESP)
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
REAL,                  INTENT(IN) :: PFIELD
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER(KIND=CDFINT)                            :: STATUS
INTEGER(KIND=CDFINT)                            :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))            :: YVARNAME
INTEGER(KIND=CDFINT)                            :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                                         :: IRESP
LOGICAL                                         :: GEXISTED !True if variable was already defined
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_X0',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)
!
! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (TPFIELD%LTIMEDEP) THEN
     ! Get the netcdf dimensions
     CALL IO_Vdims_fill_nc4(TPFILE, TPFIELD, INT(SHAPE(PFIELD),KIND=CDFINT), IVDIMS)
     ! Define the variable
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHREAL_NF90, IVDIMS, IVARID)
     IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X0','NF90_DEF_VAR',trim(YVARNAME))
     DEALLOCATE(IVDIMS)
   ELSE
     ! Define the scalar variable
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHREAL_NF90, IVARID)
     IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X0','NF90_DEF_VAR',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_X0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, PFIELD)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X0','NF90_PUT_VAR',trim(YVARNAME),IRESP)

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_X0


SUBROUTINE IO_Field_write_nc4_X1(TPFILE,TPFIELD,PFIELD,KRESP)
!
TYPE(TFILEDATA),TARGET,INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
REAL,DIMENSION(:),     INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER(KIND=CDFINT)                            :: STATUS
INTEGER(KIND=CDFINT)                            :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)+4)          :: YVARNAME
INTEGER(KIND=CDFINT)                            :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                                         :: IRESP
LOGICAL                                         :: GEXISTED !True if variable was already defined
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_X1',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(PFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_X1','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL IO_Vdims_fill_nc4(TPFILE, TPFIELD, INT(SHAPE(PFIELD),KIND=CDFINT), IVDIMS)

   ! Define the variable
   IF (TPFILE%LNCREDUCE_FLOAT_PRECISION) THEN
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
   ELSE
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHREAL_NF90, IVDIMS, IVARID)
   END IF
     IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X1','NF90_DEF_VAR',trim(YVARNAME))
   ! Add compression if asked for
   IF (TPFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TPFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X1','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_X1',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, PFIELD)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X1','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_X1


SUBROUTINE IO_Field_write_nc4_X2(TPFILE,TPFIELD,PFIELD,KRESP,KVERTLEVEL,KZFILE,OISCOORD)
!
TYPE(TFILEDATA),TARGET,INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
REAL,DIMENSION(:,:),   INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,               INTENT(OUT):: KRESP
INTEGER,OPTIONAL,      INTENT(IN) :: KVERTLEVEL ! Number of the vertical level (needed for Z-level split files)
INTEGER,OPTIONAL,      INTENT(IN) :: KZFILE     ! Number of the Z-level split file
LOGICAL,OPTIONAL,      INTENT(IN) :: OISCOORD   ! Is a coordinate variable (->do not write coordinates attribute)
!
INTEGER(KIND=CDFINT)                            :: STATUS
INTEGER(KIND=CDFINT)                            :: INCID
CHARACTER(LEN=4)                                :: YSUFFIX
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)+4)          :: YVARNAME
INTEGER(KIND=CDFINT)                            :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                                         :: IRESP
TYPE(TFIELDDATA)                                :: TZFIELD
TYPE(TFILEDATA),POINTER                         :: TZFILE
LOGICAL                                         :: GEXISTED !True if variable was already defined
!
IRESP = 0
!
IF (PRESENT(KVERTLEVEL)) THEN
  WRITE(YSUFFIX,'(I4.4)') KVERTLEVEL
  IF (.NOT.PRESENT(KZFILE)) THEN
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_nc4_X2','KZFILE argument not provided')
    RETURN
  END IF
  IF (KZFILE>TPFILE%NSUBFILES_IOZ) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Field_write_nc4_X2','KZFILE value too high')
  TZFILE => TPFILE%TFILES_IOZ(KZFILE)%TFILE
  TZFIELD = TPFIELD
  TZFIELD%CMNHNAME  = TRIM(TZFIELD%CMNHNAME)//YSUFFIX
  IF (LEN_TRIM(TZFIELD%CSTDNAME)>0)  TZFIELD%CSTDNAME  = TRIM(TZFIELD%CSTDNAME)//'_at_level_'//YSUFFIX
  IF (LEN_TRIM(TZFIELD%CLONGNAME)>0) TZFIELD%CLONGNAME = TRIM(TZFIELD%CLONGNAME)//' at level '//YSUFFIX
  TZFIELD%NDIMS = 2
ELSE
  TZFILE => TPFILE
  TZFIELD = TPFIELD
ENDIF
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_X2',TRIM(TZFILE%CNAME)//': writing '//TRIM(TZFIELD%CMNHNAME))
!
! Get the Netcdf file ID
INCID = TZFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TZFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(PFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_X2','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL IO_Vdims_fill_nc4(TZFILE, TZFIELD, INT(SHAPE(PFIELD),KIND=CDFINT), IVDIMS)

   ! Define the variable
   IF (TZFILE%LNCREDUCE_FLOAT_PRECISION) THEN
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
   ELSE
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHREAL_NF90, IVDIMS, IVARID)
   END IF
     IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X2','NF90_DEF_VAR',trim(YVARNAME))
   ! Add compression if asked for
   IF (TZFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TZFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X2','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_X2',TRIM(TZFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_Field_attr_write_nc4(TZFILE,TZFIELD,IVARID,GEXISTED,KSHAPE=INT(SHAPE(PFIELD),KIND=CDFINT),OISCOORD=OISCOORD)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, PFIELD)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X2','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_X2


SUBROUTINE IO_Field_write_nc4_X3(TPFILE,TPFIELD,PFIELD,KRESP)
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
REAL,DIMENSION(:,:,:), INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER(KIND=CDFINT)                            :: STATUS
INTEGER(KIND=CDFINT)                            :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))            :: YVARNAME
INTEGER(KIND=CDFINT)                            :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                                         :: IRESP
LOGICAL                                         :: GEXISTED !True if variable was already defined
!
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_X3',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(PFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_X3','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL IO_Vdims_fill_nc4(TPFILE, TPFIELD, INT(SHAPE(PFIELD),KIND=CDFINT), IVDIMS)

   ! Define the variable
   IF (TPFILE%LNCREDUCE_FLOAT_PRECISION) THEN
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
   ELSE
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHREAL_NF90, IVDIMS, IVARID)
   END IF
   IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X3','NF90_DEF_VAR',trim(YVARNAME))

   ! Add compression if asked for
   IF (TPFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TPFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X3','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_X3',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TPFIELD,IVARID,GEXISTED,KSHAPE=INT(SHAPE(PFIELD),KIND=CDFINT))
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, PFIELD)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X3','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_X3


SUBROUTINE IO_Field_write_nc4_X4(TPFILE,TPFIELD,PFIELD,KRESP)
!
TYPE(TFILEDATA),           INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),          INTENT(IN) :: TPFIELD
REAL,DIMENSION(:,:,:,:),   INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,                   INTENT(OUT):: KRESP
!
INTEGER(KIND=CDFINT)                            :: STATUS
INTEGER(KIND=CDFINT)                            :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))            :: YVARNAME
INTEGER(KIND=CDFINT)                            :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                                         :: IRESP
LOGICAL                                         :: GEXISTED !True if variable was already defined
!
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_X4',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(PFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_X4','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL IO_Vdims_fill_nc4(TPFILE, TPFIELD, INT(SHAPE(PFIELD),KIND=CDFINT), IVDIMS)

   ! Define the variable
   IF (TPFILE%LNCREDUCE_FLOAT_PRECISION) THEN
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
   ELSE
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHREAL_NF90, IVDIMS, IVARID)
   END IF
   IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X4','NF90_DEF_VAR',trim(YVARNAME))

   ! Add compression if asked for
   IF (TPFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TPFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X4','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_X4',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TPFIELD,IVARID,GEXISTED,KSHAPE=INT(SHAPE(PFIELD),KIND=CDFINT))
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, PFIELD)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X4','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_X4


SUBROUTINE IO_Field_write_nc4_X5(TPFILE,TPFIELD,PFIELD,KRESP)
!
TYPE(TFILEDATA),           INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),          INTENT(IN) :: TPFIELD
REAL,DIMENSION(:,:,:,:,:), INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,                   INTENT(OUT):: KRESP
!
INTEGER(KIND=CDFINT)                            :: STATUS
INTEGER(KIND=CDFINT)                            :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))            :: YVARNAME
INTEGER(KIND=CDFINT)                            :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                                         :: IRESP
LOGICAL                                         :: GEXISTED !True if variable was already defined
!
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_X5',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(PFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_X5','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL IO_Vdims_fill_nc4(TPFILE, TPFIELD, INT(SHAPE(PFIELD),KIND=CDFINT), IVDIMS)

   ! Define the variable
   IF (TPFILE%LNCREDUCE_FLOAT_PRECISION) THEN
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
   ELSE
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHREAL_NF90, IVDIMS, IVARID)
   END IF
   IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X5','NF90_DEF_VAR',trim(YVARNAME))

   ! Add compression if asked for
   IF (TPFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TPFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X5','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_X5',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TPFIELD,IVARID,GEXISTED,KSHAPE=INT(SHAPE(PFIELD),KIND=CDFINT))
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, PFIELD)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X5','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_X5


SUBROUTINE IO_Field_write_nc4_X6(TPFILE,TPFIELD,PFIELD,KRESP)
!
TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),            INTENT(IN) :: TPFIELD
REAL,DIMENSION(:,:,:,:,:,:), INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,                     INTENT(OUT):: KRESP
!
INTEGER(KIND=CDFINT)                            :: STATUS
INTEGER(KIND=CDFINT)                            :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))            :: YVARNAME
INTEGER(KIND=CDFINT)                            :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                                         :: IRESP
LOGICAL                                         :: GEXISTED !True if variable was already defined
!
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_X6',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(PFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_X6','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL IO_Vdims_fill_nc4(TPFILE, TPFIELD, INT(SHAPE(PFIELD),KIND=CDFINT), IVDIMS)

   ! Define the variable
   IF (TPFILE%LNCREDUCE_FLOAT_PRECISION) THEN
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
   ELSE
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHREAL_NF90, IVDIMS, IVARID)
   END IF
   IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X6','NF90_DEF_VAR',trim(YVARNAME))

     ! Add compression if asked for
   IF (TPFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TPFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X6','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_X6',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TPFIELD,IVARID,GEXISTED,KSHAPE=INT(SHAPE(PFIELD),KIND=CDFINT))
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, PFIELD)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_X6','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_X6


SUBROUTINE IO_Field_write_nc4_N0(TPFILE,TPFIELD,KFIELD,KRESP)
!
#if 0
use modd_field,          only: NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_LEVEL
USE MODD_IO,             ONLY: LPACK,L1D,L2D
USE MODD_PARAMETERS_ll,  ONLY: JPHEXT, JPVEXT
#else
use modd_field,          only: NMNHDIM_LEVEL
USE MODD_PARAMETERS_ll,  ONLY: JPVEXT
#endif
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
INTEGER,               INTENT(IN) :: KFIELD
INTEGER,               INTENT(OUT):: KRESP
!
integer                                         :: iidx
INTEGER(KIND=CDFINT)                            :: STATUS
INTEGER(KIND=CDFINT)                            :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))            :: YVARNAME
INTEGER(KIND=CDFINT)                            :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                                         :: IRESP
LOGICAL                                         :: GEXISTED !True if variable was already defined
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_N0',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)
!
! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (TPFIELD%LTIMEDEP) THEN
     ! Get the netcdf dimensions
     CALL IO_Vdims_fill_nc4(TPFILE, TPFIELD, INT(SHAPE(KFIELD),KIND=CDFINT), IVDIMS)
     ! Define the variable
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHINT_NF90, IVDIMS, IVARID)
     IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_N0','NF90_DEF_VAR',trim(YVARNAME))
     DEALLOCATE(IVDIMS)
   ELSE
     ! Define the scalar variable
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHINT_NF90, IVARID)
     IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_N0','NF90_DEF_VAR',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_N0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, KFIELD)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_N0','NF90_PUT_VAR',trim(YVARNAME),IRESP)

!
! Use IMAX, JMAX, KMAX to define DIM_NI, DIM_NJ, DIM_LEVEL
! /!\ Can only work if IMAX, JMAX or KMAX are written before any array
!
#if 0
if ( yvarname == 'IMAX' .and. tpfile%tncdims%tdims(NMNHDIM_NI)%nid == -1 ) then
  call IO_Dim_find_create_nc4( tpfile, kfield + 2 * jphext, iidx, 'X' )
end if
if ( yvarname == 'JMAX' .and. tpfile%tncdims%tdims(NMNHDIM_NJ)%nid == -1 ) then
  if ( lpack .and. l2d ) then
    call IO_Dim_find_create_nc4( tpfile, 1,                   iidx, 'Y' )
  else
    call IO_Dim_find_create_nc4( tpfile, kfield + 2 * jphext, iidx, 'Z' )
  end if
end if
#endif
if ( yvarname == 'KMAX' .and. tpfile%tncdims%tdims(NMNHDIM_LEVEL)%nid == -1 ) then
  call IO_Dim_find_create_nc4( tpfile, kfield + 2 * JPVEXT, iidx, 'Z' )
end if

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_N0


SUBROUTINE IO_Field_write_nc4_N1(TPFILE,TPFIELD,KFIELD,KRESP)
!
#if 0
USE MODD_IO,             ONLY: LPACK,L1D,L2D
USE MODD_PARAMETERS_ll,  ONLY: JPHEXT, JPVEXT
#else
USE MODD_PARAMETERS_ll,  ONLY: JPVEXT
#endif
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
INTEGER, DIMENSION(:), INTENT(IN) :: KFIELD
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER(KIND=CDFINT)                            :: STATUS
INTEGER(KIND=CDFINT)                            :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))            :: YVARNAME
INTEGER(KIND=CDFINT)                            :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                                         :: IRESP
LOGICAL                                         :: GEXISTED !True if variable was already defined
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_N1',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(KFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_N1','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL IO_Vdims_fill_nc4(TPFILE, TPFIELD, INT(SHAPE(KFIELD),KIND=CDFINT), IVDIMS)

   ! Define the variable
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHINT_NF90, IVDIMS, IVARID)
   IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_N1','NF90_DEF_VAR',trim(YVARNAME))
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_N1',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, KFIELD)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_N1','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_N1


SUBROUTINE IO_Field_write_nc4_N2(TPFILE,TPFIELD,KFIELD,KRESP)
!
TYPE(TFILEDATA),TARGET,INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
INTEGER,DIMENSION(:,:),INTENT(IN) :: KFIELD   ! array containing the data field
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER(KIND=CDFINT)                            :: STATUS
INTEGER(KIND=CDFINT)                            :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))            :: YVARNAME
INTEGER(KIND=CDFINT)                            :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                                         :: IRESP
LOGICAL                                         :: GEXISTED !True if variable was already defined
!
IRESP = 0
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_N2',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(KFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_N2','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL IO_Vdims_fill_nc4(TPFILE, TPFIELD, INT(SHAPE(KFIELD),KIND=CDFINT), IVDIMS)

   ! Define the variable
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHINT_NF90, IVDIMS, IVARID)
   IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_N2','NF90_DEF_VAR',trim(YVARNAME))
   ! Add compression if asked for
   IF (TPFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TPFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_N2','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_N2',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TPFIELD,IVARID,GEXISTED,KSHAPE=INT(SHAPE(KFIELD),KIND=CDFINT))
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, KFIELD)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_N2','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_N2

SUBROUTINE IO_Field_write_nc4_N3(TPFILE,TPFIELD,KFIELD,KRESP)
!
TYPE(TFILEDATA),TARGET,  INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),        INTENT(IN) :: TPFIELD
INTEGER,DIMENSION(:,:,:),INTENT(IN) :: KFIELD   ! array containing the data field
INTEGER,                 INTENT(OUT):: KRESP
!
INTEGER(KIND=CDFINT)                            :: STATUS
INTEGER(KIND=CDFINT)                            :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))            :: YVARNAME
INTEGER(KIND=CDFINT)                            :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                                         :: IRESP
LOGICAL                                         :: GEXISTED !True if variable was already defined
!
IRESP = 0
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_N3',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(KFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_N3','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL IO_Vdims_fill_nc4(TPFILE, TPFIELD, INT(SHAPE(KFIELD),KIND=CDFINT), IVDIMS)

   ! Define the variable
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHINT_NF90, IVDIMS, IVARID)
   IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_N3','NF90_DEF_VAR',trim(YVARNAME))
   ! Add compression if asked for
   IF (TPFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TPFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_N3','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_N3',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TPFIELD,IVARID,GEXISTED,KSHAPE=INT(SHAPE(KFIELD),KIND=CDFINT))
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, KFIELD)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_N3','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_N3

SUBROUTINE IO_Field_write_nc4_L0(TPFILE,TPFIELD,OFIELD,KRESP)
!
USE MODD_PARAMETERS_ll,  ONLY: JPVEXT
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
LOGICAL,               INTENT(IN) :: OFIELD
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER                                         :: IFIELD
INTEGER(KIND=CDFINT)                            :: STATUS
INTEGER(KIND=CDFINT)                            :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))            :: YVARNAME
INTEGER(KIND=CDFINT)                            :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                                         :: IRESP
LOGICAL                                         :: GEXISTED !True if variable was already defined
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_L0',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)
!
! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (TPFIELD%LTIMEDEP) THEN
     ! Get the netcdf dimensions
     CALL IO_Vdims_fill_nc4(TPFILE, TPFIELD, INT(SHAPE(OFIELD),KIND=CDFINT), IVDIMS)
     ! Define the variable
     ! Use of NF90_INT1 datatype (=NF90_BYTE) that is enough to store a boolean
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT1, IVDIMS, IVARID)
     IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_L0','NF90_DEF_VAR',trim(YVARNAME))
     DEALLOCATE(IVDIMS)
   ELSE
     ! Define the scalar variable
     ! Use of NF90_INT1 datatype (=NF90_BYTE) that is enough to store a boolean
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT1, IVARID)
     IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_L0','NF90_DEF_VAR',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_L0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

!Convert LOGICAL to INTEGER (LOGICAL format not supported by netCDF files)
IF (OFIELD) THEN
  IFIELD = 1
ELSE
  IFIELD = 0
END IF

! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, IFIELD)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_L0','NF90_PUT_VAR',trim(YVARNAME),IRESP)

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_L0

SUBROUTINE IO_Field_write_nc4_L1(TPFILE,TPFIELD,OFIELD,KRESP)
!
USE MODD_PARAMETERS_ll,  ONLY: JPVEXT
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
LOGICAL, DIMENSION(:), INTENT(IN) :: OFIELD
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER, DIMENSION(SIZE(OFIELD))                :: IFIELD
INTEGER(KIND=CDFINT)                            :: STATUS
INTEGER(KIND=CDFINT)                            :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))            :: YVARNAME
INTEGER(KIND=CDFINT)                            :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                                         :: IRESP
LOGICAL                                         :: GEXISTED !True if variable was already defined
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_L1',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(OFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_L1','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL IO_Vdims_fill_nc4(TPFILE, TPFIELD, INT(SHAPE(OFIELD),KIND=CDFINT), IVDIMS)

   ! Define the variable
   ! Use of NF90_INT1 datatype (=NF90_BYTE) that is enough to store a boolean
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT1, IVDIMS, IVARID)
     IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_L1','NF90_DEF_VAR',trim(YVARNAME))
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_L1',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

!Convert LOGICAL to INTEGER (LOGICAL format not supported by netCDF files)
WHERE (OFIELD)
  IFIELD = 1
ELSEWHERE
  IFIELD = 0
END WHERE

! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, IFIELD)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_L1','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_L1


SUBROUTINE IO_Field_write_nc4_C0(TPFILE,TPFIELD,HFIELD,KRESP)
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
CHARACTER(LEN=*),      INTENT(IN) :: HFIELD
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER(KIND=CDFINT)                 :: STATUS
INTEGER(KIND=CDFINT)                 :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)) :: YVARNAME
INTEGER(KIND=CDFINT)                 :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(1)   :: IVDIMS
INTEGER                              :: IRESP, ILEN
CHARACTER(LEN=:),ALLOCATABLE         :: YFIELD
LOGICAL                              :: GEXISTED !True if variable was already defined
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_C0',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
!Store the character string in a string of a size multiple of NSTRINGCHUNKSIZE
!This is done to limit the number of dimensions in the netCDF file
ILEN = ((LEN_TRIM(HFIELD)+NSTRINGCHUNKSIZE-1)/NSTRINGCHUNKSIZE)*NSTRINGCHUNKSIZE
!If the string is empty, create it anyway with a non-zero size (to prevent problems later)
IF (ILEN==0) ILEN = NSTRINGCHUNKSIZE

! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)
!
IF (TPFIELD%LTIMEDEP) &
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_C0',TRIM(TPFILE%CNAME)// &
                 ': time dependent variable not (yet) possible for 0D variable '//TRIM(TPFIELD%CMNHNAME))
!
! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   ! Get the netcdf string dimensions id
   IVDIMS(1) = IO_Strdimid_get_nc4(TPFILE,INT(ILEN,KIND=CDFINT))
   ! Define the variable
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_CHAR, IVDIMS, IVARID)
     IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_C0','NF90_DEF_VAR',trim(YVARNAME))
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_C0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

ALLOCATE(CHARACTER(LEN=ILEN)::YFIELD)
YFIELD(1:LEN_TRIM(HFIELD))=TRIM(HFIELD)
YFIELD(LEN_TRIM(HFIELD)+1:)=' '
! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, YFIELD)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_C0','NF90_PUT_VAR',trim(YVARNAME),IRESP)
DEALLOCATE(YFIELD)

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_C0

SUBROUTINE IO_Field_write_nc4_C1(TPFILE,TPFIELD,HFIELD,KRESP)
!  Modif
!    J.Escobar : 25/04/2018 : missing 'IF ALLOCATED(IVDIMSTMP)' DEALLOCATE
!----------------------------------------------------------------
TYPE(TFILEDATA),              INTENT(IN)  :: TPFILE
TYPE(TFIELDDATA),             INTENT(IN)  :: TPFIELD
CHARACTER(LEN=*),DIMENSION(:),INTENT(IN)  :: HFIELD
INTEGER,                      INTENT(OUT) :: KRESP
!
INTEGER(KIND=CDFINT),PARAMETER :: IONE = 1
!
INTEGER(KIND=CDFINT)                            :: STATUS
INTEGER(KIND=CDFINT)                            :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))            :: YVARNAME
INTEGER(KIND=CDFINT)                            :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(2)              :: IVDIMS
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMSTMP
INTEGER(KIND=CDFINT)                            :: ILEN, ISIZE
INTEGER                                         :: IRESP
LOGICAL                                         :: GEXISTED !True if variable was already defined
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_C1',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0

ILEN  = LEN(HFIELD)
ISIZE = SIZE(HFIELD)

! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)
!
IF (TPFIELD%LTIMEDEP) THEN
  !This is an error (+return) and not a warning because IVDIMSTMP could be of size 2 if LTIMEDEP=T
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_nc4_C1',TRIM(TPFILE%CNAME)// &
                 ': time dependent variable not (yet) possible for '//TRIM(TPFIELD%CMNHNAME))
  RETURN
END IF
!
! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   ! Get the netcdf string dimensions id
   IVDIMS(1) = IO_Strdimid_get_nc4(TPFILE,ILEN)
   CALL IO_Vdims_fill_nc4(TPFILE, TPFIELD, (/ISIZE/), IVDIMSTMP)
   IVDIMS(2) = IVDIMSTMP(1)
   ! Define the variable
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_CHAR, IVDIMS, IVARID)
     IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_C1','NF90_DEF_VAR',trim(YVARNAME))
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_C1',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, HFIELD(1:ISIZE)(1:ILEN), START=(/IONE,IONE/), COUNT=(/ILEN,ISIZE/))
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_C1','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMSTMP)) DEALLOCATE(IVDIMSTMP)

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_C1


SUBROUTINE IO_Field_write_nc4_T0(TPFILE,TPFIELD,TPDATA,KRESP)
!
USE MODD_TIME_n,     ONLY: TDTMOD
USE MODD_TYPE_DATE
!
USE MODE_DATETIME
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
TYPE (DATE_TIME),      INTENT(IN) :: TPDATA
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER(KIND=CDFINT)                 :: STATUS
INTEGER(KIND=CDFINT)                 :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)) :: YVARNAME
INTEGER(KIND=CDFINT)                 :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                              :: IRESP
TYPE(TFIELDDATA)                     :: TZFIELD
CHARACTER(LEN=40)                    :: YUNITS
LOGICAL                              :: GEXISTED !True if variable was already defined
REAL                                 :: ZDELTATIME !Distance in seconds since reference date and time
TYPE(DATE_TIME)                      :: TZREF
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_T0',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
!
TZFIELD = TPFIELD
!
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)
!
TZFIELD%CMNHNAME = TRIM(YVARNAME)
!
! Model beginning date (TDTMOD%TDATE) is used as the reference date
! Reference time is set to 0.
IF (.NOT.ASSOCIATED(TDTMOD)) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_T0',TRIM(TPFILE%CNAME)// &
                 ': '//TRIM(TZFIELD%CMNHNAME)//': DTMOD is not associated and not known. Reference date set to 2000/01/01')
  TZREF%TDATE%YEAR  = 2000
  TZREF%TDATE%MONTH = 1
  TZREF%TDATE%DAY   = 1
  TZREF%TIME        = 0.
ELSE
  TZREF = TDTMOD
  TZREF%TIME = 0.
END IF
WRITE(YUNITS,'( "seconds since ",I4.4,"-",I2.2,"-",I2.2," 00:00:00 +0:00" )') &
      TZREF%TDATE%YEAR, TZREF%TDATE%MONTH, TZREF%TDATE%DAY
TZFIELD%CUNITS = TRIM(YUNITS)
!
IF (TPFIELD%LTIMEDEP) &
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_T0',TRIM(TPFILE%CNAME)// &
                 ': time dependent variable not (yet) possible for 0D variable '//TRIM(TPFIELD%CMNHNAME))
!
! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   ! Define the scalar variable
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHREAL_NF90, IVARID)
   IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_T0','NF90_DEF_VAR',trim(YVARNAME))
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_T0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TZFIELD,IVARID,GEXISTED,HCALENDAR='standard')
!
! Compute the temporal distance from reference
CALL DATETIME_DISTANCE(TZREF,TPDATA,ZDELTATIME)

! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, ZDELTATIME)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_T0','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF (IRESP/=0) THEN
  KRESP = IRESP
  RETURN
END IF

#if 0
!This part is to keep backward compatibility with MesoNH files
!but date/time is not conform to CF convention
!
! Write date
!
TZFIELD%CMNHNAME  = TRIM(YVARNAME)//'__TDATE'
TZFIELD%CLONGNAME = TRIM(TPFIELD%CLONGNAME)//'%TDATE'
TZFIELD%CUNITS    = ''
TZFIELD%CCOMMENT  = 'YYYYMMDD'

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, TZFIELD%CMNHNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   ! Get the netcdf dimensions
   CALL IO_Vdims_fill_nc4(TPFILE, TPFIELD, INT(SHAPE(ITDATE),KIND=CDFINT), IVDIMS)

   ! Define the variable
   STATUS = NF90_DEF_VAR(INCID, TZFIELD%CMNHNAME, NF90_INT, IVDIMS, IVARID)
     IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_T0','NF90_DEF_VAR',trim(TZFIELD%CMNHNAME))
   CALL IO_Field_attr_write_nc4(TPFILE,TZFIELD,IVARID,GEXISTED)
ELSE
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_T0',TRIM(TPFILE%CNAME)//': '//TRIM(TZFIELD%CMNHNAME)//' already defined')
END IF

! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, ITDATE)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_T0','NF90_PUT_VAR',trim(TZFIELD%CMNHNAME),IRESP)

IF (IRESP/=0) THEN
  KRESP = IRESP
  RETURN
END IF
!
! Write time
!
TZFIELD%CMNHNAME  = TRIM(YVARNAME)//'__TIME'
TZFIELD%CLONGNAME = TRIM(TPFIELD%CLONGNAME)//'%TIME'
TZFIELD%CUNITS    = 's'
TZFIELD%CCOMMENT  = 'SECONDS'

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, TZFIELD%CMNHNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   ! Define the scalar variable
   STATUS = NF90_DEF_VAR(INCID, TZFIELD%CMNHNAME, MNHREAL_NF90, IVARID)
   IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_T0','NF90_DEF_VAR',trim(TZFIELD%CMNHNAME))
   CALL IO_Field_attr_write_nc4(TPFILE,TZFIELD,IVARID,GEXISTED)
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_T0',TRIM(TPFILE%CNAME)//': '//TRIM(TZFIELD%CMNHNAME)//' already defined')
END IF

! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, TPDATA%TIME)
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_T0','NF90_PUT_VAR',trim(TZFIELD%CMNHNAME),IRESP)
#endif

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_T0

SUBROUTINE IO_Field_write_nc4_T1(TPFILE,TPFIELD,TPDATA,KRESP)
!
USE MODD_TIME_n,     ONLY: TDTMOD
USE MODD_TYPE_DATE
!
USE MODE_DATETIME
!
TYPE(TFILEDATA),                INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),               INTENT(IN) :: TPFIELD
TYPE (DATE_TIME), DIMENSION(:), INTENT(IN) :: TPDATA
INTEGER,                        INTENT(OUT):: KRESP
!
INTEGER                              :: JI
INTEGER(KIND=CDFINT)                 :: STATUS
INTEGER(KIND=CDFINT)                 :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)) :: YVARNAME
INTEGER(KIND=CDFINT)                 :: IVARID
INTEGER(KIND=CDFINT), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                              :: IRESP
TYPE(TFIELDDATA)                     :: TZFIELD
CHARACTER(LEN=40)                    :: YUNITS
LOGICAL                              :: GEXISTED !True if variable was already defined
REAL, DIMENSION(:), ALLOCATABLE      :: ZDELTATIME !Distance in seconds since reference date and time
TYPE(DATE_TIME)                      :: TZREF
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_nc4_T1',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
!
TZFIELD = TPFIELD
!
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)
!
TZFIELD%CMNHNAME = TRIM(YVARNAME)
!
! Model beginning date (TDTMOD%TDATE) is used as the reference date
! Reference time is set to 0.
IF (.NOT.ASSOCIATED(TDTMOD)) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_T1',TRIM(TPFILE%CNAME)// &
                 ': '//TRIM(TZFIELD%CMNHNAME)//': DTMOD is not associated and not known. Reference date set to 2000/01/01')
  TZREF%TDATE%YEAR  = 2000
  TZREF%TDATE%MONTH = 1
  TZREF%TDATE%DAY   = 1
  TZREF%TIME        = 0.
ELSE
  TZREF = TDTMOD
  TZREF%TIME = 0.
END IF
WRITE(YUNITS,'( "seconds since ",I4.4,"-",I2.2,"-",I2.2," 00:00:00 +0:00" )') &
      TZREF%TDATE%YEAR, TZREF%TDATE%MONTH, TZREF%TDATE%DAY
TZFIELD%CUNITS = TRIM(YUNITS)
!
IF (TPFIELD%LTIMEDEP) &
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_T1',TRIM(TPFILE%CNAME)// &
                 ': time dependent variable not (yet) possible for 1D variable '//TRIM(TPFIELD%CMNHNAME))
!
! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(TPDATA)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_T1','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL IO_Vdims_fill_nc4(TPFILE, TPFIELD, INT(SHAPE(TPDATA),KIND=CDFINT), IVDIMS)

   ! Define the variable
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHREAL_NF90, IVDIMS, IVARID)
   IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_T1','NF90_DEF_VAR',trim(YVARNAME))
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_nc4_N1',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_Field_attr_write_nc4(TPFILE,TZFIELD,IVARID,GEXISTED,HCALENDAR='standard')
!
! Compute the temporal distances from reference
ALLOCATE( ZDELTATIME( SIZE( TPDATA ) ) )

DO JI = 1, SIZE( TPDATA )
  CALL DATETIME_DISTANCE( TZREF, TPDATA(JI ), ZDELTATIME(JI) )
END DO

! Write the data
STATUS = NF90_PUT_VAR( INCID, IVARID, ZDELTATIME(:) )
IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Field_write_nc4_T1','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF( ALLOCATED( IVDIMS ) ) DEALLOCATE( IVDIMS )
DEALLOCATE( ZDELTATIME )

IF (IRESP/=0) THEN
  KRESP = IRESP
  RETURN
END IF

KRESP = IRESP
END SUBROUTINE IO_Field_write_nc4_T1

SUBROUTINE IO_Coordvar_write_nc4(TPFILE,HPROGRAM_ORIG)
USE MODD_CONF,       ONLY: CPROGRAM, LCARTESIAN
USE MODD_CONF_n,     ONLY: CSTORAGE_TYPE
use modd_field,      only: NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_NI_U, NMNHDIM_NJ_U, NMNHDIM_NI_V, NMNHDIM_NJ_V, &
                           NMNHDIM_LEVEL, NMNHDIM_LEVEL_W, NMNHDIM_TIME,                                   &
                           tfieldlist
USE MODD_GRID,       ONLY: XLATORI, XLONORI
USE MODD_GRID_n,     ONLY: LSLEVE, XXHAT, XYHAT, XZHAT
use modd_netcdf,     only: tdimnc
USE MODD_PARAMETERS, ONLY: JPHEXT, JPVEXT

use mode_field,      only: Find_field_id_from_mnhname
USE MODE_GRIDPROJ
USE MODE_NEST_ll,    ONLY: GET_MODEL_NUMBER_ll, GO_TOMODEL_ll

TYPE(TFILEDATA),          INTENT(IN) :: TPFILE
CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: HPROGRAM_ORIG !To emulate a file coming from this program

CHARACTER(LEN=:),ALLOCATABLE    :: YSTDNAMEPREFIX
CHARACTER(LEN=:),ALLOCATABLE    :: YPROGRAM
INTEGER                         :: IIU, IJU, IKU
INTEGER                         :: ID, IID, IRESP
INTEGER                         :: IMI
INTEGER(KIND=CDFINT)            :: INCID
LOGICAL                         :: GCHANGEMODEL
logical                         :: gdealloc
LOGICAL,POINTER                 :: GSLEVE
REAL,DIMENSION(:),POINTER       :: ZXHAT, ZYHAT, ZZHAT
REAL,DIMENSION(:),ALLOCATABLE   :: ZXHATM, ZYHATM,ZZHATM !Coordinates at mass points in the transformed space
REAL,DIMENSION(:,:),POINTER     :: ZLAT, ZLON
type(tdimnc), pointer           :: tzdim_ni, tzdim_nj, tzdim_ni_u, tzdim_nj_u, tzdim_ni_v, tzdim_nj_v

!These variables are save: they are populated once for the master Z-split file and freed after the last file has been written
real, dimension(:),   pointer, save :: zxhat_glob  => null(), zyhat_glob  => null()
real, dimension(:),   pointer, save :: zxhatm_glob => null(), zyhatm_glob => null()
real, dimension(:,:), pointer, save :: zlatm_glob  => null(), zlonm_glob  => null()
real, dimension(:,:), pointer, save :: zlatu_glob  => null(), zlonu_glob  => null()
real, dimension(:,:), pointer, save :: zlatv_glob  => null(), zlonv_glob  => null()
real, dimension(:,:), pointer, save :: zlatf_glob  => null(), zlonf_glob  => null()

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Coordvar_write_nc4','called for '//TRIM(TPFILE%CNAME))

ZXHAT => NULL()
ZYHAT => NULL()
ZZHAT => NULL()

GCHANGEMODEL = .FALSE.

IF (PRESENT(HPROGRAM_ORIG)) THEN
  YPROGRAM = HPROGRAM_ORIG
ELSE
  YPROGRAM = CPROGRAM
ENDIF

! Get the Netcdf file ID
INCID = TPFILE%NNCID

IF (TPFILE%NMODEL>0) THEN
  CALL FIND_FIELD_ID_FROM_MNHNAME('XHAT',IID,IRESP)
  ZXHAT => TFIELDLIST(IID)%TFIELD_X1D(TPFILE%NMODEL)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('YHAT',IID,IRESP)
  ZYHAT => TFIELDLIST(IID)%TFIELD_X1D(TPFILE%NMODEL)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('ZHAT',IID,IRESP)
  ZZHAT => TFIELDLIST(IID)%TFIELD_X1D(TPFILE%NMODEL)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('SLEVE',IID,IRESP)
  GSLEVE => TFIELDLIST(IID)%TFIELD_L0D(TPFILE%NMODEL)%DATA
  !
  CALL GET_MODEL_NUMBER_ll(IMI)
  IF (IMI/=TPFILE%NMODEL) THEN
    !This is necessary to have correct domain sizes (used by GATHER_XXFIELD)
    CALL GO_TOMODEL_ll(TPFILE%NMODEL,IRESP)
    GCHANGEMODEL = .TRUE.
  END IF
ELSE
  ZXHAT => XXHAT
  ZYHAT => XYHAT
  ZZHAT => XZHAT
  GSLEVE => LSLEVE
END IF

IIU = SIZE(ZXHAT)
IJU = SIZE(ZYHAT)
ALLOCATE(ZXHATM(IIU),ZYHATM(IJU))
!ZXHATM(IIU) and ZYHATM(IJU) are correct only on some processes
!but it is OK due to the way GATHER_XXFIELD is done
ZXHATM(1:IIU-1) = 0.5*(ZXHAT(1:IIU-1)+ZXHAT(2:IIU))
ZXHATM(IIU)     = 2.*ZXHAT(IIU)-ZXHATM(IIU-1)
ZYHATM(1:IJU-1) = 0.5*(ZYHAT(1:IJU-1)+ZYHAT(2:IJU))
ZYHATM(IJU)     = 2.*ZYHAT(IJU)-ZYHATM(IJU-1)
!
IF (LCARTESIAN) THEN
  YSTDNAMEPREFIX = 'plane'
ELSE
  YSTDNAMEPREFIX = 'projection'
ENDIF

if ( Associated( tpfile%tncdims ) ) then
  tzdim_ni   => tpfile%tncdims%tdims(NMNHDIM_NI)
  tzdim_nj   => tpfile%tncdims%tdims(NMNHDIM_NJ)
  tzdim_ni_u => tpfile%tncdims%tdims(NMNHDIM_NI_U)
  tzdim_nj_u => tpfile%tncdims%tdims(NMNHDIM_NJ_U)
  tzdim_ni_v => tpfile%tncdims%tdims(NMNHDIM_NI_V)
  tzdim_nj_v => tpfile%tncdims%tdims(NMNHDIM_NJ_V)
else
  tzdim_ni   => Null()
  tzdim_nj   => Null()
  tzdim_ni_u => Null()
  tzdim_nj_u => Null()
  tzdim_ni_v => Null()
  tzdim_nj_v => Null()
end if

!If the file is a Z-split subfile, coordinates are already collected
if ( .not. associated( tpfile%tmainfile ) ) then
  call Gather_hor_coord1d( 'X', zxhat,  zxhat_glob  )
  call Gather_hor_coord1d( 'X', zxhatm, zxhatm_glob )
  call Gather_hor_coord1d( 'Y', zyhat,  zyhat_glob  )
  call Gather_hor_coord1d( 'Y', zyhatm, zyhatm_glob )
end if

call Write_hor_coord1d( tzdim_ni,   'x-dimension of the grid', &
                        trim(ystdnameprefix)//'_x_coordinate',              'x', 0.,    jphext, jphext, zxhatm_glob )
call Write_hor_coord1d( tzdim_nj,   'y-dimension of the grid', &
                        trim(ystdnameprefix)//'_y_coordinate',               'y', 0.,   jphext, jphext, zyhatm_glob )
call Write_hor_coord1d( tzdim_ni_u, 'x-dimension of the grid at u location', &
                        trim(ystdnameprefix)//'_x_coordinate_at_u_location', 'x', -0.5, jphext, 0,      zxhat_glob  )
call Write_hor_coord1d( tzdim_nj_u, 'y-dimension of the grid at u location', &
                        trim(ystdnameprefix)//'_y_coordinate_at_u_location', 'y', 0.,   jphext, jphext, zyhatm_glob )
call Write_hor_coord1d( tzdim_ni_v, 'x-dimension of the grid at v location', &
                        trim(ystdnameprefix)//'_x_coordinate_at_v_location', 'x', 0.,   jphext, jphext, zxhatm_glob )
call Write_hor_coord1d( tzdim_nj_v, 'y-dimension of the grid at v location', &
                        trim(ystdnameprefix)//'_y_coordinate_at_v_location', 'y', -0.5, jphext, 0,      zyhat_glob  )

!The z?hat*_glob were allocated in Gather_hor_coord1d calls
!Deallocate only if it is a non Z-split file or the last Z-split subfile
gdealloc = .false.
if ( associated( tpfile%tmainfile ) ) then
  if ( tpfile%cname == tpfile%tmainfile%tfiles_ioz(tpfile%tmainfile%nsubfiles_ioz)%tfile%cname ) gdealloc = .true.
else if ( tpfile%nsubfiles_ioz == 0 .and. .not. associated( tpfile%tmainfile ) ) then
  gdealloc = .true.
end if

if ( gdealloc ) deallocate( zxhat_glob, zxhatm_glob, zyhat_glob, zyhatm_glob )

IF (.NOT.LCARTESIAN) THEN
  !
  !Compute latitude/longitude for the Arakawa points
  !
  ALLOCATE(ZLAT(IIU,IJU),ZLON(IIU,IJU))

  !If the file is a Z-split subfile, coordinates are already collected
  if ( .not. associated( tpfile%tmainfile ) ) then
    call Gather_hor_coord2d( zxhatm, zyhatm, zlatm_glob, zlonm_glob )
    call Gather_hor_coord2d( zxhat,  zyhatm, zlatu_glob, zlonu_glob )
    call Gather_hor_coord2d( zxhatm, zyhat,  zlatv_glob, zlonv_glob )
    call Gather_hor_coord2d( zxhat,  zyhat,  zlatf_glob, zlonf_glob )
  end if

  ! Mass point
  call Write_hor_coord2d( zlatm_glob, zlonm_glob, 'latitude',  'longitude')
  ! u point
  call Write_hor_coord2d( zlatu_glob, zlonu_glob, 'latitude_u','longitude_u')
  ! v point
  call Write_hor_coord2d( zlatv_glob, zlonv_glob, 'latitude_v','longitude_v')
  ! xi vorticity point (=f point =uv point)
  call Write_hor_coord2d( zlatf_glob, zlonf_glob, 'latitude_f','longitude_f')

  DEALLOCATE(ZLAT,ZLON)

  !The zlat/lon._glob were allocated in Gather_hor_coord2d calls
  !Deallocate only if it is non Z-split file or the last Z-split subfile
  if ( gdealloc ) deallocate( zlatm_glob, zlonm_glob, zlatu_glob, zlonu_glob, zlatv_glob, zlonv_glob, zlatf_glob, zlonf_glob )
END IF
!
DEALLOCATE(ZXHATM,ZYHATM)
!
IF (TPFILE%LMASTER) THEN !vertical coordinates in the transformed space are the same on all processes
  IF (TRIM(YPROGRAM)/='PGD' .AND. TRIM(YPROGRAM)/='NESPGD' .AND. TRIM(YPROGRAM)/='ZOOMPG' &
      .AND. .NOT.(TRIM(YPROGRAM)=='REAL' .AND. CSTORAGE_TYPE=='SU') ) THEN !condition to detect PREP_SURFEX
    !
    IKU = SIZE(ZZHAT)
    ALLOCATE(ZZHATM(IKU))
    ZZHATM(1:IKU-1) = 0.5 * (ZZHAT(2:IKU)+ZZHAT(1:IKU-1))
    ZZHATM(IKU)     = 2.* ZZHAT(IKU) - ZZHATM(IKU-1)
    !
    CALL WRITE_VER_COORD(tpfile%tncdims%tdims(NMNHDIM_LEVEL),  'position z in the transformed space',              '', &
                         'altitude',               0., JPVEXT,JPVEXT,ZZHATM)
    !
    CALL WRITE_VER_COORD(tpfile%tncdims%tdims(NMNHDIM_LEVEL_W),'position z in the transformed space at w location','', &
                         'altitude_at_w_location',-0.5,JPVEXT,0,     ZZHAT)
    !
    DEALLOCATE(ZZHATM)
  END IF
END IF
!
!Write time scale
IF (TPFILE%LMASTER) THEN !Time scale is the same on all processes
  IF (TRIM(YPROGRAM)/='PGD' .AND. TRIM(YPROGRAM)/='NESPGD' .AND. TRIM(YPROGRAM)/='ZOOMPG' &
      .AND. .NOT.(TRIM(YPROGRAM)=='REAL' .AND. CSTORAGE_TYPE=='SU') ) THEN !condition to detect PREP_SURFEX
    if ( tpfile%ctype /= 'MNHDIACHRONIC' ) &
      CALL WRITE_TIME_COORD(tpfile%tncdims%tdims(NMNHDIM_TIME))
  END IF
END IF

IF (GCHANGEMODEL) CALL GO_TOMODEL_ll(IMI,IRESP)

CONTAINS
subroutine Gather_hor_coord1d( haxis, pcoords_loc, pcoords_glob )
  use mode_allocbuffer_ll, only: Allocbuffer_ll
  use mode_gather_ll,      only: Gather_xxfield

  character(len=*),            intent(in)  :: haxis
  real, dimension(:),          intent(in)  :: pcoords_loc
  real, dimension(:), pointer, intent(out) :: pcoords_glob

  character(len=2) :: ydir
  integer          :: ierr
  logical          :: galloc

  if ( haxis == 'X' ) then
    ydir = 'XX'
  else if ( haxis == 'Y' ) then
    ydir = 'YY'
  else
    call Print_msg( NVERB_FATAL, 'IO', 'Gather_hor_coord1d', 'invalid haxis ('//trim(haxis)//')' )
  end if

  ! Allocate pcoords_glob
  if ( gsmonoproc ) then ! sequential execution
    allocate( pcoords_glob( size( pcoords_loc) ) )
  else if ( tpfile%nsubfiles_ioz > 0 ) then
    !If there are Z-split subfiles, all subfile writers need the coordinates
    call Allocbuffer_ll( pcoords_glob, pcoords_loc, ydir, galloc )
  else if ( .not. tpfile%lmaster ) then
    allocate( pcoords_glob(0 ) ) !to prevent false positive with valgrind
  else !Master process
    call Allocbuffer_ll( pcoords_glob, pcoords_loc, ydir, galloc )
  end if

  !Gather coordinates
  if ( gsmonoproc ) then ! sequential execution
      pcoords_glob(: ) = pcoords_loc(: )
  else ! multiprocesses execution
      call Gather_xxfield( ydir, pcoords_loc, pcoords_glob, tpfile%nmaster_rank, tpfile%nmpicomm )
  endif

  !If the file has Z-split subfiles, broadcast the coordinates to all processes
  !PW: TODO: broadcast only to subfile writers
  if ( tpfile%nsubfiles_ioz > 0 ) &
    call MPI_BCAST( pcoords_glob, size( pcoords_glob ), MNHREAL_MPI, tpfile%nmaster_rank - 1,  tpfile%nmpicomm, ierr )
end subroutine Gather_hor_coord1d


subroutine Gather_hor_coord2d( px, py, plat_glob, plon_glob )
  use mode_allocbuffer_ll, only: Allocbuffer_ll
  use mode_gather_ll,      only: Gather_xyfield

  real,dimension(:), intent(in) :: px
  real,dimension(:), intent(in) :: py
  real, dimension(:,:), pointer, intent(out) :: plat_glob
  real, dimension(:,:), pointer, intent(out) :: plon_glob

  integer          :: ierr
  logical :: galloc1, galloc2

  call Sm_latlon( xlatori, xlonori,                             &
                  spread( source = px, dim = 2, ncopies = iju), &
                  spread( source = py, dim = 1, ncopies = iiu), &
                  zlat, zlon )

  ! Allocate coordinate arrays
  if ( gsmonoproc ) then ! sequential execution
    allocate( plat_glob( size( zlat, 1 ), size( zlat, 2 ) ) )
    allocate( plon_glob( size( zlon, 1 ), size( zlon, 2 ) ) )
  else if ( tpfile%nsubfiles_ioz > 0 ) then
    !If there are Z-split subfiles, all subfile writers need the coordinates
    call Allocbuffer_ll( plat_glob, zlat, 'XY', galloc1 )
    call Allocbuffer_ll( plon_glob, zlon, 'XY', galloc2 )
  else if ( .not. tpfile%lmaster ) then
    allocate( plat_glob( 0, 0 ), plon_glob( 0, 0 ) ) !to prevent false positive with valgrind
  else !Master process
    call Allocbuffer_ll( plat_glob, zlat, 'XY', galloc1 )
    call Allocbuffer_ll( plon_glob, zlon, 'XY', galloc2 )
  end if

  !Gather coordinates
  if ( gsmonoproc ) then ! sequential execution
      plat_glob(:, : ) = zlat(:, : )
      plon_glob(:, : ) = zlon(:, : )
  else ! multiprocesses execution
      call Gather_xyfield( zlat, plat_glob, tpfile%nmaster_rank, tpfile%nmpicomm )
      call Gather_xyfield( zlon, plon_glob, tpfile%nmaster_rank, tpfile%nmpicomm )
  endif

  !If the file has Z-split subfiles, broadcast the coordinates to all processes
  !PW: TODO: broadcast only to subfile writers
  if ( tpfile%nsubfiles_ioz > 0 ) then
    call MPI_BCAST( plat_glob, size( plat_glob ), MNHREAL_MPI, tpfile%nmaster_rank - 1,  tpfile%nmpicomm, ierr )
    call MPI_BCAST( plon_glob, size( plon_glob ), MNHREAL_MPI, tpfile%nmaster_rank - 1,  tpfile%nmpicomm, ierr )
  end if
end subroutine Gather_hor_coord2d


subroutine Write_hor_coord1d(TDIM,HLONGNAME,HSTDNAME,HAXIS,PSHIFT,KBOUNDLOW,KBOUNDHIGH,PCOORDS)
  USE MODE_ALLOCBUFFER_ll, ONLY: ALLOCBUFFER_ll
  USE MODE_GATHER_ll,      ONLY: GATHER_XXFIELD

  TYPE(tdimnc), POINTER,      INTENT(IN) :: TDIM
  CHARACTER(LEN=*),           INTENT(IN) :: HLONGNAME
  CHARACTER(LEN=*),           INTENT(IN) :: HSTDNAME
  CHARACTER(LEN=*),           INTENT(IN) :: HAXIS
  REAL,                       INTENT(IN) :: PSHIFT
  INTEGER,                    INTENT(IN) :: KBOUNDLOW
  INTEGER,                    INTENT(IN) :: KBOUNDHIGH
  REAL, DIMENSION(:), TARGET, INTENT(IN) :: PCOORDS

  CHARACTER(LEN=64)             :: YRANGE
  CHARACTER(LEN=:),ALLOCATABLE  :: YVARNAME
  INTEGER                       :: IRESP
  INTEGER                       :: ISIZE
  INTEGER(KIND=CDFINT)          :: IVARID
  INTEGER(KIND=CDFINT)          :: IVDIM
  INTEGER(KIND=CDFINT)          :: ISTATUS

  IF (TPFILE%LMASTER) THEN
    isize    = tdim%nlen
    yvarname = Trim( tdim%cname )
    ivdim    = tdim%nid

    ISTATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
    IF (ISTATUS /= NF90_NOERR) THEN
      ! Define the coordinate variable
      ISTATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHREAL_NF90, IVDIM, IVARID)
      IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'Write_hor_coord1d','NF90_DEF_VAR',trim(YVARNAME))
    ELSE
      CALL PRINT_MSG(NVERB_ERROR,'IO','Write_hor_coord1d',TRIM(YVARNAME)//' already defined')
    END IF

    ! Write metadata
    ISTATUS = NF90_PUT_ATT(INCID, IVARID, 'long_name',HLONGNAME)
    IF (ISTATUS /= NF90_NOERR) &
      CALL IO_Err_handle_nc4(istatus,'Write_hor_coord1d','NF90_PUT_ATT','long_name for '//trim(YVARNAME))

    ISTATUS = NF90_PUT_ATT(INCID, IVARID, 'standard_name',HSTDNAME)
    IF (ISTATUS /= NF90_NOERR) &
      CALL IO_Err_handle_nc4(istatus,'Write_hor_coord1d','NF90_PUT_ATT','standard_name for '//trim(YVARNAME))

    ISTATUS = NF90_PUT_ATT(INCID, IVARID, 'units','m')
    IF (ISTATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'Write_hor_coord1d','NF90_PUT_ATT','units for '//trim(YVARNAME))

    ISTATUS = NF90_PUT_ATT(INCID, IVARID, 'axis',HAXIS)
    IF (ISTATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'Write_hor_coord1d','NF90_PUT_ATT','axis for '//trim(YVARNAME))

    ISTATUS = NF90_PUT_ATT(INCID, IVARID, 'c_grid_axis_shift',PSHIFT)
    IF (ISTATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'Write_hor_coord1d','NF90_PUT_ATT','c_grid_axis_shift for ' &
                                                     //trim(YVARNAME))

    WRITE(YRANGE,'( I0,":",I0 )') 1+KBOUNDLOW,ISIZE-KBOUNDHIGH
    ISTATUS = NF90_PUT_ATT(INCID, IVARID, 'c_grid_dynamic_range',TRIM(YRANGE))
    IF (ISTATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'Write_hor_coord1d','NF90_PUT_ATT','c_grid_dynamic_range for ' &
                                                     //trim(YVARNAME))

    ! Write the data
    ISTATUS = NF90_PUT_VAR(INCID, IVARID, PCOORDS)
    IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'Write_hor_coord1d','NF90_PUT_VAR',trim(YVARNAME),IRESP)
  END IF
end subroutine Write_hor_coord1d


subroutine Write_hor_coord2d( plat, plon, hlat, hlon )
  real,dimension(:,:), intent(in) :: plat
  real,dimension(:,:), intent(in) :: plon
  character(len=*),    intent(in) :: hlat
  character(len=*),    intent(in) :: hlon

  if ( tpfile%lmaster ) then
    call Find_field_id_from_mnhname( hlat, id, iresp )
    call IO_Field_write_nc4_x2( tpfile, tfieldlist(id ), plat, iresp, oiscoord = .true. )
    call Find_field_id_from_mnhname( hlon, id, iresp )
    call IO_Field_write_nc4_x2( tpfile, tfieldlist(id ), plon, iresp, oiscoord = .true. )
  end if
end subroutine Write_hor_coord2d


SUBROUTINE WRITE_VER_COORD(TDIM,HLONGNAME,HSTDNAME,HCOMPNAME,PSHIFT,KBOUNDLOW,KBOUNDHIGH,PCOORDS)
  TYPE(tdimnc), POINTER, INTENT(IN) :: TDIM
  CHARACTER(LEN=*),      INTENT(IN) :: HLONGNAME
  CHARACTER(LEN=*),      INTENT(IN) :: HSTDNAME
  CHARACTER(LEN=*),      INTENT(IN) :: HCOMPNAME
  REAL,                  INTENT(IN) :: PSHIFT
  INTEGER,               INTENT(IN) :: KBOUNDLOW
  INTEGER,               INTENT(IN) :: KBOUNDHIGH
  REAL,DIMENSION(:),     INTENT(IN) :: PCOORDS

  CHARACTER(LEN=64)             :: YRANGE
  CHARACTER(LEN=:),ALLOCATABLE  :: YVARNAME
  INTEGER                       :: IRESP
  INTEGER                       :: ISIZE
  INTEGER                       :: JI
  INTEGER(KIND=CDFINT)          :: IVARID
  INTEGER(KIND=CDFINT)          :: IVDIM
  INTEGER(KIND=CDFINT)          :: STATUS

  isize    = tdim%nlen
  yvarname = Trim( tdim%cname )
  ivdim    = tdim%nid

  STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
  IF (STATUS /= NF90_NOERR) THEN
    ! Define the coordinate variable
    STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHREAL_NF90, IVDIM, IVARID)
    IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_VER_COORD','NF90_DEF_VAR',trim(YVARNAME))
  ELSE
    CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_VER_COORD',TRIM(YVARNAME)//' already defined')
  END IF

  ! Write metadata
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'long_name',HLONGNAME)
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_VER_COORD','NF90_PUT_ATT','long_name for '//trim(YVARNAME))
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'standard_name',HSTDNAME)
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_VER_COORD','NF90_PUT_ATT','standard_name for '//trim(YVARNAME))
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'units','m')
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_VER_COORD','NF90_PUT_ATT','units for '//trim(YVARNAME))
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'axis','Z')
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_VER_COORD','NF90_PUT_ATT','axis for '//trim(YVARNAME))
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'positive','up')
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_VER_COORD','NF90_PUT_ATT','positive for '//trim(YVARNAME))
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'c_grid_axis_shift',PSHIFT)
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_VER_COORD','NF90_PUT_ATT','c_grid_axis_shift for ' &
                                                   //trim(YVARNAME))
  WRITE(YRANGE,'( I0,":",I0 )') 1+KBOUNDLOW,ISIZE-KBOUNDHIGH
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'c_grid_dynamic_range',TRIM(YRANGE))
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_VER_COORD','NF90_PUT_ATT','c_grid_dynamic_range for ' &
                                                   //trim(YVARNAME))
  !
  IF (GSLEVE) THEN
    !Remark: ZS, ZSMT and ZTOP in the formula are the same for mass point or flux point
    STATUS = NF90_PUT_ATT(INCID, IVARID,'formula_terms','s: '//TRIM(YVARNAME)//                   &
                                        ' height: ZTOP oro_ls: ZSMT oro: ZS len1: LEN1 len2: LEN2')
    IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_VER_COORD','NF90_PUT_ATT','formula_terms for '//trim(YVARNAME))
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'formula_definition','z(n,k,j,i)=s(k)'//                                      &
                          '+ oro_ls(j,i)*sinh((height/len1)**1.35-(s(k)/len1)**1.35)/sinh((s(k)/len1)**1.35)'//        &
                          '+(oro(j,i)-oro_ls(j,i))*sinh((height/len2)**1.35-(s(k)/len2)**1.35)/sinh((s(k)/len2)**1.35)')
    IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_VER_COORD','NF90_PUT_ATT','formula_definition for ' &
                                                     //trim(YVARNAME))
  ELSE
    !Remark: ZS and ZTOP in the formula are the same for mass point or flux point
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'formula_terms','s: '//TRIM(YVARNAME)//' height: ZTOP orog: ZS')
    IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_VER_COORD','NF90_PUT_ATT','formula_terms for '//trim(YVARNAME))
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'formula_definition','z(n,k,j,i)=s(k)*(height-orog(j,i))/height+orog(j,i)')
    IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_VER_COORD','NF90_PUT_ATT','formula_definition for ' &
                                                     //trim(YVARNAME))
  ENDIF
  !
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'computed_standard_name',HCOMPNAME)
  IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_VER_COORD','NF90_PUT_ATT','computed_standard_name for ' &
                                                   //trim(YVARNAME))

  ! Write the data
  STATUS = NF90_PUT_VAR(INCID, IVARID, PCOORDS)
  IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_VER_COORD','NF90_PUT_VAR',trim(YVARNAME))

END SUBROUTINE WRITE_VER_COORD

SUBROUTINE WRITE_TIME_COORD(TDIM)
  use modd_field,      only: tfieldlist
  USE MODD_TIME_n,     ONLY: TDTMOD, TDTCUR
  USE MODD_TYPE_DATE

  USE MODE_DATETIME
  use mode_field,      only: Find_field_id_from_mnhname
  USE MODE_GRIDPROJ

  TYPE(tdimnc), POINTER, INTENT(IN) :: TDIM

  REAL                         :: ZDELTATIME
  CHARACTER(LEN=40)            :: YUNITS
  CHARACTER(LEN=:),ALLOCATABLE :: YVARNAME
  INTEGER(KIND=CDFINT)         :: IVARID
  INTEGER(KIND=CDFINT)         :: IVDIM
  INTEGER(KIND=CDFINT)         :: STATUS
  TYPE(DATE_TIME)              :: TZREF


  IF (ASSOCIATED(TDTCUR) .AND. ASSOCIATED(TDTMOD)) THEN
    yvarname = Trim( tdim%cname )
    ivdim    = tdim%nid

    STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
    IF (STATUS /= NF90_NOERR) THEN
      ! Define the coordinate variable
      STATUS = NF90_DEF_VAR(INCID, YVARNAME, MNHREAL_NF90, IVDIM, IVARID)
      IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_TIME_COORD','NF90_DEF_VAR',trim(YVARNAME))
    ELSE
      CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_TIME_COORD',TRIM(YVARNAME)//' already defined')
    END IF

    ! Write metadata
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'long_name','time axis')
    IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_TIME_COORD','NF90_PUT_ATT','long_name for '//trim(YVARNAME))
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'standard_name','time')
    IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_TIME_COORD','NF90_PUT_ATT','standard_name for '//trim(YVARNAME))
    WRITE(YUNITS,'( "seconds since ",I4.4,"-",I2.2,"-",I2.2," 00:00:00 +0:00" )') &
          TDTMOD%TDATE%YEAR,TDTMOD%TDATE%MONTH,TDTMOD%TDATE%DAY
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'units',YUNITS)
    IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_TIME_COORD','NF90_PUT_ATT','units for '//trim(YVARNAME))
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'axis','T')
    IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_TIME_COORD','NF90_PUT_ATT','axis for '//trim(YVARNAME))
    STATUS = NF90_PUT_ATT(INCID, IVARID,'calendar','standard')
    IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_TIME_COORD','NF90_PUT_ATT','calendar for '//trim(YVARNAME))

    ! Model beginning date (TDTMOD%TDATE) is used as the reference date
    ! Reference time is set to 0.
    TZREF = TDTMOD
    TZREF%TIME = 0.
    ! Compute the temporal distance from reference
    CALL DATETIME_DISTANCE(TZREF,TDTCUR,ZDELTATIME)
    ! Write the data
    STATUS = NF90_PUT_VAR(INCID, IVARID, ZDELTATIME)
    IF (status /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'WRITE_TIME_COORD','NF90_PUT_VAR',trim(YVARNAME))
  END IF

END SUBROUTINE WRITE_TIME_COORD

END SUBROUTINE IO_Coordvar_write_nc4


SUBROUTINE IO_Header_write_nc4(TPFILE)
!
TYPE(TFILEDATA), INTENT(IN)  :: TPFILE ! File structure
!
INTEGER(KIND=CDFINT) :: ISTATUS
!
IF (TRIM(TPFILE%CFORMAT)/='NETCDF4' .AND. TRIM(TPFILE%CFORMAT)/='LFICDF4') RETURN
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Header_write_nc4','called for file '//TRIM(TPFILE%CNAME))
!
IF (TPFILE%LMASTER)  THEN
  ISTATUS = NF90_PUT_ATT(TPFILE%NNCID, NF90_GLOBAL, 'Conventions', 'CF-1.7 COMODO-1.4')
  IF (ISTATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_FILE_WRITE_HEADER','NF90_PUT_ATT','Conventions')

#if (MNH_REAL == 8)
  ISTATUS = NF90_PUT_ATT(TPFILE%NNCID, NF90_GLOBAL, 'MNH_REAL', '8')
#else
  ISTATUS = NF90_PUT_ATT(TPFILE%NNCID, NF90_GLOBAL, 'MNH_REAL', '4')
#endif
  IF (ISTATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_FILE_WRITE_HEADER','NF90_PUT_ATT','MNH_REAL')

#if (MNH_INT == 4)
  ISTATUS = NF90_PUT_ATT(TPFILE%NNCID, NF90_GLOBAL, 'MNH_INT', '4')
#else
  ISTATUS = NF90_PUT_ATT(TPFILE%NNCID, NF90_GLOBAL, 'MNH_INT', '8')
#endif
  IF (ISTATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_FILE_WRITE_HEADER','NF90_PUT_ATT','MNH_INT')

  if ( tpfile%ldimreduced ) then
    istatus = NF90_PUT_ATT( tpfile%nncid, NF90_GLOBAL, 'MNH_REDUCE_DIMENSIONS_IN_FILES', '1')
  else
    istatus = NF90_PUT_ATT( tpfile%nncid, NF90_GLOBAL, 'MNH_REDUCE_DIMENSIONS_IN_FILES', '0')
  endif

!title

  !history
  CALL IO_History_append_nc4(TPFILE)

!institution

!source

!comment

!references
END IF
!
END SUBROUTINE IO_Header_write_nc4


SUBROUTINE IO_History_append_nc4(TPFILE)
!
USE MODD_IO, ONLY: TFILEDATA
!
TYPE(TFILEDATA), INTENT(IN)  :: TPFILE ! File structure
!
INTEGER,PARAMETER :: YEAR=1, MONTH=2, DAY=3, HH=5, MM=6, SS=7
!
CHARACTER(len=5)             :: YZONE
CHARACTER(LEN=:),ALLOCATABLE :: YCMD, YHISTORY, YHISTORY_NEW, YHISTORY_PREV
INTEGER                      :: ILEN_CMD, ILEN_PREV
INTEGER(KIND=CDFINT)         :: ILEN_NC
INTEGER(KIND=CDFINT)         :: ISTATUS
INTEGER,DIMENSION(8)         :: IDATETIME
!
IF (TRIM(TPFILE%CFORMAT)/='NETCDF4' .AND. TRIM(TPFILE%CFORMAT)/='LFICDF4') RETURN
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_History_append_nc4','called for file '//TRIM(TPFILE%CNAME))
!
IF (TPFILE%LMASTER)  THEN
  !Check if history attribute already exists in file and read it
  ISTATUS = NF90_INQUIRE_ATTRIBUTE(TPFILE%NNCID, NF90_GLOBAL, 'history', LEN=ILEN_NC)
  ILEN_PREV = int( ILEN_NC, kind=kind(ILEN_PREV) )
  IF (ISTATUS == NF90_NOERR) THEN
    ALLOCATE(CHARACTER(LEN=ILEN_PREV) :: YHISTORY_PREV)
    ISTATUS = NF90_GET_ATT(TPFILE%NNCID, NF90_GLOBAL, 'history', YHISTORY_PREV)
    IF (ISTATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(ISTATUS,'IO_History_append_nc4','NF90_GET_ATT','history')
    YHISTORY_PREV = YHISTORY_PREV
  ELSE
    ILEN_PREV = 0
  END IF

  !Get date and time
  call DATE_AND_TIME( VALUES=IDATETIME, ZONE=YZONE )
  call GET_COMMAND(lENGTH=ILEN_CMD)
  IF (ILEN_CMD>0) THEN
    ALLOCATE(CHARACTER(ILEN_CMD) :: YCMD)
    CALL GET_COMMAND(COMMAND=YCMD)
  END IF

  !Prepare new history entry
  ALLOCATE(CHARACTER(26+ILEN_CMD) :: YHISTORY_NEW)
  WRITE(YHISTORY_NEW, '( I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2,A5,": ", A )') &
        IDATETIME(YEAR),IDATETIME(MONTH),IDATETIME(DAY),IDATETIME(HH),IDATETIME(MM),IDATETIME(SS),YZONE, YCMD

  !Write full history
  IF (ILEN_PREV == 0) THEN
    YHISTORY = YHISTORY_NEW
  ELSE
    YHISTORY = YHISTORY_NEW//NEW_LINE('A')//YHISTORY_PREV
  END IF
  ISTATUS = NF90_PUT_ATT(TPFILE%NNCID, NF90_GLOBAL, 'history', YHISTORY  )
  IF (ISTATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_History_append_nc4','NF90_PUT_ATT','history')
END IF

END SUBROUTINE IO_History_append_nc4


end module mode_io_write_nc4
#else
!
! External dummy subroutines
!
subroutine IO_Coordvar_write_nc4(a, b)
use mode_msg
integer :: a, b
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Coordvar_write_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine IO_Coordvar_write_nc4
!
subroutine IO_Field_write_nc4(a, b, c, d, e, f, g)
use mode_msg
integer :: a, b, c, d, e, f, g
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine IO_Field_write_nc4
!
subroutine IO_Header_write_nc4(a)
use mode_msg
integer :: a
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Header_write_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine IO_Header_write_nc4
!
#endif
