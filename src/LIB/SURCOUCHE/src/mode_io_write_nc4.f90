!MNH_LIC Copyright 1994-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!  Modifications:
!    P. Wautelet : may 2016   : use NetCDF Fortran module
!    J.Escobar   : 14/12/2017 : Correction for MNH_INT=8
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/12/2018: split of mode_netcdf into multiple modules/files
!  P. Wautelet 10/01/2019: replace handle_err by io_handle_err_nc4 for better netCDF error messages
!  P. Wautelet 11/01/2019: NVERB_INFO->NVERB_WARNING for zero size fields
!  P. Wautelet 01/02/2019: IO_WRITE_COORDVAR_NC4: bug: use of non-associated pointers (PIOCDF%DIM_Nx_y)
!  P. Wautelet 18/09/2019: correct support of 64bit integers (MNH_INT=8)
!  P. Wautelet 19/09/2019: temporary workaround for netCDF bug if MNH_INT=8 (if netCDF fortran < 4.4.5)
!  P. Wautelet 11/02/2020: add 'dims' attribute in IO_Write_field_header_split_nc4
!-----------------------------------------------------------------
#if defined(MNH_IOCDF4)
module mode_io_write_nc4

use modd_io_ll,        only: gsmonoproc, tfiledata
use modd_netcdf,       only: dimcdf, IDCDF_KIND, iocdf

use mode_field,        only: tfielddata
use mode_io_tools_nc4, only: cleanmnhname, fillvdims, getdimcdf, getstrdimid, io_handle_err_nc4
use mode_msg

use NETCDF,            only: NF90_CHAR, NF90_DOUBLE, NF90_FLOAT, NF90_INT, NF90_INT1, NF90_INT64, &
                             NF90_GLOBAL, NF90_NOERR,                                             &
                             NF90_DEF_VAR, NF90_DEF_VAR_DEFLATE, NF90_GET_ATT, NF90_INQ_VARID,    &
                             NF90_INQUIRE_ATTRIBUTE, NF90_PUT_ATT, NF90_PUT_VAR

implicit none

private

public :: IO_Write_field_header_split_nc4
public :: io_write_coordvar_nc4, io_write_field_nc4, io_write_header_nc4

INTERFACE IO_WRITE_FIELD_NC4
   MODULE PROCEDURE IO_WRITE_FIELD_NC4_X0,IO_WRITE_FIELD_NC4_X1, &
                    IO_WRITE_FIELD_NC4_X2,IO_WRITE_FIELD_NC4_X3, &
                    IO_WRITE_FIELD_NC4_X4,IO_WRITE_FIELD_NC4_X5, &
                    IO_WRITE_FIELD_NC4_X6,                       &
                    IO_WRITE_FIELD_NC4_N0,IO_WRITE_FIELD_NC4_N1, &
                    IO_WRITE_FIELD_NC4_N2,IO_WRITE_FIELD_NC4_N3, &
                    IO_WRITE_FIELD_NC4_L0,IO_WRITE_FIELD_NC4_L1, &
                    IO_WRITE_FIELD_NC4_C0,IO_WRITE_FIELD_NC4_C1, &
                    IO_WRITE_FIELD_NC4_T0
END INTERFACE IO_WRITE_FIELD_NC4

integer,parameter :: NSTRINGCHUNKSIZE = 16 !Dimension of the chunks of strings
                                           !(to limit the number of dimensions for strings)

integer(kind=IDCDF_KIND),parameter :: SHUFFLE = 1 !Set to 1 for (usually) better compression
integer(kind=IDCDF_KIND),parameter :: DEFLATE = 1

contains

subroutine IO_Write_field_header_split_nc4( tpfile, tpfield, knblocks )
use modd_parameters, only : jphext

use mode_field,      only: TYPEREAL
use mode_tools_ll,   only: Get_globaldims_ll

type(tfiledata),       intent(in) :: tpfile
type(tfielddata),      intent(in) :: tpfield
integer,               intent(in) :: knblocks

character(len=len(tpfield%cmnhname))  :: yvarname
integer                               :: iimax, ijmax
integer(kind=idcdf_kind)              :: istatus
integer(kind=idcdf_kind)              :: incid
integer(kind=idcdf_kind)              :: ivarid
integer(kind=idcdf_kind),dimension(3) :: ishape

call Print_msg( NVERB_DEBUG, 'IO', 'IO_Write_field_header_split_nc4', 'called for field '//trim( tpfield%cmnhname ) )

if ( tpfield%ntype /= TYPEREAL ) then
  call Print_msg( NVERB_ERROR, 'IO', 'IO_Write_field_header_split_nc4', 'invalid ntype for field '//trim( tpfield%cmnhname ) )
  return
end if

! Get the Netcdf file ID
incid = tpfile%nncid

call Cleanmnhname( tpfield%cmnhname, yvarname )

istatus = NF90_INQ_VARID( incid, yvarname, ivarid )
if ( istatus /= NF90_NOERR ) then

#if (MNH_REAL == 8)
  istatus = NF90_DEF_VAR( incid, yvarname, NF90_DOUBLE, ivarid)
#else
  istatus = NF90_DEF_VAR( incid, yvarname, NF90_FLOAT,  ivarid)
#endif

  if ( tpfield%ndims /= 3 ) call Print_msg( NVERB_FATAL, 'IO', 'IO_Write_field_header_split_nc4', &
                  trim( tpfile%cname )//': '//trim( yvarname )//': NDIMS should be 3' )

  if ( tpfield%cdir /= 'XY' ) call Print_msg( NVERB_FATAL, 'IO', 'IO_Write_field_header_split_nc4', &
                  trim( tpfile%cname )//': '//trim( yvarname )//': CDIR should be XY' )

  call Get_globaldims_ll( iimax, ijmax )
  ishape(1) = int( iimax + 2 * jphext, kind = idcdf_kind )
  ishape(2) = int( ijmax + 2 * jphext, kind = idcdf_kind )
  ishape(3) = knblocks
  call IO_Write_field_attr_nc4( tpfile, tpfield, ivarid, .false., kshape = ishape )

  if ( istatus /= NF90_NOERR ) call IO_Handle_err_nc4( istatus, 'IO_Write_field_header_split_nc4', 'NF90_DEF_VAR', trim(yvarname) )

  istatus = NF90_PUT_ATT( incid, ivarid,'split_variable', 'yes')
  if ( istatus /= NF90_NOERR ) call IO_HANDLE_ERR_NC4( istatus, 'IO_Write_field_header_split_nc4', 'NF90_PUT_ATT', &
                                                     'split_variable for '//trim( tpfield%cmnhname ) )

  istatus = NF90_PUT_ATT( incid, ivarid,'split_mode', 'Z')
  if ( istatus /= NF90_NOERR ) call IO_HANDLE_ERR_NC4( istatus, 'IO_Write_field_header_split_nc4', 'NF90_PUT_ATT', &
                                                     'split_mode for '//trim( tpfield%cmnhname ) )

  istatus = NF90_PUT_ATT( incid, ivarid,'split_nblocks', knblocks )
  if ( istatus /= NF90_NOERR ) call IO_HANDLE_ERR_NC4( istatus, 'IO_Write_field_header_split_nc4', 'NF90_PUT_ATT', &
                                                     'split_nblocks for '//trim( tpfield%cmnhname ) )

  istatus = NF90_PUT_ATT( incid, ivarid,'split_nfiles', tpfile%nsubfiles_ioz )
  if ( istatus /= NF90_NOERR ) call IO_HANDLE_ERR_NC4( istatus, 'IO_Write_field_header_split_nc4', 'NF90_PUT_ATT', &
                                                     'split_nfiles for '//trim( tpfield%cmnhname ) )

  istatus = NF90_PUT_ATT( incid, ivarid,'split_distribution', 'round-robin' )
  if ( istatus /= NF90_NOERR ) call IO_HANDLE_ERR_NC4( istatus, 'IO_Write_field_header_split_nc4', 'NF90_PUT_ATT', &
                                                     'split_distribution for '//trim( tpfield%cmnhname ) )

  istatus = NF90_PUT_ATT( incid, ivarid,'ndims', tpfield%ndims )
  if ( istatus /= NF90_NOERR ) call IO_HANDLE_ERR_NC4( istatus, 'IO_Write_field_header_split_nc4', 'NF90_PUT_ATT', &
                                                     'ndims for '//trim( tpfield%cmnhname ) )

  istatus = NF90_PUT_ATT( incid, ivarid,'dims', ishape )
  if ( istatus /= NF90_NOERR ) call IO_HANDLE_ERR_NC4( istatus, 'IO_Write_field_header_split_nc4', 'NF90_PUT_ATT', &
                                                     'dims for '//trim( tpfield%cmnhname ) )

  if ( tpfield%ltimedep ) then
    istatus = NF90_PUT_ATT( incid, ivarid,'time_dependent', 'yes' )
  else
    istatus = NF90_PUT_ATT( incid, ivarid,'time_dependent', 'no' )
  end if
  if ( istatus /= NF90_NOERR ) call IO_HANDLE_ERR_NC4( istatus, 'IO_Write_field_header_split_nc4', 'NF90_PUT_ATT', &
                                                     'time_dependent for '//trim( tpfield%cmnhname ) )
else
  call Print_msg( NVERB_WARNING, 'IO', 'IO_Write_field_header_split_nc4', &
                  trim( tpfile%cname )//': '//trim( yvarname )//' already defined' )
end if

end subroutine IO_Write_field_header_split_nc4

SUBROUTINE IO_WRITE_FIELD_ATTR_NC4(TPFILE,TPFIELD,KVARID,OEXISTED,KSHAPE,HCALENDAR,OISCOORD)
!
USE MODD_CONF,   ONLY: CPROGRAM, LCARTESIAN
USE MODD_CONF_n, ONLY: CSTORAGE_TYPE
!
USE MODE_FIELD,  ONLY: TYPEINT, TYPEREAL
!
TYPE(TFILEDATA),                               INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),                              INTENT(IN) :: TPFIELD
INTEGER(KIND=IDCDF_KIND),                      INTENT(IN) :: KVARID
LOGICAL,                                       INTENT(IN) :: OEXISTED !True if variable was already defined
INTEGER(KIND=IDCDF_KIND),DIMENSION(:),OPTIONAL,INTENT(IN) :: KSHAPE
CHARACTER(LEN=*),                     OPTIONAL,INTENT(IN) :: HCALENDAR
LOGICAL,                              OPTIONAL,INTENT(IN) :: OISCOORD   ! Is a coordinate variable (->do not write coordinates attribute)
!
INTEGER(KIND=IDCDF_KIND)     :: INCID
INTEGER(KIND=IDCDF_KIND)     :: STATUS
CHARACTER(LEN=:),ALLOCATABLE :: YCOORDS
LOGICAL                      :: GISCOORD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_ATTR_NC4','called for field '//TRIM(TPFIELD%CMNHNAME))
!
IF(LEN_TRIM(TPFIELD%CSTDNAME)==0 .AND. LEN_TRIM(TPFIELD%CLONGNAME)==0) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_ATTR_NC4','at least long_name or standard_name must be provided &
  &to respect CF-convention for variable '//TRIM(TPFIELD%CMNHNAME))
ENDIF
!
IF (TPFIELD%NDIMS>1 .AND. .NOT.PRESENT(KSHAPE)) &
  CALL PRINT_MSG(NVERB_FATAL,'IO','IO_WRITE_FIELD_ATTR_NC4','KSHAPE not provided for '//TRIM(TPFIELD%CMNHNAME))
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
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_ATTR_NC4','TPFIELD%CSTDNAME not set for variable '//TRIM(TPFIELD%CMNHNAME))
ELSE
  STATUS = NF90_PUT_ATT(INCID, KVARID,'standard_name', TRIM(TPFIELD%CSTDNAME))
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_ATTR_NC4','NF90_PUT_ATT','standard_name for ' &
                                                   //trim(TPFIELD%CMNHNAME))
ENDIF
!
! Long_name attribute definition (CF convention)
IF(LEN_TRIM(TPFIELD%CLONGNAME)==0) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_ATTR_NC4','TPFIELD%CLONGNAME not set for variable '//TRIM(TPFIELD%CMNHNAME))
ELSE
  STATUS = NF90_PUT_ATT(INCID, KVARID,'long_name', TRIM(TPFIELD%CLONGNAME))
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_ATTR_NC4','NF90_PUT_ATT','long_name for ' &
                                                   //trim(TPFIELD%CMNHNAME))
ENDIF
!
! Canonical units attribute definition (CF convention)
IF(LEN_TRIM(TPFIELD%CUNITS)==0) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_ATTR_NC4','TPFIELD%CUNITS not set for variable '//TRIM(TPFIELD%CMNHNAME))
ELSE
  STATUS = NF90_PUT_ATT(INCID, KVARID,'units', TRIM(TPFIELD%CUNITS))
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_ATTR_NC4','NF90_PUT_ATT','units for ' &
                                                   //trim(TPFIELD%CMNHNAME))
ENDIF
!
! GRID attribute definition
IF(TPFIELD%NGRID<0) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_ATTR_NC4','TPFIELD%NGRID not set for variable '//TRIM(TPFIELD%CMNHNAME))
!Do not write GRID attribute if NGRID=0
ELSE IF (TPFIELD%NGRID>0) THEN
  STATUS = NF90_PUT_ATT(INCID, KVARID, 'grid', TPFIELD%NGRID)
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_ATTR_NC4','NF90_PUT_ATT','grid for ' &
                                                   //trim(TPFIELD%CMNHNAME))
ENDIF
!
! COMMENT attribute definition
IF(LEN_TRIM(TPFIELD%CCOMMENT)==0) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_ATTR_NC4','TPFIELD%CCOMMENT not set for variable '//TRIM(TPFIELD%CMNHNAME))
ELSE
  STATUS = NF90_PUT_ATT(INCID, KVARID,'comment', TRIM(TPFIELD%CCOMMENT))
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_ATTR_NC4','NF90_PUT_ATT','comment for ' &
                                                   //trim(TPFIELD%CMNHNAME))
ENDIF
!
! Calendar (CF convention)
IF(PRESENT(HCALENDAR)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_ATTR_NC4','CALENDAR provided for variable '//TRIM(TPFIELD%CMNHNAME))
  STATUS = NF90_PUT_ATT(INCID, KVARID,'calendar', TRIM(HCALENDAR))
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_ATTR_NC4','NF90_PUT_ATT','calendar for ' &
                                                   //trim(TPFIELD%CMNHNAME))
ENDIF
!
! Coordinates (CF convention)
IF (.NOT.GISCOORD) THEN
  !0D: nothing to do
  !1D: no direct correspondance with latitude(_x)/longitude(_x) 2D variables => nothing to do
  IF (.NOT.LCARTESIAN .AND. TPFIELD%NDIMS>1 .AND. TPFIELD%NGRID/=0) THEN
    IF (TPFIELD%CDIR=='XY') THEN
      IF (KSHAPE(1)==TPFILE%TNCCOORDS(1,TPFIELD%NGRID)%TDIM%LEN .AND. KSHAPE(2)==TPFILE%TNCCOORDS(2,TPFIELD%NGRID)%TDIM%LEN ) THEN
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
            CALL PRINT_MSG(NVERB_ERROR,'IO','IO_WRITE_FIELD_ATTR_NC4','invalid NGRID for variable '//TRIM(TPFIELD%CMNHNAME))
        END SELECT
        !
        STATUS = NF90_PUT_ATT(INCID, KVARID,'coordinates',YCOORDS)
        IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_ATTR_NC4','NF90_PUT_ATT','coordinates')
        DEALLOCATE(YCOORDS)
      ELSE
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_ATTR_NC4','coordinates not implemented for variable ' &
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
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_ATTR_NC4','_FillValue is not outside of valid_min - valid_max'// &
                                                                'interval for variable '//TRIM(TPFIELD%CMNHNAME))
  !
  ! Fillvalue (CF/COMODO convention)
  ! Remarks: * the attribute '_FillValue' is also recognized by the netCDF library
  !            and is used when pre-filling a variable
  !          * it cannot be modified if some data has already been written (->check OEXISTED)
#if ( MNH_INT == 4 )
!BUG: NF90_PUT_ATT does not work for NF90_INT64 and _FillValue attribute if netCDF-fortran version < 4.4.5 (bug in netCDF)
!     (see https://github.com/Unidata/netcdf-fortran/issues/62)
  IF(.NOT.OEXISTED) THEN
    STATUS = NF90_PUT_ATT(INCID, KVARID,'_FillValue', TPFIELD%NFILLVALUE)
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_ATTR_NC4','NF90_PUT_ATT','_FillValue')
  END IF
#endif
  !
  ! Valid_min/max (CF/COMODO convention)
  STATUS = NF90_PUT_ATT(INCID, KVARID,'valid_min', TPFIELD%NVALIDMIN)
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_ATTR_NC4','NF90_PUT_ATT','valid_min')
  !
  STATUS = NF90_PUT_ATT(INCID, KVARID,'valid_max',TPFIELD%NVALIDMAX)
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_ATTR_NC4','NF90_PUT_ATT','valid_max')
ENDIF
!
IF(TPFIELD%NTYPE==TYPEREAL .AND. TPFIELD%NDIMS>0) THEN
  IF (TPFIELD%XFILLVALUE>=TPFIELD%XVALIDMIN .AND. TPFIELD%XFILLVALUE<=TPFIELD%XVALIDMAX) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_ATTR_NC4','_FillValue is not outside of valid_min - valid_max'// &
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
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_ATTR_NC4','NF90_PUT_ATT','_FillValue')
  END IF
  !
  ! Valid_min/max (CF/COMODO convention)
  IF (TPFILE%LNCREDUCE_FLOAT_PRECISION) THEN
    STATUS = NF90_PUT_ATT(INCID, KVARID,'valid_min', REAL(TPFIELD%XVALIDMIN,KIND=4))
  ELSE
    STATUS = NF90_PUT_ATT(INCID, KVARID,'valid_min', TPFIELD%XVALIDMIN)
  END IF
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_ATTR_NC4','NF90_PUT_ATT','valid_min')
  !
  IF (TPFILE%LNCREDUCE_FLOAT_PRECISION) THEN
    STATUS = NF90_PUT_ATT(INCID, KVARID,'valid_max', REAL(TPFIELD%XVALIDMAX,KIND=4))
  ELSE
    STATUS = NF90_PUT_ATT(INCID, KVARID,'valid_max',TPFIELD%XVALIDMAX)
  END IF
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_ATTR_NC4','NF90_PUT_ATT','valid_max')
ENDIF
!
END SUBROUTINE IO_WRITE_FIELD_ATTR_NC4


SUBROUTINE IO_WRITE_FIELD_NC4_X0(TPFILE,TPFIELD,PFIELD,KRESP)
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
REAL,                  INTENT(IN) :: PFIELD
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER(KIND=IDCDF_KIND) :: STATUS
INTEGER(KIND=IDCDF_KIND) :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)) :: YVARNAME
INTEGER(KIND=IDCDF_KIND) :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                  :: IRESP
LOGICAL                  :: GEXISTED !True if variable was already defined
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_X0',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL CLEANMNHNAME(TPFIELD%CMNHNAME,YVARNAME)
!
! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (TPFIELD%LTIMEDEP) THEN
     ! Get the netcdf dimensions
     CALL FILLVDIMS(TPFILE, TPFIELD, INT(SHAPE(PFIELD),KIND=IDCDF_KIND), IVDIMS)
     ! Define the variable
#if (MNH_REAL == 8)
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_DOUBLE, IVDIMS, IVARID)
#else
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
#endif
     IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X0','NF90_DEF_VAR',trim(YVARNAME))
     DEALLOCATE(IVDIMS)
   ELSE
     ! Define the scalar variable
#if (MNH_REAL == 8)
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_DOUBLE, IVARID)
#else
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVARID)
#endif
     IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X0','NF90_DEF_VAR',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_X0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, PFIELD)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X0','NF90_PUT_VAR',trim(YVARNAME),IRESP)

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_X0


SUBROUTINE IO_WRITE_FIELD_NC4_X1(TPFILE,TPFIELD,PFIELD,KRESP)
!
TYPE(TFILEDATA),TARGET,INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
REAL,DIMENSION(:),     INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER(KIND=IDCDF_KIND) :: STATUS
INTEGER(KIND=IDCDF_KIND) :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)+4) :: YVARNAME
INTEGER(KIND=IDCDF_KIND) :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                  :: IRESP
LOGICAL                  :: GEXISTED !True if variable was already defined
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_X1',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL CLEANMNHNAME(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(PFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_X1','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL FILLVDIMS(TPFILE, TPFIELD, INT(SHAPE(PFIELD),KIND=IDCDF_KIND), IVDIMS)

   ! Define the variable
   IF (TPFILE%LNCREDUCE_FLOAT_PRECISION) THEN
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
   ELSE
#if (MNH_REAL == 8)
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_DOUBLE, IVDIMS, IVARID)
#else
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
#endif
   END IF
     IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X1','NF90_DEF_VAR',trim(YVARNAME))
   ! Add compression if asked for
   IF (TPFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TPFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X1','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_X1',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, PFIELD)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X1','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_X1


SUBROUTINE IO_WRITE_FIELD_NC4_X2(TPFILE,TPFIELD,PFIELD,KRESP,KVERTLEVEL,KZFILE,OISCOORD)
!
TYPE(TFILEDATA),TARGET,INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
REAL,DIMENSION(:,:),   INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,               INTENT(OUT):: KRESP
INTEGER,OPTIONAL,      INTENT(IN) :: KVERTLEVEL ! Number of the vertical level (needed for Z-level splitted files)
INTEGER,OPTIONAL,      INTENT(IN) :: KZFILE     ! Number of the Z-level splitted file
LOGICAL,OPTIONAL,      INTENT(IN) :: OISCOORD   ! Is a coordinate variable (->do not write coordinates attribute)
!
INTEGER(KIND=IDCDF_KIND) :: STATUS
INTEGER(KIND=IDCDF_KIND) :: INCID
CHARACTER(LEN=4)         :: YSUFFIX
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)+4) :: YVARNAME
INTEGER(KIND=IDCDF_KIND) :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                  :: IRESP
TYPE(TFIELDDATA)         :: TZFIELD
TYPE(TFILEDATA),POINTER  :: TZFILE
LOGICAL                  :: GEXISTED !True if variable was already defined
!
IRESP = 0
!
IF (PRESENT(KVERTLEVEL)) THEN
  WRITE(YSUFFIX,'(I4.4)') KVERTLEVEL
  IF (.NOT.PRESENT(KZFILE)) THEN
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_WRITE_FIELD_NC4_X2','KZFILE argument not provided')
    RETURN
  END IF
  IF (KZFILE>TPFILE%NSUBFILES_IOZ) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_WRITE_FIELD_NC4_X2','KZFILE value too high')
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
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_X2',TRIM(TZFILE%CNAME)//': writing '//TRIM(TZFIELD%CMNHNAME))
!
! Get the Netcdf file ID
INCID = TZFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL CLEANMNHNAME(TZFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(PFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_X2','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL FILLVDIMS(TZFILE, TZFIELD, INT(SHAPE(PFIELD),KIND=IDCDF_KIND), IVDIMS)

   ! Define the variable
   IF (TZFILE%LNCREDUCE_FLOAT_PRECISION) THEN
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
   ELSE
#if (MNH_REAL == 8)
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_DOUBLE, IVDIMS, IVARID)
#else
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
#endif
   END IF
     IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X2','NF90_DEF_VAR',trim(YVARNAME))
   ! Add compression if asked for
   IF (TZFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TZFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X2','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_X2',TRIM(TZFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TZFILE,TZFIELD,IVARID,GEXISTED,KSHAPE=INT(SHAPE(PFIELD),KIND=IDCDF_KIND),OISCOORD=OISCOORD)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, PFIELD)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X2','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_X2


SUBROUTINE IO_WRITE_FIELD_NC4_X3(TPFILE,TPFIELD,PFIELD,KRESP)
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
REAL,DIMENSION(:,:,:), INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER(KIND=IDCDF_KIND) :: STATUS
INTEGER(KIND=IDCDF_KIND) :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)) :: YVARNAME
INTEGER(KIND=IDCDF_KIND) :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                  :: IRESP
LOGICAL                  :: GEXISTED !True if variable was already defined
!
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_X3',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL CLEANMNHNAME(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(PFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_X3','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL FILLVDIMS(TPFILE, TPFIELD, INT(SHAPE(PFIELD),KIND=IDCDF_KIND), IVDIMS)

   ! Define the variable
   IF (TPFILE%LNCREDUCE_FLOAT_PRECISION) THEN
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
   ELSE
#if (MNH_REAL == 8)
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_DOUBLE, IVDIMS, IVARID)
#else
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
#endif
   END IF
   IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X3','NF90_DEF_VAR',trim(YVARNAME))

   ! Add compression if asked for
   IF (TPFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TPFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X3','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_X3',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TPFIELD,IVARID,GEXISTED,KSHAPE=INT(SHAPE(PFIELD),KIND=IDCDF_KIND))
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, PFIELD)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X3','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_X3


SUBROUTINE IO_WRITE_FIELD_NC4_X4(TPFILE,TPFIELD,PFIELD,KRESP)
!
TYPE(TFILEDATA),           INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),          INTENT(IN) :: TPFIELD
REAL,DIMENSION(:,:,:,:),   INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,                   INTENT(OUT):: KRESP
!
INTEGER(KIND=IDCDF_KIND) :: STATUS
INTEGER(KIND=IDCDF_KIND) :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)) :: YVARNAME
INTEGER(KIND=IDCDF_KIND) :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                  :: IRESP
LOGICAL                  :: GEXISTED !True if variable was already defined
!
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_X4',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL CLEANMNHNAME(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(PFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_X4','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL FILLVDIMS(TPFILE, TPFIELD, INT(SHAPE(PFIELD),KIND=IDCDF_KIND), IVDIMS)

   ! Define the variable
   IF (TPFILE%LNCREDUCE_FLOAT_PRECISION) THEN
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
   ELSE
#if (MNH_REAL == 8)
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_DOUBLE, IVDIMS, IVARID)
#else
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
#endif
   END IF
   IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X4','NF90_DEF_VAR',trim(YVARNAME))

   ! Add compression if asked for
   IF (TPFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TPFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X4','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_X4',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TPFIELD,IVARID,GEXISTED,KSHAPE=INT(SHAPE(PFIELD),KIND=IDCDF_KIND))
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, PFIELD)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X4','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_X4


SUBROUTINE IO_WRITE_FIELD_NC4_X5(TPFILE,TPFIELD,PFIELD,KRESP)
!
TYPE(TFILEDATA),           INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),          INTENT(IN) :: TPFIELD
REAL,DIMENSION(:,:,:,:,:), INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,                   INTENT(OUT):: KRESP
!
INTEGER(KIND=IDCDF_KIND) :: STATUS
INTEGER(KIND=IDCDF_KIND) :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)) :: YVARNAME
INTEGER(KIND=IDCDF_KIND) :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                  :: IRESP
LOGICAL                  :: GEXISTED !True if variable was already defined
!
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_X5',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL CLEANMNHNAME(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(PFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_X5','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL FILLVDIMS(TPFILE, TPFIELD, INT(SHAPE(PFIELD),KIND=IDCDF_KIND), IVDIMS)

   ! Define the variable
   IF (TPFILE%LNCREDUCE_FLOAT_PRECISION) THEN
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
   ELSE
#if (MNH_REAL == 8)
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_DOUBLE, IVDIMS, IVARID)
#else
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
#endif
   END IF
   IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X5','NF90_DEF_VAR',trim(YVARNAME))

   ! Add compression if asked for
   IF (TPFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TPFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X5','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_X5',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TPFIELD,IVARID,GEXISTED,KSHAPE=INT(SHAPE(PFIELD),KIND=IDCDF_KIND))
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, PFIELD)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X5','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_X5


SUBROUTINE IO_WRITE_FIELD_NC4_X6(TPFILE,TPFIELD,PFIELD,KRESP)
!
TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),            INTENT(IN) :: TPFIELD
REAL,DIMENSION(:,:,:,:,:,:), INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,                     INTENT(OUT):: KRESP
!
INTEGER(KIND=IDCDF_KIND) :: STATUS
INTEGER(KIND=IDCDF_KIND) :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)) :: YVARNAME
INTEGER(KIND=IDCDF_KIND) :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                  :: IRESP
LOGICAL                  :: GEXISTED !True if variable was already defined
!
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_X6',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL CLEANMNHNAME(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(PFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_X6','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL FILLVDIMS(TPFILE, TPFIELD, INT(SHAPE(PFIELD),KIND=IDCDF_KIND), IVDIMS)

   ! Define the variable
   IF (TPFILE%LNCREDUCE_FLOAT_PRECISION) THEN
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
   ELSE
#if (MNH_REAL == 8)
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_DOUBLE, IVDIMS, IVARID)
#else
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIMS, IVARID)
#endif
   END IF
   IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X6','NF90_DEF_VAR',trim(YVARNAME))

     ! Add compression if asked for
   IF (TPFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TPFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X6','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_X6',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TPFIELD,IVARID,GEXISTED,KSHAPE=INT(SHAPE(PFIELD),KIND=IDCDF_KIND))
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, PFIELD)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_X6','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_X6


SUBROUTINE IO_WRITE_FIELD_NC4_N0(TPFILE,TPFIELD,KFIELD,KRESP)
!
USE MODD_PARAMETERS_ll,  ONLY : JPVEXT
#if 0
USE MODD_PARAMETERS_ll,  ONLY : JPHEXT, JPVEXT
USE MODD_IO_ll, ONLY : LPACK,L1D,L2D
#endif
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
INTEGER,               INTENT(IN) :: KFIELD
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER(KIND=IDCDF_KIND) :: STATUS
INTEGER(KIND=IDCDF_KIND) :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)) :: YVARNAME
INTEGER(KIND=IDCDF_KIND) :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                  :: IRESP
LOGICAL                  :: GEXISTED !True if variable was already defined
TYPE(IOCDF), POINTER     :: TZIOCDF
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_N0',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL CLEANMNHNAME(TPFIELD%CMNHNAME,YVARNAME)
!
! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (TPFIELD%LTIMEDEP) THEN
     ! Get the netcdf dimensions
     CALL FILLVDIMS(TPFILE, TPFIELD, INT(SHAPE(KFIELD),KIND=IDCDF_KIND), IVDIMS)
     ! Define the variable
#if ( MNH_INT == 4 )
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT,   IVDIMS, IVARID)
#else
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT64, IVDIMS, IVARID)
#endif
     IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_N0','NF90_DEF_VAR',trim(YVARNAME))
     DEALLOCATE(IVDIMS)
   ELSE
     ! Define the scalar variable
#if ( MNH_INT == 4 )
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT,   IVARID)
#else
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT64, IVARID)
#endif
     IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_N0','NF90_DEF_VAR',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_N0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, KFIELD)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_N0','NF90_PUT_VAR',trim(YVARNAME),IRESP)

!
! Use IMAX, JMAX, KMAX to define DIM_NI, DIM_NJ, DIM_LEVEL
! /!\ Can only work if IMAX, JMAX or KMAX are written before any array
!
#if 0
IF (YVARNAME == 'IMAX' .AND. .NOT. ASSOCIATED(TPFILE%TNCDIMS%DIM_NI)) TPFILE%TNCDIMS%DIM_NI=>GETDIMCDF(TPFILE%TNCDIMS,KFIELD+2*JPHEXT,'X')
IF (YVARNAME == 'JMAX' .AND. .NOT. ASSOCIATED(TPFILE%TNCDIMS%DIM_NJ)) THEN
   IF (LPACK .AND. L2D) THEN
      TPFILE%TNCDIMS%DIM_NJ=>GETDIMCDF(TPFILE, 1,'Y')
   ELSE
      TPFILE%TNCDIMS%DIM_NJ=>GETDIMCDF(TPFILE, KFIELD+2*JPHEXT, 'Y')
   END IF
END IF
#endif
IF (YVARNAME == 'KMAX' .AND. .NOT. ASSOCIATED(TPFILE%TNCDIMS%DIM_LEVEL)) THEN
  TZIOCDF => TPFILE%TNCDIMS
  TZIOCDF%DIM_LEVEL=>GETDIMCDF(TPFILE,INT(KFIELD+2*JPVEXT,KIND=IDCDF_KIND),'Z')
END IF

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_N0


SUBROUTINE IO_WRITE_FIELD_NC4_N1(TPFILE,TPFIELD,KFIELD,KRESP)
!
USE MODD_PARAMETERS_ll,  ONLY : JPVEXT
#if 0
USE MODD_PARAMETERS_ll,  ONLY : JPHEXT, JPVEXT
USE MODD_IO_ll, ONLY : LPACK,L1D,L2D
#endif
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
INTEGER, DIMENSION(:), INTENT(IN) :: KFIELD
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER(KIND=IDCDF_KIND) :: STATUS
INTEGER(KIND=IDCDF_KIND) :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)) :: YVARNAME
INTEGER(KIND=IDCDF_KIND) :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                  :: IRESP
LOGICAL                  :: GEXISTED !True if variable was already defined
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_N1',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL CLEANMNHNAME(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(KFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_N1','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL FILLVDIMS(TPFILE, TPFIELD, INT(SHAPE(KFIELD),KIND=IDCDF_KIND), IVDIMS)

   ! Define the variable
#if ( MNH_INT == 4 )
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT, IVDIMS, IVARID)
#else
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT64, IVDIMS, IVARID)
#endif
     IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_N1','NF90_DEF_VAR',trim(YVARNAME))
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_N1',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, KFIELD)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_N1','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_N1


SUBROUTINE IO_WRITE_FIELD_NC4_N2(TPFILE,TPFIELD,KFIELD,KRESP)
!
TYPE(TFILEDATA),TARGET,INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
INTEGER,DIMENSION(:,:),INTENT(IN) :: KFIELD   ! array containing the data field
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER(KIND=IDCDF_KIND) :: STATUS
INTEGER(KIND=IDCDF_KIND) :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)) :: YVARNAME
INTEGER(KIND=IDCDF_KIND) :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                  :: IRESP
LOGICAL                  :: GEXISTED !True if variable was already defined
!
IRESP = 0
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_N2',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL CLEANMNHNAME(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(KFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_N2','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL FILLVDIMS(TPFILE, TPFIELD, INT(SHAPE(KFIELD),KIND=IDCDF_KIND), IVDIMS)

   ! Define the variable
#if ( MNH_INT == 4 )
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT, IVDIMS, IVARID)
#else
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT64, IVDIMS, IVARID)
#endif
   IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_N2','NF90_DEF_VAR',trim(YVARNAME))
   ! Add compression if asked for
   IF (TPFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TPFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_N2','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_N2',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TPFIELD,IVARID,GEXISTED,KSHAPE=INT(SHAPE(KFIELD),KIND=IDCDF_KIND))
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, KFIELD)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_N2','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_N2

SUBROUTINE IO_WRITE_FIELD_NC4_N3(TPFILE,TPFIELD,KFIELD,KRESP)
!
TYPE(TFILEDATA),TARGET,  INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),        INTENT(IN) :: TPFIELD
INTEGER,DIMENSION(:,:,:),INTENT(IN) :: KFIELD   ! array containing the data field
INTEGER,                 INTENT(OUT):: KRESP
!
INTEGER(KIND=IDCDF_KIND) :: STATUS
INTEGER(KIND=IDCDF_KIND) :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)) :: YVARNAME
INTEGER(KIND=IDCDF_KIND) :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(:), ALLOCATABLE :: IVDIMS
INTEGER                  :: IRESP
LOGICAL                  :: GEXISTED !True if variable was already defined
!
IRESP = 0
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_N3',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL CLEANMNHNAME(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(KFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_N3','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL FILLVDIMS(TPFILE, TPFIELD, INT(SHAPE(KFIELD),KIND=IDCDF_KIND), IVDIMS)

   ! Define the variable
#if ( MNH_INT == 4 )
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT, IVDIMS, IVARID)
#else
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT64, IVDIMS, IVARID)
#endif
   IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_N3','NF90_DEF_VAR',trim(YVARNAME))
   ! Add compression if asked for
   IF (TPFILE%LNCCOMPRESS) THEN
     STATUS = NF90_DEF_VAR_DEFLATE(INCID, IVARID, SHUFFLE, DEFLATE, TPFILE%NNCCOMPRESS_LEVEL)
     IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_N3','NF90_DEF_VAR_DEFLATE',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_N3',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TPFIELD,IVARID,GEXISTED,KSHAPE=INT(SHAPE(KFIELD),KIND=IDCDF_KIND))
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, KFIELD)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_N3','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_N3

SUBROUTINE IO_WRITE_FIELD_NC4_L0(TPFILE,TPFIELD,OFIELD,KRESP)
!
USE MODD_PARAMETERS_ll,  ONLY : JPVEXT
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
LOGICAL,               INTENT(IN) :: OFIELD
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER                  :: IFIELD
INTEGER(KIND=IDCDF_KIND) :: STATUS
INTEGER(KIND=IDCDF_KIND) :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)) :: YVARNAME
INTEGER(KIND=IDCDF_KIND) :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(:), ALLOCATABLE      :: IVDIMS
INTEGER                  :: IRESP
LOGICAL                  :: GEXISTED !True if variable was already defined
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_L0',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL CLEANMNHNAME(TPFIELD%CMNHNAME,YVARNAME)
!
! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (TPFIELD%LTIMEDEP) THEN
     ! Get the netcdf dimensions
     CALL FILLVDIMS(TPFILE, TPFIELD, INT(SHAPE(OFIELD),KIND=IDCDF_KIND), IVDIMS)
     ! Define the variable
     ! Use of NF90_INT1 datatype (=NF90_BYTE) that is enough to store a boolean
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT1, IVDIMS, IVARID)
     IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_L0','NF90_DEF_VAR',trim(YVARNAME))
     DEALLOCATE(IVDIMS)
   ELSE
     ! Define the scalar variable
     ! Use of NF90_INT1 datatype (=NF90_BYTE) that is enough to store a boolean
     STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT1, IVARID)
     IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_L0','NF90_DEF_VAR',trim(YVARNAME))
   END IF
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_L0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

!Convert LOGICAL to INTEGER (LOGICAL format not supported by netCDF files)
IF (OFIELD) THEN
  IFIELD = 1
ELSE
  IFIELD = 0
END IF

! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, IFIELD)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_L0','NF90_PUT_VAR',trim(YVARNAME),IRESP)

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_L0

SUBROUTINE IO_WRITE_FIELD_NC4_L1(TPFILE,TPFIELD,OFIELD,KRESP)
!
USE MODD_PARAMETERS_ll,  ONLY : JPVEXT
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
LOGICAL, DIMENSION(:), INTENT(IN) :: OFIELD
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER, DIMENSION(SIZE(OFIELD))                         :: IFIELD
INTEGER(KIND=IDCDF_KIND)                                 :: STATUS
INTEGER(KIND=IDCDF_KIND)                                 :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))                     :: YVARNAME
INTEGER(KIND=IDCDF_KIND)                                 :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(:), ALLOCATABLE      :: IVDIMS
INTEGER                                                  :: IRESP
LOGICAL                                                  :: GEXISTED !True if variable was already defined
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_L1',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL CLEANMNHNAME(TPFIELD%CMNHNAME,YVARNAME)

! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   IF (SIZE(OFIELD)==0) THEN
     CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_L1','ignoring variable with a zero size ('//TRIM(YVARNAME)//')')
     KRESP = 0
     RETURN
   END IF

   ! Get the netcdf dimensions
   CALL FILLVDIMS(TPFILE, TPFIELD, INT(SHAPE(OFIELD),KIND=IDCDF_KIND), IVDIMS)

   ! Define the variable
   ! Use of NF90_INT1 datatype (=NF90_BYTE) that is enough to store a boolean
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_INT1, IVDIMS, IVARID)
     IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_L1','NF90_DEF_VAR',trim(YVARNAME))
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_L1',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

!Convert LOGICAL to INTEGER (LOGICAL format not supported by netCDF files)
WHERE (OFIELD)
  IFIELD = 1
ELSEWHERE
  IFIELD = 0
END WHERE

! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, IFIELD)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_L1','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMS)) DEALLOCATE(IVDIMS)

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_L1


SUBROUTINE IO_WRITE_FIELD_NC4_C0(TPFILE,TPFIELD,HFIELD,KRESP)
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
TYPE(TFIELDDATA),      INTENT(IN) :: TPFIELD
CHARACTER(LEN=*),      INTENT(IN) :: HFIELD
INTEGER,               INTENT(OUT):: KRESP
!
INTEGER(KIND=IDCDF_KIND)               :: STATUS
INTEGER(KIND=IDCDF_KIND)               :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))   :: YVARNAME
INTEGER(KIND=IDCDF_KIND)               :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(1) :: IVDIMS
INTEGER                                :: IRESP, ILEN
CHARACTER(LEN=:),ALLOCATABLE           :: YFIELD
LOGICAL                                :: GEXISTED !True if variable was already defined
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_C0',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
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
CALL CLEANMNHNAME(TPFIELD%CMNHNAME,YVARNAME)
!
IF (TPFIELD%LTIMEDEP) &
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_C0',TRIM(TPFILE%CNAME)// &
                 ': time dependent variable not (yet) possible for 0D variable '//TRIM(TPFIELD%CMNHNAME))
!
! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   ! Get the netcdf string dimensions id
   IVDIMS(1) = GETSTRDIMID(TPFILE,INT(ILEN,KIND=IDCDF_KIND))
   ! Define the variable
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_CHAR, IVDIMS, IVARID)
     IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_C0','NF90_DEF_VAR',trim(YVARNAME))
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_C0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

ALLOCATE(CHARACTER(LEN=ILEN)::YFIELD)
YFIELD(1:LEN_TRIM(HFIELD))=TRIM(HFIELD)
YFIELD(LEN_TRIM(HFIELD)+1:)=' '
! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, YFIELD)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_C0','NF90_PUT_VAR',trim(YVARNAME),IRESP)
DEALLOCATE(YFIELD)

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_C0

SUBROUTINE IO_WRITE_FIELD_NC4_C1(TPFILE,TPFIELD,HFIELD,KRESP)
!  Modif
!    J.Escobar : 25/04/2018 : missing 'IF ALLOCATED(IVDIMSTMP)' DEALLOCATE
!----------------------------------------------------------------
TYPE(TFILEDATA),              INTENT(IN)  :: TPFILE
TYPE(TFIELDDATA),             INTENT(IN)  :: TPFIELD
CHARACTER(LEN=*),DIMENSION(:),INTENT(IN)  :: HFIELD
INTEGER,                      INTENT(OUT) :: KRESP
!
INTEGER(KIND=IDCDF_KIND),PARAMETER :: IONE = 1
!
INTEGER(KIND=IDCDF_KIND)               :: STATUS
INTEGER(KIND=IDCDF_KIND)               :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))   :: YVARNAME
INTEGER(KIND=IDCDF_KIND)               :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(2) :: IVDIMS
INTEGER(KIND=IDCDF_KIND), DIMENSION(:), ALLOCATABLE :: IVDIMSTMP
INTEGER(KIND=IDCDF_KIND)               :: ILEN, ISIZE
INTEGER                                :: IRESP
LOGICAL                                :: GEXISTED !True if variable was already defined
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_C1',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0

ILEN  = LEN(HFIELD)
ISIZE = SIZE(HFIELD)

! Get the Netcdf file ID
INCID = TPFILE%NNCID
!
GEXISTED = .FALSE.
!
CALL CLEANMNHNAME(TPFIELD%CMNHNAME,YVARNAME)
!
IF (TPFIELD%LTIMEDEP) THEN
  !This is an error (+return) and not a warning because IVDIMSTMP could be of size 2 if LTIMEDEP=T
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_WRITE_FIELD_NC4_C1',TRIM(TPFILE%CNAME)// &
                 ': time dependent variable not (yet) possible for '//TRIM(TPFIELD%CMNHNAME))
  RETURN
END IF
!
! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   ! Get the netcdf string dimensions id
   IVDIMS(1) = GETSTRDIMID(TPFILE,ILEN)
   CALL FILLVDIMS(TPFILE, TPFIELD, (/ISIZE/), IVDIMSTMP)
   IVDIMS(2) = IVDIMSTMP(1)
   ! Define the variable
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_CHAR, IVDIMS, IVARID)
     IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_C1','NF90_DEF_VAR',trim(YVARNAME))
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_C1',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TPFIELD,IVARID,GEXISTED)
! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, HFIELD(1:ISIZE)(1:ILEN), START=(/IONE,IONE/), COUNT=(/ILEN,ISIZE/))
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_C1','NF90_PUT_VAR',trim(YVARNAME),IRESP)

IF(ALLOCATED(IVDIMSTMP)) DEALLOCATE(IVDIMSTMP)

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_C1


SUBROUTINE IO_WRITE_FIELD_NC4_T0(TPFILE,TPFIELD,TPDATA,KRESP)
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
INTEGER(KIND=IDCDF_KIND)               :: STATUS
INTEGER(KIND=IDCDF_KIND)               :: INCID
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME))   :: YVARNAME
INTEGER(KIND=IDCDF_KIND)               :: IVARID
INTEGER(KIND=IDCDF_KIND), DIMENSION(1) :: IVDIMS
INTEGER                                :: IRESP
TYPE(TFIELDDATA)                       :: TZFIELD
CHARACTER(LEN=40)                      :: YUNITS
LOGICAL                                :: GEXISTED !True if variable was already defined
REAL                                   :: ZDELTATIME !Distance in seconds since reference date and time
TYPE(DATE_TIME)                        :: TZREF
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_FIELD_NC4_T0',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
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
CALL CLEANMNHNAME(TPFIELD%CMNHNAME,YVARNAME)
!
TZFIELD%CMNHNAME = TRIM(YVARNAME)
!
! Model beginning date (TDTMOD%TDATE) is used as the reference date
! Reference time is set to 0.
IF (.NOT.ASSOCIATED(TDTMOD)) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_T0',TRIM(TPFILE%CNAME)// &
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
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_T0',TRIM(TPFILE%CNAME)// &
                 ': time dependent variable not (yet) possible for 0D variable '//TRIM(TPFIELD%CMNHNAME))
!
! The variable should not already exist but who knows ?
STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (STATUS /= NF90_NOERR) THEN
   ! Define the scalar variable
#if (MNH_REAL == 8)
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_DOUBLE, IVARID)
#else
   STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVARID)
#endif
     IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_T0','NF90_DEF_VAR',trim(YVARNAME))
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_T0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' already defined')
END IF

! Write metadata
CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TZFIELD,IVARID,GEXISTED,HCALENDAR='standard')
!
! Compute the temporal distance from reference
CALL DATETIME_DISTANCE(TZREF,TPDATA,ZDELTATIME)

! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, ZDELTATIME)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_T0','NF90_PUT_VAR',trim(YVARNAME),IRESP)

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
   CALL FILLVDIMS(TPFILE, TPFIELD, INT(SHAPE(ITDATE),KIND=IDCDF_KIND), IVDIMS)

   ! Define the variable
   STATUS = NF90_DEF_VAR(INCID, TZFIELD%CMNHNAME, NF90_INT, IVDIMS, IVARID)
     IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_T0','NF90_DEF_VAR',trim(TZFIELD%CMNHNAME))
   CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TZFIELD,IVARID,GEXISTED)
ELSE
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_T0',TRIM(TPFILE%CNAME)//': '//TRIM(TZFIELD%CMNHNAME)//' already defined')
END IF

! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, ITDATE)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_T0','NF90_PUT_VAR',trim(TZFIELD%CMNHNAME),IRESP)

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
#if (MNH_REAL == 8)
   STATUS = NF90_DEF_VAR(INCID, TZFIELD%CMNHNAME, NF90_DOUBLE, IVARID)
#else
   STATUS = NF90_DEF_VAR(INCID, TZFIELD%CMNHNAME, NF90_FLOAT,  IVARID)
#endif
   IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_T0','NF90_DEF_VAR',trim(TZFIELD%CMNHNAME))
   CALL IO_WRITE_FIELD_ATTR_NC4(TPFILE,TZFIELD,IVARID,GEXISTED)
ELSE
   GEXISTED = .TRUE.
   CALL PRINT_MSG(NVERB_WARNING,'IO','IO_WRITE_FIELD_NC4_T0',TRIM(TPFILE%CNAME)//': '//TRIM(TZFIELD%CMNHNAME)//' already defined')
END IF

! Write the data
STATUS = NF90_PUT_VAR(INCID, IVARID, TPDATA%TIME)
IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'IO_WRITE_FIELD_NC4_T0','NF90_PUT_VAR',trim(TZFIELD%CMNHNAME),IRESP)
#endif

KRESP = IRESP
END SUBROUTINE IO_WRITE_FIELD_NC4_T0

SUBROUTINE IO_WRITE_COORDVAR_NC4(TPFILE,HPROGRAM_ORIG)
USE MODD_CONF,       ONLY: CPROGRAM, LCARTESIAN
USE MODD_CONF_n,     ONLY: CSTORAGE_TYPE
USE MODD_GRID,       ONLY: XLATORI, XLONORI
USE MODD_GRID_n,     ONLY: LSLEVE, XXHAT, XYHAT, XZHAT
use modd_netcdf,     only: dimcdf
USE MODD_PARAMETERS, ONLY: JPHEXT, JPVEXT

USE MODE_FIELD,      ONLY: TFIELDLIST,FIND_FIELD_ID_FROM_MNHNAME
USE MODE_GRIDPROJ
USE MODE_NEST_ll,    ONLY: GET_MODEL_NUMBER_ll, GO_TOMODEL_ll

TYPE(TFILEDATA),          INTENT(IN) :: TPFILE
CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: HPROGRAM_ORIG !To emulate a file coming from this program

CHARACTER(LEN=:),ALLOCATABLE    :: YSTDNAMEPREFIX
CHARACTER(LEN=:),ALLOCATABLE    :: YPROGRAM
INTEGER                         :: IIU, IJU, IKU
INTEGER                         :: ID, IID, IRESP
INTEGER                         :: IMI
INTEGER(KIND=IDCDF_KIND)        :: INCID
LOGICAL                         :: GCHANGEMODEL
LOGICAL,POINTER                 :: GSLEVE
REAL,DIMENSION(:),POINTER       :: ZXHAT, ZYHAT, ZZHAT
REAL,DIMENSION(:),ALLOCATABLE   :: ZXHATM, ZYHATM,ZZHATM !Coordinates at mass points in the transformed space
REAL,DIMENSION(:,:),POINTER     :: ZLAT, ZLON
type(dimcdf), pointer           :: tzdim_ni, tzdim_nj, tzdim_ni_u, tzdim_nj_u, tzdim_ni_v, tzdim_nj_v
TYPE(IOCDF),  POINTER           :: PIOCDF

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_COORDVAR_NC4','called for '//TRIM(TPFILE%CNAME))

ZXHAT => NULL()
ZYHAT => NULL()
ZZHAT => NULL()

PIOCDF => TPFILE%TNCDIMS

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

if(associated(piocdf)) then
tzdim_ni   => piocdf%dim_ni
tzdim_nj   => piocdf%dim_nj
tzdim_ni_u => piocdf%dim_ni_u
tzdim_nj_u => piocdf%dim_nj_u
tzdim_ni_v => piocdf%dim_ni_v
tzdim_nj_v => piocdf%dim_nj_v
else
tzdim_ni   => null()
tzdim_nj   => null()
tzdim_ni_u => null()
tzdim_nj_u => null()
tzdim_ni_v => null()
tzdim_nj_v => null()
end if

CALL WRITE_HOR_COORD(tzdim_ni,'x-dimension of the grid',TRIM(YSTDNAMEPREFIX)//'_x_coordinate','X',0.,JPHEXT,JPHEXT,ZXHATM)
CALL WRITE_HOR_COORD(tzdim_nj,'y-dimension of the grid',TRIM(YSTDNAMEPREFIX)//'_y_coordinate','Y',0.,JPHEXT,JPHEXT,ZYHATM)
CALL WRITE_HOR_COORD(tzdim_ni_u,'x-dimension of the grid at u location', &
                     TRIM(YSTDNAMEPREFIX)//'_x_coordinate_at_u_location','X',-0.5,JPHEXT,0,     ZXHAT)
CALL WRITE_HOR_COORD(tzdim_nj_u,'y-dimension of the grid at u location', &
                     TRIM(YSTDNAMEPREFIX)//'_y_coordinate_at_u_location','Y', 0., JPHEXT,JPHEXT,ZYHATM)
CALL WRITE_HOR_COORD(tzdim_ni_v,'x-dimension of the grid at v location', &
                     TRIM(YSTDNAMEPREFIX)//'_x_coordinate_at_v_location','X', 0., JPHEXT,JPHEXT,ZXHATM)
CALL WRITE_HOR_COORD(tzdim_nj_v,'y-dimension of the grid at v location', &
                     TRIM(YSTDNAMEPREFIX)//'_y_coordinate_at_v_location','Y',-0.5,JPHEXT,0,     ZYHAT)

IF (.NOT.LCARTESIAN) THEN
  ALLOCATE(ZLAT(IIU,IJU),ZLON(IIU,IJU))
  !
  !Compute latitude/longitude for the Arakawa points
  !
  ! Mass point
  CALL WRITE_HOR_2DCOORD(ZXHATM,ZYHATM,'latitude',  'longitude')
  ! u point
  CALL WRITE_HOR_2DCOORD(ZXHAT, ZYHATM,'latitude_u','longitude_u')
  ! v point
  CALL WRITE_HOR_2DCOORD(ZXHATM,ZYHAT, 'latitude_v','longitude_v')
  ! xi vorticity point (=f point =uv point)
  CALL WRITE_HOR_2DCOORD(ZXHAT, ZYHAT, 'latitude_f','longitude_f')
  !
  DEALLOCATE(ZLAT,ZLON)
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
    CALL WRITE_VER_COORD(PIOCDF%DIM_LEVEL,  'position z in the transformed space',              '', &
                         'altitude',               0., JPVEXT,JPVEXT,ZZHATM)
    !
    CALL WRITE_VER_COORD(PIOCDF%DIM_LEVEL_W,'position z in the transformed space at w location','', &
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
    CALL WRITE_TIME_COORD(PIOCDF%DIMTIME)
  END IF
END IF

IF (GCHANGEMODEL) CALL GO_TOMODEL_ll(IMI,IRESP)

CONTAINS
SUBROUTINE WRITE_HOR_COORD(TDIM,HLONGNAME,HSTDNAME,HAXIS,PSHIFT,KBOUNDLOW,KBOUNDHIGH,PCOORDS)
  USE MODE_ALLOCBUFFER_ll, ONLY: ALLOCBUFFER_ll
  USE MODE_GATHER_ll,      ONLY: GATHER_XXFIELD

  TYPE(DIMCDF), POINTER,            INTENT(IN) :: TDIM
  CHARACTER(LEN=*),                 INTENT(IN) :: HLONGNAME
  CHARACTER(LEN=*),                 INTENT(IN) :: HSTDNAME
  CHARACTER(LEN=*),                 INTENT(IN) :: HAXIS
  REAL,                             INTENT(IN) :: PSHIFT
  INTEGER,                          INTENT(IN) :: KBOUNDLOW
  INTEGER,                          INTENT(IN) :: KBOUNDHIGH
  REAL,DIMENSION(:),TARGET,OPTIONAL,INTENT(IN) :: PCOORDS

  CHARACTER(LEN=2)              :: YDIR
  CHARACTER(LEN=64)             :: YRANGE
  CHARACTER(LEN=:),ALLOCATABLE  :: YVARNAME
  INTEGER                       :: IRESP
  INTEGER                       :: ISIZE
  INTEGER                       :: JI
  INTEGER(KIND=IDCDF_KIND)      :: IVARID
  INTEGER(KIND=IDCDF_KIND)      :: IVDIM
  INTEGER(KIND=IDCDF_KIND)      :: STATUS
  LOGICAL                       :: GALLOC
  REAL,DIMENSION(:),POINTER     :: ZTAB

  GALLOC = .FALSE.
  ZTAB => NULL()

  IF (HAXIS=='X') THEN
    YDIR = 'XX'
  ELSE IF (HAXIS=='Y') THEN
    YDIR = 'YY'
  ELSE
    CALL PRINT_MSG(NVERB_FATAL,'IO','WRITE_HOR_COORD','invalid HAXIS ('//TRIM(HAXIS)//')')
  END IF

  IF (.NOT.TPFILE%LMASTER) THEN
    IF (PRESENT(PCOORDS)) THEN
      ALLOCATE(ZTAB(0)) !To prevent false positive with valgrind
      GALLOC = .TRUE.
      CALL GATHER_XXFIELD(YDIR,PCOORDS,ZTAB,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
    END IF
  ELSE !TPFILE%LMASTER
    ISIZE = TDIM%LEN
    YVARNAME = TRIM(TDIM%NAME)
    IVDIM = TDIM%ID

    IF (.NOT.PRESENT(PCOORDS)) THEN
      ALLOCATE(ZTAB(ISIZE))
      GALLOC = .TRUE.
      DO JI=1,ISIZE
        ZTAB(JI) = REAL(JI,KIND=KIND(ZTAB(1)))+PSHIFT
      END DO
    ELSE
      IF (GSMONOPROC) THEN ! sequential execution
        ZTAB => PCOORDS
      ELSE ! multiprocesses execution
        CALL ALLOCBUFFER_ll(ZTAB,PCOORDS,YDIR,GALLOC)
        CALL GATHER_XXFIELD(YDIR,PCOORDS,ZTAB,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
      ENDIF
    END IF

    STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
    IF (STATUS /= NF90_NOERR) THEN
      ! Define the coordinate variable
#if (MNH_REAL == 8)
      STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_DOUBLE, IVDIM, IVARID)
#else
      STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIM, IVARID)
#endif
      IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_HOR_COORD','NF90_DEF_VAR',trim(YVARNAME))
    ELSE
      CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_HOR_COORD',TRIM(YVARNAME)//' already defined')
    END IF

    ! Write metadata
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'long_name',HLONGNAME)
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_HOR_COORD','NF90_PUT_ATT','long_name for '//trim(YVARNAME))
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'standard_name',HSTDNAME)
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_HOR_COORD','NF90_PUT_ATT','standard_name for '//trim(YVARNAME))
    IF (PRESENT(PCOORDS)) THEN
      STATUS = NF90_PUT_ATT(INCID, IVARID, 'units','m')
      IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_HOR_COORD','NF90_PUT_ATT','units for '//trim(YVARNAME))
    END IF
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'axis',HAXIS)
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_HOR_COORD','NF90_PUT_ATT','axis for '//trim(YVARNAME))
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'c_grid_axis_shift',PSHIFT)
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_HOR_COORD','NF90_PUT_ATT','c_grid_axis_shift for ' &
                                                     //trim(YVARNAME))
    WRITE(YRANGE,'( I0,":",I0 )') 1+KBOUNDLOW,ISIZE-KBOUNDHIGH
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'c_grid_dynamic_range',TRIM(YRANGE))
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_HOR_COORD','NF90_PUT_ATT','c_grid_dynamic_range for ' &
                                                     //trim(YVARNAME))

    ! Write the data
    STATUS = NF90_PUT_VAR(INCID, IVARID, ZTAB)
    IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_HOR_COORD','NF90_PUT_VAR',trim(YVARNAME),IRESP)
  END IF

  IF (GALLOC) DEALLOCATE(ZTAB)
END SUBROUTINE WRITE_HOR_COORD

SUBROUTINE WRITE_HOR_2DCOORD(PX,PY,HLAT,HLON)
  USE MODE_ALLOCBUFFER_ll, ONLY: ALLOCBUFFER_ll
  USE MODE_GATHER_ll,      ONLY: GATHER_XYFIELD

  REAL,DIMENSION(:), INTENT(IN) :: PX
  REAL,DIMENSION(:), INTENT(IN) :: PY
  CHARACTER(LEN=*),  INTENT(IN) :: HLAT
  CHARACTER(LEN=*),  INTENT(IN) :: HLON

  LOGICAL                       :: GALLOC1, GALLOC2
  REAL,DIMENSION(:,:),POINTER   :: ZTAB1, ZTAB2

  GALLOC1 = .FALSE.
  GALLOC2 = .FALSE.
  ZTAB1 => NULL()
  ZTAB2 => NULL()

  CALL SM_LATLON(XLATORI,XLONORI,                     &
                 SPREAD(SOURCE=PX,DIM=2,NCOPIES=IJU), &
                 SPREAD(SOURCE=PY,DIM=1,NCOPIES=IIU), &
                 ZLAT,ZLON)

  IF (.NOT.TPFILE%LMASTER) THEN
    ALLOCATE(ZTAB1(0,0),ZTAB2(0,0)) !To prevent false positive with valgrind
    GALLOC1 = .TRUE. ; GALLOC2 = .TRUE.
    CALL GATHER_XYFIELD(ZLAT,ZTAB1,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
    CALL GATHER_XYFIELD(ZLON,ZTAB2,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
  ELSE !TPFILE%LMASTER
    IF (GSMONOPROC) THEN ! sequential execution
      ZTAB1 => ZLAT
      ZTAB2 => ZLON
    ELSE ! multiprocesses execution
      CALL ALLOCBUFFER_ll(ZTAB1,ZLAT,'XY',GALLOC1)
      CALL ALLOCBUFFER_ll(ZTAB2,ZLON,'XY',GALLOC2)
      CALL GATHER_XYFIELD(ZLAT,ZTAB1,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
      CALL GATHER_XYFIELD(ZLON,ZTAB2,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
    ENDIF
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HLAT,ID,IRESP)
    CALL IO_WRITE_FIELD_NC4_X2(TPFILE,TFIELDLIST(ID),ZTAB1,IRESP,OISCOORD=.TRUE.)
    CALL FIND_FIELD_ID_FROM_MNHNAME(HLON,ID,IRESP)
    CALL IO_WRITE_FIELD_NC4_X2(TPFILE,TFIELDLIST(ID),ZTAB2,IRESP,OISCOORD=.TRUE.)
  END IF

  IF (GALLOC1) DEALLOCATE(ZTAB1)
  IF (GALLOC2) DEALLOCATE(ZTAB2)
END SUBROUTINE WRITE_HOR_2DCOORD

SUBROUTINE WRITE_VER_COORD(TDIM,HLONGNAME,HSTDNAME,HCOMPNAME,PSHIFT,KBOUNDLOW,KBOUNDHIGH,PCOORDS)
  TYPE(DIMCDF), POINTER, INTENT(IN) :: TDIM
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
  INTEGER(KIND=IDCDF_KIND)      :: IVARID
  INTEGER(KIND=IDCDF_KIND)      :: IVDIM
  INTEGER(KIND=IDCDF_KIND)      :: STATUS

  ISIZE = TDIM%LEN
  YVARNAME = TRIM(TDIM%NAME)
  IVDIM = TDIM%ID

  STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
  IF (STATUS /= NF90_NOERR) THEN
    ! Define the coordinate variable
#if (MNH_REAL == 8)
    STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_DOUBLE, IVDIM, IVARID)
#else
    STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIM, IVARID)
#endif
    IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_VER_COORD','NF90_DEF_VAR',trim(YVARNAME))
  ELSE
    CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_VER_COORD',TRIM(YVARNAME)//' already defined')
  END IF

  ! Write metadata
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'long_name',HLONGNAME)
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_VER_COORD','NF90_PUT_ATT','long_name for '//trim(YVARNAME))
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'standard_name',HSTDNAME)
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_VER_COORD','NF90_PUT_ATT','standard_name for '//trim(YVARNAME))
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'units','m')
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_VER_COORD','NF90_PUT_ATT','units for '//trim(YVARNAME))
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'axis','Z')
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_VER_COORD','NF90_PUT_ATT','axis for '//trim(YVARNAME))
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'positive','up')
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_VER_COORD','NF90_PUT_ATT','positive for '//trim(YVARNAME))
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'c_grid_axis_shift',PSHIFT)
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_VER_COORD','NF90_PUT_ATT','c_grid_axis_shift for ' &
                                                   //trim(YVARNAME))
  WRITE(YRANGE,'( I0,":",I0 )') 1+KBOUNDLOW,ISIZE-KBOUNDHIGH
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'c_grid_dynamic_range',TRIM(YRANGE))
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_VER_COORD','NF90_PUT_ATT','c_grid_dynamic_range for ' &
                                                   //trim(YVARNAME))
  !
  IF (GSLEVE) THEN
    !Remark: ZS, ZSMT and ZTOP in the formula are the same for mass point or flux point
    STATUS = NF90_PUT_ATT(INCID, IVARID,'formula_terms','s: '//TRIM(YVARNAME)//                   &
                                        ' height: ZTOP oro_ls: ZSMT oro: ZS len1: LEN1 len2: LEN2')
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_VER_COORD','NF90_PUT_ATT','formula_terms for '//trim(YVARNAME))
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'formula_definition','z(n,k,j,i)=s(k)'//                                      &
                          '+ oro_ls(j,i)*sinh((height/len1)**1.35-(s(k)/len1)**1.35)/sinh((s(k)/len1)**1.35)'//        &
                          '+(oro(j,i)-oro_ls(j,i))*sinh((height/len2)**1.35-(s(k)/len2)**1.35)/sinh((s(k)/len2)**1.35)')
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_VER_COORD','NF90_PUT_ATT','formula_definition for ' &
                                                     //trim(YVARNAME))
  ELSE
    !Remark: ZS and ZTOP in the formula are the same for mass point or flux point
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'formula_terms','s: '//TRIM(YVARNAME)//' height: ZTOP orog: ZS')
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_VER_COORD','NF90_PUT_ATT','formula_terms for '//trim(YVARNAME))
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'formula_definition','z(n,k,j,i)=s(k)*(height-orog(j,i))/height+orog(j,i)')
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_VER_COORD','NF90_PUT_ATT','formula_definition for ' &
                                                     //trim(YVARNAME))
  ENDIF
  !
  STATUS = NF90_PUT_ATT(INCID, IVARID, 'computed_standard_name',HCOMPNAME)
  IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_VER_COORD','NF90_PUT_ATT','computed_standard_name for ' &
                                                   //trim(YVARNAME))

  ! Write the data
  STATUS = NF90_PUT_VAR(INCID, IVARID, PCOORDS)
  IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_VER_COORD','NF90_PUT_VAR',trim(YVARNAME))

END SUBROUTINE WRITE_VER_COORD

SUBROUTINE WRITE_TIME_COORD(TDIM)
  USE MODD_TIME_n,     ONLY: TDTMOD, TDTCUR
  USE MODD_TYPE_DATE

  USE MODE_DATETIME
  USE MODE_FIELD,      ONLY: TFIELDLIST,FIND_FIELD_ID_FROM_MNHNAME
  USE MODE_GRIDPROJ

  TYPE(DIMCDF), POINTER, INTENT(IN) :: TDIM

  REAL                         :: ZDELTATIME
  CHARACTER(LEN=40)            :: YUNITS
  CHARACTER(LEN=:),ALLOCATABLE :: YVARNAME
  INTEGER(KIND=IDCDF_KIND)     :: IVARID
  INTEGER(KIND=IDCDF_KIND)     :: IVDIM
  INTEGER(KIND=IDCDF_KIND)     :: STATUS
  TYPE(DATE_TIME)              :: TZREF


  IF (ASSOCIATED(TDTCUR) .AND. ASSOCIATED(TDTMOD)) THEN
    YVARNAME = TRIM(TDIM%NAME)
    IVDIM = TDIM%ID

    STATUS = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
    IF (STATUS /= NF90_NOERR) THEN
      ! Define the coordinate variable
#if (MNH_REAL == 8)
      STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_DOUBLE, IVDIM, IVARID)
#else
      STATUS = NF90_DEF_VAR(INCID, YVARNAME, NF90_FLOAT,  IVDIM, IVARID)
#endif
      IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_TIME_COORD','NF90_DEF_VAR',trim(YVARNAME))
    ELSE
      CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_TIME_COORD',TRIM(YVARNAME)//' already defined')
    END IF

    ! Write metadata
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'long_name','time axis')
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_TIME_COORD','NF90_PUT_ATT','long_name for '//trim(YVARNAME))
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'standard_name','time')
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_TIME_COORD','NF90_PUT_ATT','standard_name for '//trim(YVARNAME))
    WRITE(YUNITS,'( "seconds since ",I4.4,"-",I2.2,"-",I2.2," 00:00:00 +0:00" )') &
          TDTMOD%TDATE%YEAR,TDTMOD%TDATE%MONTH,TDTMOD%TDATE%DAY
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'units',YUNITS)
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_TIME_COORD','NF90_PUT_ATT','units for '//trim(YVARNAME))
    STATUS = NF90_PUT_ATT(INCID, IVARID, 'axis','T')
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_TIME_COORD','NF90_PUT_ATT','axis for '//trim(YVARNAME))
    STATUS = NF90_PUT_ATT(INCID, IVARID,'calendar','standard')
    IF (STATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_TIME_COORD','NF90_PUT_ATT','calendar for '//trim(YVARNAME))

    ! Model beginning date (TDTMOD%TDATE) is used as the reference date
    ! Reference time is set to 0.
    TZREF = TDTMOD
    TZREF%TIME = 0.
    ! Compute the temporal distance from reference
    CALL DATETIME_DISTANCE(TZREF,TDTCUR,ZDELTATIME)
    ! Write the data
    STATUS = NF90_PUT_VAR(INCID, IVARID, ZDELTATIME)
    IF (status /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(status,'WRITE_TIME_COORD','NF90_PUT_VAR',trim(YVARNAME))
  END IF

END SUBROUTINE WRITE_TIME_COORD

END SUBROUTINE IO_WRITE_COORDVAR_NC4


SUBROUTINE IO_WRITE_HEADER_NC4(TPFILE)
!
TYPE(TFILEDATA), INTENT(IN)  :: TPFILE ! File structure
!
INTEGER(KIND=IDCDF_KIND)     :: ISTATUS
!
IF (TRIM(TPFILE%CFORMAT)/='NETCDF4' .AND. TRIM(TPFILE%CFORMAT)/='LFICDF4') RETURN
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_WRITE_HEADER_NC4','called for file '//TRIM(TPFILE%CNAME))
!
IF (TPFILE%LMASTER)  THEN
  ISTATUS = NF90_PUT_ATT(TPFILE%NNCID, NF90_GLOBAL, 'Conventions', 'CF-1.7 COMODO-1.4')
  IF (ISTATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(istatus,'IO_FILE_WRITE_HEADER','NF90_PUT_ATT','Conventions')

#if (MNH_REAL == 8)
  ISTATUS = NF90_PUT_ATT(TPFILE%NNCID, NF90_GLOBAL, 'MNH_REAL', '8')
#else
  ISTATUS = NF90_PUT_ATT(TPFILE%NNCID, NF90_GLOBAL, 'MNH_REAL', '4')
#endif
  IF (ISTATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(istatus,'IO_FILE_WRITE_HEADER','NF90_PUT_ATT','MNH_REAL')

#if (MNH_INT == 4)
  ISTATUS = NF90_PUT_ATT(TPFILE%NNCID, NF90_GLOBAL, 'MNH_INT', '4')
#else
  ISTATUS = NF90_PUT_ATT(TPFILE%NNCID, NF90_GLOBAL, 'MNH_INT', '8')
#endif
  IF (ISTATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(istatus,'IO_FILE_WRITE_HEADER','NF90_PUT_ATT','MNH_INT')

!title

  !history
  CALL IO_APPEND_HISTORY_NC4(TPFILE)

!institution

!source

!comment

!references
END IF
!
END SUBROUTINE IO_WRITE_HEADER_NC4


SUBROUTINE IO_APPEND_HISTORY_NC4(TPFILE)
!
USE MODD_IO_ll, ONLY: TFILEDATA
!
TYPE(TFILEDATA), INTENT(IN)  :: TPFILE ! File structure
!
INTEGER,PARAMETER :: YEAR=1, MONTH=2, DAY=3, HH=5, MM=6, SS=7
!
CHARACTER(len=5)             :: YZONE
CHARACTER(LEN=:),ALLOCATABLE :: YCMD, YHISTORY, YHISTORY_NEW, YHISTORY_PREV
INTEGER                      :: ILEN_CMD, ILEN_PREV
INTEGER(KIND=IDCDF_KIND)     :: ILEN_NC
INTEGER(KIND=IDCDF_KIND)     :: ISTATUS
INTEGER,DIMENSION(8)         :: IDATETIME
!
IF (TRIM(TPFILE%CFORMAT)/='NETCDF4' .AND. TRIM(TPFILE%CFORMAT)/='LFICDF4') RETURN
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_APPEND_HISTORY_NC4','called for file '//TRIM(TPFILE%CNAME))
!
IF (TPFILE%LMASTER)  THEN
  !Check if history attribute already exists in file and read it
  ISTATUS = NF90_INQUIRE_ATTRIBUTE(TPFILE%NNCID, NF90_GLOBAL, 'history', LEN=ILEN_NC)
  ILEN_PREV = int( ILEN_NC, kind=kind(ILEN_PREV) )
  IF (ISTATUS == NF90_NOERR) THEN
    ALLOCATE(CHARACTER(LEN=ILEN_PREV) :: YHISTORY_PREV)
    ISTATUS = NF90_GET_ATT(TPFILE%NNCID, NF90_GLOBAL, 'history', YHISTORY_PREV)
    IF (ISTATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(ISTATUS,'IO_APPEND_HISTORY_NC4','NF90_GET_ATT','history')
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
  IF (ISTATUS /= NF90_NOERR) CALL IO_HANDLE_ERR_NC4(istatus,'IO_APPEND_HISTORY_NC4','NF90_PUT_ATT','history')
END IF

END SUBROUTINE IO_APPEND_HISTORY_NC4


end module mode_io_write_nc4
#else
!
! External dummy subroutines
!
subroutine io_write_coordvar_nc4(a, b)
use mode_msg
integer :: a, b
CALL PRINT_MSG(NVERB_ERROR,'IO','io_write_coordvar_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine io_write_coordvar_nc4
!
subroutine io_write_field_nc4(a, b, c, d, e, f, g)
use mode_msg
integer :: a, b, c, d, e, f, g
CALL PRINT_MSG(NVERB_ERROR,'IO','io_write_field_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine io_write_field_nc4
!
subroutine io_write_header_nc4(a)
use mode_msg
integer :: a
CALL PRINT_MSG(NVERB_ERROR,'IO','io_write_header_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine io_write_header_nc4
!
#endif
