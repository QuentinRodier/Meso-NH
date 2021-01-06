!MNH_LIC Copyright 1994-2021 CNRS, Meteo-France and Universite Paul Sabatier
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
!  P. Wautelet 21/02/2019: bugfix: intent of read fields: OUT->INOUT to keep initial value if not found in file
!  P. Wautelet 05/03/2019: rename IO subroutines and modules
!  P. Wautelet 25/06/2019: added IO_Field_read for 3D integer arrays (IO_Field_read_nc4_N3)
!  P. Wautelet 18/09/2019: correct support of 64bit integers (MNH_INT=8)
!-----------------------------------------------------------------
#ifdef MNH_IOCDF4
module mode_io_read_nc4

use modd_field,        only: tfielddata
use modd_io,           only: tfiledata
use modd_precision,    only: CDFINT

use mode_io_tools_nc4, only: IO_Mnhname_clean, IO_Err_handle_nc4
use mode_msg

use NETCDF,            only: NF90_CHAR, NF90_DOUBLE, NF90_FLOAT, NF90_INT, NF90_INT1, NF90_INT64,  &
                             NF90_MAX_VAR_DIMS, NF90_NOERR,                                        &
                             NF90_GET_ATT, NF90_GET_VAR, NF90_INQ_VARID,                           &
                             NF90_INQUIRE_ATTRIBUTE, NF90_INQUIRE_DIMENSION, NF90_INQUIRE_VARIABLE

implicit none

private

public :: IO_Field_read_nc4

INTERFACE IO_Field_read_nc4
   MODULE PROCEDURE IO_Field_read_nc4_X0,IO_Field_read_nc4_X1, &
                    IO_Field_read_nc4_X2,IO_Field_read_nc4_X3, &
                    IO_Field_read_nc4_X4,IO_Field_read_nc4_X5, &
                    IO_Field_read_nc4_X6,                      &
                    IO_Field_read_nc4_N0,IO_Field_read_nc4_N1, &
                    IO_Field_read_nc4_N2,IO_Field_read_nc4_N3, &
                    IO_Field_read_nc4_L0,IO_Field_read_nc4_L1, &
                    IO_Field_read_nc4_C0,                      &
                    IO_Field_read_nc4_T0
END INTERFACE IO_Field_read_nc4

contains

SUBROUTINE IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,KVARID,KRESP,HCALENDAR)
!
USE MODD_PARAMETERS, ONLY: NGRIDUNKNOWN
!
TYPE(TFILEDATA),          INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),         INTENT(INOUT) :: TPFIELD
INTEGER(KIND=CDFINT),     INTENT(IN)    :: KVARID
INTEGER,                  INTENT(OUT)   :: KRESP  ! return-code
CHARACTER(LEN=*),OPTIONAL,INTENT(IN)    :: HCALENDAR
!
INTEGER                      :: IERRLEVEL
INTEGER                      :: IGRID
INTEGER(KIND=CDFINT)         :: INCID
INTEGER(KIND=CDFINT)         :: ILEN
INTEGER(KIND=CDFINT)         :: istatus
CHARACTER(LEN=12)            :: YVAL_FILE, YVAL_MEM
CHARACTER(LEN=:),ALLOCATABLE :: YVALUE
LOGICAL                      :: GOLDMNH !if old version of MesoNH (<5.4, old files without complete and correct metadata)
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)//': called for field '//TRIM(TPFIELD%CMNHNAME))
!
KRESP = 0
INCID = TPFILE%NNCID
!
GOLDMNH = TPFILE%NMNHVERSION(1)<5 .OR. (TPFILE%NMNHVERSION(1)==5 .AND. TPFILE%NMNHVERSION(2)<4)
!
IF (GOLDMNH) THEN !Set a lower level of error if file comes from an old MesoNH version
  IERRLEVEL = NVERB_WARNING
ELSE
  IERRLEVEL = NVERB_ERROR
END IF
!
! GRID
!
istatus = NF90_GET_ATT(INCID,KVARID,'grid',IGRID)
IF (istatus /= NF90_NOERR) istatus = NF90_GET_ATT(INCID,KVARID,'GRID',IGRID)
IF (istatus == NF90_NOERR) THEN
  IF (IGRID/=TPFIELD%NGRID) THEN
    WRITE(YVAL_FILE,'(I12)') IGRID
    WRITE(YVAL_MEM, '(I12)') TPFIELD%NGRID
    CALL PRINT_MSG(IERRLEVEL,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': expected GRID     value ('//TRIM(ADJUSTL(YVAL_MEM))//             &
                   ') is different than found in file ('//TRIM(ADJUSTL(YVAL_FILE))//') for variable '//TRIM(TPFIELD%CMNHNAME))
    IF (.NOT.GOLDMNH) THEN !Do not modify probably incorrect grid number (to prevent problems later with other correct files)
      TPFIELD%NGRID = IGRID
      KRESP = -111 !Used later to broadcast modified metadata
    END IF
  ELSE
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': expected GRID found in file for field '//TRIM(TPFIELD%CMNHNAME))
  ENDIF
ELSE !no GRID
  IF (TPFIELD%NGRID==0 .OR. TPFIELD%NGRID==NGRIDUNKNOWN) THEN
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': no GRID (as expected) in file for field '//TRIM(TPFIELD%CMNHNAME))
  ELSE
    CALL PRINT_MSG(IERRLEVEL,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': expected GRID but not found in file for field '//TRIM(TPFIELD%CMNHNAME))
  END IF
ENDIF
!
! COMMENT
!
istatus = NF90_INQUIRE_ATTRIBUTE(INCID, KVARID, 'comment', LEN=ILEN)
IF (istatus == NF90_NOERR) THEN
  ALLOCATE(CHARACTER(LEN=ILEN) :: YVALUE)
  istatus = NF90_GET_ATT(INCID, KVARID, 'comment', YVALUE)
  IF (LEN_TRIM(TPFIELD%CCOMMENT)==0 .AND. LEN_TRIM(YVALUE)>0) THEN
    !Expected comment is empty, read comment is not
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': COMMENT  found (unexpected) in file for field '//TRIM(TPFIELD%CMNHNAME))
    TPFIELD%CCOMMENT=TRIM(YVALUE)
  ELSE IF (TRIM(YVALUE)/=TRIM(TPFIELD%CCOMMENT)) THEN
    CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': expected COMMENT ('//TRIM(TPFIELD%CCOMMENT)//                    &
                   ') is different than found ('//TRIM(YVALUE)//') in file for field '//TRIM(TPFIELD%CMNHNAME))
    TPFIELD%CCOMMENT=TRIM(YVALUE)
    KRESP = -111 !Used later to broadcast modified metadata
  ELSE
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': expected COMMENT  found in file for field '//TRIM(TPFIELD%CMNHNAME))
  END IF
  DEALLOCATE(YVALUE)
ELSE !no COMMENT
  IF (LEN_TRIM(TPFIELD%CCOMMENT)==0) THEN
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': no COMMENT (as expected) in file for field '//TRIM(TPFIELD%CMNHNAME))
  ELSE
    CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': expected COMMENT but not found in file for field '//TRIM(TPFIELD%CMNHNAME))
  END IF
END IF
!
! STDNAME
!
istatus = NF90_INQUIRE_ATTRIBUTE(INCID, KVARID, 'standard_name', LEN=ILEN)
IF (istatus == NF90_NOERR) THEN
  ALLOCATE(CHARACTER(LEN=ILEN) :: YVALUE)
  istatus = NF90_GET_ATT(INCID, KVARID, 'standard_name', YVALUE)
  IF (TRIM(YVALUE)/=TRIM(TPFIELD%CSTDNAME)) THEN
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': expected STDNAME  ('//TRIM(TPFIELD%CSTDNAME)//                      &
                   ') is different than found ('//TRIM(YVALUE)//') in file for field '//TRIM(TPFIELD%CMNHNAME))
    TPFIELD%CSTDNAME=TRIM(YVALUE)
    KRESP = -111 !Used later to broadcast modified metadata
  ELSE
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': expected STDNAME  found in file for field '//TRIM(TPFIELD%CMNHNAME))
  END IF
  DEALLOCATE(YVALUE)
ELSE !no STDNAME
  IF (LEN_TRIM(TPFIELD%CSTDNAME)==0) THEN
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': no STDNAME (as expected) in file for field '//TRIM(TPFIELD%CMNHNAME))
  ELSE
    CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': expected STDNAME but not found in file for field '//TRIM(TPFIELD%CMNHNAME))
  END IF
END IF
!
! LONGNAME
!
istatus = NF90_INQUIRE_ATTRIBUTE(INCID, KVARID, 'long_name', LEN=ILEN)
IF (istatus == NF90_NOERR) THEN
  ALLOCATE(CHARACTER(LEN=ILEN) :: YVALUE)
  istatus = NF90_GET_ATT(INCID, KVARID, 'long_name', YVALUE)
  IF (TRIM(YVALUE)/=TRIM(TPFIELD%CLONGNAME)) THEN
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': expected LONGNAME ('//TRIM(TPFIELD%CLONGNAME)//                  &
                   ') is different than found ('//TRIM(YVALUE)//') in file for field '//TRIM(TPFIELD%CMNHNAME))
    TPFIELD%CLONGNAME=TRIM(YVALUE)
    KRESP = -111 !Used later to broadcast modified metadata
  ELSE
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': expected LONGNAME found in file for field '//TRIM(TPFIELD%CMNHNAME))
  END IF
  DEALLOCATE(YVALUE)
ELSE !no LONGNAME
  IF (LEN_TRIM(TPFIELD%CLONGNAME)==0) THEN
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': no LONGNAME (as expected) in file for field '//TRIM(TPFIELD%CMNHNAME))
  ELSE
    CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': expected LONGNAME but not found in file for field '//TRIM(TPFIELD%CMNHNAME))
  END IF
END IF
!
! UNITS
!
istatus = NF90_INQUIRE_ATTRIBUTE(INCID, KVARID, 'units', LEN=ILEN)
IF (istatus == NF90_NOERR) THEN
  ALLOCATE(CHARACTER(LEN=ILEN) :: YVALUE)
  istatus = NF90_GET_ATT(INCID, KVARID, 'units', YVALUE)
  IF (TRIM(YVALUE)/=TRIM(TPFIELD%CUNITS)) THEN
    IF(.NOT.PRESENT(HCALENDAR)) THEN
      CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                     ': expected UNITS ('//TRIM(TPFIELD%CUNITS)//                           &
                     ') is different than found ('//TRIM(YVALUE)//') in file for field '//TRIM(TPFIELD%CMNHNAME))
      KRESP = -111 !Used later to broadcast modified metadata
    ELSE
      CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                     ': UNITS found in file for field '//TRIM(TPFIELD%CMNHNAME)//' (will be analysed later)')
    END IF
    TPFIELD%CUNITS=TRIM(YVALUE)
  ELSE
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': expected UNITS    found in file for field '//TRIM(TPFIELD%CMNHNAME))
  END IF
  DEALLOCATE(YVALUE)
ELSE !no UNITS
  IF (LEN_TRIM(TPFIELD%CUNITS)==0) THEN
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': no UNITS (as expected) in file for field '//TRIM(TPFIELD%CMNHNAME))
  ELSE
    IF(.NOT.PRESENT(HCALENDAR)) THEN
      CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                     ': expected UNITS but not found in file for field '//TRIM(TPFIELD%CMNHNAME))
    ELSE
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                     ': expected UNITS but not found in file for field '//TRIM(TPFIELD%CMNHNAME))
      KRESP = -3
    END IF
  END IF
END IF
!
! CALENDAR
!
IF(PRESENT(HCALENDAR)) THEN
istatus = NF90_INQUIRE_ATTRIBUTE(INCID, KVARID, 'calendar', LEN=ILEN)
IF (istatus == NF90_NOERR) THEN
  ALLOCATE(CHARACTER(LEN=ILEN) :: YVALUE)
  istatus = NF90_GET_ATT(INCID, KVARID, 'calendar', YVALUE)
  IF (TRIM(YVALUE)/=TRIM(HCALENDAR)) THEN
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': expected CALENDAR ('//TRIM(HCALENDAR)//                             &
                   ') is different than found ('//TRIM(YVALUE)//') in file for field '//TRIM(TPFIELD%CMNHNAME))
  ELSE
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                   ': expected CALENDAR found in file for field '//TRIM(TPFIELD%CMNHNAME))
  END IF
  DEALLOCATE(YVALUE)
ELSE !no CALENDAR
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_attr_read_check_nc4',TRIM(TPFILE%CNAME)// &
                 ': expected CALENDAR but not found in file for field '//TRIM(TPFIELD%CMNHNAME))
END IF
ENDIF
!
END SUBROUTINE IO_Field_attr_read_check_nc4


SUBROUTINE IO_Field_read_nc4_X0(TPFILE, TPFIELD, PFIELD, KRESP)
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
REAL,             INTENT(INOUT) :: PFIELD
INTEGER,          INTENT(OUT)   :: KRESP  ! return-code

INTEGER(KIND=CDFINT) :: istatus
INTEGER(KIND=CDFINT) :: INCID
INTEGER(KIND=CDFINT) :: IVARID
INTEGER(KIND=CDFINT) :: ITYPE   ! variable type
INTEGER(KIND=CDFINT) :: IDIMS   ! number of dimensions
CHARACTER(LEN=30)    :: YVARNAME
INTEGER              :: IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_nc4_X0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))

IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID

CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! Get variable ID, NDIMS and TYPE
istatus = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (istatus /= NF90_NOERR) THEN
  CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X0','NF90_INQ_VARID',TRIM(YVARNAME),IRESP)
  GOTO 1000
END IF
istatus = NF90_INQUIRE_VARIABLE(INCID, IVARID, XTYPE=ITYPE, NDIMS=IDIMS)
IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X0','NF90_INQUIRE_VARIABLE',TRIM(YVARNAME))

!Neglect the time dimension (of size 1)
IF (TPFIELD%LTIMEDEP) IDIMS=IDIMS-1

IF (IDIMS == 0 .AND. (ITYPE == NF90_FLOAT .OR. ITYPE == NF90_DOUBLE) ) THEN
  ! Read variable
  istatus = NF90_GET_VAR(INCID, IVARID, PFIELD)
  IF (istatus /= NF90_NOERR) THEN
    CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X0','NF90_GET_VAR',TRIM(YVARNAME),IRESP)
    GOTO 1000
  END IF
  ! Read and check attributes of variable
  CALL IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,IVARID,IRESP)
ELSE
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_X0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                         ' not read (wrong size or type)')
  IRESP = -3
END IF

1000 CONTINUE
KRESP = IRESP

END SUBROUTINE IO_Field_read_nc4_X0


SUBROUTINE IO_Field_read_nc4_X1(TPFILE, TPFIELD, PFIELD, KRESP)
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
REAL,DIMENSION(:),INTENT(INOUT) :: PFIELD
INTEGER,          INTENT(OUT)   :: KRESP  ! return-code

INTEGER(KIND=CDFINT) :: istatus
INTEGER(KIND=CDFINT) :: INCID
INTEGER(KIND=CDFINT) :: IVARID
INTEGER(KIND=CDFINT) :: ITYPE   ! variable type
INTEGER(KIND=CDFINT) :: IDIMS   ! number of dimensions
INTEGER(KIND=CDFINT),DIMENSION(NF90_MAX_VAR_DIMS) :: IVDIMS
CHARACTER(LEN=30)    :: YVARNAME
INTEGER(KIND=CDFINT) :: IDIMLEN
INTEGER              :: IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_nc4_X1',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))

IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID

CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! Get variable ID, NDIMS and TYPE
istatus = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (istatus /= NF90_NOERR) THEN
  CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X1','NF90_INQ_VARID',TRIM(YVARNAME),IRESP)
  GOTO 1000
END IF
istatus = NF90_INQUIRE_VARIABLE(INCID, IVARID, XTYPE=ITYPE, NDIMS=IDIMS, DIMIDS=IVDIMS)
IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X1','NF90_INQUIRE_VARIABLE',TRIM(YVARNAME))

!Neglect the time dimension (of size 1)
IF (TPFIELD%LTIMEDEP) IDIMS=IDIMS-1

IF (IDIMS == 1 .AND. (ITYPE == NF90_FLOAT .OR. ITYPE == NF90_DOUBLE) ) THEN
  ! Check size of variable before reading
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(1), LEN=IDIMLEN)
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X1','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))

  IF (IDIMLEN == SIZE(PFIELD)) THEN
    ! Read variable
    istatus = NF90_GET_VAR(INCID, IVARID, PFIELD)
    IF (istatus /= NF90_NOERR) THEN
      CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X1','NF90_GET_VAR',TRIM(YVARNAME),IRESP)
      GOTO 1000
    END IF
    ! Read and check attributes of variable
    CALL IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,IVARID,IRESP)
  ELSE
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_X1',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                           ' not read (wrong size)')
    IRESP = -3
  END IF
ELSE
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_X1',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                         ' not read (wrong number of dimensions or wrong type)')
  IRESP = -3
END IF

1000 CONTINUE
KRESP = IRESP

END SUBROUTINE IO_Field_read_nc4_X1


SUBROUTINE IO_Field_read_nc4_X2(TPFILE, TPFIELD, PFIELD, KRESP)
TYPE(TFILEDATA),    INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),   INTENT(INOUT) :: TPFIELD
REAL,DIMENSION(:,:),INTENT(INOUT) :: PFIELD
INTEGER,            INTENT(OUT)   :: KRESP  ! return-code

INTEGER(KIND=CDFINT) :: istatus
INTEGER(KIND=CDFINT) :: INCID
INTEGER(KIND=CDFINT) :: IVARID
INTEGER(KIND=CDFINT) :: ITYPE   ! variable type
INTEGER(KIND=CDFINT) :: IDIMS   ! number of dimensions
INTEGER(KIND=CDFINT),DIMENSION(NF90_MAX_VAR_DIMS) :: IVDIMS
CHARACTER(LEN=30)    :: YVARNAME
INTEGER(KIND=CDFINT),DIMENSION(3) :: IDIMLEN
INTEGER              :: IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_nc4_X2',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))

IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID

CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! Get variable ID, NDIMS and TYPE
istatus = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (istatus /= NF90_NOERR) THEN
  CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X2','NF90_INQ_VARID',TRIM(YVARNAME),IRESP)
  GOTO 1000
END IF
istatus = NF90_INQUIRE_VARIABLE(INCID, IVARID, XTYPE=ITYPE, NDIMS=IDIMS, DIMIDS=IVDIMS)
IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X2','NF90_INQUIRE_VARIABLE',TRIM(YVARNAME))

!Neglect the time dimension (of size 1)
IF (TPFIELD%LTIMEDEP) IDIMS=IDIMS-1

!Treat special case of a degenerated 3D array (3rd dimension size is 1)
IF (IDIMS==3) THEN
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(3), LEN=IDIMLEN(3))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X2','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  IF (IDIMLEN(3)==1) THEN
    CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_read_nc4_X2',TRIM(TPFILE%CNAME)// &
                   ': reading 3D array with degenerated third dimension in 2D array for '//TRIM(YVARNAME))
    IDIMS = 2
  ELSE
    CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Field_read_nc4_X2',TRIM(TPFILE%CNAME)//': wrong number of dimensions for '//TRIM(YVARNAME))
  END IF
END IF

IF (IDIMS == 2 .AND. (ITYPE == NF90_FLOAT .OR. ITYPE == NF90_DOUBLE) ) THEN
  ! Check size of variable before reading
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(1), LEN=IDIMLEN(1))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X2','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(2), LEN=IDIMLEN(2))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X2','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))

  IF (IDIMLEN(1) == SIZE(PFIELD,1) .AND. IDIMLEN(2) == SIZE(PFIELD,2)) THEN
    ! Read variable
    istatus = NF90_GET_VAR(INCID, IVARID, PFIELD)
    IF (istatus /= NF90_NOERR) THEN
      CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X2','NF90_GET_VAR',TRIM(YVARNAME),IRESP)
      GOTO 1000
    END IF
    ! Read and check attributes of variable
    CALL IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,IVARID,IRESP)
  ELSE
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_X2',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                           ' not read (wrong size)')
    IRESP = -3
  END IF
ELSE
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_X2',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                         ' not read (wrong number of dimensions or wrong type)')
  IRESP = -3
END IF

1000 CONTINUE
KRESP = IRESP

END SUBROUTINE IO_Field_read_nc4_X2


SUBROUTINE IO_Field_read_nc4_X3(TPFILE, TPFIELD, PFIELD, KRESP)
TYPE(TFILEDATA),      INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),     INTENT(INOUT) :: TPFIELD
REAL,DIMENSION(:,:,:),INTENT(INOUT) :: PFIELD
INTEGER,              INTENT(OUT)   :: KRESP  ! return-code

INTEGER(KIND=CDFINT)                              :: istatus
INTEGER(KIND=CDFINT)                              :: INCID
INTEGER(KIND=CDFINT)                              :: IVARID
INTEGER(KIND=CDFINT)                              :: ITYPE   ! variable type
INTEGER(KIND=CDFINT)                              :: IDIMS   ! number of dimensions
INTEGER(KIND=CDFINT),DIMENSION(NF90_MAX_VAR_DIMS) :: IVDIMS
INTEGER(KIND=CDFINT),DIMENSION(3)                 :: IDIMLEN
CHARACTER(LEN=30)                                 :: YVARNAME
INTEGER                                           :: IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_nc4_X3',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))

IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID

CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! Get variable ID, NDIMS and TYPE
istatus = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (istatus /= NF90_NOERR) THEN
  CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X3','NF90_INQ_VARID',TRIM(YVARNAME),IRESP)
  GOTO 1000
END IF
istatus = NF90_INQUIRE_VARIABLE(INCID, IVARID, XTYPE=ITYPE, NDIMS=IDIMS, DIMIDS=IVDIMS)
IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X3','NF90_INQUIRE_VARIABLE',TRIM(YVARNAME))

!Neglect the time dimension (of size 1)
IF (TPFIELD%LTIMEDEP) IDIMS=IDIMS-1

IF (IDIMS == 3 .AND. (ITYPE == NF90_FLOAT .OR. ITYPE == NF90_DOUBLE) ) THEN
  ! Check size of variable before reading
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(1), LEN=IDIMLEN(1))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X3','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(2), LEN=IDIMLEN(2))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X3','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(3), LEN=IDIMLEN(3))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X3','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))

  IF (IDIMLEN(1) == SIZE(PFIELD,1) .AND. IDIMLEN(2) == SIZE(PFIELD,2) .AND. IDIMLEN(3) == SIZE(PFIELD,3)) THEN
    ! Read variable
    istatus = NF90_GET_VAR(INCID, IVARID, PFIELD)
    IF (istatus /= NF90_NOERR) THEN
      CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X3','NF90_GET_VAR',TRIM(YVARNAME),IRESP)
      GOTO 1000
    END IF
    ! Read and check attributes of variable
    CALL IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,IVARID,IRESP)
  ELSE
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_X3',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                           ' not read (wrong size)')
    IRESP = -3
  END IF
ELSE
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_X3',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                         ' not read (wrong number of dimensions or wrong type)')
  IRESP = -3
END IF

1000 CONTINUE
KRESP = IRESP

END SUBROUTINE IO_Field_read_nc4_X3


SUBROUTINE IO_Field_read_nc4_X4(TPFILE, TPFIELD, PFIELD, KRESP)
TYPE(TFILEDATA),        INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),       INTENT(INOUT) :: TPFIELD
REAL,DIMENSION(:,:,:,:),INTENT(INOUT) :: PFIELD
INTEGER,                INTENT(OUT)   :: KRESP  ! return-code

INTEGER(KIND=CDFINT)                              :: istatus
INTEGER(KIND=CDFINT)                              :: INCID
INTEGER(KIND=CDFINT)                              :: IVARID
INTEGER(KIND=CDFINT)                              :: ITYPE   ! variable type
INTEGER(KIND=CDFINT)                              :: IDIMS   ! number of dimensions
INTEGER(KIND=CDFINT),DIMENSION(NF90_MAX_VAR_DIMS) :: IVDIMS
INTEGER(KIND=CDFINT),DIMENSION(4)                 :: IDIMLEN
CHARACTER(LEN=30)                                 :: YVARNAME
INTEGER                                           :: IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_nc4_X4',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))

IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID

CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! Get variable ID, NDIMS and TYPE
istatus = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (istatus /= NF90_NOERR) THEN
  CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X4','NF90_INQ_VARID',TRIM(YVARNAME),IRESP)
  GOTO 1000
END IF
istatus = NF90_INQUIRE_VARIABLE(INCID, IVARID, XTYPE=ITYPE, NDIMS=IDIMS, DIMIDS=IVDIMS)
IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X4','NF90_INQUIRE_VARIABLE',TRIM(YVARNAME))

!Neglect the time dimension (of size 1)
IF (TPFIELD%LTIMEDEP) IDIMS=IDIMS-1

IF (IDIMS == 4 .AND. (ITYPE == NF90_FLOAT .OR. ITYPE == NF90_DOUBLE) ) THEN
  ! Check size of variable before reading
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(1), LEN=IDIMLEN(1))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X4','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(2), LEN=IDIMLEN(2))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X4','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(3), LEN=IDIMLEN(3))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X4','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(4), LEN=IDIMLEN(4))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X4','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))

  IF ( IDIMLEN(1) == SIZE(PFIELD,1) .AND. IDIMLEN(2) == SIZE(PFIELD,2) .AND. &
       IDIMLEN(3) == SIZE(PFIELD,3) .AND. IDIMLEN(4) == SIZE(PFIELD,4)) THEN
    ! Read variable
    istatus = NF90_GET_VAR(INCID, IVARID, PFIELD)
    IF (istatus /= NF90_NOERR) THEN
      CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X4','NF90_GET_VAR',TRIM(YVARNAME),IRESP)
      GOTO 1000
    END IF
    ! Read and check attributes of variable
    CALL IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,IVARID,IRESP)
  ELSE
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_X4',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                           ' not read (wrong size)')
    IRESP = -3
  END IF
ELSE
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_X4',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                         ' not read (wrong number of dimensions or wrong type)')
  IRESP = -3
END IF

1000 CONTINUE
KRESP = IRESP

END SUBROUTINE IO_Field_read_nc4_X4


SUBROUTINE IO_Field_read_nc4_X5(TPFILE, TPFIELD, PFIELD, KRESP)
TYPE(TFILEDATA),          INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),         INTENT(INOUT) :: TPFIELD
REAL,DIMENSION(:,:,:,:,:),INTENT(INOUT) :: PFIELD
INTEGER,                  INTENT(OUT)   :: KRESP  ! return-code

INTEGER(KIND=CDFINT)                              :: istatus
INTEGER(KIND=CDFINT)                              :: INCID
INTEGER(KIND=CDFINT)                              :: IVARID
INTEGER(KIND=CDFINT)                              :: ITYPE   ! variable type
INTEGER(KIND=CDFINT)                              :: IDIMS   ! number of dimensions
INTEGER(KIND=CDFINT),DIMENSION(NF90_MAX_VAR_DIMS) :: IVDIMS
INTEGER(KIND=CDFINT),DIMENSION(5)                 :: IDIMLEN
CHARACTER(LEN=30)                                 :: YVARNAME
INTEGER                                           :: IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_nc4_X5',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))

IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID

CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! Get variable ID, NDIMS and TYPE
istatus = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (istatus /= NF90_NOERR) THEN
  CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X5','NF90_INQ_VARID',TRIM(YVARNAME),IRESP)
  GOTO 1000
END IF
istatus = NF90_INQUIRE_VARIABLE(INCID, IVARID, XTYPE=ITYPE, NDIMS=IDIMS, DIMIDS=IVDIMS)
IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X5','NF90_INQUIRE_VARIABLE',TRIM(YVARNAME))

!Neglect the time dimension (of size 1)
IF (TPFIELD%LTIMEDEP) IDIMS=IDIMS-1

IF (IDIMS == 5 .AND. (ITYPE == NF90_FLOAT .OR. ITYPE == NF90_DOUBLE) ) THEN
  ! Check size of variable before reading
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(1), LEN=IDIMLEN(1))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X5','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(2), LEN=IDIMLEN(2))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X5','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(3), LEN=IDIMLEN(3))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X5','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(4), LEN=IDIMLEN(4))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X5','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(5), LEN=IDIMLEN(5))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X5','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))

  IF ( IDIMLEN(1) == SIZE(PFIELD,1) .AND. IDIMLEN(2) == SIZE(PFIELD,2) .AND. &
       IDIMLEN(3) == SIZE(PFIELD,3) .AND. IDIMLEN(4) == SIZE(PFIELD,4) .AND. &
       IDIMLEN(5) == SIZE(PFIELD,5) ) THEN
    ! Read variable
    istatus = NF90_GET_VAR(INCID, IVARID, PFIELD)
    IF (istatus /= NF90_NOERR) THEN
      CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X5','NF90_GET_VAR',TRIM(YVARNAME),IRESP)
      GOTO 1000
    END IF
    ! Read and check attributes of variable
    CALL IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,IVARID,IRESP)
  ELSE
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_X5',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                           ' not read (wrong size)')
    IRESP = -3
  END IF
ELSE
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_X5',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                         ' not read (wrong number of dimensions or wrong type)')
  IRESP = -3
END IF

1000 CONTINUE
KRESP = IRESP

END SUBROUTINE IO_Field_read_nc4_X5


SUBROUTINE IO_Field_read_nc4_X6(TPFILE, TPFIELD, PFIELD, KRESP)
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),           INTENT(INOUT) :: TPFIELD
REAL,DIMENSION(:,:,:,:,:,:),INTENT(INOUT) :: PFIELD
INTEGER,                    INTENT(OUT)   :: KRESP  ! return-code

INTEGER(KIND=CDFINT)                              :: istatus
INTEGER(KIND=CDFINT)                              :: INCID
INTEGER(KIND=CDFINT)                              :: IVARID
INTEGER(KIND=CDFINT)                              :: ITYPE   ! variable type
INTEGER(KIND=CDFINT)                              :: IDIMS   ! number of dimensions
INTEGER(KIND=CDFINT),DIMENSION(NF90_MAX_VAR_DIMS) :: IVDIMS
INTEGER(KIND=CDFINT),DIMENSION(6)                 :: IDIMLEN
CHARACTER(LEN=30)                                 :: YVARNAME
INTEGER                                           :: IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_nc4_X6',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))

IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID

CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! Get variable ID, NDIMS and TYPE
istatus = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (istatus /= NF90_NOERR) THEN
  CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X6','NF90_INQ_VARID',TRIM(YVARNAME),IRESP)
  GOTO 1000
END IF
istatus = NF90_INQUIRE_VARIABLE(INCID, IVARID, XTYPE=ITYPE, NDIMS=IDIMS, DIMIDS=IVDIMS)
IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X6','NF90_INQUIRE_VARIABLE',TRIM(YVARNAME))

!Neglect the time dimension (of size 1)
IF (TPFIELD%LTIMEDEP) IDIMS=IDIMS-1

IF (IDIMS == 6 .AND. (ITYPE == NF90_FLOAT .OR. ITYPE == NF90_DOUBLE) ) THEN
  ! Check size of variable before reading
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(1), LEN=IDIMLEN(1))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X6','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(2), LEN=IDIMLEN(2))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X6','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(3), LEN=IDIMLEN(3))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X6','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(4), LEN=IDIMLEN(4))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X6','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(5), LEN=IDIMLEN(5))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X6','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(6), LEN=IDIMLEN(6))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X6','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))

  IF ( IDIMLEN(1) == SIZE(PFIELD,1) .AND. IDIMLEN(2) == SIZE(PFIELD,2) .AND. &
       IDIMLEN(3) == SIZE(PFIELD,3) .AND. IDIMLEN(4) == SIZE(PFIELD,4) .AND. &
       IDIMLEN(5) == SIZE(PFIELD,5) .AND. IDIMLEN(6) == SIZE(PFIELD,6) ) THEN
    ! Read variable
    istatus = NF90_GET_VAR(INCID, IVARID, PFIELD)
    IF (istatus /= NF90_NOERR) THEN
      CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_X6','NF90_GET_VAR',TRIM(YVARNAME),IRESP)
      GOTO 1000
    END IF
    ! Read and check attributes of variable
    CALL IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,IVARID,IRESP)
  ELSE
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_X6',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                           ' not read (wrong size)')
    IRESP = -3
  END IF
ELSE
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_X6',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                         ' not read (wrong number of dimensions or wrong type)')
  IRESP = -3
END IF

1000 CONTINUE
KRESP = IRESP

END SUBROUTINE IO_Field_read_nc4_X6


SUBROUTINE IO_Field_read_nc4_N0(TPFILE, TPFIELD, KFIELD, KRESP)
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
INTEGER,          INTENT(INOUT) :: KFIELD
INTEGER,          INTENT(OUT)   :: KRESP  ! return-code

INTEGER(KIND=CDFINT) :: istatus
INTEGER(KIND=CDFINT) :: INCID
INTEGER(KIND=CDFINT) :: IVARID
INTEGER(KIND=CDFINT) :: ITYPE   ! variable type
INTEGER(KIND=CDFINT) :: IDIMS   ! number of dimensions
CHARACTER(LEN=30)    :: YVARNAME
INTEGER              :: IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_nc4_N0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))

IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID

CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! Get variable ID, NDIMS and TYPE
istatus = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (istatus /= NF90_NOERR) THEN
  CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N0','NF90_INQ_VARID',TRIM(YVARNAME),IRESP)
  GOTO 1000
END IF
istatus = NF90_INQUIRE_VARIABLE(INCID, IVARID, XTYPE=ITYPE, NDIMS=IDIMS)
IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N0','NF90_INQUIRE_VARIABLE',TRIM(YVARNAME))

!Neglect the time dimension (of size 1)
IF (TPFIELD%LTIMEDEP) IDIMS=IDIMS-1

!Can read either 4 or 8 byte integers
IF (IDIMS == 0 .AND. (ITYPE == NF90_INT .OR. ITYPE == NF90_INT64) ) THEN
   ! Read variable
   istatus = NF90_GET_VAR(INCID, IVARID, KFIELD)
   IF (istatus /= NF90_NOERR) THEN
      CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N0','NF90_GET_VAR',TRIM(YVARNAME),IRESP)
      GOTO 1000
   END IF
   ! Read and check attributes of variable
   CALL IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,IVARID,IRESP)
ELSE
   CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_N0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                          ' not read (wrong size or type)')
   IRESP = -3
END IF

1000 CONTINUE
KRESP = IRESP

END SUBROUTINE IO_Field_read_nc4_N0


SUBROUTINE IO_Field_read_nc4_N1(TPFILE, TPFIELD, KFIELD, KRESP)
TYPE(TFILEDATA),         INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),        INTENT(INOUT) :: TPFIELD
INTEGER, DIMENSION(:),   INTENT(INOUT) :: KFIELD
INTEGER,                 INTENT(OUT)   :: KRESP  ! return-code

INTEGER(KIND=CDFINT)                              :: istatus
INTEGER(KIND=CDFINT)                              :: INCID
INTEGER(KIND=CDFINT)                              :: IVARID
INTEGER(KIND=CDFINT)                              :: ITYPE   ! variable type
INTEGER(KIND=CDFINT)                              :: IDIMS   ! number of dimensions
INTEGER(KIND=CDFINT),DIMENSION(NF90_MAX_VAR_DIMS) :: IVDIMS
CHARACTER(LEN=30)                                 :: YVARNAME
INTEGER(KIND=CDFINT)                              :: IDIMLEN
INTEGER                                           :: IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_nc4_N1',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))

IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID

CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! Get variable ID, NDIMS and TYPE
istatus = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (istatus /= NF90_NOERR) THEN
  CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N1','NF90_INQ_VARID',TRIM(YVARNAME),IRESP)
  GOTO 1000
END IF
istatus = NF90_INQUIRE_VARIABLE(INCID, IVARID, XTYPE=ITYPE, NDIMS=IDIMS, DIMIDS=IVDIMS)
IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N1','NF90_INQUIRE_VARIABLE',TRIM(YVARNAME))

!Neglect the time dimension (of size 1)
IF (TPFIELD%LTIMEDEP) IDIMS=IDIMS-1

!NF90_INT1 is for the case a boolean was written
IF (IDIMS == 1 .AND. (ITYPE == NF90_INT .OR. ITYPE == NF90_INT64 .OR. ITYPE == NF90_INT1) ) THEN
  ! Check size of variable before reading
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(1), LEN=IDIMLEN)
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N1','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))

  IF (IDIMLEN == SIZE(KFIELD)) THEN
    ! Read variable
    istatus = NF90_GET_VAR(INCID, IVARID, KFIELD)
    IF (istatus /= NF90_NOERR) THEN
      CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N1','NF90_GET_VAR',TRIM(YVARNAME),IRESP)
      GOTO 1000
    END IF
    ! Read and check attributes of variable
    CALL IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,IVARID,IRESP)
  ELSE
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_N1',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                           ' not read (wrong size)')
    IRESP = -3
  END IF
ELSE
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_N1',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                         ' not read (wrong number of dimensions or wrong type)')
  IRESP = -3
END IF

1000 CONTINUE
KRESP = IRESP

END SUBROUTINE IO_Field_read_nc4_N1


SUBROUTINE IO_Field_read_nc4_N2(TPFILE, TPFIELD, KFIELD, KRESP)
TYPE(TFILEDATA),         INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),        INTENT(INOUT) :: TPFIELD
INTEGER, DIMENSION(:,:), INTENT(INOUT) :: KFIELD
INTEGER,                 INTENT(OUT)   :: KRESP  ! return-code

INTEGER(KIND=CDFINT)                              :: istatus
INTEGER(KIND=CDFINT)                              :: INCID
INTEGER(KIND=CDFINT)                              :: IVARID
INTEGER(KIND=CDFINT)                              :: ITYPE   ! variable type
INTEGER(KIND=CDFINT)                              :: IDIMS   ! number of dimensions
INTEGER(KIND=CDFINT),DIMENSION(NF90_MAX_VAR_DIMS) :: IVDIMS
CHARACTER(LEN=30)                                 :: YVARNAME
INTEGER(KIND=CDFINT),DIMENSION(3)                 :: IDIMLEN
INTEGER                                           :: IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_nc4_N2',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))

IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID

CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! Get variable ID, NDIMS and TYPE
istatus = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (istatus /= NF90_NOERR) THEN
  CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N2','NF90_INQ_VARID',TRIM(YVARNAME),IRESP)
  GOTO 1000
END IF
istatus = NF90_INQUIRE_VARIABLE(INCID, IVARID, XTYPE=ITYPE, NDIMS=IDIMS, DIMIDS=IVDIMS)
IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N2','NF90_INQUIRE_VARIABLE',TRIM(YVARNAME))

!Neglect the time dimension (of size 1)
IF (TPFIELD%LTIMEDEP) IDIMS=IDIMS-1

!Treat special case of a degenerated 3D array (3rd dimension size is 1)
IF (IDIMS==3) THEN
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(3), LEN=IDIMLEN(3))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N2','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  IF (IDIMLEN(3)==1) THEN
    CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_read_nc4_N2',TRIM(TPFILE%CNAME)// &
                   ': reading 3D array with degenerated third dimension in 2D array for '//TRIM(YVARNAME))
    IDIMS = 2
  ELSE
    CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Field_read_nc4_N2',TRIM(TPFILE%CNAME)//': wrong number of dimensions for '//TRIM(YVARNAME))
  END IF
END IF

!NF90_INT1 is for the case a boolean was written
IF (IDIMS == SIZE(SHAPE(KFIELD)) .AND. (ITYPE == NF90_INT .OR. ITYPE == NF90_INT64 .OR. ITYPE == NF90_INT1) ) THEN
  ! Check size of variable before reading
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(1), LEN=IDIMLEN(1))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N2','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(2), LEN=IDIMLEN(2))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N2','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))

  IF (IDIMLEN(1) == SIZE(KFIELD,1) .AND. IDIMLEN(2) == SIZE(KFIELD,2)) THEN
    ! Read variable
    istatus = NF90_GET_VAR(INCID, IVARID, KFIELD)
    IF (istatus /= NF90_NOERR) THEN
      CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N2','NF90_GET_VAR',TRIM(YVARNAME),IRESP)
      GOTO 1000
    END IF
    ! Read and check attributes of variable
    CALL IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,IVARID,IRESP)
  ELSE
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_N2',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                           ' not read (wrong size)')
    IRESP = -3
  END IF
ELSE
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_N2',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                         ' not read (wrong number of dimensions or wrong type)')
  IRESP = -3
END IF

1000 CONTINUE
KRESP = IRESP

END SUBROUTINE IO_Field_read_nc4_N2

SUBROUTINE IO_Field_read_nc4_N3(TPFILE, TPFIELD, KFIELD, KRESP)
TYPE(TFILEDATA),           INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),          INTENT(INOUT) :: TPFIELD
INTEGER, DIMENSION(:,:,:), INTENT(INOUT) :: KFIELD
INTEGER,                   INTENT(OUT)   :: KRESP  ! return-code

INTEGER(KIND=CDFINT)                              :: istatus
INTEGER(KIND=CDFINT)                              :: INCID
INTEGER(KIND=CDFINT)                              :: IVARID
INTEGER(KIND=CDFINT)                              :: ITYPE   ! variable type
INTEGER(KIND=CDFINT)                              :: IDIMS   ! number of dimensions
INTEGER(KIND=CDFINT),DIMENSION(NF90_MAX_VAR_DIMS) :: IVDIMS
CHARACTER(LEN=30)                                 :: YVARNAME
INTEGER(KIND=CDFINT),DIMENSION(3)                 :: IDIMLEN
INTEGER                                           :: IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_nc4_N3',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))

IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID

CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! Get variable ID, NDIMS and TYPE
istatus = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (istatus /= NF90_NOERR) THEN
  CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N3','NF90_INQ_VARID',TRIM(YVARNAME),IRESP)
  GOTO 1000
END IF
istatus = NF90_INQUIRE_VARIABLE(INCID, IVARID, XTYPE=ITYPE, NDIMS=IDIMS, DIMIDS=IVDIMS)
IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N3','NF90_INQUIRE_VARIABLE',TRIM(YVARNAME))

!Neglect the time dimension (of size 1)
IF (TPFIELD%LTIMEDEP) IDIMS=IDIMS-1

!NF90_INT1 is for the case a boolean was written
IF (IDIMS == SIZE(SHAPE(KFIELD)) .AND. (ITYPE == NF90_INT .OR. ITYPE == NF90_INT64 .OR. ITYPE == NF90_INT1) ) THEN
  ! Check size of variable before reading
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(1), LEN=IDIMLEN(1))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N3','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(2), LEN=IDIMLEN(2))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N3','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(3), LEN=IDIMLEN(3))
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N3','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))

  IF (IDIMLEN(1) == SIZE(KFIELD,1) .AND. IDIMLEN(2) == SIZE(KFIELD,2) .AND. IDIMLEN(3) == SIZE(KFIELD,3)) THEN
    ! Read variable
    istatus = NF90_GET_VAR(INCID, IVARID, KFIELD)
    IF (istatus /= NF90_NOERR) THEN
      CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_N3','NF90_GET_VAR',TRIM(YVARNAME),IRESP)
      GOTO 1000
    END IF
    ! Read and check attributes of variable
    CALL IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,IVARID,IRESP)
  ELSE
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_N3',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                           ' not read (wrong size)')
    IRESP = -3
  END IF
ELSE
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_N3',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                         ' not read (wrong number of dimensions or wrong type)')
  IRESP = -3
END IF

1000 CONTINUE
KRESP = IRESP

END SUBROUTINE IO_Field_read_nc4_N3

SUBROUTINE IO_Field_read_nc4_L0(TPFILE, TPFIELD, OFIELD, KRESP)
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
LOGICAL,          INTENT(INOUT) :: OFIELD
INTEGER,          INTENT(OUT)   :: KRESP  ! return-code

INTEGER(KIND=CDFINT) :: istatus
INTEGER(KIND=CDFINT) :: INCID
INTEGER(KIND=CDFINT) :: IVARID
INTEGER(KIND=CDFINT) :: ITYPE   ! variable type
INTEGER(KIND=CDFINT) :: IDIMS   ! number of dimensions
CHARACTER(LEN=30)    :: YVARNAME
INTEGER              :: IRESP
INTEGER              :: IFIELD

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_nc4_L0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))

IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID

CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! Get variable ID, NDIMS and TYPE
istatus = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (istatus /= NF90_NOERR) THEN
  CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_L0','NF90_INQ_VARID',TRIM(YVARNAME),IRESP)
  GOTO 1000
END IF
istatus = NF90_INQUIRE_VARIABLE(INCID, IVARID, XTYPE=ITYPE, NDIMS=IDIMS)
IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_L0','NF90_INQUIRE_VARIABLE',TRIM(YVARNAME))

!Neglect the time dimension (of size 1)
IF (TPFIELD%LTIMEDEP) IDIMS=IDIMS-1

!NF90_INT1 is for the case a boolean was written
!Accept also INT and INT64 (for backward compatibility)
IF (IDIMS == 0 .AND. (ITYPE == NF90_INT1 .OR. ITYPE == NF90_INT .OR. ITYPE == NF90_INT64)  ) THEN
  ! Read variable
  istatus = NF90_GET_VAR(INCID, IVARID, IFIELD)
  IF (istatus /= NF90_NOERR) THEN
    CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_L0','NF90_GET_VAR',TRIM(YVARNAME),IRESP)
    GOTO 1000
  END IF

  IF (IFIELD==0) THEN
    OFIELD = .FALSE.
  ELSE IF (IFIELD==1) THEN
    OFIELD = .TRUE.
  ELSE
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_L0',TRIM(TPFILE%CNAME)//': invalid value in file for ' &
                                                           //TRIM(TPFIELD%CMNHNAME))
    OFIELD = .TRUE.
    IRESP = -112
  END IF

  ! Read and check attributes of variable
  CALL IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,IVARID,IRESP)
ELSE
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_L0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                         ' not read (wrong size or type)')
  IRESP = -3
END IF

1000 CONTINUE
KRESP = IRESP

END SUBROUTINE IO_Field_read_nc4_L0


SUBROUTINE IO_Field_read_nc4_L1(TPFILE, TPFIELD, OFIELD, KRESP)
TYPE(TFILEDATA),     INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),    INTENT(INOUT) :: TPFIELD
LOGICAL,DIMENSION(:),INTENT(INOUT) :: OFIELD
INTEGER,             INTENT(OUT)   :: KRESP  ! return-code

INTEGER(KIND=CDFINT)                              :: istatus
INTEGER(KIND=CDFINT)                              :: INCID
INTEGER(KIND=CDFINT)                              :: IVARID
INTEGER(KIND=CDFINT)                              :: ITYPE   ! variable type
INTEGER(KIND=CDFINT)                              :: IDIMS   ! number of dimensions
INTEGER(KIND=CDFINT),DIMENSION(NF90_MAX_VAR_DIMS) :: IVDIMS
INTEGER(KIND=CDFINT)                              :: IDIMLEN
CHARACTER(LEN=30)                                 :: YVARNAME
INTEGER                                           :: IRESP
INTEGER                                           :: JI
INTEGER,DIMENSION(SIZE(OFIELD))                   :: IFIELD

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_nc4_L1',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))

IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID

CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! Get variable ID, NDIMS and TYPE
istatus = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (istatus /= NF90_NOERR) THEN
  CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_L1','NF90_INQ_VARID',TRIM(YVARNAME),IRESP)
  GOTO 1000
END IF
istatus = NF90_INQUIRE_VARIABLE(INCID, IVARID, XTYPE=ITYPE, NDIMS=IDIMS, DIMIDS=IVDIMS)
IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_L1','NF90_INQUIRE_VARIABLE',TRIM(YVARNAME))

!Neglect the time dimension (of size 1)
IF (TPFIELD%LTIMEDEP) IDIMS=IDIMS-1

!NF90_INT1 is for the case a boolean was written
!Accept also INT and INT64 (for backward compatibility)
IF (IDIMS == 1 .AND. (ITYPE == NF90_INT1 .OR. ITYPE == NF90_INT .OR. ITYPE == NF90_INT64)  ) THEN
  ! Check size of variable before reading
  istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(1), LEN=IDIMLEN)
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_L1','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))

  IF (IDIMLEN == SIZE(OFIELD)) THEN
    ! Read variable
    istatus = NF90_GET_VAR(INCID, IVARID, IFIELD)
    IF (istatus /= NF90_NOERR) THEN
      CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_L1','NF90_GET_VAR',TRIM(YVARNAME),IRESP)
      GOTO 1000
    END IF

    DO JI=1,IDIMLEN
      IF (IFIELD(JI)==0) THEN
        OFIELD(JI) = .FALSE.
      ELSE IF (IFIELD(JI)==1) THEN
        OFIELD(JI) = .TRUE.
      ELSE
        OFIELD(JI) = .TRUE.
        IRESP = -112
      END IF
    END DO
    IF (IRESP==-112) THEN
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_L1',TRIM(TPFILE%CNAME)//': invalid value(s) in file for ' &
                                                             //TRIM(TPFIELD%CMNHNAME))
    END IF

    ! Read and check attributes of variable
    CALL IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,IVARID,IRESP)
  ELSE
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_L1',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                           ' not read (wrong size)')
    IRESP = -3
  END IF
ELSE
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_L1',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                         ' not read (wrong number of dimensions or wrong type)')
  IRESP = -3
END IF

1000 CONTINUE
KRESP = IRESP

END SUBROUTINE IO_Field_read_nc4_L1


SUBROUTINE IO_Field_read_nc4_C0(TPFILE, TPFIELD, HFIELD, KRESP)
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
CHARACTER(LEN=*), INTENT(INOUT) :: HFIELD
INTEGER,          INTENT(OUT)   :: KRESP  ! return-code

INTEGER(KIND=CDFINT)                              :: istatus
INTEGER(KIND=CDFINT)                              :: INCID
INTEGER(KIND=CDFINT)                              :: IVARID
INTEGER(KIND=CDFINT)                              :: ITYPE   ! variable type
INTEGER(KIND=CDFINT)                              :: IDIMS   ! number of dimensions
INTEGER(KIND=CDFINT),DIMENSION(NF90_MAX_VAR_DIMS) :: IVDIMS
CHARACTER(LEN=30)                                 :: YVARNAME
CHARACTER(LEN=:),ALLOCATABLE                      :: YSTR
INTEGER(KIND=CDFINT)                              :: IDIMLEN
INTEGER                                           :: IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_nc4_C0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))

IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID

CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! Get variable ID, NDIMS and TYPE
istatus = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (istatus /= NF90_NOERR) THEN
  CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_C0','NF90_INQ_VARID',TRIM(YVARNAME),IRESP)
  GOTO 1000
END IF
istatus = NF90_INQUIRE_VARIABLE(INCID, IVARID, XTYPE=ITYPE, NDIMS=IDIMS, DIMIDS=IVDIMS)
IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_C0','NF90_INQUIRE_VARIABLE',TRIM(YVARNAME))

IF (IDIMS == 1 .AND. (ITYPE == NF90_CHAR) ) THEN
   ! Check size of variable before reading
   istatus = NF90_INQUIRE_DIMENSION(INCID, IVDIMS(1), LEN=IDIMLEN)
  IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_C0','NF90_INQUIRE_DIMENSION',TRIM(YVARNAME))
   !
   ALLOCATE(CHARACTER(LEN=IDIMLEN)::YSTR)
   ! Read variable
   istatus = NF90_GET_VAR(INCID, IVARID, YSTR)
   IF (istatus /= NF90_NOERR) THEN
     CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_C0','NF90_GET_VAR',TRIM(YVARNAME),IRESP)
     GOTO 1000
   END IF
   IF (LEN_TRIM(YSTR) > LEN(HFIELD)) &
     CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_C0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)//' truncated')
   HFIELD = TRIM(YSTR)
   DEALLOCATE(YSTR)

   ! Read and check attributes of variable
   CALL IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,IVARID,IRESP)
ELSE
   CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_C0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                          ' not read (wrong size or type)')
   IRESP = -3
END IF

1000 CONTINUE
KRESP = IRESP

END SUBROUTINE IO_Field_read_nc4_C0

SUBROUTINE IO_Field_read_nc4_T0(TPFILE, TPFIELD, TPDATA, KRESP)
!
USE MODD_TYPE_DATE
!
USE MODE_DATETIME
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
TYPE (DATE_TIME), INTENT(INOUT) :: TPDATA
INTEGER,          INTENT(OUT)   :: KRESP  ! return-code

INTEGER(KIND=CDFINT)         :: istatus
INTEGER(KIND=CDFINT)         :: INCID
INTEGER(KIND=CDFINT)         :: IVARID
INTEGER(KIND=CDFINT)         :: ITYPE   ! variable type
INTEGER(KIND=CDFINT)         :: IDIMS   ! number of dimensions
CHARACTER(LEN=30)            :: YVARNAME
CHARACTER(LEN=:),ALLOCATABLE :: YSTR
INTEGER(KIND=CDFINT)         :: IDIMLEN
INTEGER                      :: IDX,IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_nc4_T0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))

IRESP = 0
! Get the Netcdf file ID
INCID = TPFILE%NNCID

CALL IO_Mnhname_clean(TPFIELD%CMNHNAME,YVARNAME)

! Get variable ID, NDIMS and TYPE
istatus = NF90_INQ_VARID(INCID, YVARNAME, IVARID)
IF (istatus /= NF90_NOERR) THEN
  CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_T0','NF90_INQ_VARID',TRIM(YVARNAME),IRESP)
  GOTO 1000
END IF
istatus = NF90_INQUIRE_VARIABLE(INCID, IVARID, XTYPE=ITYPE, NDIMS=IDIMS)
IF (istatus /= NF90_NOERR) CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_T0','NF90_INQUIRE_VARIABLE',TRIM(YVARNAME))

IF (IDIMS == 0 .AND. (ITYPE == NF90_FLOAT .OR. ITYPE == NF90_DOUBLE) ) THEN
  ! Read time
  istatus = NF90_GET_VAR(INCID, IVARID, TPDATA%xtime)
  IF (istatus /= NF90_NOERR) THEN
    CALL IO_Err_handle_nc4(istatus,'IO_Field_read_nc4_T0','NF90_GET_VAR',TRIM(YVARNAME),IRESP)
    GOTO 1000
  END IF
  ! Read and check attributes of variable
  CALL IO_Field_attr_read_check_nc4(TPFILE,TPFIELD,IVARID,IRESP,HCALENDAR='standard')
  ! Extract date from UNITS
  IDX =  INDEX(TPFIELD%CUNITS,'since ')
  READ(TPFIELD%CUNITS(IDX+6 :IDX+9), '( I4.4 )') TPDATA%nyear
  READ(TPFIELD%CUNITS(IDX+11:IDX+12),'( I2.2 )') TPDATA%nmonth
  READ(TPFIELD%CUNITS(IDX+14:IDX+15),'( I2.2 )') TPDATA%nday
  ! Simple check (should catch most errors)
  IF ( TPDATA%nday<1 .OR. TPDATA%nday>31 .OR. TPDATA%nmonth<1 .OR. TPDATA%nmonth>12 ) THEN
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_T0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                           ' read date is invalid')
    IRESP = -3
  END IF
  ! Correct date and time (necessary for example if time is bigger than 86400 s)
  CALL DATETIME_CORRECTDATE(TPDATA)
ELSE
   CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4_T0',TRIM(TPFILE%CNAME)//': '//TRIM(YVARNAME)// &
                                                          ' not read (wrong size or type)')
   IRESP = -3
END IF

1000 CONTINUE
KRESP = IRESP

END SUBROUTINE IO_Field_read_nc4_T0


end module mode_io_read_nc4
#else
!
! External dummy subroutines
!
subroutine IO_Field_read_nc4(a, b, c, d, e, f, g)
use mode_msg
integer :: a, b, c, d, e, f, g
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine IO_Field_read_nc4
!
#endif
