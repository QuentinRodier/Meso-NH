!MNH_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet may 2016  : use NetCDF Fortran module
!  J.Escobar   14/12/2017: correction for MNH_INT=8
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/12/2018: split of mode_netcdf into multiple modules/files
!  P. Wautelet 10/01/2019: replace handle_err by IO_Err_handle_nc4 for better netCDF error messages
!  P. Wautelet 05/03/2019: rename IO subroutines and modules
!  P. Wautelet 18/09/2019: correct support of 64bit integers (MNH_INT=8)
!-----------------------------------------------------------------
#if defined(MNH_IOCDF4)
module mode_io_tools_nc4

use modd_io,        only: tfiledata
use modd_netcdf,    only: dimcdf, iocdf, tdim_dummy
use modd_precision, only: CDFINT

use mode_field,  only: tfielddata
use mode_msg

use NETCDF,      only: NF90_NOERR, NF90_UNLIMITED, &
                       NF90_DEF_DIM, NF90_STRERROR

implicit none

private

public :: IO_Dim_find_byname_nc4, IO_Dimids_guess_nc4, IO_Knowndims_set_nc4
public :: IO_Iocdf_alloc_nc4, IO_Iocdf_dealloc_nc4, IO_Mnhname_clean
public :: IO_Dimcdf_get_nc4, IO_Strdimid_get_nc4, IO_Vdims_fill_nc4, IO_Err_handle_nc4

contains

SUBROUTINE IO_Dim_find_byname_nc4(TPFILE, HDIMNAME, TPDIM, KRESP)
TYPE(TFILEDATA),         INTENT(IN)  :: TPFILE
CHARACTER(LEN=*),        INTENT(IN)  :: HDIMNAME
TYPE(DIMCDF),            INTENT(OUT) :: TPDIM
INTEGER,                 INTENT(OUT) :: KRESP
!
TYPE(DIMCDF), POINTER :: TMP
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Dim_find_byname_nc4','called for dimension name '//TRIM(HDIMNAME))
!
KRESP = -2
!
IF(.NOT.ASSOCIATED(TPFILE%TNCDIMS%DIMLIST)) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dim_find_byname_nc4','DIMLIST not associated for file  '//TRIM(TPFILE%CNAME))
  KRESP = -1
  RETURN
END IF
!
TMP => TPFILE%TNCDIMS%DIMLIST
!
DO WHILE(ASSOCIATED(TMP))
  IF (TRIM(TMP%NAME)==TRIM(HDIMNAME)) THEN
    TPDIM = TMP
    KRESP = 0
    EXIT
  END IF
  TMP => TMP%NEXT
END DO
!
END SUBROUTINE IO_Dim_find_byname_nc4


SUBROUTINE IO_Dimids_guess_nc4(TPFILE, TPFIELD, KLEN, TPDIMS, KRESP)
!
USE MODE_FIELD, ONLY: TYPECHAR
!
!Used by LFI2CDF
TYPE(TFILEDATA),                      INTENT(IN)  :: TPFILE
TYPE(TFIELDDATA),                     INTENT(IN)  :: TPFIELD
INTEGER,                              INTENT(IN)  :: KLEN
TYPE(DIMCDF),DIMENSION(:),            INTENT(OUT) :: TPDIMS
INTEGER,                              INTENT(OUT) :: KRESP
!
INTEGER               :: IGRID
INTEGER(kind=CDFINT)  :: ILEN, ISIZE
INTEGER               :: JI
CHARACTER(LEN=32)     :: YINT
CHARACTER(LEN=2)      :: YDIR
TYPE(DIMCDF), POINTER :: PTDIM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Dimids_guess_nc4','called for '//TRIM(TPFIELD%CMNHNAME))
!
IGRID  =  TPFIELD%NGRID
YDIR   =  TPFIELD%CDIR
!
KRESP = 0
ILEN = 0
PTDIM => NULL()
!
IF(IGRID<0 .OR. IGRID>8) THEN
  WRITE(YINT,'( I0 )') IGRID
  CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Dimids_guess_nc4','invalid NGRID ('//TRIM(YINT)//') for field '//TRIM(TPFIELD%CMNHNAME))
END IF
!
IF(IGRID==0 .AND. YDIR/='--' .AND. YDIR/=''  ) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4','invalid YDIR ('//TRIM(YDIR)//') with NGRID=0 for field '&
                 //TRIM(TPFIELD%CMNHNAME))
END IF
!
IF (IGRID==0) THEN
  SELECT CASE(TPFIELD%NDIMS)
    CASE (0)
      IF (TPFIELD%NTYPE == TYPECHAR) THEN
        ILEN = KLEN
      ELSE
        ILEN = 1
      END IF
    CASE (1)
      PTDIM => IO_Dimcdf_get_nc4(TPFILE, int( KLEN, kind=CDFINT ) )
      TPDIMS(1) = PTDIM
      ILEN      = PTDIM%LEN
    CASE DEFAULT
      CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4','NGRID=0 and NDIMS>1 not yet supported (field '&
                     //TRIM(TPFIELD%CMNHNAME)//')')
  END SELECT
ELSE IF (TPFIELD%CLBTYPE/='NONE') THEN
  IF (TPFIELD%NDIMS/=3) THEN
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4','CLBTYPE/=NONE and NDIMS/=3 not supported (field '&
                     //TRIM(TPFIELD%CMNHNAME)//')')
  END IF
  !
  IF (TPFIELD%CLBTYPE=='LBX' .OR. TPFIELD%CLBTYPE=='LBXU') THEN
    PTDIM => TPFILE%TNCCOORDS(2,IGRID)%TDIM
    TPDIMS(2) = PTDIM
    PTDIM => TPFILE%TNCCOORDS(3,IGRID)%TDIM
    TPDIMS(3) = PTDIM
    ILEN = TPDIMS(2)%LEN * TPDIMS(3)%LEN
    ISIZE = KLEN/ILEN
    IF (MOD(KLEN,ILEN)/=0) CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4', &
                                              'can not guess 1st dimension for field '//TRIM(TPFIELD%CMNHNAME))
    PTDIM => IO_Dimcdf_get_nc4(TPFILE, ISIZE)
    TPDIMS(1) = PTDIM
    ILEN       = ILEN * PTDIM%LEN
  ELSE IF (TPFIELD%CLBTYPE=='LBY' .OR. TPFIELD%CLBTYPE=='LBYV') THEN
    PTDIM => TPFILE%TNCCOORDS(1,IGRID)%TDIM
    TPDIMS(1) = PTDIM
    PTDIM => TPFILE%TNCCOORDS(3,IGRID)%TDIM
    TPDIMS(3) = PTDIM
    ILEN = TPDIMS(1)%LEN * TPDIMS(3)%LEN
    ISIZE = KLEN/ILEN
    IF (MOD(KLEN,ILEN)/=0) CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4', &
                                              'can not guess 2nd dimension for field '//TRIM(TPFIELD%CMNHNAME))
    PTDIM => IO_Dimcdf_get_nc4(TPFILE, ISIZE)
    TPDIMS(2) = PTDIM
    ILEN       = ILEN * PTDIM%LEN
  ELSE
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4','invalid CLBTYPE ('//TPFIELD%CLBTYPE//') for field '&
                     //TRIM(TPFIELD%CMNHNAME))
  END IF
ELSE
  IF (TPFIELD%NDIMS==0) ILEN = 1
  !
  DO JI=1,TPFIELD%NDIMS
    IF (JI == 1) THEN
      IF ( (YDIR == 'XX' .OR. YDIR == 'XY') ) THEN
        PTDIM => TPFILE%TNCCOORDS(1,IGRID)%TDIM
      ELSE IF ( YDIR == 'YY' ) THEN
        PTDIM => TPFILE%TNCCOORDS(2,IGRID)%TDIM
      ELSE IF ( YDIR == 'ZZ' ) THEN
        PTDIM => TPFILE%TNCCOORDS(3,IGRID)%TDIM
      ELSE IF (JI==TPFIELD%NDIMS) THEN !Guess last dimension
        PTDIM => IO_Dimcdf_get_nc4(TPFILE, int( KLEN, kind=CDFINT ) )
      END IF
      ILEN       = PTDIM%LEN
      TPDIMS(JI) = PTDIM
    ELSE IF (JI == 2) THEN
      IF ( YDIR == 'XY') THEN
        PTDIM => TPFILE%TNCCOORDS(2,IGRID)%TDIM
      ELSE IF (JI==TPFIELD%NDIMS) THEN !Guess last dimension
        ISIZE = KLEN/ILEN
        IF (MOD(KLEN,ILEN)/=0) THEN
          CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4', &
                                            'can not guess 2nd and last dimension for field '//TRIM(TPFIELD%CMNHNAME))
          EXIT
        END IF
        PTDIM => IO_Dimcdf_get_nc4(TPFILE, ISIZE)
      ELSE
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4','can not guess 2nd dimension for field '//TRIM(TPFIELD%CMNHNAME))
        EXIT
      END IF
      ILEN       = ILEN * PTDIM%LEN
      TPDIMS(JI) = PTDIM
    ELSE IF (JI == 3) THEN
      IF ( YDIR == 'XY' ) THEN
        IF (JI==TPFIELD%NDIMS .AND. KLEN/ILEN==1 .AND. MOD(KLEN,ILEN)==0) THEN
          !The last dimension is of size 1 => probably time dimension
          ISIZE = 1
          PTDIM => IO_Dimcdf_get_nc4(TPFILE,ISIZE)
        ELSE
          PTDIM => TPFILE%TNCCOORDS(3,IGRID)%TDIM
        END IF
      ELSE IF (JI==TPFIELD%NDIMS) THEN !Guess last dimension
        ISIZE = KLEN/ILEN
        IF (MOD(KLEN,ILEN)/=0) THEN
          CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4', &
                                            'can not guess 3rd and last dimension for field '//TRIM(TPFIELD%CMNHNAME))
          EXIT
        END IF
        PTDIM => IO_Dimcdf_get_nc4(TPFILE, ISIZE)
      ELSE
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4','can not guess 3rd dimension for field '//TRIM(TPFIELD%CMNHNAME))
        EXIT
      END IF
      ILEN       = ILEN * PTDIM%LEN
      TPDIMS(JI) = PTDIM
    ELSE IF (JI==4 .AND. JI==TPFIELD%NDIMS) THEN !Guess last dimension
      ISIZE = KLEN/ILEN
      IF (MOD(KLEN,ILEN)/=0) THEN
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4', &
                                          'can not guess 4th and last dimension for field '//TRIM(TPFIELD%CMNHNAME))
        EXIT
      END IF
      PTDIM => IO_Dimcdf_get_nc4(TPFILE, ISIZE)
      ILEN       = ILEN * PTDIM%LEN
      TPDIMS(JI) = PTDIM
    ELSE
      CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4','can not guess dimension above 4 for field '&
                     //TRIM(TPFIELD%CMNHNAME))
    END IF
  END DO
END IF
!
IF (KLEN /= ILEN) THEN
  CALL PRINT_MSG(NVERB_INFO,'IO','IO_Dimids_guess_nc4','can not guess dimensions of field '&
                                   //TRIM(TPFIELD%CMNHNAME))
  KRESP = 1
END IF
!
END SUBROUTINE IO_Dimids_guess_nc4


SUBROUTINE IO_Knowndims_set_nc4(TPFILE,HPROGRAM_ORIG)

USE MODD_CONF,          ONLY: CPROGRAM
USE MODD_CONF_n,        ONLY: CSTORAGE_TYPE
USE MODD_DIM_n,         ONLY: NIMAX_ll, NJMAX_ll, NKMAX
USE MODD_PARAMETERS_ll, ONLY: JPHEXT, JPVEXT

TYPE(TFILEDATA),INTENT(INOUT)        :: TPFILE
CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: HPROGRAM_ORIG !To emulate a file coming from this program

CHARACTER(LEN=:),ALLOCATABLE :: YPROGRAM
INTEGER                      :: IIU_ll, IJU_ll, IKU
TYPE(IOCDF), POINTER         :: PIOCDF

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Knowndims_set_nc4','called for '//TRIM(TPFILE%CNAME))

PIOCDF => TPFILE%TNCDIMS

IF (PRESENT(HPROGRAM_ORIG)) THEN
  YPROGRAM = HPROGRAM_ORIG
ELSE
  YPROGRAM = CPROGRAM
ENDIF

IIU_ll = NIMAX_ll + 2*JPHEXT
IJU_ll = NJMAX_ll + 2*JPHEXT
IKU    = NKMAX    + 2*JPVEXT

IF (.NOT. ASSOCIATED(PIOCDF%DIM_NI))      PIOCDF%DIM_NI      => IO_Dimcdf_get_nc4(TPFILE, int( IIU_ll, kind=CDFINT ), 'ni')
IF (.NOT. ASSOCIATED(PIOCDF%DIM_NJ))      PIOCDF%DIM_NJ      => IO_Dimcdf_get_nc4(TPFILE, int( IJU_ll, kind=CDFINT ), 'nj')
IF (.NOT. ASSOCIATED(PIOCDF%DIM_NI_U))    PIOCDF%DIM_NI_U    => IO_Dimcdf_get_nc4(TPFILE, int( IIU_ll, kind=CDFINT ), 'ni_u')
IF (.NOT. ASSOCIATED(PIOCDF%DIM_NJ_U))    PIOCDF%DIM_NJ_U    => IO_Dimcdf_get_nc4(TPFILE, int( IJU_ll, kind=CDFINT ), 'nj_u')
IF (.NOT. ASSOCIATED(PIOCDF%DIM_NI_V))    PIOCDF%DIM_NI_V    => IO_Dimcdf_get_nc4(TPFILE, int( IIU_ll, kind=CDFINT ), 'ni_v')
IF (.NOT. ASSOCIATED(PIOCDF%DIM_NJ_V))    PIOCDF%DIM_NJ_V    => IO_Dimcdf_get_nc4(TPFILE, int( IJU_ll, kind=CDFINT ), 'nj_v')
IF (TRIM(YPROGRAM)/='PGD' .AND. TRIM(YPROGRAM)/='NESPGD' .AND. TRIM(YPROGRAM)/='ZOOMPG' &
    .AND. .NOT.(TRIM(YPROGRAM)=='REAL' .AND. CSTORAGE_TYPE=='SU') ) THEN !condition to detect PREP_SURFEX
  IF (.NOT. ASSOCIATED(PIOCDF%DIM_LEVEL))   PIOCDF%DIM_LEVEL   => IO_Dimcdf_get_nc4(TPFILE, int( IKU, kind=CDFINT ), 'level')
  IF (.NOT. ASSOCIATED(PIOCDF%DIM_LEVEL_W)) PIOCDF%DIM_LEVEL_W => IO_Dimcdf_get_nc4(TPFILE, int( IKU, kind=CDFINT ), 'level_w')
  IF (.NOT. ASSOCIATED(PIOCDF%DIMTIME)) PIOCDF%DIMTIME => IO_Dimcdf_get_nc4(TPFILE, NF90_UNLIMITED, 'time')
ELSE
  !PGD and SURFEX files for MesoNH have no vertical levels or time scale
  !These dimensions are allocated to default values
  !(they need to be allocated when looking for dimensions of variables)
  IF (.NOT. ASSOCIATED(PIOCDF%DIM_LEVEL))   ALLOCATE(PIOCDF%DIM_LEVEL)
  IF (.NOT. ASSOCIATED(PIOCDF%DIM_LEVEL_W)) ALLOCATE(PIOCDF%DIM_LEVEL_W)
  IF (.NOT. ASSOCIATED(PIOCDF%DIMTIME))     ALLOCATE(PIOCDF%DIMTIME)
END IF

!Store X,Y,Z coordinates for the Arakawa points
!0 2nd-dimension is to treat NGRID=0 case without crash
IF (.NOT.ALLOCATED(TPFILE%TNCCOORDS)) ALLOCATE(TPFILE%TNCCOORDS(3,0:8))
!Dummy point
TPFILE%TNCCOORDS(1,0)%TDIM => TDIM_DUMMY
TPFILE%TNCCOORDS(2,0)%TDIM => TDIM_DUMMY
TPFILE%TNCCOORDS(3,0)%TDIM => TDIM_DUMMY
! Mass point
TPFILE%TNCCOORDS(1,1)%TDIM => PIOCDF%DIM_NI
TPFILE%TNCCOORDS(2,1)%TDIM => PIOCDF%DIM_NJ
TPFILE%TNCCOORDS(3,1)%TDIM => PIOCDF%DIM_LEVEL
! u point
TPFILE%TNCCOORDS(1,2)%TDIM => PIOCDF%DIM_NI_U
TPFILE%TNCCOORDS(2,2)%TDIM => PIOCDF%DIM_NJ_U
TPFILE%TNCCOORDS(3,2)%TDIM => PIOCDF%DIM_LEVEL
! v point
TPFILE%TNCCOORDS(1,3)%TDIM => PIOCDF%DIM_NI_V
TPFILE%TNCCOORDS(2,3)%TDIM => PIOCDF%DIM_NJ_V
TPFILE%TNCCOORDS(3,3)%TDIM => PIOCDF%DIM_LEVEL
! w point
TPFILE%TNCCOORDS(1,4)%TDIM => PIOCDF%DIM_NI
TPFILE%TNCCOORDS(2,4)%TDIM => PIOCDF%DIM_NJ
TPFILE%TNCCOORDS(3,4)%TDIM => PIOCDF%DIM_LEVEL_W
! xi vorticity point (=f point =uv point)
TPFILE%TNCCOORDS(1,5)%TDIM => PIOCDF%DIM_NI_U
TPFILE%TNCCOORDS(2,5)%TDIM => PIOCDF%DIM_NJ_V
TPFILE%TNCCOORDS(3,5)%TDIM => PIOCDF%DIM_LEVEL
! eta vorticity point (=uw point)
TPFILE%TNCCOORDS(1,6)%TDIM => PIOCDF%DIM_NI_U
TPFILE%TNCCOORDS(2,6)%TDIM => PIOCDF%DIM_NJ_U
TPFILE%TNCCOORDS(3,6)%TDIM => PIOCDF%DIM_LEVEL_W
! zeta vorticity point (=vw point)
TPFILE%TNCCOORDS(1,7)%TDIM => PIOCDF%DIM_NI_V
TPFILE%TNCCOORDS(2,7)%TDIM => PIOCDF%DIM_NJ_V
TPFILE%TNCCOORDS(3,7)%TDIM => PIOCDF%DIM_LEVEL_W
! fw point (=uvw point)
TPFILE%TNCCOORDS(1,8)%TDIM => PIOCDF%DIM_NI_U
TPFILE%TNCCOORDS(2,8)%TDIM => PIOCDF%DIM_NJ_V
TPFILE%TNCCOORDS(3,8)%TDIM => PIOCDF%DIM_LEVEL_W


END SUBROUTINE IO_Knowndims_set_nc4


SUBROUTINE IO_Iocdf_dealloc_nc4(PIOCDF)
TYPE(IOCDF),  POINTER :: PIOCDF

INTEGER(KIND=CDFINT) :: IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Iocdf_dealloc_nc4','called')

! Clean DIMLIST and DIMSTR
CALL CLEANLIST(PIOCDF%DIMLIST)
CALL CLEANLIST(PIOCDF%DIMSTR)
! Then free iocdf
DEALLOCATE(PIOCDF)

CONTAINS

SUBROUTINE CLEANLIST(PLIST)
TYPE(DIMCDF), POINTER :: PLIST,TZDIMCUR, TZDIMNEXT

TZDIMCUR  => PLIST
DO WHILE(ASSOCIATED(TZDIMCUR))
   TZDIMNEXT => TZDIMCUR%NEXT
   DEALLOCATE(TZDIMCUR)
   TZDIMCUR => TZDIMNEXT
END DO

END SUBROUTINE CLEANLIST

END SUBROUTINE IO_Iocdf_dealloc_nc4


SUBROUTINE IO_Vdims_fill_nc4(TPFILE, TPFIELD, KSHAPE, KVDIMS)
TYPE(TFILEDATA),                              INTENT(IN)  :: TPFILE
TYPE(TFIELDDATA),                             INTENT(IN)  :: TPFIELD
INTEGER(KIND=CDFINT),DIMENSION(:),            INTENT(IN)  :: KSHAPE
INTEGER(KIND=CDFINT),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: KVDIMS
!
INTEGER               :: IGRID
INTEGER               :: JI
CHARACTER(LEN=32)     :: YINT
CHARACTER(LEN=2)      :: YDIR
TYPE(DIMCDF), POINTER :: PTDIM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Vdims_fill_nc4','called for '//TRIM(TPFIELD%CMNHNAME))
!
IF (SIZE(KSHAPE) < 1 .AND. .NOT.TPFIELD%LTIMEDEP) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Vdims_fill_nc4','empty KSHAPE')
!
IGRID  =  TPFIELD%NGRID
YDIR   =  TPFIELD%CDIR
!
IF(SIZE(KSHAPE)/=TPFIELD%NDIMS) THEN
  WRITE(YINT,'( I0,"/",I0 )') SIZE(KSHAPE),TPFIELD%NDIMS
  CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Vdims_fill_nc4','SIZE(KSHAPE)/=TPFIELD%NDIMS ('//TRIM(YINT)//') for field ' &
                 //TRIM(TPFIELD%CMNHNAME))
END IF
!
IF (TPFIELD%LTIMEDEP) THEN
  !Add time dimension
  ALLOCATE(KVDIMS(TPFIELD%NDIMS+1))
  KVDIMS(TPFIELD%NDIMS+1) = TPFILE%TNCDIMS%DIMTIME%ID
ELSE
  ALLOCATE(KVDIMS(TPFIELD%NDIMS))
END IF
!
IF(IGRID<0 .OR. IGRID>8) THEN
  WRITE(YINT,'( I0 )') IGRID
  CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Vdims_fill_nc4','invalid NGRID ('//TRIM(YINT)//') for field '//TRIM(TPFIELD%CMNHNAME))
END IF
!
IF(IGRID==0 .AND. YDIR/='--' .AND. YDIR/=''  ) THEN
  CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Vdims_fill_nc4','invalid YDIR ('//TRIM(YDIR)//') with NGRID=0 for field ' &
                 //TRIM(TPFIELD%CMNHNAME))
END IF
!
DO JI=1,SIZE(KSHAPE)
  IF (JI == 1) THEN
    IF ( (YDIR == 'XX' .OR. YDIR == 'XY') .AND. KSHAPE(1)==TPFILE%TNCCOORDS(1,IGRID)%TDIM%LEN) THEN
      KVDIMS(1) = TPFILE%TNCCOORDS(1,IGRID)%TDIM%ID
    ELSE IF ( YDIR == 'YY'                .AND. KSHAPE(1)==TPFILE%TNCCOORDS(2,IGRID)%TDIM%LEN) THEN
      KVDIMS(1) = TPFILE%TNCCOORDS(2,IGRID)%TDIM%ID
    ELSE IF ( YDIR == 'ZZ'                .AND. KSHAPE(1)==TPFILE%TNCCOORDS(3,IGRID)%TDIM%LEN) THEN
      KVDIMS(1) = TPFILE%TNCCOORDS(3,IGRID)%TDIM%ID
    ELSE
      PTDIM => IO_Dimcdf_get_nc4(TPFILE, KSHAPE(1)); KVDIMS(1) = PTDIM%ID
    END IF
  ELSE IF (JI == 2) THEN
    IF ( YDIR == 'XY' .AND. KSHAPE(2)==TPFILE%TNCCOORDS(2,IGRID)%TDIM%LEN) THEN
      KVDIMS(2) = TPFILE%TNCCOORDS(2,IGRID)%TDIM%ID
    ELSE
      PTDIM => IO_Dimcdf_get_nc4(TPFILE, KSHAPE(2)); KVDIMS(2) = PTDIM%ID
    END IF
  ELSE IF (JI == 3) THEN
    IF ( YDIR == 'XY' .AND. KSHAPE(3)==TPFILE%TNCCOORDS(3,IGRID)%TDIM%LEN) THEN
      KVDIMS(3) = TPFILE%TNCCOORDS(3,IGRID)%TDIM%ID
    ELSE
      PTDIM => IO_Dimcdf_get_nc4(TPFILE, KSHAPE(3)); KVDIMS(3) = PTDIM%ID
    END IF
  ELSE
      PTDIM => IO_Dimcdf_get_nc4(TPFILE, KSHAPE(JI)); KVDIMS(JI) = PTDIM%ID
  END IF
END DO
!
END SUBROUTINE IO_Vdims_fill_nc4


FUNCTION IO_Dimcdf_get_nc4(TPFILE, KLEN, HDIMNAME)
TYPE(TFILEDATA),            INTENT(IN) :: TPFILE
INTEGER(KIND=CDFINT),       INTENT(IN) :: KLEN
CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: HDIMNAME ! When provided don't search but
                                                   ! simply create with name HDIMNAME
TYPE(DIMCDF), POINTER   :: IO_Dimcdf_get_nc4

TYPE(DIMCDF), POINTER :: TMP
INTEGER               :: COUNT
CHARACTER(LEN=16)     :: YSUFFIX
CHARACTER(LEN=20)     :: YDIMNAME
INTEGER(KIND=CDFINT)  :: STATUS
LOGICAL               :: GCHKLEN !Check if KLEN is valid
TYPE(IOCDF), POINTER  :: PIOCDF

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Dimcdf_get_nc4','called')

PIOCDF => TPFILE%TNCDIMS

GCHKLEN = .TRUE.
!Do not check KLEN if 'time' (because NF90_UNLIMITED = 0)
IF (PRESENT(HDIMNAME)) THEN
  IF (TRIM(HDIMNAME)=='time') THEN
    GCHKLEN = .FALSE.
  END IF
END IF

WRITE(YSUFFIX,'(I0)') KLEN

IF (GCHKLEN .AND. KLEN < 1) THEN
  CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Dimcdf_get_nc4','KLEN='//TRIM(YSUFFIX))
END IF

IF (PRESENT(HDIMNAME)) THEN
   NULLIFY(TMP)
   YDIMNAME = TRIM(HDIMNAME)
ELSE
   ! Search dimension with KLEN length
   COUNT = 1
   TMP  => PIOCDF%DIMLIST
   DO WHILE(ASSOCIATED(TMP))
      IF (TMP%LEN == KLEN .AND. TMP%NAME(1:4) /= 'char') EXIT
      TMP=>TMP%NEXT
      COUNT = COUNT+1
   END DO
   YDIMNAME = 'size'//TRIM(YSUFFIX)
END IF

IF (.NOT. ASSOCIATED(TMP)) THEN
   ! Not found then define new dimension
   ALLOCATE(TMP)
   TMP%NAME = YDIMNAME
   TMP%LEN = KLEN
   STATUS = NF90_DEF_DIM(TPFILE%NNCID, TMP%NAME, KLEN, TMP%ID)
   IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Dimcdf_get_nc4','NF90_DEF_DIM',trim(TMP%NAME))
   NULLIFY(TMP%NEXT)
   TMP%NEXT       => PIOCDF%DIMLIST
   PIOCDF%DIMLIST => TMP
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Dimcdf_get_nc4','new dimension: '//TRIM(TMP%NAME))
END IF

IO_Dimcdf_get_nc4 => TMP

END FUNCTION IO_Dimcdf_get_nc4


FUNCTION IO_Strdimid_get_nc4(TPFILE,KLEN)
TYPE(TFILEDATA),      INTENT(IN) :: TPFILE
INTEGER(KIND=CDFINT), INTENT(IN) :: KLEN
INTEGER(KIND=CDFINT)             :: IO_Strdimid_get_nc4

TYPE(DIMCDF), POINTER :: TMP
TYPE(IOCDF),  POINTER :: TZIOCDF
CHARACTER(LEN=16)     :: YSUFFIX
INTEGER(KIND=CDFINT)  :: STATUS

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Strdimid_get_nc4','called')

WRITE(YSUFFIX,'(I0)') KLEN

IF (KLEN < 1) THEN
  CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Strdimid_get_nc4','KLEN='//TRIM(YSUFFIX))
END IF

! Search string dimension with KLEN length
TMP  => TPFILE%TNCDIMS%DIMSTR
DO WHILE(ASSOCIATED(TMP))
   IF (TMP%LEN == KLEN) EXIT
   TMP=>TMP%NEXT
END DO

IF (.NOT. ASSOCIATED(TMP)) THEN
   ! Not found then define new dimension
   ALLOCATE(TMP)
   TMP%NAME = 'char'//TRIM(YSUFFIX)
   TMP%LEN = KLEN
   STATUS = NF90_DEF_DIM(TPFILE%NNCID, TMP%NAME, KLEN, TMP%ID)
   IF (STATUS /= NF90_NOERR) CALL IO_Err_handle_nc4(status,'IO_Strdimid_get_nc4','NF90_DEF_DIM',trim(TMP%NAME))
   NULLIFY(TMP%NEXT)
   TMP%NEXT      => TPFILE%TNCDIMS%DIMSTR
   TZIOCDF => TPFILE%TNCDIMS
   TZIOCDF%DIMSTR => TMP
END IF

IO_Strdimid_get_nc4 = TMP%ID

END FUNCTION IO_Strdimid_get_nc4


FUNCTION IO_Iocdf_alloc_nc4()
TYPE(IOCDF), POINTER :: IO_Iocdf_alloc_nc4
TYPE(IOCDF), POINTER :: TZIOCDF
INTEGER              :: IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Iocdf_alloc_nc4','called')

ALLOCATE(TZIOCDF, STAT=IRESP)
IF (IRESP > 0) THEN
  CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Iocdf_alloc_nc4','memory allocation error')
END IF

IO_Iocdf_alloc_nc4=>TZIOCDF

END FUNCTION IO_Iocdf_alloc_nc4


subroutine IO_Err_handle_nc4(kstatus,hsubr,hncsubr,hvar,kresp)
integer(kind=CDFINT), intent(in)  :: kstatus
character(len=*),     intent(in)  :: hsubr
character(len=*),     intent(in)  :: hncsubr
character(len=*),     intent(in)  :: hvar
integer, optional,    intent(out) :: kresp

! Don't stop (by default) the code when kresp is present
! and ensure kresp is a negative integer
if (kstatus /= NF90_NOERR) then
  if (present(kresp)) then
    if (kstatus < 0) then
      kresp = kstatus
    else
      kresp = -kstatus
    end if
    call print_msg(NVERB_WARNING,'IO',trim(hsubr),trim(hvar)//': '//trim(hncsubr)//': '//trim(NF90_STRERROR(kstatus)))
  else
    call print_msg(NVERB_ERROR,  'IO',trim(hsubr),trim(hvar)//': '//trim(hncsubr)//': '//trim(NF90_STRERROR(kstatus)))
  end if
end if
end subroutine IO_Err_handle_nc4


SUBROUTINE IO_Mnhname_clean(HINNAME,HOUTNAME)
  CHARACTER(LEN=*),INTENT(IN)  :: HINNAME
  CHARACTER(LEN=*),INTENT(OUT) :: HOUTNAME

  ! NetCDF var names can't contain '%' nor '.'
  ! CF convention allows only letters, digits and underscores
  HOUTNAME = str_replace(HINNAME,  '%', '__')
  HOUTNAME = str_replace(HOUTNAME, '.', '___')
END SUBROUTINE


FUNCTION str_replace(hstr, hold, hnew)
CHARACTER(LEN=*) :: hstr, hold, hnew
CHARACTER(LEN=LEN_TRIM(hstr)+MAX(0,LEN(hnew)-LEN(hold))) :: str_replace

INTEGER :: pos

pos = INDEX(hstr,hold)
IF (pos /= 0) THEN
   str_replace = hstr(1:pos-1)//hnew//hstr(pos+LEN(hold):)
ELSE
   str_replace = hstr
END IF

END FUNCTION str_replace


end module mode_io_tools_nc4
#else
!
! External dummy subroutines
!
subroutine IO_Dim_find_byname_nc4(a, b, c, d)
use mode_msg
integer :: a, b, c, d
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Dim_find_byname_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine IO_Dim_find_byname_nc4
!
subroutine IO_Dimids_guess_nc4(a, b, c, d)
use mode_msg
integer :: a, b, c, d
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Dimids_guess_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine IO_Dimids_guess_nc4
!
subroutine IO_Knowndims_set_nc4(a, b)
use mode_msg
integer :: a, b,
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Knowndims_set_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine IO_Knowndims_set_nc4
!
subroutine IO_Iocdf_dealloc_nc4(a)
use mode_msg
integer :: a
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Iocdf_dealloc_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine IO_Iocdf_dealloc_nc4
!
subroutine IO_Mnhname_clean(a, b)
use mode_msg
integer :: a, b
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Mnhname_clean','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine IO_Mnhname_clean
!
subroutine IO_Vdims_fill_nc4(a, b, c, d)
use mode_msg
integer :: a, b, c, d
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Vdims_fill_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine IO_Vdims_fill_nc4
!
function IO_Dimcdf_get_nc4(a, b, c)
use mode_msg
integer :: IO_Dimcdf_get_nc4
integer :: a, b, c
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Dimcdf_get_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end function IO_Dimcdf_get_nc4
!
function IO_Strdimid_get_nc4(a, b)
use mode_msg
integer :: IO_Strdimid_get_nc4
integer :: a, b
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Strdimid_get_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end function IO_Strdimid_get_nc4
!
subroutine IO_Err_handle_nc4(a, b, c, d, e)
use mode_msg
integer :: a, b, c, d, e
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Err_handle_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine IO_Err_handle_nc4
!
function IO_Iocdf_alloc_nc4()
use mode_msg
integer :: IO_Iocdf_alloc_nc4
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Iocdf_alloc_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end function IO_Iocdf_alloc_nc4()
!
#endif
