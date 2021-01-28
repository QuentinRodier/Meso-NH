!MNH_LIC Copyright 1994-2020 CNRS, Meteo-France and Universite Paul Sabatier
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
!  P. Wautelet 14/09/2020: IO_Knowndims_set_nc4: add new dimensions + remove 'time' dimension in diachronic files
!  P. Wautelet 14/09/2020: IO_Vdims_fill_nc4: use ndimlist when provided to fill dimensions ids
!  P. Wautelet 10/11/2020: new data structures for netCDF dimensions
!  P. Wautelet 26/11/2020: IO_Vdims_fill_nc4: support for empty kshape
!  P. Wautelet 08/12/2020: add nbutotwrite
!-----------------------------------------------------------------
#ifdef MNH_IOCDF4
module mode_io_tools_nc4

use modd_field,     only: tfielddata
use modd_io,        only: tfiledata
use modd_netcdf,    only: tdimnc, tdimsnc
use modd_precision, only: CDFINT

use mode_msg

use NETCDF,      only: NF90_NOERR, NF90_UNLIMITED, &
                       NF90_DEF_DIM, NF90_STRERROR

implicit none

private

public :: IO_Dim_find_byname_nc4, IO_Dimids_guess_nc4, IO_Knowndims_set_nc4
public :: IO_Iocdf_alloc_nc4, IO_Iocdf_dealloc_nc4, IO_Mnhname_clean
public :: IO_Dim_find_create_nc4, IO_Strdimid_get_nc4, IO_Vdims_fill_nc4, IO_Err_handle_nc4

contains

subroutine IO_Dim_find_byname_nc4( tpfile, hdimname, tpdim, kresp )
type(tfiledata),  intent(in)  :: tpfile
character(len=*), intent(in)  :: hdimname
type(tdimnc),     intent(out) :: tpdim
integer,          intent(out) :: kresp

integer :: ji

call Print_msg( NVERB_DEBUG, 'IO', 'IO_Dim_find_byname_nc4', 'called for dimension name ' // Trim( hdimname ) )

kresp = -2

if ( .not.Associated( tpfile%tncdims ) ) then
  call Print_msg( NVERB_ERROR, 'IO', 'IO_Dim_find_byname_nc4', 'tncdims not associated for file  '//trim(tpfile%cname) )
  kresp = -3
  return
end if

if ( .not.Allocated( tpfile%tncdims%tdims ) ) then
  call Print_msg( NVERB_WARNING, 'IO', 'IO_Dim_find_byname_nc4', 'tdims not allocated for file  '//trim(tpfile%cname) )
  kresp = -1
  return
end if

do ji = 1, tpfile%tncdims%nmaxdims
  if ( Trim( hdimname ) == Trim( tpfile%tncdims%tdims(ji)%cname ) ) then
    tpdim = tpfile%tncdims%tdims(ji)
    kresp = 0
    exit
  end if
end do

end subroutine IO_Dim_find_byname_nc4


SUBROUTINE IO_Dimids_guess_nc4(TPFILE, TPFIELD, KLEN, TPDIMS, KRESP)
!
USE MODD_FIELD, ONLY: NMNHDIM_ARAKAWA, TYPECHAR
!
!Used by LFI2CDF
TYPE(TFILEDATA),                      INTENT(IN)  :: TPFILE
TYPE(TFIELDDATA),                     INTENT(IN)  :: TPFIELD
INTEGER,                              INTENT(IN)  :: KLEN
TYPE(tdimnc),DIMENSION(:),            INTENT(OUT) :: TPDIMS
INTEGER,                              INTENT(OUT) :: KRESP
!
INTEGER               :: IGRID
integer               :: iidx
INTEGER(kind=CDFINT)  :: ILEN, ISIZE
INTEGER               :: JI
CHARACTER(LEN=32)     :: YINT
CHARACTER(LEN=2)      :: YDIR
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Dimids_guess_nc4','called for '//TRIM(TPFIELD%CMNHNAME))
!
IGRID  =  TPFIELD%NGRID
YDIR   =  TPFIELD%CDIR
!
KRESP = 0
ILEN = 0
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
      call IO_Dim_find_create_nc4( tpfile, klen, iidx )
      tpdims(1) = tpfile%tncdims%tdims(iidx)
      ilen = tpdims(1)%nlen
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
    tpdims(2) = tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,2) )
    tpdims(3) = tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,3) )
    ilen = tpdims(2)%nlen * tpdims(3)%nlen
    ISIZE = KLEN/ILEN
    IF (MOD(KLEN,ILEN)/=0) CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4', &
                                              'can not guess 1st dimension for field '//TRIM(TPFIELD%CMNHNAME))
    call IO_Dim_find_create_nc4( tpfile, isize, iidx )
    tpdims(1) = tpfile%tncdims%tdims(iidx)
    ilen = ilen * tpdims(1)%nlen
  ELSE IF (TPFIELD%CLBTYPE=='LBY' .OR. TPFIELD%CLBTYPE=='LBYV') THEN
    tpdims(1) = tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,1) )
    tpdims(3) = tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,3) )
    ilen = tpdims(1)%nlen * tpdims(3)%nlen
    ISIZE = KLEN/ILEN
    IF (MOD(KLEN,ILEN)/=0) CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4', &
                                              'can not guess 2nd dimension for field '//TRIM(TPFIELD%CMNHNAME))
    call IO_Dim_find_create_nc4( tpfile, isize, iidx )
    tpdims(2) = tpfile%tncdims%tdims(iidx)
    ilen = ilen * tpdims(2)%nlen
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
        tpdims(1) = tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,1) )
      ELSE IF ( YDIR == 'YY' ) THEN
        tpdims(1) = tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,2) )
      ELSE IF ( YDIR == 'ZZ' ) THEN
        tpdims(1) = tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,3) )
      ELSE IF (JI==TPFIELD%NDIMS) THEN !Guess last dimension
        call IO_Dim_find_create_nc4( tpfile, klen, iidx )
        tpdims(1) = tpfile%tncdims%tdims(iidx)
      END IF
      ilen = tpdims(1)%nlen
    ELSE IF (JI == 2) THEN
      IF ( YDIR == 'XY') THEN
        tpdims(2) = tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,2) )
      ELSE IF (JI==TPFIELD%NDIMS) THEN !Guess last dimension
        ISIZE = KLEN/ILEN
        IF (MOD(KLEN,ILEN)/=0) THEN
          CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4', &
                                            'can not guess 2nd and last dimension for field '//TRIM(TPFIELD%CMNHNAME))
          EXIT
        END IF
        call IO_Dim_find_create_nc4( tpfile, isize, iidx )
        tpdims(2) = tpfile%tncdims%tdims(iidx)
      ELSE
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4','can not guess 2nd dimension for field '//TRIM(TPFIELD%CMNHNAME))
        EXIT
      END IF
      ilen = ilen * tpdims(2)%nlen
    ELSE IF (JI == 3) THEN
      IF ( YDIR == 'XY' ) THEN
        IF (JI==TPFIELD%NDIMS .AND. KLEN/ILEN==1 .AND. MOD(KLEN,ILEN)==0) THEN
          !The last dimension is of size 1 => probably time dimension
          ISIZE = 1
          call IO_Dim_find_create_nc4( tpfile, isize, iidx )
          tpdims(3) = tpfile%tncdims%tdims(iidx)
        ELSE
          tpdims(3) = tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,3) )
        END IF
      ELSE IF (JI==TPFIELD%NDIMS) THEN !Guess last dimension
        ISIZE = KLEN/ILEN
        IF (MOD(KLEN,ILEN)/=0) THEN
          CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4', &
                                            'can not guess 3rd and last dimension for field '//TRIM(TPFIELD%CMNHNAME))
          EXIT
        END IF
        call IO_Dim_find_create_nc4( tpfile, isize, iidx )
        tpdims(3) = tpfile%tncdims%tdims(iidx)
      ELSE
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4','can not guess 3rd dimension for field '//TRIM(TPFIELD%CMNHNAME))
        EXIT
      END IF
      ilen = ilen * tpdims(3)%nlen
    ELSE IF (JI==4 .AND. JI==TPFIELD%NDIMS) THEN !Guess last dimension
      ISIZE = KLEN/ILEN
      IF (MOD(KLEN,ILEN)/=0) THEN
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Dimids_guess_nc4', &
                                          'can not guess 4th and last dimension for field '//TRIM(TPFIELD%CMNHNAME))
        EXIT
      END IF
      call IO_Dim_find_create_nc4( tpfile, isize, iidx )
      tpdims(4) = tpfile%tncdims%tdims(iidx)
      ilen = ilen * tpdims(4)%nlen
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

use modd_budget,        only: cbutype, lbu_icp, lbu_jcp, lbu_kcp, nbuimax_ll, nbujmax_ll, nbukmax, nbumask, nbutotwrite
use modd_lbc_n,         only: clbcx, clbcy
USE MODD_CONF,          ONLY: CPROGRAM, l2d, lpack
USE MODD_CONF_n,        ONLY: CSTORAGE_TYPE
USE MODD_DIM_n,         ONLY: NIMAX_ll, NJMAX_ll, NKMAX
use modd_dyn,           only: xseglen
use modd_dyn_n,         only: xtstep
use modd_field,         only: NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_NI_U, NMNHDIM_NJ_U, NMNHDIM_NI_V, NMNHDIM_NJ_V,   &
                              NMNHDIM_LEVEL, NMNHDIM_LEVEL_W, NMNHDIM_TIME,                                     &
                              NMNHDIM_ONE,  NMNHDIM_COMPLEX,                                                    &
                              NMNHDIM_BUDGET_CART_NI, NMNHDIM_BUDGET_CART_NJ, NMNHDIM_BUDGET_CART_NI_U,         &
                              NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NI_V, NMNHDIM_BUDGET_CART_NJ_V,     &
                              NMNHDIM_BUDGET_CART_LEVEL, NMNHDIM_BUDGET_CART_LEVEL_W,                           &
                              NMNHDIM_BUDGET_MASK_LEVEL, NMNHDIM_BUDGET_MASK_LEVEL_W,                           &
                              NMNHDIM_BUDGET_MASK_NBUMASK, NMNHDIM_BUDGET_TIME,                                 &
                              NMNHDIM_BUDGET_LES_TIME, NMNHDIM_BUDGET_LES_AVG_TIME, NMNHDIM_BUDGET_LES_LEVEL,   &
                              NMNHDIM_BUDGET_LES_SV,                                                            &
                              NMNHDIM_SPECTRA_2PTS_NI, NMNHDIM_SPECTRA_2PTS_NJ,                                 &
                              NMNHDIM_SPECTRA_SPEC_NI, NMNHDIM_SPECTRA_SPEC_NJ, NMNHDIM_SPECTRA_LEVEL,          &
                              NMNHDIM_SERIES_LEVEL, NMNHDIM_SERIES_LEVEL_W,                                     &
                              NMNHDIM_SERIES_TIME, NMNHDIM_PROFILER_TIME, NMNHDIM_STATION_TIME,                 &
                              NMNHDIM_PAIR,                                                                     &
                              NMNHDIM_ARAKAWA,                                                                  &
                              NMNHDIM_LASTDIM_NODIACHRO, NMNHDIM_LASTDIM_DIACHRO

use modd_les,           only: nles_k, nspectra_k, xles_temp_mean_start, xles_temp_mean_step, xles_temp_mean_end
use modd_les_n,         only: nles_times, nspectra_ni, nspectra_nj
use modd_nsv,           only: nsv
USE MODD_PARAMETERS_ll, ONLY: JPHEXT, JPVEXT
use modd_profiler_n,    only: numbprofiler, tprofiler
use modd_series,        only: lseries
use modd_series_n,      only: nsnbstept
use modd_station_n,     only: numbstat, tstation

TYPE(TFILEDATA),INTENT(INOUT)        :: TPFILE
CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: HPROGRAM_ORIG !To emulate a file coming from this program

CHARACTER(LEN=:),ALLOCATABLE :: YPROGRAM
integer                      :: iavg, iprof, istation
integer                      :: ispectra_ni, ispectra_nj
INTEGER                      :: IIU_ll, IJU_ll, IKU

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Knowndims_set_nc4','called for '//TRIM(TPFILE%CNAME))

IF (PRESENT(HPROGRAM_ORIG)) THEN
  YPROGRAM = HPROGRAM_ORIG
ELSE
  YPROGRAM = CPROGRAM
ENDIF

IIU_ll = NIMAX_ll + 2*JPHEXT
IJU_ll = NJMAX_ll + 2*JPHEXT
IKU    = NKMAX    + 2*JPVEXT

if ( .not.Associated( tpfile%tncdims ) ) then
  call Print_msg( NVERB_ERROR, 'IO', 'IO_Knowndims_set_nc4', 'tncdims not associated for ' // Trim( tpfile%cname ) )
  Allocate( tpfile%tncdims )
end if

if ( Allocated( tpfile%tncdims%tdims ) ) then
  call Print_msg( NVERB_ERROR, 'IO', 'IO_Knowndims_set_nc4', 'tdims already allocated for ' // Trim( tpfile%cname ) )
  Deallocate( tpfile%tncdims%tdims )
end if

if ( tpfile%ctype /= 'MNHDIACHRONIC' ) then
  tpfile%tncdims%nmaxdims = NMNHDIM_LASTDIM_NODIACHRO
  Allocate( tpfile%tncdims%tdims(NMNHDIM_LASTDIM_NODIACHRO) )
else
  tpfile%tncdims%nmaxdims = NMNHDIM_LASTDIM_DIACHRO
  Allocate( tpfile%tncdims%tdims(NMNHDIM_LASTDIM_DIACHRO) )
end if

call IO_Add_dim_nc4( tpfile, NMNHDIM_NI,   'ni',   IIU_ll )
call IO_Add_dim_nc4( tpfile, NMNHDIM_NJ,   'nj',   IJU_ll )
call IO_Add_dim_nc4( tpfile, NMNHDIM_NI_U, 'ni_u', IIU_ll )
call IO_Add_dim_nc4( tpfile, NMNHDIM_NJ_U, 'nj_u', IJU_ll )
call IO_Add_dim_nc4( tpfile, NMNHDIM_NI_V, 'ni_v', IIU_ll )
call IO_Add_dim_nc4( tpfile, NMNHDIM_NJ_V, 'nj_v', IJU_ll )
if ( Trim( yprogram ) /= 'PGD' .and. Trim( yprogram ) /= 'NESPGD' .and. Trim( yprogram ) /= 'ZOOMPG' &
     .and. .not. ( Trim( yprogram ) == 'REAL' .and. cstorage_type == 'SU' ) ) then !condition to detect PREP_SURFEX
  call IO_Add_dim_nc4( tpfile, NMNHDIM_LEVEL,   'level',   IKU )
  call IO_Add_dim_nc4( tpfile, NMNHDIM_LEVEL_W, 'level_w', IKU )
  if ( tpfile%ctype /= 'MNHDIACHRONIC' ) &
    call IO_Add_dim_nc4( tpfile, NMNHDIM_TIME, 'time', NF90_UNLIMITED )
end if

if ( tpfile%ctype == 'MNHDIACHRONIC' .or. ( lpack .and. l2d ) ) then
  !Dimension of size 1 used for NMNHDIM_ONE
  call IO_Add_dim_nc4( tpfile, NMNHDIM_ONE, 'one', 1 )
end if

!Write dimensions used in diachronic files
if ( tpfile%ctype == 'MNHDIACHRONIC' ) then
  !Dimension of size 2 used for NMNHDIM_COMPLEX
  call IO_Add_dim_nc4( tpfile, NMNHDIM_COMPLEX, 'real_imaginary', 2 )

  !Dimension pair is used ie for boundaries of time intervals
  call IO_Add_dim_nc4( tpfile, NMNHDIM_PAIR,    'pair',           2 )

  !Dimensions for the budgets masks
  if ( cbutype == 'CART' .or. cbutype == 'SKIP' ) then
    if ( .not. lbu_icp )   call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_CART_NI,      'cart_ni',      nbuimax_ll  )
    if ( .not. lbu_jcp )   call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_CART_NJ,      'cart_nj',      nbujmax_ll  )
    if ( .not. lbu_icp )   call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_CART_NI_U,    'cart_ni_u',    nbuimax_ll  )
    if ( .not. lbu_jcp )   call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_CART_NJ_U,    'cart_nj_u',    nbujmax_ll  )
    if ( .not. lbu_icp )   call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_CART_NI_V,    'cart_ni_v',    nbuimax_ll  )
    if ( .not. lbu_jcp )   call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_CART_NJ_V,    'cart_nj_v',    nbujmax_ll  )
    if ( .not. lbu_kcp )   call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_CART_LEVEL,   'cart_level',   nbukmax     )
    if ( .not. lbu_kcp )   call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_CART_LEVEL_W, 'cart_level_w', nbukmax     )
    if ( nbutotwrite > 0 ) call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_TIME,         'time_budget',  nbutotwrite )
  else if ( cbutype == 'MASK' ) then
    if ( .not. lbu_kcp )   call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_MASK_LEVEL,   'mask_level',   nbukmax     )
    if ( .not. lbu_kcp )   call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_MASK_LEVEL_W, 'mask_level_w', nbukmax     )
    if ( nbutotwrite > 0 ) call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_TIME,         'time_budget',  nbutotwrite )
    if ( nbumask > 0 )     call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_MASK_NBUMASK, 'nbumask',      nbumask     )
  end if

  !Dimension for the number of LES budget time samplings
  if ( nles_times > 0 ) call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_LES_TIME, 'time_les', nles_times )

  !Dimension for the number of LES budget time averages
  iavg = int( xles_temp_mean_end - 1.e-10 - xles_temp_mean_start ) / xles_temp_mean_step + 1
  !Condition also on nles_times to not create this dimension when not used (no time average if nles_times=0)
  if ( nles_times > 0 .and. iavg > 0 ) &
    call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_LES_AVG_TIME, 'time_les_avg', iavg )

  !Dimension for the number of vertical levels for local LES budgets
  if ( nles_k > 0 ) call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_LES_LEVEL, 'level_les', nles_k )

  !Dimension for the number of scalar variables
  if ( nsv > 0 ) call IO_Add_dim_nc4( tpfile, NMNHDIM_BUDGET_LES_SV, 'nsv', nsv )

  !Dimensions for the number of horizontal wavelengths for non-local LES budgets (2 points correlations)
  if ( nspectra_ni > 0 ) call IO_Add_dim_nc4( tpfile, NMNHDIM_SPECTRA_2PTS_NI, 'nspectra_2pts_ni', nspectra_ni )
  if ( nspectra_nj > 0 .and. .not. l2d ) &
    call IO_Add_dim_nc4( tpfile, NMNHDIM_SPECTRA_2PTS_NJ, 'nspectra_2pts_nj', nspectra_nj )

  !Dimensions for the number of horizontal wavelengths for LES spectra budgets
  if ( nspectra_ni > 0 ) then
    if ( clbcx(1) == 'CYCL' ) then
      ispectra_ni = ( nspectra_ni + 1 ) / 2 - 1
    else
      ispectra_ni =  nspectra_ni - 1
    end if
    if ( ispectra_ni == 0 ) call Print_msg( NVERB_WARNING, 'IO', 'IO_Knowndims_set_nc4', &
                                            'nspectra_spec_ni dimension is zero sized for ' // Trim( tpfile%cname ) )
    call IO_Add_dim_nc4( tpfile, NMNHDIM_SPECTRA_SPEC_NI, 'nspectra_spec_ni', ispectra_ni )
  end if

  if ( nspectra_nj > 0 .and. .not. l2d ) then
    if ( clbcy(1) == 'CYCL' ) then
      ispectra_nj = ( nspectra_nj + 1 ) / 2 - 1
    else
      ispectra_nj =  nspectra_nj - 1
    end if
    if ( ispectra_nj == 0 ) call Print_msg( NVERB_WARNING, 'IO', 'IO_Knowndims_set_nc4', &
                                            'nspectra_spec_nj dimension is zero sized for ' // Trim( tpfile%cname ) )
    call IO_Add_dim_nc4( tpfile, NMNHDIM_SPECTRA_SPEC_NJ, 'nspectra_spec_nj', ispectra_nj )
  end if

  !Dimension for the number of vertical levels for non-local LES budgets
  if ( nspectra_k > 0 ) call IO_Add_dim_nc4( tpfile, NMNHDIM_SPECTRA_LEVEL, 'nspectra_level', nspectra_k )

  !Dimension for the number of profiler times
  if ( numbprofiler > 0 ) then
    iprof = Int ( ( xseglen - xtstep ) / tprofiler%step ) + 1
    call IO_Add_dim_nc4( tpfile, NMNHDIM_PROFILER_TIME, 'time_profiler', iprof )
  end if

  !Dimension for the number of station times
  if ( numbstat > 0 ) then
    istation = Int ( ( xseglen - xtstep ) / tstation%step ) + 1
    call IO_Add_dim_nc4( tpfile, NMNHDIM_STATION_TIME, 'time_station', istation )
  end if

  !Dimension for the number of series times
  if ( lseries .and. nsnbstept > 0 ) then
    call IO_Add_dim_nc4( tpfile, NMNHDIM_SERIES_LEVEL,   'series_level',   nkmax     )
    call IO_Add_dim_nc4( tpfile, NMNHDIM_SERIES_LEVEL_W, 'series_level_w', nkmax     )
    call IO_Add_dim_nc4( tpfile, NMNHDIM_SERIES_TIME,    'time_series',    nsnbstept )
  end if
end if

END SUBROUTINE IO_Knowndims_set_nc4


subroutine IO_Add_dim_nc4( tpfile, kidx, hdimname, klen )
use NETCDF, only: NF90_DEF_DIM, NF90_NOERR

type(tfiledata),  intent(inout) :: tpfile
integer,          intent(in)    :: kidx     !Position of the dimension in the list
character(len=*), intent(in)    :: hdimname !Name of the dimension
integer,          intent(in)    :: klen     !Length of the dimension

integer(kind=CDFINT)          :: istatus


if ( .not.Associated( tpfile%tncdims ) ) &
  call Print_msg( NVERB_FATAL, 'IO', 'IO_Add_dim_nc4', 'tncdims not associated for ' // Trim( tpfile%cname ) )

if ( kidx < 1 .or. kidx > Size( tpfile%tncdims%tdims ) )                                                      &
  call Print_msg( NVERB_FATAL, 'IO', 'IO_Add_dim_nc4', 'index out of range for dimension ' // Trim( hdimname ) // &
                  ' of file ' //Trim( tpfile%cname ) )

if ( tpfile%tncdims%tdims(kidx)%nlen /= -1 .or. tpfile%tncdims%tdims(kidx)%nid /= -1 ) &
  call Print_msg( NVERB_WARNING, 'IO', 'IO_Add_dim_nc4', 'dimension ' // Trim( hdimname ) //   &
                  ' already defined for file ' //Trim( tpfile%cname ) )

tpfile%tncdims%tdims(kidx)%cname = hdimname
tpfile%tncdims%tdims(kidx)%nlen  = Int( klen, kind = CDFINT )

istatus = NF90_DEF_DIM( tpfile%nncid, Trim( hdimname ), Int( klen, kind = CDFINT ), tpfile%tncdims%tdims(kidx)%nid )
if ( istatus /= NF90_NOERR ) &
  call IO_Err_handle_nc4( istatus, 'IO_Add_dim_nc4', 'NF90_DEF_DIM', Trim( hdimname ) )

end subroutine IO_Add_dim_nc4


SUBROUTINE IO_Iocdf_dealloc_nc4(tpdimsnc)
TYPE(tdimsnc),  POINTER :: tpdimsnc

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Iocdf_dealloc_nc4','called')

if ( Allocated( tpdimsnc%tdims     ) ) deallocate( tpdimsnc%tdims     )
if ( Allocated( tpdimsnc%tdims_str ) ) deallocate( tpdimsnc%tdims_str )

deallocate( tpdimsnc )
tpdimsnc => Null()

END SUBROUTINE IO_Iocdf_dealloc_nc4


SUBROUTINE IO_Vdims_fill_nc4(TPFILE, TPFIELD, KSHAPE, KVDIMS)

use NETCDF, only: NF90_INQ_DIMID, NF90_INQUIRE_DIMENSION

use modd_field,  only: NMNHDIM_UNKNOWN, NMNHDIM_ONE, NMNHDIM_COMPLEX,                                   &
                       NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_NI_U, NMNHDIM_NJ_U, NMNHDIM_NI_V, NMNHDIM_NJ_V,  &
                       NMNHDIM_LEVEL, NMNHDIM_LEVEL_W, NMNHDIM_TIME,                                    &
                       NMNHDIM_BUDGET_CART_NI,      NMNHDIM_BUDGET_CART_NJ,  NMNHDIM_BUDGET_CART_NI_U,  &
                       NMNHDIM_BUDGET_CART_NJ_U,    NMNHDIM_BUDGET_CART_NI_V, NMNHDIM_BUDGET_CART_NJ_V, &
                       NMNHDIM_BUDGET_CART_LEVEL,   NMNHDIM_BUDGET_CART_LEVEL_W,                        &
                       NMNHDIM_BUDGET_MASK_LEVEL,   NMNHDIM_BUDGET_MASK_LEVEL_W,                        &
                       NMNHDIM_BUDGET_MASK_NBUMASK, NMNHDIM_BUDGET_TIME,                                &
                       NMNHDIM_BUDGET_LES_AVG_TIME, NMNHDIM_BUDGET_LES_TIME,                            &
                       NMNHDIM_BUDGET_LES_LEVEL,    NMNHDIM_BUDGET_LES_SV,                              &
                       NMNHDIM_SPECTRA_2PTS_NI,     NMNHDIM_SPECTRA_2PTS_NJ,                            &
                       NMNHDIM_SPECTRA_SPEC_NI,     NMNHDIM_SPECTRA_SPEC_NJ,                            &
                       NMNHDIM_SPECTRA_LEVEL,                                                           &
                       NMNHDIM_SERIES_LEVEL,        NMNHDIM_SERIES_LEVEL_W, NMNHDIM_SERIES_TIME,        &
                       NMNHDIM_FLYER_TIME,          NMNHDIM_PROFILER_TIME,                              &
                       NMNHDIM_STATION_TIME,        NMNHDIM_LASTDIM_DIACHRO,                            &
                       NMNHDIM_NOTLISTED, NMNHDIM_UNUSED, NMNHDIM_ARAKAWA

TYPE(TFILEDATA),                              INTENT(IN)  :: TPFILE
TYPE(TFIELDDATA),                             INTENT(IN)  :: TPFIELD
INTEGER(KIND=CDFINT),DIMENSION(:),            INTENT(IN)  :: KSHAPE
INTEGER(KIND=CDFINT),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: KVDIMS
!
CHARACTER(LEN=32)             :: YINT
CHARACTER(LEN=2)              :: YDIR
character(len=:), allocatable :: ydimname
INTEGER                       :: IGRID
integer                       :: iidx
integer                       :: iresp
INTEGER                       :: JI
integer(kind=CDFINT)          :: ilen
integer(kind=CDFINT)          :: istatus
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Vdims_fill_nc4','called for '//TRIM(TPFIELD%CMNHNAME))
!
if ( Size( kshape ) == 0 .and. .not. tpfield%ltimedep) then
  !Scalar variable case not time dependent
  if ( tpfield%ndims == 0 ) then
    Allocate( kvdims(0) )
    return
  else
    call Print_msg( NVERB_FATAL, 'IO', 'IO_Vdims_fill_nc4', 'empty kshape' )
  end if
end if

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
  KVDIMS(TPFIELD%NDIMS+1) = tpfile%tncdims%tdims(NMNHDIM_TIME)%nid
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

if ( Any( tpfield%ndimlist(:) /= NMNHDIM_UNKNOWN ) ) then
  do ji = 1, Size( kvdims )
    if ( tpfield%ndimlist(ji) == NMNHDIM_UNKNOWN ) &
      call Print_msg( NVERB_FATAL, 'IO', 'IO_Vdims_fill_nc4', 'ndimlist partially filled for field ' // Trim( tpfield%cmnhname ) )

    if ( tpfield%ndimlist(ji) == NMNHDIM_NOTLISTED ) then
      call IO_Dim_find_create_nc4( tpfile, kshape(ji), iidx )
      kvdims(ji) = tpfile%tncdims%tdims(iidx)%nid
      cycle
    end if

    if ( tpfield%ndimlist(ji) == NMNHDIM_FLYER_TIME ) then
      istatus = NF90_INQ_DIMID( tpfile%nncid, 'time_flyer', kvdims(ji) )
      if ( istatus /= NF90_NOERR ) &
        call IO_Err_handle_nc4( istatus, 'IO_Vdims_fill_nc4','NF90_INQ_DIMID', Trim( tpfield%cmnhname ) )

      istatus = NF90_INQUIRE_DIMENSION( tpfile%nncid, kvdims(ji), len = ilen)
      if ( istatus /= NF90_NOERR ) &
        call IO_Err_handle_nc4( istatus, 'IO_Vdims_fill_nc4','NF90_INQUIRE_DIMENSION', Trim( tpfield%cmnhname ) )

      if ( kshape(ji) /= ilen ) then
        call Print_msg( NVERB_FATAL, 'IO', 'IO_Vdims_fill_nc4', &
                                     'wrong size for dimension '// 'time_flyer' // ' of field ' // Trim( tpfield%cmnhname ) )
      end if
      cycle
    end if

    if ( tpfield%ndimlist(ji) > 0 .and. tpfield%ndimlist(ji) <= tpfile%tncdims%nmaxdims ) then
      kvdims(ji) = tpfile%tncdims%tdims(tpfield%ndimlist(ji))%nid

      ! Check if dimension sizes are consistent with the declared dimensions ( skip ji>size(kshape), timedep dimension)
      if ( ji <= Size( kshape ) ) then
        if ( kshape(ji) /= tpfile%tncdims%tdims(tpfield%ndimlist(ji))%nlen )              &
          call Print_msg( NVERB_FATAL, 'IO', 'IO_Vdims_fill_nc4', 'wrong size for dimension ' &
                          // Trim( tpfile%tncdims%tdims(tpfield%ndimlist(ji))%cname ) //  &
                                       ' of field ' // Trim( tpfield%cmnhname ) )
      end if
    else
      call Print_msg( NVERB_FATAL, 'IO', 'IO_Vdims_fill_nc4', 'wrong dimension' )
    end if
  end do
else !ndimlist not provided
  do ji = 1, Size( kshape )
    if ( igrid > 0 .and. igrid < 9 ) then
      if ( ji == 1 ) then
        if ( ( ydir == 'XX' .or. ydir == 'XY') .and. kshape(1) == tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,1) )%nlen ) then
          kvdims(1) = tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,1) )%nid
        else if ( ydir == 'YY'                .and. kshape(1) == tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,2) )%nlen ) then
          kvdims(1) = tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,2) )%nid
        else if ( ydir == 'ZZ'                .and. kshape(1) == tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,3) )%nlen ) then
          kvdims(1) = tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,3) )%nid
        else
          call IO_Dim_find_create_nc4( tpfile, kshape(1), iidx )
          kvdims(1) = tpfile%tncdims%tdims(iidx)%nid
        end if
      else if ( ji == 2 ) then
        if ( ydir == 'XY' .and. kshape(2) == tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,2) )%nlen ) then
          kvdims(2) = tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,2) )%nid
        else
          call IO_Dim_find_create_nc4( tpfile, kshape(2), iidx )
          kvdims(2) = tpfile%tncdims%tdims(iidx)%nid
        end if
      else if ( ji == 3 ) then
        if ( ydir == 'XY' .and. kshape(3) == tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,3) )%nlen ) then
          kvdims(3) = tpfile%tncdims%tdims( NMNHDIM_ARAKAWA(igrid,3) )%nid
        else
          call IO_Dim_find_create_nc4( tpfile, kshape(3), iidx )
          kvdims(3) = tpfile%tncdims%tdims(iidx)%nid
        end if
      else
        call IO_Dim_find_create_nc4( tpfile, kshape(ji), iidx )
        kvdims(ji) = tpfile%tncdims%tdims(iidx)%nid
      end if
    else
      call IO_Dim_find_create_nc4( tpfile, kshape(ji), iidx )
      kvdims(ji) = tpfile%tncdims%tdims(iidx)%nid
    end if
  END DO
end if

END SUBROUTINE IO_Vdims_fill_nc4


subroutine IO_Dim_find_create_nc4( tpfile, klen, kidx, hdimname)
use modd_netcdf, only: tdimnc

type(tfiledata),            intent(in) :: tpfile
integer,       intent(in) :: klen
CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: HDIMNAME
integer, intent(out) :: kidx !Position of the dimension in the dimension array

character(len=16)     :: ysuffix
integer :: inewsize
integer :: ji
integer(kind=CDFINT)  :: istatus
type(tdimnc), dimension(:), allocatable :: tzncdims


kidx = -1

do ji = 1, Size( tpfile%tncdims%tdims )
  if ( tpfile%tncdims%tdims(ji)%nlen == klen ) then
    if ( Present( hdimname ) ) then
      if ( hdimname == Trim( tpfile%tncdims%tdims(ji)%cname ) ) then
        kidx = ji
        exit
      end if
    else
      kidx = ji
      exit
    end if
  end if
end do

if ( kidx == - 1 ) then
  !Check if already exist with the provided name (if so => error)
  if ( Present( hdimname ) ) then
    do ji = 1, Size( tpfile%tncdims%tdims )
      if ( hdimname == Trim( tpfile%tncdims%tdims(ji)%cname ) )                             &
        call Print_msg( NVERB_ERROR, 'IO', 'IO_Dim_find_create_nc4', 'dimension '          &
                        // Trim( hdimname ) // ' already exist but with a different size' )
    end do
  end if

  !Create new dimension
  inewsize = Size( tpfile%tncdims%tdims ) + 1
  allocate( tzncdims(inewsize) )
  tzncdims(1 : inewsize - 1) = tpfile%tncdims%tdims(:)

  if ( Present( hdimname ) ) then
    tzncdims(inewsize)%cname = Trim( hdimname )
  else
    Write( ysuffix, '( i0 )' ) klen
    tzncdims(inewsize)%cname = 'size' // Trim( ysuffix )
  end if
  tzncdims(inewsize)%nlen = Int( klen, kind = CDFINT )

  istatus = NF90_DEF_DIM( tpfile%nncid, tzncdims(inewsize)%cname, Int( klen, kind = CDFINT ), tzncdims(inewsize)%nid )
  if ( istatus /= NF90_NOERR ) &
    call IO_Err_handle_nc4( istatus, 'IO_Dim_find_create_nc4', 'NF90_DEF_DIM', Trim( tzncdims(inewsize)%cname) )

  call Move_alloc( from = tzncdims, to = tpfile%tncdims%tdims )
  tpfile%tncdims%nmaxdims = inewsize

  kidx = inewsize
  call Print_msg( NVERB_DEBUG, 'IO', 'IO_Dim_find_create_nc4','new dimension: ' // Trim( tpfile%tncdims%tdims(inewsize)%cname ) )
end if

end subroutine IO_Dim_find_create_nc4


FUNCTION IO_Strdimid_get_nc4(TPFILE,KLEN)
use modd_netcdf, only: tdimnc

TYPE(TFILEDATA),      INTENT(IN) :: TPFILE
INTEGER(KIND=CDFINT), INTENT(IN) :: KLEN
INTEGER(KIND=CDFINT)             :: IO_Strdimid_get_nc4

CHARACTER(LEN=16)                       :: YSUFFIX
integer                                 :: idx
integer                                 :: inewsize
integer                                 :: ji
integer(kind=CDFINT)                    :: istatus
type(tdimnc), dimension(:), allocatable :: tzncdims

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Strdimid_get_nc4','called')

IF (KLEN < 1) THEN
  CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Strdimid_get_nc4','KLEN='//TRIM(YSUFFIX))
END IF

! Search string dimension with KLEN length
idx = -1
if ( Allocated( tpfile%tncdims%tdims_str ) ) then
  do ji = 1, Size( tpfile%tncdims%tdims_str )
    if ( tpfile%tncdims%tdims_str(ji)%nlen == klen ) then
      idx = ji
      exit
    end if
  end do
end if

if ( idx == -1 ) then
  !Create new dimension
  inewsize = tpfile%tncdims%nmaxdims_str + 1
  allocate( tzncdims(inewsize) )
  if ( Allocated( tpfile%tncdims%tdims_str ) ) &
    tzncdims(1 : inewsize - 1) = tpfile%tncdims%tdims_str(:)

  Write( ysuffix, '( i0 )' ) klen
  tzncdims(inewsize)%cname = 'char' // Trim( ysuffix )
  tzncdims(inewsize)%nlen = Int( klen, kind = CDFINT )

  istatus = NF90_DEF_DIM( tpfile%nncid, tzncdims(inewsize)%cname, Int( klen, kind = CDFINT ), tzncdims(inewsize)%nid )
  if ( istatus /= NF90_NOERR ) &
    call IO_Err_handle_nc4( istatus, 'IO_Strdimid_get_nc4', 'NF90_DEF_DIM', Trim( tzncdims(inewsize)%cname) )

  call Move_alloc( from = tzncdims, to = tpfile%tncdims%tdims_str )
  tpfile%tncdims%nmaxdims_str = inewsize

  idx = inewsize

  call Print_msg( NVERB_DEBUG, 'IO', 'IO_Dim_find_create_nc4','new dimension: ' &
                  // Trim( tpfile%tncdims%tdims_str(inewsize)%cname ) )
end if

IO_Strdimid_get_nc4 = tpfile%tncdims%tdims_str(idx)%nid

END FUNCTION IO_Strdimid_get_nc4


FUNCTION IO_Iocdf_alloc_nc4()
type(tdimsnc), pointer :: IO_Iocdf_alloc_nc4
type(tdimsnc), pointer :: tzdimsnc
INTEGER                :: IRESP

CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Iocdf_alloc_nc4','called')

ALLOCATE(tzdimsnc, STAT=IRESP)
IF (IRESP > 0) THEN
  CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Iocdf_alloc_nc4','memory allocation error')
END IF

IO_Iocdf_alloc_nc4 => tzdimsnc

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
