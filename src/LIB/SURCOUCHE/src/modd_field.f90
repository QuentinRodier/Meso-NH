!MNH_LIC Copyright 2016-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Original version:
!  P. Wautelet: 05/2016-04/2018: new data structures and calls for I/O
! Modifications:
!  P. Wautelet 12/04/2019: added pointers for C1D, L1D, N1D, X5D and X6D structures in TFIELDDATA
!  P. Wautelet 12/07/2019: add pointers for T1D structure in TFIELDDATA
!  P. Wautelet 23/01/2020: split in modd_field.f90 and mode_field.f90
!  P. Wautelet 27/01/2020: create the tfieldmetadata_base abstract datatype
!  P. Wautelet 14/09/2020: add ndimlist field to tfieldmetadata_base
!  P. Wautelet 10/11/2020: new data structures for netCDF dimensions
!  P. Wautelet 24/09/2021: add Fill_tfielddata and use it as a custom constructor for tfielddata type
!  P. Wautelet 08/10/2021: add 2 new dimensions: LW_bands (NMNHDIM_NLWB) and SW_bands (NMNHDIM_NSWB)
!  P. Wautelet 14/10/2021: dynamically allocate tfieldlist (+ reallocate if necessary)
!  P. Wautelet 04/11/2021: add TFIELDMETADATA type
!-----------------------------------------------------------------
module modd_field

use modd_parameters, only: NGRIDUNKNOWN, NMNHNAMELGTMAX, NSTDNAMELGTMAX, NLONGNAMELGTMAX, NUNITLGTMAX, NCOMMENTLGTMAX
use modd_type_date,  only: date_time
#ifdef MNH_IOCDF4
use NETCDF,          only: NF90_FILL_INT, NF90_FILL_REAL
#endif

implicit none

integer, parameter :: NMNHMAXDIMS = 6 ! Cannot be less than 6
INTEGER,PARAMETER :: TYPEUNDEF = -1, TYPEINT = 1, TYPELOG = 2, TYPEREAL = 3, TYPECHAR = 4, TYPEDATE = 5
!
INTEGER, PARAMETER :: NMAXFIELDINIT = 200 !Initial maximum number of fields in tfieldlist
INTEGER, PARAMETER :: NMAXFIELDSTEP = 50  !Number of fields to add each time tfieldlist is too small

integer, parameter :: NMNHDIM_UNKNOWN             = -2

!For efficient use of memory, it is better that all values for real dimensions be contiguous
integer, parameter :: NMNHDIM_NI                  = 1
integer, parameter :: NMNHDIM_NJ                  = 2
integer, parameter :: NMNHDIM_NI_U                = 3
integer, parameter :: NMNHDIM_NJ_U                = 4
integer, parameter :: NMNHDIM_NI_V                = 5
integer, parameter :: NMNHDIM_NJ_V                = 6
integer, parameter :: NMNHDIM_LEVEL               = 7
integer, parameter :: NMNHDIM_LEVEL_W             = 8
integer, parameter :: NMNHDIM_TIME                = 9

integer, parameter :: NMNHDIM_ONE                 = 10

integer, parameter :: NMNHDIM_NSWB                = 11
integer, parameter :: NMNHDIM_NLWB                = 12

integer, parameter :: NMNHDIM_LASTDIM_NODIACHRO   = 12 ! Index of the last defined dimension for non-diachronic files

integer, parameter :: NMNHDIM_COMPLEX             = 13

integer, parameter :: NMNHDIM_BUDGET_CART_NI      = 14
integer, parameter :: NMNHDIM_BUDGET_CART_NJ      = 15
integer, parameter :: NMNHDIM_BUDGET_CART_NI_U    = 16
integer, parameter :: NMNHDIM_BUDGET_CART_NJ_U    = 17
integer, parameter :: NMNHDIM_BUDGET_CART_NI_V    = 18
integer, parameter :: NMNHDIM_BUDGET_CART_NJ_V    = 19
integer, parameter :: NMNHDIM_BUDGET_CART_LEVEL   = 20
integer, parameter :: NMNHDIM_BUDGET_CART_LEVEL_W = 21

integer, parameter :: NMNHDIM_BUDGET_MASK_LEVEL   = 22
integer, parameter :: NMNHDIM_BUDGET_MASK_LEVEL_W = 23
integer, parameter :: NMNHDIM_BUDGET_MASK_NBUMASK = 24

integer, parameter :: NMNHDIM_BUDGET_TIME         = 25

integer, parameter :: NMNHDIM_BUDGET_LES_TIME     = 26
integer, parameter :: NMNHDIM_BUDGET_LES_AVG_TIME = 27
integer, parameter :: NMNHDIM_BUDGET_LES_LEVEL    = 28
integer, parameter :: NMNHDIM_BUDGET_LES_SV       = 29
integer, parameter :: NMNHDIM_BUDGET_LES_PDF      = 30
integer, parameter :: NMNHDIM_BUDGET_LES_MASK     = 100 ! This is not a true dimension

integer, parameter :: NMNHDIM_SPECTRA_2PTS_NI     = 31
integer, parameter :: NMNHDIM_SPECTRA_2PTS_NJ     = 32
integer, parameter :: NMNHDIM_SPECTRA_SPEC_NI     = 33
integer, parameter :: NMNHDIM_SPECTRA_SPEC_NJ     = 34
integer, parameter :: NMNHDIM_SPECTRA_LEVEL       = 35

integer, parameter :: NMNHDIM_SERIES_LEVEL        = 36
integer, parameter :: NMNHDIM_SERIES_LEVEL_W      = 37
integer, parameter :: NMNHDIM_SERIES_TIME         = 38  ! Time dimension for time series

integer, parameter :: NMNHDIM_FLYER_TIME          = 39  ! Time dimension for aircraft/balloon (dimension local to each flyer)
integer, parameter :: NMNHDIM_PROFILER_TIME       = 40  ! Time dimension for profilers
integer, parameter :: NMNHDIM_STATION_TIME        = 41  ! Time dimension for stations

integer, parameter :: NMNHDIM_PAIR                = 42  ! For values coming by pair (ie boundaries)

integer, parameter :: NMNHDIM_LASTDIM_DIACHRO     = 42  ! Index of the last defined dimension for diachronic files

integer, parameter :: NMNHDIM_BUDGET_NGROUPS      = 101 ! This is not a true dimension
integer, parameter :: NMNHDIM_FLYER_PROC          = 102 ! This is not a true dimension
integer, parameter :: NMNHDIM_PROFILER_PROC       = 103 ! This is not a true dimension
integer, parameter :: NMNHDIM_STATION_PROC        = 104 ! This is not a true dimension
integer, parameter :: NMNHDIM_SERIES_PROC         = 105 ! This is not a true dimension
integer, parameter :: NMNHDIM_BUDGET_TERM         = 106 ! This is not a true dimension

integer, parameter :: NMNHDIM_NOTLISTED           = 200 ! Special case for valid dimension but not in this parameter list

integer, parameter :: NMNHDIM_UNUSED              = 300

!Array to allow easy identification of dimensions for Arakawa grid points
integer, dimension(0:8,3), parameter :: NMNHDIM_ARAKAWA = reshape( [ &
  NMNHDIM_UNKNOWN, NMNHDIM_UNKNOWN, NMNHDIM_UNKNOWN, & ! dummy point (to treat ngrid=0 without crash)
  NMNHDIM_NI,      NMNHDIM_NJ,      NMNHDIM_LEVEL,   & ! mass point
  NMNHDIM_NI_U,    NMNHDIM_NJ_U,    NMNHDIM_LEVEL,   & ! u point
  NMNHDIM_NI_V,    NMNHDIM_NJ_V,    NMNHDIM_LEVEL,   & ! v point
  NMNHDIM_NI,      NMNHDIM_NJ,      NMNHDIM_LEVEL_W, & ! w point
  NMNHDIM_NI_U,    NMNHDIM_NJ_V,    NMNHDIM_LEVEL,   & ! xi vorticity point (=f point =uv point)
  NMNHDIM_NI_U,    NMNHDIM_NJ_U,    NMNHDIM_LEVEL_W, & ! eta vorticity point (=uw point)
  NMNHDIM_NI_V,    NMNHDIM_NJ_V,    NMNHDIM_LEVEL_W, & ! zeta vorticity point (=vw point)
  NMNHDIM_NI_U,    NMNHDIM_NJ_V,    NMNHDIM_LEVEL_W] & ! fw point (=uvw point)
  , shape = [ 9, 3 ], order = [ 2, 1 ] )

INTEGER, SAVE :: NMAXFIELDS !Maximum number of fields in tfieldlist (value is automatically increased if too small)

TYPE TFIELDPTR_C0D
  CHARACTER(LEN=:),     POINTER :: DATA => NULL()
END TYPE TFIELDPTR_C0D
!
TYPE TFIELDPTR_C1D
  CHARACTER(LEN=:),DIMENSION(:),POINTER :: DATA => NULL()
END TYPE TFIELDPTR_C1D
!
TYPE TFIELDPTR_L0D
  LOGICAL,              POINTER :: DATA => NULL()
END TYPE TFIELDPTR_L0D
!
TYPE TFIELDPTR_L1D
  LOGICAL,DIMENSION(:), POINTER :: DATA => NULL()
END TYPE TFIELDPTR_L1D
!
TYPE TFIELDPTR_N0D
  INTEGER,              POINTER :: DATA => NULL()
END TYPE TFIELDPTR_N0D
!
TYPE TFIELDPTR_N1D
  INTEGER,DIMENSION(:),   POINTER :: DATA => NULL()
END TYPE TFIELDPTR_N1D
!
TYPE TFIELDPTR_N2D
  INTEGER,DIMENSION(:,:), POINTER :: DATA => NULL()
END TYPE TFIELDPTR_N2D
!
TYPE TFIELDPTR_N3D
  INTEGER,DIMENSION(:,:,:),POINTER :: DATA => NULL()
END TYPE TFIELDPTR_N3D
!
TYPE TFIELDPTR_X0D
  REAL,                 POINTER :: DATA => NULL()
END TYPE TFIELDPTR_X0D
!
TYPE TFIELDPTR_X1D
  REAL,DIMENSION(:),    POINTER :: DATA => NULL()
END TYPE TFIELDPTR_X1D
!
TYPE TFIELDPTR_X2D
  REAL,DIMENSION(:,:),  POINTER :: DATA => NULL()
END TYPE TFIELDPTR_X2D
!
TYPE TFIELDPTR_X3D
  REAL,DIMENSION(:,:,:),POINTER :: DATA => NULL()
END TYPE TFIELDPTR_X3D
!
TYPE TFIELDPTR_X4D
  REAL,DIMENSION(:,:,:,:),POINTER :: DATA => NULL()
END TYPE TFIELDPTR_X4D
!
TYPE TFIELDPTR_X5D
  REAL,DIMENSION(:,:,:,:,:),POINTER :: DATA => NULL()
END TYPE TFIELDPTR_X5D
!
TYPE TFIELDPTR_X6D
  REAL,DIMENSION(:,:,:,:,:,:),POINTER :: DATA => NULL()
END TYPE TFIELDPTR_X6D
!
TYPE TFIELDPTR_T0D
  TYPE(DATE_TIME),      POINTER :: DATA => NULL()
END TYPE TFIELDPTR_T0D
!
TYPE TFIELDPTR_T1D
  TYPE(DATE_TIME), DIMENSION(:), POINTER :: DATA => NULL()
END TYPE TFIELDPTR_T1D
!
type :: tfieldmetadata_base
  CHARACTER(LEN=NMNHNAMELGTMAX)  :: CMNHNAME  = '' !Name of the field (for MesoNH, non CF convention)
  CHARACTER(LEN=NSTDNAMELGTMAX)  :: CSTDNAME  = '' !Standard name (CF convention)
  CHARACTER(LEN=NLONGNAMELGTMAX) :: CLONGNAME = '' !Long name (CF convention)
  CHARACTER(LEN=NUNITLGTMAX)     :: CUNITS    = '' !Canonical units (CF convention)
  CHARACTER(LEN=NCOMMENTLGTMAX)  :: CCOMMENT  = '' !Comment (for MesoNH, non CF convention)
  INTEGER            :: NGRID     = NGRIDUNKNOWN !Localization on the model grid
  INTEGER            :: NTYPE     = TYPEUNDEF !Datatype
  INTEGER            :: NDIMS     = 0  !Number of dimensions
  INTEGER, DIMENSION(NMNHMAXDIMS) :: NDIMLIST = NMNHDIM_UNKNOWN ! List of dimensions of the data field
  !
#ifdef MNH_IOCDF4
  INTEGER            :: NFILLVALUE =  NF90_FILL_INT  !Fill value for integer fields
  REAL               :: XFILLVALUE =  NF90_FILL_REAL !Fill value for real fields
                                                     !NF90_FILL_REAL is the default fill value
                                                     !used by netCDF to pre-fill real and also double
                                                     !variables
#else
  INTEGER            :: NFILLVALUE =  -2147483647            !Fill value for integer fields
  REAL               :: XFILLVALUE =  9.9692099683868690e+36 !Fill value for real fields
#endif
  INTEGER            :: NVALIDMIN  = -2147483646 !Minimum valid value for integer fields
  INTEGER            :: NVALIDMAX  =  2147483647 !Maximum valid value for integer fields
  REAL               :: XVALIDMIN  = -1.E36 !Minimum valid value for real fields
  REAL               :: XVALIDMAX  =  1.E36 !Maximum valid value for real fields
end type tfieldmetadata_base

TYPE, extends( tfieldmetadata_base ) :: TFIELDMETADATA
  CHARACTER(LEN=2)   :: CDIR      = '' !Type of the data field (XX,XY,--...)
  CHARACTER(LEN=4)   :: CLBTYPE   = 'NONE' !Type of the lateral boundary (LBX,LBY,LBXU,LBYV)
  LOGICAL            :: LTIMEDEP  = .FALSE. !Is the field time-dependent?
END TYPE TFIELDMETADATA

!Structure describing the characteristics of a field
TYPE, EXTENDS( TFIELDMETADATA ) :: TFIELDDATA
  INTEGER :: NMODELMAX = -1 !Number of models for which the field has been allocated (default value must be negative)
  !
  TYPE(TFIELDPTR_C0D),DIMENSION(:),ALLOCATABLE :: TFIELD_C0D !Pointer to the character string fields (one per nested mesh)
  TYPE(TFIELDPTR_C1D),DIMENSION(:),ALLOCATABLE :: TFIELD_C1D !Pointer to the character string 1D fields (one per nested mesh)
  !
  TYPE(TFIELDPTR_L0D),DIMENSION(:),ALLOCATABLE :: TFIELD_L0D !Pointer to the scalar logical fields (one per nested mesh)
  TYPE(TFIELDPTR_L1D),DIMENSION(:),ALLOCATABLE :: TFIELD_L1D !Pointer to the logical 1D fields (one per nested mesh)
  !
  TYPE(TFIELDPTR_N0D),DIMENSION(:),ALLOCATABLE :: TFIELD_N0D !Pointer to the scalar integer fields (one per nested mesh)
  TYPE(TFIELDPTR_N1D),DIMENSION(:),ALLOCATABLE :: TFIELD_N1D !Pointer to the integer 1D fields (one per nested mesh)
  TYPE(TFIELDPTR_N2D),DIMENSION(:),ALLOCATABLE :: TFIELD_N2D !Pointer to the integer 2D fields (one per nested mesh)
  TYPE(TFIELDPTR_N3D),DIMENSION(:),ALLOCATABLE :: TFIELD_N3D !Pointer to the integer 3D fields (one per nested mesh)
  !
  TYPE(TFIELDPTR_X0D),DIMENSION(:),ALLOCATABLE :: TFIELD_X0D !Pointer to the scalar real fields (one per nested mesh)
  TYPE(TFIELDPTR_X1D),DIMENSION(:),ALLOCATABLE :: TFIELD_X1D !Pointer to the real 1D fields (one per nested mesh)
  TYPE(TFIELDPTR_X2D),DIMENSION(:),ALLOCATABLE :: TFIELD_X2D !Pointer to the real 2D fields (one per nested mesh)
  TYPE(TFIELDPTR_X3D),DIMENSION(:),ALLOCATABLE :: TFIELD_X3D !Pointer to the real 3D fields (one per nested mesh)
  TYPE(TFIELDPTR_X4D),DIMENSION(:),ALLOCATABLE :: TFIELD_X4D !Pointer to the real 4D fields (one per nested mesh)
  TYPE(TFIELDPTR_X5D),DIMENSION(:),ALLOCATABLE :: TFIELD_X5D !Pointer to the real 5D fields (one per nested mesh)
  TYPE(TFIELDPTR_X6D),DIMENSION(:),ALLOCATABLE :: TFIELD_X6D !Pointer to the real 6D fields (one per nested mesh)
  !
  TYPE(TFIELDPTR_T0D),DIMENSION(:),ALLOCATABLE :: TFIELD_T0D !Pointer to the scalar date/time fields (one per nested mesh)
  TYPE(TFIELDPTR_T1D),DIMENSION(:),ALLOCATABLE :: TFIELD_T1D !Pointer to the date/time 1D fields (one per nested mesh)
END TYPE TFIELDDATA
!
integer, save :: NFIELDS_USED = 0
LOGICAL, SAVE :: LFIELDLIST_ISINIT = .FALSE.
TYPE(TFIELDDATA), ALLOCATABLE, DIMENSION(:), SAVE :: TFIELDLIST

interface TFIELDMETADATA
  module procedure :: Fill_tfieldmetadata
  module procedure :: Fill_tfieldmetadata_from_tfielddata
end interface TFIELDMETADATA

interface TFIELDDATA
  module procedure :: Fill_tfielddata
  module procedure :: Fill_tfielddata_from_tfieldmetadata
end interface TFIELDDATA

contains

type(tfieldmetadata) function Fill_tfieldmetadata( cmnhname, cstdname, clongname, cunits, ccomment,                    &
                                               ngrid, ntype, ndims, ndimlist,                                      &
                                               nfillvalue, xfillvalue, nvalidmin, nvalidmax, xvalidmin, xvalidmax, &
                                               cdir, clbtype, ltimedep ) result(tpfield)

  use mode_msg

  character(len=*),      optional, intent(in) :: cmnhname
  character(len=*),      optional, intent(in) :: cstdname
  character(len=*),      optional, intent(in) :: clongname
  character(len=*),      optional, intent(in) :: cunits
  character(len=*),      optional, intent(in) :: ccomment
  integer,               optional, intent(in) :: ngrid
  integer,                         intent(in) :: ntype
  integer,               optional, intent(in) :: ndims
  integer, dimension(:), optional, intent(in) :: ndimlist
  integer,               optional, intent(in) :: nfillvalue
  real,                  optional, intent(in) :: xfillvalue
  integer,               optional, intent(in) :: nvalidmin
  integer,               optional, intent(in) :: nvalidmax
  real,                  optional, intent(in) :: xvalidmin
  real,                  optional, intent(in) :: xvalidmax

  character(len=*),      optional, intent(in) :: cdir
  character(len=*),      optional, intent(in) :: clbtype
  logical,               optional, intent(in) :: ltimedep

  character(len=:), allocatable :: ymnhname

  ! cmnhname
  if ( Present( cmnhname ) ) then
    tpfield%cmnhname = cmnhname
    if ( Len_trim(cmnhname) > NMNHNAMELGTMAX )                                                               &
      call Print_msg( NVERB_WARNING, 'GEN', 'Fill_tfielddata',                                               &
                      'cmnhname was truncated to ' // Trim( tpfield%cmnhname ) // ' from ' // Trim( cmnhname ) )
    ymnhname = Trim( cmnhname )
  else
    ymnhname = 'unknown mnhname'
  end if

  ! cstdname
  if ( Present( cstdname ) ) then
    tpfield%cstdname = cstdname
    if ( Len_trim(cstdname) > NSTDNAMELGTMAX )                                                                 &
      call Print_msg( NVERB_WARNING, 'GEN', 'Fill_tfielddata',                                                 &
                      'cstdname was truncated to ' // Trim( tpfield%cstdname ) // ' from ' // Trim( cstdname ) &
                      // ' for variable ' // Trim( ymnhname ) )
  end if

  ! clongname
  if ( Present( clongname ) ) then
    tpfield%clongname = clongname
    if ( Len_trim(clongname) > NLONGNAMELGTMAX )                                                                  &
      call Print_msg( NVERB_WARNING, 'GEN', 'Fill_tfielddata',                                                    &
                      'clongname was truncated to ' // Trim( tpfield%clongname ) // ' from ' // Trim( clongname ) &
                      // ' for variable ' // Trim( ymnhname ) )
  end if

  ! cunits
  if ( Present( cunits ) ) then
    tpfield%cunits = cunits
    if ( Len_trim(cunits) > NUNITLGTMAX )                                                                &
      call Print_msg( NVERB_WARNING, 'GEN', 'Fill_tfielddata',                                           &
                      'cunits was truncated to ' // Trim( tpfield%cunits ) // ' from ' // Trim( cunits ) &
                      // ' for variable ' // Trim( ymnhname ) )
  end if

  ! ccomment
  if ( Present( ccomment ) ) then
    tpfield%ccomment = ccomment
    if ( Len_trim(ccomment) > NCOMMENTLGTMAX )                                                                 &
      call Print_msg( NVERB_WARNING, 'GEN', 'Fill_tfielddata',                                                 &
                      'ccomment was truncated to ' // Trim( tpfield%ccomment ) // ' from ' // Trim( ccomment ) )
  end if

  ! ngrid
  if ( Present( ngrid ) ) then
    if ( ngrid /= NGRIDUNKNOWN .and. ngrid < 0 .and. ngrid > 8 ) then
      call Print_msg( NVERB_ERROR, 'GEN', 'Fill_tfielddata',                     &
                      'invalid value of ngrid for variable ' // Trim( ymnhname ) )
    else
      tpfield%ngrid = ngrid
    end if
  end if

  ! ntype
  if ( All( ntype /= [ TYPEINT, TYPELOG, TYPEREAL, TYPECHAR, TYPEDATE ] ) )    &
    call Print_msg( NVERB_ERROR, 'GEN', 'Fill_tfielddata',                     &
                    'invalid value of ntype for variable ' // Trim( ymnhname ) )
  tpfield%ntype = ntype

  ! ndims
  if ( Present( ndims ) ) then
    select case ( ntype )
      case ( TYPECHAR )
        if ( ndims < 0 .or. ndims > 1 )                                                                 &
          call Print_msg( NVERB_ERROR, 'GEN', 'Fill_tfielddata', 'invalid value of ndims for variable ' &
                          // Trim( ymnhname ) // ' of type TYPECHAR' )
      case ( TYPELOG )
        if ( ndims < 0 .or. ndims > 1 )                                                                 &
          call Print_msg( NVERB_ERROR, 'GEN', 'Fill_tfielddata', 'invalid value of ndims for variable ' &
                          // Trim( ymnhname ) // ' of type TYPELOG' )
      case ( TYPEINT )
        if ( ndims < 0 .or. ndims > 3 )                                                                 &
          call Print_msg( NVERB_ERROR, 'GEN', 'Fill_tfielddata', 'invalid value of ndims for variable ' &
                          // Trim( ymnhname ) // ' of type TYPEINT' )
      case ( TYPEREAL )
        if ( ndims < 0 .or. ndims > 6 )                                                                 &
          call Print_msg( NVERB_ERROR, 'GEN', 'Fill_tfielddata', 'invalid value of ndims for variable ' &
                          // Trim( ymnhname ) // ' of type TYPEREAL' )
      case ( TYPEDATE )
        if ( ndims < 0 .or. ndims > 1 )                                                                 &
          call Print_msg( NVERB_ERROR, 'GEN', 'Fill_tfielddata', 'invalid value of ndims for variable ' &
                          // Trim( ymnhname ) // ' of type TYPEDATE' )
      case default
        call Print_msg( NVERB_ERROR, 'GEN', 'Fill_tfielddata',                     &
                        'invalid value of ntype for variable ' // Trim( ymnhname ) )

    end select
    tpfield%ndims = ndims
  end if

  ! ndimlist
  if ( Present( ndimlist ) ) then
    if ( Size( ndimlist ) /= tpfield%ndims ) &
      call Print_msg( NVERB_ERROR, 'GEN', 'Fill_tfielddata', 'ndimlist size different of ndims for variable ' // Trim( ymnhname ) )

    tpfield%ndimlist(1:tpfield%ndims)  = ndimlist(:)
    tpfield%ndimlist(tpfield%ndims+1:) = NMNHDIM_UNUSED
  else
    !If ndimlist is not provided, it is possible to fill it if some information is available
    if ( Present( cdir ) ) then
      if ( cdir == 'XY' ) then
        ! Check also on NGRIDUNKNOWN (this happens if the optional ngrid argument is not provided)
        if ( tpfield%ndims == 2 .and. tpfield%ngrid /= NGRIDUNKNOWN ) then
          tpfield%ndimlist(1:2) = NMNHDIM_ARAKAWA(tpfield%ngrid,1:2)
        else if ( tpfield%ndims == 3 .and. tpfield%ngrid /= NGRIDUNKNOWN ) then
          tpfield%ndimlist(1:3) = NMNHDIM_ARAKAWA(tpfield%ngrid,1:3)
        else
          call Print_msg( NVERB_DEBUG, 'GEN', 'Fill_tfielddata', 'ndimlist not filled for variable ' // Trim( ymnhname ) )
        end if
      else
        call Print_msg( NVERB_DEBUG, 'GEN', 'Fill_tfielddata', 'ndimlist not filled for variable ' // Trim( ymnhname ) )
      end if
    else
      call Print_msg( NVERB_DEBUG, 'GEN', 'Fill_tfielddata', 'ndimlist not filled for variable ' // Trim( ymnhname ) )
    end if
  end if
  if ( Present( ltimedep ) ) then
    if ( ltimedep ) then
      if ( tpfield%ndims == NMNHMAXDIMS )                                                                        &
        call Print_msg( NVERB_ERROR, 'GEN', 'Fill_tfielddata',                                           &
                        'ltimedep=T not possible if ndims=NMNHMAXDIMS for variable ' // Trim( ymnhname ) )
      !Set this dimension only if ndimlist already filled up or tpfield%ndims = 0
      if ( tpfield%ndims == 0 ) then
        tpfield%ndimlist( tpfield%ndims + 1 ) = NMNHDIM_TIME
      else if ( tpfield%ndimlist(tpfield%ndims) /= NMNHDIM_UNKNOWN ) then
        tpfield%ndimlist( tpfield%ndims + 1 ) = NMNHDIM_TIME
      end if
    end if
  end if

  ! nfillvalue
  if ( Present( nfillvalue ) ) then
    if ( ntype /= TYPEINT )                                                                   &
      call Print_msg( NVERB_WARNING, 'GEN', 'Fill_tfielddata',                                &
                      'nfillvalue provided for the non-integer variable ' // Trim( ymnhname ) )
    tpfield%nfillvalue = nfillvalue
  end if

  ! xfillvalue
  if ( Present( xfillvalue ) ) then
    if ( ntype /= TYPEREAL )                                                               &
      call Print_msg( NVERB_WARNING, 'GEN', 'Fill_tfielddata',                             &
                      'xfillvalue provided for the non-real variable ' // Trim( ymnhname ) )
    tpfield%xfillvalue = xfillvalue
  end if

  ! nvalidmin
  if ( Present( nvalidmin ) ) then
    if ( ntype /= TYPEINT )                                                                  &
      call Print_msg( NVERB_WARNING, 'GEN', 'Fill_tfielddata',                               &
                      'nvalidmin provided for the non-integer variable ' // Trim( ymnhname ) )
    tpfield%nvalidmin = nvalidmin
  end if

  ! nvalidmax
  if ( Present( nvalidmax ) ) then
    if ( ntype /= TYPEINT )                                                                  &
      call Print_msg( NVERB_WARNING, 'GEN', 'Fill_tfielddata',                               &
                      'nvalidmax provided for the non-integer variable ' // Trim( ymnhname ) )
    if ( Present( nvalidmin ) ) then
      if ( nvalidmax < nvalidmin ) &
        call Print_msg( NVERB_WARNING, 'GEN', 'Fill_tfielddata', 'nvalidmax < nvalidmin for variable ' // Trim( ymnhname ) )
    end if
    tpfield%nvalidmax = nvalidmax
  end if

  ! xvalidmin
  if ( Present( xvalidmin ) ) then
    if ( ntype /= TYPEREAL )                                                              &
      call Print_msg( NVERB_WARNING, 'GEN', 'Fill_tfielddata',                            &
                      'xvalidmin provided for the non-real variable ' // Trim( ymnhname ) )
    tpfield%xvalidmin = xvalidmin
  end if

  ! xvalidmax
  if ( Present( xvalidmax ) ) then
    if ( ntype /= TYPEREAL )                                                              &
      call Print_msg( NVERB_WARNING, 'GEN', 'Fill_tfielddata',                            &
                      'xvalidmax provided for the non-real variable ' // Trim( ymnhname ) )
    if ( Present( xvalidmin ) ) then
      if ( xvalidmax < xvalidmin ) &
        call Print_msg( NVERB_WARNING, 'GEN', 'Fill_tfielddata', 'xvalidmax < xvalidmin for variable ' // Trim( ymnhname ) )
    end if
    tpfield%xvalidmax = xvalidmax
  end if

  ! cdir
  if ( Present( cdir ) ) then
    if ( Any( cdir == [ '  ', '--', 'XX', 'XY', 'YY', 'ZZ' ] ) ) then
      tpfield%cdir = cdir
    else
      call Print_msg( NVERB_ERROR, 'GEN', 'Fill_tfielddata',                                             &
                      'invalid value of cdir (' // Trim( cdir ) // ') for variable ' // Trim( ymnhname ) )
    end if
  end if

  ! clbtype
  if ( Present( clbtype ) ) then
    if ( Any( clbtype == [ 'NONE', 'LBX ', 'LBXU', 'LBY ', 'LBYV' ] ) ) then
      tpfield%clbtype = clbtype
    else
      call Print_msg( NVERB_ERROR, 'GEN', 'Fill_tfielddata',                                                   &
                      'invalid value of clbtype (' // Trim( clbtype ) // ') for variable ' // Trim( ymnhname ) )
    end if
  end if

  ! ltimedep
  if ( Present( ltimedep ) ) tpfield%ltimedep = ltimedep
end function Fill_tfieldmetadata


type(tfieldmetadata) function Fill_tfieldmetadata_from_tfielddata( tpfieldin ) result(tpfield)
  type(tfielddata), intent(in) :: tpfieldin

  tpfield%CMNHNAME =   tpfieldin%CMNHNAME
  tpfield%CSTDNAME =   tpfieldin%CSTDNAME
  tpfield%CLONGNAME =  tpfieldin%CLONGNAME
  tpfield%CUNITS =     tpfieldin%CUNITS
  tpfield%CCOMMENT =   tpfieldin%CCOMMENT
  tpfield%NGRID =      tpfieldin%NGRID
  tpfield%NTYPE =      tpfieldin%NTYPE
  tpfield%NDIMS =      tpfieldin%NDIMS
  tpfield%NDIMLIST =   tpfieldin%NDIMLIST
  tpfield%NFILLVALUE = tpfieldin%NFILLVALUE
  tpfield%XFILLVALUE = tpfieldin%XFILLVALUE
  tpfield%NVALIDMIN =  tpfieldin%NVALIDMIN
  tpfield%NVALIDMAX =  tpfieldin%NVALIDMAX
  tpfield%XVALIDMIN =  tpfieldin%XVALIDMIN
  tpfield%XVALIDMAX =  tpfieldin%XVALIDMAX
  tpfield%CDIR =       tpfieldin%CDIR
  tpfield%CLBTYPE =    tpfieldin%CLBTYPE
  tpfield%LTIMEDEP =   tpfieldin%LTIMEDEP

end function Fill_tfieldmetadata_from_tfielddata


type(tfielddata) function Fill_tfielddata_from_tfieldmetadata( tpfieldin ) result(tpfield)
  type(tfieldmetadata), intent(in) :: tpfieldin

  tpfield%CMNHNAME =   tpfieldin%CMNHNAME
  tpfield%CSTDNAME =   tpfieldin%CSTDNAME
  tpfield%CLONGNAME =  tpfieldin%CLONGNAME
  tpfield%CUNITS =     tpfieldin%CUNITS
  tpfield%CCOMMENT =   tpfieldin%CCOMMENT
  tpfield%NGRID =      tpfieldin%NGRID
  tpfield%NTYPE =      tpfieldin%NTYPE
  tpfield%NDIMS =      tpfieldin%NDIMS
  tpfield%NDIMLIST =   tpfieldin%NDIMLIST
  tpfield%NFILLVALUE = tpfieldin%NFILLVALUE
  tpfield%XFILLVALUE = tpfieldin%XFILLVALUE
  tpfield%NVALIDMIN =  tpfieldin%NVALIDMIN
  tpfield%NVALIDMAX =  tpfieldin%NVALIDMAX
  tpfield%XVALIDMIN =  tpfieldin%XVALIDMIN
  tpfield%XVALIDMAX =  tpfieldin%XVALIDMAX
  tpfield%CDIR =       tpfieldin%CDIR
  tpfield%CLBTYPE =    tpfieldin%CLBTYPE
  tpfield%LTIMEDEP =   tpfieldin%LTIMEDEP

end function Fill_tfielddata_from_tfieldmetadata


type(tfielddata) function Fill_tfielddata( cmnhname, cstdname, clongname, cunits, ccomment,                    &
                                           ngrid, ntype, ndims, ndimlist,                                      &
                                           nfillvalue, xfillvalue, nvalidmin, nvalidmax, xvalidmin, xvalidmax, &
                                           cdir, clbtype, ltimedep ) result(tpfield)

  character(len=*),      optional, intent(in) :: cmnhname
  character(len=*),      optional, intent(in) :: cstdname
  character(len=*),      optional, intent(in) :: clongname
  character(len=*),      optional, intent(in) :: cunits
  character(len=*),      optional, intent(in) :: ccomment
  integer,               optional, intent(in) :: ngrid
  integer,                         intent(in) :: ntype
  integer,               optional, intent(in) :: ndims
  integer, dimension(:), optional, intent(in) :: ndimlist
  integer,               optional, intent(in) :: nfillvalue
  real,                  optional, intent(in) :: xfillvalue
  integer,               optional, intent(in) :: nvalidmin
  integer,               optional, intent(in) :: nvalidmax
  real,                  optional, intent(in) :: xvalidmin
  real,                  optional, intent(in) :: xvalidmax

  character(len=*),      optional, intent(in) :: cdir
  character(len=*),      optional, intent(in) :: clbtype
  logical,               optional, intent(in) :: ltimedep

#if 0
!Works with GCC (10.4)
!Does not works with Intel ifort 18.0.2.199, NVHPC 22.11
  !Use the tfieldmetadata custom constructor and modify nmodelmax
  !The data structures tfield_xyd are not set (null)
  tpfield = tfielddata ( tfieldmetadata = tfieldmetadata( &
                                 cmnhname   = cmnhname,   &
                                 cstdname   = cstdname,   &
                                 clongname  = clongname,  &
                                 cunits     = cunits,     &
                                 ccomment   = ccomment,   &
                                 ngrid      = ngrid,      &
                                 ntype      = ntype,      &
                                 ndims      = ndims,      &
                                 ndimlist   = ndimlist,   &
                                 nfillvalue = nfillvalue, &
                                 xfillvalue = xfillvalue, &
                                 nvalidmin  = nvalidmin,  &
                                 nvalidmax  = nvalidmax,  &
                                 xvalidmin  = xvalidmin,  &
                                 xvalidmax  = xvalidmax,  &
                                 cdir       = cdir,       &
                                 clbtype    = clbtype,    &
                                 ltimedep   = ltimedep ) ,&
! Set nmodelmax to 0 instead of -1 by default.
! This value can therefore be used to determine if the field was initialized by calling this constructor.
                         nmodelmax = 0,                   &
                         tfield_c0d = null(),             &
                         tfield_c1d = null(),             &
                         tfield_l0d = null(),             &
                         tfield_l1d = null(),             &
                         tfield_n0d = null(),             &
                         tfield_n1d = null(),             &
                         tfield_n2d = null(),             &
                         tfield_n3d = null(),             &
                         tfield_x0d = null(),             &
                         tfield_x1d = null(),             &
                         tfield_x2d = null(),             &
                         tfield_x3d = null(),             &
                         tfield_x4d = null(),             &
                         tfield_x5d = null(),             &
                         tfield_x6d = null(),             &
                         tfield_t0d = null(),             &
                         tfield_t1d = null()              )
#else
  tpfield = tfielddata( tfieldmetadata( &
        cmnhname   = cmnhname,          &
        cstdname   = cstdname,          &
        clongname  = clongname,         &
        cunits     = cunits,            &
        ccomment   = ccomment,          &
        ngrid      = ngrid,             &
        ntype      = ntype,             &
        ndims      = ndims,             &
        ndimlist   = ndimlist,          &
        nfillvalue = nfillvalue,        &
        xfillvalue = xfillvalue,        &
        nvalidmin  = nvalidmin,         &
        nvalidmax  = nvalidmax,         &
        xvalidmin  = xvalidmin,         &
        xvalidmax  = xvalidmax,         &
        cdir       = cdir,              &
        clbtype    = clbtype,           &
        ltimedep   = ltimedep           ) )

  ! Set nmodelmax to 0 instead of -1 by default.
  ! This value can therefore be used to determine if the field was initialized by calling this constructor.
  tpfield%nmodelmax = 0
#endif

end function Fill_tfielddata

end module modd_field
