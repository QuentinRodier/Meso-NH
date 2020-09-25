!MNH_LIC Copyright 2016-2020 CNRS, Meteo-France and Universite Paul Sabatier
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
!  P. Wautelet 27/01/2020: create the tfield_metadata_base abstract datatype
!  P. Wautelet 14/09/2020: add ndimlist field to tfield_metadata_base
!-----------------------------------------------------------------
module modd_field

use modd_parameters, only: NGRIDUNKNOWN, NMNHNAMELGTMAX, NSTDNAMELGTMAX
use modd_type_date,  only: date_time
#ifdef MNH_IOCDF4
use NETCDF,          only: NF90_FILL_INT, NF90_FILL_REAL
#endif

implicit none

integer, parameter :: NMNHMAXDIMS = 6
INTEGER,PARAMETER :: MAXFIELDS = 250
INTEGER,PARAMETER :: TYPEUNDEF = -1, TYPEINT = 1, TYPELOG = 2, TYPEREAL = 3, TYPECHAR = 4, TYPEDATE = 5
!
integer, parameter :: NMNHDIM_UNKNOWN             = -2

integer, parameter :: NMNHDIM_ONE                 = 10
integer, parameter :: NMNHDIM_COMPLEX             = 11

integer, parameter :: NMNHDIM_NI                  = 21
integer, parameter :: NMNHDIM_NJ                  = 22
integer, parameter :: NMNHDIM_NI_U                = 23
integer, parameter :: NMNHDIM_NJ_U                = 24
integer, parameter :: NMNHDIM_NI_V                = 25
integer, parameter :: NMNHDIM_NJ_V                = 26
integer, parameter :: NMNHDIM_LEVEL               = 27
integer, parameter :: NMNHDIM_LEVEL_W             = 28
integer, parameter :: NMNHDIM_TIME                = 29

integer, parameter :: NMNHDIM_BUDGET_CART_NI      = 30
integer, parameter :: NMNHDIM_BUDGET_CART_NJ      = 31
integer, parameter :: NMNHDIM_BUDGET_CART_NI_U    = 32
integer, parameter :: NMNHDIM_BUDGET_CART_NJ_U    = 33
integer, parameter :: NMNHDIM_BUDGET_CART_NI_V    = 34
integer, parameter :: NMNHDIM_BUDGET_CART_NJ_V    = 35
integer, parameter :: NMNHDIM_BUDGET_CART_LEVEL   = 36
integer, parameter :: NMNHDIM_BUDGET_CART_LEVEL_W = 37

integer, parameter :: NMNHDIM_BUDGET_MASK_LEVEL   = 40
integer, parameter :: NMNHDIM_BUDGET_MASK_LEVEL_W = 41
integer, parameter :: NMNHDIM_BUDGET_MASK_TIME    = 42
integer, parameter :: NMNHDIM_BUDGET_MASK_NBUMASK = 43

integer, parameter :: NMNHDIM_BUDGET_LES_TIME     = 50
integer, parameter :: NMNHDIM_BUDGET_LES_AVG_TIME = 51
integer, parameter :: NMNHDIM_BUDGET_LES_LEVEL    = 52
integer, parameter :: NMNHDIM_BUDGET_LES_SV       = 53
integer, parameter :: NMNHDIM_BUDGET_LES_MASK     = 54

integer, parameter :: NMNHDIM_SPECTRA_2PTS_NI     = 60
integer, parameter :: NMNHDIM_SPECTRA_2PTS_NJ     = 61
integer, parameter :: NMNHDIM_SPECTRA_SPEC_NI     = 62
integer, parameter :: NMNHDIM_SPECTRA_SPEC_NJ     = 63
integer, parameter :: NMNHDIM_SPECTRA_LEVEL       = 64

integer, parameter :: NMNHDIM_BUDGET_NGROUPS      = 100 ! This is not a true dimension

integer, parameter :: NMNHDIM_UNUSED              = 200
!
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
type, abstract :: tfield_metadata_base
  CHARACTER(LEN=NMNHNAMELGTMAX) :: CMNHNAME  = '' !Name of the field (for MesoNH, non CF convention)
  CHARACTER(LEN=NSTDNAMELGTMAX) :: CSTDNAME  = '' !Standard name (CF convention)
  CHARACTER(LEN=32)  :: CLONGNAME = '' !Long name (CF convention)
  CHARACTER(LEN=40)  :: CUNITS    = '' !Canonical units (CF convention)
  CHARACTER(LEN=100) :: CCOMMENT  = '' !Comment (for MesoNH, non CF convention)
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
end type tfield_metadata_base

!Structure describing the characteristics of a field
TYPE, extends( tfield_metadata_base ) :: TFIELDDATA
  CHARACTER(LEN=2)   :: CDIR      = '' !Type of the data field (XX,XY,--...)
  CHARACTER(LEN=4)   :: CLBTYPE   = 'NONE' !Type of the lateral boundary (LBX,LBY,LBXU,LBYV)
  LOGICAL            :: LTIMEDEP  = .FALSE. !Is the field time-dependent?
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
integer, save :: NMODEL_ALLOCATED
LOGICAL, SAVE :: LFIELDLIST_ISINIT = .FALSE.
TYPE(TFIELDDATA),DIMENSION(MAXFIELDS),SAVE :: TFIELDLIST

end module modd_field
