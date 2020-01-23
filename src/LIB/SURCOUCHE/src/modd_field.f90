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
!-----------------------------------------------------------------
module modd_field

use modd_parameters, only: NGRIDUNKNOWN, NMNHNAMELGTMAX, NSTDNAMELGTMAX
use modd_type_date,  only: date_time
#if defined(MNH_IOCDF4)
use NETCDF,          only: NF90_FILL_INT, NF90_FILL_REAL
#endif

implicit none

INTEGER,PARAMETER :: MAXFIELDS = 250
INTEGER,PARAMETER :: TYPEUNDEF = -1, TYPEINT = 1, TYPELOG = 2, TYPEREAL = 3, TYPECHAR = 4, TYPEDATE = 5
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
!Structure describing the characteristics of a field
TYPE :: TFIELDDATA
  CHARACTER(LEN=NMNHNAMELGTMAX) :: CMNHNAME  = '' !Name of the field (for MesoNH, non CF convention)
  CHARACTER(LEN=NSTDNAMELGTMAX) :: CSTDNAME  = '' !Standard name (CF convention)
  CHARACTER(LEN=32)  :: CLONGNAME = '' !Long name (CF convention)
  CHARACTER(LEN=40)  :: CUNITS    = '' !Canonical units (CF convention)
  CHARACTER(LEN=2)   :: CDIR      = '' !Type of the data field (XX,XY,--...)
  CHARACTER(LEN=4)   :: CLBTYPE   = 'NONE' !Type of the lateral boundary (LBX,LBY,LBXU,LBYV)
  CHARACTER(LEN=100) :: CCOMMENT  = '' !Comment (for MesoNH, non CF convention)
  INTEGER            :: NGRID     = NGRIDUNKNOWN !Localization on the model grid
  INTEGER            :: NTYPE     = TYPEUNDEF !Datatype
  INTEGER            :: NDIMS     = 0  !Number of dimensions
  LOGICAL            :: LTIMEDEP  = .FALSE. !Is the field time-dependent?
  !
#if defined(MNH_IOCDF4)
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
