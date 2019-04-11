!MNH_LIC Copyright 2019-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author:
!  P. Wautelet 08/03/2019
! Modifications:
!  P. Wautelet 22/03/2019: add MNHINT/REAL32/64_MPI, MNH2REAL32/64_MPI + more public parameters
!  P. Wautelet 27/03/2019: add MNHTIME and MNHTIME_MPI
!-----------------------------------------------------------------
module modd_precision

use modd_mpif

#ifdef MNH_IOCDF4
use NETCDF, only: NF90_DOUBLE, NF90_FLOAT, NF90_INT, NF90_INT64
#endif

implicit none

private

public :: MNHINT32, MNHINT64, MNHREAL32, MNHREAL64, MNHREAL128

public :: MNHINT32_MPI,   MNHINT64_MPI
public :: MNHREAL32_MPI,  MNHREAL64_MPI
public :: MNH2REAL32_MPI, MNH2REAL64_MPI

public :: MNHINT, MNHREAL
public :: MNHINT_MPI, MNHREAL_MPI, MNH2REAL_MPI
public :: MNHTIME, MNHTIME_MPI

public :: LFIINT

#ifdef MNH_IOCDF4
public :: CDFINT, MNHINT_NF90, MNHREAL_NF90
#endif


integer, parameter :: MNHINT32 = selected_int_kind( r = 9 )
integer, parameter :: MNHINT64 = selected_int_kind( r = 18 )

integer, parameter :: MNHREAL32  = selected_real_kind( p = 6,  r = 37 )
integer, parameter :: MNHREAL64  = selected_real_kind( p = 15, r = 307 )
integer, parameter :: MNHREAL128 = selected_real_kind( p = 33, r = 4931 )

integer, parameter :: MNHINT32_MPI  = MPI_INTEGER4
integer, parameter :: MNHINT64_MPI  = MPI_INTEGER8

integer, parameter :: MNHREAL32_MPI  = MPI_REAL4
integer, parameter :: MNHREAL64_MPI  = MPI_REAL8

integer, parameter :: MNH2REAL32_MPI  = MPI_2REAL
integer, parameter :: MNH2REAL64_MPI  = MPI_2DOUBLE_PRECISION


! Kinds for MesoNH
#if ( MNH_INT == 4 )
integer, parameter :: MNHINT     = MNHINT32
integer, parameter :: MNHINT_MPI = MNHINT32_MPI
#elif ( MNH_INT == 8 )
integer, parameter :: MNHINT     = MNHINT64
integer, parameter :: MNHINT_MPI = MNHINT64_MPI
#else
#error "Invalid MNH_INT"
#endif

#if ( MNH_REAL == 4 )
integer, parameter :: MNHREAL      = MNHREAL32
integer, parameter :: MNHREAL_MPI  = MNHREAL32_MPI
integer, parameter :: MNH2REAL_MPI = MNH2REAL32_MPI
#elif ( MNH_REAL == 8 )
integer, parameter :: MNHREAL      = MNHREAL64
integer, parameter :: MNHREAL_MPI  = MNHREAL64_MPI
integer, parameter :: MNH2REAL_MPI = MNH2REAL64_MPI
#elif ( MNH_REAL == 16 )
integer, parameter :: MNHREAL     = MNHREAL128
integer, parameter :: MNHREAL_MPI = MPI_REAL16
#error "No MNH2REAL_MPI for MNH_REAL=16"
#else
#error "Invalid MNH_REAL"
#endif

integer, parameter :: MNHTIME     = MNHREAL64
integer, parameter :: MNHTIME_MPI = MNHREAL64_MPI


! Kinds for LFI
#if ( LFI_INT == 4 )
integer, parameter :: LFIINT = MNHINT32
#elif ( LFI_INT == 8 )
integer, parameter :: LFIINT = MNHINT64
#else
#error "Invalid LFI_INT"
#endif


#ifdef MNH_IOCDF4
! Kinds for netCDF
integer, parameter :: CDFINT = selected_int_kind( r = 9 )

#if (MNH_INT == 4)
integer, parameter :: MNHINT_NF90 = NF90_INT
#elif (MNH_INT == 8)
integer, parameter :: MNHINT_NF90 = NF90_INT64
#else
#error "Invalid MNH_INT"
#endif

#if (MNH_REAL == 4)
integer, parameter :: MNHREAL_NF90 = NF90_FLOAT
#elif (MNH_REAL == 8)
integer, parameter :: MNHREAL_NF90 = NF90_DOUBLE
#else
#error "Invalid MNH_REAL"
#endif
#endif

end module modd_precision
