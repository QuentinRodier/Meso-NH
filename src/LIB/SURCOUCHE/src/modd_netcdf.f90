!MNH_LIC Copyright 1994-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 10/11/2020: new data structures for netCDF dimensions
!-----------------------------------------------------------------
module modd_netcdf

use modd_precision, only: CDFINT

implicit none

public

integer, parameter :: NMAXDIMNAMELGTNC4 = 16

!Datatype to store metadata of 1 dimension
type tdimnc
  character(len=NMAXDIMNAMELGTNC4) :: cname = ''
  integer(kind=CDFINT)             :: nlen  = -1
  integer(kind=CDFINT)             :: nid   = -1
end type tdimnc

!Datatype to store dimension metadata of a netCDF file
type tdimsnc
  integer :: nmaxdims = 0
  type(tdimnc), dimension(:), allocatable :: tdims
  integer :: nmaxdims_str = 0                          ! For character strings
  type(tdimnc), dimension(:), allocatable :: tdims_str ! For character strings
end type tdimsnc

end module modd_netcdf
