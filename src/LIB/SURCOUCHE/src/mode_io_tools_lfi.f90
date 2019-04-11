!MNH_LIC Copyright 2018-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author(s):
!  P. Wautelet : 14/12/2018
! Modifications:
!  P. Wautelet 05/03/2019: rename IO subroutines and modules
!-----------------------------------------------------------------
module mode_io_tools_lfi

use modd_io,        only: tfiledata
use modd_precision, only: LFIINT

implicit none

private

public :: IO_Verbosity_prepare_lfi

contains

subroutine IO_Verbosity_prepare_lfi(tpfile, kmelev, ostats)
  type(tfiledata),      intent(in)  :: tpfile
  integer(kind=LFIINT), intent(out) :: kmelev
  logical,              intent(out) :: ostats

  select case (tpfile%nlfiverb)
    case(:2)
      ostats = .false.
      kmelev = 0
    case(3:6)
      ostats = .false.
      kmelev = 1
    case(7:9)
      ostats = .false.
      kmelev = 2
    case(10:)
      ostats = .true.
      kmelev = 2
  end select

end subroutine IO_Verbosity_prepare_lfi


end module mode_io_tools_lfi
