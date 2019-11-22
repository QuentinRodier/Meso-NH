!MNH_LIC Copyright 2018-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------

!#############
module modi_tools_c
!#############
!
!!    Purpose
!!    -------
!
!     The Purpose of this module is to provide interfaces
!     to C functions
!
!    Authors
!    -------
!
!     P. Wautelet 04/12/2018
!
! Modifications:
!  P. Wautelet 18/09/2019: correct support of 64bit integers (MNH_INT=8)

  use, intrinsic :: iso_c_binding

  implicit none

  private

  public :: sleep_c

  interface
    subroutine sleep_c_intern(ksec) bind(c, name="sleep")
      import C_INT
      integer(kind=C_INT), VALUE :: ksec
    end subroutine sleep_c_intern
  end interface

contains

  subroutine sleep_c(ksec)
    integer, intent(in) :: ksec

    integer(kind=C_INT) :: isec_c

    isec_c = int( ksec, kind=C_INT )

    call sleep_c_intern( isec_c )
  end subroutine

end module modi_tools_c
