!MNH_LIC Copyright 2019-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------

!################
module mode_tools
!################
!
!    Purpose
!    -------
!
!     The Purpose of this module is to provide useful tools for MesoNH
!
!    Author
!    ------
!     P. Wautelet 14/02/2019
!
! Modifications:
!  P. Wautelet 28/05/2019: move COUNTJV function to tools.f90

implicit none

private

public :: Countjv
public :: Upcase

interface Countjv
  module procedure Countjv2d, Countjv3d
end interface


contains

function Countjv2d(ltab,i1,i2) result(ic)
  logical, dimension(:,:), intent(in)  :: ltab   ! Mask
  integer, dimension(:),   intent(out) :: i1, i2 ! Positions of elements with 'true' value
  integer                              :: ic     ! Total number of 'true' values

  integer :: ji, jj

  ic = 0

  do jj = 1, size( ltab, 2 )
    do ji = 1, size( ltab, 1 )
      if ( ltab(ji, jj ) ) then
        ic = ic +1
        i1(ic) = ji
        i2(ic) = jj
      end if
    end do
  end do
end function Countjv2d


function Countjv3d(ltab,i1,i2,i3) result(ic)
  logical, dimension(:,:,:), intent(in)  :: ltab       ! Mask
  integer, dimension(:),     intent(out) :: i1, i2, i3 ! Positions of elements with 'true' value
  integer                                :: ic         ! Total number of 'true' values

  integer :: ji, jj, jk

  ic = 0

  do jk = 1, size( ltab, 3 )
    do jj = 1, size( ltab, 2 )
      do ji = 1, size( ltab, 1 )
        if ( ltab(ji, jj, jk ) ) then
          ic = ic +1
          i1(ic) = ji
          i2(ic) = jj
          i3(ic) = jk
        end if
      end do
    end do
  end do
end function Countjv3d


function Upcase(hstring)
  character(len=*), intent(in) :: hstring
  character(len=len(hstring))  :: upcase

  integer :: jc
  integer, parameter :: iamin = iachar("a")
  integer, parameter :: iamaj = iachar("A")

  do jc = 1,len(hstring)
    if ( hstring(jc:jc) >= "a" .and. hstring(jc:jc) <= "z" ) then
      upcase(jc:jc) = achar( iachar( hstring(jc:jc) ) - iamin + iamaj )
    else
      upcase(jc:jc) = hstring(jc:jc)
    end if
  end do
end function Upcase

end module mode_tools
