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

implicit none

private

public :: upcase

contains

function upcase(hstring)
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
end function upcase

end module mode_tools
