!SFX_LIC Copyright 2019-2019 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
! Author: P. Wautelet 18/09/2019
module modd_netcdf_sfx

#ifdef SFX_MNH
use modd_netcdf, only: IDCDF_KIND
#endif

implicit none

#ifndef SFX_MNH
integer, parameter :: IDCDF_KIND = selected_int_kind( 8 )
#endif

end module modd_netcdf_sfx
