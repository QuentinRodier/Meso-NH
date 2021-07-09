!MNH_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ###################### 
      MODULE MODI_READ_EMISS_DATA
!     ######################
!


USE MODD_PARAM_ECRAD_n, ONLY : USER_EMISS

implicit none

CONTAINS


!
      SUBROUTINE READ_EMISS_DATA(SURF_TYPE)
!
use easy_netcdf, only : netcdf_file
use radiation_config, only: get_enum_code


character(len=*), intent(in) :: SURF_TYPE


character(len=*), parameter :: EmissModelName(0:5) = (/ 'OCEA', &
                                               &        'VEGE',&
                                               &        'DESE',&
                                               &        'SNOW',&
                                               &        'ZERO',&
                                               &        'UNIT'    /)
                                               
type(netcdf_file)  :: file
character(len=255)    :: emiss_data_name 
integer :: NEMISS
real, allocatable :: ALL_EMISS(:,:)

emiss_data_name = '/home/liboisq/MesoNH/MNH-V5-4-4/src/QUENTIN/ecrad-1.4.0/data/spectral_emissivity.nc'

! Determine emissivity model
call get_enum_code(SURF_TYPE, EmissModelName, &
        &            ' ', NEMISS) 
        

! Open the file and configure the way it is read
call file%open(trim(emiss_data_name), iverbose=1)
call file%transpose_matrices()

! Read the emissivities
call file%get_real_matrix('emissivity', ALL_EMISS)

USER_EMISS(:) = ALL_EMISS(NEMISS+1,:)

call file%close()


END SUBROUTINE READ_EMISS_DATA

END MODULE MODI_READ_EMISS_DATA

