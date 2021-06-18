!MNH_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ###################### 
      MODULE MODI_READ_ALBEDO_DATA
!     ######################
!


USE MODD_PARAM_ECRAD_n, ONLY : USER_ALB_DIFF, USER_ALB_DIR

implicit none

CONTAINS

      SUBROUTINE READ_ALBEDO_DATA(SURF_TYPE)

use easy_netcdf, only : netcdf_file
use radiation_config, only: get_enum_code

character(len=*), intent(in) :: SURF_TYPE

!enum, bind(c) 
 !    enumerator IAlbedoSnow, &
  !        &     IAlbedoGrass
  !end enum
character(len=*), parameter :: AlbedoModelName(0:5) =  (/ 'OCEA', &
                                               &        'VEGE',&
                                               &        'DESE',&
                                               &        'SNOW',&
                                               &        'ZERO',&
                                               &        'UNIT'    /)
                                               
type(netcdf_file)  :: file
character(len=255) :: albedo_data_name 
integer :: NALB
real, allocatable :: ALL_ALB_DIFF(:,:)
real, allocatable :: ALL_ALB_DIR(:,:)

albedo_data_name = '/home/liboisq/MesoNH/MNH-V5-4-4/src/QUENTIN/ecrad-1.4.0/data/spectral_albedo.nc'

! Determine albedo model
call get_enum_code(SURF_TYPE, AlbedoModelName, &
        &            ' ', NALB) 
        
! Open the file and configure the way it is read
call file%open(trim(albedo_data_name), iverbose=1)
call file%transpose_matrices()

! Read the albedos
call file%get_real_matrix('albedo_diff', ALL_ALB_DIFF)
call file%get_real_matrix('albedo_dir', ALL_ALB_DIR)
USER_ALB_DIFF(:) = ALL_ALB_DIFF(NALB+1,:)  ! because index starts at 0
USER_ALB_DIR(:) = ALL_ALB_DIR(NALB+1,:)

call file%close()


END SUBROUTINE READ_ALBEDO_DATA

END MODULE MODI_READ_ALBEDO_DATA

