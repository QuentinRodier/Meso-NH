! radsurf_homogeneous_vegetation.f90 - Compute radiative transfer in homogeneous vegetation canopy
!
! (C) Copyright 2018- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
!

module radsurf_homogeneous_vegetation

contains
  subroutine calc_boundary_conditions_sw(config, tile_fraction, &
       &  canopy_depth, vegetation_optical_depth, vegetation_albedo, &
       &  ground_albedo_diffuse, ground_albedo_direct, &
       &  ref_dif, tra_dif, ref_dir, tra_dir_dif, tra_dir_dir, &
       &  albedo_diffuse_reg, albedo_direct_reg, &
       &  albedo_diffuse_out, albedo_direct_out, &
       &  ext_air, ssa_air, g_air)

    use parkind1, only :jprb
    use radiation_config, only : config_type
    
    implicit none

    ! Number of regions
    integer, parameter :: nreg = 2

    type(config_type), intent(in) :: config

    ! Fraction of gridbox occupied by this tile
    real(kind=jprb),   intent(in) :: tile_fraction

    ! Depth of vegetation canopy in metres
    real(kind=jprb),   intent(in) :: canopy_depth

    ! Optical properties of vegetation
    real(kind=jprb),   intent(in) :: vegetation_optical_depth
    real(kind=jprb),   intent(in) :: vegetation_albedo(:)  ! Spectral interval

    ! Optical properties of the ground (function of spectral interval)
    real(kind=jprb),   intent(in) :: ground_albedo_diffuse(:)
    real(kind=jprb),   intent(in) :: ground_albedo_direct(:)

    ! Intermediate properties to store
    real(kind=jprb),   intent(in), dimension(nreg,nreg) :: ref_dif, ref_dir, tra_dif, tra_dir_dif, tra_dir_dir

    ! Outputs
    real(kind=jprb),   intent(inout) :: albedo_diffuse_reg, albedo_direct_reg, albedo_diffuse_out, albedo_direct_out

    real(kind=jprb),   intent(inout) :: ext_air, ssa_air, g_air
     
  end subroutine calc_boundary_conditions_sw

  subroutine calc_boundary_conditions_lw

  end subroutine calc_boundary_conditions_lw


end module radsurf_homogeneous_vegetation
