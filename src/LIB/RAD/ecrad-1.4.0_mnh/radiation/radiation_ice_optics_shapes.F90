!MNH_LIC Copyright 2023-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
! radiation_ice_optics_shapes.F90 - 
!
! Authors: Marie Taufour
! Email:   marie.taufour@aero.obs-mip.fr
!
!
module radiation_ice_optics_shapes

  implicit none
  public

  ! The number of ice coefficients depends on the parameterization
  integer, parameter :: NIceOpticsCoeffsShapesSW  = 69
  integer, parameter :: NIceOpticsCoeffsShapesLW  = 69

  integer, parameter :: NSingleCoeffs = 23

contains

  !---------------------------------------------------------------------
  ! Compute shortwave ice-particle scattering properties using Yi et
  ! al. (2013) parameterization
  subroutine calc_ice_optics_shapes_sw(nb, coeff, ice_wp, &
       &  re, od, scat_od, g)

    use parkind1, only : jprb, jpim
    !use yomhook,  only : lhook, dr_hook

    ! Number of bands
    integer, intent(in)  :: nb
    ! Coefficients read from a data file
    real(jprb), intent(in) :: coeff(:,:)
    ! Ice water path (kg m-2)
    real(jprb), intent(in) :: ice_wp
    ! Effective radius (m)
    real(jprb), intent(in) :: re
    ! Total optical depth, scattering optical depth and asymmetry factor
    real(jprb), intent(out) :: od(nb), scat_od(nb), g(nb)

    ! Yi's effective diameter (microns)
    real(jprb) :: de_um
    ! Ice water path in g m-2
    real (jprb) :: iwp_gm_2
    ! LUT temp variables
    real(jprb) :: wts_1, wts_2
    integer(jpim) :: lu_idx
    real(kind=jprb), parameter    :: lu_scale  = 0.2_jprb
    real(kind=jprb), parameter    :: lu_offset = 1.0_jprb
    !real(jprb)  :: hook_handle

    !if (lhook) call dr_hook('radiation_ice_optics:calc_ice_optics_yi_sw',0,hook_handle)

    ! Convert to effective diameter using the relationship in the IFS
    !de_um     = re * (1.0e6_jprb / 0.64952_jprb)
    de_um     = re * 2.0e6_jprb

    ! limit de_um to validity of LUT
    de_um = max(de_um,10.0_jprb)
    de_um = min(de_um,119.99_jprb) !avoid greater than or equal to 120 um

    iwp_gm_2  = ice_wp * 1000.0_jprb

    lu_idx = floor(de_um * lu_scale - lu_offset)
    wts_2  = (de_um * lu_scale - lu_offset) - lu_idx
    wts_1  = 1.0_jprb - wts_2
    od     = 0.001_jprb * iwp_gm_2 * & 
             & ( wts_1 * coeff(1:nb,lu_idx) + wts_2 * coeff(1:nb,lu_idx+1) )
    scat_od = od * & 
             & ( wts_1 * coeff(1:nb,lu_idx+NSingleCoeffs) + wts_2 * coeff(1:nb,lu_idx+NSingleCoeffs+1) )
    g = wts_1 * coeff(1:nb,lu_idx+2*NSingleCoeffs) + wts_2 * coeff(1:nb,lu_idx+2*NSingleCoeffs+1)

    !if (lhook) call dr_hook('radiation_ice_optics:calc_ice_optics_yi_sw',1,hook_handle)

  end subroutine calc_ice_optics_shapes_sw


  !---------------------------------------------------------------------
  ! Compute longwave ice-particle scattering properties using .....
  subroutine calc_ice_optics_shapes_lw(nb, coeff, ice_wp, &
       &  re, od, scat_od, g)

    use parkind1, only : jprb, jpim
    !use yomhook,  only : lhook, dr_hook

    ! Number of bands
    integer, intent(in)  :: nb
    ! Coefficients read from a data file
    real(jprb), intent(in) :: coeff(:,:)
    ! Ice water path (kg m-2)
    real(jprb), intent(in) :: ice_wp
    ! Effective radius (m)
    real(jprb), intent(in) :: re
    ! Total optical depth, scattering optical depth and asymmetry factor
    real(jprb), intent(out) :: od(nb), scat_od(nb), g(nb)

    ! Yi's effective diameter (microns)
    real(jprb) :: de_um
    ! Ice water path in g m-2
    real (jprb) :: iwp_gm_2
    ! LUT temp variables
    real(jprb) :: wts_1, wts_2
    integer(jpim) :: lu_idx
    real(kind=jprb), parameter    :: lu_scale  = 0.2_jprb
    real(kind=jprb), parameter    :: lu_offset = 1.0_jprb
    !real(jprb)  :: hook_handle

    !if (lhook) call dr_hook('radiation_ice_optics:calc_ice_optics_yi_sw',0,hook_handle)

    ! Convert to effective diameter using the relationship in the IFS
    !de_um     = re * (1.0e6_jprb / 0.64952_jprb)
    de_um     = re * 2.0e6_jprb

    ! limit de_um to validity of LUT
    de_um = max(de_um,10.0_jprb)
    de_um = min(de_um,119.99_jprb) !avoid greater than or equal to 120 um

    iwp_gm_2  = ice_wp * 1000.0_jprb

    lu_idx = floor(de_um * lu_scale - lu_offset)
    wts_2  = (de_um * lu_scale - lu_offset) - lu_idx
    wts_1  = 1.0_jprb - wts_2
    od     = 0.001_jprb * iwp_gm_2 * & 
             & ( wts_1 * coeff(1:nb,lu_idx) + wts_2 * coeff(1:nb,lu_idx+1) )
    scat_od = od * & 
             & ( wts_1 * coeff(1:nb,lu_idx+NSingleCoeffs) + wts_2 * coeff(1:nb,lu_idx+NSingleCoeffs+1) )
    g = wts_1 * coeff(1:nb,lu_idx+2*NSingleCoeffs) + wts_2 * coeff(1:nb,lu_idx+2*NSingleCoeffs+1)

     !if (lhook) call dr_hook('radiation_ice_optics:calc_ice_optics_yi_lw',1,hook_handle)

  end subroutine calc_ice_optics_shapes_lw

end module radiation_ice_optics_shapes
