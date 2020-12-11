!MNH_LIC Copyright 2020-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author: P. Wautelet 25/06/2020 (deduplication of code from advection_metsv, resolved_cloud and turb)
! Modifications:
!  P. Wautelet 30/06/2020: remove non-local corrections in resolved_cloud for NEGA => new local corrections here
!  J. Escobar  21/07/2020: bug <-> array of size(:,:,:,0) => return if krr=0
!-----------------------------------------------------------------
module mode_sources_neg_correct

implicit none

private

public :: Sources_neg_correct

contains

subroutine Sources_neg_correct( hcloud, hbudname, krr, ptstep, ppabst, ptht, prt, prths, prrs, prsvs, prhodj )

use modd_budget,     only: lbudget_th, lbudget_rv, lbudget_rc, lbudget_rr, lbudget_ri, &
                           lbudget_rs, lbudget_rg, lbudget_rh, lbudget_sv
use modd_cst,        only: xci, xcl, xcpd, xcpv, xlstt, xlvtt, xp00, xrd, xtt
use modd_nsv,        only: nsv_c2r2beg, nsv_lima_beg, nsv_lima_end, nsv_lima_nc, nsv_lima_nr, nsv_lima_ni
use modd_param_lima, only: lcold_lima => lcold, lrain_lima => lrain, lwarm_lima => lwarm, &
                           xctmin_lima => xctmin, xrtmin_lima => xrtmin

use mode_msg

use modi_budget

implicit none

character(len=*),            intent(in)           :: hcloud   ! Kind of cloud parameterization
character(len=*),            intent(in)           :: hbudname ! Budget name
integer,                     intent(in)           :: krr      ! Number of moist variables
real,                        intent(in)           :: ptstep   ! Timestep
real, dimension(:, :, :),    intent(in)           :: ppabst   ! Absolute pressure at time t
real, dimension(:, :, :),    intent(in)           :: ptht     ! Theta at time t
real, dimension(:, :, :, :), intent(in)           :: prt      ! Moist variables at time t
real, dimension(:, :, :),    intent(inout)        :: prths    ! Source terms
real, dimension(:, :, :, :), intent(inout)        :: prrs     ! Source terms
real, dimension(:, :, :, :), intent(inout)        :: prsvs    ! Source terms
real, dimension(:, :, :),    intent(in), optional :: prhodj   ! Dry density * jacobian

integer :: ji, jj, jk
integer :: jr
integer :: jrmax
integer :: jsv
real, dimension(:, :, :), allocatable :: zt, zexn, zlv, zls, zcph, zcor

if (krr .eq. 0 ) return

if ( hbudname /= 'NEADV' .and. hbudname /= 'NECON' .and. hbudname /= 'NEGA' .and. hbudname /= 'NETUR' ) &
  call Print_msg( NVERB_WARNING, 'GEN', 'Sources_neg_correct', 'budget '//hbudname//' not yet tested' )

allocate( zt  ( Size( prths, 1 ), Size( prths, 2 ), Size( prths, 3 ) ) )
allocate( zexn( Size( prths, 1 ), Size( prths, 2 ), Size( prths, 3 ) ) )
allocate( zlv ( Size( prths, 1 ), Size( prths, 2 ), Size( prths, 3 ) ) )
allocate( zcph( Size( prths, 1 ), Size( prths, 2 ), Size( prths, 3 ) ) )

zexn(:, :, :) = ( ppabst(:, :, :) / xp00 ) ** (xrd / xcpd )
zt  (:, :, :) = ptht(:, :, :) * zexn(:, :, :)
zlv (:, :, :) = xlvtt + ( xcpv - xcl ) * ( zt(:, :, :) - xtt )
if ( hcloud == 'ICE3' .or. hcloud == 'ICE4' .or. hcloud == 'LIMA' ) then
  allocate( zls( Size( prths, 1 ), Size( prths, 2 ), Size( prths, 3 ) ) )
  zls(:, :, :) = xlstt + ( xcpv - xci ) * ( zt(:, :, :) - xtt )
end if
zcph(:, :, :) = xcpd + xcpv * prt(:, :, :, 1)

deallocate( zt )

CLOUD: select case ( hcloud )
  case ( 'KESS' )
    jrmax = Size( prrs, 4 )
    do jr = 2, jrmax
      where ( prrs(:, :, :, jr) < 0. )
        prrs(:, :, :, 1) = prrs(:, :, :, 1) + prrs(:, :, :, jr)
        prths(:, :, :) = prths(:, :, :) - prrs(:, :, :, jr) * zlv(:, :, :) /  &
           ( zcph(:, :, :) * zexn(:, :, :) )
        prrs(:, :, :, jr) = 0.
      end where
    end do

    where ( prrs(:, :, :, 1) < 0. .and. prrs(:, :, :, 2) > 0. )
      prrs(:, :, :, 1) = prrs(:, :, :, 1) + prrs(:, :, :, 2)
      prths(:, :, :) = prths(:, :, :) - prrs(:, :, :, 2) * zlv(:, :, :) /  &
           ( zcph(:, :, :) * zexn(:, :, :) )
      prrs(:, :, :, 2) = 0.
    end where
    prrs(:,:,:,1) = max(0.0,prrs(:,:,:,1))


  case( 'ICE3', 'ICE4' )
    if ( hbudname == 'NETUR' ) then
      jrmax = 4
    else
      jrmax = Size( prrs, 4 )
    end if
    do jr = 4, jrmax
      where ( prrs(:, :, :, jr) < 0.)
        prrs(:, :, :, 1) = prrs(:, :, :, 1) + prrs(:, :, :, jr)
        prths(:, :, :) = prths(:, :, :) - prrs(:, :, :, jr) * zls(:, :, :) /  &
           ( zcph(:, :, :) * zexn(:, :, :) )
        prrs(:, :, :, jr) = 0.
      end where
    end do
!
!   cloud
    if ( hbudname == 'NETUR' ) then
      jrmax = 2
    else
      jrmax = 3
    end if
    do jr = 2, jrmax
      where ( prrs(:, :, :, jr) < 0.)
        prrs(:, :, :, 1) = prrs(:, :, :, 1) + prrs(:, :, :, jr)
        prths(:, :, :) = prths(:, :, :) - prrs(:, :, :, jr) * zlv(:, :, :) /  &
           ( zcph(:, :, :) * zexn(:, :, :) )
        prrs(:, :, :, jr) = 0.
      end where
    end do
!
! if rc or ri are positive, we can correct negative rv
!   cloud
    where ( prrs(:, :, :, 1) < 0. .and. prrs(:, :, :, 2) > 0. )
      prrs(:, :, :, 1) = prrs(:, :, :, 1) + prrs(:, :, :, 2)
      prths(:, :, :) = prths(:, :, :) - prrs(:, :, :, 2) * zlv(:, :, :) /  &
           ( zcph(:, :, :) * zexn(:, :, :) )
      prrs(:, :, :, 2) = 0.
    end where
!   ice
    if ( krr > 3 ) then
      allocate( zcor( Size( prths, 1 ), Size( prths, 2 ), Size( prths, 3 ) ) )
      where ( prrs(:, :, :, 1) < 0. .and. prrs(:, :, :, 4) > 0. )
        zcor(:, :, :) = Min( -prrs(:, :, :, 1), prrs(:, :, :, 4) )
        prrs(:, :, :, 1) = prrs(:, :, :, 1) + zcor(:, :, :)
        prths(:, :, :) = prths(:, :, :) - zcor(:, :, :) * zls(:, :, :) /  &
             ( zcph(:, :, :) * zexn(:, :, :) )
        prrs(:, :, :, 4) = prrs(:, :, :, 4) - zcor(:, :, :)
      end where
    end if
   prrs(:,:,:,1) = max(0.0,prrs(:,:,:,1))

!
!
  case( 'C2R2', 'KHKO' )
    where ( prrs(:, :, :, 2) < 0. .or. prsvs(:, :, :, nsv_c2r2beg + 1) < 0. )
      prsvs(:, :, :, nsv_c2r2beg) = 0.
    end where
    do jsv = 2, 3
      where ( prrs(:, :, :, jsv) < 0. .or. prsvs(:, :, :, nsv_c2r2beg - 1 + jsv) < 0. )
        prrs(:, :, :, 1) = prrs(:, :, :, 1) + prrs(:, :, :, jsv)
        prths(:, :, :) = prths(:, :, :) - prrs(:, :, :, jsv) * zlv(:, :, :) /  &
                ( zcph(:, :, :) * zexn(:, :, :) )
        prrs(:, :, :, jsv)  = 0.
        prsvs(:, :, :, nsv_c2r2beg - 1 + jsv) = 0.
      end where
    end do
    where ( prrs(:, :, :, 1) < 0. .and. prrs(:, :, :, 2) > 0. )
      prrs(:, :, :, 1) = prrs(:, :, :, 1) + prrs(:, :, :, 2)
      prths(:, :, :) = prths(:, :, :) - prrs(:, :, :, 2) * zlv(:, :, :) /  &
           ( zcph(:, :, :) * zexn(:, :, :) )
      prrs(:, :, :, 2) = 0.
      prsvs(:, :, :, nsv_c2r2beg + 1) = 0.
    end where
    prrs(:,:,:,1) = max(0.0,prrs(:,:,:,1))

!
!
  case( 'LIMA' )
! Correction where rc<0 or Nc<0
    if ( lwarm_lima ) then
      where ( prrs(:, :, :, 2) < xrtmin_lima(2) / ptstep .or. prsvs(:, :, :, nsv_lima_nc) < xctmin_lima(2) / ptstep )
        prrs(:, :, :, 1) = prrs(:, :, :, 1) + prrs(:, :, :, 2)
        prths(:, :, :) = prths(:, :, :) - prrs(:, :, :, 2) * zlv(:, :, :) /  &
                 ( zcph(:, :, :) * zexn(:, :, :) )
        prrs(:, :, :, 2)  = 0.
        prsvs(:, :, :, nsv_lima_nc) = 0.
      end where
      where ( prrs(:, :, :, 1) < 0. .and. prrs(:, :, :, 2) > 0. )
        prrs(:, :, :, 1) = prrs(:, :, :, 1) + prrs(:, :, :, 2)
        prths(:, :, :) = prths(:, :, :) - prrs(:, :, :, 2) * zlv(:, :, :) /  &
           ( zcph(:, :, :) * zexn(:, :, :) )
        prrs(:, :, :, 2) = 0.
        prsvs(:, :, :, nsv_lima_nc) = 0.
      end where
    end if
! Correction where rr<0 or Nr<0
    if ( lwarm_lima .and. lrain_lima ) then
      where ( prrs(:, :, :, 3) < xrtmin_lima(3) / ptstep .or. prsvs(:, :, :, nsv_lima_nr) < xctmin_lima(3) / ptstep )
        prrs(:, :, :, 1) = prrs(:, :, :, 1) + prrs(:, :, :, 3)
        prths(:, :, :) = prths(:, :, :) - prrs(:, :, :, 3) * zlv(:, :, :) /  &
                 ( zcph(:, :, :) * zexn(:, :, :) )
        prrs(:, :, :, 3)  = 0.
        prsvs(:, :, :, nsv_lima_nr) = 0.
      end where
    end if
! Correction where ri<0 or Ni<0
    if ( lcold_lima ) then
      where ( prrs(:, :, :, 4) < xrtmin_lima(4) / ptstep .or. prsvs(:, :, :, nsv_lima_ni) < xctmin_lima(4) / ptstep )
        prrs(:, :, :, 1) = prrs(:, :, :, 1) + prrs(:, :, :, 4)
        prths(:, :, :) = prths(:, :, :) - prrs(:, :, :, 4) * zls(:, :, :) /  &
                 ( zcph(:, :, :) * zexn(:, :, :) )
        prrs(:, :, :, 4)  = 0.
        prsvs(:, :, :, nsv_lima_ni) = 0.
      end where
      if ( hbudname /= 'NETUR' ) then
        do jr = 5, Size( prrs, 4 )
          where ( prrs(:, :, :, jr) < 0. )
            prrs(:, :, :, 1) = prrs(:, :, :, 1) + prrs(:, :, :, jr)
            prths(:, :, :) = prths(:, :, :) - prrs(:, :, :, jr) * zls(:, :, :) /  &
                    ( zcph(:, :, :) * zexn(:, :, :) )
            prrs(:, :, :, jr) = 0.
          end where
        end do
      end if
      if(krr > 3) then
        allocate( zcor( Size( prths, 1 ), Size( prths, 2 ), Size( prths, 3 ) ) )
        where ( prrs(:, :, :, 1) < 0. .and. prrs(:, :, :, 4) > 0. )
          zcor(:, :, :) = Min( -prrs(:, :, :, 1), prrs(:, :, :, 4) )
          prrs(:, :, :, 1) = prrs(:, :, :, 1) + zcor(:, :, :)
          prths(:, :, :) = prths(:, :, :) - zcor(:, :, :) * zls(:, :, :) /  &
             ( zcph(:, :, :) * zexn(:, :, :) )
          prrs(:, :, :, 4) = prrs(:, :, :, 4) - zcor(:, :, :)
        end where
        deallocate( zcor )
      end if
    end if
    prrs(:,:,:,1) = max(0.0,prrs(:,:,:,1))
    prsvs(:, :, :, nsv_lima_beg : nsv_lima_end) = Max( 0.0, prsvs(:, :, :, nsv_lima_beg : nsv_lima_end) )

end select CLOUD

if ( hbudname /= 'NECON' .and. hbudname /= 'NEGA' ) then
  if ( hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' .or. &
       hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' ) then
    if ( lbudget_th ) call Budget( prths(:, :, :),    4, Trim( hbudname )//'_BU_RTH' )
    if ( lbudget_rv ) call Budget( prrs (:, :, :, 1), 6, Trim( hbudname )//'_BU_RRV' )
    if ( lbudget_rc ) call Budget( prrs (:, :, :, 2), 7, Trim( hbudname )//'_BU_RRC' )
    if ( lbudget_rr .and.                                                                                   &
       (   hbudname /= 'NETUR' .or.                                                                         &
         ( hbudname == 'NETUR' .and. ( hcloud == 'C2R2' .or. hcloud == 'KHKO' .or. hcloud == 'LIMA' ) ) ) ) &
      call Budget( prrs(:, :, :, 3), 8, Trim( hbudname )//'_BU_RRR' )
    IF (lbudget_ri .and.                                                                                    &
       (   hbudname /= 'NETUR' .or.                                                                         &
         ( hbudname == 'NETUR' .and. ( hcloud == 'ICE3' .or. hcloud == 'ICE4' .or. hcloud == 'LIMA' ) ) ) ) &
      call Budget( prrs(:, :, :, 4), 9, Trim( hbudname )//'_BU_RRI' )
    IF (lbudget_rs .and. hbudname /= 'NETUR' ) call Budget( prrs(:, :, :, 5), 10, Trim( hbudname )//'_BU_RRS' )
    IF (lbudget_rg .and. hbudname /= 'NETUR' ) call Budget( prrs(:, :, :, 6), 11, Trim( hbudname )//'_BU_RRG' )
    IF (lbudget_rh .and. hbudname /= 'NETUR' ) call Budget( prrs(:, :, :, 7), 12, Trim( hbudname )//'_BU_RRH' )
  END IF

  if ( lbudget_sv .and. (hcloud == 'C2R2' .or. hcloud == 'KHKO' ) ) then
    do ji = 1, 3
      call Budget( prsvs(:, :, :, nsv_c2r2beg - 1 + ji), 12 + nsv_c2r2beg - 1 + ji, Trim( hbudname )//'_BU_RSV' )
    end do
  end if
  if ( lbudget_sv .and. hcloud == 'LIMA' ) then
    do ji = nsv_lima_beg, nsv_lima_end
      call Budget( prsvs(:, :, :, ji), 12 + ji, Trim( hbudname )//'_BU_RSV' )
    end do
  end if
else !NECON + NEGA
  if ( .not. present( prhodj ) ) &
    call Print_msg( NVERB_FATAL, 'GEN', 'Sources_neg_correct', 'optional argument prhodj not present' )

  if ( hcloud == 'KESS' .or. hcloud == 'ICE3' .or. hcloud == 'ICE4' .or. &
       hcloud == 'KHKO' .or. hcloud == 'C2R2' .or. hcloud == 'LIMA' ) then
    if ( lbudget_th) call Budget( prths(:, :, :)    * prhodj(:, :, :),  4,  Trim( hbudname )//'_BU_RTH' )
    if ( lbudget_rv) call Budget( prrs (:, :, :, 1) * prhodj(:, :, :),  6,  Trim( hbudname )//'_BU_RRV' )
    if ( lbudget_rc) call Budget( prrs (:, :, :, 2) * prhodj(:, :, :),  7,  Trim( hbudname )//'_BU_RRC' )
    if ( lbudget_rr) call Budget( prrs (:, :, :, 3) * prhodj(:, :, :),  8,  Trim( hbudname )//'_BU_RRR' )
    if ( lbudget_ri) call Budget( prrs (:, :, :, 4) * prhodj(:, :, :),  9,  Trim( hbudname )//'_BU_RRI' )
    if ( lbudget_rs) call Budget( prrs (:, :, :, 5) * prhodj(:, :, :),  10, Trim( hbudname )//'_BU_RRS' )
    if ( lbudget_rg) call Budget( prrs (:, :, :, 6) * prhodj(:, :, :),  11, Trim( hbudname )//'_BU_RRG' )
    if ( lbudget_rh) call Budget( prrs (:, :, :, 7) * prhodj(:, :, :),  12, Trim( hbudname )//'_BU_RRH' )
  end if

  if ( lbudget_sv .and. (hcloud == 'C2R2' .or. hcloud == 'KHKO' )) then
    do ji = 1, 3
      call Budget( prsvs(:, :, :, nsv_c2r2beg - 1 + ji) * prhodj(:, :, :), 12 + nsv_c2r2beg - 1 + ji, Trim( hbudname )//'_BU_RSV' )
    end do
  end if
  if ( lbudget_sv .and. hcloud == 'LIMA' ) then
    do ji = nsv_lima_beg, nsv_lima_end
      call Budget( prsvs(:, :, :, ji) * prhodj(:, :, :), 12 + ji, Trim( hbudname )//'_BU_RSV' )
    end do
  end if
end if

end subroutine Sources_neg_correct

end module mode_sources_neg_correct
