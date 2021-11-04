!MNH_LIC Copyright 2000-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 12/10/2020: restructure Les_diachro_spec subroutine to use tfieldmetadata_base type
!-----------------------------------------------------------------
!     ######################
      MODULE MODE_LES_SPEC_n
!     ######################

use modd_field, only: tfieldmetadata_base

implicit none

private

public :: Les_spec_n


real, dimension(:,:,:,:), allocatable :: xspectrax ! spectra coeffcients for
real, dimension(:,:,:,:), allocatable :: xspectray ! x and y direction spectra

type(tfieldmetadata_base) :: tlesfieldx
type(tfieldmetadata_base) :: tlesfieldy


CONTAINS
!
!     ######################
      SUBROUTINE LES_SPEC_n(TPDIAFILE)
!     ######################
!
!
!!****  *LES_SPEC_n* computes and writes the LES spectra for model $n
!!
!!
!!    PURPOSE
!!    -------
!!
!!
!!    METHOD
!!    ------
!!
!!    Simple Fourier transforms are used, because there is no condition
!!    on the number of points in each direction (they can be different from
!!    factors of 2,3 and 5, because of possible subdomains).
!!
!!    However, the computations are performed only once, at the end of the run.
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      V. Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original         07/02/00
!!                       01/02/01 (D. Gazen) add module MODD_NSV for NSV variable
!!                       01/04/03 (V. Masson) bug in spectra normalization
! P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
use modd_conf_n, only: luserv, luserc, luseri
use modd_field,  only: NMNHDIM_COMPLEX, NMNHDIM_BUDGET_LES_TIME, NMNHDIM_SPECTRA_SPEC_NI, &
                       NMNHDIM_SPECTRA_SPEC_NJ, NMNHDIM_SPECTRA_LEVEL, NMNHDIM_UNUSED,    &
                       TYPEREAL
use modd_io,     only: tfiledata
use modd_lbc_n,  only: clbcx, clbcy
use modd_les,    only: nspectra_k
use modd_les_n,  only: nles_times, nspectra_ni, nspectra_nj, &
                       XCORRi_UU, XCORRi_VV, XCORRi_UV, XCORRi_WU, XCORRi_WV, XCORRi_WW,        &
                       XCORRi_WTh, XCORRi_WThl, XCORRi_WRv, XCORRi_WRc, XCORRi_WRi, XCORRi_WSv, &
                       XCORRi_ThTh, XCORRi_ThRv, XCORRi_ThRc, XCORRi_ThRi, XCORRi_ThlThl,       &
                       XCORRi_ThlRv, XCORRi_ThlRc, XCORRi_ThlRi,                                &
                       XCORRi_RvRv, XCORRi_RcRc, XCORRi_RiRi, XCORRi_SvSv,                      &
                       XCORRj_UU, XCORRj_VV, XCORRj_UV, XCORRj_WU, XCORRj_WV, XCORRj_WW,        &
                       XCORRj_WTh, XCORRj_WThl, XCORRj_WRv, XCORRj_WRc, XCORRj_WRi, XCORRj_WSv, &
                       XCORRj_ThTh, XCORRj_ThRv, XCORRj_ThRc, XCORRj_ThRi, XCORRj_ThlThl,       &
                       XCORRj_ThlRv, XCORRj_ThlRc, XCORRj_ThlRi,                                &
                       XCORRj_RvRv, XCORRj_RcRc, XCORRj_RiRi, XCORRj_SvSv

use modd_nsv,    only: nsv

IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
TYPE(TFILEDATA), INTENT(IN) :: TPDIAFILE ! file to write
!
!*      0.2  declaration of local variables
!
INTEGER          :: ISPECTRA_NI
INTEGER          :: ISPECTRA_NJ
INTEGER          :: JSV         ! scalar loop counter
CHARACTER(len=5) :: YGROUP
!-------------------------------------------------------------------------------
!
IF (CLBCX(1)=='CYCL') THEN
  ISPECTRA_NI = (NSPECTRA_NI+1)/2 - 1
ELSE
  ISPECTRA_NI =  NSPECTRA_NI - 1
END IF
!
IF (CLBCY(1)=='CYCL') THEN
  ISPECTRA_NJ = (NSPECTRA_NJ+1)/2 - 1
ELSE
  ISPECTRA_NJ =  NSPECTRA_NJ - 1
END IF
!
!*      1.   (ni,z,t) and (nj,z,t) spectra
!            -----------------------------
!
if ( nspectra_k > 0 ) then
  allocate( xspectrax(ispectra_ni, 2, nspectra_k, nles_times) )
  allocate( xspectray(ispectra_nj, 2, nspectra_k, nles_times) )

  tlesfieldx%clongname = ''
  tlesfieldx%ngrid     = 0
  tlesfieldx%ntype     = TYPEREAL
  tlesfieldx%ndims     = 4
  tlesfieldx%ndimlist(1)  = NMNHDIM_SPECTRA_SPEC_NI
  tlesfieldx%ndimlist(2)  = NMNHDIM_COMPLEX
  tlesfieldx%ndimlist(3)  = NMNHDIM_SPECTRA_LEVEL
  tlesfieldx%ndimlist(4)  = NMNHDIM_BUDGET_LES_TIME
  tlesfieldx%ndimlist(5:) = NMNHDIM_UNUSED

  tlesfieldy = tlesfieldx
  tlesfieldy%ndimlist(1)  = NMNHDIM_SPECTRA_SPEC_NJ

  call Les_specn_write_one_field( tpdiafile, XCORRi_UU, XCORRj_UU, 'UU', 'U*U     spectra', 'm3 s-2' )
  call Les_specn_write_one_field( tpdiafile, XCORRi_VV, XCORRj_VV, 'VV', 'V*V     spectra', 'm3 s-2' )
  call Les_specn_write_one_field( tpdiafile, XCORRi_WW, XCORRj_WW, 'WW', 'W*W     spectra', 'm3 s-2' )
  call Les_specn_write_one_field( tpdiafile, XCORRi_UV, XCORRj_UV, 'UV', 'U*V     spectra', 'm3 s-2' )
  call Les_specn_write_one_field( tpdiafile, XCORRi_WU, XCORRj_WU, 'WU', 'W*U     spectra', 'm3 s-2' )
  call Les_specn_write_one_field( tpdiafile, XCORRi_WV, XCORRj_WV, 'WV', 'W*V     spectra', 'm3 s-2' )

  call Les_specn_write_one_field( tpdiafile, XCORRi_ThTh, XCORRj_ThTh, 'THTH', 'Th*Th   spectra', 'm K2' )
  if ( luserc ) then
    call Les_specn_write_one_field( tpdiafile, XCORRi_ThlThl, XCORRj_ThlThl, 'TLTL', 'Thl*Thl spectra', 'm K2' )
  end if

  call Les_specn_write_one_field( tpdiafile, XCORRi_WTh, XCORRj_WTh, 'WTH',  'W*Th    spectra', 'm2 K s-1' )
  if ( luserc ) then
    call Les_specn_write_one_field( tpdiafile, XCORRi_WThl, XCORRj_WThl, 'WTHL', 'W*Thl   spectra', 'm2 K s-1' )
  end if

  if ( luserv ) then
    call Les_specn_write_one_field( tpdiafile, XCORRi_RvRv, XCORRj_RvRv, 'RVRV', 'rv*rv   spectra', 'm' )
    call Les_specn_write_one_field( tpdiafile, XCORRi_ThRv, XCORRj_ThRv, 'THRV', 'th*rv   spectra', 'K m' )
    if ( luserc ) then
      call Les_specn_write_one_field( tpdiafile, XCORRi_ThlRv, XCORRj_ThlRv, 'TLRV', 'thl*rv  spectra', 'K m' )
    end if
    call Les_specn_write_one_field( tpdiafile, XCORRi_WRv,  XCORRj_WRv,  'WRV',  'W*rv    spectra', 'm K s-1' )
  end if

  if ( luserc ) then
    call Les_specn_write_one_field( tpdiafile, XCORRi_RcRc,  XCORRj_RcRc,  'RCRC', 'rc*rc   spectra', 'm' )
    call Les_specn_write_one_field( tpdiafile, XCORRi_ThRc,  XCORRj_ThRc,  'THRC', 'th*rc   spectra', 'K m' )
    call Les_specn_write_one_field( tpdiafile, XCORRi_ThlRc, XCORRj_ThlRc, 'TLRC', 'thl*rc  spectra', 'K m' )
    call Les_specn_write_one_field( tpdiafile, XCORRi_WRc,   XCORRj_WRc,   'WRC',  'W*rc    spectra', 'm K s-1' )
  end if

  if ( luseri ) then
    call Les_specn_write_one_field( tpdiafile, XCORRi_RiRi,  XCORRj_RiRi,  'RIRI', 'ri*ri   spectra', 'm' )
    call Les_specn_write_one_field( tpdiafile, XCORRi_ThRi,  XCORRj_ThRi,  'THRI', 'th*ri   spectra', 'K m' )
    call Les_specn_write_one_field( tpdiafile, XCORRi_ThlRi, XCORRj_ThlRi, 'TLRI', 'thl*ri  spectra', 'K m' )
    call Les_specn_write_one_field( tpdiafile, XCORRi_WRi,   XCORRj_WRi,   'WRI',  'W*ri    spectra', 'm K s-1' )
  end if

  do jsv = 1, nsv
    Write( ygroup, fmt = "( a2, i3.3 )" ) "SS", jsv
    call Les_specn_write_one_field( tpdiafile, XCORRi_SvSv(:,:,:,JSV), XCORRj_SvSv(:,:,:,JSV), ygroup, 'Sv*Sv   spectra','m' )
  end do

  do jsv = 1, nsv
    Write( ygroup, fmt = "( a2, i3.3 )" ) "WS", jsv
    call Les_specn_write_one_field( tpdiafile, XCORRi_WSv(:,:,:,JSV), XCORRj_WSv(:,:,:,JSV), ygroup, 'W*Sv    spectra','m2 s-1' )
  end do

  deallocate( xspectrax )
  deallocate( xspectray )
end if

END SUBROUTINE LES_SPEC_n
!
!------------------------------------------------------------------------------

subroutine  Les_specn_write_one_field( tpdiafile, zcorri, zcorrj, ymnhname, ycomment, yunits )

use modd_io,          only: tfiledata

use mode_les_diachro, only: Les_diachro_spec

type(tfiledata),          intent(in) :: tpdiafile ! file to write
real, dimension(:,:,:),   intent(in) :: zcorri    ! 2 pts correlation data
real, dimension(:,:,:),   intent(in) :: zcorrj    ! 2 pts correlation data
character(len=*),         intent(in) :: ymnhname
character(len=*),         intent(in) :: ycomment
character(len=*),         intent(in) :: yunits

call Les_spec('X', zcorri, xspectrax)
call Les_spec('Y', zcorrj, xspectray)

tlesfieldx%cmnhname = ymnhname
tlesfieldx%ccomment = ycomment
tlesfieldx%cunits   = yunits

tlesfieldy%cmnhname = tlesfieldx%cmnhname
tlesfieldy%ccomment = tlesfieldx%ccomment
tlesfieldy%cunits   = tlesfieldx%cunits

call Les_diachro_spec( tpdiafile, tlesfieldx, tlesfieldy, xspectrax, xspectray )

end subroutine Les_specn_write_one_field

!------------------------------------------------------------------------------
!
SUBROUTINE LES_SPEC(HDIR,ZCORR,ZSPECTRA)
!
use modd_conf,   only: l2d
use modd_cst,    only: xpi
use modd_grid_n, only: xxhat, xyhat
!
CHARACTER(len=1),         INTENT(IN)  :: HDIR        ! direction of spectra
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: ZCORR       ! 2 pts correlation data
REAL, DIMENSION(:,:,:,:), INTENT(OUT) :: ZSPECTRA    ! spectra
!
INTEGER                               :: INK      ! number of wavelength
INTEGER                               :: JK       ! loop counter
REAL                                  :: ZK       ! wavelength
INTEGER                               :: JR       ! loop counter
INTEGER                               :: ITIME    ! number of averaging points
!
REAL                                  :: ZDX      ! grid mesh
!------------------------------------------------------------------------------
!
!
IF ((L2D) .AND. HDIR=='Y') RETURN
!
INK = SIZE(ZCORR,1)
!
ZSPECTRA(:,:,:,:)=0.
!
!
!* loop on wavelengths
!
DO JK=1,SIZE(ZSPECTRA,1)
!
!* actual wavelength
!
  ZK = (2.*XPI) / JK
!
!* Fourier summation
!
! warning : index JR=1 corresponds to autocorrelations.
! warning : integration on wavelengths is done only for the half of the
!           domain, in order (i) to avoid extra counting of the spectra
!           in CYCL case, or (ii) to take into account only wavelentgh
!           with a significant number of occurences in other boundary
!           conditions.
!
  DO JR=1,INK/2
    ZSPECTRA(JK,1,:,:) = ZSPECTRA(JK,1,:,:) + ZCORR(JR,:,:)*COS(-ZK*(JR-1))
    ZSPECTRA(JK,2,:,:) = ZSPECTRA(JK,2,:,:) + ZCORR(JR,:,:)*SIN(-ZK*(JR-1))
  END DO
END DO
!
IF (HDIR=='X') THEN
  ZDX=(XXHAT(2)-XXHAT(1))
ELSE IF (HDIR=='Y') THEN
  ZDX=(XYHAT(2)-XYHAT(1))
END IF
!
ZSPECTRA(:,:,:,:) = ZSPECTRA(:,:,:,:) / (2.*XPI*ZDX*(INK/2))
!
!------------------------------------------------------------------------------
!
END SUBROUTINE LES_SPEC
!
!------------------------------------------------------------------------------

END MODULE MODE_LES_SPEC_n
