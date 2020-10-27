!MNH_LIC Copyright 1994-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications
!  G. Tanguy   19/05/2014: correct DATIME in case of time average
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet 20/09/2019: rewrite normalization of LES budgets
!  P. Wautelet 14/08/2020: deduplicate LES_DIACHRO* subroutines
!  P. Wautelet    10/2020: restructure subroutines to use tfield_metadata_base type
!-----------------------------------------------------------------
!#######################
MODULE MODE_LES_DIACHRO
!#######################

USE MODD_LUNIT
use modd_les_n, only: xles_dates, xles_times

use mode_msg

implicit none

private

public :: Les_diachro, Les_diachro_2pt, Les_diachro_spec

interface Les_diachro
  module procedure Les_diachro_1D, Les_diachro_2D, Les_diachro_3D, Les_diachro_4D
end interface


CONTAINS
!
!---------------------------------------------------------------------
!
!########################################################
subroutine Make_norm( pa_norm, ples_norm, kpower )
!########################################################

use modd_les,        only: nles_current_times, nles_k
use modd_parameters, only: XUNDEF

real, dimension(:,:,:,:), intent(inout) :: pa_norm   ! normalized field
real, dimension(:),       intent(in)    :: ples_norm ! normalization coefficient
integer,                  intent(in)    :: kpower    ! normalization power

integer :: jk ! z counter
integer :: jt ! time counter
integer :: jp ! process counter
integer :: jn ! variable number counter (larger than 1 only for scalar var.)

if ( kpower == 0 ) return

!normalization is not possible if some values are zero
if ( any( ples_norm(: ) == 0. ) ) then
  pa_norm(:, :, :, : ) = XUNDEF
  return
end if

do jn = 1, size( pa_norm, 4 )
  do jp = 1, size( pa_norm, 3 )
    do jt = 1, nles_current_times
      do jk = 1, nles_k
        if ( pa_norm(jk, jt, jp, jn ) /= XUNDEF ) &
          pa_norm(jk, jt, jp, jn ) = pa_norm(jk, jt, jp, jn ) * ples_norm(jt )**( -kpower )
      end do
    end do
  end do
end do

end subroutine Make_norm

!########################################################
subroutine Make_norm_sv( pa_norm, ples_norm, kpower )
!########################################################

use modd_les,        only: nles_current_times, nles_k
use modd_parameters, only: XUNDEF

real, dimension(:,:,:,:), intent(inout) :: pa_norm   ! normalized field
real, dimension(:,:),     intent(in)    :: ples_norm ! normalization coefficient
integer,                  intent(in)    :: kpower    ! normalization power

integer :: jk ! z counter
integer :: jt ! time counter
integer :: jp ! process counter
integer :: jsv! scalar variables counter

if ( kpower == 0 ) return

!normalization is not possible if some values are zero
do jsv = 1, size( ples_norm, 2 )
  if ( any( ples_norm(:, jsv ) == 0. ) ) then
    pa_norm(:, :, :, jsv) = XUNDEF
    cycle
  end if

  do jp = 1, size( pa_norm, 3 )
    do jt = 1, nles_current_times
      do jk = 1, nles_k
        if ( pa_norm(jk, jt, jp, jsv) /= XUNDEF ) &
            pa_norm(jk, jt,jp, jsv ) = pa_norm(jk, jt, jp, jsv ) * ples_norm(jt, jsv )**( -kpower )
      end do
    end do
  end do
end do

end subroutine Make_norm_sv
!
!     ###################################################
      SUBROUTINE LES_NORM_4D(HUNIT, PA_LES, PA_NORM, OSV)
!     ###################################################
!
!
!!****  *LES_NORM* normalizes a field according to the chosen normalization
!!
!!    PURPOSE
!!    -------
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
!!      Original         30/10/00
!!
!! --------------------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
USE MODD_LES
!
!
IMPLICIT NONE
!
!*      0.1  declarations of arguments
!
CHARACTER(LEN=*),            INTENT(IN)  :: HUNIT   ! physical unit of field
REAL,    DIMENSION(:,:,:,:), INTENT(IN)  :: PA_LES  ! field
!
REAL,    DIMENSION(:,:,:,:), INTENT(OUT) :: PA_NORM ! normalized field
LOGICAL, OPTIONAL,           INTENT(IN)  :: OSV     ! flag for scalar variables
!
!       0.2  declaration of local variables
!
integer, parameter :: NMAXUNITS = 10

integer, parameter :: NNORM_K  = 1
integer, parameter :: NNORM_KG = 2
integer, parameter :: NNORM_M  = 3
integer, parameter :: NNORM_PA = 4
integer, parameter :: NNORM_S  = 5
integer, parameter :: NNORM_RV = 6
integer, parameter :: NNORM_SV = 7
integer, parameter :: NNORMS   = 7

integer :: idx, ispace
integer :: ikg   ! number of 'kg' in the field unit
integer :: inunits
integer :: ipower
integer :: ipower_kg_1st
integer :: ji
integer, dimension ( NNORMS ) :: ipowers
character( len = 8 ) :: yun, yname, ypower
character( len = : ), allocatable :: yunit
character( len = 8 ), dimension( NMAXUNITS ) :: yunits
logical :: gsv
!------------------------------------------------------------------------------

gsv = .false.
if ( present( osv ) ) gsv = osv

pa_norm(:, :, :, : ) = pa_les(:, :, :, : )

!Parse units
!Each unit is separated by blanks
!First part: unit, second part: power (and sign of it)
ipowers(: ) = 0
inunits = 0
ikg = 0
idx = 1

yunit = trim( hunit )

!Separate units
do
  ispace = scan( yunit(idx: ), ' ' )
  if ( ispace == 0 ) then
    inunits = inunits + 1
    if (inunits > NMAXUNITS ) call Print_msg( NVERB_FATAL, 'GEN', 'LES_NORM_4D', 'inunits > NMAXUNITS' )
    yunits(inunits ) = yunit(idx:)
    exit
  else if ( ispace == len(yunit(idx: )) ) then
    exit
  else
    inunits = inunits + 1
    if (inunits > NMAXUNITS ) call Print_msg( NVERB_FATAL, 'GEN', 'LES_NORM_4D', 'inunits > NMAXUNITS' )
    yunits(inunits ) =  yunit( idx : idx+ispace-1 )
    idx = idx + ispace
  end if
end do

!Treat units and their power
!kg are special: they can appear twice with opposite power signs (kg kg-1)
!In that case, they are normalized with xles_norm_rv or xles_norm_sv
do ji = 1, inunits
  yun = yunits(ji )

  !Non dimensional unit
  if ( trim( yun ) == '-' .or. trim( yun ) == '1' .or. trim( yun ) == 'percent') then
    cycle
  end if

  !Separate unit and its power
  idx = scan( yun, '-1234567890' )
  if ( idx == 0 ) then
    yname = trim( yun )
    ypower = ''
    ipower = 1
  else
    yname  = yun( 1 : idx - 1 )
    ypower = yun( idx : )
    read (ypower,'(I8)') ipower
  end if

  select case( trim( yname ) )
    case ( 'K' )
      ipowers(NNORM_K  ) = ipowers(NNORM_K  ) + ipower
    case ( 'kg' )
      ikg = ikg + 1
      if ( ikg == 1 ) ipower_kg_1st = ipower
      ipowers(NNORM_KG ) = ipowers(NNORM_KG ) + ipower
    case ( 'm' )
      ipowers(NNORM_M  ) = ipowers(NNORM_M  ) + ipower
    case ( 'Pa' )
      ipowers(NNORM_PA ) = ipowers(NNORM_PA ) + ipower
    case ( 's' )
      ipowers(NNORM_S  ) = ipowers(NNORM_S  ) + ipower
    case default
      call Print_msg( NVERB_WARNING, 'IO', 'LES_NORM_4D', 'unknown unit: '//trim(yname)//'. Conversion could be wrong.' )
  end select
end do

if ( ikg > 1 .and. ipowers(NNORM_KG ) /= 0 ) &
  call Print_msg( NVERB_ERROR, 'IO', 'LES_NORM_4D', 'if kg appears more than one time, it should be adimensional' )

if ( ikg > 2 ) &
  call Print_msg( NVERB_ERROR, 'IO', 'LES_NORM_4D', 'kg should not appear more than 2 times' )

if ( ikg == 2 ) then
  if ( gsv ) then
    ipowers(NNORM_SV ) = ipower_kg_1st
  else
    ipowers(NNORM_RV ) = ipower_kg_1st
  end if
end if

if (ipowers(NNORM_K  ) /= 0 ) call Make_norm   ( pa_norm, xles_norm_k,   ipowers(NNORM_K  ) )
if (ipowers(NNORM_KG ) /= 0 ) call Make_norm   ( pa_norm, xles_norm_rho, ipowers(NNORM_KG ) )
if (ipowers(NNORM_M  ) /= 0 ) call Make_norm   ( pa_norm, xles_norm_m,   ipowers(NNORM_M  ) )
if (ipowers(NNORM_PA ) /= 0 ) call Make_norm   ( pa_norm, xles_norm_p,   ipowers(NNORM_PA ) )
if (ipowers(NNORM_S  ) /= 0 ) call Make_norm   ( pa_norm, xles_norm_s,   ipowers(NNORM_S  ) )
if (ipowers(NNORM_RV ) /= 0 ) call Make_norm   ( pa_norm, xles_norm_rv,  ipowers(NNORM_RV ) )
if (ipowers(NNORM_SV ) /= 0 ) call Make_norm_sv( pa_norm, xles_norm_sv,  ipowers(NNORM_SV ) )

where( pa_norm == XUNDEF ) pa_norm = pa_les

END SUBROUTINE LES_NORM_4D
!
!------------------------------------------------------------------------------
!
!###################################
SUBROUTINE LES_Z_NORM(OAVG,PTRAJZ,PWORK4)
!###################################
!
!* this subroutine interpolates the normalized field PWORK4 to the
!  vertical normalized coordinate.
!
USE MODD_LES
USE MODD_PARAMETERS, ONLY: XUNDEF, JPVEXT
use modd_time,       only: tdtseg
!
USE MODI_COEF_VER_INTERP_LIN
USE MODI_VER_INTERP_LIN
!
IMPLICIT NONE
!
LOGICAL,                      INTENT(IN)    :: OAVG   ! time average ?
REAL, DIMENSION(:,:,:),       INTENT(INOUT) :: PTRAJZ ! vertical grid
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PWORK4 ! field data array
!
! initial grid
REAL,    DIMENSION(SIZE(PWORK4,2),SIZE(PWORK4,4),SIZE(PWORK4,1),SIZE(PWORK4,3) ) :: ZWORK4
! initial data
REAL,    DIMENSION(SIZE(PWORK4,2),SIZE(PWORK4,4),SIZE(PWORK4,1))  :: ZNORMZ
! grid in normalized coordinates
!
REAL,    DIMENSION(SIZE(PWORK4,2),SIZE(PWORK4,4),SIZE(PWORK4,1),SIZE(PWORK4,3) ) :: ZNORM4
! data interpolated on normalized vertical grid
!
REAL,    DIMENSION(SIZE(PWORK4,2),SIZE(PWORK4,4),SIZE(PWORK4,1)+2*JPVEXT)  :: ZZ
REAL,    DIMENSION(SIZE(PWORK4,2),SIZE(PWORK4,4),SIZE(PWORK4,1)+2*JPVEXT)  :: ZW
INTEGER, DIMENSION(SIZE(PWORK4,2),SIZE(PWORK4,4),SIZE(PWORK4,1))           :: IKLIN
REAL,    DIMENSION(SIZE(PWORK4,2),SIZE(PWORK4,4),SIZE(PWORK4,1))           :: ZCOEFLIN
!
INTEGER :: JK, JT, JN, JP
INTEGER :: ITEMP_MEAN_START, ITEMP_MEAN_END
!
REAL    :: ZMAX_NORM_M
!------------------------------------------------------------------------------
!
!* normalization height (usually maximum BL height)
!
IF (OAVG) THEN

  ITEMP_MEAN_START = COUNT( xles_times(:)<=XLES_TEMP_MEAN_START ) + 1
  ITEMP_MEAN_END   = COUNT( xles_times(:)<=XLES_TEMP_MEAN_END   )

  IF (ITEMP_MEAN_START > ITEMP_MEAN_END) THEN
    ITEMP_MEAN_START = 1
    ITEMP_MEAN_END   = NLES_CURRENT_TIMES
  END IF
ELSE
  ITEMP_MEAN_START = 1
  ITEMP_MEAN_END   = NLES_CURRENT_TIMES
END IF
!
ZMAX_NORM_M = MAXVAL(ABS(XLES_NORM_M(ITEMP_MEAN_START:ITEMP_MEAN_END)))
!
IF (ZMAX_NORM_M<=0.) THEN
  PWORK4(:,:,:,:) = XUNDEF
  RETURN
END IF
!
!* moves K index in third position
!
DO JK=1,NLES_K
  DO JN=1,SIZE(PWORK4,4)
    DO JT=1,SIZE(PWORK4,2)
      ZWORK4(JT,JN,JK,:) = PWORK4(JK,JT,:,JN)
    END DO
  END DO
END DO
!
!* computes the stretching due to the use of a normalized grid
!
DO JK=1,NLES_K
  DO JN=1,SIZE(PWORK4,4)
    DO JT=1,SIZE(PWORK4,2)
      ZNORMZ(JT,JN,JK) = (xles_current_z(JK)-XLES_CURRENT_ZS)/ZMAX_NORM_M * XLES_NORM_M(JT)  &
                         + XLES_CURRENT_ZS
    END DO
  END DO
END DO
!
!
IF (NLES_K>1) THEN
!
!* computes the interpolation coefficients
!
  DO JN=1,SIZE(PWORK4,4)
    DO JT=1,SIZE(PWORK4,2)
      ZZ(JT,JN,JPVEXT+1:JPVEXT+NLES_K) = xles_current_z(:)
    END DO
  END DO
  DO JK=1,JPVEXT
      ZZ(:,:,              JK) = xles_current_z(1)      - (xles_current_z(2)     -xles_current_z(1)       )*(JPVEXT+1-JK)
      ZZ(:,:,NLES_K+JPVEXT+JK) = xles_current_z(NLES_K) + (xles_current_z(NLES_K)-xles_current_z(NLES_K-1))*          JK
  END DO

  CALL COEF_VER_INTERP_LIN(ZZ,ZNORMZ,IKLIN,ZCOEFLIN)
!
!* performs the interpolation
!
  DO JP=1,SIZE(PWORK4,3)
    ZW = XUNDEF
    ZW(:,:,JPVEXT+1:JPVEXT+NLES_K) = ZWORK4(:,:,:,JP)
    ZNORM4(:,:,:,JP) = VER_INTERP_LIN(ZW,IKLIN,ZCOEFLIN)
  END DO
!
ELSE
  ZNORM4 = ZWORK4
END IF
!
!* puts the normalized grid and data in diachro arrays
!
PTRAJZ(:,:,:) = (PTRAJZ(:,:,:)-XLES_CURRENT_ZS)/ZMAX_NORM_M
!
DO JN=1,SIZE(PWORK4,4)
  DO JT=1,SIZE(PWORK4,2)
    DO JK=1,NLES_K
      PWORK4(JK,JT,:,JN) = ZNORM4(JT,JN,JK,:)
    END DO
  END DO
END DO
!
END SUBROUTINE LES_Z_NORM
!------------------------------------------------------------------------------
!
!########################################################
SUBROUTINE LES_TIME_AVG(PWORK6,tpdates,KRESP)
!########################################################
!
! this routine computes time averaging
!
! Modifications:
!  P. Wautelet    03/2018: replace ADD_FORECAST_TO_DATE by DATETIME_CORRECTDATE
!  P. Wautelet 28/08/2020: bugfix: allocate ZWORK6 at the correct dimension (3rd one)
!
use modd_les,        only: nles_current_times, xles_temp_mean_start, xles_temp_mean_end, xles_temp_mean_step
use modd_parameters, only: XUNDEF
use modd_time,       only: tdtseg
use modd_type_date,  only: date_time
!
use mode_datetime,  only: Datetime_correctdate
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:,:,:,:,:), allocatable     :: PWORK6 ! contains physical field
type(date_time), dimension(:), allocatable, intent(inout) :: tpdates
INTEGER,                      INTENT(OUT) :: KRESP  ! return code (0 is OK)
!------------------------------------------------------------------------------
INTEGER                                :: JT       ! time counter
INTEGER                                :: ITIME    ! nb of avg. points
INTEGER                                :: IAVG     ! nb of avg. periods
INTEGER                                :: JAVG     ! loop counter on avg. periods
REAL                                   :: ZLES_TEMP_MEAN_START ! initial and end times
REAL                                   :: ZLES_TEMP_MEAN_END   ! of one avergaing preiod
REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: ZWORK6     ! contains averaged physical field
INTEGER :: JK                            ! vertical loop counter
INTEGER :: JP                            ! process loop counter
INTEGER :: JSV                           ! scalar loop counter
INTEGER :: JX                            ! first  spatial or spectral coordinate loop counter
INTEGER :: JY                            ! second spatial or spectral coordinate loop counter
integer :: jtb, jte
!------------------------------------------------------------------------------
!
IF (     XLES_TEMP_MEAN_END==XUNDEF   &
    .OR. XLES_TEMP_MEAN_START==XUNDEF &
    .OR. XLES_TEMP_MEAN_STEP==XUNDEF  ) THEN
  KRESP=-1
  RETURN
END IF
!
IAVG=INT(XLES_TEMP_MEAN_END-1.E-10-XLES_TEMP_MEAN_START)/XLES_TEMP_MEAN_STEP + 1
IF (IAVG<=0) THEN
  KRESP=-1
  RETURN
END IF
!
deallocate( tpdates )
!
allocate( tpdates( iavg ) )
ALLOCATE (ZWORK6(SIZE(PWORK6,1),SIZE(PWORK6,2),SIZE(PWORK6,3),IAVG,SIZE(PWORK6,5),SIZE(PWORK6,6)))
!
ZWORK6(:,:,:,:,:,:) = 0.
!
DO JAVG=1,IAVG
  ZLES_TEMP_MEAN_START=XLES_TEMP_MEAN_START + (JAVG-1) * XLES_TEMP_MEAN_STEP
  ZLES_TEMP_MEAN_END  =MIN(XLES_TEMP_MEAN_END, ZLES_TEMP_MEAN_START + XLES_TEMP_MEAN_STEP)

  jtb = -1
  jte = -2
  do jt = 1, nles_current_times
    if ( xles_times(jt) >= zles_temp_mean_start ) then
      jtb = jt
      exit
    end if
  end do
  do jt = jtb, nles_current_times
    if ( xles_times(jt) <= zles_temp_mean_end ) then
      jte = jt
    else
      exit
    end if
  end do

  DO JP=1,SIZE(PWORK6,6)
    DO JSV=1,SIZE(PWORK6,5)
      DO JK=1,SIZE(PWORK6,3)
        DO JY=1,SIZE(PWORK6,2)
          DO JX=1,SIZE(PWORK6,1)
            zwork6(jx, jy, jk, javg, jsv, jp) = Les_time_avg_1pt( pwork6(jx, jy, jk, :, jsv, jp), jtb, jte )
          END DO
        END DO
      END DO
    END DO
  END DO

  tpdates(javg )%tdate%year  = tdtseg%tdate%year
  tpdates(javg )%tdate%month = tdtseg%tdate%month
  tpdates(javg )%tdate%day   = tdtseg%tdate%day
  tpdates(javg )%time        = tdtseg%time + ( zles_temp_mean_start + zles_temp_mean_end ) / 2.
  call Datetime_correctdate( tpdates(javg ) )
END DO
!
DEALLOCATE(PWORK6)
ALLOCATE(PWORK6(SIZE(ZWORK6,1),SIZE(ZWORK6,2),SIZE(ZWORK6,3),IAVG,SIZE(ZWORK6,5),SIZE(ZWORK6,6)))
PWORK6 = ZWORK6
DEALLOCATE(ZWORK6)

KRESP = 0

END SUBROUTINE LES_TIME_AVG
!------------------------------------------------------------------------------

!########################################################
subroutine Les_time_avg_4D( pwork4, tpdates, kresp )
!########################################################

use modd_les,        only: nles_current_times, xles_temp_mean_start, xles_temp_mean_end, xles_temp_mean_step
use modd_parameters, only: XUNDEF
use modd_time,       only: tdtseg
use modd_type_date,  only: date_time

use mode_datetime,   only: Datetime_correctdate

implicit none

real,            dimension(:,:,:,:), allocatable, intent(inout) :: pwork4 ! contains physical field
type(date_time), dimension(:),       allocatable, intent(inout) :: tpdates
integer,                                          intent(out)   :: kresp  ! return code (0 is ok)
!------------------------------------------------------------------------------
integer                               :: jt                   ! time counter
integer                               :: itime                ! nb of avg. points
integer                               :: iavg                 ! nb of avg. periods
integer                               :: javg                 ! loop counter on avg. periods
integer                               :: jk                   ! vertical loop counter
integer                               :: jp                   ! process loop counter
integer                               :: jsv                  ! scalar loop counter
integer                               :: jx                   ! first  spatial or spectral coordinate loop counter
integer                               :: jy                   ! second spatial or spectral coordinate loop counter
integer                               :: jtb, jte
real                                  :: zles_temp_mean_start ! initial and end times
real                                  :: zles_temp_mean_end   ! of one avergaing preiod
real, dimension(:,:,:,:), allocatable :: zwork4               ! contains averaged physical field
!------------------------------------------------------------------------------

if (     xles_temp_mean_end   == XUNDEF &
    .or. xles_temp_mean_start == XUNDEF &
    .or. xles_temp_mean_step  == XUNDEF ) then
  kresp = -1
  return
end if

iavg = Int( xles_temp_mean_end - 1.e-10 - xles_temp_mean_start ) / xles_temp_mean_step + 1
if ( iavg <= 0 ) then
  kresp = -1
  return
end if

Deallocate( tpdates )

Allocate( tpdates(iavg) )
Allocate( zwork4(Size( pwork4, 1 ), iavg, Size( pwork4, 3 ), Size( pwork4, 4 )) )

zwork4(:, :, :, :) = 0.

do javg = 1, iavg
  zles_temp_mean_start = xles_temp_mean_start + (javg - 1) * xles_temp_mean_step
  zles_temp_mean_end   = Min( xles_temp_mean_end, zles_temp_mean_start + xles_temp_mean_step )

  jtb = -1
  jte = -2
  do jt = 1, nles_current_times
    if ( xles_times(jt) >= zles_temp_mean_start ) then
      jtb = jt
      exit
    end if
  end do
  do jt = jtb, nles_current_times
    if ( xles_times(jt) <= zles_temp_mean_end ) then
      jte = jt
    else
      exit
    end if
  end do

  do jp = 1, Size( pwork4, 4 )
    do jsv = 1, Size( pwork4, 3 )
      do jk = 1, Size( pwork4, 1 )
        zwork4(jk, javg, jsv, jp) = Les_time_avg_1pt( pwork4(jk, :, jsv, jp), jtb, jte )
      end do
    end do
  end do

  tpdates(javg)%tdate%year  = tdtseg%tdate%year
  tpdates(javg)%tdate%month = tdtseg%tdate%month
  tpdates(javg)%tdate%day   = tdtseg%tdate%day
  tpdates(javg)%time        = tdtseg%time + ( zles_temp_mean_start + zles_temp_mean_end ) / 2.
  call Datetime_correctdate( tpdates(javg) )
end do

Deallocate( pwork4 )
Allocate( pwork4(Size( zwork4, 1 ), iavg, Size( zwork4, 3 ), Size( zwork4, 4 )) )

pwork4 = zwork4

Deallocate( zwork4 )

kresp = 0

end subroutine Les_time_avg_4D
!------------------------------------------------------------------------------

!#######################################################################
pure function Les_time_avg_1pt( pfieldin, ktb, kte ) result( pfieldout )
!#######################################################################
! This routine computes time averaging for 1 point
use modd_parameters, only: XUNDEF

implicit none

real,    dimension(:), intent(in) :: pfieldin
integer,               intent(in) :: ktb
integer,               intent(in) :: kte

real :: pfieldout

integer :: itime, jt

itime = 0

pfieldout = 0.

do jt = ktb, kte
  if ( pfieldin(jt) /= XUNDEF ) then
    pfieldout =  pfieldout + pfieldin(jt)
    itime = itime + 1
  end if
end do

if (itime == 0) then
  pfieldout = XUNDEF
else
  pfieldout = pfieldout / itime
end if

end function Les_time_avg_1pt

!#######################################################################
subroutine Les_diachro_1D( tpdiafile, tpfield, odoavg, odonorm, pfield )
!#######################################################################

use modd_field, only: NMNHDIM_BUDGET_LES_TIME, NMNHDIM_UNUSED, tfield_metadata_base
use modd_io,    only: tfiledata

type(tfiledata),                       intent(in) :: tpdiafile  ! File to write
type(tfield_metadata_base),            intent(in) :: tpfield    ! Metadata of field
logical,                               intent(in) :: odoavg     ! Compute and store time average
logical,                               intent(in) :: odonorm    ! Compute and store normalized field
real,                    dimension(:), intent(in) :: pfield     ! Data array

type(tfield_metadata_base) :: tzfield

tzfield = tpfield

if ( tzfield%ndims /= 1 ) then
  call Print_msg( NVERB_ERROR, 'IO', 'Les_diachro_1D', 'ndims /= 1 for ' // Trim( tzfield%cmnhname ) )
  tzfield%ndims = 1
end if

if ( Any( tzfield%ndimlist(2:) /= NMNHDIM_UNUSED ) ) then
  call Print_msg( NVERB_ERROR, 'IO', 'Les_diachro_1D', 'unexpected type for some dimensions of ' &
                  // Trim( tzfield%cmnhname ) )
  tzfield%ndimlist(2:) = NMNHDIM_UNUSED
end if

if ( tzfield%ndimlist(1) == NMNHDIM_BUDGET_LES_TIME ) then
  tzfield%ndimlist(2) = tzfield%ndimlist(1)
  tzfield%ndimlist(1) = NMNHDIM_UNUSED
  tzfield%ndimlist(3) = NMNHDIM_UNUSED
  tzfield%ndimlist(4) = NMNHDIM_UNUSED
  call Les_diachro_common( tpdiafile, tzfield, reshape( pfield, [ 1, size( pfield, 1 ), 1, 1 ] ), &
                           odoavg, odonorm )
else
  call Print_msg( NVERB_ERROR, 'IO', 'Les_diachro_1D', &
                  'ndimlist configuration not yet implemented for ' // Trim( tzfield%cmnhname ) )
end if

end subroutine Les_diachro_1D

!#######################################################################
subroutine Les_diachro_2D( tpdiafile, tpfield, odoavg, odonorm, pfield )
!#######################################################################

use modd_field, only: NMNHDIM_BUDGET_LES_LEVEL, NMNHDIM_BUDGET_LES_SV, NMNHDIM_BUDGET_LES_TIME, NMNHDIM_UNUSED, &
                      tfield_metadata_base
use modd_io,    only: tfiledata

type(tfiledata),                         intent(in) :: tpdiafile  ! File to write
type(tfield_metadata_base),              intent(in) :: tpfield    ! Metadata of field
logical,                                 intent(in) :: odoavg     ! Compute and store time average
logical,                                 intent(in) :: odonorm    ! Compute and store normalized field
real,                    dimension(:,:), intent(in) :: pfield     ! Data array

type(tfield_metadata_base) :: tzfield

tzfield = tpfield

if ( tzfield%ndims /= 2 ) then
  call Print_msg( NVERB_ERROR, 'IO', 'Les_diachro_2D', 'ndims /= 2 for ' // Trim( tzfield%cmnhname ) )
  tzfield%ndims = 2
end if

if ( Any( tzfield%ndimlist(3:) /= NMNHDIM_UNUSED ) ) then
  call Print_msg( NVERB_ERROR, 'IO', 'Les_diachro_2D', 'unexpected type for some dimensions of ' &
                  // Trim( tzfield%cmnhname ) )
  tzfield%ndimlist(3:) = NMNHDIM_UNUSED
end if

if (       tzfield%ndimlist(1) == NMNHDIM_BUDGET_LES_LEVEL &
     .and. tzfield%ndimlist(2) == NMNHDIM_BUDGET_LES_TIME  ) then
  tzfield%ndimlist(3) = NMNHDIM_UNUSED
  tzfield%ndimlist(4) = NMNHDIM_UNUSED
  call Les_diachro_common( tpdiafile, tzfield,                                                &
                           reshape( pfield, [ size( pfield, 1 ), size( pfield, 2 ), 1, 1 ] ), &
                           odoavg, odonorm )
else if (       tzfield%ndimlist(1) == NMNHDIM_BUDGET_LES_TIME &
          .and. tzfield%ndimlist(2) == NMNHDIM_BUDGET_LES_SV   ) then
  tzfield%ndimlist(4) = tzfield%ndimlist(2)
  tzfield%ndimlist(2) = tzfield%ndimlist(1)
  tzfield%ndimlist(1) = NMNHDIM_UNUSED
  tzfield%ndimlist(3) = NMNHDIM_UNUSED
  call Les_diachro_common( tpdiafile, tzfield,                                                &
                           reshape( pfield, [ 1, size( pfield, 1 ), 1, size( pfield, 2 ) ] ), &
                           odoavg, odonorm )
else
  call Print_msg( NVERB_ERROR, 'IO', 'Les_diachro_2D', &
                  'ndimlist configuration not yet implemented for ' // Trim( tzfield%cmnhname ) )

end if

end subroutine Les_diachro_2D

!##################################################################################
subroutine Les_diachro_3D( tpdiafile, tpfield, odoavg, odonorm, pfield, hsuffixes )
!##################################################################################

use modd_field, only: NMNHDIM_BUDGET_LES_LEVEL, NMNHDIM_BUDGET_LES_MASK, NMNHDIM_BUDGET_LES_SV, &
                      NMNHDIM_BUDGET_LES_TIME,  NMNHDIM_BUDGET_TERM,     NMNHDIM_UNUSED,        &
                      tfield_metadata_base
use modd_io,    only: tfiledata

type(tfiledata),                           intent(in) :: tpdiafile  ! File to write
type(tfield_metadata_base),                intent(in) :: tpfield    ! Metadata of field
logical,                                   intent(in) :: odoavg     ! Compute and store time average
logical,                                   intent(in) :: odonorm    ! Compute and store normalized field
real,                    dimension(:,:,:), intent(in) :: pfield     ! Data array
character(len=*),        dimension(:),     optional, intent(in) :: hsuffixes

type(tfield_metadata_base) :: tzfield

tzfield = tpfield

if ( tzfield%ndims /= 3 ) then
  call Print_msg( NVERB_ERROR, 'IO', 'Les_diachro_3D', 'ndims /= 3 for ' // Trim( tzfield%cmnhname ) )
  tzfield%ndims = 3
end if

if ( Any( tzfield%ndimlist(4:) /= NMNHDIM_UNUSED ) ) then
  call Print_msg( NVERB_ERROR, 'IO', 'Les_diachro_3D', 'unexpected type for some dimensions of ' &
                  // Trim( tzfield%cmnhname ) )
  tzfield%ndimlist(4:) = NMNHDIM_UNUSED
end if

if (         tzfield%ndimlist(1) == NMNHDIM_BUDGET_LES_LEVEL                                                 &
     .and.   tzfield%ndimlist(2) == NMNHDIM_BUDGET_LES_TIME                                                  &
     .and. ( tzfield%ndimlist(3) == NMNHDIM_BUDGET_LES_MASK .or.tzfield%ndimlist(3) == NMNHDIM_BUDGET_TERM ) ) then
  if ( .not. Present( hsuffixes ) ) &
    call Print_msg( NVERB_ERROR, 'IO', 'Les_diachro_3D', &
                    'optional dummy argument hsuffixes is needed for tpfield (' // Trim( tzfield%cmnhname ) // ')' )

  if ( Size( hsuffixes ) /= Size( pfield, 3) ) &
    call Print_msg( NVERB_FATAL, 'IO', 'Les_diachro_3D', 'wrong size for hsuffixes (' // Trim( tzfield%cmnhname ) // ')' )

  tzfield%ndimlist(4) = NMNHDIM_UNUSED
  call Les_diachro_common( tpdiafile, tzfield,                                                                &
                           reshape( pfield, [ size( pfield, 1 ), size( pfield, 2 ), size( pfield, 3 ), 1 ] ), &
                           odoavg, odonorm, hsuffixes )
else if (       tzfield%ndimlist(1) == NMNHDIM_BUDGET_LES_LEVEL &
          .and. tzfield%ndimlist(2) == NMNHDIM_BUDGET_LES_TIME  &
          .and. tzfield%ndimlist(3) == NMNHDIM_BUDGET_LES_SV    ) then
  if ( Present( hsuffixes ) ) &
    call Print_msg( NVERB_ERROR, 'IO', 'Les_diachro_3D', &
                    'optional dummy argument hsuffixes is not needed for tpfield (' // Trim( tzfield%cmnhname ) // ')' )

  tzfield%ndimlist(4) = tzfield%ndimlist(3)
  tzfield%ndimlist(3) = NMNHDIM_UNUSED
  call Les_diachro_common( tpdiafile, tzfield,                                                                &
                           reshape( pfield, [ size( pfield, 1 ), size( pfield, 2 ), 1, size( pfield, 3 ) ] ), &
                           odoavg, odonorm )
else
  call Print_msg( NVERB_ERROR, 'IO', 'Les_diachro_3D', &
                  'ndimlist configuration not yet implemented for ' // Trim( tzfield%cmnhname ) )
end if

end subroutine Les_diachro_3D

!##################################################################################
subroutine Les_diachro_4D( tpdiafile, tpfield, odoavg, odonorm, pfield, hsuffixes )
!##################################################################################

use modd_field, only: NMNHDIM_BUDGET_LES_LEVEL, NMNHDIM_BUDGET_LES_MASK, NMNHDIM_BUDGET_LES_SV, &
                      NMNHDIM_BUDGET_LES_TIME,  NMNHDIM_BUDGET_TERM,     NMNHDIM_UNUSED,        &
                      tfield_metadata_base
use modd_io,    only: tfiledata

type(tfiledata),                             intent(in) :: tpdiafile  ! File to write
type(tfield_metadata_base), intent(in) :: tpfield ! Metadata of field
logical,                                     intent(in) :: odoavg     ! Compute and store time average
logical,                                     intent(in) :: odonorm    ! Compute and store normalized field
real,                    dimension(:,:,:,:), intent(in) :: pfield     ! Data array
character(len=*),        dimension(:),     optional, intent(in) :: hsuffixes

type(tfield_metadata_base) :: tzfield

tzfield = tpfield

if ( tzfield%ndims /= 4 ) then
  call Print_msg( NVERB_ERROR, 'IO', 'Les_diachro_4D', 'ndims /= 4 for ' // Trim( tzfield%cmnhname ) )
  tzfield%ndims = 4
end if

if ( Any( tzfield%ndimlist(5:) /= NMNHDIM_UNUSED ) ) then
  call Print_msg( NVERB_ERROR, 'IO', 'Les_diachro_4D', 'unexpected type for some dimensions of ' &
                  // Trim( tzfield%cmnhname ) )
  tzfield%ndimlist(5:) = NMNHDIM_UNUSED
end if

if (         tzfield%ndimlist(1) == NMNHDIM_BUDGET_LES_LEVEL                                                 &
     .and.   tzfield%ndimlist(2) == NMNHDIM_BUDGET_LES_TIME                                                  &
     .and. ( tzfield%ndimlist(3) == NMNHDIM_BUDGET_LES_MASK .or.tzfield%ndimlist(3) == NMNHDIM_BUDGET_TERM ) &
     .and.   tzfield%ndimlist(4) == NMNHDIM_BUDGET_LES_SV                                                    ) then
  if ( .not. Present( hsuffixes ) ) &
    call Print_msg( NVERB_ERROR, 'IO', 'Les_diachro_4D', &
                    'optional dummy argument hsuffixes is needed for tpfield (' // Trim( tzfield%cmnhname ) // ')' )

  if ( Size( hsuffixes ) /= Size( pfield, 3) ) &
    call Print_msg( NVERB_FATAL, 'IO', 'Les_diachro_4D', 'wrong size for hsuffixes (' // Trim( tzfield%cmnhname ) // ')' )

  call Les_diachro_common( tpdiafile, tzfield, pfield, odoavg, odonorm, hsuffixes )
else
  call Print_msg( NVERB_ERROR, 'IO', 'Les_diachro_4D', &
                  'ndimlist configuration not yet implemented for ' // Trim( tzfield%cmnhname ) )
end if

end subroutine Les_diachro_4D

!#######################################################################################
subroutine Les_diachro_common( tpdiafile, tpfield, pfield, odoavg, odonorm, hsuffixes )
!#######################################################################################

use modd_field,         only: tfield_metadata_base
use modd_io,            only: tfiledata
use modd_les,           only: nles_current_iinf, nles_current_isup, nles_current_jinf, nles_current_jsup, &
                              nles_levels, xles_current_z
use modd_parameters,    only: XUNDEF
use modd_type_date,     only: date_time

implicit none

type(tfiledata),                                          intent(in) :: tpdiafile ! File to write
type(tfield_metadata_base),                               intent(in) :: tpfield
real,                       dimension(:,:,:,:),           intent(in) :: pfield    ! Data array
logical,                                                  intent(in) :: odoavg    ! Compute and store time average
logical,                                                  intent(in) :: odonorm   ! Compute and store normalized field
character(len=*),           dimension(:),       optional, intent(in) :: hsuffixes

character(len=100),         dimension(:),     allocatable :: ycomment                      ! Comment string
character(len=100),         dimension(:),     allocatable :: ytitle                        ! Title
integer                                                   :: iavg
integer                                                   :: iles_k                        ! Number of vertical levels
integer                                                   :: iil, iih, ijl, ijh, ikl, ikh  ! Cartesian area relatively to the
                                                                                           ! entire domain
integer                                                   :: jk                            ! Vertical loop counter
real,                       dimension(:,:,:), allocatable :: ztrajx                        ! Localization of the temporal
real,                       dimension(:,:,:), allocatable :: ztrajy                        ! series in x,y and z. remark:
real,                       dimension(:,:,:), allocatable :: ztrajz                        ! x and y are not used for LES
type(tfield_metadata_base), dimension(:),     allocatable :: tzfields
!------------------------------------------------------------------------------

iles_k = Size( pfield, 1 )

! Initialization of diachro variables for les (z,t) profiles
Allocate( ztrajx(1,      1, Size( pfield, 4 )) )
Allocate( ztrajy(1,      1, Size( pfield, 4 )) )
Allocate( ztrajz(iles_k, 1, Size( pfield, 4 )) )
Allocate( ycomment(Size( pfield, 3 )) )
Allocate( ytitle  (Size( pfield, 3 )) )
Allocate( tzfields(Size( pfield, 3 )) )

iil = nles_current_iinf
iih = nles_current_isup
ijl = nles_current_jinf
ijh = nles_current_jsup
ikl = nles_levels(1)
ikh = nles_levels(iles_k)

ztrajx(:, :, :) = ( iil + iih ) / 2
ztrajy(:, :, :) = ( ijl + ijh ) / 2
do jk = 1, iles_k
  ztrajz(jk, :, :) = xles_current_z(jk)
end do

if ( Present( hsuffixes ) ) then
  if ( Size( hsuffixes ) /= Size( pfield, 3) ) &
    call Print_msg( NVERB_FATAL, 'IO', 'Les_diachro_common', 'wrong size for hsuffixes (' // Trim( tpfield%cmnhname ) // ')' )
  ycomment(:) = Trim( tpfield%ccomment(:) ) // hsuffixes(:)
else
  ycomment(:) = tpfield%ccomment(:)
end if
                            call Les_diachro_common_intern( .false., .false. )
if ( odoavg )               call Les_diachro_common_intern( .true.,  .false. )
if ( odonorm )              call Les_diachro_common_intern( .false.,  .true. )
if ( odoavg .and. odonorm ) call Les_diachro_common_intern( .true.,  .true. )


contains

!#######################################################################################
subroutine Les_diachro_common_intern( oavg, onorm )
!#######################################################################################

use modd_field,         only: NMNHDIM_BUDGET_LES_TIME, NMNHDIM_BUDGET_LES_AVG_TIME, NMNHDIM_BUDGET_LES_SV, NMNHDIM_UNUSED
use modd_les,           only: nles_current_times

use mode_write_diachro, only: Write_diachro

logical, intent(in) :: oavg
logical, intent(in) :: onorm

character(len=10)                                    :: ygroup  ! Group title
integer                                              :: iresp   ! Return code
integer                                              :: ji
integer                                              :: jp      ! Process loop counter
integer                                              :: jsv     ! Scalar loop counter
logical                                              :: gsv
real,            dimension(:,:,:,:),     allocatable :: zfield  ! Normalized field
real,            dimension(:,:,:,:,:,:), allocatable :: zwork6  ! Contains physical field
type(date_time), dimension(:),           allocatable :: tzdates

!Reallocate each time necessary because can be reallocated to an other size in Les_time_avg
Allocate( zfield(Size( pfield, 1 ), Size( pfield, 2 ), Size( pfield, 3 ), Size( pfield, 4 )) )
Allocate( tzdates( nles_current_times ) )

tzdates(:) = xles_dates(:)

!Copy all fields from tpfield
tzfields(:) = tpfield

if ( onorm ) then
  gsv = tzfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_SV
  call Les_norm_4d( tzfields(1)%cunits, pfield, zfield, gsv )

  ! Normalization of vertical dimension
  tzfields(:)%cunits = '1'
  call Les_z_norm( oavg, ztrajz, zfield(:,:,:,:) )
else
  zfield(:, :, :, :) = pfield(:, :, :, :)
end if

! Time average
iresp = 0
if ( oavg ) call Les_time_avg_4d( zfield, tzdates, iresp )

if ( oavg ) then
  if ( onorm ) then
    ygroup = 'H_' // tpfield%cmnhname
  else
    ygroup = 'A_' // tpfield%cmnhname
  end if
else
  if ( onorm ) then
    ygroup = 'E_' // tpfield%cmnhname
  else
    ygroup = tpfield%cmnhname
  end if
endif

if ( Present( hsuffixes ) ) then
  ytitle(:) = ygroup // hsuffixes(:)
else
  ytitle(:) = ygroup
endif

! Write the profile
if ( iresp == 0 .and. any( zfield /= XUNDEF ) ) then
  allocate(zwork6(1,1,size(zfield,1),size(zfield,2),size(zfield,4),size(zfield,3)))
  do jsv = 1, size( zfield, 4 )
    do jp = 1, size( zfield, 3 )
      zwork6(1, 1, :, :, jsv, jp) = zfield (:, :, jp, jsv)
    end do
  end do

  tzfields(:)%ndimlist(6) = tpfield%ndimlist(3)
  tzfields(:)%ndimlist(5) = tpfield%ndimlist(4)
  tzfields(:)%ndimlist(4) = tpfield%ndimlist(2)
  tzfields(:)%ndimlist(3) = tpfield%ndimlist(1)
  tzfields(:)%ndimlist(2) = NMNHDIM_UNUSED
  tzfields(:)%ndimlist(1) = NMNHDIM_UNUSED

  if ( oavg ) then
    do ji = 1, 6
      if ( tzfields(1)%ndimlist(ji) == NMNHDIM_BUDGET_LES_TIME ) tzfields(:)%ndimlist(ji) = NMNHDIM_BUDGET_LES_AVG_TIME
    end do
  end if

  tzfields(:)%cmnhname  = ytitle(:)
  tzfields(:)%clongname = ytitle(:)
  tzfields(:)%ccomment  = ycomment(:)

  call Write_diachro( tpdiafile, tzfields, ygroup, "SSOL", tzdates,                     &
                      zwork6,                                                           &
                      oicp = .false., ojcp = .false., okcp = .false.,                   &
                      kil = iil, kih = iih, kjl = ijl, kjh = ijh, kkl = ikl, kkh = ikh, &
                      ptrajx = ztrajx, ptrajy = ztrajy, ptrajz = ztrajz                 )
end if

!-------------------------------------------------------------------------------
end subroutine Les_diachro_common_intern
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
end subroutine Les_diachro_common
!------------------------------------------------------------------------------

!############################################################################
subroutine Les_diachro_2pt( tpdiafile, tpfieldx, tpfieldy, pfieldx, pfieldy )
!############################################################################
!
!* Modification 01/04/03 (V. Masson) safer use of ZWORK6 with loops
!
!
use modd_conf,       only: l2d
use modd_field,      only: tfield_metadata_base
use modd_io,         only: tfiledata
use modd_les,        only: xles_temp_mean_start, xles_temp_mean_end
use modd_parameters, only: XUNDEF
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
type(tfiledata),                    intent(in) :: tpdiafile! file to write
type(tfield_metadata_base),         intent(in) :: tpfieldx ! Metadata of field pfieldx
type(tfield_metadata_base),         intent(in) :: tpfieldy ! Metadata of field pfieldy
real,             dimension(:,:,:), intent(in) :: pfieldx
real,             dimension(:,:,:), intent(in) :: pfieldy
!-------------------------------------------------------------------------------

                call Les_diachro_2pt_1d_intern( tpdiafile, tpfieldx, .false., pfieldx )
if ( .not.l2d ) call Les_diachro_2pt_1d_intern( tpdiafile, tpfieldy, .false., pfieldy )

! With time average
if ( xles_temp_mean_start /= xundef .and. xles_temp_mean_end /= XUNDEF ) then
                  call Les_diachro_2pt_1d_intern( tpdiafile, tpfieldx, .true., pfieldx )
  if ( .not.l2d ) call Les_diachro_2pt_1d_intern( tpdiafile, tpfieldy, .true., pfieldy )
end if

end subroutine Les_diachro_2pt
!-------------------------------------------------------------------------------

!#######################################################################
subroutine Les_diachro_2pt_1d_intern( tpdiafile, tpfield, yavg, pfield )
!#######################################################################

use modd_field,         only: NMNHDIM_BUDGET_LES_AVG_TIME, NMNHDIM_BUDGET_LES_TIME, NMNHDIM_UNUSED, &
                              NMNHDIM_SPECTRA_2PTS_NI, NMNHDIM_SPECTRA_2PTS_NJ,                     &
                              NMNHMAXDIMS, tfield_metadata_base
use modd_io,            only: tfiledata
use modd_les,           only: nles_current_iinf, nles_current_isup, nles_current_jinf, nles_current_jsup, &
                              nles_current_times, nspectra_k, xles_current_domegax, xles_current_domegay
use modd_type_date,     only: date_time

use mode_write_diachro, only: Write_diachro

type(tfiledata),                    intent(in) :: tpdiafile! file to write
type(tfield_metadata_base),         intent(in) :: tpfield ! Metadata of field pfield
logical,                            intent(in) :: yavg
real,             dimension(:,:,:), intent(in) :: pfield

character(len=6)                                     :: ystring
character(len= 10)                                   :: ygroup   ! group title
character(len=100)                                   :: ycomment ! comment string
integer                                              :: iil, iih, ijl, ijh, ikl, ikh  ! cartesian area relatively to the
integer                                              :: iresp    ! return code
integer                                              :: ji
integer                                              :: jt       ! time counter
integer                                              :: jk       ! level counter
real,            dimension(:,:,:,:,:,:), allocatable :: zwork6 ! contains physical field
type(date_time), dimension(:),           allocatable :: tzdates
type(tfield_metadata_base)                           :: tzfield

!*      1.0  Initialization of diachro variables for LES (z,t) profiles
!            ----------------------------------------------------------

allocate( tzdates( NLES_CURRENT_TIMES ) )
tzdates(:) = xles_dates(:)

ikl = 1
ikh = nspectra_k

!Copy all fields from tpfield
tzfield = tpfield

if ( tzfield%ndimlist(1) == NMNHDIM_SPECTRA_2PTS_NI ) then
  Allocate( zwork6(Size( pfield, 1 ), 1, nspectra_k, nles_current_times, 1, 1) )

  iil = nles_current_iinf
  iih = nles_current_isup
  ijl = 1
  ijh = 1

  do jt = 1, Size( pfield,  3 )
    do jk = 1, Size( pfield, 2 )
      zwork6(:, 1, jk, jt, 1, 1) = pfield (:, jk, jt)
    end do
  end do

  tzfield%ndimlist(6) = NMNHDIM_UNUSED
  tzfield%ndimlist(5) = NMNHDIM_UNUSED
  tzfield%ndimlist(4) = tpfield%ndimlist(3)
  tzfield%ndimlist(3) = tpfield%ndimlist(2)
  tzfield%ndimlist(2) = NMNHDIM_UNUSED
  tzfield%ndimlist(1) = tpfield%ndimlist(1)

  ygroup    = 'CI_' // tpfield%cmnhname
  Write( ystring, fmt = "( i6.6 )" ) Nint( xles_current_domegax )
  ycomment(:) = " DOMEGAX=" // ystring // ' ' // tpfield%ccomment
else if ( tzfield%ndimlist(1) == NMNHDIM_SPECTRA_2PTS_NJ ) then
  Allocate( zwork6(1, Size( pfield, 1 ), nspectra_k, nles_current_times, 1, 1) )

  iil = 1
  iih = 1
  ijl = nles_current_jinf
  ijh = nles_current_jsup

  do jt = 1, Size( pfield, 3 )
    do jk = 1, Size( pfield, 2 )
      zwork6(1, :, jk, jt, 1, 1) = pfield (:, jk, jt)
    end do
  end do

  tzfield%ndimlist(6) = NMNHDIM_UNUSED
  tzfield%ndimlist(5) = NMNHDIM_UNUSED
  tzfield%ndimlist(4) = tpfield%ndimlist(3)
  tzfield%ndimlist(3) = tpfield%ndimlist(2)
  tzfield%ndimlist(2) = tpfield%ndimlist(1)
  tzfield%ndimlist(1) = NMNHDIM_UNUSED

  ygroup    = 'CJ_' // tpfield%cmnhname
  Write( ystring, fmt ="( i6.6 )" ) Nint( xles_current_domegay )
  ycomment(:) = " DOMEGAY=" // ystring // ' ' // tpfield%ccomment
else
  call Print_msg( NVERB_FATAL, 'BUD', 'Les_diachro_2pt_1d_intern', 'invalid dimensions for' // Trim( tpfield%cmnhname ) )
end if

!Done here because ygroup is modified later
tzfield%cmnhname  = ygroup
tzfield%clongname = ygroup
tzfield%ccomment  = ycomment(:)

!* time average
iresp = 0
if ( yavg ) then
  call Les_time_avg( zwork6, tzdates, iresp )
  ygroup    = 'T_'//ygroup

  do ji = 1, NMNHMAXDIMS
    if ( tzfield%ndimlist(ji) == NMNHDIM_BUDGET_LES_TIME ) tzfield%ndimlist(ji) = NMNHDIM_BUDGET_LES_AVG_TIME
  end do
end if

!*      2.0  Writing of the profile
!            ----------------------
if ( iresp == 0 ) then
    call Write_diachro( tpdiafile, [ tzfield ], ygroup, "SPXY", tzdates,                 &
                        zwork6,                                                          &
                        oicp = .false., ojcp = .false., okcp = .false.,                  &
                        kil = iil, kih = iih, kjl = ijl, kjh = ijh, kkl = ikl, kkh = ikh )
end if

end subroutine Les_diachro_2pt_1d_intern
!------------------------------------------------------------------------------

!#################################################################################
subroutine Les_diachro_spec( tpdiafile, tpfieldx, tpfieldy, pspectrax, pspectray )
!#################################################################################
!
!* Modification 01/04/03 (V. Masson) safer use of ZWORK6 with loops
!
!
use modd_conf,  only: l2d
use modd_field, only: tfield_metadata_base
use modd_io,    only: tfiledata

implicit none

type(tfiledata),                      intent(in) :: tpdiafile! file to write
type(tfield_metadata_base),           intent(in) :: tpfieldx ! metadata of field pfieldx
type(tfield_metadata_base),           intent(in) :: tpfieldy ! metadata of field pfieldy
real,             dimension(:,:,:,:), intent(in) :: pspectrax! spectra in x
real,             dimension(:,:,:,:), intent(in) :: pspectray! and y directions

                 call Les_diachro_spec_1D_intern( tpdiafile, tpfieldx, pspectrax )
if ( .not. l2d ) call Les_diachro_spec_1D_intern( tpdiafile, tpfieldy, pspectray )

end subroutine Les_diachro_spec


!####################################################################
subroutine Les_diachro_spec_1D_intern( tpdiafile, tpfield, pspectra )
!####################################################################

use modd_field,         only: NMNHDIM_BUDGET_LES_AVG_TIME, NMNHDIM_BUDGET_LES_TIME, NMNHDIM_UNUSED, &
                              NMNHDIM_SPECTRA_SPEC_NI, NMNHDIM_SPECTRA_SPEC_NJ,                     &
                              NMNHMAXDIMS, tfield_metadata_base
use modd_io,            only: tfiledata
use modd_les,           only: nles_current_iinf, nles_current_isup, nles_current_jinf, nles_current_jsup, &
                              nles_current_times, nspectra_k, &
                              xles_current_domegax, xles_current_domegay
use modd_les_n,         only: xles_dates
use modd_type_date,     only: date_time

use mode_write_diachro, only: Write_diachro

implicit none

type(tfiledata),             intent(in) :: tpdiafile ! file to write
type(tfield_metadata_base),  intent(in) :: tpfield   ! metadata of field pfield
real, dimension(:,:,:,:),    intent(in) :: pspectra

character(len=10)                                    :: ygroup   ! group title
character(len=100)                                   :: ycomment ! comment string
character(len=6)                                     :: ystring
integer                                              :: iresp    ! return code
integer                                              :: iil, iih, ijl, ijh, ikl, ikh  !cartesian area relative to the entire domain
integer                                              :: ji
integer                                              :: jt       ! time counter
integer                                              :: jk       ! level counter
real,            dimension(:,:,:,:,:,:), allocatable :: zwork6   ! physical field
type(date_time), dimension(:),           allocatable :: tzdates
type(tfield_metadata_base)                           :: tzfield
!
!*      1.0  Initialization of diachro variables for LES (z,t) profiles
!            ----------------------------------------------------------
allocate( tzdates( nles_current_times ) )
tzdates(:) = xles_dates(:)

ikl = 1
ikh = nspectra_k

!Copy all fields from tpfield
tzfield = tpfield
!
!*      2.0  Writing of the profile
!            ----------------------

if ( tzfield%ndimlist(1) == NMNHDIM_SPECTRA_SPEC_NI ) then
  Allocate( zwork6(Size( pspectra, 1 ), 1, nspectra_k, nles_current_times, 2, 1) )

  iil = nles_current_iinf
  iih = nles_current_isup
  ijl = 1
  ijh = 1

  do jt = 1, Size( pspectra, 4 )
    do jk = 1, Size( pspectra, 3 )
      zwork6(:, 1, jk, jt, 1, 1) = pspectra (:, 1, jk, jt)
      zwork6(:, 1, jk, jt, 2, 1) = pspectra (:, 2, jk, jt)
    end do
  end do

  tzfield%ndimlist(6:) = NMNHDIM_UNUSED
  tzfield%ndimlist(5)  = tpfield%ndimlist(2)
  tzfield%ndimlist(4)  = tpfield%ndimlist(4)
  tzfield%ndimlist(3)  = tpfield%ndimlist(3)
  tzfield%ndimlist(2)  = NMNHDIM_UNUSED
  tzfield%ndimlist(1)  = tpfield%ndimlist(1)

  ygroup    = 'SI_' // tpfield%cmnhname
  Write( ystring, fmt = "( i6.6 )" ) Nint( xles_current_domegax )
  ycomment(:) = " DOMEGAX=" // ystring // ' ' // tpfield%ccomment
else if ( tzfield%ndimlist(1) == NMNHDIM_SPECTRA_SPEC_NJ ) then
  Allocate( zwork6( 1, Size( pspectra, 1 ), nspectra_k, nles_current_times, 2, 1 ) )

  iil = 1
  iih = 1
  ijl = nles_current_jinf
  ijh = nles_current_jsup

  do jt = 1, Size( pspectra, 4 )
    do jk = 1, Size( pspectra, 3 )
      zwork6(1, :, jk, jt, 1, 1) = pspectra (:, 1, jk, jt)
      zwork6(1, :, jk, jt, 2, 1) = pspectra (:, 2, jk, jt)
    end do
  end do

  tzfield%ndimlist(6:) = NMNHDIM_UNUSED
  tzfield%ndimlist(5)  = tpfield%ndimlist(2)
  tzfield%ndimlist(4)  = tpfield%ndimlist(4)
  tzfield%ndimlist(3)  = tpfield%ndimlist(3)
  tzfield%ndimlist(2)  = tpfield%ndimlist(1)
  tzfield%ndimlist(1)  = NMNHDIM_UNUSED

  ygroup    = 'SJ_' // tpfield%cmnhname
  Write( ystring, fmt = "( i6.6 )" ) Nint( xles_current_domegay )
  ycomment(:) = " DOMEGAY=" // ystring // ' ' // tpfield%ccomment
else
  call Print_msg( NVERB_FATAL, 'BUD', 'Les_diachro_spec_1D_intern', 'invalid dimensions for' // Trim( tpfield%cmnhname ) )
end if

tzfield%cmnhname  = ygroup
tzfield%clongname = ygroup
tzfield%ccomment  = ycomment(:)

call Write_diachro( tpdiafile, [ tzfield ], ygroup, "SPXY", tzdates,                 &
                    zwork6,                                                          &
                    oicp = .false., ojcp = .false., okcp = .false.,                  &
                    kil = iil, kih = iih, kjl = ijl, kjh = ijh, kkl = ikl, kkh = ikh )
!
!* time average
!
iresp = 0
call Les_time_avg( zwork6, tzdates, iresp )
ygroup = 'T_' // ygroup
do ji = 1, NMNHMAXDIMS
  if ( tzfield%ndimlist(ji) == NMNHDIM_BUDGET_LES_TIME ) tzfield%ndimlist(ji) = NMNHDIM_BUDGET_LES_AVG_TIME
end do

if ( iresp == 0 ) then
  call Write_diachro( tpdiafile, [ tzfield ], ygroup, "SPXY", tzdates,                 &
                      zwork6,                                                          &
                      oicp = .false., ojcp = .false., okcp = .false.,                  &
                      kil = iil, kih = iih, kjl = ijl, kjh = ijh, kkl = ikl, kkh = ikh )
endif

end subroutine Les_diachro_spec_1D_intern

!-------------------------------------------------------------------------------
END MODULE MODE_LES_DIACHRO
