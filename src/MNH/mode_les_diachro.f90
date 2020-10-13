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

public :: LES_DIACHRO, Les_diachro_2pt, LES_DIACHRO_MASKS, Les_diachro_spec, &
          LES_DIACHRO_SURF, LES_DIACHRO_SURF_SV, LES_DIACHRO_SV, LES_DIACHRO_SV_MASKS

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
use modd_time,      only: tdtseg
USE MODD_LES
USE MODD_TYPE_DATE, only: date_time
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
  !
  DO JP=1,SIZE(PWORK6,6)
    DO JSV=1,SIZE(PWORK6,5)
      DO JK=1,SIZE(PWORK6,3)
        DO JY=1,SIZE(PWORK6,2)
          DO JX=1,SIZE(PWORK6,1)
            ITIME=0
            DO JT=1,NLES_CURRENT_TIMES
              IF ( xles_times(JT) >= ZLES_TEMP_MEAN_START .AND. &
                   xles_times(JT) <= ZLES_TEMP_MEAN_END         ) THEN
                IF (PWORK6(JX,JY,JK,JT,JSV,JP) /= XUNDEF) THEN
                 ZWORK6(JX,JY,JK,JAVG,JSV,JP) =  ZWORK6(JX,JY,JK,JAVG,JSV,JP) &
                                              + PWORK6(JX,JY,JK,JT,JSV,JP)
                 ITIME=ITIME+1
                END IF
              END IF
            END DO
            IF (ITIME >= 1) THEN
                    ZWORK6(JX,JY,JK,JAVG,JSV,JP)= &
                            ZWORK6(JX,JY,JK,JAVG,JSV,JP) / ITIME
            END IF
            IF (ITIME == 0) THEN
                   ZWORK6(JX,JY,JK,JAVG,JSV,JP)= XUNDEF
            END IF
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
!
!########################################################
subroutine Les_diachro_gen(tpdiafile, hgroup, hcomment, hunit, pfield, havg, htitle, osurf, osv )
!########################################################

use modd_field,         only: NMNHDIM_UNKNOWN, tfield_metadata_base, TYPEREAL
use modd_io,            only: tfiledata
use modd_les,           only: nles_current_iinf, nles_current_isup, nles_current_jinf, nles_current_jsup, nles_current_times, &
                              nles_k, nles_levels, xles_current_z, xles_temp_mean_start, xles_temp_mean_end
use modd_parameters,    only: XUNDEF
use modd_type_date,     only: date_time

use mode_write_diachro, only: Write_diachro

implicit none
!
!*      0.1  Declarations of arguments
!
type(tfiledata),                                intent(in) :: tpdiafile ! File to write
character(len=*),                               intent(in) :: hgroup    ! Group title
character(len=*), dimension(:),                 intent(in) :: hcomment  ! Comment string
character(len=*),                               intent(in) :: hunit     ! Physical unit
real,             dimension(:,:,:,:),           intent(in) :: pfield    ! Data array
character(len=1),                               intent(in) :: havg      ! Flag to compute avg.
character(len=*), dimension(:),       optional, intent(in) :: htitle    ! Title
logical,                              optional, intent(in) :: osurf     ! Flag to know if pfield is a surface variable
logical,                              optional, intent(in) :: osv       ! Flag for scalar variables
!
!*      0.2  Declaration of local variables for diachro
!
character(len=10)                                       :: ygroup                        ! Group title
character(len=100), dimension(:),           allocatable :: ycomment                      ! Comment string
character(len=100), dimension(:),           allocatable :: ytitle                        ! Title
character(len=100), dimension(:),           allocatable :: yunit                         ! Physical unit
integer                                                 :: iresp                         ! Return code
integer                                                 :: iles_k                        ! Number of vertical levels
integer                                                 :: iil, iih, ijl, ijh, ikl, ikh  ! Cartesian area relatively to the
                                                                                         ! entire domain
integer                                                 :: jk                            ! Vertical loop counter
integer                                                 :: jp                            ! Process loop counter
integer                                                 :: jsv                           ! Scalar loop counter
integer,            dimension(:),           allocatable :: igrid                         ! Grid indicator
logical                                                 :: gavg                          ! Flag to compute time averagings
logical                                                 :: gnorm                         ! Flag to compute normalizations
logical                                                 :: gsurf                         ! Flag for surface variables
logical                                                 :: gsv                           ! Flag for scalar variables
real,               dimension(:,:,:),       allocatable :: ztrajx                        ! Localization of the temporal
real,               dimension(:,:,:),       allocatable :: ztrajy                        ! series in x,y and z. remark:
real,               dimension(:,:,:),       allocatable :: ztrajz                        ! x and y are not used for LES
real,               dimension(size(pfield,1),size(pfield,2),size(pfield,3),size(pfield,4))    &
                                                        :: zfield                        ! Normalized field
real,               dimension(:,:,:,:,:,:), allocatable :: zwork6                        ! Contains physical field
type(date_time),    dimension(:),           allocatable :: tzdates
type(tfield_metadata_base), dimension(:),   allocatable :: tzfields
!------------------------------------------------------------------------------

if ( present( osurf ) .and. present ( htitle ) ) then
  call Print_msg( NVERB_ERROR, 'BUD', 'LES_DIACHRO_gen', &
                  'unexpected: osurf and htitle simultaneously present (not yet validated)' )
end if

if ( present( osurf ) ) then
  gsurf = osurf
else
  gsurf = .false.
end if

if ( present( osv ) ) then
  gsv = osv
else
  gsv = .false.
end if

if ( gsurf ) then
  iles_k = 1
else
  iles_k = nles_k
end if

gavg  = ( havg == 'A' .or. havg == 'H' )
gnorm = ( havg == 'E' .or. havg == 'H' )

if ( gavg .and. ( xles_temp_mean_start == XUNDEF .or. xles_temp_mean_end == XUNDEF ) ) return

if ( gnorm ) then
  if ( gsurf) then
    return
  else
    call Les_norm_4d( hunit, pfield, zfield, gsv )
  end if
else
  zfield(:, :, :, :) = pfield(:, :, :, :)
end if

! Initialization of diachro variables for les (z,t) profiles
allocate( ztrajx( 1,      1, size( pfield, 4 ) ) )
allocate( ztrajy( 1,      1, size( pfield, 4 ) ) )
allocate( ztrajz( iles_k, 1, size( pfield, 4 ) ) )
allocate( zwork6( 1, 1, iles_k, nles_current_times, size( pfield, 4 ), size( pfield, 3 ) ) )
allocate( igrid   ( size( pfield, 3 ) ) )
allocate( ycomment( size( pfield, 3 ) ) )
allocate( ytitle  ( size( pfield, 3 ) ) )
allocate( yunit   ( size( pfield, 3 ) ) )
allocate( tzdates( nles_current_times ) )

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

igrid(:) = 1
ycomment(:) = hcomment(:)
yunit(:)    = hunit(:)
ygroup      = hgroup

tzdates(:) = xles_dates(:)

! Normalization of vertical dimension
if ( gnorm ) then
  if ( hunit(1:1) /= ' ') yunit = '1'
  call Les_z_norm( gavg, ztrajz, zfield(:,:,:,:) )
end if

do jsv = 1, size( pfield, 4 )
  do jp = 1, size( pfield, 3 )
    zwork6(1, 1, :, :, jsv, jp) = zfield (:, :, jp, jsv)
  end do
end do

! Time average
iresp = 0
if ( gavg ) call Les_time_avg( zwork6, tzdates, iresp )

if ( havg /= ' ' )  ygroup = havg // '_' // ygroup
if ( present( htitle ) ) then
  ytitle(:) = ygroup // htitle(:)
else
  if ( gsurf ) then
    ytitle(:) = hgroup
  else
    ytitle(:) = ygroup
  end if
end if

! Write the profile
if ( iresp==0 .and. ( gsurf .or. any( zwork6 /= xundef ) ) ) then
  allocate( tzfields( Size( pfield, 3 ) ) )

  tzfields(:)%cmnhname  = ytitle(:)
  tzfields(:)%cstdname  = ''
  tzfields(:)%clongname = ytitle(:)
  tzfields(:)%cunits    = yunit(:)
  tzfields(:)%ccomment  = ycomment(:)
  tzfields(:)%ngrid     = igrid
  tzfields(:)%ntype     = TYPEREAL
  tzfields(:)%ndims     = 6
  do jp = 1, Size( tzfields )
    tzfields(jp)%ndimlist(:) = NMNHDIM_UNKNOWN
  end do

  call Write_diachro( tpdiafile, tzfields, ygroup, "SSOL", tzdates,     &
                        zwork6,                                                           &
                        oicp = .false., ojcp = .false., okcp = .false.,                   &
                        kil = iil, kih = iih, kjl = ijl, kjh = ijh, kkl = ikl, kkh = ikh, &
                        ptrajx = ztrajx, ptrajy = ztrajy, ptrajz = ztrajz                 )

  deallocate( tzfields )
end if

!-------------------------------------------------------------------------------
end subroutine Les_diachro_gen
!------------------------------------------------------------------------------
!########################################################
SUBROUTINE LES_DIACHRO(TPDIAFILE,HGROUP,HCOMMENT,HUNIT,PFIELD,HAVG)
!########################################################

IMPLICIT NONE
!
!*      0.1  declarations of arguments
!
TYPE(TFILEDATA),      INTENT(IN)       :: TPDIAFILE! file to write
CHARACTER(LEN=*),     INTENT(IN)       :: HGROUP   ! group title
CHARACTER(LEN=*),     INTENT(IN)       :: HCOMMENT ! comment string
CHARACTER(LEN=*),     INTENT(IN)       :: HUNIT    ! physical unit
REAL, DIMENSION(:,:), INTENT(IN)       :: PFIELD
CHARACTER(LEN=1),     INTENT(IN)       :: HAVG     ! flag to compute avg.

call Les_diachro_gen( tpdiafile, hgroup, [ hcomment ], hunit, &
                      reshape( pfield, [ size( pfield, 1), size( pfield, 2), 1, 1 ] ), havg )

!-------------------------------------------------------------------------------
END SUBROUTINE LES_DIACHRO
!-------------------------------------------------------------------------------
!###########################################################
SUBROUTINE LES_DIACHRO_SV(TPDIAFILE,HGROUP,HCOMMENT,HUNIT,PFIELD,HAVG)
!###########################################################

IMPLICIT NONE
!
!*      0.1  declarations of arguments
!
TYPE(TFILEDATA),        INTENT(IN)       :: TPDIAFILE! file to write
CHARACTER(LEN=*),       INTENT(IN)       :: HGROUP   ! group title
CHARACTER(LEN=*),       INTENT(IN)       :: HCOMMENT ! comment string
CHARACTER(LEN=*),       INTENT(IN)       :: HUNIT    ! physical unit
REAL, DIMENSION(:,:,:), INTENT(IN)       :: PFIELD
CHARACTER(LEN=1),       INTENT(IN)       :: HAVG     ! flag to compute avg.

call Les_diachro_gen( tpdiafile, hgroup, [ hcomment ], hunit,                                            &
                      reshape( pfield, [ size( pfield, 1 ), size( pfield, 2 ), 1, size( pfield, 3 ) ] ), &
                      havg, osv = .true. )

!-------------------------------------------------------------------------------
END SUBROUTINE LES_DIACHRO_SV
!-------------------------------------------------------------------------------
!#####################################################################
SUBROUTINE LES_DIACHRO_MASKS(TPDIAFILE,HGROUP,HTITLE,HCOMMENT,HUNIT,PFIELD,HAVG)
!#####################################################################

IMPLICIT NONE
!
!*      0.1  declarations of arguments
!
TYPE(TFILEDATA),                    INTENT(IN) :: TPDIAFILE! file to write
CHARACTER(LEN=*),                   INTENT(IN) :: HGROUP   ! group title
CHARACTER(LEN=*), DIMENSION(:),     INTENT(IN) :: HTITLE   ! title
CHARACTER(LEN=*), DIMENSION(:),     INTENT(IN) :: HCOMMENT ! comment string
CHARACTER(LEN=*),                   INTENT(IN) :: HUNIT    ! physical unit
REAL,             DIMENSION(:,:,:), INTENT(IN) :: PFIELD
CHARACTER(LEN=1),                   INTENT(IN) :: HAVG     ! flag to compute avg.

call Les_diachro_gen( tpdiafile, hgroup, hcomment, hunit,                                                &
                      reshape( pfield, [ size( pfield, 1 ), size( pfield, 2 ), size( pfield, 3 ), 1 ] ), &
                      havg, htitle = htitle )

!-------------------------------------------------------------------------------
END SUBROUTINE LES_DIACHRO_MASKS
!-------------------------------------------------------------------------------
!########################################################################
SUBROUTINE LES_DIACHRO_SV_MASKS(TPDIAFILE,HGROUP,HTITLE,HCOMMENT,HUNIT,PFIELD,HAVG)
!########################################################################

IMPLICIT NONE
!
!*      0.1  declarations of arguments
!
TYPE(TFILEDATA),                      INTENT(IN) :: TPDIAFILE! file to write
CHARACTER(LEN=*),                     INTENT(IN) :: HGROUP   ! group title
CHARACTER(LEN=*), DIMENSION(:),       INTENT(IN) :: HTITLE   ! title
CHARACTER(LEN=*), DIMENSION(:),       INTENT(IN) :: HCOMMENT ! comment string
CHARACTER(LEN=*),                     INTENT(IN) :: HUNIT    ! physical unit
REAL,             DIMENSION(:,:,:,:), INTENT(IN) :: PFIELD
CHARACTER(LEN=1),                     INTENT(IN) :: HAVG     ! flag to compute avg.

call Les_diachro_gen( tpdiafile, hgroup, hcomment, hunit, pfield, &
                      havg, htitle = htitle, osv = .true.)

!-------------------------------------------------------------------------------
END SUBROUTINE LES_DIACHRO_SV_MASKS
!-------------------------------------------------------------------------------

!#############################################################
SUBROUTINE LES_DIACHRO_SURF(TPDIAFILE,HGROUP,HCOMMENT,HUNIT,PFIELD,HAVG)
!#############################################################

IMPLICIT NONE
!
!*      0.1  declarations of arguments
!
TYPE(TFILEDATA),      INTENT(IN)       :: TPDIAFILE! file to write
CHARACTER(LEN=*),     INTENT(IN)       :: HGROUP   ! group title
CHARACTER(LEN=*),     INTENT(IN)       :: HCOMMENT ! comment string
CHARACTER(LEN=*),     INTENT(IN)       :: HUNIT    ! physical unit
REAL, DIMENSION(:),   INTENT(IN)       :: PFIELD
CHARACTER(LEN=1),     INTENT(IN)       :: HAVG     ! flag to compute avg.

call Les_diachro_gen( tpdiafile, hgroup, [ hcomment ], hunit, &
                      reshape( pfield, [ 1, size( pfield, 1 ), 1, 1 ] ), havg, osurf = .true. )

!-------------------------------------------------------------------------------
END SUBROUTINE LES_DIACHRO_SURF
!-------------------------------------------------------------------------------
!################################################################
SUBROUTINE LES_DIACHRO_SURF_SV(TPDIAFILE,HGROUP,HCOMMENT,HUNIT,PFIELD,HAVG)
!################################################################

IMPLICIT NONE
!
!*      0.1  declarations of arguments
!
TYPE(TFILEDATA),        INTENT(IN)       :: TPDIAFILE! file to write
CHARACTER(LEN=*),       INTENT(IN)       :: HGROUP   ! group title
CHARACTER(LEN=*),       INTENT(IN)       :: HCOMMENT ! comment string
CHARACTER(LEN=*),       INTENT(IN)       :: HUNIT    ! physical unit
REAL, DIMENSION(:,:),   INTENT(IN)       :: PFIELD
CHARACTER(LEN=1),       INTENT(IN)       :: HAVG     ! flag to compute avg.

call Les_diachro_gen( tpdiafile, hgroup, [ hcomment ], hunit,                            &
                      reshape( pfield, [ 1, size( pfield, 1 ), 1, size( pfield, 2 ) ] ), &
                      havg, osurf = .true., osv = .true. )

!-------------------------------------------------------------------------------
END SUBROUTINE LES_DIACHRO_SURF_SV
!-------------------------------------------------------------------------------

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

                call Les_diachro_2pt_1d_intern( tpdiafile, tpfieldx, 'X', .false., pfieldx )
if ( .not.l2d ) call Les_diachro_2pt_1d_intern( tpdiafile, tpfieldy, 'Y', .false., pfieldy )

! With time average
if ( xles_temp_mean_start /= xundef .and. xles_temp_mean_end /= XUNDEF ) then
                  call Les_diachro_2pt_1d_intern( tpdiafile, tpfieldx, 'X', .true., pfieldx )
  if ( .not.l2d ) call Les_diachro_2pt_1d_intern( tpdiafile, tpfieldy, 'Y', .true., pfieldy )
end if

end subroutine Les_diachro_2pt
!-------------------------------------------------------------------------------

!#############################################################################
subroutine Les_diachro_2pt_1d_intern( tpdiafile, tpfield, hdir, yavg, pfield )
!#############################################################################

use modd_field,         only: NMNHDIM_BUDGET_LES_AVG_TIME, NMNHDIM_BUDGET_LES_TIME, NMNHDIM_UNUSED, &
                              NMNHMAXDIMS, tfield_metadata_base
use modd_io,            only: tfiledata
use modd_les,           only: nles_current_iinf, nles_current_isup, nles_current_jinf, nles_current_jsup, &
                              nles_current_times, nspectra_k, xles_current_domegax, xles_current_domegay
use modd_type_date,     only: date_time

use mode_write_diachro, only: Write_diachro

type(tfiledata),                    intent(in) :: tpdiafile! file to write
type(tfield_metadata_base),         intent(in) :: tpfield ! Metadata of field pfield
character,                          intent(in) :: hdir
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

if ( hdir /= 'X' .and. hdir /= 'Y' ) &
  call Print_msg( NVERB_FATAL, 'BUD', 'Les_diachro_2pt_1d_intern', 'invalid hdir' // hdir )

!*      1.0  Initialization of diachro variables for LES (z,t) profiles
!            ----------------------------------------------------------

allocate( tzdates( NLES_CURRENT_TIMES ) )
tzdates(:) = xles_dates(:)

ikl = 1
ikh = nspectra_k

!Copy all fields from tpfield
tzfield = tpfield

if ( hdir == 'X' ) then
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
else
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

                 call Les_diachro_spec_1D_intern( tpdiafile, tpfieldx, 'X', pspectrax )
if ( .not. l2d ) call Les_diachro_spec_1D_intern( tpdiafile, tpfieldy, 'Y', pspectray )

end subroutine Les_diachro_spec


!##########################################################################
subroutine Les_diachro_spec_1D_intern( tpdiafile, tpfield, hdir, pspectra )
!##########################################################################

use modd_field,         only: NMNHDIM_BUDGET_LES_AVG_TIME, NMNHDIM_BUDGET_LES_TIME, NMNHDIM_UNUSED, &
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
character,                   intent(in) :: hdir
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

if ( hdir /= 'X' .and. hdir /= 'Y' ) &
  call Print_msg( NVERB_FATAL, 'BUD', 'Les_diachro_spec_1D_intern', 'invalid hdir' // hdir )
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

if ( hdir == 'X' ) then
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
else
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
