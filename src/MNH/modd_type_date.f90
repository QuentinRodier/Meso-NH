!MNH_LIC Copyright 1997-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!####################
module modd_type_date
!####################
!
!!****  *MODD_TYPE_DATE* - declaration of temporal types
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to define
!      the time types
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      NONE 
!!
!!    REFERENCE
!!    --------- 
!!      Book2 of documentation of Meso-NH (module MODD_TYPE_DATE)
!!
!!    AUTHOR
!!    ------
!!	P. Jabouille   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    11/08/97
!  P. Wautelet 24/07/2019: set default values
!  P. Wautelet 17/12/2020: restructure type date_time
!  P. Wautelet 11/07/2022: add Datetime_initialized_check function
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------

use modd_parameters, only: NNEGUNDEF, XNEGUNDEF

implicit none

type date
  integer :: nyear  = NNEGUNDEF
  integer :: nmonth = 0
  integer :: nday   = 0
end type date

#if 0
!GCC BUG: if an extended type is used in an array for a namelist, the reading fails
!GCC bug (at least from 5.5 to 12.1, see GCC bug 106065)
type, extends( date ) :: date_time
  real :: xtime = XNEGUNDEF
end type date_time
#else
type :: date_time
  integer :: nyear  = NNEGUNDEF
  integer :: nmonth = 0
  integer :: nday   = 0
  real :: xtime = XNEGUNDEF

  contains
    procedure, pass(tpdt) :: check => Datetime_initialized_check
end type date_time
#endif

contains

logical function Datetime_initialized_check( tpdt, hname ) result( gok )
  !Check if the values of the date_time type have been set and are valid
  !Remark: xtime must be inside the day (between 0. and 24h)

  use mode_msg

  class( date_time ),           intent(in) :: tpdt
  character(len=*),   optional, intent(in) :: hname !Name of the variable (useful for messages)

  character(len=:), allocatable :: yname
  logical :: gdayok

  gok = .true.

  if ( Present( hname ) ) then
    yname = Trim( hname ) // ': '
  else
    yname = ''
  end if

  if ( tpdt%nyear == NNEGUNDEF ) then
    call Print_msg( NVERB_WARNING, 'GEN', 'Datetime_initialized_check', yname // 'year has not been set' )
    gok = .false.
  end if

  if ( tpdt%nmonth == 0 ) then
    call Print_msg( NVERB_WARNING, 'GEN', 'Datetime_initialized_check', yname // 'month has not been set (=0)' )
    gok = .false.
  else if ( tpdt%nmonth < 0 .or. tpdt%nmonth > 12 ) then
    Write( cmnhmsg(1), '( A, "invalid month: ", I18 )' ) Trim( yname ), tpdt%nmonth
    call Print_msg( NVERB_WARNING, 'GEN', 'Datetime_initialized_check' )
    gok = .false.
  end if

  if ( tpdt%nday == 0 ) then
    call Print_msg( NVERB_WARNING, 'GEN', 'Datetime_initialized_check', yname // 'day has not been set (=0)' )
    gdayok = .false.
    gok = .false.
  else
    gdayok = .true.
    if ( tpdt%nday < 1 ) then
      gdayok = .false.
    else if ( Any( tpdt%nmonth == [ 1, 3, 5, 7, 8, 10, 12 ] ) .and. tpdt%nday > 31 ) then
      gdayok = .false.
    else if ( Any( tpdt%nmonth == [ 4, 6, 9, 11 ] ) .and. tpdt%nday > 30 ) then
      gdayok = .false.
    else if ( tpdt%nmonth == 2 ) then
      if ( ( Mod( tpdt%nyear, 4 ) == 0 .and. Mod( tpdt%nyear, 100 ) /= 0 ) .or. Mod( tpdt%nyear, 400 ) == 0 ) then
        if ( tpdt%nday > 29 ) then
          gdayok = .false.
        end if
      else if ( tpdt%nday > 28 ) then
        gdayok = .false.
      end if
    end if
    if ( .not. gdayok ) then
      Write( cmnhmsg(1), '( A, "invalid day", I18, " for month: ", I18 )' ) Trim( yname ), tpdt%nday, tpdt%nmonth
      call Print_msg( NVERB_WARNING, 'GEN', 'Datetime_initialized_check' )
      gok = .false.
    end if
  end if

  if ( tpdt%xtime == XNEGUNDEF ) then
    call Print_msg( NVERB_WARNING, 'GEN', 'Datetime_initialized_check', yname // 'time has not been set' )
    gok = .false.
  else if ( tpdt%xtime < 0 .or. tpdt%xtime > ( 3600. * 24 ) ) then
    Write( cmnhmsg(1), '( A, "invalid time: ", EN12.3 )' ) Trim( yname ), tpdt%xtime
    call Print_msg( NVERB_WARNING, 'GEN', 'Datetime_initialized_check' )
    gok = .false.
  end if

end function Datetime_initialized_check

end module modd_type_date
