!MNH_LIC Copyright 1997-2020 CNRS, Meteo-France and Universite Paul Sabatier
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
!
type, extends( date ) :: date_time
  real :: xtime = XNEGUNDEF
end type date_time
!
end module modd_type_date
