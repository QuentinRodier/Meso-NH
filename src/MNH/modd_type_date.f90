!MNH_LIC Copyright 1997-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #################
      MODULE MODD_TYPE_DATE
!     #################
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
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
use modd_parameters, only: NNEGUNDEF, XNEGUNDEF

IMPLICIT NONE
!
TYPE DATE
  INTEGER :: YEAR  = NNEGUNDEF
  INTEGER :: MONTH = 0
  INTEGER :: DAY   = 0
END TYPE DATE
!
TYPE DATE_TIME
  TYPE (DATE) :: TDATE
  REAL        :: TIME = XNEGUNDEF
END TYPE DATE_TIME
!
END MODULE MODD_TYPE_DATE
