!MNH_LIC Copyright 2022-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author: P. Wautelet 19/08/2022
! Modifications:
!-----------------------------------------------------------------
!####################
MODULE MODN_AIRCRAFTS
!####################
!
! Namelist with the the characteristics of the aircrafts
!
USE MODD_AIRCRAFT_BALLOON
USE MODD_PARAMETERS,       ONLY: NFILENAMELGTMAX, NSENSORNAMELGTMAX
USE MODD_TYPE_DATE,        ONLY: DATE_TIME

IMPLICIT NONE

!Use separated arrays for the different aircraft characteristics
!Using directly TAIRCRAFTDATA derived types does not work due to compiler bug (GCC at least from 5.5 to 12.1, see GCC bug 106065)

CHARACTER(LEN=3),                 DIMENSION(:), ALLOCATABLE :: CMODEL
INTEGER,                          DIMENSION(:), ALLOCATABLE :: NMODEL
CHARACTER(LEN=NSENSORNAMELGTMAX), DIMENSION(:), ALLOCATABLE :: CTYPE
CHARACTER(LEN=NSENSORNAMELGTMAX), DIMENSION(:), ALLOCATABLE :: CTITLE
TYPE(DATE_TIME),                  DIMENSION(:), ALLOCATABLE :: TLAUNCH
REAL,                             DIMENSION(:), ALLOCATABLE :: XTSTEP
INTEGER,                          DIMENSION(:), ALLOCATABLE :: NPOS
LOGICAL,                          DIMENSION(:), ALLOCATABLE :: LALTDEF
CHARACTER(LEN=NFILENAMELGTMAX),   DIMENSION(:), ALLOCATABLE :: CFILE !Names of CSV files with trajectory data

!Do not read CTYPE, value is always forced to 'AIRCRAFT'
NAMELIST / NAM_AIRCRAFTS / CFILE, CMODEL, CTITLE, LALTDEF, NMODEL, NPOS, TLAUNCH, XTSTEP

CONTAINS

SUBROUTINE AIRCRAFTS_NML_ALLOCATE( KAIRCRAFTS )
  INTEGER, INTENT(IN) :: KAIRCRAFTS

  !Note: the default values are used/checked in ini_aircraft => be careful to ensure coherency
  ALLOCATE( CMODEL (KAIRCRAFTS) ); CMODEL(:)  = 'FIX'
  ALLOCATE( CTITLE (KAIRCRAFTS) ); CTITLE(:)  = ''
  ALLOCATE( CTYPE  (KAIRCRAFTS) ); CTYPE(:)   = 'AIRCRAFT'
  ALLOCATE( NMODEL (KAIRCRAFTS) ); NMODEL(:)  = 0
  ALLOCATE( TLAUNCH(KAIRCRAFTS) )
  ALLOCATE( XTSTEP (KAIRCRAFTS) ); XTSTEP(:)  = XNEGUNDEF
  ALLOCATE( NPOS   (KAIRCRAFTS) ); NPOS(:)    = 0
  ALLOCATE( LALTDEF(KAIRCRAFTS) ); LALTDEF(:) = .FALSE.
  ALLOCATE( CFILE  (KAIRCRAFTS) ); CFILE(:)   = ''
END SUBROUTINE AIRCRAFTS_NML_ALLOCATE


SUBROUTINE AIRCRAFTS_NML_DEALLOCATE( )
  !Deallocate namelist arrays
  DEALLOCATE( CMODEL     )
  DEALLOCATE( CTITLE     )
  DEALLOCATE( CTYPE      )
  DEALLOCATE( NMODEL     )
  DEALLOCATE( TLAUNCH    )
  DEALLOCATE( XTSTEP     )
  DEALLOCATE( NPOS       )
  DEALLOCATE( LALTDEF    )
  DEALLOCATE( CFILE      )
END SUBROUTINE AIRCRAFTS_NML_DEALLOCATE


END MODULE MODN_AIRCRAFTS
