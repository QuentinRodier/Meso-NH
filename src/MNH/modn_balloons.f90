!MNH_LIC Copyright 2022-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author: P. Wautelet 13/07/2022
! Modifications:
!-----------------------------------------------------------------
!###################
MODULE MODN_BALLOONS
!###################
!
! Namelist with the the characteristics of the balloons
!
USE MODD_AIRCRAFT_BALLOON
USE MODD_TYPE_DATE,        ONLY: DATE_TIME

IMPLICIT NONE

!Use separated arrays for the different balloon characteristics
!Using directly TBALLOONDATA derived types does not work due to compiler bug (GCC at least from 5.5 to 12.1, see GCC bug 106065)

CHARACTER(LEN=3),  DIMENSION(:), ALLOCATABLE :: CMODEL
INTEGER,           DIMENSION(:), ALLOCATABLE :: NMODEL
CHARACTER(LEN=6),  DIMENSION(:), ALLOCATABLE :: CTYPE
CHARACTER(LEN=10), DIMENSION(:), ALLOCATABLE :: CTITLE
TYPE(DATE_TIME),   DIMENSION(:), ALLOCATABLE :: TLAUNCH
REAL,              DIMENSION(:), ALLOCATABLE :: XLATLAUNCH
REAL,              DIMENSION(:), ALLOCATABLE :: XLONLAUNCH
!Not needed: XXLAUNCH
!Not needed: XYLAUNCH
REAL,              DIMENSION(:), ALLOCATABLE :: XALTLAUNCH
REAL,              DIMENSION(:), ALLOCATABLE :: XTSTEP
REAL,              DIMENSION(:), ALLOCATABLE :: XWASCENT
!Not used in NML (computed): REAL,              DIMENSION(:), ALLOCATABLE :: XRHO
REAL,              DIMENSION(:), ALLOCATABLE :: XPRES
REAL,              DIMENSION(:), ALLOCATABLE :: XDIAMETER
REAL,              DIMENSION(:), ALLOCATABLE :: XAERODRAG
REAL,              DIMENSION(:), ALLOCATABLE :: XINDDRAG
REAL,              DIMENSION(:), ALLOCATABLE :: XVOLUME
REAL,              DIMENSION(:), ALLOCATABLE :: XMASS

NAMELIST / NAM_BALLOONS / CMODEL, CTITLE, CTYPE, NMODEL, TLAUNCH,                      &
                          XLATLAUNCH, XLONLAUNCH, XALTLAUNCH, XTSTEP, XWASCENT, XPRES, &
                          XDIAMETER, XAERODRAG, XINDDRAG, XVOLUME, XMASS

CONTAINS

SUBROUTINE BALLOONS_NML_ALLOCATE( KBALLOONS )
  INTEGER, INTENT(IN) :: KBALLOONS

  !Note: the default values are used/checked in ini_balloon => be careful to ensure coherency
  ALLOCATE( CMODEL (KBALLOONS) ); CMODEL(:)  = 'FIX'
  ALLOCATE( CTITLE (KBALLOONS) ); CTITLE(:)  = ''
  ALLOCATE( CTYPE  (KBALLOONS) ); CTYPE(:)   = ''
  ALLOCATE( NMODEL (KBALLOONS) ); NMODEL(:)  = 0
  ALLOCATE( TLAUNCH(KBALLOONS) )
  ALLOCATE( XLATLAUNCH  (KBALLOONS) ); XLATLAUNCH(:) = XUNDEF
  ALLOCATE( XLONLAUNCH  (KBALLOONS) ); XLONLAUNCH(:) = XUNDEF
  ALLOCATE( XALTLAUNCH  (KBALLOONS) ); XALTLAUNCH(:) = XNEGUNDEF
  ALLOCATE( XTSTEP      (KBALLOONS) ); XTSTEP(:)     = XNEGUNDEF
  ALLOCATE( XWASCENT    (KBALLOONS) ); XWASCENT(:)   = XNEGUNDEF
  ALLOCATE( XPRES       (KBALLOONS) ); XPRES(:)      = XNEGUNDEF
  ALLOCATE( XDIAMETER   (KBALLOONS) ); XDIAMETER(:)  = XNEGUNDEF
  ALLOCATE( XAERODRAG   (KBALLOONS) ); XAERODRAG(:)  = XNEGUNDEF
  ALLOCATE( XINDDRAG    (KBALLOONS) ); XINDDRAG(:)   = XNEGUNDEF
  ALLOCATE( XVOLUME     (KBALLOONS) ); XVOLUME(:)    = XNEGUNDEF
  ALLOCATE( XMASS       (KBALLOONS) ); XMASS(:)      = XNEGUNDEF
END SUBROUTINE BALLOONS_NML_ALLOCATE


SUBROUTINE BALLOONS_NML_DEALLOCATE( )
  !Deallocate namelist arrays
  DEALLOCATE( CMODEL     )
  DEALLOCATE( CTITLE     )
  DEALLOCATE( CTYPE      )
  DEALLOCATE( NMODEL     )
  DEALLOCATE( TLAUNCH    )
  DEALLOCATE( XLATLAUNCH )
  DEALLOCATE( XLONLAUNCH )
  DEALLOCATE( XALTLAUNCH )
  DEALLOCATE( XTSTEP     )
  DEALLOCATE( XWASCENT   )
  DEALLOCATE( XPRES      )
  DEALLOCATE( XDIAMETER  )
  DEALLOCATE( XAERODRAG  )
  DEALLOCATE( XINDDRAG   )
  DEALLOCATE( XVOLUME    )
  DEALLOCATE( XMASS      )
END SUBROUTINE BALLOONS_NML_DEALLOCATE


END MODULE MODN_BALLOONS
