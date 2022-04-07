!MNH_LIC Copyright 2002-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ############################
      MODULE MODD_STATION_n
!     ############################
!
!!****  *MODD_STATION* - declaration of stations
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to define
!      the different stations types.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      NONE 
!!
!!    REFERENCE
!!    --------- 
!!       
!!    AUTHOR
!!    ------
!!	P. Tulet   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    15/01/02
!  P. Wautelet 07/04/2022: rewrite types for stations
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
USE MODD_TYPE_STATION

IMPLICIT NONE

TYPE STATION_t
!
!-------------------------------------------------------------------------------------------
!
  LOGICAL                          :: LSTATION    ! flag to use stations
  INTEGER                          :: NUMBSTAT    ! number of stations
  LOGICAL                          :: LSTATLAT    ! positioning in lat/lon
!
  TYPE(TSTATIONTIME) :: TSTATIONS_TIME
  TYPE(TSTATIONDATA), DIMENSION(:), POINTER :: TSTATIONS ! characteristics and records of the stations
!
END TYPE STATION_t

TYPE(STATION_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: STATION_MODEL

LOGICAL, POINTER :: LSTATION=>NULL()
INTEGER, POINTER :: NUMBSTAT=>NULL()
LOGICAL, POINTER :: LSTATLAT=>NULL()
TYPE(TSTATIONTIME),               POINTER :: TSTATIONS_TIME => NULL()
TYPE(TSTATIONDATA), DIMENSION(:), POINTER :: TSTATIONS      => NULL()

CONTAINS

SUBROUTINE STATION_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
STATION_MODEL(KFROM)%TSTATIONS => TSTATIONS
!
! Current model is set to model KTO
LSTATION       => STATION_MODEL(KTO)%LSTATION
NUMBSTAT       => STATION_MODEL(KTO)%NUMBSTAT
LSTATLAT       => STATION_MODEL(KTO)%LSTATLAT
TSTATIONS_TIME => STATION_MODEL(KTO)%TSTATIONS_TIME
TSTATIONS      => STATION_MODEL(KTO)%TSTATIONS

END SUBROUTINE STATION_GOTO_MODEL

END MODULE MODD_STATION_n
