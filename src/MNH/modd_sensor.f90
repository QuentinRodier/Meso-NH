!MNH_LIC Copyright 2023-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Original version:
!  P. Wautelet: 27/04/2023
! Modifications:
!-----------------------------------------------------------------
MODULE MODD_SENSOR
  USE MODD_PARAMETERS, ONLY: NSENSORNAMELGTMAX

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: TSENSOR

  TYPE, ABSTRACT :: TSENSOR
    CHARACTER(LEN=NSENSORNAMELGTMAX) :: CNAME = '' ! Title or name of the sensor

    LOGICAL :: LFIX ! true if sensor is fix (can not move)
  END TYPE TSENSOR

END MODULE MODD_SENSOR
