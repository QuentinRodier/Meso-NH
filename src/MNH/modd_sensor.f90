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
  USE MODD_PARAMETERS, ONLY: NSENSORNAMELGTMAX, XNEGUNDEF

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: TSENSOR

  TYPE, ABSTRACT :: TSENSOR
      CHARACTER(LEN=NSENSORNAMELGTMAX) :: CNAME = '' ! Title or name of the sensor

      LOGICAL :: LFIX ! true if sensor is fix (can not move)

      ! Current position of sensor
      REAL :: XX_CUR   = XNEGUNDEF  ! x position
      REAL :: XY_CUR   = XNEGUNDEF  ! y position
      REAL :: XZ_CUR   = XNEGUNDEF  ! z position
      REAL :: XLAT_CUR = XNEGUNDEF  ! latitude
      REAL :: XLON_CUR = XNEGUNDEF  ! longitude
  END TYPE TSENSOR

END MODULE MODD_SENSOR
