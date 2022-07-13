!MNH_LIC Copyright 2022-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author: P. Wautelet 13/07/2022
! Modifications:
!-----------------------------------------------------------------
!#################
MODULE MODN_FLYERS
!#################
!
! Namelist to prepare the aircrafts and balloon namelists (dynamic allocation)
!
USE MODD_AIRCRAFT_BALLOON, ONLY: NAIRCRAFTS, NBALLOONS

IMPLICIT NONE

NAMELIST / NAM_FLYERS / NAIRCRAFTS, NBALLOONS

END MODULE MODN_FLYERS
