!MNH_LIC Copyright 1998-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!########################
MODULE MODD_STRUCTURE2_ll
!########################
!
!!****  *MODD_PARALLEL2* Contains the variables to treat
!                        the second layer of the halo
!
!!    Purpose
!!    -------
!
!     The purpose of this module is to provide the type
!     to manipulate the second layer of the halo
!
!!    Reference
!!    ---------
!
!!    Authors
!!    -------
!
!     R. Guivarch               * CERFACS - ENSEEIHT *
!     Ph. Kloos                 * CERFACS - CNRM *
!
!!    Implicit Arguments
!!    ------------------
!
!     None
!
!!    Modifications
!!    -------------
!
!    Original 04/05/98
!
!-------------------------------------------------------------------------------
!
implicit none
!
!############
TYPE HALO2_ll
!############
!
! Type for the second layer of the halo
!
  REAL, DIMENSION(:,:), POINTER :: WEST  => NULL()
  REAL, DIMENSION(:,:), POINTER :: EAST  => NULL()
  REAL, DIMENSION(:,:), POINTER :: NORTH => NULL()
  REAL, DIMENSION(:,:), POINTER :: SOUTH => NULL()
!
END TYPE HALO2_ll
!
!-------------------------------------------------------------------------------
!
END MODULE MODD_STRUCTURE2_ll
