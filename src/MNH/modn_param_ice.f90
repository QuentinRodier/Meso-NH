!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source: /srv/cvsroot/MNH-VX-Y-Z/src/MNH/modn_param_ice.f90,v $ $Revision: 1.2.2.1.2.1.18.2 $
! MASDEV4_7 modn 2006/10/16 14:23:23
!!
!! MODIFICATIONS
!!
!!                    10/2016 (C.Lac) Add droplet deposition
!-----------------------------------------------------------------
!     #####################
      MODULE MODN_PARAM_ICE
!     #####################
!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAM_ICE
!
IMPLICIT NONE
!
NAMELIST/NAM_PARAM_ICE/LWARM,LSEDIC,LCONVHG,CPRISTINE_ICE,CSEDIM,LDEPOSC,XVDEPOSC
!
END MODULE MODN_PARAM_ICE
