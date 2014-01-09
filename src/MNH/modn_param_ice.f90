!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modn 2006/10/16 14:23:23
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
NAMELIST/NAM_PARAM_ICE/LWARM,LSEDIC,CPRISTINE_ICE,CSEDIM
!
END MODULE MODN_PARAM_ICE
