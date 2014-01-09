!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/10/16 14:23:23
!-----------------------------------------------------------------
!     #####################
      MODULE MODD_PARAM_ICE
!     #####################
!
!!****  *MODD_PARAM_ICE* - declaration of the control parameters for the
!!                           mixed phase cloud parameterization
!!
!!    PURPOSE
!!    -------
!!      The purpose of this declarative module is to define the set of space
!!    and time control parameters for the microphysics.
!!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_PARAM_ICE)
!!
!!    AUTHOR
!!    ------
!!     J.-P. Pinty   *Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      14/12/95
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
LOGICAL, SAVE :: LWARM       ! When .TRUE. activates the formation of rain by
                             ! the warm microphysical processes
LOGICAL, SAVE :: LSEDIC                 ! TRUE to enable the droplet sedimentation
!
CHARACTER(LEN=4), SAVE :: CPRISTINE_ICE ! Pristine ice type PLAT, COLU or BURO
CHARACTER(LEN=4), SAVE :: CSEDIM        ! Sedimentation calculation mode      
!
!-------------------------------------------------------------------------------
!
END MODULE MODD_PARAM_ICE
