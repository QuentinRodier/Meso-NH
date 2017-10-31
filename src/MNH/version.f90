!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
! $Source$ $Revision$
!-----------------------------------------------------------------
!     ##################
      SUBROUTINE VERSION
!     ##################
!
!!****  *VERSION * - subroutine to initialize the Mesonh version
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to initialize NMASDEV and NBUGFIX
!     corresponding to the version chosen by the MesoNH user.
!       The user can also set the name of his own binary library
!      These values will be writen in the MesoNH output files
!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CONF       : NMASDEV,NBUGFIX,CBIBUSER
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation (routine VERSION)
!!
!!
!!    AUTHOR
!!    ------
!!      P. Jabouille       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    17/04/02
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
USE MODD_CONF, ONLY : NMASDEV,NBUGFIX,CBIBUSER
!
IMPLICIT NONE
!
NMASDEV=54
NBUGFIX=0
CBIBUSER=''
!
END SUBROUTINE VERSION
