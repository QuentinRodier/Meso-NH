!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------

! USED ONLY BY IO
! ---------------

MODULE MODD_ERRCODES
  IMPLICIT NONE 

  !! Error codes
  INTEGER, PARAMETER :: NOERROR    =  0
  INTEGER, PARAMETER :: IOERROR    = -1
  INTEGER, PARAMETER :: NOSLOTLEFT = -2
  INTEGER, PARAMETER :: BADVALUE   = -3
  INTEGER, PARAMETER :: UNDEFINED  = -999

END MODULE MODD_ERRCODES
