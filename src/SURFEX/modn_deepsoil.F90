!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ##################
      MODULE MODN_DEEPSOIL
!     ##################
!
!!****  *MODN_DEEPSOIL - deep soil characteristics
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   05/2008
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_DEEPSOIL, ONLY:  LDEEPSOIL, LPHYSDOMC
!
IMPLICIT NONE
!
NAMELIST/NAM_DEEPSOIL/LDEEPSOIL, LPHYSDOMC
!
END MODULE MODN_DEEPSOIL
