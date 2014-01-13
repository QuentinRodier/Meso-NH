!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ################
      MODULE MODD_PREP_SNOW
!     ################
!
!!****  *MODD_PREP - declaration for field interpolations
!!
!!    PURPOSE
!!    -------
!     Declaration of surface parameters
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
!!	V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2004
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_SNOW_PAR
!
IMPLICIT NONE
!
!--------------------------------------------------------------------------
!
!* climatological gradient for vertical extrapolations of snow content
!  a rate of 8cm of snow per degree below 0 C is chosen for these mountain tops
! (climatology from Etchevers 2000 in the Alps and the Jura mountains).
!
REAL, PARAMETER  :: XWSNOW_CLIM_GRAD = - 0.08 * 300.     * (-0.0065)
!
!--------------------------------------------------------------------------
! Parameters for snow field uniforn initialization
!
LOGICAL :: LSNOW_FRAC_TOT
INTEGER, PARAMETER :: NSNOW_LAYER_MAX = 50
!
!--------------------------------------------------------------------------
!
!* normalized dimensions for interpolation grids for soil
INTEGER, PARAMETER           :: NGRID_LEVEL = 6
REAL, DIMENSION(NGRID_LEVEL) :: XGRID_SNOW = (/ 0., 0.05, 0.1, 0.5, 0.9, 1.  /)
!
!--------------------------------------------------------------------------
!
END MODULE MODD_PREP_SNOW
