!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
#ifdef MNH_NCWRIT
!     ################
      MODULE MODN_NCOUT
!     ################
!
!!****  *MODN_NCOUT* - declaration of namelist NAM_NCOUT
!!
!!    PURPOSE
!!    -------
!    writting of NETCDF output
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_NCOUT : contains declaration of configuration variables
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	S. Bielli L.A.
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    26/03/2012    
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_NCOUT
!
IMPLICIT NONE
!
NAMELIST/NAM_NCOUT/ LNETCDF,LLFIFM
!
END MODULE MODN_NCOUT
#endif
