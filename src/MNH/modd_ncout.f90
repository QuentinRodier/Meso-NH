!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
#ifdef MNH_NCWRIT
!     #################
      MODULE MODD_NCOUT
!     #################
!
!!****  *MODD_NCOUT* - declaration of configuration variables
!!
!!    PURPOSE
!!    -------
!    Flag for NETCDF output 
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!    AUTHOR
!!    ------
!!	S. Bielli L.A.
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    26/03/2012    
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
LOGICAL,SAVE      :: LNETCDF = .FALSE.  ! default no netcdf output
LOGICAL,SAVE      :: LLFIFM = .TRUE.  ! default lfi output
!
END MODULE MODD_NCOUT
#endif
