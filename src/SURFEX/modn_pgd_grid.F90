!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ##################
      MODULE MODN_PGD_GRID
!     ##################
!
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!       
!!    AUTHOR
!!    ------
!!	V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2003                    
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PGD_GRID, ONLY : CGRID
!
IMPLICIT NONE
!
 CHARACTER(LEN=28):: YINIFILE ! name of input file
 CHARACTER(LEN=6) :: YFILETYPE! type of input file
!
!
NAMELIST/NAM_PGD_GRID/CGRID,YINIFILE,YFILETYPE
!
END MODULE MODN_PGD_GRID
