!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!!
!!    #####################
      MODULE MODN_WRITE_SURF_ATM
!!    #####################
!!
!!*** *MODN_WRITE_SURF_ATM*
!!
!!    PURPOSE
!!    -------
!       Namelist for writing into output files
!!
!!**  AUTHOR
!!    ------
!!    P. Le Moigne      *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 02/2008
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_WRITE_SURF_ATM, ONLY : LNOWRITE_CANOPY, LNOWRITE_TEXFILE
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_WRITE_SURF_ATM/LNOWRITE_CANOPY, LNOWRITE_TEXFILE
!
END MODULE MODN_WRITE_SURF_ATM
