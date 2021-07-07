!MNH_LIC Copyright 2017-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!!
!!    #####################
      MODULE MODN_EOL
!!    #####################
!!
!!*** *MODN_EOL*
!!
!!    PURPOSE
!!    -------
!!       NAM_EOL activate the parameterization of wind turbines, and allows
!!       the selection of the aerodynamic method.
!!
!!**  AUTHOR
!!    ------
!!    PA. Joulin                   *CNRM & IFPEN*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 24/01/17
!!    Modification 14/10/20 (PA. Joulin) Updated for a main version
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_EOL_MAIN 
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_EOL/  &
     LMAIN_EOL,CMETH_EOL,CSMEAR,NMODEL_EOL
!
END MODULE MODN_EOL
