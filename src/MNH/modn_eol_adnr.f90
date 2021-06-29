!MNH_LIC Copyright 2016-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!!
!!    #####################
      MODULE MODN_EOL_ADNR
!!    #####################
!!
!!*** *MODN_EOL_ADNR*
!!
!!    PURPOSE
!!    -------
!!       NAM_EOL activate the parameterization of wind turbines, and several
!!       models are available. One of the models is the Non-Rotating Actuator 
!!       Disk Non Rotating (ADNR).
!!       The aim of NAM_EOL_ADNR is to specify ADNR parameters. 
!!
!!**  AUTHOR
!!    ------
!!    PA. Joulin                   *CNRM & IFPEN*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 04/16
!!    Modification 14/10/20 (PA. Joulin) Updated for a main version
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_EOL_ADNR
USE MODD_EOL_SHARED_IO
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_EOL_ADNR/  &
     CFARM_CSVDATA, CTURBINE_CSVDATA, &
     CINTERP
!
END MODULE MODN_EOL_ADNR
