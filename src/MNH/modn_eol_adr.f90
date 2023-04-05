!MNH_LIC Copyright 2017-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!!
!!    #####################
      MODULE MODN_EOL_ADR
!!    #####################
!!
!!*** *MODN_EOL_ALM*
!!
!!    PURPOSE
!!    -------
!!       NAM_EOL activate the parameterization of wind turbines, and several
!!       models are available. One of the models is the Actuator Line
!!       Method (ALM).
!!       The aim of NAM_EOL_ALM is to specify ALM parameters. 
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
USE MODD_EOL_ADR                           
USE MODD_EOL_SHARED_IO                           
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_EOL_ADR/                                                  &
     CFARM_CSVDATA, CTURBINE_CSVDATA, CBLADE_CSVDATA, CAIRFOIL_CSVDATA, &
     NNB_AZIELT, NNB_RADELT,                                            &
     CINTERP, LTIPLOSSG,                                                &
     LTECOUTPTS,LCSVOUTFRM
!
END MODULE MODN_EOL_ADR
