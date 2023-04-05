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
!!*** *MODN_EOL_ADR*
!!
!!    PURPOSE
!!    -------
!!       NAM_EOL activate the parameterization of wind turbines, and several
!!       models are available. One of the models is the Actuator Disc with
!!       Rotation (ADR).
!!       The aim of NAM_EOL_ADR is to specify ADR parameters. 
!!
!!**  AUTHOR
!!    ------
!!    H. Toumi                    *IFPEN*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 09/22
!!    Modification 05/04/23 (PA. Joulin) Updated for a main version
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_EOL_ADR      , ONLY : NNB_AZIELT, NNB_RADELT
USE MODD_EOL_SHARED_IO, ONLY : CFARM_CSVDATA,CTURBINE_CSVDATA 
USE MODD_EOL_SHARED_IO, ONLY : CBLADE_CSVDATA, CAIRFOIL_CSVDATA
USE MODD_EOL_SHARED_IO, ONLY : CINTERP, LTIPLOSSG
USE MODD_EOL_SHARED_IO, ONLY : LTECOUTPTS,LCSVOUTFR
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
