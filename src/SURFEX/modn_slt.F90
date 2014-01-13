!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!!
!!    #####################
      MODULE MODN_SLT
!!    #####################
!!
!!*** *MODN_DUST*
!!
!!    PURPOSE
!!    -------
!       Namelist for SEA SALT EMISSION SCHEME aerosol scheme parameters 
!!
!!**  AUTHOR
!!    ------
!!    A. Grini / P. Tulet     *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 24/02/05
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_SLT_SURF, ONLY : CEMISPARAM_SLT
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_SURF_SLT/  &
       CEMISPARAM_SLT            !Parameterization type   

!
END MODULE MODN_SLT
