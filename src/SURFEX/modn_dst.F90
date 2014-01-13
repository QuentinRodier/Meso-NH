!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!!
!!    #####################
      MODULE MODN_DST
!!    #####################
!!
!!*** *MODN_DUST*
!!
!!    PURPOSE
!!    -------
!       Namelist for DUST EMISSION SCHEME aerosol scheme parameters 
!!
!!**  AUTHOR
!!    ------
!!    A. Grini      *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 24/02/05
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_DST_SURF, ONLY : CEMISPARAM_DST, CVERMOD, XFLX_MSS_FDG_FCT
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_SURF_DST/  &
       CEMISPARAM_DST, CVERMOD, XFLX_MSS_FDG_FCT         !Parameterization type   

!
END MODULE MODN_DST
