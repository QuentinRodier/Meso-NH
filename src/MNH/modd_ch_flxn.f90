!MNH_LIC Copyright 2016-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #####################
      MODULE MODD_CH_FLX_n
!     ######################
!
!!
!!    PURPOSE
!!    -------
!     Save the net surface flux at the surface 
!     for output with diag
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!
!!    AUTHOR
!!    ------
!!  P. Tulet   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!  12/07/16 (M. Leriche) keep only the flux
!!  
!------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE CH_FLX_t
!
  REAL, DIMENSION(:,:,:), POINTER :: XCHFLX=>NULL() ! chemical fluxes ppv.m/s at t
!
END TYPE CH_FLX_t

TYPE(CH_FLX_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: CH_FLX_MODEL

REAL, DIMENSION(:,:,:), POINTER :: XCHFLX=>NULL()

CONTAINS

SUBROUTINE CH_FLX_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
CH_FLX_MODEL(KFROM)%XCHFLX=>XCHFLX
!
! Current model is set to model KTO
XCHFLX=>CH_FLX_MODEL(KTO)%XCHFLX

END SUBROUTINE CH_FLX_GOTO_MODEL

END MODULE MODD_CH_FLX_n
