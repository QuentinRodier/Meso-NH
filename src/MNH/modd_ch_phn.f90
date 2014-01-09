!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source: /home//MESONH/MNH-V4-6-5/src/SRC_CHIMAQ/modd_ch_pHn.f90
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!-----------------------------------------------------------------------------
!!    ########################
      MODULE MODD_CH_PH_n
!!    ########################
!!
!!*** *MODD_CH_PH$n*
!!
!!    PURPOSE
!!    -------
!       This module contains the pH field of the cloud water of the rainwater
!!
!!**  AUTHOR
!!    ------
!!    M. Leriche      *Laboratoire d'Aerologie*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 01/06/07
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!    none
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE CH_PH_t
!
  REAL, POINTER, DIMENSION(:,:,:) :: XPHC ! cloud
  REAL, POINTER, DIMENSION(:,:,:) :: XPHR ! rain
!
!-----------------------------------------------------------------------------
END TYPE CH_PH_t

TYPE(CH_PH_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: CH_PH_MODEL

REAL, POINTER, DIMENSION(:,:,:) :: XPHC=>NULL()
REAL, POINTER, DIMENSION(:,:,:) :: XPHR=>NULL()

CONTAINS

SUBROUTINE CH_PH_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
CH_PH_MODEL(KFROM)%XPHC=>XPHC
CH_PH_MODEL(KFROM)%XPHR=>XPHR
!
! Current model is set to model KTO
XPHC=>CH_PH_MODEL(KTO)%XPHC
XPHR=>CH_PH_MODEL(KTO)%XPHR

END SUBROUTINE CH_PH_GOTO_MODEL

END MODULE MODD_CH_PH_n
