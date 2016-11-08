!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source: /home//MESONH/MNH-V5-1-4/src/MODIF_TMICICE/modd_ch_icen.f90
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!-----------------------------------------------------------------------------
!!    ########################
      MODULE MODD_CH_ICE_n
!!    ########################
!!
!!*** *MODD_CH_ICE$n*
!!
!!    PURPOSE
!!    -------
!       This module contains the index for ice phase chemistry or degassing when
!!    the cloud microphysics scheme is ICE3/4
!!
!!**  AUTHOR
!!    ------
!!    M. Leriche      *Laboratoire d'Aerologie*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 11/12/15
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

TYPE CH_ICE_t
!

  INTEGER, POINTER, DIMENSION(:)  :: NINDEXGI, NINDEXWI, NINDEXWG
!
!-----------------------------------------------------------------------------
END TYPE CH_ICE_t

TYPE(CH_ICE_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: CH_ICE_MODEL

INTEGER, POINTER, DIMENSION(:) :: NINDEXGI=>NULL()
INTEGER, POINTER, DIMENSION(:) :: NINDEXWI=>NULL()
INTEGER, POINTER, DIMENSION(:) :: NINDEXWG=>NULL()

CONTAINS

SUBROUTINE CH_ICE_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
CH_ICE_MODEL(KFROM)%NINDEXGI=>NINDEXGI
CH_ICE_MODEL(KFROM)%NINDEXWI=>NINDEXWI
CH_ICE_MODEL(KFROM)%NINDEXWG=>NINDEXWG
!
! Current model is set to model KTO
NINDEXGI=>CH_ICE_MODEL(KTO)%NINDEXGI
NINDEXWI=>CH_ICE_MODEL(KTO)%NINDEXWI
NINDEXWG=>CH_ICE_MODEL(KTO)%NINDEXWG

END SUBROUTINE CH_ICE_GOTO_MODEL

END MODULE MODD_CH_ICE_n
