!MNH_LIC Copyright 2019-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!!
!!    #######################
MODULE MODD_COUPLING_LEVELS_n
  !!    #####################
  !!
  !!*** *MODD_COUPLING_LEVELS_n*
  !!
  !!    PURPOSE
  !!    -------
  !       Declaration to couple SURFEX and Meso-NH at several levels 
  !!
  !!**  AUTHOR
  !!    ------
  !!    R.Schoetter                   *CNRM*
  !
  !!    MODIFICATIONS
  !!    -------------
  !!    Original 12/2019
  !-----------------------------------------------------------------------------
  !
  !*       0.   DECLARATIONS
  !        -----------------
  !
  USE MODD_PARAMETERS, ONLY: JPMODELMAX
  !
  IMPLICIT NONE
  !
  TYPE COUPLING_MULT_t
     !
     INTEGER :: NLEV_COUPLE
     !
  END TYPE COUPLING_MULT_t
  !
  TYPE(COUPLING_MULT_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: COUPLING_MULT_MODEL
  !
  INTEGER, POINTER :: NLEV_COUPLE=>NULL()
  !
CONTAINS
  !
  SUBROUTINE COUPLING_MULT_GOTO_MODEL(KFROM, KTO)
    !
    INTEGER, INTENT(IN) :: KFROM, KTO
    !
    NLEV_COUPLE=>COUPLING_MULT_MODEL(KTO)%NLEV_COUPLE
    !
  END SUBROUTINE COUPLING_MULT_GOTO_MODEL
  !
END MODULE MODD_COUPLING_LEVELS_n
