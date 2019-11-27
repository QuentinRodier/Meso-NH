!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!!
!!    #####################
      MODULE MODD_DRAGBLDG_n
!!    #####################
!!
!!*** *MODD_DRAGBLDG*
!!
!!    PURPOSE
!!    -------
!       Declaration to take into account building drag in Meso-NH instead of SURFEX. 
!!
!!**  AUTHOR
!!    ------
!!    R.Schoetter                   *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 09/2019
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
!
IMPLICIT NONE
!
TYPE DRAGBLDG_t
  !
  LOGICAL    ::     LDRAGBLDG    ! flag used to take into account building drag in 
  !                              ! the atmospheric model instead of SURFEX.
  !
END TYPE DRAGBLDG_t
!
TYPE(DRAGBLDG_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: DRAGBLDG_MODEL
!
LOGICAL, POINTER :: LDRAGBLDG=>NULL()
!
CONTAINS
!
SUBROUTINE DRAGBLDG_GOTO_MODEL(KFROM, KTO)
  !
  INTEGER, INTENT(IN) :: KFROM, KTO
  !
  LDRAGBLDG=>DRAGBLDG_MODEL(KTO)%LDRAGBLDG
  !
END SUBROUTINE DRAGBLDG_GOTO_MODEL
!
END MODULE MODD_DRAGBLDG_n
