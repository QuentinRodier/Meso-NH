!MNH_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
! 
!    #######################
MODULE MODD_IBM_LSF
  !  #######################
  !
  !!****  MODD_IBM_LSF_ - declaration of the control parameters
  !!                      used in the LSF building
  !!
  !!    PURPOSE
  !!    -------
  !!****  The purpose of this declarative module is to declare the constants
  !!      which allow to initialize the embedded fluid-solid interface 
  !!
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!      None 
  !!
  !!    REFERENCE
  !!    ---------
  !!          
  !!    AUTHOR
  !!    ------
  !!	Franck Auguste (CERFACS-AE)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original    01/01/2019
  !
  !------------------------------------------------------------------------------
  !
  !**** 0. DECLARATIONS
  !     ---------------
  !
  USE MODD_PARAMETERS, ONLY: JPMODELMAX
  !
  IMPLICIT NONE
  !
  TYPE LSF_t
     !
     LOGICAL          :: LIBM_LSF = .FALSE.     ! IBM logical
     CHARACTER(LEN=4) :: CIBM_TYPE = 'NONE'     ! switch generalized/idealized surface 
     INTEGER          :: NIBM_SMOOTH = 1        ! smooth levels for LS
     REAL             :: XIBM_SMOOTH  = 0.0001  ! smooth weighting
     !
  END TYPE LSF_t
  !
  TYPE(LSF_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: LSF_MODEL
  !
  LOGICAL          , POINTER :: LIBM_LSF=>NULL()
  CHARACTER(LEN=4) , POINTER :: CIBM_TYPE=>NULL()
  INTEGER          , POINTER :: NIBM_SMOOTH=>NULL()
  REAL             , POINTER :: XIBM_SMOOTH=>NULL()
  !
CONTAINS
  !
  SUBROUTINE LSF_GOTO_MODEL(KFROM, KTO)
    !
    INTEGER, INTENT(IN) :: KFROM, KTO
    !
    ! Save current state for allocated arrays
    !
    ! Current model is set to model KTO
    LIBM_LSF=>LSF_MODEL(KTO)%LIBM_LSF
    CIBM_TYPE=>LSF_MODEL(KTO)%CIBM_TYPE
    XIBM_SMOOTH=>LSF_MODEL(KTO)%XIBM_SMOOTH
    NIBM_SMOOTH=>LSF_MODEL(KTO)%NIBM_SMOOTH
    !
  END SUBROUTINE LSF_GOTO_MODEL
  !
END MODULE MODD_IBM_LSF
!

