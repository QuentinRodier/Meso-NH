!MNH_LIC Copyright 2019-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------------------
!!
!!    #####################
MODULE MODN_DRAGBLDG_n
  !!    #####################
  !!
  !!*** *MODN_DRAGBLDG*
  !!
  !!    PURPOSE
  !!    -------
  !       Namelist to take into account building drag in the atmospheric model
  !              instead of SURFEX. 
  !!
  !!**  AUTHOR
  !!    ------
  !!    R.Schoetter                   *CNRM*
  !
  !!    MODIFICATIONS
  !!    -------------
  !!    Original 09/2019
  ! R. Schoetter    12/2021  multi-level coupling between MesoNH and SURFEX  
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !
  USE MODD_DRAGBLDG_n, ONLY :      &
       LDRAGBLDG_n   => LDRAGBLDG,  &
       LFLUXBLDG_n   => LFLUXBLDG,  &
       LDRAGURBVEG_n => LDRAGURBVEG
  !
  !-----------------------------------------------------------------------------
  !
  !*       0.   DECLARATIONS
  !        -----------------
  IMPLICIT NONE
  !
  LOGICAL, SAVE :: LDRAGBLDG
  LOGICAL, SAVE :: LFLUXBLDG
  LOGICAL, SAVE :: LDRAGURBVEG
  !
  NAMELIST /NAM_DRAGBLDGn/LDRAGBLDG,LFLUXBLDG,LDRAGURBVEG
  !
CONTAINS
  !
  SUBROUTINE INIT_NAM_DRAGBLDGn
    LDRAGBLDG = LDRAGBLDG_n
    LFLUXBLDG = LFLUXBLDG_n
    LDRAGURBVEG = LDRAGURBVEG_n
  END SUBROUTINE INIT_NAM_DRAGBLDGn
  !
  SUBROUTINE UPDATE_NAM_DRAGBLDGn
    LDRAGBLDG_n = LDRAGBLDG
    LFLUXBLDG_n = LFLUXBLDG
    LDRAGURBVEG_n = LDRAGURBVEG
  END SUBROUTINE UPDATE_NAM_DRAGBLDGn
  !
END MODULE MODN_DRAGBLDG_n
