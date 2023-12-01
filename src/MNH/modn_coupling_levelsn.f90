!MNH_LIC Copyright 2019-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!!
!!    #######################
MODULE MODN_COUPLING_LEVELS_n
  !!    #####################
  !!
  !!*** *MODN_COUPLING_LEVELS_n*
  !!
  !!    PURPOSE
  !!    -------
  !   Namelist to couple SURFEX and Meso-NH at several levels 
  !!
  !!**  AUTHOR
  !!    ------
  !!    R.Schoetter                   *CNRM*
  !
  !!    MODIFICATIONS
  !!    -------------
  !!    Original 12/2019
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !
  USE MODD_COUPLING_LEVELS_n, ONLY :       &
       NLEV_COUPLE_n   => NLEV_COUPLE
  !
  !-----------------------------------------------------------------------------
  !
  !*       0.   DECLARATIONS
  !        -----------------
  IMPLICIT NONE
  !
  INTEGER, SAVE :: NLEV_COUPLE
  !
  NAMELIST /NAM_COUPLING_LEVELSn/NLEV_COUPLE
  !
CONTAINS
  !
  SUBROUTINE INIT_NAM_COUPLING_LEVELSn
    NLEV_COUPLE = NLEV_COUPLE_n
  END SUBROUTINE INIT_NAM_COUPLING_LEVELSn
  !
  SUBROUTINE UPDATE_NAM_COUPLING_LEVELSn
    NLEV_COUPLE_n = NLEV_COUPLE
  END SUBROUTINE UPDATE_NAM_COUPLING_LEVELSn
  !
END MODULE MODN_COUPLING_LEVELS_n
