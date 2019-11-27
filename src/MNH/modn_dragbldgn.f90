!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
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
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!
USE MODD_DRAGBLDG_n, ONLY :    &
    LDRAGBLDG_n => LDRAGBLDG   
!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
!
LOGICAL, SAVE :: LDRAGBLDG
!
NAMELIST /NAM_DRAGBLDGn/LDRAGBLDG
!
CONTAINS
!
SUBROUTINE INIT_NAM_DRAGBLDGn
   LDRAGBLDG = LDRAGBLDG_n
END SUBROUTINE INIT_NAM_DRAGBLDGn
!
SUBROUTINE UPDATE_NAM_DRAGBLDGn
   LDRAGBLDG_n = LDRAGBLDG
END SUBROUTINE UPDATE_NAM_DRAGBLDGn
!
END MODULE MODN_DRAGBLDG_n
