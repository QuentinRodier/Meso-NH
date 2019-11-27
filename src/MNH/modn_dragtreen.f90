!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!!
!!    #####################
      MODULE MODN_DRAGTREE_n
!!    #####################
!!
!!*** *MODN_DRAGTREE*
!!
!!    PURPOSE
!!    -------
!       Namelist to take into account tree drag in the atmospheric model
!              instead of SURFEX. 
!!
!!**  AUTHOR
!!    ------
!!    C.Lac                   *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 30/06/11
!!
!!    10/2016 : (C.Lac) Add droplet deposition on trees
!!    11/2019 C.Lac correction in the drag formula and application to building in addition to tree
!!    IMPLICIT ARGUMENTS
!!    ------------------
!
USE MODD_DRAGTREE_n, ONLY :    &
    LDRAGTREE_n => LDRAGTREE,  &
    LDEPOTREE_n => LDEPOTREE,  &
    XVDEPOTREE_n => XVDEPOTREE
!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
!
LOGICAL, SAVE :: LDRAGTREE
LOGICAL, SAVE :: LDEPOTREE
REAL, SAVE    :: XVDEPOTREE
!
NAMELIST /NAM_DRAGTREEn/LDRAGTREE,LDEPOTREE,XVDEPOTREE
!
CONTAINS
!
SUBROUTINE INIT_NAM_DRAGTREEn
   LDRAGTREE  = LDRAGTREE_n
   LDEPOTREE  = LDEPOTREE_n
   XVDEPOTREE = XVDEPOTREE_n
END SUBROUTINE INIT_NAM_DRAGTREEn
!
SUBROUTINE UPDATE_NAM_DRAGTREEn
   LDRAGTREE_n  = LDRAGTREE
   LDEPOTREE_n  = LDEPOTREE
   XVDEPOTREE_n = XVDEPOTREE
END SUBROUTINE UPDATE_NAM_DRAGTREEn
!
END MODULE MODN_DRAGTREE_n
