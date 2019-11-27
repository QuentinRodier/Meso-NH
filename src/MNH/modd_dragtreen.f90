!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!!
!!    #####################
      MODULE MODD_DRAGTREE_n
!!    #####################
!!
!!*** *MODD_DRAGTREE*
!!
!!    PURPOSE
!!    -------
!       Declaration to take into account tree drag in Meso-NH              
!              instead of SURFEX. 
!!
!!**  AUTHOR
!!    ------
!!    C.Lac                   *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 30/06/11
!!    06/16  (C.Lac) Add droplet deposition
!!    11/2019 C.Lac correction in the drag formula and application to building in addition to tree

!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
!
IMPLICIT NONE
!
TYPE DRAGTREE_t
  !
  LOGICAL    ::     LDRAGTREE    ! flag used to  take into account tree drag in 
  !                              ! the atmospheric model instead of SURFEX.
  LOGICAL    ::     LDEPOTREE    ! flag for droplet deposition on trees
  !
  REAL       ::     XVDEPOTREE   ! Droplet deposition velocity
  !
END TYPE DRAGTREE_t
!
TYPE(DRAGTREE_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: DRAGTREE_MODEL
!
LOGICAL,POINTER :: LDRAGTREE=>NULL()
LOGICAL,POINTER :: LDEPOTREE=>NULL()
REAL   ,POINTER :: XVDEPOTREE=>NULL()
!
CONTAINS
!
SUBROUTINE DRAGTREE_GOTO_MODEL(KFROM, KTO)
  !
  INTEGER, INTENT(IN) :: KFROM, KTO
  !
  LDRAGTREE=>DRAGTREE_MODEL(KTO)%LDRAGTREE
  LDEPOTREE=>DRAGTREE_MODEL(KTO)%LDEPOTREE
  XVDEPOTREE=>DRAGTREE_MODEL(KTO)%XVDEPOTREE
  !
END SUBROUTINE DRAGTREE_GOTO_MODEL
!
END MODULE MODD_DRAGTREE_n
