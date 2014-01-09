!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!!
!!    #####################
      MODULE MODN_DRAGTREE
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
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_DRAGTREE                           
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_DRAGTREE/  &
     LDRAGTREE                                             

!
END MODULE MODN_DRAGTREE
