!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     #####################
      MODULE MODD_CH_CONST
!     ######################
!
!!
!!    PURPOSE
!!    -------
!     
!   
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!
!!    AUTHOR
!!    ------
!!  P. Tulet  (16/01/01) *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!

REAL, SAVE, DIMENSION(:), ALLOCATABLE :: XSREALMASSMOLVAL ! final molecular
                                                          ! diffusivity value
REAL, SAVE, DIMENSION(:), ALLOCATABLE :: XSREALREACTVAL ! final chemical
                                                        ! reactivity factor
                                                        ! with biologie
REAL, SAVE, DIMENSION(:,:), ALLOCATABLE :: XSREALHENRYVAL ! chemical Henry
                                                          ! constant value
REAL, SAVE                            :: XCONVERSION ! emission unit 
                                                     ! conversion factor
!
END MODULE MODD_CH_CONST


