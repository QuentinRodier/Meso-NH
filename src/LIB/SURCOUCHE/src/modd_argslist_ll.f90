!MNH_LIC Copyright 1998-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------

!      #######################
       MODULE MODD_ARGSLIST_ll
!      #######################
!
!!****  *MODD_ARGSLIST_ll* - declaration of lists type 
!
!!    Purpose
!!    -------
!
!     The purpose of this module is to provide lists to manipulate
!     sets of 1D, 2D, or 3D fields and sets of HALO2_ll
!
!!    Reference
!!    ---------
!
!!    Authors
!!    -------
!
!     R. Guivarch               * CERFACS - ENSEEIHT *
!     Ph. Kloos                 * CERFACS - CNRM *
!
!     Implicit Arguments
!     ------------------
!
!     Module MODD_STRUCTURE2_ll
!         type HALO2_ll, halo 2
!
!!    Modifications
!     -------------
!!    Original 04/05/98
!
!-------------------------------------------------------------------------------
!
USE MODD_STRUCTURE2_ll, ONLY: HALO2_ll

implicit none
!
!###########
TYPE LIST_ll
!###########
!
!!****  *Type LIST_ll* -
!
!!    Purpose
!!    -------
!
!     Type for a list of fields
!     This type may be used to handle a list of 1D, 2D or 3D fields.
!
!-------------------------------------------------------------------------------
!
  INTEGER :: NCARD = 0
  LOGICAL :: L1D = .FALSE.
  LOGICAL :: L2D = .FALSE.
  LOGICAL :: L3D = .FALSE.
!
  REAL, DIMENSION(:),     POINTER :: ARRAY1D => NULL()
  REAL, DIMENSION(:,:),   POINTER :: ARRAY2D => NULL()
  REAL, DIMENSION(:,:,:), POINTER :: ARRAY3D => NULL()
!
  TYPE(LIST_ll),          POINTER :: NEXT => NULL()
!
END TYPE LIST_ll
!
!-------------------------------------------------------------------------------
!
!#############
TYPE LIST1D_ll
!#############
!
!!****  *Type LIST1D_ll* -
!
!!    Purpose
!!    -------
!
!     Type for a list of 1D fields
!
!-------------------------------------------------------------------------------
!
  INTEGER :: NCARD = 0
!
  REAL, DIMENSION(:), POINTER :: ARRAY1D => NULL()
  CHARACTER(LEN=1)            :: CDIR
!
  TYPE(LIST1D_ll),    POINTER :: NEXT => NULL()
!
END TYPE LIST1D_ll
!
!-------------------------------------------------------------------------------
!
!################
TYPE HALO2LIST_ll
!################
!
!!****  *Type HALO2LIST_ll* -
!
!!    Purpose
!!    -------
!
!     Type for a list of HALO2_ll
!
!-------------------------------------------------------------------------------
!
  INTEGER :: NCARD = 0
!
  TYPE(HALO2_ll),     POINTER :: HALO2 => NULL()
!
  TYPE(HALO2LIST_ll), POINTER :: NEXT => NULL()
!
END TYPE HALO2LIST_ll
!
!-------------------------------------------------------------------------------
!
END MODULE MODD_ARGSLIST_ll
