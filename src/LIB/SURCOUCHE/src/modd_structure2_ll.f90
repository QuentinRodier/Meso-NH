!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------

!      ########################
       MODULE MODD_STRUCTURE2_ll
!      ########################
!
!!****  *MODD_PARALLEL2* Contains the variables to treat
!                        the second layer of the halo
!
!!    Purpose
!!    -------
!
!     The purpose of this module is to provide the type
!     to manipulate the second layer of the halo
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
!!    Implicit Arguments
!!    ------------------
!
!     None
!
!!    Modifications
!!    -------------
!
!    Original 04/05/98
!
!-------------------------------------------------------------------------------
!
!     #############
      TYPE HALO2_ll
!     #############
!
! Type for the second layer of the halo
!
  REAL, DIMENSION(:,:), POINTER :: WEST
  REAL, DIMENSION(:,:), POINTER :: EAST
  REAL, DIMENSION(:,:), POINTER :: NORTH
  REAL, DIMENSION(:,:), POINTER :: SOUTH
!
      END TYPE HALO2_ll
!
!-------------------------------------------------------------------------------
!
END MODULE MODD_STRUCTURE2_ll
