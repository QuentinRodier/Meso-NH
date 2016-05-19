!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------

!     #########################
      MODULE MODD_PARAMETERS_ll
!     #########################
!
!!****  *MODD_PARAMETERS_ll* - declaration of parameter variables
!!                             communication layer
!
!!
!!    Purpose
!!    -------
!       The purpose of this declarative module is to specify  the variables 
!     which have the PARAMETER attribute   
!
!!   Reference
!!    ---------
!
!!    Authors
!!    -------
!
!     R. Guivarch               * CERFACS - ENSEEIHT *
!     Ph. Kloos                 * CERFACS - CNRM *
!     N. Gicquel                * CNRM *
!
!!    Implicit Arguments
!!    ------------------
!
!     None
!
!!    Modifications
!!    -------------
!
!    Original 04/05/99

!-------------------------------------------------------------------------------
!
!
  INTEGER :: JPHEXT        ! Horizontal External points number
  INTEGER :: JPVEXT        ! Vertical External points number
  INTEGER :: JPMODELMAX    ! Maximum allowed number of nested models 
!
  INTEGER, PARAMETER :: NMAXRIM = 10 ! maximum number of different RIM sizes
!
END MODULE MODD_PARAMETERS_ll
