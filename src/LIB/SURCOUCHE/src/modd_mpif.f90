!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------

MODULE MODD_MPIF
#ifdef USE_MPI
  USE MPI
  IMPLICIT NONE
#else
  IMPLICIT NONE
  INCLUDE 'mpif.h'
#endif
END MODULE MODD_MPIF
