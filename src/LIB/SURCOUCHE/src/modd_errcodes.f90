!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------

! USED ONLY BY IO
! ---------------

MODULE MODD_ERRCODES
  IMPLICIT NONE 

  !! Error codes
  INTEGER, PARAMETER :: NOERROR    =  0
  INTEGER, PARAMETER :: IOERROR    = -1
  INTEGER, PARAMETER :: NOSLOTLEFT = -2
  INTEGER, PARAMETER :: BADVALUE   = -3
  INTEGER, PARAMETER :: UNDEFINED  = -999

END MODULE MODD_ERRCODES
