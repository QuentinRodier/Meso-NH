!     ######################
      MODULE MODD_MASK
!     ######################
!
!!****  *MODD_MASK
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	S. Faroux   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       11/03/11
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!-------------------------------------------------------------------------------
!
INTEGER, ALLOCATABLE, DIMENSION(:), TARGET :: NMASK_FULL
!$OMP THREADPRIVATE(NMASK_FULL)
!
!-------------------------------------------------------------------------------
!
END MODULE MODD_MASK

