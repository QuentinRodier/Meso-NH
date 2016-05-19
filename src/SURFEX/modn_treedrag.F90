!!
!!    #####################
      MODULE MODN_TREEDRAG
!!    #####################
!!
!!*** *MODN_TREEDRAG*
!!
!!    PURPOSE
!!    -------
!       Namelist to take into account tree drag in the atmospheric model
!              instead of SURFEX. The Z0 forest is therefore reduced to
!              the Z0 grass
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
USE MODD_TREEDRAG                           
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_TREEDRAG/  &
     LTREEDRAG                                             

!
END MODULE MODN_TREEDRAG
