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
