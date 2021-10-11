!     ######################
      MODULE MODD_URBTREE
!     ######################
!
!!****  *MODD_URBTREE* - declaration of parameters related
!!                          to the high vegetation parameterizations
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the 
!     parameters related to the high veegtation interception surface parameterizations of TEB Garden
!
!!
!!      
!!
!!    AUTHOR
!!    ------
!!	E. Redon
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/2014                     
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
! Vegetation (SW or LW) radiation interception properties
!
! Extinction coefficient for view factor for long-wave radiation 
!
REAL, PARAMETER        :: XTAU_SWHV = 0.5 
!
! Extinction coefficient for view factor for short-wave radiation 
!
REAL, PARAMETER        :: XTAU_LWHV = 0.5 
!
!-------------------------------------------------------------------------------
!
END MODULE MODD_URBTREE
