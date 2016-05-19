!     ######spl
      MODULE MODN_PASPOL
!     ##################
!-------------------------------------------------------------------------------
!***	MODD_PASPOL  Declaration of namelist NAM_PASPOL
!
!!    AUTHOR
!!    ------
!	           : Michel Bouzom, DP/SERV/ENV
!	Creation   : 09.10.2001
!-------------------------------------------------------------------------------
!
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PASPOL
!
IMPLICIT NONE
!
NAMELIST /NAM_PASPOL/ &
     LPASPOL,NRELEASE,CPPINIT,XPPLAT,XPPLON,XPPMASS , &
     XPPBOT,XPPTOP,CPPT1,CPPT2,CPPT3,CPPT4
!
END MODULE MODN_PASPOL
